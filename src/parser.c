#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "dict.h"
#include "lexer.h"
#include "list.h"
#include "parser.h"
#include "prslib.h"
#include "prsasm.h"
#include "prsutil.h"
#include "util.h"

#define MAX_ALIGN         16

/* PARSER Prototypes */
Ast *ParseStatement(Cctrl *cc);
Ast *ParseIfStatement(Cctrl *cc);
Ast *ParseForStatement(Cctrl *cc);
Ast *ParseVariableInitialiser(Cctrl *cc, Ast *var, long terminator_flags);
Ast *ParseDecl(Cctrl *cc);
Ast *ParseDeclOrStatement(Cctrl *cc);
Ast *ParseCompoundStatement(Cctrl *cc);
AstType *ParseClassDef(Cctrl *cc, int is_intrinsic);
AstType *ParseUnionDef(Cctrl *cc);

/* Kinda cheating converting it to a string and calling printf */
Ast *ParseFloatingCharConst(Cctrl *cc, lexeme *tok) {
    unsigned long ch = (unsigned long)tok->i64;
    Ast *ast;
    char str[9];
    List *argv = ListNew();
    str[0] = ch & 0xFF;
    str[1] = ((unsigned long)ch) >> 8  & 0xFF;
    str[2] = ((unsigned long)ch) >> 16 & 0xFF;
    str[3] = ((unsigned long)ch) >> 24 & 0xFF;
    str[4] = ((unsigned long)ch) >> 32 & 0xFF;
    str[5] = ((unsigned long)ch) >> 40 & 0xFF;
    str[6] = ((unsigned long)ch) >> 48 & 0xFF;
    str[7] = ((unsigned long)ch) >> 56 & 0xFF;
    str[8] = '\0';

    ast = AstString(str,sizeof(str));
    ListAppend(cc->strings,ast);
    ListAppend(argv,ast);
    ast = AstFunctionCall(ast_void_type,"printf",6,argv,ListNew());
    CctrlTokenExpect(cc,';');
    return ast;
}

Ast *ParseDeclArrayInitInt(Cctrl *cc, AstType *type) {
    lexeme *tok = CctrlTokenGet(cc);
    List *initlist;
    Ast *init;

    if (type->kind == AST_TYPE_CLASS) {
        loggerPanic("line %d: Initalising an array of classes is unsupported\n",
                tok->line);
    }

    if (type->ptr->kind == AST_TYPE_CHAR  && tok->tk_type == TK_STR) {
        return AstString(tok->start,tok->len);
    }

    if (!TokenPunctIs(tok, '{')) {
        loggerPanic("line %ld: Expected intializer list starting with '{' got: '%c'",
                cc->lineno,(char)tok->i64);
    }

    initlist = ListNew();
    while (1) {
        tok = CctrlTokenGet(cc);
        if (TokenPunctIs(tok, '}')) {
            break;
        }
        CctrlTokenRewind(cc);
        if (TokenPunctIs(tok,'{')) {
            init = ParseDeclArrayInitInt(cc,type->ptr);
            tok = CctrlTokenGet(cc);
            ListAppend(initlist,init);
            if (TokenPunctIs(tok,'}')) {
                init = AstArrayInit(initlist);
                return init;
            }
            continue;
        } else {
            init = ParseExpr(cc,16);
            if ((AstGetResultType('=', init->type, type->ptr)) == NULL) {
                loggerPanic("line %ld: Incompatiable types: %s %s\n",
                        cc->lineno,
                        AstToString(init),
                        AstTypeToString(type->ptr));
            }
        }
        ListAppend(initlist,init);

        tok = CctrlTokenGet(cc);
        if (!TokenPunctIs(tok, ',')) {
            CctrlTokenRewind(cc);
        }
    }
    return AstArrayInit(initlist);
}

void ParseFlattenAnnonymous(AstType *anon, Dict *fields_dict,
        int offset, int make_copy)
{
    AstType *base, *type;
    for (int i = 0; i < anon->fields->capacity; ++i) {
        for (DictNode *dn = anon->fields->body[i]; dn; dn = dn->next) {
            base = (AstType *)dn->val;
            if (make_copy) {
                type = AstTypeCopy(base);
            } else {
                type = base;
            }
            type->offset += offset;
            DictSet(fields_dict,dn->key,type);
        }
    }
}

List *ParseClassOrUnionFields(Cctrl *cc, aoStr *name,
        unsigned int (*computeSize)(List *), unsigned int *_size)
{
    unsigned int size;
    char *fnptr_name;
    int fnptr_name_len;
    lexeme *tok, *tok_name;
    AstType *next_type, *base_type, *field_type;
    aoStr  *field_name;
    List *fields_list;

    tok = CctrlTokenGet(cc);

    if (!TokenPunctIs(tok,'{')) {
        CctrlTokenRewind(cc);
        return NULL;
    }

    fields_list = ListNew();

    while (1) {
        /* Peek not a consume */
        tok_name = CctrlTokenPeek(cc);
        if (TokenPunctIs(tok_name,'}')) {
            break;
        } else if (CctrlIsKeyword(cc,tok_name->start,tok_name->len)) {
            base_type = ParseBaseDeclSpec(cc);
        } else if (name && !memcmp(name->data,tok_name->start,tok_name->len)) {
            base_type = ParseBaseDeclSpec(cc);
            if (base_type == NULL) {
                base_type = AstClassType(NULL, aoStrDup(name), 8,0);
            }
        } else if (tok_name->tk_type == TK_KEYWORD) { 
            switch (tok_name->i64) {
                case KW_UNION: {
                    AstType *_union = ParseUnionDef(cc);
                    ListAppend(fields_list,_union);
                    CctrlTokenExpect(cc,';');
                    continue;
                }
                case KW_CLASS: {
                    AstType *cls = ParseClassDef(cc,0);
                    ListAppend(fields_list,cls);
                    CctrlTokenExpect(cc,';');
                    continue;
                }
                default:
                    loggerPanic("line %d: Unexpected keyword: %.*s\n",
                            tok->line,
                            tok->len,tok->start);
            }
        } else {
            loggerPanic("Unexpected type: %s\n", lexemeToString(tok_name));
        }

        while (1) {
            next_type = ParsePointerType(cc,base_type);
            tok_name = CctrlTokenGet(cc);
            if (!tok_name) {
                loggerPanic("line %ld: <type> <variable_name | (> expected got NULL\n",
                        cc->lineno);
            }
            if (!TokenPunctIs(tok_name, '(')) {
                next_type = ParseArrayDimensions(cc,next_type);
                field_name = aoStrDupRaw(tok_name->start,tok_name->len);
            } else {
               next_type = ParseFunctionPointerType(cc,&fnptr_name,&fnptr_name_len,next_type);
               field_name = aoStrDupRaw(fnptr_name,fnptr_name_len);
            }

            field_type = AstMakeClassField(next_type, 0);
            field_type->clsname = field_name;
            /* The list is here to ease calculating the offset of the class 
             * fields as they have been defined by the programmer... Hashing 
             * loses the ordering.*/
            ListAppend(fields_list,field_type);

            tok = CctrlTokenGet(cc);
            if (TokenPunctIs(tok, ',')) {
                continue;
            } else if (TokenPunctIs(tok,';')) {
                break;
            } else {
                loggerPanic("line %d: Unexpected token: %s\n",
                        tok->line,lexemeToString(tok));
           }
        }
    }
    CctrlTokenExpect(cc,'}');
    size = 0;
    size = computeSize(fields_list);

    if (_size) {
        *_size = size;
    }
    return fields_list;
}

unsigned int CalcUnionSize(List *fields) {
    unsigned int max = 0;
    AstType *type;
    ListForEach(fields) {
        type = it->value;
        if (max < type->size) {
            max = type->size;
        }
    }
    return max;
}

unsigned int CalcClassSize(List *fields) {
    unsigned int offset = 0;
    unsigned int size = 0;
    AstType *type;
    ListForEach(fields) {
        type = it->value;
        if (type->size < MAX_ALIGN) size = type->size;
        else                        size = MAX_ALIGN;

        if (offset % size != 0) {
            offset += size - offset % size;
        }

        type->offset = offset;
        offset += type->size;
    }
    return offset;
}

int CalcPadding(int offset, int size) {
    return offset % size == 0 ? 0 : size - offset % size;
}

Dict *ParseClassOffsets(int *real_size, List *fields, AstType *base_class,
        aoStr *clsname, int is_intrinsic)
{
    int offset;
    AstType *field;
    Dict *fields_dict = DictNew(&default_table_type);
    
    if (base_class) {
        offset = base_class->size;
        ParseFlattenAnnonymous(base_class,fields_dict,0,1);
    } else {
        offset = 0;
    }

    if (is_intrinsic) {
        *real_size = 16;
        ListForEach(fields) {
            field = it->value;
            field->offset = offset;
            offset+=field->size;
            if (field->clsname) {
                DictSet(fields_dict,aoStrMove(field->clsname),field);
            }
        }

        return fields_dict;
    }

    ListForEach(fields) {
        field = it->value;
        if (field->clsname == NULL && ParseIsClassOrUnion(field->kind)) {
            ParseFlattenAnnonymous(field,fields_dict,offset,0);
            offset += field->size;
            continue;
        } else {
            if (field->kind == AST_TYPE_POINTER && field->ptr->kind == AST_TYPE_CLASS) {
                if (field->ptr->clsname &&
                    field->ptr->clsname->len == clsname->len && 
                    !memcmp(clsname->data,
                        field->ptr->clsname->data,clsname->len)) {
                    field->fields = fields_dict;
                }
            }
            field->offset = offset;
            offset += field->size;
        }

        if (field->clsname) {
            DictSet(fields_dict,aoStrMove(field->clsname),field);
        }
    }

    *real_size = align(offset, 8);
    return fields_dict;
}

Dict *ParseUnionOffsets(int *real_size, List *fields) {
    int max_size;
    AstType *field;
    Dict *fields_dict = DictNew(&default_table_type);

    max_size = 0;
    ListForEach(fields) {
        field = it->value;
        if (max_size < field->size) {
            max_size = field->size;
        }
        if (field->clsname == NULL && ParseIsClassOrUnion(field->kind)) {
            ParseFlattenAnnonymous(field,fields_dict,0,0);
            continue;
        }

        field->offset = 0;
        if (field->clsname) {
            DictSet(fields_dict,field->clsname->data, field);
        }
    }
    *real_size = max_size;
    return fields_dict;
}

AstType *ParseClassOrUnion(Cctrl *cc, Dict *env,
        int is_class,
        unsigned int (*computeSize)(List *),
        int is_intrinsic)
{
    aoStr *tag = NULL;
    int real_size = 0;
    unsigned int class_size;
    lexeme *tok = CctrlTokenGet(cc);
    AstType *prev = NULL, *ref = NULL, *base_class = NULL;
    List *fields = NULL;
    Dict *fields_dict;
    cc->localenv = DictNewWithParent(cc->localenv);

    if (tok->tk_type == TK_IDENT) {
        tag = aoStrDupRaw(tok->start,tok->len);

        tok = CctrlTokenGet(cc);
        if (TokenPunctIs(tok, ':')) { // Class inheritance
            if (!is_class) {
                loggerPanic("line %d: Cannot use inheritance with a union\n",
                        tok->line);
            }
            tok = CctrlTokenGet(cc);
            if (tok->tk_type != TK_IDENT) {
                loggerPanic("line %d: Expected Identifier for class inheritance\n",
                        tok->line);
            }
            base_class = DictGetLen(cc->clsdefs,tok->start,tok->len);
            if (base_class == NULL) {
                loggerPanic("line %d: class %.*s has not been defined\n",
                        tok->line,tok->len,tok->start);
            }

            if (TokenPunctIs(CctrlTokenPeek(cc),',')) {
                loggerPanic("line %d: Only one base class allowed at this time\n",
                        tok->line);
            }
        } else {
            CctrlTokenRewind(cc);
        }
    }

    if (tag) {
        prev = DictGetLen(env, tag->data, tag->len);
    }

    fields = ParseClassOrUnionFields(cc,tag,computeSize,&class_size);

    if (prev && !fields) {
        return prev;
    }

    if (is_class) {
        fields_dict = ParseClassOffsets(&real_size,fields,base_class,tag,is_intrinsic);
    } else {
        fields_dict = ParseUnionOffsets(&real_size,fields);
    }
    ListRelease(fields,NULL);

    if (prev && fields_dict) {
        prev->fields = fields_dict;
        prev->size = real_size;
        return prev;
    }

    if (fields_dict) {
        ref = AstClassType(fields_dict,tag,real_size,is_intrinsic); 
        if (base_class) {
            ref->size += base_class->size;
        }
    } else {
        ref = AstClassType(NULL,tag,0,is_intrinsic); 
    }
    if (tag) {
        DictSet(env,tag->data,ref);
    }
    return ref;
}

AstType *ParseClassDef(Cctrl *cc, int is_intrinsic) {
    return ParseClassOrUnion(cc,cc->clsdefs,1,CalcClassSize,is_intrinsic);
}

AstType *ParseUnionDef(Cctrl *cc) {
    AstType *_union = ParseClassOrUnion(cc,cc->uniondefs,0,CalcUnionSize,0);
    _union->kind = AST_TYPE_UNION;
    return _union;
}

Ast *ParseVariableAssignment(Cctrl *cc, Ast *var, long terminator_flags) {
    Ast *init;
    int len;
    if (var->type->kind == AST_TYPE_ARRAY) {
        init = ParseDeclArrayInitInt(cc,var->type);
        if (init->kind == AST_STRING) {
            len = init->sval->len+1;
        } else {
            len = ListCount(init->arrayinit);
        }
        if (var->type->len == -1) {
            var->type->len = len;
            var->type->size = len * var->type->ptr->size;
        } else if (var->type->len != len) {
            loggerPanic("line %ld: Invalid array initializeer: expected %d items but got %d\n",
                cc->lineno,var->type->len, len);
        }
        lexeme *tok = CctrlTokenGet(cc);
        AssertTokenIsTerminator(tok,terminator_flags);
        return AstDecl(var,init);
    }

    init = ParseExpr(cc,16);
    lexeme *tok = CctrlTokenGet(cc);
    AssertTokenIsTerminator(tok,terminator_flags);
    if (var->kind == AST_GVAR) {
        init = AstI64Type(EvalIntConstExpr(init));
    }
    return AstDecl(var,init);
}

Ast *ParseVariableInitialiser(Cctrl *cc, Ast *var, long terminator_flags) {
    lexeme *tok = CctrlTokenGet(cc);
    if (TokenPunctIs(tok,'=')) {
        return ParseVariableAssignment(cc,var,terminator_flags);
    }
    if (var->type->len == -1) {
        loggerPanic("line %d: Missing array initializer: %s\n",
                tok->line,AstToString(var));
    }
    CctrlTokenRewind(cc);
    tok = CctrlTokenGet(cc);

    AssertTokenIsTerminator(tok,terminator_flags);
    return AstDecl(var,NULL);
}

Ast *ParseDecl(Cctrl *cc) {
    AstType *type;
    lexeme *varname;
    Ast *var, *ast;

    ParseDeclInternal(cc,&varname,&type);
    if (varname == NULL) {
        CctrlTokenExpect(cc,';');
        return NULL;
    }
    var = AstLVar(type,varname->start,varname->len);
    DictSet(cc->localenv,var->lname->data,var);
    if (cc->tmp_locals) {
        ListAppend(cc->tmp_locals, var);
    }
    ast = ParseVariableInitialiser(cc,var,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
    if (type->kind == AST_TYPE_AUTO) {
        ParseAssignAuto(cc,ast);
    }
    return ast;
}

Ast *ParseIfStatement(Cctrl *cc) {
    Ast *cond, *then, *els;
    lexeme *tok;

    CctrlTokenExpect(cc,'(');
    cond = ParseExpr(cc,16);
    CctrlTokenExpect(cc,')');
    then = ParseStatement(cc);
    tok = CctrlTokenGet(cc);
    if (tok && tok->tk_type == TK_KEYWORD && tok->i64 == KW_ELSE) {
        els = ParseStatement(cc);
        return AstIf(cond,then,els);
    }
    CctrlTokenRewind(cc);
    return AstIf(cond,then,NULL);
}

Ast *ParseOptDeclOrStmt(Cctrl *cc) {
    lexeme *tok = CctrlTokenGet(cc);
    if (TokenPunctIs(tok,';')) {
        return NULL;
    }
    CctrlTokenRewind(cc);
    return ParseDeclOrStatement(cc);
}

Ast *ParseOptExpr(Cctrl *cc) {
    lexeme *tok = CctrlTokenGet(cc);
    if (TokenPunctIs(tok,';')) {
        return NULL;
    }
    CctrlTokenRewind(cc);
    Ast *ast = ParseExpr(cc,16);
    tok = CctrlTokenGet(cc);
    AssertTokenIsTerminator(tok,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
    return ast;
}

Ast *ParseForStatement(Cctrl *cc) {
    Ast *forinit, *forcond, *forstep, *forbody;
    aoStr *for_begin, *for_end, *for_middle,
          *prev_begin, *prev_end;
    CctrlTokenExpect(cc,'(');

    prev_begin = cc->tmp_loop_begin;
    prev_end = cc->tmp_loop_end;

    for_begin = AstMakeLabel();
    for_middle = AstMakeLabel();
    for_end = AstMakeLabel();

    cc->tmp_loop_begin = for_middle;
    cc->tmp_loop_end = for_end;

    cc->localenv = DictNewWithParent(cc->localenv);
    forinit = ParseOptDeclOrStmt(cc);
    forcond = ParseOptExpr(cc);
    if (TokenPunctIs(CctrlTokenPeek(cc), ')')) {
        forstep = NULL;
    } else {
        forstep = ParseExpr(cc,16);
    }
    CctrlTokenExpect(cc,')');
    forbody = ParseStatement(cc);
    /* Go back up */
    cc->localenv = cc->localenv->parent;
    cc->tmp_loop_begin = prev_begin;
    cc->tmp_loop_end = prev_end;
    return AstFor(forinit,forcond,forstep,forbody,for_begin,for_middle,for_end);
}

Ast *ParseWhileStatement(Cctrl *cc) {
    Ast *whilecond, *whilebody;
    aoStr *while_begin, *while_end,
          *prev_begin, *prev_end;
    CctrlTokenExpect(cc,'(');

    prev_begin = cc->tmp_loop_begin;
    prev_end = cc->tmp_loop_end;

    while_begin = AstMakeLabel();
    while_end = AstMakeLabel();
    cc->tmp_loop_begin = while_begin;
    cc->tmp_loop_end = while_end;

    cc->localenv = DictNewWithParent(cc->localenv);
    whilecond = ParseExpr(cc,16);
    CctrlTokenExpect(cc,')');
    whilebody = ParseStatement(cc);
    cc->localenv = cc->localenv->parent;
    cc->tmp_loop_begin = prev_begin;
    cc->tmp_loop_end = prev_end;
    return AstWhile(whilecond,whilebody,while_begin, while_end);
}

Ast *ParseDoWhileStatement(Cctrl *cc) {
    Ast *whilecond, *whilebody;
    lexeme *tok;
    aoStr *while_begin, *while_end,
          *prev_begin, *prev_end;
    

    prev_begin = cc->tmp_loop_begin;
    prev_end = cc->tmp_loop_end;

    while_begin = AstMakeLabel();
    while_end = AstMakeLabel();
    cc->tmp_loop_begin = while_begin;
    cc->tmp_loop_end = while_end;

    cc->localenv = DictNewWithParent(cc->localenv);

    whilebody = ParseStatement(cc);
    
    tok = CctrlTokenGet(cc);

    if (tok->tk_type != TK_KEYWORD && tok->i64 != KW_WHILE) {
        loggerPanic("line %ld: Expected keyword 'while'\n",cc->lineno);
    }

    CctrlTokenExpect(cc, '(');
    whilecond = ParseExpr(cc,16);
    CctrlTokenExpect(cc,')');
    CctrlTokenExpect(cc,';');
    cc->localenv = cc->localenv->parent;
    cc->tmp_loop_begin = prev_begin;
    cc->tmp_loop_end = prev_end;
    return AstWhile(whilecond,whilebody,while_begin, while_end);
}

Ast *ParseBreakStatement(Cctrl *cc) {
    if (cc->tmp_loop_end == NULL) {
        loggerPanic("line %ld: Floating break, not inside a breakable statement\n",
                cc->lineno);
    }
    Ast *ast = AstBreak(cc->tmp_loop_end);
    CctrlTokenExpect(cc,';');
    return ast;
}

Ast *ParseContinueStatement(Cctrl *cc) {
    if (cc->tmp_loop_begin == NULL) {
        loggerPanic("line %ld: Floating continue, not inside a loop\n",
                cc->lineno);
    }
    Ast *ast = AstContinue(cc->tmp_loop_begin);
    CctrlTokenExpect(cc,';');
    return ast;
}

Ast *ParseReturnStatement(Cctrl *cc) {
    Ast *retval = ParseExpr(cc,16);
    AstType *check;
    long lineno = cc->lineno;
    CctrlTokenExpect(cc,';');
    /* A best attempt at trying to get the return type of a function */
    if (cc->tmp_rettype->kind == AST_TYPE_AUTO) {
        cc->tmp_rettype = ParseReturnAuto(cc,retval);
    }

    if (retval) check = retval->type;
    else        check = ast_void_type;

    if (check->kind == AST_TYPE_VOID && cc->tmp_rettype->kind == AST_TYPE_VOID) {
        return AstReturn(retval,cc->tmp_rettype);
    }

    AstType *t = AstTypeCheck(cc->tmp_rettype,retval);
    if (!t) {
        Ast *func = DictGet(cc->global_env,cc->tmp_fname->data);
        char *fstring, *expected, *got, *ast_str;

        if (func) {
            fstring = AstFunctionToString(func);
        } else {
            fstring = AstFunctionNameToString(cc->tmp_rettype,
                    cc->tmp_fname->data,cc->tmp_fname->len);
        }

        expected = AstTypeToColorString(cc->tmp_rettype);
        got = AstTypeToColorString(check);
        ast_str = AstLValueToString(retval);
        loggerWarning("line %ld: %s expected return type %s got %s %s\n",
                lineno,fstring,expected,got,ast_str);
        free(fstring);
        free(expected);
        free(got);
        free(ast_str);
    }
    return AstReturn(retval,cc->tmp_rettype);
}

Ast *ParseLabelBlock(Cctrl *cc, Ast *label) {
    Ast *stmt = ParseStatement(cc);
    List *stmts = ListNew();
    ListAppend(stmts,label);
    if (stmt) {
        ListAppend(stmts,stmt);
    }
    return AstCompountStatement(stmts);
}

Ast *ParseCaseLabel(Cctrl *cc, lexeme *tok) {
    if (cc->tmp_case_list == NULL) {
        loggerPanic("line %d: unexpected 'case' found\n",tok->line);
    }
    Ast *case_, *prev;
    lexeme *peek;
    aoStr *label;
    int begining;

    peek = CctrlTokenPeek(cc);
    if (TokenPunctIs(peek,':')) {
        if (ListEmpty(cc->tmp_case_list)) {
            begining = 0;
        } else {
            prev = cc->tmp_case_list->prev->value;
            begining = prev->case_end+1;
        }
    } else {
        begining = EvalIntConstExpr(ParseExpr(cc,16));
    }
    label = AstMakeLabel();
    tok = CctrlTokenPeek(cc);

    if (TokenPunctIs(tok,TK_ELLIPSIS)) {
        CctrlTokenGet(cc);
        int end = EvalIntConstExpr(ParseExpr(cc,16));
        CctrlTokenExpect(cc,':');
        if (begining > end) {
            loggerPanic("line %d: Condition is in the wrong order '%d' must be lower than '%d'\n",
                    tok->line,begining, end);
        }
        case_ = AstCase(label,begining,end);
        AssertUniqueSwitchCaseLabels(cc->tmp_case_list,case_);
        ListAppend(cc->tmp_case_list,case_);
    } else {
        CctrlTokenExpect(cc,':');
        case_ = AstCase(label,begining,begining);
        AssertUniqueSwitchCaseLabels(cc->tmp_case_list,case_);
           ListAppend(cc->tmp_case_list,case_);
    }

    return ParseLabelBlock(cc,case_);
}

Ast *ParseCreateSwitchJump(Ast *var, Ast *case_) {
    Ast *cond,*x,*y;
    if (case_->case_begin == case_->case_end) {
        cond = AstBinaryOp(TK_EQU_EQU, var, AstI64Type(case_->case_begin));
    } else {
        x = AstBinaryOp(TK_LESS_EQU,AstI64Type(case_->case_begin),var);
        y = AstBinaryOp(TK_LESS_EQU,var,AstI64Type(case_->case_end));
        cond = AstBinaryOp(TK_AND_AND,x,y);
    }
    return AstIf(cond,
            AstJump(case_->case_label->data,case_->case_label->len), NULL);
}

Ast *ParseDefaultStatement(Cctrl *cc, lexeme *tok) {
    CctrlTokenExpect(cc,':');
    if (cc->tmp_default_case) {
        loggerPanic("line %d: Duplicate default case\n",tok->line);
    }
    Ast *dest;
    cc->tmp_default_case = AstMakeLabel();
    dest = AstDest(
            cc->tmp_default_case->data,
            cc->tmp_default_case->len);
    return ParseLabelBlock(cc,dest);
}

Ast *ParseSwitchStatement(Cctrl *cc) {
    Ast *body, *cond, *tmp;
    List *switch_statement, *original_cases;
    aoStr *end_label, *original_default_label, *tmp_name,*original_break;

    CctrlTokenExpect(cc,'(');
    cond = ParseExpr(cc,16);
    if (!AstIsIntType(cond->type)) {
        loggerPanic("line %ld: Switch can only have int's at this time\n", cc->lineno);
    }
    CctrlTokenExpect(cc,')');

    original_break = cc->tmp_loop_end;
    original_default_label = cc->tmp_default_case;
    original_cases = cc->tmp_case_list;

    cc->tmp_case_list = ListNew();
    cc->tmp_default_case = NULL;

    end_label = AstMakeLabel();

    /* this is the current label */
    cc->tmp_loop_end = end_label;

    switch_statement = ListNew();
    tmp_name = AstMakeTmpName();
    tmp = AstLVar(cond->type, tmp_name->data, tmp_name->len);
    ListAppend(cc->tmp_locals,tmp);
    ListAppend(switch_statement, AstBinaryOp('=', tmp, cond));
    body = ParseStatement(cc);
    aoStrRelease(tmp_name);

    for (List *it = cc->tmp_case_list->next;
            it != cc->tmp_case_list; it = it->next) {
        ListAppend(switch_statement,
                ParseCreateSwitchJump(tmp,
                    it->value));
    }

    if (cc->tmp_default_case) {
        ListAppend(switch_statement,
                AstJump(cc->tmp_default_case->data,
                    cc->tmp_default_case->len));
    } else {
        ListAppend(switch_statement,
                AstJump(end_label->data,
                    end_label->len));
    }
    if (body) {
        ListAppend(switch_statement,body);
    }
    ListAppend(switch_statement, AstDest(end_label->data,end_label->len));

    cc->tmp_loop_end = original_break;
    cc->tmp_case_list = original_cases;
    cc->tmp_default_case = original_default_label;
    return AstCompountStatement(switch_statement);
}

/* Concatinate the label of the goto with the name of the function 
 * currently being parsed to be able to have uniqe goto labels  */
aoStr *createFunctionLevelGotoLabel(Cctrl *cc, lexeme *tok) {
    aoStr *label = aoStrNew();
    aoStrCatPrintf(label,".%s_%.*s",cc->tmp_fname->data,tok->len,tok->start);
    return label;
}

Ast *ParseStatement(Cctrl *cc) {
    lexeme *tok, *peek;
    aoStr *label;
    Ast *ret, *ast;
    Dict *env;
    tok = CctrlTokenGet(cc);

    if (tok->tk_type == TK_KEYWORD) {
        switch (tok->i64) {
            case KW_IF:       return ParseIfStatement(cc);
            case KW_FOR:      return ParseForStatement(cc);
            case KW_WHILE:    return ParseWhileStatement(cc);
            case KW_DO:       return ParseDoWhileStatement(cc);
            case KW_RETURN:   return ParseReturnStatement(cc);
            case KW_SWITCH:   return ParseSwitchStatement(cc);
            case KW_CASE:     return ParseCaseLabel(cc,tok);
            case KW_DEFAULT:  return ParseDefaultStatement(cc,tok);
            case KW_BREAK:    return ParseBreakStatement(cc);
            case KW_CONTINUE: return ParseContinueStatement(cc);
            case KW_STATIC: {
                env = cc->localenv;
                cc->localenv = NULL;
                AstType *type = ParseFullType(cc);
                type->is_static = 1;
                tok = CctrlTokenGet(cc);
                if (tok->tk_type != TK_IDENT) {
                    loggerPanic("line %d: Expected variable name at line\n",tok->line);
                }
                ast = AstGVar(type,tok->start,tok->len, 1);
                DictSet(env,ast->gname->data,ast);
                peek = CctrlTokenPeek(cc);
                if (TokenPunctIs(peek,'=')) {
                    ast = ParseVariableInitialiser(cc,ast,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
                    if (type->kind == AST_TYPE_AUTO) {
                        ParseAssignAuto(cc,ast);
                    }
                } else {
                    CctrlTokenExpect(cc,';');
                }
                cc->localenv = env;
                ListAppend(cc->ast_list,ast);
                return ast;
            }

            case KW_GOTO: {
                tok = CctrlTokenGet(cc);
                label = createFunctionLevelGotoLabel(cc,tok);
                ret = AstGoto(label);
                CctrlTokenExpect(cc,';');
                return ret;
            }

            /* It is possible to do: 
             * static_cast<Obj *>(_ptr)->x = 10;
             * */
            case KW_CAST: {
                CctrlTokenRewind(cc);
                ast = ParseExpr(cc,16);
                tok = CctrlTokenGet(cc);
                AssertTokenIsTerminator(tok,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
                return ast;
            }
            default:
                loggerPanic("line %d: Unexpected keyword: %.*s\n",
                        tok->line,tok->len,tok->start);
        }
    }

    if (tok->tk_type == TK_CHAR_CONST) {
        return ParseFloatingCharConst(cc,tok);
    }

    if (tok->tk_type == TK_STR) {
        CctrlTokenRewind(cc);
        /* HACK in holyc printf */
        return ParseFunctionArguments(cc,"printf",6,';');
    }

    /* Hacked in goto label ;) */
    peek = CctrlTokenPeek(cc);
    if (tok->tk_type == TK_IDENT && peek && peek->i64 == ':') {
        label = createFunctionLevelGotoLabel(cc,tok);
        ret = AstLabel(label);
        /* consume ':' */
        CctrlTokenExpect(cc,':');
        return ret;
    }

    if (TokenPunctIs(tok,'{')) {
        return ParseCompoundStatement(cc);
    }

    CctrlTokenRewind(cc);
    ast = ParseExpr(cc,16);
    tok = CctrlTokenGet(cc);
    AssertTokenIsTerminator(tok,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
    return ast;
}

Ast *ParseDeclOrStatement(Cctrl *cc) {
    lexeme *tok = CctrlTokenPeek(cc);
    if (!tok) {
        return NULL;
    }
    if (CctrlIsKeyword(cc,tok->start,tok->len)) {
        return ParseDecl(cc);
    }
    return ParseStatement(cc);
}

Ast *ParseCompoundStatement(Cctrl *cc) {
    List *stmts; 
    Ast *stmt, *var;
    AstType *base_type, *type, *next_type;
    lexeme *tok, *varname, *peek;

    stmts = ListNew();
    cc->localenv = DictNewWithParent(cc->localenv);
    tok = NULL;

    tok = CctrlTokenPeek(cc);

    while (tok && !TokenPunctIs(tok, '}')) {
        if (CctrlIsKeyword(cc,tok->start,tok->len)) {
            base_type = ParseBaseDeclSpec(cc);
            while (1) {
                next_type = ParsePointerType(cc,base_type);
                peek = CctrlTokenPeek(cc);
                
                if (!TokenPunctIs(peek,'(')) {
                    /* A normal variable */
                    varname = CctrlTokenGet(cc);
                    if (!varname) {
                        break;
                    }
                    type = ParseArrayDimensions(cc,next_type);
                    var = AstLVar(type,varname->start,varname->len);
                    DictSet(cc->localenv,var->lname->data,var);
                } else {
                    CctrlTokenGet(cc);
                    var = ParseFunctionPointer(cc,next_type);
                    DictSet(cc->localenv,var->fname->data,var);
                }

                if (cc->tmp_locals) {
                    ListAppend(cc->tmp_locals, var);
                }

                stmt = ParseVariableInitialiser(cc,var,PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);
                if (next_type->kind == AST_TYPE_AUTO) {
                    ParseAssignAuto(cc,stmt);
                }

                if (stmt) {
                    ListAppend(stmts,stmt);
                }
                CctrlTokenRewind(cc);
                tok = CctrlTokenGet(cc);

                if (TokenPunctIs(tok, ',')) {
                    continue;
                } else if (TokenPunctIs(tok,';')) {
                    break;
                } else {
                    loggerPanic("line %d: Unexpected token: %s\n",
                            tok->line, lexemeToString(tok));
                }
            }
        } else { 
            if ((stmt = ParseStatement(cc)) != NULL) {
                if (stmt->kind == AST_DECL && stmt->declvar->kind == AST_GVAR) {
                    tok = CctrlTokenPeek(cc);
                    continue;
                }
                ListAppend(stmts,stmt);
            } else {
                break;
            }
        }
        tok = CctrlTokenPeek(cc);
    }
    cc->localenv = cc->localenv->parent;
    CctrlTokenExpect(cc,'}');
    CctrlTokenPeek(cc);
    return AstCompountStatement(stmts);
}

Ast *ParseFunctionDef(Cctrl *cc, AstType *rettype,
        char *fname, int len, List *params, int has_var_args) 
{
    List *locals = ListNew();
    Ast *func = NULL;
    AstType *fn_type;

    cc->localenv = DictNewWithParent(cc->localenv);
    cc->tmp_locals = locals;

    /* Upgrade a prototype to an actual function */
    func = DictGetLen(cc->global_env,fname,len);
    if (!func) {
        cc->tmp_params = params;
        cc->tmp_rettype = rettype;
        fn_type = AstMakeFunctionType(cc->tmp_rettype, AstParamTypes(params));
        func = AstFunction(fn_type,fname,len,params,NULL,locals,
                has_var_args);
        DictSet(cc->global_env,func->fname->data,func);
    } else {
        switch (func->kind) {
            case AST_EXTERN_FUNC:
                loggerPanic("line %ld: Cannot redefine extern function: %.*s\n",
                        cc->lineno,len,fname);

            case AST_FUNC:
                loggerPanic("line %ld: Cannot redefine function: %.*s\n",
                        cc->lineno,len,fname);

            case AST_ASM_FUNC_BIND:
                loggerPanic("line %ld: Cannot redefine asm function: %.*s\n",
                        cc->lineno,len,fname);

            case AST_FUN_PROTO:
                /* upgrade prototype to a function */
                func->locals = cc->tmp_locals;
                AstReleaseList(func->params);
                func->params = params;
                cc->tmp_params = func->params;
                cc->tmp_rettype = func->type->rettype;
                fn_type = func->type;
                func->kind = AST_FUNC;
                break;

            default:
                loggerPanic("line %ld: unexpected function: %.*s -> %s\n",
                        cc->lineno,
                        len,fname, AstToString(func));
                break;
        }
    }

    /* XXX: This allows us to do recursion by parsing the body after */
    cc->tmp_fname = func->fname;
    Ast *body = ParseCompoundStatement(cc);
    func->body = body;
    fn_type->rettype = cc->tmp_rettype;

    cc->localenv = NULL;
    cc->tmp_locals = NULL;
    cc->tmp_rettype = NULL;
    cc->tmp_params = NULL;
    cc->tmp_fname = NULL;
    return func;
}

Ast *ParseExternFunctionProto(Cctrl *cc, AstType *rettype, char *fname, int len) {
    Ast *func;
    int has_var_args = 0;
    List *params;
    lexeme *tok;
    cc->localenv = DictNewWithParent(cc->localenv);

    params = ParseParams(cc,')', &has_var_args);
    tok = CctrlTokenGet(cc);
    if (!TokenPunctIs(tok, ';')) {
        loggerPanic("line %d: extern %.*s() cannot have a function body "
                "this will be defined elsewhere\n",
                tok->line,len,fname);
    }
    AstType *type = AstMakeFunctionType(rettype, AstParamTypes(params));
    func = AstFunction(type,fname,len,params,NULL,NULL,0);
    func->kind = AST_EXTERN_FUNC;
    DictSet(cc->global_env,func->fname->data,func);
    return func;
}

Ast *ParseFunctionOrDef(Cctrl *cc, AstType *rettype, char *fname, int len) {
    int has_var_args = 0;
    CctrlTokenExpect(cc,'(');
    cc->localenv = DictNewWithParent(cc->localenv);
    List *params = ParseParams(cc,')',&has_var_args);
    lexeme *tok = CctrlTokenGet(cc);
    if (TokenPunctIs(tok, '{')) {
        return ParseFunctionDef(cc,rettype,fname,len,params,has_var_args);
    } else {
        if (rettype->kind == AST_TYPE_AUTO) {
            loggerPanic("line %ld: auto cannot be used with a function prototype %.*s() at this time\n",
                    cc->lineno,len,fname);
        }
        AstType *type = AstMakeFunctionType(rettype, AstParamTypes(params));
        Ast *fn = AstFunction(type,fname,len,params,NULL,NULL,has_var_args);
        fn->kind = AST_FUN_PROTO;
        DictSet(cc->global_env,fn->fname->data,fn);
        return fn;
    }
}

Ast *ParseAsmFunctionBinding(Cctrl *cc) {
    lexeme *tok;
    aoStr *asm_fname, *c_fname;
    AstType *rettype;
    List *params;
    Ast *asm_func;
    int has_var_args = 0;

    tok = CctrlTokenGet(cc);

    if (tok->tk_type != TK_IDENT && tok->start[0] != '_') {
        loggerPanic("line %d: ASM function binds must begin with '_' got: %.*s\n",
                tok->line,tok->len,tok->start);
    }

    asm_fname = aoStrDupRaw(tok->start,tok->len);
    rettype = ParseDeclSpec(cc);

    if (rettype->kind == AST_TYPE_AUTO) {
        loggerPanic("line %ld: auto cannot be used with an assembly binding for function %s(), type cannot be automatically deduced\n",
                cc->lineno,
                asm_fname->data);
    }

    tok = CctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        loggerPanic("line %d: ASM function requires c function name\n",
                tok->line);
    }
    c_fname = aoStrDupRaw(tok->start,tok->len);
    cc->localenv = DictNewWithParent(cc->localenv);
    CctrlTokenExpect(cc,'(');

    params = ParseParams(cc,')',&has_var_args);

    asm_func = AstAsmFunctionBind(
            AstMakeFunctionType(rettype, AstParamTypes(params)),
            asm_fname,c_fname,params);

    cc->localenv = NULL;
    /* Map a c function to an ASM function */
    DictSet(cc->asm_funcs,c_fname->data,asm_func);
    CctrlTokenExpect(cc,';');
    return asm_func;
}

Ast *ParseToplevelDef(Cctrl *cc, int *is_global) {
    Ast *variable, *asm_block, *asm_func, *extern_func, *ast;
    AstType *type;
    lexeme *tok, *name, *peek;

    while (1) {
        if ((tok = CctrlTokenGet(cc)) == NULL) {
            return NULL;
        }

        if (tok->tk_type == TK_KEYWORD) {
            switch (tok->i64) {
                case KW_ASM_EXTERN: {
                    if ((asm_func = ParseAsmFunctionBinding(cc)) != NULL) {
                        return asm_func;
                    }
                    loggerPanic("line %d: Floating \"_extern\" keyword, expected \"_extern '_ASM_FUNC'\"\n", 
                            tok->line);
                }

                case KW_ASM: {
                    peek = CctrlTokenPeek(cc);
                    if (TokenPunctIs(peek,'{')) {
                        asm_block = PrsAsm(cc);
                        ListAppend(cc->asm_blocks,asm_block);
                        return asm_block;
                    }
                    loggerPanic("line %d: Floating \"asm\" keyword, expected \"asm {\"\n", 
                            tok->line);
                }

                case KW_EXTERN: {
                    tok = CctrlTokenGet(cc);
                    if (tok->tk_type == TK_STR && !strncmp(tok->start,"c",1)) {
                        type = ParseDeclSpec(cc);
                        name = CctrlTokenGet(cc);
                        CctrlTokenExpect(cc,'(');
                        extern_func = ParseExternFunctionProto(cc,type,
                                name->start,name->len);
                        return extern_func;
                    }
                    loggerPanic("line %d: Can only call external c functions for the "
                            "time being.\n", tok->line);
                }
                case KW_PUBLIC:
                case KW_PRIVATE:
                case KW_ATOMIC:
                case KW_INLINE:
                    continue;

                case KW_STATIC:
                    type = ParseDeclSpec(cc);
                    if (type == NULL) {
                        loggerPanic("line %ld: Expected type declaration\n",cc->lineno);
                    }
                    type->is_static = 1;
                    break;
                
                case KW_CLASS:
                    ParseClassDef(cc,0);
                    CctrlTokenExpect(cc,';');
                    continue;

                case KW_UNION:
                    ParseUnionDef(cc);
                    CctrlTokenExpect(cc,';');
                    continue;

                case KW_U0: 
                case KW_BOOL:
                case KW_I8:
                case KW_U8:
                case KW_I16:
                case KW_U16:
                case KW_I32:
                case KW_U32:
                case KW_I64:
                case KW_U64:
                case KW_F64:
                case KW_AUTO:
                    CctrlTokenRewind(cc);
                    type = ParseDeclSpec(cc);
                    break;

                case KW_IF:
                    cc->tmp_locals = ListNew();
                    *is_global = 1;
                    return ParseIfStatement(cc);

                case KW_WHILE:
                    cc->tmp_locals = ListNew();
                    *is_global = 1;
                    return ParseWhileStatement(cc);

                case KW_DO:
                    cc->tmp_locals = ListNew();
                    *is_global = 1;
                    return ParseDoWhileStatement(cc);

                case KW_FOR:
                    cc->tmp_locals = ListNew();
                    *is_global = 1;
                    return ParseForStatement(cc);

                default:
                    loggerPanic("line %d: Unexpected floating keyword: %.*s\n",
                            tok->line,tok->len,tok->start);
            }
        } else if (tok->tk_type == TK_IDENT) {
            /* top level function call */
            peek = CctrlTokenPeek(cc);
            if (TokenPunctIs(peek,'(')) {
                CctrlTokenGet(cc);
                ast = ParseFunctionArguments(cc,tok->start,tok->len,')');
                CctrlTokenExpect(cc,';');
                *is_global = 1;
                return ast;
            } else if ((variable = DictGetLen(cc->global_env,tok->start, tok->len))) {
                CctrlTokenRewind(cc);
                ast = ParseExpr(cc,16);
                CctrlTokenExpect(cc,';');
                *is_global = 1;
                return ast;
            } else {
                CctrlTokenRewind(cc);
                type = ParseDeclSpec(cc);
                if (type == NULL) {
                    loggerPanic("line %d: Undefined type: %.*s\n",
                            tok->line,tok->len,tok->start);
                }
            }
        } else if (tok->tk_type == TK_CHAR_CONST) {
            ast = ParseFloatingCharConst(cc,tok);
            *is_global = 1;
            return ast;
        } else if (tok->tk_type == TK_STR) {
            CctrlTokenRewind(cc);
            ast = ParseFunctionArguments(cc,"printf",6,';');
            *is_global = 1;
            return ast;
        }

        name = CctrlTokenGet(cc);

        if (name->tk_type == TK_KEYWORD) {
            switch (name->i64) {
                case KW_CLASS:
                    if (!AstIsIntType(type)) {
                        loggerPanic("line %ld: Can only make intrinsic types from integer types, got %s\n",
                                cc->lineno,AstTypeToString(type));
                    }
                    ParseClassDef(cc,1);
                    CctrlTokenExpect(cc,';');
                    continue;
                default:
                    loggerPanic("line %ld: %s can only prefix a class\n",
                            cc->lineno,AstTypeToString(type));
            }
        }

        if (name->tk_type != TK_IDENT) {
            loggerPanic("line %d: Identifier expected: got %s\n",
                    name->line, lexemeToString(name));
        }

        type = ParseArrayDimensions(cc,type);
        tok = CctrlTokenPeek(cc);

        if (TokenPunctIs(tok,'=') && type->kind != AST_TYPE_ARRAY) {
            variable = AstGVar(type,name->start,name->len,0);
            if (type->kind == AST_TYPE_AUTO) {
                loggerPanic("line %ld: auto cannot be used without an initialiser\n",
                        cc->lineno);
            }
            ast = AstDecl(variable,NULL);
            ListAppend(cc->ast_list,ast);
            DictSet(cc->global_env,variable->gname->data,variable);
            CctrlTokenRewind(cc);
            *is_global = 1;
            
            ast = ParseExpr(cc,16); // ParseVariableInitialiser(cc,variable,PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);
            CctrlTokenExpect(cc,';');
            return ast;
        } else if (type->kind == AST_TYPE_ARRAY) {
            variable = AstGVar(type,name->start,name->len,0);
            DictSet(cc->global_env,variable->gname->data,variable);
            ast = ParseVariableInitialiser(cc,variable,PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);
            if (type->kind == AST_TYPE_AUTO) {
                ParseAssignAuto(cc,ast);
            }
            return ast;
        }

        if (TokenPunctIs(tok, '(')) {
            return ParseFunctionOrDef(cc,type,name->start,name->len); 
        }

        if (TokenPunctIs(tok,';') || TokenPunctIs(tok, ',')) {
            CctrlTokenGet(cc);
            variable = AstGVar(type,name->start,name->len,0);
            DictSet(cc->global_env,variable->gname->data,variable);
            return AstDecl(variable,NULL);
        }
        loggerPanic("line %d: Cannot handle '%s'\n",tok->line,
                lexemeToString(tok));
    }
}

void ParseToAst(Cctrl *cc) {
    Ast *ast;
    lexeme *tok;
    int is_global = 0;
    while ((ast = ParseToplevelDef(cc,&is_global)) != NULL) {
        if (is_global) {
            ListAppend(cc->initalisers,ast);
            if (!ListEmpty(cc->tmp_locals)) {
                ListMergeAppend(cc->initaliser_locals,cc->tmp_locals);
            }
        } else if (ast->kind != AST_FUN_PROTO) {
            ListAppend(cc->ast_list,ast);
        }
        is_global = 0;
        tok = CctrlTokenPeek(cc);
        if (!tok) {
            break;
        }
        cc->tmp_locals = NULL;
        cc->localenv = NULL;
    }
}
