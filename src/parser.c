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
Ast *parseStatement(Cctrl *cc);
Ast *parseIfStatement(Cctrl *cc);
Ast *parseForStatement(Cctrl *cc);
Ast *parseVariableInitialiser(Cctrl *cc, Ast *var, long terminator_flags);
Ast *parseDecl(Cctrl *cc);
Ast *parseDeclOrStatement(Cctrl *cc);
Ast *parseCompoundStatement(Cctrl *cc);
AstType *parseClassDef(Cctrl *cc, int is_intrinsic);
AstType *parseUnionDef(Cctrl *cc);

/* Kinda cheating converting it to a string and calling printf */
Ast *parseFloatingCharConst(Cctrl *cc, lexeme *tok) {
    unsigned long ch = (unsigned long)tok->i64;
    Ast *ast;
    char str[9];
    List *argv = listNew();
    str[0] = ch & 0xFF;
    str[1] = ((unsigned long)ch) >> 8  & 0xFF;
    str[2] = ((unsigned long)ch) >> 16 & 0xFF;
    str[3] = ((unsigned long)ch) >> 24 & 0xFF;
    str[4] = ((unsigned long)ch) >> 32 & 0xFF;
    str[5] = ((unsigned long)ch) >> 40 & 0xFF;
    str[6] = ((unsigned long)ch) >> 48 & 0xFF;
    str[7] = ((unsigned long)ch) >> 56 & 0xFF;
    str[8] = '\0';

    ast = astString(str,sizeof(str));
    listAppend(cc->strings,ast);
    listAppend(argv,ast);
    ast = astFunctionCall(ast_void_type,"printf",6,argv,listNew());
    cctrlTokenExpect(cc,';');
    return ast;
}

Ast *parseDeclArrayInitInt(Cctrl *cc, AstType *type) {
    lexeme *tok = cctrlTokenGet(cc);
    List *initlist;
    Ast *init;

    if (type->kind == AST_TYPE_CLASS) {
        loggerPanic("line %d: Initalising an array of classes is unsupported\n",
                tok->line);
    }

    if (type->ptr->kind == AST_TYPE_CHAR && tok->tk_type == TK_STR) {
        return astString(tok->start,tok->len);
    }

    if (!tokenPunctIs(tok, '{')) {
        loggerPanic("line %ld: Expected intializer list starting with '{' got: '%c'",
                cc->lineno,(char)tok->i64);
    }

    initlist = listNew();
    while (1) {
        tok = cctrlTokenGet(cc);
        if (tokenPunctIs(tok, '}')) {
            break;
        }
        cctrlTokenRewind(cc);
        if (tokenPunctIs(tok,'{')) {
            init = parseDeclArrayInitInt(cc,type->ptr);
            tok = cctrlTokenGet(cc);
            listAppend(initlist,init);
            if (tokenPunctIs(tok,'}')) {
                init = astArrayInit(initlist);
                return init;
            }
            continue;
        } else {
            init = parseExpr(cc,16);
            if ((astGetResultType('=', init->type, type->ptr)) == NULL) {
                loggerPanic("line %ld: Incompatiable types: %s %s\n",
                        cc->lineno,
                        astToString(init),
                        astTypeToString(type->ptr));
            }
        }
        listAppend(initlist,init);

        tok = cctrlTokenGet(cc);
        if (!tokenPunctIs(tok, ',')) {
            cctrlTokenRewind(cc);
        }
    }
    return astArrayInit(initlist);
}

void parseFlattenAnnonymous(AstType *anon, Dict *fields_dict,
        int offset, int make_copy)
{
    AstType *base, *type;
    for (int i = 0; i < anon->fields->capacity; ++i) {
        for (DictNode *dn = anon->fields->body[i]; dn; dn = dn->next) {
            base = (AstType *)dn->val;
            if (make_copy) {
                type = astTypeCopy(base);
            } else {
                type = base;
            }
            type->offset += offset;
            dictSet(fields_dict,dn->key,type);
        }
    }
}

List *parseClassOrUnionFields(Cctrl *cc, aoStr *name,
        unsigned int (*computeSize)(List *), unsigned int *_size)
{
    unsigned int size;
    char *fnptr_name;
    int fnptr_name_len;
    lexeme *tok, *tok_name;
    AstType *next_type, *base_type, *field_type;
    aoStr  *field_name;
    List *fields_list;

    tok = cctrlTokenGet(cc);

    if (!tokenPunctIs(tok,'{')) {
        cctrlTokenRewind(cc);
        return NULL;
    }

    fields_list = listNew();

    while (1) {
        /* Peek not a consume */
        tok_name = cctrlTokenPeek(cc);
        if (tokenPunctIs(tok_name,'}')) {
            break;
        } else if (cctrlIsKeyword(cc,tok_name->start,tok_name->len)) {
            base_type = parseBaseDeclSpec(cc);
        } else if (name && !memcmp(name->data,tok_name->start,tok_name->len)) {
            base_type = parseBaseDeclSpec(cc);
            if (base_type == NULL) {
                base_type = astClassType(NULL, aoStrDup(name), 8,0);
            }
        } else if (tok_name->tk_type == TK_KEYWORD) { 
            switch (tok_name->i64) {
                case KW_UNION: {
                    AstType *_union = parseUnionDef(cc);
                    listAppend(fields_list,_union);
                    cctrlTokenExpect(cc,';');
                    continue;
                }
                case KW_CLASS: {
                    AstType *cls = parseClassDef(cc,0);
                    listAppend(fields_list,cls);
                    cctrlTokenExpect(cc,';');
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
            next_type = parsePointerType(cc,base_type);
            tok_name = cctrlTokenGet(cc);
            if (!tok_name) {
                loggerPanic("line %ld: <type> <variable_name | (> expected got NULL\n",
                        cc->lineno);
            }
            if (!tokenPunctIs(tok_name, '(')) {
                next_type = parseArrayDimensions(cc,next_type);
                field_name = aoStrDupRaw(tok_name->start,tok_name->len);
            } else {
               next_type = parseFunctionPointerType(cc,&fnptr_name,&fnptr_name_len,next_type);
               field_name = aoStrDupRaw(fnptr_name,fnptr_name_len);
            }

            field_type = astMakeClassField(next_type, 0);
            field_type->clsname = field_name;
            /* The list is here to ease calculating the offset of the class 
             * fields as they have been defined by the programmer... Hashing 
             * loses the ordering.*/
            listAppend(fields_list,field_type);

            tok = cctrlTokenGet(cc);
            if (tokenPunctIs(tok, ',')) {
                continue;
            } else if (tokenPunctIs(tok,';')) {
                break;
            } else {
                loggerPanic("line %d: Unexpected token: %s\n",
                        tok->line,lexemeToString(tok));
           }
        }
    }
    cctrlTokenExpect(cc,'}');
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
    listForEach(fields) {
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
    listForEach(fields) {
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

Dict *parseClassOffsets(int *real_size, List *fields, AstType *base_class,
        aoStr *clsname, int is_intrinsic)
{
    int offset,size,padding;
    AstType *field;
    Dict *fields_dict = dictNew(&default_table_type);

    /* XXX: Assumes the class definition will be made later */
    if (listEmpty(fields)) {
        return fields_dict;
    }
    
    if (base_class) {
        offset = base_class->size;
        parseFlattenAnnonymous(base_class,fields_dict,0,1);
    } else {
        offset = 0;
    }

    if (is_intrinsic) {
        *real_size = 16;
        listForEach(fields) {
            field = it->value;
            field->offset = offset;
            offset+=field->size;
            if (field->clsname) {
                dictSet(fields_dict,aoStrMove(field->clsname),field);
            }
        }

        return fields_dict;
    }

    listForEach(fields) {
        field = it->value;
        if (field->clsname == NULL && parseIsClassOrUnion(field->kind)) {
            parseFlattenAnnonymous(field,fields_dict,offset,0);
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
            /* Align to the type not the size of the array */
            if (field->kind == AST_TYPE_ARRAY) {
                size = field->ptr->size;
            } else {
                size = field->size;
            }
            padding = CalcPadding(offset,size);
            offset += (padding);
            field->offset = offset;
            offset += (field->size);
        }

        if (field->clsname) {
            dictSet(fields_dict,aoStrMove(field->clsname),field);
        }
    }

    *real_size = offset + CalcPadding(offset, 8);
    return fields_dict;
}

Dict *parseUnionOffsets(int *real_size, List *fields) {
    int max_size;
    AstType *field;
    Dict *fields_dict = dictNew(&default_table_type);

    max_size = 0;
    listForEach(fields) {
        field = it->value;
        if (max_size < field->size) {
            max_size = field->size;
        }
        if (field->clsname == NULL && parseIsClassOrUnion(field->kind)) {
            parseFlattenAnnonymous(field,fields_dict,0,0);
            continue;
        }

        field->offset = 0;
        if (field->clsname) {
            dictSet(fields_dict,field->clsname->data, field);
        }
    }
    *real_size = max_size;
    return fields_dict;
}

AstType *parseClassOrUnion(Cctrl *cc, Dict *env,
        int is_class,
        unsigned int (*computeSize)(List *),
        int is_intrinsic)
{
    aoStr *tag = NULL;
    int real_size = 0;
    unsigned int class_size;
    lexeme *tok = cctrlTokenGet(cc);
    AstType *prev = NULL, *ref = NULL, *base_class = NULL;
    List *fields = NULL;
    Dict *fields_dict;
    cc->localenv = dictNewWithParent(cc->localenv);

    if (tok->tk_type == TK_IDENT) {
        tag = aoStrDupRaw(tok->start,tok->len);

        tok = cctrlTokenGet(cc);
        if (tokenPunctIs(tok, ':')) { // Class inheritance
            if (!is_class) {
                loggerPanic("line %d: Cannot use inheritance with a union\n",
                        tok->line);
            }
            tok = cctrlTokenGet(cc);
            if (tok->tk_type != TK_IDENT) {
                loggerPanic("line %d: Expected Identifier for class inheritance\n",
                        tok->line);
            }
            base_class = dictGetLen(cc->clsdefs,tok->start,tok->len);
            if (base_class == NULL) {
                loggerPanic("line %d: class %.*s has not been defined\n",
                        tok->line,tok->len,tok->start);
            }

            if (tokenPunctIs(cctrlTokenPeek(cc),',')) {
                loggerPanic("line %d: Only one base class allowed at this time\n",
                        tok->line);
            }
        } else {
            cctrlTokenRewind(cc);
        }
    }

    if (tag) {
        prev = dictGetLen(env, tag->data, tag->len);
    }

    fields = parseClassOrUnionFields(cc,tag,computeSize,&class_size);

    if (prev && !fields) {
        return prev;
    }

    if (is_class) {
        fields_dict = parseClassOffsets(&real_size,fields,base_class,tag,is_intrinsic);
    } else {
        fields_dict = parseUnionOffsets(&real_size,fields);
    }
    listRelease(fields,NULL);

    if (prev && fields_dict) {
        prev->fields = fields_dict;
        prev->size = real_size;
        return prev;
    }

    if (fields_dict) {
        ref = astClassType(fields_dict,tag,real_size,is_intrinsic); 
        if (base_class) {
            ref->size += base_class->size;
        }
    } else {
        ref = astClassType(NULL,tag,0,is_intrinsic); 
    }
    if (tag) {
        dictSet(env,tag->data,ref);
    }
    return ref;
}

AstType *parseClassDef(Cctrl *cc, int is_intrinsic) {
    return parseClassOrUnion(cc,cc->clsdefs,1,CalcClassSize,is_intrinsic);
}

AstType *parseUnionDef(Cctrl *cc) {
    AstType *_union = parseClassOrUnion(cc,cc->uniondefs,0,CalcUnionSize,0);
    _union->kind = AST_TYPE_UNION;
    return _union;
}

Ast *parseVariableAssignment(Cctrl *cc, Ast *var, long terminator_flags) {
    Ast *init;
    int len;
    if (var->type->kind == AST_TYPE_ARRAY) {
        init = parseDeclArrayInitInt(cc,var->type);
        if (init->kind == AST_STRING) {
            len = init->sval->len+1;
        } else {
            len = listCount(init->arrayinit);
        }
        if (var->type->len == -1) {
            var->type->len = len;
            var->type->size = len * var->type->ptr->size;
        } else if (var->type->len != len) {
            loggerPanic("line %ld: Invalid array initializeer: expected %d items but got %d\n",
                cc->lineno,var->type->len, len);
        }
        lexeme *tok = cctrlTokenGet(cc);
        assertTokenIsTerminator(tok,terminator_flags);
        return astDecl(var,init);
    }

    init = parseExpr(cc,16);
    lexeme *tok = cctrlTokenGet(cc);
    assertTokenIsTerminator(tok,terminator_flags);
    if (var->kind == AST_GVAR && var->type->kind == AST_TYPE_INT) {
        init = astI64Type(evalIntConstExpr(init));
    }
    return astDecl(var,init);
}

Ast *parseVariableInitialiser(Cctrl *cc, Ast *var, long terminator_flags) {
    lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok,'=')) {
        return parseVariableAssignment(cc,var,terminator_flags);
    }
    if (var->type->len == -1) {
        loggerPanic("line %d: Missing array initializer: %s\n",
                tok->line,astToString(var));
    }
    cctrlTokenRewind(cc);
    tok = cctrlTokenGet(cc);

    assertTokenIsTerminator(tok,terminator_flags);
    return astDecl(var,NULL);
}

Ast *parseDecl(Cctrl *cc) {
    AstType *type;
    lexeme *varname;
    Ast *var, *ast;

    parseDeclInternal(cc,&varname,&type);
    if (varname == NULL) {
        cctrlTokenExpect(cc,';');
        return NULL;
    }
    var = astLVar(type,varname->start,varname->len);
    if (!dictSet(cc->localenv,var->lname->data,var)) {
        loggerPanic("line: %ld variable %s already declared\n",
                cc->lineno,astLValueToString(var));
    }
    if (cc->tmp_locals) {
        listAppend(cc->tmp_locals, var);
    }
    ast = parseVariableInitialiser(cc,var,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
    if (type->kind == AST_TYPE_AUTO) {
        parseAssignAuto(cc,ast);
    }
    return ast;
}

Ast *parseIfStatement(Cctrl *cc) {
    Ast *cond, *then, *els;
    lexeme *tok;

    cctrlTokenExpect(cc,'(');
    cond = parseExpr(cc,16);
    cctrlTokenExpect(cc,')');
    then = parseStatement(cc);
    tok = cctrlTokenGet(cc);
    if (tok && tok->tk_type == TK_KEYWORD && tok->i64 == KW_ELSE) {
        els = parseStatement(cc);
        return astIf(cond,then,els);
    }
    cctrlTokenRewind(cc);
    return astIf(cond,then,NULL);
}

Ast *parseOptDeclOrStmt(Cctrl *cc) {
    lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok,';')) {
        return NULL;
    }
    cctrlTokenRewind(cc);
    return parseDeclOrStatement(cc);
}

Ast *parseOptExpr(Cctrl *cc) {
    lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok,';')) {
        return NULL;
    }
    cctrlTokenRewind(cc);
    Ast *ast = parseExpr(cc,16);
    tok = cctrlTokenGet(cc);
    assertTokenIsTerminator(tok,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
    return ast;
}

Ast *parseForStatement(Cctrl *cc) {
    Ast *forinit, *forcond, *forstep, *forbody;
    aoStr *for_begin, *for_end, *for_middle,
          *prev_begin, *prev_end;
    cctrlTokenExpect(cc,'(');

    prev_begin = cc->tmp_loop_begin;
    prev_end = cc->tmp_loop_end;

    for_begin = astMakeLabel();
    for_middle = astMakeLabel();
    for_end = astMakeLabel();

    cc->tmp_loop_begin = for_middle;
    cc->tmp_loop_end = for_end;

    cc->localenv = dictNewWithParent(cc->localenv);
    forinit = parseOptDeclOrStmt(cc);
    forcond = parseOptExpr(cc);
    if (tokenPunctIs(cctrlTokenPeek(cc), ')')) {
        forstep = NULL;
    } else {
        forstep = parseExpr(cc,16);
    }
    cctrlTokenExpect(cc,')');
    forbody = parseStatement(cc);
    /* Go back up */
    cc->localenv = cc->localenv->parent;
    cc->tmp_loop_begin = prev_begin;
    cc->tmp_loop_end = prev_end;
    return astFor(forinit,forcond,forstep,forbody,for_begin,for_middle,for_end);
}

Ast *parseWhileStatement(Cctrl *cc) {
    Ast *whilecond, *whilebody;
    aoStr *while_begin, *while_end,
          *prev_begin, *prev_end;
    cctrlTokenExpect(cc,'(');

    prev_begin = cc->tmp_loop_begin;
    prev_end = cc->tmp_loop_end;

    while_begin = astMakeLabel();
    while_end = astMakeLabel();
    cc->tmp_loop_begin = while_begin;
    cc->tmp_loop_end = while_end;

    cc->localenv = dictNewWithParent(cc->localenv);
    whilecond = parseExpr(cc,16);
    cctrlTokenExpect(cc,')');
    whilebody = parseStatement(cc);
    cc->localenv = cc->localenv->parent;
    cc->tmp_loop_begin = prev_begin;
    cc->tmp_loop_end = prev_end;
    return astWhile(whilecond,whilebody,while_begin, while_end);
}

Ast *parseDoWhileStatement(Cctrl *cc) {
    Ast *whilecond, *whilebody;
    lexeme *tok;
    aoStr *while_begin, *while_end,
          *prev_begin, *prev_end;
    

    prev_begin = cc->tmp_loop_begin;
    prev_end = cc->tmp_loop_end;

    while_begin = astMakeLabel();
    while_end = astMakeLabel();
    cc->tmp_loop_begin = while_begin;
    cc->tmp_loop_end = while_end;

    cc->localenv = dictNewWithParent(cc->localenv);

    whilebody = parseStatement(cc);
    
    tok = cctrlTokenGet(cc);

    if (tok->tk_type != TK_KEYWORD && tok->i64 != KW_WHILE) {
        loggerPanic("line %ld: Expected keyword 'while'\n",cc->lineno);
    }

    cctrlTokenExpect(cc, '(');
    whilecond = parseExpr(cc,16);
    cctrlTokenExpect(cc,')');
    cctrlTokenExpect(cc,';');
    cc->localenv = cc->localenv->parent;
    cc->tmp_loop_begin = prev_begin;
    cc->tmp_loop_end = prev_end;
    return astDoWhile(whilecond,whilebody,while_begin, while_end);
}

Ast *parseBreakStatement(Cctrl *cc) {
    if (cc->tmp_loop_end == NULL) {
        loggerPanic("line %ld: Floating break, not inside a breakable statement\n",
                cc->lineno);
    }
    Ast *ast = astBreak(cc->tmp_loop_end);
    cctrlTokenExpect(cc,';');
    return ast;
}

Ast *parseContinueStatement(Cctrl *cc) {
    if (cc->tmp_loop_begin == NULL) {
        loggerPanic("line %ld: Floating continue, not inside a loop\n",
                cc->lineno);
    }
    Ast *ast = astContinue(cc->tmp_loop_begin);
    cctrlTokenExpect(cc,';');
    return ast;
}

Ast *parseReturnStatement(Cctrl *cc) {
    Ast *retval = parseExpr(cc,16);
    AstType *check;
    long lineno = cc->lineno;
    cctrlTokenExpect(cc,';');
    /* A best attempt at trying to get the return type of a function */
    if (cc->tmp_rettype->kind == AST_TYPE_AUTO) {
        cc->tmp_rettype = parseReturnAuto(cc,retval);
    }

    if (retval) check = retval->type;
    else        check = ast_void_type;

    if (check->kind == AST_TYPE_VOID && cc->tmp_rettype->kind == AST_TYPE_VOID) {
        return astReturn(retval,cc->tmp_rettype);
    }

    AstType *t = astTypeCheck(cc->tmp_rettype,retval);
    if (!t) {
        Ast *func = dictGet(cc->global_env,cc->tmp_fname->data);
        char *fstring, *expected, *got, *ast_str;

        if (func) {
            fstring = astFunctionToString(func);
        } else {
            fstring = astFunctionNameToString(cc->tmp_rettype,
                    cc->tmp_fname->data,cc->tmp_fname->len);
        }

        expected = astTypeToColorString(cc->tmp_rettype);
        got = astTypeToColorString(check);
        ast_str = astLValueToString(retval);
        loggerWarning("line %ld: %s expected return type %s got %s %s\n",
                lineno,fstring,expected,got,ast_str);
        free(fstring);
        free(expected);
        free(got);
        free(ast_str);
    }
    return astReturn(retval,cc->tmp_rettype);
}

Ast *parseLabelBlock(Cctrl *cc, Ast *label) {
    Ast *stmt = parseStatement(cc);
    List *stmts = listNew();
    listAppend(stmts,label);
    if (stmt) {
        listAppend(stmts,stmt);
    }
    return astCompountStatement(stmts);
}

Ast *parseCaseLabel(Cctrl *cc, lexeme *tok) {
    if (cc->tmp_case_list == NULL) {
        loggerPanic("line %d: unexpected 'case' found\n",tok->line);
    }
    Ast *case_, *prev;
    lexeme *peek;
    aoStr *label;
    int begining;

    peek = cctrlTokenPeek(cc);
    if (tokenPunctIs(peek,':')) {
        if (listEmpty(cc->tmp_case_list)) {
            begining = 0;
        } else {
            prev = cc->tmp_case_list->prev->value;
            begining = prev->case_end+1;
        }
    } else {
        begining = evalIntConstExpr(parseExpr(cc,16));
    }
    label = astMakeLabel();
    tok = cctrlTokenPeek(cc);

    if (tokenPunctIs(tok,TK_ELLIPSIS)) {
        cctrlTokenGet(cc);
        int end = evalIntConstExpr(parseExpr(cc,16));
        cctrlTokenExpect(cc,':');
        if (begining > end) {
            loggerPanic("line %d: Condition is in the wrong order '%d' must be lower than '%d'\n",
                    tok->line,begining, end);
        }
        case_ = astCase(label,begining,end);
        assertUniqueSwitchCaseLabels(cc->tmp_case_list,case_);
        listAppend(cc->tmp_case_list,case_);
    } else {
        cctrlTokenExpect(cc,':');
        case_ = astCase(label,begining,begining);
        assertUniqueSwitchCaseLabels(cc->tmp_case_list,case_);
           listAppend(cc->tmp_case_list,case_);
    }

    return parseLabelBlock(cc,case_);
}

Ast *parseCreateSwitchJump(Ast *var, Ast *case_) {
    Ast *cond,*x,*y;
    if (case_->case_begin == case_->case_end) {
        cond = astBinaryOp(TK_EQU_EQU, var, astI64Type(case_->case_begin));
    } else {
        x = astBinaryOp(TK_LESS_EQU,astI64Type(case_->case_begin),var);
        y = astBinaryOp(TK_LESS_EQU,var,astI64Type(case_->case_end));
        cond = astBinaryOp(TK_AND_AND,x,y);
    }
    return astIf(cond,
            astJump(case_->case_label->data,case_->case_label->len), NULL);
}

Ast *parseDefaultStatement(Cctrl *cc, lexeme *tok) {
    cctrlTokenExpect(cc,':');
    if (cc->tmp_default_case) {
        loggerPanic("line %d: Duplicate default case\n",tok->line);
    }
    Ast *dest;
    cc->tmp_default_case = astMakeLabel();
    dest = astDest(
            cc->tmp_default_case->data,
            cc->tmp_default_case->len);
    return parseLabelBlock(cc,dest);
}

Ast *parseSwitchStatement(Cctrl *cc) {
    Ast *body, *cond, *tmp;
    List *switch_statement, *original_cases;
    aoStr *end_label, *original_default_label, *tmp_name,*original_break;

    cctrlTokenExpect(cc,'(');
    cond = parseExpr(cc,16);
    if (!astIsIntType(cond->type)) {
        loggerPanic("line %ld: Switch can only have int's at this time\n", cc->lineno);
    }
    cctrlTokenExpect(cc,')');

    original_break = cc->tmp_loop_end;
    original_default_label = cc->tmp_default_case;
    original_cases = cc->tmp_case_list;

    cc->tmp_case_list = listNew();
    cc->tmp_default_case = NULL;

    end_label = astMakeLabel();

    /* this is the current label */
    cc->tmp_loop_end = end_label;

    switch_statement = listNew();
    tmp_name = astMakeTmpName();
    tmp = astLVar(cond->type, tmp_name->data, tmp_name->len);
    listAppend(cc->tmp_locals,tmp);
    listAppend(switch_statement, astBinaryOp('=', tmp, cond));
    body = parseStatement(cc);
    aoStrRelease(tmp_name);

    for (List *it = cc->tmp_case_list->next;
            it != cc->tmp_case_list; it = it->next) {
        listAppend(switch_statement,
                parseCreateSwitchJump(tmp,
                    it->value));
    }

    if (cc->tmp_default_case) {
        listAppend(switch_statement,
                astJump(cc->tmp_default_case->data,
                    cc->tmp_default_case->len));
    } else {
        listAppend(switch_statement,
                astJump(end_label->data,
                    end_label->len));
    }
    if (body) {
        listAppend(switch_statement,body);
    }
    listAppend(switch_statement, astDest(end_label->data,end_label->len));

    cc->tmp_loop_end = original_break;
    cc->tmp_case_list = original_cases;
    cc->tmp_default_case = original_default_label;
    return astCompountStatement(switch_statement);
}

/* Concatinate the label of the goto with the name of the function 
 * currently being parsed to be able to have uniqe goto labels  */
aoStr *createFunctionLevelGotoLabel(Cctrl *cc, lexeme *tok) {
    aoStr *label = aoStrNew();
    aoStrCatPrintf(label,".%s_%.*s",cc->tmp_fname->data,tok->len,tok->start);
    return label;
}

Ast *parseStatement(Cctrl *cc) {
    lexeme *tok, *peek;
    aoStr *label;
    Ast *ret, *ast;
    Dict *env;
    tok = cctrlTokenGet(cc);

    if (tok->tk_type == TK_KEYWORD) {
        switch (tok->i64) {
            case KW_IF:       return parseIfStatement(cc);
            case KW_FOR:      return parseForStatement(cc);
            case KW_WHILE:    return parseWhileStatement(cc);
            case KW_DO:       return parseDoWhileStatement(cc);
            case KW_RETURN:   return parseReturnStatement(cc);
            case KW_SWITCH:   return parseSwitchStatement(cc);
            case KW_CASE:     return parseCaseLabel(cc,tok);
            case KW_DEFAULT:  return parseDefaultStatement(cc,tok);
            case KW_BREAK:    return parseBreakStatement(cc);
            case KW_CONTINUE: return parseContinueStatement(cc);
            case KW_STATIC: {
                env = cc->localenv;
                cc->localenv = NULL;

                AstType *type = parseFullType(cc);
                tok = cctrlTokenGet(cc);

                if (tok->tk_type != TK_IDENT) {
                    loggerPanic("line %d: Expected variable name at line\n",tok->line);
                }
                type = parseArrayDimensions(cc,type);

                Ast *gvar_ast = astGVar(type,tok->start,tok->len,1);


                if (type->kind == AST_TYPE_ARRAY) {
                    ast = parseVariableInitialiser(cc,gvar_ast,
                            PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);

                    cc->localenv = env;
                    dictSet(env,gvar_ast->gname->data,gvar_ast);
                    listAppend(cc->ast_list,ast);
                    return ast;
                }

                peek = cctrlTokenPeek(cc);
                if (tokenPunctIs(peek,'=')) {
                    ast = parseVariableInitialiser(cc,gvar_ast,
                            PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);

                    if (type->kind == AST_TYPE_AUTO) {
                        gvar_ast->type = ast->declinit->type;
                        if (ast->declinit->kind == AST_STRING) {
                            /* @Leak: we've just lost the original string array
                             * that was parsed... or have we? Possibly not as it
                             * would exist on the declinit->type and we do not 
                             * change it */
                            gvar_ast->type = astMakePointerType(ast_u8_type);
                            gvar_ast->type->len = ast->declinit->type->len;
                        }
                    }

                    if (ast->declinit->kind == AST_ASM_FUNCALL ||
                        parseIsFunctionCall(ast->declinit))
                    {
                        loggerPanic("line %ld: '%s %s' must be a compile time "
                                "constant\n",
                                cc->lineno,
                                astTypeToColorString(gvar_ast->type),
                                astLValueToString(ast));
                    }
                } else {
                    cctrlTokenExpect(cc,';');
                    ast = astDecl(gvar_ast,NULL);
                }
                cc->localenv = env;
                dictSet(env,gvar_ast->gname->data,gvar_ast);
                listAppend(cc->ast_list,ast);
                return ast;
            }

            case KW_GOTO: {
                tok = cctrlTokenGet(cc);
                label = createFunctionLevelGotoLabel(cc,tok);
                ret = astGoto(label);
                cctrlTokenExpect(cc,';');
                return ret;
            }

            /**
             * XXX: DELETE cast<>
             * It is possible to do: 
             * cast<Obj *>(_ptr)->x = 10;
             * */
            case KW_CAST: {
                cctrlTokenRewind(cc);
                ast = parseExpr(cc,16);
                tok = cctrlTokenGet(cc);
                assertTokenIsTerminator(tok,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
                return ast;
            }
            default:
                loggerPanic("line %d: Unexpected keyword: %.*s\n",
                        tok->line,tok->len,tok->start);
        }
    }

    if (tok->tk_type == TK_CHAR_CONST) {
        return parseFloatingCharConst(cc,tok);
    }

    if (tok->tk_type == TK_STR) {
        cctrlTokenRewind(cc);
        /* HACK in holyc printf */
        return parseFunctionArguments(cc,"printf",6,';');
    }

    /* Hacked in goto label ;) */
    peek = cctrlTokenPeek(cc);
    if (tok->tk_type == TK_IDENT && peek && peek->i64 == ':') {
        label = createFunctionLevelGotoLabel(cc,tok);
        ret = astLabel(label);
        /* consume ':' */
        cctrlTokenExpect(cc,':');
        return ret;
    }

    if (tokenPunctIs(tok,'{')) {
        return parseCompoundStatement(cc);
    }

    cctrlTokenRewind(cc);
    ast = parseExpr(cc,16);
    tok = cctrlTokenGet(cc);
    assertTokenIsTerminator(tok,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
    return ast;
}

Ast *parseDeclOrStatement(Cctrl *cc) {
    lexeme *tok = cctrlTokenPeek(cc);
    if (!tok) {
        return NULL;
    }
    if (cctrlIsKeyword(cc,tok->start,tok->len)) {
        return parseDecl(cc);
    }
    return parseStatement(cc);
}

Ast *parseCompoundStatement(Cctrl *cc) {
    List *stmts; 
    Ast *stmt, *var;
    AstType *base_type, *type, *next_type;
    lexeme *tok, *varname, *peek;

    stmts = listNew();
    cc->localenv = dictNewWithParent(cc->localenv);
    tok = NULL;

    tok = cctrlTokenPeek(cc);

    while (tok && !tokenPunctIs(tok, '}')) {
        if (cctrlIsKeyword(cc,tok->start,tok->len)) {
            base_type = parseBaseDeclSpec(cc);
            while (1) {
                next_type = parsePointerType(cc,base_type);
                peek = cctrlTokenPeek(cc);
                
                if (!tokenPunctIs(peek,'(')) {
                    /* A normal variable */
                    varname = cctrlTokenGet(cc);
                    if (!varname) {
                        break;
                    }
                    type = parseArrayDimensions(cc,next_type);
                    var = astLVar(type,varname->start,varname->len);
                    if (!dictSet(cc->localenv,var->lname->data,var)) {
                        loggerPanic("line: %ld variable %s already declared\n",
                                cc->lineno,astLValueToString(var));
                    }
                } else {
                    cctrlTokenGet(cc);
                    var = parseFunctionPointer(cc,next_type);
                    dictSet(cc->localenv,var->fname->data,var);
                }

                if (cc->tmp_locals) {
                    listAppend(cc->tmp_locals, var);
                }

                stmt = parseVariableInitialiser(cc,var,PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);
                if (next_type->kind == AST_TYPE_AUTO) {
                    parseAssignAuto(cc,stmt);
                }

                if (stmt) {
                    listAppend(stmts,stmt);
                }
                cctrlTokenRewind(cc);
                tok = cctrlTokenGet(cc);

                if (tokenPunctIs(tok, ',')) {
                    continue;
                } else if (tokenPunctIs(tok,';')) {
                    break;
                } else {
                    loggerPanic("line %d: Unexpected token: %s\n",
                            tok->line, lexemeToString(tok));
                }
            }
        } else { 
            if ((stmt = parseStatement(cc)) != NULL) {
                if (stmt->kind == AST_DECL && stmt->declvar->kind == AST_GVAR) {
                    tok = cctrlTokenPeek(cc);
                    continue;
                }
                listAppend(stmts,stmt);
            } else {
                break;
            }
        }
        tok = cctrlTokenPeek(cc);
    }
    cc->localenv = cc->localenv->parent;
    cctrlTokenExpect(cc,'}');
    cctrlTokenPeek(cc);
    return astCompountStatement(stmts);
}

Ast *parseFunctionDef(Cctrl *cc, AstType *rettype,
        char *fname, int len, List *params, int has_var_args) 
{
    List *locals = listNew();
    Ast *func = NULL;
    AstType *fn_type;

    cc->localenv = dictNewWithParent(cc->localenv);
    cc->tmp_locals = locals;

    /* Upgrade a prototype to an actual function */
    func = dictGetLen(cc->global_env,fname,len);
    if (!func) {
        cc->tmp_params = params;
        cc->tmp_rettype = rettype;
        fn_type = astMakeFunctionType(cc->tmp_rettype, astParamTypes(params));
        func = astFunction(fn_type,fname,len,params,NULL,locals,
                has_var_args);
        dictSet(cc->global_env,func->fname->data,func);
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
                astReleaseList(func->params);
                func->params = params;
                cc->tmp_params = func->params;
                cc->tmp_rettype = func->type->rettype;
                fn_type = func->type;
                func->kind = AST_FUNC;
                break;

            default:
                loggerPanic("line %ld: unexpected function: %.*s -> %s\n",
                        cc->lineno,
                        len,fname, astToString(func));
                break;
        }
    }

    /* XXX: This allows us to do recursion by parsing the body after */
    cc->tmp_fname = func->fname;
    Ast *body = parseCompoundStatement(cc);
    func->body = body;
    fn_type->rettype = cc->tmp_rettype;

    cc->localenv = NULL;
    cc->tmp_locals = NULL;
    cc->tmp_rettype = NULL;
    cc->tmp_params = NULL;
    cc->tmp_fname = NULL;
    return func;
}

Ast *parseExternFunctionProto(Cctrl *cc, AstType *rettype, char *fname, int len) {
    Ast *func;
    int has_var_args = 0;
    List *params;
    lexeme *tok;
    cc->localenv = dictNewWithParent(cc->localenv);
    cc->tmp_locals = NULL;

    params = parseParams(cc,')', &has_var_args,1);
    tok = cctrlTokenGet(cc);
    if (!tokenPunctIs(tok, ';')) {
        loggerPanic("line %d: extern %.*s() cannot have a function body "
                "this will be defined elsewhere\n",
                tok->line,len,fname);
    }
    AstType *type = astMakeFunctionType(rettype, astParamTypes(params));
    func = astFunction(type,fname,len,params,NULL,NULL,0);
    func->kind = AST_EXTERN_FUNC;
    dictSet(cc->global_env,func->fname->data,func);
    return func;
}

Ast *parseFunctionOrDef(Cctrl *cc, AstType *rettype, char *fname, int len) {
    int has_var_args = 0;
    cctrlTokenExpect(cc,'(');
    cc->localenv = dictNewWithParent(cc->localenv);
    cc->tmp_locals = listNew();

    List *params = parseParams(cc,')',&has_var_args,1);
    lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok, '{')) {
        return parseFunctionDef(cc,rettype,fname,len,params,has_var_args);
    } else {
        if (rettype->kind == AST_TYPE_AUTO) {
            loggerPanic("line %ld: auto cannot be used with a function prototype %.*s() at this time\n",
                    cc->lineno,len,fname);
        }
        AstType *type = astMakeFunctionType(rettype, astParamTypes(params));
        Ast *fn = astFunction(type,fname,len,params,NULL,NULL,has_var_args);
        fn->kind = AST_FUN_PROTO;
        dictSet(cc->global_env,fn->fname->data,fn);
        return fn;
    }
}

Ast *parseAsmFunctionBinding(Cctrl *cc) {
    lexeme *tok;
    aoStr *asm_fname, *c_fname;
    AstType *rettype;
    List *params;
    Ast *asm_func;
    int has_var_args = 0;

    tok = cctrlTokenGet(cc);

    if (tok->tk_type != TK_IDENT && tok->start[0] != '_') {
        loggerPanic("line %d: ASM function binds must begin with '_' got: %.*s\n",
                tok->line,tok->len,tok->start);
    }

    asm_fname = aoStrDupRaw(tok->start,tok->len);
    rettype = parseDeclSpec(cc);

    if (rettype->kind == AST_TYPE_AUTO) {
        loggerPanic("line %ld: auto cannot be used with an assembly binding for function %s(), type cannot be automatically deduced\n",
                cc->lineno,
                asm_fname->data);
    }

    tok = cctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        loggerPanic("line %d: ASM function requires c function name\n",
                tok->line);
    }
    c_fname = aoStrDupRaw(tok->start,tok->len);
    cc->localenv = dictNewWithParent(cc->localenv);
    cctrlTokenExpect(cc,'(');

    params = parseParams(cc,')',&has_var_args,0);

    asm_func = astAsmFunctionBind(
            astMakeFunctionType(rettype, astParamTypes(params)),
            asm_fname,c_fname,params);

    cc->localenv = NULL;
    /* Map a c function to an ASM function */
    dictSet(cc->asm_funcs,c_fname->data,asm_func);
    cctrlTokenExpect(cc,';');
    return asm_func;
}

Ast *parseToplevelDef(Cctrl *cc, int *is_global) {
    Ast *variable, *asm_block, *asm_func, *extern_func, *ast;
    AstType *type;
    lexeme *tok, *name, *peek;
    int is_static = 0;

    while (1) {
        if ((tok = cctrlTokenGet(cc)) == NULL) {
            return NULL;
        }

        if (tok->tk_type == TK_KEYWORD) {
            switch (tok->i64) {
                case KW_ASM_EXTERN: {
                    if ((asm_func = parseAsmFunctionBinding(cc)) != NULL) {
                        return asm_func;
                    }
                    loggerPanic("line %d: Floating \"_extern\" keyword, expected \"_extern '_ASM_FUNC'\"\n", 
                            tok->line);
                }

                case KW_ASM: {
                    peek = cctrlTokenPeek(cc);
                    if (tokenPunctIs(peek,'{')) {
                        asm_block = prsAsm(cc);
                        listAppend(cc->asm_blocks,asm_block);
                        return asm_block;
                    }
                    loggerPanic("line %d: Floating \"asm\" keyword, expected \"asm {\"\n", 
                            tok->line);
                }

                case KW_EXTERN: {
                    tok = cctrlTokenGet(cc);
                    if (tok->tk_type == TK_STR && !strncmp(tok->start,"c",1)) {
                        type = parseDeclSpec(cc);
                        name = cctrlTokenGet(cc);
                        cctrlTokenExpect(cc,'(');
                        extern_func = parseExternFunctionProto(cc,type,
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
                    type = parseDeclSpec(cc);
                    if (type == NULL) {
                        loggerPanic("line %ld: Expected type declaration\n",cc->lineno);
                    }
                    /* static at the global scope does not yet do anything */
                    is_static = 1;
                    (void)is_static;
                    break;
                
                case KW_CLASS:
                    parseClassDef(cc,0);
                    cctrlTokenExpect(cc,';');
                    continue;

                case KW_UNION:
                    parseUnionDef(cc);
                    cctrlTokenExpect(cc,';');
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
                    cctrlTokenRewind(cc);
                    type = parseDeclSpec(cc);
                    break;

                case KW_IF:
                    cc->tmp_locals = listNew();
                    *is_global = 1;
                    return parseIfStatement(cc);

                case KW_WHILE:
                    cc->tmp_locals = listNew();
                    *is_global = 1;
                    return parseWhileStatement(cc);

                case KW_DO:
                    cc->tmp_locals = listNew();
                    *is_global = 1;
                    return parseDoWhileStatement(cc);

                case KW_FOR:
                    cc->tmp_locals = listNew();
                    *is_global = 1;
                    return parseForStatement(cc);

                default:
                    loggerPanic("line %d: Unexpected floating keyword: %.*s\n",
                            tok->line,tok->len,tok->start);
            }
        } else if (tok->tk_type == TK_IDENT) {
            /* top level function call */
            peek = cctrlTokenPeek(cc);
            if (tokenPunctIs(peek,'(')) {
                cctrlTokenGet(cc);
                ast = parseFunctionArguments(cc,tok->start,tok->len,')');
                cctrlTokenExpect(cc,';');
                *is_global = 1;
                return ast;
            } else if ((variable = dictGetLen(cc->global_env,tok->start, tok->len))) {
                cctrlTokenRewind(cc);
                ast = parseExpr(cc,16);
                cctrlTokenExpect(cc,';');
                *is_global = 1;
                return ast;
            } else {
                cctrlTokenRewind(cc);
                type = parseDeclSpec(cc);
                if (type == NULL) {
                    loggerPanic("line %d: Undefined type: %.*s\n",
                            tok->line,tok->len,tok->start);
                }
            }
        } else if (tok->tk_type == TK_CHAR_CONST) {
            ast = parseFloatingCharConst(cc,tok);
            *is_global = 1;
            return ast;
        } else if (tok->tk_type == TK_STR) {
            cctrlTokenRewind(cc);
            ast = parseFunctionArguments(cc,"printf",6,';');
            *is_global = 1;
            return ast;
        }

        name = cctrlTokenGet(cc);

        if (name->tk_type == TK_KEYWORD) {
            switch (name->i64) {
                case KW_CLASS:
                    if (!astIsIntType(type)) {
                        loggerPanic("line %ld: Can only make intrinsic types from integer types, got %s\n",
                                cc->lineno,astTypeToString(type));
                    }
                    parseClassDef(cc,1);
                    cctrlTokenExpect(cc,';');
                    continue;
                default:
                    loggerPanic("line %ld: %s can only prefix a class\n",
                            cc->lineno,lexemeToString(name));
            }
        }

        if (name->tk_type != TK_IDENT) {
            loggerPanic("line %d: Identifier expected: got %s\n",
                    name->line, lexemeToString(name));
        }

        type = parseArrayDimensions(cc,type);
        tok = cctrlTokenPeek(cc);

        if (tokenPunctIs(tok,'=') && type->kind != AST_TYPE_ARRAY) {
            variable = astGVar(type,name->start,name->len,0);
            if (type->kind == AST_TYPE_AUTO) {
                loggerPanic("line %ld: auto cannot be used without an initialiser\n",
                        cc->lineno);
            }
            ast = astDecl(variable,NULL);
            listAppend(cc->ast_list,ast);
            dictSet(cc->global_env,variable->gname->data,variable);
            cctrlTokenRewind(cc);
            *is_global = 1;
            
            ast = parseExpr(cc,16); // parseVariableInitialiser(cc,variable,PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);
            cctrlTokenExpect(cc,';');
            return ast;
        } else if (type->kind == AST_TYPE_ARRAY) {
            variable = astGVar(type,name->start,name->len,0);
            dictSet(cc->global_env,variable->gname->data,variable);
            ast = parseVariableInitialiser(cc,variable,PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);
            if (type->kind == AST_TYPE_AUTO) {
                parseAssignAuto(cc,ast);
            }
            return ast;
        }

        if (tokenPunctIs(tok, '(')) {
            return parseFunctionOrDef(cc,type,name->start,name->len); 
        }

        if (tokenPunctIs(tok,';') || tokenPunctIs(tok, ',')) {
            cctrlTokenGet(cc);
            variable = astGVar(type,name->start,name->len,0);
            dictSet(cc->global_env,variable->gname->data,variable);
            return astDecl(variable,NULL);
        }
        loggerPanic("line %d: Cannot handle '%s'\n",tok->line,
                lexemeToString(tok));
    }
}

void parseToAst(Cctrl *cc) {
    Ast *ast;
    lexeme *tok;
    int is_global = 0;
    while ((ast = parseToplevelDef(cc,&is_global)) != NULL) {
        if (is_global) {
            listAppend(cc->initalisers,ast);
            if (!listEmpty(cc->tmp_locals)) {
                listMergeAppend(cc->initaliser_locals,cc->tmp_locals);
            }
        } else if (ast->kind != AST_FUN_PROTO) {
            listAppend(cc->ast_list,ast);
        }
        is_global = 0;
        tok = cctrlTokenPeek(cc);
        if (!tok) {
            break;
        }
        cc->tmp_locals = NULL;
        cc->localenv = NULL;
    }
}
