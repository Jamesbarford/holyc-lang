#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
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
Ast *parseVariableInitialiser(Cctrl *cc, Ast *var, s64 terminator_flags);
Ast *parseDecl(Cctrl *cc);
Ast *parseDeclOrStatement(Cctrl *cc);
Ast *parseCompoundStatement(Cctrl *cc);
AstType *parseClassDef(Cctrl *cc, int is_intrinsic);
AstType *parseUnionDef(Cctrl *cc);

static AoStr *getRangeLoopIdx(void) {
    static int id = 0;
    return aoStrPrintf("___tmp%d___", ++id);
}

/* Kinda cheating converting it to a string and calling printf */
Ast *parseFloatingCharConst(Cctrl *cc, Lexeme *tok) {
    u64 ch = (unsigned long)tok->i64;
    char str[16];
    Vec *argv = astVecNew();
    int len = 0;
    int real_len = 0;

    while (ch) {
        switch (ch & 0xFF) {
            case '\'': str[len++] = '\\'; str[len++] = '\''; break;
            case '\\': str[len++] = '\\'; str[len++] = '\\'; break;
            case '\"': str[len++] = '\''; str[len++] = '\"'; break;
            case '\b': str[len++] = '\\';  str[len++] = 'b'; break;
            case '\n': str[len++] = '\\';  str[len++] = 'n'; break;
            case '\t': str[len++] = '\\';  str[len++] = 't'; break;
            case '\v': str[len++] = '\\';  str[len++] = 'v'; break;
            case '\f': str[len++] = '\\';  str[len++] = 'f'; break;
            case '\r': str[len++] = '\\';  str[len++] = 'r'; break;

            default: str[len++] = ch & 0xFF; break;
        }
        real_len++;
        ch = ch >> 8;
    }

    Ast *ast = cctrlGetOrSetString(cc,str,len,real_len);
    vecPush(argv, ast);
    cctrlTokenExpect(cc,';');
    return astFunctionCall(ast_void_type,"printf",6,argv);
}

void parseTypeCheckClassFieldInitaliser(Cctrl *cc, AstType *cls_field_type, Ast *init) {
    if (!astTypeCheck(cls_field_type, init, AST_BIN_OP_ASSIGN)) {
        char *cls_field_str = astTypeToColorString(cls_field_type);
        char *init_field = astTypeToColorString(init->type);
        char *var_string = astLValueToString(init,0);
        cctrlWarning(cc,"Incompatible value being assigned to class field expected '%s' got '%s %s'",
                cls_field_str,init_field,var_string);
    }
}

Ast *parseDeclArrayInitInt(Cctrl *cc, AstType *type) {
    Lexeme *tok = cctrlTokenGet(cc);
    List *initlist;
    Ast *init;

    if (type->ptr && type->ptr->kind == AST_TYPE_CHAR && tok->tk_type == TK_STR) {
        return cctrlGetOrSetString(cc,tok->start,tok->len,tok->i64);
    }

    if (!tokenPunctIs(tok, '{')) {
        cctrlRaiseException(cc,"Expected intializer list starting with '{' got: '%c'",
                (char)tok->i64);
    }

    initlist = listNew();
    u64 i = 0;
    Map *cls_fields = NULL;
    if (type->kind == AST_TYPE_CLASS) {
        cls_fields = type->fields;
    }

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
            if (init == NULL) {
                cctrlRaiseExceptionFromTo(cc,NULL,'{','}',"Array initaliser encountered an unexpected token");
            } else if (type->ptr) {
              if ((astGetResultType(AST_BIN_OP_ASSIGN, init->type, type->ptr)) == NULL) {
                  cctrlRaiseException(cc,"Incompatiable types: %s %s",
                          astTypeToString(init->type),
                          astTypeToString(type->ptr));
              }
            } else if (type->kind == AST_TYPE_CLASS) {
                if (i >= cls_fields->indexes->size) {
                    cctrlRaiseException(cc, 
                            "More initialisers than class fields for class: %s",
                            astTypeToString(type));
                }

                u64 cls_field_idx = (unsigned long)cls_fields->indexes->entries[i];
                MapNode *entry = &cls_fields->entries[cls_field_idx];
                AstType *cls_field_type = entry->value;
                parseTypeCheckClassFieldInitaliser(cc,cls_field_type,init);
                i++;
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

void parseFlattenAnnonymous(AstType *anon,
                            Map *fields_dict,
                            int offset,
                            int make_copy)
{
    MapIter it;
    mapIterInit(anon->fields, &it);
    while (mapIterNext(&it)) {
        MapNode *n = it.node;
        AstType *base = (AstType *)n->value;
        AstType *type = NULL;
        if (make_copy) {
            type = astTypeCopy(base);
        } else {
            type = base;
        }
        type->offset += offset;

        mapAdd(fields_dict,n->key,type);
    }
}

typedef struct ClsField {
    AstType *type;
    AoStr *field_name;
} ClsField;

ClsField *clsFieldNew(AstType *type, AoStr *field_name) {
    ClsField *f = (ClsField *)malloc(sizeof(ClsField));
    f->type = type;
    f->field_name = field_name;
    return f;
}

List *parseClassOrUnionFields(Cctrl *cc, AoStr *name,
        u32 (*computeSize)(List *), u32 *_size)
{
    u32 size;
    char *fnptr_name;
    int fnptr_name_len;
    Lexeme *tok, *tok_name;
    AstType *next_type = NULL, *base_type = NULL, *field_type = NULL;
    AoStr  *field_name = NULL;
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
                base_type = astClassType(NULL, aoStrDup(name),8,0);
            }
        } else if (tok_name->tk_type == TK_KEYWORD) { 
            switch (tok_name->i64) {
                case KW_UNION: {
                    AstType *_union = parseUnionDef(cc);
                    _union->kind = AST_TYPE_UNION;
                    listAppend(fields_list, clsFieldNew(_union,_union->clsname));
                    cctrlTokenExpect(cc,';');
                    continue;
                }
                case KW_CLASS: {
                    AstType *cls = parseClassDef(cc,0);
                    listAppend(fields_list, clsFieldNew(cls,cls->clsname));
                    cctrlTokenExpect(cc,';');
                    continue;
                }
                default:
                    cctrlRaiseException(cc,"Unexpected keyword: `%.*s` while parsing class %s",
                            tok_name->len,
                            tok_name->start,
                            name->data);
            }
        } else {
            if (name) {
                cctrlRaiseException(cc,"Unexpected type `%.*s` while parsing class %s",
                        tok_name->len, tok_name->start, name->data);
            } else {
                cctrlRewindUntilStrMatch(cc,tok_name->start,tok_name->len,NULL);
                cctrlRaiseException(cc,"Unexpected type `%.*s` while parsing annoymous class",
                        tok_name->len, tok_name->start);
            }
        }

        while (1) {
            next_type = parsePointerType(cc,base_type);
            tok_name = cctrlTokenGet(cc);
            if (!tok_name) {
                cctrlRaiseException(cc, "<type> <variable_name | (> expected got NULL while parsing class %s",
                        name->data);
            }
            if (tok_name->tk_type != TK_IDENT && !tokenPunctIs(tok_name, '(')) {
                cctrlRewindUntilPunctMatch(cc,tok_name->i64,NULL);
                cctrlRaiseException(cc, "Unexpected character `%.*s` while parsing class %s",
                        tok_name->len,tok_name->start,name->data);
            } else if (tokenPunctIs(tok_name, '(')) {
                next_type = parseFunctionPointerType(cc,&fnptr_name,&fnptr_name_len,next_type);
                field_name = aoStrDupRaw(fnptr_name,fnptr_name_len);
            } else  {
                /* XXX: this does not work properly for classes which are not 
                 * pointers as we miss one of the strings */
                next_type = parseArrayDimensions(cc,next_type);
                field_name = aoStrDupRaw(tok_name->start,tok_name->len);
            }

            field_type = astMakeClassField(next_type, 0);
            if (next_type && next_type->clsname) {
                field_type->clsname = next_type->clsname;
            }
            /* The list is here to ease calculating the offset of the class 
             * fields as they have been defined by the programmer... Hashing 
             * loses the ordering.*/
            listAppend(fields_list,clsFieldNew(field_type,field_name));

            tok = cctrlTokenGet(cc);
            if (tokenPunctIs(tok, ',')) {
                continue;
            } else if (tokenPunctIs(tok,';')) {
                break;
            } else {
                cctrlRewindUntilPunctMatch(cc,tok->i64,NULL);
                cctrlRaiseException(cc, "Unexpected token '%.*s' while parsing class %s",
                        tok->len, tok->start, name->data);
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

u32 CalcUnionSize(List *fields) {
    int max = 0;
    AstType *type;
    listForEach(fields) {
        ClsField *cls_field = (ClsField *)it->value;
        type = cls_field->type;
        if (max < type->size) {
            max = type->size;
        }
    }
    return max;
}

u32 CalcClassSize(List *fields) {
    u32 offset = 0;
    u32 size = 0;
    listForEach(fields) {
        ClsField *cls_field = (ClsField *)it->value;
        AstType *type = cls_field->type;
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

Map *parseClassOffsets(Cctrl *cc,
                       int *aligned_size, 
                       List *fields, 
                       AstType *base_class,
                       AoStr *clsname,
                       int is_intrinsic)
{
    int offset,size,padding;
    AstType *field;
    Map *fields_dict = astTypeMapNew();

    /* XXX: Assumes the class definition will be made later */
    if (listEmpty(fields)) {
        return fields_dict;
    }
    
    if (base_class) {
        offset = base_class->size;
        if (cc->flags & CCTRL_SAVE_ANONYMOUS) {
            mapAdd(fields_dict,astAnnonymousLabel(),base_class);
        }
        parseFlattenAnnonymous(base_class,fields_dict,0,1);
    } else {
        offset = 0;
    }

    if (is_intrinsic) {
        *aligned_size = 16;
        listForEach(fields) {
            ClsField *cls_field = (ClsField *)it->value;
            field = cls_field->type;
            field->offset = offset;
            offset+=field->size;
            if (cls_field->field_name) {
                mapAdd(fields_dict,cls_field->field_name->data,field);
            }
        }

        return fields_dict;
    }

    listForEach(fields) {
        ClsField *cls_field = (ClsField *)it->value;
        field = cls_field->type;
        AoStr *field_name = cls_field->field_name;

        if (field_name == NULL && parseIsClassOrUnion(field->kind)) {
            if (cc->flags & CCTRL_SAVE_ANONYMOUS) {
                mapAdd(fields_dict,astAnnonymousLabel(),field);
            }
            parseFlattenAnnonymous(field,fields_dict,offset,0);
            offset += field->size;
            free(cls_field);
            continue;
        } else {
            if (field->kind == AST_TYPE_POINTER && 
                    (field->ptr->kind == AST_TYPE_CLASS || field->ptr->kind == AST_TYPE_UNION)) {
                if (clsname && field->ptr->clsname) {
                    if (aoStrCmp(field->ptr->clsname, clsname)) {
                        field->fields = fields_dict;
                    }
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

        if (field_name) {
            mapAdd(fields_dict,field_name->data,field);
        }
        free(cls_field);
    }

    *aligned_size = offset + CalcPadding(offset, 8);
    return fields_dict;
}

Map *parseUnionOffsets(Cctrl *cc, int *real_size, List *fields) {
    int max_size;
    AstType *field;
    Map *fields_dict = astTypeMapNew();

    max_size = 0;
    listForEach(fields) {
        ClsField *cls_field = (ClsField *)it->value;
        field = cls_field->type;
        AoStr *field_name = cls_field->field_name;
        if (max_size < field->size) {
            max_size = field->size;
        }
        if (field->clsname == NULL && parseIsClassOrUnion(field->kind)) {
            if (cc->flags & CCTRL_SAVE_ANONYMOUS) {
                mapAdd(fields_dict,astAnnonymousLabel(),field);
            }
            parseFlattenAnnonymous(field,fields_dict,0,0);
            continue;
        }

        field->offset = 0;
        if (field_name) {
            mapAdd(fields_dict,field_name->data,field);
        }
    }
    *real_size = max_size;
    return fields_dict;
}

AstType *parseClassOrUnion(Cctrl *cc, Map *env,
        int is_class,
        u32 (*computeSize)(List *),
        int is_intrinsic)
{
    AoStr *tag = NULL;
    int aligned_size = 0;
    u32 class_size;
    Lexeme *tok = cctrlTokenGet(cc);
    AstType *prev = NULL, *ref = NULL, *base_class = NULL;
    List *fields = NULL;
    Map *fields_dict;
    cc->localenv = cctrlCreateAstMap(cc->localenv);

    if (tok->tk_type == TK_IDENT) {
        tag = aoStrDupRaw(tok->start,tok->len);

        tok = cctrlTokenGet(cc);
        if (tokenPunctIs(tok, ':')) { // Class inheritance
            if (!is_class) {
                cctrlRaiseException(cc,"Cannot use inheritance with a union");
            }
            tok = cctrlTokenGet(cc);
            if (tok->tk_type != TK_IDENT) {
                cctrlRaiseException(cc, "Expected Identifier for class inheritance");
            }
            base_class = mapGetLen(cc->clsdefs,tok->start,tok->len);
            if (base_class == NULL) {
                cctrlRaiseException(cc,"class %.*s has not been defined\n",
                        tok->len,tok->start);
            }

            if (tokenPunctIs(cctrlTokenPeek(cc),',')) {
                cctrlRaiseException(cc,"Only one base class allowed at this time");
            }
        } else {
            cctrlTokenRewind(cc);
        }
    }

    if (tag) {
        prev = mapGetLen(env, tag->data, tag->len);
    }

    fields = parseClassOrUnionFields(cc,tag,computeSize,&class_size);

    if (prev && !fields) {
        return prev;
    }

    if (is_class) {
        fields_dict = parseClassOffsets(cc,&aligned_size,fields,base_class,tag,is_intrinsic);
    } else {
        fields_dict = parseUnionOffsets(cc,&aligned_size,fields);
    }
    listRelease(fields,NULL);

    if (prev && fields_dict) {
        prev->fields = fields_dict;
        prev->size = aligned_size;
        return prev;
    }

    if (fields_dict) {
        ref = astClassType(fields_dict,tag,aligned_size,is_intrinsic); 
        if (base_class) {
            ref->size += base_class->size;
        }
    } else {
        ref = astClassType(NULL,tag,aligned_size,is_intrinsic); 
    }
    if (!is_class) ref->kind = AST_TYPE_UNION;
    if (tag) {
        mapAdd(env,tag->data,ref);
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

Ast *parseVariableAssignment(Cctrl *cc, Ast *var, s64 terminator_flags) {
    Ast *init;
    int len;
    Lexeme *peek = cctrlTokenPeek(cc);
    assert(var);

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
            cctrlRaiseExceptionFromTo(cc, NULL, '{', '}',
                                     "Invalid array initializer: expected %d items but got %d",
                                      var->type->len, len);
        }
        Lexeme *tok = cctrlTokenGet(cc);
        assertTokenIsTerminator(cc,tok,terminator_flags);
        return astDecl(var,init);
    } else if (var->type->kind == AST_TYPE_CLASS && 
               !var->type->is_intrinsic && tokenPunctIs(peek,'{')) {
        init = parseDeclArrayInitInt(cc,var->type);
        Lexeme *tok = cctrlTokenGet(cc);
        assertTokenIsTerminator(cc,tok,terminator_flags);
        return astDecl(var,init);
    }

    init = parseExpr(cc,16);
    Lexeme *tok = cctrlTokenGet(cc);
    assertTokenIsTerminator(cc,tok,terminator_flags);
    if (var->kind == AST_GVAR && var->type->kind == AST_TYPE_INT) {
        init = astI64Type(evalIntConstExpr(init));
    }

    if (var->type->kind == AST_TYPE_AUTO) {
        var->type = init->type;
    }

    /* Check what we are trying to assign is valid */
    AstType *ok = astTypeCheck(var->type,init,AST_BIN_OP_ASSIGN);
    if (!ok) {
        typeCheckWarn(cc,'=',var,init);
    }

    /* This is for when we have parsed a call to an inline function that is 
     * being assigned to a variable */
    if (init->kind == AST_COMPOUND_STMT) {
        /* Attach the Ast to the current function that is being called */
        if (cc->tmp_func) {
            listAppend(cc->tmp_func->body->stms,init);
        }
        return astDecl(var,init->inline_ret);
    }

    return astDecl(var,init);
}

Ast *parseVariableInitialiser(Cctrl *cc, Ast *var, s64 terminator_flags) {
    Lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok,'=')) {
        return parseVariableAssignment(cc,var,terminator_flags);
    }
    if (var->type->len == -1) {
        cctrlRaiseException(cc, "Missing array initializer: %s",astToString(var));
    }
    cctrlTokenRewind(cc);
    tok = cctrlTokenGet(cc);

    assertTokenIsTerminator(cc,tok,terminator_flags);
    return astDecl(var,NULL);
}

Ast *parseDecl(Cctrl *cc) {
    AstType *type;
    Lexeme *varname;
    Ast *var, *ast;

    parseDeclInternal(cc,&varname,&type);
    if (varname == NULL) {
        cctrlTokenExpect(cc,';');
        return NULL;
    }
    var = astLVar(type,varname->start,varname->len);
    if (!mapAddOrErr(cc->localenv,var->lname->data,var)) {
        cctrlRaiseException(cc,"variable %s already declared",astLValueToString(var,0));
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

int parseValidPostControlFlowToken(Lexeme *tok) {
    if (tok->tk_type == TK_IDENT) return 1;
    if (tok->tk_type == TK_STR) return 1;
    if (tok->tk_type == TK_PUNCT) {
        switch (tok->i64) {
            case TK_MINUS_MINUS:
            case TK_PLUS_PLUS:
            case '{':
            case ';':
            case '*':
                return 1;
        }
    }
    if (tok->tk_type == TK_KEYWORD) {
        switch (tok->i64) {
            case KW_DO:
            case KW_FOR:
            case KW_GOTO:
            case KW_IF:
            case KW_RETURN:
            case KW_SWITCH:
            case KW_WHILE:
            case KW_BREAK:
            case KW_CONTINUE:
                return 1;
        }
    }
    return 0;
}

Ast *parseIfStatement(Cctrl *cc) {
    cctrlTokenExpect(cc,'(');
    Ast *cond = parseExpr(cc,16);
    cctrlTokenExpect(cc,')');

    Lexeme *peek = cctrlTokenPeek(cc);
    if (!parseValidPostControlFlowToken(peek)) {
        cctrlRaiseException(cc,"Unexpected %s `%.*s` while parsing if body", 
                lexemeTypeToString(peek->tk_type), peek->len, peek->start);
    }

    Ast *then = parseStatement(cc);
    Lexeme *tok = cctrlTokenGet(cc);

    if (tok && tok->tk_type == TK_KEYWORD && tok->i64 == KW_ELSE) {
        Lexeme *peek = cctrlTokenPeek(cc);
        if (!parseValidPostControlFlowToken(peek)) {
            cctrlTokenRewind(cc);
            cctrlTokenRewind(cc);
            cctrlRaiseException(cc,"Unexpected %s `%.*s` while parsing else body", 
                    lexemeTypeToString(peek->tk_type), peek->len, peek->start);
        }
        Ast *els = parseStatement(cc);
        return astIf(cond,then,els);
    }
    cctrlTokenRewind(cc);
    return astIf(cond,then,NULL);
}

Ast *parseOptDeclOrStmt(Cctrl *cc) {
    Lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok,';')) {
        return NULL;
    }
    cctrlTokenRewind(cc);
    return parseDeclOrStatement(cc);
}

Ast *parseOptExpr(Cctrl *cc) {
    Lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok,';')) {
        return NULL;
    }
    cctrlTokenRewind(cc);
    Ast *ast = parseExpr(cc,16);
    tok = cctrlTokenGet(cc);
    assertTokenIsTerminatorWithMsg(cc,tok,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA, "for loop requires either a conditional statement or a semi colon to terminate the initalisation");
    return ast;
}

Ast *parseDesugarArrayLoop(Cctrl *cc, Ast *iteratee, Ast *static_array) {
    /* Create a temporay variable as the counter */
    AoStr *range_tmp_var = getRangeLoopIdx();
    Ast *counter_var = astLVar(ast_int_type,range_tmp_var->data,
                               range_tmp_var->len);

    Ast *counter = astDecl(counter_var,astI64Type(0));

    /* How much memory it takes up / size of one element */
    Ast *array_len = astI64Type(static_array->type->size/static_array->type->ptr->size);

    if (iteratee->type->kind == AST_TYPE_AUTO) {
        AstType *deref_type = static_array->type->ptr;
        iteratee->type = deref_type;
    }

    mapAddLen(cc->localenv,range_tmp_var->data,
                 range_tmp_var->len,counter_var);
    mapAdd(cc->localenv,iteratee->lname->data,iteratee);

    if (cc->tmp_locals) {
        listAppend(cc->tmp_locals,counter_var);
        listAppend(cc->tmp_locals,iteratee);
    }

    Ast *cond = parseCreateBinaryOp(cc,AST_BIN_OP_LT, counter_var, array_len);


    Ast *iterator = astDecl(iteratee,
            astUnaryOperator(static_array->type->ptr,
                AST_UN_OP_DEREF,
                parseCreateBinaryOp(cc,AST_BIN_OP_ADD, static_array, counter_var))
            );
    Ast *step = astUnaryOperator(ast_int_type,AST_UN_OP_PRE_INC,counter_var);

    cctrlTokenExpect(cc,')');
    Ast *forbody = parseStatement(cc);
    listPrepend(forbody->stms,iterator);
    return astFor(counter,cond,step,forbody,NULL,NULL,NULL);
}

void parseAssertContainerHasFields(Cctrl *cc, AstType *size_field, 
                                   AstType *entries_field)
{
    if (!size_field) {
        cctrlRaiseException(cc,"Range for loop must be on a struct with both a 'size' and 'entries' property");
    }

    if (!astIsIntType(size_field)) {
        cctrlRaiseException(cc,"Range for loop struct's size field must be an int got: %s",
                astTypeToColorString(size_field));
    }

    if (!entries_field) {
        cctrlRaiseException(cc,"Range for loop must be on a struct with both a 'size' and 'entries' property");
    }

    if (entries_field->kind != AST_TYPE_POINTER && entries_field->kind != AST_TYPE_ARRAY) {
        cctrlRaiseException(cc,"'entries' field must be a pointer or array got '%s'",
                astTypeKindToString(entries_field->kind));
    }

    if (entries_field->ptr->kind == AST_TYPE_VOID) {
        cctrlRaiseException(cc,"cannot dereference void pointer");
    }
}

Ast *parseCreateForRange(Cctrl *cc, Ast *iteratee,
                         Ast *size_ref, Ast *entries_ref)
{
    AoStr *range_tmp_var = getRangeLoopIdx();
    Ast *counter_var = astLVar(ast_int_type,range_tmp_var->data,
                               range_tmp_var->len);
    Ast *counter = astDecl(counter_var,astI64Type(0));

    mapAddLen(cc->localenv,range_tmp_var->data,
                 range_tmp_var->len,counter_var);
    mapAdd(cc->localenv,iteratee->lname->data,iteratee);
    if (cc->tmp_locals) {
        listAppend(cc->tmp_locals,counter_var);
        listAppend(cc->tmp_locals,iteratee);
    }

    Ast *cond = parseCreateBinaryOp(cc,AST_BIN_OP_LT, counter_var, size_ref);
    Ast *iterator = astDecl(iteratee,
            astUnaryOperator(entries_ref->type->ptr,
                AST_UN_OP_DEREF,
                parseCreateBinaryOp(cc,AST_BIN_OP_ADD, entries_ref, counter_var))
            );
    Ast *step = astUnaryOperator(ast_int_type,AST_UN_OP_PRE_INC,counter_var);
    cctrlTokenExpect(cc,')');
    Ast *forbody = parseStatement(cc);
    listPrepend(forbody->stms,iterator);
    return astFor(counter,cond,step,forbody,NULL,NULL,NULL);
}

Ast *parseRangeLoop(Cctrl *cc, Ast *iteratee) {
    cctrlTokenGet(cc);
    Ast *container = parseExpr(cc,16);

    if (container->kind == AST_LVAR) {
        if (container->type->kind == AST_TYPE_POINTER) {
            if (container->type->ptr->kind != AST_TYPE_CLASS) {
                cctrlRaiseException(cc,"pointer '%s' has no fields; range requires 'I64 size' and '<type> *entries'",
                        astLValueToString(container, 0));
            }

            AstType *size_field = mapGetLen(container->type->ptr->fields, str_lit("size"));
            AstType *entries_field = mapGetLen(container->type->ptr->fields, str_lit("entries"));

            parseAssertContainerHasFields(cc,size_field,entries_field);

            Ast *deref = astUnaryOperator(container->type->ptr,
                                          AST_UN_OP_DEREF,
                                          container);
            Ast *size_ref = astClassRef(size_field,deref,"size");
            Ast *entries_ref = astClassRef(entries_field,deref,"entries");

            if (iteratee->type->kind == AST_TYPE_AUTO) {
                AstType *deref_type = entries_field->ptr;
                iteratee->type = deref_type;
            }

            return parseCreateForRange(cc, iteratee, size_ref, entries_ref);
        } else if (container->type->kind == AST_TYPE_ARRAY) {
            return parseDesugarArrayLoop(cc,iteratee,container);
        } else {
            cctrlRaiseException(cc,"can only range over arrays and pointers");
        }
    } else if (container->kind == AST_CLASS_REF) {
        Map *fields = NULL;
        if (container->type->kind == AST_TYPE_POINTER) {
            fields = container->type->ptr->fields;
        } else if (container->type->kind == AST_TYPE_ARRAY) {
            return parseDesugarArrayLoop(cc,iteratee,container);
        } else {
            fields = container->type->fields;
        }

        AstType *size_field = mapGetLen(fields, str_lit("size"));
        AstType *entries_field = mapGetLen(fields, str_lit("entries"));

        parseAssertContainerHasFields(cc,size_field,entries_field);

        Ast *deref = astUnaryOperator(container->type->ptr,AST_UN_OP_DEREF,container);
        Ast *size_ref = astClassRef(size_field,deref,"size");
        Ast *entries_ref = astClassRef(entries_field,deref,"entries");

        if (iteratee->type->kind == AST_TYPE_AUTO) {
            AstType *deref_type = entries_ref->type;
            iteratee->type = deref_type;
        }

        return parseCreateForRange(cc, iteratee, size_ref, entries_ref);
    }
    cctrlRaiseException(cc,"Can only handle lvars, arrays and class references at this time got: %s %s",
                astKindToString(container->kind), astToString(container));
}

/* Either parse the initialiser or a range loop. Range loop is experimental */
Ast *parseForLoopInitialiser(Cctrl *cc) {
    Lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok,';')) {
        return NULL;
    }
    cctrlTokenRewind(cc);
    
    tok = cctrlTokenPeek(cc);
    if (!tok) {
        return NULL;
    }

    if (cctrlIsKeyword(cc,tok->start,tok->len)) {
        AstType *type = parseDeclSpec(cc);
        tok = cctrlTokenGet(cc);
        if (tok->tk_type != TK_IDENT) {
            cctrlRaiseException(cc,"expected Identifier got: %s",
                    lexemeToString(tok)); 
        }

        /* We have a for loop variable */
        Ast *init_var = astLVar(type,tok->start,tok->len);

        /* can be : for an auto loop or ';' for a normal loop */
        tok = cctrlTokenPeek(cc); 
        if (!mapAddOrErr(cc->localenv,init_var->lname->data,init_var)) {
            cctrlRaiseException(cc,"variable `%s` already declared!",
                    astLValueToString(init_var,0));
        }
        
        /* For this to work the type must have:
         * 1) a pointer called entries 
         * 2) a size value that must be an integer */
        if (tokenPunctIs(tok,':')) {
            return parseRangeLoop(cc,init_var);
        } else if (tokenPunctIs(tok, '=')) {
            if (cc->tmp_locals) {
                listAppend(cc->tmp_locals, init_var);
            }
            Ast *ast = parseVariableInitialiser(cc,init_var,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
            if (type->kind == AST_TYPE_AUTO) {
                parseAssignAuto(cc,ast);
            }
            return ast;
        }
    }
    return parseStatement(cc);
}

Ast *parseForStatement(Cctrl *cc) {
    Ast *forinit, *forcond, *forstep, *forbody;
    AoStr *for_begin, *for_end, *for_middle,
          *prev_begin, *prev_end;
    cctrlTokenExpect(cc,'(');

    prev_begin = cc->tmp_loop_begin;
    prev_end = cc->tmp_loop_end;

    for_begin = astMakeLabel();
    for_middle = astMakeLabel();
    for_end = astMakeLabel();

    cc->tmp_loop_begin = for_middle;
    cc->tmp_loop_end = for_end;

    cc->localenv = cctrlCreateAstMap(cc->localenv);
    forinit = parseForLoopInitialiser(cc);
    //parseOptDeclOrStmt(cc);

    if (forinit && forinit->kind == AST_FOR) {
        forinit->for_begin = for_begin;
        forinit->for_middle = for_middle;
        forinit->for_end = for_end;
        cc->localenv = cc->localenv->parent;
        cc->tmp_loop_begin = prev_begin;
        cc->tmp_loop_end = prev_end;
        return forinit;
    }

    forcond = parseOptExpr(cc);
    if (tokenPunctIs(cctrlTokenPeek(cc), ')')) {
        forstep = NULL;
    } else {
        forstep = parseExpr(cc,16);
    }
    cctrlTokenExpect(cc,')');

    Lexeme *peek = cctrlTokenPeek(cc);
    if (!parseValidPostControlFlowToken(peek)) {
        cctrlRaiseException(cc,"Unexpected %s `%.*s` while parsing for loop body", 
                lexemeTypeToString(peek->tk_type), peek->len, peek->start);
    }
    forbody = parseStatement(cc);
    /* Go back up */
    cc->localenv = cc->localenv->parent;
    cc->tmp_loop_begin = prev_begin;
    cc->tmp_loop_end = prev_end;
    return astFor(forinit,forcond,forstep,forbody,for_begin,for_middle,for_end);
}

Ast *parseWhileStatement(Cctrl *cc) {
    Ast *whilecond, *whilebody;
    AoStr *while_begin, *while_end,
          *prev_begin, *prev_end;
    cctrlTokenExpect(cc,'(');

    prev_begin = cc->tmp_loop_begin;
    prev_end = cc->tmp_loop_end;

    while_begin = astMakeLabel();
    while_end = astMakeLabel();
    cc->tmp_loop_begin = while_begin;
    cc->tmp_loop_end = while_end;

    cc->localenv = cctrlCreateAstMap(cc->localenv);
    whilecond = parseExpr(cc,16);
    cctrlTokenExpect(cc,')');

    Lexeme *peek = cctrlTokenPeek(cc);
    if (!parseValidPostControlFlowToken(peek)) {
        cctrlRaiseException(cc,"Unexpected %s `%.*s` while parsing while loop body", 
                lexemeTypeToString(peek->tk_type), peek->len, peek->start);
    }

    whilebody = parseStatement(cc);
    cc->localenv = cc->localenv->parent;
    cc->tmp_loop_begin = prev_begin;
    cc->tmp_loop_end = prev_end;
    return astWhile(whilecond,whilebody,while_begin, while_end);
}

Ast *parseDoWhileStatement(Cctrl *cc) {
    Ast *whilecond, *whilebody;
    Lexeme *tok;
    AoStr *while_begin, *while_end,
          *prev_begin, *prev_end;
    

    prev_begin = cc->tmp_loop_begin;
    prev_end = cc->tmp_loop_end;

    while_begin = astMakeLabel();
    while_end = astMakeLabel();
    cc->tmp_loop_begin = while_begin;
    cc->tmp_loop_end = while_end;

    cc->localenv = cctrlCreateAstMap(cc->localenv);

    Lexeme *peek = cctrlTokenPeek(cc);
    if (!parseValidPostControlFlowToken(peek)) {
        cctrlRewindUntilStrMatch(cc,peek->start,peek->len,NULL);
        cctrlRaiseException(cc,"Unexpected %s `%.*s` while parsing do while loop body", 
                lexemeTypeToString(peek->tk_type), peek->len, peek->start);
    }


    whilebody = parseStatement(cc);
    
    tok = cctrlTokenGet(cc);

    if (tok->tk_type != TK_KEYWORD && tok->i64 != KW_WHILE) {
        cctrlRaiseException(cc,"Expected keyword 'while'");
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
        cctrlRaiseException(cc,"Floating break, not inside a breakable statement");
    }
    Ast *ast = astBreak(cc->tmp_loop_end);
    cctrlTokenExpect(cc,';');
    return ast;
}

Ast *parseContinueStatement(Cctrl *cc) {
    if (cc->tmp_loop_begin == NULL) {
        cctrlRaiseException(cc,"Floating continue, not inside a loop");
    }
    Ast *ast = astContinue(cc->tmp_loop_begin);
    cctrlTokenExpect(cc,';');
    return ast;
}

Ast *parseReturnStatement(Cctrl *cc) {
    Ast *retval = parseExpr(cc,16);
    AstType *check;
    cctrlTokenExpect(cc,';');
    /* A best attempt at trying to get the return type of a function */
    if (cc->tmp_rettype->kind == AST_TYPE_AUTO) {
        cc->tmp_rettype = parseReturnAuto(cc,retval);
    }

    Ast *maybe_fn = findFunctionDecl(cc,cc->tmp_fname->data,cc->tmp_fname->len);

    if (retval) check = retval->type;
    else        check = ast_void_type;

    if (check->kind == AST_TYPE_VOID && maybe_fn->type->rettype->kind == AST_TYPE_VOID) {
        if (maybe_fn->flags & AST_FLAG_INLINE && !(cc->flags & CCTRL_TRANSPILING)) {
            return astDecl(maybe_fn->inline_ret,NULL);
        }
        return astReturn(retval,cc->tmp_rettype);
    }

    AstType *ok = astTypeCheck(cc->tmp_rettype,retval,'\0');
    if (!ok) {
        Ast *func = mapGet(cc->global_env, cc->tmp_fname->data);
        typeCheckReturnTypeWarn(cc,func,check,retval);
    }
    if (maybe_fn->flags & AST_FLAG_INLINE && !(cc->flags & CCTRL_TRANSPILING)) {
        return astDecl(maybe_fn->inline_ret,retval);
    }
    return astReturn(retval,cc->tmp_rettype);
}

void parseRaiseCaseException(Cctrl *cc, Ast *case_expr) {
    cctrlRewindUntilStrMatch(cc,str_lit("case"),NULL);
    char *exp = astLValueToString(case_expr,0);
    char *type = astTypeToString(case_expr->type);
    char *suggestion = mprintf("Invalid use of type %s", type);
    cctrlRaiseExceptionFromTo(cc, suggestion, 'c', ':', "`case` must be followed by an integer constant got - %s", exp);
}

Ast *parseCaseLabel(Cctrl *cc, Lexeme *tok) {
    if (cc->tmp_case_list == NULL) {
        cctrlRaiseException(cc,"unexpected 'case' found");
    }
    Ast *case_, *prev, *case_expr = NULL;
    Lexeme *peek;
    AoStr *label;
    int begining,end;
    int ok = 1;

    peek = cctrlTokenPeek(cc);
    if (tokenPunctIs(peek,':')) {
        if (vecEmpty(cc->tmp_case_list)) {
            begining = 0;
        } else {
            prev = cc->tmp_case_list->entries[cc->tmp_case_list->size - 1];
            begining = prev->case_end+1;
        }
        label = astMakeLabel();
    } else {
        case_expr = parseExpr(cc,16);
        begining = evalIntConstExprOrErr(case_expr, &ok);
        if (!ok) {
            if (cc->flags & CCTRL_PASTE_DEFINES && case_expr->kind == AST_LVAR) {
                label = case_expr->lname;
            } else {
                parseRaiseCaseException(cc,case_expr);
            }
        } else {
            label = astMakeLabel();
        }
    }

    tok = cctrlTokenPeek(cc);

    /* We're not doing label to label for transpilation */
    if (tokenPunctIs(tok,TK_ELLIPSIS)) {
        cctrlTokenGet(cc);
        case_expr = parseExpr(cc,16);
        ok = 1;
        end = evalIntConstExprOrErr(case_expr, &ok);
        cctrlTokenExpect(cc,':');
        if (begining > end) {
            cctrlRewindUntilStrMatch(cc,str_lit("case"),NULL);
            char *suggestion = mprintf("Swap the conditions around `case %d ... %d: `",end,begining);
            cctrlRaiseExceptionFromTo(cc, suggestion, 'c', ':', " Condition is in the wrong order '%d' must be lower than '%d'",
                    begining, end);
        }
    } else {
        cctrlTokenExpect(cc,':');
        end = begining;
    }

    List *stmts = listNew();
    peek = cctrlTokenPeek(cc);

    if (!ok && cc->flags & CCTRL_PASTE_DEFINES) {
        case_ = astCase(label,begining,end,stmts);
        case_->type = ast_void_type;
    } else {
        case_ = astCase(label,begining,end,stmts);
        assertUniqueSwitchCaseLabels(cc->tmp_case_list,case_);
    }

    vecPush(cc->tmp_case_list,case_);

    do {
        Ast *stmt = parseStatement(cc);
        if (stmt && stmt->kind != AST_CASE && stmt->kind != AST_DEFAULT) {
            listAppend(stmts,stmt);
        }
        if (stmt->kind == AST_COMPOUND_STMT || stmt->kind == AST_CASE ||
                stmt->kind == AST_DEFAULT || stmt->kind == AST_BREAK || stmt->kind == AST_RETURN
                || stmt->kind == AST_GOTO) break;
        peek = cctrlTokenPeek(cc);
   
        /* @Bug - Something is afoot, this ensures we don't go on forever
         * parsing if there isn't a break and the case is a fall through...
         * feels as though we could go on and just ommit the case. Which would
         * avoid the call to `astCasesCompress()`? */
        if (tokenPunctIs(peek,'}')) break;
    } while (1);

    return case_;
}

Ast *parseDefaultStatement(Cctrl *cc) {
    cctrlTokenExpect(cc,':');
    if (cc->tmp_default_case) {
        cctrlRaiseException(cc,"Duplicate default case");
    }
    Lexeme *peek;
    List *stmts = listNew();
    AoStr *default_label = astMakeLabel();
    /* set here so this is non-null for the next call to parseStatement */
    cc->tmp_default_case = astDefault(default_label,stmts);

    do {
        Ast *stmt = parseStatement(cc);
        if (stmt && stmt->kind != AST_CASE) {
            listAppend(stmts,stmt);
        }
        if (stmt->kind == AST_COMPOUND_STMT || stmt->kind == AST_CASE || stmt->kind == AST_BREAK || stmt->kind == AST_RETURN
                || stmt->kind == AST_GOTO) break;
        peek = cctrlTokenPeek(cc);
        if (tokenPunctIs(peek,'}')) break;
    } while (1);

    return cc->tmp_default_case;
}

Ast *parseSwitchStatement(Cctrl *cc) {
    Ast *cond, *tmp, *original_default_label;
    Lexeme *peek;
    Vec *original_cases;
    AoStr *end_label,*tmp_name,*original_break;
    int switch_bounds_checked = 1;
    char terminating_char = ')';

    peek = cctrlTokenPeek(cc);

    if (!tokenPunctIs(peek,'[') && !tokenPunctIs(peek, '(')) {
        cctrlRaiseException(cc,"Switch '(' or '[' expected got: %s",
                lexemeToString(peek));
    }

    /* Walk past and set the expected terminting character */
    if (peek->i64 == '[') {
        switch_bounds_checked = 0;
        terminating_char = ']';
    }

    cctrlTokenGet(cc);
    cond = parseExpr(cc,16);
    if (!astIsIntType(cond->type)) {
        cctrlRaiseException(cc,"Switch can only have int's at this time");
    }
    cctrlTokenExpect(cc,terminating_char);

    original_break = cc->tmp_loop_end;
    original_default_label = cc->tmp_default_case;
    original_cases = cc->tmp_case_list;

    cc->tmp_case_list = astVecNew();
    cc->tmp_default_case = NULL;

    end_label = astMakeLabel();

    /* this is the current label */
    cc->tmp_loop_end = end_label;

    tmp_name = astMakeTmpName();

    tmp = astLVar(cond->type, tmp_name->data, tmp_name->len);
    listAppend(cc->tmp_locals,tmp);

    parseStatement(cc);
    aoStrRelease(tmp_name);

    Ast *switch_ast = astSwitch(
        cond,
        cc->tmp_case_list,
        cc->tmp_default_case,
        end_label,
        switch_bounds_checked
    );

    cc->tmp_loop_end = original_break;
    cc->tmp_case_list = original_cases;
    cc->tmp_default_case = original_default_label;
    return switch_ast;
}

/* Concatinate the label of the goto with the name of the function 
 * currently being parsed to be able to have uniqe goto labels  */
AoStr *createFunctionLevelGotoLabel(Cctrl *cc, Lexeme *tok) {
    AoStr *label = aoStrNew();
    aoStrCatFmt(label,".%S_%.*s",cc->tmp_fname,tok->len,tok->start);
    return label;
}

Ast *parseStatement(Cctrl *cc) {
    Lexeme *tok, *peek;
    AoStr *label;
    Ast *ret, *ast;
    Map *env;
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
            case KW_DEFAULT:  return parseDefaultStatement(cc);
            case KW_BREAK:    return parseBreakStatement(cc);
            case KW_CONTINUE: return parseContinueStatement(cc);
            case KW_STATIC: {
                env = cc->localenv;
                cc->localenv = NULL;

                AstType *type = parseFullType(cc);
                tok = cctrlTokenGet(cc);

                if (tok->tk_type != TK_IDENT) {
                    cctrlTokenRewind(cc);
                    cctrlRaiseException(cc,"Expected variable name following type declaration '%s' - '%s' <var_name>",
                            astTypeToString(type), astTypeToString(type));
                }
                type = parseArrayDimensions(cc,type);

                Ast *gvar_ast = astGVar(type,tok->start,tok->len,1);


                if (type->kind == AST_TYPE_ARRAY) {
                    ast = parseVariableInitialiser(cc,gvar_ast,
                            PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);

                    cc->localenv = env;
                    mapAdd(env,gvar_ast->gname->data,gvar_ast);
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
                        cctrlRaiseException(cc,"'%s %s' must be a compile time "
                                "constant",
                                astTypeToColorString(gvar_ast->type),
                                astLValueToString(ast,0));
                    }
                } else {
                    cctrlTokenExpect(cc,';');
                    ast = astDecl(gvar_ast,NULL);
                }
                cc->localenv = env;
                mapAdd(env,gvar_ast->gname->data,gvar_ast);
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
                assertTokenIsTerminator(cc,tok,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
                return ast;
            }
            default: {
                cctrlTokenRewind(cc);
                cctrlRaiseException(cc,"Keyword '%.*s' cannot be used in this context",
                        tok->len,tok->start);
            }
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
    if (tok->tk_type == TK_IDENT && tokenPunctIs(peek,':')) {
        label = createFunctionLevelGotoLabel(cc,tok);
        ret = astLabel(label);
        /* consume ':' */
        cctrlTokenExpect(cc,':');
        return ret;
    }

    if (tokenPunctIs(tok,'{')) {
        return parseCompoundStatement(cc);
    }

    if (tok->tk_type == TK_I64) {
        cctrlRaiseException(cc,"Floating integer constant '%ld' cannot be used in this context", tok->i64);
    } else if (tok->tk_type == TK_F64) {
        cctrlRaiseException(cc,"Floating integer constant '%f' cannot be used in this context", tok->f64);
    } else if (tok->tk_type == TK_PUNCT && (tok->i64 == ',')) {
        lexemePrint(tok);
        cctrlTokenRewind(cc);
        cctrlRaiseException(cc, "Floating '%c' cannot be used in this context", tok->i64);
    }

    cctrlTokenRewind(cc);
    ast = parseExpr(cc,16);
    tok = cctrlTokenGet(cc);
    assertTokenIsTerminator(cc,tok,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
    return ast;
}

Ast *parseDeclOrStatement(Cctrl *cc) {
    Lexeme *tok = cctrlTokenPeek(cc);
    if (!tok) {
        return NULL;
    }
    if (cctrlIsKeyword(cc,tok->start,tok->len)) {
        return parseDecl(cc);
    }
    return parseStatement(cc);
}

void parseCompoundStatementInternal(Cctrl *cc, Ast *body) {
    Ast *stmt = NULL;
    Ast *var = NULL;
    AstType *base_type, *type, *next_type;
    Lexeme *tok, *varname, *peek;
    cc->localenv = cctrlCreateAstMap(cc->localenv);
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
                    if (varname->tk_type != TK_IDENT) {
                        cctrlTokenRewind(cc);
                        cctrlRaiseException(cc,"Expected type declaration with identifer got '%.*s' - should be"ESC_BLUE" '%s "ESC_RESET ESC_BOLD"<var_name>'",
                                peek->len, peek->start, astTypeToString(next_type));
                        break;
                    }
                    type = parseArrayDimensions(cc,next_type);
                    var = astLVar(type,varname->start,varname->len);
                    if (!mapAddOrErr(cc->localenv,var->lname->data,var)) {
                        cctrlRewindUntilStrMatch(cc,var->lname->data,var->lname->len,NULL);
                        cctrlRaiseException(cc,"variable `%s` already declared",
                                astLValueToString(var,0));
                    }
                } else {
                    cctrlTokenGet(cc);
                    var = parseFunctionPointer(cc,next_type);
                    mapAdd(cc->localenv,var->fname->data,var);
                }

                if (cc->tmp_locals) {
                    listAppend(cc->tmp_locals, var);
                }

                stmt = parseVariableInitialiser(cc,var,PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);
                if (next_type->kind == AST_TYPE_AUTO) {
                    parseAssignAuto(cc,stmt);
                }

                if (stmt) {
                    listAppend(body->stms,stmt);
                }
                cctrlTokenRewind(cc);
                tok = cctrlTokenGet(cc);

                if (tokenPunctIs(tok, ',')) {
                    continue;
                } else if (tokenPunctIs(tok,';')) {
                    break;
                } else {
                    cctrlRaiseException(cc,"Unexpected %s `%.*s` while parsing statement, perhaps you meant to terminate the declaration with `;`?",
                            lexemeTypeToString(tok->tk_type), tok->len,tok->start);
                }
            }
        } else { 
            if ((stmt = parseStatement(cc)) != NULL) {
                if (stmt->kind == AST_DECL && stmt->declvar->kind == AST_GVAR) {
                    tok = cctrlTokenPeek(cc);
                    continue;
                }
                listAppend(body->stms,stmt);
            } else {
                break;
            }
        }
        tok = cctrlTokenPeek(cc);
    }
    cc->localenv = cc->localenv->parent;
    cctrlTokenExpect(cc,'}');
    cctrlTokenPeek(cc);
}

Ast *parseCompoundStatement(Cctrl *cc) {
    List *stmts = listNew();
    Ast *ast_compound = astCompountStatement(stmts);
    parseCompoundStatementInternal(cc, ast_compound);
    return ast_compound;
}

Ast *parseFunctionDef(Cctrl *cc, AstType *rettype,
        char *fname, int len, Vec *params, int has_var_args, int is_inline) 
{
    Lexeme *next = cctrlTokenPeek(cc);
    if (next->tk_type == TK_KEYWORD && next->i64 == KW_ASM) {
        cctrlTokenGet(cc);
        Lexeme *peek = cctrlTokenPeek(cc);
        if (tokenPunctIs(peek,'{')) {
            AoStr *fname_duped = aoStrDupRaw(fname,len);
            AoStr *asm_fname = astNormaliseFunctionName(fname_duped->data);
            AoStr *prev_asm_name = cc->tmp_asm_fname;
            cc->tmp_asm_fname = asm_fname;
            Ast *asm_block = prsAsm(cc,1);

            Ast *asm_function = astAsmFunctionDef(asm_fname, asm_block->asm_stmt);

            listAppend(asm_block->funcs, asm_function);
            listAppend(cc->asm_blocks, asm_block);

            Ast *asm_func = astAsmFunctionBind(
                    astMakeFunctionType(rettype, params),
                    asm_fname,asm_fname,params);

            if (!mapAddOrErr(cc->asm_functions, asm_fname->data, asm_func)) {
                cctrlIce(cc, "Already defined assembly function: %s", asm_fname->data);
            }

            if (!mapAddOrErr(cc->global_env, fname_duped->data, asm_func)) {
                cctrlIce(cc, "Already defined assembly function: %s as a non Assembly function", asm_fname->data);
            }

            if (is_inline) {
                asm_func->flags = AST_FLAG_INLINE;
            }
            cctrlTokenExpect(cc,'}');
            cc->tmp_asm_fname = prev_asm_name;
            return asm_func;
        } else {
            cctrlRaiseException(cc,"Floating \"asm\" keyword, expected \"asm {\" got - %.*s", 
                    peek->len, peek->start);
        }
    }

    List *locals = listNew();
    Ast *func = NULL;
    List *body = listNew();
    Ast *func_body = astCompountStatement(body);
    AstType *fn_type = NULL;

    cc->localenv = cctrlCreateAstMap(cc->localenv);
    cc->tmp_locals = locals;

    /* Upgrade a prototype to an actual function */
    func = mapGetLen(cc->global_env, fname, len);
    if (!func) {
        cc->tmp_params = params;
        cc->tmp_rettype = rettype;
        fn_type = astMakeFunctionType(cc->tmp_rettype, params);
        func = astFunction(fn_type,fname,len,params,NULL,locals,
                has_var_args);
        mapAdd(cc->global_env, func->fname->data, func);
    } else {
        switch (func->kind) {
            case AST_EXTERN_FUNC:
                cctrlRaiseException(cc,"Cannot redefine extern function: %.*s",len,fname);

            case AST_FUNC:
                cctrlRaiseException(cc,"Cannot redefine function: %.*s",len,fname);

            case AST_ASM_FUNC_BIND:
                cctrlRaiseException(cc,"Cannot redefine asm function: %.*s",
                        len,fname);

            case AST_FUN_PROTO:
                /* upgrade prototype to a function */
                func->locals = cc->tmp_locals;
                func->params = params;
                cc->tmp_params = func->params;
                cc->tmp_rettype = func->type->rettype;
                fn_type = func->type;
                func->kind = AST_FUNC;
                break;

            default:
                cctrlRaiseException(cc,"Unexpected function: %.*s -> %s",
                        cc->lineno,
                        len,fname, astToString(func));
                break;
        }
    }

    /* XXX: This allows us to do recursion by parsing the body after */
    cc->tmp_fname = func->fname;
    Ast *prev_func = cc->tmp_func;
    cc->tmp_func = func;
    func->body = func_body;

    if (is_inline) {
        func->flags |= AST_FLAG_INLINE;
        func->inline_ret = astLVar(func->type->rettype, str_lit("retval"));
    }
    parseCompoundStatementInternal(cc, func_body);
    fn_type->rettype = cc->tmp_rettype;
    if (is_inline) {
        listAppend(func->locals,func->inline_ret);
    }

    cc->localenv = NULL;
    cc->tmp_locals = NULL;
    cc->tmp_rettype = NULL;
    cc->tmp_params = NULL;
    cc->tmp_fname = NULL;
    cc->tmp_func = prev_func;
    return func;
}

Ast *parseExternFunctionProto(Cctrl *cc, AstType *rettype, char *fname, int len) {
    Ast *func;
    int has_var_args = 0;
    Lexeme *tok;
    cc->localenv = cctrlCreateAstMap(cc->localenv);
    cc->tmp_locals = NULL;

    Vec *params = parseParams(cc,')', &has_var_args,1);
    tok = cctrlTokenGet(cc);
    if (!tokenPunctIs(tok, ';')) {
        cctrlRaiseException(cc,"extern %.*s() cannot have a function body "
                "this will be defined elsewhere",
                len,fname);
    }
    AstType *type = astMakeFunctionType(rettype, params);
    func = astFunction(type,fname,len,params,NULL,NULL,0);
    func->kind = AST_EXTERN_FUNC;
    mapAdd(cc->global_env,func->fname->data,func);
    return func;
}

Ast *parseFunctionOrDef(Cctrl *cc, AstType *rettype, char *fname, int len, int is_inline) {
    int has_var_args = 0;
    cctrlTokenExpect(cc,'(');
    cc->localenv = cctrlCreateAstMap(cc->localenv);
    cc->tmp_locals = listNew();

    /* Reset the unique id counter otherwise we get ridiculous numbers */
    astResetLVarId();

    Vec *params = parseParams(cc,')',&has_var_args,1);
    Lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok, '{')) {
        return parseFunctionDef(cc,rettype,fname,len,params,has_var_args,is_inline);
    } else {
        if (rettype->kind == AST_TYPE_AUTO) {
            cctrlRaiseException(cc,"auto cannot be used with a function prototype %.*s() at this time",
                    len,fname);
        }
        AstType *type = astMakeFunctionType(rettype, params);
        Ast *fn = astFunction(type,fname,len,params,NULL,NULL,has_var_args);
        fn->kind = AST_FUN_PROTO;
        mapAdd(cc->global_env,fn->fname->data,fn);
        return fn;
    }
}

Ast *parseAsmFunctionBinding(Cctrl *cc) {
    Lexeme *tok;
    AoStr *asm_fname, *c_fname;
    AstType *rettype;
    Ast *asm_func;
    int has_var_args = 0, is_inline = 0;

    tok = cctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT && tok->start[0] != '_') {
        cctrlRaiseException(cc,"ASM function binds must begin with '_' got: %.*s",
                tok->len,tok->start);
    }

    asm_fname = aoStrDupRaw(tok->start,tok->len);
    Ast *asm_blk = mapGetLen(cc->asm_functions, asm_fname->data, asm_fname->len);

    rettype = parseDeclSpec(cc);

    if (rettype->kind == AST_TYPE_AUTO) {
        cctrlRaiseException(cc,"auto cannot be used with an assembly binding for function %s(), type cannot be automatically deduced",
                asm_fname->data);
    }

    tok = cctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        cctrlRaiseException(cc,"line %d: ASM function requires c function name");
    }
    c_fname = aoStrDupRaw(tok->start,tok->len);
    cc->localenv = cctrlCreateAstMap(cc->localenv);
    cctrlTokenExpect(cc,'(');

    Vec *params = parseParams(cc,')',&has_var_args,0);

    asm_func = astAsmFunctionBind(
            astMakeFunctionType(rettype, params),
            asm_fname,c_fname,params);

    /* update the assembly block so it knows the HC function name */
    if (asm_blk && asm_blk->fname == NULL) {
        asm_blk->fname = c_fname;
    }

    cc->localenv = NULL;
    /* Map a c function to an ASM function */
    mapAdd(cc->asm_funcs,c_fname->data,asm_func);
    cctrlTokenExpect(cc,';');
    if (is_inline) {
        asm_func->flags |= AST_FLAG_INLINE;
    }
    return asm_func;
}

Ast *parseToplevelDef(Cctrl *cc, int *is_global) {
    Ast *variable, *asm_block, *asm_func, *extern_func, *ast;
    AstType *type = NULL;
    Lexeme *tok, *name, *peek;
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
                    cctrlRaiseException(cc,"Floating \"_extern\" keyword, expected \"_extern '_ASM_FUNC'\""); 
                }

                case KW_ASM: {
                    peek = cctrlTokenPeek(cc);
                    if (tokenPunctIs(peek,'{')) {
                        asm_block = prsAsm(cc, 0);
                        listAppend(cc->asm_blocks,asm_block);
                        return asm_block;
                    }
                    cctrlRaiseException(cc,"Floating \"asm\" keyword, expected \"asm {\" got - %.*s",
                            peek->len, peek->start);
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
                    cctrlRaiseException(cc,"Can only call external c functions for the "
                            "time being");
                }
                case KW_INLINE: {
                    peek = cctrlTokenPeek(cc);
                    if (!cctrlGetKeyWord(cc, peek->start, peek->len)) {
                        cctrlRaiseException(cc,"Expected return type declaration got: '%.*s'",
                                peek->len,peek->start);
                    }
                    type = parseFullType(cc);
                    name = cctrlTokenGet(cc);
                    
                    ast = parseFunctionOrDef(cc,type,name->start,name->len,1); 
                    ast->flags |= AST_FLAG_INLINE;
                    return ast;
                }
                case KW_PUBLIC:
                case KW_PRIVATE:
                case KW_ATOMIC:
                    continue;

                case KW_STATIC:
                    type = parseDeclSpec(cc);
                    if (type == NULL) {
                        cctrlRaiseException(cc,"Expected type declaration");
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
                    cctrlRaiseException(cc,"Unexpected floating keyword: %.*s",
                            tok->len,tok->start);
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
            } else if ((variable = mapGetLen(cc->global_env,tok->start, tok->len))) {
                cctrlTokenRewind(cc);
                ast = parseExpr(cc,16);
                cctrlTokenExpect(cc,';');
                *is_global = 1;
                return ast;
            } else {
                cctrlTokenRewind(cc);
                type = parseDeclSpec(cc);
                if (type == NULL) {
                    cctrlRaiseException(cc,"Undefined type: %.*s",tok->len,tok->start);
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
        } else if (tok->tk_type == TK_I64) {
            cctrlRaiseException(cc,"Floating integer constant '%ld' cannot be used in this context", tok->i64);
        } else if (tok->tk_type == TK_F64) {
            cctrlRaiseException(cc,"Floating float constant '%f' cannot be used in this context", tok->f64);
        }

        name = cctrlTokenGet(cc);

        if (name->tk_type == TK_KEYWORD) {
            switch (name->i64) {
                case KW_CLASS:
                    if (!astIsIntType(type)) {
                        cctrlRaiseException(cc,"Can only make intrinsic types from integer types, got %s",
                                astTypeToString(type));
                    }
                    parseClassDef(cc,1);
                    cctrlTokenExpect(cc,';');
                    continue;
                default: {
                    cctrlRaiseException(cc,"%s can only prefix a class",lexemeToString(name));
                }
            }
        }

        if (name->tk_type != TK_IDENT) {
            cctrlRaiseException(cc,"Identifier expected: got %s",lexemeToString(name));
        }

        type = parseArrayDimensions(cc,type);
        tok = cctrlTokenPeek(cc);

        if (tokenPunctIs(tok,'=') && type->kind != AST_TYPE_ARRAY) {
            variable = astGVar(type,name->start,name->len,0);
            if (type->kind == AST_TYPE_AUTO) {
                cctrlRaiseException(cc,"line auto cannot be used without an initialiser");
            }
            Ast *ast_decl = astDecl(variable,NULL);

            listAppend(cc->ast_list,ast_decl);
            mapAdd(cc->global_env,variable->gname->data,variable);
            cctrlTokenRewind(cc);

            Ast *ast_expr = parseExpr(cc,16); // parseVariableInitialiser(cc,variable,PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);

            if (ast_expr->right->kind != AST_STRING) {
                *is_global = 1;
            } else {
                ast_decl->declinit = ast_expr->right;
                cctrlTokenExpect(cc,';');
                return ast_decl;
            }
            cctrlTokenExpect(cc,';');
            return ast_expr;
        } else if (type->kind == AST_TYPE_ARRAY) {
            variable = astGVar(type,name->start,name->len,0);
            mapAdd(cc->global_env,variable->gname->data,variable);
            ast = parseVariableInitialiser(cc,variable,PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);
            if (type->kind == AST_TYPE_AUTO) {
                parseAssignAuto(cc,ast);
            }
            return ast;
        }

        if (tokenPunctIs(tok, '(')) {
            return parseFunctionOrDef(cc,type,name->start,name->len,0); 
        }

        if (tokenPunctIs(tok,';') || tokenPunctIs(tok, ',')) {
            cctrlTokenGet(cc);
            variable = astGVar(type,name->start,name->len,0);
            mapAdd(cc->global_env,variable->gname->data,variable);
            return astDecl(variable,NULL);
        }
        cctrlRaiseException(cc,"Cannot handle '%s'",lexemeToString(tok));
    }
}

void parseToAst(Cctrl *cc) {
    Ast *ast;
    Lexeme *tok;
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
