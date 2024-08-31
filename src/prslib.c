#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "dict.h"
#include "lexer.h"
#include "list.h"
#include "prsutil.h"
#include "prslib.h"
#include "util.h"

static AstType *parseArrayDimensionsInternal(Cctrl *cc, AstType *base_type);

void parseAssignAuto(Cctrl *cc, Ast *ast) {
    if (ast->declinit == NULL) {
        loggerPanic("line %ld: auto must have an initaliser\n", cc->lineno);
    }

    if (ast->declinit->kind == AST_FUNC) {
        ast->declvar->type = astMakeFunctionType(
                ast->declinit->type->rettype,
                ast->declinit->params);
        ast->declvar->type->has_var_args = ast->declinit->type->has_var_args;
        ast->declvar->type->rettype->has_var_args = ast->declinit->type->has_var_args;
    //return astMakeFunctionType(rettype,params);
        return;
        loggerPanic("line %ld: auto with functions is not yet supported\n",
                cc->lineno);
    }
    ast->declvar->type = ast->declinit->type;
}

AstType *parseReturnAuto(Cctrl *cc, Ast *retval) {
    AstType *copy;
    switch (retval->kind) {
        case AST_LVAR:
        case AST_STRING:
        case AST_ASM_FUNCALL:
        case AST_FUNPTR_CALL:
        case AST_FUNCALL:
        case AST_FUNPTR:
        case AST_TYPE_ARRAY:
        case AST_TYPE_INT:
        case AST_TYPE_CHAR:
        case AST_TYPE_FLOAT:
        case AST_TYPE_POINTER:
        case AST_TYPE_VOID:
        case '+':
        case '-':
        case '*':
        case '/':
        case '<':
        case '>':
        case '!':
        case '&':
        case '|':
        case '%':
        case '$':
        case '~':
        case TK_AND_AND:
        case TK_OR_OR:
        case TK_EQU_EQU:
        case TK_NOT_EQU:
        case TK_LESS_EQU:
        case TK_GREATER_EQU:
        case TK_PLUS_PLUS:
        case TK_MINUS_MINUS:
        case TK_SHL:
        case TK_SHR:
            copy = malloc(sizeof(AstType));
            memcpy(copy,retval->type,sizeof(AstType));
            return copy;
        default:
            loggerPanic("line %ld: Could not determine return type for %s()\n",
                    cc->lineno,cc->tmp_fname->data);

    }
}

AstType *parsePointerType(Cctrl *cc, AstType *type) {
    lexeme *tok;
    while (1) {
        tok = cctrlTokenGet(cc);
        if (!tokenPunctIs(tok, '*')) {
            cctrlTokenRewind(cc);
            return type;
        }
        type = astMakePointerType(type);
    }
    return type;
}

AstType *parseFunctionPointerType(Cctrl *cc,
        char **fnptr_name, int *fnptr_name_len, AstType *rettype)
{
    int has_var_args;
    lexeme *fname;
    List *params;
    cctrlTokenExpect(cc,'*');
    fname = cctrlTokenGet(cc);
    if (fname->tk_type != TK_IDENT) {
        loggerPanic("line %d: Expected function pointer name got: %s\n",
                fname->line, lexemeToString(fname));
    }
    *fnptr_name = fname->start;
    *fnptr_name_len = fname->len;
    cctrlTokenExpect(cc,')');
    cctrlTokenExpect(cc,'(');
    params = parseParams(cc,')',&has_var_args,0);
    return astMakeFunctionType(rettype,params);
}

Ast *parseFunctionPointer(Cctrl *cc, AstType *rettype) {
    Ast *ast;
    char *fnptr_name;
    int fnptr_name_len;
    AstType *fnptr_type;

    fnptr_type = parseFunctionPointerType(cc,
            &fnptr_name,
            &fnptr_name_len,
            rettype);

    ast = astFunctionPtr(
            fnptr_type,
            fnptr_name,
            fnptr_name_len,
            fnptr_type->params);
    return ast;
}

Ast *parseDefaultFunctionParam(Cctrl *cc, Ast *var) {
    if (var->type->kind == AST_TYPE_ARRAY) {
        loggerPanic("line %ld: Cannot have a default value of type array.\n",
                cc->lineno);
    }
    Ast *init = parseExpr(cc,16);
    return astFunctionDefaultParam(var,init);
}

List *parseParams(Cctrl *cc, long terminator, int *has_var_args, int store) {
    List *params = listNew();
    lexeme *tok, *pname;
    AstType *type;
    Ast *var;
    int arg_count = 0;

    tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok,terminator)) {
        return params;
    }
    cctrlTokenRewind(cc);
    
    while (1) {
        pname = cctrlTokenPeek(cc);

        /* VarArgs */
        if (tokenPunctIs(pname, TK_ELLIPSIS)) {
            cctrlTokenGet(cc);
            var = astVarArgs();
            dictSet(cc->localenv,var->argc->lname->data,var->argc);
            dictSet(cc->localenv,var->argv->lname->data,var->argv);
            listAppend(params,var);
            if (cc->tmp_locals) {
                listAppend(cc->tmp_locals, var->argc);
                listAppend(cc->tmp_locals, var->argv);
            }
            *has_var_args = 1;
            /* Var args _has_ to be last */
            cctrlTokenExpect(cc,terminator);
            return params;
        }

        type = parseDeclSpec(cc);
        pname = cctrlTokenGet(cc);

        if (tokenPunctIs(pname, ')') && arg_count == 0) {
            if (type->kind == AST_TYPE_VOID) {
                return params;
            }
        }

        arg_count++;
        if (pname->tk_type != TK_IDENT) {
            /* Function pointer */
            if (tokenPunctIs(pname, '(')) {
                type = parseArrayDimensions(cc,type);
                if (type->kind == AST_TYPE_ARRAY) {
                    type = astMakePointerType(type->ptr);
                }
                var = parseFunctionPointer(cc,type);
                dictSet(cc->localenv,var->fname->data,var);
                if (cc->tmp_locals) {
                    listAppend(cc->tmp_locals, var);
                }
                listAppend(params, var);
                tok = cctrlTokenGet(cc);
                if (tokenPunctIs(tok, terminator)) {
                    return params;
                }
                continue;
            } else if (tokenPunctIs(pname,',') || tokenPunctIs(pname,')')) {
                type = parseArrayDimensions(cc,type);
                if (type->kind == AST_TYPE_ARRAY) {
                    type = astMakePointerType(type->ptr);
                }
                var = astLVar(type,"_unknown",7);
                if (cc->tmp_locals) {
                    listAppend(cc->tmp_locals, var);
                }
                listAppend(params, var);
                if (tokenPunctIs(pname, terminator)) {
                    return params;
                }
                continue;
            } else {
                loggerPanic("line %d: Identifier expected, got: %s\n",
                        pname->line,lexemeToString(pname));
            }
        }

        type = parseArrayDimensions(cc,type);
        if (type->kind == AST_TYPE_ARRAY) {
            type = astMakePointerType(type->ptr);
        }
        var = astLVar(type,pname->start,pname->len);

        tok = cctrlTokenGet(cc);
        if (tokenPunctIs(tok, '=')) {
            var = parseDefaultFunctionParam(cc,var);
            if (store) {
                if (!dictSet(cc->localenv,var->declvar->lname->data,var)) {
                    loggerPanic("line: %ld variable %s already declared\n",
                            cc->lineno,astLValueToString(var,0));
                }
            }
            tok = cctrlTokenGet(cc);
        } else {
            if (store) {
                if (!dictSet(cc->localenv,var->lname->data,var)) {
                    loggerPanic("line: %ld variable %s already declared\n",
                            cc->lineno,astLValueToString(var,0));
                }
            }
       }

        if (cc->tmp_locals) {
            listAppend(cc->tmp_locals, var);
        }

        listAppend(params, var);

        if (tokenPunctIs(tok,terminator)) {
            return params;
        }

        if (!tokenPunctIs(tok,',')) {
            loggerPanic("line %d: Comma expected, got %s\n",
                    tok->line,lexemeToString(tok));
        }
    }
}

/* Does not parse pointer types, only the base type */
AstType *parseBaseDeclSpec(Cctrl *cc) {
    lexeme *tok = cctrlTokenGet(cc);
    AstType *type;

    if (!tok) {
        return NULL;
    }

    if (tok->tk_type == TK_KEYWORD || tok->tk_type == TK_IDENT) {
        if ((type = parseGetType(cc, tok)) == NULL) {
            loggerPanic("line %ld: Type %.*s not found\n",cc->lineno,
                    tok->len,tok->start);
        }
    } else {
        loggerPanic("line %ld: Declaration or specfier must be an itentifier got: %s\n",
                cc->lineno,
                lexemeToString(tok));
    }

    return type;
}

AstType *parseDeclSpec(Cctrl *cc) {
    AstType *type = parseBaseDeclSpec(cc);
    type = parsePointerType(cc,type);
    return type;
}

static AstType *parseArrayDimensionsInternal(Cctrl *cc, AstType *base_type) {
    Ast *size;
    lexeme *tok, *next_tok;
    int dimension;

    tok = cctrlTokenGet(cc);
    dimension = -1;

    if (!tokenPunctIs(tok, '[')) {
        cctrlTokenRewind(cc);
        return NULL;
    }

    next_tok = cctrlTokenPeek(cc);
    if (!tokenPunctIs(next_tok, ']')) {
        size = parseExpr(cc,16);
        dimension = evalIntConstExpr(size);
    }
    cctrlTokenExpect(cc,']');
    AstType *sub_type = parseArrayDimensionsInternal(cc,base_type);
    if (sub_type) {
        if (sub_type->len == -1 && dimension == -1) {
            loggerPanic("line %ld: Array size not specified.\n",cc->lineno);
        }
        return astMakeArrayType(sub_type,dimension);
    }

    return astMakeArrayType(base_type,dimension);
}

AstType *parseArrayDimensions(Cctrl *cc, AstType *base_type) {
    AstType *type = parseArrayDimensionsInternal(cc,base_type);
    if (type) {
        return type;
    }
    return base_type;
} 

/* Calls TokenGet internally so ensure you are 1 token behind the type */
AstType *parseFullType(Cctrl *cc) {
    AstType *type = parseDeclSpec(cc);
    return parseArrayDimensions(cc,type);
}

void parseDeclInternal(Cctrl *cc, lexeme **tok, AstType **type) {
    AstType *_type = parseDeclSpec(cc);
    lexeme *_tok = cctrlTokenGet(cc);
    if (tokenPunctIs(_tok,';')) {
        cctrlTokenRewind(cc);
        *tok = NULL;
        return;
    }

    if (_tok->tk_type != TK_IDENT) {
        cctrlTokenRewind(cc);
        *tok = NULL;
    } else {
        *tok = _tok;
    }
    *type = parseArrayDimensions(cc,_type);
}


static Ast *findFunctionDecl(Cctrl *cc, char *fname, int len) {
    Ast *decl;
    if ((decl = dictGetLen(cc->global_env,fname,len)) != NULL) {
        if (decl->kind == AST_FUNC ||
            decl->kind == AST_EXTERN_FUNC ||
            decl->kind == AST_FUN_PROTO) {
            return decl;
        }
    } else if (cc->localenv && (decl = dictGetLen(cc->localenv,fname,len)) != NULL) {
        if (decl->kind == AST_FUNPTR || decl->kind == AST_LVAR) {
            return decl;
        }
    } else if ((decl = dictGetLen(cc->asm_funcs,fname,len)) != NULL) {
        /* Assembly function */
        return decl;
    }
    return NULL;
}

List *parseArgv(Cctrl *cc, Ast *decl, long terminator, char *fname, int len) {
    List *argv, *var_args = NULL, *params = NULL, *parameter = NULL; 
    Ast *ast, *param = NULL;
    AstType *check, *rettype;
    lexeme *tok;

    if (decl) {
        rettype = decl->type->rettype;
        if (decl->kind == AST_FUNPTR) {
            if (decl->params == NULL) {
                params = ((Ast *)decl)->type->params;
            } else {
                params = decl->params;
            }
        } else {
            params = decl->params;
        }
    }


    argv = listNew();
    tok = cctrlTokenPeek(cc);
    if (params) {
        parameter = params->next;
        param = parameter->value;
    }

    argv = listNew();
    tok = cctrlTokenPeek(cc);

    while (tok && !tokenPunctIs(tok, terminator)) {
        ast = parseExpr(cc,16);
        if (ast == NULL) {
            if (param && param->kind == AST_DEFAULT_PARAM) {
                ast = param->declinit;
            }
        }

        if (param != NULL && param->kind == AST_VAR_ARGS) {
            if (decl && decl->kind == AST_EXTERN_FUNC) {
                listAppend(argv,ast);
            } else {
                if (var_args == NULL) {
                    var_args = listNew();
                }
                listAppend(var_args,ast);
            }
        } else {
            /* Does a distinctly adequate job of type checking function parameters */
            if (param && param->kind != AST_DEFAULT_PARAM) {
                if ((check = astTypeCheck(param->type,ast)) == NULL) {
                    Ast *func = findFunctionDecl(cc,fname,len);
                    char *fstring, *expected, *got, *ast_str;
                    if (func) {
                        fstring = astFunctionToString(func);
                    } else {
                        fstring = astFunctionNameToString(rettype,fname,len);
                    }

                    expected = astTypeToColorString(param->type);
                    got = astTypeToColorString(ast->type);
                    ast_str = astLValueToString(ast,0);
                    loggerWarning("line %ld: %s incompatiable function argument %s got %s %s\n",
                            cc->lineno,fstring,expected,got,ast_str);
                    free(fstring);
                    free(expected);
                    free(got);
                    free(ast_str);
                }
            }
            listAppend(argv,ast);
        }

        tok = cctrlTokenGet(cc);

        if (tokenPunctIs(tok, terminator)) {
            break;
        }

        if (!tokenPunctIs(tok,',')) {
            loggerPanic("line %d: Unexpected token: Expected: ',' got: '%s'\n",
                   tok->line,lexemeToString(tok));
        }

        if (parameter && parameter->next != parameter) {
            if (param->kind != AST_VAR_ARGS) {
                parameter = parameter->next;
                param = parameter->value;
            }
        }
        tok = cctrlTokenPeek(cc);
    }

    if (var_args) {
        /* set the argument count and merge lists */
        listAppend(argv, astI64Type(listCount(var_args)));
        listMergeAppend(argv,var_args);
    }

    if (argv->next == argv) {
        cctrlTokenGet(cc);
    }

    return argv;
}

/* Read function arguments for a function being called */
Ast *parseFunctionArguments(Cctrl *cc, char *fname, int len, long terminator) {
    List *argv, *params = NULL; 
    AstType *rettype;
    Ast *decl;

    decl = findFunctionDecl(cc,fname,len);
    if (decl) {
        rettype = decl->type->rettype;
    }

    argv = parseArgv(cc,decl,terminator,fname,len);

    if (!decl) {
        if ((len == 6 && !strncmp(fname,"printf",6))) {
            params = listNew();
            rettype = ast_int_type;
            return astFunctionCall(rettype,fname,len,argv,params);
        }
        loggerPanic("line %ld: Function: %.*s() not defined\n",
                cc->lineno,len,fname);
    }

    if (argv->next == argv) {
        params = listNew();
    }

    switch (decl->kind) {
        case AST_LVAR:
        case AST_FUNPTR:
            return astFunctionPtrCall(rettype,fname,len,argv,params,decl);

        case AST_ASM_FUNCDEF:
        case AST_ASM_FUNC_BIND:
            return astAsmFunctionCall(rettype,
                    aoStrDup(decl->asmfname),argv,params);

        case AST_EXTERN_FUNC:
        case AST_FUNC:
        case AST_FUN_PROTO:
            return astFunctionCall(rettype,fname,len,argv,params);
        default: {
            loggerPanic("line %ld: Unknown function: %.*s\n",
                    cc->lineno,len,fname);
        }
    }
}

/* parse either a function call or a variable being used */
static Ast *parseIdentifierOrFunction(Cctrl *cc, char *name, int len,
        int can_call_function)
{
    Ast *ast;
    int is_lparen;
    lexeme *tok,*peek;

    tok = cctrlTokenGet(cc);
    peek = cctrlTokenPeek(cc);
    is_lparen = tokenPunctIs(tok,'(');

    if (!is_lparen) {
        cctrlTokenRewind(cc);
        if ((ast = cctrlGetVar(cc, name, len)) == NULL) {
            loggerPanic("line %ld: Cannot find variable: %.*s\n",
                    cc->lineno,len,name);
        }

        if (can_call_function) {
            /* Function calls with no arguments are 'Function;' */
            if ((tokenPunctIs(tok,';') || tokenPunctIs(tok,',') || 
                tokenPunctIs(tok,')'))  
                    && parseIsFunction(ast)) {
                if (ast->kind == AST_ASM_FUNC_BIND || ast->kind == AST_ASM_FUNCDEF) {
                    return astAsmFunctionCall(ast->type->rettype,
                            aoStrDup(ast->asmfname),listNew(),listNew());
                } else {
                    return astFunctionCall(ast->type->rettype,
                            ast->fname->data,ast->fname->len,listNew(),listNew());
                }
            }
        }
        return ast;
    }

    /* Is a function call if the next char is '(' and the peek is not a type */
    if (!cctrlIsKeyword(cc,peek->start,peek->len)) {
        return parseFunctionArguments(cc,name,len,')');
    }

    /* Is a postfix typecast, need to grap the variable and 'ParsePostFixExpr' will do the cast */
    if ((ast = cctrlGetVar(cc, name, len)) == NULL) {
        loggerPanic("line %ld: Cannot find variable: %.*s\n",cc->lineno,
                len, name);
    }
    cctrlTokenRewind(cc);
    return ast;
}

static Ast *parsePrimary(Cctrl *cc) {
    Ast *ast;
    lexeme *prev,*tok;
    int can_call_function = 1;
    cctrlTokenRewind(cc);

    prev = cctrlTokenGet(cc);
    tok = cctrlTokenGet(cc);

    if (!tok) {
        return NULL;
    }

    if (tokenPunctIs(prev,'&')) {
        can_call_function = 0;
    }

    switch (tok->tk_type) {
    case TK_IDENT: {
        ast = parseIdentifierOrFunction(cc, tok->start, tok->len,
                can_call_function);
        return ast;
    }
    case TK_I64: {
        ast = astI64Type(tok->i64);
        return ast;
    }
    case TK_F64:
        return astF64Type(tok->f64);
    case TK_CHAR_CONST:
        ast = astCharType(tok->i64);
        if (tok->i64 > UCHAR_MAX) {
            ast->type = ast_uint_type;
        }
        return ast;
    case TK_STR: {
        aoStr *str = aoStrNew();
        cctrlTokenRewind(cc);
        /* Concatinate adjacent strings together */
        while ((tok = cctrlTokenGet(cc)) != NULL && tok->tk_type == TK_STR) {
            aoStrCatPrintf(str,"%.*s",tok->len,tok->start);
        }
        cctrlTokenRewind(cc);
        ast = astString(str->data, str->len);
        if (cc->strings) {
            listAppend(cc->strings, ast);
        }
        free(str);
        return ast;
    }
    case TK_PUNCT:
        cctrlTokenRewind(cc);
        return NULL;
    case TK_EOF:
        loggerPanic("line %d: Unexpected EOF\n", tok->line);
        break;
    default:
        loggerPanic("line %d: Unexpected input %s\n",tok->line,astKindToString(tok->tk_type));
    }
}

static int parseGetPriority(lexeme *tok) {
    switch (tok->i64) {
    case '.': case '[': case TK_ARROW: case '(':
        return 1;
    case '!': case '~': 
    case TK_PLUS_PLUS: 
    case TK_MINUS_MINUS: 
    case TK_PRE_PLUS_PLUS:
    case TK_PRE_MINUS_MINUS:
        return 2;

    case '*': case '/': case '%':
        return 3;

    case '-':
    case '+':
        return 4;

    case TK_SHL:
    case TK_SHR:
        return 5;

    case TK_LESS_EQU: 
    case TK_GREATER_EQU:
    case '<':
    case '>':
        return 6;

    case TK_EQU_EQU:
    case TK_NOT_EQU:
        return 7;

    case '^': 
        return 8;
    case '&':
        return 9;
    case '|':
        return 10;

    case TK_AND_AND:
        return 11;

    case TK_OR_OR:
        return 12;

    case '=':
    case TK_ADD_EQU: 
    case TK_SUB_EQU: 
    case TK_MUL_EQU: 
    case TK_DIV_EQU: 
    case TK_MOD_EQU: 
    case TK_AND_EQU: 
    case TK_OR_EQU:  
    case TK_XOR_EQU: 
    case TK_SHL_EQU: 
    case TK_SHR_EQU:
        return 13;

    default:
        return -1;
    }
}

Ast *parseSubscriptExpr(Cctrl *cc, Ast *ast) {
    Ast *subscript = parseExpr(cc,16);
    cctrlTokenExpect(cc, ']');
    Ast *binop = astBinaryOp('+', ast, subscript);
    return astUnaryOperator(binop->type->ptr, AST_DEREF, binop);
}

Ast *parseGetClassField(Cctrl *cc, Ast *cls) {
    AstType *type;
    if (!parseIsClassOrUnion(cls->type->kind)) {
        loggerPanic("line %ld: Expected class got: %s %s\n",
                cc->lineno,
                astKindToString(cls->kind),
                astToString(cls));
    }

    type = cls->type;

    lexeme *tok = cctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        loggerPanic("line %d: Expected class member got: %s\n",
                tok->line,
                lexemeToString(tok));
    }

    // XXX: This is hacky and only for recusive data types 
    if (type->fields == NULL && cls->kind == AST_DEREF) {
        type = cls->cls->type;
    }
    AstType *field = dictGetLen(type->fields, tok->start, tok->len);
    if (!field) {
        loggerPanic("line %d: Property: %.*s does not exist on class\n", 
                tok->line,tok->len, tok->start);
    }
    aoStr *field_name = aoStrDupRaw(tok->start, tok->len);
    return astClassRef(field, cls, aoStrMove(field_name));
}

static int parseCompoundAssign(lexeme *tok) {
    if (tok->tk_type != TK_PUNCT) {
        return 0;
    }

    switch (tok->i64) {
        case TK_ADD_EQU: return '+';
        case TK_SUB_EQU: return '-';
        case TK_MUL_EQU: return '*';
        case TK_DIV_EQU: return '/';
        case TK_MOD_EQU: return '%';
        case TK_AND_EQU: return '&';
        case TK_OR_EQU:  return '|';
        case TK_XOR_EQU: return '^';
        case TK_SHL_EQU: return TK_SHL;
        case TK_SHR_EQU: return TK_SHR;
        default: return 0;
    }
}

Ast *parseExpr(Cctrl *cc, int prec) {
    Ast *LHS, *RHS;
    lexeme *tok;
    int prec2, next_prec, compound_assign;

    if ((LHS = parseUnaryExpr(cc)) == NULL) {
        return NULL;
    }

    while (1) {
        /* Can be TK_IDENT, or TK_F64 or TK_I64 or when type casting exists '(' TK_PUNCT */
        if ((tok = cctrlTokenGet(cc)) == NULL) {
            return LHS;
        }

        if (tok->tk_type != TK_PUNCT) {
            cctrlTokenRewind(cc);
            return LHS;
        }

        prec2 = parseGetPriority(tok);
        if (prec2 < 0 || prec <= prec2) {
            cctrlTokenRewind(cc);
            return LHS;
        }

        if (tokenPunctIs(tok,'.')) {
            LHS = parseGetClassField(cc,LHS);
            continue;
        }

        if (tokenPunctIs(tok,'(')) {
            /* XXX: Should this check still be here ? Not sure as 
             * casting _anything_ makes sense */
            // assertLValue(LHS,cc->lineno);
            AstType *type = parseDeclSpec(cc);
            LHS = astCast(LHS,type);
            cctrlTokenExpect(cc,')');
            continue;
        }

        if (tokenPunctIs(tok, TK_PLUS_PLUS) || tokenPunctIs(tok, TK_MINUS_MINUS)) {
            assertLValue(LHS,cc->lineno);
            LHS = astUnaryOperator(LHS->type, tok->i64, LHS);
            continue;
        }

        if (tokenPunctIs(tok,'[')) {
            LHS = parseSubscriptExpr(cc,LHS);
            continue;
        }

        if (tokenPunctIs(tok, TK_ARROW)) {
            if (LHS->type->kind != AST_TYPE_POINTER) {
                loggerPanic("line %d: Pointer expected got: %s %s\n",
                        tok->line,
                        astTypeToString(LHS->type), astToString(LHS));
            }
            LHS = astUnaryOperator(LHS->type->ptr, AST_DEREF, LHS);
            LHS = parseGetClassField(cc, LHS);
            continue;
        }

        compound_assign = parseCompoundAssign(tok);
        if (tokenPunctIs(tok, '=') || compound_assign) {
            assertLValue(LHS,cc->lineno);
        }
        
        next_prec = prec2;
        if (tok->i64 == '=') {
            next_prec++;
        }

        RHS = parseExpr(cc,next_prec);
        if (!RHS) {
            loggerPanic("line %d: Second operand missing to: %s\n",
                tok->line,astToString(LHS));
        }

        if (compound_assign) {
            LHS = astBinaryOp('=', LHS, 
                    astBinaryOp(compound_assign, LHS, RHS));
        } else {
            LHS = astBinaryOp(tok->i64,LHS,RHS);
        }
    }
}

static Ast *parseCast(Cctrl *cc) {
    Ast *ast;
    AstType *cast_type;

    cctrlTokenExpect(cc,'<');
    cast_type = parseDeclSpec(cc);
    cctrlTokenExpect(cc,'>');
    cctrlTokenExpect(cc,'(');
    ast = parseExpr(cc,16);
    cctrlTokenExpect(cc,')');
    ast = astCast(ast,cast_type);
    return ast;
}

static AstType *parseSizeOfType(Cctrl *cc) {
    lexeme *tok,*peek;
    AstType *type;
    Ast *ast;

    tok = cctrlTokenGet(cc);
    peek = cctrlTokenPeek(cc);

    if (tokenPunctIs(tok,'(') && cctrlIsKeyword(cc,peek->start,peek->len)) {
        type = parseFullType(cc);
        cctrlTokenExpect(cc,')');
        peek = cctrlTokenPeek(cc);
        return type;
    }

    cctrlTokenRewind(cc);
    ast = parseUnaryExpr(cc);
    return ast->type;
}

static Ast *parseSizeof(Cctrl *cc) {
    AstType *type = parseSizeOfType(cc);
    long size = type->size;
    assert(size >= 0);
    return astI64Type(size);
}

Ast *parsePostFixExpr(Cctrl *cc) {
    Ast *ast;
    AstType *type;
    lexeme *tok,*peek;

    /* parse primary rightly or wrongly, amongst other things, either gets a 
     * variable or parses a function call. */
    if ((ast = parsePrimary(cc)) == NULL) {
        return NULL;
    }

    if (parseIsFunctionCall(ast)) {
        return ast;
    }

    while (1) {
        tok = cctrlTokenGet(cc);

        if (tokenPunctIs(tok,'[')) {
            /* XXX: something is wrong with the below however, if a pointer gets 
             * parsed as a binary expression it works, but does not for arrays.
             * However if you put parenthesis around the pointer access it blows 
             * up.
             */

            /* This is for normal arrays */
            if (ast->type->kind == AST_TYPE_ARRAY) {
                ast = parseSubscriptExpr(cc,ast);
                if (tokenPunctIs(cctrlTokenPeek(cc),'(')) {
                    continue;
                } else {
                    return ast;
                }
            }
            peek = cctrlTokenPeek(cc);
            if (tokenPunctIs(peek,'(')) {
                continue;
            } else {
                cctrlTokenRewind(cc);
                return ast;
            }
        }


        if (tokenPunctIs(tok,'.')) {
            ast = parseGetClassField(cc,ast);
            continue;
        }

        /* Postfix type cast OR function pointer on a class */
        if (tokenPunctIs(tok,'(')) {
            peek = cctrlTokenPeek(cc);
            if (cctrlIsKeyword(cc,peek->start,peek->len)) {
                type = parseDeclSpec(cc);
                cctrlTokenExpect(cc,')');
                ast = astCast(ast,type);
                continue;
            } else if (ast->kind == AST_CLASS_REF) { 
                int len = strlen(ast->field);
                List *argv = parseArgv(cc,ast,')',ast->field,len);
                ast = astFunctionPtrCall(
                        ast->type->rettype,
                        ast->field,
                        len,
                        argv,
                        ast->type->params,
                        ast);
                continue;
            }
        }

        if (tokenPunctIs(tok,TK_ARROW)) {
            if (ast->type->kind != AST_TYPE_POINTER) {
                loggerPanic("line %d: Pointer expected got: %s\n",
                        tok->line,
                        astKindToString(ast->type->kind));
            }
            ast = astUnaryOperator(ast->type->ptr,AST_DEREF,ast);
            ast = parseGetClassField(cc,ast);
            continue;
        }

        if (tokenPunctIs(tok,TK_PLUS_PLUS) ||
                tokenPunctIs(tok,TK_MINUS_MINUS)) {
            assertLValue(ast,cc->lineno);
            if (tok->i64 == TK_PLUS_PLUS) {
                return astUnaryOperator(ast->type,TK_PLUS_PLUS,ast);
            } else {
                return astUnaryOperator(ast->type,TK_MINUS_MINUS,ast);
            }
        }

        cctrlTokenRewind(cc);
        return ast;
    }
}

Ast *parseUnaryExpr(Cctrl *cc) {
    lexeme *tok;
    Ast *ast;

    if ((tok = cctrlTokenGet(cc)) == NULL) {
        loggerPanic("line %ld: Unexpected end of input\n",cc->lineno);
    }

    if (tok->tk_type == TK_KEYWORD) {
        switch (tok->i64) {
            case KW_SIZEOF: return parseSizeof(cc);
            case KW_CAST:   return parseCast(cc);
            case KW_DEFINED: {
                cctrlTokenExpect(cc,'(');
                tok = cctrlTokenGet(cc);

                if (tok->i64 != ')') {
                    ast = astI64Type(1);
                } else {
                    ast = astI64Type(0);
                }
                if (tok->i64 != ')') {
                    cctrlTokenExpect(cc,')');
                }
                return ast;
            }
            default:
                loggerPanic("line %d: Unexpected keyword: %.*s while parsing unary expression\n",
                        tok->line,tok->len,tok->start);
        }
    }

    if (tokenPunctIs(tok,'(')) {
        ast = parseExpr(cc,16);
        cctrlTokenExpect(cc,')');
        return ast;
    }

    /* Obtaining the address of a variable */
    if (tokenPunctIs(tok, '&')) {
        Ast *operand = parseUnaryExpr(cc);
        /* We do not take the address of a function */
        if (parseIsFunction(operand)) {
            return operand;
        }
        return astUnaryOperator(astMakePointerType(operand->type),
                AST_ADDR,operand);
    }

    /* Dereferencing */
    if (tokenPunctIs(tok, '*')) {
        Ast *operand = parseUnaryExpr(cc);
        return astUnaryOperator(operand->type->ptr,
                AST_DEREF,operand);
    }

    if (tokenPunctIs(tok, TK_PLUS_PLUS)) {
        Ast *operand = parseUnaryExpr(cc);
        return astUnaryOperator(operand->type,
                TK_PRE_PLUS_PLUS,operand);
    }

    if (tokenPunctIs(tok, TK_MINUS_MINUS)) {
        Ast *operand = parseUnaryExpr(cc);
        return astUnaryOperator(operand->type,
                TK_PRE_MINUS_MINUS,operand);
    }

    if (tokenPunctIs(tok,'~')) {
        Ast *operand = parseUnaryExpr(cc);
        return astUnaryOperator(ast_int_type,'~',operand);
    }

    if (tokenPunctIs(tok,'!')) {
        Ast *operand = parseUnaryExpr(cc);
        return astUnaryOperator(operand->type,'!',operand);
    }

    if (tokenPunctIs(tok,'-')) {
        Ast *operand = parseUnaryExpr(cc);
        operand = astUnaryOperator(operand->type,'-',operand);
        return operand;
    }

    cctrlTokenRewind(cc);
    return parsePostFixExpr(cc);
}
