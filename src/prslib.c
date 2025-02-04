#include <assert.h>
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "list.h"
#include "map.h"
#include "prsutil.h"
#include "prslib.h"
#include "util.h"

static AstType *parseArrayDimensionsInternal(Cctrl *cc, AstType *base_type);
Ast *parseSubscriptExpr(Cctrl *cc, Ast *ast);

void parseAssignAuto(Cctrl *cc, Ast *ast) {
    if (ast->declinit == NULL) {
        cctrlRaiseException(cc,"auto must have an initaliser");
    }

    if (ast->declinit->kind == AST_FUNC) {
        ast->declvar->type = astMakeFunctionType(
                ast->declinit->type->rettype,
                ast->declinit->params);
        ast->declvar->type->has_var_args = ast->declinit->type->has_var_args;
        ast->declvar->type->rettype->has_var_args = ast->declinit->type->has_var_args;
    //return astMakeFunctionType(rettype,params);
        return;
        cctrlRaiseException(cc,"auto with functions is not yet supported");
    }
    ast->declvar->type = ast->declinit->type;
}

AstType *parseReturnAuto(Cctrl *cc, Ast *retval) {
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
        case AST_LITERAL:
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
        case TK_SHR: {
            return astTypeCopy(retval->type);
        }
        default:
            cctrlRaiseException(cc,"Could not determine return type for %s()",
                    cc->tmp_fname->data);

    }
}

AstType *parsePointerType(Cctrl *cc, AstType *type) {
    Lexeme *tok;
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
    Lexeme *fname;
    cctrlTokenExpect(cc,'*');
    fname = cctrlTokenGet(cc);
    if (fname->tk_type != TK_IDENT) {
        cctrlRaiseException(cc,"Expected function pointer name got: %s",
                lexemeToString(fname));
    }
    *fnptr_name = fname->start;
    *fnptr_name_len = fname->len;
    cctrlTokenExpect(cc,')');
    cctrlTokenExpect(cc,'(');
    PtrVec *params = parseParams(cc,')',&has_var_args,0);
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
        cctrlRaiseException(cc,"Cannot have a default value of type array");
    }
    Ast *init = parseExpr(cc,16);
    return astFunctionDefaultParam(var,init);
}

PtrVec *parseParams(Cctrl *cc, long terminator, int *has_var_args, int store) {
    PtrVec *params = ptrVecNew();

    Lexeme *tok, *pname;
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
            strMapAdd(cc->localenv,var->argc->lname->data,var->argc);
            strMapAdd(cc->localenv,var->argv->lname->data,var->argv);
            
            ptrVecPush(params, var);

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
                if (!strMapAddOrErr(cc->localenv,var->fname->data,var)) {
                    cctrlRaiseException(cc,"variable %s already declared",
                            astLValueToString(var,0));
                }
                if (cc->tmp_locals) {
                    listAppend(cc->tmp_locals, var);
                }
                ptrVecPush(params, var);

                tok = cctrlTokenGet(cc);
                if (tokenPunctIs(tok, '=')) {
                    Ast *default_fnptr = parseDefaultFunctionParam(cc,var);
                    var->default_fn = default_fnptr;
                    tok = cctrlTokenGet(cc);
                }
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
                ptrVecPush(params, var);

                if (tokenPunctIs(pname, terminator)) {
                    return params;
                }
                continue;
            } else {
                cctrlRaiseException(cc,"Identifier expected, got: %s",
                        lexemeToString(pname));
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
                if (!strMapAddOrErr(cc->localenv,var->declvar->lname->data,var)) {
                    cctrlRaiseException(cc,"variable %s already declared",
                            astLValueToString(var,0));
                }
            }
            tok = cctrlTokenGet(cc);
        } else {
            if (store) {
                if (!strMapAddOrErr(cc->localenv,var->lname->data,var)) {
                        cctrlRaiseException(cc,"variable %s already declared",
                            astLValueToString(var,0));
                }
            }
       }

        if (cc->tmp_locals) {
            listAppend(cc->tmp_locals, var);
        }

        ptrVecPush(params, var);

        if (tokenPunctIs(tok,terminator)) {
            return params;
        }

        if (!tokenPunctIs(tok,',')) {
            cctrlRaiseException(cc,"Comma expected, got %s",
                    lexemeToString(tok));
        }
    }
}

/* Does not parse pointer types, only the base type */
AstType *parseBaseDeclSpec(Cctrl *cc) {
    Lexeme *tok = cctrlTokenGet(cc);
    AstType *type;

    if (!tok) {
        return NULL;
    }

    if (tok->tk_type == TK_KEYWORD || tok->tk_type == TK_IDENT) {
        if ((type = parseGetType(cc, tok)) == NULL) {
            cctrlRewindUntilStrMatch(cc,tok->start,tok->len,NULL);
            cctrlRaiseException(cc,"Type `%.*s` has not been declared, expected type declaration",tok->len,tok->start);
        }
        return type;
    }
    cctrlRewindUntilStrMatch(cc,tok->start,tok->len,NULL);
    cctrlRaiseException(cc,"Declaration or specfier must be an identifier got %s `%.*s`",
            lexemeTypeToString(tok->tk_type),
            tok->len, tok->start);
}

AstType *parseDeclSpec(Cctrl *cc) {
    AstType *type = parseBaseDeclSpec(cc);
    type = parsePointerType(cc,type);
    return type;
}

static AstType *parseArrayDimensionsInternal(Cctrl *cc, AstType *base_type) {
    Ast *size;
    AstType *type = base_type;
    Lexeme *tok, *next_tok;
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
        int ok = 1;
        dimension = evalIntConstExprOrErr(size, &ok);

        if (!ok && (cc->flags & CCTRL_PASTE_DEFINES)) {
            if (size->kind == AST_LVAR) {
                dimension = -3;
                type->clsname = size->lname;
            } else if (astIsArithmetic(size->kind,0)) {
                /* Relent... otherwise this will get far too complicated for a
                 * feature that likely will never be used */
                Ast *left = size->left;
                Ast *right = size->right;
                aoStr *lname = NULL;
                int literal;

                if (left->kind == AST_LVAR && right->kind == AST_LITERAL) {
                    literal = right->i64;
                    lname = left->lname;
                } else if (right->kind == AST_LVAR && left->kind == AST_LITERAL) {
                    literal = left->i64;
                    lname = right->lname;
                } else {
                    goto invalid_subscript;
                }
                
                Lexeme *le = strMapGet(cc->macro_defs,lname->data);
                if (le->tk_type != TK_I64) {
                    goto invalid_subscript;
                }

                dimension = literal + le->i64;
            } else {
                goto invalid_subscript;
            }
        } else if (!ok) {
            goto invalid_subscript;
        }
    }

    cctrlTokenExpect(cc,']');
    AstType *sub_type = parseArrayDimensionsInternal(cc,type);
    if (sub_type) {
        if (sub_type->len == -1 && dimension == -1) {
            cctrlRaiseException(cc,"Array size not specified");
        }
        type = sub_type;
    }

    return astMakeArrayType(type,dimension);

invalid_subscript:
    cctrlRewindUntilPunctMatch(cc, '[',NULL);
    cctrlRaiseExceptionFromTo(cc,"Expected constant integer expression",
            '[',']',"Invalid array subscript");
    return NULL;
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

void parseDeclInternal(Cctrl *cc, Lexeme **tok, AstType **type) {
    AstType *_type = parseDeclSpec(cc);
    Lexeme *_tok = cctrlTokenGet(cc);
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


Ast *findFunctionDecl(Cctrl *cc, char *fname, int len) {
    Ast *decl;
    if ((decl = strMapGetLen(cc->global_env,fname,len)) != NULL) {
        if (decl->kind == AST_FUNC ||
            decl->kind == AST_EXTERN_FUNC ||
            decl->kind == AST_FUN_PROTO) {
            return decl;
        }
    } else if (cc->localenv && (decl = strMapGetLen(cc->localenv,fname,len)) != NULL) {
        if (decl->kind == AST_FUNPTR || decl->kind == AST_LVAR) {
            return decl;
        }
    } else if ((decl = strMapGetLen(cc->asm_funcs,fname,len)) != NULL) {
        /* Assembly function */
        return decl;
    }
    return NULL;
}

PtrVec *parseArgv(Cctrl *cc, Ast *decl, long terminator, char *fname, int len) {
    List *var_args = NULL, *parameter = NULL;
    Ast *ast, *param = NULL;
    AstType *check;
    Lexeme *tok;
    PtrVec *params = NULL;
    int param_idx = 0;

    if (decl) {
        params = decl->params;
    }

    PtrVec *argv_vec = ptrVecNew();

    tok = cctrlTokenPeek(cc);

    while (tok && !tokenPunctIs(tok, terminator)) {
        if (params && vecInBounds(params, param_idx)) {
            param = params->entries[param_idx++];
        }
        ast = parseExpr(cc,16);
        if (ast == NULL) {
            if (param && param->kind == AST_DEFAULT_PARAM) {
                ast = param->declinit;
            }
        }

        if (param != NULL && param->kind == AST_VAR_ARGS) {
            if (decl && decl->kind == AST_EXTERN_FUNC) {
                ptrVecPush(argv_vec,ast);
            } else {
                /* Will merge this to the end of the arguments list */
                if (var_args == NULL) {
                    var_args = listNew();
                }
                listAppend(var_args,ast);
            }
        } else {
            /* Does a distinctly adequate job of type checking function parameters */
            if (param && param->kind != AST_DEFAULT_PARAM) {
                if ((check = astTypeCheck(param->type,ast,'=')) == NULL) {
                    char *expected = astTypeToColorString(param->type);
                    char *got = astTypeToColorString(ast->type);
                    //cctrlRewindUntilStrMatch(cc,tok->start,tok->len,&count);
                    //cctrlTokenPeek(cc);
                    cctrlWarning(cc,"Incompatible function argument, expected '%s' got '%s' function '%.*s'",
                            expected,got,len,fname);
                    //for (int i = 0; i < count; ++i) {
                    //    cctrlTokenGet(cc);
                    //}

                    free(expected);
                    free(got);
                }
            }
            ptrVecPush(argv_vec,ast);
        }

        tok = cctrlTokenGet(cc);

        if (tokenPunctIs(tok, terminator)) {
            break;
        }

        if (!tokenPunctIs(tok,',')) {
            cctrlRewindUntilPunctMatch(cc, tok->i64, NULL);
            cctrlInfo(cc, "Function call `%.*s` not terminated correctly", len,fname);
            cctrlRaiseSuggestion(cc,"terminate with `)`",
                    "Invalid %s `%.*s` while parsing function call, perhaps you meant to terminate the arguments with `)` or keep going with `,`?",
                    lexemeTypeToString(tok->tk_type),
                   tok->len, tok->start);
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
        ptrVecPush(argv_vec,astI64Type(listCount(var_args)));
        listForEach(var_args) {

            ptrVecPush(argv_vec,(Ast*)it->value);
        }
    }

    if (vecEmpty(argv_vec)) {
        cctrlTokenGet(cc);
    }

    return argv_vec;
}

Ast *parseInlineFunctionCall(Cctrl *cc, Ast *fn, PtrVec *argv) {
    List *stmts = listNew();
    List *fn_body = listCopy(fn->body->stms);
    if (fn->params->size == argv->size) {
        int size = fn->params->size;
        for (int i = 0; i < size; ++i) {
            Ast *param = fn->params->entries[i];
            Ast *arg = argv->entries[i];
            Ast *var_decl = astDecl(param,arg);
            listAppend(cc->tmp_func->locals, param);
            listAppend(stmts, var_decl);
        }
    } else {
        cctrlRaiseException(cc,"Cannot presently inline a function with default arguments or where the number of parameters does not match the number of arguments");
    }
    listMergeAppend(stmts, fn_body);
    if (!listEmpty(fn->locals)) {
        List *fn_locals = listCopy(fn->locals);
        listMergeAppend(cc->tmp_func->locals, fn_locals);
    }
    Ast *inlined = astCompountStatement(stmts);
    inlined->inline_ret = fn->inline_ret;
    inlined->type = fn->type->rettype;
    return inlined;
}

/* Read function arguments for a function being called */
Ast *parseFunctionArguments(Cctrl *cc, char *fname, int len, long terminator) {
    AstType *rettype = NULL;
    Ast *maybe_fn = findFunctionDecl(cc,fname,len);
    PtrVec *argv = parseArgv(cc,maybe_fn,terminator,fname,len);

    if (maybe_fn) {
        rettype = maybe_fn->type->rettype;
        if (maybe_fn->flags & AST_FLAG_INLINE && !(cc->flags & CCTRL_TRANSPILING)) {
            return parseInlineFunctionCall(cc, maybe_fn, argv);
        }
    }

    if (!maybe_fn) {
        if ((len == 6 && !strncmp(fname,"printf",6))) {
            rettype = ast_int_type;
            return astFunctionCall(rettype,fname,len,argv);
        }
        /* Walk back untill we fin the missing function */
        Lexeme *peek = cctrlTokenPeek(cc);
        while (peek->len != len && memcmp(fname,peek->start,len) != 0) {
            cctrlTokenRewind(cc);
            peek = cctrlTokenPeek(cc);
        }
        
        cctrlRaiseException(cc,"Function: %.*s() not defined",len,fname);
    }

    switch (maybe_fn->kind) {
        case AST_LVAR:
        case AST_FUNPTR:
            return astFunctionPtrCall(rettype,fname,len,argv,maybe_fn);

        case AST_ASM_FUNCDEF:
        case AST_ASM_FUNC_BIND:
            return astAsmFunctionCall(rettype,
                    aoStrDup(maybe_fn->asmfname),argv);

        case AST_EXTERN_FUNC:
        case AST_FUNC:
        case AST_FUN_PROTO:
            return astFunctionCall(rettype,fname,len,argv);
        default: {
            cctrlRaiseException(cc,"Unknown function: %.*s", len, fname);
        }
    }
}

/* parse either a function call or a variable being used */
static Ast *parseIdentifierOrFunction(Cctrl *cc, 
                                      char *name, 
                                      int len,
                                      int can_call_function)
{
    Ast *ast = NULL;

    Lexeme *tok = cctrlTokenGet(cc);
    Lexeme *peek = cctrlTokenPeek(cc);
    int is_lparen = tokenPunctIs(tok,'(');

    if ((ast = cctrlGetVar(cc, name, len)) == NULL) {
        cctrlRewindUntilStrMatch(cc, name, len, NULL);
        peek = cctrlTokenPeek(cc);
        if (tok->tk_type == TK_PUNCT) {
            switch (tok->i64) {
                case '(': {
                    char *msg = mprintf("Try defining function `%.*s()`?",len,name);
                    cctrlRaiseSuggestion(cc,msg,"Variable or function `%.*s` has not been defined", len, name);
                    free(msg);
                    break;
                }
                case '[': {
                    char *msg = mprintf("Try defining array `I64 %.*s[] = {1, 2, 3}`?",len,name);
                    cctrlRaiseSuggestion(cc,msg,"Variable or function `%.*s` has not been defined", len, name);
                    free(msg);
                    break;
                }
                default:
                    cctrlRaiseException(cc,"Variable or function `%.*s` has not been defined", len, name);
                    break;
            }
        }
    }

    if (!is_lparen) {
        cctrlTokenRewind(cc);
        if (can_call_function) {
            /* Function calls with no arguments are 'Function;' */
            if ((tokenPunctIs(tok,';') || tokenPunctIs(tok,',') || 
                tokenPunctIs(tok,')'))  
                    && parseIsFunction(ast)) {
                if (ast->flags & AST_FLAG_INLINE && !(cc->flags & CCTRL_TRANSPILING)) {
                    return parseInlineFunctionCall(cc,ast,ptrVecNew());
                }
                if (ast->kind == AST_ASM_FUNC_BIND || ast->kind == AST_ASM_FUNCDEF) {
                    return astAsmFunctionCall(ast->type->rettype,
                            aoStrDup(ast->asmfname),ptrVecNew());
                } else {
                    return astFunctionCall(ast->type->rettype,
                            ast->fname->data,ast->fname->len,ptrVecNew());
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
    cctrlTokenRewind(cc);
    return ast;
}

static Ast *parsePrimary(Cctrl *cc) {
    Ast *ast;
    Lexeme *prev,*tok;
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
        if (tokenPunctIs(prev, '&')) {
            if (ast->flags & AST_FLAG_INLINE) {
                Lexeme *peek = cctrlTokenPeek(cc);
                while (peek->tk_type != TK_PUNCT && peek->i64 != '&') {
                    cctrlTokenRewind(cc);
                    peek = cctrlTokenPeek(cc);
                }
                cctrlRaiseException(cc,"Inline functions cannot be used as function pointers: '%.*s'",tok->len, tok->start);
            }
        }
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
        long real_len = 0;
        aoStr *str = aoStrNew();
        cctrlTokenRewind(cc);
        /* Concatinate adjacent strings together */
        while ((tok = cctrlTokenGet(cc)) != NULL && tok->tk_type == TK_STR) {
            aoStrCatFmt(str,"%.*s",tok->len,tok->start);
            real_len += tok->i64-1;
        }
        real_len++;
        cctrlTokenRewind(cc);
        ast = cctrlGetOrSetString(cc, str->data, str->len, real_len);
        free(str);
        return ast;
    }
    case TK_PUNCT:
        cctrlTokenRewind(cc);
        return NULL;
    case TK_EOF:
        cctrlRaiseException(cc,"Unexpected EOF");
        break;
    default:
        cctrlRaiseException(cc,"Unexpected input %s",astKindToString(tok->tk_type));
    }
}

static int parseGetPriority(Lexeme *tok) {
    switch (tok->i64) {
    case TK_ARROW: 
    case '.':
    case '(':
        return 1;

    case '[':
        return 2;

    case '!':
    case '~':
    case TK_PLUS_PLUS: 
    case TK_MINUS_MINUS: 
    case TK_PRE_PLUS_PLUS:
    case TK_PRE_MINUS_MINUS:
        return 3;

    case '*': case '/': case '%':
        return 4;

    case '-':
    case '+':
        return 5;

    case TK_SHL:
    case TK_SHR:
        return 6;

    case TK_LESS_EQU: 
    case TK_GREATER_EQU:
    case '<':
    case '>':
        return 7;

    case TK_EQU_EQU:
    case TK_NOT_EQU:
        return 8;

    case '^': 
        return 9;
    case '&':
        return 10;
    case '|':
        return 11;

    case TK_AND_AND:
        return 12;

    case TK_OR_OR:
        return 13;

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
        return 14;

    default:
        return -1;
    }
}

Ast *parseSubscriptExpr(Cctrl *cc, Ast *ast) {
    Ast *subscript = parseExpr(cc,16);
    if (subscript == NULL) {
        cctrlRewindUntilPunctMatch(cc,'[',NULL);
        Lexeme *tok = cctrlTokenPeek(cc);
        cctrlRaiseException(cc,"Failed to parse subscript value last valid %s was `%.*s`",
                lexemeTypeToString(tok->tk_type),tok->len,tok->start);
    }
    cctrlTokenExpect(cc, ']');
    Ast *binop = parseCreateBinaryOp(cc,'+', ast, subscript);
    return astUnaryOperator(binop->type->ptr, AST_DEREF, binop);
}

Ast *parseGetClassField(Cctrl *cc, Ast *cls) {
    AstType *type;
    if (!parseIsClassOrUnion(cls->type->kind)) {
        char *type_str = astTypeToString(cls->type);
        char *var_str = astLValueToString(cls,0);
        cctrlTokenRewind(cc);
        Lexeme *peek = cctrlTokenPeek(cc);
        char *msg = mprintf("Using `%c` is an invalid operand for `%s %s`",peek->i64,
                type_str,var_str);
        cctrlRaiseSuggestion(cc, msg,
                                "Expected as class however got %s `%s %s`",
                                astTypeKindToHumanReadable(cls->type),
                                type_str,
                                var_str);
        free(msg);
    }

    type = cls->type;

    Lexeme *tok = cctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        cctrlRaiseExceptionFromTo(cc,NULL,'-',*tok->start,"Expected class member got %s `%.*s`",
                            lexemeTypeToString(tok->tk_type),
                            tok->len,
                            tok->start);
    }

    // XXX: This is hacky and only for recusive data types 
    if (type->fields == NULL && cls->kind == AST_DEREF) {
        type = cls->cls->type;
    }
    AstType *field = strMapGetLen(type->fields, tok->start, tok->len);
    if (!field) {
        cctrlRewindUntilStrMatch(cc, tok->start, tok->len, NULL);
        cctrlRaiseException(cc,"Property: %.*s does not exist on class", 
                tok->len, tok->start);
    }
    aoStr *field_name = aoStrDupRaw(tok->start, tok->len);
    return astClassRef(field, cls, aoStrMove(field_name));
}

static int parseCompoundAssign(Lexeme *tok) {
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

Ast *parseCreateBinaryOp(Cctrl *cc, long operation, Ast *left, Ast *right) {
    int is_err = 0;
    Ast *binop = astBinaryOp(operation,left,right,&is_err);
    if (!is_err) {
        return binop;
    } else if (is_err && !(cc->flags & CCTRL_TRANSPILING)) {
        char *opstr = lexemePunctToString(operation);
        const char *left_kind = astTypeKindToHumanReadable(left->type);
        const char *right_kind = astKindToHumanReadable(right);
        const char *right_type_kind = astTypeKindToHumanReadable(right->type);
        cctrlRewindUntilPunctMatch(cc,operation,NULL);
        char *msg = mprintf("`%s %s %s` is invalid",
                            astTypeToString(left->type),
                            opstr,
                            astTypeToString(right->type));
        cctrlRaiseSuggestion(cc,msg,"The `%s` operator cannot be applied to a %s with a %s of %s", opstr, left_kind, right_kind, right_type_kind);
    } else if (cc->flags & CCTRL_TRANSPILING) {
        binop->type = ast_int_type;
    }
    return binop;
    
}

Ast *parseExpr(Cctrl *cc, int prec) {
    Ast *LHS, *RHS;
    Lexeme *tok;
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
            if (!assertLValue(LHS)) {
                char *ast_str = astLValueToString(LHS,0);
                cctrlRaiseException(cc,"Expected an L-Value however got a %s - `%s`",
                        astKindToHumanReadable(LHS), ast_str);
            }
            LHS = astUnaryOperator(LHS->type, tok->i64, LHS);
            continue;
        }

        if (tokenPunctIs(tok,'[')) {
            if (LHS->type->kind != AST_TYPE_POINTER && LHS->type->kind != AST_TYPE_ARRAY) {
                cctrlRewindUntilPunctMatch(cc,'[',NULL);
                cctrlRaiseException(cc,"Cannot subscript a %s, only pointers or arrays can be subscripted with '['",
                                   astTypeKindToHumanReadable(LHS->type));
            }
            LHS = parseSubscriptExpr(cc,LHS);
            continue;
        }

        if (tokenPunctIs(tok, TK_ARROW)) {
            if (LHS->type->kind != AST_TYPE_POINTER) {
                cctrlRaiseException(cc,"Pointer type expected got `%s` `%s`",
                        astTypeToString(LHS->type), astLValueToString(LHS,0));
            }
            LHS = astUnaryOperator(LHS->type->ptr, AST_DEREF, LHS);
            LHS->deref_symbol = TK_ARROW;
            LHS = parseGetClassField(cc, LHS);
            continue;
        }

        compound_assign = parseCompoundAssign(tok);
        if (tokenPunctIs(tok, '=') || compound_assign) {
            if (!assertLValue(LHS)) {
                char *ast_str = astLValueToString(LHS,0);
                cctrlRaiseException(cc,"Expected an L-Value however got a %s - `%s`",
                        astKindToHumanReadable(LHS), ast_str);
            }
        }
        
        next_prec = prec2;
        if (tok->i64 == '=') {
            next_prec++;
        }

        RHS = parseExpr(cc,next_prec);
        if (!RHS) {
            Lexeme *peek = cctrlTokenPeek(cc);
            cctrlTokenRewind(cc);
            char *err_lvar = astLValueToString(LHS,0);
            char *punct_str = lexemePunctToStringWithFlags(tok->i64,0);
            char *msg = mprintf("`%s %s var2` is the expected usage however got `%.*s`",
                    err_lvar,
                    punct_str,peek->len,peek->start);
            cctrlRaiseSuggestion(cc,msg,"Second operand missing to `%s %s` got invalid %s `%.*s`",
                                 err_lvar,
                                 punct_str,
                                 lexemeTypeToString(peek->tk_type),
                                 peek->len, 
                                 peek->start);
            free(msg);
        }

        if (compound_assign) {
            AstType *ok = astTypeCheck(LHS->type,RHS,compound_assign);
            if (!ok) {
                typeCheckWarn(cc,compound_assign,LHS,RHS);
            }
            LHS = parseCreateBinaryOp(cc,'=', LHS, 
                    parseCreateBinaryOp(cc,compound_assign, LHS, RHS));
        } else {
            if (tok->i64 == '=') {
                AstType *ok = astTypeCheck(LHS->type,RHS,'=');
                if (!ok) {
                    typeCheckWarn(cc,'=',LHS,RHS);
                }
            }
            LHS = parseCreateBinaryOp(cc,tok->i64,LHS,RHS);
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

static Ast *parseSizeof(Cctrl *cc) {
    Lexeme *tok = cctrlTokenGet(cc);
    Lexeme *peek = cctrlTokenPeek(cc);
    AstType *type = NULL;
    Ast *ast = NULL;

    if (tokenPunctIs(tok,'(') && cctrlIsKeyword(cc,peek->start,peek->len)) {
        type = parseFullType(cc);
        cctrlTokenExpect(cc,')');
    } else {
        cctrlTokenRewind(cc);
        ast = parseUnaryExpr(cc);
        type = ast->type;
    }

    if (cc->flags & CCTRL_PRESERVE_SIZEOF) {
        return astSizeOf(type);
    }

    /* If we have a string we want the _true_ length of it which as the string
     * is escaped needs to be calculated manually. */
    long size = 0; 
    if (ast && ast->kind == AST_STRING) {
        size = ast->real_len;
    } else {
        /* Otherwise the size is sufficient */
        size = type->size;
    }

    assert(size >= 0);
    return astI64Type(size);
}

Ast *parsePostFixExpr(Cctrl *cc) {
    Ast *ast;
    AstType *type;
    Lexeme *tok,*peek;

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
                PtrVec *argv = parseArgv(cc,ast,')',ast->field,len);
                ast = astFunctionPtrCall(
                        ast->type->rettype,
                        ast->field,
                        len,
                        argv,
                        ast);
                continue;
            }
        }

        if (tokenPunctIs(tok,TK_ARROW)) {
            if (ast->type->kind != AST_TYPE_POINTER) {
                char *type_str = astTypeToString(ast->type);
                char *var_str = astLValueToString(ast,0);
                cctrlRaiseException(cc,"Pointer expected got a %s `%s %s`",
                                        astTypeKindToHumanReadable(ast->type),
                                        type_str, 
                                        var_str);
            }
            ast = astUnaryOperator(ast->type->ptr,AST_DEREF,ast);
            ast->deref_symbol = TK_ARROW;
            ast = parseGetClassField(cc,ast);
            continue;
        }

        if (tokenPunctIs(tok,TK_PLUS_PLUS) || 
            tokenPunctIs(tok,TK_MINUS_MINUS)) {
            if (!assertLValue(ast)) {
                char *ast_str = astLValueToString(ast,0);
                cctrlRaiseException(cc,"Expected an L-Value however got a %s - `%s`",
                        astKindToHumanReadable(ast), ast_str);
            }
            if (tok->i64 == TK_PLUS_PLUS) {
                return astUnaryOperator(ast->type,TK_PLUS_PLUS,ast);
            } else {
                return astUnaryOperator(ast->type,TK_MINUS_MINUS,ast);
            }
        }

        cctrlTokenRewind(cc);
        tok = cctrlTokenPeek(cc);
        return ast;
    }
}

Ast *parseUnaryExpr(Cctrl *cc) {
    Lexeme *tok;
    Ast *ast;

    if ((tok = cctrlTokenGet(cc)) == NULL) {
        cctrlRaiseException(cc,"Unexpected end of input");
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
            default: {
                cctrlTokenRewind(cc);
                cctrlRaiseException(cc,"Unexpected keyword: %.*s while parsing unary expression",
                        tok->len,tok->start);
            }
        }
    }

    if (tokenPunctIs(tok,'(')) {
        ast = parseExpr(cc,16);
        cctrlTokenExpect(cc,')');
        return ast;
    }

    if (tok->tk_type == TK_PUNCT) {
        long unary_op = -1;
        switch (tok->i64) {
            case '&': { unary_op = AST_ADDR; break; }
            case '*': { unary_op = AST_DEREF; break; }
            case '~': { unary_op = '~'; break; }
            case '!': { unary_op = '!'; break; }
            case '-': { unary_op = '-'; break; }
            case TK_PLUS_PLUS: { unary_op = TK_PRE_PLUS_PLUS; break; }
            case TK_MINUS_MINUS: { unary_op = TK_PRE_MINUS_MINUS; break; }
            default: {
                cctrlTokenRewind(cc);
                return parsePostFixExpr(cc);
            }
        }

        Lexeme *peek = cctrlTokenPeekBy(cc,1);
        Ast *operand = NULL;
        AstType *type = NULL;

        /* XXX: This feels wrong but allows things like:
         * !arr[0][1][2] to work properly */
        if (tokenPunctIs(peek, '[') && !(unary_op == AST_ADDR || unary_op == AST_DEREF)) {
            operand = parseExpr(cc,16);
        } else {
            operand = parseUnaryExpr(cc);
            peek = cctrlTokenPeek(cc);
            if (tokenPunctIs(peek, '[') && (operand->kind == AST_CLASS_REF || operand->type->kind == AST_TYPE_ARRAY)) {
                cctrlTokenGet(cc);
                operand = parseSubscriptExpr(cc, operand);
            }
        }


        if (unary_op == AST_ADDR && parseIsFunction(operand)) {
            return operand;
        }

        switch (unary_op) {
            case AST_ADDR:  type = astMakePointerType(operand->type); break;
            case AST_DEREF: type = operand->type->ptr; break;
            case '~':       type = ast_int_type; break;
            default:        type = operand->type; break;
        }

        return astUnaryOperator(type, unary_op, operand);
    }
    
    cctrlTokenRewind(cc);
    return parsePostFixExpr(cc);
}
