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
        case AST_LITERAL:
        case AST_BINOP: {
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
                                  char **fnptr_name,
                                  int *fnptr_name_len,
                                  AstType *rettype)
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
    Vec *params = parseParams(cc,')',&has_var_args,0);
    return astMakeFunctionType(rettype, params);
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

    Vec *params = fnptr_type->params;
    //fnptr_type = astMakeFunctionType(fnptr_type, params);

    ast = astFunctionPtr(
            fnptr_type,
            fnptr_name,
            fnptr_name_len,
            params);
    return ast;
}

Ast *parseDefaultFunctionParam(Cctrl *cc, Ast *var) {
    if (var->type->kind == AST_TYPE_ARRAY) {
        cctrlRaiseException(cc,"Cannot have a default value of type array");
    }
    Ast *init = parseExpr(cc,16);
    return astFunctionDefaultParam(var,init);
}

Vec *parseParams(Cctrl *cc, s64 terminator, int *has_var_args, int store) {
    Vec *params = astVecNew();

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
            mapAdd(cc->localenv,var->argc->lname->data,var->argc);
            mapAdd(cc->localenv,var->argv->lname->data,var->argv);
            
            vecPush(params, var);

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
        int pinned_kind;
        AoStr *pinned_reg;
        parseRegModifier(cc, &pinned_kind, &pinned_reg);
        pname = cctrlTokenGet(cc);
        if (pname == NULL) {
            cctrlRaiseException(cc,
                    "Unexpected end of input in parameter list");
        }

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
                var = parseFunctionPointer(cc, type);
                if (!mapAddOrErr(cc->localenv,var->fname->data,var)) {
                    cctrlRaiseException(cc,"variable %s already declared",
                            astLValueToString(var,0));
                }
                if (cc->tmp_locals) {
                    listAppend(cc->tmp_locals, var);
                }

                tok = cctrlTokenGet(cc);
                if (tokenPunctIs(tok, '=')) {
                    Ast *default_fnptr = parseDefaultFunctionParam(cc,var);
                    var->default_fn = default_fnptr;
                    vecPush(params, default_fnptr);
                    tok = cctrlTokenGet(cc);
                } else {
                    vecPush(params, var);
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
                /* Anonymous param can still carry a `reg <REG>` modifier
                 * (TempleOS-style `Foo(I64 reg X19, I64 reg X20)` -
                 * names omitted, register IS the identity). */
                var->pinned_kind = pinned_kind;
                var->pinned_reg = pinned_reg;
                if (cc->tmp_locals) {
                    listAppend(cc->tmp_locals, var);
                }
                vecPush(params, var);

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
        /* Anchor the param to its name token (astNew stamped whatever
         * token the parser had just consumed). */
        var->line = pname->line;
        var->col = pname->col;
        var->pinned_kind = pinned_kind;
        var->pinned_reg = pinned_reg;

        tok = cctrlTokenGet(cc);
        if (tokenPunctIs(tok, '=')) {
            var = parseDefaultFunctionParam(cc,var);
            if (store) {
                if (!mapAddOrErr(cc->localenv,var->declvar->lname->data,var)) {
                    cctrlRaiseException(cc,"variable %s already declared",
                            astLValueToString(var,0));
                }
            }
            tok = cctrlTokenGet(cc);
        } else {
            if (store) {
                if (!mapAddOrErr(cc->localenv,var->lname->data,var)) {
                        cctrlRaiseException(cc,"variable %s already declared",
                            astLValueToString(var,0));
                }
            }
       }

        if (cc->tmp_locals) {
            listAppend(cc->tmp_locals, var);
        }

        vecPush(params, var);

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
    /* NULL means end-of-input (parseBaseDeclSpec's guard) - no caller
     * can make progress with a missing type, and several deref it. */
    if (type == NULL) {
        cctrlRaiseException(cc,
                "Unexpected end of input, expected a type");
    }
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
            } else if (astIsArithmetic(size,0)) {
                /* Relent... otherwise this will get far too complicated for a
                 * feature that likely will never be used */
                Ast *left = size->left;
                Ast *right = size->right;
                AoStr *lname = NULL;
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
                
                Lexeme *le = mapGet(cc->macro_defs,lname->data);
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
    if ((decl = mapGetLen(cc->global_env,fname,len)) != NULL) {
        if (decl->kind == AST_FUNC ||
            decl->kind == AST_EXTERN_FUNC ||
            decl->kind == AST_FUN_PROTO || decl->kind == AST_ASM_FUNC_BIND) {
            return decl;
        }
    } else if (cc->localenv && (decl = mapGetLen(cc->localenv,fname,len)) != NULL) {
        if (decl->kind == AST_FUNPTR || decl->kind == AST_LVAR) {
            return decl;
        }
    } else if ((decl = mapGetLen(cc->asm_funcs,fname,len)) != NULL) {
        /* Assembly function */
        return decl;
    }
    return NULL;
}

static Vec *parseGetFunctionParams(Ast *def) {
    if (!def) return NULL;

    /* Function-typed references store params on their type; declarations and
     * function pointers store them directly on the AST. */
    if (def->kind == AST_CLASS_REF || def->kind == AST_LVAR || def->kind == AST_GVAR)
        return def->type && def->type->kind == AST_TYPE_FUNC ? def->type->params : NULL;
    return def->params;
}

/* We use this to check function calls against their definitions reusably */
static void parseFunctionArgumentCheck(Cctrl *cc, Ast *def, Vec *argv, char *fname, int len)
{
    Vec *params = parseGetFunctionParams(def);
    int param_count = params ? (int)params->size : 0;
    int arg_count = argv ? (int)argv->size : 0;
    int default_count = 0, required_count = 0, fixed_param_count = param_count;

    /* count fixed/default/required params and find the varargs point if it exists */
    for (int i = 0; i < param_count; ++i) {
        Ast *param = params->entries[i];
        if (param->kind == AST_VAR_ARGS) {
            fixed_param_count = i;
            break;
        }

        if (param->kind == AST_DEFAULT_PARAM) default_count++;
        else required_count++;
    }

    /* validate each fixed parameter against its call argument */
    for (int i = 0; i < fixed_param_count; ++i) {
        Ast *param = params->entries[i];
        Ast *arg = i < arg_count ? argv->entries[i] : NULL;

        if (!arg || arg->kind == AST_PLACEHOLDER) {
            if (param->kind == AST_DEFAULT_PARAM) continue;

            if (default_count) {
                cctrlRaiseExceptionFromTo(cc, NULL, '(', ')',
                        "Unexpected number of arguments %d in call to %.*s(), argument %d is required",
                        arg_count, len, fname, i + 1);
            }

            cctrlRaiseExceptionFromTo(cc, NULL, '(', ')',
                    "Unexpected number of arguments %d in call to %.*s(), expected %d args",
                    arg_count, len, fname, fixed_param_count);
        }

        AstType *type = param->kind == AST_DEFAULT_PARAM ? param->declvar->type : param->type;
        if (type && astTypeCheck(type,arg,AST_BIN_OP_ASSIGN) == NULL) {
            char *expected = astTypeToColorString(type);
            char *got = astTypeToColorString(arg->type);
            cctrlWarning(cc,"Incompatible function argument, expected '%s' got '%s' in function '%.*s'",
                    expected,got,len,fname);
        }
    }

    /* vararg calls allow extras, but still require fixed args */
    if (fixed_param_count != param_count) {
        if (arg_count < required_count) {
            cctrlRaiseExceptionFromTo(cc, NULL, '(', ')',
                    "Unexpected number of arguments %d in call to %.*s(), expected at least %d args",
                    arg_count, len, fname, required_count);
        }
        return;
    }

    /* non-vararg calls cannot use more than the fixed parameter count */
    if (arg_count > fixed_param_count) {
        if (default_count) {
            cctrlRaiseExceptionFromTo(cc, NULL, '(', ')',
                    "Unexpected number of arguments %d in call to %.*s(), expected args in range %d to %d",
                    arg_count, len, fname, required_count, fixed_param_count);
        }

        cctrlRaiseExceptionFromTo(cc, NULL, '(', ')',
                "Unexpected number of arguments %d in call to %.*s(), expected %d args",
                arg_count, len, fname, fixed_param_count);
    }
}

/* Materialize default values into `argv` so every fixed-parameter position
 * holds a real expression. Mutates in place. Run after
 * parseFunctionArgumentCheck has validated counts/types, so any still-missing
 * position past this point is one the check let through (e.g. inline-only
 * mismatches) and we silently leave alone rather than splice in NULL. */
static void parseFlattenDefaultArgs(Ast *def, Vec *argv) {
    if (!def || !argv) {
        return;
    }
    Vec *params = parseGetFunctionParams(def);
    if (!params)
        return;

    u64 param_count = params->size;
    for (u64 i = 0; i < param_count; ++i) {
        Ast *param = (Ast *)params->entries[i];
        if (!param)
            continue;
        if (param->kind == AST_VAR_ARGS)
            break;

        Ast *def_ast = NULL;
        if (param->kind == AST_DEFAULT_PARAM) {
            def_ast = param->declinit;
        } else if (param->kind == AST_FUNPTR && param->default_fn) {
            def_ast = param->default_fn->declinit;
        }

        int in_bounds = i < argv->size;
        Ast *cur = in_bounds ? (Ast *)argv->entries[i] : NULL;
        if (in_bounds && cur && cur->kind != AST_PLACEHOLDER)
            continue;
        if (!def_ast)
            continue;

        if (in_bounds) argv->entries[i] = def_ast;
        else           vecPush(argv, def_ast);
    }
}

/* Wrap any fixed-parameter argument that crosses the int/float boundary
 * in a cast, so codegen converts the value (sitofp/fptosi) instead of
 * reinterpreting the raw bits. C does this implicitly for prototyped
 * params - e.g. `Twice(3)` where `Twice` takes an F64 must pass 3.0.
 * Runs after parseFlattenDefaultArgs so filled-in default values get
 * coerced too. Variadic extras are left alone (no declared type). */
/* Promote F32 arguments in variadic positions (index >= from_idx) to
 * F64. C's default argument promotions widen a float to a double when
 * passed through `...`, and both the C-extern and HolyC printf read the
 * slot as 8 bytes - without this an F32 arg prints as garbage. */
static void parsePromoteVarargFloats(Vec *argv, u64 from_idx) {
    if (!argv)
        return;
    for (u64 i = from_idx; i < argv->size; ++i) {
        Ast *arg = (Ast *)argv->entries[i];
        if (!arg || arg->kind == AST_PLACEHOLDER || !arg->type)
            continue;
        if (astIsFloatType(arg->type) && arg->type->size < 8) {
            argv->entries[i] = astCast(arg, ast_float_type);
        }
    }
}

static void parseCoerceArgs(Ast *def, Vec *argv) {
    Vec *params = parseGetFunctionParams(def);
    if (!params || !argv)
        return;

    u64 param_count = params->size;
    for (u64 i = 0; i < param_count && i < argv->size; ++i) {
        Ast *param = (Ast *)params->entries[i];
        if (!param)
            continue;
        if (param->kind == AST_VAR_ARGS) {
            /* Everything from here on is variadic: widen F32 -> F64. */
            parsePromoteVarargFloats(argv, i);
            break;
        }

        Ast *arg = (Ast *)argv->entries[i];
        if (!arg || arg->kind == AST_PLACEHOLDER || !arg->type)
            continue;

        AstType *type = param->kind == AST_DEFAULT_PARAM
                      ? param->declvar->type : param->type;
        if (!type)
            continue;

        int want_float = astIsFloatType(type);
        int want_int   = astIsIntType(type);
        int got_float  = astIsFloatType(arg->type);
        int got_int    = astIsIntType(arg->type);
        if ((want_float && got_int) || (want_int && got_float)) {
            argv->entries[i] = astCast(arg, type);
        } else if (want_float && got_float &&
                   type->size != arg->type->size) {
            /* float <-> float of a different width (e.g. passing an
             * F32 where an F64 param is declared): convert, don't
             * reinterpret the bits. */
            argv->entries[i] = astCast(arg, type);
        }
    }
}

Vec *parseArgv(Cctrl *cc, Ast *decl, s64 terminator, char *fname, int len) {
    List *var_args = NULL;
    Ast *ast, *param = NULL;
    Lexeme *tok;
    Vec *params = NULL;
    int param_idx = 0;

    if (decl) params = parseGetFunctionParams(decl);

    Vec *argv_vec = astVecNew();

    tok = cctrlTokenPeek(cc);

    while (tok && !tokenPunctIs(tok, terminator)) {
        /* For a function with varadic arguments this will never be in bounds */
        int is_in_bounds = params && vecInBounds(params, (unsigned long)param_idx);
        if (is_in_bounds) {
            param = params->entries[param_idx++];
        }

        if (tokenPunctIs(tok, ',')) {
            ast = NULL;
            if (param && param->kind == AST_DEFAULT_PARAM) {
                ast = param->declinit;
            } else if (param && param->kind == AST_FUNPTR) {
                if (param->default_fn != NULL) {
                    ast = param->default_fn->declinit;
                }
            }
            if (ast == NULL) {
                ast = astMakePlaceHolder();
            }

            vecPush(argv_vec, ast);
            cctrlTokenGet(cc);
            tok = cctrlTokenPeek(cc);
            continue;
        }

        ast = parseExpr(cc,16);
        if (ast == NULL) {
            if (param && param->kind == AST_DEFAULT_PARAM) {
                ast = param->declinit;
            }
        }

        if (param != NULL && param->kind == AST_VAR_ARGS) {
            if (decl && decl->kind == AST_EXTERN_FUNC) {
                vecPush(argv_vec,ast);
            } else {
                /* Will merge this to the end of the arguments list */
                if (var_args == NULL) {
                    var_args = listNew();
                }
                listAppend(var_args,ast);
            }
        } else {
            vecPush(argv_vec,ast);
        }

        tok = cctrlTokenGet(cc);

        if (tokenPunctIs(tok, terminator)) {
            break;
        }

        if (tok == NULL) {
            cctrlRaiseException(cc,
                    "Unexpected end of input in call to `%.*s` "
                    "(unterminated `( ... )`?)", len, fname);
        }
        if (!tokenPunctIs(tok,',')) {
            cctrlRewindUntilPunctMatch(cc, tok->i64, NULL);
            /* We could have a malformed string as a function argument */
            int is_string = tok->tk_type == TK_STR;
            char terminator = is_string ? '"' : ')';
            char *suggestion = is_string ?
                                "terminate with `\"`" :
                                "terminate with `)`";
            cctrlInfo(cc, "Function call `%.*s` not terminated correctly", len,fname);
            cctrlRaiseSuggestion(cc,suggestion,
                    "Invalid %s `%.*s` while parsing function call, perhaps you meant to terminate the arguments with `%c` or keep going with `,`?",
                    lexemeTypeToString(tok->tk_type),
                   tok->len, tok->start, terminator);
        }

        tok = cctrlTokenPeek(cc);
    }

    if (var_args) {
        /* set the argument count and merge lists */
        vecPush(argv_vec,astI64Type(listCount(var_args)));
        listForEach(var_args) {
            vecPush(argv_vec,(Ast*)it->value);
        }
    }

    if (vecEmpty(argv_vec)) {
        cctrlTokenGet(cc);
    }

    return argv_vec;
}

Ast *parseInlineFunctionCall(Cctrl *cc, Ast *fn, Vec *argv) {
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

/* Count printf-style conversion specifiers in a format-string literal. Skips
 * `%%`; each `%...c` (where c is a conversion char) counts as one expected
 * argument, plus one extra for each `*` used as a width or precision. Returns
 * -1 if a trailing/malformed spec is encountered so the caller can skip the
 *  warning rather than report a confusing count. */
static int parseCountPrintfFormatSpecs(const char *fmt, int len) {
    int n = 0;
    int i = 0;
    while (i < len) {
        if (fmt[i] != '%') {
            i++;
            continue;
        }
        if (i + 1 < len && fmt[i+1] == '%') {
            i += 2;
            continue;
        }
        i++; /* past `%` */
        while (i < len) {
            char ch = fmt[i];
            if (ch == '*') n++;
            if (ch == 'd' || ch == 'i' || ch == 'u' || ch == 'o' ||
                ch == 'x' || ch == 'X' || ch == 'e' || ch == 'E' ||
                ch == 'f' || ch == 'F' || ch == 'g' || ch == 'G' ||
                ch == 'c' || ch == 's' || ch == 'p' || ch == 'n' ||
                ch == 'a' || ch == 'A')
            {
                n++;
                i++;
                goto next_spec;
            }
            i++;
        }
        return -1; /* `%` with no terminating conversion */
next_spec: ;
    }
    return n;
}

/* Position of the format string within a known printf-style call's argument
 * list. -1 means this isn't one of the format-checked functions. Single
 * source of truth shared by the pre-parse position snapshot and the
 * post-parse arg-count check. */
static int parseFormatArgIdx(char *fname, int len) {
    /* Feels like we may want to occasionally add to this list? */
    if      (len == 6 && !strncmp(fname, str_lit("printf"))) return 0;
    else if (len == 7 && !strncmp(fname, str_lit("sprintf"))) return 0;
    else if (len == 8 && !strncmp(fname, str_lit("snprintf"))) return 0;
    /* May as well type-check holyc's library too which is even more
     * prone to erroring. */
    else if (len == 9 && !strncmp(fname, str_lit("MStrPrint"))) return 0;
    else if (len == 8 && !strncmp(fname, str_lit("StrPrint"))) return 1;
    else if (len == 8 && !strncmp(fname, str_lit("CatPrint"))) return 1;
    else if (len == 11 && !strncmp(fname, str_lit("CatLenPrint"))) return 2;
    return -1;
}

/* Walk the lookahead buffer past the leading args up to position
 * `fmt_arg_idx`, returning that arg's first token. Used pre-parse to
 * snapshot the format-literal's source position so the format-check
 * diagnostic can underline it - by the time the check runs, parseArgv
 * has long since moved the buffer head. Balances `( [ {` so calls
 * like `StrPrint(getBuf(), "fmt")` don't trip on inner commas. */
static Lexeme *parsePeekArgAt(Cctrl *cc, int fmt_arg_idx) {
    int depth = 0;
    int args_seen = 0;
    for (int off = 0; off <= 64; off++) {
        /* off=0 is the current peek (first not-yet-consumed token);
         * cctrlTokenPeekBy(cc, n) returns the n-th token AFTER that,
         * so we have to dispatch on the two helpers explicitly. */
        Lexeme *tok = off == 0 ? cctrlTokenPeek(cc)
                               : cctrlTokenPeekBy(cc, off);
        if (!tok) return NULL;
        if (tok->tk_type == TK_PUNCT) {
            if (tok->i64 == '(' || tok->i64 == '[' || tok->i64 == '{') {
                depth++;
                continue;
            }
            if (tok->i64 == ')' || tok->i64 == ']' || tok->i64 == '}') {
                if (depth == 0) return NULL;
                depth--;
                continue;
            }
            if (tok->i64 == ',' && depth == 0) {
                args_seen++;
                continue;
            }
        }
        if (args_seen == fmt_arg_idx) return tok;
    }
    return NULL;
}

/* Raise an error when a printf-style call's format-string literal disagrees
 * with the supplied argument count. Very easy footgun. `"%d\n";`
 * Only fires when argv[0] is a real string literal so we stay silent on
 * `printf(fmt, ...)` where the `fmt` is dynamic. */
static void parsePrintfFormatCheck(Cctrl *cc,
                                   Ast *maybe_fn,
                                   Vec *argv,
                                   char *fname,
                                   int len,
                                   int fmt_line,
                                   int fmt_col,
                                   int fmt_len)
{
    if (!argv || argv->size == 0)
        return;

    int fmt_arg_idx = parseFormatArgIdx(fname, len);
    if (fmt_arg_idx == -1) return;

    Ast *fmt_ast = argv->entries[fmt_arg_idx];
    if (!fmt_ast || fmt_ast->kind != AST_STRING || !fmt_ast->sval)
        return;

    int expected = parseCountPrintfFormatSpecs(fmt_ast->sval->data,
            fmt_ast->sval->len);
    if (expected < 0)
        return;

    /* Number of arguments the user actually wrote after the format
     * string. parseArgv stashes everything from the fixed-param slot
     * onwards behind a synthetic count prefix for HolyC-native
     * varargs (printf-the-extern is C-style and has no prefix), so
     * we subtract one extra slot in that case to land on the
     * caller-visible count. */
    int supplied = (int)argv->size - fmt_arg_idx - 1;
    int holyc_vararg = maybe_fn && maybe_fn->type &&
                       maybe_fn->type->has_var_args &&
                       maybe_fn->kind != AST_EXTERN_FUNC;
    if (holyc_vararg && supplied > 0) supplied -= 1;
    if (supplied == expected)
        return;

    char *raw = mprintf(
        "printf format expects %d argument%s but %d %s supplied",
        expected, expected == 1 ? "" : "s",
        supplied, supplied == 1 ? "was" : "were");
    AoStr *bold = aoStrNew();
    aoStrCatColoured(bold, ESC_BOLD, raw);
    AoStr *buf = cctrlCreateErrorLineAt(cc, fmt_line, fmt_col, fmt_len,
                                        bold->data, CCTRL_ERROR, NULL);
    aoStrRelease(bold);
    CctrlDiagnostic *d = cctrlMakeDiag(cc, CCTRL_ERROR, buf, NULL);
    cctrlDiagPush(cc, d);
    cctrlTerminate(cc);
}

/* Read function arguments for a function being called */
Ast *parseFunctionArguments(Cctrl *cc, char *fname, int len, s64 terminator) {
    AstType *rettype = NULL;
    Ast *maybe_fn = findFunctionDecl(cc,fname,len);

    /* Snapshot the format-string token's position before parseArgv
     * consumes it, so the format-check below can underline the
     * literal rather than wherever the buffer head landed after
     * parsing all the args. Handles format-arg-at-idx>0 callees
     * (StrPrint, CatPrint, CatLenPrint) by walking past the leading
     * positional args via parsePeekArgAt. */
    int fmt_line = 0, fmt_col = 0, fmt_len = 0;
    int fmt_arg_idx = parseFormatArgIdx(fname, len);
    if (fmt_arg_idx != -1) {
        Lexeme *peek = parsePeekArgAt(cc, fmt_arg_idx);
        if (peek && peek->tk_type == TK_STR) {
            fmt_line = peek->line;
            fmt_col = peek->col;
            /* peek->len is the inner content (escapes kept as 2-char
             * pairs); +2 to cover the surrounding quotes. */
            fmt_len = peek->len + 2;
        }
    }

    Vec *argv = parseArgv(cc,maybe_fn,terminator,fname,len);

    if (maybe_fn) {
        parseFunctionArgumentCheck(cc,maybe_fn,argv,fname,len);
        parseFlattenDefaultArgs(maybe_fn, argv);
        parseCoerceArgs(maybe_fn, argv);
        parsePrintfFormatCheck(cc, maybe_fn, argv, fname, len,
                               fmt_line, fmt_col, fmt_len);


        rettype = maybe_fn->type->rettype;
        if (maybe_fn->flags & AST_FLAG_INLINE && !(cc->flags & CCTRL_TRANSPILING)) {
            if (maybe_fn->kind == AST_ASM_FUNC_BIND || maybe_fn->kind == AST_ASM_FUNCDEF) {
                Ast *call = astAsmFunctionCall(rettype,
                                               aoStrDup(maybe_fn->asmfname),
                                               astVecNew());
                call->flags |= maybe_fn->flags;
                return call;
            }
            return parseInlineFunctionCall(cc, maybe_fn, argv);
        }
    }

    if (!maybe_fn) {
        if ((len == 6 && !strncmp(fname,"printf",6))) {
            rettype = ast_int_type;
            /* Implicit C printf(fmt, ...): widen F32 varargs to F64. */
            parsePromoteVarargFloats(argv, 1);
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
        if (tok == NULL) {
            /* Identifier at end of input - nothing to suggest from. */
            cctrlRaiseException(cc,
                    "Variable or function `%.*s` has not been defined",
                    len, name);
        }
        if (tok->tk_type == TK_PUNCT) {
            switch (tok->i64) {
                case '(': {
                    char *msg = mprintf("Try defining function `%.*s()`?",len,name);
                    cctrlRaiseSuggestion(cc,msg,"Variable or function `%.*s` has not been defined", len, name);
                    break;
                }
                case '[': {
                    char *msg = mprintf("Try defining array `I64 %.*s[] = {1, 2, 3}`?",len,name);
                    cctrlRaiseSuggestion(cc,msg,"Variable or function `%.*s` has not been defined", len, name);
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
                Vec *argv = astVecNew();
                parseFunctionArgumentCheck(cc,ast,argv,ast->fname->data,ast->fname->len);
                parseFlattenDefaultArgs(ast, argv);
                if (ast->flags & AST_FLAG_INLINE && !(cc->flags & CCTRL_TRANSPILING)) {
                    if (ast->kind == AST_ASM_FUNC_BIND || ast->kind == AST_ASM_FUNCDEF) {
                        Ast *call = astAsmFunctionCall(ast->type->rettype, aoStrDup(ast->asmfname), argv);
                        call->flags |= ast->flags;
                        return call;
                    }
                    return parseInlineFunctionCall(cc,ast,argv);
                }
                if (ast->kind == AST_ASM_FUNC_BIND || ast->kind == AST_ASM_FUNCDEF) {
                    return astAsmFunctionCall(ast->type->rettype,
                            aoStrDup(ast->asmfname),argv);
                } else {
                    return astFunctionCall(ast->type->rettype,
                            ast->fname->data,ast->fname->len,argv);
                }
            }
        }
        return ast;
    }

    /* Is a function call if the next char is '(' and the peek is not a
     * type. peek == NULL (end of input) counts as a call attempt - the
     * argument parser raises the unterminated-`(` diagnostic. */
    if (peek == NULL || !cctrlIsKeyword(cc,peek->start,peek->len)) {
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
        s64 real_len = 0;
        AoStr *str = aoStrNew();
        cctrlTokenRewind(cc);
        /* Concatinate adjacent strings together */
        while ((tok = cctrlTokenGet(cc)) != NULL && tok->tk_type == TK_STR) {
            aoStrCatFmt(str,"%.*s",tok->len,tok->start);
            real_len += tok->i64-1;
        }
        real_len++;
        cctrlTokenRewind(cc);
        ast = cctrlGetOrSetString(cc, str->data, str->len, real_len);
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
    Ast *binop = parseCreateBinaryOp(cc, AST_BIN_OP_ADD, ast, subscript);
    //binop->type = ast->type;
    return astUnaryOperator(binop->type->ptr, AST_UN_OP_DEREF, binop);
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
    }

    type = cls->type;

    Lexeme *tok = cctrlTokenGet(cc);
    if (tok == NULL) {
        cctrlRaiseException(cc,
                "Unexpected end of input after '->' or '.'");
    }
    if (tok->tk_type != TK_IDENT) {
        cctrlRaiseExceptionFromTo(cc,NULL,'-',*tok->start,"Expected class member got %s `%.*s`",
                            lexemeTypeToString(tok->tk_type),
                            tok->len,
                            tok->start);
    }

    // XXX: This is hacky and only for recusive data types 
    if (type->fields == NULL && astIsDeref(cls)) {
        type = cls->operand->type;
    }
    AstType *field = mapGetLen(type->fields, tok->start, tok->len);
    if (!field) {
        cctrlRewindUntilStrMatch(cc, tok->start, tok->len, NULL);
        cctrlRaiseException(cc,"Property: %.*s does not exist on class", 
                tok->len, tok->start);
    }

    AoStr *field_name = aoStrDupRaw(tok->start, tok->len);
    Ast *class_ref = astClassRef(field, cls, aoStrMove(field_name));
    return class_ref;
}

static int parseCompoundAssign(Lexeme *tok, AstBinOp *_op) {
    if (tok->tk_type != TK_PUNCT) {
        return 0;
    }

    switch (tok->i64) {
        case TK_ADD_EQU: *_op = AST_BIN_OP_ADD; return 1;
        case TK_SUB_EQU: *_op = AST_BIN_OP_SUB; return 1;
        case TK_MUL_EQU: *_op = AST_BIN_OP_MUL; return 1;
        case TK_DIV_EQU: *_op = AST_BIN_OP_DIV; return 1;
        case TK_MOD_EQU: *_op = AST_BIN_OP_MOD; return 1;
        case TK_AND_EQU: *_op = AST_BIN_OP_BIT_AND; return 1;
        case TK_OR_EQU:  *_op = AST_BIN_OP_BIT_OR; return 1;
        case TK_XOR_EQU: *_op = AST_BIN_OP_BIT_XOR; return 1;
        case TK_SHL_EQU: *_op = AST_BIN_OP_SHL; return 1;
        case TK_SHR_EQU: *_op = AST_BIN_OP_SHR; return 1;
        default: return 0;
    }

    return 0;
}

/* Inverse of astBinOpFromToken. Returns the source token punct value
 * (e.g. '<', TK_SHL) for an AstBinOp. */
static s64 astBinOpToTokenPunct(AstBinOp op) {
    switch (op) {
        case AST_BIN_OP_ADD:     return '+';
        case AST_BIN_OP_SUB:     return '-';
        case AST_BIN_OP_MUL:     return '*';
        case AST_BIN_OP_DIV:     return '/';
        case AST_BIN_OP_MOD:     return '%';
        case AST_BIN_OP_ASSIGN:  return '=';
        case AST_BIN_OP_LT:      return '<';
        case AST_BIN_OP_GT:      return '>';
        case AST_BIN_OP_LE:      return TK_LESS_EQU;
        case AST_BIN_OP_GE:      return TK_GREATER_EQU;
        case AST_BIN_OP_EQ:      return TK_EQU_EQU;
        case AST_BIN_OP_NE:      return TK_NOT_EQU;
        case AST_BIN_OP_LOG_AND: return TK_AND_AND;
        case AST_BIN_OP_LOG_OR:  return TK_OR_OR;
        case AST_BIN_OP_BIT_XOR: return '^';
        case AST_BIN_OP_BIT_AND: return '&';
        case AST_BIN_OP_BIT_OR:  return '|';
        case AST_BIN_OP_SHL:     return TK_SHL;
        case AST_BIN_OP_SHR:     return TK_SHR;
        default:                 return 0;
    }
}

Ast *parseCreateBinaryOp(Cctrl *cc, AstBinOp operation, Ast *left, Ast *right) {
    int is_err = 0;
    Ast *binop = astBinaryOp(operation,left,right,&is_err);
    if (!is_err) {
        return binop;
    } else if (is_err && !(cc->flags & CCTRL_TRANSPILING)) {
        const char *opstr = astBinOpKindToString(operation);
        const char *left_kind = astTypeKindToHumanReadable(left->type);
        const char *right_kind = astKindToHumanReadable(right);
        const char *right_type_kind = astTypeKindToHumanReadable(right->type);
        s64 op_tok = astBinOpToTokenPunct(operation);
        if (op_tok) {
            cctrlRewindUntilPunctMatch(cc, op_tok, NULL);
        }
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
            AstUnOp op;
            if (!astUnaryOpFromToken(tok->i64, &op)) {
                /* This should be impossible to hit as we've already pre-validated */
                loggerPanic("Invalid token for unary operator; %c\n", (char)tok->i64);
            }
            LHS = astUnaryOperator(LHS->type, op, LHS);
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
            LHS = astUnaryOperator(LHS->type->ptr, AST_UN_OP_DEREF, LHS);
            LHS->deref_symbol = TK_ARROW;
            LHS = parseGetClassField(cc, LHS);
            continue;
        }

        AstBinOp deconstructed_compound_op;
        compound_assign = parseCompoundAssign(tok, &deconstructed_compound_op);
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
        }

        /* This de-sugars the compound assign which I think is okay */
        if (compound_assign) {
            AstType *ok = astTypeCheck(LHS->type,RHS,compound_assign);
            if (!ok) {
                typeCheckWarn(cc,'=',LHS,RHS);
            }
            LHS = parseCreateBinaryOp(cc,AST_BIN_OP_ASSIGN, LHS,
                    parseCreateBinaryOp(cc, deconstructed_compound_op, LHS, RHS));
        } else {
            if (tok->i64 == '=') {
                AstType *ok = astTypeCheck(LHS->type,RHS,AST_BIN_OP_ASSIGN);
                if (!ok) {
                    typeCheckWarn(cc,'=',LHS,RHS);
                }
            }
            AstBinOp binop;
            if (!astBinOpFromToken(tok->i64,&binop)) {
                cctrlRaiseException(cc,"Invalid binary operator %c => %d", (char)tok->i64, tok->i64);
            }
            if (!LHS) {
                lexemePrint(tok);
            }

            LHS = parseCreateBinaryOp(cc,binop,LHS,RHS);
        }
    }
}

Ast *parseSizeof(Cctrl *cc) {
    Lexeme *tok = cctrlTokenGet(cc);
    Lexeme *peek = cctrlTokenPeek(cc);
    AstType *type = NULL;
    Ast *ast = NULL;

    /* EOF right after `sizeof`: rewinding and re-parsing would re-read
     * the `sizeof` keyword and recurse here forever (stack overflow). */
    if (tok == NULL) {
        cctrlRaiseException(cc, "Unexpected end of input after `sizeof`");
    }
    if (tokenPunctIs(tok,'(') && peek &&
        cctrlIsKeyword(cc,peek->start,peek->len)) {
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
    s64 size = 0; 
    if (ast && ast->kind == AST_STRING) {
        size = ast->real_len;
    } else {
        /* Otherwise the size is sufficient */
        size = type->size;
    }

    assert(size >= 0);
    return astI64Type(size);
}

/* `alignof(<type or expr>)` - mirrors parseSizeof but folds to the
 * type's alignment. Always a compile-time constant. */
Ast *parseAlignof(Cctrl *cc) {
    Lexeme *tok = cctrlTokenGet(cc);
    Lexeme *peek = cctrlTokenPeek(cc);
    AstType *type = NULL;

    /* Same EOF-recursion hazard as parseSizeof. */
    if (tok == NULL) {
        cctrlRaiseException(cc, "Unexpected end of input after `alignof`");
    }
    if (tokenPunctIs(tok,'(') && peek &&
        cctrlIsKeyword(cc,peek->start,peek->len)) {
        type = parseFullType(cc);
        cctrlTokenExpect(cc,')');
    } else {
        cctrlTokenRewind(cc);
        type = parseUnaryExpr(cc)->type;
    }

    return astI64Type(astTypeAlign(type));
}

/* `typeof(<type or expr>)` - same operand handling as sizeof/alignof
 * but folds to a string literal naming the type: typeof(1+1) == "I64",
 * typeof("s") == "U8[2]", typeof(&f) == "F64 *". The expression is
 * only type-checked, never evaluated. */
Ast *parseTypeof(Cctrl *cc) {
    Lexeme *tok = cctrlTokenGet(cc);
    Lexeme *peek = cctrlTokenPeek(cc);
    AstType *type = NULL;

    if (tokenPunctIs(tok,'(') && cctrlIsKeyword(cc,peek->start,peek->len)) {
        type = parseFullType(cc);
        cctrlTokenExpect(cc,')');
    } else {
        cctrlTokenRewind(cc);
        Ast *ast = parseUnaryExpr(cc);
        type = ast ? ast->type : NULL;
    }

    if (!type) {
        cctrlRaiseException(cc,
                "typeof: could not determine the type of the operand");
    }

    AoStr *name = astTypeToAoStr(type);
    /* real_len is the unescaped byte length + NUL; type names contain
     * no escapes so it's just len + 1. */
    return cctrlGetOrSetString(cc, name->data, name->len, name->len + 1);
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
            if (peek == NULL) {
                cctrlRaiseException(cc,
                        "Unexpected end of input after '('");
            }
            if (cctrlIsKeyword(cc,peek->start,peek->len)) {
                type = parseDeclSpec(cc);
                cctrlTokenExpect(cc,')');
                ast = astCast(ast,type);
                continue;
            } else if (ast->kind == AST_CLASS_REF) { 
                int len = strlen(ast->field);
                Vec *argv = parseArgv(cc,ast,')',ast->field,len);
                parseFunctionArgumentCheck(cc,ast,argv,ast->field,len);
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
            ast = astUnaryOperator(ast->type->ptr,AST_UN_OP_DEREF,ast);
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
                return astUnaryOperator(ast->type,AST_UN_OP_POST_INC,ast);
            } else {
                return astUnaryOperator(ast->type,AST_UN_OP_POST_DEC,ast);
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
            case KW_ALIGNOF: return parseAlignof(cc);
            case KW_TYPEOF: return parseTypeof(cc);
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
        AstUnOp unary_op;
        if (!astUnaryOpFromToken(tok->i64, &unary_op)) {
            cctrlTokenRewind(cc);
            return parsePostFixExpr(cc);
        }

        if (unary_op == AST_UN_OP_POST_INC) {
            unary_op = AST_UN_OP_PRE_INC;
        } else if (unary_op == AST_UN_OP_POST_DEC) {
            unary_op = AST_UN_OP_PRE_DEC;
        }

        Lexeme *peek = cctrlTokenPeekBy(cc,1);
        Ast *operand = NULL;
        AstType *type = NULL;

        /* XXX: This feels wrong but allows things like:
         * !arr[0][1][2] to work properly */
        if (tokenPunctIs(peek, '[') && !(unary_op == AST_UN_OP_ADDR_OF ||
                                         unary_op == AST_UN_OP_DEREF)) {
            operand = parseExpr(cc,16);
        } else {
            operand = parseUnaryExpr(cc);
            /* parsePrimary returns NULL when it sees a punct that
             * can't start an expression. Such as a stray
             * slash left over from a malformed block-comment close.
             * Bail with a real error rather than dereffing `NULL`. */
            if (!operand) {
                cctrlRaiseException(cc,
                    "Expected expression after unary '%s'",
                    lexemePunctToString(tok->i64));
            }
            peek = cctrlTokenPeek(cc);
            /* Subscript binds tighter than the unary op (C precedence), so
             * `&t[i]` / `*t[i]` is `&(t[i])` / `*(t[i])`. This must apply to
             * a POINTER operand too, not just arrays/class-refs: otherwise
             * `&ptr[i]` parsed as `(&ptr)[i]` = `*((&ptr)+i)`, indexing the
             * pointer variable's address by sizeof(ptr) instead of computing
             * `ptr + i*sizeof(*ptr)` - garbage for any non-array pointer.
             * After the first subscript, keep consuming the postfix chain
             * (`.field`, `->field`, further `[]`) so `&t[i].x` is
             * `&(t[i].x)` rather than `(&t[i]).x`. */
            if (tokenPunctIs(peek, '[') && operand->type &&
                (operand->kind == AST_CLASS_REF ||
                 operand->type->kind == AST_TYPE_ARRAY ||
                 operand->type->kind == AST_TYPE_POINTER)) {
                cctrlTokenGet(cc);
                operand = parseSubscriptExpr(cc, operand);
                while (1) {
                    peek = cctrlTokenPeek(cc);
                    if (tokenPunctIs(peek, '[')) {
                        cctrlTokenGet(cc);
                        operand = parseSubscriptExpr(cc, operand);
                    } else if (tokenPunctIs(peek, '.')) {
                        cctrlTokenGet(cc);
                        operand = parseGetClassField(cc, operand);
                    } else if (tokenPunctIs(peek, TK_ARROW)) {
                        cctrlTokenGet(cc);
                        if (operand->type->kind != AST_TYPE_POINTER) {
                            cctrlRaiseException(cc,
                                "Pointer expected before '->' got a %s",
                                astTypeKindToHumanReadable(operand->type));
                        }
                        operand = astUnaryOperator(operand->type->ptr,
                                                   AST_UN_OP_DEREF, operand);
                        operand->deref_symbol = TK_ARROW;
                        operand = parseGetClassField(cc, operand);
                    } else {
                        break;
                    }
                }
            }
        }

        switch (unary_op) {
            case AST_UN_OP_ADDR_OF: {
                if (parseIsFunction(operand)) {
                    /* TODO;
                     * Making this a pointer type effects the return type of 
                     * the function. It's hard in this parser to say we want
                     * a pointer to a function not mutate the return value to 
                     * a pointer. This does work however and the "hack" does not
                     * leak. So perhaps we can delete this comment at a later
                     * date */
                    type = operand->type;
                } else {
                    type = astMakePointerType(operand->type);
                }
                break;
            }
            case AST_UN_OP_DEREF:   type = operand->type->ptr; break;
            case AST_UN_OP_BIT_NOT: type = ast_int_type; break;
            default:                type = operand->type; break;
        }

        return astUnaryOperator(type, unary_op, operand);
    }
    
    cctrlTokenRewind(cc);
    return parsePostFixExpr(cc);
}
