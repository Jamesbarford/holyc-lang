#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "lambda.h"
#include "lexer.h"
#include "list.h"
#include "map.h"
#include "parser.h"
#include "prslib.h"
#include "util.h"

/* A function call in a lambda needs to be handled slightly differently
 * as the arguments are references and need to be dereferenced */
Ast *
parseLambdaInnerFunctionArguments(Cctrl *cc, Ast *ast_lambda,
                                      Ast *func_call)
{
    /* this is what the programmer has declared */
    PtrVec *real_argv = parseArgv(cc, func_call, ')', func_call->fname->data,
                                 func_call->fname->len);
    /* However as this is a reference what they actually want is to call
     * the function with a dereferenced version not our ficticious pointer! */
    PtrVec *dereferenced_argv = ptrVecNew();
    for (ssize_t i = 0; i < real_argv->size; ++i) {
        Ast *arg = (Ast *)real_argv->entries[i];
        if (arg->kind != AST_LVAR) {
            ptrVecPush(dereferenced_argv, arg);
        } else {
            Ast *val = strMapGet(ast_lambda->capture, arg->lname->data);
            /* If the value is not in the capture group then this is a normal
             * lvar being passed as a function argument */
            if (!val) {
                ptrVecPush(dereferenced_argv, arg);
            } else {
                /* A 'reference' is techincally a pointer, we want to
                 * dereference whatever it is that is being pointed to.
                 * We do this here rather than in the backend as keeping
                 * track of why something is happening is virtually impossible
                 * due to the recursive nature of the tree. Making this a
                 * dereference allows for minimal changes in the backend. */
                Ast *deref = astUnaryOperator(val->type, AST_DEREF, val);
                ptrVecPush(dereferenced_argv, deref);
            }
        }
    }

    return parseFunctionPrepArgv(cc, func_call, func_call->type->rettype,
                                 func_call->fname->data,
                                 func_call->fname->len,
                                 dereferenced_argv);
}

Ast *
parseLambdaCreateCall(Cctrl *cc, Ast *ast_lambda, PtrVec *argv)
{
    aoStr *scoped_function = cc->tmp_fname;
    int is_recursive_call = aoStrCmp(ast_lambda->lambdafname,
                                       scoped_function);

    /* Add variables from the capture croup */
    if (ast_lambda->capture->size) {
        StrMapIterator *it = strMapIteratorNew(ast_lambda->capture);
        StrMapNode *node = NULL;
        while ((node = strMapNext(it))) {
            /* Find the outer scopes variablec*/
            Ast *original = cctrlGetVar(cc, node->key, strlen(node->key));
            if (!is_recursive_call && !astTypeIsReference(original->type)) {
                /* Take the address of it to pass by pointer */
                AstType *ref_type = astMakeReferenceType(original->type);
                Ast *ref_arg = ref_arg = astUnaryOperator(ref_type, AST_ADDR,
                                                            original);
                ptrVecPush(argv, ref_arg);
            } else {
                ptrVecPush(argv, original);
            }
        }
        strMapIteratorRelease(it);
    }

    if (ast_lambda->lambdafname != NULL) {
        return astFunctionCall(ast_lambda->type->rettype,
                                 ast_lambda->lambdafname->data,
                                 ast_lambda->lambdafname->len, argv);
    } else {
        return astFunctionCall(ast_lambda->type->rettype,
                                 ast_lambda->fname->data,
                                 ast_lambda->fname->len, argv);
    }
}

/**
 * The things in the capture group are _always_ passed by pointer, and are
 * tacked on to the end of the lambdas argv as an additional function parameter.
 * As we now have default arguments this makes some sense and is possible
 * through that mechanism, but is essentially a hack :).
 *
 * XXX: Might remove the '&' as it adds bloat and we are always going to pass
 * by whatever the value of the thing is.
 *
 * gramma:
 * '[' ']'
 * '[' '&' 'variable_name' ']'
 * '[' '&' 'variable_name' ',' ... ']'
 */
static int
parse_lambda_capture_group(Cctrl *cc, lexeme **toks)
{
    lexeme *tok = cctrlTokenGet(cc);
    /* Empty capture group */
    if (tokenPunctIs(tok, ']')) {
        return 0;
    }

    int token_count = 0;
    while (1) {
        if (!tokenPunctIs(tok, '&')) {
            loggerPanic(
                    "line %ld: Expected '&' got '%s' when parsing lambda definition"
                    " can only pass by pointer\n",
                    cc->lineno, lexemeToString(tok));
        }

        tok = cctrlTokenGet(cc);
        if (!tokenIsIdent(tok)) {
            loggerPanic("line %d: Expected identifier got %s\n", tok->line,
                         lexemeToString(tok));
        }
        if (token_count + 1 == LAMBDA_MAX_CAPTURE) {
            loggerPanic(
                    "line %d: Max number of lambda capture variables is %d\n",
                    tok->line, LAMBDA_MAX_CAPTURE);
        }
        toks[token_count++] = tok;

        tok = cctrlTokenGet(cc);
        if (tokenPunctIs(tok, ']')) {
            break;
        } else if (tokenPunctIs(tok, ',')) {
            tok = cctrlTokenGet(cc);
            continue;
        } else {
            loggerPanic("line %d: Expected ']' or ',' got '%s'\n", tok->line,
                         lexemeToString(tok));
        }
    }

    return token_count;
}

/**
 * C++ lambda parsing
 *
 * We join the party at [ :
 *                             +------- peek
 *                             V
 * <AstType> <identifier> '=' '[' '<options>' ']' '(' '<argv>' ')' '->'
 * '<AstType>' '{'
 *    '<Ast>'
 * '}' ';'
 */
Ast *
parseLambda(Cctrl *cc, AstType *decl_type, lexeme *identifier)
{
    if (decl_type->kind != AST_TYPE_AUTO) {
        loggerPanic("line %ld: lambdas can only be declared with auto\n",
                     cc->lineno);
    }

    lexeme *tok = cctrlTokenGet(cc);
    if (!tokenPunctIs(tok, '[')) {
        cctrlRaiseException(cc,"Expected '[' got %s\n", lexemeToString(tok));
    }

    /* Save the enclosing functions scope */
    StrMap *scope_env = cc->localenv;
    List *scope_locals = cc->tmp_locals;
    AstType *scope_rettype = cc->tmp_rettype;
    aoStr *scope_fname = cc->tmp_fname;
    aoStr *lambda_name = aoStrNew();
    aoStr *lambda_global_name = NULL;

    aoStrCatLen(lambda_name, identifier->start, identifier->len);

    /* We have declared a scoped lambda function */
    if (scope_fname != NULL) {
        lambda_global_name = aoStrNew();
        aoStrCatLen(lambda_global_name, scope_fname->data, scope_fname->len);
        aoStrPutChar(lambda_global_name, '_');
        aoStrPutChar(lambda_global_name, '_');
        aoStrCatLen(lambda_global_name, lambda_name->data, lambda_name->len);
    } else {
        lambda_global_name = lambda_name;
    }

    cc->tmp_fname = lambda_global_name;

    List *lambda_locals = listNew();

    cc->localenv = strMapNewWithParent(32, cc->global_env);
    cc->tmp_locals = lambda_locals;

    lexeme *tokens[LAMBDA_MAX_CAPTURE];
    int token_count = parse_lambda_capture_group(cc, tokens);
    StrMap *lambda_capture_map = strMapNew(LAMBDA_MAX_CAPTURE);

    tok = cctrlTokenGet(cc);

    if (!tokenPunctIs(tok, '(')) {
        cctrlRaiseException(cc,"Expected '(' got %s\n", lexemeToString(tok));
    }

    int has_var_args = 0;
    PtrVec *lambda_params = parseParams(cc, ')', &has_var_args, 1);

    for (int i = 0; i < token_count; ++i) {
        /* Look up the variables from the parent */
        lexeme *capture = tokens[i];
        Ast *capture_var = strMapGetLen(scope_env, capture->start,
                                          capture->len);
        Ast *ref = NULL;
        /* This can happen if we are inside another lambda who's capture
         * group has the same variable as a reference */
        if (astTypeIsReference(capture_var->type)) {
            ref = capture_var;
        } else {
            ref = astMakeParamReference(capture_var);
        }

        /* I think the only thing we can actually deal with is lvars
         * I can't see how anything else would work */
        switch (capture_var->kind) {
        case AST_LVAR:
            strMapAdd(cc->localenv, capture_var->lname->data, ref);
            strMapAdd(lambda_capture_map, capture_var->lname->data, ref);
            break;
        case AST_DECL:
            strMapAdd(cc->localenv, capture_var->declvar->lname->data, ref);
            strMapAdd(lambda_capture_map, capture_var->declvar->lname->data,
                       ref);
            break;
        }
        listAppend(cc->tmp_locals, ref);
        ptrVecPush(lambda_params, ref);
    }

    AstType *return_type = NULL;

    /* We've skipped explicit type definitions for the return type so will
     * need to infer it */
    tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok, '{')) {
        return_type = cctrlGetKeyWord(cc, "auto", 4);
        cctrlTokenRewind(cc);
    } else if (tokenPunctIs(tok, TK_ARROW)) {
        return_type = parseFullType(cc);
    } else {
        cctrlRaiseException(cc,
                "Expected '->' with an explicit type or '{' for inference got %s",
                lexemeToString(tok));
    }

    if (return_type->kind == AST_TYPE_ARRAY) {
        cctrlRaiseException(cc,"Cannot return arrays from a lambda");
    }

    tok = cctrlTokenGet(cc);
    if (!tokenPunctIs(tok, '{')) {
        cctrlRaiseException(cc, "Expected '{' got %s", lexemeToString(tok));
    }

    AstType *rettype = astMakeFunctionType(return_type,lambda_params);

    cc->tmp_rettype = return_type;

    /* instantiate the lambda without a body, as we want to store it in the
     * symbol table so it can be used for recursion */
    Ast *ast_lambda = astMakeLambda(rettype, lambda_global_name, lambda_name,
                                      lambda_params,
                                      /*lambda_body=*/NULL, lambda_locals,
                                      lambda_capture_map, has_var_args);

    if (scope_env) {
        if (!strMapAddOrErr(scope_env, ast_lambda->fname->data, ast_lambda)) {
            cctrlRaiseException(cc,"Lambda %s already declared",
                astLValueToString(ast_lambda, 0));
        }
    }

    /* Allow for recursion */
    if (cc->localenv) {
        if (!strMapAddOrErr(cc->localenv, ast_lambda->fname->data,
                             ast_lambda)) {
            cctrlRaiseException(cc,"Lambda %s already declared",
                         astLValueToString(ast_lambda, 0));
        }
    }
    if (!strMapAddOrErr(cc->global_env, ast_lambda->lambdafname->data,
                         ast_lambda)) {
        cctrlRaiseException(cc, "Lambda %s already declared",
                     astLValueToString(ast_lambda, 0));
    }

    ast_lambda->body = parseCompoundStatement(cc);//, PRS_STMT_BAN_GOTO);
    /* Correct the return type */
    if (cc->tmp_rettype != return_type) {
        ast_lambda->type = astMakeFunctionType(cc->tmp_rettype, lambda_params);
    } else if (return_type->kind == AST_TYPE_AUTO) {
        ast_lambda->type = cctrlGetKeyWord(cc,str_lit("U0"));
    }

    cctrlTokenExpect(cc, ';');

    /* Rest back to the enclosing functions scope */
    cc->tmp_locals = scope_locals;
    cc->localenv = scope_env;
    cc->tmp_rettype = scope_rettype;
    cc->tmp_fname = scope_fname;
    return ast_lambda;
}

/* we join at ( */
Ast *parseLambdaNoCapture(Cctrl *cc, AstType *type, lexeme *identifier) {
    int has_var_args = 0;
    StrMap *scope_env = cc->localenv;
    List *scope_locals = cc->tmp_locals;
    AstType *scope_rettype = cc->tmp_rettype;
    aoStr *scope_fname = cc->tmp_fname;
    aoStr *lambda_name = aoStrNew();
    aoStr *lambda_global_name = NULL;
    List *lambda_locals = listNew();

    cc->localenv = strMapNewWithParent(32, cc->global_env);
    cc->tmp_locals = lambda_locals;
    
    PtrVec *lambda_params = parseParams(cc, ')', &has_var_args, 1);
    AstType *lambda_rettype = astMakeFunctionType(type, lambda_params);



    aoStrCatLen(lambda_name, identifier->start, identifier->len);

    /* We have declared a scoped lambda function */
    if (scope_fname != NULL) {
        lambda_global_name = aoStrNew();
        aoStrCatLen(lambda_global_name, scope_fname->data, scope_fname->len);
        aoStrPutChar(lambda_global_name, '_');
        aoStrPutChar(lambda_global_name, '_');
        aoStrCatLen(lambda_global_name, lambda_name->data, lambda_name->len);
    } else {
        lambda_global_name = lambda_name;
    }


    StrMap *lambda_capture_map = strMapNew(4);

    Ast *ast_lambda = astMakeLambda(lambda_rettype, lambda_global_name,
                                    lambda_name,
                                    lambda_params,
                                    /*lambda_body=*/NULL,
                                    lambda_locals,
                                    lambda_capture_map,
                                    has_var_args);

    if (scope_env) {
        if (!strMapAddOrErr(scope_env, ast_lambda->fname->data, ast_lambda)) {
            cctrlRaiseException(cc,"Lambda %s already declared",
                astLValueToString(ast_lambda, 0));
        }
    }

    /* Allow for recursion */
    if (cc->localenv) {
        if (!strMapAddOrErr(cc->localenv, ast_lambda->fname->data,
                             ast_lambda)) {
            cctrlRaiseException(cc,"Lambda %s already declared",
                         astLValueToString(ast_lambda, 0));
        }
    }
    if (!strMapAddOrErr(cc->global_env, ast_lambda->lambdafname->data,
                         ast_lambda)) {
        cctrlRaiseException(cc, "Lambda %s already declared\n",
                     astLValueToString(ast_lambda, 0));
    }


    lexeme *tok = cctrlTokenGet(cc);
    cc->tmp_fname = ast_lambda->fname;
    Ast *body = parseCompoundStatement(cc);
    ast_lambda->body = body;
    lambda_rettype->rettype = cc->tmp_rettype;

    /* Rest back to the enclosing functions scope */
    cc->tmp_locals = scope_locals;
    cc->localenv = scope_env;
    cc->tmp_rettype = scope_rettype;
    cc->tmp_fname = scope_fname;
    return ast_lambda;
} 
