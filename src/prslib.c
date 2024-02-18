#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "list.h"
#include "prsutil.h"
#include "prslib.h"
#include "util.h"

static AstType *ParseArrayDimensionsInternal(Cctrl *cc, AstType *base_type);

void ParseAssignAuto(Cctrl *cc, Ast *ast) {
    if (ast->declinit == NULL) {
        loggerPanic("line %ld: auto must have an initaliser\n", cc->lineno);
    }
    if (ast->declinit->kind == AST_FUNC) {
        loggerPanic("line %ld: auto with functions is not yet supported\n",
                cc->lineno);
    }
    ast->declvar->type = ast->declinit->type;
}

AstType *ParseReturnAuto(Cctrl *cc, Ast *retval) {
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

AstType *ParsePointerType(Cctrl *cc, AstType *type) {
    lexeme *tok;
    while (1) {
        tok = CctrlTokenGet(cc);
        if (!TokenPunctIs(tok, '*')) {
            CctrlTokenRewind(cc);
            return type;
        }
        type = AstMakePointerType(type);
    }
    return type;
}

AstType *ParseFunctionPointerType(Cctrl *cc,
        char **fnptr_name, int *fnptr_name_len, AstType *rettype)
{
    int has_var_args;
    lexeme *fname;
    List *params;
    CctrlTokenExpect(cc,'*');
    fname = CctrlTokenGet(cc);
    if (fname->tk_type != TK_IDENT) {
        loggerPanic("line %d: Expected function pointer name got: %s\n",
                fname->line, lexemeToString(fname));
    }
    *fnptr_name = fname->start;
    *fnptr_name_len = fname->len;
    CctrlTokenExpect(cc,')');
    CctrlTokenExpect(cc,'(');
    params = ParseParams(cc,')',&has_var_args);
    return AstMakeFunctionType(rettype,params);
}

Ast *ParseFunctionPointer(Cctrl *cc, AstType *rettype) {
    Ast *ast;
    char *fnptr_name;
    int fnptr_name_len;
    AstType *fnptr_type;

    fnptr_type = ParseFunctionPointerType(cc,
            &fnptr_name,
            &fnptr_name_len,
            rettype);

    ast = AstFunctionPtr(
            fnptr_type,
            fnptr_name,
            fnptr_name_len,
            fnptr_type->params);
    return ast;
}

Ast *ParseDefaultFunctionParam(Cctrl *cc, Ast *var) {
    if (var->type->kind == AST_TYPE_ARRAY) {
        loggerPanic("line %ld: Cannot have a default value of type array.\n",
                cc->lineno);
    }
    Ast *init = ParseExpr(cc,16);
    return AstFunctionDefaultParam(var,init);
}

List *ParseParams(Cctrl *cc, long terminator,
        int *has_var_args)
{
    List *params = ListNew();
    lexeme *tok, *pname;
    AstType *type;
    Ast *var;

    tok = CctrlTokenGet(cc);
    if (TokenPunctIs(tok,terminator)) {
        return params;
    }
    CctrlTokenRewind(cc);
    
    while (1) {
        pname = CctrlTokenPeek(cc);

        /* VarArgs */
        if (TokenPunctIs(pname, TK_ELLIPSIS)) {
            CctrlTokenGet(cc);
            var = AstVarArgs();
            DictSet(cc->localenv,var->argc->lname->data,var->argc);
            DictSet(cc->localenv,var->argv->lname->data,var->argv);
            ListAppend(params,var);
            if (cc->tmp_locals) {
                ListAppend(cc->tmp_locals, var->argc);
                ListAppend(cc->tmp_locals, var->argv);
            }
            *has_var_args = 1;
            /* Var args _has_ to be last */
            CctrlTokenExpect(cc,terminator);
            return params;
        }

        type = ParseDeclSpec(cc);
        pname = CctrlTokenGet(cc);
        if (pname->tk_type != TK_IDENT) {
            /* Function pointer */
            if (TokenPunctIs(pname, '(')) {
                type = ParseArrayDimensions(cc,type);
                if (type->kind == AST_TYPE_ARRAY) {
                    type = AstMakePointerType(type->ptr);
                }
                var = ParseFunctionPointer(cc,type);
                DictSet(cc->localenv,var->fname->data,var);
                if (cc->tmp_locals) {
                    ListAppend(cc->tmp_locals, var);
                }
                ListAppend(params, var);
                tok = CctrlTokenGet(cc);
                if (TokenPunctIs(tok, terminator)) {
                    return params;
                }
                continue;
            } else {
                loggerPanic("line %d: Identifier expected, got: %s\n",
                        pname->line,lexemeToString(pname));
            }
        }

        type = ParseArrayDimensions(cc,type);
        if (type->kind == AST_TYPE_ARRAY) {
            type = AstMakePointerType(type->ptr);
        }
        var = AstLVar(type,pname->start,pname->len);

        tok = CctrlTokenGet(cc);
        if (TokenPunctIs(tok, '=')) {
            var = ParseDefaultFunctionParam(cc,var);
            DictSet(cc->localenv,var->declvar->lname->data,var);
            tok = CctrlTokenGet(cc);
        } else {
            DictSet(cc->localenv,var->lname->data,var);
        }

        if (cc->tmp_locals) {
            ListAppend(cc->tmp_locals, var);
        }
        ListAppend(params, var);

        if (TokenPunctIs(tok,terminator)) {
            return params;
        }

        if (!TokenPunctIs(tok,',')) {
            loggerPanic("line %d: Comma expected, got %s\n",
                    tok->line,lexemeToString(tok));
        }
    }
}

/* Does not parse pointer types, only the base type */
AstType *ParseBaseDeclSpec(Cctrl *cc) {
    lexeme *tok = CctrlTokenGet(cc);
    AstType *type;

    if (!tok) {
        return NULL;
    }

    if (tok->tk_type == TK_KEYWORD || tok->tk_type == TK_IDENT) {
        type = ParseGetType(cc, tok);
    } else {
        loggerPanic("line %ld: Declaration or specfier must be an itentifier got: %s\n",
                cc->lineno,
                lexemeToString(tok));
    }

    return type;
}

AstType *ParseDeclSpec(Cctrl *cc) {
    AstType *type = ParseBaseDeclSpec(cc);
    type = ParsePointerType(cc,type);
    return type;
}

static AstType *ParseArrayDimensionsInternal(Cctrl *cc, AstType *base_type) {
    Ast *size;
    lexeme *tok, *next_tok;
    int dimension;

    tok = CctrlTokenGet(cc);
    dimension = -1;

    if (!TokenPunctIs(tok, '[')) {
        CctrlTokenRewind(cc);
        return NULL;
    }

    next_tok = CctrlTokenPeek(cc);
    if (!TokenPunctIs(next_tok, ']')) {
        size = ParseExpr(cc,16);
        dimension = EvalIntConstExpr(size);
    }
    CctrlTokenExpect(cc,']');
    AstType *sub_type = ParseArrayDimensionsInternal(cc,base_type);
    if (sub_type) {
        if (sub_type->len == -1 && dimension == -1) {
            loggerPanic("line %ld: Array size not specified.\n",cc->lineno);
        }
        return AstMakeArrayType(sub_type,dimension);
    }

    return AstMakeArrayType(base_type,dimension);
}

AstType *ParseArrayDimensions(Cctrl *cc, AstType *base_type) {
    AstType *type = ParseArrayDimensionsInternal(cc,base_type);
    if (type) {
        return type;
    }
    return base_type;
} 

/* Calls TokenGet internally so ensure you are 1 token behind the type */
AstType *ParseFullType(Cctrl *cc) {
    AstType *type = ParseDeclSpec(cc);
    return ParseArrayDimensions(cc,type);
}

void ParseDeclInternal(Cctrl *cc, lexeme **tok, AstType **type) {
    AstType *_type = ParseDeclSpec(cc);
    lexeme *_tok = CctrlTokenGet(cc);
    if (TokenPunctIs(_tok,';')) {
        CctrlTokenRewind(cc);
        *tok = NULL;
        return;
    }

    if (_tok->tk_type != TK_IDENT) {
        CctrlTokenRewind(cc);
        *tok = NULL;
    } else {
        *tok = _tok;
    }
    *type = ParseArrayDimensions(cc,_type);
}


static Ast *findFunctionDecl(Cctrl *cc, char *fname, int len) {
    Ast *decl;
    if ((decl = DictGetLen(cc->global_env,fname,len)) != NULL) {
        if (decl->kind == AST_FUNC ||
            decl->kind == AST_EXTERN_FUNC ||
            decl->kind == AST_FUN_PROTO) {
            return decl;
        }
    } else if (cc->localenv && (decl = DictGetLen(cc->localenv,fname,len)) != NULL) {
        if (decl->kind == AST_FUNPTR || decl->kind == AST_LVAR) {
            return decl;
        }
    } else if ((decl = DictGetLen(cc->asm_funcs,fname,len)) != NULL) {
        /* Assembly function */
        return decl;
    }
    return NULL;
}

List *ParseArgv(Cctrl *cc, Ast *decl, long terminator, char *fname, int len) {
    List *argv, *var_args = NULL, *params = NULL, *parameter = NULL; 
    Ast *ast, *param = NULL;
    AstType *check, *rettype;
    lexeme *tok;

    if (decl) {
        rettype = decl->type->rettype;
        if (decl->kind == AST_FUNPTR) {
            params = ((Ast *)decl)->type->params;
        } else {
            params = decl->params;
        }
    }


    argv = ListNew();
    tok = CctrlTokenPeek(cc);
    if (params) {
        parameter = params->next;
        param = parameter->value;
    }

    argv = ListNew();
    tok = CctrlTokenPeek(cc);

    while (tok && !TokenPunctIs(tok, terminator)) {
        ast = ParseExpr(cc,16);
        if (ast == NULL) {
            if (param && param->kind == AST_DEFAULT_PARAM) {
                ast = param->declinit;
            }
        }

        if (param != NULL && param->kind == AST_VAR_ARGS) {
            if (decl && decl->kind == AST_EXTERN_FUNC) {
                ListAppend(argv,ast);
            } else {
                if (var_args == NULL) {
                    var_args = ListNew();
                }
                ListAppend(var_args,ast);
            }
        } else {
            /* Does a distinctly adequate job of type checking function parameters */
            if (param && param->kind != AST_DEFAULT_PARAM) {
                if ((check = AstTypeCheck(param->type,ast)) == NULL) {
                    Ast *func = findFunctionDecl(cc,fname,len);
                    char *fstring, *expected, *got, *ast_str;
                    if (func) {
                        fstring = AstFunctionToString(func);
                    } else {
                        fstring = AstFunctionNameToString(rettype,fname,len);
                    }

                    expected = AstTypeToColorString(param->type);
                    got = AstTypeToColorString(ast->type);
                    ast_str = AstLValueToString(ast);
                    loggerWarning("line %ld: %s incompatiable function argument %s got %s %s\n",
                            cc->lineno,fstring,expected,got,ast_str);
                    free(fstring);
                    free(expected);
                    free(got);
                    free(ast_str);
                }
            }
            ListAppend(argv,ast);
        }

        tok = CctrlTokenGet(cc);

        if (TokenPunctIs(tok, terminator)) {
            break;
        }

        if (!TokenPunctIs(tok,',')) {
            loggerPanic("line %d: Unexpected token: Expected: ',' got: '%s'\n",
                   tok->line,lexemeToString(tok));
        }

        if (parameter && parameter->next != parameter) {
            if (param->kind != AST_VAR_ARGS) {
                parameter = parameter->next;
                param = parameter->value;
            }
        }
        tok = CctrlTokenPeek(cc);
    }

    if (var_args) {
        /* set the argument count and merge lists */
        ListAppend(argv, AstI64Type(ListCount(var_args)));
        ListMergeAppend(argv,var_args);
    }

    if (argv->next == argv) {
        CctrlTokenGet(cc);
    }

    return argv;
}

/* Read function arguments for a function being called */
Ast *ParseFunctionArguments(Cctrl *cc, char *fname, int len, long terminator) {
    List *argv, *params = NULL; 
    AstType *rettype;
    Ast *decl;

    decl = findFunctionDecl(cc,fname,len);
    if (decl) {
        rettype = decl->type->rettype;
    }

    argv = ParseArgv(cc,decl,terminator,fname,len);

    if (!decl) {
        if ((len == 6 && !strncmp(fname,"printf",6))) {
            params = ListNew();
            rettype = ast_int_type;
            return AstFunctionCall(rettype,fname,len,argv,params);
        }
        loggerPanic("line %ld: Function: %.*s() not defined\n",
                cc->lineno,len,fname);
    }

    if (argv->next == argv) {
        params = ListNew();
    }

    switch (decl->kind) {
        case AST_LVAR:
        case AST_FUNPTR:
            return AstFunctionPtrCall(rettype,fname,len,argv,params,decl);

        case AST_ASM_FUNCDEF:
        case AST_ASM_FUNC_BIND:
            return AstAsmFunctionCall(rettype,
                    aoStrDup(decl->asmfname),argv,params);

        case AST_EXTERN_FUNC:
        case AST_FUNC:
        case AST_FUN_PROTO:
            return AstFunctionCall(rettype,fname,len,argv,params);
        default: {
            loggerPanic("line %ld: Unknown function: %.*s\n",
                    cc->lineno,len,fname);
        }
    }
}

/* Parse either a function call or a variable being used */
static Ast *ParseIdentifierOrFunction(Cctrl *cc, char *name, int len,
        int can_call_function)
{
    Ast *ast;
    int is_lparen;
    lexeme *tok,*peek;

    tok = CctrlTokenGet(cc);
    peek = CctrlTokenPeek(cc);
    is_lparen = TokenPunctIs(tok,'(');

    if (!is_lparen) {
        CctrlTokenRewind(cc);
        if ((ast = CctrlGetVar(cc, name, len)) == NULL) {
            loggerPanic("line %ld: Cannot find variable: %.*s\n",
                    cc->lineno,len,name);
        }
        /* Function calls with no arguments are 'Function;' */
        if (can_call_function && ast->kind == AST_FUNC) {
            return AstFunctionCall(ast->type->rettype,ast->fname->data,ast->fname->len,ListNew(),ListNew());
        }
        return ast;
    }

    /* Is a function call if the next char is '(' and the peek is not a type */
    if (!CctrlIsKeyword(cc,peek->start,peek->len)) {
        return ParseFunctionArguments(cc,name,len,')');
    }

    /* Is a postfix typecast, need to grap the variable and 'ParsePostFixExpr' will do the cast */
    if ((ast = CctrlGetVar(cc, name, len)) == NULL) {
        loggerPanic("line %ld: Cannot find variable: %.*s\n",cc->lineno,
                len, name);
    }
    CctrlTokenRewind(cc);
    return ast;
}

static Ast *ParsePrimary(Cctrl *cc) {
    lexeme *tok; 
    Ast *ast;
    lexeme *prev;
    int can_call_function = 1;
    CctrlTokenRewind(cc);

    prev = CctrlTokenGet(cc);
    tok = CctrlTokenGet(cc);

    if (!tok) {
        return NULL;
    }

    if (TokenPunctIs(prev,'&')) {
        can_call_function = 0;
    }

    switch (tok->tk_type) {
    case TK_IDENT: {
        ast = ParseIdentifierOrFunction(cc, tok->start, tok->len,
                can_call_function);
        return ast;
    }
    case TK_I64: {
        ast = AstI64Type(tok->i64);
        return ast;
    }
    case TK_F64:
        return AstF64Type(tok->f64);
    case TK_CHAR_CONST:
        return AstCharType(tok->i64);
    case TK_STR:
        ast = AstString(tok->start, tok->len);
        if (cc->strings) {
            ListAppend(cc->strings, ast);
        }
        return ast;

    case TK_PUNCT:
        CctrlTokenRewind(cc);
        return NULL;
    case TK_EOF:
        loggerPanic("line %d: Unexpected EOF\n", tok->line);
        break;
    default:
        loggerPanic("line %d: Unexpected input %s\n",tok->line,AstKindToString(tok->tk_type));
    }
}

static int ParseGetPriority(lexeme *tok) {
    switch (tok->i64) {
    case '.': case '[': case TK_ARROW:
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

Ast *ParseSubscriptExpr(Cctrl *cc, Ast *ast) {
    Ast *subscript = ParseExpr(cc,16);
    CctrlTokenExpect(cc, ']');
    Ast *binop = AstBinaryOp('+', ast, subscript);
    return AstUnaryOperator(binop->type->ptr, AST_DEREF, binop);
}

Ast *ParseGetClassField(Cctrl *cc, Ast *cls) {
    AstType *type;
    if (!ParseIsClassOrUnion(cls->type->kind)) {
        loggerPanic("line %ld: Expected class got: %s %s\n",
                cc->lineno,
                AstKindToString(cls->kind),
                AstToString(cls));
    }

    type = cls->type;

    lexeme *tok = CctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        loggerPanic("line %d: Expected class member got: %s\n",
                tok->line,
                lexemeToString(tok));
    }

    // XXX: This is hacky and only for recusive data types 
    if (type->fields == NULL && cls->kind == AST_DEREF) {
        type = cls->cls->type;
    }
    AstType *field = DictGetLen(type->fields, tok->start, tok->len);
    if (!field) {
        loggerPanic("line %d: Property: %.*s does not exist on class\n", 
                tok->line,tok->len, tok->start);
    }
    aoStr *field_name = aoStrDupRaw(tok->start, tok->len);
    return AstClassRef(field, cls, aoStrMove(field_name));
}

static int ParseCompoundAssign(lexeme *tok) {
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

Ast *ParseExpr(Cctrl *cc, int prec) {
    Ast *LHS, *RHS;
    lexeme *tok;
    int prec2, next_prec, compound_assign;

    if ((LHS = ParseUnaryExpr(cc)) == NULL) {
        return NULL;
    }

    while (1) {
        /* Can be TK_IDENT, or TK_F64 or TK_I64 or when type casting exists '(' TK_PUNCT */
        if ((tok = CctrlTokenGet(cc)) == NULL) {
            return LHS;
        }

        if (tok->tk_type != TK_PUNCT) {
            CctrlTokenRewind(cc);
            return LHS;
        }

        prec2 = ParseGetPriority(tok);
        if (prec2 < 0 || prec <= prec2) {
            CctrlTokenRewind(cc);
            return LHS;
        }

        if (TokenPunctIs(tok,'.')) {
            LHS = ParseGetClassField(cc,LHS);
            continue;
        }

        if (TokenPunctIs(tok,'(')) {
            AssertLValue(LHS,cc->lineno);
            AstType *type = ParseDeclSpec(cc);
            LHS = AstCast(LHS,type);
            continue;
        }

        if (TokenPunctIs(tok, TK_PLUS_PLUS) || TokenPunctIs(tok, TK_MINUS_MINUS)) {
            AssertLValue(LHS,cc->lineno);
            LHS = AstUnaryOperator(LHS->type, tok->i64, LHS);
            continue;
        }

        if (TokenPunctIs(tok,'[')) {
            LHS = ParseSubscriptExpr(cc,LHS);
            continue;
        }

        if (TokenPunctIs(tok, TK_ARROW)) {
            if (LHS->type->kind != AST_TYPE_POINTER) {
                loggerPanic("line %d: Pointer expected got: %s %s\n",
                        tok->line,
                        AstTypeToString(LHS->type), AstToString(LHS));
            }
            LHS = AstUnaryOperator(LHS->type->ptr, AST_DEREF, LHS);
            LHS = ParseGetClassField(cc, LHS);
            continue;
        }

        compound_assign = ParseCompoundAssign(tok);
        if (TokenPunctIs(tok, '=') || compound_assign) {
            AssertLValue(LHS,cc->lineno);
        }
        
        next_prec = prec2;
        if (tok->i64 == '=') {
            next_prec++;
        }

        RHS = ParseExpr(cc,next_prec);
        if (!RHS) {
            loggerPanic("line %d: Second operand missing to: %s\n",
                tok->line,AstToString(LHS));
        }

        if (compound_assign) {
            LHS = AstBinaryOp('=', LHS, 
                    AstBinaryOp(compound_assign, LHS, RHS));
        } else {
            LHS = AstBinaryOp(tok->i64,LHS,RHS);
        }
    }
}

static Ast *ParseCast(Cctrl *cc) {
    Ast *ast;
    AstType *cast_type;

    CctrlTokenExpect(cc,'<');
    cast_type = ParseDeclSpec(cc);
    CctrlTokenExpect(cc,'>');
    CctrlTokenExpect(cc,'(');
    ast = ParseExpr(cc,16);
    CctrlTokenExpect(cc,')');
    ast = AstCast(ast,cast_type);
    return ast;
}

static AstType *ParseSizeOfType(Cctrl *cc) {
    lexeme *tok,*peek;
    AstType *type;
    Ast *ast;

    tok = CctrlTokenGet(cc);
    peek = CctrlTokenPeek(cc);

    if (TokenPunctIs(tok,'(') && CctrlIsKeyword(cc,peek->start,peek->len)) {
        type = ParseFullType(cc);
        CctrlTokenExpect(cc,')');
        peek = CctrlTokenPeek(cc);
        return type;
    }

    CctrlTokenRewind(cc);
    ast = ParseUnaryExpr(cc);
    return ast->type;
}

static Ast *ParseSizeof(Cctrl *cc) {
    AstType *type = ParseSizeOfType(cc);
    long size = type->size;
    assert(size >= 0);
    return AstI64Type(size);
}

Ast *ParsePostFixExpr(Cctrl *cc) {
    Ast *ast;
    AstType *type;
    lexeme *tok,*peek;

    /* Parse primary rightly or wrongly, amongst other things, either gets a 
     * variable or parses a function call. */
    if ((ast = ParsePrimary(cc)) == NULL) {
        return NULL;
    }

    if (ParseIsFunctionCall(ast)) {
        return ast;
    }

    while (1) {
        tok = CctrlTokenGet(cc);

        if (TokenPunctIs(tok,'[')) {
            /* XXX: something is wrong with the below however, if a pointer gets 
             * parsed as a binary expression it works, but does not for arrays.
             * However if you put parenthesis around the pointer access it blows 
             * up.
             */

            /* This is for normal arrays */
            if (ast->type->kind == AST_TYPE_ARRAY) {
                ast = ParseSubscriptExpr(cc,ast);
                if (TokenPunctIs(CctrlTokenPeek(cc),'(')) {
                    continue;
                } else {
                    return ast;
                }
            }
            peek = CctrlTokenPeek(cc);
            if (TokenPunctIs(peek,'(')) {
                continue;
            } else {
                CctrlTokenRewind(cc);
                return ast;
            }
        }


        if (TokenPunctIs(tok,'.')) {
            ast = ParseGetClassField(cc,ast);
            continue;
        }

        /* Postfix type cast OR function pointer on a class */
        if (TokenPunctIs(tok,'(')) {
            peek = CctrlTokenPeek(cc);
            if (CctrlIsKeyword(cc,peek->start,peek->len)) {
                type = ParseDeclSpec(cc);
                CctrlTokenExpect(cc,')');
                ast = AstCast(ast,type);
                continue;
            } else if (ast->kind == AST_CLASS_REF) { 
                int len = strlen(ast->field);
                List *argv = ParseArgv(cc,ast,')',ast->field,len);
                ast = AstFunctionPtrCall(
                        ast->type->rettype,
                        ast->field,
                        len,
                        argv,
                        ast->type->params,
                        ast);
                continue;
            }
        }

        if (TokenPunctIs(tok,TK_ARROW)) {
            if (ast->type->kind != AST_TYPE_POINTER) {
                loggerPanic("line %d: Pointer expected got: %s\n",
                        tok->line,
                        AstKindToString(ast->type->kind));
            }
            ast = AstUnaryOperator(ast->type->ptr,AST_DEREF,ast);
            ast = ParseGetClassField(cc,ast);
            continue;
        }

        if (TokenPunctIs(tok,TK_PLUS_PLUS) ||
                TokenPunctIs(tok,TK_MINUS_MINUS)) {
            AssertLValue(ast,cc->lineno);
            if (tok->i64 == TK_PLUS_PLUS) {
                return AstUnaryOperator(ast->type,TK_PLUS_PLUS,ast);
            } else {
                return AstUnaryOperator(ast->type,TK_MINUS_MINUS,ast);
            }
        }

        CctrlTokenRewind(cc);
        return ast;
    }
}

Ast *ParseUnaryExpr(Cctrl *cc) {
    lexeme *tok;
    Ast *ast;

    if ((tok = CctrlTokenGet(cc)) == NULL) {
        loggerPanic("line %ld: Unexpected end of input\n",cc->lineno);
    }

    if (tok->tk_type == TK_KEYWORD) {
        switch (tok->i64) {
            case KW_SIZEOF: return ParseSizeof(cc);
            case KW_CAST:   return ParseCast(cc);
            default:
                loggerPanic("line %d: Unexpected keyword: %.*s while parsing unary expression\n",
                        tok->line,tok->len,tok->start);
        }
    }

    if (TokenPunctIs(tok,'(')) {
        ast = ParseExpr(cc,16);
        CctrlTokenExpect(cc,')');
        return ast;
    }

    /* Obtaining the address of a variable */
    if (TokenPunctIs(tok, '&')) {
        Ast *operand = ParseUnaryExpr(cc);
        /* We do not take the address of a function */
        if (operand->kind == AST_FUNC) {
            return operand;
        }
        return AstUnaryOperator(AstMakePointerType(operand->type),
                AST_ADDR,operand);
    }

    /* Dereferencing */
    if (TokenPunctIs(tok, '*')) {
        Ast *operand = ParseUnaryExpr(cc);
        return AstUnaryOperator(operand->type->ptr,
                AST_DEREF,operand);
    }

    if (TokenPunctIs(tok, TK_PLUS_PLUS)) {
        Ast *operand = ParseUnaryExpr(cc);
        return AstUnaryOperator(operand->type,
                TK_PRE_PLUS_PLUS,operand);
    }

    if (TokenPunctIs(tok, TK_MINUS_MINUS)) {
        Ast *operand = ParseUnaryExpr(cc);
        return AstUnaryOperator(operand->type,
                TK_PRE_MINUS_MINUS,operand);
    }

    if (TokenPunctIs(tok,'~')) {
        Ast *operand = ParseUnaryExpr(cc);
        return AstUnaryOperator(ast_int_type,'~',operand);
    }

    if (TokenPunctIs(tok,'!')) {
        Ast *operand = ParseUnaryExpr(cc);
        return AstUnaryOperator(operand->type,'!',operand);
    }

    if (TokenPunctIs(tok,'-')) {
        Ast *operand = ParseUnaryExpr(cc);
        operand = AstUnaryOperator(operand->type,'-',operand);
        return operand;
    }

    CctrlTokenRewind(cc);
    return ParsePostFixExpr(cc);
}
