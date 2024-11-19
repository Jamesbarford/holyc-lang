#include <__stdarg_va_list.h>
#include <string.h>
#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "prsutil.h"
#include "util.h"

int align(int n, int m) {
    int rem = n % m;
    if (rem == 0) {
        return n;
    }
    return n - rem + m;
}

inline int parseIsFloatOrInt(Ast *ast) {
    return astIsIntType(ast->type) || 
           astIsFloatType(ast->type);
}

inline int parseIsClassOrUnion(int kind) {
    return kind == AST_TYPE_CLASS || 
           kind == AST_TYPE_UNION;
}

inline int parseIsFunction(Ast *ast) {
    if (ast) {
        switch (ast->kind) {
            case AST_FUNC:
            case AST_FUN_PROTO:
            case AST_ASM_FUNC_BIND:
            case AST_ASM_FUNCDEF:
            case AST_EXTERN_FUNC:
                return 1;
            default:
                return 0;
        }
    }
    return 0;
}

int parseIsFunctionCall(Ast *ast) {
    return ast && (ast->type->kind == AST_FUNCALL || 
           ast->type->kind == AST_FUNPTR_CALL || 
           ast->type->kind == AST_ASM_FUNCALL);
}

void assertIsFloat(Ast *ast, long lineno) {
    if (ast && !astIsFloatType(ast->type)) {
        loggerPanic("line %ld: Expected float type got %s\n",
                lineno,astTypeToString(ast->type));
    }
}

void assertIsInt(Ast *ast, long lineno) {
    if (ast && !astIsIntType(ast->type)) {
        loggerPanic("line %ld: Expected int type got %s\n",
                lineno, astTypeToString(ast->type));
    }
}

void assertIsFloatOrInt(Ast *ast, long lineno) {
    if (!parseIsFloatOrInt(ast)) {
        loggerPanic("line %ld: Expected float got %s\n",
                lineno, astTypeToString(ast->type));
    }
}

void assertIsPointer(Ast *ast, long lineno) {
    if (!ast || ast->type->kind != AST_TYPE_POINTER) {
        loggerPanic("line %ld: Expected float got %s\n",
                lineno,astTypeToString(ast->type));
    }
}

static char *getTerminator(long terminator_flags) {
    static char buffer[64];
    int cnt = 0;
    if (terminator_flags & PUNCT_TERM_SEMI) {
        buffer[cnt++] = ';';
    } 
    if (terminator_flags & PUNCT_TERM_RPAREN) {
        if (cnt > 0) {
            cnt += snprintf(buffer+cnt,sizeof(buffer)," or ");
        }
        buffer[cnt++] = ';';
    } 
    if (terminator_flags & PUNCT_TERM_COMMA) {
        if (cnt > 0) {
            cnt += snprintf(buffer+cnt,sizeof(buffer)," or ");
        }
        buffer[cnt++] = ',';
    }
    buffer[cnt] = '\0';
    return buffer;
}

char *assertionTerminatorMessage(Cctrl *cc, lexeme *tok, long terminator_flags) {
    if ((terminator_flags & PUNCT_TERM_SEMI) &&
        (terminator_flags & PUNCT_TERM_COMMA)) {
        return "Expected terminating ';' or ','";
    } else if ((terminator_flags & PUNCT_TERM_SEMI)) {
        return "Expected terminating ';'";
    } else if ((terminator_flags & PUNCT_TERM_COMMA)) {
        return "Expected terminating ','";
    } else if (terminator_flags & PUNCT_TERM_RPAREN) {
        return "Expected terminating ')'";
    } else {
        cctrlIce(cc,"Expected terminating token with flags: 0x%lX, got: %s",
                terminator_flags, lexemeToString(tok));
    }
}

/* Check if one of the characters matches and the flag wants that character to
 * terminate */
void assertTokenIsTerminator(Cctrl *cc, lexeme *tok, long terminator_flags) {
    if (tok == NULL) {
        cctrlRaiseException(cc,"NULL token passed to assertTokenIsTerminator");
    }

    if ((tok->i64 == ';' && (terminator_flags & PUNCT_TERM_SEMI)) ||
        (tok->i64 == ')' && (terminator_flags & PUNCT_TERM_RPAREN)) ||
        (tok->i64 == ',' && (terminator_flags & PUNCT_TERM_COMMA))) {
        return;
    }

    cctrlTokenRewind(cc);
    cctrlTokenRewind(cc);
    tok = cctrlTokenPeek(cc);
    cctrlRaiseException(cc, assertionTerminatorMessage(cc,tok,terminator_flags));
}

void assertTokenIsTerminatorWithMsg(Cctrl *cc, lexeme *tok,
        long terminator_flags, const char *fmt, ...)
{
    if ((tok->i64 == ';' && (terminator_flags & PUNCT_TERM_SEMI)) ||
        (tok->i64 == ')' && (terminator_flags & PUNCT_TERM_RPAREN)) ||
        (tok->i64 == ',' && (terminator_flags & PUNCT_TERM_COMMA))) {
        return;
    }

    va_list ap;
    va_start(ap,fmt);
    char *msg = mprintVa(fmt, ap);
    va_end(ap);

    if (tok == NULL) {
        cctrlRaiseException(cc,"NULL token passed to assertTokenIsTerminator");
    }

    cctrlTokenRewind(cc);
    cctrlTokenRewind(cc);
    tok = cctrlTokenPeek(cc);

    char *token_msg = assertionTerminatorMessage(cc,tok,terminator_flags);
    cctrlRaiseException(cc,"%s - %s",token_msg,msg);
    free(msg);
}

AstType *parseGetType(Cctrl *cc, lexeme *tok) {
    if (!tok) {
        return NULL;
    }
    if (tok->tk_type != TK_IDENT && tok->tk_type != TK_KEYWORD) {
        return NULL;
    }
    return cctrlGetKeyWord(cc,tok->start,tok->len);
}

int parseIsKeyword(lexeme *tok, Cctrl *cc) {
    return cctrlIsKeyword(cc,tok->start,tok->len) || NULL;
}

int evalClassRef(Ast *ast, int offset) {
    if (ast->kind == AST_CLASS_REF)
        return evalClassRef(ast->cls, ast->type->offset + offset);
    return evalIntConstExpr(ast) + offset;
}

int astIsArithmetic(long op, int is_float) {
    switch (op) {
        case '+':
        case '-':
        case '*':
        case '/':
            return 1;
        case '%':
        case TK_SHL:
        case TK_SHR:
        case '&':
        case '|':
        case '^':
            return !is_float;
        default:
            return 0;
    }
}

int astCanEval(Ast *ast, int *_ok, int is_float) {
    if (ast->left && ast->right) {
        return astCanEval(ast->left, _ok,is_float) && 
               astCanEval(ast->right, _ok,is_float);
    } else if (astIsArithmetic(ast->kind, is_float)) {
        return 1;
    } else if (ast->kind == AST_LITERAL && (astIsIntType(ast->type) || astIsFloatType(ast->type))) {
        return 1;
    }
    *_ok = 0;
    return 0;
}

double evalFloatExprOrErr(Ast *ast, int *_ok) {
    switch (ast->kind) {
    case AST_LITERAL:
        if (astIsFloatType(ast->type)) {
            return ast->f64;
        } else if (astIsIntType(ast->type)) {
            return (double)ast->i64;
        }
    case '+': return evalFloatExprOrErr(ast->left, _ok) + evalFloatExprOrErr(ast->right, _ok);
    case '-': {
        if (ast->right == NULL && ast->operand != NULL) {
            return -ast->operand->f64;
        }
        return evalFloatExprOrErr(ast->left, _ok) - evalFloatExprOrErr(ast->right, _ok);
    }
    case '/': return evalFloatExprOrErr(ast->left, _ok) / evalFloatExprOrErr(ast->right, _ok);
    case '*': return evalFloatExprOrErr(ast->left, _ok) * evalFloatExprOrErr(ast->right, _ok);
    case TK_EQU_EQU: return evalFloatExprOrErr(ast->left, _ok) == evalFloatExprOrErr(ast->right, _ok);
    case TK_GREATER_EQU: return evalFloatExprOrErr(ast->left, _ok) >= evalFloatExprOrErr(ast->right, _ok);
    case TK_LESS_EQU: return evalFloatExprOrErr(ast->left, _ok) <= evalFloatExprOrErr(ast->right, _ok);
    case TK_NOT_EQU: return evalFloatExprOrErr(ast->left, _ok) != evalFloatExprOrErr(ast->right, _ok);
    case TK_OR_OR: return evalFloatExprOrErr(ast->left, _ok) || evalFloatExprOrErr(ast->right, _ok);
    case TK_AND_AND: return evalFloatExprOrErr(ast->left, _ok) && evalFloatExprOrErr(ast->right, _ok);
    default: {
        *_ok = 0;
        return 0;
    }
    }
}

double evalFloatExpr(Ast *ast) {
    int ok = 1;
    double result = evalFloatExprOrErr(ast,&ok);
    if (!ok) {
        loggerPanic("Expected float expression: %s\n", astToString(ast));
    }
    return result;
}

double evalFloatArithmeticOrErr(Ast *ast, int *_ok) {
    if (!astCanEval(ast,_ok,1)) {
        *_ok = 0;
        return 0.0;
    }
    switch (ast->kind) {
    case AST_LITERAL:
        if (astIsFloatType(ast->type)) {
            return ast->f64;
        } else if (astIsIntType(ast->type)) {
            return (double)ast->i64;
        }
    case '+': return evalFloatExprOrErr(ast->left, _ok) + evalFloatExprOrErr(ast->right, _ok);
    case '-': {
        if (ast->right == NULL && ast->operand != NULL) {
            return -ast->operand->f64;
        }
        return evalFloatExprOrErr(ast->left, _ok) - evalFloatExprOrErr(ast->right, _ok);
    }
    case '/': return evalFloatExprOrErr(ast->left, _ok) / evalFloatExprOrErr(ast->right, _ok);
    case '*': return evalFloatExprOrErr(ast->left, _ok) * evalFloatExprOrErr(ast->right, _ok);
    default: {
        *_ok = 0;
        return 0;
    }
    }
}

double evalOneFloatExprOrErr(Ast *LHS, Ast *RHS, long op, int *_ok) {
    if (LHS->kind == AST_LITERAL && RHS->kind == AST_LITERAL) {
        ssize_t result = 0;
        ssize_t left = astIsIntType(LHS->type) ? (double)LHS->i64 : LHS->f64;
        ssize_t right = astIsIntType(RHS->type) ? (double)RHS->i64 : LHS->f64;
        switch(op) {
            case '+':    result = left + right; break;
            case '-':    result = left - right; break;
            case '*':    result = left * right; break;
            case '/':    result = left / right; break;
            default:
                loggerPanic("Invalid operator: '%s'\n",
                        astKindToString(op));
        }
        *_ok = 1;
        return result;
    }
    *_ok = 0;
    return 0;
}

long evalIntArithmeticOrErr(Ast *ast, int *_ok) {
    if (!astCanEval(ast,_ok,0)) {
        *_ok = 0;
        return 0.0;
    }

    switch (ast->kind) {
    case AST_LITERAL:
        if (astIsIntType(ast->type)) {
            return ast->i64;
        } else if (astIsFloatType(ast->type)) {
            return (long)ast->f64;
        }
    case '+': return evalIntArithmeticOrErr(ast->left, _ok) + evalIntArithmeticOrErr(ast->right, _ok);
    case '-': {
        return evalIntArithmeticOrErr(ast->left, _ok) - evalIntArithmeticOrErr(ast->right, _ok);
    }
    case TK_SHL: return evalIntArithmeticOrErr(ast->left, _ok) << evalIntArithmeticOrErr(ast->right, _ok);
    case TK_SHR: return evalIntArithmeticOrErr(ast->left, _ok) >> evalIntArithmeticOrErr(ast->right, _ok);
    case '&': return evalIntArithmeticOrErr(ast->left, _ok) & evalIntArithmeticOrErr(ast->right, _ok);
    case '|': return evalIntArithmeticOrErr(ast->left, _ok) | evalIntArithmeticOrErr(ast->right, _ok);
    case '^': return evalIntArithmeticOrErr(ast->left, _ok) ^ evalIntArithmeticOrErr(ast->right, _ok);
    case '*': return evalIntArithmeticOrErr(ast->left, _ok) * evalIntArithmeticOrErr(ast->right, _ok);
    case '/': return evalIntArithmeticOrErr(ast->left, _ok) / evalIntArithmeticOrErr(ast->right, _ok);
    case '%': return evalIntArithmeticOrErr(ast->left, _ok) % evalIntArithmeticOrErr(ast->right, _ok);
    default: {
        *_ok = 0;
        return 0;
    }
    }
}

long evalOneIntExprOrErr(Ast *LHS, Ast *RHS, long op, int *_ok) {
    if (LHS->kind == AST_LITERAL && RHS->kind == AST_LITERAL) {
        ssize_t left =  astIsIntType(LHS->type) ? LHS->i64 : (ssize_t)LHS->f64;
        ssize_t right = astIsIntType(RHS->type) ? RHS->i64 : (ssize_t)LHS->f64;
        ssize_t result = 0;
        switch(op) {
            case '+':    result = left + right; break;
            case '-':    result = left - right; break;
            case '*':    result = left * right; break;
            case '^':    result = left ^ right; break;
            case '&':    result = left & right; break;
            case '|':    result = left | right; break;
            case TK_SHL: result = left << right; break;
            case TK_SHR: result = left >> right; break;
            case '/':    result = left / right; break;
            case '%':    result = left % right; break;
            default:
                loggerPanic("Invalid operator: '%s'\n",
                        astKindToString(op));
        }
        *_ok = 1;
        return result;
    }
    *_ok = 0;
    return 0;
}

long evalIntConstExprOrErr(Ast *ast, int *_ok) {
    switch (ast->kind) {
    case AST_LITERAL:
        if (astIsIntType(ast->type)) {
            return ast->i64;
        } else if (astIsFloatType(ast->type)) {
            return (long)ast->f64;
        }
    case AST_ADDR:
        if (ast->operand->kind == AST_CLASS_REF) {
            return evalClassRef(ast->operand, 0);
        }
    case AST_DEREF:
        if (ast->operand->type->kind == AST_TYPE_POINTER) {
            return evalIntConstExprOrErr(ast->operand, _ok);
        }
    case '+': return evalIntConstExprOrErr(ast->left, _ok) + evalIntConstExprOrErr(ast->right, _ok);
    case '-': {
        if (ast->right == NULL && ast->operand != NULL) {
            return -ast->operand->i64;
        }
        return evalIntConstExprOrErr(ast->left, _ok) - evalIntConstExprOrErr(ast->right, _ok);
    }
    case TK_SHL: return evalIntConstExprOrErr(ast->left, _ok) << evalIntConstExprOrErr(ast->right, _ok);
    case TK_SHR: return evalIntConstExprOrErr(ast->left, _ok) >> evalIntConstExprOrErr(ast->right, _ok);
    case '&': return evalIntConstExprOrErr(ast->left, _ok) & evalIntConstExprOrErr(ast->right, _ok);
    case '|': return evalIntConstExprOrErr(ast->left, _ok) | evalIntConstExprOrErr(ast->right, _ok);
    case '^': return evalIntConstExprOrErr(ast->left, _ok) ^ evalIntConstExprOrErr(ast->right, _ok);
    case '*': return evalIntConstExprOrErr(ast->left, _ok) * evalIntConstExprOrErr(ast->right, _ok);
    case '/': return evalIntConstExprOrErr(ast->left, _ok) / evalIntConstExprOrErr(ast->right, _ok);
    case '%': return evalIntConstExprOrErr(ast->left, _ok) % evalIntConstExprOrErr(ast->right, _ok);
    case '~': {
        if (ast->operand != NULL) {
            return ~ast->operand->i64;
        }
    }
    case '!': return !evalIntConstExprOrErr(ast->operand, _ok);
    case TK_EQU_EQU: return evalIntConstExprOrErr(ast->left, _ok) == evalIntConstExprOrErr(ast->right, _ok);
    case TK_GREATER_EQU: return evalIntConstExprOrErr(ast->left, _ok) >= evalIntConstExprOrErr(ast->right, _ok);
    case TK_LESS_EQU: return evalIntConstExprOrErr(ast->left, _ok) <= evalIntConstExprOrErr(ast->right, _ok);
    case TK_NOT_EQU: return evalIntConstExprOrErr(ast->left, _ok) != evalIntConstExprOrErr(ast->right, _ok);
    case TK_OR_OR: return evalIntConstExprOrErr(ast->left, _ok) || evalIntConstExprOrErr(ast->right, _ok);
    case TK_AND_AND: return evalIntConstExprOrErr(ast->left, _ok) && evalIntConstExprOrErr(ast->right, _ok);
    default: {
        *_ok = 0;
        return 0;
    }
    }
}

long evalIntConstExpr(Ast *ast) {
    int ok = 1;
    ssize_t res = evalIntConstExprOrErr(ast,&ok);
    if (!ok) {
        loggerPanic("Expected integer expression: %s\n", astToString(ast));
    }
    return res;
}

void assertLValue(Ast *ast, long lineno) {
    switch (ast->kind) {
    case AST_LVAR:
    case AST_GVAR:
    case AST_DEREF:
    case AST_CLASS_REF:
    case AST_FUNPTR:
    case AST_DEFAULT_PARAM:
    case AST_CAST:
        return;
    default:
        loggerPanic("line %ld: Expected lvalue, got: %s\n",lineno,astToString(ast));
    }
}

void assertUniqueSwitchCaseLabels(PtrVec *case_vector, Ast *case_) {
    for (int i = 0; i < case_vector->size; ++i) {
        Ast *cur = case_vector->entries[i];
        if (case_->case_end < cur->case_begin ||
            cur->case_begin < case_->case_begin) {
            continue;
        }
        if (case_->case_begin == cur->case_end) {
            for (int j = 0; j < case_vector->size; ++j) {
                astPrint(case_vector->entries[i]);
            }
            loggerPanic("Duplicate case value: %ld\n",case_->case_begin);
        }
        loggerPanic("Duplicate case value: %ld\n",case_->case_begin);
    }
}


void repeatRedSquiggles(aoStr *str, int count) {
    aoStrCatLen(str, str_lit(ESC_BOLD_RED));
    aoStrCatRepeat(str, "~", count);
    aoStrCatLen(str, str_lit(ESC_RESET));
}

void typeCheckWarn(Cctrl *cc, long op, Ast *expected, Ast *actual) {
    aoStr *expected_type = astTypeToColorAoStr(expected->type);
    aoStr *actual_type = astTypeToColorAoStr(actual->type);

    cctrlTokenRewind(cc);
    cctrlTokenRewind(cc);
    cctrlWarning(cc, "Incompatible types '%s' is not assignable to type '%s'", 
            actual_type->data, expected_type->data);
    cctrlTokenGet(cc);
    cctrlTokenGet(cc);
    aoStrRelease(expected_type);
    aoStrRelease(actual_type);
}

void typeCheckReturnTypeWarn(Cctrl *cc, long lineno, Ast *maybe_func, 
                             AstType *check, Ast *retval)
{
    char *fstring = NULL;
    if (maybe_func) {
        fstring = astFunctionToString(maybe_func);
    } else {
        fstring = astFunctionNameToString(cc->tmp_rettype,
                cc->tmp_fname->data,cc->tmp_fname->len);
    }

    char *expected = astTypeToColorString(cc->tmp_rettype);
    aoStr *got = astTypeToColorAoStr(check);
    aoStr *ast_str = astLValueToAoStr(retval,0);


    char *msg = mprintf(ESC_BOLD"%s unexpected return value '%s' of type '%s' expected '%s'"ESC_CLEAR_BOLD,
                        fstring,
                        ast_str->data,
                        got->data,
                        expected);

    lexeme *peek = cctrlTokenPeek(cc);
    while (peek->len != 6 && memcmp(peek->start,str_lit("return"))) {
        cctrlTokenRewind(cc);
        peek = cctrlTokenPeek(cc);
    }
    //cctrlTokenGet(cc);
    cctrlRaiseException(cc, msg);
    free(msg);
    cctrlTokenGet(cc);
}
