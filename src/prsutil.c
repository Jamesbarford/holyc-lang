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
    return ast && (ast->kind == AST_FUNCALL || 
           ast->kind == AST_FUNPTR_CALL || 
           ast->kind == AST_ASM_FUNCALL);
}

void assertIsFloat(Ast *ast, s64 lineno) {
    if (ast && !astIsFloatType(ast->type)) {
        loggerPanic("line %ld: Expected float type got %s\n",
                lineno,astTypeToString(ast->type));
    }
}

void assertIsInt(Ast *ast, s64 lineno) {
    if (ast && !astIsIntType(ast->type)) {
        loggerPanic("line %ld: Expected int type got %s\n",
                lineno, astTypeToString(ast->type));
    }
}

void assertIsFloatOrInt(Ast *ast, s64 lineno) {
    if (!parseIsFloatOrInt(ast)) {
        loggerPanic("line %ld: Expected float got %s\n",
                lineno, astTypeToString(ast->type));
    }
}

void assertIsPointer(Ast *ast, s64 lineno) {
    if (!ast || ast->type->kind != AST_TYPE_POINTER) {
        loggerPanic("line %ld: Expected float got %s\n",
                lineno,astTypeToString(ast->type));
    }
}

char *assertionTerminatorMessage(Cctrl *cc, Lexeme *tok,
                                 s64 terminator_flags)
{
    if ((terminator_flags & PUNCT_TERM_SEMI) &&
        (terminator_flags & PUNCT_TERM_COMMA)) {
        return "perhaps you meant ';' or ','?";
    } else if ((terminator_flags & PUNCT_TERM_SEMI)) {
        return "perhaps you meant ';' ?";
    } else if ((terminator_flags & PUNCT_TERM_COMMA)) {
        return "perhaps you meant ',' ?";
    } else if (terminator_flags & PUNCT_TERM_RPAREN) {
        return "perhaps you meant ')'";
    } else {
        cctrlIce(cc,"Expected terminating token with flags: 0x%lX, got: %s",
                terminator_flags, lexemeToString(tok));
    }
}

/* Check if one of the characters matches and the flag wants that character to
 * terminate */
void assertTokenIsTerminator(Cctrl *cc, Lexeme *tok, s64 terminator_flags) {
    if (tok == NULL) {
        cctrlRaiseException(cc,"NULL token passed to assertTokenIsTerminator");
    }

    if ((tok->i64 == ';' && (terminator_flags & PUNCT_TERM_SEMI)) ||
        (tok->i64 == ')' && (terminator_flags & PUNCT_TERM_RPAREN)) ||
        (tok->i64 == ',' && (terminator_flags & PUNCT_TERM_COMMA))) {
        return;
    }

    char *suggestion = assertionTerminatorMessage(cc, tok, terminator_flags);

    /* tok has already been consumed by the caller, so `peek` is
     * pointing one past it. Rewind once so the diagnostic
     * renderer sees `tok` as the current position - otherwise
     * cctrlMessagePrintF captures the *next* token's line/col and
     * the underline lands on the wrong thing. */
    cctrlTokenRewind(cc);
    AoStr *error_msg = cctrlMessagePrintF(cc,CCTRL_ERROR,
            "Unexpected %s `%.*s` %s",
            lexemeTypeToString(tok->tk_type),
            tok->len, tok->start,
            suggestion);
    /* Re-consume tok so the buffer head is back where the caller
     * left it before we start hunting for the hint. */
    cctrlTokenGet(cc);

    /* Build a follow-up note pointing at the *previous* line. The
     * statement that almost certainly forgot its terminator and let
     * the next statement collide with our parser. */
    AoStr *info_msg = cctrlInfoAtPreviousLine(cc, tok,
            "perhaps a missing `;` at the end of this line?");

    /* Push diagnostics in source order: the error fires first
     * (rendered at tok's actual position), then the info hint. */ 
    CctrlDiagnostic *err_d = cctrlMakeDiag(cc, CCTRL_ERROR, error_msg, NULL);
    cctrlDiagPush(cc, err_d);
    if (info_msg) {
        CctrlDiagnostic *info_d = cctrlMakeDiag(cc, CCTRL_INFO, info_msg, NULL);
        cctrlDiagPush(cc, info_d);
    }
    cctrlTerminate(cc);
}

void assertTokenIsTerminatorWithMsg(Cctrl *cc, Lexeme *tok,
                                    s64 terminator_flags,
                                    const char *fmt, ...)
{
    if ((tok->i64 == ';' && (terminator_flags & PUNCT_TERM_SEMI)) ||
        (tok->i64 == ')' && (terminator_flags & PUNCT_TERM_RPAREN)) ||
        (tok->i64 == ',' && (terminator_flags & PUNCT_TERM_COMMA))) {
        return;
    }

    va_list ap;
    va_start(ap,fmt);
    char *msg = mprintVa(fmt, ap, NULL);
    va_end(ap);

    if (tok == NULL) {
        cctrlRaiseException(cc,"NULL token passed to assertTokenIsTerminator");
    }

    cctrlRewindUntilPunctMatch(cc,tok->i64,NULL);
    char *token_msg = assertionTerminatorMessage(cc,tok,terminator_flags);
    cctrlRaiseException(cc,"Unexpected %s `%.*s` %s - %s",
                        lexemeTypeToString(tok->tk_type),
                        tok->len,tok->start,
                        msg,
                        token_msg);
}

AstType *parseGetType(Cctrl *cc, Lexeme *tok) {
    if (!tok) {
        return NULL;
    }
    if (tok->tk_type != TK_IDENT && tok->tk_type != TK_KEYWORD) {
        return NULL;
    }
    return cctrlGetKeyWord(cc,tok->start,tok->len);
}

int parseIsKeyword(Lexeme *tok, Cctrl *cc) {
    return cctrlIsKeyword(cc,tok->start,tok->len) || NULL;
}

int evalClassRef(Ast *ast, int offset) {
    if (ast->kind == AST_CLASS_REF)
        return evalClassRef(ast->cls, ast->type->offset + offset);
    return evalIntConstExpr(ast) + offset;
}

int astIsArithmetic(Ast *ast, int is_float) {
    if (astIsBinOp(ast)) {
        switch (ast->binop) {
            case AST_BIN_OP_MUL:
            case AST_BIN_OP_DIV:
            case AST_BIN_OP_ADD:
            case AST_BIN_OP_SUB:
                return 1;
            case AST_BIN_OP_MOD:
            case AST_BIN_OP_SHL:
            case AST_BIN_OP_SHR:
            case AST_BIN_OP_BIT_AND:
            case AST_BIN_OP_BIT_XOR:
            case AST_BIN_OP_BIT_OR:
                return !is_float;
            default:
                return 0;
        }
    } 
    return 0;
}

int astCanEval(Ast *ast, int *_ok, int is_float) {
    if (ast->left && ast->right) {
        return astCanEval(ast->left, _ok,is_float) && 
               astCanEval(ast->right, _ok,is_float);
    } else if (astIsArithmetic(ast, is_float)) {
        return 1;
    } else if (ast->kind == AST_LITERAL && (astIsIntType(ast->type) || astIsFloatType(ast->type))) {
        return 1;
    } else {
        *_ok = 0;
        return 0;
    }
}

double evalFloatExprOrErr(Ast *ast, int *_ok) {
#define eval evalFloatExprOrErr
    switch (ast->kind) {
        case AST_LITERAL: {
            if (astIsFloatType(ast->type)) {
                return ast->f64;
            } else if (astIsIntType(ast->type)) {
                return (double)ast->i64;
            } else {
                *_ok = 0;
                return 0;
            }
        }
        case AST_BINOP: {
            switch (ast->binop) {
                case AST_BIN_OP_MUL: return eval(ast->left, _ok) * eval(ast->right, _ok);
                case AST_BIN_OP_DIV: return eval(ast->left, _ok) / eval(ast->right, _ok);
                case AST_BIN_OP_ADD: return eval(ast->left, _ok) + eval(ast->right, _ok);
                case AST_BIN_OP_SUB: return eval(ast->left, _ok) - eval(ast->right, _ok);
                case AST_BIN_OP_LT: return eval(ast->left, _ok) < eval(ast->right, _ok);
                case AST_BIN_OP_LE: return eval(ast->left, _ok) <= eval(ast->right, _ok);
                case AST_BIN_OP_GT: return eval(ast->left, _ok) > eval(ast->right, _ok);
                case AST_BIN_OP_GE: return eval(ast->left, _ok) >= eval(ast->right, _ok);
                case AST_BIN_OP_EQ: return eval(ast->left, _ok) == eval(ast->right, _ok);
                case AST_BIN_OP_NE: return eval(ast->left, _ok) != eval(ast->right, _ok);
                default: {
                    *_ok = 0;
                    return 0;
                }
            }
        }
        default: {
            *_ok = 0;
            return 0;
        }
    }
#undef eval
}

double evalFloatExpr(Ast *ast) {
    int ok = 1;
    double result = evalFloatExprOrErr(ast,&ok);
    if (!ok) {
        loggerPanic("Expected float expression: %s\n", astToString(ast));
    }
    return result;
}

s64 evalIntArithmeticOrErr(Ast *ast, int *_ok) {
#define eval evalIntArithmeticOrErr
    if (!astCanEval(ast,_ok,0)) {
        *_ok = 0;
        return 0.0;
    }

    switch (ast->kind) {
        case AST_LITERAL: {
            if (astIsIntType(ast->type)) {
                return ast->i64;
            } else if (astIsFloatType(ast->type)) {
                return (long)ast->f64;
            } else {
                *_ok = 0;
                return 0;
            }
        }

        case AST_BINOP:
            switch (ast->binop) {
                case AST_BIN_OP_MUL: return eval(ast->left, _ok) * eval(ast->right, _ok);
                case AST_BIN_OP_DIV: return eval(ast->left, _ok) / eval(ast->right, _ok);
                case AST_BIN_OP_MOD: return eval(ast->left, _ok) % eval(ast->right, _ok);
                case AST_BIN_OP_ADD: return eval(ast->left, _ok) + eval(ast->right, _ok);
                case AST_BIN_OP_SUB: return eval(ast->left, _ok) - eval(ast->right, _ok);
                case AST_BIN_OP_SHL: return eval(ast->left, _ok) << eval(ast->right, _ok);
                case AST_BIN_OP_SHR: return eval(ast->left, _ok) >> eval(ast->right, _ok);
                case AST_BIN_OP_BIT_AND: return eval(ast->left, _ok) & eval(ast->right, _ok);
                case AST_BIN_OP_BIT_XOR: return eval(ast->left, _ok) ^ eval(ast->right, _ok);
                case AST_BIN_OP_BIT_OR: return eval(ast->left, _ok) | eval(ast->right, _ok);
                default: {
                    *_ok = 0;
                    return 0;
                }
            }
            break;
        default: {
            *_ok = 0;
            return 0;
        }
    }
#undef eval
}

s64 evalOneIntExprOrErr(Ast *LHS, Ast *RHS, AstBinOp op, int *_ok) {
    if (LHS->kind == AST_LITERAL && RHS->kind == AST_LITERAL) {
        s64 left =  astIsIntType(LHS->type) ? LHS->i64 : (ssize_t)LHS->f64;
        s64 right = astIsIntType(RHS->type) ? RHS->i64 : (ssize_t)LHS->f64;
        s64 result = 0;
        switch (op) {
            case AST_BIN_OP_ADD:  result = left + right; break;
            case AST_BIN_OP_SUB:  result = left - right; break;
            case AST_BIN_OP_MUL:  result = left * right; break;
            case AST_BIN_OP_DIV:  result = left / right; break;
            case AST_BIN_OP_MOD:  result = left % right; break;
            case AST_BIN_OP_BIT_XOR:  result = left ^ right; break;
            case AST_BIN_OP_BIT_AND:  result = left & right; break;
            case AST_BIN_OP_BIT_OR:   result = left | right; break;
            case AST_BIN_OP_SHL:  result = left << right; break;
            case AST_BIN_OP_SHR: result = left >> right; break;
            default:
                loggerPanic("Invalid operator: '%s'\n",
                        astBinOpKindToString(op));
        }
        *_ok = 1;
        return result;
    }
    *_ok = 0;
    return 0;
}

s64 evalIntConstExprOrErr(Ast *ast, int *_ok) {
    switch (ast->kind) {
        case AST_LITERAL: {
            if (astIsIntType(ast->type)) {
                return ast->i64;
            } else if (astIsFloatType(ast->type)) {
                return (long)ast->f64;
            } else {
                *_ok = 0;
                return 0;
            }
        }
        case AST_UNOP: {
            switch (ast->unop) {
                case AST_UN_OP_PLUS: return +(evalIntConstExprOrErr(ast->operand, _ok));
                case AST_UN_OP_MINUS: return -(evalIntConstExprOrErr(ast->operand, _ok));
                case AST_UN_OP_LOG_NOT: return !(evalIntConstExprOrErr(ast->operand, _ok));
                case AST_UN_OP_BIT_NOT: return ~(evalIntConstExprOrErr(ast->operand, _ok));
                case AST_UN_OP_ADDR_OF:
                    if (ast->operand->kind == AST_CLASS_REF) {
                        return evalClassRef(ast->operand, 0);
                    }
                    break;
                default: {
                    *_ok = 0;
                    return 0;
                }
            }
            break;
        }

        case AST_BINOP:
            switch (ast->binop) {
                case AST_BIN_OP_MUL:
                    return evalIntConstExprOrErr(ast->left, _ok) *
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_DIV:
                    return evalIntConstExprOrErr(ast->left, _ok) /
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_MOD:
                    return evalIntConstExprOrErr(ast->left, _ok) %
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_ADD:
                    return evalIntConstExprOrErr(ast->left, _ok) +
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_SUB:
                    return evalIntConstExprOrErr(ast->left, _ok) -
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_SHL:
                    return evalIntConstExprOrErr(ast->left, _ok) <<
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_SHR:
                    return evalIntConstExprOrErr(ast->left, _ok) >>
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_LT:
                    return evalIntConstExprOrErr(ast->left, _ok) <
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_LE:
                    return evalIntConstExprOrErr(ast->left, _ok) <=
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_GT:
                    return evalIntConstExprOrErr(ast->left, _ok) >
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_GE:
                    return evalIntConstExprOrErr(ast->left, _ok) >=
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_EQ:
                    return evalIntConstExprOrErr(ast->left, _ok) ==
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_NE:
                    return evalIntConstExprOrErr(ast->left, _ok) !=
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_BIT_AND:
                    return evalIntConstExprOrErr(ast->left, _ok) &
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_BIT_XOR:
                    return evalIntConstExprOrErr(ast->left, _ok) ^
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_BIT_OR:
                    return evalIntConstExprOrErr(ast->left, _ok) |
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_LOG_AND:
                    return evalIntConstExprOrErr(ast->left, _ok) &&
                        evalIntConstExprOrErr(ast->right, _ok);
                case AST_BIN_OP_LOG_OR:
                    return evalIntConstExprOrErr(ast->left, _ok) ||
                        evalIntConstExprOrErr(ast->right, _ok);
                default: {
                    *_ok = 0;
                    return 0;
                }
            }
            break;
        default: {
            *_ok = 0;
            return 0;
        }
    }
    *_ok = 0;
    return 0;
}

s64 evalIntConstExpr(Ast *ast) {
    int ok = 1;
    s64 res = evalIntConstExprOrErr(ast,&ok);
    if (!ok) {
        loggerPanic("Expected integer expression: %s\n", astToString(ast));
    }
    return res;
}

int assertLValue(Ast *ast) {
    switch (ast->kind) {
        case AST_LVAR:
        case AST_GVAR:
        case AST_CLASS_REF:
        case AST_FUNPTR:
        case AST_DEFAULT_PARAM:
        case AST_CAST:
        case AST_UNOP:
            return 1;

        default:
            return 0;
    }
}

void assertUniqueSwitchCaseLabels(Vec *case_vector, Ast *case_) {
    for (u64 i = 0; i < case_vector->size; ++i) {
        Ast *cur = case_vector->entries[i];
        if (case_->case_end < cur->case_begin ||
            cur->case_begin < case_->case_begin) {
            continue;
        }
        if (case_->case_begin == cur->case_end) {
            for (u64 j = 0; j < case_vector->size; ++j) {
                astPrint(case_vector->entries[i]);
            }
            loggerPanic("Duplicate case value: %ld\n",case_->case_begin);
        }
        loggerPanic("Duplicate case value: %ld\n",case_->case_begin);
    }
}

void typeCheckWarn(Cctrl *cc, s64 op, Ast *expected, Ast *actual) {
    AoStr *expected_type = astTypeToColorAoStr(expected->type);
    AoStr *actual_type = astTypeToColorAoStr(actual->type);
    char *actual_str = astToString(actual);

    int count = 0;
    cctrlRewindUntilPunctMatch(cc,op,&count);
    cctrlTokenGet(cc);
    count--;

    char *suggestion = mprintf("%s is not of type %s, perhaps change the type to %s?",
            actual_str,
            expected_type->data,
            actual_type->data);
    cctrlWarningFromTo(cc, suggestion, op, ';', "Incompatible types '%s' is not assignable to type '%s'",
            actual_type->data, expected_type->data);
    for (int i = 0; i < count; ++i) {
        cctrlTokenGet(cc);
    }
    aoStrRelease(expected_type);
    aoStrRelease(actual_type);
}

void typeCheckReturnTypeWarn(Cctrl *cc, Ast *maybe_func, 
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
    AoStr *got = astTypeToColorAoStr(check);
    AoStr *ast_str = astLValueToAoStr(retval,0);


    char *msg = mprintf(ESC_BOLD"%s unexpected return value '%s' of type '%s' expected '%s'"ESC_CLEAR_BOLD,
                        fstring,
                        ast_str->data,
                        got->data,
                        expected);

    int count = 0;
    cctrlRewindUntilStrMatch(cc,str_lit("return"),&count);
    cctrlWarning(cc, msg);
    for (int i = 0; i < count; ++i) {
        cctrlTokenGet(cc);
    }
}
