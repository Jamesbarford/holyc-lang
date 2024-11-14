#include <string.h>
#include "ast.h"
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

/* Check if one of the characters matches and the flag wants that character to
 * terminate */
void assertTokenIsTerminator(Cctrl *cc, lexeme *tok, long terminator_flags) {
    if (tok == NULL) {
        cctrlRaiseException(cc,"NULL token passed to assertTokenIsTerminator");
    }

    if (tok->tk_type != TK_PUNCT) {
        cctrlTokenRewind(cc);
        cctrlTokenRewind(cc);
        cctrlRaiseException(cc,"Expected token of type TK_PUNCT, got type: %s",
                lexemeTypeToString(tok->tk_type));
    }

    if ((tok->i64 == ';' && (terminator_flags & PUNCT_TERM_SEMI)) ||
        (tok->i64 == ')' && (terminator_flags & PUNCT_TERM_RPAREN)) ||
        (tok->i64 == ',' && (terminator_flags & PUNCT_TERM_COMMA))) {
        return;
    }

    if ((terminator_flags & PUNCT_TERM_SEMI) &&
        (terminator_flags & PUNCT_TERM_COMMA)) {
        cctrlRaiseException(cc,"Expected ';' or ',' got: %s",
                lexemePunctToString(tok->i64));
    } else if ((terminator_flags & PUNCT_TERM_SEMI)) {
        cctrlRaiseException(cc,"Expected ';' got: %s",
                lexemePunctToString(tok->i64));
    } else if ((terminator_flags & PUNCT_TERM_COMMA)) {
        cctrlRaiseException(cc,"Expected ',' got: %s",
                lexemePunctToString(tok->i64));
    } else if (terminator_flags & PUNCT_TERM_RPAREN) {
        cctrlRaiseException(cc,"Expected ')' got: %s",
                lexemePunctToString(tok->i64));
    } else {
        cctrlRaiseException(cc,"Expected terminating token with flags: 0x%lX, got: %s",
                terminator_flags, lexemeToString(tok));
    }
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

double evalFloatExpr(Ast *ast) {
    switch (ast->kind) {
    case AST_LITERAL:
        if (astIsFloatType(ast->type)) {
            return ast->f64;
        } else if (astIsIntType(ast->type)) {
            return (double)ast->i64;
        }
    case '+': return evalFloatExpr(ast->left) + evalFloatExpr(ast->right);
    case '-': {
        if (ast->right == NULL && ast->operand != NULL) {
            return -ast->operand->f64;
        }
        return evalFloatExpr(ast->left) - evalFloatExpr(ast->right);
    }
    case '/': return evalFloatExpr(ast->left) / evalFloatExpr(ast->right);
    case '*': return evalFloatExpr(ast->left) * evalFloatExpr(ast->right);
    case TK_EQU_EQU: return evalFloatExpr(ast->left) == evalFloatExpr(ast->right);
    case TK_GREATER_EQU: return evalFloatExpr(ast->left) >= evalFloatExpr(ast->right);
    case TK_LESS_EQU: return evalFloatExpr(ast->left) <= evalFloatExpr(ast->right);
    case TK_NOT_EQU: return evalFloatExpr(ast->left) != evalFloatExpr(ast->right);
    case TK_OR_OR: return evalFloatExpr(ast->left) || evalFloatExpr(ast->right);
    case TK_AND_AND: return evalFloatExpr(ast->left) && evalFloatExpr(ast->right);
    default:
        loggerPanic("%s is an invalid floating point constant expression operator\n",
                astKindToString(ast->kind));
    }
}

long evalIntConstExpr(Ast *ast) {
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
            return evalIntConstExpr(ast->operand);
        }
    case '+': return evalIntConstExpr(ast->left) + evalIntConstExpr(ast->right);
    case '-': {
        if (ast->right == NULL && ast->operand != NULL) {
            return -ast->operand->i64;
        }
        return evalIntConstExpr(ast->left) - evalIntConstExpr(ast->right);
    }
    case TK_SHL: return evalIntConstExpr(ast->left) << evalIntConstExpr(ast->right);
    case TK_SHR: return evalIntConstExpr(ast->left) >> evalIntConstExpr(ast->right);
    case '&': return evalIntConstExpr(ast->left) & evalIntConstExpr(ast->right);
    case '|': return evalIntConstExpr(ast->left) | evalIntConstExpr(ast->right);
    case '^': return evalIntConstExpr(ast->left) ^ evalIntConstExpr(ast->right);
    case '*': return evalIntConstExpr(ast->left) * evalIntConstExpr(ast->right);
    case '/': return evalIntConstExpr(ast->left) / evalIntConstExpr(ast->right);
    case '%': return evalIntConstExpr(ast->left) % evalIntConstExpr(ast->right);
    case '~': {
        if (ast->operand != NULL) {
            return ~ast->operand->i64;
        }
    }
    case '!': return !evalIntConstExpr(ast->operand);
    case TK_EQU_EQU: return evalIntConstExpr(ast->left) == evalIntConstExpr(ast->right);
    case TK_GREATER_EQU: return evalIntConstExpr(ast->left) >= evalIntConstExpr(ast->right);
    case TK_LESS_EQU: return evalIntConstExpr(ast->left) <= evalIntConstExpr(ast->right);
    case TK_NOT_EQU: return evalIntConstExpr(ast->left) != evalIntConstExpr(ast->right);
    case TK_OR_OR: return evalIntConstExpr(ast->left) || evalIntConstExpr(ast->right);
    case TK_AND_AND: return evalIntConstExpr(ast->left) && evalIntConstExpr(ast->right);
    default:
        loggerPanic("Expected integer expression: %s\n", astToString(ast));
    }
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
    aoStr *expected_type_color = astTypeToColorAoStr(expected->type);
    aoStr *expected_type = astTypeToAoStr(expected->type);
    aoStr *actual_type = astTypeToAoStr(actual->type);
    aoStr *expected_variable = astLValueToAoStr(expected,0);
    aoStr *actual_variable = astLValueToAoStr(actual,0);

    char *operation = lexemePunctToString(op);
    if (operation[0] != '=') {
        operation[1] = '=';
    }
    
    aoStr *str = aoStrNew();
    aoStrCatPrintf(str, "%4ld |    %s", cc->lineno, expected_type_color->data);

    int ends_with_star = expected_type->data[expected_type->len - 1] == '*';
    if (!ends_with_star) {
        aoStrPutChar(str, ' ');
    }

    aoStrCatAoStr(str, expected_variable);
    aoStrCatPrintf(str, " %s ", operation);

    aoStrCatLen(str, str_lit(ESC_BOLD_RED));
    aoStrCatAoStr(str, actual_variable);
    aoStrCatLen(str, str_lit(ESC_RESET));

    aoStrCat(str, ";\n");
    aoStrCatLen(str, str_lit("     |    "));
    for (int i = 0; i < expected_type->len; ++i) {
        aoStrPutChar(str, ' ');
    }
    if (!ends_with_star) {
        aoStrPutChar(str, ' ');
    }

    aoStrCatLen(str, str_lit(ESC_GREEN));
    aoStrPutChar(str, '^');
    aoStrCatLen(str, str_lit(ESC_RESET));
    aoStrCatRepeat(str, " ", expected_variable->len+1+strlen(operation));

    repeatRedSquiggles(str,actual_variable->len);

    loggerWarning("line %ld: Incompatible types '%s' is not assignable to type '%s'\n%s\n",
            cc->lineno, actual_type->data, expected_type->data,str->data);

    aoStrRelease(expected_variable);
    aoStrRelease(actual_variable);
    aoStrRelease(actual_type);
    aoStrRelease(expected_type);
    aoStrRelease(expected_type_color);
    aoStrRelease(str);
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

    aoStr *str = aoStrNew();

    aoStrCatPrintf(str, "     | %s { \n", fstring);
    aoStrCatLen(str, str_lit("     |    ... \n"));
    aoStrCatPrintf(str, "%4ld |    return ", cc->lineno);
    
    aoStrCatLen(str, str_lit(ESC_BOLD_RED));
    aoStrCatAoStr(str, ast_str);
    aoStrCatLen(str, str_lit(ESC_RESET));

    aoStrCatLen(str, str_lit(";\n"));
    aoStrCatLen(str, str_lit("     | }  "));

    aoStrCatRepeat(str, " ", 7);
    repeatRedSquiggles(str,ast_str->len);


    loggerWarning("line %ld: %s unexpected return value '%s' of type '%s' expected '%s'\n%s\n",
            lineno,fstring,ast_str->data,got->data,expected,str->data);
    
    free(fstring);
    free(expected);
    aoStrRelease(ast_str);
    aoStrRelease(got);
    aoStrRelease(str);
}
