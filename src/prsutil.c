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

inline int ParseIsFloatOrInt(Ast *ast) {
    return AstIsIntType(ast->type) || 
           AstIsFloatType(ast->type);
}

inline int ParseIsClassOrUnion(int kind) {
    return kind == AST_TYPE_CLASS || 
           kind == AST_TYPE_UNION;
}

inline int ParseIsFunction(Ast *ast) {
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

inline int ParseIsFunctionCall(Ast *ast) {
    return ast && (ast->type->kind == AST_FUNCALL || 
           ast->type->kind == AST_FUNPTR_CALL || 
           ast->type->kind == AST_ASM_FUNCALL);
}

void AssertIsFloat(Ast *ast, long lineno) {
    if (ast && !AstIsFloatType(ast->type)) {
        loggerPanic("line %ld: Expected float type got %s\n",
                lineno,AstTypeToString(ast->type));
    }
}

void AssertIsInt(Ast *ast, long lineno) {
    if (ast && !AstIsIntType(ast->type)) {
        loggerPanic("line %ld: Expected int type got %s\n",
                lineno, AstTypeToString(ast->type));
    }
}

void AssertIsFloatOrInt(Ast *ast, long lineno) {
    if (!ParseIsFloatOrInt(ast)) {
        loggerPanic("line %ld: Expected float got %s\n",
                lineno, AstTypeToString(ast->type));
    }
}

void AssertIsPointer(Ast *ast, long lineno) {
    if (!ast || ast->type->kind != AST_TYPE_POINTER) {
        loggerPanic("line %ld: Expected float got %s\n",
                lineno,AstTypeToString(ast->type));
    }
}

/* Check if one of the characters matches and the flag wants that character to
 * terminate */
void AssertTokenIsTerminator(lexeme *tok, long terminator_flags) {
    if (tok == NULL) {
        loggerPanic("NULL token passed to AssertTokenIsTerminator\n");
    }

    if (tok->tk_type != TK_PUNCT) {
        loggerPanic("line %d: Expected token of type TK_PUNCT, got type: %s\n",
                tok->line,lexemeTypeToString(tok->tk_type));
    }

    if ((tok->i64 == ';' && (terminator_flags & PUNCT_TERM_SEMI)) ||
        (tok->i64 == ')' && (terminator_flags & PUNCT_TERM_RPAREN)) ||
        (tok->i64 == ',' && (terminator_flags & PUNCT_TERM_COMMA))) {
        return;
    }

    if ((terminator_flags & PUNCT_TERM_SEMI) &&
        (terminator_flags & PUNCT_TERM_COMMA)) {
        loggerPanic("line %d: Expected ';' or ',' got: %s\n",
                tok->line, lexemePunctToString(tok->i64));
    } else if ((terminator_flags & PUNCT_TERM_SEMI)) {
        loggerPanic("line %d: Expected ';' got: %s\n",
                tok->line, lexemePunctToString(tok->i64));
    } else if ((terminator_flags & PUNCT_TERM_COMMA)) {
        loggerPanic("line %d: Expected ',' got: %s\n",
                tok->line, lexemePunctToString(tok->i64));
    } else if (terminator_flags & PUNCT_TERM_RPAREN) {
        loggerPanic("line %d: Expected ')' got: %s\n",
                tok->line, lexemePunctToString(tok->i64));
    } else {
        loggerPanic("line %d: Expected terminating token with flags: 0x%lX, got: %s\n",
                tok->line, terminator_flags, lexemeToString(tok));
    }
}

AstType *ParseGetType(Cctrl *cc, lexeme *tok) {
    if (!tok) {
        return NULL;
    }
    if (tok->tk_type != TK_IDENT && tok->tk_type != TK_KEYWORD) {
        return NULL;
    }
    return CctrlGetKeyWord(cc,tok->start,tok->len);
}

int ParseIsKeyword(lexeme *tok, Cctrl *cc) {
    return CctrlIsKeyword(cc,tok->start,tok->len) || NULL;
}

int EvalClassRef(Ast *ast, int offset) {
    if (ast->kind == AST_CLASS_REF)
        return EvalClassRef(ast->cls, ast->type->offset + offset);
    return EvalIntConstExpr(ast) + offset;
}

double EvalFloatExpr(Ast *ast) {
    switch (ast->kind) {
    case AST_LITERAL:
        if (AstIsFloatType(ast->type)) {
            return ast->f64;
        } else if (AstIsIntType(ast->type)) {
            return (double)ast->i64;
        }
    case '+': return EvalFloatExpr(ast->left) + EvalFloatExpr(ast->right);
    case '-': {
        if (ast->right == NULL && ast->operand != NULL) {
            return -ast->operand->f64;
        }
        return EvalFloatExpr(ast->left) - EvalFloatExpr(ast->right);
    }
    case '/': return EvalFloatExpr(ast->left) / EvalFloatExpr(ast->right);
    case '*': return EvalFloatExpr(ast->left) * EvalFloatExpr(ast->right);
    case TK_EQU_EQU: return EvalFloatExpr(ast->left) == EvalFloatExpr(ast->right);
    case TK_GREATER_EQU: return EvalFloatExpr(ast->left) >= EvalFloatExpr(ast->right);
    case TK_LESS_EQU: return EvalFloatExpr(ast->left) <= EvalFloatExpr(ast->right);
    case TK_NOT_EQU: return EvalFloatExpr(ast->left) != EvalFloatExpr(ast->right);
    case TK_OR_OR: return EvalFloatExpr(ast->left) || EvalFloatExpr(ast->right);
    case TK_AND_AND: return EvalFloatExpr(ast->left) && EvalFloatExpr(ast->right);
    default:
        loggerPanic("%s is an invalid floating point constant expression operator\n",
                AstKindToString(ast->kind));
    }
}

long EvalIntConstExpr(Ast *ast) {
    switch (ast->kind) {
    case AST_LITERAL:
        if (AstIsIntType(ast->type)) {
            return ast->i64;
        } else if (AstIsFloatType(ast->type)) {
            return (long)ast->f64;
        }
    case AST_ADDR:
        if (ast->operand->kind == AST_CLASS_REF) {
            return EvalClassRef(ast->operand, 0);
        }
    case AST_DEREF:
        if (ast->operand->type->kind == AST_TYPE_POINTER) {
            return EvalIntConstExpr(ast->operand);
        }
    case '+': return EvalIntConstExpr(ast->left) + EvalIntConstExpr(ast->right);
    case '-': {
        if (ast->right == NULL && ast->operand != NULL) {
            return -ast->operand->i64;
        }
        return EvalIntConstExpr(ast->left) - EvalIntConstExpr(ast->right);
    }
    case TK_SHL: return EvalIntConstExpr(ast->left) << EvalIntConstExpr(ast->right);
    case TK_SHR: return EvalIntConstExpr(ast->left) >> EvalIntConstExpr(ast->right);
    case '&': return EvalIntConstExpr(ast->left) & EvalIntConstExpr(ast->right);
    case '|': return EvalIntConstExpr(ast->left) | EvalIntConstExpr(ast->right);
    case '^': return EvalIntConstExpr(ast->left) ^ EvalIntConstExpr(ast->right);
    case '*': return EvalIntConstExpr(ast->left) * EvalIntConstExpr(ast->right);
    case '/': return EvalIntConstExpr(ast->left) / EvalIntConstExpr(ast->right);
    case '%': return EvalIntConstExpr(ast->left) % EvalIntConstExpr(ast->right);
    case '~': {
        if (ast->operand != NULL) {
            return ~ast->operand->i64;
        }
    }
    case '!': return !EvalIntConstExpr(ast->operand);
    case TK_EQU_EQU: return EvalIntConstExpr(ast->left) == EvalIntConstExpr(ast->right);
    case TK_GREATER_EQU: return EvalIntConstExpr(ast->left) >= EvalIntConstExpr(ast->right);
    case TK_LESS_EQU: return EvalIntConstExpr(ast->left) <= EvalIntConstExpr(ast->right);
    case TK_NOT_EQU: return EvalIntConstExpr(ast->left) != EvalIntConstExpr(ast->right);
    case TK_OR_OR: return EvalIntConstExpr(ast->left) || EvalIntConstExpr(ast->right);
    case TK_AND_AND: return EvalIntConstExpr(ast->left) && EvalIntConstExpr(ast->right);
    default:
        loggerPanic("Expected integer expression: %s\n", AstToString(ast));
    }
}

void AssertLValue(Ast *ast, long lineno) {
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
        loggerPanic("line %ld: Expected lvalue, got: %s\n",lineno,AstToString(ast));
    }
}

void AssertUniqueSwitchCaseLabels(List *case_list, Ast *case_) {
    Ast *cur;
    for (List *it = case_list->next; it != case_list; it = it->next) {
        cur = it->value;
        if (case_->case_end < cur->case_begin ||
            cur->case_begin < case_->case_begin) {
            continue;
        }
        if (case_->case_begin == cur->case_end) {
            loggerPanic("Duplicate case value: %ld\n",case_->case_begin);
        }
        loggerPanic("Duplicate case value: %ld\n",case_->case_begin);
    }
}
