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
/* Can handle matching ';', '(', ',', '(', '->', '.', '[', ']' */
int TokenMatchFlag(lexeme *tok, long flags) {
    if (tok == NULL) {
        loggerPanic("NULL token passed to TokenMatchFlag\n");
    }

    if ((tok->i64 == ';' &&      (flags & PUNCT_TERM_SEMI))   ||
        (tok->i64 == ',' &&      (flags & PUNCT_TERM_COMMA))  ||
        (tok->i64 == ')' &&      (flags & PUNCT_TERM_RPAREN)) ||
        (tok->i64 == '(' &&      (flags & PUNCT_TERM_LPAREN)) ||
        (tok->i64 == TK_ARROW && (flags & PUNCT_TERM_ARROW))  ||
        (tok->i64 == '.' &&      (flags & PUNCT_TERM_DOT))    ||
        (tok->i64 == '[' &&      (flags & PUNCT_TERM_LSQR))   ||
        (tok->i64 == ']' &&      (flags & PUNCT_TERM_RSQR))) {
        return 1;
    }
    return 0;
}

inline int ParseIsFloatOrInt(Ast *ast) {
    return AstIsIntType(ast->type) || 
           AstIsFloatType(ast->type);
}

inline int ParseIsClassOrUnion(int kind) {
    return kind == AST_TYPE_CLASS || 
           kind == AST_TYPE_UNION;
}

inline int ParseIsFunctionCall(Ast *ast) {
    return ast && (ast->type->kind == AST_FUNCALL || 
           ast->type->kind == AST_FUNPTR_CALL || 
           ast->type->kind == AST_ASM_FUNCALL);
}

void AssertIsFloat(Ast *ast, long lineno) {
    if (ast && !AstIsFloatType(ast->type)) {
        loggerPanic("Expected float type got %s at line %ld\n",
                AstTypeToString(ast->type),lineno);
    }
}

void AssertIsInt(Ast *ast, long lineno) {
    if (ast && !AstIsIntType(ast->type)) {
        loggerPanic("Expected int type got %s at line %ld\n",
                AstTypeToString(ast->type),lineno);
    }
}

void AssertIsFloatOrInt(Ast *ast, long lineno) {
    if (!ParseIsFloatOrInt(ast)) {
        loggerPanic("Expected float got %s at line %ld\n",
                AstTypeToString(ast->type),lineno);
    }
}

void AssertIsPointer(Ast *ast, long lineno) {
    if (!ast || ast->type->kind != AST_TYPE_POINTER) {
        loggerPanic("Expected float got %s at line %ld\n",
                AstTypeToString(ast->type),lineno);
    }
}

/* Check if one of the characters matches and the flag wants that character to
 * terminate */
void AssertTokenIsTerminator(lexeme *tok, long terminator_flags) {
    if (tok == NULL) {
        loggerPanic("NULL token passed to AssertTokenIsTerminator\n");
    }

    if (tok->tk_type != TK_PUNCT) {
        loggerPanic("Expected token of type TK_PUNCT, got type: %s, line: %d\n",
                lexemeToString(tok),
                tok->line);
    }

    if ((tok->i64 == ';' && (terminator_flags & PUNCT_TERM_SEMI)) ||
        (tok->i64 == ')' && (terminator_flags & PUNCT_TERM_RPAREN)) ||
        (tok->i64 == ',' && (terminator_flags & PUNCT_TERM_COMMA))) {
        return;
    }

    loggerPanic("Expected terminating token with flags: 0x%lX, got: %s, line: %d\n",
            terminator_flags, 
            lexemeToString(tok), tok->line);
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
        if (ast->operand != NULL) {
            AstPrint(ast->operand);
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
        if (ast->operand != NULL) {
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
        return;
    default:
        loggerPanic("Expected lvalue, got: %s at line: %ld\n", AstToString(ast),lineno);
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
