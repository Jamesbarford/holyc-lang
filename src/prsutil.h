#ifndef PRS_UTIL
#define PRS_UTIL

#include "ast.h"
#include "cctrl.h"
#include "lexer.h"

#define PUNCT_TERM_COMMA  (1<<0) // ';'
#define PUNCT_TERM_SEMI   (1<<1) // ','
#define PUNCT_TERM_RPAREN (1<<2) // ')'
#define PUNCT_TERM_LPAREN (1<<3) // '('
#define PUNCT_TERM_ARROW  (1<<4) // '->'
#define PUNCT_TERM_DOT    (1<<5) // '.'
#define PUNCT_TERM_LSQR   (1<<6) // '['
#define PUNCT_TERM_RSQR   (1<<7) // ']'

int align(int n, int m);


/* Is the lexeme both of type TK_PUNCT and does 'ch' match */
int ParseIsFunctionCall(Ast *ast);
AstType *ParseGetType(Cctrl *cc, lexeme *tok);
int ParseIsKeyword(lexeme *tok, Cctrl *cc);
long EvalIntConstExpr(Ast *ast);
double EvalFloatExpr(Ast *ast);
int EvalClassRef(Ast *ast, int offset);
void AssertLValue(Ast *ast, long lineno);
int ParseIsFloatOrInt(Ast *ast);
int ParseIsClassOrUnion(int kind);

void AssertTokenIsTerminator(lexeme *tok, long terminator_flags);
void AssertUniqueSwitchCaseLabels(List *case_list, Ast *case_);
void AssertIsFloatOrInt(Ast *ast, long lineno);
void AssertIsInt(Ast *ast, long lineno);
void AssertIsFloat(Ast *ast, long lineno);
void AssertIsPointer(Ast *ast, long lineno);

#endif // PRS_UTIL
