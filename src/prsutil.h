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
int parseIsFunctionCall(Ast *ast);
AstType *parseGetType(Cctrl *cc, lexeme *tok);
int parseIsKeyword(lexeme *tok, Cctrl *cc);
long evalIntConstExpr(Ast *ast);
double evalFloatExpr(Ast *ast);
int evalClassRef(Ast *ast, int offset);
void assertLValue(Ast *ast, long lineno);
int parseIsFloatOrInt(Ast *ast);
int parseIsClassOrUnion(int kind);
int parseIsFunction(Ast *ast);

void assertTokenIsTerminator(lexeme *tok, long terminator_flags);
void assertUniqueSwitchCaseLabels(List *case_list, Ast *case_);
void assertIsFloatOrInt(Ast *ast, long lineno);
void assertIsInt(Ast *ast, long lineno);
void assertIsFloat(Ast *ast, long lineno);
void assertIsPointer(Ast *ast, long lineno);

#endif // PRS_UTIL
