#ifndef PRS_UTIL
#define PRS_UTIL

#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "map.h"

#define PUNCT_TERM_COMMA  (1 << 0) // ';'
#define PUNCT_TERM_SEMI   (1 << 1) // ','
#define PUNCT_TERM_RPAREN (1 << 2) // ')'
#define PUNCT_TERM_LPAREN (1 << 3) // '('
#define PUNCT_TERM_ARROW  (1 << 4) // '->'
#define PUNCT_TERM_DOT    (1 << 5) // '.'
#define PUNCT_TERM_LSQR   (1 << 6) // '['
#define PUNCT_TERM_RSQR   (1 << 7) // ']'

int align(int n, int m);

/* Is the lexeme both of type TK_PUNCT and does 'ch' match */
int parseIsFunctionCall(const Ast *ast);
AstType *parseGetType(Cctrl *cc, Lexeme *tok);
int parseIsKeyword(const Lexeme *tok, const Cctrl *cc);
double evalFloatExpr(Ast *ast);
double evalFloatArithmeticOrErr(const Ast *ast, int *_ok);
double evalFloatExprOrErr(const Ast *ast, int *_ok);
double evalOneFloatExprOrErr(const Ast *LHS, const Ast *RHS, long op, int *_ok);
long evalIntConstExpr(Ast *ast);
long evalIntConstExprOrErr(const Ast *ast, int *_ok);
long evalOneIntExprOrErr(const Ast *LHS, const Ast *RHS, long op, int *_ok);
long evalIntArithmeticOrErr(const Ast *ast, int *_ok);
int evalClassRef(Ast *ast, int offset);
int assertLValue(const Ast *ast);
int parseIsFloatOrInt(const Ast *ast);
int parseIsClassOrUnion(int kind);
int parseIsFunction(const Ast *ast);
int astIsArithmetic(long op, int is_float);

void assertTokenIsTerminator(Cctrl *cc, Lexeme *tok, long terminator_flags);
void assertTokenIsTerminatorWithMsg(Cctrl *cc, Lexeme *tok,
                                    long terminator_flags, const char *fmt,
                                    ...);
void assertUniqueSwitchCaseLabels(const PtrVec *case_vector, const Ast *case_);
void assertIsFloatOrInt(const Ast *ast, long lineno);
void assertIsInt(const Ast *ast, long lineno);
void assertIsFloat(const Ast *ast, long lineno);
void assertIsPointer(Ast *ast, long lineno);

void typeCheckWarn(Cctrl *cc, long op, const Ast *expected, Ast *actual);
void typeCheckReturnTypeWarn(Cctrl *cc, const Ast *maybe_func,
                             const AstType *check, const Ast *retval);
#endif // PRS_UTIL
