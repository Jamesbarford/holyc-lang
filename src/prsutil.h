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
AstType *parseGetType(Cctrl *cc, Lexeme *tok);
int parseIsKeyword(Lexeme *tok, Cctrl *cc);
double evalFloatExpr(Ast *ast);
double evalFloatExprOrErr(Ast *ast, int *_ok);
s64 evalIntConstExpr(Ast *ast);
s64 evalIntConstExprOrErr(Ast *ast, int *_ok);
s64 evalOneIntExprOrErr(Ast *LHS, Ast *RHS, AstBinOp op, int *_ok);
s64 evalIntArithmeticOrErr(Ast *ast, int *_ok);
int evalClassRef(Ast *ast, int offset);
int assertLValue(Ast *ast);
int parseIsFloatOrInt(Ast *ast);
int parseIsClassOrUnion(int kind);
int parseIsFunction(Ast *ast);
int astIsArithmetic(Ast *ast, int is_float);

void assertTokenIsTerminator(Cctrl *cc, Lexeme *tok, s64 terminator_flags);
void assertTokenIsTerminatorWithMsg(Cctrl *cc, Lexeme *tok,
        s64 terminator_flags, const char *fmt, ...);
void assertUniqueSwitchCaseLabels(Vec *case_vector, Ast *case_);
void assertIsFloatOrInt(Ast *ast, s64 lineno);
void assertIsInt(Ast *ast, s64 lineno);
void assertIsFloat(Ast *ast, s64 lineno);
void assertIsPointer(Ast *ast, s64 lineno);

void typeCheckWarn(Cctrl *cc, s64 op, Ast *expected, Ast *actual);
void typeCheckReturnTypeWarn(Cctrl *cc, Ast *maybe_func, 
                             AstType *check, Ast *retval);
#endif // PRS_UTIL
