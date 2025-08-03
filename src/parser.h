#ifndef PARSER_H
#define PARSER_H

#include "list.h"
#include "ast.h"
#include "cctrl.h"

#define PUNCT_TERM_COMMA  (1<<0) // ';'
#define PUNCT_TERM_SEMI   (1<<1) // ','
#define PUNCT_TERM_RPAREN (1<<2) // ')'
#define PUNCT_TERM_LPAREN (1<<3) // '('
#define PUNCT_TERM_ARROW  (1<<4) // '->'
#define PUNCT_TERM_DOT    (1<<5) // '.'
#define PUNCT_TERM_LSQR   (1<<6) // '['
#define PUNCT_TERM_RSQR   (1<<7) // ']'

void parseToAst(Cctrl *cc);
Ast *parseStatement(Cctrl *cc);
s64 evalIntConstExpr(Ast *ast);

#endif // !PARSER_H
