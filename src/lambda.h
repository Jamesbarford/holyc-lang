#ifndef LAMBDA_H
#define LAMBDA_H

#define LAMBDA_MAX_CAPTURE 32

#include "ast.h"
#include "cctrl.h"
#include "map.h"

Ast *parseLambdaInnerFunctionArguments(Cctrl *cc, Ast *lambda, Ast *func_call);
Ast *parseLambdaCreateCall(Cctrl *cc, Ast *ast_lambda, PtrVec *argv);
Ast *parseLambda(Cctrl *cc, AstType *decl_type, Lexeme *identifier);
Ast *parseLambdaNoCapture(Cctrl *cc, AstType *decl_type, Lexeme *identifier);

#endif
