#ifndef PRS_LIB_H
#define PRS_LIB_H

#include "cctrl.h"

Ast *ParseUnaryExpr(Cctrl *cc);
Ast *ParseExpr(Cctrl *cc, int prec);
Ast *ParseFunctionArguments(Cctrl *cc, char *fname, int len, long terminator);
List *ParseParams(Cctrl *cc, long terminator, int *has_var_args);
void ParseDeclInternal(Cctrl *cc, lexeme **tok, AstType **type);
void ParseAssignAuto(Cctrl *cc, Ast *ast);
AstType *ParseReturnAuto(Cctrl *cc, Ast *retval);
AstType *ParseArrayDimensions(Cctrl *cc, AstType *base_type);
AstType *ParsePointerType(Cctrl *cc, AstType *type);
AstType *ParseBaseDeclSpec(Cctrl *cc);
AstType *ParseDeclSpec(Cctrl *cc);
AstType *ParseFullType(Cctrl *cc);
Ast *ParseFunctionPointer(Cctrl *cc, AstType *rettype);
AstType *ParseFunctionPointerType(Cctrl *cc,
        char **fnptr_name, int *fnptr_name_len, AstType *rettype);

#endif
