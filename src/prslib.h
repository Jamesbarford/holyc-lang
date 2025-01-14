#ifndef PRS_LIB_H
#define PRS_LIB_H

#include "cctrl.h"

#ifdef __cplusplus
extern "C" {
#endif

Ast *parseUnaryExpr(Cctrl *cc);
Ast *parseExpr(Cctrl *cc, int prec);
Ast *parseFunctionArguments(Cctrl *cc, char *fname, int len, long terminator);
PtrVec *parseParams(Cctrl *cc, long terminator, int *has_var_args, int store);
void parseDeclInternal(Cctrl *cc, Lexeme **tok, AstType **type);
void parseAssignAuto(Cctrl *cc, Ast *ast);
AstType *parseReturnAuto(Cctrl *cc, Ast *retval);
AstType *parseArrayDimensions(Cctrl *cc, AstType *base_type);
AstType *parsePointerType(Cctrl *cc, AstType *type);
AstType *parseBaseDeclSpec(Cctrl *cc);
AstType *parseDeclSpec(Cctrl *cc);
AstType *parseFullType(Cctrl *cc);
Ast *parseFunctionPointer(Cctrl *cc, AstType *rettype);
AstType *parseFunctionPointerType(Cctrl *cc,
        char **fnptr_name, int *fnptr_name_len, AstType *rettype);
Ast *findFunctionDecl(Cctrl *cc, char *fname, int len);
Ast *parseCreateBinaryOp(Cctrl *cc, long operation, Ast *left, Ast *right);

#ifdef __cplusplus
};
#endif

#endif
