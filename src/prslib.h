#ifndef PRS_LIB_H
#define PRS_LIB_H

#include "cctrl.h"

Ast *parseUnaryExpr(Cctrl *cc);
Ast *parseExpr(Cctrl *cc, int prec);
Ast *parseFunctionArguments(Cctrl *cc, char *fname, int len, s64 terminator);
Vec *parseParams(Cctrl *cc, s64 terminator, int *has_var_args, int store);
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
Ast *parseCreateBinaryOp(Cctrl *cc, AstBinOp operation, Ast *left, Ast *right);
Ast *parseSizeof(Cctrl *cc);

#endif
