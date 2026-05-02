#ifndef IR_H__
#define IR_H__

#include "ast.h"
#include "cctrl.h"
#include "ir-types.h"

void irMemoryInit(void);
void irMemoryRelease(void);
void irMemoryStats(void);
void irDump(Cctrl *cc);
IrCtx *irLowerProgram(Cctrl *cc);

/* Slice-0: function body uses only AST_COMPOUND_STMT, AST_DECL (int),
 * AST_RETURN, AST_IF, AST_LVAR (int), AST_LITERAL (int), AST_BINOP (int,
 * assignment LHS limited to AST_LVAR). Return type and all locals/params
 * must be int or void (the return type only). */
int irFunctionEligibleForSlice(Ast *ast_func);
int irFunctionEligibleForSliceCc(Cctrl *cc, Ast *ast_func);

/* Lower a single AST function to IR. Caller must have verified eligibility. */
IrFunction *irLowerFunction(IrCtx *ctx, Ast *ast_func);

#endif
