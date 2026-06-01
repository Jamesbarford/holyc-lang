#ifndef IR_H__
#define IR_H__

#include "cctrl.h"
#include "ir-types.h"

void irMemoryInit(void);
void irMemoryRelease(void);
void irMemoryStats(void);
void irDump(Cctrl *cc);
IrValue *irExpr(IrCtx *ctx, Ast *ast);
IrCtx *irLowerProgram(Cctrl *cc);
IrFunction *irLowerFunction(IrCtx *ctx, Ast *ast_func);

void irFunctionPrepForCodeGen(IrCgCtx *ctx, IrFunction *fn, Ast *ast_fn);

#endif
