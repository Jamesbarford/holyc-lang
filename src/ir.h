#ifndef IR_H__
#define IR_H__

#include "cctrl.h"
#include "ir-types.h"

void irArenaInit(unsigned int capacity);
void irMemoryRelease(void);
void irMemoryStats(void);
IrProgram *irLowerAst(Cctrl *cc);

IrCtx *irCtxNew(void);
int irCouldReadMemory(IrInstr *instr);
void irDump(Cctrl *cc);

IrValue *irConstInt(IrValueType ir_value_type, long i64);
IrValue *irConstFloat(IrValueType ir_value_type, double f64);

#endif
