#ifndef IR_H__
#define IR_H__

#include "aostr.h"
#include "cctrl.h"
#include "ir-types.h"

void irArenaInit(unsigned int capacity);
void irMemoryRelease(void);
void irMemoryStats(void);
IrProgram *irLowerAst(Cctrl *cc);
aoStr *irProgramToString(IrProgram *ir_program);
aoStr *irValueToString(IrValue *ir_value);
const char *irValueKindToString(IrValueKind ir_value_kind);
const char *irValueTypeToString(IrValueType ir_value_type);
void irDump(Cctrl *cc);

#endif
