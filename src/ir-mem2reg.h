#ifndef IR_MEM2REG_H__
#define IR_MEM2REG_H__

#include "ir-types.h"

/* Mini mem2reg: promote alloca slots that are written exactly once to plain
 * SSA value flow. Each LOAD reading such a slot is replaced (via operand
 * rewriting) by the value the unique STORE wrote; the alloca, the store,
 * and the loads are marked IR_NOP. Multi-store allocas would need phi
 * insertion at merge points and are intentionally left alone. */
void irMem2Reg(IrFunction *func);

#endif
