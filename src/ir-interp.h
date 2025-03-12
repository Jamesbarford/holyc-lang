#ifndef IR_INTERP_H__
#define IR_INTERP_H__

#include "ir.h"

int irOpIsCmp(IrOpcode opcode);
int irIsFloat(IrValueType ir_value_type);
int irIsInt(IrValueType ir_value_type);
int irIsConst(IrValueKind ir_value_kind);
int irAreCompatibleCmpTypes(IrValueType t1, IrValueType t2);
IrValueType irConvertType(AstType *type);
int irBlockIsConstCompareAndBranch(IrBlock *block);
IrBlock *irInstrEvalConstBranch(IrInstr *ir_cmp, IrInstr *ir_branch);


IrInstr *irBlockLastInstr(IrBlock *block);
int irLastInstructionIsJumpLike(IrBlock *block);

int irBlockHasSuccessors(IrFunction *func, IrBlock *block);
IntMap *irBlockGetSuccessors(IrFunction *func, IrBlock *block);

int irBlockHasPredecessors(IrFunction *func, IrBlock *block);
IntMap *irBlockGetPredecessors(IrFunction *func, IrBlock *block);

int irBlockIsRedundant(IrFunction *func, IrBlock *block);
int irBlockIsRedundantJump(IrFunction *func, IrBlock *block);

#endif
