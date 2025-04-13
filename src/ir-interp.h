#ifndef IR_INTERP_H__
#define IR_INTERP_H__

#include "ast.h"
#include "ir-types.h"

int irOpIsCmp(IrOpcode opcode);
int irIsFloat(IrValueType ir_value_type);
int irIsInt(IrValueType ir_value_type);
int irTypeIsScalar(IrValueType ir_value_type);
int irGetIntSize(IrValueType ir_value_type);
int irIsConst(IrValueKind ir_value_kind);
int irIsPtr(IrValueType ir_value_type);
int irIsStore(IrOpcode opcode);
int irIsLoad(IrOpcode opcode);
int irIsStruct(IrValueType ir_value_type);

int irValueIsVariable(IrValue *value);
int irAreCompatibleCmpTypes(IrValueType t1, IrValueType t2);
IrValueType irConvertType(AstType *type);
int irBlockIsConstCompareAndBranch(IrBlock *block);
IrBlock *irInstrEvalConstBranch(IrInstr *ir_cmp, IrInstr *ir_branch);

int irCouldModifyMemory(IrInstr *instr);
int irCouldReadMemory(IrInstr *instr);
int irBlockIsStartOrEnd(IrFunction *func, IrBlock *block);

int irValuesEq(IrValue *v1, IrValue *v2);

int irBlocksPointToEachOther(IrFunction *func, IrBlock *prev_block, IrBlock *next_block);

IrInstr *irBlockLastInstr(IrBlock *block);
int irLastInstructionIsJumpLike(IrBlock *block);

int irBlockHasSuccessors(IrFunction *func, IrBlock *block);
Map *irBlockGetSuccessors(IrFunction *func, IrBlock *block);

int irBlockHasPredecessors(IrFunction *func, IrBlock *block);
Map *irBlockGetPredecessors(IrFunction *func, IrBlock *block);

int irBlockIsRedundant(IrFunction *func, IrBlock *block);
int irBlockIsOnlyJump(IrFunction *func, IrBlock *block);
int irBlockIsRedundantJump(IrFunction *func, IrBlock *block);

#endif
