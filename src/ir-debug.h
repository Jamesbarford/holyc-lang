#ifndef IR_DEBUG__
#define IR_DEBUG__

#include "ir-types.h"

AoStr *debugColourInt(long value);
AoStr *debugColourIntAsPtr(void *value);
AoStr *debugColourFloat(double value);
AoStr *debugColourAoStr(AoStr *str);
void debugVectorAoStrStringify(AoStr *buf, AoStr *value);
const char *irValueTypeToString(IrValueType ir_value_type);
const char *irValueKindToString(IrValueKind ir_value_kind);
const char *irValueKindToPrettyString(IrValueKind ir_value_kind);
void irArrayInitToString(AoStr *buf, IrValue *ir_value);
AoStr *irValueToString(IrValue *ir_value);
const char *irCmpKindToString(IrCmpKind ir_cmp_kind);
const char *irOpcodeToString(IrInstr *ir_instr);
void irPairToString(AoStr *buf, IrPair *ir_phi_pair);
AoStr *irInstrToString(IrInstr *ir_instr);
AoStr *irBlockToString(IrFunction *func, IrBlock *ir_block);
AoStr *irParamsToString(Vec *ir_value_vector);
AoStr *irFunctionToString(IrFunction *ir_func);
void irPrintFunction(IrFunction *ir_function);
AoStr *irBlockToStringSimplified(IrBlock *ir_block);
AoStr *irFunctionCFGToString(IrFunction *func);

void irValuePrint(IrValue *ir_value);
void irInstrPrint(IrInstr *ir_instr);

#endif
