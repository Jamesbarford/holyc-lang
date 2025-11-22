#include "ir-types.h"


int irIsFloat(IrValueType type) {
    return type == IR_TYPE_F64;
}

int irIsInt(IrValueType type) {
    return type == IR_TYPE_I8 ||
           type == IR_TYPE_I16 ||
           type == IR_TYPE_I32 ||
           type == IR_TYPE_I64;
}

int irIsStruct(IrValueType ir_value_type) {
    return ir_value_type == IR_TYPE_STRUCT;
}

/* This is very easy to inline */
IrBlockMapping *irFunctionGetBlockMapping(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)mapGet(func->cfg,
                                                                (void *)(u64)ir_block->id);
    return ir_block_mapping;
}

Map *irFunctionGetSuccessors(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *ir_block_mapping = irFunctionGetBlockMapping(func, ir_block);
    if (ir_block_mapping) {
        return ir_block_mapping->successors;
    }
    return NULL;
}

Map *irFunctionGetPredecessors(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *ir_block_mapping = irFunctionGetBlockMapping(func, ir_block);
    if (ir_block_mapping) {
        return ir_block_mapping->predecessors;
    }
    return NULL;
}
