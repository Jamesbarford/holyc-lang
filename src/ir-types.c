#include "ir-types.h"
#include "util.h"

u8 irIsFloat(IrValueType type) {
    return type == IR_TYPE_F64;
}

u8 irIsInt(IrValueType type) {
    return type == IR_TYPE_I8 ||
           type == IR_TYPE_I16 ||
           type == IR_TYPE_I32 ||
           type == IR_TYPE_I64;
}

u8 irIsConstInt(IrValue *val) {
    return irIsInt(val->type) &&
           val->kind == IR_VAL_CONST_INT;
}

u8 irIsStruct(IrValueType ir_value_type) {
    return ir_value_type == IR_TYPE_STRUCT;
}

int irGetIntSize(IrValueType ir_value_type) {
    switch (ir_value_type) {
        case IR_TYPE_I8:  return 1;
        case IR_TYPE_I16: return 2;
        case IR_TYPE_I32: return 4;
        case IR_TYPE_I64: return 8;
        default: loggerPanic("%d is not an integer type\n", ir_value_type);
    }
}

IrValueType irConvertType(AstType *type) {
    switch (type->kind) {
        case AST_TYPE_VOID: return IR_TYPE_VOID;
        case AST_TYPE_INT: {
            switch (type->size) {
                case 1: return IR_TYPE_I8;
                case 2: return IR_TYPE_I16;
                case 4: return IR_TYPE_I32;
                case 8: return IR_TYPE_I64;
                default:
                    loggerPanic("Invalid integer size `%d` for type %s\n",
                            type->size,
                            astTypeToString(type));
            }
        }
        case AST_TYPE_CHAR:    return IR_TYPE_I8;
        case AST_TYPE_FLOAT:   return IR_TYPE_F64;
        case AST_TYPE_ARRAY:   return IR_TYPE_ARRAY;
        case AST_TYPE_POINTER: return IR_TYPE_PTR;
        case AST_TYPE_FUNC:    return IR_TYPE_FUNCTION;
        case AST_TYPE_CLASS:   return IR_TYPE_STRUCT;
        case AST_TYPE_UNION:   return IR_TYPE_STRUCT;
        case AST_TYPE_VIS_MODIFIER:
            loggerPanic("Type visibility modifier is not a type!\n");
        case AST_TYPE_INLINE:
            loggerPanic("Type `inline` is not a type!\n");
        case AST_TYPE_AUTO:
            loggerPanic("Type `auto` failed to infer it's runtime type\n");
        default:
            loggerPanic("Type `%s` unhandled\n", astTypeToString(type));
    }
}

Map *irBlockGetSuccessors(IrFunction *func, IrBlock *block) {
    IrBlockMapping *mapping = mapGetInt(func->cfg, block->id);
    if (!mapping) return NULL;
    return mapping->successors;
}

static IrBlockMapping *irFunctionGetBlockMapping(IrFunction *func,
                                                 IrBlock *ir_block) {
    return (IrBlockMapping *)mapGet(func->cfg,
                                    (void *)(u64)ir_block->id);
}

Map *irFunctionGetSuccessors(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *m = irFunctionGetBlockMapping(func, ir_block);
    return m ? m->successors : NULL;
}

Map *irFunctionGetPredecessors(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *m = irFunctionGetBlockMapping(func, ir_block);
    return m ? m->predecessors : NULL;
}
