#include <assert.h>
#include <math.h>

#include "ir-types.h"
#include "util.h"

u8 irOpIsCmp(IrOp opcode) {
    if (opcode == IR_ICMP || opcode == IR_FCMP) {
        return 1;
    }
    return 0;
}

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

u8 irIsPtr(IrValueType ir_value_type) {
    return ir_value_type == IR_TYPE_PTR;
}

u8 irIsStore(IrOp opcode) {
    return opcode = IR_STORE;
}

u8 irIsLoad(IrOp opcode) {
    return opcode = IR_LOAD;
}

u8 irTypeIsScalar(IrValueType ir_value_type) {
    return irIsFloat(ir_value_type) || irIsInt(ir_value_type);
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

u8 irIsConst(IrValueKind ir_value_kind) {
    return ir_value_kind == IR_VAL_CONST_INT || 
           ir_value_kind == IR_VAL_CONST_FLOAT;
}

IrBlock *irInstrGetTargetBlock(IrInstr *instr) {
    return instr->extra.blocks.target_block;
}

IrBlock *irInstrGetFallthroughBlock(IrInstr *instr) {
    return instr->extra.blocks.fallthrough_block;
}

int irAreCompatibleCmpTypes(IrValueType t1, IrValueType t2) {
    if (t1 == t2) return 1;
    if (irIsInt(t1) & irIsInt(t2)) return 1;
    if (irIsFloat(t1) & irIsFloat(t2)) return 1;

    if ((t1 == IR_TYPE_PTR && irIsInt(t2)) || (t2 == IR_TYPE_PTR && irIsInt(t1)))
        return 1;

    return 0;
}

int irVarEq(IrValue *v1, IrValue *v2) {
    if (!v2 || !v1) return 0;
    if (v1 == v2) return 1;
    if (v1->kind == v2->kind && v1->type == v2->type && v1->as.var.id == v2->as.var.id) {
        return 1;
    }
    return 0;
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
            loggerPanic( "Type `inline` is not a type!\n");
        case AST_TYPE_AUTO:
            loggerPanic("Type `auto` failed to infer it's runtime type\n");
        default:
            loggerPanic("Type `%s` unhandled\n",astTypeToString(type));
    }
}

int irValueIsVariable(IrValue *value) {
    return value->kind == IR_VAL_LOCAL ||
           value->kind == IR_VAL_PARAM ||
           value->kind == IR_VAL_TMP ||
           value->kind == IR_VAL_GLOBAL;
}

/*=========================================================================== */


/*==================== IR COMPILE TIME EVALUATION =========================== */

IrInstr *irBlockLastInstr(IrBlock *block) {
    List *tail = listTail(block->instructions);
    if (!tail) return NULL;
    return listValue(IrInstr *, tail);
}

int irLastInstructionIsJumpLike(IrBlock *block) {
    IrInstr *ir_instr = irBlockLastInstr(block);
    if (!ir_instr) return 0;
    return ir_instr->op == IR_LOOP ||
           ir_instr->op == IR_JMP  ||
           ir_instr->op == IR_BR;
}

Map *irBlockGetSuccessors(IrFunction *func, IrBlock *block) {
    IrBlockMapping *mapping = mapGetInt(func->cfg, block->id);
    if (!mapping) return NULL;
    return mapping->successors;
}

Map *irBlockGetPredecessors(IrFunction *func, IrBlock *block) {
    IrBlockMapping *mapping = mapGetInt(func->cfg, block->id);
    if (!mapping) return NULL;
    return mapping->predecessors;
}

int irBlockHasSuccessors(IrFunction *func, IrBlock *block) {
    Map *successors = irBlockGetSuccessors(func, block);
    return successors ? successors->size > 0 : 0;
}

int irBlockHasPredecessors(IrFunction *func, IrBlock *block) {
    Map *predecessors = irBlockGetPredecessors(func, block);
    return predecessors ? predecessors->size > 0 : 0;
}

int irBlockIsStartOrEnd(IrFunction *func, IrBlock *block) {
    return block == func->entry_block || block == func->exit_block; 
}

int irBlockIsRedundant(IrFunction *func, IrBlock *block) {
    IrBlockMapping *mapping = mapGetInt(func->cfg, block->id);

    /* Start and exit will be merged seprately */
    if (irBlockIsStartOrEnd(func, block)) {
        return 0;
    }
    /* No instructions in a node means it can be killed */
    else if (listEmpty(block->instructions)) {
        return 1;
    } 
    /* No mapping means the block is essentially not used */
    else if (!mapping) {
        return 1;
    }
    /* There is nothing that points to this node and it is not the start 
     * block for the function */
    else if (mapping->predecessors->size == 0) {
        return 1;
    }

    return 0;
}

/* Does the previous block have one successor and jump to the next block and 
 * does the next block have one predecessor that is the previous block. 
 * If yes... we can join them together. */
u8 irBlocksPointToEachOther(IrFunction *func, IrBlock *cur_block, IrBlock *next_block) {
    IrInstr *ir_last_instr = irBlockLastInstr(cur_block);
    assert(ir_last_instr);

    if (ir_last_instr->op == IR_JMP) {
        IrBlock *target_block = irInstrGetTargetBlock(ir_last_instr);
        /* The target block does not point to the block of interest */
        if (target_block->id != next_block->id) return 0;
        Map *cur_successors = irBlockGetSuccessors(func, cur_block);
        Map *next_predecessors = irBlockGetPredecessors(func, next_block);

        /* This check is overly safe */
        if (next_predecessors->size == 1 &&
            mapHasInt(next_predecessors, cur_block->id) &&
            cur_successors->size == 1 &&
            mapHasInt(cur_successors, next_block->id)) {
            return 1;
        }
    }
    return 0;
}

/* Is the block a solitary jump instruction? */
int irBlockIsOnlyJump(IrFunction *func, IrBlock *block) {
    IrInstr *ir_last_instr = irBlockLastInstr(block);
    assert(ir_last_instr);
    if (ir_last_instr->op == IR_JMP && listIsOne(block->instructions)) {
        Map *successors = irBlockGetSuccessors(func, block);
        Map *predecessors = irBlockGetSuccessors(func, block);
        if (successors && predecessors) {
            return successors->size == 1 && predecessors->size == 1;
        } else {
            return 0;
        }
    }
    return 0;
}

/**
 * Does the current block end with a jump, have one successor and the successor
 * has one predecessor?
 */
int irBlockIsRedundantJump(IrFunction *func, IrBlock *block) {
    IrInstr *ir_last_instr = irBlockLastInstr(block);
    assert(ir_last_instr);
    if (ir_last_instr->op == IR_JMP) {
        IrBlock *next_block = irInstrGetTargetBlock(ir_last_instr);
        Map *cur_successors = irBlockGetSuccessors(func, block);
        Map *next_predecessors = irBlockGetPredecessors(func, next_block);

        unsigned long next_predecessors_size = next_predecessors ? 
                                               next_predecessors->size : 0;
        unsigned long cur_successors_size = cur_successors ?
                                            cur_successors->size : 0;

        /* This check is overly safe */
        if (next_predecessors_size == 1 &&
            mapHasInt(next_predecessors, block->id) &&
            cur_successors_size == 1 &&
            mapHasInt(cur_successors, next_block->id))
        {
            return next_block->id != block->id;
        }
    }
    return 0;
}

int irBlockIsConstCompareAndBranch(IrBlock *block) {
    /* We need to ensure that the instruction list is 2 in length and is
     * a `cmp` followed by a `br` */
    if (listEmpty(block->instructions) || listIsOne(block->instructions)) {
        return 0;
    }

    /* We have at least 2 instructions */
    IrInstr *maybe_cmp = listValue(IrInstr *, block->instructions->next);
    IrInstr *maybe_br = listValue(IrInstr *, block->instructions->next->next);

    if (irOpIsCmp(maybe_cmp->op) && maybe_br->op == IR_BR) {
        if (irIsConst(maybe_cmp->r1->kind) && irIsConst(maybe_cmp->r2->kind)) {
            return 1;
        }
    }
    return 0;
}

/* Evaluate the current block and return the actual target from the result of 
 * the comparison. Handles both floats and integer comparisons */
IrBlock *irInstrEvalConstBranch(IrInstr *ir_cmp, IrInstr *ir_branch) {
    assert(irOpIsCmp(ir_cmp->op));
    IrValue *v1 = ir_cmp->r1;
    IrValue *v2 = ir_cmp->r2;

#define I1 v1->as._i64
#define I2 v2->as._i64

#define F1 v1->as._f64
#define F2 v2->as._f64

#define V1 (irIsFloat(v1->type) ? F1 : I1)
#define V2 (irIsFloat(v2->type) ? F2 : I2)

    switch (ir_cmp->extra.cmp_kind) {
        case IR_CMP_EQ:
            if (V1 == V2) goto target;
            else          goto fallthrough;
        
        case IR_CMP_NE:
            if (V1 != V2) goto target;
            else          goto fallthrough;

        case IR_CMP_LT:
            if (V1 < V2) goto target;
            else         goto fallthrough;

        case IR_CMP_LE:
            if (V1 <= V2) goto target;
            else          goto fallthrough;

        case IR_CMP_GT:
            if (V1 > V2) goto target;
            else         goto fallthrough;
        
        case IR_CMP_GE:
            if (V1 >= V2) goto target;
            else          goto fallthrough;

        case IR_CMP_ULT:
            if ((unsigned long)I1 < (unsigned long)I2) goto target;
            else                                       goto fallthrough;

        case IR_CMP_ULE:
            if ((unsigned long)I1 <= (unsigned long)I2) goto target;
            else                                        goto fallthrough;

        case IR_CMP_UGT:
            if ((unsigned long)I1 > (unsigned long)I2) goto target;
            else                                       goto fallthrough;

        case IR_CMP_UGE:
            if ((unsigned long)I1 >= (unsigned long)I2) goto target;
            else                                        goto fallthrough;

        case IR_CMP_OEQ:
            if (F1 == F2) goto target;
            else          goto fallthrough;

        case IR_CMP_ONE:
            if (F1 != F2) goto target;
            else          goto fallthrough;

        case IR_CMP_OLT:
            if (F1 < F2) goto target;
            else         goto fallthrough;

        case IR_CMP_OLE:
            if (F1 <= F2) goto target;
            else          goto fallthrough;

        case IR_CMP_OGT:
            if (F1 > F2) goto target;
            else         goto fallthrough;

        case IR_CMP_OGE:
            if (F1 >= F2) goto target;
            else          goto fallthrough;

        case IR_CMP_UNO:
            if (isnan(F1) || isnan(F2)) goto target;
            else                        goto fallthrough;

        case IR_CMP_ORD:
            if (!isnan(F1) && !isnan(F2)) goto target;
            else                          goto fallthrough;
        case IR_CMP_INVALID:
            loggerPanic("Invalid comparison");
    }

/* We can remove these symbolic constants */
#undef V1
#undef V2 
#undef I1
#undef I2
#undef F1
#undef F2

fallthrough:
    return irInstrGetFallthroughBlock(ir_branch);

target:
    return irInstrGetTargetBlock(ir_branch);
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
