#include <assert.h>
#include <math.h>

#include "ir-interp.h"
#include "ir-types.h"
#include "util.h"

/*==================== IR HELPERS =========================================== */
/* Is what we are looking at a comparison? */
int irOpIsCmp(IrOpcode opcode) {
    if (opcode == IR_OP_ICMP || opcode == IR_OP_FCMP) {
        return 1;
    }
    return 0;
}

int irIsFloat(IrValueType ir_value_type) {
    return ir_value_type == IR_TYPE_F64;
}

int irIsInt(IrValueType ir_value_type) {
    return ir_value_type == IR_TYPE_I8 ||
           ir_value_type == IR_TYPE_I16 ||
           ir_value_type == IR_TYPE_I32 ||
           ir_value_type == IR_TYPE_I64;
}

int irIsConst(IrValueKind ir_value_kind) {
    return ir_value_kind == IR_VALUE_CONST_INT || 
           ir_value_kind == IR_VALUE_CONST_FLOAT;
}

int irAreCompatibleCmpTypes(IrValueType t1, IrValueType t2) {
    if (t1 == t2) return 1;
    if (irIsInt(t1) & irIsInt(t2)) return 1;
    if (irIsFloat(t1) & irIsFloat(t2)) return 1;

    if ((t1 == IR_TYPE_PTR && irIsInt(t2)) || (t2 == IR_TYPE_PTR && irIsInt(t1)))
        return 1;

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


/*==================== IR COMPILE TIME EVALUATION =========================== */

IrInstr *irBlockLastInstr(IrBlock *block) {
    List *tail = listTail(block->instructions);
    if (!tail) return NULL;
    return listValue(IrInstr *, tail);
}

int irLastInstructionIsJumpLike(IrBlock *block) {
    IrInstr *ir_instr = irBlockLastInstr(block);
    if (!ir_instr) return 0;
    return ir_instr->opcode == IR_OP_LOOP ||
           ir_instr->opcode == IR_OP_JMP  ||
           ir_instr->opcode == IR_OP_BR;
}

IntMap *irBlockGetSuccessors(IrFunction *func, IrBlock *block) {
    IrBlockMapping *mapping = intMapGet(func->cfg, block->id);
    if (!mapping) return NULL;
    return mapping->successors;
}

IntMap *irBlockGetPredecessors(IrFunction *func, IrBlock *block) {
    IrBlockMapping *mapping = intMapGet(func->cfg, block->id);
    if (!mapping) return NULL;
    return mapping->predecessors;
}

int irBlockHasSuccessors(IrFunction *func, IrBlock *block) {
    IntMap *successors = irBlockGetSuccessors(func, block);
    return successors ? successors->size > 0 : 0;
}

int irBlockHasPredecessors(IrFunction *func, IrBlock *block) {
    IntMap *predecessors = irBlockGetPredecessors(func, block);
    return predecessors ? predecessors->size > 0 : 0;
}

int irBlockIsRedundant(IrFunction *func, IrBlock *block) {
    IrBlockMapping *mapping = intMapGet(func->cfg, block->id);

    /* No instructions in a node means it can be killed */
    if (listEmpty(block->instructions)) {
        return 1;
    } 
    /* No mapping means the block is essentially not used */
    else if (!mapping) {
        return 1;
    }
    /* There is nothing that points to this node and it is not the start 
     * block for the function */
    else if (mapping->predecessors->size == 0 && block->id != 0) {
        return 1;
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
    if (ir_last_instr->opcode == IR_OP_JMP) {
        IrBlock *next_block = ir_last_instr->target_block;
        IntMap *next_predecessors = irBlockGetPredecessors(func, next_block);
        IntMap *cur_successors = irBlockGetSuccessors(func, block);

        /* This check is overly safe */
        if (next_predecessors->size == 1 &&
            intMapHas(next_predecessors, block->id) &&
            cur_successors->size == 1 &&
            intMapHas(cur_successors, next_block->id)) {
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

    if (irOpIsCmp(maybe_cmp->opcode) && maybe_br->opcode == IR_OP_BR) {
        if (irIsConst(maybe_cmp->op1->kind) && irIsConst(maybe_cmp->op2->kind)) {
            return 1;
        }
    }
    return 0;
}

/* Evaluate the current block and return the actual target from the result of 
 * the comparison. Handles both floats and integer comparisons */
IrBlock *irInstrEvalConstBranch(IrInstr *ir_cmp, IrInstr *ir_branch) {
    assert(irOpIsCmp(ir_cmp->opcode));
    IrValue *v1 = ir_cmp->op1;
    IrValue *v2 = ir_cmp->op2;

#define I1 v1->i64
#define I2 v2->i64

#define F1 v1->f64
#define F2 v2->f64

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
    }

/* We can remove these symbolic constants */
#undef V1
#undef V2 
#undef I1
#undef I2
#undef F1
#undef F2

fallthrough:
    return ir_branch->fallthrough_block;

target:
    return ir_branch->target_block;
    
}
