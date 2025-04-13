#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "aostr.h"
#include "map.h"
#include "list.h"
#include "util.h"
#include "ir-types.h"
#include "ir-interp.h"
#include "ir-optimiser.h"



int irInstrHasSideEffects(IrInstr *instr);
int irFnCallReturnUsed(IrBlock *block, IrLivenessInfo *info, IrInstr *instr);
aoStr *irMemLocation(IrValue *value);
void irReplaceAllUses(IrFunction *func, IrValue *old_value, IrValue *new_value);
void irBlockRemoveNops(IrBlock *block);
int irCanPromoteAlloca(IrFunction *func, IrInstr *instr);
int irStoreIsReadBefore(IrFunction *func, IrBlock *block, IrInstr *instr);
int irInstrLive(IrBlock *block, IrLivenessInfo *info, IrInstr *instr);

/* Optimisation passese */
void irEliminateRedundantLoads(IrFunction *func);
void irEliminateRedundantStores(IrFunction *func);
void irEliminateDeadStores(IrFunction *func);
void irEliminateDeadAllocas(IrFunction *func);
void irPerformLoadStoreForwarding(IrFunction *func);
void irPerformCopyPropagation(IrFunction *func);
void irFoldParameterLoads(IrFunction *func);

IrLivenessInfo *irLivenessInfoNew(void) {
    IrLivenessInfo *info = (IrLivenessInfo *)malloc(sizeof(IrLivenessInfo));
    info->live_in = irValueSetNew();
    info->live_out = irValueSetNew();
    info->def = irValueSetNew();
    info->use = irValueSetNew();
    return info;
}

IrLivenessInfo *irLivenessGetInfo(IrLivenessAnalysis *liveness, IrBlock *block) {
    return (IrLivenessInfo *)intMapGet(liveness->block_info, block->id);
}

aoStr *irLivenessInfoToString(IrLivenessInfo *info) {
    aoStr *buf = aoStrNew();
    aoStrCatFmt(buf, "IrLivenessInfo {\n");
    aoStr *tmp = setToString(info->live_in);
    aoStrCatFmt(buf, "  live_in = %S\n", tmp);
    aoStrRelease(tmp);
    tmp = setToString(info->live_out);
    aoStrCatFmt(buf, "  live_out = %S\n", tmp);
    aoStrRelease(tmp);
    tmp = setToString(info->def);
    aoStrCatFmt(buf, "  def      = %S\n", tmp);
    aoStrRelease(tmp);
    tmp = setToString(info->use);
    aoStrCatFmt(buf, "  use      = %S\n", tmp);
    aoStrRelease(tmp);
    aoStrCatFmt(buf, "\n}\n");
    return buf;
}

int irCouldReadMemory(IrInstr *instr) {
    /* Direct memory reads */
    if (instr->opcode == IR_OP_LOAD)
        return 1;
    
    /* Variadic argument handling reads memory */
    if (instr->opcode == IR_OP_VA_ARG || 
        instr->opcode == IR_OP_VA_START || 
        instr->opcode == IR_OP_VA_END)
        return 1;
    
    /* Comparisons and branches don't read memory (except their operands) */
    if (instr->opcode == IR_OP_ICMP || 
        instr->opcode == IR_OP_FCMP || 
        instr->opcode == IR_OP_BR || 
        instr->opcode == IR_OP_JMP)
        return 0;
    
    /* GEP doesn't read memory, it just computes an address */
    if (instr->opcode == IR_OP_GEP)
        return 1;
    
    /* Check if any operands involve loads from a volatile location */
    /* (would require more detailed IR analysis) */
    
    /* Inline assembly might read memory */
    if (instr->opcode == IR_OP_CALL && 
        instr->op1 && 
        instr->op1->type == IR_TYPE_ASM_FUNCTION)
        return 1;
    
    /* For select instruction, we only read the operands, not memory */
    if (instr->opcode == IR_OP_SELECT)
        return 0;
    
    /* Switch doesn't read memory */
    if (instr->opcode == IR_OP_SWITCH)
        return 0;
    
    /* Bitwise and arithmetic operations don't read memory */
    if (instr->opcode >= IR_OP_IADD && instr->opcode <= IR_OP_NOT)
        return 0;
    
    /* Conversion operations don't read memory */
    if (instr->opcode >= IR_OP_TRUNC && instr->opcode <= IR_OP_BITCAST)
        return 0;
    
    /* PHI nodes don't read memory */
    if (instr->opcode == IR_OP_PHI)
        return 0;
    
    /* Return doesn't read memory (except its operand) */
    if (instr->opcode == IR_OP_RET)
        return 0;
    
    /* By default, be conservative and assume it might read memory */
    return 1;
}

int irCouldModifyMemory(IrInstr *instr) {
    /* Direct memory writes */
    if (instr->opcode == IR_OP_STORE)
        return 1;
    
    /* Function calls may modify any memory */
    if (instr->opcode == IR_OP_CALL) {
        /* Scan arguments to see if any of them are a pointer */
        PtrVec *fn_args = instr->op1->array_.values;
        if (fn_args && fn_args->size > 0) {
            for (int i = 0; i < fn_args->size; ++i) {
                IrValue *ir_value = vecGet(IrValue *,fn_args,i);
                if (ir_value->type == IR_TYPE_PTR) {
                    return 1;
                }
            }
        }
        return 0;
    }
    
    /* Variadic argument handling modifies memory */
    if (instr->opcode == IR_OP_VA_START || 
        instr->opcode == IR_OP_VA_END)
        return 1;
    
    /* Most arithmetic, comparison, and logical operations don't modify memory */
    if ((instr->opcode >= IR_OP_IADD && instr->opcode <= IR_OP_NOT) ||
        instr->opcode == IR_OP_ICMP || 
        instr->opcode == IR_OP_FCMP)
        return 0;
    
    /* Memory allocation affects memory state */
    if (instr->opcode == IR_OP_ALLOCA)
        return 1;
    
    /* Control flow doesn't modify memory */
    if (instr->opcode == IR_OP_BR || 
        instr->opcode == IR_OP_JMP || 
        instr->opcode == IR_OP_LOOP || 
        instr->opcode == IR_OP_SWITCH || 
        instr->opcode == IR_OP_RET)
        return 0;
    
    /* Conversion operations don't modify memory */
    if (instr->opcode >= IR_OP_TRUNC && instr->opcode <= IR_OP_BITCAST)
        return 0;
    
    /* PHI nodes don't modify memory */
    if (instr->opcode == IR_OP_PHI)
        return 0;
    
    /* GEP computes addresses but doesn't modify memory */
    if (instr->opcode == IR_OP_GEP)
        return 1;
    
    /* Inline assembly might modify memory */
    if (instr->opcode == IR_OP_CALL && 
        instr->op1 && 
        instr->op1->type == IR_TYPE_ASM_FUNCTION)
        return 1;
    
    /* By default, be conservative and assume it might modify memory */
    return 1;
}


IrLivenessInfo *irComputeBlockUseDef(IrBlock *block) {
    IrLivenessInfo *info = irLivenessInfoNew();
    listForEach(block->instructions) {
        IrInstr *instr = listValue(IrInstr *, it);

        if (instr->op1 && irValueIsVariable(instr->op1)) setAdd(info->use, instr->op1);
        if (instr->op2 && irValueIsVariable(instr->op2)) setAdd(info->use, instr->op2);
        if (instr->op3 && irValueIsVariable(instr->op3)) setAdd(info->use, instr->op3);

        if (instr->opcode == IR_OP_PHI) {
            for (int i = 0; i < instr->extra.phi.pairs->size; ++i) {
                IrPair *pair = vecGet(IrPair *, instr->extra.phi.pairs, i);
                if (irValueIsVariable(pair->ir_value)) {
                    setAdd(info->use, pair->ir_value);
                }
            }
        }

        if (instr->result && irValueIsVariable(instr->result)) setAdd(info->def, instr->result);
    }

    return info;
}

aoStr *irLivenessAnalysisToString(IrLivenessAnalysis *analysis) {
    aoStr *buf = aoStrNew();
    aoStrCatFmt(buf, "IrLivenessAnalysis {\n");
    aoStr *tmp = intMapToString(analysis->block_info,
                                ",\n",
                                (aoStr *(*)(void *))&irLivenessInfoToString);
    aoStrCatFmt(buf, "%S\n}\n", tmp);
    aoStrRelease(tmp);
    return buf;
}

IrLivenessAnalysis *irLivenessAnalysis(IrFunction *func) {
    IrLivenessAnalysis *analysis = malloc(sizeof(IrLivenessAnalysis));
    analysis->block_info = intMapNew(32);

    listForEachReverse(func->blocks) {
        IrBlock *block = listValue(IrBlock *, it);
        IrLivenessInfo *info = irComputeBlockUseDef(block);
        intMapAdd(analysis->block_info, block->id, info);
    }

    int changed = 1;
    while (changed) {
        changed = 0;
        listForEachReverse(func->blocks) {
            IrBlock *block = listValue(IrBlock *, it);
            IrLivenessInfo *info = intMapGet(analysis->block_info, block->id);
            Map *successors = irBlockGetSuccessors(func, block);

            /* Get the successors from the CFG */
            if (successors) {
                Set *new_live_out = irValueSetNew();
                MapIter iter;
                mapIterInit(successors, &iter);
                /* Add all elements from the successor live_in to live_out */
                while (mapIterNext(&iter)) {
                    IrBlock *successor_block = getValue(IrBlock *, iter.node);
                    IrLivenessInfo *successor_info = irLivenessGetInfo(analysis, successor_block);
                    if (successor_info) {
                        SetIter *set_iter = setIterNew(successor_info->live_in);
                        SetFor(set_iter, set_value) {
                            setAdd(new_live_out, set_value); 
                        }
                        setIterRelease(set_iter);
                    }
                }

                /* See if live_out changed */
                if (!setEq(info->live_out, new_live_out)) {
                    setRelease(info->live_out);
                    info->live_out = new_live_out;
                    changed = 1;
                } else {
                    setRelease(new_live_out);
                }

                /* Calculate new live_in = `use ∪ (live_out - def)` */
                Set *diff = setDifference(info->live_out, info->def);
                Set *new_live_in = setUnion(info->use, diff);

                if (!setEq(info->live_in, new_live_in)) {
                    setRelease(info->live_in);
                    info->live_in = new_live_in;
                    changed = 1;
                } else {
                    setRelease(new_live_in);
                }
            }
        }
    }

    return analysis;
}

int irInstrHasSideEffects(IrInstr *instr) {
    /* A memory write has a side effect */
    if (instr->opcode == IR_OP_STORE) {
        return 1;
    }

    /* Function calls could have a side effect, it might not but for simplicity
     * lets assume that it could, however we do want to nuke temporaries which
     * are the result of the function */
    if (instr->result == NULL && instr->opcode == IR_OP_CALL) {
        return 0;
    }

    if (instr->opcode == IR_OP_VA_START || instr->opcode == IR_OP_VA_ARG ||
        instr->opcode == IR_OP_VA_END) {
        return 1;
    }

    return 0;
}

int irInstrIsControlFlow(IrInstr *instr) {
    switch (instr->opcode) {
        case IR_OP_RET:
        case IR_OP_BR:
        case IR_OP_JMP:
        case IR_OP_LOOP:
        case IR_OP_SWITCH:
        case IR_OP_CALL:  // Calls can be considered control flow
            return 1;

        case IR_OP_PHI:
            return 0;

        case IR_OP_LABEL:
            return 0;

        default:
            return 0;
    }
}

int irInstrLive(IrBlock *block, IrLivenessInfo *info, IrInstr *instr) {
    if (!instr->result) {
        return 0;
    }

    SetIter *iter = NULL;
    if (instr->opcode == IR_OP_PHI) {
        for (iter = setIterNew(info->live_out); setIterNext(iter); ) {
            IrValue *value = getValue(IrValue *, iter);
            if (value == instr->result) {
                return 1;
            }
        }
        setIterRelease(iter);
    }

    /* Does the instruction live outside of this block? */
    for (iter = setIterNew(info->live_out); setIterNext(iter); ) {
        IrValue *value = getValue(IrValue *, iter);
        if (value == instr->result) {
            return 1;
        }
    }
    setIterRelease(iter);

    /* Check if it is used in any subsequent instruction */
    List *cur_list_node = NULL;
    listForEach(block->instructions) {
        IrInstr *needle = getValue(IrInstr *, it);
        if (needle == instr) {
            cur_list_node = it;
            break;
        }
    }

    /* It was not used anywhere else in the block! */
    if (!cur_list_node) {
        return 0;
    }

    /* Move past what we are currently pointing to as rather obviously the
     * instruction uses itself. */
    cur_list_node = cur_list_node->next;
    /* See if we have a match... */
    while (cur_list_node != block->instructions) {
        IrInstr *next_instr = getValue(IrInstr *, cur_list_node);

        /* Check the operands of the instruction */
        if (next_instr->result == instr->result ||
            next_instr->op1 == instr->result ||
            next_instr->op2 == instr->result ||
            next_instr->op3 == instr->result)
        {
            return 1;
        }

        /* See if it is used in a function argument */
        if (next_instr->opcode == IR_OP_CALL) {
            PtrVec *fn_args = next_instr->op1->array_.values;
            if (fn_args) {
                for (int i = 0; i < fn_args->size; ++i) {
                    IrValue *ir_value = vecGet(IrValue *,fn_args,i);
                    if (ir_value == instr->result) {
                        return 1;
                    }
                }
            }
        }

        /* Check all incoming values for a phi node */
        if (next_instr->opcode == IR_OP_PHI) {
            for (int i = 0; i < next_instr->extra.phi.pairs->size; ++i) {
                IrPair *pair = vecGet(IrPair *, next_instr->extra.phi.pairs, i);
                if (pair->ir_value == instr->result) {
                    return 1;
                }
            }
        }

        cur_list_node = cur_list_node->next;
    }

    /* Instruction is not used in this block or successors */
    return 0;
}


int irFnCallReturnUsed(IrBlock *block, IrLivenessInfo *info, IrInstr *instr) {
    /* Does the temorary live outside of this block? */
    SetIter *iter = NULL;
    if (instr->result->flags & IR_FN_VAL_USED) return 1;

    for (iter = setIterNew(info->live_out); setIterNext(iter); ) {
        IrValue *value = getValue(IrValue *, iter);
        if (value->name && aoStrEq(value->name, instr->result->name)) { // == instr->result) {
            return 1;
        }
    }
    setIterRelease(iter);

    /* Check if the temorary is used in any subsequent instruction */
    listForEach(block->instructions) {
        IrInstr *next_instr = getValue(IrInstr *, it);
        if (next_instr == instr) {
            continue;
        }

        if (next_instr->opcode == IR_OP_CALL) {
            PtrVec *fn_args = next_instr->op1->array_.values;
            if (fn_args && fn_args->size > 0) {
                for (int i = 0; i < fn_args->size; ++i) {
                    IrValue *ir_value = vecGet(IrValue *,fn_args,i);
                    if (ir_value->flags & IR_FN_VAL_USED) return 1;
                    if (ir_value->name && aoStrEq(ir_value->name,
                                                  instr->result->name)) {
                        return 1;
                    }
                }
            }
        }

        /* Check the operands of the instruction */
        if (next_instr->result == instr->result ||
            next_instr->op1 == instr->result ||
            next_instr->op2 == instr->result ||
            next_instr->op3 == instr->result)
        {
            return 1;
        }
    }

    return 0;
}

aoStr *irMemLocation(IrValue *value) {
    /* @Bug
     * Get element pointer might be a problem */
    return value->name;
}

int irStoreIsReadBefore(IrFunction *func, IrBlock *block, IrInstr *instr) {
    (void)func;
    List *list_node = NULL;
    listForEach(block->instructions) {
        IrInstr *needle = getValue(IrInstr *, it);
        if (needle == instr) {
            list_node = it;
            break;
        }
    }

    assert(list_node != NULL);

    while (list_node != block->instructions) {
        IrInstr *needle = getValue(IrInstr *, list_node);
        if (needle->opcode == IR_OP_LOAD) {
            aoStr *store_location = irMemLocation(needle->op1);
            aoStr *load_location = irMemLocation(needle->op2);
            /* Is this the same location? */
            if (aoStrEq(store_location, load_location)) {
                return 1;
            }
        } else if (needle->opcode == IR_OP_CALL) {
            loggerWarning("%s\n",instr->op1->name->data);
            /* This defensive, probably too much so, as it assumes a call 
             * can read any memory - perhaps should scan the function arguments?
             * */
            return 0;
        }
        list_node = list_node->next;
    }

    return 0;
}

int irCanPromoteAlloca(IrFunction *func, IrInstr *instr) {
    IrValue *slot = instr->result;
    if (!irTypeIsScalar(slot->type)) {
        return 0;
    }

    listForEach(func->blocks) {
        IrBlock *block = getValue(IrBlock *,it);
        if (block->removed) continue;

        listForEach(block->instructions) {
            IrInstr *needle = getValue(IrInstr *, it);

            /* Direct load from alloca is ok */
            if (needle->opcode == IR_OP_LOAD && needle->op1 == slot) {
                continue;
            }

            /* Direct store of alloca is ok */
            if (needle->opcode == IR_OP_STORE && needle->op1 == slot) {
                continue;
            }

            /* Any other use means we cannot promote this */
            if (needle->result == slot ||
                needle->op1 == slot ||
                needle->op2 == slot ||
                needle->op3 == slot)
            {
                return 0;
            }

            if (needle->opcode == IR_OP_PHI) {
                for (int i = 0; i < instr->extra.phi.pairs->size; ++i) {
                    IrPair *pair = vecGet(IrPair *, instr->extra.phi.pairs, i);
                    if (pair->ir_value == slot) {
                        return 0;
                    }
                }
            }
        }
    }

    return 1;
}

int irValueMaybeMatch(IrValue *v1, IrValue *v2) {
    if (!v1) return 0;
    if (!v2) return 0;
    if (v1 == v2) return 1;
    if (v1->name && v2->name) return aoStrEq(v1->name, v2->name);
    return 0;
}

/*
 * @Speed - this is a fairly expensive function to be calling regularly
 */
void irReplaceAllUses(IrFunction *func, IrValue *old_value, IrValue *new_value) {
    if (old_value == new_value) return;

    listForEach(func->blocks) {
        IrBlock *block = getValue(IrBlock *, it);
        if (block->removed) continue;

        listForEach(block->instructions) {
            IrInstr *instr = getValue(IrInstr *, it);
            /* Do not change the result field as that may change the definition 
             * of the instruction not its result */
            if (instr->opcode != IR_OP_ALLOCA && instr->opcode != IR_OP_STORE) {
                if (irValueMaybeMatch(instr->result, old_value)) {
                    instr->result = new_value;
                }
            }

            if (irValueMaybeMatch(instr->op1, old_value)) instr->op1 = new_value;
            if (irValueMaybeMatch(instr->op2, old_value)) instr->op2 = new_value;
            if (irValueMaybeMatch(instr->op3, old_value)) instr->op3 = new_value;

            if (instr->opcode == IR_OP_PHI) {
                for (int i = 0; i < instr->extra.phi.pairs->size; ++i) {
                    IrPair *pair = vecGet(IrPair *, instr->extra.phi.pairs, i);
                    if (irValueMaybeMatch(pair->ir_value, old_value)) {
                        pair->ir_value = new_value;
                    }
                }
            }

            /* See if it is used in a function argument */
            if (instr->opcode == IR_OP_CALL) {
                PtrVec *fn_args = instr->op1->array_.values;
                if (fn_args) {
                    for (int i = 0; i < fn_args->size; ++i) {
                        IrValue *ir_value = vecGet(IrValue *,fn_args,i);
                        if (irValueMaybeMatch(ir_value, old_value)) {
                            fn_args->entries[i] = new_value;
                        }
                    }
                }
            }

            if (instr->opcode == IR_OP_SWITCH) {
                listForEach(instr->extra.cases) {
                    IrPair *pair = getValue(IrPair *, it);
                    if (irValueMaybeMatch(pair->ir_value, old_value)) {
                        pair->ir_value = new_value;
                    }
                }
            }
        }
    }

    /* Update function parameters */
    for (int i = 0; i < func->params->size; ++i) {
        IrValue *param = vecGet(IrValue *, func->params, i);
        if (irValueMaybeMatch(param, old_value)) {
            func->params->entries[i] = new_value;
        }
    }
    
    /* Update global initialisers */
    MapIter *iter = mapIterNew(func->program->global_variables);
    while (mapIterNext(iter)) {
        IrValue *global = getValue(IrValue *, iter->node);
        if (global->kind == IR_VALUE_GLOBAL && irValueMaybeMatch(global->global.value, old_value)) {
            global->global.value = new_value;
        }
    }
    mapIterRelease(iter);
}

void irBlockRemoveNops(IrBlock *block) {
    List *nop_list = listNew();
    listForEach(block->instructions) {
        IrInstr *instr = getValue(IrInstr *, it);
        if (instr->opcode == IR_OP_NOP) {
            listAppend(nop_list, it);
        }
    }

    listForEach(nop_list) {
        List *nop_list_node = getValue(List *, it);
        listUnlink(block->instructions, nop_list_node);
    }

    listRelease(nop_list, NULL);
}

void irPerformCopyPropagation(IrFunction *func) {
    /* Track register value */
    Map *reg_values = irIntValueMapNew();
    /* Track memory content */
    Map *mem_values = irStrValueMapNew();

    listForEach(func->blocks) {
        IrBlock *block = getValue(IrBlock *, it);
        if (block->removed) continue;

        listForEach(block->instructions) {
            IrInstr *instr = getValue(IrInstr *, it);
            if (instr->opcode == IR_OP_CALL) {
                List *next_node = it->next;
                IrInstr *next = getValue(IrInstr *, next_node);
                if (next->opcode == IR_OP_STORE) {
                    if (next->op1 == instr->result) {
                        next->opcode = IR_OP_NOP;
                        instr->result = next->result;
                        irReplaceAllUses(func, next->op1, next->result);
                    }
                }
            } else if (instr->opcode == IR_OP_LOAD) {
                aoStr *memory_location = irMemLocation(instr->op1);
                if (!memory_location) continue; //|| irCouldModifyMemory(instr)) continue;

                if (mapHas(mem_values, memory_location)) {
                    IrValue *value = mapGet(mem_values, memory_location);
                    /* Replace the load with the known value */
                    irReplaceAllUses(func, instr->result, value);
                    instr->opcode = IR_OP_NOP;
                } else {
                    /* Store what comes from the location */
                    mapAdd(mem_values, memory_location, instr->op1);
                }
            } else if (instr->opcode == IR_OP_STORE && instr->op1->kind != IR_VALUE_CONST_INT) {
                aoStr *memory_location = irMemLocation(instr->result);
                if (!memory_location) continue;
                mapAdd(mem_values, memory_location, instr->op1);

                /* Check if the store is immediately used */
                int store_needed = 0;
                List *next_node = it->next;
                while (next_node != block->instructions) {
                    IrInstr *next = getValue(IrInstr *, next_node);
                    if (next->opcode == IR_OP_LOAD) {
                        /* @Speed & Simplicity, the 'name' on the value should 
                         * _really_ be an int, then all comparisons are cheap */
                        aoStr *load_location = irMemLocation(next->op1);
                        if (aoStrEq(memory_location, load_location)) {
                            store_needed = 1;
                        }
                    } else if (next->opcode == IR_OP_STORE) {
                        aoStr *next_location = irMemLocation(next->result);
                        if (aoStrEq(memory_location, next_location)) {
                            /* The store is overwritten before being read */
                            store_needed = 0;
                        }
                    } else if (irCouldReadMemory(next)) {
                        store_needed = 1;
                    }

                    if (instr->opcode == IR_OP_CALL) {
                        PtrVec *fn_args = instr->op1->array_.values;
                        if (fn_args && fn_args->size > 0) {
                            /* We could analyse the body of a function pointer, however 
                             * this is conservative and simply returns true */
                            for (int i = 0; i < fn_args->size; ++i) {
                                IrValue *ir_value = vecGet(IrValue *,fn_args,i);
                                if (ir_value->name && aoStrEq(ir_value->name, memory_location)) {
                                    store_needed = 1;
                                }
                            }
                        }
                    }

                    next_node = next_node->next;
                }

                if (!store_needed) {
                    /* So we want to delete the result and make everything that was 
                     * pointting to the result to point to op1 the value that 
                     * we were storing, it's essentially replacing all uses from 
                     * a point in time and then we'll run a 'remove redundant allocas'
                     * pass */
                    irReplaceAllUses(func, instr->result, instr->op1);
                    instr->opcode = IR_OP_NOP;
                }
            } else if (irCouldModifyMemory(instr) && instr->opcode != IR_OP_ALLOCA) {
                mapClear(mem_values);
            }
        }

        irBlockRemoveNops(block);
        mapClear(mem_values);
        mapClear(reg_values);
    }

    mapRelease(mem_values);
    mapRelease(reg_values);
}

/* Where the same location is read multiple times without any writes */
void irEliminateRedundantLoads(IrFunction *func) {
    /* memory locations to the last loaded value */
    Map *load_map = irStrValueMapNew();

    listForEach(func->blocks) {
        IrBlock *block = getValue(IrBlock *, it);
        if (block->removed) continue;

        listForEach(block->instructions) {
            IrInstr *instr = getValue(IrInstr *, it);

            if (instr->opcode == IR_OP_LOAD) {
                aoStr *memory_location = irMemLocation(instr->op1);
                if (mapHas(load_map, memory_location)) {
                    IrValue *prev = mapGet(load_map, memory_location);
                    irReplaceAllUses(func, instr->result, prev);
                    instr->opcode = IR_OP_NOP;
                } else {
                    mapAdd(load_map, memory_location, instr->op1);
                }
            } else if (instr->opcode == IR_OP_CALL || irCouldModifyMemory(instr)) {
                mapClear(load_map);
            }
        }

        /* Filter out the instructions that are no longer needed */
        irBlockRemoveNops(block);
        mapClear(load_map);
    }
    mapRelease(load_map);
}


/* Removes stores that are never read or immediately overwritten */
void irEliminateRedundantStores(IrFunction *func) {
    /* memory locations to the last loaded value */
    Map *store_map = irStrInstrMapNew();

    listForEach(func->blocks) {
        IrBlock *block = getValue(IrBlock *, it);
        if (block->removed) continue;

        List *redundant_stores = listNew();

        listForEach(block->instructions) {
            IrInstr *instr = getValue(IrInstr *, it);
            if (instr->opcode == IR_OP_STORE) {
                aoStr *memory_location = irMemLocation(instr->op1);
                if (!memory_location) continue;

                if (mapHas(store_map, memory_location)) {
                    IrInstr *prev = mapGet(store_map, memory_location);
                    if (irValuesEq(prev->op1, instr->op1)) {
                        listAppend(redundant_stores, it);
                    } else {
                        if (!irStoreIsReadBefore(func, block, prev)) {
                            listAppend(redundant_stores, prev);
                        }

                        mapAdd(store_map, memory_location, instr);
                    }
                } else {
                    mapAdd(store_map, memory_location, instr);
                }
            } else if (instr->opcode == IR_OP_CALL || irCouldModifyMemory(instr)) {
                mapClear(store_map);
            }
        }

        listForEach(redundant_stores) {
            List *store_list_node = getValue(List *, it);
            listUnlink(block->instructions, store_list_node);
        }

        listRelease(redundant_stores, NULL);
        mapClear(store_map);
    }
    mapRelease(store_map);
}

/* Directly connect stores with their subsequent loads */
void irPerformLoadStoreForwarding(IrFunction *func) {
    /* Tracks the last stored value for each memory location */
    Map *last_store = irStrValueMapNew();

    listForEach(func->blocks) {
        IrBlock *block = getValue(IrBlock *, it);
        if (block->removed) continue;

        listForEach(block->instructions) {
            IrInstr *instr = getValue(IrInstr *, it);

            if (instr->opcode == IR_OP_STORE) {
                aoStr *memory_location = irMemLocation(instr->result);
                mapAdd(last_store, memory_location, instr->op1);
            } else if (instr->opcode == IR_OP_LOAD) {
                aoStr *memory_location = irMemLocation(instr->result);
                if (mapHas(last_store, memory_location)) {
                    IrValue *stored = mapGet(last_store, memory_location);
                    irReplaceAllUses(func, instr->result, stored);
                    instr->opcode = IR_OP_NOP;
                }
            } else if (instr->opcode == IR_OP_CALL) {
                mapClear(last_store);
            } else if (irCouldModifyMemory(instr)) {
                mapClear(last_store);
            }
        }

        irBlockRemoveNops(block);
        mapClear(last_store);
    }
    mapRelease(last_store);
}

void irReplaceLoadStores(IrFunction *func, IrValue *stack_slot, IrValue *reg_value) {
    IrValue *current_value = irValueNew(reg_value->type, IR_VALUE_UNDEFINED);
    Map *block_values = irIntValueMapNew();

    listForEach(func->blocks) {
        IrBlock *block = getValue(IrBlock *, it);
        if (block->removed) continue;

        listForEach(block->instructions) {
            IrInstr *instr = getValue(IrInstr *, it);

            if (instr->opcode == IR_OP_LOAD && instr->op1 == stack_slot) {
                irReplaceAllUses(func, instr->result, current_value);
            } else if (instr->opcode == IR_OP_STORE && instr->op1 == stack_slot) {
                current_value = instr->op2;
                instr->opcode = IR_OP_NOP;
            }
        }

        mapAdd(block_values, ptrcast(block->id), current_value);
    }

    listForEach(func->blocks) {
        IrBlock *block = getValue(IrBlock *, it);
        if (block->removed) continue;
        irBlockRemoveNops(block);
    }

    mapRelease(block_values);
}

void irEliminateDeadStores(IrFunction *func) {
    int changed = 1;
    Set *mem_uses = setNew(32, &aostr_set_type);

    while (changed) {
        changed = 0;

        /* First pass is to find all loads */
        listForEach(func->blocks) {
            IrBlock *block = getValue(IrBlock *, it);
            if (block->removed) continue;

            listForEach(block->instructions) {
                IrInstr *instr = getValue(IrInstr *, it);

                if (instr->opcode == IR_OP_LOAD) {
                    aoStr *memory_location = irMemLocation(instr->op1);
                    if (!memory_location) continue;
                    setAdd(mem_uses, memory_location);
                } else if (instr->opcode == IR_OP_STORE) {
                    aoStr *memory_location = irMemLocation(instr->op1);
                    if (memory_location && setHas(mem_uses, memory_location)) {
                        instr->opcode = IR_OP_NOP;
                        continue;
                    }
                }
                /* Check if function arguments use it */
                else  if (instr->opcode == IR_OP_CALL) {
                    PtrVec *fn_args = instr->op1->array_.values;
                    if (fn_args && fn_args->size > 0) {
                        /* We could analyse the body of a function pointer, however 
                         * this is conservative and simply returns true */
                        for (int i = 0; i < fn_args->size; ++i) {
                            IrValue *ir_value = vecGet(IrValue *,fn_args,i);
                            if (ir_value->name) {
                                setAdd(mem_uses, ir_value->name);
                            }
                        }
                    }
                } else if (instr->result && instr->result->kind == IR_VALUE_TEMP) {
                    aoStr *memory_location = irMemLocation(instr->result);
                    if (!memory_location) continue;
                    setAdd(mem_uses, memory_location);
                }
                /* Check for implicit memory reads like function calls */
                else if (irCouldReadMemory(instr) && instr->opcode != IR_OP_LOAD) {
                    /* This defensively assumes it could read any memory and 
                     * marks allocas as possibly read */
                    listForEach(func->blocks) {
                        IrBlock *block = getValue(IrBlock *, it);
                        if (block->removed) continue;

                        listForEach(block->instructions) {
                            IrInstr *inner_instr = getValue(IrInstr *, it);
                            
                            if (inner_instr->opcode == IR_OP_ALLOCA) {
                                aoStr *memory_location = irMemLocation(inner_instr->result);
                                setAdd(mem_uses, memory_location);
                            }
                        }
                    }
                }
            }
        }

        /* Pass two */
        listForEach(func->blocks) {
            IrBlock *block = getValue(IrBlock *, it);
            if (block->removed) continue;

            listForEach(block->instructions) {
                IrInstr *instr = getValue(IrInstr *, it);
                if (instr->opcode == IR_OP_STORE) {
                    aoStr *memory_location = irMemLocation(instr->op1);
                    if (!memory_location) continue;
                    if (!setHas(mem_uses, memory_location)) {
                        instr->opcode = IR_OP_NOP;
                        changed = 1;
                    }
                }
            }
        }

        /* Remove noops */
        listForEach(func->blocks) {
            IrBlock *block = getValue(IrBlock *, it);
            if (block->removed) continue;
            irBlockRemoveNops(block);
        }

        setClear(mem_uses);
    }
}

int irInstrCouldUsePointer(IrFunction *func, IrInstr *instr) {
    /* CASE 1: Direct memory operations - these explicitly use pointers */
    if (instr->opcode == IR_OP_LOAD || instr->opcode == IR_OP_STORE) {
        /* Only the pointer operand matters (op1 in both cases) */
        return instr->op1->name && mapHas(func->allocas, instr->op1->name);
    }

    /* CASE 2: GetElementPtr (array/struct indexing) */
    if (instr->opcode == IR_OP_GEP) {
        /* The base address (op1) could be an alloca-derived pointer */
        return mapHas(func->allocas, instr->op1->name);
    }
    
    /* CASE 3: Pointer arithmetic and conversions */
    if (instr->opcode == IR_OP_PTRTOINT || instr->opcode == IR_OP_INTTOPTR) {
        return mapHas(func->allocas, instr->op1->name);
    }
    
    /* CASE 4: Pointer comparisons */
    if (instr->opcode == IR_OP_ICMP) {
        /* Check if either operand could be a pointer (based on type) */
        if (instr->op1->type == IR_TYPE_PTR) {
            return mapHas(func->allocas, instr->op1->name);
        }
        if (instr->op2->type == IR_TYPE_PTR) {
            return mapHas(func->allocas, instr->op2->name);
        }
        return 0;
    }
    
    /* CASE 5: Bitcasts of pointers */
    if (instr->opcode == IR_OP_BITCAST) {
        if (instr->op1->type == IR_TYPE_PTR) {
            return mapHas(func->allocas, instr->op1->name);
        }
        return 0;
    }
    
    /* CASE 6: Function calls - check if alloca-derived pointer is passed */
    if (instr->opcode == IR_OP_CALL) {
        PtrVec *fn_args = instr->op1->array_.values;
        if (fn_args && fn_args->size > 0) {
            /* We could analyse the body of a function pointer, however 
             * this is conservative and simply returns true */
            for (int i = 0; i < fn_args->size; ++i) {
                IrValue *ir_value = vecGet(IrValue *,fn_args,i);
                if (ir_value->name) {
                    if (mapHas(func->allocas, ir_value->name)) {
                        return 1;
                    }
                }
            }
        }
        return 0;
    }
    
    /* CASE 7: Select instruction (conditional selection) */
    if (instr->opcode == IR_OP_SELECT) {
        /* If either value could be an alloca-derived pointer */
        if (mapHas(func->allocas, instr->op2->name) || 
            mapHas(func->allocas, instr->op3->name))
            return 1;
        return 0;
    }
    
    /* CASE 8: Phi nodes (for SSA form) */
    if (instr->opcode == IR_OP_PHI) {
        /* Check each incoming value */
        for (int i = 0; i < instr->extra.phi.pairs->size; i++) {
            IrPair *pair = vecGet(IrPair *, instr->extra.phi.pairs, i);
            if (mapHas(func->allocas, pair->ir_value->name))
                return 1;
        }
        return 0;
    }
    
    /* Integer/floating point operations don't use pointers */
    if ((instr->opcode >= IR_OP_IADD && instr->opcode <= IR_OP_FNEG) ||
        (instr->opcode >= IR_OP_AND && instr->opcode <= IR_OP_NOT))
        return 0;
    
    /* Non-pointer conversion operations */
    if ((instr->opcode >= IR_OP_TRUNC && instr->opcode <= IR_OP_SEXT) ||
        (instr->opcode >= IR_OP_FPTRUNC && instr->opcode <= IR_OP_SITOFP))
        return 0;
    
    /* Control flow instructions don't use pointers (except their operands) */
    if (instr->opcode == IR_OP_BR || instr->opcode == IR_OP_JMP || 
        instr->opcode == IR_OP_LOOP || instr->opcode == IR_OP_LABEL)
        return 0;
    
    /* SWITCH might have pointer comparisons in some IRs, but usually not */
    if (instr->opcode == IR_OP_SWITCH) {
        // Only if the condition is pointer-derived
        return mapHas(func->allocas, instr->op1->name);
    }
    
    /* For VA_ARG, VA_START, and VA_END - these manipulate va_list
     * which might contain pointers, but not usually alloca pointers */
    if (instr->opcode == IR_OP_VA_ARG || 
        instr->opcode == IR_OP_VA_START || 
        instr->opcode == IR_OP_VA_END)
        return 0;

    /* Return instruction could return a pointer */
    if (instr->opcode == IR_OP_RET) {
        IrValue *ret_val = instr->result;
        if (ret_val && ret_val->type == IR_TYPE_VOID) {
            return 0;
        }
    }
    
    /* For everything else assume it to be true */
    return 1;
}


void irEliminateDeadAllocas(IrFunction *func) {
    int changed = 1;
    Set *used_allocas = setNew(16, &aostr_set_type);

    while (changed) {
        changed = 0;
        listForEach(func->blocks) {
            IrBlock *block = getValue(IrBlock *, it);
            if (block->removed) continue;

            listForEach(block->instructions) {
                IrInstr *instr = getValue(IrInstr *, it);

                if (instr->opcode == IR_OP_STORE && instr->result &&
                        instr->result->name &&
                        mapHas(func->allocas, instr->result->name)) {
                    setAdd(used_allocas, instr->result->name);
                }

                if (instr->opcode == IR_OP_LOAD && instr->op1 &&
                        instr->op1->name && 
                        mapHas(func->allocas, instr->op1->name)) {
                    setAdd(used_allocas, instr->op1->name);
                }

                if (instr->opcode == IR_OP_GEP && instr->op1 && 
                        mapHas(func->allocas, instr->op1->name)) {
                    setAdd(used_allocas, instr->op1->name);
                }

                if (instr->opcode != IR_OP_ALLOCA && irInstrCouldUsePointer(func, instr)) {
                    listForEach(func->blocks) {
                        IrBlock *inner_block = getValue(IrBlock *, it);
                        if (inner_block->removed) continue;

                        listForEach(block->instructions) {
                            IrInstr *inner_instr = getValue(IrInstr *, it);
                            if (inner_instr->opcode == IR_OP_ALLOCA) {
                                setAdd(used_allocas, instr->result->name);
                            }
                        }
                    }
                }
            }
        }

        /* Pass 2 removed unused allocas */
        listForEach(func->blocks) {
            IrBlock *block = getValue(IrBlock *, it);
            if (block->removed) continue;

            listForEach(block->instructions) {
                IrInstr *instr = getValue(IrInstr *, it);
                if (instr->opcode == IR_OP_ALLOCA) {
                    /* If unused nuke it */
                    if (!setHas(used_allocas, instr->result->name)) {
                        instr->opcode = IR_OP_NOP;
                        changed = 1;
                    }
                }
            }
        }
        listForEach(func->blocks) {
            IrBlock *block = getValue(IrBlock *, it);
            if (block->removed) continue;
            irBlockRemoveNops(block);
        }

        setClear(used_allocas);
    }

    setRelease(used_allocas);
}

void irFoldParameterLoads(IrFunction *func) {
    /* Add all parameters to the set as though we are going to delete them 
     * then if there are loads we will 'undelete' them */
    Set *eliminate_params = irValueSetNew();
    for (int i = 0; i < func->params->size; ++i) {
        IrValue *param = vecGet(IrValue *, func->params, i);
        setAdd(eliminate_params, param);
    }

    listForEach(func->blocks) {
        IrBlock *block = getValue(IrBlock *, it);
        if (block->removed) continue;

        listForEach(block->instructions) {
            IrInstr *instr = getValue(IrInstr *, it);
            if (instr->opcode == IR_OP_LOAD && instr->op1 && instr->op1->kind == IR_VALUE_PARAM) {
                irReplaceAllUses(func, instr->result, instr->op1);
                setRemove(eliminate_params, instr->op1);
                instr->opcode = IR_OP_NOP;
            }
        }

        irBlockRemoveNops(block);
    }

    if (eliminate_params->size) {
        PtrVec *new_params = ptrVecNew();
        for (int i = 0; i < func->params->size; ++i) {
            IrValue *param = vecGet(IrValue *, func->params, i);
            if (!setHas(eliminate_params, param)) {
                ptrVecPush(new_params, param);
            }
        }
        ptrVecRelease(func->params);
        func->params = new_params;
    }
    setRelease(eliminate_params);
}

/* @Optimiser - This is veering on optimisiation */
IrLivenessAnalysis *irEliminateDeadCode(IrFunction *func,
                                        IrLivenessAnalysis *liveness)
{
    int changed = 1;
    int re_compute = 0;

    while (changed) {
        changed = 0;

        listForEach(func->blocks) {
            IrBlock *block = getValue(IrBlock *, it);
            if (block->removed) continue;
            IrLivenessInfo *info = irLivenessGetInfo(liveness, block);
            List *dead_list = listNew();

            listForEach(block->instructions) {
                IrInstr *instr = getValue(IrInstr *, it);

                /* We want to nuke temorary variables which are used to capture 
                 * the return value on a function call if they are unused. */
                if (instr->result && instr->opcode == IR_OP_CALL) {
                    if (!irFnCallReturnUsed(block, info, instr)) {
                        instr->result = NULL;
                        continue;
                    }
                }
                if (irInstrHasSideEffects(instr)) continue;
                if (irInstrIsControlFlow(instr)) continue;
                if (instr->result && !irInstrLive(block, info, instr)) {
                    listAppend(dead_list, it);
                }
            }

            listForEach(dead_list) {
                List *instr_list_node = getValue(List *, it);
                listUnlink(block->instructions, instr_list_node);
                re_compute = 1;
                changed = 1;
            }

            listRelease(dead_list, NULL);
        }
    }

    if (re_compute) {
        liveness = irLivenessAnalysis(func);
    }
    return liveness;
}

void irOptimiseFunction(IrFunction *func) {
    int i = 0;
    while (i  < 5) {
        IrLivenessAnalysis *analysis = irLivenessAnalysis(func);
        analysis = irEliminateDeadCode(func, analysis);
        irPerformLoadStoreForwarding(func);
        irEliminateRedundantLoads(func);
        irEliminateRedundantStores(func);
        irPerformCopyPropagation(func);
        analysis = irEliminateDeadCode(func, analysis);
        irPerformCopyPropagation(func);
        //irEliminateDeadStores(func);
        irEliminateRedundantStores(func);
        irFoldParameterLoads(func);
        irEliminateDeadAllocas(func);
        irPerformCopyPropagation(func);
        i++;
    }
}
