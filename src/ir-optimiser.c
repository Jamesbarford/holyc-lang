#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "aostr.h"
#include "debug.h"
#include "map.h"
#include "list.h"
#include "util.h"
#include "ir-types.h"
#include "ir-interp.h"
#include "ir-optimiser.h"

int irInstrHasSideEffects(IrInstr *instr);
int irFnCallReturnUsed(IrBlock *block, IrLivenessInfo *info, IrInstr *instr);
AoStr *irMemLocation(IrValue *value);
void irReplaceAllUses(IrFunction *func, IrValue *old_value, IrValue *new_value);
void irBlockRemoveNops(IrBlock *block);
int irCanPromoteAlloca(IrFunction *func, IrInstr *instr);
int irStoreIsReadBefore(IrFunction *func, IrBlock *block, IrInstr *instr);
int irInstrLive(IrBlock *block, IrLivenessInfo *info, IrInstr *instr);

/* Optimisation passese */
void irEliminateRedundantLoads(IrFunction *func);
void irEliminateRedundantStores(IrFunction *func);
void irEliminateDeadAllocas(IrFunction *func);
void irPerformLoadStoreForwarding(IrFunction *func);
void irPerformCopyPropagation(IrFunction *func);
void irFoldParameterLoads(IrFunction *func);


/*====== DATA STRUCTURES =====================================================*/
/* `Map<AoStr *, Set<IrValue *>>`*/
static MapType ir_value_chain_map = {
    .match           = (mapKeyMatch *)&aoStrEq,
    .hash            = (mapKeyHash *)&aoStrHashFunction,
    .get_key_len     = (mapKeyLen *)&aoStrGetLen,
    .key_to_string   = (mapKeyToString *)&debugColourAoStr,
    .value_to_string = (mapValueToString *)&setEntriesToString,
    .value_release   = (mapValueRelease *)&setRelease,
    .value_type      = "Set<IrValue *>",
    .key_type        = "AoStr *",
};

/* `Map<AoStr *, Set<IrValue *>>`*/
Map *irValueChainMap(void) {
    return mapNew(16, &ir_value_chain_map);
}

/* This can _only_ be used for get element pointer instructions */
unsigned long irInstrGepHash(IrInstr *instr) {
    assert(instr->opcode == IR_OP_GEP);
    const size_t capacity = 32;
    char tmp[capacity];
    AoStr tmp_str = {
        .capacity = capacity,
        .data = tmp,
        .len = 0,
    };

    if (instr->op2->name != NULL) {
        tmp_str.len = snprintf(tmp, sizeof(tmp), "%s%s",instr->op1->name->data,
                                                        instr->op2->name->data);
    } else if (instr->op2->kind == IR_VALUE_CONST_INT) {
        tmp_str.len = snprintf(tmp, sizeof(tmp), "%s%ld",instr->op1->name->data,
                                                        instr->op2->i64);
    } else {
        loggerPanic("Unhandled hash for; %s\n", irInstrToString(instr)->data);
    }

    debug("%s\n", tmp);
    return aoStrHashFunction(&tmp_str);
}

int irInstrGepMatch(IrInstr *i1, IrInstr *i2) {
    if (i1->opcode != IR_OP_GEP) return 0;
    if (i2->opcode != IR_OP_GEP) return 0;
    if (i1->op1->type == i2->op1->type && i1->op2->type == i2->op2->type) {
        if (aoStrEq(i1->op1->name, i2->op1->name)) {
            if (i1->op2->kind == IR_VALUE_CONST_INT) {
                return i1->op2->i64 == i2->op2->i64;
            } else if (i1->op2->name && i2->op2->name) {
                return aoStrEq(i1->op2->name, i2->op2->name);
            }
        }
    }
    return 0;
}

/* `Map<IrInstr *, Set<IrValue *>>`*/
static MapType ir_gep_chain_map = {
    .match           = (mapKeyMatch *)&irInstrGepMatch,
    .hash            = (mapKeyHash *)&irInstrGepHash,
    .get_key_len     = (mapKeyLen *)&aoStrGetLen,
    .key_to_string   = (mapKeyToString *)&irInstrToString,
    .value_to_string = (mapValueToString *)&setEntriesToString,
    .value_release   = (mapValueRelease *)&setRelease,
    .value_type      = "Set<IrValue *>",
    .key_type        = "IrInstr *",
};

/* `Map<AoStr *, Set<IrValue *>>`*/
Map *irGepChainMap(void) {
    return mapNew(16, &ir_gep_chain_map);
}

typedef struct IrInstrIter {
    List *block_it;
    List *blocks;
    List *instr_it;
    IrBlock *block;
    IrInstr *instr;
} IrInstrIter;

void irInstrIterNextInit(IrInstrIter *it, IrFunction *func) {
    it->blocks = func->blocks;
    it->block_it = it->blocks->next;
    it->block = it->block_it->value;
    it->instr_it = it->block->instructions->next;
    it->instr = it->instr_it->value;
}

/* Iterate through instructions */
int irInstrIterNext(IrInstrIter *it) {
    if (it->instr_it != it->block->instructions) {
        IrInstr *value = it->instr_it->value;
        it->instr_it = it->instr_it->next;
        it->instr = value;
        return 1;
    } else if (it->block_it != it->blocks) {
        it->block_it = it->block_it->next;
        while (it->block_it != it->blocks) {
            it->block = it->block_it->value;
            if (!listEmpty(it->block->instructions)) {
                it->instr_it = it->block->instructions->next;
                it->instr = it->instr_it->value;
                /* Set iterator to next instruction */
                it->instr_it = it->instr_it->next;
                return 1;
            }
            it->block_it = it->block_it->next;
        }
        return 0;
    }
    return 0;
}

void irInstrIterPrevInit(IrInstrIter *it, IrFunction *func) {
    it->blocks = func->blocks;
    it->block_it = it->blocks->prev;
    it->block = it->block_it->value;
    it->instr_it = it->block->instructions->prev;
    it->instr = it->instr_it->value;
}

int irInstrIterPrev(IrInstrIter *it) {
    if (it->instr_it != it->block->instructions) {
        IrInstr *value = it->instr_it->value;
        it->instr_it = it->instr_it->prev;
        it->instr = value;
        return 1;
    } else if (it->block_it != it->blocks) {
        it->block_it = it->block_it->prev;
        while (it->block_it != it->blocks) {
            it->block = it->block_it->value;
            if (!listEmpty(it->block->instructions)) {
                it->instr_it = it->block->instructions->prev;
                it->instr = it->instr_it->value;
                /* Set iterator to prev instruction */
                it->instr_it = it->instr_it->prev;
                return 1;
            }
            it->block_it = it->block_it->prev;
        }
        return 0;
    }
    return 0;
}

/*============================================================================*/

IrLivenessInfo *irLivenessInfoNew(void) {
    IrLivenessInfo *info = (IrLivenessInfo *)malloc(sizeof(IrLivenessInfo));
    info->live_in = irValueSetNew();
    info->live_out = irValueSetNew();
    info->def = irValueSetNew();
    info->use = irValueSetNew();
    return info;
}

IrLivenessInfo *irLivenessGetInfo(IrLivenessAnalysis *liveness, IrBlock *block) {
    return (IrLivenessInfo *)mapGet(liveness->block_info, block->id);
}

int irInstrHasPtr(IrInstr *instr) {
    IrValue *values[] = {instr->result, instr->op1, instr->op2};
    for (int i = 0; i < 3; ++i) {
        IrValue *value = values[i];
        if (value && irIsPtr(value->type)) {
            return 1;
        }
    }
    return 0;
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

AoStr *irLivenessAnalysisToString(IrLivenessAnalysis *analysis) {
    AoStr *buf = aoStrNew();
    aoStrCatFmt(buf, "IrLivenessAnalysis {\n");
    AoStr *tmp = mapToString(analysis->block_info, ",\n");
    aoStrCatFmt(buf, "%S\n}\n", tmp);
    aoStrRelease(tmp);
    return buf;
}

void irLivenessAnalysisPrint(IrLivenessAnalysis *analysis) {
    AoStr *str = irLivenessAnalysisToString(analysis);
    printf("%s\n",str->data);
    aoStrRelease(str);
}

Map *irLivenessAnalysis(IrFunction *func) {
    Map *liveness_map = irLivenessMap();

    listForEachReverse(func->blocks) {
        IrBlock *block = listValue(IrBlock *, it);
        IrLivenessInfo *info = irComputeBlockUseDef(block);
        mapAdd(liveness_map, block->id, info);
    }

    int changed = 1;
    while (changed) {
        changed = 0;
        listForEachReverse(func->blocks) {
            IrBlock *block = listValue(IrBlock *, it);
            IrLivenessInfo *info = mapGet(liveness_map, block->id);
            Map *successors = irBlockGetSuccessors(func, block);

            /* Get the successors from the CFG */
            if (successors) {
                Set *new_live_out = irValueSetNew();
                MapIter iter;
                mapIterInit(successors, &iter);
                /* Add all elements from the successor live_in to live_out */
                while (mapIterNext(&iter)) {
                    IrBlock *successor_block = getValue(IrBlock *, iter.node);
                    IrLivenessInfo *successor_info = mapGet(liveness_map, block->id);
                    if (successor_info) {
                        SetIter set_iter;
                        setIterInit(&set_iter, successor_info->live_in);
                        for (setIterInit(&set_iter, successor_info->live_in); setIterNext(&set_iter); ) {
                            setAdd(new_live_out, set_iter.value); 
                        }
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

    IrLivenessInfo *info = mapGet(liveness_map, 0);
    setPrint(info->def);
    return liveness_map;
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

    SetIter iter;
    if (instr->opcode == IR_OP_PHI) {
        for (setIterInit(&iter, info->live_out); setIterNext(&iter); ) {
            IrValue *value = iter.value;
            if (value == instr->result) {
                return 1;
            }
        }
    }

    /* Does the instruction live outside of this block? */
    for (setIterInit(&iter, info->live_out); setIterNext(&iter); ) {
        IrValue *value = iter.value;
        if (value == instr->result) {
            return 1;
        }
    }

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
    if (!info) return 1;

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

AoStr *irMemLocation(IrValue *value) {
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
            AoStr *store_location = irMemLocation(needle->op1);
            AoStr *load_location = irMemLocation(needle->op2);
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


    IrInstrIter it;
    irInstrIterNextInit(&it, func);
    while (irInstrIterNext(&it)) {
        IrInstr *instr = it.instr;
        /* Do not change the result field as that may change the definition 
         * of the instruction not its result */
        if (instr->opcode != IR_OP_ALLOCA && instr->opcode != IR_OP_STORE) {
            if (irValueMaybeMatch(instr->result, old_value)) {
                instr->result = new_value;
            }
        }

        if (irValueMaybeMatch(instr->result, old_value)) instr->result = new_value;
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
                AoStr *memory_location = irMemLocation(instr->op1);
                if (!memory_location || irCouldModifyMemory(instr)) continue;

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
                AoStr *memory_location = irMemLocation(instr->result);
                //if (!memory_location) continue;
                if (!memory_location || irCouldModifyMemory(instr)) continue;
                mapAdd(mem_values, memory_location, instr->op1);

                /* Check if the store is immediately used */
                int store_needed = 0;
                List *next_node = it->next;
                while (next_node != block->instructions) {
                    IrInstr *next = getValue(IrInstr *, next_node);
                    if (next->opcode == IR_OP_LOAD) {
                        /* @Speed & Simplicity, the 'name' on the value should 
                         * _really_ be an int, then all comparisons are cheap */
                        AoStr *load_location = irMemLocation(next->op1);
                        if (aoStrEq(memory_location, load_location)) {
                            store_needed = 1;
                        }
                    } else if (next->opcode == IR_OP_STORE) {
                        AoStr *next_location = irMemLocation(next->result);
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
                AoStr *memory_location = irMemLocation(instr->op1);
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
                AoStr *memory_location = irMemLocation(instr->op1);
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
                AoStr *memory_location = irMemLocation(instr->result);
                mapAdd(last_store, memory_location, instr->op1);
            } else if (instr->opcode == IR_OP_LOAD) {
                AoStr *memory_location = irMemLocation(instr->result);
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
                                if (!instr->result) {
                                    debug("%s\n", irInstrToString(instr)->data);
                                }
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
Map *irEliminateDeadCode(IrFunction *func, Map *liveness) {
    int changed = 1;
    int re_compute = 0;

    while (changed) {
        changed = 0;

        listForEach(func->blocks) {
            IrBlock *block = getValue(IrBlock *, it);
            if (block->removed) continue;
            IrLivenessInfo *info = mapGet(liveness, ptrcast(block->id));
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

/* Given `target_name` the name of the `IrValue` to replace replace it with
 * `replacement` */
void irForwardPropagate(IrFunction *func, AoStr *target_name, IrValue *replacement) {
    listForEach(func->blocks) {
        IrBlock *block = getValue(IrBlock *, it);
        if (block->removed) continue;
        listForEach(block->instructions) {
            IrInstr *instr = getValue(IrInstr *, it);
            AoStr *result_name = irValueGetName(instr->result);
            AoStr *op1_name = irValueGetName(instr->op1);
            AoStr *op2_name = irValueGetName(instr->op2);

            if (aoStrEq(target_name, result_name)) {
                debug("target == result_name `%s`\n", irInstrToString(instr)->data);
            } else if (aoStrEq(target_name, op1_name)) {
                debug("target == op1_name `%s`\n", irInstrToString(instr)->data);
            } else if (aoStrEq(target_name, op2_name)) {
                debug("target == op2_name `%s`\n", irInstrToString(instr)->data);
            }
        }
    }
}

static void irChainReplaceAllUses(Map *chain_map, IrFunction *func, IrValue *old_value, IrValue *new_value) {
    if (old_value == new_value) return;

    IrInstrIter it;
    irInstrIterNextInit(&it, func);
    while (irInstrIterNext(&it)) {
        IrInstr *instr = it.instr;
        /* Do not change the result field as that may change the definition 
         * of the instruction not its result */
        if (instr->opcode != IR_OP_ALLOCA && instr->opcode != IR_OP_STORE) {
            if (irValueMaybeMatch(instr->result, old_value)) {
                instr->result = new_value;
            }
        }

        if (irValueMaybeMatch(instr->result, old_value)) instr->result = new_value;
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

    /* Prevent the chains from updating to an incorrect value */
    iter = mapIterNew(chain_map);
    mapRemove(chain_map, old_value->name);
    while (mapIterNext(iter)) {
        setRemove(iter->node->value, old_value);
    }
    mapIterRelease(iter);
}

/**
 * Creates various mappings between variable definitions and their usage. For example; 
 * ```
 * store %t1, %t2
 * add   %t3, %t1, %t4
 * ret   %t3
 * ```
 * Would map
 * ```
 * %t1 => %t2
 * ```
 * Which would optimise to;
 * ```
 * add   %t3, %2, %t4
 * ret   %t3
 * ```
 * Thus removing a store */
static void irBuildChain(Map *chain_map, IrInstr *instr, IrInstr *maybe_prev_instr) {
    AoStr *result_name = irValueGetName(instr->result);
    AoStr *op1_name = irValueGetName(instr->op1);
    AoStr *op2_name = irValueGetName(instr->op2);

    /* If the instruction is a call and the next operation is to store
     * the return value... we'll update the return value to be the 
     * store operation's variable and mark the store as obsolete. */
    switch (instr->opcode) {
        case IR_OP_CALL: {
            if (result_name && maybe_prev_instr) {
                /* We want to tee this up to eliminate the store instruction */
                if (maybe_prev_instr->opcode == IR_OP_STORE &&
                        aoStrEq(maybe_prev_instr->op1->name, result_name)) {
                    if (!mapHas(chain_map, result_name)) {
                        Set *new_chain = irValueSetNew();
                        setAdd(new_chain, maybe_prev_instr->result);
                        mapAdd(chain_map, result_name, new_chain);
                    }
                }
            }
            break;
        }

        case IR_OP_RET: {
            Set *new_chain = irValueSetNew();
            mapAdd(chain_map, result_name, new_chain);
            break;
        }

        case IR_OP_ALLOCA: {
            if (!mapHas(chain_map, result_name)) {
                Set *new_chain = irValueSetNew();
                mapAdd(chain_map, result_name, new_chain);
            }
            break;
        }

        /* As we are iterating backwards we can obliterate the load IF 
         * we have already seen the temporaries that are being used as the 
         * result */
        case IR_OP_LOAD: {
            if (mapHas(chain_map, result_name) && mapHas(chain_map, op1_name)) {
                Set *chain = mapGet(chain_map, result_name);
                setAdd(chain, instr->op1);
            }
            break;
        }

        case IR_OP_STORE: {
            if (mapHas(chain_map, result_name)) {
                Set *chain = mapGet(chain_map, result_name);
                setAdd(chain, instr->op1);
            }
            break;
        }

        default: {
            if (irInstrHasPtr(instr)) return;

            if (result_name && op1_name && op2_name) {
                /* This is going to be some maths or get element
                 * pointer type operation */
                if (!mapHas(chain_map, result_name)) {
                    Set *new_chain = irValueSetNew();
                    mapAdd(chain_map, result_name, new_chain);
                }
                if (!mapHas(chain_map, op1_name)) {
                    Set *new_chain = irValueSetNew();
                    mapAdd(chain_map, op1_name, new_chain);
                }
                if (!mapHas(chain_map, op2_name)) {
                    Set *new_chain = irValueSetNew();
                    mapAdd(chain_map, op2_name, new_chain);
                }
            } else if (result_name && op1_name) {
                if (!mapHas(chain_map, result_name)) {
                    Set *chain = irValueSetNew();
                    setAdd(chain, instr->op1);
                    mapAdd(chain_map, result_name, chain);
                } else {
                    Set *chain = mapGet(chain_map, result_name);
                    setAdd(chain, instr->op1);
                }
            } else if (result_name && !mapHas(chain_map, result_name)) {
                Set *chain = irValueSetNew();
                if (instr->op1) {
                    setAdd(chain, instr->op1);
                }
                mapAdd(chain_map, result_name, chain);
            }
            break;
        }
    }
}

/* Collapses what I'm calling chains of variable references */
static int irChainCollapse(IrFunction *func, Map *chain_map) {
    IrInstrIter it;
    irInstrIterNextInit(&it, func);
    int changed = 0;

    while (irInstrIterNext(&it)) {
        IrInstr *instr = it.instr;
        switch (instr->opcode) {
            case IR_OP_LOAD: {
                AoStr *result_name = irValueGetName(instr->result);
                AoStr *op1_name = irValueGetName(instr->op1);
                if (mapHas(chain_map, result_name)) {
                    Set *chain = mapGet(chain_map, result_name);
                    if (chain->size) {
                        irChainReplaceAllUses(chain_map, func, instr->op1, instr->result);
                        instr->op1 = instr->result;
                        instr->opcode = IR_OP_NOP;
                        changed = 1;
                    }
                }
                break;
            }

            case IR_OP_STORE: {
                AoStr *result_name = irValueGetName(instr->result);
                AoStr *op1_name = irValueGetName(instr->op1);
                if (mapHas(chain_map, result_name)) {
                    Set *chain = mapGet(chain_map, result_name);
                    if (chain->size) {
                        irChainReplaceAllUses(chain_map, func, instr->op1, instr->result);
                        if (aoStrEq(result_name, op1_name)) {
                            instr->opcode = IR_OP_NOP;
                            changed = 1;
                        }
                    }
                }
                break;
            }

            case IR_OP_CALL: {
                AoStr *result_name = irValueGetName(instr->result);
                if (result_name && mapHas(chain_map, result_name)) {
                    Set *chain = mapGet(chain_map, result_name);
                    if (chain->size) {
                        IrValue *replacement = setGetAt(chain, 0);
                        irChainReplaceAllUses(chain_map, func, instr->result, replacement);
                        instr->result = replacement;
                        changed = 1;
                    }
                }
                break;
            }

            case IR_OP_RET: {
                AoStr *result_name = irValueGetName(instr->result);
                if (mapHas(chain_map, result_name)) {
                    Set *chain = mapGet(chain_map, result_name);
                    if (chain->size) {
                        instr->result = setGetAt(chain, 0);
                        changed = 1;
                    }
                }
                break;
            }

            default:
                break;
        }
    }

    return changed;
}

/* This is correct as we are nuking redundant loads, stores and temporary 
 * variables. Which means when passing things off to the register allocator 
 * there are less things to allocate and the register allocator decides what 
 * requires a stack slot and what requires a register. Thus something like 
 * `I64 x = <some_value>` has not guarantee of being on the stack! 
 *
 * That property also this makes the HolyC feature of declaring that a 
 * variable has to be in a specific register simpler as we'd mark it as unusable 
 * in the allocator. This in turn makes it easier to pass values between HolyC 
 * and inline assembly */
int irReduceLoadStoreChain(IrFunction *func) {
    Map *chain_map = irValueChainMap();

    for (int i = 0; i < func->params->size; ++i) {
        IrValue *val = func->params->entries[i];
        mapAdd(chain_map, val->name, irValueSetNew());
    }

    int iters = 0;
    int changed = 1;
    IrInstrIter it;
    while (changed) {
        changed = 0;

        irInstrIterPrevInit(&it, func);
        /* Stage 1 is find a chain of usage, this will nuke almost all loads
         * as we are going to assign a load to a stack slot in the codegen 
         * so perhaps this function actually de-optimises? The code starts to 
         * look more clearer in any case */
        for (int i = 0 ; i < 2; ++i) {
            while (irInstrIterPrev(&it)) {
                /* The instruction behind the one we are looking at, as we are
                 * iterating in reverse this is, in the view of the linked list
                 * the `next`, node */
                IrInstr *maybe_prev_instr = listNext(it.instr_it->next);
                irBuildChain(chain_map, it.instr, maybe_prev_instr);
            }
        }

        /* Eliminate as many instructions as possible */
        changed = irChainCollapse(func, chain_map);

        mapPrint(chain_map);

        if (changed) {
            listForEach(func->blocks) {
                IrBlock *block = getValue(IrBlock *, it);
                if (block->removed) continue;
                irBlockRemoveNops(block);
            }
            iters++;
            mapClear(chain_map);
        }
    }

    mapRelease(chain_map);
    return iters;
}

/* Split this out for simplicity though it could quite possibly be merged 
 * with `irReduceLoadStoreChain(...)` */
int irReduceGepStoreChains(IrFunction *func) {
    int changed = 1;
    int iters = 0;
    Map *chain_map = irGepChainMap();

    while (changed) {
        changed = 0;
        IrInstrIter it;
        irInstrIterPrevInit(&it, func);
        while (irInstrIterPrev(&it)) {
            IrInstr *instr = it.instr;
            if (instr->opcode == IR_OP_GEP) {
                if (!mapHas(chain_map, instr)) {
                    Set *chain = irValueSetNew();
                    setAdd(chain, instr->result);
                    mapAdd(chain_map, instr, chain);
                } else {
                    Set *chain = mapGet(chain_map, instr);
                    setAdd(chain, instr->result);
                }
            }
        }

        MapIter map_iter;
        for (mapIterInit(chain_map, &map_iter); mapIterNext(&map_iter); ) {
            IrInstr *instr = map_iter.node->key;
            Set *values = map_iter.node->value;
            SetIter set_iter;
            for (setIterInit(&set_iter, values); setIterNext(&set_iter); ) {
                IrValue *value = set_iter.value;
                if (value == instr->result) continue;
                irReplaceAllUses(func, value, instr->result);
                changed = 1;
            }
        }

        mapPrint(chain_map);
        mapClear(chain_map);
    }
    mapRelease(chain_map);

    return iters;
}

void irOptimiseFunction(IrFunction *func) {
    int changed = 1;
    int iters = 0;
    debug("PRE OPTIMISAION;\n");
    printf("%s\n", irFunctionToString(func)->data);
    debug("================\n");

    while (changed && iters < 2) {
        debug("ITER; %d\n", iters);
        Map *liveness_map = irLivenessAnalysis(func);
        mapPrint(liveness_map);
        liveness_map = irEliminateDeadCode(func, liveness_map);
         irPerformCopyPropagation(func);
        if (irReduceLoadStoreChain(func) != 0) {
            changed = 1;
        }
        
        if (irReduceGepStoreChains(func) != 0) {
            changed = 1;
        }

        printf("%s\n", irFunctionToString(func)->data);
        debug("================\n");
        iters++;
    }
}
