#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "containers.h"
#include "ir-eval.h"
#include "ir-optimise.h"
#include "ir-regalloc.h"
#include "ir-types.h"
#include "list.h"

static void irRemoveAllNops(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = it->value;
        List *l = bb->instructions;
        List *it = l->next;

        while (it != l) {
            List *next = it->next;
            List *prev = it->prev;
            IrInstr *I = listValue(IrInstr *, it);
            if (I->op == IR_NOP) {
                prev->next = next;
                next->prev = prev;
                free(it);
            }
            it = next;
        }
    }
}

/* Swap `removal_id` for `target` in every branch / phi / jump within `bb` */
static void irRemapInstrBlockIds(IrBlock *bb, u64 removal_id, IrBlock *target) {
    listForEach(bb->instructions) {
        IrInstr *I = it->value;
        switch (I->op) {
            case IR_PHI: {
                Vec *pairs = I->extra.phi_pairs;
                for (u64 i = 0; i < pairs->size; ++i) {
                    IrPair *p = pairs->entries[i];
                    if (p->ir_block->id == removal_id) {
                        p->ir_block = target;
                    }
                }
                break;
            }
            case IR_BR: {
                IrBlockPair *blk_pair = &I->extra.blocks;
                if (blk_pair->target_block->id == removal_id) {
                    blk_pair->target_block = target;
                }
                if (blk_pair->fallthrough_block->id == removal_id) {
                    blk_pair->fallthrough_block = target;
                }
                break;
            }
            case IR_CMP_BR: {
                IrCmpBr *cb = &I->extra.cmp_br;
                if (cb->target_block->id == removal_id) {
                    cb->target_block = target;
                }
                if (cb->fallthrough_block->id == removal_id) {
                    cb->fallthrough_block = target;
                }
                break;
            }
            case IR_JMP: {
                IrBlockPair *blk_pair = &I->extra.blocks;
                if (blk_pair->target_block->id == removal_id) {
                    blk_pair->target_block = target;
                }
                break;
            }
            default:
                break;
        }
    }
}

static void irRemoveRedundantBlocks(IrFunction *fn) {
    Vec *removal = vecNew(&vec_ir_block_type);
    Map *from_to = mapNew(16, &map_u32_to_ir_block_type);

    listForEach(fn->blocks) {
        IrBlock *bb = it->value;
        IrInstr *jmp = irBlockLastInstr(bb);

        if (jmp && jmp->op == IR_JMP) {
            if (irBlockIsRedundantJump(fn, bb)) {
                if (jmp->op == IR_JMP) {
                    IrBlock *R = jmp->extra.blocks.target_block;
                    IrInstr *first = irBlockFirstInstr(R);
                    /* We can't remove phis */
                    if (first && first->op == IR_PHI) {
                        continue;
                    }

                    mapAdd(from_to, (void *)(u64)R->id, bb);
                    vecPush(removal, R);
                    /* Remove redundant jump */
                    listPop(bb->instructions);
                    /* Merge instructions.
                     * This is destructive and leads R->instructions pointing
                     * to garbage as it gets freed */
                    listMergeAppend(bb->instructions, R->instructions);
                    /* Make the instructions NULL, otherwise if we see this
                     * block again we could touch invalid memory */
                    R->instructions = NULL;
                }
            }
        }
    }

    for (u64 i = 0; i < removal->size; ++i) {
        IrBlock *R = removal->entries[i];
        IrBlock *target = mapGet(from_to, (void *)(u64)R->id);
        listRemoveValue(fn->blocks,removal->entries[i]);

        listForEach(fn->blocks) {
            IrBlock *bb = it->value;
            Map *successors = irBlockGetSuccessors(fn, bb);
            Map *predecessors = irBlockGetPredecessors(fn, bb);

            /* Remap sucessors */
            if (successors && mapHasInt(successors, R->id)) {
                irRemapInstrBlockIds(bb, R->id, target);
                mapRemoveInt(successors, R->id);
                mapAdd(successors, (void *)(u64)target->id, target);
            }

            /* Remap predecessors */
            if (predecessors && mapHasInt(predecessors, R->id)) {
                irRemapInstrBlockIds(bb, R->id, target);
                mapRemoveInt(predecessors, R->id);
                mapAdd(predecessors, (void *)(u64)target->id, target);
            }
        }
    }

    vecRelease(removal);
    mapRelease(from_to);
}

/* Fold "pass-through" blocks: a block whose only instruction is a
 * single IR_JMP X. Every predecessor's branch-target is rewired to X
 * directly and the empty block is dropped. Skipped when X starts
 * with a phi (its per-predecessor entries would have to be cloned). */
static void irFoldPassThroughBlocks(IrFunction *fn) {
    Vec *removal = vecNew(&vec_ir_block_type);

    listForEach(fn->blocks) {
        IrBlock *bb = it->value;
        Map *bb_preds = irBlockGetPredecessors(fn, bb);
        if (!bb_preds || bb_preds->size == 0) continue;
        IrInstr *jmp = irBlockLastInstr(bb);
        if (!jmp || jmp->op != IR_JMP) continue;
        if (!listIsOne(bb->instructions)) continue;

        IrBlock *target = jmp->extra.blocks.target_block;
        if (!target || target->id == bb->id) continue;
        IrInstr *first = irBlockFirstInstr(target);
        if (first && first->op == IR_PHI) continue;

        Map *preds = irBlockGetPredecessors(fn, bb);
        Map *target_preds = irBlockGetPredecessors(fn, target);
        if (!preds || !target_preds) continue;

        MapIter it_p;
        mapIterInit(preds, &it_p);
        while (mapIterNext(&it_p)) {
            IrBlock *P = (IrBlock *)it_p.node->value;
            if (!P || P->id == bb->id) continue;
            irRemapInstrBlockIds(P, bb->id, target);
            Map *psucc = irBlockGetSuccessors(fn, P);
            if (psucc && mapHasInt(psucc, bb->id)) {
                mapRemoveInt(psucc, bb->id);
                if (!mapHasInt(psucc, target->id))
                    mapAdd(psucc, (void *)(u64)target->id, target);
            }
            if (!mapHasInt(target_preds, P->id))
                mapAdd(target_preds, (void *)(u64)P->id, P);
        }

        mapRemoveInt(target_preds, bb->id);
        vecPush(removal, bb);
    }

    for (u64 i = 0; i < removal->size; ++i) {
        listRemoveValue(fn->blocks, removal->entries[i]);
    }
    vecRelease(removal);
}

/* Fold `lea %t, global G; ...; <load|store>_deref %t, ...` into the
 * deref pointing straight at the global. Backend addressing modes can
 * usually reach a global in one instruction, so dropping the LEA both
 * shrinks codegen and frees the address register. */
static void irFoldGlobalDeref(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        Map *tmp_to_global = mapNew(16, &map_uint_to_uint_type);

        listForEach(bb->instructions) {
            IrInstr *cur = (IrInstr *)it->value;
            if (cur->op != IR_LEA) continue;
            if (!cur->r1 || cur->r1->kind != IR_VAL_GLOBAL) continue;
            if (!irIsTmp(cur->dst)) continue;
            mapAdd(tmp_to_global,
                   (void *)(u64)irDstVarId(cur), cur->r1);
        }

        if (tmp_to_global->size == 0) {
            mapRelease(tmp_to_global);
            continue;
        }

        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_LOAD_DEREF && irIsTmp(I->r1) &&
                mapHasInt(tmp_to_global, irR1VarId(I)))
            {
                I->r1 = (IrValue *)mapGetInt(tmp_to_global, irR1VarId(I));
            } else if (I->op == IR_STORE_DEREF && irIsTmp(I->dst) &&
                       mapHasInt(tmp_to_global, irDstVarId(I)))
            {
                I->dst = (IrValue *)mapGetInt(tmp_to_global, irDstVarId(I));
            }
        }

        mapRelease(tmp_to_global);
    }
}

/* IR builder routes function returns through an alloca'd slot:
 *   alloca %slot, ...; store %slot, %value; load %tmp, %slot; ret %tmp
 * For straight-line returns this round-trip is dead work. Detect the
 * exact 3-instr pattern within a block and rewrite to `ret %value`,
 * NOP'ing the store + load. If the alloca it was using has no other
 * references, NOP that too so layout doesn't reserve a slot. */
static int irSlotHasUses(IrFunction *fn, u32 var_id) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP || I->op == IR_ALLOCA) continue;
            IrValue *vs[3] = { I->dst, I->r1, I->r2 };
            for (int k = 0; k < 3; ++k) {
                if (irIsTmp(vs[k]) && irVarId(vs[k]) == var_id)
                    return 1;
            }
        }
    }
    return 0;
}

static void irForwardReturnSlot(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        for (List *node = bb->instructions->next;
             node != bb->instructions;
             node = node->next)
        {
            IrInstr *st = (IrInstr *)node->value;
            if (st->op != IR_STORE) continue;
            if (!st->dst || !st->r1) continue;

            List *n2 = node->next;
            while (n2 != bb->instructions &&
                   ((IrInstr *)n2->value)->op == IR_NOP) n2 = n2->next;
            if (n2 == bb->instructions) continue;
            IrInstr *ld = (IrInstr *)n2->value;
            if (ld->op != IR_LOAD) continue;
            if (!ld->r1 || !ld->dst) continue;
            if (ld->r1->kind != st->dst->kind ||
                irR1VarId(ld) != irDstVarId(st)) continue;

            List *n3 = n2->next;
            while (n3 != bb->instructions &&
                   ((IrInstr *)n3->value)->op == IR_NOP) n3 = n3->next;
            if (n3 == bb->instructions) continue;
            IrInstr *rt = (IrInstr *)n3->value;
            if (rt->op != IR_RET) continue;
            if (!irIsTmp(rt->dst)) continue;
            if (irDstVarId(rt) != irDstVarId(ld)) continue;

            u32 slot_id = irDstVarId(st);
            rt->dst = st->r1;
            st->op = IR_NOP;
            st->dst = st->r1 = st->r2 = NULL;
            ld->op = IR_NOP;
            ld->dst = ld->r1 = ld->r2 = NULL;

            /* If the alloca slot now has no users, drop it too. */
            if (!irSlotHasUses(fn, slot_id)) {
                listForEach(fn->blocks) {
                    IrBlock *bb2 = (IrBlock *)it->value;
                    listForEach(bb2->instructions) {
                        IrInstr *I = (IrInstr *)it->value;
                        if (I->op == IR_ALLOCA && irIsTmp(I->dst) &&
                            irDstVarId(I) == slot_id) {
                            I->op = IR_NOP;
                            I->dst = I->r1 = I->r2 = NULL;
                        }
                    }
                }
            }
        }
    }
}

/* Store -> read forwarding within a basic block.
 *
 *   store %slot, %value
 *   ... (no kill of %slot) ...
 *   <any op>, ..., %slot, ...
 *
 * After the store, every read of %slot has the same value as %value.
 * Rewrite the operand reference from %slot to %value so consumers
 * read %value's location (often an ABI reg) instead of bouncing
 * through the slot. Kills: IR_CALL (clobbers caller-saved regs),
 * IR_STORE_DEREF / IR_RMW_DEREF (may alias), IR_STORE to same slot. */
static int irValMatchesSlot(IrValue *v, IrValue *slot) {
    return v && slot &&
           v->kind == slot->kind &&
           irVarId(v) == irVarId(slot);
}

static void irRewriteOperands(IrInstr *I, IrValue *slot, IrValue *source) {
    /* IR_LEA / IR_GEP / IR_LOAD / IR_STORE_DEREF (address operand) all
     * need their operand to have a real frame loff. They emit
     * `leaq loff(%rbp), ...` and friends. A `loc=REG`-only source
     * (e.g. a param's arrive value) can't satisfy that. */
    if (I->op == IR_LEA || I->op == IR_GEP || I->op == IR_LOAD)
        return;
    if (irValMatchesSlot(I->r1, slot) && I->r1 != source) I->r1 = source;
    if (irValMatchesSlot(I->r2, slot) && I->r2 != source) I->r2 = source;
    /* IR_BR/IR_RET carry their value operand in dst. STORE_DEREF's
     * dst is the address (needs loff, don't substitute). */
    if ((I->op == IR_BR || I->op == IR_RET) &&
        irValMatchesSlot(I->dst, slot) && I->dst != source)
    {
        I->dst = source;
    }
    /* CALL args: substituting here is unsafe. The call's per-arg
     * loading sequence clobbers ABI arg regs in order, so a later
     * arg whose forwarded source.loc names an earlier arg's target
     * reg reads garbage. */
    if (I->op == IR_PHI && I->extra.phi_pairs) {
        for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
            IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
            if (p && irValMatchesSlot(p->ir_value, slot) &&
                p->ir_value != source)
            {
                p->ir_value = source;
            }
        }
    }
}

/* True if `v` is pinned to a register the backend declared as
 * per-instruction scratch. After a non trivial op the forwarded
 * value sitting there is gone, so the forwarding map drops it. */
static int irLocIsScratchClobbered(IrValue *v) {
    if (!v || v->loc.kind != IR_LOC_REG || !v->loc.as.reg) return 0;
    IrRegPool *pool = irRegPoolGet();
    if (!pool || !pool->scratch_regs) return 0;
    AoStr *vr = v->loc.as.reg;
    for (u64 i = 0; i < pool->scratch_regs->size; ++i) {
        /* String-compare: scratch_regs and arg_regs are independent
         * AoStr* vecs, so pointer identity misses overlaps like x0
         * (arg #1 AND scratch on AArch64). */
        if (aoStrEq(vr, vecGet(AoStr *, pool->scratch_regs, i))) return 1;
    }
    return 0;
}

static void irForwardStoreToReads(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;

        /* Parallel arrays: slots[i] is a slot IrValue, sources[i] is
         * the value currently held in it. */
        IrValue **slots = NULL;
        IrValue **sources = NULL;
        u64 n = 0, cap = 0;

        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;

            for (u64 i = 0; i < n; ++i) {
                irRewriteOperands(I, slots[i], sources[i]);
            }

            /* Kills clear the entire map. */
            if (I->op == IR_CALL || I->op == IR_STORE_DEREF ||
                I->op == IR_RMW_DEREF)
            {
                n = 0;
                continue;
            }

            /* Scratch-clobber: drop any source whose loc names a
             * backend-declared scratch reg, since this op overwrote it.
             * IR_STORE counts: a const r1 is materialised via the
             * scratch reg on AArch64 (mov x0, #N; stur x0, ...), so a
             * param pinned to x0 is gone after the store. */
            if (I->op != IR_NOP && I->op != IR_JMP && I->op != IR_PHI &&
                I->op != IR_LABEL && I->op != IR_ALLOCA)
            {
                u64 j = 0;
                while (j < n) {
                    if (irLocIsScratchClobbered(sources[j])) {
                        slots[j] = slots[n - 1];
                        sources[j] = sources[n - 1];
                        n--;
                    } else {
                        j++;
                    }
                }
            }

            /* Update map on STORE: dst-slot now holds r1's value.
             * Only forward when the value is 8 bytes wide. A
             * sub-word value (i32 argc, i8 char param) has unspecified
             * upper bits in its ABI arg reg, and a movq forwarding
             * skips the sign/zero extension a slot read would do. */
            if (I->op == IR_STORE && I->dst && I->r1 &&
                I->r1->as.var.size == 8)
            {
                for (u64 i = 0; i < n; ++i) {
                    if (irValMatchesSlot(I->dst, slots[i])) {
                        sources[i] = I->r1;
                        goto did_update;
                    }
                }
                if (n == cap) {
                    cap = cap ? cap * 2 : 8;
                    slots = realloc(slots, cap * sizeof(IrValue *));
                    sources = realloc(sources, cap * sizeof(IrValue *));
                }
                slots[n] = I->dst;
                sources[n] = I->r1;
                n++;
                did_update:;
            } else if (I->op == IR_STORE && I->dst && I->r1) {
                /* Sub-word store: drop any tracking for this slot
                 * (it's being overwritten with a different value). */
                for (u64 i = 0; i < n; ++i) {
                    if (irValMatchesSlot(I->dst, slots[i])) {
                        slots[i] = slots[n - 1];
                        sources[i] = sources[n - 1];
                        n--;
                        break;
                    }
                }
            }
        }

        free(slots);
        free(sources);
    }
}

/* Dead store elimination: after forwarding, the original IR_STOREs
 * into a slot may have no remaining readers. Conservative: refuse
 * when the slot's address is taken via IR_LEA / IR_GEP (could be
 * aliased through pointer arithmetic). */
static int irSlotHasAddressTaken(IrFunction *fn, u32 var_id) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if ((I->op == IR_LEA || I->op == IR_GEP) &&
                I->r1 && I->r1->kind != IR_VAL_CONST_INT &&
                irR1VarId(I) == var_id) return 1;
        }
    }
    return 0;
}

static int irSlotReadCount(IrFunction *fn, u32 var_id) {
    int n = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            IrValue *vs[3] = { I->r1, I->r2, NULL };
            /* IR_BR/IR_RET read dst; IR_STORE_DEREF reads dst (as
             * address). IR_STORE's dst is a write target, not a read. */
            if (I->op == IR_BR || I->op == IR_RET ||
                I->op == IR_STORE_DEREF) vs[2] = I->dst;
            for (int k = 0; k < 3; ++k) {
                if (vs[k] && irVarId(vs[k]) == var_id) n++;
            }
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 i = 0; i < args->size; ++i) {
                    IrValue *v = (IrValue *)args->entries[i];
                    if (v && irVarId(v) == var_id) n++;
                }
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                    IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                    if (p && p->ir_value &&
                        irVarId(p->ir_value) == var_id) n++;
                }
            }
        }
    }
    return n;
}

/* Function-wide TMP use-count helpers, shared across the fusion
 * passes below. Caller releases the maps. */
static void irBumpUse(Map *uses, IrValue *v) {
    if (!irIsTmp(v)) return;
    u32 id = irVarId(v);
    int n = mapHasInt(uses, id) ? (int)(u64)mapGetInt(uses, id) : 0;
    mapAdd(uses, (void *)(u64)id, (void *)(u64)(n + 1));
}

static Map *irBuildTmpUseCounts(IrFunction *fn) {
    Map *uses = mapNew(64, &map_uint_to_uint_type);
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            irBumpUse(uses, I->r1);
            irBumpUse(uses, I->r2);
            irBumpUse(uses, I->idx);
            if (I->op == IR_BR || I->op == IR_RET ||
                I->op == IR_STORE || I->op == IR_STORE_DEREF ||
                I->op == IR_RMW_DEREF)
                irBumpUse(uses, I->dst);
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 i = 0; i < args->size; ++i)
                    irBumpUse(uses, (IrValue *)args->entries[i]);
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                Vec *pairs = I->extra.phi_pairs;
                for (u64 i = 0; i < pairs->size; ++i) {
                    IrPair *p = vecGet(IrPair *, pairs, i);
                    if (p) irBumpUse(uses, p->ir_value);
                }
            }
        }
    }
    return uses;
}

static int irTmpUseCount(Map *uses, IrValue *v) {
    if (!irIsTmp(v)) return -1;
    u32 id = irVarId(v);
    return mapHasInt(uses, id) ? (int)(u64)mapGetInt(uses, id) : 0;
}

/* Wipe an instruction to a NOP, clearing every operand field. */
static void irMakeNop(IrInstr *I) {
    I->op = IR_NOP;
    I->dst = I->r1 = I->r2 = I->idx = NULL;
    I->disp = 0;
    I->scale = 0;
}

/* Walk forward in `bb` from the node after `cur` looking for the
 * single use of `dst` as a memory op's address operand. Returns the
 * consumer or NULL if any other reference appears first. */
static IrInstr *irFindMemConsumer(List *cur, List *sentinel, IrValue *dst) {
    for (List *p = cur->next; p != sentinel; p = p->next) {
        IrInstr *J = (IrInstr *)p->value;
        if (J->op == IR_NOP) continue;
        int hits = (J->op == IR_LOAD_DEREF && J->r1 == dst) ||
                   ((J->op == IR_STORE_DEREF || J->op == IR_RMW_DEREF) &&
                    J->dst == dst);
        if (hits) return J;
        if (J->r1 == dst || J->r2 == dst ||
            J->dst == dst || J->idx == dst) return NULL;
    }
    return NULL;
}

/* True if any op strictly between `from` and `to` (exclusive on both
 * ends) is a scratch-clobbering op. Used by addressing-mode fusion to
 * refuse folds that would resurrect a reg-pinned value (typically a
 * PARAM) past a clobber. */
static int irRangeClobbersScratch(List *from, List *to) {
    for (List *p = from->next; p != to; p = p->next) {
        IrInstr *J = (IrInstr *)p->value;
        switch (J->op) {
        case IR_NOP: case IR_JMP: case IR_PHI:
        case IR_LABEL: case IR_ALLOCA:
            continue;
        default:
            return 1;
        }
    }
    return 0;
}

/* TMP id -> defining instruction. Skips ops that don't "produce"
 * their dst (STORE/STORE_DEREF/RMW_DEREF write to memory; BR/RET
 * pass a value via dst). */
static Map *irBuildTmpDefs(IrFunction *fn) {
    Map *defs = mapNew(64, &map_uint_to_uint_type);
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (!irIsTmp(I->dst)) continue;
            switch (I->op) {
                case IR_STORE:
                case IR_STORE_DEREF:
                case IR_RMW_DEREF:
                case IR_BR: case IR_RET:
                    continue;
                default: break;
            }
            mapAdd(defs, (void *)(u64)irDstVarId(I), I);
        }
    }
    return defs;
}

/* If `v` is a TMP defined by SHL/IMUL with a constant scale fitting
 * the x86 SIB encoding (1/2/4/8), return the def and set *scale. */
static IrInstr *irFindScalingProducer(Map *defs, IrValue *v, u8 *scale) {
    if (!irIsTmp(v)) return NULL;
    IrInstr *def = (IrInstr *)mapGetInt(defs, irVarId(v));
    if (!def || !irIsConstInt(def->r2)) return NULL;
    s64 k = def->r2->as._i64;
    if (def->op == IR_SHL) {
        if (k < 0 || k > 3) return NULL;
        *scale = (u8)(1 << k); return def;
    }
    if (def->op == IR_IMUL) {
        if (k != 1 && k != 2 && k != 4 && k != 8) return NULL;
        *scale = (u8)k; return def;
    }
    return NULL;
}

/* Fold IADD/ISUB into the addressing mode of its single mem-op
 * consumer. Two folds, tried in order per IADD:
 *   - const disp: `iadd t, ptr, k_imm` (or `isub ptr, k`) -> set
 *     consumer's `disp = k`, retarget address to ptr.
 *   - SIB scale: `iadd t, base, scaled` where scaled comes from a
 *     single-use SHL/IMUL by 1/2/4/8 -> set consumer's `idx, scale`,
 *     retarget address to base.
 * The two paths are mutually exclusive on any single IADD (const
 * fold needs one const operand, scale fold needs two TMP operands).
 * Each consumer accepts at most one disp fold and one scale fold. */
static int irFuseIaddIntoMemAddressing(IrFunction *fn) {
    Map *uses = irBuildTmpUseCounts(fn);
    Map *defs = irBuildTmpDefs(fn);
    int changed = 0;

    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        List *l = bb->instructions;
        for (List *cur = l->next; cur != l; cur = cur->next) {
            IrInstr *I = (IrInstr *)cur->value;
            if (I->op != IR_IADD && I->op != IR_ISUB) continue;
            if (!irIsTmp(I->dst)) continue;
            if (!I->r1 || !I->r2) continue;
            if (I->dst->as.var.size != 8) continue;
            if (irTmpUseCount(uses, I->dst) != 1) continue;

            IrInstr *use = irFindMemConsumer(cur, l, I->dst);
            if (!use) continue;

            /* Find the consumer's list node for the clobber check. */
            List *use_node = NULL;
            for (List *p = cur->next; p != l; p = p->next) {
                if ((IrInstr *)p->value == use) { use_node = p; break; }
            }

            /* Disp fold. IADD commutative; ISUB only `ptr - k_imm`. */
            if (use->disp == 0) {
                IrValue *ptr = NULL;
                s64 k = 0;
                if (I->op == IR_IADD) {
                    if (irIsConstInt(I->r2)) { ptr = I->r1; k = I->r2->as._i64; }
                    else if (irIsConstInt(I->r1)) { ptr = I->r2; k = I->r1->as._i64; }
                } else if (irIsConstInt(I->r2)) {
                    ptr = I->r1; k = -I->r2->as._i64;
                }
                if (ptr && k >= INT32_MIN && k <= INT32_MAX) {
                    /* Refuse the fold if ptr is reg-pinned and any
                     * intervening op clobbers scratch, the codegen
                     * would read a stale register at the consumer. */
                    if (irLocIsScratchClobbered(ptr) && use_node &&
                        irRangeClobbersScratch(cur, use_node))
                    {
                        /* fall through to scale-fold attempt */
                    } else {
                        use->disp = (s32)k;
                        if (use->op == IR_LOAD_DEREF) use->r1 = ptr;
                        else                          use->dst = ptr;
                        irMakeNop(I);
                        changed = 1;
                        continue;
                    }
                }
            }

            /* SIB scale fold. IADD only (ISUB can't synthesise a
             * negative-stride SIB on x86). */
            if (I->op == IR_IADD && use->scale == 0) {
                u8 scale = 0;
                IrInstr *scaler = NULL;
                IrValue *base = NULL;
                if ((scaler = irFindScalingProducer(defs, I->r2, &scale)) &&
                    irTmpUseCount(uses, I->r2) == 1) base = I->r1;
                else if ((scaler = irFindScalingProducer(defs, I->r1, &scale)) &&
                         irTmpUseCount(uses, I->r1) == 1) base = I->r2;
                else scaler = NULL;
                if (scaler && irLocIsScratchClobbered(base) && use_node &&
                    irRangeClobbersScratch(cur, use_node))
                {
                    scaler = NULL;
                }
                if (scaler) {
                    use->idx = scaler->r1;
                    use->scale = scale;
                    if (use->op == IR_LOAD_DEREF) use->r1 = base;
                    else                          use->dst = base;
                    irMakeNop(scaler);
                    irMakeNop(I);
                    changed = 1;
                }
            }
        }
    }

    mapRelease(defs);
    mapRelease(uses);
    return changed;
}

static int irRmwOpCommutes(IrOp op) {
    return op == IR_IADD || op == IR_AND || op == IR_OR || op == IR_XOR;
}

static int irRmwOpSupported(IrOp op) {
    return op == IR_IADD || op == IR_ISUB ||
           op == IR_AND  || op == IR_OR  || op == IR_XOR;
}

static int irAddressingMatches(IrInstr *load, IrInstr *store) {
    return load->r1 == store->dst && load->disp == store->disp &&
           load->scale == store->scale && load->idx == store->idx;
}

static List *irPrevNonNop(List *node, List *sentinel) {
    for (List *p = node->prev; p != sentinel; p = p->prev) {
        if (((IrInstr *)p->value)->op != IR_NOP) return p;
    }
    return NULL;
}

/* Rewrite `load + binop + store_deref` triples on the same address
 * (`*p OP= val`) to IR_RMW_DEREF. The three ops must be adjacent
 * (skipping only NOPs). Anything else could touch the memory. */
static int irFuseLoadOpStore(IrFunction *fn) {
    Map *uses = irBuildTmpUseCounts(fn);
    Map *defs = irBuildTmpDefs(fn);
    int changed = 0;

    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *S = (IrInstr *)it->value;
            if (S->op != IR_STORE_DEREF) continue;
            if (!irIsTmp(S->r1)) continue;
            if (irTmpUseCount(uses, S->r1) != 1) continue;
            IrInstr *B = (IrInstr *)mapGetInt(defs, irR1VarId(S));
            if (!B || !irRmwOpSupported(B->op)) continue;
            if (!B->r1 || !B->r2) continue;

            /* Find which side is the load. ISUB needs load on r1
             * (`*p -= val`); commutative ops accept either side. */
            IrValue *t_load = NULL, *val = NULL;
            if (irIsTmp(B->r1) && irTmpUseCount(uses, B->r1) == 1) {
                IrInstr *L = (IrInstr *)mapGetInt(defs, irR1VarId(B));
                if (L && L->op == IR_LOAD_DEREF && irAddressingMatches(L, S)) {
                    t_load = B->r1; val = B->r2;
                }
            }
            if (!t_load && irRmwOpCommutes(B->op) &&
                irIsTmp(B->r2) && irTmpUseCount(uses, B->r2) == 1)
            {
                IrInstr *L = (IrInstr *)mapGetInt(defs, irR2VarId(B));
                if (L && L->op == IR_LOAD_DEREF && irAddressingMatches(L, S)) {
                    t_load = B->r2; val = B->r1;
                }
            }
            if (!t_load) continue;

            IrInstr *L = (IrInstr *)mapGetInt(defs, irVarId(t_load));
            List *l = bb->instructions;
            List *node_b = irPrevNonNop(it, l);
            if (!node_b || (IrInstr *)node_b->value != B) continue;
            List *node_l = irPrevNonNop(node_b, l);
            if (!node_l || (IrInstr *)node_l->value != L) continue;

            S->op = IR_RMW_DEREF;
            S->extra.rmw_op = B->op;
            S->r1 = val;
            irMakeNop(L);
            irMakeNop(B);
            changed = 1;
        }
    }

    mapRelease(defs);
    mapRelease(uses);
    return changed;
}

static void irDeadStoreEliminate(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_STORE) continue;
            if (!I->dst) continue;
            /* Only target LOCAL/PARAM slots. TMP dsts can be GEP'd
             * field addresses whose base struct is still referenced
             * elsewhere. Killing the store would skip the field write. */
            if (I->dst->kind != IR_VAL_LOCAL &&
                I->dst->kind != IR_VAL_PARAM) continue;
            u32 vid = irDstVarId(I);
            if (irSlotHasAddressTaken(fn, vid)) continue;
            if (irSlotReadCount(fn, vid) > 0) continue;
            I->op = IR_NOP;
            I->dst = I->r1 = I->r2 = NULL;
        }
    }
}

void irBasicFunctionOptimisations(IrFunction *fn) {
    irRemoveAllNops(fn);
    irRemoveRedundantBlocks(fn);
    irFoldPassThroughBlocks(fn);
    irRemoveRedundantBlocks(fn);
    irFoldGlobalDeref(fn);
    irEvalConstantExpressions(fn);
    irForwardReturnSlot(fn);
    irForwardStoreToReads(fn);
    irDeadStoreEliminate(fn);
    /* Address-mode + RMW fusion to fixed point. RMW collapses a
     * two-use IADD (used by both load and store on the same addr)
     * into one use, exposing a new disp-fold opportunity. Loop
     * until neither pass changes anything. Each "changed" iteration
     * strictly reduces the active instruction count, so this
     * terminates in O(N) sweeps. */
    int changed;
    do {
        changed  = irFuseIaddIntoMemAddressing(fn);
        changed |= irFuseLoadOpStore(fn);
    } while (changed);
    irRemoveAllNops(fn);
}

/* Does op `I` leave its dst's value in the architecture-canonical
 * result register (rax/xmm0 on x86_64, x0/d0 on aarch64)? If yes
 * it's eligible to fuse with the next consumer, skipping the
 * spill-then-reload bounce through a stack slot. */
static int irOpDefsIntoResultReg(IrInstr *I) {
    if (!I->dst) return 0;
    int is_float = irIsFloat(I->dst->type);
    switch (I->op) {
        case IR_LOAD: case IR_LOAD_DEREF: case IR_CALL:
            return I->dst->type != IR_TYPE_VOID;
        case IR_LEA:
        case IR_IADD: case IR_ISUB: case IR_IMUL:
        case IR_AND:  case IR_OR:   case IR_XOR:
        case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
        case IR_SHL:  case IR_SHR:  case IR_SAR:
        case IR_INEG: case IR_NOT:
        case IR_ICMP: case IR_FCMP:
        case IR_TRUNC: case IR_ZEXT: case IR_SEXT:
        case IR_PTRTOINT: case IR_INTTOPTR:
        case IR_FPTOSI: case IR_FPTOUI:
        case IR_BITCAST:
            return !is_float;
        case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV:
        case IR_FNEG:
        case IR_SITOFP: case IR_UITOFP:
        case IR_FPTRUNC: case IR_FPEXT:
            return is_float;
        default:
            return 0;
    }
}

/* Does op `I` read `var_id` as its first source, the operand the
 * backend will load into the result register first? */
static int irOpReadsAsR1(IrInstr *I, u32 var_id) {
    IrValue *r1 = NULL;
    switch (I->op) {
        case IR_STORE:
        case IR_STORE_DEREF:
        case IR_LOAD_DEREF:
        case IR_IADD: case IR_ISUB: case IR_IMUL:
        case IR_AND:  case IR_OR:   case IR_XOR:
        case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
        case IR_SHL:  case IR_SHR:  case IR_SAR:
        case IR_INEG: case IR_NOT:
        case IR_ICMP: case IR_FCMP:
        case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV:
        case IR_FNEG:
        case IR_TRUNC: case IR_ZEXT: case IR_SEXT:
        case IR_FPTRUNC: case IR_FPEXT:
        case IR_FPTOSI: case IR_FPTOUI:
        case IR_SITOFP: case IR_UITOFP:
        case IR_PTRTOINT: case IR_INTTOPTR:
        case IR_BITCAST:
        case IR_CMP_BR:
            r1 = I->r1; break;
        case IR_BR: case IR_RET:
            r1 = I->dst; break;
        default:
            return 0;
    }
    return irIsTmp(r1) && irVarId(r1) == var_id;
}

/* Does op `I` read `var_id` anywhere as an operand? */
static int irOpReadsTmp(IrInstr *I, u32 var_id) {
    IrValue *vs[3] = { I->r1, I->r2, NULL };
    if (I->op == IR_BR || I->op == IR_RET ||
        I->op == IR_STORE_DEREF || I->op == IR_RMW_DEREF) vs[2] = I->dst;
    for (int k = 0; k < 3; ++k) {
        if (irIsTmp(vs[k]) && irVarId(vs[k]) == var_id) return 1;
    }
    if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
        Vec *args = I->r1->as.array.values;
        for (u64 i = 0; i < args->size; ++i) {
            IrValue *v = (IrValue *)args->entries[i];
            if (irIsTmp(v) && irVarId(v) == var_id) return 1;
        }
    }
    if (I->op == IR_PHI && I->extra.phi_pairs) {
        for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
            IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
            if (p && irIsTmp(p->ir_value) &&
                irVarId(p->ir_value) == var_id) return 1;
        }
    }
    return 0;
}

/* For each single-use TMP producer whose consumer is the immediate
 * next non-NOP instr, stamp `producer.dst.loc = REG/result_reg`.
 * That single state captures every fusion case the old flag system
 * handled: codegen's SpillDst sees dst.loc and skips the slot store,
 * the consumer's LoadToReg sees src.loc and skips the slot load.
 *
 * Also handles the CMP -> BR rewrite into a single IR_CMP_BR. */
void irOptPinResultReg(IrFunction *fn) {
    Map *uses = irBuildTmpUseCounts(fn);
    IrRegPool *pool = irRegPoolGet();

    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            List *node = it;
            IrInstr *cur = (IrInstr *)node->value;
            if (cur->op == IR_NOP) continue;
            if (!irIsTmp(cur->dst)) continue;
            if (!irOpDefsIntoResultReg(cur)) continue;
            u32 dst_id = irDstVarId(cur);
            if (irTmpUseCount(uses, cur->dst) != 1) continue;

            IrInstr *next = NULL;
            for (List *scan = node->next;
                 scan != bb->instructions;
                 scan = scan->next)
            {
                IrInstr *cand = (IrInstr *)scan->value;
                if (cand->op == IR_NOP) continue;
                next = cand; break;
            }
            if (!next) continue;

            /* CMP -> BR fusion: collapse to a single IR_CMP_BR. */
            if ((cur->op == IR_ICMP || cur->op == IR_FCMP) &&
                next->op == IR_BR &&
                irOpReadsTmp(next, dst_id))
            {
                IrCmpKind kind = cur->extra.cmp_kind;
                IrBlock *t = next->extra.blocks.target_block;
                IrBlock *f = next->extra.blocks.fallthrough_block;
                next->op = IR_CMP_BR;
                next->dst = NULL;
                next->r1 = cur->r1;
                next->r2 = cur->r2;
                next->extra.cmp_br.cmp_kind = kind;
                next->extra.cmp_br.target_block = t;
                next->extra.cmp_br.fallthrough_block = f;
                irMakeNop(cur);
                continue;
            }

            /* The consumer reads this tmp from the result reg.
             * Match r1-of-next, dst-of-next (STORE_DEREF address),
             * idx-of-next (SIB), or args[0]-of-next (CALL arg0).
             * Skip type-mismatched chains. */
            int matches = 0;
            int consumer_reads_float = 0;
            if (irOpReadsAsR1(next, dst_id)) {
                matches = 1;
                /* Most ops follow r1->type; a few are type-opaque. */
                switch (next->op) {
                    case IR_FPTOSI: case IR_FPTOUI:
                    case IR_FPTRUNC: case IR_FPEXT:
                        consumer_reads_float = 1; break;
                    case IR_SITOFP: case IR_UITOFP:
                        consumer_reads_float = 0; break;
                    case IR_BITCAST:
                        consumer_reads_float = next->dst &&
                                               !irIsFloat(next->dst->type);
                        break;
                    case IR_BR: case IR_RET:
                        consumer_reads_float = next->dst &&
                                               irIsFloat(next->dst->type);
                        break;
                    default:
                        consumer_reads_float = next->r1 &&
                                               irIsFloat(next->r1->type);
                        break;
                }
            } else if (next->op == IR_STORE_DEREF &&
                       irIsTmp(next->dst) &&
                       irDstVarId(next) == dst_id) {
                matches = 1;
                consumer_reads_float = 0;
            } else if ((next->op == IR_LOAD_DEREF ||
                        next->op == IR_STORE_DEREF) &&
                       next->scale && irIsTmp(next->idx) &&
                       irVarId(next->idx) == dst_id)
            {
                /* SIB idx-operand fusion. LOAD_DEREF emits
                 * `disp(%base,%rax,scale),%rax` (idx read before
                 * rax is overwritten with the loaded value).
                 * STORE_DEREF codegen picks a non-rax val_reg. */
                matches = 1;
                consumer_reads_float = 0;
            } else if (next->op == IR_CALL && next->r1 &&
                       next->r1->as.array.values &&
                       next->r1->as.array.values->size > 0)
            {
                IrValue *arg0 = (IrValue *)
                    next->r1->as.array.values->entries[0];
                if (irIsTmp(arg0) && irVarId(arg0) == dst_id) {
                    matches = 1;
                    consumer_reads_float = irIsFloat(arg0->type);
                }
            }
            if (!matches) continue;

            int producer_is_float = irIsFloat(cur->dst->type);
            if (producer_is_float != consumer_reads_float) continue;
            if (!pool) continue;
            cur->dst->loc.kind   = IR_LOC_REG;
            cur->dst->loc.as.reg = producer_is_float
                                   ? pool->float_return_reg
                                   : pool->int_return_reg;
        }
    }

    mapRelease(uses);
}

/* Dead-code elimination on pure value-producers, run to fixed point.
 * Side-effecting ops (IR_STORE, IR_CALL, IR_BR/JMP/RET, IR_PHI,
 * IR_ALLOCA, IR_LOAD, IR_LOAD_DEREF (may fault), divisions (may
 * trap), IR_ASM, IR_VA_*) are always kept.
 *
 * After DCE, any IR_CALL whose dst tmp ended up unused is pinned
 * to the result reg so the allocator skips its slot. */
void irOptDeadCodeElim(IrFunction *fn) {
    Map *uses = irBuildTmpUseCounts(fn);
    int changed = 1;
    while (changed) {
        changed = 0;
        listForEach(fn->blocks) {
            IrBlock *bb = (IrBlock *)it->value;
            listForEach(bb->instructions) {
                IrInstr *I = (IrInstr *)it->value;
                int safe;
                switch (I->op) {
                case IR_LEA:
                case IR_IADD: case IR_ISUB: case IR_IMUL:
                case IR_AND:  case IR_OR:   case IR_XOR:
                case IR_SHL:  case IR_SHR:  case IR_SAR:
                case IR_INEG: case IR_NOT:
                case IR_ICMP: case IR_FCMP:
                case IR_FADD: case IR_FSUB: case IR_FMUL:
                case IR_FDIV: case IR_FNEG:
                case IR_TRUNC: case IR_ZEXT: case IR_SEXT:
                case IR_FPTRUNC: case IR_FPEXT:
                case IR_FPTOSI: case IR_FPTOUI:
                case IR_SITOFP: case IR_UITOFP:
                case IR_PTRTOINT: case IR_INTTOPTR:
                case IR_BITCAST:
                    safe = 1; break;
                default:
                    safe = 0;
                }
                if (!safe) continue;
                if (!irIsTmp(I->dst)) continue;
                if (irTmpUseCount(uses, I->dst) != 0) continue;
                irMakeNop(I);
                changed = 1;
            }
        }
        if (changed) {
            mapRelease(uses);
            uses = irBuildTmpUseCounts(fn);
        }
    }

    /* Unused-call-result: IR_CALL stays (side-effecting) but pin its
     * dead dst to the result reg so the slot allocator skips it. */
    IrRegPool *pool = irRegPoolGet();
    if (pool) {
        listForEach(fn->blocks) {
            IrBlock *bb = (IrBlock *)it->value;
            listForEach(bb->instructions) {
                IrInstr *I = (IrInstr *)it->value;
                if (I->op != IR_CALL) continue;
                if (!irIsTmp(I->dst)) continue;
                if (I->dst->type == IR_TYPE_VOID) continue;
                if (irTmpUseCount(uses, I->dst) != 0) continue;
                I->dst->loc.kind   = IR_LOC_REG;
                I->dst->loc.as.reg = irIsFloat(I->dst->type)
                                     ? pool->float_return_reg
                                     : pool->int_return_reg;
            }
        }
    }

    mapRelease(uses);
}
