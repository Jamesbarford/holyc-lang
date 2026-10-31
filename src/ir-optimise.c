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
                if (bb == target) {
                    /* `target` absorbed R's instructions, so it now
                     * ends with R's terminator and leaves where R left.
                     * Pointing it at `target` instead - which is what
                     * every OTHER predecessor of R correctly wants -
                     * made the merged block its own successor, and a
                     * self-loop is not something the source contained.
                     * Latent until something read the edges; the
                     * liveness pass is the first thing that does. */
                    Map *r_succ = irBlockGetSuccessors(fn, R);
                    if (r_succ) {
                        MapIter sit;
                        mapIterInit(r_succ, &sit);
                        while (mapIterNext(&sit)) {
                            IrBlock *S = (IrBlock *)sit.node->value;
                            if (!S || S == R) continue;
                            mapAdd(successors, (void *)(u64)S->id, S);
                        }
                    }
                } else {
                    mapAdd(successors, (void *)(u64)target->id, target);
                }
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

/* The cross-block half of irForwardReturnSlot. Both arms of
 *
 *     if (a < 0) return 0;
 *     return a;
 *
 * store into the alloca'd return slot and the exit block loads it
 * back - a frame slot and a round-trip through memory for a value
 * that merges cleanly in a register. When a slot's ONLY uses are one
 * store in each predecessor of the block whose first instruction
 * loads it, the load IS a phi:
 *
 *     %t = phi [(v1, P1), (v2, P2), ...]
 *
 * The phi machinery downstream already does everything else: codegen
 * materialises the value at each predecessor's terminator, the tmp
 * allocator gives %t a register home, and with the slot gone the
 * frame usually goes with it. */
static int irPromoteSlotToPhi(IrFunction *fn) {
    int changed = 0;
    listForEach(fn->blocks) {
        IrBlock *join = (IrBlock *)it->value;

        IrInstr *ld = NULL;
        listForEach(join->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            ld = I;
            break;
        }
        if (!ld || ld->op != IR_LOAD) continue;
        if (!irIsTmp(ld->dst) || !irIsTmp(ld->r1)) continue;
        u32 slot = irR1VarId(ld);

        /* Survey every mention of the slot: the alloca that defines
         * it, stores into it, and this one load. Anything else - a
         * second load, an address-taken use, a deref - disqualifies. */
        IrInstr *slot_alloca = NULL;
        Map *store_of = mapNew(16, &map_uint_to_uint_type); /* block->store */
        int bad = 0;
        listForEach(fn->blocks) {
            IrBlock *bb = (IrBlock *)it->value;
            listForEach(bb->instructions) {
                IrInstr *I = (IrInstr *)it->value;
                if (I->op == IR_NOP || I == ld) continue;
                int dst_is_slot = irIsTmp(I->dst) && irDstVarId(I) == slot;
                if (I->op == IR_ALLOCA && dst_is_slot) {
                    slot_alloca = I;
                    continue;
                }
                if (I->op == IR_STORE && dst_is_slot &&
                    I->dst->kind == ld->r1->kind)
                {
                    if (bb == join) { bad = 1; break; }
                    /* Two stores in one block: the later one is the
                     * value that reaches the block's end - overwrite. */
                    mapRemove(store_of, (void *)(u64)bb->id);
                    mapAdd(store_of, (void *)(u64)bb->id, I);
                    continue;
                }
                IrValue *vs[4] = { I->dst, I->r1, I->r2, I->idx };
                for (int k = 0; k < 4 && !bad; ++k) {
                    if (irIsTmp(vs[k]) && irVarId(vs[k]) == slot) bad = 1;
                }
                if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                    Vec *args = I->r1->as.array.values;
                    for (u64 a = 0; a < args->size && !bad; ++a) {
                        IrValue *v = (IrValue *)args->entries[a];
                        if (irIsTmp(v) && irVarId(v) == slot) bad = 1;
                    }
                }
                if (bad) break;
            }
            if (bad) break;
        }

        Map *preds = irFunctionGetPredecessors(fn, join);
        if (bad || !slot_alloca || !preds || preds->size == 0 ||
            preds->size != store_of->size)
        {
            mapRelease(store_of);
            continue;
        }
        /* Every predecessor must be a storing block (the size check
         * above then makes the sets equal). */
        int all_covered = 1;
        MapIter pit;
        mapIterInit(preds, &pit);
        while (mapIterNext(&pit)) {
            IrBlock *P = (IrBlock *)pit.node->value;
            if (!P || !mapHasInt(store_of, P->id)) { all_covered = 0; break; }
        }
        if (!all_covered) {
            mapRelease(store_of);
            continue;
        }

        /* Rewrite the load into the phi and retire the stores. */
        ld->op = IR_PHI;
        ld->r1 = NULL;
        ld->extra.phi_pairs = irPairVecNew();
        mapIterInit(preds, &pit);
        while (mapIterNext(&pit)) {
            IrBlock *P = (IrBlock *)pit.node->value;
            IrInstr *st = (IrInstr *)mapGetInt(store_of, P->id);
            vecPush(ld->extra.phi_pairs, irPairNew(P, st->r1));
        }
        mapIterInit(preds, &pit);
        while (mapIterNext(&pit)) {
            IrBlock *P = (IrBlock *)pit.node->value;
            IrInstr *st = (IrInstr *)mapGetInt(store_of, P->id);
            st->op = IR_NOP;
            st->dst = st->r1 = st->r2 = NULL;
        }
        mapRelease(store_of);
        if (!irSlotHasUses(fn, slot)) {
            slot_alloca->op = IR_NOP;
            slot_alloca->dst = slot_alloca->r1 = slot_alloca->r2 = NULL;
        }
        changed++;
    }
    return changed;
}

/* How many instructions other than the `skip` ones mention `var_id`?
 * Var-id based rather than pointer based: every mention of a value is
 * its own IrValue, so pointer identity finds only the one occurrence. */
static int irVarMentions(IrFunction *fn, u32 var_id, IrInstr **skip, int nskip)
{
    int n = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            int skipped = 0;
            for (int k = 0; k < nskip; ++k) {
                if (I == skip[k]) { skipped = 1; break; }
            }
            if (skipped) continue;
            IrValue *vs[4] = { I->dst, I->r1, I->r2, I->idx };
            for (int k = 0; k < 4; ++k) {
                if (irIsTmp(vs[k]) && irVarId(vs[k]) == var_id) n++;
            }
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 j = 0; j < args->size; ++j) {
                    IrValue *v = (IrValue *)args->entries[j];
                    if (irIsTmp(v) && irVarId(v) == var_id) n++;
                }
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                Vec *pairs = I->extra.phi_pairs;
                for (u64 j = 0; j < pairs->size; ++j) {
                    IrPair *p = (IrPair *)pairs->entries[j];
                    if (irIsTmp(p->ir_value) &&
                        irVarId(p->ir_value) == var_id) n++;
                }
            }
        }
    }
    return n;
}

/* The instruction that defines `var_id` with `op`, if there is one. */
static IrInstr *irFindDefOf(IrFunction *fn, u32 var_id, IrOp op) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != op || !irIsTmp(I->dst)) continue;
            if (irDstVarId(I) == var_id) return I;
        }
    }
    return NULL;
}

/* A register-returned aggregate handed straight back to the caller.
 *
 *   alloca %slot, <struct>
 *   lea    %addr, %slot
 *   ...
 *   call   %buf, F      ; args[0] == %addr, IRCG_CALL_AGG_RETURN
 *   ret    %slot
 *
 * The callee has already left the bytes in the registers the epilogue
 * wants. The emitter stores them through %addr anyway and the `ret`
 * loads them straight back out - a round trip to nowhere, and it drags
 * the slot and the address-across-the-call with it.
 *
 * Three things must hold at once, because if any is missed the function
 * returns a wrong value in silence:
 *
 *  - the `ret` immediately follows the call, so nothing in between can
 *    disturb the result registers;
 *  - the buffer the call writes through is the SAME storage the `ret`
 *    reads - a `lea` of the ret's own slot;
 *  - both functions return the identical type. Not merely the same
 *    size: how an aggregate is spread across registers is the ABI's
 *    business and the two targets disagree, and this pass runs before
 *    either has been consulted. Identity sidesteps the question.
 *
 * The `ret` then keeps no operand at all, which is already how a void
 * return is spelled - so no backend needs to learn anything for that
 * half. Only the call is flagged. */
static void irForwardAggReturn(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        for (List *node = bb->instructions->next;
             node != bb->instructions;
             node = node->next)
        {
            IrInstr *call = (IrInstr *)node->value;
            if (call->op != IR_CALL) continue;
            if (!(call->flags & IRCG_CALL_AGG_RETURN)) continue;
            if (!call->dst || !call->dst->byval_struct_type) continue;
            /* An INDIRECT return has the callee write through the
             * buffer itself, so there is no round trip to remove. */
            AstType *rt = call->dst->byval_struct_type;
            if (astAggRetIsIndirect(rt, fn->aapcs64)) continue;
            if (!call->r1 || !call->r1->as.array.values) continue;
            Vec *args = call->r1->as.array.values;
            if (args->size == 0) continue;
            IrValue *buf = (IrValue *)args->entries[0];
            if (!irIsTmp(buf)) continue;

            List *n2 = node->next;
            while (n2 != bb->instructions &&
                   ((IrInstr *)n2->value)->op == IR_NOP) n2 = n2->next;
            if (n2 == bb->instructions) continue;
            IrInstr *rt_i = (IrInstr *)n2->value;
            if (rt_i->op != IR_RET) continue;
            if (!irIsTmp(rt_i->dst)) continue;
            if (rt_i->dst->byval_struct_type != rt) continue;

            /* `buf` has to be the address OF the slot the ret reads. */
            u32 buf_id = irVarId(buf);
            u32 slot_id = irDstVarId(rt_i);
            IrInstr *lea = NULL;
            listForEach(bb->instructions) {
                IrInstr *I = (IrInstr *)it->value;
                if (I->op != IR_LEA || !irIsTmp(I->dst)) continue;
                if (irDstVarId(I) != buf_id) continue;
                lea = I;
                break;
            }
            if (!lea || !irIsTmp(lea->r1)) continue;
            if (irVarId(lea->r1) != slot_id) continue;

            /* Nothing else may read the buffer address, the slot, or
             * the call's own destination - all three stop existing.
             * The alloca is excused: it is the slot's definition, and
             * it goes with it. */
            IrInstr *alloca = irFindDefOf(fn, slot_id, IR_ALLOCA);
            IrInstr *skip[4] = { call, lea, rt_i, alloca };
            if (irVarMentions(fn, buf_id, skip, 4)) continue;
            if (irVarMentions(fn, slot_id, skip, 4)) continue;
            if (irIsTmp(call->dst) &&
                irVarMentions(fn, irDstVarId(call), skip, 4)) continue;

            call->flags |= IRCG_AGG_RET_FORWARD;
            rt_i->dst = NULL;
            lea->op = IR_NOP;
            lea->dst = lea->r1 = lea->r2 = lea->idx = NULL;
            if (alloca) {
                alloca->op = IR_NOP;
                alloca->dst = alloca->r1 = alloca->r2 = alloca->idx = NULL;
            }
        }
    }
}

/* How many instructions other than the `skip` ones mention LOCAL `lid`?
 *
 * Not irVarMentions, which matches on the var id alone and is only ever
 * asked about temporaries. Locals are numbered from `lvar_id` and
 * temporaries from `ir_tmp_var_id` - two counters, so the same id names
 * one of each and matching on the number alone answers about the wrong
 * value. Asking it about a local instead counted no mentions at all,
 * because every test in it is `irIsTmp`. */
static int irLocalMentions(IrFunction *fn, u32 lid,
                           IrInstr **skip, int nskip)
{
    int n = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            int skipped = 0;
            for (int k = 0; k < nskip; ++k) {
                if (I == skip[k]) { skipped = 1; break; }
            }
            if (skipped) continue;
            IrValue *vs[4] = { I->dst, I->r1, I->r2, I->idx };
            for (int k = 0; k < 4; ++k) {
                IrValue *v = vs[k];
                if (v && v->kind == IR_VAL_LOCAL && irVarId(v) == lid) n++;
            }
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 j = 0; j < args->size; ++j) {
                    IrValue *a = (IrValue *)args->entries[j];
                    if (a && a->kind == IR_VAL_LOCAL && irVarId(a) == lid) n++;
                }
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                Vec *pairs = I->extra.phi_pairs;
                for (u64 j = 0; j < pairs->size; ++j) {
                    IrPair *p = (IrPair *)pairs->entries[j];
                    if (p->ir_value && p->ir_value->kind == IR_VAL_LOCAL &&
                        irVarId(p->ir_value) == lid) n++;
                }
            }
        }
    }
    return n;
}

/* A parameter spilled into its slot and immediately handed to a call as
 * the FIRST argument:
 *
 *   store %l, %p
 *   call  %t, f      ; args[0] == %l
 *
 * The value is already in the register the parameter arrived in, so the
 * spill and the reload are a copy to nowhere. `void *M(u64 cap) { return
 * malloc(cap); }` compiled to `mov x13, x0` and then nothing - the
 * reload was already elided because x0 still held the value, leaving a
 * home store whose home is never read.
 *
 * Forwarding a register-resident source into call arguments is refused
 * in general (see irRewriteOperands): the argument-loading sequence
 * writes the ABI registers in order, so a later argument whose source
 * sits in an earlier argument's target register reads garbage. Position
 * ZERO has no earlier argument. Both backends materialise args[0] first
 * and write no general register before it - provided this is not an
 * aggregate-returning call, where args[0] is a destination buffer
 * rather than an argument at all and the emitter touches x9/r10 on the
 * way past. That case is excluded rather than reasoned about.
 *
 * The local then has no references left, so it loses its stack slot and
 * its promotion together. */
static void irForwardParamToFirstArg(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        for (List *node = bb->instructions->next;
             node != bb->instructions;
             node = node->next)
        {
            IrInstr *st = (IrInstr *)node->value;
            if (st->op != IR_STORE) continue;
            if (!st->dst || st->dst->kind != IR_VAL_LOCAL) continue;
            if (!st->r1  || st->r1->kind  != IR_VAL_PARAM) continue;
            /* A narrowing spill is doing real work - the parameter is
             * wider than the cell it lands in, and the argument must be
             * the truncated value. */
            if (st->dst->type != st->r1->type) continue;
            if (irValueByteSize(st->dst) != irValueByteSize(st->r1)) continue;

            List *n2 = node->next;
            while (n2 != bb->instructions &&
                   ((IrInstr *)n2->value)->op == IR_NOP) n2 = n2->next;
            if (n2 == bb->instructions) continue;
            IrInstr *call = (IrInstr *)n2->value;
            if (call->op != IR_CALL) continue;
            if (call->flags & IRCG_CALL_AGG_RETURN) continue;
            if (!call->r1 || !call->r1->as.array.values) continue;

            Vec *args = call->r1->as.array.values;
            if (args->size == 0) continue;
            u32 lid = irDstVarId(st);
            IrValue *a0 = (IrValue *)args->entries[0];
            if (!a0 || a0->kind != IR_VAL_LOCAL || irVarId(a0) != lid) {
                continue;
            }
            /* Exactly one argument, and no other mention anywhere - a
             * second read would still want the slot this is about to
             * take away. */
            int in_args = 0;
            for (u64 k = 0; k < args->size; ++k) {
                IrValue *a = (IrValue *)args->entries[k];
                if (a && a->kind == IR_VAL_LOCAL && irVarId(a) == lid) {
                    in_args++;
                }
            }
            if (in_args != 1) continue;
            IrInstr *skip[2] = { st, call };
            if (irLocalMentions(fn, lid, skip, 2)) continue;

            args->entries[0] = st->r1;
            st->op = IR_NOP;
            st->dst = st->r1 = st->r2 = st->idx = NULL;
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
/* Can `I` destroy a caller-saved register the IR does not name? A call,
 * or an `asm {}` block, which may write any register at all. */
static int irOpClobbersCallerSaved(IrInstr *I) {
    return I->op == IR_CALL || I->op == IR_ASM;
}

static int irValMatchesSlot(IrValue *v, IrValue *slot) {
    return v && slot &&
           v->kind == slot->kind &&
           irVarId(v) == irVarId(slot);
}

static int irLocIsScratchClobbered(IrValue *v);
static int irLocClobberedBy(IrInstr *I, IrValue *v, IrClobberPoint at);

/* May a SUB-WORD source be forwarded into this instruction?
 *
 * Reading a narrow value out of its slot canonicalises it for free -
 * the load picks the movs/movz form from the slot's declared
 * signedness. A value still sitting in its ABI argument register has
 * had no such treatment: SysV leaves the upper bits of a narrow
 * argument register UNSPECIFIED, so forwarding past the load would
 * hand the consumer whatever the caller happened to leave there.
 *
 * Unless the consumer canonicalises it ITSELF. IR_SEXT / IR_ZEXT /
 * IR_TRUNC read their operand at exactly `src_sz` bytes and widen or
 * narrow it explicitly - `movslq %eax, %rax` looks at 32 bits and no
 * more - so the upper bits they inherit are irrelevant. And those
 * three are precisely what the IR wraps around every narrow read
 * (irNormaliseNarrowInt), which is why this is worth doing at all:
 * `int add(int a, int b)` is a store, a reload and a widen per
 * parameter, where `long add(long, long)` is a single `addq`. */
static int irNarrowForwardOk(IrInstr *I) {
    return I->op == IR_SEXT || I->op == IR_ZEXT || I->op == IR_TRUNC;
}

static int irRewriteOperands(IrInstr *I, IrValue *slot, IrValue *source,
                             int narrow)
{
    int changed = 0;
    /* IR_LEA / IR_GEP / IR_LOAD / IR_STORE_DEREF (address operand) all
     * need their operand to have a real frame loff. They emit
     * `leaq loff(%rbp), ...` and friends. A `loc=REG`-only source
     * (e.g. a param's arrive value) can't satisfy that. */
    if (I->op == IR_LEA || I->op == IR_GEP || I->op == IR_LOAD)
        return 0;
    if (narrow && !irNarrowForwardOk(I))
        return 0;
    /* A source living in a backend scratch register (a param's arrive
     * reg overlapping x0/x1/x2 on AArch64) only survives until the
     * consumer's OWN operand materialisation starts writing scratch
     * regs. Ask the backend per operand position rather than refusing
     * every scratch-resident source outright: r1 is materialised
     * first, so nothing has been written when it is read; r2 is read
     * after r1 landed in scratch 0, which is what broke
     * `D(I64 x){ 10/x }` (`mov x0,#10` over the param, then 10/10).
     * Only the source actually sitting in the register r1's load
     * targets is at risk - a param in x1 is untouched by a load into
     * x0. STORE_DEREF / RMW_DEREF read their value operand LAST, so
     * the backend reports both positions unsafe there. */
    if (!irLocClobberedBy(I, source, IR_CLOBBER_BEFORE_R1) &&
        irValMatchesSlot(I->r1, slot) && I->r1 != source)
    {
        I->r1 = source;
        changed = 1;
    }
    if (!irLocClobberedBy(I, source, IR_CLOBBER_BEFORE_R2) &&
        irValMatchesSlot(I->r2, slot) && I->r2 != source)
    {
        I->r2 = source;
        changed = 1;
    }
    /* IR_BR/IR_RET carry their value operand in dst. */
    if ((I->op == IR_BR || I->op == IR_RET) &&
        irValMatchesSlot(I->dst, slot) && I->dst != source)
    {
        I->dst = source;
        changed = 1;
    }
    /* A deref store's dst is the ADDRESS it writes through - an input,
     * and the first one materialised, so nothing in the instruction has
     * run when it is read. Both backends take it straight out of a
     * register when it already lives in one, so there is no frame
     * offset to need; the reason this used to be refused outright.
     * Leaving it refused is what kept `f->e[i] = v` copying `f` into a
     * register of its own before the store. */
    if ((I->op == IR_STORE_DEREF || I->op == IR_RMW_DEREF) &&
        !irLocClobberedBy(I, source, IR_CLOBBER_BEFORE_DST) &&
        irValMatchesSlot(I->dst, slot) && I->dst != source)
    {
        I->dst = source;
        changed = 1;
    }
    /* CALL args: forwarding a *register-resident* source here is unsafe -
     * the call's per-arg loading sequence clobbers ABI arg regs in order,
     * so a later arg whose forwarded source.loc names an earlier arg's
     * target reg reads garbage. A *constant* source has no such hazard
     * (it's materialised fresh as an immediate at the use site), so we can
     * fold `f(x)` after `x = <const>` into `f(<const>)`. */
    if (I->op == IR_CALL && irIsConstInt(source) && I->r1 &&
        I->r1->as.array.values)
    {
        Vec *args = I->r1->as.array.values;
        for (u64 i = 0; i < args->size; ++i) {
            IrValue *a = (IrValue *)args->entries[i];
            if (irValMatchesSlot(a, slot) && a != source) {
                args->entries[i] = source;
                changed = 1;
            }
        }
    }
    /* Phi values materialise at the predecessor's terminator - AFTER a
     * CMP_BR terminator's compare has already run r1/r2 through the
     * scratch regs - so a scratch-homed source is stale by then. The
     * clobbering instruction is that terminator rather than `I`, so
     * this stays on the blanket "is it scratch at all" test. */
    if (I->op == IR_PHI && I->extra.phi_pairs &&
        !irLocIsScratchClobbered(source))
    {
        for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
            IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
            if (p && irValMatchesSlot(p->ir_value, slot) &&
                p->ir_value != source)
            {
                p->ir_value = source;
                changed = 1;
            }
        }
    }
    return changed;
}

/* True if emitting `I` destroys the value `v` is holding in its
 * register, at the point `at` of I's emission.
 *
 * Two questions, deliberately kept apart: is `v` even in a register
 * the backend treats as scratch (if not, nothing can disturb it), and
 * if so does THIS instruction actually write that register by that
 * point. The backend answers the second via `pool->op_clobbers`; with
 * no such hook we fall back to "any op clobbers any scratch reg". */
static int irLocClobberedBy(IrInstr *I, IrValue *v, IrClobberPoint at) {
    if (!irLocIsScratchClobbered(v)) return 0;
    IrRegPool *pool = irRegPoolGet();
    if (!pool || !pool->op_clobbers) return 1;
    return pool->op_clobbers(I, v->loc.as.reg, at);
}

/* True if `v` is pinned to a register the backend declared as
 * per-instruction scratch - i.e. a register that SOME op may clobber.
 * Callers with an instruction in hand should prefer
 * `irLocClobberedBy`, which narrows this to the op actually crossed. */
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

static int irForwardStoreToReads(IrFunction *fn) {
    int changed = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;

        /* Parallel arrays: slots[i] is a slot IrValue, sources[i] is
         * the value currently held in it, narrow[i] records whether
         * that value is sub-word (see irNarrowForwardOk). */
        IrValue **slots = NULL;
        IrValue **sources = NULL;
        int *narrow = NULL;
        u64 n = 0, cap = 0;

        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;

            for (u64 i = 0; i < n; ++i) {
                changed += irRewriteOperands(I, slots[i], sources[i],
                                             narrow[i]);
            }

            /* Kills clear the entire map. */
            if (irOpClobbersCallerSaved(I) || I->op == IR_STORE_DEREF ||
                I->op == IR_RMW_DEREF)
            {
                n = 0;
                continue;
            }

            /* Scratch-clobber: drop any source whose register THIS op
             * actually overwrites. The backend decides - e.g. an
             * IR_STORE of a value that is already in a register stores
             * straight out of it and burns no scratch at all, while a
             * store of a constant materialises it through scratch 0
             * (mov x0, #N; stur x0, ...) and does destroy a param
             * sitting in x0. Asking per instruction is what lets two
             * consecutive param spills both forward. */
            if (I->op != IR_NOP && I->op != IR_JMP && I->op != IR_PHI &&
                I->op != IR_LABEL && I->op != IR_ALLOCA)
            {
                u64 j = 0;
                while (j < n) {
                    if (irLocClobberedBy(I, sources[j], IR_CLOBBER_AFTER)) {
                        slots[j] = slots[n - 1];
                        sources[j] = sources[n - 1];
                        narrow[j] = narrow[n - 1];
                        n--;
                    } else {
                        j++;
                    }
                }
            }

            /* Redefinition kill: a non-store op whose dst is a tracked
             * slot (e.g. `iadd l2, l2, 1` writing a param's slot in
             * place) makes the recorded source stale - the slot now holds
             * this op's result, not the stored value. Without this, a
             * later read of the slot is wrongly rewritten to the old
             * value (`return arg` after `arg += 1` became `ret <param>`).
             * The IR_STORE case below maintains its own entry. */
            if (I->op != IR_STORE && I->dst) {
                for (u64 i = 0; i < n; ++i) {
                    if (irValMatchesSlot(I->dst, slots[i])) {
                        slots[i] = slots[n - 1];
                        sources[i] = sources[n - 1];
                        narrow[i] = narrow[n - 1];
                        n--;
                        break;
                    }
                }
            }

            /* Writing a slot also invalidates every entry that names it
             * as a SOURCE. An entry means "this slot holds whatever S
             * holds"; once S is overwritten that is no longer true, and
             * forwarding through it reads the new value instead of the
             * one that was copied.
             *
             * A three-way swap is the shape that exposes it:
             *
             *     t = p;      map: t -> p
             *     p = q;      map: t -> p (STALE), p -> q
             *     q = t;      t rewritten to p, p rewritten to q
             *
             * leaving `q = q` and two identical pointers. The dead-store
             * pass then removed the other two stores as unread, so the
             * whole swap vanished. Any local swapped through a temporary
             * was affected, which is most hand-written swaps.
             *
             * Applies to any op that writes a slot, not just a store:
             * `iadd l2, l2, 1` invalidates an entry recording l2 just as
             * much. A `dst` that is a tmp matches nothing here, because
             * only locals are ever recorded as slots. */
            if (I->dst) {
                u64 i = 0;
                while (i < n) {
                    if (!irValMatchesSlot(I->dst, slots[i]) &&
                        irValMatchesSlot(I->dst, sources[i]))
                    {
                        slots[i] = slots[n - 1];
                        sources[i] = sources[n - 1];
                        narrow[i] = narrow[n - 1];
                        n--;
                    } else {
                        i++;
                    }
                }
            }

            /* Update map on STORE: dst-slot now holds r1's value.
             *
             * A full-width store is forwardable to any reader. A
             * SUB-WORD one is recorded too, but flagged, because only
             * a reader that re-canonicalises may have it - see
             * irNarrowForwardOk. The store must not itself be doing the
             * narrowing (`store i8 <- i32` truncates, and that work
             * cannot be skipped), so the widths have to match, and so
             * must the signedness the two would extend by. */
            int st_ok = 0, st_narrow = 0;
            /* A REG-PINNED local is never forwarded from. The point of
             * pinning is that the value lives in a named machine
             * register, where something outside the compiler's view can
             * change it - an `asm` block being the whole reason anyone
             * writes it. Forwarding the stored value past that reads a
             * stale constant, and once every read is rewritten the
             * store is dead and the pin disappears entirely: no `mov
             * x23, #23` was emitted at all, so `int reg X23 x = 23;
             * asm { ADD X23, X23, 1 } return x;` folded to 23. */
            if (I->op == IR_STORE && I->dst && I->dst->pinned_reg) {
                /* not forwardable - fall through with st_ok = 0 */
            } else if (I->op == IR_STORE && I->dst && I->r1) {
                u32 dsz = irValueByteSize(I->dst);
                u32 ssz = irValueByteSize(I->r1);
                if (ssz == 8 && dsz == 8) {
                    st_ok = 1;
                } else if (ssz == dsz && (ssz == 1 || ssz == 2 || ssz == 4) &&
                           irIsInt(I->dst->type) && irIsInt(I->r1->type) &&
                           ((I->dst->flags & IR_VAL_FLAG_UNSIGNED) ==
                            (I->r1->flags & IR_VAL_FLAG_UNSIGNED)))
                {
                    st_ok = 1;
                    st_narrow = 1;
                }
            }

            if (st_ok) {
                for (u64 i = 0; i < n; ++i) {
                    if (irValMatchesSlot(I->dst, slots[i])) {
                        sources[i] = I->r1;
                        narrow[i] = st_narrow;
                        goto did_update;
                    }
                }
                if (n == cap) {
                    cap = cap ? cap * 2 : 8;
                    slots = realloc(slots, cap * sizeof(IrValue *));
                    sources = realloc(sources, cap * sizeof(IrValue *));
                    narrow = realloc(narrow, cap * sizeof(int));
                }
                slots[n] = I->dst;
                sources[n] = I->r1;
                narrow[n] = st_narrow;
                n++;
                did_update:;
            } else if (I->op == IR_STORE && I->dst && I->r1) {
                /* A store we cannot forward still KILLS the slot: it now
                 * holds something the map does not describe. */
                for (u64 i = 0; i < n; ++i) {
                    if (irValMatchesSlot(I->dst, slots[i])) {
                        slots[i] = slots[n - 1];
                        sources[i] = sources[n - 1];
                        narrow[i] = narrow[n - 1];
                        n--;
                        break;
                    }
                }
            }
        }

        free(slots);
        free(sources);
        free(narrow);
    }
    return changed;
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
            /* `idx` is a READ too. Addressing-mode fusion moves a
             * value into it and out of r1/r2, so leaving it out here
             * made a slot look unread the moment it became an index -
             * dead-store elimination then killed the write, and the
             * backend emitted the base register for the index as well:
             * `ldrsw x0, [x1, x1, lsl #2]` for `p->data[i]`. */
            IrValue *vs[4] = { I->r1, I->r2, I->idx, NULL };
            /* IR_BR/IR_RET read dst; IR_STORE_DEREF and IR_RMW_DEREF
             * read dst (as address). IR_STORE's dst is a write target,
             * not a read.
             *
             * IR_RMW_DEREF is the one that has to be spelled out: it is
             * MADE here, by irFuseLocalRmw folding `*p OP= v` moments
             * before this runs, so a slot holding the base can lose
             * every other reader in the same sweep. Leave it out and
             * the store that fills the base is dead-store eliminated
             * while the RMW still reads it - a local with no remaining
             * definition, which regalloc then has no slot for. */
            if (I->op == IR_BR || I->op == IR_RET ||
                I->op == IR_STORE_DEREF || I->op == IR_RMW_DEREF)
            {
                vs[3] = I->dst;
            }
            for (int k = 0; k < 4; ++k) {
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

/* The list node holding `I`, searching forward from the node after
 * `from` up to `sentinel`; NULL if it is not in that range. */
static List *irNodeOf(List *from, List *sentinel, IrInstr *I) {
    for (List *p = from->next; p != sentinel; p = p->next) {
        if ((IrInstr *)p->value == I) return p;
    }
    return NULL;
}

/* True if a call (or an asm block) sits strictly between `from` and `to`. */
static int irRangeContainsCall(List *from, List *to) {
    for (List *p = from->next; p != to; p = p->next) {
        if (irOpClobbersCallerSaved((IrInstr *)p->value)) return 1;
    }
    return 0;
}

/* True if `v` is pinned to a register a callee is free to destroy -
 * anything outside the callee-saved pool.
 *
 * Not the same question as irLocIsScratchClobbered. That one asks
 * about the registers THIS function's emitter burns between
 * instructions, and rdi is scratch to nobody; but every call clobbers
 * it, along with the rest of the argument registers a PARAM arrives
 * in (rdi/rsi/r8/r9 on SysV, x3-x7 on AAPCS64 - x0-x2 are also
 * scratch, which is why the bug hid on arm64 for a one-parameter
 * method). With no pool to consult every register is assumed at
 * risk. */
static int irLocIsCallClobbered(IrValue *v) {
    if (!v || v->loc.kind != IR_LOC_REG || !v->loc.as.reg) return 0;
    IrRegPool *pool = irRegPoolGet();
    if (!pool || !pool->callee_saved_regs) return 1;
    AoStr *vr = v->loc.as.reg;
    for (u64 i = 0; i < pool->callee_saved_regs->size; ++i) {
        if (aoStrEq(vr, vecGet(AoStr *, pool->callee_saved_regs, i))) return 0;
    }
    return 1;
}

/* Would moving a read of `v` from `from` to the consumer at `to` carry
 * it across a call that destroys its register?
 *
 * Addressing-mode fusion RELOCATES reads: the base of `iadd %t, %p,
 * 16` was read at the IADD, and folding it into `store* %p [16]`
 * reads it at the store instead. A parameter forwarded into the IADD
 * sits in its argument register, which the forwarding pass correctly
 * stopped propagating AT the call - and this fold then resurrected it
 * on the far side. `self->offset += f(...)` stored through rdi after
 * f had been handed rdi as its own first argument. A consumer whose
 * node is unknown is treated as out of reach. */
static int irReadCrossesCall(IrValue *v, List *from, List *to) {
    if (!irLocIsCallClobbered(v)) return 0;
    return !to || irRangeContainsCall(from, to);
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

/* Is `J` reading or writing memory through `v` as its address? */
static int irIsMemAddressUse(IrInstr *J, IrValue *v) {
    return (J->op == IR_LOAD_DEREF && J->r1 == v) ||
           ((J->op == IR_STORE_DEREF || J->op == IR_RMW_DEREF) && J->dst == v);
}

/* Is the local `v`'s address taken anywhere in `fn`? Then a store
 * through a pointer, or a call, could change it. */
static int irLocalAddressTaken(IrFunction *fn, IrValue *v) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *J = (IrInstr *)it->value;
            if ((J->op == IR_LEA || J->op == IR_GEP) && J->r1 &&
                irValMatchesSlot(J->r1, v)) return 1;
        }
    }
    return 0;
}

/* Does anything strictly between `from` and `to` write the local `v`? */
static int irLocalWrittenBetween(IrValue *v, List *from, List *to) {
    for (List *q = from->next; q != to; q = q->next) {
        IrInstr *J = (IrInstr *)q->value;
        if (J->op == IR_STORE_DEREF || J->op == IR_RMW_DEREF ||
            J->op == IR_BR || J->op == IR_RET) continue;
        if (J->dst && irValMatchesSlot(J->dst, v)) return 1;
    }
    return 0;
}

/* Does `v` read the same between `from` and `to`? Its REGISTER has to
 * survive as well as its value, so never across a call or an `asm`
 * block, and never for a value in a scratch register across an op that
 * burns scratch - SysV's fourth argument arrives in %rcx, the
 * emitters' scratch. The same two checks the folds make. Otherwise
 * always for a TMP, a PARAM (never reassigned - a write to a parameter
 * goes to its local) or a constant; for a LOCAL, when its address is
 * never taken and nothing in between writes it. */
static int irValueStable(IrFunction *fn, IrValue *v, List *from, List *to) {
    if (irIsConstInt(v)) return 1;
    if (irRangeContainsCall(from, to)) return 0;
    if (irLocIsScratchClobbered(v) && irRangeClobbersScratch(from, to)) return 0;
    if (irIsTmp(v) || v->kind == IR_VAL_PARAM) return 1;
    if (v->kind != IR_VAL_LOCAL) return 0;
    return !irLocalAddressTaken(fn, v) && !irLocalWrittenBetween(v, from, to);
}

/* Give each memory access through a shared address its own copy of
 * the arithmetic, so irFuseIaddIntoMemAddressing - which folds an add
 * into its ONE consumer - can fold every copy into the addressing mode.
 * Two shapes: `ptr + k` (a displacement) and `base + (i << s)` (a
 * scaled index, whose shift is copied too).
 *
 * `s->f += 1` reads and writes through one address, computed once so
 * the base is evaluated once. Shared, the add could not fold and cost
 * instructions per access (`add x14, x13, #2; ldrh w15, [x14]` where
 * `ldrh w15, [x13, #2]` will do).
 *
 * Only when every operand the copy re-reads is irValueStable up to the
 * use, and every use of the address is such an access in the same
 * block, so nothing else needs the sum. Run after the fusion loop has
 * settled: load-op-store fusion wants the address shared. */
static int irSplitSharedAddressing(IrFunction *fn) {
    Map *uses = irBuildTmpUseCounts(fn);
    Map *defs = irBuildTmpDefs(fn);
    int changed = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        List *l = bb->instructions;
        for (List *cur = l->next; cur != l; cur = cur->next) {
            IrInstr *I = (IrInstr *)cur->value;
            if (I->op != IR_IADD && I->op != IR_ISUB) continue;
            if (!irIsTmp(I->dst) || I->dst->as.var.size != 8) continue;
            if (!I->r1 || !I->r2) continue;
            int n_uses = irTmpUseCount(uses, I->dst);
            if (n_uses < 2) continue;

            /* What a copy has to re-read: the operands of the add, or -
             * for a scaled index - the base and the shift's operands. */
            IrInstr *scaler = NULL;
            List *scaler_node = NULL;
            IrValue *reads[3] = { I->r1, I->r2, NULL };
            if (!irIsConstInt(I->r1) && !irIsConstInt(I->r2)) {
                u8 scale = 0;
                if (I->op != IR_IADD) continue;
                int on_r2 = 1;
                scaler = irFindScalingProducer(defs, I->r2, &scale);
                if (!scaler) {
                    scaler = irFindScalingProducer(defs, I->r1, &scale);
                    on_r2 = 0;
                }
                if (!scaler) continue;
                IrValue *scaled = on_r2 ? I->r2 : I->r1;
                if (irTmpUseCount(uses, scaled) != 1) continue;
                for (List *q = l->next; q != cur; q = q->next) {
                    if (q->value == scaler) { scaler_node = q; break; }
                }
                if (!scaler_node) continue;
                reads[0] = on_r2 ? I->r1 : I->r2;
                reads[1] = scaler->r1;
                reads[2] = scaler->r2;
            }

            int n_mem = 0;
            for (List *q = cur->next; q != l; q = q->next) {
                if (irIsMemAddressUse((IrInstr *)q->value, I->dst)) n_mem++;
            }
            if (n_mem != n_uses) continue;

            int seen = 0;
            for (List *q = cur->next; q != l; q = q->next) {
                IrInstr *J = (IrInstr *)q->value;
                if (!irIsMemAddressUse(J, I->dst)) continue;
                if (seen++ == 0) continue;
                List *from = scaler_node ? scaler_node : cur;
                int stable = 1;
                for (int k = 0; k < 3 && stable; ++k) {
                    if (reads[k] && !irValueStable(fn, reads[k], from, q))
                        stable = 0;
                }
                if (!stable) break;

                IrValue *r1 = I->r1, *r2 = I->r2;
                if (scaler) {
                    IrValue *sc = irTmp(scaler->dst->type,
                                        scaler->dst->as.var.size);
                    listInsertValueBefore(q, irInstrNew(scaler->op, sc,
                                                        scaler->r1,
                                                        scaler->r2));
                    if (r2 == scaler->dst) r2 = sc; else r1 = sc;
                }
                IrValue *copy = irTmp(I->dst->type, 8);
                listInsertValueBefore(q, irInstrNew(I->op, copy, r1, r2));
                if (J->op == IR_LOAD_DEREF) J->r1 = copy;
                else                        J->dst = copy;
                changed = 1;
            }
        }
    }
    mapRelease(uses);
    mapRelease(defs);
    return changed;
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

            /* Find the consumer's list node for the clobber checks. */
            List *use_node = irNodeOf(cur, l, use);

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
                    /* Refuse the fold if ptr is reg-pinned and either
                     * an intervening op clobbers scratch, or the
                     * CONSUMER's own expansion does: an IR_RMW_DEREF
                     * loads through the base into scratch 0 and then
                     * needs the base again for the store-back, so a
                     * base living in scratch 0 is gone by then
                     * (`str x0, [x0, #40]`). Either way the codegen
                     * would read a stale register at the consumer.
                     *
                     * And refuse it when a CALL sits between the two:
                     * a base in a caller-saved register - every
                     * parameter's - does not survive one, whether or
                     * not the emitter counts that register as scratch.
                     * See irReadCrossesCall. */
                    if ((irLocIsScratchClobbered(ptr) &&
                         ((use_node && irRangeClobbersScratch(cur, use_node)) ||
                          irLocClobberedBy(use, ptr, IR_CLOBBER_BEFORE_R1))) ||
                        irReadCrossesCall(ptr, cur, use_node))
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
                /* The index needs no check against the BASE any
                 * more. It used to: the base was materialised into a
                 * hardcoded x1, which is also where the second integer
                 * argument arrives, so folding `a[i]` destroyed its
                 * own index and the fold had to be declined for every
                 * scratch-resident index - which was nearly all of
                 * them. The deref emitters now resolve the index's
                 * home first and take a base scratch that avoids it,
                 * so there is no longer a way for the base to land on
                 * the index.
                 *
                 * That is a claim about ONE instruction, though, and
                 * it was mistaken for a claim about the whole range.
                 * Both the base check below and the index check under
                 * it ask the other question - whether the operand
                 * survives the instructions BETWEEN the fold and its
                 * consumer - and the index needs it just as much. */
                if (scaler &&
                    ((irLocIsScratchClobbered(base) &&
                      ((use_node && irRangeClobbersScratch(cur, use_node)) ||
                       irLocClobberedBy(use, base, IR_CLOBBER_BEFORE_R1))) ||
                     irReadCrossesCall(base, cur, use_node)))
                {
                    scaler = NULL;
                }
                /* The index's read moves too, and from further back:
                 * it was read at the scaler, which may sit well before
                 * this IADD. `s->d[i] += f(s->a)` read `i` out of rsi
                 * at the `shl`, then again at the store on the other
                 * side of f. A scaler this block cannot find lives in
                 * another block, and the whole prefix is scanned. */
                if (scaler) {
                    List *scaler_node = irNodeOf(l, l, scaler);
                    List *from = scaler_node ? scaler_node : l;
                    if (irReadCrossesCall(scaler->r1, from, use_node)) {
                        scaler = NULL;
                    } else if (irLocIsScratchClobbered(scaler->r1) &&
                               use_node &&
                               irRangeClobbersScratch(from, use_node))
                    {
                        /* The same question the base is asked, and the
                         * one a CALL check alone does not answer. A
                         * global's address is materialised into the
                         * scratch pair (`adrp x1, _g@PAGE`), which is
                         * no call at all but destroys an index sitting
                         * in x1 just the same:
                         *
                         *     void f(struct S *s, long i) { s->e[i] += g; }
                         *
                         * folded the index down past the `lea` and
                         * indexed by the ADDRESS of `g`. */
                        scaler = NULL;
                    }
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

            /* Every backend sizes the RMW's load and store from `r1`,
             * and `r1` is about to stop being the value that was STORED
             * and start being the addend. Those two need not be the same
             * width: `s->i += 1` on a 64-bit field has an `i32` literal
             * on the right, so the fused form loaded, added and stored
             * four bytes and dropped the carry - `4294967295 + 1` came
             * out as 0. The stored value carries the memory's width, so
             * match it or leave the triple alone.
             *
             * A literal is re-labelled rather than rejected: its value
             * is already held full-width, so only the label changes. */
            if (irValueByteSize(val) != irValueByteSize(S->r1)) {
                if (val->kind != IR_VAL_CONST_INT) continue;
                val = irConstInt(S->r1->type, val->as._i64);
            }

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

/* `a >= b` with the same operands swapped is `b <= a`. Mirroring a
 * predicate lets a comparison put its constant operand second. */
static IrCmpKind irCmpSwapped(IrCmpKind k) {
    switch (k) {
        case IR_CMP_LT:  return IR_CMP_GT;
        case IR_CMP_GT:  return IR_CMP_LT;
        case IR_CMP_LE:  return IR_CMP_GE;
        case IR_CMP_GE:  return IR_CMP_LE;
        case IR_CMP_ULT: return IR_CMP_UGT;
        case IR_CMP_UGT: return IR_CMP_ULT;
        case IR_CMP_ULE: return IR_CMP_UGE;
        case IR_CMP_UGE: return IR_CMP_ULE;
        case IR_CMP_OLT: return IR_CMP_OGT;
        case IR_CMP_OGT: return IR_CMP_OLT;
        case IR_CMP_OLE: return IR_CMP_OGE;
        case IR_CMP_OGE: return IR_CMP_OLE;
        /* Symmetric already. */
        case IR_CMP_EQ:  case IR_CMP_NE:
        case IR_CMP_OEQ: case IR_CMP_ONE:
        case IR_CMP_UNO: case IR_CMP_ORD:
            return k;
        default: return IR_CMP_INVALID;
    }
}

/* Put a comparison's constant operand on the RIGHT.
 *
 * Every target compares register-against-immediate, so `ch <= '9'`
 * lowers to a single `cmp w0, #57`. Written the other way round -
 * `'0' >= ch`, which is how a range check reads - the constant had to
 * be materialised into a register first, and that forced the other
 * operand out to a stack slot and back:
 *
 *      mov  x0, #48        vs.     cmp  x0, #48
 *      ldur x1, [x29,#-16]
 *      cmp  x0, x1
 *
 * Swapping the operands and mirroring the predicate means the source
 * spelling stops mattering. */
static int irCanonicaliseCmpOperands(IrFunction *fn) {
    int changed = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_ICMP && I->op != IR_FCMP &&
                I->op != IR_CMP_BR)
            {
                continue;
            }
            if (!I->r1 || !I->r2) continue;
            int l_const = I->r1->kind == IR_VAL_CONST_INT ||
                          I->r1->kind == IR_VAL_CONST_FLOAT;
            int r_const = I->r2->kind == IR_VAL_CONST_INT ||
                          I->r2->kind == IR_VAL_CONST_FLOAT;
            if (!l_const || r_const) continue;

            IrCmpKind *kind = (I->op == IR_CMP_BR)
                            ? &I->extra.cmp_br.cmp_kind
                            : &I->extra.cmp_kind;
            IrCmpKind swapped = irCmpSwapped(*kind);
            if (swapped == IR_CMP_INVALID) continue;

            IrValue *tmp = I->r1;
            I->r1 = I->r2;
            I->r2 = tmp;
            *kind = swapped;
            changed = 1;
        }
    }
    return changed;
}

/* Every use of tmp `from` becomes `to`, and how many there were.
 * Only operand positions - a `dst` that is a definition is left alone,
 * so the caller has to be replacing something whose def it removes. */
static int irReplaceTmpUses(IrFunction *fn, IrValue *from, IrValue *to) {
    int n = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->r1 == from) { if (to) I->r1 = to; n++; }
            if (I->r2 == from) { if (to) I->r2 = to; n++; }
            /* IR_BR / IR_RET carry their value in dst. */
            if ((I->op == IR_BR || I->op == IR_RET) && I->dst == from) {
                if (to) I->dst = to;
                n++;
            }
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 k = 0; k < args->size; ++k) {
                    if ((IrValue *)args->entries[k] == from) {
                        if (to) args->entries[k] = to;
                        n++;
                    }
                }
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 k = 0; k < I->extra.phi_pairs->size; ++k) {
                    IrPair *pr = (IrPair *)I->extra.phi_pairs->entries[k];
                    if (pr && pr->ir_value == from) {
                        if (to) pr->ir_value = to;
                        n++;
                    }
                }
            }
        }
    }
    return n;
}

/* `trunc i32 <- (sext i64 <- x i32)` is `x`.
 *
 * The pair comes from an assignment converting to the arithmetic type
 * on the way in and back to the destination's type on the way out -
 * `v->entries[i] = data` widens `data` to i64 and immediately narrows
 * it again. On aarch64 that is an `ldursw` plus an `sxtw` producing a
 * value the very next instruction stores as four bytes.
 *
 * Sound only when the extend's SOURCE is exactly the truncation's
 * result width: `trunc i16 <- sext i64 <- x i32` really does discard
 * bits. The extend must also have no other use, or removing it would
 * strand them. */
static int irFoldExtendTruncate(IrFunction *fn) {
    int changed = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_TRUNC || !I->dst || !I->r1) continue;

            /* Find the definition of the truncation's operand. */
            IrInstr *ext = NULL;
            listForEach(bb->instructions) {
                IrInstr *J = (IrInstr *)it->value;
                if (J == I) break;
                if (J->dst == I->r1 &&
                    (J->op == IR_SEXT || J->op == IR_ZEXT))
                {
                    ext = J;
                }
            }
            if (!ext || !ext->r1) continue;
            /* The fold rewrites every USE of the truncation's
             * destination to name `ext->r1` instead, which is only
             * meaningful when that destination is a temporary - a
             * value, not a place.
             *
             * `x = y;` between narrow locals lowers to `trunc <local>,
             * <tmp>`, so the destination is a LOCAL, and rewriting its
             * uses substituted a temporary for the variable itself.
             * Everything downstream then named the temporary, including
             * `&x`, which became the address of a value with no
             * storage. The visible symptom was a swap through a
             * temporary producing garbage, but only once the address
             * was also taken and dereferenced. */
            if (!irIsTmp(I->dst)) continue;
            if (irValueByteSize(ext->r1) != irValueByteSize(I->dst)) continue;
            /* The fold hands the truncation's consumers `ext->r1`
             * directly, which is sound only while that value is already
             * CANONICAL for its own width - sign-extended if signed,
             * zero-extended if unsigned.
             *
             * A narrow value read out of a slot is: the load picks its
             * movs/movz form from the slot's signedness. A narrow
             * PARAMETER still sitting in its ABI argument register is
             * not - the psABI leaves those upper bits unspecified - so
             * there the extend is doing real work rather than repeating
             * the load's. Dropping it made
             * `char TakeI8(char p) { return p; }` return 200 where C
             * gives -56.
             *
             * Only reachable since irForwardStoreToReads learned to
             * forward sub-word parameters past their home slot; before
             * that every narrow read came from memory. */
            if (irValueByteSize(ext->r1) < 8 &&
                ext->r1->kind == IR_VAL_PARAM &&
                ext->r1->loc.kind == IR_LOC_REG)
            {
                continue;
            }
            if (irReplaceTmpUses(fn, ext->dst, NULL) != 1) continue;

            irReplaceTmpUses(fn, I->dst, ext->r1);
            irMakeNop(I);
            irMakeNop(ext);
            changed = 1;
        }
    }
    return changed;
}

static int irOpReadsTmp(IrInstr *I, u32 var_id);

/* Does `I` WRITE its dst, as opposed to reading it? Mirrors
 * ir-regalloc.c's irLiveInstrDefines - four ops keep an operand there
 * rather than a result, and calling those definitions would end a
 * value's range an instruction early. */
static int irInstrWritesDst(IrInstr *I) {
    if (!I->dst) return 0;
    switch (I->op) {
    case IR_STORE_DEREF: case IR_RMW_DEREF:
    case IR_BR: case IR_RET:
        return 0;
    default:
        return 1;
    }
}

/* A widening read of a PROMOTED narrow local is a no-op.
 *
 * `irPromoteNarrowInt` widens every narrow read at lowering, because a
 * narrow value in a register otherwise has unspecified upper bits. For
 * a local that promotion later moved into a register that is no longer
 * true: its home is canonicalised on every write (see
 * aarch64StoreNarrowHome), so it already holds exactly the value the
 * extension would compute. `short s` in x13 read `sxth x0, w13` and
 * then used x0 - one instruction to copy a register into another
 * register.
 *
 * So the widen is dropped and its destination tmp is given the SOURCE's
 * home. The tmp keeps its own i64 tagging, which matters: substituting
 * the narrow value itself would let a consumer size its operation from
 * two bytes instead of eight.
 *
 * Run AFTER irOptPromoteToRegs - `pinned_reg` is what it keys on, and
 * before promotion nothing has one. That also makes it a no-op on
 * x86_64, which does not promote at all.
 *
 * Two conditions, both about the home holding the right thing for as
 * long as the tmp is read:
 *
 *   - the extension must match how the home is canonicalised (SEXT for
 *     a signed local, ZEXT for an unsigned one), or it would compute a
 *     different number than the register holds;
 *   - nothing may write the local while the tmp is still live, or the
 *     tmp would silently see the new value. `short s = 1; long a = s;
 *     s = 2; use(a);` must not read 2. */
int irElideCanonicalWiden(IrFunction *fn) {
    int changed = 0;
    /* Which tmps are call arguments - see the refusal below. */
    Set *call_args = setNew(16, &set_uint_type);
    irCollectCallArgValues(fn, call_args);
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        for (List *node = bb->instructions->next; node != bb->instructions;
             node = node->next)
        {
            IrInstr *I = (IrInstr *)node->value;
            if (I->op != IR_SEXT && I->op != IR_ZEXT) continue;
            IrValue *src = I->r1, *dst = I->dst;
            if (!src || !dst || !irIsTmp(dst)) continue;
            if (src->kind != IR_VAL_LOCAL && src->kind != IR_VAL_PARAM)
                continue;
            if (!src->pinned_reg) continue;   /* not promoted */
            if (!irIsInt(src->type) || !irIsInt(dst->type)) continue;
            u32 sz = irValueByteSize(src);
            if (sz == 0 || sz >= 8) continue;
            if (irValueByteSize(dst) != 8) continue;
            /* irOptPinResultReg may already have parked this tmp in the
             * result register so its consumer could read it there. That
             * is a preference, not a requirement - every consumer looks
             * the home up - and the source's own register is strictly
             * better, so it is overridden. A HAND-pinned register is
             * not: the source asked for that one by name. */
            if (dst->pinned_reg) continue;

            int uns = (src->flags & IR_VAL_FLAG_UNSIGNED) != 0;
            if (uns ? (I->op != IR_ZEXT) : (I->op != IR_SEXT)) continue;

            /* Every read of the tmp must happen in this block, before
             * anything redefines the local. Anything else - a use in
             * another block, a use after a write - and the home has
             * stopped standing for the value the tmp was given. */
            u32 src_id = irVarId(src), dst_id = irVarId(dst);
            int reads = 0, written = 0, unsafe = 0;
            /* Is any read of the tmp on the far side of a CALL? */
            int seen_call = 0, read_after_call = 0;
            for (List *p = node->next; p != bb->instructions; p = p->next) {
                IrInstr *J = (IrInstr *)p->value;
                if (J->op == IR_NOP) continue;
                if (irOpClobbersCallerSaved(J)) seen_call = 1;
                /* Read first, then write: an instruction that does both
                 * - `trunc %s <- %t`, the store back of `s += 7` - reads
                 * the old value before producing the new one, so it is
                 * a safe read, not a stale one. */
                if (irOpReadsTmp(J, dst_id)) {
                    if (written) { unsafe = 1; break; }
                    if (seen_call) read_after_call = 1;
                    reads++;
                }
                if (irInstrWritesDst(J) && irVarId(J->dst) == src_id) {
                    written = 1;
                }
            }
            if (unsafe) continue;
            /* Every use in the whole function must be one of the reads
             * counted above - anything else is in another block, or
             * after the local was rewritten. */
            if (irReplaceTmpUses(fn, dst, NULL) != reads) continue;

            /* The home may be an ABI ARGUMENT register - the promoter
             * hands those out now - and `dst` may be handed to a call
             * as an argument. Sharing the home then puts a value in a
             * register the argument-loading sequence overwrites before
             * it reads it: `int a = INT_MAX; a += 1; printf("%ld", a)`
             * printed the format string's address.
             *
             * The allocator refuses that pairing (see
             * irCollectCallArgValues); this shares a register it did
             * not choose, so it has to refuse it too. */
            if (call_args && irRegIsAbiArg(irRegPoolGet(), src->pinned_reg) &&
                setHas(call_args, (void *)(u64)irVarId(dst)))
            {
                continue;
            }

            /* The LOCAL's range and the TMP's are not the same range.
             * Promotion picks the local's home from its own liveness,
             * so a local that dies before the next call gets a
             * caller-saved register - correct for it, wrong for a tmp
             * that is still being read afterwards:
             *
             *     int m = ...;
             *     take(m, mk(1), mk(2));    // m's widen read LAST
             *
             * `m` is dead after the widen, so it had x13; the widened
             * value was then read across two calls out of a register
             * they are free to clobber, and the callee saw an address.
             * Sharing is only sound while the home outlives every read
             * of the tmp. */
            if (read_after_call &&
                !irRegSurvivesCall(irRegPoolGet(), src->pinned_reg))
            {
                continue;
            }

            /* Share the home and mark it a copy - do NOT remove the
             * instruction; it is what tells liveness the local is read
             * here, and without it the store filling that home looks
             * dead. */
            dst->loc.kind   = IR_LOC_REG;
            dst->loc.as.reg = src->pinned_reg;
            I->flags |= IRCG_WIDEN_IS_COPY;
            changed = 1;
        }
    }
    setRelease(call_args);
    return changed;
}

/* `trunc iM <- (trunc iN <- x)` with M <= N is `trunc iM <- x`.
 *
 * Only the low M bits of the inner result survive the outer
 * truncation, and those are the low M bits of `x` whichever way round
 * it is done - so the inner one computes nothing that is read. Its
 * SIGNEDNESS does not matter either, for the same reason: it only ever
 * set bits at or above M, and the outer extension overwrites all of
 * them.
 *
 * The pair comes from normalising twice. `short s; s += 7;` promotes to
 * int for the arithmetic, so the result is canonicalised to i32, and
 * the assignment back to `s` immediately canonicalises to i16 -
 * `sxtw x0, w0` followed by `sxth x13, w0`.
 *
 * The inner truncation must have no other use, or dropping it would
 * strand them. */
static int irOpReadsTmp(IrInstr *I, u32 var_id);
static int irTmpReadCount(IrFunction *fn, u32 id);

/* How many of `val`'s low bytes does anything actually READ?
 *
 * The op's own destination width is an upper bound, not the answer.
 * C's integer promotions make `short + short` an INT operation, so the
 * add below produces an i32 - but that i32 is immediately truncated
 * back to i16 and nothing else ever looks at it, so only two bytes are
 * demanded:
 *
 *     iadd  %t18 i32, ...
 *     trunc %t20 i16, %t18        <- the only reader
 *
 * Every reader has to be a truncation for this to be worth anything.
 * Any other consumer - a store, a comparison, a call argument - reads
 * the value as it stands, so the answer is the full width and the
 * caller learns nothing. Returns `dflt` in that case, and for a value
 * with no readers at all. */
static u32 irDemandedWidth(IrFunction *fn, IrValue *val, u32 dflt) {
    if (!irIsTmp(val)) return dflt;
    u32 id = irVarId(val);
    u32 demanded = 0;
    int seen = 0;

    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (!irOpReadsTmp(I, id)) continue;
            seen = 1;
            if (I->op != IR_TRUNC || !I->dst) return dflt;
            u32 m = irValueByteSize(I->dst);
            if (m > demanded) demanded = m;
        }
    }
    if (!seen || demanded == 0) return dflt;
    return demanded < dflt ? demanded : dflt;
}

/* An operand of a NARROW op only needs its low bytes.
 *
 * `int add(int a, int b)` lowers to a pair of widening reads, a 64-bit
 * add and a truncation - the widen exists because a narrow value in a
 * register is otherwise kept sign-extended, and arithmetic on it has to
 * see the whole thing. But an op the backend will emit in its 32-bit
 * form (IRCG_OP_NARROW - see x86_64OpIs32Bit / a64OpIs32Bit) reads only
 * the low 32 bits of its operands, and those are the SAME bits before
 * and after a widening extend. So the extend is dead work, and the op
 * can read the extend's source directly.
 *
 * Sound only while the source is at least as wide as what is DEMANDED
 * of the result: `sext i64 <- i8` feeding an add whose low 32 bits are
 * read really does change those bits, and dropping it there would lose
 * the 8->32 sign extension.
 *
 * Demanded, not the op's own width, because C's integer promotions
 * make `short + short` an INT operation - the add produces an i32 that
 * is immediately truncated back to i16. Only two bytes are observed,
 * and two bytes is exactly what the i16 operands already carry, so the
 * widen either side of it is dead work. Using the op's width instead
 * refuses that case (2 < 4) and leaves `short` and `char` arithmetic
 * paying for a widen nobody reads.
 *
 * The extend itself is left alone - it may have other readers - and
 * simply becomes unreferenced when this was its last one, which the
 * dead-code sweep then collects. */

/* The rewrite moves the READ of `src` from the extend's position down
 * to the consumer's. A source homed in a backend scratch register - a
 * parameter still in its arrival x0, say - only survives that journey
 * if nothing in between writes the register: the same question
 * irForwardStoreToReads asks one instruction at a time, asked here
 * across the whole distance. Without it, `a + (int)c` substituted %p1
 * into the add PAST the fptosi, whose conversion goes through scratch
 * x0, and the add summed the conversion result with itself.
 *
 * `at` is the consumer's own hazard: materialising r1 has already
 * written scratch 0 by the time r2 is read. A def in another block
 * never reaches the consumer's block here and falls out as a refusal,
 * which is the conservative side. */
static int irNarrowSrcSurvivesTo(IrBlock *bb, IrInstr *ext, IrInstr *I,
                                 IrValue *src, IrClobberPoint at,
                                 IrInstr *skip)
{
    if (!irLocIsScratchClobbered(src)) return 1;
    int seen_ext = 0;
    listForEach(bb->instructions) {
        IrInstr *J = (IrInstr *)it->value;
        if (J == ext) { seen_ext = 1; continue; }
        if (J == I) {
            if (!seen_ext) return 0;
            return !irLocClobberedBy(I, src, at);
        }
        if (!seen_ext || J->op == IR_NOP) continue;
        /* `skip` is the op's OTHER feeding extend, known single-use and
         * being substituted in the same batch - it dies with this one,
         * so its emission never happens and cannot clobber anything.
         * This is what lets BOTH of `int add(int a, int b)`'s widens
         * disappear instead of the second extend pinning the first
         * parameter's register. */
        if (J == skip) continue;
        if (irLocClobberedBy(J, src, IR_CLOBBER_AFTER)) return 0;
    }
    return 0;
}

static int irNarrowOpOperands(IrFunction *fn) {
    int changed = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (!(I->flags & IRCG_OP_NARROW) || !I->dst) continue;
            u32 w = irValueByteSize(I->dst);
            if (w == 0 || w > 4) continue;
            /* What the readers actually take, capped at the op's own
             * width - the backend computes no more than that. */
            u32 need = irDemandedWidth(fn, I->dst, w);

            IrValue **slots[2] = { &I->r1, &I->r2 };
            IrInstr *exts[2] = { NULL, NULL };
            int solo[2] = { 0, 0 };   /* ext dst has no other reader */
            for (int k = 0; k < 2; ++k) {
                IrValue *v = *slots[k];
                if (!v || !irIsTmp(v)) continue;
                IrInstr *ext = irFindDefOf(fn, irVarId(v), IR_SEXT);
                if (!ext) ext = irFindDefOf(fn, irVarId(v), IR_ZEXT);
                if (!ext || !ext->r1) continue;
                if (irValueByteSize(ext->r1) < need) continue;
                if (!irIsInt(ext->r1->type)) continue;
                exts[k] = ext;
                solo[k] = irTmpReadCount(fn, irVarId(ext->dst)) == 1;
            }
            /* Substitutability is mutual: each operand's clobber walk
             * may skip the OTHER extend only while that one is itself
             * being substituted (and dying - single reader). Start
             * optimistic, drop failures, re-check the survivor without
             * its dead companion; two operands need at most two
             * rounds. */
            int sub[2];
            for (int k = 0; k < 2; ++k) {
                IrClobberPoint at = k == 0 ? IR_CLOBBER_BEFORE_R1
                                           : IR_CLOBBER_BEFORE_R2;
                IrInstr *skip =
                    (exts[1 - k] && solo[1 - k]) ? exts[1 - k] : NULL;
                sub[k] = exts[k] &&
                         irNarrowSrcSurvivesTo(bb, exts[k], I,
                                               exts[k]->r1, at, skip);
            }
            for (int k = 0; k < 2; ++k) {
                if (!sub[k] || sub[1 - k]) continue;
                IrClobberPoint at = k == 0 ? IR_CLOBBER_BEFORE_R1
                                           : IR_CLOBBER_BEFORE_R2;
                sub[k] = irNarrowSrcSurvivesTo(bb, exts[k], I,
                                               exts[k]->r1, at, NULL);
            }
            for (int k = 0; k < 2; ++k) {
                if (!sub[k]) continue;
                *slots[k] = exts[k]->r1;
                changed = 1;
            }
        }
    }
    return changed;
}

/* How many instructions read tmp `id` anywhere in the function. */
static int irTmpReadCount(IrFunction *fn, u32 id) {
    int n = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (irOpReadsTmp(I, id)) n++;
        }
    }
    return n;
}

/* Collapse the canonical increment-of-a-local triple:
 *
 *     load  %t,  %L
 *     iadd  %t2, %t, <rhs>
 *     store %L,  %t2
 *
 * into `iadd %L, %L, <rhs>` - one instruction once %L has a register
 * home (`add x13, x13, #1`), where the triple costs a load, an op and
 * a store even when everything is register-resident. The lowering
 * already produces local-destination arithmetic itself (a compound
 * assignment's fadd writes its accumulator local directly), so every
 * consumer downstream handles the rewritten shape.
 *
 * The three instructions must be CONSECUTIVE - nothing can touch %L,
 * %t or memory in between - and both tmps single-use, or a second
 * reader would see a value this fold no longer computes. */
static int irFuseLocalRmw(IrFunction *fn) {
    int changed = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        List *n1 = bb->instructions->next;
        for (; n1 != bb->instructions; n1 = n1->next) {
            IrInstr *ld = (IrInstr *)n1->value;
            if (ld->op != IR_LOAD || !irIsTmp(ld->dst)) continue;
            if (!ld->r1 || ld->r1->kind != IR_VAL_LOCAL) continue;
            List *n2 = n1->next;
            while (n2 != bb->instructions &&
                   ((IrInstr *)n2->value)->op == IR_NOP) n2 = n2->next;
            if (n2 == bb->instructions) continue;
            IrInstr *op = (IrInstr *)n2->value;
            switch (op->op) {
            case IR_IADD: case IR_ISUB: case IR_IMUL:
            case IR_AND:  case IR_OR:   case IR_XOR:
            case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV:
                break;
            default: continue;
            }
            if (!irIsTmp(op->dst)) continue;
            List *n3 = n2->next;
            while (n3 != bb->instructions &&
                   ((IrInstr *)n3->value)->op == IR_NOP) n3 = n3->next;
            if (n3 == bb->instructions) continue;
            IrInstr *st = (IrInstr *)n3->value;
            if (st->op != IR_STORE || !st->dst ||
                st->dst->kind != IR_VAL_LOCAL ||
                irVarId(st->dst) != irVarId(ld->r1))
            {
                continue;
            }
            if (!irIsTmp(st->r1) ||
                irVarId(st->r1) != irDstVarId(op)) continue;
            /* The op must consume the load - either operand side. */
            u32 t = irVarId(ld->dst);
            int r1_is_t = irIsTmp(op->r1) && irVarId(op->r1) == t;
            int r2_is_t = irIsTmp(op->r2) && irVarId(op->r2) == t;
            if (!r1_is_t && !r2_is_t) continue;
            /* Same width and value class either side of the store, or
             * the fold would skip a truncation the store performed. */
            if (irValueByteSize(st->dst) != irValueByteSize(op->dst))
                continue;
            if (irIsInt(st->dst->type) != irIsInt(op->dst->type)) continue;
            /* Narrow INT locals stay on the load/op/store path: a
             * register-homed one is kept sign-extended by the store's
             * canonicalisation (jitStoreNarrowHome), which a local-dst
             * op writing the home via the w-form alu3 would skip. */
            if (irIsInt(st->dst->type) && irValueByteSize(st->dst) != 8)
                continue;
            /* Single-use tmps: the load feeds the op, the op feeds the
             * store, nothing else looks at either. */
            if (irTmpReadCount(fn, t) != 1) continue;
            if (irTmpReadCount(fn, irDstVarId(op)) != 1) continue;

            if (r1_is_t) op->r1 = ld->r1;
            if (r2_is_t) op->r2 = ld->r1;
            op->dst = st->dst;
            ld->op = IR_NOP;
            st->op = IR_NOP;
            changed = 1;
        }
    }
    return changed;
}

static int irFoldChainedTruncate(IrFunction *fn) {
    int changed = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_TRUNC || !I->dst || !I->r1) continue;

            /* Definition of the outer truncation's operand. */
            IrInstr *inner = NULL;
            listForEach(bb->instructions) {
                IrInstr *J = (IrInstr *)it->value;
                if (J == I) break;
                if (J->dst == I->r1) {
                    inner = (J->op == IR_TRUNC) ? J : NULL;
                }
            }
            if (!inner || !inner->r1) continue;
            /* The inner truncation is about to be deleted, so its
             * destination has to be a temporary. A LOCAL destination is
             * an assignment to a variable, and deleting it drops the
             * write - the same hazard as the fold above, where a narrow
             * `x = y` lowers to `trunc <local>, <tmp>`. */
            if (!irIsTmp(inner->dst)) continue;
            if (irValueByteSize(I->dst) > irValueByteSize(inner->dst)) continue;
            if (irReplaceTmpUses(fn, inner->dst, NULL) != 1) continue;

            I->r1 = inner->r1;
            irMakeNop(inner);
            changed = 1;
        }
    }
    return changed;
}

/* Mark sign-extensions a 4-byte load already performed.
 *
 * `int x = p->field;` lowers to `load* %t (i32), ...` then
 * `sext %u (i64), %t` - and the load emitted `ldrsw`, which already
 * produced the 64-bit sign-extended value. The convert is pure
 * overhead.
 *
 * The existing SEXT path in a64EmitConvert already skips the widen when
 * the operand is IN MEMORY, because the load folds it. This is the same
 * observation for an operand the register allocator moved into a
 * register, where the fold was being missed. */
static void irMarkRedundantSext(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_SEXT || !I->r1 || !I->dst) continue;
            if (irValueByteSize(I->r1) != 4) continue;

            /* Producer must be a 4-byte load in this same block - the
             * only shape that guarantees the sign extension happened.
             * Anything defining r1 after that load invalidates it. */
            IrInstr *def = NULL;
            listForEach(bb->instructions) {
                IrInstr *J = (IrInstr *)it->value;
                if (J == I) break;
                if (J->dst == I->r1) def = J;
            }
            if (!def) continue;
            if (def->op != IR_LOAD && def->op != IR_LOAD_DEREF) continue;
            if (irValueByteSize(def->dst) != 4) continue;
            I->flags |= IRCG_SEXT_NOOP;
        }
    }
}

/* A SEXT the pass above proved to be a no-op is BIT-IDENTICAL to its
 * source: the 4-byte load already sign-extended the full register.
 * The flag alone only saved the extension instruction - emission
 * still degraded it to a register-to-register copy, which is where
 * every `ldrsw x0, [..]; mov x13, x0` pair came from. Since source
 * and destination hold the same bits, every reader of the destination
 * can read the source instead and the widen disappears entirely.
 * The source is SSA (one def, never re-stored), so the substitution
 * needs no liveness argument beyond the flag's own guarantee. */
/* Does anything read `v` in a way that depends on its declared WIDTH
 * rather than on the bits in the register?
 *
 * A store does: the backends size the memory access from the VALUE's
 * width, because the destination of a deref store is only an address
 * and carries no type. So substituting a 4-byte source for its 8-byte
 * sign-extended result - bit-identical in the register - silently
 * turns `str x0` into `str w0`, and the upper four bytes of the
 * destination keep whatever they held:
 *
 *     le->i64 = type->kind;      // int field -> s64 field
 *     ldrsw x0, [x20, #8]        // correct, x0 = 95
 *     str   w0, [x19, #8]        // writes 4 bytes, not 8
 *
 * That is how a keyword id came back as (1 << 32) | 95 and the lexer
 * stopped recognising `long`. */
static int irValueWidthIsObserved(IrFunction *fn, IrValue *v) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->r1 != v) continue;
            if (I->op == IR_STORE || I->op == IR_STORE_DEREF) return 1;
        }
    }
    return 0;
}

static int irCoalesceNoopWidens(IrFunction *fn) {
    int changed = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_SEXT || !(I->flags & IRCG_SEXT_NOOP)) continue;
            if (!irIsTmp(I->dst) || !irIsTmp(I->r1)) continue;
            /* The widen is only free where the WIDTH does not matter. */
            if (irValueByteSize(I->dst) != irValueByteSize(I->r1) &&
                irValueWidthIsObserved(fn, I->dst))
            {
                continue;
            }
            if (irReplaceTmpUses(fn, I->dst, NULL) > 0)
                irReplaceTmpUses(fn, I->dst, I->r1);
            I->op = IR_NOP;
            I->dst = I->r1 = I->r2 = NULL;
            changed++;
        }
    }
    return changed;
}

/* ---- Devirtualising calls ------------------------------------------
 *
 * An indirect call whose target is loaded from a slot that only ever
 * holds ONE known function becomes a direct call to it:
 *
 *     int (*cmp)(int) = twice;        cmp(21)   ->  bl _twice
 *     auto f = [&x]() { ... };        f()       ->  bl _lambda$0
 *
 * A direct call is cheaper on its own, and it is what the inliner can
 * see through - which is the point for a closure, whose body is
 * usually a line or two.
 *
 * A slot is a local, or a constant offset inside one, reached through
 * `gep` / `lea` / `iadd <const>`. It qualifies when every write to it
 * stores the same non-variadic function, nothing else writes over it,
 * and no pointer to it escapes. An escaping pointer to offset `c` is
 * taken to reach everything from `c` on - a pointer to a struct member
 * cannot be walked back to an earlier member - which is what lets a
 * closure keep its function: the call hands the callee `&f.data`, past
 * `f.fn`. A read that comes before the only write is undefined, so it
 * need not stop anything. */

typedef struct IrDvPtr {
    u32 local;
    s64 off;
} IrDvPtr;

typedef struct IrDvWrite {
    u32 local;
    s64 off;
    IrValue *fn;     /* the function stored, or NULL for anything else */
} IrDvWrite;

typedef struct IrDv {
    Map *derived;    /* tmp key -> IrDvPtr * */
    Map *defs;       /* tmp key -> IrInstr *, or IR_DV_MULTI */
    IrDvWrite *writes;
    u64 n_writes, cap_writes;
    IrDvPtr *escapes;   /* off == INT64_MIN: the whole local */
    u64 n_escapes, cap_escapes;
} IrDv;

#define IR_DV_MULTI ((IrInstr *)1)

/* Does `op` DEFINE its dst? Everywhere else dst is an operand being
 * read: a store's target, a `ret`'s value, a branch's condition. */
static int irDvDstIsDef(IrOp op) {
    switch (op) {
        case IR_ALLOCA: case IR_LOAD: case IR_LOAD_DEREF: case IR_LEA:
        case IR_GEP:
        case IR_IADD: case IR_ISUB: case IR_IMUL: case IR_IDIV:
        case IR_UDIV: case IR_IREM: case IR_UREM: case IR_INEG:
        case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV:
        case IR_FNEG:
        case IR_AND: case IR_OR: case IR_XOR: case IR_SHL: case IR_SHR:
        case IR_SAR: case IR_NOT:
        case IR_ICMP: case IR_FCMP:
        case IR_TRUNC: case IR_ZEXT: case IR_SEXT: case IR_FPTRUNC:
        case IR_FPEXT: case IR_FPTOUI: case IR_FPTOSI: case IR_UITOFP:
        case IR_SITOFP: case IR_PTRTOINT: case IR_INTTOPTR: case IR_BITCAST:
        case IR_CALL: case IR_PHI: case IR_SELECT: case IR_VA_ARG:
        case IR_FRAME_ADDR:
            return 1;
        default:
            return 0;
    }
}

static u64 irDvKey(IrValue *v) {
    return ((u64)v->kind << 32) | (u64)irVarId(v);
}

/* The slot `v` names - a local itself, or a pointer derived from one. */
static int irDvSlotOf(IrDv *dv, IrValue *v, u32 *local, s64 *off) {
    if (!v) return 0;
    if (v->kind == IR_VAL_LOCAL && !v->pinned_reg) {
        *local = irVarId(v);
        *off = 0;
        return 1;
    }
    if (v->kind != IR_VAL_TMP) return 0;
    IrDvPtr *p = (IrDvPtr *)mapGet(dv->derived, (void *)irDvKey(v));
    if (!p) return 0;
    *local = p->local;
    *off = p->off;
    return 1;
}

/* The slot a POINTER VALUE points at - the address operand of a
 * `*_DEREF` op. Only a derived pointer qualifies: a local there is not
 * the slot, it is a variable HOLDING some pointer. */
static int irDvSlotAt(IrDv *dv, IrValue *v, u32 *local, s64 *off) {
    if (!v || v->kind != IR_VAL_TMP) return 0;
    return irDvSlotOf(dv, v, local, off);
}

static int irDvIsDerivedTmp(IrDv *dv, IrValue *v) {
    return v && v->kind == IR_VAL_TMP &&
           mapHas(dv->derived, (void *)irDvKey(v));
}

/* The function `v` is the address of, if it is one. */
static IrValue *irDvFunction(IrDv *dv, IrValue *v) {
    if (!v) return NULL;
    if (v->kind == IR_VAL_GLOBAL && (v->flags & IR_VAL_FLAG_FUNC)) return v;
    if (v->kind != IR_VAL_TMP) return NULL;
    IrInstr *def = (IrInstr *)mapGet(dv->defs, (void *)irDvKey(v));
    if (!def || def == IR_DV_MULTI || def->op != IR_LEA) return NULL;
    IrValue *g = def->r1;
    if (g && g->kind == IR_VAL_GLOBAL && (g->flags & IR_VAL_FLAG_FUNC)) {
        return g;
    }
    return NULL;
}

static void irDvWrite(IrDv *dv, u32 local, s64 off, IrValue *fn) {
    if (dv->n_writes == dv->cap_writes) {
        dv->cap_writes = dv->cap_writes ? dv->cap_writes * 2 : 16;
        dv->writes = (IrDvWrite *)realloc(dv->writes,
                dv->cap_writes * sizeof(IrDvWrite));
    }
    dv->writes[dv->n_writes++] = (IrDvWrite){ local, off, fn };
}

static void irDvEscape(IrDv *dv, u32 local, s64 off) {
    if (dv->n_escapes == dv->cap_escapes) {
        dv->cap_escapes = dv->cap_escapes ? dv->cap_escapes * 2 : 16;
        dv->escapes = (IrDvPtr *)realloc(dv->escapes,
                dv->cap_escapes * sizeof(IrDvPtr));
    }
    dv->escapes[dv->n_escapes++] = (IrDvPtr){ local, off };
}

/* `v` used somewhere that is not a plain load or store through it - an
 * argument, a value stored away, an operand of arithmetic. */
static void irDvUse(IrDv *dv, IrValue *v) {
    u32 local;
    s64 off;
    if (irDvIsDerivedTmp(dv, v) && irDvSlotOf(dv, v, &local, &off)) {
        irDvEscape(dv, local, off);
    }
}

/* A store whose value is an aggregate writes more than a word. */
static int irDvWideValue(IrValue *v) {
    return v && (v->type == IR_TYPE_STRUCT || v->type == IR_TYPE_ARRAY ||
                 v->type == IR_TYPE_ARRAY_INIT);
}

/* The function a load from (`local`, `off`) must produce, or NULL. */
static IrValue *irDvSlotFunction(IrDv *dv, u32 local, s64 off) {
    IrValue *fn = NULL;
    for (u64 i = 0; i < dv->n_escapes; ++i) {
        if (dv->escapes[i].local == local && dv->escapes[i].off <= off) {
            return NULL;
        }
    }
    for (u64 i = 0; i < dv->n_writes; ++i) {
        IrDvWrite *w = &dv->writes[i];
        if (w->local != local) continue;
        if (w->off == INT64_MIN) return NULL;
        if (w->off != off) {
            /* Word-sized writes: anything starting within a word of the
             * slot overlaps it. */
            if (w->off < off + 8 && off < w->off + 8) return NULL;
            continue;
        }
        if (!w->fn) return NULL;
        if (fn && !aoStrEq(fn->as.global.name, w->fn->as.global.name)) {
            return NULL;
        }
        fn = w->fn;
    }
    if (fn && (fn->flags & IR_VAL_FLAG_VARIADIC_FUNC)) return NULL;
    return fn;
}

static int g_devirtualise = 0;

void irOptSetDevirtualise(int enabled) {
    g_devirtualise = enabled;
}

static void irDvRelease(IrDv *dv) {
    MapIter mi;
    mapIterInit(dv->derived, &mi);
    while (mapIterNext(&mi)) free(mi.node->value);
    mapRelease(dv->derived);
    mapRelease(dv->defs);
    free(dv->writes);
    free(dv->escapes);
}

/* Fill `dv` for `fn`: every tmp's definition, every pointer derived
 * from a local, every write to a local's slots and every escape of
 * one. Returns 0, with nothing to release, when the function has
 * inline asm - which can write any local it names - or when
 * `need_indirect` and there is no indirect call to look at. */
static int irDvAnalyse(IrFunction *fn, IrDv *dv_out, int need_indirect) {
    IrDv dv;
    memset(&dv, 0, sizeof(IrDv));
    IrDv *dv_ref = &dv;
    int has_indirect = 0;

    /* Definitions, and whether there is anything to do at all. */
    dv.defs = mapNew(64, &map_uint_to_uint_type);
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            /* Inline asm can write any local it names. */
            if (I->op == IR_ASM) {
                mapRelease(dv.defs);
                return 0;
            }
            if (I->op == IR_CALL && I->r2 &&
                (!I->r1 || !I->r1->as.array.label))
            {
                has_indirect = 1;
            }
            if (I->dst && I->dst->kind == IR_VAL_TMP &&
                I->op != IR_STORE && I->op != IR_STORE_DEREF &&
                I->op != IR_RMW_DEREF)
            {
                void *k = (void *)irDvKey(I->dst);
                mapAdd(dv.defs, k, mapHas(dv.defs, k) ? IR_DV_MULTI : I);
            }
        }
    }
    if (need_indirect && !has_indirect) {
        mapRelease(dv.defs);
        return 0;
    }

    /* Pointers derived from locals: `gep l, c`, `lea l`, `lea <gep>`,
     * `iadd <ptr>, c`. Chains are resolved by going round until nothing
     * new turns up. */
    dv.derived = mapNew(64, &map_uint_to_uint_type);
    for (int grew = 1; grew; ) {
        grew = 0;
        listForEach(fn->blocks) {
            IrBlock *bb = (IrBlock *)it->value;
            listForEach(bb->instructions) {
                IrInstr *I = (IrInstr *)it->value;
                if (!I->dst || I->dst->kind != IR_VAL_TMP) continue;
                void *k = (void *)irDvKey(I->dst);
                if (mapHas(dv.derived, k)) continue;
                if (mapGet(dv.defs, k) != I) continue;   /* one def only */
                u32 local;
                s64 off, add = 0;
                IrValue *base = NULL;
                if (I->op == IR_GEP) {
                    if (!I->r2 || I->r2->kind != IR_VAL_CONST_INT) continue;
                    base = I->r1;
                    add = I->r2->as._i64;
                } else if (I->op == IR_LEA) {
                    base = I->r1;
                } else if (I->op == IR_IADD || I->op == IR_ISUB) {
                    if (I->r2 && I->r2->kind == IR_VAL_CONST_INT &&
                        irDvIsDerivedTmp(dv_ref, I->r1))
                    {
                        base = I->r1;
                        add = I->r2->as._i64;
                    } else if (I->op == IR_IADD && I->r1 &&
                               I->r1->kind == IR_VAL_CONST_INT &&
                               irDvIsDerivedTmp(dv_ref, I->r2))
                    {
                        base = I->r2;
                        add = I->r1->as._i64;
                    }
                    if (I->op == IR_ISUB) add = -add;
                }
                if (!base || !irDvSlotOf(dv_ref, base, &local, &off)) continue;
                IrDvPtr *p = (IrDvPtr *)malloc(sizeof(IrDvPtr));
                p->local = local;
                p->off = off + add;
                mapAdd(dv.derived, k, p);
                grew = 1;
            }
        }
    }

    /* Every write and every escape. */
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            u32 local;
            s64 off;
            switch (I->op) {
                case IR_NOP:
                    break;
                case IR_STORE:
                    if (irDvSlotOf(dv_ref, I->dst, &local, &off)) {
                        irDvWrite(dv_ref, local,
                                  irDvWideValue(I->r1) ? INT64_MIN : off,
                                  irDvFunction(dv_ref, I->r1));
                    }
                    irDvUse(dv_ref, I->r1);
                    break;
                case IR_STORE_DEREF:
                case IR_RMW_DEREF:
                    if (irDvSlotAt(dv_ref, I->dst, &local, &off)) {
                        int unknown = I->idx || irDvWideValue(I->r1);
                        irDvWrite(dv_ref, local,
                                  unknown ? INT64_MIN : off + I->disp,
                                  I->op == IR_STORE_DEREF
                                      ? irDvFunction(dv_ref, I->r1) : NULL);
                    } else {
                        irDvUse(dv_ref, I->dst);
                    }
                    irDvUse(dv_ref, I->r1);
                    irDvUse(dv_ref, I->idx);
                    break;
                case IR_LOAD:
                case IR_LOAD_DEREF:
                    /* Reading through the address is no escape. */
                    irDvUse(dv_ref, I->idx);
                    break;
                case IR_GEP:
                case IR_LEA:
                case IR_IADD:
                case IR_ISUB:
                    /* Deriving a tracked pointer is no escape either -
                     * its own uses are what matter. */
                    if (I->dst && irDvIsDerivedTmp(dv_ref, I->dst)) break;
                    /* `lea %local, <function>` - a function pointer
                     * local initialised with an address. */
                    if (I->op == IR_LEA && I->dst &&
                        I->dst->kind == IR_VAL_LOCAL &&
                        irDvFunction(dv_ref, I->r1) &&
                        irDvSlotOf(dv_ref, I->dst, &local, &off))
                    {
                        irDvWrite(dv_ref, local, 0, irDvFunction(dv_ref, I->r1));
                        break;
                    }
                    /* An address taken and not tracked - stored in a
                     * local (`pp = &f`), or derived in a way this does
                     * not follow - lets anything write the slot. */
                    if ((I->op == IR_LEA || I->op == IR_GEP) && I->r1 &&
                        I->r1->kind == IR_VAL_LOCAL &&
                        irDvSlotOf(dv_ref, I->r1, &local, &off))
                    {
                        s64 at = 0;
                        if (I->op == IR_GEP && I->r2 &&
                            I->r2->kind == IR_VAL_CONST_INT)
                        {
                            at = I->r2->as._i64;
                        } else if (I->op == IR_GEP) {
                            at = INT64_MIN;
                        }
                        irDvEscape(dv_ref, local, at);
                    }
                    irDvUse(dv_ref, I->r1);
                    irDvUse(dv_ref, I->r2);
                    break;
                case IR_CALL:
                    if (I->r1 && I->r1->as.array.values) {
                        Vec *args = I->r1->as.array.values;
                        for (u64 a = 0; a < args->size; ++a) {
                            irDvUse(dv_ref, (IrValue *)args->entries[a]);
                        }
                    }
                    break;
                case IR_PHI:
                    if (I->extra.phi_pairs) {
                        for (u64 p = 0; p < I->extra.phi_pairs->size; ++p) {
                            IrPair *pr = vecGet(IrPair *, I->extra.phi_pairs, p);
                            if (pr) irDvUse(dv_ref, pr->ir_value);
                        }
                    }
                    break;
                default:
                    if (!irDvDstIsDef(I->op)) irDvUse(dv_ref, I->dst);
                    irDvUse(dv_ref, I->r1);
                    irDvUse(dv_ref, I->r2);
                    irDvUse(dv_ref, I->idx);
                    break;
            }
            /* Any other definition of a local overwrites it. */
            if (I->dst && I->dst->kind == IR_VAL_LOCAL &&
                irDvDstIsDef(I->op) && I->op != IR_LEA &&
                irDvSlotOf(dv_ref, I->dst, &local, &off))
            {
                irDvWrite(dv_ref, local, INT64_MIN, NULL);
            }
        }
    }
    *dv_out = dv;
    return 1;
}

int irDevirtualiseCalls(IrFunction *fn) {
    if (!g_devirtualise) return 0;
    IrDv dv;
    if (!irDvAnalyse(fn, &dv, 1)) return 0;

    /* Rewrite the calls whose target is pinned down. */
    int changed = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_CALL || !I->r2 || !I->r1 ||
                I->r1->as.array.label)
            {
                continue;
            }
            u32 local;
            s64 off;
            IrValue *target = I->r2;
            int found = 0;
            if (target->kind == IR_VAL_LOCAL) {
                found = irDvSlotOf(&dv, target, &local, &off);
            } else if (target->kind == IR_VAL_TMP) {
                IrInstr *def = (IrInstr *)mapGet(dv.defs,
                                                 (void *)irDvKey(target));
                if (def && def != IR_DV_MULTI && !def->idx &&
                    ((def->op == IR_LOAD &&
                      irDvSlotOf(&dv, def->r1, &local, &off)) ||
                     (def->op == IR_LOAD_DEREF &&
                      irDvSlotAt(&dv, def->r1, &local, &off))))
                {
                    off += def->op == IR_LOAD_DEREF ? def->disp : 0;
                    found = 1;
                }
            }
            if (!found) continue;
            IrValue *callee = irDvSlotFunction(&dv, local, off);
            if (!callee) continue;
            I->r1->as.array.label = callee->as.global.name;
            I->r2 = NULL;
            changed++;
        }
    }

    irDvRelease(&dv);
    return changed;
}

/* ---- Local memory (`-O`) --------------------------------------------
 *
 * Three rewrites of locals that live in memory, run to a fixed point:
 *
 *   forward  a load from a slot takes the value last stored to it, or
 *            what an identical earlier load produced, within a block;
 *   direct   `*&x` - a load or store through the address of a scalar
 *            local - becomes a plain load or store of `x`, so taking
 *            the address stops being what keeps `x` in memory;
 *   dead     stores to a local nothing reads and nobody else can reach
 *            are dropped.
 *
 * A local whose address never escapes cannot be touched by a store
 * through an unknown pointer or by a call, which is what makes the
 * first and last safe. Same analysis as devirtualisation (IrDv).
 *
 * After inlining a closure this is most of what is left: its state was
 * written into the closure only to be read straight back out. */

typedef struct IrLmEntry {
    u32 local;
    s64 off;
    u32 size;
    IrValue *val;
} IrLmEntry;

static int irLmEscaped(IrDv *dv, u32 local) {
    for (u64 i = 0; i < dv->n_escapes; ++i) {
        if (dv->escapes[i].local == local) return 1;
    }
    return 0;
}

static int irLmSame(IrValue *a, IrValue *b) {
    return a && b && a->kind == b->kind && irVarId(a) == irVarId(b);
}

/* Every use of tmp `from` now reads `to`. */
static void irLmSubstitute(IrFunction *fn, IrValue *from, IrValue *to) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (irLmSame(I->dst, from)) I->dst = to;
            if (irLmSame(I->r1, from)) I->r1 = to;
            if (irLmSame(I->r2, from)) I->r2 = to;
            if (irLmSame(I->idx, from)) I->idx = to;
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 k = 0; k < args->size; ++k) {
                    if (irLmSame(args->entries[k], from)) args->entries[k] = to;
                }
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                Vec *pairs = I->extra.phi_pairs;
                for (u64 k = 0; k < pairs->size; ++k) {
                    IrPair *pr = (IrPair *)pairs->entries[k];
                    if (pr && irLmSame(pr->ir_value, from)) pr->ir_value = to;
                }
            }
        }
    }
}

/* A value that means the same thing wherever it is read later in the
 * block: a constant, a global's address, or a tmp defined once. */
static int irLmStable(IrDv *dv, IrValue *v) {
    if (!v) return 0;
    if (v->kind == IR_VAL_CONST_INT || v->kind == IR_VAL_GLOBAL) return 1;
    if (v->kind != IR_VAL_TMP) return 0;
    IrInstr *def = (IrInstr *)mapGet(dv->defs, (void *)irDvKey(v));
    return def && def != IR_DV_MULTI;
}

/* The slot a memory op reads or writes, when it is one of ours. */
static int irLmSlot(IrDv *dv, IrInstr *I, IrValue *addr, u32 *local,
                    s64 *off)
{
    if (I->idx) return 0;
    if (I->op == IR_LOAD || I->op == IR_STORE) {
        return irDvSlotOf(dv, addr, local, off);
    }
    if (!irDvSlotAt(dv, addr, local, off)) return 0;
    *off += I->disp;
    return 1;
}

static int irLmForward(IrFunction *fn, IrDv *dv) {
    int changed = 0;
    IrLmEntry *tab = NULL;
    u64 n = 0, cap = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        n = 0;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            u32 local;
            s64 off;
            if (I->op == IR_NOP) continue;

            if ((I->op == IR_LOAD || I->op == IR_LOAD_DEREF) &&
                irLmSlot(dv, I, I->r1, &local, &off))
            {
                u32 size = irValueByteSize(I->dst);
                IrLmEntry *hit = NULL;
                for (u64 k = 0; k < n; ++k) {
                    if (tab[k].local == local && tab[k].off == off &&
                        tab[k].size == size &&
                        tab[k].val->type == I->dst->type)
                    {
                        hit = &tab[k];
                    }
                }
                /* A constant stands in for a pointer badly: it would end
                 * up in address positions. */
                if (hit && !(hit->val->kind == IR_VAL_CONST_INT &&
                             I->dst->type == IR_TYPE_PTR))
                {
                    IrValue *from = I->dst;
                    irMakeNop(I);
                    irLmSubstitute(fn, from, hit->val);
                    changed = 1;
                    continue;
                }
                /* Remember the load itself: the next identical one can
                 * reuse it. */
                if (irLmStable(dv, I->dst)) {
                    if (n == cap) {
                        cap = cap ? cap * 2 : 16;
                        tab = (IrLmEntry *)realloc(tab, cap * sizeof(IrLmEntry));
                    }
                    tab[n++] = (IrLmEntry){ local, off, size, I->dst };
                }
                continue;
            }

            int is_store = I->op == IR_STORE || I->op == IR_STORE_DEREF;
            if ((is_store || I->op == IR_RMW_DEREF) &&
                irLmSlot(dv, I, I->dst, &local, &off))
            {
                u32 size = irValueByteSize(I->r1);
                /* Drop whatever this overwrites. */
                for (u64 k = 0; k < n; ) {
                    if (tab[k].local == local &&
                        tab[k].off < off + (s64)size &&
                        off < tab[k].off + (s64)tab[k].size)
                    {
                        tab[k] = tab[--n];
                    } else {
                        ++k;
                    }
                }
                if (is_store && irLmStable(dv, I->r1)) {
                    if (n == cap) {
                        cap = cap ? cap * 2 : 16;
                        tab = (IrLmEntry *)realloc(tab, cap * sizeof(IrLmEntry));
                    }
                    tab[n++] = (IrLmEntry){ local, off, size, I->r1 };
                }
                continue;
            }

            /* Anything else that can write memory reaches only locals
             * whose address got out. */
            if (I->op == IR_STORE_DEREF || I->op == IR_RMW_DEREF ||
                I->op == IR_CALL || I->op == IR_VA_START ||
                I->op == IR_VA_ARG)
            {
                for (u64 k = 0; k < n; ) {
                    if (irLmEscaped(dv, tab[k].local)) tab[k] = tab[--n];
                    else ++k;
                }
            }
            /* A local defined some other way. */
            if (I->dst && I->dst->kind == IR_VAL_LOCAL) {
                for (u64 k = 0; k < n; ) {
                    if (tab[k].local == irVarId(I->dst)) tab[k] = tab[--n];
                    else ++k;
                }
            }
        }
    }
    free(tab);
    return changed;
}

/* `load* %t, &x` / `store* &x, %v` -> a plain access to scalar `x`. */
static int irLmDirect(IrFunction *fn, IrDv *dv, Map *locals) {
    int changed = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_LOAD_DEREF && I->op != IR_STORE_DEREF) continue;
            if (I->idx || I->disp) continue;
            IrValue *addr = I->op == IR_LOAD_DEREF ? I->r1 : I->dst;
            u32 local;
            s64 off;
            if (!irDvSlotAt(dv, addr, &local, &off) || off != 0) continue;
            IrValue *lv = (IrValue *)mapGet(locals, (void *)(u64)local);
            if (!lv || lv->type == IR_TYPE_STRUCT || lv->type == IR_TYPE_ARRAY) {
                continue;
            }
            IrValue *val = I->op == IR_LOAD_DEREF ? I->dst : I->r1;
            if (irValueByteSize(val) != irValueByteSize(lv)) continue;
            if (I->op == IR_LOAD_DEREF) {
                I->op = IR_LOAD;
                I->r1 = lv;
            } else {
                I->op = IR_STORE;
                I->dst = lv;
            }
            changed = 1;
        }
    }
    return changed;
}

/* Is tmp `v` read by anything? */
static int irLmUsed(Map *uses, IrValue *v) {
    return v && mapHas(uses, (void *)irDvKey(v));
}

static void irLmCountUse(Map *uses, IrValue *v) {
    if (v && (v->kind == IR_VAL_TMP || v->kind == IR_VAL_LOCAL)) {
        mapAdd(uses, (void *)irDvKey(v), (void *)1);
    }
}

static int irLmDead(IrFunction *fn, IrDv *dv) {
    /* Who reads what. A local counts as read when anything takes its
     * VALUE, or a live load reads one of its slots. */
    Map *uses = mapNew(64, &map_uint_to_uint_type);
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (!irDvDstIsDef(I->op)) irLmCountUse(uses, I->dst);
            irLmCountUse(uses, I->r1);
            irLmCountUse(uses, I->r2);
            irLmCountUse(uses, I->idx);
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 k = 0; k < args->size; ++k) {
                    irLmCountUse(uses, args->entries[k]);
                }
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                Vec *pairs = I->extra.phi_pairs;
                for (u64 k = 0; k < pairs->size; ++k) {
                    IrPair *pr = (IrPair *)pairs->entries[k];
                    if (pr) irLmCountUse(uses, pr->ir_value);
                }
            }
        }
    }

    Set *read = setNew(16, &set_uint_type);
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            u32 local;
            s64 off;
            if (I->op == IR_NOP) continue;
            if ((I->op == IR_LOAD || I->op == IR_LOAD_DEREF ||
                 I->op == IR_RMW_DEREF) &&
                irLmUsed(uses, I->dst) | (I->op == IR_RMW_DEREF))
            {
                IrValue *addr = I->op == IR_RMW_DEREF ? I->dst : I->r1;
                if (I->op == IR_LOAD ? irDvSlotOf(dv, addr, &local, &off)
                                     : irDvSlotAt(dv, addr, &local, &off))
                {
                    setAdd(read, (void *)(u64)local);
                }
            }
            /* A local's value used directly. Address positions - a
             * store's target, a load's source, `lea`/`gep` of it - are
             * not reads of the value. */
            IrValue *ops[3] = { I->r1, I->r2, I->idx };
            for (int k = 0; k < 3; ++k) {
                IrValue *v = ops[k];
                if (!v || v->kind != IR_VAL_LOCAL) continue;
                if (k == 0 && (I->op == IR_LOAD || I->op == IR_LEA ||
                               I->op == IR_GEP)) continue;
                setAdd(read, (void *)(u64)irVarId(v));
            }
            /* `ret %l`, `br %l`, and the pointer a `store*` writes
             * through: the dst is read. A plain store's is the slot. */
            if (I->dst && I->dst->kind == IR_VAL_LOCAL &&
                !irDvDstIsDef(I->op) && I->op != IR_STORE)
            {
                setAdd(read, (void *)(u64)irVarId(I->dst));
            }
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 k = 0; k < args->size; ++k) {
                    IrValue *v = args->entries[k];
                    if (v && v->kind == IR_VAL_LOCAL) {
                        setAdd(read, (void *)(u64)irVarId(v));
                    }
                }
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                Vec *pairs = I->extra.phi_pairs;
                for (u64 k = 0; k < pairs->size; ++k) {
                    IrPair *pr = (IrPair *)pairs->entries[k];
                    if (pr && pr->ir_value && pr->ir_value->kind == IR_VAL_LOCAL) {
                        setAdd(read, (void *)(u64)irVarId(pr->ir_value));
                    }
                }
            }
        }
    }

    int changed = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            u32 local;
            s64 off;
            int hit = 0;
            switch (I->op) {
                case IR_STORE:
                    hit = !I->idx && irDvSlotOf(dv, I->dst, &local, &off);
                    break;
                case IR_STORE_DEREF:
                case IR_RMW_DEREF:
                    hit = irDvSlotAt(dv, I->dst, &local, &off);
                    break;
                case IR_LOAD:
                    hit = !irLmUsed(uses, I->dst) &&
                          irDvSlotOf(dv, I->r1, &local, &off);
                    break;
                case IR_LOAD_DEREF:
                    hit = !irLmUsed(uses, I->dst) &&
                          irDvSlotAt(dv, I->r1, &local, &off);
                    break;
                case IR_LEA:
                    /* `lea %local, <function>` writes the local. */
                    hit = I->dst && I->dst->kind == IR_VAL_LOCAL &&
                          irDvSlotOf(dv, I->dst, &local, &off);
                    break;
                default:
                    break;
            }
            if (!hit) continue;
            if (setHas(read, (void *)(u64)local)) continue;
            if (irLmEscaped(dv, local)) continue;
            irMakeNop(I);
            changed = 1;
        }
    }
    setRelease(read);
    mapRelease(uses);
    return changed;
}

/* setjmp returns twice, and the second time the values forwarding put
 * in temporaries are whatever the registers held at the first. */
static int irLmCallsSetjmp(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_CALL || !I->r1 || !I->r1->as.array.label) continue;
            char *name = I->r1->as.array.label->data;
            if (!strcmp(name, "setjmp") || !strcmp(name, "_setjmp") ||
                !strcmp(name, "sigsetjmp") || !strcmp(name, "__sigsetjmp"))
            {
                return 1;
            }
        }
    }
    return 0;
}

int irOptLocalMemory(IrFunction *fn) {
    if (!g_devirtualise) return 0;
    if (irLmCallsSetjmp(fn)) return 0;
    int total = 0;
    for (int round = 0; round < 8; ++round) {
        int changed = 0;
        for (int phase = 0; phase < 3; ++phase) {
            IrDv dv;
            if (!irDvAnalyse(fn, &dv, 0)) return total;
            /* The function's OWN IrValue for each local: the register
             * allocator places values by object, not by id, so a load
             * built on some other object naming the same local reads
             * a home that was never assigned. */
            Map *locals = mapNew(32, &map_uint_to_uint_type);
            if (fn->variables) {
                MapIter vi;
                mapIterInit(fn->variables, &vi);
                while (mapIterNext(&vi)) {
                    IrValue *v = (IrValue *)vi.node->value;
                    if (v && v->kind == IR_VAL_LOCAL) {
                        mapAdd(locals, (void *)(u64)irVarId(v), v);
                    }
                }
            }
            int c = phase == 0 ? irLmForward(fn, &dv)
                  : phase == 1 ? irLmDirect(fn, &dv, locals)
                               : irLmDead(fn, &dv);
            mapRelease(locals);
            irDvRelease(&dv);
            changed |= c;
        }
        if (!changed) break;
        total = 1;
    }
    return total;
}

void irBasicFunctionOptimisations(IrFunction *fn) {
    irRemoveAllNops(fn);
    /* Before anything reasons about operand positions. */
    irCanonicaliseCmpOperands(fn);
    /* First, while loads and stores are still in the shape lowering
     * left them: the fusion passes below fold addresses into them. And
     * before the inliner, which is what a direct call is for. */
    irDevirtualiseCalls(fn);
    irRemoveRedundantBlocks(fn);
    irFoldPassThroughBlocks(fn);
    irRemoveRedundantBlocks(fn);
    irFoldGlobalDeref(fn);
    irForwardReturnSlot(fn);
    irPromoteSlotToPhi(fn);
    /* Constant folding and store->read forwarding feed each other: a
     * forwarded `store %slot, <const>` turns a downstream `iadd %t,
     * %slot, %slot2` into a const+const the folder can collapse, and
     * the folded result is itself a const store the next forwarding
     * sweep can propagate. Run them to a fixed point so `z = x + y`
     * over constants reduces all the way to a single value. Each pass
     * only ever removes/narrows work, so this terminates. */
    int folded;
    do {
        folded  = irEvalConstantExpressions(fn);
        folded += irForwardStoreToReads(fn);
    } while (folded);
    /* `-O`: memory traffic on locals, then fold what it exposes. */
    if (irOptLocalMemory(fn)) {
        do {
            folded  = irEvalConstantExpressions(fn);
            folded += irForwardStoreToReads(fn);
        } while (folded);
    }
    while (irFoldExtendTruncate(fn)) { /* to a fixed point */ }
    while (irFoldChainedTruncate(fn)) { /* likewise */ }
    /* After the folds, so it sees the SEXTs that survived them. */
    irMarkRedundantSext(fn);
    irCoalesceNoopWidens(fn);
    irFuseLocalRmw(fn);
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
    if (irSplitSharedAddressing(fn)) {
        while (irFuseIaddIntoMemAddressing(fn)) { /* fold the copies */ }
    }

    /* Forwarding again, now that fusion has been over the block.
     *
     * A source living in a scratch register survives only until an
     * instruction writes that register, so the first sweep drops it at
     * the first arithmetic op it crosses. Fusion then folds much of
     * that arithmetic INTO an addressing mode - the `shl` that scaled
     * an index becomes an `lsl #2` on the load - and the instruction
     * whose clobber ended the range no longer exists. `int at(struct S
     * *s, long i) { return s->e[i]; }` kept `mov x13, x0` purely
     * because the base was considered while a since-vanished `shl` was
     * still in the way of `s`.
     *
     * Only worth a second pass, not a fixed point: fusion consumes
     * arithmetic, and forwarding rewrites operands without producing
     * any for it to consume. */
    if (irForwardStoreToReads(fn)) {
        irDeadStoreEliminate(fn);
    }
    /* After the extend/truncate folds have settled which widens are
     * really there: an op the backend will emit 32-bit reads only the
     * low half of its operands, so any widen feeding one is dead work.
     * Removing the reference is what makes the widen collectable. */
    if (irNarrowOpOperands(fn)) {
        irDeadStoreEliminate(fn);
    }

    /* Both need the instruction ORDER settled, so they run once nothing
     * else is moving things about. */
    irForwardParamToFirstArg(fn);
    irForwardAggReturn(fn);
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

static int irOpIsCommutativeInt(IrOp op) {
    switch (op) {
        case IR_IADD: case IR_IMUL:
        case IR_AND:  case IR_OR:   case IR_XOR:
            return 1;
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
    /* `idx` counts - see irSlotReadCount. */
    IrValue *vs[4] = { I->r1, I->r2, I->idx, NULL };
    if (I->op == IR_BR || I->op == IR_RET ||
        I->op == IR_STORE_DEREF || I->op == IR_RMW_DEREF) vs[3] = I->dst;
    for (int k = 0; k < 4; ++k) {
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

            /* The only use is a phi in the block this one falls into.
             *
             * A phi's copy is materialised on the EDGE, after the
             * terminator, so the consumer is not an instruction in this
             * block and none of the checks below can see it. Without
             * this the value went to its stack slot and came straight
             * back out again:
             *
             *     cset x0, le
             *     stur x0, [x29,#-16]      <- def spills
             *     ldur x0, [x29,#-16]      <- phi copy reloads
             *     sturb w0, [x29,#-24]
             *
             * Only for an unconditional jump: a conditional terminator
             * reads its own operand out of the result register, and the
             * copies then run per-edge. */
            if (next->op == IR_JMP) {
                IrBlock *succ = next->extra.blocks.target_block;
                IrInstr *first = succ && succ->instructions
                               ? (IrInstr *)succ->instructions->next->value
                               : NULL;
                if (first && first->op == IR_PHI &&
                    irOpReadsTmp(first, dst_id) &&
                    !irIsFloat(cur->dst->type))
                {
                    if (pool) {
                        cur->dst->loc.kind   = IR_LOC_REG;
                        cur->dst->loc.as.reg = cur->op == IR_CALL
                                               ? pool->int_return_reg
                                               : pool->int_result_reg;
                        continue;
                    }
                }
            }

            /* A commutative consumer reading this tmp as r2: swap it to
             * r1, the only operand the backend can take from the result
             * reg (it may load r1 there, clobbering an r2 it held).
             * Same-typed operands only: some emitters size the op from
             * r1. */
            if (irOpIsCommutativeInt(next->op) &&
                irIsTmp(next->r2) && irVarId(next->r2) == dst_id &&
                next->r1 && !(irIsTmp(next->r1) && irVarId(next->r1) == dst_id) &&
                next->r1->type == next->r2->type &&
                irValueByteSize(next->r1) == irValueByteSize(next->r2))
            {
                IrValue *tmp = next->r1;
                next->r1 = next->r2;
                next->r2 = tmp;
            }

            /* The consumer reads this tmp from the result reg.
             * Match r1-of-next, dst-of-next (STORE_DEREF address),
             * idx-of-next (SIB), or args[0]-of-next (CALL arg0).
             * Skip type-mismatched chains. */
            int matches = 0;
            int consumer_reads_float = 0;
            int via_arg0 = 0;
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
                    via_arg0 = 1;
                    consumer_reads_float = irIsFloat(arg0->type);
                }
            }
            if (!matches) continue;

            int producer_is_float = irIsFloat(cur->dst->type);
            if (producer_is_float != consumer_reads_float) continue;
            if (!pool) continue;
            /* The ABI register when the value is born there (a call
             * result) or needed there (returned, or the first argument);
             * otherwise the accumulator the consumer computes in. */
            int abi = cur->op == IR_CALL || next->op == IR_RET || via_arg0;
            cur->dst->loc.kind   = IR_LOC_REG;
            if (producer_is_float)
                cur->dst->loc.as.reg = abi ? pool->float_return_reg
                                           : pool->float_result_reg;
            else
                cur->dst->loc.as.reg = abi ? pool->int_return_reg
                                           : pool->int_result_reg;
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
                    safe = 1;
                    break;
                default:
                    safe = 0;
                    break;
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
