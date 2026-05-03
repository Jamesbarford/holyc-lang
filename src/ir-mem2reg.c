#include <stdlib.h>
#include <string.h>

#include "containers.h"
#include "ir-mem2reg.h"
#include "ir-types.h"
#include "list.h"
#include "util.h"

/* IR-internals from ir.c — not in ir.h. */
extern IrValue *irTmp(IrValueType type, u16 size);
extern IrInstr *irPhi(IrValue *result);
extern void irAddPhiIncoming(IrInstr *phi, IrValue *value, IrBlock *block);

/*
 * Slice-0 mem2reg with phi insertion.
 *
 * Algorithm — pessimistic phi insertion + redundant-phi drop:
 *
 *   1. Catalogue allocas. An alloca is promotable iff its dst tmp appears
 *      only as IR_LOAD.r1 (address) or IR_STORE.dst (address).
 *
 *   2. For every multi-predecessor block, insert one IR_PHI per promotable
 *      alloca at the head of the block. Pairs are left empty initially.
 *
 *   3. Forward dataflow over the block list (lowering order ≈ RPO). At each
 *      block, in_env starts from the phis (multi-pred) or the single pred's
 *      out_env. Walk instructions: STORE updates current_env and becomes
 *      IR_NOP; LOAD renames its dst tmp to current_env and becomes IR_NOP.
 *      Save out_env per block.
 *
 *      Loops work because the phi.dst is itself a fresh SSA value placed at
 *      the loop header; the body's out_env will reference phi.dst via the
 *      load-rename, so when we fill the back-edge pair we get the right
 *      "value at end of body" expression — a self-referential tmp graph
 *      that is exactly the SSA form a loop wants.
 *
 *   4. Fill each inserted phi's pairs from each predecessor's saved out_env.
 *
 *   5. Forward IR_LOADs whose address is a never-stored PARAM/LOCAL
 *      directly to that PARAM/LOCAL.
 *
 *   6. Iteratively drop redundant phis (phis whose pairs all collapse to a
 *      single value) by funnelling them into the rename map.
 *
 *   7. Final operand rewrite: chase rename through r1 / r2 / IR_BR&IR_RET
 *      dst / IR_PHI pair values.
 */

typedef struct AllocaInfo {
    IrValue *alloca_dst;
    IrInstr *alloca_instr;
    int other_uses;          /* dst tmp used somewhere we can't promote */
} AllocaInfo;

static AllocaInfo *infoNew(IrInstr *alloca) {
    AllocaInfo *ai = (AllocaInfo *)calloc(1, sizeof(*ai));
    ai->alloca_instr = alloca;
    ai->alloca_dst = alloca->dst;
    return ai;
}

static AllocaInfo *infoLookup(Map *info, IrValue *v) {
    if (!v || v->kind != IR_VAL_TMP) return NULL;
    if (!mapHasInt(info, v->as.var.id)) return NULL;
    return (AllocaInfo *)mapGetInt(info, v->as.var.id);
}

static IrValue *chase(Map *rename, IrValue *v) {
    while (v && v->kind == IR_VAL_TMP && mapHasInt(rename, v->as.var.id)) {
        IrValue *next = (IrValue *)mapGetInt(rename, v->as.var.id);
        if (next == v) break;
        v = next;
    }
    return v;
}

static Map *blockOut(Map *block_outs, IrBlock *bb) {
    if (!mapHasInt(block_outs, bb->id)) return NULL;
    return (Map *)mapGetInt(block_outs, bb->id);
}

/* Apply chase to every operand-position in I. */
static void rewriteOperands(IrInstr *I, Map *rename) {
    if (I->op == IR_NOP) return;
    I->r1 = chase(rename, I->r1);
    I->r2 = chase(rename, I->r2);
    /* dst is a source operand for branches/returns and for stores
     * (which read dst as the destination address). */
    if (I->op == IR_BR || I->op == IR_RET ||
        I->op == IR_STORE || I->op == IR_STORE_DEREF) {
        I->dst = chase(rename, I->dst);
    }
    if (I->op == IR_PHI && I->extra.phi_pairs) {
        for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
            IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
            p->ir_value = chase(rename, p->ir_value);
        }
    }
    if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
        Vec *args = I->r1->as.array.values;
        for (u64 i = 0; i < args->size; ++i) {
            IrValue *a = (IrValue *)args->entries[i];
            args->entries[i] = chase(rename, a);
        }
    }
}

void irMem2Reg(IrFunction *func) {
    if (!func) return;

    Map *info = mapNew(16, &map_uint_to_uint_type);

    /* === Pass 1: catalogue allocas. === */
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_ALLOCA && I->dst && I->dst->kind == IR_VAL_TMP) {
                AllocaInfo *ai = infoNew(I);
                mapAdd(info, (void *)(u64)I->dst->as.var.id, ai);
            }
        }
    }

    /* === Pass 2: classify uses (disqualify if alloca tmp leaks). === */
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_ALLOCA) continue;
            if (I->op == IR_STORE) {
                AllocaInfo *r1ai = infoLookup(info, I->r1);
                if (r1ai) r1ai->other_uses++;
                continue;
            }
            if (I->op == IR_LOAD) continue;
            AllocaInfo *a;
            if ((a = infoLookup(info, I->dst))) a->other_uses++;
            if ((a = infoLookup(info, I->r1)))  a->other_uses++;
            if ((a = infoLookup(info, I->r2)))  a->other_uses++;
        }
    }

    /* === Pass 3: insert phis at every multi-pred block for every
     *             promotable alloca. block_phis[B->id] is a Map<alloca_id,
     *             IrInstr* phi>. We'll iterate this later to fill pairs and
     *             again to detect redundancy. === */
    Map *block_phis = mapNew(8, &map_uint_to_uint_type);
    listForEach(func->blocks) {
        IrBlock *B = (IrBlock *)it->value;
        Map *preds = irFunctionGetPredecessors(func, B);
        if (!preds || preds->size < 2) continue;

        Map *phis_here = mapNew(8, &map_uint_to_uint_type);
        MapIter *ai_it = mapIterNew(info);
        while (mapIterNext(ai_it)) {
            AllocaInfo *ai = (AllocaInfo *)ai_it->node->value;
            if (!ai || ai->other_uses != 0) continue;
            IrValue *phi_dst = irTmp(ai->alloca_dst->type,
                                     ai->alloca_dst->as.var.size);
            IrInstr *phi = irPhi(phi_dst);
            listPrepend(B->instructions, phi);
            mapAdd(phis_here,
                   (void *)(u64)ai->alloca_dst->as.var.id,
                   (void *)phi);
        }
        mapIterRelease(ai_it);
        mapAdd(block_phis, (void *)(u64)B->id, (void *)phis_here);
    }

    /* === Pass 4: forward dataflow. ===
     *
     * For each block, build current_env. At a multi-pred block we seed it
     * from the inserted phis' dsts; at a single-pred block we copy the
     * pred's out_env. Then walk instructions to NOP stores/loads and
     * record renames. */
    Map *block_outs = mapNew(16, &map_uint_to_uint_type);
    Map *rename = mapNew(32, &map_uint_to_uint_type);

    listForEach(func->blocks) {
        IrBlock *B = (IrBlock *)it->value;
        Map *current_env = mapNew(8, &map_uint_to_uint_type);
        Map *preds = irFunctionGetPredecessors(func, B);

        if (preds && preds->size >= 2 && mapHasInt(block_phis, B->id)) {
            Map *phis_here = (Map *)mapGetInt(block_phis, B->id);
            MapIter *pi = mapIterNew(phis_here);
            while (mapIterNext(pi)) {
                u32 alloca_id = (u32)(intptr_t)pi->node->key;
                IrInstr *phi = (IrInstr *)pi->node->value;
                mapAdd(current_env, (void *)(u64)alloca_id, (void *)phi->dst);
            }
            mapIterRelease(pi);
        } else if (preds && preds->size == 1) {
            MapIter *pi = mapIterNew(preds);
            mapIterNext(pi);
            IrBlock *P = (IrBlock *)pi->node->value;
            mapIterRelease(pi);
            Map *po = blockOut(block_outs, P);
            if (po) {
                MapIter *si = mapIterNew(po);
                while (mapIterNext(si)) {
                    mapAdd(current_env, si->node->key, si->node->value);
                }
                mapIterRelease(si);
            }
        }

        listForEach(B->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (I->op == IR_PHI) continue;

            if (I->op == IR_STORE) {
                AllocaInfo *ai = infoLookup(info, I->dst);
                if (ai && ai->other_uses == 0) {
                    /* Resolve r1 through any prior load-rename so the env
                     * holds the value the store sees, not a stale tmp id. */
                    IrValue *val = chase(rename, I->r1);
                    mapAdd(current_env,
                           (void *)(u64)ai->alloca_dst->as.var.id,
                           (void *)val);
                    I->op = IR_NOP;
                    I->r1 = NULL;
                    I->dst = NULL;
                }
                continue;
            }
            if (I->op == IR_LOAD) {
                AllocaInfo *ai = infoLookup(info, I->r1);
                if (ai && ai->other_uses == 0) {
                    IrValue *v = mapHasInt(current_env, ai->alloca_dst->as.var.id)
                                 ? (IrValue *)mapGetInt(current_env,
                                                        ai->alloca_dst->as.var.id)
                                 : NULL;
                    if (v && I->dst && I->dst->kind == IR_VAL_TMP) {
                        mapAdd(rename,
                               (void *)(u64)I->dst->as.var.id,
                               (void *)v);
                    }
                    I->op = IR_NOP;
                    I->r1 = NULL;
                }
                continue;
            }
        }

        mapAdd(block_outs, (void *)(u64)B->id, (void *)current_env);
    }

    /* === Pass 5: NOP the promoted allocas. === */
    {
        MapIter *iter = mapIterNew(info);
        while (mapIterNext(iter)) {
            AllocaInfo *ai = (AllocaInfo *)iter->node->value;
            if (!ai || ai->other_uses != 0) continue;
            ai->alloca_instr->op = IR_NOP;
            ai->alloca_instr->dst = NULL;
            ai->alloca_instr->r1 = NULL;
        }
        mapIterRelease(iter);
    }

    /* === Pass 6: fill phi pairs from out_envs. If no pred defined the
     *             alloca, the phi is useless — NOP it. (Pessimistic phi
     *             insertion in pass 3 produces these for allocas only
     *             written downstream of the merge block, e.g. a return
     *             slot only stored after the loop.) === */
    listForEach(func->blocks) {
        IrBlock *B = (IrBlock *)it->value;
        if (!mapHasInt(block_phis, B->id)) continue;
        Map *phis_here = (Map *)mapGetInt(block_phis, B->id);
        Map *preds = irFunctionGetPredecessors(func, B);
        if (!preds) continue;

        MapIter *phi_it = mapIterNew(phis_here);
        while (mapIterNext(phi_it)) {
            u32 alloca_id = (u32)(intptr_t)phi_it->node->key;
            IrInstr *phi = (IrInstr *)phi_it->node->value;

            /* The phi is meaningful only if at least one predecessor
             * actually defined the alloca with a value other than the
             * phi's own dst. A self-reference would arise when the alloca
             * is unwritten on every path from entry to here — e.g. a
             * return slot that's only stored downstream of the merge. */
            int any_defined = 0;
            {
                MapIter *pi = mapIterNew(preds);
                while (mapIterNext(pi)) {
                    IrBlock *P = (IrBlock *)pi->node->value;
                    Map *po = blockOut(block_outs, P);
                    IrValue *v = po && mapHasInt(po, alloca_id)
                                 ? (IrValue *)mapGetInt(po, alloca_id)
                                 : NULL;
                    if (v && v != phi->dst) any_defined = 1;
                }
                mapIterRelease(pi);
            }
            if (!any_defined) {
                phi->op = IR_NOP;
                phi->dst = NULL;
                continue;
            }

            MapIter *pi = mapIterNew(preds);
            while (mapIterNext(pi)) {
                IrBlock *P = (IrBlock *)pi->node->value;
                Map *po = blockOut(block_outs, P);
                IrValue *v = po && mapHasInt(po, alloca_id)
                             ? (IrValue *)mapGetInt(po, alloca_id) : NULL;
                irAddPhiIncoming(phi, v, P);
            }
            mapIterRelease(pi);
        }
        mapIterRelease(phi_it);
    }

    /* === Pass 7: forward IR_LOADs from never-stored PARAM/LOCAL. === */
    {
        Set *ever_stored = setNew(8, &set_uint_type);
        listForEach(func->blocks) {
            IrBlock *bb = (IrBlock *)it->value;
            listForEach(bb->instructions) {
                IrInstr *I = (IrInstr *)it->value;
                if (I->op != IR_STORE || !I->dst) continue;
                if (I->dst->kind == IR_VAL_PARAM ||
                    I->dst->kind == IR_VAL_LOCAL) {
                    setAdd(ever_stored, (void *)(u64)I->dst->as.var.id);
                }
            }
        }
        listForEach(func->blocks) {
            IrBlock *bb = (IrBlock *)it->value;
            listForEach(bb->instructions) {
                IrInstr *I = (IrInstr *)it->value;
                if (I->op != IR_LOAD) continue;
                IrValue *addr = I->r1;
                if (!addr) continue;
                if (addr->kind != IR_VAL_PARAM &&
                    addr->kind != IR_VAL_LOCAL) continue;
                if (setHas(ever_stored, (void *)(u64)addr->as.var.id)) continue;
                if (I->dst && I->dst->kind == IR_VAL_TMP) {
                    mapAdd(rename, (void *)(u64)I->dst->as.var.id, (void *)addr);
                }
                I->op = IR_NOP;
                I->r1 = NULL;
                I->r2 = NULL;
            }
        }
        setRelease(ever_stored);
    }

    /* === Pass 8: rewrite operands once with the rename map we have so
     *             far. This resolves load-rename indirections (and PARAM
     *             forwarding) inside phi pairs, which is needed for the
     *             redundant-phi pass to compare values for equality. === */
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            rewriteOperands((IrInstr *)it->value, rename);
        }
    }

    /* === Pass 9: drop redundant phis to a fixpoint. A phi is redundant if
     *             every pair value, after substituting away self-references,
     *             collapses to a single value v. We fold it into rename
     *             (phi.dst -> v) and NOP it; that may make further phis
     *             redundant, hence iterate. === */
    int dropped_any;
    do {
        dropped_any = 0;
        MapIter *bp_it = mapIterNew(block_phis);
        while (mapIterNext(bp_it)) {
            Map *phis_here = (Map *)bp_it->node->value;
            MapIter *p_it = mapIterNew(phis_here);
            while (mapIterNext(p_it)) {
                IrInstr *phi = (IrInstr *)p_it->node->value;
                if (phi->op == IR_NOP) continue;
                if (!phi->extra.phi_pairs) continue;

                IrValue *unique = NULL;
                int ok = 1;
                for (u64 i = 0; i < phi->extra.phi_pairs->size; ++i) {
                    IrPair *pair = vecGet(IrPair *, phi->extra.phi_pairs, i);
                    IrValue *v = chase(rename, pair->ir_value);
                    /* Self-references (phi -> its own dst) and undef pairs
                     * (NULL value) carry no constraint - skip both. A phi
                     * whose pairs are all self/undef is fully degenerate
                     * (its value is purely undef); we drop it without a
                     * rename, since nothing real should be reading it. */
                    if (v == phi->dst) continue;
                    if (!v) continue;
                    if (!unique) unique = v;
                    else if (unique != v) { ok = 0; break; }
                }
                if (!ok) continue;

                if (unique) {
                    /* All non-self/undef pairs agree on `unique`. Replace
                     * and NOP. */
                    mapAdd(rename, (void *)(u64)phi->dst->as.var.id,
                           (void *)unique);
                } else {
                    /* Pairs were all self-refs / undef. The phi is dead;
                     * NOP it without adding a rename. */
                }
                phi->op = IR_NOP;
                phi->dst = NULL;
                dropped_any = 1;
            }
            mapIterRelease(p_it);
        }
        mapIterRelease(bp_it);
    } while (dropped_any);

    /* === Pass 10: final operand rewrite (handles renames added by the
     *              redundant-phi loop). === */
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            rewriteOperands((IrInstr *)it->value, rename);
        }
    }

    /* === Cleanup. === */
    {
        MapIter *iter = mapIterNew(info);
        while (mapIterNext(iter)) {
            AllocaInfo *ai = (AllocaInfo *)iter->node->value;
            if (ai) free(ai);
        }
        mapIterRelease(iter);
    }
    {
        MapIter *iter = mapIterNew(block_outs);
        while (mapIterNext(iter)) {
            mapRelease((Map *)iter->node->value);
        }
        mapIterRelease(iter);
    }
    {
        MapIter *iter = mapIterNew(block_phis);
        while (mapIterNext(iter)) {
            mapRelease((Map *)iter->node->value);
        }
        mapIterRelease(iter);
    }
    mapRelease(block_outs);
    mapRelease(block_phis);
    mapRelease(info);
    mapRelease(rename);
}
