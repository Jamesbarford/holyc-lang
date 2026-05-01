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
 * For each promotable alloca (one whose dst tmp appears only as an IR_LOAD
 * address or an IR_STORE address) we walk the function in CFG order,
 * tracking the alloca's "current value" per block. Where predecessors
 * disagree we insert an IR_PHI at the start of the merge block and use
 * its dst as the entry value. Loads are rewritten (via a global rename
 * map) to the current value at the load point; stores update the env
 * and become IR_NOP.
 *
 * Slice-0 has no loops, so a single forward pass over the block list
 * (which is added in lowering order, ~RPO) is sufficient — every
 * predecessor of a block is visited before that block.
 *
 * As a separate, simpler pass we also forward IR_LOADs whose address is
 * a PARAM/LOCAL that nothing ever stores to.
 */

typedef struct AllocaInfo {
    IrValue *alloca_dst;     /* IR_VAL_TMP returned by IR_ALLOCA */
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

/* Chase a rename chain. */
static IrValue *chase(Map *rename, IrValue *v) {
    while (v && v->kind == IR_VAL_TMP && mapHasInt(rename, v->as.var.id)) {
        IrValue *next = (IrValue *)mapGetInt(rename, v->as.var.id);
        if (next == v) break;
        v = next;
    }
    return v;
}

/* Get a block's saved out-environment, or NULL if it hasn't been visited. */
static Map *blockOut(Map *block_outs, IrBlock *bb) {
    if (!mapHasInt(block_outs, bb->id)) return NULL;
    return (Map *)mapGetInt(block_outs, bb->id);
}

void irMem2Reg(IrFunction *func) {
    if (!func) return;

    Map *info = mapNew(16, &map_uint_to_uint_type);

    /* Pass 1: find allocas. */
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

    /* Pass 2: classify uses. An alloca tmp may only appear as IR_ALLOCA.dst,
     * IR_STORE.dst (address), or IR_LOAD.r1 (address). Anything else
     * disqualifies. */
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
            if (I->op == IR_LOAD) {
                /* dst of LOAD is the loaded value, not an alloca tmp. */
                continue;
            }
            AllocaInfo *a;
            if ((a = infoLookup(info, I->dst))) a->other_uses++;
            if ((a = infoLookup(info, I->r1)))  a->other_uses++;
            if ((a = infoLookup(info, I->r2)))  a->other_uses++;
        }
    }

    /* Pass 3: per-block dataflow. Walk blocks in list order (which is the
     * lowering order ~ RPO for our acyclic CFG) so every predecessor is
     * already processed when we visit a block. */
    Map *block_outs = mapNew(16, &map_uint_to_uint_type);
    Map *rename = mapNew(32, &map_uint_to_uint_type);

    listForEach(func->blocks) {
        IrBlock *B = (IrBlock *)it->value;
        Map *current_env = mapNew(8, &map_uint_to_uint_type);
        Map *preds = irFunctionGetPredecessors(func, B);

        if (preds && preds->size > 0) {
            /* For each promotable alloca, compute the in-env entry. */
            MapIter *iter = mapIterNew(info);
            while (mapIterNext(iter)) {
                AllocaInfo *ai = (AllocaInfo *)iter->node->value;
                if (!ai || ai->other_uses != 0) continue;
                u32 X = ai->alloca_dst->as.var.id;

                /* Inspect each pred's out value for X. */
                IrValue *first = NULL;
                int agree = 1;
                int any = 0;
                {
                    MapIter *pi = mapIterNew(preds);
                    while (mapIterNext(pi)) {
                        IrBlock *P = (IrBlock *)pi->node->value;
                        Map *po = blockOut(block_outs, P);
                        IrValue *v = po && mapHasInt(po, X)
                                     ? (IrValue *)mapGetInt(po, X) : NULL;
                        if (v) {
                            any = 1;
                            if (!first) first = v;
                            else if (first != v) agree = 0;
                        }
                    }
                    mapIterRelease(pi);
                }
                if (!any) continue;

                if (agree) {
                    mapAdd(current_env, (void *)(u64)X, (void *)first);
                    continue;
                }

                /* Disagreement: insert phi at head of B and fill its pairs. */
                IrValue *phi_dst = irTmp(ai->alloca_dst->type,
                                         ai->alloca_dst->as.var.size);
                IrInstr *phi = irPhi(phi_dst);

                MapIter *pi = mapIterNew(preds);
                while (mapIterNext(pi)) {
                    IrBlock *P = (IrBlock *)pi->node->value;
                    Map *po = blockOut(block_outs, P);
                    IrValue *v = po && mapHasInt(po, X)
                                 ? (IrValue *)mapGetInt(po, X) : NULL;
                    irAddPhiIncoming(phi, v, P);
                }
                mapIterRelease(pi);

                listPrepend(B->instructions, phi);
                mapAdd(current_env, (void *)(u64)X, (void *)phi_dst);
            }
            mapIterRelease(iter);
        }

        /* Walk B's instructions, evolving current_env. Skip the phis we just
         * prepended (op == IR_PHI). */
        listForEach(B->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (I->op == IR_PHI) continue;

            if (I->op == IR_STORE) {
                AllocaInfo *ai = infoLookup(info, I->dst);
                if (ai && ai->other_uses == 0) {
                    mapAdd(current_env,
                           (void *)(u64)ai->alloca_dst->as.var.id,
                           (void *)I->r1);
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

    /* Pass 3.5: NOP the alloca instructions of all promoted slots. */
    {
        MapIter *iter = mapIterNew(info);
        while (mapIterNext(iter)) {
            AllocaInfo *ai = (AllocaInfo *)iter->node->value;
            if (!ai) continue;
            if (ai->other_uses != 0) continue;
            ai->alloca_instr->op = IR_NOP;
            ai->alloca_instr->dst = NULL;
            ai->alloca_instr->r1 = NULL;
        }
        mapIterRelease(iter);
    }

    /* Pass 4: forward IR_LOADs from never-stored PARAM / LOCAL addresses. */
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

    /* Pass 5: rewrite all surviving operand pointers via the rename map. */
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            I->r1 = chase(rename, I->r1);
            I->r2 = chase(rename, I->r2);
            if (I->op == IR_BR || I->op == IR_RET) {
                I->dst = chase(rename, I->dst);
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                    IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                    p->ir_value = chase(rename, p->ir_value);
                }
            }
        }
    }

    /* Cleanup. */
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
    mapRelease(block_outs);
    mapRelease(info);
    mapRelease(rename);
}
