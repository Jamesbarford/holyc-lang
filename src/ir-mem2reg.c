#include <stdlib.h>
#include <string.h>

#include "containers.h"
#include "ir-mem2reg.h"
#include "ir-types.h"
#include "list.h"
#include "util.h"

/*
 * Slice-0 mini mem2reg.
 *
 * Scope: an alloca is "promotable" iff its dst tmp appears only as
 *   - the alloca instruction's own dst,
 *   - the dst (address) operand of an IR_STORE,
 *   - the r1 (address) operand of an IR_LOAD,
 * and the alloca has exactly one IR_STORE in the function. Loads can be in
 * any block; they all read the unique stored value because slice-0 has no
 * loops, no aliasing, no address-of, so the store either dominates every
 * load or the program is reading uninitialized memory (programmer bug, not
 * our problem).
 *
 * Multi-store allocas would need phi nodes at CFG merges. The codegen
 * doesn't lower IR_PHI yet, so they're left as is — fusion alone already
 * handles the common multi-store cases (early-return if/else) reasonably.
 */

typedef struct AllocaInfo {
    IrValue *alloca_dst;     /* the alloca's IR_VAL_TMP */
    IrInstr *alloca_instr;
    IrInstr *unique_store;   /* valid only when store_count == 1 */
    int store_count;
    int other_uses;          /* dst tmp used somewhere it can't be promoted */
    List *loads;             /* List<IrInstr *> */
} AllocaInfo;

static AllocaInfo *infoNew(IrInstr *alloca) {
    AllocaInfo *ai = (AllocaInfo *)calloc(1, sizeof(*ai));
    ai->alloca_instr = alloca;
    ai->alloca_dst = alloca->dst;
    ai->loads = listNew();
    return ai;
}

static void infoRelease(AllocaInfo *ai) {
    if (!ai) return;
    listRelease(ai->loads, NULL);
    free(ai);
}

static AllocaInfo *infoLookup(Map *info, IrValue *v) {
    if (!v || v->kind != IR_VAL_TMP) return NULL;
    if (!mapHasInt(info, v->as.var.id)) return NULL;
    return (AllocaInfo *)mapGetInt(info, v->as.var.id);
}

/* Chase a rename chain. Stops as soon as the value is not a tmp or has no
 * further entry. The map can never produce a cycle for slice-0 because we
 * only insert "load_dst -> stored_value" pairs and stored values aren't
 * load dsts (loads are NOP'd). */
static IrValue *chase(Map *rename, IrValue *v) {
    while (v && v->kind == IR_VAL_TMP && mapHasInt(rename, v->as.var.id)) {
        IrValue *next = (IrValue *)mapGetInt(rename, v->as.var.id);
        if (next == v) break;
        v = next;
    }
    return v;
}

void irMem2Reg(IrFunction *func) {
    if (!func) return;

    Map *info = mapNew(16, &map_uint_to_uint_type);

    /* Pass 1: catalogue allocas. */
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

    /* Pass 2: classify how each tracked alloca's dst tmp is used. We must
     * see *every* reference to the alloca tmp to decide promotability. */
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_ALLOCA) continue;

            /* Address operand of a STORE: dst position. */
            if (I->op == IR_STORE) {
                AllocaInfo *ai = infoLookup(info, I->dst);
                if (ai) {
                    ai->store_count++;
                    ai->unique_store = I;
                }
                /* The value-side r1 is never an alloca tmp address; if it
                 * somehow is, treat it as escaping. */
                AllocaInfo *r1ai = infoLookup(info, I->r1);
                if (r1ai) r1ai->other_uses++;
                continue;
            }

            /* Address operand of a LOAD: r1 position. */
            if (I->op == IR_LOAD) {
                AllocaInfo *ai = infoLookup(info, I->r1);
                if (ai) {
                    listAppend(ai->loads, I);
                }
                /* Defensive: dst of a LOAD is the loaded value, not an
                 * alloca address. r2 isn't used by LOAD. */
                continue;
            }

            /* Any other appearance of an alloca tmp disqualifies it. */
            AllocaInfo *a;
            if ((a = infoLookup(info, I->dst))) a->other_uses++;
            if ((a = infoLookup(info, I->r1)))  a->other_uses++;
            if ((a = infoLookup(info, I->r2)))  a->other_uses++;
        }
    }

    /* Pass 3: promote single-store allocas, recording renames. */
    Map *rename = mapNew(32, &map_uint_to_uint_type);
    {
        MapIter *iter = mapIterNew(info);
        while (mapIterNext(iter)) {
            AllocaInfo *ai = (AllocaInfo *)iter->node->value;
            if (!ai) continue;
            if (ai->other_uses != 0) continue;
            if (ai->store_count != 1) continue;

            IrValue *stored = ai->unique_store->r1;

            /* Each load's result tmp gets renamed to the stored value. */
            listForEach(ai->loads) {
                IrInstr *load = (IrInstr *)it->value;
                if (load->dst && load->dst->kind == IR_VAL_TMP) {
                    mapAdd(rename,
                           (void *)(u64)load->dst->as.var.id,
                           (void *)stored);
                }
                load->op = IR_NOP;
                load->r1 = NULL;
                load->r2 = NULL;
            }

            ai->unique_store->op = IR_NOP;
            ai->unique_store->r1 = NULL;
            ai->unique_store->dst = NULL;
            ai->alloca_instr->op = IR_NOP;
            ai->alloca_instr->dst = NULL;
            ai->alloca_instr->r1 = NULL;
        }
        mapIterRelease(iter);
    }

    /* Pass 3.5: forward LOADs whose address operand is a PARAM that nothing
     * ever stores to. Slice-0 doesn't currently store to params, so this
     * pass is the dominant source of stack-frame savings on small leaf
     * functions like add(I64,I64). For each such load, the loaded value is
     * exactly the param itself — we just rename load->dst -> param. */
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

    /* Pass 4: rewrite all surviving operands through the rename map. */
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
        }
    }

    /* Cleanup. */
    {
        MapIter *iter = mapIterNew(info);
        while (mapIterNext(iter)) {
            infoRelease((AllocaInfo *)iter->node->value);
        }
        mapIterRelease(iter);
    }
    mapRelease(info);
    mapRelease(rename);
}
