#include <stdlib.h>
#include <string.h>

#include "arena.h"
#include "containers.h"
#include "ir-debug.h"
#include "ir-mem2reg.h"
#include "ir-types.h"
#include "list.h"

static Arena ir_arena;
static int ir_arena_init = 0;

void irMem2RegMemoryInit(void) {
    if (!ir_arena_init) {
        /* @TODO; correct the size of the arena */
        arenaInit(&ir_arena, 512);
        ir_arena_init = 1;
    }
}

void irMem2RegMemoryRelease(void) {
    if (ir_arena_init) {
        ir_arena_init = 0;
        arenaClear(&ir_arena);
    }
}

void *irMem2RegAlloc(u32 size) {
    return arenaAlloc(&ir_arena, size);
}

typedef struct IrAllocaInfo {
    IrValue *alloca_dst;
    IrInstr *alloca_instr;
    /* Signals that this cannot be promoted */
    int other_uses;
} IrAllocaInfo;

AoStr *irAllocaInfoToString(void *_info) {
    IrAllocaInfo *info = _info;
    AoStr *s = aoStrNew();
    AoStr *dst = irValueToString(info->alloca_dst);
    AoStr *instr = irInstrToString(info->alloca_instr);
    aoStrCatFmt(s, "IrAllocaInfo { %S, %S, %i }", dst, instr, info->other_uses);
    aoStrRelease(dst);
    aoStrRelease(instr);
    return s;
}

AoStr *irMapToString(void *_map) {
    return mapToString(_map, ",\n");
}

/* `Map<u64, IrAllocaInfo *> `*/
MapType map_uint_to_ir_alloca_info_type = {
    .match           = mapIntKeyMatch,
    .hash            = mapIntKeyHash,
    .get_key_len     = mapIntKeyLen,
    .key_to_string   = mapIntToString,
    .key_release     = NULL,
    .value_to_string = irAllocaInfoToString,
    .value_release   = NULL,
    .key_type        = "u64",
    .value_type      = "IrAllocaInfo *",
};


/* Map<u64, Map<u64, IrInstr *>> */
MapType map_uint_to_ir_instr_map_type = {
    .match           = mapIntKeyMatch,
    .hash            = mapIntKeyHash,
    .get_key_len     = mapIntKeyLen,
    .key_to_string   = mapIntToString,
    .key_release     = NULL,
    .value_to_string = irMapToString,
    .value_release   = (void (*)(void *))mapRelease,
    .key_type        = "u64",
    .value_type      = "Map<u64, Map<u64, IrInstr *>>",
};

/* Map<u64, Map<u64, IrInstr *>> */
MapType map_uint_to_ir_value_map_type = {
    .match           = mapIntKeyMatch,
    .hash            = mapIntKeyHash,
    .get_key_len     = mapIntKeyLen,
    .key_to_string   = mapIntToString,
    .key_release     = NULL,
    .value_to_string = irMapToString,
    .value_release   = (void (*)(void *))mapRelease,
    .key_type        = "u64",
    .value_type      = "Map<u64, Map<u64, Value *>>",
};

static Map *irAllocaInfoMapNew(void) {
    return mapNew(16, &map_uint_to_ir_alloca_info_type);
}

static IrAllocaInfo *irAllocaInfoNew(IrInstr *alloca) {
    IrAllocaInfo *info = (IrAllocaInfo *)irMem2RegAlloc(sizeof(IrAllocaInfo));
    info->alloca_instr = alloca;
    info->alloca_dst = alloca->dst;
    info->other_uses = 0;
    return info;
}

static IrAllocaInfo *irAllocInfoLookup(Map *info, IrValue *val) {
    if (irIsTmp(val)) {
        return (IrAllocaInfo *)mapGetInt(info, val->as.var.id);
    }
    return NULL;
}

static IrValue *irChaseValue(Map *rename, IrValue *val) {
    while (irIsTmp(val) && mapHasInt(rename, val->as.var.id)) {
        IrValue *next = (IrValue *)mapGetInt(rename, val->as.var.id);
        if (next == val) break;
        val = next;
    }
    return val;
}

static Map *irBlockOut(Map *block_outs, IrBlock *bb) {
    return (Map *)mapGetInt(block_outs, bb->id);
}


/* Find the `IrAllocaInfo` in the Map and bump the `other_uses` counter */
static void irInfoLookupAndBumpUse(Map *info, IrValue *val) {
    IrAllocaInfo *ai = irAllocInfoLookup(info, val);
    if (ai) ai->other_uses++;
}

/* Find out where all of the allocas are an store them in the hash table */
static Map *irTrackAllocas(IrFunction *fn) {
    Map *info = irAllocaInfoMapNew();
    listForEach(fn->blocks) {
        IrBlock *bb = listValue(IrBlock *, it);
        listForEach(bb->instructions) {
            IrInstr *I = listValue(IrInstr *, it);
            if (I->op == IR_ALLOCA && irIsTmp(I->dst)) {
                IrAllocaInfo *ai = irAllocaInfoNew(I);
                mapAdd(info, (void *)(u64)I->dst->as.var.id, ai);
            }
        }
    }
    return info;
}

/* Classify the usages of `alloca` */
static void irQualifyAllocaUsage(IrFunction *fn, Map *info) {
    listForEach(fn->blocks) {
        IrBlock *bb = listValue(IrBlock *, it);
        listForEach(bb->instructions) {
            IrInstr *I = listValue(IrInstr *, it);
            if (I->op == IR_ALLOCA) continue;
            if (I->op == IR_STORE) {
                irInfoLookupAndBumpUse(info, I->r1);
                continue;
            }
            if (I->op == IR_LOAD) continue;
            irInfoLookupAndBumpUse(info, I->dst);
            irInfoLookupAndBumpUse(info, I->r1);
            irInfoLookupAndBumpUse(info, I->r2);
        }
    }
}

/* Insert phi nodes for every block with multiple predecessors for every 
 * promotable alloca. Later the `Map<u64, Map<u64, IrInstr *>>` will be 
 * checked for redundancy */
static Map *irInsertPhis(IrFunction *fn, Map *info) {
    Map *block_phis = mapNew(16, &map_uint_to_ir_instr_map_type);
    listForEach(fn->blocks) {
        IrBlock *bb = listValue(IrBlock *, it);
        Map *preds = irFunctionGetPredecessors(fn, bb);
        /* Are there multiple predecessors? */
        if (!preds || preds->size < 2) continue;

        Map *phis_here = irInstrMapNew();
        MapIter ai_it;
        mapIterInit(info, &ai_it);
        while (mapIterNext(&ai_it)) {
            IrAllocaInfo *ai = (IrAllocaInfo *)ai_it.node->value;
            /* Is the alloca used? */
            if (!ai || ai->other_uses != 0)
                continue;
            IrValue *phi_dst = irTmp(ai->alloca_dst->type,
                    ai->alloca_dst->as.var.size);
            IrInstr *phi = irPhi(phi_dst);
            listPrepend(bb->instructions, phi);
            mapAdd(phis_here,
                    (void *)(u64)ai->alloca_dst->as.var.id,
                    (void *)phi);
        }
        mapAdd(block_phis, (void *)(u64)bb->id, phis_here);
    }
    return block_phis;
}

void irForwardDataFlow(IrFunction *fn,
                       Map *info,
                       Map *block_phis,
                       Map **_block_outs,
                       Map **_rename)
{
    Map *block_outs = mapNew(16, &map_uint_to_ir_value_map_type);
    Map *rename = irVarValueMapNew();
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        Map *current_env = irVarValueMapNew();
        Map *preds = irFunctionGetPredecessors(fn, bb);

        if (preds) {
            if (preds->size >= 2 && mapHasInt(block_phis, bb->id)) {
                Map *phis_here = (Map *)mapGetInt(block_phis, bb->id);
                MapIter phi_iter;
                mapIterInit(phis_here, &phi_iter);
                while (mapIterNext(&phi_iter)) {
                    MapNode *n = phi_iter.node;
                    u32 alloca_id = (u32)(u64)(void *)n->key;
                    IrInstr *phi = (IrInstr *)n->value;
                    mapAdd(current_env, (void *)(u64)alloca_id, (void *)phi->dst);
                }
            } else if (preds->size == 1) {
                MapIter preds_iter;
                mapIterInit(preds, &preds_iter);
                mapIterNext(&preds_iter);
                IrBlock *pred = (IrBlock *)preds_iter.node->value;
                Map *po = irBlockOut(block_outs, pred);
                if (po) {
                    MapIter block_out_iter;
                    mapIterInit(po, &block_out_iter);
                    while (mapIterNext(&block_out_iter)) {
                        MapNode *n = block_out_iter.node;
                        mapAdd(current_env, n->key, (IrValue *)n->value);
                    }
                }
            }
        }

        listForEach(bb->instructions) {
            IrInstr *I = listValue(IrInstr *, it);
            if (I->op == IR_NOP || I->op == IR_PHI) continue;

            if (I->op == IR_STORE) {
                IrAllocaInfo *ai = irAllocInfoLookup(info, I->dst);
                if (ai && ai->other_uses == 0) {
                    /* Resolve r1 through any previous load-rename. Then the
                     * environment will have the value that the store sees as
                     * opposed to a stale tmp id */
                    IrValue *val = irChaseValue(rename, I->r1);
                    mapAdd(current_env,
                            (void *)(u64)ai->alloca_dst->as.var.id,
                            (void *)val);
                    I->op = IR_NOP;
                    I->r1 = I->dst = NULL;
                }
                continue;
            }

            if (I->op == IR_LOAD) {
                IrAllocaInfo *ai = irAllocInfoLookup(info, I->r1);
                if (ai && ai->other_uses == 0) {
                    IrValue *val = mapGetInt(current_env, ai->alloca_dst->as.var.id);
                    if (val && irIsTmp(I->dst)) {
                        mapAdd(rename, (void *)(u64)I->dst->as.var.id, val);
                    }
                    I->op = IR_NOP;
                    I->r1 = NULL;
                }
                continue;
            }
        }
        mapAdd(block_outs, (void *)(u64)bb->id, (void *)current_env);
    }
    
    *_block_outs = block_outs;
    *_rename = rename;
}

/* Mutates the info map's values */
void irNopPromotedAllocas(Map *info) {
    MapIter iter;
    mapIterInit(info, &iter);
    /* Mark all unused with `IR_NOP` */
    while (mapIterNext(&iter)) {
        IrAllocaInfo *ai = (IrAllocaInfo *)iter.node->value;
        if (!ai || ai->other_uses != 0)
            continue;
        ai->alloca_instr->op = IR_NOP;
        ai->alloca_instr->dst = NULL;
        ai->alloca_instr->r1 = NULL;
    }
}

/* If no predecessor defined the PHI then it becomes a nop. This could have 
 * occurred during pass 3. */
void irFillPhiPairs(IrFunction *fn, Map *block_phis, Map *blocks_out) {
    Map *phis_here, *preds;
    MapIter phi_iter, pred_iter;

    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (!mapHasInt(block_phis, bb->id))
            continue;

        phis_here = (Map *)mapGetInt(block_phis, bb->id);
        if ((preds = (Map *)irFunctionGetPredecessors(fn, bb)) == NULL)
            continue;

        mapIterInit(phis_here, &phi_iter);
        while (mapIterNext(&phi_iter)) {
            MapNode *n = phi_iter.node;
            u32 alloca_id = (u32)(u64)(void *)n->key;
            IrInstr *phi = (IrInstr *)n->value;

            /* We need the phi if one predecessor defined the alloca with a 
             * value other than the phi's own `dst`. A self reference can 
             * occur if the phi is unwritten on every path from the entry
             * to here. For exmplae a return slot that is only stored downstream
             * of the merge. */
            int any_defined = 0;
            mapIterInit(preds, &pred_iter);
            while (mapIterNext(&pred_iter)) {
                IrBlock *pb = (IrBlock *)pred_iter.node->value;
                Map *po = irBlockOut(blocks_out, pb);
                if (po) {
                    IrValue *val = (IrValue *)mapGetInt(po, alloca_id);
                    if (val && val != phi->dst) {
                        any_defined = 1;
                    }
                }
            }

            if (!any_defined) {
                phi->op = IR_NOP;
                phi->dst = NULL;
                continue;
            }
            mapIterInit(preds, &pred_iter);
            while (mapIterNext(&pred_iter)) {
                IrBlock *pb = (IrBlock *)pred_iter.node->value;
                Map *po = irBlockOut(blocks_out, pb);
                if (po) {
                    IrValue *val = (IrValue *)mapGetInt(po, alloca_id);
                    irAddPhiIncoming(phi, val, pb);
                }
            }
        }
    }
}

/* Forweard IR_LOADs from PARAMS and LOCALS that have never been stored */
void irForwardIrLoads(IrFunction *fn, Map *rename) {
    Set *ever_stored = setNew(8, &set_uint_type);

    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = listValue(IrInstr *, it);
            if (I->op != IR_STORE || !I->dst)
                continue;
            if (I->dst->kind == IR_VAL_PARAM || I->dst->kind == IR_VAL_LOCAL)
                setAdd(ever_stored, (void *)(u64)I->dst->as.var.id);
        }
    }

    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = listValue(IrInstr *, it);
            if (I->op != IR_LOAD)
                continue;
            IrValue *addr = I->r1;
            if (!addr)
                continue;
            if (addr->kind != IR_VAL_PARAM && addr->kind != IR_VAL_LOCAL)
                continue;
            if (setHas(ever_stored, (void *)(u64)addr->as.var.id))
                continue;
            if (irIsTmp(I->dst))
                mapAdd(rename, (void *)(u64)I->dst->as.var.id, addr);
            I->op = IR_NOP;
            I->r1 = I->r2 = NULL;
        }
    }

    setRelease(ever_stored);
}

/* Use the rename map to rewrite operands. This will resolve load-rename
 * indirections and param forwadring inside phi paris. This is needed to find 
 * redundant phis to compare for equality */
static void irRewriteOperands(IrFunction *fn, Map *rename) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            I->r1 = irChaseValue(rename, I->r1);
            I->r2 = irChaseValue(rename, I->r2);

            switch (I->op) {
                case IR_BR:
                case IR_RET:
                case IR_STORE:
                case IR_STORE_DEREF:
                    I->dst = irChaseValue(rename, I->dst);
                    break;
                default:
                    break;
            }

            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                    IrPair *pair = vecGet(IrPair *, I->extra.phi_pairs, i);
                    pair->ir_value = irChaseValue(rename, pair->ir_value);
                }
            }

            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 i = 0; i < args->size; ++i) {
                    IrValue *arg = vecGet(IrValue *, args, i);
                    args->entries[i] = irChaseValue(rename, arg);
                }
            }
        }
    }
}


void irDropRedundantPhis(Map *block_phis, Map *rename) {
    MapIter bp_it, phi_iter;
    int dropped_any;

    do {
        dropped_any = 0;
        mapIterInit(block_phis, &bp_it);
        while (mapIterNext(&bp_it)) {
            Map *phis_here = (Map *)bp_it.node->value;
            mapIterInit(phis_here, &phi_iter);
            while (mapIterNext(&phi_iter)) {
                IrInstr *phi = (IrInstr *)phi_iter.node->value;
                if (phi->op == IR_NOP)
                    continue;
                if (!phi->extra.phi_pairs)
                    continue;

                IrValue *uniq = NULL;
                int ok = 1;

                for (u64 i = 0; i < phi->extra.phi_pairs->size; ++i) {
                    IrPair *pair = vecGet(IrPair *, phi->extra.phi_pairs, i);
                    IrValue *val = irChaseValue(rename, pair->ir_value);

                    /* A phi that references itself and undefined pairs are
                     * skipped. If its value is purely undefined then it is
                     * dropped without a rename as nothing should be using it.*/
                    if (val == phi->dst)
                        continue;
                    if (!val)
                        continue;
                    if (!uniq)
                        uniq = val;
                    else if (uniq != val) {
                        ok = 0;
                        break;
                    }
                }
                if (!ok) continue;

                if (uniq) {
                    /* All non-self/undefined pairs agree on `uniq`. So it can
                     * be replaced and nop'd */
                    mapAdd(rename, (void *)(u64)phi->dst->as.var.id, uniq);
                }
                phi->op = IR_NOP;
                phi->dst = NULL;
                dropped_any = 1;
            }
        }
    } while (dropped_any);
}

void irMem2Reg(IrFunction *fn) {
    Map *info, *block_phis, *block_outs, *rename;

    /* Pass 1 */
    info = irTrackAllocas(fn);

    /* Pass 2 */
    irQualifyAllocaUsage(fn, info);

    /* Pass 3 */
    block_phis = irInsertPhis(fn, info);

    /* Pass 4 */
    irForwardDataFlow(fn, info, block_phis, &block_outs, &rename);

    /* Pass 5 */
    irNopPromotedAllocas(info);

    /* Pass 6 */
    irFillPhiPairs(fn, block_phis, block_outs);

    /* Pass 7 */
    irForwardIrLoads(fn, rename);

    /* Pass 8 */
    irRewriteOperands(fn, rename);

    /* Pass 9 */
    irDropRedundantPhis(block_phis, rename);

    /* Pass 10 */
    irRewriteOperands(fn, rename);

    irMem2RegMemoryRelease();
    mapRelease(block_outs);
    mapRelease(block_phis);
    mapRelease(info);
    mapRelease(rename);
}
