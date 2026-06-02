#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "ast.h"
#include "containers.h"
#include "ir-regalloc.h"
#include "ir-types.h"
#include "list.h"
#include "prsutil.h"
#include "util.h"

/* The active backend's register descriptor. Set by the backend driver
 * (e.g. x86_64.c) before codegen runs; read by both the regalloc pass
 * and codegen so neither has to know about specific ABIs. */
static IrRegPool *g_reg_pool = NULL;

void irRegPoolSet(IrRegPool *pool) {
    g_reg_pool = pool;
}

IrRegPool *irRegPoolGet(void) {
    return g_reg_pool;
}

void irCgSetLoff(IrRaCtx *ra, u32 var_id, int loff) {
    /* mapAddIntOrErr fails on duplicate; use it as an assert for distinctness. */
    int ok = mapAddIntOrErr(ra->id_to_loff, var_id, (void *)(u64)loff);
    if (!ok) {
        loggerPanic("ir-regalloc: duplicate loff mapping for var.id=%u\n",
                var_id);
    }
}

int irCgGetLoff(IrRaCtx *ra, IrValue *val) {
    assert(val != NULL);
    u32 id = irVarId(val);
    if (!mapHasInt(ra->id_to_loff, id)) {
        loggerPanic("ir-regalloc: no slot for var.id=%u kind=%d\n",
                    id, val->kind);
    }
    return (int)(u64)mapGetInt(ra->id_to_loff, id);
}

/* Reserve a fresh stack slot for an SSA temp. */
void irCgAllocTmp(IrRaCtx *ra, IrValue *val, int starting_offset) {
    if (!irIsTmp(val)) return;
    u32 id = irVarId(val);
    if (mapHasInt(ra->id_to_loff, id)) return;
    ra->extra_stack += 8;
    irCgSetLoff(ra, id, -(starting_offset + ra->extra_stack));
}

/* Reserve a fresh stack slot of a specific size (rounded up to 8-byte
 * alignment). Used by IR_ALLOCA so that a 256-byte alloca actually
 * gets 256 bytes of stack, not a default 8. */
static void irCgAllocTmpSized(IrRaCtx *ra,
                              IrValue *val,
                              int starting_offset,
                              int size_bytes)
{
    if (!irIsTmp(val)) return;
    u32 id = irVarId(val);
    if (mapHasInt(ra->id_to_loff, id)) return;
    int aligned = (size_bytes + 7) & ~7;
    if (aligned < 8) aligned = 8;
    ra->extra_stack += aligned;
    irCgSetLoff(ra, id, -(starting_offset + ra->extra_stack));
}

void irCgAllocOperandsForInstr(IrRaCtx *ra, IrInstr *I, int start) {
    /* Operands whose IrValue->loc has already been pinned to a
     * register (by the peephole) need no stack slot. */
    int skip_dst = I->dst && I->dst->loc.kind == IR_LOC_REG;
    int skip_r1  = I->r1  && I->r1->loc.kind  == IR_LOC_REG;

    switch (I->op) {
    case IR_NOP:
    case IR_JMP:
        return;

    case IR_ALLOCA: {
        /* The alloca's r1 operand carries the byte size as a constant.
         * Honour it so multi-byte allocations (struct buffers,
         * CatchFrame storage, etc.) get the right amount of stack
         * rather than a default 8 bytes. */
        int alloca_bytes = 8;
        if (I->r1 && I->r1->kind == IR_VAL_CONST_INT) {
            alloca_bytes = (int)I->r1->as._i64;
        }
        irCgAllocTmpSized(ra, I->dst, start, alloca_bytes);
        return;
    }

    case IR_LOAD:
    case IR_LOAD_DEREF:
        if (!skip_dst) irCgAllocTmp(ra, I->dst, start);
        if (!skip_r1)  irCgAllocTmp(ra, I->r1, start);
        return;

    case IR_STORE:
    case IR_STORE_DEREF:
        irCgAllocTmp(ra, I->dst, start);
        if (!skip_r1) irCgAllocTmp(ra, I->r1, start);
        return;

    case IR_RMW_DEREF:
        /* Same operand shape as STORE_DEREF: dst is the base address
         * (slot needed), r1 is the value to op-in (slot needed unless
         * a const or a register-pinned producer). */
        irCgAllocTmp(ra, I->dst, start);
        if (!skip_r1) irCgAllocTmp(ra, I->r1, start);
        if (I->idx) irCgAllocTmp(ra, I->idx, start);
        return;

    case IR_LEA:
        if (!skip_dst) irCgAllocTmp(ra, I->dst, start);
        return;

    case IR_IADD: case IR_ISUB: case IR_IMUL:
    case IR_AND:  case IR_OR:   case IR_XOR:
    case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
    case IR_SHL:  case IR_SHR:  case IR_SAR:
    case IR_ICMP:
    case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV:
    case IR_FCMP:
        if (!skip_dst) irCgAllocTmp(ra, I->dst, start);
        if (!skip_r1)  irCgAllocTmp(ra, I->r1, start);
        irCgAllocTmp(ra, I->r2, start);
        return;

    case IR_FNEG:
        if (!skip_dst) irCgAllocTmp(ra, I->dst, start);
        if (!skip_r1)  irCgAllocTmp(ra, I->r1, start);
        return;

    case IR_BR:
        if (!skip_r1) irCgAllocTmp(ra, I->dst, start);
        return;

    case IR_CMP_BR:
        /* Both operands need slots so the compare can load them; dst
         * is unused (result lives in EFLAGS). */
        irCgAllocTmp(ra, I->r1, start);
        irCgAllocTmp(ra, I->r2, start);
        return;

    case IR_RET:
        if (I->dst && !skip_r1) irCgAllocTmp(ra, I->dst, start);
        return;

    case IR_PHI:
        irCgAllocTmp(ra, I->dst, start);
        return;

    case IR_CALL:
        if (I->dst && I->dst->type != IR_TYPE_VOID && !skip_dst &&
            (I->dst->kind == IR_VAL_TMP || I->dst->kind == IR_VAL_LOCAL)) {
            irCgAllocTmp(ra, I->dst, start);
        }
        irCgAllocTmp(ra, I->r2, start);
        return;

    case IR_GEP: {
        /* Get-element-pointer for stack-allocated structs. dst aliases
         * `base + offset`, we bind its loff right here so subsequent
         * loads/stores through it just look up the right frame offset. */
        if (!irIsTmp(I->dst) || !I->r1 || !I->r2 ||
            I->r2->kind != IR_VAL_CONST_INT) {
            return;
        }
        int base_loff = irCgGetLoff(ra, I->r1);
        int field_loff = base_loff + (int)I->r2->as._i64;
        u32 id = irDstVarId(I);
        if (!mapHasInt(ra->id_to_loff, id)) {
            irCgSetLoff(ra, id, field_loff);
        }
        return;
    }

    default:
        irCgAllocTmp(ra, I->dst, start);
        irCgAllocTmp(ra, I->r1, start);
        irCgAllocTmp(ra, I->r2, start);
        break;
    }
}

/* Per-tmp live range: [first_pos, last_pos] in textual emit order
 * across all instructions. */
typedef struct IrCgRange {
    u32 var_id;
    int first;
    int last;
    /* The canonical IrValue for this tmp. All references share the
     * same pointer, so stashing it here lets the allocator write
     * `iv->loc` at the end of the pass without an extra lookup. */
    IrValue *val;
} IrCgRange;

static int irCgRangeCmp(const void *a, const void *b) {
    int af = ((const IrCgRange *)a)->first;
    int bf = ((const IrCgRange *)b)->first;
    if (af != bf) return af - bf;
    return ((const IrCgRange *)a)->last - ((const IrCgRange *)b)->last;
}

static void irCgRecordRef(Map *ranges, IrValue *v, int pos) {
    if (!irIsTmp(v)) return;
    /* Void-typed tmps (e.g. dst of a void IR_CALL) carry no value
     * and need no slot; recording them just inflates the frame. */
    if (v->type == IR_TYPE_VOID) return;
    u32 id = irVarId(v);
    IrCgRange *r = (IrCgRange *)mapGetInt(ranges, id);
    if (!r) {
        r = (IrCgRange *)malloc(sizeof(IrCgRange));
        r->var_id = id;
        r->first  = pos;
        r->last   = pos;
        r->val    = v;
        mapAdd(ranges, (void *)(u64)id, (void *)r);
    } else {
        if (pos < r->first) r->first = pos;
        if (pos > r->last)  r->last  = pos;
    }
}

/* Slot allocation for IR tmps. Two passes:
 *   1. Bind IR_ALLOCA sized scratch and IR_GEP aliases via the
 *      existing per-op path. Both need a fixed loff (sized buffer for
 *      ALLOCA; base+offset alias for GEP) that the per-tmp path can't
 *      compute generically.
 *   2. Linear-scan colour the rest. Compute each tmp's live range,
 *      sort by start, hand out 8-byte slots from a free-list. Slots
 *      get returned once their current holder's last use passes.
 *      Sharing slots across non-overlapping tmps is the only thing
 *      keeping the stack frame small without a real regalloc. */
void irCgAllocAllTmps(IrRaCtx *ra, int starting_offset) {
    listForEach(ra->func->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        listForEach(block->instructions) {
            IrInstr *instr = (IrInstr *)it->value;
            if (instr->op == IR_ALLOCA || instr->op == IR_GEP) {
                irCgAllocOperandsForInstr(ra, instr, starting_offset);
            }
        }
    }

    Map *ranges = mapNew(64, &map_uint_to_uint_type);
    int pos = 0;
    listForEach(ra->func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            irCgRecordRef(ranges, I->dst, pos);
            irCgRecordRef(ranges, I->r1, pos);
            irCgRecordRef(ranges, I->r2, pos);
            /* Addressing-mode-fused idx (LOAD_DEREF / STORE_DEREF /
             * RMW_DEREF). Without this the linear-scan misses tmps
             * that only appear as a SIB index. */
            irCgRecordRef(ranges, I->idx, pos);
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 i = 0; i < args->size; ++i) {
                    irCgRecordRef(ranges,
                                  (IrValue *)args->entries[i], pos);
                }
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                    IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                    irCgRecordRef(ranges, p->ir_value, pos);
                }
            }
            pos++;
        }
    }

    /* Tmps the peephole already parked in the result register (their
     * IrValue->loc is IR_LOC_REG) never see memory; skip them. */
    IrCgRange *arr = NULL;
    u64 n = 0;
    u64 cap = 0;
    MapIter it_r;
    mapIterInit(ranges, &it_r);
    while (mapIterNext(&it_r)) {
        IrCgRange *r = (IrCgRange *)it_r.node->value;
        if (!r) continue;
        if (mapHasInt(ra->id_to_loff, r->var_id)) continue;
        if (r->val && r->val->loc.kind == IR_LOC_REG) continue;
        if (n == cap) {
            cap = cap ? cap * 2 : 16;
            arr = (IrCgRange *)realloc(arr, cap * sizeof(IrCgRange));
        }
        arr[n++] = *r;
    }
    qsort(arr, n, sizeof(IrCgRange), irCgRangeCmp);

    int *slot_loff = NULL;
    int *slot_last = NULL;
    int n_slots = 0;
    int slot_cap = 0;

    for (u64 i = 0; i < n; ++i) {
        IrCgRange *r = &arr[i];
        int chosen = -1;
        for (int s = 0; s < n_slots; ++s) {
            if (slot_last[s] < r->first) {
                chosen = s;
                break;
            }
        }
        if (chosen < 0) {
            if (n_slots == slot_cap) {
                slot_cap = slot_cap ? slot_cap * 2 : 16;
                slot_loff = (int *)realloc(slot_loff, slot_cap * sizeof(int));
                slot_last = (int *)realloc(slot_last, slot_cap * sizeof(int));
            }
            ra->extra_stack += 8;
            slot_loff[n_slots] = -(starting_offset + ra->extra_stack);
            chosen = n_slots++;
        }
        slot_last[chosen] = r->last;
        irCgSetLoff(ra, r->var_id, slot_loff[chosen]);
        if (r->val) {
            r->val->loc.kind    = IR_LOC_SLOT;
            r->val->loc.as.loff = slot_loff[chosen];
        }
    }

    mapIterInit(ranges, &it_r);
    while (mapIterNext(&it_r)) {
        free(it_r.node->value);
    }
    mapRelease(ranges);
    free(arr);
    if (slot_loff) free(slot_loff);
    if (slot_last) free(slot_last);
}


int irBlockHasPhi(IrBlock *bb) {
    if (!bb) return 0;
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_PHI) return 1;
        if (I->op != IR_NOP) return 0;
    }
    return 0;
}

static IrInstr *irBlockTerminator(IrBlock *bb) {
    IrInstr *term = NULL;
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_NOP) continue;
        term = I;
    }
    return term;
}

/* We should be able to delete this as we already remove unused blocks */
Set *irCgComputeReferencedBlocks(IrFunction *fn) {
    Set *referenced = setNew(16, &set_uint_type);
    listForEach(fn->blocks) { 
        IrBlock *block = (IrBlock *)it->value;
        IrBlock *next_block = (it->next != fn->blocks)
                              ? (IrBlock *)it->next->value
                              : NULL;
        IrInstr *term = irBlockTerminator(block);
        if (!term)
            continue;

        switch (term->op) {
            case IR_JMP: {
                IrBlock *t = term->extra.blocks.target_block;
                if (t && t != next_block) {
                    setAdd(referenced, (void *)(u64)t->id);
                }
                break;
            }
            case IR_BR:
            case IR_CMP_BR: {
                IrBlock *t = (term->op == IR_CMP_BR)
                             ? term->extra.cmp_br.target_block
                             : term->extra.blocks.target_block;
                IrBlock *f = (term->op == IR_CMP_BR)
                             ? term->extra.cmp_br.fallthrough_block
                             : term->extra.blocks.fallthrough_block;
                int t_phi = irBlockHasPhi(t);
                int f_phi = irBlockHasPhi(f);
                if (!t_phi && !f_phi) {
                    if (next_block == t) {
                        if (f) setAdd(referenced, (void *)(u64)f->id);
                    } else if (next_block == f) {
                        if (t) setAdd(referenced, (void *)(u64)t->id);
                    } else {
                        if (t) setAdd(referenced, (void *)(u64)t->id);
                        if (f) setAdd(referenced, (void *)(u64)f->id);
                    }
                } else {
                    if (t) setAdd(referenced, (void *)(u64)t->id);
                    if (f && next_block != f) {
                        setAdd(referenced, (void *)(u64)f->id);
                    }
                }
                break;
            }
            default:
                break;
            }
    }
    return referenced;
}

static int irFnValueIsReferenced(IrFunction *fn, IrValue *target);
static int irFnLocalSlotNeeded(IrFunction *fn, IrValue *local);

/* Bind every variable known to the IR (params + alloca'd locals) to its
 * AST-driven loff. Walk the AST function's params and locals lists; the
 * eligibility predicate guarantees they're plain AST_LVAR ints. */
void irCgBindAstLoffs(IrRaCtx *ra, Ast *ast_func) {
    /* Hidden out-pointer param for struct-by-value return: the layout
     * pass stashed its loff on `ast_func->loff` and the IR-side
     * IR_VAL_PARAM lives on `ra->func->return_value`. */
    AstType *rt = ast_func->type ? ast_func->type->rettype : NULL;
    int has_hidden_out_ptr = rt &&
        (rt->kind == AST_TYPE_CLASS || rt->kind == AST_TYPE_UNION) &&
        !rt->is_intrinsic && rt->size > 0;

    if (has_hidden_out_ptr && ra->func->return_value) {
        irCgSetLoff(ra, irVarId(ra->func->return_value), ast_func->loff);
    }

    if (ast_func->params) {
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            if (p->kind == AST_VAR_ARGS) {
                IrValue *cv = irFnGetVar(ra->func, p->argc->lvar_id);
                if (cv) irCgSetLoff(ra, irVarId(cv), p->argc->loff);
                IrValue *vv = irFnGetVar(ra->func, p->argv->lvar_id);
                if (vv) irCgSetLoff(ra, irVarId(vv), p->argv->loff);
                continue;
            }

            IrValue *iv = irFnGetVar(ra->func, irGetParamId(p));
            /* Pinned param: no slot. */
            if (iv && iv->pinned_reg) continue;
            /* Unreferenced param: the layout pass assigned it no slot
             * (left p->loff at 0), so don't bind a bogus loff - it would
             * make backends think the slot exists and store over the
             * frame base. Must mirror the layout pass's skip. */
            if (iv && !irFnValueIsReferenced(ra->func, iv)) continue;
            if (iv) irCgSetLoff(ra, irVarId(iv), p->loff);
        }
    }

    listForEach(ast_func->locals) {
        Ast *l = (Ast *)it->value;
        IrValue *iv = irFnGetVar(ra->func, irGetParamId(l));
        /* Register-pinned locals have no slot - the codegen reads /
         * writes the named register directly. */
        if (iv && iv->pinned_reg) continue;
        /* Mirror the layout pass: a local that appears nowhere in the
         * optimised IR got no loff there, so don't bind a bogus one. */
        if (iv && !irFnLocalSlotNeeded(ra->func, iv)) continue;
        if (iv) irCgSetLoff(ra, irVarId(iv), l->loff);
    }
}

/* True if any instruction in fn references `target` as r1, r2, dst
 * (excluding the dst that defines it), or as a CALL arg. Used by the
 * layout pass to skip stack slots for params/locals whose stores
 * have been forwarded and DSE'd away. */
static int irFnValueIsReferenced(IrFunction *fn, IrValue *target) {
    if (!target) return 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (I->r1 == target) return 1;
            if (I->r2 == target) return 1;
            /* A pure-value def of target itself doesn't count; a
             * STORE/STORE_DEREF/CALL whose dst is target IS a use. */
            if (I->dst == target) {
                if (I->op == IR_STORE || I->op == IR_STORE_DEREF ||
                    I->op == IR_CALL)
                    return 1;
            }
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 k = 0; k < args->size; ++k) {
                    if ((IrValue *)args->entries[k] == target) return 1;
                }
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                Vec *pairs = I->extra.phi_pairs;
                for (u64 k = 0; k < pairs->size; ++k) {
                    IrPair *p = (IrPair *)pairs->entries[k];
                    if (p->ir_value == target) return 1;
                }
            }
        }
    }
    return 0;
}

/* True if `local` is mentioned ANYWHERE in fn's instruction stream:
 * as the dst of any op, r1, r2, idx, a CALL arg, a phi value, or a
 * branch/return value. Used to decide whether a local needs a stack
 * slot at all - after constant folding + store->read forwarding + DSE,
 * a `I64 x = 10;` whose every store and read has been removed appears
 * nowhere and its frame slot is pure waste.
 *
 * This is deliberately STRICTER than irFnValueIsReferenced (which is
 * fine for params, as they're never computed into): a local CAN be the
 * dst of a computational op (`iadd %local, a, b`), and DCE only prunes
 * dead *tmp* defs - never local ones - so such an op survives and its
 * codegen will store into the slot. Counting any dst appearance keeps
 * that slot alive and avoids an `ir-regalloc: no slot` panic. */
static int irFnLocalSlotNeeded(IrFunction *fn, IrValue *local) {
    if (!local) return 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (I->dst == local || I->r1 == local ||
                I->r2 == local || I->idx == local)
                return 1;
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 k = 0; k < args->size; ++k)
                    if ((IrValue *)args->entries[k] == local) return 1;
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                Vec *pairs = I->extra.phi_pairs;
                for (u64 k = 0; k < pairs->size; ++k) {
                    IrPair *p = (IrPair *)pairs->entries[k];
                    if (p->ir_value == local) return 1;
                }
            }
        }
    }
    return 0;
}

/* True if fn contains an IR_CALL. The x86_64 prologue uses this to
 * decide whether the leaf-function frame-elision is safe (a call needs
 * the entry rsp to be 8 mod 16, which the rbp push gives us; without
 * a frame we'd be 8-misaligned by the return addr alone). */
int irFnHasCalls(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_CALL) return 1;
        }
    }
    return 0;
}

int irCgComputeAstLayout(Ast *ast_func, IrFunction *ir_func) {
    int total = 0;
    int new_offset = 0;

    listForEach(ast_func->locals) {
        Ast *l = (Ast *)it->value;
        int size;
        if (l->kind == AST_DEFAULT_PARAM) {
            size = l->declvar->type->size;
        } else if (l->kind == AST_FUNPTR) {
            size = 8;
        } else {
            size = l->type->size;
        }
        IrValue *iv = irFnGetVar(ir_func, irGetParamId(l));
        /* Register-pinned local: no stack slot, no loff. */
        if (iv && iv->pinned_reg)
            continue;
        /* Local that no longer appears anywhere in the optimised IR
         * (e.g. `I64 x = 10;` folded + DSE'd away): skip its slot so it
         * doesn't bloat the frame. Must mirror the identical skip in
         * irCgBindAstLoffs so both agree on the surviving loffs. */
        if (iv && !irFnLocalSlotNeeded(ir_func, iv))
            continue;
        total += align(size, 8);
        new_offset -= size;
        l->loff = new_offset;
    }

    int param_total = 0;
    AstType *rt = ast_func->type ? ast_func->type->rettype : NULL;
    int has_hidden_out_ptr = rt &&
        (rt->kind == AST_TYPE_CLASS || rt->kind == AST_TYPE_UNION) &&
        !rt->is_intrinsic && rt->size > 0;
    if (has_hidden_out_ptr) {
        param_total += 8;
    }
    if (ast_func->params) {
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            /* TempleOS-pinned param has no stack slot. */
            if (p->kind == AST_LVAR &&
                p->pinned_kind == LVAR_REG &&
                p->pinned_reg) continue;
            /* Param whose slot is unreferenced after store-forwarding +
             * DSE: skip the slot too. VARGS is always live. */
            if (p->kind != AST_VAR_ARGS) {
                IrValue *iv = irFnGetVar(ir_func, irGetParamId(p));
                if (iv && !irFnValueIsReferenced(ir_func, iv)) continue;
            }
            int sz;
            if (p->kind == AST_FUNPTR) {
                sz = 8;
            } else if (p->kind == AST_VAR_ARGS) {
                /* Apple AArch64 reads argc straight from the caller's
                 * stack region, so no callee slot to reserve. */
                IrRegPool *pool = irRegPoolGet();
                if (pool && pool->variadic_on_stack) continue;
                sz = 8;
            } else {
                sz = p->type->size;
            }
            param_total += align(sz, 8);
        }
    }
    total += param_total;
    /* Each contribution is already 8-aligned, so `total` is too. The
     * 16-byte alignment requirement applies to the FINAL frame size
     * (handled by irFunctionPrepForCodeGen after tmps are added);
     * pre-aligning the locals+params region only here just adds a
     * dead gap between params and tmps. */
    int locals_aligned = total;

    int offset = locals_aligned;
    if (has_hidden_out_ptr) {
        /* Stash the loff on the AST function's `loff` field as a
         * side channel; both the prologue and irCgBindAstLoffs read
         * it back. AST_FUNC otherwise doesn't use this field. */
        ast_func->loff = -offset;
        offset -= 8;
    }
    if (ast_func->params) {
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            if (p->kind == AST_VAR_ARGS) {
                IrRegPool *pool = irRegPoolGet();
                if (pool && pool->variadic_on_stack) {
                    /* Apple AArch64: argc lives at the start of the
                     * caller's variadic stack region, argv just after. */
                    p->argc->loff = 16;
                    p->argv->loff = 24;
                } else {
                    p->argc->loff = -offset;
                    offset -= 8;
                    p->argv->loff = 16;
                }
                continue;
            }
            /* Pinned param: no slot, no loff. */
            if (p->kind == AST_LVAR &&
                p->pinned_kind == LVAR_REG &&
                p->pinned_reg) continue;
            /* Slot unreferenced (forwarded + DSE'd): no slot, no loff.
             * Must match the totalling-loop skip exactly. */
            IrValue *iv = irFnGetVar(ir_func, irGetParamId(p));
            if (iv && !irFnValueIsReferenced(ir_func, iv)) continue;
            int sz = (p->kind == AST_FUNPTR) ? 8 : p->type->size;
            p->loff = -offset;
            offset -= align(sz, 8);
        }
    }
    return locals_aligned;
}
