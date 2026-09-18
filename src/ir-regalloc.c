#include <assert.h>
#include <limits.h>
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
static int irFnValueIsReferenced(IrFunction *fn, IrValue *target);

static IrRegPool *g_reg_pool = NULL;

/* Set once from the CLI. A global rather than a threaded parameter for
 * the same reason g_reg_pool is one: the backends and the passes both
 * need it and neither owns the other. */
static int g_no_reg_promote = 0;

void irRegPromoteSetEnabled(int enabled) {
    g_no_reg_promote = !enabled;
}

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
        /* An internal error, but it should still say WHERE. Without the
         * function name there is nothing to bisect a large file with. */
        loggerPanic("ir-regalloc: no slot for var.id=%u kind=%d in `%s`\n",
                    id, val->kind,
                    (ra->func && ra->func->name) ? ra->func->name->data
                                                 : "<unknown>");
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
        /* An alloca nothing refers to needs no stack.
         *
         * Every value-returning function allocates a return slot, and
         * irForwardReturnSlot then routes the value past it - but the
         * reservation stayed, so `stack_space` was non-zero for
         * essentially every function and the frame-elision test could
         * never fire. A leaf function that touches no memory was still
         * paying `stp x29,x30 / mov x29,sp / sub sp / add sp / ldp`.
         *
         * Same reference test irCgComputeAstLayout already uses to drop
         * slots for locals the optimiser removed. */
        if (I->dst && !irFnValueIsReferenced(ra->func, I->dst)) return;

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
    /* How many times this tmp is MENTIONED - its definition plus every
     * read. What the allocator wants is the read count, which is this
     * minus one; see the callee-saved profitability test below. */
    int refs;
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
        r->refs   = 1;
        r->val    = v;
        mapAdd(ranges, (void *)(u64)id, (void *)r);
    } else {
        if (pos < r->first) r->first = pos;
        if (pos > r->last)  r->last  = pos;
        r->refs++;
    }
}

/* ---- automatic register promotion -----------------------------------
 *
 * A local or param that never has its address taken can live in a
 * register for the whole function instead of a stack slot. That is
 * exactly what a hand-written `<Type> reg <REG> name` does, so this
 * sets the same `pinned_reg` field and every path downstream - slot
 * elision in irCgComputeAstLayout, register reads and writes in the
 * backends, the callee-saved save/restore - already handles it.
 *
 * Whole-function binding, not live-range splitting: the register IS
 * the cell. Every store writes it, every load reads it, and a merge
 * point needs no phi because both predecessors wrote the same register
 * just as they previously wrote the same memory. That is why this needs
 * no SSA construction for locals, which are not in SSA form here - only
 * temporaries are.
 *
 * Callee-saved registers only. They survive a call by definition, so a
 * promoted value needs no spill around one - which is what keeps this
 * simple enough to be obviously correct. */

static void irLiveCollectEscaped(IrFunction *fn, Set *escaped);
static int irRegNameEq(AoStr *a, AoStr *b);
struct IrLiveness {
    Map *in;    /* Map<u32 block_id, Set<u32 var_id> *> */
    Map *out;   /* Map<u32 block_id, Set<u32 var_id> *> */
};
typedef struct IrLiveness IrLiveness;
static void irLivenessCompute(IrFunction *fn, IrLiveness *lv);
static void irLivenessRelease(IrLiveness *lv);
static int irLiveUnion(Set *dst, Set *src);
static void irLiveForEachRead(IrInstr *I, Set *out);
static void irLiveAddRef(Set *set, IrValue *v);
static int irLiveTracked(IrValue *v);
static int irLiveInstrDefines(IrInstr *I);

typedef struct IrPromoCand {
    IrValue *val;
    int uses;
    int is_float;
} IrPromoCand;

static int irPromoCandCmp(const void *a, const void *b) {
    const IrPromoCand *x = (const IrPromoCand *)a;
    const IrPromoCand *y = (const IrPromoCand *)b;
    if (x->uses != y->uses) return y->uses - x->uses;   /* hottest first */
    /* Stable on ties: var id keeps output deterministic across runs. */
    return (int)irVarId(x->val) - (int)irVarId(y->val);
}

static int irPromoTypeIsFloat(IrValue *v) {
    return v->type == IR_TYPE_F64 || v->type == IR_TYPE_F32;
}

/* `d8`-`d15` on AAPCS64, `xmm*` on SysV. A float candidate must not be
 * handed an integer register or the moves emitted for it are nonsense. */
static int irPromoRegIsFloat(AoStr *r) {
    if (!r || r->len == 0) return 0;
    if (r->data[0] == 'd' || r->data[0] == 'D') return 1;
    return r->len >= 3 && (r->data[0] == 'x' || r->data[0] == 'X') &&
           (r->data[1] == 'm' || r->data[1] == 'M');
}

/* Does this value fit the shape of something a register can hold? */
static int irPromoEligible(IrValue *v) {
    if (!v) return 0;
    if (v->kind != IR_VAL_LOCAL && v->kind != IR_VAL_PARAM) return 0;
    if (v->pinned_reg) return 0;              /* the source already chose */
    /* An incoming parameter already lives in its ABI arrival register;
     * ir.c marks it IR_LOC_REG during lowering. Promoting it burns a
     * second register that only ever gets saved and restored - the
     * value worth promoting is its home LOCAL, which is what the
     * arrival is stored into. */
    if (v->loc.kind == IR_LOC_REG) return 0;
    if (v->type == IR_TYPE_VOID) return 0;
    if (v->type == IR_TYPE_ASM_FUNCTION) return 0;
    /* Narrow locals ARE eligible.
     *
     * A local is a memory cell and its SLOT used to enforce its
     * declared width for free: `sturh w0` kept two bytes and the
     * matching load re-extended. A register does neither - `mov x13,
     * x0` keeps all 64 - so this once required a full-width value.
     *
     * The canonical form is now produced explicitly on every write to a
     * register home instead (aarch64StoreNarrowHome / its JIT twin),
     * which is what the signedness recorded on the value is for:
     * IrValueType gives the width, IR_VAL_FLAG_UNSIGNED the sign, and
     * between them they pick `sxth` from `uxth`.
     *
     * Floats are excluded - a narrow float in a register is an F32, not
     * a truncated F64, and nothing above canonicalises one. */
    u32 sz = irValueByteSize(v);
    if (sz != 8 && !irIsInt(v->type)) return 0;
    return 1;
}

/* Flag instructions whose result nothing reads.
 *
 * A call still has to happen, but storing its result into the
 * destination's home when no one will look is a wasted move. Backward
 * walk per block from live_out, the standard shape.
 *
 * Note what this does NOT catch: a value that IS read, where every read
 * happens to be satisfiable from the register it was produced in. That
 * looks like a dead store in the output but is a missing COALESCE - the
 * allocator gave the value a home it never needed - and pretending it
 * is dead would drop a live value. */
static void irMarkDeadDsts(IrFunction *fn, IrLiveness *lv) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        Set *live = setNew(32, &set_uint_type);
        Set *out = (Set *)mapGetInt(lv->out, bb->id);
        if (out) irLiveUnion(live, out);

        for (List *p = bb->instructions->prev; p != bb->instructions;
             p = p->prev)
        {
            IrInstr *I = (IrInstr *)p->value;
            if (I->op == IR_NOP) continue;

            if (I->dst && irLiveTracked(I->dst) && irLiveInstrDefines(I)) {
                void *k = (void *)(u64)irVarId(I->dst);
                if (!setHas(live, k)) I->flags |= IRCG_DST_DEAD;
                setRemove(live, k);
            }
            Set *reads = setNew(8, &set_uint_type);
            irLiveForEachRead(I, reads);
            irLiveUnion(live, reads);
            setRelease(reads);
        }
        setRelease(live);
    }
}

/* Values whose live range crosses a call.
 *
 * A value that dies before the first call - a parameter read once to
 * index an array, say - never needs a callee-saved register, even in a
 * function full of calls. Asking "is this a leaf function?" is too
 * coarse: it answers for the whole body when the question is about one
 * value's range.
 *
 * Conservative in two directions, both safe: a value live on entry to
 * or exit from any block containing a call counts as crossing, and so
 * does one referenced on both sides of a call within a block. A false
 * positive costs a callee-saved register instead of a free one; a false
 * negative would cost the value. */
static void irCollectCallCrossers(IrFunction *fn, IrLiveness *lv,
                                  Set *crossers)
{
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        int has_call = 0;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_CALL) { has_call = 1; break; }
        }
        if (!has_call) continue;

        Set *in  = (Set *)mapGetInt(lv->in,  bb->id);
        Set *out = (Set *)mapGetInt(lv->out, bb->id);
        if (in)  irLiveUnion(crossers, in);
        if (out) irLiveUnion(crossers, out);

        /* A struct-returning call keeps using its destination buffer
         * AFTER the call - the result registers are stored through it -
         * but the IR shows that address only as an argument, so its
         * last use looks like it is before the call. The emitter's use
         * is real and invisible here, so say so explicitly. */
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_CALL) continue;
            if (!(I->flags & IRCG_CALL_AGG_RETURN)) continue;
            if (!I->r1 || !I->r1->as.array.values) continue;
            Vec *cargs = I->r1->as.array.values;
            if (cargs->size == 0) continue;
            irLiveAddRef(crossers, (IrValue *)cargs->entries[0]);
        }

        /* Referenced on both sides of a call, within this block.
         *
         * Block-level liveness cannot see this: a straight-line
         * function is ONE block, so a counter updated around four calls
         * is neither live-in nor live-out anywhere - it is born and
         * dies inside the block. That is the common shape for `main`.
         *
         * Done as first/last reference positions rather than a
         * "have we passed a call yet" flag. The flag latched: nothing
         * was recorded after the first call, so a value first defined
         * there - `correct += f()` with no separate initialiser - was
         * never seen to cross the calls that followed it. */
        Map *first = mapNew(32, &map_uint_to_uint_type);
        Map *last  = mapNew(32, &map_uint_to_uint_type);
        int *call_pos = NULL;
        int n_calls = 0, cap_calls = 0;
        int pos = 0;

        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            pos++;
            if (I->op == IR_CALL) {
                if (n_calls == cap_calls) {
                    cap_calls = cap_calls ? cap_calls * 2 : 8;
                    call_pos = (int *)realloc(call_pos,
                                              cap_calls * sizeof(int));
                }
                call_pos[n_calls++] = pos;
                /* A call's own operands are read AT the call, so they
                 * are recorded below like any other reference. */
            }
            Set *refs = setNew(8, &set_uint_type);
            irLiveForEachRead(I, refs);
            irLiveAddRef(refs, I->dst);
            SetIter rit;
            setIterInit(refs, &rit);
            while (setIterNext(&rit)) {
                void *k = rit.value;
                if (!mapHas(first, k)) mapAdd(first, k, (void *)(u64)pos);
                mapRemove(last, k);
                mapAdd(last, k, (void *)(u64)pos);
            }
            setRelease(refs);
        }

        MapIter fit;
        mapIterInit(first, &fit);
        while (mapIterNext(&fit)) {
            void *k = fit.node->key;
            int f = (int)(u64)fit.node->value;
            int l = (int)(u64)mapGet(last, k);
            for (int c = 0; c < n_calls; ++c) {
                if (call_pos[c] > f && call_pos[c] < l) {
                    setAdd(crossers, k);
                    break;
                }
            }
        }
        free(call_pos);
        mapRelease(first);
        mapRelease(last);
    }
}

/* Is `reg` bound by a source-level `<Type> reg <REG> name` anywhere in
 * this function? */
static int irRegIsHandPinned(IrFunction *fn, AoStr *reg) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            IrValue *ops[4];
            ops[0] = I->dst; ops[1] = I->r1; ops[2] = I->r2; ops[3] = I->idx;
            for (int k = 0; k < 4; ++k) {
                if (ops[k] && ops[k]->pinned_reg &&
                    irRegNameEq(ops[k]->pinned_reg, reg))
                {
                    return 1;
                }
            }
        }
    }
    return 0;
}

/* ---- ABI argument registers as allocatable storage -------------------
 *
 * An argument register is caller-saved, so a value that never crosses a
 * call could live in one for free - the same bargain leaf_free_regs
 * exists to make. Two things stop it, and both are properties of the
 * ABI rather than of any one backend, which is why the reasoning lives
 * here rather than in x86_64.c or aarch64.c:
 *
 *   INCOMING   a parameter ARRIVES in one of these registers and is
 *              read out of it. Handing that register to something else
 *              destroys the parameter.
 *
 *   OUTGOING   the argument-loading sequence writes them in ABI order,
 *              so a value living in argument register K that is itself
 *              argument J (J placed after K) is gone by the time it is
 *              read. Note such a value is NOT a "call crosser" - it
 *              dies AT the call - so irCollectCallCrossers does not
 *              catch it.
 *
 * Both tests below are deliberately whole-function and conservative.
 * The precise versions want parameter live ranges in the interference
 * graph, which is a bigger change; these give most of the benefit for
 * a fraction of the risk, and err only towards refusing a register.
 *
 * Every test is gated on the register actually BEING an argument
 * register, so a pool whose leaf-free list contains none of them - as
 * aarch64's x13/x14/x15 does - is unaffected. */
/* Does a value in `reg` survive a call? True only for the callee-saved
 * pool; the "leaf free" registers are caller-saved and a call may
 * write them. */
int irRegSurvivesCall(IrRegPool *pool, AoStr *reg) {
    if (!pool || !reg || !pool->callee_saved_regs) return 0;
    for (u64 i = 0; i < pool->callee_saved_regs->size; ++i) {
        if (irRegNameEq((AoStr *)pool->callee_saved_regs->entries[i], reg))
            return 1;
    }
    return 0;
}

int irRegIsAbiArg(IrRegPool *pool, AoStr *reg) {
    if (!pool || !reg) return 0;
    Vec *lists[2];
    lists[0] = pool->int_arg_regs;
    lists[1] = pool->float_arg_regs;
    for (int L = 0; L < 2; ++L) {
        if (!lists[L]) continue;
        for (u64 i = 0; i < lists[L]->size; ++i) {
            if (irRegNameEq((AoStr *)lists[L]->entries[i], reg)) return 1;
        }
    }
    return 0;
}

/* INCOMING: does a parameter arrive in `reg` and get read from it?
 *
 * "Read anywhere" rather than "read up to position N" on purpose. A
 * linear last-use position is unsound around a loop: a parameter read
 * at the top of a loop body is read again after the back edge, so a
 * range that starts textually later can still overlap it. Reserving
 * the register for the whole function sidesteps that without needing
 * parameter ranges in the liveness sets. */
static int irRegHoldsReadParam(IrFunction *fn, AoStr *reg) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            IrValue *ops[3];
            ops[0] = I->r1; ops[1] = I->r2; ops[2] = I->idx;
            for (int k = 0; k < 3; ++k) {
                IrValue *v = ops[k];
                if (!v || v->kind != IR_VAL_PARAM) continue;
                if (v->loc.kind != IR_LOC_REG || !v->loc.as.reg) continue;
                if (irRegNameEq(v->loc.as.reg, reg)) return 1;
            }
            /* IR_BR / IR_RET carry their value operand in dst. */
            if ((I->op == IR_BR || I->op == IR_RET) && I->dst &&
                I->dst->kind == IR_VAL_PARAM &&
                I->dst->loc.kind == IR_LOC_REG && I->dst->loc.as.reg &&
                irRegNameEq(I->dst->loc.as.reg, reg))
            {
                return 1;
            }
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *cargs = I->r1->as.array.values;
                for (u64 i = 0; i < cargs->size; ++i) {
                    IrValue *v = (IrValue *)cargs->entries[i];
                    if (!v || v->kind != IR_VAL_PARAM) continue;
                    if (v->loc.kind != IR_LOC_REG || !v->loc.as.reg) continue;
                    if (irRegNameEq(v->loc.as.reg, reg)) return 1;
                }
            }
        }
    }
    return 0;
}

/* OUTGOING: which values are handed to a call as arguments. */
void irCollectCallArgValues(IrFunction *fn, Set *out) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_CALL) continue;
            if (!I->r1 || !I->r1->as.array.values) continue;
            Vec *cargs = I->r1->as.array.values;
            for (u64 i = 0; i < cargs->size; ++i) {
                irLiveAddRef(out, (IrValue *)cargs->entries[i]);
            }
            /* An indirect call's target is materialised the same way
             * and lands in the same hazard. */
            if (I->r2) irLiveAddRef(out, I->r2);
        }
    }
}

void irOptPromoteToRegs(IrFunction *fn) {
    if (g_no_reg_promote) return;
    IrRegPool *pool = irRegPoolGet();
    if (!pool || !pool->callee_saved_regs ||
        pool->callee_saved_regs->size == 0)
    {
        return;
    }

    /* A variadic function does not own its parameter homes. `argc` and
     * `argv` live in the CALLER's frame - [x29,#16] and [x29,#24] - and
     * the callee reads them there; nothing in the callee ever writes
     * them. The lowering does emit `store %l231, %p230`, but both names
     * resolve to the same caller-side slot, so it is a self-assignment
     * that never reaches the output - which is why asking whether the
     * IR defines the local says yes and means nothing.
     *
     * Rather than special-case argc, decline the whole function: its
     * parameter area is somebody else's memory. */
    if (fn->has_var_args) return;

    /* An `asm { }` block can write any register it likes, and nothing
     * tells us which. Promoting anything in such a function risks the
     * block quietly destroying a value that now lives in a register
     * rather than a slot. Whole-function opt-out is the only honest
     * rule until asm blocks declare their clobbers. */
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_ASM) return;
        }
    }

    /* Address-taken values must stay addressable. Reuses the same
     * IR_LEA / IR_GEP sweep liveness uses to decide what it cannot
     * reason about. */
    Set *escaped = setNew(16, &set_uint_type);
    irLiveCollectEscaped(fn, escaped);

    /* Only promote a local the IR actually WRITES.
     *
     * If no instruction defines it, its value arrives by a mechanism
     * tied to its address, and moving it to a register removes the only
     * thing that would have filled it. Two shapes do this, and neither
     * emits a store:
     *
     *   - a parameter past the register quota, which the backend's
     *     stack-param prologue copies out of the caller's frame with
     *     `ldr x10,[x29,#16]; str x10,<slot>`;
     *   - `argc` on Apple AArch64, where every variadic argument is
     *     passed on the stack and the slot is, in the lowering's own
     *     words, "just a view of the caller's stack region".
     *
     * Asking "does the IR define this?" covers both without naming
     * either. */
    Set *defined = setNew(16, &set_uint_type);
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP || !I->dst) continue;
            /* These write THROUGH dst, they do not define it. */
            if (I->op == IR_STORE_DEREF || I->op == IR_RMW_DEREF) continue;
            if (I->dst->kind != IR_VAL_LOCAL) continue;
            setAdd(defined, (void *)(u64)irVarId(I->dst));
        }
    }

    Map *seen = mapNew(32, &map_uint_to_uint_type);
    IrPromoCand *cands = NULL;
    u64 n = 0, cap = 0;

    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            IrValue *ops[4];
            ops[0] = I->dst; ops[1] = I->r1; ops[2] = I->r2; ops[3] = I->idx;
            for (int k = 0; k < 4; ++k) {
                IrValue *v = ops[k];
                if (!irPromoEligible(v)) continue;
                u32 id = irVarId(v);
                if (setHas(escaped, (void *)(u64)id)) continue;
                if (v->kind == IR_VAL_LOCAL &&
                    !setHas(defined, (void *)(u64)id))
                {
                    continue;
                }
                IrPromoCand *c = (IrPromoCand *)mapGetInt(seen, id);
                if (c) { c->uses++; continue; }
                if (n == cap) {
                    cap = cap ? cap * 2 : 16;
                    cands = (IrPromoCand *)realloc(cands,
                            cap * sizeof(IrPromoCand));
                    /* Re-seat the map: realloc may have moved them. */
                    mapRelease(seen);
                    seen = mapNew(32, &map_uint_to_uint_type);
                    for (u64 q = 0; q < n; ++q) {
                        mapAdd(seen, (void *)(u64)irVarId(cands[q].val),
                               &cands[q]);
                    }
                }
                cands[n].val = v;
                cands[n].uses = 1;
                cands[n].is_float = irPromoTypeIsFloat(v);
                mapAdd(seen, (void *)(u64)id, &cands[n]);
                n++;
            }
        }
    }
    mapRelease(seen);
    setRelease(escaped);
    setRelease(defined);
    if (n == 0) { free(cands); return; }

    qsort(cands, n, sizeof(IrPromoCand), irPromoCandCmp);

    /* A promoted value in a callee-saved register costs a store in the
     * prologue and a load in the epilogue, unconditionally. It saves
     * roughly one memory access per use. Below the threshold the trade
     * is a loss - and it still consumes a register and frame bytes that
     * a hotter value could have used.
     *
     * Static use count under-reads a loop body, where one textual use
     * runs many times. Until there is loop-depth information to weight
     * with, the threshold is deliberately low so a loop-carried value
     * with two or three uses still qualifies. */
    const int kMinUses = 3;
    int have_free = pool->leaf_free_regs && pool->leaf_free_regs->size > 0;

    /* Which candidates outlive a call decides both the pool they draw
     * from and whether the floor applies at all. */
    Set *crossers = setNew(16, &set_uint_type);
    if (irFnHasCalls(fn)) {
        IrLiveness lv;
        irLivenessCompute(fn, &lv);
        irCollectCallCrossers(fn, &lv, crossers);
        irLivenessRelease(&lv);
    }

    /* Hand out registers hottest-first. Integer and float candidates
     * draw from disjoint parts of the pool, so a float never lands in
     * an x register.
     *
     * A register the source already pinned by hand is spoken for. The
     * candidate holding it is skipped above, but the REGISTER has to
     * come out of the pool as well or a promoted value lands on top of
     * it - silently, since both write the same name. */
    /* Which values are handed to a call as arguments - they may not
     * take an ABI argument register. See irCollectCallArgValues. */
    Set *call_args = setNew(16, &set_uint_type);
    irCollectCallArgValues(fn, call_args);

    /* Separate free-list cursors per class: the list mixes x and d
     * registers, and one shared cursor let a float candidate taking
     * d16 strand the x registers before it for every later int
     * candidate. The classes are disjoint, so two cursors over the
     * same list cannot double-allocate. */
    u64 int_next = 0, flt_next = 0, free_int_next = 0, free_flt_next = 0;
    for (u64 i = 0; i < n; ++i) {
        /* A value that never crosses a call costs nothing to keep in a
         * caller-saved register - no save, no restore - so the floor,
         * which exists to pay for exactly that, does not apply to it.
         * Asking per value rather than per function is what lets a
         * parameter read once before the first call stop living in a
         * stack slot, in a function that calls plenty of things. */
        int crosses = setHas(crossers, (void *)(u64)irVarId(cands[i].val));
        int use_free = have_free && !crosses;
        if (!use_free && cands[i].uses < kMinUses) continue;
        int is_call_arg =
            setHas(call_args, (void *)(u64)irVarId(cands[i].val));

        AoStr *chosen = NULL;
        Vec *from = use_free ? pool->leaf_free_regs : pool->callee_saved_regs;
        u64 *cursor = use_free
                    ? (cands[i].is_float ? &free_flt_next : &free_int_next)
                    : (cands[i].is_float ? &flt_next : &int_next);
        for (u64 r = *cursor; r < from->size; ++r) {
            AoStr *cand = (AoStr *)from->entries[r];
            if (irPromoRegIsFloat(cand) != cands[i].is_float) continue;
            if (irRegIsHandPinned(fn, cand)) continue;
            if (irRegIsAbiArg(pool, cand)) {
                if (is_call_arg) continue;
                if (irRegHoldsReadParam(fn, cand)) continue;
            }
            chosen = cand;
            *cursor = r + 1;
            break;
        }
        if (!chosen) continue;  /* pool exhausted; this one keeps its slot */
        cands[i].val->pinned_reg = chosen;
    }
    setRelease(call_args);
    setRelease(crossers);
    free(cands);
}

/* ---- liveness -------------------------------------------------------
 *
 * Backward dataflow over the IR's own CFG (ir.c's block mappings; the
 * old cfg.c is gone):
 *
 *   live_out[B] = U live_in[S] for every successor S of B
 *   live_in[B]  = use[B] U (live_out[B] \ def[B])
 *
 * iterated to a fixpoint. `use[B]` is the UPWARD-EXPOSED uses only -
 * read before being written in B - or a value that B both defines and
 * reads would look live on entry and pin a register through every
 * predecessor.
 *
 * Why this exists: slot ranges used to be [min, max] of an
 * instruction's position in block-list order, which equals real
 * liveness only while every value lives and dies inside one block.
 * That holds for IR tmps today, so nothing was wrong - but it is a
 * property of how naively we lower, not something the allocator can
 * rely on, and it breaks the moment anything spans a loop. In
 * `for (i = 0; i < n; ++i)` the local `n` is read once, in the header:
 * its positional range stops there, and anything defined in the body
 * sits at a higher position and passes the "does not overlap" test.
 * The body would take n's storage and the second iteration would read
 * whatever replaced it. */


/* Everything liveness tracks: SSA temporaries AND the memory-resident
 * locals and params.
 *
 * Only tmps are in SSA - 288 phis across the test suite, not one of
 * them touching a local - so a local is a cell that is stored to
 * repeatedly rather than a versioned value. Liveness works on it just
 * the same: a store to a scalar local fully overwrites it, so it kills
 * exactly like an SSA def. */
static int irLiveTracked(IrValue *v) {
    if (!v) return 0;
    if (v->type == IR_TYPE_VOID) return 0;
    return v->kind == IR_VAL_TMP || v->kind == IR_VAL_LOCAL ||
           v->kind == IR_VAL_PARAM;
}

static void irLiveAddRef(Set *set, IrValue *v) {
    if (!irLiveTracked(v)) return;
    setAdd(set, (void *)(u64)irVarId(v));
}

/* Every value an instruction READS. Kept in one place so a new operand
 * field cannot be added to IrInstr and silently skipped here - a missed
 * read makes a value look dead and hands its register to someone
 * else. */
static void irLiveForEachRead(IrInstr *I, Set *out) {
    irLiveAddRef(out, I->r1);
    irLiveAddRef(out, I->r2);
    irLiveAddRef(out, I->idx);
    if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
        Vec *args = I->r1->as.array.values;
        for (u64 i = 0; i < args->size; ++i) {
            irLiveAddRef(out, (IrValue *)args->entries[i]);
        }
    }
    /* Four ops keep an OPERAND in `dst` rather than a result.
     * STORE_DEREF and RMW_DEREF read it as an address; IR_BR reads it as
     * the branch condition and IR_RET as the value being returned. All
     * four are uses, and calling any of them a definition ends the
     * value's live range one instruction early - so the producer's home
     * store is elided as dead while the consumer still reads that home,
     * and the branch tests whatever was left in the register.
     *
     * Only IR_BR/IR_RET whose operand is a temporary are affected in
     * practice, which is why this stayed hidden: a call result went
     * straight into the branch and the call's spill is unconditional.
     * Inlining put an ordinary `load` there instead. */
    if (I->op == IR_STORE_DEREF || I->op == IR_RMW_DEREF ||
        I->op == IR_BR || I->op == IR_RET)
    {
        irLiveAddRef(out, I->dst);
    }
}

static int irLiveInstrDefines(IrInstr *I) {
    if (!I->dst) return 0;
    switch (I->op) {
    /* See irLiveForEachRead: `dst` is an operand on these, not a
     * result. The two functions must agree or a value is either
     * resurrected or killed early. */
    case IR_STORE_DEREF:
    case IR_RMW_DEREF:
    case IR_BR:
    case IR_RET:
        return 0;
    default:
        return 1;
    }
}

static Set *irLiveSetFor(Map *m, u32 id) {
    Set *s = (Set *)mapGetInt(m, id);
    if (!s) {
        s = setNew(16, &set_uint_type);
        mapAdd(m, (void *)(u64)id, s);
    }
    return s;
}

/* Add everything in `src` to `dst`; returns 1 if `dst` grew. */
static int irLiveUnion(Set *dst, Set *src) {
    int changed = 0;
    SetIter it;
    setIterInit(src, &it);
    while (setIterNext(&it)) {
        void *k = it.value;
        if (!setHas(dst, k)) {
            setAdd(dst, k);
            changed = 1;
        }
    }
    return changed;
}

/* A local whose address is taken can be written through a pointer, by
 * an instruction that never names it. Liveness cannot see those writes,
 * so any conclusion that such a local is dead is unfounded - it is held
 * live everywhere instead. Costs nothing real: an escaped local is
 * excluded from register promotion anyway, and this stops a future
 * consumer drawing a wrong conclusion from a set that never modelled
 * it. */
static void irLiveCollectEscaped(IrFunction *fn, Set *escaped) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_LEA && I->op != IR_GEP) continue;
            irLiveAddRef(escaped, I->r1);
            irLiveAddRef(escaped, I->r2);
        }
    }
}

static void irLivenessCompute(IrFunction *fn, IrLiveness *lv) {
    lv->in  = mapNew(32, &map_uint_to_uint_type);
    lv->out = mapNew(32, &map_uint_to_uint_type);

    /* Per-block def and upward-exposed use, computed once. */
    Map *defs = mapNew(32, &map_uint_to_uint_type);
    Map *uses = mapNew(32, &map_uint_to_uint_type);

    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        Set *def = irLiveSetFor(defs, bb->id);
        Set *use = irLiveSetFor(uses, bb->id);
        Set *reads = setNew(16, &set_uint_type);

        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            /* A phi's operands are used on the EDGE from the matching
             * predecessor, not here - crediting them to this block
             * would make each one live along every incoming edge. They
             * are added to the predecessors' live_out below. */
            if (I->op == IR_PHI) {
                if (I->dst) irLiveAddRef(def, I->dst);
                continue;
            }
            setRelease(reads);
            reads = setNew(16, &set_uint_type);
            irLiveForEachRead(I, reads);
            SetIter rit;
            setIterInit(reads, &rit);
            while (setIterNext(&rit)) {
                if (!setHas(def, rit.value)) setAdd(use, rit.value);
            }
            if (irLiveInstrDefines(I)) irLiveAddRef(def, I->dst);
        }
        setRelease(reads);
        irLiveSetFor(lv->in, bb->id);
        irLiveSetFor(lv->out, bb->id);
    }

    /* A phi operand is live out of the predecessor it arrives from. */
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (I->op != IR_PHI) break;
            if (!I->extra.phi_pairs) continue;
            for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                IrPair *pr = (IrPair *)I->extra.phi_pairs->entries[i];
                if (!pr || !pr->ir_block) continue;
                irLiveAddRef(irLiveSetFor(lv->out, pr->ir_block->id),
                             pr->ir_value);
                /* and live on entry here, so it reaches the phi */
                irLiveAddRef(irLiveSetFor(uses, bb->id), pr->ir_value);
            }
        }
    }

    /* Fixpoint. Blocks are walked in reverse list order, which is a
     * decent approximation of reverse post-order and just means fewer
     * rounds - the result does not depend on it. */
    int changed = 1;
    while (changed) {
        changed = 0;
        for (List *ln = fn->blocks->prev; ln != fn->blocks;
             ln = ln->prev)
        {
            IrBlock *bb = (IrBlock *)ln->value;
            Set *live_out = irLiveSetFor(lv->out, bb->id);
            Map *succs = irBlockGetSuccessors(fn, bb);
            if (succs) {
                MapIter sit;
                mapIterInit(succs, &sit);
                while (mapIterNext(&sit)) {
                    IrBlock *S = (IrBlock *)sit.node->value;
                    if (!S) continue;
                    changed |= irLiveUnion(live_out,
                                           irLiveSetFor(lv->in, S->id));
                }
            }

            /* live_in = use U (live_out \ def) */
            Set *live_in = irLiveSetFor(lv->in, bb->id);
            Set *def = irLiveSetFor(defs, bb->id);
            changed |= irLiveUnion(live_in, irLiveSetFor(uses, bb->id));
            SetIter oit;
            setIterInit(live_out, &oit);
            while (setIterNext(&oit)) {
                void *k = oit.value;
                if (setHas(def, k)) continue;
                if (!setHas(live_in, k)) {
                    setAdd(live_in, k);
                    changed = 1;
                }
            }
        }
    }

    Set *escaped = setNew(16, &set_uint_type);
    irLiveCollectEscaped(fn, escaped);
    if (escaped->size) {
        listForEach(fn->blocks) {
            IrBlock *bb = (IrBlock *)it->value;
            irLiveUnion(irLiveSetFor(lv->in,  bb->id), escaped);
            irLiveUnion(irLiveSetFor(lv->out, bb->id), escaped);
        }
    }
    setRelease(escaped);

    MapIter mit;
    mapIterInit(defs, &mit);
    while (mapIterNext(&mit)) setRelease((Set *)mit.node->value);
    mapRelease(defs);
    mapIterInit(uses, &mit);
    while (mapIterNext(&mit)) setRelease((Set *)mit.node->value);
    mapRelease(uses);
}

static void irLivenessRelease(IrLiveness *lv) {
    MapIter it;
    mapIterInit(lv->in, &it);
    while (mapIterNext(&it)) setRelease((Set *)it.node->value);
    mapRelease(lv->in);
    mapIterInit(lv->out, &it);
    while (mapIterNext(&it)) setRelease((Set *)it.node->value);
    mapRelease(lv->out);
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

    IrLiveness lv;
    irLivenessCompute(ra->func, &lv);

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
    /* Round every range out to the blocks the value is actually live
     * in. A positional [min, max] is only equal to the live range while
     * a value lives and dies in one block; a value carried round a loop
     * is live at points OUTSIDE that window - after its last textual
     * read and before the back edge - and reusing its storage there
     * corrupts the next iteration.
     *
     * Whole-block granularity over-approximates (a value counts as live
     * in any block between its first and last live one, even where it
     * is dead) which only ever wastes storage. It needs no assumption
     * about block order: every point a value is live at lies inside
     * some block it is live in, so it lies inside the widened range. */
    {
        int bpos = 0;
        listForEach(ra->func->blocks) {
            IrBlock *bb = (IrBlock *)it->value;
            int start = bpos;
            listForEach(bb->instructions) {
                IrInstr *I = (IrInstr *)it->value;
                if (I->op == IR_NOP) continue;
                bpos++;
            }
            int end = bpos;

            Set *live[2];
            live[0] = (Set *)mapGetInt(lv.in,  bb->id);
            live[1] = (Set *)mapGetInt(lv.out, bb->id);
            for (int k = 0; k < 2; ++k) {
                if (!live[k]) continue;
                SetIter sit;
                setIterInit(live[k], &sit);
                while (setIterNext(&sit)) {
                    u32 id = (u32)(u64)sit.value;
                    IrCgRange *r = (IrCgRange *)mapGetInt(ranges, id);
                    if (!r) continue;
                    if (start < r->first) r->first = start;
                    if (end   > r->last)  r->last  = end;
                }
            }
        }
    }
    /* Which tmps outlive a call, computed while liveness is still in
     * hand. Promotion already asks this question per value; the tmp
     * allocator was still asking the coarser "is this a leaf
     * function?", so in any function with a call EVERY temporary drew
     * from the callee-saved pool and paid a save/restore pair - even
     * one produced and consumed entirely on one side of the call. */
    Set *tmp_crossers = setNew(16, &set_uint_type);
    if (irFnHasCalls(ra->func)) {
        irCollectCallCrossers(ra->func, &lv, tmp_crossers);
    }
    irMarkDeadDsts(ra->func, &lv);
    irLivenessRelease(&lv);

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

    /* Registers a tmp may live in: the callee-saved pool, minus
     * anything a source `reg <REG>` or promotion already claimed.
     * Callee-saved so a tmp whose range spans a call needs no spill -
     * the same reason promotion uses them.
     *
     * This is the free list the whole exercise was for. Without it
     * every SSA temporary round-trips through memory, so `a + b + c`
     * costs six accesses for three values that arrived in registers. */
    /* A tmp whose address is taken needs somewhere to point AT. The
     * same IR_LEA / IR_GEP sweep promotion uses - it was only applied
     * to locals before, and a tmp feeding an IR_LEA reached codegen
     * with a register and no slot. */
    Set *tmp_escaped = setNew(16, &set_uint_type);
    irLiveCollectEscaped(ra->func, tmp_escaped);

    /* -fno-reg-promote turns off ALL register homing, tmps included -
     * a bisect switch that only disabled half of it would not tell you
     * whether registers are implicated. */
    IrRegPool *rpool = g_no_reg_promote ? NULL : irRegPoolGet();
    AoStr **regs = NULL;
    int *reg_last = NULL;
    int n_regs = 0;
    int *reg_survives = NULL;   /* 1 = safe for a value crossing a call */
    int *reg_abi_arg  = NULL;   /* 1 = an ABI argument register        */
    /* Values handed to a call as arguments; they may not take an ABI
     * argument register, because the argument-loading sequence writes
     * those in order. See irCollectCallArgValues. */
    Set *tmp_call_args = setNew(16, &set_uint_type);
    irCollectCallArgValues(ra->func, tmp_call_args);
    if (rpool) {
        /* Free registers first, callee-saved second: the former cost
         * nothing, the latter a save/restore pair, so exhaust the free
         * ones before spending. Which of them a given value may take
         * depends on whether it crosses a call - recorded per register
         * in `reg_survives`. */
        Vec *lists[2];
        lists[0] = rpool->leaf_free_regs;      /* caller-saved: free */
        lists[1] = rpool->callee_saved_regs;   /* survive a call      */
        u64 total = (lists[0] ? lists[0]->size : 0) +
                    (lists[1] ? lists[1]->size : 0);
        if (total) {
            regs = (AoStr **)calloc(total, sizeof(AoStr *));
            reg_last = (int *)calloc(total, sizeof(int));
            reg_survives = (int *)calloc(total, sizeof(int));
            reg_abi_arg  = (int *)calloc(total, sizeof(int));
            for (int L = 0; L < 2; ++L) {
                if (!lists[L]) continue;
                for (u64 i = 0; i < lists[L]->size; ++i) {
                    AoStr *cand = (AoStr *)lists[L]->entries[i];
                    if (irRegIsHandPinned(ra->func, cand)) continue;
                    int is_arg = irRegIsAbiArg(rpool, cand);
                    /* A parameter arrives in this register and is read
                     * out of it - the register is spoken for, for the
                     * whole function. Dropped from the pool entirely
                     * rather than tested per range: reserving it only
                     * up to the parameter's last read is unsound around
                     * a loop. */
                    if (is_arg && irRegHoldsReadParam(ra->func, cand))
                        continue;
                    regs[n_regs] = cand;
                    reg_last[n_regs] = INT_MIN;   /* free from the start */
                    reg_survives[n_regs] = (L == 1);
                    reg_abi_arg[n_regs] = is_arg;
                    n_regs++;
                }
            }
        }
    }

    for (u64 i = 0; i < n; ++i) {
        IrCgRange *r = &arr[i];

        /* Try a register before a slot. Same non-overlap test the slot
         * list uses, against the ranges liveness widened. */
        if (n_regs > 0 && r->val &&
            !setHas(tmp_escaped, (void *)(u64)r->var_id))
        {
            int want_float = irPromoTypeIsFloat(r->val);
            u32 sz = irValueByteSize(r->val);
            if (sz > 0 && sz <= 8) {
                int needs_survivor =
                    setHas(tmp_crossers, (void *)(u64)r->var_id);
                /* A CALLEE-SAVED register has to be written in the
                 * prologue and read back in the epilogue whatever it
                 * holds, so it only repays that pair if the tmp it
                 * holds would otherwise have done more memory traffic
                 * than that: one store at the definition plus one load
                 * per read. Two reads break even, so ask for two.
                 *
                 * A caller-saved (leaf-free) register costs nothing and
                 * is handed out regardless - which is why this test is
                 * on `reg_survives` rather than on the tmp.
                 *
                 * Without it, `int add(int a, int b)` put both of its
                 * single-read temporaries in callee-saved registers and
                 * paid six instructions - a frame, two saves and two
                 * restores - to avoid four memory accesses.
                 *
                 * irOptPromoteToRegs applies the same reasoning to
                 * locals and params via kMinUses; the tmp allocator had
                 * no threshold at all. */
                int reads = r->refs - 1;
                int is_call_arg =
                    setHas(tmp_call_args, (void *)(u64)r->var_id);
                for (int k = 0; k < n_regs; ++k) {
                    if (irPromoRegIsFloat(regs[k]) != want_float) continue;
                    if (needs_survivor && !reg_survives[k]) continue;
                    if (reg_survives[k] && reads < 2) continue;
                    /* Handed to a call as an argument: the loading
                     * sequence writes the argument registers in ABI
                     * order, so one of them cannot also be where this
                     * value is waiting. */
                    if (reg_abi_arg[k] && is_call_arg) continue;
                    if (reg_last[k] >= r->first) continue;
                    reg_last[k] = r->last;
                    r->val->loc.kind   = IR_LOC_REG;
                    r->val->loc.as.reg = regs[k];
                    goto next_range;
                }
            }
        }

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
next_range: ;
    }
    setRelease(tmp_escaped);
    setRelease(tmp_crossers);
    free(regs);
    free(reg_last);
    free(reg_survives);
    free(reg_abi_arg);
    setRelease(tmp_call_args);

    mapIterInit(ranges, &it_r);
    while (mapIterNext(&it_r)) {
        free(it_r.node->value);
    }
    mapRelease(ranges);
    free(arr);
    if (slot_loff) free(slot_loff);
    if (slot_last) free(slot_last);
}


/* Case-insensitive register-name compare. `pinned_reg` is whatever the
 * source wrote - `reg X19` emits `X19` verbatim - while the pool spells
 * its registers in lowercase. */
static int irRegNameEq(AoStr *a, AoStr *b) {
    if (!a || !b || a->len != b->len) return 0;
    for (u64 i = 0; i < a->len; ++i) {
        char ca = a->data[i], cb = b->data[i];
        if (ca >= 'A' && ca <= 'Z') ca = (char)(ca - 'A' + 'a');
        if (cb >= 'A' && cb <= 'Z') cb = (char)(cb - 'A' + 'a');
        if (ca != cb) return 0;
    }
    return 1;
}

Vec *irCgPinnedCalleeSaved(IrFunction *fn) {
    Vec *out = vecNew(&vec_aostr_type);
    IrRegPool *pool = irRegPoolGet();
    if (!pool || !pool->callee_saved_regs) return out;

    for (u64 i = 0; i < pool->callee_saved_regs->size; ++i) {
        AoStr *cs = (AoStr *)pool->callee_saved_regs->entries[i];
        int used = 0;
        listForEach(fn->blocks) {
            IrBlock *bb = (IrBlock *)it->value;
            listForEach(bb->instructions) {
                IrInstr *I = (IrInstr *)it->value;
                IrValue *vals[3];
                vals[0] = I->dst; vals[1] = I->r1; vals[2] = I->r2;
                for (int k = 0; k < 3 && !used; ++k) {
                    IrValue *v = vals[k];
                    if (!v) continue;
                    /* Both ways a value can name a register: a source
                     * `reg <REG>` / promotion (pinned_reg) and the tmp
                     * allocator (loc.as.reg). Missing either leaves a
                     * callee-saved register written and not restored. */
                    if (v->pinned_reg && irRegNameEq(v->pinned_reg, cs)) {
                        used = 1;
                    } else if (v->loc.kind == IR_LOC_REG && v->loc.as.reg &&
                               irRegNameEq(v->loc.as.reg, cs)) {
                        used = 1;
                    }
                }
                if (used) break;
            }
            if (used) break;
        }
        if (used) vecPush(out, cs);
    }
    return out;
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
    /* Must match irRetIsIndirect in ir.c: only an aggregate LARGER than
     * 16 bytes gets a hidden out-pointer. Anything at or below that is
     * returned in registers and needs a full-size buffer, not an 8-byte
     * pointer slot.
     *
     * Without the size check the layout reserved 8 bytes and bound the
     * return value to them, so a 16-byte struct sat in an 8-byte slot
     * and its second word landed on whatever came next. That was the
     * dead tail of the parameter area, which is why it never showed -
     * until promotion moved the parameters into registers and the
     * overrun reached the saved frame pointer. */
    int has_hidden_out_ptr = rt &&
        (rt->kind == AST_TYPE_CLASS || rt->kind == AST_TYPE_UNION) &&
        !rt->is_intrinsic && rt->size > 16;

    if (has_hidden_out_ptr && ra->func->return_value) {
        irCgSetLoff(ra, irVarId(ra->func->return_value), ast_func->loff);
    }

    if (ast_func->params) {
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            if (p->kind == AST_VAR_ARGS) {
                /* C's `...` binds nothing - no count, no slot array -
                 * so there is no loff to hand out. */
                if (!p->argc) continue;
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
            /* The scaled index of a fused addressing mode is a read.
             * Its two siblings, irSlotReadCount and
             * irFnLocalSlotNeeded, already count it; this one decides
             * whether an alloca keeps its stack slot, so missing it
             * takes the slot away from a buffer still being indexed. */
            if (I->idx == target) return 1;
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
    return irFnHasOp(fn, IR_CALL);
}

/* True if fn contains `op` anywhere. Both backends ask this of the
 * ops whose lowering constrains the FRAME - a `va_start` or a
 * `__builtin_frame_address` reads x29/rbp, so the leaf-function frame
 * elision has to be switched off for it. */
int irFnHasOp(IrFunction *fn, IrOp op) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == op) return 1;
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
        /* The SLOT, not the raw size, for both. `total` was already
         * 8-aligned per local while the offset walked down by the raw
         * width - so a single 4-byte local left every local after it
         * at a 4-aligned address. An array of pointers landed on one,
         * and `IrValue **slots[2] = { &I->r1, &I->r2 };` in
         * irNarrowOpOperands indexed it with `ldr x, [base, i, lsl
         * #3]` from a base 4 bytes out of true.
         *
         * It also meant `total` and the offsets disagreed, so the
         * region was bigger than the addresses it handed out. */
        int slot = align(size, 8);
        total += slot;
        new_offset -= slot;
        l->loff = new_offset;
    }

    int param_total = 0;
    AstType *rt = ast_func->type ? ast_func->type->rettype : NULL;
    /* Must match irRetIsIndirect in ir.c: only an aggregate LARGER than
     * 16 bytes gets a hidden out-pointer. Anything at or below that is
     * returned in registers and needs a full-size buffer, not an 8-byte
     * pointer slot.
     *
     * Without the size check the layout reserved 8 bytes and bound the
     * return value to them, so a 16-byte struct sat in an 8-byte slot
     * and its second word landed on whatever came next. That was the
     * dead tail of the parameter area, which is why it never showed -
     * until promotion moved the parameters into registers and the
     * overrun reached the saved frame pointer. */
    int has_hidden_out_ptr = rt &&
        (rt->kind == AST_TYPE_CLASS || rt->kind == AST_TYPE_UNION) &&
        !rt->is_intrinsic && rt->size > 16;
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
                /* ...or one the promoter moved into a register. The
                 * locals loop above already checks this; the params
                 * loop only knew about a source-level `reg <REG>` pin,
                 * so a PROMOTED param kept reserving 8 bytes it never
                 * touched - which held stack_space above zero and
                 * stopped the frame being elided at all. */
                if (iv && iv->pinned_reg) continue;
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
                if (!p->argc) continue;      /* C's `...`: nothing bound */
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
            /* ...and promoted into a register, likewise. Three places
             * decide this - the totalling loop, this one, and
             * irCgBindAstLoffs - and every offset after a disagreement
             * shifts. */
            if (iv && iv->pinned_reg) continue;
            int sz = (p->kind == AST_FUNPTR) ? 8 : p->type->size;
            p->loff = -offset;
            offset -= align(sz, 8);
        }
    }
    return locals_aligned;
}
