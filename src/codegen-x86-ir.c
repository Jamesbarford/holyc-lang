#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "codegen-x86-ir.h"
#include "containers.h"
#include "ir.h"
#include "ir-fold.h"
#include "ir-mem2reg.h"
#include "ir-types.h"
#include "list.h"
#include "prsutil.h"
#include "util.h"
#include "x86.h"

/* These ir.c internals aren't otherwise exposed; declare them locally. */
extern IrCtx *irCtxNew(Cctrl *cc);
extern IrValue *irFnGetVar(IrFunction *func, u32 lvar_id);

/*
 * IR -> x86_64 codegen for slice-0.
 *
 * Baseline strategy: every IR temp gets its own stack slot. Each instruction
 * loads its sources into rax/rcx, performs the op, and writes the result to
 * its tmp's slot.
 *
 * On top of that we run a tiny "fusion" pre-pass that sets two flag bits on
 * each IrInstr (IRCG_FUSE_TO_NEXT, IRCG_R1_IN_RAX) when a single-use def is
 * immediately consumed as the next instruction's first %rax source. The
 * emitter then skips the spill on the def and the reload on the use, and
 * the slot allocator drops the (unused) stack slot. This collapses the bulk
 * of the bloat — straight-line code keeps values in %rax exactly the way
 * the AST -> x86 codegen does.
 *
 * Locals and parameters re-use the offsets that asmFunctionInit assigned
 * to AST nodes, so the prologue (and any future fix-ups to it) stays in
 * one place.
 */

/* Codegen-private bits stored on IrInstr.flags. ir.c initialises flags to 0
 * and no other code (ir-fold, ir-debug, ARM64) reads or writes it, so we own
 * these bits exclusively during codegen. */
#define IRCG_FUSE_TO_NEXT (1u << 0)
#define IRCG_R1_IN_RAX    (1u << 1)
/* Set on an IR_PHI when the value can live in %rax across the block boundary
 * (the only phi at this block's head and all predecessors arrive via IR_JMP,
 * not IR_BR). When set, predecessors materialise the incoming value into
 * %rax before their jmp; the phi itself emits nothing; the slot allocator
 * skips the phi's dst. */
#define IRCG_PHI_IN_RAX   (1u << 2)

typedef struct IrCgCtx {
    Cctrl *cc;
    AoStr *buf;
    IrFunction *func;
    /* Map<u32 IrValue.var.id -> int loff>. loff is sign-extended on read. */
    Map *id_to_loff;
    /* Bytes added to %rsp beyond what asmFunctionInit reserved, for IR tmps. */
    int extra_stack;
    /* Unique block-label prefix per function so multiple slice functions
     * compiled in the same translation unit don't collide. */
    int func_uid;
    /* Set during the per-block emission loop so terminator codegen knows
     * who's emitting and which block (if any) follows in layout order. */
    IrBlock *cur_block;
    IrBlock *next_block;
} IrCgCtx;

static int ircg_func_seq = 0;

static void irCgSetLoff(IrCgCtx *ctx, u32 var_id, int loff) {
    /* mapAddIntOrErr fails on duplicate; use it as an assert for distinctness. */
    int ok = mapAddIntOrErr(ctx->id_to_loff, var_id, (void *)(intptr_t)loff);
    if (!ok) {
        loggerPanic("ir-cg: duplicate loff mapping for var.id=%u\n", var_id);
    }
}

static int irCgGetLoff(IrCgCtx *ctx, IrValue *val) {
    assert(val != NULL);
    if (!mapHasInt(ctx->id_to_loff, val->as.var.id)) {
        loggerPanic("ir-cg: no slot for var.id=%u kind=%d\n",
                    val->as.var.id, val->kind);
    }
    return (int)(intptr_t)mapGetInt(ctx->id_to_loff, val->as.var.id);
}

/* Reserve a fresh stack slot for an SSA temp. */
static void irCgAllocTmp(IrCgCtx *ctx, IrValue *val, int starting_offset) {
    if (!val || val->kind != IR_VAL_TMP) return;
    if (mapHasInt(ctx->id_to_loff, val->as.var.id)) return;
    ctx->extra_stack += 8;
    irCgSetLoff(ctx, val->as.var.id, -(starting_offset + ctx->extra_stack));
}

/* Bind every variable known to the IR (params + alloca'd locals) to its
 * AST-driven loff. Walk the AST function's params and locals lists; the
 * eligibility predicate guarantees they're plain AST_LVAR ints. */
static void irCgBindAstLoffs(IrCgCtx *ctx, Ast *ast_func) {
    if (ast_func->params) {
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            IrValue *iv = irFnGetVar(ctx->func, p->lvar_id);
            if (iv) irCgSetLoff(ctx, iv->as.var.id, p->loff);
        }
    }
    listForEach(ast_func->locals) {
        Ast *l = (Ast *)it->value;
        u32 lid = (l->kind == AST_DEFAULT_PARAM) ? l->declvar->lvar_id
                                                  : l->lvar_id;
        IrValue *iv = irFnGetVar(ctx->func, lid);
        if (iv) irCgSetLoff(ctx, iv->as.var.id, l->loff);
    }
}

/* Allocate slots only for tmps the emitter is actually going to touch.
 * Operand roles per opcode:
 *   - "spill dst"    (write rax to slot) — skip if IRCG_FUSE_TO_NEXT.
 *   - "load r1"      (read slot to rax)  — skip if IRCG_R1_IN_RAX.
 *   - "address dst"  (offset literal)    — always needs a slot.
 *   - "load r2"      (read slot to rcx)  — always needs a slot.
 * Fused (dst, r1) pairs are never spilled or read, so their tmp ids
 * legitimately have no slot. */
static void irCgAllocOperandsForInstr(IrCgCtx *ctx, IrInstr *I, int start) {
    int spill_dst   = !(I->flags & IRCG_FUSE_TO_NEXT);
    int load_r1     = !(I->flags & IRCG_R1_IN_RAX);

    switch (I->op) {
    case IR_NOP:
    case IR_JMP:
    case IR_LOOP:
        return;

    case IR_ALLOCA:
        irCgAllocTmp(ctx, I->dst, start);
        return;

    case IR_LOAD:
        if (spill_dst) irCgAllocTmp(ctx, I->dst, start);
        irCgAllocTmp(ctx, I->r1, start);            /* address */
        return;

    case IR_STORE:
        irCgAllocTmp(ctx, I->dst, start);           /* address */
        if (load_r1) irCgAllocTmp(ctx, I->r1, start);
        return;

    case IR_LOAD_DEREF:
        /* dst = *r1; r1 holds a runtime pointer in its slot. */
        if (spill_dst) irCgAllocTmp(ctx, I->dst, start);
        irCgAllocTmp(ctx, I->r1, start);
        return;

    case IR_STORE_DEREF:
        /* *dst = r1; dst holds a runtime pointer, r1 the value. */
        irCgAllocTmp(ctx, I->dst, start);
        irCgAllocTmp(ctx, I->r1, start);
        return;

    case IR_LEA:
        /* dst = &r1's slot. dst gets a slot to hold the pointer value;
         * r1 is the slot we're taking the address of (already bound
         * to its loff via param-spill or alloca). */
        if (spill_dst) irCgAllocTmp(ctx, I->dst, start);
        return;

    case IR_IADD: case IR_ISUB: case IR_IMUL:
    case IR_AND:  case IR_OR:   case IR_XOR:
    case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
    case IR_SHL:  case IR_SHR:  case IR_SAR:
    case IR_ICMP:
        if (spill_dst) irCgAllocTmp(ctx, I->dst, start);
        if (load_r1)   irCgAllocTmp(ctx, I->r1, start);
        irCgAllocTmp(ctx, I->r2, start);
        return;

    case IR_BR:
        if (load_r1) irCgAllocTmp(ctx, I->dst, start);
        return;

    case IR_RET:
        if (I->dst && load_r1) irCgAllocTmp(ctx, I->dst, start);
        return;

    case IR_PHI:
        /* Rax-resident phi: no slot. Slot-resident phi: dst gets a slot,
         * predecessors store into it, in-block consumers reload from it. */
        if (!(I->flags & IRCG_PHI_IN_RAX)) {
            irCgAllocTmp(ctx, I->dst, start);
        }
        return;

    case IR_CALL:
        /* dst (return value) gets a slot like any other def, unless fused.
         * r1 is the args wrapper (IR_VAL_UNRESOLVED), not a tmp — no slot.
         * The arg values themselves are loaded directly into argument
         * registers from their existing slots at emit time. */
        if (spill_dst && I->dst && I->dst->type != IR_TYPE_VOID &&
            I->dst->kind == IR_VAL_TMP) {
            irCgAllocTmp(ctx, I->dst, start);
        }
        return;

    case IR_GEP: {
        /* Get-element-pointer for stack-allocated structs. dst aliases
         * `base + offset` — we bind its loff right here so subsequent
         * loads/stores through it just look up the right %rbp offset. */
        if (!I->dst || I->dst->kind != IR_VAL_TMP ||
            !I->r1 || !I->r2 || I->r2->kind != IR_VAL_CONST_INT) {
            return;
        }
        int base_loff = irCgGetLoff(ctx, I->r1);
        int field_loff = base_loff + (int)I->r2->as._i64;
        if (!mapHasInt(ctx->id_to_loff, I->dst->as.var.id)) {
            irCgSetLoff(ctx, I->dst->as.var.id, field_loff);
        }
        return;
    }

    default:
        /* Be conservative for ops we don't model yet. */
        irCgAllocTmp(ctx, I->dst, start);
        irCgAllocTmp(ctx, I->r1, start);
        irCgAllocTmp(ctx, I->r2, start);
    }
}

static void irCgAllocAllTmps(IrCgCtx *ctx, int starting_offset) {
    listForEach(ctx->func->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        listForEach(block->instructions) {
            IrInstr *instr = (IrInstr *)it->value;
            irCgAllocOperandsForInstr(ctx, instr, starting_offset);
        }
    }
}

/* Load any IR value into the given 64-bit register (rax or rcx). */
static void irCgLoadToReg(IrCgCtx *ctx, IrValue *val, const char *reg) {
    switch (val->kind) {
    case IR_VAL_CONST_INT:
        aoStrCatPrintf(ctx->buf, "movq   $%lld, %%%s\n\t",
                       (long long)val->as._i64, reg);
        return;
    case IR_VAL_CONST_STR:
        /* String literal address. The data section was emitted earlier in
         * asmGenerate with this label, so we just take its address. */
        aoStrCatPrintf(ctx->buf, "leaq   %s(%%rip), %%%s\n\t",
                       val->as.str.label->data, reg);
        return;
    case IR_VAL_TMP:
    case IR_VAL_LOCAL:
    case IR_VAL_PARAM:
        aoStrCatPrintf(ctx->buf, "movq   %d(%%rbp), %%%s\n\t",
                       irCgGetLoff(ctx, val), reg);
        return;
    default:
        loggerPanic("ir-cg: cannot load value of kind %d\n", val->kind);
    }
}

static void irCgStoreReg(IrCgCtx *ctx, IrValue *dst, const char *reg) {
    aoStrCatPrintf(ctx->buf, "movq   %%%s, %d(%%rbp)\n\t",
                   reg, irCgGetLoff(ctx, dst));
}

/* Emit `<setcc> %al; movzbq %al, %rax` for a comparison kind. */
static void irCgEmitSetCC(IrCgCtx *ctx, IrCmpKind cmp) {
    const char *cc = NULL;
    switch (cmp) {
    case IR_CMP_EQ:  cc = "sete";  break;
    case IR_CMP_NE:  cc = "setne"; break;
    case IR_CMP_LT:  cc = "setl";  break;
    case IR_CMP_LE:  cc = "setle"; break;
    case IR_CMP_GT:  cc = "setg";  break;
    case IR_CMP_GE:  cc = "setge"; break;
    case IR_CMP_ULT: cc = "setb";  break;
    case IR_CMP_ULE: cc = "setbe"; break;
    case IR_CMP_UGT: cc = "seta";  break;
    case IR_CMP_UGE: cc = "setae"; break;
    default:
        loggerPanic("ir-cg: unsupported cmp kind %d for slice\n", cmp);
    }
    aoStrCatPrintf(ctx->buf, "%s   %%al\n\tmovzbq %%al, %%rax\n\t", cc);
}

static void irCgBlockLabel(IrCgCtx *ctx, IrBlock *block, char *out, int n) {
    snprintf(out, n, ".LIRBB%d_%u", ctx->func_uid, block->id);
}

/* ---- fusion pre-pass --------------------------------------------------- */

/* True if the instruction's natural codegen ends with the result in %rax,
 * i.e. it's a candidate to "leave in rax" rather than spill to a slot. */
static int instrDefsIntoRax(IrInstr *I) {
    if (I->op == IR_PHI && (I->flags & IRCG_PHI_IN_RAX)) return 1;
    switch (I->op) {
    case IR_LOAD:
    case IR_LOAD_DEREF:
    case IR_LEA:
    case IR_IADD: case IR_ISUB: case IR_IMUL:
    case IR_AND:  case IR_OR:   case IR_XOR:
    case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
    case IR_SHL:  case IR_SHR:  case IR_SAR:
    case IR_ICMP:
        return 1;
    case IR_CALL:
        /* Non-void return value lives in %rax after the call. */
        return I->dst && I->dst->type != IR_TYPE_VOID;
    default:
        return 0;
    }
}

static IrValue *firstRaxSource(IrInstr *I);

/* Classify each IR_PHI: rax-resident if
 *   (a) it's the only phi at the block head,
 *   (b) every predecessor arrives via IR_JMP / IR_LOOP (so %rax survives),
 *   (c) the phi's dst has at most one use across the function, AND
 *   (d) that use is the very next non-NOP instruction in this block, as
 *       its first-rax source — so the value in rax is consumed before any
 *       arithmetic / call clobbers it.
 * All other phis fall back to slot-resident (store on the pred side, load
 * on the use side, just like an alloca). */
static void irCgClassifyPhis(IrFunction *func) {
    /* Pre-compute use counts for tmp values across the whole function. We
     * count r1, r2, dst (for BR/RET — they treat dst as a source), and
     * phi pair values (consumed at materialisation time). */
    Map *uses = mapNew(64, &map_uint_to_uint_type);
#define BUMP_TMP(v) do { \
    if ((v) && (v)->kind == IR_VAL_TMP) { \
        u32 _id = (v)->as.var.id; \
        int _n = mapHasInt(uses, _id) ? (int)(intptr_t)mapGetInt(uses, _id) : 0; \
        mapAdd(uses, (void *)(u64)_id, (void *)(intptr_t)(_n + 1)); \
    } \
} while (0)
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 i = 0; i < args->size; ++i) {
                    BUMP_TMP((IrValue *)args->entries[i]);
                }
            } else {
                BUMP_TMP(I->r1);
            }
            BUMP_TMP(I->r2);
            if (I->op == IR_BR || I->op == IR_RET) {
                BUMP_TMP(I->dst);
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                    IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                    BUMP_TMP(p->ir_value);
                }
            }
        }
    }
#undef BUMP_TMP

    listForEach(func->blocks) {
        IrBlock *B = (IrBlock *)it->value;
        if (listEmpty(B->instructions)) continue;

        int phi_count = 0;
        IrInstr *the_phi = NULL;
        listForEach(B->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_PHI) { phi_count++; the_phi = I; continue; }
            if (I->op == IR_NOP) continue;
            break;
        }
        if (phi_count != 1) continue;

        Map *preds = irFunctionGetPredecessors(func, B);
        if (!preds || preds->size == 0) continue;

        int all_jmp = 1;
        MapIter *iter = mapIterNew(preds);
        while (mapIterNext(iter)) {
            IrBlock *P = (IrBlock *)iter->node->value;
            IrInstr *term = (IrInstr *)listValue(IrInstr *,
                                                 listTail(P->instructions));
            if (!term || (term->op != IR_JMP && term->op != IR_LOOP)) {
                all_jmp = 0; break;
            }
        }
        mapIterRelease(iter);
        if (!all_jmp) continue;

        if (!the_phi->dst || the_phi->dst->kind != IR_VAL_TMP) continue;
        u32 dst_id = the_phi->dst->as.var.id;
        int dst_uses = mapHasInt(uses, dst_id)
                       ? (int)(intptr_t)mapGetInt(uses, dst_id) : 0;
        if (dst_uses > 1) continue;

        /* The single use must be in *this* block as the very next non-NOP
         * instruction's first-rax source — otherwise rax may be clobbered
         * before the use. (If the use is in a successor's phi pair, the
         * predecessor-side materialisation can't read it from rax either.) */
        IrInstr *next = NULL;
        int seen_phi = 0;
        listForEach(B->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I == the_phi) { seen_phi = 1; continue; }
            if (!seen_phi) continue;
            if (I->op == IR_NOP) continue;
            if (I->op == IR_PHI) continue;   /* skip sibling phis */
            next = I;
            break;
        }
        if (!next) continue;
        IrValue *next_src = firstRaxSource(next);
        if (!next_src || next_src->kind != IR_VAL_TMP) continue;
        if (next_src->as.var.id != dst_id) continue;

        the_phi->flags |= IRCG_PHI_IN_RAX;
    }

    mapRelease(uses);
}

/* The single source operand the instruction loads into %rax first. NULL
 * when the op doesn't have one (alloca, unconditional jmp, nop, ...). */
static IrValue *firstRaxSource(IrInstr *I) {
    switch (I->op) {
    case IR_STORE:
    case IR_IADD: case IR_ISUB: case IR_IMUL:
    case IR_AND:  case IR_OR:   case IR_XOR:
    case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
    case IR_SHL:  case IR_SHR:  case IR_SAR:
    case IR_ICMP:
        return I->r1;
    case IR_BR:
    case IR_RET:
        return I->dst;
    default:
        return NULL;
    }
}

static void bumpUseIfTmp(Map *uses, IrValue *v) {
    if (!v || v->kind != IR_VAL_TMP) return;
    int n = mapHasInt(uses, v->as.var.id)
            ? (int)(intptr_t)mapGetInt(uses, v->as.var.id) : 0;
    mapAdd(uses, (void *)(u64)v->as.var.id, (void *)(intptr_t)(n + 1));
}

/* Walk every instruction once, counting tmp source-uses; then walk again to
 * mark FUSE_TO_NEXT / R1_IN_RAX pairs. Sources for use-counting purposes are:
 * r1, r2, and dst (for IR_BR / IR_RET only — those treat dst as a source). */
static void irCgAnnotate(IrFunction *func) {
    Map *uses = mapNew(64, &map_uint_to_uint_type);

    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                /* IR_CALL's r1 is the args wrapper — count each arg, not
                 * the wrapper itself. */
                Vec *args = I->r1->as.array.values;
                for (u64 i = 0; i < args->size; ++i) {
                    bumpUseIfTmp(uses, (IrValue *)args->entries[i]);
                }
            } else {
                bumpUseIfTmp(uses, I->r1);
            }
            bumpUseIfTmp(uses, I->r2);
            if (I->op == IR_BR || I->op == IR_RET) {
                bumpUseIfTmp(uses, I->dst);
            }
            /* Phi pair values are consumed at predecessor-side
             * materialisation; count them so the zero-use slot-skip
             * heuristic below doesn't drop slots a phi still reads from. */
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                    IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                    bumpUseIfTmp(uses, p->ir_value);
                }
            }
            /* Make sure stale flag bits from a previous compilation can't
             * leak in if instructions ever get recycled. */
            I->flags &= ~(u64)(IRCG_FUSE_TO_NEXT | IRCG_R1_IN_RAX);
        }
    }

    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (listEmpty(bb->instructions)) continue;
        for (List *node = bb->instructions->next;
             node != bb->instructions;
             node = node->next)
        {
            IrInstr *cur = (IrInstr *)node->value;
            if (cur->op == IR_NOP) continue;
            if (!instrDefsIntoRax(cur)) continue;
            if (!cur->dst || cur->dst->kind != IR_VAL_TMP) continue;

            int use_count = mapHasInt(uses, cur->dst->as.var.id)
                            ? (int)(intptr_t)mapGetInt(uses, cur->dst->as.var.id)
                            : 0;
            /* Zero-use def: the result is dead, so we still emit the op
             * (it may have side effects — IR_CALL — or simply be cheap to
             * leave) but skip the spill. The slot allocator drops the
             * unused dst's slot. */
            if (use_count == 0) {
                cur->flags |= IRCG_FUSE_TO_NEXT;
                continue;
            }
            if (use_count != 1) continue;

            /* Find the next instruction that actually emits something. Skip
             * IR_NOPs left by mem2reg / fold. If we hit an IR_JMP whose
             * target has a single predecessor (this block) we can chase
             * into the target — %rax survives an unconditional jump. We
             * stop at IR_BR / IR_LOOP since branch arms may need different
             * rax setups. */
            IrInstr *next = NULL;
            IrBlock *scan_bb = bb;
            List *scan = node->next;
            int hops = 0;
            while (1) {
                while (scan == scan_bb->instructions) {
                    /* end of current block — try to chase through a jmp */
                    IrInstr *term = (IrInstr *)listValue(IrInstr *,
                                                         listTail(scan_bb->instructions));
                    if (!term || term->op != IR_JMP) { scan_bb = NULL; break; }
                    IrBlock *tgt = term->extra.blocks.target_block;
                    if (!tgt) { scan_bb = NULL; break; }
                    Map *preds = irFunctionGetPredecessors(func, tgt);
                    if (!preds || preds->size != 1) { scan_bb = NULL; break; }
                    scan_bb = tgt;
                    scan = scan_bb->instructions->next;
                    if (++hops > 8) { scan_bb = NULL; break; }
                }
                if (!scan_bb) break;
                IrInstr *cand = (IrInstr *)scan->value;
                if (cand->op != IR_NOP && cand->op != IR_JMP) {
                    next = cand;
                    break;
                }
                /* Treat IR_JMP as "end of block" so the outer loop chases. */
                scan = scan->next;
            }
            if (!next) continue;

            IrValue *next_src = firstRaxSource(next);
            if (!next_src || next_src->kind != IR_VAL_TMP) continue;
            if (next_src->as.var.id != cur->dst->as.var.id) continue;

            cur->flags |= IRCG_FUSE_TO_NEXT;
            next->flags |= IRCG_R1_IN_RAX;
        }
    }

    mapRelease(uses);
}

/* Emit the body of a single IR instruction. Block boundaries / labels are
 * handled by the caller. */
/* Helpers wrapping the spill / first-rax-load sites so the bit-flag checks
 * happen in one place. */
static void irCgLoadFirstSrc(IrCgCtx *ctx, IrInstr *instr, IrValue *src) {
    if (instr->flags & IRCG_R1_IN_RAX) return;
    irCgLoadToReg(ctx, src, "rax");
}

static void irCgSpillDst(IrCgCtx *ctx, IrInstr *instr, const char *reg) {
    if (instr->flags & IRCG_FUSE_TO_NEXT) return;
    irCgStoreReg(ctx, instr->dst, reg);
}

/* Emit, for each phi at the head of `to`, the move(s) that put the value
 * coming from `from` into either %rax (rax-resident phi) or the phi's
 * stack slot. Caller has already arranged that we're at the end of `from`'s
 * block, just before its terminator.
 *
 * Order matters: when phi A's pair value reads phi B's dst at this same
 * block, A must materialise before B — otherwise B's write clobbers the
 * slot A is about to read. We collect all phis here, then repeatedly pick
 * one whose dst is not read by any still-pending phi (a leaf in the
 * read-dependency graph). Cycles fall back to scratch via %rcx. */
static void irCgEmitOnePhi(IrCgCtx *ctx, IrInstr *phi, IrPair *match) {
    irCgLoadToReg(ctx, match->ir_value, "rax");
    if (!(phi->flags & IRCG_PHI_IN_RAX)) {
        irCgStoreReg(ctx, phi->dst, "rax");
    }
}

static void irCgEmitPhiMaterialize(IrCgCtx *ctx, IrBlock *from, IrBlock *to) {
    if (!to || !from) return;

    /* Gather (phi, pair-from-`from`) pairs and a `done` flag. */
    enum { kMaxPhis = 32 };
    IrInstr *phis[kMaxPhis];
    IrPair  *pairs[kMaxPhis];
    int     done[kMaxPhis];
    int n = 0;

    listForEach(to->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_NOP) continue;
        if (I->op != IR_PHI) break;
        if (n >= kMaxPhis) {
            loggerPanic("ir-cg: too many phis at one block (>%d)\n", kMaxPhis);
        }
        IrPair *match = NULL;
        if (I->extra.phi_pairs) {
            for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                if (p->ir_block == from) { match = p; break; }
            }
        }
        if (!match || !match->ir_value) continue;
        phis[n] = I;
        pairs[n] = match;
        done[n] = 0;
        n++;
    }
    if (n == 0) return;

    /* Repeatedly pick a phi whose dst isn't read by any still-pending
     * phi's pair. That's a leaf in the read-dependency graph: emitting
     * it can't break any later read. */
    int emitted = 0;
    while (emitted < n) {
        int progress = 0;
        for (int i = 0; i < n; ++i) {
            if (done[i]) continue;
            int read_by_pending = 0;
            for (int j = 0; j < n; ++j) {
                if (i == j || done[j]) continue;
                IrValue *v = pairs[j]->ir_value;
                if (v && v->kind == IR_VAL_TMP &&
                    phis[i]->dst &&
                    phis[i]->dst->kind == IR_VAL_TMP &&
                    v->as.var.id == phis[i]->dst->as.var.id) {
                    read_by_pending = 1;
                    break;
                }
            }
            if (!read_by_pending) {
                irCgEmitOnePhi(ctx, phis[i], pairs[i]);
                done[i] = 1;
                emitted++;
                progress = 1;
            }
        }
        if (!progress) {
            /* Cycle (every remaining phi's dst is read by another). Stash
             * the next pending phi's source value in a scratch slot via
             * %rcx, then break the cycle. For slice-0 with structured
             * loops, cycles are rare — panic for now so we notice. */
            loggerPanic("ir-cg: phi materialisation cycle (NYI)\n");
        }
    }
}

/* Mirror of the BR/JMP emission's layout decisions: returns the set of
 * block ids that are actually referenced by some asm jump after elision.
 * Blocks not in the set still have their bodies emitted, but their label
 * header is dead code and can be dropped. */
static Set *irCgComputeReferencedBlocks(IrFunction *func);

/* Does this block start with at least one IR_PHI? */
static int blockHasPhi(IrBlock *bb) {
    if (!bb) return 0;
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_PHI) return 1;
        if (I->op != IR_NOP) return 0;
    }
    return 0;
}

static IrInstr *blockTerminator(IrBlock *bb) {
    IrInstr *term = NULL;
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_NOP) continue;
        term = I;
    }
    return term;
}

static Set *irCgComputeReferencedBlocks(IrFunction *func) {
    Set *referenced = setNew(16, &set_uint_type);
    for (List *bn = func->blocks->next;
         bn != func->blocks;
         bn = bn->next)
    {
        IrBlock *block = (IrBlock *)bn->value;
        IrBlock *next_block = (bn->next != func->blocks)
                              ? (IrBlock *)bn->next->value
                              : NULL;
        IrInstr *term = blockTerminator(block);
        if (!term) continue;

        switch (term->op) {
        case IR_JMP:
        case IR_LOOP: {
            IrBlock *t = term->extra.blocks.target_block;
            if (t && t != next_block) {
                setAdd(referenced, (void *)(u64)t->id);
            }
            break;
        }
        case IR_BR: {
            IrBlock *t = term->extra.blocks.target_block;
            IrBlock *f = term->extra.blocks.fallthrough_block;
            int t_phi = blockHasPhi(t);
            int f_phi = blockHasPhi(f);
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
                /* Phi case always emits `jmp tlbl` after the true-arm
                 * materialisation; the trailing `jmp flbl` is elidable
                 * when next == f. */
                if (t) setAdd(referenced, (void *)(u64)t->id);
                if (f && next_block != f) {
                    setAdd(referenced, (void *)(u64)f->id);
                }
            }
            break;
        }
        default:
            /* IR_RET — no jump targets to mark. */
            break;
        }
    }
    return referenced;
}

static void irCgEmitInstr(IrCgCtx *ctx, IrInstr *instr) {
    switch (instr->op) {
    case IR_NOP:
        /* Folded / mem2reg'd away. */
        break;

    case IR_ALLOCA:
        /* Stack slot already reserved at prologue time. */
        break;

    case IR_PHI:
        /* Phi values are produced at the predecessor side (see
         * irCgEmitPhiMaterialize). The phi itself emits nothing. For an
         * in-rax phi, the value is already in %rax at block entry; for a
         * slot-resident phi, it's already in the slot. */
        break;

    case IR_GEP:
        /* All bookkeeping happened at slot-allocation time; the dst's
         * loff is already bound to base+offset. Nothing to emit. */
        break;

    case IR_LOAD:
        /* dst = *r1. r1 is a slot pointer (always addressed by offset, never
         * read into rax), so R1_IN_RAX never applies here. */
        irCgLoadToReg(ctx, instr->r1, "rax");
        irCgSpillDst(ctx, instr, "rax");
        break;

    case IR_STORE:
        /* *dst = r1. dst is a slot pointer; r1 is the value. The store
         * always writes memory so spill-skipping doesn't apply. */
        irCgLoadFirstSrc(ctx, instr, instr->r1);
        irCgStoreReg(ctx, instr->dst, "rax");
        break;

    case IR_LOAD_DEREF:
        /* dst = *r1, where r1 is a runtime pointer value (not a slot id).
         * Load the pointer into rcx, then dereference. */
        irCgLoadToReg(ctx, instr->r1, "rcx");
        aoStrCatPrintf(ctx->buf, "movq   (%%rcx), %%rax\n\t");
        irCgSpillDst(ctx, instr, "rax");
        break;

    case IR_STORE_DEREF:
        /* *dst = r1; dst holds a runtime pointer, r1 is the value. */
        irCgLoadFirstSrc(ctx, instr, instr->r1);
        irCgLoadToReg(ctx, instr->dst, "rcx");
        aoStrCatPrintf(ctx->buf, "movq   %%rax, (%%rcx)\n\t");
        break;

    case IR_LEA: {
        /* dst = address of r1's slot. */
        int loff = irCgGetLoff(ctx, instr->r1);
        aoStrCatPrintf(ctx->buf, "leaq   %d(%%rbp), %%rax\n\t", loff);
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_IADD:
    case IR_ISUB:
    case IR_IMUL:
    case IR_AND:
    case IR_OR:
    case IR_XOR: {
        const char *op = NULL;
        switch (instr->op) {
        case IR_IADD: op = "addq"; break;
        case IR_ISUB: op = "subq"; break;
        case IR_IMUL: op = "imulq"; break;
        case IR_AND:  op = "andq"; break;
        case IR_OR:   op = "orq";  break;
        case IR_XOR:  op = "xorq"; break;
        default: assert(0);
        }
        irCgLoadFirstSrc(ctx, instr, instr->r1);
        irCgLoadToReg(ctx, instr->r2, "rcx");
        aoStrCatPrintf(ctx->buf, "%s   %%rcx, %%rax\n\t", op);
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_IDIV:
    case IR_UDIV:
    case IR_IREM:
    case IR_UREM: {
        irCgLoadFirstSrc(ctx, instr, instr->r1);
        irCgLoadToReg(ctx, instr->r2, "rcx");
        if (instr->op == IR_IDIV || instr->op == IR_IREM) {
            aoStrCatPrintf(ctx->buf, "cqto\n\tidivq %%rcx\n\t");
        } else {
            aoStrCatPrintf(ctx->buf, "xorq %%rdx, %%rdx\n\tdivq %%rcx\n\t");
        }
        /* idivq/divq leave the quotient in rax and the remainder in rdx.
         * Park the result in rax so spill or fused consumer always reads
         * from one place. */
        if (instr->op == IR_IREM || instr->op == IR_UREM) {
            aoStrCatPrintf(ctx->buf, "movq   %%rdx, %%rax\n\t");
        }
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_SHL:
    case IR_SHR:
    case IR_SAR: {
        const char *op = (instr->op == IR_SHL) ? "shlq"
                       : (instr->op == IR_SAR) ? "sarq" : "shrq";
        irCgLoadFirstSrc(ctx, instr, instr->r1);
        irCgLoadToReg(ctx, instr->r2, "rcx");
        aoStrCatPrintf(ctx->buf, "%s   %%cl, %%rax\n\t", op);
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_ICMP: {
        irCgLoadFirstSrc(ctx, instr, instr->r1);
        irCgLoadToReg(ctx, instr->r2, "rcx");
        aoStrCatPrintf(ctx->buf, "cmpq   %%rcx, %%rax\n\t");
        irCgEmitSetCC(ctx, instr->extra.cmp_kind);
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_BR: {
        IrBlock *t = instr->extra.blocks.target_block;
        IrBlock *f = instr->extra.blocks.fallthrough_block;
        char tlbl[64], flbl[64];
        irCgBlockLabel(ctx, t, tlbl, sizeof(tlbl));
        irCgBlockLabel(ctx, f, flbl, sizeof(flbl));

        irCgLoadFirstSrc(ctx, instr, instr->dst);

        int t_phi = blockHasPhi(t);
        int f_phi = blockHasPhi(f);
        if (!t_phi && !f_phi) {
            /* Layout-aware: avoid emitting a `jmp` that's an immediate
             * fall-through. If the next block is the true target, use
             * `je flbl` and fall through. If it's the false target,
             * invert to `jne tlbl`. Otherwise emit both jumps. */
            aoStrCatPrintf(ctx->buf, "testq  %%rax, %%rax\n\t");
            if (ctx->next_block == t) {
                aoStrCatPrintf(ctx->buf, "je     %s\n\t", flbl);
            } else if (ctx->next_block == f) {
                aoStrCatPrintf(ctx->buf, "jne    %s\n\t", tlbl);
            } else {
                aoStrCatPrintf(ctx->buf,
                               "je     %s\n\t"
                               "jmp    %s\n\t",
                               flbl, tlbl);
            }
        } else {
            /* Inline per-arm phi materialisation. Phis at BR targets are
             * always slot-resident (the classifier rejects rax-residency
             * if any predecessor reaches via BR), so materialisation
             * doesn't need rax preserved across — we write to slots via
             * rax and let the next arm reuse it. The trailing `jmp flbl`
             * after the false-arm materialisation can also fall through
             * if flbl is the next block. */
            static int br_seq = 0;
            char else_lbl[64];
            int br_id = br_seq++;
            snprintf(else_lbl, sizeof(else_lbl), ".LIRBR%d_E%d",
                     ctx->func_uid, br_id);
            aoStrCatPrintf(ctx->buf,
                           "testq  %%rax, %%rax\n\t"
                           "je     %s\n\t",
                           else_lbl);
            irCgEmitPhiMaterialize(ctx, ctx->cur_block, t);
            aoStrCatPrintf(ctx->buf, "jmp    %s\n", tlbl);
            asmRemovePreviousTab(ctx->buf);
            aoStrCatPrintf(ctx->buf, "%s:\n\t", else_lbl);
            irCgEmitPhiMaterialize(ctx, ctx->cur_block, f);
            if (ctx->next_block != f) {
                aoStrCatPrintf(ctx->buf, "jmp    %s\n\t", flbl);
            }
        }
        break;
    }

    case IR_JMP:
    case IR_LOOP: {
        IrBlock *target = instr->extra.blocks.target_block;
        irCgEmitPhiMaterialize(ctx, ctx->cur_block, target);
        /* JMP elision: if we're about to fall straight into the target
         * label, the unconditional jump is dead weight. Phi materialisation
         * still has to run (the values must be in place). */
        if (target != ctx->next_block) {
            char tlbl[64];
            irCgBlockLabel(ctx, target, tlbl, sizeof(tlbl));
            aoStrCatPrintf(ctx->buf, "jmp    %s\n\t", tlbl);
        }
        break;
    }

    case IR_RET:
        if (instr->dst) {
            irCgLoadFirstSrc(ctx, instr, instr->dst);
        }
        aoStrCatPrintf(ctx->buf, "leave\n\tret\n");
        break;

    case IR_CALL: {
        /* r1 is the args wrapper carrying the Vec of arg IrValues plus the
         * function name as as.array.label. */
        static const char *kArgRegs[] = {
            "rdi", "rsi", "rdx", "rcx", "r8", "r9"
        };
        IrValue *wrap = instr->r1;
        Vec *args = wrap ? wrap->as.array.values : NULL;
        AoStr *fname = wrap ? wrap->as.array.label : NULL;
        if (!fname) {
            loggerPanic("ir-cg: IR_CALL with no function name\n");
        }
        u64 n = args ? args->size : 0;
        if (n > 6) {
            loggerPanic("ir-cg: IR_CALL with %llu args (slice limit 6)\n",
                        (unsigned long long)n);
        }
        for (u64 i = 0; i < n; ++i) {
            IrValue *a = vecGet(IrValue *, args, i);
            irCgLoadToReg(ctx, a, kArgRegs[i]);
        }

        /* If the callee is variadic, System V says %al holds the number of
         * SSE registers used. The slice doesn't pass float args yet, so
         * zero it. We look the function up by name in the global env. */
        Ast *callee = (Ast *)mapGetLen(ctx->cc->global_env,
                                       fname->data, fname->len);
        int callee_variadic =
            callee && callee->type && callee->type->has_var_args;
        if (callee_variadic) {
            aoStrCatPrintf(ctx->buf, "xorl   %%eax, %%eax\n\t");
        }

        aoStrCatPrintf(ctx->buf, "call   %s\n\t",
                       asmNormaliseFunctionName(fname->data));

        if (instr->dst && instr->dst->type != IR_TYPE_VOID &&
            instr->dst->kind == IR_VAL_TMP) {
            irCgSpillDst(ctx, instr, "rax");
        }
        break;
    }

    default:
        loggerPanic("ir-cg: unsupported op %d in slice\n", instr->op);
    }
}

/* AST loff layout for the IR codegen. Runs *after* mem2reg so we can skip
 * locals whose alloca got promoted away — they have no readers/writers
 * left and don't deserve a slot. Slice constraints (no varargs, int-only
 * locals/params, AST_LVAR params) keep this short. Returns the locals
 * + params reservation, NOT 16-byte-aligned. */
static int irCgComputeAstLayout(Ast *ast_func, IrFunction *ir_func) {
    Set *surviving = setNew(8, &set_uint_type);
    listForEach(ir_func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_ALLOCA && I->dst &&
                I->dst->kind == IR_VAL_TMP) {
                setAdd(surviving, (void *)(u64)I->dst->as.var.id);
            }
        }
    }

    int total = 0;
    int new_offset = 0;

    listForEach(ast_func->locals) {
        Ast *l = (Ast *)it->value;
        u32 lid = (l->kind == AST_DEFAULT_PARAM)
                  ? l->declvar->lvar_id : l->lvar_id;
        IrValue *iv = irFnGetVar(ir_func, lid);
        int promoted = iv && iv->kind == IR_VAL_TMP &&
                       !setHas(surviving, (void *)(u64)iv->as.var.id);
        if (promoted) {
            /* mem2reg killed the alloca — skip the slot entirely. The AST
             * loff is no longer reachable (loads/stores via lvar_id were
             * NOP'd) so leaving it 0 is safe. */
            continue;
        }
        total += align(l->type->size, 8);
        new_offset -= l->type->size;
        l->loff = new_offset;
    }

    int param_total = 0;
    if (ast_func->params) {
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            param_total += align(p->type->size, 8);
        }
    }
    total += param_total;
    int locals_aligned = total ? align(total, 16) : 0;

    /* Params live at the bottom of the locals+params region: first param
     * at the most-negative offset, working upward toward locals. Matches
     * asmFunctionInit's layout. */
    int offset = locals_aligned;
    if (ast_func->params) {
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            p->loff = -offset;
            offset -= align(p->type->size, 8);
        }
    }
    setRelease(surviving);
    return locals_aligned;
}

static void irCgEmitFunctionPrologue(AoStr *buf, Ast *func, int total_stack) {
    char *fname = asmNormaliseFunctionName(func->fname->data);
    aoStrCatPrintf(buf,
                   ".text\n\t"
                   ".global %s\n"
                   "%s:\n\t"
                   "push   %%rbp\n\t"
                   "movq   %%rsp, %%rbp\n\t",
                   fname, fname);
    if (total_stack > 0) {
        aoStrCatPrintf(buf, "subq   $%d, %%rsp\n\t", total_stack);
    }
}

static void irCgEmitParamSpills(AoStr *buf, Ast *func) {
    static const char *kArgRegs[] = {
        "rdi", "rsi", "rdx", "rcx", "r8", "r9"
    };
    if (!func->params) return;
    for (u64 i = 0; i < func->params->size; ++i) {
        Ast *p = vecGet(Ast *, func->params, i);
        aoStrCatPrintf(buf, "movq   %%%s, %d(%%rbp)\n\t",
                       kArgRegs[i], p->loff);
    }
}

void asmFunctionFromIr(Cctrl *cc, AoStr *buf, Ast *ast_func) {
    assert(ast_func->kind == AST_FUNC);

    /* 1. Lower to IR. */
    IrCtx *ir_ctx = irCtxNew(cc);
    IrFunction *func = irLowerFunction(ir_ctx, ast_func);

    /* 2. Run analyses. mem2reg promotes allocas away; the layout pass
     *    below uses that info to skip slots for promoted locals. */
    irMem2Reg(func);
    irFoldFunction(func);
    irCgClassifyPhis(func);
    irCgAnnotate(func);

    /* 3. Compute AST-side layout *after* mem2reg so promoted locals
     *    don't reserve dead slots. */
    int locals_params_space = irCgComputeAstLayout(ast_func, func);

    /* 4. Bind IR values to the loffs we just computed. */
    IrCgCtx ctx;
    ctx.cc = cc;
    ctx.buf = buf;
    ctx.func = func;
    ctx.id_to_loff = mapNew(32, &map_uint_to_uint_type);
    ctx.extra_stack = 0;
    ctx.func_uid = ircg_func_seq++;
    ctx.cur_block = NULL;
    ctx.next_block = NULL;
    irCgBindAstLoffs(&ctx, ast_func);

    /* 5. Allocate slots for IR tmps starting just below the params area. */
    irCgAllocAllTmps(&ctx, locals_params_space);

    /* 6. Emit prologue with one subq covering params + locals + tmps. */
    int total_stack = locals_params_space + ctx.extra_stack;
    if (total_stack > 0) total_stack = align(total_stack, 16);
    irCgEmitFunctionPrologue(buf, ast_func, total_stack);
    irCgEmitParamSpills(buf, ast_func);

    /* 7. Walk blocks in order, emit label (if referenced) + instructions.
     *    `referenced` mirrors the BR/JMP layout decisions so we know which
     *    block labels actually appear as jump targets in the asm output. */
    Set *referenced = irCgComputeReferencedBlocks(func);
    for (List *bn = func->blocks->next;
         bn != func->blocks;
         bn = bn->next)
    {
        IrBlock *block = (IrBlock *)bn->value;
        ctx.cur_block = block;
        ctx.next_block = (bn->next != func->blocks)
                         ? (IrBlock *)bn->next->value
                         : NULL;
        if (setHas(referenced, (void *)(u64)block->id)) {
            char lbl[64];
            irCgBlockLabel(&ctx, block, lbl, sizeof(lbl));
            asmRemovePreviousTab(buf);
            aoStrCatPrintf(buf, "%s:\n\t", lbl);
        }
        listForEach(block->instructions) {
            IrInstr *instr = (IrInstr *)it->value;
            irCgEmitInstr(&ctx, instr);
        }
    }
    setRelease(referenced);

    /* 7. Safety net: if the exit block didn't already terminate with `ret`,
     *    emit one. The exit block always ends in IR_RET, so this is a
     *    belt-and-braces match for asmFunction(). */
    if (!asmHasRet(buf)) {
        asmFunctionLeave(buf);
    }

    mapRelease(ctx.id_to_loff);
    free(ir_ctx->prog);
    free(ir_ctx);
}
