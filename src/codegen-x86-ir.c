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
    case IR_IADD: case IR_ISUB: case IR_IMUL:
    case IR_AND:  case IR_OR:   case IR_XOR:
    case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
    case IR_SHL:  case IR_SHR:  case IR_SAR:
    case IR_ICMP:
        return 1;
    default:
        return 0;
    }
}

/* Classify each IR_PHI: rax-resident if it is the only phi at its block's
 * head and every predecessor reaches the block via IR_JMP / IR_LOOP. Anything
 * else (multiple phis, branch-arrived predecessor) gets a stack slot and the
 * usual store-to-slot-then-load codegen. */
static void irCgClassifyPhis(IrFunction *func) {
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

        the_phi->flags |= IRCG_PHI_IN_RAX;
    }
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
            bumpUseIfTmp(uses, I->r1);
            bumpUseIfTmp(uses, I->r2);
            if (I->op == IR_BR || I->op == IR_RET) {
                bumpUseIfTmp(uses, I->dst);
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
 * block, just before its terminator. */
static void irCgEmitPhiMaterialize(IrCgCtx *ctx, IrBlock *from, IrBlock *to) {
    if (!to || !from) return;
    listForEach(to->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_NOP) continue;
        if (I->op != IR_PHI) break;          /* past the phi prefix */

        IrPair *match = NULL;
        if (I->extra.phi_pairs) {
            for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                if (p->ir_block == from) { match = p; break; }
            }
        }
        if (!match || !match->ir_value) continue;  /* well-formed IR shouldn't hit this */

        irCgLoadToReg(ctx, match->ir_value, "rax");
        if (!(I->flags & IRCG_PHI_IN_RAX)) {
            irCgStoreReg(ctx, I->dst, "rax");
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

    default:
        loggerPanic("ir-cg: unsupported op %d in slice\n", instr->op);
    }
}

void asmFunctionFromIr(Cctrl *cc, AoStr *buf, Ast *ast_func) {
    assert(ast_func->kind == AST_FUNC);

    /* 1. Standard prologue + AST loff assignment. */
    int stack_used = asmFunctionInit(cc, buf, ast_func);

    /* 2. Lower to IR. The IR arena is reset between programs by the global
     *    irMemoryRelease but persists for the duration of compilation, so it's
     *    fine to leave the structures in place after we emit assembly. */
    IrCtx *ir_ctx = irCtxNew(cc);
    IrFunction *func = irLowerFunction(ir_ctx, ast_func);

    /* 2a. Promote single-store allocas to plain SSA value flow before
     *     anything else looks at the IR. */
    irMem2Reg(func);

    /* 2b. Run constant folding so we emit fewer redundant loads/stores.
     *     mem2reg first means fold sees through what used to be loads. */
    irFoldFunction(func);

    /* 2c. Classify phi nodes: rax-resident vs slot-resident. Has to run
     *     before fusion analysis because fusion treats rax-resident phis
     *     as defs-into-rax. */
    irCgClassifyPhis(func);

    /* 2d. Mark fusion pairs (single-use def whose result is consumed by the
     *     immediately following instruction). The emitter and the slot
     *     allocator both honor these flags. */
    irCgAnnotate(func);

    /* 3. Bind known IR values to existing AST loffs. */
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

    /* 4. Reserve slots for return-value alloca and every SSA tmp. */
    irCgAllocAllTmps(&ctx, stack_used);

    /* 5. Extend the stack frame for tmp slots (16-byte aligned). */
    if (ctx.extra_stack > 0) {
        int extra_aligned = align(ctx.extra_stack, 16);
        aoStrCatPrintf(buf, "subq   $%d, %%rsp\n\t", extra_aligned);
    }

    /* 6. Walk blocks in order, emit label (if referenced) + instructions.
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
