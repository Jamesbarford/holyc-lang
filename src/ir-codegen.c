#include <assert.h>
#include <math.h>
#include <stdlib.h>

#include "ast.h"
#include "containers.h"
#include "ir.h"
#include "ir-codegen.h"
#include "ir-fold.h"
#include "ir-mem2reg.h"
#include "ir-peephole.h"
#include "ir-regalloc.h"
#include "ir-types.h"
#include "list.h"
#include "prsutil.h"
#include "util.h"

void irCgEmitPhiMaterialize(IrCgCtx *ctx, IrBlock *from, IrBlock *to,
                            ir_cg_emit_one_phi *emit_one_phi) {
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
                emit_one_phi(ctx, phis[i], pairs[i]);
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
/* True when the predecessor block (`from`) ended with a value-producing
 * instruction whose dst is `v` and which the peephole pass marked
 * IRCG_FUSE_TO_NEXT (suppressed-spill, "value is in %rax going into
 * the JMP"). Lets the phi-mat skip the otherwise-redundant slot
 * reload. */
int phiPairValueLiveInRax(IrBlock *from, IrValue *v) {
    if (!from || !v || v->kind != IR_VAL_TMP) return 0;
    if (listEmpty(from->instructions)) return 0;
    for (List *node = from->instructions->prev;
         node != from->instructions;
         node = node->prev) {
        IrInstr *I = (IrInstr *)node->value;
        if (I->op == IR_NOP) continue;
        if (I->op == IR_JMP || I->op == IR_BR ||
            I->op == IR_LOOP || I->op == IR_RET) continue;
        if (!I->dst || I->dst->kind != IR_VAL_TMP) return 0;
        if (I->dst->as.var.id != v->as.var.id) return 0;
        return (I->flags & IRCG_FUSE_TO_NEXT) != 0;
    }
    return 0;
}

/* True when `val` is a constant integer whose value fits in a sign-
 * extended 32-bit immediate - the largest immediate form x86_64
 * arithmetic / compare instructions accept. Larger 64-bit constants
 * have no immediate encoding and must go through a register. */
int irCgIsImm32(IrValue *val, s64 *out) {
    if (!val || val->kind != IR_VAL_CONST_INT) return 0;
    s64 n = val->as._i64;
    if (n < INT32_MIN || n > INT32_MAX) return 0;
    *out = n;
    return 1;
}

uint64_t ieee754(double _f64) {
    if (_f64 == 0.0) return 0;  // Handle zero value explicitly

    // Calculate exponent and adjust fraction
    double base2_exp = floorl(log2l(fabs(_f64)));
    double exponet2_removed = ldexpl(_f64, -base2_exp - 1);

    // Initialize fraction and calculate it bit by bit
    uint64_t fraction = 0;
    double digit = 0.5;  // Start with 1/2
    for (s64 i = 0; i != 53; i++) {
        if (exponet2_removed >= digit) {
            exponet2_removed -= digit;
            fraction |= 1ULL << (52 - i);
        }
        digit *= 0.5;  // Move to the next digit (1/4, 1/8, ...)
    }

    // Calculate exponent representation
    uint64_t exponent = ((1 << 10) - 1) + base2_exp;

    // Handle sign bit
    uint64_t sign = (_f64 < 0.0) ? 1 : 0;

    // Assemble the IEEE 754 representation
    return (sign << 63) |
           ((exponent & 0x7FF) << 52) |
           (fraction & ~(1ULL << 52));
}

/* Per-translation-unit counter so block labels in two functions
 * compiled together can't collide. */
static int ircg_func_seq = 0;

IrCgPrepared irCgPrepareFunction(Cctrl *cc, Ast *ast_func, AoStr *buf,
                                  IrCgCtx *ctx) {
    assert(ast_func->kind == AST_FUNC);

    /* 1. Lower to IR. */
    IrCtx *ir_ctx = irCtxNew(cc);
    IrFunction *func = irLowerFunction(ir_ctx, ast_func);

    /* 2. Run analyses. mem2reg promotes allocas away; the layout pass
     *    below uses that info to skip slots for promoted locals.
     *    Peephole runs *after* the annotation pass, since the
     *    annotation pass clears IRCG_FUSE_TO_NEXT / IRCG_R1_IN_REG
     *    before its own re-marking - any flag the peephole set
     *    earlier would be wiped. */
    irMem2Reg(func);
    irFoldFunction(func);
    irCgClassifyPhis(func);
    irCgAnnotate(func);
    irPeephole(cc, func);

    /* 3. AST-driven layout (after mem2reg so promoted locals don't
     *    reserve dead slots). */
    int locals_params_space = irCgComputeAstLayout(ast_func, func);

    /* 4. Initialise the codegen context. */
    ctx->cc = cc;
    ctx->buf = buf;
    ctx->ra.func = func;
    ctx->ra.id_to_loff = mapNew(32, &map_uint_to_uint_type);
    ctx->ra.extra_stack = 0;
    ctx->func_uid = ircg_func_seq++;
    ctx->cur_block = NULL;
    ctx->next_block = NULL;
    /* Index every peephole-deferred LEA by its dst tmp id, so the
     * IR_CALL emit can re-create the leaq into the arg register. */
    ctx->lea_inline_map = mapNew(8, &map_uint_to_uint_type);
    for (List *bn = func->blocks->next;
         bn != func->blocks;
         bn = bn->next) {
        IrBlock *b = (IrBlock *)bn->value;
        for (List *in = b->instructions->next;
             in != b->instructions;
             in = in->next) {
            IrInstr *I = (IrInstr *)in->value;
            if (I->op != IR_LEA) continue;
            if (!(I->flags & IRCG_LEA_INLINE_AT_CALL)) continue;
            if (!I->dst || I->dst->kind != IR_VAL_TMP) continue;
            mapAdd(ctx->lea_inline_map,
                   (void *)(u64)I->dst->as.var.id, (void *)I);
        }
    }
    irCgBindAstLoffs(&ctx->ra, ast_func);

    /* 5. Allocate slots for IR tmps just below the params area. */
    irCgAllocAllTmps(&ctx->ra, locals_params_space);

    /* 6. Compute total stack reservation (params + locals + tmps),
     *    aligned to 16. The architecture-specific emitter feeds this
     *    into its prologue. */
    int total_stack = locals_params_space + ctx->ra.extra_stack;
    if (total_stack > 0) total_stack = align(total_stack, 16);

    IrCgPrepared prep = { ir_ctx, func, total_stack };
    return prep;
}

void irCgFinishFunction(IrCgPrepared *prep, IrCgCtx *ctx) {
    if (ctx) {
        if (ctx->ra.id_to_loff) mapRelease(ctx->ra.id_to_loff);
        if (ctx->lea_inline_map) mapRelease(ctx->lea_inline_map);
    }
    if (prep && prep->ir_ctx) {
        free(prep->ir_ctx->prog);
        free(prep->ir_ctx);
    }
}
