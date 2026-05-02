#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "codegen-x86-ir.h"
#include "containers.h"
#include "ir.h"
#include "ir-fold.h"
#include "ir-mem2reg.h"
#include "ir-peephole.h"
#include "ir-regalloc.h"
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
 * each IrInstr (IRCG_FUSE_TO_NEXT, IRCG_R1_IN_REG) when a single-use def is
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

/* IRCG_* flag macros and the IrRaCtx live in ir-regalloc.h - they're
 * shared with the peephole pass which sets the same bits the codegen
 * here reads. */

typedef struct IrCgCtx {
    Cctrl *cc;
    AoStr *buf;
    /* Slot offset map + extra-stack counter + IR function. The
     * regalloc helpers in ir-regalloc.c take an IrRaCtx*; the codegen
     * passes `&ctx->ra` when calling into them. */
    IrRaCtx ra;
    /* Unique block-label prefix per function so multiple slice
     * functions compiled in the same translation unit don't collide. */
    int func_uid;
    /* Set during the per-block emission loop so terminator codegen
     * knows who's emitting and which block (if any) follows in layout
     * order. */
    IrBlock *cur_block;
    IrBlock *next_block;
} IrCgCtx;

static int ircg_func_seq = 0;


/* Load any IR value into the given 64-bit register (rax or rcx). */
static void irCgLoadToReg(IrCgCtx *ctx, IrValue *val, const char *reg) {
    switch (val->kind) {
    case IR_VAL_CONST_INT:
        aoStrCatPrintf(ctx->buf, "movq   $%lld, %%%s\n\t",
                       (long long)val->as._i64, reg);
        return;
    case IR_VAL_CONST_FLOAT:
        /* The 64 bits of the IEEE-754 representation, loaded as raw int.
         * This shows up when a float constant flows through a non-arith
         * path — e.g. `point->x = 3.14;` lowers to IR_STORE_DEREF whose r1
         * must reach the slot via rax. movabsq accepts a 64-bit immediate. */
        aoStrCatPrintf(ctx->buf, "movabsq $0x%lX, %%%s\n\t",
                       (unsigned long)ieee754(val->as._f64), reg);
        return;
    case IR_VAL_CONST_STR:
        /* String literal address. The data section was emitted earlier in
         * asmGenerate with this label, so we just take its address. */
        aoStrCatPrintf(ctx->buf, "leaq   %s(%%rip), %%%s\n\t",
                       val->as.str.label->data, reg);
        return;
    case IR_VAL_TMP:
    case IR_VAL_LOCAL:
    case IR_VAL_PARAM: {
        /* Width-aware: the matching spill (param prologue, IR_LOAD,
         * etc.) writes only the live bytes, so reading 8 unconditionally
         * would pull in the unused gap above the slot. Zero-extend
         * sub-word values, sign-extend i32 (HolyC's native integer is
         * I64 so a narrowed signed value should preserve sign). */
        u32 sz = irValueByteSize(val);
        int loff = irCgGetLoff(&ctx->ra, val);
        if (sz == 1) {
            aoStrCatPrintf(ctx->buf, "movzbq %d(%%rbp), %%%s\n\t",
                           loff, reg);
        } else if (sz == 2) {
            aoStrCatPrintf(ctx->buf, "movzwq %d(%%rbp), %%%s\n\t",
                           loff, reg);
        } else if (sz == 4) {
            aoStrCatPrintf(ctx->buf, "movslq %d(%%rbp), %%%s\n\t",
                           loff, reg);
        } else {
            aoStrCatPrintf(ctx->buf, "movq   %d(%%rbp), %%%s\n\t",
                           loff, reg);
        }
        return;
    }
    default:
        loggerPanic("ir-cg: cannot load value of kind %d\n", val->kind);
    }
}

static void irCgStoreReg(IrCgCtx *ctx, IrValue *dst, const char *reg) {
    aoStrCatPrintf(ctx->buf, "movq   %%%s, %d(%%rbp)\n\t",
                   reg, irCgGetLoff(&ctx->ra, dst));
}

/* Emit `.data <label>: .quad <bits> .text` lazily for a float constant.
 * Returns the label data pointer. We re-emit per occurrence; the linker
 * doesn't dedup but that's fine — the AST codegen does the same. */
static char *irCgEmitFloatLiteralData(IrCgCtx *ctx, double f) {
    char buf[64];
    snprintf(buf, sizeof(buf), ".LIRF%d_%llu",
             ctx->func_uid,
             (unsigned long long)ctx->ra.extra_stack);
    /* extra_stack changes per allocation; combined with func_uid that gives
     * us a unique-enough label. The chosen scheme just needs to avoid
     * collisions inside this translation unit. */
    static int float_seq = 0;
    snprintf(buf, sizeof(buf), ".LIRF%d", float_seq++);

    asmRemovePreviousTab(ctx->buf);
    aoStrCatPrintf(ctx->buf,
                   ".data\n %s:\n\t"
                   ".quad 0x%lX #%.9f\n"
                   ".text\n\t",
                   buf, (unsigned long)ieee754(f), f);

    /* Caller wants a pointer it can use in the next emit; copy into a
     * persistent allocation. */
    int len = (int)strlen(buf);
    char *out = malloc(len + 1);
    memcpy(out, buf, len + 1);
    return out;
}

/* Load any IR value into the given xmm register. */
static void irCgLoadToXmm(IrCgCtx *ctx, IrValue *val, const char *xmm) {
    switch (val->kind) {
    case IR_VAL_CONST_FLOAT: {
        char *lbl = irCgEmitFloatLiteralData(ctx, val->as._f64);
        aoStrCatPrintf(ctx->buf, "movsd  %s(%%rip), %%%s\n\t", lbl, xmm);
        free(lbl);
        return;
    }
    case IR_VAL_CONST_INT: {
        /* The HolyC parser sometimes types `0` as int even when the
         * surrounding context is float. Treat as a float constant. */
        char *lbl = irCgEmitFloatLiteralData(ctx, (double)val->as._i64);
        aoStrCatPrintf(ctx->buf, "movsd  %s(%%rip), %%%s\n\t", lbl, xmm);
        free(lbl);
        return;
    }
    case IR_VAL_TMP:
    case IR_VAL_LOCAL:
    case IR_VAL_PARAM:
        aoStrCatPrintf(ctx->buf, "movsd  %d(%%rbp), %%%s\n\t",
                       irCgGetLoff(&ctx->ra, val), xmm);
        return;
    default:
        loggerPanic("ir-cg: cannot load float of kind %d\n", val->kind);
    }
}

static void irCgStoreXmm(IrCgCtx *ctx, IrValue *dst, const char *xmm) {
    aoStrCatPrintf(ctx->buf, "movsd  %%%s, %d(%%rbp)\n\t",
                   xmm, irCgGetLoff(&ctx->ra, dst));
}

/* Map a cmp kind to the matching `j[cc]` mnemonic. `is_float` picks
 * between signed-int (`jl`/`jle`/...) and unsigned/float (`jb`/`jbe`/...)
 * forms - x86 ucomisd/`cmpq <unsigned>` use the unsigned variants. */
static const char *irCgJccFor(IrCmpKind cmp, int is_float) {
    switch (cmp) {
    case IR_CMP_EQ:  return "je";
    case IR_CMP_NE:  return "jne";
    case IR_CMP_LT:  return is_float ? "jb"  : "jl";
    case IR_CMP_LE:  return is_float ? "jbe" : "jle";
    case IR_CMP_GT:  return is_float ? "ja"  : "jg";
    case IR_CMP_GE:  return is_float ? "jae" : "jge";
    case IR_CMP_ULT: return "jb";
    case IR_CMP_ULE: return "jbe";
    case IR_CMP_UGT: return "ja";
    case IR_CMP_UGE: return "jae";
    default:         return NULL;
    }
}

/* Inverse jcc for "branch if cmp is false". Used when layout has the
 * true target falling through, so we j[invcc] over to the false arm. */
static const char *irCgJccInvFor(IrCmpKind cmp, int is_float) {
    switch (cmp) {
    case IR_CMP_EQ:  return "jne";
    case IR_CMP_NE:  return "je";
    case IR_CMP_LT:  return is_float ? "jae" : "jge";
    case IR_CMP_LE:  return is_float ? "ja"  : "jg";
    case IR_CMP_GT:  return is_float ? "jbe" : "jle";
    case IR_CMP_GE:  return is_float ? "jb"  : "jl";
    case IR_CMP_ULT: return "jae";
    case IR_CMP_ULE: return "ja";
    case IR_CMP_UGT: return "jbe";
    case IR_CMP_UGE: return "jb";
    default:         return NULL;
    }
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


/* Emit the body of a single IR instruction. Block boundaries / labels are
 * handled by the caller. */
/* Helpers wrapping the spill / first-rax-load sites so the bit-flag checks
 * happen in one place. */
static void irCgLoadFirstSrc(IrCgCtx *ctx, IrInstr *instr, IrValue *src) {
    if (instr->flags & IRCG_R1_IN_REG) return;
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
/* True when the predecessor block (`from`) ended with a value-producing
 * instruction whose dst is `v` and which the peephole pass marked
 * IRCG_FUSE_TO_NEXT (suppressed-spill, "value is in %rax going into
 * the JMP"). Lets the phi-mat skip the otherwise-redundant slot
 * reload. */
static int phiPairValueLiveInRax(IrBlock *from, IrValue *v) {
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

static void irCgEmitOnePhi(IrCgCtx *ctx, IrInstr *phi, IrPair *match) {
    IrValue *v = match->ir_value;
    /* If the predecessor's last def was peephole-tail-fused, %rax (for
     * int phis) already holds `v` going into this materialise step;
     * skip both the slot reload and the dangling check below. */
    int in_rax = !irIsFloat(phi->dst->type)
                  && phiPairValueLiveInRax(match->ir_block, v);
    /* mem2reg can emit phi pairs whose tmp value flows from a branch
     * that never wrote anything live (typically the first branch of a
     * promoted variable initialised inside the if/else). Such tmps have
     * no slot — treat them as 0/undef. */
    int v_dangling = !in_rax && v && v->kind == IR_VAL_TMP &&
                     !mapHasInt(ctx->ra.id_to_loff, v->as.var.id);
    if (irIsFloat(phi->dst->type)) {
        if (v_dangling) {
            aoStrCatPrintf(ctx->buf, "xorpd  %%xmm0, %%xmm0\n\t");
        } else {
            irCgLoadToXmm(ctx, v, "xmm0");
        }
        irCgStoreXmm(ctx, phi->dst, "xmm0");
        return;
    }
    if (v_dangling) {
        aoStrCatPrintf(ctx->buf, "xorq   %%rax, %%rax\n\t");
    } else if (!in_rax) {
        irCgLoadToReg(ctx, v, "rax");
    }
    if (!(phi->flags & IRCG_PHI_IN_REG)) {
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

    case IR_LOAD: {
        /* dst = *r1. r1 is a slot pointer — most of the time it's an
         * alloca's 8-byte-aligned slot, but it can also be a GEP-aliased
         * field within a packed class. Pick the load width from the
         * destination type so we don't pull in adjacent-field bytes. */
        int loff = irCgGetLoff(&ctx->ra, instr->r1);
        u32 sz = instr->dst ? instr->dst->as.var.size : 8;
        switch (sz) {
        case 1:
            aoStrCatPrintf(ctx->buf, "movzbq %d(%%rbp), %%rax\n\t", loff);
            break;
        case 2:
            aoStrCatPrintf(ctx->buf, "movzwq %d(%%rbp), %%rax\n\t", loff);
            break;
        case 4:
            aoStrCatPrintf(ctx->buf, "movslq %d(%%rbp), %%rax\n\t", loff);
            break;
        default:
            aoStrCatPrintf(ctx->buf, "movq   %d(%%rbp), %%rax\n\t", loff);
            break;
        }
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_STORE: {
        /* *dst = r1. dst is a slot pointer; r1 is the value. Pick width
         * from r1's value type (irLowerAssign retypes the rhs to match
         * the assignment target so this width comes out right for both
         * direct slot stores and IR_GEP-aliased field writes). */
        irCgLoadFirstSrc(ctx, instr, instr->r1);
        int loff = irCgGetLoff(&ctx->ra, instr->dst);
        u32 sz = irValueByteSize(instr->r1);
        switch (sz) {
        case 1:
            aoStrCatPrintf(ctx->buf, "movb   %%al, %d(%%rbp)\n\t", loff);
            break;
        case 2:
            aoStrCatPrintf(ctx->buf, "movw   %%ax, %d(%%rbp)\n\t", loff);
            break;
        case 4:
            aoStrCatPrintf(ctx->buf, "movl   %%eax, %d(%%rbp)\n\t", loff);
            break;
        default:
            aoStrCatPrintf(ctx->buf, "movq   %%rax, %d(%%rbp)\n\t", loff);
            break;
        }
        break;
    }

    case IR_LOAD_DEREF: {
        /* dst = *r1, where r1 is a runtime pointer value (not a slot id).
         * Load the pointer into rcx, then dereference using a width
         * appropriate for the destination type — `movq` over a 1- or
         * 2-byte value reads adjacent bytes as garbage, which broke
         * U8/U16 deref of array elements (e.g. `pattern[i]`). */
        irCgLoadToReg(ctx, instr->r1, "rcx");
        u32 sz = instr->dst ? instr->dst->as.var.size : 8;
        switch (sz) {
        case 1: aoStrCatPrintf(ctx->buf, "movzbq (%%rcx), %%rax\n\t"); break;
        case 2: aoStrCatPrintf(ctx->buf, "movzwq (%%rcx), %%rax\n\t"); break;
        case 4: aoStrCatPrintf(ctx->buf, "movslq (%%rcx), %%rax\n\t"); break;
        default: aoStrCatPrintf(ctx->buf, "movq   (%%rcx), %%rax\n\t"); break;
        }
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_STORE_DEREF: {
        /* *dst = r1; dst holds a runtime pointer, r1 is the value.
         * Pick the store width from r1's value type — writing 8 bytes
         * through a U8/U16/I32 pointer would trample adjacent struct
         * fields. Mirrors IR_LOAD_DEREF. */
        if (instr->flags & IRCG_DST_IN_REG) {
            /* Peephole-fused with the prior rax-defining instr: its
             * value is the address we're storing through. Stash %rax
             * in %rcx before the r1 load clobbers it. */
            aoStrCatPrintf(ctx->buf, "movq   %%rax, %%rcx\n\t");
            irCgLoadFirstSrc(ctx, instr, instr->r1);
        } else {
            irCgLoadFirstSrc(ctx, instr, instr->r1);
            irCgLoadToReg(ctx, instr->dst, "rcx");
        }
        u32 sz = irValueByteSize(instr->r1);
        switch (sz) {
        case 1: aoStrCatPrintf(ctx->buf, "movb   %%al, (%%rcx)\n\t"); break;
        case 2: aoStrCatPrintf(ctx->buf, "movw   %%ax, (%%rcx)\n\t"); break;
        case 4: aoStrCatPrintf(ctx->buf, "movl   %%eax, (%%rcx)\n\t"); break;
        default: aoStrCatPrintf(ctx->buf, "movq   %%rax, (%%rcx)\n\t"); break;
        }
        break;
    }

    case IR_LEA: {
        /* dst = &r1. r1 is either a stack slot (TMP/LOCAL/PARAM) or a
         * global symbol (IR_VAL_GLOBAL); pick the right addressing mode.
         * Function names need asmNormaliseFunctionName(); plain data
         * labels go in verbatim. */
        if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
            const char *name = instr->r1->as.global.name->data;
            if (instr->r1->flags & IR_VAL_FLAG_FUNC) {
                name = asmNormaliseFunctionName((char *)name);
            }
            aoStrCatPrintf(ctx->buf, "leaq   %s(%%rip), %%rax\n\t", name);
        } else {
            int loff = irCgGetLoff(&ctx->ra, instr->r1);
            aoStrCatPrintf(ctx->buf, "leaq   %d(%%rbp), %%rax\n\t", loff);
        }
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
        /* Peephole-fused with the next IR_BR: skip setcc + movzbq +
         * spill and let the BR turn the flags into a direct j[cc]. */
        if (instr->flags & IRCG_CMP_FUSED_BR) break;
        irCgEmitSetCC(ctx, instr->extra.cmp_kind);
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_FADD:
    case IR_FSUB:
    case IR_FMUL:
    case IR_FDIV: {
        const char *op = (instr->op == IR_FADD) ? "addsd"
                       : (instr->op == IR_FSUB) ? "subsd"
                       : (instr->op == IR_FMUL) ? "mulsd"
                                                : "divsd";
        irCgLoadToXmm(ctx, instr->r1, "xmm0");
        irCgLoadToXmm(ctx, instr->r2, "xmm1");
        aoStrCatPrintf(ctx->buf, "%s  %%xmm1, %%xmm0\n\t", op);
        irCgStoreXmm(ctx, instr->dst, "xmm0");
        break;
    }

    case IR_FNEG: {
        /* Flip the sign bit by XORing with the 64-bit `sign_bit` constant
         * defined in asmDataSection. */
        irCgLoadToXmm(ctx, instr->r1, "xmm0");
        aoStrCatPrintf(ctx->buf,
                       "movsd  sign_bit(%%rip), %%xmm1\n\t"
                       "xorpd  %%xmm1, %%xmm0\n\t");
        irCgStoreXmm(ctx, instr->dst, "xmm0");
        break;
    }

    case IR_INEG: {
        irCgLoadFirstSrc(ctx, instr, instr->r1);
        aoStrCatPrintf(ctx->buf, "negq   %%rax\n\t");
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_NOT: {
        irCgLoadFirstSrc(ctx, instr, instr->r1);
        aoStrCatPrintf(ctx->buf, "notq   %%rax\n\t");
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_SITOFP: {
        /* signed int -> F64. Operand is in rax, result lands in xmm0. */
        irCgLoadToReg(ctx, instr->r1, "rax");
        aoStrCatPrintf(ctx->buf, "cvtsi2sdq %%rax, %%xmm0\n\t");
        irCgStoreXmm(ctx, instr->dst, "xmm0");
        break;
    }

    case IR_BITCAST: {
        /* Reinterpret an 8-byte value's bits between the integer and
         * float register files (no numerical conversion). HolyC uses
         * this for `argv[i](F64)` — the variadic slot was pushed as
         * raw 8 bytes that should be read back as F64, not converted
         * from the integer interpretation. Direction is determined by
         * the dst's type tag. */
        if (instr->dst && irIsFloat(instr->dst->type)) {
            irCgLoadToReg(ctx, instr->r1, "rax");
            aoStrCatPrintf(ctx->buf, "movq   %%rax, %%xmm0\n\t");
            irCgStoreXmm(ctx, instr->dst, "xmm0");
        } else {
            irCgLoadToXmm(ctx, instr->r1, "xmm0");
            aoStrCatPrintf(ctx->buf, "movq   %%xmm0, %%rax\n\t");
            irCgSpillDst(ctx, instr, "rax");
        }
        break;
    }

    case IR_FPTOSI: {
        /* F64 -> signed int (truncation toward zero). */
        irCgLoadToXmm(ctx, instr->r1, "xmm0");
        aoStrCatPrintf(ctx->buf, "cvttsd2siq %%xmm0, %%rax\n\t");
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_TRUNC: {
        /* Narrow an integer to a smaller width by masking the low bits.
         * Signedness is irrelevant for truncation itself; sign-extension
         * happens on the next read if the destination type is signed. */
        irCgLoadFirstSrc(ctx, instr, instr->r1);
        u32 sz = instr->dst ? instr->dst->as.var.size : 8;
        switch (sz) {
        case 1:
            aoStrCatPrintf(ctx->buf, "andq   $0xFF, %%rax\n\t");
            break;
        case 2:
            aoStrCatPrintf(ctx->buf, "andq   $0xFFFF, %%rax\n\t");
            break;
        case 4:
            aoStrCatPrintf(ctx->buf, "movl   %%eax, %%eax\n\t");
            break;
        default:
            /* No-op for sizes we don't actually narrow. */
            break;
        }
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_FCMP: {
        /* ucomisd sets ZF/CF/PF; setcc lowers to {EQ,NE,LT,LE,GT,GE}.
         * NaN handling is not modeled — the slice doesn't surface OEQ/UNO
         * etc. and matches the AST codegen's plain set{e,ne,b,be,a,ae}. */
        irCgLoadToXmm(ctx, instr->r1, "xmm0");
        irCgLoadToXmm(ctx, instr->r2, "xmm1");
        aoStrCatPrintf(ctx->buf, "ucomisd %%xmm1, %%xmm0\n\t");
        /* Peephole-fused with the next IR_BR. */
        if (instr->flags & IRCG_CMP_FUSED_BR) break;
        const char *cc = NULL;
        switch (instr->extra.cmp_kind) {
        case IR_CMP_EQ: cc = "sete";  break;
        case IR_CMP_NE: cc = "setne"; break;
        case IR_CMP_LT: cc = "setb";  break;
        case IR_CMP_LE: cc = "setbe"; break;
        case IR_CMP_GT: cc = "seta";  break;
        case IR_CMP_GE: cc = "setae"; break;
        default:
            loggerPanic("ir-cg: unsupported FCMP kind %d\n",
                        instr->extra.cmp_kind);
        }
        aoStrCatPrintf(ctx->buf, "%s   %%al\n\tmovzbq %%al, %%rax\n\t", cc);
        irCgSpillDst(ctx, instr, "rax");
        break;
    }

    case IR_BR: {
        IrBlock *t = instr->extra.blocks.target_block;
        IrBlock *f = instr->extra.blocks.fallthrough_block;
        char tlbl[64], flbl[64];
        irCgBlockLabel(ctx, t, tlbl, sizeof(tlbl));
        irCgBlockLabel(ctx, f, flbl, sizeof(flbl));

        /* Peephole-fused with the immediately-prior IR_ICMP / IR_FCMP:
         * the cmp's flags are still live (it skipped setcc/spill), so
         * we can branch directly off them instead of the usual
         * `load-cond-into-rax + testq + je/jne` triple. */
        int fused_br = (instr->flags & IRCG_BR_USE_PRIOR_CMP) != 0;
        IrCmpKind fused_kind = IR_CMP_INVALID;
        int fused_is_float = 0;
        if (fused_br) {
            for (List *node = ctx->cur_block->instructions->prev;
                 node != ctx->cur_block->instructions;
                 node = node->prev)
            {
                IrInstr *prev = (IrInstr *)node->value;
                if (prev == instr) continue;
                if (prev->op == IR_NOP) continue;
                fused_kind = prev->extra.cmp_kind;
                fused_is_float = (prev->op == IR_FCMP);
                break;
            }
        }

        if (!fused_br) irCgLoadFirstSrc(ctx, instr, instr->dst);

        int t_phi = blockHasPhi(t);
        int f_phi = blockHasPhi(f);
        if (!t_phi && !f_phi) {
            /* Layout-aware: avoid emitting a `jmp` that's an immediate
             * fall-through. If the next block is the true target, use
             * `je flbl` and fall through. If it's the false target,
             * invert to `jne tlbl`. Otherwise emit both jumps. */
            if (fused_br) {
                const char *jcc_t = irCgJccFor(fused_kind, fused_is_float);
                const char *jcc_f = irCgJccInvFor(fused_kind, fused_is_float);
                if (ctx->next_block == t) {
                    aoStrCatPrintf(ctx->buf, "%s    %s\n\t", jcc_f, flbl);
                } else if (ctx->next_block == f) {
                    aoStrCatPrintf(ctx->buf, "%s    %s\n\t", jcc_t, tlbl);
                } else {
                    aoStrCatPrintf(ctx->buf,
                                   "%s    %s\n\t"
                                   "jmp    %s\n\t",
                                   jcc_t, tlbl, flbl);
                }
                break;
            }
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
            if (fused_br) {
                const char *jcc_f = irCgJccInvFor(fused_kind, fused_is_float);
                aoStrCatPrintf(ctx->buf, "%s    %s\n\t", jcc_f, else_lbl);
            } else {
                aoStrCatPrintf(ctx->buf,
                               "testq  %%rax, %%rax\n\t"
                               "je     %s\n\t",
                               else_lbl);
            }
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
            if (irIsFloat(instr->dst->type)) {
                irCgLoadToXmm(ctx, instr->dst, "xmm0");
            } else {
                irCgLoadFirstSrc(ctx, instr, instr->dst);
            }
        }
        aoStrCatPrintf(ctx->buf, "leave\n\tret\n");
        break;

    case IR_CALL: {
        /* r1 is the args wrapper carrying the Vec of arg IrValues plus the
         * function name as as.array.label. Three calling conventions:
         *
         *   SysV non-variadic: int-class args in rdi/rsi/rdx/rcx/r8/r9,
         *     float-class in xmm0..xmm7.
         *
         *   SysV extern variadic (e.g. printf): same regs, plus %al = the
         *     number of XMM regs used. We set %al whenever any float args
         *     were passed (harmless for non-variadic callees).
         *
         *   HolyC variadic (e.g. MStrPrint): the parser injects an extra
         *     I64 count argument right after the fixed params; that goes
         *     in the next int reg. The variadic *values* go on the stack
         *     in source order (pushed in reverse). The callee reads them
         *     via positive offsets from %rbp.
         *
         * Indirect call (AST_FUNPTR_CALL): wrap->as.array.label is NULL
         * and the function-pointer source lives in instr->r2. We load it
         * into %r11 before the arg loads so arg evaluation can't clobber
         * it. */
        static const char *kIntRegs[] = {
            "rdi", "rsi", "rdx", "rcx", "r8", "r9"
        };
        static const char *kFloatRegs[] = {
            "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"
        };
        IrValue *wrap = instr->r1;
        Vec *args = wrap ? wrap->as.array.values : NULL;
        AoStr *fname = wrap ? wrap->as.array.label : NULL;
        int indirect = (fname == NULL);
        if (indirect) {
            if (!instr->r2) {
                loggerPanic("ir-cg: indirect IR_CALL without target\n");
            }
            irCgLoadToReg(ctx, instr->r2, "r11");
        }

        /* Discover the callee so we can route HolyC-variadic args to the
         * stack. SysV-extern variadic doesn't need this — args still go
         * in regs. */
        Ast *callee = NULL;
        if (!indirect) {
            callee = (Ast *)mapGetLen(ctx->cc->global_env,
                                       fname->data, fname->len);
        }
        /* The parser drops has_var_args on extern decls; fall back to
         * checking for a trailing AST_VAR_ARGS in the params list. */
        int callee_va = 0;
        if (callee) {
            if ((callee->type && callee->type->has_var_args) ||
                callee->has_var_args) {
                callee_va = 1;
            } else if (callee->params && callee->params->size > 0) {
                Ast *last = vecGet(Ast *, callee->params,
                                   callee->params->size - 1);
                if (last && last->kind == AST_VAR_ARGS) callee_va = 1;
            }
        }
        int holyc_variadic =
            callee_va && callee->kind != AST_EXTERN_FUNC;
        int extern_variadic =
            callee_va && callee->kind == AST_EXTERN_FUNC;

        /* var_arg_start = number of args that go in registers. The +1 is
         * the count argument that the parser injects right before the
         * variadic values. For non-variadic callees, var_arg_start
         * effectively means "all args go in regs". */
        int var_arg_start = -1;
        if (holyc_variadic && callee->params) {
            for (u64 i = 0; i < callee->params->size; ++i) {
                Ast *p = vecGet(Ast *, callee->params, i);
                var_arg_start++;
                if (p->kind == AST_VAR_ARGS) break;
            }
            var_arg_start += 1;  /* matches asmPrepFuncCallArgs */
        }

        u64 n = args ? args->size : 0;

        /* Partition args into reg-bound vs stack-bound.
         *
         * HolyC-variadic: first var_arg_start args in regs, rest on
         * stack regardless of type.
         *
         * SysV extern-variadic (printf etc.): walk left-to-right; an arg
         * goes in regs while there's a slot of the right kind (int_idx<6
         * for ints, float_idx<8 for floats); overflow into the stack
         * preserves source order at the call site (see push-in-reverse
         * below).
         *
         * Non-variadic: every arg is reg-bound (eligibility caps total
         * to 6 int / 8 float). */
        u8 *is_stack = (n > 0)
            ? (u8 *)alloca(sizeof(u8) * n) : NULL;
        if (is_stack) memset(is_stack, 0, n);
        int n_stack_total = 0;
        if (holyc_variadic && (s64)n > var_arg_start) {
            for (u64 i = (u64)var_arg_start; i < n; ++i) {
                is_stack[i] = 1;
                n_stack_total++;
            }
        } else if (extern_variadic) {
            int probe_int = 0, probe_float = 0;
            for (u64 i = 0; i < n; ++i) {
                IrValue *a = vecGet(IrValue *, args, i);
                int is_f = irIsFloat(a->type);
                if (is_f) {
                    if (probe_float >= 8) { is_stack[i] = 1; n_stack_total++; }
                    else probe_float++;
                } else {
                    if (probe_int >= 6) { is_stack[i] = 1; n_stack_total++; }
                    else probe_int++;
                }
            }
        }

        /* If any stack args, ensure %rsp ends up 16-byte-aligned at the
         * call. Each push is 8 bytes; an odd count needs an 8-byte pad. */
        int needs_pad = (n_stack_total & 1) ? 1 : 0;
        if (needs_pad) {
            aoStrCatPrintf(ctx->buf, "subq   $8, %%rsp\n\t");
        }

        /* Push stack args in reverse order so the first one ends up at
         * the lowest stack offset (rsp+0 after all pushes). */
        int n_stack = 0;
        for (s64 i = (s64)n - 1; i >= 0; --i) {
            if (!is_stack[i]) continue;
            IrValue *a = vecGet(IrValue *, args, (u64)i);
            if (irIsFloat(a->type)) {
                irCgLoadToXmm(ctx, a, "xmm0");
                aoStrCatPrintf(ctx->buf,
                               "subq   $8, %%rsp\n\t"
                               "movsd  %%xmm0, (%%rsp)\n\t");
            } else {
                irCgLoadToReg(ctx, a, "rax");
                aoStrCatPrintf(ctx->buf, "pushq  %%rax\n\t");
            }
            n_stack++;
        }

        /* Reg args: every arg not flagged for the stack. */
        int int_idx = 0, float_idx = 0;
        for (u64 i = 0; i < n; ++i) {
            if (is_stack[i]) continue;
            IrValue *a = vecGet(IrValue *, args, i);
            if (irIsFloat(a->type)) {
                if (float_idx >= 8) {
                    loggerPanic("ir-cg: too many float args (slice limit 8)\n");
                }
                irCgLoadToXmm(ctx, a, kFloatRegs[float_idx++]);
            } else {
                if (int_idx >= 6) {
                    loggerPanic("ir-cg: too many int args (slice limit 6)\n");
                }
                irCgLoadToReg(ctx, a, kIntRegs[int_idx++]);
            }
        }

        /* SysV variadic ABI: %al holds the count of SSE registers used.
         * The AST codegen sets it whenever any float args are passed. */
        if (float_idx > 0) {
            aoStrCatPrintf(ctx->buf, "movl   $%d, %%eax\n\t", float_idx);
        }

        if (indirect) {
            aoStrCatPrintf(ctx->buf, "call   *%%r11\n\t");
        } else {
            aoStrCatPrintf(ctx->buf, "call   %s\n\t",
                           asmNormaliseFunctionName(fname->data));
        }

        /* Tear down stack args + alignment pad. */
        int teardown = n_stack * 8 + (needs_pad ? 8 : 0);
        if (teardown > 0) {
            aoStrCatPrintf(ctx->buf, "addq   $%d, %%rsp\n\t", teardown);
        }

        if (instr->dst && instr->dst->type != IR_TYPE_VOID &&
            instr->dst->kind == IR_VAL_TMP) {
            if (irIsFloat(instr->dst->type)) {
                irCgStoreXmm(ctx, instr->dst, "xmm0");
            } else {
                irCgSpillDst(ctx, instr, "rax");
            }
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

/* Narrow sub-register names for the System V int-arg registers,
 * indexed `[width][reg]`. Width 1 = byte, 2 = word, 4 = dword,
 * 8 = qword. Used to spill a U8/U16/I32/U32 param with the matching
 * mov mnemonic instead of always going through the full %r* register. */
static const char *kIntRegByWidth[4][6] = {
    /* 1 byte */ {"dil",  "sil",  "dl",   "cl",   "r8b",  "r9b"  },
    /* 2 byte */ {"di",   "si",   "dx",   "cx",   "r8w",  "r9w"  },
    /* 4 byte */ {"edi",  "esi",  "edx",  "ecx",  "r8d",  "r9d"  },
    /* 8 byte */ {"rdi",  "rsi",  "rdx",  "rcx",  "r8",   "r9"   },
};
static const char *kIntMovByWidth[4] = {"movb", "movw", "movl", "movq"};

static const char *intRegName(int width, int idx) {
    int w = (width == 1) ? 0 : (width == 2) ? 1 : (width == 4) ? 2 : 3;
    return kIntRegByWidth[w][idx];
}
static const char *intMovMnemonic(int width) {
    int w = (width == 1) ? 0 : (width == 2) ? 1 : (width == 4) ? 2 : 3;
    return kIntMovByWidth[w];
}

static void irCgEmitParamSpills(AoStr *buf, Ast *func) {
    static const char *kIntRegs[] = {
        "rdi", "rsi", "rdx", "rcx", "r8", "r9"
    };
    static const char *kFloatRegs[] = {
        "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7"
    };
    int int_idx = 0, float_idx = 0;
    /* Struct-by-value return: %rdi holds the caller-provided buffer
     * pointer; spill it to the hidden slot the layout pass reserved
     * (loff stashed on `func->loff` for AST_FUNC). User params then
     * occupy rsi onwards. */
    AstType *rt = func->type ? func->type->rettype : NULL;
    if (rt && (rt->kind == AST_TYPE_CLASS || rt->kind == AST_TYPE_UNION)
        && !rt->is_intrinsic && rt->size > 0) {
        aoStrCatPrintf(buf, "movq   %%rdi, %d(%%rbp)\n\t", func->loff);
        int_idx = 1;
    }
    if (!func->params) return;
    for (u64 i = 0; i < func->params->size; ++i) {
        Ast *p = vecGet(Ast *, func->params, i);
        if (p->kind == AST_VAR_ARGS) {
            /* HolyC variadic ABI:
             *   - argc (count of variadic values) is in the next-after-
             *     fixed integer register (rdi/rsi/rdx/rcx/r8/r9). Spill
             *     it into argc's slot.
             *   - argv is positioned at +16(%rbp) — no register, no
             *     spill: the body's `&argv` (LEA on the slot id) directly
             *     produces the variadic-area pointer because the loff
             *     bound by the layout pass is +16. */
            aoStrCatPrintf(buf, "movq   %%%s, %d(%%rbp)\n\t",
                           kIntRegs[int_idx++], p->argc->loff);
            continue;
        }
        AstType *ptype = (p->kind == AST_DEFAULT_PARAM)
                         ? p->declvar->type : p->type;
        if (ptype && ptype->kind == AST_TYPE_FLOAT) {
            aoStrCatPrintf(buf, "movsd  %%%s, %d(%%rbp)\n\t",
                           kFloatRegs[float_idx++], p->loff);
        } else {
            /* Narrow params (Bool, U8, U16, I32...) get a width-matching
             * mov + sub-register so the spill writes only the live bytes;
             * the gap above the slot stays whatever the caller left. The
             * matching `irCgLoadToReg` reads back through `mov{zb,zw,sl}q`,
             * zero/sign-extending into %rax. Pointers, I64 and FUNPTR
             * use plain movq. */
            int sz = (p->kind == AST_FUNPTR) ? 8 :
                     (ptype ? ptype->size : 8);
            if (sz != 1 && sz != 2 && sz != 4) sz = 8;
            aoStrCatPrintf(buf, "%s   %%%s, %d(%%rbp)\n\t",
                           intMovMnemonic(sz),
                           intRegName(sz, int_idx),
                           p->loff);
            int_idx++;
        }
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
    /* Peephole runs *after* annotation: the annotation pass clears
     * IRCG_FUSE_TO_NEXT / IRCG_R1_IN_REG before its own re-marking,
     * so any flag the peephole sets earlier would be wiped. */
    irPeephole(func);

    /* 3. Compute AST-side layout *after* mem2reg so promoted locals
     *    don't reserve dead slots. */
    int locals_params_space = irCgComputeAstLayout(ast_func, func);

    /* 4. Bind IR values to the loffs we just computed. */
    IrCgCtx ctx;
    ctx.cc = cc;
    ctx.buf = buf;
    ctx.ra.func = func;
    ctx.ra.id_to_loff = mapNew(32, &map_uint_to_uint_type);
    ctx.ra.extra_stack = 0;
    ctx.func_uid = ircg_func_seq++;
    ctx.cur_block = NULL;
    ctx.next_block = NULL;
    irCgBindAstLoffs(&ctx.ra, ast_func);

    /* 5. Allocate slots for IR tmps starting just below the params area. */
    irCgAllocAllTmps(&ctx.ra, locals_params_space);

    /* 6. Emit prologue with one subq covering params + locals + tmps. */
    int total_stack = locals_params_space + ctx.ra.extra_stack;
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

    mapRelease(ctx.ra.id_to_loff);
    free(ir_ctx->prog);
    free(ir_ctx);
}
