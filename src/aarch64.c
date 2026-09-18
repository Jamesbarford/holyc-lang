/* AArch64 backend: one instruction selector, three sinks.
 *
 * The selector lowers IR into machine IR (MirFn - an AsmBlock under
 * construction, see machine/mir.h) and never touches bytes or text
 * itself. What happens to the lines depends on who asked:
 *
 *   -jit / -repl   the lines are encoded straight into the chunk's
 *                  AsmEnc (aarch64_encode_instr), then mapped RX and
 *                  patched by asm_jit_finalize. Addresses of globals
 *                  and host functions are known now, so they are
 *                  materialised inline (HCC_ADDR_ABSOLUTE).
 *   object writer  the same, with HCC_ADDR_RELOCATED selected so every
 *                  global reference is an adrp/add pair with a fixup.
 *   -S / AOT text  the lines are printed in GNU-as dialect
 *                  (asm_line_format_gas) and handed to the system
 *                  assembler. Same selection, same peephole, so what
 *                  `-S` shows is what the JIT runs.
 *
 * Register roles: x0/x1/x2 scratch, d0/d1 FP scratch, x29 fp, x30 lr,
 * x8 indirect-result, x9 large-offset pivot / struct address, x10
 * addressing scratch, x16 indirect-call target. */

#include <dlfcn.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <strings.h>

#include "aarch64.h"
#include "aostr.h"
#include "asm.h"
#include "ast.h"
#include "cctrl.h"
#include "cli.h"
#include "containers.h"
#include "ir-inliner.h"
#include "ir-optimise.h"
#include "ir-regalloc.h"
#include "ir-types.h"
#include "ir.h"
#include "jit-common.h"
#include "list.h"
#include "machine/asm.h"
#include "machine/asm_enc.h"
#include "machine/asm_jit.h"
#include "machine/enc_arm64.h"
#include "machine/mir.h"
#include "util.h"

/* ---------------- register pool ---------------- */


static const char *const kIntRegs64[] = {
    "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"
};
static const char *const kFloatRegs[] = {
    "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7"
};

static Vec *aarch64MakeAoStrVec(const char *const *src, int n) {
    Vec *v = vecNew(&vec_aostr_type);
    for (int i = 0; i < n; ++i) {
        vecPush(v, aoStrDupRaw((char *)src[i], strlen(src[i])));
    }
    return v;
}

/* Which of x0/x1/x2/d0/d1 does emitting `I` actually write, by the
 * point `at`? Mirrors a64EmitArith's fixed shape: r1 is materialised
 * into x0/d0, then r2 into x1/d1, then the result spills out of x0/d0.
 *
 * Conservative by default - only the cases audited against the
 * emitters answer "not clobbered", so an opcode nobody has thought
 * about keeps the old blanket behaviour. */
static const char *a64HomeReg(IrValue *v);

/* Will a deref store have to put its address or index INTO a scratch
 * register before it can read its value?
 *
 * Mirrors what the emitter does: `a64BaseHome` uses the address's own
 * register when it has one, `aarch64IdxHome` likewise for the index,
 * and only a value with neither goes through `a64ScratchTake`. Kept
 * beside them deliberately - the two must agree, and the failure mode
 * if they drift is a source read out of a register something else has
 * since written.
 *
 * Deliberately coarse in one respect: it answers for ANY scratch
 * register rather than working out which one would be taken. The
 * refinement worth having is "nothing is materialised at all", and
 * that is exactly what this tests. */
static int aarch64DerefTakesScratch(IrInstr *I) {
    /* An address computed rather than held - a global's symbol has to
     * be materialised, and anything without a home is loaded. */
    if (!I->dst) return 1;
    if (I->dst->kind == IR_VAL_GLOBAL) return 1;
    if (!a64HomeReg(I->dst)) return 1;
    /* `a64BaseHome` refuses a home for the one shape that would mutate
     * it, and the emitter then takes scratch after all. */
    if (I->idx && I->scale && I->disp != 0) return 1;
    /* An index is only free when it too is already in a register. */
    if (I->idx && I->scale && !a64HomeReg(I->idx)) return 1;
    return 0;
}

static int aarch64OpClobbers(IrInstr *I, AoStr *reg, IrClobberPoint at) {
    const char *r = reg->data;
    int is_scratch0 = !strcmp(r, "x0") || !strcmp(r, "d0");

    switch (at) {
        case IR_CLOBBER_BEFORE_DST:
            /* Only the deref stores ask: their dst is the ADDRESS, and
             * it is the first thing the emitter touches. Nothing has
             * run, so nothing is clobbered. */
            return 0;

        case IR_CLOBBER_BEFORE_R1:
            /* r1 is the first thing materialised, so nothing has been
             * written yet - except for the two deref stores, where r1
             * is the VALUE and comes last, after the address and the
             * index have been put where they need to be.
             *
             * Those two only burn scratch when they have to compute
             * something: an address already sitting in a register is
             * used where it is, and so is an index. When neither needs
             * materialising, nothing is written before the value is
             * read and a source in a scratch register survives after
             * all. Answering a flat "clobbered" here is what kept
             * `f->e[i] = v` copying every operand into place first. */
            if (I->op == IR_STORE_DEREF || I->op == IR_RMW_DEREF) {
                return aarch64DerefTakesScratch(I);
            }
            return 0;

        case IR_CLOBBER_BEFORE_R2:
            if (I->op == IR_STORE_DEREF || I->op == IR_RMW_DEREF) return 1;
            /* r1 has landed in scratch 0 by now; x1/x2 are still
             * untouched, so a source living there survives. */
            return is_scratch0;

        case IR_CLOBBER_AFTER:
        default:
            /* a64TxtSrcReg stores a reg-resident value straight out of
             * its own register - no load, no scratch touched. Only a
             * source needing materialisation burns scratch 0. This is
             * what lets consecutive param spills all forward.
             *
             * A pinned destination is written via `mov <pinned>, src`,
             * so it clobbers whatever register it names. */
            if (I->op == IR_STORE) {
                if (I->dst && I->dst->pinned_reg) return 1;
                int src_in_reg = I->r1 && I->r1->loc.kind == IR_LOC_REG &&
                                 I->r1->loc.as.reg;
                return src_in_reg ? 0 : is_scratch0;
            }
            return 1;
    }
}

void aarch64InitRegPool(void) {
    static int initialised = 0;
    static IrRegPool pool;
    if (initialised) return;

    static const char *kScratchRegs[] = { "x0", "x1", "x2", "d0", "d1" };
    /* AAPCS64: x19-x28 and the low 64 bits of d8-d15 belong to the
     * caller. A `reg X19 x` binding writes one, so the function has to
     * put it back. */
    static const char *kCalleeSaved[] = {
        "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27",
        "x28", "d8", "d9", "d10", "d11", "d12", "d13", "d14", "d15",
    };
    int n_int     = (int)(sizeof(kIntRegs64)   / sizeof(kIntRegs64[0]));
    int n_float   = (int)(sizeof(kFloatRegs)   / sizeof(kFloatRegs[0]));
    int n_scratch = (int)(sizeof(kScratchRegs) / sizeof(kScratchRegs[0]));
    pool.int_arg_regs     = aarch64MakeAoStrVec(kIntRegs64,   n_int);
    pool.float_arg_regs   = aarch64MakeAoStrVec(kFloatRegs,   n_float);
    pool.int_return_reg   = aoStrDupRaw((char *)"x0", 2);
    pool.float_return_reg = aoStrDupRaw((char *)"d0", 2);
    pool.sret_reg         = aoStrDupRaw((char *)"x8", 2);
    pool.scratch_regs     = aarch64MakeAoStrVec(kScratchRegs, n_scratch);
    /* x0-x2 scratch, x8 sret, x9 large-offset pivot, x10 addressing
     * scratch, x16 indirect-call veneer, x18 platform, x29/x30 frame.
     * x3-x7 can still hold incoming arguments. That leaves x11-x15. */
    /* x0-x2 scratch, x8 sret, x9 large-offset pivot, x10 addressing
     * scratch, x11/x12 parameter-copy scratch, x16 indirect-call
     * veneer, x18 platform, x29/x30 frame; x3-x7 may hold incoming
     * arguments. Verified by grepping every register name this backend
     * emits - x11 and x12 were in this list until the arg-overflow and
     * varargs tests found them. */
    static const char *kLeafFree[] = { "x13","x14","x15" };
    pool.leaf_free_regs = aarch64MakeAoStrVec(kLeafFree,
            (int)(sizeof(kLeafFree) / sizeof(kLeafFree[0])));
    pool.callee_saved_regs = aarch64MakeAoStrVec(kCalleeSaved,
            (int)(sizeof(kCalleeSaved) / sizeof(kCalleeSaved[0])));
    pool.op_clobbers      = aarch64OpClobbers;
    pool.variadic_on_stack = 1;

    irRegPoolSet(&pool);
    initialised = 1;
}

/* Map an x-register name (e.g. "x0") to its w-variant ("w0") for a
 * 4-byte access, or back to x for 8-byte. AArch64's 32-bit ops
 * automatically zero the upper 32 bits of the matching x register. */

static const char *a64HomeReg(IrValue *v) {
    if (!v) return NULL;
    if (v->pinned_reg) return v->pinned_reg->data;
    if (v->loc.kind == IR_LOC_REG && v->loc.as.reg) return v->loc.as.reg->data;
    return NULL;
}

/* Local labels in `-S` output. Distinct from astMakeLabel's `.L<n>`
 * (strings and static globals) so the two counters cannot collide. */
#define A64_LOCAL_PREFIX ".LBB"

/* ---------------- per-function selection context ---------------- */

typedef struct A64Ctx {
    HccJit *jit;
    Cctrl *cc;
    IrFunction *fn;
    Ast *ast;
    IrBlock *cur_block;
    IrBlock *next_block;
    int omit_frame;
    u32 aligned_frame;     /* what the prologue subtracted from sp */
    /* Which register holds a struct-return destination buffer: x9
     * unless the value's own home survives the call. */
    int struct_dest;
    int struct_dest_set;

    MirFn *mir;
    int flushed;           /* lines [0, flushed) are already sunk */
    int src_line;          /* stamped onto every emitted line */
    /* The text sink, or NULL when the lines are encoded. */
    AoStr *text;
} A64Ctx;

/* ---------------- operand constructors ----------------
 *
 * Register numbers follow the hardware encoding (A64Reg). 31 is XZR as
 * a data register and SP as an address - the constructors that name an
 * address base pick the SP class so the encoder and the printer agree. */

static AsmOperand rX(int n)    { return mir_x(n); }
static AsmOperand rW(int n)    { return mir_w(n); }
static AsmOperand rD(int n)    { return mir_d(n); }
static AsmOperand rS(int n)    { return mir_s(n); }
static AsmOperand rSP(void)    { return mir_sp(); }
static AsmOperand rBase(int n) { return n == 31 ? mir_sp() : mir_x(n); }
/* The integer register view for a `size`-byte access. */
static AsmOperand rG(int size, int n) { return size >= 8 ? mir_x(n) : mir_w(n); }
/* The FP register view: sN for singles, dN for doubles. */
static AsmOperand rF(int size, int n) { return size == 4 ? mir_s(n) : mir_d(n); }

/* Memory operand `[base, sym]` - the load half of a GOT access. */
static AsmOperand mSym(int base, const char *sym) {
    AsmOperand m = mir_mem(rBase(base), 0);
    m.label_name = (char *)sym;   /* borrowed; deep-copied on emit */
    return m;
}

/* ---------------- emission ----------------
 *
 * Every line carries the source line of the IR instruction being
 * lowered, which the byte sink turns into the pc -> line table. */

static AsmLine *a64Stamp(A64Ctx *ctx, AsmLine *ln) {
    ln->line = ctx->src_line;
    return ln;
}
static AsmLine *E0(A64Ctx *c, int mn) { return a64Stamp(c, mir0(c->mir, mn)); }
static AsmLine *E1(A64Ctx *c, int mn, AsmOperand a) {
    return a64Stamp(c, mir1(c->mir, mn, a));
}
static AsmLine *E2(A64Ctx *c, int mn, AsmOperand a, AsmOperand b) {
    return a64Stamp(c, mir2(c->mir, mn, a, b));
}
static AsmLine *E3(A64Ctx *c, int mn, AsmOperand a, AsmOperand b, AsmOperand d) {
    return a64Stamp(c, mir3(c->mir, mn, a, b, d));
}
static AsmLine *E4(A64Ctx *c, int mn, AsmOperand a, AsmOperand b, AsmOperand d,
                   AsmOperand e) {
    return a64Stamp(c, mir4(c->mir, mn, a, b, d, e));
}
static void EB(A64Ctx *c, int local_num) {
    E1(c, A64MN_B, mir_label_local(local_num));
}
static void EBCC(A64Ctx *c, const char *cc, int local_num) {
    a64Stamp(c, mir_bcond(c->mir, cc, local_num));
}
static void ELABEL(A64Ctx *c, int local_num) {
    mir_local_label(c->mir, local_num);
}

/* ---------------- sinks ---------------- */

/* Sink lines [flushed, n). `peephole` runs the machine-layer cleanup
 * first; it is only safe at the END of a function, because it drops
 * local labels nothing references and a branch emitted after the
 * flush may still be on its way to one. */
static int a64Flush(A64Ctx *ctx, int peephole) {
    MirFn *mf = ctx->mir;
    if (peephole) mir_fn_peephole_arm64(mf, ctx->flushed);
    AsmBlock *b = mf->block;
    AsmEnc *enc = &ctx->jit->enc;
    for (int i = ctx->flushed; i < b->n_lines; ++i) {
        AsmLine *ln = &b->lines[i];
        if (ctx->text) {
            char buf[512];
            asm_line_format_gas(ln, A64_LOCAL_PREFIX, buf, sizeof(buf));
            if (ln->kind == AINS_INSTR) aoStrCatFmt(ctx->text, "\t%s\n", buf);
            else                        aoStrCatFmt(ctx->text, "%s\n", buf);
            continue;
        }
        switch (ln->kind) {
            case AINS_LABEL_LOCAL:
                asm_define_label(enc, ln->local_num, NULL);
                break;
            case AINS_LABEL_PUBLIC:
                asm_define_label(enc, -1, ln->label_name);
                break;
            case AINS_INSTR:
                if (ln->line) hccJitNoteLine(ctx->jit, ln->line);
                aarch64_encode_instr(enc, ln);
                break;
            default:
                break;
        }
    }
    ctx->flushed = b->n_lines;
    return ctx->text ? 0 : enc->errors;
}

/* ---------------- small helpers ---------------- */

static int a64IsAddImm(s64 v) {
    if (v < 0) return 0;
    if (v <= 0xFFF) return 1;
    if ((v & 0xFFF) == 0 && (v >> 12) <= 0xFFF) return 1;
    return 0;
}

/* Can `[base, #disp]` encode for a `size`-byte access? Unscaled
 * (ldur/stur) covers signed -256..255; the scaled unsigned form
 * covers size-aligned 0..4095*size. The encoder picks between the
 * two by itself; this only asks whether either fits. */
static int a64IsMemDisp(s64 disp, int size) {
    if (disp >= -256 && disp <= 255) return 1;
    if (disp >= 0 && (disp % size) == 0 && disp <= 4095 * (s64)size) return 1;
    return 0;
}

static int a64RegFromName(const char *name) {
    if (!name || !*name) return 0;
    if (name[0] == 'x' || name[0] == 'X' || name[0] == 'w' || name[0] == 'W' ||
        name[0] == 'd' || name[0] == 'D' || name[0] == 's' || name[0] == 'S')
    {
        int n = atoi(name + 1);
        if (n < 0 || n > 31) return 0;
        return n;
    }
    if (!strcasecmp(name, "sp"))  return 31;
    if (!strcasecmp(name, "lr"))  return 30;
    if (!strcasecmp(name, "fp"))  return 29;
    if (!strcasecmp(name, "xzr") || !strcasecmp(name, "wzr")) return 31;
    return 0;
}

static int a64IsFpRegName(const char *name) {
    return name && (name[0] == 'd' || name[0] == 's' ||
                    name[0] == 'D' || name[0] == 'S');
}

/* sN -> 4, dN -> 8. A `reg`-pinned name is the user's spelling. */
static int a64FpRegWidth(const char *name) {
    return (name[0] == 's' || name[0] == 'S') ? 4 : 8;
}

/* The register a value lives in, or NULL when it lives in memory. */
static AoStr *a64Home(IrValue *v) {
    if (!v) return NULL;
    if (v->pinned_reg) return v->pinned_reg;
    if (v->loc.kind == IR_LOC_REG && v->loc.as.reg) return v->loc.as.reg;
    return NULL;
}

/* reg = imm. Small values are a single mov/movn; anything else is a
 * movz/movk sequence, spelled out so the text sink assembles too. */
static void a64MovImm(A64Ctx *ctx, int reg, s64 imm) {
    u64 u = (u64)imm;
    if (u <= 0xFFFFULL) {
        E2(ctx, A64MN_MOV, rX(reg), mir_imm(imm));
        return;
    }
    if (imm < 0 && imm >= -65536) {
        E2(ctx, A64MN_MOVN, rX(reg), mir_imm((s64)((u64)(~imm) & 0xFFFFULL)));
        return;
    }
    u64 w0 = u & 0xFFFFULL, w1 = (u >> 16) & 0xFFFFULL,
        w2 = (u >> 32) & 0xFFFFULL, w3 = (u >> 48) & 0xFFFFULL;
    int emitted = 0;
    if (w0 || (!w1 && !w2 && !w3)) {
        E2(ctx, A64MN_MOVZ, rX(reg), mir_imm((s64)w0));
        emitted = 1;
    }
    u64 words[3] = { w1, w2, w3 };
    for (int i = 0; i < 3; ++i) {
        if (!words[i]) continue;
        E3(ctx, emitted ? A64MN_MOVK : A64MN_MOVZ, rX(reg),
           mir_imm((s64)words[i]), mir_shift(ASHIFT_LSL, 16 * (i + 1)));
        emitted = 1;
    }
}

/* sp -=/+= n for any n up to 16MB, via the imm12 (+ lsl #12) split.
 * The register form is a trap here: reg 31 in the shifted-register
 * ADD/SUB is XZR, not SP. */
static void a64SpAdjust(A64Ctx *ctx, int is_sub, u64 n) {
    if (n == 0) return;
    if ((n >> 12) > 0xFFF) {
        loggerPanic("aarch64: stack adjustment %llu exceeds 16MB\n",
                    (unsigned long long)n);
    }
    u64 hi = n & ~0xFFFULL, lo = n & 0xFFF;
    int mn = is_sub ? A64MN_SUB : A64MN_ADD;
    if (hi) E3(ctx, mn, rSP(), rSP(), mir_imm((s64)hi));
    if (lo) E3(ctx, mn, rSP(), rSP(), mir_imm((s64)lo));
}

/* reg += disp via add/sub imm12, or x9 and the register form. */
static void a64AddSubImm(A64Ctx *ctx, int reg, s64 disp) {
    if (disp == 0) return;
    int is_sub = disp < 0;
    u64 mag = (u64)(is_sub ? -disp : disp);
    int mn = is_sub ? A64MN_SUB : A64MN_ADD;
    if (mag <= 0xFFF) {
        E3(ctx, mn, rBase(reg), rBase(reg), mir_imm((s64)mag));
        return;
    }
    a64MovImm(ctx, 9, disp);
    E3(ctx, A64MN_ADD, rBase(reg), rBase(reg), rX(9));
}

/* ---------------- frame access ---------------- */

/* `[x29, #loff]` when an immediate form fits, else the offset goes
 * through x9 and the register-offset form is used. */
static AsmOperand a64FrameMem(A64Ctx *ctx, int loff, int size) {
    if (a64IsMemDisp(loff, size)) return mir_mem(rX(29), loff);
    a64MovImm(ctx, 9, (s64)loff);
    return mir_mem_idx(rX(29), rX(9), 0);
}

/* Integer frame load with size-aware extension: sub-word loads zero
 * extend (as a `w` write does), a 4-byte load sign-extends, matching
 * x86's movslq. */
static void a64FrameLoad(A64Ctx *ctx, int reg, int size, int loff) {
    AsmOperand m = a64FrameMem(ctx, loff, size);
    switch (size) {
        case 1:  E2(ctx, A64MN_LDRB,  rW(reg), m); break;
        case 2:  E2(ctx, A64MN_LDRH,  rW(reg), m); break;
        case 4:  E2(ctx, A64MN_LDRSW, rX(reg), m); break;
        default: E2(ctx, A64MN_LDR,   rX(reg), m); break;
    }
}

/* Sign-extending frame load: `ldrsb x0, [x29, #-8]` in one. */
static void a64FrameLoadSx(A64Ctx *ctx, int reg, int size, int loff) {
    AsmOperand m = a64FrameMem(ctx, loff, size);
    switch (size) {
        case 1:  E2(ctx, A64MN_LDRSB, rX(reg), m); break;
        case 2:  E2(ctx, A64MN_LDRSH, rX(reg), m); break;
        default: E2(ctx, A64MN_LDRSW, rX(reg), m); break;
    }
}

static void a64FrameStore(A64Ctx *ctx, int reg, int size, int loff) {
    AsmOperand m = a64FrameMem(ctx, loff, size);
    switch (size) {
        case 1:  E2(ctx, A64MN_STRB, rW(reg), m); break;
        case 2:  E2(ctx, A64MN_STRH, rW(reg), m); break;
        case 4:  E2(ctx, A64MN_STR,  rW(reg), m); break;
        default: E2(ctx, A64MN_STR,  rX(reg), m); break;
    }
}

static void a64FpFrameLoad(A64Ctx *ctx, int dreg, int size, int loff) {
    E2(ctx, A64MN_LDR, rF(size, dreg), a64FrameMem(ctx, loff, size));
}

static void a64FpFrameStore(A64Ctx *ctx, int dreg, int size, int loff) {
    E2(ctx, A64MN_STR, rF(size, dreg), a64FrameMem(ctx, loff, size));
}

/* ---------------- deref access ----------------
 *
 * The register-offset encoding has a single S bit meaning "scale by
 * the ACCESS width", so it can only express scale == size. `Point
 * pts[3]` indexed by a variable needs base + i*8 for a 4-byte field;
 * where they disagree, shift the index by hand into x10 and use the
 * unshifted form. Returns the index register and writes the lsl. */
static int a64IdxScaled(A64Ctx *ctx, int idx, int scale, int size, int *lsl) {
    int sh = (scale == 8) ? 3 : (scale == 4) ? 2 : (scale == 2) ? 1 : 0;
    if (sh != 0 && scale != size) {
        E3(ctx, A64MN_LSL, rX(10), rX(idx), mir_imm(sh));
        *lsl = 0;
        return 10;
    }
    *lsl = sh;
    return idx;
}

/* Address operand for `[base + idx*scale + disp]` on a `size`-byte
 * access. A displacement alongside an index folds into the base (no
 * `[base, idx, #disp]` form exists); one out of every immediate range
 * goes through x10 - base may be a live home that must not move, and
 * x9 can be holding a store value. */
static AsmOperand a64DerefMem(A64Ctx *ctx, int size, int base, int has_idx,
                              int idx, int scale, s32 disp)
{
    if (has_idx) {
        if (disp) a64AddSubImm(ctx, base, (s64)disp);
        int lsl;
        idx = a64IdxScaled(ctx, idx, scale, size, &lsl);
        return mir_mem_idx(rBase(base), rX(idx), lsl);
    }
    if (disp != 0 && !a64IsMemDisp(disp, size)) {
        a64MovImm(ctx, 10, (s64)disp);
        return mir_mem_idx(rBase(base), rX(10), 0);
    }
    return mir_mem(rBase(base), disp);
}

static void a64DerefLoad(A64Ctx *ctx, int size, int dst, int base, int has_idx,
                         int idx, int scale, s32 disp)
{
    AsmOperand m = a64DerefMem(ctx, size, base, has_idx, idx, scale, disp);
    switch (size) {
        case 1:  E2(ctx, A64MN_LDRB,  rW(dst), m); break;
        case 2:  E2(ctx, A64MN_LDRH,  rW(dst), m); break;
        case 4:  E2(ctx, A64MN_LDRSW, rX(dst), m); break;
        default: E2(ctx, A64MN_LDR,   rX(dst), m); break;
    }
}

static void a64DerefStore(A64Ctx *ctx, int size, int val, int base, int has_idx,
                          int idx, int scale, s32 disp)
{
    AsmOperand m = a64DerefMem(ctx, size, base, has_idx, idx, scale, disp);
    switch (size) {
        case 1:  E2(ctx, A64MN_STRB, rW(val), m); break;
        case 2:  E2(ctx, A64MN_STRH, rW(val), m); break;
        case 4:  E2(ctx, A64MN_STR,  rW(val), m); break;
        default: E2(ctx, A64MN_STR,  rX(val), m); break;
    }
}

static void a64FpDerefAccess(A64Ctx *ctx, int is_load, int size, int vreg,
                             int base, int has_idx, int idx, int scale, s32 disp)
{
    AsmOperand m = a64DerefMem(ctx, size, base, has_idx, idx, scale, disp);
    E2(ctx, is_load ? A64MN_LDR : A64MN_STR, rF(size, vreg), m);
}

/* ---------------- global addressing ----------------
 *
 * RELOCATED (an object file, or text for the assembler): PC-relative
 * with a fixup. Darwin spells the pair `adrp x, sym@PAGE` / `add x, x,
 * sym@PAGEOFF`; ELF `adrp x, sym` / `add x, x, :lo12:sym`. A symbol
 * from another image cannot be reached that way - the static linker
 * has no address to fold in - so it is loaded from the GOT instead.
 *
 * ABSOLUTE (the JIT): a function in this chunk has no address yet, so
 * it takes the same adrp/add pair and asm_jit_finalize patches it
 * against the label. Everything else - this chunk's globals in their
 * host arena, host functions through dlsym - is materialised inline.
 * ADRP's +/-4GB reach does not cover libc on macOS. */
static void a64GlobalAddrEx(A64Ctx *ctx, int reg, const char *sym, int force_got) {
    HccJit *jit = ctx->jit;
    Cctrl *cc = ctx->cc;
    int darwin = cc->target == TARGET_AARCH64_APPLE_DARWIN;
    char page[512], off[512];

    if (jit->addr_mode == HCC_ADDR_RELOCATED) {
        int is_extern = force_got ||
                        (cc->extern_globals &&
                         setHas(cc->extern_globals, (char *)sym)) ||
                        asmSymbolIsPreemptible(cc, sym);
        if (is_extern) {
            snprintf(page, sizeof(page), darwin ? "%s@GOTPAGE" : ":got:%s", sym);
            snprintf(off, sizeof(off), darwin ? "%s@GOTPAGEOFF" : ":got_lo12:%s", sym);
            E2(ctx, A64MN_ADRP, rX(reg), mir_sym(page));
            E2(ctx, A64MN_LDR, rX(reg), mSym(reg, off));
        } else {
            snprintf(page, sizeof(page), darwin ? "%s@PAGE" : "%s", sym);
            snprintf(off, sizeof(off), darwin ? "%s@PAGEOFF" : ":lo12:%s", sym);
            E2(ctx, A64MN_ADRP, rX(reg), mir_sym(page));
            E3(ctx, A64MN_ADD, rX(reg), rX(reg), mir_sym(off));
        }
        return;
    }

    if (hccJitIsInternalFunc(jit, sym)) {
        snprintf(page, sizeof(page), "%s@PAGE", sym);
        snprintf(off, sizeof(off), "%s@PAGEOFF", sym);
        E2(ctx, A64MN_ADRP, rX(reg), mir_sym(page));
        E3(ctx, A64MN_ADD, rX(reg), rX(reg), mir_sym(off));
        return;
    }
    void *addr = mapGet(jit->host_symbols, (void *)sym);
    if (!addr) addr = asm_jit_dlsym_resolver(NULL, sym);
    a64MovImm(ctx, reg, (s64)(uintptr_t)addr);
}

static void a64GlobalAddr(A64Ctx *ctx, int reg, const char *sym) {
    a64GlobalAddrEx(ctx, reg, sym, 0);
}

/* The symbol a global is addressed by. An object or the assembler
 * wants the platform-normalised spelling (`_g` on Mach-O); the JIT
 * keys its host arena by the raw name. */
static const char *a64GlobalName(A64Ctx *ctx, AoStr *name) {
    if (ctx->jit->addr_mode == HCC_ADDR_RELOCATED) {
        return asmNormaliseGlobalLabel(ctx->cc, name)->data;
    }
    return name->data;
}

/* ---------------- load/store IR values ---------------- */

static int a64FpDbl(IrValue *v) { return (int)irValueByteSize(v) == 8; }

static void a64LoadToReg(A64Ctx *ctx, IrValue *val, int reg) {
    switch (val->kind) {
        case IR_VAL_CONST_INT:
            if (val->as._i64 == 0) E2(ctx, A64MN_MOV, rX(reg), rX(31));
            else                   a64MovImm(ctx, reg, val->as._i64);
            break;
        case IR_VAL_CONST_FLOAT:
            /* Rare bit-cast: materialise the bit pattern. */
            a64MovImm(ctx, reg, (s64)(u64)ieee754_64(val->as._f64));
            break;
        case IR_VAL_CONST_STR:
            a64GlobalAddr(ctx, reg, val->as.str.label->data);
            break;
        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            AoStr *home = a64Home(val);
            if (home) {
                int src = a64RegFromName(home->data);
                if (src != reg) E2(ctx, A64MN_MOV, rX(reg), rX(src));
                break;
            }
            a64FrameLoad(ctx, reg, (int)irValueByteSize(val),
                         irCgGetLoff(&ctx->fn->ra, val));
            break;
        }
        case IR_VAL_GLOBAL:
            a64GlobalAddr(ctx, reg, a64GlobalName(ctx, val->as.global.name));
            break;
        default:
            loggerPanic("aarch64: cannot load int value of kind %d\n", val->kind);
    }
}

/* Move between FP registers. `fmov` needs both the same precision;
 * crossing widths is a conversion, and the instruction for that is
 * `fcvt`. A `reg`-pinned float has whatever width the user wrote. */
static void a64FpMove(A64Ctx *ctx, int dst_size, int dst, int src_size, int src) {
    /* The same register at two widths is the same value: sN is the low
     * half of dN, and a "conversion" onto itself would overwrite the
     * single with the double's bit pattern. */
    if (dst == src) return;
    E2(ctx, dst_size == src_size ? A64MN_FMOV : A64MN_FCVT,
       rF(dst_size, dst), rF(src_size, src));
}

static void a64LoadToFpr(A64Ctx *ctx, IrValue *val, int dreg) {
    switch (val->kind) {
        case IR_VAL_CONST_FLOAT:
        case IR_VAL_CONST_INT: {
            double f = val->kind == IR_VAL_CONST_INT
                       ? (double)val->as._i64 : val->as._f64;
            int is_dbl = (val->kind == IR_VAL_CONST_FLOAT) ? a64FpDbl(val) : 1;
            int size = is_dbl ? 8 : 4;
            u64 bits = is_dbl ? (u64)ieee754_64(f) : (u64)(u32)ieee754_32((f32)f);
            if (bits == 0) {
                E2(ctx, A64MN_FMOV, rF(size, dreg), rG(size, 31));
                break;
            }
            /* Materialise via x9 = bit pattern, then fmov to dreg. */
            a64MovImm(ctx, 9, (s64)bits);
            E2(ctx, A64MN_FMOV, rF(size, dreg), rG(size, 9));
            break;
        }
        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            int size = (int)irValueByteSize(val);
            AoStr *home = a64Home(val);
            if (home) {
                int src = a64RegFromName(home->data);
                if (a64IsFpRegName(home->data)) {
                    a64FpMove(ctx, size, dreg, a64FpRegWidth(home->data), src);
                } else {
                    /* A GPR holding the bits (a pinned int register). */
                    E2(ctx, A64MN_FMOV, rF(size, dreg), rG(size, src));
                }
                break;
            }
            a64FpFrameLoad(ctx, dreg, size, irCgGetLoff(&ctx->fn->ra, val));
            break;
        }
        default:
            loggerPanic("aarch64: cannot load fp value of kind %d\n", val->kind);
    }
}

static void a64StoreReg(A64Ctx *ctx, IrValue *dst, int reg) {
    AoStr *home = a64Home(dst);
    if (home) {
        int h = a64RegFromName(home->data);
        if (h != reg) E2(ctx, A64MN_MOV, rX(h), rX(reg));
        return;
    }
    a64FrameStore(ctx, reg, (int)irValueByteSize(dst),
                  irCgGetLoff(&ctx->fn->ra, dst));
}

/* `val_size` (0 = derive from dst) is the width of the value being
 * stored. When dst is a pointer/gep tmp aliasing a narrow field, sizing
 * the store from dst (8 bytes) would clobber the neighbouring field. */
static void a64StoreFprSized(A64Ctx *ctx, IrValue *dst, int dreg, int val_size) {
    int size = val_size > 0 ? val_size : (int)irValueByteSize(dst);
    AoStr *home = a64Home(dst);
    if (home) {
        int h = a64RegFromName(home->data);
        if (a64IsFpRegName(home->data)) {
            a64FpMove(ctx, a64FpRegWidth(home->data), h, size, dreg);
        } else {
            E2(ctx, A64MN_FMOV, rG(size, h), rF(size, dreg));
        }
        return;
    }
    a64FpFrameStore(ctx, dreg, size, irCgGetLoff(&ctx->fn->ra, dst));
}

static void a64StoreFpr(A64Ctx *ctx, IrValue *dst, int dreg) {
    a64StoreFprSized(ctx, dst, dreg, 0);
}

/* Skip a home store for a result nothing reads. Register homes only -
 * see IRCG_DST_DEAD. */
static int a64DstStoreIsDead(IrInstr *instr, IrValue *dst) {
    if (!instr || !(instr->flags & IRCG_DST_DEAD)) return 0;
    if (instr->dst != dst) return 0;
    return a64Home(dst) != NULL;
}

static void a64SpillDst(A64Ctx *ctx, IrInstr *instr, int reg) {
    if (a64DstStoreIsDead(instr, instr->dst)) return;
    a64StoreReg(ctx, instr->dst, reg);
}

static void a64SpillDstFpr(A64Ctx *ctx, IrInstr *instr, int dreg) {
    if (a64DstStoreIsDead(instr, instr->dst)) return;
    a64StoreFpr(ctx, instr->dst, dreg);
}

/* r1's live register, or x0/d0 after loading it there. */
static int a64SrcReg(A64Ctx *ctx, IrInstr *i, int is_fpr) {
    if (i->r1->loc.kind == IR_LOC_REG && i->r1->loc.as.reg)
        return a64RegFromName(i->r1->loc.as.reg->data);
    if (is_fpr) a64LoadToFpr(ctx, i->r1, 0);
    else        a64LoadToReg(ctx, i->r1, 0);
    return 0;
}

/* ---------------- addressing-mode fusion ----------------
 *
 * A base scratch that avoids the index, and no addressing off a home
 * the emitter would mutate. */
static int a64ScratchTake(int avoid_valid, int avoid) {
    if (!avoid_valid || avoid != 1) return 1;
    return 2;
}

/* Where the index already lives, or 0 if it must be materialised. */
static int a64IdxHome(IrInstr *instr, int *out) {
    if (!instr->idx || !instr->scale) return 0;
    AoStr *h = a64Home(instr->idx);
    if (!h) return 0;
    *out = a64RegFromName(h->data);
    return 1;
}

/* May we address straight off this value's home? Only when nothing
 * writes to it: the displacement fold adds into the base register in
 * place, which destroys a promoted local. Indexed AND displaced is the
 * mutating shape. */
static int a64BaseHome(IrValue *v, IrInstr *instr, int *out) {
    AoStr *h = a64Home(v);
    if (!h) return 0;
    if (instr->idx && instr->scale && instr->disp != 0) return 0;
    *out = a64RegFromName(h->data);
    return 1;
}

/* Index register for addressing-mode fusion. Returns 1 if there is an
 * index component; `*out` is always written. */
static int a64IdxReg(A64Ctx *ctx, IrInstr *instr, int base, int *out) {
    *out = 2;
    if (!instr->idx || !instr->scale) return 0;
    if (a64IdxHome(instr, out)) return 1;
    int scratch = (base == 2) ? 1 : 2;
    a64LoadToReg(ctx, instr->idx, scratch);
    *out = scratch;
    return 1;
}

/* ---------------- condition codes ---------------- */

static const char *a64CcFor(IrCmpKind cmp, int is_float) {
    if (is_float) {
        switch (cmp) {
            case IR_CMP_EQ: return "eq";
            case IR_CMP_NE: return "ne";
            case IR_CMP_LT: return "mi";
            case IR_CMP_LE: return "ls";
            case IR_CMP_GT: return "gt";
            case IR_CMP_GE: return "ge";
            default: loggerPanic("aarch64: bad float cmp %d\n", cmp);
        }
    }
    switch (cmp) {
        case IR_CMP_EQ:  return "eq";
        case IR_CMP_NE:  return "ne";
        case IR_CMP_LT:  return "lt";
        case IR_CMP_LE:  return "le";
        case IR_CMP_GT:  return "gt";
        case IR_CMP_GE:  return "ge";
        case IR_CMP_ULT: return "lo";
        case IR_CMP_ULE: return "ls";
        case IR_CMP_UGT: return "hi";
        case IR_CMP_UGE: return "hs";
        default: loggerPanic("aarch64: bad int cmp %d\n", cmp);
    }
}

static const char *a64CcInvFor(IrCmpKind cmp, int is_float) {
    if (is_float) {
        switch (cmp) {
            case IR_CMP_EQ: return "ne";
            case IR_CMP_NE: return "eq";
            case IR_CMP_LT: return "pl";
            case IR_CMP_LE: return "hi";
            case IR_CMP_GT: return "le";
            case IR_CMP_GE: return "lt";
            default: loggerPanic("aarch64: bad float cmp %d\n", cmp);
        }
    }
    switch (cmp) {
        case IR_CMP_EQ:  return "ne";
        case IR_CMP_NE:  return "eq";
        case IR_CMP_LT:  return "ge";
        case IR_CMP_LE:  return "gt";
        case IR_CMP_GT:  return "le";
        case IR_CMP_GE:  return "lt";
        case IR_CMP_ULT: return "hs";
        case IR_CMP_ULE: return "hi";
        case IR_CMP_UGT: return "ls";
        case IR_CMP_UGE: return "lo";
        default: loggerPanic("aarch64: bad int cmp %d\n", cmp);
    }
}

static void a64EmitCSet(A64Ctx *ctx, IrCmpKind cmp, int is_float) {
    E2(ctx, A64MN_CSET, rX(0), mir_sym(a64CcFor(cmp, is_float)));
}

/* ---------------- phi materialisation ---------------- */

static void a64EmitOnePhi(void *ud, IrInstr *phi, IrPair *match) {
    A64Ctx *ctx = (A64Ctx *)ud;
    IrValue *v = match ? match->ir_value : NULL;
    int v_dangling = irIsTmp(v) && v->loc.kind != IR_LOC_REG &&
                     !mapHasInt(ctx->fn->ra.id_to_loff, irVarId(v));
    if (irIsFloat(phi->dst->type)) {
        if (v_dangling) {
            int size = a64FpDbl(phi->dst) ? 8 : 4;
            E2(ctx, A64MN_FMOV, rF(size, 0), rG(size, 31));
        } else {
            a64LoadToFpr(ctx, v, 0);
        }
        a64StoreFpr(ctx, phi->dst, 0);
        return;
    }
    if (v_dangling) E2(ctx, A64MN_MOV, rX(0), rX(31));
    else            a64LoadToReg(ctx, v, 0);
    a64StoreReg(ctx, phi->dst, 0);
}

static void a64PhiMaterialise(A64Ctx *ctx, IrBlock *from, IrBlock *to) {
    hccJitPhiMaterialise(from, to, a64EmitOnePhi, ctx);
}

/* ---------------- calls: the simple (scalar) path ---------------- */

static s32 a64PartitionCallArgs(u8 *is_stack, u64 n, Vec *args, int is_variadic,
                                int named_count, int force_va_stack)
{
    int int_used = 0, float_used = 0, stack_count = 0;
    for (u64 i = 0; i < n; ++i) {
        IrValue *a = vecGet(IrValue *, args, i);
        int is_float = irIsFloat(a->type);
        int force_stack = is_variadic && force_va_stack && (int)i >= named_count;
        if (force_stack) { is_stack[i] = 1; stack_count++; }
        else if (is_float) {
            if (float_used < 8) { is_stack[i] = 0; float_used++; }
            else { is_stack[i] = 1; stack_count++; }
        } else {
            if (int_used < 8) { is_stack[i] = 0; int_used++; }
            else { is_stack[i] = 1; stack_count++; }
        }
    }
    return (s32)stack_count;
}

static void a64Call(A64Ctx *ctx, AoStr *fname, int indirect) {
    if (indirect) {
        E1(ctx, A64MN_BLR, rX(16));
    } else {
        E1(ctx, A64MN_BL, mir_sym(asmNormaliseFunctionName(ctx->cc, fname)));
    }
}

/* ---------------- struct copies ---------------- */

static int a64IsByvalStruct(AstType *t) {
    return t && (t->kind == AST_TYPE_CLASS || t->kind == AST_TYPE_UNION) &&
           !t->is_intrinsic;
}

/* Load `size` (1/2/4/8) bytes at [base, #off] into x-reg `reg`. */
static void a64ChunkLoad(A64Ctx *ctx, int reg, int base, int off, int size) {
    AsmOperand m = mir_mem(rBase(base), off);
    switch (size) {
        case 8:  E2(ctx, A64MN_LDR,  rX(reg), m); break;
        case 4:  E2(ctx, A64MN_LDR,  rW(reg), m); break;
        case 2:  E2(ctx, A64MN_LDRH, rW(reg), m); break;
        case 1:  E2(ctx, A64MN_LDRB, rW(reg), m); break;
        default: loggerPanic("aarch64: %d-byte struct chunk not supported\n", size);
    }
}

static void a64ChunkStore(A64Ctx *ctx, int reg, int base, int off, int size) {
    AsmOperand m = mir_mem(rBase(base), off);
    switch (size) {
        case 8:  E2(ctx, A64MN_STR,  rX(reg), m); break;
        case 4:  E2(ctx, A64MN_STR,  rW(reg), m); break;
        case 2:  E2(ctx, A64MN_STRH, rW(reg), m); break;
        case 1:  E2(ctx, A64MN_STRB, rW(reg), m); break;
        default: loggerPanic("aarch64: %d-byte struct chunk not supported\n", size);
    }
}

static int a64ChunkTail(int rem) {
    if (rem >= 8) return 8;
    if (rem == 4 || rem == 2 || rem == 1) return rem;
    loggerPanic("aarch64: %d-byte copy chunk not supported\n", rem);
}

/* Copy `size` bytes from [base, #src] to the frame slot at #dst via x10. */
static void a64CopyToSlot(A64Ctx *ctx, int base, int size, int src, int dst) {
    int o = 0;
    while (o < size) {
        int w = a64ChunkTail(size - o);
        a64ChunkLoad(ctx, 10, base, src + o, w);
        a64FrameStore(ctx, 10, w, dst + o);
        o += w;
    }
}

/* Copy `size` bytes from [src, #0..] to [sp, #dst] via x10. */
static void a64CopyToSp(A64Ctx *ctx, int src, int size, int dst) {
    int o = 0;
    while (o < size) {
        int w = a64ChunkTail(size - o);
        a64ChunkLoad(ctx, 10, src, o, w);
        a64ChunkStore(ctx, 10, 31, dst + o, w);
        o += w;
    }
}

/* ---------------- AAPCS64 call with aggregates ----------------
 *
 * All of the ABI accounting lives here: the pre-pass that sizes the
 * two below-sp regions (the stack-argument area for register overflow
 * + the INDIRECT copy area for >16-byte aggregates), the per-argument
 * classification, and the NGRN/NSRN/stack-cursor bookkeeping. The
 * struct address is parked in x9; x10 is the copy scratch. */

static int a64DestReg(A64Ctx *ctx) {
    return ctx->struct_dest_set ? ctx->struct_dest : 9;
}

/* Does `v` still hold the destination address AFTER a call, so the
 * address can be recomputed instead of spilled? True for a stack slot
 * and for a callee-saved register; false for a caller-saved one. */
static int a64DestSurvivesCall(IrValue *v) {
    AoStr *home = a64Home(v);
    if (!home) return 1;
    IrRegPool *pool = irRegPoolGet();
    if (!pool || !pool->callee_saved_regs) return 0;
    for (u64 i = 0; i < pool->callee_saved_regs->size; ++i) {
        AoStr *cs = (AoStr *)pool->callee_saved_regs->entries[i];
        if (!strcmp(cs->data, home->data)) return 1;
    }
    return 0;
}

/* Point the destination-buffer register at `v`'s own home when it has
 * one that survives the call: the address then needs no materialising
 * into x9 at all. */
static int a64UseDestHome(A64Ctx *ctx, IrValue *v) {
    ctx->struct_dest_set = 0;
    AoStr *home = a64Home(v);
    if (!home || !a64DestSurvivesCall(v)) return 0;
    ctx->struct_dest = a64RegFromName(home->data);
    ctx->struct_dest_set = 1;
    return 1;
}

static void a64AggLoad(A64Ctx *ctx, int is_fp, int reg, int src_off, int size) {
    if (is_fp) E2(ctx, A64MN_LDR, rF(size, reg), mir_mem(rX(9), src_off));
    else       a64ChunkLoad(ctx, reg, 9, src_off, size);
}

/* Store a result-register chunk of a register-returned struct into the
 * destination buffer. */
static void a64RetChunkStore(A64Ctx *ctx, int is_fp, int reg, int off, int size) {
    int d = a64DestReg(ctx);
    if (is_fp) E2(ctx, A64MN_STR, rF(size, reg), mir_mem(rBase(d), off));
    else       a64ChunkStore(ctx, reg, d, off, size);
}

static void a64ScalarToReg(A64Ctx *ctx, IrValue *a, int is_fp, int reg) {
    if (is_fp) a64LoadToFpr(ctx, a, reg);
    else       a64LoadToReg(ctx, a, reg);
}

static void a64ScalarToStack(A64Ctx *ctx, IrValue *a, int is_fp, int nsaa) {
    if (is_fp) {
        int size = (int)irValueByteSize(a);
        a64LoadToFpr(ctx, a, 0);
        E2(ctx, A64MN_STR, rF(size, 0), mir_mem(rSP(), nsaa));
    } else {
        a64LoadToReg(ctx, a, 9);
        E2(ctx, A64MN_STR, rX(9), mir_mem(rSP(), nsaa));
    }
}

static void a64EmitAapcsCall(A64Ctx *ctx, IrInstr *instr, Vec *args,
                             AoStr *fname, int indirect)
{
    u64 n = args ? args->size : 0;
    int ngrn = 0, nsrn = 0;

    /* args[0] of an aggregate-returning call is the destination buffer,
     * and it is never one of the x0-x7 arguments: a <=16-byte return
     * comes back in registers (copied into the buffer after the call),
     * a >16-byte (INDIRECT) return passes the buffer address in x8. */
    AstType *ret_st = (instr->flags & IRCG_CALL_AGG_RETURN) && instr->dst
                      ? instr->dst->byval_struct_type : NULL;
    /* The result IS this function's return value and the `ret` is next:
     * the bytes are already in the registers the epilogue wants. */
    int fwd_ret = (instr->flags & IRCG_AGG_RET_FORWARD) != 0;
    int reg_ret = ret_st && ret_st->size <= 16 && !fwd_ret;
    int ind_ret = ret_st && ret_st->size > 16;
    u64 a0 = ret_st ? 1 : 0;

    /* Pre-pass: size the stack-arg area and the INDIRECT copy area. */
    int nsaa_total = 0, indirect_bytes = 0;
    {
        int g = 0, s = 0;
        for (u64 i = a0; i < n; ++i) {
            IrValue *a = vecGet(IrValue *, args, i);
            if (a->byval_struct_type) {
                AstType *t = a->byval_struct_type;
                int el = 0, c = 0;
                AapcsClass cl = astAapcsClassify(t, &el, &c);
                if (cl == AAPCS_INDIRECT) {
                    indirect_bytes += (t->size + 15) & ~15;
                    if (g < 8) g++; else nsaa_total += 8;
                } else if (cl == AAPCS_HFA) {
                    if (s + c <= 8) s += c;
                    else { nsaa_total += (t->size + 7) & ~7; s = 8; }
                } else {
                    int ngp = (t->size + 7) / 8;
                    if (g + ngp <= 8) g += ngp;
                    else { nsaa_total += (t->size + 7) & ~7; g = 8; }
                }
            } else if (irIsFloat(a->type)) {
                if (s < 8) s++; else nsaa_total += 8;
            } else {
                if (g < 8) g++; else nsaa_total += 8;
            }
        }
    }
    /* The destination pointer is computed into x9, which is
     * caller-saved, so it normally has to be spilled across the call.
     * When the VALUE it came from survives the call by itself - a slot,
     * or a callee-saved register - recompute the address afterwards
     * instead. */
    int need_dest = ret_st && !fwd_ret;
    int dest_resident = need_dest &&
                        a64DestSurvivesCall(vecGet(IrValue *, args, 0));
    /* Stronger still: the address is ALREADY in a surviving register. */
    int dest_homed = need_dest &&
                     a64UseDestHome(ctx, vecGet(IrValue *, args, 0));
    int dest_save_off = nsaa_total + indirect_bytes;
    int total = (nsaa_total + indirect_bytes +
                 ((need_dest && !dest_resident) ? 16 : 0) + 15) & ~15;
    if (total > 0) E3(ctx, A64MN_SUB, rSP(), rSP(), mir_imm(total));
    int nsaa = 0, copy_off = 0, copy_base = nsaa_total;

    if (need_dest && !dest_resident && !dest_homed) {
        /* Compute the destination buffer address into x9 (before the
         * arg loop clobbers x9) and stash it across the call. */
        a64LoadToReg(ctx, vecGet(IrValue *, args, 0), 9);
        E2(ctx, A64MN_STR, rX(9), mir_mem(rSP(), dest_save_off));
    }
    /* AAPCS64: an INDIRECT return buffer's address goes in x8. Load it
     * BEFORE the arg loop: args[0] may live in an arg register (a
     * forwarded LEA) the loop is about to overwrite. */
    if (ind_ret) a64ScalarToReg(ctx, vecGet(IrValue *, args, 0), 0, 8);

    for (u64 i = a0; i < n; ++i) {
        IrValue *a = vecGet(IrValue *, args, i);
        if (a->byval_struct_type) {
            AstType *t = a->byval_struct_type;
            int elem = 0, count = 0;
            AapcsClass cls = astAapcsClassify(t, &elem, &count);
            a64LoadToReg(ctx, a, 9);   /* struct address -> x9 */
            if (cls == AAPCS_INDIRECT) {
                int dst = copy_base + copy_off;
                a64CopyToSp(ctx, 9, t->size, dst);
                if (ngrn < 8) {
                    E3(ctx, A64MN_ADD, rX(ngrn), rSP(), mir_imm(dst));
                    ngrn++;
                } else {
                    E3(ctx, A64MN_ADD, rX(10), rSP(), mir_imm(dst));
                    E2(ctx, A64MN_STR, rX(10), mir_mem(rSP(), nsaa));
                    nsaa += 8;
                }
                copy_off += (t->size + 15) & ~15;
            } else if (cls == AAPCS_HFA) {
                if (nsrn + count <= 8) {
                    for (int k = 0; k < count; ++k)
                        a64AggLoad(ctx, 1, nsrn + k, k * elem, elem);
                    nsrn += count;
                } else {
                    a64CopyToSp(ctx, 9, t->size, nsaa);
                    nsaa += (t->size + 7) & ~7; nsrn = 8;
                }
            } else { /* AAPCS_INTEGER */
                int ngp = (t->size + 7) / 8;
                if (ngrn + ngp <= 8) {
                    for (int k = 0; k < ngp; ++k) {
                        int off = k * 8, rem = t->size - off;
                        a64AggLoad(ctx, 0, ngrn + k, off, rem >= 8 ? 8 : rem);
                    }
                    ngrn += ngp;
                } else {
                    a64CopyToSp(ctx, 9, t->size, nsaa);
                    nsaa += (t->size + 7) & ~7; ngrn = 8;
                }
            }
        } else if (irIsFloat(a->type)) {
            if (nsrn < 8) { a64ScalarToReg(ctx, a, 1, nsrn); nsrn++; }
            else { a64ScalarToStack(ctx, a, 1, nsaa); nsaa += 8; }
        } else {
            if (ngrn < 8) { a64ScalarToReg(ctx, a, 0, ngrn); ngrn++; }
            else { a64ScalarToStack(ctx, a, 0, nsaa); nsaa += 8; }
        }
    }

    a64Call(ctx, fname, indirect);

    if (reg_ret) {
        /* Reload the destination buffer and copy the result registers
         * into it; do this while sp is still lowered. */
        int re = 0, rc = 0;
        AapcsClass rcls = astAapcsClassify(ret_st, &re, &rc);
        if (dest_homed)         { /* already there */ }
        else if (dest_resident) a64LoadToReg(ctx, vecGet(IrValue *, args, 0), 9);
        else E2(ctx, A64MN_LDR, rX(9), mir_mem(rSP(), dest_save_off));
        if (rcls == AAPCS_HFA) {
            for (int k = 0; k < rc; ++k) a64RetChunkStore(ctx, 1, k, k * re, re);
        } else { /* AAPCS_INTEGER, <=16 bytes -> x0,(x1) */
            int ngp = (ret_st->size + 7) / 8;
            for (int k = 0; k < ngp; ++k) {
                int off = k * 8, rem = ret_st->size - off;
                a64RetChunkStore(ctx, 0, k, off, rem >= 8 ? 8 : rem);
            }
        }
    } else if (ind_ret) {
        /* The call's dst is the buffer address, but unlike SysV the
         * callee does NOT return it in x0: get it back while sp is
         * still lowered. */
        if (dest_homed)         { /* already there */ }
        else if (dest_resident) a64LoadToReg(ctx, vecGet(IrValue *, args, 0), 9);
        else E2(ctx, A64MN_LDR, rX(9), mir_mem(rSP(), dest_save_off));
    }

    if (total > 0) E3(ctx, A64MN_ADD, rSP(), rSP(), mir_imm(total));

    if (need_dest) {
        a64SpillDst(ctx, instr, a64DestReg(ctx));   /* dst = buffer address */
    } else if (!ret_st && instr->dst && instr->dst->type != IR_TYPE_VOID) {
        if (irIsFloat(instr->dst->type)) a64SpillDstFpr(ctx, instr, 0);
        else                              a64SpillDst(ctx, instr, 0);
    }
}

/* ---------------- AAPCS64 callee param prologue ----------------
 *
 * Receives by-value struct params (and register-overflow scalars) into
 * their frame slots, with the same ABI accounting as the caller. An
 * unreferenced param gets no slot, but the caller still placed it so
 * the counters and the incoming-stack cursor (args start at [x29,
 * #16]) must advance. Reg-resident scalar params are handled by the
 * IR-level arrive+store; here they only advance the counters. */

static void a64AggStore(A64Ctx *ctx, int is_fp, int reg, int loff, int size) {
    if (is_fp) {
        a64FpFrameStore(ctx, reg, size, loff);
    } else {
        if (size != 1 && size != 2 && size != 4 && size != 8)
            loggerPanic("aarch64: %d-byte struct chunk not supported\n", size);
        a64FrameStore(ctx, reg, size, loff);
    }
}

static void a64IndirectToSlot(A64Ctx *ctx, int from_stack, int reg,
                              int incoming_off, int size, int loff)
{
    int base = reg;
    if (from_stack) {
        E2(ctx, A64MN_LDR, rX(9), mir_mem(rX(29), incoming_off));
        base = 9;
    }
    a64CopyToSlot(ctx, base, size, 0, loff);
}

static void a64EmitParamPrologue(A64Ctx *ctx) {
    Ast *ast = ctx->ast;
    IrFunction *fn = ctx->fn;
    if (!ast->params) return;
    int ngrn = 0, nsrn = 0, nsaa = 0;

    /* An INDIRECT (>16-byte) struct return's hidden out-pointer arrives
     * in x8, so it consumes no argument register; a <=16-byte aggregate
     * comes back in registers. Either way ngrn starts at 0. */
    for (u64 i = 0; i < ast->params->size; ++i) {
        Ast *p = vecGet(Ast *, ast->params, i);
        if (p->kind == AST_VAR_ARGS) break;
        AstType *t = p->type;

        IrValue *slot = irFnGetVar(fn, irGetParamId(p));
        int has_slot = slot && mapHasInt(fn->ra.id_to_loff, irVarId(slot));
        int loff = has_slot ? irCgGetLoff(&fn->ra, slot) : 0;

        if (!a64IsByvalStruct(t)) {
            int is_float = (t->kind == AST_TYPE_FLOAT);
            if (is_float ? (nsrn < 8) : (ngrn < 8)) {
                if (is_float) nsrn++; else ngrn++;
            } else {
                if (has_slot) a64CopyToSlot(ctx, 29, t->size, 16 + nsaa, loff);
                nsaa += 8;
            }
            continue;
        }

        int elem = 0, count = 0;
        AapcsClass cls = astAapcsClassify(t, &elem, &count);
        if (cls == AAPCS_HFA) {
            if (nsrn + count <= 8) {
                if (has_slot) {
                    for (int k = 0; k < count; ++k)
                        a64AggStore(ctx, 1, nsrn + k, loff + k * elem, elem);
                }
                nsrn += count;
            } else {
                if (has_slot) a64CopyToSlot(ctx, 29, t->size, 16 + nsaa, loff);
                nsaa += (t->size + 7) & ~7; nsrn = 8;
            }
        } else if (cls == AAPCS_INTEGER) {
            int ngp = (t->size + 7) / 8;
            if (ngrn + ngp <= 8) {
                if (has_slot) for (int k = 0; k < ngp; ++k) {
                    int off = k * 8, rem = t->size - off;
                    a64AggStore(ctx, 0, ngrn + k, loff + off, rem >= 8 ? 8 : rem);
                }
                ngrn += ngp;
            } else {
                if (has_slot) a64CopyToSlot(ctx, 29, t->size, 16 + nsaa, loff);
                nsaa += (t->size + 7) & ~7; ngrn = 8;
            }
        } else { /* AAPCS_INDIRECT */
            int from_stack = (ngrn >= 8);
            int incoming_off = 16 + nsaa;
            int reg = ngrn;
            if (from_stack) nsaa += 8; else ngrn += 1;
            if (has_slot)
                a64IndirectToSlot(ctx, from_stack, reg, incoming_off, t->size, loff);
        }
    }
}

/* Where this function's VARIADIC arguments begin, as an offset from
 * x29. Apple AArch64 passes every variadic argument on the stack, so
 * they sit in the incoming argument area past any named parameter that
 * overflowed the registers. Mirrors a64EmitParamPrologue's accounting:
 * where that loop stops reading is where the variadic list starts. */
static int a64VaRegionOffset(Ast *ast) {
    int ngrn = 0, nsrn = 0, nsaa = 0;
    if (!ast || !ast->params) return 16;
    for (u64 i = 0; i < ast->params->size; ++i) {
        Ast *p = vecGet(Ast *, ast->params, i);
        if (p->kind == AST_VAR_ARGS) break;
        AstType *t = p->type;
        if (!a64IsByvalStruct(t)) {
            int is_float = (t->kind == AST_TYPE_FLOAT);
            if (is_float ? (nsrn < 8) : (ngrn < 8)) {
                if (is_float) nsrn++; else ngrn++;
            } else {
                nsaa += 8;
            }
            continue;
        }
        int elem = 0, count = 0;
        AapcsClass cls = astAapcsClassify(t, &elem, &count);
        if (cls == AAPCS_HFA) {
            if (nsrn + count <= 8) nsrn += count;
            else { nsaa += (t->size + 7) & ~7; nsrn = 8; }
        } else if (cls == AAPCS_INTEGER) {
            int ngp = (t->size + 7) / 8;
            if (ngrn + ngp <= 8) ngrn += ngp;
            else { nsaa += (t->size + 7) & ~7; ngrn = 8; }
        } else {
            if (ngrn >= 8) nsaa += 8; else ngrn += 1;
        }
    }
    return 16 + nsaa;
}

/* ---------------- conversions ----------------
 *
 * Every conversion has the same shape - load the source into the
 * canonical scratch (x0 / d0), emit ONE convert in place, spill the
 * result - with two short cuts: the convert straight between home
 * registers (`uxth w19, w20`), and a sign-extending load folded into
 * the widen. */

typedef enum {
    A64_CVT_UXTB, A64_CVT_UXTH, A64_CVT_MOVW,        /* zero / truncate */
    A64_CVT_SXTB, A64_CVT_SXTH, A64_CVT_SXTW,        /* sign-extend */
} A64CvtKind;

/* The integer narrowing/widening ops, `rd = ext(rs)`. gas spells the
 * zero-extending ones on w registers and the sign-extending ones as
 * `sxtb x, w`. */
static void a64IntConvert(A64Ctx *ctx, A64CvtKind k, int rd, int rs) {
    switch (k) {
        case A64_CVT_UXTB: E2(ctx, A64MN_UXTB, rW(rd), rW(rs)); break;
        case A64_CVT_UXTH: E2(ctx, A64MN_UXTH, rW(rd), rW(rs)); break;
        case A64_CVT_MOVW: E2(ctx, A64MN_MOV,  rW(rd), rW(rs)); break;
        case A64_CVT_SXTB: E2(ctx, A64MN_SXTB, rX(rd), rW(rs)); break;
        case A64_CVT_SXTH: E2(ctx, A64MN_SXTH, rX(rd), rW(rs)); break;
        case A64_CVT_SXTW: E2(ctx, A64MN_SXTW, rX(rd), rW(rs)); break;
    }
}

static int a64CvtKindFor(int size, int sgn, A64CvtKind *out) {
    switch (size) {
        case 1: *out = sgn ? A64_CVT_SXTB : A64_CVT_UXTB; return 1;
        case 2: *out = sgn ? A64_CVT_SXTH : A64_CVT_UXTH; return 1;
        case 4: *out = sgn ? A64_CVT_SXTW : A64_CVT_MOVW; return 1;
        default: return 0;
    }
}

/* `uxth w19, w20` rather than a move in, a convert in x0, a move out.
 * Returns 0 when either end lives in memory. */
static int a64Conv3(A64Ctx *ctx, A64CvtKind k, IrInstr *i) {
    AoStr *d = a64Home(i->dst);
    AoStr *a = a64Home(i->r1);
    if (!d || !a) return 0;
    a64IntConvert(ctx, k, a64RegFromName(d->data), a64RegFromName(a->data));
    return 1;
}

/* Integer widen/narrow through x0: load r1, convert, spill. */
static void a64IntConvertVia0(A64Ctx *ctx, IrInstr *instr, int size, int sgn) {
    A64CvtKind k;
    a64LoadToReg(ctx, instr->r1, 0);
    if (a64CvtKindFor(size, sgn, &k)) a64IntConvert(ctx, k, 0, 0);
    a64SpillDst(ctx, instr, 0);
}

static void a64EmitConvert(A64Ctx *ctx, IrInstr *instr) {
    switch (instr->op) {
    case IR_TRUNC: {
        int sz = instr->dst ? (int)instr->dst->as.var.size : 8;
        /* The result still lives in a 64-bit register, so the high bits
         * have to reflect the destination's signedness - otherwise a
         * later compare reads a different number entirely. */
        int sgn = (instr->flags & IRCG_TRUNC_SIGNED) != 0;
        A64CvtKind k;
        if (a64CvtKindFor(sz, sgn, &k) && a64Conv3(ctx, k, instr)) break;
        a64IntConvertVia0(ctx, instr, sz, sgn);
        break;
    }
    case IR_ZEXT: {
        int sz = (int)irValueByteSize(instr->r1);
        A64CvtKind k;
        if (a64CvtKindFor(sz, 0, &k) && a64Conv3(ctx, k, instr)) break;
        a64LoadToReg(ctx, instr->r1, 0);
        /* A 1- or 2-byte frame load already zero-extends, so widening
         * it again is a no-op. A 4-byte load does NOT - it
         * sign-extends, matching `movslq` - so that one still needs
         * the explicit widen. */
        int fused_zx = irValueInMemory(instr->r1) && (sz == 1 || sz == 2);
        if (!fused_zx && a64CvtKindFor(sz, 0, &k)) a64IntConvert(ctx, k, 0, 0);
        a64SpillDst(ctx, instr, 0);
        break;
    }
    case IR_SEXT: {
        int sz = (int)irValueByteSize(instr->r1);
        /* A 4-byte load already sign-extended to 64 bits, so this widen
         * is a copy. When source and destination share a register it is
         * not even that. */
        if (instr->flags & IRCG_SEXT_NOOP) {
            a64LoadToReg(ctx, instr->r1, 0);
            a64SpillDst(ctx, instr, 0);
            break;
        }
        A64CvtKind k;
        if (a64CvtKindFor(sz, 1, &k) && a64Conv3(ctx, k, instr)) break;
        /* Fold the widen into the load where the operand is in memory:
         * one `ldrsb` instead of `ldrb` + `sxtb`. */
        if (irValueInMemory(instr->r1) && (sz == 1 || sz == 2 || sz == 4)) {
            a64FrameLoadSx(ctx, 0, sz, irCgGetLoff(&ctx->fn->ra, instr->r1));
            a64SpillDst(ctx, instr, 0);
            break;
        }
        a64IntConvertVia0(ctx, instr, sz, 1);
        break;
    }
    case IR_FPTRUNC:
        a64LoadToFpr(ctx, instr->r1, 0);
        E2(ctx, A64MN_FCVT, rS(0), rD(0));
        a64SpillDstFpr(ctx, instr, 0);
        break;
    case IR_FPEXT:
        a64LoadToFpr(ctx, instr->r1, 0);
        E2(ctx, A64MN_FCVT, rD(0), rS(0));
        a64SpillDstFpr(ctx, instr, 0);
        break;
    case IR_FPTOSI:
    case IR_FPTOUI: {
        int w = (int)irValueByteSize(instr->r1);
        a64LoadToFpr(ctx, instr->r1, 0);
        E2(ctx, instr->op == IR_FPTOSI ? A64MN_FCVTZS : A64MN_FCVTZU,
           rX(0), rF(w, 0));
        a64SpillDst(ctx, instr, 0);
        break;
    }
    case IR_SITOFP:
    case IR_UITOFP: {
        int w = (int)irValueByteSize(instr->dst);
        a64LoadToReg(ctx, instr->r1, 0);
        E2(ctx, instr->op == IR_SITOFP ? A64MN_SCVTF : A64MN_UCVTF,
           rF(w, 0), rX(0));
        a64SpillDstFpr(ctx, instr, 0);
        break;
    }
    case IR_PTRTOINT:
    case IR_INTTOPTR:
    case IR_BITCAST: {
        int dst_fp = irIsFloat(instr->dst->type);
        int src_fp = irIsFloat(instr->r1->type);
        if (dst_fp == src_fp) {
            if (dst_fp) { a64LoadToFpr(ctx, instr->r1, 0); a64SpillDstFpr(ctx, instr, 0); }
            else        { a64LoadToReg(ctx, instr->r1, 0); a64SpillDst(ctx, instr, 0); }
        } else if (dst_fp) {
            int w = (int)irValueByteSize(instr->dst);
            a64LoadToReg(ctx, instr->r1, 0);
            E2(ctx, A64MN_FMOV, rF(w, 0), rG(w, 0));
            a64SpillDstFpr(ctx, instr, 0);
        } else {
            int w = (int)irValueByteSize(instr->r1);
            a64LoadToFpr(ctx, instr->r1, 0);
            E2(ctx, A64MN_FMOV, rG(w, 0), rF(w, 0));
            a64SpillDst(ctx, instr, 0);
        }
        break;
    }
    default: break;
    }
}

/* ---------------- arithmetic ----------------
 *
 * The operand pattern: `dst = r1 <op> r2` straight into the operands'
 * home registers when every end has one, else load r1 into the
 * scratch, fold a constant r2 when it encodes, use the register path
 * otherwise, then spill. Canonical regs: x0/x1 for GP, d0/d1 for FP. */

static int a64AluMn(IrOp op) {
    switch (op) {
        case IR_IADD: return A64MN_ADD;
        case IR_ISUB: return A64MN_SUB;
        case IR_AND:  return A64MN_AND;
        case IR_OR:   return A64MN_ORR;
        case IR_XOR:  return A64MN_EOR;
        default: return TASM_MN_UNKNOWN;
    }
}

static int a64ShiftMn(IrOp op) {
    return op == IR_SHL ? A64MN_LSL : op == IR_SHR ? A64MN_LSR : A64MN_ASR;
}

/* `rd = ra +/- imm` when the immediate (or its negation) encodes. Only
 * add/sub have an immediate form here; the logical ops fall back. */
static int a64AluImm(A64Ctx *ctx, IrOp op, int rd, int ra, s64 imm) {
    if (op != IR_IADD && op != IR_ISUB) return 0;
    int as_add = (op == IR_IADD);
    if (a64IsAddImm(imm)) { /* keep */ }
    else if (imm < 0 && a64IsAddImm(-imm)) { as_add = !as_add; imm = -imm; }
    else return 0;
    E3(ctx, as_add ? A64MN_ADD : A64MN_SUB, rX(rd), rX(ra), mir_imm(imm));
    return 1;
}

/* `add x19, x20, x21` rather than three moves and an add through x0. */
static int a64Alu3(A64Ctx *ctx, IrOp op, IrInstr *i) {
    AoStr *d = a64Home(i->dst);
    AoStr *a = a64Home(i->r1);
    if (!d || !a) return 0;
    int rd = a64RegFromName(d->data), ra = a64RegFromName(a->data);
    if (i->r2 && i->r2->kind == IR_VAL_CONST_INT)
        return a64AluImm(ctx, op, rd, ra, i->r2->as._i64);
    AoStr *b = a64Home(i->r2);
    if (!b) return 0;
    E3(ctx, a64AluMn(op), rX(rd), rX(ra), rX(a64RegFromName(b->data)));
    return 1;
}

static int a64Shift3(A64Ctx *ctx, IrOp op, IrInstr *i) {
    AoStr *d = a64Home(i->dst);
    AoStr *a = a64Home(i->r1);
    if (!d || !a) return 0;
    int rd = a64RegFromName(d->data), ra = a64RegFromName(a->data);
    IrValue *r2 = i->r2;
    if (r2 && r2->kind == IR_VAL_CONST_INT && r2->as._i64 >= 0 && r2->as._i64 < 64) {
        E3(ctx, a64ShiftMn(op), rX(rd), rX(ra), mir_imm(r2->as._i64));
        return 1;
    }
    AoStr *b = a64Home(r2);
    if (!b) return 0;
    E3(ctx, a64ShiftMn(op), rX(rd), rX(ra), rX(a64RegFromName(b->data)));
    return 1;
}

static int a64Mul3(A64Ctx *ctx, IrInstr *i) {
    AoStr *d = a64Home(i->dst);
    AoStr *a = a64Home(i->r1);
    AoStr *b = a64Home(i->r2);
    if (!d || !a || !b) return 0;
    E3(ctx, A64MN_MUL, rX(a64RegFromName(d->data)), rX(a64RegFromName(a->data)),
       rX(a64RegFromName(b->data)));
    return 1;
}

static int a64Unary3(A64Ctx *ctx, IrOp op, IrInstr *i) {
    AoStr *d = a64Home(i->dst);
    AoStr *a = a64Home(i->r1);
    if (!d || !a) return 0;
    E2(ctx, op == IR_INEG ? A64MN_NEG : A64MN_MVN,
       rX(a64RegFromName(d->data)), rX(a64RegFromName(a->data)));
    return 1;
}

static void a64EmitArith(A64Ctx *ctx, IrInstr *instr) {
    IrOp op = instr->op;
    switch (op) {
    case IR_IADD: case IR_ISUB: case IR_AND: case IR_OR: case IR_XOR: {
        if (a64Alu3(ctx, op, instr)) break;
        a64LoadToReg(ctx, instr->r1, 0);
        int has_imm = instr->r2 && instr->r2->kind == IR_VAL_CONST_INT;
        if (!(has_imm && a64AluImm(ctx, op, 0, 0, instr->r2->as._i64))) {
            int rhs = 1;
            if (instr->r2->loc.kind == IR_LOC_REG && instr->r2->loc.as.reg)
                rhs = a64RegFromName(instr->r2->loc.as.reg->data);
            else a64LoadToReg(ctx, instr->r2, 1);
            E3(ctx, a64AluMn(op), rX(0), rX(0), rX(rhs));
        }
        a64SpillDst(ctx, instr, 0);
        break;
    }
    case IR_IMUL:
        if (a64Mul3(ctx, instr)) break;
        a64LoadToReg(ctx, instr->r1, 0);
        a64LoadToReg(ctx, instr->r2, 1);
        E3(ctx, A64MN_MUL, rX(0), rX(0), rX(1));
        a64SpillDst(ctx, instr, 0);
        break;
    case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM: {
        a64LoadToReg(ctx, instr->r1, 0);
        a64LoadToReg(ctx, instr->r2, 1);
        int is_signed = (op == IR_IDIV || op == IR_IREM);
        int is_rem = (op == IR_IREM || op == IR_UREM);
        int div = is_signed ? A64MN_SDIV : A64MN_UDIV;
        if (!is_rem) {
            E3(ctx, div, rX(0), rX(0), rX(1));
        } else {
            E3(ctx, div, rX(2), rX(0), rX(1));
            E4(ctx, A64MN_MSUB, rX(0), rX(2), rX(1), rX(0));
        }
        a64SpillDst(ctx, instr, 0);
        break;
    }
    case IR_INEG: case IR_NOT:
        if (a64Unary3(ctx, op, instr)) break;
        a64LoadToReg(ctx, instr->r1, 0);
        E2(ctx, op == IR_INEG ? A64MN_NEG : A64MN_MVN, rX(0), rX(0));
        a64SpillDst(ctx, instr, 0);
        break;
    case IR_SHL: case IR_SHR: case IR_SAR: {
        if (a64Shift3(ctx, op, instr)) break;
        a64LoadToReg(ctx, instr->r1, 0);
        IrValue *r2 = instr->r2;
        if (r2 && r2->kind == IR_VAL_CONST_INT && r2->as._i64 >= 0 && r2->as._i64 < 64) {
            E3(ctx, a64ShiftMn(op), rX(0), rX(0), mir_imm(r2->as._i64));
        } else {
            a64LoadToReg(ctx, r2, 1);
            E3(ctx, a64ShiftMn(op), rX(0), rX(0), rX(1));
        }
        a64SpillDst(ctx, instr, 0);
        break;
    }
    case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV: {
        int w = (int)irValueByteSize(instr->dst);
        a64LoadToFpr(ctx, instr->r1, 0);
        a64LoadToFpr(ctx, instr->r2, 1);
        int mn = op == IR_FADD ? A64MN_FADD : op == IR_FSUB ? A64MN_FSUB
               : op == IR_FMUL ? A64MN_FMUL : A64MN_FDIV;
        E3(ctx, mn, rF(w, 0), rF(w, 0), rF(w, 1));
        a64SpillDstFpr(ctx, instr, 0);
        break;
    }
    case IR_FNEG: {
        int w = (int)irValueByteSize(instr->dst);
        a64LoadToFpr(ctx, instr->r1, 0);
        E2(ctx, A64MN_FNEG, rF(w, 0), rF(w, 0));
        a64SpillDstFpr(ctx, instr, 0);
        break;
    }
    default: break;
    }
}

/* ---------------- compares ---------------- */

/* Flags := r1 <cmp> r2. `cmp` reads its operands and writes only the
 * flags, so operands already in registers are named directly. */
static void a64EmitCompare(A64Ctx *ctx, IrInstr *instr, int is_float) {
    if (is_float) {
        int w = (int)irValueByteSize(instr->r1);
        a64LoadToFpr(ctx, instr->r1, 0);
        if (instr->r2 && instr->r2->kind == IR_VAL_CONST_FLOAT &&
            ieee754_64(instr->r2->as._f64) == 0)
        {
            E2(ctx, A64MN_FCMP, rF(w, 0), mir_fimm(0.0));
        } else {
            a64LoadToFpr(ctx, instr->r2, 1);
            E2(ctx, A64MN_FCMP, rF(w, 0), rF(w, 1));
        }
        return;
    }
    AoStr *lhs_home = a64Home(instr->r1);
    int lhs = 0;
    if (lhs_home) lhs = a64RegFromName(lhs_home->data);
    else          a64LoadToReg(ctx, instr->r1, 0);
    if (instr->r2 && instr->r2->kind == IR_VAL_CONST_INT &&
        a64IsAddImm(instr->r2->as._i64))
    {
        E2(ctx, A64MN_CMP, rX(lhs), mir_imm(instr->r2->as._i64));
    } else {
        AoStr *rhs_home = a64Home(instr->r2);
        int rhs = 1;
        if (rhs_home) rhs = a64RegFromName(rhs_home->data);
        else          a64LoadToReg(ctx, instr->r2, 1);
        E2(ctx, A64MN_CMP, rX(lhs), rX(rhs));
    }
}

/* Conditional branch to `t`, else `f`, given the flags are set and
 * `cc_t`/`cc_f` are the taken/not-taken conditions. Phi moves on an
 * edge need a landing strip of their own. */
static void a64EmitCondBranch(A64Ctx *ctx, IrBlock *t, IrBlock *f,
                              const char *cc_t, const char *cc_f)
{
    HccJit *jit = ctx->jit;
    int t_ln = hccJitBlockLocalNum(jit, ctx->fn, t);
    int f_ln = hccJitBlockLocalNum(jit, ctx->fn, f);
    if (!irBlockHasPhi(t) && !irBlockHasPhi(f)) {
        if (ctx->next_block == t) {
            EBCC(ctx, cc_f, f_ln);
        } else if (ctx->next_block == f) {
            EBCC(ctx, cc_t, t_ln);
        } else {
            EBCC(ctx, cc_t, t_ln);
            EB(ctx, f_ln);
        }
        return;
    }
    int else_ln = hccJitFreshLocalNum(jit);
    EBCC(ctx, cc_f, else_ln);
    a64PhiMaterialise(ctx, ctx->cur_block, t);
    EB(ctx, t_ln);
    ELABEL(ctx, else_ln);
    a64PhiMaterialise(ctx, ctx->cur_block, f);
    if (ctx->next_block != f) EB(ctx, f_ln);
}

/* ---------------- the main IR emitter ---------------- */

static void a64EmitInstr(A64Ctx *ctx, IrInstr *instr) {
    HccJit *jit = ctx->jit;

    switch (instr->op) {
        case IR_NOP:
        case IR_LABEL:
        case IR_ALLOCA:
        case IR_GEP:
            break;

        case IR_LOAD: {
            /* IR_LOAD names a home, so a register home makes the load
             * a read of that register. */
            AoStr *home = instr->r1 ? a64Home(instr->r1) : NULL;
            if (home) {
                int src = a64RegFromName(home->data);
                if (instr->dst && irIsFloat(instr->dst->type) &&
                    a64IsFpRegName(home->data))
                {
                    int w = (int)irValueByteSize(instr->dst);
                    a64FpMove(ctx, w, 0, a64FpRegWidth(home->data), src);
                    a64SpillDstFpr(ctx, instr, 0);
                } else {
                    E2(ctx, A64MN_MOV, rX(0), rX(src));
                    a64SpillDst(ctx, instr, 0);
                }
                break;
            }
            int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
            if (instr->dst && irIsFloat(instr->dst->type)) {
                a64FpFrameLoad(ctx, 0, (int)instr->dst->as.var.size, loff);
                a64SpillDstFpr(ctx, instr, 0);
                break;
            }
            int size = instr->dst ? (int)instr->dst->as.var.size : 8;
            a64FrameLoad(ctx, 0, size, loff);
            a64SpillDst(ctx, instr, 0);
            break;
        }

        case IR_STORE: {
            IrValue *r1 = instr->r1;
            IrValue *dst = instr->dst;
            if (r1 && irIsFloat(r1->type)) {
                int src = a64SrcReg(ctx, instr, 1);
                a64StoreFprSized(ctx, dst, src, (int)irValueByteSize(r1));
                break;
            }
            /* Pinned destination lives in a register, not a slot. */
            if (dst && dst->pinned_reg) {
                int src = a64SrcReg(ctx, instr, 0);
                E2(ctx, A64MN_MOV, rX(a64RegFromName(dst->pinned_reg->data)),
                   rX(src));
                break;
            }
            /* Width is the destination's, not the source's: a wide value
             * stored into a narrow slot must truncate, else the extra
             * bytes clobber neighbouring slots. */
            int src_size = (int)irValueByteSize(r1);
            int dst_size = (int)irValueByteSize(dst);
            int src = a64SrcReg(ctx, instr, 0);
            a64FrameStore(ctx, src, dst_size < src_size ? dst_size : src_size,
                          irCgGetLoff(&ctx->fn->ra, dst));
            break;
        }

        case IR_LOAD_DEREF: {
            int is_fp = instr->dst && irIsFloat(instr->dst->type);
            int size = instr->dst ? (int)instr->dst->as.var.size : 8;
            if (is_fp) size = (int)irValueByteSize(instr->dst);
            /* Global base: materialise the address into x1, then load. */
            if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
                a64GlobalAddr(ctx, 1, a64GlobalName(ctx, instr->r1->as.global.name));
                if (is_fp) {
                    a64FpDerefAccess(ctx, 1, size, 0, 1, 0, 0, 0, instr->disp);
                    a64SpillDstFpr(ctx, instr, 0);
                } else {
                    a64DerefLoad(ctx, size, 0, 1, 0, 0, 0, instr->disp);
                    a64SpillDst(ctx, instr, 0);
                }
                break;
            }
            /* Index home first, then a base scratch that avoids it. */
            int idx_home; int idx_homed = a64IdxHome(instr, &idx_home);
            int base;
            if (!a64BaseHome(instr->r1, instr, &base)) {
                base = a64ScratchTake(idx_homed, idx_home);
                a64LoadToReg(ctx, instr->r1, base);
            }
            int idx; int has_idx = a64IdxReg(ctx, instr, base, &idx);
            if (is_fp) {
                a64FpDerefAccess(ctx, 1, size, 0, base, has_idx, idx,
                                 instr->scale, instr->disp);
                a64SpillDstFpr(ctx, instr, 0);
                break;
            }
            a64DerefLoad(ctx, size, 0, base, has_idx, idx, instr->scale, instr->disp);
            a64SpillDst(ctx, instr, 0);
            break;
        }

        case IR_STORE_DEREF: {
            int is_fp = instr->r1 && irIsFloat(instr->r1->type);
            int sz = (int)irValueByteSize(instr->r1);
            if (instr->dst && instr->dst->kind == IR_VAL_GLOBAL) {
                a64GlobalAddr(ctx, 1, a64GlobalName(ctx, instr->dst->as.global.name));
                if (is_fp) {
                    a64LoadToFpr(ctx, instr->r1, 0);
                    a64FpDerefAccess(ctx, 0, sz, 0, 1, 0, 0, 0, instr->disp);
                } else {
                    a64LoadToReg(ctx, instr->r1, 0);
                    a64DerefStore(ctx, sz, 0, 1, 0, 0, 0, instr->disp);
                }
                break;
            }
            int idx_home; int idx_homed = a64IdxHome(instr, &idx_home);
            int base;
            if (!a64BaseHome(instr->dst, instr, &base)) {
                base = a64ScratchTake(idx_homed, idx_home);
                a64LoadToReg(ctx, instr->dst, base);
            }
            int idx; int has_idx = a64IdxReg(ctx, instr, base, &idx);
            if (is_fp) {
                a64LoadToFpr(ctx, instr->r1, 0);
                a64FpDerefAccess(ctx, 0, sz, 0, base, has_idx, idx,
                                 instr->scale, instr->disp);
                break;
            }
            int val = 0;
            if (has_idx && val == idx) val = 9;
            if (val == base) val = 9;
            a64LoadToReg(ctx, instr->r1, val);
            a64DerefStore(ctx, sz, val, base, has_idx, idx, instr->scale, instr->disp);
            break;
        }

        case IR_RMW_DEREF: {
            int idx_home; int idx_homed = a64IdxHome(instr, &idx_home);
            int base;
            if (!a64BaseHome(instr->dst, instr, &base)) {
                base = a64ScratchTake(idx_homed, idx_home);
                if (instr->dst && instr->dst->kind == IR_VAL_GLOBAL) {
                    a64GlobalAddr(ctx, base,
                                  a64GlobalName(ctx, instr->dst->as.global.name));
                } else {
                    a64LoadToReg(ctx, instr->dst, base);
                }
            }
            int idx; int has_idx = a64IdxReg(ctx, instr, base, &idx);
            int sz = (int)irValueByteSize(instr->r1);
            a64DerefLoad(ctx, sz, 0, base, has_idx, idx, instr->scale, instr->disp);
            IrOp rop = instr->extra.rmw_op;
            /* Fold add/sub immediates; and/or/xor stay on the register
             * path (no logical-immediate form here). */
            if (instr->r1 && instr->r1->kind == IR_VAL_CONST_INT &&
                (rop == IR_IADD || rop == IR_ISUB) &&
                a64IsAddImm(instr->r1->as._i64))
            {
                E3(ctx, rop == IR_IADD ? A64MN_ADD : A64MN_SUB, rX(0), rX(0),
                   mir_imm(instr->r1->as._i64));
            } else {
                a64LoadToReg(ctx, instr->r1, 9);
                int mn = a64AluMn(rop);
                if (mn == TASM_MN_UNKNOWN)
                    loggerPanic("aarch64: bad RMW op %d\n", rop);
                E3(ctx, mn, rX(0), rX(0), rX(9));
            }
            a64DerefStore(ctx, sz, 0, base, has_idx, idx, instr->scale, instr->disp);
            break;
        }

        case IR_LEA: {
            /* Compute the address straight into the destination's home
             * when it has one; addresses are taken constantly. */
            AoStr *home = a64Home(instr->dst);
            int out = home ? a64RegFromName(home->data) : 0;
            if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
                const char *name = a64GlobalName(ctx, instr->r1->as.global.name);
                int got = 0;
                if (instr->r1->flags & IR_VAL_FLAG_FUNC) {
                    name = asmNormaliseFunctionName(ctx->cc,
                            instr->r1->as.global.name);
                    got = asmFunctionIsExtern(ctx->cc, instr->r1->as.global.name);
                }
                a64GlobalAddrEx(ctx, out, name, got);
            } else if (instr->r1) {
                int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
                if (loff >= 0 && a64IsAddImm(loff)) {
                    E3(ctx, A64MN_ADD, rX(out), rX(29), mir_imm(loff));
                } else if (loff < 0 && a64IsAddImm(-loff)) {
                    E3(ctx, A64MN_SUB, rX(out), rX(29), mir_imm(-loff));
                } else {
                    a64MovImm(ctx, 9, (s64)loff);
                    E3(ctx, A64MN_ADD, rX(out), rX(29), rX(9));
                }
            }
            /* A no-op when `out` is already the home. */
            a64SpillDst(ctx, instr, out);
            break;
        }

        case IR_IADD: case IR_ISUB: case IR_AND: case IR_OR: case IR_XOR:
        case IR_IMUL: case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
        case IR_INEG: case IR_NOT: case IR_SHL: case IR_SHR: case IR_SAR:
        case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV: case IR_FNEG:
            a64EmitArith(ctx, instr);
            break;

        case IR_ICMP:
            a64EmitCompare(ctx, instr, 0);
            a64EmitCSet(ctx, instr->extra.cmp_kind, 0);
            a64SpillDst(ctx, instr, 0);
            break;

        case IR_FCMP:
            a64EmitCompare(ctx, instr, 1);
            a64EmitCSet(ctx, instr->extra.cmp_kind, 1);
            a64SpillDst(ctx, instr, 0);
            break;

        case IR_CMP_BR: {
            IrCmpKind kind = instr->extra.cmp_br.cmp_kind;
            int is_float = instr->r1 && irIsFloat(instr->r1->type);
            a64EmitCompare(ctx, instr, is_float);
            a64EmitCondBranch(ctx, instr->extra.cmp_br.target_block,
                              instr->extra.cmp_br.fallthrough_block,
                              a64CcFor(kind, is_float), a64CcInvFor(kind, is_float));
            break;
        }

        case IR_BR:
            a64LoadToReg(ctx, instr->dst, 0);
            E2(ctx, A64MN_CMP, rX(0), mir_imm(0));
            a64EmitCondBranch(ctx, instr->extra.blocks.target_block,
                              instr->extra.blocks.fallthrough_block, "ne", "eq");
            break;

        case IR_JMP: {
            IrBlock *target = instr->extra.blocks.target_block;
            a64PhiMaterialise(ctx, ctx->cur_block, target);
            if (target != ctx->next_block) {
                EB(ctx, hccJitBlockLocalNum(jit, ctx->fn, target));
            }
            break;
        }

        case IR_RET:
            if (instr->dst && instr->dst->byval_struct_type) {
                /* <=16-byte struct returned in registers: load its bytes
                 * from the return slot into v0.. (HFA) or x0,x1. */
                AstType *t = instr->dst->byval_struct_type;
                int loff = irCgGetLoff(&ctx->fn->ra, instr->dst);
                int re = 0, rc = 0;
                AapcsClass rcls = astAapcsClassify(t, &re, &rc);
                if (rcls == AAPCS_HFA) {
                    for (int k = 0; k < rc; ++k)
                        a64FpFrameLoad(ctx, k, re, loff + k * re);
                } else {
                    int ngp = (t->size + 7) / 8;
                    for (int k = 0; k < ngp; ++k) {
                        int off = k * 8, rem = t->size - off;
                        a64FrameLoad(ctx, k, rem >= 8 ? 8 : rem, loff + off);
                    }
                }
            } else if (instr->dst) {
                if (irIsFloat(instr->dst->type)) a64LoadToFpr(ctx, instr->dst, 0);
                else                              a64LoadToReg(ctx, instr->dst, 0);
            }
            /* The epilogue label is defined straight after the last
             * block, so a ret ending that block falls through to it. */
            if (ctx->next_block != NULL ||
                ctx->cur_block->instructions->prev->value != (void *)instr)
            {
                EB(ctx, hccJitEpilogueLocalNum(jit, ctx->fn));
            }
            break;

        case IR_TRUNC: case IR_ZEXT: case IR_SEXT:
        case IR_FPTRUNC: case IR_FPEXT: case IR_FPTOSI: case IR_FPTOUI:
        case IR_SITOFP: case IR_UITOFP:
        case IR_PTRTOINT: case IR_INTTOPTR: case IR_BITCAST:
            a64EmitConvert(ctx, instr);
            break;

        case IR_PHI:
            /* Materialised at the predecessor's terminator. */
            break;

        case IR_CALL: {
            IrValue *wrap = instr->r1;
            Vec *args = wrap ? wrap->as.array.values : NULL;
            AoStr *fname = wrap ? wrap->as.array.label : NULL;
            int indirect = (fname == NULL);
            if (indirect) {
                if (!instr->r2) loggerPanic("aarch64: indirect call without target\n");
                a64LoadToReg(ctx, instr->r2, 16);
            }
            /* By-value struct args: use the uniform AAPCS path. */
            int use_aapcs = (instr->flags & IRCG_CALL_AGG_RETURN) != 0;
            if (args) {
                for (u64 ai = 0; ai < args->size; ++ai) {
                    if (vecGet(IrValue *, args, ai)->byval_struct_type) {
                        use_aapcs = 1;
                        break;
                    }
                }
            }
            if (use_aapcs) {
                a64EmitAapcsCall(ctx, instr, args, fname, indirect);
                break;
            }
            Ast *callee = NULL;
            if (!indirect) {
                callee = (Ast *)mapGetLen(ctx->cc->global_env,
                                          fname->data, fname->len);
            }
            int callee_va = 0, named_count = 0;
            if (callee) {
                if ((callee->type && callee->type->has_var_args) || callee->has_var_args)
                    callee_va = 1;
                else if (callee->params && callee->params->size > 0) {
                    Ast *last = vecGet(Ast *, callee->params, callee->params->size - 1);
                    if (last && last->kind == AST_VAR_ARGS) callee_va = 1;
                }
                if (callee->params) {
                    named_count = (int)callee->params->size;
                    if (callee_va && named_count > 0) named_count--;
                }
            }
            int apple_target = ctx->cc->target == TARGET_AARCH64_APPLE_DARWIN;
            int holyc_variadic = callee_va && astUsesHolyCVarargs(callee);
            int extern_variadic = callee_va && !holyc_variadic;
            int force_va_stack = holyc_variadic || (extern_variadic && apple_target);

            u64 n = args ? args->size : 0;
            u8 *is_stack = n ? calloc(n, 1) : NULL;
            s32 n_stack = a64PartitionCallArgs(is_stack, n, args, callee_va,
                                               named_count, force_va_stack);
            int stack_bytes = (n_stack * 8 + 15) & ~15;
            a64SpAdjust(ctx, 1, (u64)stack_bytes);
            int stack_idx = 0;
            for (u64 i = 0; i < n; ++i) {
                if (!is_stack[i]) continue;
                IrValue *a = vecGet(IrValue *, args, i);
                int off = stack_idx * 8;
                if (irIsFloat(a->type)) {
                    int w = (int)irValueByteSize(a);
                    a64LoadToFpr(ctx, a, 0);
                    E2(ctx, A64MN_STR, rF(w, 0), mir_mem(rSP(), off));
                } else {
                    a64LoadToReg(ctx, a, 9);
                    E2(ctx, A64MN_STR, rX(9), mir_mem(rSP(), off));
                }
                stack_idx++;
            }
            int int_idx = 0, float_idx = 0;
            for (u64 i = 0; i < n; ++i) {
                if (is_stack[i]) continue;
                IrValue *a = vecGet(IrValue *, args, i);
                if (irIsFloat(a->type)) a64LoadToFpr(ctx, a, float_idx++);
                else                    a64LoadToReg(ctx, a, int_idx++);
            }
            a64Call(ctx, fname, indirect);
            a64SpAdjust(ctx, 0, (u64)stack_bytes);
            if (instr->dst && instr->dst->type != IR_TYPE_VOID) {
                if (irIsFloat(instr->dst->type)) a64SpillDstFpr(ctx, instr, 0);
                else                              a64SpillDst(ctx, instr, 0);
            }
            free(is_stack);
            break;
        }

        case IR_ASM:
            /* Inline `asm {...}`: the bytes are assembled separately and
             * spliced in at this point, so everything selected so far
             * has to be sunk first. No peephole across the barrier. */
            if (instr->extra.asm_fragments) {
                loggerPanic("aarch64: `&var` asm fragments are not "
                            "supported; address locals directly\n");
            }
            a64Flush(ctx, 0);
            if (instr->r1 && instr->r1->as.str.str) {
                if (ctx->text) {
                    asmEmitBlockBytes(ctx->cc, ctx->text, instr->r1->as.str.str, 0);
                } else if (hccJitAssembleText(jit, instr->r1->as.str.str, 0) != 0) {
                    loggerPanic("aarch64: failed to assemble inline asm\n");
                }
            }
            break;

        case IR_VA_START: {
            /* The variadic arguments live in the incoming argument area,
             * past any named parameter that overflowed the registers. */
            if (ctx->cc->target != TARGET_AARCH64_APPLE_DARWIN) {
                loggerPanic("aarch64: <stdarg.h> is implemented for "
                            "aarch64-apple-darwin only\n");
            }
            int va_off = a64VaRegionOffset(ctx->ast);
            E2(ctx, A64MN_MOV, rX(0), rX(29));
            a64AddSubImm(ctx, 0, (s64)va_off);
            a64SpillDst(ctx, instr, 0);
            break;
        }

        case IR_FRAME_ADDR:
            E2(ctx, A64MN_MOV, rX(0), rX(29));
            a64SpillDst(ctx, instr, 0);
            break;

        case IR_SELECT:
        case IR_SWITCH:
        case IR_VA_ARG:
        case IR_VA_END:
            loggerPanic("aarch64: op %d not yet implemented\n", instr->op);
        default:
            loggerPanic("aarch64: unknown op %d\n", instr->op);
    }
}

/* ---------------- prologue / epilogue ----------------
 *
 * Callee-saved registers live at the BOTTOM of the frame - `[sp, #0]`
 * upwards - because locals are addressed downwards from x29 and the
 * save area has to miss them. Reserving it inside `stack_space` means
 * the prologue and the epilogue derive the same frame size from the
 * same field. Not pushed before `mov x29, sp`: that would put them
 * between x29 and the saved {x29,x30} pair and break the frame chain a
 * debugger walks. */

static int a64SaveAreaBytes(IrFunction *fn) {
    Vec *saved = irCgPinnedCalleeSaved(fn);
    int bytes = (int)saved->size * 8;
    vecRelease(saved);
    return (bytes + 15) & ~15;
}

static void a64EmitSaveRestore(A64Ctx *ctx, int store) {
    Vec *saved = irCgPinnedCalleeSaved(ctx->fn);
    for (u64 i = 0; i < saved->size; ++i) {
        AoStr *r = (AoStr *)saved->entries[i];
        int reg = a64RegFromName(r->data);
        int mn = store ? A64MN_STR : A64MN_LDR;
        AsmOperand op = a64IsFpRegName(r->data) ? rD(reg) : rX(reg);
        E2(ctx, mn, op, mir_mem(rSP(), (s64)(i * 8)));
    }
    vecRelease(saved);
}

static void a64EmitPrologue(A64Ctx *ctx) {
    if (ctx->omit_frame) return;
    E3(ctx, A64MN_STP, rX(29), rX(30), mir_mem_pre(rSP(), -16));
    E2(ctx, A64MN_MOV, rX(29), rSP());
    u32 aligned = ((u32)ctx->fn->stack_space + 15u) & ~15u;
    ctx->aligned_frame = aligned;
    a64SpAdjust(ctx, 1, aligned);
    a64EmitSaveRestore(ctx, 1);
}

static void a64EmitEpilogue(A64Ctx *ctx) {
    if (ctx->omit_frame) {
        E0(ctx, A64MN_RET);
        return;
    }
    a64EmitSaveRestore(ctx, 0);   /* before sp moves */
    a64SpAdjust(ctx, 0, ctx->aligned_frame);
    E3(ctx, A64MN_LDP, rX(29), rX(30), mir_mem_post(rSP(), 16));
    E0(ctx, A64MN_RET);
}

/* ---------------- per-function selection ---------------- */

/* Select `fn` (already lowered, optimised and laid out) into MIR and
 * sink it: into `jit->enc` when `text` is NULL, as GNU-as text
 * otherwise. Non-zero when the encoder rejected a line. */
static int a64EmitFunction(HccJit *jit, IrFunction *fn, Ast *ast, AoStr *text) {
    int is_variadic = (ast->type && ast->type->has_var_args) || ast->has_var_args;
    /* A by-value struct param is unpacked into a frame slot relative to
     * x29 by the param prologue, so the frame must be set up even if
     * the body looks trivial. A variadic function reads argc/argv
     * relative to x29 too. */
    int has_struct_param = 0;
    if (ast->params) {
        for (u64 pi = 0; pi < ast->params->size; ++pi) {
            Ast *p = vecGet(Ast *, ast->params, pi);
            if (a64IsByvalStruct(p->type)) { has_struct_param = 1; break; }
        }
    }
    /* Reserve the callee-saved area before anything reads stack_space:
     * the prologue, the epilogue and the frame-elision test all derive
     * their size from this one field. */
    fn->stack_space = (u16)(fn->stack_space + a64SaveAreaBytes(fn));

    A64Ctx ctx = {0};
    ctx.jit = jit;
    ctx.cc = jit->cc;
    ctx.fn = fn;
    ctx.ast = ast;
    ctx.text = text;
    ctx.omit_frame = (fn->stack_space == 0) && !irFnHasCalls(fn) &&
                     (ast->loff == 0) && !is_variadic && !has_struct_param;

    char *fname = asmNormaliseFunctionName(jit->cc, ast->fname);
    ctx.mir = mir_fn_new(fname, TASM_ARCH_ARM64);

    if (text) {
        aoStrRemovePreviousChar(text, '\t');
        aoStrCatFmt(text, ".text\n\t.p2align 2\n\t.globl %s\n", fname);
    }

    a64EmitPrologue(&ctx);
    a64EmitParamPrologue(&ctx);

    Set *referenced = irCgComputeReferencedBlocks(fn);
    listForEach(fn->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        ctx.cur_block = block;
        ctx.next_block = (it->next != fn->blocks) ? (IrBlock *)it->next->value : NULL;
        if (setHas(referenced, (void *)(u64)block->id)) {
            ELABEL(&ctx, hccJitBlockLocalNum(jit, fn, block));
        }
        listForEach(block->instructions) {
            IrInstr *in = (IrInstr *)it->value;
            ctx.src_line = in->line;
            a64EmitInstr(&ctx, in);
        }
    }
    setRelease(referenced);

    ELABEL(&ctx, hccJitEpilogueLocalNum(jit, fn));
    ctx.src_line = 0;
    a64EmitEpilogue(&ctx);

    int rc = a64Flush(&ctx, 1);
    mir_fn_free(ctx.mir);
    return rc ? -1 : 0;
}

/* The JIT / object-writer entry: lower, optimise, lay out, select. */
static int a64CompileFunction(HccJit *jit, Ast *ast, IrCtx *ir_ctx) {
    IrFunction *fn = irLowerFunction(ir_ctx, ast);
    irBasicFunctionOptimisations(fn);
    if (irInlineFunction(ir_ctx, fn)) irBasicFunctionOptimisations(fn);

    /* Layout passes (slot offsets, IR-CG ctx), so the regalloc loff map
     * is populated before anything asks for offsets. */
    IrCgCtx cg = {0};
    cg.cc = jit->cc;
    cg.fn = fn;
    irFunctionPrepForCodeGen(&cg, fn, ast);
    return a64EmitFunction(jit, fn, ast, NULL);
}

/* The AOT text entry, called by asmGenerate once per function with the
 * IR already prepared. The selector needs a HccJit for its local-label
 * numbering and addressing mode; one is kept for the translation unit,
 * in RELOCATED mode and without loading any host library - nothing
 * here ever runs. */
void aarch64MirGenerateFunction(IrCgCtx *ctx, Ast *ast) {
    static HccJit *text_jit = NULL;
    static Cctrl *text_cc = NULL;
    if (!text_jit || text_cc != ctx->cc) {
        text_jit = hccJitNewFor(ctx->cc, aarch64JitBackend(), 0, 1);
        if (!text_jit) loggerPanic("aarch64: cannot create the text emitter\n");
        text_jit->addr_mode = HCC_ADDR_RELOCATED;
        text_cc = ctx->cc;
    }
    ctx->ast_fn = ast;
    if (a64EmitFunction(text_jit, ctx->fn, ast, ctx->buf) != 0) {
        loggerPanic("aarch64: failed to select `%s`\n", ast->fname->data);
    }
}

/* ---------------- backend vtable ---------------- */

static int a64TargetOk(enum CliTarget target) {
    return target == TARGET_AARCH64_APPLE_DARWIN ||
           target == TARGET_AARCH64_UNKNOWN_LINUX_GNU;
}

static const HccJitBackend aarch64_backend = {
    .name = "aarch64",
    .target_ok = a64TargetOk,
    .init_reg_pool = aarch64InitRegPool,
    .compile_function = a64CompileFunction,
};

HccJit *aarch64JitCompile(Cctrl *cc) {
    return hccJitCompile(cc, &aarch64_backend);
}

const HccJitBackend *aarch64JitBackend(void) {
    return &aarch64_backend;
}

/* ---------------- data section (AOT text) ---------------- */

static void aarch64PasteDataSectionDarwin(Cctrl *cc, AoStr *buf) {
    aoStrCatFmt(buf, ".section __TEXT,__cstring,cstring_literals\n");
    MapIter it;
    mapIterInit(cc->strs, &it);
    while (mapIterNext(&it)) {
        MapNode *n = it.node;
        Ast *ast = (Ast *)n->value;
        if (ast->kind != AST_STRING) continue;
        aoStrCatFmt(buf, "%S:\n\t.asciz \"%S\"\n", ast->slabel, ast->sval);
    }
    aoStrPutChar(buf, '\t');
}

static void aarch64PasteDataSectionLinux(Cctrl *cc, AoStr *buf) {
    aoStrCatFmt(buf, ".section .rodata\n");
    MapIter it;
    mapIterInit(cc->strs, &it);
    while (mapIterNext(&it)) {
        MapNode *n = it.node;
        Ast *ast = (Ast *)n->value;
        if (ast->kind != AST_STRING) continue;
        aoStrCatFmt(buf, "%S:\n\t.asciz \"%S\"\n", ast->slabel, ast->sval);
    }
    aoStrPutChar(buf, '\t');
}

void aarch64PasteDataSection(Cctrl *cc, AoStr *buf) {
    if (cc->target == TARGET_AARCH64_APPLE_DARWIN) {
        aarch64PasteDataSectionDarwin(cc, buf);
    } else {
        aarch64PasteDataSectionLinux(cc, buf);
    }
}


void aarch64InitialiseEmptyGlobal(Cctrl *cc, AoStr *buf, Ast *global,
                                  int zerofill)
{
    AoStr *label = global->is_static ? global->glabel : global->gname;
    label = asmNormaliseGlobalLabel(cc, label);
    int size = global->type->size;
    /* Matches x86: zerofill=1 means "real zero-init this TU owns"
     * (.zerofill on Mac, .comm on Linux); zerofill=0 means "tentative
     * common symbol the linker should merge" (always .comm). */
    if (zerofill && cc->target == TARGET_AARCH64_APPLE_DARWIN) {
        aoStrCatFmt(buf, ".globl %S\n\t", label);
        aoStrCatFmt(buf, ".zerofill __DATA,__common,%S,%i,3\n", label, size);
    } else {
        aoStrCatFmt(buf, ".globl %S\n\t.comm %S, %i, 8\n", label, label, size);
    }
}

/* Returns the number of bytes emitted, so a partially-written
 * aggregate can be padded out to its declared size. C zero-initialises
 * every member an initialiser does not mention; emitting only what was
 * written leaves the rest of the object as whatever follows it in the
 * section - for `MapType` that meant reading function pointers out of
 * the text segment and calling them. */
static int aarch64DataInternal(Cctrl *cc, AoStr *buf, Ast *data) {
    /* `&Fn` / a lambda in a static or global aggregate initialiser: a
     * relocation to the symbol, not a number. Falling through to the
     * integer path below emitted whatever happened to be in `i64` -
     * `.quad 8` for a function pointer, which segfaults on the first
     * call through it. */
    if (astIsUnOpKind(data, AST_UN_OP_ADDR_OF) && data->operand) {
        data = data->operand;
    }
    if (data->kind == AST_FUNC || data->kind == AST_FUN_PROTO ||
        data->kind == AST_EXTERN_FUNC)
    {
        char *fn = asmNormaliseFunctionName(cc, data->fname);
        aoStrCatPrintf(buf, ".quad   %s\n\t", fn);
        return 8;
    }
    if (data->kind == AST_ASM_FUNCDEF || data->kind == AST_ASM_FUNC_BIND) {
        aoStrCatFmt(buf, ".quad   %S\n\t", data->asmfname);
        return 8;
    }
    if (data->kind == AST_GVAR) {
        AoStr *g = asmNormaliseGlobalLabel(cc,
                data->is_static ? data->glabel : data->gname);
        aoStrCatFmt(buf, ".quad   %S\n\t", g);
        return 8;
    }
    if (data->kind == AST_STRING) {
        aoStrCatFmt(buf, ".quad   %S\n\t", data->slabel);
        return 8;
    }
    if (data->kind == AST_ARRAY_INIT) {
        int wrote = 0;
        listForEach(data->arrayinit) {
            wrote += aarch64DataInternal(cc, buf, (Ast *)it->value);
        }
        /* Nested aggregates pad too - `{ {1}, 2 }` has to leave the
         * inner struct its full width or every later field shifts. */
        if (data->type && data->type->size > wrote) {
            aoStrCatPrintf(buf, ".space  %d\n\t", data->type->size - wrote);
            wrote = data->type->size;
        }
        return wrote;
    }
    if (data->type->kind == AST_TYPE_FLOAT) {
        /* aoStrCatFmt has no hex specifier; use aoStrCatPrintf so the IEEE
         * bit pattern is emitted as a real hex literal. F32 lands in 4 bytes
         * (ieee754_32), F64 in 8. */
        if (data->type->size == 4) {
            aoStrCatPrintf(buf, ".long   0x%X\n\t",
                           (unsigned)ieee754_32((f32)data->f64));
            return 4;
        } else {
            aoStrCatPrintf(buf, ".quad   0x%llX\n\t",
                           (unsigned long long)ieee754_64(data->f64));
        }
        return 8;
    }
    switch (data->type->size) {
        case 1:  aoStrCatFmt(buf, ".byte   %i\n\t", data->i64); return 1;
        case 2:  aoStrCatFmt(buf, ".short  %i\n\t", data->i64); return 2;
        case 4:  aoStrCatFmt(buf, ".long   %i\n\t", data->i64); return 4;
        default: aoStrCatFmt(buf, ".quad   %I\n\t", data->i64); return 8;
    }
}

void aarch64GlobalVar(Cctrl *cc, Set *seen_globals, AoStr *buf, Ast *ast) {
    Ast *declvar = ast->declvar;
    Ast *declinit = ast->declinit;
    AoStr *varname = declvar->gname;
    AoStr *label = declvar->is_static ? declvar->glabel : declvar->gname;
    /* Mach-O: escape an uppercase-`L` symbol so `.globl` accepts it. Must
     * match the references emitted via asmNormaliseGlobalLabel elsewhere. */
    label = asmNormaliseGlobalLabel(cc, label);

    if (ast->flags & AST_FLAG_EXTERN) return;
    if (declvar->flags & AST_FLAG_EXTERN) return;
    if (setHasLen(seen_globals, varname->data, varname->len)) return;
    aoStrRemovePreviousChar(buf, '\t');
    setAdd(seen_globals, varname->data);

    if (declinit && (declinit->kind == AST_LITERAL ||
                     declinit->kind == AST_STRING ||
                     declinit->kind == AST_ARRAY_INIT))
    {
        if (declinit->kind == AST_STRING) {
            /* `U8 *p = "..."`: the global is a *pointer* to the string
             * literal (emitted separately in the cstring section), not
             * inline storage. `U8 buf[] = "..."` keeps the bytes inline. */
            if (declvar->type && declvar->type->kind == AST_TYPE_POINTER) {
                if (!declvar->is_static) {
                    aoStrCatFmt(buf, ".globl %S\n", label);
                }
                aoStrCatFmt(buf, ".data\n\t.p2align 3\n%S:\n\t.quad %S\n\t",
                            label, declinit->slabel);
            } else {
                aoStrCatFmt(buf, "%S:\n\t.asciz \"%S\"\n\t",
                            label, declinit->sval);
            }
            return;
        }
        if (!declvar->is_static) {
            aoStrCatFmt(buf, ".globl %S\n", label);
        }
        aoStrCatFmt(buf, ".data\n");
        if (declinit->kind == AST_ARRAY_INIT) {
            Ast *head = (Ast *)declinit->arrayinit->next->value;
            if (head->kind == AST_STRING) {
                aoStrCatFmt(buf, ".align 4\n");
            }
        }
        /* An aggregate can hold a pointer, so the whole object needs
         * pointer alignment - the linker warns about an under-aligned
         * atom otherwise, and an unaligned load is a fault on some
         * targets. */
        aoStrCatFmt(buf, ".p2align 3\n");
        aoStrCatFmt(buf, "%S:\n\t", label);
        int wrote = aarch64DataInternal(cc, buf, declinit);
        if (declvar->type && declvar->type->size > wrote) {
            aoStrCatPrintf(buf, ".space  %d\n\t",
                           declvar->type->size - wrote);
        }
    } else {
        if (declvar->is_static) {
            aoStrCatFmt(buf, ".lcomm %S, %i\n\t", label,
                        declvar->type->size);
        } else if (varname->len == 4 &&
                   (!strncmp(varname->data, str_lit("argc")) ||
                    !strncmp(varname->data, str_lit("argv"))))
        {
            /* Test the source name, not `label` - Mach-O has already
             * prefixed that with `_`, so matching on it never fired
             * and every TU emitted its own definition of argc/argv,
             * colliding with libtos's at link time. */
            aarch64InitialiseEmptyGlobal(cc, buf, declvar, 0);
        } else {
            aarch64InitialiseEmptyGlobal(cc, buf, declvar, 1);
        }
    }
}

/* `asm {}` function blocks, encoded to bytes by libtasm. External
 * symbol references can't be expressed as `.byte` runs on arm64 (the
 * relocation lands inside an instruction word), so self-contained
 * functions work and ones that call out report an error. */
void aarch64PasteAsmBlocks(Cctrl *cc, AoStr *buf) {
    if (!cc->asm_blocks) return;
    for (List *bl = cc->asm_blocks->next; bl != cc->asm_blocks;
         bl = bl->next)
    {
        Ast *asm_block = (Ast *)bl->value;
        if (!asm_block->funcs) continue;
        for (List *fl = asm_block->funcs->next; fl != asm_block->funcs;
             fl = fl->next)
        {
            Ast *asm_func = (Ast *)fl->value;
            aoStrCatFmt(buf, ".text\n.globl %s\n%s:\n",
                        asm_func->asmfname->data, asm_func->asmfname->data);
            asmEmitBlockBytes(cc, buf, asm_func->body->asm_stmt, 0);
        }
    }
}
