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
 * Register roles: x11/x12/x17 scratch, d20/d21 FP scratch, x29 fp, x30
 * lr, x8 indirect-result, x9 large-offset pivot / struct address, x10
 * addressing scratch, x16 indirect-call target. */

#include <dlfcn.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <strings.h>

#include "aostr.h"
#include "ast.h"
#include "asm.h"
#include "machine/asm_enc.h"
#include "machine/asm_jit.h"
#include "machine/enc_arm64.h"
#include "machine/mir.h"
#include "aarch64.h"
#include "cli.h"
#include "containers.h"
#include "ir-inliner.h"
#include "ir.h"
#include "ir-optimise.h"
#include "ir-regalloc.h"
#include "list.h"
#include "util.h"
#include <assert.h>
#include "config.h"
#include "ir-debug.h"

/* Per-function emission context. Same shape as IrCgCtx in spirit but
 * carries JIT-specific bits (the encoder, the local-label counter). */
typedef struct {
    HccJit *jit;
    IrFunction *fn;
    Ast *ast;
    IrBlock *cur_block;
    IrBlock *next_block;
    int omit_frame;
    uint32_t aligned_frame; /* what the prologue subtracted from sp */
    /* Outgoing-argument area at [sp, out_area): sized once for the
     * largest call, so no call moves sp. Callee saves sit above it. */
    int out_area;
    /* Which register holds a struct-return destination buffer. A_X9
     * unless the value's own home survives the call; mirrors
     * IrCgCtx.struct_dest in the text backend. */
    int struct_dest;
    int struct_dest_set;
    /* AAPCS64 (Linux) variadic support, mirroring IrCgCtx: set when this
     * function reads a C `va_list`, with the x29-relative offsets of the
     * register save area and the frame-resident va_list record the
     * prologue builds. */
    int uses_va_start;
    int va_save_off;
    int va_list_off;
    /* The function's machine-IR stream: every instruction is built
     * here and pushed through the active sink as it is appended. */
    MirFn *mir;
    /* AOT text sink. NULL = JIT (encode to bytes); non-NULL = format
     * each line as GNU-as text into this buffer instead. Same
     * selection either way - the sink is the only difference. */
    AoStr *text_out;
    /* Flush cursor: lines [0, sunk) have been sunk (encoded/printed).
     * Sinking is deferred so the machine layer's peephole can rewrite
     * a segment before it is committed; an inline `asm {}` block
     * forces a flush since its bytes bypass the stream. */
    int sunk;
    /* Source line for the IR instruction being lowered; stamped onto
     * each built line so the JIT's line notes bind at sink time. */
    int cur_line;
} A64Ctx;

/* The emitter's per-instruction scratch: never a register home, never
 * live across an IR instruction. S0 is the accumulator (r1 lands and
 * results are computed there), S1 the second operand or base, S2 the
 * index or remainder; F0/F1 the FP pair. ABI uses of x0/d0 - return
 * values, arguments - spell A_X0 instead. */
#define A64_S0 A_X11
#define A64_S1 A_X12
#define A64_S2 A_X17
#define A64_F0 ((int)20)
#define A64_F1 ((int)21)

/* ---------------- forward decls ---------------- */

static void a64EmitInstr(A64Ctx *ctx, IrInstr *instr);
static void a64LoadToReg(A64Ctx *ctx, IrValue *val, int reg);
static void a64LoadToFpr(A64Ctx *ctx, IrValue *val, int dreg);
static void a64StoreReg(A64Ctx *ctx, IrValue *dst, int reg);
static void a64StoreFpr(A64Ctx *ctx, IrValue *dst, int dreg);
static void a64PhiMaterialise(A64Ctx *ctx, IrBlock *from, IrBlock *to);

/* ---------------- value-kind helpers ---------------- */

static int a64IsAddImm(s64 v) {
    if (v < 0) return 0;
    if (v <= 0xFFF) return 1;
    if ((v & 0xFFF) == 0 && (v >> 12) <= 0xFFF) return 1;
    return 0;
}

/* Can `[base, #disp]` encode for a `size`-byte access? Unscaled
 * (ldur/stur) covers signed -256..255; the scaled unsigned form
 * covers size-aligned 0..4095*size. */
static int a64IsMemDisp(s32 disp, int size) {
    if (disp >= -256 && disp <= 255) return 1;
    if (disp >= 0 && (disp % size) == 0 && disp <= 4095 * size) return 1;
    return 0;
}

/* Parse a register name like "x9", "w0", "d0", "s3" into an int
 * (which carries just the numeric index 0..31).  Used to translate
 * the pinned-reg / IR_LOC_REG `AoStr *reg` fields into encoder regs. */
static int a64RegFromName(const char *name) {
    if (!name || !*name) return A_X0;
    if (name[0] == 'x' || name[0] == 'X' ||
        name[0] == 'w' || name[0] == 'W' ||
        name[0] == 'd' || name[0] == 'D' ||
        name[0] == 's' || name[0] == 'S')
    {
        int n = atoi(name + 1);
        if (n < 0 || n > 31) return A_X0;
        return (int)n;
    }
    if (!strcasecmp(name, "sp")) return A_SP;
    if (!strcasecmp(name, "lr")) return A_LR;
    if (!strcasecmp(name, "fp")) return A_FP;
    if (!strcasecmp(name, "xzr") || !strcasecmp(name, "wzr")) return A_XZR;
    return A_X0;
}

static int a64IsFpRegName(const char *name) {
    return name && (name[0] == 'd' || name[0] == 's' ||
                    name[0] == 'D' || name[0] == 'S');
}

/* ---------------- MIR emission ----------------
 *
 * Every instruction the helpers and leaves below produce is built as a
 * machine-IR line on ctx->mir and encoded from it in the same breath,
 * so the function accumulates a MIR stream while the byte stream stays
 * identical to what the direct encoder calls emitted. HCC_JIT_MIR=dump
 * prints each function's stream to stderr at the end of its compile. */

static int a64MirDump(void) {
    static int mode = -1;
    if (mode < 0) {
        const char *s = getenv("HCC_JIT_MIR");
        mode = (s && !strcmp(s, "dump")) ? 1 : 0;
    }
    return mode;
}

/* The encoders treat any width that is not 1/2/4 as 64-bit
 * (aarch64_size_for_width defaults to 3), so a 16-byte aggregate slot
 * access was always an 8-byte x-form. The spelling must clamp the same
 * way or a fat pointer's load narrows to w and drops its top half. */
static int a64MirIs64(int size) {
    return size != 1 && size != 2 && size != 4;
}

static AsmOperand a64MirGp(int sf, int num) {
    return sf ? mir_x(num) : mir_w(num);
}
static AsmOperand a64MirFp(int is64, int num) {
    return is64 ? mir_d(num) : mir_s(num);
}
/* FP register view by ACCESS width - the q-form appears in the va-list
 * register save area, which stores whole v registers. */
static AsmOperand a64MirFpSized(int size, int num) {
    return size == 16 ? mir_q(num) : size == 8 ? mir_d(num) : mir_s(num);
}
/* Condition spelling for cset/csel operands and the b.<cc> family. */
static const char *a64MirCcName(A64Cond cc) {
    static const char *const names[] = {
        "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
        "hi", "ls", "ge", "lt", "gt", "le", "al",
    };
    return (cc >= 0 && cc <= A_AL) ? names[cc] : "al";
}
/* Symbolic (RELOCATED) compilation normalises global names for the
 * assembler or object writer; ABSOLUTE mode wants the raw name - it is
 * the host-symbol key.
 *
 * A rename applies in BOTH, because it is not decoration: it says
 * which object this declaration means. Two C names carrying the same
 * `__asm("sym")` are one variable, and in the JIT that has to be one
 * host-symbol key or the second name looks up nothing and the code
 * loads through a null address. */
static const char *a64GlobalName(A64Ctx *ctx, AoStr *name) {
    if (ctx->jit->addr_mode == HCC_ADDR_RELOCATED)
        return asmNormaliseGlobalLabel(ctx->jit->cc, name)->data;
    const char *renamed = asmRenameFor(ctx->jit->cc, name);
    return renamed ? renamed : name->data;
}

/* The backend only BUILDS lines; sinking is deferred to a64MirFlush
 * so the machine layer's peephole sees whole segments first. This hook
 * just stamps the source line for the JIT's line notes. */
static void a64MirLine(A64Ctx *ctx, AsmLine *ln) {
    ln->line = ctx->cur_line;
}

static void E0(A64Ctx *c, int mn) { a64MirLine(c, mir0(c->mir, mn)); }
static void E1(A64Ctx *c, int mn, AsmOperand a) {
    a64MirLine(c, mir1(c->mir, mn, a));
}
static void E2(A64Ctx *c, int mn, AsmOperand a, AsmOperand b) {
    a64MirLine(c, mir2(c->mir, mn, a, b));
}
static void E3(A64Ctx *c, int mn, AsmOperand a, AsmOperand b, AsmOperand d) {
    a64MirLine(c, mir3(c->mir, mn, a, b, d));
}
static void E4(A64Ctx *c, int mn, AsmOperand a, AsmOperand b, AsmOperand d,
               AsmOperand e) {
    a64MirLine(c, mir4(c->mir, mn, a, b, d, e));
}
/* A local label is only a stream line until the flush defines it. */
static void a64MirLocalLabel(A64Ctx *ctx, int local_num) {
    mir_local_label(ctx->mir, local_num);
}

/* The two sinks, each a plain walk over the stream. Text: format every
 * line in the GNU-as dialect (labels unindented). Encode: define
 * labels, bind line notes, encode. */
static void a64MirFlushText(A64Ctx *ctx) {
    AsmBlock *b = ctx->mir->block;
    char line[512];
    for (int i = ctx->sunk; i < b->n_lines; i++) {
        AsmLine *ln = &b->lines[i];
        if (ln->kind != AINS_INSTR && ln->kind != AINS_LABEL_LOCAL &&
            ln->kind != AINS_LABEL_PUBLIC)
        {
            continue;
        }
        asm_line_format_gas(ln, ".Ljag", line, sizeof(line));
        if (ln->kind != AINS_INSTR)
            aoStrRemovePreviousChar(ctx->text_out, '\t');
        aoStrCatFmt(ctx->text_out, "%s\n\t", line);
    }
}

static void a64MirFlushEncode(A64Ctx *ctx) {
    AsmEnc *enc = &ctx->jit->enc;
    AsmBlock *b = ctx->mir->block;
    for (int i = ctx->sunk; i < b->n_lines; i++) {
        AsmLine *ln = &b->lines[i];
        switch (ln->kind) {
        case AINS_LABEL_LOCAL:
            asm_define_label(enc, ln->local_num, NULL);
            break;
        case AINS_LABEL_PUBLIC:
            asm_define_label(enc, -1, ln->label_name);
            break;
        case AINS_INSTR:
            if (ln->line > 0) hccJitNoteLine(ctx->jit, ln->line);
            aarch64_encode_instr(enc, ln);
            break;
        default:
            break;
        }
    }
}

/* Peephole the un-sunk segment, then commit it through the active
 * sink. */
static void a64MirFlush(A64Ctx *ctx) {
    mir_fn_peephole_arm64(ctx->mir, ctx->sunk);
    if (ctx->text_out) a64MirFlushText(ctx);
    else               a64MirFlushEncode(ctx);
    ctx->sunk = ctx->mir->block->n_lines;
}

static int a64MirAluMn(IrOp op) {
    switch (op) {
    case IR_IADD: return A64MN_ADD;
    case IR_ISUB: return A64MN_SUB;
    case IR_AND:  return A64MN_AND;
    case IR_OR:   return A64MN_ORR;
    case IR_XOR:  return A64MN_EOR;
    default:      return TASM_MN_UNKNOWN;
    }
}
static int a64MirShiftMn(IrOp op) {
    return op == IR_SHL ? A64MN_LSL : op == IR_SHR ? A64MN_LSR : A64MN_ASR;
}
static int a64MirFopMn(IrOp op) {
    return op == IR_FMUL ? A64MN_FMUL : op == IR_FDIV ? A64MN_FDIV :
           op == IR_FADD ? A64MN_FADD : A64MN_FSUB;
}

/* sp -=/+= n for any n up to 16MB, via the imm12 (+ lsl #12) split.
 * The register form is a trap here: reg 31 in ADD/SUB
 * (shifted-register) decodes as XZR, not SP, so `sub sp, sp, x9`
 * assembles to a silent no-op. The immediate form treats 31 as SP. */
static void a64SpAdjust(A64Ctx *ctx, int is_sub, u64 n) {
    if (n == 0) return;
    if ((n >> 12) > 0xFFF) {
        loggerPanic("jit-aarch64: stack adjustment %llu exceeds 16MB\n",
                    (unsigned long long)n);
    }
    u64 hi = n & ~0xFFFULL; /* 4096-aligned: encoder picks the sh=1 form */
    u64 lo = n & 0xFFF;
    int mn = is_sub ? A64MN_SUB : A64MN_ADD;
    if (hi) E3(ctx, mn, mir_sp(), mir_sp(), mir_imm((s64)hi));
    if (lo) E3(ctx, mn, mir_sp(), mir_sp(), mir_imm((s64)lo));
}



/* ---------------- immediate materialise ---------------- */

/* Emit `reg = imm` using movz / movk / movn sequences.  Mirrors
 * aarch64EmitMovImm in aarch64.c. */
static void a64MovImm(A64Ctx *ctx, int reg, s64 imm) {
    u64 u = (u64)imm;
    /* Small values are one MOVZ (the mov alias), small negatives one
     * MOVN. */
    if (u <= 0xFFFFULL) {
        E2(ctx, A64MN_MOV, mir_x(reg), mir_imm(imm));
        return;
    }
    if (imm < 0 && imm >= -65536) {
        u64 n = (u64)(~imm) & 0xFFFFULL;
        E2(ctx, A64MN_MOVN, mir_x(reg), mir_imm((s64)n));
        return;
    }
    /* The MOVZ/MOVK sequence is spelled out rather than left to the
     * MOV pseudo: the system assembler will not synthesise it, and the
     * expansion below is instruction-for-instruction what the
     * encoder's mov_imm64 produces - so the stream carries the real
     * instructions for every sink. */
    u64 w0v = u & 0xFFFFULL, w1v = (u >> 16) & 0xFFFFULL;
    u64 w2v = (u >> 32) & 0xFFFFULL, w3v = (u >> 48) & 0xFFFFULL;
    int emitted = 0;
    if (w0v || (!w1v && !w2v && !w3v)) {
        E2(ctx, A64MN_MOVZ, mir_x(reg), mir_imm((s64)w0v));
        emitted = 1;
    }
    u64 hws[3] = { w1v, w2v, w3v };
    for (int k = 0; k < 3; ++k) {
        if (!hws[k]) continue;
        E3(ctx, emitted ? A64MN_MOVK : A64MN_MOVZ, mir_x(reg), mir_imm((s64)hws[k]), mir_shift(ASHIFT_LSL, 16 * (k + 1)));
        emitted = 1;
    }
}

/* Emit `reg += disp` using add/sub imm12 (optionally shifted), or fall
 * back to materialising the offset into x9 and adding the register
 * form. Mirrors aarch64AddSubImm. */
static void a64AddSubImm(A64Ctx *ctx, int reg, s64 disp) {
    if (disp == 0) return;
    int is_sub = disp < 0;
    u64 mag = (u64)(is_sub ? -disp : disp);
    int mn = is_sub ? A64MN_SUB : A64MN_ADD;
    if (mag <= 0xFFF) {
        E3(ctx, mn, mir_x(reg), mir_x(reg), mir_imm((s64)mag));
        return;
    }
    /* Fall back: x9 = disp, then add/sub reg form. */
    a64MovImm(ctx, A_X9, disp);
    E3(ctx, mn, mir_x(reg), mir_x(reg), mir_x(A_X9));
}

/* ---------------- frame load/store ---------------- */

/* Decide which form (unscaled imm9, scaled imm12, or register offset)
 * fits `loff` for an access of `size` bytes. Sets `*use_x9` to 1 when
 * the caller must use the register-offset variant; returns the (signed)
 * displacement to plug into the encoder. */
typedef enum {
    JFRAME_UNSCALED,
    JFRAME_SCALED,
    JFRAME_REGOFF
} JFrameForm;

/* Which scratch register carries the frame offset when it is too big
 * for an immediate.
 *
 * x9 by default, x10 when x9 IS the value being moved. Materialising
 * the offset into the value's own register overwrote the value and
 * then stored the OFFSET in its place:
 *
 *     movn  x9, #335        ; x9 = -336, the frame offset
 *     str   x9, [fp, x9]    ; stores -336, not the pointer
 *
 * It takes a frame over 256 bytes AND a spill of the one register the
 * offset was being built in, which is why nothing hit it until a
 * function passed two >16-byte structs by value - each needs its own
 * 160-byte copy, and the pointer to the first is spilled across the
 * call that produces the second. The value read back was a small
 * negative number used as an address. */
static int a64FrameIdxReg(int value_reg) {
    return value_reg == A_X9 ? A_X10 : A_X9;
}

static JFrameForm a64FrameForm(A64Ctx *ctx, int loff, int size,
                               int idx) {
    if (loff >= -256 && loff <= 255) return JFRAME_UNSCALED;
    if (loff >= 0) {
        int step = (size == 1) ? 1 : (size == 2) ? 2 : (size == 4) ? 4 : 8;
        int max  = step * 4095;
        if ((loff % step) == 0 && loff <= max) return JFRAME_SCALED;
    }
    a64MovImm(ctx, idx, (s64)loff);
    return JFRAME_REGOFF;
}

/* Integer frame load with size-aware sign/zero extension (matches
 * x86 movslq for 4 bytes, movzbq/movzwq for sub-word). */
static void a64FrameLoad(A64Ctx *ctx, int reg, int size, int loff) {
    MirFn *m = ctx->mir;
    int fidx = a64FrameIdxReg(reg);
    JFrameForm form = a64FrameForm(ctx, loff, size, fidx);
    if (form == JFRAME_REGOFF) {
        /* The word load sign-extends like the other two forms. Without
         * the sxtw an `int` slot more than 256 bytes below fp read back
         * zero-extended, so -1 compared as 0xffffffff in 64 bits:
         * `if (chosen < 0)` in irCgAllocAllTmps fell through and the
         * hcc-built hcc stored to slot_last[4294967295]. Only big
         * frames reach this form, which is why the suite never did. */
        int mn = size == 1 ? A64MN_LDRB : size == 2 ? A64MN_LDRH : A64MN_LDR;
        a64MirLine(ctx, mir2(m, mn, a64MirGp(a64MirIs64(size), reg),
                             mir_mem_idx(mir_x(A_FP), mir_x(fidx), 0)));
        if (size == 4)
            a64MirLine(ctx, mir2(m, A64MN_SXTW, mir_x(reg), mir_w(reg)));
        return;
    }
    if (form == JFRAME_UNSCALED) {
        if (size == 4) {
            /* ldursw Xt, [Xn, #simm9] -- not in our encoder; ldur the
             * word then sign-extend it. */
            a64MirLine(ctx, mir2(m, A64MN_LDUR, mir_w(reg),
                                 mir_mem(mir_x(A_FP), loff)));
            a64MirLine(ctx, mir2(m, A64MN_SXTW, mir_x(reg), mir_w(reg)));
            return;
        }
        int mn = size == 1 ? A64MN_LDURB : size == 2 ? A64MN_LDURH
                                         : A64MN_LDUR;
        a64MirLine(ctx, mir2(m, mn, a64MirGp(a64MirIs64(size), reg),
                             mir_mem(mir_x(A_FP), loff)));
        if (size == 1)
            a64MirLine(ctx, mir2(m, A64MN_UXTB, mir_w(reg), mir_w(reg)));
        else if (size == 2)
            a64MirLine(ctx, mir2(m, A64MN_UXTH, mir_w(reg), mir_w(reg)));
        return;
    }
    /* Scaled imm12 form. Encoders take byte offsets and scale internally. */
    switch (size) {
    case 1:
        a64MirLine(ctx, mir2(m, A64MN_LDRB, mir_w(reg),
                             mir_mem(mir_x(A_FP), loff)));
        break;
    case 2:
        a64MirLine(ctx, mir2(m, A64MN_LDRH, mir_w(reg),
                             mir_mem(mir_x(A_FP), loff)));
        break;
    case 4:
        a64MirLine(ctx, mir2(m, A64MN_LDRSW, mir_x(reg),
                             mir_mem(mir_x(A_FP), loff)));
        break;
    default:
        a64MirLine(ctx, mir2(m, A64MN_LDR, mir_x(reg),
                             mir_mem(mir_x(A_FP), loff)));
        break;
    }
}

/* Sign-extending sub-word frame load: the widen folded into the load
 * that had to happen anyway. Mirrors aarch64FrameLoadSx. */
static void a64FrameLoadSx(A64Ctx *ctx, int reg, int size, int loff) {
    a64FrameLoad(ctx, reg, size, loff);
    if (size == 1)
        E2(ctx, A64MN_SXTB, mir_x(reg), mir_w(reg));
    else if (size == 2)
        E2(ctx, A64MN_SXTH, mir_x(reg), mir_w(reg));
}


/* Integer frame store: size-aware width (strb / strh / str). */
static void a64FrameStore(A64Ctx *ctx, int reg, int size, int loff) {
    MirFn *m = ctx->mir;
    int fidx = a64FrameIdxReg(reg);
    JFrameForm form = a64FrameForm(ctx, loff, size, fidx);
    if (form == JFRAME_REGOFF) {
        int mn = size == 1 ? A64MN_STRB : size == 2 ? A64MN_STRH : A64MN_STR;
        a64MirLine(ctx, mir2(m, mn, a64MirGp(a64MirIs64(size), reg),
                             mir_mem_idx(mir_x(A_FP), mir_x(fidx), 0)));
        return;
    }
    if (form == JFRAME_UNSCALED) {
        int mn = size == 1 ? A64MN_STURB : size == 2 ? A64MN_STURH
                                         : A64MN_STUR;
        a64MirLine(ctx, mir2(m, mn, a64MirGp(a64MirIs64(size), reg),
                             mir_mem(mir_x(A_FP), loff)));
        return;
    }
    int mn = size == 1 ? A64MN_STRB : size == 2 ? A64MN_STRH : A64MN_STR;
    a64MirLine(ctx, mir2(m, mn, a64MirGp(a64MirIs64(size), reg),
                         mir_mem(mir_x(A_FP), loff)));
}

/* FP frame load/store via the FP load/store encoder family.
 *
 * AArch64's scaled `LDR Dt, [Xn, #imm]` only accepts non-negative
 * size-aligned offsets, but local slots typically live at negative loff
 * from x29. The encoder doesn't expose an FP-LDUR helper, so for both
 * negative and out-of-range cases we route through `[Xn, Xm]` (regoff)
 * with the offset materialised into x9. `a64FrameForm` emits the movz
 * only on the REGOFF branch; we re-issue it on the UNSCALED branch
 * here so the regoff load below has the offset in x9. */
static void a64FpFrameLoad(A64Ctx *ctx, int dreg, int size, int loff) {
    MirFn *m = ctx->mir;
    /* The value is in a D register, so x9 can never be it. */
    int fidx = A_X9;
    JFrameForm form = a64FrameForm(ctx, loff, size, fidx);
    if (form == JFRAME_REGOFF) {
        a64MirLine(ctx, mir2(m, A64MN_LDR, a64MirFp(size == 8, dreg),
                             mir_mem_idx(mir_x(A_FP), mir_x(fidx), 0)));
        return;
    }
    /* One spelling for both immediate forms: the encoder (and the
     * system assembler) pick the ldur-class encoding for a negative
     * or unaligned offset. The old materialise-into-x9 dance predates
     * the dispatch layer growing that fallback. */
    a64MirLine(ctx, mir2(m, A64MN_LDR, a64MirFp(size == 8, dreg),
                         mir_mem(mir_x(A_FP), loff)));
}

static void a64FpFrameStore(A64Ctx *ctx, int dreg, int size, int loff) {
    MirFn *m = ctx->mir;
    /* The value is in a D register, so x9 can never be it. */
    int fidx = A_X9;
    JFrameForm form = a64FrameForm(ctx, loff, size, fidx);
    if (form == JFRAME_REGOFF) {
        a64MirLine(ctx, mir2(m, A64MN_STR, a64MirFp(size == 8, dreg),
                             mir_mem_idx(mir_x(A_FP), mir_x(fidx), 0)));
        return;
    }
    a64MirLine(ctx, mir2(m, A64MN_STR, a64MirFp(size == 8, dreg),
                         mir_mem(mir_x(A_FP), loff)));
}

/* See aarch64IdxScaled in aarch64.c. The register-offset encoding has
 * a single S bit meaning "scale by the ACCESS width", so it can only
 * express scale == size. `Point pts[3]` indexed by a variable needs
 * base + i*8 for a 4-byte field; asking for S=1 on a 4-byte access
 * scaled by 4 instead, which silently read and wrote the wrong
 * element. Where they disagree, shift the index by hand into x10 and
 * use the unscaled form.
 *
 * Returns the index register to use and clears `*scaled`. */
static int a64IdxScaled(A64Ctx *ctx, int idx, int scale, int size,
                           int *scaled)
{
    int sh = (scale == 8) ? 3 : (scale == 4) ? 2 : (scale == 2) ? 1 : 0;
    if (sh != 0 && scale != size) {
        E3(ctx, A64MN_LSL, mir_x(A_X10), mir_x(idx), mir_imm(sh));
        *scaled = 0;
        return A_X10;
    }
    *scaled = (sh != 0);
    return idx;
}


/* Regoff spelling helpers: the natural `lsl #log2(size)` when the S
 * bit is set, plain `[base, idx]` when it is not. */
static AsmOperand a64MirRegoff(int base, int idx, int scaled,
                               int size) {
    int sh = scaled ? (size == 8 ? 3 : size == 4 ? 2 : size == 2 ? 1 : 0)
                    : 0;
    return mir_mem_idx(mir_x(base), mir_x(idx), sh);
}
/* The load mnemonic per access width; `ldrsw` is spelled explicitly by
 * the callers that want the sign-extending form. */
static int a64MirLdMn(int size) {
    return size == 1 ? A64MN_LDRB : size == 2 ? A64MN_LDRH : A64MN_LDR;
}
static int a64MirStMn(int size) {
    return size == 1 ? A64MN_STRB : size == 2 ? A64MN_STRH : A64MN_STR;
}
static int a64MirLdurMn(int size) {
    return size == 1 ? A64MN_LDURB : size == 2 ? A64MN_LDURH : A64MN_LDUR;
}
/* The zero/sign-extension a sub-word or word load is followed by. */
static void a64MirLoadExt(A64Ctx *ctx, int size, int dst) {
    if (size == 4)
        E2(ctx, A64MN_SXTW, mir_x(dst), mir_w(dst));
    else if (size == 1)
        E2(ctx, A64MN_UXTB, mir_w(dst), mir_w(dst));
    else if (size == 2)
        E2(ctx, A64MN_UXTH, mir_w(dst), mir_w(dst));
}

/* Width-aware load through an address held in `base_reg`. Mirrors
 * aarch64DerefLoad: handles disp + idx + scale fusion. */
static void a64DerefLoad(A64Ctx *ctx, int size, int dst, int base,
                         int has_idx, int idx, int scale, s32 disp)
{
    MirFn *m = ctx->mir;
    if (has_idx) {
    /* `base + disp` into a scratch, NEVER into `base` itself.
     *
     * This used to do `a64AddSubImm(ctx, base, disp)`. Two things go
     * wrong with that. IR_RMW_DEREF calls the load and then the store
     * with the SAME base register, so the displacement lands twice -
     * `ctx->misses[i]++` wrote at `misses + 208 + i*8` instead of
     * `misses + i*8`, past the end of the struct. And when `base` is a
     * variable's HOME register rather than a scratch, adding to it
     * moves the variable permanently.
     *
     * x16 because everything nearer is spoken for: S0 is the value,
     * x9 the RHS, x10 is where a64IdxScaled puts a scaled index, and
     * S1/S2 can be holding base and idx already. x16 is the ABI's
     * call-scratch and nothing between these two instructions uses
     * it. The non-indexed path below already refused to mutate base
     * for the same reason. */
        if (disp) {
            a64MirLine(ctx, mir2(m, A64MN_MOV, mir_x(A_X16), mir_x(base)));
            a64AddSubImm(ctx, A_X16, (s64)disp);
            base = A_X16;
        }
        int scaled;
        idx = a64IdxScaled(ctx, idx, scale, size, &scaled);
        /* ldrsw via regoff: encoder doesn't expose the sign-extending
         * variant; load plain word then sxtw. */
        a64MirLine(ctx, mir2(m, a64MirLdMn(size), a64MirGp(a64MirIs64(size), dst),
                             a64MirRegoff(base, idx, scaled, size)));
        a64MirLoadExt(ctx, size, dst);
        return;
    }
    if (disp != 0) {
        int ok_scaled = disp >= 0 && (disp % size) == 0 &&
                        disp <= 4095 * size;
        if (!ok_scaled && !a64IsMemDisp(disp, size)) {
            /* Out of every immediate form's range: register-offset
             * via x10. base may be a live allocated register (must
             * not be mutated) and x9 can hold a store value. */
            a64MovImm(ctx, A_X10, (s64)disp);
            a64MirLine(ctx, mir2(m, a64MirLdMn(size),
                                 a64MirGp(a64MirIs64(size), dst),
                                 a64MirRegoff(base, A_X10, 0, size)));
            a64MirLoadExt(ctx, size, dst);
            return;
        }
        if (!ok_scaled) {
            /* Small negative or misaligned: ldur (imm9 unscaled). */
            a64MirLine(ctx, mir2(m, a64MirLdurMn(size),
                                 a64MirGp(a64MirIs64(size), dst),
                                 mir_mem(mir_x(base), disp)));
            a64MirLoadExt(ctx, size, dst);
            return;
        }
    }
    /* Scaled imm12 (disp may be 0): the word load takes the
     * sign-extending LDRSW form directly. */
    int mn = (size == 4) ? A64MN_LDRSW : a64MirLdMn(size);
    a64MirLine(ctx, mir2(m, mn, a64MirGp(size >= 4, dst),
                         mir_mem(mir_x(base), disp)));
}

static void a64DerefStore(A64Ctx *ctx, int size, int val, int base,
                          int has_idx, int idx, int scale, s32 disp)
{
    MirFn *m = ctx->mir;
    if (has_idx) {
        /* See a64DerefLoad: the effective address goes in x16, and
         * `base` is left alone. */
        if (disp) {
            a64MirLine(ctx, mir2(m, A64MN_MOV, mir_x(A_X16), mir_x(base)));
            a64AddSubImm(ctx, A_X16, (s64)disp);
            base = A_X16;
        }
        int scaled;
        idx = a64IdxScaled(ctx, idx, scale, size, &scaled);
        a64MirLine(ctx, mir2(m, a64MirStMn(size), a64MirGp(a64MirIs64(size), val),
                             a64MirRegoff(base, idx, scaled, size)));
        return;
    }
    if (disp != 0) {
        int ok_scaled = disp >= 0 && (disp % size) == 0 &&
                        disp <= 4095 * size;
        if (!ok_scaled && !a64IsMemDisp(disp, size)) {
            /* See a64DerefLoad: x10, never mutate base, x9 may be
             * the value. */
            a64MovImm(ctx, A_X10, (s64)disp);
            a64MirLine(ctx, mir2(m, a64MirStMn(size),
                                 a64MirGp(a64MirIs64(size), val),
                                 a64MirRegoff(base, A_X10, 0, size)));
            return;
        }
        if (!ok_scaled) {
            int mn = size == 1 ? A64MN_STURB : size == 2 ? A64MN_STURH
                                             : A64MN_STUR;
            a64MirLine(ctx, mir2(m, mn, a64MirGp(a64MirIs64(size), val),
                                 mir_mem(mir_x(base), disp)));
            return;
        }
    }
    a64MirLine(ctx, mir2(m, a64MirStMn(size), a64MirGp(a64MirIs64(size), val),
                         mir_mem(mir_x(base), disp)));
}

/* ---------------- global addressing ----------------
 *
 * Three kinds of symbol need different treatment:
 *
 *  - Internal functions (defined in this TU): their address is the JIT
 *    mapping base + the label's byte_offset, but we don't know the base
 *    at emit time. Emit ADRP + ADD with PAGE21/PAGEOFF12 fixups; the
 *    finalize step looks up the label in enc->labels by name and
 *    patches the 21-bit page delta + 12-bit page offset (same scheme
 *    the AOT path uses for @PAGE/@PAGEOFF on Darwin).
 *
 *  - Internal globals/strings (registered in host_symbols by
 *    a64AllocateGlobals): we know the absolute address NOW because
 *    they live in a host malloc'd arena. Emit mov_imm64 with the
 *    materialised address.
 *
 *  - External functions (printf/MAlloc/etc., dlsym-discoverable):
 *    materialise via mov_imm64 with the dlsym address. Don't use
 *    ADRP+ADD here -- libc is typically beyond ADRP's +/-4GB reach
 *    on macOS. */

static void a64GlobalAddrEx(A64Ctx *ctx, int reg, const char *sym,
                            int force_got) {
    HccJit *jit = ctx->jit;
    if (jit->addr_mode == HCC_ADDR_RELOCATED) {
        /* Everything is symbolic. A symbol from another image cannot
         * be reached by adrp+add - the linker has no address to fold
         * in - so it loads from the GOT. Under `-fPIC` an exported
         * symbol defined HERE is in the same position: it may be
         * interposed at load time, so `ld` refuses the adrp+add too
         * (see asmSymbolIsPreemptible). Mirrors the old
         * aarch64GlobalAddrEx, with the relocation decoration baked
         * into the operand text (the printer passes syms verbatim). */
        Cctrl *cc = jit->cc;
        int is_extern = force_got ||
                        (cc->extern_globals &&
                         setHas(cc->extern_globals, (char *)sym)) ||
                        asmSymbolIsPreemptible(cc, sym);
        int darwin = (cc->target == TARGET_AARCH64_APPLE_DARWIN);
        char hi[512], lo[512];
        if (is_extern) {
            snprintf(hi, sizeof(hi), darwin ? "%s@GOTPAGE" : ":got:%s", sym);
            snprintf(lo, sizeof(lo), darwin ? "%s@GOTPAGEOFF" : ":got_lo12:%s",
                     sym);
            E2(ctx, A64MN_ADRP, mir_x(reg), mir_sym(hi));
            AsmOperand m = mir_mem(mir_x(reg), 0);
            m.label_name = lo;   /* borrowed; deep-copied on emit */
            E2(ctx, A64MN_LDR, mir_x(reg), m);
        } else {
            snprintf(hi, sizeof(hi), darwin ? "%s@PAGE" : "%s", sym);
            snprintf(lo, sizeof(lo), darwin ? "%s@PAGEOFF" : ":lo12:%s", sym);
            E2(ctx, A64MN_ADRP, mir_x(reg), mir_sym(hi));
            E3(ctx, A64MN_ADD, mir_x(reg), mir_x(reg), mir_sym(lo));
        }
        return;
    }
    /* Internal function -> ADRP + ADD with PAGE21/PAGEOFF12 fixups; the
     * asm dispatch records the same fixups the direct calls did. */
    if (hccJitIsInternalFunc(jit, sym)) {
        E2(ctx, A64MN_ADRP, mir_x(reg), mir_sym(sym));
        E3(ctx, A64MN_ADD, mir_x(reg), mir_x(reg), mir_sym(sym));
        return;
    }
    /* Internal global / string / external function: address is known
     * now (host arena malloc, or dlsym for libc). Materialise it. */
    void *addr = mapGet(jit->host_symbols, (void *)sym);
    if (!addr) addr = asm_jit_dlsym_resolver(NULL, sym);
    E2(ctx, A64MN_MOV, mir_x(reg), mir_imm((s64)(uintptr_t)addr));
}

static void a64GlobalAddr(A64Ctx *ctx, int reg, const char *sym) {
    a64GlobalAddrEx(ctx, reg, sym, 0);
}

/* ---------------- load/store IR values ---------------- */

static void a64LoadToReg(A64Ctx *ctx, IrValue *val, int reg) {
    switch (val->kind) {
        case IR_VAL_CONST_INT:
            if (val->as._i64 == 0) {
                E2(ctx, A64MN_MOV, mir_x(reg), mir_x(A_XZR));
            } else {
                a64MovImm(ctx, reg, val->as._i64);
            }
            break;
        case IR_VAL_CONST_FLOAT: {
            /* Rare bit-cast: materialise the bit pattern. */
            a64MovImm(ctx, reg, (s64)(u64)ieee754_64(val->as._f64));
            break;
        }
        case IR_VAL_CONST_STR:
            a64GlobalAddr(ctx, reg, val->as.str.label->data);
            break;
        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            if (val->pinned_reg) {
                int src = a64RegFromName(val->pinned_reg->data);
                if (src != reg)
                    E2(ctx, A64MN_MOV, mir_x(reg), mir_x(src));
                break;
            }
            if (val->loc.kind == IR_LOC_REG && val->loc.as.reg) {
                int src = a64RegFromName(val->loc.as.reg->data);
                if (src != reg)
                    E2(ctx, A64MN_MOV, mir_x(reg), mir_x(src));
                break;
            }
            int size = (int)irSpillWidth(val);
            int loff = irCgGetLoff(&ctx->fn->ra, val);
            /* See aarch64LoadToReg - a sub-word load zero-extends, so a
             * signed narrow local must use the sign-extending form. */
            if ((val->kind == IR_VAL_LOCAL || val->kind == IR_VAL_PARAM) &&
                (size == 1 || size == 2) && irIsInt(val->type) &&
                !(val->flags & IR_VAL_FLAG_UNSIGNED))
            {
                a64FrameLoadSx(ctx, reg, size, loff);
                break;
            }
            a64FrameLoad(ctx, reg, size, loff);
            break;
        }
        case IR_VAL_GLOBAL:
            a64GlobalAddr(ctx, reg,
                          a64GlobalName(ctx, val->as.global.name));
            break;
        default:
            loggerPanic("jit-aarch64: cannot load int value of kind %d\n", val->kind);
    }
}

/* 1 for a double-width (F64) float value, 0 for single (F32). */
static int a64FpDbl(IrValue *v) { return (int)irValueByteSize(v) == 8; }

static void a64LoadToFpr(A64Ctx *ctx, IrValue *val, int dreg) {
    MirFn *m = ctx->mir;
    switch (val->kind) {
        case IR_VAL_CONST_FLOAT:
        case IR_VAL_CONST_INT: {
            double f = val->kind == IR_VAL_CONST_INT
                       ? (double)val->as._i64
                       : val->as._f64;
            int is_dbl = (val->kind == IR_VAL_CONST_FLOAT) ? a64FpDbl(val) : 1;
            u64 bits = is_dbl ? (u64)ieee754_64(f)
                              : (u64)(u32)ieee754_32((f32)f);
            if (bits == 0) {
                a64MirLine(ctx, mir2(m, A64MN_FMOV, a64MirFp(is_dbl, dreg),
                                     a64MirGp(is_dbl, A_XZR)));
                break;
            }
            /* Materialise via x9 = bit pattern, then fmov to dreg. */
            a64MovImm(ctx, A_X9, (s64)bits);
            a64MirLine(ctx, mir2(m, A64MN_FMOV, a64MirFp(is_dbl, dreg),
                                 a64MirGp(is_dbl, A_X9)));
            break;
        }
        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            int is_dbl = a64FpDbl(val);
            /* TempleOS-pinned FP local: the value lives in the named
             * register (d/s for FP, x for a GPR holding the bits). */
            AoStr *home = val->pinned_reg
                        ? val->pinned_reg
                        : (val->loc.kind == IR_LOC_REG ? val->loc.as.reg
                                                       : NULL);
            if (home) {
                int src = a64RegFromName(home->data);
                if (a64IsFpRegName(home->data)) {
                    if (src != dreg)
                        a64MirLine(ctx, mir2(m, A64MN_FMOV,
                                             a64MirFp(is_dbl, dreg),
                                             a64MirFp(is_dbl, src)));
                } else {
                    a64MirLine(ctx, mir2(m, A64MN_FMOV,
                                         a64MirFp(is_dbl, dreg),
                                         a64MirGp(is_dbl, src)));
                }
                break;
            }
            int size = (int)irValueByteSize(val);
            int loff = irCgGetLoff(&ctx->fn->ra, val);
            a64FpFrameLoad(ctx, dreg, size, loff);
            break;
        }
        default:
            loggerPanic("jit-aarch64: cannot load fp value of kind %d\n", val->kind);
    }
}

/* Mirrors aarch64.c's aarch64StoreNarrowHome - see the reasoning there.
 * The two must agree or AOT and JIT differ in the upper half. */
static int a64StoreNarrowHome(A64Ctx *ctx, IrValue *dst, int home,
                              int reg)
{
    if (dst->kind != IR_VAL_LOCAL && dst->kind != IR_VAL_PARAM) return 0;
    u32 sz = irValueByteSize(dst);
    if (sz >= 8 || !irIsInt(dst->type)) return 0;
    int uns = (dst->flags & IR_VAL_FLAG_UNSIGNED) != 0;
    MirFn *m = ctx->mir;
    if (sz == 1) {
        if (uns) a64MirLine(ctx, mir2(m, A64MN_UXTB, mir_w(home),
                                      mir_w(reg)));
        else     a64MirLine(ctx, mir2(m, A64MN_SXTB, mir_x(home),
                                      mir_w(reg)));
    } else if (sz == 2) {
        if (uns) a64MirLine(ctx, mir2(m, A64MN_UXTH, mir_w(home),
                                      mir_w(reg)));
        else     a64MirLine(ctx, mir2(m, A64MN_SXTH, mir_x(home),
                                      mir_w(reg)));
    } else if (sz == 4) {
        if (uns) a64MirLine(ctx, mir2(m, A64MN_MOV, mir_w(home),
                                      mir_w(reg)));
        else     a64MirLine(ctx, mir2(m, A64MN_SXTW, mir_x(home),
                                      mir_w(reg)));
    } else return 0;
    return 1;
}

static void a64StoreReg(A64Ctx *ctx, IrValue *dst, int reg) {
    AoStr *hname = dst->pinned_reg
                 ? dst->pinned_reg
                 : (dst->loc.kind == IR_LOC_REG ? dst->loc.as.reg : NULL);
    if (hname) {
        int home = a64RegFromName(hname->data);
        if (a64StoreNarrowHome(ctx, dst, home, reg)) return;
        if (home != reg)
            E2(ctx, A64MN_MOV, mir_x(home), mir_x(reg));
        return;
    }
    int size = (int)irSpillWidth(dst);
    int loff = irCgGetLoff(&ctx->fn->ra, dst);
    a64FrameStore(ctx, reg, size, loff);
}

/* `val_size` (0 = derive from dst) is the width of the value being stored.
 * When dst is a pointer/gep tmp aliasing a narrow field, sizing the store
 * from dst (8 bytes) would clobber the neighbouring field; pass the value
 * width instead. Mirrors aarch64.c's aarch64StoreFprSized. */
static void a64StoreFprSized(A64Ctx *ctx, IrValue *dst, int dreg,
                             int val_size) {
    int size = val_size > 0 ? val_size : (int)irValueByteSize(dst);
    int is_dbl = (size == 8);
    /* Pinned FP local: write the value into the named register. */
    AoStr *hname = dst->pinned_reg
                 ? dst->pinned_reg
                 : (dst->loc.kind == IR_LOC_REG ? dst->loc.as.reg : NULL);
    if (hname) {
        int home = a64RegFromName(hname->data);
        if (a64IsFpRegName(hname->data)) {
            if (home != dreg)
                E2(ctx, A64MN_FMOV, a64MirFp(is_dbl, home), a64MirFp(is_dbl, dreg));
        } else {
            E2(ctx, A64MN_FMOV, a64MirGp(is_dbl, home), a64MirFp(is_dbl, dreg));
        }
        return;
    }
    int loff = irCgGetLoff(&ctx->fn->ra, dst);
    a64FpFrameStore(ctx, dreg, size, loff);
}

static void a64StoreFpr(A64Ctx *ctx, IrValue *dst, int dreg) {
    a64StoreFprSized(ctx, dst, dreg, 0);
}

/* Mirrors aarch64.c's aarch64DstStoreIsDead: skip a home store for a
 * result nothing reads. Register homes only - see IRCG_DST_DEAD. */
static int a64DstStoreIsDead(IrInstr *instr, IrValue *dst) {
    if (!instr || !(instr->flags & IRCG_DST_DEAD)) return 0;
    if (instr->dst != dst) return 0;
    return dst->pinned_reg ||
           (dst->loc.kind == IR_LOC_REG && dst->loc.as.reg);
}

static void a64SpillDst(A64Ctx *ctx, IrInstr *instr, int reg) {
    if (a64DstStoreIsDead(instr, instr->dst)) return;
    a64StoreReg(ctx, instr->dst, reg);
}

static void a64SpillDstFpr(A64Ctx *ctx, IrInstr *instr, int dreg) {
    if (a64DstStoreIsDead(instr, instr->dst)) return;
    a64StoreFpr(ctx, instr->dst, dreg);
}

static void a64LoadFirstSrc(A64Ctx *ctx, IrValue *src) {
    a64LoadToReg(ctx, src, A64_S0);
}

static void a64LoadFirstSrcFpr(A64Ctx *ctx, IrValue *src) {
    a64LoadToFpr(ctx, src, A64_F0);
}

/* The register a value lives in, or NULL when it lives in memory.
 * Mirrors aarch64.c's a64HomeReg - the two backends must agree on what
 * counts as "already in a register" or they emit different programs. */
static AoStr *a64Home(IrValue *v) {
    if (!v) return NULL;
    if (v->pinned_reg) return v->pinned_reg;
    if (v->loc.kind == IR_LOC_REG && v->loc.as.reg) return v->loc.as.reg;
    return NULL;
}

/* Mirrors aarch64.c's a64ScratchTake / a64BaseHome / aarch64IdxHome.
 * The IR-side fusion guard was relaxed for BOTH backends, so the JIT
 * has to make the same guarantee: a base scratch that avoids the
 * index, and no addressing off a home the emitter would mutate. */
static int a64ScratchTake(int avoid_valid, int avoid) {
    if (!avoid_valid || avoid != A64_S1) return A64_S1;
    return A64_S2;
}

/* Where the index already lives, or 0 if it must be materialised.
 * Emits nothing - the base scratch is chosen from this. */
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

/* idx register for addressing-mode fusion. Returns 1 if there is an
 * idx component, 0 otherwise. `*out` is always written so callers can
 * forward it unconditionally. */
static int a64IdxReg(A64Ctx *ctx, IrInstr *instr, int base,
                     int *out) {
    *out = A64_S2;
    if (!instr->idx || !instr->scale) return 0;
    if (a64IdxHome(instr, out)) return 1;
    int scratch = (base == A64_S2) ? A64_S1 : A64_S2;
    a64LoadToReg(ctx, instr->idx, scratch);
    *out = scratch;
    return 1;
}

/* ---------------- condition codes ---------------- */

static A64Cond a64CcFor(IrCmpKind cmp, int is_float) {
    if (is_float) {
        switch (cmp) {
            case IR_CMP_EQ: return A_EQ;
            case IR_CMP_NE: return A_NE;
            case IR_CMP_LT: return A_MI;
            case IR_CMP_LE: return A_LS;
            case IR_CMP_GT: return A_GT;
            case IR_CMP_GE: return A_GE;
            default: loggerPanic("jit-aarch64: bad float cmp %d\n", cmp);
        }
    }
    switch (cmp) {
        case IR_CMP_EQ:  return A_EQ;
        case IR_CMP_NE:  return A_NE;
        case IR_CMP_LT:  return A_LT;
        case IR_CMP_LE:  return A_LE;
        case IR_CMP_GT:  return A_GT;
        case IR_CMP_GE:  return A_GE;
        case IR_CMP_ULT: return A_CC;  /* lo */
        case IR_CMP_ULE: return A_LS;
        case IR_CMP_UGT: return A_HI;
        case IR_CMP_UGE: return A_CS;  /* hs */
        default: loggerPanic("jit-aarch64: bad int cmp %d\n", cmp);
    }
}

static A64Cond a64CcInvFor(IrCmpKind cmp, int is_float) {
    if (is_float) {
        switch (cmp) {
            case IR_CMP_EQ: return A_NE;
            case IR_CMP_NE: return A_EQ;
            case IR_CMP_LT: return A_PL;
            case IR_CMP_LE: return A_HI;
            case IR_CMP_GT: return A_LE;
            case IR_CMP_GE: return A_LT;
            default: loggerPanic("jit-aarch64: bad float cmp %d\n", cmp);
        }
    }
    switch (cmp) {
        case IR_CMP_EQ:  return A_NE;
        case IR_CMP_NE:  return A_EQ;
        case IR_CMP_LT:  return A_GE;
        case IR_CMP_LE:  return A_GT;
        case IR_CMP_GT:  return A_LE;
        case IR_CMP_GE:  return A_LT;
        case IR_CMP_ULT: return A_CS;
        case IR_CMP_ULE: return A_HI;
        case IR_CMP_UGT: return A_LS;
        case IR_CMP_UGE: return A_CC;
        default: loggerPanic("jit-aarch64: bad int cmp %d\n", cmp);
    }
}

/* Where a result should land: the destination's home register when it
 * has one of the right class, else the scratch. Loading straight into
 * the home makes the a64SpillDst afterwards a no-op (it keeps running
 * for the slot-resident and narrow-canonicalisation cases). Safe even
 * against source operands sharing the register: the allocator only
 * gives two values one home when their ranges do not overlap, i.e.
 * the source died at this instruction - the same invariant the alu3
 * forms already lean on. */
static int a64DstOut(IrInstr *instr, int want_fp, int fallback) {
    AoStr *home = a64Home(instr->dst);
    if (!home) return fallback;
    if (a64IsFpRegName(home->data) != want_fp) return fallback;
    return a64RegFromName(home->data);
}

static void a64EmitCSet(A64Ctx *ctx, IrCmpKind cmp, int is_float,
                        int out) {
    E2(ctx, A64MN_CSET, mir_x(out), mir_sym(a64MirCcName(a64CcFor(cmp, is_float))));
}

/* ---------------- phi materialisation ---------------- */

static void a64EmitOnePhi(A64Ctx *ctx, IrInstr *phi, IrPair *match) {
    IrValue *v = match ? match->ir_value : NULL;
    int v_dangling = irIsTmp(v) &&
                     v->loc.kind != IR_LOC_REG &&
                     !mapHasInt(ctx->fn->ra.id_to_loff, irVarId(v));
    /* Materialise straight into the phi's home when it has one - the
     * store then self-elides. Only slot-resident phis still route
     * through the scratch. */
    AoStr *h = a64Home(phi->dst);
    if (irIsFloat(phi->dst->type)) {
        int is_dbl = a64FpDbl(phi->dst);
        int out = (h && a64IsFpRegName(h->data))
                   ? a64RegFromName(h->data) : A64_F0;
        if (v_dangling) {
            E2(ctx, A64MN_FMOV, a64MirFp(is_dbl, out), a64MirGp(is_dbl, A_XZR));
        } else {
            a64LoadToFpr(ctx, v, out);
        }
        a64StoreFpr(ctx, phi->dst, out);
        return;
    }
    int out = (h && !a64IsFpRegName(h->data))
               ? a64RegFromName(h->data) : A64_S0;
    if (v_dangling) {
        E2(ctx, A64MN_MOV, mir_x(out), mir_x(A_XZR));
    } else {
        a64LoadToReg(ctx, v, out);
    }
    a64StoreReg(ctx, phi->dst, out);
}

static void a64EmitOnePhiCb(void *ud, IrInstr *phi, IrPair *match) {
    a64EmitOnePhi((A64Ctx *)ud, phi, match);
}

static void a64PhiMaterialise(A64Ctx *ctx, IrBlock *from, IrBlock *to) {
    hccJitPhiMaterialise(from, to, a64EmitOnePhiCb, ctx);
}

/* ---------------- call args partition ---------------- */

static s32 a64PartitionCallArgs(u8 *is_stack, u64 n, Vec *args,
                                int is_variadic, int named_count,
                                int force_va_stack)
{
    int int_used = 0, float_used = 0, stack_count = 0;
    for (u64 i = 0; i < n; ++i) {
        IrValue *a = vecGet(IrValue *, args, i);
        int is_float = irIsFloat(a->type);
        int force_stack = 0;
        if (is_variadic && force_va_stack && (int)i >= named_count) force_stack = 1;
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

/* The callee's calling convention. Both call paths need it: HolyC
 * varargs and Apple's C varargs put the tail on the stack whichever
 * path emits the call. */
static void a64CallConv(Cctrl *cc, IrInstr *instr, int *callee_va_out,
                        int *named_count_out, int *force_va_stack_out)
{
    IrValue *wrap = instr->r1;
    AoStr *fname = wrap ? wrap->as.array.label : NULL;
    Ast *callee = NULL;
    if (fname) {
        callee = (Ast *)mapGetLen(cc->global_env,
                                  fname->data, fname->len);
    } else if (wrap) {
        /* Indirect: what the function pointer's type says. */
        callee = wrap->as.array.callee_decl;
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
    int apple_target = cc->target == TARGET_AARCH64_APPLE_DARWIN;
    int holyc_variadic = callee_va && astUsesHolyCVarargs(callee);
    int extern_variadic = callee_va && !holyc_variadic;
    *callee_va_out = callee_va;
    *named_count_out = named_count;
    *force_va_stack_out = holyc_variadic || (extern_variadic && apple_target);
}

/* By-value struct args or return go through the uniform AAPCS path. */
static int a64CallUsesAapcs(IrInstr *instr) {
    if (instr->flags & IRCG_CALL_AGG_RETURN) return 1;
    Vec *args = instr->r1 ? instr->r1->as.array.values : NULL;
    if (!args) return 0;
    for (u64 i = 0; i < args->size; ++i) {
        if (vecGet(IrValue *, args, i)->byval_struct_type) return 1;
    }
    return 0;
}

/* IrRegPool.call_arg_reg: the plain path's register for argument `i`,
 * mirroring IR_CALL's staging loop. */
static AoStr *a64CallArgReg(Cctrl *cc, IrInstr *call, u64 i) {
    if (!cc || a64CallUsesAapcs(call)) return NULL;
    Vec *args = call->r1 ? call->r1->as.array.values : NULL;
    if (!args || i >= args->size) return NULL;
    int callee_va, named_count, force_va_stack;
    a64CallConv(cc, call, &callee_va, &named_count, &force_va_stack);
    u8 is_stack[64];
    if (args->size > sizeof(is_stack)) return NULL;
    a64PartitionCallArgs(is_stack, args->size, args, callee_va,
                         named_count, force_va_stack);
    if (is_stack[i]) return NULL;
    int want_float = !!irIsFloat(vecGet(IrValue *, args, i)->type);
    int idx = 0;
    for (u64 k = 0; k < i; ++k) {
        if (is_stack[k]) continue;
        if (!!irIsFloat(vecGet(IrValue *, args, k)->type) == want_float) idx++;
    }
    IrRegPool *pool = irRegPoolGet();
    Vec *regs = want_float ? pool->float_arg_regs : pool->int_arg_regs;
    return (AoStr *)regs->entries[idx];
}

/* ---------------- branches / IR_RET ---------------- */

/* `b @@N` - the dispatch emits the same placeholder word and a
 * reloc-tagged AF_LOCAL fixup the JIT finalize resolves. */
static void a64EmitBranchLocal(A64Ctx *ctx, int local_num) {
    E1(ctx, A64MN_B, mir_label_local(local_num));
}

/* `b.<cc> @@N`. */
static void a64EmitBcondLocal(A64Ctx *ctx, A64Cond cc, int local_num) {
    a64MirLine(ctx, mir_bcond(ctx->mir, a64MirCcName(cc), local_num));
}

/* ---------------- AAPCS struct-by-value passing ----------------
 *
 * Mirrors aarch64.c's aarch64EmitAapcsCall / aarch64EmitStructParamPrologue
 * for the JIT (machine-code) path. Same classification (HFA -> v-regs,
 * INTEGER <=16B -> GP regs, INDIRECT >16B -> caller copy + pointer) and
 * the same register-only support window (stack spill panics, matching the
 * text backend). */

static int a64IsByvalStruct(AstType *t) {
    return t && (t->kind == AST_TYPE_CLASS || t->kind == AST_TYPE_UNION) &&
           !t->is_intrinsic;
}

/* Where this function's NAMED parameters leave off in each of the three
 * places a variadic argument can continue from: the GP registers
 * (x0..x7), the FP/SIMD registers (q0..q7), and the incoming stack area.
 * The variadic list picks up exactly where they stop, so both `va_start`
 * and the register-save prologue read their starting points from here.
 * Mirrors a64EmitParamPrologue's own walk - any disagreement between the
 * two reads arguments that are not there. */
static void a64VaNamedCounts(Ast *ast, int *out_ngrn,
                             int *out_nsrn, int *out_nsaa) {
    int ngrn = 0, nsrn = 0, nsaa = 0;
    if (ast && ast->params) {
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
            } else { /* AAPCS_INDIRECT: a pointer, in a register or not */
                if (ngrn >= 8) nsaa += 8; else ngrn += 1;
            }
        }
    }
    *out_ngrn = ngrn;
    *out_nsrn = nsrn;
    *out_nsaa = nsaa;
}

/* Where this function's VARIADIC arguments begin, as an offset from
 * x29. Apple AArch64 passes every variadic argument on the stack, in
 * the incoming argument area that starts at [x29, #16]; named
 * parameters that overflowed the registers were placed first and have
 * to be stepped over. */
static int a64VaRegionOffset(Ast *ast) {
    int ngrn, nsrn, nsaa;
    a64VaNamedCounts(ast, &ngrn, &nsrn, &nsaa);
    return 16 + nsaa;
}

/* AAPCS64 (Linux) register save area, spilled by the prologue of a
 * variadic function that reads a C `va_list`: eight 8-byte GP registers
 * (x0..x7) then eight 16-byte FP/SIMD registers (q0..q7). The
 * frame-resident va_list record the header copies from sits just below
 * it. Apple AArch64 passes varargs on the stack and needs neither. */
#define A64_VA_SAVE_BYTES 192   /* 8 GP * 8  +  8 FP * 16            */
#define A64_VA_LIST_BYTES 32    /* __stack,__gr_top,__vr_top + 2 offs */

/* Bytes a va_start function reserves under its locals. 16-aligned so the
 * q-register stores land on a 16-byte boundary and sp stays aligned. */
static unsigned a64VaExtraBytes(void) {
    return (A64_VA_SAVE_BYTES + A64_VA_LIST_BYTES + 15u) & ~15u;
}

/* Does this function actually read a C variadic list? Asking the IR, not
 * the signature: HolyC's `auto ...` is variadic too but hands the body an
 * argc/argv pair and never touches a save area, so keying off
 * has_var_args would spill 192 bytes into every one of them. IR_VA_START
 * is emitted only by `__builtin_va_region`, reachable only through C's
 * `va_start`. */
static int a64FnUsesVaStart(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_VA_START) return 1;
        }
    }
    return 0;
}

/* Emit the 32-bit form of an integer op (`add w0, w0, w1`)?
 *
 * For add/sub/mul and the bitwise ops the low 32 bits of the result
 * depend only on the low 32 bits of the operands, so the narrow form
 * computes the same value - and a W-register destination ZERO-EXTENDS
 * into the full X register, which makes the upper bits DEFINED rather
 * than unspecified.
 *
 * The width alone does NOT authorise this. A narrow value in a register
 * is otherwise kept SIGN-extended, so a w-form op standing on its own
 * turns a negative result positive: `int n = INT_MAX; n = n + 1;` would
 * leave 0x0000000080000000, right in its low half and positive to
 * anything reading all 64. What makes it safe is the canonicalising
 * IR_TRUNC that irNormaliseNarrowInt emits after the op - so the
 * permission travels on the instruction as IRCG_OP_NARROW, set at that
 * one place. A narrow IADD reached by a path that does not normalise
 * (`x += y`, RMW, GEP address arithmetic) keeps the 64-bit form. */
static int a64OpIs32Bit(IrInstr *i) {
    if (!i || !i->dst) return 0;
    if (!(i->flags & IRCG_OP_NARROW)) return 0;
    return irValueByteSize(i->dst) <= 4;
}

/* Conversion kinds for the shared convert lowering: one leaf emits the
 * instruction, a64EmitConvert owns the per-op dispatch and width
 * selection. */
typedef enum {
    A64_CVT_UXTB, A64_CVT_UXTH, A64_CVT_MOVW,        /* zero / truncate */
    A64_CVT_SXTB, A64_CVT_SXTH, A64_CVT_SXTW,        /* sign-extend */
    A64_CVT_FCVT_NARROW, A64_CVT_FCVT_WIDEN,         /* F64<->F32 */
    A64_CVT_FCVTZS, A64_CVT_FCVTZU,                  /* fp -> int (width=fp src) */
    A64_CVT_SCVTF, A64_CVT_UCVTF,                    /* int -> fp (width=fp dst) */
    A64_CVT_FMOV_GP2FP, A64_CVT_FMOV_FP2GP           /* bitcast (width=fp side) */
} A64CvtKind;

/* Copy `size` bytes from [base, #src] to the frame slot at #dst via x10. */
static void a64CopyToSlot(A64Ctx *ctx, int base, int size, int src,
                          int dst) {
    MirFn *m = ctx->mir;
    int o = 0;
    while (size - o >= 8) {
        a64MirLine(ctx, mir2(m, A64MN_LDR, mir_x(A_X10),
                             mir_mem(mir_x(base), src + o)));
        a64FrameStore(ctx, A_X10, 8, dst + o);
        o += 8;
    }
    int rem = size - o;
    if (rem == 4) {
        a64MirLine(ctx, mir2(m, A64MN_LDR, mir_w(A_X10),
                             mir_mem(mir_x(base), src + o)));
        a64FrameStore(ctx, A_X10, 4, dst + o);
    } else if (rem == 2) {
        a64MirLine(ctx, mir2(m, A64MN_LDRH, mir_w(A_X10),
                             mir_mem(mir_x(base), src + o)));
        a64FrameStore(ctx, A_X10, 2, dst + o);
    } else if (rem == 1) {
        a64MirLine(ctx, mir2(m, A64MN_LDRB, mir_w(A_X10),
                             mir_mem(mir_x(base), src + o)));
        a64FrameStore(ctx, A_X10, 1, dst + o);
    } else if (rem != 0) {
        loggerPanic("jit-aarch64: %d-byte param copy chunk not supported\n", rem);
    }
}

/* Copy `size` bytes from [src, #0..] to [sp, #dst] via x10. */
static void a64CopyToSp(A64Ctx *ctx, int src, int size, int dst) {
    MirFn *m = ctx->mir;
    int o = 0;
    while (size - o >= 8) {
        a64MirLine(ctx, mir2(m, A64MN_LDR, mir_x(A_X10),
                             mir_mem(mir_x(src), o)));
        a64MirLine(ctx, mir2(m, A64MN_STR, mir_x(A_X10),
                             mir_mem(mir_sp(), dst + o)));
        o += 8;
    }
    int rem = size - o;
    if (rem == 0) return;
    int ld = rem == 4 ? A64MN_LDR : rem == 2 ? A64MN_LDRH :
             rem == 1 ? A64MN_LDRB : TASM_MN_UNKNOWN;
    int st = rem == 4 ? A64MN_STR : rem == 2 ? A64MN_STRH :
             rem == 1 ? A64MN_STRB : TASM_MN_UNKNOWN;
    if (ld == TASM_MN_UNKNOWN)
        loggerPanic("jit-aarch64: %d-byte arg copy chunk not supported\n", rem);
    a64MirLine(ctx, mir2(m, ld, mir_w(A_X10), mir_mem(mir_x(src), o)));
    a64MirLine(ctx, mir2(m, st, mir_w(A_X10), mir_mem(mir_sp(), dst + o)));
}

/* ---------------- the main IR emitter ---------------- */

/* JIT (machine-code) leaf ops for the shared a64Emit* lowering. The
 * helper resolves r1 to the register it already lives in (a home or a
 * pin), or materialises it into `target` - which callers set to the
 * DESTINATION's home when there is one, so a constant lands there
 * directly instead of bouncing through the scratch. */
static int a64SrcReg(A64Ctx *ctx, IrInstr *i, int is_fpr,
                           int target) {
    AoStr *h = a64Home(i->r1);
    if (h && a64IsFpRegName(h->data) == is_fpr)
        return a64RegFromName(h->data);
    if (is_fpr) a64LoadToFpr(ctx, i->r1, target);
    else        a64LoadToReg(ctx, i->r1, target);
    return target;
}

static void a64StoreInstrFpr(A64Ctx *ctx, IrInstr *i, int val_size) {
    AoStr *dh = a64Home(i->dst);
    int target = (dh && a64IsFpRegName(dh->data))
                  ? a64RegFromName(dh->data) : A64_F0;
    a64StoreFprSized(ctx, i->dst, a64SrcReg(ctx, i, 1, target),
                     val_size);
}

static int a64IsZero(IrValue *v) {
    return v && v->kind == IR_VAL_CONST_INT && v->as._i64 == 0;
}

static void a64StoreInt(A64Ctx *ctx, IrInstr *i, int size) {
    int src = a64IsZero(i->r1) ? A_XZR : a64SrcReg(ctx, i, 0, A64_S0);
    a64FrameStore(ctx, src, size,
                  irCgGetLoff(&ctx->fn->ra, i->dst));
}

static void a64StoreIntPinned(A64Ctx *ctx, IrInstr *i) {
    int home = a64RegFromName(i->dst->pinned_reg->data);
    int src = a64SrcReg(ctx, i, 0, home);
    /* See a64TxtStoreIntPinned - the slot no longer truncates. */
    if (a64StoreNarrowHome(ctx, i->dst, home, src)) return;
    if (home != src)
        E2(ctx, A64MN_MOV, mir_x(home), mir_x(src));
}

/* Store instr->r1 into instr->dst. The leaves - not this dispatch -
 * decide whether the source value is already in a register or must be
 * loaded, because forcing every store through a fixed scratch register
 * clobbers a later store's reg-resident source. */
static void a64EmitStore(A64Ctx *ctx, IrInstr *instr) {
    IrValue *r1 = instr->r1;
    IrValue *dst = instr->dst;

    if (r1 && irIsFloat(r1->type)) {
        a64StoreInstrFpr(ctx, instr, (int)irValueByteSize(r1));
        return;
    }

    /* Pinned destination lives in a register, not a slot. */
    if (dst && dst->pinned_reg) {
        a64StoreIntPinned(ctx, instr);
        return;
    }

    /* Width is the destination's, not the source's: a wide value stored
     * into a narrow slot must truncate, else the extra bytes clobber
     * neighbouring slots (including saved x29/x30). */
    int src_size = (int)irValueByteSize(r1);
    int dst_size = (int)irValueByteSize(dst);
    a64StoreInt(ctx, instr, dst_size < src_size ? dst_size : src_size);
}

/* JIT (machine-code) leaf ops for the shared a64EmitCall lowering. */
static void a64LoadStructAddr(A64Ctx *ctx, IrValue *a) {
    a64LoadToReg(ctx, a, A_X9);
}

static void a64CopyX9ToSp(A64Ctx *ctx, int size, int dst_off) {
    a64CopyToSp(ctx, A_X9, size, dst_off);
}

static void a64AggLoad(A64Ctx *ctx, int is_fp, int reg, int src_off, int size) {
    if (is_fp) {
        E2(ctx, A64MN_LDR, a64MirFpSized(size, reg), mir_mem(mir_x(A_X9), src_off));
        return;
    }
    int mn = size >= 8 ? A64MN_LDR : size == 4 ? A64MN_LDR :
             size == 2 ? A64MN_LDRH : size == 1 ? A64MN_LDRB
                                                : TASM_MN_UNKNOWN;
    if (mn == TASM_MN_UNKNOWN)
        loggerPanic("jit-aarch64: %d-byte struct chunk not supported\n", size);
    E2(ctx, mn, a64MirGp(size >= 8, reg), mir_mem(mir_x(A_X9), src_off));
}

static void a64PtrInReg(A64Ctx *ctx, int reg, int sp_off) {
    E3(ctx, A64MN_ADD, mir_x(reg), mir_sp(), mir_imm(sp_off));
}

static void a64PtrToStack(A64Ctx *ctx, int sp_off, int nsaa) {
    E3(ctx, A64MN_ADD, mir_x(A_X10), mir_sp(), mir_imm(sp_off));
    E2(ctx, A64MN_STR, mir_x(A_X10), mir_mem(mir_sp(), nsaa));
}

static void a64ScalarToReg(A64Ctx *ctx, IrValue *a, int is_fp, int reg) {
    if (is_fp) a64LoadToFpr(ctx, a, reg);
    else       a64LoadToReg(ctx, a, (int)reg);
}

/* The register to store an integer value from: xzr for 0, its home if
 * it has one, else `scratch` after loading it there. Saves the copy
 * through a scratch that a store straight from the home makes moot. */
static int a64IntStoreSrc(A64Ctx *ctx, IrValue *a, int scratch) {
    if (a->kind == IR_VAL_CONST_INT && a->as._i64 == 0) return A_XZR;
    if (a->kind == IR_VAL_TMP || a->kind == IR_VAL_LOCAL ||
        a->kind == IR_VAL_PARAM)
    {
        AoStr *home = a64Home(a);
        if (home) return a64RegFromName(home->data);
    }
    a64LoadToReg(ctx, a, scratch);
    return scratch;
}

static void a64ScalarToStack(A64Ctx *ctx, IrValue *a, int is_fp, int nsaa) {
    if (is_fp) {
        /* Through v19, NOT v0. Arguments are staged left to right, so by
         * the time one overflows to the stack d0-d7 already hold the
         * register arguments - two `CGRect`s fill all eight - and
         * bouncing the overflow through d0 replaced the first of them.
         * v19 is nobody's: not an argument register, not callee-saved,
         * and not in the allocator's pool (d8-d18). */
        a64LoadToFpr(ctx, a, (int)19 /*v19*/);
        E2(ctx, A64MN_STR, a64MirFpSized((int)irValueByteSize(a), 19), mir_mem(mir_sp(), nsaa));
    } else {
        int src = a64IntStoreSrc(ctx, a, A_X9);
        E2(ctx, A64MN_STR, mir_x(src), mir_mem(mir_sp(), nsaa));
    }
}

static void a64Call(A64Ctx *ctx, AoStr *fname, int indirect) {
    if (indirect) {
        E1(ctx, A64MN_BLR, mir_x(A_X16));
    } else {
        char *norm = asmNormaliseFunctionName(ctx->jit->cc, fname);
        E1(ctx, A64MN_BL, mir_sym(norm));
    }
}

static void a64SpillRet(A64Ctx *ctx, IrInstr *instr, int is_float) {
    if (is_float) a64SpillDstFpr(ctx, instr, A_X0);
    else          a64SpillDst(ctx, instr, A_X0);
}
/* Store a result-register chunk of a register-returned struct into the
 * destination buffer parked in x9 (mirror of a64AggLoad). */
static int a64DestReg(A64Ctx *ctx) {
    return ctx->struct_dest_set ? ctx->struct_dest : A_X9;
}

static void a64RetChunkStore(A64Ctx *ctx, int is_fp, int reg, int off, int size) {
    int d = a64DestReg(ctx);
    if (is_fp) {
        E2(ctx, A64MN_STR, a64MirFpSized(size, reg), mir_mem(mir_x(d), off));
        return;
    }
    int mn = size >= 8 ? A64MN_STR : size == 4 ? A64MN_STR :
             size == 2 ? A64MN_STRH : size == 1 ? A64MN_STRB
                                                : TASM_MN_UNKNOWN;
    if (mn == TASM_MN_UNKNOWN)
        loggerPanic("jit-aarch64: %d-byte ret chunk not supported\n", size);
    E2(ctx, mn, a64MirGp(size >= 8, reg), mir_mem(mir_x(d), off));
}

static void a64SpillStructDst(A64Ctx *ctx, IrInstr *instr) {
    a64SpillDst(ctx, instr, a64DestReg(ctx));
}

/* Mirrors a64TxtDestSurvivesCall. */
static int a64DestSurvivesCall(A64Ctx *ctx, IrValue *v) {
    (void)ctx;
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

static int a64UseDestHome(A64Ctx *ctx, IrValue *v) {
    ctx->struct_dest_set = 0;
    AoStr *home = a64Home(v);
    if (!home || !a64DestSurvivesCall(ctx, v)) return 0;
    ctx->struct_dest = a64RegFromName(home->data);
    ctx->struct_dest_set = 1;
    return 1;
}

static void a64UnstashDest(A64Ctx *ctx, int sp_off) {
    E2(ctx, A64MN_LDR, mir_x(A_X9), mir_mem(mir_sp(), sp_off));
}

/* Bytes a64EmitCall needs below sp: the stack-argument area for
 * register overflow, the INDIRECT copy area for >16-byte aggregates,
 * and a slot to stash the return buffer's address across the call. */
static int a64AapcsCallBytes(IrInstr *instr, Vec *args, int callee_va,
                             int named_count, int force_va_stack,
                             int *nsaa_out, int *indirect_out,
                             int *dest_resident_out)
{
    u64 n = args ? args->size : 0;
    AstType *ret_st = (instr->flags & IRCG_CALL_AGG_RETURN) && instr->dst
                      ? instr->dst->byval_struct_type : NULL;
    int fwd_ret = (instr->flags & IRCG_AGG_RET_FORWARD) != 0;
    u64 a0 = ret_st ? 1 : 0;
    int tail_start = (callee_va && force_va_stack) ? (int)a0 + named_count : -1;

    int nsaa_total = 0, indirect_bytes = 0;
    int g = 0, sr = 0;
    for (u64 i = a0; i < n; ++i) {
        IrValue *a = vecGet(IrValue *, args, i);
        int forced = tail_start >= 0 && (int)i >= tail_start;
        if (a->byval_struct_type) {
            AstType *t = a->byval_struct_type;
            int el = 0, c = 0;
            AapcsClass cl = astAapcsClassify(t, &el, &c);
            if (cl == AAPCS_INDIRECT) {
                indirect_bytes += (t->size + 15) & ~15;
                if (g < 8) g++; else nsaa_total += 8;
            } else if (cl == AAPCS_HFA) {
                if (sr + c <= 8) sr += c;
                else { nsaa_total += (t->size + 7) & ~7; sr = 8; }
            } else {
                int ngp = (t->size + 7) / 8;
                if (g + ngp <= 8) g += ngp;
                else { nsaa_total += (t->size + 7) & ~7; g = 8; }
            }
        } else if (irIsFloat(a->type)) {
            if (!forced && sr < 8) sr++; else nsaa_total += 8;
        } else {
            if (!forced && g < 8) g++; else nsaa_total += 8;
        }
    }
    /* The destination pointer is computed into x9, which is
     * caller-saved, so it normally has to be spilled across the call.
     * When the VALUE it came from survives the call by itself - a slot,
     * or a callee-saved register the allocator handed out - the spill
     * is pure waste: recompute the address afterwards instead. */
    int need_dest = ret_st && !fwd_ret;
    int dest_resident = need_dest &&
                        a64DestSurvivesCall(NULL, vecGet(IrValue *, args, 0));
    if (nsaa_out) *nsaa_out = nsaa_total;
    if (indirect_out) *indirect_out = indirect_bytes;
    if (dest_resident_out) *dest_resident_out = dest_resident;
    return (nsaa_total + indirect_bytes +
            ((need_dest && !dest_resident) ? 16 : 0) + 15) & ~15;
}

/* AAPCS call emission. ALL of the ABI accounting lives here once: the
 * pre-pass that sizes the two below-sp regions (the stack-argument
 * area for register overflow + the INDIRECT copy area for >16-byte
 * aggregates), the per-argument classification, and the NGRN/NSRN/
 * stack-cursor bookkeeping. The leaves above emit one primitive each;
 * the struct address is parked in x9 by a64LoadStructAddr, x10 is
 * the copy scratch. */
static void a64EmitCall(A64Ctx *ctx, IrInstr *instr,
                        Vec *args, AoStr *fname, int indirect,
                        int callee_va, int named_count, int force_va_stack) {
    u64 n = args ? args->size : 0;
    int ngrn = 0, nsrn = 0;

    /* args[0] of an aggregate-returning call is the destination buffer,
     * and it is never one of the x0-x7 arguments: a <=16-byte return
     * comes back in registers (copy them into the buffer after the
     * call), a >16-byte (INDIRECT) return passes the buffer address in
     * x8, AAPCS64's dedicated indirect-result register. Skip it in the
     * arg passing either way. */
    AstType *ret_st = (instr->flags & IRCG_CALL_AGG_RETURN) && instr->dst
                      ? instr->dst->byval_struct_type : NULL;
    /* The result IS this function's return value and the `ret` is next:
     * the bytes are already in the registers the epilogue wants, so the
     * buffer, the address kept alive to reach it, and the store through
     * it all go. args[0] stays in the vector as the placeholder the arg
     * loop skips - there is simply nothing left to do with it. */
    int fwd_ret = (instr->flags & IRCG_AGG_RET_FORWARD) != 0;
    int ind_ret = ret_st && astAggRetIsIndirect(ret_st, 1);
    int reg_ret = ret_st && !ind_ret && !fwd_ret;
    u64 a0 = ret_st ? 1 : 0;

    /* HolyC's `auto ...` (and C varargs on Apple) send the WHOLE tail -
     * the implicit argc and every slot - through the stack, whatever
     * registers are free; the callee reads argc at [x29+16]. The simple
     * call path applies that rule in a64PartitionCallArgs, and this one
     * has to apply the same rule, or a variadic function that returns
     * a struct finds its arguments in registers and reads argc from a
     * slot nobody wrote. `named_count` counts the callee's declared
     * params; args[0] is the return buffer when there is one. */
    int tail_start = (callee_va && force_va_stack) ? (int)a0 + named_count : -1;

    /* The area itself was reserved by the prologue: see out_area. */
    int nsaa_total = 0, indirect_bytes = 0, dest_resident = 0;
    a64AapcsCallBytes(instr, args, callee_va, named_count, force_va_stack,
                      &nsaa_total, &indirect_bytes, &dest_resident);
    int need_dest = ret_st && !fwd_ret;
    /* Stronger still: the address is ALREADY in a surviving register,
     * so it need not be moved into x9 either side of the call. */
    int dest_homed = need_dest &&
                     a64UseDestHome(ctx, vecGet(IrValue *, args, 0));
    int dest_save_off = nsaa_total + indirect_bytes;
    int nsaa = 0, copy_off = 0, copy_base = nsaa_total;

    if (need_dest && !dest_resident && !dest_homed) {
        /* Compute the destination buffer address into x9 (before the arg
         * loop clobbers x9) and stash it across the call. */
        int r = a64IntStoreSrc(ctx, vecGet(IrValue *, args, 0), A_X9);
        E2(ctx, A64MN_STR, mir_x(r), mir_mem(mir_sp(), dest_save_off));
    }
    /* AAPCS64: an INDIRECT return buffer's address goes in x8, the
     * dedicated indirect-result register (not x0 - that's SysV's rdi
     * rule). Load it BEFORE the arg loop: args[0] may live in an arg
     * register (a forwarded LEA), which the loop is about to overwrite;
     * nothing in the loop touches x8. */
    if (ind_ret) a64ScalarToReg(ctx, vecGet(IrValue *, args, 0), 0, 8);

    for (u64 i = a0; i < n; ++i) {
        IrValue *a = vecGet(IrValue *, args, i);
        int forced = tail_start >= 0 && (int)i >= tail_start;
        if (a->byval_struct_type) {
            AstType *t = a->byval_struct_type;
            int elem = 0, count = 0;
            AapcsClass cls = astAapcsClassify(t, &elem, &count);
            a64LoadStructAddr(ctx, a);   /* -> x9 */
            if (cls == AAPCS_INDIRECT) {
                int dst = copy_base + copy_off;
                a64CopyX9ToSp(ctx, t->size, dst);
                if (ngrn < 8) {
                    a64PtrInReg(ctx, ngrn, dst);
                    ngrn++;
                } else {
                    a64PtrToStack(ctx, dst, nsaa);
                    nsaa += 8;
                }
                copy_off += (t->size + 15) & ~15;
            } else if (cls == AAPCS_HFA) {
                if (nsrn + count <= 8) {
                    for (int k = 0; k < count; ++k) {
                        a64AggLoad(ctx, 1, nsrn + k, k * elem, elem);
                    }
                    nsrn += count;
                } else {
                    a64CopyX9ToSp(ctx, t->size, nsaa);
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
                    a64CopyX9ToSp(ctx, t->size, nsaa);
                    nsaa += (t->size + 7) & ~7; ngrn = 8;
                }
            }
        } else if (irIsFloat(a->type)) {
            if (!forced && nsrn < 8) {
                a64ScalarToReg(ctx, a, 1, nsrn);
                nsrn++;
            } else {
                a64ScalarToStack(ctx, a, 1, nsaa);
                nsaa += 8;
            }
        } else {
            if (!forced && ngrn < 8) {
                a64ScalarToReg(ctx, a, 0, ngrn);
                ngrn++;
            } else {
                a64ScalarToStack(ctx, a, 0, nsaa);
                nsaa += 8;
            }
        }
    }

    a64Call(ctx, fname, indirect);

    if (reg_ret) {
        /* Reload the destination buffer (saved before the call) and copy the
         * result registers into it; do this while sp is still lowered. */
        int re = 0, rc = 0;
        AapcsClass rcls = astAapcsClassify(ret_st, &re, &rc);
        if (dest_homed)         { /* already there */ }
        else if (dest_resident) a64LoadStructAddr(ctx, vecGet(IrValue *, args, 0));
        else                    a64UnstashDest(ctx, dest_save_off);
        if (rcls == AAPCS_HFA) {
            for (int k = 0; k < rc; ++k)
                a64RetChunkStore(ctx, 1, k, k * re, re);
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
         * still lowered - reloaded, or recomputed when the source
         * outlived the call on its own. */
        if (dest_homed)         { /* already there */ }
        else if (dest_resident) a64LoadStructAddr(ctx, vecGet(IrValue *, args, 0));
        else                    a64UnstashDest(ctx, dest_save_off);
    }

    if (need_dest) {
        a64SpillStructDst(ctx, instr);  /* dst = buffer address (x9) */
    } else if (!ret_st && instr->dst && instr->dst->type != IR_TYPE_VOID) {
        /* Scalar return in x0 / d0. */
        a64SpillRet(ctx, instr, irIsFloat(instr->dst->type));
    }
}

/* JIT (machine-code) leaf ops for the shared a64EmitParamPrologue. */
static void a64AggStore(A64Ctx *ctx, int is_fp, int reg, int loff, int size) {
    if (is_fp) {
        a64FpFrameStore(ctx, (int)reg, size, loff);
    } else {
        if (size != 1 && size != 2 && size != 4 && size != 8)
            loggerPanic("jit-aarch64: %d-byte struct chunk not supported\n",
                        size);
        a64FrameStore(ctx, (int)reg, size, loff);
    }
}

static void a64CopyStackToSlot(A64Ctx *ctx, int size, int incoming_off,
                                  int loff) {
    a64CopyToSlot(ctx, A_FP, size, incoming_off, loff);
}

static void a64IndirectToSlot(A64Ctx *ctx, int from_stack, int reg,
                                 int incoming_off, int size, int loff) {
    int base;
    if (from_stack) {
        E2(ctx, A64MN_LDR, mir_x(A_X9), mir_mem(mir_x(A_FP), incoming_off));
        base = A_X9;
    } else {
        base = (int)reg;
    }
    a64CopyToSlot(ctx, base, size, 0, loff);
}

/* AAPCS callee param prologue (the mirror of a64EmitCall). Receives
 * by-value struct params (and register-overflow scalars) into their
 * frame slots, owning the same ABI accounting as the caller side -
 * classification, NGRN/NSRN counters, the incoming-stack cursor (args
 * start at [x29, #16]), and the "unreferenced param -> no slot, still
 * advance the cursor" rule. Reg-resident scalar params are handled by
 * the IR arrive+store path; here they only advance the counters. */
static void a64EmitParamPrologue(A64Ctx *ctx, Ast *ast, IrFunction *fn) {
    if (!ast->params) return;
    /* Next General-purpose Register Number */
    int ngrn = 0;
    /* Next simd reg number */
    int nsrn = 0;
    /* Next stack argument address */
    int nsaa = 0;

    /* An INDIRECT (>16-byte) struct return's hidden out-pointer arrives
     * in x8, AAPCS64's dedicated indirect-result register, so it consumes
     * no argument register (the IR-level arrive+store spills it); a
     * <=16-byte aggregate comes back in registers. Either way ngrn
     * starts at 0. */

    for (u64 i = 0; i < ast->params->size; ++i) {
        Ast *p = vecGet(Ast *, ast->params, i);
        if (p->kind == AST_VAR_ARGS) break;
        AstType *t = p->type;

        /* loff is only valid for a referenced param; an unreferenced one
         * gets no slot, but the caller still placed it so the counters /
         * cursor must advance. */
        IrValue *slot = irFnGetVar(fn, irGetParamId(p));
        int has_slot = slot && mapHasInt(fn->ra.id_to_loff, irVarId(slot));
        int loff = has_slot ? irCgGetLoff(&fn->ra, slot) : 0;

        if (!a64IsByvalStruct(t)) {
            int is_float = (t->kind == AST_TYPE_FLOAT);
            if (is_float ? (nsrn < 8) : (ngrn < 8)) {
                if (is_float) nsrn++;
                else          ngrn++;  /* reg: arrive+store */
            } else {
                if (has_slot)
                    a64CopyStackToSlot(ctx, t->size, 16 + nsaa, loff);
                nsaa += 8;
            }
            continue;
        }

        int elem = 0, count = 0;
        AapcsClass cls = astAapcsClassify(t, &elem, &count);
        if (cls == AAPCS_HFA) {
            if (nsrn + count <= 8) {
                if (has_slot) {
                    for (int k = 0; k < count; ++k) {
                        a64AggStore(ctx, 1, nsrn + k, loff + k * elem, elem);
                    }
                }
                nsrn += count;
            } else {
                if (has_slot)
                    a64CopyStackToSlot(ctx, t->size, 16 + nsaa, loff);
                nsaa += (t->size + 7) & ~7; nsrn = 8;
            }
        } else if (cls == AAPCS_INTEGER) {
            int ngp = (t->size + 7) / 8;
            if (ngrn + ngp <= 8) {
                if (has_slot) for (int k = 0; k < ngp; ++k) {
                    int off = k * 8, rem = t->size - off;
                    a64AggStore(ctx, 0, ngrn + k, loff + off,
                                   rem >= 8 ? 8 : rem);
                }
                ngrn += ngp;
            } else {
                if (has_slot)
                    a64CopyStackToSlot(ctx, t->size, 16 + nsaa, loff);
                nsaa += (t->size + 7) & ~7; ngrn = 8;
            }
        } else { /* AAPCS_INDIRECT */
            int from_stack = (ngrn >= 8);
            int incoming_off = 16 + nsaa;
            int reg = ngrn;
            if (from_stack) nsaa += 8; else ngrn += 1;
            if (has_slot)
                a64IndirectToSlot(ctx, from_stack, reg, incoming_off,
                                     t->size, loff);
        }
    }
}

/* ---------------- arith / convert leaf set ----------------
 *
 * The fallback for an op the home-to-home forms above cannot take:
 * `out` is where the result is computed, `src` where r1 is read. Every
 * op is built as a MIR line and encoded from it. */

/* r1's register: its home when it has one of the right class, else
 * `out` after loading it there. */
static int a64SrcIn(A64Ctx *ctx, IrValue *v, int fp, int out) {
    AoStr *h = a64Home(v);
    if (h && a64IsFpRegName(h->data) == fp) return a64RegFromName(h->data);
    if (fp) a64LoadToFpr(ctx, v, out);
    else    a64LoadToReg(ctx, v, out);
    return out;
}

/* Where to compute: the destination's home, so no copy follows. */
static int a64AccOut(IrInstr *i, int fp) {
    return a64DstOut(i, fp, fp ? A64_F0 : A64_S0);
}

/* Where to load r1 when it has no home: `out`, unless r2 lives there
 * (its range ended here) - loading r1 first would destroy r2 before the
 * op reads it. The op itself reads both before writing `out`. */
static int a64LoadTo(IrInstr *i, int fp, int out) {
    AoStr *r2h = a64Home(i->r2);
    if (r2h && a64IsFpRegName(r2h->data) == fp &&
        a64RegFromName(r2h->data) == out)
    {
        return fp ? A64_F0 : A64_S0;
    }
    return out;
}

/* r2's register: its home, else loaded into the second scratch. */
static int a64RhsIn(A64Ctx *ctx, IrInstr *i, int fp) {
    return a64SrcIn(ctx, i->r2, fp, fp ? A64_F1 : A64_S1);
}

static int a64MirAluImm(A64Ctx *ctx, IrOp op, s64 imm, IrInstr *i,
                        int out, int src) {
    if (op != IR_IADD && op != IR_ISUB) return 0;
    int sf = !a64OpIs32Bit(i);
    int as_add = (op == IR_IADD);
    if (a64IsAddImm(imm)) { /* keep */ }
    else if (imm < 0 && a64IsAddImm(-imm)) { as_add = !as_add; imm = -imm; }
    else return 0;
    E3(ctx, as_add ? A64MN_ADD : A64MN_SUB, a64MirGp(sf, out), a64MirGp(sf, src), mir_imm(imm));
    return 1;
}
static void a64MirAluReg(A64Ctx *ctx, IrOp op, IrInstr *i,
                         int out, int src) {
    int rhs = a64RhsIn(ctx, i, 0);
    int sf = !a64OpIs32Bit(i);
    E3(ctx, a64MirAluMn(op), a64MirGp(sf, out), a64MirGp(sf, src), a64MirGp(sf, rhs));
}
static void a64MirMul(A64Ctx *ctx, IrInstr *i, int out, int src) {
    int rhs = a64RhsIn(ctx, i, 0);
    int sf = !a64OpIs32Bit(i);
    E3(ctx, A64MN_MUL, a64MirGp(sf, out), a64MirGp(sf, src), a64MirGp(sf, rhs));
}
static void a64MirDivRem(A64Ctx *ctx, IrOp op, IrInstr *i,
                         int out, int src) {
    int rhs = a64RhsIn(ctx, i, 0);
    int mn = (op == IR_IDIV || op == IR_IREM) ? A64MN_SDIV : A64MN_UDIV;
    int is_rem = (op == IR_IREM || op == IR_UREM);
    if (!is_rem) {
        E3(ctx, mn, mir_x(out), mir_x(src), mir_x(rhs));
    } else {
        E3(ctx, mn, mir_x(A64_S2), mir_x(src), mir_x(rhs));
        E4(ctx, A64MN_MSUB, mir_x(out), mir_x(A64_S2), mir_x(rhs), mir_x(src));
    }
}
static void a64MirUnary(A64Ctx *ctx, IrOp op, IrInstr *i,
                        int out, int src) {
    int sf = !a64OpIs32Bit(i);
    E2(ctx, op == IR_INEG ? A64MN_NEG : A64MN_MVN, a64MirGp(sf, out), a64MirGp(sf, src));
}
static void a64MirShiftImm(A64Ctx *ctx, IrOp op, int sh,
                           int out, int src) {
    E3(ctx, a64MirShiftMn(op), mir_x(out), mir_x(src), mir_imm(sh));
}
static void a64MirShiftReg(A64Ctx *ctx, IrOp op, IrInstr *i,
                           int out, int src) {
    int rhs = a64RhsIn(ctx, i, 0);
    E3(ctx, a64MirShiftMn(op), mir_x(out), mir_x(src), mir_x(rhs));
}
static int a64MirAlu3(A64Ctx *ctx, IrOp op, IrInstr *i) {
    AoStr *d = a64Home(i->dst);
    AoStr *a = a64Home(i->r1);
    if (!d || !a) return 0;
    int rd = a64RegFromName(d->data), ra = a64RegFromName(a->data);
    int sf = !a64OpIs32Bit(i);

    if (i->r2 && i->r2->kind == IR_VAL_CONST_INT) {
        if (op != IR_IADD && op != IR_ISUB) return 0;
        s64 imm = i->r2->as._i64;
        int as_add = (op == IR_IADD);
        if (a64IsAddImm(imm)) { /* keep */ }
        else if (imm < 0 && a64IsAddImm(-imm)) { as_add = !as_add; imm = -imm; }
        else return 0;
        E3(ctx, as_add ? A64MN_ADD : A64MN_SUB, a64MirGp(sf, rd), a64MirGp(sf, ra), mir_imm(imm));
        return 1;
    }

    AoStr *b = a64Home(i->r2);
    if (!b) return 0;
    int mn = a64MirAluMn(op);
    if (mn == TASM_MN_UNKNOWN) return 0;
    E3(ctx, mn, a64MirGp(sf, rd), a64MirGp(sf, ra), a64MirGp(sf, a64RegFromName(b->data)));
    return 1;
}
static int a64MirShift3(A64Ctx *ctx, IrOp op, IrInstr *i) {
    AoStr *d = a64Home(i->dst);
    AoStr *a = a64Home(i->r1);
    if (!d || !a) return 0;
    int rd = a64RegFromName(d->data), ra = a64RegFromName(a->data);
    IrValue *r2 = i->r2;
    if (r2 && r2->kind == IR_VAL_CONST_INT &&
        r2->as._i64 >= 0 && r2->as._i64 < 64)
    {
        E3(ctx, a64MirShiftMn(op), mir_x(rd), mir_x(ra), mir_imm(r2->as._i64));
        return 1;
    }
    AoStr *b = a64Home(r2);
    if (!b) return 0;
    E3(ctx, a64MirShiftMn(op), mir_x(rd), mir_x(ra), mir_x(a64RegFromName(b->data)));
    return 1;
}
static int a64MirMul3(A64Ctx *ctx, IrInstr *i) {
    AoStr *d = a64Home(i->dst);
    AoStr *a = a64Home(i->r1);
    AoStr *b = a64Home(i->r2);
    if (!d || !a || !b) return 0;
    int sf = !a64OpIs32Bit(i);
    E3(ctx, A64MN_MUL, a64MirGp(sf, a64RegFromName(d->data)), a64MirGp(sf, a64RegFromName(a->data)), a64MirGp(sf, a64RegFromName(b->data)));
    return 1;
}
static int a64MirUnary3(A64Ctx *ctx, IrOp op, IrInstr *i) {
    AoStr *d = a64Home(i->dst);
    AoStr *a = a64Home(i->r1);
    if (!d || !a) return 0;
    int sf = !a64OpIs32Bit(i);
    E2(ctx, op == IR_INEG ? A64MN_NEG : A64MN_MVN, a64MirGp(sf, a64RegFromName(d->data)), a64MirGp(sf, a64RegFromName(a->data)));
    return 1;
}
static int a64MirFalu3(A64Ctx *ctx, IrOp op, IrInstr *i) {
    AoStr *d = a64Home(i->dst);
    AoStr *a = a64Home(i->r1);
    AoStr *b = a64Home(i->r2);
    if (!d || !a || !b) return 0;
    /* A TempleOS-pinned GP register can hold FP bits; that shape keeps
     * the scratch path. */
    if (!a64IsFpRegName(d->data) || !a64IsFpRegName(a->data) ||
        !a64IsFpRegName(b->data))
    {
        return 0;
    }
    int is64 = (int)irValueByteSize(i->dst) == 8;
    E3(ctx, a64MirFopMn(op), a64MirFp(is64, a64RegFromName(d->data)), a64MirFp(is64, a64RegFromName(a->data)), a64MirFp(is64, a64RegFromName(b->data)));
    return 1;
}
static void a64MirFbinop(A64Ctx *ctx, IrOp op, IrInstr *i, int width,
                         int out, int src) {
    int is64 = (width == 8);
    int rhs = a64RhsIn(ctx, i, 1);
    E3(ctx, a64MirFopMn(op), a64MirFp(is64, out), a64MirFp(is64, src), a64MirFp(is64, rhs));
}
static void a64MirFneg(A64Ctx *ctx, int width, int out, int src) {
    int is64 = (width == 8);
    E2(ctx, A64MN_FNEG, a64MirFp(is64, out), a64MirFp(is64, src));
}

static void a64MirConvert(A64Ctx *ctx, A64CvtKind k, int width,
                          int out, int src) {
    MirFn *m = ctx->mir;
    switch (k) {
    case A64_CVT_UXTB:
        a64MirLine(ctx, mir2(m, A64MN_UXTB, mir_w(out), mir_w(src))); break;
    case A64_CVT_UXTH:
        a64MirLine(ctx, mir2(m, A64MN_UXTH, mir_w(out), mir_w(src))); break;
    case A64_CVT_MOVW:
        a64MirLine(ctx, mir2(m, A64MN_MOV, mir_w(out), mir_w(src))); break;
    case A64_CVT_SXTB:
        a64MirLine(ctx, mir2(m, A64MN_SXTB, mir_x(out), mir_w(src))); break;
    case A64_CVT_SXTH:
        a64MirLine(ctx, mir2(m, A64MN_SXTH, mir_x(out), mir_w(src))); break;
    case A64_CVT_SXTW:
        a64MirLine(ctx, mir2(m, A64MN_SXTW, mir_x(out), mir_w(src))); break;
    case A64_CVT_FCVT_NARROW:
        a64MirLine(ctx, mir2(m, A64MN_FCVT, mir_s(out), mir_d(src))); break;
    case A64_CVT_FCVT_WIDEN:
        a64MirLine(ctx, mir2(m, A64MN_FCVT, mir_d(out), mir_s(src))); break;
    case A64_CVT_FCVTZS:
        a64MirLine(ctx, mir2(m, A64MN_FCVTZS, mir_x(out),
                             a64MirFp(width == 8, src)));
        break;
    case A64_CVT_FCVTZU:
        a64MirLine(ctx, mir2(m, A64MN_FCVTZU, mir_x(out),
                             a64MirFp(width == 8, src)));
        break;
    case A64_CVT_SCVTF:
        a64MirLine(ctx, mir2(m, A64MN_SCVTF, a64MirFp(width == 8, out),
                             mir_x(src)));
        break;
    case A64_CVT_UCVTF:
        a64MirLine(ctx, mir2(m, A64MN_UCVTF, a64MirFp(width == 8, out),
                             mir_x(src)));
        break;
    case A64_CVT_FMOV_GP2FP:
        a64MirLine(ctx, mir2(m, A64MN_FMOV, a64MirFp(width == 8, out),
                             a64MirGp(width == 8, src)));
        break;
    case A64_CVT_FMOV_FP2GP:
        a64MirLine(ctx, mir2(m, A64MN_FMOV, a64MirGp(width == 8, out),
                             a64MirFp(width == 8, src)));
        break;
    }
}
static int a64MirConv3(A64Ctx *ctx, A64CvtKind k, IrInstr *i) {
    AoStr *d = a64Home(i->dst);
    AoStr *a = a64Home(i->r1);
    if (!d || !a) return 0;
    int rd = a64RegFromName(d->data), rs = a64RegFromName(a->data);
    /* See a64TxtConv3 - proved a register-to-itself copy. */
    if ((i->flags & IRCG_WIDEN_IS_COPY) && rd == rs) return 1;
    switch (k) {
    case A64_CVT_UXTB:
        E2(ctx, A64MN_UXTB, mir_w(rd), mir_w(rs));
        return 1;
    case A64_CVT_UXTH:
        E2(ctx, A64MN_UXTH, mir_w(rd), mir_w(rs));
        return 1;
    case A64_CVT_MOVW:
        E2(ctx, A64MN_MOV, mir_w(rd), mir_w(rs));
        return 1;
    case A64_CVT_SXTB:
        E2(ctx, A64MN_SXTB, mir_x(rd), mir_w(rs));
        return 1;
    case A64_CVT_SXTH:
        E2(ctx, A64MN_SXTH, mir_x(rd), mir_w(rs));
        return 1;
    case A64_CVT_SXTW:
        E2(ctx, A64MN_SXTW, mir_x(rd), mir_w(rs));
        return 1;
    default: return 0;
    }
}

/* A conversion's registers: `out` from a64AccOut, `src` r1's home
 * or r1 loaded into `out` - or into its own class's scratch when the
 * conversion crosses between GP and FP. */
static void a64CvtRegs(A64Ctx *ctx, IrInstr *i, int src_fp, int dst_fp,
                       int *out, int *src) {
    *out = a64AccOut(i, dst_fp);
    int load_to = src_fp == dst_fp ? *out : (src_fp ? A64_F0 : A64_S0);
    *src = a64SrcIn(ctx, i->r1, src_fp, load_to);
}

static void a64CvtSpill(A64Ctx *ctx, IrInstr *i, int fp, int reg) {
    if (fp) a64SpillDstFpr(ctx, i, reg);
    else    a64SpillDst(ctx, i, reg);
}

/* Conversion lowering (TRUNC/ZEXT/SEXT, fp<->fp, int<->fp, bitcast).
 * Every conversion has the same shape - try the home-to-home form
 * first, else take r1 where it is (or load it), emit ONE convert into
 * `out`, spill - so this dispatch owns the per-op width selection
 * (this is where the ZEXT-of-a-4-byte-load bug lived, fixed once
 * here). A conversion that turns out to be a no-op spills `src`. */
static void a64EmitConvert(A64Ctx *ctx, IrInstr *instr) {
    int out, src;
    switch (instr->op) {
    case IR_TRUNC: {
        int sz = instr->dst ? (int)instr->dst->as.var.size : 8;
        /* The result still lives in a 64-bit register, so the high bits
         * have to reflect the destination's signedness - otherwise a
         * later compare reads a different number entirely. */
        int sgn = (instr->flags & IRCG_TRUNC_SIGNED) != 0;
        A64CvtKind k = sz == 1 ? (sgn ? A64_CVT_SXTB : A64_CVT_UXTB)
                     : sz == 2 ? (sgn ? A64_CVT_SXTH : A64_CVT_UXTH)
                     : sz == 4 ? (sgn ? A64_CVT_SXTW : A64_CVT_MOVW)
                     : (A64CvtKind)-1;
        if ((int)k != -1 && a64MirConv3(ctx, k, instr)) break;
        a64CvtRegs(ctx, instr, 0, 0, &out, &src);
        if ((int)k == -1) { a64CvtSpill(ctx, instr, 0, src); break; }
        a64MirConvert(ctx, k, 0, out, src);
        a64CvtSpill(ctx, instr, 0, out);
        break;
    }
    case IR_ZEXT: {
        int sz = (int)irValueByteSize(instr->r1);
        A64CvtKind k = sz == 1 ? A64_CVT_UXTB
                     : sz == 2 ? A64_CVT_UXTH
                     : sz == 4 ? A64_CVT_MOVW : (A64CvtKind)-1;
        if ((int)k != -1 && a64MirConv3(ctx, k, instr)) break;
        a64CvtRegs(ctx, instr, 0, 0, &out, &src);
        /* A 1- or 2-byte frame load already zero-extends (`ldurb`/
         * `ldurh`), so widening it again is a no-op. A 4-byte load does
         * NOT - it sign-extends, matching `movslq` - so that one still
         * needs the explicit widen. */
        int fused_zx = irValueInMemory(instr->r1) && (sz == 1 || sz == 2);
        if (fused_zx || (int)k == -1) { a64CvtSpill(ctx, instr, 0, src); break; }
        a64MirConvert(ctx, k, 0, out, src);
        a64CvtSpill(ctx, instr, 0, out);
        break;
    }
    case IR_SEXT: {
        int sz = (int)irValueByteSize(instr->r1);
        /* A 4-byte load already sign-extended to 64 bits, so this widen
         * is a copy. When source and destination share a register it is
         * not even that. */
        if (instr->flags & IRCG_SEXT_NOOP) {
            a64CvtRegs(ctx, instr, 0, 0, &out, &src);
            a64CvtSpill(ctx, instr, 0, src);
            break;
        }
        A64CvtKind k = sz == 1 ? A64_CVT_SXTB
                     : sz == 2 ? A64_CVT_SXTH
                     : sz == 4 ? A64_CVT_SXTW : (A64CvtKind)-1;
        if ((int)k != -1 && a64MirConv3(ctx, k, instr)) break;
        a64CvtRegs(ctx, instr, 0, 0, &out, &src);
        /* A 4-byte memory load already sign-extends, so that case needs
         * nothing extra on top of the load. */
        if ((irValueInMemory(instr->r1) && sz == 4) || (int)k == -1) {
            a64CvtSpill(ctx, instr, 0, src);
            break;
        }
        a64MirConvert(ctx, k, 0, out, src);
        a64CvtSpill(ctx, instr, 0, out);
        break;
    }
    case IR_FPTRUNC:
        a64CvtRegs(ctx, instr, 1, 1, &out, &src);
        a64MirConvert(ctx, A64_CVT_FCVT_NARROW, 0, out, src);
        a64CvtSpill(ctx, instr, 1, out); break;
    case IR_FPEXT:
        a64CvtRegs(ctx, instr, 1, 1, &out, &src);
        a64MirConvert(ctx, A64_CVT_FCVT_WIDEN, 0, out, src);
        a64CvtSpill(ctx, instr, 1, out); break;
    case IR_FPTOSI:
        a64CvtRegs(ctx, instr, 1, 0, &out, &src);
        a64MirConvert(ctx, A64_CVT_FCVTZS, (int)irValueByteSize(instr->r1),
                      out, src);
        a64CvtSpill(ctx, instr, 0, out); break;
    case IR_FPTOUI:
        a64CvtRegs(ctx, instr, 1, 0, &out, &src);
        a64MirConvert(ctx, A64_CVT_FCVTZU, (int)irValueByteSize(instr->r1),
                      out, src);
        a64CvtSpill(ctx, instr, 0, out); break;
    case IR_SITOFP:
        a64CvtRegs(ctx, instr, 0, 1, &out, &src);
        a64MirConvert(ctx, A64_CVT_SCVTF, (int)irValueByteSize(instr->dst),
                      out, src);
        a64CvtSpill(ctx, instr, 1, out); break;
    case IR_UITOFP:
        a64CvtRegs(ctx, instr, 0, 1, &out, &src);
        a64MirConvert(ctx, A64_CVT_UCVTF, (int)irValueByteSize(instr->dst),
                      out, src);
        a64CvtSpill(ctx, instr, 1, out); break;
    case IR_PTRTOINT:
    case IR_INTTOPTR:
    case IR_BITCAST: {
        int dst_fp = irIsFloat(instr->dst->type);
        int src_fp = irIsFloat(instr->r1->type);
        a64CvtRegs(ctx, instr, src_fp, dst_fp, &out, &src);
        if (dst_fp == src_fp) {
            a64CvtSpill(ctx, instr, dst_fp, src);
        } else if (dst_fp) {
            a64MirConvert(ctx, A64_CVT_FMOV_GP2FP,
                          (int)irValueByteSize(instr->dst), out, src);
            a64CvtSpill(ctx, instr, 1, out);
        } else {
            a64MirConvert(ctx, A64_CVT_FMOV_FP2GP,
                          (int)irValueByteSize(instr->r1), out, src);
            a64CvtSpill(ctx, instr, 0, out);
        }
        break;
    }
    default: break;
    }
}

/* Arithmetic lowering (integer binary/mul/div/rem/unary/shift + float
 * binary/neg). This dispatch owns the op selection and the operand
 * pattern: try the home-to-home 3-address form first, else compute
 * into `out` from r1 where it is, fold a constant r2 when it encodes,
 * spill. */
static void a64EmitArith(A64Ctx *ctx, IrInstr *instr) {
    IrOp op = instr->op;
    int fp = op == IR_FADD || op == IR_FSUB || op == IR_FMUL ||
             op == IR_FDIV || op == IR_FNEG;
    switch (op) {
    case IR_IADD: case IR_ISUB: case IR_AND: case IR_OR: case IR_XOR:
        if (a64MirAlu3(ctx, op, instr)) return;
        break;
    case IR_IMUL:
        if (a64MirMul3(ctx, instr)) return;
        break;
    case IR_INEG: case IR_NOT:
        if (a64MirUnary3(ctx, op, instr)) return;
        break;
    case IR_SHL: case IR_SHR: case IR_SAR:
        if (a64MirShift3(ctx, op, instr)) return;
        break;
    case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV:
        if (a64MirFalu3(ctx, op, instr)) return;
        break;
    default: break;
    }

    int out = a64AccOut(instr, fp);
    int src = a64SrcIn(ctx, instr->r1, fp,
                             a64LoadTo(instr, fp, out));
    switch (op) {
    case IR_IADD: case IR_ISUB: case IR_AND: case IR_OR: case IR_XOR: {
        int has_imm = instr->r2 && instr->r2->kind == IR_VAL_CONST_INT;
        if (!(has_imm && a64MirAluImm(ctx, op, instr->r2->as._i64, instr,
                                      out, src)))
            a64MirAluReg(ctx, op, instr, out, src);
        break;
    }
    case IR_IMUL:
        a64MirMul(ctx, instr, out, src);
        break;
    case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
        a64MirDivRem(ctx, op, instr, out, src);
        break;
    case IR_INEG: case IR_NOT:
        a64MirUnary(ctx, op, instr, out, src);
        break;
    case IR_SHL: case IR_SHR: case IR_SAR: {
        IrValue *r2 = instr->r2;
        if (r2 && r2->kind == IR_VAL_CONST_INT &&
            r2->as._i64 >= 0 && r2->as._i64 < 64)
            a64MirShiftImm(ctx, op, (int)r2->as._i64, out, src);
        else
            a64MirShiftReg(ctx, op, instr, out, src);
        break;
    }
    case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV:
        a64MirFbinop(ctx, op, instr, (int)irValueByteSize(instr->dst),
                     out, src);
        break;
    case IR_FNEG:
        a64MirFneg(ctx, (int)irValueByteSize(instr->dst), out, src);
        break;
    default:
        return;
    }
    if (fp) a64SpillDstFpr(ctx, instr, out);
    else    a64SpillDst(ctx, instr, out);
}

static void a64EmitInstr(A64Ctx *ctx, IrInstr *instr) {
    HccJit *jit = ctx->jit;

    switch (instr->op) {
        case IR_NOP:
        case IR_LABEL:
        case IR_ALLOCA:
        case IR_GEP:
            break;

        case IR_LOAD: {
            /* Mirrors aarch64.c: IR_LOAD names a home, so a register
             * home makes the load a read of that register. */
            if (instr->r1 && instr->r1->pinned_reg) {
                int src = a64RegFromName(instr->r1->pinned_reg->data);
                /* A float home is a `d` register: copied as one, or the
                 * `mov` reads the GPR of the same number instead. */
                if (instr->dst && irIsFloat(instr->dst->type)) {
                    int out = a64DstOut(instr, 1, A64_F0);
                    if (out != src)
                        E2(ctx, A64MN_FMOV, mir_d(out), mir_d(src));
                    a64SpillDstFpr(ctx, instr, out);
                    break;
                }
                int out = a64DstOut(instr, 0, A64_S0);
                if (out != src)
                    E2(ctx, A64MN_MOV, mir_x(out), mir_x(src));
                a64SpillDst(ctx, instr, out);
                break;
            }
            if (instr->r1 && instr->r1->loc.kind == IR_LOC_REG &&
                instr->r1->loc.as.reg)
            {
                int src = a64RegFromName(instr->r1->loc.as.reg->data);
                if (instr->dst && irIsFloat(instr->dst->type)) {
                    int out = a64DstOut(instr, 1, A64_F0);
                    if (out != src)
                        E2(ctx, A64MN_FMOV, mir_d(out), mir_d(src));
                    a64SpillDstFpr(ctx, instr, out);
                } else {
                    int out = a64DstOut(instr, 0, A64_S0);
                    if (out != src)
                        E2(ctx, A64MN_MOV, mir_x(out), mir_x(src));
                    a64SpillDst(ctx, instr, out);
                }
                break;
            }
            if (instr->dst && irIsFloat(instr->dst->type)) {
                int out = a64DstOut(instr, 1, A64_F0);
                int size = (int)instr->dst->as.var.size;
                int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
                a64FpFrameLoad(ctx, out, size, loff);
                a64SpillDstFpr(ctx, instr, out);
                break;
            }
            int out = a64DstOut(instr, 0, A64_S0);
            int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
            int size = instr->dst ? (int)instr->dst->as.var.size : 8;
            a64FrameLoad(ctx, out, size, loff);
            a64SpillDst(ctx, instr, out);
            break;
        }

        case IR_STORE: {
            a64EmitStore(ctx, instr);
            break;
        }

        case IR_LOAD_DEREF: {
            /* Global base: materialise address into x1, then load.
             *
             * Only when there is no INDEX. This path passes has_idx=0,
             * so a fused `g[i]` lost its subscript and read g[0] - the
             * disp was already rescued here once; the index is the
             * other half. An indexed access takes the general path
             * below, which knows how to fold both. */
            if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL &&
                !instr->idx) {
                a64GlobalAddr(ctx, A64_S1,
                              a64GlobalName(ctx, instr->r1->as.global.name));
                int size = instr->dst ? (int)instr->dst->as.var.size : 8;
                if (instr->dst && irIsFloat(instr->dst->type)) {
                    int out = a64DstOut(instr, 1, A64_F0);
                    E2(ctx, A64MN_LDR, a64MirFpSized((int)irValueByteSize(instr->dst), out), mir_mem(mir_x(A64_S1), instr->disp));
                    a64SpillDstFpr(ctx, instr, out);
                } else {
                    /* instr->disp, not 0 - the global fast path
                     * dropped it, so a displaced access to a global
                     * read offset zero. Mirrors the aarch64.c fix. */
                    int out = a64DstOut(instr, 0, A64_S0);
                    a64DerefLoad(ctx, size, out, A64_S1, 0, A64_S0, 0,
                                 instr->disp);
                    a64SpillDst(ctx, instr, out);
                }
                break;
            }
            /* Index home first, then a base scratch that avoids it -
             * this site loaded the base BEFORE consulting the index, so
             * it lacked even the ordering the text backend had. */
            int idx_home; int idx_homed = a64IdxHome(instr, &idx_home);
            int base;
            if (!a64BaseHome(instr->r1, instr, &base)) {
                base = a64ScratchTake(idx_homed, idx_home);
                /* A GLOBAL base has no register to load from - its
                 * address is materialised. IR_RMW_DEREF has always
                 * done this; the load and store paths only ever saw a
                 * global through their fast path above, which now
                 * declines anything with an index. */
                if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
                    a64GlobalAddr(ctx, base,
                            a64GlobalName(ctx, instr->r1->as.global.name));
                } else {
                    a64LoadToReg(ctx, instr->r1, base);
                }
            }
            int idx; int has_idx = a64IdxReg(ctx, instr, base, &idx);
            if (instr->dst && irIsFloat(instr->dst->type)) {
                int fsz = (int)irValueByteSize(instr->dst);
                int fout = a64DstOut(instr, 1, A64_F0);
                if (has_idx) {
                    /* No `[base, idx, lsl #n, #disp]` form exists, so
                     * the displacement folds into the base - what
                     * a64DerefLoad does for integers. Dropping it read
                     * from the start of the struct instead of the
                     * field: `s->d[i]` where `d` is not at offset 0.
                     * The text backend had the identical hole. */
                    if (instr->disp) a64AddSubImm(ctx, base, (s64)instr->disp);
                    int sh = (instr->scale == 8) ? 3 :
                             (instr->scale == 4) ? 2 :
                             (instr->scale == 2) ? 1 : 0;
                    E2(ctx, A64MN_LDR, a64MirFpSized(fsz, fout), a64MirRegoff(base, idx, sh ? 1 : 0, fsz));
                } else if (instr->disp != 0) {
                    if (instr->disp >= 0 && (instr->disp % fsz) == 0 &&
                        instr->disp <= 4095 * fsz) {
                        E2(ctx, A64MN_LDR, a64MirFpSized(fsz, fout), mir_mem(mir_x(base), instr->disp));
                    } else {
                        a64MovImm(ctx, A_X10, (s64)instr->disp);
                        E2(ctx, A64MN_LDR, a64MirFpSized(fsz, fout), a64MirRegoff(base, A_X10, 0, fsz));
                    }
                } else {
                    E2(ctx, A64MN_LDR, a64MirFpSized(fsz, fout), mir_mem(mir_x(base), 0));
                }
                a64SpillDstFpr(ctx, instr, fout);
                break;
            }
            int out = a64DstOut(instr, 0, A64_S0);
            int size = instr->dst ? (int)instr->dst->as.var.size : 8;
            a64DerefLoad(ctx, size, out, base, has_idx, idx,
                         instr->scale, instr->disp);
            a64SpillDst(ctx, instr, out);
            break;
        }

        case IR_STORE_DEREF: {
            /* See IR_LOAD_DEREF: no INDEX on the global fast path. */
            if (instr->dst && instr->dst->kind == IR_VAL_GLOBAL &&
                !instr->idx) {
                a64GlobalAddr(ctx, A64_S1,
                              a64GlobalName(ctx, instr->dst->as.global.name));
                int sz = (int)irValueByteSize(instr->r1);
                if (instr->r1 && irIsFloat(instr->r1->type)) {
                    a64LoadFirstSrcFpr(ctx, instr->r1);
                    E2(ctx, A64MN_STR, a64MirFpSized((int)irValueByteSize(instr->r1), A64_F0), mir_mem(mir_x(A64_S1), instr->disp));
                } else {
                    a64LoadFirstSrc(ctx, instr->r1);
                    a64DerefStore(ctx, sz, A64_S0, A64_S1, 0, A64_S0, 0,
                                  instr->disp);
                }
                break;
            }
            int idx_home; int idx_homed = a64IdxHome(instr, &idx_home);
            int base;
            if (!a64BaseHome(instr->dst, instr, &base)) {
                base = a64ScratchTake(idx_homed, idx_home);
                /* A GLOBAL base has no register to load from - its
                 * address is materialised. IR_RMW_DEREF has always
                 * done this; the load and store paths only ever saw a
                 * global through their fast path above, which now
                 * declines anything with an index. */
                if (instr->dst && instr->dst->kind == IR_VAL_GLOBAL) {
                    a64GlobalAddr(ctx, base,
                            a64GlobalName(ctx, instr->dst->as.global.name));
                } else {
                    a64LoadToReg(ctx, instr->dst, base);
                }
            }
            int idx; int has_idx = a64IdxReg(ctx, instr, base, &idx);
            if (instr->r1 && irIsFloat(instr->r1->type)) {
                int fsz = (int)irValueByteSize(instr->r1);
                AoStr *vh = a64Home(instr->r1);
                int fval = A64_F0;
                if (vh && a64IsFpRegName(vh->data))
                    fval = a64RegFromName(vh->data);
                else
                    a64LoadFirstSrcFpr(ctx, instr->r1);
                if (has_idx) {
                    /* As the load above. */
                    if (instr->disp) a64AddSubImm(ctx, base, (s64)instr->disp);
                    int sh = (instr->scale == 8) ? 3 :
                             (instr->scale == 4) ? 2 :
                             (instr->scale == 2) ? 1 : 0;
                    E2(ctx, A64MN_STR, a64MirFpSized(fsz, fval), a64MirRegoff(base, idx, sh ? 1 : 0, fsz));
                } else if (instr->disp != 0) {
                    if (instr->disp >= 0 && (instr->disp % fsz) == 0 &&
                        instr->disp <= 4095 * fsz) {
                        E2(ctx, A64MN_STR, a64MirFpSized(fsz, fval), mir_mem(mir_x(base), instr->disp));
                    } else {
                        a64MovImm(ctx, A_X10, (s64)instr->disp);
                        E2(ctx, A64MN_STR, a64MirFpSized(fsz, fval), a64MirRegoff(base, A_X10, 0, fsz));
                    }
                } else {
                    E2(ctx, A64MN_STR, a64MirFpSized(fsz, fval), mir_mem(mir_x(base), 0));
                }
                break;
            }
            int sz = (int)irValueByteSize(instr->r1);
            /* A store only READS the value, so one already in a home
             * is named in place; only slot/const values materialise,
             * into a scratch that avoids the address registers. */
            AoStr *vh = a64Home(instr->r1);
            int val;
            if (vh && !a64IsFpRegName(vh->data)) {
                val = a64RegFromName(vh->data);
            } else if (a64IsZero(instr->r1)) {
                val = A_XZR;
            } else {
                val = A64_S0;
                if (has_idx && val == idx) val = A_X9;
                if (val == base) val = A_X9;
                a64LoadToReg(ctx, instr->r1, val);
            }
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
                                  a64GlobalName(ctx,
                                          instr->dst->as.global.name));
                } else {
                    a64LoadToReg(ctx, instr->dst, base);
                }
            }
            int idx; int has_idx = a64IdxReg(ctx, instr, base, &idx);
            int sz = (int)irValueByteSize(instr->r1);
            a64DerefLoad(ctx, sz, A64_S0, base, has_idx, idx, instr->scale, instr->disp);
            IrOp rop = instr->extra.rmw_op;
            /* Fold add/sub immediates like the AOT backend. The JIT has no
             * logical-immediate encoder, so and/or/xor stay on the reg path. */
            if (instr->r1 && instr->r1->kind == IR_VAL_CONST_INT &&
                (rop == IR_IADD || rop == IR_ISUB) &&
                a64IsAddImm(instr->r1->as._i64))
            {
                s64 imm = instr->r1->as._i64;
                E3(ctx, rop == IR_IADD ? A64MN_ADD : A64MN_SUB, mir_x(A64_S0), mir_x(A64_S0), mir_imm(imm));
            } else {
                a64LoadToReg(ctx, instr->r1, A_X9);
                int mn = a64MirAluMn(rop);
                if (mn == TASM_MN_UNKNOWN)
                    loggerPanic("jit-aarch64: bad RMW op %d\n", rop);
                E3(ctx, mn, mir_x(A64_S0), mir_x(A64_S0), mir_x(A_X9));
            }
            a64DerefStore(ctx, sz, A64_S0, base, has_idx, idx, instr->scale, instr->disp);
            break;
        }

        case IR_LEA: {
            /* Compute straight into the destination's home, as the AOT
             * does. Not just to save the move: `a64SpillDst` may DROP it
             * when the result is marked dead, and computing into x0 then
             * dropping the copy leaves the home holding a stale address
             * - the previous block's, which is a valid pointer to the
             * wrong object, so it corrupts silently. Writing the home
             * directly makes the elision harmless either way. */
            int out = A64_S0;
            AoStr *home = a64Home(instr->dst);
            if (home) out = a64RegFromName(home->data);
            if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
                const char *name;
                int got = 0;
                if (instr->r1->flags & IR_VAL_FLAG_FUNC) {
                    name = asmNormaliseFunctionName(jit->cc,
                            instr->r1->as.global.name);
                    if (jit->addr_mode == HCC_ADDR_RELOCATED)
                        got = asmFunctionIsExtern(jit->cc,
                                instr->r1->as.global.name);
                } else {
                    name = a64GlobalName(ctx, instr->r1->as.global.name);
                }
                a64GlobalAddrEx(ctx, out, name, got);
            } else if (instr->r1) {
                int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
                if (loff >= 0 && a64IsAddImm(loff)) {
                    E3(ctx, A64MN_ADD, mir_x(out), mir_x(A_FP), mir_imm(loff));
                } else if (loff < 0 && a64IsAddImm(-loff)) {
                    E3(ctx, A64MN_SUB, mir_x(out), mir_x(A_FP), mir_imm(-loff));
                } else {
                    a64MovImm(ctx, A_X9, (s64)loff);
                    E3(ctx, A64MN_ADD, mir_x(out), mir_x(A_FP), mir_x(A_X9));
                }
            }
            /* A no-op when `out` is already the home. */
            a64SpillDst(ctx, instr, out);
            break;
        }

        case IR_IADD: case IR_ISUB: case IR_AND: case IR_OR: case IR_XOR:
        case IR_IMUL: case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
        case IR_INEG: case IR_NOT: case IR_SHL: case IR_SHR: case IR_SAR:
        case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV: case IR_FNEG: {
            a64EmitArith(ctx, instr);
            break;
        }

        case IR_ICMP: {
            /* Mirrors aarch64.c: `cmp` reads its operands and writes
             * only the flags, so operands already in registers can be
             * named directly rather than moved into the scratch pair. */
            AoStr *lhs_home = a64Home(instr->r1);
            int lhs = A64_S0;
            if (lhs_home) lhs = a64RegFromName(lhs_home->data);
            else          a64LoadFirstSrc(ctx, instr->r1);
            if (instr->r2 && instr->r2->kind == IR_VAL_CONST_INT &&
                a64IsAddImm(instr->r2->as._i64))
            {
                E2(ctx, A64MN_CMP, mir_x(lhs), mir_imm(instr->r2->as._i64));
            } else {
                AoStr *rhs_home = a64Home(instr->r2);
                int rhs = A64_S1;
                if (rhs_home) rhs = a64RegFromName(rhs_home->data);
                else          a64LoadToReg(ctx, instr->r2, A64_S1);
                E2(ctx, A64MN_CMP, mir_x(lhs), mir_x(rhs));
            }
            int out = a64DstOut(instr, 0, A64_S0);
            a64EmitCSet(ctx, instr->extra.cmp_kind, 0, out);
            a64SpillDst(ctx, instr, out);
            break;
        }

        case IR_FCMP: {
            int is_dbl = a64FpDbl(instr->r1);
            a64LoadFirstSrcFpr(ctx, instr->r1);
            if (instr->r2 && instr->r2->kind == IR_VAL_CONST_FLOAT &&
                ieee754_64(instr->r2->as._f64) == 0)
            {
                E2(ctx, A64MN_FCMP, a64MirFp(is_dbl, A64_F0), mir_fimm(0.0));
            } else {
                a64LoadToFpr(ctx, instr->r2, A64_F1);
                E2(ctx, A64MN_FCMP, a64MirFp(is_dbl, A64_F0), a64MirFp(is_dbl, A64_F1));
            }
            int out = a64DstOut(instr, 0, A64_S0);
            a64EmitCSet(ctx, instr->extra.cmp_kind, 1, out);
            a64SpillDst(ctx, instr, out);
            break;
        }

        case IR_CMP_BR: {
            IrCmpKind kind = instr->extra.cmp_br.cmp_kind;
            IrBlock *t = instr->extra.cmp_br.target_block;
            IrBlock *f = instr->extra.cmp_br.fallthrough_block;
            int is_float = instr->r1 && irIsFloat(instr->r1->type);
            if (is_float) {
                int is_dbl = a64FpDbl(instr->r1);
                a64LoadFirstSrcFpr(ctx, instr->r1);
                if (instr->r2 && instr->r2->kind == IR_VAL_CONST_FLOAT &&
                    ieee754_64(instr->r2->as._f64) == 0)
                {
                    E2(ctx, A64MN_FCMP, a64MirFp(is_dbl, A64_F0), mir_fimm(0.0));
                } else {
                    a64LoadToFpr(ctx, instr->r2, A64_F1);
                    E2(ctx, A64MN_FCMP, a64MirFp(is_dbl, A64_F0), a64MirFp(is_dbl, A64_F1));
                }
            } else {
                /* As IR_ICMP: `cmp` only reads, so operands with a
                 * register home are named in place instead of shuttled
                 * through the scratch pair. */
                AoStr *lh = a64Home(instr->r1);
                int lhs = A64_S0;
                if (lh) lhs = a64RegFromName(lh->data);
                else    a64LoadFirstSrc(ctx, instr->r1);
                if (instr->r2 && instr->r2->kind == IR_VAL_CONST_INT &&
                    a64IsAddImm(instr->r2->as._i64))
                {
                    E2(ctx, A64MN_CMP, mir_x(lhs), mir_imm(instr->r2->as._i64));
                } else {
                    AoStr *rh = a64Home(instr->r2);
                    int rhs = A64_S1;
                    if (rh) rhs = a64RegFromName(rh->data);
                    else    a64LoadToReg(ctx, instr->r2, A64_S1);
                    E2(ctx, A64MN_CMP, mir_x(lhs), mir_x(rhs));
                }
            }
            A64Cond cc_t = a64CcFor(kind, is_float);
            A64Cond cc_f = a64CcInvFor(kind, is_float);
            int t_ln = hccJitBlockLocalNum(jit, ctx->fn, t);
            int f_ln = hccJitBlockLocalNum(jit, ctx->fn, f);
            int t_phi = irBlockHasPhi(t);
            int f_phi = irBlockHasPhi(f);
            if (!t_phi && !f_phi) {
                if (ctx->next_block == t) {
                    a64EmitBcondLocal(ctx, cc_f, f_ln);
                } else if (ctx->next_block == f) {
                    a64EmitBcondLocal(ctx, cc_t, t_ln);
                } else {
                    a64EmitBcondLocal(ctx, cc_t, t_ln);
                    a64EmitBranchLocal(ctx, f_ln);
                }
            } else {
                int else_ln = hccJitFreshLocalNum(jit);
                a64EmitBcondLocal(ctx, cc_f, else_ln);
                a64PhiMaterialise(ctx, ctx->cur_block, t);
                a64EmitBranchLocal(ctx, t_ln);
                a64MirLocalLabel(ctx, else_ln);
                a64PhiMaterialise(ctx, ctx->cur_block, f);
                if (ctx->next_block != f) a64EmitBranchLocal(ctx, f_ln);
            }
            break;
        }

        case IR_BR: {
            IrBlock *t = instr->extra.blocks.target_block;
            IrBlock *f = instr->extra.blocks.fallthrough_block;
            AoStr *ch = a64Home(instr->dst);
            int creg = A64_S0;
            if (ch) creg = a64RegFromName(ch->data);
            else    a64LoadFirstSrc(ctx, instr->dst);
            E2(ctx, A64MN_CMP, mir_x(creg), mir_imm(0));
            int t_ln = hccJitBlockLocalNum(jit, ctx->fn, t);
            int f_ln = hccJitBlockLocalNum(jit, ctx->fn, f);
            int t_phi = irBlockHasPhi(t);
            int f_phi = irBlockHasPhi(f);
            if (!t_phi && !f_phi) {
                if (ctx->next_block == t) {
                    a64EmitBcondLocal(ctx, A_EQ, f_ln);
                } else if (ctx->next_block == f) {
                    a64EmitBcondLocal(ctx, A_NE, t_ln);
                } else {
                    a64EmitBcondLocal(ctx, A_NE, t_ln);
                    a64EmitBranchLocal(ctx, f_ln);
                }
            } else {
                int else_ln = hccJitFreshLocalNum(jit);
                a64EmitBcondLocal(ctx, A_EQ, else_ln);
                a64PhiMaterialise(ctx, ctx->cur_block, t);
                a64EmitBranchLocal(ctx, t_ln);
                a64MirLocalLabel(ctx, else_ln);
                a64PhiMaterialise(ctx, ctx->cur_block, f);
                if (ctx->next_block != f) a64EmitBranchLocal(ctx, f_ln);
            }
            break;
        }

        case IR_JMP: {
            IrBlock *target = instr->extra.blocks.target_block;
            a64PhiMaterialise(ctx, ctx->cur_block, target);
            if (target != ctx->next_block) {
                int ln = hccJitBlockLocalNum(jit, ctx->fn, target);
                a64EmitBranchLocal(ctx, ln);
            }
            break;
        }

        case IR_RET:
            if (instr->dst && instr->dst->byval_struct_type) {
                /* <=16-byte struct returned in registers: load its bytes from
                 * the return slot into v0.. (HFA) or x0,x1 (INTEGER). */
                AstType *t = instr->dst->byval_struct_type;
                int loff = irCgGetLoff(&ctx->fn->ra, instr->dst);
                int re = 0, rc = 0;
                AapcsClass rcls = astAapcsClassify(t, &re, &rc);
                if (rcls == AAPCS_HFA) {
                    for (int k = 0; k < rc; ++k)
                        a64FpFrameLoad(ctx, (int)k, re, loff + k * re);
                } else { /* INTEGER, <=16 bytes -> x0,(x1) */
                    int ngp = (t->size + 7) / 8;
                    for (int k = 0; k < ngp; ++k) {
                        int off = k * 8, rem = t->size - off;
                        a64FrameLoad(ctx, (int)k, rem >= 8 ? 8 : rem,
                                     loff + off);
                    }
                }
            } else if (instr->dst) {
                if (irIsFloat(instr->dst->type)) a64LoadToFpr(ctx, instr->dst, A_X0 /*d0*/);
                else                              a64LoadToReg(ctx, instr->dst, A_X0);
            }
            /* The epilogue label is defined straight after the last block,
             * so a ret ending that block falls through to it. */
            if (ctx->next_block != NULL ||
                ctx->cur_block->instructions->prev->value != (void *)instr)
            {
                a64EmitBranchLocal(ctx, hccJitEpilogueLocalNum(jit, ctx->fn));
            }
            break;

        case IR_TRUNC: case IR_ZEXT: case IR_SEXT:
        case IR_FPTRUNC: case IR_FPEXT: case IR_FPTOSI: case IR_FPTOUI:
        case IR_SITOFP: case IR_UITOFP:
        case IR_PTRTOINT: case IR_INTTOPTR: case IR_BITCAST: {
            a64EmitConvert(ctx, instr);
            break;
        }

        case IR_PHI:
            /* Materialised at predecessor's terminator. */
            break;

        case IR_CALL: {
            IrValue *wrap = instr->r1;
            Vec *args = wrap ? wrap->as.array.values : NULL;
            AoStr *fname = wrap ? wrap->as.array.label : NULL;
            int indirect = (fname == NULL);
            if (indirect) {
                if (!instr->r2) loggerPanic("jit-aarch64: indirect call without target\n");
                a64LoadToReg(ctx, instr->r2, A_X16);
            }
            int callee_va, named_count, force_va_stack;
            a64CallConv(jit->cc, instr, &callee_va, &named_count, &force_va_stack);
            if (a64CallUsesAapcs(instr)) {
                a64EmitCall(ctx, instr, args, fname, indirect,
                            callee_va, named_count, force_va_stack);
                break;
            }

            u64 n = args ? args->size : 0;
            u8 *is_stack = n ? calloc(n, 1) : NULL;
            a64PartitionCallArgs(is_stack, n, args, callee_va,
                                 named_count, force_va_stack);
            int stack_idx = 0;
            for (u64 i = 0; i < n; ++i) {
                if (!is_stack[i]) continue;
                IrValue *a = vecGet(IrValue *, args, i);
                uint32_t off = (uint32_t)(stack_idx * 8);
                if (irIsFloat(a->type)) {
                    a64LoadToFpr(ctx, a, A64_F0);
                    E2(ctx, A64MN_STR, a64MirFpSized((int)irValueByteSize(a), A64_F0), mir_mem(mir_sp(), (s64)off));
                } else {
                    int src = a64IntStoreSrc(ctx, a, A_X9);
                    E2(ctx, A64MN_STR, mir_x(src), mir_mem(mir_sp(), (s64)off));
                }
                stack_idx++;
            }
            int int_idx = 0, float_idx = 0;
            for (u64 i = 0; i < n; ++i) {
                if (is_stack[i]) continue;
                IrValue *a = vecGet(IrValue *, args, i);
                if (irIsFloat(a->type)) {
                    a64LoadToFpr(ctx, a, (int)float_idx++);
                } else {
                    a64LoadToReg(ctx, a, (int)int_idx++);
                }
            }
            if (indirect) {
                E1(ctx, A64MN_BLR, mir_x(A_X16));
            } else {
                char *normalised = asmNormaliseFunctionName(jit->cc, fname);
                E1(ctx, A64MN_BL, mir_sym(normalised));
            }
            if (instr->dst && instr->dst->type != IR_TYPE_VOID) {
                if (irIsFloat(instr->dst->type)) a64SpillDstFpr(ctx, instr, A_X0);
                else                              a64SpillDst   (ctx, instr, A_X0);
            }
            free(is_stack);
            break;
        }

        case IR_ASM:
            /* Inline `asm {...}` statement: libtasm-encode and splice
             * the bytes at the current position. */
            if (instr->extra.asm_fragments) {
                loggerPanic("jit-aarch64: `&var` asm fragments are not "
                            "supported; address locals directly\n");
            }
            /* The blob's bytes bypass the stream, so everything built
             * so far must be committed first. */
            a64MirFlush(ctx);
            if (ctx->text_out) {
                /* The block is HolyC dialect the system assembler
                 * cannot read - it goes out as .byte runs, exactly as
                 * the text backend emitted it. */
                aoStrRemovePreviousChar(ctx->text_out, '\t');
                if (instr->r1 && instr->r1->as.str.str) {
                    asmEmitBlockBytes(jit->cc, ctx->text_out,
                                      instr->r1->as.str.str, 0);
                }
                aoStrPutChar(ctx->text_out, '\t');
                break;
            }
            if (instr->r1 && instr->r1->as.str.str &&
                hccJitAssembleText(jit, instr->r1->as.str.str, 0) != 0)
            {
                loggerPanic("jit-aarch64: failed to assemble inline asm\n");
            }
            break;

        case IR_VA_START: {
            /* Mirrors aarch64.c. */
            if (ctx->jit->cc->target == TARGET_AARCH64_APPLE_DARWIN) {
                /* Apple: the list is the incoming argument area, past any
                 * named parameter that overflowed the registers. */
                int va_off = a64VaRegionOffset(ctx->ast);
                E2(ctx, A64MN_MOV, mir_x(A64_S0), mir_x(A_FP));
                a64AddSubImm(ctx, A64_S0, (s64)va_off);
            } else {
                /* AAPCS64 Linux: the address of the frame-resident
                 * va_list record a64EmitVaSaveArea built. */
                a64MovImm(ctx, A_X9, (s64)ctx->va_list_off);
                E3(ctx, A64MN_ADD, mir_x(A64_S0), mir_x(A_FP), mir_x(A_X9));
            }
            a64SpillDst(ctx, instr, A64_S0);
            break;
        }

        case IR_FRAME_ADDR:
            /* AAPCS64 names x29 the frame pointer, and a64EmitPrologue
             * sets it up for every function that keeps a frame -
             * a64FnUsesFrameAddr is what stops this one being elided. */
            E2(ctx, A64MN_MOV, mir_x(A64_S0), mir_x(A_FP));
            a64SpillDst(ctx, instr, A64_S0);
            break;

        case IR_SELECT:
        case IR_SWITCH:
        case IR_VA_ARG:
        case IR_VA_END:
            loggerPanic("jit-aarch64: op %d not yet implemented\n", instr->op);
        default:
            loggerPanic("jit-aarch64: unknown op %d\n", instr->op);
    }
}

/* The largest below-sp area any call in `fn` needs. Reserved once by
 * the prologue rather than by a `sub sp`/`add sp` pair per call. */
static int a64OutgoingAreaBytes(HccJit *jit, IrFunction *fn) {
    int max = 0;
    listForEach(fn->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        listForEach(block->instructions) {
            IrInstr *in = (IrInstr *)it->value;
            if (in->op != IR_CALL) continue;
            Vec *args = in->r1 ? in->r1->as.array.values : NULL;
            int va, named, force;
            a64CallConv(jit->cc, in, &va, &named, &force);
            int bytes;
            if (a64CallUsesAapcs(in)) {
                bytes = a64AapcsCallBytes(in, args, va, named, force,
                                          NULL, NULL, NULL);
            } else {
                u64 n = args ? args->size : 0;
                u8 *is_stack = n ? calloc(n, 1) : NULL;
                s32 n_stack = a64PartitionCallArgs(is_stack, n, args, va,
                                                   named, force);
                free(is_stack);
                bytes = (n_stack * 8 + 15) & ~15;
            }
            if (bytes > max) max = bytes;
        }
    }
    return max;
}

/* ---------------- prologue / epilogue ---------------- */

/* Callee saves sit at the bottom of the frame, sp-relative, just above
 * the outgoing-argument area. */
static int a64SaveAreaBytes(IrFunction *fn) {
    Vec *saved = irCgPinnedCalleeSaved(fn);
    int bytes = (int)saved->size * 8;
    vecRelease(saved);
    return (bytes + 15) & ~15;
}

static void a64EmitSaveRestore(A64Ctx *ctx, int store) {
    Vec *saved = irCgPinnedCalleeSaved(ctx->fn);
    int mn = store ? A64MN_STR : A64MN_LDR;
    for (u64 i = 0; i < saved->size; ++i) {
        AoStr *r = (AoStr *)saved->entries[i];
        int reg = a64RegFromName(r->data);
        s64 off = (s64)(ctx->out_area + i * 8);
        AsmOperand rt = a64IsFpRegName(r->data) ? mir_d(reg) : mir_x(reg);
        E2(ctx, mn, rt, mir_mem(mir_sp(), off));
    }
    vecRelease(saved);
}

/* Spill the argument registers into the save area and build the
 * frame-resident va_list record. Runs at the top of a variadic function
 * that reads a C list, before the parameter prologue reads an incoming
 * register. The byte-for-byte twin of aarch64.c's aarch64EmitVaSaveArea:
 * x9 = save base, x10 = record (32 bytes below it), x11 = scratch. */
static void a64EmitVaSaveArea(A64Ctx *ctx) {
    MirFn *m = ctx->mir;

    /* x9 = save base = x29 + va_save_off. The offset is materialised and
     * added signed; a64AddSubImm's fallback would mis-sign a large
     * negative displacement. */
    a64MovImm(ctx, A_X9, (s64)ctx->va_save_off);
    a64MirLine(ctx, mir3(m, A64MN_ADD, mir_x(A_X9), mir_x(A_FP),
                         mir_x(A_X9)));
    for (int i = 0; i < 8; ++i) {
        a64MirLine(ctx, mir2(m, A64MN_STR, mir_x(A_X0 + i),
                             mir_mem(mir_x(A_X9), i * 8)));
    }
    for (int i = 0; i < 8; ++i) {
        a64MirLine(ctx, mir2(m, A64MN_STR, mir_q(i),
                             mir_mem(mir_x(A_X9), 64 + i * 16)));
    }

    int ngrn, nsrn, nsaa;
    a64VaNamedCounts(ctx->ast, &ngrn, &nsrn, &nsaa);

    /* x10 = record base, exactly A64_VA_LIST_BYTES below the save area. */
    a64MirLine(ctx, mir3(m, A64MN_SUB, mir_x(A_X10), mir_x(A_X9),
                         mir_imm(A64_VA_LIST_BYTES)));
    /* __stack = x29 + 16 + nsaa */
    a64MirLine(ctx, mir3(m, A64MN_ADD, mir_x(A_X11), mir_x(A_FP),
                         mir_imm(16 + nsaa)));
    a64MirLine(ctx, mir2(m, A64MN_STR, mir_x(A_X11), mir_mem(mir_x(A_X10), 0)));
    /* __gr_top = save base + 64 */
    a64MirLine(ctx, mir3(m, A64MN_ADD, mir_x(A_X11), mir_x(A_X9),
                         mir_imm(64)));
    a64MirLine(ctx, mir2(m, A64MN_STR, mir_x(A_X11), mir_mem(mir_x(A_X10), 8)));
    /* __vr_top = save base + 192 */
    a64MirLine(ctx, mir3(m, A64MN_ADD, mir_x(A_X11), mir_x(A_X9),
                         mir_imm(192)));
    a64MirLine(ctx, mir2(m, A64MN_STR, mir_x(A_X11), mir_mem(mir_x(A_X10), 16)));
    /* __gr_offs = -(8 - ngrn) * 8, __vr_offs = -(8 - nsrn) * 16 (32-bit) */
    a64MovImm(ctx, A_X11, (s64)(-(8 - ngrn) * 8));
    a64MirLine(ctx, mir2(m, A64MN_STR, mir_w(A_X11), mir_mem(mir_x(A_X10), 24)));
    a64MovImm(ctx, A_X11, (s64)(-(8 - nsrn) * 16));
    a64MirLine(ctx, mir2(m, A64MN_STR, mir_w(A_X11), mir_mem(mir_x(A_X10), 28)));
}

static void a64EmitPrologue(A64Ctx *ctx) {
    if (ctx->omit_frame) return;
    E3(ctx, A64MN_STP, mir_x(A_FP), mir_x(A_LR), mir_mem_pre(mir_sp(), -16));
    /* `mov x29, sp` - the dispatch encodes it as the ADD-imm alias,
     * because Xm=31 decodes as XZR in the ORR form. */
    E2(ctx, A64MN_MOV, mir_x(A_FP), mir_sp());
    uint32_t aligned = ((uint32_t)ctx->fn->stack_space + 15u) & ~15u;
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
    E3(ctx, A64MN_LDP, mir_x(A_FP), mir_x(A_LR), mir_mem_post(mir_sp(), 16));
    E0(ctx, A64MN_RET);
}

/* ---------------- per-function compilation ---------------- */

/* Emit one ALREADY-LOWERED-AND-PREPPED function. `text_out` selects
 * the sink: NULL encodes bytes into jit->enc (the JIT), a buffer gets
 * GNU-as text appended (the AOT path, sharing this selector). */
static int a64EmitFunctionBody(HccJit *jit, Ast *ast, IrFunction *fn,
                               AoStr *text_out) {
    int is_variadic = (ast->type && ast->type->has_var_args) || ast->has_var_args;
    /* A by-value struct param is unpacked into a frame slot relative to
     * x29 by a64EmitParamPrologue, so the frame must be set up even
     * if the body looks trivial. */
    int has_struct_param = 0;
    if (ast->params) {
        for (u64 pi = 0; pi < ast->params->size; ++pi) {
            Ast *p = vecGet(Ast *, ast->params, pi);
            if (a64IsByvalStruct(p->type)) { has_struct_param = 1; break; }
        }
    }
    /* AAPCS64 Linux passes C varargs in registers; a function that reads
     * them spills the argument registers to a save area and builds a
     * va_list record below the (16-aligned) locals and above the
     * callee-saved area. Apple AArch64 passes them on the stack. Mirrors
     * aarch64.c's aarch64GenerateFunction exactly. */
    int uses_va_start =
        (jit->cc->target != TARGET_AARCH64_APPLE_DARWIN) &&
        a64FnUsesVaStart(fn);
    int va_save_off = 0, va_list_off = 0;
    if (uses_va_start) {
        unsigned locals_aligned = ((unsigned)fn->stack_space + 15u) & ~15u;
        va_save_off = -(int)(locals_aligned + A64_VA_SAVE_BYTES);
        va_list_off =
            -(int)(locals_aligned + A64_VA_SAVE_BYTES + A64_VA_LIST_BYTES);
        fn->stack_space = (u16)(locals_aligned + a64VaExtraBytes());
    }
    int out_area = a64OutgoingAreaBytes(jit, fn);
    fn->stack_space = (u16)(fn->stack_space + a64SaveAreaBytes(fn) + out_area);
    int omit_frame = (fn->stack_space == 0) && !irFnHasCalls(fn) &&
                     (ast->loff == 0) && !is_variadic && !has_struct_param &&
                     !irFnHasOp(fn, IR_FRAME_ADDR);

    A64Ctx ctx = {0};
    ctx.jit = jit;
    ctx.fn = fn;
    ctx.ast = ast;
    ctx.omit_frame = omit_frame;
    ctx.out_area = out_area;
    ctx.uses_va_start = uses_va_start;
    ctx.va_save_off = va_save_off;
    ctx.va_list_off = va_list_off;

    /* Publish the function entry as a public label. asmNormaliseFunctionName
     * strips/adds underscores per platform; the resolver uses the same name. */
    ctx.text_out = text_out;
    /* The function's public label is the stream's first line; each
     * sink materialises it (asm_define_label / `name:`). */
    char *fname = asmNormaliseFunctionName(jit->cc, ast->fname);
    ctx.mir = mir_fn_new(fname, TASM_ARCH_ARM64);

    a64EmitPrologue(&ctx);
    if (ctx.uses_va_start) a64EmitVaSaveArea(&ctx);
    a64EmitParamPrologue(&ctx, ctx.ast, ctx.fn);

    Set *referenced = irCgComputeReferencedBlocks(fn);

    listForEach(fn->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        ctx.cur_block = block;
        ctx.next_block = (it->next != fn->blocks)
                       ? (IrBlock *)it->next->value : NULL;
        if (setHas(referenced, (void *)(u64)block->id)) {
            int ln = hccJitBlockLocalNum(jit, fn, block);
            a64MirLocalLabel(&ctx, ln);
        }
        listForEach(block->instructions) {
            IrInstr *in = (IrInstr *)it->value;
            ctx.cur_line = in->line;
            a64EmitInstr(&ctx, in);
        }
    }
    setRelease(referenced);

    /* Epilogue label + bytes. */
    a64MirLocalLabel(&ctx, hccJitEpilogueLocalNum(jit, fn));
    a64EmitEpilogue(&ctx);
    a64MirFlush(&ctx);

    if (a64MirDump()) mir_fn_print(ctx.mir, stderr);
    mir_fn_free(ctx.mir);

    return 0;
}

static int a64CompileFunction(HccJit *jit, Ast *ast, IrCtx *ir_ctx) {
    IrFunction *fn = irLowerFunction(ir_ctx, ast);
    irBasicFunctionOptimisations(fn);
    if (irInlineFunction(ir_ctx, fn)) irBasicFunctionOptimisations(fn);

    /* Layout passes (slot offsets, IR-CG ctx). The text path goes
     * through irFunctionPrepForCodeGen; same here so the regalloc loff
     * map is populated before we ask for offsets. */
    IrCgCtx dummy = {0};
    dummy.cc = jit->cc;
    dummy.fn = fn;
    irFunctionPrepForCodeGen(&dummy, fn, ast);

    return a64EmitFunctionBody(jit, ast, fn, NULL);
}

/* ---------------- public API ---------------- */

static int a64TargetOk(enum CliTarget target) {
    return target == TARGET_AARCH64_APPLE_DARWIN ||
           target == TARGET_AARCH64_UNKNOWN_LINUX_GNU;
}

static const HccJitBackend aarch64_jit_backend = {
    .name = "aarch64",
    .target_ok = a64TargetOk,
    .init_reg_pool = aarch64InitRegPool,
    .compile_function = a64CompileFunction,
};

/* AOT text generation through the SAME selector as the JIT: the driver
 * (asm.c) has already lowered, optimised and prepped ctx->fn; this
 * runs a64EmitFunctionBody with the text sink so every instruction is
 * built as MIR and printed by the machine layer's GNU-as formatter.
 * One HccJit context for the whole file keeps the .Ljag label numbers
 * unique across functions. */
void aarch64MirGenerateFunction(IrCgCtx *cgctx, Ast *ast) {
    static HccJit *aot = NULL;
    if (!aot) {
        aot = hccJitNewEx(cgctx->cc, &aarch64_jit_backend, 0);
        if (!aot) {
            loggerPanic("hcc: cannot initialise aarch64 MIR backend\n");
        }
        /* Everything is symbolic for an external assembler or object
         * writer; the selector keys its addressing decisions off this,
         * never off the sink. */
        aot->addr_mode = HCC_ADDR_RELOCATED;
    }
    aoStrCatFmt(cgctx->buf,
                ".text\n\t.p2align 2\n");
    /* `static` means internal linkage: exporting it would let two
     * translation units collide on a name each meant to keep private.
     * No `.globl` is what makes the symbol local. */
    if (!(ast->flags & AST_FLAG_STATIC_FN)) {
        char *__sym = asmNormaliseFunctionName(cgctx->cc, ast->fname);
        asmSpangleNote(cgctx->cc, cgctx->buf, __sym);
        aoStrCatFmt(cgctx->buf, "\t.globl %s\n", __sym);
    }
    a64EmitFunctionBody(aot, ast, cgctx->fn, cgctx->buf);
    aoStrPutChar(cgctx->buf, '\n');
}

HccJit *aarch64JitCompile(Cctrl *cc) {
    return hccJitCompile(cc, &aarch64_jit_backend);
}

const HccJitBackend *aarch64JitBackend(void) {
    return &aarch64_jit_backend;
}

/* ================================================================
 * Target description + file-format emission (formerly aarch64.c):
 * the register pool and clobber model the selector allocates
 * against, and everything that surrounds function bodies in an
 * assembly file - data sections, `asm {}` blocks, globals.
 * ================================================================
 *
 AArch64 codegen, mirroring src/x86_64.c. AAPCS64 ABI with Apple
 * Darwin variations (variadic args on stack; PC-relative globals
 * via `adrp` + `@PAGE`/`@PAGEOFF`).
 *
 * Register usage matches the x86 backend's role assignment so the
 * IR layer's `loc=REG` semantics transfer. This is simple however probably
 * not the optimal strategy 😅.
 *   x11 - scratch 0 / accumulator      (A64_S0; analogous to %rax)
 *   x12 - scratch 1                    (A64_S1; %rcx)
 *   x17 - scratch 2 / index            (A64_S2; %rdx)
 *   d20 - float scratch 0              (A64_F0; %xmm0)
 *   d21 - float scratch 1              (A64_F1; %xmm1)
 *   x0/d0 - ABI return; x0-x7/d0-d7 ABI arguments only
 *   x9  - pinned-reg save area pivot   (caller-saved, free)
 *   x29 - frame pointer
 *   x30 - link register
 *   sp  - stack pointer
 *
 * Int arg regs: x0-x7. Float arg regs: d0-d7. */

static const char *const kIntRegs64[] = {
    "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"
};
static const char *const kFloatRegs[] = {
    "d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7"
};

/* Permanent, NOT arena-backed: the pool this fills is built once and
   read on every later compile, and the arenas are reclaimed between
   parses. See aoStrDupRawPermanent. */
static Vec *aarch64MakeAoStrVec(const char *const *src, int n) {
    Vec *v = vecNew(&vec_aostr_type);
    for (int i = 0; i < n; ++i) {
        vecPush(v, aoStrDupRaw((char *)src[i], strlen(src[i])));
    }
    return v;
}

/* Which scratch register (x11/x12/x17/d20/d21) does emitting `I`
 * actually write, by the point `at`? Mirrors a64EmitArith's shape: r1
 * is materialised into scratch 0, then r2 into scratch 1, and the
 * result is computed in scratch 0 unless the destination has a home.
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
    int is_scratch0 = !strcmp(r, "x11") || !strcmp(r, "d20");

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
            /* r1 has landed in scratch 0 by now; scratch 1/2 are
             * still untouched, so a source living there survives. */
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
            /* Audited against a64EmitConvert's emission: a conversion
             * either runs register-to-register between homes (conv3 -
             * and a home is never a scratch register) or materialises
             * through scratch 0, converting there, spilling out of it.
             * Either way scratch 1/2 are untouched, so a parameter waiting in one
             * survives - which is what lets BOTH parameters of
             * `char add(char a, char b)` forward past each other's
             * canonicalising extends. */
            switch (I->op) {
            case IR_TRUNC: case IR_ZEXT: case IR_SEXT:
            case IR_FPTRUNC: case IR_FPEXT: case IR_FPTOSI:
            case IR_FPTOUI: case IR_SITOFP: case IR_UITOFP:
            case IR_PTRTOINT: case IR_INTTOPTR: case IR_BITCAST:
                return is_scratch0;
            default:
                break;
            }
            return 1;
    }
}

void aarch64InitRegPool(void) {
    static int initialised = 0;
    static IrRegPool pool;
    /* Already built: INSTALL it and return. The early return used to
     * skip the install too, which made this a no-op as a setter - so
     * with both backends in play the pool stayed with whoever ran last.
     * `#eval` under `--target` is exactly that case: the driver
     * installs the target's pool, the block is compiled for the HOST,
     * and the two disagreed about whether variadic arguments go in
     * registers or on the stack. Every `Emit("%d", x)` in a
     * cross-compiled block printed garbage. */
    if (initialised) return;

    /* Must match A64_S0..S2 / A64_F0..F1. */
    static const char *kScratchRegs[] = { "x11", "x12", "x17", "d20", "d21" };
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
    pool.int_result_reg   = aoStrDupRaw((char *)"x11", 3);
    pool.float_result_reg = aoStrDupRaw((char *)"d20", 3);
    pool.sret_reg         = aoStrDupRaw((char *)"x8", 2);
    pool.scratch_regs     = aarch64MakeAoStrVec(kScratchRegs, n_scratch);
    /* Caller-saved registers nothing else in the backend touches, so
     * a value that does not cross a call lives in one for free.
     *
     *   x13-x15  never emitted by anything else
     *   x0-x7    argument registers. The allocators carry the two
     *            hazards these introduce (a param ARRIVING in one, a
     *            value passed as an argument while one is staged) -
     *            see irRegIsAbiArg / irRegHoldsReadParam /
     *            irCollectCallArgValues. Listed last, high to low, so
     *            the hazard-free registers are preferred and the
     *            least-often-occupied argument slots go first.
     *   d16-d18  caller-saved FP temporaries; same bargain for floats
     *
     * x11/x12/x17 and d20/d21 are the emitter's scratch (A64_S0-S2, A64_F0-F1;
     * x11 also builds the va_list in the prologue), x8 sret, x9/x10
     * offset+addressing pivots, x16 the indirect-call veneer, x18 the
     * platform register. */
    static const char *kLeafFree[] = { "x13","x14","x15",
                                       "x7","x6","x5","x4","x3",
                                       "x2","x1","x0",
                                       "d16","d17","d18" };
    pool.leaf_free_regs = aarch64MakeAoStrVec(kLeafFree,
            (int)(sizeof(kLeafFree) / sizeof(kLeafFree[0])));
    pool.callee_saved_regs = aarch64MakeAoStrVec(kCalleeSaved,
            (int)(sizeof(kCalleeSaved) / sizeof(kCalleeSaved[0])));
    pool.op_clobbers      = aarch64OpClobbers;
    pool.call_arg_reg     = a64CallArgReg;
    pool.variadic_on_stack = 1;

    irRegPoolSet(&pool);
    initialised = 1;
}

/* The index register for a `[base, idx, lsl #n]` addressing mode.
 *
 * `base_scratch` is the register the caller is about to load the base
 * into, or NULL when the base already lives in one. An index whose home
 * IS that register has to be moved out of the way first: x1 is the
 * second integer parameter's home, so `p->data[i]` loaded the base over
 * the top of `i` and then used one register for both -
 * `ldrsw x0, [x1, x1, lsl #2]`. Callers must resolve the index BEFORE
 * loading the base, which is what makes the copy land in time. */
/* The register a value lives in, or NULL when it lives in memory. */
static const char *a64HomeReg(IrValue *v) {
    if (!v) return NULL;
    if (v->pinned_reg) return v->pinned_reg->data;
    if (v->loc.kind == IR_LOC_REG && v->loc.as.reg) return v->loc.as.reg->data;
    return NULL;
}

#define kMaxPhisArm 32
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

/* `argc` / `argv`, with or without the platform's leading underscore. */
static int asmLabelIsArgcArgv(AoStr *label) {
    char *p = label->data;
    s64 n = label->len;
    if (n && *p == '_') { p++; n--; }
    return n == 4 && (!memcmp(p, "argc", 4) || !memcmp(p, "argv", 4));
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
        asmSpangleNote(cc, buf, label->data);
        aoStrCatFmt(buf, ".globl %S\n\t", label);
        aoStrCatFmt(buf, ".zerofill __DATA,__common,%S,%i,3\n", label, size);
    } else {
        asmSpangleNote(cc, buf, label->data);
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
    {
        /* `&Fn`, `Fn`, or either with a CAST around it. */
        Ast *addr = astInitAddressTarget(data);
        if (addr) data = addr;
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
        /* In OFFSET order, one entry per offset - see
         * astAggregateInitOrder. Walking the list positionally put a
         * union member wherever the cursor had got to, because `.space`
         * only ever moves forward. */
        u64 n_slots = 0;
        AstInitSlot *slots = astAggregateInitOrder(data->type,
                                                  data->arrayinit, &n_slots);
        for (u64 si = 0; si < n_slots; ++si) {
            int off = slots[si].offset;
            if (off > wrote) {
                aoStrCatPrintf(buf, ".space  %d\n\t", off - wrote);
                wrote = off;
            }
            wrote += aarch64DataInternal(cc, buf, slots[si].value);
        }
        free(slots);
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
    /* Dedup on the LABEL, not the source name: two function-scope
     * statics can both be called `cmd` but they are distinct objects
     * with distinct `.L<n>` labels - keying on the name silently
     * dropped the second one's storage and the link failed on its
     * references. Same-name redeclarations of a true global share the
     * label, so those still collapse. */
    (void)varname;
    if (setHasLen(seen_globals, label->data, label->len)) return;
    aoStrRemovePreviousChar(buf, '\t');
    setAdd(seen_globals, label->data);

    /* AST_UNOP is `&thing` - a relocation to another global's label.
     * aarch64DataInternal has always known how to write one (it is how
     * a function pointer inside an aggregate is emitted); it was just
     * never reached, because a bare `&g` initialiser is not one of the
     * three kinds this admitted, and fell through to the uninitialised
     * branch below. */
    if (declinit && (declinit->kind == AST_LITERAL ||
                     declinit->kind == AST_STRING ||
                     declinit->kind == AST_ARRAY_INIT ||
                     astIsUnOpKind(declinit, AST_UN_OP_ADDR_OF)))
    {
        if (declinit->kind == AST_STRING) {
            /* `U8 *p = "..."`: the global is a *pointer* to the string
             * literal (emitted separately in the cstring section), not
             * inline storage. `U8 buf[] = "..."` keeps the bytes inline. */
            if (declvar->type && declvar->type->kind == AST_TYPE_POINTER) {
                if (!declvar->is_static) {
                    asmSpangleNote(cc, buf, label->data);
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
            asmSpangleNote(cc, buf, label->data);
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
        } else if (asmLabelIsArgcArgv(label)) {
            /* Only the unit holding the entry point DEFINES these;
             * everywhere else they stay undefined so the linker binds
             * them to that single definition. Two definitions collided
             * and made linking any two HolyC objects impossible. */
            if (!cctrlUnitHasEntryPoint(cc)) return;
            aarch64InitialiseEmptyGlobal(cc, buf, declvar, 1);
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
