/* AArch64 codegen, mirroring src/x86_64.c. AAPCS64 ABI with Apple
 * Darwin variations (variadic args on stack; PC-relative globals
 * via `adrp` + `@PAGE`/`@PAGEOFF`).
 *
 * Register usage matches the x86 backend's role assignment so the
 * IR layer's `loc=REG` semantics transfer. This is simple however probably
 * not the optimal strategy 😅.
 *   x0  - int result reg / scratch 0   (analogous to %rax)
 *   x1  - scratch 1                    (%rcx)
 *   x2  - scratch 2 / base addr        (%rdx - repurposed for SIB-ish)
 *   d0  - float result / scratch       (%xmm0)
 *   d1  - float scratch                (%xmm1)
 *   x9  - pinned-reg save area pivot   (caller-saved, free)
 *   x29 - frame pointer
 *   x30 - link register
 *   sp  - stack pointer
 *
 * Int arg regs: x0-x7. Float arg regs: d0-d7. */
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aarch64.h"
#include "aarch64-emit.h"
#include "aostr.h"
#include "ast.h"
#include "asm.h"
#include "cctrl.h"
#include "cli.h"
#include "config.h"
#include "containers.h"
#include "ir.h"
#include "ir-debug.h"
#include "ir-optimise.h"
#include "ir-regalloc.h"
#include "list.h"
#include "util.h"
#include "version.h"

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

static void aarch64InitRegPool(void) {
    static int initialised = 0;
    static IrRegPool pool;
    if (initialised) return;

    static const char *kScratchRegs[] = { "x0", "x1", "x2", "d0", "d1" };
    int n_int     = (int)(sizeof(kIntRegs64)   / sizeof(kIntRegs64[0]));
    int n_float   = (int)(sizeof(kFloatRegs)   / sizeof(kFloatRegs[0]));
    int n_scratch = (int)(sizeof(kScratchRegs) / sizeof(kScratchRegs[0]));
    pool.int_arg_regs     = aarch64MakeAoStrVec(kIntRegs64,   n_int);
    pool.float_arg_regs   = aarch64MakeAoStrVec(kFloatRegs,   n_float);
    pool.int_return_reg   = aoStrDupRaw((char *)"x0", 2);
    pool.float_return_reg = aoStrDupRaw((char *)"d0", 2);
    pool.scratch_regs     = aarch64MakeAoStrVec(kScratchRegs, n_scratch);
    pool.variadic_on_stack = 1;

    irRegPoolSet(&pool);
    initialised = 1;
}

/* Map an x-register name (e.g. "x0") to its w-variant ("w0") for a
 * 4-byte access, or back to x for 8-byte. AArch64's 32-bit ops
 * automatically zero the upper 32 bits of the matching x register. */
static void aarch64RegForWidth(const char *xreg, int size,
                               char *out, u64 out_sz)
{
    if (size >= 8) {
        snprintf(out, out_sz, "%s", xreg);
        return;
    }
    /* xN -> wN. Last char of xreg is the digit run; replace the
     * leading 'x' with 'w'. */
    if (xreg[0] == 'x') {
        snprintf(out, out_sz, "w%s", xreg + 1);
    } else {
        snprintf(out, out_sz, "%s", xreg);
    }
}

/* Adjust an FP register name (dN) to the right precision for `size`.
 * 8 bytes -> dN (double), 4 bytes -> sN (float). */
static void aarch64FpRegForWidth(const char *dreg, int size,
                                 char *out, u64 out_sz)
{
    if (size <= 4 && dreg[0] == 'd') {
        snprintf(out, out_sz, "s%s", dreg + 1);
    } else {
        snprintf(out, out_sz, "%s", dreg);
    }
}

/* FP scalar register name for index `n` at the given byte width:
 * 4 -> sN (single), otherwise dN (double). Used for the float ops
 * (fadd/fcmp/fcvt/...) whose precision is encoded in the register. */
static const char *aarch64Fpr(int size, int n) {
    static const char *d[] = { "d0", "d1", "d2", "d3" };
    static const char *s[] = { "s0", "s1", "s2", "s3" };
    return (size == 4) ? s[n] : d[n];
}

/* Materialise an arbitrary 64-bit immediate into `reg`. Small
 * (<=16-bit unsigned or movz-shifted) values use a single mov;
 * larger values fall through to a movz/movk sequence. */
static void aarch64EmitMovImm(AoStr *buf, const char *reg, s64 imm) {
    u64 u = (u64)imm;
    /* Fits in 16 bits unsigned -> single movz. */
    if (u <= 0xFFFFULL) {
        aoStrCatFmt(buf, "mov %s, #%I\n\t", reg, imm);
        return;
    }
    /* Fits in a single negative-imm pattern: -1..-65536 -> movn. */
    if (imm < 0 && imm >= -65536) {
        u64 n = (u64)(~imm) & 0xFFFFULL;
        aoStrCatPrintf(buf, "movn    %s, #0x%lX\n\t", reg, (long unsigned)n);
        return;
    }
    /* General: movz lo16; movk shifts. */
    u64 w0 = u & 0xFFFFULL;
    u64 w1 = (u >> 16) & 0xFFFFULL;
    u64 w2 = (u >> 32) & 0xFFFFULL;
    u64 w3 = (u >> 48) & 0xFFFFULL;
    int emitted = 0;
    if (w0 || (!w1 && !w2 && !w3)) {
        aoStrCatPrintf(buf, "movz    %s, #0x%lX\n\t", reg, (long unsigned)w0);
        emitted = 1;
    }
    if (w1) {
        aoStrCatPrintf(buf, "%s    %s, #0x%lX, lsl #16\n\t",
                       emitted ? "movk" : "movz", reg, (long unsigned)w1);
        emitted = 1;
    }
    if (w2) {
        aoStrCatPrintf(buf, "%s    %s, #0x%lX, lsl #32\n\t",
                       emitted ? "movk" : "movz", reg, (long unsigned)w2);
        emitted = 1;
    }
    if (w3) {
        aoStrCatPrintf(buf, "%s    %s, #0x%lX, lsl #48\n\t",
                       emitted ? "movk" : "movz", reg, (long unsigned)w3);
    }
}

/* PC-relative address of a global symbol into `reg`. Apple Darwin
 * uses `adrp X, sym@PAGE; add X, X, sym@PAGEOFF`. Linux is the
 * same form (`:lo12:sym` instead of `@PAGEOFF`). */
static void aarch64GlobalAddr(Cctrl *cc, AoStr *buf, const char *sym,
                              const char *reg)
{
    if (cc->target == TARGET_AARCH64_APPLE_DARWIN) {
        aoStrCatFmt(buf, "adrp    %s, %s@PAGE\n\t", reg, sym);
        aoStrCatFmt(buf, "add %s, %s, %s@PAGEOFF\n\t", reg, reg, sym);
    } else {
        aoStrCatFmt(buf, "adrp    %s, %s\n\t", reg, sym);
        aoStrCatFmt(buf, "add %s, %s, :lo12:%s\n\t", reg, reg, sym);
    }
}

/* Pick the addressing form for a frame access. AArch64 ldr/str
 * scaled-imm wants non-negative offsets in size-multiples; ldur/stur
 * supports signed -256..255; anything else needs the offset
 * materialised into a scratch register. Returns 1 if the caller
 * should use the bracketed form below; the operand string is
 * written into `out`. */
static int aarch64FrameOperand(AoStr *buf, int loff, u32 size,
                               char *out, u64 out_sz)
{
    /* In-range unscaled form: ldur/stur accepts signed 9-bit. */
    if (loff >= -256 && loff <= 255) {
        snprintf(out, out_sz, "[x29, #%d]", loff);
        return 0;  /* signal: use ldur/stur */
    }
    /* In-range scaled form for non-negative offsets that match the
     * access size. ldr 8-byte: imm in [0, 32760] step 8. */
    if (loff >= 0) {
        int step;
        switch (size) {
            case 1: step = 1; break;
            case 2: step = 2; break;
            case 4: step = 4; break;
            default: step = 8; break;
        }
        int max = step * 4095;
        if ((loff % step) == 0 && loff <= max) {
            snprintf(out, out_sz, "[x29, #%d]", loff);
            return 1;  /* signal: use ldr/str */
        }
    }
    /* Materialise the offset into x9 (scratch) and use the register
     * form. The caller emits the access through `[x29, x9]`. */
    aarch64EmitMovImm(buf, "x9", (s64)loff);
    snprintf(out, out_sz, "[x29, x9]");
    return 2;  /* signal: register-offset, use ldr/str */
}

static void aarch64FrameLoad(AoStr *buf, const char *reg, u32 size,
                             int loff)
{
    /* 4-byte load sign-extends to 64-bit to match x86's movslq (HolyC
     * I32 is signed comparisons that promote to 64-bit need the
     * upper bits to be the sign). Sub-word (1/2) zero-extend, matching
     * x86's movzbq/movzwq. */
    char dst[8];
    aarch64RegForWidth(reg, (size == 4) ? 8 : (int)(size <= 4 ? 4 : 8),
                       dst, sizeof(dst));
    char mem[32];
    int form = aarch64FrameOperand(buf, loff, size, mem, sizeof(mem));
    const char *op;
    switch (size) {
        case 1: op = (form == 0) ? "ldurb" : "ldrb"; break;
        case 2: op = (form == 0) ? "ldurh" : "ldrh"; break;
        case 4: op = (form == 0) ? "ldursw" : "ldrsw"; break;
        default: op = (form == 0) ? "ldur" : "ldr";  break;
    }
    aoStrCatFmt(buf, "%s %s, %s\n\t", op, dst, mem);
}

static void aarch64FrameStore(AoStr *buf, const char *reg, u32 size,
                              int loff)
{
    char wreg[8];
    aarch64RegForWidth(reg, (int)(size <= 4 ? 4 : 8), wreg, sizeof(wreg));
    char mem[32];
    int form = aarch64FrameOperand(buf, loff, size, mem, sizeof(mem));
    const char *op;
    switch (size) {
        case 1: op = (form == 0) ? "sturb" : "strb"; break;
        case 2: op = (form == 0) ? "sturh" : "strh"; break;
        case 4: op = (form == 0) ? "stur"  : "str";  break;
        default: op = (form == 0) ? "stur" : "str";  break;
    }
    aoStrCatFmt(buf, "%s %s, %s\n\t", op, wreg, mem);
}

static void aarch64FpFrameLoad(AoStr *buf, const char *dreg, u32 size,
                               int loff)
{
    char fpr[8];
    aarch64FpRegForWidth(dreg, (int)size, fpr, sizeof(fpr));
    char mem[32];
    int form = aarch64FrameOperand(buf, loff, size, mem, sizeof(mem));
    const char *op = (form == 0) ? "ldur" : "ldr";
    aoStrCatFmt(buf, "%s %s, %s\n\t", op, fpr, mem);
}

static void aarch64FpFrameStore(AoStr *buf, const char *dreg, u32 size,
                                int loff)
{
    char fpr[8];
    aarch64FpRegForWidth(dreg, (int)size, fpr, sizeof(fpr));
    char mem[32];
    int form = aarch64FrameOperand(buf, loff, size, mem, sizeof(mem));
    const char *op = (form == 0) ? "stur" : "str";
    aoStrCatFmt(buf, "%s %s, %s\n\t", op, fpr, mem);
}

/* Emit `op dst, src, #v` (op is "add"/"sub", v its magnitude, >= 0)
 * for any v: one instruction when the immediate encodes, the
 * hi-(lsl #12)+lo two-instruction split up to 16MB, and a
 * materialise + register form beyond that. The register form is
 * legal for sp operands too (extended-register encoding). Only the
 * >16MB path clobbers x9, so src == x9 is safe for real frames. */
static void aarch64EmitAddSubImm(AoStr *buf, const char *op,
                                 const char *dst, const char *src, s64 v)
{
    if (v < 0) {
        loggerPanic("ir-cg-aarch64: %s immediate must be a magnitude, "
                    "got %lld\n", op, (long long)v);
    }
    if (v <= 0xFFF) {
        aoStrCatFmt(buf, "%s %s, %s, #%I\n\t", op, dst, src, v);
        return;
    }
    if ((v >> 12) <= 0xFFF) {
        aoStrCatFmt(buf, "%s %s, %s, #%I, lsl #12\n\t",
                    op, dst, src, v >> 12);
        if (v & 0xFFF) {
            aoStrCatFmt(buf, "%s %s, %s, #%I\n\t",
                        op, dst, dst, v & 0xFFF);
        }
        return;
    }
    aarch64EmitMovImm(buf, "x9", v);
    aoStrCatFmt(buf, "%s %s, %s, x9\n\t", op, dst, src);
}

/* Emit `reg += disp` (the in-place form of the above). */
static void aarch64AddSubImm(AoStr *buf, const char *reg, s64 disp) {
    if (disp == 0) return;
    if (disp < 0) aarch64EmitAddSubImm(buf, "sub", reg, reg, -disp);
    else          aarch64EmitAddSubImm(buf, "add", reg, reg, disp);
}

/* `dst = x29 + loff` for any signed frame offset. */
static void aarch64EmitFrameAddr(AoStr *buf, const char *dst, s64 loff)
{
    if (loff >= 0) aarch64EmitAddSubImm(buf, "add", dst, "x29", loff);
    else           aarch64EmitAddSubImm(buf, "sub", dst, "x29", -loff);
}

/* Can `[base, #disp]` encode for a `size`-byte access? Unscaled
 * (ldur/stur) covers signed -256..255; the scaled unsigned form
 * covers size-aligned 0..4095*size. */
static int aarch64IsMemDisp(s32 disp, u32 size) {
    if (disp >= -256 && disp <= 255) return 1;
    if (disp >= 0 && (disp % (s32)size) == 0 &&
        disp <= 4095 * (s32)size) return 1;
    return 0;
}

/* Width-aware load through a register-held address. */
static void aarch64DerefLoad(AoStr *buf, u32 size, const char *dst_reg,
                             const char *base_reg, const char *idx_reg,
                             u8 scale, s32 disp)
{
    char wdst[8];
    aarch64RegForWidth(dst_reg, (int)(size <= 4 ? 4 : 8), wdst, sizeof(wdst));
    char mem[64];
    if (idx_reg) {
        /* AArch64 indexed: [base, idx, lsl #log2(scale)]. Scale must
         * match the access size (1/2/4/8). Otherwise we need an
         * explicit shift before the load. */
        int sh = 0;
        if      (scale == 2) sh = 1;
        else if (scale == 4) sh = 2;
        else if (scale == 8) sh = 3;

        if (sh == 0) snprintf(mem, sizeof(mem), "[%s, %s]", base_reg, idx_reg);
        else snprintf(mem, sizeof(mem), "[%s, %s, lsl #%d]",
                      base_reg, idx_reg, sh);
        aarch64AddSubImm(buf, base_reg, (s64)disp);
    } else if (disp != 0 && aarch64IsMemDisp(disp, size)) {
        snprintf(mem, sizeof(mem), "[%s, #%d]", base_reg, (int)disp);
    } else if (disp != 0) {
        /* Out-of-range displacement: register-offset form via x10 -
         * base_reg may be a live allocated register, so it must not
         * be mutated (x10 is free: values use x0/x9, bases x1-x7). */
        aarch64EmitMovImm(buf, "x10", (s64)disp);
        snprintf(mem, sizeof(mem), "[%s, x10]", base_reg);
    } else {
        snprintf(mem, sizeof(mem), "[%s]", base_reg);
    }
    /* 4-byte deref-load sign-extends to 64-bit (matches FrameLoad
     * policy / x86 movslq); xreg holds the x-name for that case. */
    char xreg[8];
    aarch64RegForWidth(dst_reg, 8, xreg, sizeof(xreg));
    switch (size) {
        case 1:  aoStrCatFmt(buf, "ldrb    %s, %s\n\t", wdst, mem); break;
        case 2:  aoStrCatFmt(buf, "ldrh    %s, %s\n\t", wdst, mem); break;
        case 4:  aoStrCatFmt(buf, "ldrsw   %s, %s\n\t", xreg, mem); break;
        default: aoStrCatFmt(buf, "ldr %s, %s\n\t", wdst, mem); break;
    }
}

static void aarch64DerefStore(AoStr *buf, u32 size, const char *val_reg,
                              const char *base_reg, const char *idx_reg,
                              u8 scale, s32 disp)
{
    char wval[8];
    aarch64RegForWidth(val_reg, (int)(size <= 4 ? 4 : 8), wval, sizeof(wval));
    char mem[64];
    if (idx_reg) {
        int sh = 0;
        switch (scale) { case 1: sh=0; break; case 2: sh=1; break;
                         case 4: sh=2; break; case 8: sh=3; break;
                         default: sh = 0; }
        if (sh == 0) snprintf(mem, sizeof(mem), "[%s, %s]", base_reg, idx_reg);
        else snprintf(mem, sizeof(mem), "[%s, %s, lsl #%d]",
                      base_reg, idx_reg, sh);
        aarch64AddSubImm(buf, base_reg, (s64)disp);
    } else if (disp != 0 && aarch64IsMemDisp(disp, size)) {
        snprintf(mem, sizeof(mem), "[%s, #%d]", base_reg, (int)disp);
    } else if (disp != 0) {
        /* See aarch64DerefLoad: never mutate base_reg, x10 is free
         * (the value may be sitting in x9). */
        aarch64EmitMovImm(buf, "x10", (s64)disp);
        snprintf(mem, sizeof(mem), "[%s, x10]", base_reg);
    } else {
        snprintf(mem, sizeof(mem), "[%s]", base_reg);
    }
    switch (size) {
        case 1: aoStrCatFmt(buf, "strb    %s, %s\n\t", wval, mem); break;
        case 2: aoStrCatFmt(buf, "strh    %s, %s\n\t", wval, mem); break;
        case 4: aoStrCatFmt(buf, "str %s, %s\n\t", wval, mem); break;
        default: aoStrCatFmt(buf, "str %s, %s\n\t", wval, mem); break;
    }
}

/* Emit a float constant to the literal pool and load it into the
 * target FP register. `size` selects single (4) vs double (8)
 * precision: an 8-byte literal uses .quad / __literal8 and an s-reg
 * narrows to .long / __literal4. Bit-pattern dedup keeps identical
 * constants sharing one label across the function (4- and 8-byte
 * pools are kept separate so widths never alias). */
static void aarch64EmitFloatLiteral(IrCgCtx *ctx, const char *dreg, f64 f,
                                    int size)
{
    static int float_seq = 0;
    static Map *bits_to_label8 = NULL;
    static Map *bits_to_label4 = NULL;
    if (!bits_to_label8) bits_to_label8 = mapNew(16, &map_uint_to_uint_type);
    if (!bits_to_label4) bits_to_label4 = mapNew(16, &map_uint_to_uint_type);

    int is_single = (size == 4);
    Map *bits_to_label = is_single ? bits_to_label4 : bits_to_label8;
    u64 bits = is_single ? (u64)ieee754_32((f32)f) : (u64)ieee754_64(f);

    /* Adjust the destination register name to the right precision. */
    char dr[8];
    aarch64FpRegForWidth(dreg, size, dr, sizeof(dr));

    AoStr *label = (AoStr *)mapGetInt(bits_to_label, bits);
    if (!label) {
        char buf[64];
        int n = snprintf(buf, sizeof(buf), "LAArchF%d", float_seq++);
        label = aoStrDupRaw(buf, (u64)n);
        mapAddIntOrErr(bits_to_label, bits, label);
        /* Emit into a literal section interleaved with code, then
         * jump back to .text via a label-relative ldr. */
        aoStrRemovePreviousChar(ctx->buf, '\t');
        const char *directive = is_single ? ".long" : ".quad";
        if (ctx->cc->target == TARGET_AARCH64_APPLE_DARWIN) {
            const char *sect = is_single
                ? ".section __TEXT,__literal4,4byte_literals\n\t.p2align 2\n"
                : ".section __TEXT,__literal8,8byte_literals\n\t.p2align 3\n";
            aoStrCatPrintf(ctx->buf, "%s%s:\n\t%s 0x%llX\n.text\n\t",
                sect, label->data, directive, (unsigned long long)bits);
        } else {
            const char *sect = is_single
                ? ".section .rodata.cst4,\"aM\",@progbits,4\n\t.p2align 2\n"
                : ".section .rodata.cst8,\"aM\",@progbits,8\n\t.p2align 3\n";
            aoStrCatPrintf(ctx->buf, "%s%s:\n\t%s 0x%llX\n.text\n\t",
                sect, label->data, directive, (unsigned long long)bits);
        }
    }
    /* Load via adrp/ldr. */
    if (ctx->cc->target == TARGET_AARCH64_APPLE_DARWIN) {
        aoStrCatFmt(ctx->buf, "adrp    x9, %S@PAGE\n\t", label);
        aoStrCatFmt(ctx->buf, "ldr %s, [x9, %S@PAGEOFF]\n\t", dr, label);
    } else {
        aoStrCatFmt(ctx->buf, "adrp    x9, %S\n\t", label);
        aoStrCatFmt(ctx->buf, "ldr %s, [x9, :lo12:%S]\n\t", dr, label);
    }
}

/* Load any IrValue into the given x-register. */
static void aarch64LoadToReg(IrCgCtx *ctx, IrValue *val, const char *reg) {
    switch (val->kind) {
        case IR_VAL_CONST_INT:
            if (val->as._i64 == 0) {
                aoStrCatFmt(ctx->buf, "mov %s, xzr\n\t", reg);
            } else {
                aarch64EmitMovImm(ctx->buf, reg, val->as._i64);
            }
            break;
        case IR_VAL_CONST_FLOAT: {
            /* Rare: bit-cast double to integer. Materialise via
             * movz/movk on the bit pattern. */
            aarch64EmitMovImm(ctx->buf, reg, (s64)(u64)ieee754_64(val->as._f64));
            break;
        }
        case IR_VAL_CONST_STR:
            aarch64GlobalAddr(ctx->cc, ctx->buf, val->as.str.label->data, reg);
            break;
        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            if (val->pinned_reg) {
                aoStrCatFmt(ctx->buf, "mov %s, %s\n\t",
                            reg, val->pinned_reg->data);
                break;
            }
            if (val->loc.kind == IR_LOC_REG && val->loc.as.reg) {
                const char *src = val->loc.as.reg->data;
                if (strcmp(src, reg) != 0) {
                    aoStrCatFmt(ctx->buf, "mov %s, %s\n\t", reg, src);
                }
                break;
            }
            u32 size = irValueByteSize(val);
            int loff = irCgGetLoff(&ctx->fn->ra, val);
            aarch64FrameLoad(ctx->buf, reg, size, loff);
            break;
        }
        default:
            loggerPanic("ir-cg-aarch64: cannot load value of kind %s\n",
                        irValueKindToString(val->kind));
    }
}

static void aarch64StoreReg(IrCgCtx *ctx, IrValue *dst, const char *reg) {
    if (dst->pinned_reg) {
        aoStrCatFmt(ctx->buf, "mov %s, %s\n\t", dst->pinned_reg->data, reg);
        return;
    }
    if (dst->loc.kind == IR_LOC_REG && dst->loc.as.reg) {
        const char *home = dst->loc.as.reg->data;
        if (strcmp(home, reg) != 0) {
            aoStrCatFmt(ctx->buf, "mov %s, %s\n\t", home, reg);
        }
        return;
    }
    u32 size = irValueByteSize(dst);
    int loff = irCgGetLoff(&ctx->fn->ra, dst);
    aarch64FrameStore(ctx->buf, reg, size, loff);
}

static void aarch64SpillDst(IrCgCtx *ctx, IrInstr *instr, const char *reg) {
    aarch64StoreReg(ctx, instr->dst, reg);
}

static void aarch64LoadToFpr(IrCgCtx *ctx, IrValue *val, const char *dreg) {
    switch (val->kind) {
        case IR_VAL_CONST_FLOAT:
        case IR_VAL_CONST_INT: {
            double _f64 = val->kind == IR_VAL_CONST_INT ?
                                       (double)val->as._i64 :
                                       val->as._f64;
            int size = (val->kind == IR_VAL_CONST_FLOAT)
                     ? (int)irValueByteSize(val) : 8;
            if (ieee754_64(_f64) == 0) {
                /* Zero via the zero register (wzr for s-regs, xzr for d). */
                char dr[8];
                aarch64FpRegForWidth(dreg, size, dr, sizeof(dr));
                aoStrCatFmt(ctx->buf, "fmov    %s, %s\n\t", dr,
                            size == 4 ? "wzr" : "xzr");
                break;
            }
            aarch64EmitFloatLiteral(ctx, dreg, _f64, size);
            break;
        }
        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            /* Pinned FP local: `fmov dreg, <pinned>` - the assembler
             * picks the FP-FP or GPR-FP form from the register names. */
            if (val->pinned_reg) {
                const char *src = val->pinned_reg->data;
                if (strcmp(src, dreg) != 0)
                    aoStrCatFmt(ctx->buf, "fmov    %s, %s\n\t", dreg, src);
                break;
            }
            if (val->loc.kind == IR_LOC_REG && val->loc.as.reg) {
                const char *src = val->loc.as.reg->data;
                if (strcmp(src, dreg) == 0) break;
                /* Cross-file move: int reg -> FP via fmov. */
                const char *mnem = (src[0] == 'd' || src[0] == 's')
                                    ? "fmov" : "fmov";
                aoStrCatFmt(ctx->buf, "%s    %s, %s\n\t", mnem, dreg, src);
                break;
            }
            u32 size = irValueByteSize(val);
            int loff = irCgGetLoff(&ctx->fn->ra, val);
            aarch64FpFrameLoad(ctx->buf, dreg, size, loff);
            break;
        }
        default:
            loggerPanic("ir-cg-aarch64: cannot load float of kind %s\n",
                        irValueKindToString(val->kind));
    }
}

/* `val_size` is the byte width of the value being stored (0 = derive
 * from `dst`). An FP store writes the value's width, which differs from
 * `dst`'s when `dst` is a GEP'd field address (a `ptr` tmp, size 8) -
 * storing 8 bytes there would clobber the adjacent field. */
static void aarch64StoreFprSized(IrCgCtx *ctx, IrValue *dst,
                                 const char *dreg, int val_size) {
    /* Pinned FP local: `fmov <pinned>, dreg` (FP-FP or FP-GPR per the
     * register names). */
    if (dst->pinned_reg) {
        const char *home = dst->pinned_reg->data;
        if (strcmp(home, dreg) != 0)
            aoStrCatFmt(ctx->buf, "fmov    %s, %s\n\t", home, dreg);
        return;
    }
    if (dst->loc.kind == IR_LOC_REG && dst->loc.as.reg) {
        const char *home = dst->loc.as.reg->data;
        if (strcmp(home, dreg) != 0) {
            aoStrCatFmt(ctx->buf, "fmov    %s, %s\n\t", home, dreg);
        }
        return;
    }
    u32 size = val_size > 0 ? (u32)val_size : irValueByteSize(dst);
    int loff = irCgGetLoff(&ctx->fn->ra, dst);
    aarch64FpFrameStore(ctx->buf, dreg, size, loff);
}

static void aarch64StoreFpr(IrCgCtx *ctx, IrValue *dst, const char *dreg) {
    aarch64StoreFprSized(ctx, dst, dreg, 0);
}

static void aarch64SpillDstFpr(IrCgCtx *ctx, IrInstr *instr, const char *dreg) {
    aarch64StoreFpr(ctx, instr->dst, dreg);
}

static void aarch64LoadFirstSrc(IrCgCtx *ctx, IrInstr *instr, IrValue *src) {
    (void)instr; aarch64LoadToReg(ctx, src, "x0");
}

static void aarch64LoadFirstSrcFpr(IrCgCtx *ctx, IrInstr *instr, IrValue *src) {
    (void)instr; aarch64LoadToFpr(ctx, src, "d0");
}

/* AArch64 add/sub take a 12-bit unsigned immediate (optionally
 * shifted by 12). Anything bigger needs a materialise + reg form. */
static int aarch64IsAddImm(s64 v) {
    if (v < 0) return 0;
    if (v <= 0xFFF) return 1;
    if ((v & 0xFFF) == 0 && (v >> 12) <= 0xFFF) return 1;
    return 0;
}


/* True if `imm` is encodable as an AArch64 logical bitmask immediate
 * (rotation of N consecutive 1-bits, repeated at 2/4/8/16/32/64 bit
 * element size). Returns 0 to force the caller to materialise the
 * imm in a register. This should always be sound, sometimes pessimistic. */
static int aarch64IsLogicalImm(s64 imm, int is_64) {
    u64 v = (u64)imm;
    u64 width = is_64 ? 64 : 32;
    if (v == 0) return 0;            /* and 0 is "mov 0" not a logical imm */
    if (!is_64) v &= 0xFFFFFFFFULL;
    if (is_64 && v == ~(u64)0) return 0;  /* all-ones not encodable */
    /* Try each element size; the pattern repeats at that period. */
    for (u64 size = 2; size <= width; size *= 2) {
        u64 mask = (size == 64) ? ~(u64)0 : ((u64)1 << size) - 1;
        u64 e = v & mask;
        /* Check `v` repeats `e` at every element. */
        u64 r = 0;
        for (u64 b = 0; b < width; b += size) r |= e << b;
        if (r != v) continue;
        /* Count 1s in `e`. */
        u64 ones = 0, tmp = e;
        while (tmp) { ones += tmp & 1; tmp >>= 1; }
        if (ones == 0 || ones == size) continue;
        /* `e` must be N consecutive 1-bits, possibly rotated within
         * the element. Try rotating `e` until its low bits are all 1
         * and no higher 1-bits remain (i.e. it's `(1<<ones)-1`). */
        u64 target = ((u64)1 << ones) - 1;
        for (u64 rot = 0; rot < size; rot++) {
            if (e == target) return 1;
            e = ((e >> 1) | (e << (size - 1))) & mask;
        }
    }
    return 0;
}

static const char *aarch64IdxReg(IrCgCtx *ctx, IrInstr *instr) {
    if (!instr->idx || !instr->scale) return NULL;
    if (instr->idx->loc.kind == IR_LOC_REG && instr->idx->loc.as.reg)
        return instr->idx->loc.as.reg->data;
    aarch64LoadToReg(ctx, instr->idx, "x2");
    return "x2";
}

/* Condition codes for branches. AArch64 uses the same suffixes as
 * x86's jCC; mapping is straightforward. */
static const char *aarch64CcFor(IrCmpKind cmp, int is_float) {
    if (is_float) {
        /* FP unordered compares use the "mi"/"pl"/"gt"/"ge" variants;
         * we treat NaN like x86's unordered (most ops fall through). */
        switch (cmp) {
            case IR_CMP_EQ:  return "eq";
            case IR_CMP_NE:  return "ne";
            case IR_CMP_LT:  return "mi";
            case IR_CMP_LE:  return "ls";
            case IR_CMP_GT:  return "gt";
            case IR_CMP_GE:  return "ge";
            default: loggerPanic("ir-cg-aarch64: float cmp %d\n", cmp);
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
        default: loggerPanic("ir-cg-aarch64: cmp %d\n", cmp);
    }
}

static const char *aarch64CcInvFor(IrCmpKind cmp, int is_float) {
    if (is_float) {
        switch (cmp) {
            case IR_CMP_EQ:  return "ne";
            case IR_CMP_NE:  return "eq";
            case IR_CMP_LT:  return "pl";
            case IR_CMP_LE:  return "hi";
            case IR_CMP_GT:  return "le";
            case IR_CMP_GE:  return "lt";
            default: loggerPanic("ir-cg-aarch64: float cmp %d\n", cmp);
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
        default: loggerPanic("ir-cg-aarch64: cmp %d\n", cmp);
    }
}

/* Emit a cset into x0 setting it to 1 or 0 based on the last cmp. */
static void aarch64EmitCSet(IrCgCtx *ctx, IrCmpKind cmp, int is_float) {
    aoStrCatFmt(ctx->buf, "cset    x0, %s\n\t", aarch64CcFor(cmp, is_float));
}

static void aarch64Epilogue(IrCgCtx *ctx, int frame_size, int omit_frame);

static void aarch64BlockLabel(IrCgCtx *ctx, IrBlock *block, char *out, int n) {
    snprintf(out, n, ".LIRBB%d_%u", ctx->fn->uuid, block->id);
}

static void aarch64EmitOnePhi(IrCgCtx *ctx, IrInstr *phi, IrPair *match) {
    IrValue *v = match ? match->ir_value : NULL;
    int v_dangling = irIsTmp(v) &&
                     v->loc.kind != IR_LOC_REG &&
                     !mapHasInt(ctx->fn->ra.id_to_loff, irVarId(v));
    if (irIsFloat(phi->dst->type)) {
        if (v_dangling) {
            aoStrCatFmt(ctx->buf, "fmov    d0, xzr\n\t");
        } else {
            aarch64LoadToFpr(ctx, v, "d0");
        }
        aarch64StoreFpr(ctx, phi->dst, "d0");
        return;
    }
    if (v_dangling) {
        aoStrCatFmt(ctx->buf, "mov x0, xzr\n\t");
    } else {
        aarch64LoadToReg(ctx, v, "x0");
    }
    aarch64StoreReg(ctx, phi->dst, "x0");
}

#define kMaxPhisArm 32
static void aarch64PhiMaterialise(IrCgCtx *ctx, IrBlock *from, IrBlock *to) {
    if (!to || !from) return;
    IrInstr *phis[kMaxPhisArm];
    IrPair  *pairs[kMaxPhisArm];
    int     done[kMaxPhisArm];
    int n = 0;

    listForEach(to->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_NOP) continue;
        if (I->op != IR_PHI) break;
        if (n >= kMaxPhisArm) {
            loggerPanic("ir-cg-aarch64: too many phis at one block (>%d)\n",
                        kMaxPhisArm);
        }
        IrPair *match = NULL;
        if (I->extra.phi_pairs) {
            for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                if (p->ir_block == from) { match = p; break; }
            }
        }
        if (!match || !match->ir_value) continue;
        phis[n] = I; pairs[n] = match; done[n] = 0; n++;
    }
    if (n == 0) return;

    int emitted = 0;
    while (emitted < n) {
        int progress = 0;
        for (int i = 0; i < n; ++i) {
            if (done[i]) continue;
            int read_by_pending = 0;
            for (int j = 0; j < n; ++j) {
                if (i == j || done[j]) continue;
                IrValue *v = pairs[j]->ir_value;
                if (irIsTmp(v) && irIsTmp(phis[i]->dst) &&
                    irVarId(v) == irDstVarId(phis[i])) {
                    read_by_pending = 1; break;
                }
            }
            if (!read_by_pending) {
                aarch64EmitOnePhi(ctx, phis[i], pairs[i]);
                done[i] = 1; emitted++; progress = 1;
            }
        }
        if (!progress) {
            loggerPanic("ir-cg-aarch64: phi cycle at block %u\n", to->id);
        }
    }
}

/* HolyC variadic args go on the stack starting at sp+0, not in
 * argument registers, so the callee can index them as a contiguous
 * argv region. Returns the number of stack arg slots needed (each
 * 8 bytes). */
static s32 aarch64PartitionCallArgs(u8 *is_stack, u64 n, Vec *args,
                                    int is_variadic, int named_count,
                                    int force_va_stack)
{
    int int_used = 0;
    int float_used = 0;
    int stack_count = 0;
    for (u64 i = 0; i < n; ++i) {
        IrValue *a = vecGet(IrValue *, args, i);
        int is_float = irIsFloat(a->type);
        int force_stack = 0;
        if (is_variadic && force_va_stack && (int)i >= named_count) {
            force_stack = 1;
        }
        if (force_stack) {
            is_stack[i] = 1; stack_count++;
        } else if (is_float) {
            if (float_used < 8) { is_stack[i] = 0; float_used++; }
            else { is_stack[i] = 1; stack_count++; }
        } else {
            if (int_used < 8) { is_stack[i] = 0; int_used++; }
            else { is_stack[i] = 1; stack_count++; }
        }
    }
    /* Round to 16-byte alignment (each slot 8). */
    return (s32)stack_count;
}

/* Emit an `extern "c"` call that passes one or more structs by value,
 * following AAPCS64. Scalars and structs are classified together so the
 * GP/FP register counters (NGRN/NSRN) stay in sync with the C ABI.
 * Covers the register cases (HFA -> v regs, aggregate <=16 -> x regs,
 * scalars); register-exhaustion stack spills, >16-byte by-reference
 * args, and odd partial-chunk widths are not handled yet and panic
 * loudly rather than miscompile. */
/* Copy `size` bytes from [src_reg, #0..] to [sp, #dst] via x10. */
static void aarch64CopyToSp(AoStr *buf, const char *src_reg, int size, int dst)
{
    int o = 0;
    while (size - o >= 8) {
        aoStrCatFmt(buf, "ldr x10, [%s, #%i]\n\t", src_reg, o);
        aoStrCatFmt(buf, "str x10, [sp, #%i]\n\t", dst + o);
        o += 8;
    }
    int rem = size - o;
    if (rem == 4) {
        aoStrCatFmt(buf, "ldr w10, [%s, #%i]\n\t", src_reg, o);
        aoStrCatFmt(buf, "str w10, [sp, #%i]\n\t", dst + o);
    } else if (rem == 2) {
        aoStrCatFmt(buf, "ldrh w10, [%s, #%i]\n\t", src_reg, o);
        aoStrCatFmt(buf, "strh w10, [sp, #%i]\n\t", dst + o);
    } else if (rem == 1) {
        aoStrCatFmt(buf, "ldrb w10, [%s, #%i]\n\t", src_reg, o);
        aoStrCatFmt(buf, "strb w10, [sp, #%i]\n\t", dst + o);
    } else if (rem != 0) {
        loggerPanic("ir-cg-aarch64: %d-byte arg copy chunk not supported\n",
                    rem);
    }
}

/* Copy `size` bytes from [base_reg, #src] to [x29, #dst] using x10 as a
 * scratch register (8/4/2/1-byte chunks). Both src and dst are signed
 * displacements; clang selects ldur/stur for the negative-slot case.
 * Either side whose window leaves the always-encodable ldur/stur range
 * is rebased onto a scratch first (x11 for the source - base_reg may
 * itself be x9 - and x12 for the frame side; both are free, the
 * allocator only hands out x0-x7). */
static void aarch64CopyToSlot(AoStr *buf, const char *base_reg, int size,
                              int src, int dst)
{
    const char *dst_base = "x29";
    if (src < -240 || src + size > 240) {
        if (src >= 0) aarch64EmitAddSubImm(buf, "add", "x11", base_reg, src);
        else          aarch64EmitAddSubImm(buf, "sub", "x11", base_reg, -src);
        base_reg = "x11";
        src = 0;
    }
    if (dst < -240 || dst + size > 240) {
        aarch64EmitFrameAddr(buf, "x12", dst);
        dst_base = "x12";
        dst = 0;
    }
    int o = 0;
    while (size - o >= 8) {
        aoStrCatFmt(buf, "ldr x10, [%s, #%i]\n\t", base_reg, src + o);
        aoStrCatFmt(buf, "str x10, [%s, #%i]\n\t", dst_base, dst + o);
        o += 8;
    }
    int rem = size - o;
    if (rem == 4) {
        aoStrCatFmt(buf, "ldr w10, [%s, #%i]\n\t", base_reg, src + o);
        aoStrCatFmt(buf, "str w10, [%s, #%i]\n\t", dst_base, dst + o);
    } else if (rem == 2) {
        aoStrCatFmt(buf, "ldrh w10, [%s, #%i]\n\t", base_reg, src + o);
        aoStrCatFmt(buf, "strh w10, [%s, #%i]\n\t", dst_base, dst + o);
    } else if (rem == 1) {
        aoStrCatFmt(buf, "ldrb w10, [%s, #%i]\n\t", base_reg, src + o);
        aoStrCatFmt(buf, "strb w10, [%s, #%i]\n\t", dst_base, dst + o);
    } else if (rem != 0) {
        loggerPanic("ir-cg-aarch64: %d-byte param copy chunk not "
                    "supported\n", rem);
    }
}

/* AOT (assembly-text) leaf ops for the shared a64Emit* lowering. The
 * helper resolves r1 to its live register, or loads it into the scratch. */
static const char *a64TxtSrcReg(IrCgCtx *ctx, IrInstr *i, int is_fpr) {
    if (i->r1->loc.kind == IR_LOC_REG && i->r1->loc.as.reg)
        return i->r1->loc.as.reg->data;
    if (is_fpr) { aarch64LoadFirstSrcFpr(ctx, i, i->r1); return "d0"; }
    aarch64LoadFirstSrc(ctx, i, i->r1); return "x0";
}
static void a64TxtStoreFpr(void *be, IrInstr *i, int val_size) {
    IrCgCtx *ctx = (IrCgCtx *)be;
    aarch64StoreFprSized(ctx, i->dst, a64TxtSrcReg(ctx, i, 1), val_size);
}
static void a64TxtStoreInt(void *be, IrInstr *i, int size) {
    IrCgCtx *ctx = (IrCgCtx *)be;
    const char *src = a64TxtSrcReg(ctx, i, 0);
    aarch64FrameStore(ctx->buf, src, (u32)size,
                      irCgGetLoff(&ctx->fn->ra, i->dst));
}
static void a64TxtStoreIntPinned(void *be, IrInstr *i) {
    IrCgCtx *ctx = (IrCgCtx *)be;
    aoStrCatFmt(ctx->buf, "mov %s, %s\n\t", i->dst->pinned_reg->data,
                a64TxtSrcReg(ctx, i, 0));
}
static void a64TxtEmitInit(A64Emitter *e, IrCgCtx *ctx) {
    e->be = ctx;
    e->store_fpr        = a64TxtStoreFpr;
    e->store_int        = a64TxtStoreInt;
    e->store_int_pinned = a64TxtStoreIntPinned;
}

/* AOT (assembly-text) leaf ops for the shared a64EmitCall lowering. */
static void a64TxtSubSp(void *be, int n) {
    aarch64EmitAddSubImm(((IrCgCtx *)be)->buf, "sub", "sp", "sp", n);
}
static void a64TxtAddSp(void *be, int n) {
    aarch64EmitAddSubImm(((IrCgCtx *)be)->buf, "add", "sp", "sp", n);
}
static void a64TxtLoadStructAddr(void *be, IrValue *a) {
    aarch64LoadToReg((IrCgCtx *)be, a, "x9");
}
static void a64TxtCopyX9ToSp(void *be, int size, int dst_off) {
    aarch64CopyToSp(((IrCgCtx *)be)->buf, "x9", size, dst_off);
}
static void a64TxtAggLoad(void *be, int is_fp, int reg, int src_off, int size) {
    AoStr *buf = ((IrCgCtx *)be)->buf;
    if (is_fp)
        aoStrCatFmt(buf, "ldr %s%i, [x9, #%i]\n\t", size == 4 ? "s" : "d",
                    reg, src_off);
    else if (size >= 8) aoStrCatFmt(buf, "ldr x%i, [x9, #%i]\n\t", reg, src_off);
    else if (size == 4) aoStrCatFmt(buf, "ldr w%i, [x9, #%i]\n\t", reg, src_off);
    else if (size == 2) aoStrCatFmt(buf, "ldrh w%i, [x9, #%i]\n\t", reg, src_off);
    else if (size == 1) aoStrCatFmt(buf, "ldrb w%i, [x9, #%i]\n\t", reg, src_off);
    else loggerPanic("ir-cg-aarch64: %d-byte struct chunk not supported\n", size);
}
static void a64TxtPtrInReg(void *be, int reg, int sp_off) {
    aoStrCatFmt(((IrCgCtx *)be)->buf, "add x%i, sp, #%i\n\t", reg, sp_off);
}
static void a64TxtPtrToStack(void *be, int sp_off, int nsaa) {
    AoStr *buf = ((IrCgCtx *)be)->buf;
    aoStrCatFmt(buf, "add x10, sp, #%i\n\t", sp_off);
    aoStrCatFmt(buf, "str x10, [sp, #%i]\n\t", nsaa);
}
static void a64TxtScalarToReg(void *be, IrValue *a, int is_fp, int reg) {
    IrCgCtx *ctx = (IrCgCtx *)be;
    char r[8];
    if (is_fp) { snprintf(r, sizeof(r), "d%d", reg); aarch64LoadToFpr(ctx, a, r); }
    else       { snprintf(r, sizeof(r), "x%d", reg); aarch64LoadToReg(ctx, a, r); }
}
static void a64TxtScalarToStack(void *be, IrValue *a, int is_fp, int nsaa) {
    IrCgCtx *ctx = (IrCgCtx *)be;
    if (is_fp) {
        aarch64LoadToFpr(ctx, a, "d0");
        aoStrCatFmt(ctx->buf, "str %s, [sp, #%i]\n\t",
                    aarch64Fpr((int)irValueByteSize(a), 0), nsaa);
    } else {
        aarch64LoadToReg(ctx, a, "x9");
        aoStrCatFmt(ctx->buf, "str x9, [sp, #%i]\n\t", nsaa);
    }
}
static void a64TxtCall(void *be, AoStr *fname, int indirect) {
    IrCgCtx *ctx = (IrCgCtx *)be;
    if (indirect) aoStrCatFmt(ctx->buf, "blr x16\n\t");
    else aoStrCatFmt(ctx->buf, "bl  %s\n\t",
                     asmNormaliseFunctionName(ctx->cc, fname));
}
static void a64TxtSpillRet(void *be, IrInstr *instr, int is_float) {
    IrCgCtx *ctx = (IrCgCtx *)be;
    if (is_float) aarch64SpillDstFpr(ctx, instr, "d0");
    else          aarch64SpillDst(ctx, instr, "x0");
}
/* Store a result-register chunk of a register-returned struct into the
 * destination buffer parked in x9 (mirror of a64TxtAggLoad). */
static void a64TxtRetChunkStore(void *be, int is_fp, int reg, int off, int size) {
    AoStr *buf = ((IrCgCtx *)be)->buf;
    if (is_fp)
        aoStrCatFmt(buf, "str %s%i, [x9, #%i]\n\t", size == 4 ? "s" : "d",
                    reg, off);
    else if (size >= 8) aoStrCatFmt(buf, "str x%i, [x9, #%i]\n\t", reg, off);
    else if (size == 4) aoStrCatFmt(buf, "str w%i, [x9, #%i]\n\t", reg, off);
    else if (size == 2) aoStrCatFmt(buf, "strh w%i, [x9, #%i]\n\t", reg, off);
    else if (size == 1) aoStrCatFmt(buf, "strb w%i, [x9, #%i]\n\t", reg, off);
    else loggerPanic("ir-cg-aarch64: %d-byte ret chunk not supported\n", size);
}
static void a64TxtSpillStructDst(void *be, IrInstr *instr) {
    aarch64SpillDst((IrCgCtx *)be, instr, "x9");
}
static void a64TxtStashDest(void *be, int sp_off) {
    aoStrCatFmt(((IrCgCtx *)be)->buf, "str x9, [sp, #%i]\n\t", sp_off);
}
static void a64TxtUnstashDest(void *be, int sp_off) {
    aoStrCatFmt(((IrCgCtx *)be)->buf, "ldr x9, [sp, #%i]\n\t", sp_off);
}
static void a64TxtCallInit(A64CallEmitter *e, IrCgCtx *ctx) {
    e->be = ctx;
    e->sub_sp           = a64TxtSubSp;
    e->add_sp           = a64TxtAddSp;
    e->load_struct_addr = a64TxtLoadStructAddr;
    e->copy_x9_to_sp    = a64TxtCopyX9ToSp;
    e->agg_load         = a64TxtAggLoad;
    e->ptr_in_reg       = a64TxtPtrInReg;
    e->ptr_to_stack     = a64TxtPtrToStack;
    e->scalar_to_reg    = a64TxtScalarToReg;
    e->scalar_to_stack  = a64TxtScalarToStack;
    e->call             = a64TxtCall;
    e->spill_ret        = a64TxtSpillRet;
    e->ret_chunk_store  = a64TxtRetChunkStore;
    e->spill_struct_dst = a64TxtSpillStructDst;
    e->stash_dest       = a64TxtStashDest;
    e->unstash_dest     = a64TxtUnstashDest;
}

/* AOT (assembly-text) leaf ops for the shared a64EmitParamPrologue. */
static void a64TxtAggStore(void *be, int is_fp, int reg, int loff, int size) {
    AoStr *buf = ((IrCgCtx *)be)->buf;
    /* Big frames put the slot beyond stur range; rebase onto x9 (free
     * here - incoming args live in x0-x7/d0-d7). */
    const char *base = "x29";
    if (loff < -240 || loff > 240) {
        aarch64EmitFrameAddr(buf, "x9", loff);
        base = "x9";
        loff = 0;
    }
    if (is_fp)
        aoStrCatFmt(buf, "str %s%i, [%s, #%i]\n\t", size == 4 ? "s" : "d",
                    reg, base, loff);
    else if (size >= 8) aoStrCatFmt(buf, "str x%i, [%s, #%i]\n\t", reg, base, loff);
    else if (size == 4) aoStrCatFmt(buf, "str w%i, [%s, #%i]\n\t", reg, base, loff);
    else if (size == 2) aoStrCatFmt(buf, "strh w%i, [%s, #%i]\n\t", reg, base, loff);
    else if (size == 1) aoStrCatFmt(buf, "strb w%i, [%s, #%i]\n\t", reg, base, loff);
    else loggerPanic("ir-cg-aarch64: %d-byte struct param chunk not "
                     "supported\n", size);
}
static void a64TxtCopyStackToSlot(void *be, int size, int incoming_off,
                                  int loff) {
    aarch64CopyToSlot(((IrCgCtx *)be)->buf, "x29", size, incoming_off, loff);
}
static void a64TxtIndirectToSlot(void *be, int from_stack, int reg,
                                 int incoming_off, int size, int loff) {
    AoStr *buf = ((IrCgCtx *)be)->buf;
    const char *base;
    char r[8];
    if (from_stack) {
        aoStrCatFmt(buf, "ldr x9, [x29, #%i]\n\t", incoming_off);
        base = "x9";
    } else {
        snprintf(r, sizeof(r), "x%d", reg);
        base = r;
    }
    aarch64CopyToSlot(buf, base, size, 0, loff);
}
static void a64TxtParamInit(A64ParamEmitter *e, IrCgCtx *ctx) {
    e->be = ctx;
    e->agg_store          = a64TxtAggStore;
    e->copy_stack_to_slot = a64TxtCopyStackToSlot;
    e->indirect_to_slot   = a64TxtIndirectToSlot;
}

/* AOT (assembly-text) leaf ops for the shared a64EmitConvert lowering. */
static void a64TxtCvtLoadGp(void *be, IrInstr *i) {
    aarch64LoadFirstSrc((IrCgCtx *)be, i, i->r1);
}
static void a64TxtCvtLoadFpr(void *be, IrInstr *i) {
    aarch64LoadFirstSrcFpr((IrCgCtx *)be, i, i->r1);
}
static void a64TxtCvtSpillGp(void *be, IrInstr *i) {
    aarch64SpillDst((IrCgCtx *)be, i, "x0");
}
static void a64TxtCvtSpillFpr(void *be, IrInstr *i) {
    aarch64SpillDstFpr((IrCgCtx *)be, i, "d0");
}
static void a64TxtConvert(void *be, A64CvtKind k, int width) {
    AoStr *buf = ((IrCgCtx *)be)->buf;
    switch (k) {
    case A64_CVT_UXTB: aoStrCatFmt(buf, "uxtb    w0, w0\n\t"); break;
    case A64_CVT_UXTH: aoStrCatFmt(buf, "uxth    w0, w0\n\t"); break;
    case A64_CVT_MOVW: aoStrCatFmt(buf, "mov w0, w0\n\t"); break;
    case A64_CVT_SXTB: aoStrCatFmt(buf, "sxtb    x0, w0\n\t"); break;
    case A64_CVT_SXTH: aoStrCatFmt(buf, "sxth    x0, w0\n\t"); break;
    case A64_CVT_SXTW: aoStrCatFmt(buf, "sxtw    x0, w0\n\t"); break;
    case A64_CVT_FCVT_NARROW: aoStrCatFmt(buf, "fcvt    s0, d0\n\t"); break;
    case A64_CVT_FCVT_WIDEN:  aoStrCatFmt(buf, "fcvt    d0, s0\n\t"); break;
    case A64_CVT_FCVTZS: aoStrCatFmt(buf, "fcvtzs  x0, %s\n\t", aarch64Fpr(width, 0)); break;
    case A64_CVT_FCVTZU: aoStrCatFmt(buf, "fcvtzu  x0, %s\n\t", aarch64Fpr(width, 0)); break;
    case A64_CVT_SCVTF:  aoStrCatFmt(buf, "scvtf   %s, x0\n\t", aarch64Fpr(width, 0)); break;
    case A64_CVT_UCVTF:  aoStrCatFmt(buf, "ucvtf   %s, x0\n\t", aarch64Fpr(width, 0)); break;
    case A64_CVT_FMOV_GP2FP:
        aoStrCatFmt(buf, width == 4 ? "fmov    s0, w0\n\t" : "fmov    d0, x0\n\t"); break;
    case A64_CVT_FMOV_FP2GP:
        aoStrCatFmt(buf, width == 4 ? "fmov    w0, s0\n\t" : "fmov    x0, d0\n\t"); break;
    }
}
static void a64TxtConvInit(A64ConvEmitter *e, IrCgCtx *ctx) {
    e->be = ctx;
    e->load_gp   = a64TxtCvtLoadGp;
    e->load_fpr  = a64TxtCvtLoadFpr;
    e->spill_gp  = a64TxtCvtSpillGp;
    e->spill_fpr = a64TxtCvtSpillFpr;
    e->convert   = a64TxtConvert;
}

/* AOT (assembly-text) leaf ops for the shared a64EmitArith lowering. */
static int a64TxtAluImm(void *be, IrOp op, s64 imm) {
    AoStr *buf = ((IrCgCtx *)be)->buf;
    if (op == IR_IADD || op == IR_ISUB) {
        const char *m = (op == IR_IADD) ? "add" : "sub";
        if (aarch64IsAddImm(imm)) { /* keep */ }
        else if (imm < 0 && aarch64IsAddImm(-imm)) {
            m = (op == IR_IADD) ? "sub" : "add"; imm = -imm;
        } else {
            return 0;
        }
        aoStrCatFmt(buf, "%s x0, x0, #%I\n\t", m, imm);
        return 1;
    }
    if (!aarch64IsLogicalImm(imm, 1)) return 0;
    aoStrCatFmt(buf, "%s x0, x0, #%I\n\t",
                op == IR_AND ? "and" : op == IR_OR ? "orr" : "eor", imm);
    return 1;
}
static const char *a64TxtIntMnem(IrOp op) {
    switch (op) {
    case IR_IADD: return "add"; case IR_ISUB: return "sub";
    case IR_AND:  return "and"; case IR_OR:   return "orr";
    default:      return "eor";
    }
}
static void a64TxtAluReg(void *be, IrOp op, IrInstr *i) {
    IrCgCtx *ctx = (IrCgCtx *)be;
    const char *rhs;
    if (i->r2->loc.kind == IR_LOC_REG && i->r2->loc.as.reg)
        rhs = i->r2->loc.as.reg->data;
    else { aarch64LoadToReg(ctx, i->r2, "x1"); rhs = "x1"; }
    aoStrCatFmt(ctx->buf, "%s x0, x0, %s\n\t", a64TxtIntMnem(op), rhs);
}
static void a64TxtMul(void *be) {
    aoStrCatFmt(((IrCgCtx *)be)->buf, "mul x0, x0, x1\n\t");
}
static void a64TxtDivRem(void *be, IrOp op) {
    AoStr *buf = ((IrCgCtx *)be)->buf;
    const char *div = (op == IR_IDIV || op == IR_IREM) ? "sdiv" : "udiv";
    if (op == IR_IDIV || op == IR_UDIV) {
        aoStrCatFmt(buf, "%s x0, x0, x1\n\t", div);
    } else {
        aoStrCatFmt(buf, "%s x2, x0, x1\n\t", div);
        aoStrCatFmt(buf, "msub    x0, x2, x1, x0\n\t");
    }
}
static void a64TxtUnary(void *be, IrOp op) {
    aoStrCatFmt(((IrCgCtx *)be)->buf,
                op == IR_INEG ? "neg x0, x0\n\t" : "mvn x0, x0\n\t");
}
static const char *a64TxtShiftMnem(IrOp op) {
    return (op == IR_SHL) ? "lsl" : (op == IR_SAR) ? "asr" : "lsr";
}
static void a64TxtShiftImm(void *be, IrOp op, int sh) {
    aoStrCatFmt(((IrCgCtx *)be)->buf, "%s x0, x0, #%i\n\t",
                a64TxtShiftMnem(op), sh);
}
static void a64TxtShiftReg(void *be, IrOp op, IrInstr *i) {
    IrCgCtx *ctx = (IrCgCtx *)be;
    aarch64LoadToReg(ctx, i->r2, "x1");
    aoStrCatFmt(ctx->buf, "%s x0, x0, x1\n\t", a64TxtShiftMnem(op));
}
static void a64TxtLoadX1(void *be, IrInstr *i) {
    aarch64LoadToReg((IrCgCtx *)be, i->r2, "x1");
}
static void a64TxtFbinop(void *be, IrOp op, int width) {
    AoStr *buf = ((IrCgCtx *)be)->buf;
    const char *m = (op == IR_FADD) ? "fadd" : (op == IR_FSUB) ? "fsub" :
                    (op == IR_FMUL) ? "fmul" : "fdiv";
    aoStrCatFmt(buf, "%s %s, %s, %s\n\t", m, aarch64Fpr(width, 0),
                aarch64Fpr(width, 0), aarch64Fpr(width, 1));
}
static void a64TxtLoadD1(void *be, IrInstr *i) {
    aarch64LoadToFpr((IrCgCtx *)be, i->r2, "d1");
}
static void a64TxtFneg(void *be, int width) {
    aoStrCatFmt(((IrCgCtx *)be)->buf, "fneg    %s, %s\n\t",
                aarch64Fpr(width, 0), aarch64Fpr(width, 0));
}
static void a64TxtArithInit(A64ArithEmitter *e, IrCgCtx *ctx) {
    e->be        = ctx;
    e->load_gp   = a64TxtCvtLoadGp;   e->load_fpr  = a64TxtCvtLoadFpr;
    e->spill_gp  = a64TxtCvtSpillGp;  e->spill_fpr = a64TxtCvtSpillFpr;
    e->alu_imm   = a64TxtAluImm;      e->alu_reg   = a64TxtAluReg;
    e->mul       = a64TxtMul;         e->divrem    = a64TxtDivRem;
    e->unary     = a64TxtUnary;       e->shift_imm = a64TxtShiftImm;
    e->shift_reg = a64TxtShiftReg;    e->load_x1   = a64TxtLoadX1;
    e->fbinop    = a64TxtFbinop;      e->load_d1   = a64TxtLoadD1;
    e->fneg      = a64TxtFneg;
}

static void aarch64EmitInstr(IrCgCtx *ctx, IrInstr *instr) {
    switch (instr->op) {
        case IR_NOP:
        case IR_LABEL:
            break;

        case IR_ALLOCA:
            /* The frame layout already reserved the slot; runtime
             * stack adjustment isn't needed. */
            break;

        case IR_LOAD: {
            if (instr->r1 && instr->r1->pinned_reg) {
                aoStrCatFmt(ctx->buf, "mov x0, %s\n\t",
                            instr->r1->pinned_reg->data);
                aarch64SpillDst(ctx, instr, "x0");
                break;
            }
            if (instr->dst && irIsFloat(instr->dst->type)) {
                u32 size = instr->dst->as.var.size;
                int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
                aarch64FpFrameLoad(ctx->buf, "d0", size, loff);
                aarch64SpillDstFpr(ctx, instr, "d0");
                break;
            }
            int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
            u32 size = instr->dst ? instr->dst->as.var.size : 8;
            aarch64FrameLoad(ctx->buf, "x0", size, loff);
            aarch64SpillDst(ctx, instr, "x0");
            break;
        }

        case IR_STORE: {
            /* Unified lowering shared with the JIT backend (aarch64-emit.h). */
            A64Emitter e;
            a64TxtEmitInit(&e, ctx);
            a64EmitStore(&e, instr);
            break;
        }

        case IR_LOAD_DEREF: {
            if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
                const char *sym = asmNormaliseGlobalLabel(ctx->cc,
                        instr->r1->as.global.name)->data;
                aarch64GlobalAddr(ctx->cc, ctx->buf, sym, "x1");
                u32 size = instr->dst ? instr->dst->as.var.size : 8;
                if (instr->dst && irIsFloat(instr->dst->type)) {
                    aoStrCatFmt(ctx->buf, "ldr %s, [x1]\n\t",
                                aarch64Fpr((int)irValueByteSize(instr->dst), 0));
                    aarch64SpillDstFpr(ctx, instr, "d0");
                } else {
                    aarch64DerefLoad(ctx->buf, size, "x0", "x1", NULL, 0, 0);
                    aarch64SpillDst(ctx, instr, "x0");
                }
                break;
            }
            const char *base_reg = "x1";
            if (instr->r1 && instr->r1->loc.kind == IR_LOC_REG &&
                instr->r1->loc.as.reg)
            {
                base_reg = instr->r1->loc.as.reg->data;
            } else {
                aarch64LoadToReg(ctx, instr->r1, "x1");
            }
            const char *idx_reg = aarch64IdxReg(ctx, instr);
            if (instr->dst && irIsFloat(instr->dst->type)) {
                /* AArch64 LDR for FP supports the same indexed form. */
                const char *fr = aarch64Fpr((int)irValueByteSize(instr->dst), 0);
                if (idx_reg) {
                    int sh = (instr->scale == 8) ? 3 :
                             (instr->scale == 4) ? 2 :
                             (instr->scale == 2) ? 1 : 0;
                    if (sh)
                        aoStrCatFmt(ctx->buf,
                            "ldr %s, [%s, %s, lsl #%i]\n\t",
                            fr, base_reg, idx_reg, sh);
                    else
                        aoStrCatFmt(ctx->buf, "ldr %s, [%s, %s]\n\t",
                                    fr, base_reg, idx_reg);
                } else if (instr->disp != 0 &&
                           aarch64IsMemDisp(instr->disp,
                               (u32)irValueByteSize(instr->dst))) {
                    aoStrCatFmt(ctx->buf, "ldr %s, [%s, #%i]\n\t",
                                fr, base_reg, (int)instr->disp);
                } else if (instr->disp != 0) {
                    aarch64EmitMovImm(ctx->buf, "x10", (s64)instr->disp);
                    aoStrCatFmt(ctx->buf, "ldr %s, [%s, x10]\n\t",
                                fr, base_reg);
                } else {
                    aoStrCatFmt(ctx->buf, "ldr %s, [%s]\n\t", fr, base_reg);
                }
                aarch64SpillDstFpr(ctx, instr, "d0");
                break;
            }
            u32 size = instr->dst ? instr->dst->as.var.size : 8;
            aarch64DerefLoad(ctx->buf, size, "x0", base_reg, idx_reg,
                             instr->scale, instr->disp);
            aarch64SpillDst(ctx, instr, "x0");
            break;
        }

        case IR_STORE_DEREF: {
            if (instr->dst && instr->dst->kind == IR_VAL_GLOBAL) {
                const char *sym = asmNormaliseGlobalLabel(ctx->cc,
                        instr->dst->as.global.name)->data;
                u32 sz = irValueByteSize(instr->r1);
                aarch64GlobalAddr(ctx->cc, ctx->buf, sym, "x1");
                if (instr->r1 && irIsFloat(instr->r1->type)) {
                    aarch64LoadFirstSrcFpr(ctx, instr, instr->r1);
                    aoStrCatFmt(ctx->buf, "str %s, [x1]\n\t",
                                aarch64Fpr((int)irValueByteSize(instr->r1), 0));
                } else {
                    aarch64LoadFirstSrc(ctx, instr, instr->r1);
                    aarch64DerefStore(ctx->buf, sz, "x0", "x1", NULL, 0, 0);
                }
                break;
            }
            const char *base_reg = "x1";
            int addr_in_reg = instr->dst &&
                              instr->dst->loc.kind == IR_LOC_REG &&
                              instr->dst->loc.as.reg;
            if (addr_in_reg) base_reg = instr->dst->loc.as.reg->data;
            const char *idx_reg = aarch64IdxReg(ctx, instr);
            if (!addr_in_reg) {
                aarch64LoadToReg(ctx, instr->dst, "x1");
            }
            if (instr->r1 && irIsFloat(instr->r1->type)) {
                const char *fr = aarch64Fpr((int)irValueByteSize(instr->r1), 0);
                aarch64LoadFirstSrcFpr(ctx, instr, instr->r1);
                if (idx_reg) {
                    int sh = (instr->scale == 8) ? 3 :
                             (instr->scale == 4) ? 2 :
                             (instr->scale == 2) ? 1 : 0;
                    if (sh)
                        aoStrCatFmt(ctx->buf,
                            "str %s, [%s, %s, lsl #%i]\n\t",
                            fr, base_reg, idx_reg, sh);
                    else
                        aoStrCatFmt(ctx->buf, "str %s, [%s, %s]\n\t",
                                    fr, base_reg, idx_reg);
                } else if (instr->disp != 0 &&
                           aarch64IsMemDisp(instr->disp,
                               (u32)irValueByteSize(instr->r1))) {
                    aoStrCatFmt(ctx->buf, "str %s, [%s, #%i]\n\t",
                                fr, base_reg, (int)instr->disp);
                } else if (instr->disp != 0) {
                    aarch64EmitMovImm(ctx->buf, "x10", (s64)instr->disp);
                    aoStrCatFmt(ctx->buf, "str %s, [%s, x10]\n\t",
                                fr, base_reg);
                } else {
                    aoStrCatFmt(ctx->buf, "str %s, [%s]\n\t", fr, base_reg);
                }
                break;
            }
            u32 sz = irValueByteSize(instr->r1);
            const char *val_reg = "x0";
            if (idx_reg && !strcmp(val_reg, idx_reg)) val_reg = "x9";
            if (!strcmp(val_reg, base_reg)) val_reg = "x9";
            aarch64LoadToReg(ctx, instr->r1, val_reg);
            aarch64DerefStore(ctx->buf, sz, val_reg, base_reg, idx_reg,
                              instr->scale, instr->disp);
            break;
        }

        case IR_RMW_DEREF: {
            /* AArch64 has no memory-destination ops. Lower to
             * load-modify-store on the same address. */
            const char *base_reg = "x1";
            int addr_in_reg = instr->dst &&
                              instr->dst->loc.kind == IR_LOC_REG &&
                              instr->dst->loc.as.reg;
            if (addr_in_reg) base_reg = instr->dst->loc.as.reg->data;
            else if (instr->dst && instr->dst->kind == IR_VAL_GLOBAL) {
                const char *sym = asmNormaliseGlobalLabel(ctx->cc,
                        instr->dst->as.global.name)->data;
                aarch64GlobalAddr(ctx->cc, ctx->buf, sym, "x1");
            } else {
                aarch64LoadToReg(ctx, instr->dst, "x1");
            }
            const char *idx_reg = aarch64IdxReg(ctx, instr);
            u32 sz = irValueByteSize(instr->r1);
            aarch64DerefLoad(ctx->buf, sz, "x0", base_reg, idx_reg,
                             instr->scale, instr->disp);
            /* Apply op to x0 and r1. */
            IrOp rop = instr->extra.rmw_op;
            const char *mnem;
            switch (rop) {
                case IR_IADD: mnem = "add"; break;
                case IR_ISUB: mnem = "sub"; break;
                case IR_AND:  mnem = "and"; break;
                case IR_OR:   mnem = "orr"; break;
                case IR_XOR:  mnem = "eor"; break;
                default: loggerPanic("ir-cg-aarch64: bad RMW op\n");
            }
            s64 imm;
            if (instr->r1 && instr->r1->kind == IR_VAL_CONST_INT) {
                imm = instr->r1->as._i64;
                int ok = (rop == IR_IADD || rop == IR_ISUB)
                         ? aarch64IsAddImm(imm)
                         : aarch64IsLogicalImm(imm, 1);
                if (ok) {
                    aoStrCatFmt(ctx->buf, "%s x0, x0, #%I\n\t", mnem, imm);
                } else {
                    aarch64LoadToReg(ctx, instr->r1, "x9");
                    aoStrCatFmt(ctx->buf, "%s x0, x0, x9\n\t", mnem);
                }
            } else {
                aarch64LoadToReg(ctx, instr->r1, "x9");
                aoStrCatFmt(ctx->buf, "%s x0, x0, x9\n\t", mnem);
            }
            aarch64DerefStore(ctx->buf, sz, "x0", base_reg, idx_reg,
                              instr->scale, instr->disp);
            break;
        }

        case IR_LEA: {
            if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
                const char *name = asmNormaliseGlobalLabel(ctx->cc,
                        instr->r1->as.global.name)->data;
                if (instr->r1->flags & IR_VAL_FLAG_FUNC) {
                    name = asmNormaliseFunctionName(ctx->cc,
                            instr->r1->as.global.name);
                }
                aarch64GlobalAddr(ctx->cc, ctx->buf, name, "x0");
            } else if (instr->r1) {
                int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
                aarch64EmitFrameAddr(ctx->buf, "x0", loff);
            }
            aarch64SpillDst(ctx, instr, "x0");
            break;
        }

        case IR_GEP: {
            /* GEP'd field address: alias with offset, handled in
             * layout. Codegen does nothing the dst's loff was
             * bound during regalloc. */
            break;
        }

        case IR_IADD: case IR_ISUB: case IR_AND: case IR_OR: case IR_XOR:
        case IR_IMUL: case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
        case IR_INEG: case IR_NOT: case IR_SHL: case IR_SHR: case IR_SAR:
        case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV: case IR_FNEG: {
            /* Unified arithmetic lowering shared with the JIT (aarch64-emit.h). */
            A64ArithEmitter ae;
            a64TxtArithInit(&ae, ctx);
            a64EmitArith(&ae, instr);
            break;
        }

        case IR_ICMP: {
            aarch64LoadFirstSrc(ctx, instr, instr->r1);
            if (instr->r2 && instr->r2->kind == IR_VAL_CONST_INT &&
                aarch64IsAddImm(instr->r2->as._i64))
            {
                aoStrCatFmt(ctx->buf, "cmp x0, #%I\n\t",
                            instr->r2->as._i64);
            } else {
                aarch64LoadToReg(ctx, instr->r2, "x1");
                aoStrCatFmt(ctx->buf, "cmp x0, x1\n\t");
            }
            aarch64EmitCSet(ctx, instr->extra.cmp_kind, 0);
            aarch64SpillDst(ctx, instr, "x0");
            break;
        }

        case IR_FCMP: {
            int fsz = (int)irValueByteSize(instr->r1);
            aarch64LoadFirstSrcFpr(ctx, instr, instr->r1);
            /* fcmp Dn, #0.0 if r2 is zero literal. */
            if (instr->r2 && instr->r2->kind == IR_VAL_CONST_FLOAT &&
                ieee754_64(instr->r2->as._f64) == 0)
            {
                aoStrCatFmt(ctx->buf, "fcmp    %s, #0.0\n\t",
                            aarch64Fpr(fsz, 0));
            } else {
                aarch64LoadToFpr(ctx, instr->r2, "d1");
                aoStrCatFmt(ctx->buf, "fcmp    %s, %s\n\t",
                            aarch64Fpr(fsz, 0), aarch64Fpr(fsz, 1));
            }
            aarch64EmitCSet(ctx, instr->extra.cmp_kind, 1);
            aarch64SpillDst(ctx, instr, "x0");
            break;
        }

        case IR_CMP_BR: {
            IrCmpKind kind = instr->extra.cmp_br.cmp_kind;
            IrBlock *t = instr->extra.cmp_br.target_block;
            IrBlock *f = instr->extra.cmp_br.fallthrough_block;
            int is_float = instr->r1 && irIsFloat(instr->r1->type);
            if (is_float) {
                int fsz = (int)irValueByteSize(instr->r1);
                aarch64LoadFirstSrcFpr(ctx, instr, instr->r1);
                if (instr->r2 && instr->r2->kind == IR_VAL_CONST_FLOAT &&
                    ieee754_64(instr->r2->as._f64) == 0)
                {
                    aoStrCatFmt(ctx->buf, "fcmp    %s, #0.0\n\t",
                                aarch64Fpr(fsz, 0));
                } else {
                    aarch64LoadToFpr(ctx, instr->r2, "d1");
                    aoStrCatFmt(ctx->buf, "fcmp    %s, %s\n\t",
                                aarch64Fpr(fsz, 0), aarch64Fpr(fsz, 1));
                }
            } else {
                aarch64LoadFirstSrc(ctx, instr, instr->r1);
                if (instr->r2 && instr->r2->kind == IR_VAL_CONST_INT &&
                    aarch64IsAddImm(instr->r2->as._i64))
                {
                    aoStrCatFmt(ctx->buf, "cmp x0, #%I\n\t",
                                instr->r2->as._i64);
                } else {
                    aarch64LoadToReg(ctx, instr->r2, "x1");
                    aoStrCatFmt(ctx->buf, "cmp x0, x1\n\t");
                }
            }
            char tlbl[64], flbl[64];
            aarch64BlockLabel(ctx, t, tlbl, sizeof(tlbl));
            aarch64BlockLabel(ctx, f, flbl, sizeof(flbl));
            const char *cc_t = aarch64CcFor(kind, is_float);
            const char *cc_f = aarch64CcInvFor(kind, is_float);
            int t_phi = irBlockHasPhi(t);
            int f_phi = irBlockHasPhi(f);
            if (!t_phi && !f_phi) {
                if (ctx->next_block == t) {
                    aoStrCatFmt(ctx->buf, "b.%s    %s\n\t", cc_f, flbl);
                } else if (ctx->next_block == f) {
                    aoStrCatFmt(ctx->buf, "b.%s    %s\n\t", cc_t, tlbl);
                } else {
                    aoStrCatFmt(ctx->buf, "b.%s    %s\n\t", cc_t, tlbl);
                    aoStrCatFmt(ctx->buf, "b   %s\n\t", flbl);
                }
            } else {
                /* One or both arms have phis. Branch to a per-arm
                 * intermediate label that materialises the phi
                 * incoming value, then jumps to the real target. */
                static int cb_seq = 0;
                char else_lbl[64];
                int br_id = cb_seq++;
                snprintf(else_lbl, sizeof(else_lbl),
                         ".LIRCB%u_E%d", ctx->fn->uuid, br_id);
                aoStrCatFmt(ctx->buf, "b.%s    %s\n\t", cc_f, else_lbl);
                aarch64PhiMaterialise(ctx, ctx->cur_block, t);
                aoStrCatFmt(ctx->buf, "b   %s\n", tlbl);
                aoStrRemovePreviousChar(ctx->buf, '\t');
                aoStrCatFmt(ctx->buf, "%s:\n\t", else_lbl);
                aarch64PhiMaterialise(ctx, ctx->cur_block, f);
                if (ctx->next_block != f) {
                    aoStrCatFmt(ctx->buf, "b   %s\n\t", flbl);
                }
            }
            break;
        }

        case IR_BR: {
            IrBlock *t = instr->extra.blocks.target_block;
            IrBlock *f = instr->extra.blocks.fallthrough_block;
            aarch64LoadFirstSrc(ctx, instr, instr->dst);
            aoStrCatFmt(ctx->buf, "cmp x0, #0\n\t");
            char tlbl[64], flbl[64];
            aarch64BlockLabel(ctx, t, tlbl, sizeof(tlbl));
            aarch64BlockLabel(ctx, f, flbl, sizeof(flbl));
            int t_phi = irBlockHasPhi(t);
            int f_phi = irBlockHasPhi(f);
            if (!t_phi && !f_phi) {
                if (ctx->next_block == t) {
                    aoStrCatFmt(ctx->buf, "b.eq    %s\n\t", flbl);
                } else if (ctx->next_block == f) {
                    aoStrCatFmt(ctx->buf, "b.ne    %s\n\t", tlbl);
                } else {
                    aoStrCatFmt(ctx->buf, "b.ne    %s\n\t", tlbl);
                    aoStrCatFmt(ctx->buf, "b   %s\n\t", flbl);
                }
            } else {
                static int br_seq = 0;
                char else_lbl[64];
                int br_id = br_seq++;
                snprintf(else_lbl, sizeof(else_lbl),
                         ".LIRBRP%u_E%d", ctx->fn->uuid, br_id);
                aoStrCatFmt(ctx->buf, "b.eq    %s\n\t", else_lbl);
                aarch64PhiMaterialise(ctx, ctx->cur_block, t);
                aoStrCatFmt(ctx->buf, "b   %s\n", tlbl);
                aoStrRemovePreviousChar(ctx->buf, '\t');
                aoStrCatFmt(ctx->buf, "%s:\n\t", else_lbl);
                aarch64PhiMaterialise(ctx, ctx->cur_block, f);
                if (ctx->next_block != f) {
                    aoStrCatFmt(ctx->buf, "b   %s\n\t", flbl);
                }
            }
            break;
        }

        case IR_JMP: {
            IrBlock *target = instr->extra.blocks.target_block;
            aarch64PhiMaterialise(ctx, ctx->cur_block, target);
            if (target != ctx->next_block) {
                char lbl[64];
                aarch64BlockLabel(ctx, target, lbl, sizeof(lbl));
                aoStrCatFmt(ctx->buf, "b   %s\n\t", lbl);
            }
            break;
        }

        case IR_RET: {
            if (instr->dst && instr->dst->byval_struct_type) {
                /* <=16-byte struct returned in registers: load its bytes from
                 * the return slot into v0.. (HFA) or x0,x1 (INTEGER). */
                AstType *t = instr->dst->byval_struct_type;
                int loff = irCgGetLoff(&ctx->fn->ra, instr->dst);
                int re = 0, rc = 0;
                AapcsClass rcls = astAapcsClassify(t, &re, &rc);
                /* Big frames put the slot beyond ldur/ldr immediate
                 * range; rebase onto x9 so every chunk offset below
                 * is small. x9 is dead here (return regs are x0/x1
                 * or v0..; the epilogue touches only sp/x29/x30). */
                const char *rbase = "x29";
                if (loff < -240 || loff > 240) {
                    aarch64EmitFrameAddr(ctx->buf, "x9", loff);
                    rbase = "x9";
                    loff = 0;
                }
                if (rcls == AAPCS_HFA) {
                    for (int k = 0; k < rc; ++k)
                        aoStrCatFmt(ctx->buf, "ldr %s%i, [%s, #%i]\n\t",
                                    re == 4 ? "s" : "d", k, rbase,
                                    loff + k * re);
                } else { /* INTEGER, <=16 bytes -> x0,(x1) */
                    int ngp = (t->size + 7) / 8;
                    for (int k = 0; k < ngp; ++k) {
                        int off = k * 8, rem = t->size - off;
                        int at = loff + off;
                        if (rem >= 8)
                            aoStrCatFmt(ctx->buf, "ldr x%i, [%s, #%i]\n\t", k, rbase, at);
                        else if (rem == 4)
                            aoStrCatFmt(ctx->buf, "ldr w%i, [%s, #%i]\n\t", k, rbase, at);
                        else if (rem == 2)
                            aoStrCatFmt(ctx->buf, "ldrh w%i, [%s, #%i]\n\t", k, rbase, at);
                        else
                            aoStrCatFmt(ctx->buf, "ldrb w%i, [%s, #%i]\n\t", k, rbase, at);
                    }
                }
            } else if (instr->dst) {
                if (irIsFloat(instr->dst->type)) {
                    aarch64LoadFirstSrcFpr(ctx, instr, instr->dst);
                } else {
                    aarch64LoadFirstSrc(ctx, instr, instr->dst);
                }
            }
            /* Emit the epilogue inline (matches the x86_64 backend). The
             * IR already routes every `return` through the exit block,
             * so the old `b .Lepi<n>` just jumped to the next line. */
            u32 aligned = ((u32)ctx->fn->stack_space + 15u) & ~15u;
            aarch64Epilogue(ctx, (int)aligned, ctx->omit_frame);
            break;
        }

        case IR_TRUNC: case IR_ZEXT: case IR_SEXT:
        case IR_FPTRUNC: case IR_FPEXT: case IR_FPTOSI: case IR_FPTOUI:
        case IR_SITOFP: case IR_UITOFP:
        case IR_PTRTOINT: case IR_INTTOPTR: case IR_BITCAST: {
            /* Unified conversion lowering shared with the JIT (aarch64-emit.h). */
            A64ConvEmitter ce;
            a64TxtConvInit(&ce, ctx);
            a64EmitConvert(&ce, instr);
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
                if (!instr->r2) {
                    loggerPanic("ir-cg-aarch64: indirect call without "
                                "target\n");
                }
                /* x16 is the intra-procedure-call scratch reg 
                 * outside the arg-reg range so the arg loads below
                 * can't clobber it. */
                aarch64LoadToReg(ctx, instr->r2, "x16");
            }

            /* A call passing a struct by value follows the platform C
             * ABI; route it through the AAPCS path. Scalar/variadic
             * calls keep the existing (simpler) path below. */
            /* Route through the AAPCS path when a struct is passed by value
             * OR returned by value (the latter needs register-return /
             * hidden-out-ptr handling). */
            int use_aapcs = (instr->flags & IRCG_CALL_AGG_RETURN) != 0;
            for (u64 i = 0; args && i < args->size; ++i) {
                if (vecGet(IrValue *, args, i)->byval_struct_type) {
                    use_aapcs = 1;
                    break;
                }
            }
            if (use_aapcs) {
                A64CallEmitter ce;
                a64TxtCallInit(&ce, ctx);
                a64EmitCall(&ce, instr, args, fname, indirect);
                break;
            }

            Ast *callee = NULL;
            if (!indirect) {
                callee = (Ast *)mapGetLen(ctx->cc->global_env,
                                          fname->data, fname->len);
            }
            int callee_va = 0;
            int named_count = 0;
            if (callee) {
                if ((callee->type && callee->type->has_var_args) ||
                    callee->has_var_args) callee_va = 1;
                else if (callee->params && callee->params->size > 0) {
                    Ast *last = vecGet(Ast *, callee->params,
                                       callee->params->size - 1);
                    if (last && last->kind == AST_VAR_ARGS) callee_va = 1;
                }
                if (callee->params) {
                    named_count = (int)callee->params->size;
                    if (callee_va && named_count > 0) named_count--;
                }
            }
            /* Two different variadic ABIs converge at this call site:
             *
             *  - HolyC-defined variadic callees read argc and the
             *    variadic slots from a contiguous on-stack region
             *    (argc@[x29,#16], argv@[x29,#24]). Their callee layout
             *    always uses this region (pool->variadic_on_stack is set
             *    for every AArch64 target), so we must push the varargs
             *    on the stack regardless of platform.
             *
             *  - External C variadic callees (printf, ...) follow the
             *    platform AAPCS64 rule: Apple Darwin passes varargs on
             *    the stack, Linux passes them in argument registers.
             *
             * Keying purely off the target (the old behaviour) left
             * Linux passing HolyC varargs in registers while the callee
             * read them from the stack - a guaranteed crash. */
            int apple_target =
                ctx->cc->target == TARGET_AARCH64_APPLE_DARWIN;
            int holyc_variadic =
                callee_va && callee && callee->kind != AST_EXTERN_FUNC;
            int extern_variadic =
                callee_va && callee && callee->kind == AST_EXTERN_FUNC;
            int force_va_stack =
                holyc_variadic || (extern_variadic && apple_target);
            u64 n = args ? args->size : 0;
            u8 *is_stack = (n > 0) ? (u8 *)calloc(n, 1) : NULL;
            s32 n_stack = aarch64PartitionCallArgs(is_stack, n, args,
                                                   callee_va,
                                                   named_count,
                                                   force_va_stack);
            int stack_bytes = (n_stack * 8 + 15) & ~15;
            if (stack_bytes > 0) {
                aarch64EmitAddSubImm(ctx->buf, "sub", "sp", "sp",
                                     stack_bytes);
            }
            int stack_idx = 0;
            for (u64 i = 0; i < n; ++i) {
                if (!is_stack[i]) continue;
                IrValue *a = vecGet(IrValue *, args, i);
                int off = stack_idx * 8;
                if (irIsFloat(a->type)) {
                    aarch64LoadToFpr(ctx, a, "d0");
                    aoStrCatFmt(ctx->buf, "str %s, [sp, #%i]\n\t",
                                aarch64Fpr((int)irValueByteSize(a), 0), off);
                } else {
                    aarch64LoadToReg(ctx, a, "x9");
                    aoStrCatFmt(ctx->buf, "str x9, [sp, #%i]\n\t", off);
                }
                stack_idx++;
            }
            IrRegPool *pool = irRegPoolGet();
            int int_idx = 0, float_idx = 0;
            for (u64 i = 0; i < n; ++i) {
                if (is_stack[i]) continue;
                IrValue *a = vecGet(IrValue *, args, i);
                if (irIsFloat(a->type)) {
                    AoStr *r = vecGet(AoStr *, pool->float_arg_regs,
                                      float_idx++);
                    aarch64LoadToFpr(ctx, a, r->data);
                } else {
                    AoStr *r = vecGet(AoStr *, pool->int_arg_regs,
                                      int_idx++);
                    aarch64LoadToReg(ctx, a, r->data);
                }
            }
            if (indirect) {
                aoStrCatFmt(ctx->buf, "blr x16\n\t");
            } else {
                char *normalised =
                    asmNormaliseFunctionName(ctx->cc, fname);
                aoStrCatFmt(ctx->buf, "bl  %s\n\t", normalised);
            }
            if (stack_bytes > 0) {
                aarch64EmitAddSubImm(ctx->buf, "add", "sp", "sp",
                                     stack_bytes);
            }
            if (instr->dst && instr->dst->type != IR_TYPE_VOID) {
                if (irIsFloat(instr->dst->type)) {
                    aarch64SpillDstFpr(ctx, instr, "d0");
                } else {
                    aarch64SpillDst(ctx, instr, "x0");
                }
            }
            if (is_stack) free(is_stack);
            break;
        }

        case IR_SELECT:
        case IR_ASM:
            /* Inline `asm { ... }` statement: assemble with libtasm
             * and emit the bytes in place. */
            if (instr->extra.asm_fragments) {
                loggerPanic("ir-cg-aarch64: `&var` asm fragments are not "
                            "supported; address locals directly\n");
            }
            aoStrRemovePreviousChar(ctx->buf, '\t');
            if (instr->r1 && instr->r1->as.str.str) {
                asmEmitBlockBytes(ctx->cc, ctx->buf,
                                  instr->r1->as.str.str, 0);
            }
            aoStrPutChar(ctx->buf, '\t');
            break;

        case IR_SWITCH:
        case IR_VA_ARG:
        case IR_VA_START:
        case IR_VA_END:
            loggerPanic("ir-cg-aarch64: op %d not yet implemented\n",
                        instr->op);

        default:
            loggerPanic("ir-cg-aarch64: unknown op %d\n", instr->op);
    }
}

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

static void aarch64EmitFunctionPrologue(Cctrl *cc, AoStr *buf, Ast *func,
                                        u16 total_stack, int omit_frame)
{
    char *fname = asmNormaliseFunctionName(cc, func->fname);
    aoStrCatFmt(buf,
                ".text\n\t"
                ".p2align 2\n\t"
                ".globl %s\n"
                "%s:\n\t",
                fname, fname);
    if (omit_frame) return;
    /* Standard prologue: push fp/lr pair, set new fp = sp, sub sp. */
    aoStrCatFmt(buf,
                "stp x29, x30, [sp, #-16]!\n\t"
                "mov x29, sp\n\t");
    if (total_stack > 0) {
        u32 aligned = ((u32)total_stack + 15u) & ~15u;
        aarch64EmitAddSubImm(buf, "sub", "sp", "sp", (s64)aligned);
    }
}

static void aarch64Epilogue(IrCgCtx *ctx, int frame_size, int omit_frame) {
    if (omit_frame) {
        aoStrCatFmt(ctx->buf, "ret\n");
        return;
    }
    if (frame_size > 0) {
        aarch64EmitAddSubImm(ctx->buf, "add", "sp", "sp", frame_size);
    }
    aoStrCatFmt(ctx->buf,
                "ldp x29, x30, [sp], #16\n\t"
                "ret\n");
}

/* Did the last emit already place an epilogue at the tail of buf? Lets
 * IR_RET emit the epilogue inline (no jump-to-end-label dance) while
 * the function driver only adds a terminating epilogue when the body
 * could fall off the end. The with-frame tail is stable regardless of
 * the (variable) `add sp, sp, #N` that may precede it. */
static int aarch64HasRet(IrCgCtx *ctx) {
    static const char *with_frame    = "ldp x29, x30, [sp], #16\n\tret\n";
    static const char *without_frame = "ret\n";
    const char *needle = ctx->omit_frame ? without_frame : with_frame;
    int nlen = (int)strlen(needle);
    AoStr *buf = ctx->buf;
    if ((int)buf->len < nlen) return 0;
    return memcmp(buf->data + buf->len - nlen, needle, nlen) == 0;
}

void aarch64GenerateFunction(IrCgCtx *ctx, Ast *ast) {
    AoStr *buf = ctx->buf;
    IrFunction *fn = ctx->fn;

    /* A variadic HolyC function reads argc/argv relative to the frame
     * pointer ([x29,#16]/[x29,#24]), so it must establish a frame even
     * when it has no locals and makes no calls - otherwise x29 still
     * holds the caller's value and the varargs reads return garbage. */
    int is_variadic = (ast->type && ast->type->has_var_args) ||
                      ast->has_var_args;
    ctx->omit_frame =
        (fn->stack_space == 0) && !irFnHasCalls(fn) && (ast->loff == 0) &&
        !is_variadic;

    aarch64EmitFunctionPrologue(ctx->cc, buf, ast, fn->stack_space,
                                ctx->omit_frame);
    A64ParamEmitter pe;
    a64TxtParamInit(&pe, ctx);
    a64EmitParamPrologue(&pe, ast, fn);

    Set *referenced = irCgComputeReferencedBlocks(fn);

    listForEach(fn->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        ctx->cur_block = block;
        ctx->next_block = (it->next != fn->blocks)
                         ? (IrBlock *)it->next->value
                         : NULL;
        if (setHas(referenced, (void *)(u64)block->id)) {
            char lbl[64];
            aarch64BlockLabel(ctx, block, lbl, sizeof(lbl));
            aoStrRemovePreviousChar(buf, '\t');
            aoStrCatPrintf(buf, "%s:\n\t", lbl);
        }
        listForEach(block->instructions) {
            IrInstr *instr = (IrInstr *)it->value;
            aarch64EmitInstr(ctx, instr);
        }
    }
    setRelease(referenced);

    /* If the body didn't already end with an inline epilogue (e.g. the
     * last block falls off the end rather than returning), emit one so
     * we don't run past the function. */
    if (!aarch64HasRet(ctx)) {
        u32 aligned = ((u32)fn->stack_space + 15u) & ~15u;
        aoStrRemovePreviousChar(buf, '\t');
        aarch64Epilogue(ctx, (int)aligned, ctx->omit_frame);
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

static void aarch64DataInternal(AoStr *buf, Ast *data) {
    if (data->kind == AST_STRING) {
        aoStrCatFmt(buf, ".quad   %S\n\t", data->slabel);
        return;
    }
    if (data->kind == AST_ARRAY_INIT) {
        listForEach(data->arrayinit) {
            aarch64DataInternal(buf, (Ast *)it->value);
        }
        return;
    }
    if (data->type->kind == AST_TYPE_FLOAT) {
        /* aoStrCatFmt has no hex specifier; use aoStrCatPrintf so the IEEE
         * bit pattern is emitted as a real hex literal. F32 lands in 4 bytes
         * (ieee754_32), F64 in 8. */
        if (data->type->size == 4) {
            aoStrCatPrintf(buf, ".long   0x%X\n\t",
                           (unsigned)ieee754_32((f32)data->f64));
        } else {
            aoStrCatPrintf(buf, ".quad   0x%llX\n\t",
                           (unsigned long long)ieee754_64(data->f64));
        }
        return;
    }
    switch (data->type->size) {
        case 1:  aoStrCatFmt(buf, ".byte   %i\n\t", data->i64); break;
        case 2:  aoStrCatFmt(buf, ".short  %i\n\t", data->i64); break;
        case 4:  aoStrCatFmt(buf, ".long   %i\n\t", data->i64); break;
        default: aoStrCatFmt(buf, ".quad   %I\n\t", data->i64); break;
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
        aoStrCatFmt(buf, "%S:\n\t", label);
        aarch64DataInternal(buf, declinit);
    } else {
        if (declvar->is_static) {
            aoStrCatFmt(buf, ".lcomm %S, %i\n\t", label,
                        declvar->type->size);
        } else if (label->len == 4 &&
                   (!strncmp(label->data, str_lit("argc")) ||
                    !strncmp(label->data, str_lit("argv"))))
        {
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
static void aarch64PasteAsmBlocks(AoStr *buf, Cctrl *cc) {
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

AoStr *aarch64AsmGenerate(Cctrl *cc) {
    AoStr *buf = aoStrAlloc(2048);
    aarch64InitRegPool();
    aarch64PasteDataSection(cc, buf);
    aarch64PasteAsmBlocks(buf, cc);

    Ast *synth_main = asmBuildInitialiserMain(cc);

    IrCtx *ir_ctx = irCtxNew(cc);
    IrCgCtx ctx;
    ctx.buf = buf;
    ctx.cc = cc;

    Set *seen_globals = setNew(32, &set_cstring_type);

    listForEach(cc->ast_list) {
        Ast *ast = it->value;
        if (ast->kind == AST_FUNC) {
            IrFunction *fn = irLowerFunction(ir_ctx, ast);
            irBasicFunctionOptimisations(fn);
            irFunctionPrepForCodeGen(&ctx, fn, ast);
            aarch64GenerateFunction(&ctx, ast);
        } else if (ast->kind == AST_DECL || ast->kind == AST_GVAR) {
            aarch64GlobalVar(cc, seen_globals, buf, ast);
        }
    }
    if (synth_main) {
        IrFunction *fn = irLowerFunction(ir_ctx, synth_main);
        irBasicFunctionOptimisations(fn);
        irFunctionPrepForCodeGen(&ctx, fn, synth_main);
        aarch64GenerateFunction(&ctx, synth_main);
    }

    asmEmitAsmInfo(cc, buf);
    setRelease(seen_globals);
    return buf;
}
