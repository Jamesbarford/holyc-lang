/* x86_64 IR-based codegen.
 *
 * SysV AMD64 ABI for both Linux and macOS (only the symbol naming and
 * PIC syntax differ - same call convention, same register set, same
 * struct passing). 
 *
 * Default codegen for X86_64 targets. The legacy AST-based x86.c
 * remains compiled in as a debugging fallback - pass --use-legacy-x86
 * (sets CCTRL_USE_LEGACY_X86) to route through it instead. Thes AST backend
 * will eventually be deleted. */
#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
#include "x86_64.h"

/* SysV first-six integer arg registers, ordered as arg 0..5. */
static const char *kIntRegs64[] = {
    "rdi", "rsi", "rdx", "rcx", "r8", "r9"
};
/* SysV first-eight FP arg registers. */
static const char *kXmmRegs[] = {
    "xmm0", "xmm1", "xmm2", "xmm3",
    "xmm4", "xmm5", "xmm6", "xmm7"
};

/* Build AoStr-wrapped backend pool once, lazily, and hand it to the
 * IR layer via `irRegPoolSet`. The AoStr instances live for the
 * compiler's lifetime; the Vec owns the pointer array. */
static Vec *x86_64MakeAoStrVec(const char *const *src, int n) {
    Vec *v = vecNew(&vec_aostr_type);
    for (int i = 0; i < n; ++i) {
        vecPush(v, aoStrDupRaw((char *)src[i], strlen(src[i])));
    }
    return v;
}

static void x86_64InitRegPool(void) {
    static int initialised = 0;
    static IrRegPool pool;
    if (initialised) return;

    static const char *kScratchRegs[] = { "rax", "rcx", "rdx", "xmm0", "xmm1" };
    int n_int     = (int)(sizeof(kIntRegs64)   / sizeof(kIntRegs64[0]));
    int n_float   = (int)(sizeof(kXmmRegs)     / sizeof(kXmmRegs[0]));
    int n_scratch = (int)(sizeof(kScratchRegs) / sizeof(kScratchRegs[0]));
    pool.int_arg_regs     = x86_64MakeAoStrVec(kIntRegs64,   n_int);
    pool.float_arg_regs   = x86_64MakeAoStrVec(kXmmRegs,     n_float);
    pool.int_return_reg   = aoStrDupRaw((char *)"rax", 3);
    pool.float_return_reg = aoStrDupRaw((char *)"xmm0", 4);
    pool.scratch_regs     = x86_64MakeAoStrVec(kScratchRegs, n_scratch);

    irRegPoolSet(&pool);
    initialised = 1;
}

/* Map a 64-bit GP register name to its narrower variant. The
 * heterogeneous register-naming scheme on x86 makes this a lookup
 * table rather than a name-mangling rule. */
static void x86_64RegForWidth(const char *reg64,
                              int size,
                              char *out,
                              u64 out_sz)
{
    static const struct { const char *q, *d, *w, *b; } tab[] = {
        {"rax", "eax",  "ax",  "al"},
        {"rbx", "ebx",  "bx",  "bl"},
        {"rcx", "ecx",  "cx",  "cl"},
        {"rdx", "edx",  "dx",  "dl"},
        {"rsi", "esi",  "si",  "sil"},
        {"rdi", "edi",  "di",  "dil"},
        {"rbp", "ebp",  "bp",  "bpl"},
        {"rsp", "esp",  "sp",  "spl"},
        {"r8",  "r8d",  "r8w", "r8b"},
        {"r9",  "r9d",  "r9w", "r9b"},
        {"r10", "r10d", "r10w","r10b"},
        {"r11", "r11d", "r11w","r11b"},
        {"r12", "r12d", "r12w","r12b"},
        {"r13", "r13d", "r13w","r13b"},
        {"r14", "r14d", "r14w","r14b"},
        {"r15", "r15d", "r15w","r15b"},
    };
    for (u64 i = 0; i < sizeof(tab)/sizeof(tab[0]); ++i) {
        if (!strcmp(reg64, tab[i].q)) {
            const char *pick;
            if (size == 1)      pick = tab[i].b;
            else if (size == 2) pick = tab[i].w;
            else if (size == 4) pick = tab[i].d;
            else                pick = tab[i].q;
            snprintf(out, out_sz, "%s", pick);
            return;
        }
    }
    loggerPanic("ir-cg-x86_64: unrecognised 64-bit reg name '%s'\n", reg64);
}

/* leaq <sym>(%rip), %<reg64>. Caller has already normalised the
 * symbol name */
static void x86_64GlobalAddr(IrCgCtx *ctx, const char *sym, const char *reg) {
    (void)ctx;
    aoStrCatFmt(ctx->buf, "leaq    %s(%%rip), %%%s\n\t", sym, reg);
}

/* leaq <loff>(%rbp), %<reg64>. AT&T allows signed displacements
 * natively. */
static void x86_64FrameAddr(AoStr *buf, int loff, const char *reg) {
    aoStrCatFmt(buf, "leaq    %i(%%rbp), %%%s\n\t", loff, reg);
}

static void x86_64Lea(IrCgCtx *ctx, IrInstr *lea, const char *reg) {
    if (lea->r1 && lea->r1->kind == IR_VAL_GLOBAL) {
        const char *name = lea->r1->as.global.name->data;
        if (lea->r1->flags & IR_VAL_FLAG_FUNC) {
            name = asmNormaliseFunctionName(ctx->cc,
                    lea->r1->as.global.name);
        }
        x86_64GlobalAddr(ctx, name, reg);
    } else {
        int loff = irCgGetLoff(&ctx->fn->ra, lea->r1);
        x86_64FrameAddr(ctx->buf, loff, reg);
    }
}

/* Width-aware load from <loff>(%rbp) into <reg64>.
 *
 * Width policy mirrors aarch64's choice: sub-word loads (1/2) zero-
 * extend to the full register, 4-byte loads sign-extend.
 * 8-byte loads are plain movq. */
static void x86_64Load(AoStr *buf, const char *reg, u32 size, int loff) {
    switch (size) {
        case 1:  aoStrCatFmt(buf, "movzbq  %i(%%rbp), %%%s\n\t", loff, reg); break;
        case 2:  aoStrCatFmt(buf, "movzwq  %i(%%rbp), %%%s\n\t", loff, reg); break;
        case 4:  aoStrCatFmt(buf, "movslq  %i(%%rbp), %%%s\n\t", loff, reg); break;
        default: aoStrCatFmt(buf, "movq    %i(%%rbp), %%%s\n\t", loff, reg); break;
    }
}

/* Build the `[disp](%base[,%idx,scale])` operand text. disp==0 ->
 * no displacement; idx_reg==NULL -> no SIB component. The scale
 * must be one of 1/2/4/8 when idx_reg is provided. */
static void x86_64FmtMemOperand(char *out, u64 out_sz,
                                s32 disp,
                                const char *base_reg,
                                const char *idx_reg,
                                u8 scale)
{
    if (idx_reg) {
        if (disp == 0) {
            snprintf(out, out_sz, "(%%%s,%%%s,%d)",
                     base_reg, idx_reg, (int)scale);
        } else {
            snprintf(out, out_sz, "%d(%%%s,%%%s,%d)",
                     (int)disp, base_reg, idx_reg, (int)scale);
        }
    } else if (disp == 0) {
        snprintf(out, out_sz, "(%%%s)", base_reg);
    } else {
        snprintf(out, out_sz, "%d(%%%s)", (int)disp, base_reg);
    }
}

/* Width-aware load from `disp(<base_reg>[, <idx_reg>, scale])` into
 * <reg64>. Same extension policy as x86_64Load. idx_reg==NULL drops
 * the SIB component. */
static void x86_64DerefLoadWidth(AoStr *buf,
                                 u32 size,
                                 const char *reg,
                                 const char *base_reg,
                                 const char *idx_reg,
                                 u8 scale,
                                 s32 disp)
{
    char mem[48];
    x86_64FmtMemOperand(mem, sizeof(mem), disp, base_reg, idx_reg, scale);
    switch (size) {
        case 1:  aoStrCatFmt(buf, "movzbq  %s, %%%s\n\t", mem, reg); break;
        case 2:  aoStrCatFmt(buf, "movzwq  %s, %%%s\n\t", mem, reg); break;
        case 4:  aoStrCatFmt(buf, "movslq  %s, %%%s\n\t", mem, reg); break;
        default: aoStrCatFmt(buf, "movq    %s, %%%s\n\t", mem, reg); break;
    }
}


const char *x86_64GetSizedMov(u32 size) {
    switch (size) {
        case 1:  return "movb";
        case 2:  return "movw";
        case 4:  return "movl";
        default: return "movq";
    }
}


static void x86_64GenericStoreWidth(AoStr *buf, u32 size, const char *reg, char *fmt) {
    switch (size) {
        case 1:
        case 2:
        case 4: {
            char nreg[8];
            const char *sized_mov = x86_64GetSizedMov(size);
            x86_64RegForWidth(reg, size, nreg, sizeof(nreg));
            aoStrCatFmt(buf, "%s    %%%s, %s\n\t", sized_mov, nreg, fmt);
            break;
        }
        default: aoStrCatFmt(buf, "movq    %%%s, %s\n\t", reg, fmt); break;
    }
}

/* Width-aware store of <reg64>'s low bits into <loff>(%rbp). The
 * narrower stores use the matching subregister name (e.g. al for
 * size=1) so the assembler picks the right encoding. */
static void x86_64FrameStoreWidth(AoStr *buf,
                                  int loff,
                                  u32 size,
                                  const char *reg)
{
    char fmt[32];
    snprintf(fmt, sizeof(fmt), "%d(%%rbp)", loff);
    x86_64GenericStoreWidth(buf, size, reg, fmt);

}

/* Width-aware store of <reg64> into `disp(<base_reg>[, <idx_reg>,
 * scale])`. idx_reg==NULL drops the SIB component. */
static void x86_64DerefStoreWidth(AoStr *buf,
                                  u32 size,
                                  const char *reg,
                                  const char *base_reg,
                                  const char *idx_reg,
                                  u8 scale,
                                  s32 disp)
{
    char fmt[48];
    x86_64FmtMemOperand(fmt, sizeof(fmt), disp, base_reg, idx_reg, scale);
    x86_64GenericStoreWidth(buf, size, reg, fmt);
}

static void x86_64DerefStoreGlobal(AoStr *buf, u32 size, const char *reg, const char *sym) {
    char fmt[32];
    snprintf(fmt, sizeof(fmt), "%s(%%rip)", sym);
    x86_64GenericStoreWidth(buf, size, reg, fmt);
}

/* x86 mnemonic suffix for an N-byte operation. */
static const char *x86_64SizedSuffix(u32 size) {
    switch (size) {
        case 1:  return "b";
        case 2:  return "w";
        case 4:  return "l";
        default: return "q";
    }
}

/* Mnemonic base for an RMW IrOp; suffix added by caller. */
static const char *x86_64RmwMnem(IrOp op) {
    switch (op) {
        case IR_IADD: return "add";
        case IR_ISUB: return "sub";
        case IR_AND:  return "and";
        case IR_OR:   return "or";
        case IR_XOR:  return "xor";
        default:      return NULL;
    }
}

/* Lay an immediate down through a register-held address (optionally
 * displaced and SIB-scaled). Used by the DST_IN_REG path so we
 * don't bounce through %rax for the value. */
static void x86_64DerefStoreImm(AoStr *buf,
                                u32 size,
                                s64 imm,
                                const char *base_reg,
                                const char *idx_reg,
                                u8 scale,
                                s32 disp)
{
    const char *mnem = x86_64GetSizedMov(size);
    char mem[48];
    x86_64FmtMemOperand(mem, sizeof(mem), disp, base_reg, idx_reg, scale);
    aoStrCatFmt(buf, "%s    $%I, %s\n\t", mnem, (s64)imm, mem);
}

/* Load any IrValue into the given 64-bit GP register.
 *
 * - Constants are materialised directly (no slot needed).
 * - String labels become PC-relative LEA.
 * - Tmp/local/param values either come from their pinned register
 *   (when one is set) or are loaded from their frame slot via
 *   x86_64Load. */
static void x86_64LoadToReg(IrCgCtx *ctx, IrValue *val, const char *reg) {
    switch (val->kind) {
        case IR_VAL_CONST_INT:
            /* Zero load: `xorl r32, r32` is 2 bytes and the implicit
             * zero-extend gives us a clean 64-bit zero. */
            if (val->as._i64 == 0) {
                char reg32[8];
                x86_64RegForWidth(reg, 4, reg32, sizeof(reg32));
                aoStrCatFmt(ctx->buf, "xorl    %%%s, %%%s\n\t",
                            reg32, reg32);
                break;
            }
            /* GNU as picks the 7-byte mov-imm32 or 10-byte movabs
             * encoding depending on whether the value fits in a
             * 32-bit sign-extended immediate. */
            aoStrCatFmt(ctx->buf, "movq    $%I, %%%s\n\t",
                        val->as._i64, reg);
            break;

        case IR_VAL_CONST_FLOAT:
            /* Load the raw bit pattern as an integer. Rarely needed
             * - float constants normally go through LoadToFpr but might
             *   be needed in some cases for bitcasts. */
            aoStrCatPrintf(ctx->buf, "movabsq $0x%lX, %%%s\n\t",
                           (u64)ieee754(val->as._f64), reg);
            break;

        case IR_VAL_CONST_STR:
            x86_64GlobalAddr(ctx, val->as.str.label->data, reg);
            break;

        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            if (val->pinned_reg) {
                aoStrCatFmt(ctx->buf, "movq    %%%s, %%%s\n\t",
                            val->pinned_reg->data, reg);
                break;
            }
            /* Regalloc parked the value in a physical reg, just a
             * reg-to-reg move (or nothing if it's already there). */
            if (val->loc.kind == IR_LOC_REG && val->loc.as.reg) {
                const char *src = val->loc.as.reg->data;
                if (strcmp(src, reg) != 0) {
                    aoStrCatFmt(ctx->buf, "movq    %%%s, %%%s\n\t", src, reg);
                }
                break;
            }
            u32 size = irValueByteSize(val);
            int loff = irCgGetLoff(&ctx->fn->ra, val);
            x86_64Load(ctx->buf, reg, size, loff);
            break;
        }
        default:
            loggerPanic("ir-cg-x86_64: cannot load value of kind %s\n",
                        irValueKindToString(val->kind));
    }
}

/* Store a 64-bit GP register into the destination IrValue's home.
 * If the destination is pinned, this is just a register-to-register
 * move; otherwise it's a width-aware store to the frame slot. */
static void x86_64StoreReg(IrCgCtx *ctx, IrValue *dst, const char *reg) {
    if (dst->pinned_reg) {
        aoStrCatFmt(ctx->buf, "movq    %%%s, %%%s\n\t",
                    reg, dst->pinned_reg->data);
        return;
    }
    /* Regalloc-assigned register: reg-to-reg move (or nothing). */
    if (dst->loc.kind == IR_LOC_REG && dst->loc.as.reg) {
        const char *home = dst->loc.as.reg->data;
        if (strcmp(home, reg) != 0) {
            aoStrCatFmt(ctx->buf, "movq    %%%s, %%%s\n\t",
                        reg, home);
        }
        return;
    }
    u32 size = irValueByteSize(dst);
    int loff = irCgGetLoff(&ctx->fn->ra, dst);
    x86_64FrameStoreWidth(ctx->buf, loff, size, reg);
}

/* Spill instr->dst into its home. When dst.loc is a register (set by
 * the peephole or the linear-scan), StoreReg's same-reg short-circuit
 * makes this a no-op move; when dst is a slot, it's a real store. */
static void x86_64SpillDst(IrCgCtx *ctx, IrInstr *instr, const char *reg) {
    x86_64StoreReg(ctx, instr->dst, reg);
}

/* Materialise a 64-bit double constant in .data and load it via
 * PC-relative addressing. x86_64 movsd <sym>(%rip), %xmm is a
 * single instruction. Bit-pattern dedup: identical doubles share a
 * single .LIRFn slot across the whole translation unit. */
static void x86_64EmitFloatLiteralData(IrCgCtx *ctx,
                                       const char *xmm_reg,
                                       f64 f)
{
    static int float_seq = 0;
    static Map *bits_to_label = NULL;
    if (!bits_to_label)
        bits_to_label = mapNew(16, &map_uint_to_uint_type);

    u64 bits = (u64)ieee754(f);
    AoStr *label = (AoStr *)mapGetInt(bits_to_label, bits);
    if (label) {
        /* Already in the data section, just reference it. */
        aoStrCatFmt(ctx->buf, "movsd   %S(%%rip), %%%s\n\t",
                    label, xmm_reg);
        return;
    }
    char buf[64];
    int n = snprintf(buf, sizeof(buf), ".LIRF%d", float_seq++);
    label = aoStrDupRaw(buf, (u64)n);
    mapAddIntOrErr(bits_to_label, bits, label);

    aoStrRemovePreviousChar(ctx->buf, '\t');
    aoStrCatPrintf(ctx->buf,
                   ".data\n\t.p2align 3\n%s:\n\t"
                   ".quad 0x%lX # %.9f\n"
                   ".text\n\t"
                   "movsd   %s(%%rip), %%%s\n\t",
                   label->data, (unsigned long)bits, f,
                   label->data, xmm_reg);
}

/* Load any IrValue into an xmm register. Integer constants are
 * coerced to double (matching aarch64). Float-typed locals/params
 * come from their frame slot via movsd. Pinned-reg variants are not
 * yet supported on the FP side. */
static void x86_64LoadToFpr(IrCgCtx *ctx, IrValue *val, const char *xmm_reg) {
    switch (val->kind) {
        case IR_VAL_CONST_FLOAT:
        case IR_VAL_CONST_INT: {
            double _f64 = val->kind == IR_VAL_CONST_INT ?
                                       (double)val->as._i64 :
                                       val->as._f64;
            /* +0.0 (bit pattern 0): xorpd is shorter than a rodata
             * load and leaves no .LIRF entry behind. -0.0 has the
             * sign bit set, so it must keep the rodata path. */
            if (ieee754(_f64) == 0) {
                aoStrCatFmt(ctx->buf, "xorpd   %%%s, %%%s\n\t",
                            xmm_reg, xmm_reg);
                break;
            }
            x86_64EmitFloatLiteralData(ctx, xmm_reg, _f64);
            break;
        }

        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            if (val->loc.kind == IR_LOC_REG && val->loc.as.reg) {
                const char *src = val->loc.as.reg->data;
                if (strcmp(src, xmm_reg) == 0) break;
                /* Cross-file move (int reg -> xmm) uses `movq`; same
                 * register file uses `movsd`. xmm reg names start
                 * with 'x'; everything else is a GP reg. */
                const char *mnem = (src[0] == 'x') ? "movsd" : "movq";
                aoStrCatFmt(ctx->buf, "%s   %%%s, %%%s\n\t",
                            mnem, src, xmm_reg);
                break;
            }
            int loff = irCgGetLoff(&ctx->fn->ra, val);
            aoStrCatFmt(ctx->buf, "movsd   %i(%%rbp), %%%s\n\t",
                        loff, xmm_reg);
            break;
        }
        default:
            loggerPanic("ir-cg-x86_64: cannot load float of kind %s\n",
                        irValueKindToString(val->kind));
    }
}

static void x86_64StoreFpr(IrCgCtx *ctx, IrValue *dst, const char *xmm_reg) {
    if (dst->loc.kind == IR_LOC_REG && dst->loc.as.reg) {
        const char *home = dst->loc.as.reg->data;
        if (strcmp(home, xmm_reg) == 0) return;
        const char *mnem = (home[0] == 'x') ? "movsd" : "movq";
        aoStrCatFmt(ctx->buf, "%s   %%%s, %%%s\n\t",
                    mnem, xmm_reg, home);
        return;
    }
    int loff = irCgGetLoff(&ctx->fn->ra, dst);
    aoStrCatFmt(ctx->buf, "movsd   %%%s, %i(%%rbp)\n\t", xmm_reg, loff);
}

/* Does this value fit in the 32-bit sign-extended immediate form that
 * most x86 integer ops accept (add/sub/and/or/xor/cmp etc.)? */
static int x86_64IsImm32(IrValue *val, s64 *out) {
    if (!val || !irIsConstInt(val)) return 0;
    s64 v = val->as._i64;
    if (v < INT32_MIN || v > INT32_MAX) return 0;
    *out = v;
    return 1;
}

/* Load src into %rax. When src.loc is already %rax (peephole-fused
 * producer), LoadToReg's same-reg check makes this a no-op. */
static void x86_64LoadFirstSrc(IrCgCtx *ctx, IrInstr *instr, IrValue *src) {
    (void)instr;
    x86_64LoadToReg(ctx, src, "rax");
}

/* Float counterpart. */
static void x86_64LoadFirstSrcFpr(IrCgCtx *ctx, IrInstr *instr, IrValue *src) {
    (void)instr;
    x86_64LoadToFpr(ctx, src, "xmm0");
}

static void x86_64SpillDstFpr(IrCgCtx *ctx, IrInstr *instr, const char *xmm_reg) {
    x86_64StoreFpr(ctx, instr->dst, xmm_reg);
}

/* Resolve a mem op's SIB idx to its physical register: use idx.loc
 * when set, otherwise load into %rdx. NULL if no scaled-idx. */
static const char *x86_64IdxReg(IrCgCtx *ctx, IrInstr *instr) {
    if (!instr->idx || !instr->scale) return NULL;
    if (instr->idx->loc.kind == IR_LOC_REG && instr->idx->loc.as.reg)
        return instr->idx->loc.as.reg->data;
    x86_64LoadToReg(ctx, instr->idx, "rdx");
    return "rdx";
}

/* The float branch uses ucomisd, whose flags follow unsigned-int
 * semantics (b/be/a/ae). NaN sets CF=ZF=PF=1; we treat IR cmps as
 * loose-ordered (matches aarch64's behaviour - the IR_CMP_O* /
 * IR_CMP_UNO / IR_CMP_ORD strict variants aren't currently emitted
 * by the HolyC frontend). */
static const char *x86_64CcFor(IrCmpKind cmp, int is_float) {
    if (is_float) {
        switch (cmp) {
            case IR_CMP_EQ: return "e";
            case IR_CMP_NE: return "ne";
            case IR_CMP_LT: return "b";
            case IR_CMP_LE: return "be";
            case IR_CMP_GT: return "a";
            case IR_CMP_GE: return "ae";
            default:        return NULL;
        }
    }
    switch (cmp) {
        case IR_CMP_EQ:  return "e";
        case IR_CMP_NE:  return "ne";
        case IR_CMP_LT:  return "l";
        case IR_CMP_LE:  return "le";
        case IR_CMP_GT:  return "g";
        case IR_CMP_GE:  return "ge";
        case IR_CMP_ULT: return "b";
        case IR_CMP_ULE: return "be";
        case IR_CMP_UGT: return "a";
        case IR_CMP_UGE: return "ae";
        default:         return NULL;
    }
}

/* Inverted condition for jumping when the cmp is FALSE. */
static const char *x86_64CcInvFor(IrCmpKind cmp, int is_float) {
    if (is_float) {
        switch (cmp) {
            case IR_CMP_EQ: return "ne";
            case IR_CMP_NE: return "e";
            case IR_CMP_LT: return "ae";
            case IR_CMP_LE: return "a";
            case IR_CMP_GT: return "be";
            case IR_CMP_GE: return "b";
            default:        return NULL;
        }
    }
    switch (cmp) {
        case IR_CMP_EQ:  return "ne";
        case IR_CMP_NE:  return "e";
        case IR_CMP_LT:  return "ge";
        case IR_CMP_LE:  return "g";
        case IR_CMP_GT:  return "le";
        case IR_CMP_GE:  return "l";
        case IR_CMP_ULT: return "ae";
        case IR_CMP_ULE: return "a";
        case IR_CMP_UGT: return "be";
        case IR_CMP_UGE: return "b";
        default:         return NULL;
    }
}

/* Materialise the EFLAGS-encoded comparison result as 0/1 in %rax.
 * setcc only writes the low byte, so we follow with a zero-extension
 * into the full 64-bit register. */
static void x86_64EmitSetCC(IrCgCtx *ctx, IrCmpKind cmp, int is_float) {
    const char *cc = x86_64CcFor(cmp, is_float);
    if (!cc) {
        loggerPanic("ir-cg-x86_64: unsupported cmp kind %s\n",
                    irCmpKindToString(cmp));
    }
    aoStrCatFmt(ctx->buf,
                "set%s    %%al\n\t"
                "movzbq  %%al, %%rax\n\t",
                cc);
}

/* Save each pinned register with `pushq`. If the count is odd, add a
 * trailing 8-byte pad so the stack stays 16-byte aligned for any
 * downstream call. The restore mirror reverses both halves. */
static void x86_64SavePinnedRegs(AoStr *buf, Vec *pinned) {
    if (!pinned || pinned->size == 0) return;
    for (u64 i = 0; i < pinned->size; ++i) {
        AoStr *r = (AoStr *)pinned->entries[i];
        aoStrCatFmt(buf, "pushq   %%%S\n\t", r);
    }
    if (pinned->size & 1) {
        aoStrCatFmt(buf, "subq    $8, %%rsp\n\t");
    }
}

static void x86_64RestorePinnedRegs(AoStr *buf, Vec *pinned) {
    if (!pinned || pinned->size == 0) return;
    if (pinned->size & 1) {
        aoStrCatFmt(buf, "addq    $8, %%rsp\n\t");
    }
    for (s64 i = (s64)pinned->size - 1; i >= 0; --i) {
        AoStr *r = (AoStr *)pinned->entries[i];
        aoStrCatFmt(buf, "popq    %%%S\n\t", r);
    }
}

/* leaveq is `movq %rbp, %rsp; popq %rbp` in one instruction. Saves a
 * byte over the explicit pair, no other difference. When the matching
 * prologue elided its rbp dance, emit just retq. */
static void x86_64Epilogue(IrCgCtx *ctx) {
    if (ctx->omit_frame) {
        aoStrCatFmt(ctx->buf, "retq\n");
    } else {
        aoStrCatFmt(ctx->buf,
                    "leaveq\n\t"
                    "retq\n");
    }
}

/* Did the last emit already place an epilogue at the tail of buf?
 * Used so we don't double-emit when the function ends with an
 * explicit return. */
static int x86_64HasRet(IrCgCtx *ctx) {
    static const char *with_frame    = "leaveq\n\tretq\n";
    static const char *without_frame = "retq\n";
    const char *needle = ctx->omit_frame ? without_frame : with_frame;
    int nlen = (int)strlen(needle);
    AoStr *buf = ctx->buf;
    if ((int)buf->len < nlen) return 0;
    return memcmp(buf->data + buf->len - nlen, needle, nlen) == 0;
}

static void x86_64BlockLabel(IrCgCtx *ctx, IrBlock *block, char *out, int n) {
    snprintf(out, n, ".LIRBB%d_%u", ctx->fn->uuid, block->id);
}

/* Move a single phi's incoming value into the phi's destination.
 * Dangling values (tmp with no slot) only arise from unreachable
 * incoming edges, zero out for safety. Phis here always materialise
 * via the slot: register-resident phi classification was removed
 * alongside the FUSE_TO_NEXT machinery. */
static void x86_64EmitOnePhi(IrCgCtx *ctx, IrInstr *phi, IrPair *match) {
    IrValue *v = match->ir_value;
    /* A tmp is "dangling" when it has neither a slot nor an
     * allocator-assigned register, which happens for short-circuit
     * shims that compute their value via control flow rather than a
     * real producer. Source it as zero. */
    int v_dangling = irIsTmp(v) &&
                     v->loc.kind != IR_LOC_REG &&
                     !mapHasInt(ctx->fn->ra.id_to_loff, irVarId(v));

    if (irIsFloat(phi->dst->type)) {
        if (v_dangling) {
            aoStrCatFmt(ctx->buf, "xorpd   %%xmm0, %%xmm0\n\t");
        } else {
            x86_64LoadToFpr(ctx, v, "xmm0");
        }
        x86_64StoreFpr(ctx, phi->dst, "xmm0");
        return;
    }
    if (v_dangling) {
        aoStrCatFmt(ctx->buf, "xorq    %%rax, %%rax\n\t");
    } else {
        x86_64LoadToReg(ctx, v, "rax");
    }
    x86_64StoreReg(ctx, phi->dst, "rax");
}

/* Materialise all phis at block `to` from predecessor `from`, ordered
 * so a phi never overwrites a value still needed by another pending
 * phi in the same group (parallel-copy semantics). Cycles among
 * phis would need a temporary swap; we panic loudly if one appears,
 * matching aarch64's behaviour. This is target-agnostic */
#define kMaxPhisX86 32
static void x86_64PhiMaterialise(IrCgCtx *ctx, IrBlock *from, IrBlock *to) {
    if (!to || !from) return;

    IrInstr *phis[kMaxPhisX86];
    IrPair  *pairs[kMaxPhisX86];
    int     done[kMaxPhisX86];
    int n = 0;

    listForEach(to->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_NOP) continue;
        if (I->op != IR_PHI) break;
        if (n >= kMaxPhisX86) {
            loggerPanic("ir-cg-x86_64: too many phis at one block "
                        "(>%d)\n", kMaxPhisX86);
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
                    read_by_pending = 1;
                    break;
                }
            }
            if (!read_by_pending) {
                x86_64EmitOnePhi(ctx, phis[i], pairs[i]);
                done[i] = 1;
                emitted++;
                progress = 1;
            }
        }
        if (!progress) {
            loggerPanic("ir-cg-x86_64: phi materialisation cycle "
                        "unimplemented\n");
        }
    }
}

/* SysV AMD64 has 6 int arg regs (rdi/rsi/rdx/rcx/r8/r9) and 8 FP
 * (xmm0-7); anything past that goes on the stack right-to-left
 * (we lay them out left-to-right at [rsp + i*8] which produces the
 * same memory image once the call instruction's return-address
 * push lands on top).
 *
 * HolyC's variadic convention sends every arg past var_arg_start
 * through the stack regardless of register availability (unlike
 * C-style variadic, which uses normal regs until they run out).
 *
 * Unlike aarch64, x86_64 has no separate indirect-result-location
 * register: when the callee returns by hidden pointer, args[0] is
 * that pointer and lives in %rdi like any first int arg. So we
 * don't special-case it here. */
static s32 x86_64PartitionCallArgs(u8 *is_stack, u64 n, Vec *args,
                                   int holyc_variadic,
                                   int var_arg_start)
{
    s32 n_stack_total = 0;
    if (holyc_variadic && (s64)n > var_arg_start) {
        for (u64 i = (u64)var_arg_start; i < n; ++i) {
            is_stack[i] = 1;
            n_stack_total++;
        }
    }
    int int_idx = 0, float_idx = 0;
    for (u64 i = 0; i < n; ++i) {
        if (is_stack[i]) continue;
        IrValue *a = vecGet(IrValue *, args, i);
        if (irIsFloat(a->type)) {
            if (float_idx >= 8) {
                is_stack[i] = 1;
                n_stack_total++;
            } else {
                float_idx++;
            }
        } else {
            if (int_idx >= 6) {
                is_stack[i] = 1;
                n_stack_total++;
            } else {
                int_idx++;
            }
        }
    }
    return n_stack_total;
}

static void x86_64EmitInstr(IrCgCtx *ctx, IrInstr *instr) {
    switch (instr->op) {
        /* No-ops at codegen: ALLOCA/GEP/PHI/LABEL are all resolved
         * elsewhere (alloca slots are assigned during layout, GEPs
         * fold into addressing modes, PHIs materialise at predecessor
         * edges, LABEL is dropped during IR resolution). */
        case IR_NOP:
        case IR_ALLOCA:
        case IR_PHI:
        case IR_GEP:
        case IR_LABEL:
            break;

        case IR_LOAD: {
            if (instr->r1 && instr->r1->pinned_reg) {
                aoStrCatFmt(ctx->buf, "movq    %%%s, %%rax\n\t",
                            instr->r1->pinned_reg->data);
                x86_64SpillDst(ctx, instr, "rax");
                break;
            }

            /* Float-typed loads go straight to xmm0 with movsd so a
             * fused FP consumer reads from the result reg directly. */
            if (instr->dst && irIsFloat(instr->dst->type)) {
                int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
                aoStrCatFmt(ctx->buf,
                            "movsd   %i(%%rbp), %%xmm0\n\t", loff);
                x86_64SpillDstFpr(ctx, instr, "xmm0");
                break;
            }

            int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
            u32 size = instr->dst ? instr->dst->as.var.size : 8;
            x86_64Load(ctx->buf, "rax", size, loff);
            x86_64SpillDst(ctx, instr, "rax");
            break;
        }

        case IR_STORE: {
            /* If r1 already lives in a register (e.g. a param's ABI
             * arg reg at function entry), store from there directly
             * instead of bouncing through %rax / %xmm0. */
            const char *src_reg = NULL;
            if (instr->r1 && instr->r1->loc.kind == IR_LOC_REG &&
                instr->r1->loc.as.reg)
            {
                src_reg = instr->r1->loc.as.reg->data;
            }

            /* Float r1 takes the FP path so a fused producer can hand
             * the value over in xmm0 (movsd xmm0->slot, no rax bounce). */
            if (instr->r1 && irIsFloat(instr->r1->type) &&
                !(instr->dst && instr->dst->pinned_reg))
            {
                if (!src_reg) {
                    x86_64LoadFirstSrcFpr(ctx, instr, instr->r1);
                    src_reg = "xmm0";
                }
                x86_64StoreFpr(ctx, instr->dst, src_reg);
                break;
            }

            /* Width comes from the value's size, not the destination's:
             * when dst is a GEP'd pointer to a sub-word slot (e.g.
             * `I8 a[3]`), forcing an 8-byte store would overrun the
             * slot and clobber adjacent stack data. */
            int loff = irCgGetLoff(&ctx->fn->ra, instr->dst);
            if (!src_reg) {
                x86_64LoadFirstSrc(ctx, instr, instr->r1);
                src_reg = "rax";
            }
            if (instr->dst && instr->dst->pinned_reg) {
                aoStrCatFmt(ctx->buf, "movq    %%%s, %%%s\n\t",
                            src_reg, instr->dst->pinned_reg->data);
                break;
            }
            u32 size = irValueByteSize(instr->r1);
            x86_64FrameStoreWidth(ctx->buf, loff, size, src_reg);
            break;
        }

        case IR_LOAD_DEREF: {
            /* Global address folded in by irFoldGlobalDeref: read the
             * symbol RIP-relative in a single instruction. Disp on a
             * GLOBAL operand has no current use (the IADD fusion only
             * runs when r1 is a TMP), so we assert it's zero. */
            if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
                const char *sym = instr->r1->as.global.name->data;
                if (instr->dst && irIsFloat(instr->dst->type)) {
                    aoStrCatFmt(ctx->buf, "movsd   %s(%%rip), %%xmm0\n\t", sym);
                    x86_64SpillDstFpr(ctx, instr, "xmm0");
                } else {
                    aoStrCatFmt(ctx->buf, "movq    %s(%%rip), %%rax\n\t",
                                sym);
                    x86_64SpillDst(ctx, instr, "rax");
                }
                break;
            }
            /* Address may already live in %rax from a fused producer.
             * Read r1's loc to skip the slot reload in that case. */
            const char *base_reg = "rcx";
            if (instr->r1 && instr->r1->loc.kind == IR_LOC_REG &&
                instr->r1->loc.as.reg)
            {
                base_reg = instr->r1->loc.as.reg->data;
            } else {
                x86_64LoadToReg(ctx, instr->r1, "rcx");
            }
            const char *idx_reg = x86_64IdxReg(ctx, instr);
            if (instr->dst && irIsFloat(instr->dst->type)) {
                char mem[48];
                x86_64FmtMemOperand(mem, sizeof(mem), instr->disp,
                                    base_reg, idx_reg, instr->scale);
                aoStrCatFmt(ctx->buf, "movsd   %s, %%xmm0\n\t", mem);
                x86_64SpillDstFpr(ctx, instr, "xmm0");
                break;
            }
            u32 size = instr->dst ? instr->dst->as.var.size : 8;
            x86_64DerefLoadWidth(ctx->buf, size, "rax", base_reg,
                                 idx_reg, instr->scale, instr->disp);
            x86_64SpillDst(ctx, instr, "rax");
            break;
        }

        case IR_STORE_DEREF: {
            /* Global address folded in by irFoldGlobalDeref: write
             * directly to sym(%rip). */
            if (instr->dst && instr->dst->kind == IR_VAL_GLOBAL) {
                const char *sym = instr->dst->as.global.name->data;
                u32 sz = irValueByteSize(instr->r1);
                if (instr->r1 && irIsFloat(instr->r1->type)) {
                    x86_64LoadFirstSrcFpr(ctx, instr, instr->r1);
                    aoStrCatFmt(ctx->buf, "movsd   %%xmm0, %s(%%rip)\n\t", sym);
                    break;
                }
                s64 imm;
                if (x86_64IsImm32(instr->r1, &imm)) {
                    const char *mnem = x86_64GetSizedMov(sz);
                    aoStrCatFmt(ctx->buf, "%s    $%I, %s(%%rip)\n\t",
                                mnem, (s64)imm, sym);
                    break;
                }
                x86_64LoadFirstSrc(ctx, instr, instr->r1);
                x86_64DerefStoreGlobal(ctx->buf, sz, "rax", sym);
                break;
            }

            /* Address may already live in a register thanks to the
             * peephole's loc-pinning. Pull it out of there directly
             * instead of bouncing through a slot reload. */
            const char *base_reg = "rcx";
            int addr_in_reg = instr->dst &&
                              instr->dst->loc.kind == IR_LOC_REG &&
                              instr->dst->loc.as.reg;
            if (addr_in_reg) {
                base_reg = instr->dst->loc.as.reg->data;
            }
            const char *idx_reg = x86_64IdxReg(ctx, instr);
            /* Float r1: value through xmm0. */
            if (instr->r1 && irIsFloat(instr->r1->type)) {
                if (!addr_in_reg) {
                    x86_64LoadToReg(ctx, instr->dst, "rcx");
                }
                x86_64LoadFirstSrcFpr(ctx, instr, instr->r1);
                char mem[48];
                x86_64FmtMemOperand(mem, sizeof(mem), instr->disp,
                                    base_reg, idx_reg, instr->scale);
                aoStrCatFmt(ctx->buf, "movsd   %%xmm0, %s\n\t", mem);
                break;
            }
            u32 sz = irValueByteSize(instr->r1);
            /* Immediate value: lay the constant down directly through
             * the address register (skip the rax bounce). */
            s64 imm;
            int has_imm = x86_64IsImm32(instr->r1, &imm);
            if (!addr_in_reg) {
                x86_64LoadToReg(ctx, instr->dst, "rcx");
            }
            if (has_imm) {
                x86_64DerefStoreImm(ctx->buf, sz, imm, base_reg,
                                    idx_reg, instr->scale, instr->disp);
                break;
            }
            /* General path: pick a val_reg that conflicts with
             * neither base_reg nor idx_reg. Defaults are rcx for the
             * "addr already in a reg" case, otherwise rax; bump to
             * rdx when an SIB-pinned idx has stolen our chosen slot
             * (e.g. idx fused from an immediate producer landed in
             * %rax). */
            const char *val_reg = addr_in_reg ? "rcx" : "rax";
            if (idx_reg && !strcmp(val_reg, idx_reg)) val_reg = "rdx";
            if (!strcmp(val_reg, base_reg)) val_reg = "rdx";
            x86_64LoadToReg(ctx, instr->r1, val_reg);
            x86_64DerefStoreWidth(ctx->buf, sz, val_reg, base_reg,
                                  idx_reg, instr->scale, instr->disp);
            break;
        }

        case IR_RMW_DEREF: {
            /* `*addr <op>= r1`. Build the memory-operand string for
             * the addressing mode, then emit through one shared
             * emitter that picks inc/dec / `op $imm, mem` / `op
             * %reg, mem` based on r1. */
            u32 sz = irValueByteSize(instr->r1);
            IrOp rop = instr->extra.rmw_op;
            char mem[64];
            const char *val_reg = "rax";

            if (instr->dst && instr->dst->kind == IR_VAL_GLOBAL) {
                snprintf(mem, sizeof(mem), "%s(%%rip)",
                         instr->dst->as.global.name->data);
            } else {
                const char *base_reg = "rcx";
                int addr_in_reg = instr->dst &&
                                  instr->dst->loc.kind == IR_LOC_REG &&
                                  instr->dst->loc.as.reg;
                if (addr_in_reg) base_reg = instr->dst->loc.as.reg->data;
                else             x86_64LoadToReg(ctx, instr->dst, "rcx");

                const char *idx_reg = x86_64IdxReg(ctx, instr);
                x86_64FmtMemOperand(mem, sizeof(mem), instr->disp,
                                    base_reg, idx_reg, instr->scale);
                /* Pick a val_reg not aliased with base/idx. */
                val_reg = addr_in_reg ? "rcx" : "rax";
                if (idx_reg && !strcmp(val_reg, idx_reg)) val_reg = "rdx";
                if (!strcmp(val_reg, base_reg)) val_reg = "rdx";
            }

            const char *sfx = x86_64SizedSuffix(sz);
            s64 imm;
            if (x86_64IsImm32(instr->r1, &imm)) {
                int inc = (rop == IR_IADD && imm ==  1) ||
                          (rop == IR_ISUB && imm == -1);
                int dec = (rop == IR_IADD && imm == -1) ||
                          (rop == IR_ISUB && imm ==  1);
                if (inc || dec) {
                    aoStrCatFmt(ctx->buf, "%s%s    %s\n\t",
                                inc ? "inc" : "dec", sfx, mem);
                } else {
                    aoStrCatFmt(ctx->buf, "%s%s    $%I, %s\n\t",
                                x86_64RmwMnem(rop), sfx, (s64)imm, mem);
                }
                break;
            }
            x86_64LoadToReg(ctx, instr->r1, val_reg);
            char sub[8];
            x86_64RegForWidth(val_reg, (int)sz, sub, sizeof(sub));
            aoStrCatFmt(ctx->buf, "%s%s    %%%s, %s\n\t",
                        x86_64RmwMnem(rop), sfx, sub, mem);
            break;
        }

        case IR_LEA: {
            x86_64Lea(ctx, instr, "rax");
            x86_64SpillDst(ctx, instr, "rax");
            break;
        }

        case IR_IADD:
        case IR_ISUB:
        case IR_AND:
        case IR_OR:
        case IR_XOR: {
            const char *op;
            switch (instr->op) {
                case IR_IADD: op = "addq"; break;
                case IR_ISUB: op = "subq"; break;
                case IR_AND:  op = "andq"; break;
                case IR_OR:   op = "orq";  break;
                case IR_XOR:  op = "xorq"; break;
                default: loggerPanic("Invalid op: %s\n",
                                     irOpcodeToString(instr));
            }
            x86_64LoadFirstSrc(ctx, instr, instr->r1);
            s64 imm;
            if (x86_64IsImm32(instr->r2, &imm)) {
                /* +/-1 with IADD/ISUB folds to incq/decq (3 bytes vs
                 * 4 for `addq $1, %rax`). Logical ops have no such
                 * single-operand form, so they stay on `op $imm`. */
                if ((instr->op == IR_IADD && imm == 1) ||
                    (instr->op == IR_ISUB && imm == -1))
                {
                    aoStrCatFmt(ctx->buf, "incq    %%rax\n\t");
                } else if ((instr->op == IR_IADD && imm == -1) ||
                           (instr->op == IR_ISUB && imm == 1))
                {
                    aoStrCatFmt(ctx->buf, "decq    %%rax\n\t");
                } else {
                    aoStrCatFmt(ctx->buf, "%s    $%I, %%rax\n\t",
                                op, (s64)imm);
                }
            } else if (instr->r2->loc.kind == IR_LOC_REG &&
                       instr->r2->loc.as.reg)
            {
                /* r2 already lives in a register (typically an ABI
                 * arg reg from a forwarded param store). Use it as
                 * the source operand directly, skips the rcx bounce. */
                aoStrCatFmt(ctx->buf, "%s    %%%s, %%rax\n\t",
                            op, instr->r2->loc.as.reg->data);
            } else {
                x86_64LoadToReg(ctx, instr->r2, "rcx");
                aoStrCatFmt(ctx->buf, "%s    %%rcx, %%rax\n\t", op);
            }
            x86_64SpillDst(ctx, instr, "rax");
            break;
        }

        case IR_IMUL: {
            x86_64LoadFirstSrc(ctx, instr, instr->r1);
            s64 imm;
            if (x86_64IsImm32(instr->r2, &imm)) {
                /* imulq's 3-operand form with a 32-bit immediate:
                 * dst = src * imm. */
                aoStrCatFmt(ctx->buf, "imulq   $%I, %%rax, %%rax\n\t",
                            (s64)imm);
            } else if (instr->r2->loc.kind == IR_LOC_REG &&
                       instr->r2->loc.as.reg)
            {
                aoStrCatFmt(ctx->buf, "imulq   %%%s, %%rax\n\t",
                            instr->r2->loc.as.reg->data);
            } else {
                x86_64LoadToReg(ctx, instr->r2, "rcx");
                aoStrCatFmt(ctx->buf, "imulq   %%rcx, %%rax\n\t");
            }
            x86_64SpillDst(ctx, instr, "rax");
            break;
        }

        case IR_IDIV:
        case IR_UDIV:
        case IR_IREM:
        case IR_UREM: {
            /* idiv/div divide rdx:rax by the source operand.
             * Sign-extend rax into rdx with cqo for signed, or zero
             * rdx for unsigned. Quotient lands in rax, remainder in
             * rdx; we move rdx into rax for the REM ops so SpillDst
             * always writes from rax. */
            x86_64LoadFirstSrc(ctx, instr, instr->r1);
            x86_64LoadToReg(ctx, instr->r2, "rcx");
            if (instr->op == IR_IDIV || instr->op == IR_IREM) {
                aoStrCatFmt(ctx->buf,
                            "cqo\n\t"
                            "idivq   %%rcx\n\t");
            } else {
                aoStrCatFmt(ctx->buf,
                            "xorq    %%rdx, %%rdx\n\t"
                            "divq    %%rcx\n\t");
            }
            if (instr->op == IR_IREM || instr->op == IR_UREM) {
                aoStrCatFmt(ctx->buf, "movq    %%rdx, %%rax\n\t");
            }
            x86_64SpillDst(ctx, instr, "rax");
            break;
        }

        case IR_SHL:
        case IR_SHR:
        case IR_SAR: {
            const char *op = (instr->op == IR_SHL) ? "shlq"
                           : (instr->op == IR_SAR) ? "sarq"
                                                   : "shrq";
            x86_64LoadFirstSrc(ctx, instr, instr->r1);
            if (irIsConstInt(instr->r2) &&
                instr->r2->as._i64 >= 0 &&
                instr->r2->as._i64 <= 63)
            {
                aoStrCatFmt(ctx->buf, "%s    $%I, %%rax\n\t",
                            op, (s64)instr->r2->as._i64);
            } else {
                /* Variable shift count must live in %cl. */
                x86_64LoadToReg(ctx, instr->r2, "rcx");
                aoStrCatFmt(ctx->buf, "%s    %%cl, %%rax\n\t", op);
            }
            x86_64SpillDst(ctx, instr, "rax");
            break;
        }

        case IR_INEG:
            x86_64LoadFirstSrc(ctx, instr, instr->r1);
            aoStrCatFmt(ctx->buf, "negq    %%rax\n\t");
            x86_64SpillDst(ctx, instr, "rax");
            break;

        case IR_NOT:
            x86_64LoadFirstSrc(ctx, instr, instr->r1);
            aoStrCatFmt(ctx->buf, "notq    %%rax\n\t");
            x86_64SpillDst(ctx, instr, "rax");
            break;

        case IR_FADD:
        case IR_FSUB:
        case IR_FMUL:
        case IR_FDIV: {
            const char *op = (instr->op == IR_FADD) ? "addsd"
                           : (instr->op == IR_FSUB) ? "subsd"
                           : (instr->op == IR_FMUL) ? "mulsd"
                                                    : "divsd";
            x86_64LoadFirstSrcFpr(ctx, instr, instr->r1);
            x86_64LoadToFpr(ctx, instr->r2, "xmm1");
            aoStrCatFmt(ctx->buf, "%s   %%xmm1, %%xmm0\n\t", op);
            x86_64SpillDstFpr(ctx, instr, "xmm0");
            break;
        }

        case IR_FNEG: {
            /* No fneg on SSE - flip the sign bit by XORing with
             * 0x8000000000000000. `sign_bit` is the shared global
             * emitted by the data-section pass. */
            x86_64LoadFirstSrcFpr(ctx, instr, instr->r1);
            aoStrCatFmt(ctx->buf,
                        "xorpd   sign_bit(%%rip), %%xmm0\n\t");
            x86_64SpillDstFpr(ctx, instr, "xmm0");
            break;
        }

        case IR_ICMP: {
            x86_64LoadFirstSrc(ctx, instr, instr->r1);
            s64 imm;
            if (x86_64IsImm32(instr->r2, &imm)) {
                /* AT&T cmpq A, B computes B - A; cc names mirror the
                 * natural ordering (e.g. cmpq $5, %rax + setg = rax>5). */
                aoStrCatFmt(ctx->buf, "cmpq    $%I, %%rax\n\t", (s64)imm);
            } else {
                x86_64LoadToReg(ctx, instr->r2, "rcx");
                aoStrCatFmt(ctx->buf, "cmpq    %%rcx, %%rax\n\t");
            }
            x86_64EmitSetCC(ctx, instr->extra.cmp_kind, 0);
            x86_64SpillDst(ctx, instr, "rax");
            break;
        }

        case IR_FCMP: {
            x86_64LoadFirstSrcFpr(ctx, instr, instr->r1);
            x86_64LoadToFpr(ctx, instr->r2, "xmm1");
            /* ucomisd %xmm1, %xmm0 sets flags from (xmm0 cmp xmm1). */
            aoStrCatFmt(ctx->buf, "ucomisd %%xmm1, %%xmm0\n\t");
            x86_64EmitSetCC(ctx, instr->extra.cmp_kind, 1);
            x86_64SpillDst(ctx, instr, "rax");
            break;
        }

        /* IR_TRUNC, SITOFP, UITOFP, FPTOSI, FPTOUI, BITCAST are
         * implemented. The remaining ZEXT/SEXT/FPTRUNC/FPEXT/
         * PTRTOINT/INTTOPTR aren't currently emitted by the IR
         * builder for HolyC programs. */
        case IR_TRUNC: {
            x86_64LoadFirstSrc(ctx, instr, instr->r1);
            u32 sz = instr->dst ? instr->dst->as.var.size : 8;
            switch (sz) {
                case 1: aoStrCatFmt(ctx->buf, "movzbq  %%al, %%rax\n\t"); break;
                case 2: aoStrCatFmt(ctx->buf, "movzwq  %%ax, %%rax\n\t"); break;
                case 4: /* movl into a 32-bit subreg zero-extends to 64 */
                        aoStrCatFmt(ctx->buf, "movl    %%eax, %%eax\n\t"); break;
                default: break;
            }
            x86_64SpillDst(ctx, instr, "rax");
            break;
        }

        case IR_SITOFP:
            x86_64LoadFirstSrc(ctx, instr, instr->r1);
            aoStrCatFmt(ctx->buf, "cvtsi2sdq %%rax, %%xmm0\n\t");
            x86_64SpillDstFpr(ctx, instr, "xmm0");
            break;

        case IR_UITOFP:
            /* x86 has no unsigned int-to-float; treat the source as
             * signed. True U64 -> double for values >= 2^63 would
             * need the signed-bit-fixup dance; not currently needed. */
            x86_64LoadFirstSrc(ctx, instr, instr->r1);
            aoStrCatFmt(ctx->buf, "cvtsi2sdq %%rax, %%xmm0\n\t");
            x86_64SpillDstFpr(ctx, instr, "xmm0");
            break;

        case IR_FPTOSI:
            x86_64LoadFirstSrcFpr(ctx, instr, instr->r1);
            aoStrCatFmt(ctx->buf, "cvttsd2siq %%xmm0, %%rax\n\t");
            x86_64SpillDst(ctx, instr, "rax");
            break;

        case IR_FPTOUI:
            /* Same simplification as UITOFP - signed truncation. */
            x86_64LoadFirstSrcFpr(ctx, instr, instr->r1);
            aoStrCatFmt(ctx->buf, "cvttsd2siq %%xmm0, %%rax\n\t");
            x86_64SpillDst(ctx, instr, "rax");
            break;

        case IR_BITCAST:
            /* Reinterpret 8 bytes between int and fp. x86's movq has a
             * dedicated xmm<->gp form for exactly this. */
            if (instr->dst && irIsFloat(instr->dst->type)) {
                x86_64LoadFirstSrc(ctx, instr, instr->r1);
                aoStrCatFmt(ctx->buf, "movq    %%rax, %%xmm0\n\t");
                x86_64SpillDstFpr(ctx, instr, "xmm0");
            } else {
                x86_64LoadFirstSrcFpr(ctx, instr, instr->r1);
                aoStrCatFmt(ctx->buf, "movq    %%xmm0, %%rax\n\t");
                x86_64SpillDst(ctx, instr, "rax");
            }
            break;

        case IR_ZEXT:
        case IR_SEXT: {
            /* Width-extending int-to-int. Source width comes from r1's
             * tmp size; we pick the matching movz/movs encoding. */
            x86_64LoadFirstSrc(ctx, instr, instr->r1);
            u32 src_sz = instr->r1 ? irValueByteSize(instr->r1) : 8;
            int signed_ext = (instr->op == IR_SEXT);
            switch (src_sz) {
                case 1:
                    aoStrCatFmt(ctx->buf,
                                signed_ext ? "movsbq  %%al, %%rax\n\t"
                                           : "movzbq  %%al, %%rax\n\t");
                    break;
                case 2:
                    aoStrCatFmt(ctx->buf,
                                signed_ext ? "movswq  %%ax, %%rax\n\t"
                                           : "movzwq  %%ax, %%rax\n\t");
                    break;
                case 4:
                    aoStrCatFmt(ctx->buf,
                                signed_ext ? "movslq  %%eax, %%rax\n\t"
                                           /* movl r32, r32 zero-extends */
                                           : "movl    %%eax, %%eax\n\t");
                    break;
                default:
                    /* src already 8 bytes: nothing to extend. */
                    break;
            }
            x86_64SpillDst(ctx, instr, "rax");
            break;
        }

        case IR_FPTRUNC: case IR_FPEXT:
        case IR_PTRTOINT: case IR_INTTOPTR:
            loggerPanic("ir-cg-x86_64: conversion op not yet implemented "
                        "(IR op %d) - not currently emitted by IR\n",
                        instr->op);

        case IR_RET:
            if (instr->dst) {
                if (irIsFloat(instr->dst->type)) {
                    x86_64LoadFirstSrcFpr(ctx, instr, instr->dst);
                } else {
                    /* Canonically XOR EAX for returning 0 */
                    if (irIsConstInt(instr->dst) && instr->dst->as._i64 == 0) {
                        aoStrCatFmt(ctx->buf, "xorl    %%eax, %%eax\n\t");
                    } else {
                        x86_64LoadFirstSrc(ctx, instr, instr->dst);
                    }
                }
            }
            x86_64RestorePinnedRegs(ctx->buf, ctx->pinned_regs);
            x86_64Epilogue(ctx);
            break;

        case IR_JMP: {
            IrBlock *target = instr->extra.blocks.target_block;
            x86_64PhiMaterialise(ctx, ctx->cur_block, target);
            if (target != ctx->next_block) {
                char tlbl[64];
                x86_64BlockLabel(ctx, target, tlbl, sizeof(tlbl));
                aoStrCatFmt(ctx->buf, "jmp     %s\n\t", tlbl);
            }
            break;
        }

        case IR_BR: {
            IrBlock *t = instr->extra.blocks.target_block;
            IrBlock *f = instr->extra.blocks.fallthrough_block;
            char tlbl[64], flbl[64];

            x86_64BlockLabel(ctx, t, tlbl, sizeof(tlbl));
            x86_64BlockLabel(ctx, f, flbl, sizeof(flbl));

            x86_64LoadFirstSrc(ctx, instr, instr->dst);

            int t_phi = irBlockHasPhi(t);
            int f_phi = irBlockHasPhi(f);
            if (!t_phi && !f_phi) {
                /* dst holds a 0/1 in %rax. testq sets ZF without
                 * clobbering rax; jz/jnz selects the path. */
                aoStrCatFmt(ctx->buf, "testq   %%rax, %%rax\n\t");
                if (ctx->next_block == t) {
                    aoStrCatFmt(ctx->buf, "jz      %s\n\t", flbl);
                } else if (ctx->next_block == f) {
                    aoStrCatFmt(ctx->buf, "jnz     %s\n\t", tlbl);
                } else {
                    aoStrCatFmt(ctx->buf,
                                "jz      %s\n\t"
                                "jmp     %s\n\t",
                                flbl, tlbl);
                }
            } else {
                /* One arm has phis: land at a dedicated shim that
                 * materialises them before the real jump. */
                static int br_seq = 0;
                char else_lbl[64];
                int br_id = br_seq++;
                snprintf(else_lbl, sizeof(else_lbl),
                         ".LIRBR%d_E%d", ctx->fn->uuid, br_id);
                aoStrCatFmt(ctx->buf,
                            "testq   %%rax, %%rax\n\t"
                            "jz      %s\n\t", else_lbl);
                x86_64PhiMaterialise(ctx, ctx->cur_block, t);
                aoStrCatFmt(ctx->buf, "jmp     %s\n", tlbl);
                aoStrRemovePreviousChar(ctx->buf, '\t');
                aoStrCatFmt(ctx->buf, "%s:\n\t", else_lbl);
                x86_64PhiMaterialise(ctx, ctx->cur_block, f);
                if (ctx->next_block != f) {
                    aoStrCatFmt(ctx->buf, "jmp     %s\n\t", flbl);
                }
            }
            break;
        }

        case IR_CMP_BR: {
            IrBlock *t = instr->extra.cmp_br.target_block;
            IrBlock *f = instr->extra.cmp_br.fallthrough_block;
            IrCmpKind kind = instr->extra.cmp_br.cmp_kind;
            int is_float = instr->r1 && irIsFloat(instr->r1->type);
            char tlbl[64], flbl[64];
            x86_64BlockLabel(ctx, t, tlbl, sizeof(tlbl));
            x86_64BlockLabel(ctx, f, flbl, sizeof(flbl));

            /* Emit the compare. Mirror IR_ICMP / IR_FCMP but skip the
             * setcc + spill since the result lives in EFLAGS for one
             * instruction. */
            if (is_float) {
                x86_64LoadFirstSrcFpr(ctx, instr, instr->r1);
                x86_64LoadToFpr(ctx, instr->r2, "xmm1");
                aoStrCatFmt(ctx->buf, "ucomisd %%xmm1, %%xmm0\n\t");
            } else {
                x86_64LoadFirstSrc(ctx, instr, instr->r1);
                s64 imm;
                if (x86_64IsImm32(instr->r2, &imm)) {
                    aoStrCatFmt(ctx->buf, "cmpq    $%I, %%rax\n\t",
                                (s64)imm);
                } else {
                    x86_64LoadToReg(ctx, instr->r2, "rcx");
                    aoStrCatFmt(ctx->buf, "cmpq    %%rcx, %%rax\n\t");
                }
            }

            const char *cc_t = x86_64CcFor(kind, is_float);
            const char *cc_f = x86_64CcInvFor(kind, is_float);
            int t_phi = irBlockHasPhi(t);
            int f_phi = irBlockHasPhi(f);
            if (!t_phi && !f_phi) {
                if (ctx->next_block == t) {
                    aoStrCatFmt(ctx->buf, "j%s     %s\n\t", cc_f, flbl);
                } else if (ctx->next_block == f) {
                    aoStrCatFmt(ctx->buf, "j%s     %s\n\t", cc_t, tlbl);
                } else {
                    aoStrCatFmt(ctx->buf,
                                "j%s     %s\n\t"
                                "jmp     %s\n\t",
                                cc_t, tlbl, flbl);
                }
            } else {
                static int cb_seq = 0;
                char else_lbl[64];
                int br_id = cb_seq++;
                snprintf(else_lbl, sizeof(else_lbl),
                         ".LIRCB%d_E%d", ctx->fn->uuid, br_id);
                aoStrCatFmt(ctx->buf, "j%s     %s\n\t", cc_f, else_lbl);
                x86_64PhiMaterialise(ctx, ctx->cur_block, t);
                aoStrCatFmt(ctx->buf, "jmp     %s\n", tlbl);
                aoStrRemovePreviousChar(ctx->buf, '\t');
                aoStrCatFmt(ctx->buf, "%s:\n\t", else_lbl);
                x86_64PhiMaterialise(ctx, ctx->cur_block, f);
                if (ctx->next_block != f) {
                    aoStrCatFmt(ctx->buf, "jmp     %s\n\t", flbl);
                }
            }
            break;
        }

        case IR_SWITCH:
            loggerPanic("ir-cg-x86_64: IR_SWITCH not currently emitted "
                        "by the IR builder\n");

        case IR_CALL: {
            /* Calls clobber all caller-saved regs. Any tmp the
             * tracker thinks is in %rax / %xmm0 must be considered
             * gone after the callq itself. The arg-loading sequence
             * below, though, may legitimately consume a still-live
             * fused tmp from the prior producer. We don't clear the
             * tracker yet. We zero it after the callq emit.
             * For an inndirect call: wrap->as.array.label is NULL and the
             * function-pointer source lives in instr->r2. Load it
             * into %r11 (scratch, outside the arg-reg range) BEFORE
             * the arg loads so subsequent arg evaluation can't
             * clobber the target. callq *%r11 is the indirect form. */
            IrValue *wrap = instr->r1;
            Vec *args = wrap ? wrap->as.array.values : NULL;
            AoStr *fname = wrap ? wrap->as.array.label : NULL;
            int indirect = (fname == NULL);
            if (indirect) {
                if (!instr->r2) {
                    loggerPanic("ir-cg-x86_64: indirect IR_CALL "
                                "without target\n");
                }
                x86_64LoadToReg(ctx, instr->r2, "r11");
            }

            Ast *callee = NULL;
            if (!indirect) {
                callee = (Ast *)mapGetLen(ctx->cc->global_env,
                                          fname->data, fname->len);
            }
            int callee_va = 0;
            if (callee) {
                if ((callee->type && callee->type->has_var_args) ||
                    callee->has_var_args)
                {
                    callee_va = 1;
                } else if (callee->params && callee->params->size > 0) {
                    Ast *last = vecGet(Ast *, callee->params,
                                       callee->params->size - 1);
                    if (last && last->kind == AST_VAR_ARGS)
                        callee_va = 1;
                }
            }
            int holyc_variadic = callee_va && callee &&
                                 callee->kind != AST_EXTERN_FUNC;
            int extern_variadic = callee_va && callee &&
                                  callee->kind == AST_EXTERN_FUNC;

            int var_arg_start = -1;
            if (holyc_variadic && callee->params) {
                for (u64 i = 0; i < callee->params->size; ++i) {
                    Ast *p = vecGet(Ast *, callee->params, i);
                    var_arg_start++;
                    if (p && p->kind == AST_VAR_ARGS)
                        break;
                }
                var_arg_start += 1;
            }

            u64 n = args ? args->size : 0;
            u8 *is_stack = (n > 0) ? (u8 *)malloc(sizeof(u8) * n) : NULL;
            if (is_stack) memset(is_stack, 0, n);
            s32 n_stack_total = x86_64PartitionCallArgs(
                    is_stack, n, args, holyc_variadic, var_arg_start);

            /* Stack args sit in 8-byte lanes; SysV requires the
             * stack to be 16-byte aligned at the call instruction
             * (and the call pushes 8 bytes for the return address),
             * so the region we sub off has to be a multiple of 16. */
            s32 stack_bytes = n_stack_total * 8;
            if (stack_bytes & 15) stack_bytes += 8;
            if (stack_bytes > 0) {
                aoStrCatFmt(ctx->buf, "subq    $%i, %%rsp\n\t", stack_bytes);
            }

            /* If arg[0]'s producer left it in a register (its
             * IrValue->loc is REG), route it into the first ABI
             * int-arg register *before* the stack-arg loop, that
             * loop uses %rax as scratch and would clobber an
             * %rax-resident value. Only fires for an int arg[0] in
             * a reg slot. */
            int arg0_in_reg = 0;
            if (n > 0 && !is_stack[0]) {
                IrValue *a0 = vecGet(IrValue *, args, 0);
                if (a0 && !irIsFloat(a0->type) &&
                    a0->loc.kind == IR_LOC_REG && a0->loc.as.reg)
                {
                    IrRegPool *pool = irRegPoolGet();
                    const char *src = a0->loc.as.reg->data;
                    const char *dst_reg =
                        vecGet(AoStr *, pool->int_arg_regs, 0)->data;
                    if (strcmp(src, dst_reg) != 0) {
                        aoStrCatFmt(ctx->buf, "movq    %%%s, %%%s\n\t",
                                    src, dst_reg);
                    }
                    arg0_in_reg = 1;
                }
            }

            /* Place stack args at [rsp + i*8] in source order. */
            s32 stack_idx = 0;
            for (u64 i = 0; i < n; ++i) {
                if (!is_stack[i]) continue;
                IrValue *a = vecGet(IrValue *, args, i);
                s32 slot_off = stack_idx * 8;
                if (irIsFloat(a->type)) {
                    x86_64LoadToFpr(ctx, a, "xmm0");
                    aoStrCatFmt(ctx->buf, "movsd   %%xmm0, %i(%%rsp)\n\t", slot_off);
                } else {
                    x86_64LoadToReg(ctx, a, "rax");
                    aoStrCatFmt(ctx->buf, "movq    %%rax, %i(%%rsp)\n\t", slot_off);
                }
                stack_idx++;
            }

            /* Reg args. Hidden struct-return pointer (when present)
             * is args[0] and naturally lands in %rdi. No special
             * handling needed (unlike aarch64's x8). The backend's
             * IrRegPool descriptor supplies the ABI mapping so this
             * loop stays target-agnostic. */
            IrRegPool *pool = irRegPoolGet();
            int int_idx = arg0_in_reg ? 1 : 0;
            int float_idx = 0;
            int xmm_used = 0;
            for (u64 i = 0; i < n; ++i) {
                if (is_stack[i]) continue;
                if (i == 0 && arg0_in_reg) continue;
                IrValue *a = vecGet(IrValue *, args, i);
                if (irIsFloat(a->type)) {
                    if ((u64)float_idx >= pool->float_arg_regs->size) {
                        loggerPanic("ir-cg-x86_64: too many float args\n");
                    }
                    AoStr *r = vecGet(AoStr *, pool->float_arg_regs,
                                      float_idx++);
                    x86_64LoadToFpr(ctx, a, r->data);
                    xmm_used++;
                } else {
                    if ((u64)int_idx >= pool->int_arg_regs->size) {
                        loggerPanic("ir-cg-x86_64: too many int args\n");
                    }
                    AoStr *r = vecGet(AoStr *, pool->int_arg_regs, int_idx++);
                    x86_64LoadToReg(ctx, a, r->data);
                }
            }

            /* SysV variadic ABI: %al = number of XMM regs used.
             * Required by C-variadic callees so their va_start can
             * spill the right registers; cheap to set unconditionally
             * for any variadic call. */
            if (extern_variadic || holyc_variadic) {
                aoStrCatFmt(ctx->buf, "movb    $%i, %%al\n\t", xmm_used);
            }

            if (indirect) {
                aoStrCatFmt(ctx->buf, "callq   *%%r11\n\t");
            } else {
                aoStrCatFmt(ctx->buf, "callq   %s\n\t",
                            asmNormaliseFunctionName(ctx->cc, fname));
            }
            /* The call clobbered all caller-saved regs; any prior
             * FUSE_TO_NEXT producer's value is gone. */
            if (stack_bytes > 0) {
                aoStrCatFmt(ctx->buf, "addq    $%i, %%rsp\n\t", stack_bytes);
            }

            if (instr->dst && instr->dst->type != IR_TYPE_VOID &&
                (instr->dst->kind == IR_VAL_TMP ||
                 instr->dst->kind == IR_VAL_LOCAL ||
                 instr->dst->kind == IR_VAL_PARAM))
            {
                if (irIsFloat(instr->dst->type)) {
                    x86_64SpillDstFpr(ctx, instr, "xmm0");
                } else {
                    x86_64SpillDst(ctx, instr, "rax");
                }
            }
            if (is_stack) free(is_stack);
            break;
        }

        /* Inline `asm { ... }` block. When fragments are present
         * (the parser saw `&var` references in the asm body) walk
         * them and substitute `<loff>(%rbp)` for each LVAR_REF using
         * the lvar's now-known stack offset. Otherwise paste the
         * plain text. Mirrors aarch64's IR_ASM case modulo the
         * frame-relative syntax. */
        case IR_ASM: {
            aoStrRemovePreviousChar(ctx->buf, '\t');
            if (instr->extra.asm_fragments) {
                listForEach(instr->extra.asm_fragments) {
                    AsmFragment *f = (AsmFragment *)it->value;
                    if (f->kind == ASM_FRAG_TEXT) {
                        if (f->text) {
                            aoStrCatLen(ctx->buf,
                                        f->text->data,
                                        f->text->len);
                        }
                    } else if (f->kind == ASM_FRAG_LVAR_REF) {
                        if (f->lvar) {
                            aoStrCatFmt(ctx->buf,
                                        "%i(%%rbp)",
                                        f->lvar->loff);
                        }
                    }
                }
            } else if (instr->r1 && instr->r1->as.str.str) {
                aoStrCatLen(ctx->buf,
                            instr->r1->as.str.str->data,
                            instr->r1->as.str.str->len);
            }
            aoStrPutChar(ctx->buf, '\n');
            aoStrPutChar(ctx->buf, '\t');
            break;
        }

        /* IR_SELECT and IR_VA_* aren't currently emitted by the IR
         * builder for HolyC programs. Panic if they ever start appearing so
         * we notice. Select could be interesting to avoid branching */
        case IR_SELECT:
        case IR_VA_ARG: case IR_VA_START: case IR_VA_END:
            loggerPanic("ir-cg-x86_64: IR op %d not yet implemented "
                        "(not currently emitted by IR builder; "
                        "matches aarch64)\n", instr->op);
    }

}

void x86_64PasteDataSection(Cctrl *cc, AoStr *buf) {
    /* sign_bit / one_dbl are shared FP scratch globals - referenced
     * by FNEG and convert-float ops. */
    aoStrCatFmt(buf, "sign_bit:\n\t.quad 0x8000000000000000\n");
    aoStrCatFmt(buf, "one_dbl:\n\t.double 1.0\n");

    MapIter it;
    mapIterInit(cc->strs, &it);
    while (mapIterNext(&it)) {
        MapNode *n = it.node;
        Ast *ast = (Ast *)n->value;
        assert(ast->kind == AST_STRING);
        aoStrCatFmt(buf,
                    "%S:\n\t"
                    ".asciz \"%S\"\n",
                    ast->slabel, ast->sval);
    }
    aoStrPutChar(buf, '\t');
}

static void x86_64EmitFunctionPrologue(Cctrl *cc,
                                       AoStr *buf,
                                       Ast *func,
                                       u16 total_stack,
                                       Vec *pinned,
                                       int omit_frame)
{
    char *fname = asmNormaliseFunctionName(cc, func->fname);
    /* .p2align 4 = 16-byte aligned function entries (x86_64
     * convention for the I-cache fetch boundary). */
    aoStrCatFmt(buf,
                ".text\n\t"
                ".p2align 4\n\t"
                ".globl %s\n"
                "%s:\n\t",
                fname, fname);
    /* Leaf function with empty frame: skip the rbp dance entirely
     * (saves 3 instructions per call). Caller-saved rbp survives
     * untouched, no slot accesses to anchor, no callee-saved regs
     * to preserve. The matching epilogue is just `retq`. */
    if (omit_frame) return;

    aoStrCatFmt(buf,
                "pushq   %%rbp\n\t"
                "movq    %%rsp, %%rbp\n\t");
    if (total_stack > 0) {
        /* Round up to 16 so rsp stays aligned for inner calls. The
         * call instruction pushes 8 bytes, our pushq %rbp pushes
         * another 8 - so the frame base is already aligned; the
         * subq has to preserve that. */
        u32 aligned = ((u32)total_stack + 15u) & ~15u;
        aoStrCatFmt(buf, "subq    $%u, %%rsp\n\t", aligned);
    }
    /* Pinned saves AFTER reserving locals so rbp-relative offsets
     * for slots stay valid throughout the function body. */
    x86_64SavePinnedRegs(buf, pinned);
}

void x86_64GenerateFunction(IrCgCtx *ctx, Ast *ast) {
    AoStr *buf = ctx->buf;
    IrFunction *fn = ctx->fn;

    /* Collect pinned-register set used by this function's locals
     * AND params (same sweep aarch64 does). The prologue saves;
     * each ret restores. */
    ctx->pinned_regs = NULL;
    if (ast->locals) {
        listForEach(ast->locals) {
            Ast *l = (Ast *)it->value;
            if (!l || l->kind != AST_LVAR) continue;
            if (l->pinned_kind != LVAR_REG) continue;
            if (!l->pinned_reg) continue;
            if (!ctx->pinned_regs) ctx->pinned_regs = irValueVecNew();
            vecPush(ctx->pinned_regs, l->pinned_reg);
        }
    }
    if (ast->params) {
        for (u64 i = 0; i < ast->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast->params, i);
            if (!p || p->kind != AST_LVAR) continue;
            if (p->pinned_kind != LVAR_REG) continue;
            if (!p->pinned_reg) continue;
            int seen = 0;
            if (ctx->pinned_regs) {
                for (u64 k = 0; k < ctx->pinned_regs->size; ++k) {
                    AoStr *r = (AoStr *)ctx->pinned_regs->entries[k];
                    if (r == p->pinned_reg) { seen = 1; break; }
                }
            }
            if (seen) continue;
            if (!ctx->pinned_regs) ctx->pinned_regs = irValueVecNew();
            vecPush(ctx->pinned_regs, p->pinned_reg);
        }
    }

    /* Leaf function with zero frame: no stack slots, no pinned regs
     * to save, no calls (so we don't need rsp realignment) and no
     * VARGS area. Skip the rbp prologue/epilogue. */
    ctx->omit_frame = (fn->stack_space == 0)
                      && (ctx->pinned_regs == NULL
                          || ctx->pinned_regs->size == 0)
                      && !irFnHasCalls(fn)
                      && (ast->loff == 0);

    x86_64EmitFunctionPrologue(ctx->cc, buf, ast,
                               fn->stack_space,
                               ctx->pinned_regs,
                               ctx->omit_frame);

    Set *referenced = irCgComputeReferencedBlocks(fn);

    listForEach(fn->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        ctx->cur_block = block;
        ctx->next_block = (it->next != fn->blocks)
                         ? (IrBlock *)it->next->value
                         : NULL;
        if (setHas(referenced, (void *)(u64)block->id)) {
            char lbl[64];
            x86_64BlockLabel(ctx, block, lbl, sizeof(lbl));
            aoStrRemovePreviousChar(buf, '\t');
            aoStrCatPrintf(buf, "%s:\n\t", lbl);
        }
        listForEach(block->instructions) {
            IrInstr *instr = (IrInstr *)it->value;
            x86_64EmitInstr(ctx, instr);
        }
    }
    setRelease(referenced);

    /* If the function body didn't end with an explicit ret, emit
     * the epilogue ourselves so we don't fall through. */
    if (!x86_64HasRet(ctx)) {
        x86_64RestorePinnedRegs(buf, ctx->pinned_regs);
        x86_64Epilogue(ctx);
    }
}

void x86_64InitialiseEmptyGlobal(Cctrl *cc,
                                 AoStr *buf,
                                 Ast *global,
                                 int zerofill)
{
    AoStr *label = global->is_static ? global->glabel : global->gname;
    int size = global->type->size;

    if (zerofill && (cc->target == TARGET_AARCH64_APPLE_DARWIN ||
                     cc->target == TARGET_X86_64_APPLE_DARWIN))
    {
        aoStrCatFmt(buf, ".globl %S\n\t", label);
        aoStrCatFmt(buf, ".zerofill __DATA,__common,%S,%i,%i\n\t",
                    label, size, (int)log2((double)size));
    } else {
        aoStrCatFmt(buf, ".globl %S\n\t.comm %S, %i, %u\n\t",
                    label, label, size,
                    roundUpToNextPowerOf2((unsigned long)size));
    }
}

static void x86_64DataInternal(AoStr *buf, Ast *data) {
    if (data->kind == AST_STRING) {
        aoStrCatPrintf(buf, ".quad %s\n\t", data->slabel->data);
        return;
    }
    if (data->kind == AST_ARRAY_INIT) {
        listForEach(data->arrayinit) {
            x86_64DataInternal(buf, (Ast *)it->value);
        }
        return;
    }
    if (data->type->kind == AST_TYPE_FLOAT) {
        aoStrCatPrintf(buf, ".quad 0x%lX #%.9f\n\t",
                       ieee754(data->f64), data->f64);
        return;
    }
    switch (data->type->size) {
        case 1: aoStrCatPrintf(buf, ".byte %d\n\t", data->i64); break;
        case 4: aoStrCatPrintf(buf, ".long %d\n\t", data->i64); break;
        case 8: aoStrCatPrintf(buf, ".quad %d\n\t", data->i64); break;
        default:
            loggerPanic("Cannot create size information for: %s\n",
                        astToString(data));
    }
}

void x86_64GlobalVar(Cctrl *cc,
                     Set *seen_globals,
                     AoStr *buf,
                     Ast *ast)
{
    Ast *declvar = ast->declvar;
    Ast *declinit = ast->declinit;
    AoStr *varname = declvar->gname;
    AoStr *label = declvar->is_static ? declvar->glabel : declvar->gname;

    if (ast->flags & AST_FLAG_EXTERN) return;
    if (declvar->flags & AST_FLAG_EXTERN) return;
    if (setHasLen(seen_globals, varname->data, varname->len)) return;

    aoStrRemovePreviousChar(buf, '\t');
    setAdd(seen_globals, varname->data);

    if (declinit &&
        (declinit->kind == AST_ARRAY_INIT ||
         declinit->kind == AST_LITERAL ||
         declinit->kind == AST_STRING))
    {
        if (declinit->kind == AST_STRING) {
            aoStrCatFmt(buf,
                        "%S:\n\t.asciz \"%S\"\n\t",
                        declvar->gname, declinit->sval);
            return;
        }
        if (!declvar->is_static) {
            aoStrCatFmt(buf, ".globl %S\n", label);
        }
        aoStrCatPrintf(buf, ".data\n");
        if (declinit->kind == AST_ARRAY_INIT) {
            Ast *head = (Ast *)declinit->arrayinit->next->value;
            if (head->kind == AST_STRING) {
                aoStrCatPrintf(buf, ".align 4\n");
            }
        }
        aoStrCatFmt(buf, "%S:\n\t", label);
        if (declinit->kind == AST_ARRAY_INIT) {
            listForEach(declinit->arrayinit) {
                x86_64DataInternal(buf, (Ast *)it->value);
            }
            return;
        }
        x86_64DataInternal(buf, declinit);
    } else {
        if (declvar->is_static) {
            aoStrCatFmt(buf, ".lcomm %S, %i\n\t", label,
                        declvar->type->size);
        } else {
            /* argc/argv are linked from the startup file - use .comm
             * not .zerofill so the linker doesn't double-allocate. */
            if (label->len == 4 &&
                (!strncmp(label->data, str_lit("argc")) ||
                 !strncmp(label->data, str_lit("argv"))))
            {
                x86_64InitialiseEmptyGlobal(cc, buf, declvar, 0);
            } else {
                x86_64InitialiseEmptyGlobal(cc, buf, declvar, 1);
            }
        }
    }
}

static void x86_64PasteAsmBlocks(AoStr *buf, Cctrl *cc) {
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
            aoStrCatPrintf(buf, ".text\n");
            aoStrCatPrintf(buf, ".globl %s\n",
                           asm_func->asmfname->data);
            aoStrCatPrintf(buf, "%s:\n", asm_func->asmfname->data);
            aoStrCatLen(buf,
                        asm_func->body->asm_stmt->data,
                        asm_func->body->asm_stmt->len);
            aoStrPutChar(buf, '\n');
        }
    }
}

AoStr *x86_64AsmGenerate(Cctrl *cc) {
    AoStr *buf = aoStrAlloc(2048);
    x86_64InitRegPool();
    x86_64PasteDataSection(cc, buf);
    x86_64PasteAsmBlocks(buf, cc);

    /* Top-level statements parsed into cc->initalisers become the
     * body of a synthetic `main`. CCTRL_ASM_HAS_INITIALISERS makes
     * asmNormaliseFunctionName rename the user's `Main` to `MainFn`
     * so the symbols don't collide. */
    Ast *synth_main = NULL;
    if (!listEmpty(cc->initalisers)) {
        cc->flags |= CCTRL_ASM_HAS_INITIALISERS;
        listAppend(cc->initalisers,
                   astReturn(astI64Type(0), ast_i32_type));
        Ast *body = astCompountStatement(cc->initalisers);
        Vec *empty_params = astVecNew();
        AstType *fn_type = astMakeFunctionType(ast_i32_type, empty_params);
        synth_main = astFunction(fn_type, "main", 4, empty_params, body,
                                 cc->initaliser_locals, 0);
    }

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
            x86_64GenerateFunction(&ctx, ast);
        } else if (ast->kind == AST_DECL || ast->kind == AST_GVAR) {
            x86_64GlobalVar(cc, seen_globals, buf, ast);
        }
    }

    if (synth_main) {
        IrFunction *fn = irLowerFunction(ir_ctx, synth_main);
        irBasicFunctionOptimisations(fn);
        irFunctionPrepForCodeGen(&ctx, fn, synth_main);
        x86_64GenerateFunction(&ctx, synth_main);
    }

    aoStrCatFmt(buf, ".ident      \"hcc: %s %s %s hash: %s\"\n",
                OS_STR,
                cliTargetToString(cc->target),
                cctrlGetVersion(),
                HCC_GIT_HASH);
    setRelease(seen_globals);
    return buf;
}
