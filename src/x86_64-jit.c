/* x86_64 in-process JIT backend.
 *
 * Lowers IR straight to native instruction bytes via the libtasm x86_64
 * direct-emit encoders, then maps the result RX and exposes public function
 * entries by name. Mirrors src/x86_64.c case-for-case: same register roles
 * (rax/rcx/rdx scratch, xmm0/xmm1 FP scratch, r11 indirect-call target),
 * same omit_frame rule, same frame layout, same addressing-mode fusion,
 * same SysV AMD64 + HolyC variadic convention. The only material
 * difference is global addressing: at JIT time we know absolute
 * addresses, so a global reference materialises via movabs instead of
 * RIP-relative loads (saves a fixup).
 *
 *   IR -> enc_x86_64 direct emitters -> AsmEnc.bytes + AsmFixup list
 *   AsmEnc -> asm_jit_finalize -> RX mapping, branches patched
 *   public labels exposed as `name -> entry pointer` via x86_64JitLookup
 *
 * Inter-function calls and external symbols share one mechanism: emit a
 * CALL rel32 placeholder + AF_SYMBOL fixup. asm_jit_finalize matches
 * internal labels first, then resolves externals via jitResolveSymbol
 * (host override -> dlsym). Out-of-range targets get a movabs+jmp veneer. */

#include "x86_64-jit.h"

#if !defined(__x86_64__) || !defined(HCC_ENABLE_JIT)

HccJit *x86_64JitCompile(Cctrl *cc) { (void)cc; return NULL; }

#else /* __x86_64__ && HCC_ENABLE_JIT */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdio.h>

#include "aostr.h"
#include "ast.h"
#include "asm.h"
#include "asm/asm_enc.h"
#include "asm/asm_jit.h"
#include "asm/enc_x86_64.h"
#include "cli.h"
#include "containers.h"
#include "ir.h"
#include "ir-optimise.h"
#include "ir-regalloc.h"
#include "list.h"
#include "util.h"

/* Per-function emission context. Same shape as IrCgCtx in spirit but
 * carries JIT-specific bits (the encoder, the local-label counter). */
typedef struct {
    HccJit *jit;
    IrFunction *fn;
    Ast *ast;
    IrBlock *cur_block;
    IrBlock *next_block;
    int omit_frame;
} JitFnCtx;

/* SysV integer arg registers, ordered as arg 0..5. */
static const X86Reg kIntArgRegs[] = {
    R_RDI, R_RSI, R_RDX, R_RCX, R_R8, R_R9
};

/* ---------------- forward decls ---------------- */

static void jitEmitInstr(JitFnCtx *ctx, IrInstr *instr);
static void jitLoadToReg(JitFnCtx *ctx, IrValue *val, X86Reg reg);
static void jitLoadToFpr(JitFnCtx *ctx, IrValue *val, int xmm);
static void jitStoreReg(JitFnCtx *ctx, IrValue *dst, X86Reg reg);
static void jitStoreFpr(JitFnCtx *ctx, IrValue *dst, int xmm);
static void jitPhiMaterialise(JitFnCtx *ctx, IrBlock *from, IrBlock *to);

/* ---------------- value-kind helpers ---------------- */

static int jitIsImm32(IrValue *v, s64 *out) {
    if (!v || v->kind != IR_VAL_CONST_INT) return 0;
    s64 i = v->as._i64;
    if (i < INT32_MIN || i > INT32_MAX) return 0;
    *out = i;
    return 1;
}

/* Parse a register name like "rax", "r8", "xmm0" into its hardware
 * number. Used to translate the pinned-reg / IR_LOC_REG `AoStr *reg`
 * fields into encoder regs. */
static X86Reg jitRegFromName(const char *name) {
    static const struct { const char *n; X86Reg r; } tab[] = {
        {"rax", R_RAX}, {"rcx", R_RCX}, {"rdx", R_RDX}, {"rbx", R_RBX},
        {"rsp", R_RSP}, {"rbp", R_RBP}, {"rsi", R_RSI}, {"rdi", R_RDI},
        {"r8",  R_R8},  {"r9",  R_R9},  {"r10", R_R10}, {"r11", R_R11},
        {"r12", R_R12}, {"r13", R_R13}, {"r14", R_R14}, {"r15", R_R15},
    };
    if (!name || !*name) return R_RAX;
    if (!strncasecmp(name, "xmm", 3)) {
        int n = atoi(name + 3);
        if (n < 0 || n > 15) return 0;
        return (X86Reg)n;
    }
    for (size_t i = 0; i < sizeof(tab)/sizeof(tab[0]); ++i) {
        if (!strcasecmp(name, tab[i].n)) return tab[i].r;
    }
    return R_RAX;
}

static int jitIsFpRegName(const char *name) {
    return name && !strncasecmp(name, "xmm", 3);
}

/* ---------------- immediate materialise ---------------- */

/* Emit `reg = imm`. Zero gets the canonical xorl (which also zeroes the
 * high half); everything else a movabs. Never emitted between a compare
 * and its consuming jcc/setcc, so the flags clobber is safe. */
static void jitEmitMovImm(AsmEnc *enc, X86Reg reg, s64 imm) {
    if (imm == 0) {
        x86_64_enc_xor32_reg(enc, reg);
        return;
    }
    x86_64_enc_movabsq_imm_reg(enc, reg, (uint64_t)imm);
}

/* ---------------- frame load/store ----------------
 *
 * x86 mem operands carry a full signed disp32, so unlike aarch64 there
 * are no range/alignment forms to pick between - every local is
 * directly addressable off rbp. */

static void jitFrameLoad(AsmEnc *enc, X86Reg reg, int size, int loff) {
    x86_64_enc_load_mem(enc, size, reg, R_RBP, -1, 0, loff);
}

static void jitFrameStore(AsmEnc *enc, X86Reg reg, int size, int loff) {
    x86_64_enc_store_mem(enc, size, reg, R_RBP, -1, 0, loff);
}

/* 1 for a double-width (F64) float value, 0 for single (F32). */
static int jitFpDbl(IrValue *v) { return (int)irValueByteSize(v) == 8; }

static void jitFpFrameLoad(AsmEnc *enc, int xmm, int loff, int is_dbl) {
    if (is_dbl) x86_64_enc_movsd_load(enc, xmm, R_RBP, -1, 0, loff);
    else        x86_64_enc_movss_load(enc, xmm, R_RBP, -1, 0, loff);
}

static void jitFpFrameStore(AsmEnc *enc, int xmm, int loff, int is_dbl) {
    if (is_dbl) x86_64_enc_movsd_store(enc, xmm, R_RBP, -1, 0, loff);
    else        x86_64_enc_movss_store(enc, xmm, R_RBP, -1, 0, loff);
}

/* ---------------- global addressing ----------------
 *
 * Three kinds of symbol need different treatment:
 *
 *  - Internal functions (defined in this TU): their address is the JIT
 *    mapping base + the label's byte_offset, but we don't know the base
 *    at emit time. Emit `lea sym(%rip), reg` with a SIGNED rel32 fixup;
 *    the finalize step matches the label in enc->labels by name and
 *    patches the displacement (always reachable - same mapping).
 *
 *  - Internal globals/strings (registered in host_symbols by
 *    jitAllocateGlobals): we know the absolute address NOW because
 *    they live in a host malloc'd arena. Emit movabs with the
 *    materialised address.
 *
 *  - External functions (printf/MAlloc/etc., dlsym-discoverable):
 *    materialise via movabs with the dlsym address. Don't use
 *    RIP-relative here -- libc may be beyond rel32's +/-2GB reach. */

static void jitGlobalAddr(HccJit *jit, X86Reg reg, const char *sym) {
    /* Internal function -> lea rip-rel with a SIGNED rel32 fixup. */
    if (hccJitIsInternalFunc(jit, sym)) {
        size_t disp_off = x86_64_enc_lea_rip_rel_reg(&jit->enc, reg);
        AsmFixup f = {
            .kind = AF_SYMBOL,
            .reloc = AFR_X86_64_SIGNED,
            .patch_offset = disp_off,
            .width = 4, .pcrel = 1,
            .sym = strdup(sym),
        };
        asm_add_fixup(&jit->enc, f);
        return;
    }
    /* Internal global / string / external function: address is known
     * now (host arena malloc, or dlsym for libc). Materialise it. */
    void *addr = mapGet(jit->host_symbols, (void *)sym);
    if (!addr) addr = asm_jit_dlsym_resolver(NULL, sym);
    if (!addr) {
        jitEmitMovImm(&jit->enc, reg, 0);
        return;
    }
    x86_64_enc_movabsq_imm_reg(&jit->enc, reg, (uint64_t)(uintptr_t)addr);
}

/* ---------------- load/store IR values ---------------- */

static void jitLoadToReg(JitFnCtx *ctx, IrValue *val, X86Reg reg) {
    AsmEnc *enc = &ctx->jit->enc;
    switch (val->kind) {
        case IR_VAL_CONST_INT:
            jitEmitMovImm(enc, reg, val->as._i64);
            break;
        case IR_VAL_CONST_FLOAT:
            /* Rare bit-cast: materialise the bit pattern. */
            jitEmitMovImm(enc, reg, (s64)(u64)ieee754_64(val->as._f64));
            break;
        case IR_VAL_CONST_STR:
            jitGlobalAddr(ctx->jit, reg, val->as.str.label->data);
            break;
        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            if (val->pinned_reg) {
                X86Reg src = jitRegFromName(val->pinned_reg->data);
                if (src != reg) x86_64_enc_mov_reg_reg(enc, reg, src);
                break;
            }
            if (val->loc.kind == IR_LOC_REG && val->loc.as.reg) {
                X86Reg src = jitRegFromName(val->loc.as.reg->data);
                if (src != reg) x86_64_enc_mov_reg_reg(enc, reg, src);
                break;
            }
            int size = (int)irValueByteSize(val);
            int loff = irCgGetLoff(&ctx->fn->ra, val);
            jitFrameLoad(enc, reg, size, loff);
            break;
        }
        case IR_VAL_GLOBAL:
            jitGlobalAddr(ctx->jit, reg, val->as.global.name->data);
            break;
        default:
            loggerPanic("jit-x86_64: cannot load int value of kind %d\n", val->kind);
    }
}

static void jitLoadToFpr(JitFnCtx *ctx, IrValue *val, int xmm) {
    AsmEnc *enc = &ctx->jit->enc;
    switch (val->kind) {
        case IR_VAL_CONST_FLOAT:
        case IR_VAL_CONST_INT: {
            double f = val->kind == IR_VAL_CONST_INT
                       ? (double)val->as._i64
                       : val->as._f64;
            int is_dbl = (val->kind == IR_VAL_CONST_FLOAT) ? jitFpDbl(val) : 1;
            u64 bits = is_dbl ? (u64)ieee754_64(f)
                              : (u64)(u32)ieee754_32((f32)f);
            if (bits == 0) {
                if (is_dbl) x86_64_enc_xorpd(enc, xmm, xmm);
                else        x86_64_enc_xorps(enc, xmm, xmm);
                break;
            }
            /* Materialise via rax = bit pattern, then movq/movd to xmm. */
            jitEmitMovImm(enc, R_RAX, (s64)bits);
            if (is_dbl) x86_64_enc_movq_gpr_xmm(enc, xmm, R_RAX);
            else        x86_64_enc_movd_gpr_xmm(enc, xmm, R_RAX);
            break;
        }
        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            int is_dbl = jitFpDbl(val);
            /* TempleOS-pinned FP local: the value lives in the named
             * register (xmm for FP, a GPR holding the bits otherwise). */
            if (val->pinned_reg) {
                X86Reg src = jitRegFromName(val->pinned_reg->data);
                if (jitIsFpRegName(val->pinned_reg->data)) {
                    if ((int)src != xmm) {
                        if (is_dbl) x86_64_enc_movsd_xmm_xmm(enc, xmm, (int)src);
                        else        x86_64_enc_movss_xmm_xmm(enc, xmm, (int)src);
                    }
                } else {
                    if (is_dbl) x86_64_enc_movq_gpr_xmm(enc, xmm, src);
                    else        x86_64_enc_movd_gpr_xmm(enc, xmm, src);
                }
                break;
            }
            if (val->loc.kind == IR_LOC_REG && val->loc.as.reg) {
                X86Reg src = jitRegFromName(val->loc.as.reg->data);
                if (jitIsFpRegName(val->loc.as.reg->data)) {
                    if ((int)src != xmm) {
                        if (is_dbl) x86_64_enc_movsd_xmm_xmm(enc, xmm, (int)src);
                        else        x86_64_enc_movss_xmm_xmm(enc, xmm, (int)src);
                    }
                } else {
                    if (is_dbl) x86_64_enc_movq_gpr_xmm(enc, xmm, src);
                    else        x86_64_enc_movd_gpr_xmm(enc, xmm, src);
                }
                break;
            }
            int loff = irCgGetLoff(&ctx->fn->ra, val);
            jitFpFrameLoad(enc, xmm, loff, is_dbl);
            break;
        }
        default:
            loggerPanic("jit-x86_64: cannot load fp value of kind %d\n", val->kind);
    }
}

static void jitStoreReg(JitFnCtx *ctx, IrValue *dst, X86Reg reg) {
    AsmEnc *enc = &ctx->jit->enc;
    if (dst->pinned_reg) {
        X86Reg home = jitRegFromName(dst->pinned_reg->data);
        if (home != reg) x86_64_enc_mov_reg_reg(enc, home, reg);
        return;
    }
    if (dst->loc.kind == IR_LOC_REG && dst->loc.as.reg) {
        X86Reg home = jitRegFromName(dst->loc.as.reg->data);
        if (home != reg) x86_64_enc_mov_reg_reg(enc, home, reg);
        return;
    }
    int size = (int)irValueByteSize(dst);
    int loff = irCgGetLoff(&ctx->fn->ra, dst);
    jitFrameStore(enc, reg, size, loff);
}

/* `val_size` (0 = derive from dst) is the stored value's width. When dst is
 * a gep/pointer tmp aliasing a narrow field, sizing from dst (8 bytes) would
 * clobber the neighbouring field; pass the value width so a F32 field store
 * is a 4-byte movss not an 8-byte movsd. */
static void jitStoreFprSized(JitFnCtx *ctx, IrValue *dst, int xmm,
                             int val_size) {
    AsmEnc *enc = &ctx->jit->enc;
    int is_dbl = val_size > 0 ? (val_size == 8) : jitFpDbl(dst);
    if (dst->pinned_reg) {
        X86Reg home = jitRegFromName(dst->pinned_reg->data);
        if (jitIsFpRegName(dst->pinned_reg->data)) {
            if ((int)home != xmm) {
                if (is_dbl) x86_64_enc_movsd_xmm_xmm(enc, (int)home, xmm);
                else        x86_64_enc_movss_xmm_xmm(enc, (int)home, xmm);
            }
        } else {
            if (is_dbl) x86_64_enc_movq_xmm_gpr(enc, home, xmm);
            else        x86_64_enc_movd_xmm_gpr(enc, home, xmm);
        }
        return;
    }
    if (dst->loc.kind == IR_LOC_REG && dst->loc.as.reg) {
        X86Reg home = jitRegFromName(dst->loc.as.reg->data);
        if (jitIsFpRegName(dst->loc.as.reg->data)) {
            if ((int)home != xmm) {
                if (is_dbl) x86_64_enc_movsd_xmm_xmm(enc, (int)home, xmm);
                else        x86_64_enc_movss_xmm_xmm(enc, (int)home, xmm);
            }
        } else {
            if (is_dbl) x86_64_enc_movq_xmm_gpr(enc, home, xmm);
            else        x86_64_enc_movd_xmm_gpr(enc, home, xmm);
        }
        return;
    }
    int loff = irCgGetLoff(&ctx->fn->ra, dst);
    jitFpFrameStore(enc, xmm, loff, is_dbl);
}

static void jitStoreFpr(JitFnCtx *ctx, IrValue *dst, int xmm) {
    AsmEnc *enc = &ctx->jit->enc;
    int is_dbl = jitFpDbl(dst);
    /* Pinned FP local: write the value into the named register. */
    if (dst->pinned_reg) {
        X86Reg home = jitRegFromName(dst->pinned_reg->data);
        if (jitIsFpRegName(dst->pinned_reg->data)) {
            if ((int)home != xmm) {
                if (is_dbl) x86_64_enc_movsd_xmm_xmm(enc, (int)home, xmm);
                else        x86_64_enc_movss_xmm_xmm(enc, (int)home, xmm);
            }
        } else {
            if (is_dbl) x86_64_enc_movq_xmm_gpr(enc, home, xmm);
            else        x86_64_enc_movd_xmm_gpr(enc, home, xmm);
        }
        return;
    }
    if (dst->loc.kind == IR_LOC_REG && dst->loc.as.reg) {
        X86Reg home = jitRegFromName(dst->loc.as.reg->data);
        if (jitIsFpRegName(dst->loc.as.reg->data)) {
            if ((int)home != xmm) {
                if (is_dbl) x86_64_enc_movsd_xmm_xmm(enc, (int)home, xmm);
                else        x86_64_enc_movss_xmm_xmm(enc, (int)home, xmm);
            }
        } else {
            if (is_dbl) x86_64_enc_movq_xmm_gpr(enc, home, xmm);
            else        x86_64_enc_movd_xmm_gpr(enc, home, xmm);
        }
        return;
    }
    int loff = irCgGetLoff(&ctx->fn->ra, dst);
    jitFpFrameStore(enc, xmm, loff, is_dbl);
}

static void jitSpillDst(JitFnCtx *ctx, IrInstr *instr, X86Reg reg) {
    jitStoreReg(ctx, instr->dst, reg);
}

static void jitSpillDstFpr(JitFnCtx *ctx, IrInstr *instr, int xmm) {
    jitStoreFpr(ctx, instr->dst, xmm);
}

static void jitLoadFirstSrc(JitFnCtx *ctx, IrValue *src) {
    jitLoadToReg(ctx, src, R_RAX);
}

static void jitLoadFirstSrcFpr(JitFnCtx *ctx, IrValue *src) {
    jitLoadToFpr(ctx, src, 0 /* xmm0 */);
}

/* idx register for addressing-mode fusion. Returns 1 if `*out` was
 * populated, 0 if no idx component. */
static int jitIdxReg(JitFnCtx *ctx, IrInstr *instr, X86Reg *out) {
    if (!instr->idx || !instr->scale) return 0;
    if (instr->idx->loc.kind == IR_LOC_REG && instr->idx->loc.as.reg) {
        *out = jitRegFromName(instr->idx->loc.as.reg->data);
        return 1;
    }
    jitLoadToReg(ctx, instr->idx, R_RDX);
    *out = R_RDX;
    return 1;
}

/* ---------------- condition codes ----------------
 *
 * The float column follows ucomisd, whose flags use unsigned-int
 * semantics (b/be/a/ae). NaN sets CF=ZF=PF=1; we treat IR cmps as
 * loose-ordered (matches the AOT backend and aarch64). */

static int jitCcFor(IrCmpKind cmp, int is_float) {
    if (is_float) {
        switch (cmp) {
            case IR_CMP_EQ: return X86_CC_E;
            case IR_CMP_NE: return X86_CC_NE;
            case IR_CMP_LT: return X86_CC_B;
            case IR_CMP_LE: return X86_CC_BE;
            case IR_CMP_GT: return X86_CC_A;
            case IR_CMP_GE: return X86_CC_AE;
            default: loggerPanic("jit-x86_64: bad float cmp %d\n", cmp);
        }
    }
    switch (cmp) {
        case IR_CMP_EQ:  return X86_CC_E;
        case IR_CMP_NE:  return X86_CC_NE;
        case IR_CMP_LT:  return X86_CC_L;
        case IR_CMP_LE:  return X86_CC_LE;
        case IR_CMP_GT:  return X86_CC_G;
        case IR_CMP_GE:  return X86_CC_GE;
        case IR_CMP_ULT: return X86_CC_B;
        case IR_CMP_ULE: return X86_CC_BE;
        case IR_CMP_UGT: return X86_CC_A;
        case IR_CMP_UGE: return X86_CC_AE;
        default: loggerPanic("jit-x86_64: bad int cmp %d\n", cmp);
    }
}

static int jitCcInvFor(IrCmpKind cmp, int is_float) {
    /* All the codes we use pair up as cc ^ 1 (e/ne, b/ae, l/ge, ...). */
    return jitCcFor(cmp, is_float) ^ 1;
}

static void jitEmitSetCC(AsmEnc *enc, IrCmpKind cmp, int is_float) {
    x86_64_enc_setcc_al(enc, jitCcFor(cmp, is_float));
    x86_64_enc_movzbq_al_rax(enc);
}

/* ---------------- phi materialisation ---------------- */

static void jitEmitOnePhi(JitFnCtx *ctx, IrInstr *phi, IrPair *match) {
    IrValue *v = match ? match->ir_value : NULL;
    int v_dangling = irIsTmp(v) &&
                     v->loc.kind != IR_LOC_REG &&
                     !mapHasInt(ctx->fn->ra.id_to_loff, irVarId(v));
    if (irIsFloat(phi->dst->type)) {
        if (v_dangling) {
            x86_64_enc_xorpd(&ctx->jit->enc, 0, 0);
        } else {
            jitLoadToFpr(ctx, v, 0 /* xmm0 */);
        }
        jitStoreFpr(ctx, phi->dst, 0 /* xmm0 */);
        return;
    }
    if (v_dangling) {
        x86_64_enc_xor32_reg(&ctx->jit->enc, R_RAX);
    } else {
        jitLoadToReg(ctx, v, R_RAX);
    }
    jitStoreReg(ctx, phi->dst, R_RAX);
}

static void jitEmitOnePhiCb(void *ud, IrInstr *phi, IrPair *match) {
    jitEmitOnePhi((JitFnCtx *)ud, phi, match);
}

static void jitPhiMaterialise(JitFnCtx *ctx, IrBlock *from, IrBlock *to) {
    hccJitPhiMaterialise(from, to, jitEmitOnePhiCb, ctx);
}

/* ---------------- call args partition ----------------
 *
 * HolyC's variadic convention sends every arg past var_arg_start
 * through the stack regardless of register availability (unlike
 * C-style variadic, which uses normal regs until they run out). */

static s32 jitPartitionCallArgs(u8 *is_stack, u64 n, Vec *args,
                                int holyc_variadic, int var_arg_start)
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
            if (float_idx >= 8) { is_stack[i] = 1; n_stack_total++; }
            else float_idx++;
        } else {
            if (int_idx >= 6) { is_stack[i] = 1; n_stack_total++; }
            else int_idx++;
        }
    }
    return n_stack_total;
}

/* ---------------- branches ---------------- */

/* Emit `jmp <local>` placeholder + AF_LOCAL fixup. */
static void jitEmitBranchLocal(JitFnCtx *ctx, int local_num) {
    size_t off = x86_64_enc_jmp_rel32(&ctx->jit->enc);
    hccJitAddLocalBranchFixup(&ctx->jit->enc, off, local_num, AFR_X86_64_JMP32);
}

/* Emit `jcc <local>` placeholder + AF_LOCAL fixup. */
static void jitEmitJccLocal(JitFnCtx *ctx, int cc, int local_num) {
    size_t off = x86_64_enc_jcc_rel32(&ctx->jit->enc, cc);
    hccJitAddLocalBranchFixup(&ctx->jit->enc, off, local_num, AFR_X86_64_JCC32);
}

/* ---------------- System V struct-by-value passing ----------------
 * Mirrors x86_64.c's x86_64EmitSysvCall / x86_64EmitSysvParamPrologue for
 * the JIT (machine-code) path: eightbyte INTEGER->GP / SSE->XMM, MEMORY
 * (>16B) on the stack by value. Struct returns keep the hidden-out-ptr. */

static int jitIsByvalStruct(AstType *t) {
    return t && (t->kind == AST_TYPE_CLASS || t->kind == AST_TYPE_UNION) &&
           !t->is_intrinsic;
}

static void jitEmitSysvCall(JitFnCtx *ctx, IrInstr *instr, Vec *args,
                            AoStr *fname, int indirect)
{
    AsmEnc *enc = &ctx->jit->enc;
    u64 n = args ? args->size : 0;

    /* A <=16-byte aggregate return comes back in registers; its destination
     * buffer (args[0]) is not an ABI argument. */
    AstType *ret_st = (instr->flags & IRCG_CALL_AGG_RETURN) && instr->dst
                      ? instr->dst->byval_struct_type : NULL;
    int reg_ret = ret_st && ret_st->size <= 16;
    u64 a0 = reg_ret ? 1 : 0;

    /* Pre-pass: size the stack-argument area (MEMORY / register-overflow
     * aggregates + overflow scalars). */
    int nsaa_total = 0;
    {
        int g = 0, s = 0;
        for (u64 i = a0; i < n; ++i) {
            IrValue *a = vecGet(IrValue *, args, i);
            if (a->byval_struct_type) {
                AstType *t = a->byval_struct_type;
                SysvClass cl[2]; int neb = 0;
                int mem = astSysvClassify(t, cl, &neb);
                int gp = 0, sse = 0;
                if (!mem)
                    for (int e = 0; e < neb; ++e)
                        if (cl[e] == SYSV_INTEGER) gp++; else sse++;
                if (!mem && g + gp <= 6 && s + sse <= 8) { g += gp; s += sse; }
                else nsaa_total += (t->size + 7) & ~7;
            } else if (irIsFloat(a->type)) {
                if (s < 8) s++; else nsaa_total += 8;
            } else {
                if (g < 6) g++; else nsaa_total += 8;
            }
        }
    }
    int dest_save_off = nsaa_total;
    int stack_bytes = (nsaa_total + (reg_ret ? 16 : 0) + 15) & ~15;
    if (stack_bytes > 0) x86_64_enc_subq_imm_reg(enc, R_RSP, stack_bytes);
    int stack_off = 0;

    if (reg_ret) {
        /* Compute the destination buffer address into r10 and stash it
         * across the call (r10 is caller-saved). */
        jitLoadToReg(ctx, vecGet(IrValue *, args, 0), R_R10);
        x86_64_enc_store_mem(enc, 8, R_R10, R_RSP, -1, 0, dest_save_off);
    }

    int ngp = 0, nsse = 0;
    for (u64 i = a0; i < n; ++i) {
        IrValue *a = vecGet(IrValue *, args, i);
        if (a->byval_struct_type) {
            AstType *t = a->byval_struct_type;
            SysvClass cl[2]; int neb = 0;
            int mem = astSysvClassify(t, cl, &neb);
            int gp = 0, sse = 0;
            if (!mem)
                for (int e = 0; e < neb; ++e)
                    if (cl[e] == SYSV_INTEGER) gp++; else sse++;
            jitLoadToReg(ctx, a, R_R10); /* struct address */
            if (mem || ngp + gp > 6 || nsse + sse > 8) {
                int words = (t->size + 7) / 8;
                for (int k = 0; k < words; ++k) {
                    int off = k * 8;
                    int rem = t->size - off;
                    int sz = rem >= 8 ? 8 : rem;
                    x86_64_enc_load_mem(enc, sz, R_RAX, R_R10, -1, 0, off);
                    x86_64_enc_store_mem(enc, sz, R_RAX, R_RSP, -1, 0,
                                         stack_off + off);
                }
                stack_off += (t->size + 7) & ~7;
                continue;
            }
            for (int e = 0; e < neb; ++e) {
                int off = e * 8;
                int rem = t->size - off;
                int sz = rem >= 8 ? 8 : rem;
                if (cl[e] == SYSV_SSE) {
                    if (sz == 4) x86_64_enc_movss_load(enc, nsse, R_R10, -1, 0, off);
                    else         x86_64_enc_movsd_load(enc, nsse, R_R10, -1, 0, off);
                    nsse++;
                } else {
                    x86_64_enc_load_mem(enc, sz, kIntArgRegs[ngp], R_R10,
                                        -1, 0, off);
                    ngp++;
                }
            }
            continue;
        }
        if (irIsFloat(a->type)) {
            if (nsse < 8) {
                jitLoadToFpr(ctx, a, nsse++);
            } else {
                jitLoadToFpr(ctx, a, 0 /*xmm0*/);
                if (jitFpDbl(a)) x86_64_enc_movsd_store(enc, 0, R_RSP, -1, 0, stack_off);
                else             x86_64_enc_movss_store(enc, 0, R_RSP, -1, 0, stack_off);
                stack_off += 8;
            }
        } else {
            if (ngp < 6) {
                jitLoadToReg(ctx, a, kIntArgRegs[ngp++]);
            } else {
                jitLoadToReg(ctx, a, R_RAX);
                x86_64_enc_store_mem(enc, 8, R_RAX, R_RSP, -1, 0, stack_off);
                stack_off += 8;
            }
        }
    }

    if (indirect) {
        x86_64_enc_call_reg(enc, R_R11);
    } else {
        char *normalised = asmNormaliseFunctionName(ctx->jit->cc, fname);
        size_t off = x86_64_enc_call_rel32(enc);
        hccJitAddCallFixup(enc, off, normalised, AFR_X86_64_CALL32);
    }
    if (reg_ret) {
        /* Reload the destination buffer and copy the SysV result registers
         * (INTEGER eightbytes -> rax,rdx; SSE -> xmm0,xmm1) into it. */
        SysvClass cl[2]; int neb = 0;
        astSysvClassify(ret_st, cl, &neb);
        int gpret[2] = { R_RAX, R_RDX };
        int gpi = 0, ssei = 0;
        x86_64_enc_load_mem(enc, 8, R_R10, R_RSP, -1, 0, dest_save_off);
        for (int e = 0; e < neb; ++e) {
            int off = e * 8, rem = (int)ret_st->size - off;
            int sz = rem >= 8 ? 8 : rem;
            if (cl[e] == SYSV_SSE) {
                if (sz == 4) x86_64_enc_movss_store(enc, ssei, R_R10, -1, 0, off);
                else         x86_64_enc_movsd_store(enc, ssei, R_R10, -1, 0, off);
                ssei++;
            } else {
                x86_64_enc_store_mem(enc, sz, gpret[gpi], R_R10, -1, 0, off);
                gpi++;
            }
        }
    }

    if (stack_bytes > 0) x86_64_enc_addq_imm_reg(enc, R_RSP, stack_bytes);

    if (reg_ret) {
        if (instr->dst) jitSpillDst(ctx, instr, R_R10);  /* dst = buffer addr */
    } else if (instr->dst && instr->dst->type != IR_TYPE_VOID) {
        if (irIsFloat(instr->dst->type)) jitSpillDstFpr(ctx, instr, 0);
        else                             jitSpillDst(ctx, instr, R_RAX);
    }
}

static void jitEmitSysvParamPrologue(JitFnCtx *ctx) {
    Ast *ast = ctx->ast;
    if (!ast->params) return;
    AsmEnc *enc = &ctx->jit->enc;
    int ngp = 0, nsse = 0;

    AstType *rt = ast->type ? ast->type->rettype : NULL;
    if (jitIsByvalStruct(rt) && rt->size > 0) ngp = 1; /* hidden ret ptr */
    int incoming_off = 0;

    for (u64 i = 0; i < ast->params->size; ++i) {
        Ast *p = vecGet(Ast *, ast->params, i);
        if (p->kind == AST_VAR_ARGS) break;
        AstType *t = p->type;

        IrValue *slot = irFnGetVar(ctx->fn, irGetParamId(p));
        int has_slot = slot &&
            mapHasInt(ctx->fn->ra.id_to_loff, irVarId(slot));
        int loff = has_slot ? irCgGetLoff(&ctx->fn->ra, slot) : 0;

        if (!jitIsByvalStruct(t)) {
            int is_float = (t->kind == AST_TYPE_FLOAT);
            if (is_float ? (nsse < 8) : (ngp < 6)) {
                if (is_float) nsse++; else ngp++; /* reg: arrive+store */
            } else {
                if (has_slot) {
                    int sz = t->size;
                    x86_64_enc_load_mem(enc, sz, R_RAX, R_RBP, -1, 0,
                                        16 + incoming_off);
                    jitFrameStore(enc, R_RAX, sz, loff);
                }
                incoming_off += 8;
            }
            continue;
        }

        SysvClass cl[2]; int neb = 0;
        int mem = astSysvClassify(t, cl, &neb);

        int gp = 0, sse = 0;
        if (!mem)
            for (int e = 0; e < neb; ++e)
                if (cl[e] == SYSV_INTEGER) gp++; else sse++;

        if (mem || ngp + gp > 6 || nsse + sse > 8) {
            if (has_slot) {
                int words = (t->size + 7) / 8;
                for (int k = 0; k < words; ++k) {
                    int off = k * 8;
                    int rem = t->size - off;
                    int sz = rem >= 8 ? 8 : rem;
                    x86_64_enc_load_mem(enc, sz, R_RAX, R_RBP, -1, 0,
                                        16 + incoming_off + off);
                    jitFrameStore(enc, R_RAX, sz, loff + off);
                }
            }
            incoming_off += (t->size + 7) & ~7;
            continue;
        }

        for (int e = 0; e < neb; ++e) {
            int off = e * 8;
            int rem = t->size - off;
            int sz = rem >= 8 ? 8 : rem;
            if (cl[e] == SYSV_SSE) {
                if (has_slot) jitFpFrameStore(enc, nsse, loff + off, sz == 8);
                nsse++;
            } else {
                if (has_slot)
                    jitFrameStore(enc, kIntArgRegs[ngp], sz, loff + off);
                ngp++;
            }
        }
    }
}

/* ---------------- the main IR emitter ---------------- */

static void jitEmitInstr(JitFnCtx *ctx, IrInstr *instr) {
    HccJit *jit = ctx->jit;
    AsmEnc *enc = &jit->enc;

    switch (instr->op) {
        case IR_NOP:
        case IR_LABEL:
        case IR_ALLOCA:
        case IR_GEP:
            break;

        case IR_LOAD: {
            if (instr->r1 && instr->r1->pinned_reg) {
                X86Reg src = jitRegFromName(instr->r1->pinned_reg->data);
                x86_64_enc_mov_reg_reg(enc, R_RAX, src);
                jitSpillDst(ctx, instr, R_RAX);
                break;
            }
            if (instr->dst && irIsFloat(instr->dst->type)) {
                int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
                jitFpFrameLoad(enc, 0 /* xmm0 */, loff, jitFpDbl(instr->dst));
                jitSpillDstFpr(ctx, instr, 0);
                break;
            }
            int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
            int size = instr->dst ? (int)instr->dst->as.var.size : 8;
            jitFrameLoad(enc, R_RAX, size, loff);
            jitSpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_STORE: {
            X86Reg src_reg = R_RAX;
            int have_src = 0;
            if (instr->r1 && instr->r1->loc.kind == IR_LOC_REG &&
                instr->r1->loc.as.reg)
            {
                src_reg = jitRegFromName(instr->r1->loc.as.reg->data);
                have_src = 1;
            }
            if (instr->r1 && irIsFloat(instr->r1->type))
            {
                int src_xmm = 0;
                if (have_src && jitIsFpRegName(instr->r1->loc.as.reg->data)) {
                    src_xmm = (int)src_reg;
                } else {
                    jitLoadFirstSrcFpr(ctx, instr->r1);
                }
                jitStoreFprSized(ctx, instr->dst, src_xmm,
                                 (int)irValueByteSize(instr->r1));
                break;
            }
            if (!have_src) { jitLoadFirstSrc(ctx, instr->r1); src_reg = R_RAX; }
            /* Pinned destination lives in a register, not a slot - so
             * check it before asking for a (nonexistent) loff. */
            if (instr->dst && instr->dst->pinned_reg) {
                X86Reg home = jitRegFromName(instr->dst->pinned_reg->data);
                if (home != src_reg) x86_64_enc_mov_reg_reg(enc, home, src_reg);
                break;
            }
            int loff = irCgGetLoff(&ctx->fn->ra, instr->dst);
            int size = (int)irValueByteSize(instr->r1);
            jitFrameStore(enc, src_reg, size, loff);
            break;
        }

        case IR_LOAD_DEREF: {
            /* Global base: materialise the address into rcx, then load. */
            if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
                jitGlobalAddr(jit, R_RCX, instr->r1->as.global.name->data);
                if (instr->dst && irIsFloat(instr->dst->type)) {
                    if (jitFpDbl(instr->dst))
                        x86_64_enc_movsd_load(enc, 0, R_RCX, -1, 0, 0);
                    else
                        x86_64_enc_movss_load(enc, 0, R_RCX, -1, 0, 0);
                    jitSpillDstFpr(ctx, instr, 0);
                } else {
                    int size = instr->dst ? (int)instr->dst->as.var.size : 8;
                    x86_64_enc_load_mem(enc, size, R_RAX, R_RCX, -1, 0, 0);
                    jitSpillDst(ctx, instr, R_RAX);
                }
                break;
            }
            X86Reg base = R_RCX;
            if (instr->r1 && instr->r1->loc.kind == IR_LOC_REG &&
                instr->r1->loc.as.reg)
            {
                base = jitRegFromName(instr->r1->loc.as.reg->data);
            } else {
                jitLoadToReg(ctx, instr->r1, R_RCX);
            }
            X86Reg idx; int has_idx = jitIdxReg(ctx, instr, &idx);
            if (instr->dst && irIsFloat(instr->dst->type)) {
                if (jitFpDbl(instr->dst))
                    x86_64_enc_movsd_load(enc, 0, base,
                                          has_idx ? (int)idx : -1,
                                          instr->scale, instr->disp);
                else
                    x86_64_enc_movss_load(enc, 0, base,
                                          has_idx ? (int)idx : -1,
                                          instr->scale, instr->disp);
                jitSpillDstFpr(ctx, instr, 0);
                break;
            }
            int size = instr->dst ? (int)instr->dst->as.var.size : 8;
            x86_64_enc_load_mem(enc, size, R_RAX, base,
                                has_idx ? (int)idx : -1,
                                instr->scale, instr->disp);
            jitSpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_STORE_DEREF: {
            if (instr->dst && instr->dst->kind == IR_VAL_GLOBAL) {
                jitGlobalAddr(jit, R_RCX, instr->dst->as.global.name->data);
                if (instr->r1 && irIsFloat(instr->r1->type)) {
                    jitLoadFirstSrcFpr(ctx, instr->r1);
                    if (jitFpDbl(instr->r1))
                        x86_64_enc_movsd_store(enc, 0, R_RCX, -1, 0, 0);
                    else
                        x86_64_enc_movss_store(enc, 0, R_RCX, -1, 0, 0);
                } else {
                    int sz = (int)irValueByteSize(instr->r1);
                    jitLoadFirstSrc(ctx, instr->r1);
                    x86_64_enc_store_mem(enc, sz, R_RAX, R_RCX, -1, 0, 0);
                }
                break;
            }
            X86Reg base = R_RCX;
            int addr_in_reg = instr->dst && instr->dst->loc.kind == IR_LOC_REG &&
                              instr->dst->loc.as.reg;
            if (addr_in_reg) base = jitRegFromName(instr->dst->loc.as.reg->data);
            X86Reg idx; int has_idx = jitIdxReg(ctx, instr, &idx);
            if (!addr_in_reg) jitLoadToReg(ctx, instr->dst, R_RCX);
            if (instr->r1 && irIsFloat(instr->r1->type)) {
                /* Materialising a float value can clobber RAX (a const is
                 * built with movabs+movq through RAX - the JIT has no
                 * rodata to RIP-load from like the AOT backend does). If
                 * the destination address sits in RAX (e.g. a local
                 * array element whose `lea` result was pinned there),
                 * that store target would be destroyed. Relocate base /
                 * idx out of RAX first. */
                if (base == R_RAX) {
                    X86Reg t = (has_idx && idx == R_RCX) ? R_RDX : R_RCX;
                    x86_64_enc_mov_reg_reg(enc, t, R_RAX);
                    base = t;
                }
                if (has_idx && idx == R_RAX) {
                    X86Reg t = (base == R_RCX) ? R_RDX : R_RCX;
                    x86_64_enc_mov_reg_reg(enc, t, R_RAX);
                    idx = t;
                }
                jitLoadFirstSrcFpr(ctx, instr->r1);
                if (jitFpDbl(instr->r1))
                    x86_64_enc_movsd_store(enc, 0, base,
                                           has_idx ? (int)idx : -1,
                                           instr->scale, instr->disp);
                else
                    x86_64_enc_movss_store(enc, 0, base,
                                           has_idx ? (int)idx : -1,
                                           instr->scale, instr->disp);
                break;
            }
            int sz = (int)irValueByteSize(instr->r1);
            /* Pick a val reg that conflicts with neither base nor idx. */
            X86Reg val = addr_in_reg ? R_RCX : R_RAX;
            if (has_idx && val == idx) val = R_RDX == base ? R_R10 : R_RDX;
            if (val == base) val = (has_idx && idx == R_RDX) ? R_R10 : R_RDX;
            jitLoadToReg(ctx, instr->r1, val);
            x86_64_enc_store_mem(enc, sz, val, base,
                                 has_idx ? (int)idx : -1,
                                 instr->scale, instr->disp);
            break;
        }

        case IR_RMW_DEREF: {
            X86Reg base = R_RCX;
            int addr_in_reg = instr->dst && instr->dst->loc.kind == IR_LOC_REG &&
                              instr->dst->loc.as.reg;
            if (addr_in_reg) base = jitRegFromName(instr->dst->loc.as.reg->data);
            else if (instr->dst && instr->dst->kind == IR_VAL_GLOBAL)
                jitGlobalAddr(jit, R_RCX, instr->dst->as.global.name->data);
            else jitLoadToReg(ctx, instr->dst, R_RCX);

            X86Reg idx; int has_idx = jitIdxReg(ctx, instr, &idx);
            int sz = (int)irValueByteSize(instr->r1);
            /* Load-modify-store through an accumulator that aliases
             * neither base nor idx; rhs goes through r10. */
            X86Reg acc = R_RAX;
            if (base == R_RAX || (has_idx && idx == R_RAX)) acc = R_R11;
            x86_64_enc_load_mem(enc, sz, acc, base,
                                has_idx ? (int)idx : -1,
                                instr->scale, instr->disp);
            jitLoadToReg(ctx, instr->r1, R_R10);
            IrOp rop = instr->extra.rmw_op;
            int alu_op;
            switch (rop) {
                case IR_IADD: alu_op = '+'; break;
                case IR_ISUB: alu_op = '-'; break;
                case IR_AND:  alu_op = '&'; break;
                case IR_OR:   alu_op = '|'; break;
                case IR_XOR:  alu_op = '^'; break;
                default: loggerPanic("jit-x86_64: bad RMW op %d\n", rop);
            }
            x86_64_enc_alu_reg_reg(enc, alu_op, acc, R_R10);
            x86_64_enc_store_mem(enc, sz, acc, base,
                                 has_idx ? (int)idx : -1,
                                 instr->scale, instr->disp);
            break;
        }

        case IR_LEA: {
            if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
                const char *name = instr->r1->as.global.name->data;
                if (instr->r1->flags & IR_VAL_FLAG_FUNC) {
                    name = asmNormaliseFunctionName(jit->cc,
                            instr->r1->as.global.name);
                }
                jitGlobalAddr(jit, R_RAX, name);
            } else if (instr->r1) {
                int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
                x86_64_enc_lea_mem(enc, R_RAX, R_RBP, -1, 0, loff);
            }
            jitSpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_IADD:
        case IR_ISUB:
        case IR_AND:
        case IR_OR:
        case IR_XOR: {
            int alu_op;
            switch (instr->op) {
                case IR_IADD: alu_op = '+'; break;
                case IR_ISUB: alu_op = '-'; break;
                case IR_AND:  alu_op = '&'; break;
                case IR_OR:   alu_op = '|'; break;
                default:      alu_op = '^'; break;
            }
            jitLoadFirstSrc(ctx, instr->r1);
            s64 imm;
            if (jitIsImm32(instr->r2, &imm)) {
                x86_64_enc_alu_imm_reg(enc, alu_op, R_RAX, (int32_t)imm);
            } else if (instr->r2->loc.kind == IR_LOC_REG &&
                       instr->r2->loc.as.reg)
            {
                X86Reg rhs = jitRegFromName(instr->r2->loc.as.reg->data);
                x86_64_enc_alu_reg_reg(enc, alu_op, R_RAX, rhs);
            } else {
                jitLoadToReg(ctx, instr->r2, R_RCX);
                x86_64_enc_alu_reg_reg(enc, alu_op, R_RAX, R_RCX);
            }
            jitSpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_IMUL:
            jitLoadFirstSrc(ctx, instr->r1);
            if (instr->r2->loc.kind == IR_LOC_REG && instr->r2->loc.as.reg) {
                X86Reg rhs = jitRegFromName(instr->r2->loc.as.reg->data);
                x86_64_enc_imul_reg_reg(enc, R_RAX, rhs);
            } else {
                jitLoadToReg(ctx, instr->r2, R_RCX);
                x86_64_enc_imul_reg_reg(enc, R_RAX, R_RCX);
            }
            jitSpillDst(ctx, instr, R_RAX);
            break;

        case IR_IDIV:
        case IR_UDIV:
        case IR_IREM:
        case IR_UREM: {
            /* idiv/div divide rdx:rax by the source operand. Quotient
             * lands in rax, remainder in rdx. */
            int is_signed = (instr->op == IR_IDIV || instr->op == IR_IREM);
            int is_rem    = (instr->op == IR_IREM || instr->op == IR_UREM);
            jitLoadFirstSrc(ctx, instr->r1);
            jitLoadToReg(ctx, instr->r2, R_RCX);
            if (is_signed) {
                x86_64_enc_cqto(enc);
                x86_64_enc_idivq_reg(enc, R_RCX);
            } else {
                x86_64_enc_xor32_reg(enc, R_RDX);
                x86_64_enc_divq_reg(enc, R_RCX);
            }
            if (is_rem) x86_64_enc_mov_reg_reg(enc, R_RAX, R_RDX);
            jitSpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_INEG:
            jitLoadFirstSrc(ctx, instr->r1);
            x86_64_enc_neg_reg(enc, R_RAX);
            jitSpillDst(ctx, instr, R_RAX);
            break;

        case IR_NOT:
            jitLoadFirstSrc(ctx, instr->r1);
            x86_64_enc_not_reg(enc, R_RAX);
            jitSpillDst(ctx, instr, R_RAX);
            break;

        case IR_SHL:
        case IR_SHR:
        case IR_SAR: {
            /* /digit of the C1/D3 shift group. */
            int kind = (instr->op == IR_SHL) ? 4
                     : (instr->op == IR_SHR) ? 5 : 7;
            jitLoadFirstSrc(ctx, instr->r1);
            if (instr->r2 && instr->r2->kind == IR_VAL_CONST_INT &&
                instr->r2->as._i64 >= 0 && instr->r2->as._i64 < 64)
            {
                x86_64_enc_shift_imm_reg(enc, kind, R_RAX,
                                         (uint8_t)instr->r2->as._i64);
            } else {
                /* Variable shift count must live in %cl. */
                jitLoadToReg(ctx, instr->r2, R_RCX);
                x86_64_enc_shift_cl_reg(enc, kind, R_RAX);
            }
            jitSpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_FADD:
        case IR_FSUB:
        case IR_FMUL:
        case IR_FDIV: {
            uint8_t opc = (instr->op == IR_FADD) ? X86_SSE_ADDSD
                        : (instr->op == IR_FSUB) ? X86_SSE_SUBSD
                        : (instr->op == IR_FMUL) ? X86_SSE_MULSD
                                                 : X86_SSE_DIVSD;
            int is_dbl = jitFpDbl(instr->dst);
            jitLoadFirstSrcFpr(ctx, instr->r1);          /* xmm0 */
            jitLoadToFpr(ctx, instr->r2, 1 /* xmm1 */);
            if (is_dbl) x86_64_enc_sse_arith(enc, opc, 0, 1);
            else        x86_64_enc_sse_arith_ss(enc, opc, 0, 1);
            jitSpillDstFpr(ctx, instr, 0);
            break;
        }

        case IR_FNEG:
            /* No fneg on SSE - flip the sign bit by XORing in a mask
             * materialised through rax/xmm1. F64 flips bit 63, F32
             * flips bit 31, with the matching xorpd/xorps width. */
            jitLoadFirstSrcFpr(ctx, instr->r1);
            if (jitFpDbl(instr->dst)) {
                x86_64_enc_movabsq_imm_reg(enc, R_RAX, 0x8000000000000000ULL);
                x86_64_enc_movq_gpr_xmm(enc, 1, R_RAX);
                x86_64_enc_xorpd(enc, 0, 1);
            } else {
                jitEmitMovImm(enc, R_RAX, (s64)(u64)0x80000000ULL);
                x86_64_enc_movd_gpr_xmm(enc, 1, R_RAX);
                x86_64_enc_xorps(enc, 0, 1);
            }
            jitSpillDstFpr(ctx, instr, 0);
            break;

        case IR_ICMP: {
            jitLoadFirstSrc(ctx, instr->r1);
            s64 imm;
            if (jitIsImm32(instr->r2, &imm)) {
                x86_64_enc_cmpq_imm_reg(enc, R_RAX, (int32_t)imm);
            } else {
                jitLoadToReg(ctx, instr->r2, R_RCX);
                x86_64_enc_cmp_reg_reg(enc, R_RAX, R_RCX);
            }
            jitEmitSetCC(enc, instr->extra.cmp_kind, 0);
            jitSpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_FCMP: {
            int is_dbl = jitFpDbl(instr->r1);
            jitLoadFirstSrcFpr(ctx, instr->r1);
            jitLoadToFpr(ctx, instr->r2, 1 /* xmm1 */);
            if (is_dbl) x86_64_enc_ucomisd(enc, 0, 1);
            else        x86_64_enc_ucomiss(enc, 0, 1);
            jitEmitSetCC(enc, instr->extra.cmp_kind, 1);
            jitSpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_CMP_BR: {
            IrCmpKind kind = instr->extra.cmp_br.cmp_kind;
            IrBlock *t = instr->extra.cmp_br.target_block;
            IrBlock *f = instr->extra.cmp_br.fallthrough_block;
            int is_float = instr->r1 && irIsFloat(instr->r1->type);
            if (is_float) {
                jitLoadFirstSrcFpr(ctx, instr->r1);
                jitLoadToFpr(ctx, instr->r2, 1 /* xmm1 */);
                if (jitFpDbl(instr->r1)) x86_64_enc_ucomisd(enc, 0, 1);
                else                     x86_64_enc_ucomiss(enc, 0, 1);
            } else {
                jitLoadFirstSrc(ctx, instr->r1);
                s64 imm;
                if (jitIsImm32(instr->r2, &imm)) {
                    x86_64_enc_cmpq_imm_reg(enc, R_RAX, (int32_t)imm);
                } else {
                    jitLoadToReg(ctx, instr->r2, R_RCX);
                    x86_64_enc_cmp_reg_reg(enc, R_RAX, R_RCX);
                }
            }
            int cc_t = jitCcFor(kind, is_float);
            int cc_f = jitCcInvFor(kind, is_float);
            int t_ln = hccJitBlockLocalNum(jit, ctx->fn, t);
            int f_ln = hccJitBlockLocalNum(jit, ctx->fn, f);
            int t_phi = irBlockHasPhi(t);
            int f_phi = irBlockHasPhi(f);
            if (!t_phi && !f_phi) {
                if (ctx->next_block == t) {
                    jitEmitJccLocal(ctx, cc_f, f_ln);
                } else if (ctx->next_block == f) {
                    jitEmitJccLocal(ctx, cc_t, t_ln);
                } else {
                    jitEmitJccLocal(ctx, cc_t, t_ln);
                    jitEmitBranchLocal(ctx, f_ln);
                }
            } else {
                int else_ln = hccJitFreshLocalNum(jit);
                jitEmitJccLocal(ctx, cc_f, else_ln);
                jitPhiMaterialise(ctx, ctx->cur_block, t);
                jitEmitBranchLocal(ctx, t_ln);
                asm_define_label(enc, else_ln, NULL);
                jitPhiMaterialise(ctx, ctx->cur_block, f);
                if (ctx->next_block != f) jitEmitBranchLocal(ctx, f_ln);
            }
            break;
        }

        case IR_BR: {
            IrBlock *t = instr->extra.blocks.target_block;
            IrBlock *f = instr->extra.blocks.fallthrough_block;
            jitLoadFirstSrc(ctx, instr->dst);
            x86_64_enc_test_reg_reg(enc, R_RAX, R_RAX);
            int t_ln = hccJitBlockLocalNum(jit, ctx->fn, t);
            int f_ln = hccJitBlockLocalNum(jit, ctx->fn, f);
            int t_phi = irBlockHasPhi(t);
            int f_phi = irBlockHasPhi(f);
            if (!t_phi && !f_phi) {
                if (ctx->next_block == t) {
                    jitEmitJccLocal(ctx, X86_CC_E, f_ln);
                } else if (ctx->next_block == f) {
                    jitEmitJccLocal(ctx, X86_CC_NE, t_ln);
                } else {
                    jitEmitJccLocal(ctx, X86_CC_NE, t_ln);
                    jitEmitBranchLocal(ctx, f_ln);
                }
            } else {
                int else_ln = hccJitFreshLocalNum(jit);
                jitEmitJccLocal(ctx, X86_CC_E, else_ln);
                jitPhiMaterialise(ctx, ctx->cur_block, t);
                jitEmitBranchLocal(ctx, t_ln);
                asm_define_label(enc, else_ln, NULL);
                jitPhiMaterialise(ctx, ctx->cur_block, f);
                if (ctx->next_block != f) jitEmitBranchLocal(ctx, f_ln);
            }
            break;
        }

        case IR_JMP: {
            IrBlock *target = instr->extra.blocks.target_block;
            jitPhiMaterialise(ctx, ctx->cur_block, target);
            if (target != ctx->next_block) {
                int ln = hccJitBlockLocalNum(jit, ctx->fn, target);
                jitEmitBranchLocal(ctx, ln);
            }
            break;
        }

        case IR_RET:
            if (instr->dst && instr->dst->byval_struct_type) {
                /* <=16-byte struct returned in registers: load the return
                 * slot's bytes into rax,rdx (INTEGER) / xmm0,xmm1 (SSE). */
                AstType *t = instr->dst->byval_struct_type;
                int loff = irCgGetLoff(&ctx->fn->ra, instr->dst);
                SysvClass cl[2]; int neb = 0;
                astSysvClassify(t, cl, &neb);
                int gpret[2] = { R_RAX, R_RDX };
                int gpi = 0, ssei = 0;
                for (int e = 0; e < neb; ++e) {
                    int off = e * 8, rem = (int)t->size - off;
                    int sz = rem >= 8 ? 8 : rem;
                    if (cl[e] == SYSV_SSE) {
                        if (sz == 4) x86_64_enc_movss_load(enc, ssei, R_RBP, -1, 0, loff + off);
                        else         x86_64_enc_movsd_load(enc, ssei, R_RBP, -1, 0, loff + off);
                        ssei++;
                    } else {
                        x86_64_enc_load_mem(enc, sz, gpret[gpi], R_RBP, -1, 0, loff + off);
                        gpi++;
                    }
                }
            } else if (instr->dst) {
                if (irIsFloat(instr->dst->type)) jitLoadFirstSrcFpr(ctx, instr->dst);
                else                              jitLoadFirstSrc(ctx, instr->dst);
            }
            jitEmitBranchLocal(ctx, hccJitEpilogueLocalNum(jit, ctx->fn));
            break;

        case IR_TRUNC: {
            jitLoadFirstSrc(ctx, instr->r1);
            int sz = instr->dst ? (int)instr->dst->as.var.size : 8;
            if (sz < 8) x86_64_enc_movzx_reg_reg(enc, sz, R_RAX, R_RAX);
            jitSpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_ZEXT: {
            jitLoadFirstSrc(ctx, instr->r1);
            int src_sz = (int)irValueByteSize(instr->r1);
            if (src_sz < 8) x86_64_enc_movzx_reg_reg(enc, src_sz, R_RAX, R_RAX);
            jitSpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_SEXT: {
            jitLoadFirstSrc(ctx, instr->r1);
            int src_sz = (int)irValueByteSize(instr->r1);
            if (src_sz < 8) x86_64_enc_movsx_reg_reg(enc, src_sz, R_RAX, R_RAX);
            jitSpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_FPTRUNC:
            jitLoadFirstSrcFpr(ctx, instr->r1);
            x86_64_enc_cvtsd2ss(enc, 0, 0);
            jitSpillDstFpr(ctx, instr, 0);
            break;

        case IR_FPEXT:
            jitLoadFirstSrcFpr(ctx, instr->r1);
            x86_64_enc_cvtss2sd(enc, 0, 0);
            jitSpillDstFpr(ctx, instr, 0);
            break;

        case IR_FPTOSI:
        case IR_FPTOUI:
            /* x86 has no unsigned truncation pre-AVX512; treat as
             * signed (same simplification as the AOT backend). */
            jitLoadFirstSrcFpr(ctx, instr->r1);
            if (jitFpDbl(instr->r1)) x86_64_enc_cvttsd2si(enc, R_RAX, 0);
            else                     x86_64_enc_cvttss2si(enc, R_RAX, 0);
            jitSpillDst(ctx, instr, R_RAX);
            break;

        case IR_SITOFP:
        case IR_UITOFP:
            /* Signed conversion either way (matches the AOT backend). */
            jitLoadFirstSrc(ctx, instr->r1);
            if (jitFpDbl(instr->dst)) x86_64_enc_cvtsi2sd(enc, 0, R_RAX);
            else                      x86_64_enc_cvtsi2ss(enc, 0, R_RAX);
            jitSpillDstFpr(ctx, instr, 0);
            break;

        case IR_PTRTOINT:
        case IR_INTTOPTR:
        case IR_BITCAST: {
            int dst_is_float = irIsFloat(instr->dst->type);
            int src_is_float = irIsFloat(instr->r1->type);
            if (dst_is_float == src_is_float) {
                if (dst_is_float) {
                    jitLoadFirstSrcFpr(ctx, instr->r1);
                    jitSpillDstFpr(ctx, instr, 0);
                } else {
                    jitLoadFirstSrc(ctx, instr->r1);
                    jitSpillDst(ctx, instr, R_RAX);
                }
            } else if (dst_is_float) {
                jitLoadFirstSrc(ctx, instr->r1);
                if (jitFpDbl(instr->dst)) x86_64_enc_movq_gpr_xmm(enc, 0, R_RAX);
                else                      x86_64_enc_movd_gpr_xmm(enc, 0, R_RAX);
                jitSpillDstFpr(ctx, instr, 0);
            } else {
                jitLoadFirstSrcFpr(ctx, instr->r1);
                if (jitFpDbl(instr->r1)) x86_64_enc_movq_xmm_gpr(enc, R_RAX, 0);
                else                     x86_64_enc_movd_xmm_gpr(enc, R_RAX, 0);
                jitSpillDst(ctx, instr, R_RAX);
            }
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
                if (!instr->r2) loggerPanic("jit-x86_64: indirect call without target\n");
                /* r11 sits outside the arg-reg range so the arg loads
                 * below can't clobber the target. */
                jitLoadToReg(ctx, instr->r2, R_R11);
            }
            /* Route through the SysV path for by-value struct args OR a
             * by-value struct return (register-return / hidden-out-ptr). */
            {
                int use_sysv = (instr->flags & IRCG_CALL_AGG_RETURN) != 0;
                for (u64 ai = 0; args && ai < args->size; ++ai) {
                    if (vecGet(IrValue *, args, ai)->byval_struct_type) {
                        use_sysv = 1;
                        break;
                    }
                }
                if (use_sysv) {
                    jitEmitSysvCall(ctx, instr, args, fname, indirect);
                    break;
                }
            }
            Ast *callee = NULL;
            if (!indirect) {
                callee = (Ast *)mapGetLen(jit->cc->global_env,
                                          fname->data, fname->len);
            }
            int callee_va = 0;
            if (callee) {
                if ((callee->type && callee->type->has_var_args) || callee->has_var_args)
                    callee_va = 1;
                else if (callee->params && callee->params->size > 0) {
                    Ast *last = vecGet(Ast *, callee->params, callee->params->size - 1);
                    if (last && last->kind == AST_VAR_ARGS) callee_va = 1;
                }
            }
            int holyc_variadic = callee_va && callee && callee->kind != AST_EXTERN_FUNC;
            int extern_variadic = callee_va && callee && callee->kind == AST_EXTERN_FUNC;

            int var_arg_start = -1;
            if (holyc_variadic && callee->params) {
                for (u64 i = 0; i < callee->params->size; ++i) {
                    Ast *p = vecGet(Ast *, callee->params, i);
                    var_arg_start++;
                    if (p && p->kind == AST_VAR_ARGS) break;
                }
                var_arg_start += 1;
            }

            u64 n = args ? args->size : 0;
            u8 *is_stack = n ? calloc(n, 1) : NULL;
            s32 n_stack = jitPartitionCallArgs(is_stack, n, args,
                                               holyc_variadic, var_arg_start);
            /* Stack args sit in 8-byte lanes; SysV requires 16-byte
             * alignment at the call instruction. */
            s32 stack_bytes = n_stack * 8;
            if (stack_bytes & 15) stack_bytes += 8;
            if (stack_bytes > 0) {
                x86_64_enc_subq_imm_reg(enc, R_RSP, stack_bytes);
            }

            /* If arg[0]'s producer left it in a register, route it into
             * rdi before the stack-arg loop - that loop uses rax as
             * scratch and would clobber an rax-resident value. */
            int arg0_in_reg = 0;
            if (n > 0 && !is_stack[0]) {
                IrValue *a0 = vecGet(IrValue *, args, 0);
                if (a0 && !irIsFloat(a0->type) &&
                    a0->loc.kind == IR_LOC_REG && a0->loc.as.reg)
                {
                    X86Reg src = jitRegFromName(a0->loc.as.reg->data);
                    if (src != kIntArgRegs[0])
                        x86_64_enc_mov_reg_reg(enc, kIntArgRegs[0], src);
                    arg0_in_reg = 1;
                }
            }

            /* Place stack args at [rsp + i*8] in source order. */
            s32 stack_idx = 0;
            for (u64 i = 0; i < n; ++i) {
                if (!is_stack[i]) continue;
                IrValue *a = vecGet(IrValue *, args, i);
                int32_t off = stack_idx * 8;
                if (irIsFloat(a->type)) {
                    jitLoadToFpr(ctx, a, 0 /* xmm0 */);
                    if (jitFpDbl(a)) x86_64_enc_movsd_store(enc, 0, R_RSP, -1, 0, off);
                    else             x86_64_enc_movss_store(enc, 0, R_RSP, -1, 0, off);
                } else {
                    jitLoadToReg(ctx, a, R_RAX);
                    x86_64_enc_store_mem(enc, 8, R_RAX, R_RSP, -1, 0, off);
                }
                stack_idx++;
            }

            /* Reg args. Hidden struct-return pointer (when present) is
             * args[0] and naturally lands in rdi. */
            int int_idx = arg0_in_reg ? 1 : 0;
            int float_idx = 0;
            int xmm_used = 0;
            for (u64 i = 0; i < n; ++i) {
                if (is_stack[i]) continue;
                if (i == 0 && arg0_in_reg) continue;
                IrValue *a = vecGet(IrValue *, args, i);
                if (irIsFloat(a->type)) {
                    jitLoadToFpr(ctx, a, float_idx++);
                    xmm_used++;
                } else {
                    jitLoadToReg(ctx, a, kIntArgRegs[int_idx++]);
                }
            }

            /* SysV variadic ABI: %al = number of XMM regs used. */
            if (extern_variadic || holyc_variadic) {
                x86_64_enc_mov_al_imm8(enc, (uint8_t)xmm_used);
            }

            if (indirect) {
                x86_64_enc_call_reg(enc, R_R11);
            } else {
                char *normalised = asmNormaliseFunctionName(jit->cc, fname);
                size_t off = x86_64_enc_call_rel32(enc);
                hccJitAddCallFixup(enc, off, normalised, AFR_X86_64_CALL32);
            }
            if (stack_bytes > 0) {
                x86_64_enc_addq_imm_reg(enc, R_RSP, stack_bytes);
            }
            if (instr->dst && instr->dst->type != IR_TYPE_VOID) {
                if (irIsFloat(instr->dst->type)) jitSpillDstFpr(ctx, instr, 0);
                else                              jitSpillDst   (ctx, instr, R_RAX);
            }
            free(is_stack);
            break;
        }

        case IR_ASM:
            /* Inline `asm {...}` statement: libtasm-encode and splice
             * the bytes at the current position. */
            if (instr->extra.asm_fragments) {
                loggerPanic("jit-x86_64: `&var` asm fragments are not "
                            "supported; address locals directly\n");
            }
            if (instr->r1 && instr->r1->as.str.str &&
                hccJitAssembleText(jit, instr->r1->as.str.str, 0) != 0)
            {
                loggerPanic("jit-x86_64: failed to assemble inline asm\n");
            }
            break;

        case IR_SELECT:
        case IR_SWITCH:
        case IR_VA_ARG:
        case IR_VA_START:
        case IR_VA_END:
            loggerPanic("jit-x86_64: op %d not yet implemented\n", instr->op);
        default:
            loggerPanic("jit-x86_64: unknown op %d\n", instr->op);
    }
}

/* ---------------- prologue / epilogue ---------------- */

static void jitEmitPrologue(JitFnCtx *ctx) {
    AsmEnc *enc = &ctx->jit->enc;
    if (ctx->omit_frame) return;
    x86_64_enc_pushq_rbp(enc);
    x86_64_enc_mov_rsp_rbp(enc);  /* movq %rsp, %rbp */
    uint32_t aligned = ((uint32_t)ctx->fn->stack_space + 15u) & ~15u;
    if (aligned > 0) {
        x86_64_enc_subq_imm_reg(enc, R_RSP, (int32_t)aligned);
    }
}

static void jitEmitEpilogue(JitFnCtx *ctx) {
    AsmEnc *enc = &ctx->jit->enc;
    if (!ctx->omit_frame) {
        /* leave = movq %rbp, %rsp ; popq %rbp. */
        x86_64_enc_mov_rbp_rsp(enc);
        x86_64_enc_popq_rbp(enc);
    }
    x86_64_enc_retq(enc);
}

/* ---------------- per-function compilation ---------------- */

static int jitCompileFunction(HccJit *jit, Ast *ast, IrCtx *ir_ctx) {
    IrFunction *fn = irLowerFunction(ir_ctx, ast);
    irBasicFunctionOptimisations(fn);

    /* Layout passes (slot offsets, IR-CG ctx). The text path goes
     * through irFunctionPrepForCodeGen; same here so the regalloc loff
     * map is populated before we ask for offsets. */
    IrCgCtx dummy = {0};
    dummy.cc = jit->cc;
    dummy.fn = fn;
    irFunctionPrepForCodeGen(&dummy, fn, ast);

    int is_variadic = (ast->type && ast->type->has_var_args) || ast->has_var_args;
    /* A by-value struct param is unpacked into an rbp-relative slot by
     * jitEmitSysvParamPrologue, so the frame must exist. */
    int has_struct_param = 0;
    if (ast->params) {
        for (u64 pi = 0; pi < ast->params->size; ++pi) {
            Ast *p = vecGet(Ast *, ast->params, pi);
            if (jitIsByvalStruct(p->type)) { has_struct_param = 1; break; }
        }
    }
    int omit_frame = (fn->stack_space == 0) && !irFnHasCalls(fn) &&
                     (ast->loff == 0) && !is_variadic && !has_struct_param;

    JitFnCtx ctx = {0};
    ctx.jit = jit;
    ctx.fn = fn;
    ctx.ast = ast;
    ctx.omit_frame = omit_frame;

    /* Publish the function entry as a public label. asmNormaliseFunctionName
     * strips/adds underscores per platform; the resolver uses the same name. */
    char *fname = asmNormaliseFunctionName(jit->cc, ast->fname);
    asm_define_label(&jit->enc, -1, fname);

    jitEmitPrologue(&ctx);
    jitEmitSysvParamPrologue(&ctx);

    Set *referenced = irCgComputeReferencedBlocks(fn);

    listForEach(fn->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        ctx.cur_block = block;
        ctx.next_block = (it->next != fn->blocks)
                       ? (IrBlock *)it->next->value : NULL;
        if (setHas(referenced, (void *)(u64)block->id)) {
            int ln = hccJitBlockLocalNum(jit, fn, block);
            asm_define_label(&jit->enc, ln, NULL);
        }
        listForEach(block->instructions) {
            jitEmitInstr(&ctx, (IrInstr *)it->value);
        }
    }
    setRelease(referenced);

    /* Epilogue label + bytes. */
    asm_define_label(&jit->enc, hccJitEpilogueLocalNum(jit, fn), NULL);
    jitEmitEpilogue(&ctx);

    return 0;
}

/* ---------------- reg pool setup ---------------- */

/* Self-contained pool init: x86_64.c's x86_64InitRegPool is static,
 * so we replicate the (small) setup here. The text path may not have
 * run yet, and even if it has the pool is idempotent (irRegPoolSet
 * just overwrites the slot). */
static void jitInitRegPool(void) {
    static int initialised = 0;
    static IrRegPool pool;
    if (initialised) return;

    static const char *const kInt[] = { "rdi","rsi","rdx","rcx","r8","r9" };
    static const char *const kFloat[] = {
        "xmm0","xmm1","xmm2","xmm3","xmm4","xmm5","xmm6","xmm7"
    };
    static const char *const kScratch[] = { "rax","rcx","rdx","xmm0","xmm1" };

    Vec *iv = vecNew(&vec_aostr_type);
    Vec *fv = vecNew(&vec_aostr_type);
    Vec *sv = vecNew(&vec_aostr_type);
    for (int i = 0; i < 6; i++) vecPush(iv, aoStrDupRaw((char *)kInt[i], strlen(kInt[i])));
    for (int i = 0; i < 8; i++) vecPush(fv, aoStrDupRaw((char *)kFloat[i], strlen(kFloat[i])));
    for (int i = 0; i < 5; i++) vecPush(sv, aoStrDupRaw((char *)kScratch[i], strlen(kScratch[i])));

    pool.int_arg_regs     = iv;
    pool.float_arg_regs   = fv;
    pool.int_return_reg   = aoStrDupRaw((char *)"rax", 3);
    pool.float_return_reg = aoStrDupRaw((char *)"xmm0", 4);
    pool.scratch_regs     = sv;
    pool.variadic_on_stack = 0;
    irRegPoolSet(&pool);
    initialised = 1;
}

/* ---------------- public API ---------------- */

static int jitTargetOk(enum CliTarget target) {
    return target == TARGET_X86_64_APPLE_DARWIN ||
           target == TARGET_X86_64_UNKNOWN_LINUX_GNU;
}

static const HccJitBackend x86_64_jit_backend = {
    .name = "x86_64",
    .target_ok = jitTargetOk,
    .init_reg_pool = jitInitRegPool,
    .compile_function = jitCompileFunction,
};

HccJit *x86_64JitCompile(Cctrl *cc) {
    return hccJitCompile(cc, &x86_64_jit_backend);
}

#endif /* __x86_64__ && HCC_ENABLE_JIT */
