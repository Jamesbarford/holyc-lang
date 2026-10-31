/* x86_64 backend: one instruction selector, three sinks.
 *
 * The selector lowers IR into machine IR (MirFn - an AsmBlock under
 * construction, see machine/mir.h) and never touches bytes or text
 * itself. What happens to the lines depends on who asked:
 *
 *   -jit / -repl   the lines are encoded straight into the chunk's
 *                  AsmEnc (x86_64_encode_instr), then mapped RX and
 *                  patched by asm_jit_finalize. Addresses of globals
 *                  and host functions are known now, so they are
 *                  materialised inline (HCC_ADDR_ABSOLUTE).
 *   object writer  the same, with HCC_ADDR_RELOCATED selected so every
 *                  global reference is a RIP-relative lea with a fixup.
 *   -S / AOT text  the lines are printed in AT&T GNU-as text
 *                  (asm_line_format_gas_x86) and handed to the system
 *                  assembler. Same selection, same peephole, so what
 *                  `-S` shows is what the JIT runs.
 *
 * Register roles: rax/rcx/rdx scratch, xmm0/xmm1 FP scratch, r10 struct
 * address / RMW operand, r11 indirect-call target, rbp frame. */

#include <dlfcn.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdio.h>

#define X86_LOCAL_PREFIX ".LBB"

#include "x86_64.h"
#include "aostr.h"
#include "ast.h"
#include "asm.h"
#include "machine/asm_enc.h"
#include "machine/asm_jit.h"
#include "machine/enc_x86_64.h"
#include "machine/mir.h"
#include "cli.h"
#include "containers.h"
#include "ir-inliner.h"
#include "ir.h"
#include "ir-optimise.h"
#include "ir-regalloc.h"
#include "jit-common.h"
#include "list.h"
#include "util.h"
#include <assert.h>
#include <math.h>
#include "config.h"
#include "ir-debug.h"

/* Per-function emission context. Same shape as IrCgCtx in spirit but
 * carries JIT-specific bits (the machine-IR stream, the flush cursor). */
typedef struct {
    HccJit *jit;
    IrFunction *fn;
    Ast *ast;
    IrBlock *cur_block;
    IrBlock *next_block;
    int omit_frame;
    int uses_va_start;
    /* The function's machine-IR stream: every instruction is built
     * here and committed by x86MirFlush. */
    MirFn *mir;
    /* AOT text sink. NULL = JIT (encode to bytes); non-NULL = format
     * each line as AT&T GNU-as text into this
     * buffer instead. Same selection either way - the sink is the
     * only difference. */
    AoStr *text_out;
    /* Flush cursor: lines [0, sunk) have been sunk (encoded/printed).
     * Sinking is deferred so a peephole can rewrite a segment before
     * it is committed; an inline `asm {}` block forces a flush since
     * its bytes bypass the stream. */
    int sunk;
    /* Source line for the IR instruction being lowered; stamped onto
     * each built line so the JIT's line notes bind at sink time. */
    int cur_line;
} X86Ctx;

/* SysV integer arg registers, ordered as arg 0..5. */
static const X86Reg kIntArgRegs[] = {
    R_RDI, R_RSI, R_RDX, R_RCX, R_R8, R_R9
};

/* ---------------- forward decls ---------------- */

static void x86EmitInstr(X86Ctx *ctx, IrInstr *instr);
static void x86LoadToReg(X86Ctx *ctx, IrValue *val, X86Reg reg);
static void x86LoadToFpr(X86Ctx *ctx, IrValue *val, int xmm);
static void x86StoreReg(X86Ctx *ctx, IrValue *dst, X86Reg reg);
static void x86StoreFpr(X86Ctx *ctx, IrValue *dst, int xmm);
static void x86PhiMaterialise(X86Ctx *ctx, IrBlock *from, IrBlock *to);

/* ---------------- value-kind helpers ---------------- */

static int x86IsImm32(IrValue *v, s64 *out) {
    if (!v || v->kind != IR_VAL_CONST_INT) return 0;
    s64 i = v->as._i64;
    if (i < INT32_MIN || i > INT32_MAX) return 0;
    *out = i;
    return 1;
}

/* Parse a register name like "rax", "r8", "xmm0" into its hardware
 * number. Used to translate the pinned-reg / IR_LOC_REG `AoStr *reg`
 * fields into encoder regs. */
static X86Reg x86RegFromName(const char *name) {
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

static int x86IsFpRegName(const char *name) {
    return name && !strncasecmp(name, "xmm", 3);
}

/* ---------------- MIR emission ----------------
 *
 * The backend only BUILDS lines; sinking is deferred to x86MirFlush so
 * a machine-layer peephole can see whole segments first. This hook
 * just stamps the source line for the JIT's line notes. */

static int x86MirDump(void) {
    static int mode = -1;
    if (mode < 0) {
        const char *s = getenv("HCC_JIT_MIR");
        mode = (s && !strcmp(s, "dump")) ? 1 : 0;
    }
    return mode;
}

static void x86MirLine(X86Ctx *ctx, AsmLine *ln) {
    ln->line = ctx->cur_line;
}

static void E0(X86Ctx *c, int mn) { x86MirLine(c, mir0(c->mir, mn)); }
static void E1(X86Ctx *c, int mn, AsmOperand a) {
    x86MirLine(c, mir1(c->mir, mn, a));
}
static void E2(X86Ctx *c, int mn, AsmOperand a, AsmOperand b) {
    x86MirLine(c, mir2(c->mir, mn, a, b));
}
static void E3(X86Ctx *c, int mn, AsmOperand a, AsmOperand b, AsmOperand d) {
    x86MirLine(c, mir3(c->mir, mn, a, b, d));
}
/* A local label is only a stream line until the flush defines it. */
static void x86MirLocalLabel(X86Ctx *ctx, int local_num) {
    mir_local_label(ctx->mir, local_num);
}

/* Text sink: format every line as AT&T GNU-as (labels unindented).
 *
 * AT&T rather than Intel because `.intel_syntax noprefix` reads a bare
 * name as a register or operator first, in any case, and ELF symbols
 * carry no prefix. The global `Fs` is the segment register, `Mod` an
 * operator, and nothing guarantees an error: `call Si` assembles as
 * `call *%rsi`. Quoting the name is no way out - binutils 2.38 still
 * reads `"Si"` as the register, again silently. Registers here carry a
 * `%`, so a symbol can only ever be a symbol. */
static void x86MirFlushText(X86Ctx *ctx) {
    AsmBlock *b = ctx->mir->block;
    char line[512];
    for (int i = ctx->sunk; i < b->n_lines; i++) {
        AsmLine *ln = &b->lines[i];
        if (ln->kind != AINS_INSTR && ln->kind != AINS_LABEL_LOCAL &&
            ln->kind != AINS_LABEL_PUBLIC)
        {
            continue;
        }
        asm_line_format_att_x86(ln, X86_LOCAL_PREFIX, line, sizeof(line));
        if (ln->kind != AINS_INSTR)
            aoStrRemovePreviousChar(ctx->text_out, '\t');
        aoStrCatFmt(ctx->text_out, "%s\n\t", line);
    }
}

/* Encode sink: define labels, bind line notes, encode per line. */
static void x86MirFlushEncode(X86Ctx *ctx) {
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
            x86_64_encode_instr(enc, ln);
            break;
        default:
            break;
        }
    }
}

/* Peephole the un-sunk segment, then commit it through the active
 * sink. */
static void x86MirFlush(X86Ctx *ctx) {
    mir_fn_peephole_x86_64(ctx->mir, ctx->sunk);
    if (ctx->text_out) x86MirFlushText(ctx);
    else               x86MirFlushEncode(ctx);
    ctx->sunk = ctx->mir->block->n_lines;
}

/* Register view at a store width. The encoders treat any width not in
 * {1,2,4} as the full 64 bits (x86_64_enc_store_mem's default arm), so
 * the spelling clamps the same way - a 3-byte struct-tail store is an
 * 8-byte mov into the rounded-up slot, exactly as before. */
static AsmOperand x86MirGp(int size, int num) {
    switch (size) {
    case 1: return mir_r8(num);
    case 2: return mir_r16(num);
    case 4: return mir_r32(num);
    default: return mir_r64(num);
    }
}

/* [base + idx*scale + disp]; idx < 0 for no index. */
static AsmOperand x86MemOp(X86Reg base, int idx, int scale, s64 disp) {
    if (idx >= 0)
        return mir_mem_sib(mir_r64(base), mir_r64((X86Reg)idx),
                           scale > 0 ? scale : 1, disp);
    return mir_mem(mir_r64(base), disp);
}

/* cc (X86_CC_*) to jcc mnemonic id - the Jcc enum block is
 * alphabetical, not cc-ordered, so this cannot be an offset. */
static int x86MirJccMn(int cc) {
    switch (cc) {
        case X86_CC_O:  return X86MN_JO;
        case X86_CC_NO: return X86MN_JNO;
        case X86_CC_B:  return X86MN_JB;
        case X86_CC_AE: return X86MN_JAE;
        case X86_CC_E:  return X86MN_JE;
        case X86_CC_NE: return X86MN_JNE;
        case X86_CC_BE: return X86MN_JBE;
        case X86_CC_A:  return X86MN_JA;
        case X86_CC_S:  return X86MN_JS;
        case X86_CC_NS: return X86MN_JNS;
        case X86_CC_P:  return X86MN_JP;
        case X86_CC_NP: return X86MN_JNP;
        case X86_CC_L:  return X86MN_JL;
        case X86_CC_GE: return X86MN_JGE;
        case X86_CC_LE: return X86MN_JLE;
        case X86_CC_G:  return X86MN_JG;
        default:        return X86MN_UNKNOWN;
    }
}

static int x86MirAluMn(IrOp op) {
    switch (op) {
    case IR_IADD: return X86MN_ADD;
    case IR_ISUB: return X86MN_SUB;
    case IR_AND:  return X86MN_AND;
    case IR_OR:   return X86MN_OR;
    case IR_XOR:  return X86MN_XOR;
    default:      return X86MN_UNKNOWN;
    }
}

static int x86MirShiftMn(IrOp op) {
    return op == IR_SHL ? X86MN_SHL : op == IR_SHR ? X86MN_SHR : X86MN_SAR;
}

static int x86MirFopMn(IrOp op, int is_dbl) {
    switch (op) {
    case IR_FADD: return is_dbl ? X86MN_ADDSD : X86MN_ADDSS;
    case IR_FSUB: return is_dbl ? X86MN_SUBSD : X86MN_SUBSS;
    case IR_FMUL: return is_dbl ? X86MN_MULSD : X86MN_MULSS;
    default:      return is_dbl ? X86MN_DIVSD : X86MN_DIVSS;
    }
}

/* ---------------- immediate materialise ---------------- */

/* Emit `reg = imm`. Zero gets the canonical xorl (which also zeroes the
 * high half); everything else a mov whose encoding the dispatch sizes
 * (sign-extended imm32, or the wide 10-byte form past int32). Never
 * emitted between a compare and its consuming jcc/setcc, so the flags
 * clobber is safe. */
static void x86EmitMovImm(X86Ctx *ctx, X86Reg reg, s64 imm) {
    if (imm == 0) {
        E2(ctx, X86MN_XOR, mir_r32(reg), mir_r32(reg));
        return;
    }
    E2(ctx, X86MN_MOV, mir_r64(reg), mir_imm(imm));
}

/* ---------------- memory load/store spellings ----------------
 *
 * x86 mem operands carry a full signed disp32, so unlike aarch64 there
 * are no range/alignment forms to pick between - every local is
 * directly addressable off rbp. The width rules are the direct
 * encoders' (x86_64_enc_load_mem): 1/2 zero-extend, 4 sign-extends
 * (movslq), everything else moves the full 8 bytes. */

static void x86LoadMem(X86Ctx *ctx, int size, X86Reg dst, AsmOperand mem) {
    MirFn *m = ctx->mir;
    switch (size) {
    case 1:
        x86MirLine(ctx, mir2(m, X86MN_MOVZX, mir_r64(dst),
                             mir_mem_hint(mem, 1)));
        break;
    case 2:
        x86MirLine(ctx, mir2(m, X86MN_MOVZX, mir_r64(dst),
                             mir_mem_hint(mem, 2)));
        break;
    case 4:
        x86MirLine(ctx, mir2(m, X86MN_MOVSX, mir_r64(dst),
                             mir_mem_hint(mem, 4)));
        break;
    default:
        x86MirLine(ctx, mir2(m, X86MN_MOV, mir_r64(dst), mem));
        break;
    }
}

static void x86StoreMem(X86Ctx *ctx, int size, X86Reg src, AsmOperand mem) {
    E2(ctx, X86MN_MOV, mem, x86MirGp(size, src));
}

static void x86FrameLoad(X86Ctx *ctx, X86Reg reg, int size, int loff) {
    x86LoadMem(ctx, size, reg, mir_mem(mir_r64(R_RBP), loff));
}

/* Sign- and zero-extending sub-word loads - see x86_64LoadSx /
 * x86_64LoadZx in the AOT backend. Only called for sizes below 8. */
static void x86FrameLoadSx(X86Ctx *ctx, X86Reg reg, int size, int loff) {
    int hint = size == 1 ? 1 : size == 2 ? 2 : 4;
    E2(ctx, X86MN_MOVSX, mir_r64(reg), mir_mem_hint(mir_mem(mir_r64(R_RBP), loff), hint));
}

static void x86FrameLoadZx(X86Ctx *ctx, X86Reg reg, int size, int loff) {
    AsmOperand mem = mir_mem(mir_r64(R_RBP), loff);
    if (size == 4) {
        /* A 32-bit mov zero-extends into the full 64. */
        E2(ctx, X86MN_MOV, mir_r32(reg), mem);
        return;
    }
    E2(ctx, X86MN_MOVZX, mir_r64(reg), mir_mem_hint(mem, size == 2 ? 2 : 1));
}

static void x86FrameStore(X86Ctx *ctx, X86Reg reg, int size, int loff) {
    x86StoreMem(ctx, size, reg, mir_mem(mir_r64(R_RBP), loff));
}

/* 1 for a double-width (F64) float value, 0 for single (F32). */
static int x86FpDbl(IrValue *v) { return (int)irValueByteSize(v) == 8; }

static void x86FpLoadMem(X86Ctx *ctx, int xmm, AsmOperand mem, int is_dbl) {
    E2(ctx, is_dbl ? X86MN_MOVSD : X86MN_MOVSS, mir_xmm(xmm), mem);
}

static void x86FpStoreMem(X86Ctx *ctx, int xmm, AsmOperand mem, int is_dbl) {
    E2(ctx, is_dbl ? X86MN_MOVSD : X86MN_MOVSS, mem, mir_xmm(xmm));
}

static void x86FpFrameLoad(X86Ctx *ctx, int xmm, int loff, int is_dbl) {
    x86FpLoadMem(ctx, xmm, mir_mem(mir_r64(R_RBP), loff), is_dbl);
}

static void x86FpFrameStore(X86Ctx *ctx, int xmm, int loff, int is_dbl) {
    x86FpStoreMem(ctx, xmm, mir_mem(mir_r64(R_RBP), loff), is_dbl);
}

/* ---------------- register moves ---------------- */

static void x86MovRegReg(X86Ctx *ctx, X86Reg dst, X86Reg src) {
    E2(ctx, X86MN_MOV, mir_r64(dst), mir_r64(src));
}

static void x86FpMovRegReg(X86Ctx *ctx, int dst_xmm, int src_xmm,
                           int is_dbl) {
    E2(ctx, is_dbl ? X86MN_MOVSD : X86MN_MOVSS, mir_xmm(dst_xmm), mir_xmm(src_xmm));
}

/* movq/movd between a GPR (holding float bits) and an xmm. */
static void x86BitsToXmm(X86Ctx *ctx, int xmm, X86Reg gpr, int is_dbl) {
    if (is_dbl)
        E2(ctx, X86MN_MOVQ, mir_xmm(xmm), mir_r64(gpr));
    else
        E2(ctx, X86MN_MOVD, mir_xmm(xmm), mir_r32(gpr));
}

static void x86XmmToBits(X86Ctx *ctx, X86Reg gpr, int xmm, int is_dbl) {
    if (is_dbl)
        E2(ctx, X86MN_MOVQ, mir_r64(gpr), mir_xmm(xmm));
    else
        E2(ctx, X86MN_MOVD, mir_r32(gpr), mir_xmm(xmm));
}

static void x86XmmZero(X86Ctx *ctx, int xmm, int is_dbl) {
    E2(ctx, is_dbl ? X86MN_XORPD : X86MN_XORPS, mir_xmm(xmm), mir_xmm(xmm));
}

/* ---------------- global addressing ----------------
 *
 * Three kinds of symbol need different treatment:
 *
 *  - Internal functions (defined in this TU): their address is the JIT
 *    mapping base + the label's byte_offset, but we don't know the base
 *    at emit time. `lea sym(%rip), reg` - the dispatch attaches the
 *    SIGNED rel32 fixup; the finalize step matches the label in
 *    enc->labels by name and patches the displacement (always
 *    reachable - same mapping).
 *
 *  - Internal globals/strings (registered in host_symbols by
 *    x86AllocateGlobals): we know the absolute address NOW because
 *    they live in a host malloc'd arena. Materialise it.
 *
 *  - External functions (printf/MAlloc/etc., dlsym-discoverable):
 *    materialise the dlsym address. Don't use RIP-relative here --
 *    libc may be beyond rel32's +/-2GB reach. */

/* Symbolic (RELOCATED) compilation normalises global names for the
 * assembler or object writer; ABSOLUTE mode wants the raw name - it is
 * the host-symbol key.
 *
 * A rename applies in BOTH, because it is not decoration: it says
 * which object this declaration means. Two C names carrying the same
 * `__asm("sym")` are one variable, and in the JIT that has to be one
 * host-symbol key or the second name looks up nothing and the code
 * loads through a null address. */
static const char *x86GlobalName(X86Ctx *ctx, AoStr *name) {
    if (ctx->jit->addr_mode == HCC_ADDR_RELOCATED)
        return asmNormaliseGlobalLabel(ctx->jit->cc, name)->data;
    const char *renamed = asmRenameFor(ctx->jit->cc, name);
    return renamed ? renamed : name->data;
}

static void x86GlobalAddrEx(X86Ctx *ctx, X86Reg reg, const char *sym,
                            int force_got) {
    HccJit *jit = ctx->jit;
    if (jit->addr_mode == HCC_ADDR_RELOCATED) {
        /* Everything is symbolic. A symbol from another image cannot
         * be reached PC-relatively - its address is not known until
         * the dynamic loader has placed the library - so it loads
         * from the GOT slot the linker allocates. `ld` rejects the
         * PC-relative form outright in a PIE ("relocation against
         * `free@@GLIBC' in read-only section"). Under `-fPIC` an
         * exported symbol defined HERE needs the same treatment - it
         * may be interposed at load time (see
         * asmSymbolIsPreemptible). The decoration rides
         * the operand text: the dispatch peels it off into an
         * AFR_X86_64_GOTPCREL fixup, the text sink prints the
         * `qword ptr [rip + sym@GOTPCREL]` spelling. */
        Cctrl *cc = jit->cc;
        int is_extern = force_got ||
                        (cc->extern_globals &&
                         setHas(cc->extern_globals, (char *)sym)) ||
                        asmSymbolIsPreemptible(cc, sym);
        if (is_extern) {
            char dec[512];
            snprintf(dec, sizeof(dec), "%s@GOTPCREL", sym);
            E2(ctx, X86MN_MOV, mir_r64(reg), mir_sym(dec));
        } else {
            E2(ctx, X86MN_LEA, mir_r64(reg), mir_sym(sym));
        }
        return;
    }
    /* Internal function -> lea rip-rel; the dispatch attaches the
     * SIGNED rel32 fixup and the finalize step patches it against the
     * label (always reachable - same mapping). */
    if (hccJitIsInternalFunc(jit, sym)) {
        E2(ctx, X86MN_LEA, mir_r64(reg), mir_sym(sym));
        return;
    }
    /* Internal global / string / external function: address is known
     * now (host arena malloc, or dlsym for libc). Materialise it. */
    void *addr = mapGet(jit->host_symbols, (void *)sym);
    if (!addr) addr = asm_jit_dlsym_resolver(NULL, sym);
    if (!addr) {
        x86EmitMovImm(ctx, reg, 0);
        return;
    }
    x86EmitMovImm(ctx, reg, (s64)(u64)(uintptr_t)addr);
}

static void x86GlobalAddr(X86Ctx *ctx, X86Reg reg, const char *sym) {
    x86GlobalAddrEx(ctx, reg, sym, 0);
}

/* ---------------- load/store IR values ---------------- */

static void x86LoadToReg(X86Ctx *ctx, IrValue *val, X86Reg reg) {
    switch (val->kind) {
        case IR_VAL_CONST_INT:
            x86EmitMovImm(ctx, reg, val->as._i64);
            break;
        case IR_VAL_CONST_FLOAT:
            /* Rare bit-cast: materialise the bit pattern. */
            x86EmitMovImm(ctx, reg, (s64)(u64)ieee754_64(val->as._f64));
            break;
        case IR_VAL_CONST_STR:
            x86GlobalAddr(ctx, reg, val->as.str.label->data);
            break;
        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            if (val->pinned_reg) {
                X86Reg src = x86RegFromName(val->pinned_reg->data);
                if (src != reg) x86MovRegReg(ctx, reg, src);
                break;
            }
            if (val->loc.kind == IR_LOC_REG && val->loc.as.reg) {
                X86Reg src = x86RegFromName(val->loc.as.reg->data);
                if (src != reg) x86MovRegReg(ctx, reg, src);
                break;
            }
            /* irSpillWidth, not irValueByteSize: a TEMPORARY owns a
             * full 8-byte slot and the register that spilled into it
             * already held a value canonical for its type, so all 64
             * bits move back. Reading only the tagged width re-derives
             * the upper bits from the load - and a sub-word load
             * ZERO-extends - which turned every signed narrow round
             * trip through a slot into an unsigned one. */
            int size = (int)irSpillWidth(val);
            int loff = irCgGetLoff(&ctx->fn->ra, val);
            /* See x86_64LoadToReg - a narrow local or param has to
             * arrive canonical for its own width, because the widen
             * that would otherwise fix it up is not guaranteed to
             * survive irFoldExtendTruncate. The plain load spells 1/2
             * zero-extending and 4 sign-extending, so neither
             * signedness is served by it alone. */
            if ((val->kind == IR_VAL_LOCAL || val->kind == IR_VAL_PARAM) &&
                size < 8 && irIsInt(val->type))
            {
                if (val->flags & IR_VAL_FLAG_UNSIGNED)
                    x86FrameLoadZx(ctx, reg, size, loff);
                else
                    x86FrameLoadSx(ctx, reg, size, loff);
                break;
            }
            x86FrameLoad(ctx, reg, size, loff);
            break;
        }
        case IR_VAL_GLOBAL:
            x86GlobalAddr(ctx, reg,
                          x86GlobalName(ctx, val->as.global.name));
            break;
        default:
            loggerPanic("jit-x86_64: cannot load int value of kind %d\n", val->kind);
    }
}

static void x86LoadToFpr(X86Ctx *ctx, IrValue *val, int xmm) {
    switch (val->kind) {
        case IR_VAL_CONST_FLOAT:
        case IR_VAL_CONST_INT: {
            double f = val->kind == IR_VAL_CONST_INT
                       ? (double)val->as._i64
                       : val->as._f64;
            int is_dbl = (val->kind == IR_VAL_CONST_FLOAT) ? x86FpDbl(val) : 1;
            u64 bits = is_dbl ? (u64)ieee754_64(f)
                              : (u64)(u32)ieee754_32((f32)f);
            if (bits == 0) {
                x86XmmZero(ctx, xmm, is_dbl);
                break;
            }
            /* Materialise via rax = bit pattern, then movq/movd to xmm. */
            x86EmitMovImm(ctx, R_RAX, (s64)bits);
            x86BitsToXmm(ctx, xmm, R_RAX, is_dbl);
            break;
        }
        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            int is_dbl = x86FpDbl(val);
            /* TempleOS-pinned FP local: the value lives in the named
             * register (xmm for FP, a GPR holding the bits otherwise). */
            if (val->pinned_reg) {
                X86Reg src = x86RegFromName(val->pinned_reg->data);
                if (x86IsFpRegName(val->pinned_reg->data)) {
                    if ((int)src != xmm)
                        x86FpMovRegReg(ctx, xmm, (int)src, is_dbl);
                } else {
                    x86BitsToXmm(ctx, xmm, src, is_dbl);
                }
                break;
            }
            if (val->loc.kind == IR_LOC_REG && val->loc.as.reg) {
                X86Reg src = x86RegFromName(val->loc.as.reg->data);
                if (x86IsFpRegName(val->loc.as.reg->data)) {
                    if ((int)src != xmm)
                        x86FpMovRegReg(ctx, xmm, (int)src, is_dbl);
                } else {
                    x86BitsToXmm(ctx, xmm, src, is_dbl);
                }
                break;
            }
            int loff = irCgGetLoff(&ctx->fn->ra, val);
            x86FpFrameLoad(ctx, xmm, loff, is_dbl);
            break;
        }
        default:
            loggerPanic("jit-x86_64: cannot load fp value of kind %d\n", val->kind);
    }
}

/* Mirrors x86_64StoreNarrowHome in the AOT backend, and
 * x86StoreNarrowHome in the aarch64 JIT.
 *
 * A narrow local PROMOTED out of its slot loses the truncation the
 * slot used to do for free, so the canonical value - sign-extended if
 * signed, zero-extended if unsigned - has to be produced here instead.
 * All four backends must agree or they differ in the upper half.
 *
 * The unsigned 4-byte case comes out as a 32-bit mov: writing a 32-bit
 * register zero-extends into the full 64 on x86-64, exactly as a
 * W-register write does on aarch64 - which is also why a `mov eax,
 * eax` self-move is NOT dead code. */
static int x86StoreNarrowHome(X86Ctx *ctx, IrValue *dst, X86Reg home,
                              X86Reg reg)
{
    if (dst->kind != IR_VAL_LOCAL && dst->kind != IR_VAL_PARAM) return 0;
    if (!irIsInt(dst->type)) return 0;
    u32 sz = irValueByteSize(dst);
    if (sz != 1 && sz != 2 && sz != 4) return 0;
    MirFn *m = ctx->mir;
    if (dst->flags & IR_VAL_FLAG_UNSIGNED) {
        if (sz == 4)
            x86MirLine(ctx, mir2(m, X86MN_MOV, mir_r32(home), mir_r32(reg)));
        else
            x86MirLine(ctx, mir2(m, X86MN_MOVZX, mir_r64(home),
                                 x86MirGp((int)sz, reg)));
    } else {
        /* sz 4 is movsxd (the dispatch spells movsx r64, r32). */
        x86MirLine(ctx, mir2(m, X86MN_MOVSX, mir_r64(home),
                             x86MirGp((int)sz, reg)));
    }
    return 1;
}

static void x86StoreReg(X86Ctx *ctx, IrValue *dst, X86Reg reg) {
    if (dst->pinned_reg) {
        X86Reg home = x86RegFromName(dst->pinned_reg->data);
        if (x86StoreNarrowHome(ctx, dst, home, reg)) return;
        if (home != reg) x86MovRegReg(ctx, home, reg);
        return;
    }
    if (dst->loc.kind == IR_LOC_REG && dst->loc.as.reg) {
        X86Reg home = x86RegFromName(dst->loc.as.reg->data);
        if (x86StoreNarrowHome(ctx, dst, home, reg)) return;
        if (home != reg) x86MovRegReg(ctx, home, reg);
        return;
    }
    /* irSpillWidth - a temporary's slot is a full 8 bytes and holds an
     * already-canonical value; a local keeps its declared width, since
     * writing eight bytes would tread on its neighbours. */
    int size = (int)irSpillWidth(dst);
    int loff = irCgGetLoff(&ctx->fn->ra, dst);
    x86FrameStore(ctx, reg, size, loff);
}

/* `val_size` (0 = derive from dst) is the stored value's width. When dst is
 * a gep/pointer tmp aliasing a narrow field, sizing from dst (8 bytes) would
 * clobber the neighbouring field; pass the value width so a F32 field store
 * is a 4-byte movss not an 8-byte movsd. */
static void x86StoreFprSized(X86Ctx *ctx, IrValue *dst, int xmm,
                             int val_size) {
    int is_dbl = val_size > 0 ? (val_size == 8) : x86FpDbl(dst);
    if (dst->pinned_reg) {
        X86Reg home = x86RegFromName(dst->pinned_reg->data);
        if (x86IsFpRegName(dst->pinned_reg->data)) {
            if ((int)home != xmm) x86FpMovRegReg(ctx, (int)home, xmm, is_dbl);
        } else {
            x86XmmToBits(ctx, home, xmm, is_dbl);
        }
        return;
    }
    if (dst->loc.kind == IR_LOC_REG && dst->loc.as.reg) {
        X86Reg home = x86RegFromName(dst->loc.as.reg->data);
        if (x86IsFpRegName(dst->loc.as.reg->data)) {
            if ((int)home != xmm) x86FpMovRegReg(ctx, (int)home, xmm, is_dbl);
        } else {
            x86XmmToBits(ctx, home, xmm, is_dbl);
        }
        return;
    }
    int loff = irCgGetLoff(&ctx->fn->ra, dst);
    x86FpFrameStore(ctx, xmm, loff, is_dbl);
}

static void x86StoreFpr(X86Ctx *ctx, IrValue *dst, int xmm) {
    x86StoreFprSized(ctx, dst, xmm, 0);
}

static void x86SpillDst(X86Ctx *ctx, IrInstr *instr, X86Reg reg) {
    x86StoreReg(ctx, instr->dst, reg);
}

static void x86SpillDstFpr(X86Ctx *ctx, IrInstr *instr, int xmm) {
    x86StoreFpr(ctx, instr->dst, xmm);
}

static void x86LoadFirstSrc(X86Ctx *ctx, IrValue *src) {
    x86LoadToReg(ctx, src, R_RAX);
}

static void x86LoadFirstSrcFpr(X86Ctx *ctx, IrValue *src) {
    x86LoadToFpr(ctx, src, 0 /* xmm0 */);
}

/* idx register for addressing-mode fusion. Returns 1 if `*out` was
 * populated, 0 if no idx component. */
static int x86IdxReg(X86Ctx *ctx, IrInstr *instr, X86Reg *out) {
    if (!instr->idx || !instr->scale) return 0;
    if (instr->idx->loc.kind == IR_LOC_REG && instr->idx->loc.as.reg) {
        *out = x86RegFromName(instr->idx->loc.as.reg->data);
        return 1;
    }
    x86LoadToReg(ctx, instr->idx, R_RDX);
    *out = R_RDX;
    return 1;
}

/* ---------------- condition codes ----------------
 *
 * The float column follows ucomisd, whose flags use unsigned-int
 * semantics (b/be/a/ae). NaN sets CF=ZF=PF=1; we treat IR cmps as
 * loose-ordered (matches the AOT backend and aarch64). */

static int x86CcFor(IrCmpKind cmp, int is_float) {
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

static int x86CcInvFor(IrCmpKind cmp, int is_float) {
    /* All the codes we use pair up as cc ^ 1 (e/ne, b/ae, l/ge, ...). */
    return x86CcFor(cmp, is_float) ^ 1;
}

/* setcc al; movzx rax, al - the SETcc ids are cc-ordered, so the
 * mnemonic is an offset from SETO. */
static void x86EmitSetCC(X86Ctx *ctx, IrCmpKind cmp, int is_float) {
    E1(ctx, X86MN_SETO + x86CcFor(cmp, is_float), mir_r8(R_RAX));
    E2(ctx, X86MN_MOVZX, mir_r64(R_RAX), mir_r8(R_RAX));
}

/* ---------------- phi materialisation ---------------- */

static void x86EmitOnePhi(X86Ctx *ctx, IrInstr *phi, IrPair *match) {
    IrValue *v = match ? match->ir_value : NULL;
    int v_dangling = irIsTmp(v) &&
                     v->loc.kind != IR_LOC_REG &&
                     !mapHasInt(ctx->fn->ra.id_to_loff, irVarId(v));
    if (irIsFloat(phi->dst->type)) {
        if (v_dangling) {
            x86XmmZero(ctx, 0, 1);
        } else {
            x86LoadToFpr(ctx, v, 0 /* xmm0 */);
        }
        x86StoreFpr(ctx, phi->dst, 0 /* xmm0 */);
        return;
    }
    if (v_dangling) {
        x86EmitMovImm(ctx, R_RAX, 0);
    } else {
        x86LoadToReg(ctx, v, R_RAX);
    }
    x86StoreReg(ctx, phi->dst, R_RAX);
}

static void x86EmitOnePhiCb(void *ud, IrInstr *phi, IrPair *match) {
    x86EmitOnePhi((X86Ctx *)ud, phi, match);
}

static void x86PhiMaterialise(X86Ctx *ctx, IrBlock *from, IrBlock *to) {
    hccJitPhiMaterialise(from, to, x86EmitOnePhiCb, ctx);
}

/* ---------------- call args partition ----------------
 *
 * HolyC's variadic convention sends every arg past var_arg_start
 * through the stack regardless of register availability (unlike
 * C-style variadic, which uses normal regs until they run out). */

static s32 x86PartitionCallArgs(u8 *is_stack, u64 n, Vec *args,
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

/* `jmp @@N` / `jcc @@N`: the dispatch encodes the rel32 placeholder and
 * attaches the AF_LOCAL fixup the JIT finalize patches. */
static void x86EmitBranchLocal(X86Ctx *ctx, int local_num) {
    E1(ctx, X86MN_JMP, mir_label_local(local_num));
}

static void x86EmitJccLocal(X86Ctx *ctx, int cc, int local_num) {
    E1(ctx, x86MirJccMn(cc), mir_label_local(local_num));
}

/* rax -> xmm0. x86 only has signed conversions before AVX-512, so a U64
 * with the top bit set is halved, converted and doubled. The bit the halving
 * drops is ORed back in so that rounding still sees it. */
static void x86U64ToFp(X86Ctx *ctx, int is_dbl) {
    int cvt = is_dbl ? X86MN_CVTSI2SD : X86MN_CVTSI2SS;
    int big = hccJitFreshLocalNum(ctx->jit);
    int done = hccJitFreshLocalNum(ctx->jit);

    E2(ctx, X86MN_TEST, mir_r64(R_RAX), mir_r64(R_RAX));
    x86EmitJccLocal(ctx, X86_CC_S, big);
    E2(ctx, cvt, mir_xmm(0), mir_r64(R_RAX));
    x86EmitBranchLocal(ctx, done);
    x86MirLocalLabel(ctx, big);
    E2(ctx, X86MN_MOV, mir_r64(R_RCX), mir_r64(R_RAX));
    E2(ctx, X86MN_SHR, mir_r64(R_RCX), mir_imm(1));
    E2(ctx, X86MN_AND, mir_r64(R_RAX), mir_imm(1));
    E2(ctx, X86MN_OR, mir_r64(R_RCX), mir_r64(R_RAX));
    E2(ctx, cvt, mir_xmm(0), mir_r64(R_RCX));
    E2(ctx, is_dbl ? X86MN_ADDSD : X86MN_ADDSS, mir_xmm(0), mir_xmm(0));
    x86MirLocalLabel(ctx, done);
}

/* xmm0 -> rax. The signed conversion stops at 2^63, so anything from there
 * up has 2^63 taken off first and the top bit put back afterwards. */
static void x86FpToU64(X86Ctx *ctx, int is_dbl) {
    int big = hccJitFreshLocalNum(ctx->jit);
    int done = hccJitFreshLocalNum(ctx->jit);

    if (!is_dbl) {
        E2(ctx, X86MN_CVTSS2SD, mir_xmm(0), mir_xmm(0));
    }
    x86EmitMovImm(ctx, R_RAX, 0x43E0000000000000); /* 2^63 as an F64 */
    E2(ctx, X86MN_MOVQ, mir_xmm(1), mir_r64(R_RAX));
    E2(ctx, X86MN_UCOMISD, mir_xmm(0), mir_xmm(1));
    x86EmitJccLocal(ctx, X86_CC_AE, big);
    E2(ctx, X86MN_CVTTSD2SI, mir_r64(R_RAX), mir_xmm(0));
    x86EmitBranchLocal(ctx, done);
    x86MirLocalLabel(ctx, big);
    E2(ctx, X86MN_SUBSD, mir_xmm(0), mir_xmm(1));
    E2(ctx, X86MN_CVTTSD2SI, mir_r64(R_RAX), mir_xmm(0));
    x86EmitMovImm(ctx, R_RCX, INT64_MIN);
    E2(ctx, X86MN_XOR, mir_r64(R_RAX), mir_r64(R_RCX));
    x86MirLocalLabel(ctx, done);
}

static void x86EmitCallSym(X86Ctx *ctx, const char *sym) {
    E1(ctx, X86MN_CALL, mir_sym(sym));
}

/* ---------------- System V struct-by-value passing ----------------
 * Mirrors x86_64.c's x86_64EmitSysvCall / x86_64EmitSysvParamPrologue for
 * the JIT (machine-code) path: eightbyte INTEGER->GP / SSE->XMM, MEMORY
 * (>16B) on the stack by value. Struct returns keep the hidden-out-ptr. */

static int x86IsByvalStruct(AstType *t) {
    return t && (t->kind == AST_TYPE_CLASS || t->kind == AST_TYPE_UNION) &&
           !t->is_intrinsic;
}

static void x86EmitSysvCall(X86Ctx *ctx, IrInstr *instr, Vec *args,
                            AoStr *fname, int indirect,
                            int holyc_variadic, int var_arg_start)
{
    u64 n = args ? args->size : 0;

    /* A <=16-byte aggregate return comes back in registers; its destination
     * buffer (args[0]) is not an ABI argument. */
    AstType *ret_st = (instr->flags & IRCG_CALL_AGG_RETURN) && instr->dst
                      ? instr->dst->byval_struct_type : NULL;
    /* The result IS this function's return value and the `ret` is next,
     * so the callee has already left the bytes in rax/rdx (or xmm0/1).
     * The buffer is gone from the IR along with the address that
     * reached it - which is why nothing below may go looking for it.
     * args[0] is still skipped as an argument: it was never one. */
    int fwd_ret = (instr->flags & IRCG_AGG_RET_FORWARD) != 0;
    int reg_ret = ret_st && ret_st->size <= 16 && !fwd_ret;
    u64 a0 = (ret_st && ret_st->size <= 16) ? 1 : 0;

    /* HolyC's `auto ...` sends every slot past var_arg_start through
     * the stack whatever registers are free - the rule the simple call
     * path applies in x86PartitionCallArgs. This path has to apply it
     * too, or a variadic function that returns a struct finds its slots
     * in registers while reading them from the stack. args[0] is the
     * return buffer whenever there is one - skipped for a register
     * return (a0 = 1), passed in %rdi for a memory return (a0 = 0) -
     * and either way it sits in front of the callee's parameters. */
    int tail_start = (holyc_variadic && var_arg_start >= 0)
                     ? (ret_st ? 1 : 0) + var_arg_start : -1;

    /* Pre-pass: size the stack-argument area (MEMORY / register-overflow
     * aggregates + overflow scalars). */
    int nsaa_total = 0;
    {
        int g = 0, s = 0;
        for (u64 i = a0; i < n; ++i) {
            IrValue *a = vecGet(IrValue *, args, i);
            int forced = tail_start >= 0 && (int)i >= tail_start;
            if (a->byval_struct_type) {
                AstType *t = a->byval_struct_type;
                SysvClass cl[2]; int neb = 0;
                int mem = astSysvClassify(t, cl, &neb);
                int gp = 0, sse = 0;
                if (!mem) {
                    for (int e = 0; e < neb; ++e) {
                        if (cl[e] == SYSV_INTEGER) {
                            gp++;
                        } else {
                            sse++;
                        }
                    }
                }
                if (!mem && g + gp <= 6 && s + sse <= 8) {
                    g += gp;
                    s += sse;
                } else {
                    nsaa_total += (t->size + 7) & ~7;
                }
            } else if (irIsFloat(a->type)) {
                if   (!forced && s < 8) s++;
                else nsaa_total += 8;
            } else {
                if (!forced && g < 6) g++;
                else nsaa_total += 8;
            }
        }
    }
    /* Two 8-byte parking spaces above the stack-argument area - the
     * register-return destination pointer, and an indirect call's
     * target. Mirrors x86_64EmitSysvCall; see the reasoning there. The
     * target cannot stay in %r11 because the argument loop below uses
     * it to walk a by-value struct, and it cannot use %r10 because
     * %r10 is allocatable now. */
    int dest_save_off   = nsaa_total;
    int target_save_off = nsaa_total + (reg_ret ? 8 : 0);
    int save_bytes      = (reg_ret ? 8 : 0) + (indirect ? 8 : 0);
    int stack_bytes = (nsaa_total + save_bytes + 15) & ~15;
    if (stack_bytes > 0)
        E2(ctx, X86MN_SUB, mir_r64(R_RSP), mir_imm(stack_bytes));
    int stack_off = 0;

    /* Both stashes happen before the argument loop, while the values
     * they read are still where the IR says. Staged through %r11 and
     * NOT %rax - a fused producer may have left an argument in %rax.
     * See the AOT twin. */
    if (reg_ret) {
        x86LoadToReg(ctx, vecGet(IrValue *, args, 0), R_R11);
        x86StoreMem(ctx, 8, R_R11, x86MemOp(R_RSP, -1, 0, dest_save_off));
    }
    if (indirect) {
        if (!instr->r2) {
            loggerPanic("jit-x86_64: indirect IR_CALL without target\n");
        }
        x86LoadToReg(ctx, instr->r2, R_R11);
        x86StoreMem(ctx, 8, R_R11, x86MemOp(R_RSP, -1, 0, target_save_off));
    }

    int ngp = 0, nsse = 0;
    for (u64 i = a0; i < n; ++i) {
        IrValue *a = vecGet(IrValue *, args, i);
        int forced = tail_start >= 0 && (int)i >= tail_start;
        if (a->byval_struct_type) {
            AstType *t = a->byval_struct_type;
            SysvClass cl[2]; int neb = 0;
            int mem = astSysvClassify(t, cl, &neb);
            int gp = 0, sse = 0;
            if (!mem) {
                for (int e = 0; e < neb; ++e) {
                    if (cl[e] == SYSV_INTEGER) {
                        gp++;
                    } else {
                        sse++;
                    }
                }
            }
            x86LoadToReg(ctx, a, R_R11); /* struct address - see the AOT twin */
            if (mem || ngp + gp > 6 || nsse + sse > 8) {
                int words = (t->size + 7) / 8;
                for (int k = 0; k < words; ++k) {
                    int off = k * 8;
                    int rem = t->size - off;
                    int sz = rem >= 8 ? 8 : rem;
                    x86LoadMem(ctx, sz, R_RAX, x86MemOp(R_R11, -1, 0, off));
                    x86StoreMem(ctx, sz, R_RAX,
                                x86MemOp(R_RSP, -1, 0, stack_off + off));
                }
                stack_off += (t->size + 7) & ~7;
                continue;
            }
            for (int e = 0; e < neb; ++e) {
                int off = e * 8;
                int rem = t->size - off;
                int sz = rem >= 8 ? 8 : rem;
                if (cl[e] == SYSV_SSE) {
                    x86FpLoadMem(ctx, nsse, x86MemOp(R_R11, -1, 0, off),
                                 sz != 4);
                    nsse++;
                } else {
                    x86LoadMem(ctx, sz, kIntArgRegs[ngp],
                               x86MemOp(R_R11, -1, 0, off));
                    ngp++;
                }
            }
            continue;
        }
        if (irIsFloat(a->type)) {
            if (!forced && nsse < 8) {
                x86LoadToFpr(ctx, a, nsse++);
            } else {
                x86LoadToFpr(ctx, a, 0 /*xmm0*/);
                x86FpStoreMem(ctx, 0, x86MemOp(R_RSP, -1, 0, stack_off),
                              x86FpDbl(a));
                stack_off += 8;
            }
        } else {
            if (!forced && ngp < 6) {
                x86LoadToReg(ctx, a, kIntArgRegs[ngp++]);
            } else {
                x86LoadToReg(ctx, a, R_RAX);
                x86StoreMem(ctx, 8, R_RAX, x86MemOp(R_RSP, -1, 0, stack_off));
                stack_off += 8;
            }
        }
    }

    if (indirect) {
        E1(ctx, X86MN_CALL, mir_mem(mir_r64(R_RSP), target_save_off));
    } else {
        x86EmitCallSym(ctx, asmNormaliseFunctionName(ctx->jit->cc, fname));
    }
    if (reg_ret) {
        /* Reload the destination buffer and copy the SysV result registers
         * (INTEGER eightbytes -> rax,rdx; SSE -> xmm0,xmm1) into it. */
        SysvClass cl[2]; int neb = 0;
        astSysvClassify(ret_st, cl, &neb);
        int gpret[2] = { R_RAX, R_RDX };
        int gpi = 0, ssei = 0;
        x86LoadMem(ctx, 8, R_R11, x86MemOp(R_RSP, -1, 0, dest_save_off));
        for (int e = 0; e < neb; ++e) {
            int off = e * 8, rem = (int)ret_st->size - off;
            int sz = rem >= 8 ? 8 : rem;
            if (cl[e] == SYSV_SSE) {
                x86FpStoreMem(ctx, ssei, x86MemOp(R_R11, -1, 0, off),
                              sz != 4);
                ssei++;
            } else {
                x86StoreMem(ctx, sz, gpret[gpi], x86MemOp(R_R11, -1, 0, off));
                gpi++;
            }
        }
    }

    if (stack_bytes > 0)
        E2(ctx, X86MN_ADD, mir_r64(R_RSP), mir_imm(stack_bytes));

    if (reg_ret) {
        if (instr->dst) x86SpillDst(ctx, instr, R_R11);  /* dst = buffer addr */
    } else if (!fwd_ret && instr->dst && instr->dst->type != IR_TYPE_VOID) {
        if (irIsFloat(instr->dst->type)) x86SpillDstFpr(ctx, instr, 0);
        else                             x86SpillDst(ctx, instr, R_RAX);
    }
}

/* Spill the argument registers into the register save area and build
 * the va_list record. The AOT twin is x86_64EmitVaSaveArea; the frame
 * offsets both use come from x86_64.h, so the two cannot drift.
 *
 * Runs before the parameter prologue, so every incoming register is
 * captured untouched, and spills unconditionally: the psABI lets a
 * callee read %al and skip the eight SSE stores when no vector
 * registers were passed, but that is an optimisation - storing a
 * register nobody passed writes a slot nobody reads. */
static void x86EmitVaSaveArea(X86Ctx *ctx) {
    u32 stack = ctx->fn->stack_space;
    int save = x86_64VaSaveAreaOff(stack);
    int valist = x86_64VaListOff(stack);
    MirFn *m = ctx->mir;

    for (int i = 0; i < 6; ++i) {
        x86StoreMem(ctx, 8, kIntArgRegs[i],
                    x86MemOp(R_RBP, -1, 0, save + i * 8));
    }
    /* Eight bytes per xmm, at the psABI's sixteen-byte stride. Only the
     * low half of each slot is ever read - <stdarg.h> takes a `double`
     * out of it and we have no type wider than that going through
     * `...` - so the upper half is left alone rather than requiring a
     * 16-byte move. */
    for (int i = 0; i < 8; ++i) {
        x86FpStoreMem(ctx, i, x86MemOp(R_RBP, -1, 0, save + 48 + i * 16), 1);
    }

    int ngp, nsse, overflow;
    x86_64VaNamedCounts(ctx->ast, &ngp, &nsse, &overflow);

    /* gp_offset / fp_offset start PAST the named parameters - those
     * registers were spilled too, but they have already been read.
     * Via rax: there is no store-immediate-to-memory spelling here. */
    x86EmitMovImm(ctx, R_RAX, (s64)(ngp * 8));
    x86StoreMem(ctx, 4, R_RAX, x86MemOp(R_RBP, -1, 0, valist));
    x86EmitMovImm(ctx, R_RAX, (s64)(48 + nsse * 16));
    x86StoreMem(ctx, 4, R_RAX, x86MemOp(R_RBP, -1, 0, valist + 4));

    /* The overflow area begins after the return address and saved rbp,
     * then past any named parameter that did not fit in a register. */
    x86MirLine(ctx, mir2(m, X86MN_LEA, mir_r64(R_RAX),
                         mir_mem(mir_r64(R_RBP), 16 + overflow)));
    x86StoreMem(ctx, 8, R_RAX, x86MemOp(R_RBP, -1, 0, valist + 8));

    x86MirLine(ctx, mir2(m, X86MN_LEA, mir_r64(R_RAX),
                         mir_mem(mir_r64(R_RBP), save)));
    x86StoreMem(ctx, 8, R_RAX, x86MemOp(R_RBP, -1, 0, valist + 16));
}

static void x86EmitSysvParamPrologue(X86Ctx *ctx) {
    Ast *ast = ctx->ast;
    if (!ast->params) return;
    int ngp = 0, nsse = 0;

    AstType *rt = ast->type ? ast->type->rettype : NULL;
    /* Only an INDIRECT (>16-byte) struct return uses a hidden
     * out-pointer in rdi; a <=16-byte aggregate comes back in rax/rdx
     * or xmm0/xmm1 and consumes no argument register. Claiming one for
     * every struct return shifted every parameter down by a register,
     * so a function taking a small struct by value read its arguments
     * from the wrong places entirely. */
    if (x86IsByvalStruct(rt) && rt->size > 16) ngp = 1; /* hidden ret ptr */
    int incoming_off = 0;

    for (u64 i = 0; i < ast->params->size; ++i) {
        Ast *p = vecGet(Ast *, ast->params, i);
        if (p->kind == AST_VAR_ARGS) break;
        AstType *t = p->type;

        IrValue *slot = irFnGetVar(ctx->fn, irGetParamId(p));
        int has_slot = slot &&
            mapHasInt(ctx->fn->ra.id_to_loff, irVarId(slot));
        int loff = has_slot ? irCgGetLoff(&ctx->fn->ra, slot) : 0;

        if (!x86IsByvalStruct(t)) {
            int is_float = (t->kind == AST_TYPE_FLOAT);
            if (is_float ? (nsse < 8) : (ngp < 6)) {
                if (is_float) nsse++; else ngp++; /* reg: arrive+store */
            } else {
                if (has_slot) {
                    int sz = t->size;
                    x86LoadMem(ctx, sz, R_RAX,
                               x86MemOp(R_RBP, -1, 0, 16 + incoming_off));
                    x86FrameStore(ctx, R_RAX, sz, loff);
                }
                incoming_off += 8;
            }
            continue;
        }

        SysvClass cl[2]; int neb = 0;
        int mem = astSysvClassify(t, cl, &neb);

        int gp = 0, sse = 0;
        if (!mem) {
            for (int e = 0; e < neb; ++e) {
                if (cl[e] == SYSV_INTEGER) {
                    gp++;
                } else {
                    sse++;
                }
            }
        }

        if (mem || ngp + gp > 6 || nsse + sse > 8) {
            if (has_slot) {
                int words = (t->size + 7) / 8;
                for (int k = 0; k < words; ++k) {
                    int off = k * 8;
                    int rem = t->size - off;
                    int sz = rem >= 8 ? 8 : rem;
                    x86LoadMem(ctx, sz, R_RAX,
                               x86MemOp(R_RBP, -1, 0, 16 + incoming_off + off));
                    x86FrameStore(ctx, R_RAX, sz, loff + off);
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
                if (has_slot) x86FpFrameStore(ctx, nsse, loff + off, sz == 8);
                nsse++;
            } else {
                if (has_slot)
                    x86FrameStore(ctx, kIntArgRegs[ngp], sz, loff + off);
                ngp++;
            }
        }
    }
}

/* ---------------- the main IR emitter ---------------- */

static void x86EmitInstr(X86Ctx *ctx, IrInstr *instr) {
    HccJit *jit = ctx->jit;
    MirFn *m = ctx->mir;

    switch (instr->op) {
        case IR_NOP:
        case IR_LABEL:
        case IR_ALLOCA:
        case IR_GEP:
            break;

        case IR_LOAD: {
            if (instr->r1 && instr->r1->pinned_reg) {
                X86Reg src = x86RegFromName(instr->r1->pinned_reg->data);
                /* A float home is an xmm register: copied as one, or the
                 * move reads the GPR of the same number instead. */
                if (x86IsFpRegName(instr->r1->pinned_reg->data) ||
                    (instr->dst && irIsFloat(instr->dst->type)))
                {
                    x86FpMovRegReg(ctx, 0, (int)src, x86FpDbl(instr->dst));
                    x86SpillDstFpr(ctx, instr, 0);
                    break;
                }
                x86MovRegReg(ctx, R_RAX, src);
                x86SpillDst(ctx, instr, R_RAX);
                break;
            }
            if (instr->dst && irIsFloat(instr->dst->type)) {
                int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
                x86FpFrameLoad(ctx, 0 /* xmm0 */, loff, x86FpDbl(instr->dst));
                x86SpillDstFpr(ctx, instr, 0);
                break;
            }
            int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
            int size = instr->dst ? (int)instr->dst->as.var.size : 8;
            /* Canonicality on the way out of the slot - see the AOT
             * twin. The signedness is the SOURCE's; a temporary carries
             * none and needs none. */
            if (instr->r1 &&
                (instr->r1->kind == IR_VAL_LOCAL ||
                 instr->r1->kind == IR_VAL_PARAM) &&
                size < 8 && irIsInt(instr->r1->type))
            {
                if (instr->r1->flags & IR_VAL_FLAG_UNSIGNED)
                    x86FrameLoadZx(ctx, R_RAX, size, loff);
                else
                    x86FrameLoadSx(ctx, R_RAX, size, loff);
            } else {
                x86FrameLoad(ctx, R_RAX, size, loff);
            }
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_STORE: {
            X86Reg src_reg = R_RAX;
            int have_src = 0;
            if (instr->r1 && instr->r1->loc.kind == IR_LOC_REG &&
                instr->r1->loc.as.reg)
            {
                src_reg = x86RegFromName(instr->r1->loc.as.reg->data);
                have_src = 1;
            }
            if (instr->r1 && irIsFloat(instr->r1->type))
            {
                int src_xmm = 0;
                if (have_src && x86IsFpRegName(instr->r1->loc.as.reg->data)) {
                    src_xmm = (int)src_reg;
                } else {
                    x86LoadFirstSrcFpr(ctx, instr->r1);
                }
                x86StoreFprSized(ctx, instr->dst, src_xmm,
                                 (int)irValueByteSize(instr->r1));
                break;
            }
            if (!have_src) { x86LoadFirstSrc(ctx, instr->r1); src_reg = R_RAX; }
            /* Pinned destination lives in a register, not a slot - so
             * check it before asking for a (nonexistent) loff. A narrow
             * destination is canonicalised on the way in; see the AOT
             * twin for why a register home cannot skip that. */
            if (instr->dst && instr->dst->pinned_reg) {
                X86Reg home = x86RegFromName(instr->dst->pinned_reg->data);
                if (x86StoreNarrowHome(ctx, instr->dst, home, src_reg)) break;
                if (home != src_reg) x86MovRegReg(ctx, home, src_reg);
                break;
            }
            int loff = irCgGetLoff(&ctx->fn->ra, instr->dst);
            int size = (int)irValueByteSize(instr->r1);
            x86FrameStore(ctx, src_reg, size, loff);
            break;
        }

        case IR_LOAD_DEREF: {
            /* A GLOBAL base is just an address like any other once it is
             * in a register, so it goes through the common path below
             * rather than a branch of its own - which is how IR_RMW_DEREF
             * already did it, and why that op was not affected.
             *
             * The branch this replaces passed a hardcoded disp of 0, so
             * a constant offset from the symbol was silently dropped:
             * reading `g.e[2]` read `g.e[0]`. The AOT backend threads
             * instr->disp into its RIP-relative form for the same
             * reason. */
            X86Reg base = R_RCX;
            if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
                x86GlobalAddr(ctx, R_RCX,
                              x86GlobalName(ctx, instr->r1->as.global.name));
            } else if (instr->r1 && instr->r1->loc.kind == IR_LOC_REG &&
                       instr->r1->loc.as.reg)
            {
                base = x86RegFromName(instr->r1->loc.as.reg->data);
            } else {
                x86LoadToReg(ctx, instr->r1, R_RCX);
            }
            X86Reg idx; int has_idx = x86IdxReg(ctx, instr, &idx);
            AsmOperand mem = x86MemOp(base, has_idx ? (int)idx : -1,
                                      instr->scale, instr->disp);
            if (instr->dst && irIsFloat(instr->dst->type)) {
                x86FpLoadMem(ctx, 0, mem, x86FpDbl(instr->dst));
                x86SpillDstFpr(ctx, instr, 0);
                break;
            }
            int size = instr->dst ? (int)instr->dst->as.var.size : 8;
            x86LoadMem(ctx, size, R_RAX, mem);
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_STORE_DEREF: {
            /* As in IR_LOAD_DEREF: a global base joins the common path
             * instead of a branch that dropped instr->disp, which wrote
             * `g.e[0]` for every `g.e[<constant>]`. */
            X86Reg base = R_RCX;
            int addr_in_reg = instr->dst && instr->dst->loc.kind == IR_LOC_REG &&
                              instr->dst->loc.as.reg;
            int addr_is_global = !addr_in_reg && instr->dst &&
                                 instr->dst->kind == IR_VAL_GLOBAL;
            if (addr_in_reg) base = x86RegFromName(instr->dst->loc.as.reg->data);
            else if (addr_is_global)
                x86GlobalAddr(ctx, R_RCX,
                              x86GlobalName(ctx, instr->dst->as.global.name));
            X86Reg idx; int has_idx = x86IdxReg(ctx, instr, &idx);
            if (!addr_in_reg && !addr_is_global)
                x86LoadToReg(ctx, instr->dst, R_RCX);
            if (instr->r1 && irIsFloat(instr->r1->type)) {
                /* Materialising a float value can clobber RAX (a const is
                 * built with a wide mov + movq through RAX - the JIT has
                 * no rodata to RIP-load from like the AOT backend does).
                 * If the destination address sits in RAX (e.g. a local
                 * array element whose `lea` result was pinned there),
                 * that store target would be destroyed. Relocate base /
                 * idx out of RAX first. */
                if (base == R_RAX) {
                    X86Reg t = (has_idx && idx == R_RCX) ? R_RDX : R_RCX;
                    x86MovRegReg(ctx, t, R_RAX);
                    base = t;
                }
                if (has_idx && idx == R_RAX) {
                    X86Reg t = (base == R_RCX) ? R_RDX : R_RCX;
                    x86MovRegReg(ctx, t, R_RAX);
                    idx = t;
                }
                x86LoadFirstSrcFpr(ctx, instr->r1);
                x86FpStoreMem(ctx, 0,
                              x86MemOp(base, has_idx ? (int)idx : -1,
                                       instr->scale, instr->disp),
                              x86FpDbl(instr->r1));
                break;
            }
            int sz = (int)irValueByteSize(instr->r1);
            /* Pick a val reg that conflicts with neither base nor idx.
             * The last resort is %r11, which cannot be either: the
             * allocator never hands it out and no path here loads an
             * address into it. (%r10 used to serve that role and is now
             * allocatable - see x86_64InitRegPool.) */
            X86Reg val = addr_in_reg ? R_RCX : R_RAX;
            if (has_idx && val == idx) val = R_RDX == base ? R_R11 : R_RDX;
            if (val == base) val = (has_idx && idx == R_RDX) ? R_R11 : R_RDX;
            x86LoadToReg(ctx, instr->r1, val);
            x86StoreMem(ctx, sz, val,
                        x86MemOp(base, has_idx ? (int)idx : -1,
                                 instr->scale, instr->disp));
            break;
        }

        case IR_RMW_DEREF: {
            X86Reg base = R_RCX;
            int addr_in_reg = instr->dst && instr->dst->loc.kind == IR_LOC_REG &&
                              instr->dst->loc.as.reg;
            if (addr_in_reg) base = x86RegFromName(instr->dst->loc.as.reg->data);
            else if (instr->dst && instr->dst->kind == IR_VAL_GLOBAL)
                x86GlobalAddr(ctx, R_RCX,
                              x86GlobalName(ctx, instr->dst->as.global.name));
            else x86LoadToReg(ctx, instr->dst, R_RCX);

            X86Reg idx; int has_idx = x86IdxReg(ctx, instr, &idx);
            int sz = (int)irValueByteSize(instr->r1);
            /* Load-modify-store needs TWO scratch registers live at
             * once - the accumulator holding the loaded value, and the
             * right-hand side - and neither may alias base or idx.
             *
             * %rax and %r11 are exactly the two the allocator never
             * hands out, and no path above loads an address into
             * either: `base` is an allocated register, %rcx, or a
             * global materialised into %rcx; `idx` is an allocated
             * register or %rdx. So this pair is always available and
             * always distinct. (%r10 used to be the rhs on the same
             * reasoning - it is allocatable now, so it no longer
             * qualifies.) */
            X86Reg acc = R_RAX, rhs = R_R11;
            AsmOperand mem = x86MemOp(base, has_idx ? (int)idx : -1,
                                      instr->scale, instr->disp);
            x86LoadMem(ctx, sz, acc, mem);
            x86LoadToReg(ctx, instr->r1, rhs);
            int alu_mn = x86MirAluMn(instr->extra.rmw_op);
            if (alu_mn == X86MN_UNKNOWN) {
                loggerPanic("jit-x86_64: bad RMW op %d\n",
                            instr->extra.rmw_op);
            }
            x86MirLine(ctx, mir2(m, alu_mn, mir_r64(acc), mir_r64(rhs)));
            x86StoreMem(ctx, sz, acc, mem);
            break;
        }

        case IR_LEA: {
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
                    name = x86GlobalName(ctx, instr->r1->as.global.name);
                }
                x86GlobalAddrEx(ctx, R_RAX, name, got);
            } else if (instr->r1) {
                int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
                x86MirLine(ctx, mir2(m, X86MN_LEA, mir_r64(R_RAX),
                                     mir_mem(mir_r64(R_RBP), loff)));
            }
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_IADD:
        case IR_ISUB:
        case IR_AND:
        case IR_OR:
        case IR_XOR: {
            int alu_mn = x86MirAluMn(instr->op);
            /* 32-bit operand size when the IR says so - the AOT twin
             * picks `addl` over `addq` on the same test. */
            int w = x86_64OpIs32Bit(instr) ? 4 : 8;
            x86LoadFirstSrc(ctx, instr->r1);
            s64 imm;
            if (x86IsImm32(instr->r2, &imm)) {
                x86MirLine(ctx, mir2(m, alu_mn, x86MirGp(w, R_RAX),
                                     mir_imm(imm)));
            } else if (instr->r2->loc.kind == IR_LOC_REG &&
                       instr->r2->loc.as.reg)
            {
                X86Reg rhs = x86RegFromName(instr->r2->loc.as.reg->data);
                x86MirLine(ctx, mir2(m, alu_mn, x86MirGp(w, R_RAX),
                                     x86MirGp(w, rhs)));
            } else {
                x86LoadToReg(ctx, instr->r2, R_RCX);
                x86MirLine(ctx, mir2(m, alu_mn, x86MirGp(w, R_RAX),
                                     x86MirGp(w, R_RCX)));
            }
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_IMUL: {
            int w = x86_64OpIs32Bit(instr) ? 4 : 8;
            x86LoadFirstSrc(ctx, instr->r1);
            if (instr->r2->loc.kind == IR_LOC_REG && instr->r2->loc.as.reg) {
                X86Reg rhs = x86RegFromName(instr->r2->loc.as.reg->data);
                x86MirLine(ctx, mir2(m, X86MN_IMUL, x86MirGp(w, R_RAX),
                                     x86MirGp(w, rhs)));
            } else {
                x86LoadToReg(ctx, instr->r2, R_RCX);
                x86MirLine(ctx, mir2(m, X86MN_IMUL, x86MirGp(w, R_RAX),
                                     x86MirGp(w, R_RCX)));
            }
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_IDIV:
        case IR_UDIV:
        case IR_IREM:
        case IR_UREM: {
            /* idiv/div divide rdx:rax by the source operand. Quotient
             * lands in rax, remainder in rdx. */
            int is_signed = (instr->op == IR_IDIV || instr->op == IR_IREM);
            int is_rem    = (instr->op == IR_IREM || instr->op == IR_UREM);
            x86LoadFirstSrc(ctx, instr->r1);
            x86LoadToReg(ctx, instr->r2, R_RCX);
            if (is_signed) {
                x86MirLine(ctx, mir0(m, X86MN_CQO));
                x86MirLine(ctx, mir1(m, X86MN_IDIV, mir_r64(R_RCX)));
            } else {
                x86MirLine(ctx, mir2(m, X86MN_XOR, mir_r32(R_RDX),
                                     mir_r32(R_RDX)));
                x86MirLine(ctx, mir1(m, X86MN_DIV, mir_r64(R_RCX)));
            }
            if (is_rem) x86MovRegReg(ctx, R_RAX, R_RDX);
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_INEG:
            x86LoadFirstSrc(ctx, instr->r1);
            x86MirLine(ctx, mir1(m, X86MN_NEG,
                                 x86MirGp(x86_64OpIs32Bit(instr) ? 4 : 8,
                                          R_RAX)));
            x86SpillDst(ctx, instr, R_RAX);
            break;

        case IR_NOT:
            x86LoadFirstSrc(ctx, instr->r1);
            x86MirLine(ctx, mir1(m, X86MN_NOT,
                                 x86MirGp(x86_64OpIs32Bit(instr) ? 4 : 8,
                                          R_RAX)));
            x86SpillDst(ctx, instr, R_RAX);
            break;

        case IR_SHL:
        case IR_SHR:
        case IR_SAR: {
            int mn = x86MirShiftMn(instr->op);
            x86LoadFirstSrc(ctx, instr->r1);
            if (instr->r2 && instr->r2->kind == IR_VAL_CONST_INT &&
                instr->r2->as._i64 >= 0 && instr->r2->as._i64 < 64)
            {
                x86MirLine(ctx, mir2(m, mn, mir_r64(R_RAX),
                                     mir_imm(instr->r2->as._i64)));
            } else {
                /* Variable shift count must live in %cl. */
                x86LoadToReg(ctx, instr->r2, R_RCX);
                x86MirLine(ctx, mir2(m, mn, mir_r64(R_RAX), mir_r8(R_RCX)));
            }
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_FADD:
        case IR_FSUB:
        case IR_FMUL:
        case IR_FDIV: {
            int is_dbl = x86FpDbl(instr->dst);
            x86LoadFirstSrcFpr(ctx, instr->r1);          /* xmm0 */
            x86LoadToFpr(ctx, instr->r2, 1 /* xmm1 */);
            x86MirLine(ctx, mir2(m, x86MirFopMn(instr->op, is_dbl),
                                 mir_xmm(0), mir_xmm(1)));
            x86SpillDstFpr(ctx, instr, 0);
            break;
        }

        case IR_FNEG:
            /* No fneg on SSE - flip the sign bit by XORing in a mask
             * materialised through rax/xmm1. F64 flips bit 63, F32
             * flips bit 31, with the matching xorpd/xorps width. */
            x86LoadFirstSrcFpr(ctx, instr->r1);
            if (x86FpDbl(instr->dst)) {
                x86EmitMovImm(ctx, R_RAX, (s64)(u64)0x8000000000000000ULL);
                x86BitsToXmm(ctx, 1, R_RAX, 1);
                x86MirLine(ctx, mir2(m, X86MN_XORPD, mir_xmm(0), mir_xmm(1)));
            } else {
                x86EmitMovImm(ctx, R_RAX, (s64)(u64)0x80000000ULL);
                x86BitsToXmm(ctx, 1, R_RAX, 0);
                x86MirLine(ctx, mir2(m, X86MN_XORPS, mir_xmm(0), mir_xmm(1)));
            }
            x86SpillDstFpr(ctx, instr, 0);
            break;

        case IR_ICMP: {
            x86LoadFirstSrc(ctx, instr->r1);
            s64 imm;
            if (x86IsImm32(instr->r2, &imm)) {
                x86MirLine(ctx, mir2(m, X86MN_CMP, mir_r64(R_RAX),
                                     mir_imm(imm)));
            } else {
                x86LoadToReg(ctx, instr->r2, R_RCX);
                x86MirLine(ctx, mir2(m, X86MN_CMP, mir_r64(R_RAX),
                                     mir_r64(R_RCX)));
            }
            x86EmitSetCC(ctx, instr->extra.cmp_kind, 0);
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_FCMP: {
            int is_dbl = x86FpDbl(instr->r1);
            x86LoadFirstSrcFpr(ctx, instr->r1);
            x86LoadToFpr(ctx, instr->r2, 1 /* xmm1 */);
            x86MirLine(ctx, mir2(m, is_dbl ? X86MN_UCOMISD : X86MN_UCOMISS,
                                 mir_xmm(0), mir_xmm(1)));
            x86EmitSetCC(ctx, instr->extra.cmp_kind, 1);
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_CMP_BR: {
            IrCmpKind kind = instr->extra.cmp_br.cmp_kind;
            IrBlock *t = instr->extra.cmp_br.target_block;
            IrBlock *f = instr->extra.cmp_br.fallthrough_block;
            int is_float = instr->r1 && irIsFloat(instr->r1->type);
            if (is_float) {
                x86LoadFirstSrcFpr(ctx, instr->r1);
                x86LoadToFpr(ctx, instr->r2, 1 /* xmm1 */);
                x86MirLine(ctx, mir2(m, x86FpDbl(instr->r1) ? X86MN_UCOMISD
                                                            : X86MN_UCOMISS,
                                     mir_xmm(0), mir_xmm(1)));
            } else {
                x86LoadFirstSrc(ctx, instr->r1);
                s64 imm;
                if (x86IsImm32(instr->r2, &imm)) {
                    x86MirLine(ctx, mir2(m, X86MN_CMP, mir_r64(R_RAX),
                                         mir_imm(imm)));
                } else {
                    x86LoadToReg(ctx, instr->r2, R_RCX);
                    x86MirLine(ctx, mir2(m, X86MN_CMP, mir_r64(R_RAX),
                                         mir_r64(R_RCX)));
                }
            }
            int cc_t = x86CcFor(kind, is_float);
            int cc_f = x86CcInvFor(kind, is_float);
            int t_ln = hccJitBlockLocalNum(jit, ctx->fn, t);
            int f_ln = hccJitBlockLocalNum(jit, ctx->fn, f);
            int t_phi = irBlockHasPhi(t);
            int f_phi = irBlockHasPhi(f);
            if (!t_phi && !f_phi) {
                if (ctx->next_block == t) {
                    x86EmitJccLocal(ctx, cc_f, f_ln);
                } else if (ctx->next_block == f) {
                    x86EmitJccLocal(ctx, cc_t, t_ln);
                } else {
                    x86EmitJccLocal(ctx, cc_t, t_ln);
                    x86EmitBranchLocal(ctx, f_ln);
                }
            } else {
                int else_ln = hccJitFreshLocalNum(jit);
                x86EmitJccLocal(ctx, cc_f, else_ln);
                x86PhiMaterialise(ctx, ctx->cur_block, t);
                x86EmitBranchLocal(ctx, t_ln);
                x86MirLocalLabel(ctx, else_ln);
                x86PhiMaterialise(ctx, ctx->cur_block, f);
                if (ctx->next_block != f) x86EmitBranchLocal(ctx, f_ln);
            }
            break;
        }

        case IR_BR: {
            IrBlock *t = instr->extra.blocks.target_block;
            IrBlock *f = instr->extra.blocks.fallthrough_block;
            x86LoadFirstSrc(ctx, instr->dst);
            x86MirLine(ctx, mir2(m, X86MN_TEST, mir_r64(R_RAX),
                                 mir_r64(R_RAX)));
            int t_ln = hccJitBlockLocalNum(jit, ctx->fn, t);
            int f_ln = hccJitBlockLocalNum(jit, ctx->fn, f);
            int t_phi = irBlockHasPhi(t);
            int f_phi = irBlockHasPhi(f);
            if (!t_phi && !f_phi) {
                if (ctx->next_block == t) {
                    x86EmitJccLocal(ctx, X86_CC_E, f_ln);
                } else if (ctx->next_block == f) {
                    x86EmitJccLocal(ctx, X86_CC_NE, t_ln);
                } else {
                    x86EmitJccLocal(ctx, X86_CC_NE, t_ln);
                    x86EmitBranchLocal(ctx, f_ln);
                }
            } else {
                int else_ln = hccJitFreshLocalNum(jit);
                x86EmitJccLocal(ctx, X86_CC_E, else_ln);
                x86PhiMaterialise(ctx, ctx->cur_block, t);
                x86EmitBranchLocal(ctx, t_ln);
                x86MirLocalLabel(ctx, else_ln);
                x86PhiMaterialise(ctx, ctx->cur_block, f);
                if (ctx->next_block != f) x86EmitBranchLocal(ctx, f_ln);
            }
            break;
        }

        case IR_JMP: {
            IrBlock *target = instr->extra.blocks.target_block;
            x86PhiMaterialise(ctx, ctx->cur_block, target);
            if (target != ctx->next_block) {
                int ln = hccJitBlockLocalNum(jit, ctx->fn, target);
                x86EmitBranchLocal(ctx, ln);
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
                int gpi = 0;
                int ssei = 0;
                for (int e = 0; e < neb; ++e) {
                    int off = e * 8, rem = (int)t->size - off;
                    int sz = rem >= 8 ? 8 : rem;
                    if (cl[e] == SYSV_SSE) {
                        x86FpLoadMem(ctx, ssei,
                                     x86MemOp(R_RBP, -1, 0, loff + off),
                                     sz != 4);
                        ssei++;
                    } else {
                        x86LoadMem(ctx, sz, gpret[gpi],
                                   x86MemOp(R_RBP, -1, 0, loff + off));
                        gpi++;
                    }
                }
            } else if (instr->dst) {
                if (irIsFloat(instr->dst->type)) x86LoadFirstSrcFpr(ctx, instr->dst);
                else                              x86LoadFirstSrc(ctx, instr->dst);
            }
            /* The epilogue label is defined straight after the last block,
             * so a ret ending that block falls through to it. */
            if (ctx->next_block != NULL ||
                ctx->cur_block->instructions->prev->value != (void *)instr)
            {
                x86EmitBranchLocal(ctx, hccJitEpilogueLocalNum(jit, ctx->fn));
            }
            break;

        case IR_TRUNC: {
            x86LoadFirstSrc(ctx, instr->r1);
            int sz = instr->dst ? (int)instr->dst->as.var.size : 8;
            /* The result still lives in a 64-bit register, so the high
             * bits have to reflect the DESTINATION's signedness - this
             * always zero-extended, which made every signed truncation
             * come back as a large positive number. `(char)200 == -56`
             * compared -56 against 200.
             *
             * IRCG_TRUNC_SIGNED is how the IR carries that; the AOT
             * backend and the aarch64 selector both read it, and this was the
             * one place that did not. */
            int sgn = (instr->flags & IRCG_TRUNC_SIGNED) != 0;
            if (sz < 8) {
                if (sgn)
                    x86MirLine(ctx, mir2(m, X86MN_MOVSX, mir_r64(R_RAX),
                                         x86MirGp(sz, R_RAX)));
                else if (sz == 4)
                    x86MirLine(ctx, mir2(m, X86MN_MOV, mir_r32(R_RAX),
                                         mir_r32(R_RAX)));
                else
                    x86MirLine(ctx, mir2(m, X86MN_MOVZX, mir_r64(R_RAX),
                                         x86MirGp(sz, R_RAX)));
            }
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_ZEXT: {
            x86LoadFirstSrc(ctx, instr->r1);
            int src_sz = (int)irValueByteSize(instr->r1);
            if (src_sz < 8) {
                if (src_sz == 4)
                    x86MirLine(ctx, mir2(m, X86MN_MOV, mir_r32(R_RAX),
                                         mir_r32(R_RAX)));
                else
                    x86MirLine(ctx, mir2(m, X86MN_MOVZX, mir_r64(R_RAX),
                                         x86MirGp(src_sz, R_RAX)));
            }
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_SEXT: {
            x86LoadFirstSrc(ctx, instr->r1);
            int src_sz = (int)irValueByteSize(instr->r1);
            if (src_sz < 8)
                x86MirLine(ctx, mir2(m, X86MN_MOVSX, mir_r64(R_RAX),
                                     x86MirGp(src_sz, R_RAX)));
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_FPTRUNC:
            x86LoadFirstSrcFpr(ctx, instr->r1);
            x86MirLine(ctx, mir2(m, X86MN_CVTSD2SS, mir_xmm(0), mir_xmm(0)));
            x86SpillDstFpr(ctx, instr, 0);
            break;

        case IR_FPEXT:
            x86LoadFirstSrcFpr(ctx, instr->r1);
            x86MirLine(ctx, mir2(m, X86MN_CVTSS2SD, mir_xmm(0), mir_xmm(0)));
            x86SpillDstFpr(ctx, instr, 0);
            break;

        case IR_FPTOSI:
        case IR_FPTOUI:
            x86LoadFirstSrcFpr(ctx, instr->r1);
            if (instr->op == IR_FPTOUI && irValueByteSize(instr->dst) == 8) {
                x86FpToU64(ctx, x86FpDbl(instr->r1));
                x86SpillDst(ctx, instr, R_RAX);
                break;
            }
            /* Anything narrower fits the signed conversion. */
            x86MirLine(ctx, mir2(m, x86FpDbl(instr->r1) ? X86MN_CVTTSD2SI
                                                        : X86MN_CVTTSS2SI,
                                 mir_r64(R_RAX), mir_xmm(0)));
            x86SpillDst(ctx, instr, R_RAX);
            break;

        case IR_SITOFP:
        case IR_UITOFP:
            x86LoadFirstSrc(ctx, instr->r1);
            if (instr->op == IR_UITOFP && irValueByteSize(instr->r1) == 8) {
                x86U64ToFp(ctx, x86FpDbl(instr->dst));
                x86SpillDstFpr(ctx, instr, 0);
                break;
            }
            /* Anything narrower is zero-extended, so positive. */
            x86MirLine(ctx, mir2(m, x86FpDbl(instr->dst) ? X86MN_CVTSI2SD
                                                         : X86MN_CVTSI2SS,
                                 mir_xmm(0), mir_r64(R_RAX)));
            x86SpillDstFpr(ctx, instr, 0);
            break;

        case IR_PTRTOINT:
        case IR_INTTOPTR:
        case IR_BITCAST: {
            int dst_is_float = irIsFloat(instr->dst->type);
            int src_is_float = irIsFloat(instr->r1->type);
            if (dst_is_float == src_is_float) {
                if (dst_is_float) {
                    x86LoadFirstSrcFpr(ctx, instr->r1);
                    x86SpillDstFpr(ctx, instr, 0);
                } else {
                    x86LoadFirstSrc(ctx, instr->r1);
                    x86SpillDst(ctx, instr, R_RAX);
                }
            } else if (dst_is_float) {
                x86LoadFirstSrc(ctx, instr->r1);
                x86BitsToXmm(ctx, 0, R_RAX, x86FpDbl(instr->dst));
                x86SpillDstFpr(ctx, instr, 0);
            } else {
                x86LoadFirstSrcFpr(ctx, instr->r1);
                x86XmmToBits(ctx, R_RAX, 0, x86FpDbl(instr->r1));
                x86SpillDst(ctx, instr, R_RAX);
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
                x86LoadToReg(ctx, instr->r2, R_R11);
            }
            /* The callee's calling convention first, because BOTH paths
             * below need it: HolyC varargs go on the stack whichever
             * path emits the call. */
            Ast *callee = NULL;
            if (!indirect) {
                callee = (Ast *)mapGetLen(jit->cc->global_env,
                                          fname->data, fname->len);
            } else if (wrap) {
                /* What the function pointer's type says. */
                callee = wrap->as.array.callee_decl;
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
            int holyc_variadic = callee_va && astUsesHolyCVarargs(callee);
            int extern_variadic = callee_va && !holyc_variadic;

            int var_arg_start = -1;
            if (holyc_variadic && callee->params) {
                for (u64 i = 0; i < callee->params->size; ++i) {
                    Ast *p = vecGet(Ast *, callee->params, i);
                    var_arg_start++;
                    if (p && p->kind == AST_VAR_ARGS) break;
                }
                var_arg_start += 1;
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
                    x86EmitSysvCall(ctx, instr, args, fname, indirect,
                                    holyc_variadic, var_arg_start);
                    break;
                }
            }

            u64 n = args ? args->size : 0;
            u8 *is_stack = n ? calloc(n, 1) : NULL;
            s32 n_stack = x86PartitionCallArgs(is_stack, n, args,
                                               holyc_variadic, var_arg_start);
            /* Stack args sit in 8-byte lanes; SysV requires 16-byte
             * alignment at the call instruction. */
            s32 stack_bytes = n_stack * 8;
            if (stack_bytes & 15) stack_bytes += 8;
            if (stack_bytes > 0) {
                x86MirLine(ctx, mir2(m, X86MN_SUB, mir_r64(R_RSP),
                                     mir_imm(stack_bytes)));
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
                    X86Reg src = x86RegFromName(a0->loc.as.reg->data);
                    if (src != kIntArgRegs[0])
                        x86MovRegReg(ctx, kIntArgRegs[0], src);
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
                    x86LoadToFpr(ctx, a, 0 /* xmm0 */);
                    x86FpStoreMem(ctx, 0, x86MemOp(R_RSP, -1, 0, off),
                                  x86FpDbl(a));
                } else {
                    x86LoadToReg(ctx, a, R_RAX);
                    x86StoreMem(ctx, 8, R_RAX, x86MemOp(R_RSP, -1, 0, off));
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
                    x86LoadToFpr(ctx, a, float_idx++);
                    xmm_used++;
                } else {
                    x86LoadToReg(ctx, a, kIntArgRegs[int_idx++]);
                }
            }

            /* SysV variadic ABI: %al = number of XMM regs used. */
            if (extern_variadic || holyc_variadic) {
                x86MirLine(ctx, mir2(m, X86MN_MOV, mir_r8(R_RAX),
                                     mir_imm(xmm_used)));
            }

            if (indirect) {
                x86MirLine(ctx, mir1(m, X86MN_CALL, mir_r64(R_R11)));
            } else {
                x86EmitCallSym(ctx, asmNormaliseFunctionName(jit->cc, fname));
            }
            if (stack_bytes > 0) {
                x86MirLine(ctx, mir2(m, X86MN_ADD, mir_r64(R_RSP),
                                     mir_imm(stack_bytes)));
            }
            if (instr->dst && instr->dst->type != IR_TYPE_VOID) {
                if (irIsFloat(instr->dst->type)) x86SpillDstFpr(ctx, instr, 0);
                else                              x86SpillDst   (ctx, instr, R_RAX);
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
            /* The blob's bytes bypass the stream, so everything built
             * so far must be committed first. */
            x86MirFlush(ctx);
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
                loggerPanic("jit-x86_64: failed to assemble inline asm\n");
            }
            break;

        case IR_VA_START: {
            /* `__builtin_va_region()` hands back the address of the
             * va_list record x86EmitVaSaveArea built. <stdarg.h>'s
             * `va_start` copies from it, so the record is never walked
             * and stays valid for a second `va_start`. */
            x86MirLine(ctx, mir2(m, X86MN_LEA, mir_r64(R_RAX),
                                 mir_mem(mir_r64(R_RBP),
                                         x86_64VaListOff(ctx->fn->stack_space))));
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_FRAME_ADDR:
            /* rbp, which the prologue establishes for every function
             * that keeps a frame - the omit_frame test below is what
             * stops this one being elided. */
            x86MirLine(ctx, mir2(m, X86MN_MOV, mir_r64(R_RAX),
                                 mir_r64(R_RBP)));
            x86SpillDst(ctx, instr, R_RAX);
            break;

        case IR_SELECT:
        case IR_SWITCH:
        case IR_VA_ARG:
        case IR_VA_END:
            loggerPanic("jit-x86_64: op %d not yet implemented\n", instr->op);
        default:
            loggerPanic("jit-x86_64: unknown op %d\n", instr->op);
    }
}

/* ---------------- prologue / epilogue ---------------- */

static int x86IsXmmName(const char *n) {
    return n && n[0] == 'x' && n[1] == 'm' && n[2] == 'm';
}

/* Save/restore the callee-saved registers the source pinned or the
 * promoter handed out, in a rounded-up area just below the locals -
 * the same placement as x86_64SavePinnedRegs/x86_64RestorePinnedRegs
 * in the text backend. The JIT never did this at all, so every call
 * from jitted code clobbered the caller's promoted values - which is
 * why the whole suite's `correct` counters came back as garbage. */
static void x86EmitSaveRestore(X86Ctx *ctx, int store) {
    Vec *saved = irCgPinnedCalleeSaved(ctx->fn);
    if (saved->size) {
        uint32_t bytes = ((uint32_t)saved->size * 8 + 15u) & ~15u;
        if (store)
            E2(ctx, X86MN_SUB, mir_r64(R_RSP), mir_imm((s64)bytes));
        for (u64 i = 0; i < saved->size; ++i) {
            AoStr *r = (AoStr *)saved->entries[i];
            int32_t off = (int32_t)(i * 8);
            AsmOperand mem = mir_mem(mir_r64(R_RSP), off);
            if (x86IsXmmName(r->data)) {
                int xmm = atoi(r->data + 3);
                if (store) x86FpStoreMem(ctx, xmm, mem, 1);
                else       x86FpLoadMem(ctx, xmm, mem, 1);
            } else {
                X86Reg reg = x86RegFromName(r->data);
                if (store) x86StoreMem(ctx, 8, reg, mem);
                else       x86LoadMem(ctx, 8, reg, mem);
            }
        }
        /* No addq on restore: the epilogue reloads rsp from rbp anyway. */
    }
    vecRelease(saved);
}

static void x86EmitPrologue(X86Ctx *ctx) {
    if (ctx->omit_frame) return;
    MirFn *m = ctx->mir;
    x86MirLine(ctx, mir1(m, X86MN_PUSH, mir_r64(R_RBP)));
    x86MirLine(ctx, mir2(m, X86MN_MOV, mir_r64(R_RBP), mir_r64(R_RSP)));
    /* A function that reads a variadic list carries the register save
     * area and its va_list record UNDER the locals, so it reserves
     * that much again. Every local's offset is unchanged - the extra
     * is all further from rbp than they are. */
    uint32_t aligned = x86_64VaLocalsBase((u32)ctx->fn->stack_space);
    if (ctx->uses_va_start) aligned += x86_64VaExtraBytes();
    if (aligned > 0) {
        x86MirLine(ctx, mir2(m, X86MN_SUB, mir_r64(R_RSP),
                             mir_imm((s64)aligned)));
    }
    /* Pinned saves AFTER reserving locals so rbp-relative offsets for
     * slots stay valid throughout the body. */
    x86EmitSaveRestore(ctx, 1);
}

static void x86EmitEpilogue(X86Ctx *ctx) {
    MirFn *m = ctx->mir;
    if (!ctx->omit_frame) {
        x86EmitSaveRestore(ctx, 0);   /* before rsp moves */
        /* leave = movq %rbp, %rsp ; popq %rbp. */
        x86MirLine(ctx, mir2(m, X86MN_MOV, mir_r64(R_RSP), mir_r64(R_RBP)));
        x86MirLine(ctx, mir1(m, X86MN_POP, mir_r64(R_RBP)));
    }
    x86MirLine(ctx, mir0(m, X86MN_RET));
}

/* ---------------- per-function compilation ---------------- */

/* Emit one ALREADY-LOWERED-AND-PREPPED function. `text_out` selects
 * the sink: NULL encodes bytes into jit->enc (the JIT / object
 * writer), a buffer gets AT&T GNU-as text appended (the AOT
 * path, sharing this selector). */
static int x86EmitFunctionBody(HccJit *jit, Ast *ast, IrFunction *fn,
                               AoStr *text_out) {
    int is_variadic = (ast->type && ast->type->has_var_args) || ast->has_var_args;
    /* A by-value struct param is unpacked into an rbp-relative slot by
     * x86EmitSysvParamPrologue, so the frame must exist. */
    int has_struct_param = 0;
    if (ast->params) {
        for (u64 pi = 0; pi < ast->params->size; ++pi) {
            Ast *p = vecGet(Ast *, ast->params, pi);
            if (x86IsByvalStruct(p->type)) { has_struct_param = 1; break; }
        }
    }
    /* Reading a variadic list needs a frame: the save area and the
     * va_list record are both rbp-relative. */
    int uses_va_start = x86_64FnUsesVaStart(fn);

    Vec *pinned_check = irCgPinnedCalleeSaved(fn);
    int has_pinned = pinned_check->size > 0;
    vecRelease(pinned_check);
    int omit_frame = (fn->stack_space == 0) && !irFnHasCalls(fn) &&
                     (ast->loff == 0) && !is_variadic && !has_struct_param &&
                     !uses_va_start && !has_pinned &&
                     !irFnHasOp(fn, IR_FRAME_ADDR);

    X86Ctx ctx = {0};
    ctx.jit = jit;
    ctx.fn = fn;
    ctx.ast = ast;
    ctx.omit_frame = omit_frame;
    ctx.uses_va_start = uses_va_start;
    ctx.text_out = text_out;

    /* The function's public label is the stream's first line; each
     * sink materialises it (asm_define_label / `name:`).
     * asmNormaliseFunctionName strips/adds underscores per platform;
     * the resolver uses the same name. */
    char *fname = asmNormaliseFunctionName(jit->cc, ast->fname);
    ctx.mir = mir_fn_new(fname, TASM_ARCH_X86_64);

    x86EmitPrologue(&ctx);
    /* Before the parameter prologue: that one reads the incoming
     * argument registers, and this one has to capture them all
     * untouched. */
    if (uses_va_start) x86EmitVaSaveArea(&ctx);
    x86EmitSysvParamPrologue(&ctx);

    Set *referenced = irCgComputeReferencedBlocks(fn);

    listForEach(fn->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        ctx.cur_block = block;
        ctx.next_block = (it->next != fn->blocks)
                       ? (IrBlock *)it->next->value : NULL;
        if (setHas(referenced, (void *)(u64)block->id)) {
            int ln = hccJitBlockLocalNum(jit, fn, block);
            x86MirLocalLabel(&ctx, ln);
        }
        listForEach(block->instructions) {
            IrInstr *in = (IrInstr *)it->value;
            ctx.cur_line = in->line;
            x86EmitInstr(&ctx, in);
        }
    }
    setRelease(referenced);

    /* Epilogue label + bytes. */
    x86MirLocalLabel(&ctx, hccJitEpilogueLocalNum(jit, fn));
    x86EmitEpilogue(&ctx);
    x86MirFlush(&ctx);

    if (x86MirDump()) mir_fn_print(ctx.mir, stderr);
    mir_fn_free(ctx.mir);

    return 0;
}

static int x86CompileFunction(HccJit *jit, Ast *ast, IrCtx *ir_ctx) {
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

    return x86EmitFunctionBody(jit, ast, fn, NULL);
}

/* ---------------- public API ---------------- */

static int x86TargetOk(enum CliTarget target) {
    return target == TARGET_X86_64_APPLE_DARWIN ||
           target == TARGET_X86_64_UNKNOWN_LINUX_GNU;
}

static const HccJitBackend x86_64_jit_backend = {
    .name = "x86_64",
    .target_ok = x86TargetOk,
    .init_reg_pool = x86_64InitRegPool,
    .compile_function = x86CompileFunction,
};

/* AOT text generation through the SAME selector as the JIT: the driver
 * (asm.c) has already lowered, optimised and prepped ctx->fn; this
 * runs x86EmitFunctionBody with the text sink so every instruction is
 * built as MIR and printed by the machine layer's AT&T
 * formatter. One HccJit context for the whole file keeps the .Ljag
 * label numbers unique across functions. */
void x86_64MirGenerateFunction(IrCgCtx *cgctx, Ast *ast) {
    static HccJit *aot = NULL;
    if (!aot) {
        aot = hccJitNewEx(cgctx->cc, &x86_64_jit_backend, 0);
        if (!aot) {
            loggerPanic("hcc: cannot initialise x86_64 MIR backend\n");
        }
        /* Everything is symbolic for an external assembler or object
         * writer; the selector keys its addressing decisions off this,
         * never off the sink. */
        aot->addr_mode = HCC_ADDR_RELOCATED;
    }
    /* .p2align 4 = 16-byte aligned function entries (x86_64 convention
     * for the I-cache fetch boundary). The syntax directive repeats
     * per function - it is idempotent, and everything between
     * functions is syntax-neutral directives. */
    /* A `static` function has internal linkage, so it must NOT be
     * exported: two translation units are entitled to define the same
     * name privately, and two global symbols collide at link time.
     * Omitting `.globl` is what makes the symbol local, on both Mach-O
     * and ELF. */
    int is_local = (ast->flags & AST_FLAG_STATIC_FN) != 0;
    aoStrCatFmt(cgctx->buf, ".text\n\t.p2align 4\n");
    if (!is_local) {
        char *__sym = asmNormaliseFunctionName(cgctx->cc, ast->fname);
        asmSpangleNote(cgctx->cc, cgctx->buf, __sym);
        aoStrCatFmt(cgctx->buf, "\t.globl %s\n", __sym);
    }
    x86EmitFunctionBody(aot, ast, cgctx->fn, cgctx->buf);
    aoStrPutChar(cgctx->buf, '\n');
}

HccJit *x86_64JitCompile(Cctrl *cc) {
    return hccJitCompile(cc, &x86_64_jit_backend);
}

const HccJitBackend *x86_64JitBackend(void) {
    return &x86_64_jit_backend;
}

/* ================================================================
 * Target description + file-format emission (formerly x86_64.c):
 * the register pool and clobber model the selector allocates
 * against, and everything that surrounds function bodies in an
 * assembly file - data sections, `asm {}` blocks, globals.
 * ================================================================
 *
 x86_64 target description and file-format emission.
 *
 * SysV AMD64 ABI for both Linux and macOS (only the symbol naming and
 * PIC syntax differ - same call convention, same register set, same
 * struct passing).
 *
 * Function BODY generation runs the MIR selector above: one selector
 * shared by the JIT, the object writer and the AOT text path (see
 * x86_64MirGenerateFunction). What remains here is what the selector
 * consumes and what surrounds the bodies in an assembly file: the
 * register pool + clobber model, the SysV variadic frame constants,
 * and the data-section / asm-block / global emitters. */

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
/* Permanent, NOT arena-backed: the pool this fills is built once and
   read on every later compile, and the arenas are reclaimed between
   parses. See aoStrDupRawPermanent. */
static Vec *x86_64MakeAoStrVec(const char *const *src, int n) {
    Vec *v = vecNew(&vec_aostr_type);
    for (int i = 0; i < n; ++i) {
        vecPush(v, aoStrDupRaw((char *)src[i], strlen(src[i])));
    }
    return v;
}

/* Which of rax/rcx/rdx/xmm0/xmm1 does emitting `I` actually write, by
 * the point `at`? r1 is materialised into rax/xmm0, r2 into rcx, and
 * the result spills out of rax/xmm0.
 *
 * Conservative by default - only the audited cases answer "not
 * clobbered", so an unconsidered opcode keeps the old behaviour. */
static int x86_64OpClobbers(IrInstr *I, AoStr *reg, IrClobberPoint at) {
    const char *r = reg->data;
    int is_scratch0 = !strcmp(r, "rax") || !strcmp(r, "xmm0");
    /* Division sign-extends into rdx (cqto) *before* the divisor is
     * loaded, so rdx is unsafe for r2 - and rdx is SysV arg #3. */
    int is_divrem = I->op == IR_IDIV || I->op == IR_UDIV ||
                    I->op == IR_IREM || I->op == IR_UREM;

    switch (at) {
        case IR_CLOBBER_BEFORE_DST:
            /* The deref stores read their dst as an ADDRESS, and it is
             * the first thing put in place - `x86_64LoadToReg(dst,
             * "rcx")` runs before the value is touched - so nothing has
             * been written yet. */
            return 0;

        case IR_CLOBBER_BEFORE_R1:
            /* For the deref stores r1 is the VALUE and comes last,
             * after the address and index. Left conservative here: the
             * AArch64 backend narrows this to "only when something
             * actually has to be materialised", and doing the same for
             * SysV means working through rcx/rdx and the immediate
             * fast paths, which is its own piece of work. */
            if (I->op == IR_STORE_DEREF || I->op == IR_RMW_DEREF) return 1;
            return 0;

        case IR_CLOBBER_BEFORE_R2:
            if (I->op == IR_STORE_DEREF || I->op == IR_RMW_DEREF) return 1;
            if (is_divrem) return 1;
            /* r1 has landed in rax/xmm0; rcx and rdx are untouched. */
            return is_scratch0;

        case IR_CLOBBER_AFTER:
        default:
            /* The IR_STORE path stores a reg-resident r1 straight out
             * of its own register instead of bouncing through
             * rax/xmm0, so nothing is clobbered - which is what lets
             * consecutive param spills all forward. A pinned
             * destination names its own register and does clobber. */
            if (I->op == IR_STORE) {
                if (I->dst && I->dst->pinned_reg) return 1;
                int src_in_reg = I->r1 && I->r1->loc.kind == IR_LOC_REG &&
                                 I->r1->loc.as.reg;
                return src_in_reg ? 0 : is_scratch0;
            }
            return 1;
    }
}

void x86_64InitRegPool(void) {
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

    static const char *kScratchRegs[] = { "rax", "rcx", "rdx", "xmm0", "xmm1" };
    int n_int     = (int)(sizeof(kIntRegs64)   / sizeof(kIntRegs64[0]));
    int n_float   = (int)(sizeof(kXmmRegs)     / sizeof(kXmmRegs[0]));
    int n_scratch = (int)(sizeof(kScratchRegs) / sizeof(kScratchRegs[0]));
    pool.int_arg_regs     = x86_64MakeAoStrVec(kIntRegs64,   n_int);
    pool.float_arg_regs   = x86_64MakeAoStrVec(kXmmRegs,     n_float);
    pool.int_return_reg   = aoStrDupRaw((char *)"rax", 3);
    pool.int_result_reg   = pool.int_return_reg;
    pool.float_return_reg = aoStrDupRaw((char *)"xmm0", 4);
    pool.float_result_reg = pool.float_return_reg;
    pool.sret_reg         = NULL; /* SysV: sret ptr is the first int arg */
    pool.scratch_regs     = x86_64MakeAoStrVec(kScratchRegs, n_scratch);
    /* SysV callee-saved GPRs. rbp is the frame pointer and rsp the
     * stack, so neither is available; rbx and r12-r15 are. No XMM
     * register is callee-saved on SysV, so a promoted float has
     * nowhere to live and irOptPromoteToRegs simply never picks one.
     *
     * rax/rcx/rdx are the backend's scratch pair and must stay out:
     * every emitter materialises operands through them. */
    static const char *kCalleeSaved[] = { "rbx", "r12", "r13", "r14", "r15" };
    pool.callee_saved_regs = x86_64MakeAoStrVec(kCalleeSaved,
            (int)(sizeof(kCalleeSaved) / sizeof(kCalleeSaved[0])));
    /* Caller-saved and free: costs no save/restore pair, so a value
     * that never crosses a call can have one for nothing. That is the
     * whole point - a leaf function stops paying six instructions (a
     * frame, two saves, two restores) to keep two values in registers.
     *
     * %r10 is the only one, and it took work to free. Ruling out the
     * rest of the caller-saved set:
     *
     *   rax, rcx, rdx   the emitters' scratch - every operand is
     *                   materialised through them
     *   rdi, rsi, r8, r9  argument registers. A promoted value is
     *                   allowed to be an ARGUMENT to a call (it dies
     *                   there, so it is not a "crosser"), and the
     *                   argument-loading sequence writes these in
     *                   order - a later argument sitting in an earlier
     *                   one's target register would read garbage.
     *   r11             kept back deliberately. Two places need a
     *                   register that provably aliases nothing the
     *                   allocator handed out: walking a by-value
     *                   struct during argument setup, and the
     *                   accumulator/rhs pair in the JIT's RMW_DEREF.
     *
     * What made %r10 free was moving the indirect-call target out of a
     * register entirely - it is parked in the call's own stack area and
     * called with `callq *N(%rsp)` - and moving the struct-marshalling
     * uses onto %r11.
     *
     * Nothing writes %r10 between a value's definition and a call that
     * reads it as an argument, which is the property that matters.
     *
     * rdi/rsi/r8/r9 join it, and the two hazards named above are what
     * the shared allocator now reasons about explicitly (see
     * irRegIsAbiArg / irRegHoldsReadParam / irCollectCallArgValues in
     * ir-regalloc.c): a register holding a parameter is held until its
     * last read, and a value handed to a call as an argument may not
     * take one - no `call_arg_reg` hook here yet. They are listed AFTER %r10
     * so the unencumbered register is spent first.
     *
     * %rdx and %rcx are argument registers too and stay out regardless:
     * unlike the other four they double as the emitters' scratch -
     * every deref reads its base through %rcx and its index through
     * %rdx - so no amount of allocator reasoning makes them safe. */
    static const char *kLeafFree[] = { "r10", "rdi", "rsi", "r8", "r9" };
    pool.leaf_free_regs = x86_64MakeAoStrVec(kLeafFree,
            (int)(sizeof(kLeafFree) / sizeof(kLeafFree[0])));
    pool.op_clobbers      = x86_64OpClobbers;

    irRegPoolSet(&pool);
    initialised = 1;
}

const char *x86_64GetSizedMov(u32 size) {
    switch (size) {
        case 1:  return "movb";
        case 2:  return "movw";
        case 4:  return "movl";
        default: return "movq";
    }
}


/* ---------------- System V struct-by-value passing ----------------
 *
 * Mirrors the aarch64 AAPCS path for the x86_64 SysV ABI. Each by-value
 * aggregate is classified into eightbytes (INTEGER -> GP reg, SSE -> XMM
 * reg); >16-byte aggregates are MEMORY class and passed on the stack by
 * value. Struct *returns* keep using the legacy hidden-out-pointer (which
 * the IR passes as args[0] -> %rdi) + memcpy. Register-only overflow and
 * scalar stack spilling beyond 6 GP / 8 SSE panic, matching the support
 * window of the other backends' struct paths. */

static int x86_64IsByvalStruct(AstType *t) {
    return t && (t->kind == AST_TYPE_CLASS || t->kind == AST_TYPE_UNION) &&
           !t->is_intrinsic;
}

/* SysV classification of a by-value aggregate with the per-class eightbyte
 * counts pre-summed, so the caller (sizing + emit) and the callee prologue
 * share one classification and one reg/stack decision. */
typedef struct {
    SysvClass cls[2];  /* per-eightbyte class (valid when !memory) */
    int neb;           /* number of eightbytes */
    int gp;            /* INTEGER eightbytes -> GP regs  (0 if memory) */
    int sse;           /* SSE eightbytes -> XMM regs     (0 if memory) */
    int memory;        /* 1 -> >16B / otherwise passed on the stack */
} X86SysvAgg;

static void x86_64ClassifyAgg(AstType *t, X86SysvAgg *a) {
    a->gp = a->sse = a->neb = 0;
    a->memory = astSysvClassify(t, a->cls, &a->neb);
    if (!a->memory) {
        for (int e = 0; e < a->neb; ++e) {
            if (a->cls[e] == SYSV_INTEGER) a->gp++;
            else a->sse++;
        }
    }
}

/* True when the whole aggregate still fits in the remaining argument
 * registers; otherwise it is passed on the stack by value. */
static int x86_64AggInRegs(const X86SysvAgg *a, int ngp, int nsse) {
    return !a->memory && ngp + a->gp <= 6 && nsse + a->sse <= 8;
}

/* Callee entry: unpack by-value struct params (arrived in GP/XMM regs, or
 * on the stack for MEMORY class) into their frame slots. */
/* ---- SysV variadic support -----------------------------------------
 *
 * Unlike Apple AArch64, which puts every variadic argument on the
 * stack, SysV passes the first six integer/pointer ones in rdi..r9 and
 * the first eight floating point ones in xmm0..7. A callee that wants
 * to walk that list therefore has to write those registers somewhere
 * addressable first - the REGISTER SAVE AREA - because by the time
 * `va_arg` runs they are long gone.
 *
 * The frame layout itself lives in x86_64.h, shared with the JIT. */
int x86_64FnUsesVaStart(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_VA_START) return 1;
        }
    }
    return 0;
}

/* Where the NAMED parameters stop, in each of the three places the
 * variadic list can continue from.
 *
 * This mirrors x86_64EmitSysvParamPrologue's walk exactly, because it
 * is answering the same question from the other side: where that loop
 * stops consuming is where the variadic arguments begin. The aarch64
 * backend pairs a64VaRegionOffset with its prologue for the same
 * reason. Any disagreement reads arguments that are not there.
 *
 * The JIT's parameter prologue mirrors this file's, so it shares this
 * rather than keeping a third copy of the same accounting. */
void x86_64VaNamedCounts(Ast *ast, int *out_ngp, int *out_nsse,
                         int *out_overflow)
{
    int ngp = 0, nsse = 0, incoming_off = 0;

    AstType *rt = ast->type ? ast->type->rettype : NULL;
    if (x86_64IsByvalStruct(rt) && rt->size > 16) ngp = 1; /* hidden ret ptr */

    if (ast->params) {
        for (u64 i = 0; i < ast->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast->params, i);
            if (p->kind == AST_VAR_ARGS) break;
            AstType *t = p->type;

            if (!x86_64IsByvalStruct(t)) {
                int is_float = (t->kind == AST_TYPE_FLOAT);
                if (is_float ? (nsse < 8) : (ngp < 6)) {
                    if (is_float) nsse++; else ngp++;
                } else {
                    incoming_off += 8;
                }
                continue;
            }

            X86SysvAgg ag; x86_64ClassifyAgg(t, &ag);
            if (!x86_64AggInRegs(&ag, ngp, nsse)) {
                incoming_off += (t->size + 7) & ~7;
                continue;
            }
            for (int e = 0; e < ag.neb; ++e) {
                if (ag.cls[e] == SYSV_SSE) nsse++; else ngp++;
            }
        }
    }

    *out_ngp = ngp;
    *out_nsse = nsse;
    *out_overflow = incoming_off;
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

void x86_64InitialiseEmptyGlobal(Cctrl *cc,
                                 AoStr *buf,
                                 Ast *global,
                                 int zerofill)
{
    AoStr *label = global->is_static ? global->glabel : global->gname;
    label = asmNormaliseGlobalLabel(cc, label);
    int size = global->type->size;

    if (zerofill && (cc->target == TARGET_AARCH64_APPLE_DARWIN ||
                     cc->target == TARGET_X86_64_APPLE_DARWIN))
    {
        asmSpangleNote(cc, buf, label->data);
        aoStrCatFmt(buf, ".globl %S\n\t", label);
        aoStrCatFmt(buf, ".zerofill __DATA,__common,%S,%i,%i\n\t",
                    label, size, (int)log2((double)size));
    } else {
        asmSpangleNote(cc, buf, label->data);
        aoStrCatFmt(buf, ".globl %S\n\t.comm %S, %i, %u\n\t",
                    label, label, size,
                    roundUpToNextPowerOf2((unsigned long)size));
    }
}

/* Returns the number of bytes emitted, so a partially-written
 * aggregate can be padded out to its declared size. C zero-initialises
 * every member an initialiser does not mention; emitting only what was
 * written leaves the rest of the object as whatever follows it in the
 * section - for `MapType` that meant reading function pointers out of
 * the text segment and calling them. */
static int x86_64DataInternal(Cctrl *cc, AoStr *buf, Ast *data) {
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
        aoStrCatPrintf(buf, ".quad %s\n\t", fn);
        return 8;
    }
    if (data->kind == AST_ASM_FUNCDEF || data->kind == AST_ASM_FUNC_BIND) {
        aoStrCatFmt(buf, ".quad %S\n\t", data->asmfname);
        return 8;
    }
    if (data->kind == AST_GVAR) {
        AoStr *g = asmNormaliseGlobalLabel(cc,
                data->is_static ? data->glabel : data->gname);
        aoStrCatFmt(buf, ".quad %S\n\t", g);
        return 8;
    }
    if (data->kind == AST_STRING) {
        aoStrCatPrintf(buf, ".quad %s\n\t", data->slabel->data);
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
            wrote += x86_64DataInternal(cc, buf, slots[si].value);
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
        if (data->type->size == 4) {
            aoStrCatPrintf(buf, ".long 0x%lX #%.9f\n\t",
                           (unsigned long)(u32)ieee754_32((f32)data->f64),
                           data->f64);
            return 4;
        } else {
            aoStrCatPrintf(buf, ".quad 0x%lX #%.9f\n\t",
                           ieee754_64(data->f64), data->f64);
        }
        return 8;
    }
    switch (data->type->size) {
        case 1: aoStrCatPrintf(buf, ".byte %d\n\t", data->i64); return 1;
        case 2: aoStrCatPrintf(buf, ".short %d\n\t", data->i64); return 2;
        case 4: aoStrCatPrintf(buf, ".long %d\n\t", data->i64); return 4;
        case 8: aoStrCatPrintf(buf, ".quad %d\n\t", data->i64); return 8;
        default:
            loggerPanic("Cannot create size information for: %s\n",
                        astToString(data));
    }
    return 0;
}

void x86_64GlobalVar(Cctrl *cc, Set *seen_globals, AoStr *buf, Ast *ast) {
    Ast *declvar = ast->declvar;
    Ast *declinit = ast->declinit;
    AoStr *varname = declvar->gname;
    AoStr *label = declvar->is_static ? declvar->glabel : declvar->gname;
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

    /* `&thing` too - see the note in aarch64GlobalVar. */
    if (declinit &&
        (declinit->kind == AST_ARRAY_INIT ||
         declinit->kind == AST_LITERAL ||
         declinit->kind == AST_STRING ||
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
                aoStrCatFmt(buf,
                            "%S:\n\t.asciz \"%S\"\n\t",
                            label, declinit->sval);
            }
            return;
        }
        if (!declvar->is_static) {
            asmSpangleNote(cc, buf, label->data);
            aoStrCatFmt(buf, ".globl %S\n", label);
        }
        aoStrCatPrintf(buf, ".data\n");
        if (declinit->kind == AST_ARRAY_INIT) {
            Ast *head = (Ast *)declinit->arrayinit->next->value;
            if (head->kind == AST_STRING) {
                aoStrCatPrintf(buf, ".align 4\n");
            }
        }
        /* An aggregate can hold a pointer, so the object needs pointer
         * alignment - the linker warns about an under-aligned atom
         * otherwise. */
        aoStrCatPrintf(buf, ".p2align 3\n");
        aoStrCatFmt(buf, "%S:\n\t", label);
        int wrote;
        if (declinit->kind == AST_ARRAY_INIT) {
            wrote = 0;
            listForEach(declinit->arrayinit) {
                wrote += x86_64DataInternal(cc, buf, (Ast *)it->value);
            }
        } else {
            wrote = x86_64DataInternal(cc, buf, declinit);
        }
        /* Members the initialiser did not mention are zero, not
         * whatever the section happens to hold next. */
        if (declvar->type && declvar->type->size > wrote) {
            aoStrCatPrintf(buf, ".space %d\n\t", declvar->type->size - wrote);
        }
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
                /* Only the unit holding the entry point DEFINES these;
                 * everywhere else they stay undefined so the linker binds
                 * them to that single definition. Two definitions collided
                 * and made linking any two HolyC objects impossible. */
                if (!cctrlUnitHasEntryPoint(cc)) return;
                x86_64InitialiseEmptyGlobal(cc, buf, declvar, 1);
            } else {
                x86_64InitialiseEmptyGlobal(cc, buf, declvar, 1);
            }
        }
    }
}

/* `asm {}` function blocks: assemble each function chunk with libtasm
 * and emit the encoded bytes under the function's label. Per-function
 * encoding scopes `@@N` local labels the TempleOS way. */
void x86_64PasteAsmBlocks(Cctrl *cc, AoStr *buf) {
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
            asmEmitBlockBytes(cc, buf, asm_func->body->asm_stmt, 0);
        }
    }
}
