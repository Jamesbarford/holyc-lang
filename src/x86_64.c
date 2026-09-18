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
 *   -S / AOT text  the lines are printed in Intel-dialect GNU-as text
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
#include <stdio.h>
#include <strings.h>
#include <assert.h>
#include <math.h>

#include "x86_64.h"
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
#include "machine/enc_x86_64.h"
#include "machine/mir.h"
#include "util.h"

/* ---------------- register pool ---------------- */

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
    if (initialised) return;

    static const char *kScratchRegs[] = { "rax", "rcx", "rdx", "xmm0", "xmm1" };
    int n_int     = (int)(sizeof(kIntRegs64)   / sizeof(kIntRegs64[0]));
    int n_float   = (int)(sizeof(kXmmRegs)     / sizeof(kXmmRegs[0]));
    int n_scratch = (int)(sizeof(kScratchRegs) / sizeof(kScratchRegs[0]));
    pool.int_arg_regs     = x86_64MakeAoStrVec(kIntRegs64,   n_int);
    pool.float_arg_regs   = x86_64MakeAoStrVec(kXmmRegs,     n_float);
    pool.int_return_reg   = aoStrDupRaw((char *)"rax", 3);
    pool.float_return_reg = aoStrDupRaw((char *)"xmm0", 4);
    pool.sret_reg         = NULL; /* SysV: sret ptr is the first int arg */
    pool.scratch_regs     = x86_64MakeAoStrVec(kScratchRegs, n_scratch);
    pool.callee_saved_regs = NULL;  /* x86 saves every pinned reg already */
    pool.leaf_free_regs    = NULL;  /* not yet surveyed for this ABI */
    pool.op_clobbers      = x86_64OpClobbers;

    irRegPoolSet(&pool);
    initialised = 1;
}

/* Map a 64-bit GP register name to its narrower variant. The
 * heterogeneous register-naming scheme on x86 makes this a lookup
 * table rather than a name-mangling rule. */

/* Local labels in `-S` output. Distinct from astMakeLabel's `.L<n>`
 * (strings and static globals) so the two counters cannot collide. */
#define X86_LOCAL_PREFIX ".LBB"

/* SysV integer arg registers, ordered as arg 0..5. */
static const int kIntArgRegs[] = { R_RDI, R_RSI, R_RDX, R_RCX, R_R8, R_R9 };

/* ---------------- per-function selection context ---------------- */

typedef struct X86Ctx {
    HccJit *jit;
    Cctrl *cc;
    IrFunction *fn;
    Ast *ast;
    IrBlock *cur_block;
    IrBlock *next_block;
    int omit_frame;
    /* Vec<AoStr *>: registers the function's pinned locals and params
     * occupy. The prologue saves them, the epilogue restores them. */
    Vec *pinned;

    MirFn *mir;
    int flushed;           /* lines [0, flushed) are already sunk */
    int src_line;          /* stamped onto every emitted line */
    /* The text sink, or NULL when the lines are encoded. */
    AoStr *text;
} X86Ctx;

/* ---------------- operand constructors ---------------- */

static AsmOperand r64(int n) { return mir_r64(n); }
static AsmOperand r32(int n) { return mir_r32(n); }
static AsmOperand r8(int n)  { return mir_r8(n); }
static AsmOperand xmm(int n) { return mir_xmm(n); }
/* The integer register view for a `size`-byte access. */
static AsmOperand rG(int size, int n) {
    switch (size) {
        case 1: return mir_r8(n);
        case 2: return mir_r16(n);
        case 4: return mir_r32(n);
        default: return mir_r64(n);
    }
}
/* `[base + idx*scale + disp]`; idx < 0 for none. */
static AsmOperand mem(int base, int idx, int scale, s64 disp) {
    if (idx < 0) return mir_mem(mir_r64(base), disp);
    return mir_mem_sib(mir_r64(base), mir_r64(idx), scale ? scale : 1, disp);
}
static AsmOperand mFrame(s64 loff) { return mir_mem(mir_r64(R_RBP), loff); }
static AsmOperand mSp(s64 off)     { return mir_mem(mir_r64(R_RSP), off); }

/* ---------------- emission ----------------
 *
 * Every line carries the source line of the IR instruction being
 * lowered, which the byte sink turns into the pc -> line table. */

static AsmLine *x86Stamp(X86Ctx *ctx, AsmLine *ln) {
    ln->line = ctx->src_line;
    return ln;
}
static AsmLine *E0(X86Ctx *c, int mn) { return x86Stamp(c, mir0(c->mir, mn)); }
static AsmLine *E1(X86Ctx *c, int mn, AsmOperand a) {
    return x86Stamp(c, mir1(c->mir, mn, a));
}
static AsmLine *E2(X86Ctx *c, int mn, AsmOperand a, AsmOperand b) {
    return x86Stamp(c, mir2(c->mir, mn, a, b));
}
static void EJMP(X86Ctx *c, int local_num) {
    E1(c, X86MN_JMP, mir_label_local(local_num));
}
/* Jcc by condition code (X86_CC_*): the mnemonic enum is alphabetical,
 * so a table maps the code to its id. */
static const int kJccMn[16] = {
    X86MN_JO, X86MN_JNO, X86MN_JB,  X86MN_JAE, X86MN_JE, X86MN_JNE,
    X86MN_JBE, X86MN_JA, X86MN_JS,  X86MN_JNS, X86MN_JP, X86MN_JNP,
    X86MN_JL,  X86MN_JGE, X86MN_JLE, X86MN_JG,
};
static void EJCC(X86Ctx *c, int cc, int local_num) {
    E1(c, kJccMn[cc & 15], mir_label_local(local_num));
}
static void ELABEL(X86Ctx *c, int local_num) {
    mir_local_label(c->mir, local_num);
}

/* ---------------- sinks ---------------- */

/* Sink lines [flushed, n). `peephole` runs the machine-layer cleanup
 * first; it is only safe at the END of a function, because it drops
 * local labels nothing references and a branch emitted after the
 * flush may still be on its way to one. */
static int x86Flush(X86Ctx *ctx, int peephole) {
    MirFn *mf = ctx->mir;
    if (peephole) mir_fn_peephole_x86_64(mf, ctx->flushed);
    AsmBlock *b = mf->block;
    AsmEnc *enc = &ctx->jit->enc;
    for (int i = ctx->flushed; i < b->n_lines; ++i) {
        AsmLine *ln = &b->lines[i];
        if (ctx->text) {
            char buf[512];
            asm_line_format_gas_x86(ln, X86_LOCAL_PREFIX, buf, sizeof(buf));
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
                x86_64_encode_instr(enc, ln);
                break;
            default:
                break;
        }
    }
    ctx->flushed = b->n_lines;
    return ctx->text ? 0 : enc->errors;
}

/* ---------------- small helpers ---------------- */

static int x86IsImm32(IrValue *v, s64 *out) {
    if (!v || v->kind != IR_VAL_CONST_INT) return 0;
    s64 i = v->as._i64;
    if (i < INT32_MIN || i > INT32_MAX) return 0;
    *out = i;
    return 1;
}

/* Parse a register name like "rax", "r8", "xmm0" into its hardware
 * number. */
static int x86RegFromName(const char *name) {
    static const struct { const char *n; int r; } tab[] = {
        {"rax", R_RAX}, {"rcx", R_RCX}, {"rdx", R_RDX}, {"rbx", R_RBX},
        {"rsp", R_RSP}, {"rbp", R_RBP}, {"rsi", R_RSI}, {"rdi", R_RDI},
        {"r8",  R_R8},  {"r9",  R_R9},  {"r10", R_R10}, {"r11", R_R11},
        {"r12", R_R12}, {"r13", R_R13}, {"r14", R_R14}, {"r15", R_R15},
    };
    if (!name || !*name) return R_RAX;
    if (!strncasecmp(name, "xmm", 3)) {
        int n = atoi(name + 3);
        if (n < 0 || n > 15) return 0;
        return n;
    }
    for (size_t i = 0; i < sizeof(tab)/sizeof(tab[0]); ++i) {
        if (!strcasecmp(name, tab[i].n)) return tab[i].r;
    }
    return R_RAX;
}

static int x86IsFpRegName(const char *name) {
    return name && !strncasecmp(name, "xmm", 3);
}

/* The register a value lives in, or NULL when it lives in memory. */
static AoStr *x86Home(IrValue *v) {
    if (!v) return NULL;
    if (v->pinned_reg) return v->pinned_reg;
    if (v->loc.kind == IR_LOC_REG && v->loc.as.reg) return v->loc.as.reg;
    return NULL;
}

static int x86FpDbl(IrValue *v) { return (int)irValueByteSize(v) == 8; }

/* reg = imm. Zero gets the canonical `xor r32, r32` (which also zeroes
 * the high half); everything else a mov, which the encoder shortens to
 * the imm32 form when it fits. Never emitted between a compare and its
 * consuming jcc/setcc, so the flags clobber is safe. */
static void x86MovImm(X86Ctx *ctx, int reg, s64 imm) {
    if (imm == 0) E2(ctx, X86MN_XOR, r32(reg), r32(reg));
    else          E2(ctx, X86MN_MOV, r64(reg), mir_imm(imm));
}

/* ---------------- memory access ----------------
 *
 * Width policy mirrors aarch64: sub-word loads zero-extend to the full
 * register, 4-byte loads sign-extend (movsxd), 8-byte loads are plain. */

static void x86Load(X86Ctx *ctx, int size, int dst, AsmOperand m) {
    switch (size) {
        case 1: E2(ctx, X86MN_MOVZX, r64(dst), mir_mem_hint(m, 1)); break;
        case 2: E2(ctx, X86MN_MOVZX, r64(dst), mir_mem_hint(m, 2)); break;
        case 4: E2(ctx, X86MN_MOVSX, r64(dst), mir_mem_hint(m, 4)); break;
        default: E2(ctx, X86MN_MOV, r64(dst), m); break;
    }
}

static void x86Store(X86Ctx *ctx, int size, int src, AsmOperand m) {
    E2(ctx, X86MN_MOV, m, rG(size, src));
}

static void x86FpLoad(X86Ctx *ctx, int is_dbl, int x, AsmOperand m) {
    E2(ctx, is_dbl ? X86MN_MOVSD : X86MN_MOVSS, xmm(x), m);
}

static void x86FpStore(X86Ctx *ctx, int is_dbl, int x, AsmOperand m) {
    E2(ctx, is_dbl ? X86MN_MOVSD : X86MN_MOVSS, m, xmm(x));
}

static void x86FrameLoad(X86Ctx *ctx, int reg, int size, int loff) {
    x86Load(ctx, size, reg, mFrame(loff));
}

static void x86FrameStore(X86Ctx *ctx, int reg, int size, int loff) {
    x86Store(ctx, size, reg, mFrame(loff));
}

/* xmm <-> xmm and gpr <-> xmm bit moves, width by the value. */
static void x86FpMoveXmm(X86Ctx *ctx, int is_dbl, int dst, int src) {
    if (dst == src) return;
    E2(ctx, is_dbl ? X86MN_MOVSD : X86MN_MOVSS, xmm(dst), xmm(src));
}
static void x86GprToXmm(X86Ctx *ctx, int is_dbl, int x, int gpr) {
    if (is_dbl) E2(ctx, X86MN_MOVQ, xmm(x), r64(gpr));
    else        E2(ctx, X86MN_MOVD, xmm(x), r32(gpr));
}
static void x86XmmToGpr(X86Ctx *ctx, int is_dbl, int gpr, int x) {
    if (is_dbl) E2(ctx, X86MN_MOVQ, r64(gpr), xmm(x));
    else        E2(ctx, X86MN_MOVD, r32(gpr), xmm(x));
}

/* ---------------- global addressing ----------------
 *
 * RELOCATED (an object file, or text for the assembler): `lea r, [rip +
 * sym]` with a fixup. A symbol from another image has no link-time
 * address of its own, so it is loaded from the GOT instead.
 *
 * ABSOLUTE (the JIT): a function in this chunk has no address yet, so
 * it takes the same RIP-relative lea and asm_jit_finalize patches the
 * displacement against the label (always reachable - same mapping).
 * Everything else - this chunk's globals in their host arena, host
 * functions through dlsym - is materialised inline: libc may be beyond
 * rel32's +/-2GB reach. */
static void x86GlobalAddrEx(X86Ctx *ctx, int reg, const char *sym, int force_got) {
    HccJit *jit = ctx->jit;
    Cctrl *cc = ctx->cc;
    if (jit->addr_mode == HCC_ADDR_RELOCATED) {
        int is_extern = force_got ||
                        (cc->extern_globals &&
                         setHas(cc->extern_globals, (char *)sym)) ||
                        asmSymbolIsPreemptible(cc, sym);
        if (is_extern) {
            char got[512];
            snprintf(got, sizeof(got), "%s@GOTPCREL", sym);
            E2(ctx, X86MN_MOV, r64(reg), mir_sym(got));
        } else {
            E2(ctx, X86MN_LEA, r64(reg), mir_sym(sym));
        }
        return;
    }
    if (hccJitIsInternalFunc(jit, sym)) {
        E2(ctx, X86MN_LEA, r64(reg), mir_sym(sym));
        return;
    }
    void *addr = mapGet(jit->host_symbols, (void *)sym);
    if (!addr) addr = asm_jit_dlsym_resolver(NULL, sym);
    x86MovImm(ctx, reg, (s64)(uintptr_t)addr);
}

static void x86GlobalAddr(X86Ctx *ctx, int reg, const char *sym) {
    x86GlobalAddrEx(ctx, reg, sym, 0);
}

/* The symbol a global is addressed by. An object or the assembler
 * wants the platform-normalised spelling (`_g` on Mach-O); the JIT
 * keys its host arena by the raw name. */
static const char *x86GlobalName(X86Ctx *ctx, AoStr *name) {
    if (ctx->jit->addr_mode == HCC_ADDR_RELOCATED) {
        return asmNormaliseGlobalLabel(ctx->cc, name)->data;
    }
    return name->data;
}

/* ---------------- load/store IR values ---------------- */

static void x86LoadToReg(X86Ctx *ctx, IrValue *val, int reg) {
    switch (val->kind) {
        case IR_VAL_CONST_INT:
            x86MovImm(ctx, reg, val->as._i64);
            break;
        case IR_VAL_CONST_FLOAT:
            /* Rare bit-cast: materialise the bit pattern. */
            x86MovImm(ctx, reg, (s64)(u64)ieee754_64(val->as._f64));
            break;
        case IR_VAL_CONST_STR:
            x86GlobalAddr(ctx, reg, val->as.str.label->data);
            break;
        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            AoStr *home = x86Home(val);
            if (home) {
                int src = x86RegFromName(home->data);
                if (src != reg) E2(ctx, X86MN_MOV, r64(reg), r64(src));
                break;
            }
            x86FrameLoad(ctx, reg, (int)irValueByteSize(val),
                         irCgGetLoff(&ctx->fn->ra, val));
            break;
        }
        case IR_VAL_GLOBAL:
            x86GlobalAddr(ctx, reg, x86GlobalName(ctx, val->as.global.name));
            break;
        default:
            loggerPanic("x86_64: cannot load int value of kind %d\n", val->kind);
    }
}

static void x86LoadToFpr(X86Ctx *ctx, IrValue *val, int x) {
    switch (val->kind) {
        case IR_VAL_CONST_FLOAT:
        case IR_VAL_CONST_INT: {
            double f = val->kind == IR_VAL_CONST_INT
                       ? (double)val->as._i64 : val->as._f64;
            int is_dbl = (val->kind == IR_VAL_CONST_FLOAT) ? x86FpDbl(val) : 1;
            u64 bits = is_dbl ? (u64)ieee754_64(f) : (u64)(u32)ieee754_32((f32)f);
            if (bits == 0) {
                E2(ctx, is_dbl ? X86MN_XORPD : X86MN_XORPS, xmm(x), xmm(x));
                break;
            }
            /* Materialise via rax = bit pattern, then movq/movd to xmm. */
            x86MovImm(ctx, R_RAX, (s64)bits);
            x86GprToXmm(ctx, is_dbl, x, R_RAX);
            break;
        }
        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            int is_dbl = x86FpDbl(val);
            AoStr *home = x86Home(val);
            if (home) {
                int src = x86RegFromName(home->data);
                /* A pinned local may name a GPR holding the bits. */
                if (x86IsFpRegName(home->data)) x86FpMoveXmm(ctx, is_dbl, x, src);
                else                            x86GprToXmm(ctx, is_dbl, x, src);
                break;
            }
            x86FpLoad(ctx, is_dbl, x, mFrame(irCgGetLoff(&ctx->fn->ra, val)));
            break;
        }
        default:
            loggerPanic("x86_64: cannot load fp value of kind %d\n", val->kind);
    }
}

static void x86StoreReg(X86Ctx *ctx, IrValue *dst, int reg) {
    AoStr *home = x86Home(dst);
    if (home) {
        int h = x86RegFromName(home->data);
        if (h != reg) E2(ctx, X86MN_MOV, r64(h), r64(reg));
        return;
    }
    x86FrameStore(ctx, reg, (int)irValueByteSize(dst),
                  irCgGetLoff(&ctx->fn->ra, dst));
}

/* `val_size` (0 = derive from dst) is the stored value's width. When
 * dst is a gep/pointer tmp aliasing a narrow field, sizing from dst (8
 * bytes) would clobber the neighbouring field; pass the value width so
 * a F32 field store is a 4-byte movss not an 8-byte movsd. */
static void x86StoreFprSized(X86Ctx *ctx, IrValue *dst, int x, int val_size) {
    int is_dbl = val_size > 0 ? (val_size == 8) : x86FpDbl(dst);
    AoStr *home = x86Home(dst);
    if (home) {
        int h = x86RegFromName(home->data);
        if (x86IsFpRegName(home->data)) x86FpMoveXmm(ctx, is_dbl, h, x);
        else                            x86XmmToGpr(ctx, is_dbl, h, x);
        return;
    }
    x86FpStore(ctx, is_dbl, x, mFrame(irCgGetLoff(&ctx->fn->ra, dst)));
}

static void x86StoreFpr(X86Ctx *ctx, IrValue *dst, int x) {
    x86StoreFprSized(ctx, dst, x, 0);
}

static void x86SpillDst(X86Ctx *ctx, IrInstr *instr, int reg) {
    x86StoreReg(ctx, instr->dst, reg);
}

static void x86SpillDstFpr(X86Ctx *ctx, IrInstr *instr, int x) {
    x86StoreFpr(ctx, instr->dst, x);
}

/* Index register for addressing-mode fusion. Returns 1 if there is an
 * index component. */
static int x86IdxReg(X86Ctx *ctx, IrInstr *instr, int *out) {
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
 * semantics (b/be/a/ae). NaN sets CF=ZF=PF=1; IR compares are
 * loose-ordered (matches aarch64). */

static int x86CcFor(IrCmpKind cmp, int is_float) {
    if (is_float) {
        switch (cmp) {
            case IR_CMP_EQ: return X86_CC_E;
            case IR_CMP_NE: return X86_CC_NE;
            case IR_CMP_LT: return X86_CC_B;
            case IR_CMP_LE: return X86_CC_BE;
            case IR_CMP_GT: return X86_CC_A;
            case IR_CMP_GE: return X86_CC_AE;
            default: loggerPanic("x86_64: bad float cmp %d\n", cmp);
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
        default: loggerPanic("x86_64: bad int cmp %d\n", cmp);
    }
}

/* All the codes used pair up as cc ^ 1 (e/ne, b/ae, l/ge, ...). */
static int x86CcInvFor(IrCmpKind cmp, int is_float) {
    return x86CcFor(cmp, is_float) ^ 1;
}

/* rax = (flags satisfy cc), as a 0/1 in the full register. */
static void x86EmitSetCC(X86Ctx *ctx, IrCmpKind cmp, int is_float) {
    E1(ctx, X86MN_SETO + x86CcFor(cmp, is_float), r8(R_RAX));
    E2(ctx, X86MN_MOVZX, r64(R_RAX), r8(R_RAX));
}

/* ---------------- phi materialisation ---------------- */

static void x86EmitOnePhi(void *ud, IrInstr *phi, IrPair *match) {
    X86Ctx *ctx = (X86Ctx *)ud;
    IrValue *v = match ? match->ir_value : NULL;
    int v_dangling = irIsTmp(v) && v->loc.kind != IR_LOC_REG &&
                     !mapHasInt(ctx->fn->ra.id_to_loff, irVarId(v));
    if (irIsFloat(phi->dst->type)) {
        if (v_dangling) E2(ctx, X86MN_XORPD, xmm(0), xmm(0));
        else            x86LoadToFpr(ctx, v, 0);
        x86StoreFpr(ctx, phi->dst, 0);
        return;
    }
    if (v_dangling) E2(ctx, X86MN_XOR, r32(R_RAX), r32(R_RAX));
    else            x86LoadToReg(ctx, v, R_RAX);
    x86StoreReg(ctx, phi->dst, R_RAX);
}

static void x86PhiMaterialise(X86Ctx *ctx, IrBlock *from, IrBlock *to) {
    hccJitPhiMaterialise(from, to, x86EmitOnePhi, ctx);
}

/* ---------------- calls ---------------- */

/* HolyC's variadic convention sends every arg past var_arg_start
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

static void x86Call(X86Ctx *ctx, AoStr *fname, int indirect) {
    if (indirect) E1(ctx, X86MN_CALL, r64(R_R11));
    else E1(ctx, X86MN_CALL, mir_sym(asmNormaliseFunctionName(ctx->cc, fname)));
}

static int x86IsByvalStruct(AstType *t) {
    return t && (t->kind == AST_TYPE_CLASS || t->kind == AST_TYPE_UNION) &&
           !t->is_intrinsic;
}

/* Copy `size` bytes from [src + 0..] to [rsp + dst_off] via rax. */
static void x86CopyToSp(X86Ctx *ctx, int src, int size, int dst_off) {
    int words = (size + 7) / 8;
    for (int k = 0; k < words; ++k) {
        int off = k * 8, rem = size - off;
        int sz = rem >= 8 ? 8 : rem;
        x86Load(ctx, sz, R_RAX, mem(src, -1, 0, off));
        x86Store(ctx, sz, R_RAX, mSp(dst_off + off));
    }
}

/* ---------------- System V struct-by-value calls ----------------
 *
 * Eightbyte INTEGER -> GP regs, SSE -> XMM regs, MEMORY (>16B or
 * register overflow) on the stack by value. A <=16-byte aggregate
 * return comes back in registers; its destination buffer (args[0]) is
 * not an ABI argument. */

static void x86SysvCount(AstType *t, SysvClass *cl, int *neb, int *mem_out,
                         int *gp, int *sse)
{
    *neb = 0; *gp = 0; *sse = 0;
    *mem_out = astSysvClassify(t, cl, neb);
    if (*mem_out) return;
    for (int e = 0; e < *neb; ++e) {
        if (cl[e] == SYSV_INTEGER) (*gp)++; else (*sse)++;
    }
}

static void x86EmitSysvCall(X86Ctx *ctx, IrInstr *instr, Vec *args,
                            AoStr *fname, int indirect)
{
    u64 n = args ? args->size : 0;

    AstType *ret_st = (instr->flags & IRCG_CALL_AGG_RETURN) && instr->dst
                      ? instr->dst->byval_struct_type : NULL;
    /* The result IS this function's return value and the `ret` is next,
     * so the callee has already left the bytes in rax/rdx (or xmm0/1).
     * The buffer is gone from the IR along with the address that
     * reached it. args[0] is still skipped as an argument. */
    int fwd_ret = (instr->flags & IRCG_AGG_RET_FORWARD) != 0;
    int reg_ret = ret_st && ret_st->size <= 16 && !fwd_ret;
    u64 a0 = (ret_st && ret_st->size <= 16) ? 1 : 0;

    /* Pre-pass: size the stack-argument area. */
    int nsaa_total = 0;
    {
        int g = 0, s = 0;
        for (u64 i = a0; i < n; ++i) {
            IrValue *a = vecGet(IrValue *, args, i);
            if (a->byval_struct_type) {
                AstType *t = a->byval_struct_type;
                SysvClass cl[2]; int neb, memc, gp, sse;
                x86SysvCount(t, cl, &neb, &memc, &gp, &sse);
                if (!memc && g + gp <= 6 && s + sse <= 8) { g += gp; s += sse; }
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
    if (stack_bytes > 0) E2(ctx, X86MN_SUB, r64(R_RSP), mir_imm(stack_bytes));
    int stack_off = 0;

    if (reg_ret) {
        /* Compute the destination buffer address into r10 and stash it
         * across the call (r10 is caller-saved). */
        x86LoadToReg(ctx, vecGet(IrValue *, args, 0), R_R10);
        x86Store(ctx, 8, R_R10, mSp(dest_save_off));
    }

    int ngp = 0, nsse = 0;
    for (u64 i = a0; i < n; ++i) {
        IrValue *a = vecGet(IrValue *, args, i);
        if (a->byval_struct_type) {
            AstType *t = a->byval_struct_type;
            SysvClass cl[2]; int neb, memc, gp, sse;
            x86SysvCount(t, cl, &neb, &memc, &gp, &sse);
            x86LoadToReg(ctx, a, R_R10); /* struct address */
            if (memc || ngp + gp > 6 || nsse + sse > 8) {
                x86CopyToSp(ctx, R_R10, t->size, stack_off);
                stack_off += (t->size + 7) & ~7;
                continue;
            }
            for (int e = 0; e < neb; ++e) {
                int off = e * 8, rem = t->size - off;
                int sz = rem >= 8 ? 8 : rem;
                if (cl[e] == SYSV_SSE) {
                    x86FpLoad(ctx, sz == 8, nsse, mem(R_R10, -1, 0, off));
                    nsse++;
                } else {
                    x86Load(ctx, sz, kIntArgRegs[ngp], mem(R_R10, -1, 0, off));
                    ngp++;
                }
            }
            continue;
        }
        if (irIsFloat(a->type)) {
            if (nsse < 8) {
                x86LoadToFpr(ctx, a, nsse++);
            } else {
                x86LoadToFpr(ctx, a, 0);
                x86FpStore(ctx, x86FpDbl(a), 0, mSp(stack_off));
                stack_off += 8;
            }
        } else {
            if (ngp < 6) {
                x86LoadToReg(ctx, a, kIntArgRegs[ngp++]);
            } else {
                x86LoadToReg(ctx, a, R_RAX);
                x86Store(ctx, 8, R_RAX, mSp(stack_off));
                stack_off += 8;
            }
        }
    }

    x86Call(ctx, fname, indirect);

    if (reg_ret) {
        /* Reload the destination buffer and copy the SysV result
         * registers (INTEGER eightbytes -> rax,rdx; SSE -> xmm0,xmm1)
         * into it. */
        SysvClass cl[2]; int neb = 0;
        astSysvClassify(ret_st, cl, &neb);
        int gpret[2] = { R_RAX, R_RDX };
        int gpi = 0, ssei = 0;
        x86Load(ctx, 8, R_R10, mSp(dest_save_off));
        for (int e = 0; e < neb; ++e) {
            int off = e * 8, rem = (int)ret_st->size - off;
            int sz = rem >= 8 ? 8 : rem;
            if (cl[e] == SYSV_SSE) {
                x86FpStore(ctx, sz == 8, ssei, mem(R_R10, -1, 0, off));
                ssei++;
            } else {
                x86Store(ctx, sz, gpret[gpi], mem(R_R10, -1, 0, off));
                gpi++;
            }
        }
    }

    if (stack_bytes > 0) E2(ctx, X86MN_ADD, r64(R_RSP), mir_imm(stack_bytes));

    if (reg_ret) {
        if (instr->dst) x86SpillDst(ctx, instr, R_R10);  /* dst = buffer addr */
    } else if (!fwd_ret && instr->dst && instr->dst->type != IR_TYPE_VOID) {
        if (irIsFloat(instr->dst->type)) x86SpillDstFpr(ctx, instr, 0);
        else                             x86SpillDst(ctx, instr, R_RAX);
    }
}

/* The callee side: receive by-value struct params (and register-
 * overflow scalars) into their frame slots. An unreferenced param gets
 * no slot, but the caller still placed it so the counters and the
 * incoming-stack cursor (args start at [rbp + 16]) must advance. */
static void x86EmitSysvParamPrologue(X86Ctx *ctx) {
    Ast *ast = ctx->ast;
    if (!ast->params) return;
    int ngp = 0, nsse = 0;

    AstType *rt = ast->type ? ast->type->rettype : NULL;
    if (x86IsByvalStruct(rt) && rt->size > 0) ngp = 1; /* hidden ret ptr */
    int incoming_off = 0;

    for (u64 i = 0; i < ast->params->size; ++i) {
        Ast *p = vecGet(Ast *, ast->params, i);
        if (p->kind == AST_VAR_ARGS) break;
        AstType *t = p->type;

        IrValue *slot = irFnGetVar(ctx->fn, irGetParamId(p));
        int has_slot = slot && mapHasInt(ctx->fn->ra.id_to_loff, irVarId(slot));
        int loff = has_slot ? irCgGetLoff(&ctx->fn->ra, slot) : 0;

        if (!x86IsByvalStruct(t)) {
            int is_float = (t->kind == AST_TYPE_FLOAT);
            if (is_float ? (nsse < 8) : (ngp < 6)) {
                if (is_float) nsse++; else ngp++; /* reg: arrive+store */
            } else {
                if (has_slot) {
                    x86Load(ctx, t->size, R_RAX, mFrame(16 + incoming_off));
                    x86FrameStore(ctx, R_RAX, t->size, loff);
                }
                incoming_off += 8;
            }
            continue;
        }

        SysvClass cl[2]; int neb, memc, gp, sse;
        x86SysvCount(t, cl, &neb, &memc, &gp, &sse);

        if (memc || ngp + gp > 6 || nsse + sse > 8) {
            if (has_slot) {
                int words = (t->size + 7) / 8;
                for (int k = 0; k < words; ++k) {
                    int off = k * 8, rem = t->size - off;
                    int sz = rem >= 8 ? 8 : rem;
                    x86Load(ctx, sz, R_RAX, mFrame(16 + incoming_off + off));
                    x86FrameStore(ctx, R_RAX, sz, loff + off);
                }
            }
            incoming_off += (t->size + 7) & ~7;
            continue;
        }

        for (int e = 0; e < neb; ++e) {
            int off = e * 8, rem = t->size - off;
            int sz = rem >= 8 ? 8 : rem;
            if (cl[e] == SYSV_SSE) {
                if (has_slot) x86FpStore(ctx, sz == 8, nsse, mFrame(loff + off));
                nsse++;
            } else {
                if (has_slot) x86FrameStore(ctx, kIntArgRegs[ngp], sz, loff + off);
                ngp++;
            }
        }
    }
}

/* ---------------- compares and branches ---------------- */

/* Flags := r1 <cmp> r2. */
static void x86EmitCompare(X86Ctx *ctx, IrInstr *instr, int is_float) {
    if (is_float) {
        x86LoadToFpr(ctx, instr->r1, 0);
        x86LoadToFpr(ctx, instr->r2, 1);
        E2(ctx, x86FpDbl(instr->r1) ? X86MN_UCOMISD : X86MN_UCOMISS, xmm(0), xmm(1));
        return;
    }
    x86LoadToReg(ctx, instr->r1, R_RAX);
    s64 imm;
    if (x86IsImm32(instr->r2, &imm)) {
        E2(ctx, X86MN_CMP, r64(R_RAX), mir_imm(imm));
    } else {
        x86LoadToReg(ctx, instr->r2, R_RCX);
        E2(ctx, X86MN_CMP, r64(R_RAX), r64(R_RCX));
    }
}

/* Conditional branch to `t`, else `f`, given the flags are set and
 * `cc_t`/`cc_f` are the taken/not-taken conditions. Phi moves on an
 * edge need a landing strip of their own. */
static void x86EmitCondBranch(X86Ctx *ctx, IrBlock *t, IrBlock *f, int cc_t, int cc_f) {
    HccJit *jit = ctx->jit;
    int t_ln = hccJitBlockLocalNum(jit, ctx->fn, t);
    int f_ln = hccJitBlockLocalNum(jit, ctx->fn, f);
    if (!irBlockHasPhi(t) && !irBlockHasPhi(f)) {
        if (ctx->next_block == t) {
            EJCC(ctx, cc_f, f_ln);
        } else if (ctx->next_block == f) {
            EJCC(ctx, cc_t, t_ln);
        } else {
            EJCC(ctx, cc_t, t_ln);
            EJMP(ctx, f_ln);
        }
        return;
    }
    int else_ln = hccJitFreshLocalNum(jit);
    EJCC(ctx, cc_f, else_ln);
    x86PhiMaterialise(ctx, ctx->cur_block, t);
    EJMP(ctx, t_ln);
    ELABEL(ctx, else_ln);
    x86PhiMaterialise(ctx, ctx->cur_block, f);
    if (ctx->next_block != f) EJMP(ctx, f_ln);
}

/* ---------------- the main IR emitter ---------------- */

static int x86AluMn(IrOp op) {
    switch (op) {
        case IR_IADD: return X86MN_ADD;
        case IR_ISUB: return X86MN_SUB;
        case IR_AND:  return X86MN_AND;
        case IR_OR:   return X86MN_OR;
        case IR_XOR:  return X86MN_XOR;
        default: return TASM_MN_UNKNOWN;
    }
}

static void x86EmitInstr(X86Ctx *ctx, IrInstr *instr) {
    HccJit *jit = ctx->jit;

    switch (instr->op) {
        case IR_NOP:
        case IR_LABEL:
        case IR_ALLOCA:
        case IR_GEP:
            break;

        case IR_LOAD: {
            if (instr->r1 && instr->r1->pinned_reg) {
                E2(ctx, X86MN_MOV, r64(R_RAX),
                   r64(x86RegFromName(instr->r1->pinned_reg->data)));
                x86SpillDst(ctx, instr, R_RAX);
                break;
            }
            int loff = irCgGetLoff(&ctx->fn->ra, instr->r1);
            if (instr->dst && irIsFloat(instr->dst->type)) {
                x86FpLoad(ctx, x86FpDbl(instr->dst), 0, mFrame(loff));
                x86SpillDstFpr(ctx, instr, 0);
                break;
            }
            int size = instr->dst ? (int)instr->dst->as.var.size : 8;
            x86FrameLoad(ctx, R_RAX, size, loff);
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_STORE: {
            int src_reg = R_RAX;
            int have_src = 0;
            if (instr->r1 && instr->r1->loc.kind == IR_LOC_REG &&
                instr->r1->loc.as.reg)
            {
                src_reg = x86RegFromName(instr->r1->loc.as.reg->data);
                have_src = 1;
            }
            if (instr->r1 && irIsFloat(instr->r1->type)) {
                int src_xmm = 0;
                if (have_src && x86IsFpRegName(instr->r1->loc.as.reg->data)) {
                    src_xmm = src_reg;
                } else {
                    x86LoadToFpr(ctx, instr->r1, 0);
                }
                x86StoreFprSized(ctx, instr->dst, src_xmm,
                                 (int)irValueByteSize(instr->r1));
                break;
            }
            if (!have_src) { x86LoadToReg(ctx, instr->r1, R_RAX); src_reg = R_RAX; }
            /* Pinned destination lives in a register, not a slot. */
            if (instr->dst && instr->dst->pinned_reg) {
                int home = x86RegFromName(instr->dst->pinned_reg->data);
                if (home != src_reg) E2(ctx, X86MN_MOV, r64(home), r64(src_reg));
                break;
            }
            x86FrameStore(ctx, src_reg, (int)irValueByteSize(instr->r1),
                          irCgGetLoff(&ctx->fn->ra, instr->dst));
            break;
        }

        case IR_LOAD_DEREF: {
            int base = R_RCX;
            if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
                /* Global base: materialise the address into rcx and go
                 * through the ordinary disp/idx addressing below. */
                x86GlobalAddr(ctx, R_RCX, x86GlobalName(ctx, instr->r1->as.global.name));
            } else if (instr->r1 && instr->r1->loc.kind == IR_LOC_REG &&
                       instr->r1->loc.as.reg)
            {
                base = x86RegFromName(instr->r1->loc.as.reg->data);
            } else {
                x86LoadToReg(ctx, instr->r1, R_RCX);
            }
            int idx = -1; int has_idx = x86IdxReg(ctx, instr, &idx);
            AsmOperand m = mem(base, has_idx ? idx : -1, instr->scale, instr->disp);
            if (instr->dst && irIsFloat(instr->dst->type)) {
                x86FpLoad(ctx, x86FpDbl(instr->dst), 0, m);
                x86SpillDstFpr(ctx, instr, 0);
                break;
            }
            int size = instr->dst ? (int)instr->dst->as.var.size : 8;
            x86Load(ctx, size, R_RAX, m);
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_STORE_DEREF: {
            int is_global = instr->dst && instr->dst->kind == IR_VAL_GLOBAL;
            int base = R_RCX;
            int addr_in_reg = !is_global && instr->dst &&
                              instr->dst->loc.kind == IR_LOC_REG &&
                              instr->dst->loc.as.reg;
            if (addr_in_reg) base = x86RegFromName(instr->dst->loc.as.reg->data);
            int idx = -1; int has_idx = x86IdxReg(ctx, instr, &idx);
            if (is_global) {
                x86GlobalAddr(ctx, R_RCX, x86GlobalName(ctx, instr->dst->as.global.name));
            } else if (!addr_in_reg) {
                x86LoadToReg(ctx, instr->dst, R_RCX);
            }
            if (instr->r1 && irIsFloat(instr->r1->type)) {
                /* Materialising a float value can clobber rax (a const
                 * is built with mov+movq through rax). If the
                 * destination address sits in rax, that store target
                 * would be destroyed: relocate base / idx out of rax
                 * first. */
                if (base == R_RAX) {
                    int t = (has_idx && idx == R_RCX) ? R_RDX : R_RCX;
                    E2(ctx, X86MN_MOV, r64(t), r64(R_RAX));
                    base = t;
                }
                if (has_idx && idx == R_RAX) {
                    int t = (base == R_RCX) ? R_RDX : R_RCX;
                    E2(ctx, X86MN_MOV, r64(t), r64(R_RAX));
                    idx = t;
                }
                x86LoadToFpr(ctx, instr->r1, 0);
                x86FpStore(ctx, x86FpDbl(instr->r1), 0,
                           mem(base, has_idx ? idx : -1, instr->scale, instr->disp));
                break;
            }
            int sz = (int)irValueByteSize(instr->r1);
            /* Pick a val reg that conflicts with neither base nor idx. */
            int val = addr_in_reg ? R_RCX : R_RAX;
            if (has_idx && val == idx) val = (R_RDX == base) ? R_R10 : R_RDX;
            if (val == base) val = (has_idx && idx == R_RDX) ? R_R10 : R_RDX;
            x86LoadToReg(ctx, instr->r1, val);
            x86Store(ctx, sz, val, mem(base, has_idx ? idx : -1, instr->scale, instr->disp));
            break;
        }

        case IR_RMW_DEREF: {
            int base = R_RCX;
            int addr_in_reg = instr->dst && instr->dst->loc.kind == IR_LOC_REG &&
                              instr->dst->loc.as.reg;
            if (addr_in_reg) base = x86RegFromName(instr->dst->loc.as.reg->data);
            else if (instr->dst && instr->dst->kind == IR_VAL_GLOBAL)
                x86GlobalAddr(ctx, R_RCX, x86GlobalName(ctx, instr->dst->as.global.name));
            else x86LoadToReg(ctx, instr->dst, R_RCX);

            int idx = -1; int has_idx = x86IdxReg(ctx, instr, &idx);
            int sz = (int)irValueByteSize(instr->r1);
            AsmOperand m = mem(base, has_idx ? idx : -1, instr->scale, instr->disp);
            /* Load-modify-store through an accumulator that aliases
             * neither base nor idx; rhs goes through r10. */
            int acc = R_RAX;
            if (base == R_RAX || (has_idx && idx == R_RAX)) acc = R_R11;
            x86Load(ctx, sz, acc, m);
            x86LoadToReg(ctx, instr->r1, R_R10);
            int mn = x86AluMn(instr->extra.rmw_op);
            if (mn == TASM_MN_UNKNOWN)
                loggerPanic("x86_64: bad RMW op %d\n", instr->extra.rmw_op);
            E2(ctx, mn, r64(acc), r64(R_R10));
            x86Store(ctx, sz, acc, m);
            break;
        }

        case IR_LEA: {
            if (instr->r1 && instr->r1->kind == IR_VAL_GLOBAL) {
                const char *name = x86GlobalName(ctx, instr->r1->as.global.name);
                int got = 0;
                if (instr->r1->flags & IR_VAL_FLAG_FUNC) {
                    name = asmNormaliseFunctionName(ctx->cc, instr->r1->as.global.name);
                    got = asmFunctionIsExtern(ctx->cc, instr->r1->as.global.name);
                }
                x86GlobalAddrEx(ctx, R_RAX, name, got);
            } else if (instr->r1) {
                E2(ctx, X86MN_LEA, r64(R_RAX),
                   mFrame(irCgGetLoff(&ctx->fn->ra, instr->r1)));
            }
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_IADD: case IR_ISUB: case IR_AND: case IR_OR: case IR_XOR: {
            int mn = x86AluMn(instr->op);
            x86LoadToReg(ctx, instr->r1, R_RAX);
            s64 imm;
            if (x86IsImm32(instr->r2, &imm)) {
                E2(ctx, mn, r64(R_RAX), mir_imm(imm));
            } else if (instr->r2->loc.kind == IR_LOC_REG && instr->r2->loc.as.reg) {
                E2(ctx, mn, r64(R_RAX), r64(x86RegFromName(instr->r2->loc.as.reg->data)));
            } else {
                x86LoadToReg(ctx, instr->r2, R_RCX);
                E2(ctx, mn, r64(R_RAX), r64(R_RCX));
            }
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_IMUL:
            x86LoadToReg(ctx, instr->r1, R_RAX);
            if (instr->r2->loc.kind == IR_LOC_REG && instr->r2->loc.as.reg) {
                E2(ctx, X86MN_IMUL, r64(R_RAX),
                   r64(x86RegFromName(instr->r2->loc.as.reg->data)));
            } else {
                x86LoadToReg(ctx, instr->r2, R_RCX);
                E2(ctx, X86MN_IMUL, r64(R_RAX), r64(R_RCX));
            }
            x86SpillDst(ctx, instr, R_RAX);
            break;

        case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM: {
            /* idiv/div divide rdx:rax by the source operand. Quotient
             * lands in rax, remainder in rdx. */
            int is_signed = (instr->op == IR_IDIV || instr->op == IR_IREM);
            int is_rem    = (instr->op == IR_IREM || instr->op == IR_UREM);
            x86LoadToReg(ctx, instr->r1, R_RAX);
            x86LoadToReg(ctx, instr->r2, R_RCX);
            if (is_signed) {
                E0(ctx, X86MN_CQO);
                E1(ctx, X86MN_IDIV, r64(R_RCX));
            } else {
                E2(ctx, X86MN_XOR, r32(R_RDX), r32(R_RDX));
                E1(ctx, X86MN_DIV, r64(R_RCX));
            }
            if (is_rem) E2(ctx, X86MN_MOV, r64(R_RAX), r64(R_RDX));
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_INEG:
        case IR_NOT:
            x86LoadToReg(ctx, instr->r1, R_RAX);
            E1(ctx, instr->op == IR_INEG ? X86MN_NEG : X86MN_NOT, r64(R_RAX));
            x86SpillDst(ctx, instr, R_RAX);
            break;

        case IR_SHL: case IR_SHR: case IR_SAR: {
            int mn = instr->op == IR_SHL ? X86MN_SHL
                   : instr->op == IR_SHR ? X86MN_SHR : X86MN_SAR;
            x86LoadToReg(ctx, instr->r1, R_RAX);
            if (instr->r2 && instr->r2->kind == IR_VAL_CONST_INT &&
                instr->r2->as._i64 >= 0 && instr->r2->as._i64 < 64)
            {
                E2(ctx, mn, r64(R_RAX), mir_imm(instr->r2->as._i64));
            } else {
                /* Variable shift count must live in cl. */
                x86LoadToReg(ctx, instr->r2, R_RCX);
                E2(ctx, mn, r64(R_RAX), r8(R_RCX));
            }
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV: {
            int is_dbl = x86FpDbl(instr->dst);
            int mn = instr->op == IR_FADD ? (is_dbl ? X86MN_ADDSD : X86MN_ADDSS)
                   : instr->op == IR_FSUB ? (is_dbl ? X86MN_SUBSD : X86MN_SUBSS)
                   : instr->op == IR_FMUL ? (is_dbl ? X86MN_MULSD : X86MN_MULSS)
                                          : (is_dbl ? X86MN_DIVSD : X86MN_DIVSS);
            x86LoadToFpr(ctx, instr->r1, 0);
            x86LoadToFpr(ctx, instr->r2, 1);
            E2(ctx, mn, xmm(0), xmm(1));
            x86SpillDstFpr(ctx, instr, 0);
            break;
        }

        case IR_FNEG:
            /* No fneg on SSE - flip the sign bit by XORing in a mask
             * materialised through rax/xmm1. F64 flips bit 63, F32 bit
             * 31, with the matching xorpd/xorps width. */
            x86LoadToFpr(ctx, instr->r1, 0);
            if (x86FpDbl(instr->dst)) {
                E2(ctx, X86MN_MOV, r64(R_RAX), mir_imm((s64)0x8000000000000000ULL));
                E2(ctx, X86MN_MOVQ, xmm(1), r64(R_RAX));
                E2(ctx, X86MN_XORPD, xmm(0), xmm(1));
            } else {
                E2(ctx, X86MN_MOV, r64(R_RAX), mir_imm((s64)0x80000000ULL));
                E2(ctx, X86MN_MOVD, xmm(1), r32(R_RAX));
                E2(ctx, X86MN_XORPS, xmm(0), xmm(1));
            }
            x86SpillDstFpr(ctx, instr, 0);
            break;

        case IR_ICMP:
            x86EmitCompare(ctx, instr, 0);
            x86EmitSetCC(ctx, instr->extra.cmp_kind, 0);
            x86SpillDst(ctx, instr, R_RAX);
            break;

        case IR_FCMP:
            x86EmitCompare(ctx, instr, 1);
            x86EmitSetCC(ctx, instr->extra.cmp_kind, 1);
            x86SpillDst(ctx, instr, R_RAX);
            break;

        case IR_CMP_BR: {
            IrCmpKind kind = instr->extra.cmp_br.cmp_kind;
            int is_float = instr->r1 && irIsFloat(instr->r1->type);
            x86EmitCompare(ctx, instr, is_float);
            x86EmitCondBranch(ctx, instr->extra.cmp_br.target_block,
                              instr->extra.cmp_br.fallthrough_block,
                              x86CcFor(kind, is_float), x86CcInvFor(kind, is_float));
            break;
        }

        case IR_BR:
            x86LoadToReg(ctx, instr->dst, R_RAX);
            E2(ctx, X86MN_TEST, r64(R_RAX), r64(R_RAX));
            x86EmitCondBranch(ctx, instr->extra.blocks.target_block,
                              instr->extra.blocks.fallthrough_block,
                              X86_CC_NE, X86_CC_E);
            break;

        case IR_JMP: {
            IrBlock *target = instr->extra.blocks.target_block;
            x86PhiMaterialise(ctx, ctx->cur_block, target);
            if (target != ctx->next_block) {
                EJMP(ctx, hccJitBlockLocalNum(jit, ctx->fn, target));
            }
            break;
        }

        case IR_RET:
            if (instr->dst && instr->dst->byval_struct_type) {
                /* <=16-byte struct returned in registers: load the
                 * return slot's bytes into rax,rdx / xmm0,xmm1. */
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
                        x86FpLoad(ctx, sz == 8, ssei, mFrame(loff + off));
                        ssei++;
                    } else {
                        x86Load(ctx, sz, gpret[gpi], mFrame(loff + off));
                        gpi++;
                    }
                }
            } else if (instr->dst) {
                if (irIsFloat(instr->dst->type)) x86LoadToFpr(ctx, instr->dst, 0);
                else                              x86LoadToReg(ctx, instr->dst, R_RAX);
            }
            /* The epilogue label is defined straight after the last
             * block, so a ret ending that block falls through to it. */
            if (ctx->next_block != NULL ||
                ctx->cur_block->instructions->prev->value != (void *)instr)
            {
                EJMP(ctx, hccJitEpilogueLocalNum(jit, ctx->fn));
            }
            break;

        case IR_TRUNC: {
            x86LoadToReg(ctx, instr->r1, R_RAX);
            int sz = instr->dst ? (int)instr->dst->as.var.size : 8;
            if (sz == 4)     E2(ctx, X86MN_MOV, r32(R_RAX), r32(R_RAX));
            else if (sz < 8) E2(ctx, X86MN_MOVZX, r64(R_RAX), rG(sz, R_RAX));
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_ZEXT: {
            x86LoadToReg(ctx, instr->r1, R_RAX);
            int sz = (int)irValueByteSize(instr->r1);
            if (sz == 4)     E2(ctx, X86MN_MOV, r32(R_RAX), r32(R_RAX));
            else if (sz < 8) E2(ctx, X86MN_MOVZX, r64(R_RAX), rG(sz, R_RAX));
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_SEXT: {
            x86LoadToReg(ctx, instr->r1, R_RAX);
            int sz = (int)irValueByteSize(instr->r1);
            if (sz < 8) E2(ctx, X86MN_MOVSX, r64(R_RAX), rG(sz, R_RAX));
            x86SpillDst(ctx, instr, R_RAX);
            break;
        }

        case IR_FPTRUNC:
            x86LoadToFpr(ctx, instr->r1, 0);
            E2(ctx, X86MN_CVTSD2SS, xmm(0), xmm(0));
            x86SpillDstFpr(ctx, instr, 0);
            break;

        case IR_FPEXT:
            x86LoadToFpr(ctx, instr->r1, 0);
            E2(ctx, X86MN_CVTSS2SD, xmm(0), xmm(0));
            x86SpillDstFpr(ctx, instr, 0);
            break;

        case IR_FPTOSI:
        case IR_FPTOUI:
            /* x86 has no unsigned truncation pre-AVX512; treat as
             * signed. */
            x86LoadToFpr(ctx, instr->r1, 0);
            E2(ctx, x86FpDbl(instr->r1) ? X86MN_CVTTSD2SI : X86MN_CVTTSS2SI,
               r64(R_RAX), xmm(0));
            x86SpillDst(ctx, instr, R_RAX);
            break;

        case IR_SITOFP:
        case IR_UITOFP:
            /* Signed conversion either way. */
            x86LoadToReg(ctx, instr->r1, R_RAX);
            E2(ctx, x86FpDbl(instr->dst) ? X86MN_CVTSI2SD : X86MN_CVTSI2SS,
               xmm(0), r64(R_RAX));
            x86SpillDstFpr(ctx, instr, 0);
            break;

        case IR_PTRTOINT:
        case IR_INTTOPTR:
        case IR_BITCAST: {
            int dst_fp = irIsFloat(instr->dst->type);
            int src_fp = irIsFloat(instr->r1->type);
            if (dst_fp == src_fp) {
                if (dst_fp) { x86LoadToFpr(ctx, instr->r1, 0); x86SpillDstFpr(ctx, instr, 0); }
                else        { x86LoadToReg(ctx, instr->r1, R_RAX); x86SpillDst(ctx, instr, R_RAX); }
            } else if (dst_fp) {
                x86LoadToReg(ctx, instr->r1, R_RAX);
                x86GprToXmm(ctx, x86FpDbl(instr->dst), 0, R_RAX);
                x86SpillDstFpr(ctx, instr, 0);
            } else {
                x86LoadToFpr(ctx, instr->r1, 0);
                x86XmmToGpr(ctx, x86FpDbl(instr->r1), R_RAX, 0);
                x86SpillDst(ctx, instr, R_RAX);
            }
            break;
        }

        case IR_PHI:
            /* Materialised at the predecessor's terminator. */
            break;

        case IR_CALL: {
            IrValue *wrap = instr->r1;
            Vec *args = wrap ? wrap->as.array.values : NULL;
            AoStr *fname = wrap ? wrap->as.array.label : NULL;
            int indirect = (fname == NULL);
            if (indirect) {
                if (!instr->r2) loggerPanic("x86_64: indirect call without target\n");
                /* r11 sits outside the arg-reg range so the arg loads
                 * below can't clobber the target. */
                x86LoadToReg(ctx, instr->r2, R_R11);
            }
            /* Route through the SysV path for by-value struct args OR a
             * by-value struct return. */
            {
                int use_sysv = (instr->flags & IRCG_CALL_AGG_RETURN) != 0;
                for (u64 ai = 0; args && ai < args->size; ++ai) {
                    if (vecGet(IrValue *, args, ai)->byval_struct_type) {
                        use_sysv = 1;
                        break;
                    }
                }
                if (use_sysv) {
                    x86EmitSysvCall(ctx, instr, args, fname, indirect);
                    break;
                }
            }
            Ast *callee = NULL;
            if (!indirect) {
                callee = (Ast *)mapGetLen(ctx->cc->global_env, fname->data, fname->len);
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

            u64 n = args ? args->size : 0;
            u8 *is_stack = n ? calloc(n, 1) : NULL;
            s32 n_stack = x86PartitionCallArgs(is_stack, n, args, holyc_variadic,
                                               var_arg_start);
            /* Stack args sit in 8-byte lanes; SysV requires 16-byte
             * alignment at the call instruction. */
            s32 stack_bytes = n_stack * 8;
            if (stack_bytes & 15) stack_bytes += 8;
            if (stack_bytes > 0) E2(ctx, X86MN_SUB, r64(R_RSP), mir_imm(stack_bytes));

            /* If arg[0]'s producer left it in a register, route it into
             * rdi before the stack-arg loop - that loop uses rax as
             * scratch and would clobber an rax-resident value. */
            int arg0_in_reg = 0;
            if (n > 0 && !is_stack[0]) {
                IrValue *a0 = vecGet(IrValue *, args, 0);
                if (a0 && !irIsFloat(a0->type) &&
                    a0->loc.kind == IR_LOC_REG && a0->loc.as.reg)
                {
                    int src = x86RegFromName(a0->loc.as.reg->data);
                    if (src != kIntArgRegs[0])
                        E2(ctx, X86MN_MOV, r64(kIntArgRegs[0]), r64(src));
                    arg0_in_reg = 1;
                }
            }

            /* Place stack args at [rsp + i*8] in source order. */
            s32 stack_idx = 0;
            for (u64 i = 0; i < n; ++i) {
                if (!is_stack[i]) continue;
                IrValue *a = vecGet(IrValue *, args, i);
                int off = stack_idx * 8;
                if (irIsFloat(a->type)) {
                    x86LoadToFpr(ctx, a, 0);
                    x86FpStore(ctx, x86FpDbl(a), 0, mSp(off));
                } else {
                    x86LoadToReg(ctx, a, R_RAX);
                    x86Store(ctx, 8, R_RAX, mSp(off));
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

            /* SysV variadic ABI: al = number of XMM regs used. */
            if (extern_variadic || holyc_variadic) {
                E2(ctx, X86MN_MOV, r8(R_RAX), mir_imm(xmm_used));
            }

            x86Call(ctx, fname, indirect);
            if (stack_bytes > 0) E2(ctx, X86MN_ADD, r64(R_RSP), mir_imm(stack_bytes));
            if (instr->dst && instr->dst->type != IR_TYPE_VOID) {
                if (irIsFloat(instr->dst->type)) x86SpillDstFpr(ctx, instr, 0);
                else                              x86SpillDst(ctx, instr, R_RAX);
            }
            free(is_stack);
            break;
        }

        case IR_ASM:
            /* Inline `asm {...}`: the bytes are assembled separately and
             * spliced in at this point, so everything selected so far
             * has to be sunk first. No peephole across the barrier. */
            if (instr->extra.asm_fragments) {
                loggerPanic("x86_64: `&var` asm fragments are not "
                            "supported; address locals directly\n");
            }
            x86Flush(ctx, 0);
            if (instr->r1 && instr->r1->as.str.str) {
                if (ctx->text) {
                    asmEmitBlockBytes(ctx->cc, ctx->text, instr->r1->as.str.str, 0);
                } else if (hccJitAssembleText(jit, instr->r1->as.str.str, 0) != 0) {
                    loggerPanic("x86_64: failed to assemble inline asm\n");
                }
            }
            break;

        case IR_FRAME_ADDR:
            E2(ctx, X86MN_MOV, r64(R_RAX), r64(R_RBP));
            x86SpillDst(ctx, instr, R_RAX);
            break;

        case IR_SELECT:
        case IR_SWITCH:
        case IR_VA_ARG:
        case IR_VA_START:
        case IR_VA_END:
            loggerPanic("x86_64: op %d not yet implemented\n", instr->op);
        default:
            loggerPanic("x86_64: unknown op %d\n", instr->op);
    }
}

/* ---------------- prologue / epilogue ----------------
 *
 * Every register a local/param is pinned to is preserved. XMM regs
 * cannot be pushed, so instead of push/pop one 8-byte slot per pinned
 * register (rounded to 16 for SysV alignment) is reserved below the
 * locals and each is spilled with mov (GPR) or movsd (XMM). */

static u32 x86PinnedSaveBytes(Vec *pinned) {
    if (!pinned) return 0;
    return (u32)((pinned->size * 8 + 15) & ~(u64)15);
}

static void x86SavePinnedRegs(X86Ctx *ctx, int store) {
    Vec *pinned = ctx->pinned;
    if (!pinned || pinned->size == 0) return;
    u32 bytes = x86PinnedSaveBytes(pinned);
    if (store) E2(ctx, X86MN_SUB, r64(R_RSP), mir_imm(bytes));
    for (u64 i = 0; i < pinned->size; ++i) {
        AoStr *r = (AoStr *)pinned->entries[i];
        int num = x86RegFromName(r->data);
        AsmOperand slot = mSp((s64)(i * 8));
        if (x86IsFpRegName(r->data)) {
            if (store) E2(ctx, X86MN_MOVSD, slot, xmm(num));
            else       E2(ctx, X86MN_MOVSD, xmm(num), slot);
        } else {
            if (store) E2(ctx, X86MN_MOV, slot, r64(num));
            else       E2(ctx, X86MN_MOV, r64(num), slot);
        }
    }
    if (!store) E2(ctx, X86MN_ADD, r64(R_RSP), mir_imm(bytes));
}

static void x86EmitPrologue(X86Ctx *ctx) {
    if (ctx->omit_frame) return;
    E1(ctx, X86MN_PUSH, r64(R_RBP));
    E2(ctx, X86MN_MOV, r64(R_RBP), r64(R_RSP));
    /* Round up to 16 so rsp stays aligned for inner calls. The call
     * pushed 8 bytes and `push rbp` another 8, so the frame base is
     * aligned; the sub has to preserve that. */
    u32 aligned = ((u32)ctx->fn->stack_space + 15u) & ~15u;
    if (aligned > 0) E2(ctx, X86MN_SUB, r64(R_RSP), mir_imm(aligned));
    /* Pinned saves AFTER reserving locals so rbp-relative slot offsets
     * stay valid throughout the body. */
    x86SavePinnedRegs(ctx, 1);
}

static void x86EmitEpilogue(X86Ctx *ctx) {
    if (!ctx->omit_frame) {
        x86SavePinnedRegs(ctx, 0);
        E0(ctx, X86MN_LEAVE);
    }
    E0(ctx, X86MN_RET);
}

/* The registers this function's pinned locals and params occupy. */
static Vec *x86CollectPinned(Ast *ast) {
    Vec *pinned = NULL;
    if (ast->locals) {
        listForEach(ast->locals) {
            Ast *l = (Ast *)it->value;
            if (!l || l->kind != AST_LVAR) continue;
            if (l->pinned_kind != LVAR_REG || !l->pinned_reg) continue;
            if (!pinned) pinned = irValueVecNew();
            vecPush(pinned, l->pinned_reg);
        }
    }
    if (ast->params) {
        for (u64 i = 0; i < ast->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast->params, i);
            if (!p || p->kind != AST_LVAR) continue;
            if (p->pinned_kind != LVAR_REG || !p->pinned_reg) continue;
            int seen = 0;
            if (pinned) {
                for (u64 k = 0; k < pinned->size; ++k) {
                    if ((AoStr *)pinned->entries[k] == p->pinned_reg) { seen = 1; break; }
                }
            }
            if (seen) continue;
            if (!pinned) pinned = irValueVecNew();
            vecPush(pinned, p->pinned_reg);
        }
    }
    return pinned;
}

/* ---------------- per-function selection ---------------- */

/* Select `fn` (already lowered, optimised and laid out) into MIR and
 * sink it: into `jit->enc` when `text` is NULL, as GNU-as text
 * otherwise. Non-zero when the encoder rejected a line. */
static int x86EmitFunction(HccJit *jit, IrFunction *fn, Ast *ast, AoStr *text) {
    int is_variadic = (ast->type && ast->type->has_var_args) || ast->has_var_args;
    /* A by-value struct param is unpacked into an rbp-relative slot by
     * the param prologue, so the frame must exist. */
    int has_struct_param = 0;
    if (ast->params) {
        for (u64 pi = 0; pi < ast->params->size; ++pi) {
            Ast *p = vecGet(Ast *, ast->params, pi);
            if (x86IsByvalStruct(p->type)) { has_struct_param = 1; break; }
        }
    }

    X86Ctx ctx = {0};
    ctx.jit = jit;
    ctx.cc = jit->cc;
    ctx.fn = fn;
    ctx.ast = ast;
    ctx.text = text;
    ctx.pinned = x86CollectPinned(ast);
    /* Leaf function with zero frame: no stack slots, no pinned regs to
     * save, no calls (so no rsp realignment) and no varargs area. Skip
     * the rbp prologue/epilogue. */
    ctx.omit_frame = (fn->stack_space == 0) &&
                     (ctx.pinned == NULL || ctx.pinned->size == 0) &&
                     !irFnHasCalls(fn) && (ast->loff == 0) &&
                     !is_variadic && !has_struct_param;

    char *fname = asmNormaliseFunctionName(jit->cc, ast->fname);
    ctx.mir = mir_fn_new(fname, TASM_ARCH_X86_64);

    if (text) {
        aoStrRemovePreviousChar(text, '\t');
        /* .p2align 4 = 16-byte aligned function entries. */
        aoStrCatFmt(text, ".text\n\t.p2align 4\n\t.globl %s\n", fname);
    }

    x86EmitPrologue(&ctx);
    x86EmitSysvParamPrologue(&ctx);

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
            x86EmitInstr(&ctx, in);
        }
    }
    setRelease(referenced);

    ELABEL(&ctx, hccJitEpilogueLocalNum(jit, fn));
    ctx.src_line = 0;
    x86EmitEpilogue(&ctx);

    int rc = x86Flush(&ctx, 1);
    mir_fn_free(ctx.mir);
    if (ctx.pinned) vecRelease(ctx.pinned);
    return rc ? -1 : 0;
}

/* The JIT / object-writer entry: lower, optimise, lay out, select. */
static int x86CompileFunction(HccJit *jit, Ast *ast, IrCtx *ir_ctx) {
    IrFunction *fn = irLowerFunction(ir_ctx, ast);
    irBasicFunctionOptimisations(fn);
    if (irInlineFunction(ir_ctx, fn)) irBasicFunctionOptimisations(fn);

    IrCgCtx cg = {0};
    cg.cc = jit->cc;
    cg.fn = fn;
    irFunctionPrepForCodeGen(&cg, fn, ast);
    return x86EmitFunction(jit, fn, ast, NULL);
}

/* The AOT text entry, called by asmGenerate once per function with the
 * IR already prepared. The selector needs a HccJit for its local-label
 * numbering and addressing mode; one is kept for the translation unit,
 * in RELOCATED mode and without loading any host library - nothing
 * here ever runs. */
void x86_64MirGenerateFunction(IrCgCtx *ctx, Ast *ast) {
    static HccJit *text_jit = NULL;
    static Cctrl *text_cc = NULL;
    if (!text_jit || text_cc != ctx->cc) {
        text_jit = hccJitNewFor(ctx->cc, x86_64JitBackend(), 0, 1);
        if (!text_jit) loggerPanic("x86_64: cannot create the text emitter\n");
        text_jit->addr_mode = HCC_ADDR_RELOCATED;
        text_cc = ctx->cc;
    }
    ctx->ast_fn = ast;
    if (x86EmitFunction(text_jit, ctx->fn, ast, ctx->buf) != 0) {
        loggerPanic("x86_64: failed to select `%s`\n", ast->fname->data);
    }
}

/* ---------------- backend vtable ---------------- */

static int x86TargetOk(enum CliTarget target) {
    return target == TARGET_X86_64_APPLE_DARWIN ||
           target == TARGET_X86_64_UNKNOWN_LINUX_GNU;
}

static const HccJitBackend x86_64_backend = {
    .name = "x86_64",
    .target_ok = x86TargetOk,
    .init_reg_pool = x86_64InitRegPool,
    .compile_function = x86CompileFunction,
};

HccJit *x86_64JitCompile(Cctrl *cc) {
    return hccJitCompile(cc, &x86_64_backend);
}

const HccJitBackend *x86_64JitBackend(void) {
    return &x86_64_backend;
}

/* ---------------- data section (AOT text) ---------------- */

void x86_64PasteDataSection(Cctrl *cc, AoStr *buf) {
    /* The selector's text is Intel-dialect (asm_line_format_gas_x86),
     * so the whole file is. */
    aoStrCatFmt(buf, ".intel_syntax noprefix\n");
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
        aoStrCatFmt(buf, ".globl %S\n\t", label);
        aoStrCatFmt(buf, ".zerofill __DATA,__common,%S,%i,%i\n\t",
                    label, size, (int)log2((double)size));
    } else {
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
    if (astIsUnOpKind(data, AST_UN_OP_ADDR_OF) && data->operand) {
        data = data->operand;
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
        listForEach(data->arrayinit) {
            wrote += x86_64DataInternal(cc, buf, (Ast *)it->value);
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
    if (setHasLen(seen_globals, varname->data, varname->len)) return;

    aoStrRemovePreviousChar(buf, '\t');
    setAdd(seen_globals, varname->data);

    if (declinit &&
        (declinit->kind == AST_ARRAY_INIT ||
         declinit->kind == AST_LITERAL ||
         declinit->kind == AST_STRING))
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
                aoStrCatFmt(buf,
                            "%S:\n\t.asciz \"%S\"\n\t",
                            label, declinit->sval);
            }
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
             * not .zerofill so the linker doesn't double-allocate.
             * Test the source name, not `label`: Mach-O has already
             * prefixed that with `_`, so matching on it never fired. */
            if (varname->len == 4 &&
                (!strncmp(varname->data, str_lit("argc")) ||
                 !strncmp(varname->data, str_lit("argv"))))
            {
                x86_64InitialiseEmptyGlobal(cc, buf, declvar, 0);
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
