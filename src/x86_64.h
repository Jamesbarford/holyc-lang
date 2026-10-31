#ifndef X86_64_H__
#define X86_64_H__

/* The x86_64 backend: one MIR selector (x86_64.c) drives every sink -
 * JIT bytes, the object writer, AT&T AOT text - plus the
 * target description and the file-format emission around function
 * bodies. Arch-independent machinery (state, globals, symbol
 * resolution, the runner) lives in jit-common.h.
 *
 * Always built: selection is host-independent; only EXECUTING the
 * bytes needs an x86_64 host, and main.c gates that. */

#include "aostr.h"
#include "ir-types.h"
#include "ir-regalloc.h"
#include "cctrl.h"
#include "jit-common.h"

/* Emit the 32-bit form of an integer op (`addl %ecx, %eax`)?
 *
 * The twin of a64OpIs32Bit, and the reasoning is the same on both
 * targets. For add/sub/mul and the bitwise ops the low 32 bits of the
 * result depend only on the low 32 bits of the operands, so the narrow
 * form computes the same value - and a 32-bit destination ZERO-EXTENDS
 * into the full 64-bit register on x86-64 exactly as a W-register write
 * does on aarch64, which makes the upper bits DEFINED rather than
 * unspecified.
 *
 * The width alone does NOT authorise this. A narrow value in a register
 * is otherwise kept SIGN-extended, so a 32-bit op standing on its own
 * turns a negative result positive: `int n = INT_MAX; n = n + 1;` would
 * leave 0x0000000080000000, right in its low half and positive to
 * anything reading all 64. What makes it safe is the canonicalising
 * IR_TRUNC that irNormaliseNarrowInt emits after the op - so the
 * permission travels on the instruction as IRCG_OP_NARROW, set at that
 * one place. A narrow IADD reached by a path that does not normalise
 * (`x += y`, RMW, GEP address arithmetic) keeps the 64-bit form.
 *
 * Both backends must agree, or AOT and JIT differ in the upper half. */
static inline int x86_64OpIs32Bit(IrInstr *i) {
    if (!i || !i->dst) return 0;
    if (!(i->flags & IRCG_OP_NARROW)) return 0;
    return irValueByteSize(i->dst) <= 4;
}

/* ---- SysV variadic frame layout ------------------------------------
 *
 * Shared by the AOT backend and the JIT, because they are describing
 * the same frame: <stdarg.h> reads the record these lay down, and a
 * disagreement between the two would be a silent ABI bug that only
 * shows up as wrong argument values under `-jit`.
 *
 * The register save area's SHAPE is fixed by the psABI - `va_arg`
 * indexes into it using the offsets it finds in the va_list:
 *
 *   [  0 ..  47]   rdi rsi rdx rcx r8 r9      (8 bytes each)
 *   [ 48 .. 175]   xmm0 .. xmm7               (16 bytes each)
 *
 * Both the save area and the va_list record sit BELOW every local
 * slot, so adding them leaves the offsets locals were assigned during
 * layout exactly as they were. */
#define X86_VA_SAVE_BYTES 176   /* 6 GP * 8  +  8 SSE * 16 */
#define X86_VA_LIST_BYTES 24    /* two u32 offsets + two pointers   */

/* The locals region, rounded the way both prologues round it -
 * everything below is measured from its far edge. */
static inline u32 x86_64VaLocalsBase(u32 total_stack) {
    return (total_stack + 15u) & ~15u;
}

/* Extra bytes a function that reads a variadic list reserves under its
 * locals. Rounded to 16 so rsp stays aligned for inner calls. */
static inline u32 x86_64VaExtraBytes(void) {
    return (X86_VA_SAVE_BYTES + X86_VA_LIST_BYTES + 15u) & ~15u;
}

static inline int x86_64VaSaveAreaOff(u32 total_stack) {
    return -(int)(x86_64VaLocalsBase(total_stack) + X86_VA_SAVE_BYTES);
}

/* The frame-resident va_list the prologue fills in. `va_start` COPIES
 * it rather than aliasing it, so it stays pristine and a second
 * `va_start` in the same function starts the walk over. */
static inline int x86_64VaListOff(u32 total_stack) {
    return -(int)(x86_64VaLocalsBase(total_stack) + X86_VA_SAVE_BYTES
                  + X86_VA_LIST_BYTES);
}

/* Does this function actually read its variadic list?
 *
 * Asking the IR rather than the signature is deliberate. HolyC's own
 * counted varargs (`auto ...`) are variadic too but hand the body an
 * argc/argv pair and never touch a save area, so keying off
 * `has_var_args` would put 200 bytes and fourteen stores into every
 * one of them. IR_VA_START is emitted only by `__builtin_va_region`,
 * which is only reachable through C's `va_start`. */
int x86_64FnUsesVaStart(IrFunction *fn);

/* Where the NAMED parameters stop, in each of the three places the
 * variadic list can continue from: the GP registers, the SSE ones, and
 * the incoming stack area. Mirrors the parameter prologue's own walk. */
void x86_64VaNamedCounts(Ast *ast, int *out_ngp, int *out_nsse,
                         int *out_overflow);

void x86_64InitRegPool(void);
void x86_64PasteDataSection(Cctrl *cc, AoStr *buf);
void x86_64PasteAsmBlocks(Cctrl *cc, AoStr *buf);
void x86_64GlobalVar(Cctrl *cc, Set *seen_globals, AoStr *buf, Ast *ast);

/* ---- the selector's entry points ---- */

/* Run the front-end -> IR -> x86_64 bytes -> RX mapping pipeline for
 * every function in `cc`. Returns NULL on error (errors are reported
 * via the same channels as the AOT path). Use hccJitRunMain /
 * hccJitLookup / hccJitFree on the result. */
HccJit *x86_64JitCompile(Cctrl *cc);

/* The backend vtable, for incremental compilation via hccJitNew /
 * hccJitCompileChunk (the REPL, the object writer). */
const HccJitBackend *x86_64JitBackend(void);

/* AOT body generation for asm.c: emit `ast` (already lowered and
 * prepped onto ctx->fn) as GNU-as text into ctx->buf, sharing the
 * selector with the JIT (see x86_64MirGenerateFunction). */
void x86_64MirGenerateFunction(IrCgCtx *ctx, Ast *ast);

#endif
