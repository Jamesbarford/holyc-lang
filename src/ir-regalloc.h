#ifndef IR_REGALLOC_H
#define IR_REGALLOC_H

/* Slot- and register-allocation state for the IR backend.
 *
 * The "register allocator" here is intentionally tiny and target
 * agnostic: every SSA temp is assigned its own stack slot at a known
 * frame-pointer offset, and the per-architecture emitter reads/writes
 * through that slot. On top of that we run a few analysis passes that
 * mark instruction pairs as "value lives in the result register across
 * this pair" so the emitter can skip the obvious spill / reload round
 * trips. The flags below are the wire format between the passes here
 * and the architecture-specific emitter.
 *
 * What this module owns:
 *   - per-function slot-offset map (ast_lvar_id / IR var.id -> loff);
 *   - per-instruction slot allocation for IR temps;
 *   - AST-driven layout of params / locals (incl. struct-return
 *     hidden out-pointer, HolyC variadic argc/argv);
 *   - use-count + fusion-pair annotation;
 *   - phi register-residency classification;
 *   - generic IR analysis helpers (block has phi? referenced blocks?
 *     value byte size). */

#include "ast.h"
#include "containers.h"
#include "ir-types.h"

/* Codegen-private bits stored on IrInstr.flags. ir.c initialises flags
 * to 0 and no other code (ir-fold, ir-debug) reads or writes them, so
 * the regalloc + peephole passes own these bits exclusively. The
 * names speak in target-agnostic terms ("result register") - the
 * emitter maps that to whatever register file is canonical for the
 * arch (e.g. %rax/%xmm0 on x86_64). */

/* The instruction's result is consumed by the next non-NOP instruction
 * as its first source — emit nothing for the spill, the consumer knows
 * to read directly from the result register. */
#define IRCG_FUSE_TO_NEXT     (1u << 0)
/* Paired with IRCG_FUSE_TO_NEXT on the consumer: skip the load of the
 * first source - it's already in the result register from the prior
 * instruction. */
#define IRCG_R1_IN_REG        (1u << 1)
/* Set on an IR_PHI when the value can live in the result register
 * across the block boundary (the only phi at this block's head and all
 * predecessors arrive via IR_JMP, not IR_BR). When set, predecessors
 * materialise the incoming value into the register before their jmp;
 * the phi itself emits nothing; the slot allocator skips the phi's
 * dst. */
#define IRCG_PHI_IN_REG       (1u << 2)
/* Set by the peephole pass on an IR_ICMP / IR_FCMP whose result feeds
 * a single immediately-following IR_BR. The cmp emits the bare
 * compare (no setcc / movzbq / spill); the BR reads the cmp's
 * cmp_kind off the prior instruction and emits a direct conditional
 * jump. */
#define IRCG_CMP_FUSED_BR     (1u << 3)
#define IRCG_BR_USE_PRIOR_CMP (1u << 4)
/* Set on an IR_STORE_DEREF whose `dst` (address) tmp came from the
 * immediately-prior rax-defining instruction (single-use). The
 * producer kept its result in the result register (FUSE_TO_NEXT),
 * and the STORE_DEREF stashes that into the scratch register
 * (`movq %rax, %rcx` on x86) before loading r1 into the result
 * register and emitting the indirect store. Saves the slot spill /
 * reload that would otherwise sit between IADD and STORE_DEREF for
 * `*(p + offset) = value` patterns. */
#define IRCG_DST_IN_REG       (1u << 5)
/* Set on an IR_CALL whose last source-order argument is a single-use
 * tmp produced by the immediately-prior rax-defining instruction, AND
 * the callee is HolyC-variadic so that argument is the FIRST one
 * pushed onto the stack (variadic args push in reverse). The producer
 * keeps its result in the result register (FUSE_TO_NEXT); the call
 * emit skips the reload-into-rax for that first push and goes
 * straight to `pushq %rax`. */
#define IRCG_CALL_TAIL_ARG_IN_REG (1u << 6)
/* Set on an IR_LEA whose dst is a single-use tmp consumed only as one
 * argument of an IR_CALL. Address values from LEA are pure functions
 * of rbp (locals) or RIP (globals), so we can re-emit the leaq at the
 * call site directly into the target arg register, bypassing the
 * slot round-trip (`leaq -16(%rbp), %rax; movq %rax, slot;
 * movq slot, %rsi` becomes `leaq -16(%rbp), %rsi`). The original LEA
 * emits nothing and reserves no slot. */
#define IRCG_LEA_INLINE_AT_CALL (1u << 7)

/* The slot-offset map plus a counter for "extra" stack reserved beyond
 * what the AST layout already covers (used by IR-only allocas, e.g. the
 * struct-return hidden buffer at a call site). The arch-specific
 * codegen embeds this inside its own context. */
typedef struct IrRaCtx {
    IrFunction *func;
    /* Map<u32 IrValue.var.id -> int loff>. loff is sign-extended on
     * read; never zero for a real param/local (those start below the
     * frame base). */
    Map *id_to_loff;
    /* Bytes added to the stack pointer beyond the AST locals/params
     * region (for IR tmps and anything alloca'd at lowering time). The
     * layout pass reserves this in the function prologue. */
    int extra_stack;
} IrRaCtx;

/* Slot offset map. */
void irCgSetLoff(IrRaCtx *ra, u32 var_id, int loff);
int  irCgGetLoff(IrRaCtx *ra, IrValue *val);

/* Per-temp slot allocation. `irCgAllocTmp` reserves one slot for `val`;
 * `irCgAllocOperandsForInstr` does the per-opcode "which operands need
 * a slot" decision (consults FUSE_TO_NEXT / R1_IN_REG); the driver
 * `irCgAllocAllTmps` walks every block. */
void irCgAllocTmp(IrRaCtx *ra, IrValue *val, int starting_offset);
void irCgAllocOperandsForInstr(IrRaCtx *ra, IrInstr *I, int start);
void irCgAllocAllTmps(IrRaCtx *ra, int starting_offset);

/* AST-driven layout of params + locals. Returns the aligned bytes
 * the prologue must reserve. After it runs:
 *   - each AST param's `loff` field holds its slot offset
 *     (special cases for AST_VAR_ARGS, struct-return hidden out-ptr);
 *   - each AST local's `loff` field holds its slot offset, with
 *     mem2reg-promoted locals dropping their reservation;
 *   - `ast_func->loff` carries the hidden out-pointer slot for
 *     class-returning functions (consumed by the prologue). */
int  irCgComputeAstLayout(Ast *ast_func, IrFunction *ir_func);

/* Bind every AST-side loff into the regalloc map so the codegen can
 * look up an IR value's slot via `irCgGetLoff`. */
void irCgBindAstLoffs(IrRaCtx *ra, Ast *ast_func);

/* Annotation passes (mutate `IrInstr.flags`). */
void irCgClassifyPhis(IrFunction *func);
void irCgAnnotate(IrFunction *func);

/* Analysis helpers used by codegen and the peephole pass. */
int      blockHasPhi(IrBlock *bb);
/* Does this op produce its result naturally into the architecture's
 * canonical result register (i.e. is it a candidate for the
 * "leave in register" fusion path)? Float-typed defs and ops that
 * don't compute a value return 0. */
int      instrDefsIntoReg(IrInstr *I);
/* The single source operand the instruction loads into the result
 * register first. NULL when the op doesn't have one (alloca,
 * unconditional jmp, nop, ...). */
IrValue *firstFusableSource(IrInstr *I);
Set     *irCgComputeReferencedBlocks(IrFunction *func);
u32      irValueByteSize(IrValue *v);

#endif
