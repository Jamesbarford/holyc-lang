#ifndef IR_REGALLOC_H__
#define IR_REGALLOC_H__

#include "ast.h"
#include "containers.h"
#include "ir-types.h"

/* Codegen-private bits stored on IrInstr.flags. */

/* Builtin compiler intrinsic, the backend handles it directly. */
#define IRCG_FN_BUILTIN (1u << 0)

/* Set on an IR_CALL whose first source-order argument is the hidden
 * out-pointer for a struct/union by-value return. On x86_64 the
 * pointer naturally goes in %rdi via the standard arg-slot mapping,
 * so this flag is informational. */
#define IRCG_CALL_AGG_RETURN (1u << 1)

/* Set on an IR_TRUNC whose destination type is SIGNED. Truncating to a
 * narrower width still leaves the value in a 64-bit register, so the
 * high bits have to be filled correctly or any later 64-bit compare
 * reads a different number: `(int)(3L - 5L)` zero-extended reads as
 * +4294967294 and tests greater-than-zero. Unsigned destinations keep
 * the zero-extending forms. */
#define IRCG_TRUNC_SIGNED    (1u << 2)

/* Set on an IR_SEXT whose operand a 4-byte load already sign-extended.
 * Every 4-byte load on this target widens to 64 bits (`ldrsw`/`ldursw`,
 * matching x86's `movslq`), so the widen that follows it re-does work
 * already done. The emitter cannot see the producer, so an IR pass
 * marks it - the same arrangement IRCG_TRUNC_SIGNED uses to carry
 * signedness the value type does not hold. */
#define IRCG_SEXT_NOOP       (1u << 3)

/* Set on an instruction whose destination is dead immediately after it.
 * The instruction still runs - it may be a call - but writing the
 * result to the destination's home is pointless. Only honoured for a
 * REGISTER home: a dead store to a stack SLOT is skipped too readily,
 * because parts of the emitter read a slot the IR never mentions (the
 * struct-return out-pointer being the case that bit). */
#define IRCG_DST_DEAD        (1u << 4)

/* One omission described from both ends: a call whose register-returned
 * aggregate IS this function's return value, followed immediately by
 * the `ret` that returns it. The callee has already left the bytes in
 * the very registers the epilogue wants, so the buffer store and the
 * reload that follows it are a round trip to nowhere.
 *
 * IRCG_AGG_RET_FORWARD sits on the IR_CALL and says "leave the result
 * registers alone"; IRCG_RET_IN_REGS sits on the IR_RET and says "they
 * already hold the value, emit only the epilogue". They are set and
 * cleared together by irForwardAggReturn - one without the other
 * either returns garbage or drops the value on the floor, so neither
 * backend may honour one alone. */
#define IRCG_AGG_RET_FORWARD (1u << 5)
#define IRCG_RET_IN_REGS     (1u << 6)

/* Where in an instruction's emission a clobber question is asked.
 * Both backends materialise r1 into their first scratch register and
 * r2 into the second, so a forwarded value's survival depends on how
 * far through the instruction it has to stay alive. */
typedef enum IrClobberPoint {
    IR_CLOBBER_AFTER = 0,   /* after the whole instruction is emitted */
    IR_CLOBBER_BEFORE_R1,   /* before r1 is read */
    IR_CLOBBER_BEFORE_R2,   /* before r2 is read */
    /* Before `dst` is read AS AN ADDRESS - the deref stores only.
     * Their dst is an input, and it is the FIRST thing materialised,
     * so nothing in the instruction has run yet. Distinct from
     * BEFORE_R1, which for those ops is the last thing. */
    IR_CLOBBER_BEFORE_DST,
} IrClobberPoint;

/* Per-backend ABI descriptor. The peephole reads this to know which
 * register a fused producer's value will live in (the result reg);
 * the codegen reads it when laying out call arguments and parameter
 * spills. A new backend slots in by providing one. */
typedef struct IrRegPool {
    /* ABI integer argument registers, in argument order. Vec<AoStr *>. */
    Vec *int_arg_regs;
    /* ABI float argument registers, in argument order. Vec<AoStr *>. */
    Vec *float_arg_regs;
    /* Integer return register (rax on SysV, x0 on AAPCS64). */
    AoStr *int_return_reg;
    /* Indirect struct-return pointer register, when the ABI dedicates
     * one outside the argument sequence (AAPCS64: x8). NULL means the
     * pointer is passed as the first integer argument and consumes
     * that arg slot (x86-64 SysV: rdi). */
    AoStr *sret_reg;
    /* Float return register (xmm0 / v0). */
    AoStr *float_return_reg;
    /* Per-instruction scratch surface: registers the codegen clobbers
     * while emitting an arithmetic/load/store/cast. Forward-store
     * passes drop any forwarded source whose loc names one of these
     * after a non-trivial op. Vec<AoStr *>. */
    Vec *scratch_regs;
    /* Does emitting `I` write `reg` by the point `at`? Lets the
     * forwarding passes ask about the instruction actually crossed
     * rather than assuming every op burns every scratch register -
     * without that distinction a param arriving in a register that is
     * also scratch (x0-x2 on AAPCS64, rdx/rcx on SysV) can never be
     * forwarded, so its home slot stays live and forces a frame.
     *
     * `reg` is always one of `scratch_regs`; non-scratch registers are
     * never at risk and are filtered out before the call. NULL here
     * means "assume every op clobbers every scratch reg", which is the
     * conservative behaviour a backend gets for free. */
    int (*op_clobbers)(IrInstr *I, AoStr *reg, IrClobberPoint at);
    /* Registers the ABI requires a function to preserve across a call.
     * A `<Type> reg <REG> name` binding to one of these has to be saved
     * and restored, or the function silently destroys a value its
     * caller was relying on. Vec<AoStr *>, lowercase. */
    Vec *callee_saved_regs;
    /* Caller-saved registers that no part of codegen reserves for
     * itself. Usable without save/restore in a function that makes no
     * calls, where nothing can clobber them - which is most leaf
     * functions. Empty is always safe: the allocator just falls back to
     * the callee-saved pool and pays the save. Vec<AoStr *>. */
    Vec *leaf_free_regs;
    /* Apple AArch64 ABI: variadic args (incl. HolyC's implicit argc)
     * are passed on the stack, not in arg regs. The IR builder and
     * layout pass consult this to skip the argc spill and place argc
     * at the start of the caller's variadic stack region. */
    int variadic_on_stack;
} IrRegPool;

void irRegPoolSet(IrRegPool *pool);
IrRegPool *irRegPoolGet(void);

/* Slot offset map. */
void irCgSetLoff(IrRaCtx *ra, u32 var_id, int loff);
int  irCgGetLoff(IrRaCtx *ra, IrValue *val);

/* Per-temp slot allocation. `irCgAllocTmp` reserves one slot for `val`;
 * `irCgAllocOperandsForInstr` decides per-opcode which operands need
 * a slot; `irCgAllocAllTmps` is the linear-scan driver. */
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
int irCgComputeAstLayout(Ast *ast_func, IrFunction *ir_func);

/* Bind every AST-side loff into the regalloc map so the codegen can
 * look up an IR value's slot via `irCgGetLoff`. */
void irCgBindAstLoffs(IrRaCtx *ra, Ast *ast_func);

/* True if `bb` starts with at least one IR_PHI (skipping IR_NOPs).
 * Used by the codegen to decide whether a branch arm needs phi
 * materialisation before jumping in. */
int irBlockHasPhi(IrBlock *bb);

Set *irCgComputeReferencedBlocks(IrFunction *func);
u32 irValueByteSize(IrValue *v);

/* True if the function body contains at least one IR_CALL. The x86_64
 * frame-elision check uses this to know whether it must keep an rbp
 * prologue (calls require rsp to be 16-byte aligned at the call
 * point). Linear in function size; codegen-driver only. */
int irFnHasCalls(IrFunction *fn);

/* The callee-saved registers this function pins, in pool order and
 * without duplicates. Vec<AoStr *> borrowed from the pool; caller frees
 * the vector, not the strings. Empty when nothing is pinned. */
Vec *irCgPinnedCalleeSaved(IrFunction *fn);

/* Bind hot address-free locals and params to callee-saved registers by
 * setting `pinned_reg`, the same field a source-level `reg <REG>` uses.
 * No-op when the target has no callee-saved pool, when the function
 * contains an `asm { }` block, or under -fno-reg-promote. */
void irOptPromoteToRegs(IrFunction *fn);
void irRegPromoteSetEnabled(int enabled);

#endif
