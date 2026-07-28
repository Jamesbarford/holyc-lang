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

/* Where in an instruction's emission a clobber question is asked.
 * Both backends materialise r1 into their first scratch register and
 * r2 into the second, so a forwarded value's survival depends on how
 * far through the instruction it has to stay alive. */
typedef enum IrClobberPoint {
    IR_CLOBBER_AFTER = 0,   /* after the whole instruction is emitted */
    IR_CLOBBER_BEFORE_R1,   /* before r1 is read */
    IR_CLOBBER_BEFORE_R2,   /* before r2 is read */
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

#endif
