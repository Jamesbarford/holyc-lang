#ifndef IR_OPTIMISE_H__
#define IR_OPTIMISE_H__

#include "ir-types.h"

/* Run the standard pipeline of backend-agnostic IR optimisation
 * passes on `fn`: block cleanup, global-deref folding, constant
 * eval, return-slot forwarding, store-to-reads forwarding, dead
 * store elimination, and the addressing-mode + RMW fusion chain. */
void irBasicFunctionOptimisations(IrFunction *fn);
/* Indirect calls through a slot that only ever holds one function become
 * direct calls to it. Returns how many were rewritten. */
int irDevirtualiseCalls(IrFunction *fn);
/* `-O`. Off by default: devirtualisation and whatever builds on it are
 * the optimisations a program opts into. */
void irOptSetDevirtualise(int enabled);
/* `-O`: store-to-load forwarding, `*&x` to `x`, and dead stores, on
 * locals whose address does not escape. */
int irOptLocalMemory(IrFunction *fn);


/* Pin single-use TMP producers to their consumer's input register
 * and rewrite adjacent CMP+BR into a single IR_CMP_BR. */
void irOptPinResultReg(IrFunction *fn);
/* Drop the widening read of a narrow local that promotion moved into a
 * register - its home is already canonical. Must run AFTER
 * irOptPromoteToRegs. */
int irElideCanonicalWiden(IrFunction *fn);

/* Iterate to fixed point dropping pure value-producers whose dst
 * tmp has zero uses. Pin unused IR_CALL result tmps to the result
 * reg so the slot allocator skips them. */
void irOptDeadCodeElim(IrFunction *fn);

#endif
