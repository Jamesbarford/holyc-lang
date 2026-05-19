#ifndef IR_OPTIMISE_H__
#define IR_OPTIMISE_H__

#include "ir-types.h"

/* Run the standard pipeline of backend-agnostic IR optimisation
 * passes on `fn`: block cleanup, global-deref folding, constant
 * eval, return-slot forwarding, store-to-reads forwarding, dead
 * store elimination, and the addressing-mode + RMW fusion chain. */
void irBasicFunctionOptimisations(IrFunction *fn);

/* Pin single-use TMP producers to their consumer's input register
 * and rewrite adjacent CMP+BR into a single IR_CMP_BR. */
void irOptPinResultReg(IrFunction *fn);

/* Iterate to fixed point dropping pure value-producers whose dst
 * tmp has zero uses. Pin unused IR_CALL result tmps to the result
 * reg so the slot allocator skips them. */
void irOptDeadCodeElim(IrFunction *fn);

#endif
