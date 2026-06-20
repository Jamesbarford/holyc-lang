#ifndef IR_EVAL_H__
#define IR_EVAL_H__

#include "ir-types.h"

/* Evaluates constant integer operations for each block and propagates across
 * blocks. Very simple. Creates a bunck of Nops. Returns the number of
 * instructions it rewrote, so the optimiser can run it to a fixed point
 * alongside store->read forwarding. */
int irEvalConstantExpressions(IrFunction *fn);

#endif
