#ifndef IR_EVAL_H__
#define IR_EVAL_H__

#include "ir-types.h"

/* Evaluates constant integer operations for each block and propagates across
 * blocks. Very simple. Creates a bunck of Nops */
void irEvalConstantExpressions(IrFunction *fn);

#endif
