#ifndef IR_FOLD_H__
#define IR_FOLD_H__

#include "ir-types.h"

/* Fold pure-constant integer binops within each basic block and propagate
 * the resulting constant into single-use successors (any later instruction
 * in the same block that names the folded tmp). Mutates the function in
 * place: folded instructions become IR_NOP, operand pointers may be
 * rewritten to constant IrValues. */
void irFoldFunction(IrFunction *func);

#endif
