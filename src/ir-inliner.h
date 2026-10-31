#ifndef IR_INLINER_H__
#define IR_INLINER_H__

#include "ir-types.h"

/* `-fno-inline`. Off means every call stays a call; the switch exists so
 * a misbehaving program can say whether inlining is implicated, the same
 * bisect role `-fno-reg-promote` plays for register homing. */
void irInlineSetEnabled(int enabled);

/* Inline eligible direct calls in `fn`, in place.
 *
 * Returns the number of calls replaced, so the caller can decide whether
 * re-running the optimisation passes is worth it - the spliced body is
 * raw and only pays off once folding has been over it again.
 *
 * `ctx` supplies the Cctrl the candidate lookup needs; nothing else about
 * it is read or written. */
int irInlineFunction(IrCtx *ctx, IrFunction *fn);

/* Drop the candidate cache. Called once at the end of compilation; the
 * cached IrFunctions themselves are arena-allocated, so this only frees
 * the maps. */
void irInlineRelease(void);

#endif
