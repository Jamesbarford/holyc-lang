#ifndef IR_PEEPHOLE_H
#define IR_PEEPHOLE_H

#include "ir-types.h"

/* Run codegen-targeted peephole rewrites over `func`. Sets per-instr
 * codegen flags (IRCG_FUSE_TO_NEXT, IRCG_CMP_FUSED_BR,
 * IRCG_BR_USE_PRIOR_CMP) that the architecture-specific emitter
 * consumes:
 *
 *   - cmp + br fusion: a single-use IR_ICMP / IR_FCMP feeding an
 *     immediately-following IR_BR collapses to one compare + one
 *     conditional jump, instead of compare + setcc + movzbq + test +
 *     conditional jump.
 *
 *   - tail value into phi: when a block's last value-producing instr
 *     is single-use and consumed by a phi at the JMP target, the
 *     spill to the def's slot is suppressed; the phi-materialiser
 *     reads the value straight out of the result register instead of
 *     reloading from the slot. */
void irPeephole(IrFunction *func);

#endif
