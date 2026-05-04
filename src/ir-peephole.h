#ifndef IR_PEEPHOLE_H__
#define IR_PEEPHOLE_H__

#include "cctrl.h"
#include "ir-types.h"

/* Run codegen-targeted peephole rewrites over `func`. Sets per-instr
 * codegen flags that the architecture specific emitter consumes:
 *
 * cmp + br fusion: a single-use IR_ICMP / IR_FCMP feeding an
 * immediately-following IR_BR collapses to one compare + one
 * conditional jump, instead of compare + setcc + movzbq + test +
 * conditional jump.
 *
 * tail value into phi: when a block's last value-producing instr
 * is single-use and consumed by a phi at the JMP target, the
 * spill to the def's slot is suppressed; the phi-materialiser
 * reads the value straight out of the result register instead of
 * reloading from the slot.
 *
 * addr-into-store-deref: an IADD / LOAD whose dst is the address
 * operand of an immediately-following IR_STORE_DEREF stays in the
 * result register; the store stashes it before loading the value.
 *
 * tail-arg-into-call-push: a single-use rax-def whose dst is the
 * last source-order argument of an immediately-following HolyC
 * variadic call stays in the result register; the call emits
 * `pushq %rax` for that first push without a reload. Needs
 * callee lookup (`cc`) to determine variadic-ness. */
void irPeephole(Cctrl *cc, IrFunction *func);


#endif
