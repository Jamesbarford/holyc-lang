#include "ast.h"
#include "cctrl.h"
#include "containers.h"
#include "ir-peephole.h"
#include "ir-regalloc.h"
#include "ir-types.h"
#include "list.h"

/* Count tmp uses across the whole function. The keys are tmp var.id,
 * values are u64 counts (encoded into the void* pointer). Sources:
 *   - r1 / r2 of every instruction;
 *   - dst of IR_BR / IR_RET / IR_STORE_DEREF (those treat dst as a
 *     read source);
 *   - the args wrapper of an IR_CALL (one count per arg);
 *   - the value field of every phi pair.
 * Phi.dst is treated as a definition; phi pair values are uses of
 * whatever produced them. */
static void irPeepholeBumpUse(Map *uses, IrValue *v) {
    if (!v || v->kind != IR_VAL_TMP) return;
    int n = mapHasInt(uses, v->as.var.id)
            ? (int)(u64)mapGetInt(uses, v->as.var.id) : 0;
    mapAdd(uses, (void *)(u64)v->as.var.id, (void *)(u64)(n + 1));
}

static int irPeepholeUseCount(Map *uses, IrValue *v) {
    if (!v || v->kind != IR_VAL_TMP) return 0;
    if (!mapHasInt(uses, v->as.var.id)) return 0;
    return (int)(u64)mapGetInt(uses, v->as.var.id);
}

static void irPeepholeCollectUses(IrFunction *func, Map *uses) {
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 i = 0; i < args->size; ++i) {
                    irPeepholeBumpUse(uses, (IrValue *)args->entries[i]);
                }
            } else {
                irPeepholeBumpUse(uses, I->r1);
            }
            irPeepholeBumpUse(uses, I->r2);
            if (I->op == IR_BR || I->op == IR_RET ||
                I->op == IR_STORE_DEREF) {
                irPeepholeBumpUse(uses, I->dst);
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                    IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                    irPeepholeBumpUse(uses, p->ir_value);
                }
            }
        }
    }
}

/* Pattern: an IR_ICMP / IR_FCMP whose dst tmp is consumed by the very
 * next non-NOP instruction (an IR_BR) and by no one else. Lets the
 * codegen turn `cmp; setcc; movzbq; testq; j[cc]` into a single
 * `cmp; j[cc]`. */
static void irPeepholeFuseCmpBr(IrFunction *fn, Map *uses) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (listEmpty(bb->instructions)) continue;
        listForEach(bb->instructions) {
            IrInstr *cmp = (IrInstr *)it->value;
            if (cmp->op != IR_ICMP && cmp->op != IR_FCMP) continue;
            if (!cmp->dst || cmp->dst->kind != IR_VAL_TMP) continue;
            if (irPeepholeUseCount(uses, cmp->dst) != 1) continue;
            /* Find the next non-NOP instruction in the same block. */
            List *next_node = it->next;
            while (next_node != bb->instructions) {
                IrInstr *cand = (IrInstr *)next_node->value;
                if (cand->op != IR_NOP) break;
                next_node = next_node->next;
            }
            if (next_node == bb->instructions) continue;
            IrInstr *br = (IrInstr *)next_node->value;
            if (br->op != IR_BR) continue;
            if (br->dst != cmp->dst) continue;
            cmp->flags |= IRCG_CMP_FUSED_BR;
            br->flags  |= IRCG_BR_USE_PRIOR_CMP;
        }
    }
}

/* Pattern: a block ends in `IR_JMP` to a target whose
 * head has a phi consuming the block's last value-producing tmp and
 * nothing else uses that tmp. Without this, the codegen would emit:
 *
 *     <op>     %t, ...           ; result reg -> %t's slot (spill)
 *     <load>   <t-slot> -> reg   ; phi-mat reload back into the reg
 *     <store>  reg -> <phi-slot>
 *
 * The reload is dead: the result register still holds the value the
 * spill just wrote out. Marking the producing instr with
 * IRCG_FUSE_TO_NEXT tells the emitter to skip the spill;
 * `irCgEmitOnePhi` then notices the flag on the predecessor's last
 * def and skips the matching reload too. */
static void irPeepholeFuseTailValueToPhi(IrFunction *func, Map *uses) {
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (listEmpty(bb->instructions))
            continue;

        /* Terminator: must be a plain unconditional jump (BR has its
         * cmp emitted inline so the in register lifetime window is
         * narrower - skip for now). */
        IrInstr *term = (IrInstr *)bb->instructions->prev->value;
        if (!term || (term->op != IR_JMP)) continue;
        IrBlock *target = term->extra.blocks.target_block;
        if (!target) continue;

        /* Only fuse when the target has exactly one phi at its head:
         * with multiple phis the dependency-driven phi-mat scheduler
         * in irCgEmitPhiMaterialize can emit our fused phi *after*
         * another one whose load clobbers the result register. The
         * single-phi guarantee is enough for the loop-counter case in
         * `loop.HC` and the same shape in eligibility-cleared library
         * code. */
        int target_phi_count = 0;
        listForEach(target->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (I->op != IR_PHI) break;
            target_phi_count++;
        }
        if (target_phi_count != 1) continue;

        /* Last value-producing non-NOP, non-terminator instruction in
         * the block. */
        IrInstr *last_def = NULL;
        /* Can't use the macro here as we are iterating backwards */
        for (List *node = bb->instructions->prev;
             node != bb->instructions;
             node = node->prev)
        {
            IrInstr *I = (IrInstr *)node->value;
            if (I->op == IR_NOP) continue;
            if (I->op == IR_JMP || I->op == IR_BR || I->op == IR_RET) continue;
            if (irInstrDefsIntoReg(I)) last_def = I;
            break;
        }

        if (!last_def)
            continue;
        if (!last_def->dst || last_def->dst->kind != IR_VAL_TMP)
            continue;
        if (irPeepholeUseCount(uses, last_def->dst) != 1)
            continue;

        /* Find a phi at `target`'s head whose pair from `bb` is the
         * tmp we just identified. */
        int found = 0;
        listForEach(target->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP)
                continue;
            if (I->op != IR_PHI)
                break;
            if (!I->extra.phi_pairs)
                continue;
            for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                if (p->ir_block == bb &&
                    p->ir_value &&
                    p->ir_value->kind == IR_VAL_TMP &&
                    p->ir_value->as.var.id == last_def->dst->as.var.id)
                {
                    found = 1;
                    break;
                }
            }
            if (found)
                break;
        }
        if (!found)
            continue;

        last_def->flags |= IRCG_FUSE_TO_NEXT;
    }
}

/* Pattern: `IR_STORE_DEREF` whose `dst` tmp comes from the
 * immediately-prior rax-defining instruction (typically IR_IADD that
 * computed `&p->field`, but works for any rax-def) and is single-use.
 * Without this fusion the codegen emits:
 *
 *     <op>     %dst, ...        ; result in result reg, then spill
 *     <load>   slot -> rax       ; reload r1 (clobbers reg)
 *     <load>   slot -> rcx       ; reload dst into scratch
 *     <store>  rax -> (rcx)
 *
 * After fusion: skip the spill (FUSE_TO_NEXT on the def), and have
 * STORE_DEREF stash the live result reg into the scratch reg before
 * its r1 load (`movq %rax, %rcx`, then load r1, then store). One
 * spill + one reload disappear; the address tmp's slot vanishes too. */
static void irPeepholeFuseAddrIntoStoreDeref(IrFunction *func, Map *uses) {
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (listEmpty(bb->instructions)) continue;
        listForEach(bb->instructions) {
            IrInstr *cur = (IrInstr *)it->value;
            if (cur->op != IR_STORE_DEREF) continue;
            if (!cur->dst || cur->dst->kind != IR_VAL_TMP) continue;
            if (irPeepholeUseCount(uses, cur->dst) != 1) continue;
            /* If the existing FUSE_TO_NEXT/R1_IN_REG annotator already
             * paired the prior instr with this STORE_DEREF as r1, leave
             * it alone - both fusions can't co-exist (they want the
             * single rax-def's value in different registers). */
            if (cur->flags & IRCG_R1_IN_REG) continue;
            /* Walk back to the previous non-NOP, non-PHI instruction
             * in this block. */
            List *p = it->prev;
            while (p != bb->instructions) {
                IrInstr *cand = (IrInstr *)p->value;
                if (cand->op != IR_NOP && cand->op != IR_PHI) break;
                p = p->prev;
            }
            if (p == bb->instructions) continue;
            IrInstr *prev = (IrInstr *)p->value;
            if (!irInstrDefsIntoReg(prev)) continue;
            if (!prev->dst || prev->dst->kind != IR_VAL_TMP) continue;
            if (prev->dst->as.var.id != cur->dst->as.var.id) continue;
            prev->flags |= IRCG_FUSE_TO_NEXT;
            cur->flags  |= IRCG_DST_IN_REG;
        }
    }
}

/* Pattern: an IR_CALL whose last source-order argument is a single-use
 * tmp produced by the immediately-prior rax-defining instruction, AND
 * the callee is HolyC variadic so that arg lands on the stack and is
 * pushed FIRST (variadic args push in reverse). Without this fusion
 * we emit:
 *
 *     <op>     %dst, ...        ; result in rax, spill to slot
 *     subq     $8, %rsp           ; alignment pad (if needed)
 *     movq     <slot>, %rax       ; reload (this pair is the waste)
 *     pushq    %rax
 *
 * After: producer keeps rax (FUSE_TO_NEXT), the call emits the alignment
 * pad and `pushq %rax` directly. SysV-extern variadic (printf etc.) is
 * skipped here: arg-to-stack overflow only happens when the int / float
 * register pools are full, which depends on the arg sequence in
 * complicated ways - the existing arg-load loop handles those, but we
 * don't try to fuse them. */
static void irPeepholeFuseTailArgIntoCall(Cctrl *cc, IrFunction *fn, Map *uses) {
    if (!cc) return;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (listEmpty(bb->instructions)) continue;
        listForEach(bb->instructions) {
            IrInstr *call = (IrInstr *)it->value;
            if (call->op != IR_CALL)
                continue;
            if (!call->r1 || !call->r1->as.array.values)
                continue;

            AoStr *fname = call->r1->as.array.label;
            if (!fname)
              continue;  /* indirect call - no callee info */

            Ast *callee = (Ast *)mapGetLen(cc->global_env,
                                            fname->data, fname->len);
            if (!callee)
                continue;
            /* SysV-extern variadic: arg-to-stack rules depend on the
             * full arg sequence; skip. */
            if (callee->kind == AST_EXTERN_FUNC)
                continue;

            int callee_va = 0;
            if ((callee->type && callee->type->has_var_args) ||
                callee->has_var_args) {
                callee_va = 1;
            } else if (callee->params && callee->params->size > 0) {
                Ast *last_p = vecGet(Ast *, callee->params,
                                     callee->params->size - 1);
                if (last_p && last_p->kind == AST_VAR_ARGS) callee_va = 1;
            }
            if (!callee_va)
                continue;

            /* var_arg_start computation: the count of fixed params + 1
             * (for the injected count arg). */
            int var_arg_start = -1;
            if (callee->params) {
                for (u64 i = 0; i < callee->params->size; ++i) {
                    Ast *p = vecGet(Ast *, callee->params, i);
                    var_arg_start++;
                    if (p->kind == AST_VAR_ARGS) break;
                }
                var_arg_start += 1;
            }

            Vec *args = call->r1->as.array.values;
            if ((s64)args->size <= var_arg_start) continue;

            IrValue *tail = vecGet(IrValue *, args, args->size - 1);
            if (!tail || tail->kind != IR_VAL_TMP) continue;
            /* Float variadic uses xmm0 + movsd; the rax-fusion path
             * doesn't apply. */
            if (irIsFloat(tail->type)) continue;
            if (irPeepholeUseCount(uses, tail) != 1) continue;

            List *p = it->prev;
            while (p != bb->instructions) {
                IrInstr *cand = (IrInstr *)p->value;
                if (cand->op != IR_NOP && cand->op != IR_PHI) break;
                p = p->prev;
            }
            if (p == bb->instructions) continue;
            IrInstr *prev = (IrInstr *)p->value;
            if (!irInstrDefsIntoReg(prev)) continue;
            if (!prev->dst || prev->dst->kind != IR_VAL_TMP) continue;
            if (prev->dst->as.var.id != tail->as.var.id) continue;

            prev->flags |= IRCG_FUSE_TO_NEXT;
            call->flags |= IRCG_CALL_TAIL_ARG_IN_REG;
        }
    }
}

/* Pattern: `IR_LEA` whose dst tmp is single-use and whose only use is
 * one argument of an `IR_CALL`. LEA values are pure (`rbp + const`
 * for locals or `RIP-relative const` for globals), so we can move the
 * leaq emit from the LEA's lexical position to the call site, with
 * the target arg register as the destination. Eliminates the slot
 * round-trip:
 *
 *     leaq   -16(%rbp), %rax    ; LEA emit
 *     movq   %rax, slot          ; spill
 *     ...
 *     movq   slot, %rsi          ; reload into arg reg
 *
 * After: the call's arg-load loop emits `leaq -16(%rbp), %rsi`, the
 * LEA emits nothing and reserves no slot.
 *
 * Skipped when the LEA is the tail arg of a HolyC-variadic call that
 * already has IRCG_CALL_TAIL_ARG_IN_REG: that fusion expects the
 * value in %rax going into the push and would conflict. The outcomes
 * are equivalent (3 instr / 0 slots either way) so first-fusion-wins
 * is fine. */
static void irPeepholeFuseLeaIntoCallArg(IrFunction *func, Map *uses) {
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *call = (IrInstr *)it->value;
            if (call->op != IR_CALL)
                continue;
            if (!call->r1 || !call->r1->as.array.values)
                continue;
            Vec *args = call->r1->as.array.values;

            int tail_already_fused =
                (call->flags & IRCG_CALL_TAIL_ARG_IN_REG) != 0;

            for (u64 i = 0; i < args->size; ++i) {
                IrValue *a = (IrValue *)args->entries[i];
                if (!a || a->kind != IR_VAL_TMP) continue;
                if (irPeepholeUseCount(uses, a) != 1) continue;
                if (tail_already_fused && i == args->size - 1) continue;

                /* Find the LEA producing this tmp. We don't constrain to
                 * the same block: LEA semantics are position-independent
                 * (rbp + const or RIP + const), and the slot allocator
                 * only reserves a slot if FUSE_TO_NEXT is unset, which
                 * we'll override with INLINE_AT_CALL anyway. */
                IrInstr *lea = NULL;
                for (List *bn = func->blocks->next;
                     bn != func->blocks && !lea;
                     bn = bn->next) {
                    IrBlock *b2 = (IrBlock *)bn->value;
                    for (List *in = b2->instructions->next;
                         in != b2->instructions;
                         in = in->next) {
                        IrInstr *I = (IrInstr *)in->value;
                        if (I->op != IR_LEA) continue;
                        if (!I->dst || I->dst->kind != IR_VAL_TMP) continue;
                        if (I->dst->as.var.id == a->as.var.id) {
                            lea = I;
                            break;
                        }
                    }
                }
                if (!lea) continue;
                lea->flags |= IRCG_LEA_INLINE_AT_CALL;
            }
        }
    }
}

void irPeephole(Cctrl *cc, IrFunction *func) {
    if (!func) return;
    Map *uses = mapNew(64, &map_uint_to_uint_type);
    irPeepholeCollectUses(func, uses);
    irPeepholeFuseCmpBr(func, uses);
    irPeepholeFuseTailValueToPhi(func, uses);
    irPeepholeFuseAddrIntoStoreDeref(func, uses);
    irPeepholeFuseTailArgIntoCall(cc, func, uses);
    irPeepholeFuseLeaIntoCallArg(func, uses);
    mapRelease(uses);
}
