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
static void bumpUse(Map *uses, IrValue *v) {
    if (!v || v->kind != IR_VAL_TMP) return;
    int n = mapHasInt(uses, v->as.var.id)
            ? (int)(intptr_t)mapGetInt(uses, v->as.var.id) : 0;
    mapAdd(uses, (void *)(u64)v->as.var.id, (void *)(intptr_t)(n + 1));
}

static int useCount(Map *uses, IrValue *v) {
    if (!v || v->kind != IR_VAL_TMP) return 0;
    if (!mapHasInt(uses, v->as.var.id)) return 0;
    return (int)(intptr_t)mapGetInt(uses, v->as.var.id);
}

static void collectUses(IrFunction *func, Map *uses) {
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 i = 0; i < args->size; ++i) {
                    bumpUse(uses, (IrValue *)args->entries[i]);
                }
            } else {
                bumpUse(uses, I->r1);
            }
            bumpUse(uses, I->r2);
            if (I->op == IR_BR || I->op == IR_RET ||
                I->op == IR_STORE_DEREF) {
                bumpUse(uses, I->dst);
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                    IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                    bumpUse(uses, p->ir_value);
                }
            }
        }
    }
}

/* Pattern: an IR_ICMP / IR_FCMP whose dst tmp is consumed by the very
 * next non-NOP instruction (an IR_BR) - and by no one else. Lets the
 * codegen turn `cmp; setcc; movzbq; testq; j[cc]` into a single
 * `cmp; j[cc]`. */
static void fuseCmpBr(IrFunction *func, Map *uses) {
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (listEmpty(bb->instructions)) continue;
        for (List *node = bb->instructions->next;
             node != bb->instructions;
             node = node->next)
        {
            IrInstr *cmp = (IrInstr *)node->value;
            if (cmp->op != IR_ICMP && cmp->op != IR_FCMP) continue;
            if (!cmp->dst || cmp->dst->kind != IR_VAL_TMP) continue;
            if (useCount(uses, cmp->dst) != 1) continue;
            /* Find the next non-NOP instruction in the same block. */
            List *next_node = node->next;
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

/* Pattern: a block ends in `IR_JMP` (or `IR_LOOP`) to a target whose
 * head has a phi consuming the block's last value-producing tmp - and
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
static void fuseTailValueToPhi(IrFunction *func, Map *uses) {
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (listEmpty(bb->instructions)) continue;

        /* Terminator: must be a plain unconditional jump (BR has its
         * cmp emitted inline so the in-register lifetime window is
         * narrower - skip for now). */
        IrInstr *term = (IrInstr *)bb->instructions->prev->value;
        if (!term || (term->op != IR_JMP && term->op != IR_LOOP)) continue;
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
        for (List *node = bb->instructions->prev;
             node != bb->instructions;
             node = node->prev)
        {
            IrInstr *I = (IrInstr *)node->value;
            if (I->op == IR_NOP) continue;
            if (I->op == IR_JMP || I->op == IR_BR ||
                I->op == IR_LOOP || I->op == IR_RET) continue;
            if (instrDefsIntoReg(I)) last_def = I;
            break;
        }
        if (!last_def) continue;
        if (!last_def->dst || last_def->dst->kind != IR_VAL_TMP) continue;
        if (useCount(uses, last_def->dst) != 1) continue;

        /* Find a phi at `target`'s head whose pair from `bb` is the
         * tmp we just identified. */
        int found = 0;
        listForEach(target->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (I->op != IR_PHI) break;
            if (!I->extra.phi_pairs) continue;
            for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                if (p->ir_block == bb &&
                    p->ir_value &&
                    p->ir_value->kind == IR_VAL_TMP &&
                    p->ir_value->as.var.id == last_def->dst->as.var.id) {
                    found = 1;
                    break;
                }
            }
            if (found) break;
        }
        if (!found) continue;

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
static void fuseAddrIntoStoreDeref(IrFunction *func, Map *uses) {
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (listEmpty(bb->instructions)) continue;
        for (List *node = bb->instructions->next;
             node != bb->instructions;
             node = node->next)
        {
            IrInstr *cur = (IrInstr *)node->value;
            if (cur->op != IR_STORE_DEREF) continue;
            if (!cur->dst || cur->dst->kind != IR_VAL_TMP) continue;
            if (useCount(uses, cur->dst) != 1) continue;
            /* If the existing FUSE_TO_NEXT/R1_IN_REG annotator already
             * paired the prior instr with this STORE_DEREF as r1, leave
             * it alone - both fusions can't co-exist (they want the
             * single rax-def's value in different registers). */
            if (cur->flags & IRCG_R1_IN_REG) continue;
            /* Walk back to the previous non-NOP, non-PHI instruction
             * in this block. */
            List *p = node->prev;
            while (p != bb->instructions) {
                IrInstr *cand = (IrInstr *)p->value;
                if (cand->op != IR_NOP && cand->op != IR_PHI) break;
                p = p->prev;
            }
            if (p == bb->instructions) continue;
            IrInstr *prev = (IrInstr *)p->value;
            if (!instrDefsIntoReg(prev)) continue;
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
static void fuseTailArgIntoCall(Cctrl *cc, IrFunction *func, Map *uses) {
    if (!cc) return;
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (listEmpty(bb->instructions)) continue;
        for (List *node = bb->instructions->next;
             node != bb->instructions;
             node = node->next)
        {
            IrInstr *call = (IrInstr *)node->value;
            if (call->op != IR_CALL) continue;
            if (!call->r1 || !call->r1->as.array.values) continue;

            AoStr *fname = call->r1->as.array.label;
            if (!fname) continue;  /* indirect call - no callee info */

            Ast *callee = (Ast *)mapGetLen(cc->global_env,
                                            fname->data, fname->len);
            if (!callee) continue;
            /* SysV-extern variadic: arg-to-stack rules depend on the
             * full arg sequence; skip. */
            if (callee->kind == AST_EXTERN_FUNC) continue;

            int callee_va = 0;
            if ((callee->type && callee->type->has_var_args) ||
                callee->has_var_args) {
                callee_va = 1;
            } else if (callee->params && callee->params->size > 0) {
                Ast *last_p = vecGet(Ast *, callee->params,
                                     callee->params->size - 1);
                if (last_p && last_p->kind == AST_VAR_ARGS) callee_va = 1;
            }
            if (!callee_va) continue;

            /* Mirror codegen-x86-ir.c's var_arg_start computation: the
             * count of fixed params + 1 (for the injected count arg). */
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
            if (useCount(uses, tail) != 1) continue;

            List *p = node->prev;
            while (p != bb->instructions) {
                IrInstr *cand = (IrInstr *)p->value;
                if (cand->op != IR_NOP && cand->op != IR_PHI) break;
                p = p->prev;
            }
            if (p == bb->instructions) continue;
            IrInstr *prev = (IrInstr *)p->value;
            if (!instrDefsIntoReg(prev)) continue;
            if (!prev->dst || prev->dst->kind != IR_VAL_TMP) continue;
            if (prev->dst->as.var.id != tail->as.var.id) continue;

            prev->flags |= IRCG_FUSE_TO_NEXT;
            call->flags |= IRCG_CALL_TAIL_ARG_IN_REG;
        }
    }
}

void irPeephole(Cctrl *cc, IrFunction *func) {
    if (!func) return;
    Map *uses = mapNew(64, &map_uint_to_uint_type);
    collectUses(func, uses);
    fuseCmpBr(func, uses);
    fuseTailValueToPhi(func, uses);
    fuseAddrIntoStoreDeref(func, uses);
    fuseTailArgIntoCall(cc, func, uses);
    mapRelease(uses);
}

