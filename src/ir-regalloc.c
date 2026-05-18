#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "ast.h"
#include "containers.h"
#include "ir-regalloc.h"
#include "ir-types.h"
#include "list.h"
#include "prsutil.h"
#include "util.h"

void irCgSetLoff(IrRaCtx *ra, u32 var_id, int loff) {
    /* mapAddIntOrErr fails on duplicate; use it as an assert for distinctness. */
    int ok = mapAddIntOrErr(ra->id_to_loff, var_id, (void *)(u64)loff);
    if (!ok) {
        loggerPanic("ir-regalloc: duplicate loff mapping for var.id=%u\n", var_id);
    }
}

int irCgGetLoff(IrRaCtx *ra, IrValue *val) {
    assert(val != NULL);
    if (!mapHasInt(ra->id_to_loff, val->as.var.id)) {
        loggerPanic("ir-regalloc: no slot for var.id=%u kind=%d\n",
                    val->as.var.id, val->kind);
    }
    return (int)(u64)mapGetInt(ra->id_to_loff, val->as.var.id);
}

/* Reserve a fresh stack slot for an SSA temp. */
void irCgAllocTmp(IrRaCtx *ra, IrValue *val, int starting_offset) {
    if (!val || val->kind != IR_VAL_TMP) return;
    if (mapHasInt(ra->id_to_loff, val->as.var.id)) return;
    ra->extra_stack += 8;
    irCgSetLoff(ra, val->as.var.id, -(starting_offset + ra->extra_stack));
}

/* Reserve a fresh stack slot of a specific size (rounded up to 8-byte
 * alignment). Used by IR_ALLOCA so that a 256-byte alloca actually
 * gets 256 bytes of stack, not a default 8. */
static void irCgAllocTmpSized(IrRaCtx *ra, IrValue *val,
                               int starting_offset, int size_bytes) {
    if (!val || val->kind != IR_VAL_TMP) return;
    if (mapHasInt(ra->id_to_loff, val->as.var.id)) return;
    int aligned = (size_bytes + 7) & ~7;
    if (aligned < 8) aligned = 8;
    ra->extra_stack += aligned;
    irCgSetLoff(ra, val->as.var.id, -(starting_offset + ra->extra_stack));
}

void irCgAllocOperandsForInstr(IrRaCtx *ra, IrInstr *I, int start) {
    int spill_dst = !(I->flags & IRCG_FUSE_TO_NEXT);
    int load_r1 = !(I->flags & IRCG_R1_IN_REG);

    switch (I->op) {
    case IR_NOP:
    case IR_JMP:
        return;

    case IR_ALLOCA: {
        /* The alloca's r1 operand carries the byte size as a constant.
         * Honour it so multi-byte allocations (struct buffers,
         * CatchFrame storage, etc.) get the right amount of stack
         * rather than a default 8 bytes. */
        int alloca_bytes = 8;
        if (I->r1 && I->r1->kind == IR_VAL_CONST_INT) {
            alloca_bytes = (int)I->r1->as._i64;
        }
        irCgAllocTmpSized(ra, I->dst, start, alloca_bytes);
        return;
    }

    case IR_LOAD:
        if (spill_dst) irCgAllocTmp(ra, I->dst, start);
        irCgAllocTmp(ra, I->r1, start);
        return;

    case IR_STORE:
        irCgAllocTmp(ra, I->dst, start);
        if (load_r1) irCgAllocTmp(ra, I->r1, start);
        return;

    case IR_LOAD_DEREF:
        if (spill_dst) irCgAllocTmp(ra, I->dst, start);
        if (load_r1)   irCgAllocTmp(ra, I->r1, start);
        return;

    case IR_STORE_DEREF:
        /* Address slot only matters when STORE_DEREF will load it from
         * memory; if peephole fused with the prior rax-defining instr
         * (IRCG_DST_IN_REG), the address arrives via `movq %rax, %rcx`
         * with no slot involved. */
        if (!(I->flags & IRCG_DST_IN_REG)) {
            irCgAllocTmp(ra, I->dst, start);
        }
        if (load_r1) irCgAllocTmp(ra, I->r1, start);
        return;

    case IR_LEA:
        /* Inlined-at-call LEAs emit nothing here; the call site
         * re-creates the leaq into the target arg register. No slot. */
        if (I->flags & IRCG_LEA_INLINE_AT_CALL) return;
        if (spill_dst) irCgAllocTmp(ra, I->dst, start);
        return;

    case IR_IADD: case IR_ISUB: case IR_IMUL:
    case IR_AND:  case IR_OR:   case IR_XOR:
    case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
    case IR_SHL:  case IR_SHR:  case IR_SAR:
    case IR_ICMP:
        if (spill_dst) irCgAllocTmp(ra, I->dst, start);
        if (load_r1)   irCgAllocTmp(ra, I->r1, start);
        irCgAllocTmp(ra, I->r2, start);
        return;

    case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV:
        if (spill_dst) irCgAllocTmp(ra, I->dst, start);
        if (load_r1)   irCgAllocTmp(ra, I->r1, start);
        irCgAllocTmp(ra, I->r2, start);
        return;

    case IR_FCMP:
        /* FCMP's dst is the integer bool result; integer rules already
         * cover its spill. r1 is the float operand that loads to xmm0. */
        if (spill_dst) irCgAllocTmp(ra, I->dst, start);
        if (load_r1)   irCgAllocTmp(ra, I->r1, start);
        irCgAllocTmp(ra, I->r2, start);
        return;

    case IR_FNEG:
        if (spill_dst) irCgAllocTmp(ra, I->dst, start);
        if (load_r1)   irCgAllocTmp(ra, I->r1, start);
        return;

    case IR_BR:
        if (load_r1) irCgAllocTmp(ra, I->dst, start);
        return;

    case IR_RET:
        if (I->dst && load_r1) irCgAllocTmp(ra, I->dst, start);
        return;

    case IR_PHI:
        if (!(I->flags & IRCG_PHI_IN_REG)) {
            irCgAllocTmp(ra, I->dst, start);
        }
        return;

    case IR_CALL:
        if (spill_dst && I->dst && I->dst->type != IR_TYPE_VOID &&
            I->dst->kind == IR_VAL_TMP) {
            irCgAllocTmp(ra, I->dst, start);
        }
        irCgAllocTmp(ra, I->r2, start);
        return;

    case IR_GEP: {
        /* Get-element-pointer for stack-allocated structs. dst aliases
         * `base + offset`, we bind its loff right here so subsequent
         * loads/stores through it just look up the right frame offset. */
        if (!I->dst || I->dst->kind != IR_VAL_TMP ||
            !I->r1 || !I->r2 || I->r2->kind != IR_VAL_CONST_INT) {
            return;
        }
        int base_loff = irCgGetLoff(ra, I->r1);
        int field_loff = base_loff + (int)I->r2->as._i64;
        if (!mapHasInt(ra->id_to_loff, I->dst->as.var.id)) {
            irCgSetLoff(ra, I->dst->as.var.id, field_loff);
        }
        return;
    }

    default:
        irCgAllocTmp(ra, I->dst, start);
        irCgAllocTmp(ra, I->r1, start);
        irCgAllocTmp(ra, I->r2, start);
        break;
    }
}

void irCgAllocAllTmps(IrRaCtx *ra, int starting_offset) {
    listForEach(ra->func->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        listForEach(block->instructions) {
            IrInstr *instr = (IrInstr *)it->value;
            irCgAllocOperandsForInstr(ra, instr, starting_offset);
        }
    }
}

int irInstrDefsIntoReg(IrInstr *I) {
    if (I->op == IR_PHI && (I->flags & IRCG_PHI_IN_REG))
        return 1;
    int is_float = I->dst && irIsFloat(I->dst->type);
    switch (I->op) {
        /* Loads carry the value into the canonical result register for
         * the dst's type (rax for int, xmm0 for float). */
        case IR_LOAD:
        case IR_LOAD_DEREF:
            return 1;
        /* Integer-result-only ops: not eligible when dst is float. */
        case IR_LEA:
        case IR_IADD: case IR_ISUB: case IR_IMUL:
        case IR_AND:  case IR_OR:   case IR_XOR:
        case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
        case IR_SHL:  case IR_SHR:  case IR_SAR:
        case IR_ICMP:
            return !is_float;
        /* Float arith leaves the result in xmm0. */
        case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV:
        case IR_FNEG:
            return 1;
        case IR_CALL:
            return I->dst && I->dst->type != IR_TYPE_VOID;
        default:
            return 0;
    }
}

IrValue *irFirstFusableSource(IrInstr *I) {
    switch (I->op) {
        case IR_STORE:
        case IR_STORE_DEREF:
        case IR_LOAD_DEREF:
        case IR_IADD: case IR_ISUB: case IR_IMUL:
        case IR_AND:  case IR_OR:   case IR_XOR:
        case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
        case IR_SHL:  case IR_SHR:  case IR_SAR:
        case IR_ICMP:
        /* Float ops take r1 from xmm0; the consumer-side skip uses the
         * same R1_IN_REG flag, dispatched by the codegen on the value's
         * type. */
        case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV:
        case IR_FNEG: case IR_FCMP:
            return I->r1;
        case IR_BR:
        case IR_RET:
            return I->dst;
        default:
            return NULL;
    }
}

static void irBumpUseIfTmp(Map *uses, IrValue *v) {
    if (!v || v->kind != IR_VAL_TMP) return;
    int n = mapHasInt(uses, v->as.var.id)
            ? (int)(u64)mapGetInt(uses, v->as.var.id)
            : 0;
    mapAdd(uses, (void *)(u64)v->as.var.id, (void *)(u64)(n + 1));
}

void irCgAnnotate(IrFunction *fn) {
    Map *uses = mapNew(64, &map_uint_to_uint_type);

    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 i = 0; i < args->size; ++i) {
                    irBumpUseIfTmp(uses, (IrValue *)args->entries[i]);
                }
            } else {
                irBumpUseIfTmp(uses, I->r1);
            }
            irBumpUseIfTmp(uses, I->r2);
            if (I->op == IR_BR || I->op == IR_RET || I->op == IR_STORE_DEREF) {
                irBumpUseIfTmp(uses, I->dst);
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                    IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                    irBumpUseIfTmp(uses, p->ir_value);
                }
            }
            I->flags &= ~(u64)(IRCG_FUSE_TO_NEXT | IRCG_R1_IN_REG);
        }
    }

    /* Dead-code elimination: drop instructions whose dst tmp is
     * never read AND which have no observable side effects. Iterate
     * to a fixed point - dropping op A may make op B (which fed A)
     * itself unused. Each iteration recomputes the use-count map.
     *
     * Safely droppable ops (pure value producers):
     *   IR_LEA, IR_IADD, IR_ISUB, IR_IMUL,
     *   IR_AND, IR_OR, IR_XOR,
     *   IR_SHL, IR_SHR, IR_SAR,
     *   IR_INEG, IR_NOT,
     *   IR_ICMP, IR_FCMP,
     *   IR_FADD, IR_FSUB, IR_FMUL, IR_FDIV, IR_FNEG,
     *   IR_TRUNC, IR_ZEXT, IR_SEXT,
     *   IR_FPTRUNC, IR_FPEXT, IR_FPTOSI, IR_FPTOUI,
     *   IR_SITOFP, IR_UITOFP,
     *   IR_PTRTOINT, IR_INTTOPTR, IR_BITCAST
     *
     * Kept (side-effecting / control flow / storage):
     *   IR_STORE, IR_STORE_DEREF, IR_CALL, IR_BR/JMP/RET/SWITCH/LOOP,
     *   IR_PHI, IR_ALLOCA, IR_LOAD, IR_LOAD_DEREF (may fault),
     *   IR_IDIV/IR_UDIV/IR_IREM/IR_UREM (may divide by zero),
     *   IR_ASM, IR_VA_* */
    int changed = 1;
    while (changed) {
        changed = 0;
        listForEach(fn->blocks) {
            IrBlock *bb = (IrBlock *)it->value;
            listForEach(bb->instructions) {
                IrInstr *I = (IrInstr *)it->value;
                int safe;
                switch (I->op) {
                case IR_LEA:
                case IR_IADD: case IR_ISUB: case IR_IMUL:
                case IR_AND:  case IR_OR:   case IR_XOR:
                case IR_SHL:  case IR_SHR:  case IR_SAR:
                case IR_INEG: case IR_NOT:
                case IR_ICMP: case IR_FCMP:
                case IR_FADD: case IR_FSUB: case IR_FMUL:
                case IR_FDIV: case IR_FNEG:
                case IR_TRUNC: case IR_ZEXT: case IR_SEXT:
                case IR_FPTRUNC: case IR_FPEXT:
                case IR_FPTOSI: case IR_FPTOUI:
                case IR_SITOFP: case IR_UITOFP:
                case IR_PTRTOINT: case IR_INTTOPTR:
                case IR_BITCAST:
                    safe = 1; break;
                default:
                    safe = 0;
                }
                if (!safe) continue;
                if (!I->dst || I->dst->kind != IR_VAL_TMP) continue;
                int n = mapHasInt(uses, I->dst->as.var.id)
                        ? (int)(intptr_t)mapGetInt(uses,
                                                    I->dst->as.var.id)
                        : 0;
                if (n != 0) continue;
                I->op = IR_NOP;
                I->dst = NULL;
                I->r1 = NULL;
                I->r2 = NULL;
                changed = 1;
            }
        }
        if (!changed) break;
        /* Re-tally uses for the next iteration. */
        mapRelease(uses);
        uses = mapNew(64, &map_uint_to_uint_type);
        listForEach(fn->blocks) {
            IrBlock *bb = (IrBlock *)it->value;
            listForEach(bb->instructions) {
                IrInstr *I = (IrInstr *)it->value;
                if (I->op == IR_NOP) continue;
                if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                    Vec *args = I->r1->as.array.values;
                    for (u64 i = 0; i < args->size; ++i) {
                        irBumpUseIfTmp(uses, (IrValue *)args->entries[i]);
                    }
                } else {
                    irBumpUseIfTmp(uses, I->r1);
                }
                irBumpUseIfTmp(uses, I->r2);
                if (I->op == IR_BR || I->op == IR_RET ||
                    I->op == IR_STORE_DEREF) {
                    irBumpUseIfTmp(uses, I->dst);
                }
                if (I->op == IR_PHI && I->extra.phi_pairs) {
                    for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                        IrPair *p = vecGet(IrPair *,
                                            I->extra.phi_pairs, i);
                        irBumpUseIfTmp(uses, p->ir_value);
                    }
                }
            }
        }
    }

    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (listEmpty(bb->instructions)) continue;
        listForEach(bb->instructions) {
            IrInstr *cur = (IrInstr *)it->value;
            if (cur->op == IR_NOP) continue;
            if (!irInstrDefsIntoReg(cur)) continue;
            if (!cur->dst || cur->dst->kind != IR_VAL_TMP) continue;

            int use_count = mapHasInt(uses, cur->dst->as.var.id)
                            ? (int)(intptr_t)mapGetInt(uses, cur->dst->as.var.id)
                            : 0;
            if (use_count == 0) {
                cur->flags |= IRCG_FUSE_TO_NEXT;
                continue;
            }
            /* use_count >= 1: don't bail out for use_count > 1. The producer
             * still has to spill so the non-immediate uses can reload from
             * the slot, but the IMMEDIATE consumer can still skip its
             * reload-into-rax: the spill (movq %rax, slot) doesn't clobber
             * rax, and the inter-instruction NOP/JMP skip below also
             * preserves it. So we set FUSE_TO_NEXT (suppress spill) only
             * when use_count == 1, but always set R1_IN_REG on the matched
             * consumer regardless. */

            /* Find the next instruction that actually emits something.
             * Skip IR_NOPs left by mem2reg / fold. If we hit an IR_JMP
             * whose target has a single predecessor (this block) we
             * can chase into the target, the result register survives
             * an unconditional jump. We stop at IR_BR / IR_LOOP since
             * branch arms may need different register setups. */
            IrInstr *next = NULL;
            IrBlock *scan_bb = bb;
            List *scan = it->next;
            int hops = 0;
            while (1) {
                while (scan == scan_bb->instructions) {
                    IrInstr *term = (IrInstr *)listValue(IrInstr *,
                                                         listTail(scan_bb->instructions));
                    if (!term || term->op != IR_JMP) {
                        scan_bb = NULL;
                        break;
                    }

                    IrBlock *target = term->extra.blocks.target_block;
                    if (!target) {
                        scan_bb = NULL;
                        break;
                    }

                    Map *preds = irFunctionGetPredecessors(fn, target);
                    if (!preds || preds->size != 1) {
                        scan_bb = NULL;
                        break;
                    }
                    scan_bb = target;
                    scan = scan_bb->instructions->next;
                    if (++hops > 8) {
                        scan_bb = NULL;
                        break;
                    }
                }
                if (!scan_bb) break;
                IrInstr *cand = (IrInstr *)scan->value;
                if (cand->op != IR_NOP && cand->op != IR_JMP) {
                    next = cand;
                    break;
                }
                scan = scan->next;
            }
            if (!next)
                continue;

            IrValue *next_src = irFirstFusableSource(next);
            if (!next_src || next_src->kind != IR_VAL_TMP) continue;
            if (next_src->as.var.id != cur->dst->as.var.id) continue;

            if (use_count == 1) cur->flags |= IRCG_FUSE_TO_NEXT;
            next->flags |= IRCG_R1_IN_REG;
        }
    }

    mapRelease(uses);
}

/* Classify each IR_PHI: register-resident (IRCG_PHI_IN_REG) if
 *   (a) it's the only phi at the block head,
 *   (b) every predecessor arrives via IR_JMP / IR_LOOP (so the result
 *       register survives - conditional branches may not preserve it),
 *   (c) the phi's dst has at most one use across the function, AND
 *   (d) that use is the very next non-NOP instruction in this block,
 *       as its first fusable source - so the value in the register is
 *       consumed before any arithmetic / call clobbers it.
 * All other phis fall back to slot-resident (store on the pred side,
 * load on the use side, just like an alloca). */
void irCgClassifyPhis(IrFunction *func) {
    Map *uses = mapNew(64, &map_uint_to_uint_type);
    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 i = 0; i < args->size; ++i) {
                    irBumpUseIfTmp(uses, (IrValue *)args->entries[i]);
                }
            } else {
                irBumpUseIfTmp(uses, I->r1);
            }
            irBumpUseIfTmp(uses, I->r2);
            if (I->op == IR_BR || I->op == IR_RET ||
                I->op == IR_STORE_DEREF) {
                irBumpUseIfTmp(uses, I->dst);
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                    IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                    irBumpUseIfTmp(uses, p->ir_value);
                }
            }
        }
    }

    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (listEmpty(bb->instructions)) continue;

        int phi_count = 0;
        IrInstr *the_phi = NULL;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_PHI) {
                phi_count++;
                the_phi = I;
                continue;
            }
            if (I->op == IR_NOP)
                continue;
            break;
        }
        if (phi_count != 1)
            continue;

        Map *preds = irFunctionGetPredecessors(func, bb);
        if (!preds || preds->size == 0) continue;

        int all_jmp = 1;
        MapIter iter;
        mapIterInit(preds, &iter);
        while (mapIterNext(&iter)) {
            IrBlock *pb = (IrBlock *)iter.node->value;
            IrInstr *term = (IrInstr *)listValue(IrInstr *,
                                                 listTail(pb->instructions));
            if (!term || (term->op != IR_JMP)) {
                all_jmp = 0;
                break;
            }
        }
        if (!all_jmp) continue;

        if (!the_phi->dst || the_phi->dst->kind != IR_VAL_TMP) continue;
        if (irIsFloat(the_phi->dst->type)) continue;
        u32 dst_id = the_phi->dst->as.var.id;
        int dst_uses = mapHasInt(uses, dst_id)
                       ? (int)(u64)mapGetInt(uses, dst_id) : 0;
        if (dst_uses > 1) continue;

        IrInstr *next = NULL;
        int seen_phi = 0;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I == the_phi) { seen_phi = 1; continue; }
            if (!seen_phi) continue;
            if (I->op == IR_NOP) continue;
            if (I->op == IR_PHI) continue;
            next = I;
            break;
        }
        if (!next) continue;
        IrValue *next_src = irFirstFusableSource(next);
        if (!next_src || next_src->kind != IR_VAL_TMP)
            continue;
        if (next_src->as.var.id != dst_id)
            continue;

        the_phi->flags |= IRCG_PHI_IN_REG;
    }

    mapRelease(uses);
}

int irBlockHasPhi(IrBlock *bb) {
    if (!bb) return 0;
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_PHI) return 1;
        if (I->op != IR_NOP) return 0;
    }
    return 0;
}

int irPhiPairValueLiveInResultReg(IrBlock *from, IrValue *v) {
    if (!from || !v || v->kind != IR_VAL_TMP) return 0;
    if (listEmpty(from->instructions)) return 0;
    for (List *node = from->instructions->prev;
         node != from->instructions;
         node = node->prev) {
        IrInstr *I = (IrInstr *)node->value;
        if (I->op == IR_NOP)
            continue;
        if (I->op == IR_JMP ||
            I->op == IR_BR ||
            I->op == IR_RET)
            continue;
        if (!I->dst || I->dst->kind != IR_VAL_TMP)
            return 0;
        if (I->dst->as.var.id != v->as.var.id)
            return 0;
        return (I->flags & IRCG_FUSE_TO_NEXT) != 0;
    }
    return 0;
}

static IrInstr *irBlockTerminator(IrBlock *bb) {
    IrInstr *term = NULL;
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_NOP) continue;
        term = I;
    }
    return term;
}

/* We should be able to delete this as we already remove unused blocks */
Set *irCgComputeReferencedBlocks(IrFunction *fn) {
    Set *referenced = setNew(16, &set_uint_type);
    listForEach(fn->blocks) { 
        IrBlock *block = (IrBlock *)it->value;
        IrBlock *next_block = (it->next != fn->blocks)
                              ? (IrBlock *)it->next->value
                              : NULL;
        IrInstr *term = irBlockTerminator(block);
        if (!term)
            continue;

        switch (term->op) {
            case IR_JMP: {
                IrBlock *t = term->extra.blocks.target_block;
                if (t && t != next_block) {
                    setAdd(referenced, (void *)(u64)t->id);
                }
                break;
            }
            case IR_BR: {
                IrBlock *t = term->extra.blocks.target_block;
                IrBlock *f = term->extra.blocks.fallthrough_block;
                int t_phi = irBlockHasPhi(t);
                int f_phi = irBlockHasPhi(f);
                if (!t_phi && !f_phi) {
                    if (next_block == t) {
                        if (f) setAdd(referenced, (void *)(u64)f->id);
                    } else if (next_block == f) {
                        if (t) setAdd(referenced, (void *)(u64)t->id);
                    } else {
                        if (t) setAdd(referenced, (void *)(u64)t->id);
                        if (f) setAdd(referenced, (void *)(u64)f->id);
                    }
                } else {
                    if (t) setAdd(referenced, (void *)(u64)t->id);
                    if (f && next_block != f) {
                        setAdd(referenced, (void *)(u64)f->id);
                    }
                }
                break;
            }
            default:
                break;
            }
    }
    return referenced;
}

/* Bind every variable known to the IR (params + alloca'd locals) to its
 * AST-driven loff. Walk the AST function's params and locals lists; the
 * eligibility predicate guarantees they're plain AST_LVAR ints. */
void irCgBindAstLoffs(IrRaCtx *ra, Ast *ast_func) {
    /* Hidden out-pointer param for struct-by-value return: the layout
     * pass stashed its loff on `ast_func->loff` and the IR-side
     * IR_VAL_PARAM lives on `ra->func->return_value`. */
    AstType *rt = ast_func->type ? ast_func->type->rettype : NULL;
    int has_hidden_out_ptr = rt &&
        (rt->kind == AST_TYPE_CLASS || rt->kind == AST_TYPE_UNION) &&
        !rt->is_intrinsic && rt->size > 0;

    if (has_hidden_out_ptr && ra->func->return_value) {
        irCgSetLoff(ra, ra->func->return_value->as.var.id,
                    ast_func->loff);
    }

    if (ast_func->params) {
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            if (p->kind == AST_VAR_ARGS) {
                IrValue *cv = irFnGetVar(ra->func, p->argc->lvar_id);
                if (cv) irCgSetLoff(ra, cv->as.var.id, p->argc->loff);
                IrValue *vv = irFnGetVar(ra->func, p->argv->lvar_id);
                if (vv) irCgSetLoff(ra, vv->as.var.id, p->argv->loff);
                continue;
            }

            IrValue *iv = irFnGetVar(ra->func, irGetParamId(p));
            /* Pinned param: no slot. */
            if (iv && iv->pinned_reg) continue;
            if (iv) irCgSetLoff(ra, iv->as.var.id, p->loff);
        }
    }

    listForEach(ast_func->locals) {
        Ast *l = (Ast *)it->value;
        IrValue *iv = irFnGetVar(ra->func, irGetParamId(l));
        /* Register-pinned locals have no slot - the codegen reads /
         * writes the named register directly. */
        if (iv && iv->pinned_reg) continue;
        if (iv) irCgSetLoff(ra, iv->as.var.id, l->loff);
    }
}

int irCgComputeAstLayout(Ast *ast_func, IrFunction *ir_func) {
    Set *surviving = setNew(8, &set_uint_type);
    listForEach(ir_func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_ALLOCA && I->dst && I->dst->kind == IR_VAL_TMP) {
                setAdd(surviving, (void *)(u64)I->dst->as.var.id);
            }
        }
    }

    int total = 0;
    int new_offset = 0;

    listForEach(ast_func->locals) {
        Ast *l = (Ast *)it->value;
        int size;
        if (l->kind == AST_DEFAULT_PARAM) {
            size = l->declvar->type->size;
        } else if (l->kind == AST_FUNPTR) {
            size = 8;
        } else {
            size = l->type->size;
        }
        IrValue *iv = irFnGetVar(ir_func, irGetParamId(l));
        int promoted = iv && iv->kind == IR_VAL_TMP &&
                       !setHas(surviving, (void *)(u64)iv->as.var.id);
        if (promoted)
            continue;
        /* Register-pinned local: no stack slot, no loff. */
        if (iv && iv->pinned_reg)
            continue;
        total += align(size, 8);
        new_offset -= size;
        l->loff = new_offset;
    }

    int param_total = 0;
    AstType *rt = ast_func->type ? ast_func->type->rettype : NULL;
    int has_hidden_out_ptr = rt &&
        (rt->kind == AST_TYPE_CLASS || rt->kind == AST_TYPE_UNION) &&
        !rt->is_intrinsic && rt->size > 0;
    if (has_hidden_out_ptr) {
        param_total += 8;
    }
    if (ast_func->params) {
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            /* TempleOS-pinned param has no stack slot. */
            if (p->kind == AST_LVAR &&
                p->pinned_kind == LVAR_REG &&
                p->pinned_reg) continue;
            int sz;
            if (p->kind == AST_FUNPTR) {
                sz = 8;
            } else if (p->kind == AST_VAR_ARGS) {
                sz = 8;
            } else {
                sz = p->type->size;
            }
            param_total += align(sz, 8);
        }
    }
    total += param_total;
    int locals_aligned = total ? align(total, 16) : 0;

    int offset = locals_aligned;
    if (has_hidden_out_ptr) {
        /* Stash the loff on the AST function's `loff` field as a
         * side channel; both the prologue and irCgBindAstLoffs read
         * it back. AST_FUNC otherwise doesn't use this field. */
        ast_func->loff = -offset;
        offset -= 8;
    }
    if (ast_func->params) {
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            if (p->kind == AST_VAR_ARGS) {
                p->argc->loff = -offset;
                offset -= 8;
                p->argv->loff = 16;
                continue;
            }
            /* Pinned param: no slot, no loff. */
            if (p->kind == AST_LVAR &&
                p->pinned_kind == LVAR_REG &&
                p->pinned_reg) continue;
            int sz = (p->kind == AST_FUNPTR) ? 8 : p->type->size;
            p->loff = -offset;
            offset -= align(sz, 8);
        }
    }
    setRelease(surviving);
    return locals_aligned;
}
