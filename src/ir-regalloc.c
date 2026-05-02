#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "ast.h"
#include "containers.h"
#include "ir.h"
#include "ir-regalloc.h"
#include "ir-types.h"
#include "list.h"
#include "prsutil.h"
#include "util.h"

/* ir.c internals not in ir.h. */
extern IrValue *irFnGetVar(IrFunction *func, u32 lvar_id);

/* ---------- slot offset map -------------------------------------------- */

void irCgSetLoff(IrRaCtx *ra, u32 var_id, int loff) {
    /* mapAddIntOrErr fails on duplicate; use it as an assert for distinctness. */
    int ok = mapAddIntOrErr(ra->id_to_loff, var_id, (void *)(intptr_t)loff);
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
    return (int)(intptr_t)mapGetInt(ra->id_to_loff, val->as.var.id);
}

/* ---------- per-instruction tmp slot allocation ------------------------ */

/* Reserve a fresh stack slot for an SSA temp. */
void irCgAllocTmp(IrRaCtx *ra, IrValue *val, int starting_offset) {
    if (!val || val->kind != IR_VAL_TMP) return;
    if (mapHasInt(ra->id_to_loff, val->as.var.id)) return;
    ra->extra_stack += 8;
    irCgSetLoff(ra, val->as.var.id, -(starting_offset + ra->extra_stack));
}

/* Allocate slots only for tmps the emitter is actually going to touch.
 * Operand roles per opcode:
 *   - "spill dst"    (write result reg to slot)  - skip on FUSE_TO_NEXT.
 *   - "load r1"      (read slot to result reg)   - skip on R1_IN_REG.
 *   - "address dst"  (offset literal)            - always needs a slot.
 *   - "load r2"      (read slot to scratch reg)  - always needs a slot.
 * Fused (dst, r1) pairs are never spilled or read, so their tmp ids
 * legitimately have no slot. */
void irCgAllocOperandsForInstr(IrRaCtx *ra, IrInstr *I, int start) {
    int spill_dst = !(I->flags & IRCG_FUSE_TO_NEXT);
    int load_r1   = !(I->flags & IRCG_R1_IN_REG);

    switch (I->op) {
    case IR_NOP:
    case IR_JMP:
    case IR_LOOP:
        return;

    case IR_ALLOCA:
        irCgAllocTmp(ra, I->dst, start);
        return;

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
        irCgAllocTmp(ra, I->r1, start);
        return;

    case IR_STORE_DEREF:
        irCgAllocTmp(ra, I->dst, start);
        irCgAllocTmp(ra, I->r1, start);
        return;

    case IR_LEA:
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
    case IR_FCMP:
        irCgAllocTmp(ra, I->dst, start);
        irCgAllocTmp(ra, I->r1, start);
        irCgAllocTmp(ra, I->r2, start);
        return;

    case IR_FNEG:
        irCgAllocTmp(ra, I->dst, start);
        irCgAllocTmp(ra, I->r1, start);
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
         * `base + offset` - we bind its loff right here so subsequent
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

/* ---------- value byte size ------------------------------------------- */

/* Width in bytes the codegen should use when reading/writing an
 * IrValue. Tmps carry an explicit `var.size`; other kinds fall back to
 * the IR type tag (pointer/F64/I64 = 8, I32 = 4, I16 = 2, I8/CHAR = 1). */
u32 irValueByteSize(IrValue *v) {
    if (!v) return 8;
    if (v->kind == IR_VAL_TMP || v->kind == IR_VAL_LOCAL ||
        v->kind == IR_VAL_PARAM) {
        if (v->as.var.size > 0) return v->as.var.size;
    }
    switch (v->type) {
    case IR_TYPE_I8:  return 1;
    case IR_TYPE_I16: return 2;
    case IR_TYPE_I32: return 4;
    default:          return 8;
    }
}

/* ---------- fusion classification + use counting ----------------------- */

/* True if the instruction's natural codegen leaves its result in the
 * arch's canonical result register, so it's a candidate to "leave in
 * register" rather than spill to a slot. Float-typed defs land in the
 * float register file, which doesn't ride the same fusion path
 * (float consumers always reload via a slot today). */
int instrDefsIntoReg(IrInstr *I) {
    if (I->dst && irIsFloat(I->dst->type)) return 0;
    if (I->op == IR_PHI && (I->flags & IRCG_PHI_IN_REG)) return 1;
    switch (I->op) {
    case IR_LOAD:
    case IR_LOAD_DEREF:
    case IR_LEA:
    case IR_IADD: case IR_ISUB: case IR_IMUL:
    case IR_AND:  case IR_OR:   case IR_XOR:
    case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
    case IR_SHL:  case IR_SHR:  case IR_SAR:
    case IR_ICMP:
        return 1;
    case IR_CALL:
        return I->dst && I->dst->type != IR_TYPE_VOID;
    default:
        return 0;
    }
}

IrValue *firstFusableSource(IrInstr *I) {
    switch (I->op) {
    case IR_STORE:
    case IR_STORE_DEREF:
    case IR_IADD: case IR_ISUB: case IR_IMUL:
    case IR_AND:  case IR_OR:   case IR_XOR:
    case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
    case IR_SHL:  case IR_SHR:  case IR_SAR:
    case IR_ICMP:
        return I->r1;
    case IR_BR:
    case IR_RET:
        return I->dst;
    default:
        return NULL;
    }
}

static void bumpUseIfTmp(Map *uses, IrValue *v) {
    if (!v || v->kind != IR_VAL_TMP) return;
    int n = mapHasInt(uses, v->as.var.id)
            ? (int)(intptr_t)mapGetInt(uses, v->as.var.id) : 0;
    mapAdd(uses, (void *)(u64)v->as.var.id, (void *)(intptr_t)(n + 1));
}

/* Walk every instruction once, counting tmp source-uses; then walk
 * again to mark FUSE_TO_NEXT / R1_IN_REG pairs. Sources for the
 * use-count pass are: r1, r2, dst (for IR_BR / IR_RET / IR_STORE_DEREF
 * which treat dst as a read source), each IR_CALL arg, and each phi
 * pair value. */
void irCgAnnotate(IrFunction *func) {
    Map *uses = mapNew(64, &map_uint_to_uint_type);

    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 i = 0; i < args->size; ++i) {
                    bumpUseIfTmp(uses, (IrValue *)args->entries[i]);
                }
            } else {
                bumpUseIfTmp(uses, I->r1);
            }
            bumpUseIfTmp(uses, I->r2);
            if (I->op == IR_BR || I->op == IR_RET ||
                I->op == IR_STORE_DEREF) {
                bumpUseIfTmp(uses, I->dst);
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                    IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                    bumpUseIfTmp(uses, p->ir_value);
                }
            }
            I->flags &= ~(u64)(IRCG_FUSE_TO_NEXT | IRCG_R1_IN_REG);
        }
    }

    listForEach(func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (listEmpty(bb->instructions)) continue;
        for (List *node = bb->instructions->next;
             node != bb->instructions;
             node = node->next)
        {
            IrInstr *cur = (IrInstr *)node->value;
            if (cur->op == IR_NOP) continue;
            if (!instrDefsIntoReg(cur)) continue;
            if (!cur->dst || cur->dst->kind != IR_VAL_TMP) continue;

            int use_count = mapHasInt(uses, cur->dst->as.var.id)
                            ? (int)(intptr_t)mapGetInt(uses, cur->dst->as.var.id)
                            : 0;
            if (use_count == 0) {
                cur->flags |= IRCG_FUSE_TO_NEXT;
                continue;
            }
            if (use_count != 1) continue;

            /* Find the next instruction that actually emits something.
             * Skip IR_NOPs left by mem2reg / fold. If we hit an IR_JMP
             * whose target has a single predecessor (this block) we
             * can chase into the target - the result register survives
             * an unconditional jump. We stop at IR_BR / IR_LOOP since
             * branch arms may need different register setups. */
            IrInstr *next = NULL;
            IrBlock *scan_bb = bb;
            List *scan = node->next;
            int hops = 0;
            while (1) {
                while (scan == scan_bb->instructions) {
                    IrInstr *term = (IrInstr *)listValue(IrInstr *,
                                                         listTail(scan_bb->instructions));
                    if (!term || term->op != IR_JMP) { scan_bb = NULL; break; }
                    IrBlock *tgt = term->extra.blocks.target_block;
                    if (!tgt) { scan_bb = NULL; break; }
                    Map *preds = irFunctionGetPredecessors(func, tgt);
                    if (!preds || preds->size != 1) { scan_bb = NULL; break; }
                    scan_bb = tgt;
                    scan = scan_bb->instructions->next;
                    if (++hops > 8) { scan_bb = NULL; break; }
                }
                if (!scan_bb) break;
                IrInstr *cand = (IrInstr *)scan->value;
                if (cand->op != IR_NOP && cand->op != IR_JMP) {
                    next = cand;
                    break;
                }
                scan = scan->next;
            }
            if (!next) continue;

            IrValue *next_src = firstFusableSource(next);
            if (!next_src || next_src->kind != IR_VAL_TMP) continue;
            if (next_src->as.var.id != cur->dst->as.var.id) continue;

            cur->flags |= IRCG_FUSE_TO_NEXT;
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
                    bumpUseIfTmp(uses, (IrValue *)args->entries[i]);
                }
            } else {
                bumpUseIfTmp(uses, I->r1);
            }
            bumpUseIfTmp(uses, I->r2);
            if (I->op == IR_BR || I->op == IR_RET ||
                I->op == IR_STORE_DEREF) {
                bumpUseIfTmp(uses, I->dst);
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                    IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                    bumpUseIfTmp(uses, p->ir_value);
                }
            }
        }
    }

    listForEach(func->blocks) {
        IrBlock *B = (IrBlock *)it->value;
        if (listEmpty(B->instructions)) continue;

        int phi_count = 0;
        IrInstr *the_phi = NULL;
        listForEach(B->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_PHI) { phi_count++; the_phi = I; continue; }
            if (I->op == IR_NOP) continue;
            break;
        }
        if (phi_count != 1) continue;

        Map *preds = irFunctionGetPredecessors(func, B);
        if (!preds || preds->size == 0) continue;

        int all_jmp = 1;
        MapIter *iter = mapIterNew(preds);
        while (mapIterNext(iter)) {
            IrBlock *P = (IrBlock *)iter->node->value;
            IrInstr *term = (IrInstr *)listValue(IrInstr *,
                                                 listTail(P->instructions));
            if (!term || (term->op != IR_JMP && term->op != IR_LOOP)) {
                all_jmp = 0; break;
            }
        }
        mapIterRelease(iter);
        if (!all_jmp) continue;

        if (!the_phi->dst || the_phi->dst->kind != IR_VAL_TMP) continue;
        if (irIsFloat(the_phi->dst->type)) continue;
        u32 dst_id = the_phi->dst->as.var.id;
        int dst_uses = mapHasInt(uses, dst_id)
                       ? (int)(intptr_t)mapGetInt(uses, dst_id) : 0;
        if (dst_uses > 1) continue;

        IrInstr *next = NULL;
        int seen_phi = 0;
        listForEach(B->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I == the_phi) { seen_phi = 1; continue; }
            if (!seen_phi) continue;
            if (I->op == IR_NOP) continue;
            if (I->op == IR_PHI) continue;
            next = I;
            break;
        }
        if (!next) continue;
        IrValue *next_src = firstFusableSource(next);
        if (!next_src || next_src->kind != IR_VAL_TMP) continue;
        if (next_src->as.var.id != dst_id) continue;

        the_phi->flags |= IRCG_PHI_IN_REG;
    }

    mapRelease(uses);
}

/* ---------- block / value analysis ------------------------------------ */

int blockHasPhi(IrBlock *bb) {
    if (!bb) return 0;
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_PHI) return 1;
        if (I->op != IR_NOP) return 0;
    }
    return 0;
}

static IrInstr *blockTerminator(IrBlock *bb) {
    IrInstr *term = NULL;
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_NOP) continue;
        term = I;
    }
    return term;
}

Set *irCgComputeReferencedBlocks(IrFunction *func) {
    Set *referenced = setNew(16, &set_uint_type);
    for (List *bn = func->blocks->next;
         bn != func->blocks;
         bn = bn->next)
    {
        IrBlock *block = (IrBlock *)bn->value;
        IrBlock *next_block = (bn->next != func->blocks)
                              ? (IrBlock *)bn->next->value
                              : NULL;
        IrInstr *term = blockTerminator(block);
        if (!term) continue;

        switch (term->op) {
        case IR_JMP:
        case IR_LOOP: {
            IrBlock *t = term->extra.blocks.target_block;
            if (t && t != next_block) {
                setAdd(referenced, (void *)(u64)t->id);
            }
            break;
        }
        case IR_BR: {
            IrBlock *t = term->extra.blocks.target_block;
            IrBlock *f = term->extra.blocks.fallthrough_block;
            int t_phi = blockHasPhi(t);
            int f_phi = blockHasPhi(f);
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

/* ---------- AST-driven layout ----------------------------------------- */

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
            u32 pid;
            if (p->kind == AST_DEFAULT_PARAM) pid = p->declvar->lvar_id;
            else if (p->kind == AST_FUNPTR)   pid = p->fn_ptr_id;
            else                              pid = p->lvar_id;
            IrValue *iv = irFnGetVar(ra->func, pid);
            if (iv) irCgSetLoff(ra, iv->as.var.id, p->loff);
        }
    }
    listForEach(ast_func->locals) {
        Ast *l = (Ast *)it->value;
        u32 lid;
        if (l->kind == AST_DEFAULT_PARAM) lid = l->declvar->lvar_id;
        else if (l->kind == AST_FUNPTR)   lid = l->fn_ptr_id;
        else                              lid = l->lvar_id;
        IrValue *iv = irFnGetVar(ra->func, lid);
        if (iv) irCgSetLoff(ra, iv->as.var.id, l->loff);
    }
}

int irCgComputeAstLayout(Ast *ast_func, IrFunction *ir_func) {
    Set *surviving = setNew(8, &set_uint_type);
    listForEach(ir_func->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_ALLOCA && I->dst &&
                I->dst->kind == IR_VAL_TMP) {
                setAdd(surviving, (void *)(u64)I->dst->as.var.id);
            }
        }
    }

    int total = 0;
    int new_offset = 0;

    listForEach(ast_func->locals) {
        Ast *l = (Ast *)it->value;
        u32 lid;
        int sz;
        if (l->kind == AST_DEFAULT_PARAM) {
            lid = l->declvar->lvar_id;
            sz = l->declvar->type->size;
        } else if (l->kind == AST_FUNPTR) {
            lid = l->fn_ptr_id;
            sz = 8;
        } else {
            lid = l->lvar_id;
            sz = l->type->size;
        }
        IrValue *iv = irFnGetVar(ir_func, lid);
        int promoted = iv && iv->kind == IR_VAL_TMP &&
                       !setHas(surviving, (void *)(u64)iv->as.var.id);
        if (promoted) continue;
        total += align(sz, 8);
        new_offset -= sz;
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
            int sz = (p->kind == AST_FUNPTR) ? 8 : p->type->size;
            p->loff = -offset;
            offset -= align(sz, 8);
        }
    }
    setRelease(surviving);
    return locals_aligned;
}
