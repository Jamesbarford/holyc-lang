#ifndef AARCH64_EMIT_H
#define AARCH64_EMIT_H

#include "aostr.h"
#include "ast.h"
#include "containers.h"
#include "ir-types.h"
#include "ir-regalloc.h"

/* ---------------------------------------------------------------------------
 * Unified aarch64 instruction lowering (prototype - IR_STORE slice).
 *
 * This is to avoid drift between the AOT backend and the JIT backend.
 *
 * Each backend supplies thin "leaf" ops that emit one store as text or bytes.
 * The leaf - not the shared logic - decides whether the source value is
 * already in a register or must be loaded, because forcing every store
 * through a fixed scratch register clobbers a later store's reg-resident
 * source (that bug is exactly why the first cut of this prototype crashed on
 * struct returns: the hidden out-pointer in x0 was overwritten).
 * ------------------------------------------------------------------------- */
typedef struct {
    void *be;  /* backend context: IrCgCtx* (AOT) or JitFnCtx* (JIT) */

    /* Store instr->r1 into instr->dst. The leaf reads r1's location and
     * emits a direct store from its register or loads it first. */
    void (*store_fpr)(void *be, IrInstr *instr, int val_size);  /* float -> slot   */
    void (*store_int)(void *be, IrInstr *instr, int size);      /* int   -> slot   */
    void (*store_int_pinned)(void *be, IrInstr *instr);         /* int   -> pinned */
} A64Emitter;

static inline void a64EmitStore(A64Emitter *e, IrInstr *instr) {
    IrValue *r1 = instr->r1;
    IrValue *dst = instr->dst;

    if (r1 && irIsFloat(r1->type)) {
        e->store_fpr(e->be, instr, (int)irValueByteSize(r1));
        return;
    }

    /* Pinned destination lives in a register, not a slot. */
    if (dst && dst->pinned_reg) {
        e->store_int_pinned(e->be, instr);
        return;
    }

    /* Width is the destination's, not the source's: a wide value stored
     * into a narrow slot must truncate, else the extra bytes clobber
     * neighbouring slots (including saved x29/x30). */
    int src_size = (int)irValueByteSize(r1);
    int dst_size = (int)irValueByteSize(dst);
    e->store_int(e->be, instr, dst_size < src_size ? dst_size : src_size);
}

/* ---------------------------------------------------------------------------
 * Unified AAPCS call emission (shared by aarch64.c / aarch64-jit.c).
 *
 * ALL of the ABI accounting lives here once: the pre-pass that sizes the two
 * below-sp regions (the stack-argument area for register overflow + the
 * INDIRECT copy area for >16-byte aggregates), the per-argument
 * classification, and the NGRN/NSRN/stack-cursor bookkeeping. 
 * The backend only supplies leaf ops that emit one primitive as text or
 * machine code. The struct address is parked in x9 by load_struct_addr; x10
 * is the copy scratch.
 * ------------------------------------------------------------------------- */
typedef struct {
    void *be;
    void (*sub_sp)(void *be, int n);
    void (*add_sp)(void *be, int n);
    void (*load_struct_addr)(void *be, IrValue *a);              /* arg addr -> x9 */
    void (*copy_x9_to_sp)(void *be, int size, int dst_off);     /* [x9] -> [sp+off] */
    void (*agg_load)(void *be, int is_fp, int reg, int src_off, int size);
    void (*ptr_in_reg)(void *be, int reg, int sp_off);          /* add x[reg], sp,#off */
    void (*ptr_to_stack)(void *be, int sp_off, int nsaa);       /* (sp+off) -> [sp+nsaa] */
    void (*scalar_to_reg)(void *be, IrValue *a, int is_fp, int reg);
    void (*scalar_to_stack)(void *be, IrValue *a, int is_fp, int nsaa);
    void (*call)(void *be, AoStr *fname, int indirect);
    void (*spill_ret)(void *be, IrInstr *instr, int is_float);
    /* Register-returned struct (<=16 bytes): store result reg #reg
     * (v0.. for HFA / x0,x1 for INTEGER) to the destination buffer [x9+off];
     * then park the buffer address (x9) into the call's dst slot. The
     * destination address is computed into x9 before the call and saved to
     * [sp+off] (stash) because x9 is caller-saved; it is reloaded (unstash)
     * after the call to receive the result registers. */
    void (*ret_chunk_store)(void *be, int is_fp, int reg, int off, int size);
    void (*spill_struct_dst)(void *be, IrInstr *instr);
    void (*stash_dest)(void *be, int sp_off);    /* str x9, [sp,#off] */
    void (*unstash_dest)(void *be, int sp_off);  /* ldr x9, [sp,#off] */
    /* Does `v` still hold the destination address AFTER a call, so the
     * address can be recomputed instead of spilled? True for a stack
     * slot and for a callee-saved register; false for a caller-saved
     * one, which the callee may destroy. NULL means "assume not". */
    int (*dest_survives_call)(void *be, IrValue *v);
    /* Point the backend's destination-buffer register at `v`'s own
     * home, if it has one that survives the call. Returns 1 when it
     * did, meaning the address needs no materialising into x9 at all.
     * The register itself stays inside the backend - the two of them
     * name registers differently, so it cannot cross this interface. */
    int (*use_dest_home)(void *be, IrValue *v);
} A64CallEmitter;

static inline void a64EmitCall(A64CallEmitter *e, IrInstr *instr,
                               Vec *args, AoStr *fname, int indirect) {
    u64 n = args ? args->size : 0;
    int ngrn = 0, nsrn = 0;

    /* args[0] of an aggregate-returning call is the destination buffer,
     * and it is never one of the x0-x7 arguments: a <=16-byte return
     * comes back in registers (copy them into the buffer after the
     * call), a >16-byte (INDIRECT) return passes the buffer address in
     * x8, AAPCS64's dedicated indirect-result register. Skip it in the
     * arg passing either way. */
    AstType *ret_st = (instr->flags & IRCG_CALL_AGG_RETURN) && instr->dst
                      ? instr->dst->byval_struct_type : NULL;
    /* The result IS this function's return value and the `ret` is next:
     * the bytes are already in the registers the epilogue wants, so the
     * buffer, the address kept alive to reach it, and the store through
     * it all go. args[0] stays in the vector as the placeholder the arg
     * loop skips - there is simply nothing left to do with it. */
    int fwd_ret = (instr->flags & IRCG_AGG_RET_FORWARD) != 0;
    int reg_ret = ret_st && ret_st->size <= 16 && !fwd_ret;
    int ind_ret = ret_st && ret_st->size > 16;
    u64 a0 = ret_st ? 1 : 0;

    /* Pre-pass: size the stack-arg area (nsaa_total) and the INDIRECT copy
     * area (indirect_bytes). */
    int nsaa_total = 0, indirect_bytes = 0;
    {
        int g = 0, s = 0;
        for (u64 i = a0; i < n; ++i) {
            IrValue *a = vecGet(IrValue *, args, i);
            if (a->byval_struct_type) {
                AstType *t = a->byval_struct_type;
                int el = 0, c = 0;
                AapcsClass cl = astAapcsClassify(t, &el, &c);
                if (cl == AAPCS_INDIRECT) {
                    indirect_bytes += (t->size + 15) & ~15;
                    if (g < 8) g++; else nsaa_total += 8;
                } else if (cl == AAPCS_HFA) {
                    if (s + c <= 8) s += c;
                    else { nsaa_total += (t->size + 7) & ~7; s = 8; }
                } else {
                    int ngp = (t->size + 7) / 8;
                    if (g + ngp <= 8) g += ngp;
                    else { nsaa_total += (t->size + 7) & ~7; g = 8; }
                }
            } else if (irIsFloat(a->type)) {
                if (s < 8) s++; else nsaa_total += 8;
            } else {
                if (g < 8) g++; else nsaa_total += 8;
            }
        }
    }
    /* The destination pointer is computed into x9, which is
     * caller-saved, so it normally has to be spilled across the call.
     * When the VALUE it came from survives the call by itself - a slot,
     * or a callee-saved register the allocator handed out - the spill
     * is pure waste: recompute the address afterwards instead. That
     * also drops the 8 bytes reserved to hold it, which was the whole
     * reason for a `sub sp` around some calls. */
    int need_dest = ret_st && !fwd_ret;
    int dest_resident = need_dest && e->dest_survives_call &&
                        e->dest_survives_call(e->be, vecGet(IrValue *, args, 0));
    /* Stronger still: the address is ALREADY in a surviving register,
     * so it need not be moved into x9 either side of the call. */
    int dest_homed = need_dest && e->use_dest_home &&
                     e->use_dest_home(e->be, vecGet(IrValue *, args, 0));
    int dest_save_off = nsaa_total + indirect_bytes;
    int total = (nsaa_total + indirect_bytes +
                 ((need_dest && !dest_resident) ? 16 : 0) + 15) & ~15;
    if (total > 0) e->sub_sp(e->be, total);
    int nsaa = 0, copy_off = 0, copy_base = nsaa_total;

    if (need_dest && !dest_resident && !dest_homed) {
        /* Compute the destination buffer address into x9 (before the arg
         * loop clobbers x9) and stash it across the call. */
        e->load_struct_addr(e->be, vecGet(IrValue *, args, 0));
        e->stash_dest(e->be, dest_save_off);
    }
    /* AAPCS64: an INDIRECT return buffer's address goes in x8, the
     * dedicated indirect-result register (not x0 - that's SysV's rdi
     * rule). Load it BEFORE the arg loop: args[0] may live in an arg
     * register (a forwarded LEA), which the loop is about to overwrite;
     * nothing in the loop touches x8. */
    if (ind_ret) e->scalar_to_reg(e->be, vecGet(IrValue *, args, 0), 0, 8);

    for (u64 i = a0; i < n; ++i) {
        IrValue *a = vecGet(IrValue *, args, i);
        if (a->byval_struct_type) {
            AstType *t = a->byval_struct_type;
            int elem = 0, count = 0;
            AapcsClass cls = astAapcsClassify(t, &elem, &count);
            e->load_struct_addr(e->be, a);   /* -> x9 */
            if (cls == AAPCS_INDIRECT) {
                int dst = copy_base + copy_off;
                e->copy_x9_to_sp(e->be, t->size, dst);
                if (ngrn < 8) {
                    e->ptr_in_reg(e->be, ngrn, dst);
                    ngrn++;
                } else {
                    e->ptr_to_stack(e->be, dst, nsaa);
                    nsaa += 8;
                }
                copy_off += (t->size + 15) & ~15;
            } else if (cls == AAPCS_HFA) {
                if (nsrn + count <= 8) {
                    for (int k = 0; k < count; ++k) {
                        e->agg_load(e->be, 1, nsrn + k, k * elem, elem);
                    }
                    nsrn += count;
                } else {
                    e->copy_x9_to_sp(e->be, t->size, nsaa);
                    nsaa += (t->size + 7) & ~7; nsrn = 8;
                }
            } else { /* AAPCS_INTEGER */
                int ngp = (t->size + 7) / 8;
                if (ngrn + ngp <= 8) {
                    for (int k = 0; k < ngp; ++k) {
                        int off = k * 8, rem = t->size - off;
                        e->agg_load(e->be, 0, ngrn + k, off, rem >= 8 ? 8 : rem);
                    }
                    ngrn += ngp;
                } else {
                    e->copy_x9_to_sp(e->be, t->size, nsaa);
                    nsaa += (t->size + 7) & ~7; ngrn = 8;
                }
            }
        } else if (irIsFloat(a->type)) {
            if (nsrn < 8) {
                e->scalar_to_reg(e->be, a, 1, nsrn);
                nsrn++;
            } else {
                e->scalar_to_stack(e->be, a, 1, nsaa);
                nsaa += 8;
            }
        } else {
            if (ngrn < 8) {
                e->scalar_to_reg(e->be, a, 0, ngrn);
                ngrn++;
            } else {
                e->scalar_to_stack(e->be, a, 0, nsaa);
                nsaa += 8;
            }
        }
    }

    e->call(e->be, fname, indirect);

    if (reg_ret) {
        /* Reload the destination buffer (saved before the call) and copy the
         * result registers into it; do this while sp is still lowered. */
        int re = 0, rc = 0;
        AapcsClass rcls = astAapcsClassify(ret_st, &re, &rc);
        if (dest_homed)         { /* already there */ }
        else if (dest_resident) e->load_struct_addr(e->be, vecGet(IrValue *, args, 0));
        else                    e->unstash_dest(e->be, dest_save_off);
        if (rcls == AAPCS_HFA) {
            for (int k = 0; k < rc; ++k)
                e->ret_chunk_store(e->be, 1, k, k * re, re);
        } else { /* AAPCS_INTEGER, <=16 bytes -> x0,(x1) */
            int ngp = (ret_st->size + 7) / 8;
            for (int k = 0; k < ngp; ++k) {
                int off = k * 8, rem = ret_st->size - off;
                e->ret_chunk_store(e->be, 0, k, off, rem >= 8 ? 8 : rem);
            }
        }
    } else if (ind_ret) {
        /* The call's dst is the buffer address, but unlike SysV the
         * callee does NOT return it in x0: get it back while sp is
         * still lowered - reloaded, or recomputed when the source
         * outlived the call on its own. */
        if (dest_homed)         { /* already there */ }
        else if (dest_resident) e->load_struct_addr(e->be, vecGet(IrValue *, args, 0));
        else                    e->unstash_dest(e->be, dest_save_off);
    }

    if (total > 0) e->add_sp(e->be, total);

    if (need_dest) {
        e->spill_struct_dst(e->be, instr);  /* dst = buffer address (x9) */
    } else if (!ret_st && instr->dst && instr->dst->type != IR_TYPE_VOID) {
        /* Scalar return in x0 / d0. */
        e->spill_ret(e->be, instr, irIsFloat(instr->dst->type));
    }
}

/* ---------------------------------------------------------------------------
 * Unified AAPCS callee param prologue (the mirror of a64EmitCall).
 *
 * Receives by-value struct params (and register-overflow scalars) into their
 * frame slots. The shared body owns the same ABI accounting as the caller -
 * classification, NGRN/NSRN counters,
 * the incoming-stack cursor (args start at [x29, #16]), and the
 * "unreferenced param -> no slot, still advance the cursor" rule. The backend
 * supplies leaves that store one aggregate chunk / copy a stack param /
 * unpack an INDIRECT pointee. Reg-resident scalar params are handled by the
 * IR arrive+store path; here they only advance the counters.
 * ------------------------------------------------------------------------- */
static inline int a64IsByvalStruct(AstType *t) {
    return t && (t->kind == AST_TYPE_CLASS || t->kind == AST_TYPE_UNION) &&
           !t->is_intrinsic;
}

typedef struct {
    void *be;
    /* Store reg `reg` (v-reg if is_fp, else x-reg), `size` bytes, to the
     * param slot at [x29, #loff]. */
    void (*agg_store)(void *be, int is_fp, int reg, int loff, int size);
    /* Copy a `size`-byte param from the incoming stack ([x29,#incoming_off])
     * into its slot at #loff. */
    void (*copy_stack_to_slot)(void *be, int size, int incoming_off, int loff);
    /* INDIRECT: a pointer arrived in x[reg] (or on the incoming stack);
     * copy the `size`-byte pointee into the slot at #loff. */
    void (*indirect_to_slot)(void *be, int from_stack, int reg,
                             int incoming_off, int size, int loff);
} A64ParamEmitter;

/* Where this function's VARIADIC arguments begin, as an offset from
 * x29.
 *
 * Apple AArch64 passes every variadic argument on the stack, so they sit
 * in the incoming argument area that starts at [x29, #16] - just past
 * the saved x29/x30 pair. Named parameters normally arrive in registers
 * and take no room there, but ones that overflowed the registers were
 * placed first and have to be stepped over.
 *
 * The accounting below deliberately mirrors a64EmitParamPrologue's,
 * because it is answering the same question from the other side: where
 * that loop stops reading is where the variadic list starts. Any
 * disagreement between the two reads arguments that are not there. */
static inline int a64VaRegionOffset(Ast *ast) {
    int ngrn = 0, nsrn = 0, nsaa = 0;
    if (!ast || !ast->params) return 16;

    for (u64 i = 0; i < ast->params->size; ++i) {
        Ast *p = vecGet(Ast *, ast->params, i);
        if (p->kind == AST_VAR_ARGS) break;
        AstType *t = p->type;

        if (!a64IsByvalStruct(t)) {
            int is_float = (t->kind == AST_TYPE_FLOAT);
            if (is_float ? (nsrn < 8) : (ngrn < 8)) {
                if (is_float) nsrn++; else ngrn++;
            } else {
                nsaa += 8;
            }
            continue;
        }

        int elem = 0, count = 0;
        AapcsClass cls = astAapcsClassify(t, &elem, &count);
        if (cls == AAPCS_HFA) {
            if (nsrn + count <= 8) nsrn += count;
            else { nsaa += (t->size + 7) & ~7; nsrn = 8; }
        } else if (cls == AAPCS_INTEGER) {
            int ngp = (t->size + 7) / 8;
            if (ngrn + ngp <= 8) ngrn += ngp;
            else { nsaa += (t->size + 7) & ~7; ngrn = 8; }
        } else { /* AAPCS_INDIRECT: a pointer, in a register or not */
            if (ngrn >= 8) nsaa += 8; else ngrn += 1;
        }
    }
    return 16 + nsaa;
}

static inline void a64EmitParamPrologue(A64ParamEmitter *e, Ast *ast,
                                        IrFunction *fn) {
    if (!ast->params) return;
    /* Next General-purpose Register Number */
    int ngrn = 0;
    /* Next simd reg number */
    int nsrn = 0;
    /* Next stack argument address */
    int nsaa = 0;

    /* An INDIRECT (>16-byte) struct return's hidden out-pointer arrives
     * in x8, AAPCS64's dedicated indirect-result register, so it consumes
     * no argument register (the IR-level arrive+store spills it); a
     * <=16-byte aggregate comes back in registers. Either way ngrn
     * starts at 0. */

    for (u64 i = 0; i < ast->params->size; ++i) {
        Ast *p = vecGet(Ast *, ast->params, i);
        if (p->kind == AST_VAR_ARGS) break;
        AstType *t = p->type;

        /* loff is only valid for a referenced param; an unreferenced one
         * gets no slot, but the caller still placed it so the counters /
         * cursor must advance. */
        IrValue *slot = irFnGetVar(fn, irGetParamId(p));
        int has_slot = slot && mapHasInt(fn->ra.id_to_loff, irVarId(slot));
        int loff = has_slot ? irCgGetLoff(&fn->ra, slot) : 0;

        if (!a64IsByvalStruct(t)) {
            int is_float = (t->kind == AST_TYPE_FLOAT);
            if (is_float ? (nsrn < 8) : (ngrn < 8)) {
                if (is_float) nsrn++;
                else          ngrn++;  /* reg: arrive+store */
            } else {
                if (has_slot) e->copy_stack_to_slot(e->be, t->size, 16 + nsaa, loff);
                nsaa += 8;
            }
            continue;
        }

        int elem = 0, count = 0;
        AapcsClass cls = astAapcsClassify(t, &elem, &count);
        if (cls == AAPCS_HFA) {
            if (nsrn + count <= 8) {
                if (has_slot) {
                    for (int k = 0; k < count; ++k) {
                        e->agg_store(e->be, 1, nsrn + k, loff + k * elem, elem);
                    }
                }
                nsrn += count;
            } else {
                if (has_slot)
                    e->copy_stack_to_slot(e->be, t->size, 16 + nsaa, loff);
                nsaa += (t->size + 7) & ~7; nsrn = 8;
            }
        } else if (cls == AAPCS_INTEGER) {
            int ngp = (t->size + 7) / 8;
            if (ngrn + ngp <= 8) {
                if (has_slot) for (int k = 0; k < ngp; ++k) {
                    int off = k * 8, rem = t->size - off;
                    e->agg_store(e->be, 0, ngrn + k, loff + off,
                                 rem >= 8 ? 8 : rem);
                }
                ngrn += ngp;
            } else {
                if (has_slot) e->copy_stack_to_slot(e->be, t->size, 16 + nsaa, loff);
                nsaa += (t->size + 7) & ~7; ngrn = 8;
            }
        } else { /* AAPCS_INDIRECT */
            int from_stack = (ngrn >= 8);
            int incoming_off = 16 + nsaa;
            int reg = ngrn;
            if (from_stack) nsaa += 8; else ngrn += 1;
            if (has_slot)
                e->indirect_to_slot(e->be, from_stack, reg, incoming_off,
                                    t->size, loff);
        }
    }
}

/* ---------------------------------------------------------------------------
 * Unified conversion ops (TRUNC/ZEXT/SEXT, fp<->fp, int<->fp, bitcast).
 *
 * Every conversion has the same shape - load the source into the canonical
 * scratch (x0 / d0), emit ONE convert in place, spill the result - so the
 * shared body owns the per-op dispatch and the width selection (this is
 * where the ZEXT-of-a-4-byte-load bug lived, fixed once here), and the
 * backend supplies a single `convert` leaf keyed by kind + width.
 * ------------------------------------------------------------------------- */
typedef enum {
    A64_CVT_UXTB, A64_CVT_UXTH, A64_CVT_MOVW,        /* zero / truncate */
    A64_CVT_SXTB, A64_CVT_SXTH, A64_CVT_SXTW,        /* sign-extend */
    A64_CVT_FCVT_NARROW, A64_CVT_FCVT_WIDEN,         /* F64<->F32 */
    A64_CVT_FCVTZS, A64_CVT_FCVTZU,                  /* fp -> int (width=fp src) */
    A64_CVT_SCVTF, A64_CVT_UCVTF,                    /* int -> fp (width=fp dst) */
    A64_CVT_FMOV_GP2FP, A64_CVT_FMOV_FP2GP           /* bitcast (width=fp side) */
} A64CvtKind;

typedef struct {
    void *be;
    void (*load_gp)(void *be, IrInstr *instr);   /* r1 -> x0 */
    void (*load_fpr)(void *be, IrInstr *instr);  /* r1 -> d0 */
    void (*spill_gp)(void *be, IrInstr *instr);  /* x0 -> dst */
    void (*spill_fpr)(void *be, IrInstr *instr); /* d0 -> dst */
    void (*convert)(void *be, A64CvtKind kind, int width);
    /* Sign-extending load of a MEMORY-resident r1, folding the widen
     * into the load: `ldursb x0, [x29,#-8]` rather than `ldurb` plus a
     * separate `sxtb`. Optional - NULL falls back to load + convert. */
    void (*load_gp_sx)(void *be, IrInstr *instr, int src_size);
    /* Optional: emit the conversion straight between home registers,
     * `uxth w19, w20` rather than a move in, a convert in x0, and a
     * move out. Returns 0 when either end lives in memory. */
    int (*conv3)(void *be, A64CvtKind k, IrInstr *i);
} A64ConvEmitter;

static inline void a64EmitConvert(A64ConvEmitter *e, IrInstr *instr) {
    void *be = e->be;
    switch (instr->op) {
    case IR_TRUNC: {
        {
            int sz3 = instr->dst ? (int)instr->dst->as.var.size : 8;
            int sgn3 = (instr->flags & IRCG_TRUNC_SIGNED) != 0;
            A64CvtKind k3 = sz3 == 1 ? (sgn3 ? A64_CVT_SXTB : A64_CVT_UXTB)
                          : sz3 == 2 ? (sgn3 ? A64_CVT_SXTH : A64_CVT_UXTH)
                          : sz3 == 4 ? (sgn3 ? A64_CVT_SXTW : A64_CVT_MOVW)
                          : (A64CvtKind)-1;
            if ((int)k3 != -1 && e->conv3 && e->conv3(be, k3, instr)) break;
        }
        e->load_gp(be, instr);
        int sz = instr->dst ? (int)instr->dst->as.var.size : 8;
        /* The result still lives in a 64-bit register, so the high bits
         * have to reflect the destination's signedness - otherwise a
         * later compare reads a different number entirely. */
        int sgn = (instr->flags & IRCG_TRUNC_SIGNED) != 0;
        if      (sz == 1) e->convert(be, sgn ? A64_CVT_SXTB : A64_CVT_UXTB, 0);
        else if (sz == 2) e->convert(be, sgn ? A64_CVT_SXTH : A64_CVT_UXTH, 0);
        else if (sz == 4) e->convert(be, sgn ? A64_CVT_SXTW : A64_CVT_MOVW, 0);
        e->spill_gp(be, instr);
        break;
    }
    case IR_ZEXT: {
        int sz = (int)irValueByteSize(instr->r1);
        {
            A64CvtKind k3 = sz == 1 ? A64_CVT_UXTB
                          : sz == 2 ? A64_CVT_UXTH
                          : sz == 4 ? A64_CVT_MOVW : (A64CvtKind)-1;
            if ((int)k3 != -1 && e->conv3 && e->conv3(be, k3, instr)) break;
        }
        e->load_gp(be, instr);
        /* A 1- or 2-byte frame load already zero-extends (`ldurb`/
         * `ldurh`, `movzbq`/`movzwq`), so widening it again is a no-op.
         * A 4-byte load does NOT - it sign-extends, matching `movslq` -
         * so that one still needs the explicit widen. */
        int fused_zx = irValueInMemory(instr->r1) && (sz == 1 || sz == 2);
        if (!fused_zx) {
            if      (sz == 1) e->convert(be, A64_CVT_UXTB, 0);
            else if (sz == 2) e->convert(be, A64_CVT_UXTH, 0);
            else if (sz == 4) e->convert(be, A64_CVT_MOVW, 0);
        }
        e->spill_gp(be, instr);
        break;
    }
    case IR_SEXT: {
        int sz = (int)irValueByteSize(instr->r1);
        /* A 4-byte load already sign-extended to 64 bits, so this widen
         * is a copy. When source and destination share a register it is
         * not even that. */
        if (instr->flags & IRCG_SEXT_NOOP) {
            e->load_gp(be, instr);
            e->spill_gp(be, instr);
            break;
        }
        {
            A64CvtKind k3 = sz == 1 ? A64_CVT_SXTB
                          : sz == 2 ? A64_CVT_SXTH
                          : sz == 4 ? A64_CVT_SXTW : (A64CvtKind)-1;
            if ((int)k3 != -1 && e->conv3 && e->conv3(be, k3, instr)) break;
        }
        /* Fold the widen into the load where the operand is in memory:
         * one `ldursb` instead of `ldurb` + `sxtb`. A 4-byte load
         * already sign-extends, so that case needs nothing extra. */
        if (irValueInMemory(instr->r1) && sz == 4) {
            e->load_gp(be, instr);
        } else if (irValueInMemory(instr->r1) && e->load_gp_sx &&
                   (sz == 1 || sz == 2))
        {
            e->load_gp_sx(be, instr, sz);
        } else {
            e->load_gp(be, instr);
            if      (sz == 1) e->convert(be, A64_CVT_SXTB, 0);
            else if (sz == 2) e->convert(be, A64_CVT_SXTH, 0);
            else if (sz == 4) e->convert(be, A64_CVT_SXTW, 0);
        }
        e->spill_gp(be, instr);
        break;
    }
    case IR_FPTRUNC:
        e->load_fpr(be, instr); e->convert(be, A64_CVT_FCVT_NARROW, 0);
        e->spill_fpr(be, instr); break;
    case IR_FPEXT:
        e->load_fpr(be, instr); e->convert(be, A64_CVT_FCVT_WIDEN, 0);
        e->spill_fpr(be, instr); break;
    case IR_FPTOSI:
        e->load_fpr(be, instr);
        e->convert(be, A64_CVT_FCVTZS, (int)irValueByteSize(instr->r1));
        e->spill_gp(be, instr); break;
    case IR_FPTOUI:
        e->load_fpr(be, instr);
        e->convert(be, A64_CVT_FCVTZU, (int)irValueByteSize(instr->r1));
        e->spill_gp(be, instr); break;
    case IR_SITOFP:
        e->load_gp(be, instr);
        e->convert(be, A64_CVT_SCVTF, (int)irValueByteSize(instr->dst));
        e->spill_fpr(be, instr); break;
    case IR_UITOFP:
        e->load_gp(be, instr);
        e->convert(be, A64_CVT_UCVTF, (int)irValueByteSize(instr->dst));
        e->spill_fpr(be, instr); break;
    case IR_PTRTOINT:
    case IR_INTTOPTR:
    case IR_BITCAST: {
        int dst_fp = irIsFloat(instr->dst->type);
        int src_fp = irIsFloat(instr->r1->type);
        if (dst_fp == src_fp) {
            if (dst_fp) { e->load_fpr(be, instr); e->spill_fpr(be, instr); }
            else        { e->load_gp(be, instr);  e->spill_gp(be, instr); }
        } else if (dst_fp) {
            e->load_gp(be, instr);
            e->convert(be, A64_CVT_FMOV_GP2FP, (int)irValueByteSize(instr->dst));
            e->spill_fpr(be, instr);
        } else {
            e->load_fpr(be, instr);
            e->convert(be, A64_CVT_FMOV_FP2GP, (int)irValueByteSize(instr->r1));
            e->spill_gp(be, instr);
        }
        break;
    }
    default: break;
    }
}

/* ---------------------------------------------------------------------------
 * Unified arithmetic ops (integer binary/mul/div/rem/unary/shift + float
 * binary/neg). The shared body owns the op dispatch and the operand pattern
 * (load r1 into the scratch, fold a constant r2 when the backend can encode
 * it, else use the register path, then spill). Each backend supplies leaves
 * that emit the actual instruction; `alu_imm` returns whether it managed to
 * fold the immediate (the two backends legitimately differ on which
 * immediates encode - e.g. logical immediates - so that capability stays in
 * the leaf). Canonical regs: x0/x1 for GP, d0/d1 for FP.
 * ------------------------------------------------------------------------- */
typedef struct {
    void *be;
    void (*load_gp)(void *be, IrInstr *instr);   /* r1 -> x0 */
    void (*load_fpr)(void *be, IrInstr *instr);  /* r1 -> d0 */
    void (*spill_gp)(void *be, IrInstr *instr);  /* x0 -> dst */
    void (*spill_fpr)(void *be, IrInstr *instr); /* d0 -> dst */

    int  (*alu_imm)(void *be, IrOp op, s64 imm); /* try `op x0,x0,#imm`; 1 if done */
    void (*alu_reg)(void *be, IrOp op, IrInstr *instr); /* op x0,x0,<r2> */
    void (*mul)(void *be);                       /* mul x0,x0,x1 */
    void (*divrem)(void *be, IrOp op);           /* sdiv/udiv [+ msub] */
    void (*unary)(void *be, IrOp op);            /* neg / mvn x0,x0 */
    void (*shift_imm)(void *be, IrOp op, int sh);
    void (*shift_reg)(void *be, IrOp op, IrInstr *instr);
    void (*load_x1)(void *be, IrInstr *instr);   /* r2 -> x1 */
    void (*fbinop)(void *be, IrOp op, int width);/* fadd/.. d0,d0,d1 */
    void (*load_d1)(void *be, IrInstr *instr);   /* r2 -> d1 */
    void (*fneg)(void *be, int width);
    /* Optional: emit `dst = r1 <op> r2` straight into the operands'
     * home registers, with no trip through the scratch pair. Returns 0
     * when it cannot - a missing home, an immediate that will not
     * encode - and the scratch path below runs instead.
     *
     * Without this, giving values register homes only turns
     * "load slot into x0" into "mov home into x0": the same three
     * instructions, now moving between registers. */
    int (*alu3)(void *be, IrOp op, IrInstr *i);
    /* Same idea for shifts, multiplies and the unary ops. */
    int (*shift3)(void *be, IrOp op, IrInstr *i);
    int (*mul3)(void *be, IrInstr *i);
    int (*unary3)(void *be, IrOp op, IrInstr *i);
} A64ArithEmitter;

static inline void a64EmitArith(A64ArithEmitter *e, IrInstr *instr) {
    void *be = e->be;
    IrOp op = instr->op;
    switch (op) {
    case IR_IADD: case IR_ISUB: case IR_AND: case IR_OR: case IR_XOR: {
        if (e->alu3 && e->alu3(be, op, instr)) break;
        e->load_gp(be, instr);
        int has_imm = instr->r2 && instr->r2->kind == IR_VAL_CONST_INT;
        if (!(has_imm && e->alu_imm(be, op, instr->r2->as._i64)))
            e->alu_reg(be, op, instr);
        e->spill_gp(be, instr);
        break;
    }
    case IR_IMUL:
        if (e->mul3 && e->mul3(be, instr)) break;
        e->load_gp(be, instr); e->load_x1(be, instr);
        e->mul(be); e->spill_gp(be, instr);
        break;
    case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
        e->load_gp(be, instr); e->load_x1(be, instr);
        e->divrem(be, op); e->spill_gp(be, instr);
        break;
    case IR_INEG: case IR_NOT:
        if (e->unary3 && e->unary3(be, op, instr)) break;
        e->load_gp(be, instr); e->unary(be, op); e->spill_gp(be, instr);
        break;
    case IR_SHL: case IR_SHR: case IR_SAR: {
        if (e->shift3 && e->shift3(be, op, instr)) break;
        e->load_gp(be, instr);
        IrValue *r2 = instr->r2;
        if (r2 && r2->kind == IR_VAL_CONST_INT &&
            r2->as._i64 >= 0 && r2->as._i64 < 64)
            e->shift_imm(be, op, (int)r2->as._i64);
        else
            e->shift_reg(be, op, instr);
        e->spill_gp(be, instr);
        break;
    }
    case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV:
        e->load_fpr(be, instr); e->load_d1(be, instr);
        e->fbinop(be, op, (int)irValueByteSize(instr->dst));
        e->spill_fpr(be, instr);
        break;
    case IR_FNEG:
        e->load_fpr(be, instr);
        e->fneg(be, (int)irValueByteSize(instr->dst));
        e->spill_fpr(be, instr);
        break;
    default: break;
    }
}

#endif /* AARCH64_EMIT_H */
