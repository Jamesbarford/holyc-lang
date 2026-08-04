/* Inlining of small accessor functions, at the IR level.
 *
 * Why the IR and not the AST: an AST splicer has to invent lvar ids for
 * the callee's variables inside the caller's numbering, and gets them
 * wrong. By the time a function is lowered every value already carries a
 * globally unique id (`ir_tmp_var_id` is never reset, `lvar_id` comes
 * from one program-wide counter), so cloning is a matter of handing the
 * copy fresh ids from the same counters - mechanical, and impossible to
 * get subtly wrong.
 *
 * What v1 covers, and why the line is drawn there: a callee's LOCALS get
 * their stack slots from `ast_func->locals` in irCgComputeAstLayout, not
 * from the IR. Splice a body that has one into a caller and it lands in
 * a frame that reserved nothing for it - a silent read and write of
 * offset 0. Temporaries are different: they are allocated from the IR
 * (irCgAllocAllTmps), so a cloned tmp gets a slot for free. So v1 takes
 * only callees whose locals are all gone after their own optimisation,
 * which is exactly the population worth having - `Len`, `At`, `UnWrap`,
 * `Get` and the rest of the one-line accessors, whose `self` folds into
 * the incoming pointer.
 *
 * Everything else is left alone. An aggregate return, a call, an alloca,
 * a branch or a by-value struct argument all mean "not this time"; there
 * is no partial handling that could be half-right. */
#include <string.h>

#include "aostr.h"
#include "cctrl.h"
#include "containers.h"
#include "ir-inliner.h"
#include "ir-optimise.h"
#include "ir-regalloc.h"
#include "ir-types.h"
#include "ir.h"
#include "list.h"

/* A body bigger than this is not an accessor, and inlining it trades
 * code size for a call that was not costing much. */
#define IR_INLINE_MAX_INSTRS 8

/* Marked with `inline` / `__attribute__((inline))` / `@[inline]`. The
 * author has said the trade is worth making, so the budget is theirs
 * rather than the heuristic's - but not unbounded, because a splice is
 * still a copy at every call site. */
#define IR_INLINE_MARKED_MAX_INSTRS 32

static int g_inline_enabled = 1;

/* `Map<char *fname, Ast *>` - every AST_FUNC in the program, so a call's
 * label can be resolved back to the definition it names. */
static Map *g_fn_asts = NULL;
/* `Map<char *fname, IrFunction *>` - candidates already lowered and
 * optimised. A rejection is cached as `g_not_a_candidate` so an
 * ineligible callee is only ever lowered once. */
static Map *g_candidates = NULL;
static Cctrl *g_cc = NULL;
static IrFunction g_not_a_candidate;

void irInlineSetEnabled(int enabled) {
    g_inline_enabled = enabled;
}

void irInlineRelease(void) {
    if (g_fn_asts)    { mapRelease(g_fn_asts);    g_fn_asts = NULL; }
    if (g_candidates) { mapRelease(g_candidates); g_candidates = NULL; }
    g_cc = NULL;
}

/* Ops a spliced body may contain.
 *
 * A whitelist rather than a blacklist: a new op added elsewhere in the
 * compiler then declines to be inlined until someone has thought about
 * it, which is the right default for a pass whose failure mode is a
 * wrong value rather than a crash.
 *
 * Note what is absent. IR_LOAD/IR_STORE name a slot, and a slot in the
 * callee is a slot the caller never reserved. IR_ALLOCA is the same
 * thing asked for directly. IR_CALL would need the arg-passing machinery
 * to be re-checked against the caller's frame. Branch ops reference
 * blocks, and v1 splices no blocks. */
static int irInlineOpAllowed(IrOp op) {
    switch (op) {
    case IR_NOP:
    case IR_LOAD_DEREF: case IR_STORE_DEREF: case IR_RMW_DEREF:
    case IR_GEP:
    case IR_IADD: case IR_ISUB: case IR_IMUL:
    case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
    case IR_INEG:
    case IR_FADD: case IR_FSUB: case IR_FMUL: case IR_FDIV: case IR_FNEG:
    case IR_AND: case IR_OR: case IR_XOR:
    case IR_SHL: case IR_SHR: case IR_SAR: case IR_NOT:
    case IR_ICMP: case IR_FCMP:
    case IR_TRUNC: case IR_ZEXT: case IR_SEXT:
    case IR_FPTRUNC: case IR_FPEXT:
    case IR_FPTOUI: case IR_FPTOSI: case IR_UITOFP: case IR_SITOFP:
    case IR_PTRTOINT: case IR_INTTOPTR: case IR_BITCAST:
    case IR_SELECT:
        return 1;
    default:
        return 0;
    }
}

/* The function's one live block, or NULL if it does not have exactly
 * one. `removed` blocks are the optimiser's leavings and do not count. */
static IrBlock *irInlineSoleBlock(IrFunction *fn) {
    IrBlock *found = NULL;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (bb->removed) continue;
        if (found) return NULL;
        found = bb;
    }
    return found;
}

static int irInlineValueIsLocal(IrValue *v) {
    return v && v->kind == IR_VAL_LOCAL;
}

/* Locals that are nothing but an unmodified copy of a parameter.
 *
 * Every function opens by spilling its parameters into locals, and the
 * forwarding pass usually rewrites the reads back onto the parameter and
 * lets the spill die. Usually - the index of `T At(self, u64 i)` reaches
 * the load through the FUSED `idx` operand of an addressing mode, and
 * that read is not one the forwarding pass rewrites, so the local and
 * its spill survive. Refusing them would leave out `At`, which is most
 * of the reason to have this pass at all.
 *
 * The copy is safe to see through when it happens exactly once and the
 * local is never written again: the local then holds that parameter's
 * value for the whole body, so a read of it is a read of the argument.
 * `Map<u32 local_id, IrValue *param>`, or NULL if any local in the body
 * fails to fit the shape. Caller owns the map. */
static Map *irInlineParamCopies(IrBlock *bb) {
    Map *copies = irVarValueMapNew();
    /* First: every `store <local>, <param>` is a candidate. A local
     * written twice, or written from anything else, disqualifies the
     * whole body - there is no partial version of this. */
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op != IR_STORE) continue;
        if (!irInlineValueIsLocal(I->dst)) goto reject;
        if (!I->r1 || I->r1->kind != IR_VAL_PARAM) goto reject;
        if (mapGetInt(copies, irVarId(I->dst))) goto reject;   /* written twice */
        mapAddIntOrErr(copies, irVarId(I->dst), I->r1);
    }
    /* Second: no local may appear except as one of those copies, and -
     * the part that is easy to miss - none of them may be WRITTEN again.
     *
     * `arg += 3` on a narrow parameter does not lower to a store: it is
     * a `trunc` whose DESTINATION is the local. Counting stores alone
     * therefore called a mutated parameter a pure copy, bound it to the
     * caller's argument, and produced `trunc <the constant 23>, %t` - a
     * write into a constant, which the slot allocator then asked for the
     * address of.
     *
     * So the test is by position and op: for STORE_DEREF and RMW_DEREF
     * the destination is an ADDRESS being read, which is fine; for
     * everything else a destination is a write. */
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_NOP) continue;
        IrValue *vs[4] = { I->dst, I->r1, I->r2, I->idx };
        for (int k = 0; k < 4; ++k) {
            if (!irInlineValueIsLocal(vs[k])) continue;
            if (!mapGetInt(copies, irVarId(vs[k]))) goto reject;
            if (k != 0) continue;                          /* a read */
            if (I->op == IR_STORE) continue;               /* the copy itself */
            if (I->op == IR_STORE_DEREF || I->op == IR_RMW_DEREF) continue;
            goto reject;                                   /* written again */
        }
    }
    return copies;
reject:
    mapRelease(copies);
    return NULL;
}

/* Does `fn` have the shape v1 can splice? Reports the block, the
 * terminating IR_RET and the param-copy map so the caller need not work
 * them out again. The map is owned by the caller on success. */
static int irInlineEligible(IrFunction *fn, int marked, IrBlock **out_bb,
                            IrInstr **out_ret, Map **out_copies)
{
    if (fn->has_var_args) return 0;

    IrBlock *bb = irInlineSoleBlock(fn);
    if (!bb) return 0;

    Map *copies = irInlineParamCopies(bb);
    if (!copies) return 0;

    IrInstr *ret = NULL;
    int n = 0;
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_NOP) continue;
        /* A return that is not the last instruction means control flow
         * this pass cannot reproduce - and finding one AFTER the body
         * has been walked is too late, so refuse on the second. */
        if (ret) goto reject;
        if (I->op == IR_RET) { ret = I; continue; }
        /* A param spill is dropped at splice time, so it costs nothing
         * and does not count against the size budget. */
        if (I->op == IR_STORE) continue;
        /* A marked function may CALL. The body is spliced where the call
         * to it stood, so a call inside it happens exactly where it
         * would have anyway; what the automatic rule was avoiding is the
         * arg-passing machinery, not a correctness problem. An
         * aggregate-returning call is still out - args[0] of one is a
         * buffer reached through an alloca, and an alloca is a slot the
         * caller never reserved. */
        if (marked && I->op == IR_CALL) {
            if (I->flags & IRCG_CALL_AGG_RETURN) goto reject;
            if (!I->r1 || !I->r1->as.array.label) goto reject;  /* indirect */
        } else if (!irInlineOpAllowed(I->op)) {
            goto reject;
        }
        if (++n > (marked ? IR_INLINE_MARKED_MAX_INSTRS
                          : IR_INLINE_MAX_INSTRS)) goto reject;
    }
    if (!ret) goto reject;
    /* An aggregate return is handed back through a buffer the caller
     * allocated; splicing it needs the call's args[0] machinery, which
     * v1 does not reproduce. */
    if (ret->dst && ret->dst->byval_struct_type) goto reject;

    *out_bb = bb;
    *out_ret = ret;
    *out_copies = copies;
    return 1;
reject:
    mapRelease(copies);
    return 0;
}

/* Build the name -> AST_FUNC index once per compilation. */
static void irInlineIndexProgram(Cctrl *cc) {
    if (g_fn_asts && g_cc == cc) return;
    if (g_fn_asts) irInlineRelease();
    g_cc = cc;
    g_fn_asts    = mapNew(64, &map_cstring_opaque_type);
    g_candidates = mapNew(64, &map_cstring_opaque_type);
    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind != AST_FUNC || !ast->fname) continue;
        mapAddLen(g_fn_asts, ast->fname->data, ast->fname->len, ast);
    }
}

/* The lowered, optimised body for `name`, or NULL if it cannot be
 * inlined. Lowered on demand rather than in a program-wide pass: that
 * keeps every caller of this pass a one-line change, and it works the
 * same in the JITs, which compile one function at a time and never have
 * the whole program in IR at once. */
static IrFunction *irInlineCandidate(Cctrl *cc, AoStr *name, int *marked) {
    irInlineIndexProgram(cc);

    Ast *ast = mapGetLen(g_fn_asts, name->data, name->len);
    *marked = ast && (ast->flags & AST_FLAG_INLINE_HINT) != 0;

    IrFunction *cached = mapGetLen(g_candidates, name->data, name->len);
    if (cached) {
        return cached == &g_not_a_candidate ? NULL : cached;
    }

    if (!ast || !ast->body) {
        mapAddLen(g_candidates, name->data, name->len, &g_not_a_candidate);
        return NULL;
    }

    /* A context of its own: irLowerFunction takes over cur_func, the
     * label table and the escape set, and the caller's lowering is
     * finished with none of those but should not have them pulled out
     * from under it either. */
    IrCtx *sub = irCtxNew(cc);
    IrFunction *fn = irLowerFunction(sub, ast);
    irBasicFunctionOptimisations(fn);

    IrBlock *bb = NULL;
    IrInstr *ret = NULL;
    Map *copies = NULL;
    if (!irInlineEligible(fn, *marked, &bb, &ret, &copies)) {
        mapAddLen(g_candidates, name->data, name->len, &g_not_a_candidate);
        return NULL;
    }
    mapRelease(copies);
    mapAddLen(g_candidates, name->data, name->len, fn);
    return fn;
}

/* ------------------------------------------------------------------
 * Cloning
 * ------------------------------------------------------------------ */

static IrValue *irInlineCopyValue(IrValue *v) {
    IrValue *c = irValueNew(v->type, v->kind);
    *c = *v;
    return c;
}

/* Map one of the callee's operands into the caller.
 *
 * Params and param-copy locals are pre-bound to the arguments the call
 * site supplied - that binding is the whole point of the exercise. A tmp
 * gets a fresh id, created once and shared by every mention, so def and
 * use still name one value. Anything else (a constant, a global, a
 * string) is copied: the candidate's IR is shared by every call site
 * that inlines it and must not pick up a register assignment from any
 * one of them. */
static IrValue *irInlineMap(Map *vmap, IrValue *v) {
    if (!v) return NULL;
    if (v->kind == IR_VAL_TMP || v->kind == IR_VAL_PARAM ||
        v->kind == IR_VAL_LOCAL)
    {
        IrValue *got = mapGetInt(vmap, irVarId(v));
        if (got) return got;
        /* Only a tmp may be unbound here - eligibility has already
         * established that every param and local has a binding. */
        IrValue *fresh = irTmp(v->type, v->as.var.size);
        fresh->byval_struct_type = v->byval_struct_type;
        mapAddIntOrErr(vmap, irVarId(v), fresh);
        return fresh;
    }
    return irInlineCopyValue(v);
}

/* Rewrite every mention of `from`'s variable in `fn` to `to`.
 *
 * The call's destination is defined exactly once - it is a temporary,
 * and temporaries are in SSA form - so replacing all of its uses is the
 * whole of the substitution. */
static void irInlineSubstitute(IrFunction *fn, u32 from_id, IrValue *to) {
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_NOP) continue;
            if (irIsTmp(I->dst) && irVarId(I->dst) == from_id) I->dst = to;
            if (irIsTmp(I->r1)  && irVarId(I->r1)  == from_id) I->r1  = to;
            if (irIsTmp(I->r2)  && irVarId(I->r2)  == from_id) I->r2  = to;
            if (irIsTmp(I->idx) && irVarId(I->idx) == from_id) I->idx = to;
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
                Vec *args = I->r1->as.array.values;
                for (u64 k = 0; k < args->size; ++k) {
                    IrValue *a = (IrValue *)args->entries[k];
                    if (irIsTmp(a) && irVarId(a) == from_id) {
                        args->entries[k] = to;
                    }
                }
            }
            if (I->op == IR_PHI && I->extra.phi_pairs) {
                Vec *pairs = I->extra.phi_pairs;
                for (u64 k = 0; k < pairs->size; ++k) {
                    IrPair *p = (IrPair *)pairs->entries[k];
                    if (irIsTmp(p->ir_value) &&
                        irVarId(p->ir_value) == from_id)
                    {
                        p->ir_value = to;
                    }
                }
            }
        }
    }
}

/* The value the inlined body hands back, in a type the call site can
 * take without conversion.
 *
 * The callee's `ret` is often WIDER than the call's destination: a
 * function returning `int` finishes on `sext %t i64, %t i32`, because
 * the return goes through a register and the width stops mattering
 * there. Substituting that i64 for an i32 destination would hand every
 * downstream consumer a value one size larger than it asked for, so
 * either the types match, or the ret is exactly one widening step away
 * from matching and the step's source is used instead. Anything else
 * declines - guessing at signedness to insert a truncation is how a
 * wrong value gets returned in silence.
 *
 * `vmap` is consulted, not populated: everything here was mapped during
 * the body walk. */
static IrValue *irInlineResult(Map *vmap, IrBlock *bb, IrInstr *ret,
                               IrValue *call_dst)
{
    if (!ret->dst) return NULL;                    /* void */
    IrValue *mapped = irInlineMap(vmap, ret->dst);
    if (!call_dst) return mapped;
    if (mapped->type == call_dst->type) return mapped;

    /* One widening step back. The producer is in the same block and its
     * dst is the value being returned. */
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op != IR_SEXT && I->op != IR_ZEXT) continue;
        if (!irIsTmp(I->dst) || !irIsTmp(ret->dst)) continue;
        if (irVarId(I->dst) != irVarId(ret->dst)) continue;
        IrValue *src = irInlineMap(vmap, I->r1);
        return src->type == call_dst->type ? src : NULL;
    }
    return NULL;
}

/* Width of an integer IR type, or 0 for anything else. */
static int irInlineIntWidth(IrValueType t) {
    switch (t) {
    case IR_TYPE_I8:  return 1;
    case IR_TYPE_I16: return 2;
    case IR_TYPE_I32: return 4;
    case IR_TYPE_I64: return 8;
    case IR_TYPE_PTR: return 8;
    default:          return 0;
    }
}

/* Is `arg` usable as `param` with no conversion at all?
 *
 * At a real call the ABI is the conversion: the argument goes into a
 * register and the callee reads its parameter at the parameter's width.
 * Inlining deletes that boundary, so a narrower argument silently
 * narrows every operation the body performs on it. `Push(91)` passes the
 * literal as i32 into an i64 parameter, and the `entries[n] = x` store
 * went from eight bytes to four - three pushes then overlapped and the
 * container read back as garbage.
 *
 * An integer LITERAL is the one safe exception: its value is already
 * held as a full-width s64, so re-labelling it to a wider integer type
 * changes the label and nothing else. */
static int irInlineArgFits(IrValue *arg, IrValue *param) {
    if (arg->type == param->type) return 1;
    if (arg->kind != IR_VAL_CONST_INT) return 0;
    int aw = irInlineIntWidth(arg->type);
    int pw = irInlineIntWidth(param->type);
    return aw && pw && pw >= aw;
}

/* Can this particular call site take the candidate? */
static int irInlineCallSiteOk(IrInstr *call, IrFunction *callee,
                              IrFunction *caller, Vec **out_args)
{
    if (call->flags & IRCG_FN_BUILTIN) return 0;
    /* args[0] of one of these is a destination buffer, not an argument. */
    if (call->flags & IRCG_CALL_AGG_RETURN) return 0;
    if (!call->r1 || !call->r1->as.array.label) return 0;   /* indirect */
    if (callee == caller) return 0;                         /* recursion */

    Vec *args = call->r1->as.array.values;
    u64 n_args = args ? args->size : 0;
    u64 n_params = callee->params ? callee->params->size : 0;
    if (n_args != n_params) return 0;
    for (u64 i = 0; i < n_args; ++i) {
        IrValue *a = (IrValue *)args->entries[i];
        IrValue *p = vecGet(IrValue *, callee->params, i);
        /* A by-value struct is passed by the platform ABI's rules, which
         * the body has no way to re-read once the call is gone. */
        if (!a || a->byval_struct_type) return 0;
        if (!p || !irInlineArgFits(a, p)) return 0;
    }
    *out_args = args;
    return 1;
}

/* Splice `callee`'s body into `caller` in place of `call`, which sits at
 * list node `node`. Returns 1 if it happened. */
static int irInlineOne(IrFunction *caller, IrInstr *call, List *node,
                       IrFunction *callee, int marked)
{
    IrBlock *bb = NULL;
    IrInstr *ret = NULL;
    Map *copies = NULL;
    Vec *args = NULL;

    if (!irInlineEligible(callee, marked, &bb, &ret, &copies)) return 0;
    if (!irInlineCallSiteOk(call, callee, caller, &args)) {
        mapRelease(copies);
        return 0;
    }

    Map *vmap = irVarValueMapNew();

    /* Params first, so the body walk finds them already bound. */
    u64 n_params = callee->params ? callee->params->size : 0;
    for (u64 i = 0; i < n_params; ++i) {
        IrValue *p = vecGet(IrValue *, callee->params, i);
        IrValue *a = (IrValue *)args->entries[i];
        /* A literal argument keeps its value but takes the parameter's
         * width - see irInlineArgFits for why that matters. */
        if (a->type != p->type) {
            a = irInlineCopyValue(a);
            a->type = p->type;
        }
        mapAddIntOrErr(vmap, irVarId(p), a);
    }
    /* Then the locals that are only ever a copy of one of them: bind
     * each straight to the same argument, so the spill can be dropped
     * and every read of the local reads the argument instead. */
    {
        MapIter mit;
        mapIterInit(copies, &mit);
        while (mapIterNext(&mit)) {
            IrValue *param = (IrValue *)mit.node->value;
            IrValue *bound = mapGetInt(vmap, irVarId(param));
            if (!bound) continue;
            mapAddIntOrErr(vmap, (u32)(u64)mit.node->key, bound);
        }
    }

    /* Walk the body once to bind every value, cloning as we go. The
     * clones go into a list of their own rather than straight into the
     * caller: the result type check below can still decline, and a
     * half-spliced body would be left behind. */
    List *staged = listNew();
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_NOP || I == ret) continue;
        /* The parameter spill, whose destination is now the argument
         * itself - there is nothing left to copy. */
        if (I->op == IR_STORE) continue;
        IrInstr *c = irInstrNew(I->op, NULL, NULL, NULL);
        *c = *I;                       /* flags, disp, scale, line, extra */
        c->dst = irInlineMap(vmap, I->dst);
        c->r1  = irInlineMap(vmap, I->r1);
        c->r2  = irInlineMap(vmap, I->r2);
        c->idx = irInlineMap(vmap, I->idx);
        /* A nested call's `r1` is a wrapper around its ARGUMENT VECTOR,
         * and a shallow copy of the wrapper shares that vector with the
         * candidate every other call site is also cloning from. Mapping
         * arguments through it would rewrite the original. */
        if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
            Vec *src = I->r1->as.array.values;
            Vec *dup = irValueVecNew();
            for (u64 k = 0; k < src->size; ++k) {
                vecPush(dup, irInlineMap(vmap, (IrValue *)src->entries[k]));
            }
            c->r1->as.array.values = dup;
        }
        /* The call's line is what a reader expects to see attributed to
         * this work, not the accessor's one-liner. */
        c->line = call->line;
        listAppend(staged, c);
    }

    IrValue *result = irInlineResult(vmap, bb, ret, call->dst);
    if (ret->dst && call->dst && !result) {
        listRelease(staged, NULL);
        mapRelease(vmap);
        mapRelease(copies);
        return 0;
    }

    listForEach(staged) {
        listInsertValueBefore(node, it->value);
    }
    listRelease(staged, NULL);

    /* Where the result has to end up.
     *
     * A temporary destination is in SSA form, so rewriting its uses to
     * the value the body produced is the whole of it and costs nothing.
     * A LOCAL destination is a memory cell - `t = Add(t, x)` lowers to
     * `call %l_t, Add` - and the backend was going to store the return
     * register into its slot. Nothing else will do that once the call is
     * gone, so the call becomes the store it was standing in for.
     * Missing this returned zero from every such statement. */
    if (call->dst && result && !irIsTmp(call->dst)) {
        IrValue *slot = call->dst;
        call->op = IR_STORE;
        call->dst = slot;
        call->r1 = result;
        call->r2 = call->idx = NULL;
        call->flags = 0;
        call->disp = 0;
        call->scale = 0;
        mapRelease(vmap);
        mapRelease(copies);
        return 1;
    }
    if (call->dst && result) {
        irInlineSubstitute(caller, irVarId(call->dst), result);
    }

    call->op = IR_NOP;
    call->dst = call->r1 = call->r2 = call->idx = NULL;
    mapRelease(vmap);
    mapRelease(copies);
    return 1;
}

int irInlineFunction(IrCtx *ctx, IrFunction *fn) {
    if (!g_inline_enabled || !ctx || !ctx->cc)
        return 0;

    int inlined = 0;
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        if (bb->removed) continue;
        for (List *node = bb->instructions->next;
             node != bb->instructions;
             node = node->next)
        {
            IrInstr *call = (IrInstr *)node->value;
            if (call->op != IR_CALL) continue;
            if (!call->r1 || !call->r1->as.array.label) continue;
            int marked = 0;
            IrFunction *callee =
                irInlineCandidate(ctx->cc, call->r1->as.array.label, &marked);
            if (!callee) continue;
            inlined += irInlineOne(fn, call, node, callee, marked);
        }
    }
    return inlined;
}
