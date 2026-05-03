#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "cctrl.h"
#include "containers.h"
#include "ir.h"
#include "ir-fold.h"
#include "ir-inline.h"
#include "ir-mem2reg.h"
#include "ir-types.h"
#include "list.h"
#include "util.h"

/* IR builders not exposed in ir.h; declared inline here to keep the
 * headers slim. */
extern Vec     *irPairVecNew(void);
extern Vec     *irValueVecNew(void);
extern IrValue *irValueNew(IrValueType type, IrValueKind kind);
extern IrInstr *irInstrNew(IrOp op, IrValue *dst, IrValue *r1, IrValue *r2);
extern IrBlock *irBlockNew(void);
extern IrValue *irTmp(IrValueType type, u16 size);
extern IrPair  *irPairNew(IrBlock *ir_block, IrValue *ir_value);

/* ---------- Direct-recursion check ------------------------------------- */

static int aoStrEqRaw(AoStr *a, AoStr *b) {
    if (!a || !b) return 0;
    if (a->len != b->len) return 0;
    return memcmp(a->data, b->data, a->len) == 0;
}

/* Best-effort recursive walker: returns 1 if any AST_FUNCALL /
 * AST_ASM_FUNCALL with fname == target is reachable from `node`. The
 * AST is a tagged union with many shapes; we cover the common control
 * flow + expression kinds. Anything we don't recognise contributes
 * nothing - if a future construct hides a call we'll miss it (the
 * function would just not be inlined, which is the safe fallback). */
static int astCallsName(Ast *node, AoStr *target) {
    if (!node) return 0;
    switch (node->kind) {
    case AST_FUNCALL:
    case AST_ASM_FUNCALL:
        if (aoStrEqRaw(node->fname, target)) return 1;
        if (node->args) {
            for (u64 i = 0; i < node->args->size; ++i) {
                if (astCallsName((Ast *)node->args->entries[i], target)) {
                    return 1;
                }
            }
        }
        return 0;

    case AST_FUNPTR_CALL:
        if (node->args) {
            for (u64 i = 0; i < node->args->size; ++i) {
                if (astCallsName((Ast *)node->args->entries[i], target)) {
                    return 1;
                }
            }
        }
        return 0;

    case AST_COMPOUND_STMT:
        if (node->stms) {
            listForEach(node->stms) {
                if (astCallsName((Ast *)it->value, target)) return 1;
            }
        }
        return 0;

    case AST_IF:
        return astCallsName(node->cond, target) ||
               astCallsName(node->then, target) ||
               astCallsName(node->els,  target);

    case AST_FOR:
        return astCallsName(node->forinit, target) ||
               astCallsName(node->forcond, target) ||
               astCallsName(node->forstep, target) ||
               astCallsName(node->forbody, target);

    case AST_WHILE:
    case AST_DO_WHILE:
        return astCallsName(node->whilecond, target) ||
               astCallsName(node->whilebody, target);

    case AST_RETURN:
        return astCallsName(node->retval, target);

    case AST_BINOP:
        return astCallsName(node->left,  target) ||
               astCallsName(node->right, target);

    case AST_UNOP:
    case AST_CAST:
        return astCallsName(node->operand, target);

    case AST_DECL:
        return astCallsName(node->declinit, target);

    case AST_CLASS_REF:
        return astCallsName(node->cls, target);

    case AST_SWITCH:
        if (astCallsName(node->switch_cond, target)) return 1;
        if (node->cases) {
            for (u64 i = 0; i < node->cases->size; ++i) {
                if (astCallsName((Ast *)node->cases->entries[i], target)) {
                    return 1;
                }
            }
        }
        if (astCallsName(node->case_default, target)) return 1;
        return 0;

    case AST_CASE:
    case AST_DEFAULT:
        /* Case bodies are usually injected as siblings into the
         * surrounding compound statement; the AST_CASE node itself
         * carries no body field we need to recurse into. */
        return 0;

    default:
        return 0;
    }
}

void irClearRecursiveInline(Cctrl *cc) {
    if (!cc || !cc->ast_list) return;
    listForEach(cc->ast_list) {
        Ast *fn = (Ast *)it->value;
        if (fn->kind != AST_FUNC) continue;
        if (!(fn->flags & AST_FLAG_INLINE)) continue;
        if (!fn->body || !fn->fname) continue;

        if (astCallsName(fn->body, fn->fname)) {
            /* This pass runs after parsing, so cctrlWarning's
             * cur-token lookup is unsafe; loggerWarning is unbound
             * to lexer state. */
            loggerWarning("inline function `%.*s` calls itself; "
                          "inlining disabled (recursive functions "
                          "can't be inlined)\n",
                          (int)fn->fname->len, fn->fname->data);
            fn->flags &= ~AST_FLAG_INLINE;
        }
    }
}

/* ---------- IR-cloning helper ----------------------------------------- */

typedef struct CloneCtx {
    /* u32 src_block_id -> IrBlock* (cloned) */
    Map *block_remap;
    /* u32 src_tmp_id -> IrValue* (cloned tmp). Cached so the same src
     * tmp id always maps to the same cloned IrValue*: preserves the
     * pointer-identity invariant some passes rely on (e.g. ir-peephole
     * compares dst pointers across instructions). */
    Map *tmp_remap;
    /* u32 src_param_id -> IrValue* (caller-side substitution). NULL
     * for standalone clones. */
    Map *param_subst;
} CloneCtx;

static IrValue *cloneValue(CloneCtx *ctx, IrValue *src) {
    if (!src) return NULL;
    switch (src->kind) {
    case IR_VAL_TMP: {
        if (mapHasInt(ctx->tmp_remap, src->as.var.id)) {
            return (IrValue *)mapGetInt(ctx->tmp_remap, src->as.var.id);
        }
        IrValue *clone = irTmp(src->type, src->as.var.size);
        clone->flags = src->flags;
        mapAdd(ctx->tmp_remap,
               (void *)(u64)src->as.var.id, (void *)clone);
        return clone;
    }
    case IR_VAL_PARAM:
        /* Splice path: rewrite the param to the caller-supplied arg
         * value. With no substitution we share with the source (the
         * standalone-clone case). */
        if (ctx->param_subst &&
            mapHasInt(ctx->param_subst, src->as.var.id)) {
            return (IrValue *)mapGetInt(ctx->param_subst,
                                          src->as.var.id);
        }
        return src;
    case IR_VAL_LOCAL:
        /* Locals alias the caller's stack slots; share. */
        return src;
    case IR_VAL_CONST_INT:
    case IR_VAL_CONST_FLOAT:
    case IR_VAL_CONST_STR:
    case IR_VAL_GLOBAL:
    case IR_VAL_LABEL:
    case IR_VAL_UNDEFINED:
    case IR_VAL_UNRESOLVED:
        /* Immutable / address-equivalent: share. */
        return src;
    case IR_VAL_PHI:
        loggerPanic("ir-inline: IR_VAL_PHI operand cloning not supported\n");
    }
    loggerPanic("ir-inline: unknown IrValueKind %d\n", src->kind);
    return NULL;
}

/* Clone an IR_CALL's r1 args wrapper. The wrapper itself is an
 * IrValue (kind IR_VAL_UNRESOLVED) with as.array carrying the function
 * name (label) plus a Vec<IrValue*> of arg values. */
static IrValue *cloneArgsWrap(CloneCtx *ctx, IrValue *src) {
    if (!src) return NULL;
    IrValue *clone = irValueNew(src->type, src->kind);
    clone->flags = src->flags;
    clone->as.array.label = src->as.array.label;        /* share */
    clone->as.array.nesting = src->as.array.nesting;
    clone->as.array.length_per_array = src->as.array.length_per_array;
    Vec *new_args = irValueVecNew();
    if (src->as.array.values) {
        Vec *old = src->as.array.values;
        for (u64 i = 0; i < old->size; ++i) {
            vecPush(new_args, cloneValue(ctx, (IrValue *)old->entries[i]));
        }
    }
    clone->as.array.values = new_args;
    return clone;
}

static IrInstr *cloneInstr(CloneCtx *ctx, IrInstr *src) {
    IrInstr *clone = irInstrNew(src->op, NULL, NULL, NULL);
    clone->flags = src->flags;

    if (src->op == IR_CALL) {
        clone->r1  = cloneArgsWrap(ctx, src->r1);
        clone->r2  = cloneValue(ctx, src->r2);  /* indirect-call target */
    } else {
        clone->r1  = cloneValue(ctx, src->r1);
        clone->r2  = cloneValue(ctx, src->r2);
    }
    clone->dst = cloneValue(ctx, src->dst);

    switch (src->op) {
    case IR_ICMP:
    case IR_FCMP:
        clone->extra.cmp_kind = src->extra.cmp_kind;
        break;

    case IR_BR:
    case IR_JMP:
    case IR_LOOP: {
        IrBlock *t = src->extra.blocks.target_block;
        IrBlock *f = src->extra.blocks.fallthrough_block;
        clone->extra.blocks.target_block = (t && mapHasInt(ctx->block_remap, t->id))
            ? (IrBlock *)mapGetInt(ctx->block_remap, t->id) : NULL;
        clone->extra.blocks.fallthrough_block = (f && mapHasInt(ctx->block_remap, f->id))
            ? (IrBlock *)mapGetInt(ctx->block_remap, f->id) : NULL;
        break;
    }

    case IR_PHI:
        if (src->extra.phi_pairs) {
            Vec *new_pairs = irPairVecNew();
            for (u64 i = 0; i < src->extra.phi_pairs->size; ++i) {
                IrPair *p = vecGet(IrPair *, src->extra.phi_pairs, i);
                IrBlock *pred = (p->ir_block &&
                                 mapHasInt(ctx->block_remap, p->ir_block->id))
                    ? (IrBlock *)mapGetInt(ctx->block_remap, p->ir_block->id)
                    : NULL;
                IrValue *val = cloneValue(ctx, p->ir_value);
                vecPush(new_pairs, irPairNew(pred, val));
            }
            clone->extra.phi_pairs = new_pairs;
        }
        break;

    case IR_SWITCH:
        loggerPanic("ir-inline: IR_SWITCH cloning not supported yet\n");
        break;

    default:
        /* IR_NOP, IR_ALLOCA, IR_LOAD, IR_STORE, IR_LOAD_DEREF,
         * IR_STORE_DEREF, IR_LEA, IR_GEP, integer/float arithmetic,
         * IR_RET, IR_NOT, IR_INEG, IR_FNEG, conversions, IR_CALL
         * (extra unused), ... no extra payload to copy. */
        break;
    }

    return clone;
}

IrCloneResult *irFunctionClone(IrFunction *src, Map *param_subst) {
    if (!src) return NULL;

    CloneCtx ctx;
    ctx.block_remap = mapNew(16, &map_uint_to_uint_type);
    ctx.tmp_remap   = mapNew(64, &map_uint_to_uint_type);
    ctx.param_subst = param_subst;

    /* Phase 1: fresh IrBlocks for every src block, recorded in
     * block_remap so subsequent instruction-clone steps can resolve
     * jump targets and phi pair predecessors regardless of order. */
    List *new_blocks = listNew();
    listForEach(src->blocks) {
        IrBlock *src_bb = (IrBlock *)it->value;
        IrBlock *new_bb = irBlockNew();
        new_bb->removed = src_bb->removed;
        new_bb->sealed  = src_bb->sealed;
        mapAdd(ctx.block_remap,
               (void *)(u64)src_bb->id, (void *)new_bb);
        listAppend(new_blocks, new_bb);
    }

    /* Phase 2: walk instructions in source order, cloning each into
     * the matching cloned block. cloneValue's tmp cache makes forward
     * tmp references (used in phi back-edges) Just Work: the first
     * time we touch a src tmp id we mint the cloned IrValue, and any
     * later visit of the same id sees it in the cache. */
    listForEach(src->blocks) {
        IrBlock *src_bb = (IrBlock *)it->value;
        IrBlock *new_bb = (IrBlock *)mapGetInt(ctx.block_remap, src_bb->id);
        listForEach(src_bb->instructions) {
            IrInstr *src_i = (IrInstr *)it->value;
            IrInstr *new_i = cloneInstr(&ctx, src_i);
            listAppend(new_bb->instructions, new_i);
        }
    }

    IrCloneResult *r = (IrCloneResult *)malloc(sizeof(IrCloneResult));
    r->blocks = new_blocks;
    r->entry_block = src->entry_block
        ? (IrBlock *)mapGetInt(ctx.block_remap, src->entry_block->id)
        : NULL;
    r->exit_block = src->exit_block
        ? (IrBlock *)mapGetInt(ctx.block_remap, src->exit_block->id)
        : NULL;
    r->tmp_remap   = ctx.tmp_remap;
    r->block_remap = ctx.block_remap;
    return r;
}

void irCloneResultRelease(IrCloneResult *r) {
    if (!r) return;
    if (r->tmp_remap)   mapRelease(r->tmp_remap);
    if (r->block_remap) mapRelease(r->block_remap);
    /* `blocks` itself is a list of pointers; the blocks are owned by
     * whatever spliced them in. We don't free the list here either -
     * the splice will hand its nodes off to a target function. */
    free(r);
}

/* ---------- Inline cache ---------------------------------------------- */

/* Map<char *fname -> IrFunction *>. The cache holds the canonical
 * lowered IR for each AST_FLAG_INLINE function; every splice deep-
 * clones from here, so the cache itself is never mutated. */
static Map *inline_ir_cache = NULL;

void irInlineCacheBuild(Cctrl *cc) {
    if (!cc || !cc->ast_list) return;
    if (inline_ir_cache) return;  /* already built */
    inline_ir_cache = mapNew(8, &map_cstring_opaque_type);
    /* Each lowering needs an IrCtx; we stage all inline lowerings
     * through one shared ctx so the global tmp / block-id counters
     * stay coherent. */
    IrCtx *ctx = irCtxNew(cc);
    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind != AST_FUNC) continue;
        if (!(ast->flags & AST_FLAG_INLINE)) continue;
        if (!ast->fname) continue;
        IrFunction *func = irLowerFunction(ctx, ast);
        if (!func) continue;
        /* Promote allocas + constant-fold the body before caching:
         * the splice replaces IR_VAL_PARAM operands with the call
         * site's arg values, so any IR_LOAD that still reads a param
         * via its alloca slot would see a constant where the codegen
         * expects a slot id. mem2reg + fold collapse those loads to
         * direct param references, which DO substitute cleanly. */
        irMem2Reg(func);
        irFoldFunction(func);
        mapAdd(inline_ir_cache, ast->fname->data, (void *)func);
    }
    /* The IrCtx is intentionally leaked: the cached IR holds pointers
     * into ctx->prog and free'ing it would invalidate the cache. The
     * lifetime of the cache is the lifetime of the compile run, so
     * this is fine. */
    (void)ctx;
}

IrFunction *irInlineCacheGet(AoStr *fname) {
    if (!inline_ir_cache || !fname) return NULL;
    return (IrFunction *)mapGetLen(inline_ir_cache,
                                    fname->data, fname->len);
}

/* ---------- Splice pass ----------------------------------------------- */

/* True for callee shapes the first-cut splice can handle: any number
 * of blocks but exactly one IR_RET, and the path from the function
 * entry to that IR_RET is straight-line (only IR_JMP terminators, no
 * IR_BR / IR_LOOP / IR_SWITCH along the way). Branching bodies need
 * a continuation block + multi-pred phi (deferred). */
static int isLinearSingleRet(IrFunction *callee, IrInstr **ret_out) {
    if (!callee || !callee->blocks) return 0;
    int n_ret = 0;
    int has_branch = 0;
    IrInstr *the_ret = NULL;
    listForEach(callee->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op == IR_RET) {
                n_ret++;
                the_ret = I;
            } else if (I->op == IR_BR || I->op == IR_SWITCH ||
                       I->op == IR_LOOP) {
                has_branch = 1;
            }
        }
    }
    if (n_ret != 1) return 0;
    if (has_branch) return 0;
    if (ret_out) *ret_out = the_ret;
    return 1;
}

/* Splice one IR_CALL at `call_node` in `caller_bb` whose callee IR is
 * `callee`. Returns 1 on success (call replaced), 0 if we declined
 * (e.g. callee shape unsupported, arg count mismatch). */
static int spliceOneCall(IrFunction *caller, IrBlock *caller_bb,
                          List *call_node, IrFunction *callee) {
    (void)caller;
    (void)caller_bb;
    IrInstr *call = (IrInstr *)call_node->value;
    if (call->op != IR_CALL) return 0;
    if (!call->r1 || !call->r1->as.array.values) return 0;

    /* Only linear (no-branching) single-RET callees for the first
     * cut. Multi-block branching needs a continuation block + multi-
     * pred phi at the splice site. */
    IrInstr *callee_ret = NULL;
    if (!isLinearSingleRet(callee, &callee_ret)) return 0;

    Vec *args = call->r1->as.array.values;
    if (!callee->params) return 0;
    if (args->size != callee->params->size) return 0;

    /* Build param_subst: every param's var.id -> caller's matching
     * arg IrValue. */
    Map *param_subst = mapNew(8, &map_uint_to_uint_type);
    for (u64 i = 0; i < args->size; ++i) {
        IrValue *p = (IrValue *)callee->params->entries[i];
        IrValue *a = (IrValue *)args->entries[i];
        if (!p || p->kind != IR_VAL_PARAM) {
            mapRelease(param_subst);
            return 0;
        }
        mapAdd(param_subst, (void *)(u64)p->as.var.id, (void *)a);
    }

    /* Clone the callee with param substitution. */
    IrCloneResult *clone = irFunctionClone(callee, param_subst);
    mapRelease(param_subst);
    if (!clone || !clone->entry_block) {
        if (clone) irCloneResultRelease(clone);
        return 0;
    }

    /* Locate the (single) cloned IR_RET across all cloned blocks. */
    IrInstr *cloned_ret = NULL;
    List *cloned_ret_node = NULL;
    IrBlock *cloned_ret_bb = NULL;
    for (List *bn = clone->blocks->next;
         bn != clone->blocks;
         bn = bn->next) {
        IrBlock *bb = (IrBlock *)bn->value;
        for (List *n = bb->instructions->next;
             n != bb->instructions;
             n = n->next) {
            IrInstr *I = (IrInstr *)n->value;
            if (I->op == IR_RET) {
                cloned_ret = I;
                cloned_ret_node = n;
                cloned_ret_bb = bb;
                break;
            }
        }
        if (cloned_ret) break;
    }
    if (!cloned_ret) {
        irCloneResultRelease(clone);
        return 0;
    }

    /* The return value is whatever the IR_RET pointed at (cloneInstr
     * already substituted params and remapped tmps for us). */
    IrValue *ret_value = cloned_ret->dst;

    /* If the call had a non-void dst tmp, the caller's downstream uses
     * still reference call->dst; rewire by finding the LAST cloned
     * instruction (across all blocks) that *defines* ret_value and
     * making it write into call->dst instead. Handles arithmetic /
     * load / lea / etc. - the common case. If ret_value isn't a tmp
     * defined inside the body (constant return, or a passed-through
     * param), we can't redirect; decline. */
    if (call->dst && call->dst->kind == IR_VAL_TMP) {
        if (!ret_value || ret_value->kind != IR_VAL_TMP) {
            irCloneResultRelease(clone);
            return 0;
        }
        IrInstr *def = NULL;
        for (List *bn = clone->blocks->next;
             bn != clone->blocks;
             bn = bn->next) {
            IrBlock *bb = (IrBlock *)bn->value;
            for (List *n = bb->instructions->next;
                 n != bb->instructions;
                 n = n->next) {
                IrInstr *I = (IrInstr *)n->value;
                if (I == cloned_ret) continue;
                if (I->op == IR_NOP) continue;
                if (I->dst && I->dst->kind == IR_VAL_TMP &&
                    I->dst->as.var.id == ret_value->as.var.id) {
                    def = I;
                }
            }
        }
        if (!def) {
            irCloneResultRelease(clone);
            return 0;
        }
        def->dst = call->dst;
    }

    /* Drop the IR_RET node from its block. */
    cloned_ret_node->prev->next = cloned_ret_node->next;
    cloned_ret_node->next->prev = cloned_ret_node->prev;
    (void)cloned_ret_bb;

    /* Splice every remaining cloned instruction (skipping IR_JMP -
     * the linear inter-block stitching collapses on inline) into
     * caller_bb just before call_node, in source order. */
    for (List *bn = clone->blocks->next;
         bn != clone->blocks;
         bn = bn->next) {
        IrBlock *bb = (IrBlock *)bn->value;
        for (List *n = bb->instructions->next;
             n != bb->instructions; ) {
            List *next = n->next;
            IrInstr *I = (IrInstr *)n->value;
            if (I->op == IR_JMP) {
                /* Detach but don't reattach: the cloned linear flow
                 * is now a single straight-line sequence in
                 * caller_bb. */
                n->prev->next = n->next;
                n->next->prev = n->prev;
                n = next;
                continue;
            }
            /* Detach from cloned block. */
            n->prev->next = n->next;
            n->next->prev = n->prev;
            /* Insert before call_node in caller_bb. */
            n->prev = call_node->prev;
            n->next = call_node;
            call_node->prev->next = n;
            call_node->prev = n;
            n = next;
        }
    }

    /* Remove the IR_CALL itself. */
    call_node->prev->next = call_node->next;
    call_node->next->prev = call_node->prev;

    irCloneResultRelease(clone);
    return 1;
}

void irInlineSplice(IrFunction *caller) {
    if (!caller || !caller->blocks) return;
    if (!inline_ir_cache) return;
    listForEach(caller->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        /* The list mutates as we splice - walk via raw nodes and
         * advance to the next BEFORE the splice (we never look at the
         * cloned-in nodes since they don't carry IR_CALL ops in
         * single-block-RET shape, and even if they did, they came
         * from a cache copy so a re-pass would be idempotent up to
         * fixed point). */
        for (List *n = bb->instructions->next;
             n != bb->instructions; ) {
            List *next = n->next;
            IrInstr *I = (IrInstr *)n->value;
            if (I->op == IR_CALL && I->r1 && I->r1->as.array.label) {
                IrFunction *callee = irInlineCacheGet(
                    I->r1->as.array.label);
                if (callee) {
                    spliceOneCall(caller, bb, n, callee);
                }
            }
            n = next;
        }
    }
}
