#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "ast.h"
#include "ir.h"
#include "ir-eval.h"
#include "ir-optimise.h"
#include "ir-regalloc.h"
#include "ir-types.h"
#include "ir-debug.h"
#include "prsutil.h"
#include "util.h"

/* Forward decl for inc/dec lowering on AST_CLASS_REF, defined below
 * alongside the assignment lowering. */
typedef struct IrAssignTarget {
    IrValue *target;
    AstType *type;
    int indirect;
} IrAssignTarget;

/* Forward declarations */
static IrAssignTarget irLowerAssignTarget(IrCtx *ctx, Ast *lhs);
static IrValue *irLowerAssign(IrCtx *ctx, Ast *ast);
void irLowerAst(IrCtx *ctx, Ast *ast);

static void irPushLoopCtx(IrCtx *ctx,
                          IrBlock *continue_blk,
                          IrBlock *break_blk)
{
    if (ctx->loop_depth >= IR_LOOP_STACK_MAX) {
        loggerPanic("Loop nesting above %d not supported in slice\n",
                    IR_LOOP_STACK_MAX);
    }
    ctx->loop_stack[ctx->loop_depth].continue_block = continue_blk;
    ctx->loop_stack[ctx->loop_depth].break_block = break_blk;
    ctx->loop_depth++;
}

static void irPopLoopCtx(IrCtx *ctx) {
    if (ctx->loop_depth == 0) {
        loggerPanic("irPopLoopCtx underflow\n");
    }
    ctx->loop_depth--;
}

static IrLoopCtx *irCurLoopCtx(IrCtx *ctx) {
    if (ctx->loop_depth == 0) return NULL;
    return &ctx->loop_stack[ctx->loop_depth - 1];
}

/* Collapse something that looks like `a.b.c` or `p->a.b`*/
static void irCollapseClassRefChain(Ast **cls, int *offset) {
    while (*cls && (*cls)->kind == AST_CLASS_REF) {
        *offset += (*cls)->type->offset;
        *cls = (*cls)->cls;
    }
}

void irBlockAddInstr(IrCtx *ctx, IrInstr *instr) {
    listAppend(ctx->cur_block->instructions, instr);
}

IrValue *irConstFloat(IrValueType type, f64 _f64) {
    IrValue *ir_value = irValueNew(type, IR_VAL_CONST_FLOAT);
    ir_value->as._f64 = _f64;
    return ir_value;
}

IrInstr *irAlloca(AstType *ast_type) {
    IrValueType ir_type = irConvertType(ast_type);
    IrValue *ir_size = irConstInt(ir_type, ast_type->size);
    IrValue *tmp = irTmp(ir_type, ast_type->size);
    IrInstr *ir_alloca = irInstrNew(IR_ALLOCA, tmp, ir_size, NULL);
    return ir_alloca;
}

IrInstr *irICmp(IrValue *result,
                IrCmpKind kind,
                IrValue *op1, 
                IrValue *op2)
{
    if (op1->type == IR_TYPE_PTR || op2->type == IR_TYPE_PTR) {
        if      (kind == IR_CMP_LT) kind = IR_CMP_ULT;
        else if (kind == IR_CMP_LE) kind = IR_CMP_ULE;
        else if (kind == IR_CMP_GT) kind = IR_CMP_UGT;
        else if (kind == IR_CMP_GE) kind = IR_CMP_UGE;
    }

    IrInstr *instr = irInstrNew(IR_ICMP, result, op1, op2);
    instr->extra.cmp_kind = kind;
    return instr;
}

IrInstr *irBranch(IrFunction *func,
                  IrBlock *block,
                  IrValue *cond,
                  IrBlock *true_block,
                  IrBlock *false_block)
{
    if (!block || !cond || !true_block || !false_block) {
        loggerPanic("irBranch: NULL parameter provided\n");
    }

    if (cond->type != IR_TYPE_I8) {
        int isa_bool = 0;
        if (!listEmpty(block->instructions)) {
            IrInstr *last = (IrInstr *)block->instructions->prev->value;
            if (irOpIsCmp(last->op) && last->dst == cond) {
                isa_bool = 1;
            }
        }
        
        if (!isa_bool) {
            IrValue *zero = irConstInt(IR_TYPE_I8, 0);
            IrValue *bool_cond = irTmp(IR_TYPE_I8, 1);
            IrInstr *cmp = irICmp(bool_cond, IR_CMP_NE, cond, zero);
            listAppend(block->instructions, cmp);
            cond = bool_cond;
        }
    }

    IrInstr *instr = irInstrNew(IR_BR, cond, NULL, NULL);
    instr->extra.blocks.target_block = true_block;
    instr->extra.blocks.fallthrough_block = false_block;

    listAppend(block->instructions, instr);
    block->sealed = 1;

    irFunctionAddMapping(func, block, true_block);
    irFunctionAddMapping(func, block, false_block);

    return instr;
}

/* result is where we are storing something and op1 is the thing we are storing 
 * I think op1 could/shoule have an offset as it is either going to be the 
 * stack or it is going to be a struct/pointer offset? */
IrInstr *irStore(IrValue *ir_dest, IrValue *ir_value) {
    return irInstrNew(IR_STORE, ir_dest, ir_value, NULL);
}

/* The parser wraps body-side references to default-param parameters in
 * AST_DEFAULT_PARAM. The default expression matters only at the call
 * site; in the body we want the underlying lvar. Returns `ast` unchanged
 * when it isn't a default-param wrapper. */
static Ast *irUnwrapDefaultParam(Ast *ast) {
    if (ast && ast->kind == AST_DEFAULT_PARAM && ast->declvar) {
        return ast->declvar;
    }
    return ast;
}

static IrValue *irNarrowToTargetWidth(IrCtx *ctx,
                                      IrValue *val,
                                      AstType *target_ty)
{
    if (!val || !target_ty || target_ty->size <= 0) return val;
    if (irIsFloat(val->type)) return val;
    IrValueType narrow_ty = irConvertType(target_ty);
    if (val->kind == IR_VAL_CONST_INT) {
        if (narrow_ty != val->type) {
            return irConstInt(narrow_ty, val->as._i64);
        }
        return val;
    }

    /* Tmps, locals and params all carry a width in `as.var.size`. A
     * wider source assigned to a narrower slot/field must be truncated;
     * otherwise the codegen sizes the store from the source width and
     * the extra bytes clobber adjacent storage (e.g. a U8 field store
     * spilling over a neighbouring field or a saved frame register). */
    if ((val->kind == IR_VAL_TMP || val->kind == IR_VAL_LOCAL ||
         val->kind == IR_VAL_PARAM) &&
        val->as.var.size > (u64)target_ty->size)
    {
        IrValue *narrow = irTmp(narrow_ty, target_ty->size);
        irBlockAddInstr(ctx, irInstrNew(IR_TRUNC, narrow, val, NULL));
        return narrow;
    }

    return val;
}

/* Widen a value to match a target slot's width. Caller supplies the
 * source AST type so we know whether to zero- or sign-extend. Needed
 * before IR_STORE into a wider slot: the codegen sizes the store from
 * the value's byte size, so storing an i8 into an i64 slot leaves the
 * top 7 bytes stale. Without explicit widening (or mem2reg promotion),
 * a subsequent full-width read of that slot gets garbage. */
static IrValue *irWidenToTargetWidth(IrCtx *ctx,
                                     IrValue *val,
                                     AstType *src_ty,
                                     AstType *target_ty)
{
    if (!val || !target_ty || target_ty->size <= 0) return val;
    if (irIsFloat(val->type)) return val;
    /* Tmps, locals and params all carry a width in `as.var.size`. A
     * narrow source stored into a wider slot must be sign-/zero-extended
     * to the full width; otherwise the codegen sizes the store from the
     * source width, leaving the upper bytes stale (and a signed value
     * never gets its sign bits). Mirrors irNarrowToTargetWidth. */
    if (val->kind != IR_VAL_TMP && val->kind != IR_VAL_LOCAL &&
        val->kind != IR_VAL_PARAM)
        return val;
    if (val->as.var.size >= (u64)target_ty->size) return val;

    IrValueType wide_ty = irConvertType(target_ty);
    IrValue *wide = irTmp(wide_ty, target_ty->size);
    int sext = src_ty && src_ty->issigned;
    IrOp ext = sext ? IR_SEXT : IR_ZEXT;
    irBlockAddInstr(ctx, irInstrNew(ext, wide, val, NULL));
    return wide;
}

/* Convert a float value to the target float slot's width. The codegen
 * sizes a float store from the value's byte width, so storing an F64
 * value into an F32 slot (or vice versa) would write the wrong bytes -
 * the raw double bit pattern truncated, not the single-precision value.
 * Emit an fptrunc / fpext (or re-type a literal) so the bits match. */
static IrValue *irConvertFloatToTargetWidth(IrCtx *ctx,
                                            IrValue *val,
                                            AstType *target_ty)
{
    if (!val || !target_ty || target_ty->size <= 0) return val;
    if (!irIsFloat(val->type)) return val;
    if (target_ty->kind != AST_TYPE_FLOAT) return val;

    IrValueType ft = irConvertType(target_ty);
    if (val->type == ft) return val;

    int val_sz = (int)irValueByteSize(val);
    int to_sz = target_ty->size;
    if (val->kind == IR_VAL_CONST_FLOAT) {
        return irConstFloat(ft, val->as._f64);
    }
    IrValue *dst = irTmp(ft, to_sz);
    IrOp op = (to_sz < val_sz) ? IR_FPTRUNC : IR_FPEXT;
    irBlockAddInstr(ctx, irInstrNew(op, dst, val, NULL));
    return dst;
}

/* Widen a float value to `ty` (a wider float type). Re-types a literal
 * in place, otherwise emits an fpext. Used to equalise the operand
 * widths of a mixed-precision float binop (e.g. F32 + F64). */
static IrValue *irFloatExtend(IrCtx *ctx, IrValue *v, IrValueType ty, int sz) {
    if (v->kind == IR_VAL_CONST_FLOAT) return irConstFloat(ty, v->as._f64);
    IrValue *dst = irTmp(ty, sz);
    irBlockAddInstr(ctx, irInstrNew(IR_FPEXT, dst, v, NULL));
    return dst;
}

/* Redirect the most recent value-producing instruction's dst to
 * `target`. Returns 1 on success (caller can skip emitting an IR_STORE
 * since the producer wrote straight into the slot), 0 if no redirect
 * was possible (constant, slot-direct read, value produced elsewhere,
 * unsafe op like PHI). The "safe to redirect" set matches the DCE
 * pass's pure-value-producer list. */
static int irRedirectLastDst(IrCtx *ctx, IrValue *val, IrValue *target) {
    if (!val || !target) return 0;
    if (!irIsTmp(val)) return 0;
    if (!ctx->cur_block || listEmpty(ctx->cur_block->instructions)) return 0;
    for (List *node = ctx->cur_block->instructions->prev;
         node != ctx->cur_block->instructions;
         node = node->prev)
    {
        IrInstr *I = (IrInstr *)node->value;
        if (I->op == IR_NOP) continue;
        if (I->dst != val) return 0;
        switch (I->op) {
            case IR_LOAD:
            case IR_LOAD_DEREF:
            case IR_LEA:
            case IR_IADD: case IR_ISUB: case IR_IMUL:
            case IR_AND:  case IR_OR:   case IR_XOR:
            case IR_SHL:  case IR_SHR:  case IR_SAR:
            case IR_IDIV: case IR_UDIV: case IR_IREM: case IR_UREM:
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
            case IR_CALL:
                I->dst = target;
                return 1;
            default:
                return 0;
        }
    }
    return 0;
}

static int irBinOpIsCompoundAssign(AstBinOp op) {
    switch (op) {
    case AST_BIN_OP_ADD_ASSIGN:
    case AST_BIN_OP_SUB_ASSIGN:
    case AST_BIN_OP_MUL_ASSIGN:
    case AST_BIN_OP_DIV_ASSIGN:
    case AST_BIN_OP_MOD_ASSIGN:
    case AST_BIN_OP_SHL_ASSIGN:
    case AST_BIN_OP_SHR_ASSIGN:
    case AST_BIN_OP_AND_ASSIGN:
    case AST_BIN_OP_XOR_ASSIGN:
    case AST_BIN_OP_OR_ASSIGN:
        return 1;
    default:
        return 0;
    }
}

/* True when a function returns a non-intrinsic class / union by value -
 * i.e., the hidden out-pointer convention applies. The caller allocates a
 * buffer of `sizeof(rettype)`, passes its address as a hidden first arg
 * (in `rdi` on `x86_64`), and the callee writes the struct bytes there;
 * the call returns the buffer pointer in `rax`. */
static int irRetTypeIsAggregate(AstType *rettype) {
    if (!rettype) return 0;
    if (rettype->kind != AST_TYPE_CLASS && rettype->kind != AST_TYPE_UNION)
        return 0;
    if (rettype->is_intrinsic) return 0;
    return rettype->size > 0;
}

/* A struct return is passed INDIRECTLY (hidden out-pointer the caller
 * supplies and the callee writes through) only when it is larger than 16
 * bytes. This 16-byte threshold is the same for AArch64 AAPCS and x86-64
 * SysV, so the indirect-vs-register decision is ABI-agnostic and lives here;
 * *which* registers a <=16-byte aggregate uses is decided per-backend
 * (astAapcsClassify / astSysvClassify). */
static int irRetIsIndirect(AstType *rettype) {
    return irRetTypeIsAggregate(rettype) && rettype->size > 16;
}
static int irRetIsRegisterAggregate(AstType *rettype) {
    return irRetTypeIsAggregate(rettype) && rettype->size <= 16;
}

/* Make a call to holyc's memcpy routine. Used for struct by value returns. 
 * Simpler to do this than emit a bunch of instructions and from codegen is 
 * easy enough to reason with without feeling like magic. */
static void irEmitMemcpy(IrCtx *ctx,
                         IrValue *dst, 
                         IrValue *src,
                         int n_bytes)
{
    IrValue *args_wrap = irValueNew(IR_TYPE_ARRAY, IR_VAL_UNRESOLVED);
    /* libc memcpy(dst, src, n) -> dst; resolves via libc for both the
     * AOT link and JIT dlsym. */
    args_wrap->as.array.label = aoStrPrintf("memcpy");
    Vec *args = irValueVecNew();
    args_wrap->as.array.values = args;
    vecPush(args, dst);
    vecPush(args, src);
    vecPush(args, irConstInt(IR_TYPE_I64, n_bytes));

    /* MEMCPY's return is `U0 *` (the dst pointer); we don't consume
     * it. The codegen reserves a slot for the dst tmp anyway, but the
     * use-counting + zero-use slot-skip in irOptDeadCodeElim will prune
     * it. */
    IrValue *ret = irTmp(IR_TYPE_PTR, 8);
    IrInstr *call = irInstrNew(IR_CALL, ret, args_wrap, NULL);
    irBlockAddInstr(ctx, call);
}

/* Emit a one-argument call to a HolyC runtime function by name. The
 * args wrapper's label is the bare function name (no `_` prefix);
 * asmNormaliseFunctionName adds the leading underscore at emit time.
 * Returns the call's dst tmp (for the I64-returning variant) or
 * NULL for the void variant. */
static IrValue *irEmitRuntimeCall1(IrCtx *ctx, const char *fname,
                                    IrValueType ret_type, int ret_size,
                                    IrValue *arg) {
    IrValue *args_wrap = irValueNew(IR_TYPE_ARRAY, IR_VAL_UNRESOLVED);
    args_wrap->as.array.label = aoStrPrintf("%s", fname);
    Vec *args = irValueVecNew();
    args_wrap->as.array.values = args;
    vecPush(args, arg);
    IrValue *ret = irTmp(ret_type, ret_size);
    IrInstr *call = irInstrNew(IR_CALL, ret, args_wrap, NULL);
    irBlockAddInstr(ctx, call);
    return ret;
}


IrValue *irFnCallTo(IrCtx *ctx, Ast *ast, IrValue *preallocated_buffer) {
    /* Is this returning a struct from the stack? */
    int agg_return = irRetTypeIsAggregate(ast->type);
    IrValueType ret_type = agg_return ? IR_TYPE_PTR : irConvertType(ast->type);
    int ret_size = agg_return ? 8 : ast->type->size;
    IrValue *ir_call_args = irValueNew(IR_TYPE_ARRAY, IR_VAL_UNRESOLVED);
    IrValue *ir_ret_val = irTmp(ret_type, ret_size);
    IrInstr *ir_call_instr = irInstrNew(IR_CALL, ir_ret_val, ir_call_args, NULL);

    if (ast->flags & AST_FLAG_BUILTIN) {
        ir_call_instr->flags |= IRCG_FN_BUILTIN;
    }

    if (agg_return) {
        ir_call_instr->flags |= IRCG_CALL_AGG_RETURN;
        /* Carry the struct return type on the call result so the backend
         * can classify it: >16 bytes uses the hidden out-pointer (args[0] is
         * the ABI arg), <=16 bytes is returned in registers (args[0] is the
         * post-call store destination, not an ABI arg). */
        ir_ret_val->byval_struct_type = ast->type;
    }

    assert(ast->kind == AST_FUNCALL || ast->kind == AST_FUNPTR_CALL ||
           ast->kind == AST_ASM_FUNCALL);

    /* Functions call arguments */
    Vec *args = irValueVecNew();
    ir_call_args->as.array.values = args;

    /* Push the out buffer to the function calls arguments, using the
     * callers preallocated buffer. Else create a new slot. */
    if (agg_return) {
        IrValue *buf_addr;
        if (preallocated_buffer) {
            buf_addr = preallocated_buffer;
        } else {
            IrInstr *buf_alloca = irAlloca(ast->type);
            irAddStackSpace(ctx, ast->type->size);
            irBlockAddInstr(ctx, buf_alloca);
            buf_addr = irTmp(IR_TYPE_PTR, 8);
            irBlockAddInstr(ctx,
                    irInstrNew(IR_LEA, buf_addr, buf_alloca->dst, NULL));
        }
        vecPush(args, buf_addr);
    }

    /* Direct call: store the static function name on the wrapper.
     * Indirect call: leave label NULL and stash the function-pointer
     * source in r2; codegen knows to load it into a register and emit
     * `call *<reg>`. */
    if (ast->kind == AST_FUNPTR_CALL) {
        ir_call_args->as.array.label = NULL;
        IrValue *ptr = irExpr(ctx, ast->ref);
        ir_call_instr->r2 = ptr;
    } else {
        ir_call_args->as.array.label = ast->fname;
    }

    /* Default-parameter fill-in already happened in the parser
     * (parseFlattenDefaultArgs), so ast->args is the complete
     * argument list - just lower each entry. A by-value struct arg is
     * tagged with its source type so the backend can pass it per the
     * platform C ABI (HFA / register classification). */
    u64 n_args = ast->args ? ast->args->size : 0;
    for (u64 i = 0; i < n_args; ++i) {
        Ast *ast_arg = (Ast *)ast->args->entries[i];
        if (!ast_arg) break;
        IrValue *ir_arg = irExpr(ctx, ast_arg);
        if (ast_arg->type &&
            (ast_arg->type->kind == AST_TYPE_CLASS ||
             ast_arg->type->kind == AST_TYPE_UNION) &&
            !ast_arg->type->is_intrinsic)
        {
            /* Mark this value as a by-value struct arg so the backend
             * applies the platform C ABI. irExpr returns a value private to
             * this read for an aggregate (a fresh address tmp - see
             * irLowerLVar / irLowerGlobal / irLowerClassRef), so tagging it
             * does not leak onto a value shared with other instructions. */
            ir_arg->byval_struct_type = ast_arg->type;
        }
        vecPush(args, ir_arg);
    }

    irBlockAddInstr(ctx, ir_call_instr);
    return ir_ret_val;
}

IrValue *irLowerFnCall(IrCtx *ctx, Ast *ast) {
    return irFnCallTo(ctx, ast, NULL);
}

/* Binary expressions are assumed to always be assigning to something. I'm not
 * 100% sure this is a valid assumption to make. Well I guess;
 * `I64 x = y + 32 * 10` _could_ continually be assigned to `x` */
IrValue *irLowerBinOpExpr(IrCtx *ctx, Ast *ast) {
    /* HolyC range comparison: `a OP1 b OP2 c` parses as
     * `binop=OP2, left=(binop=OP1, a, b), right=c`. Semantics:
     * `(a OP1 b) && (b OP2 c)`. Lower as a short-circuit AND of two
     * regular comparisons. b is re-evaluated for OP2 - matches AST
     * codegen (asmRangeOperation), and matters when b has side effects. */
    if (astIsRangeOperator(ast->binop) && ast->left &&
        ast->left->kind == AST_BINOP &&
        astIsRangeOperator(ast->left->binop)) {
        IrBlock *ir_right_block = irBlockNew();
        IrBlock *ir_end_block = irBlockNew();

        IrValue *left = irExpr(ctx, ast->left);   /* a OP1 b */
        IrBlock *ir_block = ctx->cur_block;
        IrValue *ir_result = irTmp(IR_TYPE_I8, 1);

        irBranch(ctx->cur_func, ir_block, left, ir_right_block, ir_end_block);
        irFnAddBlock(ctx->cur_func, ir_right_block);
        ctx->cur_block = ir_right_block;

        /* Second comparison: synthesize `b OP2 c` and lower normally so
         * the existing int/float/signed-vs-unsigned dispatch kicks in. */
        int is_err = 0;
        Ast *rhs_cmp = astBinaryOp(ast->binop, ast->left->right,
                                   ast->right, &is_err);
        IrValue *right = irExpr(ctx, rhs_cmp);
        IrBlock *right_end = ctx->cur_block;

        IrInstr *jump_instr = irJump(ctx->cur_func, ctx->cur_block,
                                     ir_end_block);
        irBlockAddInstr(ctx, jump_instr);
        irFnAddBlock(ctx->cur_func, ir_end_block);
        ctx->cur_block = ir_end_block;

        IrInstr *phi_instr = irPhi(ir_result);
        irBlockAddInstr(ctx, phi_instr);
        irAddPhiIncoming(phi_instr, irConstInt(IR_TYPE_I8, 0), ir_block);
        irAddPhiIncoming(phi_instr, right, right_end);
        return ir_result;
    }

    /* Logical AND/OR short-circuit: evaluate left in the current block;
     * branch on it; evaluate right only in the "right" block. Can't
     * eagerly evaluate both operands at the top - that would emit the
     * RHS computation in the predecessor block where it shouldn't run. */
    if (ast->binop == AST_BIN_OP_LOG_AND) {
        IrBlock *ir_right_block = irBlockNew();
        IrBlock *ir_end_block = irBlockNew();

        IrValue *left = irExpr(ctx, ast->left);
        IrBlock *ir_block = ctx->cur_block;
        IrValue *ir_result = irTmp(IR_TYPE_I8, 1);

        irBranch(ctx->cur_func, ir_block, left, ir_right_block, ir_end_block);
        irFnAddBlock(ctx->cur_func, ir_right_block);
        ctx->cur_block = ir_right_block;

        IrValue *right = irExpr(ctx, ast->right);
        IrBlock *right_end = ctx->cur_block;

        IrInstr *jump_instr = irJump(ctx->cur_func, ctx->cur_block, ir_end_block);
        irBlockAddInstr(ctx, jump_instr);
        irFnAddBlock(ctx->cur_func, ir_end_block);
        ctx->cur_block = ir_end_block;

        IrInstr *phi_instr = irPhi(ir_result);
        irBlockAddInstr(ctx, phi_instr);
        irAddPhiIncoming(phi_instr, irConstInt(IR_TYPE_I8, 0), ir_block);
        irAddPhiIncoming(phi_instr, right, right_end);
        return ir_result;
    } else if (ast->binop == AST_BIN_OP_LOG_OR) {
        IrBlock *ir_right_block = irBlockNew();
        IrBlock *ir_end_block = irBlockNew();

        IrValue *left = irExpr(ctx, ast->left);
        IrBlock *ir_block = ctx->cur_block;
        IrValue *ir_result = irTmp(IR_TYPE_I8, 1);

        /* If left is truthy, short-circuit to end with result=1. */
        irBranch(ctx->cur_func, ir_block, left, ir_end_block, ir_right_block);
        irFnAddBlock(ctx->cur_func, ir_right_block);
        ctx->cur_block = ir_right_block;

        IrValue *right = irExpr(ctx, ast->right);
        IrBlock *right_end = ctx->cur_block;

        IrInstr *jump_instr = irJump(ctx->cur_func, ctx->cur_block, ir_end_block);
        irBlockAddInstr(ctx, jump_instr);
        irFnAddBlock(ctx->cur_func, ir_end_block);
        ctx->cur_block = ir_end_block;

        IrInstr *phi_instr = irPhi(ir_result);
        irBlockAddInstr(ctx, phi_instr);
        irAddPhiIncoming(phi_instr, irConstInt(IR_TYPE_I8, 1), ir_block);
        irAddPhiIncoming(phi_instr, right, right_end);
        return ir_result;
    }

    IrValue *lhs = irExpr(ctx, ast->left);
    IrValue *rhs = irExpr(ctx, ast->right);
    IrValueType ir_type = irConvertType(ast->type);
    IrValue *ir_result = irTmp(ir_type, ast->type->size);
    IrOp op;
    IrCmpKind cmp = IR_CMP_INVALID;

    /* Pointer - pointer: result is element count C semantics, not byte count.
     * Subtract as ints, then divide by sizeof(element). */
    if (astIsBinOpKind(ast, AST_BIN_OP_SUB) &&
        ast->left && ast->right &&
        (astTypeIsPtr(ast->left->type) || astTypeIsArray(ast->left->type)) &&
        (astTypeIsPtr(ast->right->type) || astTypeIsArray(ast->right->type)))
    {
        AstType *elem = ast->left->type->ptr;
        int scale = elem ? elem->size : 1;
        if (scale < 1) scale = 1;
        IrValue *diff = irTmp(IR_TYPE_I64, 8);
        irBlockAddInstr(ctx, irInstrNew(IR_ISUB, diff, lhs, rhs));
        if (scale == 1) return diff;
        IrValue *k = irConstInt(IR_TYPE_I64, scale);
        irBlockAddInstr(ctx, irInstrNew(IR_IDIV, ir_result, diff, k));
        return ir_result;
    }

    /* Pointer arithmetic: if the result is pointer/array, scale the int
     * operand by sizeof(element). */
    if ((astTypeIsPtr(ast->type) || astTypeIsArray(ast->type)) &&
        (astIsBinOpKind(ast, AST_BIN_OP_ADD) || astIsBinOpKind(ast, AST_BIN_OP_SUB))) {
        AstType *elem = ast->type->ptr;
        int scale = elem ? elem->size : 1;
        if (scale < 1) scale = 1;
        /* Figure out which operand is the pointer/array (the "base") and
         * which is the int (the "index"). One of them must be ptr/array;
         * the other gets scaled. */
        Ast *ast_lhs = ast->left;
        int left_is_ptr = ast_lhs &&
            (astTypeIsPtr(ast_lhs->type) || astTypeIsArray(ast_lhs->type));

        IrValue *base = left_is_ptr ? lhs : rhs;
        IrValue *idx  = left_is_ptr ? rhs : lhs;
        if (scale != 1) {
            IrValue *scaled = irTmp(IR_TYPE_I64, 8);
            IrValue *k = irConstInt(IR_TYPE_I64, scale);
            irBlockAddInstr(ctx, irInstrNew(IR_IMUL, scaled, idx, k));
            idx = scaled;
        }
        IrOp pop = astIsBinOpKind(ast, AST_BIN_OP_ADD) ? IR_IADD : IR_ISUB;
        irBlockAddInstr(ctx, irInstrNew(pop, ir_result, base, idx));
        return ir_result;
    }

    Ast *ast_lhs = ast->left;
    Ast *ast_rhs = ast->right;

    int is_cmp = astIsBinOpCmp(ast);
    int left_is_float = ast_lhs && astIsFloatType(ast_lhs->type);
    int right_is_float = ast_rhs && astIsFloatType(ast_rhs->type);
    int float_dispatch;

    if (is_cmp) {
        float_dispatch = left_is_float && right_is_float;
        /* Mixed-type compare: truncate the float operand to int so ICMP
         * sees two integers, matching HolyC AST codegen. */
        if (!float_dispatch && (left_is_float || right_is_float)) {
            IrValue *trunc_dst = irTmp(IR_TYPE_I64, 8);
            if (left_is_float) {
                irBlockAddInstr(ctx, irInstrNew(IR_FPTOSI, trunc_dst, lhs, NULL));
                lhs = trunc_dst;
            } else {
                irBlockAddInstr(ctx, irInstrNew(IR_FPTOSI, trunc_dst, rhs, NULL));
                rhs = trunc_dst;
            }
        }

        /* Force result tmp to int for the ICMP/FCMP path below. The
         * parser may type a comparison as float (mislabel) or as a
         * pointer / function-pointer kind (`fp1 == fp2`); the runtime
         * result is always 0/1. */
        if (ir_type != IR_TYPE_I64) {
            ir_type = IR_TYPE_I64;
            ir_result = irTmp(ir_type, 8);
        }
    } else {
        float_dispatch = irIsFloat(ir_type) || left_is_float ||
                         right_is_float;
        /* Mixed int/float arithmetic: promote the int side to the float
         * operand's type so the IR_F* op sees two same-width float
         * operands. Without this, e.g. `f64 * -1` lowered to
         * fmul(f64, i64) which the codegen treats as a fmul of garbage.
         * The promotion width must match the float operand (F32 vs F64)
         * or the codegen picks the wrong register width. */
        if (float_dispatch && !(left_is_float && right_is_float)) {
            IrValueType fty = left_is_float ? lhs->type : rhs->type;
            int fsz = left_is_float ? (int)irValueByteSize(lhs)
                                    : (int)irValueByteSize(rhs);
            IrValue *prom = irTmp(fty, fsz);
            IrInstr *cast_instr = NULL;
            if (!left_is_float) {
                cast_instr = irInstrNew(IR_SITOFP, prom, lhs, NULL);
                lhs = prom;
            }
            if (!right_is_float) {
                cast_instr = irInstrNew(IR_SITOFP, prom, rhs, NULL);
                rhs = prom;
            }
            irBlockAddInstr(ctx, cast_instr);
        }
    }

    /* Equalise float operand widths: a mixed F32/F64 op promotes the
     * narrower operand to the wider so the codegen sees two same-width
     * floats (matches C's usual arithmetic conversions). */
    if (float_dispatch && irIsFloat(lhs->type) && irIsFloat(rhs->type)) {
        int lsz = (int)irValueByteSize(lhs);
        int rsz = (int)irValueByteSize(rhs);
        if (lsz < rsz) {
            lhs = irFloatExtend(ctx, lhs, rhs->type, rsz);
        } else if (rsz < lsz) {
            rhs = irFloatExtend(ctx, rhs, lhs->type, lsz);
        }
    }

    if (float_dispatch) {
        switch (ast->binop) {
            case AST_BIN_OP_ADD:
                op = IR_FADD;
                break;
            case AST_BIN_OP_MUL:
                op = IR_FMUL;
                break;
            case AST_BIN_OP_DIV:
                op = IR_FDIV;
                break;
            case AST_BIN_OP_SUB:
                op = IR_FSUB;
                break;
            case AST_BIN_OP_LT:
                op = IR_FCMP;
                cmp = IR_CMP_LT;
                break;
            case AST_BIN_OP_LE:
                op = IR_FCMP;
                cmp = IR_CMP_LE;
                break;
            case AST_BIN_OP_GT:
                op = IR_FCMP;
                cmp = IR_CMP_GT;
                break;
            case AST_BIN_OP_GE:
                op = IR_FCMP;
                cmp = IR_CMP_GE;
                break;
            case AST_BIN_OP_EQ:
                op = IR_FCMP;
                cmp = IR_CMP_EQ;
                break;
            case AST_BIN_OP_NE:
                op = IR_FCMP;
                cmp = IR_CMP_NE;
                break;
            default:
                loggerPanic("Op `%s` not handled for float \n",
                        astBinOpKindToString(ast->binop));
        }
    } else if (irIsInt(ir_type)) {
        switch (ast->binop) {
            case AST_BIN_OP_ADD:
                op = IR_IADD;
                break;
            case AST_BIN_OP_MUL:
                op = IR_IMUL;
                break;
            case AST_BIN_OP_DIV:
                if (ast->type->issigned) {
                    op = IR_IDIV;
                } else {
                    op = IR_UDIV;
                }
                break;
            case AST_BIN_OP_MOD:
                if (ast->type->issigned) {
                    op = IR_IREM;
                } else {
                    op = IR_UREM;
                }
                break;
            case AST_BIN_OP_SUB:
                op = IR_ISUB;
                break;
            case AST_BIN_OP_SHL:
                op = IR_SHL;
                break;
            case AST_BIN_OP_SHR:
                /* HolyC `>>` is arithmetic for signed types (matches the
                 * AST codegen's `sarq`). Use logical shift only for
                 * unsigned types. */
                op = ast->type->issigned ? IR_SAR : IR_SHR;
                break;
            case AST_BIN_OP_BIT_AND:
                op = IR_AND;
                break;
            case AST_BIN_OP_BIT_XOR:
                op = IR_XOR;
                break;
            case AST_BIN_OP_BIT_OR:
                op = IR_OR;
                break;
            case AST_BIN_OP_LT:
                op = IR_ICMP;
                if (ast->type->issigned) {
                    cmp = IR_CMP_LT;
                } else {
                    cmp = IR_CMP_ULT;
                }
                break;
            case AST_BIN_OP_LE:
                op = IR_ICMP;
                if (ast->type->issigned) {
                    cmp = IR_CMP_LE;
                } else {
                    cmp = IR_CMP_ULE;
                }
                break;
            case AST_BIN_OP_GT:
                op = IR_ICMP;
                if (ast->type->issigned) {
                    cmp = IR_CMP_GT;
                } else {
                    cmp = IR_CMP_UGT;
                }
                break;
            case AST_BIN_OP_GE:
                op = IR_ICMP;
                if (ast->type->issigned) {
                    cmp = IR_CMP_GE;
                } else {
                    cmp = IR_CMP_UGE;
                }
                break;
            case AST_BIN_OP_EQ:
                op = IR_ICMP;
                cmp = IR_CMP_EQ;
                break;
            case AST_BIN_OP_NE:
                op = IR_ICMP;
                cmp = IR_CMP_NE;
                break;
            default:
                loggerPanic("Op `%s` not handled for int\n",
                        astBinOpKindToString(ast->binop));
        }
    } else {
        loggerPanic("Unhandled Ir type: %s\n", irValueTypeToString(ir_type));
    }

    /* Correct the parser mislabelling `f1 < f2` comparisons */
    if (op == IR_FCMP) {
        ir_result = irTmp(IR_TYPE_I64, 8);
    }
    IrInstr *instr = irInstrNew(op, ir_result, lhs, rhs);
    instr->extra.cmp_kind = cmp;
    irBlockAddInstr(ctx, instr);
    return ir_result;
}

static IrValue *irGlobalValue(Ast *gvar_ast) {
    IrValue *v = irValueNew(IR_TYPE_PTR, IR_VAL_GLOBAL);
    /* @TODO, this should really be fixed; one label field on the struct */
    v->as.global.name = (gvar_ast->is_static && gvar_ast->glabel)
                        ? gvar_ast->glabel
                        : gvar_ast->gname;
    v->as.global.value = NULL;
    return v;
}

static int irIsIntLikeCast(AstType *type) {
    /* Intrinsic classes (`I64 class CDate`) are int-shaped at the value
     * level, casting in/out is a pass-through. */
    return type->kind == AST_TYPE_INT ||
           type->kind == AST_TYPE_CHAR ||
           type->kind == AST_TYPE_POINTER ||
           astIsIntrinsicClass(type);
}

/* Lower a value of `from_type` (already in `src`) to `to_type`. Returns the
 * IrValue holding the converted result, or `src` unchanged when no
 * conversion is needed. Handles int<->float and pointer<->int. */
static IrValue *irLowerCast(IrCtx *ctx,
                            IrValue *src,
                            AstType *from_type,
                            AstType *to_type) {
    if (!from_type || !to_type) return src;
    /* Intrinsic classes (`I64 class CDate`) are int-shaped at the value
     * level, casting in/out is a pass-through. */
    int from_int = irIsIntLikeCast(from_type);
    int to_int = irIsIntLikeCast(to_type);
    int from_float =  astIsFloatType(from_type);
    int to_float = astIsFloatType(to_type);

    if (from_int && to_float) {
        /* HolyC variadic-slot cast `argv[i](F64)`: the source's type
         * carries `has_var_args=1` (the parser tags the argv element
         * type). The slot holds raw 8 bytes that were pushed as the
         * F64 bit pattern - reinterpret rather than convert, matching
         * `asmToFloat`'s `movq %rax, %xmm0` path. */
        if (from_type->has_var_args) {
            IrValue *dst = irTmp(IR_TYPE_F64, 8);
            irBlockAddInstr(ctx, irInstrNew(IR_BITCAST, dst, src, NULL));
            return dst;
        }
        IrValueType ft = irConvertType(to_type);
        IrValue *dst = irTmp(ft, to_type->size ? to_type->size : 8);
        if (from_type->issigned) {
            irBlockAddInstr(ctx, irInstrNew(IR_SITOFP, dst, src, NULL));
            return dst;
        } else {
            irBlockAddInstr(ctx, irInstrNew(IR_UITOFP, dst, src, NULL));
            return dst;
        }
    }
    /* float -> float of a different width: F32<->F64 conversion.
     * A constant literal is simply re-typed (codegen materialises the
     * right bit pattern); otherwise emit fpext / fptrunc. */
    if (from_float && to_float) {
        int from_sz = from_type->size ? from_type->size : 8;
        int to_sz = to_type->size ? to_type->size : 8;
        if (from_sz == to_sz) return src;
        IrValueType ft = irConvertType(to_type);
        if (src->kind == IR_VAL_CONST_FLOAT) {
            return irConstFloat(ft, src->as._f64);
        }
        IrValue *dst = irTmp(ft, to_sz);
        IrOp op = (to_sz < from_sz) ? IR_FPTRUNC : IR_FPEXT;
        irBlockAddInstr(ctx, irInstrNew(op, dst, src, NULL));
        return dst;
    }
    if (from_float && to_int) {
        IrValueType t = irConvertType(to_type);
        IrValue *dst = irTmp(t, to_type->size);
        /* Signedness comes from the DESTINATION integer type, not the
         * float source (a float is never `issigned`). fptoui saturates
         * negatives to 0, so using the float's flag here turned every
         * `(I64)negativeFloat` into 0. */
        if (to_type->issigned) {
            irBlockAddInstr(ctx, irInstrNew(IR_FPTOSI, dst, src, NULL));
        } else {
            irBlockAddInstr(ctx, irInstrNew(IR_FPTOUI, dst, src, NULL));
        }
        return dst;
    }
    /* int<->int and ptr<->int: in our 64-bit-everywhere codegen the bits
     * already match for same-or-larger destinations. For a narrowing cast
     * we still need to mask off the high bits so comparisons against
     * smaller-typed constants (e.g. `cast<U8>(0xAAFF) == 0xFF`) match. */
    if (from_int && to_int) {
        int from_size = from_type->size ? from_type->size : 8;
        int to_size = to_type->size ? to_type->size : 8;
        if (to_size < from_size && to_size < 8) {
            IrValueType t = irConvertType(to_type);
            IrValue *dst = irTmp(t, to_size);
            irBlockAddInstr(ctx, irInstrNew(IR_TRUNC, dst, src, NULL));
            return dst;
        }
    }
    return src;
}

static IrOp irCompoundAssignToIrOp(AstBinOp op, int issigned) {
    switch (op) {
        case AST_BIN_OP_ADD_ASSIGN: return IR_IADD;
        case AST_BIN_OP_SUB_ASSIGN: return IR_ISUB;
        case AST_BIN_OP_MUL_ASSIGN: return IR_IMUL;
        case AST_BIN_OP_DIV_ASSIGN: return issigned ? IR_IDIV : IR_UDIV;
        case AST_BIN_OP_MOD_ASSIGN: return issigned ? IR_IREM : IR_UREM;
        case AST_BIN_OP_SHL_ASSIGN: return IR_SHL;
        case AST_BIN_OP_SHR_ASSIGN: return IR_SHR;
        case AST_BIN_OP_AND_ASSIGN: return IR_AND;
        case AST_BIN_OP_XOR_ASSIGN: return IR_XOR;
        case AST_BIN_OP_OR_ASSIGN:  return IR_OR;
        default:
            loggerPanic("Not a compound assign: %s\n", astBinOpKindToString(op));
    }
}

static IrAssignTarget irLowerAssignTarget(IrCtx *ctx, Ast *lhs) {
    IrAssignTarget out = { NULL, NULL, 0 };

    lhs = irUnwrapDefaultParam(lhs);

    if (lhs->kind == AST_LVAR) {
        IrValue *local = irFnGetVar(ctx->cur_func, lhs->lvar_id);
        if (!local) loggerPanic("irLowerAssign: unknown local %s\n",
                                astToString(lhs));
        out.target = local;
        out.type = lhs->type;
        return out;
    }

    if (lhs->kind == AST_FUNPTR) {
        IrValue *local = irFnGetVar(ctx->cur_func, lhs->fn_ptr_id);
        if (!local) loggerPanic("irLowerAssign: unknown funptr %s\n",
                                astToString(lhs));
        out.target = local;
        out.type = lhs->type;
        return out;
    }

    if (lhs->kind == AST_CLASS_REF) {
        Ast *cls = lhs->cls;
        int offset = lhs->type->offset;
        out.type = lhs->type;

        /* Nested ref `a.b.c` / `p->b.c`: collapse to a single root and
         * combined offset. Mirrors the read-path lowering. */
        irCollapseClassRefChain(&cls, &offset);

        if (astIsUnOpKind(cls, AST_UN_OP_DEREF)) {
            /* Pointer-to-class field write: evaluate the pointer, add the
             * field's offset, store via STORE_DEREF. */
            IrValue *ptr_val = irExpr(ctx, cls->operand);
            IrValue *addr = ptr_val;
            if (offset != 0) {
                addr = irTmp(IR_TYPE_PTR, 8);
                IrValue *off_const = irConstInt(IR_TYPE_I64, offset);
                irBlockAddInstr(ctx, irInstrNew(IR_IADD, addr, ptr_val, off_const));
            }
            out.target = addr;
            out.indirect = 1;
            return out;
        }

        if (cls && cls->kind == AST_GVAR) {
            /* Global class field write: lea the global's address, add the
             * field offset, store via STORE_DEREF. */
            IrValue *gv = irGlobalValue(cls);
            IrValue *addr = irTmp(IR_TYPE_PTR, 8);
            irBlockAddInstr(ctx, irInstrNew(IR_LEA, addr, gv, NULL));
            if (offset != 0) {
                IrValue *off_const = irConstInt(IR_TYPE_I64, offset);
                IrValue *field_addr = irTmp(IR_TYPE_PTR, 8);
                irBlockAddInstr(ctx,
                        irInstrNew(IR_IADD, field_addr, addr, off_const));
                addr = field_addr;
            }
            out.target = addr;
            out.indirect = 1;
            return out;
        }

        if (!cls || cls->kind != AST_LVAR) {
            loggerPanic("Slice AST_CLASS_REF assign: unsupported cls kind %s\n",
                        astKindToString(cls->kind));
        }
        IrValue *base = irFnGetVar(ctx->cur_func, cls->lvar_id);
        if (!base) loggerPanic("AST_CLASS_REF on unknown lvar\n");

        /* Stack-allocated class: alias base+offset directly. */
        IrValue *field_addr = irTmp(IR_TYPE_PTR, 8);
        IrValue *off = irConstInt(IR_TYPE_I64, offset);
        irBlockAddInstr(ctx, irInstrNew(IR_GEP, field_addr, base, off));
        out.target = field_addr;
        return out;
    }

    if (astIsUnOpKind(lhs, AST_UN_OP_DEREF)) { 
        /* `*p = ...`: evaluate the pointer; STORE_DEREF takes care of the
         * indirection. */
        IrValue *ptr = irExpr(ctx, lhs->operand);
        out.target = ptr;
        out.type = lhs->type;
        out.indirect = 1;
        return out;
    }

    if (lhs->kind == AST_GVAR) {
        /* `g = ...`: lea g's address, store* through it. */
        IrValue *gv = irGlobalValue(lhs);
        IrValue *addr = irTmp(IR_TYPE_PTR, 8);
        irBlockAddInstr(ctx, irInstrNew(IR_LEA, addr, gv, NULL));
        out.target = addr;
        out.type = lhs->type;
        out.indirect = 1;
        return out;
    }

    loggerPanic("irLowerAssign: unsupported LHS %s\n", astKindToString(lhs->kind));
}

/* Materialise the ADDRESS of an lvalue (lvar / global / class field / `*p`).
 * Reuses the assign-target machinery: an indirect target is already a
 * pointer, a direct one is a slot/alias we LEA. Used for by-value struct
 * copies, where both sides are addressed and the bytes memcpy'd. */
static IrValue *irLValueAddr(IrCtx *ctx, Ast *ast) {
    IrAssignTarget t = irLowerAssignTarget(ctx, ast);
    if (t.indirect) {
        return t.target;
    }
    IrValue *addr = irTmp(IR_TYPE_PTR, 8);
    irBlockAddInstr(ctx, irInstrNew(IR_LEA, addr, t.target, NULL));
    return addr;
}

/* True for a by-value aggregate (non-intrinsic class/union) - the things
 * that are copied wholesale rather than held in a register. */
static int irIsByValAggregate(AstType *ty) {
    return ty && !ty->is_intrinsic &&
           (ty->kind == AST_TYPE_CLASS || ty->kind == AST_TYPE_UNION) &&
           ty->size > 0;
}

/* Lower `<lhs> op= rhs` (or `<lhs> = rhs` when op is AST_BIN_OP_ASSIGN)
 * and return the value written back. */
static IrValue *irLowerAssign(IrCtx *ctx, Ast *ast) {
    IrAssignTarget tgt = irLowerAssignTarget(ctx, ast->left);

    /* Struct/class copy: `dst = src`. Get the dst address, then either route
     * a struct-returning call's hidden out-pointer at it, or memcpy the
     * source aggregate's bytes into it. */
    if (astIsBinOpKind(ast, AST_BIN_OP_ASSIGN) && irIsByValAggregate(tgt.type)) {
        IrValue *dst_addr;
        if (tgt.indirect) {
            dst_addr = tgt.target;
        } else {
            dst_addr = irTmp(IR_TYPE_PTR, 8);
            irBlockAddInstr(ctx, irInstrNew(IR_LEA, dst_addr, tgt.target, NULL));
        }
        Ast *rhs = ast->right;
        if ((rhs->kind == AST_FUNCALL || rhs->kind == AST_FUNPTR_CALL ||
             rhs->kind == AST_ASM_FUNCALL) &&
            rhs->type && irRetTypeIsAggregate(rhs->type))
        {
            irFnCallTo(ctx, rhs, dst_addr);
        } else {
            /* Chained `c = b = src`: lower the inner assignment first (it
             * performs its own copy and returns the inner dst's address),
             * then copy from there. Otherwise the source is a plain lvalue. */
            IrValue *src_addr = astIsBinOpKind(rhs, AST_BIN_OP_ASSIGN)
                ? irLowerAssign(ctx, rhs)
                : irLValueAddr(ctx, rhs);
            irEmitMemcpy(ctx, dst_addr, src_addr, tgt.type->size);
        }
        return dst_addr;
    }

    IrValue *new_val;
    if (astIsBinOpKind(ast, AST_BIN_OP_ASSIGN)) {
        new_val = irExpr(ctx, ast->right);
    } else {
        /* Compound: load through target, op, store back. */
        IrValueType ir_type = irConvertType(tgt.type);
        IrValue *cur = irTmp(ir_type, tgt.type->size);
        IrOp load_op = tgt.indirect ? IR_LOAD_DEREF : IR_LOAD;
        irBlockAddInstr(ctx, irInstrNew(load_op, cur, tgt.target, NULL));
        IrValue *rhs = irExpr(ctx, ast->right);
        IrOp ir_op = irCompoundAssignToIrOp(ast->binop, tgt.type->issigned);
        new_val = irTmp(ir_type, tgt.type->size);
        irBlockAddInstr(ctx, irInstrNew(ir_op, new_val, cur, rhs));
    }

    /* float<->int assignment needs a real conversion (sitofp/fptosi). */
    if (tgt.type && ast->right->type &&
        ((astIsFloatType(ast->right->type) && astIsIntType(tgt.type)) ||
         (astIsIntType(ast->right->type) && astIsFloatType(tgt.type))))
    {
        new_val = irLowerCast(ctx, new_val, ast->right->type, tgt.type);
    }
    new_val = irNarrowToTargetWidth(ctx, new_val, tgt.type);
    new_val = irWidenToTargetWidth(ctx, new_val, ast->right->type, tgt.type);
    new_val = irConvertFloatToTargetWidth(ctx, new_val, tgt.type);

    /* Direct-write: only safe when the target is a real lvar slot
     * (IR_VAL_LOCAL or IR_VAL_PARAM). For GEP-aliased field stores
     * (stack-class fields) the target is an IR_VAL_TMP whose loff
     * came from `IR_GEP base, offset`. A virtual alias the codegen
     * understands but DCE doesn't. If we redirected the producer to
     * write that tmp's dst, DCE would see "tmp with no later reader"
     * and drop the producer entirely, and the field never gets
     * written. Force the IR_STORE path in that case so the side
     * effect on memory is explicit. */
    if (!tgt.indirect && !tgt.target->pinned_reg &&
        (tgt.target->kind == IR_VAL_LOCAL ||
         tgt.target->kind == IR_VAL_PARAM) &&
        irRedirectLastDst(ctx, new_val, tgt.target))
    {
        return tgt.target;
    }

    IrOp store_op = tgt.indirect ? IR_STORE_DEREF : IR_STORE;
    irBlockAddInstr(ctx, irInstrNew(store_op, tgt.target, new_val, NULL));
    return new_val;
}

/* Recursively lower an AST_ARRAY_INIT against `base + offset_bytes`,
 * mirroring `asmArrayInit`'s walk: when iterating items of an array
 * type the stride is the element type's size, when iterating items of
 * a class/union the stride comes from the item's own type (the parser
 * lays each scalar at the next 8-byte slot regardless of the field's
 * actual width, and struct alignment makes it work). Returns the next
 * free byte offset. */
static int irLowerArrayInitWalk(IrCtx *ctx,
                                IrValue *base,
                                int offset_bytes,
                                AstType *target_ty,
                                Ast *init)
{
    if (!init || init->kind != AST_ARRAY_INIT) return offset_bytes;
    if (!init->arrayinit) return offset_bytes;
    AstType *elem_ty = target_ty ? target_ty->ptr : NULL;
    int parent_is_array = astTypeIsArray(target_ty);

    /* Class/union target: pair each positional item with its field, so the
     * value is coerced to the field's type (e.g. an F64 literal narrowed
     * to an F32 field) and stored at the field's real offset - not laid
     * out at the literal's own (possibly wider) width. */
    int parent_is_class = target_ty && !astIsIntrinsicClass(target_ty) &&
        (target_ty->kind == AST_TYPE_CLASS || target_ty->kind == AST_TYPE_UNION);
    if (parent_is_class) {
        int idx = 0;
        listForEach(init->arrayinit) {
            Ast *item = (Ast *)it->value;
            AstType *fld = astClassFieldAt(target_ty, idx);
            int foff = offset_bytes + (fld ? fld->offset : idx * 8);
            if (item->kind == AST_ARRAY_INIT) {
                irLowerArrayInitWalk(ctx, base, foff, fld, item);
            } else {
                IrValue *val = irExpr(ctx, item);
                AstType *cty = fld ? fld : item->type;
                val = irConvertFloatToTargetWidth(ctx, val, cty);
                val = irNarrowToTargetWidth(ctx, val, cty);
                IrValue *field = irTmp(IR_TYPE_PTR, 8);
                IrValue *off = irConstInt(IR_TYPE_I64, foff);
                irBlockAddInstr(ctx, irInstrNew(IR_GEP, field, base, off));
                irBlockAddInstr(ctx, irInstrNew(IR_STORE, field, val, NULL));
            }
            idx++;
        }
        return offset_bytes + (target_ty->size > 0 ? target_ty->size : 0);
    }

    listForEach(init->arrayinit) {
        Ast *item = (Ast *)it->value;
        if (item->kind == AST_ARRAY_INIT) {
            /* Recurse to calculate the full size */
            irLowerArrayInitWalk(ctx, base, offset_bytes, elem_ty, item);
            if (parent_is_array && elem_ty) {
                offset_bytes += elem_ty->size;
            }
            continue;
        }

        /* String literal targeting an inline array slot; for example
         * `U8 s[][N] = {"hello", ...}`: copy the bytes into the
         * slot rather than the literal's address. Slots typed as
         * `U8*` still take the pointer via the regular path.
         *
         * Pack the string bytes (zero-padded past the null
         * terminator) into the widest power-of-two immediates that
         * fit and emit them as inline GEP+STOREs. Clang produces
         * the same shape via a `__const` blob; for small fixed
         * slots, materialising the bytes as movabsq immediates is
         * just as compact and avoids a runtime memcpy. */
        if (item->kind == AST_STRING &&
            parent_is_array &&
            astTypeIsArray(elem_ty))
        {
            int slot_size = elem_ty->size;
            int str_len = item->sval ? (int)item->sval->len : 0;
            const char *str_data = item->sval ? item->sval->data : NULL;
            int pos = 0;
            while (pos < slot_size) {
                int remaining = slot_size - pos;
                int chunk;
                IrValueType chunk_ty;
                if      (remaining >= 8) { chunk = 8; chunk_ty = IR_TYPE_I64; }
                else if (remaining >= 4) { chunk = 4; chunk_ty = IR_TYPE_I32; }
                else if (remaining >= 2) { chunk = 2; chunk_ty = IR_TYPE_I16; }
                else                     { chunk = 1; chunk_ty = IR_TYPE_I8;  }
                u64 packed = 0;
                for (int i = 0; i < chunk; i++) {
                    int idx = pos + i;
                    u8 b = (idx < str_len) ? (u8)str_data[idx] : 0;
                    packed |= (u64)b << (i * 8);
                }
                IrValue *field = irTmp(IR_TYPE_PTR, 8);
                IrValue *off = irConstInt(IR_TYPE_I64, offset_bytes + pos);
                irBlockAddInstr(ctx, irInstrNew(IR_GEP, field, base, off));
                IrValue *val = irConstInt(chunk_ty, (s64)packed);
                irBlockAddInstr(ctx, irInstrNew(IR_STORE, field, val, NULL));
                pos += chunk;
            }
            offset_bytes += slot_size;
            continue;
        }
        IrValue *val = irExpr(ctx, item);
        AstType *narrow_ty = (parent_is_array && elem_ty)
                             ? elem_ty : item->type;
        val = irNarrowToTargetWidth(ctx, val, narrow_ty);
        IrValue *field = irTmp(IR_TYPE_PTR, 8);
        IrValue *off = irConstInt(IR_TYPE_I64, offset_bytes);
        irBlockAddInstr(ctx, irInstrNew(IR_GEP, field, base, off));
        irBlockAddInstr(ctx, irInstrNew(IR_STORE, field, val, NULL));
        if (parent_is_array && elem_ty) {
            offset_bytes += elem_ty->size;
        } else {
            int size = ((item->kind == AST_STRING) || !item->type) ? 8 : item->type->size;
            offset_bytes += size;
        }
    }
    return offset_bytes;
}

/* C integer promotion for a value just read from memory: a narrow integer
 * (size < 8) read in an expression widens to 64-bit, sign- or zero-extended
 * per the declared type's signedness. The IR is signedness-lossy (LLVM-style:
 * signedness lives in the ops, not the value type), so the builder must emit
 * the extension here. Without it, consumers that read the value full-width
 * (comparisons, variadic args, wider arithmetic) see a zero-extended narrow
 * load and a signed value loses its sign. Applies to EVERY read that loads a
 * scalar through an address - locals, globals, derefs and class fields -
 * `ty` is the value's source AST type. Returns the (possibly widened) value. */
static IrValue *irPromoteNarrowInt(IrCtx *ctx, IrValue *v, AstType *ty) {
    if (ty &&
        (ty->kind == AST_TYPE_INT || ty->kind == AST_TYPE_CHAR) &&
        ty->size > 0 && ty->size < 8)
    {
        IrValue *wide = irTmp(IR_TYPE_I64, 8);
        IrOp ext = ty->issigned ? IR_SEXT : IR_ZEXT;
        irBlockAddInstr(ctx, irInstrNew(ext, wide, v, NULL));
        return wide;
    }
    return v;
}

IrValue *irLowerLVar(IrCtx *ctx, Ast *ast) {
    IrValue *local_var = irFnGetVar(ctx->cur_func, ast->lvar_id);
    if (!local_var) {
        loggerPanic("Variable %s not found\n", astToString(ast));
    }

    AstType *ast_ty = ast->type;

    /* Array LVARs decay to a pointer to their first element. We
     * emit an LEA on the slot rather than loading its bytes. */
    if (astTypeIsArray(ast_ty)) {
        IrValue *addr = irTmp(IR_TYPE_PTR, 8);
        irBlockAddInstr(ctx, irInstrNew(IR_LEA, addr, local_var, NULL));
        return addr;
    }

    int is_intrinsic_class = astIsIntrinsicClass(ast_ty);

    int is_aggregate = !is_intrinsic_class &&
        (ast_ty->kind == AST_TYPE_CLASS || ast_ty->kind == AST_TYPE_UNION);

    /* For non-intrinsic types we want to produce a LEA. We do not have 
     * a "load a struct into register" type of lowering */
    if (is_aggregate) {
        IrValue *addr = irTmp(IR_TYPE_PTR, 8);
        irBlockAddInstr(ctx, irInstrNew(IR_LEA, addr, local_var, NULL));
        return addr;
    }

    /* Operand-direct read: when the lvar's address is never taken, the
     * value held by its slot can't be mutated by aliasing, so the
     * consumer can read the slot operand directly and we skip emitting
     * the IR_LOAD tmp entirely. Intrinsic classes still go through the
     * load: they carry a class type at the AST level which we coerce to
     * I64 here, and downstream type-based logic relies on that
     * coercion. */
    IrValue *result;
    if (!is_intrinsic_class &&
        ctx->escape_set &&
        !setHas(ctx->escape_set, (void *)(u64)ast->lvar_id))
    {
        result = local_var;
    } else {
        IrValueType ir_value_type = is_intrinsic_class
            ? IR_TYPE_I64 : irConvertType(ast->type);

        int load_size = is_intrinsic_class ? 8 : ast_ty->size;
        IrValue *ir_load_dest = irTmp(ir_value_type, load_size);
        IrInstr *load_instr = irLoad(ir_load_dest, local_var);
        irBlockAddInstr(ctx, load_instr);
        result = ir_load_dest;
    }

    /* C integer promotion for narrow reads (see irPromoteNarrowInt).
     * Intrinsic classes are coerced to I64 above and must not be re-extended. */
    if (!is_intrinsic_class) {
        result = irPromoteNarrowInt(ctx, result, ast_ty);
    }
    return result;
}

IrValue *irLowerGlobal(IrCtx *ctx, Ast *ast) {
    /* Global int read: lea its address, then load*. Arrays
     * decay to a pointer to the first element - just return
     * the LEA'd address with no load. */
    IrValue *gv = irGlobalValue(ast);
    IrValue *addr = irTmp(IR_TYPE_PTR, 8);
    irBlockAddInstr(ctx, irInstrNew(IR_LEA, addr, gv, NULL));
    /* Arrays and by-value aggregates decay to a pointer to their storage -
     * return the LEA'd address with no load. (Mirrors irLowerLVar; a struct
     * read by value, e.g. as a call argument, needs its address, not a
     * scalar load of its first 8 bytes.) */
    if (astTypeIsArray(ast->type) || irIsByValAggregate(ast->type)) {
        return addr;
    }
    IrValueType ir_type = irConvertType(ast->type);
    IrValue *load_dst = irTmp(ir_type, ast->type->size);
    irBlockAddInstr(ctx, irInstrNew(IR_LOAD_DEREF, load_dst, addr, NULL));
    return irPromoteNarrowInt(ctx, load_dst, ast->type);
}

IrValue *irLowerFunPtr(IrCtx *ctx, Ast *ast) {
    /* Reading a function-pointer slot: load the 8-byte pointer. */
    IrValue *slot = irFnGetVar(ctx->cur_func, ast->fn_ptr_id);
    if (!slot) loggerPanic("AST_FUNPTR with no slot\n");
    IrValue *dst = irTmp(IR_TYPE_PTR, 8);
    irBlockAddInstr(ctx, irLoad(dst, slot));
    return dst;
}

IrValue *irLowerClassRef(IrCtx *ctx, Ast *ast) {
    /* Two shapes:
     *   stack class: cls is AST_LVAR with class type. Field is at a known offset
     *                within the local's stack frame; IR_GEP aliases that loff.
     *
     *   pointer:     cls is AST_UNOP DEREF (the parser lowers `p->field` to
     *                `(*p).field`); operand is a pointer-typed expression.
     *                Evaluate it, add the field offset, then deref. */
    Ast *cls = ast->cls;
    /* An array or by-value aggregate field decays to its address rather than
     * being loaded as a scalar (matches irLowerLVar / irLowerGlobal) - needed
     * e.g. when `p->structfield` / `cls.structfield` is passed by value or
     * copied; otherwise the first 8 bytes would be mistaken for an address. */
    int field_decays = astTypeIsArray(ast->type) ||
                       irIsByValAggregate(ast->type);
    IrValueType field_ir_type = field_decays
        ? IR_TYPE_PTR : irConvertType(ast->type);
    IrValue *load_dst = irTmp(field_ir_type,
            field_decays ? 8 : ast->type->size);
    int offset = ast->type->offset;

    irCollapseClassRefChain(&cls, &offset);

    if (astIsUnOpKind(cls, AST_UN_OP_DEREF)) {
        IrValue *ptr_val = irExpr(ctx, cls->operand);
        IrValue *addr = ptr_val;
        /* A decaying field returns an address the caller may tag (e.g. a
         * by-value struct arg). At offset 0 `addr` would alias the shared
         * operand pointer (`e` in `e->color`), so force a fresh tmp via an
         * explicit add - tagging that must not leak onto `e`. (The add #0 is
         * kept out of the algebraic-identity fold for tagged values; see
         * irEvalCanFold.) */
        if (offset != 0 || field_decays) {
            addr = irTmp(IR_TYPE_PTR, 8);
            IrValue *off_const = irConstInt(IR_TYPE_I64, offset);
            irBlockAddInstr(ctx,
                    irInstrNew(IR_IADD, addr, ptr_val, off_const));
        }
        if (field_decays) {
            /* `p->arr` decays to the address of the first
             * element - that's just the field pointer itself. */
            return addr;
        }
        irBlockAddInstr(ctx, irInstrNew(IR_LOAD_DEREF, load_dst, addr, NULL));
        return irPromoteNarrowInt(ctx, load_dst, ast->type);
    }

    if (cls && cls->kind == AST_GVAR) {
        /* Global class: lea the global's address, add the field offset,
         * then load (or return the field address for an array field). */
        IrValue *gv = irGlobalValue(cls);
        IrValue *addr = irTmp(IR_TYPE_PTR, 8);
        irBlockAddInstr(ctx, irInstrNew(IR_LEA, addr, gv, NULL));
        if (offset != 0) {
            IrValue *off_const = irConstInt(IR_TYPE_I64, offset);
            IrValue *field_addr = irTmp(IR_TYPE_PTR, 8);
            irBlockAddInstr(ctx,
                    irInstrNew(IR_IADD, field_addr, addr, off_const));
            addr = field_addr;
        }
        if (field_decays) {
            return addr;
        }
        irBlockAddInstr(ctx, irInstrNew(IR_LOAD_DEREF, load_dst, addr, NULL));
        return irPromoteNarrowInt(ctx, load_dst, ast->type);
    }

    if (cls && (cls->kind == AST_FUNCALL || cls->kind == AST_FUNPTR_CALL ||
                cls->kind == AST_ASM_FUNCALL))
    {
        /* Field of a struct-returning call result, e.g. `MakeVec2(..).y`.
         * Evaluating an aggregate call yields the address of its result
         * buffer; index into it like the pointer-deref case. */
        IrValue *base = irExpr(ctx, cls);
        IrValue *addr = base;
        if (offset != 0) {
            addr = irTmp(IR_TYPE_PTR, 8);
            IrValue *off_const = irConstInt(IR_TYPE_I64, offset);
            irBlockAddInstr(ctx, irInstrNew(IR_IADD, addr, base, off_const));
        }
        if (field_decays) {
            return addr;
        }
        irBlockAddInstr(ctx, irInstrNew(IR_LOAD_DEREF, load_dst, addr, NULL));
        return irPromoteNarrowInt(ctx, load_dst, ast->type);
    }

    if (!cls || cls->kind != AST_LVAR) {
        loggerPanic("Slice AST_CLASS_REF: unsupported cls kind %s\n",
                cls ? astKindToString(cls->kind) : "<null>");
    }
    IrValue *base = irFnGetVar(ctx->cur_func, cls->lvar_id);
    if (!base) loggerPanic("AST_CLASS_REF on unknown lvar\n");

    /* Stack-allocated class. */
    IrValue *field_addr = irTmp(IR_TYPE_PTR, 8);
    IrValue *off = irConstInt(IR_TYPE_I64, offset);
    irBlockAddInstr(ctx, irInstrNew(IR_GEP, field_addr, base, off));
    if (field_decays) {
        /* `cls.arr` decays to the field's address. The GEP tmp aliases the
         * stack slot rather than holding a pointer, so emit an explicit
         * IR_LEA to materialize the pointer value. */
        IrValue *dst = irTmp(IR_TYPE_PTR, 8);
        irBlockAddInstr(ctx, irInstrNew(IR_LEA, dst, field_addr, NULL));
        return dst;
    }
    irBlockAddInstr(ctx, irLoad(load_dst, field_addr));
    return irPromoteNarrowInt(ctx, load_dst, ast->type);
}

IrValue *irLowerUnOp(IrCtx *ctx, Ast *ast) {
    if (astIsUnOpKind(ast, AST_UN_OP_DEREF)) {
        /* `*p`: evaluate the pointer expression, then load through it.
         *
         * Parser quirk: `*pp` where the result is pointer-to-scalar
         * (int / pointer / F64) is a no-op in the AST codegen
         * (`leaq (%rax), %rax`). Match it by returning the operand pointer
         * directly. This is what makes `&arr[i]` (parsed as `*((&arr) + i)`)
         * yield a real pointer rather than dereffing through the slot. */
        IrValue *ptr = irExpr(ctx, ast->operand);
        if (astTypeIsPtr(ast->type) && ast->type->ptr) {
            AstTypeKind ptr_kind = ast->type->ptr->kind;
            if (ptr_kind == AST_TYPE_INT ||
                ptr_kind == AST_TYPE_POINTER ||
                ptr_kind == AST_TYPE_FLOAT)
            {
                return ptr;
            }
        }
        /* `*p` where the result type is itself an array
         * (multi-dim indexing intermediates, e.g. the inner
         * `*matrix` in `matrix[i][j]`): array decays to its
         * pointer, no real load. Same for class result -
         * the parent expression is `&*x` (`&arr[i]`) which
         * cancels out. */
        if (astTypeIsArray(ast->type) || ast->type->kind == AST_TYPE_CLASS) return ptr;
        IrValueType ir_type = irConvertType(ast->type);
        IrValue *load_dst = irTmp(ir_type, ast->type->size);
        irBlockAddInstr(ctx, irInstrNew(IR_LOAD_DEREF, load_dst, ptr, NULL));
        return irPromoteNarrowInt(ctx, load_dst, ast->type);
    }
    if (astIsUnOpKind(ast, AST_UN_OP_ADDR_OF)) {
        /* `&lvar` / `&gvar` / `&fn`: produce a pointer value.
         *
         * Parser quirk (matches asmAddr): when the operand is a
         * pointer-typed LVAR pointing at a SCALAR (int/F64/ptr -
         * NOT array/char/class), `&lvar` returns the pointer's
         * VALUE rather than the slot address. So `&arr[2]` for
         * `I64 *arr;` is `arr + 2` (arr's value plus 2*sizeof). */
        Ast *operand = ast->operand;
        /* `&*x` cancels to x. Used by the parser for `&arr[i]`
         * which is `&*(arr+i)`. */
        if (astIsUnOpKind(operand, AST_UN_OP_DEREF)) {
            return irExpr(ctx, operand->operand);
        }
        IrValue *src = NULL;
        if (operand->kind == AST_LVAR) {
            src = irFnGetVar(ctx->cur_func, operand->lvar_id);
            if (!src) loggerPanic("&lvar on unknown lvar\n");
            AstType *ot = operand->type;
            if (ot && astTypeIsPtr(ot) && ot->ptr) {
                AstTypeKind ptr_kind = ot->ptr->kind;
                if (ptr_kind != AST_TYPE_ARRAY &&
                    ptr_kind != AST_TYPE_CHAR &&
                    ptr_kind != AST_TYPE_CLASS)
                {
                    /* Load the pointer's value. */
                    IrValue *dst = irTmp(IR_TYPE_PTR, 8);
                    irBlockAddInstr(ctx, irLoad(dst, src));
                    return dst;
                }
            }
        } else if (operand->kind == AST_GVAR) {
            src = irGlobalValue(operand);
        } else if (operand->kind == AST_FUNC ||
                   operand->kind == AST_FUN_PROTO ||
                   operand->kind == AST_EXTERN_FUNC)
        {
            /* Address of a function: emit `leaq <fname>(%rip), rax`
             * via the global-value mechanism. */
            src = irValueNew(IR_TYPE_PTR, IR_VAL_GLOBAL);
            src->as.global.name = operand->fname;
            src->as.global.value = NULL;
            src->flags |= IR_VAL_FLAG_FUNC;
        } else if (operand->kind == AST_ASM_FUNCDEF ||
                operand->kind == AST_ASM_FUNC_BIND) {
            /* Asm-bound function - use the raw asm name */
            src = irValueNew(IR_TYPE_PTR, IR_VAL_GLOBAL);
            src->as.global.name = operand->asmfname;
            src->as.global.value = NULL;
        } else if (operand->kind == AST_CLASS_REF) {
            /* `&v->field` / `&cls.field` / `&v->a.b`: same address calculation
             * as a write target. The result is the field's pointer, no LEA
             * needed (target.target is already a pointer when indirect, or a 
             * slot-aliased tmp via IR_GEP when direct). */
            IrAssignTarget tgt = irLowerAssignTarget(ctx, operand);
            if (tgt.indirect) {
                return tgt.target;
            }
            /* Direct (stack class field): tgt.target is a tmp aliased to
             * `base+offset` via `IR_GEP`, used as a slot elsewhere - but here
             * we want its value (the pointer to that slot) so emit an IR_LEA
             * over it. */
            IrValue *dst = irTmp(IR_TYPE_PTR, 8);
            irBlockAddInstr(ctx, irInstrNew(IR_LEA, dst, tgt.target, NULL));
            return dst;
        } else {
            loggerPanic("&%s not supported in slice\n",
                    astKindToString(operand->kind));
        }
        IrValue *dst = irTmp(IR_TYPE_PTR, 8);
        irBlockAddInstr(ctx, irInstrNew(IR_LEA, dst, src, NULL));
        return dst;
    }
    if (astIsUnOpKind(ast, AST_UN_OP_PLUS)) {
        /* Identity for arithmetic types - just evaluate operand. */
        return irExpr(ctx, ast->operand);
    }
    if (astIsUnOpKind(ast, AST_UN_OP_MINUS)) {
        IrValue *v = irExpr(ctx, ast->operand);
        IrValueType vt = v->type;
        IrValue *dst = irTmp(vt, irIsFloat(vt) ? (int)irValueByteSize(v)
                                               : ast->operand->type->size);
        IrOp negop = irIsFloat(vt) ? IR_FNEG : IR_INEG;
        irBlockAddInstr(ctx, irInstrNew(negop, dst, v, NULL));
        return dst;
    }
    if (astIsUnOpKind(ast, AST_UN_OP_BIT_NOT)) {
        /* Bitwise NOT on int. Lowers to `dst = ~v` via IR_NOT. */
        IrValue *v = irExpr(ctx, ast->operand);
        IrValue *dst = irTmp(v->type, 8);
        irBlockAddInstr(ctx, irInstrNew(IR_NOT, dst, v, NULL));
        return dst;
    }
    if (astIsUnOpKind(ast, AST_UN_OP_LOG_NOT)) {
        /* Logical NOT: result is 1 if operand is zero, else 0.
         * Lower as ICMP_EQ with 0. */
        IrValue *v = irExpr(ctx, ast->operand);
        IrValue *dst = irTmp(IR_TYPE_I64, 8);
        IrValue *zero = irConstInt(IR_TYPE_I64, 0);
        irBlockAddInstr(ctx, irICmp(dst, IR_CMP_EQ, v, zero));
        return dst;
    }
    /* Slice supports ++/-- on int / pointer locals and on int /
     * pointer class fields. The store back is the side effect; we
     * return either the original value (POST) or the incremented
     * one (PRE). */
    if (ast->unop != AST_UN_OP_PRE_INC &&
        ast->unop != AST_UN_OP_PRE_DEC &&
        ast->unop != AST_UN_OP_POST_INC &&
        ast->unop != AST_UN_OP_POST_DEC)
    {
        loggerPanic("Slice unop only supports ++/-- and *, got %s\n",
                astUnOpKindToString(ast->unop));
    }
    Ast *operand = irUnwrapDefaultParam(ast->operand);
    AstType *val_type = operand->type;
    IrValueType ir_type = irConvertType(val_type);
    int size = val_type->size;

    /* Resolve the slot/address-and-load shape: for an lvar we
     * load/store through the slot; for a class field we reuse
     * irLowerAssignTarget so the addressing logic (LEA+offset for
     * stack class, ptr+offset for ptr-to-class) is shared with
     * compound assign. */
    IrValue *target = NULL;
    int indirect = 0;
    if (operand->kind == AST_LVAR) {
        target = irFnGetVar(ctx->cur_func, operand->lvar_id);
        if (!target) loggerPanic("inc/dec on unknown lvar\n");
    } else if (operand->kind == AST_GVAR ||
               operand->kind == AST_CLASS_REF ||
               (astIsUnOpKind(operand, AST_UN_OP_DEREF)))
    {
        /* Global / class field / `*p`: irLowerAssignTarget
         * computes the address (LEA'd global, direct slot for
         * stack class field, or runtime pointer for ptr-deref
         * and ptr-to-class field). */
        IrAssignTarget tgt = irLowerAssignTarget(ctx, operand);
        target = tgt.target;
        indirect = tgt.indirect;
    } else {
        loggerPanic("inc/dec unhandled operand kind %s\n",
                astKindToString(operand->kind));
    }

    IrValue *cur = irTmp(ir_type, size);
    IrOp load_op = indirect ? IR_LOAD_DEREF : IR_LOAD;
    irBlockAddInstr(ctx, irInstrNew(load_op, cur, target, NULL));

    int is_inc = astIsUnOpKind(ast, AST_UN_OP_PRE_INC) ||
                 astIsUnOpKind(ast, AST_UN_OP_POST_INC);
    /* Pointer ++/--: step by sizeof(*ptr). Float ++/--: step
     * by 1.0 via IR_FADD/IR_FSUB. Everything else uses IR_IADD
     * /IR_ISUB by 1. */
    IrValue *delta;
    IrOp op_kind;
    if (val_type->kind == AST_TYPE_FLOAT) {
        delta = irConstFloat(irConvertType(val_type), 1.0);
        op_kind = is_inc ? IR_FADD : IR_FSUB;
    } else {
        int step = 1;
        if (val_type->kind == AST_TYPE_POINTER && val_type->ptr) {
            step = val_type->ptr->size;
            if (step < 1) step = 1;
        }
        delta = irConstInt(ir_type, step);
        op_kind = is_inc ? IR_IADD : IR_ISUB;
    }
    IrValue *new_val = irTmp(ir_type, size);
    IrInstr *op = irInstrNew(op_kind, new_val, cur, delta);
    irBlockAddInstr(ctx, op);
    IrOp store_op = indirect ? IR_STORE_DEREF : IR_STORE;
    irBlockAddInstr(ctx, irInstrNew(store_op, target, new_val, NULL));

    int is_post = astIsUnOpKind(ast, AST_UN_OP_POST_INC) ||
                  astIsUnOpKind(ast, AST_UN_OP_POST_DEC);
    return is_post ? cur : new_val;
}

IrValue *irExpr(IrCtx *ctx, Ast *ast) {
    /* Variable references are SHARED Ast nodes stamped with their
     * declaration line - letting them touch the hint would attribute
     * the rest of the statement (and anything after) to the decl. */
    if (ast && ast->line && ast->kind != AST_LVAR && ast->kind != AST_GVAR)
        ir_line_hint = ast->line;
    switch (ast->kind) {
        case AST_BINOP: {
            if (astIsBinOpKind(ast, AST_BIN_OP_ASSIGN) ||
                    irBinOpIsCompoundAssign(ast->binop)) {
                return irLowerAssign(ctx, ast);
            }
            return irLowerBinOpExpr(ctx, ast);
        }
        case AST_LITERAL:
            switch (ast->type->kind) {
                case AST_TYPE_INT:
                case AST_TYPE_CHAR:
                    return irConstInt(irConvertType(ast->type), ast->i64);
                case AST_TYPE_FLOAT: {
                        IrValue *value = irConstFloat(irConvertType(ast->type), ast->f64);
                        // irAddConstFloat(ctx->ir_program, value);
                        return value;
                    }
                default:
                    loggerPanic("Unknown literal: %s\n",
                            astTypeKindToString(ast->type->kind));
            }
            break;
        
        case AST_DEFAULT_PARAM:
            /* In the function body we want the lvar value. The default init
             * only matters at the callsite. */
            return irExpr(ctx, ast->declvar);
        case AST_LVAR:
            return irLowerLVar(ctx, ast);
        case AST_GVAR:
            return irLowerGlobal(ctx, ast);
        case AST_ASM_FUNCALL:
        case AST_FUNPTR_CALL:
        case AST_FUNCALL:
            return irLowerFnCall(ctx, ast);
        case AST_FUNPTR:
            return irLowerFunPtr(ctx, ast);

        case AST_CAST: {
            /* Evaluate the operand, then convert if int/float boundary. */
            IrValue *src = irExpr(ctx, ast->operand);
            return irLowerCast(ctx, src, ast->operand->type, ast->type);
        }

        case AST_STRING: {
            IrValue *value = irValueNew(IR_TYPE_ARRAY, IR_VAL_CONST_STR);
            value->as.str.str = ast->sval;
            value->as.str.label = ast->slabel;
            value->as.str.str_real_len = ast->real_len;
            return value;
        }

        case AST_CLASS_REF:
            return irLowerClassRef(ctx, ast);
        case AST_UNOP:
            return irLowerUnOp(ctx, ast);

        default:
            loggerPanic("Expr Unhandled AST kind: %s\n", astKindToString(ast->kind));
    }
}

void irLowerReturn(IrCtx *ctx, Ast *ast) {
    if (ast->retval && ctx->cur_func->return_value) {
        /* Struct-by-value return: copy the retval's bytes into
         * the hidden out-pointer's destination. irExpr on a
         * class-typed AST_LVAR produces the lvar's address; the
         * out_ptr param's slot holds the destination address. */
        AstType *rt = ast->retval->type;
        if (rt && irRetTypeIsAggregate(rt)) {
            IrValue *src_addr = irExpr(ctx, ast->retval);
            IrValue *dst_addr = irTmp(IR_TYPE_PTR, 8);
            if (irRetIsIndirect(rt)) {
                /* return_value holds the caller's hidden out-pointer: load
                 * it, then copy the struct bytes through it. */
                irBlockAddInstr(ctx,
                        irLoad(dst_addr, ctx->cur_func->return_value));
            } else {
                /* return_value IS the struct-sized return slot: copy the
                 * bytes into it; the exit-block RET loads it into the
                 * result registers. */
                irBlockAddInstr(ctx, irInstrNew(IR_LEA, dst_addr,
                                ctx->cur_func->return_value, NULL));
            }
            irEmitMemcpy(ctx, dst_addr, src_addr, rt->size);
        } else {
            IrValue *val = irExpr(ctx, ast->retval);
            IrValue *rv = ctx->cur_func->return_value;
            /* Convert the return value to the function's return type so
             * the stored width matches the slot. The codegen sizes a
             * store from the value's own byte width, so a narrow return
             * value (e.g. a Bool returned from a helper, or a literal
             * like FALSE) written into a wider return slot would leave
             * the slot's upper bytes stale; the epilogue then reads the
             * slot at full width and returns garbage in those bytes.
             * Matches C's "the return value is converted to the
             * function's return type". */
            if (rv && irIsFloat(rv->type)) {
                /* Returning a float of a different width than the
                 * declared return type: convert (fptrunc/fpext) so the
                 * stored bits match the slot the epilogue reads. */
                if (irIsFloat(val->type) && val->type != rv->type) {
                    if (val->kind == IR_VAL_CONST_FLOAT) {
                        val = irConstFloat(rv->type, val->as._f64);
                    } else {
                        int to_sz = (int)irValueByteSize(rv);
                        int from_sz = (int)irValueByteSize(val);
                        IrValue *cv = irTmp(rv->type, to_sz);
                        IrOp op = (to_sz < from_sz) ? IR_FPTRUNC : IR_FPEXT;
                        irBlockAddInstr(ctx, irInstrNew(op, cv, val, NULL));
                        val = cv;
                    }
                }
            } else if (rv) {
                if (val->kind == IR_VAL_CONST_INT &&
                    irValueByteSize(val) < irValueByteSize(rv))
                {
                    val = irConstInt(rv->type, val->as._i64);
                } else if (irIsTmp(val) &&
                           val->as.var.size < (u64)irValueByteSize(rv))
                {
                    IrValue *wide = irTmp(rv->type, irValueByteSize(rv));
                    int sext = ast->retval->type &&
                               ast->retval->type->issigned;
                    irBlockAddInstr(ctx,
                            irInstrNew(sext ? IR_SEXT : IR_ZEXT,
                                       wide, val, NULL));
                    val = wide;
                }
            }
            IrInstr *st = irStore(ctx->cur_func->return_value, val);
            irBlockAddInstr(ctx, st);
        }
    }
    IrInstr *jmp = irJump(ctx->cur_func,
                          ctx->cur_block,
                          ctx->cur_func->exit_block);
    irBlockAddInstr(ctx, jmp);
}

/* try { try_body } catch <catch_body> compiles to:
 *
 *     frame = alloca(CatchFrame)                 ; ~256 bytes, opaque
 *     t = HCC_TryEnter(&frame)                   ; setjmp inside runtime
 *     br (t == 0), body_block, catch_block
 *   body_block:
 *     <lower try_body>
 *     HCC_TryLeave(&frame)                       ; normal completion
 *     jmp end_block
 *   catch_block:
 *     HCC_TryLeave(&frame)                       ; pop *before* handler
 *                                                ; so a throw inside catch
 *                                                ; escapes outward
 *     <lower catch_body>
 *     jmp end_block
 *   end_block:
 *
 * The CatchFrame layout (jmp_buf bytes + prev pointer) is opaque to
 * the compiler; we just reserve enough stack for the largest platform
 * jmp_buf. The HolyC-side `class CatchFrame` agrees on the size. */
void irLowerTry(IrCtx *ctx, Ast *ast) {
    /* 1. Reserve CatchFrame storage on the caller's stack. Generous
     *    size (256 bytes) covers every host jmp_buf we care about. */
    int frame_size = 256;
    IrValue *size_const = irConstInt(IR_TYPE_I64, frame_size);
    IrValue *frame_tmp = irTmp(IR_TYPE_PTR, frame_size);
    IrInstr *frame_alloca = irInstrNew(IR_ALLOCA, frame_tmp, size_const, NULL);
    irBlockAddInstr(ctx, frame_alloca);
    irAddStackSpace(ctx, frame_size);

    /* 2. Materialise &frame as a pointer-typed tmp for the runtime
     *    calls' arg. */
    IrValue *frame_addr = irTmp(IR_TYPE_PTR, 8);
    irBlockAddInstr(ctx,
        irInstrNew(IR_LEA, frame_addr, frame_tmp, NULL));

    /* 3. Push the frame onto the catch-stack via a tiny helper, then
     *    call setjmp INLINE in the user's function. setjmp must be
     *    called from a function whose stack frame still exists when
     *    longjmp fires - calling it inside HCC_TryEnter (a returned
     *    helper) was UB. The helper that managed the chain is split
     *    out so the IR stays declarative. */
    irEmitRuntimeCall1(ctx, "HCC_PushFrame",
                        IR_TYPE_VOID, 8, frame_addr);
    IrValue *enter_ret = irEmitRuntimeCall1(ctx, "setjmp",
                                              IR_TYPE_I64, 8, frame_addr);

    /* 4. Branch: (enter_ret == 0) -> body, else -> catch. */
    IrBlock *body_block  = irBlockNew();
    IrBlock *catch_block = irBlockNew();
    IrBlock *end_block   = irBlockNew();

    IrValue *cmp_zero = irTmp(IR_TYPE_I64, 8);
    IrInstr *cmp = irICmp(cmp_zero, IR_CMP_EQ, enter_ret,
                          irConstInt(IR_TYPE_I64, 0));
    irBlockAddInstr(ctx, cmp);
    irBranch(ctx->cur_func, ctx->cur_block, cmp_zero,
             body_block, catch_block);

    /* 5. body_block: lower try body, leave the catch frame, jump to end. */
    irFnAddBlock(ctx->cur_func, body_block);
    ctx->cur_block = body_block;
    irLowerAst(ctx, ast->try_body);
    if (!ctx->cur_block->sealed) {
        irEmitRuntimeCall1(ctx, "HCC_TryLeave",
                            IR_TYPE_VOID, 8, frame_addr);
        IrInstr *jmp = irJump(ctx->cur_func, ctx->cur_block, end_block);
        irBlockAddInstr(ctx, jmp);
    }

    /* 6. catch_block: pop the frame first (so a throw from inside the
     *    handler propagates to the next outer try, not back to us),
     *    then lower the handler, then jump to end. */
    irFnAddBlock(ctx->cur_func, catch_block);
    ctx->cur_block = catch_block;
    irEmitRuntimeCall1(ctx, "HCC_TryLeave",
                        IR_TYPE_VOID, 8, frame_addr);
    irLowerAst(ctx, ast->catch_body);
    if (!ctx->cur_block->sealed) {
        IrInstr *jmp = irJump(ctx->cur_func, ctx->cur_block, end_block);
        irBlockAddInstr(ctx, jmp);
    }

    /* 7. end_block: subsequent statements continue here. */
    irFnAddBlock(ctx->cur_func, end_block);
    ctx->cur_block = end_block;
}

/* throw(value) lowers to a single HCC_Throw(value) call. The runtime
 * longjmps so control never returns; mark the current block sealed
 * so the lowering driver doesn't append a fall-through edge. */
void irLowerThrow(IrCtx *ctx, Ast *ast) {
    IrValue *v = irExpr(ctx, ast->throw_value);
    irEmitRuntimeCall1(ctx, "HCC_Throw", IR_TYPE_VOID, 8, v);
    ctx->cur_block->sealed = 1;
}

void irLowerIf(IrCtx *ctx, Ast *ast) {
    IrValue *cond_val = irExpr(ctx, ast->cond);
    IrBlock *then_block = irBlockNew();
    IrBlock *else_block = ast->els ? irBlockNew() : NULL;
    IrBlock *join_block = irBlockNew();
    IrBlock *false_target = else_block ? else_block : join_block;

    irBranch(ctx->cur_func,
             ctx->cur_block,
             cond_val,
             then_block,
             false_target);

    irFnAddBlock(ctx->cur_func, then_block);
    ctx->cur_block = then_block;
    irLowerAst(ctx, ast->then);

    if (!ctx->cur_block->sealed) {
        IrInstr *jmp = irJump(ctx->cur_func, ctx->cur_block, join_block);
        irBlockAddInstr(ctx, jmp);
    }

    if (else_block) {
        irFnAddBlock(ctx->cur_func, else_block);
        ctx->cur_block = else_block;
        irLowerAst(ctx, ast->els);
        if (!ctx->cur_block->sealed) {
            IrInstr *jmp = irJump(ctx->cur_func, ctx->cur_block, join_block);
            irBlockAddInstr(ctx, jmp);
        }
    }

    irFnAddBlock(ctx->cur_func, join_block);
    ctx->cur_block = join_block;
}

void irLowerForLoop(IrCtx *ctx, Ast *ast) {
    if (!ast->forbody) return;
    if (ast->forinit)
        irLowerAst(ctx, ast->forinit);
    if (ctx->cur_block->sealed) return;

    IrBlock *header = irBlockNew();
    IrBlock *body = irBlockNew();
    IrBlock *step = irBlockNew();
    IrBlock *end = irBlockNew();

    IrInstr *to_header = irJump(ctx->cur_func, ctx->cur_block, header);
    irBlockAddInstr(ctx, to_header);

    irFnAddBlock(ctx->cur_func, header);
    ctx->cur_block = header;

    if (ast->forcond) {
        IrValue *cond_val = irExpr(ctx, ast->forcond);
        irBranch(ctx->cur_func, ctx->cur_block, cond_val, body, end);
    } else {
        IrInstr *j = irJump(ctx->cur_func, ctx->cur_block, body);
        irBlockAddInstr(ctx, j);
    }

    irPushLoopCtx(ctx, step, end);

    irFnAddBlock(ctx->cur_func, body);
    ctx->cur_block = body;
    irLowerAst(ctx, ast->forbody);

    if (!ctx->cur_block->sealed) {
        IrInstr *j = irJump(ctx->cur_func, ctx->cur_block, step);
        irBlockAddInstr(ctx, j);
    }

    irFnAddBlock(ctx->cur_func, step);
    ctx->cur_block = step;

    if (ast->forstep)
        irLowerAst(ctx, ast->forstep);

    if (!ctx->cur_block->sealed) {
        IrInstr *j = irJump(ctx->cur_func, ctx->cur_block, header);
        irBlockAddInstr(ctx, j);
    }

    irPopLoopCtx(ctx);

    irFnAddBlock(ctx->cur_func, end);
    ctx->cur_block = end;
}

void irLowerWhileLoop(IrCtx *ctx, Ast *ast) {
    if (!ast->whilecond) return;
    IrBlock *header = irBlockNew();
    IrBlock *body = irBlockNew();
    IrBlock *end = irBlockNew();

    IrInstr *to_header = irJump(ctx->cur_func, ctx->cur_block, header);
    irBlockAddInstr(ctx, to_header);

    irFnAddBlock(ctx->cur_func, header);
    ctx->cur_block = header;
    IrValue *cond_val = irExpr(ctx, ast->whilecond);
    irBranch(ctx->cur_func, ctx->cur_block, cond_val, body, end);

    irPushLoopCtx(ctx, header, end);

    irFnAddBlock(ctx->cur_func, body);
    ctx->cur_block = body;
    if (ast->whilebody)
        irLowerAst(ctx, ast->whilebody);
    if (!ctx->cur_block->sealed) {
        IrInstr *j = irJump(ctx->cur_func, ctx->cur_block, header);
        irBlockAddInstr(ctx, j);
    }

    irPopLoopCtx(ctx);

    irFnAddBlock(ctx->cur_func, end);
    ctx->cur_block = end;
}

void irLowerDoWhileLoop(IrCtx *ctx, Ast *ast) {
    /* Cond runs at end of body, but we put it in its own block so `continue`
     * has a target. */
    if (!ast->whilecond) return;
    IrBlock *body = irBlockNew();
    IrBlock *cond = irBlockNew();
    IrBlock *end = irBlockNew();

    IrInstr *to_body = irJump(ctx->cur_func, ctx->cur_block, body);
    irBlockAddInstr(ctx, to_body);

    irPushLoopCtx(ctx, cond, end);

    irFnAddBlock(ctx->cur_func, body);
    ctx->cur_block = body;
    if (ast->whilebody)
        irLowerAst(ctx, ast->whilebody);
    if (!ctx->cur_block->sealed) {
        IrInstr *j = irJump(ctx->cur_func, ctx->cur_block, cond);
        irBlockAddInstr(ctx, j);
    }

    irFnAddBlock(ctx->cur_func, cond);
    ctx->cur_block = cond;
    IrValue *cond_val = irExpr(ctx, ast->whilecond);
    irBranch(ctx->cur_func, ctx->cur_block, cond_val, body, end);

    irPopLoopCtx(ctx);

    irFnAddBlock(ctx->cur_func, end);
    ctx->cur_block = end;
}

/* Build the lvar's `IrValue` directly, no IR_ALLOCA emitted. The
 * stack slot lives implicitly via `irCgComputeAstLayout` (which walks
 * `ast_func->locals` and assigns each lvar's loff) plus `irCgBindAstLoffs`
 * (which binds the IrValue.var.id -> loff). The IR instruction stream
 * doesn't need to mention the slot at all.
 *
 * Note: `var_id` is the AST-side lvar id (key in `func->variables`).
 * The IrValue's own `var.id` is minted from the IR tmp counter so it
 * doesn't collide with tmp ids in the loff map (the two id spaces both
 * start at 1). `irFnGetVar(func, lvar_id)` still finds the IrValue;
 * the codegen looks up loff via `IrValue.var.id`. */
static IrValue *irMakeLocalSlot(IrCtx *ctx,
                                IrValueType ir_type,
                                int size,
                                u32 var_id)
{
    IrValue *local = irTmp(ir_type, size);
    local->kind = IR_VAL_LOCAL;
    irFnAddVar(ctx->cur_func, var_id, local);
    irAddStackSpace(ctx, size);
    return local;
}

void irLowerDecl(IrCtx *ctx, Ast *ast) {
    Ast *var = ast->declvar;
    Ast *init = ast->declinit;
    u32 var_id;

    /* Register-pinned local (TempleOS `<Type> reg <REG> name`).
     * No slot - the value lives in the named machine register for the
     * whole function. The per-arch emitter sees the IrValue's
     * `pinned_reg` and writes the register directly. */
    if (var->kind == AST_LVAR && var->pinned_kind == LVAR_REG) {
        IrValue *local = irValueNew(irConvertType(var->type), IR_VAL_LOCAL);
        local->as.var.id = var->lvar_id;
        local->as.var.size = var->type ? var->type->size : 8;
        local->pinned_reg = var->pinned_reg;
        irFnAddVar(ctx->cur_func, var->lvar_id, local);
        if (init) {
            IrValue *iv = irExpr(ctx, init);
            IrInstr *st = irInstrNew(IR_STORE, local, iv, NULL);
            irBlockAddInstr(ctx, st);
        }
        return;
    }

    IrValue *local;
    if (var->kind == AST_FUNPTR) {
        local = irMakeLocalSlot(ctx, IR_TYPE_PTR, 8, var->fn_ptr_id);
        var_id = var->fn_ptr_id;
    } else {
        local = irMakeLocalSlot(ctx,
                                irConvertType(var->type),
                                var->type->size,
                                var->lvar_id);
        var_id = var->lvar_id;
    }
    (void)var_id;

    if (init) {
        IrValue *ir_init = NULL;

        switch (init->kind) {
            case AST_ARRAY_INIT: {
                /* `T arr[N] = {...}` (and `T m[R][C] = {...}`,
                 * `Type3 arr[] = {{1,1,"a"},...}`). The walk
                 * is type-aware: array parents stride by elem
                 * size; class / union parents stride by each
                 * item's own size (matching asmArrayInit). */
                irLowerArrayInitWalk(ctx, local, 0, var->type, init);
                break;
            }
            case AST_FUN_PROTO:
            case AST_FUNC:
            case AST_EXTERN_FUNC:
            case AST_ASM_FUNCDEF:
            case AST_ASM_FUNC_BIND: {
                loggerPanic("Unhandled: %s\n", astKindToString(init->kind));
                break;
            }
            case AST_ASM_FUNCALL:
            case AST_FUNPTR_CALL:
            case AST_FUNCALL: {
                /* Struct-by-value return: route the call's
                 * hidden out-pointer directly at the local's
                 * slot so the call writes the result in-place
                 * (no temp / memcpy needed). For scalar
                 * returns, redirect the call's dst at the local's
                 * slot so the spill writes straight into the frame. */
                if (init->type && irRetTypeIsAggregate(init->type)) {
                    IrValue *out = irTmp(IR_TYPE_PTR, 8);
                    irBlockAddInstr(ctx, irInstrNew(IR_LEA, out, local, NULL));
                    irFnCallTo(ctx, init, out);
                } else {
                    IrValue *ret = irLowerFnCall(ctx, init);
                    /* The return value lands in the variable's slot, so it
                     * must first be coerced to the variable's type - a
                     * float return assigned to an int (or vice versa)
                     * needs a real fcvt, not a raw register spill. Same
                     * chain as the scalar `default` initialiser path. */
                    if ((astIsFloatType(init->type) && astIsIntType(var->type)) ||
                        (astIsIntType(init->type) && astIsFloatType(var->type)))
                    {
                        ret = irLowerCast(ctx, ret, init->type, var->type);
                    }
                    ret = irNarrowToTargetWidth(ctx, ret, var->type);
                    ret = irWidenToTargetWidth(ctx, ret, init->type, var->type);
                    ret = irConvertFloatToTargetWidth(ctx, ret, var->type);
                    if (local->pinned_reg ||
                        !irRedirectLastDst(ctx, ret, local))
                    {
                        irBlockAddInstr(ctx, irStore(local, ret));
                    }
                }
                break;
            }

            default: {
                /* Struct/class copy-init `Color c = other;`: memcpy the
                 * source aggregate's bytes into the new slot rather than
                 * treating it as a scalar (which would store the source's
                 * address into the first 8 bytes). Struct-returning calls
                 * are handled by the AST_FUNCALL case above. */
                if (irIsByValAggregate(var->type)) {
                    IrValue *dst_addr = irTmp(IR_TYPE_PTR, 8);
                    irBlockAddInstr(ctx,
                            irInstrNew(IR_LEA, dst_addr, local, NULL));
                    IrValue *src_addr = irLValueAddr(ctx, init);
                    irEmitMemcpy(ctx, dst_addr, src_addr, var->type->size);
                    break;
                }
                ir_init = irExpr(ctx, init);
                /* float<->int initialiser (`I64 j = f32v;`) needs a real
                 * conversion (sitofp/fptosi), not a raw bit copy. */
                if ((astIsFloatType(init->type) && astIsIntType(var->type)) ||
                    (astIsIntType(init->type) && astIsFloatType(var->type)))
                {
                    ir_init = irLowerCast(ctx, ir_init, init->type, var->type);
                }
                ir_init = irNarrowToTargetWidth(ctx, ir_init, var->type);
                ir_init = irWidenToTargetWidth(ctx, ir_init,
                                               init->type, var->type);
                ir_init = irConvertFloatToTargetWidth(ctx, ir_init, var->type);
                if (!local->pinned_reg &&
                    irRedirectLastDst(ctx, ir_init, local))
                {
                    break;
                }
                irBlockAddInstr(ctx, irStore(local, ir_init));
                break;
            }
        }
    }

}

void irLowerGoto(IrCtx *ctx, Ast *ast) {
    /* Look up or lazily create the target block by label name.
     * Forward gotos hit this branch first; the matching AST_LABEL
     * later finds the same block in the map. */
    AoStr *lname = ast->slabel;
    if (!lname)
        loggerPanic("AST_GOTO with no label\n");
    IrBlock *target = ctx->labels
        ? (IrBlock *)mapGetLen(ctx->labels, lname->data, lname->len)
        : NULL;

    if (!target) {
        target = irBlockNew();
        if (!ctx->labels) ctx->labels = mapNew(8, &map_cstring_opaque_type);
        mapAddLen(ctx->labels, lname->data, lname->len, target);
    }

    IrInstr *jmp = irJump(ctx->cur_func, ctx->cur_block, target);
    irBlockAddInstr(ctx, jmp);
    /* Anything emitted before the next AST_LABEL is dead - give
     * it a fresh block so seal-once invariants hold. */
    IrBlock *unreach = irBlockNew();
    irFnAddBlock(ctx->cur_func, unreach);
    ctx->cur_block = unreach;

}

void irLowerLabel(IrCtx *ctx, Ast *ast) {
    AoStr *lname = ast->slabel;
    if (!lname) loggerPanic("AST_LABEL with no label\n");
    IrBlock *target = ctx->labels
        ? (IrBlock *)mapGetLen(ctx->labels, lname->data, lname->len)
        : NULL;
    if (!target) {
        target = irBlockNew();
        if (!ctx->labels) ctx->labels = mapNew(8, &map_cstring_opaque_type);
        mapAddLen(ctx->labels, lname->data, lname->len, target);
    }
    if (!ctx->cur_block->sealed) {
        IrInstr *jmp = irJump(ctx->cur_func, ctx->cur_block, target);
        irBlockAddInstr(ctx, jmp);
    }
    irFnAddBlock(ctx->cur_func, target);
    ctx->cur_block = target;

}

void irLowerSwitch(IrCtx *ctx, Ast *ast) {
    /* This is lowered as an if else chain as opposed to a jump table */
    IrValue *cond = irExpr(ctx, ast->switch_cond);
    int n = ast->cases ? (int)ast->cases->size : 0;

    IrBlock *end_block = irBlockNew();
    IrBlock *default_block = ast->case_default ? irBlockNew() : end_block;

    IrBlock **body_blocks = NULL;
    if (n > 0) {
        body_blocks = (IrBlock **)calloc(n, sizeof(IrBlock *));
        for (int i = 0; i < n; i++) {
            body_blocks[i] = irBlockNew();
        }
    }

    irPushLoopCtx(ctx, NULL, end_block);

    /* Test chain. */
    for (int i = 0; i < n; i++) {
        Ast *_case = (Ast *)ast->cases->entries[i];
        IrBlock *body = body_blocks[i];
        IrBlock *next_test = (i + 1 < n)
            ? irBlockNew() : default_block;

        if (_case->case_begin == _case->case_end) {
            IrValue *cmp = irTmp(IR_TYPE_I8, 1);
            IrValue *k = irConstInt(IR_TYPE_I64, _case->case_begin);
            IrInstr *cmp_instr = irInstrNew(IR_ICMP, cmp, cond, k);
            cmp_instr->extra.cmp_kind = IR_CMP_EQ;
            irBlockAddInstr(ctx, cmp_instr);
            irBranch(ctx->cur_func, ctx->cur_block, cmp, body, next_test);
        } else {
            /* Range case `case lo...hi:` -> two compares
             * chained with a transient block. */
            IrBlock *upper = irBlockNew();
            IrValue *ge = irTmp(IR_TYPE_I8, 1);
            IrValue *kb = irConstInt(IR_TYPE_I64, _case->case_begin);
            IrInstr *ge_i = irInstrNew(IR_ICMP, ge, cond, kb);
            ge_i->extra.cmp_kind = IR_CMP_GE;
            irBlockAddInstr(ctx, ge_i);
            irBranch(ctx->cur_func, ctx->cur_block, ge, upper, next_test);
            irFnAddBlock(ctx->cur_func, upper);
            ctx->cur_block = upper;

            IrValue *le = irTmp(IR_TYPE_I8, 1);
            IrValue *ke = irConstInt(IR_TYPE_I64, _case->case_end);
            IrInstr *le_i = irInstrNew(IR_ICMP, le, cond, ke);
            le_i->extra.cmp_kind = IR_CMP_LE;
            irBlockAddInstr(ctx, le_i);
            irBranch(ctx->cur_func, ctx->cur_block, le, body, next_test);
        }

        if (i + 1 < n) {
            irFnAddBlock(ctx->cur_func, next_test);
            ctx->cur_block = next_test;
        }
    }

    /* If there were no cases, the cond eval ran but we never
     * branched - just jump to default/end. */
    if (n == 0 && !ctx->cur_block->sealed) {
        IrInstr *j = irJump(ctx->cur_func, ctx->cur_block, default_block);
        irBlockAddInstr(ctx, j);
    }

    /* Case bodies - each falls through to the next on no break. */
    for (int i = 0; i < n; i++) {
        irFnAddBlock(ctx->cur_func, body_blocks[i]);
        ctx->cur_block = body_blocks[i];
        Ast *_case = (Ast *)ast->cases->entries[i];
        if (_case->case_asts) {
            listForEach(_case->case_asts) {
                irLowerAst(ctx, (Ast *)it->value);
            }
        }
        if (!ctx->cur_block->sealed) {
            IrBlock *next = (i + 1 < n)
                ? body_blocks[i + 1] : default_block;
            IrInstr *j = irJump(ctx->cur_func, ctx->cur_block,
                    next);
            irBlockAddInstr(ctx, j);
        }
    }

    if (ast->case_default) {
        irFnAddBlock(ctx->cur_func, default_block);
        ctx->cur_block = default_block;
        if (ast->case_default->case_asts) {
            listForEach(ast->case_default->case_asts) {
                irLowerAst(ctx, (Ast *)it->value);
            }
        }
        if (!ctx->cur_block->sealed) {
            IrInstr *j = irJump(ctx->cur_func, ctx->cur_block,
                    end_block);
            irBlockAddInstr(ctx, j);
        }
    }

    irPopLoopCtx(ctx);
    irFnAddBlock(ctx->cur_func, end_block);
    ctx->cur_block = end_block;
    free(body_blocks);
}

void irLowerAst(IrCtx *ctx, Ast *ast) {
    if (!ast) return;
    if (ast->line && ast->kind != AST_LVAR && ast->kind != AST_GVAR)
        ir_line_hint = ast->line;

    /* Once a block is sealed (it ended in ret/jmp/br) any subsequent
     * statement is unreachable; just drop it. AST_LABEL / AST_CASE /
     * AST_DEFAULT are re-entry points - their handlers start fresh
     * blocks, so let them run even when the prior block is sealed. */
    if (ctx->cur_block && ctx->cur_block->sealed &&
            ast->kind != AST_LABEL && ast->kind != AST_CASE &&
            ast->kind != AST_DEFAULT) {
        return;
    }

    switch (ast->kind) {
        case AST_COMPOUND_STMT: {
            listForEach(ast->stms) {
                Ast *next = (Ast *)it->value;
                irLowerAst(ctx, next);
            }
            break;
        }

        case AST_GVAR:
        case AST_LVAR:
            break;

        case AST_BINOP: {
            irExpr(ctx, ast);
            break;
        }

        case AST_RETURN: {
            irLowerReturn(ctx, ast);
            break;
        }

        case AST_IF: {
            irLowerIf(ctx, ast);
            break;
        }

        case AST_TRY: {
            irLowerTry(ctx, ast);
            break;
        }

        case AST_THROW: {
            irLowerThrow(ctx, ast);
            break;
        }

        case AST_ASM_STMT: {
            /* Inline `asm { ... }` block. Carry both the text (for
             * legacy/pure asm) and the optional fragment list (when
             * `&var` references need late binding to stack offsets).
             * Each lvar referenced in the asm body is already in the
             * escape set, so it gets a real slot via the usual
             * layout path the asm fragment renders against that
             * slot's loff at emit time. */
            IrValue *txt = irValueNew(IR_TYPE_ARRAY, IR_VAL_CONST_STR);
            txt->as.str.str = ast->asm_stmt;
            txt->as.str.label = NULL;
            txt->as.str.str_real_len = ast->asm_stmt
                ? (int)ast->asm_stmt->len : 0;
            IrInstr *instr = irInstrNew(IR_ASM, NULL, txt, NULL);
            instr->extra.asm_fragments = ast->asm_fragments;
            irBlockAddInstr(ctx, instr);
            break;
        }

        case AST_UNOP: {
            /* Bare ++/-- as a statement: lower as expression and discard. */
            irExpr(ctx, ast);
            break;
        }

        case AST_FOR: {
            irLowerForLoop(ctx, ast);
            break;
        }

        case AST_WHILE: {
            irLowerWhileLoop(ctx, ast);
            break;
        }

        case AST_DO_WHILE: {
            irLowerDoWhileLoop(ctx, ast);
            break;
        }

        case AST_BREAK: {
            IrLoopCtx *lc = irCurLoopCtx(ctx);
            if (!lc) loggerPanic("break outside a loop\n");
            IrInstr *j = irJump(ctx->cur_func, ctx->cur_block, lc->break_block);
            irBlockAddInstr(ctx, j);
            break;
        }

        case AST_CONTINUE: {
            IrLoopCtx *lc = irCurLoopCtx(ctx);
            if (!lc) loggerPanic("continue outside a loop\n");
            IrInstr *j = irJump(ctx->cur_func, ctx->cur_block, lc->continue_block);
            irBlockAddInstr(ctx, j);
            break;
        }
 
        case AST_DECL: {
            irLowerDecl(ctx, ast);
            break;
        }

        case AST_FUNCALL:
        case AST_ASM_FUNCALL:
        case AST_FUNPTR_CALL: {
            irExpr(ctx, ast);
            break;
        }

        case AST_STRING: {
            irExpr(ctx, ast);
            break;
        }

        case AST_GOTO: {
            irLowerGoto(ctx, ast);
            break;
        }

        case AST_LABEL: {
            irLowerLabel(ctx, ast);
            break;
        }

        case AST_SWITCH: {
            irLowerSwitch(ctx, ast);
            break;
        }

        default:
            loggerPanic("Unhandled Ast kind `%s`\n%s\n",
                    astKindToString(ast->kind),
                    astToString(ast));
            break;
    }
}

/* Escape analysis: walk a function's body and collect lvar_ids whose
 * address is taken. The downstream lowerer uses this to decide whether
 * a read of `x` can return x's slot directly (no IR_LOAD tmp) or must
 * go through the load so aliased writes are observed. */
static void irEscapeWalk(Ast *ast, Set *escape);

static void irEscapeWalkList(List *l, Set *escape) {
    if (!l) return;
    listForEach(l) {
        irEscapeWalk((Ast *)it->value, escape);
    }
}

static void irEscapeWalkVec(Vec *v, Set *escape) {
    if (!v) return;
    for (u64 i = 0; i < v->size; ++i) {
        irEscapeWalk(vecGet(Ast *, v, i), escape);
    }
}

static void irEscapeWalk(Ast *ast, Set *escape) {
    if (!ast) return;
    switch (ast->kind) {
        case AST_LVAR:
        case AST_GVAR:
        case AST_LITERAL:
        case AST_STRING:
        case AST_BREAK:
        case AST_CONTINUE:
        case AST_GOTO:
        case AST_LABEL:
        case AST_JUMP:
        case AST_COMMENT:
        case AST_PLACEHOLDER:
        case AST_FUNPTR:
        case AST_FUN_PROTO:
        case AST_EXTERN_FUNC:
        case AST_FUNC:
        case AST_ASM_FUNCDEF:
        case AST_ASM_FUNC_BIND:
            return;

        case AST_UNOP: {
            /* Address-of an lvar (possibly wrapped in a default-param
             * shim) marks it as escaping. Be conservative on the parser
             * quirk for &ptr-to-scalar (loads the value) still mark. */
            if (ast->unop == AST_UN_OP_ADDR_OF && ast->operand) {
                Ast *target = irUnwrapDefaultParam(ast->operand);
                if (target && target->kind == AST_LVAR) {
                    setAdd(escape, (void *)(u64)target->lvar_id);
                }
            }
            irEscapeWalk(ast->operand, escape);
            return;
        }

        case AST_CAST: {
            irEscapeWalk(ast->operand, escape);
            return;
        }

        case AST_BINOP: {
            irEscapeWalk(ast->left, escape);
            irEscapeWalk(ast->right, escape);
            return;
        }

        case AST_FUNCALL:
        case AST_FUNPTR_CALL:
        case AST_ASM_FUNCALL: {
            irEscapeWalkVec(ast->args, escape);
            irEscapeWalk(ast->ref, escape);
            return;
        }

        case AST_RETURN: {
            irEscapeWalk(ast->retval, escape);
            return;
        }

        case AST_IF: {
            irEscapeWalk(ast->cond, escape);
            irEscapeWalk(ast->then, escape);
            irEscapeWalk(ast->els, escape);
            return;
        }

        case AST_FOR: {
            irEscapeWalk(ast->forinit, escape);
            irEscapeWalk(ast->forcond, escape);
            irEscapeWalk(ast->forstep, escape);
            irEscapeWalk(ast->forbody, escape);
            return;
        }

        case AST_WHILE:
        case AST_DO_WHILE: {
            irEscapeWalk(ast->whilecond, escape);
            irEscapeWalk(ast->whilebody, escape);
            return;
        }

        case AST_TRY: {
            irEscapeWalk(ast->try_body, escape);
            irEscapeWalk(ast->catch_body, escape);
            return;
        }

        case AST_THROW: {
            irEscapeWalk(ast->throw_value, escape);
            return;
        }

        case AST_COMPOUND_STMT: {
            irEscapeWalkList(ast->stms, escape);
            irEscapeWalk(ast->inline_ret, escape);
            return;
        }

        case AST_DECL:
        case AST_DEFAULT_PARAM: {
            irEscapeWalk(ast->declinit, escape);
            return;
        }

        case AST_CLASS_REF: {
            irEscapeWalk(ast->cls, escape);
            return;
        }

        case AST_SWITCH: {
            irEscapeWalk(ast->switch_cond, escape);
            irEscapeWalkVec(ast->cases, escape);
            irEscapeWalk(ast->case_default, escape);
            return;
        }

        case AST_CASE:
        case AST_DEFAULT: {
            irEscapeWalkList(ast->case_asts, escape);
            return;
        }

        case AST_SIZEOF: {
            /* sizeof's operand isn't evaluated, but `&x` inside still
             * marks x: cheap and keeps the walker pessimistic. */
            irEscapeWalk(ast->operand, escape);
            return;
        }

        case AST_ASM_STMT: {
            if (ast->asm_fragments) {
                listForEach(ast->asm_fragments) {
                    AsmFragment *f = (AsmFragment *)it->value;
                    if (f->kind == ASM_FRAG_LVAR_REF && f->lvar) {
                        setAdd(escape,
                               (void *)(u64)f->lvar->lvar_id);
                    }
                }
            }
            return;
        }

        case AST_ARRAY_INIT: {
            irEscapeWalkList(ast->arrayinit, escape);
            return;
        }

        case AST_VAR_ARGS:
            return;

        default:
            /* Unknown kind: safe no-op. Missing a recurse means we
             * over-load (extra IR_LOADs), never miscompile. */
            return;
    }
}

static Set *irCollectEscapingLVars(Ast *ast_func) {
    Set *escape = setNew(8, &set_uint_type);
    if (ast_func && ast_func->body) {
        irEscapeWalk(ast_func->body, escape);
    }
    return escape;
}

IrFunction *irLowerFunction(IrCtx *ctx, Ast *ast_func) {
    /* Prologue instructions attribute to the definition, not to
     * whatever line the previous function ended on. */
    ir_line_hint = ast_func->line;

    IrFunction *func = irFunctionNew(ast_func->fname);
    IrBlock *entry = irBlockNew();
    IrBlock *exit_block = irBlockNew();

    ctx->cur_block = entry;
    ctx->cur_func = func;
    ctx->labels = NULL; /* fresh label table per function */
    if (ctx->escape_set) setRelease(ctx->escape_set);
    ctx->escape_set = irCollectEscapingLVars(ast_func);
    func->entry_block = entry;
    func->exit_block = exit_block;
    irFnAddBlock(func, entry);

    /* Lower parameters. Each AST param maps to two IrValues:
     *
     *   - arrive  (IR_VAL_PARAM, loc=REG/abi_arg_reg): the value as it
     *     arrives in the ABI register. Consumed exactly once, by the
     *     entry IR_STORE below.
     *   - slot    (IR_VAL_LOCAL, ast-driven loff): the frame slot that
     *     all body-level references read/write. AST -> IR lookups
     *     resolve to this.
     *
     * The entry IR_STORE `store slot, arrive` makes the param spill an
     * ordinary IR operation. Codegen needs no special prologue path.
     * A later store-load forwarding pass collapses the spill+reload
     * when the value is still live in the ABI register at first use. */
    IrRegPool *pool = irRegPoolGet();
    int int_arg_idx = 0, float_arg_idx = 0;

    AstType *rettype = ast_func->type->rettype;
    /* Only an INDIRECT (>16-byte) struct return uses a hidden out-pointer.
     * A <=16-byte aggregate is returned in registers, so it has a normal
     * struct-sized return slot (the scalar-return branch below) and no
     * hidden parameter. */
    int has_hidden_out_ptr = rettype && irRetIsIndirect(rettype);
    /* Struct-by-value return: the caller passes a hidden out-pointer
     * in the first int-arg register. Bumps int_arg_idx so the user's
     * first real int param lands on the second arg reg. */
    if (has_hidden_out_ptr) int_arg_idx = 1;

    for (u64 i = 0; i < ast_func->params->size; ++i) {
        Ast *ast_param = vecGet(Ast *, ast_func->params, i);

        if (ast_param->kind == AST_VAR_ARGS) {
            assert(func->has_var_args);
            /* Apple AArch64 puts ALL variadic args (including HolyC's
             * implicit argc) on the stack, so argc doesn't arrive in a
             * register and shouldn't be spilled. Its slot is just a
             * view of the caller's stack region (loff set by layout). */
            int apple_aarch64_va = pool && pool->variadic_on_stack;

            IrValue *argc_arrive = irTmp(IR_TYPE_I64, 8);
            argc_arrive->kind = IR_VAL_PARAM;
            if (!apple_aarch64_va && pool &&
                (u64)int_arg_idx < pool->int_arg_regs->size)
            {
                argc_arrive->loc.kind = IR_LOC_REG;
                argc_arrive->loc.as.reg =
                    vecGet(AoStr *, pool->int_arg_regs, int_arg_idx);
            }
            int_arg_idx++;

            IrValue *argc_slot = irTmp(IR_TYPE_I64, 8);
            argc_slot->kind = IR_VAL_LOCAL;
            irFnAddVar(func, ast_param->argc->lvar_id, argc_slot);
            vecPush(func->params, argc_arrive);
            irAddStackSpace(ctx, 8);
            if (!apple_aarch64_va) {
                irBlockAddInstr(ctx,
                                irInstrNew(IR_STORE, argc_slot,
                                           argc_arrive, NULL));
            }

            IrValue *argv_iv = irTmp(IR_TYPE_PTR, 8);
            argv_iv->kind = IR_VAL_PARAM;
            irFnAddVar(func, ast_param->argv->lvar_id, argv_iv);
            vecPush(func->params, argv_iv);
            irAddStackSpace(ctx, 8);
            break;
        }
        if (ast_param->kind != AST_LVAR &&
            ast_param->kind != AST_FUNPTR &&
            ast_param->kind != AST_DEFAULT_PARAM) {
            loggerPanic("Unhandled key kind: %s\n",
                    astKindToString(ast_param->kind));
        }

        u32 key = irGetParamId(ast_param);
        IrValueType ir_type = irConvertType(ast_param->type);
        u16 size = ast_param->type->size;
        int is_float = ast_param->type->kind == AST_TYPE_FLOAT;

        /* AArch64 by-value struct param: it arrives in GP/FP registers
         * (or by reference) per AAPCS, not as a single pointer. Create
         * the slot only; the backend prologue unpacks the registers into
         * it. Advance the ABI counters by the struct's register class so
         * later scalar params land on the right registers. */
        int aarch64_target =
            ctx->cc->target == TARGET_AARCH64_APPLE_DARWIN ||
            ctx->cc->target == TARGET_AARCH64_UNKNOWN_LINUX_GNU;
        int x86_target =
            ctx->cc->target == TARGET_X86_64_APPLE_DARWIN ||
            ctx->cc->target == TARGET_X86_64_UNKNOWN_LINUX_GNU;
        if ((aarch64_target || x86_target) &&
            (ast_param->type->kind == AST_TYPE_CLASS ||
             ast_param->type->kind == AST_TYPE_UNION) &&
            !ast_param->type->is_intrinsic)
        {
            IrValue *slot = irTmp(IR_TYPE_STRUCT, size);
            slot->kind = IR_VAL_LOCAL;
            slot->byval_struct_type = ast_param->type;
            irFnAddVar(func, key, slot);
            vecPush(func->params, slot);
            irAddStackSpace(ctx, size);

            if (aarch64_target) {
                int elem = 0, count = 0;
                AapcsClass cls = astAapcsClassify(ast_param->type, &elem,
                                                  &count);
                if (cls == AAPCS_HFA) {
                    if (float_arg_idx + count <= 8) float_arg_idx += count;
                    else float_arg_idx = 8;
                } else if (cls == AAPCS_INTEGER) {
                    int ngp = (ast_param->type->size + 7) / 8;
                    if (int_arg_idx + ngp <= 8) int_arg_idx += ngp;
                    else int_arg_idx = 8;
                } else {
                    if (int_arg_idx < 8) int_arg_idx++;
                }
            } else { /* x86 SysV: advance GP/SSE counters per eightbyte. */
                SysvClass classes[2];
                int neb = 0;
                if (!astSysvClassify(ast_param->type, classes, &neb)) {
                    int gp = 0, sse = 0;
                    for (int e = 0; e < neb; ++e)
                        if (classes[e] == SYSV_INTEGER) gp++; else sse++;
                    /* Both classes must fit, else the whole aggregate is
                     * MEMORY (consumes no registers). */
                    if (int_arg_idx + gp <= 6 && float_arg_idx + sse <= 8) {
                        int_arg_idx += gp;
                        float_arg_idx += sse;
                    }
                }
            }
            continue;
        }

        /* Allocate this param's ABI arg register up front so the
         * arrive-IrValue's loc reflects where the caller put it. */
        AoStr *abi_reg = NULL;
        if (pool) {
            if (is_float) {
                if ((u64)float_arg_idx < pool->float_arg_regs->size)
                    abi_reg = vecGet(AoStr *, pool->float_arg_regs,
                                     float_arg_idx);
                float_arg_idx++;
            } else {
                if ((u64)int_arg_idx < pool->int_arg_regs->size)
                    abi_reg = vecGet(AoStr *, pool->int_arg_regs,
                                     int_arg_idx);
                int_arg_idx++;
            }
        }

        /* Register overflow: the param was passed on the stack. Create
         * the slot only (no arrive value for regalloc to place); the
         * backend's stack-param prologue copies it in from the incoming
         * argument area. Mirrors the by-value struct param handling.
         * Pinned params keep the existing reg-move path. */
        if (!abi_reg && pool &&
            !(ast_param->kind == AST_LVAR &&
              ast_param->pinned_kind == LVAR_REG && ast_param->pinned_reg))
        {
            IrValue *slot = irTmp(ir_type, size);
            slot->kind = IR_VAL_LOCAL;
            irFnAddVar(func, key, slot);
            vecPush(func->params, slot);
            irAddStackSpace(ctx, size);
            continue;
        }

        /* TempleOS-pinned param: the body reads/writes the named
         * register directly, no stack slot. Still need to copy the
         * value from the ABI arg reg into the pinned reg at entry,
         * which we model as an IR_STORE dst=pinned_iv,
         * r1=arrive. Codegen's IR_STORE recognises pinned_reg
         * destinations and emits the reg-to-reg move uniformly. */
        if (ast_param->kind == AST_LVAR &&
            ast_param->pinned_kind == LVAR_REG &&
            ast_param->pinned_reg)
        {
            IrValue *pin = irTmp(ir_type, size);
            pin->kind = IR_VAL_PARAM;
            pin->pinned_reg = ast_param->pinned_reg;
            irFnAddVar(func, key, pin);
            vecPush(func->params, pin);
            irAddStackSpace(ctx, size);

            IrValue *arrive = irTmp(ir_type, size);
            arrive->kind = IR_VAL_PARAM;
            if (abi_reg) {
                arrive->loc.kind = IR_LOC_REG;
                arrive->loc.as.reg = abi_reg;
            }
            irBlockAddInstr(ctx, irInstrNew(IR_STORE, pin, arrive, NULL));
            continue;
        }

        IrValue *arrive = irTmp(ir_type, size);
        arrive->kind = IR_VAL_PARAM;
        if (abi_reg) {
            arrive->loc.kind = IR_LOC_REG;
            arrive->loc.as.reg = abi_reg;
        }

        IrValue *slot = irTmp(ir_type, size);
        slot->kind = IR_VAL_LOCAL;
        irFnAddVar(func, key, slot);
        vecPush(func->params, arrive);
        irAddStackSpace(ctx, size);

        irBlockAddInstr(ctx, irInstrNew(IR_STORE, slot, arrive, NULL));
    }

    IrValue *ir_return_var = NULL;
    if (has_hidden_out_ptr) {
        /* Hidden out-pointer: arrives in int_arg_regs[0] (rdi), spilled
         * to a slot via the same arrive+store mechanism as a normal
         * param. func->return_value is the slot so RET sees a stable
         * stack location. */
        IrValue *out_arrive = irTmp(IR_TYPE_PTR, 8);
        out_arrive->kind = IR_VAL_PARAM;
        if (pool && pool->int_arg_regs->size > 0) {
            out_arrive->loc.kind = IR_LOC_REG;
            out_arrive->loc.as.reg =
                vecGet(AoStr *, pool->int_arg_regs, 0);
        }
        IrValue *out_slot = irTmp(IR_TYPE_PTR, 8);
        out_slot->kind = IR_VAL_LOCAL;
        irAddStackSpace(ctx, 8);
        irBlockAddInstr(ctx, irInstrNew(IR_STORE, out_slot, out_arrive, NULL));
        ir_return_var = out_slot;
    } else if (rettype && rettype->kind != AST_TYPE_VOID) {
        IrInstr *ir_return_alloca = irAlloca(rettype);
        irAddStackSpace(ctx, rettype->size);
        irBlockAddInstr(ctx, ir_return_alloca);
        ir_return_var = ir_return_alloca->dst;
    }
    func->return_value = ir_return_var;

    irLowerAst(ctx, ast_func->body);

    /* Fall through to exit if the body didn't already end in a return. */
    if (!ctx->cur_block->sealed) {
        IrInstr *jmp = irJump(func, ctx->cur_block, exit_block);
        irBlockAddInstr(ctx, jmp);
    }

    /* Populate the exit block: optional load, then ret. */
    irFnAddBlock(func, exit_block);
    ctx->cur_block = exit_block;
    if (ir_return_var && irRetIsRegisterAggregate(rettype)) {
        /* <=16-byte struct returned in registers: ir_return_var is the
         * struct-sized return slot. Tag it so the backend RET loads its
         * bytes into the result registers (v0.. / x0,x1 - SSE/INTEGER per
         * the per-backend classifier) instead of doing a scalar load. */
        ir_return_var->byval_struct_type = rettype;
        IrInstr *ir_ret = irInstrNew(IR_RET, ir_return_var, NULL, NULL);
        irBlockAddInstr(ctx, ir_ret);
    } else if (ir_return_var) {
        IrValue *ir_load_dst = irTmp(ir_return_var->type, rettype->size);
        IrInstr *ir_load = irLoad(ir_load_dst, ir_return_var);
        irBlockAddInstr(ctx, ir_load);
        /* IR_RET stores the return value in `dst` to match the existing
         * convention used by IR_BR (debug printer, ARM64 backend). */
        IrInstr *ir_ret = irInstrNew(IR_RET, ir_load_dst, NULL, NULL);
        irBlockAddInstr(ctx, ir_ret);
    } else {
        IrInstr *ir_ret = NULL;
        if (func->name->len == 4 &&
                !strncasecmp(func->name->data, str_lit("Main"))) {
            ir_ret = irInstrNew(IR_RET, irConstInt(IR_TYPE_I32, 0), NULL, NULL);
        } else {
            ir_ret = irInstrNew(IR_RET, NULL, NULL, NULL);
        }
        irBlockAddInstr(ctx, ir_ret);
    }
    exit_block->sealed = 1;

    if (ctx->escape_set) {
        setRelease(ctx->escape_set);
        ctx->escape_set = NULL;
    }
    return func;
}


/* Per-translation-unit counter so block labels in two functions
 * compiled together can't collide. */
static u32 ircg_func_seq = 0;

void irFunctionPrepForCodeGen(IrCgCtx *ctx, IrFunction *fn, Ast *ast_fn) {
    ctx->fn = fn;
    ctx->cur_block = NULL;
    ctx->next_block = NULL;

    fn->uuid = ircg_func_seq++;
    fn->ra.func = fn;
    fn->ra.id_to_loff = mapNew(32, &map_uint_to_uint_type);
    fn->ra.extra_stack = 0;

    irOptPinResultReg(fn);
    irOptDeadCodeElim(fn);

    int locals_params_space = irCgComputeAstLayout(ast_fn, fn);

    /* Bind AST-driven loffs (params + alloca'd locals) before walking the
     * IR for tmp slots. An alloca's dst tmp shares its var.id with the AST
     * local it represents; binding first lets `irCgAllocTmp`'s mapHasInt
     * guard skip those tmps cleanly (and avoids double-counting their
     * space in `extra_stack`). */
    irCgBindAstLoffs(&fn->ra, ast_fn);

    /* Allocate slots for IR tmps just below the params area. */
    irCgAllocAllTmps(&fn->ra, locals_params_space);

    /* Compute total stack reservation (params + locals + tmps),
     * aligned to 16. The architecture-specific emitter feeds this
     * into its prologue. */
    int total_stack = locals_params_space + fn->ra.extra_stack;
    if (total_stack > 0) total_stack = align(total_stack, 16);

    /* Stack required for the function */
    fn->stack_space = (u16)total_stack;
}

void irCgFinishFunction(IrCgCtx *ctx) {
    if (ctx) {
        IrFunction *fn = ctx->fn;
        if (fn->ra.id_to_loff)
            mapRelease(fn->ra.id_to_loff);
    }
}

void irDump(Cctrl *cc) {
    IrCtx *ctx = irCtxNew(cc);
    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            ctx->cur_func = NULL;
            IrFunction *fn = irLowerFunction(ctx, ast);
            irPrintFunction(fn);
 
            irBasicFunctionOptimisations(fn);
            printf("===== After basic optimisations ===== \n");
            irPrintFunction(fn);
        }
    }
}

IrCtx *irLowerProgram(Cctrl *cc) {
    IrCtx *ctx = irCtxNew(cc);
    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            ctx->cur_func = NULL;
            IrFunction *fn = irLowerFunction(ctx, ast);
            irBasicFunctionOptimisations(fn);
            irCtxAddFunction(ctx, fn);
        }
    }
    return ctx;
}
