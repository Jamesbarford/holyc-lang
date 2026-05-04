#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "ast.h"
#include "ir.h"
#include "ir-eval.h"
#include "ir-mem2reg.h"
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

    if (val->kind == IR_VAL_TMP && val->as.var.size < (u64)target_ty->size) {
        IrValue *narrow = irTmp(narrow_ty, target_ty->size);
        irBlockAddInstr(ctx, irInstrNew(IR_TRUNC, narrow, val, NULL));
        return narrow;
    }

    return val;
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

/* Make a call to holyc's memcpy routine. Used for struct by value returns. 
 * Simpler to do this than emit a bunch of instructions and from codegen is 
 * easy enough to reason with without feeling like magic. */
static void irEmitMemcpy(IrCtx *ctx,
                         IrValue *dst, 
                         IrValue *src,
                         int n_bytes)
{
    IrValue *args_wrap = irValueNew(IR_TYPE_ARRAY, IR_VAL_UNRESOLVED);
    args_wrap->as.array.label = aoStrPrintf("MEMCPY");
    Vec *args = irValueVecNew();
    args_wrap->as.array.values = args;
    vecPush(args, dst);
    vecPush(args, src);
    vecPush(args, irConstInt(IR_TYPE_I64, n_bytes));

    /* MEMCPY's return is `U0 *` (the dst pointer); we don't consume
     * it. The codegen reserves a slot for the dst tmp anyway, but the
     * use-counting + zero-use slot-skip in irCgAnnotate will prune
     * it. */
    IrValue *ret = irTmp(IR_TYPE_PTR, 8);
    IrInstr *call = irInstrNew(IR_CALL, ret, args_wrap, NULL);
    irBlockAddInstr(ctx, call);
}


IrValue *irFnCallTo(IrCtx *ctx, Ast *ast, IrValue *preallocated_buffer) {
    /* Is this returning a struct from the stack? */
    int agg_return = irRetTypeIsAggregate(ast->type);
    IrValueType ret_type = agg_return ? IR_TYPE_PTR : irConvertType(ast->type);
    int ret_size = agg_return ? 8 : ast->type->size;
    IrValue *ir_call_args = irValueNew(IR_TYPE_ARRAY, IR_VAL_UNRESOLVED);
    IrValue *ir_ret_val = irTmp(ret_type, ret_size);
    IrInstr *ir_call_instr = irInstrNew(IR_CALL, ir_ret_val, ir_call_args, NULL);

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
     * source in r2, codegen knows to load it into a register and emit
     * `call *<reg>`. */
    Ast *callee = NULL;
    Vec *params = NULL;
    if (ast->kind == AST_FUNPTR_CALL) {
        ir_call_args->as.array.label = NULL;
        IrValue *ptr = irExpr(ctx, ast->ref);
        ir_call_instr->r2 = ptr;
        /* AST_FUNPTR holds its declared param list on `params`. Find the
         * underlying decl so we can apply default-param fill-in. */
        Ast *ref = ast->ref;
        while (ref && ref->kind != AST_FUNPTR) {
            if (ref->kind == AST_LVAR || ref->kind == AST_GVAR) {
                /* Not a function-pointer slot we can walk for params. */
                ref = NULL;
                break;
            }
            ref = NULL;
        }
        if (ref && ref->kind == AST_FUNPTR) {
            params = ref->params;
        }
    } else {
        ir_call_args->as.array.label = ast->fname;
        /* Look up the callee to discover default-param fill-ins. The call
         * site itself can omit trailing args when the corresponding param
         * is AST_DEFAULT_PARAM; we substitute the param's declinit here.
         * Functions defined elsewhere (libtos) appear as AST_FUN_PROTO;
         * their params still carry default-value info. */
        if (ctx->cc && ast->fname) {
            callee = mapGetLen(ctx->cc->global_env,
                               ast->fname->data, ast->fname->len);
        }
        if (callee && callee->type &&
            (callee->kind == AST_FUNC || callee->kind == AST_FUN_PROTO ||
             callee->kind == AST_EXTERN_FUNC)) {
            params = callee->params;
        }
    } 

    u64 n_args = ast->args ? ast->args->size : 0;
    u64 n_params = params ? params->size : 0;
    u64 n_total = n_args > n_params ? n_args : n_params;

    for (u64 i = 0; i < n_total; ++i) {
        Ast *ast_arg = NULL;
        if (i < n_args) {
            ast_arg = ast->args->entries[i];
        } else if (i < n_params) {
            Ast *p = (Ast *)params->entries[i];
            if (p && p->kind == AST_DEFAULT_PARAM) {
                ast_arg = p->declinit;
            } else if (p && p->kind == AST_FUNPTR && p->default_fn) {
                /* `T (*fn)(...) = X` default - the parser stashes the
                 * default expression on `default_fn->declinit`. */
                ast_arg = p->default_fn->declinit;
            }
        }
        if (!ast_arg) break;
        IrValue *ir_arg = irExpr(ctx, ast_arg);
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
        float_dispatch = irIsFloat(ir_type) || left_is_float;
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
        IrValue *dst = irTmp(IR_TYPE_F64, 8);
        irBlockAddInstr(ctx, irInstrNew(IR_SITOFP, dst, src, NULL));
        return dst;
    }
    if (from_float && to_int) {
        IrValueType t = irConvertType(to_type);
        IrValue *dst = irTmp(t, to_type->size);
        irBlockAddInstr(ctx, irInstrNew(IR_FPTOSI, dst, src, NULL));
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

/* Lower `<lhs> op= rhs` (or `<lhs> = rhs` when op is AST_BIN_OP_ASSIGN)
 * and return the value written back. */
static IrValue *irLowerAssign(IrCtx *ctx, Ast *ast) {
    IrAssignTarget tgt = irLowerAssignTarget(ctx, ast->left);

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

    new_val = irNarrowToTargetWidth(ctx, new_val, tgt.type);

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
    int parent_is_array = target_ty &&
                          target_ty->kind == AST_TYPE_ARRAY;
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

    IrValueType ir_value_type = is_intrinsic_class
        ? IR_TYPE_I64 : irConvertType(ast->type);

    int load_size = is_intrinsic_class ? 8 : ast_ty->size;
    IrValue *ir_load_dest = irTmp(ir_value_type, load_size);
    IrInstr *load_instr = irLoad(ir_load_dest, local_var);
    irBlockAddInstr(ctx, load_instr);
    return ir_load_dest;
}

IrValue *irLowerGlobal(IrCtx *ctx, Ast *ast) {
    /* Global int read: lea its address, then load*. Arrays
     * decay to a pointer to the first element - just return
     * the LEA'd address with no load. */
    IrValue *gv = irGlobalValue(ast);
    IrValue *addr = irTmp(IR_TYPE_PTR, 8);
    irBlockAddInstr(ctx, irInstrNew(IR_LEA, addr, gv, NULL));
    if (astTypeIsArray(ast->type)) {
        return addr;
    }
    IrValueType ir_type = irConvertType(ast->type);
    IrValue *load_dst = irTmp(ir_type, ast->type->size);
    irBlockAddInstr(ctx, irInstrNew(IR_LOAD_DEREF, load_dst, addr, NULL));
    return load_dst;
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
    int field_is_array = astTypeIsArray(ast->type);
    IrValueType field_ir_type = field_is_array
        ? IR_TYPE_PTR : irConvertType(ast->type);
    IrValue *load_dst = irTmp(field_ir_type,
            field_is_array ? 8 : ast->type->size);
    int offset = ast->type->offset;

    irCollapseClassRefChain(&cls, &offset);

    if (astIsUnOpKind(cls, AST_UN_OP_DEREF)) {
        IrValue *ptr_val = irExpr(ctx, cls->operand);
        IrValue *addr = ptr_val;
        if (offset != 0) {
            addr = irTmp(IR_TYPE_PTR, 8);
            IrValue *off_const = irConstInt(IR_TYPE_I64, offset);
            irBlockAddInstr(ctx,
                    irInstrNew(IR_IADD, addr, ptr_val, off_const));
        }
        if (field_is_array) {
            /* `p->arr` decays to the address of the first
             * element - that's just the field pointer itself. */
            return addr;
        }
        irBlockAddInstr(ctx, irInstrNew(IR_LOAD_DEREF, load_dst, addr, NULL));
        return load_dst;
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
    if (field_is_array) {
        /* `cls.arr` decays to the field's address. The GEP tmp aliases the
         * stack slot rather than holding a pointer, so emit an explicit
         * IR_LEA to materialize the pointer value. */
        IrValue *dst = irTmp(IR_TYPE_PTR, 8);
        irBlockAddInstr(ctx, irInstrNew(IR_LEA, dst, field_addr, NULL));
        return dst;
    }
    irBlockAddInstr(ctx, irLoad(load_dst, field_addr));
    return load_dst;
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
        return load_dst;
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
        IrValue *dst = irTmp(vt, irIsFloat(vt) ? 8 : ast->operand->type->size);
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
        delta = irConstFloat(IR_TYPE_F64, 1.0);
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
                        IrValue *value = irConstFloat(IR_TYPE_F64, ast->f64);
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
            irBlockAddInstr(ctx, irLoad(dst_addr, ctx->cur_func->return_value));
            irEmitMemcpy(ctx, dst_addr, src_addr, rt->size);
        } else {
            IrValue *val = irExpr(ctx, ast->retval);
            IrInstr *st = irStore(ctx->cur_func->return_value, val);
            irBlockAddInstr(ctx, st);
        }
    }
    IrInstr *jmp = irJump(ctx->cur_func,
                          ctx->cur_block,
                          ctx->cur_func->exit_block);
    irBlockAddInstr(ctx, jmp);
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

void irLowerDecl(IrCtx *ctx, Ast *ast) {
    Ast *var = ast->declvar;
    Ast *init = ast->declinit;
    IrInstr *ir_alloca;
    u32 var_id;
    if (var->kind == AST_FUNPTR) {
        /* Function-pointer slot: 8 bytes, treated as a pointer. */
        IrValue *tmp = irTmp(IR_TYPE_PTR, 8);
        IrValue *sz = irConstInt(IR_TYPE_PTR, 8);
        ir_alloca = irInstrNew(IR_ALLOCA, tmp, sz, NULL);
        irAddStackSpace(ctx, 8);
        var_id = var->fn_ptr_id;
    } else {
        ir_alloca = irAlloca(var->type);
        irAddStackSpace(ctx, var->type->size);
        var_id = var->lvar_id;
    }
    irBlockAddInstr(ctx, ir_alloca);
    IrValue *local = ir_alloca->dst;
    irFnAddVar(ctx->cur_func, var_id, local);

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
                 * returns, store the call's value into the
                 * slot as usual. */
                if (init->type && irRetTypeIsAggregate(init->type)) {
                    IrValue *out = irTmp(IR_TYPE_PTR, 8);
                    irBlockAddInstr(ctx, irInstrNew(IR_LEA, out, local, NULL));
                    irFnCallTo(ctx, init, out);
                } else {
                    IrValue *ret = irLowerFnCall(ctx, init);
                    IrInstr *ir_store = irStore(local, ret);
                    irBlockAddInstr(ctx, ir_store);
                }
                break;
            }

            default: {
                ir_init = irExpr(ctx, init);
                ir_init = irNarrowToTargetWidth(ctx, ir_init, var->type);
                IrInstr *ir_store = irStore(local, ir_init);
                irBlockAddInstr(ctx, ir_store);
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

void irSimplifyFunction(IrFunction *fn) {
    Set *work_queue_ids = setNew(16, &set_uint_type);
    List *queue = listNew();
    Set *blocks_to_delete = setNew(16, &set_int_type);

    (void)work_queue_ids;
    (void)queue;
    (void)blocks_to_delete;

    /* Any blocks that don't have a successor lets assume they jump to 
     * the return */
    listForEach(fn->blocks) {
        IrBlock *block = it->value;
        if (irBlockIsStartOrEnd(fn, block)) continue;
        Map *cur_successors = irBlockGetSuccessors(fn, block);
        if (cur_successors && cur_successors->size == 0) {
            /* @TODO
             * Think I need a new file to contain making ir instructions/ values*/
            irJump(fn, block, fn->exit_block);
        }
    }
}

IrFunction *irLowerFunction(IrCtx *ctx, Ast *ast_func) {
    IrFunction *func = irFunctionNew(ast_func->fname);
    IrBlock *entry = irBlockNew();
    IrBlock *exit_block = irBlockNew();

    ctx->cur_block = entry;
    ctx->cur_func = func;
    ctx->labels = NULL; /* fresh label table per function */
    func->entry_block = entry;
    func->exit_block = exit_block;
    irFnAddBlock(func, entry);

    /* Lower parameters into per-id IrValue slots. */
    for (u64 i = 0; i < ast_func->params->size; ++i) {
        Ast *ast_param = vecGet(Ast *, ast_func->params, i);
        if (ast_param->kind == AST_VAR_ARGS) {
            assert(func->has_var_args);
            /* Materialize argc and argv as ordinary parameter slots. The
             * codegen prologue (asmFunctionFromIr) is responsible for
             * spilling the next-int-reg into argc and storing
             * `&caller_stack` into argv; from the body's perspective they
             * read like plain int/ptr locals. */
            IrValue *argc_iv = irTmp(IR_TYPE_I64, 8);
            argc_iv->kind = IR_VAL_PARAM;
            irFnAddVar(func, ast_param->argc->lvar_id, argc_iv);
            vecPush(func->params, argc_iv);
            irAddStackSpace(ctx, 8);

            IrValue *argv_iv = irTmp(IR_TYPE_PTR, 8);
            argv_iv->kind = IR_VAL_PARAM;
            irFnAddVar(func, ast_param->argv->lvar_id, argv_iv);
            vecPush(func->params, argv_iv);
            irAddStackSpace(ctx, 8);
            break;
        }
        u32 key = 0;
        if (ast_param->kind == AST_LVAR) {
            key = ast_param->lvar_id;
        } else if (ast_param->kind == AST_FUNPTR) {
            key = ast_param->fn_ptr_id;
        } else if (ast_param->kind == AST_DEFAULT_PARAM) {
            key = ast_param->declvar->lvar_id;
        } else {
            loggerPanic("Unhandled key kind: %s\n",
                    astKindToString(ast_param->kind));
        }
        IrValue *ir_tmp_var = irTmp(irConvertType(ast_param->type),
                                    ast_param->type->size);
        ir_tmp_var->kind = IR_VAL_PARAM;
        irFnAddVar(func, key, ir_tmp_var);
        vecPush(func->params, ir_tmp_var);
        irAddStackSpace(ctx, ast_param->type->size);
    }

     /* Struct-by-value return: instead of an alloca, we synthesize a
      * hidden out-pointer parameter (8 bytes, ptr-typed, IR_VAL_PARAM).
      * The codegen prologue spills %rdi into its slot, the body's
      * AST_RETURN-of-class memcpys the local into *out_ptr, and the exit
      * block loads the out_ptr's slot into %rax (per SysV "by memory"
      * return). */
    AstType *rettype = ast_func->type->rettype;
    IrValue *ir_return_var = NULL;
    if (rettype && irRetTypeIsAggregate(rettype)) {
        IrValue *out_ptr = irTmp(IR_TYPE_PTR, 8);
        out_ptr->kind = IR_VAL_PARAM;
        irAddStackSpace(ctx, 8);
        ir_return_var = out_ptr;
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
    if (ir_return_var) {
        IrValue *ir_load_dst = irTmp(ir_return_var->type, rettype->size);
        IrInstr *ir_load = irLoad(ir_load_dst, ir_return_var);
        irBlockAddInstr(ctx, ir_load);
        /* IR_RET stores the return value in `dst` to match the existing
         * convention used by IR_BR (debug printer, ARM64 backend). */
        IrInstr *ir_ret = irInstrNew(IR_RET, ir_load_dst, NULL, NULL);
        irBlockAddInstr(ctx, ir_ret);
    } else {
        IrInstr *ir_ret = irInstrNew(IR_RET, NULL, NULL, NULL);
        irBlockAddInstr(ctx, ir_ret);
    }
    exit_block->sealed = 1;

    return func;
}

static void irRemoveAllNops(IrFunction *fn) {
    listForEach(fn->blocks) {
        IrBlock *bb = it->value;
        List *l = bb->instructions;
        List *it = l->next;

        while (it != l) {
            List *next = it->next;
            List *prev = it->prev;
            IrInstr *I = listValue(IrInstr *, it);
            if (I->op == IR_NOP) {
                prev->next = next;
                next->prev = prev;
                free(it);
            }
            it = next;
        }
    }
}

/* If a blocks instructions match the `removal_id` then we swap it
 * for `target` */
static void irRemapInstrBlockIds(IrBlock *bb, u64 removal_id, IrBlock *target) {
    listForEach(bb->instructions) {
        IrInstr *I = it->value;
        switch (I->op) {
            case IR_PHI: {
                Vec *pairs = I->extra.phi_pairs;
                for (u64 i = 0; i < pairs->size; ++i) {
                    IrPair *p = pairs->entries[i];
                    if (p->ir_block->id == removal_id) {
                        p->ir_block = target;
                    }
                }
                break;
            }
            case IR_BR: {
                IrBlockPair *blk_pair = &I->extra.blocks;
                if (blk_pair->target_block->id == removal_id) {
                    blk_pair->target_block = target;
                }
                if (blk_pair->fallthrough_block->id == removal_id) {
                    blk_pair->fallthrough_block = target;
                }
                break;
            }
            case IR_JMP: {
                IrBlockPair *blk_pair = &I->extra.blocks;
                if (blk_pair->target_block->id == removal_id) {
                    blk_pair->target_block = target;
                }
                break;
            }
            default:
                break;
        }
    }
}

static void irRemoveRedundantBlocks(IrFunction *fn) {
    Vec *removal = vecNew(&vec_ir_block_type);
    Map *from_to = mapNew(16, &map_u32_to_ir_block_type);

    listForEach(fn->blocks) {
        IrBlock *bb = it->value;
        IrInstr *jmp = irBlockLastInstr(bb);

        if (jmp && jmp->op == IR_JMP) {
            if (irBlockIsRedundantJump(fn, bb)) {
                if (jmp->op == IR_JMP) {
                    IrBlock *R = jmp->extra.blocks.target_block;
                    IrInstr *first = irBlockFirstInstr(R);
                    /* We can't remove phis */
                    if (first && first->op == IR_PHI) {
                        continue;
                    }

                    mapAdd(from_to, (void *)(u64)R->id, bb);
                    vecPush(removal, R);
                    /* Remove redundant jump */
                    listPop(bb->instructions);
                    /* Merge instructions.
                     * This is destructive and leads R->instructions pointing
                     * to garbage as it gets freed */
                    listMergeAppend(bb->instructions, R->instructions);
                    /* Make the instructions NULL, otherwise if we see this
                     * block again we could touch invalid memory */
                    R->instructions = NULL;
                }
            }
        }
    }

    for (u64 i = 0; i < removal->size; ++i) {
        IrBlock *R = removal->entries[i];
        IrBlock *target = mapGet(from_to, (void *)(u64)R->id);
        listRemoveValue(fn->blocks,removal->entries[i]);

        listForEach(fn->blocks) {
            IrBlock *bb = it->value;
            Map *successors = irBlockGetSuccessors(fn, bb);
            Map *predecessors = irBlockGetPredecessors(fn, bb);

            /* Remap sucessors */
            if (successors && mapHasInt(successors, R->id)) {
                irRemapInstrBlockIds(bb, R->id, target);
                mapRemoveInt(successors, R->id);
                mapAdd(successors, (void *)(u64)target->id, target);
            }

            /* Remap predecessors */
            if (predecessors && mapHasInt(predecessors, R->id)) {
                irRemapInstrBlockIds(bb, R->id, target);
                mapRemoveInt(predecessors, R->id);
                mapAdd(predecessors, (void *)(u64)target->id, target);
            }
        }
    }

    vecRelease(removal);
    mapRelease(from_to);
}

void irBasicFunctionOptimisations(IrFunction *fn) {
    irRemoveAllNops(fn);
    irRemoveRedundantBlocks(fn);
    irEvalConstantExpressions(fn);
    // remove unused functions?
    // create a callgraph?
    irRemoveAllNops(fn);
}

/* Per-translation-unit counter so block labels in two functions
 * compiled together can't collide. */
static u32 ircg_func_seq = 0;

void irFunctionPrepForCodeGen(IrCgCtx *ctx, IrFunction *fn, Ast *ast_fn) {
    (void)ast_fn;
    irCgClassifyPhis(fn);
    irCgAnnotate(fn);
 
    int locals_params_space = irCgComputeAstLayout(ast_fn, fn);

    fn->uuid = ircg_func_seq++;
    fn->ra.func = fn;
    fn->ra.id_to_loff = mapNew(32, &map_uint_to_uint_type);
    fn->ra.extra_stack = 0;

    ctx->fn = fn;
    ctx->cur_block = NULL;
    ctx->next_block = NULL;
    /* Index every peephole-deferred LEA by its dst tmp id, so the
     * IR_CALL emit can re-create the leaq into the arg register. */
    ctx->lea_inline_map = mapNew(8, &map_uint_to_uint_type);
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        listForEach(bb->instructions) {
            IrInstr *I = (IrInstr *)it->value;
            if (I->op != IR_LEA)
                continue;
            if (!(I->flags & IRCG_LEA_INLINE_AT_CALL))
                continue;
            if (!I->dst || I->dst->kind != IR_VAL_TMP)
                continue;
            mapAdd(ctx->lea_inline_map,
                   (void *)(u64)I->dst->as.var.id,
                   (void *)I);
        }
    }
    /* Allocate slots for IR tmps just below the params area. */
    irCgAllocAllTmps(&fn->ra, locals_params_space);

    /* Compute total stack reservation (params + locals + tmps),
     * aligned to 16. The architecture-specific emitter feeds this
     * into its prologue. */
    int total_stack = locals_params_space + fn->ra.extra_stack;
    if (total_stack > 0) total_stack = align(total_stack, 16);

    /* Stack required for the function */
    fn->stack_space = (u16)total_stack;

    irCgBindAstLoffs(&fn->ra, ast_fn);
}

void irCgFinishFunction(IrCgCtx *ctx) {
    if (ctx) {
        IrFunction *fn = ctx->fn;
        if (fn->ra.id_to_loff)
            mapRelease(fn->ra.id_to_loff);
        if (ctx->lea_inline_map)
            mapRelease(ctx->lea_inline_map);
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
 
            irMem2Reg(fn);
            printf("===== After mem2reg ===== \n");
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
