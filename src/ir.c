#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "arena.h"
#include "ast.h"
#include "ir.h"
#include "ir-types.h"
#include "ir-debug.h"
#include "ir-fold.h"
#include "ir-mem2reg.h"
#include "util.h"

static Arena ir_arena;
static int ir_arena_init = 0;

void irMemoryInit(void) {
    if (!ir_arena_init) {
        /* @TODO; correct the size of the arena */
        arenaInit(&ir_arena, 512);
        ir_arena_init = 1;
    }
}

void irMemoryRelease(void) {
    if (ir_arena_init) {
        ir_arena_init = 0;
        arenaClear(&ir_arena);
    }
}

void *irAlloc(u32 size) {
    return arenaAlloc(&ir_arena, size);
}

void irMemoryStats(void) {
    printf("ir Arena:\n");
    arenaPrintStats(&ir_arena);
}

void vecIrFunctionToString(AoStr *buf, void *_ir_func) {
    IrFunction *ir_func = _ir_func;
    aoStrCatPrintf(buf, "%s", ir_func->name->data);
}

/* `Vec<IrFunction *>`*/
VecType vec_ir_function_type = {
    .stringify = vecIrFunctionToString,
    .match     = NULL,
    .release   = NULL,
    .type_str  = "IrFunction *",
};

Vec *irFunctionVecNew(void) {
    return vecNew(&vec_ir_function_type);
}

void vecIrPairToString(AoStr *buf, void *_ir_pair) {
    irPairToString(buf, _ir_pair);
}

/* `Vec<IrPair *>`*/
VecType vec_ir_pair_type = {
    .stringify = vecIrPairToString,
    .match     = NULL,
    .release   = NULL,
    .type_str  = "IrPair *",
};

Vec *irPairVecNew(void) {
    return vecNew(&vec_ir_pair_type);
}

void vecIrValueToString(AoStr *buf, void *_ir_value) {
    AoStr *ir_value_str = irValueToString(_ir_value);
    aoStrCatAoStr(buf, ir_value_str);
    aoStrRelease(ir_value_str);
}

/* `Vec<IrValue *>`*/
VecType vec_ir_value_type = {
    .stringify = vecIrValueToString,
    .match     = NULL,
    .release   = NULL,
    .type_str  = "IrValue *",
};

Vec *irValueVecNew(void) {
    return vecNew(&vec_ir_value_type);
}

AoStr *mapIrBlockToString(void *_ir_block) {
    IrBlock *ir_block = (IrBlock *)_ir_block;
    return aoStrPrintf("block %u", ir_block->id);
}

MapType map_u32_to_ir_block_type = {
    .match           = mapIntKeyMatch,
    .hash            = mapIntKeyHash,
    .get_key_len     = mapIntKeyLen,
    .key_to_string   = mapIntToString,
    .key_release     = NULL,
    .value_to_string = mapIrBlockToString,
    .value_release   = NULL,
    .key_type        = "u32",
    .value_type      = "IrBlock *",
};

Map *irBlockMapNew(void) {
    return mapNew(8, &map_u32_to_ir_block_type);
}

AoStr *mapIrBlockMappingToString(void *_ir_block_mapping) {
    IrBlockMapping *mapping = _ir_block_mapping;
    AoStr *str = aoStrNew();
    AoStr *preds = mapKeysToString(mapping->predecessors);
    AoStr *succ = mapKeysToString(mapping->successors);
    aoStrCatFmt(str, "predecessors: %S, successors: %S", preds, succ);
    aoStrRelease(preds);
    aoStrRelease(succ);
    return str;
}

void irBlockMappingRelease(void *_mapping) {
    IrBlockMapping *mapping = _mapping;
    mapRelease(mapping->predecessors);
    mapRelease(mapping->successors);
    
}

AoStr *mapIrValueToString(void *ir_value) {
    return irValueToString((IrValue *)ir_value);
}

/* `Map<u32, IrValue *>`*/
MapType map_u32_to_ir_var_value_type = {
    .match           = mapIntKeyMatch,
    .hash            = mapIntKeyHash,
    .get_key_len     = mapIntKeyLen,
    .key_to_string   = mapIntToString,
    .key_release     = NULL,
    .value_to_string = mapIrValueToString,
    .value_release   = NULL,
    .key_type        = "u32",
    .value_type      = "IrValue *",
};

/* `Map<u32, IrBlockMapping *>`*/
MapType map_u32_to_ir_block_mapping_type = {
    .match           = mapIntKeyMatch,
    .hash            = mapIntKeyHash,
    .get_key_len     = mapIntKeyLen,
    .key_to_string   = mapIntToString,
    .key_release     = NULL,
    .value_to_string = mapIrBlockMappingToString,
    .value_release   = NULL,
    .key_type        = "u32",
    .value_type      = "IrBlockMapping *",
};

Map *irBlockMappingMapNew(void) {
    return mapNew(32, &map_u32_to_ir_block_mapping_type);
}


IrBlockMapping *irBlockMappingNew(int id) {
    IrBlockMapping *mapping = (IrBlockMapping *)irAlloc(sizeof(IrBlockMapping));
    mapping->id = id;
    mapping->successors = irBlockMappingMapNew();
    mapping->predecessors = irBlockMappingMapNew();
    return mapping;
}

Map *irVarValueMap(void) {
    return mapNew(8, &map_u32_to_ir_var_value_type);
}

/* Pass in the whole block to abstract away that we area using an interal 
 * datastructure to keep track of things. I'm trying a few different ones out */
void irFunctionAddSuccessor(IrFunction *func, IrBlock *src, IrBlock *dest) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)mapGetInt(func->cfg, src->id);
    if (!ir_block_mapping) {
        ir_block_mapping = irBlockMappingNew(src->id);
        mapAddIntOrErr(func->cfg, src->id, ir_block_mapping);
    }
    mapAddIntOrErr(ir_block_mapping->successors, dest->id, dest);
}

void irFunctionAddPredecessor(IrFunction *func, IrBlock *src, IrBlock *prev) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)mapGetInt(func->cfg, src->id);
    if (!ir_block_mapping) {
        ir_block_mapping = irBlockMappingNew(src->id);
        mapAddIntOrErr(func->cfg, src->id, ir_block_mapping);
    }
    mapAddIntOrErr(ir_block_mapping->predecessors, prev->id, prev);
}

void irFunctionAddMapping(IrFunction *func, IrBlock *src, IrBlock *dest) {
    irFunctionAddSuccessor(func, src, dest);
    irFunctionAddPredecessor(func, dest, src);
}

static u32 ir_block_id = 1;

void irResetBlockId(void) {
    ir_block_id = 1;
}

IrBlock *irBlockNew(void) {
    IrBlock *block = irAlloc(sizeof(IrBlock));
    block->instructions = listNew();
    block->removed = 0;
    block->sealed = 0;
    block->id = ir_block_id++;
    return block;
}

IrValue *irValueNew(IrValueType type, IrValueKind kind) {
    IrValue *val = irAlloc(sizeof(IrValue));
    memset(val, 0, sizeof(IrValue));
    val->kind = kind;
    val->type = type;
    return val;
}

static u32 ir_tmp_var_id = 1;
void irTmpVariableCountReset(void) {
    ir_tmp_var_id = 1;
}

IrValue *irTmp(IrValueType type, u16 size) {
    IrValue *val = irValueNew(type, IR_VAL_TMP);
    val->as.var.id = ir_tmp_var_id++;
    val->as.var.size = size;
    return val;
}

IrCtx *irCtxNew(Cctrl *cc) {
    IrCtx *ctx = malloc(sizeof(IrCtx));
    ctx->prog = malloc(sizeof(IrProgram));
    ctx->prog->functions = irFunctionVecNew();
    ctx->prog->globals = NULL;
    ctx->cc = cc;
    ctx->loop_depth = 0;
    ctx->labels = NULL;
    return ctx;
}

static void irPushLoopCtx(IrCtx *ctx, IrBlock *cont, IrBlock *brk) {
    if (ctx->loop_depth >= IR_LOOP_STACK_MAX) {
        loggerPanic("Loop nesting > %d not supported in slice\n",
                    IR_LOOP_STACK_MAX);
    }
    ctx->loop_stack[ctx->loop_depth].continue_block = cont;
    ctx->loop_stack[ctx->loop_depth].break_block = brk;
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

void irCtxAddFunction(IrCtx *ctx, IrFunction *func) {
    vecPush(ctx->prog->functions, func);
}

IrInstr *irInstrNew(IrOp op, IrValue *dst, IrValue *r1, IrValue *r2) {
    IrInstr *instr = irAlloc(sizeof(IrInstr));
    memset(instr, 0, sizeof(IrInstr));
    instr->flags = 0;
    instr->op = op;
    instr->dst = dst;
    instr->r1 = r1;
    instr->r2 = r2;
    return instr;
}

IrFunction *irFunctionNew(AoStr *fname) {
    IrFunction *func = irAlloc(sizeof(IrFunction));
    func->name = fname;
    func->blocks = listNew();
    func->cfg = irBlockMappingMapNew();
    func->variables = irVarValueMap();
    func->stack_space = 0;
    func->params = irValueVecNew();
    return func;
}

/* Map an ast id to an ir variable */
void irFnAddVar(IrFunction *func, u32 lvar_id, IrValue *var) {
    int ok = mapAddIntOrErr(func->variables, lvar_id, var);
    if (!ok) {
        AoStr *ir_value_str = irValueToString(var);
        loggerPanic("Mapping exists for %u -> %s", lvar_id, ir_value_str->data);
        free(ir_value_str);
    }
}

void irFnAddBlock(IrFunction *fn, IrBlock *block) {
    listAppend(fn->blocks, block);
}

IrValue *irFnGetVar(IrFunction *func, u32 lvar_id) {
    return mapGetInt(func->variables, lvar_id);
}

void irAddStackSpace(IrCtx *ctx, int size) {
    ctx->cur_func->stack_space += size;
}

void irBlockAddInstr(IrCtx *ctx, IrInstr *instr) {
    listAppend(ctx->cur_block->instructions, instr);
}

IrValue *irConstInt(IrValueType type, s64 i64) {
    IrValue *ir_value = irValueNew(type, IR_VAL_CONST_INT);
    ir_value->as._i64 = i64;
    return ir_value;
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

    /* Normalize non-bool conditions by widening through cmp_ne 0. The
     * codegen of IR_BR tests-and-jumps on its operand; whatever value
     * we hand it must be 0/1 at runtime.
     *
     * Skip the wrap when `cond` was just produced by an IR_ICMP /
     * IR_FCMP: those already yield 0 or 1 (just typed as i64 because
     * the AST tagged the comparison result with the operands' int
     * type), so the extra `cmp_ne, 0` would just re-normalize the
     * already-normalized value. Saves one cmp/setcc/movzbq triple
     * around every loop and `if` test. */
    if (cond->type != IR_TYPE_I8) {
        int already_bool = 0;
        if (!listEmpty(block->instructions)) {
            IrInstr *last = (IrInstr *)block->instructions->prev->value;
            if ((last->op == IR_ICMP || last->op == IR_FCMP) &&
                last->dst == cond) {
                already_bool = 1;
            }
        }
        if (!already_bool) {
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

IrInstr *irJumpInternal(IrFunction *func,
                        IrBlock *block,
                        IrBlock *target,
                        IrOp opcode)
{
    if (!block || !target) {
        loggerPanic("NULL param\n");
    }
    /* For a do-while we need this */
    if (block->sealed) {
        loggerWarning("Tried to add a jump to a sealed block: %d\n",
                block->id);
    }

    IrInstr *instr = irInstrNew(opcode, NULL, NULL, NULL);
    instr->extra.blocks.target_block = target;
    instr->extra.blocks.fallthrough_block = NULL;

    /* This block is done */
    block->sealed = 1;

    /* Now update the control flow graph */
    irFunctionAddMapping(func, block, target);

    return instr;
}

IrInstr *irJump(IrFunction *func, IrBlock *block, IrBlock *target) {
    return irJumpInternal(func, block,target,IR_JMP);
}

IrPair *irPairNew(IrBlock *ir_block, IrValue *ir_value) {
    IrPair *ir_phi_pair = (IrPair *)irAlloc(sizeof(IrPair));
    ir_phi_pair->ir_value = ir_value;
    ir_phi_pair->ir_block = ir_block;
    return ir_phi_pair;
}

IrInstr *irPhi(IrValue *result) {
    IrInstr *ir_phi_instr = irInstrNew(IR_PHI, result, NULL, NULL);
    ir_phi_instr->extra.phi_pairs = irPairVecNew();
    return ir_phi_instr;
}

void irAddPhiIncoming(IrInstr *ir_phi_instr,
                      IrValue *ir_value, 
                      IrBlock *ir_block)
{
    IrPair *ir_phi_pair = irPairNew(ir_block, ir_value);
    vecPush(ir_phi_instr->extra.phi_pairs, ir_phi_pair);
}


IrInstr *irLoad(IrValue *ir_dest, IrValue *ir_value) {
    return irInstrNew(IR_LOAD, ir_dest, ir_value, NULL);
}

/* result is where we are storing something and op1 is the thing we are storing 
 * I think op1 could/shoule have an offset as it is either going to be the 
 * stack or it is going to be a struct/pointer offset? */
IrInstr *irStore(IrValue *ir_dest, IrValue *ir_value) {
    return irInstrNew(IR_STORE, ir_dest, ir_value, NULL);
}

IrValue *irExpr(IrCtx *ctx, Ast *ast);

/* Forward decl for inc/dec lowering on AST_CLASS_REF, defined below
 * alongside the assignment lowering. */
typedef struct IrAssignTarget {
    IrValue *target;
    AstType *type;
    int indirect;
} IrAssignTarget;
static IrAssignTarget irLowerAssignTarget(IrCtx *ctx, Ast *lhs);

/* ----- small AST shape helpers ------------------------------------------ */

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

/* Collapse a chained class-ref `(*cls)` (e.g. `a.b.c` or `p->b.c`) by
 * walking down through nested AST_CLASS_REF parents, accumulating each
 * layer's intra-parent offset into `*offset`. After the call, `*cls`
 * points at the chain's root (an AST_LVAR for stack class, or an
 * AST_UNOP DEREF for ptr-to-class), with `*offset` holding the total
 * byte offset from that root to the originally referenced field. */
static void irCollapseClassRefChain(Ast **cls, int *offset) {
    while (*cls && (*cls)->kind == AST_CLASS_REF) {
        *offset += (*cls)->type->offset;
        *cls = (*cls)->cls;
    }
}

/* Narrow `val` to match `target_type`'s width before storing it through
 * an IR_STORE / IR_STORE_DEREF / AST_DECL init. The codegens of those
 * three ops pick the memory-access width from the value's byte size, so
 * an over-wide value would write past the slot/field/pointee:
 *   - storing an i64 constant TRUE into a Bool / i8 sink writes 8 bytes
 *     (was JsonParseNumber's `*is_hex = TRUE` bug);
 *   - storing into a U8 / U16 / I32 stack class field via IR_GEP would
 *     similarly walk past the field;
 *   - `Bool b = FALSE` into a 1-byte direct slot would clobber saved
 *     %rbp.
 * Constants get re-tagged in place; tmps wider than the target get an
 * explicit IR_TRUNC. Float values pass through. */
static IrValue *irNarrowToTargetWidth(IrCtx *ctx, IrValue *val,
                                      AstType *target_type) {
    if (!val || !target_type || target_type->size <= 0) return val;
    if (irIsFloat(val->type)) return val;
    IrValueType narrow_ty = irConvertType(target_type);
    if (val->kind == IR_VAL_CONST_INT) {
        if (narrow_ty != val->type) {
            return irConstInt(narrow_ty, val->as._i64);
        }
        return val;
    }
    if (val->kind == IR_VAL_TMP &&
        val->as.var.size > (u16)target_type->size) {
        IrValue *narrow = irTmp(narrow_ty, target_type->size);
        irBlockAddInstr(ctx, irInstrNew(IR_TRUNC, narrow, val, NULL));
        return narrow;
    }
    return val;
}

/* Slice-0 eligibility: walk an AST and reject anything we don't yet lower. */
static int irTypeIsSliceInt(AstType *type) {
    return type && type->kind == AST_TYPE_INT;
}

static int irTypeIsSliceFloat(AstType *type) {
    return type && type->kind == AST_TYPE_FLOAT;
}

/* Float arithmetic ops we lower: +, -, *, /, ==, !=, <, <=, >, >=. No
 * shifts/mod/bitwise - those don't apply to F64. */
static int irBinOpIsSliceFloatArith(AstBinOp op) {
    switch (op) {
    case AST_BIN_OP_ADD: case AST_BIN_OP_SUB: case AST_BIN_OP_MUL:
    case AST_BIN_OP_DIV:
    case AST_BIN_OP_EQ: case AST_BIN_OP_NE:
    case AST_BIN_OP_LT: case AST_BIN_OP_LE:
    case AST_BIN_OP_GT: case AST_BIN_OP_GE:
        return 1;
    default:
        return 0;
    }
}

static int irExprIsSliceEligible(Ast *ast);
static int irStmtIsSliceEligible(Ast *ast);

/* The compiler context active during the current eligibility walk. Set
 * by irFunctionEligibleForSlice so AST_FUNCALL can look up callees in
 * cc->global_env to discover variadicness. NULL outside that walk. */
static Cctrl *s_elig_cc = NULL;

static int irBinOpIsSliceArith(AstBinOp op) {
    switch (op) {
    case AST_BIN_OP_ADD: case AST_BIN_OP_SUB: case AST_BIN_OP_MUL:
    case AST_BIN_OP_DIV: case AST_BIN_OP_MOD:
    case AST_BIN_OP_SHL: case AST_BIN_OP_SHR:
    case AST_BIN_OP_BIT_AND: case AST_BIN_OP_BIT_OR: case AST_BIN_OP_BIT_XOR:
    case AST_BIN_OP_EQ: case AST_BIN_OP_NE:
    case AST_BIN_OP_LT: case AST_BIN_OP_LE:
    case AST_BIN_OP_GT: case AST_BIN_OP_GE:
        return 1;
    default:
        return 0;
    }
}

/* Compound-assign forms: x op= y. We lower as `x = x op y` so codegen
 * needs nothing new. */
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

static int irUnOpIsSliceIncDec(AstUnOp op) {
    switch (op) {
    case AST_UN_OP_PRE_INC:  case AST_UN_OP_PRE_DEC:
    case AST_UN_OP_POST_INC: case AST_UN_OP_POST_DEC:
        return 1;
    default:
        return 0;
    }
}

/* Strings are only sensible as function-call args (slice doesn't have a way
 * to assign or return them). */
static int irArgIsSliceEligible(Ast *ast) {
    if (!ast) return 0;
    if (ast->kind == AST_STRING) return 1;
    return irExprIsSliceEligible(ast);
}

/* True when a function returns a non-intrinsic class / union by value -
 * i.e., the slice's hidden out-pointer convention applies. The caller
 * allocates a buffer of sizeof(rettype), passes its address as a hidden
 * first arg (in rdi), and the callee writes the struct bytes there;
 * the call returns the buffer pointer in rax. */
static int irRetTypeIsAggregate(AstType *rettype) {
    if (!rettype) return 0;
    if (rettype->kind != AST_TYPE_CLASS && rettype->kind != AST_TYPE_UNION)
        return 0;
    if (rettype->is_intrinsic) return 0;
    return rettype->size > 0;
}

static int irFuncReturnsAggregate(Ast *ast_func) {
    if (!ast_func || !ast_func->type) return 0;
    return irRetTypeIsAggregate(ast_func->type->rettype);
}

/* Inline memcpy via IR loads/stores. Walks `n_bytes` from `src` to `dst`
 * in 8-byte chunks, dropping to 4 / 2 / 1 byte chunks for the tail.
 * Used for struct-by-value return / class-typed AST_DECL initializer
 * copies; both sides are pointer values. */
static void irEmitInlineMemcpy(IrCtx *ctx, IrValue *dst, IrValue *src,
                               int n_bytes) {
    int off = 0;
    while (off < n_bytes) {
        int chunk;
        IrValueType vt;
        if (n_bytes - off >= 8) { chunk = 8; vt = IR_TYPE_I64; }
        else if (n_bytes - off >= 4) { chunk = 4; vt = IR_TYPE_I32; }
        else if (n_bytes - off >= 2) { chunk = 2; vt = IR_TYPE_I16; }
        else { chunk = 1; vt = IR_TYPE_I8; }

        IrValue *src_p = src;
        IrValue *dst_p = dst;
        if (off != 0) {
            IrValue *k = irConstInt(IR_TYPE_I64, off);
            src_p = irTmp(IR_TYPE_PTR, 8);
            irBlockAddInstr(ctx, irInstrNew(IR_IADD, src_p, src, k));
            dst_p = irTmp(IR_TYPE_PTR, 8);
            irBlockAddInstr(ctx, irInstrNew(IR_IADD, dst_p, dst, k));
        }
        IrValue *val = irTmp(vt, chunk);
        irBlockAddInstr(ctx,
            irInstrNew(IR_LOAD_DEREF, val, src_p, NULL));
        irBlockAddInstr(ctx,
            irInstrNew(IR_STORE_DEREF, dst_p, val, NULL));
        off += chunk;
    }
}

/* Walk an AST_ARRAY_INIT (possibly nested for multi-dim arrays) and
 * require every leaf scalar to be slice-eligible. */
static int irArrayInitIsSliceEligible(Ast *ast) {
    if (!ast) return 1;
    if (ast->kind != AST_ARRAY_INIT) return irExprIsSliceEligible(ast);
    if (!ast->arrayinit) return 1;
    listForEach(ast->arrayinit) {
        if (!irArrayInitIsSliceEligible((Ast *)it->value)) return 0;
    }
    return 1;
}

static int irExprIsSliceEligible(Ast *ast) {
    if (!ast) return 0;
    switch (ast->kind) {
    case AST_LITERAL:
        if (!ast->type) return 0;
        return ast->type->kind == AST_TYPE_INT ||
               ast->type->kind == AST_TYPE_CHAR ||
               ast->type->kind == AST_TYPE_FLOAT;
    case AST_STRING:
        /* A string literal evaluates to its `.L<n>` address, which is just a
         * pointer. The codegen already handles IR_VAL_CONST_STR via leaq. */
        return 1;
    case AST_CAST:
        /* int/F64/pointer casts. Operand must be eligible; the cast itself
         * lowers to IR_SITOFP / IR_FPTOSI / pass-through. */
        if (!ast->operand) return 0;
        if (!irExprIsSliceEligible(ast->operand)) return 0;
        if (!ast->type || !ast->operand->type) return 0;
        {
            AstType *ft = ast->operand->type;
            AstType *tt = ast->type;
            int from_ok = ft->kind == AST_TYPE_INT ||
                          ft->kind == AST_TYPE_CHAR ||
                          ft->kind == AST_TYPE_POINTER ||
                          ft->kind == AST_TYPE_FLOAT ||
                          (ft->kind == AST_TYPE_CLASS && ft->is_intrinsic);
            int to_ok = tt->kind == AST_TYPE_INT ||
                        tt->kind == AST_TYPE_CHAR ||
                        tt->kind == AST_TYPE_POINTER ||
                        tt->kind == AST_TYPE_FLOAT ||
                        (tt->kind == AST_TYPE_CLASS && tt->is_intrinsic);
            return from_ok && to_ok;
        }
    case AST_LVAR:
        /* Slice locals/params are int (incl. CHAR/Bool), pointer, F64,
         * stack class, or array. Loading an int/pointer/F64 reads its
         * value; an array LVAR decays to its address (LEA). */
        if (!ast->type) return 0;
        return ast->type->kind == AST_TYPE_INT ||
               ast->type->kind == AST_TYPE_CHAR ||
               ast->type->kind == AST_TYPE_POINTER ||
               ast->type->kind == AST_TYPE_FLOAT ||
               ast->type->kind == AST_TYPE_ARRAY ||
               (ast->type->kind == AST_TYPE_CLASS &&
                ast->type->is_intrinsic);
    case AST_FUNPTR:
        /* Reading a function-pointer slot - same shape as an LVAR pointer,
         * just with a different kind tag in the AST. */
        return 1;
    case AST_DEFAULT_PARAM:
        /* In a body, this is an lvar reference dressed up with a default
         * value; the lvar is what we actually read. */
        if (!ast->declvar) return 0;
        return irExprIsSliceEligible(ast->declvar);
    case AST_GVAR:
        /* Slice globals: read/write of int. Address-of (`&g`) is handled
         * by the AST_UN_OP_ADDR_OF case, which accepts AST_GVAR operand.
         * AST_TYPE_ARRAY: a global array (`U64 mon_start_days1[12]`)
         * decays to a pointer to its first element when read. */
        if (!ast->type) return 0;
        return ast->type->kind == AST_TYPE_INT ||
               ast->type->kind == AST_TYPE_CHAR ||
               ast->type->kind == AST_TYPE_POINTER ||
               ast->type->kind == AST_TYPE_FLOAT ||
               ast->type->kind == AST_TYPE_ARRAY;
    case AST_BINOP:
        if (ast->binop == AST_BIN_OP_ASSIGN ||
            irBinOpIsCompoundAssign(ast->binop)) {
            /* LHS may be:
             *  - a plain int local,
             *  - a pointer local (just reassigning a pointer),
             *  - an int-typed class field (`p.x` or `p->x`),
             *  - a deref of a pointer (`*p`). */
            if (!ast->left) return 0;
            Ast *lhs = irUnwrapDefaultParam(ast->left);
            int lhs_ok = 0;
            if (lhs->kind == AST_LVAR && lhs->type) {
                lhs_ok = lhs->type->kind == AST_TYPE_INT ||
                         lhs->type->kind == AST_TYPE_CHAR ||
                         lhs->type->kind == AST_TYPE_POINTER ||
                         lhs->type->kind == AST_TYPE_FLOAT ||
                         (lhs->type->kind == AST_TYPE_CLASS &&
                          lhs->type->is_intrinsic);
            } else if (lhs->kind == AST_FUNPTR) {
                lhs_ok = 1;
            } else if (lhs->kind == AST_GVAR && lhs->type) {
                lhs_ok = lhs->type->kind == AST_TYPE_INT ||
                         lhs->type->kind == AST_TYPE_CHAR ||
                         lhs->type->kind == AST_TYPE_POINTER;
            } else if (lhs->kind == AST_CLASS_REF) {
                lhs_ok = irExprIsSliceEligible(lhs);
            } else if (lhs->kind == AST_UNOP &&
                       lhs->unop == AST_UN_OP_DEREF) {
                lhs_ok = irExprIsSliceEligible(lhs);
            }
            return lhs_ok && irExprIsSliceEligible(ast->right);
        }
        /* Logical AND/OR: short-circuit eval, result is i8 0/1.
         * Operands can be any slice-eligible expression. */
        if (ast->binop == AST_BIN_OP_LOG_AND ||
            ast->binop == AST_BIN_OP_LOG_OR) {
            return irExprIsSliceEligible(ast->left)
                && irExprIsSliceEligible(ast->right);
        }
        /* Float arith: result is F64 and op must be a float-supported op. */
        if (irTypeIsSliceFloat(ast->type)) {
            if (!irBinOpIsSliceFloatArith(ast->binop)) return 0;
            return irExprIsSliceEligible(ast->left)
                && irExprIsSliceEligible(ast->right);
        }
        /* Float comparisons return int but operate on float operands. */
        if (irTypeIsSliceInt(ast->type) &&
            ast->left && ast->left->type &&
            ast->left->type->kind == AST_TYPE_FLOAT) {
            if (!irBinOpIsSliceFloatArith(ast->binop)) return 0;
            return irExprIsSliceEligible(ast->left)
                && irExprIsSliceEligible(ast->right);
        }
        /* Pointer arithmetic: `ptr + i`, `ptr - i`, `arr + i`. The result
         * type is pointer/array; the int operand gets scaled by the
         * element size at lowering time. */
        if ((ast->type->kind == AST_TYPE_POINTER ||
             ast->type->kind == AST_TYPE_ARRAY) &&
            (ast->binop == AST_BIN_OP_ADD ||
             ast->binop == AST_BIN_OP_SUB)) {
            return irExprIsSliceEligible(ast->left)
                && irExprIsSliceEligible(ast->right);
        }
        if (!irBinOpIsSliceArith(ast->binop)) return 0;
        /* The parser sometimes types `fp1 == fp2` as AST_TYPE_FUNC (the
         * lhs's type) rather than int. Comparison-shaped binops always
         * produce a 0/1 value at runtime, so accept FUNC/POINTER result
         * types for cmp ops. */
        int is_cmp = ast->binop == AST_BIN_OP_EQ ||
                     ast->binop == AST_BIN_OP_NE ||
                     ast->binop == AST_BIN_OP_LT ||
                     ast->binop == AST_BIN_OP_LE ||
                     ast->binop == AST_BIN_OP_GT ||
                     ast->binop == AST_BIN_OP_GE;
        if (!irTypeIsSliceInt(ast->type)) {
            if (!(is_cmp && ast->type &&
                  (ast->type->kind == AST_TYPE_POINTER ||
                   ast->type->kind == AST_TYPE_FUNC))) {
                return 0;
            }
        }
        return irExprIsSliceEligible(ast->left)
            && irExprIsSliceEligible(ast->right);
    case AST_UNOP:
        if (irUnOpIsSliceIncDec(ast->unop)) {
            if (!ast->operand) return 0;
            /* `_len--` where `_len` is a default-param: the body wraps it
             * in AST_DEFAULT_PARAM; unwrap so the lvar shape checks below
             * see the underlying decl. */
            Ast *operand = irUnwrapDefaultParam(ast->operand);
            int operand_ok = 0;
            if (operand->kind == AST_LVAR) {
                operand_ok = 1;
            } else if (operand->kind == AST_GVAR) {
                /* `g++` / `++g` for a global int / pointer slot. */
                operand_ok = irExprIsSliceEligible(operand);
            } else if (operand->kind == AST_CLASS_REF) {
                /* `v->field++` / `cls.field++`: same shape as a compound
                 * assign on the field - irLowerAssign already knows how
                 * to address it. We just need the field type to be a
                 * scalar we can load+add+store. */
                operand_ok = irExprIsSliceEligible(operand);
            } else if (operand->kind == AST_UNOP &&
                       operand->unop == AST_UN_OP_DEREF) {
                /* `(*p)++` or `arr[i][j]++` (parsed as `*(*(arr+i)+j)`):
                 * same load-add-store shape, addressing handled by
                 * irLowerAssignTarget. */
                operand_ok = irExprIsSliceEligible(operand);
            }
            if (!operand_ok) return 0;
            if (irTypeIsSliceInt(operand->type)) return 1;
            /* Pointer ++/-- scales by sizeof(*ptr). */
            if (operand->type &&
                operand->type->kind == AST_TYPE_POINTER &&
                operand->type->ptr) {
                return 1;
            }
            /* Float ++/--: lower as FADD/FSUB by 1.0. */
            if (irTypeIsSliceFloat(operand->type)) return 1;
            return 0;
        }
        if (ast->unop == AST_UN_OP_DEREF) {
            /* `*p` where p is a pointer / array. Result can be int /
             * char / pointer / F64 - codegen does an 8-byte transfer
             * regardless. AST_TYPE_ARRAY result: the parser produces
             * `matrix[i][j]` as `*(*(matrix+i)+j)` where the inner
             * `*x` has array type (decays to pointer). AST_TYPE_CLASS
             * result: `arr[i]` for an array of class produces a
             * struct-typed deref that the AST cancels via `&*x`
             * patterns (`&arr[i]`); the lowering returns the address
             * directly without loading struct bytes. */
            if (!ast->operand || !ast->operand->type) return 0;
            AstTypeKind ok = ast->operand->type->kind;
            if (ok != AST_TYPE_POINTER && ok != AST_TYPE_ARRAY) return 0;
            if (!ast->type) return 0;
            AstTypeKind tk = ast->type->kind;
            if (tk != AST_TYPE_INT && tk != AST_TYPE_CHAR &&
                tk != AST_TYPE_POINTER && tk != AST_TYPE_FLOAT &&
                tk != AST_TYPE_ARRAY && tk != AST_TYPE_CLASS) return 0;
            return irExprIsSliceEligible(ast->operand);
        }
        if (ast->unop == AST_UN_OP_ADDR_OF) {
            /* `&local` / `&param` / `&global` / `&fn`: produce a pointer
             * value. Function-address forms appear in `&Add` for
             * function-pointer assignments and DoMaths-style calls.
             * `&v->field` / `&cls.field` / `&v->a.b` produces the address
             * of a class field - used as a `Bool *` / `I64 *` out-param
             * (e.g. `CatLenPrint(buf, &js->len, ...)`); fold via the
             * existing AST_CLASS_REF eligibility chain. */
            if (!ast->operand) return 0;
            /* `&*x` (parser shape for `&arr[i]` etc.): cancels to the
             * pointer expression itself - eligible if the deref is. */
            if (ast->operand->kind == AST_UNOP &&
                ast->operand->unop == AST_UN_OP_DEREF) {
                return irExprIsSliceEligible(ast->operand);
            }
            return ast->operand->kind == AST_LVAR ||
                   ast->operand->kind == AST_GVAR ||
                   ast->operand->kind == AST_CLASS_REF ||
                   ast->operand->kind == AST_FUNC ||
                   ast->operand->kind == AST_FUN_PROTO ||
                   ast->operand->kind == AST_EXTERN_FUNC ||
                   ast->operand->kind == AST_ASM_FUNCDEF ||
                   ast->operand->kind == AST_ASM_FUNC_BIND;
        }
        if (ast->unop == AST_UN_OP_MINUS) {
            /* Negation: int (lower to IR_INEG) or F64 (lower to IR_FNEG). */
            if (!ast->operand) return 0;
            return irExprIsSliceEligible(ast->operand);
        }
        if (ast->unop == AST_UN_OP_PLUS) {
            if (!ast->operand) return 0;
            return irExprIsSliceEligible(ast->operand);
        }
        if (ast->unop == AST_UN_OP_LOG_NOT ||
            ast->unop == AST_UN_OP_BIT_NOT) {
            if (!ast->operand) return 0;
            return irExprIsSliceEligible(ast->operand);
        }
        return 0;
    case AST_FUNPTR_CALL: {
        /* Indirect call. ref must be a slot (LVAR / FUNPTR / GVAR) or a
         * field access (CLASS_REF) holding a function-pointer value.
         * Args follow the same constraints as a plain call. */
        if (!ast->ref) return 0;
        AstKind rk = ast->ref->kind;
        if (rk != AST_LVAR && rk != AST_FUNPTR && rk != AST_GVAR &&
            rk != AST_CLASS_REF) return 0;
    } /* falls through to FUNCALL arg checks */
    /* fallthrough */
    case AST_ASM_FUNCALL:
    case AST_FUNCALL: {
        /* Slice supports calls returning int, void, pointer, or F64.
         * Args go through one of three calling conventions:
         *   - SysV non-variadic: up to 6 int args (rdi..r9), 8 float
         *     (xmm0..7).
         *   - SysV extern variadic: same regs, plus %al = SSE count.
         *   - HolyC variadic: fixed params + 1 count arg in regs, then
         *     the variadic *values* on the stack. The codegen looks up
         *     the callee at emit time to figure out which to use. */
        if (!ast->type) return 0;
        AstTypeKind rk = ast->type->kind;
        if (rk != AST_TYPE_INT && rk != AST_TYPE_VOID &&
            rk != AST_TYPE_CHAR &&
            rk != AST_TYPE_POINTER && rk != AST_TYPE_FLOAT &&
            !(rk == AST_TYPE_CLASS && ast->type->is_intrinsic)) return 0;
        if (!ast->args) return 0;

        /* If the callee is HolyC-variadic, args at index >= var_arg_start
         * go on the stack - they don't compete for register slots. */
        int var_arg_start = -1;
        int extern_variadic = 0;
        if (s_elig_cc && ast->fname && ast->kind == AST_FUNCALL) {
            Ast *callee = mapGetLen(s_elig_cc->global_env,
                                    ast->fname->data, ast->fname->len);
            /* The parser drops has_var_args on extern decls (see
             * `astFunction(... ,0)` in parseExternFunc), so infer
             * variadic-ness from a trailing AST_VAR_ARGS param too. */
            int callee_va = 0;
            if (callee) {
                if ((callee->type && callee->type->has_var_args) ||
                    callee->has_var_args) {
                    callee_va = 1;
                } else if (callee->params && callee->params->size > 0) {
                    Ast *last = vecGet(Ast *, callee->params,
                                       callee->params->size - 1);
                    if (last && last->kind == AST_VAR_ARGS) callee_va = 1;
                }
            }
            if (callee && callee_va) {
                if (callee->kind == AST_EXTERN_FUNC) {
                    extern_variadic = 1;
                } else if (callee->params) {
                    for (u64 i = 0; i < callee->params->size; ++i) {
                        Ast *p = vecGet(Ast *, callee->params, i);
                        var_arg_start++;
                        if (p->kind == AST_VAR_ARGS) break;
                    }
                    var_arg_start += 1;
                }
            }
        }

        int int_count = 0, float_count = 0;
        for (u64 i = 0; i < ast->args->size; ++i) {
            Ast *arg = vecGet(Ast *, ast->args, i);
            if (!irArgIsSliceEligible(arg)) return 0;
            /* Skip stack-bound variadic positions for the reg-count check. */
            if (var_arg_start >= 0 && (s64)i >= var_arg_start) continue;
            if (arg->kind == AST_STRING) {
                int_count++;
            } else if (arg->type && arg->type->kind == AST_TYPE_FLOAT) {
                float_count++;
            } else {
                int_count++;
            }
        }
        /* SysV extern-variadic (printf/snprintf/...) overflow args spill
         * onto the stack; the slice cap of 6 ints / 8 floats only applies
         * to non-variadic and HolyC-variadic callees. */
        if (!extern_variadic && (int_count > 6 || float_count > 8)) return 0;
        return 1;
    }
    case AST_CLASS_REF: {
        /* `cls.field` (stack class) or `cls->field` (pointer-to-class).
         * Field type must be int / char / pointer / F64. The codegen uses
         * 8-byte transfers everywhere - that's correct for I64, pointer
         * and F64 fields and good enough for the existing tests on
         * narrower ints (their structs happen to be 8-byte-aligned).
         *
         * `cls` shapes the parser actually produces:
         *   stack:        AST_LVAR with class type
         *   ptr deref:    AST_UNOP DEREF whose operand is a pointer-typed
         *                 LVAR / GVAR / chained CLASS_REF - the parser
         *                 lowers `p->field` to `(*p).field` at parse time.
         */
        if (!ast->type) return 0;
        AstTypeKind ftk = ast->type->kind;
        /* AST_TYPE_FUNC: a function-pointer field (`t->fn` in fzf.HC).
         * Holds an 8-byte pointer at runtime; treat the same as POINTER.
         * AST_TYPE_ARRAY: an embedded fixed-size array field (e.g.
         * `sockaddr_un.sun_path[104]`). When read, decays to a pointer
         * to the first element (same shape as the addr-of-field path). */
        if (ftk != AST_TYPE_INT && ftk != AST_TYPE_CHAR &&
            ftk != AST_TYPE_POINTER && ftk != AST_TYPE_FLOAT &&
            ftk != AST_TYPE_FUNC && ftk != AST_TYPE_ARRAY) return 0;
        if (!ast->cls) return 0;
        /* Walk the CLASS_REF chain to its root. Intermediate CLASS_REFs
         * carry CLASS type (sub-struct view) which would fail the scalar
         * field-type check above, but they're never loaded as values,
         * only addressing steps; only the root needs checking. */
        Ast *cls = ast->cls;
        int _ignored_offset = 0;
        irCollapseClassRefChain(&cls, &_ignored_offset);
        if (!cls) return 0;
        if (cls->kind == AST_LVAR && cls->type &&
            (cls->type->kind == AST_TYPE_CLASS ||
             cls->type->kind == AST_TYPE_UNION)) return 1;
        if (cls->kind == AST_UNOP && cls->unop == AST_UN_OP_DEREF) {
            Ast *op = cls->operand;
            if (!op || !op->type) return 0;
            if (op->type->kind != AST_TYPE_POINTER) return 0;
            return irExprIsSliceEligible(op);
        }
        return 0;
    }
    default:
        return 0;
    }
}

static int irStmtIsSliceEligible(Ast *ast) {
    if (!ast) return 1;
    switch (ast->kind) {
    case AST_COMPOUND_STMT: {
        listForEach(ast->stms) {
            if (!irStmtIsSliceEligible((Ast *)it->value)) return 0;
        }
        return 1;
    }
    case AST_DECL: {
        if (!ast->declvar || !ast->declvar->type) return 0;
        if (ast->declvar->kind == AST_FUNPTR) {
            /* Function-pointer slot. Init is `&Identity` etc. */
            if (ast->declinit &&
                !irExprIsSliceEligible(ast->declinit)) return 0;
            return 1;
        }
        AstType *t = ast->declvar->type;
        if (t->kind == AST_TYPE_INT || t->kind == AST_TYPE_POINTER ||
            t->kind == AST_TYPE_FLOAT || t->kind == AST_TYPE_CHAR ||
            t->kind == AST_TYPE_FUNC ||
            (t->kind == AST_TYPE_CLASS && t->is_intrinsic)) {
            /* Intrinsic classes (`I64 class CDate`) are int-shaped at
             * the value level - accept the same scalar init forms.
             * AST_TYPE_FUNC: an `auto fp = &Add;` style decl whose
             * declvar is an AST_LVAR holding a function-pointer value
             * (8 bytes). */
            if (ast->declinit &&
                !irExprIsSliceEligible(ast->declinit)) return 0;
            return 1;
        }
        if (t->kind == AST_TYPE_CLASS || t->kind == AST_TYPE_UNION) {
            /* Stack-allocated class / union. The init must either be
             * absent or a struct-by-value FUNCALL (handled by the
             * hidden out-pointer convention). No aggregate init yet. */
            if (!ast->declinit) return 1;
            if ((ast->declinit->kind == AST_FUNCALL ||
                 ast->declinit->kind == AST_FUNPTR_CALL ||
                 ast->declinit->kind == AST_ASM_FUNCALL) &&
                irRetTypeIsAggregate(ast->declinit->type)) {
                return irExprIsSliceEligible(ast->declinit);
            }
            return 0;
        }
        if (t->kind == AST_TYPE_ARRAY) {
            /* `T arr[N];` (no init) or `T arr[N] = {1,2,3};` (incl.
             * 2D / nested initialisers). Walk the AST_ARRAY_INIT tree
             * and require each leaf scalar to be slice-eligible. */
            if (!ast->declinit) return 1;
            if (ast->declinit->kind != AST_ARRAY_INIT) return 0;
            return irArrayInitIsSliceEligible(ast->declinit);
        }
        return 0;
    }
    case AST_RETURN:
        if (!ast->retval) return 1;
        return irExprIsSliceEligible(ast->retval);
    case AST_IF:
        if (!irExprIsSliceEligible(ast->cond)) return 0;
        if (!irStmtIsSliceEligible(ast->then)) return 0;
        if (ast->els && !irStmtIsSliceEligible(ast->els)) return 0;
        return 1;
    case AST_FOR:
        if (ast->forinit && !irStmtIsSliceEligible(ast->forinit)) return 0;
        if (ast->forcond && !irExprIsSliceEligible(ast->forcond)) return 0;
        if (ast->forstep && !irStmtIsSliceEligible(ast->forstep)) return 0;
        if (ast->forbody && !irStmtIsSliceEligible(ast->forbody)) return 0;
        return 1;
    case AST_WHILE:
        if (!irExprIsSliceEligible(ast->whilecond)) return 0;
        if (ast->whilebody && !irStmtIsSliceEligible(ast->whilebody)) return 0;
        return 1;
    case AST_DO_WHILE:
        if (!irExprIsSliceEligible(ast->whilecond)) return 0;
        if (ast->whilebody && !irStmtIsSliceEligible(ast->whilebody)) return 0;
        return 1;
    case AST_BREAK:
    case AST_CONTINUE:
        /* Lowering enforces these appear inside a loop / switch. */
        return 1;
    case AST_SWITCH: {
        /* `switch (cond) { case ...: stmts; ... }`. Cond + every case
         * body must be eligible; AST_CASE / AST_DEFAULT are not exposed
         * at statement level outside the switch - their case_asts are
         * iterated directly in lowering. */
        if (!ast->switch_cond ||
            !irExprIsSliceEligible(ast->switch_cond)) return 0;
        if (ast->cases) {
            for (u64 i = 0; i < ast->cases->size; ++i) {
                Ast *_case = (Ast *)ast->cases->entries[i];
                if (!_case || !_case->case_asts) continue;
                listForEach(_case->case_asts) {
                    if (!irStmtIsSliceEligible((Ast *)it->value)) return 0;
                }
            }
        }
        if (ast->case_default && ast->case_default->case_asts) {
            listForEach(ast->case_default->case_asts) {
                if (!irStmtIsSliceEligible((Ast *)it->value)) return 0;
            }
        }
        return 1;
    }
    case AST_GOTO:
    case AST_LABEL:
        /* Lowering creates / consumes a per-function label→block map.
         * Unconditional control flow only - no fancy stuff. */
        return 1;
    case AST_BINOP:
        /* Statement-level: only assignment is meaningful */
        return irExprIsSliceEligible(ast);
    case AST_UNOP:
        /* Statement-level ++/-- on int locals. */
        return irExprIsSliceEligible(ast);
    case AST_FUNCALL:
    case AST_ASM_FUNCALL:
    case AST_FUNPTR_CALL:
        /* Discard return value. */
        return irExprIsSliceEligible(ast);
    default:
        return 0;
    }
}

int irFunctionEligibleForSliceCc(Cctrl *cc, Ast *ast_func) {
    s_elig_cc = cc;
    int r = irFunctionEligibleForSlice(ast_func);
    s_elig_cc = NULL;
    return r;
}

int irFunctionEligibleForSlice(Ast *ast_func) {
    if (!ast_func || ast_func->kind != AST_FUNC) return 0;
    if (ast_func->flags & AST_FLAG_INLINE) return 0;
    AstType *rettype = ast_func->type ? ast_func->type->rettype : NULL;
    if (!rettype) return 0;
    if (rettype->kind != AST_TYPE_VOID &&
        rettype->kind != AST_TYPE_INT &&
        rettype->kind != AST_TYPE_CHAR &&
        rettype->kind != AST_TYPE_POINTER &&
        rettype->kind != AST_TYPE_FLOAT &&
        !(rettype->kind == AST_TYPE_CLASS && rettype->is_intrinsic) &&
        !irRetTypeIsAggregate(rettype)) {
        return 0;
    }

    if (ast_func->params) {
        if (ast_func->params->size > 6) return 0;  /* SysV int-arg regs */
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            /* AST_DEFAULT_PARAM wraps an AST_LVAR with a default value;
             * the parser binds the call site so the body still references
             * the underlying lvar. We accept it the same as a plain LVAR. */
            if (p->kind == AST_DEFAULT_PARAM) {
                if (!p->declvar) return 0;
                if (p->declvar->kind != AST_LVAR &&
                    p->declvar->kind != AST_FUNPTR) return 0;
                p = p->declvar;
            }
            /* AST_FUNPTR params hold a runtime pointer (8 bytes); behave
             * just like an int/pointer param for slot allocation. */
            if (p->kind == AST_FUNPTR) continue;
            /* AST_VAR_ARGS: trailing `...` in the signature. Carries an
             * `argc` (count) and `argv` (pointer to caller-pushed args)
             * lvar; we materialize both as regular slots in the prologue
             * and the body uses them as plain locals. */
            if (p->kind == AST_VAR_ARGS) {
                if (!p->argc || !p->argv) return 0;
                continue;
            }
            if (p->kind != AST_LVAR) return 0;
            if (!p->type) return 0;
            if (p->type->kind == AST_TYPE_INT) continue;
            if (p->type->kind == AST_TYPE_CHAR) continue;
            if (p->type->kind == AST_TYPE_POINTER) continue;
            if (p->type->kind == AST_TYPE_FLOAT) continue;
            /* HolyC `public I64 class CDate { ... }` style: a class
             * declared with an integer alias is `is_intrinsic=1` and
             * passed/returned in a single 8-byte int register, just
             * like an I64. The body still accesses fields via the
             * class layout. */
            if (p->type->kind == AST_TYPE_CLASS &&
                p->type->is_intrinsic) continue;
            return 0;
        }
    }

    /* Reject when the parser inlined a call into the body. Inlining copies
     * the inlinee's locals (with their original lvar_ids) into the caller's
     * locals list, but the caller's own decls are numbered from a counter
     * that resets per function - so callee/caller ids collide. The flat
     * lvar_id -> IrValue map can't disambiguate, so fall back to AST. */
    Set *seen_ids = setNew(8, &set_uint_type);
    int dup = 0;
    listForEach(ast_func->locals) {
        Ast *l = irUnwrapDefaultParam((Ast *)it->value);
        u32 id = l->lvar_id;
        if (l->kind == AST_FUNPTR) {
            id = l->fn_ptr_id;
        } else if (l->kind != AST_LVAR) {
            dup = 1; break;
        }
        if (setHas(seen_ids, (void *)(u64)id)) { dup = 1; break; }
        setAdd(seen_ids, (void *)(u64)id);
        if (l->kind == AST_FUNPTR) continue;
        if (!l->type) { dup = 1; break; }
        if (l->type->kind == AST_TYPE_INT) continue;
        /* Stack-allocated class with int-only fields. We don't validate
         * fields here; AST_CLASS_REF accesses are checked against
         * irTypeIsSliceInt at use-site, so non-int fields just mean the
         * function loses eligibility through some specific access. */
        if (l->type->kind == AST_TYPE_CLASS) continue;
        if (l->type->kind == AST_TYPE_POINTER) continue;
        if (l->type->kind == AST_TYPE_FLOAT) continue;
        if (l->type->kind == AST_TYPE_CHAR) continue;
        if (l->type->kind == AST_TYPE_ARRAY) continue;
        /* `auto fp = &Add;` / `U0 *(*fp)(...)`: function-pointer locals
         * may carry AST_TYPE_FUNC at the value level - 8 bytes, treated
         * like a pointer slot. */
        if (l->type->kind == AST_TYPE_FUNC) continue;
        /* Intrinsic class (`I64 class CDate`): int-shaped 8 bytes. */
        if (l->type->kind == AST_TYPE_CLASS && l->type->is_intrinsic)
            continue;
        /* Local unions are stack-allocated like a class - just bytes
         * and field accesses. The slot allocator and class-ref code
         * already handle them; we just need to accept the kind here. */
        if (l->type->kind == AST_TYPE_UNION) continue;
        dup = 1; break;
    }
    setRelease(seen_ids);
    if (dup) return 0;

    return irStmtIsSliceEligible(ast_func->body);
}

/* Lower an AST_FUNCALL / AST_FUNPTR_CALL / AST_ASM_FUNCALL.
 *
 * `preallocated_out_buf`, if non-NULL, is a pointer-typed IrValue used
 * as the hidden out-pointer for a struct-by-value return. The caller
 * (typically AST_DECL for a class-typed local) uses this to route the
 * call's struct result directly into the local's slot, avoiding a
 * temp-buffer-plus-memcpy round-trip (the temp and the local would
 * overlap if the layout pass placed them adjacently). */
IrValue *irFnCallTo(IrCtx *ctx, Ast *ast, IrValue *preallocated_out_buf) {
    /* Struct-by-value return: the call's nominal return type is a class
     * but the runtime convention is "buffer pointer in rax". Make the
     * IR_CALL produce a ptr-typed result and prepend `&buffer` as a
     * hidden first arg below. */
    int agg_return = irRetTypeIsAggregate(ast->type);
    IrValueType ret_type = agg_return
        ? IR_TYPE_PTR : irConvertType(ast->type);
    int ret_size = agg_return ? 8 : ast->type->size;
    IrValue *ir_call_args = irValueNew(IR_TYPE_ARRAY, IR_VAL_UNRESOLVED);
    IrValue *ir_ret_val = irTmp(ret_type, ret_size);
    IrInstr *ir_call_instr = irInstrNew(IR_CALL, ir_ret_val, ir_call_args, NULL);

    assert(ast->kind == AST_FUNCALL || ast->kind == AST_FUNPTR_CALL ||
           ast->kind == AST_ASM_FUNCALL);

    Vec *args = irValueVecNew();
    ir_call_args->as.array.values = args;
    /* Push the hidden out-buffer pointer as args[0]. Use the caller's
     * pre-allocated buffer if provided; otherwise allocate a fresh
     * temp slot up-front (so user-arg evaluation can't clobber it). */
    if (agg_return) {
        IrValue *buf_addr;
        if (preallocated_out_buf) {
            buf_addr = preallocated_out_buf;
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
    /* Direct call: store the static function name on the wrapper. Indirect
     * call: leave label NULL and stash the function-pointer source in r2 so
     * the codegen knows to load it into a register and emit `call *<reg>`. */
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
                 * default expression on `default_fn->declinit`. Matches
                 * what asmPrepFuncCallArgs picks up. */
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

IrValue *irFnCall(IrCtx *ctx, Ast *ast) {
    return irFnCallTo(ctx, ast, NULL);
}

/* Binary expressions are assumed to always be assigning to something. I'm not
 * 100% sure this is a valid assumption to make. Well I guess;
 * `I64 x = y + 32 * 10` _could_ continually be assigned to `x` */
IrValue *irBinOpExpr(IrCtx *ctx, Ast *ast) {
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
     * branch on it; evaluate right only in the "right" block. We mustn't
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

    /* Eager-eval lhs/rhs for everything else. The arith/cmp dispatch
     * below assumes both operand IR values are already produced. */
    IrValue *lhs = irExpr(ctx, ast->left);
    IrValue *rhs = irExpr(ctx, ast->right);
    IrValueType ir_type = irConvertType(ast->type);
    IrValue *ir_result = irTmp(ir_type, ast->type->size);
    IrOp op;
    IrCmpKind cmp = IR_CMP_INVALID;

    /* Pointer arithmetic: if the result is pointer/array, scale the int
     * operand by sizeof(element). Matches asmPointerArithmetic. */
    if ((ast->type->kind == AST_TYPE_POINTER ||
         ast->type->kind == AST_TYPE_ARRAY) &&
        (ast->binop == AST_BIN_OP_ADD || ast->binop == AST_BIN_OP_SUB)) {
        AstType *elem = ast->type->ptr;
        int scale = elem ? elem->size : 1;
        if (scale < 1) scale = 1;
        /* Figure out which operand is the pointer/array (the "base") and
         * which is the int (the "index"). One of them must be ptr/array;
         * the other gets scaled. */
        int left_is_ptr = ast->left->type &&
            (ast->left->type->kind == AST_TYPE_POINTER ||
             ast->left->type->kind == AST_TYPE_ARRAY);
        IrValue *base = left_is_ptr ? lhs : rhs;
        IrValue *idx  = left_is_ptr ? rhs : lhs;
        if (scale != 1) {
            IrValue *scaled = irTmp(IR_TYPE_I64, 8);
            IrValue *k = irConstInt(IR_TYPE_I64, scale);
            irBlockAddInstr(ctx,
                irInstrNew(IR_IMUL, scaled, idx, k));
            idx = scaled;
        }
        IrOp pop = (ast->binop == AST_BIN_OP_ADD) ? IR_IADD : IR_ISUB;
        irBlockAddInstr(ctx, irInstrNew(pop, ir_result, base, idx));
        return ir_result;
    }

    /* Dispatch:
     *   - Arith (+ - * /): result is the AST type. F64 -> FADD/etc; int ->
     *     IADD/etc.
     *   - Comparison (==, <, etc): only use FCMP when BOTH operands are
     *     AST_TYPE_FLOAT, matching HolyC's AST codegen. Mixed-type compares
     *     truncate the float side via FPTOSI and dispatch through ICMP. */
    int is_cmp = ast->binop == AST_BIN_OP_EQ || ast->binop == AST_BIN_OP_NE ||
                 ast->binop == AST_BIN_OP_LT || ast->binop == AST_BIN_OP_LE ||
                 ast->binop == AST_BIN_OP_GT || ast->binop == AST_BIN_OP_GE;
    int left_is_float = ast->left && ast->left->type &&
                        ast->left->type->kind == AST_TYPE_FLOAT;
    int right_is_float = ast->right && ast->right->type &&
                         ast->right->type->kind == AST_TYPE_FLOAT;
    int float_dispatch;
    if (is_cmp) {
        float_dispatch = left_is_float && right_is_float;
        /* Mixed-type compare: truncate the float operand to int so ICMP
         * sees two integers, matching HolyC AST codegen. */
        if (!float_dispatch && (left_is_float || right_is_float)) {
            IrValue *trunc_dst = irTmp(IR_TYPE_I64, 8);
            if (left_is_float) {
                irBlockAddInstr(ctx,
                    irInstrNew(IR_FPTOSI, trunc_dst, lhs, NULL));
                lhs = trunc_dst;
            } else {
                irBlockAddInstr(ctx,
                    irInstrNew(IR_FPTOSI, trunc_dst, rhs, NULL));
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
    /* FCMP returns 0/1, not a float. The HolyC parser sometimes mislabels
     * `f1 < f2` as F64-typed; correct the result tmp here so downstream
     * loads/stores treat the comparison result as an integer. */
    if (op == IR_FCMP) {
        ir_result = irTmp(IR_TYPE_I64, 8);
    }
    IrInstr *instr = irInstrNew(op, ir_result, lhs, rhs);
    instr->extra.cmp_kind = cmp;
    irBlockAddInstr(ctx, instr);
    return ir_result;
}

static IrValue *irLowerAssign(IrCtx *ctx, Ast *ast);

/* Build an IrValue referring to an AST_GVAR symbol. The name we emit in
 * the asm is the static label for static globals or the public gname
 * otherwise - matches what asmGetGlabel() picks for the AST codegen. */
static IrValue *irGlobalValue(Ast *gvar_ast) {
    IrValue *v = irValueNew(IR_TYPE_PTR, IR_VAL_GLOBAL);
    v->as.global.name = (gvar_ast->is_static && gvar_ast->glabel)
                        ? gvar_ast->glabel
                        : gvar_ast->gname;
    v->as.global.value = NULL;
    return v;
}

/* Lower a value of `from_type` (already in `src`) to `to_type`. Returns the
 * IrValue holding the converted result, or `src` unchanged when no
 * conversion is needed. Handles int<->float and pointer<->int. */
static IrValue *irLowerCast(IrCtx *ctx, IrValue *src,
                            AstType *from_type, AstType *to_type) {
    if (!from_type || !to_type) return src;
    /* Intrinsic classes (`I64 class CDate`) are int-shaped at the value
     * level, casting in/out is a pass-through. */
    int from_int = from_type->kind == AST_TYPE_INT ||
                   from_type->kind == AST_TYPE_CHAR ||
                   from_type->kind == AST_TYPE_POINTER ||
                   (from_type->kind == AST_TYPE_CLASS &&
                    from_type->is_intrinsic);
    int to_int = to_type->kind == AST_TYPE_INT ||
                 to_type->kind == AST_TYPE_CHAR ||
                 to_type->kind == AST_TYPE_POINTER ||
                 (to_type->kind == AST_TYPE_CLASS &&
                  to_type->is_intrinsic);
    int from_float = from_type->kind == AST_TYPE_FLOAT;
    int to_float = to_type->kind == AST_TYPE_FLOAT;

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

IrValue *irExpr(IrCtx *ctx, Ast *ast) {
    switch (ast->kind) {
        case AST_BINOP:
            /* Assignments (= and compound op=) lower to a store, not a
             * binop in our IR. Route through the dedicated lowering so
             * nested forms like `y = (x += 5)` don't fall into the
             * int-binop "op not handled" panic. */
            if (ast->binop == AST_BIN_OP_ASSIGN ||
                irBinOpIsCompoundAssign(ast->binop)) {
                return irLowerAssign(ctx, ast);
            }
            return irBinOpExpr(ctx, ast);
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
            /* In a function body, the parser emits AST_DEFAULT_PARAM as a
             * wrapper around the underlying lvar reference. Resolve to the
             * lvar value; the default-init only matters at the call site. */
            return irExpr(ctx, ast->declvar);

        case AST_LVAR: {
            IrValue *local_var = irFnGetVar(ctx->cur_func, ast->lvar_id);
            if (!local_var) {
                loggerPanic("Variable %s not found\n", astToString(ast));
            }

            /* Array LVARs decay to a pointer to their first element. We
             * emit an LEA on the slot rather than loading its bytes. */
            if (ast->type->kind == AST_TYPE_ARRAY) {
                IrValue *addr = irTmp(IR_TYPE_PTR, 8);
                irBlockAddInstr(ctx,
                    irInstrNew(IR_LEA, addr, local_var, NULL));
                return addr;
            }

            /* Intrinsic class (`I64 class CDate`) is int-shaped: load
             * 8 bytes via the regular int path, ignore the AST struct
             * size (the AST tags it as 16). */
            int is_intrinsic_class =
                ast->type->kind == AST_TYPE_CLASS && ast->type->is_intrinsic;
            /* Non-intrinsic class / union read as a value: produce the
             * lvar's address (LEA on the slot). The hidden out-pointer
             * struct-return path and AST_RETURN-of-class handler both
             * consume the value as `&lvar`; there is no general
             * "load a struct into a register" lowering. */
            int is_aggregate =
                !is_intrinsic_class &&
                (ast->type->kind == AST_TYPE_CLASS ||
                 ast->type->kind == AST_TYPE_UNION);
            if (is_aggregate) {
                IrValue *addr = irTmp(IR_TYPE_PTR, 8);
                irBlockAddInstr(ctx,
                    irInstrNew(IR_LEA, addr, local_var, NULL));
                return addr;
            }
            IrValueType ir_value_type = is_intrinsic_class
                ? IR_TYPE_I64 : irConvertType(ast->type);
            int load_size = is_intrinsic_class ? 8 : ast->type->size;
            IrValue *ir_load_dest = irTmp(ir_value_type, load_size);
            IrInstr *load_instr = irLoad(ir_load_dest, local_var);
            irBlockAddInstr(ctx, load_instr);
            return ir_load_dest;
        }

        case AST_GVAR: {
            /* Global int read: lea its address, then load*. Arrays
             * decay to a pointer to the first element - just return
             * the LEA'd address with no load. */
            IrValue *gv = irGlobalValue(ast);
            IrValue *addr = irTmp(IR_TYPE_PTR, 8);
            irBlockAddInstr(ctx, irInstrNew(IR_LEA, addr, gv, NULL));
            if (ast->type->kind == AST_TYPE_ARRAY) {
                return addr;
            }
            IrValueType ir_type = irConvertType(ast->type);
            IrValue *load_dst = irTmp(ir_type, ast->type->size);
            irBlockAddInstr(ctx,
                irInstrNew(IR_LOAD_DEREF, load_dst, addr, NULL));
            return load_dst;
        }

        case AST_FUNCALL:
        case AST_ASM_FUNCALL:
            return irFnCall(ctx, ast);

        case AST_FUNPTR_CALL:
            return irFnCall(ctx, ast);

        case AST_FUNPTR: {
            /* Reading a function-pointer slot: load the 8-byte pointer. */
            IrValue *slot = irFnGetVar(ctx->cur_func, ast->fn_ptr_id);
            if (!slot) loggerPanic("AST_FUNPTR with no slot\n");
            IrValue *dst = irTmp(IR_TYPE_PTR, 8);
            irBlockAddInstr(ctx, irLoad(dst, slot));
            return dst;
        }

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

        case AST_CLASS_REF: {
            /* Two shapes:
             *   stack class: cls is AST_LVAR with class type. Field is at
             *                a known offset within the local's stack
             *                frame; IR_GEP aliases that loff.
             *   pointer:     cls is AST_UNOP DEREF (the parser lowers
             *                `p->field` to `(*p).field`); operand is a
             *                pointer-typed expression. Evaluate it, add
             *                the field offset, then deref. */
            Ast *cls = ast->cls;
            int field_is_array = ast->type->kind == AST_TYPE_ARRAY;
            IrValueType field_ir_type = field_is_array
                ? IR_TYPE_PTR : irConvertType(ast->type);
            IrValue *load_dst = irTmp(field_ir_type,
                                      field_is_array ? 8 : ast->type->size);
            int offset = ast->type->offset;

            /* Nested CLASS_REF: collapse the chain so we end up with a
             * root (AST_LVAR or AST_UNOP DEREF) and a single combined
             * offset. The leaf addressing then sees one offset. */
            irCollapseClassRefChain(&cls, &offset);

            if (cls && cls->kind == AST_UNOP && cls->unop == AST_UN_OP_DEREF) {
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
                irBlockAddInstr(ctx,
                    irInstrNew(IR_LOAD_DEREF, load_dst, addr, NULL));
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
            irBlockAddInstr(ctx,
                irInstrNew(IR_GEP, field_addr, base, off));
            if (field_is_array) {
                /* `cls.arr` decays to the field's address - but the GEP
                 * tmp aliases the stack slot rather than holding a
                 * runtime pointer, so emit an explicit IR_LEA to
                 * materialize the pointer value. */
                IrValue *dst = irTmp(IR_TYPE_PTR, 8);
                irBlockAddInstr(ctx,
                    irInstrNew(IR_LEA, dst, field_addr, NULL));
                return dst;
            }
            irBlockAddInstr(ctx, irLoad(load_dst, field_addr));
            return load_dst;
        }

        case AST_UNOP: {
            if (ast->unop == AST_UN_OP_DEREF) {
                /* `*p`: evaluate the pointer expression, then load through
                 * it.
                 *
                 * HolyC quirk: `*pp` where the result is pointer-to-scalar
                 * (int / pointer / F64) is a no-op in the AST codegen
                 * (`leaq (%rax), %rax`). Match it by returning the operand
                 * pointer directly. This is what makes `&arr[i]` (parsed
                 * as `*((&arr) + i)`) yield a real pointer rather than
                 * dereffing through the slot. */
                IrValue *ptr = irExpr(ctx, ast->operand);
                if (ast->type->kind == AST_TYPE_POINTER && ast->type->ptr) {
                    AstTypeKind pk = ast->type->ptr->kind;
                    if (pk == AST_TYPE_INT || pk == AST_TYPE_POINTER ||
                        pk == AST_TYPE_FLOAT) {
                        return ptr;
                    }
                }
                /* `*p` where the result type is itself an array
                 * (multi-dim indexing intermediates, e.g. the inner
                 * `*matrix` in `matrix[i][j]`): array decays to its
                 * pointer, no real load. Same for class result -
                 * the parent expression is `&*x` (`&arr[i]`) which
                 * cancels out. */
                if (ast->type->kind == AST_TYPE_ARRAY ||
                    ast->type->kind == AST_TYPE_CLASS) return ptr;
                IrValueType ir_type = irConvertType(ast->type);
                IrValue *load_dst = irTmp(ir_type, ast->type->size);
                irBlockAddInstr(ctx,
                    irInstrNew(IR_LOAD_DEREF, load_dst, ptr, NULL));
                return load_dst;
            }
            if (ast->unop == AST_UN_OP_ADDR_OF) {
                /* `&lvar` / `&gvar` / `&fn`: produce a pointer value.
                 *
                 * HolyC quirk (matches asmAddr): when the operand is a
                 * pointer-typed LVAR pointing at a SCALAR (int/F64/ptr -
                 * NOT array/char/class), `&lvar` returns the pointer's
                 * VALUE rather than the slot address. So `&arr[2]` for
                 * `I64 *arr;` is `arr + 2` (arr's value plus 2*sizeof). */
                Ast *operand = ast->operand;
                /* `&*x` cancels to x. Used by the parser for `&arr[i]`
                 * which is `&*(arr+i)`. */
                if (operand->kind == AST_UNOP &&
                    operand->unop == AST_UN_OP_DEREF) {
                    return irExpr(ctx, operand->operand);
                }
                IrValue *src = NULL;
                if (operand->kind == AST_LVAR) {
                    src = irFnGetVar(ctx->cur_func, operand->lvar_id);
                    if (!src) loggerPanic("&lvar on unknown lvar\n");
                    AstType *ot = operand->type;
                    if (ot && ot->kind == AST_TYPE_POINTER && ot->ptr) {
                        AstTypeKind pk = ot->ptr->kind;
                        if (pk != AST_TYPE_ARRAY && pk != AST_TYPE_CHAR &&
                            pk != AST_TYPE_CLASS) {
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
                           operand->kind == AST_EXTERN_FUNC) {
                    /* Address of a function: emit `leaq <fname>(%rip), rax`
                     * via the global-value mechanism. The codegen runs
                     * asmNormaliseFunctionName on the label. */
                    src = irValueNew(IR_TYPE_PTR, IR_VAL_GLOBAL);
                    src->as.global.name = operand->fname;
                    src->as.global.value = NULL;
                    src->flags |= IR_VAL_FLAG_FUNC;
                } else if (operand->kind == AST_ASM_FUNCDEF ||
                           operand->kind == AST_ASM_FUNC_BIND) {
                    /* Asm-bound function - use the raw asm name, no
                     * normalisation. */
                    src = irValueNew(IR_TYPE_PTR, IR_VAL_GLOBAL);
                    src->as.global.name = operand->asmfname;
                    src->as.global.value = NULL;
                } else if (operand->kind == AST_CLASS_REF) {
                    /* `&v->field` / `&cls.field` / `&v->a.b`: same
                     * address calculation as a write target - reuse
                     * irLowerAssignTarget. The result is the field's
                     * pointer, no LEA needed (target.target is already
                     * a pointer when indirect, or a slot-aliased tmp
                     * via IR_GEP when direct). */
                    IrAssignTarget tgt = irLowerAssignTarget(ctx, operand);
                    if (tgt.indirect) {
                        return tgt.target;
                    }
                    /* Direct (stack class field): tgt.target is a tmp
                     * aliased to base+offset via IR_GEP, used as a slot
                     * elsewhere - but here we want its value (the
                     * pointer to that slot) so emit an IR_LEA over it. */
                    IrValue *dst = irTmp(IR_TYPE_PTR, 8);
                    irBlockAddInstr(ctx,
                        irInstrNew(IR_LEA, dst, tgt.target, NULL));
                    return dst;
                } else {
                    loggerPanic("&%s not supported in slice\n",
                                astKindToString(operand->kind));
                }
                IrValue *dst = irTmp(IR_TYPE_PTR, 8);
                irBlockAddInstr(ctx, irInstrNew(IR_LEA, dst, src, NULL));
                return dst;
            }
            if (ast->unop == AST_UN_OP_PLUS) {
                /* Identity for arithmetic types - just evaluate operand. */
                return irExpr(ctx, ast->operand);
            }
            if (ast->unop == AST_UN_OP_MINUS) {
                IrValue *v = irExpr(ctx, ast->operand);
                IrValueType vt = v->type;
                IrValue *dst = irTmp(vt,
                                     irIsFloat(vt) ? 8
                                                   : ast->operand->type->size);
                IrOp negop = irIsFloat(vt) ? IR_FNEG : IR_INEG;
                irBlockAddInstr(ctx, irInstrNew(negop, dst, v, NULL));
                return dst;
            }
            if (ast->unop == AST_UN_OP_BIT_NOT) {
                /* Bitwise NOT on int. Lowers to `dst = ~v` via IR_NOT. */
                IrValue *v = irExpr(ctx, ast->operand);
                IrValue *dst = irTmp(v->type, 8);
                irBlockAddInstr(ctx, irInstrNew(IR_NOT, dst, v, NULL));
                return dst;
            }
            if (ast->unop == AST_UN_OP_LOG_NOT) {
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
                ast->unop != AST_UN_OP_POST_DEC) {
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
                       (operand->kind == AST_UNOP &&
                        operand->unop == AST_UN_OP_DEREF)) {
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

            int is_inc = (ast->unop == AST_UN_OP_PRE_INC ||
                          ast->unop == AST_UN_OP_POST_INC);
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

            int is_post = (ast->unop == AST_UN_OP_POST_INC ||
                           ast->unop == AST_UN_OP_POST_DEC);
            return is_post ? cur : new_val;
        }

        case AST_GOTO:
        case AST_LABEL:
        case AST_FUNC:
        case AST_DECL:
        case AST_ARRAY_INIT:
        case AST_IF:
        case AST_FOR:
        case AST_RETURN:
        case AST_WHILE:
        case AST_COMPOUND_STMT:
        case AST_ASM_STMT:
        case AST_ASM_FUNC_BIND:
        case AST_BREAK:
        case AST_CONTINUE:
        case AST_VAR_ARGS:
        case AST_ASM_FUNCDEF:
        case AST_FUN_PROTO:
        case AST_CASE:
        case AST_JUMP:
        case AST_EXTERN_FUNC:
        case AST_DO_WHILE:
        case AST_PLACEHOLDER:
        case AST_SWITCH:
        case AST_DEFAULT:
        case AST_SIZEOF:
        case AST_COMMENT:
        default:
            loggerPanic("Expr Unhandled AST kind: %s\n", astKindToString(ast->kind));
    }
}

/* Map a compound-assign AstBinOp (e.g. AST_BIN_OP_ADD_ASSIGN) to the IR
 * arithmetic op that implements its `op` half. Signedness comes from the
 * lvar's type for div/mod where it matters. */
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

        if (cls && cls->kind == AST_UNOP && cls->unop == AST_UN_OP_DEREF) {
            /* Pointer-to-class field write: evaluate the pointer, add the
             * field's offset, store via STORE_DEREF. */
            IrValue *ptr_val = irExpr(ctx, cls->operand);
            IrValue *addr = ptr_val;
            if (offset != 0) {
                addr = irTmp(IR_TYPE_PTR, 8);
                IrValue *off_const = irConstInt(IR_TYPE_I64, offset);
                irBlockAddInstr(ctx,
                    irInstrNew(IR_IADD, addr, ptr_val, off_const));
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

    if (lhs->kind == AST_UNOP && lhs->unop == AST_UN_OP_DEREF) {
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
    if (ast->binop == AST_BIN_OP_ASSIGN) {
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
static int irLowerArrayInitWalk(IrCtx *ctx, IrValue *base,
                                int offset_bytes, AstType *target_ty,
                                Ast *init) {
    if (!init || init->kind != AST_ARRAY_INIT) return offset_bytes;
    if (!init->arrayinit) return offset_bytes;
    AstType *elem_ty = target_ty ? target_ty->ptr : NULL;
    int parent_is_array = target_ty &&
                          target_ty->kind == AST_TYPE_ARRAY;
    listForEach(init->arrayinit) {
        Ast *item = (Ast *)it->value;
        if (item->kind == AST_ARRAY_INIT) {
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
            int sz = (item->kind == AST_STRING)
                ? 8 : (item->type ? item->type->size : 8);
            offset_bytes += sz;
        }
    }
    return offset_bytes;
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

        case AST_LVAR:
            /* Bare lvalue at statement level - no side effect. */
            break;

        case AST_BINOP: {
            if (ast->binop == AST_BIN_OP_ASSIGN ||
                irBinOpIsCompoundAssign(ast->binop)) {
                irLowerAssign(ctx, ast);
            } else {
                /* No-op statement; keep evaluation in case of hidden effects. */
                irExpr(ctx, ast);
            }
            break;
        }

        case AST_RETURN: {
            if (ast->retval && ctx->cur_func->return_value) {
                /* Struct-by-value return: copy the retval's bytes into
                 * the hidden out-pointer's destination. irExpr on a
                 * class-typed AST_LVAR produces the lvar's address; the
                 * out_ptr param's slot holds the destination address. */
                AstType *rt = ast->retval->type;
                if (rt && irRetTypeIsAggregate(rt)) {
                    IrValue *src_addr = irExpr(ctx, ast->retval);
                    IrValue *dst_addr = irTmp(IR_TYPE_PTR, 8);
                    irBlockAddInstr(ctx,
                        irLoad(dst_addr, ctx->cur_func->return_value));
                    irEmitInlineMemcpy(ctx, dst_addr, src_addr, rt->size);
                } else {
                    IrValue *val = irExpr(ctx, ast->retval);
                    IrInstr *st = irStore(ctx->cur_func->return_value,
                                          val);
                    irBlockAddInstr(ctx, st);
                }
            }
            IrInstr *jmp = irJump(ctx->cur_func, ctx->cur_block,
                                  ctx->cur_func->exit_block);
            irBlockAddInstr(ctx, jmp);
            break;
        }

        case AST_IF: {
            IrValue *cond_val = irExpr(ctx, ast->cond);
            IrBlock *then_block = irBlockNew();
            IrBlock *else_block = ast->els ? irBlockNew() : NULL;
            IrBlock *join_block = irBlockNew();
            IrBlock *false_target = else_block ? else_block : join_block;

            irBranch(ctx->cur_func, ctx->cur_block, cond_val,
                     then_block, false_target);

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
            break;
        }

        case AST_UNOP: {
            /* Bare ++/-- as a statement: lower as expression and discard. */
            irExpr(ctx, ast);
            break;
        }

        case AST_FOR: {
            /* `for (init; cond; step) body` ->
             *   pre-header: init ; jmp header
             *   header: cond ; br body, end
             *   body:   <body>     ; jmp step    (continue: jmp step)
             *   step:   <step>     ; jmp header  (back-edge)
             *   end:    ...                       (break: jmp end) */
            if (!ast->forbody) break;
            if (ast->forinit) irLowerAst(ctx, ast->forinit);
            if (ctx->cur_block->sealed) break;

            IrBlock *header = irBlockNew();
            IrBlock *body   = irBlockNew();
            IrBlock *step   = irBlockNew();
            IrBlock *end    = irBlockNew();

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
            if (ast->forstep) irLowerAst(ctx, ast->forstep);
            if (!ctx->cur_block->sealed) {
                IrInstr *j = irJump(ctx->cur_func, ctx->cur_block, header);
                irBlockAddInstr(ctx, j);
            }

            irPopLoopCtx(ctx);

            irFnAddBlock(ctx->cur_func, end);
            ctx->cur_block = end;
            break;
        }

        case AST_WHILE: {
            if (!ast->whilecond) break;
            IrBlock *header = irBlockNew();
            IrBlock *body   = irBlockNew();
            IrBlock *end    = irBlockNew();

            IrInstr *to_header = irJump(ctx->cur_func, ctx->cur_block, header);
            irBlockAddInstr(ctx, to_header);

            irFnAddBlock(ctx->cur_func, header);
            ctx->cur_block = header;
            IrValue *cond_val = irExpr(ctx, ast->whilecond);
            irBranch(ctx->cur_func, ctx->cur_block, cond_val, body, end);

            irPushLoopCtx(ctx, header, end);

            irFnAddBlock(ctx->cur_func, body);
            ctx->cur_block = body;
            if (ast->whilebody) irLowerAst(ctx, ast->whilebody);
            if (!ctx->cur_block->sealed) {
                IrInstr *j = irJump(ctx->cur_func, ctx->cur_block, header);
                irBlockAddInstr(ctx, j);
            }

            irPopLoopCtx(ctx);

            irFnAddBlock(ctx->cur_func, end);
            ctx->cur_block = end;
            break;
        }

        case AST_DO_WHILE: {
            /* Cond runs at end of body, but we put it in its own block so
             * `continue` has a target. */
            if (!ast->whilecond) break;
            IrBlock *body = irBlockNew();
            IrBlock *cond = irBlockNew();
            IrBlock *end  = irBlockNew();

            IrInstr *to_body = irJump(ctx->cur_func, ctx->cur_block, body);
            irBlockAddInstr(ctx, to_body);

            irPushLoopCtx(ctx, cond, end);

            irFnAddBlock(ctx->cur_func, body);
            ctx->cur_block = body;
            if (ast->whilebody) irLowerAst(ctx, ast->whilebody);
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
                        irLowerArrayInitWalk(ctx, local, 0, var->type,
                                             init);
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
                            irBlockAddInstr(ctx,
                                irInstrNew(IR_LEA, out, local, NULL));
                            irFnCallTo(ctx, init, out);
                        } else {
                            IrValue *ret = irFnCall(ctx, init);
                            IrInstr *ir_store = irStore(local, ret);
                            irBlockAddInstr(ctx, ir_store);
                        }
                        break;
                    }

                    default: {
                        ir_init = irExpr(ctx, init);
                        ir_init = irNarrowToTargetWidth(ctx, ir_init,
                                                       var->type);
                        IrInstr *ir_store = irStore(local, ir_init);
                        irBlockAddInstr(ctx, ir_store);
                        break;
                    }
                }
            }
            break;
        }

        case AST_FUNCALL:
        case AST_ASM_FUNCALL:
        case AST_FUNPTR_CALL:
            irExpr(ctx, ast);
            break;

        case AST_STRING:
            irExpr(ctx, ast);
            break;

        case AST_GVAR:
            /* Bare global at statement level - no side effect. */
            break;

        case AST_GOTO: {
            /* Look up or lazily create the target block by label name.
             * Forward gotos hit this branch first; the matching AST_LABEL
             * later finds the same block in the map. */
            AoStr *lname = ast->slabel;
            if (!lname) loggerPanic("AST_GOTO with no label\n");
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
            break;
        }

        case AST_LABEL: {
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
            break;
        }

        case AST_SWITCH: {
            /* Lower as an if-else chain: evaluate cond once, then for
             * each case test the range and branch to that case's body
             * block; on no-match fall through to the next test. After
             * all tests, branch to the default body (or end if no
             * default). Bodies fall through to the next case body
             * unless they end in break/return/goto. break inside the
             * switch jumps to end_block (pushed via irPushLoopCtx with
             * NULL continue, since switch has no continue). */
            IrValue *cond = irExpr(ctx, ast->switch_cond);
            int n = ast->cases ? (int)ast->cases->size : 0;

            IrBlock *end_block = irBlockNew();
            IrBlock *default_block = ast->case_default
                ? irBlockNew() : end_block;

            IrBlock **body_blocks = NULL;
            if (n > 0) {
                body_blocks = (IrBlock **)calloc(n, sizeof(IrBlock *));
                for (int i = 0; i < n; i++) body_blocks[i] = irBlockNew();
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
                    IrValue *k = irConstInt(IR_TYPE_I64,
                                            _case->case_begin);
                    IrInstr *cmp_instr = irInstrNew(IR_ICMP, cmp,
                                                    cond, k);
                    cmp_instr->extra.cmp_kind = IR_CMP_EQ;
                    irBlockAddInstr(ctx, cmp_instr);
                    irBranch(ctx->cur_func, ctx->cur_block, cmp,
                             body, next_test);
                } else {
                    /* Range case `case lo...hi:` -> two compares
                     * chained with a transient block. */
                    IrBlock *upper = irBlockNew();
                    IrValue *ge = irTmp(IR_TYPE_I8, 1);
                    IrValue *kb = irConstInt(IR_TYPE_I64,
                                             _case->case_begin);
                    IrInstr *ge_i = irInstrNew(IR_ICMP, ge, cond, kb);
                    ge_i->extra.cmp_kind = IR_CMP_GE;
                    irBlockAddInstr(ctx, ge_i);
                    irBranch(ctx->cur_func, ctx->cur_block, ge,
                             upper, next_test);
                    irFnAddBlock(ctx->cur_func, upper);
                    ctx->cur_block = upper;

                    IrValue *le = irTmp(IR_TYPE_I8, 1);
                    IrValue *ke = irConstInt(IR_TYPE_I64,
                                             _case->case_end);
                    IrInstr *le_i = irInstrNew(IR_ICMP, le, cond, ke);
                    le_i->extra.cmp_kind = IR_CMP_LE;
                    irBlockAddInstr(ctx, le_i);
                    irBranch(ctx->cur_func, ctx->cur_block, le,
                             body, next_test);
                }

                if (i + 1 < n) {
                    irFnAddBlock(ctx->cur_func, next_test);
                    ctx->cur_block = next_test;
                }
            }

            /* If there were no cases, the cond eval ran but we never
             * branched - just jump to default/end. */
            if (n == 0 && !ctx->cur_block->sealed) {
                IrInstr *j = irJump(ctx->cur_func, ctx->cur_block,
                                    default_block);
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
            break;
        }

        case AST_FUNC:
        case AST_LITERAL:
        case AST_ARRAY_INIT:
        case AST_ASM_STMT:
        case AST_ASM_FUNC_BIND:
        case AST_FUNPTR:
        case AST_DEFAULT_PARAM:
        case AST_VAR_ARGS:
        case AST_ASM_FUNCDEF:
        case AST_CAST:
        case AST_FUN_PROTO:
        case AST_CASE:
        case AST_JUMP:
        case AST_EXTERN_FUNC:
        case AST_PLACEHOLDER:
        case AST_DEFAULT:
        case AST_SIZEOF:
        case AST_COMMENT:
            loggerPanic("Unhandled Ast kind `%s`\n%s\n",
                    astKindToString(ast->kind),
                    astToString(ast));
            break;
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

    /* Lower parameters into per-id IrValue slots. The slice constrains params
     * to AST_LVAR (int), but keep the existing fan-out so out-of-slice callers
     * (the ARM64 backend behind __USE_NEW_BACKEND__) still work. */
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

    /* Reserve a stack slot for the return value (skipped for void). Allocated
     * in the entry block before any user code so AST_RETURN can store into it
     * and the exit block can load from it.
     *
     * Struct-by-value return: instead of an alloca, we synthesize a
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

void irDump(Cctrl *cc) {
    IrCtx *ctx = irCtxNew(cc);
    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind != AST_FUNC) continue;
        ctx->cur_func = NULL;
        if (!irFunctionEligibleForSliceCc(cc, ast)) {
            printf("# function %s: not in slice\n",
                   ast->fname ? ast->fname->data : "<anon>");
            continue;
        }
        irLowerFunction(ctx, ast);
        printf("# === lowered ===\n");
        irPrintFunction(ctx->cur_func);
        irMem2Reg(ctx->cur_func);
        printf("# === after mem2reg ===\n");
        irPrintFunction(ctx->cur_func);
        irFoldFunction(ctx->cur_func);
        printf("# === after constant-fold ===\n");
        irPrintFunction(ctx->cur_func);
    }
}

IrCtx *irLowerProgram(Cctrl *cc) {
    IrCtx *ctx = irCtxNew(cc);
    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            ctx->cur_func = NULL;
            irLowerFunction(ctx, ast);
            irCtxAddFunction(ctx, ctx->cur_func);
        }
    }
    return ctx;
}
