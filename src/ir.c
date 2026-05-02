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

    if (cond->type != IR_TYPE_I8) {
        IrValue *zero = irConstInt(IR_TYPE_I8, 0);
        IrValue *bool_cond = irTmp(IR_TYPE_I8, 1);
        IrInstr *cmp = irICmp(bool_cond, IR_CMP_NE, cond, zero);
        listAppend(block->instructions, cmp);
        cond = bool_cond;
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

/* Slice-0 eligibility: walk an AST and reject anything we don't yet lower. */
static int irTypeIsSliceInt(AstType *type) {
    return type && type->kind == AST_TYPE_INT;
}

static int irExprIsSliceEligible(Ast *ast);
static int irStmtIsSliceEligible(Ast *ast);

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

static int irExprIsSliceEligible(Ast *ast) {
    if (!ast) return 0;
    switch (ast->kind) {
    case AST_LITERAL:
        return ast->type && ast->type->kind == AST_TYPE_INT;
    case AST_LVAR:
        /* Slice locals/params are int, pointer, or stack class. Loading an
         * int reads its value; loading a pointer reads its pointer value;
         * loading a class isn't valid here (struct copy isn't supported)
         * but accessing fields via AST_CLASS_REF is. */
        if (!ast->type) return 0;
        return ast->type->kind == AST_TYPE_INT ||
               ast->type->kind == AST_TYPE_POINTER;
    case AST_BINOP:
        if (ast->binop == AST_BIN_OP_ASSIGN ||
            irBinOpIsCompoundAssign(ast->binop)) {
            /* LHS may be:
             *  - a plain int local,
             *  - a pointer local (just reassigning a pointer),
             *  - an int-typed class field (`p.x` or `p->x`),
             *  - a deref of a pointer (`*p`). */
            if (!ast->left) return 0;
            Ast *lhs = ast->left;
            int lhs_ok = 0;
            if (lhs->kind == AST_LVAR && lhs->type) {
                lhs_ok = lhs->type->kind == AST_TYPE_INT ||
                         lhs->type->kind == AST_TYPE_POINTER;
            } else if (lhs->kind == AST_CLASS_REF) {
                lhs_ok = irExprIsSliceEligible(lhs);
            } else if (lhs->kind == AST_UNOP &&
                       lhs->unop == AST_UN_OP_DEREF) {
                lhs_ok = irExprIsSliceEligible(lhs);
            }
            return lhs_ok && irExprIsSliceEligible(ast->right);
        }
        if (!irBinOpIsSliceArith(ast->binop)) return 0;
        if (!irTypeIsSliceInt(ast->type)) return 0;
        return irExprIsSliceEligible(ast->left)
            && irExprIsSliceEligible(ast->right);
    case AST_UNOP:
        if (irUnOpIsSliceIncDec(ast->unop)) {
            if (!ast->operand || ast->operand->kind != AST_LVAR) return 0;
            return irTypeIsSliceInt(ast->operand->type);
        }
        if (ast->unop == AST_UN_OP_DEREF) {
            /* `*p` where p is a pointer; result must be slice-int. */
            if (!ast->operand || !ast->operand->type) return 0;
            if (ast->operand->type->kind != AST_TYPE_POINTER) return 0;
            if (!irTypeIsSliceInt(ast->type)) return 0;
            return irExprIsSliceEligible(ast->operand);
        }
        if (ast->unop == AST_UN_OP_ADDR_OF) {
            /* `&local` / `&param`: produce a pointer value. */
            if (!ast->operand || ast->operand->kind != AST_LVAR) return 0;
            return 1;
        }
        return 0;
    case AST_FUNCALL: {
        /* Slice supports calls returning int or void with up to 6 args
         * (System V's int-arg register set). Args may be int expressions
         * or string literals (printf-style). No float args yet. */
        if (!ast->type) return 0;
        AstTypeKind rk = ast->type->kind;
        if (rk != AST_TYPE_INT && rk != AST_TYPE_VOID) return 0;
        if (!ast->args || ast->args->size > 6) return 0;
        for (u64 i = 0; i < ast->args->size; ++i) {
            Ast *arg = vecGet(Ast *, ast->args, i);
            if (!irArgIsSliceEligible(arg)) return 0;
            if (arg->kind == AST_LITERAL && arg->type &&
                arg->type->kind == AST_TYPE_FLOAT) return 0;
        }
        return 1;
    }
    case AST_CLASS_REF: {
        /* `cls.field` (stack class) or `cls->field` (pointer to class).
         * Field type must be slice-int. */
        if (!ast->type || !irTypeIsSliceInt(ast->type)) return 0;
        if (!ast->cls || ast->cls->kind != AST_LVAR) return 0;
        AstType *ct = ast->cls->type;
        if (!ct) return 0;
        if (ct->kind == AST_TYPE_CLASS) return 1;
        if (ct->kind == AST_TYPE_POINTER && ct->ptr &&
            ct->ptr->kind == AST_TYPE_CLASS) return 1;
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
        AstType *t = ast->declvar->type;
        if (t->kind == AST_TYPE_INT) {
            if (ast->declinit &&
                !irExprIsSliceEligible(ast->declinit)) return 0;
            return 1;
        }
        if (t->kind == AST_TYPE_CLASS) {
            /* No struct copy / aggregate init in slice yet. */
            if (ast->declinit) return 0;
            return 1;
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
        /* Lowering enforces these appear inside a loop. */
        return 1;
    case AST_BINOP:
        /* Statement-level: only assignment is meaningful */
        return irExprIsSliceEligible(ast);
    case AST_UNOP:
        /* Statement-level ++/-- on int locals. */
        return irExprIsSliceEligible(ast);
    case AST_FUNCALL:
        /* Discard return value. */
        return irExprIsSliceEligible(ast);
    default:
        return 0;
    }
}

int irFunctionEligibleForSlice(Ast *ast_func) {
    if (!ast_func || ast_func->kind != AST_FUNC) return 0;
    if (ast_func->flags & AST_FLAG_INLINE) return 0;
    if (ast_func->has_var_args) return 0;
    AstType *rettype = ast_func->type ? ast_func->type->rettype : NULL;
    if (!rettype) return 0;
    if (rettype->kind != AST_TYPE_VOID && rettype->kind != AST_TYPE_INT) return 0;

    if (ast_func->params) {
        if (ast_func->params->size > 6) return 0;  /* SysV int-arg regs */
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            if (p->kind != AST_LVAR) return 0;
            if (!p->type) return 0;
            if (p->type->kind == AST_TYPE_INT) continue;
            if (p->type->kind == AST_TYPE_POINTER) continue;
            return 0;
        }
    }

    listForEach(ast_func->locals) {
        Ast *l = (Ast *)it->value;
        if (l->kind != AST_LVAR) return 0;
        if (!l->type) return 0;
        if (l->type->kind == AST_TYPE_INT) continue;
        /* Stack-allocated class with int-only fields. We don't validate
         * fields here; AST_CLASS_REF accesses are checked against
         * irTypeIsSliceInt at use-site, so non-int fields just mean the
         * function loses eligibility through some specific access. */
        if (l->type->kind == AST_TYPE_CLASS) continue;
        if (l->type->kind == AST_TYPE_POINTER) continue;
        return 0;
    }

    return irStmtIsSliceEligible(ast_func->body);
}

IrValue *irFnCall(IrCtx *ctx, Ast *ast) {
    IrValueType ret_type = irConvertType(ast->type);
    IrValue *ir_call_args = irValueNew(IR_TYPE_ARRAY, IR_VAL_UNRESOLVED);
    IrValue *ir_ret_val = irTmp(ret_type, ast->type->size);
    IrInstr *ir_call_instr = irInstrNew(IR_CALL, ir_ret_val, ir_call_args, NULL);

    assert(ast->kind == AST_FUNCALL);

    Vec *args = irValueVecNew();
    ir_call_args->as.array.values = args;
    ir_call_args->as.array.label = ast->fname;

    if (ast->args) {
        for (u64 i = 0; i < ast->args->size; ++i) {
            Ast *ast_arg = ast->args->entries[i];
            IrValue *ir_arg = irExpr(ctx, ast_arg);
            vecPush(args, ir_arg);
        }
    }

    irBlockAddInstr(ctx, ir_call_instr);
    return ir_ret_val;
}

/* Binary expressions are assumed to always be assigning to something. I'm not
 * 100% sure this is a valid assumption to make. Well I guess;
 * `I64 x = y + 32 * 10` _could_ continually be assigned to `x` */
IrValue *irBinOpExpr(IrCtx *ctx, Ast *ast) {
    IrValue *lhs = irExpr(ctx, ast->left);
    IrValue *rhs = irExpr(ctx, ast->right);
    IrValueType ir_type = irConvertType(ast->type);
    IrValue *ir_result = irTmp(ir_type, ast->type->size);
    IrOp op;
    IrCmpKind cmp = IR_CMP_INVALID;

    if (ast->binop == AST_BIN_OP_LOG_AND) {
        IrBlock *ir_block = ctx->cur_block;
        IrBlock *ir_right_block = irBlockNew();
        IrBlock *ir_end_block = irBlockNew();

        IrValue *left = irExpr(ctx, ast->left);
        IrValue *ir_result = irTmp(IR_TYPE_I8, 1);

        irBranch(ctx->cur_func, ir_block, left, ir_right_block, ir_end_block);
        irFnAddBlock(ctx->cur_func, ir_right_block);
        ctx->cur_block = ir_right_block;

        IrValue *right = irExpr(ctx, ast->right);

        IrInstr *jump_instr = irJump(ctx->cur_func, ctx->cur_block, ir_end_block);

        /* Add to the current blocks instructions */
        irBlockAddInstr(ctx, jump_instr);
        irFnAddBlock(ctx->cur_func, ir_end_block);
        ctx->cur_block = ir_end_block;

        IrInstr *phi_instr = irPhi(ir_result);
        irBlockAddInstr(ctx, phi_instr);
        irAddPhiIncoming(phi_instr, irConstInt(IR_TYPE_I8, 0), ir_block);
        irAddPhiIncoming(phi_instr, right, ir_right_block);
        return ir_result;
    } else if (ast->binop == AST_BIN_OP_LOG_OR) {
        IrBlock *ir_block = ctx->cur_block;
        IrBlock *ir_right_block = irBlockNew();
        IrBlock *ir_end_block = irBlockNew();

        IrValue *left = irExpr(ctx, ast->left);
        IrValue *ir_result = irTmp(IR_TYPE_I8, 1);

        /* For an OR the difference is this is switched around */
        irBranch(ctx->cur_func, ir_block, left, ir_end_block, ir_right_block);
        irFnAddBlock(ctx->cur_func, ir_right_block);
        ctx->cur_block = ir_right_block;

        IrValue *right = irExpr(ctx, ast->right);

        IrInstr *jump_instr = irJump(ctx->cur_func, ctx->cur_block, ir_end_block);

        /* Add to the current blocks instructions */
        irBlockAddInstr(ctx, jump_instr);
        irFnAddBlock(ctx->cur_func, ir_end_block);
        ctx->cur_block = ir_end_block;

        IrInstr *phi_instr = irPhi(ir_result);
        irBlockAddInstr(ctx, phi_instr);
        irAddPhiIncoming(phi_instr, irConstInt(IR_TYPE_I8, 0), ir_block);
        irAddPhiIncoming(phi_instr, right, ir_right_block);
        return ir_result;
    } else if (irIsFloat(ir_type)) {
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
                op = IR_SHR;
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
    IrInstr *instr = irInstrNew(op, ir_result, lhs, rhs);
    instr->extra.cmp_kind = cmp;
    irBlockAddInstr(ctx, instr);
    return ir_result;
}

static IrValue *irLowerAssign(IrCtx *ctx, Ast *ast);

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
        case AST_LVAR: {
            IrValue *local_var = irFnGetVar(ctx->cur_func, ast->lvar_id);
            if (!local_var) {
                loggerPanic("Variable %s not found\n", astToString(ast));
            }

            IrValueType ir_value_type = irConvertType(ast->type);
            IrValue *ir_load_dest = irTmp(ir_value_type, ast->type->size);

            if (irIsStruct(local_var->type)) {
                loggerWarning("Unhandled load of a struct!\n");
                // irGetElementPointer(ir_block, ir_load_dest, local_var);
            } else {
                IrInstr *load_instr = irLoad(ir_load_dest, local_var);
                irBlockAddInstr(ctx, load_instr);
            }
            return ir_load_dest;
        }

        case AST_FUNCALL:
            return irFnCall(ctx, ast);

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
             *   pointer:     cls is AST_LVAR with pointer-to-class type.
             *                Load the pointer, add the field offset, then
             *                dereference. */
            Ast *cls = ast->cls;
            if (!cls || cls->kind != AST_LVAR) {
                loggerPanic("Slice AST_CLASS_REF only supports AST_LVAR cls\n");
            }
            IrValue *base = irFnGetVar(ctx->cur_func, cls->lvar_id);
            if (!base) loggerPanic("AST_CLASS_REF on unknown lvar\n");

            IrValueType field_ir_type = irConvertType(ast->type);
            IrValue *load_dst = irTmp(field_ir_type, ast->type->size);
            int offset = ast->type->offset;

            if (cls->type && cls->type->kind == AST_TYPE_POINTER) {
                /* Pointer-to-class: load pointer, add offset, deref. */
                IrValue *ptr_val = irTmp(IR_TYPE_PTR, 8);
                irBlockAddInstr(ctx, irLoad(ptr_val, base));
                IrValue *addr = ptr_val;
                if (offset != 0) {
                    addr = irTmp(IR_TYPE_PTR, 8);
                    IrValue *off_const = irConstInt(IR_TYPE_I64, offset);
                    irBlockAddInstr(ctx,
                        irInstrNew(IR_IADD, addr, ptr_val, off_const));
                }
                irBlockAddInstr(ctx,
                    irInstrNew(IR_LOAD_DEREF, load_dst, addr, NULL));
                return load_dst;
            }

            /* Stack-allocated class. */
            IrValue *field_addr = irTmp(IR_TYPE_PTR, 8);
            IrValue *off = irConstInt(IR_TYPE_I64, offset);
            irBlockAddInstr(ctx,
                irInstrNew(IR_GEP, field_addr, base, off));
            irBlockAddInstr(ctx, irLoad(load_dst, field_addr));
            return load_dst;
        }

        case AST_UNOP: {
            if (ast->unop == AST_UN_OP_DEREF) {
                /* `*p`: evaluate the pointer expression, then load through it. */
                IrValue *ptr = irExpr(ctx, ast->operand);
                IrValueType ir_type = irConvertType(ast->type);
                IrValue *load_dst = irTmp(ir_type, ast->type->size);
                irBlockAddInstr(ctx,
                    irInstrNew(IR_LOAD_DEREF, load_dst, ptr, NULL));
                return load_dst;
            }
            if (ast->unop == AST_UN_OP_ADDR_OF) {
                /* `&lvar`: take the slot's address. The lvar's IrValue
                 * carries the slot loff — IR_LEA reads that and emits
                 * the leaq. */
                Ast *lvar = ast->operand;
                IrValue *src = irFnGetVar(ctx->cur_func, lvar->lvar_id);
                if (!src) loggerPanic("&lvar on unknown lvar\n");
                IrValue *dst = irTmp(IR_TYPE_PTR, 8);
                irBlockAddInstr(ctx, irInstrNew(IR_LEA, dst, src, NULL));
                return dst;
            }
            /* Slice supports ++/-- on int locals. The store to the lvar's
             * slot is the side effect; we return either the original value
             * (POST) or the incremented one (PRE). */
            if (ast->unop != AST_UN_OP_PRE_INC &&
                ast->unop != AST_UN_OP_PRE_DEC &&
                ast->unop != AST_UN_OP_POST_INC &&
                ast->unop != AST_UN_OP_POST_DEC) {
                loggerPanic("Slice unop only supports ++/-- and *, got %s\n",
                            astUnOpKindToString(ast->unop));
            }
            Ast *lvar = ast->operand;
            IrValue *slot = irFnGetVar(ctx->cur_func, lvar->lvar_id);
            if (!slot) loggerPanic("inc/dec on unknown lvar\n");

            IrValueType ir_type = irConvertType(lvar->type);
            int size = lvar->type->size;

            IrValue *cur = irTmp(ir_type, size);
            irBlockAddInstr(ctx, irLoad(cur, slot));

            int is_inc = (ast->unop == AST_UN_OP_PRE_INC ||
                          ast->unop == AST_UN_OP_POST_INC);
            IrValue *one = irConstInt(ir_type, 1);
            IrValue *new_val = irTmp(ir_type, size);
            IrInstr *op = irInstrNew(is_inc ? IR_IADD : IR_ISUB,
                                     new_val, cur, one);
            irBlockAddInstr(ctx, op);
            irBlockAddInstr(ctx, irStore(slot, new_val));

            int is_post = (ast->unop == AST_UN_OP_POST_INC ||
                           ast->unop == AST_UN_OP_POST_DEC);
            return is_post ? cur : new_val;
        }

        case AST_GVAR:
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
        case AST_ASM_FUNCALL:
        case AST_FUNPTR:
        case AST_FUNPTR_CALL:
        case AST_BREAK:
        case AST_CONTINUE:
        case AST_DEFAULT_PARAM:
        case AST_VAR_ARGS:
        case AST_ASM_FUNCDEF:
        case AST_CAST:
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

/* Address-mode info for an assignment LHS. `target` is the IrValue used
 * by the IR_STORE / IR_STORE_DEREF; `indirect` says which one to use. */
typedef struct {
    IrValue *target;
    AstType *type;
    int indirect;   /* 1 -> IR_STORE_DEREF, 0 -> IR_STORE. */
} IrAssignTarget;

static IrAssignTarget irLowerAssignTarget(IrCtx *ctx, Ast *lhs) {
    IrAssignTarget out = { NULL, NULL, 0 };

    if (lhs->kind == AST_LVAR) {
        IrValue *local = irFnGetVar(ctx->cur_func, lhs->lvar_id);
        if (!local) loggerPanic("irLowerAssign: unknown local %s\n",
                                astToString(lhs));
        out.target = local;
        out.type = lhs->type;
        return out;
    }

    if (lhs->kind == AST_CLASS_REF) {
        Ast *cls = lhs->cls;
        if (!cls || cls->kind != AST_LVAR) {
            loggerPanic("Slice AST_CLASS_REF only supports AST_LVAR cls\n");
        }
        IrValue *base = irFnGetVar(ctx->cur_func, cls->lvar_id);
        if (!base) loggerPanic("AST_CLASS_REF on unknown lvar\n");
        int offset = lhs->type->offset;
        out.type = lhs->type;

        if (cls->type && cls->type->kind == AST_TYPE_POINTER) {
            /* Pointer-to-class: load pointer, optionally add offset; the
             * resulting tmp holds the field's runtime address. */
            IrValue *ptr_val = irTmp(IR_TYPE_PTR, 8);
            irBlockAddInstr(ctx, irLoad(ptr_val, base));
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

    IrOp store_op = tgt.indirect ? IR_STORE_DEREF : IR_STORE;
    irBlockAddInstr(ctx, irInstrNew(store_op, tgt.target, new_val, NULL));
    return new_val;
}

void irLowerAst(IrCtx *ctx, Ast *ast) {
    if (!ast) return;

    /* Once a block is sealed (it ended in ret/jmp/br) any subsequent statement
     * is unreachable; just drop it. */
    if (ctx->cur_block && ctx->cur_block->sealed) return;

    switch (ast->kind) {
        case AST_COMPOUND_STMT: {
            listForEach(ast->stms) {
                Ast *next = (Ast *)it->value;
                if (ctx->cur_block && ctx->cur_block->sealed) break;
                irLowerAst(ctx, next);
            }
            break;
        }

        case AST_LVAR:
            /* Bare lvalue at statement level — no side effect. */
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
                IrValue *val = irExpr(ctx, ast->retval);
                IrInstr *st = irStore(ctx->cur_func->return_value, val);
                irBlockAddInstr(ctx, st);
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
            IrInstr *ir_alloca = irAlloca(var->type);
            irAddStackSpace(ctx, var->type->size);
            irBlockAddInstr(ctx, ir_alloca);
            IrValue *local = ir_alloca->dst;
            irFnAddVar(ctx->cur_func, ast->declvar->lvar_id, local);

            if (init) {
                IrValue *ir_init = NULL;

                switch (init->kind) {
                    case AST_ARRAY_INIT: {
                        loggerPanic("Unhandled: %s\n", astKindToString(init->kind));
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
                        IrValue *ret = irFnCall(ctx, init);
                        IrInstr *ir_store = irStore(local, ret);
                        irBlockAddInstr(ctx, ir_store);
                        break;
                    }

                    case AST_UNOP:
                        loggerPanic("Unhandled: %s\n", astKindToString(init->kind));
                        break;

                    default: {
                        ir_init = irExpr(ctx, init);
                        IrInstr *ir_store = irStore(local, ir_init);
                        irBlockAddInstr(ctx, ir_store);
                        break;
                    }    
                }
            }
            break;
        }

        case AST_FUNCALL:
            irExpr(ctx, ast);
            break;

        case AST_STRING:
            irExpr(ctx, ast);
            break;

        case AST_GVAR:
        case AST_GOTO:
        case AST_LABEL:
        case AST_FUNC:
        case AST_LITERAL:
        case AST_ARRAY_INIT:
        case AST_ASM_STMT:
        case AST_ASM_FUNC_BIND:
        case AST_ASM_FUNCALL:
        case AST_FUNPTR:
        case AST_FUNPTR_CALL:
        case AST_DEFAULT_PARAM:
        case AST_VAR_ARGS:
        case AST_ASM_FUNCDEF:
        case AST_CAST:
        case AST_FUN_PROTO:
        case AST_CASE:
        case AST_JUMP:
        case AST_EXTERN_FUNC:
        case AST_PLACEHOLDER:
        case AST_SWITCH:
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
    func->entry_block = entry;
    func->exit_block = exit_block;
    irFnAddBlock(func, entry);

    /* Lower parameters into per-id IrValue slots. The slice constrains params
     * to AST_LVAR (int), but keep the existing fan-out so out-of-slice callers
     * (the ARM64 backend behind __USE_NEW_BACKEND__) still work. */
    Ast *ast_var_args = NULL;
    for (u64 i = 0; i < ast_func->params->size; ++i) {
        Ast *ast_param = vecGet(Ast *, ast_func->params, i);
        if (ast_param->kind == AST_VAR_ARGS) {
            assert(func->has_var_args);
            ast_var_args = ast_param;
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
    if (ast_var_args) {
        loggerWarning("%s\n", astToString(ast_var_args));
    }

    /* Reserve a stack slot for the return value (skipped for void). Allocated
     * in the entry block before any user code so AST_RETURN can store into it
     * and the exit block can load from it. */
    AstType *rettype = ast_func->type->rettype;
    IrValue *ir_return_var = NULL;
    if (rettype && rettype->kind != AST_TYPE_VOID) {
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
        IrValue *ir_load_dst = irTmp(ir_return_var->type,
                                     irGetIntSize(ir_return_var->type));
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
        if (!irFunctionEligibleForSlice(ast)) {
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
