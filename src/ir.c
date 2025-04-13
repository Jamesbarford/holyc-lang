/**
 * This is the intermediate representation of our AST getting it prepped for
 * codegen.
 *
 * @TODO:
 * - default function parameters; perhaps this is a function of the parser 
 *   as it could potentially be done at the code generation phase?
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "ir.h"
#include "ir-interp.h"
#include "ir-types.h"
#include "ir-optimiser.h"
#include "list.h"
#include "lexer.h"
#include "map.h"
#include "memory.h"
#include "uniq-list.h"
#include "util.h"

/*==================== IR PROTOTYPES ======================================== */
IrUnresolvedBlock *irUnresolvedGotoNew(List *list_node, IrBlock *ir_source_block);
IrUnresolvedBlock *irUnresolvedLabelNew(List *list_node, IrBlock *ir_destination_block);
IrInstr *irCmp(IrBlock *block, IrValue *result, IrValue *op1, IrValue *op2,
               IrCmpKind kind);
IrInstr *irICmp(IrBlock *block, IrValue *result, IrCmpKind kind, IrValue *op1, 
                IrValue *op2);
IrInstr *irFCmp(IrBlock *block, IrValue *result, IrCmpKind kind, IrValue *op1, 
                IrValue *op2);
IrInstr *irJump(IrFunction *ir_function, IrBlock *block, IrBlock *target);
IrInstr *irBranch(IrFunction *func,
                  IrBlock *block,
                  IrValue *cond,
                  IrBlock *true_block,
                  IrBlock *false_block);
IrValue *irLoadClassRef(IrCtx *ctx,
                        IrFunction *func,
                        Ast *cls,
                        AstType *field,
                        int offset);
aoStr *irBlockToString(IrFunction *func, IrBlock *ir_block);

IrValue *irGlobalExpression(IrCtx *ctx, Ast *ast);
IrValue *irExpression(IrCtx *ctx, IrFunction *func, Ast *ast);
void irStatement(IrCtx *ctx, IrFunction *func, Ast *ast);

IrValue *irFunctionGetLocalFnPtr(IrFunction *ir_function, Ast *ast);
IrBlockMapping *irFunctionGetBlockMapping(IrFunction *func, IrBlock *ir_block);
Map *irFunctionGetSuccessors(IrFunction *func, IrBlock *ir_block);
Map *irFunctionGetPredecessors(IrFunction *func, IrBlock *ir_block);

const char *irValueTypeToString(IrValueType ir_value_type);
aoStr *irValueToString(IrValue *ir_value);

/*==================== IR CONSTRUCTORS ====================================== */
IrBlock *irBlockNew(int id) {
    IrBlock *ir_block = (IrBlock *)irArenaAlloc(sizeof(IrBlock));
    ir_block->id = id;
    ir_block->instructions = listNew();
    ir_block->sealed = 0;
    ir_block->removed = 0;
    ir_block->ssa_values = strMapNew(8);
    return ir_block;
}

IrUnresolvedBlock *irUnresolvedGotoNew(List *list_node,
                                        IrBlock *ir_source_block)
{
    IrUnresolvedBlock *ir_unresolved = (IrUnresolvedBlock *)irArenaAlloc(
                                                     sizeof(IrUnresolvedBlock));
    ir_unresolved->goto_.list_node = list_node;
    ir_unresolved->goto_.ir_block = ir_source_block;
    return ir_unresolved;
}

static int ir_block_count = 0;
void irBlockCountReset(void) {
    ir_block_count = 0;
}

int irBlockId(void) {
    return ir_block_count++;
}

aoStr *irBlockName(void) {
    return aoStrPrintf("bb%d", ir_block_count++);
}


static int ir_array_count = 0;
aoStr *irArrayName(IrFunction *func) {
    return aoStrPrintf("array%d.%s",ir_array_count++,func->name->data);
}

void irArrayCountReset(void) {
    ir_array_count = 0;
}

/* We will reset this after each function has been created */
static int ir_tmp_variable_count = 1;
void irTmpVariableCountReset(void) {
    ir_tmp_variable_count = 1;
}

void irBlockAddInstruction(IrBlock *block, IrInstr *ir_instr) {
    listAppend(block->instructions, ir_instr);
}

/* Check to see if the instruction is the last one in the list */
int irBlockIsLastInstruction(IrBlock *block, IrInstr *ir_instr) {
    List *last = block->instructions->prev;
    IrInstr *ir_last_instr = (IrInstr *)last->value;
    return ir_last_instr == ir_instr;
}

IrUnresolvedBlock *irUnresolvedLabelNew(List *list_node,
                                        IrBlock *ir_destination_block)
{
    IrUnresolvedBlock *ir_unresolved = (IrUnresolvedBlock *)irArenaAlloc(
                                                     sizeof(IrUnresolvedBlock));
    ir_unresolved->label_.list_node = list_node;
    ir_unresolved->label_.ir_block = ir_destination_block;
    return ir_unresolved;
}

void irCtxResetArray(IrCtx *ctx) {
    memset(&ctx->array_,0,sizeof(IrArrayCtx));
}

void irCtxReset(IrCtx *ctx) {
    ctx->flags = 0;
    ctx->current_block = NULL;
    ctx->end_block = NULL;
    ctx->loop_head_block = NULL;

    irCtxResetArray(ctx);
    irBlockCountReset();
    irTmpVariableCountReset();

    if (ctx->unresolved_gotos) {
        ptrVecClear(ctx->unresolved_gotos, NULL);
    }
    if (ctx->unresolved_labels) {
        strMapClear(ctx->unresolved_labels);
    }
}

IrCtx *irCtxNew(void) {
    IrCtx *ctx = (IrCtx *)xmalloc(sizeof(IrCtx));
    ctx->unresolved_gotos = NULL;
    ctx->unresolved_labels = NULL;
    irCtxReset(ctx);
    ctx->unresolved_gotos = ptrVecNew();
    ctx->unresolved_labels = strMapNew(8);
    return ctx;
}

void irCtxRelease(IrCtx *ctx) {
    xfree(ctx);
}

static void irCtxSetCurrentBlock(IrCtx *ctx, IrBlock *ir_block) {
    ctx->current_block = ir_block;
}

static void irCtxSetEndBlock(IrCtx *ctx, IrBlock *ir_block) {
    ctx->end_block = ir_block;
}

static void irCtxSetLoopHeadBlock(IrCtx *ctx, IrBlock *ir_block) {
    ctx->loop_head_block = ir_block;
}

IrValue *irTmpVariable(IrValueType ir_value_type) {
    IrValue *ir_value = irValueNew(ir_value_type, IR_VALUE_TEMP);
    ir_value->name = aoStrPrintf("%%t%d",ir_tmp_variable_count++);
    return ir_value;
}

IrValue *irGetAllocaVar(IrInstr *ir_alloca) {
    assert(ir_alloca->opcode == IR_OP_ALLOCA);
    return ir_alloca->result;
}

IrValue *irConstInt(IrValueType ir_value_type, long i64) {
    IrValue *ir_value = irValueNew(ir_value_type, IR_VALUE_CONST_INT);
    ir_value->i64 = i64;
    return ir_value;
}

IrValue *irConstFloat(IrValueType ir_value_type, double f64) {
    IrValue *ir_value = irValueNew(ir_value_type, IR_VALUE_CONST_FLOAT);
    ir_value->f64 = f64;
    return ir_value;
}

static int ir_const_float_count = 0;
void irAddConstFloat(IrProgram *ir_program, IrValue *const_float) {
    /* @Bug
     * This means we can value multiple labels which have the same value of 
     * const float - means wasted space and is a bit annoying but not 
     * fundamentally broken */
    aoStr *float_label = aoStrPrintf("F%d", ir_const_float_count++);
    const_float->name = float_label;
    strMapAddAoStr(ir_program->floats, float_label, const_float);
}

IrValue *irGlobalString(IrProgram *ir_program, Ast *ast) {
    IrValue *ir_value = strMapGetAoStr(ir_program->strings, ast->slabel);
    if (!ir_value) {
        /* A string is an array of characters */
        ir_value = irValueNew(IR_TYPE_ARRAY, IR_VALUE_CONST_STR);
        ir_value->str = ast->sval;
        ir_value->str_real_len = ast->real_len;
        ir_value->name = ast->slabel;
        strMapAddAoStr(ir_program->strings, ast->slabel, ir_value);
    }
    return ir_value;
}

IrValue *irFunctionGetLocal(IrFunction *ir_function, Ast *ast) {
    assert(ast->kind == AST_LVAR);
    IrValue *ir_local_var = strMapGetAoStr(ir_function->variables,
                                           ast->tmp_var_name);
    if (!ir_local_var) {
        loggerPanic("Local variable with original name `%s` not found\n",
                ast->lname->data);
    }
    return ir_local_var;
}

IrValue *irFunctionGetGlobal(IrFunction *ir_function, Ast *ast) {
    assert(ast->kind == AST_GVAR);
    IrProgram *ir_program = ir_function->program;
    IrValue *ir_global_var = mapGet(ir_program->global_variables, ast->gname);
    if (!ir_global_var) {
        loggerPanic("Global variable with name `%s` not found\n",
                    ast->gname->data);
    }
    return ir_global_var;
}

IrValue *irFunctionGetLocalFnPtr(IrFunction *ir_function, Ast *ast) {
    assert(ast->kind == AST_FUNPTR || ast->kind == AST_FUNPTR_CALL || 
           ast->kind == AST_FUNC);

    IrValue *ir_fnptr = NULL;
    if (ast->tmp_fnptr_name) {
        ir_fnptr = strMapGetAoStr(ir_function->variables,
                ast->tmp_fnptr_name);
    } else {
        ir_fnptr = strMapGetAoStr(ir_function->variables,
                ast->tmp_var_name);
    }

    if (!ir_fnptr) {
        loggerPanic("Local function pointer with original name `%s` not found\n",
                    ast->fname->data);
    }
    return ir_fnptr;
}

/* Pass in the whole block to abstract away that we area using an interal 
 * datastructure to keep track of things. I'm trying a few different ones out */
void irFunctionAddSuccessor(IrFunction *func, IrBlock *src, IrBlock *dest) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)mapGet(func->cfg, src->id);
    if (!ir_block_mapping) {
        ir_block_mapping = irBlockMappingNew(src->id);
        mapAdd(func->cfg, src->id, ir_block_mapping);
    }
    mapAdd(ir_block_mapping->successors, dest->id, dest);
}

void irFunctionAddPredecessor(IrFunction *func, IrBlock *src, IrBlock *prev) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)mapGet(func->cfg, src->id);
    if (!ir_block_mapping) {
        ir_block_mapping = irBlockMappingNew(src->id);
        mapAdd(func->cfg, src->id, ir_block_mapping);
    }
    mapAdd(ir_block_mapping->predecessors, prev->id, prev);
}

void irFunctionRemoveSuccessor(IrFunction *func, IrBlock *src, IrBlock *dest) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)mapGet(func->cfg, src->id);
    if (ir_block_mapping) {
        mapRemove(ir_block_mapping->successors, dest->id);
    }
}

void irFunctionRemovePredecessor(IrFunction *func, IrBlock *src, IrBlock *prev) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)mapGet(func->cfg, src->id);
    if (ir_block_mapping) {
        mapRemove(ir_block_mapping->predecessors, prev->id);
    }
}

/* Add `dest` to the `src`'s successor set AND add `src` to `dest`'s 
 * predecessor set */
void irFunctionAddMapping(IrFunction *func, IrBlock *src, IrBlock *dest) {
    irFunctionAddSuccessor(func, src, dest);
    irFunctionAddPredecessor(func, dest, src);
}

void irFunctionRemoveMapping(IrFunction *func, IrBlock *src, IrBlock *dest) {
    irFunctionRemoveSuccessor(func, src, dest);
    irFunctionRemovePredecessor(func, dest, src);
}

void irFunctionAddBlock(IrFunction *func, IrBlock *block) {
    listAppend(func->blocks, block);
}

IrBlock *irFunctionFindBlock(IrFunction *func, int id) {
    listForEach(func->blocks) {
        IrBlock *ir_block = (IrBlock *)it->value;
        if (ir_block->id == id) {
            return ir_block;
        }
    }
    return NULL;
}

IrInstr *irInstrNew(IrOpcode opcode) {
    IrInstr *ir_instr = (IrInstr *)irArenaAlloc(sizeof(IrInstr));
    memset(ir_instr, 0, sizeof(IrInstr));
    ir_instr->opcode = opcode;
    return ir_instr;
}

void irInstrRelease(IrInstr *ir_instr) {
    /* haven't figured out what to do with these... They are all from the area
     * so I guess we could 'recycle' unused instructions by keeping a free list
     * of them and checking that first which should lead to less memory
     * wastage */
    (void)ir_instr;
}

IrPair *irPairNew(IrBlock *ir_block, IrValue *ir_value) {
    IrPair *ir_phi_pair = (IrPair *)irArenaAlloc(sizeof(IrPair));
    ir_phi_pair->ir_value = ir_value;
    ir_phi_pair->ir_block = ir_block;
    return ir_phi_pair;
}

IrInstr *irPhi(IrBlock *block, IrValue *result) {
    IrInstr *ir_phi_instr = irInstrNew(IR_OP_PHI);
    ir_phi_instr->result = result;
    ir_phi_instr->extra.phi.pairs = ptrVecNew();

    irBlockAddInstruction(block, ir_phi_instr);
    return ir_phi_instr;
}

void irAddPhiIncoming(IrInstr *ir_phi_instr,
                      IrValue *ir_value, 
                      IrBlock *ir_block)
{
    IrPair *ir_phi_pair = irPairNew(ir_block, ir_value);
    ptrVecPush(ir_phi_instr->extra.phi.pairs, ir_phi_pair);
}

/* Where `ir_value` is always a constant int so we know how much stack space
 * we require. We _may_ want type info for of the thing we are storing? */
IrInstr *irAlloca(IrFunction *func, IrBlock *ir_block, AstType *ast_type) {
    IrInstr *ir_alloca_instr = irInstrNew(IR_OP_ALLOCA);
    IrValueType ir_value_type = irConvertType(ast_type);
    IrValue *ir_alloca_size = irConstInt(ir_value_type, ast_type->size);
    IrValue *ir_temporary_variable = irTmpVariable(ir_value_type);
    ir_alloca_instr->result = ir_temporary_variable;
    ir_alloca_instr->op1 = ir_alloca_size;
    irBlockAddInstruction(ir_block, ir_alloca_instr);
    mapAdd(func->allocas, ir_alloca_instr->result->name, ir_alloca_instr); 
    return ir_alloca_instr;
}

/* result is where we are storing something and op1 is the thing we are storing 
 * I think op1 could/shoule have an offset as it is either going to be the 
 * stack or it is going to be a struct/pointer offset? */
IrInstr *irStore(IrBlock *ir_block, IrValue *ir_dest, IrValue *ir_value) {
    IrInstr *ir_store_instr = irInstrNew(IR_OP_STORE);
    ir_store_instr->result = ir_dest;
    ir_store_instr->op1 = ir_value;
    irBlockAddInstruction(ir_block, ir_store_instr);
    return ir_store_instr;
}

IrInstr *irLoad(IrBlock *ir_block, IrValue *ir_dest, IrValue *ir_value) {
    IrInstr *ir_load_instr = irInstrNew(IR_OP_LOAD);
    ir_load_instr->result = ir_dest;
    ir_load_instr->op1 = ir_value;
    irBlockAddInstruction(ir_block, ir_load_instr);
    return ir_load_instr;
}

IrInstr *irRet(IrBlock *ir_block, IrValue *ir_value) {
    IrInstr *ir_return_instr = irInstrNew(IR_OP_RET);
    ir_return_instr->result = ir_value;
    irBlockAddInstruction(ir_block, ir_return_instr);
    return ir_return_instr;
}

IrInstr *irGetElementPointer(IrBlock *ir_block,
                             IrValue *ir_dest,
                             IrValue *ir_value)
{
    IrInstr *ir_getelementptr_instr = irInstrNew(IR_OP_GEP);
    ir_getelementptr_instr->result = ir_dest;
    ir_getelementptr_instr->op1 = ir_value;
    /* @GEP
     * For clarity; op2 is the index, if it is NULL, there is no index */
    ir_getelementptr_instr->op2 = NULL;
    irBlockAddInstruction(ir_block, ir_getelementptr_instr);
    return ir_getelementptr_instr;
}

IrInstr *irGetElementPointerAtIdx(IrBlock *ir_block,
                                  IrValue *ir_dest,
                                  IrValue *ir_value,
                                  long offset)
{
    IrInstr *ir_getelementptr_instr = irInstrNew(IR_OP_GEP);
    IrValue *ir_offset = irConstInt(IR_TYPE_I64, offset);
    ir_getelementptr_instr->result = ir_dest;
    ir_getelementptr_instr->op1 = ir_value;
    ir_getelementptr_instr->op2 = ir_offset;
    irBlockAddInstruction(ir_block, ir_getelementptr_instr);
    return ir_getelementptr_instr;
}

IrFunction *irFunctionNew(aoStr *name) {
    IrFunction *ir_function = (IrFunction *)irArenaAlloc(sizeof(IrFunction));
    ir_function->name = name;
    ir_function->params = ptrVecNew();
    ir_function->blocks = listNew();
    ir_function->entry_block = NULL;
    ir_function->exit_block = NULL;
    ir_function->variables = strMapNew(16);
    ir_function->cfg = irIntBlockMappingMapNew();
    ir_function->has_var_args = 0;
    ir_function->allocas = irStrInstrMapNew();
    ir_function->loads = irStrInstrMapNew();
    ir_function->stores = irStrInstrMapNew();
    return ir_function;
}


/*==================== IR AST PARSING ======================================= */

IrInstr *irCmp(IrBlock *block, IrValue *result, IrValue *op1, IrValue *op2,
               IrCmpKind kind)
{
    if (result->type != IR_TYPE_I8) {
        loggerPanic("Result can only be a boolean type\n");
    }

    int is_float_cmp = irIsFloat(op1->type) || irIsFloat(op2->type);
    IrInstr *instr  = irInstrNew(is_float_cmp ? IR_OP_FCMP : IR_OP_ICMP);

    if (is_float_cmp) {
        if (op1->type != op2->type) {
            loggerPanic("Operand types do not match `op1%s` and `op2%s`",
                    irValueTypeToString(op1->type),
                    irValueTypeToString(op2->type));
        }

        switch (kind) {
            case IR_CMP_EQ: kind = IR_CMP_OEQ; break; /* ordered equal */
            case IR_CMP_NE: kind = IR_CMP_ONE; break; /* ordered not equal */
            case IR_CMP_LT: kind = IR_CMP_OLT; break; /* ordered less than */
            case IR_CMP_LE: kind = IR_CMP_OLE; break; /* ordered less or equal */
            case IR_CMP_GT: kind = IR_CMP_OGT; break; /* ordered greater than */
            case IR_CMP_GE: kind = IR_CMP_OGE; break; /* ordered greater or equal */
            default: break;
        }
    } else {
        /* Ensure that we can compare these types */
        if (!irAreCompatibleCmpTypes(op1->type, op2->type)) {
            loggerPanic("Operand types cannot be compared `op1%s` and `op2%s`",
                    irValueTypeToString(op1->type),
                    irValueTypeToString(op2->type));
        }
    }

    instr->result = result;
    instr->op1 = op1;
    instr->op2 = op2;
    instr->extra.cmp_kind = kind;
    irBlockAddInstruction(block, instr);

    return instr;
}

IrInstr *irICmp(IrBlock *block,
                IrValue *result,
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

    IrInstr *instr = irInstrNew(IR_OP_ICMP);
    instr->result = result;
    instr->op1 = op1;
    instr->op2 = op2;
    instr->extra.cmp_kind = kind;

    irBlockAddInstruction(block, instr);
    return instr;
}

IrInstr *irFCmp(IrBlock *block,
                IrValue *result,
                IrCmpKind kind,
                IrValue *op1, 
                IrValue *op2)
{
    IrInstr *instr = irInstrNew(IR_OP_FCMP);
    instr->result = result;
    instr->op1 = op1;
    instr->op2 = op2;
    instr->extra.cmp_kind = kind;

    irBlockAddInstruction(block, instr);
    return instr;
}

__inline IrInstr *irUnary(IrBlock *block,
                          IrOpcode opcode,
                          IrValue *ir_result,
                          IrValue *ir_expr)
{
    IrInstr *instr = irInstrNew(opcode);
    instr->result = ir_result;
    instr->op1 = ir_expr;
    irBlockAddInstruction(block, instr);
    return instr;
}

IrInstr *irFNeg(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_FNEG, ir_result, ir_expr);
}

IrInstr *irINeg(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_INEG, ir_result, ir_expr);
}


IrInstr *irSExt(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_SEXT, ir_result, ir_expr);
}

IrInstr *irZExt(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_ZEXT, ir_result, ir_expr);
}

IrInstr *irTrunc(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_TRUNC, ir_result, ir_expr);
}

IrInstr *irBitCast(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_BITCAST, ir_result, ir_expr);
}

IrInstr *irFPExt(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_FPEXT, ir_result, ir_expr);
}

IrInstr *irFPTrunc(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_FPTRUNC, ir_result, ir_expr);
}

IrInstr *irSIToFP(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_SITOFP, ir_result, ir_expr);
}

IrInstr *irUIToFP(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_UITOFP, ir_result, ir_expr);
}

IrInstr *irFPToSI(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_FPTOSI, ir_result, ir_expr);
}

IrInstr *irFPToUI(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_FPTOUI, ir_result, ir_expr);
}

IrInstr *irPtrToInt(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_PTRTOINT, ir_result, ir_expr);
}

IrInstr *irIntToPtr(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_INTTOPTR, ir_result, ir_expr);
}

/*==================== IR MATHS ============================================= */
// Reasonably these could be refactored to one function with an opcode

__inline IrInstr *irMathsOp(IrBlock *block,
                   IrOpcode opcode,
                   IrValue *ir_result,
                   IrValue *left,
                   IrValue *right)
{
    IrInstr *instr = irInstrNew(opcode);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    irBlockAddInstruction(block, instr);
    return instr;
}

IrInstr *irFAdd(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_FADD, ir_result, left, right);
}

IrInstr *irIAdd(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_IADD, ir_result, left, right);
}

IrInstr *irFSub(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_FSUB, ir_result, left, right);
}

IrInstr *irISub(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_ISUB, ir_result, left, right);
}

IrInstr *irFMul(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_FMUL, ir_result, left, right);
}

IrInstr *irIMul(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_IMUL, ir_result, left, right);
}

IrInstr *irFDiv(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_FDIV, ir_result, left, right);
}

IrInstr *irSDiv(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_IDIV, ir_result, left, right);
}

IrInstr *irUDiv(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_UDIV, ir_result, left, right);
}

IrInstr *irSRem(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_IREM, ir_result, left, right);
}

IrInstr *irURem(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_UREM, ir_result, left, right);
}

IrInstr *irBitAnd(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_AND, ir_result, left, right);
}

IrInstr *irBitOr(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_OR, ir_result, left, right);
}

IrInstr *irBitNot(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_NOT, ir_result, ir_expr);
}

IrInstr *irXor(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_XOR, ir_result, left, right);
}

IrInstr *irSHL(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_SHL, ir_result, left, right);
}

IrInstr *irSHR(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_SHR, ir_result, left, right);
}

/* Compare two values that are integers and branch return a `List<IrInstr *>` */
void irBuildIntCompareAndBranch(IrFunction *func,
                                IrBlock *block,
                                IrValue *var,
                                IrValue *var2,
                                IrCmpKind cmp_kind,
                                IrBlock *true_block,
                                IrBlock *false_block)
{
    IrValue *result = irTmpVariable(IR_TYPE_I8);
    IrInstr *ir_cmp_instr = irInstrNew(IR_OP_ICMP);
    ir_cmp_instr->result = result;
    ir_cmp_instr->op1 = var;
    ir_cmp_instr->op2 = var2;
    ir_cmp_instr->extra.cmp_kind = cmp_kind;

    IrInstr *ir_branch = irInstrNew(IR_OP_BR);
    ir_branch->result = ir_cmp_instr->result;
    ir_branch->target_block = true_block;
    ir_branch->fallthrough_block = false_block;


    irBlockAddInstruction(block, ir_cmp_instr);
    irBlockAddInstruction(block, ir_branch);
    irFunctionAddMapping(func, block, true_block);
    irFunctionAddMapping(func, block, false_block);
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
        IrValue *bool_cond = irTmpVariable(IR_TYPE_I8);
        irICmp(block, bool_cond, IR_CMP_NE, cond, zero);
        cond = bool_cond;
    }

    IrInstr *instr = irInstrNew(IR_OP_BR);
    instr->result = cond;
    instr->target_block = true_block;
    instr->fallthrough_block = false_block;

    irBlockAddInstruction(block, instr);
    block->sealed = 1;

    irFunctionAddMapping(func, block, true_block);
    irFunctionAddMapping(func, block, false_block);

    return instr;
}

IrInstr *irJumpInternal(IrFunction *func,
                        IrBlock *block,
                        IrBlock *target,
                        IrOpcode opcode)
{
    if (!block || !target) {
        loggerPanic("NULL param\n");
    }

    if (block->sealed) {
        loggerWarning("Tried to add a jump to a sealed block: %d\n",
                block->id);
        return NULL;
    }

    IrInstr *instr = irInstrNew(opcode);
    instr->target_block = target;
    instr->fallthrough_block = NULL;

    /* Add to the current blocks instructions */
    irBlockAddInstruction(block, instr);

    /* This block is done */
    block->sealed = 1;

    /* Now update the control flow graph */
    irFunctionAddMapping(func, block, target);

    return instr;
}

IrInstr *irJump(IrFunction *func, IrBlock *block, IrBlock *target) {
    return irJumpInternal(func, block,target,IR_OP_JMP);
}

IrInstr *irLoop(IrFunction *func, IrBlock *block, IrBlock *target) {
    return irJumpInternal(func, block,target,IR_OP_LOOP);
}

IrInstr *irTmpGoto(IrCtx *ctx, aoStr *label) {
    IrInstr *ir_instr = irInstrNew(IR_OP_JMP);
    ir_instr->target_block = NULL;
    ir_instr->extra.unresolved_label = label;

    irBlockAddInstruction(ctx->current_block, ir_instr);

    List *list_node = ctx->current_block->instructions->prev;

    IrUnresolvedBlock *ir_unresolved_goto = irUnresolvedGotoNew(list_node,
                                                            ctx->current_block);
    /* There can be many different goto / source block combinations so 
     * we do not want a set. */
    ptrVecPush(ctx->unresolved_gotos, ir_unresolved_goto);

    return ir_instr;
}

IrInstr *irTmpGotoLabel(IrCtx *ctx, aoStr *label) {
    IrInstr *ir_instr = irInstrNew(IR_OP_LABEL);
    ir_instr->result = irValueNew(IR_TYPE_LABEL, IR_VALUE_UNRESOLVED);
    ir_instr->result->name = label;
    ir_instr->target_block = NULL;
    ir_instr->extra.unresolved_label = label;

    irBlockAddInstruction(ctx->current_block, ir_instr);
    List *list_node = ctx->current_block->instructions->prev;

    IrUnresolvedBlock *ir_unresolved_label = irUnresolvedLabelNew(list_node,
                                                            ctx->current_block);
    int ok = strMapAddAoStrOrErr(ctx->unresolved_labels,
                                 label,
                                 ir_unresolved_label);
    if (!ok) {
        loggerPanic("Label %s has already been seen in IR block %d\n", 
                label->data, ctx->current_block->id);
    }

    return ir_instr;
}

IrValue *irFnCall(IrCtx *ctx, IrFunction *func, Ast *ast) {
    IrValue *ir_call_args = irValueNew(IR_TYPE_ARRAY, IR_VALUE_UNRESOLVED);
    IrValue *ir_ret_val = irTmpVariable(irConvertType(ast->type));
    IrInstr *ir_call_instr = irInstrNew(IR_OP_CALL);

    if (ast->kind == AST_FUNPTR_CALL) {
        /* This needs to be an offset load */
        if (ast->ref && ast->ref->kind == AST_CLASS_REF) {
            /* This will load the function pointer from the class */
            IrValue *ir_expr = irExpression(ctx, func, ast->ref);
            ir_call_args->name = ir_expr->name;
        } else {
            IrValue *fn_ptr_var = irFunctionGetLocalFnPtr(func, ast);
            ir_call_args->name = fn_ptr_var->name;
        }
    } else if (ast->kind == AST_FUNCALL) {
        ir_call_args->name = ast->fname;
    } else if (ast->kind == AST_ASM_FUNCALL) {
        ir_call_args->name = ast->fname;
    }

    PtrVec *call_args = ptrVecNew();
    ir_call_instr->result = ir_ret_val;
    ir_call_instr->op1 = ir_call_args;
    ir_call_args->array_.values = call_args;

    if (ast->args) {
        for (int i = 0; i < ast->args->size; ++i) {
            Ast *ast_arg = vecGet(Ast *, ast->args, i);
            IrValue *ir_arg = irExpression(ctx, func, ast_arg);
            ptrVecPush(call_args, ir_arg);
        } 
    }

    listAppend(ctx->current_block->instructions, ir_call_instr);
    /* This is the return value of the function */
    return ir_ret_val;
}

/* Get Element Pointer */
IrValue *irLoadAddr(IrCtx *ctx, IrFunction *func, Ast *ast) {
    Ast *operand = ast->operand;
    IrValue *ir_dest = irTmpVariable(IR_TYPE_PTR);

    switch (operand->kind) {
        case AST_LVAR: {
            IrValue *ir_value = irFunctionGetLocal(func, operand);
            if (operand->type->kind == AST_TYPE_POINTER) {
                /* XXX: this feels extremely hacky */
                switch (operand->type->ptr->kind) {
                    case AST_TYPE_CLASS:
                        return irLoadClassRef(ctx, func, operand, operand->type, 0);
                    default: {
                        IrInstr *instr = irGetElementPointer(ctx->current_block,
                                                             ir_dest,
                                                             ir_value);
                        instr->flags |= IR_INSTR_LVAR_ADDR;
                        break;
                    }
                }
            } else {
                // irLoad(ctx->current_block, ir_dest, ir_value);
                IrInstr *instr = irGetElementPointer(ctx->current_block,
                                                     ir_dest,
                                                     ir_value);
                instr->flags |= IR_INSTR_LVAR_ADDR;
            }
            break;
        }

        case AST_GVAR: {
            IrValue *ir_value = irFunctionGetGlobal(func, operand);
            IrInstr *instr = irGetElementPointer(ctx->current_block,
                                                 ir_dest,
                                                 ir_value);
            instr->flags |= IR_INSTR_GVAR_ADDR;
            break;
        }

        case AST_CLASS_REF: {
            loggerWarning("AST_CLASS_REF -> ir bugged\n");
            return irLoadClassRef(ctx, func, operand->cls, operand->type, 0);
        }

        case AST_DEREF: {
            /* That is the class */
            Ast *cls = ast->operand;
            return irExpression(ctx, func, cls);
        }
        //if (ast->operand->kind == '+') {
        //    Ast *left = ast->operand->left;
        //    Ast *right = ast->operand->right;
        //    transpileAstInternal(left,ctx,indent);
        //    aoStrPutChar(buf, '[');
        //    transpileAstInternal(right,ctx,indent);
        //    aoStrPutChar(buf, ']');
        //} else {
        //    /* As `->` is a dereference we need to be able to distinguish 
        //     * between a class dereference and a general pointer dereference */
        //    if (ast->deref_symbol != TK_ARROW) {
        //        aoStrCatFmt(buf, "*");
        //    }
        //    transpileAstInternal(ast->operand,ctx,indent);
        //}
        //break;

        default:
            loggerPanic("Cannot turn Kind AST:%s %s into ir\n",
                    astKindToString(operand->kind),
                    astToString(ast));
    }

    return ir_dest;
}

IrValue *irAssignClassRef(IrCtx *ctx, IrFunction *func, Ast *cls, AstType *field, IrValue *rhs, int offset) {
    switch (cls->kind) {
        case AST_LVAR: {
            int total_offset = cls->type->offset + field->offset + offset;
            /* Load the offset into a variable and then assign */
            IrValue *ir_local = irFunctionGetLocal(func, cls);
            /* @GEP fixing */
            IrValue *ir_dest = irTmpVariable(irConvertType(field));
            IrInstr *instr = irGetElementPointerAtIdx(ctx->current_block,
                                                      ir_dest,
                                                      ir_local,
                                                      total_offset);
            instr->flags |= IR_INSTR_STACK_CLASS;
            irStore(ctx->current_block,ir_dest,rhs);
            return ir_dest;
        }

        case AST_GVAR:
            // loggerPanic("Global variables unimplemented: %s\n", astToString(cls));
            // total_offset = field->offset + offset;
            // asmGSave(buf,cls->clsname->data,field,total_offset);
            break;

        case AST_CLASS_REF: {
            Ast *node = cls;
            int field_offset = 0;
            while (node && node->kind != AST_LVAR) {
                field_offset += node->type->offset;
                node = node->cls;
            }

            IrValue *ir_expr = irFunctionGetLocal(func, node);
            IrValue *ir_dest = irTmpVariable(ir_expr->type);
            IrInstr *instr = irGetElementPointerAtIdx(ctx->current_block,
                             ir_dest,
                             ir_expr,
                             field_offset);
            instr->flags |= IR_INSTR_STACK_CLASS;
            irStore(ctx->current_block,ir_dest,rhs);
            return ir_dest;
        }

        case AST_DEREF: {
            Ast *node = cls;
            int field_offset = field->offset;
            while (node && node->kind != AST_LVAR) {
                field_offset += node->type->offset;
                node = node->cls;
            }

            //IrValue *ir_expr = irExpression(ctx, func, cls->operand);
            IrValue *ir_expr = irFunctionGetLocal(func, node);
            IrValue *ir_dest = irTmpVariable(ir_expr->type);
            debug("ASSIGN CLASS DEREF: %s %d\n",irValueTypeToString(ir_dest->type), field_offset);
            IrInstr *instr = irGetElementPointerAtIdx(ctx->current_block,
                                                      ir_dest,
                                                      ir_expr,
                                                      field_offset);
            instr->flags |= IR_INSTR_HEAP_CLASS;
            irStore(ctx->current_block,ir_dest,rhs);
            return ir_dest;
        }
        default:
            loggerPanic("Failed to create ASM for: %s\n",
                    astToString(cls));
    }
    loggerPanic("Unimplemented %s\n", astKindToString(cls->kind));
    return NULL;
}

IrValue *irAssign(IrCtx *ctx, IrFunction *func, Ast *ast) {
    switch (ast->left->kind) {
        case AST_LVAR: {
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_local = irFunctionGetLocal(func, ast->left);
            irStore(ctx->current_block, ir_local, rhs);
            /* Assignments return the value */
            return rhs;
        }

        /* @Bug store the declvar in the parser not the whole AST for a 
         * default param */
        case AST_DEFAULT_PARAM: {
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_local = irFunctionGetLocal(func, ast->left->declvar);
            irStore(ctx->current_block, ir_local, rhs);
            /* Assignments return the value */
            return rhs;
        }

        case AST_GVAR: {
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_global = irFunctionGetGlobal(func, ast->left);
            irStore(ctx->current_block, ir_global, rhs);
            return rhs; 
        }

        case AST_DEREF: {
            debug("assigning to a deref\n");
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_local = irFunctionGetLocal(func, ast->left->left);
            // IrValue *ptr = irExpression(ctx, func, ast->left->left);
            irStore(ctx->current_block, ir_local, rhs);
            return rhs;
        }

        case AST_CLASS_REF: {
            IrValue *rhs = irExpression(ctx, func, ast->right);
            return irAssignClassRef(ctx, func, ast->left->cls,
                                    ast->left->type, rhs, 0);
        }

        case AST_FUNPTR: {
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_local = irFunctionGetLocalFnPtr(func, ast->left);
            irStore(ctx->current_block, ir_local, rhs);
            /* Assignments return the value */
            return rhs;
        }

        default: {
            loggerPanic("Unsupported LHS assignment %s %s\n",
                    astKindToString(ast->left->kind),
                    astToString(ast->left));

        }
    }
}

IrValue *irLoadClassRef(IrCtx *ctx,
                        IrFunction *func,
                        Ast *cls,
                        AstType *field,
                        int offset)
{
    switch (cls->kind) {
        case AST_LVAR: {
            /*AstType *cls_type = cls->type;
            *if (cls_type->kind == AST_TYPE_POINTER) {
            *    cls_type = cls->type->ptr;
            */
            IrValueType ir_dest_type = irConvertType(field);
            IrValue *ir_dest = irTmpVariable(ir_dest_type);
            //IrValue *ir_dest = irTmpVariable(IR_TYPE_PTR);
            IrValue *ir_local = irFunctionGetLocal(func, cls);
            /* @GEP fixing*/
            IrInstr *instr = irGetElementPointerAtIdx(ctx->current_block,
                                           ir_dest,
                                           ir_local,
                                           cls->type->offset + field->offset + offset);
            instr->flags |= IR_INSTR_STACK_CLASS;
            return ir_dest;
        }

        /* Call function again, this would be a nested struct or a union */
        case AST_CLASS_REF: {
            Ast *node = cls;
            int field_offset = 0;
            while (node && node->kind != AST_LVAR) {
                field_offset += node->type->offset;
                node = node->cls;
            }
            IrValue *ir_expr = irFunctionGetLocal(func, node);
            IrValue *ir_dest = irTmpVariable(ir_expr->type);
            IrInstr *instr = irGetElementPointerAtIdx(ctx->current_block,
                             ir_dest,
                             ir_expr,
                             field_offset);
            instr->flags |= IR_INSTR_STACK_CLASS;
            return ir_dest;
        }

        /* Load the `->` dereference */
        case AST_DEREF: {
            Ast *node = cls;
            int field_offset = field->offset;
            while (node && node->kind != AST_LVAR) {
                field_offset += node->type->offset;
                node = node->cls;
            }

            IrValue *ir_expr = irFunctionGetLocal(func, node);
            //IrValue *ir_expr = irExpression(ctx, func, cls->operand);
            IrValue *ir_dest = irTmpVariable(irConvertType(field));
            debug("Load CLASS DEREF: %s %d\n",irValueTypeToString(ir_dest->type), field_offset);
            IrInstr *instr = irGetElementPointerAtIdx(ctx->current_block,
                                                      ir_dest,
                                                      ir_expr,
                                                      field_offset);
            instr->flags |= IR_INSTR_HEAP_CLASS;
            return ir_dest;
        }

        case AST_GVAR:
        default:
            loggerPanic("Failed to create IR for: %s\n", astKindToString(cls->kind));

    }
}

/* Actually... we should push all values to one vector and then have the 
 * shape of the vector as different properties on the struct */
void irArrayInit(IrCtx *ctx, IrFunction *func, Ast *ast) {
    int dimension_size = ast->arrayinit->size;
    Ast *array_value = NULL;

    /* Eventually that will be correct */
    ctx->array_.length_per_array = dimension_size;

    for (int i = 0; i < ast->arrayinit->size; ++i) {
        array_value = (Ast *)ast->arrayinit->entries[i];
        IrValue *ir_value = irExpression(ctx, func, array_value);
        if (ir_value) {
            ctx->array_.type = ir_value->type;
            ptrVecPush(ctx->array_.init, ir_value);
        }
    }
}

IrValue *irExpression(IrCtx *ctx, IrFunction *func, Ast *ast) {
    IrBlock *ir_block = ctx->current_block;
    if (!ast) return NULL;

    switch (ast->kind) {
        case AST_LITERAL: {
            switch (ast->type->kind) {
                case AST_TYPE_INT:
                case AST_TYPE_CHAR:
                    return irConstInt(irConvertType(ast->type), ast->i64);
                case AST_TYPE_FLOAT: {
                    IrValue *value = irConstFloat(IR_TYPE_F64, ast->f64);
                    irAddConstFloat(ctx->ir_program, value);
                    return value;
                }
                default:
                    loggerPanic("Unknown literal: %s\n",
                             astKindToString(ast->type->kind));
            }
            break;
        }
        
        case AST_STRING: {
            return irGlobalString(func->program, ast);
        }

        case AST_DEFAULT_PARAM: {
            Ast *declvar = ast->declvar;
            IrValue *local_var = irFunctionGetLocal(func, declvar);
            if (!local_var) {
                loggerPanic("Variable %s not found\n", astToString(ast));
            }

            IrValueType ir_value_type = irConvertType(declvar->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, local_var);
            return ir_load_dest;
        }

        case AST_LVAR: {
            IrValue *local_var = irFunctionGetLocal(func, ast);
            if (!local_var) {
                loggerPanic("Variable %s not found\n", astToString(ast));
            }

            IrValueType ir_value_type = irConvertType(ast->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);

            if (irIsStruct(local_var->type)) {
                // irGetElementPointer(ir_block, ir_load_dest, local_var);
            } else {
                irLoad(ir_block, ir_load_dest, local_var);
            }

            return ir_load_dest;
        }

        case AST_GVAR: {
            IrValue *global_var = irFunctionGetGlobal(func, ast);
            if (!global_var) {
                loggerPanic("Variable %s not found\n", astToString(ast));
            }

            IrValueType ir_value_type = irConvertType(ast->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, global_var);
            return ir_load_dest;
        }

        /* @TODO: Check classes and arrays */
        case AST_DEREF: {
            loggerDebug("DEREF\n");
            IrValue *ir_ptr = irExpression(ctx, func, ast->operand);

            if (ir_ptr->type != IR_TYPE_PTR) {
                loggerPanic("Attempted to dereference a non-pointer: %s\n",
                            astToString(ast->left));
            }

            /* Dereferencing an Ast node is `type->ptr`, to get what the type
             * is pointing to */
            IrValueType ir_deref_type = irConvertType(ast->operand->type->ptr);
            IrValue *ir_tmp_var = irTmpVariable(ir_deref_type);
            irLoad(ir_block, ir_tmp_var, ir_ptr);
            return ir_tmp_var;
        }

        case AST_CLASS_REF: {
            IrValue *ir_value = irLoadClassRef(ctx, func, ast->cls, ast->type, 0); 
            return ir_value;
        }

        case AST_ADDR: {
            IrValue *dest = irLoadAddr(ctx, func, ast);
            return dest;
        }

        case AST_ASM_FUNCDEF:
        case AST_ASM_FUNC_BIND: {
            IrValue *fn = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_GLOBAL);
            fn->name = ast->asmfname;

            IrValueType ir_value_type = irConvertType(ast->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, fn);
            /* @Tracking Update where the variable is? */
            strMapAddAoStr(func->variables,ast->fname,ir_load_dest);
            return ir_load_dest;
        }
    
        /* We hit this when we load function arguments. NOT for anything else */
        case AST_EXTERN_FUNC:
        case AST_FUNC: {
            IrValue *fn = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_GLOBAL);  // irFunctionGetLocalFnPtr(func, ast);
            fn->name = ast->fname;

            IrValueType ir_value_type = irConvertType(ast->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, fn);
            /* @Tracking Update where the variable is? */
            strMapAddAoStr(func->variables,ast->fname,ir_load_dest);
            return ir_load_dest;
        }

        case AST_FUNPTR: {
            IrValue *local_fnptr = irFunctionGetLocalFnPtr(func, ast);

            if (!local_fnptr) {
                char *keys = strMapKeysToString(func->variables);
                loggerPanic("func %s Variable %s not found keys = %s\n",
                            func->name->data,
                            ast->tmp_fnptr_name->data, keys);
            }

            IrValueType ir_value_type = irConvertType(ast->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, local_fnptr);
            /* @Tracking Update where the variable is? */
            strMapAddAoStr(func->variables,ast->tmp_fnptr_name,ir_load_dest);
            return ir_load_dest;
        }

        case '=': {
            return irAssign(ctx, func, ast);
        }

        case TK_MOD_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            if (irIsFloat(irConvertType(ast->type))) {
                irFDiv(ir_block, ir_result, lhs, rhs);
            } else {
                if (ast->type->issigned) {
                    irSRem(ctx->current_block, ir_result, lhs, rhs);
                } else {
                    irURem(ctx->current_block, ir_result, lhs, rhs);
                }
            }
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_MUL_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            if (irIsFloat(irConvertType(ast->type))) {
                irFMul(ir_block, ir_result, lhs, rhs);
            } else {
                irIMul(ir_block, ir_result, lhs, rhs);
            }
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_DIV_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            if (irIsFloat(irConvertType(ast->type))) {
                irFDiv(ir_block, ir_result, lhs, rhs);
            } else {
                if (ast->type->issigned) {
                    irSDiv(ctx->current_block, ir_result, lhs, rhs);
                } else {
                    irUDiv(ctx->current_block, ir_result, lhs, rhs);
                }
            }
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_SUB_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            if (irIsFloat(irConvertType(ast->type))) {
                irFSub(ir_block, ir_result, lhs, rhs);
            } else {
                irISub(ir_block, ir_result, lhs, rhs);
            }
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_ADD_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            if (irIsFloat(irConvertType(ast->type))) {
                irFAdd(ir_block, ir_result, lhs, rhs);
            } else {
                irIAdd(ir_block, ir_result, lhs, rhs);
            }
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_XOR_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            irXor(ir_block, ir_result, lhs, rhs);
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_AND_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            irBitAnd(ir_block, ir_result, lhs, rhs);
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_OR_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            irBitOr(ir_block, ir_result, lhs, rhs);
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_SHR_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            irSHR(ir_block, ir_result, lhs, rhs);
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_SHL_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            irSHL(ir_block, ir_result, lhs, rhs);
            irAssign(ctx, func, ast);
            return ir_result;
        }

        /* @TODO: Pre and post increment/decrement
         * Not sure what the difference in IR is yet and this is functional
         * for both. This should be split out when I've figured that out */
        case TK_PLUS_PLUS:
        case TK_PRE_PLUS_PLUS:
        case TK_MINUS_MINUS:
        case TK_PRE_MINUS_MINUS: {
            IrValue *ir_var =  NULL;
            if (ast->operand->kind == AST_LVAR) {
                ir_var = irFunctionGetLocal(func, ast->operand);
            } else if (ast->operand->kind == AST_GVAR) {
                ir_var = irFunctionGetGlobal(func, ast->operand);
            } else if (ast->operand->kind == AST_DEREF ||
                       ast->operand->kind == AST_CLASS_REF) {
                /* This would be the result of derefencing, for example, a 
                 * struct member, pointer ...*/
                ir_var = irExpression(ctx, func, ast->operand);
            } else if (ast->operand->kind == AST_DEFAULT_PARAM) {
                /* @Bug
                 * The parser should just save the declvar not the whole 
                 * Ast. */
                ir_var = irFunctionGetLocal(func, ast->operand->declvar);
            } else {
                loggerPanic("Unsupported LHS assignment %s\n",
                        astKindToString(ast->operand->kind));
            }

            IrValueType ir_value_type = irConvertType(ast->operand->type);
            /* Function pointer which allows us to merge both implementations, 
             * PRE should minimally be split from POST increment/decrement */
            IrInstr *(*operator)(IrBlock *block, IrValue *ir_result,
                                 IrValue *left, IrValue *right) = NULL;
            int is_sub = ast->kind == TK_MINUS_MINUS ||
                         ast->kind == TK_PRE_MINUS_MINUS;

            IrValue *ir_size = NULL;
            if (irIsInt(ir_value_type)) {
                ir_size = irConstInt(ir_value_type, 1);
                if (is_sub) operator = &irISub;
                else        operator = &irIAdd;
            } else if (irIsFloat(ir_value_type)) {
                ir_size = irConstFloat(ir_value_type, 1.0);
                if (is_sub) operator = &irFSub;
                else        operator = &irFAdd;
            } else {
                ir_size = irConstInt(ir_value_type, ast->operand->type->size);
                if (is_sub) operator = &irISub;
                else        operator = &irIAdd;
            }

            IrValue *ir_result = irTmpVariable(ir_value_type);
            operator(ctx->current_block, ir_result,ir_var,ir_size);
            irStore(ctx->current_block, ir_var, ir_result);
            return ir_result;
        }

        case '+': {
            if (ast->right == NULL) {
                /* Unary plus - just return the operand */
                return irExpression(ctx, func, ast->left);
            } else {
                /* Binary addition */
                IrValue *left = irExpression(ctx, func, ast->left);
                IrValue *right = irExpression(ctx, func, ast->right);
    
                IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
                if (ast->type->kind == AST_TYPE_FLOAT) {
                    irFAdd(ctx->current_block, ir_result, left, right);
                } else {
                    irIAdd(ctx->current_block, ir_result, left, right);
                }
                return ir_result;
            }
        }

        case '-': {
            if (ast->right == NULL) {
                /* Unary negation */
                IrValue *ir_expr = irExpression(ctx, func, ast->left);
                IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

                if (ast->left->type->kind == AST_TYPE_FLOAT) {
                    irFNeg(ctx->current_block, ir_result, ir_expr);
                } else {
                    irINeg(ctx->current_block, ir_result, ir_expr);
                }
                return ir_result;
            } else {
                /* Binary subtraction */
                IrValue *left = irExpression(ctx, func, ast->left);
                IrValue *right = irExpression(ctx, func, ast->right);
                
                IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
                if (ast->type->kind == AST_TYPE_FLOAT) {
                    irFSub(ctx->current_block, ir_result, left, right);
                } else {
                    irISub(ctx->current_block, ir_result, left, right);
                }
                return ir_result;
            }
        }

        case '*': {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            if (ast->type->kind == AST_TYPE_FLOAT) {
                irFMul(ctx->current_block, ir_result, left, right);
            } else {
                irIMul(ctx->current_block, ir_result, left, right);
            }
            return ir_result;
        }

        case '/': {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);

            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            if (ast->type->kind == AST_TYPE_FLOAT) {
                irFDiv(ctx->current_block, ir_result, left, right);
            } else if (ast->type->issigned) {
                irSDiv(ctx->current_block, ir_result, left, right);
            } else {
                irUDiv(ctx->current_block, ir_result, left, right);
            }
            return ir_result;
        }
        
        case '%': {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            if (ast->type->issigned) {
                irSRem(ctx->current_block, ir_result, left, right);
            } else {
                irURem(ctx->current_block, ir_result, left, right);
            }
            return ir_result;
        }

        case '^': {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            irXor(ctx->current_block, ir_result, left, right);
            return ir_result;
        }

        case '~': {
            IrValue *ir_expr = irExpression(ctx, func, ast->left);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            irBitNot(ctx->current_block, ir_result, ir_expr);
            return ir_result;
        }

        case '&': {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            irBitAnd(ctx->current_block, ir_result, left, right);
            return ir_result;
        }

        case '|': {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            irBitOr(ctx->current_block, ir_result, left, right);
            return ir_result;
        }

        case TK_SHL: {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            irSHL(ctx->current_block, ir_result, left, right);
            return ir_result;
        }

        case TK_SHR: {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            irSHR(ctx->current_block, ir_result, left, right);
            return ir_result;
        }

        case TK_AND_AND: {
            IrBlock *ir_block = ctx->current_block;
            IrBlock *ir_right_block = irBlockNew(irBlockId());
            IrBlock *ir_end_block = irBlockNew(irBlockId());

            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *ir_result = irTmpVariable(IR_TYPE_I8);

            irBranch(func, ir_block, left, ir_right_block, ir_end_block);
            irFunctionAddBlock(func, ir_right_block);
            irCtxSetCurrentBlock(ctx, ir_right_block);

            IrValue *right = irExpression(ctx, func, ast->right);

            irJump(func, ctx->current_block, ir_end_block);
            irCtxSetCurrentBlock(ctx, ir_end_block);
            irFunctionAddBlock(func, ir_end_block);

            IrInstr *phi_instr = irPhi(ir_end_block, ir_result);
            irAddPhiIncoming(phi_instr, irConstInt(IR_TYPE_I8, 0), ir_block);
            irAddPhiIncoming(phi_instr, right, ir_right_block);
            return ir_result;
        }

        case TK_OR_OR: {
            IrBlock *ir_block = ctx->current_block;
            IrBlock *ir_right_block = irBlockNew(irBlockId());
            IrBlock *ir_end_block = irBlockNew(irBlockId());

            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *ir_result = irTmpVariable(IR_TYPE_I8);

            irBranch(func, ir_block, left, ir_end_block, ir_right_block);
            irFunctionAddBlock(func, ir_right_block);
            irCtxSetCurrentBlock(ctx, ir_right_block);

            IrValue *right = irExpression(ctx, func, ast->right);

            irJump(func, ctx->current_block, ir_end_block);
            irCtxSetCurrentBlock(ctx, ir_end_block);
            irFunctionAddBlock(func, ir_end_block);

            IrInstr *phi_instr = irPhi(ir_end_block, ir_result);
            irAddPhiIncoming(phi_instr, irConstInt(IR_TYPE_I8, 1), ir_block);
            irAddPhiIncoming(phi_instr, right, ir_right_block);
            return ir_result;
        }

        case '!': {
            IrValue *ir_expr = irExpression(ctx, func, ast->left);
            IrValue *ir_result = irTmpVariable(IR_TYPE_I8);

            /* Create NOT operation - a comparison against 0 */
            IrValue *zero = irConstInt(ir_expr->type, 0);
            irICmp(ctx->current_block, ir_result, IR_CMP_EQ, ir_expr, zero);

            return ir_result;
        }

        case '<': 
        case '>': 
        case TK_LESS_EQU:
        case TK_GREATER_EQU: 
        case TK_EQU_EQU: 
        case TK_NOT_EQU: {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            
            IrValue *ir_result = irTmpVariable(IR_TYPE_I8);
            IrCmpKind cmp_kind;
            switch (ast->kind) {
                case '<': {
                    cmp_kind = ast->type->issigned ? IR_CMP_LT : IR_CMP_ULT ;
                    break;
                }
                case '>': {
                    cmp_kind = ast->type->issigned ? IR_CMP_GT : IR_CMP_UGT;
                    break;
                }
                case TK_LESS_EQU: {
                    cmp_kind = ast->type->issigned ? IR_CMP_LE : IR_CMP_ULE;
                    break;
                }
                case TK_GREATER_EQU: {
                    cmp_kind = ast->type->issigned ? IR_CMP_GE : IR_CMP_UGE;
                    break;
                }
                case TK_EQU_EQU: {
                    cmp_kind = IR_CMP_EQ;
                    break;
                }
                case TK_NOT_EQU: {
                    cmp_kind = IR_CMP_NE;
                    break;
                }
                /* Should not be reached */
                default: {
                    cmp_kind = IR_CMP_EQ;
                    break;
                }
            }
            
            if (ast->left->type->kind == AST_TYPE_FLOAT) {
                irFCmp(ctx->current_block, ir_result, cmp_kind, left, right);
            } else {
                irICmp(ctx->current_block, ir_result, cmp_kind, left, right);
            }
            return ir_result;
        }

        case AST_ASM_FUNCALL:
        case AST_FUNPTR_CALL:
        case AST_FUNCALL: {
            return irFnCall(ctx, func, ast);
        }

        /* Start a basic block */
        case AST_LABEL: {
            IrBlock *ir_block = irBlockNew(irBlockId());
            irJump(func, ctx->current_block, ir_block);
            irCtxSetCurrentBlock(ctx, ir_block);
            irFunctionAddBlock(func, ir_block);
            IrInstr *ir_instr = irTmpGotoLabel(ctx, ast->slabel);
            return ir_instr->result;
        }

        /* Ends a basic block */
        case AST_GOTO: {
            /* If the block is completely empty then we can just reuse it 
             * else we should add a new block */
            irTmpGoto(ctx, ast->slabel);
            IrBlock *ir_block = irBlockNew(irBlockId());
            irCtxSetCurrentBlock(ctx, ir_block);
            irFunctionAddBlock(func, ir_block);
            return NULL;
        }

        case AST_ARRAY_INIT: {
            /* If we reach here we are nesting an array */
            ctx->array_.nesting++;
            irArrayInit(ctx, func, ast);
            break;
        }

        case AST_CAST: {
            /* Skip no-op casts if types are the same */
            AstType *from_type = ast->operand->type;
            AstType *to_type = ast->type;
            IrValue *expr = irExpression(ctx, func, ast->operand);


            if (to_type->kind == from_type->kind) {
                return expr;
            }

            IrValueType ir_to_type = irConvertType(to_type);
            IrValue *result = irTmpVariable(ir_to_type);
            /* @Bug
             * We only use classes as 'intrinsics' that are integers at the 
             * moment however this should be expanded to accept floats */
            int types_are_ints = (astIsIntType(from_type) || 
                                  from_type->is_intrinsic) &&
                                 (astIsIntType(to_type) || 
                                  to_type->is_intrinsic);

            if (types_are_ints) {
                if (from_type->size < to_type->size) {
                    if (to_type->issigned) {
                        irSExt(ctx->current_block, result, expr);  /* Sign extend */
                    } else {
                        irZExt(ctx->current_block, result, expr);  /* Zero extend */
                    }
                } else if (from_type->size > to_type->size) {
                    irTrunc(ctx->current_block, result, expr); /* Larger to smaller int */
                } else {
                    irBitCast(ctx->current_block, result, expr); /* Change signedness */
                }
            }
            /* Floating points casts */
            else if (astIsFloatType(from_type) && astIsFloatType(to_type)) {
                /* HolyC only has F64, though this is for completeness */
                if (from_type->size < to_type->size) {
                    /* floating point extend */
                    irFPExt(ctx->current_block, result, expr);
                } else {
                    /* floating point truncate */
                    irFPTrunc(ctx->current_block, result, expr);
                }
            }
            /* Integer-to-float casts */
            else if (astIsIntType(from_type) && astIsFloatType(to_type)) {
                if (from_type->issigned) {
                    /* Signed int to float */
                    irSIToFP(ctx->current_block, result, expr);
                } else {
                    /* unsigned int to float */
                    irUIToFP(ctx->current_block, result, expr);
                }
            }
            /* Floating point to integer casts */
            else if (astIsFloatType(from_type) && astIsIntType(to_type)) {
                if (to_type->issigned) {
                    /* Float to signed int */
                    irFPToSI(ctx->current_block, result, expr);
                } else {
                    /* Float to usigned int */
                    irFPToUI(ctx->current_block, result, expr);
                }
            }
            /* Pointer casts */
            else if (from_type->kind == to_type->kind) { /* @Unreached? */
                /* Pointer to pointer... no op? */
                irBitCast(ctx->current_block, result, expr);
            }
            else if (from_type->kind == AST_TYPE_POINTER && astIsIntType(to_type)) {
                /* Pointer to integer */
                irPtrToInt(ctx->current_block, result, expr);
            }
            else if (astIsIntType(from_type) && to_type->kind == AST_TYPE_POINTER) {
                /* Integer to pointer  */
                irIntToPtr(ctx->current_block, result, expr);
            } else {
                loggerPanic("Unsupported cast: %s to %s", 
                        astTypeToString(from_type),
                        astTypeToString(to_type));
            }

            return result;
        }

        case AST_COMPOUND_STMT: {
            listForEach(ast->stms) {
                Ast *next = (Ast *)it->value;
                irStatement(ctx, func, next);
            }
            break;
        }

        default:
            loggerPanic("Error with function '%s' Unhandled Ast kind: %s %s\n",
                    func->name->data,
                    astKindToString(ast->kind),
                    astToString(ast));
    }

    return NULL;
}

void irStatement(IrCtx *ctx, IrFunction *func, Ast *ast) {
    if (!ast) return;

    switch (ast->kind) {
        case AST_COMPOUND_STMT: {
            listForEach(ast->stms) {
                Ast *next = (Ast *)it->value;
                irStatement(ctx, func, next);
                /* We cannot do this because of unresolved gotos that may need 
     * to jump to this block vvvvv */
                //if (ctx->current_block->sealed) break;
            }
            break;
        }

        case AST_IF: {
            /* This creates a compare and then a branch */
            IrValue *ir_cond = irExpression(ctx, func, ast->cond);
            IrBlock *ir_then = irBlockNew(irBlockId());
            IrBlock *ir_else = ast->els ? irBlockNew(irBlockId()) : NULL;
            IrBlock *ir_end_block = irBlockNew(irBlockId());

            irFunctionAddBlock(func, ir_then);

            if (ir_else) {
                irBranch(func, ctx->current_block, ir_cond, ir_then, ir_else);
            } else {
                irBranch(func, ctx->current_block, ir_cond, ir_then, ir_end_block);
            }

            irCtxSetCurrentBlock(ctx, ir_then);

            irStatement(ctx, func, ast->then);
            if (!ctx->current_block->sealed) {
                irJump(func, ctx->current_block, ir_end_block);
            }

            if (ir_else) {
                irFunctionAddBlock(func, ir_else);
                irCtxSetCurrentBlock(ctx, ir_else);
                irStatement(ctx, func, ast->els);
                if (!ctx->current_block->sealed) {
                    irJump(func, ctx->current_block, ir_end_block);
                }
            }

            irFunctionAddBlock(func, ir_end_block);

            /* Update the current block we are pointing to */
            irCtxSetCurrentBlock(ctx, ir_end_block);
            break;
        }

        /* LOOP IR start =====================================================*/
        case AST_WHILE: {
            IrBlock *ir_while_cond = irBlockNew(irBlockId());
            IrBlock *ir_while_body = irBlockNew(irBlockId());
            IrBlock *ir_while_end = irBlockNew(irBlockId());
            IrBlock *ir_previous_end_block = ctx->end_block;
            IrBlock *ir_previous_head_block = ctx->loop_head_block;
            unsigned long ctx_prev_flags = ctx->flags;
            ctx->flags |= IR_CTX_FLAG_IN_LOOP;

            /* Update the current end block */
            irCtxSetLoopHeadBlock(ctx, ir_while_cond);
            irCtxSetEndBlock(ctx, ir_while_end);

            irJump(func, ctx->current_block, ir_while_cond);

            irCtxSetCurrentBlock(ctx, ir_while_cond);
            irFunctionAddBlock(func, ir_while_cond);

            IrValue *ir_while_cond_expr = irExpression(ctx, func, ast->whilecond);
            irBranch(func,
                     ctx->current_block, 
                     ir_while_cond_expr,
                     ir_while_body, 
                     ir_while_end);

            irCtxSetCurrentBlock(ctx, ir_while_body);

            irFunctionAddBlock(func, ir_while_body);
            irStatement(ctx, func, ast->whilebody);

            if (!ctx->current_block->sealed) {
                irLoop(func, ctx->current_block, ir_while_cond);
            }
            irFunctionAddBlock(func, ir_while_end);
            irCtxSetCurrentBlock(ctx, ir_while_end);
            ctx->flags = ctx_prev_flags;
            /* We are now out of the scope of the while loop so reset the 
             * end block to what it was previously */
            irCtxSetEndBlock(ctx, ir_previous_end_block);
            irCtxSetLoopHeadBlock(ctx, ir_previous_head_block);
            break;
        }

        case AST_DO_WHILE: {
            /**
             * jump to body
             * Body block
             * 
             * jump to condition
             * Condition block
             * jump to body or end;
             *
             * End block
             */
            IrBlock *ir_dowhile_body = irBlockNew(irBlockId());
            IrBlock *ir_dowhile_cond = irBlockNew(irBlockId());
            IrBlock *ir_dowhile_end = irBlockNew(irBlockId());

            IrBlock *ir_previous_end_block = ctx->end_block;
            IrBlock *ir_previous_head_block = ctx->loop_head_block;

            unsigned long ctx_prev_flags = ctx->flags;
            ctx->flags |= IR_CTX_FLAG_IN_LOOP;

            /* this seems weird as a continue would not hit the condition 
             * which is what would happen with a for loop. Though this is 
             * "correct" it simply seems a bit odd */
            irCtxSetLoopHeadBlock(ctx, ir_dowhile_body);
            irCtxSetEndBlock(ctx, ir_dowhile_end);

            /* Jump into the body and prepare IR for the body */
            irJump(func, ctx->current_block, ir_dowhile_body);
            irCtxSetCurrentBlock(ctx, ir_dowhile_body);
            irFunctionAddBlock(func, ir_dowhile_body);
            irStatement(ctx, func, ast->whilebody);

            /* Jump into the conditio and prepare IR for the branch */
            irJump(func, ctx->current_block, ir_dowhile_cond);
            irCtxSetCurrentBlock(ctx, ir_dowhile_cond);
            irFunctionAddBlock(func, ir_dowhile_cond);
            IrValue *ir_dowhile_cond_expr = irExpression(ctx, func,
                                                         ast->whilecond);
            irBranch(func,
                     ctx->current_block,
                     ir_dowhile_cond_expr,
                     ir_dowhile_body,
                     ir_dowhile_end);
            irLoop(func, ctx->current_block, ir_dowhile_body);

            irFunctionAddBlock(func, ir_dowhile_end);
            irCtxSetCurrentBlock(ctx, ir_dowhile_end);

            /* Restore previous state */
            ctx->flags = ctx_prev_flags;
            irCtxSetEndBlock(ctx, ir_previous_end_block);
            irCtxSetLoopHeadBlock(ctx, ir_previous_head_block);
            break;
        }

        case AST_FOR: {
            IrBlock *ir_for_cond = ast->forcond ? irBlockNew(irBlockId()) : NULL;
            IrBlock *ir_for_body = irBlockNew(irBlockId());
            IrBlock *ir_for_step = ast->forstep ? irBlockNew(irBlockId()) : NULL;
            IrBlock *ir_for_head = ir_for_cond ? ir_for_cond : ir_for_body;
            IrBlock *ir_for_end = irBlockNew(irBlockId());

            IrBlock *ir_previous_end_block = ctx->end_block;
            IrBlock *ir_previous_head_block = ctx->loop_head_block;

            unsigned long ctx_prev_flags = ctx->flags;
            ctx->flags |= IR_CTX_FLAG_IN_LOOP;

            /* Initaliser belongs to the current block */
            if (ast->forinit) {
                irStatement(ctx, func, ast->forinit);
            }

            /* Update the current end block */
            irCtxSetLoopHeadBlock(ctx, ir_for_cond ? ir_for_cond : ir_for_body);
            irCtxSetEndBlock(ctx, ir_for_end);

            if (ir_for_cond) {
                irJump(func, ctx->current_block, ir_for_cond);
                irCtxSetCurrentBlock(ctx, ir_for_cond);
                irFunctionAddBlock(func, ir_for_cond);

                IrValue *ir_for_cond_expr = irExpression(ctx, func, ast->forcond);
                irBranch(func,
                         ctx->current_block, 
                         ir_for_cond_expr,
                         ir_for_body, 
                         ir_for_end);
            }

            irCtxSetCurrentBlock(ctx, ir_for_body);

            irFunctionAddBlock(func, ir_for_body);
            irStatement(ctx, func, ast->forbody);

            /* If there is a step we want to add it to the current block */
            if (ir_for_step) {
                irJump(func, ctx->current_block, ir_for_step);
                irCtxSetCurrentBlock(ctx, ir_for_step);
                irFunctionAddBlock(func, ir_for_step);
                irExpression(ctx, func, ast->forstep);

                irLoop(func, ctx->current_block, ir_for_head);
            } else {
                if (!ctx->current_block->sealed) {
                    irLoop(func, ctx->current_block, ir_for_head);
                }
            }

            irFunctionAddBlock(func, ir_for_end);
            irCtxSetCurrentBlock(ctx, ir_for_end);

            /* Restore */
            ctx->flags = ctx_prev_flags;
            irCtxSetEndBlock(ctx, ir_previous_end_block);
            irCtxSetLoopHeadBlock(ctx, ir_previous_head_block);
            break;
        }
        /* LOOP IR end =======================================================*/

        case AST_CONTINUE: {
            if (ctx->flags & IR_CTX_FLAG_IN_LOOP) {
                irJump(func, ctx->current_block, ctx->loop_head_block);
            } else {
                loggerPanic("Continue found outside of loop\n");
            }
            break;
        }

        case AST_SWITCH: {
            unsigned long prev_flags = ctx->flags;
            IrBlock *ir_prev_end_block = ctx->end_block;

            ctx->flags |= IR_CTX_FLAG_IN_SWITCH;

            /* Evaluate the switch condition `switch(<condition>)`, as we may 
             * need to do a bounds check we create the instruction after 
             * the possible bounds check but need the Value ahead of time */
            IrValue *switch_var = irExpression(ctx, func, ast->switch_cond);
            /* The block passsed the switch */
            IrBlock *end_block = irBlockNew(irBlockId());
            IrBlock *default_block = ast->case_default ? irBlockNew(irBlockId()) : NULL;
            IrBlock *out_of_bounds_block = default_block ? default_block : end_block;

            /* We want to use the orderd jump table */
            Ast **jump_table = ast->jump_table_order;
            int jump_table_size = ast->cases->size;

            /* If we have bounds checking we want to make sure that we are in
             * range and either jump to the `default` case if there is one or 
             * jump passed the switch */
            if (ast->switch_bounds_checked) {
                Ast *case_ast_min = jump_table[0];
                Ast *case_ast_max = jump_table[jump_table_size - 1];
                /* Create some temporaries for the minimum and maximum values of 
                 * the jump table */
                IrValue *min_var = irConstInt(switch_var->type, case_ast_min->case_begin);
                IrValue *max_var = irConstInt(switch_var->type, case_ast_max->case_end);

                IrBlock *max_block = irBlockNew(irBlockId());
                IrBlock *switch_jump_table = irBlockNew(irBlockId());

                /* Compare the minimum and jump to the max check, or to the 
                 * out of bounds */
                irBuildIntCompareAndBranch(func, ctx->current_block, switch_var,
                        min_var, IR_CMP_LT, max_block, out_of_bounds_block);

                irFunctionAddBlock(func, max_block);
                irCtxSetCurrentBlock(ctx, max_block);

                /* Compare maximum and jump to the jump table condition or 
                 * to out of bounds */
                irBuildIntCompareAndBranch(func, ctx->current_block, switch_var, max_var,
                        IR_CMP_GT, switch_jump_table, out_of_bounds_block);

                irFunctionAddBlock(func, switch_jump_table);
                irCtxSetCurrentBlock(ctx, switch_jump_table);
            }

            IrInstr *ir_switch_head = irInstrNew(IR_OP_SWITCH);
            /* Setup the switch condition instruction */
            ir_switch_head->result = switch_var;
            ir_switch_head->target_block = end_block;
            ir_switch_head->extra.cases = listNew();
            irBlockAddInstruction(ctx->current_block, ir_switch_head);

            IrBlock *switch_start = ctx->current_block;

            /* Reserve the basic blocks for the case labels so we can do fall 
             * throughs if needed. */
            PtrVec *case_blocks = ptrVecNew();
            for (int i = 0; i < jump_table_size; ++i) {
                ptrVecPush(case_blocks, irBlockNew(irBlockId()));
            }

            for (int i = 0; i < jump_table_size; ++i) {
                /* The next block is either the block after or the out of bounds 
                 * block. This is for when the last instruction is not a break,
                 * thus we create a jump to the next block. */
                IrBlock *next_block = i+1 >= jump_table_size ?
                                             out_of_bounds_block :
                                             vecGet(IrBlock *,case_blocks,i+1);

                Ast *case_ = jump_table[i];
                IrBlock *ir_case_block = vecGet(IrBlock *,case_blocks,i);

                irCtxSetCurrentBlock(ctx, ir_case_block);
                /* @Redundant?
                 * Consistently re-set the end block as it is possible inside 
                 * the case there could be something that sets the end block 
                 * that we are not intrested in. */
                irCtxSetEndBlock(ctx, end_block);

                /* Expand out the case ranges to multiple labels that point to 
                 * the same block */
                for (int case_number = case_->case_begin; case_number <= case_->case_end; ++case_number) {
                    IrValue *ir_case_value = irConstInt(IR_TYPE_I64, case_number);
                    IrPair *ir_pair = irPairNew(ir_case_block, ir_case_value);
                    listAppend(ir_switch_head->extra.cases, ir_pair);
                }

                irFunctionAddMapping(func, switch_start, ir_case_block);
                irFunctionAddBlock(func,ir_case_block);
    
                /* Evaluate the body of the case */
                listForEach(case_->case_asts) {
                    Ast *stmt = listValue(Ast *, it);
                    irStatement(ctx, func, stmt);
                }

                IrInstr *last_instruction = irBlockLastInstr(ir_case_block);
                if (!last_instruction || (last_instruction && last_instruction->opcode != IR_OP_JMP)) { 
                    /* if the last operation in the block is not a jump then 
                     * we need to add a jump from the case block to the next 
                     * block. Or if we have stacked cases
                     * */
                    irJump(func, ir_case_block, next_block);
                }
            }

            /* If we have a default block then evaluate the block */
            if (default_block) {
                irCtxSetEndBlock(ctx, end_block);
                irCtxSetCurrentBlock(ctx, default_block);
                irFunctionAddBlock(func,default_block);

                listForEach(ast->case_default->case_asts) {
                    Ast *stmt = listValue(Ast *, it);
                    irStatement(ctx, func, stmt);
                }
            }

            ptrVecRelease(case_blocks);
            ctx->flags = prev_flags;
            irFunctionAddBlock(func,end_block);
            irCtxSetEndBlock(ctx, ir_prev_end_block);
            irCtxSetCurrentBlock(ctx, end_block);
            break;
        }

        case AST_BREAK: {
            assert(ctx->end_block);
            irJump(func, ctx->current_block, ctx->end_block);
            break;
        }

        case AST_DECL: {
            int ast_kind = ast->declvar->type->kind;
            IrInstr *ir_local = irAlloca(func, ctx->current_block, ast->declvar->type);
            IrValue *ir_local_var = irGetAllocaVar(ir_local);
            int ok = 0;

            if (ast_kind == AST_TYPE_FUNC) {
                ok = strMapAddAoStr(func->variables,
                                    ast->declvar->tmp_fnptr_name, 
                                    ir_local_var);
            } else {
                ok = strMapAddAoStr(func->variables,
                                    ast->declvar->tmp_var_name, 
                                    ir_local_var);
            }

            if (!ok) {
                loggerPanic("Failed to set function parameter variable with name %s already exists!\n",
                        ir_local_var->name->data);
            }

            if (ast->declinit) {
                IrValue *ir_init = NULL;
                switch (ast->declinit->kind) {
                    case AST_ARRAY_INIT: {
                        irCtxResetArray(ctx);
                        PtrVec *ir_array = ptrVecNew();
                        ctx->array_.init = ir_array;
                        irArrayInit(ctx, func, ast->declinit);

                        IrValue *head = (IrValue *)ir_array->entries[0];
                        ir_init = irValueNew(IR_TYPE_ARRAY_INIT, head->kind);
                        ir_init->array_.values = ir_array;
                        ir_init->array_.nesting = ctx->array_.nesting;
                        ir_init->array_.length_per_array = ctx->array_.length_per_array;
                        ir_init->array_.label = irArrayName(func);
                        strMapAddAoStr(ctx->ir_program->arrays, ir_init->array_.label, ir_init);
                        irStore(ctx->current_block,ir_local_var,ir_init);  
                        break;
                    }

                    case AST_FUN_PROTO:
                    case AST_FUNC:
                    case AST_EXTERN_FUNC:
                    case AST_ASM_FUNCDEF:
                    case AST_ASM_FUNC_BIND: {
                        ir_init = irGlobalExpression(ctx, ast->declinit);
                        irStore(ctx->current_block,ir_local_var,ir_init);  
                        break;
                    }

                    case AST_ASM_FUNCALL:
                    case AST_FUNPTR_CALL:
                    case AST_FUNCALL: {
                        ir_init = irExpression(ctx, func, ast->declinit);
                        ir_local_var->flags |= IR_FN_VAL_USED;
                        irStore(ctx->current_block,ir_local_var,ir_init);  
                        break;
                    }

                    case AST_ADDR:
                        ir_init = irExpression(ctx, func, ast->declinit);
                        ir_local_var->flags |= IR_VALUE_ADDR;
                        irStore(ctx->current_block,ir_local_var,ir_init);  
                        break;

                    default: {
                        ir_init = irExpression(ctx, func, ast->declinit);
                        irStore(ctx->current_block,ir_local_var,ir_init);  
                        break;
                    }    
                }

            }
            break;
        }

        /* We want to avoid having multiple return statements in a function 
         * it is more cannoincal to move the value you want to return to a
         * specified stack variable and then jump to the end of the function. 
         * Moving the stack variable into the return register. */
        case AST_RETURN: {
            IrValue *ir_return_val = irExpression(ctx, func, ast->retval);
            irStore(ctx->current_block,func->return_value,ir_return_val);
            irJump(func, ctx->current_block, func->exit_block);
            break;
        }

        default:
            irExpression(ctx, func, ast);
            break;
    }
}

void irResolveGotos(IrCtx *ctx, IrFunction *func) {
    PtrVec *ir_goto_vec = ctx->unresolved_gotos;
    for (int i = 0; i < ir_goto_vec->size; ++i) {
        IrUnresolvedBlock *ir_ugoto = vecGet(IrUnresolvedBlock *,ir_goto_vec,i);
        List *goto_list_node = ir_ugoto->goto_.list_node;
        IrInstr *goto_instr = (IrInstr *)goto_list_node->value;
        aoStr *goto_label = (aoStr *)goto_instr->extra.unresolved_label;

        IrUnresolvedBlock *ir_ulabel = strMapGetAoStr(ctx->unresolved_labels,
                                                      goto_label);

        if (!ir_ulabel) {
            loggerPanic("Goto points to label `%s` which does not exist\n",
                    goto_label->data);
        }

        IrBlock *ir_src = ir_ugoto->goto_.ir_block;
        IrBlock *ir_dest = ir_ulabel->label_.ir_block;

        /* We remove the temporary label, do we even need to add it in the 
         * first place? While developing this, it is nice to see it being 
         * printed but this can be removed once the feature is built. */
        listDeque(ir_dest->instructions);
        goto_instr->target_block = ir_dest;
        irFunctionAddMapping(func, ir_src, ir_dest);
    }
}




/*==================== BLOCK OPTIMISATIONS ===================================*/

/* Remove `delete_block` from all predecessor and successor maps */
void irFunctionDelinkBlock(IrFunction *func, IrBlock *delete_block) {
    List *needle = NULL;
    if (!mapHas(func->cfg, delete_block->id)) {
        return;
    }
    printf("DELETE: %d\n", delete_block->id);
    listForEach(func->blocks) {
        IrBlock *block = listValue(IrBlock *, it);
        if (block->id == delete_block->id) {
            needle = it;
        } else {
            irFunctionRemoveSuccessor(func, block, delete_block);
            irFunctionRemovePredecessor(func, block, delete_block);
        }
    }

    mapRemove(func->cfg, delete_block->id);
    if (needle) {
        listUnlink(func->blocks, needle);
    }
}

/* If the fallthrough id is the destination id and the destination target is 
 * the same as the target, then we can remove the branch */
int irBranchFallsbackToSameBlock(IrInstr *src, IrInstr *destination, IrBlock *block) {
    if (src->opcode == IR_OP_BR) {
        return src->fallthrough_block->id == block->id &&
               src->target_block->id == destination->target_block->id;
    }
    return 0;
}

/* Merge target to the end of block */
void irBlockMerge(IrFunction *func, IrBlock *block, IrBlock *target) {
    /* We now also need to unlink `target` which is kind of tricky as we use a 
     * linked list... */
    IrBlockMapping *target_mapping = irFunctionGetBlockMapping(func, target);
    Map *target_successors = target_mapping ? target_mapping->successors : NULL;

    /* From the target blocks successor blocks we need to add `block` as a 
     * predecessor as we are splicing out `target` and need to maintain links */
    if (target_successors) {
        MapIter it;
        mapIterInit(target_successors, &it);
        while (mapIterNext(&it)) {
            IrBlock *successor_block = getValue(IrBlock *, it.node);
            /* Our new block needs to be linked to this successor */
            irFunctionAddSuccessor(func, block, successor_block);
            /* Successor needs this block as a predecessor */
            irFunctionAddPredecessor(func, successor_block, block);
            /* Successor removes the `target` which is the block being removed */
            irFunctionRemovePredecessor(func, successor_block, target);
        }
    }

    /* Splice out target */
    irFunctionRemoveSuccessor(func, block, target);
    irFunctionRemovePredecessor(func, target, block);
    if (target->instructions != NULL) {
        listMergeAppend(block->instructions, target->instructions);
        /* A `NULL` list of instructions means it can be deleted */
        target->instructions = NULL;
    }
}

int irBlocksHaveSingleLink(IrFunction *func, IrBlock *next, IrBlock *prev) {
    IrBlockMapping *prev_mapping = mapGet(func->cfg, prev->id);
    IrBlockMapping *next_mapping = mapGet(func->cfg, next->id);
    /* This should not happen */
    if (!prev_mapping || !next_mapping) {
        return 0;
    }

    if (prev_mapping->successors->size == 1 && next_mapping->predecessors->size == 1) {
        IrBlock *previous_next = mapGetAt(prev_mapping->successors,0);

        if (previous_next->id == next->id && next->id == previous_next->id) {
            return 1;
        }
    }
    return 0;
}

/* This function takes a `IrBlock *` node and returns the id */
int irBlockListIdAccessor(void *_ir_block) {
    return ((IrBlock *)_ir_block)->id;
}

/* Can the entry and exit be joined together? This is a function with no 
 * jumps or branches and the start block has no joining blocks and the 
 * end block similarly has no joining blocks */
int irBlockEntryCanMergeExit(IrFunction *func) {
    /* The nodes do not point to anything */
    if (!irBlockHasSuccessors(func, func->entry_block) &&
        !irBlockHasPredecessors(func, func->entry_block) &&
        !irBlockHasSuccessors(func, func->exit_block) &&
        !irBlockHasPredecessors(func, func->exit_block))
    {
        return 1;
    }
    return 0;
}

void irSimplifyBlocks(IrFunction *func) {
    UniqList *work_queue = uniqListNew(&irBlockListIdAccessor);
    Set *blocks_to_delete = setNew(16, &int_set_type);


    /* Any blocks that don't have a successor lets assume they jump to 
     * the return */
    listForEach(func->blocks) {
        IrBlock *block = getValue(IrBlock *, it);
        if (irBlockIsStartOrEnd(func, block)) continue;
        Map *cur_successors = irBlockGetSuccessors(func, block);
        if (cur_successors && cur_successors->size == 0) {
            irJump(func, block, func->exit_block);
        }
    }

    int changed = 1;
    while (changed) {
        changed = 0;
        /** 
         * @THINK
         * We append the list nodes themeselves as opposed to the `IrBlock *` as this
         * allows for easier delinking from the actual `IrFunction *`'s blocks. Though
         * it is _slightly_ annoying having to cast the value, seems like a reasonable
         * compromise.
         *
         * Maybe we keep a hashtable of nodes that we need to delete from the blocks
         * and do it in one pass. Doing it as we go seems extremely problematic as
         * you'd keep doing passes of the list. */
        listForEach(func->blocks) {
            uniqListAppend(work_queue, listValue(IrBlock *, it));
        }

        while (!uniqListEmpty(work_queue)) {
            /* We kind of have a list of lists */
            IrBlock *block = (IrBlock *)uniqListDequeue(work_queue);

            if (block == func->exit_block) continue;
            if (setHas(blocks_to_delete, block->id)) continue;

            /* Currently only a few simple cases are handled for the block 
             * simplification but it is enough to work with and have some simple 
             * optimisations. */
            
            if (irBlockIsRedundant(func, block)) {
                setAdd(blocks_to_delete, block->id);
                changed = 1;
            } else if (irBlockIsConstCompareAndBranch(block)) {
                IrInstr *ir_cmp = listValue(IrInstr *, block->instructions->next);
                IrInstr *ir_branch = listValue(IrInstr *, block->instructions->next->next);
                IrBlock *jump_target = irInstrEvalConstBranch(ir_cmp, ir_branch);

                /* Clear the list, removing the cmp and the branch */
                irInstrRelease(listPop(block->instructions));
                irInstrRelease(listPop(block->instructions));

                /* Jump from the source to the destiation without doing the 
                 * comarison! */
                block->sealed = 0;

                irFunctionRemoveSuccessor(func, block, ir_branch->fallthrough_block);
                irFunctionRemovePredecessor(func, ir_branch->fallthrough_block, block);
                if (!setHas(blocks_to_delete, jump_target->id)) {
                    irJump(func, block, jump_target);
                }
                changed = 1;
            } else if (irBlockIsRedundantJump(func, block)) {
                /*
                 * block 1 :: P {...}, S {2};
                 *  ..
                 *  ..
                 *  jmp
                 *
                 * DELETE - block 2 :: P {1}, S {...} <- this successor needs 
                 *                                       the current block as a
                 *                                       predecessor
                 */
                IrInstr *ir_jmp = irBlockLastInstr(block);
                listPop(block->instructions);
                if (!setHas(blocks_to_delete, ir_jmp->target_block->id)) {
                    irBlockMerge(func, block, ir_jmp->target_block);
                }
                setAdd(blocks_to_delete, ir_jmp->target_block->id);
                changed = 1;
            } else if (irBlockIsOnlyJump(func, block)) {
                IrInstr *instr = irBlockLastInstr(block);
                IrBlock *successor = instr->target_block;
                block->instructions = NULL;
                if (!setHas(blocks_to_delete, block->id)) {
                    /* `block` is the target that is getting removed */
                    irBlockMerge(func, successor, block);
                }
                setAdd(blocks_to_delete, block->id);
            }
        }

        /* This cleans up the branch from the blocks we've mutated, this 
         * could be done in the merge function */
        listForEach(func->blocks) {
            IrBlock *block = it->value;

            if (irLastInstructionIsJumpLike(block)) {
                IrInstr *last_instr = irBlockLastInstr(block);
                switch (last_instr->opcode) {
                    case IR_OP_JMP:
                    case IR_OP_LOOP:
                        if (setHas(blocks_to_delete, last_instr->target_block->id)) {
                            loggerWarning("case 1 bb%d\n", block->id);
                            last_instr->target_block = func->exit_block;
                            irFunctionAddMapping(func, block, func->exit_block);
                            changed = 1;
                        }
                        break;
                    case IR_OP_BR: {
                        if (setHas(blocks_to_delete, last_instr->target_block->id)) {
                            loggerWarning("case 2 bb%d\n", block->id);
                            last_instr->target_block = func->exit_block;
                            irFunctionAddMapping(func, block, func->exit_block);
                            changed = 1;
                        }

                        if (setHas(blocks_to_delete, last_instr->fallthrough_block->id)) {
                            loggerWarning("case 3 bb%d\n", block->id);
                            last_instr->fallthrough_block = func->exit_block;
                            irFunctionAddMapping(func, block, func->exit_block);
                            changed = 1;
                        }
                        break;
                    }
                    default:
                        break;
                }
            }

            if (setHas(blocks_to_delete, block->id)) {
                /* This means that nothing links to it. However... Things can 
                 * reference this node so we need to traverse all blocks removing 
                 * it. Doing this is extremely slow */
                irFunctionDelinkBlock(func, block);
                changed = 1;
            }

            if (!irBlockHasPredecessors(func, block) && block != func->exit_block &&
                                                        block != func->entry_block) {
                irFunctionDelinkBlock(func, block);
                changed = 1;
            }

            //if (mapping && mapping->successors->size == 0 && block != func->exit_block) {
            //    printf("Creating jump from %d to %d\n", block->id, func->exit_block->id);
            //    irJump(func, block, func->exit_block);
            //}
        }
    } /* While end */

    if (irBlockEntryCanMergeExit(func) || 
            irBlocksPointToEachOther(func, func->entry_block, func->exit_block)) {
        IrInstr *last = irBlockLastInstr(func->entry_block);
        if (last && last->opcode == IR_OP_JMP) {
            listPop(func->entry_block->instructions);
        }
        irBlockMerge(func, func->entry_block, func->exit_block);
        irFunctionDelinkBlock(func, func->exit_block);
    }

    uniqListRelease(work_queue);
    setRelease(blocks_to_delete);
}


/*==================== IR LOWERING ========================================== */


/* Function parameters can only be a one of a few different types, thus this is
 * fairly reasonable to have separate */
IrValue *irConvertAstFuncParam(Ast *ast_param) {
    IrValueType ir_type = irConvertType(ast_param->type);
    IrValue *ir_value = irValueNew(ir_type, IR_VALUE_PARAM);
    if (ast_param->kind == AST_LVAR) {
        ir_value->name = ast_param->tmp_var_name;
    } else if (ast_param->kind == AST_FUNPTR) {
        ir_value->name = ast_param->tmp_fnptr_name;
    } else if (ast_param->kind == AST_DEFAULT_PARAM) {
        ir_value->name = ast_param->declvar->tmp_var_name;
    } else {
        loggerPanic("Unhandled Ast: %s\n", astToString(ast_param));
    }
    return ir_value;
}

IrFunction *irLowerFunction(IrCtx *ctx, IrProgram *program, Ast *ast_function) {
    IrFunction *func = irFunctionNew(ast_function->fname);
    IrBlock *ir_entry_block = irBlockNew(irBlockId());
    IrBlock *ir_exit_block = irBlockNew(irBlockId());

    func->program = program;

    irFunctionAddBlock(func, ir_entry_block);

    if (ast_function->type->has_var_args) {
        func->has_var_args = 1;
    }

    Ast *ast_var_args = NULL;
    for (int i = 0; i < ast_function->params->size; ++i) {
        Ast *ast_param = vecGet(Ast *,ast_function->params,i);

        if (ast_param->kind == AST_VAR_ARGS) {
            assert(func->has_var_args);
            ast_var_args = ast_param;
            break;
        }

        aoStr *key = NULL;
        if (ast_param->kind == AST_LVAR) {
            key = ast_param->tmp_var_name;
        } else if (ast_param->kind == AST_FUNPTR) {
            key = ast_param->tmp_fnptr_name;
        } else if (ast_param->kind == AST_DEFAULT_PARAM) {
            key = ast_param->declvar->tmp_var_name;
        } else {
            loggerPanic("Unhandled key kind: %s\n",
                    astKindToString(ast_param->kind));
        }

        IrValue *ir_tmp_var = irTmpVariable(irConvertType(ast_param->type));
        ir_tmp_var->kind = IR_VALUE_PARAM;
        int ok = strMapAddAoStrOrErr(func->variables,
                                     key,
                                     ir_tmp_var);
        ptrVecPush(func->params, ir_tmp_var);

        if (!ok) {
            loggerPanic("Failed to set function parameter variable with name %s already exists!\n",
                    ir_tmp_var->name->data);
        }
    }

    if (func->has_var_args) {
        assert(ast_var_args != NULL);
        if (ast_var_args) {
            // Create argc local variable
            IrInstr *argc_alloca = irAlloca(func, ir_entry_block, ast_var_args->argc->type);
            IrValue *argc_var = irGetAllocaVar(argc_alloca);
            argc_var->name = ast_var_args->argc->tmp_var_name;
            strMapAddAoStr(func->variables, argc_var->name, argc_var);
            
            // Create a special parameter for argc
            IrValue *argc_param = irValueNew(irConvertType(ast_var_args->argc->type), IR_VALUE_PARAM);
            argc_param->name = aoStrPrintf("argc");
            ptrVecPush(func->params, argc_param);
            
            // Store the parameter value to the local
            irStore(ir_entry_block, argc_var, argc_param);
            
            // Create argv local variable
            IrInstr *argv_alloca = irAlloca(func, ir_entry_block, ast_var_args->argv->type);
            IrValue *argv_var = irGetAllocaVar(argv_alloca);
            argv_var->name = ast_var_args->argv->tmp_var_name;
            strMapAddAoStr(func->variables, argv_var->name, argv_var);
            
            // Create a special parameter for argv
            IrValue *argv_param = irValueNew(irConvertType(ast_var_args->argv->type), IR_VALUE_PARAM);
            argv_param->name = aoStrPrintf("argv");
            ptrVecPush(func->params, argv_param);
            
            // Store the parameter value to the local
            irStore(ir_entry_block, argv_var, argv_param);
        }
    }

    IrInstr *ir_return_space = irAlloca(func, ir_entry_block, ast_function->type->rettype);
    IrValue *ir_return_var = irGetAllocaVar(ir_return_space);
    IrBlockMapping *ir_exit_block_mapping = irBlockMappingNew(ir_exit_block->id);
    mapAdd(func->cfg, ir_exit_block_mapping->id, ir_exit_block_mapping);

    /* I'm not sure if this is needed */
    func->return_value = ir_return_var;


    func->entry_block = ir_entry_block;
    func->exit_block = ir_exit_block;
    
    irCtxSetCurrentBlock(ctx, ir_entry_block);

    /* The function now has everything lowered to IR */
    irStatement(ctx, func, ast_function->body);

    irFunctionAddBlock(func, ir_exit_block);
    irRet(func->exit_block, func->return_value);

    if (ctx->unresolved_gotos->size) {
        irResolveGotos(ctx, func);
    }


    if (ctx->optimise) {
        irSimplifyBlocks(func);
        irOptimiseFunction(func);
    }

    return func;
}

void irFlattenGlobalAstArray(IrCtx *ctx, Ast *ast);

IrValue *irGlobalExpression(IrCtx *ctx, Ast *ast) {
    if (!ast) return NULL;

    // Handle the allowed types of global initializers
    switch (ast->kind) {
        case AST_LITERAL: {
            IrValueType type = irConvertType(ast->type);
            IrValue *value = irValueNew(type, type == IR_TYPE_F64 ? IR_VALUE_CONST_FLOAT : IR_VALUE_CONST_INT);
            
            if (type == IR_TYPE_F64) {
                value->f64 = ast->f64;
            } else {
                value->i64 = ast->i64;
            }
            return value;
        }
        
        case AST_STRING: {
            IrValue *value = irValueNew(IR_TYPE_ARRAY, IR_VALUE_CONST_STR);
            value->str = ast->sval;
            value->str_real_len = ast->real_len;
            return value;
        }

        /**
         * @Error AST_FUN_PROTO
         * This should perhaps be an error as it implies that the function 
         * prototype has not been bound to a function definition.
         */
        case AST_FUN_PROTO:
        case AST_FUNC:
        case AST_EXTERN_FUNC: {
            IrValue *value = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_GLOBAL);
            value->name = ast->fname;
            return value;
        }

        case AST_ASM_FUNCDEF:
        case AST_ASM_FUNC_BIND: {
            IrValue *value = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_GLOBAL);
            value->name = ast->asmfname;
            return value;
        }

        case AST_ARRAY_INIT: {
            // This case should be handled by the calling function (irFlattenAstArray)
            // since it needs to maintain array nesting context
            irFlattenGlobalAstArray(ctx,ast);
            return NULL;
        }
        
        case AST_ADDR: {
            // Handle address-of operator for globals
            if (ast->operand->kind == AST_GVAR) {
                IrValue *value = irValueNew(IR_TYPE_PTR, IR_VALUE_GLOBAL);
                value->name = ast->operand->gname;
                return value;
            }
            loggerPanic("Cannot handle address for; %s\n", astToString(ast));
        }

        default:
            loggerPanic("Error: Non-constant expression in global initializer\n");
    }
}

void irFlattenGlobalAstArray(IrCtx *ctx, Ast *ast) {
    // Base case: if this isn't an array initialization, just add the value
    if (ast->kind != AST_ARRAY_INIT) {
        IrValue *ir_value = irGlobalExpression(ctx, ast);
        if (ir_value) {
            ctx->array_.type = ir_value->type;
            ptrVecPush(ctx->array_.init, ir_value);
        }
        return;
    }

    int dimension_size = ast->arrayinit->size;
    
    /* If this is the first nesting level, initialize the tracking info */
    if (ctx->array_.nesting == 0) {
        ctx->array_.length_per_array = dimension_size;
    }
    
    /* Increment nesting for recursive calls */
    ctx->array_.nesting++;
    
    /* Process each element in the array */
    for (int i = 0; i < dimension_size; ++i) {
        Ast *array_element = (Ast *)ast->arrayinit->entries[i];
        
        /* Recursively process nested arrays */
        if (array_element->kind == AST_ARRAY_INIT) {
            irFlattenGlobalAstArray(ctx, array_element);
        } else {
            // Convert and add the element
            IrValue *ir_value = irGlobalExpression(ctx, array_element);
            if (ir_value) {
                ctx->array_.type = ir_value->type;
                ptrVecPush(ctx->array_.init, ir_value);
            }
        }
    }
    
    /* Decrement nesting as we return from recursion */
    ctx->array_.nesting--;
}

IrValue *irLowerGlobal(IrCtx *ctx, Ast *global_variable) {
    Ast *global_decl = global_variable->declvar;
    Ast *global_init = global_variable->declinit;
    IrValueType type = irConvertType(global_decl->type);
    IrValue *value = irValueNew(type, IR_VALUE_GLOBAL);
    value->name = global_decl->gname;

    /* At this point in time in the compiler we know this is either going to 
     * be an integer, float or string. This means we really want global 
     * initialisers to be a Vector of some sort? */
    if (global_init) {
        /* As the parser is guaranteed to give us a literal, either string, int 
         * or float is trivial to create a literal value. */
        IrValue *ir_init = NULL;
        
        // Handle different types of initializers
        if (global_init->kind == AST_STRING) {
            ir_init = irGlobalExpression(ctx, global_init);
            // Add to program's string table if needed
            if (ir_init) {
                strMapAddAoStr(ctx->ir_program->strings, value->name, ir_init);
            }
        } else if (global_init->kind == AST_LITERAL) {
            ir_init = irGlobalExpression(ctx, global_init);
        } else if (global_decl->type->kind == AST_TYPE_ARRAY) {
            // Reset array context for new array processing
            ctx->array_.nesting = 0;
            ctx->array_.length_per_array = 0;
            ctx->array_.init = ptrVecNew();

            irFlattenGlobalAstArray(ctx, global_init);
            
            ir_init = irValueNew(IR_TYPE_ARRAY_INIT, IR_VALUE_GLOBAL);
            ir_init->array_.label = value->name;
            ir_init->array_.nesting = ctx->array_.nesting;
            ir_init->array_.length_per_array = ctx->array_.length_per_array;
            ir_init->array_.values = ctx->array_.init;

            // Add to program's array table
            strMapAddAoStr(ctx->ir_program->arrays, value->name, ir_init);
        } else {
            loggerWarning("Trying to create global IR for : %s\n",
                    astToString(global_init));
            // Try to handle any other type of constant expression
            ir_init = irGlobalExpression(ctx, global_init);
        }

        if (ir_init) {
            value->global.value = ir_init;
        }
    }

    // Always add the global variable definition
    return value;
}

IrProgram *irLowerAst(Cctrl *cc) {
    IrProgram *ir_program = irProgramNew();
    IrCtx *ctx = irCtxNew();
    ctx->ir_program = ir_program;
    ctx->optimise = cc->flags & CCTRL_OPTIMISE;

    /* Handle asm blocks */
    if (!listEmpty(cc->asm_blocks)) {
        listForEach(cc->asm_blocks) {
            /*
             * this is one:
             *
             * ```
             * asm {
             * _FUNC1::
             *
             * _FUNC2::
             * 
             * }
             * ```
             * Thus has potentially many functions.
             * */
            Ast *asm_block = listValue(Ast *, it);
            List *func_it = asm_block->funcs->next;

            /* For each function in the block, create and save the assembly function
             * but in IR. This is essentially a cut and paste, we provide no 
             * safe guards what so ever. */
            while (func_it != asm_block->funcs) {
                IrValue *ir_asm_func = irValueNew(IR_TYPE_ASM_FUNCTION, IR_VALUE_GLOBAL);
                Ast *asm_func = (Ast *)func_it->value;
                ir_asm_func->name = asm_func->asmfname;
                ir_asm_func->str = asm_func->body->asm_stmt;

                ptrVecPush(ir_program->asm_functions, ir_asm_func);
                func_it = func_it->next;
            }
        }
    }

    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            irCtxReset(ctx);
            IrFunction *ir_func = irLowerFunction(ctx, ir_program, ast);
            ptrVecPush(ir_program->functions, ir_func);

        } else if (ast->kind == AST_DECL) { 
            IrValue *global_value = irLowerGlobal(ctx, ast);
            mapAdd(ctx->ir_program->global_variables, global_value->name, global_value);
        } else if (ast->kind == AST_ASM_FUNC_BIND) {

        } else {
       //     loggerWarning("Cannot lower AST to IR: %s\n",
       //             astToString(ast));
        }
    }

    irCtxRelease(ctx);
    return ir_program;
}

aoStr *irProgramToString(IrProgram *ir_program) {
    aoStr *buf = aoStrNew();
    aoStrCatFmt(buf, "target = %s-%s\n", ARCH_STR, OS_STR);
    if (ir_program->global_variables->size) {
        MapIter *it = mapIterNew(ir_program->global_variables);
        while (mapIterNext(it)) {
            IrValue *ir_value = getValue(IrValue *, it->node);
            aoStr *ir_value_str = irValueToString(ir_value);
            aoStrCatFmt(buf, "%S\n", ir_value_str);
            aoStrRelease(ir_value_str);
        }
        mapIterRelease(it);
        aoStrPutChar(buf,'\n');
    }

    if (ir_program->asm_functions->size > 0) {
        for (int i = 0; i < ir_program->asm_functions->size; ++i) {
            IrValue *asm_function = vecGet(IrValue *, ir_program->asm_functions, i);
            const char *type_str = irValueTypeToString(asm_function->type);
            aoStrCatFmt(buf, "%s fn %S\n", type_str, asm_function->name);
        }
        aoStrPutChar(buf,'\n');
    }

    for (int i = 0; i < ir_program->functions->size; ++i) {
        IrFunction *ir_func = vecGet(IrFunction *, ir_program->functions, i);
        aoStr *ir_func_str = irFunctionToString(ir_func);
        aoStr *ir_func_cfg = irFunctionCFGToString(ir_func);
        aoStrCatFmt(buf, "%S\n%S\n", ir_func_str, ir_func_cfg);
    }

    return buf;
}

void irDump(Cctrl *cc) {
    IrProgram *ir_program = irLowerAst(cc);
    aoStr *ir_str = irProgramToString(ir_program);
    printf("%s\n",ir_str->data);
    aoStrRelease(ir_str);
}
