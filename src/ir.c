#include <stdlib.h>
#include <stdio.h>

#include "arena.h"
#include "ast.h"
#include "ir.h"
#include "ir-types.h"
#include "ir-debug.h"
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

VecType vec_ir_function_type = {
    .stringify = vecIrFunctionToString,
    .match     = NULL,
    .release   = NULL,
    .type_str  = "long",
};

Vec *irFunctionVecNew(void) {
    return vecNew(&vec_ir_function_type);
}

void vecIrPairToString(AoStr *buf, void *_ir_pair) {
    irPairToString(buf, _ir_pair);
}

VecType vec_ir_pair_type = {
    .stringify = vecIrPairToString,
    .match     = NULL,
    .release   = NULL,
    .type_str  = "IrPair *",
};

Vec *irPairVecNew(void) {
    return vecNew(&vec_ir_pair_type);
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

void irFunctionRemoveSuccessor(IrFunction *func, IrBlock *src, IrBlock *dest) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)mapGetInt(func->cfg, src->id);
    if (ir_block_mapping) {
        mapRemoveInt(ir_block_mapping->successors, dest->id);
    }
}

void irFunctionRemovePredecessor(IrFunction *func, IrBlock *src, IrBlock *prev) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)mapGetInt(func->cfg, src->id);
    if (ir_block_mapping) {
        mapRemoveInt(ir_block_mapping->predecessors, prev->id);
    }
}

void irFunctionAddMapping(IrFunction *func, IrBlock *src, IrBlock *dest) {
    irFunctionAddSuccessor(func, src, dest);
    irFunctionAddPredecessor(func, dest, src);
}

static u32 ir_block_id = 1;

void irResetBlockId(void) {
    ir_block_id = 1;
}

IrBlock *irBlockNew(IrFunction *ir_func) {
    IrBlock *block = irAlloc(sizeof(IrBlock));
    block->instructions = listNew();
    block->removed = 0;
    block->sealed = 0;
    block->id = ir_block_id++;
    listAppend(ir_func->blocks, block);
    return block;
}

void irBlockRelease(IrBlock *block) {
    listRelease(block->instructions, NULL);
}

IrValueType irConvertType(AstType *type) {
    switch (type->kind) {
        case AST_TYPE_VOID: return IR_TYPE_VOID;
        case AST_TYPE_INT: {
            switch (type->size) {
                case 1: return IR_TYPE_I8;
                case 2: return IR_TYPE_I16;
                case 4: return IR_TYPE_I32;
                case 8: return IR_TYPE_I64;
                default:
                    loggerPanic("Invalid integer size `%d` for type %s\n",
                            type->size,
                            astTypeToString(type));
            }
        }
        case AST_TYPE_CHAR:    return IR_TYPE_I8;
        case AST_TYPE_FLOAT:   return IR_TYPE_F64;
        case AST_TYPE_ARRAY:   return IR_TYPE_ARRAY;
        case AST_TYPE_POINTER: return IR_TYPE_PTR;
        case AST_TYPE_FUNC:    return IR_TYPE_FUNCTION;
        case AST_TYPE_CLASS:   return IR_TYPE_STRUCT;
        case AST_TYPE_UNION:   return IR_TYPE_STRUCT;
        case AST_TYPE_VIS_MODIFIER:
            loggerPanic("Type visibility modifier is not a type!\n");
        case AST_TYPE_INLINE:
            loggerPanic("Type `inline` is not a type!\n");
        case AST_TYPE_AUTO:
            loggerPanic("Type `auto` failed to infer it's runtime type\n");
        default:
            loggerPanic("Type `%s` unhandled\n",astTypeToString(type));
    }
}

IrValue *irValueNew(IrValueType type, IrValueKind kind) {
    IrValue *val = irAlloc(sizeof(IrValue));
    val->kind = kind;
    val->type = type;
    return val;
}

IrValue *irTmp(IrValueType type, u32 var) {
    IrValue *val = irValueNew(type, IR_VAL_TMP);
    val->as.var = var;
    return val;
}

IrInstr *irInstrNew(IrOp op, IrValue *dst, IrValue *r1, IrValue *r2) {
    IrInstr *instr = irAlloc(sizeof(IrInstr));
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
    return func;
}

IrValue *irFnGetVar(IrFunction *func, u32 var_id) {
    return mapGetInt(func->variables, var_id);
}

int irSetVariable(IrFunction *func, u32 var_id, IrValue *var) {
    return mapAddIntOrErr(func->variables, var_id, var);
}

void irFunctionRelease(IrFunction *func) {
    listRelease(func->blocks, (void (*)(void *))&irBlockRelease);
    mapRelease(func->cfg);
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

IrInstr *irAlloca(IrCtx *ctx, u32 var_id, AstType *ast_type) {
    IrValueType ir_type = irConvertType(ast_type);
    IrValue *tmp = irTmp(ir_type, var_id);
    IrValue *ir_size = irConstInt(ir_type, ast_type->size);
    IrInstr *ir_alloca = irInstrNew(IR_ALLOCA, tmp, ir_size, NULL);
    irBlockAddInstr(ctx, ir_alloca);
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
        IrValue *bool_cond = irTmp(IR_TYPE_I8, 0);
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

IrInstr *irLoop(IrFunction *func, IrBlock *block, IrBlock *target) {
    return irJumpInternal(func, block,target,IR_LOOP);
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

IrValue *irExpr(IrCtx *ctx, Ast *ast);


/* Binary expressions are assumed to always be assigning to something. I'm not
 * 100% sure this is a valid assumption to make. Well I guess;
 * `I64 x = y + 32 * 10` _could_ continually be assigned to `x` */
IrValue *irBinOpExpr(IrCtx *ctx, u32 var_id, Ast *ast) {
    IrValue *lhs = irExpr(ctx, ast->left);
    IrValue *rhs = irExpr(ctx, ast->right);
    IrValueType ir_type = irConvertType(ast->type);
    IrValue *ir_result = irTmp(ir_type, var_id);
    IrOp op;
    IrCmpKind cmp = IR_CMP_INVALID;

    if (ast->binop == AST_BIN_OP_LOG_AND) {
        IrBlock *ir_block = ctx->cur_block;
        IrBlock *ir_right_block = irBlockNew(ctx->cur_func);
        IrBlock *ir_end_block = irBlockNew(ctx->cur_func);

        IrValue *left = irExpr(ctx, ast->left);
        IrValue *ir_result = irTmp(IR_TYPE_I8, var_id);

        irBranch(ctx->cur_func, ir_block, left, ir_right_block, ir_end_block);
        ctx->cur_block = ir_right_block;

        IrValue *right = irExpr(ctx, ast->right);

        IrInstr *jump_instr = irJump(ctx->cur_func, ctx->cur_block, ir_end_block);

        /* Add to the current blocks instructions */
        listAppend(ctx->cur_block->instructions, jump_instr);
        ctx->cur_block = ir_end_block;

        IrInstr *phi_instr = irPhi(ir_result);
        listAppend(ctx->cur_block->instructions, phi_instr);
        irAddPhiIncoming(phi_instr, irConstInt(IR_TYPE_I8, 0), ir_block);
        irAddPhiIncoming(phi_instr, right, ir_right_block);
        return ir_result;
    } else if (ast->binop == AST_BIN_OP_LOG_OR) {

        IrBlock *ir_block = ctx->cur_block;
        IrBlock *ir_right_block = irBlockNew(ctx->cur_func);
        IrBlock *ir_end_block = irBlockNew(ctx->cur_func);

        IrValue *left = irExpr(ctx, ast->left);
        IrValue *ir_result = irTmp(IR_TYPE_I8, var_id);

        /* For an OR the difference is this is switched around */
        irBranch(ctx->cur_func, ir_block, left, ir_end_block, ir_right_block);
        ctx->cur_block = ir_right_block;

        IrValue *right = irExpr(ctx, ast->right);

        IrInstr *jump_instr = irJump(ctx->cur_func, ctx->cur_block, ir_end_block);

        /* Add to the current blocks instructions */
        listAppend(ctx->cur_block->instructions, jump_instr);
        ctx->cur_block = ir_end_block;

        IrInstr *phi_instr = irPhi(ir_result);
        listAppend(ctx->cur_block->instructions, phi_instr);
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

IrValue *irExpr(IrCtx *ctx, Ast *ast) {
    switch (ast->kind) {
        case AST_BINOP:
            return irBinOpExpr(ctx, 300, ast);
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
            IrValue *ir_load_dest = irTmp(ir_value_type, local_var->as.var);

            if (irIsStruct(local_var->type)) {
                loggerWarning("Unhandled load of a struct!\n");
                // irGetElementPointer(ir_block, ir_load_dest, local_var);
            } else {
                IrInstr *load_instr = irLoad(ir_load_dest, local_var);
                listAppend(ctx->cur_block->instructions, load_instr);
            }
            return ir_load_dest;
        }

        case AST_UNOP:

        case AST_GVAR:
        case AST_GOTO:
        case AST_LABEL:
        case AST_FUNC:
        case AST_DECL:
        case AST_STRING:
        case AST_FUNCALL:
        case AST_ARRAY_INIT:
        case AST_IF:
        case AST_FOR:
        case AST_RETURN:
        case AST_WHILE:
        case AST_CLASS_REF:
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
            loggerPanic("Unhandled AST kind: %s\n", astKindToString(ast->kind));
    }
}

void irLowerAst(IrCtx *ctx, Ast *ast) {
    if (!ast) return;

    switch (ast->kind) {
        case AST_COMPOUND_STMT: {
            listForEach(ast->stms) {
                Ast *next = (Ast *)it->value;
                irLowerAst(ctx, next);
            }
            break;
        }
 
        case AST_BINOP:
        case AST_LVAR:
            break;

        case AST_DECL: {
            Ast *var = ast->declvar;
            Ast *init = ast->declinit;
            IrInstr *ir_alloca = irAlloca(ctx, var->lvar_id, var->type);
            IrValue *local = ir_alloca->dst;

            int ok = 0;
            if (!(ok = irSetVariable(ctx->cur_func, local->as.var, local))) {
                /* I can't see how this would be hit? */
                loggerPanic("Variable or function pointer already defined.\n"); 
            }

            irInstrPrint(ir_alloca);

            if (init) {
                IrValue *ir_init = irExpr(ctx, init);
            }
            break;
        }

        case AST_GVAR:
        case AST_GOTO:
        case AST_LABEL:
        case AST_FUNC:
        case AST_STRING:
        case AST_FUNCALL:
        case AST_LITERAL:
        case AST_ARRAY_INIT:
        case AST_IF:
        case AST_FOR:
        case AST_RETURN:
        case AST_WHILE:
        case AST_CLASS_REF:
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
        case AST_UNOP:
          break;
    }

    astKindPrint(ast->kind);
}

void irMakeFunction(IrCtx *ctx, Ast *ast_func) {
    IrFunction *func = irFunctionNew(ast_func->fname);
    IrBlock *entry = irBlockNew(func);

    ctx->cur_block = entry;
    func->entry_block = entry;
    ctx->cur_func = func;


    irLowerAst(ctx, ast_func->body);
    IrBlock *exit_block = irBlockNew(func);
    IrInstr *ir_return_space = irAlloca(ctx, -1, ast_func->type->rettype);
    IrValue *ir_return_var = ir_return_space->dst;
    func->return_value = ir_return_var;
    IrBlockMapping *ir_exit_block_mapping = irBlockMappingNew(exit_block->id);
    mapAddIntOrErr(func->cfg, ir_exit_block_mapping->id, ir_exit_block_mapping);

    func->exit_block = exit_block;

    irPrintFunction(func);
}

void irDump(Cctrl *cc) {
    IrCtx *ctx = malloc(sizeof(IrCtx));
    ctx->cc = cc;

    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            irMakeFunction(ctx, ast);
        }
    }
}
