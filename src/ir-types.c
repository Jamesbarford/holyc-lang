#include <assert.h>
#include <math.h>
#include <string.h>

#include "arena.h"
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

/* Container specialisation ==================================================*/
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

int irBlockVecMatch(void *_bb1, void *_bb2) {
    IrBlock *bb1 = (IrBlock *)_bb1;
    IrBlock *bb2 = (IrBlock *)_bb2;
    return bb1->id == bb2->id;
}

void irBlockVecToString(AoStr *buf, void *_bb) {
    IrBlock *bb = (IrBlock *)_bb;
    AoStr *bb_str = irBlockToStringSimplified(bb);
    aoStrCatAoStr(buf, bb_str);
    aoStrRelease(bb_str);
}

/* `Vec<IrBlock *>`*/
VecType vec_ir_block_type = {
    .stringify = irBlockVecToString,
    .match     = irBlockVecMatch,
    .release   = NULL,
    .type_str  = "IrBlock *",
};

AoStr *mapIrBlockToString(void *_ir_block) {
    IrBlock *ir_block = (IrBlock *)_ir_block;
    return aoStrPrintf("block %u", ir_block->id);
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

/* `Map<u32, IrBlock *>` */
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

/* `Map<u32, IrValue *>`*/
MapType map_u32_to_ir_value_type = {
    .match           = mapIntKeyMatch,
    .hash            = mapIntKeyHash,
    .get_key_len     = mapIntKeyLen,
    .key_to_string   = mapIntToString,
    .key_release     = NULL,
    .value_to_string = (AoStr *(*)(void *))irValueToString,
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

/* `Map<u64, IrInstr *>` */
MapType map_uint_to_ir_instr_type = {
    .match           = mapIntKeyMatch,
    .hash            = mapIntKeyHash,
    .get_key_len     = mapIntKeyLen,
    .key_to_string   = mapIntToString,
    .key_release     = NULL,
    .value_to_string = (AoStr *(*)(void *value))irInstrToString,
    .value_release   = NULL,
    .key_type        = "u64",
    .value_type      = "IrInstr *",
};

Map *irBlockMapNew(void) {
    return mapNew(8, &map_u32_to_ir_block_type);
}

Map *irBlockMappingMapNew(void) {
    return mapNew(32, &map_u32_to_ir_block_mapping_type);
}

Map *irVarValueMapNew(void) {
    return mapNew(16, &map_u32_to_ir_value_type);
}

Map *irInstrMapNew(void) {
    return mapNew(8, &map_uint_to_ir_instr_type);
}

void irBlockMappingRelease(void *_mapping) {
    IrBlockMapping *mapping = _mapping;
    mapRelease(mapping->predecessors);
    mapRelease(mapping->successors);
    
}

AoStr *mapIrValueToString(void *ir_value) {
    return irValueToString((IrValue *)ir_value);
}

IrBlockMapping *irBlockMappingNew(int id) {
    IrBlockMapping *mapping = (IrBlockMapping *)irAlloc(sizeof(IrBlockMapping));
    mapping->id = id;
    mapping->successors = irBlockMappingMapNew();
    mapping->predecessors = irBlockMappingMapNew();
    return mapping;
}
/* Container specialisation END ==============================================*/

u8 irOpIsCmp(IrOp opcode) {
    if (opcode == IR_ICMP || opcode == IR_FCMP) {
        return 1;
    }
    return 0;
}

u8 irIsFloat(IrValueType type) {
    return type == IR_TYPE_F64;
}

u8 irIsInt(IrValueType type) {
    return type == IR_TYPE_I8 ||
           type == IR_TYPE_I16 ||
           type == IR_TYPE_I32 ||
           type == IR_TYPE_I64;
}

u8 irIsConstInt(IrValue *val) {
    return val && val->kind == IR_VAL_CONST_INT;
}

u8 irIsStruct(IrValueType ir_value_type) {
    return ir_value_type == IR_TYPE_STRUCT;
}

u8 irIsPtr(IrValueType ir_value_type) {
    return ir_value_type == IR_TYPE_PTR;
}

u8 irIsTmp(IrValue *val) {
    return val && val->kind == IR_VAL_TMP;
}

u8 irIsStore(IrOp opcode) {
    return opcode = IR_STORE;
}

u8 irIsLoad(IrOp opcode) {
    return opcode = IR_LOAD;
}

u8 irTypeIsScalar(IrValueType ir_value_type) {
    return irIsFloat(ir_value_type) || irIsInt(ir_value_type);
}

int irGetIntSize(IrValueType ir_value_type) {
    switch (ir_value_type) {
        case IR_TYPE_I8:  return 1;
        case IR_TYPE_I16: return 2;
        case IR_TYPE_I32: return 4;
        case IR_TYPE_I64: return 8;
        default: loggerPanic("%d is not an integer type\n", ir_value_type);
    }
}

u8 irIsConst(IrValueKind ir_value_kind) {
    return ir_value_kind == IR_VAL_CONST_INT || 
           ir_value_kind == IR_VAL_CONST_FLOAT;
}

IrBlock *irInstrGetTargetBlock(IrInstr *instr) {
    return instr->extra.blocks.target_block;
}

IrBlock *irInstrGetFallthroughBlock(IrInstr *instr) {
    return instr->extra.blocks.fallthrough_block;
}

int irAreCompatibleCmpTypes(IrValueType t1, IrValueType t2) {
    if (t1 == t2) return 1;
    if (irIsInt(t1) & irIsInt(t2)) return 1;
    if (irIsFloat(t1) & irIsFloat(t2)) return 1;

    if ((t1 == IR_TYPE_PTR && irIsInt(t2)) || (t2 == IR_TYPE_PTR && irIsInt(t1)))
        return 1;

    return 0;
}

int irVarEq(IrValue *v1, IrValue *v2) {
    if (!v2 || !v1) return 0;
    if (v1 == v2) return 1;
    if (v1->kind == v2->kind && v1->type == v2->type && v1->as.var.id == v2->as.var.id) {
        return 1;
    }
    return 0;
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
            loggerPanic( "Type `inline` is not a type!\n");
        case AST_TYPE_AUTO:
            loggerPanic("Type `auto` failed to infer it's runtime type\n");
        default:
            loggerPanic("Type `%s` unhandled\n",astTypeToString(type));
    }
}

/* Identity used to key an IR slot for an AST param-or-local. AST_FUNPTR
 * carries its id on `fn_ptr_id`; AST_LVAR on `lvar_id`. AST_DEFAULT_PARAM
 * is a wrapper - the real id lives on its inner `declvar`. */
u32 irGetParamId(Ast *param) {
    if (param->kind == AST_DEFAULT_PARAM) {
        return param->declvar->kind == AST_FUNPTR
            ? param->declvar->fn_ptr_id
            : param->declvar->lvar_id;
    }
    if (param->kind == AST_FUNPTR) return param->fn_ptr_id;
    return param->lvar_id;
}

int irValueIsVariable(IrValue *value) {
    return value->kind == IR_VAL_LOCAL ||
           value->kind == IR_VAL_PARAM ||
           value->kind == IR_VAL_TMP ||
           value->kind == IR_VAL_GLOBAL;
}

/*=========================================================================== */


/*==================== IR COMPILE TIME EVALUATION =========================== */

IrInstr *irBlockLastInstr(IrBlock *block) {
    List *tail = listTail(block->instructions);
    if (!tail) return NULL;
    return listValue(IrInstr *, tail);
}

IrInstr *irBlockFirstInstr(IrBlock *block) {
    List *head = listHead(block->instructions);
    if (!head) return NULL;
    return listValue(IrInstr *, head);
}

int irLastInstructionIsJumpLike(IrBlock *block) {
    IrInstr *ir_instr = irBlockLastInstr(block);
    if (!ir_instr) return 0;
    return ir_instr->op == IR_JMP  ||
           ir_instr->op == IR_BR;
}

Map *irBlockGetSuccessors(IrFunction *func, IrBlock *block) {
    IrBlockMapping *mapping = mapGetInt(func->cfg, block->id);
    if (!mapping) return NULL;
    return mapping->successors;
}

Map *irBlockGetPredecessors(IrFunction *func, IrBlock *block) {
    IrBlockMapping *mapping = mapGetInt(func->cfg, block->id);
    if (!mapping) return NULL;
    return mapping->predecessors;
}

int irBlockHasSuccessors(IrFunction *func, IrBlock *block) {
    Map *successors = irBlockGetSuccessors(func, block);
    return successors ? successors->size > 0 : 0;
}

int irBlockHasPredecessors(IrFunction *func, IrBlock *block) {
    Map *predecessors = irBlockGetPredecessors(func, block);
    return predecessors ? predecessors->size > 0 : 0;
}

int irBlockIsStartOrEnd(IrFunction *func, IrBlock *block) {
    return block == func->entry_block || block == func->exit_block; 
}

int irBlockIsRedundant(IrFunction *func, IrBlock *block) {
    IrBlockMapping *mapping = mapGetInt(func->cfg, block->id);

    /* Start and exit will be merged seprately */
    if (irBlockIsStartOrEnd(func, block)) {
        return 0;
    }
    /* No instructions in a node means it can be killed */
    else if (listEmpty(block->instructions)) {
        return 1;
    } 
    /* No mapping means the block is essentially not used */
    else if (!mapping) {
        return 1;
    }
    /* There is nothing that points to this node and it is not the start 
     * block for the function */
    else if (mapping->predecessors->size == 0) {
        return 1;
    }

    return 0;
}

/* Does the previous block have one successor and jump to the next block and 
 * does the next block have one predecessor that is the previous block. 
 * If yes... we can join them together. */
u8 irBlocksPointToEachOther(IrFunction *func, IrBlock *cur_block, IrBlock *next_block) {
    IrInstr *ir_last_instr = irBlockLastInstr(cur_block);
    assert(ir_last_instr);

    if (ir_last_instr->op == IR_JMP) {
        IrBlock *target_block = irInstrGetTargetBlock(ir_last_instr);
        /* The target block does not point to the block of interest */
        if (target_block->id != next_block->id) return 0;
        Map *cur_successors = irBlockGetSuccessors(func, cur_block);
        Map *next_predecessors = irBlockGetPredecessors(func, next_block);

        /* This check is overly safe */
        if (next_predecessors->size == 1 &&
            mapHasInt(next_predecessors, cur_block->id) &&
            cur_successors->size == 1 &&
            mapHasInt(cur_successors, next_block->id)) {
            return 1;
        }
    }
    return 0;
}

/* Is the block a solitary jump instruction? */
int irBlockIsOnlyJump(IrFunction *func, IrBlock *block) {
    IrInstr *ir_last_instr = irBlockLastInstr(block);
    assert(ir_last_instr);
    if (ir_last_instr->op == IR_JMP && listIsOne(block->instructions)) {
        Map *successors = irBlockGetSuccessors(func, block);
        Map *predecessors = irBlockGetPredecessors(func, block);
        if (successors && predecessors) {
            return successors->size == 1 && predecessors->size == 1;
        } else {
            return 0;
        }
    }
    return 0;
}

/**
 * Does the current block end with a jump, have one successor and the successor
 * has one predecessor?
 */
int irBlockIsRedundantJump(IrFunction *func, IrBlock *block) {
    IrInstr *jmp = irBlockLastInstr(block);
    assert(jmp);
    if (jmp->op == IR_JMP) {
        IrBlock *next_block = irInstrGetTargetBlock(jmp);
        Map *cur_successors = irBlockGetSuccessors(func, block);
        Map *next_predecessors = irBlockGetPredecessors(func, next_block);

        unsigned long next_predecessors_size = next_predecessors ? 
                                               next_predecessors->size : 0;
        unsigned long cur_successors_size = cur_successors ?
                                            cur_successors->size : 0;

        /* This check is overly safe */
        if (next_predecessors_size == 1 &&
            mapHasInt(next_predecessors, block->id) &&
            cur_successors_size == 1 &&
            mapHasInt(cur_successors, next_block->id))
        {
            return next_block->id != block->id;
        }
    }
    return 0;
}

int irBlockIsConstCompareAndBranch(IrBlock *block) {
    /* We need to ensure that the instruction list is 2 in length and is
     * a `cmp` followed by a `br` */
    if (listEmpty(block->instructions) || listIsOne(block->instructions)) {
        return 0;
    }

    /* We have at least 2 instructions */
    IrInstr *maybe_cmp = listValue(IrInstr *, block->instructions->next);
    IrInstr *maybe_br = listValue(IrInstr *, block->instructions->next->next);

    if (irOpIsCmp(maybe_cmp->op) && maybe_br->op == IR_BR) {
        if (irIsConst(maybe_cmp->r1->kind) && irIsConst(maybe_cmp->r2->kind)) {
            return 1;
        }
    }
    return 0;
}

/* Evaluate the current block and return the actual target from the result of 
 * the comparison. Handles both floats and integer comparisons */
IrBlock *irInstrEvalConstBranch(IrInstr *ir_cmp, IrInstr *ir_branch) {
    assert(irOpIsCmp(ir_cmp->op));
    IrValue *v1 = ir_cmp->r1;
    IrValue *v2 = ir_cmp->r2;

#define I1 v1->as._i64
#define I2 v2->as._i64

#define F1 v1->as._f64
#define F2 v2->as._f64

#define V1 (irIsFloat(v1->type) ? F1 : I1)
#define V2 (irIsFloat(v2->type) ? F2 : I2)

    switch (ir_cmp->extra.cmp_kind) {
        case IR_CMP_EQ:
            if (V1 == V2) goto target;
            else          goto fallthrough;
        
        case IR_CMP_NE:
            if (V1 != V2) goto target;
            else          goto fallthrough;

        case IR_CMP_LT:
            if (V1 < V2) goto target;
            else         goto fallthrough;

        case IR_CMP_LE:
            if (V1 <= V2) goto target;
            else          goto fallthrough;

        case IR_CMP_GT:
            if (V1 > V2) goto target;
            else         goto fallthrough;
        
        case IR_CMP_GE:
            if (V1 >= V2) goto target;
            else          goto fallthrough;

        case IR_CMP_ULT:
            if ((unsigned long)I1 < (unsigned long)I2) goto target;
            else                                       goto fallthrough;

        case IR_CMP_ULE:
            if ((unsigned long)I1 <= (unsigned long)I2) goto target;
            else                                        goto fallthrough;

        case IR_CMP_UGT:
            if ((unsigned long)I1 > (unsigned long)I2) goto target;
            else                                       goto fallthrough;

        case IR_CMP_UGE:
            if ((unsigned long)I1 >= (unsigned long)I2) goto target;
            else                                        goto fallthrough;

        case IR_CMP_OEQ:
            if (F1 == F2) goto target;
            else          goto fallthrough;

        case IR_CMP_ONE:
            if (F1 != F2) goto target;
            else          goto fallthrough;

        case IR_CMP_OLT:
            if (F1 < F2) goto target;
            else         goto fallthrough;

        case IR_CMP_OLE:
            if (F1 <= F2) goto target;
            else          goto fallthrough;

        case IR_CMP_OGT:
            if (F1 > F2) goto target;
            else         goto fallthrough;

        case IR_CMP_OGE:
            if (F1 >= F2) goto target;
            else          goto fallthrough;

        case IR_CMP_UNO:
            if (isnan(F1) || isnan(F2)) goto target;
            else                        goto fallthrough;

        case IR_CMP_ORD:
            if (!isnan(F1) && !isnan(F2)) goto target;
            else                          goto fallthrough;
        case IR_CMP_INVALID:
            loggerPanic("Invalid comparison");
    }

/* We can remove these symbolic constants */
#undef V1
#undef V2 
#undef I1
#undef I2
#undef F1
#undef F2

fallthrough:
    return irInstrGetFallthroughBlock(ir_branch);

target:
    return irInstrGetTargetBlock(ir_branch);
}

/* This is very easy to inline */
IrBlockMapping *irFunctionGetBlockMapping(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)mapGet(func->cfg,
                                                                (void *)(u64)ir_block->id);
    return ir_block_mapping;
}

Map *irFunctionGetSuccessors(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *ir_block_mapping = irFunctionGetBlockMapping(func, ir_block);
    if (ir_block_mapping) {
        return ir_block_mapping->successors;
    }
    return NULL;
}

Map *irFunctionGetPredecessors(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *ir_block_mapping = irFunctionGetBlockMapping(func, ir_block);
    if (ir_block_mapping) {
        return ir_block_mapping->predecessors;
    }
    return NULL;
}

void irFunctionVecToString(AoStr *buf, void *_ir_func) {
    IrFunction *ir_func = _ir_func;
    aoStrCatPrintf(buf, "%s", ir_func->name->data);
}

/* `Vec<IrFunction *>`*/
VecType vec_ir_function_type = {
    .stringify = irFunctionVecToString,
    .match     = NULL,
    .release   = NULL,
    .type_str  = "IrFunction *",
};

Vec *irFunctionVecNew(void) {
    return vecNew(&vec_ir_function_type);
}

IrCtx *irCtxNew(Cctrl *cc) {
    IrCtx *ctx = malloc(sizeof(IrCtx));
    ctx->prog = malloc(sizeof(IrProgram));
    ctx->prog->functions = irFunctionVecNew();
    ctx->prog->globals = NULL;
    ctx->cc = cc;
    ctx->loop_depth = 0;
    memset(ctx->loop_stack, 0, sizeof(ctx->loop_stack));
    return ctx;
}

IrValue *irFnGetVar(IrFunction *func, u32 lvar_id) {
    return mapGetInt(func->variables, lvar_id);
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

IrBlock *irBlockNew(void) {
    IrBlock *block = irAlloc(sizeof(IrBlock));
    block->instructions = listNew();
    block->removed = 0;
    block->sealed = 0;
    block->id = ir_block_id++;
    return block;
}

void irBlockRelease(IrBlock *block) {
    listRelease(block->instructions, NULL);
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

IrValue *irConstInt(IrValueType type, s64 num) {
    IrValue *ir_value = irValueNew(type, IR_VAL_CONST_INT);
    ir_value->as._i64 = num;
    return ir_value;
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
    func->variables = irVarValueMapNew();
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

void irAddPhiIncoming(IrInstr *ir_phi_instr,
                      IrValue *ir_value, 
                      IrBlock *ir_block)
{
    IrPair *ir_phi_pair = irPairNew(ir_block, ir_value);
    vecPush(ir_phi_instr->extra.phi_pairs, ir_phi_pair);
}

static IrInstr *irJumpInternal(IrFunction *func,
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

IrInstr *irLoad(IrValue *ir_dest, IrValue *ir_value) {
    return irInstrNew(IR_LOAD, ir_dest, ir_value, NULL);
}

u32 irValueByteSize(IrValue *v) {
    if (!v) return 8;
    if (v->kind == IR_VAL_TMP ||
        v->kind == IR_VAL_LOCAL ||
        v->kind == IR_VAL_PARAM)
    {
        if (v->as.var.size > 0) return v->as.var.size;
    }
    switch (v->type) {
        case IR_TYPE_I8:  return 1;
        case IR_TYPE_I16: return 2;
        case IR_TYPE_I32: return 4;
        default:          return 8;
    }
}
