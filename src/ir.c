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

VecType vec_ir_funcion_type = {
    .stringify = vecIrFunctionToString,
    .match     = NULL,
    .release   = NULL,
    .type_str  = "long",
};

Vec *irFunctionVecNew(void) {
    return vecNew(&vec_ir_funcion_type);
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

Map *irBlockMapNew(void) {
    return mapNew(32, &map_u32_to_ir_block_type);
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
    func->cfg = irBlockMapNew();
    func->stack_space = 0;
    return func;
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

IrInstr *irAlloca(IrCtx *ctx, u32 var_id, AstType *ast_type) {
    IrValueType ir_type = irConvertType(ast_type);
    IrValue *tmp = irTmp(ir_type, var_id);
    IrValue *ir_size = irConstInt(ir_type, ast_type->size);
    IrInstr *ir_alloca = irInstrNew(IR_ALLOCA, tmp, ir_size, NULL);
    irBlockAddInstr(ctx, ir_alloca);
    return ir_alloca;
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
            IrInstr *ir_alloca = irAlloca(ctx, ast->declvar->lvar_id, ast->declvar->type);
            irInstrPrint(ir_alloca);
            printf("size = %d\n", ast->declvar->type->size);
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
    IrBlock *exit = irBlockNew(func);
    func->exit_block = exit;
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
