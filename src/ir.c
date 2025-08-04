#include <stdlib.h>
#include <stdio.h>

#include "arena.h"
#include "ir.h"
#include "ir-types.h"

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

IrFunction *irFunctionNew(AoStr *fname) {
    IrFunction *func = irAlloc(sizeof(IrFunction));
    func->name = fname;
    func->blocks = listNew();
    func->cfg = irBlockMapNew();
    return func;
}

void irFunctionRelease(IrFunction *func) {
    listRelease(func->blocks, (void (*)(void *))&irBlockRelease);
    mapRelease(func->cfg);
}

void irMakeFunction(Cctrl *cc, Ast *ast_func) {
    (void)cc;
    IrFunction *func = irFunctionNew(ast_func->fname);
    (void)func;
}

void irDump(Cctrl *cc) {
    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            irMakeFunction(cc, ast);
        }
    }
}
