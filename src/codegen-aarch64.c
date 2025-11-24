#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "ir-types.h"


typedef struct AArch64Ctx {
    u16 stack_size;
    u16 current_label_id;
    AoStr *buf;
    IrProgram *ir_program;
    Map *var_offsets;
} AArch64Ctx;

AArch64Ctx *aarch64CtxNew(IrProgram *ir_program) {
    AArch64Ctx *ctx = malloc(sizeof(AArch64Ctx));
    memset(ctx, 0, sizeof(AArch64Ctx));
    ctx->ir_program = ir_program;
    ctx->buf = aoStrNew();
    return ctx;
}

static int alignTo(int value, int alignment) {
    return (value + alignment) & ~alignment;
}

void aarch64EmitFunction(AArch64Ctx *ctx, IrFunction *func) {
    aoStrCatFmt(ctx->buf, ".globl %S\n"
                          "%S:\n\t", func->name, func->name);

    if (func->stack_space) {
        aoStrCatFmt(ctx->buf, "sub sp, sp, #%u\n\t", func->stack_space);
    }
}

AoStr *aarch64GenCode(IrCtx *ir_ctx) {
    IrProgram *program = ir_ctx->prog;
    AArch64Ctx *ctx = aarch64CtxNew(program);

    for (u64 i = 0; i < program->functions->size; ++i) {
        IrFunction *func = program->functions->entries[i];
        aarch64EmitFunction(ctx, func);
    }
    AoStr *buf = ctx->buf;
    free(ctx);
    return buf;
}
