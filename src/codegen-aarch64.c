#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "aostr.h"
#include "ir-debug.h"
#include "ir-types.h"
#include "util.h"

typedef struct AArch64Ctx {
    u16 stack_size;
    u16 current_label_id;
    AoStr *buf;
    IrProgram *ir_program;
    Map *var_offsets;
    Set *registers;
    u64 int_reg_cnt;
} AArch64Ctx;

AArch64Ctx *aarch64CtxNew(IrProgram *ir_program) {
    AArch64Ctx *ctx = malloc(sizeof(AArch64Ctx));
    memset(ctx, 0, sizeof(AArch64Ctx));
    ctx->ir_program = ir_program;
    ctx->buf = aoStrNew();
    ctx->var_offsets = mapNew(8, &map_uint_to_uint_type);
    ctx->registers = setNew(32, &set_uint_type);
    ctx->int_reg_cnt = 0;
    return ctx;
}

/* Does not release the assembly String buffer */
void aarch64CtxRelease(AArch64Ctx *ctx) {
    if (ctx) {
        mapRelease(ctx->var_offsets);
        setRelease(ctx->registers);
        free(ctx);
    }
}

u32 aarch64GetIntRegister(AArch64Ctx *ctx) {
    return ctx->int_reg_cnt++;
}

void aarch64ClearIntRegisters(AArch64Ctx *ctx) {
    ctx->int_reg_cnt = 0;
}

u32 aarch64CtxGetVarOffset(AArch64Ctx *ctx, u32 var_id) {
    return (u32)(u64)(void *)mapGetInt(ctx->var_offsets, var_id);
}

void aarch64CtxSetVarOffset(AArch64Ctx *ctx, u32 var_id, u32 offset) {
    mapAddIntOrErr(ctx->var_offsets, var_id, (void *)(u64)offset);
}

u32 aarch64GetFreeReg(AArch64Ctx *ctx) {

} 

static int alignTo(int value, int alignment) {
    return (value + alignment) & ~alignment;
}

void aarch64GenStore(AArch64Ctx *ctx, IrValue *dest, char *reg) {
    assert(dest);
    switch (dest->kind) {
        case IR_VAL_GLOBAL:
            break;

        case IR_VAL_TMP:
        case IR_VAL_LOCAL:
        case IR_VAL_PARAM: {
            u32 offset = aarch64CtxGetVarOffset(ctx, dest->as.var.id);
            assert(offset != 0);
            aoStrCatFmt(ctx->buf, "str %s, [sp, #%u]\n\t", reg, offset);
            break;
        }
        default:
            loggerPanic("Unsupported store: %s\n", irValueKindToString(dest->kind));
    }
}

void aarch64GenInstr(AArch64Ctx *ctx, IrInstr *instr, IrInstr *next_instr) {
    switch (instr->op) {
        case IR_NOP:
        case IR_ALLOCA:
            break;

        case IR_STORE: {
            switch (instr->dst->type) {
                case IR_TYPE_I64:
                    u32 offset = aarch64CtxGetVarOffset(ctx, instr->dst->as.var.id);
                    assert(offset != 0);
                    if (irIsConstInt(instr->r1)) {
                        aoStrCatFmt(ctx->buf, "mov x8, #%I\n\t", instr->r1->as._i64);
                    }
                    aoStrCatFmt(ctx->buf, "str x8, [sp, #%u]\n\t", offset);
                    break;
                case IR_TYPE_I8:
                case IR_TYPE_I16:
                case IR_TYPE_I32:
                case IR_TYPE_F64:
                case IR_TYPE_ARRAY:
                case IR_TYPE_ARRAY_INIT:
                case IR_TYPE_STRUCT:
                case IR_TYPE_FUNCTION:
                case IR_TYPE_ASM_FUNCTION:
                case IR_TYPE_PTR:
                case IR_TYPE_LABEL:
                default:
                    loggerPanic("Unhandled Type: %s\n",
                            irValueTypeToString(instr->dst->type));
            }
            break;
        }

        case IR_LOAD: {
            switch (instr->dst->type) {
                case IR_TYPE_I64:
                    u32 offset = aarch64CtxGetVarOffset(ctx, instr->r1->as.var.id);
                    u32 reg = aarch64GetIntRegister(ctx);
                    assert(offset != 0);
                    aoStrCatFmt(ctx->buf, "ldr x%u, [sp, #%u]\n\t", reg, offset);
                    break;
                case IR_TYPE_I8:
                case IR_TYPE_I16:
                case IR_TYPE_I32:
                case IR_TYPE_F64:
                case IR_TYPE_ARRAY:
                case IR_TYPE_ARRAY_INIT:
                case IR_TYPE_STRUCT:
                case IR_TYPE_FUNCTION:
                case IR_TYPE_ASM_FUNCTION:
                case IR_TYPE_PTR:
                case IR_TYPE_LABEL:
                default:
                    loggerPanic("Unhandled Type: %s\n",
                            irValueTypeToString(instr->dst->type));
            }
            break;
        }

        case IR_IADD: {
            aoStrCatFmt(ctx->buf, "add x8, x0, x1\n\t");
            aarch64ClearIntRegisters(ctx);
            break;
        }

        case IR_CALL: {
            IrValueArray *args = &instr->r1->as.array;
            for (u64 i = 0; i < args->values->size; ++i) {
                IrValue *arm = args->values->entries[i];
                switch (arm->kind) {
                    case IR_VAL_CONST_STR: {
                        aoStrCatFmt(ctx->buf, "adrp x%U, %S\n\t"
                                              "add x%U, x%U, :lo12:%S\n\t",
                                              i, arm->as.str.label,
                                              i, i, arm->as.str.label);
                        break;
                    }

                    case IR_VAL_LOCAL: {
                        u32 offset = aarch64CtxGetVarOffset(ctx, arm->as.var.id);
                        assert(offset != 0);
                        aoStrCatFmt(ctx->buf, "ldr x%U, [sp, #%u]\n\t", i, offset);
                        break;
                    }

                    case IR_VAL_TMP: {
                        /* @TODO - should eliminate temporaries */
                        aoStrCatFmt(ctx->buf, "ldr x%U, [sp, #-420]\n\t", i);
                        break;
                    }

                    case IR_VAL_CONST_INT:
                    case IR_VAL_CONST_FLOAT:
                    case IR_VAL_GLOBAL:
                    case IR_VAL_PARAM:
                    case IR_VAL_PHI:
                    case IR_VAL_LABEL:
                    case IR_VAL_UNDEFINED:
                    case IR_VAL_UNRESOLVED:
                    default:
                        loggerPanic("Unhandled argument kind: %s\n",
                                irValueKindToString(arm->kind));
                }
            }

            aoStrCatFmt(ctx->buf, "bl %S\n\t", args->label);
            break;
        }

        case IR_GEP:
        case IR_ISUB:
        case IR_IMUL:
        case IR_IDIV:
        case IR_UDIV:
        case IR_IREM:
        case IR_UREM:
        case IR_INEG:
        case IR_FADD:
        case IR_FSUB:
        case IR_FMUL:
        case IR_FDIV:
        case IR_FNEG:
        case IR_AND:
        case IR_OR:
        case IR_XOR:
        case IR_SHL:
        case IR_SHR:
        case IR_SAR:
        case IR_NOT:
        case IR_ICMP:
        case IR_FCMP:
        case IR_TRUNC:
        case IR_ZEXT:
        case IR_SEXT:
        case IR_FPTRUNC:
        case IR_FPEXT:
        case IR_FPTOUI:
        case IR_FPTOSI:
        case IR_UITOFP:
        case IR_SITOFP:
        case IR_PTRTOINT:
        case IR_INTTOPTR:
        case IR_BITCAST:
        case IR_RET:
        case IR_BR:
        case IR_JMP:
        case IR_LOOP:
        case IR_SWITCH:
        case IR_PHI:
        case IR_LABEL:
        case IR_SELECT:
        case IR_VA_ARG:
        case IR_VA_START:
        case IR_VA_END:
        default:
            loggerPanic("Unhandled op: %s\n", irOpcodeToString(instr));
    }
}

void aarch64EmitFunction(AArch64Ctx *ctx, IrFunction *func) {
    aoStrCatFmt(ctx->buf, ".globl %S\n"
                          "%S:\n\t", func->name, func->name);

    if (func->stack_space) {
        aoStrCatFmt(ctx->buf, "sub sp, sp, #%u\n\t", func->stack_space);
    }

    u32 stack_offset = func->stack_space;

    MapIter it;
    mapIterInit(func->variables, &it);
    while (mapIterNext(&it)) {
        IrValue *val = it.node->value;
        IrVar *var = &val->as.var;
        aarch64CtxSetVarOffset(ctx, var->id, stack_offset);
        stack_offset -= var->size;
    }

    char buf[4];
    for (u64 i = 0; i < func->params->size; ++i) {
        snprintf(buf, sizeof(buf), "x%llu", i);
        IrValue *param = func->params->entries[i];
        aarch64GenStore(ctx, param, buf);
    }

    listForEach(func->blocks) {
        IrBlock *block = listValue(IrBlock *, it);

        if (block != func->entry_block) {
            if (ctx->buf->data[ctx->buf->len - 1] == '\t') {
                ctx->buf->len--;
            }
            // aoStrCatFmt(ctx->buf, "BB%I:\n\t", block->id);
        }

        listForEach(block->instructions) {
            IrInstr *instr = listValue(IrInstr *, it);
            IrInstr *next_instr = NULL;
            if (it->next != block->instructions) {
                next_instr = listValue(IrInstr *, it->next);
            }
            /* While generating assembly we may have also nuked 
             * some instructions */
            if (instr->op == IR_NOP) continue;
            aarch64GenInstr(ctx, instr, next_instr);
        }
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
    aarch64CtxRelease(ctx);
    return buf;
}
