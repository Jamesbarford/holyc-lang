#include <stdlib.h>
#include <math.h>

#include "aostr.h"
#include "ir.h"
#include "map.h"
#include "util.h"
#include "codegen-x64.h"

typedef struct X64Ctx X64Ctx;

struct X64Ctx {
    int stack_size;
    int current_label_id;
    aoStr *buf;
    IrProgram *ir_program;
    StrMap *var_offsets;
}; 

static X64Ctx *x64CtxNew(IrProgram *ir_program) {
    X64Ctx *ctx = (X64Ctx *)malloc(sizeof(X64Ctx));
    ctx->ir_program = ir_program;
    ctx->buf = aoStrNew();
    ctx->var_offsets = strMapNew(16);
    ctx->stack_size = 0;
    ctx->current_label_id = 0;
    return ctx;
}

static int getIntSize(IrValueType type) {
    switch (type) {
        case IR_TYPE_I8:  return 1;
        case IR_TYPE_I16: return 2;
        case IR_TYPE_I32: return 4;
        case IR_TYPE_I64: return 8;
        default: loggerPanic("Invalid integer type: %s\n", irValueTypeToString(type));
    }
}

static uint64_t ieee754(double _f64) {
    if (_f64 == 0.0) return 0;

    /* Calculate exponent and adjust fraction */
    long double base2_exp = floorl(log2l(fabs(_f64)));
    long double exponet2_removed = ldexpl(_f64, -base2_exp - 1);

    /* Initialize fraction and calculate it bit by bit */
    uint64_t fraction = 0;
    long double digit = 0.5;  // Start with 1/2
    for (long i = 0; i != 53; i++) {
        if (exponet2_removed >= digit) {
            exponet2_removed -= digit;
            fraction |= 1ULL << (52 - i);
        }
        /* Move to the next digit (1/4, 1/8, ...) */
        digit *= 0.5;
    }

    /* Calculate exponent representation */
    uint64_t exponent = ((1 << 10) - 1) + base2_exp;

    /* Handle sign bit */
    uint64_t sign = (_f64 < 0.0) ? 1 : 0;

    /* Assemble the IEEE 754 representation */
    return (sign << 63) |
           ((exponent & 0x7FF) << 52) |
           (fraction & ~(1ULL << 52));
}

static void x64CtxRelease(X64Ctx *ctx) {
    /* The ctx only owns the offsets as the other parts of the compiler's 
     * structs are created else where. */
    strMapRelease(ctx->var_offsets);
    free(ctx);
}

static void x64EmitGlobaUninitlisedInt(X64Ctx *ctx, aoStr *label, IrValue *value) {
    switch (value->type) {
        case IR_TYPE_I8: aoStrCatFmt(ctx->buf, ".global %S\n\t.comm %S, 1, 1\n", label, label); break;
        case IR_TYPE_I16:aoStrCatFmt(ctx->buf, ".global %S\n\t.comm %S, 2, 2\n", label, label); break;
        case IR_TYPE_I32:aoStrCatFmt(ctx->buf, ".global %S\n\t.comm %S, 4, 4\n", label, label); break;
        case IR_TYPE_I64:aoStrCatFmt(ctx->buf, ".global %S\n\t.comm %S, 8, 8\n", label, label); break;
        default: loggerPanic("Invalid integer type: %s\n", irValueToString(value)->data);
    }
}

static void x64EmitInt(X64Ctx *ctx, IrValue *value) {
    switch (value->type) {
        case IR_TYPE_I8:  aoStrCatFmt(ctx->buf,".byte %i\n", value->i64); break;
        case IR_TYPE_I16: aoStrCatFmt(ctx->buf,".long %i\n", value->i64); break;
        case IR_TYPE_I32: aoStrCatFmt(ctx->buf,".long %i\n", value->i64); break;
        case IR_TYPE_I64: aoStrCatFmt(ctx->buf,".quad %i\n", value->i64); break;
        default: loggerPanic("Invalid integer type: %s\n", irValueToString(value)->data);
    }
}

static void x64EmitFloat(X64Ctx *ctx, IrValue *value) {
    long ieee = ieee754(value->f64);
    aoStrCatFmt(ctx->buf,".quad %X\n", ieee);
}

static void x64EmitString(X64Ctx *ctx, aoStr *label, IrValue *value) {
    aoStrCatFmt(ctx->buf, "%S:\n\t", label);
    aoStrCatFmt(ctx->buf, ".asciz \"%S\"\n", value->str);
}

static void x64ArrayGen(X64Ctx *ctx, aoStr *label, IrValue *value) {
    PtrVec *ir_array = value->array_.values;
    for (int i = 0; i < ir_array->size; ++i) {
        IrValue *it = (IrValue *)ir_array->entries[i];
        switch (it->kind) {
            case IR_VALUE_CONST_INT:   x64EmitInt(ctx, it); break;
            case IR_VALUE_CONST_FLOAT: x64EmitFloat(ctx, it); break;
            case IR_VALUE_CONST_STR:   x64EmitString(ctx, label, it); break;
            default: loggerPanic("Cannot emit array for: %s\n", irValueToString(it)->data);
        }
    }
}

static void x64GlobalGen(X64Ctx *ctx) {
    StrMapIterator *iter = strMapIteratorNew(ctx->ir_program->global_variables);
    StrMapNode *it = NULL;
    while ((it = strMapNext(iter)) != NULL) {
        IrValue *global = (IrValue *)it->value;
        IrValue *global_value = global->global.value;
    
        if (global_value) {
            if (global->type == IR_TYPE_ARRAY || global->type == IR_TYPE_ARRAY_INIT) {
                aoStrCatFmt(ctx->buf,".globl %S\n.data\n\t%S:\n\t", global->name);
                x64ArrayGen(ctx, global->name, global);
            } else {
                /* @Static is not handled correctly, should perhaps be a flag */
                switch (global_value->kind) {
                    case IR_VALUE_CONST_INT: { 
                        aoStrCatFmt(ctx->buf,".globl %S\n.data\n\t%S:\n\t", global->name, global->name);
                        x64EmitInt(ctx, global_value);
                        break;
                    }
                    case IR_VALUE_CONST_FLOAT: {
                        aoStrCatFmt(ctx->buf,".globl %S\n.data\n\t%S:\n\t", global->name, global->name);
                        x64EmitFloat(ctx, global_value);
                        break;
                    }
                    case IR_VALUE_CONST_STR: x64EmitString(ctx, global->name, global_value); break;
                    default:
                        loggerPanic("Unhandled: %s %s\n",
                                irValueKindToString(global_value->kind),
                                irValueTypeToString(global_value->type));
                }
            }
        } else {
            switch (global->type) {
                    case IR_TYPE_I8:
                    case IR_TYPE_I16:
                    case IR_TYPE_I32:
                    case IR_TYPE_I64:
                        x64EmitGlobaUninitlisedInt(ctx, global->name, global);
                        break;

                    case IR_TYPE_PTR:
#if IS_MACOS
                        aoStrCatFmt(ctx->buf,".globl %S\n\t", global->name);
                        aoStrCatFmt(ctx->buf, ".zerofill __DATA,__common,%S,%i,%i\n\t",
                                global->name,
                                8,
                                (int)log2((double)8));
#else
                        aoStrCatFmt(buf,".globl %S\n\t.comm %S, %i, %u\n\t", global->name, global->name,
                                8,
                                roundUpToNextPowerOf2((unsigned long)8));
#endif
                        break;
                    case IR_TYPE_F64:
                        break;

                    case IR_TYPE_ARRAY:
                    case IR_TYPE_ARRAY_INIT:
                    case IR_TYPE_STRUCT:
                    case IR_TYPE_FUNCTION:
                    case IR_TYPE_ASM_FUNCTION:
                    case IR_TYPE_LABEL:
                    default:
                        loggerPanic("Unhandled: %s %s %s\n",
                                global->name->data,
                                irValueKindToString(global->kind),
                                irValueTypeToString(global->type));
                }
        }
    }
    strMapIteratorRelease(iter);
}

static void x64StringGen(X64Ctx *ctx) {
    if (ctx->ir_program->strings->size) {
        StrMapIterator *iter = strMapIteratorNew(ctx->ir_program->strings);
        StrMapNode *it = NULL;
        while ((it = strMapNext(iter)) != NULL) {
            IrValue *value = (IrValue *)it->value;
            x64EmitString(ctx, value->name, value);
        }
        strMapIteratorRelease(iter);
    }
}

static void x64EmitInstruction(X64Ctx *ctx, IrInstr *instr) {
    return;
}

static void x64EmitBlock(X64Ctx *ctx, IrBlock *block) {
    return;
}

static void x64EmitFunction(X64Ctx *ctx, IrFunction *func) {
    listForEach(func->blocks) {
        IrBlock *block = listValue(IrBlock *, it);
        /* Calculate stack space */

    }
}

static void x64EmitFunctions(X64Ctx *ctx) {
    PtrVec *funcs = ctx->ir_program->functions;
    for (int i = 0; i < funcs->size; ++i) {
        IrFunction *func = vecGet(IrFunction *, funcs, i);
        x64EmitFunction(ctx, func);
    }
}


aoStr *x64CodeGen(IrProgram *ir_program) {
    X64Ctx *ctx = x64CtxNew(ir_program);

    x64GlobalGen(ctx);
    x64StringGen(ctx);
    aoStr *x64_asm = ctx->buf;


    x64CtxRelease(ctx);
    return x64_asm;
}
