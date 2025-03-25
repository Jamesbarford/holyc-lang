#include <assert.h>
#include <stdlib.h>
#include <math.h>

#include "aostr.h"
#include "ast.h"
#include "ir.h"
#include "list.h"
#include "map.h"
#include "util.h"
#include "codegen-x64.h"

/* System V ABI, for parameters passed as registers */
const char *x64_int_param_regs[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
const char *x64_float_param_regs[] = {"%xmm0", "%xmm1", "%xmm2", "%xmm3", 
                                      "%xmm4", "%xmm5", "%xmm6", "%xmm7"};
typedef struct X64Ctx X64Ctx;
typedef struct X64RegState X64RegState;

struct X64RegState {
    int used[16];         // General purpose registers
    int used_xmm[16];     // XMM registers for floating point
    const char *names[16]; // Register names
    const char *xmm_names[16]; // XMM register names
};

struct X64Ctx {
    int stack_size;
    int current_label_id;
    aoStr *buf;
    IrProgram *ir_program;
    StrMap *var_offsets;
    X64RegState reg_state;
};

static int alignTo(int value, int alignment) {
    return (value + alignment) & ~alignment;
}

aoStr *codeGenNormaliseFunctionName(aoStr *fname) {
    aoStr *normalised = aoStrNew();
    if (!strncmp(OS_STR, str_lit("apple"))) {
        aoStrPutChar(normalised, '_');
    }

    if (fname->len == 4 && !strncasecmp(fname->data, str_lit("main"))) {
        aoStrCatLen(normalised, str_lit("main"));
    } else {
        aoStrCatFmt(normalised, "%S", fname);
    }

    return normalised;
}

static void x64RegStateInit(X64RegState *state) {
    memset(state->used, 0, sizeof(state->used));
    memset(state->used_xmm, 0, sizeof(state->used_xmm));

    state->names[0] = "%rax";
    state->names[1] = "%rbx";
    state->names[2] = "%rcx";
    state->names[3] = "%rdx";
    state->names[4] = "%rsi";
    state->names[5] = "%rdi";
    state->names[6] = "%r8";
    state->names[7] = "%r9";
    state->names[8] = "%r10";
    state->names[9] = "%r11";
    state->names[10] = "%r12";
    state->names[11] = "%r13";
    state->names[12] = "%r14";
    state->names[13] = "%r15";

    for (int i = 0; i < 16; ++i) {
        char tmp[16];
        int len = snprintf(tmp,sizeof(tmp), "%%xmm%d",i);
        state->xmm_names[i] = strndup(tmp, len);
    }
}

int x64AllocRegister(X64RegState *state) {
    /* @Speed - bitvector */
    for (int i = 0; i < 16; ++i) {
        if (!state->used[i]) {
            state->used[i] = 1;
            return i;
        }
    }
    return -1;
}

int x64AllocFloatRegister(X64RegState *state) {
    /* @Speed - bitvector */
    for (int i = 0; i < 16; ++i) {
        if (!state->used_xmm[i]) {
            state->used_xmm[i] = 1;
            return i;
        }
    }
    return -1;
}

void x64FreeRegister(X64RegState *state, int reg) {
    if (reg >= 0 && reg < 16) {
        state->used[reg] = 0;
    }
}

void x64FreeFloatRegister(X64RegState *state, int reg) {
    if (reg >= 0 && reg < 16) {
        state->used_xmm[reg] = 0;
    }
}

const char *x64GetRegisterName(X64RegState *state, int reg) {
    return state->names[reg];
}

const char *x64GetFloatRegisterName(X64RegState *state, int reg) {
    return state->xmm_names[reg];
}

static X64Ctx *x64CtxNew(IrProgram *ir_program) {
    X64Ctx *ctx = (X64Ctx *)malloc(sizeof(X64Ctx));
    ctx->ir_program = ir_program;
    ctx->buf = aoStrNew();
    ctx->var_offsets = strMapNew(16);
    x64RegStateInit(&ctx->reg_state);
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
        case IR_TYPE_I16: aoStrCatFmt(ctx->buf,".short %i\n", value->i64); break;
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
    //assert(label);
    aoStrCatFmt(ctx->buf, "%S:\n\t", label);
    aoStrCatFmt(ctx->buf, ".asciz \"%S\"\n", value->str);
}

static void x64ArrayGen(X64Ctx *ctx, aoStr *label, IrValue *value) {
    PtrVec *ir_array = value->array_.values;
    for (int i = 0; i < ir_array->size; ++i) {
        IrValue *it = vecGet(IrValue *, ir_array, i);
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
                aoStrCatFmt(ctx->buf,".globl %S\n.data\n%S:\n\t", global->name);
                x64ArrayGen(ctx, global->name, global);
            } else {
                /* @Static is not handled correctly, should perhaps be a flag */
                switch (global_value->kind) {
                    case IR_VALUE_CONST_INT: { 
                        aoStrCatFmt(ctx->buf,".globl %S\n.data\n%S:\n\t", global->name, global->name);
                        x64EmitInt(ctx, global_value);
                        break;
                    }
                    case IR_VALUE_CONST_FLOAT: {
                        aoStrCatFmt(ctx->buf,".globl %S\n.data\n%S:\n\t", global->name, global->name);
                        x64EmitFloat(ctx, global_value);
                        break;
                    }
                    /* 90% confident the strings hashtable will have the string */
                    case IR_VALUE_CONST_STR: continue; // x64EmitString(ctx, global->name, global_value); break;
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
        aoStr shell;
        memset(&shell,0,sizeof(aoStr));

        while ((it = strMapNext(iter)) != NULL) {
            IrValue *value = (IrValue *)it->value;
            if (value->kind == IR_VALUE_GLOBAL) continue;
            shell.data = it->key;
            shell.len = it->key_len;
            x64EmitString(ctx, &shell, value);
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

void x64GenFloatLoad(X64Ctx *ctx, IrValue *value, const char *reg) {
    assert(value);
    switch (value->kind) {
        case IR_VALUE_CONST_FLOAT:
            /* @Float - this label should be created when we create the stack 
             * for the function */
            loggerPanic("Unimplemented: IR_VALUE_CONST_FLOAT\n");

        case IR_VALUE_GLOBAL:
            aoStrCatFmt(ctx->buf, "movsd %S(%%rip), %s\n\t", value->name, reg);
            break;

        case IR_VALUE_PARAM:
        case IR_VALUE_LOCAL:
        case IR_VALUE_TEMP: {
            long offset = (long)strMapGetAoStr(ctx->var_offsets, value->name);
            assert(offset != 0);
            aoStrCatFmt(ctx->buf, "movsd %I(%%rbp), %s\n\t", offset, reg);
            break;
        }

        default:
            loggerPanic("Unsupported Float load: %s\n", irValueKindToString(value->kind));
    }
}

void x64GenFloatStore(X64Ctx *ctx, IrValue *dest, const char *reg) {
    assert(dest);
    switch (dest->kind) {
        case IR_VALUE_GLOBAL:
            aoStrCatFmt(ctx->buf, "movsd %s, %S(%%rip)\n\t", reg, dest->name);
            break;

        case IR_VALUE_TEMP:
        case IR_VALUE_LOCAL:
        case IR_VALUE_PARAM: {
            long offset = (long)strMapGetAoStr(ctx->var_offsets, dest->name);
            if (!offset) {
                loggerWarning("Allocating a store from the stack\n");
                offset = ctx->stack_size;
                ctx->stack_size += 8;
                strMapAddAoStr(ctx->var_offsets, dest->name, ptrcast(offset));
            }
            aoStrCatFmt(ctx->buf, "movsd %s, %I(%%rbp)\n\t", reg, offset);
            break;
        }

        default:
            loggerPanic("Unsupported store: %s\n", irValueKindToString(dest->kind));
    }
}

void x64GenLoad(X64Ctx *ctx, IrValue *value, const char *reg) {
    assert(value);
    switch (value->kind) {
        case IR_VALUE_CONST_INT:
            aoStrCatFmt(ctx->buf, "movq $%I, %s\n\t", value->i64, reg);
            break;

        case IR_VALUE_CONST_STR:
            aoStrCatFmt(ctx->buf, "leaq %S(%%rip), %s\n\t", value->name, reg);
            break;

        case IR_VALUE_GLOBAL:
            aoStrCatFmt(ctx->buf, "movq %S(%%rip), %s\n\t", value->name, reg);
            break;

        case IR_VALUE_TEMP:
        case IR_VALUE_LOCAL:
        case IR_VALUE_PARAM: {
            long offset = (long)strMapGetAoStr(ctx->var_offsets, value->name);
            assert(offset != 0);
            aoStrCatFmt(ctx->buf, "movq %I(%%rbp), %s\n\t", offset, reg);
            break;
        }

        default:
            loggerPanic("Unsupported load: %s\n", irValueKindToString(value->kind));
    }
}

void x64GenStore(X64Ctx *ctx, IrValue *dest, const char *reg) {
    assert(dest);
    switch (dest->kind) {
        case IR_VALUE_GLOBAL:
            aoStrCatFmt(ctx->buf, "movq %s, %S(%%rip)\n\t", reg, dest->name);
            break;

        case IR_VALUE_TEMP:
        case IR_VALUE_LOCAL:
        case IR_VALUE_PARAM: {
            long offset = (long)strMapGetAoStr(ctx->var_offsets, dest->name);
            if (!offset) {
                loggerWarning("Allocating a store from the stack\n");
                offset = ctx->stack_size;
                ctx->stack_size += 8;
                strMapAddAoStr(ctx->var_offsets, dest->name, ptrcast(offset));
            }
            aoStrCatFmt(ctx->buf, "movq %s, %I(%%rbp)\n\t", reg, offset);
            break;
        }

        default:
            loggerPanic("Unsupported store: %s\n", irValueKindToString(dest->kind));
    }
}

void x64GenerateInstruction(X64Ctx *ctx, IrInstr *instr) {
    switch (instr->opcode) {
        case IR_OP_ALLOCA:
            /* We have already allocated in on crack */
            break;

        case IR_OP_LOAD: {
            if (instr->op2->type == IR_TYPE_F64) {
                int reg = x64AllocFloatRegister(&ctx->reg_state);
                const char *reg_name = x64GetFloatRegisterName(&ctx->reg_state, reg); 
                x64GenFloatLoad(ctx, instr->op2, reg_name);
                x64GenFloatStore(ctx, instr->op1, reg_name);
                x64FreeFloatRegister(&ctx->reg_state, reg);
            } else {
                int reg = x64AllocRegister(&ctx->reg_state);
                const char *reg_name = x64GetRegisterName(&ctx->reg_state, reg); 
                x64GenLoad(ctx, instr->op2, reg_name);
                x64GenStore(ctx, instr->op1, reg_name);
                x64FreeRegister(&ctx->reg_state, reg);
            }
            break;
        }

        case IR_OP_CALL: {
            PtrVec *fn_args = instr->op1->array_.values;
            if (fn_args && fn_args->size > 0) {
                int int_reg_idx = 0;
                int float_reg_idx = 0;
                int stack_offset = 0;

                for (int i = fn_args->size-1; i >= 0; --i) {
                    IrValue *arg = vecGet(IrValue *, fn_args, i);

                    if (i >= 6) {
                        if (arg->type == IR_TYPE_F64) {
                            int arg_reg = x64AllocFloatRegister(&ctx->reg_state);
                            const char *arg_name = x64GetFloatRegisterName(&ctx->reg_state, arg_reg); 
                            x64GenFloatLoad(ctx, arg, arg_name);
                            aoStrCatFmt(ctx->buf, "movsd %s, -%I(%%rsp)\n\t",
                                    arg_name, stack_offset);
                            x64FreeFloatRegister(&ctx->reg_state, arg_reg);
                        } else {
                            int arg_reg = x64AllocRegister(&ctx->reg_state);
                            const char *arg_name = x64GetRegisterName(&ctx->reg_state, arg_reg); 
                            x64GenLoad(ctx, arg, arg_name);
                            aoStrCatFmt(ctx->buf, "movq %s, -%I(%%rsp)\n\t",
                                    arg_name, stack_offset);
                            x64FreeRegister(&ctx->reg_state, arg_reg);
                        }

                    }
                }

                if (stack_offset > 0) {
                    aoStrCatFmt(ctx->buf, "subq $%I, %%rsp\n\t", stack_offset);
                }


                for (int i = 0; i < fn_args->size && i < 6; ++i) {
                    IrValue *arg = vecGet(IrValue *, fn_args, i);

                    if (arg->type == IR_TYPE_F64) {
                        int arg_reg = x64AllocFloatRegister(&ctx->reg_state);
                        const char *arg_name = x64GetFloatRegisterName(&ctx->reg_state, arg_reg); 
                        x64GenFloatLoad(ctx, arg, arg_name);
                        aoStrCatFmt(ctx->buf, "movsd %s, %s\n\t", arg_name,
                                    x64_float_param_regs[float_reg_idx++]);
                        x64FreeFloatRegister(&ctx->reg_state, arg_reg);
                    } else {
                            int arg_reg = x64AllocRegister(&ctx->reg_state);
                            const char *arg_name = x64GetRegisterName(&ctx->reg_state, arg_reg); 
                            printf("here:%s \n", irValueToString(arg)->data);
                            x64GenLoad(ctx, arg, arg_name);
                            aoStrCatFmt(ctx->buf, "movq %s, %s\n\t",
                                    arg_name, x64_int_param_regs[int_reg_idx++]);
                            x64FreeRegister(&ctx->reg_state, arg_reg);
                    }
                }

                if (instr->op1->name->data[0] != '%') {
                    aoStr *fname = codeGenNormaliseFunctionName(instr->op1->name);
                    aoStrCatFmt(ctx->buf, "call %S\n\t", fname);
                    aoStrRelease(fname);
                } else {
                    aoStrCatFmt(ctx->buf, "call %S\n\t", instr->op1->name);
                }

                if (fn_args->size > 6) {
                    int stack_size = (fn_args->size - 6) * 8;
                    aoStrCatFmt(ctx->buf, "    addq $%I, %%rsp\n", stack_size);
                }

                if (instr->result) {
                    if (instr->result->type == IR_TYPE_F64) {
                        x64GenFloatStore(ctx,instr->result,"%xmm0");
                    } else if (instr->result->type != IR_TYPE_VOID) {
                        x64GenStore(ctx,instr->result,"%rax");
                    }
                }
            }
            break;
        }

        case IR_OP_RET:
            break;

        case IR_OP_STORE:
        case IR_OP_GEP:
        case IR_OP_IADD:
        case IR_OP_ISUB:
        case IR_OP_IMUL:
        case IR_OP_IDIV:
        case IR_OP_UDIV:
        case IR_OP_IREM:
        case IR_OP_UREM:
        case IR_OP_INEG:
        case IR_OP_FADD:
        case IR_OP_FSUB:
        case IR_OP_FMUL:
        case IR_OP_FDIV:
        case IR_OP_FNEG:
        case IR_OP_AND:
        case IR_OP_OR:
        case IR_OP_XOR:
        case IR_OP_SHL:
        case IR_OP_SHR:
        case IR_OP_SAR:
        case IR_OP_NOT:
        case IR_OP_ICMP:
        case IR_OP_FCMP:
        case IR_OP_TRUNC:
        case IR_OP_ZEXT:
        case IR_OP_SEXT:
        case IR_OP_FPTRUNC:
        case IR_OP_FPEXT:
        case IR_OP_FPTOUI:
        case IR_OP_FPTOSI:
        case IR_OP_UITOFP:
        case IR_OP_SITOFP:
        case IR_OP_PTRTOINT:
        case IR_OP_INTTOPTR:
        case IR_OP_BITCAST:
        case IR_OP_BR:
        case IR_OP_JMP:
        case IR_OP_LOOP:
        case IR_OP_SWITCH:
        case IR_OP_PHI:
        case IR_OP_LABEL:
        case IR_OP_SELECT:
        case IR_OP_VA_ARG:
        case IR_OP_VA_START:
        case IR_OP_VA_END:
        default:
            loggerPanic("Unhandled op: %s for instruction:\n`%s`\n",
                    irOpcodeToString(instr),
                    irInstrToString(instr)->data);
             break;
    }
}

static void x64CalculateFunctionStack(X64Ctx *ctx, IrFunction *func) {
    strMapClear(ctx->var_offsets);
    ctx->stack_size = 0;
    int offset = -8;

    for (int i = 0; i < func->params->size; ++i) {
        IrValue *param = vecGet(IrValue *, func->params, i);
        int size = 8;
        if (param->type == IR_TYPE_I8) size = 1;
        else if (param->type == IR_TYPE_I16) size = 2;
        else if (param->type == IR_TYPE_I32) size = 4;

        size = alignTo(size, 8);
        offset -= size;
        strMapAddAoStr(ctx->var_offsets, param->name, ptrcast(offset));
    }

    listForEach(func->blocks) {
        IrBlock *block = listValue(IrBlock *, it);

        /* We count up all of the ALLOCA instructions, calculate the stack 
         * space and */
        for (List *node = block->instructions->next; node != block->instructions; node = node->next) {
            IrInstr *instr = listValue(IrInstr *, node);

            if (instr->opcode == IR_OP_ALLOCA) {
                IrValue *local = instr->op1;
                int size = 8;
                if (local->type == IR_TYPE_I8) size = 1;
                else if (local->type == IR_TYPE_I16) size = 2;
                else if (local->type == IR_TYPE_I32) size = 4;
                else if (local->type == IR_TYPE_I64) size = 8;
                else if (local->type == IR_TYPE_ARRAY) {
                    /* Assumes a 64 bit element */
                    size = 8 * local->array_.values->size;
                }
            
                size = alignTo(size, 8);
                offset -= size;

                strMapAddAoStr(ctx->var_offsets, local->name, ptrcast(offset));
            }
        }
    }

    /* Stack needs to be aligned to 16bytes*/
    ctx->stack_size = (-offset + 15) & ~15;
}

static void x64EmitFunction(X64Ctx *ctx, IrFunction *func) {
    x64CalculateFunctionStack(ctx,func);

    aoStr *fname = codeGenNormaliseFunctionName(func->name);

    aoStrCatFmt(ctx->buf, ".text\n"
                          ".globl %S\n%S:\n\t"
                          "push %%rbp\n\t"
                          "movq %%rsp, %%rbp\n\t",
                          fname,fname);

    if (ctx->stack_size > 0) {
        aoStrCatFmt(ctx->buf, "subq $%i, %%rsp\n\t", ctx->stack_size);
    }

    for (int i = 0; i < func->params->size && i < 6; ++i) {
        IrValue *param = vecGet(IrValue *, func->params, i);
        long offset = (long)strMapGetAoStr(ctx->var_offsets, param->name);

        if (param->type == IR_TYPE_F64) {
            aoStrCatFmt(ctx->buf, "movsd %%%s, %ld(%%rbp)\n\t",
                        x64_float_param_regs[i], offset);
        } else {
            /* @Mov
             * Get the right `mov` to sign extend etc.. into a 64 bit 
             * stack slot */
            aoStrCatFmt(ctx->buf, "movq %%%s, %ld(%%rbp)\n\t",
                        x64_int_param_regs[i], offset);
        }
    }

    if (listEmpty(func->blocks)) {
        listForEach(func->entry_block->instructions) {
            IrInstr *instr = listValue(IrInstr *, it);
            x64GenerateInstruction(ctx, instr);
        }
    } else {
        for (List *node = func->blocks->next; node != func->blocks; node = node->next) {
            IrBlock *block = listValue(IrBlock *, node);

            if (block != func->entry_block) {
                aoStrCatFmt(ctx->buf, ".L%i\n\t", block->id);
            }

            listForEach(block->instructions) {
                IrInstr *instr = listValue(IrInstr *, it);
                x64GenerateInstruction(ctx, instr);
            }
        }
    }

    aoStrCatFmt(ctx->buf, "leave\n\tret\n"); 
}

static void x64FunctionsGen(X64Ctx *ctx) {
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
    x64FunctionsGen(ctx);


    aoStr *x64_asm = ctx->buf;


    x64CtxRelease(ctx);
    return x64_asm;
}
