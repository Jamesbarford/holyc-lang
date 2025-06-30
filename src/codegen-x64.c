#include <assert.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#include <stdarg.h>

#include "aostr.h"
#include "ir.h"
#include "ir-interp.h"
#include "list.h"
#include "map.h"
#include "util.h"
#include "codegen-x64.h"
#include "version.h"

/*==================== DATA STRUCTURE SPECIALISATION =========================*/

static MapType irvalue_to_reg_map_type = {
    .match = (mapKeyMatch *)&irValuesEq,
    .hash = (mapKeyHash *)&irValueHash,
    .get_key_len = (mapKeyLen *)&irValueKeyLen,
    .key_to_string = (mapKeyToString *)&irValueKeyStringify,
    .value_to_string = (mapValueToString *)&aoStrDupCString,
    .value_release = NULL,
    .value_type = "char *",
    .key_type = "IrValue *",
};

/* `Map<AoStr *, long>` */
static MapType offset_map_type = {
    .match = (mapKeyMatch *)&aoStrEq,
    .hash = (mapKeyHash *)&aoStrHashFunction,
    .get_key_len = (mapKeyLen *)&aoStrGetLen,
    .key_to_string = (mapKeyToString *)&aoStrIdentity,
    .value_to_string = (mapValueToString *)&intMapKeyToString,
    .value_release = NULL,
    .value_type = "long",
    .key_type = "AoStr *",
};

static Map *offsetMap(void) {
    return mapNew(32, &offset_map_type);
}

int x64RegEq(char *s1, char *s2) {
    return !strcmp(s1,s2);
}

unsigned long x64RegHash(char *s1) {
    AoStr tmp = {.data = (void*)s1, .len = strlen(s1), .capacity = 0};
    return aoStrHashFunction(&tmp);
}

static MapType reg_to_irvalue_map_type = {
    .match = (mapKeyMatch *)&x64RegEq,
    .hash = (mapKeyHash *)&x64RegHash,
    .get_key_len = (mapKeyLen *)&strlen,
    .key_to_string = (mapKeyToString *)&aoStrDupCString,
    .value_to_string = (mapValueToString *)&irValueKeyStringify,
    .value_release = NULL,
    .value_type = "IrValue *",
    .key_type = "char *",
};

/* `Map<IrValue *, char *>` */
static Map *irValueToRegMapNew(void) {
    return mapNew(32, &irvalue_to_reg_map_type);
}

/* `Map<char *, IrValue *>` */
static Map *irRegToValueMapNew(void) {
    return mapNew(32, &reg_to_irvalue_map_type);
}

/*==================== CODE GEN ==============================================*/
/* System V ABI, for parameters passed as registers */
char *x64_int_param_regs[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
char *x64_float_param_regs[] = {"%xmm0", "%xmm1", "%xmm2", "%xmm3", 
                                      "%xmm4", "%xmm5", "%xmm6", "%xmm7"};
typedef struct X64Ctx X64Ctx;
typedef struct X64RegState X64RegState;

struct X64RegState {
    int used[16];         // General purpose registers
    int used_xmm[16];     // XMM registers for floating point
    char *names[16]; // Register names
    char *xmm_names[16]; // XMM register names
    Map *ir_to_reg; /* `Map<IrValue *, AoStr *>`, value to it's register 
                        *  or NULL */
    Map *reg_to_ir; /* `Map<char *, IrValue *>` register to a value */
};

struct X64Ctx {
    int stack_size;
    int current_label_id;
    AoStr *buf;
    IrProgram *ir_program;
    Map *var_offsets;
    X64RegState reg_state;
};

static int alignTo(int value, int alignment) {
    return (value + alignment) & ~alignment;
}

static AoStr *codeGenNormaliseFunctionName(AoStr *fname) {
    AoStr *normalised = aoStrNew();
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
    state->ir_to_reg = irValueToRegMapNew();
    state->reg_to_ir = irRegToValueMapNew();
}

static void x64RegStateClearMaps(X64RegState *state) {
    mapClear(state->ir_to_reg);
    mapClear(state->reg_to_ir);
    memset(state->used, 0, sizeof(state->used));
    memset(state->used_xmm, 0, sizeof(state->used_xmm));
}

static void x64RegStateClear(X64RegState *state) {
    x64RegStateClearMaps(state);
}

int x64ValueInReg(X64RegState *state, IrValue *value) {
    if (!value || !value->name) return 0;
    if (!mapHas(state->ir_to_reg, value)) return 0;
    char *reg = (char *)mapGet(state->ir_to_reg, value);
    IrValue *stored = (IrValue *)mapGet(state->reg_to_ir, reg);
    return stored == value;
}

char *x64GetRegForValue(X64RegState *state, IrValue *value) {
    if (!value || !value->name) return NULL;
    return (char *)mapGet(state->ir_to_reg, value);
}

int x64GetRegisterIdx(char *reg_name) {
    if (!strncmp(reg_name, str_lit("%rax"))) return 0;
    else if (!strncmp(reg_name, str_lit("%rbx"))) return 1;
    else if (!strncmp(reg_name, str_lit("%rcx"))) return 2;
    else if (!strncmp(reg_name, str_lit("%rdx"))) return 3;
    else if (!strncmp(reg_name, str_lit("%rsi"))) return 4;
    else if (!strncmp(reg_name, str_lit("%rdi"))) return 5;
    else if (!strncmp(reg_name, str_lit("%r8"))) return 6;
    else if (!strncmp(reg_name, str_lit("%r9"))) return 7;
    else if (!strncmp(reg_name, str_lit("%r10"))) return 8;
    else if (!strncmp(reg_name, str_lit("%r11"))) return 9;
    else if (!strncmp(reg_name, str_lit("%r12"))) return 10;
    else if (!strncmp(reg_name, str_lit("%r13"))) return 11;
    else if (!strncmp(reg_name, str_lit("%r14"))) return 12;
    else if (!strncmp(reg_name, str_lit("%r15"))) return 13;

    loggerPanic("Invalid register: %s\n", reg_name);
    return -1;
}

void x64SetRegUsed(X64RegState *state, char *reg, IrValue *value) {
    //if (value->type == IR_TYPE_F64) {
    //    for (int i = 0; i < 16; ++i) {
    //        if (!strcmp(state->xmm_names[i], reg)) {
    //            state->used_xmm[i] = 1;
    //            break;
    //        }
    //    }
    //} else {
    //    for (int i = 0; i < 16; ++i) {
    //        if (!strcmp(state->names[i], reg)) {
    //            state->used[i] = 1;
    //            break;
    //        }
    //    }
    //}
    if (!value || !value->name) return;
    mapAdd(state->ir_to_reg, value, reg);
    mapAdd(state->reg_to_ir, reg, value);
}

void x64FreeRegister(X64RegState *state, int reg, IrValue *value) {
    (void)value;
    if (reg >= 0 && reg < 16) {
        state->used[reg] = 0;
    }
}

/* Remove a value from a register */
void x64FreeValue(X64Ctx *ctx, IrValue *value) {
    char *reg_name = x64GetRegForValue(&ctx->reg_state, value);
    if (reg_name) {
        int reg_idx = x64GetRegisterIdx(reg_name);
        ctx->reg_state.used[reg_idx] = 0;
        mapRemove(ctx->reg_state.ir_to_reg, value);
        mapRemove(ctx->reg_state.reg_to_ir, reg_name);
    }
}

void x64FreeRegisterByName(X64Ctx *ctx, char *reg_name) {
    if (!reg_name) return;
    IrValue *maybe_value = mapGet(ctx->reg_state.reg_to_ir, reg_name);
    if (maybe_value) {
        x64FreeValue(ctx, maybe_value);
    } else {
        int reg_idx = x64GetRegisterIdx(reg_name);
        ctx->reg_state.used[reg_idx] = 0;
        mapRemove(ctx->reg_state.reg_to_ir, reg_name);
    }
}


void x64FreeFloatRegister(X64RegState *state, int reg, IrValue *value) {
    (void)value;
    if (reg >= 0 && reg < 16) {
        state->used_xmm[reg] = 0;
    }
}

char *x64GetRegisterName(X64RegState *state, int reg) {
    return state->names[reg];
}

char *x64GetFloatRegisterName(X64RegState *state, int reg) {
    return state->xmm_names[reg];
}

int x64AllocFloatRegister(X64RegState *state, IrValue *value, char **_name) {
    int reg = -1;
    /* @Speed - bitvector */
    for (int i = 0; i < 16; ++i) {
        if (!state->used_xmm[i]) {
            state->used_xmm[i] = 1;
            mapAdd(state->ir_to_reg, value, state->names[i]);
            mapAdd(state->reg_to_ir, state->names[i], value);
            reg = i;
            break;
        }
    }
    if (reg != -1) {
        *_name = x64GetFloatRegisterName(state, reg);
    }
    return reg;
}


static X64Ctx *x64CtxNew(IrProgram *ir_program) {
    X64Ctx *ctx = (X64Ctx *)malloc(sizeof(X64Ctx));
    ctx->ir_program = ir_program;
    ctx->buf = aoStrNew();
    ctx->var_offsets = offsetMap();
    x64RegStateInit(&ctx->reg_state);
    ctx->stack_size = 0;
    ctx->current_label_id = 0;
    return ctx;
}

/*
static int getIntSize(IrValueType type) {
    switch (type) {
        case IR_TYPE_I8:  return 1;
        case IR_TYPE_I16: return 2;
        case IR_TYPE_I32: return 4;
        case IR_TYPE_I64: return 8;
        default: loggerPanic("Invalid integer type: %s\n", irValueTypeToString(type));
    }
}
*/

/* Get the size in bytes for *any* IR scalar/pointer type */
static int getValueSize(IrValue* value) {
     switch (value->type) {
        case IR_TYPE_I8:  return 1;
        case IR_TYPE_I16: return 2;
        case IR_TYPE_I32: return 4;
        case IR_TYPE_I64: return 8;
        case IR_TYPE_F64: return 8;
        case IR_TYPE_PTR: return 8;
        default:
            // loggerWarning("Cannot determine size for IR type: %d\n", value->type);
            return 8;
    }
}

static const char *x64GetMovInstr(IrValue *value) {
    if (value->flags & IR_VALUE_ADDR) return "leaq";
    if (value->type == IR_TYPE_ARRAY) return "leaq";
    int size = getValueSize(value);
    char *mov_op = "movq"; /* Default for 64-bit */
    if (size == 4) mov_op = "movl"; /* Use movl for 32-bit (zeros upper bits of 64-bit reg) */
    else if (size == 2) mov_op = "movzwq"; /* Zero-extend 16-bit to 64-bit */
    else if (size == 1) mov_op = "movzbq"; /* Zero-extend 8-bit to 64-bit */

    /* Use movq if mov_op is movl because movl target must be 32bit reg like %eax */
    if (!strcmp(mov_op, "movl")) mov_op = "movq"; // Stick to 64-bit moves for simplicity here
    return mov_op;
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
    mapRelease(ctx->var_offsets);
    free(ctx);
}

static void x64EmitGlobaUninitlisedInt(X64Ctx *ctx, AoStr *label, IrValue *value) {
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
    aoStrCatFmt(ctx->buf,".quad %X # %f\n", ieee, value->f64);
}

static void x64EmitString(X64Ctx *ctx, AoStr *label, IrValue *value) {
    //assert(label);
    aoStrCatFmt(ctx->buf, "%S:\n\t", label);
    aoStrCatFmt(ctx->buf, ".asciz \"%S\"\n", value->str);
}

static void x64ArrayGen(X64Ctx *ctx, AoStr *label, IrValue *value) {
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
    MapIter *iter = mapIterNew(ctx->ir_program->global_variables);
    while (mapIterNext(iter)) {
        IrValue *global = getValue(IrValue *, iter->node);
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
                        aoStrCatFmt(ctx->buf,".globl %S\n\t.comm %S, %i, %u\n\t", global->name, global->name,
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
    mapIterRelease(iter);
}

static void x64StringGen(X64Ctx *ctx) {
    if (ctx->buf->data[ctx->buf->len-1] == '\t') {
        ctx->buf->len--;
    }
    if (ctx->ir_program->strings->size) {
        StrMapIterator *iter = strMapIteratorNew(ctx->ir_program->strings);
        StrMapNode *it = NULL;
        AoStr string_label;
        memset(&string_label,0,sizeof(AoStr));

        while ((it = strMapNext(iter)) != NULL) {
            IrValue *value = (IrValue *)it->value;
            if (value->kind == IR_VALUE_GLOBAL) continue;
            string_label.data = it->key;
            string_label.len = it->key_len;
            x64EmitString(ctx, &string_label, value);
        }
        strMapIteratorRelease(iter);
    }
}

static void x64FloatGen(X64Ctx *ctx) {
    if (ctx->ir_program->floats->size) {
        StrMapIterator *iter = strMapIteratorNew(ctx->ir_program->floats);
        StrMapNode *it = NULL;
        AoStr shell;
        memset(&shell,0,sizeof(AoStr));

        while ((it = strMapNext(iter)) != NULL) {
            IrValue *value = (IrValue *)it->value;
            if (value->kind == IR_VALUE_GLOBAL) continue;
            aoStrCatFmt(ctx->buf, ".%S:\n\t", value->name);
            x64EmitFloat(ctx, value);
        }
        strMapIteratorRelease(iter);
    }
}

void x64GenFloatLoad(X64Ctx *ctx, IrValue *value, char *reg) {
    assert(value);
    switch (value->kind) {
        case IR_VALUE_CONST_FLOAT:
            if (value->name) {
                aoStrCatFmt(ctx->buf, "movsd .%S(%%rip), %s\n\t", value->name, reg);
            } else {
                loggerPanic("Failed to create move instruction for float\n");
            }
            break;

        case IR_VALUE_GLOBAL:
            aoStrCatFmt(ctx->buf, "movsd %S(%%rip), %s\n\t", value->name, reg);
            break;

        case IR_VALUE_PARAM:
        case IR_VALUE_LOCAL:
        case IR_VALUE_TEMP: {
            long offset = (long)mapGet(ctx->var_offsets, value->name);
            if (offset == 0) {
                loggerPanic("Offset 0 for; %s\n", irValueToString(value)->data);

            }
            aoStrCatFmt(ctx->buf, "movsd %I(%%rbp), %s\n\t", offset, reg);
            break;
        }

        default:
            loggerPanic("Unsupported Float load: %s\n", irValueKindToString(value->kind));
    }
}

void x64GenFloatStore(X64Ctx *ctx, IrValue *dest, char *reg) {
    assert(dest);
    switch (dest->kind) {
        case IR_VALUE_GLOBAL:
            aoStrCatFmt(ctx->buf, "movsd %s, %S(%%rip)\n\t", reg, dest->name);
            break;

        case IR_VALUE_TEMP:
        case IR_VALUE_LOCAL:
        case IR_VALUE_PARAM: {
            long offset = (long)mapGet(ctx->var_offsets, dest->name);
            if (!offset) {
                offset = ctx->stack_size;
                ctx->stack_size += 8;
                mapAdd(ctx->var_offsets, dest->name, ptrcast(offset));
            }
            aoStrCatFmt(ctx->buf, "movsd %s, %I(%%rbp) # %S\n\t", reg, offset, dest->name);
            break;
        }

        default:
            loggerPanic("Unsupported store: %s\n", irValueKindToString(dest->kind));
    }
}

typedef struct X64Reg {
    int idx;
    int needs_release;
    char *name;
} X64Reg;

void x64AllocReg(X64Ctx *ctx, X64Reg *reg, IrValue *value) {
    X64RegState *state = &ctx->reg_state;
    char *current_reg = x64GetRegForValue(state, value);

    if (current_reg) {
        reg->idx = x64GetRegisterIdx(current_reg);
        reg->needs_release = 0;
        reg->name = current_reg;
        return;
    }

    int reg_idx = -1;
    char *reg_name = NULL;

    /* @Speed - bitvector */
    for (int i = 0; i < 16; ++i) {
        if (!state->used[i]) {
            state->used[i] = 1;
            reg_name = state->names[i];
            mapAdd(state->reg_to_ir, reg_name, value);
            reg_idx = i;
            break;
        }
    }

    if (reg_idx != -1) {
        reg->name = reg_name;
        reg->idx = reg_idx;
        reg->needs_release = 0;
    }
    return;
}

void x64GenLoad(X64Ctx *ctx, X64Reg *reg, const char *mov_instr, IrValue *value, int additional_offset) {
    assert(value);

    if (reg->name == NULL) {
        /* If the value is already in a register lets not bother loading it */
        char *current_reg = x64GetRegForValue(&ctx->reg_state, value);
        if (current_reg) {
            reg->idx = x64GetRegisterIdx(current_reg);
            reg->needs_release = 0;
            reg->name = current_reg;
            return;
        }

        x64AllocReg(ctx, reg, value);
    }
    
    switch (value->kind) {
        case IR_VALUE_CONST_INT:
            aoStrCatFmt(ctx->buf, "movq $%I, %s\n\t", value->i64, reg->name);
            break;

        case IR_VALUE_CONST_STR:
            aoStrCatFmt(ctx->buf, "leaq %S(%%rip), %s\n\t", value->name, reg->name);
            break;

        case IR_VALUE_GLOBAL:
            aoStrCatFmt(ctx->buf, "movq %S(%%rip), %s\n\t", value->name, reg->name);
            break;

        case IR_VALUE_TEMP:
        case IR_VALUE_LOCAL:
        case IR_VALUE_PARAM: {
            long offset = (long)mapGet(ctx->var_offsets, value->name);
            if (offset == 0) {
                loggerPanic("Offset 0 for; %s\n", irValueToString(value)->data);
            }

            aoStrCatFmt(ctx->buf, "%s %I(%%rbp), %s # Reg MOV Load %S %s\n\t",
                    mov_instr,
                    offset-additional_offset,
                    reg->name,
                    value->name,
                    irValueTypeToString(value->type));
            break;
        }
        default:
            loggerPanic("Unsupported Indirect load: %s %s\n",
                    irValueKindToString(value->kind),
                    irValueToString(value)->data);

    }
}

void x64RegisterLoadIndirect(X64Ctx *ctx, X64Reg *reg, IrInstr *instr, int additional_offset) {
    x64GenLoad(ctx,reg,"leaq",instr->op1, additional_offset);
}

void x64GenStore(X64Ctx *ctx, IrValue *dest, char *reg) {
    assert(dest);
    switch (dest->kind) {
        case IR_VALUE_GLOBAL:
            aoStrCatFmt(ctx->buf, "movq %s, %S(%%rip)\n\t", reg, dest->name);
            break;

        case IR_VALUE_TEMP:
        case IR_VALUE_LOCAL:
        case IR_VALUE_PARAM: {
            long offset = (long)mapGet(ctx->var_offsets, dest->name);
            if (offset == 0) {
                loggerPanic("No offset for: %s\n",
                        irValueToString(dest)->data);
            }
            assert(offset != 0);
            aoStrCatFmt(ctx->buf, "movq %s, %I(%%rbp) # STORE %S\n\t", reg, offset, dest->name);
            break;
        }

        default:
            loggerPanic("Unsupported store: %s\n", irValueKindToString(dest->kind));
    }
    x64SetRegUsed(&ctx->reg_state, reg, dest);
}

void x64GenStoreWithOffset(X64Ctx *ctx, IrValue *dest, char *reg, long offset) {
    assert(dest);
    switch (dest->kind) {
        case IR_VALUE_CONST_INT:
            aoStrCatFmt(ctx->buf, "movq $%I, %I(%%rbp)\n\t", dest->i64, offset);
            break;

        case IR_VALUE_GLOBAL:
            aoStrCatFmt(ctx->buf, "movq %s, %S(%%rip)\n\t", reg, dest->name);
            break;

        case IR_VALUE_TEMP:
        case IR_VALUE_LOCAL:
        case IR_VALUE_PARAM: {
            aoStrCatFmt(ctx->buf, "movq %s, %I(%%rbp) # STORE %S\n\t",
                        reg,
                        offset,
                        dest->name);
            break;
        }

        default:
            loggerPanic("Unsupported store: %s\n", irValueKindToString(dest->kind));
    }
    //x64SetRegUsed(&ctx->reg_state, reg, dest);
}

void x64GenStoreInstr(X64Ctx *ctx, IrInstr *instr, char *reg) {
    IrValue *dest = instr->result;
    assert(dest);
                //x64GenStore(ctx, instr->result, reg.name);
                //x64FreeValue(ctx, instr->result);
    switch (dest->kind) {
        case IR_VALUE_GLOBAL:
            aoStrCatFmt(ctx->buf, "movq %s, %S(%%rip)\n\t", reg, dest->name);
            break;

        case IR_VALUE_TEMP:
        case IR_VALUE_LOCAL:
        case IR_VALUE_PARAM: {
            long offset = (long)mapGet(ctx->var_offsets, dest->name);
            assert(offset != 0);
            aoStrCatFmt(ctx->buf, "movq %s, %I(%%rbp) # STORE %S\n\t", reg, offset, dest->name);
            break;
        }

        default:
            loggerPanic("Unsupported store: %s\n", irValueKindToString(dest->kind));
    }
    x64SetRegUsed(&ctx->reg_state, reg, dest);
}

void x64EmitBinaryOp(X64Ctx *ctx, char *op, char *dest_reg, IrValue *src_value) {
    if (src_value->kind == IR_VALUE_CONST_INT) {
        /* Immediate operand */
        aoStrCatFmt(ctx->buf, "%s $%I, %s\n\t", op, src_value->i64, dest_reg);
    } else {
        char *src_reg = x64GetRegForValue(&ctx->reg_state, src_value);
        if (src_reg) {
            /* Register operand */
            aoStrCatFmt(ctx->buf, "%s %s, %s\n\t", op, src_reg, dest_reg);
        } else {
            /* Memory operand (stack variable) */
            long offset = (long)mapGet(ctx->var_offsets, src_value->name);
             if (offset == 0) { //&& strncmp(src_value->name, aoStrFromLit("argc")) && !aoStrEq(src_value->name, aoStrFromLit("argv"))) {
                 loggerPanic("Offset 0 or not found for binary op source: %s\n", irValueToString(src_value)->data);
             }
            aoStrCatFmt(ctx->buf, "%s %I(%%rbp), %s # BinOp %S\n\t", op, offset, dest_reg, src_value->name);
        }
    }
}

/* Emit a float binary operation (addsd, subsd, mulsd, divsd, comisd) */
void x64EmitFloatBinaryOp(X64Ctx *ctx, char *op, char *dest_reg, IrValue *src_value) {
    if (src_value->kind == IR_VALUE_CONST_FLOAT) {
         /* Load constant from memory via label */
         if (!src_value->name) {
             loggerPanic("Float constant has no label: %f\n", src_value->f64);
         }
         aoStrCatFmt(ctx->buf, "%s .%S(%%rip), %s\n\t", op, src_value->name, dest_reg);
    } else {
        char *src_reg = x64GetRegForValue(&ctx->reg_state, src_value);
        if (src_reg) {
             /* Register operand */
             aoStrCatFmt(ctx->buf, "%s %s, %s\n\t", op, src_reg, dest_reg);
        } else {
            /* Memory operand (stack variable) */
            long offset = (long)mapGet(ctx->var_offsets, src_value->name);
            if (offset == 0) { // && !aoStrEq(src_value->name, aoStrFromLit("argc")) && !aoStrEq(src_value->name, aoStrFromLit("argv"))) {
                loggerPanic("Offset 0 or not found for float binary op source: %s\n", irValueToString(src_value)->data);
            }
            aoStrCatFmt(ctx->buf, "%s %I(%%rbp), %s # FBinOp %S\n\t", op, offset, dest_reg, src_value->name);
        }
    }
}

/* This temorarily uses `RAX` as a scratch register, it is not sophisticated. 
 * means `RAX` is constantly trashed. We check first to see if the value 
 * is already in a register, if it is we return that register and set
 * `did_reassign` to false to know that we do not need to free RAX */
void x64GetOrSetRegisterForValue(X64Ctx *ctx, IrValue *value, X64Reg *reg) {
    if (x64ValueInReg(&ctx->reg_state, value)) {
        reg->name = x64GetRegForValue(&ctx->reg_state, value);
        reg->idx = x64GetRegisterIdx(reg->name);
        return;
    }

    char *reg_name = "%rax";
    const char *mov_instr = x64GetMovInstr(value);
    reg->idx = x64GetRegisterIdx(reg_name);
    reg->name = reg_name;
    x64SetRegUsed(&ctx->reg_state, reg_name, value);
    x64GenLoad(ctx, reg, mov_instr, value, 0);
}

void x64IntMaths(X64Ctx *ctx, char *operation, IrInstr *instr) {
    X64Reg reg = {0};

    x64GetOrSetRegisterForValue(ctx, instr->op1, &reg);
    x64EmitBinaryOp(ctx, operation, reg.name, instr->op2);

    if (instr->result) {
        x64GenStore(ctx, instr->result, reg.name);
        //x64SetRegUsed(&ctx->reg_state, reg.name, instr->result);
    }

    x64FreeRegisterByName(ctx, reg.name);
}

int x64IsCompareAndBranch(IrInstr *instr, IrInstr *next_instr) {
    return instr->opcode == IR_OP_ICMP && next_instr->opcode == IR_OP_BR;
}

void x64ICmp(X64Ctx *ctx, IrInstr *instr, IrInstr *next_instr) {
    char *op = NULL;
    X64Reg reg = {0};

    if (x64IsCompareAndBranch(instr, next_instr)) {
        switch (instr->extra.cmp_kind) {
            case IR_CMP_LT:  op = "jge"; break;
            case IR_CMP_LE:  op = "jg"; break;
            case IR_CMP_EQ:  op = "jne"; break;
            case IR_CMP_NE:  op = "je"; break;
            case IR_CMP_GT:  op = "jle"; break;
            case IR_CMP_GE:  op = "jl"; break;
            case IR_CMP_ULT: op = "jae"; break;
            case IR_CMP_ULE: op = "ja"; break;
            case IR_CMP_UGT: op = "jbe"; break;
            case IR_CMP_UGE: op = "jb"; break;
            default:
                loggerPanic("Unhandled cmp: %s\n", irInstrToString(instr)->data);
        }

        x64GetOrSetRegisterForValue(ctx, instr->op1, &reg);
        x64EmitBinaryOp(ctx, "cmpq", reg.name, instr->op2);

        next_instr->opcode = IR_OP_NOP;
        aoStrCatFmt(ctx->buf, "%s  BB%I\n\t", op, next_instr->fallthrough_block->id);
    } else {
        switch (instr->extra.cmp_kind) {
            case IR_CMP_LT:  op = "setl"; break;
            case IR_CMP_LE:  op = "setle"; break;
            case IR_CMP_EQ:  op = "sete"; break;
            case IR_CMP_NE:  op = "setne"; break;
            case IR_CMP_GT:  op = "setg"; break;
            case IR_CMP_GE:  op = "setge"; break;
            case IR_CMP_ULT: op = "setb"; break;
            case IR_CMP_ULE: op = "setba"; break;
            case IR_CMP_UGT: op = "seta"; break;
            case IR_CMP_UGE: op = "setae"; break;
            default:
                loggerPanic("Unhandled cmp: %s\n", irInstrToString(instr)->data);
        }


        x64GetOrSetRegisterForValue(ctx, instr->op1, &reg);
        x64EmitBinaryOp(ctx, "cmpq", reg.name, instr->op2);

        aoStrCatFmt(ctx->buf, "%s %%al\n\t"
                              "movzbq %%al, %%rax\n\t"
                              "cltq\n\t", op);
        if (instr->result) {
            x64GenStore(ctx, instr->result, reg.name);
            x64SetRegUsed(&ctx->reg_state, reg.name, instr->result);
        }
    }

    x64FreeRegisterByName(ctx, reg.name);
}

void x64IntDivide(X64Ctx *ctx, IrInstr *instr) {
    char *op = NULL;
    int is_modulo = 0;
    X64Reg reg = {0};

    switch (instr->opcode) {
        case IR_OP_IDIV: op = "idivq"; break;
        case IR_OP_IREM: op = "idivq"; is_modulo = 1; break;
        case IR_OP_UDIV: op = "divq"; break;
        case IR_OP_UREM: op = "divq";  is_modulo = 1; break;
        default:
            loggerPanic("Should either be a divison or modulo got: %s\n",
                    irOpcodeToString(instr));
    }

    x64GetOrSetRegisterForValue(ctx, instr->op1, &reg);

    aoStrCatFmt(ctx->buf, "cqto\n\t");
    x64EmitBinaryOp(ctx, op, reg.name, instr->op2);

    if (is_modulo) {
        aoStrCatFmt(ctx->buf, "movq %%rdx, %%rax\n\t");
    }

    if (instr->result) {
        x64GenStore(ctx, instr->result, reg.name);
        x64SetRegUsed(&ctx->reg_state, reg.name, instr->result);
    }

    x64FreeRegisterByName(ctx, reg.name);
}

void x64FloatMaths(X64Ctx *ctx, IrInstr *instr) {
    char *operation = NULL;
    switch (instr->opcode) {
        case IR_OP_FADD: operation = "addsd"; break;
        case IR_OP_FSUB: operation = "subsd"; break;
        case IR_OP_FMUL: operation = "mulsd"; break;
        case IR_OP_FDIV: operation = "divsd"; break;
        default:
            loggerPanic("Invalid float maths; %s\n", irInstrToString(instr)->data);
    }

    char *reg1_name = "%xmm0";
    if (x64ValueInReg(&ctx->reg_state, instr->op1)) {
        char *tmp = x64GetRegForValue(&ctx->reg_state, instr->op1);
        if (strcmp(tmp, reg1_name) != 0) {
            aoStrCatFmt(ctx->buf, "movq %s, %s\n\t", tmp, reg1_name);
            x64SetRegUsed(&ctx->reg_state, reg1_name, instr->op1);
        }
    } else {
        x64GenFloatLoad(ctx, instr->op1, reg1_name);
        x64SetRegUsed(&ctx->reg_state, reg1_name, instr->op1);
    }

    if (instr->op2->kind == IR_VALUE_CONST_FLOAT) {
        aoStrCatFmt(ctx->buf, "%s .%S(%%rip), %s\n\t", operation, instr->op2->name, reg1_name);
    } else {
        long offset = (long)mapGet(ctx->var_offsets, instr->op2->name);
        aoStrCatFmt(ctx->buf, "%s %I(%%rbp), %s\n\t", operation, offset, reg1_name);
    }

    if (instr->result) {
        x64GenFloatStore(ctx, instr->result, reg1_name);
        // x64SetRegUsed(&ctx->reg_state, reg1_name, instr->result);
    }

    x64FreeRegister(&ctx->reg_state, 0, instr->op1);
}

void x64GetElementPointerClass(X64Ctx *ctx, IrInstr *instr, IrInstr *next_instr) {
    assert(instr->op2 != NULL);
    X64Reg reg = {0};
    X64Reg reg2 = {0};
    long const_offset = instr->op2->kind == IR_VALUE_CONST_INT ? instr->op2->i64 : 0;

    /* If we are storing something on a stack allocated class we treat the 
     * class as though it were a series of local variable allocated from the 
     * stack */
    if (irIsStore(next_instr->opcode) && irValuesEq(instr->result, next_instr->result)) {
        if (instr->flags & IR_INSTR_STACK_CLASS) {
            long offset = (long)mapGet(ctx->var_offsets, instr->op1->name);
            /* We could make this more efficient for const ints as it doesn't 
             * need to move to a register before hand */
            const char *mov_instr = x64GetMovInstr(next_instr->op1);
            x64GenLoad(ctx,&reg, mov_instr, next_instr->op1, 0);
            aoStrCatFmt(ctx->buf, "movq %s, %I(%%rbp)\n\t", reg.name, offset-const_offset);
        } else if (instr->flags & IR_INSTR_HEAP_CLASS) {
            const char *mov_instr = x64GetMovInstr(instr->result);
            x64GenLoad(ctx, &reg, mov_instr, instr->op1, 0);

            const char *mov_instr2 = x64GetMovInstr(next_instr->op1);
            x64GenLoad(ctx, &reg2, mov_instr2, next_instr->op1, 0);

            if (const_offset) {
                aoStrCatFmt(ctx->buf, "movq %s, %I(%s)\n\t", reg2.name, const_offset, reg.name);
            } else {
                aoStrCatFmt(ctx->buf, "movq %s, (%s)\n\t", reg2.name, reg.name);
            }
        } else {
            loggerPanic("Class GEP: `%s` not handled\n", irInstrToString(instr)->data);
        }
        next_instr->opcode = IR_OP_NOP;
    } else {
        const char *mov_instr = x64GetMovInstr(instr->result);
        if (instr->flags & IR_INSTR_STACK_CLASS) {
            x64GenLoad(ctx, &reg, mov_instr, instr->op1, const_offset);
            x64GenStore(ctx, instr->result, reg.name);
        } else if (instr->flags & IR_INSTR_HEAP_CLASS) {
            x64GenLoad(ctx, &reg, mov_instr, instr->op1, 0);
            if (const_offset) {
                aoStrCatFmt(ctx->buf, "movq %I(%s), %s\n\t", const_offset, reg.name, reg.name);
            } else {
                aoStrCatFmt(ctx->buf, "movq (%s), %s\n\t", reg.name, reg.name);
            }
            x64GenStore(ctx, instr->result, reg.name);
        } else {
            loggerPanic("Class GEP: `%s` not handled\n", irInstrToString(instr)->data);
        }
    }

    x64FreeRegisterByName(ctx, reg.name);
    x64FreeRegisterByName(ctx, reg2.name);
}

void x64GetElementPointer(X64Ctx *ctx, IrInstr *instr, IrInstr *next_instr) {
    if (instr->flags & (IR_INSTR_STACK_CLASS|IR_INSTR_HEAP_CLASS)) {
        x64GetElementPointerClass(ctx, instr, next_instr);
        return;
    }

    X64Reg reg = {0};
    X64Reg reg2 = {0};
    int const_offset = instr->op2 && instr->op2->kind == IR_VALUE_CONST_INT ? instr->op2->i64 : 0;
    /* we need to LEA, then assign to that piece of memory if the next_instr is
     * a store in one crack */
    if (irIsStore(next_instr->opcode) && irValuesEq(instr->result, next_instr->result)) {
        if (instr->op2 != NULL) {
            x64RegisterLoadIndirect(ctx, &reg, instr, const_offset);


            const char *mov_instr = x64GetMovInstr(next_instr->op1);
            x64GenLoad(ctx, &reg2, mov_instr, next_instr->op1, 0);
            aoStrCatFmt(ctx->buf, "movq %s, (%s)\n\t", reg2.name, reg.name);
        } else {
            const char *mov_instr = x64GetMovInstr(instr->op1);
            x64GenLoad(ctx, &reg, mov_instr, instr->op1, 0);

            const char *mov_instr2 = x64GetMovInstr(next_instr->op1);
            x64GenLoad(ctx, &reg2, mov_instr2, next_instr->op1, 0);

            aoStrCatFmt(ctx->buf, "movq %s, (%s) # :(\n\t", reg2.name, reg.name);
        }
        next_instr->opcode = IR_OP_NOP;
        x64FreeRegisterByName(ctx, reg.name);
        x64FreeRegisterByName(ctx, reg2.name);
    } else {
        /* load the thing into a register/stack slot  */
        if (instr->op2 != NULL) {
            x64RegisterLoadIndirect(ctx, &reg, instr, const_offset);
            aoStrCatFmt(ctx->buf, "movq (%s), %s\n\t", reg.name, reg.name);
            x64GenStoreInstr(ctx, instr, reg.name);
            x64FreeRegisterByName(ctx, reg.name);
        } else {
            const char *mov_instr = x64GetMovInstr(instr->op1);
            x64GenLoad(ctx, &reg, mov_instr, instr->op1, 0);
            x64FreeRegisterByName(ctx, reg.name);
        }
    }
}

void x64GenerateInstruction(X64Ctx *ctx, IrInstr *instr, IrInstr *next_instr) {
    switch (instr->opcode) {
        case IR_OP_ALLOCA:
            /* We have already allocated in on crack */
            break;

        case IR_OP_LOAD: {
            if (instr->op1->type == IR_TYPE_F64) {
                char *reg_name = NULL;
                int reg = x64AllocFloatRegister(&ctx->reg_state, instr->result, &reg_name);
                x64GenFloatLoad(ctx, instr->op1, reg_name);
                x64GenFloatStore(ctx, instr->result, reg_name);
                x64FreeFloatRegister(&ctx->reg_state, reg, instr->result);
            } else {
                X64Reg reg = {0};
                aoStrCatFmt(ctx->buf,"# loading start %s\n\t", irInstrToString(instr)->data);

                const char *mov_instr = x64GetMovInstr(instr->op1);
                x64GenLoad(ctx, &reg, mov_instr, instr->op1, 0);
                /* If this is a dereference */
                if (irIsPtr(instr->op1->type) && !irIsPtr(instr->result->type)) {
                    aoStrCatFmt(ctx->buf, "movq (%s), %s # Dereference \n\t", reg.name, reg.name);
                }

                x64GenStoreInstr(ctx, instr, reg.name);
                x64FreeRegisterByName(ctx, reg.name);
                aoStrCatFmt(ctx->buf,"# loading end\n\t");
            }
            break;
        }

        case IR_OP_GEP: {
            x64GetElementPointer(ctx, instr, next_instr);
            break;
        }

        case IR_OP_STORE: {
            /* result is dest, op1 is the value */
            if (instr->op1->type == IR_TYPE_F64) {
                char *reg_name = NULL;
                int reg = x64AllocFloatRegister(&ctx->reg_state, instr->result, &reg_name);
                x64GenFloatLoad(ctx, instr->op1, reg_name);
                x64GenFloatStore(ctx, instr->result, reg_name);
                x64FreeFloatRegister(&ctx->reg_state, reg, instr->result);
            } else {
                X64Reg reg = {0};
                X64Reg reg2 = {0};

                /**
                 * WANT:
                 * movq $300, %rbx
                 * leaq -32(%rbp), %rax
                 * movq %rbx, (%rax) # STORE %t7
                 *
                 * HAVE:
                 * movq $300, %rbx
                 * movq %rbx, -64(%rbp) # STORE %t7
                 */

                aoStrCatFmt(ctx->buf,"# storing start %s\n\t", irInstrToString(instr)->data);

                switch (instr->result->type) {
                    case IR_TYPE_PTR: {
                        const char *mov_instr = x64GetMovInstr(instr->op1);
                        x64GenLoad(ctx, &reg, mov_instr, instr->op1, 0);
                        aoStrCatFmt(ctx->buf, "# %S\n\t", irInstrToString(instr));
                        const char *mov_instr2 = x64GetMovInstr(instr->result);
                        x64GenLoad(ctx, &reg2, mov_instr2, instr->result, 0);
                        x64GenStore(ctx, instr->result, reg.name);
                        aoStrCatFmt(ctx->buf, "movq (%s), %s # dref? \n\t",
                                    reg.name,
                                    reg2.name);
                        x64FreeRegisterByName(ctx, reg.name);
                        x64FreeRegisterByName(ctx, reg2.name);
                        break;
                    }

                    case IR_TYPE_ARRAY: {
                        long offset = (long)mapGet(ctx->var_offsets, instr->result->name);
                        for (int i = 0; i < instr->op1->array_.values->size; ++i) {
                            IrValue *ir_value = vecGet(IrValue *, instr->op1->array_.values, i);
                            if (ir_value->kind == IR_VALUE_CONST_INT) {
                                x64GenStoreWithOffset(ctx, ir_value, "unused", offset);
                            } else {
                                const char *mov_instr = x64GetMovInstr(ir_value);
                                x64GenLoad(ctx, &reg2, mov_instr, ir_value, 0);
                                x64GenStoreWithOffset(ctx, ir_value, reg2.name, offset);
                            }
                            offset += getValueSize(ir_value);
                        }
                        break;
                    }

                    default: {
                        const char *mov_instr = x64GetMovInstr(instr->op1);
                        x64GenLoad(ctx, &reg, mov_instr, instr->op1, 0);
                        x64GenStoreInstr(ctx, instr, reg.name);
                        x64FreeValue(ctx, instr->result);
                        break;
                    }
                }

                aoStrCatFmt(ctx->buf,"# storing end\n\t");
            }

            break;
        }

        case IR_OP_CALL: {
            PtrVec *fn_args = instr->op1->array_.values;
            if (fn_args && fn_args->size > 0) {
                int int_reg_idx = 0;
                int float_reg_idx = 0;
                int stack_offset = 0;
                int has_float = 0;

                /* @Bug - this is incorrect as we never increment/decrement the 
                 * stack */
                for (int i = fn_args->size-1; i >= 0; --i) {
                    IrValue *value = vecGet(IrValue *, fn_args, i);

                    if (i >= 6) {
                        if (value->type == IR_TYPE_F64) {
                            has_float = 1;
                            char *arg_name = NULL;
                            int arg_reg = x64AllocFloatRegister(&ctx->reg_state, value, &arg_name);

                            x64GenFloatLoad(ctx, value, arg_name);
                            aoStrCatFmt(ctx->buf, "movsd %s, -%I(%%rsp)\n\t",
                                    arg_name, stack_offset);
                            x64FreeFloatRegister(&ctx->reg_state, arg_reg, value);
                        } else {
                            X64Reg reg = {0};
                            const char *mov_instr = x64GetMovInstr(value);
                            x64GenLoad(ctx, &reg, mov_instr, value, 0);
                            x64FreeRegisterByName(ctx, reg.name);
                            aoStrCatFmt(ctx->buf, "movq %s, -%I(%%rsp)\n\t",
                                    reg.name, stack_offset);
                        }
                    }
                }

                if (stack_offset > 0) {
                    aoStrCatFmt(ctx->buf, "subq $%I, %%rsp\n\t", stack_offset);
                }

                for (int i = 0; i < fn_args->size && i < 6; ++i) {
                    IrValue *value = vecGet(IrValue *, fn_args, i);

                    if (value->type == IR_TYPE_F64) {
                        has_float = 1;
                        char *arg_name = x64_float_param_regs[float_reg_idx++];
                        if (x64ValueInReg(&ctx->reg_state, value)) {
                            char *reg_name = x64GetRegForValue(&ctx->reg_state, value);
                            if (strcmp(arg_name, arg_name) != 0) {
                                aoStrCatFmt(ctx->buf, "movq %s, %s # yeeto\n\t", reg_name, arg_name);
                            }
                        } else {
                            x64GenFloatLoad(ctx, value, arg_name);
                        }
                    } else {
                        char *arg_name = x64_int_param_regs[int_reg_idx++];
                        if (x64ValueInReg(&ctx->reg_state, value)) {
                            char *reg_name = x64GetRegForValue(&ctx->reg_state, value);
                            if (strcmp(reg_name, arg_name) != 0) {
                                aoStrCatFmt(ctx->buf, "movq %s, %s\n\t", reg_name, arg_name);
                            }
                        } else {
                            X64Reg reg;
                            reg.name = arg_name;
                            const char *mov_instr = x64GetMovInstr(value);
                            x64GenLoad(ctx, &reg, mov_instr, value, 0);
                        }
                    }
                }

                if (has_float) {
                    aoStrCatFmt(ctx->buf, "mov  $1, %%al\n\t");
                }

                if (instr->op1->name->data[0] != '%') {
                    AoStr *fname = codeGenNormaliseFunctionName(instr->op1->name);
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
                        x64GenFloatStore(ctx, instr->result, "%xmm0");
                    } else if (instr->result->type != IR_TYPE_VOID) {
                        x64GenStore(ctx,instr->result, "%rax");
                    }
                }

                if (has_float) {
                    aoStrCatFmt(ctx->buf, "xorl %%eax, %%eax\n\t");
                }
                /* We could be more tacitical about this, however we simplify
                 * things by assuming that all registers get trashed (which is 
                 * not the case). We should push the registers being used before
                 * the call and pop them afterwards. */
                x64RegStateClearMaps(&ctx->reg_state);
            }
            break;
        }

        case IR_OP_RET: {
            if (instr->result) {
                if (instr->result->type == IR_TYPE_VOID) {
                    return;
                } else {
                    if (instr->result->type == IR_TYPE_F64) {
                        long offset = (long)mapGet(ctx->var_offsets, instr->result->name);
                        aoStrCatFmt(ctx->buf,
                                    "movq %I(%%rbp), %%xmm0 # %S\n\t", offset,
                                    instr->result->name);
                    } else {
                        if (x64ValueInReg(&ctx->reg_state, instr->result)) {
                            char *reg = x64GetRegForValue(&ctx->reg_state, instr->result);
                            if (strncmp(reg, str_lit("%rax")) != 0) {
                                aoStrCatFmt(ctx->buf, "movq %s, %%rax\n\t", reg);
                            }
                        } else {
                            long offset = (long)mapGet(ctx->var_offsets, instr->result->name);
                            aoStrCatFmt(ctx->buf, "movq %I(%%rbp), %%rax # %S\n\t", offset,
                                    instr->result->name);
                        }
                    }
                }
            }

            break;
        }

        case IR_OP_LOOP:
        case IR_OP_JMP:
            aoStrCatFmt(ctx->buf, "jmp BB%I\n\t", instr->target_block->id);
            break;

        case IR_OP_IADD: { x64IntMaths(ctx, "addq", instr); break; }
        case IR_OP_ISUB: { x64IntMaths(ctx, "subq", instr); break; }
        case IR_OP_IMUL: { x64IntMaths(ctx, "imulq", instr); break; }

        case IR_OP_IDIV:
        case IR_OP_UDIV:
        case IR_OP_IREM:
        case IR_OP_UREM: {
            x64IntDivide(ctx,  instr);
            break;
        }

        /* @Check */
        case IR_OP_INEG: {
            IrValue *const_int = irConstInt(instr->op1->type, 0);
            const_int->name = aoStrPrintf("__temp");
            X64Reg reg1 = {0};
            X64Reg reg2 = {0};

            x64AllocReg(ctx, &reg1, const_int);
            aoStrCatFmt(ctx->buf, "xorq %s, %s\n\t", reg1.name, reg1.name);

            const char *mov_instr = x64GetMovInstr(instr->op1);
            x64GenLoad(ctx, &reg2, mov_instr, instr->op1, 0);

            aoStrCatFmt(ctx->buf, "subq %s, %s\n\t", reg2.name, reg1.name);

            if (instr->result) {
                x64GenStore(ctx, instr->result, reg1.name);
            }

            x64FreeRegisterByName(ctx, reg1.name);
            x64FreeRegisterByName(ctx, reg2.name);
            break;
        }

        case IR_OP_AND: { x64IntMaths(ctx, "and", instr); break; }
        case IR_OP_OR: { x64IntMaths(ctx, "or", instr); break; }
        case IR_OP_XOR: { x64IntMaths(ctx, "xorq", instr); break; }

        /* @Bug Shifting requires an 8 bit register */
        case IR_OP_SHL: { x64IntMaths(ctx, "shl", instr); break; }
        case IR_OP_SAR:
        case IR_OP_SHR: { x64IntMaths(ctx, "shr", instr); break; }

        case IR_OP_NOT: {
            X64Reg reg = {0};

            const char *mov_instr = x64GetMovInstr(instr->op1);
            x64GenLoad(ctx, &reg, mov_instr, instr->op1, 0);

            aoStrCatFmt(ctx->buf, "not %s\n\t", reg.name);
            if (instr->result) {
                x64GenStore(ctx, instr->result, reg.name);
            }

            x64FreeRegisterByName(ctx, reg.name);
            break;
        }

        case IR_OP_ICMP: {
            x64ICmp(ctx, instr, next_instr);
            break;
        }

        case IR_OP_BR: {
            loggerPanic("IR_OP_BR had no condition? %s\n", irInstrToString(instr)->data);
            break;
        }

        case IR_OP_FADD:
        case IR_OP_FSUB:
        case IR_OP_FMUL:
        case IR_OP_FDIV: {
            x64FloatMaths(ctx, instr);
            break;
        }

        case IR_OP_LABEL:

        case IR_OP_FNEG:
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
        case IR_OP_SWITCH:
        case IR_OP_SELECT:
        /**/
        case IR_OP_VA_ARG:
        case IR_OP_VA_START:
        case IR_OP_VA_END:
        case IR_OP_PHI:

        default:
            loggerPanic("Unhandled op: %s for instruction:\n`%s`\n",
                    irOpcodeToString(instr),
                    irInstrToString(instr)->data);
             break;
    }
}

static int x64ReserveStackSlot(X64Ctx *ctx, IrValue *local, int offset) {
    if (mapGet(ctx->var_offsets, local->name) != NULL) {
        return offset;
    }

    int size = 8;
    if      (local->type == IR_TYPE_I8) size = 1;
    else if (local->type == IR_TYPE_I16) size = 2;
    else if (local->type == IR_TYPE_I32) size = 4;
    else if (local->type == IR_TYPE_I64) size = 8;
    else if (local->type == IR_TYPE_ARRAY) {
        IrValue *ir_array = mapGet(ctx->ir_program->arrays, local->name);
        if (ir_array) {
            assert(ir_array);
            debug("ok\n");
            /* Assumes a 64 bit element */
            // printf("%s\n", irValueToString(local)->data);
            size = 8 * ir_array->array_.values->size;
        }
    }

    size = alignTo(size, 8);

    assert(local->name);
    mapAdd(ctx->var_offsets, local->name, ptrcast(offset));
    offset -= size;
    return offset;
}

static int x64ReserveAlloca(X64Ctx *ctx, IrInstr *instr, int offset) {
    if (mapGet(ctx->var_offsets, instr->result->name) != NULL) {
        return offset;
    }
    int size = instr->op1->i64;
    if (size == 0) return offset;

    size = alignTo(size, 8);

    assert(instr->result->name);
    mapAdd(ctx->var_offsets, instr->result->name, ptrcast(offset));
    offset -= size;
    return offset;
}

static void x64CalculateFunctionStack(X64Ctx *ctx, IrFunction *func) {
    mapClear(ctx->var_offsets);
    ctx->stack_size = 0;
    int offset = -8;

    for (int i = 0; i < func->params->size; ++i) {
        IrValue *param = vecGet(IrValue *, func->params, i);
        offset = x64ReserveStackSlot(ctx, param, offset);
    }

    IrInstrIter it;
    for (irInstrIterNextInit(&it, func); irInstrIterNext(&it); ) {
        IrInstr *instr = it.instr;
        if (instr->opcode == IR_OP_ALLOCA) {
            offset = x64ReserveAlloca(ctx, instr, offset);
        } else if (instr->result != NULL) {
            if (instr->result->kind == IR_VALUE_TEMP) {
                debug("%s\n",irInstrToString(instr)->data);
                offset = x64ReserveStackSlot(ctx, instr->result, offset);
            }
        }
    }

    /* Stack needs to be aligned to 16bytes*/
    ctx->stack_size = (-offset + 15) & ~15;
}

static void x64EmitFunction(X64Ctx *ctx, IrFunction *func) {
    x64CalculateFunctionStack(ctx,func);
    loggerWarning("Function: %s\n", func->name->data);

    AoStr *fname = codeGenNormaliseFunctionName(func->name);

    aoStrCatFmt(ctx->buf, ".text\n"
                          ".globl %S\n%S:\n\t"
                          "push %%rbp\n\t"
                          "movq %%rsp, %%rbp\n\t",
                          fname,fname);

    if (ctx->stack_size > 0) {
        aoStrCatFmt(ctx->buf, "subq $%i, %%rsp\n\t", ctx->stack_size);
    }

    /* This is not optimal but it works, which for the moment is good enough */
    for (int i = 0; i < func->params->size && i < 6; ++i) {
        IrValue *param = vecGet(IrValue *, func->params, i);
        char *reg = NULL;

        if (param->type == IR_TYPE_F64) {
            reg = x64_float_param_regs[i];
            x64GenFloatStore(ctx, param, reg);
        } else {
            /* @Mov
             * Get the right `mov` to sign extend etc.. into a 64 bit 
             * stack slot */
            reg = x64_int_param_regs[i];
            x64GenStore(ctx, param, reg);
        }
        x64FreeRegisterByName(ctx, reg);
       // x64SetRegUsed(&ctx->reg_state, reg, param);
    }

    listForEach(func->blocks) {
        IrBlock *block = listValue(IrBlock *, it);

        if (block != func->entry_block) {
            if (ctx->buf->data[ctx->buf->len - 1] == '\t') {
                ctx->buf->len--;
            }
            aoStrCatFmt(ctx->buf, "BB%I:\n\t", block->id);
        }

        listForEach(block->instructions) {
            IrInstr *instr = listValue(IrInstr *, it);
            IrInstr *next_instr = NULL;
            if (it->next != block->instructions) {
                next_instr = listValue(IrInstr *, it->next);
            }
            /* While generating assembly we may have also nuked 
             * some instructions */
            if (instr->opcode == IR_OP_NOP) continue;
            x64GenerateInstruction(ctx, instr, next_instr);
        }

        //x64RegStateClearMaps(&ctx->reg_state);
    }

    aoStrCatFmt(ctx->buf, "leave\n\tret\n\n"); 

}

static void x64FunctionsGen(X64Ctx *ctx) {
    PtrVec *funcs = ctx->ir_program->functions;
    for (int i = 0; i < funcs->size; ++i) {
        IrFunction *func = vecGet(IrFunction *, funcs, i);
        x64RegStateClear(&ctx->reg_state);
        x64EmitFunction(ctx, func);
    }
}

AoStr *x64CodeGen(IrProgram *ir_program) {
    X64Ctx *ctx = x64CtxNew(ir_program);
    //MapIter it;
    //mapIterInit(ir_program->arrays, &it);

    x64GlobalGen(ctx);
    x64StringGen(ctx);
    x64FloatGen(ctx);
    x64FunctionsGen(ctx);


    AoStr *x64_asm = ctx->buf;
    aoStrCatFmt(x64_asm,".LFE0:\n\t");
#if IS_LINUX
    aoStrCatFmt(x64_asm, ".section    .note.GNU-stack,\"\",@progbits\n\t");
#endif
    aoStrCatFmt(x64_asm,".ident      \"hcc: %s %s %s\"\n",
            OS_STR, ARCH_STR, cctrlGetVersion());

    x64CtxRelease(ctx);
    return x64_asm;
}
