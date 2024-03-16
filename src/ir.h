#ifndef IR_H__
#define IR_H__

#include "aostr.h"
#include "dict.h"
#include "list.h"

#define IR_LOAD        200
#define IR_LOAD_GLOBAL 201
#define IR_SAVE        202

#define IR_ADD         203
#define IR_SUB         204
#define IR_MUL         205
#define IR_DIV         206
#define IR_MOD         207
#define IR_SHL         208
#define IR_SHR         209
#define IR_AND         210
#define IR_OR          211
#define IR_XOR         212
#define IR_NOT         213
#define IR_NEG         214
#define IR_CMP         215

#define IR_CMP_JMP     216

#define IR_EQU_EQU  250
#define IR_NOT_EQU  251
#define IR_LT       252
#define IR_LTE      253
#define IR_GT       254
#define IR_GTE      255

#define IR_JMP          256 // jmp

// signed, the instruction will have the unsigned flag set
#define IR_JMP_LT       257 // jl
#define IR_JMP_LTE      258 // jle
#define IR_JMP_GT       259 // jg
#define IR_JMP_GTE      260 // jge
#define IR_JMP_EQ       261 // je
#define IR_JMP_NOT_EQ   262 // jne
#define IR_JMP_NOT_ZERO 263 // jnz
#define IR_JMP_ZERO     264 // jz

#define IR_FUNCTION     300
#define IR_FLOAT        301
#define IR_INT          302
#define IR_STRING       303
#define IR_ARRAY        304
#define IR_VOID         305
#define IR_CHAR         306
#define IR_POINTER      307 
#define IR_CLASS        308
#define IR_LOCAL        309
#define IR_LABEL        310

#define IR_FLOAT_TO_INT 350
#define IR_INT_TO_FLOAT 351

#define IR_FLAGS_UNSIGNED     (1 << 0)
#define IR_FLAGS_INT          (1 << 1)
#define IR_FLAGS_FLOAT        (1 << 2)
#define IR_FLAGS_INTERMEDIATE (1 << 3)
#define IR_FLAGS_VAR_ARGS     (1 << 4)
#define IR_FLAGS_FUN_EXTERN   (1 << 5)
#define IR_FLAGS_FUN_EXISTS   (1 << 6)


typedef struct IrOperand {
    int kind;
    int offset;
    int size;
    int reg;
    unsigned long flags;

    union {
        long i64;

        /* float */
        struct {
            double f64;
            aoStr *f64_label;
        };

        /* string */
        struct {
            aoStr *sval;
            aoStr *slabel;
        };

        /* global var */
        struct {
            aoStr *gname;
        };

        /* function */
        struct {
            char *fname;
        };

        /* label */
        struct {
            aoStr *label;
        };
    };
} IrOperand;

typedef struct IrInstruction {
    int op;
    unsigned long flags;
    union {
        /* function */
        struct {
            aoStr *fname;
            List *body;
            Dict *var_to_reg;
            int reg_count;
        };

        /* function call */
        struct {
            aoStr *call_name;
            List *args;
            int has_var_args;
        };

        /* classic */
        struct {
            IrOperand *arg1;
            IrOperand *arg2;
            IrOperand *arg3;
        };
    };
} IrInstruction;

void irPrint(List *ir_instructions);

#endif
