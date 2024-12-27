#ifndef IR_H
#define IR_H

#include "cctrl.h"

#define IR_FLAG_FUNC_ARG (1<<0)
#define IR_FLAG_GLOBAL   (1<<1)

typedef enum {
    /* loads */
    IR_LOAD = 1,
    IR_LOAD_IMM,
    IR_LOAD_IMM_F64,
    IR_LOAD_IMM_STR,
    IR_LOAD_FN,
    IR_LOAD_F64,
    IR_LOAD_GLOBAL,
    IR_LOAD_ARRAY,
    /* Saves */
    IR_SAVE,
    IR_SAVE_IMM,
    IR_SAVE_IMM_F64,
    IR_SAVE_IMM_STR,
    IR_SAVE_FN,
    IR_SAVE_F64,
    IR_SAVE_GLOBAL,
    IR_SAVE_ARRAY,

    IR_JMP,
    IR_LABEL,
    IR_LT,
    IR_LTE,
    IR_GT,
    IR_GTE,
    IR_EQ,
    IR_NOT_EQ,
    IR_ADD,
    IR_SUB,
    IR_MUL,
    IR_DIV,
    IR_MOD,
    IR_SHL,
    IR_SHR,
    IR_RET,
    IR_CALL,
    IR_PUSH,
    IR_POP,
    IR_JMP_LT,
    IR_JMP_LTE,
    IR_JMP_GT,
    IR_JMP_GTE,
    IR_JMP_EQ,
    IR_JMP_NOT_EQ,
    IR_LOGICAL_OR,
    IR_LOGICAL_AND,
    IR_AND,
    IR_OR,
    IR_NOT,
    IR_XOR,
    IR_NEG,
    IR_POS,
    IR_PRE_PLUS_PLUS,
    IR_PRE_MINUS_MINUS,
    IR_PLUS_PLUS,
    IR_MINUS_MINUS,
    IR_ADDR,
    IR_DEREF,
    IR_CAST,
} IrOp;

typedef struct IrInstruction {
    IrOp op;
    unsigned long flags;
    int dest;
    int s1;
    int s2;
    int size;
    double f64;
    aoStr *label;
    aoStr *fname;
    int offset;
} IrInstruction;

typedef struct IrModule {
    PtrVec *instructions;
    PtrVec *vars;
} IrModule;

typedef struct IrCtx {
    Cctrl *cc;
    unsigned long flags;
    IntMap *var_mapping;
    int next_var_id;
    int next_temp_reg;
    IrModule *ir_module;
    int stack_size;
} IrCtx;

void irFromAst(Cctrl *cc);
IrCtx *irCtxNew(Cctrl *cc);
aoStr *irInstructionToString(IrInstruction *inst);

#endif
