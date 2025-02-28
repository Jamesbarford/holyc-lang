#ifndef IR_H__
#define IR_H__

#include "aostr.h"
#include "map.h"

typedef enum IrOpcode {
    /* Memory operations */
    IR_OP_ALLOCA,      /* Allocate stack memory */
    IR_OP_LOAD,        /* Load from memory */
    IR_OP_STORE,       /* Store to memory */
    IR_OP_GEP,         /* Get element pointer (array/struct indexing) */
    
    /* Arithmetic operations */
    IR_OP_ADD,         /* Addition */
    IR_OP_SUB,         /* Subtraction */
    IR_OP_MUL,         /* Multiplication */
    IR_OP_DIV,         /* Division */
    IR_OP_REM,         /* Remainder (modulo) */
    IR_OP_NEG,         /* Negation */
    
    /* Bitwise operations */
    IR_OP_AND,         /* Bitwise AND */
    IR_OP_OR,          /* Bitwise OR */
    IR_OP_XOR,         /* Bitwise XOR */
    IR_OP_SHL,         /* Shift left */
    IR_OP_SHR,         /* Shift right (logical) */
    IR_OP_SAR,         /* Shift right (arithmetic) */
    IR_OP_NOT,         /* Bitwise NOT */
    
    /* Comparison operations */
    IR_OP_ICMP,        /* Integer comparison */
    IR_OP_FCMP,        /* Float comparison */
    
    /* Conversion operations */
    IR_OP_TRUNC,       /* Truncate (larger to smaller int) */
    IR_OP_ZEXT,        /* Zero extend (smaller to larger int) */
    IR_OP_SEXT,        /* Sign extend (smaller to larger int) */
    IR_OP_FPTRUNC,     /* Float truncate (double to float) */
    IR_OP_FPEXT,       /* Float extend (float to double) */
    IR_OP_FPTOUI,      /* Float to unsigned int */
    IR_OP_FPTOSI,      /* Float to signed int */
    IR_OP_UITOFP,      /* Unsigned int to float */
    IR_OP_SITOFP,      /* Signed int to float */
    IR_OP_PTRTOINT,    /* Pointer to integer */
    IR_OP_INTTOPTR,    /* Integer to pointer */
    IR_OP_BITCAST,     /* Reinterpret bits as different type */
    
    /* Control flow operations */
    IR_OP_RET,         /* Return from function */
    IR_OP_BR,          /* Conditional branch */
    IR_OP_JMP,         /* Unconditional jump */
    IR_OP_SWITCH,      /* Switch statement */
    IR_OP_CALL,        /* Function call */
    IR_OP_PHI,         /* SSA phi node */
    
    /* Other operations */
    IR_OP_SELECT,      /* Select between two values based on condition */
    IR_OP_VA_ARG,      /* Get next variadic argument */
    IR_OP_VA_START,    /* Initialize va_list */
    IR_OP_VA_END       /* Clean up va_list */
} IrOpcode;

typedef enum IrCmpKind {
    IR_CMP_EQ,         /* Equal */
    IR_CMP_NE,         /* Not equal */
    IR_CMP_LT,         /* Less than */
    IR_CMP_LE,         /* Less than or equal */
    IR_CMP_GT,         /* Greater than */
    IR_CMP_GE,         /* Greater than or equal */
    IR_CMP_ULT,        /* Unsigned less than */
    IR_CMP_ULE,        /* Unsigned less than or equal */
    IR_CMP_UGT,        /* Unsigned greater than */
    IR_CMP_UGE,        /* Unsigned greater than or equal */
    IR_CMP_OEQ,        /* Ordered and equal (float) */
    IR_CMP_ONE,        /* Ordered and not equal (float) */
    IR_CMP_OLT,        /* Ordered and less than (float) */
    IR_CMP_OLE,        /* Ordered and less than or equal (float) */
    IR_CMP_OGT,        /* Ordered and greater than (float) */
    IR_CMP_OGE,        /* Ordered and greater than or equal (float) */
    IR_CMP_UNO,        /* Unordered (float, when either operand is NaN) */
    IR_CMP_ORD         /* Ordered (float, when neither operand is NaN) */
} IrCmpKind;

typedef enum IrValueType {
    IR_TYPE_VOID,       /* Void type */
   // IR_TYPE_I1,         /* Boolean (1 bit) */
    IR_TYPE_I8,         /* 8-bit integer (char) */
    IR_TYPE_I16,        /* 16-bit integer (short) */
    IR_TYPE_I32,        /* 32-bit integer (int) */
    IR_TYPE_I64,        /* 64-bit integer (long) */
    // We do not have 32 bit floats...  yet IR_TYPE_F32,        /* 32-bit float */
    IR_TYPE_F64,        /* 64-bit float (double) */
    IR_TYPE_PTR,        /* Pointer type (with element type) */
    IR_TYPE_ARRAY,      /* Array type (with element type and length) */
    IR_TYPE_STRUCT,     /* Structure type (with field types) */
    IR_TYPE_FUNCTION,   /* Function type (with return and param types) */
    IR_TYPE_LABEL       /* Label reference type */
} IrValueType;

typedef enum IrValueKind {
    IR_VALUE_CONST_INT,    /* Integer constant */
    IR_VALUE_CONST_FLOAT,  /* Floating point constant */
    IR_VALUE_CONST_STR,    /* String constant */
    IR_VALUE_GLOBAL,       /* Global variable */
    IR_VALUE_PARAM,        /* Function parameter */
    IR_VALUE_LOCAL,        /* Local variable */
    IR_VALUE_TEMP,         /* Temporary value (SSA) */
    IR_VALUE_PHI,          /* Phi node value (SSA) */
    IR_VALUE_LABEL,        /* Block label */
    IR_VALUE_UNDEFINED     /* Undefined value */
} IrValueKind;

typedef struct IrInstr IrInstr;
typedef struct IrBlock IrBlock;
typedef struct IrFunction IrFunction;

typedef struct IrBlock {
    aoStr *label;         /* Identifier for the block */
    PtrVec *instructions; /* Vector of ir instructions belonging to the block */
    PtrVec *predecessors; /* Blocks that flow into this one 
                           * PtrVec<IrBlock *> */

    PtrVec *successors;   /* Blocks that this can flow into 
                           * PtrVec<IrBlock *>*/
    int sealed;
    StrMap *ssa_values;
} IrBlock;

typedef struct IrValue {
    IrValueType type;
    IrValueKind kind;
    union {
        long i64;
        double f64;
        aoStr *name;
        int reg;
        IrInstr *phi;
    };
    int version;
} IrValue;

typedef struct IrInstr {
    IrOpcode opcode;            /* Operation type */
    IrValue *result;            /* Destination if any */
    IrValue *op1;               /* First operand if any */
    IrValue *op2;               /* Second operand if any */
    IrValue *op3;               /* 3rd operand if any */
    IrBlock *target_block;      /* For control flow instructions */
    IrBlock *fallthrough_block; /* For conditional flow */
    union {
      IrCmpKind cmp_kind;
    } extra;
} InInstr;

typedef struct IrFunction {
    IrValue *return_type; /* Not sure if this is needed but for printing as a 
                           * string it looks pretty! */
    aoStr *name;          /* Function name */
    PtrVec *params;       /* Parameter list, PtrVec<IrValue *> */
    PtrVec *blocks;       /* Basic blocks, PtrVec<IrInstr *> */
    IrBlock *entry_block; /* Entry */
    IrBlock *exit_block;  /* Exit */
    StrMap *variables;    /* The functions local variables StrMap<IrValue *> */
} IrFunction;

typedef struct IrProgram {
    PtrVec *functions;        /* PtrVec<IrFunction *> */
    /* Im not sure we need these as they exist on the Cctrl struct, certainly
     * the strings and types do... These are presumably Ast structs which 
     * is kinds nasty having both an Ast and Ir later in the codegen phase.
     */
    StrMap *global_variables; /* StrMap<IrValue *> */
    StrMap *strings;          /* StrMap<IrValue *> */
    StrMap *types;            /* @TODO: StrMap<?> I feel this one needs
                               * a bit more thought */
} IrProgram;

void irArenaInit(unsigned int capacity);
void irMemoryRelease(void);
void irMemoryStats(void);
void irLowerAst(Cctrl *cc);

#endif
