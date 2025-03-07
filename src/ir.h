#ifndef IR_H__
#define IR_H__

#include "aostr.h"
#include "cctrl.h"
#include "map.h"

#define IR_CTX_FLAG_IN_LOOP (1<<0)

typedef enum IrOpcode {
    /* Memory operations */
    IR_OP_ALLOCA,      /* Allocate stack memory */
    IR_OP_LOAD,        /* Load from memory */
    IR_OP_STORE,       /* Store to memory */
    IR_OP_GEP,         /* Get element pointer (array/struct indexing) */

    /* Integer Arithmetic operations */
    IR_OP_IADD,         /* Addition */
    IR_OP_ISUB,         /* Subtraction */
    IR_OP_IMUL,         /* Multiplication */
    IR_OP_IDIV,         /* Division */
    IR_OP_UDIV,         /* Unsigned Division */
    IR_OP_IREM,         /* Remainder (modulo) */
    IR_OP_UREM,         /* Unsigned Remainder (modulo) */
    IR_OP_INEG,         /* Negation */

    /* Floading point arithmetic */
    IR_OP_FADD,         /* Float Addition */
    IR_OP_FSUB,         /* Float Subtraction */
    IR_OP_FMUL,         /* Float Multiplication */
    IR_OP_FDIV,         /* Float Division */
    IR_OP_FNEG,         /* Float Negation */

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
    IR_OP_LOOP,        /* Unconditional jump, but identified as a loop for ease */
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
typedef struct IrProgram IrProgram;
typedef union IrUnresolvedBlock IrUnresolvedBlock;

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

        struct {
            aoStr *str;
            int str_real_len;
        };
    };
    int version;
} IrValue;

/* The value and which block it came from, easier to keep track of than
 * 2 vectors */
typedef struct IrPhiPair {
    IrValue *ir_value;
    IrBlock *ir_block;
} IrPhiPair;

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

      struct {
          /* Function call arguments */
          PtrVec *fn_args;
      };

      struct {
          PtrVec *pairs; /* PtrVec<IrPhiPair *> */
      } phi;

    } extra;
} InInstr;

typedef struct IrFunction {
    IrValue *return_value; /* Not sure if this is needed but for printing as a 
                           * string it looks pretty! */
    aoStr *name;          /* Function name */
    IrProgram *program;   /* A pointer to the program */
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

/* This seems a little daft having this but goto's wreck havoc with control 
 * flow. We need to find when we have seen a label and when we have seen a 
 * goto that goes to a label. This needs to be resolved after we have made the 
 * rest of the blocks and thus resolved all labels. */
typedef union IrUnresolvedBlock {
    struct {
        IrValue *ir_value;
        IrBlock *ir_block;
    } goto_;

    struct {
        IrValue *ir_value;
        IrBlock *ir_block;
    } label_;
} IrUnresolvedBlock;

typedef struct IrCtx {
    unsigned long flags;      /* Flags for parsing */
    IrFunction *func;         /* The current function being lowered to IR */
    IrBlock *current_block;   /* The current block being parsed */
    IrBlock *exit_block;       /* Block that will be the return statement */

    /* These allow us to more easily do break/continues */
    IrBlock *loop_head_block; /* Head block for a while/do_while/for, 
                               * allows us to make continue blocks */

    IrBlock *cond_end_block;  /* This will either be the block after an 
                               * `if/else/else_if` */

    IrBlock *loop_end_block;  /* This will either be the block after a
                               * `while/do_while/for` */

    IrBlock *switch_end_block; /* Block after a switch statement */

    PtrVec *unresolved_gotos;  /* PtrVec<IrUnresolvedBlock *>
                                * When we see a goto we need to save it till we 
                                * have finished off the function. */

    StrMap *unresolved_labels;  /* StrMap<IrUnresolvedBlock *>
                                 * When we see a label we want to add it to the 
                                 * hashtable for later resolution. We will 
                                 * iterate over the gotos and index this 
                                 * hashtable */
} IrCtx;

void irArenaInit(unsigned int capacity);
void irMemoryRelease(void);
void irMemoryStats(void);
void irLowerAst(Cctrl *cc);

#endif
