#ifndef IR_TYPES_H__
#define IR_TYPES_H__

/* The IR is quite sprawling in nature and there are a few things that we 
 * want to do with it. Thus the types have been splitout to avoid the recusive 
 * including of header files. */

#include "aostr.h"
#include "list.h"
#include "map.h"

#define IR_CTX_FLAG_IN_LOOP   (1<<0)
#define IR_CTX_FLAG_IN_SWITCH (1<<1)

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
    IR_OP_LABEL,       /* Not used for code gen, temporary while resolving unconditional jumps */
    
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
    IR_TYPE_ARRAY_INIT, /* Array initaliser type (with element type and length) */
    IR_TYPE_STRUCT,     /* Structure type (with field types) */
    IR_TYPE_FUNCTION,   /* Function type (with return and param types) */
    IR_TYPE_ASM_FUNCTION,/* Raw assembly function, just a function name and 
                          * a string containing the assembly */
    IR_TYPE_LABEL,      /* Label reference type */
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
    IR_VALUE_UNDEFINED,    /* Undefined value */
    IR_VALUE_UNRESOLVED    /* This is for GOTOs and Labels when we are
                            * constructing the IR*/
} IrValueKind;

typedef struct IrInstr IrInstr;
typedef struct IrValue IrValue;
typedef struct IrBlock IrBlock;
typedef struct IrFunction IrFunction;
typedef struct IrProgram IrProgram;
typedef union IrUnresolvedBlock IrUnresolvedBlock;

/* We keep the successors and predecessors on the function in a graph as it 
 * is easier to keep track of the mappings between the blocks. */
typedef struct IrBlock {
    int id;             /* Identifier for the block */
    char sealed;        /* Block is done */
    char removed;       /* Soft delete a block? */ 
    List *instructions; /* List<IrInstr *> ir instructions belonging to the block */
    StrMap *ssa_values;
} IrBlock;

typedef struct IrValue {
    IrValueType type;
    IrValueKind kind;
    /* @Bug
     * Used for any arbitrary string, this is not on the union as globals and
     * globally defined functions need a label/name and would conflict */
    aoStr *name;

    union {
        long i64;   /* For integer constants */
        double f64; /* Float constants */

        int reg; /* As of yet unused */

        IrInstr *phi; /* Notes that the value is a phi node, used for either
                       * a logical `OR` or an `AND` */

        /* A string literal, the `str` is an escaped string suitable for 
         * putting in a file for an assembler to read, the `str_real_len` 
         * is the length of the string without escape sequences. */
        struct {
            aoStr *str;
            int str_real_len;
        };

        struct {
            aoStr *gname;
            IrValue *value;
        } global;

        struct {
            aoStr *label;
            int nesting;
            int length_per_array;
            PtrVec *values; /* `PtrVec<IrValue *>` recurisve array definition */
        } array_;
    };
    int version;
} IrValue;

/* The value and which block it came from, easier to keep track of than
 * 2 vectors */
typedef struct IrPair {
    IrValue *ir_value;
    IrBlock *ir_block;
} IrPair;

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
      /* either an unresolved `goto label;` or a a `label:` */
        aoStr *unresolved_label;

        struct {
          /* Function call arguments PtrVec<IrValue *> */
            PtrVec *fn_args;
        };

        struct {
            List *cases; /* `List<IrPair *>`
                          * A switch statments cases in the form;
                          * i64 <number>, label <block>, */
        };

        struct {
            PtrVec *pairs; /* PtrVec<IrPair *> */
        } phi;
    } extra;
} InInstr;

typedef struct IrBlockMapping {
    int id;
    IntMap *successors;
    IntMap *predecessors;
} IrBlockMapping;

typedef struct IrFunction {
    IrValue *return_value; /* Not sure if this is needed but for printing as a 
                           * string it looks pretty! */
    aoStr *name;          /* Function name */
    IrProgram *program;   /* A pointer to the program */
    PtrVec *params;       /* Parameter list, `PtrVec<IrValue *>` */
    List *blocks;         /* Basic blocks, `List<IrBlock *>` */
    IrBlock *entry_block; /* Entry */
    IrBlock *exit_block;  /* Exit */
    StrMap *variables;    /* The functions local variables StrMap<IrValue *> */
    IntMap *cfg;          /* The interconnectivity between nodes: 
                           * IntMap<IrBlockMapping> [id] => {id, id...} */
    int has_var_args;
} IrFunction;

typedef struct IrProgram {
    PtrVec *functions;        /* `PtrVec<IrFunction *>` */
    /* Im not sure we need these as they exist on the Cctrl struct, certainly
     * the strings and types do... These are presumably Ast structs which 
     * is kinds nasty having both an Ast and Ir later in the codegen phase.
     */
    PtrVec *asm_functions;    /* `PtrVec<IrValue *>` - Raw assembly functions */
    StrMap *global_variables; /* `StrMap<IrValue *>` */
    StrMap *strings;          /* `StrMap<IrValue *>` */
    StrMap *types;            /* @TODO: StrMap<?> I feel this one needs
                               * a bit more thought */
    StrMap *arrays;           /* @TODO decide on representation `StrMap<IrValue *>` */
} IrProgram;

/* This seems a little daft having this but goto's wreck havoc with control 
 * flow. We need to find when we have seen a label and when we have seen a 
 * goto that goes to a label. This needs to be resolved after we have made the 
 * rest of the blocks and thus resolved all labels. */
typedef union IrUnresolvedBlock {
    struct {
        List *list_node;
        IrBlock *ir_block;
    } goto_;

    struct {
        List *list_node;
        IrBlock *ir_block;
    } label_;
} IrUnresolvedBlock;


typedef struct IrArrayCtx {
    PtrVec *init;
    IrValueType type;
    int nesting;
    int length_per_array;
} IrArrayCtx;

typedef struct IrCtx {
    IrProgram *ir_program;    /* The program being created */
    unsigned long flags;      /* Flags for parsing */
    IrFunction *func;         /* The current function being lowered to IR */
    IrBlock *current_block;   /* The current block being parsed */

    /* These allow us to more easily do break/continues */
    IrBlock *loop_head_block; /* Head block for a while/do_while/for, 
                               * allows us to make continue blocks */

    IrBlock *end_block;  /* This will either be the block after a
                               * `while/do_while/for` */

    PtrVec *unresolved_gotos;  /* `PtrVec<IrUnresolvedBlock *>`
                                * When we see a goto we need to save it till we 
                                * have finished off the function. */

    StrMap *unresolved_labels;  /* `StrMap<IrUnresolvedBlock *>`
                                 * When we see a label we want to add it to the 
                                 * hashtable for later resolution. We will 
                                 * iterate over the gotos and index this 
                                 * hashtable */

    /* For initialising an array, we need to recurse through multiple 
     * function calls but actually want to flatten the array and keep track 
     * of it's shape */
    IrArrayCtx array_;
} IrCtx;

#endif
