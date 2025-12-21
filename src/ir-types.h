#ifndef IR_TYPES_H__
#define IR_TYPES_H__

#include "aostr.h"
#include "cctrl.h"
#include "types.h"
#include "containers.h"
#include "list.h"

typedef struct IrInstr IrInstr;
typedef struct IrBlock IrBlock;
typedef struct IrBlockMapping IrBlockMapping;
typedef struct IrValue IrValue;

typedef enum IrOp {
    IR_NOP,         /* No op */
    /* Memory operations */
    IR_ALLOCA,      /* Allocate stack memory */
    IR_LOAD,        /* Load from memory */
    IR_STORE,       /* Store to memory */
    IR_GEP,         /* Get element pointer (array/struct indexing) */

    /* Integer Arithmetic operations */
    IR_IADD,         /* Addition */
    IR_ISUB,         /* Subtraction */
    IR_IMUL,         /* Multiplication */
    IR_IDIV,         /* Division */
    IR_UDIV,         /* Unsigned Division */
    IR_IREM,         /* Remainder (modulo) */
    IR_UREM,         /* Unsigned Remainder (modulo) */
    IR_INEG,         /* Negation */

    /* Floading point arithmetic */
    IR_FADD,         /* Float Addition */
    IR_FSUB,         /* Float Subtraction */
    IR_FMUL,         /* Float Multiplication */
    IR_FDIV,         /* Float Division */
    IR_FNEG,         /* Float Negation */

    /* Bitwise operations */
    IR_AND,         /* Bitwise AND */
    IR_OR,          /* Bitwise OR */
    IR_XOR,         /* Bitwise XOR */
    IR_SHL,         /* Shift left */
    IR_SHR,         /* Shift right (logical) */
    IR_SAR,         /* Shift right (arithmetic) */
    IR_NOT,         /* Bitwise NOT */
    
    /* Comparison operations */
    IR_ICMP,        /* Integer comparison */
    IR_FCMP,        /* Float comparison */
    
    /* Conversion operations */
    IR_TRUNC,       /* Truncate (larger to smaller int) */
    IR_ZEXT,        /* Zero extend (smaller to larger int) */
    IR_SEXT,        /* Sign extend (smaller to larger int) */
    IR_FPTRUNC,     /* Float truncate (double to float) */
    IR_FPEXT,       /* Float extend (float to double) */
    IR_FPTOUI,      /* Float to unsigned int */
    IR_FPTOSI,      /* Float to signed int */
    IR_UITOFP,      /* Unsigned int to float */
    IR_SITOFP,      /* Signed int to float */
    IR_PTRTOINT,    /* Pointer to integer */
    IR_INTTOPTR,    /* Integer to pointer */
    IR_BITCAST,     /* Reinterpret bits as different type */
    
    /* Control flow operations */
    IR_RET,         /* Return from function */
    IR_BR,          /* Conditional branch */
    IR_JMP,         /* Unconditional jump */
    IR_LOOP,        /* Unconditional jump, but identified as a loop for ease */
    IR_SWITCH,      /* Switch statement */
    IR_CALL,        /* Function call */
    IR_PHI,         /* SSA phi node */
    IR_LABEL,       /* Not used for code gen, temporary while resolving unconditional jumps */
    
    /* Other operations */
    IR_SELECT,      /* Select between two values based on condition */
    IR_VA_ARG,      /* Get next variadic argument */
    IR_VA_START,    /* Initialize va_list */
    IR_VA_END       /* Clean up va_list */
} IrOp;


typedef enum IrValueType {
    IR_TYPE_VOID,        /* Void type */
   // IR_TYPE_I1,        /* Boolean (1 bit) */
    IR_TYPE_I8,          /* 8-bit integer (char) */
    IR_TYPE_I16,         /* 16-bit integer (short) */
    IR_TYPE_I32,         /* 32-bit integer (int) */
    IR_TYPE_I64,         /* 64-bit integer (long) */
    // We do not have 32 bit floats...  yet IR_TYPE_F32,        /* 32-bit float */
    IR_TYPE_F64,         /* 64-bit float (double) */
    IR_TYPE_PTR,         /* Pointer type (with element type) */
    IR_TYPE_ARRAY,       /* Array type (with element type and length) */
    IR_TYPE_ARRAY_INIT,  /* Array initaliser type (with element type and length) */
    IR_TYPE_STRUCT,      /* Structure type (with field types) */
    IR_TYPE_FUNCTION,    /* Function type (with return and param types) */
    IR_TYPE_ASM_FUNCTION,/* Raw assembly function, just a function name and 
                          * a string containing the assembly */
    IR_TYPE_LABEL,       /* Label reference type */
} IrValueType;

typedef enum IrValueKind {
    IR_VAL_CONST_INT,    /* Integer constant */
    IR_VAL_CONST_FLOAT,  /* Floating point constant */
    IR_VAL_CONST_STR,    /* String constant */
    IR_VAL_GLOBAL,       /* Global variable */
    IR_VAL_PARAM,        /* Function parameter */
    IR_VAL_LOCAL,        /* Local variable */
    IR_VAL_TMP,          /* Temporary value (SSA) */
    IR_VAL_PHI,          /* Phi node value (SSA) */
    IR_VAL_LABEL,        /* Block label */
    IR_VAL_UNDEFINED,    /* Undefined value */
    IR_VAL_UNRESOLVED    /* This is for GOTOs and Labels when we are
                            * constructing the IR*/
} IrValueKind;

typedef enum IrCmpKind {
    IR_CMP_INVALID = -1,
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

typedef struct IrValueArray IrValueArray;
typedef struct IrValueGlobal IrValueGlobal;
typedef struct IrValueString IrValueString;

struct IrValueArray {
    /* Either the name of a function call OR the label for
     * the array in initialisers */
    AoStr *label;
    int nesting;
    int length_per_array;
    Vec *values; /* `Vec<IrValue *>` recurisve array definition,
                  * also used for storing function arguments */
};

struct IrValueGlobal {
    AoStr *name;
    IrValue *value;
};

struct IrValueString {
    AoStr *label;
    AoStr *str;
    int str_real_len;
};

typedef struct IrVar {
    /* Variable id */
    u32 id;
    /* How big the variable is */
    u16 size;
} IrVar;

struct IrValue {
    IrValueType type;
    IrValueKind kind;
    u64 flags;

    union {
        AoStr *name; /* aribitrary string that I seem to have used in my 
                      * previous implementation */
        s64 _i64; /* For integer constants */
        f64 _f64; /* Float constants */
        IrVar var;  /* 'name' of the variable, essentially an id */

        IrInstr *phi; /* Notes that the value is a phi node, used for either
                       * a logical `OR` or an `AND` */

        /* A string literal, the `str` is an escaped string suitable for 
         * putting in a file for an assembler to read, the `str_real_len` 
         * is the length of the string without escape sequences. */
        IrValueString str;
        IrValueGlobal global;
        IrValueArray array;
    } as;
};

typedef struct IrBlockPair IrBlockPair;

struct IrBlockPair {
    IrBlock *target_block;      /* For control flow instructions */
    IrBlock *fallthrough_block; /* For conditional flow */
};

struct IrInstr {
    IrOp op;
    IrValue *dst;
    IrValue *r1;
    IrValue *r2;
    u64 flags;

    union {
        IrBlockPair blocks;
        IrCmpKind cmp_kind;
        /* either an unresolved `goto label;` or a a `label:` */
        AoStr *unresolved_label;

        List *cases; /* `List<IrPair *>`
                      * A switch statments cases in the form;
                      * i64 <number>, label <block>, */
        Vec *phi_pairs; /* `Vec<IrPair *>` */
    } extra;
};

struct IrBlock {
    u32 id;
    u8 removed;
    u8 sealed;
    /* List<IrInstr *>*/
    List *instructions;
};

typedef struct IrPair {
    IrValue *ir_value;
    IrBlock *ir_block;
} IrPair;

struct IrBlockMapping {
    u32 id; /* This block */
    Map *successors;   /* `Map<u32, IrBlock *>` */
    Map *predecessors; /* `Map<u32, IrBlock *>` */
};

typedef struct IrFunction {
    /* Name of the function as defined */
    AoStr *name;
    /* Space needed on the stack for allocating variables */
    u16 stack_space;
    /* Does the function have variable arguments? */
    u8 has_var_args;
    /* Not sure if this is needed but for printing as a 
     * string it looks pretty! */
    IrValue *return_value; 
    /* Parameter list, `PtrVec<IrValue *>` */
    Vec *params;
    /* Entry */
    IrBlock *entry_block;
    /* Exit */
    IrBlock *exit_block;  
    /* Maps an ast lvar_id to an IrValue `Map<u32, IrValue>` */
    Map *variables;
    /* `List<IrBlock *>` */
    List *blocks;
    /* `Map<u32, IrBlockMapping *>`*/
    Map *cfg;
} IrFunction;

typedef struct IrProgram {
    Vec *functions;
    Vec *globals;
} IrProgram;

typedef struct IrCtx {
    /* Current function being converted to IR */
    IrFunction *cur_func;
    /* Current ir block we are looking at */
    IrBlock *cur_block;
    /* The entrity of the program in IR */
    IrProgram *prog;
    /* The main compiler struct */
    Cctrl *cc;
} IrCtx;


IrBlockMapping *irFunctionGetBlockMapping(IrFunction *func, IrBlock *ir_block);
Map *irFunctionGetSuccessors(IrFunction *func, IrBlock *ir_block);
Map *irFunctionGetPredecessors(IrFunction *func, IrBlock *ir_block);

u8 irOpIsCmp(IrOp opcode);
u8 irIsFloat(IrValueType ir_value_type);
u8 irIsInt(IrValueType ir_value_type);
u8 irTypeIsScalar(IrValueType ir_value_type);
u8 irIsConst(IrValueKind ir_value_kind);
u8 irIsPtr(IrValueType ir_value_type);
u8 irIsStore(IrOp opcode);
u8 irIsLoad(IrOp opcode);
u8 irIsStruct(IrValueType ir_value_type);
u8 irIsConstInt(IrValue *val);
int irGetIntSize(IrValueType ir_value_type);

IrBlock *irInstrEvalConstBranch(IrInstr *ir_cmp, IrInstr *ir_branch);
u8 irBlocksPointToEachOther(IrFunction *func, IrBlock *prev_block, IrBlock *next_block);
IrValueType irConvertType(AstType *type);

IrBlock *irInstrGetTargetBlock(IrInstr *instr);
IrBlock *irInstrGetFallthroughBlock(IrInstr *instr);
Map *irBlockGetSuccessors(IrFunction *func, IrBlock *block);
int irBlockIsStartOrEnd(IrFunction *func, IrBlock *block);

#endif
