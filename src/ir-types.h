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
    IR_LOAD_DEREF,  /* dst = *r1 (r1 is a pointer value, not a slot id) */
    IR_STORE_DEREF, /* *dst = r1 (dst is a pointer value) */
    IR_LEA,         /* dst = &r1 (address of a stack slot, as a pointer) */
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
    IR_SWITCH,      /* Switch statement */
    IR_CALL,        /* Function call */
    IR_PHI,         /* SSA phi node */
    IR_LABEL,       /* Not used for code gen, temporary while resolving unconditional jumps */
    
    /* Other operations */
    IR_SELECT,      /* Select between two values based on condition */
    IR_VA_ARG,      /* Get next variadic argument */
    IR_VA_START,    /* Initialize va_list */
    IR_VA_END,      /* Clean up va_list */
    /* Verbatim assembly text from an inline `asm { ... }` block.
     * The per-arch codegen just dumps the bytes into the output;
     * the user is responsible for asm validity. The raw text lives
     * in `r1->as.str.str`. */
    IR_ASM
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

#define IR_VAL_FLAG_FUNC 0x1

struct IrValue {
    IrValueType type;
    IrValueKind kind;
    u64 flags;
    /* When non-NULL, this value is bound to a specific machine
     * register (TempleOS-style `<Type> reg <REG> name`). The codegen
     * uses the register name verbatim for reads / writes; the slot
     * allocator skips reserving a stack slot. Only meaningful for
     * IR_VAL_LOCAL / IR_VAL_PARAM. */
    AoStr *pinned_reg;

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
        /* For IR_ASM: list of AsmFragment* when the inline asm uses
         * `&var` references that need stack-offset substitution at
         * emit time. NULL when the asm body is pure text (read it
         * from r1's IR_VAL_CONST_STR). */
        List *asm_fragments;
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

typedef struct IrRaCtx {
    struct IrFunction *func;
    /* `Map<u32 IrValue.var.id -> int loff>`. loff is sign-extended on read;
     * never zero for a real param/local (those start below the frame base). */
    Map *id_to_loff;
    /* Bytes added to the stack pointer beyond the AST locals/params region
     * (for IR tmps and anything alloca'd at lowering time). The layout pass
     * reserves this in the function prologue. */
    int extra_stack;
} IrRaCtx;

typedef struct IrFunction {
    u32 uuid;
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
    /* Slot offset map + extra-stack counter + IR function. The
     * regalloc helpers in ir-regalloc.c take an IrRaCtx*; the codegen
     * passes `&ctx->ra` when calling into them. */
    IrRaCtx ra;
} IrFunction;

typedef struct IrCgCtx {
    Cctrl *cc;
    AoStr *buf;
    IrFunction *fn;
    /* Set during the per-block emission loop so terminator codegen
     * knows who's emitting and which block (if any) follows in layout
     * order. */
    IrBlock *cur_block;
    IrBlock *next_block;
    /* Map<u32 IrValue.var.id -> IrInstr* (IR_LEA)>. Populated once per
     * function from peephole-marked LEAs. The IR_CALL emit consults
     * this to re-create `leaq <source>, <target_reg>` directly at the
     * call site instead of going through the slot. NULL when no
     * LEA-into-call fusion fired. */
    Map *lea_inline_map;
    /* Vec<AoStr *>: machine registers that pinned locals occupy in
     * this function. Populated once at function emit start (walking
     * the AST locals). The prologue saves these, the epilogue
     * restores them. NULL when no pinned locals exist. */
    Vec *pinned_regs;
} IrCgCtx;

typedef struct IrProgram {
    Vec *functions;
    Vec *globals;
} IrProgram;

#define IR_LOOP_STACK_MAX 32

/* Keep track of breaks and continues */
typedef struct IrLoopCtx {
    IrBlock *continue_block;  /* target of `continue` */
    IrBlock *break_block;     /* target of `break` */
} IrLoopCtx;

typedef struct IrCtx {
    /* Current function being converted to IR */
    IrFunction *cur_func;
    /* Current ir block we are looking at */
    IrBlock *cur_block;
    /* The entrity of the program in IR */
    IrProgram *prog;
    /* The main compiler struct */
    Cctrl *cc;

    IrLoopCtx loop_stack[IR_LOOP_STACK_MAX];
    u16 loop_depth;
    /*`Map<AoStr *label_name, IrBlock *>` for goto and labels within the current
     * function. */
    Map *labels;
} IrCtx;

extern VecType vec_ir_block_type;
extern MapType map_u32_to_ir_block_type;

/* Memory management */
void irMemoryInit(void);
void irMemoryRelease(void);
void *irAlloc(u32 size);
void irMemoryStats(void);

IrCtx *irCtxNew(Cctrl *cc);
void irCtxAddFunction(IrCtx *ctx, IrFunction *func);
IrValue *irFnGetVar(IrFunction *func, u32 lvar_id);

IrBlockMapping *irFunctionGetBlockMapping(IrFunction *func, IrBlock *ir_block);
Map *irFunctionGetSuccessors(IrFunction *func, IrBlock *ir_block);
Map *irFunctionGetPredecessors(IrFunction *func, IrBlock *ir_block);
Map *irBlockGetPredecessors(IrFunction *func, IrBlock *block);
Map *irBlockGetSuccessors(IrFunction *func, IrBlock *block);

u8 irOpIsCmp(IrOp opcode);
u8 irIsFloat(IrValueType ir_value_type);
u8 irIsInt(IrValueType ir_value_type);
u8 irTypeIsScalar(IrValueType ir_value_type);
u8 irIsConst(IrValueKind ir_value_kind);
u8 irIsPtr(IrValueType ir_value_type);
u8 irIsTmp(IrValue *val);
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
int irBlockIsStartOrEnd(IrFunction *func, IrBlock *block);
int irBlockIsRedundantJump(IrFunction *func, IrBlock *block);
IrInstr *irBlockLastInstr(IrBlock *block);
IrInstr *irBlockFirstInstr(IrBlock *block);

/* Constructors */
Vec *irFunctionVecNew(void);
Vec *irPairVecNew(void);
Vec *irValueVecNew(void);
Vec *irBlockVecNew(void);

Map *irBlockMapNew(void);
Map *irBlockMappingMapNew(void);
Map *irInstrMapNew(void) ;
Map *irVarValueMapNew(void);

IrBlockMapping *irBlockMappingNew(int id);
IrBlock *irBlockNew(void);
IrValue *irTmp(IrValueType type, u16 size);
IrInstr *irInstrNew(IrOp op, IrValue *dst, IrValue *r1, IrValue *r2);
IrFunction *irFunctionNew(AoStr *fname);
IrValue *irValueNew(IrValueType type, IrValueKind kind);
IrPair *irPairNew(IrBlock *ir_block, IrValue *ir_value);
IrInstr *irPhi(IrValue *result);
IrInstr *irLoad(IrValue *ir_dest, IrValue *ir_value);
IrInstr *irJump(IrFunction *func, IrBlock *block, IrBlock *target);
IrValue *irConstInt(IrValueType type, s64 num);

u32 irValueByteSize(IrValue *v);

int irBlockIsOnlyJump(IrFunction *func, IrBlock *block);

void irFunctionAddMapping(IrFunction *func, IrBlock *src, IrBlock *dest);
void irFunctionAddSuccessor(IrFunction *func, IrBlock *src, IrBlock *dest);
void irFunctionAddPredecessor(IrFunction *func, IrBlock *src, IrBlock *prev);
void irFunctionRemoveSuccessor(IrFunction *func, IrBlock *src, IrBlock *dest);
void irFunctionRemovePredecessor(IrFunction *func, IrBlock *src, IrBlock *prev);

void irResetBlockId(void);
void irTmpVariableCountReset(void);

void irBlockMappingRelease(void *_mapping);
void irBlockRelease(IrBlock *block);
void irFunctionRelease(IrFunction *func);



void irAddPhiIncoming(IrInstr *ir_phi_instr,
                      IrValue *ir_value, 
                      IrBlock *ir_block);


void irFnAddVar(IrFunction *func, u32 lvar_id, IrValue *var);
void irFnAddBlock(IrFunction *fn, IrBlock *block);
int irSetVariable(IrFunction *func, u32 var_id, IrValue *var);
void irAddStackSpace(IrCtx *ctx, int size);

#endif
