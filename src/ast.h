#ifndef AST_H
#define AST_H

#include "aostr.h"
#include "containers.h"
#include "list.h"

typedef enum AstUnOp {
    /* postfix (evaluated after value is yielded) */
    AST_UN_OP_POST_INC,   /* x++ */
    AST_UN_OP_POST_DEC,   /* x-- */

    /* prefix (evaluated before value is yielded) */
    AST_UN_OP_PRE_INC,    /* ++x */
    AST_UN_OP_PRE_DEC,    /* --x */

    /* arithmetic sign */
    AST_UN_OP_PLUS,       /* +x */
    AST_UN_OP_MINUS,      /* -x */

    /* logical & bitwise */
    AST_UN_OP_LOG_NOT,    /* !x */
    AST_UN_OP_BIT_NOT,    /* ~x */

    /* pointer & address */
    AST_UN_OP_ADDR_OF,    /* &x */
    AST_UN_OP_DEREF,      /* *p */

    AST_UN_OP_SIZEOF,
    AST_UN_OP_ALIGNOF,
    AST_UN_OP_CAST
} AstUnOp;

const char *astUnOpKindToString(AstUnOp op);

typedef enum AstBinOp {
    /* multiplicative */
    AST_BIN_OP_MUL,          /* a * b */
    AST_BIN_OP_DIV,          /* a / b */
    AST_BIN_OP_MOD,          /* a % b */

    /* additive */
    AST_BIN_OP_ADD,          /* a + b */
    AST_BIN_OP_SUB,          /* a - b */

    /* bit-shift */
    AST_BIN_OP_SHL,          /* a << b */
    AST_BIN_OP_SHR,          /* a >> b */

    /* relational */
    AST_BIN_OP_LT,           /* a <  b */
    AST_BIN_OP_LE,           /* a <= b */
    AST_BIN_OP_GT,           /* a >  b */
    AST_BIN_OP_GE,           /* a >= b */

    /* equality */
    AST_BIN_OP_EQ,           /* a == b */
    AST_BIN_OP_NE,           /* a != b */

    /* bitwise */
    AST_BIN_OP_BIT_AND,      /* a &  b */
    AST_BIN_OP_BIT_XOR,      /* a ^  b */
    AST_BIN_OP_BIT_OR,       /* a |  b */

    /* logical */
    AST_BIN_OP_LOG_AND,      /* a && b */
    AST_BIN_OP_LOG_OR,       /* a || b */

    /* assignment (simple + compound) */
    AST_BIN_OP_ASSIGN,       /* a  =  b */
    AST_BIN_OP_ADD_ASSIGN,   /* a += b */
    AST_BIN_OP_SUB_ASSIGN,   /* a -= b */
    AST_BIN_OP_MUL_ASSIGN,   /* a *= b */
    AST_BIN_OP_DIV_ASSIGN,   /* a /= b */
    AST_BIN_OP_MOD_ASSIGN,   /* a %= b */
    AST_BIN_OP_SHL_ASSIGN,   /* a <<= b */
    AST_BIN_OP_SHR_ASSIGN,   /* a >>= b */
    AST_BIN_OP_AND_ASSIGN,   /* a &= b */
    AST_BIN_OP_XOR_ASSIGN,   /* a ^= b */
    AST_BIN_OP_OR_ASSIGN     /* a |= b */
} AstBinOp;

const char *astBinOpKindToString(AstBinOp op);

/* Relates to the 'kind' property on the AstType struct */
typedef enum AstTypeKind {
    AST_TYPE_VOID         = 0,
    AST_TYPE_INT          = 1,
    AST_TYPE_FLOAT        = 2,
    AST_TYPE_CHAR         = 3,
    AST_TYPE_ARRAY        = 4,
    AST_TYPE_POINTER      = 5,
    AST_TYPE_FUNC         = 6,
    AST_TYPE_CLASS        = 7,
    AST_TYPE_VIS_MODIFIER = 8,
    AST_TYPE_INLINE       = 9,
    AST_TYPE_UNION        = 10,
    AST_TYPE_AUTO         = 11
} AstTypeKind;

/* Relates to the 'kind' property on the Ast struct */
typedef enum AstKind {
    AST_GVAR          = 256,
    AST_GOTO          = 258,
    AST_LABEL         = 259,
    AST_LVAR          = 261,
    AST_FUNC          = 262,
    AST_DECL          = 263,
    AST_STRING        = 264,
    AST_FUNCALL       = 265,
    AST_LITERAL       = 266,
    AST_ARRAY_INIT    = 267,
    AST_IF            = 268,
    AST_FOR           = 269,
    AST_RETURN        = 270,
    AST_WHILE         = 271,
    AST_CLASS_REF     = 273,
    AST_COMPOUND_STMT = 274,
    AST_ASM_STMT      = 275,
    AST_ASM_FUNC_BIND = 276,
    AST_ASM_FUNCALL   = 277,
    AST_FUNPTR        = 278,
    AST_FUNPTR_CALL   = 279,
    AST_BREAK         = 280,
    AST_CONTINUE      = 281,
    AST_DEFAULT_PARAM = 282,
    AST_VAR_ARGS      = 283,
    AST_ASM_FUNCDEF   = 284,
    AST_CAST          = 285,
    AST_FUN_PROTO     = 286,
    AST_CASE          = 287,
    AST_JUMP          = 288,
    AST_EXTERN_FUNC   = 289,
    AST_DO_WHILE      = 290,
    AST_PLACEHOLDER   = 291,
    AST_SWITCH        = 292,
    AST_DEFAULT       = 293,
    AST_SIZEOF        = 294,
    AST_COMMENT       = 295,
    AST_BINOP         = 296,
    AST_UNOP          = 297,
} AstKind;

/* @Cleanup
 * Urgently get rid of this, we do not need `n` ways of setting a label on 
 * an AST it is extremely confusing */
#define astHackedGetLabel(ast) \
    ((ast)->kind == AST_GOTO || (ast)->kind == AST_LABEL ? ((ast)->slabel ? (ast)->slabel : (ast)->sval) : \
    ((ast)->kind == AST_JUMP ? (ast)->jump_label : (ast)->kind == AST_CASE ? (ast)->case_label : NULL))


typedef struct AstType AstType;
/* Type of the variable or type of return type of a function */
typedef struct AstType {
    AstTypeKind kind;
    int size;
    int has_var_args;

    /* Alignment of a struct or union */
    u32 alignment;

    /* Value signed or unsigned? */
    int issigned;

    /* Pointer or an array */
    AstType *ptr;

    /* Array length */
    int len;

    /* Class */
    AoStr *clsname;
    Map *fields;
    int offset;
    int is_intrinsic;

    /* Function */
    AstType *rettype;
    Vec *params;
} AstType;


#define AST_FLAG_INLINE (1<<0)

typedef struct Ast Ast;
typedef struct Ast {
    AstKind kind;
    u64 flags;
    AstType *type;
    int loff;
    s64 deref_symbol;
    union {
        struct {
        /* 8, 16, 32, 64 bit number */
            s64 i64;
        };

        /* Float & Double */
        struct {
            double f64;
            AoStr *f64_label;
        };

        /* asm block */
        struct {
            AoStr *asm_stmt;
            List *funcs;
        };

        /* String */
        struct {
            AoStr *sval;
            AoStr *slabel;
            s64 real_len;
        };

        /* Local variable */
        struct {
            u32 lvar_id;
            AoStr *lname;
        };

        /* Global variable */
        struct {
            int is_static;
            AoStr *gname;
            AoStr *glabel;
        };

        /* Local reference */
        struct {
            Ast *lref;
            int lrefoff;
        };

        /* Global reference */
        struct {
            Ast *gref;
            int goff;
        };

        /* Binary operator */
        struct {
            AstBinOp binop;
            Ast *left;
            Ast *right;
        };

        /* Unary operator */
        struct {
            AstUnOp unop;
            Ast *operand;
        };

        /* Function call, declaration, pointer or assembly */
        struct {
            /* asm function binding */
            AoStr *asmfname;
            AoStr *fname;
            Vec *args;
            Vec *params;

            /* Declaration */
            List *locals;
            Ast *body;
            Ast *ref; /* for function pointers, a reference to the variable 
                       * allows for keeping track of the offset when converting 
                       * to assembly. */
            Ast *default_fn; /* For function pointers, allows setting a default
                              * value... we could use this for all default vals
                              * might be easier? */
            int has_var_args;
        };

        /* Declaration */
        struct {
            Ast *declvar;
            Ast *declinit;
        };

        struct {
            /* Array initialiser */
            List *arrayinit;
        };

        /* If statement */
        struct {
            Ast *cond;
            Ast *then;
            Ast *els;
        };

        /* For statement */
        struct {
            Ast *forinit;
            Ast *forcond;
            Ast *forstep;
            Ast *forbody;
            AoStr *for_begin;
            AoStr *for_middle;
            AoStr *for_end;
        };

        /* while statement */
        struct {
            Ast *whilecond;
            Ast *whilebody;
            AoStr *while_begin;
            AoStr *while_end;
        };

        struct {
            /* Return statement */
            Ast *retval;
        };

        /* @Typeo
         * Compound statement */
        struct {
            Ast *inline_ret;
            List *stms;
        };

        /* Class */
        struct {
            Ast *cls;
            char *field;
            AstType *fieldtype;
        };

        /* Var Args */
        struct {
            Ast *argc;
            Ast *argv;
        };

        /* For a case */
        struct {
            s64 case_begin;
            s64 case_end;
            AoStr *case_label;
            /* Scopes ast nodes to the case label... Think this makes sense;
             * in my head it does. As then all the top level case nodes 
             * have the begining and end ranges... making creating a jump table
             * feasible... or degrade into a series of 'ifs' if the range is 
             * sparse */
            List *case_asts;
        };

        /* For a switch */
        struct {
            int switch_bounds_checked;
            Ast *switch_cond;
            Ast *case_default;
            Ast **jump_table_order;
            Vec *cases;
            AoStr *case_end_label;
        };

        struct {
            AoStr *jump_label;
        };
    };
} Ast;

extern AstType *ast_int_type;
extern AstType *ast_uint_type;
extern AstType *ast_u8_type;
extern AstType *ast_i8_type;
extern AstType *ast_float_type;
extern AstType *ast_void_type;
extern AstType *ast_i16_type;
extern AstType *ast_u16_type;
extern AstType *ast_i32_type;
extern AstType *ast_u32_type;
extern Ast *placeholder_arg;
extern Ast *ast_loop_sentinal;
extern Ast *ast_forever_sentinal;

extern VecType vec_ast_type;
Vec *astVecNew(void);

extern MapType map_asttype_type;
extern MapType map_ast_type;
Map *astTypeMapNew(void);
Map *astMapNew(void);

void astMemoryInit(void);
void astMemoryRelease(void);
void astMemoryStats(void);

AstType *astTypeCopy(AstType *type);

/* Literals */
Ast *astI64Type(s64 val);
Ast *astF64Type(double val);
Ast *astCharType(s64 ch);
Ast *astString(char *str, int len, s64 real_len);

/* Declarations */
Ast *astDecl(Ast *var, Ast *init);

/* Symbol operators i.e: +-*&^><*/
Ast *astBinaryOp(AstBinOp operation, Ast *left, Ast *right, int *_is_err);
Ast *astUnaryOperator(AstType *type, AstUnOp operation, Ast *operand);

/* Variable definitions */
Ast *astLVar(AstType *type, char *name, int len);
Ast *astGVar(AstType *type, char *name, int len, int is_static);

/* More beefy data structures */
Ast *astArrayInit(List *init);
Ast *astCompountStatement(List *stmts);

/* Control */
Ast *astFor(Ast *init, Ast *cond, Ast *step, Ast *body, AoStr *for_begin,
        AoStr *for_middle, AoStr *for_end);
Ast *astIf(Ast *cond, Ast *then, Ast *els);
Ast *astWhile(Ast *whilecond, Ast *whilebody, AoStr *while_begin,
        AoStr *while_end);
Ast *astDoWhile(Ast *whilecond, Ast *whilebody, AoStr *while_begin, 
        AoStr *while_end);
Ast *astContinue(AoStr *continue_label);
Ast *astBreak(AoStr *break_label);
Ast *astCase(AoStr *case_label, s64 case_begin, s64 case_end, List *case_asts);
/* Do it with lists, then do it with a vector */
Ast *astSwitch(Ast *cond, Vec *cases, Ast *case_default,
        AoStr *case_end_label, int switch_bounds_checked);
Ast *astDefault(AoStr *case_label,List *case_asts);

/* Functions */
Ast *astFunctionCall(AstType *type, char *fname, int len, Vec *argv);
Ast *astFunction(AstType *rettype, char *fname, int len, Vec *params,
                 Ast *body, List *locals, int has_var_args);
Ast *astReturn(Ast *retval, AstType *rettype);
Ast *astFunctionPtr(AstType *type,
                    char *fname,
                    int fname_len, 
                    Vec *params);

Ast *astFunctionPtrCall(AstType *type,
                        char *fname,
                        int fname_len,
                        Vec *argv,
                        Ast *ref);

Ast *astFunctionDefaultParam(Ast *var, Ast *init);
Ast *astVarArgs(void);

Ast *astAsmBlock(AoStr *asm_stmt, List *funcs);
Ast *astAsmFunctionBind(AstType *rettype, AoStr *asm_fname, 
        AoStr *fname, Vec *params);
Ast *astAsmFunctionCall(AstType *rettype, AoStr *asm_fname, Vec *argv);
Ast *astAsmFunctionDef(AoStr *asm_fname, AoStr *asm_stmt);

/* Only used when transpiling */
Ast *astSizeOf(AstType *type);
Ast *astComment(char *comment, int len);

/* Gotos */
Ast *astGoto(AoStr *label);
Ast *astLabel(AoStr *label);
Ast *astJump(char *jumpname, int len);

/* Pointers */
AstType *astMakePointerType(AstType *type);
AstType *astMakeArrayType(AstType *type, int len);
AstType *astMakeClassField(AstType *type, int offset);
AstType *astMakeFunctionType(AstType *rettype, Vec *param_types);
AstType *astConvertArray(AstType *ast_type);
Ast *astClassRef(AstType *type, Ast *cls, char *field_name);
AstType *astClassType(Map *fields, AoStr *clsname, int size, int is_intrinsic);
Ast *astCast(Ast *var, AstType *to);

AstType *astGetResultType(AstBinOp op, AstType *a, AstType *b);
AstType *astTypeCheck(AstType *expected, Ast *ast, AstBinOp op);

/* Queries */
int astIsIntType(AstType *type);
int astIsFloatType(AstType *type);
int astTypeIsPtr(AstType *type);
int astTypeIsArray(AstType *type);
int astIsVarArg(Ast *ast);
int astIsRangeOperator(AstBinOp op);
int astIsBinCmp(s64 op);
int astIsAssignment(s64 op);
int astIsLabelMatch(Ast *ast, AoStr *goto_label);
int astIsAddr(Ast *ast);
int astIsDeref(Ast *ast);
int astIsUnOp(Ast *ast);
int astIsBinOp(Ast *ast);


AoStr *astMakeLabel(void);
AoStr *astMakeTmpName(void);
Ast *astGlobalCmdArgs(void);

AoStr *astNormaliseFunctionName(char *fname);
Ast *astMakeForeverSentinal(void);
Ast *astMakeLoopSentinal(void);
Ast *astMakePlaceHolder(void);
char *astAnnonymousLabel(void);
void astResetLVarId(void);

/* Returns `1` on successful conversion and `0` on failure */
int astUnaryOpFromToken(s64 op, AstUnOp *_result);
/* Returns `1` on successful conversion and `0` on failure */
int astBinOpFromToken(s64 op, AstBinOp *_result);

/* For debugging */
AoStr *astTypeToAoStr(AstType *type);
char *astTypeToString(AstType *type);
char *astTypeToColorString(AstType *type);
AoStr *astTypeToColorAoStr(AstType *type);
char *astKindToString(AstKind kind);
char *astTypeKindToString(AstTypeKind kind);
char *astFunctionToString(Ast *func);
char *astFunctionNameToString(AstType *rettype, char *fname, int len);
char *astToString(Ast *ast);
AoStr *astToAoStr(Ast *ast);
char *astLValueToString(Ast *ast, u64 lexme_flags);
AoStr *astLValueToAoStr(Ast *ast, u64 lexeme_flags);
void astPrint(Ast *ast);
void astTypePrint(AstType *type);
void astKindPrint(int kind);
const char *astTypeKindToHumanReadable(AstType *type);
const char *astKindToHumanReadable(Ast *ast);

#endif
