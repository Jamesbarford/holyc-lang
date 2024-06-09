#ifndef AST_H
#define AST_H

#include "aostr.h"
#include "dict.h"
#include "list.h"

/* Relates to the 'kind' property on the AstType struct */
#define AST_TYPE_VOID         0
#define AST_TYPE_INT          1
#define AST_TYPE_FLOAT        2
#define AST_TYPE_CHAR         3
#define AST_TYPE_ARRAY        4
#define AST_TYPE_POINTER      5
#define AST_TYPE_FUNC         6
#define AST_TYPE_CLASS        7
#define AST_TYPE_VIS_MODIFIER 8
#define AST_TYPE_INLINE       9
#define AST_TYPE_UNION        10
#define AST_TYPE_AUTO         11

/* Relates to the 'kind' property on the Ast struct */
#define AST_GVAR           256
#define AST_DEREF          257
#define AST_GOTO           258
#define AST_LABEL          259
#define AST_ADDR           260
#define AST_LVAR           261
#define AST_FUNC           262
#define AST_DECL           263
#define AST_STRING         264
#define AST_FUNCALL        265
#define AST_LITERAL        266
#define AST_ARRAY_INIT     267
#define AST_IF             268
#define AST_FOR            269
#define AST_RETURN         270
#define AST_WHILE          271
#define AST_OP_ADD         272
#define AST_CLASS_REF      273
#define AST_COMPOUND_STMT  274
#define AST_ASM_STMT       275
#define AST_ASM_FUNC_BIND  276
#define AST_ASM_FUNCALL    277
#define AST_FUNPTR         278
#define AST_FUNPTR_CALL    279
#define AST_BREAK          280
#define AST_CONTINUE       281
#define AST_DEFAULT_PARAM  282
#define AST_VAR_ARGS       283
#define AST_ASM_FUNCDEF    284
#define AST_CAST           285
#define AST_FUN_PROTO      286
#define AST_CASE           287
#define AST_JUMP           288
#define AST_EXTERN_FUNC    289
#define AST_DO_WHILE       290
#define AST_PLACEHOLDER    291

typedef struct AstType AstType;
/* Type of the variable or type of return type of a function */
typedef struct AstType {
    int kind;
    int size;
    int has_var_args;

    /* Alignment of a struct or union */
    unsigned int alignment;

    /* Value signed or unsigned? */
    int issigned;

    /* Pointer or an array */
    AstType *ptr;

    /* Array length */
    int len;

    /* Class */
    aoStr *clsname;
    Dict *fields;
    int offset;
    int is_intrinsic;

    /* Function */
    AstType *rettype;
    List *params;
} AstType;

typedef struct Ast Ast;
typedef struct Ast {
    long kind;
    AstType *type;
    int loff;
    union {
        /* 8, 16, 32, 64 bit number */
        long long i64;

        /* Float & Double */
        struct {
            double f64;
            aoStr *f64_label;
        };

        /* Asm block */
        struct {
            aoStr *asm_stmt;
            List *funcs;
        };

        /* String */
        struct {
            aoStr *sval;
            aoStr *slabel;
        };

        /* Local variable */
        struct {
            aoStr *lname;
        };

        /* Global variable */
        struct {
            int is_static;
            aoStr *gname;
            aoStr *glabel;
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
            Ast *left;
            Ast *right;
        };

        /* Unary operator */
        struct {
            Ast *operand;
        };

        /* Function call, declaration, pointer or assembly */
        struct {
            /* Asm function binding */
            aoStr *asmfname;
            aoStr *fname;
            List *args;
            List *paramtypes;
            /* Declaration */
            List *params;
            List *locals;
            Ast *body;
            Ast *ref; /* for function pointers, a reference to the variable 
                       * allows for keeping track of the offset when converting 
                       * to assembly. */
            int has_var_args;
        };

        /* Declaration */
        struct {
            Ast *declvar;
            Ast *declinit;
        };

        /* Array initialiser */
        List *arrayinit;

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
            aoStr *for_begin;
            aoStr *for_middle;
            aoStr *for_end;
        };

        /* while statement */
        struct {
            Ast *whilecond;
            Ast *whilebody;
            aoStr *while_begin;
            aoStr *while_end;
        };

        /* Return statement */
        Ast *retval;

        /* Compound statement */
        List *stms;

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

        /* For a switch */
        struct {
            aoStr *case_label;
            long case_begin;
            long case_end;
        };

        struct {
            aoStr *jump_label;
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

Ast *AstNew(void);
AstType *AstTypeCopy(AstType *type);
void AstRelease(Ast *ast);
void AstReleaseList(List *ast_list);

/* Literals */
Ast *AstI64Type(long long val);
Ast *AstF64Type(double val);
Ast *AstCharType(long ch);
Ast *AstString(char *str, int len);

/* Declarations */
Ast *AstDecl(Ast *var, Ast *init);

/* Symbol operators i.e: +-*&^><*/
Ast *AstBinaryOp(long operation, Ast *left, Ast *right);
Ast *AstUnaryOperator(AstType *type, long kind, Ast *operand);

/* Variable definitions */
Ast *AstLVar(AstType *type, char *name, int len);
Ast *AstGVar(AstType *type, char *name, int len, int is_static);

/* More beefy data structures */
Ast *AstArrayInit(List *init);
Ast *AstCompountStatement(List *stmts);

/* Control */
Ast *AstFor(Ast *init, Ast *cond, Ast *step, Ast *body, aoStr *for_begin,
        aoStr *for_middle, aoStr *for_end);
Ast *AstIf(Ast *cond, Ast *then, Ast *els);
Ast *AstWhile(Ast *whilecond, Ast *whilebody, aoStr *while_begin,
        aoStr *while_end);
Ast *AstDoWhile(Ast *whilecond, Ast *whilebody, aoStr *while_begin, 
        aoStr *while_end);
Ast *AstContinue(aoStr *continue_label);
Ast *AstBreak(aoStr *break_label);
Ast *AstCase(aoStr *case_label, long case_begin, long case_end);

/* Functions */
Ast *AstFunctionCall(AstType *type, char *fname, int len, List *argv,
                     List *paramtypes);
Ast *AstFunction(AstType *rettype, char *fname, int len, List *params,
                 Ast *body, List *locals, int has_var_args);
Ast *AstReturn(Ast *retval, AstType *rettype);
Ast *AstFunctionPtr(AstType *type, char *fname, int len, 
        List *params);
Ast *AstFunctionPtrCall(AstType *type, char *fname, int len,
        List *argv, List *paramtypes, Ast *ref);
Ast *AstFunctionDefaultParam(Ast *var, Ast *init);
Ast *AstVarArgs(void);

Ast *AstAsmBlock(aoStr *asm_stmt, List *funcs);
Ast *AstAsmFunctionBind(AstType *rettype, aoStr *asm_fname, 
        aoStr *fname, List *params);
Ast *AstAsmFunctionCall(AstType *rettype, aoStr *asm_fname, List *argv,
        List *paramtypes);
Ast *AstAsmFunctionDef(aoStr *asm_fname, aoStr *asm_stmt);

/* Gotos */
Ast *AstGoto(aoStr *label);
Ast *AstLabel(aoStr *label);
Ast *AstJump(char *jumpname, int len);
Ast *AstDest(char *label, int len);

/* Pointers */
AstType *AstMakePointerType(AstType *type);
AstType *AstMakeArrayType(AstType *type, int len);
AstType *AstMakeClassField(AstType *type, int offset);
AstType *AstMakeFunctionType(AstType *rettype, List *param_types);
AstType *AstConvertArray(AstType *ast_type);
List *AstParamTypes(List *params);
Ast *AstClassRef(AstType *type, Ast *cls, char *field_name);
AstType *AstClassType(Dict *fields, aoStr *clsname, int size, int is_intrinsic);
Ast *AstCast(Ast *var, AstType *to);

AstType *AstGetResultType(long op, AstType *a, AstType *b);
AstType *AstTypeCheck(AstType *expected, Ast *ast);

aoStr *AstMakeLabel(void);
aoStr *AstMakeTmpName(void);
int AstIsIntType(AstType *type);
int AstIsFloatType(AstType *type);
int AstIsRangeOperator(long op);
Ast *AstGlobalCmdArgs(void);

aoStr *AstNormaliseFunctionName(char *fname);

/* For debugging */
char *AstTypeToString(AstType *type);
char *AstTypeToColorString(AstType *type);
char *AstKindToString(int kind);
char *AstFunctionToString(Ast *func);
char *AstFunctionNameToString(AstType *rettype, char *fname, int len);
char *AstToString(Ast *ast);
char *AstLValueToString(Ast *ast);
void AstPrint(Ast *ast);
void AstTypePrint(AstType *type);
void AstKindPrint(int kind);

#endif
