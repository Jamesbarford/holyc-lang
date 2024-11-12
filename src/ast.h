#ifndef AST_H
#define AST_H

#include "aostr.h"
#include "dict.h"
#include "map.h"
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
#define AST_SWITCH         292
#define AST_DEFAULT        293

/* @Cleanup
 * Urgently get rid of this, we do not need `n` ways of setting a label on 
 * an AST it is extremely confusing */
#define astHackedGetLabel(ast) \
    ((ast)->kind == AST_GOTO || (ast)->kind == AST_LABEL ? ((ast)->slabel ? (ast)->slabel : (ast)->sval) : \
    ((ast)->kind == AST_JUMP ? (ast)->jump_label : (ast)->kind == AST_CASE ? (ast)->case_label : NULL))


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

        /* asm block */
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
            /* asm function binding */
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

        /* @Typeo
         * Compound statement */
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

        /* For a case */
        struct {
            long case_begin;
            long case_end;
            aoStr *case_label;
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
            PtrVec *cases;
            aoStr *case_end_label;
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
extern Ast *ast_loop_sentinal;
extern Ast *ast_forever_sentinal;

AstType *astTypeCopy(AstType *type);
void astRelease(Ast *ast);
void astReleaseList(List *ast_list);

/* Literals */
Ast *astI64Type(long long val);
Ast *astF64Type(double val);
Ast *astCharType(long ch);
Ast *astString(char *str, int len);

/* Declarations */
Ast *astDecl(Ast *var, Ast *init);

/* Symbol operators i.e: +-*&^><*/
Ast *astBinaryOp(long operation, Ast *left, Ast *right);
Ast *astUnaryOperator(AstType *type, long kind, Ast *operand);

/* Variable definitions */
Ast *astLVar(AstType *type, char *name, int len);
Ast *astGVar(AstType *type, char *name, int len, int is_static);

/* More beefy data structures */
Ast *astArrayInit(List *init);
Ast *astCompountStatement(List *stmts);

/* Control */
Ast *astFor(Ast *init, Ast *cond, Ast *step, Ast *body, aoStr *for_begin,
        aoStr *for_middle, aoStr *for_end);
Ast *astIf(Ast *cond, Ast *then, Ast *els);
Ast *astWhile(Ast *whilecond, Ast *whilebody, aoStr *while_begin,
        aoStr *while_end);
Ast *astDoWhile(Ast *whilecond, Ast *whilebody, aoStr *while_begin, 
        aoStr *while_end);
Ast *astContinue(aoStr *continue_label);
Ast *astBreak(aoStr *break_label);
Ast *astCase(aoStr *case_label, long case_begin, long case_end, List *case_asts);
/* Do it with lists, then do it with a vector */
Ast *astSwitch(Ast *cond, PtrVec *cases, Ast *case_default,
        aoStr *case_end_label, int switch_bounds_checked);
Ast *astDefault(aoStr *case_label,List *case_asts);

/* Functions */
Ast *astFunctionCall(AstType *type, char *fname, int len, List *argv,
                     List *paramtypes);
Ast *astFunction(AstType *rettype, char *fname, int len, List *params,
                 Ast *body, List *locals, int has_var_args);
Ast *astReturn(Ast *retval, AstType *rettype);
Ast *astFunctionPtr(AstType *type, char *fname, int len, 
        List *params);
Ast *astFunctionPtrCall(AstType *type, char *fname, int len,
        List *argv, List *paramtypes, Ast *ref);
Ast *astFunctionDefaultParam(Ast *var, Ast *init);
Ast *astVarArgs(void);

Ast *astAsmBlock(aoStr *asm_stmt, List *funcs);
Ast *astAsmFunctionBind(AstType *rettype, aoStr *asm_fname, 
        aoStr *fname, List *params);
Ast *astAsmFunctionCall(AstType *rettype, aoStr *asm_fname, List *argv,
        List *paramtypes);
Ast *astAsmFunctionDef(aoStr *asm_fname, aoStr *asm_stmt);

/* Gotos */
Ast *astGoto(aoStr *label);
Ast *astLabel(aoStr *label);
Ast *astJump(char *jumpname, int len);

/* Pointers */
AstType *astMakePointerType(AstType *type);
AstType *astMakeArrayType(AstType *type, int len);
AstType *astMakeClassField(AstType *type, int offset);
AstType *astMakeFunctionType(AstType *rettype, List *param_types);
AstType *astConvertArray(AstType *ast_type);
List *astParamTypes(List *params);
Ast *astClassRef(AstType *type, Ast *cls, char *field_name);
AstType *astClassType(Dict *fields, aoStr *clsname, int size, int is_intrinsic);
Ast *astCast(Ast *var, AstType *to);

AstType *astGetResultType(long op, AstType *a, AstType *b);
AstType *astTypeCheck(AstType *expected, Ast *ast);

aoStr *astMakeLabel(void);
aoStr *astMakeTmpName(void);
int astIsIntType(AstType *type);
int astIsFloatType(AstType *type);
int astIsRangeOperator(long op);
Ast *astGlobalCmdArgs(void);

aoStr *astNormaliseFunctionName(char *fname);
int astIsAssignment(long op);
Ast *astMakeForeverSentinal(void);
Ast *astMakeLoopSentinal(void);

int astIsLabelMatch(Ast *ast, aoStr *goto_label);

/* For debugging */
aoStr *astTypeToAoStr(AstType *type);
char *astTypeToString(AstType *type);
char *astTypeToColorString(AstType *type);
aoStr *astTypeToColorAoStr(AstType *type);
char *astKindToString(int kind);
char *astFunctionToString(Ast *func);
char *astFunctionNameToString(AstType *rettype, char *fname, int len);
char *astToString(Ast *ast);
char *astLValueToString(Ast *ast, unsigned long lexme_flags);
void astPrint(Ast *ast);
void astTypePrint(AstType *type);
void astKindPrint(int kind);

#endif
