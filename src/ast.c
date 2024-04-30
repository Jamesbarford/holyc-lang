#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "aostr.h"
#include "ast.h"
#include "config.h"
#include "dict.h"
#include "list.h"
#include "lexer.h"
#include "util.h"

AstType *ast_u8_type = &(AstType){.kind = AST_TYPE_CHAR, .size = 1, .ptr = NULL,.issigned=0};
AstType *ast_i8_type = &(AstType){.kind = AST_TYPE_CHAR, .size = 1, .ptr = NULL,.issigned=1};
AstType *ast_i16_type = &(AstType){.kind = AST_TYPE_INT, .size = 2, .ptr = NULL,.issigned=1};
AstType *ast_u16_type = &(AstType){.kind = AST_TYPE_INT, .size = 2, .ptr = NULL,.issigned=0};
AstType *ast_i32_type = &(AstType){.kind = AST_TYPE_INT, .size = 4, .ptr = NULL,.issigned=1};
AstType *ast_u32_type = &(AstType){.kind = AST_TYPE_INT, .size = 4, .ptr = NULL,.issigned=0};
AstType *ast_int_type = &(AstType){.kind = AST_TYPE_INT, .size = 8, .ptr = NULL,.issigned=1};
AstType *ast_uint_type = &(AstType){.kind = AST_TYPE_INT, .size = 8, .ptr = NULL,.issigned=0};

AstType *ast_float_type = &(AstType){.kind = AST_TYPE_FLOAT, .size = 8, .ptr = NULL,.issigned=0};
AstType *ast_void_type = &(AstType){.kind = AST_TYPE_VOID, .size = 0, .ptr = NULL,.issigned=0};
AstType *ast_auto_type = &(AstType){.kind = AST_TYPE_VOID, .size = 0, .ptr = NULL,.issigned=0};

Ast *placeholder_arg = &(Ast){ .kind = AST_PLACEHOLDER};

void _AstToString(aoStr *str, Ast *ast, int depth);
char *_AstToStringRec(Ast *ast, int depth);

Ast *AstNew(void) {
    Ast *ast; 
    if ((ast = calloc(1,sizeof(Ast))) == NULL) {
        loggerPanic("OOM when allocating AST\n");
    }
    return ast;
}

AstType *AstTypeNew(void) {
    AstType *at;
    if ((at = calloc(1,sizeof(AstType))) == NULL) {
        loggerPanic("OOM when allocating AstType\n");
    }
    return at;
}

AstType *AstTypeCopy(AstType *type) {
    AstType *copy = AstTypeNew();
    memcpy(copy,type,sizeof(AstType));
    return copy;
}

Ast *AstUnaryOperator(AstType *type, long kind, Ast *operand) {
    Ast *ast = AstNew();
    ast->type = type;
    ast->kind = kind;
    ast->operand = operand;
    return ast;
}

static void AstFreeUnaryOperator(Ast *ast) {
    free(ast);
}

Ast *AstBinaryOp(long operation, Ast *left, Ast *right) {
    Ast *ast = AstNew();
    ast->type = AstGetResultType(operation,left->type,right->type);  
    if (ast->type == NULL) {
        loggerPanic("Incompatiable operands: %s: "ESC_GREEN"%s %s"ESC_RESET"\n",
                AstKindToString(operation),
                AstToString(left),
                AstToString(right));
    }
    ast->kind = operation;
    if (operation != '=' &&
            AstConvertArray(left->type)->kind != AST_TYPE_POINTER &&
            AstConvertArray(right->type)->kind == AST_TYPE_POINTER) {
        ast->left = right;
        ast->right = left;
    } else {
        ast->left = left;
        ast->right = right;
    }
    return ast;
}

static void AstFreeBinaryOp(Ast *ast) {
    AstRelease(ast->left);
    AstRelease(ast->right);
    free(ast);
}

Ast *AstI64Type(long long val) {
    Ast *ast = AstNew();
    ast->kind = AST_LITERAL;
    ast->type = ast_int_type;
    ast->i64 = val;
    return ast;
}

Ast *AstCharType(long ch) {
    Ast *ast = AstNew();
    ast->kind = AST_LITERAL;
    ast->type = ast_u8_type;
    ast->i64 = ch;
    return ast;
}

Ast *AstF64Type(double val) {
    Ast *ast = AstNew();
    ast->kind = AST_LITERAL;
    ast->type = ast_float_type;
    ast->f64 = val;
    ast->f64_label = NULL;
    return ast;
}
static void AstFreeLiteral(Ast *ast) {
    free(ast);
}

static int label_sequence = 0;
aoStr *AstMakeLabel(void) {
    aoStr *s = aoStrNew();
    aoStrCatPrintf(s, ".L%d", label_sequence++);
    return s;
}

static int tmp_name_sequence = 0;
aoStr *AstMakeTmpName(void) {
    aoStr *s = aoStrNew();
    aoStrCatPrintf(s, ".T%d", tmp_name_sequence++);
    return s;
}

Ast *AstLVar(AstType *type, char *name, int len) {
    Ast *ast = AstNew();
    ast->kind = AST_LVAR;
    ast->type = type;
    ast->lname = aoStrDupRaw(name, len);
    return ast;
}
static void AstFreeLVar(Ast *ast) {
    aoStrRelease(ast->lname);
    free(ast);
}

Ast *AstGVar(AstType *type, char *name, int len, int local_file) {
    Ast *ast = AstNew();
    ast->kind = AST_GVAR;
    ast->type = type;
    ast->gname = aoStrDupRaw(name, len);
    ast->glabel = local_file ? AstMakeLabel() : ast->gname;
    return ast;
}
static void AstFreeGVar(Ast *ast) {
    aoStrRelease(ast->gname);
    ast->gname = NULL;
    aoStrRelease(ast->glabel);
    free(ast);
}

Ast *AstString(char *str, int len) {
    Ast *ast = AstNew();
    ast->kind = AST_STRING;
    ast->sval = aoStrDupRaw(str,len);
    ast->type = AstMakeArrayType(ast_u8_type, ast->sval->len + 1);
    ast->slabel = AstMakeLabel();
    return ast;
}
static void AstFreeString(Ast *ast) {
    aoStrRelease(ast->sval);
    aoStrRelease(ast->slabel);
    free(ast);
}

Ast *AstFunctionCall(AstType *type, char *fname, int len, List *argv, List *paramtypes) {
    Ast *ast = AstNew();
    ast->type = type;
    ast->kind = AST_FUNCALL;
    ast->fname = aoStrDupRaw(fname, len);
    ast->args = argv;
    ast->paramtypes = paramtypes;
    return ast;
}
static void AstFreeFunctionCall(Ast *ast) {
    aoStrRelease(ast->fname);
    AstReleaseList(ast->args);
    AstReleaseList(ast->paramtypes);
    free(ast);
}

Ast *AstFunctionDefaultParam(Ast *var, Ast *init) {
    Ast *ast = AstNew();
    ast->kind = AST_DEFAULT_PARAM;
    ast->type = var->type;
    ast->declvar = var;
    ast->declinit = init;
    return ast;
}
static void AstFreeFunctionDefaultParam(Ast *ast) {
    AstRelease(ast->declvar);
    AstRelease(ast->declinit);
    free(ast);
}

Ast *AstFunctionPtrCall(AstType *type, char *fname, int len,
     List *argv, List *paramtypes, Ast *ref)
{
    Ast *ast = AstNew();
    ast->type = type;
    ast->kind = AST_FUNPTR_CALL;
    ast->fname = aoStrDupRaw(fname, len);
    ast->args = argv;
    ast->ref = ref;
    ast->paramtypes = paramtypes;
    return ast;
}
static void AstFreeFunctionPtrCall(Ast *ast) {
    /* paramtypes is a shared pointer */
    AstReleaseList(ast->args);
    aoStrRelease(ast->fname);
    free(ast);
}

Ast *AstFunctionPtr(AstType *type, char *fname, int len, List *params) {
    Ast *ast = AstNew();
    ast->type = type;
    ast->kind = AST_FUNPTR;
    ast->fname = aoStrDupRaw(fname, len);
    ast->params = params;
    return ast;
}
static void AstFreeFunctionPtr(Ast *ast) {
    AstReleaseList(ast->params);
    aoStrRelease(ast->fname);
    free(ast);
}

Ast *AstFunction(AstType *type, char *fname, int len, List *params, Ast *body,
        List *locals, int has_var_args)
{
    Ast *ast = AstNew();
    ast->type = type;
    type->has_var_args = has_var_args;
    ast->kind = AST_FUNC;
    ast->fname = aoStrDupRaw(fname, len);
    ast->params = params;
    ast->locals = locals;
    ast->body = body;
    ast->has_var_args = has_var_args;
    ast->paramtypes = NULL;
    return ast;
}
static void AstFreeFunction(Ast *ast) {
    aoStrRelease(ast->fname);
    AstRelease(ast->body);
    AstReleaseList(ast->params);
    AstReleaseList(ast->locals);
    free(ast);
}

Ast *AstDecl(Ast *var, Ast *init) {
    Ast *ast = AstNew();
    ast->kind = AST_DECL;
    ast->type = NULL;
    ast->declvar = var;
    ast->declinit = init;
    return ast;
}
static void AstFreeDecl(Ast *ast) {
    AstRelease(ast->declvar);
    AstRelease(ast->declinit);
    free(ast);
}

Ast *AstArrayInit(List *init) {
    Ast *ast = AstNew();
    ast->type = NULL;
    ast->kind = AST_ARRAY_INIT;
    ast->arrayinit = init;
    return ast;
}
static void AstFreeArrayInit(Ast *ast) {
    AstReleaseList(ast->arrayinit);
    free(ast);
}

Ast *AstIf(Ast *cond, Ast *then, Ast *els) {
    Ast *ast = AstNew();
    ast->type = NULL;
    ast->kind = AST_IF;
    ast->cond = cond;
    ast->then = then;
    ast->els = els;
    return ast;
}
static void AstFreeIf(Ast *ast) {
    AstRelease(ast->cond);
    AstRelease(ast->then);
    AstRelease(ast->els);
    free(ast);
}

Ast *AstCase(aoStr *case_label, long case_begin, long case_end) {
    Ast *ast = AstNew();
    ast->type = ast_int_type;
    ast->kind = AST_CASE;
    ast->case_begin = case_begin;
    ast->case_end = case_end;
    ast->case_label = case_label;
    return ast;
}
static void AstFreeCase(Ast *ast) {
    aoStrRelease(ast->case_label);
    free(ast);
}

Ast *AstDest(char *name, int len) {
    Ast *ast = AstNew();
    ast->kind = AST_LABEL;
    ast->sval= aoStrDupRaw(name,len);
    ast->slabel = NULL;
    return ast;
}

Ast *AstJump(char *name, int len) {
    Ast *ast = AstNew();
    ast->kind = AST_JUMP;
    ast->jump_label = aoStrDupRaw(name,len);
    return ast;
}
static void AstFreeJump(Ast *ast) {
    aoStrRelease(ast->jump_label);
    free(ast);
}


Ast *AstFor(Ast *init, Ast *cond, Ast *step, Ast *body, aoStr *for_begin,
        aoStr *for_middle, aoStr *for_end)
{
    Ast *ast = AstNew();
    ast->type = NULL;
    ast->kind = AST_FOR;
    ast->forinit = init;
    ast->forcond = cond;
    ast->forstep = step;
    ast->forbody = body;
    ast->for_begin = for_begin;
    ast->for_middle = for_middle;
    ast->for_end = for_end;
    return ast;
}
static void AstFreeFor(Ast *ast) {
    AstRelease(ast->forinit);
    AstRelease(ast->forcond);
    AstRelease(ast->forstep);
    AstRelease(ast->forbody);
    aoStrRelease(ast->for_begin);
    aoStrRelease(ast->for_middle);
    aoStrRelease(ast->for_end);
    free(ast);
}

Ast *AstDoWhile(Ast *whilecond, Ast *whilebody, aoStr *while_begin, 
        aoStr *while_end)
{
    Ast *ast = AstNew();
    ast->type = NULL;
    ast->kind = AST_DO_WHILE;
    ast->whilebody = whilebody;
    ast->whilecond = whilecond;
    ast->while_begin = while_begin;
    ast->while_end = while_end;
    return ast;
}

Ast *AstWhile(Ast *whilecond, Ast *whilebody, aoStr *while_begin, 
        aoStr *while_end)
{
    Ast *ast = AstNew();
    ast->type = NULL;
    ast->kind = AST_WHILE;
    ast->whilecond = whilecond;
    ast->whilebody = whilebody;
    ast->while_begin = while_begin;
    ast->while_end = while_end;
    return ast;
}
static void AstFreeWhile(Ast *ast) {
    AstRelease(ast->whilecond);
    AstRelease(ast->whilebody);
    aoStrRelease(ast->while_begin);
    aoStrRelease(ast->while_end);
    free(ast);
}

/* Duplicates the label */
Ast *AstContinue(aoStr *continue_label) {
    Ast *ast = AstNew();
    ast->type = NULL;
    ast->kind = AST_CONTINUE;
    ast->slabel = aoStrDup(continue_label);
    return ast;
}
static void AstFreeContinue(Ast *ast) {
    aoStrRelease(ast->slabel);
    free(ast);
}

/* XXX: reference count string? 
 * Duplicates the label */
Ast *AstBreak(aoStr *break_label) {
    Ast *ast = AstNew();
    ast->type = NULL;
    ast->kind = AST_BREAK;
    ast->slabel = aoStrDup(break_label);
    return ast;
}
static void AstFreeBreak(Ast *ast) {
    aoStrRelease(ast->slabel);
    free(ast);
}

Ast *AstReturn(Ast *retval, AstType *rettype) {
    Ast *ast = AstNew();
    ast->kind = AST_RETURN;
    ast->type = rettype;
    ast->retval = retval;
    return ast;
}
static void AstFreeReturn(Ast *ast) {
    AstRelease(ast->retval);
    free(ast);
}

AstType *AstMakePointerType(AstType *type) {
    AstType *pointer_type = AstTypeNew();
    pointer_type->kind = AST_TYPE_POINTER;
    pointer_type->ptr = type;
    pointer_type->size = 8;
    return pointer_type;
}

AstType *AstMakeArrayType(AstType *type, int len) {
    AstType *array_type = AstTypeNew();
    array_type->kind = AST_TYPE_ARRAY;
    array_type->ptr = type;
    /* How much memory */
    array_type->size = len < 0 ? -1 : type->size * len;
    /* Length of array */
    array_type->len = len;
    return array_type;
}

AstType *AstConvertArray(AstType *ast_type) {
    if (ast_type->kind != AST_TYPE_ARRAY) {
        return ast_type;
    }
    return AstMakePointerType(ast_type->ptr);
}

Ast *AstGoto(aoStr *label) {
    Ast *ast = AstNew();
    ast->type = NULL;
    ast->kind = AST_GOTO;
    ast->slabel = label;
    return ast;
}
static void AstFreeGoto(Ast *ast) {
    aoStrRelease(ast->slabel);
    free(ast);
}

Ast *AstLabel(aoStr *label) {
    Ast *ast = AstNew();
    ast->type = NULL;
    ast->kind = AST_LABEL;
    ast->slabel = label;
    return ast;
}
static void AstFreeLabel(Ast *ast) {
    aoStrRelease(ast->slabel);
    free(ast);
}

AstType *AstMakeFunctionType(AstType *rettype, List *param_types) {
    AstType *type = AstTypeNew();
    type->kind = AST_TYPE_FUNC;
    type->rettype = rettype;
    type->params = param_types;
    type->size = 8;
    return type;
}

AstType *AstMakeClassField(AstType *type, int offset) {
    AstType *field_type = AstTypeNew();
    memcpy(field_type,type,sizeof(AstType));
    field_type->clsname = NULL;
    field_type->offset = offset;
    return field_type;
}

Ast *AstClassRef(AstType *type, Ast *cls, char *field_name) {
    Ast *ref = AstNew();
    ref->kind = AST_CLASS_REF;
    ref->type = type;
    ref->cls = cls;
    ref->field = field_name;
    return ref;
}
static void AstFreeClassRef(Ast *ast) {
    AstRelease(ast->cls);
    free(ast->field);
    free(ast);
}

AstType *AstClassType(Dict *fields, aoStr *clsname, int size, int is_intrinsic) {
    AstType *ref = AstTypeNew();
    ref->kind = AST_TYPE_CLASS;
    ref->fields = fields;
    ref->size = size;
    ref->clsname = clsname;
    ref->is_intrinsic = is_intrinsic;
    return ref;
}

Ast *AstCompountStatement(List *stmts) {
    Ast *ast = AstNew();
    ast->kind = AST_COMPOUND_STMT;
    ast->type = NULL;
    ast->stms = stmts;
    return ast;
}
static void AstFreeCompountStatement(Ast *ast) {
    AstReleaseList(ast->stms);
    free(ast);
}

Ast *AstAsmBlock(aoStr *asm_stmt, List *funcs) {
    Ast *ast = AstNew();
    ast->kind = AST_ASM_STMT;
    ast->type = NULL;
    ast->asm_stmt = asm_stmt;
    ast->funcs = funcs;
    return ast;
}
static void AstFreeAsmBlock(Ast *ast) {
    aoStrRelease(ast->asm_stmt);
    AstReleaseList(ast->funcs);
    free(ast);
}

Ast *AstAsmFunctionDef(aoStr *asm_fname, aoStr *asm_stmt) {
    Ast *ast = AstNew();
    ast->kind = AST_ASM_FUNCDEF;
    ast->asmfname = asm_fname;
    ast->body = AstNew();
    ast->body->asm_stmt = asm_stmt;
    return ast;
}
static void AstFreeAsmFunctionDef(Ast *ast) {
    aoStrRelease(ast->asmfname);
    aoStrRelease(ast->body->asm_stmt);
    free(ast->body);
    free(ast);
}

Ast *AstAsmFunctionBind(AstType *rettype, aoStr *asm_fname,
        aoStr *fname, List *params) {
    Ast *ast = AstNew();
    ast->kind = AST_ASM_FUNC_BIND;
    ast->fname = fname;
    ast->asmfname = asm_fname;
    ast->type = rettype;
    ast->params = params;
    return ast;
}
static void AstFreeAsmFunctionBind(Ast *ast) {
    aoStrRelease(ast->asmfname);
    aoStrRelease(ast->fname);
    AstReleaseList(ast->params);
    free(ast);
}

Ast *AstAsmFunctionCall(AstType *rettype, aoStr *asm_fname, List *argv,
        List *paramtypes) {
    Ast *ast = AstNew();
    ast->type = rettype;
    ast->kind = AST_ASM_FUNCALL;
    ast->fname = asm_fname;
    ast->args = argv;
    ast->paramtypes = paramtypes;
    return ast;
}
static void AstFreeAsmFunctionCall(Ast *ast) {
    aoStrRelease(ast->asmfname);
    aoStrRelease(ast->fname);
    AstReleaseList(ast->args);
    AstReleaseList(ast->paramtypes);
    free(ast);
}

Ast *AstVarArgs(void) {
    Ast *ast = AstNew();
    ast->kind = AST_VAR_ARGS;
    ast->type = NULL;
    ast->argc = AstLVar(ast_int_type,"argc",4);
    ast->argv = AstLVar(AstMakeArrayType(ast_int_type,0),"argv",4);
    return ast;
}
static void AstFreeVarArgs(Ast *ast) {
    AstRelease(ast->argc);
    AstRelease(ast->argv);
    free(ast);
}
Ast *AstGlobalCmdArgs(void) {
    Ast *ast = AstNew();
    ast->kind = AST_VAR_ARGS;
    ast->type = NULL;
    ast->argc = AstDecl(AstGVar(ast_int_type,"argc",4,0),NULL);
    ast->argv = AstDecl(AstGVar(AstMakePointerType(AstMakePointerType(ast_u8_type)),"argv",4,0),NULL);
    return ast;
}

Ast *AstCast(Ast *var, AstType *to) {
    Ast *ast = AstNew();
    ast->kind = AST_CAST;
    ast->operand = var;
    ast->type = to;
    return ast;
}
static void AstFreeCast(Ast *ast) {
    AstRelease(ast->operand);
    free(ast);
}

aoStr *AstNormaliseFunctionName(char *fname) {
    aoStr *newfn = aoStrNew();
#if IS_BSD
    if (fname[0] != '_') {
        aoStrPutChar(newfn, '_');
    }
#endif
    aoStrCatPrintf(newfn,fname);
    /* XXX: Dynamically create main function */
    return newfn;
}

/* Get the type from the function parameters, this includes getting the types 
 * from default arguments */
List *AstParamTypes(List *params) {
    List *ref = ListNew();
    List *it = params->next;
    Ast *ast;
    AstType *type;
    while (it != params) {
        ast = (Ast *)it->value;
        switch (ast->kind) {
        case AST_DEFAULT_PARAM: type = ast->declinit->type; break;
        case AST_VAR_ARGS: type = ast_void_type; break;
        default:
            type = ast->type; break;
        }
        ListAppend(ref,type);
        it = it->next;
    }
    return ref;
}

int AstIsAssignment(long op) {
    switch (op) {
        case '=':
        case TK_SHL_EQU:
        case TK_SHR_EQU:
        case TK_OR_EQU:
        case TK_AND_EQU:
        case TK_ADD_EQU:
        case TK_SUB_EQU:
        case TK_MUL_EQU:
        case TK_DIV_EQU:
        case TK_MOD_EQU:
        /* These can be treated as short hand expressions for */
        case TK_PLUS_PLUS:
        case TK_PRE_PLUS_PLUS:
        case TK_MINUS_MINUS:
        case TK_PRE_MINUS_MINUS:
            return 1;
        default:
            return 0;
    }
}

void AssertIsValidPointerOp(long op, long lineno) {
    switch (op) {
        case '-': case '+':
        case '<': case TK_LESS_EQU:
        case '>': case TK_GREATER_EQU:
        case TK_EQU_EQU: case TK_NOT_EQU:
        case TK_OR_OR: case TK_AND_AND:    
            return;
        default:
            loggerPanic("Invalid pointer operation: %s at line: %ld\n",
                    AstKindToString(op),lineno);
    }
}

/* This is pretty gross to look at but, eliminated recursion */
AstType *AstGetResultType(long op, AstType *a, AstType *b) {
    AstType *tmp;
    AstType *ptr1, *ptr2;
    ptr1 = AstConvertArray(a);
    ptr2 = AstConvertArray(b);

start_routine:
    if (ptr1->kind > ptr2->kind) {
        tmp = ptr2;
        ptr2 = ptr1;
        ptr1 = tmp;
    }

    if (ptr2->kind == AST_TYPE_POINTER || ptr2->kind == AST_TYPE_FUNC) {
        if (op == '=') {
            return ptr1;
        }

        AssertIsValidPointerOp(op,-1);
        if (op != '+' && op != '-'
                && op != TK_EQU_EQU &&
                op != TK_NOT_EQU && op != TK_OR_OR && op != TK_AND_AND) {
            goto error;
        }

        if (ptr2->kind == ptr1->kind) {
            return ast_uint_type;
        }

        if (ptr1->kind != AST_TYPE_INT) {
            goto error;
        }
        /* These are the only arithmetic operators for pointers,
         * the rest are boolean */
        if (op != '+' && op != '-') {
            return ast_uint_type;
        }
        return ptr2;
    }
    switch (ptr1->kind) {
    case AST_TYPE_VOID:
        goto error;
    case AST_TYPE_INT:
    case AST_TYPE_CHAR:
        switch (ptr2->kind) {
        case AST_TYPE_INT:
        case AST_TYPE_CHAR:
            return ast_int_type;
        case AST_TYPE_CLASS:
            if (ptr2->is_intrinsic) {
                return ast_int_type;
            } else {
                goto error;
            }
        case AST_TYPE_FLOAT:
            return ast_float_type;
        case AST_TYPE_ARRAY:
        case AST_TYPE_POINTER:
            return ptr2;
        }
    case AST_TYPE_FLOAT:
        if (ptr2->kind == AST_TYPE_FLOAT) {
            return ast_float_type;
        }
        goto error;
    case AST_TYPE_ARRAY:
        if (ptr2->kind != AST_TYPE_ARRAY) {
            goto error;
        }
        ptr1 = ptr1->ptr;
        ptr2 = ptr2->ptr;
        goto start_routine;
    case AST_TYPE_CLASS:
        if (ptr1->is_intrinsic && ptr2->is_intrinsic) {
            return ast_int_type;
        } else if (AstIsIntType(ptr1) && ptr2->is_intrinsic) {
            return ast_int_type;
        } else if (ptr1->is_intrinsic && AstIsIntType(ptr2)) {
            return ast_int_type;
        } else {
            goto error;
        }
    }

error:
    return NULL;
}

AstType *AstTypeCheck(AstType *expected, Ast *ast) {
    AstType *original_actual = ast->type;
    AstType *actual;
    if (original_actual == NULL) {
        original_actual = ast_void_type;
        actual = original_actual;
    } else {
        original_actual = ast->type;
        actual = AstConvertArray(original_actual);
    }
    AstType *a = actual;
    AstType *e = expected;
    AstType *ret = NULL;

    if (a->kind == AST_TYPE_ARRAY || e->kind == AST_TYPE_ARRAY) {
        goto out;
    }

check_type:
    if (e == NULL || a == NULL) {
        goto out;
    }

    if ((e->kind == AST_TYPE_POINTER || e->kind == AST_TYPE_FUNC) && a->kind == AST_TYPE_INT) {
        if (ast->kind == AST_LITERAL && ast->i64 == 0) {
            ret = e;
            goto out;
        } else {
            goto out;
        }
    } else if (e->kind == AST_TYPE_POINTER && a->kind != AST_TYPE_POINTER) {
        goto out;
    } else if (a->kind == AST_TYPE_POINTER && e->kind != AST_TYPE_POINTER) {
        /* this is a generic pointer */
        if (e->kind == AST_TYPE_VOID && expected->kind == AST_TYPE_POINTER) {
            ret = expected;
            goto out;
        } else {
            goto out;
        }
    } else if (e->kind == AST_TYPE_POINTER && a->kind == AST_TYPE_POINTER) {
        e = e->ptr;
        a = a->ptr;
        goto check_type;
    } else if (AstIsIntType(e) && AstIsIntType(a)) {
        ret = e;
        goto out;
    } else if (AstIsFloatType(e) && AstIsFloatType(a)) {
        ret = e;
        goto out;
    } else if (e->kind == a->kind) {
        ret = e;
        goto out;
    } else if (e->kind != a->kind) {
        if (expected->kind == AST_TYPE_POINTER && actual->kind == AST_TYPE_POINTER) {
            if (e->kind == AST_TYPE_VOID || a->kind == AST_TYPE_VOID) {
                ret = e;
                goto out;
            } else {
                goto out;
            }
        } else if (e->kind == AST_TYPE_CLASS && e->is_intrinsic && AstIsIntType(a)) {
            ret = e;
            goto out;
        } 
    }

out:
    if (actual != original_actual) {
        free(actual);
    }
    return ret;
}

int AstIsFloatType(AstType *type) {
    return type->kind == AST_TYPE_FLOAT;
}

int AstIsIntType(AstType *type) {
    switch (type->kind) {
    case AST_TYPE_INT:
    case AST_TYPE_CHAR:
        return 1;
    default: return 0;
    }
}

void AstStringEndStmt(aoStr *str) {
    if (str->data[str->len-1] != '\n') {
        aoStrPutChar(str, '\n');
    }
}

void AstUnaryOpToString(aoStr *str, char *op, Ast *ast,int depth) {
    aoStrCatPrintf(str, "<unary_expr> %s\n", op);
    _AstToString(str, ast->operand, depth+1);
    AstStringEndStmt(str);
}

void AstBinaryOpToString(aoStr *str, char *op, Ast *ast, int depth) {
    aoStrCatPrintf(str, "<binary_expr> %s\n", op);
    _AstToString(str, ast->left, depth+1);
    AstStringEndStmt(str);
    _AstToString(str, ast->right, depth+1);
    AstStringEndStmt(str);
}

int AstIsClassPointer(AstType *type) {
    return type && type->kind == AST_TYPE_POINTER &&
           type->ptr->kind == AST_TYPE_CLASS;
}

int AstIsUnionPointer(AstType *type) {
    return type && type->kind == AST_TYPE_POINTER &&
           type->ptr->kind == AST_TYPE_UNION;
}

char *AstTypeToString(AstType *type) {
    aoStr *str = aoStrNew();

    switch (type->kind) {
    case AST_TYPE_VOID:
        aoStrCatPrintf(str, "U0");
        return aoStrMove(str);
    
    case AST_TYPE_INT:
        switch (type->size) {
            case 0:
                if (type->issigned) aoStrCatPrintf(str, "I0");
                else                aoStrCatPrintf(str, "U0");
                break;
            case 1:
                if (type->issigned) aoStrCatPrintf(str, "I8");
                else                aoStrCatPrintf(str, "U8");
                break;
            case 2:
                if (type->issigned) aoStrCatPrintf(str, "I16");
                else                aoStrCatPrintf(str, "U16");
                break;
            case 4:
                if (type->issigned) aoStrCatPrintf(str, "I32");
                else                aoStrCatPrintf(str, "U32");
                break;
            case 8:
                if (type->issigned) aoStrCatPrintf(str, "I64");
                else                aoStrCatPrintf(str, "U64");
                break;
            default: loggerPanic("Unknown integer size: %d\n", type->size);

        }
        return aoStrMove(str);
    
    case AST_TYPE_CHAR:
        if (type->issigned) aoStrCatPrintf(str, "I8");
        else                aoStrCatPrintf(str, "U8");
        return aoStrMove(str);

    case AST_TYPE_FLOAT:
        aoStrCatPrintf(str, "F64");
        return aoStrMove(str);

    case AST_TYPE_POINTER: {
        aoStrCatPrintf(str, "%s*", AstTypeToString(type->ptr));
        return aoStrMove(str);
    }

    case AST_TYPE_ARRAY: {
        aoStrCatPrintf(str, "array %s[%d]", AstTypeToString(type->ptr), type->size);
        return aoStrMove(str);
    }

    case AST_TYPE_UNION:
    case AST_TYPE_CLASS: {
        /* class and union are very similar, for now they can live together */
        char *clsname;
        if (type->clsname) {
            clsname = type->clsname->data;
        } else {
            clsname = "anonymous";
        }

        aoStrCatPrintf(str, "%s",clsname);
        return aoStrMove(str);
    }

    case AST_TYPE_FUNC: {
        aoStrCatPrintf(str, "<fn> %s", AstTypeToString(type->rettype));
        return aoStrMove(str);
    }

    case AST_TYPE_AUTO:
        aoStrCatPrintf(str, "auto");
        return aoStrMove(str);

    default:
        loggerPanic("Unknown type: %d\n", type->kind);
    }
}

char *AstTypeToColorString(AstType *type) {
    char *str = AstTypeToString(type);
    if (!isatty(STDOUT_FILENO)) {
        return str;
    }
    aoStr *buf = aoStrNew();
    aoStrCatPrintf(buf,"\033[0;32m%s\033[0m",str);
    free(str);
    return aoStrMove(buf);
}

char *AstFunctionNameToString(AstType *rettype, char *fname, int len) {
    aoStr *str = aoStrNew();
    char *tmp = AstTypeToColorString(rettype);
    aoStrCatPrintf(str,"%s %.*s()",tmp,len,fname);
    free(tmp);
    return aoStrMove(str);
}

char *AstParamTypesToString(List *paramtypes) {
    aoStr *str = aoStrNew();
    char *tmp;
    AstType *param;
    if (ListEmpty(paramtypes)) {
        tmp = AstTypeToColorString(ast_void_type);
        aoStrCatPrintf(str,"%s",tmp);
        free(tmp);
    } else {
        ListForEach(paramtypes) {
            param = it->value;
            if (param->kind == AST_VAR_ARGS) {
                if (it->next != paramtypes) aoStrCatPrintf(str,"..., ");
                else                        aoStrCatPrintf(str,"..."); 
            } else {
                tmp = AstTypeToString(param);
                if (it->next != paramtypes) aoStrCatPrintf(str,"%s, ",tmp);
                else                        aoStrCatPrintf(str,"%s",tmp); 
                free(tmp);
            }
        }
    }
    return aoStrMove(str);
}

static char *AstParamsToString(List *params) {
    aoStr *str = aoStrNew();
    char *tmp;
    Ast *param;
    if (ListEmpty(params)) {
        tmp = AstTypeToColorString(ast_void_type);
        aoStrCatPrintf(str,"%s",tmp);
        free(tmp);
    } else {
        ListForEach(params) {
            param = it->value;
            if (param->kind == AST_VAR_ARGS) {
                if (it->next != params) aoStrCatPrintf(str,"..., ");
                else                    aoStrCatPrintf(str,"..."); 
            } else {
                tmp = AstTypeToString(param->type);
                if (it->next != params) aoStrCatPrintf(str,"%s, ",tmp);
                else                    aoStrCatPrintf(str,"%s",tmp); 
                free(tmp);
            }
        }
    }
    return aoStrMove(str);
}

static char *AstFunctionToStringInternal(Ast *func, AstType *type) {
    aoStr *str = aoStrNew();
    char *tmp,*strparams;

    tmp = AstTypeToColorString(type);
    aoStrCatPrintf(str,"%s %s",tmp,func->fname->data);
    free(tmp);

    switch (func->kind) {
        case AST_FUNCALL:
        case AST_FUNPTR_CALL:
        case AST_ASM_FUNCALL:
            strparams = AstParamsToString(func->args);
            break;

        case AST_FUNC:
            strparams = AstParamsToString(func->params);
            break;
    }

    aoStrCatPrintf(str,"(%s)",strparams);
    return aoStrMove(str);
}

char *AstFunctionToString(Ast *func) {
    return AstFunctionToStringInternal(func,func->type->rettype);
}

void _AstToString(aoStr *str, Ast *ast, int depth) {
    aoStrCatRepeat(str, "  ", depth);
    if (ast == NULL) {
        aoStrCatLen(str, "(null)", 6);
        return;
    }

    Ast *param;
    List *node;
    aoStr *escaped;
    char *tmp;

    switch(ast->kind) {
        case AST_LITERAL:
            aoStrCatPrintf(str,"<ast_literal> ");
            switch (ast->type->kind) {
            case AST_TYPE_VOID:  aoStrCatPrintf(str, "<U0>"); break;
            case AST_TYPE_INT:   aoStrCatPrintf(str, "<I64> %ld", ast->i64); break;
            case AST_TYPE_CHAR:  {
                char buf[9];
                unsigned long ch = ast->i64;
                buf[0] = ch & 0xFF;
                buf[1] = ((unsigned long)ch) >> 8  & 0xFF;
                buf[2] = ((unsigned long)ch) >> 16 & 0xFF;
                buf[3] = ((unsigned long)ch) >> 24 & 0xFF;
                buf[4] = ((unsigned long)ch) >> 32 & 0xFF;
                buf[5] = ((unsigned long)ch) >> 40 & 0xFF;
                buf[6] = ((unsigned long)ch) >> 48 & 0xFF;
                buf[7] = ((unsigned long)ch) >> 56 & 0xFF;
                buf[8] = '\0';
                aoStrCatPrintf(str, "<CONST_CHAR> '%s'", buf);
                break;
            }
            case AST_TYPE_FLOAT: aoStrCatPrintf(str, "<F64> %g", ast->f64); break;
            default:
                loggerPanic("Unhandled type: %d\n", ast->type->kind);
            }
            break;

        case AST_STRING:
            escaped = aoStrEscapeString(ast->sval);
            aoStrCatPrintf(str, "<string> \"%s\"", escaped->data);
            aoStrRelease(escaped);
            break;
    
    case AST_LVAR:
        tmp = AstTypeToString(ast->type);
        aoStrCatPrintf(str, "<lvar> %s %s\n", tmp,
                ast->lname->data);
        free(tmp);
        break;
    
    case AST_DECL:
        tmp = AstTypeToString(ast->declvar->type);
        if (ast->declvar->kind == AST_FUNPTR) {
            aoStrCatPrintf(str,"<decl> %s %s\n", tmp, ast->declvar->fname->data);
        } else {
            aoStrCatPrintf(str,"<decl> %s %s\n", tmp, ast->declvar->lname->data);
        }
        free(tmp);
        if (ast->declinit) {
            _AstToString(str,ast->declinit,depth+1);
            AstStringEndStmt(str);
        }
        break;

    case AST_GVAR:
        aoStrCatPrintf(str, "<gvar> %s", ast->gname->data);
        break;
    
    case AST_ASM_FUNCALL: {
        tmp = AstFunctionToStringInternal(ast,ast->type);
        aoStrCatPrintf(str, "<asm_function_call> %s\n", 
                    tmp);
            free(tmp);
            depth++;
            node = ast->args->next;
            while (node != ast->args) {
                aoStrCatRepeat(str, "  ", depth);
                aoStrCatPrintf(str, "<asm_function_arg>\n");
                _AstToString(str,(Ast *)node->value,depth+1);
                AstStringEndStmt(str);
                node = node->next;
            }
            break;
        }

        case AST_FUNPTR_CALL: {
            tmp = AstTypeToString(ast->type);
            aoStrCatPrintf(str, "<function_ptr_call*> %s %s\n", 
                    tmp, ast->fname->data);
            depth++;
            free(tmp);
            node = ast->args->next;
            Ast *tmp;
            while (node != ast->args) {
                aoStrCatRepeat(str, "  ", depth);
                aoStrCatPrintf(str, "<function_ptr_arg>\n");
                tmp = node->value;
                if (tmp->kind == AST_TYPE_FUNC) {
                    loggerDebug("%s\n",
                            AstTypeToString(((AstType *)tmp)->rettype));
                }
                _AstToString(str,(Ast *)node->value,depth+1);
                AstStringEndStmt(str);
                node = node->next;
            }
            break;
        }

        case AST_FUNCALL: {
            tmp = AstFunctionToStringInternal(ast,ast->type);
            aoStrCatPrintf(str, "<function_call> %s \n",tmp);
            depth++;
            free(tmp);
            node = ast->args->next;
            Ast *tmp;
            while (node != ast->args) {
                aoStrCatRepeat(str, "  ", depth);
                aoStrCatPrintf(str, "<function_arg>\n");
                tmp = node->value;
                if (tmp->kind == AST_TYPE_FUNC) {
                    loggerDebug("%s\n",
                            AstTypeToString(((AstType *)tmp)->rettype));
                }
                _AstToString(str,(Ast *)node->value,depth+1);
                AstStringEndStmt(str);
                node = node->next;
            }
            break;
        }

        case AST_FUNPTR: {
            tmp = AstTypeToString(ast->type);
            aoStrCatPrintf(str, "<function_ptr*> %s %s\n", tmp,
                    ast->fname->data);
            depth++;
            free(tmp);
            node = ast->params->next;
            while (node != ast->params) {
                aoStrCatRepeat(str, "  ", depth);
                aoStrCatPrintf(str, "<function_ptr_arg>\n");
                _AstToString(str,(Ast *)node->value,depth+1);
                AstStringEndStmt(str);
                node = node->next;
            }
            break;
        }

        case AST_EXTERN_FUNC: {
            tmp = AstTypeToString(ast->type);
            aoStrCatPrintf(str, "<extern_function> %s %s\n", tmp, ast->fname->data);
            free(tmp);
            node = ast->params->next;
            while (node != ast->params) {
                param = node->value;
                aoStrCatRepeat(str, "  ", depth+1);
                aoStrCatPrintf(str, "<extern_function_param>\n");
                _AstToString(str, param, depth+2);
                AstStringEndStmt(str);
                node = node->next;
            }
            AstStringEndStmt(str);
            break;
        }

        case AST_FUN_PROTO: {
            tmp = AstTypeToString(ast->type);
            aoStrCatPrintf(str, "<function_proto> %s %s\n", tmp, ast->fname->data);
            free(tmp);
            node = ast->params->next;
            while (node != ast->params) {
                param = node->value;
                aoStrCatRepeat(str, "  ", depth+1);
                aoStrCatPrintf(str, "<function_proto_param>\n");
                _AstToString(str, param, depth+2);
                AstStringEndStmt(str);
                node = node->next;
            }
            AstStringEndStmt(str);
            break;
        }

        case AST_FUNC: {
            tmp = AstFunctionToString(ast);
            aoStrCatPrintf(str, "<function_def> %s\n", tmp);
            free(tmp);
            node = ast->params->next;
            while (node != ast->params) {
                param = node->value;
                aoStrCatRepeat(str, "  ", depth+1);
                aoStrCatPrintf(str, "<function_param>\n");
                _AstToString(str, param, depth+2);
                AstStringEndStmt(str);
                node = node->next;
            }
            _AstToString(str, ast->body, depth + 1);
            AstStringEndStmt(str);
            break;
        }

        case AST_ASM_FUNC_BIND: {
            tmp = AstTypeToString(ast->type);
            aoStrCatPrintf(str, "<asm_function_bind> %s %s %s\n", tmp,
                    ast->asmfname->data,
                    ast->fname->data);
            free(tmp);
            node = ast->params->next;
            while (node != ast->params) {
                param = node->value;
                aoStrCatRepeat(str, "  ", depth+1);
                aoStrCatPrintf(str, "<asm_function_param>\n");
                _AstToString(str, param, depth+2);
                AstStringEndStmt(str);
                node = node->next;
            }
            AstStringEndStmt(str);
            break;
        }

        case AST_ASM_STMT: {
            aoStrCatPrintf(str, "<asm_block>\n");
            break;
        }

        case AST_ARRAY_INIT:
            node = ast->arrayinit->next;
            aoStrCatPrintf(str, "<array_initaliser>\n");
            while (node != ast->arrayinit) {
                _AstToString(str, node->value, depth + 1);
                if (node->next != ast->arrayinit) {
                    AstStringEndStmt(str);
                }
                node = node->next;
            }
            break;

        case AST_IF:
            aoStrCatPrintf(str, "<if_cond>\n");
            _AstToString(str,ast->cond,depth+1);
            AstStringEndStmt(str);
            _AstToString(str,ast->then,depth+1);
            if (ast->els) {
                aoStrCatRepeat(str, "  ", depth);
                aoStrCatPrintf(str, "<else_cond>\n");
                _AstToString(str,ast->els,depth+1);
                AstStringEndStmt(str);
            }
            break;

        case AST_DO_WHILE:

            aoStrCatPrintf(str, "do");
            aoStrCatRepeat(str, "  ", depth+1);
            _AstToString(str, ast->whilebody, depth+2);

            aoStrCatPrintf(str, "<while_cond>\n"); 
            _AstToString(str, ast->whilecond, depth+2);
            AstStringEndStmt(str);
            AstStringEndStmt(str);
            break;

        case AST_WHILE:
            aoStrCatPrintf(str, "<while_cond>\n"); 
            _AstToString(str, ast->whilecond, depth+2);
            AstStringEndStmt(str);

            aoStrCatRepeat(str, "  ", depth+1);
            aoStrCatPrintf(str, "<while_body>\n"); 
            _AstToString(str, ast->whilebody, depth+2);
            AstStringEndStmt(str);
            break;

        case AST_FOR:
            aoStrCatPrintf(str, "<for_expr>\n");
            aoStrCatRepeat(str, "  ", depth+1);
            aoStrCatPrintf(str, "<for_init>\n"); 
            _AstToString(str, ast->forinit, depth+2);
            AstStringEndStmt(str);

            aoStrCatRepeat(str, "  ", depth+1);
            aoStrCatPrintf(str, "<for_cond>\n"); 
            _AstToString(str, ast->forcond, depth+2);
            AstStringEndStmt(str);

            aoStrCatRepeat(str, "  ", depth+1);
            aoStrCatPrintf(str, "<for_step>\n"); 
            _AstToString(str, ast->forstep, depth+2);
            AstStringEndStmt(str);

            aoStrCatRepeat(str, "  ", depth+1);
            aoStrCatPrintf(str, "<for_body>\n"); 
            _AstToString(str, ast->forbody, depth+2);
            AstStringEndStmt(str);
            break;

        case AST_RETURN:
            aoStrCatPrintf(str, "<return>\n");
            _AstToString(str, ast->retval, depth+1);
            AstStringEndStmt(str);
            break;

        case AST_VAR_ARGS:
            aoStrCatPrintf(str, "<var_args>...\n");
            break;

        case AST_COMPOUND_STMT: {
            List *it = ast->stms->next;
            Ast *next;
            aoStrCatPrintf(str, "<compound_expr>\n");
            while (it != ast->stms) {
                next = it->value;
                _AstToString(str, next, depth+1);
                AstStringEndStmt(str);
                it = it->next;
            }
            AstStringEndStmt(str);
            break;
        }
        
        case AST_CLASS_REF: {
            Ast *ast_tmp;
            char *field_names[30];
            int field_name_count = 0;
            aoStrCatPrintf(str, "<class_ref>\n");
            AstType *field_type = DictGet(ast->cls->type->fields, ast->field);

            /* We only really want to print at the data type we are looking 
             * at, not the whole class */
            if (field_type && ast->cls->kind == AST_DEREF) {
                aoStrCatRepeat(str, "  ", depth+1);

                if (!AstIsClassPointer(field_type)) {
                    tmp = AstTypeToColorString(field_type);
                    aoStrCatPrintf(str, "%s ", tmp);
                    free(tmp);
                } else {
                    tmp = AstTypeToColorString(field_type);
                    aoStrCatPrintf(str, "<class> %s ",tmp);
                    free(tmp);
                }

                /* Find the name of the variable that contains this reference */
                ast_tmp = ast;
                while (ast_tmp->kind == AST_DEREF ||
                        ast_tmp->kind == AST_CLASS_REF) {
                    field_names[field_name_count++] = ast_tmp->field;
                    if (ast_tmp->kind != AST_DEREF &&
                            ast_tmp->kind != AST_CLASS_REF) {
                        break;
                    }
                    ast_tmp = ast_tmp->cls->operand;
                }

                if (ast_tmp->kind == AST_LVAR) {
                    aoStrCatPrintf(str, "%s->",ast_tmp->lname->data);
                }
                for (int i = 0; i < field_name_count; ++i) {
                    if (i + 1 == field_name_count) {
                        aoStrCatPrintf(str, "%s",field_names[i]);
                    } else {
                        aoStrCatPrintf(str, "%s->",field_names[i]);
                    }
                }
            } else {
                loggerWarning("printing whole class!\n");
                _AstToString(str,ast->cls,depth+1);
                aoStrCatPrintf(str, "->");
                aoStrCatPrintf(str, "%s", ast->field, ast->type->offset);
            }
            AstStringEndStmt(str);
            break;
        }

        case AST_GOTO:
            aoStrCatPrintf(str, "<goto> %s\n", ast->slabel->data);
            break;

        /* XXX: fix labels */
        case AST_LABEL:
            if (ast->slabel) {
                aoStrCatPrintf(str, "<label> %s:\n", ast->slabel->data);
            } else {
                aoStrCatPrintf(str, "<label> %s:\n", ast->sval->data);
            }
            break;

        case AST_ADDR:
            aoStrCatPrintf(str, "<addr>\n");
            _AstToString(str,ast->operand,depth+1);
            AstStringEndStmt(str);
            break;

        case AST_DEREF:
            aoStrCatPrintf(str, "<deref>\n");
            _AstToString(str,ast->operand,depth+1);
            //AstStringEndStmt(str);
            break;

        case AST_CAST:
            aoStrCatPrintf(str, "<cast> %s %s -> %s\n",
                    AstTypeToString(ast->operand->type),
                    AstLValueToString(ast->operand),
                    AstTypeToString(ast->type));
            break;

        case AST_JUMP:
            aoStrCatPrintf(str, "<jump> %s\n", ast->jump_label->data);
            break;

        case AST_CASE: {
            if (ast->case_begin == ast->case_end) {
                aoStrCatPrintf(str, "<case> %d:\n", ast->case_begin);
            } else {
                aoStrCatPrintf(str, "<case> %d...%d:\n",
                        ast->case_begin, ast->case_end);
            }
            break;
        }

        case AST_BREAK:
            aoStrCatPrintf(str, "<break>\n");
            break;

        case AST_CONTINUE:
            aoStrCatPrintf(str, "<continue>\n");
            break;

        case AST_DEFAULT_PARAM: {
            aoStrCatPrintf(str, "<default_param>\n");
            _AstToString(str,ast->declvar,depth+1);
            if (ast->declvar) {
                _AstToString(str,ast->declinit,depth+1);
            }
            break;
        }
        case TK_PRE_PLUS_PLUS:
        case TK_PLUS_PLUS:   
        case TK_PRE_MINUS_MINUS:
        case TK_MINUS_MINUS:
            AstUnaryOpToString(str, AstKindToString(ast->kind),ast,depth);
            break;

        default: {
            AstBinaryOpToString(str,AstKindToString(ast->kind),ast,depth);
            AstStringEndStmt(str);
            break;
        }
    }
}

/* String representation of the kind of ast we are looking at */
char *AstKindToString(int kind) {
    switch (kind) {
    /* KIND FOR AST TYPE */
    case AST_TYPE_VOID:  return "AST_TYPE_VOID";
    case AST_TYPE_INT:   return "AST_TYPE_INT";
    case AST_TYPE_FLOAT: return "AST_TYPE_FLOAT";
    case AST_TYPE_CHAR:  return "AST_TYPE_CHAR";
    case AST_TYPE_ARRAY: return "AST_TYPE_ARRAY";
    case AST_TYPE_POINTER: return "->AST_TYPE_POINTER";
    case AST_TYPE_FUNC:  return "AST_TYPE_FUNC";
    case AST_TYPE_CLASS: return "AST_TYPE_CLASS";
    case AST_TYPE_UNION: return "AST_TYPE_UNION";
    case AST_TYPE_AUTO:  return "AST_TYPE_AUTO";

    case AST_GVAR:       return "AST_GVAR";
    case AST_DEREF:      return "AST_DEREF";
    case AST_ADDR:       return "AST_ADDR";
    case AST_LVAR:       return "AST_LVAR";
    case AST_FUNC:       return "AST_FUNC";
    case AST_FUNPTR:     return "AST_FUNPTR";
    case AST_EXTERN_FUNC: return "AST_EXTERN_FUNC";
    case AST_DECL:       return "AST_DECL";
    case AST_STRING:     return "AST_STRING";
    case AST_FUNCALL:    return "AST_FUNCALL";
    case AST_LITERAL:    return "AST_LITERAL";
    case AST_ARRAY_INIT: return "AST_ARRAY_INIT";
    case AST_IF:         return "AST_IF";
    case AST_FOR:        return "AST_FOR";
    case AST_GOTO:       return "AST_GOTO";
    case AST_LABEL:      return "AST_LABEL";
    case AST_RETURN:     return "AST_RETURN";
    case AST_COMPOUND_STMT: return "AST_COMPOUND_STMT";
    case AST_CLASS_REF:  return "AST_CLASS_REF";
    case AST_DEFAULT_PARAM: return "AST_DEFAULT_PARAM";
    case AST_VAR_ARGS:   return "AST_VAR_ARGS";
    case AST_CAST:       return "AST_CAST";
    case AST_JUMP:       return "AST_JUMP";
    case AST_CASE:       return "AST_CASE";
    case AST_ASM_FUNC_BIND: return "AST_ASM_FUNC_BIND";


    case TK_AND_AND:         return "&&";
    case TK_OR_OR:           return "||";
    case TK_SHL:             return "<<";
    case TK_SHR:             return ">>";
    case TK_PLUS_PLUS:       return "++";
    case TK_PRE_PLUS_PLUS:   return "<p>++";
    case TK_PRE_MINUS_MINUS: return "<p>--";
    case TK_MINUS_MINUS:     return "--";
    case TK_EQU_EQU:         return "==";
    case TK_NOT_EQU:         return "!=";
    case TK_LESS_EQU:        return "<=";
    case TK_GREATER_EQU:     return ">=";
    case TK_DIV_EQU:         return "/=";
    case TK_MUL_EQU:         return "*=";
    case TK_MOD_EQU:         return "%=";
    case TK_ADD_EQU:         return "+=";
    case TK_SUB_EQU:         return "-=";
    case TK_SHL_EQU:         return "<<=";
    case TK_SHR_EQU:         return ">>=";
    case TK_AND_EQU:         return "&=";
    case TK_OR_EQU:          return "|=";
    case TK_XOR_EQU:         return "^=";
    case '+':
    case AST_OP_ADD:     return "+";
    case '-':            return "-";
    case '*':            return "*";
    case '~':            return "~";
    case '<':            return "<";
    case '>':            return ">";
    case '/':            return "/";
    case '&':            return "&";
    case '|':            return "|";
    case '=':            return "=";
    case '!':            return "!";
    case '%':            return "%";
    case '^':            return "^";
    default:
        loggerPanic("Cannot find kind: %d\n", kind);
    }
}

int AstIsRangeOperator(long op) {
    switch (op) {
    case TK_GREATER_EQU:
    case TK_LESS_EQU:
    case '<':
    case '>':
        return 1;
    default:
        return 0;
    }
}

char *_AstToStringRec(Ast *ast, int depth) {
    aoStr *str = aoStrNew();
    _AstToString(str,ast, depth);
    return aoStrMove(str);
}

/* Convert an Ast to a string */
char *AstToString(Ast *ast) {
    return _AstToStringRec(ast,0);
}

static void _AstLValueToString(aoStr *str, Ast *ast);

void AstUnaryArgToString(aoStr *str, char *op, Ast *ast) {
    aoStrCatPrintf(str, "%s", op);
    _AstLValueToString(str, ast->operand);
}

void AstBinaryArgToString(aoStr *str, char *op, Ast *ast) {
    if (!strncmp(op,"-",1) || !strncmp(op,"~",1)) {
        aoStrCatPrintf(str, " %s", op);
        _AstLValueToString(str, ast->left);
    } else {
        _AstLValueToString(str, ast->left);
        aoStrCatPrintf(str, " %s ", op);
        _AstLValueToString(str, ast->right);
    }
}

/* This can only be used for lvalues */
static void _AstLValueToString(aoStr *str, Ast *ast) {
    if (ast == NULL) {
        aoStrCatLen(str, "(null)", 6);
        return;
    }
    switch(ast->kind) {
        case AST_LITERAL:
            switch (ast->type->kind) {
            case AST_TYPE_VOID:  aoStrCatPrintf(str, "void"); break;
            case AST_TYPE_INT:   aoStrCatPrintf(str, "%ld", ast->i64); break;
            case AST_TYPE_CHAR:  {
                char buf[9];
                unsigned long ch = ast->i64;
                buf[0] = ch & 0xFF;
                buf[1] = ((unsigned long)ch) >> 8  & 0xFF;
                buf[2] = ((unsigned long)ch) >> 16 & 0xFF;
                buf[3] = ((unsigned long)ch) >> 24 & 0xFF;
                buf[4] = ((unsigned long)ch) >> 32 & 0xFF;
                buf[5] = ((unsigned long)ch) >> 40 & 0xFF;
                buf[6] = ((unsigned long)ch) >> 48 & 0xFF;
                buf[7] = ((unsigned long)ch) >> 56 & 0xFF;
                buf[8] = '\0';
                aoStrCatPrintf(str,"'%s'",buf);
                break;
            }
            case AST_TYPE_FLOAT: aoStrCatPrintf(str, "%g", ast->f64); break;
            default:
                loggerPanic("Unhandled type: %d\n", ast->type->kind);
            }
            break;

        case AST_STRING: {
            aoStr *escaped = aoStrEscapeString(ast->sval);
            aoStrCatPrintf(str, "\"%s\"", escaped->data);
            aoStrRelease(escaped);
            break;
        }
        
        case AST_LVAR:
            aoStrCatPrintf(str, "%s",ast->lname->data);
            break;
        
        case AST_DECL:
            if (ast->declvar->kind == AST_FUNPTR) {
                aoStrCatPrintf(str,"%s", ast->declvar->fname->data);
            } else {
                aoStrCatPrintf(str,"%s",ast->declvar->lname->data);
            }
            if (ast->declinit) {
                _AstLValueToString(str,ast->declinit);
            }
            break;

        case AST_GVAR:
            aoStrCatPrintf(str, "%s", ast->gname->data);
            break;
        
        case AST_FUNCALL:
        case AST_FUNPTR_CALL:
        case AST_ASM_FUNCALL: {
            List *node = ast->args->next;
            aoStr *internal = aoStrAlloc(256);
            while (node != ast->args) {
                Ast *val = cast(Ast *, node->value);
                _AstLValueToString(internal,val);
                if (node->next != ast->args) {
                    aoStrCatPrintf(internal,",");
                }
                node = node->next;
            }
            aoStr *escaped = aoStrEscapeString(internal);
            aoStrCatPrintf(str, "%s(%s)",ast->fname->data,escaped->data);
            aoStrRelease(internal);
            aoStrRelease(escaped);
            break;
        }

        case AST_FUNC:
        case AST_FUN_PROTO:
        case AST_EXTERN_FUNC:
        case AST_FUNPTR: {
            aoStrCatPrintf(str, "%s", ast->fname->data);
            break;
        }

        case AST_ASM_FUNC_BIND: {
            aoStrCatPrintf(str, "%s => %s",
                    ast->asmfname->data,
                    ast->fname->data);
            break;
        }

        case AST_VAR_ARGS:
            aoStrCatPrintf(str, "...\n");
            break;

        case AST_CLASS_REF: {
            Ast *ast_tmp;
            char *field_names[30];
            int field_name_count = 0;
            AstType *field_type = DictGet(ast->cls->type->fields, ast->field);

            /* We only really want to print at the data type we are looking 
             * at, not the whole class */
            if (field_type && ast->cls->kind == AST_DEREF) {
                /* Find the name of the variable that contains this reference */
                ast_tmp = ast;
                while (ast_tmp->kind == AST_DEREF ||
                        ast_tmp->kind == AST_CLASS_REF) {
                    field_names[field_name_count++] = ast_tmp->field;
                    if (ast_tmp->kind != AST_DEREF &&
                            ast_tmp->kind != AST_CLASS_REF) {
                        break;
                    }
                    ast_tmp = ast_tmp->cls->operand;
                }

                if (ast_tmp->kind == AST_LVAR) {
                    aoStrCatPrintf(str, "%s->",ast_tmp->lname->data);
                }
                for (int i = 0; i < field_name_count; ++i) {
                    if (i + 1 == field_name_count) {
                        aoStrCatPrintf(str, "%s",field_names[i]);
                    } else {
                        aoStrCatPrintf(str, "%s->",field_names[i]);
                    }
                }
            }
            break;
        }

        case AST_ADDR:
            aoStrCatPrintf(str, "&");
            _AstLValueToString(str,ast->operand);
            break;

        case AST_DEREF:
            aoStrCatPrintf(str, "*");
            _AstLValueToString(str,ast->operand);
            break;

        case AST_CAST:
            aoStrCatPrintf(str, "cast ");
            _AstLValueToString(str,ast->operand);
            break;

        case TK_PRE_PLUS_PLUS:
        case TK_PLUS_PLUS:   
        case TK_PRE_MINUS_MINUS:
        case TK_MINUS_MINUS:
            AstUnaryArgToString(str, lexemePunctToString(ast->kind),ast);
            break;

        default: {
            AstBinaryArgToString(str,lexemePunctToString(ast->kind),ast);
            break;
        }
    }
}

char *AstLValueToString(Ast *ast) {
    aoStr *str = aoStrNew();
    _AstLValueToString(str,ast);
    return aoStrMove(str);
}

/* Just print out one */
void AstPrint(Ast *ast) {
    char *str = AstToString(ast);
    printf("%s\n", str);
    //printf("%s\n", AstKindToString(ast->kind));
    free(str);
}

void AstTypePrint(AstType *type) {
    char *str = AstTypeToString(type);
    printf("%s\n", str);
    free(str);
}

void AstKindPrint(int kind) {
    char *str = AstKindToString(kind);
    printf("%s\n",str);
}

void AstReleaseList(List *ast_list) {
    ListRelease(ast_list,(void(*))AstRelease);
}

/* Free an Ast */
void AstRelease(Ast *ast) {
    if (!ast) {
        return;
    }
    switch(ast->kind) {
        case '|':case '&':
        case '!':case '~':
        case AST_ADDR:
        case AST_DEREF:
        case TK_PLUS_PLUS:
        case TK_MINUS_MINUS:
            AstFreeUnaryOperator(ast);
            break;
        case AST_LITERAL: AstFreeLiteral(ast); break;
        case AST_GVAR: AstFreeGVar(ast); break;
        case AST_GOTO: AstFreeGoto(ast); break;
        case AST_LABEL: AstFreeLabel(ast); break;
        case AST_JUMP: AstFreeJump(ast); break;
        case AST_LVAR: AstFreeLVar(ast); break;
        case AST_EXTERN_FUNC:
        case AST_FUN_PROTO:
        case AST_FUNC: AstFreeFunction(ast); break;
        case AST_DECL: AstFreeDecl(ast); break;
        case AST_STRING: AstFreeString(ast); break;
        case AST_FUNCALL: AstFreeFunctionCall(ast); break;
        case AST_ARRAY_INIT: AstFreeArrayInit(ast); break;
        case AST_IF: AstFreeIf(ast); break;
        case AST_FOR: AstFreeFor(ast); break;
        case AST_RETURN: AstFreeReturn(ast); break;
        case AST_DO_WHILE: 
        case AST_WHILE: AstFreeWhile(ast); break;
        case AST_CLASS_REF: AstFreeClassRef(ast); break;
        case AST_COMPOUND_STMT: AstFreeCompountStatement(ast); break;
        case AST_ASM_STMT: AstFreeAsmBlock(ast); break;
        case AST_ASM_FUNC_BIND: AstFreeAsmFunctionBind(ast); break;
        case AST_ASM_FUNCALL: AstFreeAsmFunctionCall(ast); break;
        case AST_FUNPTR: AstFreeFunctionPtr(ast); break;
        case AST_FUNPTR_CALL: AstFreeFunctionPtrCall(ast); break;
        case AST_BREAK: AstFreeBreak(ast); break;
        case AST_CONTINUE: AstFreeContinue(ast); break;
        case AST_DEFAULT_PARAM: AstFreeFunctionDefaultParam(ast); break;
        case AST_VAR_ARGS: AstFreeVarArgs(ast); break;
        case AST_ASM_FUNCDEF: AstFreeAsmFunctionDef(ast); break;
        case AST_CAST: AstFreeCast(ast); break;
        case AST_CASE: AstFreeCase(ast); break;
        case TK_AND_AND:        
        case TK_OR_OR:  
        case TK_EQU_EQU:    
        case TK_NOT_EQU:    
        case TK_LESS_EQU:   
        case TK_GREATER_EQU:
        case TK_SHL:        
        case TK_SHR:        
        case TK_ARROW:      
        case '=': case '>': case '<': case '/': case '+':
        case '*': case '-':
                AstFreeBinaryOp(ast);
                break;
    }
}

AstArray *AstArrayNew(int capacity) {
    AstArray *ast_array = cast(AstArray *,malloc(sizeof(AstArray)));
    ast_array->capacity = capacity;
    ast_array->count = 0;
    ast_array->entries = cast(Ast **,malloc(sizeof(Ast *)*capacity));
    return ast_array;
}

void AstArrayPush(AstArray *ast_array, Ast *ast) {
    if (ast_array->count + 1 >= ast_array->capacity) {
        int new_capacity = ast_array->capacity + 20;
        Ast **new_entries = cast(Ast **,realloc(ast_array->entries,
                    sizeof(Ast **)*new_capacity));
        if (new_entries == NULL) {
            loggerPanic("Failed to allocate memory for AstArray\n");
        }
        ast_array->entries = new_entries;
        ast_array->capacity = new_capacity;
    }
    ast_array->entries[ast_array->count++] = ast;
}

void AstArrayRelease(AstArray *ast_array) {
    if (ast_array) {
        free(ast_array->entries);
        free(ast_array);
    }
}
