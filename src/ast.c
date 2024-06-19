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

void _astToString(aoStr *str, Ast *ast, int depth);
char *_astToStringRec(Ast *ast, int depth);

Ast *astNew(void) {
    Ast *ast; 
    if ((ast = calloc(1,sizeof(Ast))) == NULL) {
        loggerPanic("OOM when allocating AST\n");
    }
    return ast;
}

AstType *astTypeNew(void) {
    AstType *type;
    if ((type = calloc(1,sizeof(AstType))) == NULL) {
        loggerPanic("OOM when allocating AstType\n");
    }
    return type;
}

AstType *astTypeCopy(AstType *type) {
    AstType *copy = astTypeNew();
    memcpy(copy,type,sizeof(AstType));
    return copy;
}

Ast *astUnaryOperator(AstType *type, long kind, Ast *operand) {
    Ast *ast = astNew();
    ast->type = type;
    ast->kind = kind;
    ast->operand = operand;
    return ast;
}

static void astFreeUnaryOperator(Ast *ast) {
    free(ast);
}

Ast *astBinaryOp(long operation, Ast *left, Ast *right) {
    Ast *ast = astNew();
    ast->type = astGetResultType(operation,left->type,right->type);  
    if (ast->type == NULL) {
        loggerPanic("Incompatiable operands: %s: "ESC_GREEN"%s %s"ESC_RESET"\n",
                astKindToString(operation),
                astToString(left),
                astToString(right));
    }
    ast->kind = operation;
    if (operation != '=' &&
            astConvertArray(left->type)->kind != AST_TYPE_POINTER &&
            astConvertArray(right->type)->kind == AST_TYPE_POINTER) {
        ast->left = right;
        ast->right = left;
    } else {
        ast->left = left;
        ast->right = right;
    }
    return ast;
}

static void astFreeBinaryOp(Ast *ast) {
    astRelease(ast->left);
    astRelease(ast->right);
    free(ast);
}

Ast *astI64Type(long long val) {
    Ast *ast = astNew();
    ast->kind = AST_LITERAL;
    ast->type = ast_int_type;
    ast->i64 = val;
    return ast;
}

Ast *astCharType(long ch) {
    Ast *ast = astNew();
    ast->kind = AST_LITERAL;
    ast->type = ast_u8_type;
    ast->i64 = ch;
    return ast;
}

Ast *astF64Type(double val) {
    Ast *ast = astNew();
    ast->kind = AST_LITERAL;
    ast->type = ast_float_type;
    ast->f64 = val;
    ast->f64_label = NULL;
    return ast;
}
static void astFreeLiteral(Ast *ast) {
    free(ast);
}

static int label_sequence = 0;
aoStr *astMakeLabel(void) {
    aoStr *s = aoStrNew();
    aoStrCatPrintf(s, ".L%d", label_sequence++);
    return s;
}

static int tmp_name_sequence = 0;
aoStr *astMakeTmpName(void) {
    aoStr *s = aoStrNew();
    aoStrCatPrintf(s, ".T%d", tmp_name_sequence++);
    return s;
}

Ast *astLVar(AstType *type, char *name, int len) {
    Ast *ast = astNew();
    ast->kind = AST_LVAR;
    ast->type = type;
    ast->lname = aoStrDupRaw(name, len);
    return ast;
}
static void astFreeLVar(Ast *ast) {
    aoStrRelease(ast->lname);
    free(ast);
}

Ast *astGVar(AstType *type, char *name, int len, int is_static) {
    Ast *ast = astNew();
    ast->kind = AST_GVAR;
    ast->type = type;
    ast->gname = aoStrDupRaw(name, len);
    ast->is_static = is_static;
    ast->glabel = is_static ? astMakeLabel() : ast->gname;
    return ast;
}
static void astFreeGVar(Ast *ast) {
    aoStrRelease(ast->gname);
    ast->gname = NULL;
    aoStrRelease(ast->glabel);
    free(ast);
}

Ast *astString(char *str, int len) {
    Ast *ast = astNew();
    ast->kind = AST_STRING;
    ast->sval = aoStrDupRaw(str,len);
    ast->type = astMakeArrayType(ast_u8_type, ast->sval->len + 1);
    ast->slabel = astMakeLabel();
    return ast;
}
static void astFreeString(Ast *ast) {
    aoStrRelease(ast->sval);
    aoStrRelease(ast->slabel);
    free(ast);
}

Ast *astFunctionCall(AstType *type, char *fname, int len, List *argv, List *paramtypes) {
    Ast *ast = astNew();
    ast->type = type;
    ast->kind = AST_FUNCALL;
    ast->fname = aoStrDupRaw(fname, len);
    ast->args = argv;
    ast->paramtypes = paramtypes;
    return ast;
}
static void astFreeFunctionCall(Ast *ast) {
    aoStrRelease(ast->fname);
    astReleaseList(ast->args);
    astReleaseList(ast->paramtypes);
    free(ast);
}

Ast *astFunctionDefaultParam(Ast *var, Ast *init) {
    Ast *ast = astNew();
    ast->kind = AST_DEFAULT_PARAM;
    ast->type = var->type;
    ast->declvar = var;
    ast->declinit = init;
    return ast;
}
static void astFreeFunctionDefaultParam(Ast *ast) {
    astRelease(ast->declvar);
    astRelease(ast->declinit);
    free(ast);
}

Ast *astFunctionPtrCall(AstType *type, char *fname, int len,
     List *argv, List *paramtypes, Ast *ref)
{
    Ast *ast = astNew();
    ast->type = type;
    ast->kind = AST_FUNPTR_CALL;
    ast->fname = aoStrDupRaw(fname, len);
    ast->args = argv;
    ast->ref = ref;
    ast->paramtypes = paramtypes;
    return ast;
}
static void astFreeFunctionPtrCall(Ast *ast) {
    /* paramtypes is a shared pointer */
    astReleaseList(ast->args);
    aoStrRelease(ast->fname);
    free(ast);
}

Ast *astFunctionPtr(AstType *type, char *fname, int len, List *params) {
    Ast *ast = astNew();
    ast->type = type;
    ast->kind = AST_FUNPTR;
    ast->fname = aoStrDupRaw(fname, len);
    ast->params = params;
    return ast;
}
static void astFreeFunctionPtr(Ast *ast) {
    astReleaseList(ast->params);
    aoStrRelease(ast->fname);
    free(ast);
}

Ast *astFunction(AstType *type, char *fname, int len, List *params, Ast *body,
        List *locals, int has_var_args)
{
    Ast *ast = astNew();
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
static void astFreeFunction(Ast *ast) {
    aoStrRelease(ast->fname);
    astRelease(ast->body);
    astReleaseList(ast->params);
    astReleaseList(ast->locals);
    free(ast);
}

Ast *astDecl(Ast *var, Ast *init) {
    Ast *ast = astNew();
    ast->kind = AST_DECL;
    ast->type = NULL;
    ast->declvar = var;
    ast->declinit = init;
    return ast;
}
static void astFreeDecl(Ast *ast) {
    astRelease(ast->declvar);
    astRelease(ast->declinit);
    free(ast);
}

Ast *astArrayInit(List *init) {
    Ast *ast = astNew();
    ast->type = NULL;
    ast->kind = AST_ARRAY_INIT;
    ast->arrayinit = init;
    return ast;
}
static void astFreeArrayInit(Ast *ast) {
    astReleaseList(ast->arrayinit);
    free(ast);
}

Ast *astIf(Ast *cond, Ast *then, Ast *els) {
    Ast *ast = astNew();
    ast->type = NULL;
    ast->kind = AST_IF;
    ast->cond = cond;
    ast->then = then;
    ast->els = els;
    return ast;
}
static void astFreeIf(Ast *ast) {
    astRelease(ast->cond);
    astRelease(ast->then);
    astRelease(ast->els);
    free(ast);
}

Ast *astCase(aoStr *case_label, long case_begin, long case_end) {
    Ast *ast = astNew();
    ast->type = ast_int_type;
    ast->kind = AST_CASE;
    ast->case_begin = case_begin;
    ast->case_end = case_end;
    ast->case_label = case_label;
    return ast;
}
static void astFreeCase(Ast *ast) {
    aoStrRelease(ast->case_label);
    free(ast);
}

Ast *astDest(char *name, int len) {
    Ast *ast = astNew();
    ast->kind = AST_LABEL;
    ast->sval= aoStrDupRaw(name,len);
    ast->slabel = NULL;
    return ast;
}

Ast *astJump(char *name, int len) {
    Ast *ast = astNew();
    ast->kind = AST_JUMP;
    ast->jump_label = aoStrDupRaw(name,len);
    return ast;
}
static void astFreeJump(Ast *ast) {
    aoStrRelease(ast->jump_label);
    free(ast);
}


Ast *astFor(Ast *init, Ast *cond, Ast *step, Ast *body, aoStr *for_begin,
        aoStr *for_middle, aoStr *for_end)
{
    Ast *ast = astNew();
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
static void astFreeFor(Ast *ast) {
    astRelease(ast->forinit);
    astRelease(ast->forcond);
    astRelease(ast->forstep);
    astRelease(ast->forbody);
    aoStrRelease(ast->for_begin);
    aoStrRelease(ast->for_middle);
    aoStrRelease(ast->for_end);
    free(ast);
}

Ast *astDoWhile(Ast *whilecond, Ast *whilebody, aoStr *while_begin, 
        aoStr *while_end)
{
    Ast *ast = astNew();
    ast->type = NULL;
    ast->kind = AST_DO_WHILE;
    ast->whilebody = whilebody;
    ast->whilecond = whilecond;
    ast->while_begin = while_begin;
    ast->while_end = while_end;
    return ast;
}

Ast *astWhile(Ast *whilecond, Ast *whilebody, aoStr *while_begin, 
        aoStr *while_end)
{
    Ast *ast = astNew();
    ast->type = NULL;
    ast->kind = AST_WHILE;
    ast->whilecond = whilecond;
    ast->whilebody = whilebody;
    ast->while_begin = while_begin;
    ast->while_end = while_end;
    return ast;
}
static void astFreeWhile(Ast *ast) {
    astRelease(ast->whilecond);
    astRelease(ast->whilebody);
    aoStrRelease(ast->while_begin);
    aoStrRelease(ast->while_end);
    free(ast);
}

/* Duplicates the label */
Ast *astContinue(aoStr *continue_label) {
    Ast *ast = astNew();
    ast->type = NULL;
    ast->kind = AST_CONTINUE;
    ast->slabel = aoStrDup(continue_label);
    return ast;
}
static void astFreeContinue(Ast *ast) {
    aoStrRelease(ast->slabel);
    free(ast);
}

/* XXX: reference count string? 
 * Duplicates the label */
Ast *astBreak(aoStr *break_label) {
    Ast *ast = astNew();
    ast->type = NULL;
    ast->kind = AST_BREAK;
    ast->slabel = aoStrDup(break_label);
    return ast;
}
static void astFreeBreak(Ast *ast) {
    aoStrRelease(ast->slabel);
    free(ast);
}

Ast *astReturn(Ast *retval, AstType *rettype) {
    Ast *ast = astNew();
    ast->kind = AST_RETURN;
    ast->type = rettype;
    ast->retval = retval;
    return ast;
}
static void astFreeReturn(Ast *ast) {
    astRelease(ast->retval);
    free(ast);
}

AstType *astMakePointerType(AstType *type) {
    AstType *pointer_type = astTypeNew();
    pointer_type->kind = AST_TYPE_POINTER;
    pointer_type->ptr = type;
    pointer_type->size = 8;
    return pointer_type;
}

AstType *astMakeArrayType(AstType *type, int len) {
    AstType *array_type = astTypeNew();
    array_type->kind = AST_TYPE_ARRAY;
    array_type->ptr = type;
    /* How much memory */
    array_type->size = len < 0 ? -1 : type->size * len;
    /* Length of array */
    array_type->len = len;
    return array_type;
}

AstType *astConvertArray(AstType *ast_type) {
    if (ast_type->kind != AST_TYPE_ARRAY) {
        return ast_type;
    }
    return astMakePointerType(ast_type->ptr);
}

Ast *astGoto(aoStr *label) {
    Ast *ast = astNew();
    ast->type = NULL;
    ast->kind = AST_GOTO;
    ast->slabel = label;
    return ast;
}
static void astFreeGoto(Ast *ast) {
    aoStrRelease(ast->slabel);
    free(ast);
}

Ast *astLabel(aoStr *label) {
    Ast *ast = astNew();
    ast->type = NULL;
    ast->kind = AST_LABEL;
    ast->slabel = label;
    return ast;
}
static void astFreeLabel(Ast *ast) {
    aoStrRelease(ast->slabel);
    free(ast);
}

AstType *astMakeFunctionType(AstType *rettype, List *param_types) {
    AstType *type = astTypeNew();
    type->kind = AST_TYPE_FUNC;
    type->rettype = rettype;
    type->params = param_types;
    type->size = 8;
    return type;
}

AstType *astMakeClassField(AstType *type, int offset) {
    AstType *field_type = astTypeNew();
    memcpy(field_type,type,sizeof(AstType));
    field_type->clsname = NULL;
    field_type->offset = offset;
    return field_type;
}

Ast *astClassRef(AstType *type, Ast *cls, char *field_name) {
    Ast *ref = astNew();
    ref->kind = AST_CLASS_REF;
    ref->type = type;
    ref->cls = cls;
    ref->field = field_name;
    return ref;
}
static void astFreeClassRef(Ast *ast) {
    astRelease(ast->cls);
    free(ast->field);
    free(ast);
}

AstType *astClassType(Dict *fields, aoStr *clsname, int size, int is_intrinsic) {
    AstType *ref = astTypeNew();
    ref->kind = AST_TYPE_CLASS;
    ref->fields = fields;
    ref->size = size;
    ref->clsname = clsname;
    ref->is_intrinsic = is_intrinsic;
    return ref;
}

Ast *astCompountStatement(List *stmts) {
    Ast *ast = astNew();
    ast->kind = AST_COMPOUND_STMT;
    ast->type = NULL;
    ast->stms = stmts;
    return ast;
}
static void astFreeCompountStatement(Ast *ast) {
    astReleaseList(ast->stms);
    free(ast);
}

Ast *astAsmBlock(aoStr *asm_stmt, List *funcs) {
    Ast *ast = astNew();
    ast->kind = AST_ASM_STMT;
    ast->type = NULL;
    ast->asm_stmt = asm_stmt;
    ast->funcs = funcs;
    return ast;
}
static void astFreeAsmBlock(Ast *ast) {
    aoStrRelease(ast->asm_stmt);
    astReleaseList(ast->funcs);
    free(ast);
}

Ast *astAsmFunctionDef(aoStr *asm_fname, aoStr *asm_stmt) {
    Ast *ast = astNew();
    ast->kind = AST_ASM_FUNCDEF;
    ast->asmfname = asm_fname;
    ast->body = astNew();
    ast->body->asm_stmt = asm_stmt;
    return ast;
}
static void astFreeAsmFunctionDef(Ast *ast) {
    aoStrRelease(ast->asmfname);
    aoStrRelease(ast->body->asm_stmt);
    free(ast->body);
    free(ast);
}

Ast *astAsmFunctionBind(AstType *rettype, aoStr *asm_fname,
        aoStr *fname, List *params) {
    Ast *ast = astNew();
    ast->kind = AST_ASM_FUNC_BIND;
    ast->fname = fname;
    ast->asmfname = asm_fname;
    ast->type = rettype;
    ast->params = params;
    return ast;
}
static void astFreeAsmFunctionBind(Ast *ast) {
    aoStrRelease(ast->asmfname);
    aoStrRelease(ast->fname);
    astReleaseList(ast->params);
    free(ast);
}

Ast *astAsmFunctionCall(AstType *rettype, aoStr *asm_fname, List *argv,
        List *paramtypes) {
    Ast *ast = astNew();
    ast->type = rettype;
    ast->kind = AST_ASM_FUNCALL;
    ast->fname = asm_fname;
    ast->args = argv;
    ast->paramtypes = paramtypes;
    return ast;
}
static void astFreeAsmFunctionCall(Ast *ast) {
    aoStrRelease(ast->asmfname);
    aoStrRelease(ast->fname);
    astReleaseList(ast->args);
    astReleaseList(ast->paramtypes);
    free(ast);
}

Ast *astVarArgs(void) {
    Ast *ast = astNew();
    ast->kind = AST_VAR_ARGS;
    ast->type = NULL;
    ast->argc = astLVar(ast_int_type,"argc",4);
    ast->argv = astLVar(astMakeArrayType(ast_int_type,0),"argv",4);
    return ast;
}
static void astFreeVarArgs(Ast *ast) {
    astRelease(ast->argc);
    astRelease(ast->argv);
    free(ast);
}
Ast *astGlobalCmdArgs(void) {
    Ast *ast = astNew();
    ast->kind = AST_VAR_ARGS;
    ast->type = NULL;
    ast->argc = astDecl(astGVar(ast_int_type,"argc",4,0),NULL);
    ast->argv = astDecl(astGVar(astMakePointerType(astMakePointerType(ast_u8_type)),"argv",4,0),NULL);
    return ast;
}

Ast *astCast(Ast *var, AstType *to) {
    Ast *ast = astNew();
    ast->kind = AST_CAST;
    ast->operand = var;
    ast->type = to;
    return ast;
}
static void astFreeCast(Ast *ast) {
    astRelease(ast->operand);
    free(ast);
}

aoStr *astNormaliseFunctionName(char *fname) {
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
List *astParamTypes(List *params) {
    List *ref = listNew();
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
        listAppend(ref,type);
        it = it->next;
    }
    return ref;
}

int astIsAssignment(long op) {
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

void assertIsValidPointerOp(long op, long lineno) {
    switch (op) {
        case '-': case '+':
        case '<': case TK_LESS_EQU:
        case '>': case TK_GREATER_EQU:
        case TK_EQU_EQU: case TK_NOT_EQU:
        case TK_OR_OR: case TK_AND_AND:    
            return;
        default:
            loggerPanic("Invalid pointer operation: %s at line: %ld\n",
                    astKindToString(op),lineno);
    }
}

/* This is pretty gross to look at but, eliminated recursion */
AstType *astGetResultType(long op, AstType *a, AstType *b) {
    AstType *tmp;
    AstType *ptr1, *ptr2;
    ptr1 = astConvertArray(a);
    ptr2 = astConvertArray(b);

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

        assertIsValidPointerOp(op,-1);
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
        } else if (astIsIntType(ptr1) && ptr2->is_intrinsic) {
            return ast_int_type;
        } else if (ptr1->is_intrinsic && astIsIntType(ptr2)) {
            return ast_int_type;
        } else {
            goto error;
        }
    }

error:
    return NULL;
}

AstType *astTypeCheck(AstType *expected, Ast *ast) {
    if (expected != NULL && ast == NULL) return NULL;

    AstType *original_actual = ast->type;
    AstType *actual;
    if (original_actual == NULL) {
        original_actual = ast_void_type;
        actual = original_actual;
    } else {
        original_actual = ast->type;
        actual = astConvertArray(original_actual);
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
    } else if (astIsIntType(e) && astIsIntType(a)) {
        ret = e;
        goto out;
    } else if (astIsFloatType(e) && astIsFloatType(a)) {
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
        } else if (e->kind == AST_TYPE_CLASS && e->is_intrinsic && astIsIntType(a)) {
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

int astIsFloatType(AstType *type) {
    return type->kind == AST_TYPE_FLOAT;
}

int astIsIntType(AstType *type) {
    switch (type->kind) {
    case AST_TYPE_INT:
    case AST_TYPE_CHAR:
        return 1;
    default: return 0;
    }
}

void astStringEndStmt(aoStr *str) {
    if (str->data[str->len-1] != '\n') {
        aoStrPutChar(str, '\n');
    }
}

void astUnaryOpToString(aoStr *str, char *op, Ast *ast,int depth) {
    aoStrCatPrintf(str, "<unary_expr> %s\n", op);
    _astToString(str, ast->operand, depth+1);
    astStringEndStmt(str);
}

void astBinaryOpToString(aoStr *str, char *op, Ast *ast, int depth) {
    aoStrCatPrintf(str, "<binary_expr> %s\n", op);
    _astToString(str, ast->left, depth+1);
    astStringEndStmt(str);
    _astToString(str, ast->right, depth+1);
    astStringEndStmt(str);
}

int astIsClassPointer(AstType *type) {
    return type && type->kind == AST_TYPE_POINTER &&
           type->ptr->kind == AST_TYPE_CLASS;
}

int astIsUnionPointer(AstType *type) {
    return type && type->kind == AST_TYPE_POINTER &&
           type->ptr->kind == AST_TYPE_UNION;
}

char *astTypeToString(AstType *type) {
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
        aoStrCatPrintf(str, "%s*", astTypeToString(type->ptr));
        return aoStrMove(str);
    }

    case AST_TYPE_ARRAY: {
        aoStrCatPrintf(str, "array %s[%d]", astTypeToString(type->ptr), type->size);
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
        aoStrCatPrintf(str, "<fn> %s", astTypeToString(type->rettype));
        return aoStrMove(str);
    }

    case AST_TYPE_AUTO:
        aoStrCatPrintf(str, "auto");
        return aoStrMove(str);

    default:
        loggerPanic("Unknown type: %d\n", type->kind);
    }
}

char *astTypeToColorString(AstType *type) {
    char *str = astTypeToString(type);
    if (!isatty(STDOUT_FILENO)) {
        return str;
    }
    aoStr *buf = aoStrNew();
    aoStrCatPrintf(buf,"\033[0;32m%s\033[0m",str);
    free(str);
    return aoStrMove(buf);
}

char *astFunctionNameToString(AstType *rettype, char *fname, int len) {
    aoStr *str = aoStrNew();
    char *tmp = astTypeToColorString(rettype);
    aoStrCatPrintf(str,"%s %.*s()",tmp,len,fname);
    free(tmp);
    return aoStrMove(str);
}

char *astParamTypesToString(List *paramtypes) {
    aoStr *str = aoStrNew();
    char *tmp;
    AstType *param;
    if (listEmpty(paramtypes)) {
        tmp = astTypeToColorString(ast_void_type);
        aoStrCatPrintf(str,"%s",tmp);
        free(tmp);
    } else {
        listForEach(paramtypes) {
            param = it->value;
            if (param->kind == AST_VAR_ARGS) {
                if (it->next != paramtypes) aoStrCatPrintf(str,"..., ");
                else                        aoStrCatPrintf(str,"..."); 
            } else {
                tmp = astTypeToString(param);
                if (it->next != paramtypes) aoStrCatPrintf(str,"%s, ",tmp);
                else                        aoStrCatPrintf(str,"%s",tmp); 
                free(tmp);
            }
        }
    }
    return aoStrMove(str);
}

static char *astParamsToString(List *params) {
    aoStr *str = aoStrNew();
    char *tmp;
    Ast *param;
    if (listEmpty(params)) {
        tmp = astTypeToColorString(ast_void_type);
        aoStrCatPrintf(str,"%s",tmp);
        free(tmp);
    } else {
        listForEach(params) {
            param = it->value;
            if (param->kind == AST_VAR_ARGS) {
                if (it->next != params) aoStrCatPrintf(str,"..., ");
                else                    aoStrCatPrintf(str,"..."); 
            } else {
                tmp = astTypeToString(param->type);
                if (it->next != params) aoStrCatPrintf(str,"%s, ",tmp);
                else                    aoStrCatPrintf(str,"%s",tmp); 
                free(tmp);
            }
        }
    }
    return aoStrMove(str);
}

static char *astFunctionToStringInternal(Ast *func, AstType *type) {
    aoStr *str = aoStrNew();
    char *tmp,*strparams;

    tmp = astTypeToColorString(type);
    aoStrCatPrintf(str,"%s %s",tmp,func->fname->data);
    free(tmp);

    switch (func->kind) {
        case AST_FUNCALL:
        case AST_FUNPTR_CALL:
        case AST_ASM_FUNCALL:
            strparams = astParamsToString(func->args);
            break;

        case AST_FUNC:
            strparams = astParamsToString(func->params);
            break;
    }

    aoStrCatPrintf(str,"(%s)",strparams);
    return aoStrMove(str);
}

char *astFunctionToString(Ast *func) {
    return astFunctionToStringInternal(func,func->type->rettype);
}

void _astToString(aoStr *str, Ast *ast, int depth) {
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
        tmp = astTypeToString(ast->type);
        aoStrCatPrintf(str, "<lvar> %s %s\n", tmp,
                ast->lname->data);
        free(tmp);
        break;
    
    case AST_DECL:
        tmp = astTypeToString(ast->declvar->type);
        if (ast->declvar->kind == AST_FUNPTR) {
            aoStrCatPrintf(str,"<decl> %s %s\n", tmp, ast->declvar->fname->data);
        } else if (ast->declvar->kind == AST_LVAR) {
            aoStrCatPrintf(str,"<decl> %s %s\n", tmp, ast->declvar->lname->data);
        } else if (ast->declvar->kind == AST_GVAR) {
            if (ast->declvar->is_static) {
                aoStrCatPrintf(str,"<decl> static %s %s\n", tmp, ast->declvar->gname->data);
            } else {
                aoStrCatPrintf(str,"<decl> %s %s\n", tmp, ast->declvar->gname->data);
            }
        } else {
            loggerPanic("Unhandled declaration: %s\n", tmp);
        }
        free(tmp);
        if (ast->declinit) {
            _astToString(str,ast->declinit,depth+1);
            astStringEndStmt(str);
        }
        break;

    case AST_GVAR:
        aoStrCatPrintf(str, "<gvar> %s", ast->gname->data);
        break;
    
    case AST_ASM_FUNCALL: {
        tmp = astFunctionToStringInternal(ast,ast->type);
        aoStrCatPrintf(str, "<asm_function_call> %s\n", 
                    tmp);
            free(tmp);
            depth++;
            node = ast->args->next;
            while (node != ast->args) {
                aoStrCatRepeat(str, "  ", depth);
                aoStrCatPrintf(str, "<asm_function_arg>\n");
                _astToString(str,(Ast *)node->value,depth+1);
                astStringEndStmt(str);
                node = node->next;
            }
            break;
        }

        case AST_FUNPTR_CALL: {
            tmp = astTypeToString(ast->type);
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
                            astTypeToString(((AstType *)tmp)->rettype));
                }
                _astToString(str,(Ast *)node->value,depth+1);
                astStringEndStmt(str);
                node = node->next;
            }
            break;
        }

        case AST_FUNCALL: {
            tmp = astFunctionToStringInternal(ast,ast->type);
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
                            astTypeToString(((AstType *)tmp)->rettype));
                }
                _astToString(str,(Ast *)node->value,depth+1);
                astStringEndStmt(str);
                node = node->next;
            }
            break;
        }

        case AST_FUNPTR: {
            tmp = astTypeToString(ast->type);
            aoStrCatPrintf(str, "<function_ptr*> %s %s\n", tmp,
                    ast->fname->data);
            depth++;
            free(tmp);
            node = ast->params->next;
            while (node != ast->params) {
                aoStrCatRepeat(str, "  ", depth);
                aoStrCatPrintf(str, "<function_ptr_arg>\n");
                _astToString(str,(Ast *)node->value,depth+1);
                astStringEndStmt(str);
                node = node->next;
            }
            break;
        }

        case AST_EXTERN_FUNC: {
            tmp = astTypeToString(ast->type);
            aoStrCatPrintf(str, "<extern_function> %s %s\n", tmp, ast->fname->data);
            free(tmp);
            node = ast->params->next;
            while (node != ast->params) {
                param = node->value;
                aoStrCatRepeat(str, "  ", depth+1);
                aoStrCatPrintf(str, "<extern_function_param>\n");
                _astToString(str, param, depth+2);
                astStringEndStmt(str);
                node = node->next;
            }
            astStringEndStmt(str);
            break;
        }

        case AST_FUN_PROTO: {
            tmp = astTypeToString(ast->type);
            aoStrCatPrintf(str, "<function_proto> %s %s\n", tmp, ast->fname->data);
            free(tmp);
            node = ast->params->next;
            while (node != ast->params) {
                param = node->value;
                aoStrCatRepeat(str, "  ", depth+1);
                aoStrCatPrintf(str, "<function_proto_param>\n");
                _astToString(str, param, depth+2);
                astStringEndStmt(str);
                node = node->next;
            }
            astStringEndStmt(str);
            break;
        }

        case AST_FUNC: {
            tmp = astFunctionToString(ast);
            aoStrCatPrintf(str, "<function_def> %s\n", tmp);
            free(tmp);
            node = ast->params->next;
            while (node != ast->params) {
                param = node->value;
                aoStrCatRepeat(str, "  ", depth+1);
                aoStrCatPrintf(str, "<function_param>\n");
                _astToString(str, param, depth+2);
                astStringEndStmt(str);
                node = node->next;
            }
            _astToString(str, ast->body, depth + 1);
            astStringEndStmt(str);
            break;
        }

        case AST_ASM_FUNC_BIND: {
            tmp = astTypeToString(ast->type);
            aoStrCatPrintf(str, "<asm_function_bind> %s %s %s\n", tmp,
                    ast->asmfname->data,
                    ast->fname->data);
            free(tmp);
            node = ast->params->next;
            while (node != ast->params) {
                param = node->value;
                aoStrCatRepeat(str, "  ", depth+1);
                aoStrCatPrintf(str, "<asm_function_param>\n");
                _astToString(str, param, depth+2);
                astStringEndStmt(str);
                node = node->next;
            }
            astStringEndStmt(str);
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
                _astToString(str, node->value, depth + 1);
                if (node->next != ast->arrayinit) {
                    astStringEndStmt(str);
                }
                node = node->next;
            }
            break;

        case AST_IF:
            aoStrCatPrintf(str, "<if_cond>\n");
            _astToString(str,ast->cond,depth+1);
            astStringEndStmt(str);
            _astToString(str,ast->then,depth+1);
            if (ast->els) {
                aoStrCatRepeat(str, "  ", depth);
                aoStrCatPrintf(str, "<else_cond>\n");
                _astToString(str,ast->els,depth+1);
                astStringEndStmt(str);
            }
            break;

        case AST_DO_WHILE:
            aoStrCatRepeat(str, " ", depth);
            aoStrCatPrintf(str, "<do_while>\n");
            _astToString(str, ast->whilebody, depth+2);
            _astToString(str, ast->whilebody, depth+2);

            aoStrCatRepeat(str, "  ", depth+1);
            aoStrCatPrintf(str, "<while_cond>\n"); 
            _astToString(str, ast->whilecond, depth+2);
            astStringEndStmt(str);
            astStringEndStmt(str);
            break;

        case AST_WHILE:
            aoStrCatPrintf(str, "<while_cond>\n"); 
            _astToString(str, ast->whilecond, depth+2);
            astStringEndStmt(str);

            aoStrCatRepeat(str, "  ", depth+1);
            aoStrCatPrintf(str, "<while_body>\n"); 
            _astToString(str, ast->whilebody, depth+2);
            astStringEndStmt(str);
            break;

        case AST_FOR:
            aoStrCatPrintf(str, "<for_expr>\n");
            aoStrCatRepeat(str, "  ", depth+1);
            aoStrCatPrintf(str, "<for_init>\n"); 
            _astToString(str, ast->forinit, depth+2);
            astStringEndStmt(str);

            aoStrCatRepeat(str, "  ", depth+1);
            aoStrCatPrintf(str, "<for_cond>\n"); 
            _astToString(str, ast->forcond, depth+2);
            astStringEndStmt(str);

            aoStrCatRepeat(str, "  ", depth+1);
            aoStrCatPrintf(str, "<for_step>\n"); 
            _astToString(str, ast->forstep, depth+2);
            astStringEndStmt(str);

            aoStrCatRepeat(str, "  ", depth+1);
            aoStrCatPrintf(str, "<for_body>\n"); 
            _astToString(str, ast->forbody, depth+2);
            astStringEndStmt(str);
            break;

        case AST_RETURN:
            aoStrCatPrintf(str, "<return>\n");
            _astToString(str, ast->retval, depth+1);
            astStringEndStmt(str);
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
                _astToString(str, next, depth+1);
                astStringEndStmt(str);
                it = it->next;
            }
            astStringEndStmt(str);
            break;
        }
        
        case AST_CLASS_REF: {
            Ast *ast_tmp;
            char *field_names[30];
            int field_name_count = 0;
            aoStrCatPrintf(str, "<class_ref>\n");
            AstType *field_type = dictGet(ast->cls->type->fields, ast->field);

            /* We only really want to print at the data type we are looking 
             * at, not the whole class */
            if (field_type && ast->cls->kind == AST_DEREF) {
                aoStrCatRepeat(str, "  ", depth+1);

                if (!astIsClassPointer(field_type)) {
                    tmp = astTypeToColorString(field_type);
                    aoStrCatPrintf(str, "%s ", tmp);
                    free(tmp);
                } else {
                    tmp = astTypeToColorString(field_type);
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
                _astToString(str,ast->cls,depth+1);
                aoStrCatPrintf(str, "->");
                aoStrCatPrintf(str, "%s", ast->field, ast->type->offset);
            }
            astStringEndStmt(str);
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
            _astToString(str,ast->operand,depth+1);
            astStringEndStmt(str);
            break;

        case AST_DEREF:
            aoStrCatPrintf(str, "<deref>\n");
            _astToString(str,ast->operand,depth+1);
            //astStringEndStmt(str);
            break;

        case AST_CAST:
            aoStrCatPrintf(str, "<cast> %s %s -> %s\n",
                    astTypeToString(ast->operand->type),
                    astLValueToString(ast->operand,0),
                    astTypeToString(ast->type));
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
            _astToString(str,ast->declvar,depth+1);
            if (ast->declvar) {
                _astToString(str,ast->declinit,depth+1);
            }
            break;
        }
        case TK_PRE_PLUS_PLUS:
        case TK_PLUS_PLUS:   
        case TK_PRE_MINUS_MINUS:
        case TK_MINUS_MINUS:
            astUnaryOpToString(str, astKindToString(ast->kind),ast,depth);
            break;

        default: {
            astBinaryOpToString(str,astKindToString(ast->kind),ast,depth);
            astStringEndStmt(str);
            break;
        }
    }
}

/* String representation of the kind of ast we are looking at */
char *astKindToString(int kind) {
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

int astIsRangeOperator(long op) {
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

char *_astToStringRec(Ast *ast, int depth) {
    aoStr *str = aoStrNew();
    _astToString(str,ast, depth);
    return aoStrMove(str);
}

/* Convert an Ast to a string */
char *astToString(Ast *ast) {
    return _astToStringRec(ast,0);
}

/* This can only be used for lvalues */
static void _astLValueToString(aoStr *str, Ast *ast, unsigned long lexeme_flags);

void astUnaryArgToString(aoStr *str, char *op, Ast *ast, unsigned long lexeme_flags) {
    aoStrCatPrintf(str, "%s", op);
    _astLValueToString(str, ast->operand,lexeme_flags);
}

void astBinaryArgToString(aoStr *str, char *op, Ast *ast, unsigned long lexeme_flags) {
    _astLValueToString(str, ast->left,lexeme_flags);
    aoStrCatPrintf(str, " %s ", op);
    _astLValueToString(str, ast->right,lexeme_flags);
}

/* This can only be used for lvalues */
static void _astLValueToString(aoStr *str, Ast *ast, unsigned long lexeme_flags) {
    char *str_op = NULL;
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
                aoStrCatPrintf(str,"%s = ",ast->declvar->fname->data);
            } else {
                aoStrCatPrintf(str,"%s",ast->declvar->lname->data);
            }
            if (ast->declinit) {
                aoStrCatPrintf(str, " %s ",
                        lexemePunctToStringWithFlags('=',lexeme_flags));
                _astLValueToString(str,ast->declinit,lexeme_flags);
                aoStrPutChar(str, ';');
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
                _astLValueToString(internal,val,lexeme_flags);
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
            AstType *field_type = dictGet(ast->cls->type->fields, ast->field);

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

        case AST_GOTO: {
            aoStr *label = astHackedGetLabel(ast);
            aoStrCatPrintf(str,"goto %s",label->data);
            break;
        }

        case AST_LABEL: {
            aoStr *label = astHackedGetLabel(ast);
            aoStrCatPrintf(str,"%s:",label->data);
            break;
        }

        case AST_ADDR:
            aoStrCatPrintf(str, "&");
            _astLValueToString(str,ast->operand,lexeme_flags);
            break;

        case AST_DEREF:
            aoStrCatPrintf(str, "*");
            _astLValueToString(str,ast->operand,lexeme_flags);
            break;

        case AST_CAST:
            aoStrCatPrintf(str, "cast ");
            _astLValueToString(str,ast->operand,lexeme_flags);
            break;

        case AST_RETURN:
            aoStrCatPrintf(str, "return ");
            _astLValueToString(str,ast->retval, lexeme_flags);
            aoStrCatPrintf(str,"%s",
                    lexemePunctToStringWithFlags(';',lexeme_flags));
            break;

        case TK_PRE_PLUS_PLUS:
        case TK_PLUS_PLUS:   
        case TK_PRE_MINUS_MINUS:
        case TK_MINUS_MINUS:
        case '~':
        case '!': {
            str_op = lexemePunctToStringWithFlags(ast->kind,lexeme_flags);
            astUnaryArgToString(str,str_op,ast,lexeme_flags);
            break;
        }
        case '-': {
            str_op = lexemePunctToStringWithFlags(ast->kind,lexeme_flags);
            if (ast->right == NULL) {
                astUnaryArgToString(str,str_op,ast,lexeme_flags);
            } else {
                astBinaryArgToString(str,str_op,ast,lexeme_flags);
            }
            break;
        }

        default: {
            str_op = lexemePunctToStringWithFlags(ast->kind,lexeme_flags);
            astBinaryArgToString(str,str_op,ast,lexeme_flags);
            if (ast->left == NULL) {
                aoStrCatPrintf(str,"%s",
                        lexemePunctToStringWithFlags(';',lexeme_flags));
            }
            break;
        }
    }
}

char *astLValueToString(Ast *ast, unsigned long lexeme_flags) {
    aoStr *str = aoStrNew();
    _astLValueToString(str,ast,lexeme_flags);
    return aoStrMove(str);
}

/* Just print out one */
void astPrint(Ast *ast) {
    char *str = astToString(ast);
    printf("%s\n", str);
    //printf("%s\n", astKindToString(ast->kind));
    free(str);
}

void astTypePrint(AstType *type) {
    char *str = astTypeToString(type);
    printf("%s\n", str);
    free(str);
}

void astKindPrint(int kind) {
    char *str = astKindToString(kind);
    printf("%s\n",str);
}

void astReleaseList(List *ast_list) {
    listRelease(ast_list,(void(*))astRelease);
}

/* Free an Ast */
void astRelease(Ast *ast) {
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
            astFreeUnaryOperator(ast);
            break;
        case AST_LITERAL: astFreeLiteral(ast); break;
        case AST_GVAR: astFreeGVar(ast); break;
        case AST_GOTO: astFreeGoto(ast); break;
        case AST_LABEL: astFreeLabel(ast); break;
        case AST_JUMP: astFreeJump(ast); break;
        case AST_LVAR: astFreeLVar(ast); break;
        case AST_EXTERN_FUNC:
        case AST_FUN_PROTO:
        case AST_FUNC: astFreeFunction(ast); break;
        case AST_DECL: astFreeDecl(ast); break;
        case AST_STRING: astFreeString(ast); break;
        case AST_FUNCALL: astFreeFunctionCall(ast); break;
        case AST_ARRAY_INIT: astFreeArrayInit(ast); break;
        case AST_IF: astFreeIf(ast); break;
        case AST_FOR: astFreeFor(ast); break;
        case AST_RETURN: astFreeReturn(ast); break;
        case AST_DO_WHILE: 
        case AST_WHILE: astFreeWhile(ast); break;
        case AST_CLASS_REF: astFreeClassRef(ast); break;
        case AST_COMPOUND_STMT: astFreeCompountStatement(ast); break;
        case AST_ASM_STMT: astFreeAsmBlock(ast); break;
        case AST_ASM_FUNC_BIND: astFreeAsmFunctionBind(ast); break;
        case AST_ASM_FUNCALL: astFreeAsmFunctionCall(ast); break;
        case AST_FUNPTR: astFreeFunctionPtr(ast); break;
        case AST_FUNPTR_CALL: astFreeFunctionPtrCall(ast); break;
        case AST_BREAK: astFreeBreak(ast); break;
        case AST_CONTINUE: astFreeContinue(ast); break;
        case AST_DEFAULT_PARAM: astFreeFunctionDefaultParam(ast); break;
        case AST_VAR_ARGS: astFreeVarArgs(ast); break;
        case AST_ASM_FUNCDEF: astFreeAsmFunctionDef(ast); break;
        case AST_CAST: astFreeCast(ast); break;
        case AST_CASE: astFreeCase(ast); break;
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
                astFreeBinaryOp(ast);
                break;
    }
}
