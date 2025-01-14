#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "ir.h"
#include "lexer.h"
#include "list.h"
#include "map.h"
#include "transpiler.h"
#include "util.h"

void irFromAstInternal(Ast *ast, IrCtx *ctx);

#define IR_VARNAME_LEN   (32)

#define IR_FLAG_FUNC_ARG (1<<0)
#define IR_FLAG_GLOBAL   (1<<1)

typedef enum {
    IR_INVALID = -1,
    IR_START = 0,
    /* loads */
    IR_LOAD = 1,
    IR_LOAD_IMM,
    IR_LOAD_IMM_F64,
    IR_LOAD_IMM_STR,
    IR_LOAD_FN,
    IR_LOAD_F64,
    IR_LOAD_GLOBAL,
    IR_LOAD_ARRAY,
    /* Saves */
    IR_SAVE,
    IR_SAVE_IMM,
    IR_SAVE_IMM_F64,
    IR_SAVE_IMM_STR,
    IR_SAVE_FN,
    IR_SAVE_F64,
    IR_SAVE_GLOBAL,
    IR_SAVE_ARRAY,

    IR_JMP,
    IR_LABEL,
    IR_LT,
    IR_LTE,
    IR_GT,
    IR_GTE,
    IR_EQ,
    IR_NOT_EQ,
    IR_ADD,
    IR_SUB,
    IR_MUL,
    IR_DIV,
    IR_MOD,
    IR_SHL,
    IR_SHR,
    IR_RET,
    IR_CALL,
    IR_PUSH,
    IR_POP,
    IR_JMP_LT,
    IR_JMP_LTE,
    IR_JMP_GT,
    IR_JMP_GTE,
    IR_JMP_EQ,
    IR_JMP_NOT_EQ,
    IR_LOGICAL_OR,
    IR_LOGICAL_AND,
    IR_AND,
    IR_OR,
    IR_NOT,
    IR_XOR,
    IR_NEG,
    IR_POS,
    IR_PRE_PLUS_PLUS,
    IR_PRE_MINUS_MINUS,
    IR_PLUS_PLUS,
    IR_MINUS_MINUS,
    IR_ADDR,
    IR_DEREF,
    IR_CAST,
} IrOp;

static int var_id = 0;
char *irVarNextName(void) {
    static char buffer[IR_VARNAME_LEN];
    auto len = snprintf(buffer,sizeof(buffer),"@V.%d", var_id++);
    buffer[len] = '\0';
    return buffer;
}

typedef struct IrVar {
    char var_name[IR_VARNAME_LEN];
    AstType *type;

    void SetName(char *name) {
        strncpy(var_name,name,sizeof(var_name));
    }

    aoStr *ToString(void) {
        aoStr *var_str = aoStrNew();
        auto type_str = astTypeToAoStr(type);
        aoStrCatFmt(var_str, "%S %s",type_str, var_name);
        return var_str;
    }
} IrVar;

IrVar *irVarAlloc(AstType *type) {
    IrVar *ir_var = (IrVar *)malloc(sizeof(IrVar));
    ir_var->type = type;
    return ir->var;
}

struct IrInstruction {
    IrOp op;
    unsigned long flags;
    IrVar *dest;
    IrVar *s1;
    IrVar *s2;
    int size;
    double f64;
    aoStr *label;
    aoStr *fname;
    int offset;
};

struct IrModule {
    PtrVec *instructions;
    PtrVec *vars;
};

enum IrConnectionType {
    NEXT,
    THEN,
    ELSE,
};

struct IrBlockConnection {
    IrBlock *block;
    IrConnectionType connection_type;
};

struct IrBlock {
    IrInstruction *instructions;
    int instruction_capacity = 8;
    int instruction_count = 0;

    IrVar *ir_vars;
    int ir_var_capacity = 8;
    int ir_var_count = 0;

    IrBlockConnection prev[32];
    int prev_count = 0;

    IrVar *live_in[32];
    IrVar *live_out[32];
    IrVar *live_kill[32];
    IrVar *live_created[32];

    IrBlock() {
        instructions = (IrInstruction *)malloc(sizeof(IrInstruction)*instruction_capacity);
        ir_vars = (IrVar *)malloc(sizeof(IrVar) * ir_var_capacity);

    }

    void Release(void) {
        free(instructions);
        free(ir_vars);
        free(this);
    }

    int live_in_count = 0;
    int live_out_count = 0;
    int live_kill_count = 0;
    int live_created_count = 0;

    IrConnectionType connection_type;
    IrBlock *next_ = NULL;
    IrBlock *then_ = NULL;
    IrBlock *else_ = NULL;

    IrInstruction *AllocInstruction(void) {
        if (instruction_count + 1 >= instruction_capacity) {
            auto new_size = sizeof(IrInstruction)*instruction_capacity*2;
            instructions = (IrInstruction *)realloc(instructions,new_size);
            if (instructions == NULL) {
                loggerPanic("Failed to re-allocated instructions\n");
            }
            instruction_capacity *= 2;
        }
        auto ir_instruction = &instructions[instruction_count++];
        return ir_instruction;
    }

    IrVar *AllocVar(char *var_name) {
        if (ir_var_count + 1 >= ir_var_capacity) {
            auto new_size = sizeof(IrVar)*ir_var_capacity*2;
            ir_vars = (IrVar *)realloc(ir_vars,sizeof(IrVar)*new_size); 
            if (ir_vars == NULL) {
                loggerPanic("Failed to re-allocated ir_vars\n");
            }
            ir_var_capacity *= 2;
        }
        auto ir_var = &ir_vars[ir_var_count++];
        ir_var->SetName(var_name);
        return ir_var;
    }

    void Connect(IrBlock *successor, IrConnectionType connection_type) {
        if (!successor) {
            loggerPanic("Successor node should exist\n");
        }

       auto &prev = successor->prev[successor->prev_count];
        prev.block = this;
        prev.connection_type = connection_type;
        successor->prev_count++;

        switch (connection_type) {
            case IrConnectionType::NEXT: next_ = successor; break;
            case IrConnectionType::THEN: then_ = successor; break;
            case IrConnectionType::ELSE: else_ = successor; break;
            default: loggerPanic("Unknown connection type: %d\n", connection_type);
        }
    }
};

struct IrCtx {
    Cctrl *cc;
    unsigned long flags;
    IntMap *var_mapping;
    IrModule *ir_module;
    IrBlock *block;
    /* How much space is required on the stack to store the variables */
    int stack_size;

    /* So we do not lose track of variables when we are traversing the tree */
    IrInstruction *stack[32];
    /* Where we are in the above stack */
    int stack_idx;
};


const char *irOpToString(IrOp op) {
    switch (op) {
        case IR_INVALID: return "IR_INVALID";
        case IR_START: return "IR_START";
        case IR_LOAD: return "IR_LOAD";
        case IR_LOAD_IMM: return "IR_LOAD_IMM";
        case IR_LOAD_IMM_F64: return "IR_LOAD_IMM_F64";
        case IR_LOAD_IMM_STR: return "IR_LOAD_IMM_STR";
        case IR_LOAD_FN: return "IR_LOAD_FN";
        case IR_LOAD_F64: return "IR_LOAD_F64";
        case IR_LOAD_GLOBAL: return "IR_LOAD_GLOBAL";
        case IR_LOAD_ARRAY: return "IR_LOAD_ARRAY";
        case IR_SAVE: return "IR_SAVE";
        case IR_SAVE_IMM: return "IR_SAVE_IMM";
        case IR_SAVE_IMM_F64: return "IR_SAVE_IMM_F64";
        case IR_SAVE_IMM_STR: return "IR_SAVE_IMM_STR";
        case IR_SAVE_FN: return "IR_SAVE_FN";
        case IR_SAVE_F64: return "IR_SAVE_F64";
        case IR_SAVE_GLOBAL: return "IR_SAVE_GLOBAL";
        case IR_SAVE_ARRAY: return "IR_SAVE_ARRAY";
        case IR_JMP: return "IR_JMP";
        case IR_LABEL: return "IR_LABEL";
        case IR_LT: return "IR_LT";
        case IR_LTE: return "IR_LTE";
        case IR_GT: return "IR_GT";
        case IR_GTE: return "IR_GTE";
        case IR_EQ: return "IR_EQ";
        case IR_NOT_EQ: return "IR_NOT_EQ";
        case IR_ADD: return "IR_ADD";
        case IR_SUB: return "IR_SUB";
        case IR_MUL: return "IR_MUL";
        case IR_DIV: return "IR_DIV";
        case IR_MOD: return "IR_MOD";
        case IR_SHL: return "IR_SHL";
        case IR_SHR: return "IR_SHR";
        case IR_RET: return "IR_RET";
        case IR_CALL: return "IR_CALL";
        case IR_PUSH: return "IR_PUSH";
        case IR_POP: return "IR_POP";
        case IR_JMP_LT: return "IR_JMP_LT";
        case IR_JMP_LTE: return "IR_JMP_LTE";
        case IR_JMP_GT: return "IR_JMP_GT";
        case IR_JMP_GTE: return "IR_JMP_GTE";
        case IR_JMP_EQ: return "IR_JMP_EQ";
        case IR_JMP_NOT_EQ: return "IR_JMP_NOT_EQ";
        case IR_LOGICAL_OR: return "IR_LOGICAL_OR";
        case IR_LOGICAL_AND: return "IR_LOGICAL_AND";
        case IR_AND: return "IR_AND";
        case IR_OR: return "IR_OR";
        case IR_NOT: return "IR_NOT";
        case IR_XOR: return "IR_XOR";
        case IR_NEG: return "IR_NEG";
        case IR_POS: return "IR_POS";
        case IR_PRE_PLUS_PLUS: return "IR_PRE_PLUS_PLUS";
        case IR_PRE_MINUS_MINUS: return "IR_PRE_MINUS_MINUS";
        case IR_PLUS_PLUS: return "IR_PLUS_PLUS";
        case IR_MINUS_MINUS: return "IR_MINUS_MINUS";
        case IR_ADDR: return "IR_ADDR";
        case IR_DEREF: return "IR_DEREF";
        case IR_CAST: return "IR_CAST";
    }
}

char *irVarToString(IrVar *var) {
    auto type_str = astTypeToAoStr(var->type);
    aoStrCatFmt(type_str, " %s", var->var_name);
    return aoStrMove(type_str);
}

IrModule *irModuleNew(void) {
    IrModule *mod = (IrModule *)malloc(sizeof(IrModule));
    mod->vars = ptrVecNew();
    mod->instructions = ptrVecNew();
    return mod;
}

IrCtx *irCtxNew(Cctrl *cc) {
    IrCtx *ctx = (IrCtx *)malloc(sizeof(IrCtx));
    ctx->cc = cc;
    ctx->flags = 0;
    ctx->stack_size = 0;
    ctx->ir_module = irModuleNew();
    ctx->var_mapping = intMapNew(HT_SMALL_SIZE);
    ctx->stack_idx = 0;
    return ctx;
}

void irCtxPush(IrCtx *ctx, IrInstruction *instruction) {
    ctx->stack[ctx->stack_idx++] = instruction;
}

IrInstruction *irCtxPush(IrCtx *ctx) {
    if (ctx->stack_idx == 0) {
        loggerPanic("Stack decrementing would lead to a negative number!\n");
    }
    return ctx->stack[--ctx->stack_idx];
}

IrInstruction *irCtxPeek(IrCtx *ctx) {
    if (ctx->stack_idx == 0) {
        return NULL;
    }
    return ctx->stack[ctx->stack_idx - 1];
}

void irCtxReset(IrCtx *ctx) {
    intMapRelease(ctx->var_mapping);
}

//IrInstruction *irNew(IrCtx *ctx) {
//    IrInstruction *inst = (IrInstruction*)malloc(sizeof(IrInstruction));
//    inst->op = op;
//    inst->dest = dest;
//    inst->s1 = s1;
//    inst->s2 = s2;
//    inst->label = label;
//    inst->fname = fname;
//    inst->size = size;
//    inst->f64 = 0;
//    inst->offset = 0;
//    inst->flags = flags;
//    return inst;
//}

aoStr *irInstructionToString(IrInstruction *inst) {
    aoStr *str = aoStrNew();
    return str;
}

IrOp irOpFromKind(int kind) {
    switch (kind) {
        case '+': return IR_ADD;
        case '-': return IR_SUB;
        case '*': return IR_MUL;
        case '/': return IR_DIV;
        case '&': return IR_AND;
        case '^': return IR_XOR;
        case '|': return IR_OR;
        case TK_SHL: return IR_SHL;
        case TK_SHR: return IR_SHR;
        default: {
            loggerWarning("Ast Kind %s not handled defaulting to IR_MUL\n", astKindToString(kind));
            return IR_MUL;
        }
    }
}

IrOp irGetLoadOp(int kind) {
    switch (kind) {
        case AST_LVAR:   return IR_LOAD;
        case AST_GVAR:   return IR_LOAD_GLOBAL;
        case AST_FUNPTR: return IR_LOAD_FN;
        default:         return IR_LOAD;
    }
}

IrOp irGetSaveOp(int kind) {
    switch (kind) {
        case AST_LVAR:   return IR_SAVE;
        case AST_GVAR:   return IR_SAVE_GLOBAL;
        case AST_FUNPTR: return IR_SAVE_FN;
        default:         return IR_SAVE;
    }
}

IrOp irGetLVarLoad(AstType *type) {
    switch (type->kind) {
        case AST_TYPE_ARRAY: return IR_LOAD_ARRAY;
        case AST_TYPE_FLOAT: return IR_LOAD_F64;
        default:             return IR_LOAD;
    }
}

IrOp irGetLVarSave(AstType *type) {
    switch (type->kind) {
        case AST_TYPE_FLOAT: return IR_LOAD_F64;
        default:             return IR_LOAD;
    }
}

/* The Array all needs to go on the stack */
void irArrayInit(IrCtx *ctx, Ast *ast, AstType *type) {
    listForEach(ast->arrayinit) {
        Ast *tmp = (Ast *)it->value;
        if (tmp->kind == AST_ARRAY_INIT) {
            irArrayInit(ctx,tmp,type->ptr);
            continue;
        }

        /* Need to figure out how to save this array on the stack... do we 
         * know the size upfront? Not sure as if it is nested we probably need 
         * to recuse through it to find the total size */
        if (tmp->kind == AST_STRING) {
        } else {
            irFromAstInternal(ast, ctx);
            if (type->ptr) {
            } else {
            }
        }
    }
}

void irPointerArithmetic(IrCtx *ctx, long op, Ast *LHS, Ast *RHS) {
    int size = astGetPointerSize(LHS);

    // ??
    irFromAstInternal(LHS,ctx);
    irFromAstInternal(RHS,ctx);

    IrOp ir_op = IR_INVALID;
    switch (op) {
        case '+':            ir_op = IR_ADD; break;
        case '-':            ir_op = IR_SUB; break;
        case '>':            ir_op = IR_GT; break;
        case '<':            ir_op = IR_LT; break;
        case TK_GREATER_EQU: ir_op = IR_GTE; break;
        case TK_LESS_EQU:    ir_op = IR_LTE; break;
        case TK_EQU_EQU:     ir_op = IR_EQ; break;
        case TK_NOT_EQU:     ir_op = IR_NOT_EQ; break;
        default: loggerPanic("Invalid pointer operation: '%s'\n",
                             lexemePunctToStringWithFlags(op,0));
    }

    if (op == '+' || op == '-') {
        if (size > 1) {
            /* The max size is 8 so loading */

            /* Now we add the offset to the pointer */
        } else {
            /* Otherwise this is some sort of comparison */
        }
    } else {
        // do not know
    }
}

void irBinaryOp(IrCtx *ctx, int kind, Ast *ast) {
    Ast *RHS = ast->right;
    Ast *LHS = ast->left;
    if (ast->type->kind == AST_TYPE_POINTER) {
        irPointerArithmetic(ctx,kind,LHS,RHS);
        return;
    }


    irFromAstInternal(LHS,ctx);
    irFromAstInternal(RHS,ctx);
    IrOp op = irOpFromKind(kind);
}

void irFromAstInternal(Ast *ast, IrCtx *ctx) {
    if (ast == NULL) {
        return;
    }

    unsigned long prev_flags;
    List *node;
    aoStr *escaped;

    switch(ast->kind) {
    case AST_LITERAL: {
      //  irCtxNextRegId(ctx);
        switch (ast->type->kind) {
        case AST_TYPE_VOID: break;
        case AST_TYPE_INT: {
            break;
        }
        case AST_TYPE_CHAR:  {
            break;
        }

        case AST_TYPE_FLOAT: {
            break;
        }
        default:
            loggerPanic("Unhandled type: %d\n", ast->type->kind);
        }
        break;

        case AST_STRING: {
            escaped = aoStrEscapeString(ast->sval);
            break;
        }
    }

    case AST_COMMENT: {
        break;
    }

    /* we are not defining anything ... */
    case AST_LVAR: {
        IrOp load_op = irGetLVarLoad(ast->type);
        //ast->tmp_reg = reg_id;
        break;
    }    

    case AST_GVAR: {
        IrOp load_op = irGetLVarLoad(ast->type);
        astPrint(ast);
        break;
    }
    case AST_DECL: {
        char *name = NULL;
        IrOp save_op = IR_INVALID;
        if (ast->declvar->kind == AST_FUNPTR) {
            name = ast->declvar->fname->data;
            save_op = IR_SAVE_FN;
        } else if (ast->declvar->kind == AST_LVAR) {
            name = ast->declvar->lname->data;
            save_op = irGetLVarSave(ast->declvar->type);
        } else if (ast->declvar->kind == AST_GVAR) {
            name = ast->declvar->gname->data;
            save_op = IR_SAVE_GLOBAL;
        } else {
            loggerPanic("Unhandled declaration\n");
        }

        auto ir_instruction = ctx->block->AllocInstruction();

        if (ast->declinit) {
            irFromAstInternal(ast->declinit,ctx);
        }
        break;
    }
    
    case AST_ASM_FUNCALL: {
        Ast *asm_stmt = (Ast *)strMapGet(ctx->cc->asm_functions, ast->fname->data);
        aoStr *asm_fname = asm_stmt ? asm_stmt->fname : ast->fname;
        break;
    }

    case AST_FUNPTR_CALL:
        /* For when a function pointer exists on a class, interestingly 
         * due to maintaining a reference to the class this we be trivial 
         * to create class methods */
        if (ast->ref && ast->ref->kind == AST_CLASS_REF) {
            Ast *ref = ast->ref;
            irFromAstInternal(ref->cls, ctx);
            if (ref->cls->deref_symbol == TK_ARROW) {
            } else {
            }
        }

    case AST_FUNCALL: {
        PtrVec *argv = ast->args;
        prev_flags = ctx->flags;
        ctx->flags |= IR_FLAG_FUNC_ARG;
        for (int i = 0; i < argv->size; ++i) {
            Ast *arg = (Ast *)argv->entries[i];
            irFromAstInternal(arg,ctx);
        }
        ctx->flags &= ~IR_FLAG_FUNC_ARG;
        ctx->flags = prev_flags;
        break;
    }

    case AST_ARRAY_INIT: {
        node = ast->arrayinit->next;
        while (node != ast->arrayinit) {
            irFromAstInternal((Ast *)node->value, ctx);
            if (node->next != ast->arrayinit) {
            }
            node = node->next;
        }
        break;
    }

    case AST_IF: {
        irFromAstInternal(ast->cond,ctx);

        if (ast->then) {
            irFromAstInternal(ast->then,ctx);
        }

        if (ast->els) {
            /* Handle if else constructs */
            irFromAstInternal(ast->els,ctx);
        }
        break;
    }

    case AST_DO_WHILE: {
        irFromAstInternal(ast->whilebody,ctx);
        irFromAstInternal(ast->whilecond,ctx);
        break;
    }

    case AST_WHILE: {
        irFromAstInternal(ast->whilecond,ctx);
        if (ast->whilebody) {
            irFromAstInternal(ast->whilebody,ctx);
        }
        break;
    }

    case AST_FOR: {
        if (ast->forinit) {
            irFromAstInternal(ast->forinit,ctx);
        }

        if (ast->forcond) {
            irFromAstInternal(ast->forcond,ctx);
        }

        if (ast->forstep) {
            irFromAstInternal(ast->forstep,ctx);
        }

        if (ast->forbody) {
            irFromAstInternal(ast->forbody,ctx);
        }
        break;
    }

    case AST_RETURN: {
        irFromAstInternal(ast->retval,ctx);
        break;
    }

    case AST_VAR_ARGS:
        break;

    case AST_COMPOUND_STMT: {
        listForEach(ast->stms) {
            irFromAstInternal((Ast *)it->value,ctx);
        }
        break;
    }

    case AST_CLASS_REF: {
        irFromAstInternal(ast->cls, ctx);
        if (ast->cls->deref_symbol == TK_ARROW) {
        } else {
        }
        break;
    }

    case AST_JUMP: {
        char *label = ast->jump_label->data;
        break;
    }

    case AST_GOTO: {
        char *label = ast->slabel->data;
        break;
    }

    /* XXX: fix labels */
    case AST_LABEL: {
        char *label = ast->slabel ? ast->slabel->data : ast->slabel->data;
        break;
    }

    case AST_CAST: {
        break;
    }

    case AST_SWITCH: {
        irFromAstInternal(ast->switch_cond, ctx);

        for (int i = 0; i < ast->cases->size; ++i) {
            Ast *_case = (Ast *)ast->cases->entries[i];
            irFromAstInternal(_case,ctx);
        }
        if (ast->case_default) {
            irFromAstInternal(ast->case_default,ctx);
        }
        break;
    }

    case AST_CASE: {
        if (!listEmpty(ast->case_asts)) {
            listForEach(ast->case_asts) {
                Ast *case_ast = (Ast *)it->value;
                irFromAstInternal(case_ast,ctx);
            }
        }
        break;
    }

    case AST_DEFAULT: {
        irFromAstInternal(ast->case_default,ctx);
        break;
    }

    case AST_BREAK:
        break;

    case AST_CONTINUE:
        break;

    case AST_DEFAULT_PARAM: {
        break;
    }

    case AST_SIZEOF: {
        break;
    }

    case TK_PLUS_PLUS:
    case TK_MINUS_MINUS: {
        irFromAstInternal(ast->left,ctx);
        break;
    }

    case AST_ADDR:
        irFromAstInternal(ast->operand, ctx);
        break;

    case AST_DEREF: {
        if (ast->operand->kind == '+') {
            //irFromAstInternal(ast->operand,ctx);
    
            /* the offset into the array */
            //irFromAstInternal(ast->operand,ctx);
            //AstType *op_type = ast->operand->type;
            //AstType *result = ast->type;
            //int var_id = ast->tmp_reg;
            //printf("%s\n",astKindToString(ast->operand->left->type->kind));
            loggerDebug("here!\n");
            irFromAstInternal(ast->operand,ctx);
            //irBinaryOp(ctx,ast->operand->kind,ast->operand);
            //astPrint(ast->operand);
         

           // if (result->kind == AST_ADDR) {

           // } else if (result->kind == AST_TYPE_CLASS) {

           // } else if (result->kind != AST_TYPE_POINTER) {

           // } else if (result->kind == AST_TYPE_POINTER) {

           // }

        
            
           // astTypePrint(op_type);
           // astTypePrint(result);
            


            //printf("%s\n", transpileOneAst(ctx->cc, ast));
            //Ast *left = ast->operand->left;
            //Ast *right = ast->operand->right;
            //irFromAstInternal(left,ctx);
    
            ///* the offset into the array */
            //irFromAstInternal(right,ctx);
        } else {
            /* As `->` is a dereference we need to be able to distinguish 
             * between a class dereference and a general pointer dereference */
            irFromAstInternal(ast->operand,ctx);
        }
        break;
    }

    default: {
        irBinaryOp(ctx,ast->kind,ast);
        break;
    }
    }
}

/* Lowers an Ast to IR */
void irLowerAst(IrCtx *ctx, Ast *ast) {
    irFromAstInternal(ast, ctx);
}

void irFunc(Cctrl *cc, Ast *func) {
    IrCtx *ctx = irCtxNew(cc);
    /* Need all vars to be id'd */

    /* We don't actually want to know all of the locals at this point in time,
     * we may however want to know the parameters to the function */
    listForEach(func->locals) {
        Ast *local = (Ast *)it->value;
        printf("%s;\n", transpileOneAst(cc,local));
    }


    irFromAstInternal(func->body, ctx);

    for (int i = 0; i < ctx->ir_module->instructions->size; ++i) {
        IrInstruction *inst = (IrInstruction *)ctx->ir_module->instructions->entries[i];
        aoStr *str = irInstructionToString(inst);
        printf("%s\n",str->data);
        aoStrRelease(str);
    }
}

void irModuleAdd(Cctrl *cc) {
    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            irFunc(cc,ast);
        } else if (ast->kind == AST_DECL || ast->kind == AST_GVAR) {
            loggerWarning("Cannot yet handle: AST_DECL or AST_GVAR\n");
        } else if (ast->kind == AST_ASM_STMT) {
            loggerWarning("Cannot yet handle: AST_ASM_STMT\n");
        }
    }
}

void irFromAst(Cctrl *cc) {
    PtrVec *ir_modules = ptrVecNew();
    cc->ir_modules = ir_modules;
    irModuleAdd(cc);
}
