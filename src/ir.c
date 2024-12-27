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
void irLoadImmediateInt(IrCtx *ctx, long _i64, int size);
void irLoadImmediateFloat(IrCtx *ctx, Ast *ast);

IrModule *irModuleNew(void) {
    IrModule *module = (IrModule *)malloc(sizeof(IrModule));
    module->vars = ptrVecNew();
    module->instructions = ptrVecNew();
    return module;
}

IrCtx *irCtxNew(Cctrl *cc) {
    IrCtx *ctx = (IrCtx *)malloc(sizeof(IrCtx));
    ctx->cc = cc;
    ctx->flags = 0;
    ctx->next_temp_reg = 1;
    ctx->next_var_id = 1;
    ctx->stack_size = 0;
    ctx->ir_module = irModuleNew();
    ctx->var_mapping = intMapNew(HT_SMALL_SIZE);
    return ctx;
}

int irCtxNextVarId(IrCtx *ctx) {
    int id = ctx->next_var_id;
    ctx->next_var_id++;
    return id;
}

int irCtxNextRegId(IrCtx *ctx) {
    int reg = ctx->next_temp_reg;
    ctx->next_temp_reg++;
    return reg;
}

void irCtxReset(IrCtx *ctx) {
    ctx->next_temp_reg = 1;
    ctx->next_var_id = 1;
    intMapRelease(ctx->var_mapping);
}

/* Add an instruction to the current module */
void irInstructionAdd(IrCtx *ctx,
                      IrOp op,
                      int dest,
                      int s1,
                      int s2,
                      int size,
                      aoStr *label,
                      aoStr *fname,
                      unsigned long flags)
{
    IrInstruction *inst = (IrInstruction*)malloc(sizeof(IrInstruction));
    inst->op = op;
    inst->dest = dest;
    inst->s1 = s1;
    inst->s2 = s2;
    inst->label = label;
    inst->fname = fname;
    inst->size = size;
    inst->f64 = 0;
    inst->offset = 0;
    inst->flags = flags;
    ptrVecPush(ctx->ir_module->instructions, inst);
}

void irIntArithmetic(IrCtx *ctx, IrOp op, int dest, int s1, int s2, int size) {
    irInstructionAdd(ctx,op,dest,s1,s2,size,NULL,NULL,0);
}

#define irInstructionAddNoFlags(ctx, op, dest, s1, s2, size, label, fname) \
    irInstructionAdd(ctx, op, dest, s1, s2, size, label, fname, 0)

aoStr *irLoadString(IrInstruction *inst) {
    aoStr *str = aoStrNew();
    int is_global = inst->flags & IR_FLAG_GLOBAL;
    char *load_str = NULL;

    if (is_global) {
        switch (inst->op) {
            case IR_LOAD:         aoStrCatFmt(str,"GLOAD          @%i, L(%S)", inst->dest, inst->label); break;
            case IR_LOAD_IMM:     aoStrCatFmt(str,"GLOAD_IMM      @%i, L(%S)", inst->dest, inst->label); break;
            case IR_LOAD_IMM_F64: aoStrCatFmt(str,"GLOAD_IMM_F64  @%i, L(%S)", inst->dest, inst->label); break;
            case IR_LOAD_IMM_STR: aoStrCatFmt(str,"GLOAD_IMM_STR  @%i, L(%S)", inst->dest, inst->label); break;
            case IR_LOAD_FN:      aoStrCatFmt(str,"GLOAD_FN       @%i, L(%S)", inst->dest, inst->label); break;
            case IR_LOAD_F64:     aoStrCatFmt(str,"GLOAD_F64      @%i, L(%S)", inst->dest, inst->label); break;
            case IR_LOAD_GLOBAL:  aoStrCatFmt(str,"GLOAD_GLOBAL   @%i, L(%S)", inst->dest, inst->label); break;
            case IR_LOAD_ARRAY:   aoStrCatFmt(str,"GLOAD_ARRAY    @%i, L(%S)", inst->dest, inst->label); break;
            default: loggerPanic("Operation %d is not a valid load instruction\n", inst->op);
        }
    } else {
        switch (inst->op) {
            case IR_LOAD:         aoStrCatFmt(str,"LOAD           @%i, @%i", inst->dest, inst->s1); break;
            case IR_LOAD_IMM:     aoStrCatFmt(str,"LOAD_IMM       @%i, $%i", inst->dest, inst->s1); break;
            case IR_LOAD_IMM_F64: aoStrCatFmt(str,"LOAD_IMM_F64   @%i, $%f", inst->dest, inst->f64); break;
            case IR_LOAD_IMM_STR: aoStrCatFmt(str,"LOAD_IMM_STR    "); break;
            case IR_LOAD_FN:      aoStrCatFmt(str,"LOAD_FN         "); break;
            case IR_LOAD_F64:     aoStrCatFmt(str,"LOAD_F64        "); break;
            case IR_LOAD_GLOBAL:  aoStrCatFmt(str,"LOAD_GLOBAL     "); break;
            case IR_LOAD_ARRAY:   aoStrCatFmt(str,"LOAD_ARRAY     @%i, @%i", inst->dest, inst->s1); break;
            default: loggerPanic("Operation %d is not a valid load instruction\n", inst->op);
        }
    }
    return str;
}

aoStr *irInstructionToString(IrInstruction *inst) {
    aoStr *str = aoStrNew();
    switch (inst->op) {
        case IR_LOAD:
        case IR_LOAD_IMM:
        case IR_LOAD_IMM_F64:
        case IR_LOAD_IMM_STR:
        case IR_LOAD_FN:
        case IR_LOAD_F64:
        case IR_LOAD_GLOBAL:
        case IR_LOAD_ARRAY: {
            aoStr *load = irLoadString(inst);
            aoStrCatFmt(str,"%S",load);
            aoStrRelease(load);
            break;
        }

        case IR_SAVE:            aoStrCatFmt(str,"SAVE           @%i, @%i",
                                         inst->dest, inst->s1); break;
        case IR_SAVE_IMM:        aoStrCatFmt(str,"SAVE_IMM        "); break;
        case IR_SAVE_IMM_F64:    aoStrCatFmt(str,"SAVE_IMM_F64    "); break;
        case IR_SAVE_IMM_STR:    aoStrCatFmt(str,"SAVE_IMM_STR    "); break;
        case IR_SAVE_FN:         aoStrCatFmt(str,"SAVE_FN         "); break;
        case IR_SAVE_F64:        aoStrCatFmt(str,"SAVE_F64        "); break;
        case IR_SAVE_GLOBAL:     aoStrCatFmt(str,"SAVE_GLOBAL     "); break;
        case IR_SAVE_ARRAY:      aoStrCatFmt(str,"SAVE_ARRAY      "); break;
        case IR_JMP:             aoStrCatFmt(str,"JMP             "); break;
        case IR_LABEL:           aoStrCatFmt(str,"LABEL           "); break;
        case IR_LT:              aoStrCatFmt(str,"LT              "); break;
        case IR_LTE:             aoStrCatFmt(str,"LTE             "); break;
        case IR_GT:              aoStrCatFmt(str,"GT              "); break;
        case IR_GTE:             aoStrCatFmt(str,"GTE             "); break;
        case IR_EQ:              aoStrCatFmt(str,"EQ              "); break;
        case IR_NOT_EQ:          aoStrCatFmt(str,"NOT_EQ          "); break;
        case IR_ADD:             aoStrCatFmt(str,"ADD            @%i, @%i, @%i",
                                         inst->dest, inst->s1, inst->s2); break;
        case IR_SUB:             aoStrCatFmt(str,"SUB            @%i, @%i, @%i",
                                         inst->dest, inst->s1, inst->s2); break;
        case IR_MUL:             aoStrCatFmt(str,"MUL            @%i, @%i, @%i",
                                         inst->dest, inst->s1, inst->s2); break;
        case IR_DIV:             aoStrCatFmt(str,"DIV            @%i, @%i, @%i",
                                         inst->dest, inst->s1, inst->s2); break;
        case IR_MOD:             aoStrCatFmt(str,"MOD            @%i, @%i, @%i",
                                         inst->dest, inst->s1, inst->s2); break;
        case IR_SHL:             aoStrCatFmt(str,"SHL             "); break;
        case IR_SHR:             aoStrCatFmt(str,"SHR             "); break;
        case IR_RET:             aoStrCatFmt(str,"RET             "); break;
        case IR_CALL:            aoStrCatFmt(str,"CALL           %S", inst->fname); break;
        case IR_PUSH:            aoStrCatFmt(str,"PUSH            "); break;
        case IR_POP:             aoStrCatFmt(str,"POP             "); break;
        case IR_JMP_LT:          aoStrCatFmt(str,"JMP_LT          "); break;
        case IR_JMP_LTE:         aoStrCatFmt(str,"JMP_LTE         "); break;
        case IR_JMP_GT:          aoStrCatFmt(str,"JMP_GT          "); break;
        case IR_JMP_GTE:         aoStrCatFmt(str,"JMP_GTE         "); break;
        case IR_JMP_EQ:          aoStrCatFmt(str,"JMP_EQ          "); break;
        case IR_JMP_NOT_EQ:      aoStrCatFmt(str,"JMP_NOT_EQ      "); break;
        case IR_LOGICAL_OR:      aoStrCatFmt(str,"LOGICAL_OR      "); break;
        case IR_LOGICAL_AND:     aoStrCatFmt(str,"LOGICAL_AND     "); break;
        case IR_AND:             aoStrCatFmt(str,"AND             "); break;
        case IR_OR:              aoStrCatFmt(str,"OR              "); break;
        case IR_NOT:             aoStrCatFmt(str,"NOT             "); break;
        case IR_XOR:             aoStrCatFmt(str,"XOR             "); break;
        case IR_NEG:             aoStrCatFmt(str,"NEG             "); break;
        case IR_POS:             aoStrCatFmt(str,"POS             "); break;
        case IR_PRE_PLUS_PLUS:   aoStrCatFmt(str,"PRE_PLUS_PLUS   "); break;
        case IR_PRE_MINUS_MINUS: aoStrCatFmt(str,"PRE_MINUS_MINUS "); break;
        case IR_PLUS_PLUS:       aoStrCatFmt(str,"PLUS_PLUS       "); break;
        case IR_MINUS_MINUS:     aoStrCatFmt(str,"MINUS_MINUS     "); break;
        case IR_ADDR:            aoStrCatFmt(str,"ADDR            "); break;
        case IR_DEREF:           aoStrCatFmt(str,"DEREF           "); break;
        case IR_CAST:            aoStrCatFmt(str,"CAST            "); break;
        default:
            loggerPanic("Unhandled op: %d\n", inst->op);
            break;

    }
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

void irInstructionFloat(IrCtx *ctx,
                        IrOp op,
                        int dest,
                        double f64,
                        unsigned long flags)
{
    IrInstruction *inst = (IrInstruction*)malloc(sizeof(IrInstruction));
    inst->f64 = f64;
    inst->op = op;
    inst->size = 8;
    inst->flags = flags;
    ptrVecPush(ctx->ir_module->instructions, inst);
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
            irInstructionAddNoFlags(ctx,
                             IR_SAVE_IMM_STR,
                             0,0,0,
                             tmp->sval->len,
                             ast->sval,
                             NULL);
        } else {
            irFromAstInternal(ast, ctx);
            if (type->ptr) {
                irInstructionAddNoFlags(ctx,
                                 irGetLVarSave(type->ptr),
                                 0,0,0,
                                 tmp->sval->len,
                                 ast->sval,
                                 NULL);
            } else {
                irInstructionAddNoFlags(ctx,
                                 irGetLVarSave(type),
                                 0,0,0,
                                 tmp->sval->len,
                                 ast->sval,
                                 NULL);
            }
        }
    }
}

void irPointerArithmetic(IrCtx *ctx, long op, Ast *LHS, Ast *RHS) {
    int size = astGetPointerSize(LHS);

    irFromAstInternal(LHS,ctx);
    irCtxNextRegId(ctx);
    irFromAstInternal(RHS,ctx);
    int save_reg = LHS->tmp_reg;

    IrOp ir_op = -1;
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
            irCtxNextRegId(ctx);
            irLoadImmediateInt(ctx,size,8);

            int reg = ctx->next_temp_reg;
            /* Now we add the offset to the pointer */
            irIntArithmetic(ctx,IR_MUL,reg,reg,RHS->tmp_reg,8);
            irIntArithmetic(ctx,ir_op,reg,save_reg,reg,8);
        } else {
            /* Otherwise this is some sort of comparison */
            irCtxNextRegId(ctx);
            irInstructionAdd(ctx,ir_op,ctx->next_temp_reg,
                    LHS->tmp_reg,RHS->tmp_reg,size,NULL,NULL,0);
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

   // if (ast->left && ast->left->kind == AST_DEREF) {
   //     if (ast->left) {
   //     } else {
   //         irFromAstInternal(ast,ctx);
   //     }

   //     if (ast->right) {
   //         irFromAstInternal(ast->right,ctx);
   //     }
   // } else if (!ast->right) { // unary
   //     irFromAstInternal(ast->left,ctx);
   // } else {
        /* eval left */
        if (LHS->tmp_reg == -1) {
            LHS->tmp_reg = ctx->next_temp_reg;
        }
        int save_reg = LHS->tmp_reg;

        irFromAstInternal(LHS,ctx);
        /* eval right */
        if (RHS->tmp_reg == -1) {
            RHS->tmp_reg = ctx->next_temp_reg;
        }
        irFromAstInternal(RHS,ctx);
        /* Well we've lost track of the variables, in assembly land we'd stash 
         * them in RAX and RCX */
        IrOp op = irOpFromKind(kind);

        int dest_reg = ctx->next_temp_reg;
        irCtxNextRegId(ctx);

        irInstructionAddNoFlags(ctx, op,
                dest_reg,
                LHS->tmp_reg,
                RHS->tmp_reg,
                ast->type->size,
                NULL,NULL);
        irInstructionAddNoFlags(ctx, IR_SAVE,
                save_reg,
                dest_reg,
                0,
                ast->type->size,
                NULL,NULL);
   // }
}

void irLoadImmediateInt(IrCtx *ctx, long _i64, int size) {
    irInstructionAdd(ctx,IR_LOAD_IMM,ctx->next_temp_reg,
                     _i64,0,size,NULL,NULL,ctx->flags);
}

void irLoadImmediateFloat(IrCtx *ctx, Ast *ast) {
    irInstructionFloat(ctx,IR_LOAD_IMM_F64, ctx->next_temp_reg,ast->f64,ctx->flags);
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
        ast->tmp_reg = ctx->next_temp_reg;
      //  irCtxNextRegId(ctx);
        switch (ast->type->kind) {
        case AST_TYPE_VOID: break;
        case AST_TYPE_INT: {
            irLoadImmediateInt(ctx,ast->i64,ast->type->size);
            break;
        }
        case AST_TYPE_CHAR:  {
            irLoadImmediateInt(ctx,ast->i64,ast->type->size);
            break;
        }

        case AST_TYPE_FLOAT: {
            irLoadImmediateFloat(ctx,ast);
            break;
        }
        default:
            loggerPanic("Unhandled type: %d\n", ast->type->kind);
        }
        break;

        case AST_STRING: {
            escaped = aoStrEscapeString(ast->sval);
            irInstructionAdd(ctx, IR_LOAD_IMM_STR, ctx->next_temp_reg,0,0,ast->type->size,escaped,NULL,ctx->flags);
            break;
        }
    }

    case AST_COMMENT: {
        break;
    }

    /* we are not defining anything ... */
    case AST_LVAR: {
        int var_id = ast->tmp_reg;
        int reg_id = irCtxNextRegId(ctx);
        IrOp load_op = irGetLVarLoad(ast->type);
        irInstructionAddNoFlags(ctx,load_op,reg_id,var_id,0,ast->type->size,NULL,NULL);
        //ast->tmp_reg = reg_id;
        break;
    }    

    case AST_GVAR: {
        int reg_id = irCtxNextRegId(ctx);
        ast->tmp_reg = reg_id;
        IrOp load_op = irGetLVarLoad(ast->type);
        astPrint(ast);
        irInstructionAdd(ctx,load_op,reg_id,0,0,
                         ast->type->size,ast->gname,NULL,IR_FLAG_GLOBAL);
        break;
    }
    case AST_DECL: {
        char *name = NULL;
        IrOp save_op = -1;
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

        int var_id = ast->declvar->tmp_reg; //irCtxNextVarId(ctx);
        int reg_id = irCtxNextRegId(ctx);

        if (ast->declinit) {
            irFromAstInternal(ast->declinit,ctx);
            irInstructionAdd(ctx,save_op,reg_id,var_id,0,ast->declvar->type->size,NULL,NULL,ctx->flags);
            ast->tmp_reg = reg_id;
        }
        break;
    }
    
    case AST_ASM_FUNCALL: {
        Ast *asm_stmt = strMapGet(ctx->cc->asm_functions, ast->fname->data);
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
        irInstructionAddNoFlags(ctx,IR_CALL,0,0,0,0,NULL,ast->fname);
        break;
    }

    case AST_ARRAY_INIT: {
        node = ast->arrayinit->next;
        while (node != ast->arrayinit) {
            irFromAstInternal(node->value, ctx);
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
            Ast *_case = ast->cases->entries[i];
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
            //int reg_id = irCtxNextRegId(ctx);
            //IrOp load_op = irGetLVarLoad(result);
            //irInstructionAddNoFlags(ctx,load_op,reg_id,var_id,0,result->size,NULL,NULL);
            loggerDebug("here!\n");
            irFromAstInternal(ast->operand,ctx);
            //irBinaryOp(ctx,ast->operand->kind,ast->operand);
            //astPrint(ast->operand);
         

           // if (result->kind == AST_ADDR) {
           //     irInstructionAdd();

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

    listForEach(func->locals) {
        Ast *local = (Ast *)it->value;
        intMapAdd(ctx->var_mapping,ctx->next_temp_reg,local);
        local->tmp_reg = ctx->next_temp_reg;
        irCtxNextRegId(ctx);
        printf("%s;\n", transpileOneAst(cc,local));
    }


    irFromAstInternal(func->body, ctx);

    for (int i = 0; i < ctx->ir_module->instructions->size; ++i) {
        IrInstruction *inst = ctx->ir_module->instructions->entries[i];
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
