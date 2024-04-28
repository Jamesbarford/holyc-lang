#include <stdio.h>
#include <stdlib.h>

#include "ast.h"
#include "cctrl.h"
#include "cfg.h"
#include "lexer.h"
#include "list.h"
#include "util.h"

static void cfgConstructCompoundStatement(CFGBuilder *builder, List *stmts);
static void cfgHandleAstNode(CFGBuilder *builder, Ast *ast);

static BasicBlock *cfgBuilderAllocBasicBlock(CFGBuilder *builder, int type) {
    BasicBlock *bb = (BasicBlock *)calloc(1,sizeof(BasicBlock));
    bb->type = 0;
    bb->prev_cnt = 0;
    bb->block_no = 0;
    bb->ast_array = AstArrayNew(16);
    bb->block_no = builder->bb_count++;
    return bb;
}

static void cfgBuilderInit(CFGBuilder *builder, Cctrl *cc) {
    builder->cc = cc;
    builder->bb_count = 0;
}

static void cfgBuilderSetCFG(CFGBuilder *builder, CFG *cfg) {
    builder->cfg = cfg;
}

static void cfgBuilderSetBasicBlock(CFGBuilder *builder, BasicBlock *bb) {
    builder->bb = bb;
}

static CFG *cfgNew(aoStr *fname, BasicBlock *head_block) {
    CFG *cfg = (CFG *)malloc(sizeof(CFG));
    cfg->head = head_block;
    cfg->ref_fname = fname;
    return cfg;
}

static void cgfHandleIfBlock(CFGBuilder *builder, Ast *ast) {
    BasicBlock *bb = builder->bb;
    BasicBlock *then_body;
    BasicBlock *else_body;
    AstArrayPush(bb->ast_array,ast->cond);

    then_body = cfgBuilderAllocBasicBlock(builder,CFG_CONTROL_BLOCK);
    cfgBuilderSetBasicBlock(builder,then_body);
    bb->_if = then_body;
    /* Start of a new basic block */
    cfgHandleAstNode(builder,ast->then);

    printf("cfg_ if\n");
    cfgBuilderSetBasicBlock(builder,bb);
 
    /* @Confirm
     * should the else block be part of this basic block and then the `bb->next` 
     * pointer set to the next block as opposed to explicitly setting a 
     * `bb->_else`? */
    if (ast->els) {
        printf("cfg_ else\n");
        else_body = cfgBuilderAllocBasicBlock(builder,CFG_CONTROL_BLOCK);
        bb->_else = else_body;
        cfgBuilderSetBasicBlock(builder,else_body);
        /* Possible entry to an alternate basic block */
        cfgHandleAstNode(builder,ast->els);
        cfgBuilderSetBasicBlock(builder,bb);
    }
}

static void cfgHandleAstNode(CFGBuilder *builder, Ast *ast) {
    int kind = ast->kind;
    BasicBlock *bb = builder->bb;
    BasicBlock *then_body;
    BasicBlock *else_body;

    /* We are only interested in the AST nodes that are the start of a new 
     * basic block or a top level `I64 x = 10;` type declaration or assignment 
     * 
     * A new basic block will start with an `if`, `for`, `while`, `goto` */
    switch (kind) {
        case AST_LABEL:
        case AST_ADDR:
        case AST_FUNC:
            break;
 
        case AST_LVAR: {
            AstArrayPush(bb->ast_array,ast);
            char *lvar_string = AstToString(ast);
            printf("cfg_ lvar: %s\n", lvar_string);
            free(lvar_string);
            break;
        }

        case AST_DECL: {
            AstArrayPush(bb->ast_array,ast);
            char *decl_string = AstToString(ast);
            printf("cfg_ decl: %s\n", decl_string);
            free(decl_string);
            break;
        }

        case AST_IF: {
            cgfHandleIfBlock(builder,ast);
            break;
        }

        case TK_PRE_PLUS_PLUS:
        case TK_PLUS_PLUS:   
        case TK_PRE_MINUS_MINUS:
        case TK_MINUS_MINUS:

        case AST_ASM_FUNCALL:
        case AST_FUNCALL:
        case AST_FUNPTR_CALL: {
            printf("cgf_function_call\n");
            AstArrayPush(bb->ast_array,ast);
            break;
        }


        case AST_CLASS_REF:
        case AST_DEREF:
        case AST_GOTO:
        case AST_ARRAY_INIT:
        case AST_DO_WHILE:
        case AST_WHILE:
        case AST_FOR:
        case AST_JUMP:
        case AST_CASE:
        case AST_BREAK:
        case AST_CONTINUE:
            break;

        case AST_COMPOUND_STMT: {
            cfgConstructCompoundStatement(builder,ast->stms);
            break;
        }

        case AST_STRING:
        case AST_LITERAL:
        case AST_RETURN:
        case AST_OP_ADD:
        case AST_ASM_STMT:
        case AST_ASM_FUNC_BIND:
        case AST_FUNPTR:
        case AST_DEFAULT_PARAM:
        case AST_VAR_ARGS:
        case AST_ASM_FUNCDEF:
        case AST_CAST:
        case AST_FUN_PROTO:
        case AST_EXTERN_FUNC:
        case AST_PLACEHOLDER:

        default:
            if (AstIsAssignment(kind)) {
                printf("cfg_assign: %s\n",lexemePunctToString(kind));
                AstArrayPush(bb->ast_array,ast);
            } else {
            }
            break;
    }
    printf("=======\n");
}

/* @Confirm:
 * I think this should be creating the graph from the List of ast's? */
static void cfgConstructCompoundStatement(CFGBuilder *builder, List *stmts) {
    /* Better to have this split out even if it means this function is 
     * comically short. */
    if (ListEmpty(stmts)) return;
    ListForEach(stmts) cfgHandleAstNode(builder,cast(Ast *,it->value));
} 

CFG *cfgConstruct(Cctrl *cc) {
    Ast *ast;
    CFG *cfg;
    CFGBuilder builder;
    BasicBlock *bb;

    cfgBuilderInit(&builder,cc);

    ListForEach(cc->ast_list) {
        ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            bb = cfgBuilderAllocBasicBlock(&builder,CFG_HEAD_BLOCK);
            cfg = cfgNew(ast->fname,bb);
            bb->next = cfgBuilderAllocBasicBlock(&builder,CFG_CONTROL_BLOCK);
            cfgBuilderSetCFG(&builder,cfg);
            cfgBuilderSetBasicBlock(&builder,bb->next);
            cfgConstructCompoundStatement(&builder,ast->body->stms);
        }
    }
    return cfg;
}
