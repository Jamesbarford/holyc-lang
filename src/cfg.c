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
    if (builder->bb_pos + 1 >= builder->bb_cap) {
        int new_cap = builder->bb_cap * 2;
        BasicBlock *bb_pool = cast(BasicBlock *,
                realloc(builder->bb_pool,new_cap));
        if (bb_pool == NULL) {
            loggerPanic("Failed to reallocate memory for basic block pool\n");
        }
        builder->bb_pool = bb_pool;
        builder->bb_cap = new_cap;
    }
    BasicBlock *bb = &builder->bb_pool[builder->bb_pos++];
    bb->type = 0;
    bb->prev_cnt = 0;
    bb->block_no = 0;
    bb->ast_array = AstArrayNew(16);
    bb->block_no = builder->bb_count++;
    return bb;
}

void cfgBuilderRelease(CFGBuilder *builder, int free_builder) {
    if (builder) {
        free(builder->bb_pool);
        if (free_builder) {
            free(builder);
        }
    }
}

static void cfgBuilderInit(CFGBuilder *builder, Cctrl *cc) {
    BasicBlock *bb_pool = cast(BasicBlock *, malloc(sizeof(BasicBlock)*64));
    builder->cc = cc;
    builder->bb_count = 0;
    builder->bb_cap = 64;
    builder->bb_pool = bb_pool;
    builder->bb_pos = 0;
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

/* @Buggy */
static void cgfHandleIfBlock(CFGBuilder *builder, Ast *ast) {
    BasicBlock *bb = builder->bb;
    BasicBlock *new_block, *then_body, *else_body;
    AstArrayPush(bb->ast_array,ast->cond);

    /* Start of a new basic block */
    then_body = cfgBuilderAllocBasicBlock(builder,CFG_CONTROL_BLOCK);
    cfgBuilderSetBasicBlock(builder,then_body);
    cfgHandleAstNode(builder,ast->then);
    bb->_if = then_body;
    new_block = cfgBuilderAllocBasicBlock(builder,CFG_CONTROL_BLOCK);
    then_body->next = new_block;
    // cfgBuilderSetBasicBlock(builder,new_block);
 

    /* @Confirm
     * should the else block be part of this basic block and then the `bb->next` 
     * pointer set to the next block as opposed to explicitly setting a 
     * `bb->_else`? */
    if (ast->els) {
        else_body = cfgBuilderAllocBasicBlock(builder,CFG_CONTROL_BLOCK);
        cfgBuilderSetBasicBlock(builder,else_body);
        cfgHandleAstNode(builder,ast->els);
        bb->_else = else_body;
        bb->_else->next = new_block;
    }
    cfgBuilderSetBasicBlock(builder,new_block);
}

static void cfgHandleAstNode(CFGBuilder *builder, Ast *ast) {
    int kind = ast->kind;
    BasicBlock *bb = builder->bb;

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
            break;
        }

        case AST_DECL: {
            AstArrayPush(bb->ast_array,ast);
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
                AstArrayPush(bb->ast_array,ast);
            } else {
            }
            break;
    }
}

/* @Confirm:
 * I think this should be creating the graph from the List of ast's? */
static void cfgConstructCompoundStatement(CFGBuilder *builder, List *stmts) {
    /* Better to have this split out even if it means this function is 
     * comically short. */
    if (ListEmpty(stmts)) return;
    ListForEach(stmts) cfgHandleAstNode(builder,cast(Ast *,it->value));
} 

static void bbPrint(BasicBlock *bb) {
    for (BasicBlock *it = bb; bb; bb = bb->next) {
        BasicBlock *next = bb->next ? bb->next : NULL;
        printf("++++++++++++\n");
        printf("block: %d -> \n", bb->block_no);
        if (bb->_if) {
            printf("    goto %d\n",bb->_if->block_no);
        }
        if (bb->_else) {
            printf("    goto %d\n",bb->_else->block_no);
        }

        if (next) {
            printf("    block: %d\n",next->block_no);
        }

        if (!bb->_if && !bb->_else && ! next) {
            printf("    nil\n");
        }

        for (int i = 0; i < bb->ast_array->count; ++i) {
            AstPrint(bb->ast_array->entries[i]);
        }

        if (bb->_if) {
            bbPrint(bb->_if);
        }
        if (bb->_else) {
            bbPrint(bb->_else);
        }
        printf("######\n");
    }
}

static void cfgWalk(CFG *cfg) {
    printf("Entry: %s\n", cfg->ref_fname->data);
    BasicBlock *bb = cfg->head;
    bbPrint(bb);
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
    cfgWalk(cfg);
    return cfg;
}
