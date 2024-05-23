#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "cfg.h"
#include "dict.h"
#include "lexer.h"
#include "list.h"
#include "util.h"

static void cfgHandleAstNode(CFGBuilder *builder, Ast *ast);

const char *bbTypeToString(BasicBlock *bb) {
    switch (bb->type) {
        case BB_END_BLOCK:          return "BB_END_BLOCK";
        case BB_HEAD_BLOCK:         return "BB_HEAD_BLOCK";
        case BB_CONTROL_BLOCK:      return "BB_CONTROL_BLOCK";
        case BB_BRANCH_BLOCK:       return "BB_BRANCH_BLOCK";
        case BB_LOOP_BLOCK:         return "BB_LOOP_BLOCK";
        case BB_RETURN_BLOCK:       return "BB_RETURN_BLOCK";
        case BB_BREAK_BLOCK:        return "BB_BREAK_BLOCK";
        case BB_DO_WHILE_COND:      return "BB_DO_WHILE_COND";
        case BB_GOTO:               return "BB_GOTO";
        default:
            loggerPanic("Unknown type: %d\n", bb->type);
    }
}

static BasicBlock *cfgBuilderAllocBasicBlock(CFGBuilder *builder, int type) {
    /* @Bug, @HashTable
     * This can and will create a new pointer sometimes and thus all references
     * in the allocated instances will be invalid. Hence we need a bigger pool,
     * or no `->next` or `->else` and work off an adjacency list. Essentially
     * either one massive slab needs to be allocated up front or we work off
     * numerical references in a hash table.
     * */
    if (builder->bb_pos + 1 >= builder->bb_cap) {
        int new_cap = builder->bb_cap * 2;
        BasicBlock *bb_pool = cast(BasicBlock *,
                realloc(builder->bb_pool,new_cap * sizeof(BasicBlock)));
        if (bb_pool == NULL) {
            loggerPanic("Failed to reallocate memory for basic block pool\n");
        }
        builder->bb_pool = bb_pool;
        builder->bb_cap = new_cap;
    }
    BasicBlock *bb = &builder->bb_pool[builder->bb_pos++];
    bb->type = type;
    bb->flags = 0;
    bb->prev_cnt = 0;
    bb->block_no = builder->bb_count++;
    /* @Leak
     * This should probably be a memory pool as well */
    bb->ast_array = AstArrayNew(16);
    bb->next = bb->prev = bb->_else = bb->_if = NULL;
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
    builder->bb_count = 1;
    builder->bb_cap = 64;
    builder->bb_pool = bb_pool;
    builder->bb_pos = 0;
    builder->flags = 0;
    builder->bb_cur_loop = NULL;

    builder->unresoved_gotos = ListNew();

    builder->resolved_labels = DictNew(&default_table_type);
    DictSetFreeKey(builder->resolved_labels,NULL);
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

static void cgfHandleBranchBlock(CFGBuilder *builder, Ast *ast) {
    /**
     * c-code:
     * +--------------------------+                                           
     * | I64 x = 10;              |                                           
     * | I64 y;                   |
     * | y = x * 89 + 1;          |
     * | if (x == 10) {           |                                           
     * |    y = 42;               |                                           
     * | } else if (x == 21) {    |                                            
     * |    y = 99;               |                                           
     * | } else {                 |                                           
     * |    y = 69;               |                                           
     * |    if (y == 69) {        |                                           
     * |       printf("69\n");    |                                           
     * |    }                     |                                           
     * | }                        |                                           
     * | printf("%ld\n",y);       |                                           
     * | if (x == 12) {           |                                           
     * |    printf("kowabunga\n");|
     * | }                        | 
     * | return 10;               |                                           
     * +--------------------------+                                           
     */

    int in_conditional = builder->flags & CFG_BUILDER_FLAG_IN_CONDITIONAL;
    BasicBlock *bb = builder->bb;
    bb->type = BB_BRANCH_BLOCK;
    AstArrayPush(bb->ast_array,ast->cond);
    builder->flags |= CFG_BUILDER_FLAG_IN_CONDITIONAL;

    BasicBlock *else_body = cfgBuilderAllocBasicBlock(builder,
                                                      BB_CONTROL_BLOCK);
    BasicBlock *if_body = cfgBuilderAllocBasicBlock(builder,
                                                    BB_CONTROL_BLOCK);
    BasicBlock *new_block;


    if_body->prev   = bb;
    else_body->prev = bb;

    bb->_if   = if_body;
    bb->_else = else_body;

    /* Start of a new basic block */
    cfgBuilderSetBasicBlock(builder,if_body);
    cfgHandleAstNode(builder,ast->then);

    if (ast->els) {
        cfgBuilderSetBasicBlock(builder,else_body);
        cfgHandleAstNode(builder,ast->els);

        /* This means the node has changed, and thus the next block needs 
         * to be this branch */
        if (builder->bb != else_body) {
            builder->bb->prev = bb;
            /* Join */ 
            if (bb->_if->type != BB_BREAK_BLOCK) {
                bb->_if->next = builder->bb;
                bb->_else->next = builder->bb;
            }
        } else {
            new_block = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);
            bb->_else->next = new_block;
            bb->_if->next = new_block;
            new_block->prev = bb->_if;

            cfgBuilderSetBasicBlock(builder,new_block);
        }
    } else {
        if (builder->bb != if_body) {
            builder->bb->prev = bb;
        }
        //if (if_body->type != BB_BREAK_BLOCK) {
        //    bb->_if->next = else_body;
        //}
        cfgBuilderSetBasicBlock(builder,else_body);
    }

    if (!in_conditional) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_CONDITIONAL);
    }
}

static void cfgHandleForLoop(CFGBuilder *builder, Ast *ast) {
    Ast *ast_init = ast->forinit;
    Ast *ast_cond = ast->forcond;
    Ast *ast_body = ast->forbody;
    Ast *ast_step = ast->forstep;

    builder->flags |= CFG_BUILDER_FLAG_IN_LOOP;
    BasicBlock *bb_prev_loop = builder->bb_cur_loop;

    BasicBlock *bb = builder->bb;
    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,
                                                         BB_CONTROL_BLOCK);
    BasicBlock *bb_cond = cfgBuilderAllocBasicBlock(builder,BB_BRANCH_BLOCK);
    /* Body may or may not be a Loop, we correct it in the if condition with
     * builder->bb != bb_body */
    BasicBlock *bb_body = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);

    if (ast_init) AstArrayPush(bb->ast_array,ast_init);

    bb_cond->flags |= BB_FLAG_LOOP_HEAD;

    bb->next = bb_cond;
    bb_cond->prev = bb;
    /* Jump into loop body if condition is met */
    bb_cond->_if = bb_body;
    /* Else move past loop body */
    bb_cond->_else = bb_cond_else;
    /* And the previously met block was the condition */
    bb_cond_else->prev = bb_cond;

    builder->bb_cur_loop = bb_cond;

    cfgBuilderSetBasicBlock(builder,bb_cond);
    cfgHandleAstNode(builder,ast_cond);

    /* Forms a loop, though this should be picked up by the if condition
     * below */
    bb_body->prev = bb_cond;

    /* Body exists within a loop 
     *
     * This c code:
     * +--------------------------------+
     * | I64 y = 420;                   |
     * | for (I64 i = 0; i < 10; ++i) { |
     * |    printf("%d\n",y);           |
     * | }                              |
     * | return 0;                      |
     * +--------------------------------+
     *
     *
     * Becomes this control flow graph:
     * +--------------------+
     * | BB1:               |
     * +--------------------+
     * | y = 420;           |
     * | i = 0;             |<- forinit.
     * +--------------------+
     *           |          
     *           V          
     * +--------------------+
     * | BB2:               |<- forcond is this whole block.
     * +--------------------+
     * | if i < 10          |         
     * |    goto BB3;       |--------+
     * | else               |        |
     * |    goto BB4;       |--------+
     * +--------------------+        |
     *                               |
     * +--------------------+        |
     * | BB3:               |<-------+
     * +--------------------+        |
     * | printf("%d\n", y); |        |<- forbody forms the base of this block.
     * | ++i                |  loop  |<- forstep is merged in with forbody.
     * | goto BB2           |=======>+<- unconditional jump to the condition
     * +--------------------+  BB2   |   block.
     *                               |
     *                               |
     * +--------------------+        |
     * | BB4:               |<-------+
     * +--------------------+
     * | return 0;          |
     * +--------------------+
     **/

    /* @Wrong? Is this actually wrong?
     * We realistically do not want to be merging these two together in the 
     * same unit.
     *
     * Think step should be separate and that bb_body->next always should point
     * to it?
     * */
    cfgBuilderSetBasicBlock(builder,bb_body);
    cfgHandleAstNode(builder,ast_body);

    if (bb_body->type != BB_BREAK_BLOCK) {
        cfgHandleAstNode(builder,ast_step);
    }

    /* This means we had branches or possibly inner loops in our loop */
    if (builder->bb != bb_body || !builder->bb->next) {
        builder->bb->type = BB_LOOP_BLOCK;
        builder->bb->prev = bb_cond;
    }

    builder->bb->flags |= BB_FLAG_LOOP_END;

    if (builder->bb->type == BB_BREAK_BLOCK && 
        (builder->bb->flags & BB_FLAG_LOOP_END)) {
        builder->bb->flags |= BB_FLAG_REDUNDANT_LOOP;
    }

    cfgBuilderSetBasicBlock(builder,bb_cond_else);

    if (!bb_prev_loop) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_LOOP);
    }

    builder->bb_cur_loop = bb_prev_loop;
}

/* @CopyPaste
 * This is a copy and paste of the above for loop code with the ast_step removed
 * it can almost certainly be merged together
 */
static void cfgHandleWhileLoop(CFGBuilder *builder, Ast *ast) {
    Ast *ast_cond = ast->whilecond;
    Ast *ast_body = ast->whilebody;
    
    BasicBlock *bb_prev_loop = builder->bb_cur_loop;
    BasicBlock *bb = builder->bb;
    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,
                                                         BB_CONTROL_BLOCK);
    BasicBlock *bb_cond = cfgBuilderAllocBasicBlock(builder,BB_BRANCH_BLOCK);
    BasicBlock *bb_body = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);

    bb_cond->flags |= BB_FLAG_LOOP_HEAD;
    bb->next = bb_cond;
    bb_cond->prev = bb;
    /* Jump into loop body if condition is met */
    bb_cond->_if = bb_body;
    /* Else move past loop body */
    bb_cond->_else = bb_cond_else;
    /* And the previously met block was the condition */
    bb_cond_else->prev = bb_cond;

    builder->bb_cur_loop = bb_cond;

    cfgBuilderSetBasicBlock(builder,bb_cond);
    cfgHandleAstNode(builder,ast_cond);

    /* Forms a loop, though this should be picked up by the if condition
     * below */
    bb_body->prev = bb_cond;

    builder->flags |= CFG_BUILDER_FLAG_IN_LOOP;

    cfgBuilderSetBasicBlock(builder,bb_body);
    cfgHandleAstNode(builder,ast_body);

    /* This means we had branches or possibly inner loops in our loop */
    if (builder->bb != bb_body || !builder->bb->next) {
        builder->bb->type = BB_LOOP_BLOCK;
        builder->bb->prev = bb_cond;
    }

    builder->bb->flags |= BB_FLAG_LOOP_END;

    if (builder->bb->type == BB_BREAK_BLOCK && 
        (builder->bb->flags & BB_FLAG_LOOP_END)) {
        builder->bb->flags |= BB_FLAG_REDUNDANT_LOOP;
    }

    cfgBuilderSetBasicBlock(builder,bb_cond_else);

    if (!bb_prev_loop) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_LOOP);
    }

    builder->bb_cur_loop = bb_prev_loop;
}

static void cfgHandleReturn(CFGBuilder *builder, Ast *ast) {
    BasicBlock *bb = builder->bb;
    BasicBlock *bb_return;

    if (bb->ast_array->count == 0) {
        bb_return = bb;
        bb_return->type = BB_RETURN_BLOCK;
    } else {
        bb_return = cfgBuilderAllocBasicBlock(builder,BB_RETURN_BLOCK);
        bb->next = bb_return;
        bb_return->prev = bb;
    }
    bb_return->next = NULL;
    AstArrayPush(bb_return->ast_array,ast);
    cfgBuilderSetBasicBlock(builder,bb_return);
}

/* @CopyPaste
 * This is a copy and paste of the above for loop code with the ast_step removed
 * it can almost certainly be merged together
 */
static void cfgHandleDoWhileLoop(CFGBuilder *builder, Ast *ast) {
    Ast *ast_cond = ast->whilecond;
    Ast *ast_body = ast->whilebody;

    BasicBlock *bb_prev_loop = builder->bb_cur_loop;
    BasicBlock *bb = builder->bb;

    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);
    BasicBlock *bb_do_while = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);

    bb->next = bb_do_while;
    bb_do_while->flags |= (BB_FLAG_LOOP_HEAD);

    builder->bb_cur_loop = bb_do_while;
    builder->flags |= CFG_BUILDER_FLAG_IN_LOOP;

    cfgBuilderSetBasicBlock(builder,bb_do_while);
    cfgHandleAstNode(builder,ast_body);

    builder->bb->type = BB_DO_WHILE_COND;
    builder->bb->prev = bb_do_while;
    builder->bb->next = bb_cond_else;
    AstArrayPush(builder->bb->ast_array,ast_cond);
    builder->bb->flags |= BB_FLAG_LOOP_END;

    if (!bb_prev_loop) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_LOOP);
    }

    cfgBuilderSetBasicBlock(builder,bb_cond_else);
    builder->bb_cur_loop = bb_prev_loop;
}

static void cfgHandleGoto(CFGBuilder *builder, Ast *ast) {
    BasicBlock *dest;
    aoStr *label = ast->slabel;

    builder->bb->type = BB_GOTO;

    /* Try straight out the gate to resolve the goto otherwise save it for 
     * later*/
    if ((dest = DictGetLen(builder->resolved_labels,label->data,
            label->len)) != NULL)
    {
        dest->flags |= BB_FLAG_LOOP_HEAD;
        builder->bb->prev = dest;
        builder->bb->type = BB_LOOP_BLOCK;
        builder->bb->flags |= BB_FLAG_LOOP_END;
    } else {
        ListAppend(builder->unresoved_gotos,builder->bb);
    }

    AstArrayPush(builder->bb->ast_array,ast);
    
    /* If this is an unconditional jump */
    if (!(builder->flags & CFG_BUILDER_FLAG_IN_CONDITIONAL)) {
        BasicBlock *bb_next = cfgBuilderAllocBasicBlock(builder,
                BB_CONTROL_BLOCK);
        builder->bb->flags |= BB_FLAG_UNCONDITIONAL_JUMP;
        builder->bb->next = bb_next;
        cfgBuilderSetBasicBlock(builder,bb_next);
    }
}

static void cfgHandleAstNode(CFGBuilder *builder, Ast *ast) {
    assert(ast != NULL);
    int kind = ast->kind;
    BasicBlock *bb = builder->bb;
    List *stmts;

    /* We are only interested in the AST nodes that are the start of a new 
     * basic block or a top level `I64 x = 10;` type declaration or assignment 
     * 
     * A new basic block will start with an `if`, `for`, `while`, `goto` */
    switch (kind) {
        case AST_LABEL: {
            AstArrayPush(bb->ast_array,ast);
            aoStr *label = AstHackedGetLabel(ast);
            DictSet(builder->resolved_labels,label->data,builder->bb);
            builder->bb->flags |= BB_FLAG_LABEL;
            if (!(builder->flags & CFG_BUILDER_FLAG_IN_LOOP)) {
                builder->bb->type = BB_BREAK_BLOCK;
            }
            break;
        }
        case AST_ADDR:
        case AST_FUNC:
            break;
 
        case TK_PRE_PLUS_PLUS:
        case TK_PLUS_PLUS:   
        case TK_PRE_MINUS_MINUS:
        case TK_MINUS_MINUS:
        case AST_ARRAY_INIT:
        case AST_ASM_FUNCALL:
        case AST_FUNCALL:
        case AST_FUNPTR_CALL:
        case AST_DECL:
        case AST_LVAR: {
            AstArrayPush(bb->ast_array,ast);
            break;
        }

        case AST_IF: {
            cgfHandleBranchBlock(builder,ast);
            break;
        }

        case AST_FOR: {
            cfgHandleForLoop(builder,ast);
            break;
        }
                      
        case AST_DO_WHILE: {
            cfgHandleDoWhileLoop(builder,ast);
            break;
        }

        case AST_WHILE: {
            cfgHandleWhileLoop(builder,ast);
            break;
        }

        case AST_RETURN: {
            cfgHandleReturn(builder,ast);
            break;
        }

        case AST_GOTO: {
            cfgHandleGoto(builder,ast);
            break;
        }

        case AST_CLASS_REF:
        case AST_DEREF:

        case AST_JUMP:
        case AST_CASE:
            break;

        case AST_BREAK: {
            /* This is a goto */
            /* Switch not implemented */
            assert(builder->flags & CFG_BUILDER_FLAG_IN_LOOP);
            loggerDebug("we broke: %d\n", builder->bb->block_no);
            builder->bb->type = BB_BREAK_BLOCK;
            builder->bb->next = builder->bb_cur_loop->_else;
            break;
        }

        case AST_CONTINUE: {
            assert(builder->flags & CFG_BUILDER_FLAG_IN_LOOP);
            break;
        }

        /* We are in a jump */
        case AST_COMPOUND_STMT: {
            stmts = ast->stms;
            if (ListEmpty(stmts)) return;
            ListForEach(stmts) {
                Ast *ast = cast(Ast *,it->value);
                bb = builder->bb;
                cfgHandleAstNode(builder,ast);
            }
            break;
        }

        case AST_STRING:
        case AST_LITERAL:
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
            /* @Refactor */
            if (AstIsAssignment(kind)) {
                AstArrayPush(bb->ast_array,ast);
            } else {
                AstArrayPush(bb->ast_array,ast);
            }
            break;
    }
}

static void bbFix(BasicBlock *bb);
static void bbRelinkBranch(BasicBlock *branch);
static void bbFixBranchBlock(BasicBlock *bb);

static void bbRelinkBranch(BasicBlock *branch) {
    AstArray *next_array = branch->next ? branch->next->ast_array : NULL;

    if (branch->type == BB_BRANCH_BLOCK) {
        if (next_array) {
            AstArray *els_array = branch->_else->ast_array;
            if (next_array->count != 0 && els_array->count == 0) {
                /* mark as destroyed */
                AstArrayRelease(els_array);
                branch->_else->block_no = BB_GARBAGE;
                branch->_else = branch->next;
                branch->next = NULL;
            } else {
                AstArrayRelease(next_array);
                branch->next->block_no = BB_GARBAGE;
                branch->next = NULL;
            }
        }
    } else if (branch->type == BB_CONTROL_BLOCK) {
        /* Then we need to reverse up the graph again like we're doing in 
         * bbFix() ... */
        if (next_array && next_array->count == 0) {
            loggerDebug("bb:%d\n", branch->block_no);
        } else if (!next_array) {
            loggerDebug("bb:%d\n", branch->block_no);
        }
    }
}

static void bbFixBranchBlock(BasicBlock *bb) {
    BasicBlock *then = bb->_if, *els = bb->_else;

    if (els->type == BB_CONTROL_BLOCK) {
        bbRelinkBranch(els);
        bbFix(els);
    }

    if (then->type == BB_CONTROL_BLOCK) {
        bbFix(then);
    }

    if (els->type == BB_BRANCH_BLOCK) {
        bbRelinkBranch(els);
        bbFix(els);
    }

    if (then->type == BB_BRANCH_BLOCK) {
        bbFix(then);
    }
}

static void bbFix(BasicBlock *bb) {
    for (; bb && bb->type != BB_END_BLOCK; bb = bb->next) {
        /* Search up the tree to fix the node */
        if (!bb->next && bb->type == BB_CONTROL_BLOCK) {
            BasicBlock *prev = bb->prev;
            /* Iterate back up the tree to find the join */
            while (prev) {
                if (prev->type == BB_BRANCH_BLOCK) {
                    BasicBlock *prev_else = prev->_else;
                    BasicBlock *prev_if = prev->_if;

                    if (prev_if->type == BB_CONTROL_BLOCK) {
                        bb->next = prev_if->next;
                    } else if (prev_else->type == BB_CONTROL_BLOCK) {
                        bb->next = prev_else->next;
                    }
                    if (prev_if->type == BB_RETURN_BLOCK) bb->next = prev_if;
                    if (prev_else->type == BB_RETURN_BLOCK) bb->next = prev_else;
                } else if (prev->type == BB_HEAD_BLOCK) {
                    break;
                } else if (prev->type == BB_CONTROL_BLOCK) {
                    /* We've found a random control block which indicates 
                     * that our current block is genuinely the end of the road*/
                    if (prev->next != bb) {
                        bb->type = BB_END_BLOCK;
                        bb->next = NULL;
                        break;
                    }
                }
                prev = prev->prev;
            }

        } else if (bb->type == BB_BRANCH_BLOCK) {
            bbFixBranchBlock(bb);
        }
    }
}

/* Split out to make it simpler to reason with */
static void cfgConstructFunction(CFGBuilder *builder, List *stmts) {
    if (ListEmpty(stmts)) return;
    builder->ast_list = stmts;
    ListForEach(stmts) {
        Ast *ast = cast(Ast *,it->value);
        builder->ast_iter = it;
        cfgHandleAstNode(builder,ast);
    }

    BasicBlock *bb = &builder->bb_pool[0];
    bbFix(bb);


    /* Reconcile any non linear jumps */
    ListForEach(builder->unresoved_gotos) {
        BasicBlock *bb_goto = cast(BasicBlock *,it->value);
        BasicBlock *bb_dest = NULL;
        AstArray *ast_array = bb_goto->ast_array;
        Ast *ast = NULL; 

        for (int i = 0; i < ast_array->count; ++i) {
            ast = ast_array->entries[i];
            if (ast->kind == AST_GOTO) {
                break;
            }
        }

        assert(ast->kind == AST_GOTO);

        bb_dest = DictGetLen(builder->resolved_labels,ast->slabel->data,
                ast->slabel->len);
        assert(bb_dest != NULL);

        if (bb_goto->flags & BB_FLAG_UNCONDITIONAL_JUMP) {
        /* If we are doing an unconditional jump into a loop, a switch or 
         * a branch we need to re-order things as the flow changes. These 
         * are somewhat contrived edge cases but _can_ happen. 
         *
         * If we are jumping into an if or a switch we can remove all of the 
         * other branches as they will never be hit
         * */
            loggerDebug("unconditional jump to: %s\n",
                    bbTypeToString(bb_dest));

            /* This kind of changes it to a do while as the condition will 
             * have to come last */
            if (bb_dest->type == BB_LOOP_BLOCK) {
                BasicBlock *head = bb_dest;
                BasicBlock *tail = bb_dest;
                
                // bb_dest->flags |= BB_FLAG_LOOP_HEAD;

                /* This is not a fair assumption as there could be potentially
                 * many `previous blocks` */
                while (head->prev && !(head->flags & BB_FLAG_LOOP_HEAD)) {
                    head = head->prev;
                }
                while (tail->next && !(tail->flags & BB_FLAG_LOOP_END)) {
                    tail = tail->next;
                }

                if (bb_dest->next == NULL) {
                    printf("next is NULL\n");
                }

                printf("loop head: bb%d %s\n", head->block_no, bbTypeToString(head));
                printf("loop tail: bb%d %s\n", tail->block_no, bbTypeToString(tail));
                
                //if (bb_dest->prev && bb_dest->prev->flags & BB_FLAG_LOOP_HEAD) {

                //    BasicBlock *tmp_prev = bb_dest->prev; 
                //    loggerDebug("tmp_prev = %s\n", bbTypeToString(tmp_prev));
                //    /* This block */
                //    tmp_prev->flags &= ~BB_FLAG_LOOP_HEAD;

                //    bb_dest->flags |= BB_FLAG_LOOP_HEAD;

                //    bb_dest->prev->next = tmp_prev;
                //    bb_dest->prev = bb_dest;
                //    bb_dest->next = tmp_prev;
                //
                //   printf("yup\n");
                //}
            }

        }

        printf("%s => %d\n",ast->slabel->data,bb_dest->block_no);
//        if (bb_dest->type == BB_BREAK_BLOCK) {
//            bb_goto->type = BB_BREAK_BLOCK;
//        }

        bb_goto->next = bb_dest;
    }

    /* We still want these structures but don't want their contents for the 
     * next functions. */
    DictClear(builder->resolved_labels);
    ListClear(builder->unresoved_gotos,NULL);
}

/* This will need to return a list of CFG's */
CFG *cfgConstruct(Cctrl *cc) {
    Ast *ast;
    CFG *cfg;
    CFGBuilder builder;
    BasicBlock *bb;

    cfgBuilderInit(&builder,cc);

    ListForEach(cc->ast_list) {
        ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            bb = cfgBuilderAllocBasicBlock(&builder,BB_HEAD_BLOCK);
            cfg = cfgNew(ast->fname,bb);
            bb->next = cfgBuilderAllocBasicBlock(&builder,BB_CONTROL_BLOCK);
            bb->next->prev = bb;
            cfgBuilderSetCFG(&builder,cfg);
            cfgBuilderSetBasicBlock(&builder,bb->next);
            cfgConstructFunction(&builder,ast->body->stms);

            cfg->bb_count = builder.bb_count;
            cfg->_memory = builder.bb_pool;
        }
    }

    DictRelease(builder.resolved_labels);
    ListRelease(builder.unresoved_gotos,NULL);

    loggerDebug("completed the construction\n");
    return cfg;
}
