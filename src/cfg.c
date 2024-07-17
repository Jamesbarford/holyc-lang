#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "cfg.h"
#include "dict.h"
#include "lexer.h"
#include "list.h"
#include "map.h"
#include "util.h"

static void cfgHandleAstNode(CFGBuilder *builder, Ast *ast);

char *bbTypeToString(int type) {
    switch (type) {
        case BB_GARBAGE:            return "BB_GARBAGE";
        case BB_END_BLOCK:          return "BB_END_BLOCK";
        case BB_HEAD_BLOCK:         return "BB_HEAD_BLOCK";
        case BB_CONTROL_BLOCK:      return "BB_CONTROL_BLOCK";
        case BB_BRANCH_BLOCK:       return "BB_BRANCH_BLOCK";
        case BB_LOOP_BLOCK:         return "BB_LOOP_BLOCK";
        case BB_RETURN_BLOCK:       return "BB_RETURN_BLOCK";
        case BB_BREAK_BLOCK:        return "BB_BREAK_BLOCK";
        case BB_DO_WHILE_COND:      return "BB_DO_WHILE_COND";
        case BB_GOTO:               return "BB_GOTO";
        case BB_SWITCH:             return "BB_SWITCH";
        case BB_CASE:               return "BB_CASE";
        default:
            loggerPanic("Unknown type: %d\n", type);
    }
}

char *bbFlagsToString(unsigned int flags) {
    if (!flags) return mprintf("(NO_FLAGS)");
    aoStr *str = aoStrNew();
    int has_flag = 0;

#define _concat_flag(str_flag) \
    has_flag ? aoStrCat(str,"|"str_flag) : aoStrCat(str,str_flag); \
    has_flag = 1;

    aoStrPutChar(str,'(');
    if (flags & BB_FLAG_IF_BRANCH) {
        _concat_flag("BB_FLAG_IF_BRANCH");
    }
    if (flags & BB_FLAG_LOOP_HEAD) {
        _concat_flag("BB_FLAG_LOOP_HEAD");
    }
    if (flags & BB_FLAG_ELSE_BRANCH) {
        _concat_flag("BB_FLAG_ELSE_BRANCH");
    }
    if (flags & BB_FLAG_LOOP_END) {
        _concat_flag("BB_FLAG_LOOP_END");
    }
    if (flags & BB_FLAG_REDUNDANT_LOOP) {
        _concat_flag("BB_FLAG_REDUNDANT_LOOP");
    }
    if (flags & BB_FLAG_LABEL) {
        _concat_flag("BB_FLAG_LABEL");
    }
    if (flags & BB_FLAG_UNCONDITIONAL_JUMP) {
        _concat_flag("BB_FLAG_UNCONDITIONAL_JUMP");
    }
    if (flags & BB_FLAG_LOOP_JUMP) {
        _concat_flag("BB_FLAG_LOOP_JUMP");
    }
    if (flags & BB_FLAG_GOTO_LOOP) {
        _concat_flag("BB_FLAG_GOTO_LOOP");
    }
    if (flags & BB_FLAG_CASE_OWNED) {
        _concat_flag("BB_FLAG_CASE_OWNED");
    }
    aoStrPutChar(str,')');

#undef _concat_flag

    return aoStrMove(str); 
}

char *bbPreviousBlockNumbersToString(BasicBlock *bb) {
    aoStr *str = aoStrNew();
    aoStrCatPrintf(str,"prev_cnt = %d: ",bb->prev_cnt);
    for (int i = 0; i < bb->prev_cnt; ++i) {
        aoStrCatPrintf(str,"%dbb",bb->prev_blocks[i]->block_no);
        if (i + 1 != bb->prev_cnt) aoStrCatPrintf(str,", ");
    }
    return aoStrMove(str);
}

char *bbToString(BasicBlock *bb) {
    aoStr *str = aoStrNew();
    char *str_flags = bbFlagsToString(bb->flags);
    char *str_type =  bbTypeToString(bb->type);
    char *str_prev =  bbPreviousBlockNumbersToString(bb);

    aoStrCatPrintf(str,"bb%d type = %s, flags = %s, %s, ",
            bb->block_no,
            str_type,
            str_flags,
            str_prev);
    free((char*)str_flags);
    free((char*)str_prev);

    if (bb->next)  aoStrCatPrintf(str,"next = bb%d, ",bb->next->block_no);
    if (bb->_if)   aoStrCatPrintf(str,"if = bb%d, ",bb->_if->block_no);
    if (bb->_else) aoStrCatPrintf(str,"else = bb%d ",bb->_else->block_no);
    if (bb->prev)  aoStrCatPrintf(str,"prev_ptr = bb%d ",bb->prev->block_no);
    return aoStrMove(str);
}

void bbPrint(BasicBlock *bb) {
    char *bb_str = bbToString(bb);
    aoStr *str = aoStrDupRaw((char*)bb_str, strlen(bb_str)); 
    aoStrPutChar(str,'\n');
    for (int i = 0; i < bb->ast_array->size; ++i) {
        char *ast_str = astLValueToString(bb->ast_array->entries[i],0);
        aoStrCat(str,ast_str);
        aoStrPutChar(str,'\n');
        free(ast_str);
    }
    free((char*)bb_str);
    printf("========\n%s========\n",str->data);
    aoStrRelease(str);
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
        loggerPanic("Out of memory - implement resizable pool");
    }
    BasicBlock *bb = &builder->bb_pool[builder->bb_pos++];
    bb->type = type;
    bb->flags = 0;
    bb->prev_cnt = 0;
    bb->block_no = builder->bb_count++;
    /* @Leak
     * This should probably be a memory pool as well */
    bb->ast_array = ptrVecNew();
    bb->visited = 0;
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
    for (int i = 0; i < 64; ++i) {
        BasicBlock *bb = &bb_pool[i];
        bb->type = -1;
    }
    builder->cc = cc;
    builder->bb_count = 1;
    builder->bb_cap = 64;
    builder->bb_pool = bb_pool;
    builder->bb_pos = 0;
    builder->flags = 0;
    builder->bb_cur_loop = NULL;
    builder->bb_cur_else = NULL;

    builder->unresoved_gotos = listNew();
    builder->resolved_labels = dictNew(&default_table_type);
    dictSetFreeKey(builder->resolved_labels,NULL);
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

BasicBlock *cfgGet(CFG *cfg, int block_no) {
    BasicBlock *bb_array = (BasicBlock*)cfg->_memory;
    return &(bb_array[block_no]);
}

int bbPrevHas(BasicBlock *bb, int block_no) {
    int prev_cnt = bb->prev_cnt;
    for (int i = 0; i < prev_cnt; ++i) {
        if (bb->prev_blocks[i]->block_no == block_no) return 1;
    }
    return 0;
}

/* @Optimise */
static void bbAddPrev(BasicBlock *bb, BasicBlock *prev) {
    if (!bbPrevHas(bb,prev->block_no)) {
        bb->prev_blocks[bb->prev_cnt++] = prev;
    }
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
    BasicBlock *current_else = builder->bb_cur_else;
    BasicBlock *bb = builder->bb;
    bb->type = BB_BRANCH_BLOCK;
    ptrVecPush(bb->ast_array,ast->cond);
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

    builder->bb_cur_else = else_body;

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
        if (if_body->type != BB_BREAK_BLOCK) {
            bb->_if->next = else_body;
        }
        cfgBuilderSetBasicBlock(builder,else_body);
    }

    if (!in_conditional) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_CONDITIONAL);
    }
    builder->bb_cur_else = current_else;
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

    if (ast_init) ptrVecPush(bb->ast_array,ast_init);

    bb_cond->flags |= BB_FLAG_LOOP_HEAD;

    bb->next = bb_cond;
    bb_cond->prev = bb;
    /* Jump into loop body if condition is met */
    bb_cond->_if = bb_body;
    /* Else move past loop body */
    bb_cond->_else = bb_cond_else;
    /* And the previously met block was the condition */
    bb_cond_else->prev = bb_cond;
    bb_cond->_if->prev = bb_cond;

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

    /* This is a loop which immediately hit a break; */
    if (builder->bb->type == BB_BREAK_BLOCK && 
        (builder->bb->flags & BB_FLAG_LOOP_END))
    {
        builder->bb->flags |= BB_FLAG_REDUNDANT_LOOP;
        builder->bb->flags &= ~(BB_FLAG_LOOP_END);
        bb_cond->flags &= ~BB_FLAG_LOOP_HEAD;
        bb_cond->flags |= BB_FLAG_REDUNDANT_LOOP;
    }

    bb_cond_else->flags |= BB_FLAG_LOOP_END;
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
    bb_cond_else->flags |= BB_FLAG_LOOP_END;
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

    /* This is a loop which immediately hit a break; */
    if (builder->bb->type == BB_BREAK_BLOCK && 
        (builder->bb->flags & BB_FLAG_LOOP_END))
    {
        builder->bb->flags |= BB_FLAG_REDUNDANT_LOOP;
        builder->bb->flags &= ~(BB_FLAG_LOOP_END);
        bb_cond->flags &= ~BB_FLAG_LOOP_HEAD;
        bb_cond->flags |= BB_FLAG_REDUNDANT_LOOP;
    }

    bb_cond_else->flags |= BB_FLAG_LOOP_END;
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
static void cfgHandleDoWhileLoop(CFGBuilder *builder, Ast *ast) {
    Ast *ast_cond = ast->whilecond;
    Ast *ast_body = ast->whilebody;

    BasicBlock *bb_prev_loop = builder->bb_cur_loop;
    BasicBlock *bb = builder->bb;

    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);
    BasicBlock *bb_do_while = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);

    bb->next = bb_do_while;

    builder->bb_cur_loop = bb_do_while;
    builder->flags |= CFG_BUILDER_FLAG_IN_LOOP;

    cfgBuilderSetBasicBlock(builder,bb_do_while);
    cfgHandleAstNode(builder,ast_body);

    builder->bb->type = BB_DO_WHILE_COND;
    builder->bb->prev = bb_do_while;
    builder->bb->prev->flags |= BB_FLAG_LOOP_HEAD;

    builder->bb->next = bb_cond_else;
    ptrVecPush(builder->bb->ast_array,ast_cond);

    if (!bb_prev_loop) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_LOOP);
    }

    cfgBuilderSetBasicBlock(builder,bb_cond_else);
    builder->bb_cur_loop = bb_prev_loop;
}

static void cfgHandleGoto(CFGBuilder *builder, Ast *ast) {
    BasicBlock *dest;
    aoStr *label = astHackedGetLabel(ast);

    builder->bb->type = BB_GOTO;

    if (builder->flags & CFG_BUILDER_FLAG_IN_LOOP) {
        builder->bb->flags |= BB_FLAG_LOOP_JUMP;
    }

    /* Try straight out the gate to resolve the goto otherwise save it for 
     * later */
    if ((dest = dictGetLen(builder->resolved_labels,label->data,
            label->len)) != NULL)
    {
        /* This forms a loop but over a potentially large number of graph nodes 
         * making it difficult to classify as a _conventional_ loop. 
         * Do we want an intermediary node? */
        builder->bb->prev = dest;
        builder->bb->flags |= BB_FLAG_GOTO_LOOP;
        builder->bb->next = NULL;
    } else {
        listAppend(builder->unresoved_gotos,builder->bb);
    }

    ptrVecPush(builder->bb->ast_array,ast);
    
    /* If this is an unconditional jump */
    if (!(builder->flags & CFG_BUILDER_FLAG_IN_CONDITIONAL)) {
       builder->bb->flags |= BB_FLAG_UNCONDITIONAL_JUMP;
    }
}

static void cfgHandleReturn(CFGBuilder *builder, Ast *ast) {
    BasicBlock *bb = builder->bb;
    BasicBlock *bb_return;

    if (bb->ast_array->size == 0) {
        bb_return = bb;
        bb_return->type = BB_RETURN_BLOCK;
    } else {
        bb_return = cfgBuilderAllocBasicBlock(builder,BB_RETURN_BLOCK);
        bb->next = bb_return;
        bb_return->prev = bb;
    }
    bb_return->next = NULL;
    ptrVecPush(bb_return->ast_array,ast);
    cfgBuilderSetBasicBlock(builder,bb_return);
}

static void cfgHandleLabel(CFGBuilder *builder, Ast *ast) {
    ptrVecPush(builder->bb->ast_array,ast);
    aoStr *label = astHackedGetLabel(ast);
    loggerWarning("l: %s\n",label->data);
    dictSet(builder->resolved_labels,label->data,builder->bb);
    builder->bb->flags |= BB_FLAG_LABEL;
}

static void cfgHandleBreak(CFGBuilder *builder, Ast *ast) {
    if (builder->flags & CFG_BUILDER_FLAG_IN_LOOP) {
        builder->bb->type = BB_BREAK_BLOCK;
        builder->bb->next = builder->bb_cur_loop->_else;
    }
}

static void cfgHandleCompound(CFGBuilder *builder, Ast *ast) {
    if (listEmpty(ast->stms)) return;
    listForEach(ast->stms) {
        Ast *ast_node = cast(Ast *,it->value);
        cfgHandleAstNode(builder,ast_node);
    }
}

static void cfgHandleContinue(CFGBuilder *builder, Ast *ast) {
    assert(builder->flags & CFG_BUILDER_FLAG_IN_LOOP);
    loggerPanic("'CONTINUE' unimplemented\n");
}

static void cfgHandleJump(CFGBuilder *builder, Ast *ast) {
    cfgHandleGoto(builder,ast);
}

static void cfgHandleCase(CFGBuilder *builder, BasicBlock *bb_end, Ast *ast) {
    BasicBlock *bb_switch = builder->bb;
    BasicBlock *bb_case = cfgBuilderAllocBasicBlock(builder,BB_CASE);
    ptrVecPush(bb_switch->next_blocks,bb_case);

    /* so we can access begining and end */
    ptrVecPush(bb_case->ast_array,ast);
    cfgBuilderSetBasicBlock(builder,bb_case);
    bb_case->prev = bb_switch;

    if (!listEmpty(ast->case_asts)) {
        listForEach(ast->case_asts) {
            Ast *case_ast = (Ast *)it->value;
            cfgHandleAstNode(builder,case_ast);
        }
    }

    if (builder->bb != bb_case) {
        builder->bb->flags |= BB_FLAG_CASE_OWNED;
    }

    builder->bb->next = bb_end;
    bb_end->prev = builder->bb;
    bbAddPrev(bb_case,bb_switch);
    bbAddPrev(bb_end,bb_case);
}

/* The first and last items of the next_blocks array are the switch condition 
 * and the next block respectively. This is easier to manage and theoretically
 * are different blocks but connected to the switch */
static void cfgHandleSwitch(CFGBuilder *builder, Ast *ast) {
    BasicBlock *bb_switch = builder->bb;
    // BasicBlock *bb_default = NULL;
    BasicBlock *bb_end  = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);

    ptrVecPush(bb_switch->ast_array,ast->switch_cond);
    //if (ast->case_default) {
    //    bb_default = cfgBuilderAllocBasicBlock(builder,BB_CASE);
    //    bb_default->flags = BB_FLAG_ELSE_BRANCH;
    //}
    bb_switch->type = BB_SWITCH;
    bb_switch->next_blocks = ptrVecNew();

    if (!ast->switch_bounds_checked) {
        bb_switch->flags |= BB_FLAG_UNCONDITIONAL_JUMP;
    }

    /* ??? - This has the potential to be a crazy loop */
    for (int i = 0; i < ast->cases->size; ++i) {
        cfgBuilderSetBasicBlock(builder,bb_switch);
        Ast *_case = (Ast *)ast->cases->entries[i];
        cfgHandleCase(builder,bb_end,_case);
    }

    /* wrong ?*/
    if (ast->case_default != NULL) {
        cfgBuilderSetBasicBlock(builder,bb_switch);
        cfgHandleCase(builder,bb_end,ast->case_default);
    }

    // ptrVecPush(bb_switch->next_blocks,bb_end);
    bb_switch->next = bb_end;
    bb_end->prev = bb_switch;
    cfgBuilderSetBasicBlock(builder,bb_end);
}

static void cfgHandleAstNode(CFGBuilder *builder, Ast *ast) {
    assert(ast != NULL);
    int kind = ast->kind;
    BasicBlock *bb = builder->bb;

    /* We are only interested in the AST nodes that are the start of a new 
     * basic block or a top level `I64 x = 10;` type declaration or assignment 
     * 
     * A new basic block will start with an `if`, `for`, `while`, `goto` */
    switch (kind) {
        case AST_BREAK:         cfgHandleBreak(builder,ast);       break;
        case AST_COMPOUND_STMT: cfgHandleCompound(builder,ast);    break;
        case AST_CONTINUE:      cfgHandleContinue(builder,ast);    break;
        case AST_DO_WHILE:      cfgHandleDoWhileLoop(builder,ast); break;
        case AST_FOR:           cfgHandleForLoop(builder,ast);     break;
        case AST_GOTO:          cfgHandleGoto(builder,ast);        break;
        case AST_IF:            cgfHandleBranchBlock(builder,ast); break;
        case AST_LABEL:         cfgHandleLabel(builder,ast);       break;
        case AST_RETURN:        cfgHandleReturn(builder,ast);      break;
        case AST_WHILE:         cfgHandleWhileLoop(builder,ast);   break;

        case AST_CASE: {
            loggerPanic("Case should be handled by switch function\n");
        }
        case AST_JUMP:         cfgHandleJump(builder,ast);        break;
        case AST_SWITCH:       cfgHandleSwitch(builder,ast);      break;

        case AST_ADDR:
        case AST_FUNC:
        case AST_ASM_FUNCDEF:
        case AST_ASM_FUNC_BIND:
        case AST_ASM_STMT:
        case AST_CAST:
        case AST_CLASS_REF:
        case AST_DEFAULT_PARAM:
        case AST_DEREF:
        case AST_EXTERN_FUNC:
        case AST_FUNPTR:
        case AST_FUN_PROTO:
        case AST_LITERAL:
        case AST_OP_ADD:
        case AST_PLACEHOLDER:
        case AST_STRING:
        case AST_VAR_ARGS:
        case TK_PRE_PLUS_PLUS:
        case TK_PLUS_PLUS:   
        case TK_PRE_MINUS_MINUS:
        case TK_MINUS_MINUS:
        case AST_ARRAY_INIT:
        case AST_ASM_FUNCALL:
        case AST_FUNCALL:
        case AST_FUNPTR_CALL:
        case AST_DECL:
        case AST_LVAR:

        default:
            ptrVecPush(bb->ast_array,ast);
            break;
    }
}

static void bbFix(BasicBlock *bb);
static void bbRelinkBranch(BasicBlock *branch, BasicBlock *block);
static void bbFixBranchBlock(BasicBlock *bb);

static void bbFixLeafNode(BasicBlock *bb) {
    BasicBlock *prev = bb->prev;
    PtrVec *next_array = NULL;

    assert(prev != NULL);

    if (bb->flags & BB_FLAG_CASE_OWNED) return;

    if (bb == bb->next) {
        bb->type = BB_GARBAGE;
        return;
    }
    if (!bb->ast_array || bb->ast_array->size == 0) {
        bb->type = BB_GARBAGE;
        return;
    }

    if (bb->next != NULL) {
        next_array = bb->next->ast_array;
    }

    if (next_array && next_array->size == 0) {
        bb->next->type = BB_GARBAGE;
        if (bb->next->next) {
            bb->next = bb->next->next;
            bbAddPrev(bb->next,bb);
        }
    } 

    /* Iterate back up the tree to find the join */
    while (prev) {
        if (prev->type == BB_BRANCH_BLOCK) {
            BasicBlock *prev_else = prev->_else;

            if (prev_else->type == BB_CONTROL_BLOCK) {
                bb->prev = prev_else->prev;
                bb->next = prev_else->next;
            }

            if (prev_else->type == BB_RETURN_BLOCK ||
                prev_else->type == BB_LOOP_BLOCK)
            {
                bb->prev = prev_else->prev;
                bb->next = prev_else;
                bbAddPrev(prev_else,bb);
                break;
            }

        } else if (prev->type == BB_HEAD_BLOCK) {
            break;
        } else if (prev->type == BB_CONTROL_BLOCK) {
            /* We've found a random control block which indicates 
             * that our current block is genuinely the end of the road*/
            if (prev->next != bb) {
                bb->type = BB_END_BLOCK;// BB_GARBAGE; BB_END_BLOCK;
                bb->next = NULL;
                break;
            }
        } else if (prev->type == BB_LOOP_BLOCK) {
            loggerDebug("we hit a loop\n");
        }

        prev = prev->prev;
    }
}

/* We _know_ that the*/
static void bbRelinkBranch(BasicBlock *branch, BasicBlock *bb) {
    if (!bb) return;
    if (bb->type == BB_GARBAGE) return;

    PtrVec *current_array = bb->ast_array;
    PtrVec *next_array = NULL;
    if (bb->next != NULL) {
        next_array = bb->next->ast_array;
    }
    bbAddPrev(bb,branch);

    if (bb->type == BB_BRANCH_BLOCK) {
        if (next_array) {
            PtrVec *els_array = bb->_else->ast_array;
            if (next_array->size != 0 && els_array->size == 0) {
                /* mark as destroyed */
                bb->_else->type = BB_GARBAGE;
                bb->_else = bb->next;
                bbAddPrev(bb->next,bb);
                bb->next = NULL;
            } else if (next_array->size != 0 && els_array->size != 0) {
                /* This seems to happen if the bb->next is a control block 
                 * where bb is of type branch and its in a loop */
                if (bb->_else->next == bb->_if->next) {
                    if (bb->_else->next->ast_array->size == 0) {
                        bb->_else->next = bb->next;
                        bb->_if->next = bb->next;
                    }
                }
                bbAddPrev(bb,branch);
            } else {
                /* The next node is garbage as it does not point to any thing 
                 * of interest, `if` and `else` have been added correctly */
                bb->next->type = BB_GARBAGE;
            }
        } else {
            bb->next = NULL;
            bbAddPrev(bb,branch);
        }
    } else if (bb->type == BB_CONTROL_BLOCK) {
        if (next_array && next_array->size == 0) {
            if (bb->next) {
                bb->next->type = BB_GARBAGE;
            }
            bbFixLeafNode(bb);
        } else if (current_array && current_array->size == 0) {
            if (bb->flags & BB_FLAG_CASE_OWNED) return;
            if (next_array) {
                bb->type = BB_GARBAGE;
                if (branch->_if == bb) {
                    branch->_if = bb->next;
                    bbAddPrev(branch->_if,branch);
                } else if (branch->_else == bb) {
                    branch->_else = bb->next;
                    bbAddPrev(branch->_else,branch);
                }
            } else {
                if (branch->_if->next->ast_array->size == 0) {
                    branch->_if->next = branch->next;
                }
            }
        }
        /* For some reason we keep getting branches with a next pointer */
        branch->next = NULL;
        bbAddPrev(bb,branch);
    } else if (bb->type == BB_GARBAGE) {
        loggerWarning("we hit the trash\n");
    }
}

static void bbFixBranchBlock(BasicBlock *bb) {
    BasicBlock *then = bb->_if, *els = bb->_else;

    bbRelinkBranch(bb,els);
    bbFix(els);
    els->flags |= BB_FLAG_ELSE_BRANCH;

    bbRelinkBranch(bb,then);
    bbFix(then);
    then->flags |= BB_FLAG_IF_BRANCH;
}

static void bbFixLoopBlock(BasicBlock *bb) {
    if (bb->ast_array->size == 0) {
        if (bb->prev_cnt == 1) {
            BasicBlock *prev = bb->prev_blocks[0];

            if (prev->type == BB_BRANCH_BLOCK) {

                if (prev->_else == bb) {
                    prev->_else = NULL;
                    prev->next = prev->_if;
                } else {
                    prev->_if = NULL;
                    prev->next = prev->_else;
                }

                BasicBlock *do_while = prev;

                do_while->type = BB_DO_WHILE_COND;
                do_while->prev = bb->prev;
                do_while->prev->flags |= (BB_FLAG_LOOP_HEAD);
                do_while->prev->prev = bb;

                bbAddPrev(prev,prev->prev);

                bb->type = BB_GARBAGE;
            }
        }
    } else {
        if (bb->prev->type == BB_DO_WHILE_COND) {
            loggerWarning("How have we ended up here? %d \n",
                    bb->block_no);
            bb->prev->flags &= ~BB_FLAG_LOOP_END;
            bb->flags |= BB_FLAG_LOOP_END;
        }
    }
}

static void bbFix(BasicBlock *bb) {
    for (; bb; bb = bb->next) {
        /* Search up the tree to fix the node */
        if (bb->type == BB_CONTROL_BLOCK || bb->type == BB_CASE) {
            bbFixLeafNode(bb);
        }

        /* Iterate back up the tree to find the join */
        if (bb->type == BB_BRANCH_BLOCK) {
            bbFixBranchBlock(bb);
        } else if (bb->type == BB_LOOP_BLOCK) {
            bbFixLoopBlock(bb);
            // bbAddPrev(bb,bb->prev);
        } else if (bb->type == BB_BREAK_BLOCK) {
            /* Trace back up to the loop head and find the else branch */
        } else if (bb->type == BB_DO_WHILE_COND) {
            bbAddPrev(bb->prev,bb);
            /* This is true in a traditional DO_WHILE, I think we need a 
             * new block type as sometimes the do while can be the loop head 
             * and a the previous be another loop */
            if (!(bb->flags & BB_FLAG_LOOP_HEAD)) {
                bb->next->flags |= BB_FLAG_LOOP_END;
            }
            bb->prev->prev = bb;
        } else if (bb->type == BB_SWITCH) {
            for (int i = 0; i < bb->next_blocks->size; ++i) {
                BasicBlock *it = vecGet(BasicBlock *,bb->next_blocks,i);
                bbFix(it);
            }
        }

        if (bb->flags & BB_FLAG_GOTO_LOOP) {
            continue;
        }

        if (bb->type != BB_GARBAGE && bb->next && bb->next->ast_array->size > 0) {
            bbAddPrev(bb->next,bb);
        }
    }
}

/* a lot of the code in this function is absolutely ridiculous, as 
 * what you can do with goto is absolutely ridiculous */
static void cfgRelocateGoto(CFGBuilder *builder, BasicBlock *bb_goto,
        BasicBlock *bb_dest, aoStr *goto_label)
{
    /* Can happen... if the block we want to goto is itself */
    if (bb_goto == bb_dest) {
        return;
    }

    BasicBlock *bb_prev = bb_goto->prev;

    if (bb_dest->type == BB_LOOP_BLOCK && !(bb_goto->flags & BB_FLAG_LOOP_JUMP)) {
        if (bb_prev->type == BB_BRANCH_BLOCK) {
            if (bb_prev->_if == bb_goto) bb_goto->flags |= BB_FLAG_IF_BRANCH;
            else                         bb_goto->flags |= BB_FLAG_ELSE_BRANCH;
        }

        PtrVec *dest_ast_array = bb_dest->ast_array;
        BasicBlock *loop_cond = bb_dest->prev;
        BasicBlock *loop_back = cfgBuilderAllocBasicBlock(builder,BB_LOOP_BLOCK);

        Ast *asts_to_move[100];
        int ast_move_cnt = 0;

        /* We need to get the statements that happened before the 
         * label */
        for (int i = 0; i < dest_ast_array->size; ++i) {
            Ast *needle = dest_ast_array->entries[i];
            if (needle->kind == AST_LABEL && aoStrCmp(goto_label,
                        astHackedGetLabel(needle))) {
                break;
            } else {
                asts_to_move[ast_move_cnt++] = needle;
            }
        }

        /* Add asts from the loop_cond */
        for (int i = 0; i < ast_move_cnt; ++i) {
            ptrVecPush(loop_back->ast_array,asts_to_move[i]);
        }

        /* resize the array */
        dest_ast_array->size -= ast_move_cnt;

        /* Remove asts that no longer exist in the block */
        for (int i = 0; i < dest_ast_array->size; ++i) {
            dest_ast_array->entries[i] = dest_ast_array->entries[ast_move_cnt+i];
        }


        /* Obvious, the goto has to point to it's destination */
        bb_goto->next = bb_dest;

        /* Destination, whatever it might be, is the loop head 
         * @Test - how does this work if we jump into an if branch?*/
        bb_dest->next = loop_cond;
        bb_dest->prev = bb_goto;
        bb_dest->flags = BB_FLAG_LOOP_HEAD;
        bb_dest->type = BB_CONTROL_BLOCK;

        loop_back->prev = bb_dest;

        /* the loop condition is now after the goto's destination */
        loop_cond->_if = loop_back;
        loop_cond->_else->flags = BB_FLAG_LOOP_END;
        loop_cond->type = BB_BRANCH_BLOCK;
        loop_cond->flags = 0;

        bbAddPrev(loop_back,loop_cond);
        bbAddPrev(loop_back,bb_dest);
        bbAddPrev(bb_dest,loop_back);
    } else {
        bbAddPrev(bb_dest,bb_goto);

        /* Making a backwards goto loop */
        if (bb_goto->flags & BB_FLAG_GOTO_LOOP) bb_goto->prev = bb_dest;
        else                                    bb_goto->next = bb_dest;

        /* We don't want to destroy the loop */
        if (bb_dest->type != BB_LOOP_BLOCK && 
             !(bb_dest->flags & BB_FLAG_GOTO_LOOP)) {
            bb_dest->prev = bb_goto;
        }
    }
}

/* Split out to make it simpler to reason with */
static void cfgConstructFunction(CFGBuilder *builder, List *stmts) {
    if (listEmpty(stmts)) return;
    builder->ast_list = stmts;
    listForEach(stmts) {
        Ast *ast = cast(Ast *,it->value);
        builder->ast_iter = it;
        cfgHandleAstNode(builder,ast);
    }

    BasicBlock *bb = &builder->bb_pool[0];
    bbFix(bb);
    /* Reconcile any non linear jumps */
    listForEach(builder->unresoved_gotos) {
        BasicBlock *bb_goto = cast(BasicBlock *,it->value);
        BasicBlock *bb_dest = NULL;
        Ast *ast = NULL; 

        for (int i = 0; i < bb_goto->ast_array->size; ++i) {
            ast = (Ast *)bb_goto->ast_array->entries[i];
            if (ast->kind == AST_GOTO || ast->kind == AST_JUMP) {
                break;
            }
        }

        aoStr *goto_label = astHackedGetLabel(ast);
        aoStr *dest_label = astHackedGetLabel(ast);

        assert(ast->kind == AST_GOTO || ast->kind == AST_JUMP);

        bb_dest = dictGetLen(builder->resolved_labels,dest_label->data,
                dest_label->len);

        assert(bb_dest != NULL);
        cfgRelocateGoto(builder,bb_goto,bb_dest,goto_label);
    }

    /* We don't need the memory... do we want to free this now or later? */
    for (int i = 0; i < builder->bb_count; ++i) {
        BasicBlock *_bb = &builder->bb_pool[i];
        if (_bb->type == BB_GARBAGE) {
            ptrVecRelease(_bb->ast_array);
        }
    }

    /* We still want these structures but don't want their contents for the 
     * next functions. */
    dictClear(builder->resolved_labels);
    listClear(builder->unresoved_gotos,NULL);
}

void cfgSCCUtil(BasicBlock *bb, int *disc, int *low, List *st, 
        int *stack_member)
{
    static int time = 0;

    disc[bb->block_no] = low[bb->block_no] = ++time;
    listAppend(st,bb);
    stack_member[bb->block_no] = 1;
    BasicBlock *adjacent[4] = {bb->next, bb->prev, bb->_if, bb->_else};
    if (bb->type != BB_LOOP_BLOCK && bb->type != BB_DO_WHILE_COND) {
        adjacent[1] = NULL;
    }

    for (int i = 0; i < 4; ++i) {
        BasicBlock *v = adjacent[i];
        if (!v) continue;

        if (disc[v->block_no] == -1) {
            cfgSCCUtil(v,disc,low,st,stack_member);
            low[bb->block_no] = min(low[bb->block_no],low[v->block_no]);
        } else if (stack_member[v->block_no] == 1) {
            low[bb->block_no] = min(low[bb->block_no],disc[v->block_no]);
        } 
    }

    if (low[bb->block_no] == disc[bb->block_no]) {
        BasicBlock *it;
        while (listHead(st) != bb) {
            it = listPop(st);
            if (!it) break;
            printf("%d ", it->block_no);
            stack_member[it->block_no] = 0;
        }
        it = listPop(st);
        if (it) {
            printf("%d ", it->block_no);
            stack_member[it->block_no] = 0;
        }
    }
}

void cfgGetStronglyConnectedComponents(CFG *cfg) {
    int *disc = cast(int *,malloc(sizeof(int) * cfg->bb_count));
    int *low = cast(int *,malloc(sizeof(int) * cfg->bb_count));
    int *stack_member = cast(int *,malloc(sizeof(int) * cfg->bb_count));
    List *st = listNew();

    for (int i = 0; i < cfg->bb_count; ++i) {
        disc[i] = low[i] = -1;
        stack_member[i] = 0;
    }

    BasicBlock *bb_array = cast(BasicBlock *, cfg->_memory);

    for (int i = 0; i < cfg->bb_count; ++i) {
        BasicBlock *bb = &(bb_array[i]);
        if (disc[i] == -1 && bb->type != BB_GARBAGE) {
            cfgSCCUtil(bb,disc,low,st,stack_member);
        }
    }
}

/* Print the Adjacency list graph representation */
void cfgAdjacencyListPrint(IntMap *map) {
    long *index_entries = map->indexes;

    for (int i = 0; i < map->size; ++i) {
        long idx = index_entries[i];
        IntVec *vec = cast(IntVec *, map->entries[idx]->value);
        printf("bb%ld\n",idx);

        for (int i = 0; i < vec->size; ++i) {
            long hash = vec->entries[i];
            long block_no = hash & 0x7FF;
            long type = hash >> 11 & 0x1F;
            long flags = hash >> 16;
            printf("      %ld\n", block_no);
            printf("    bb%2ld type = %-*s flags = %-*s\n",block_no,
                18,
                bbTypeToString(type),
                40,
                bbFlagsToString(flags));
        }
        printf("====\n");
    }
}

/** 
 * Information of a basic block packed into one `long`
 *
 * flags 16                type 5   blockno 11bits
 * +---------------------+--------+---------------+
 * | 0000_0000_0000_0000 | 0_0000 | 000_0000_0000 |
 * +---------------------+--------+---------------+
 */
long cfgHashBasicBlock(BasicBlock *bb) {
    long hash = 0;
    hash |= bb->flags << 16;
    hash |= bb->type << 11;
    hash |= bb->block_no;
    return hash;
}

/**
 * This is for creating a very compact control flow graph that feels more like 
 * a classical datascructure than the basic block node graph. It's significantly 
 * faster to traverse and also easier to reason with.
 *
 * This also fixes orphaned nodes, so is acting like a final check before 
 * handing off to the next step.
 * */
IntMap *cfgBuildAdjacencyList(CFG *cfg) {
    IntMap *map = intMapNew(32);
    BasicBlock *bb_array = ((BasicBlock *)cfg->_memory);
    long hash = 0;
    IntVec *vec;

    for (int i = 0; i < cfg->bb_count; ++i) {
        BasicBlock *bb = &(bb_array[i]);
        if (bb->type == BB_GARBAGE) continue;

        /* The order of _if _else and prev/next is of PARAMOUNT importance */
        switch (bb->type) {
            case BB_BREAK_BLOCK:
            case BB_RETURN_BLOCK:
            case BB_END_BLOCK: {
                vec = intVecNew();
                intVecPush(vec,hash);
                break;
            }

            case BB_CONTROL_BLOCK:
                /* This is a check for an orphaned node */
                if (!(bb->flags & BB_FLAG_CASE_OWNED) && 
                        (bb->ast_array->size == 0 || bb->next == NULL)) {
                    bb->type = BB_GARBAGE;
                    continue;
                }
            case BB_HEAD_BLOCK:
            case BB_CASE:
            case BB_GOTO:
                vec = intVecNew();
                hash = cfgHashBasicBlock(bb->next);
                intVecPush(vec,hash);
                break;

            case BB_BRANCH_BLOCK:
                vec = intVecNew();
                hash = cfgHashBasicBlock(bb->_if);
                intVecPush(vec,hash);
                hash = cfgHashBasicBlock(bb->_else);
                intVecPush(vec,hash);
                break;

            /* Theoretically also want to do the cases as they could have 
             * nested blocks. Can accept this as a limitation for now */
            case BB_SWITCH: {
                vec = intVecNew();
                for (int i = 0; i < bb->next_blocks->size; ++i) {
                    BasicBlock *bb_case = (BasicBlock *)bb->next_blocks->entries[i];
                    hash = cfgHashBasicBlock(bb_case);
                    intVecPush(vec,hash);
                }
                break;
            }

            case BB_LOOP_BLOCK: {
                BasicBlock *loop_head = bb->prev;
                if (bb->prev_cnt == 0 && loop_head->_if != bb && 
                        loop_head->_else != bb) {
                    bb->type = BB_GARBAGE;
                    continue;
                }

                vec = intVecNew();
                hash = cfgHashBasicBlock(bb->prev);
                intVecPush(vec,hash);
                break;
            }

            case BB_DO_WHILE_COND:
                vec = intVecNew();
                hash = cfgHashBasicBlock(bb->prev);
                intVecPush(vec,hash);
                hash = cfgHashBasicBlock(bb->next);
                intVecPush(vec,hash);
                break;

            default:
                loggerPanic("Unhandled type: %s\n", bbTypeToString(bb->type));
        }
        intMapSet(map,bb->block_no,vec);
    }
    return map;
}

/* This will need to return a list of CFG's */
CFG *cfgConstruct(Cctrl *cc) {
    Ast *ast;
    CFG *cfg;
    CFGBuilder builder;
    BasicBlock *bb;

    cfgBuilderInit(&builder,cc);

    listForEach(cc->ast_list) {
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
            cfg->graph = cfgBuildAdjacencyList(cfg);
        }
    }

 //   cfgAdjacencyListPrint(cfg->graph);
 //   cfgGetStronglyConnectedComponents(cfg);
    dictRelease(builder.resolved_labels);
    listRelease(builder.unresoved_gotos,NULL);

    return cfg;
}
