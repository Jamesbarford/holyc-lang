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
static void cfgLinkLeaves(IntMap *map, BasicBlock *bb, BasicBlock *dest);

#define cfgIsLoopControl(bb) \
    (((bb)->type == BB_CONTINUE || (bb)->type == BB_BREAK_BLOCK))

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
        case BB_CONTINUE:           return "BB_CONTINUE";
        case BB_DO_WHILE_HEAD:      return "BB_DO_WHILE_HEAD";
        default:
            loggerPanic("Unknown type: %d\n", type);
    }
}

#ifdef DEBUG
void cfgPrintAstArray(BasicBlock *bb) {
    if (bb->ast_array->size == 0) {
        printf("(null)\n");
    } else {
        for (int i = 0; i < bb->ast_array->size; ++i) {
            printf("%d: \n", i);
            astPrint(bb->ast_array->entries[i]);
        }
    }
}
#endif

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
    if (flags & BB_FLAG_WHILE_LOOP) {
        _concat_flag("BB_FLAG_WHILE_LOOP");
    }
    if (flags & BB_FLAG_CASE_BREAK) {
        _concat_flag("BB_FLAG_CASE_BREAK");
    }
    aoStrPutChar(str,')');

#undef _concat_flag

    return aoStrMove(str); 
}

char *bbPreviousBlockNumbersToString(BasicBlock *bb) {
    aoStr *str = aoStrNew();
    aoStrCatPrintf(str,"prev_cnt = %d: ",bb->prev_cnt);
    for (int i = 0; i < bb->prev_cnt; ++i) {
        aoStrCatPrintf(str,"bb%d",bb->prev_blocks[i]->block_no);
        if (i + 1 != bb->prev_cnt) aoStrCatPrintf(str,", ");
    }
    return aoStrMove(str);
}

#define BB_FMT_TYPE_CHARS 18
#define BB_FMT_FLAG_CHARS 40
char *bbToString(BasicBlock *bb) {
    aoStr *str = aoStrNew();
    char *str_flags = bbFlagsToString(bb->flags);
    char *str_type =  bbTypeToString(bb->type);
    char *str_prev =  bbPreviousBlockNumbersToString(bb);

    aoStrCatPrintf(str,"bb%d type = %-*s, flags = %-*s, %s, ",
            bb->block_no,
            BB_FMT_TYPE_CHARS,
            str_type,
            BB_FMT_FLAG_CHARS,
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

char *bbToJSON(BasicBlock *bb) {
    aoStr *str = aoStrNew();
    char *str_flags = bbFlagsToString(bb->flags);
    char *str_type =  bbTypeToString(bb->type);
    char *str_prev =  bbPreviousBlockNumbersToString(bb);
    char *str_prev_ptr = NULL;

    if (bb->prev) {
        str_prev_ptr = mprintf("bb%d", bb->prev->block_no);
    }

    aoStrCat(str,"{\n");
    aoStrCatPrintf(str,"    \"block_no\": \"bb%d\",\n",bb->block_no);
    aoStrCatPrintf(str,"    \"type\": \"%s\",\n", str_type);
    aoStrCatPrintf(str,"    \"flags\": \"%s\",\n", str_flags);
    aoStrCatPrintf(str,"    \"prev_nodes\": [%s],\n", str_prev);
    aoStrCatPrintf(str,"    \"prev_ptr\": \"%s\",\n", str_prev_ptr ? str_prev_ptr : "(null)");
    if (bb->type == BB_BRANCH_BLOCK) {
        if (bb->_if)   aoStrCatPrintf(str,"    \"if\": \"bb%d\",\n",bb->_if->block_no);
        if (bb->_else) aoStrCatPrintf(str,"    \"else\": \"bb%d\"\n",bb->_else->block_no);
    } else {
        if (bb->next) aoStrCatPrintf(str,"    \"next\": \"bb%d\"\n",bb->next->block_no);
    }
    if (str_prev_ptr) {
        free(str_prev_ptr);
    }

    free((char*)str_flags);
    free((char*)str_prev);
    aoStrCat(str,"}\n");
    return aoStrMove(str);
}

void bbPrintNoAst(BasicBlock *bb) {
    char *bb_string = bbToString(bb);
    printf("%s\n",bb_string);
    free(bb_string);
}

void bbPrint(BasicBlock *bb) {
    assert(bb != NULL);
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
    bb->idx = builder->bb_count;
    bb->type = type;
    bb->flags = 0;
    bb->prev_cnt = 0;
    bb->block_no = builder->bb_block_no++;
    builder->bb_count++;
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

#define CFG_BUILDER_MAX_MEMORY 512
static void cfgBuilderInit(CFGBuilder *builder, Cctrl *cc, IntMap *leaf_cache, int block_no) {
    BasicBlock *bb_pool = cast(BasicBlock *, malloc(sizeof(BasicBlock)*CFG_BUILDER_MAX_MEMORY));
    for (int i = 0; i < CFG_BUILDER_MAX_MEMORY; ++i) {
        BasicBlock *bb = &bb_pool[i];
        bb->type = -1;
    }
    builder->cc = cc;
    builder->bb_block_no = block_no;
    builder->bb_count = 1;
    builder->bb_cap = CFG_BUILDER_MAX_MEMORY;
    builder->bb_pool = bb_pool;
    builder->bb_pos = 0;
    builder->flags = 0;
    builder->bb_cur_loop = NULL;
    builder->bb_cur_else = NULL;
    builder->leaf_cache = leaf_cache;

    builder->unresoved_gotos = listNew();
    builder->resolved_labels = dictNew(&default_table_type);
    dictSetFreeKey(builder->resolved_labels,NULL);
}

static void cfgBuilderSetBasicBlock(CFGBuilder *builder, BasicBlock *bb) {
    builder->bb = bb;
}

static CFG *cfgNew(aoStr *fname, BasicBlock *head_block) {
    CFG *cfg = (CFG *)malloc(sizeof(CFG));
    cfg->head = head_block;
    cfg->ref_fname = fname;
    cfg->no_to_block = intMapNew(32);
    return cfg;
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

static BasicBlock *cfgSelectOrCreateBlock(CFGBuilder *builder, int type) {
    BasicBlock *bb_selected = NULL;
    if (builder->bb->ast_array->size == 0 && !cfgIsLoopControl(builder->bb)) {
        bb_selected = builder->bb;
        bb_selected->type = type;
    } else {
        bb_selected = cfgBuilderAllocBasicBlock(builder,type);
        bb_selected->prev = builder->bb;
        builder->bb->next = bb_selected;
    }
    return bb_selected;
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
    
    BasicBlock *bb = cfgSelectOrCreateBlock(builder,BB_BRANCH_BLOCK);

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
        if (builder->bb->type != BB_CONTINUE && builder->bb != else_body) {
            builder->bb->prev = bb;
            /* Join */ 
            bb->next = builder->bb;
        } else {
            new_block = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);
            bb->next = new_block;
            new_block->prev = else_body;
            cfgBuilderSetBasicBlock(builder,new_block);
        }
    } else {
        if (builder->bb != if_body) {
            bb->next = builder->bb;
        }
        if (builder->bb->type != BB_CONTINUE) {
            builder->bb->prev = bb;
        }

        int is_break = if_body->type != BB_BREAK_BLOCK && if_body->type != BB_CONTINUE;
        int is_return = if_body->next && if_body->next->type != BB_RETURN_BLOCK;

        if (!is_break && !is_return) {
            bb->next = else_body;
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

    BasicBlock *bb_cond = NULL;
    BasicBlock *bb_init = NULL;

    if (ast_init) {
        /* This is straight line code, there is no need to allocate or do 
         * any basic block manipulation*/
        if (builder->bb->type == BB_CONTROL_BLOCK) {
            bb_init = builder->bb;
        } else {
            bb_init = cfgSelectOrCreateBlock(builder,BB_CONTROL_BLOCK);
            builder->bb = bb_init;
        }
        ptrVecPush(bb_init->ast_array,ast_init);
    }

    bb_cond = cfgSelectOrCreateBlock(builder,BB_BRANCH_BLOCK);
    if (ast_cond == NULL) {
        ast_cond = astMakeForeverSentinal();
    }

    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,
                                                         BB_CONTROL_BLOCK);
    /* Body may or may not be a Loop, we correct it in the if condition with
     * builder->bb != bb_body */
    BasicBlock *bb_body = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);

    bb_cond->flags |= BB_FLAG_LOOP_HEAD;

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

    /* @Wrong, the step should exist on a continue */
    if (!cfgIsLoopControl(bb_body) && ast_step) {
        cfgHandleAstNode(builder,ast_step);
    }

    /* This means we had branches or possibly inner loops in our loop */
    if (builder->bb != bb_body || !builder->bb->next) {
        builder->bb->type = BB_LOOP_BLOCK;
        builder->bb->prev = bb_cond;
        /* So we can access the loop block from the head of the condition */
        bb_cond->next = builder->bb;
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
    
    BasicBlock *bb = builder->bb;
    BasicBlock *bb_prev_loop = builder->bb_cur_loop;
    BasicBlock *bb_cond = cfgSelectOrCreateBlock(builder,BB_BRANCH_BLOCK);
    BasicBlock *bb_body = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);
    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,
                                                         BB_CONTROL_BLOCK);

    bb_cond->flags |= (BB_FLAG_LOOP_HEAD|BB_FLAG_WHILE_LOOP);
    bb->next = bb_cond;

    /* Jump into loop body if condition is met */
    bb_cond->_if = bb_body;
    bb_cond->_if->flags |= (BB_FLAG_IF_BRANCH);
    /* Else move past loop body */
    bb_cond->_else = bb_cond_else;
    bb_cond_else->flags |= (BB_FLAG_LOOP_END|BB_FLAG_ELSE_BRANCH);
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
    if (ast_body) {
        cfgHandleAstNode(builder,ast_body);
    }

    /* This means we had branches or possibly inner loops in our loop */
    if (builder->bb != bb_body || !builder->bb->next) {
        builder->bb->type = BB_LOOP_BLOCK;
        if (builder->bb->ast_array->size == 0) {
            ptrVecPush(builder->bb->ast_array, ast_loop_sentinal);
        }
        /* So we can access the loop block from the head of the condition */
        bb_cond->next = builder->bb;
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

    cfgBuilderSetBasicBlock(builder,bb_cond_else);

    if (!bb_prev_loop) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_LOOP);
    }

    bb_cond_else->flags |= BB_FLAG_LOOP_END;
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
    BasicBlock *bb_do_body = cfgSelectOrCreateBlock(builder,BB_CONTROL_BLOCK);

    /*
     * do {
     *    ...               <--+
     *    if (x) {             |
     *      "something\n";     |
     *    }                    |
     * } while (...); <--------+ is a branch
     *
     * */
    bb_do_body->flags |= BB_FLAG_LOOP_HEAD;
    builder->bb_cur_loop = bb_do_body;
    builder->flags |= CFG_BUILDER_FLAG_IN_LOOP;

    cfgBuilderSetBasicBlock(builder,bb_do_body);
    cfgHandleAstNode(builder,ast_body);

    /* converge all orphans in the do while body to point to the node in the 
     * while(...) */
    BasicBlock *bb_do_cond = cfgSelectOrCreateBlock(builder,BB_DO_WHILE_COND);
    IntMap *leaf_cache = builder->leaf_cache;
    cfgLinkLeaves(leaf_cache,builder->bb,bb_do_cond);
    intMapClear(leaf_cache);

    // bbAddPrev(bb_do_cond,builder->bb);
    bb_do_cond->prev = bb_do_body;
    bb_do_cond->flags |= BB_FLAG_LOOP_END;

    ptrVecPush(bb_do_cond->ast_array,ast_cond);

    if (!bb_prev_loop) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_LOOP);
    }

    cfgBuilderSetBasicBlock(builder,bb_do_cond);
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
    BasicBlock *bb_return = cfgSelectOrCreateBlock(builder,BB_RETURN_BLOCK);
    bb_return->next = NULL;
    ptrVecPush(bb_return->ast_array,ast);
    if (ast->retval) {
        ptrVecPush(bb_return->ast_array,ast->retval);
    }
    BasicBlock *bb_after = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);
    cfgBuilderSetBasicBlock(builder,bb_after);
}

static void cfgHandleLabel(CFGBuilder *builder, Ast *ast) {
    ptrVecPush(builder->bb->ast_array,ast);
    aoStr *label = astHackedGetLabel(ast);
    dictSet(builder->resolved_labels,label->data,builder->bb);
    builder->bb->flags |= BB_FLAG_LABEL;
}

static void cfgHandleBreak(CFGBuilder *builder, Ast *ast) {
    if (builder->flags & CFG_BUILDER_FLAG_IN_LOOP && 
            !(builder->flags & CFG_BUILDER_FLAG_IN_SWITCH)) {
        // loggerWarning("breaking loop: %d\n", (builder->flags & CFG_BUILDER_FLAG_IN_SWITCH));
        builder->bb->type = BB_BREAK_BLOCK;
        builder->bb->next = builder->bb_cur_loop->_else;
    } else {
        builder->bb->flags |= BB_FLAG_CASE_BREAK;
        
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
    BasicBlock *bb_continue = cfgSelectOrCreateBlock(builder,BB_CONTINUE);
    bb_continue->prev = builder->bb_cur_loop;
    bbAddPrev(builder->bb_cur_loop,builder->bb);
    if (bb_continue != builder->bb) {
        loggerWarning("On a different node. builder bb%d and continue bb%d\n",
                builder->bb->block_no, bb_continue->block_no);
        builder->bb->next = bb_continue;
    }
    cfgBuilderSetBasicBlock(builder,bb_continue);
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

    bb_end->prev = builder->bb;
    builder->bb->next = bb_end;
    bbAddPrev(bb_case,bb_switch);
}

/* The first and last items of the next_blocks array are the switch condition 
 * and the next block respectively. This is easier to manage and theoretically
 * are different blocks but connected to the switch */
static void cfgHandleSwitch(CFGBuilder *builder, Ast *ast) {
    BasicBlock *bb_switch = cfgSelectOrCreateBlock(builder,BB_SWITCH);
    BasicBlock *bb_end = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);
    int sp = 0;
    int prev_in_switch = builder->flags & CFG_BUILDER_FLAG_IN_SWITCH;
    static BasicBlock *bb_stack[128];

    ptrVecPush(bb_switch->ast_array,ast->switch_cond);
    bb_switch->next_blocks = ptrVecNew();

    if (!ast->switch_bounds_checked) {
        bb_switch->flags |= BB_FLAG_UNCONDITIONAL_JUMP;
    }
    builder->flags |= CFG_BUILDER_FLAG_IN_SWITCH;

    /* ??? - This has the potential to be a crazy loop */
    for (int i = 0; i < ast->cases->size; ++i) {
        cfgBuilderSetBasicBlock(builder,bb_switch);
        Ast *_case = (Ast *)ast->cases->entries[i];
        cfgHandleCase(builder,bb_end,_case);

        if (builder->bb->ast_array->size == 1) {
            bb_stack[sp++] = builder->bb;
        } else if (sp) {
            for (int i = 0; i < sp; i++) {
                BasicBlock *stackable = bb_stack[i];
                stackable->next = builder->bb;
            }
            sp = 0;
        }
    }

    /* wrong ?*/
    if (ast->case_default != NULL) {
        cfgBuilderSetBasicBlock(builder,bb_switch);
        cfgHandleCase(builder,bb_end,ast->case_default);
    }

    IntMap *leaf_cache = builder->leaf_cache;
    cfgLinkLeaves(leaf_cache,bb_switch,bb_end);
    intMapClear(leaf_cache);

    // ptrVecPush(bb_switch->next_blocks,bb_end);
    bb_switch->next = bb_end;
    bb_end->prev = bb_switch;

    cfgBuilderSetBasicBlock(builder,bb_end);
    if (!prev_in_switch) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_SWITCH);
    } 
}

/* Top down through the tree finding blocks which are not pointing to anything */
static void cfgLinkLeaves(IntMap *map, BasicBlock *bb, BasicBlock *dest) {
    if (!intMapHas(map,bb->block_no)) {
        intMapSet(map,bb->block_no,NULL);
        switch (bb->type) {
            case BB_SWITCH:
                for (int i = 0; i < bb->next_blocks->size; ++i) {
                    cfgLinkLeaves(map,bb->next_blocks->entries[i],dest);
                } 
            case BB_CASE:
            case BB_CONTROL_BLOCK:
                if      (bb == bb->next)   bb->next = dest;
                else if (bb->next == NULL) bb->next = dest;
                else if (bb->next->ast_array->size == 0 && !cfgIsLoopControl(bb->next)) bb->next = dest;
                else cfgLinkLeaves(map,bb->next,dest);
                break;

            case BB_BRANCH_BLOCK:
                cfgLinkLeaves(map,bb->_if, dest);
                cfgLinkLeaves(map,bb->_else, dest);
                break;
        }
    }
}

static BasicBlock *cfgMergeBranches(CFGBuilder *builder, BasicBlock *pre,
        BasicBlock *post)
{ 
    IntMap *leaf_cache = builder->leaf_cache;
    BasicBlock *join = post;

    if (post->ast_array->size > 0 && !cfgIsLoopControl(post)) {
        join = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);
        join->prev = post;
        // if (post->next && !cfgIsLoopControl(post->next)) { 
        post->next = join;
        //}
    }
    cfgLinkLeaves(leaf_cache,pre,join);
    intMapClear(leaf_cache);
    return join;
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
        case AST_BREAK:         cfgHandleBreak(builder,ast);    break;
        case AST_COMPOUND_STMT: cfgHandleCompound(builder,ast); break;
        case AST_CONTINUE:      cfgHandleContinue(builder,ast); break;
        case AST_DO_WHILE: {
            cfgHandleDoWhileLoop(builder,ast);
            break;
        }

        case AST_FOR: {
            BasicBlock *pre = builder->bb;
            cfgHandleForLoop(builder,ast);
            /* this is the loop back */
            if (pre->type != BB_BRANCH_BLOCK) {
                pre = pre->next;
            }
            BasicBlock *post = builder->bb;
            BasicBlock *join = cfgMergeBranches(builder,pre,post);
            cfgBuilderSetBasicBlock(builder,join);
            break;
        }

        case AST_WHILE: {
            BasicBlock *pre = builder->bb;
            cfgHandleWhileLoop(builder,ast);
            BasicBlock *post = builder->bb;
            if (pre->type != BB_BRANCH_BLOCK) {
                pre = pre->next;
            }
            BasicBlock *join = cfgMergeBranches(builder,pre,post);
            cfgBuilderSetBasicBlock(builder,join);
            break;
        }

        case AST_IF: {
            BasicBlock *pre = builder->bb;
            cgfHandleBranchBlock(builder,ast);
            BasicBlock *post;
            if (pre->type != BB_BRANCH_BLOCK) {
                pre = pre->next;
            }
            post = builder->bb;
            BasicBlock *join = cfgMergeBranches(builder,pre,post);
            cfgBuilderSetBasicBlock(builder,join);
            break;
        }

        case AST_SWITCH: {
            cfgHandleSwitch(builder,ast);
            break;
        }

        case AST_GOTO:         cfgHandleGoto(builder,ast);   break;
        case AST_LABEL:        cfgHandleLabel(builder,ast);  break;
        case AST_RETURN:       cfgHandleReturn(builder,ast); break;
        case AST_JUMP:         cfgHandleGoto(builder,ast);   break;

        case AST_CASE: {
            loggerPanic("Case should be handled by switch function\n");
        }

        /* Should never hit this */
        case AST_FUNC: break;

        case AST_ADDR:
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
    } else if (bb_goto->type == BB_LOOP_BLOCK) {
        BasicBlock *bb_loop_head = bb_goto->prev;
        bbAddPrev(bb_dest,bb_goto);
        bb_goto->type = BB_CONTROL_BLOCK;
        bb_goto->next = bb_dest;
        bb_goto->flags &= ~(BB_FLAG_LOOP_END);
        bb_loop_head->flags &= ~(BB_FLAG_LOOP_HEAD);
        bb_loop_head->_else->flags &= ~(BB_FLAG_LOOP_END);
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

static void cfgAdjacencyPrintBlock(BasicBlock *bb, int is_parent) {
    char *bb_string = bbToJSON(bb);
    if (is_parent) printf("%s\n",bb_string);
    else           printf("child %s\n",bb_string);
}

BasicBlock *cfgVecFindBlock(PtrVec *vec, int block_no, int *_idx) {
    for (int i = 0; i < vec->size; ++i) {
        BasicBlock *bb = vecGet(BasicBlock *,vec,i);
        if (bb->block_no == block_no) {
            *_idx = i;
            return bb;
        }
    }
    *_idx = -1;
    return NULL;
}

/* Print the Adjacency list graph representation */
void cfgAdjacencyListPrint(CFG *cfg) {
    IntMap *map = cfg->graph;
    IntMap *no_to_block = cfg->no_to_block;
    long *index_entries = map->indexes;

    for (int i = 0; i < map->size; ++i) {
        long idx = index_entries[i];
        BasicBlock *parent = (BasicBlock *)intMapGet(no_to_block,idx);
        PtrVec *vec = (PtrVec *)intMapGet(map,idx);
        cfgAdjacencyPrintBlock(parent,1);

        for (int i = 0; i < vec->size; ++i) {
            BasicBlock *bb = vecGet(BasicBlock *,vec,i);
            cfgAdjacencyPrintBlock(bb,0);
        }
        printf("====\n");
    }
}

/* Print the Adjacency list graph representation */
void cfgAdjacencyListPrintShallow(CFG *cfg) {
    IntMap *map = cfg->graph;
    IntMap *no_to_block = cfg->no_to_block;
    long *index_entries = map->indexes;

    for (int i = 0; i < map->size; ++i) {
        long idx = index_entries[i];
        BasicBlock *bb = (BasicBlock *)intMapGet(no_to_block,idx);
        char *bb_string = bbToJSON(bb);
        printf("%s\n====\n",bb_string);
        free(bb_string);
    }
}

aoStr *cdfAdjacencyListToJSON(CFG *cfg) {
    IntMap *map = cfg->graph;
    IntMap *no_to_block = cfg->no_to_block;
    long *index_entries = map->indexes;
    aoStr *json = aoStrNew();

    aoStrCatPrintf(json, "{\n");

    for (int i = 0; i < map->size; ++i) {
        long idx = index_entries[i];
        BasicBlock *parent = (BasicBlock *)intMapGet(no_to_block,idx);
        PtrVec *vec = (PtrVec *)intMapGet(map,idx);
        aoStrCatPrintf(json, "    %d: [", parent->block_no);

        for (int j = 0; j < vec->size; ++j) {
            BasicBlock *bb = vecGet(BasicBlock *,vec,j);
            aoStrCatPrintf(json,"%d",bb->block_no);
            if (j + 1 != vec->size) {
                aoStrCat(json,", ");
            }
        }

        aoStrCat(json,"]");
        if (i + 1 != vec->size) {
            aoStrCat(json,",");
        }
        aoStrPutChar(json,'\n');
    }
    aoStrPutChar(json,'}');
    return json;
}


int cfgRelinkVector(IntMap *map, BasicBlock *parent, BasicBlock *child,
        BasicBlock *bb)
{
    PtrVec *vec = intMapGet(map,parent->block_no);
    loggerWarning("Vector relinking. Parent bb%d, child bb%d, block: bb%d\n",
            parent->block_no, child->block_no, bb->block_no);
    if (vec) {
        int idx = 0;
        (void)cfgVecFindBlock(vec,bb->block_no,&idx);
        if (idx == -1) {
            loggerWarning("fail 1 relink : %d\n",bb->block_no);
            bb->type = BB_GARBAGE;
            return 0;
        }
        vec->entries[idx] = child;
        /* @Bug - this is close but not quite working when creating strings.HC
         * CFG */
        child->flags |= bb->flags;
        if (bb->ast_array->size == 0) {
            bb->type = BB_GARBAGE;
        }
        return 1;
    }
    loggerWarning("fail 2 relink: %d\n",bb->block_no);
    bb->type = BB_GARBAGE;
    return 0;
}

/* Returns the parent */
BasicBlock *bbRelinkControl(IntMap *map, BasicBlock *bb) {
    BasicBlock *parent = bb->prev_blocks[0];
    BasicBlock *child = bb->next;

    if (child->type == BB_CONTROL_BLOCK && bb->type == BB_CONTROL_BLOCK) {
        if (child->flags & BB_FLAG_CASE_OWNED) {
            if (child->ast_array->size == 0 && child->next && child->next->ast_array->size > 0) {
                child = child->next;
            }
        }
    }
    
    //else if (bb->next == child && child->prev != bb && child->type != BB_DO_WHILE_COND) {
    //    bb->next = child->next;
    //    bb->prev = child->prev;
    //    return NULL;
    //}

    switch (parent->type) {
        case BB_CONTROL_BLOCK: {
            parent->next = child;
            child->prev = parent;
            bb->type = BB_GARBAGE;
            break;
        }

        case BB_BRANCH_BLOCK: {
            if (parent->_if == bb) {
                parent->_if = child;
                bbAddPrev(child,parent->_if);
            } else if (parent->_else == bb) {
                parent->_else = child;
                if (bb == child) {
                    bb->type = BB_END_BLOCK;
                    bb->next = NULL;
                    return bb;
                }
            } else {
                loggerPanic("Could neither relink if or else branch");
                return NULL;
            }
            bbAddPrev(child,parent);
            break;
        }
        default:
            loggerPanic("Could not handle parent bb:%d of type = %s\n",
                    parent->block_no, bbTypeToString(parent->type));
    }
    cfgRelinkVector(map,parent,child,bb);
    return child;
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
    BasicBlock *bb_array = ((BasicBlock *)cfg->head);
    PtrVec *vec;

    for (int i = 0; i < cfg->bb_count; ++i) {
        BasicBlock *bb = &(bb_array[i]);
        if (bb->type == BB_GARBAGE) {
            continue;
        }

        /* The order of _if _else and prev/next is of PARAMOUNT importance */
        switch (bb->type) {
            case BB_RETURN_BLOCK:
            case BB_END_BLOCK: {
                vec = ptrVecNew();
                break;
            }

            case BB_CONTROL_BLOCK:
                /* This is a check for an orphaned node */
                vec = ptrVecNew();
                /* @Bug
                 * This is where a return block is the next, potentially there 
                 * is not next either */
                if (!bb->prev) {
                    bb->type = BB_GARBAGE;
                    continue;
                }

                /* @Bug 
                 * This happens with a case */
                if (bb->prev_cnt == 0 && bb->ast_array->size == 0 && 
                        (bb->flags & (BB_FLAG_CASE_BREAK|BB_FLAG_CASE_OWNED))) 
                {
                //    assert(bb->flags & (BB_FLAG_CASE_BREAK|BB_FLAG_CASE_OWNED));
                    bb->type = BB_GARBAGE;
                    continue;
                }
                if (!(bb->flags & BB_FLAG_CASE_OWNED) && (bb->ast_array->size == 0 || bb->next == NULL)) {
                    if (bb->ast_array->size > 0) {
                        bb->type = BB_END_BLOCK;
                    } else {
                        if (bb->next) {
                            loggerWarning("GARBAGE: bb%d\n",bb->block_no);
                            bbRelinkControl(map,bb);
                            if (bb->type == BB_GARBAGE) {
                                // loggerWarning("stil GARBAGE: bb%d\n",bb->block_no);
                                intMapDelete(map,bb->block_no);
                                intMapDelete(cfg->no_to_block,bb->block_no);
                                continue;
                            }
                            break;
                        } else {
                            if (bb->prev && bb->prev->type == BB_SWITCH) {
                                bb->type = BB_END_BLOCK;
                            } else {
                                bb->type = BB_GARBAGE;
                                loggerWarning("???\n");
                                continue;
                            }
                            break;
                        }
                    }
                } else if (bb->next == bb) {
                    bb->type = BB_END_BLOCK;
                    bb->next = NULL;
                }
                if (bb->type != BB_END_BLOCK) {
                    ptrVecPush(vec,bb->next);
                    bbAddPrev(bb->next,bb);
                }
                break;


            case BB_GOTO: {
                vec = ptrVecNew();
                if (bb->flags & BB_FLAG_GOTO_LOOP) {
                    ptrVecPush(vec,bb->prev);
                    bbAddPrev(bb->prev,bb);
                } else {
                    ptrVecPush(vec,bb->next);
                    bbAddPrev(bb->next,bb);
                }
                break;
            }

            case BB_BREAK_BLOCK:
            case BB_HEAD_BLOCK:
            case BB_CASE:
                vec = ptrVecNew();
                ptrVecPush(vec,bb->next);
                bbAddPrev(bb->next,bb);
                break;

            case BB_BRANCH_BLOCK:
                vec = ptrVecNew();
                ptrVecPush(vec,bb->_if);
                ptrVecPush(vec,bb->_else);
                bbAddPrev(bb->_if,bb);
                bbAddPrev(bb->_else,bb);
                break;

            /* Theoretically also want to do the cases as they could have 
             * nested blocks. Can accept this as a limitation for now */
            case BB_SWITCH: {
                vec = ptrVecNew();
                for (int i = 0; i < bb->next_blocks->size; ++i) {
                    BasicBlock *bb_case = (BasicBlock *)bb->next_blocks->entries[i];
                    ptrVecPush(vec,bb_case);
                    bbAddPrev(bb_case,bb);
                }
                break;
            }

            case BB_CONTINUE: {
                Ast *step;
                BasicBlock *loop_head = bb->prev;
                BasicBlock *loop_block = loop_head->next;
                BasicBlock *next = bb->next;
                vec = ptrVecNew();
                ptrVecPush(vec,loop_head);
                bbAddPrev(loop_head,bb);
                bb->prev = loop_head;

                bbPrint(loop_block);
                if (next) {
                    bbPrint(bb);
                    bbPrint(next);
                }
                if (next && !(bb->flags & (BB_FLAG_ELSE_BRANCH|BB_FLAG_IF_BRANCH)) && next->prev == bb) {
                    printf("THER \n");
                }
                break;
            }

            case BB_LOOP_BLOCK: {
                BasicBlock *loop_head = bb->prev;
                vec = ptrVecNew();
                ptrVecPush(vec,loop_head);
                bbAddPrev(loop_head,bb);
                break;
            }

            case BB_DO_WHILE_COND: {
                BasicBlock *loop_head = bb->prev;
                /* I have no idea how this works */
                vec = ptrVecNew();
                bbAddPrev(loop_head,bb);
                break;
            }
            default:
                loggerPanic("Unhandled type: %s\n", bbTypeToString(bb->type));
        }
        intMapSet(cfg->no_to_block,bb->block_no,bb);
        intMapSet(map,bb->block_no,vec);
    }
    return map;
}

static void cfgStronk(CFG *cfg, int _at, int *_id, int *_qp, int *_scc_count,
        int ids[100], int low[100], int sccs[100], int visited[100], int queue[128]) {
    int tmp_stack[128];
    int qp = *_qp;
    int sp = 0;
    int id = *_id;
    IntMap *graph = cfg->graph;

    tmp_stack[sp++] = _at;
    while (sp) {
        int at = tmp_stack[sp-1];
        if (ids[at] == -1) {
            ids[at] = id;
            low[at] = id;
            id++;
            queue[qp++] = at;
            visited[at] = 1;
        }

        int is_done = 1;
        /*XXX: this is wrong */
        PtrVec *vec = (PtrVec *)intMapGet(graph,at);
        for (int i = 0; i < vec->size; ++i) {
            BasicBlock *bb = (BasicBlock *)vec->entries[i];
            int to = bb->block_no;
            if (ids[to] == -1) {
                tmp_stack[sp++] = to;
                is_done = 0;
            } else if (visited[to]) {
                low[at] = min(low[at],low[to]);
            }
        }

        if (is_done) {
            sp--;
            if (ids[at] == low[at]) {
                while (1) {
                    int node = queue[qp-1];
                    qp--;
                    visited[node] = 0;
                    sccs[node] = *_scc_count;
                    if (node == at) break;
                }
                *_scc_count = *_scc_count+1;
            }
            if (sp != 0) {
                low[sp-1] = min(low[sp-1],low[at]);
            }
        }
    }

    *_id = id;
    *_qp = qp;
    printf("qp = %d\n",qp);
}

static void cfgStronglyConnectedComponents(CFG *cfg) {
    int vert[100];
    int ids[100];
    int low[100];
    int sccs[100];
    int visited[100];
    int queue[100];
    int qp = 0;
    int scc_count = 0;

    int vidx = 0;
    int id = 0;
    IntMap *graph = cfg->no_to_block;

    for (int i = 0; i < graph->size; ++i) {
        long idx = graph->indexes[i];
        BasicBlock *bb = (BasicBlock *)intMapGet(graph,idx);
        vert[vidx++] = bb->block_no;
        ids[i] = -1;
        visited[i] = 0;
    }
    memset(ids,-1,sizeof(int)*100);
    memset(visited,0,sizeof(int)*100);
    memset(low,-1,sizeof(int)*100);
    memset(queue,-1,sizeof(int)*100);

    for (int i = 0; i < graph->size; ++i) {
        if (ids[i] == -1) {
            int block_no = vert[i];
            cfgStronk(cfg,block_no,&id,&qp,&scc_count,ids,low,sccs,visited,queue);
        }
    }

    IntMap *components = intMapNew(32);
    for (int i = 0; i < graph->size; ++i) {
        long idx = graph->indexes[i];
        BasicBlock *bb = (BasicBlock *)intMapGet(graph,idx);
        if (sccs[bb->block_no] != -1) {
            int key = sccs[bb->block_no];
            if (!intMapHas(components,key)) {
                intMapSet(components,key,intVecNew());
            }
            IntVec *vec = intMapGet(components,key);
            intVecPush(vec,bb->block_no);
        }
    }

    for (int i = 0; i < components->size; ++i) {
        long idx = components->indexes[i];
        IntVec *vec = intMapGet(components,idx);
        if (!vec) continue;
        printf("nodes: [");
        for (int j = 0; j < vec->size; j++) {
            int node = vec->entries[j];
            if (j + 1 == vec->size) {
                printf("%d", node);
            } else {
                printf("%d, ", node);
            }
        }
        printf("] form a strongly connected component.\n");
    }
}

/* Split out to make it simpler to reason with */
static void cfgConstructFunction(CFGBuilder *builder, List *stmts) {
    if (!listEmpty(stmts)) {
        builder->ast_list = stmts;
        listForEach(stmts) {
            Ast *ast = cast(Ast *,it->value);
            builder->ast_iter = it;
            cfgHandleAstNode(builder,ast);
        }
    }

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
}


static CFG *cfgCreateForFunction(Cctrl *cc, Ast *ast_fn, IntMap *leaf_cache, int *_block_no) {
    CFGBuilder builder;
    *_block_no += 1;

    cfgBuilderInit(&builder,cc,leaf_cache,*_block_no);

    loggerDebug("creating CFG for: %s\n", ast_fn->fname->data);

    BasicBlock *bb = cfgBuilderAllocBasicBlock(&builder,BB_HEAD_BLOCK);
    BasicBlock *bb_next = cfgBuilderAllocBasicBlock(&builder,BB_CONTROL_BLOCK);
    CFG *cfg = cfgNew(ast_fn->fname,bb);
    bb->next = bb_next;
    bb_next->prev = bb;
    builder.cfg = cfg;
    builder.bb = bb_next;
    cfgConstructFunction(&builder,ast_fn->body->stms);

    /* This is a function with no body */
    if (builder.bb == bb->next && bb->next->type != BB_RETURN_BLOCK) {
        BasicBlock *bb_return = cfgBuilderAllocBasicBlock(&builder,BB_RETURN_BLOCK);
        bb->next->next = bb_return;
        bb_return->prev = bb->next;
        bb_return->next = NULL;
        intMapSet(cfg->no_to_block,bb_return->block_no,bb_return);
    }

    cfg->bb_count = builder.bb_count;
    cfg->_memory = builder.bb_pool;
    cfg->graph = cfgBuildAdjacencyList(cfg);
    *_block_no = builder.bb_block_no+1;
    //cfgAdjacencyListPrintShallow(cfg);
    //aoStr *json = cdfAdjacencyListToJSON(cfg);
   // printf("%s\n",json->data);
    //cfgStronglyConnectedComponents(cfg);
//    exit(0);

    //dictRelease(builder.resolved_labels);
    //listRelease(builder.unresoved_gotos,NULL);
    return cfg;
}

static void _bbFindAllLoopNodes(BasicBlock *loop_head, BasicBlock *bb,
        IntMap *nodes, int loop_cnt)
{
    int is_start_of_new_loop = bb != loop_head && bb->flags & BB_FLAG_LOOP_HEAD;

    if (bb->flags & BB_FLAG_LOOP_END) {
        if (bb->prev == loop_head) {
            intMapSet(nodes,bb->block_no,bb);
            loop_cnt--;
            return;
        }
        if (loop_cnt > 1) {
            intMapSet(nodes,bb->block_no,bb);
        }
    } else if (bb->type != BB_END_BLOCK && bb->type != BB_RETURN_BLOCK 
            && bb->type != BB_HEAD_BLOCK) {
        intMapSet(nodes,bb->block_no,bb);
    }


    if (is_start_of_new_loop) {
        loop_cnt++;
    }

    if (bb->type == BB_BREAK_BLOCK) {
        /* Add it and it is up to the caller to handle what a break means */
        intMapSet(nodes,bb->block_no,bb);
        return;
    }

    switch (bb->type) {
        case BB_RETURN_BLOCK:
        case BB_END_BLOCK:
        case BB_HEAD_BLOCK:
        case BB_LOOP_BLOCK:
            break;

        case BB_DO_WHILE_COND: /* Come back to */
            _bbFindAllLoopNodes(loop_head,bb->next,nodes,loop_cnt);
            break;

        case BB_BRANCH_BLOCK:
            assert(bb->_if != NULL);
            assert(bb->_else != NULL);
            _bbFindAllLoopNodes(loop_head,bb->_if,nodes,loop_cnt);
            if (is_start_of_new_loop || !(bb->flags & BB_FLAG_LOOP_HEAD)) {
                _bbFindAllLoopNodes(loop_head,bb->_else,nodes,loop_cnt);
            }
            break;

        case BB_SWITCH: {
            for (int i = 0; i < bb->next_blocks->size; ++i) {
                BasicBlock *it = vecGet(BasicBlock *,bb->next_blocks,i);
                _bbFindAllLoopNodes(loop_head,it,nodes,loop_cnt);
            }
            _bbFindAllLoopNodes(loop_head,bb->next,nodes,loop_cnt);
            break;
        }

        case BB_BREAK_BLOCK: 
            if (loop_cnt > 1) {
                _bbFindAllLoopNodes(loop_head,bb->next,nodes,loop_cnt);
            }
            break;
 
        case BB_GOTO:
        case BB_CONTROL_BLOCK:
            _bbFindAllLoopNodes(loop_head,bb->next,nodes,loop_cnt);
            break;
    }

    if (bb->flags & BB_FLAG_LOOP_END) {
        loop_cnt--;
    }
}

IntMap *bbFindAllLoopNodes(BasicBlock *loop_head) {
    int loop_cnt = 0;
    IntMap *map = intMapNew(32);
    intMapSet(map,loop_head->block_no,loop_head);
    _bbFindAllLoopNodes(loop_head,loop_head,map,loop_cnt);
    for (int i = 0; i < loop_head->prev_cnt; ++i) {
        BasicBlock *prev = loop_head->prev_blocks[i];
        if (prev->type == BB_LOOP_BLOCK && prev->prev == loop_head) {
            printf(" we have prev: bb%d\n",prev->block_no);
            break;
        }
    }
    return map;
}

void cfgExplore(CFG *cfg, IntMap *seen, int block_no) {
    if (intMapHas(seen,block_no)) return;
    intMapSet(seen,block_no,NULL);
    PtrVec *vec = (PtrVec *)intMapGet(cfg->graph,block_no);
    printf("bb%d\n",block_no);
    for (int i = 0; i < vec->size; ++i) {
        BasicBlock *bb = vecGet(BasicBlock *,vec,i);
        if (!intMapHas(seen,bb->block_no) && bb->flags & BB_FLAG_LOOP_HEAD) {
            printf("loophead: bb%d\n",bb->block_no);
            IntMap *loop_nodes = bbFindAllLoopNodes(bb);
            for (int j = 0; j < loop_nodes->size; ++j) {
                long idx = loop_nodes->indexes[j];
                BasicBlock *loop_child = (BasicBlock *)intMapGet(loop_nodes,idx);
                printf("  bb%d\n",loop_child->block_no);
            }
            intMapRelease(loop_nodes);
        }
        cfgExplore(cfg,seen,bb->block_no);
    }
}

void cfgIter(CFG *cfg) {
    IntMap *map = cfg->graph;
    IntMap *no_to_block = cfg->no_to_block;
    long *index_entries = map->indexes;
    IntMap *seen = intMapNew(32);

    for (int i = 0; i < map->size; ++i) {
        long idx = index_entries[i];
        BasicBlock *parent = (BasicBlock *)intMapGet(no_to_block,idx);
        cfgExplore(cfg,seen,parent->block_no);
    }
}

/* This will need to return a list of CFG's */
PtrVec *cfgConstruct(Cctrl *cc) {
    PtrVec *cfgs = ptrVecNew();
    int block_no = 0;
    IntMap *leaf_cache = intMapNew(32);
    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            CFG *cfg = cfgCreateForFunction(cc,ast,leaf_cache,&block_no);
            ptrVecPush(cfgs,cfg);
        }
    }
    return cfgs;
}
