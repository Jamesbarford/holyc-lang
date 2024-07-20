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

void bbPrintNoAst(BasicBlock *bb) {
    char *bb_string = bbToString(bb);
    printf("%s\n",bb_string);
    free(bb_string);
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

#define CFG_BUILDER_MAX_MEMORY 128
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
    if (bb->ast_array->size > 0) {
        BasicBlock *new_bb = cfgBuilderAllocBasicBlock(builder,BB_BRANCH_BLOCK);
        bb->next = new_bb;
        new_bb->prev = bb;
        bb = new_bb;
    } else {
        bb->type = BB_BRANCH_BLOCK;
    }
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
            bb->next = builder->bb;
        } else {
            new_block = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);
            bb->next = new_block;
            new_block->prev = bb;
            cfgBuilderSetBasicBlock(builder,new_block);
        }
    } else {
        if (builder->bb != if_body) {
            builder->bb->prev = bb;
            bb->next = builder->bb;
        }

        int is_break = if_body->type != BB_BREAK_BLOCK;
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

    if (ast_cond == NULL) {
        ast_cond = ast_forever_sentinal;
    }

    builder->flags |= CFG_BUILDER_FLAG_IN_LOOP;

    BasicBlock *bb_prev_loop = builder->bb_cur_loop;
    BasicBlock *bb_cond;
    BasicBlock *bb = builder->bb;
    if (bb->ast_array->size == 0) {
        bb_cond = bb;
        bb_cond->type = BB_BRANCH_BLOCK;
    } else {
        bb_cond = cfgBuilderAllocBasicBlock(builder,BB_BRANCH_BLOCK);
        bb_cond->prev = bb;
        builder->bb->next = bb_cond;
    }

    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,
                                                         BB_CONTROL_BLOCK);
    /* Body may or may not be a Loop, we correct it in the if condition with
     * builder->bb != bb_body */
    BasicBlock *bb_body = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);

    if (ast_init) ptrVecPush(bb->ast_array,ast_init);

    bb_cond->flags |= BB_FLAG_LOOP_HEAD;

    bb->next = bb_cond;
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

    if (bb_body->type != BB_BREAK_BLOCK && ast_step) {
        cfgHandleAstNode(builder,ast_step);
    }

    /* This means we had branches or possibly inner loops in our loop */
    if (builder->bb != bb_body || !builder->bb->next) {
        builder->bb->type = BB_LOOP_BLOCK;
        builder->bb->prev = bb_cond;
        /* So we can access the loop block from the head of the condition */
        bb_cond->next = builder->bb;
        // bb_cond->_else = builder->bb;
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
    BasicBlock *bb_cond;
    BasicBlock *bb = builder->bb;

    if (bb->ast_array->size == 0) {
        bb_cond = bb;
        bb_cond->type = BB_BRANCH_BLOCK;
    } else {
        bb_cond = cfgBuilderAllocBasicBlock(builder,BB_BRANCH_BLOCK);
        bb_cond->prev = bb;
        builder->bb->next = bb_cond;
    }

    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,
                                                         BB_CONTROL_BLOCK);
    BasicBlock *bb_body = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);

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
    BasicBlock *bb = builder->bb;
    BasicBlock *bb_do_while;

    if (bb->ast_array->size == 0) {
        bb_do_while = bb;
        bb_do_while->type = BB_CONTROL_BLOCK;
    } else {
        bb_do_while = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);
        bb_do_while->prev = bb;
        builder->bb->next = bb_do_while;
    }
    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);

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
    builder->bb->type = BB_CONTINUE;
    builder->bb->prev = builder->bb_cur_loop;
    bbAddPrev(builder->bb_cur_loop,builder->bb);
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

/* Top down through the tree finding blocks which are not pointing to anything */
static void cfgLinkLeaves(IntMap *map, BasicBlock *bb, BasicBlock *dest) {
    if (!intMapHas(map,bb->block_no)) {
        intMapSet(map,bb->block_no,NULL);
        switch (bb->type) {
            case BB_SWITCH:
            case BB_CONTROL_BLOCK:
                if      (bb == bb->next)   bb->next = dest;
                else if (bb->next == NULL) bb->next = dest;
                else if (bb->next->ast_array->size == 0) bb->next = dest;
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

    if (post->ast_array->size > 0) {
        join = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);
        join->prev = post;
        post->next = join;
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
        case AST_BREAK:         cfgHandleBreak(builder,ast);       break;
        case AST_COMPOUND_STMT: cfgHandleCompound(builder,ast);    break;
        case AST_CONTINUE:      cfgHandleContinue(builder,ast);    break;
        case AST_DO_WHILE: {
            BasicBlock *pre = builder->bb;
            cfgHandleDoWhileLoop(builder,ast);
            BasicBlock *post = builder->bb;
            BasicBlock *loop_head = pre->next;
            /* this is the loop back */
            loop_head->next = NULL;
            BasicBlock *join = cfgMergeBranches(builder,pre,post);
            cfgBuilderSetBasicBlock(builder,join);
            break;
        }

        case AST_FOR: {
            BasicBlock *pre = builder->bb;
            cfgHandleForLoop(builder,ast);
            BasicBlock *loop_head = pre->next;
            /* this is the loop back */
            loop_head->next = NULL;

            BasicBlock *post = builder->bb;
            BasicBlock *join = cfgMergeBranches(builder,pre,post);

            cfgBuilderSetBasicBlock(builder,join);
            break;
        }

        case AST_WHILE: {
            BasicBlock *pre = builder->bb;
            cfgHandleWhileLoop(builder,ast);
            BasicBlock *post = builder->bb;
            BasicBlock *loop_head = pre->next;
            /* this is the loop back */
            loop_head->next = NULL;
            BasicBlock *join = cfgMergeBranches(builder,pre,post);
            cfgBuilderSetBasicBlock(builder,join);
            break;
        }

        case AST_IF: {
            BasicBlock *pre = builder->bb;
            cgfHandleBranchBlock(builder,ast);
            BasicBlock *post;
            if (pre->next && pre->next->_else) {
                post = pre->next->_else;
            } else {
                post = builder->bb;
            }
            BasicBlock *join = cfgMergeBranches(builder,pre,post);
            cfgBuilderSetBasicBlock(builder,join);
            break;
        }
        case AST_GOTO:          cfgHandleGoto(builder,ast);        break;
        case AST_LABEL:         cfgHandleLabel(builder,ast);       break;
        case AST_RETURN:        cfgHandleReturn(builder,ast);      break;
        case AST_JUMP:         cfgHandleJump(builder,ast);        break;
        case AST_SWITCH:       cfgHandleSwitch(builder,ast);      break;

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
    char *bb_string = bbToString(bb);
    if (is_parent) printf("%s\n",bb_string);
    else           printf("     %s\n",bb_string);
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

/* Returns the parent */
BasicBlock *bbRelinkControl(IntMap *map, BasicBlock *bb) {
    BasicBlock *parent = bb->prev;
    BasicBlock *child = bb->next;

    if (child->type == BB_CONTROL_BLOCK && bb->type == BB_CONTROL_BLOCK) {
        if (bb->next == child && child->prev != bb) {
            bb->next = child->next;
            bb->prev = child->prev;
            return NULL;
        }
    }

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
 //               child->prev = parent->_if;
            } else if (parent->_else == bb) {
                parent->_else = child;
                //child->prev = parent->_else;
            } else {
                // loggerPanic("Could neither relink if or else branch");
                return NULL;
            }
            break;
        }
        default:
            loggerPanic("Could not handle parent bb:%d of type = %s\n",
                    parent->block_no, bbTypeToString(bb->type));
    }
    PtrVec *vec = intMapGet(map,parent->block_no);
    if (vec) {
        int idx = 0;
        (void)cfgVecFindBlock(vec,bb->block_no,&idx);
        if (idx != -1) {
            vec->entries[idx] = child;
            if (bb->ast_array->size == 0) {
                bb->type = BB_GARBAGE;
            }
        } else {
            loggerWarning("fail 1 relink : %d\n",bb->block_no);
            bb->type = BB_GARBAGE;
        }
    } else {
        loggerWarning("fail 2 relink: %d\n",bb->block_no);
        bb->type = BB_GARBAGE;
    }

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
                //ptrVecPush(vec,hash);
                break;
            }

            case BB_CONTROL_BLOCK:
                /* This is a check for an orphaned node */
                vec = ptrVecNew();
                if (!(bb->flags & BB_FLAG_CASE_OWNED) && (bb->ast_array->size == 0 || bb->next == NULL)) {
                    if (bb->ast_array->size > 0) {
                        bb->type = BB_END_BLOCK;
                    } else {
                        if (bb->next) {
                            bbRelinkControl(map,bb);
                            if (bb->type == BB_GARBAGE) {
                               // loggerWarning("GARBAGE: bb%d\n",bb->block_no);
                                intMapDelete(map,bb->block_no);
                                intMapDelete(cfg->no_to_block,bb->block_no);
                                continue;
                            }
                            break;
                        } else {
                            bb->type = BB_GARBAGE;
                            // loggerWarning("GARBAGE: bb%d\n",bb->block_no);
                            continue;
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

            case BB_CONTINUE:
            case BB_LOOP_BLOCK: {
                BasicBlock *loop_head = bb->prev;
                vec = ptrVecNew();
                ptrVecPush(vec,loop_head);
                bbAddPrev(loop_head,bb);
                break;
            }

            case BB_DO_WHILE_COND:
                vec = ptrVecNew();
                ptrVecPush(vec,bb->prev);
                bbAddPrev(bb->prev,bb);
                if (bb->next) {
                    bbAddPrev(bb->next,bb);
                    ptrVecPush(vec,bb->next);
                }
                break;

            default:
                loggerPanic("Unhandled type: %s\n", bbTypeToString(bb->type));
        }
        intMapSet(cfg->no_to_block,bb->block_no,bb);
        intMapSet(map,bb->block_no,vec);
    }
    return map;
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

    /* We still want these structures but don't want their contents for the 
     * next functions. */
//    dictClear(builder->resolved_labels);
//    listClear(builder->unresoved_gotos,NULL);
}


static CFG *cfgCreateForFunction(Cctrl *cc, Ast *ast_fn, IntMap *leaf_cache, int *_block_no) {
    CFGBuilder builder;
    cfgBuilderInit(&builder,cc,leaf_cache,*_block_no);

    loggerDebug("creating CFG for: %s\n", ast_fn->fname->data);

    BasicBlock *bb = cfgBuilderAllocBasicBlock(&builder,BB_HEAD_BLOCK);
    BasicBlock *bb_next = cfgBuilderAllocBasicBlock(&builder,BB_CONTROL_BLOCK);
    CFG *cfg = cfgNew(ast_fn->fname,bb);
    bb->next = bb_next;
    bb_next->prev = bb;
    cfgBuilderSetCFG(&builder,cfg);
    cfgBuilderSetBasicBlock(&builder,bb_next);
    cfgConstructFunction(&builder,ast_fn->body->stms);

    /* This is a function with no body */
    if (builder.bb == bb->next) {
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

//    dictRelease(builder.resolved_labels);
//    listRelease(builder.unresoved_gotos,NULL);
    return cfg;
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
