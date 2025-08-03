#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "cfg.h"
#include "containers.h"
#include "lexer.h"
#include "list.h"
#include "util.h"

static void cfgHandleAstNode(CFGBuilder *builder, Ast *ast);
static void cfgLinkLeaves(Map *map, BasicBlock *bb, BasicBlock *dest);


VecType vec_cfg_type = {
    .stringify = NULL,
    .match     = NULL,
    .release   = NULL,
    .type_str  = "CFG *",
};

void vecBbToString(AoStr *buf, void *bb) {
    char *bb_str = bbToString((BasicBlock *)bb);
    aoStrCatPrintf(buf, "%s\n", bb_str);
}

int vecBbMatch(void *bb1, void *bb2) {
    return ((BasicBlock *)bb1)->block_no ==
           ((BasicBlock *)bb2)->block_no;
}

VecType vec_basic_block_type = {
    .stringify = vecBbToString,
    .match     = vecBbMatch,
    .release   = NULL,
    .type_str  = "BasicBlock *",
};

Vec *bbVecNew(void) {
    return vecNew(&vec_basic_block_type);
}

AoStr *mapBasicBlockToString(void *bb) {
    char *bb_str = bbToString((BasicBlock *)bb);
    return aoStrDupRaw(bb_str, strlen(bb_str));
}

MapType map_int_to_basic_block_type = {
    .match           = mapIntKeyMatch,
    .hash            = mapIntKeyHash,
    .get_key_len     = mapIntKeyLen,
    .key_to_string   = mapIntToString,
    .key_release     = NULL,
    .value_to_string = mapBasicBlockToString,
    .value_release   = NULL,
    .key_type        = "int",
    .value_type      = "BasicBlock *",
};

MapType map_cstring_to_basic_block_type = {
    .match           = mapCStringEq,
    .hash            = mapCStringHash,
    .get_key_len     = mapCStringLen,
    .key_to_string   = mapCStringToString,
    .key_release     = NULL,
    .value_to_string = mapBasicBlockToString,
    .value_release   = NULL,
    .key_type        = "char *",
    .value_type      = "BasicBlock *",
};

MapType map_int_to_int_set_type = {
    .match           = mapIntKeyMatch,
    .hash            = mapIntKeyHash,
    .get_key_len     = mapIntKeyLen,
    .key_to_string   = mapIntToString,
    .key_release     = NULL,
    .value_to_string = (mapValueToString *)setToString,
    .value_release   = (mapValueRelease *)setRelease,
    .key_type        = "char *",
    .value_type      = "Set<int>",
};

void cfgBuilderRelease(CFGBuilder *builder, int free_builder) {
    if (builder) {
        if (free_builder) {
            free(builder);
        }
    }
}

void bbInit(void *_bb) {
    BasicBlock *bb = (BasicBlock *)_bb;
    bb->type = BB_GARBAGE;
    bb->block_no = -1;
}

static void cfgBuilderInit(CFGBuilder *builder, Cctrl *cc, Map *leaf_cache, int block_no) {
    MemPool *block_pool = memPoolNew(2048);

    builder->cc = cc;
    builder->bb_block_no = block_no;
    builder->bb_count = 1;
    builder->flags = 0;
    builder->bb_cur_loop = NULL;
    builder->leaf_cache = leaf_cache;
    builder->block_pool = block_pool;

    builder->unresolved_gotos = listNew();
    builder->resolved_labels = mapNew(16, &map_cstring_to_basic_block_type);
}

static void cfgBuilderClearLeafCache(CFGBuilder *builder) {
    mapClear(builder->leaf_cache);
}

static void bbAddPrev(BasicBlock *bb, BasicBlock *prev) {
    mapAdd(bb->prev_blocks,(void *)(long)prev->block_no,prev);
}

static void bbRemovePrev(BasicBlock *bb, int prev_block_no) {
    mapRemove(bb->prev_blocks,(void *)(long)prev_block_no);
}

int bbPrevHas(BasicBlock *bb, int block_no) {
    return mapHas(bb->prev_blocks,(void *)(long)block_no);
}

int bbIsType(BasicBlock *bb, enum bbType type) {
    return bb->type == type;
}

int bbOneOfType(BasicBlock *bb, int cnt, int types[]) {
    for (int i = 0; i < cnt; ++i) {
        if (bb->type == types[i]) return 1;
    }
    return 0;
}

int bbAstCount(BasicBlock *bb) {
    return bb->ast_array->size;
}

void bbAstAdd(BasicBlock *bb, Ast *ast) {
    vecPush(bb->ast_array,ast);
}

void bbSetPrevPtr(BasicBlock *bb, BasicBlock *prev) {
    bb->prev = prev;
} 

void bbSetIf(BasicBlock *bb, BasicBlock *_if) {
    bb->_if = _if;
    _if->flags |= BB_FLAG_IF_BRANCH;
}

void bbSetElse(BasicBlock *bb, BasicBlock *_else) {
    bb->_else = _else;
    _else->flags |= BB_FLAG_ELSE_BRANCH;
}

void bbSetNext(BasicBlock *bb, BasicBlock *next) {
    bb->next = next;
}

void bbSetLoopEnd(BasicBlock *bb) {
    bb->flags |= BB_FLAG_LOOP_END;
}

void bbSetType(BasicBlock *bb, enum bbType type) {
    bb->type = type;
}

int bbIsLoopEnd(BasicBlock *bb) {
    return bb->flags & BB_FLAG_LOOP_END;
}

static void cfgBuilderSetBlock(CFGBuilder *builder, BasicBlock *bb) {
    builder->bb = bb;
}

int bbIsLoopControl(BasicBlock *bb) {
    return bbIsType(bb,BB_CONTINUE) || bbIsType(bb,BB_BREAK_BLOCK);
}

char *bbTypeToString(int type) {
    switch (type) {
        case BB_GARBAGE:       return "BB_GARBAGE";
        case BB_END_BLOCK:     return "BB_END_BLOCK";
        case BB_HEAD_BLOCK:    return "BB_HEAD_BLOCK";
        case BB_CONTROL_BLOCK: return "BB_CONTROL_BLOCK";
        case BB_BRANCH_BLOCK:  return "BB_BRANCH_BLOCK";
        case BB_LOOP_BLOCK:    return "BB_LOOP_BLOCK";
        case BB_RETURN_BLOCK:  return "BB_RETURN_BLOCK";
        case BB_BREAK_BLOCK:   return "BB_BREAK_BLOCK";
        case BB_DO_WHILE_COND: return "BB_DO_WHILE_COND";
        case BB_GOTO:          return "BB_GOTO";
        case BB_SWITCH:        return "BB_SWITCH";
        case BB_CASE:          return "BB_CASE";
        case BB_CONTINUE:      return "BB_CONTINUE";
        default:
            loggerPanic("Unknown type: %d\n", type);
    }
}

#ifdef DEBUG
void cfgPrintAstArray(BasicBlock *bb) {
    if (bb->ast_array->size == 0) {
        printf("(null)\n");
    } else {
        for (u64 i = 0; i < bb->ast_array->size; ++i) {
            printf("%lu: \n", i);
            astPrint(bb->ast_array->entries[i]);
        }
    }
}
#endif

char *bbFlagsToString(u32 flags) {
    if (!flags) return mprintf("(NO_FLAGS)");
    AoStr *str = aoStrNew();
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
    if (flags & BB_FLAG_FOR_LOOP) {
        _concat_flag("BB_FLAG_FOR_LOOP");
    }
    if (flags & BB_FLAG_FOR_LOOP_HAS_STEP) {
        _concat_flag("BB_FLAG_FOR_LOOP_HAS_STEP");
    }
    if (flags & BB_FLAG_HAD_NO_ELSE) {
        _concat_flag("BB_FLAG_HAD_NO_ELSE");
    }
    aoStrPutChar(str,')');

#undef _concat_flag

    return aoStrMove(str); 
}

#define BB_FMT_TYPE_CHARS 18
#define BB_FMT_FLAG_CHARS 40
char *bbToString(BasicBlock *bb) {
    AoStr *str = aoStrNew();
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

    if (bb->next)  aoStrCatPrintf(str,"next = bb%d, ",bb->next->block_no);
    if (bb->_if)   aoStrCatPrintf(str,"if = bb%d, ",bb->_if->block_no);
    if (bb->_else) aoStrCatPrintf(str,"else = bb%d ",bb->_else->block_no);
    if (bb->prev)  aoStrCatPrintf(str,"prev_ptr = bb%d ",bb->prev->block_no);
    return aoStrMove(str);
}

char *bbPreviousBlockNumbersToString(BasicBlock *bb) {
    return aoStrMove(mapKeysToString(bb->prev_blocks));
}

char *bbToJSON(BasicBlock *bb) {
    AoStr *str = aoStrNew();
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
    aoStrCatPrintf(str,"    \"prev_nodes\": \"%s\",\n", str_prev);
    aoStrCatPrintf(str,"    \"prev_ptr\": \"%s\",\n", str_prev_ptr ? str_prev_ptr : "(null)");
    if (bbIsType(bb, BB_BRANCH_BLOCK)) {
        if (bb->_if)   aoStrCatPrintf(str,"    \"if\": \"bb%d\",\n",bb->_if->block_no);
        if (bb->_else) aoStrCatPrintf(str,"    \"else\": \"bb%d\"\n",bb->_else->block_no);
    } else {
        if (bb->next) aoStrCatPrintf(str,"    \"next\": \"bb%d\"\n",bb->next->block_no);
    }

    aoStrCat(str,"}\n");
    return aoStrMove(str);
}

void bbPrintNoAst(BasicBlock *bb) {
    char *bb_string = bbToString(bb);
    printf("%s\n",bb_string);
}

void bbPrint(BasicBlock *bb) {
    assert(bb != NULL);
    char *bb_str = bbToString(bb);
    AoStr *str = aoStrDupRaw((char*)bb_str, strlen(bb_str)); 
    aoStrPutChar(str,'\n');
    for (u64 i = 0; i < bb->ast_array->size; ++i) {
        char *ast_str = astLValueToString(bb->ast_array->entries[i],0);
        aoStrCat(str,ast_str);
        aoStrPutChar(str,'\n');
    }
    printf("========\n%s========\n",str->data);
}


static BasicBlock *cfgBuilderAllocBasicBlock(CFGBuilder *builder,
                                             int type,
                                             u32 flags)
{
    BasicBlock *bb = (BasicBlock *)memPoolAlloc(builder->block_pool, sizeof(BasicBlock));

    bb->type = type;
    bb->flags = 0;
    bb->block_no = builder->bb_block_no++;
    builder->bb_count++;
    bb->prev_blocks = mapNew(32, &map_int_to_basic_block_type);
    bb->flags = flags;
    /* @Leak
     * This should probably be a memory pool as well */
    bb->ast_array = astVecNew();
    bb->visited = 0;
    bb->next = bb->prev = bb->_else = bb->_if = NULL;
    return bb;
}

static CFG *cfgNew(AoStr *fname, BasicBlock *head_block) {
    CFG *cfg = (CFG *)malloc(sizeof(CFG));
    cfg->head = head_block;
    cfg->ref_fname = fname;
    cfg->no_to_block = mapNew(32, &map_int_to_basic_block_type);
    cfg->graph = mapNew(32, &map_int_to_int_set_type);
    return cfg;
}

/* Sometimes the block that has been allocated and exists on the builder 
 * can be used i.e there are no asts in the array. 
 * Other times we need to allocate a new block */
static BasicBlock *cfgSelectOrCreateBlock(CFGBuilder *builder, int type,
        u32 flags)
{
    BasicBlock *bb = NULL;
    if (bbAstCount(builder->bb) == 0 && !bbIsLoopControl(builder->bb)) {
        bb = builder->bb;
        bbSetType(bb,type);
    } else {
        bb = cfgBuilderAllocBasicBlock(builder,type,0);
        bbSetPrevPtr(bb,builder->bb);
        bbSetNext(builder->bb,bb);
    }
    /* If the block is being recycled it could have flags that are needed on it 
     * so OR in the new flags as opposed to setting them */
    bb->flags |= flags;
    return bb;
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

    builder->flags |= CFG_BUILDER_FLAG_IN_CONDITIONAL;

    BasicBlock *cond = cfgSelectOrCreateBlock(builder,BB_BRANCH_BLOCK,0);
    BasicBlock *else_body = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK,BB_FLAG_ELSE_BRANCH);
    BasicBlock *if_body = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK,BB_FLAG_IF_BRANCH);
    BasicBlock *new_block;

    /* Ensure the new nodes point to the branch head */
    bbSetPrevPtr(if_body,cond);
    bbSetPrevPtr(else_body,cond);

    /* Set the if and else branches of the node */
    bbSetIf(cond,if_body);
    bbSetElse(cond,else_body);


    cfgBuilderSetBlock(builder,cond);
    cfgHandleAstNode(builder,ast->cond);

    cfgBuilderSetBlock(builder,if_body);
    /* Start of a new basic block */
    cfgHandleAstNode(builder,ast->then);

    if (ast->els) {
        cfgBuilderSetBlock(builder,else_body);
        cfgHandleAstNode(builder,ast->els);

        /* This means the node has changed, and thus the next block needs 
         * to be this branch */
        if (builder->bb != else_body) {
            if (!bbIsType(builder->bb, BB_CONTINUE)) {
                bbSetPrevPtr(builder->bb,cond);
            }
            /* Join */ 
            bbSetNext(cond,builder->bb);
        } else {
            new_block = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK,0);
            bbSetNext(cond,new_block);
            bbSetPrevPtr(new_block,else_body);
            cfgBuilderSetBlock(builder,new_block);
        }
    } else {
        cond->flags |= BB_FLAG_HAD_NO_ELSE;
        /* If the current node is not the if then it will be the next node. 
         * Presumably at this point in time it is empty and yet to be set. */
        if (builder->bb != if_body) {
            bbSetNext(cond,builder->bb);
        }

        /* Continue must point back to the loop head */
        if (!bbIsType(builder->bb,BB_CONTINUE)) {
            bbSetPrevPtr(builder->bb,cond);
        }

        int is_next_return = if_body->next && !bbIsType(if_body->next,BB_RETURN_BLOCK);

        /* Set the node subsequent to both paths of the if and else as the 
         * next pointer */
        if (!bbIsLoopControl(if_body) && !is_next_return) {
            bbSetNext(cond,else_body);
        }
        cfgBuilderSetBlock(builder,else_body);
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

    BasicBlock *bb_init = NULL;

    if (ast_init) {
        /* This is straight line code, there is no need to allocate or do 
         * any basic block manipulation. Thus */
        if (bbIsType(builder->bb,BB_CONTROL_BLOCK)) {
            bb_init = builder->bb;
        } else {
            bb_init = cfgSelectOrCreateBlock(builder,BB_CONTROL_BLOCK,0);
            cfgBuilderSetBlock(builder,bb_init);
        }
        cfgHandleAstNode(builder,ast_init);
    }

    BasicBlock *bb_cond = cfgSelectOrCreateBlock(builder,BB_BRANCH_BLOCK,
            BB_FLAG_LOOP_HEAD|BB_FLAG_FOR_LOOP);
    if (ast_step) bb_cond->flags |= BB_FLAG_FOR_LOOP_HAS_STEP;

    if (ast_cond == NULL) {
        ast_cond = astMakeForeverSentinal();
    }

    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,
                                                         BB_CONTROL_BLOCK,0);
    /* Body may or may not be a Loop, we correct it in the if condition with
     * builder->bb != bb_body */
    BasicBlock *bb_body = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK,0);

    /* Jump into loop body if condition is met */
    bbSetIf(bb_cond,bb_body);
    /* Else move past loop body */
    bbSetElse(bb_cond,bb_cond_else);
    /* And the previously met block was the condition */
    bbSetPrevPtr(bb_cond_else,bb_cond);
    bbSetPrevPtr(bb_body,bb_cond);


    builder->bb_cur_loop = bb_cond;

    cfgBuilderSetBlock(builder,bb_cond);
    cfgHandleAstNode(builder,ast_cond);

    /* Forms a loop, though this should be picked up by the if condition
     * below, This can be unset by the above call so reset it. */
    bbSetPrevPtr(bb_body,bb_cond);

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
    cfgBuilderSetBlock(builder,bb_body);
    cfgHandleAstNode(builder,ast_body);

    /* If there is a step add that as the last node to the body of the loop */
    if (!bbIsLoopControl(bb_body) && ast_step) {
        cfgHandleAstNode(builder,ast_step);
    }

    /* This means we had branches or possibly inner loops in our loop */
    if (builder->bb != bb_body || !builder->bb->next) {
        bbSetType(builder->bb,BB_LOOP_BLOCK);
        bbSetPrevPtr(builder->bb,bb_cond);
        /* So we can access the loop block from the head of the condition */
        bbSetNext(bb_cond,builder->bb);
    }

    /* This is a loop which immediately hit a break; */
    if (bbIsType(builder->bb,BB_BREAK_BLOCK) && bbIsLoopEnd(builder->bb)) {
        builder->bb->flags |= BB_FLAG_REDUNDANT_LOOP;
        builder->bb->flags &= ~(BB_FLAG_LOOP_END);
        bb_cond->flags &= ~BB_FLAG_LOOP_HEAD;
        bb_cond->flags |= BB_FLAG_REDUNDANT_LOOP;
    }

    bbSetLoopEnd(bb_cond_else);
    cfgBuilderSetBlock(builder,bb_cond_else);

    if (!bb_prev_loop) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_LOOP);
    }

    builder->bb_cur_loop = bb_prev_loop;
}

static void cfgHandleWhileLoop(CFGBuilder *builder, Ast *ast) {
    Ast *ast_cond = ast->whilecond;
    Ast *ast_body = ast->whilebody;
    
    builder->flags |= CFG_BUILDER_FLAG_IN_LOOP;
    BasicBlock *bb_prev_loop = builder->bb_cur_loop;

    BasicBlock *bb_cond = cfgSelectOrCreateBlock(builder,BB_BRANCH_BLOCK,
            (BB_FLAG_LOOP_HEAD|BB_FLAG_WHILE_LOOP));
    BasicBlock *bb_body = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK,0);
    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,
                                                         BB_CONTROL_BLOCK,0);

    /* Jump into loop body if condition is met */
    bbSetIf(bb_cond,bb_body);

    /* Else move past loop body */
    bbSetElse(bb_cond,bb_cond_else);
    bbSetLoopEnd(bb_cond_else);

    /* And the previously met block was the condition */
    bbSetPrevPtr(bb_cond_else,bb_cond);

    builder->bb_cur_loop = bb_cond;

    cfgBuilderSetBlock(builder,bb_cond);
    cfgHandleAstNode(builder,ast_cond);

    /* Forms a loop, though this should be picked up by the if condition
     * below */
    bbSetPrevPtr(bb_body,bb_cond);

    cfgBuilderSetBlock(builder,bb_body);
    if (ast_body) {
        cfgHandleAstNode(builder,ast_body);
    }

    /* This means we had branches or possibly inner loops in our loop */
    if (builder->bb != bb_body || !builder->bb->next) {
        bbSetType(builder->bb,BB_LOOP_BLOCK);
        if (bbAstCount(builder->bb) == 0) {
            vecPush(builder->bb->ast_array,astMakeLoopSentinal());
        }
        /* So we can access the loop block from the head of the condition */
        bbSetNext(bb_cond,builder->bb);
        bbSetPrevPtr(builder->bb,bb_cond);
    }
    
    /* This is a loop which immediately hit a break; */
    if (bbIsType(builder->bb,BB_BREAK_BLOCK) && bbIsLoopEnd(builder->bb)) {
        builder->bb->flags |= BB_FLAG_REDUNDANT_LOOP;
        builder->bb->flags &= ~(BB_FLAG_LOOP_END);
        bb_cond->flags &= ~BB_FLAG_LOOP_HEAD;
        bb_cond->flags |= BB_FLAG_REDUNDANT_LOOP;
    }

    bbSetLoopEnd(bb_cond_else);
    cfgBuilderSetBlock(builder,bb_cond_else);

    if (!bb_prev_loop) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_LOOP);
    }

    builder->bb_cur_loop = bb_prev_loop;
}

static void cfgHandleDoWhileLoop(CFGBuilder *builder, Ast *ast) {
    Ast *ast_cond = ast->whilecond;
    Ast *ast_body = ast->whilebody;

    BasicBlock *bb_prev_loop = builder->bb_cur_loop;
    BasicBlock *bb_do_body = cfgSelectOrCreateBlock(builder,
            BB_CONTROL_BLOCK, BB_FLAG_LOOP_HEAD);

    /*
     * do {
     *    ...               <--+
     *    if (x) {             |
     *      "something\n";     |
     *    }                    |
     * } while (...); <--------+ is a branch
     *
     * */
    builder->bb_cur_loop = bb_do_body;
    builder->flags |= CFG_BUILDER_FLAG_IN_LOOP;

    cfgBuilderSetBlock(builder,bb_do_body);
    cfgHandleAstNode(builder,ast_body);

    /* converge all orphans in the do while body to point to the node in the 
     * while(...) */
    BasicBlock *bb_do_cond = cfgSelectOrCreateBlock(builder,
            BB_DO_WHILE_COND,0);
    cfgLinkLeaves(builder->leaf_cache,builder->bb,bb_do_cond);
    cfgBuilderClearLeafCache(builder);

    /* Form the loop */
    bbSetPrevPtr(bb_do_cond,bb_do_body);
    bbSetLoopEnd(bb_do_cond);


    BasicBlock *bb_do_while_next = cfgBuilderAllocBasicBlock(builder,
            BB_CONTROL_BLOCK, 0);
    bb_do_cond->next = bb_do_while_next;

    cfgBuilderSetBlock(builder,bb_do_cond);
    cfgHandleAstNode(builder,ast_cond);

    if (!bb_prev_loop) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_LOOP);
    }

    bbAddPrev(bb_do_while_next, bb_do_cond);
    bbSetPrevPtr(bb_do_while_next, bb_do_cond);
    cfgBuilderSetBlock(builder, bb_do_while_next);
    builder->bb_cur_loop = bb_prev_loop;
}

static void cfgHandleGoto(CFGBuilder *builder, Ast *ast) {
    BasicBlock *dest;
    AoStr *label = astHackedGetLabel(ast);

    bbSetType(builder->bb,BB_GOTO);

    if (builder->flags & CFG_BUILDER_FLAG_IN_LOOP) {
        builder->bb->flags |= BB_FLAG_LOOP_JUMP;
    }

    /* Try straight out the gate to resolve the goto otherwise save it for 
     * later */
    if ((dest = mapGetLen(builder->resolved_labels,
                          label->data,
                          label->len)) != NULL)
    {
        /* This forms a loop but over a potentially large number of graph nodes 
         * making it difficult to classify as a _conventional_ loop. 
         * Do we want an intermediary node? */
        bbSetPrevPtr(builder->bb,dest);
        builder->bb->flags |= BB_FLAG_GOTO_LOOP;
        bbSetNext(builder->bb,NULL);
    } else {
        listAppend(builder->unresolved_gotos,builder->bb);
    }

    /* This should just push the ast to to ast_array */
    vecPush(builder->bb->ast_array,ast);

    /* If this is an unconditional jump */
    if (!(builder->flags & CFG_BUILDER_FLAG_IN_CONDITIONAL)) {
       builder->bb->flags |= BB_FLAG_UNCONDITIONAL_JUMP;
    }
}

static void cfgHandleReturn(CFGBuilder *builder, Ast *ast) {
    BasicBlock *bb_return = cfgSelectOrCreateBlock(builder,BB_RETURN_BLOCK,0);
    BasicBlock *bb_after = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK,0);
    bbSetNext(bb_return,NULL);

    vecPush(bb_return->ast_array,ast);
    cfgBuilderSetBlock(builder,bb_after);
}

/* Splitting the label makes sense else we get into a situation where we have 
 * multiple goto labels in the same block and the code paths get muddled. */
static void cfgHandleLabel(CFGBuilder *builder, Ast *ast) {
    BasicBlock *bb_label = cfgSelectOrCreateBlock(builder,
            BB_CONTROL_BLOCK,BB_FLAG_LABEL);
    /* We want the label in the newly allocated block */
    vecPush(bb_label->ast_array,ast);
    AoStr *label = astHackedGetLabel(ast);
    mapAdd(builder->resolved_labels,label->data,bb_label);
    cfgBuilderSetBlock(builder,bb_label);
}

static void cfgHandleBreak(CFGBuilder *builder) {
    if (builder->flags & CFG_BUILDER_FLAG_IN_LOOP && 
            !(builder->flags & CFG_BUILDER_FLAG_IN_SWITCH)) {
        bbSetType(builder->bb,BB_BREAK_BLOCK);
        bbSetNext(builder->bb,builder->bb_cur_loop->_else);
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

static void cfgHandleContinue(CFGBuilder *builder) {
    assert(builder->flags & CFG_BUILDER_FLAG_IN_LOOP);
    BasicBlock *bb_continue = cfgSelectOrCreateBlock(builder,BB_CONTINUE,0);
    bbSetPrevPtr(bb_continue,builder->bb_cur_loop);
    bbAddPrev(builder->bb_cur_loop,builder->bb);
    if (bb_continue != builder->bb) {
        loggerWarning("On a different node. builder bb%d and continue bb%d\n",
                builder->bb->block_no, bb_continue->block_no);
        bbSetNext(builder->bb,bb_continue);
    }
    cfgBuilderSetBlock(builder,bb_continue);
}

static void cfgHandleCase(CFGBuilder *builder, BasicBlock *bb_end, Ast *ast) {
    BasicBlock *bb_switch = builder->bb;
    BasicBlock *bb_case = cfgBuilderAllocBasicBlock(builder,BB_CASE,0);
    vecPush(bb_switch->next_blocks,bb_case);

    /* so we can access begining and end while in the basic block and always 
     * know it is the first node in the ast array  */
    cfgBuilderSetBlock(builder,bb_case);
    vecPush(bb_case->ast_array,ast);
    bbSetPrevPtr(bb_case,bb_switch);

    if (!listEmpty(ast->case_asts)) {
        listForEach(ast->case_asts) {
            Ast *case_ast = (Ast *)it->value;
            cfgHandleAstNode(builder,case_ast);
        }
    }

    if (builder->bb != bb_case) {
        builder->bb->flags |= BB_FLAG_CASE_OWNED;
    }

    bbSetPrevPtr(bb_end,builder->bb);
    bbSetNext(builder->bb,bb_end);
    bbAddPrev(bb_case,bb_switch);
}

/* The first and last items of the next_blocks array are the switch condition 
 * and the next block respectively. This is easier to manage and theoretically
 * are different blocks but connected to the switch */
static void cfgHandleSwitch(CFGBuilder *builder, Ast *ast) {
    BasicBlock *bb_switch = cfgSelectOrCreateBlock(builder,BB_SWITCH,0);
    BasicBlock *bb_end = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK,0);

    int sp = 0;
    int prev_in_switch = builder->flags & CFG_BUILDER_FLAG_IN_SWITCH;
    static BasicBlock *bb_stack[128];

    cfgBuilderSetBlock(builder,bb_switch);
    cfgHandleAstNode(builder,ast->switch_cond);
    bb_switch->next_blocks = bbVecNew();

    if (!ast->switch_bounds_checked) {
        bb_switch->flags |= BB_FLAG_UNCONDITIONAL_JUMP;
    }
    builder->flags |= CFG_BUILDER_FLAG_IN_SWITCH;

    /* ??? - This has the potential to be a crazy loop */
    for (u64 i = 0; i < ast->cases->size; ++i) {
        cfgBuilderSetBlock(builder,bb_switch);
        Ast *_case = (Ast *)ast->cases->entries[i];
        cfgHandleCase(builder,bb_end,_case);

        if (bbAstCount(builder->bb) == 1) {
            bb_stack[sp++] = builder->bb;
        } else if (sp) {
            for (int i = 0; i < sp; i++) {
                BasicBlock *stackable = bb_stack[i];
                bbSetNext(stackable,builder->bb);
            }
            sp = 0;
        }
    }

    /* wrong ?*/
    if (ast->case_default != NULL) {
        cfgBuilderSetBlock(builder,bb_switch);
        cfgHandleCase(builder,bb_end,ast->case_default);
    }

    cfgLinkLeaves(builder->leaf_cache,bb_switch,bb_end);
    cfgBuilderClearLeafCache(builder);

    bbSetNext(bb_switch,bb_end);
    bbSetPrevPtr(bb_end,bb_switch);

    cfgBuilderSetBlock(builder,bb_end);
    if (!prev_in_switch) {
        builder->flags &= ~(CFG_BUILDER_FLAG_IN_SWITCH);
    } 
}

/* Top down through the tree finding blocks which are not pointing to anything */
static void cfgLinkLeaves(Map *map, BasicBlock *bb, BasicBlock *dest) {
    if (!mapHas(map,(void *)(long)bb->block_no)) {
        mapAdd(map,(void *)(long)bb->block_no,NULL);
        switch (bb->type) {
            case BB_SWITCH:
                for (u64 i = 0; i < bb->next_blocks->size; ++i) {
                    cfgLinkLeaves(map,bb->next_blocks->entries[i],dest);
                }
                /* FALLTHROUGH */
            case BB_DO_WHILE_COND:
            case BB_CASE:
            case BB_CONTROL_BLOCK:
                if      (bb == bb->next)   bbSetNext(bb,dest);
                else if (bb->next == NULL) bbSetNext(bb,dest);
                else if (bbAstCount(bb->next) == 0 && !bbIsLoopControl(bb->next)) bbSetNext(bb,dest);
                else cfgLinkLeaves(map,bb->next,dest);
                break;

            case BB_BRANCH_BLOCK:
                cfgLinkLeaves(map,bb->_if,dest);
                cfgLinkLeaves(map,bb->_else,dest);
                break;
        }
    }
}

static BasicBlock *cfgMergeBranches(CFGBuilder *builder, BasicBlock *pre,
        BasicBlock *post)
{ 
    Map *leaf_cache = builder->leaf_cache;
    BasicBlock *join = post;
    if (bbAstCount(post) > 0 && !bbIsLoopControl(post)) {
        join = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK,0);
        bbSetPrevPtr(join,post);
        bbSetNext(post,join);
    }
    cfgLinkLeaves(leaf_cache,pre,join);
    cfgBuilderClearLeafCache(builder);
    return join;
}

/**
 * This function is crucial. It sets the type of block based on the AST. 
 * @DataFlow Jamesbarford 2024/08/28
 * TODO: This function should be the one responsible for converting the ast 
 * into IR and adding what a block defines or modifies which will make liveness
 * analysis easier as64 with dataflow.
 * */
static void cfgHandleAstNode(CFGBuilder *builder, Ast *ast) {
    assert(ast != NULL);
    int kind = ast->kind;
    BasicBlock *bb = builder->bb;

    /* We are only interested in the AST nodes that are the start of a new 
     * basic block or a top level `I64 x = 10;` type declaration or assignment 
     * 
     * A new basic block will start with an `if`, `for`, `while`, `goto` */
    switch (kind) {
        case AST_BREAK:         cfgHandleBreak(builder);        break;
        case AST_COMPOUND_STMT: cfgHandleCompound(builder,ast); break;
        case AST_CONTINUE:      cfgHandleContinue(builder);     break;
        case AST_DO_WHILE: {
            cfgHandleDoWhileLoop(builder,ast);
            break;
        }

        case AST_FOR: {
            BasicBlock *pre = builder->bb;
            cfgHandleForLoop(builder,ast);
            /* this is the loop back */
            if (!bbIsType(pre, BB_BRANCH_BLOCK)) {
                pre = pre->next;
            }
            BasicBlock *post = builder->bb;
            BasicBlock *join = cfgMergeBranches(builder,pre,post);
            cfgBuilderSetBlock(builder,join);
            break;
        }

        case AST_WHILE: {
            BasicBlock *pre = builder->bb;
            cfgHandleWhileLoop(builder,ast);
            BasicBlock *post = builder->bb;
            if (!bbIsType(pre, BB_BRANCH_BLOCK)) {
                pre = pre->next;
            }
            BasicBlock *join = cfgMergeBranches(builder,pre,post);
            cfgBuilderSetBlock(builder,join);
            break;
        }

        case AST_IF: {
            BasicBlock *pre = builder->bb;
            cgfHandleBranchBlock(builder,ast);
            BasicBlock *post;
            if (!bbIsType(pre, BB_BRANCH_BLOCK)) {
                pre = pre->next;
            }
            post = builder->bb;
            BasicBlock *join = cfgMergeBranches(builder,pre,post);
            cfgBuilderSetBlock(builder,join);
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

        case AST_ASM_FUNCDEF:
        case AST_ASM_FUNC_BIND:
        case AST_ASM_STMT:
        case AST_CAST:
        case AST_CLASS_REF:
        case AST_DEFAULT_PARAM:
        case AST_EXTERN_FUNC:
        case AST_FUNPTR:
        case AST_FUN_PROTO:
        case AST_LITERAL:
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
        case AST_LVAR:
            vecPush(bb->ast_array,ast);
            break;
        /* Start of a variable being alive (possibly?) */
        case AST_DECL:
        default:
            /* Need to look at this for binary operators */
            vecPush(bb->ast_array,ast);
            break;
    }
}

/* Remove nodes from the ast array that are before the label */
//static void cfgRemoveNodesBeforeLabel(BasicBlock *bb_dest, AoStr *goto_label) {
//    int collect = 0;
//    int new_size = 0;
//    Ast *ast = bb_dest->ast_array->entries[0];
//    /* If the first node in this block is the goto label we do not need 
//     * to do anything */
//    if (astIsLabelMatch(ast,goto_label)) return;
//
//    for (int i = 1; i < bb_dest->ast_array->size; ++i) {
//        ast = vecGet(Ast *,bb_dest->ast_array,i);
//        if (collect) {
//            bb_dest->ast_array->entries[new_size++] = ast;
//        } else {
//            if (astIsLabelMatch(ast,goto_label)) {
//                /* Start adding nodes now that we have seen the label */
//                collect = 1;
//                bb_dest->ast_array->entries[new_size++] = ast;
//            }
//        }
//    }
//    /* The entries pointer array will have nodes from the previous array and 
//     * possibly doubles, however we won't reach them as we truncate the vectors 
//     * size */
//    bb_dest->ast_array->size = new_size;
//}

/* a lot of the code in this function is absolutely ridiculous, as 
 * what you can do with goto is absolutely ridiculous */
static void cfgRelocateGoto(CFGBuilder *builder, BasicBlock *bb_goto,
        BasicBlock *bb_dest, AoStr *goto_label)
{
    /* Can happen... if the block we want to goto is itself */
    if (bb_goto == bb_dest) {
        return;
    }

    BasicBlock *bb_prev = bb_goto->prev;

    if (bbIsType(bb_dest,BB_LOOP_BLOCK) && !(bb_goto->flags & BB_FLAG_LOOP_JUMP)) {
        if (bbIsType(bb_prev, BB_BRANCH_BLOCK)) {
            if (bb_prev->_if == bb_goto) bb_goto->flags |= BB_FLAG_IF_BRANCH;
            else                         bb_goto->flags |= BB_FLAG_ELSE_BRANCH;
        }

        Vec *dest_ast_array = bb_dest->ast_array;
        BasicBlock *loop_cond = bb_dest->prev;
        BasicBlock *loop_back = cfgBuilderAllocBasicBlock(builder,BB_LOOP_BLOCK,0);

        Ast *asts_to_move[100];
        int ast_move_cnt = 0;

        /* We need to get the statements that happened before the 
         * label */
        for (u64 i = 0; i < dest_ast_array->size; ++i) {
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
            vecPush(loop_back->ast_array,asts_to_move[i]);
        }

        /* resize the array */
        dest_ast_array->size -= ast_move_cnt;

        /* Remove asts that no longer exist in the block */
        for (u64 i = 0; i < dest_ast_array->size; ++i) {
            dest_ast_array->entries[i] = dest_ast_array->entries[ast_move_cnt+i];
        }


        /* Obvious, the goto has to point to it's destination */
        bbSetNext(bb_goto,bb_dest);

        /* Destination, whatever it might be, is the loop head 
         * @Test - how does this work if we jump into an if branch?*/
        bbSetNext(bb_dest,loop_cond);
        bbSetPrevPtr(bb_dest,bb_goto);
        bb_dest->flags = BB_FLAG_LOOP_HEAD;
        bbSetType(bb_dest,BB_CONTROL_BLOCK);

        bbSetPrevPtr(loop_back,bb_dest);

        /* the loop condition is now after the goto's destination */
        loop_cond->_if = loop_back;
        bbSetLoopEnd(loop_cond->_else);
        bbSetType(loop_cond,BB_BRANCH_BLOCK);
        loop_cond->flags = 0;

        bbAddPrev(loop_back,loop_cond);
        bbAddPrev(loop_back,bb_dest);
        bbAddPrev(bb_dest,loop_back);
    } else if (bbIsType(bb_goto,BB_LOOP_BLOCK)) {
        BasicBlock *bb_loop_head = bb_goto->prev;
        bbAddPrev(bb_dest,bb_goto);
        bbSetType(bb_goto,BB_CONTROL_BLOCK);
        bbSetNext(bb_goto,bb_dest);
        bb_goto->flags &= ~(BB_FLAG_LOOP_END);
        bb_loop_head->flags &= ~(BB_FLAG_LOOP_HEAD);
        bb_loop_head->_else->flags &= ~(BB_FLAG_LOOP_END);
    } else {
        bbAddPrev(bb_dest,bb_goto);

        /* Making a backwards goto loop */
        if (bb_goto->flags & BB_FLAG_GOTO_LOOP) bbSetPrevPtr(bb_goto,bb_dest);
        else                                    bbSetNext(bb_goto,bb_dest);

        /* We don't want to destroy the loop */
        if (!bbIsType(bb_dest,BB_LOOP_BLOCK) &&
                !(bb_dest->flags & BB_FLAG_GOTO_LOOP)) {
            bbSetPrevPtr(bb_dest,bb_goto);
        }
    }
}

static void cfgAdjacencyPrintBlock(BasicBlock *bb, int is_parent) {
    char *bb_string = bbToJSON(bb);
    if (is_parent) printf("%s\n",bb_string);
    else           printf("child %s\n",bb_string);
}

BasicBlock *cfgVecFindBlock(Vec *vec, int block_no, int *_idx) {
    for (u64 i = 0; i < vec->size; ++i) {
        BasicBlock *bb = vecGet(BasicBlock *,vec,i);
        if (bb->block_no == block_no) {
            *_idx = i;
            return bb;
        }
    }
    *_idx = -1;
    return NULL;
}

void cfgAdjacencyListPrint(CFG *cfg) {
    MapIter it;
    mapIterInit(cfg->no_to_block, &it);
    
    while (mapIterNext(&it)) {
        MapNode *entry = it.node;
        BasicBlock *parent = entry->value;
        Set *iset = (Set *)mapGet(cfg->graph,entry->key);

        cfgAdjacencyPrintBlock(parent,1);

        SetIter set_it;
        setIterInit(iset, &set_it);

        while (setIterNext(&set_it)) {
            BasicBlock *bb = (BasicBlock *)mapGet(cfg->no_to_block,set_it.value);
            cfgAdjacencyPrintBlock(bb,0);
        }
        printf("====\n");
    }
}

/* Print the Adjacency list graph representation */
void cfgAdjacencyListPrintShallow(CFG *cfg) {
    MapIter it;
    mapIterInit(cfg->no_to_block, &it);

    while (mapIterNext(&it)) {
        BasicBlock *bb = (BasicBlock *)it.node->value;
        char *bb_string = bbToJSON(bb);
        printf("%s\n====\n",bb_string);
    }
}

AoStr *cdfAdjacencyListToJSON(CFG *cfg) {
    AoStr *json = aoStrNew();
    MapIter it;
    mapIterInit(cfg->no_to_block, &it);

    while (mapIterNext(&it)) {
        MapNode *entry = it.node;
        BasicBlock *parent = (BasicBlock *)entry->value;
        Set *iset = (Set *)mapGet(cfg->graph,entry->key);

        aoStrCatPrintf(json, "    %d: [", parent->block_no);

        SetIter set_iter;
        setIterInit(iset, &set_iter);

        while (setIterNext(&set_iter)) {
            BasicBlock *bb = (BasicBlock *)mapGet(cfg->no_to_block,set_iter.value);
            aoStrCatPrintf(json,"%d",bb->block_no);
            if (set_iter.idx + 1 != iset->size) {
                aoStrCat(json,", ");
            }
        }
        aoStrCat(json,"]");

        if (it.idx + 1 != it.map->size) {
            aoStrCat(json,",");
        }
        aoStrPutChar(json,'\n');
    }
    aoStrPutChar(json,'}');
    return json;
}

int cfgRelinkVector(Map *map,
                    BasicBlock *parent,
                    BasicBlock *child,
                    BasicBlock *bb)
{
    Set *iset = mapGet(map,(void *)(long)parent->block_no);
    loggerWarning("Vector relinking. Parent bb%d, child bb%d, block: bb%d\n",
            parent->block_no, child->block_no, bb->block_no);
    if (iset) {
        setRemove(iset,(void *)(long)bb->block_no);
        setAdd(iset,(void *)(long)child->block_no);
        /* @Bug - this is close but not quite working when creating strings.HC
         * CFG */
        child->flags |= bb->flags;
        if (bbAstCount(bb) == 0) {
            bbSetType(bb,BB_GARBAGE);
        }
        return 1;
    }
    loggerWarning("fail 2 relink: %d\n",bb->block_no);
    bbSetType(bb,BB_GARBAGE);
    return 0;
}

/* Returns the parent */
BasicBlock *bbRelinkControl(Map *map, BasicBlock *bb) {
     /* @Bug
      * What if the map has had deletions? */
    BasicBlock *parent = mapGetAt(bb->prev_blocks,0);
    BasicBlock *child = bb->next;

    if (bbIsType(child,BB_CONTROL_BLOCK) && bbIsType(bb,BB_CONTROL_BLOCK)) {
        if (child->flags & BB_FLAG_CASE_OWNED) {
            if (bbAstCount(child) == 0 && child->next && bbAstCount(child->next) > 0) {
                child = child->next;
            }
        }
    }

    switch (parent->type) {
        case BB_CONTROL_BLOCK: {
            bbSetNext(parent,child);
            bbSetPrevPtr(child,parent);
            bbSetType(bb,BB_GARBAGE);
            break;
        }

        case BB_BRANCH_BLOCK: {
            if (parent->_if == bb) {
                bbSetIf(parent,child);
                bbAddPrev(child,parent->_if);
            } else if (parent->_else == bb) {
                bbSetElse(parent,child);
                if (bb == child) {
                    bbSetType(bb,BB_END_BLOCK);
                    bbSetNext(bb,NULL);
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

/* Remove the basic block from:
 * - The block number to block hashtable 
 * - All sets with the node as a previous
 */
void cfgDeleteBasicBlock(CFG *cfg, BasicBlock *bb_del) {
    MapIter it;
    mapIterInit(cfg->graph, &it);
    bbSetType(bb_del,BB_GARBAGE);

    while (mapIterNext(&it)) {
        MapNode *entry = it.node;
        if ((long)entry->key == (long)bb_del->block_no) continue;
        Set *bb_connections = (Set *)entry->value;
        BasicBlock *bb = (BasicBlock *)mapGet(cfg->no_to_block,entry->key);
        /* Remove node as a previous link */
        void *casted_key = (void *)(long)bb->block_no;
        mapRemove(bb->prev_blocks,casted_key);
        /* Remove this node as a connection */
        setRemove(bb_connections,casted_key);
    }

    /* Remove node from both lookup tables */
    void *casted_key = (void *)(long)bb_del->block_no;
    mapRemove(cfg->graph,casted_key);
    mapRemove(cfg->no_to_block,casted_key);
}

/* For when the next pointer of a switch is a dead end, the most likely 
 * cause of this is that it is nested in a conditional branch and does not 
 * point to the node beyond the branch. */
static void cfgRelinkSwitch(CFG *cfg, BasicBlock *bb_switch) {
    BasicBlock *dest = NULL;
    BasicBlock *bb_end = bb_switch->next;

    if (bb_switch->flags & (BB_FLAG_IF_BRANCH|BB_FLAG_ELSE_BRANCH)) {
        BasicBlock *cond = bb_switch->prev;
        bbSetType(bb_end,BB_GARBAGE);
        if (cond->flags & BB_FLAG_HAD_NO_ELSE) {
            dest = cond->_else;
        } else {
            dest = cond->next;
        }
        Map *cache = mapNew(32, &map_int_to_basic_block_type);
        Vec *blocks = bb_switch->next_blocks;
        /* Recurse through the graph linking all leaf nodes to the destination 
         * node, this is fairly expensive. */
        for (u64 i = 0; i < blocks->size; ++i) {
            BasicBlock *bb_case = vecGet(BasicBlock *,blocks,i);
            cfgLinkLeaves(cache,bb_case,dest);
        }
        mapRelease(cache);
        bbSetNext(bb_switch,dest);
        cfgDeleteBasicBlock(cfg,bb_end);
    } else {
        /* legitimately the switch does not have anything after it */
        bbSetType(bb_end,BB_END_BLOCK);
    }
}

static Set *cfgCreateSwitchAdjacencyList(CFG *cfg, BasicBlock *bb) {
    Set *iset = setNew(16, &set_int_type);
    for (u64 i = 0; i < bb->next_blocks->size; ++i) {
        BasicBlock *bb_case = (BasicBlock *)bb->next_blocks->entries[i];
        setAdd(iset,(void *)(long)bb_case->block_no);
        bbAddPrev(bb_case,bb);
    }

    if (bbIsType(bb->next,BB_CONTROL_BLOCK) && bbAstCount(bb->next) == 0) {
        cfgRelinkSwitch(cfg,bb);
    } else {
        setAdd(iset,(void *)(long)bb->next->block_no);
        bbAddPrev(bb->next,bb);
    }
    return iset;
}

/* This is a check for an orphaned node */
static Set *cfgCreateControlAdjacencyList(CFG *cfg, BasicBlock *bb) {
    /* @Bug
     * This is where a return block is the next, potentially there 
     * is not next either */
    if (!bb->prev) {
        bbSetType(bb,BB_GARBAGE);
        return NULL;
    }

    /* @Bug 
     * This happens with a case */
    if (bb->prev_blocks->size == 0 && bbAstCount(bb) == 0 && 
            (bb->flags & (BB_FLAG_CASE_BREAK|BB_FLAG_CASE_OWNED))) 
    {
        bbSetType(bb,BB_GARBAGE);
        return NULL;
    }

    if (!(bb->flags & BB_FLAG_CASE_OWNED) && (bbAstCount(bb) == 0 || bb->next == NULL)) {
        if (bbAstCount(bb) > 0) {
            bbSetType(bb,BB_END_BLOCK);
        } else {
            if (bb->next) {
                loggerWarning("GARBAGE: bb%d\n",bb->block_no);
                bbRelinkControl(cfg->graph,bb);
                if (bbIsType(bb, BB_GARBAGE)) {
                    cfgDeleteBasicBlock(cfg,bb);
                    return NULL;
                }
            } else {
                if (bb->prev && bbIsType(bb->prev, BB_SWITCH)) {
                    bbSetType(bb,BB_END_BLOCK);
                    Set *iset = setNew(8, &set_int_type);
                    return iset;
                } else {
                    bbSetType(bb,BB_GARBAGE);
                    return NULL;
                }
            }
        }
    } else if (bb->next == bb) {
        Set *iset = setNew(8, &set_int_type);
        setAdd(iset,(void *)(long)bb->next->block_no);
        bbSetType(bb,BB_END_BLOCK);
        bbSetNext(bb,NULL);
        return iset;
    }
    if (!bbIsType(bb, BB_END_BLOCK)) {
        Set *iset = setNew(8, &set_int_type);
        setAdd(iset,(void *)(long)bb->next->block_no);
        bbAddPrev(bb->next,bb);
        return iset;
    } else {
        Set *iset = setNew(8, &set_int_type);
        return iset;
    }
}

static Set *cfgCreateGotoAdjacencyList(CFG *cfg, BasicBlock *bb) {
    (void)cfg;
    Set *iset = setNew(8, &set_int_type);
    if (bb->flags & BB_FLAG_GOTO_LOOP) {
        setAdd(iset,(void *)(long)bb->prev->block_no);
        bbAddPrev(bb->prev,bb);
    } else {
        setAdd(iset,(void *)(long)bb->next->block_no);
        bbAddPrev(bb->next,bb);
    }
    return iset;
}

static Set *cfgCreateBranchAdjacencyList(CFG *cfg, BasicBlock *bb) {
    (void)cfg;
    Set *iset = setNew(8, &set_int_type);
    setAdd(iset,(void *)(long)bb->_if->block_no);
    setAdd(iset,(void *)(long)bb->_else->block_no);
    bbAddPrev(bb->_if,bb);
    bbAddPrev(bb->_else,bb);
    return iset;
}

static Set *cfgCreateContinueAdjacencyList(CFG *cfg, BasicBlock *bb) {
    (void)cfg;
    Set *iset = setNew(8, &set_int_type);
    BasicBlock *loop_head = bb->prev;

    assert(bb->prev != NULL);

    setAdd(iset,(void *)(long)bb->prev->block_no);
    bbAddPrev(loop_head,bb);

    if (bb->next) {
        bbRemovePrev(bb->next,bb->block_no);
        bbSetNext(bb,NULL);
    }

    if (loop_head->flags & (BB_FLAG_FOR_LOOP|BB_FLAG_FOR_LOOP_HAS_STEP)) {
        BasicBlock *loop_block = loop_head->next;
        /* this push to the bb->ast_array is ok*/
        vecPush(bb->ast_array,vecTail(Ast *,loop_block->ast_array));
    }
    return iset;
}

static Set *cfgCreateLoopAdjacencyList(CFG *cfg, BasicBlock *bb) {
    (void)cfg;
    Set *iset = setNew(8, &set_int_type);
    BasicBlock *loop_head = bb->prev;
    setAdd(iset,(void *)(long)loop_head->block_no);
    bbAddPrev(loop_head,bb);
    return iset;
}

static Set *cfgCreateDoWhileAdjacencyList(CFG *cfg, BasicBlock *bb) {
    (void)cfg;
    Set *iset = setNew(16, &set_int_type);
    BasicBlock *loop_head = bb->prev;
    bbAddPrev(loop_head,bb);
    if (bb->next) {
        bbAddPrev(bb->next,bb);
    }
    setAdd(iset,(void *)(long)loop_head->block_no);
    return iset;
}

/**
 * This is for creating a very compact control flow graph that feels more like 
 * a classical datascructure than the basic block node graph. It's significantly 
 * faster to traverse and also easier to reason with.
 *
 * This also fixes orphaned nodes, so is acting like a final check before 
 * handing off to the next step.
 * */
void cfgBuildAdjacencyList(CFG *cfg) {
    Set *iset;
    MemPoolIterator *it = memPoolIteratorNew(cfg->_memory);
    BasicBlock *bb;
    while ((bb = memPoolNext(it)) != NULL) {
        if (bbIsType(bb, BB_GARBAGE)) {
            continue;
        }

        /* The order of _if _else and prev/next is of PARAMOUNT importance */
        switch (bb->type) {
            case BB_BREAK_BLOCK:
            case BB_HEAD_BLOCK:
            case BB_CASE:
                iset = setNew(8, &set_int_type);
                setAdd(iset,(void *)(long)bb->next->block_no);
                bbAddPrev(bb->next,bb);
                break;
            case BB_RETURN_BLOCK:
            case BB_END_BLOCK:
                iset = setNew(8, &set_int_type);
                break;
            case BB_CONTROL_BLOCK:
                iset = cfgCreateControlAdjacencyList(cfg,bb);
                if (!iset) continue;
                break;
            case BB_GOTO:
                iset = cfgCreateGotoAdjacencyList(cfg,bb); 
                break;
            case BB_BRANCH_BLOCK:
                iset = cfgCreateBranchAdjacencyList(cfg,bb);
                break;
            case BB_SWITCH:
                iset = cfgCreateSwitchAdjacencyList(cfg,bb);
                break;
            case BB_CONTINUE:
                iset = cfgCreateContinueAdjacencyList(cfg,bb);
                break;
            case BB_LOOP_BLOCK:
                iset = cfgCreateLoopAdjacencyList(cfg,bb);
                break;
            case BB_DO_WHILE_COND:
                iset = cfgCreateDoWhileAdjacencyList(cfg,bb);
                break;

            default:
                loggerPanic("Unhandled type: %s\n", bbTypeToString(bb->type));
        }
        mapAdd(cfg->no_to_block,(void *)(long)bb->block_no,bb);
        mapAdd(cfg->graph,(void *)(long)bb->block_no,iset);
    }
    memPoolIteratorRelease(it);
}

/* Given a parent with no code paths to the node remove the node from all of 
 * it's child nodes */
void cfgRemoveNodeFromChild(BasicBlock *parent) {
    switch (parent->type) {
        case BB_GARBAGE:
        case BB_END_BLOCK:
        case BB_HEAD_BLOCK:
        case BB_RETURN_BLOCK:
            break;

        case BB_CONTROL_BLOCK: {
            bbRemovePrev(parent->next,parent->block_no);
            break;
        }

        case BB_BRANCH_BLOCK:
            bbRemovePrev(parent->_if,parent->block_no);
            bbRemovePrev(parent->_else,parent->block_no);
            break;

        case BB_LOOP_BLOCK:
            bbRemovePrev(parent->prev,parent->block_no);
            break;

        case BB_BREAK_BLOCK:
            bbRemovePrev(parent->next,parent->block_no);
            break;

        case BB_DO_WHILE_COND:
            bbRemovePrev(parent->prev,parent->block_no);
            if (parent->next) {
                bbRemovePrev(parent->next,parent->block_no);
            }
            break;

        case BB_GOTO:
            if (parent->flags & (BB_FLAG_GOTO_LOOP)) {
                bbRemovePrev(parent->prev,parent->block_no);
            } else {
                bbRemovePrev(parent->next,parent->block_no);
            }
            break;

        case BB_SWITCH:
            /* We're nuking the thing after the switch */
            bbRemovePrev(parent->next,parent->block_no);
            break;

        case BB_CASE:
            loggerPanic("Cannot have an orphaned case\n");
            break;

        case BB_CONTINUE:
            loggerWarning("Continue nuked\n");
            bbRemovePrev(parent->prev,parent->block_no);
            break;
    }
}

/* BFS flagging nodes as deleted and removing from their childrens previous 
 * array */
void cfgRemoveUnreachableNodes(CFG *cfg) {
    MapIter it;
    int ids[64];
    int id_cnt = 0;
    mapIterInit(cfg->no_to_block, &it);

    while (mapIterNext(&it)) {
        BasicBlock *bb = (BasicBlock *)it.node->value;
        if (!bbIsType(bb, BB_HEAD_BLOCK) && bbPrevCnt(bb) == 0) {
            /* Node can be deleted and we need to nuke all paths from it, 
             * deleting nodes if their previous contained only this node */
            ids[id_cnt++] = bb->block_no;
            loggerWarning("Unreachable: %s\n", bbToString(bb));
            cfgRemoveNodeFromChild(bb);
            bbSetType(bb,BB_GARBAGE);
        }
    }

    for (int i = 0; i < id_cnt; ++i) {
        s64 block_no = (long)ids[i];
        mapRemove(cfg->graph,(void *)block_no);
        mapRemove(cfg->no_to_block,(void *)block_no);
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
    listForEach(builder->unresolved_gotos) {
        BasicBlock *bb_goto = cast(BasicBlock *,it->value);
        BasicBlock *bb_dest = NULL;
        Ast *ast = NULL; 

        for (u64 i = 0; i < bb_goto->ast_array->size; ++i) {
            ast = (Ast *)bb_goto->ast_array->entries[i];
            if (ast->kind == AST_GOTO || ast->kind == AST_JUMP) {
                break;
            }
        }

        AoStr *goto_label = astHackedGetLabel(ast);
        AoStr *dest_label = astHackedGetLabel(ast);

        assert(ast->kind == AST_GOTO || ast->kind == AST_JUMP);

        bb_dest = mapGetLen(builder->resolved_labels,
                            dest_label->data,
                            dest_label->len);

        assert(bb_dest != NULL);
        cfgRelocateGoto(builder,bb_goto,bb_dest,goto_label);
    }
}


static CFG *cfgCreateForFunction(Cctrl *cc, Ast *ast_fn, Map *leaf_cache, int *_block_no) {
    CFGBuilder builder;
    *_block_no += 1;

    cfgBuilderInit(&builder,cc,leaf_cache,*_block_no);

    loggerDebug("creating CFG for: %s\n", ast_fn->fname->data);

    BasicBlock *bb = cfgBuilderAllocBasicBlock(&builder,BB_HEAD_BLOCK,0);
    BasicBlock *bb_next = cfgBuilderAllocBasicBlock(&builder,BB_CONTROL_BLOCK,0);
    CFG *cfg = cfgNew(ast_fn->fname,bb);
    bbSetNext(bb,bb_next);
    bbSetPrevPtr(bb_next,bb);
    builder.cfg = cfg;
    builder.bb = bb_next;
    cfgConstructFunction(&builder,ast_fn->body->stms);

    /* This is a function with no body */
    if (builder.bb == bb->next && !bbIsType(bb->next, BB_RETURN_BLOCK)) {
        BasicBlock *bb_return = cfgBuilderAllocBasicBlock(&builder,
                                                          BB_RETURN_BLOCK,0);
        bbSetNext(bb->next,bb_return);
        bbSetPrevPtr(bb_return,bb->next);
        bbSetNext(bb_return,NULL);
        mapAdd(cfg->no_to_block,(void *)(long)bb_return->block_no,bb_return);
    }

    cfg->bb_count = builder.bb_count;
    cfg->_memory = builder.block_pool;
    *_block_no = builder.bb_block_no+1;
    cfgBuildAdjacencyList(cfg);
    cfgRemoveUnreachableNodes(cfg);

    mapRelease(builder.resolved_labels);
    listRelease(builder.unresolved_gotos,NULL);
    return cfg;
}

static void _bbFindAllLoopNodes(BasicBlock *loop_head,
                                BasicBlock *bb,
                                Map *nodes,
                                int loop_cnt)
{
    int is_start_of_new_loop = bb != loop_head && bb->flags & BB_FLAG_LOOP_HEAD;
    int types[] = {BB_END_BLOCK,BB_RETURN_BLOCK,BB_HEAD_BLOCK};

    if (bbIsLoopEnd(bb)) {
        if (bb->prev == loop_head) {
            mapAdd(nodes,(void *)(long)bb->block_no,bb);
            loop_cnt--;
            return;
        }
        if (loop_cnt > 1) {
            mapAdd(nodes,(void *)(long)bb->block_no,bb);
        }
    } else if (!bbOneOfType(bb,sizeof(types)/sizeof(types[0]),types)) {
        mapAdd(nodes,(void *)(long)bb->block_no,bb);
    }

    if (is_start_of_new_loop) {
        loop_cnt++;
    }

    if (bbIsType(bb, BB_BREAK_BLOCK)) {
        /* Add it and it is up to the caller to handle what a break means */
        mapAdd(nodes,(void *)(long)bb->block_no,bb);
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
            for (u64 i = 0; i < bb->next_blocks->size; ++i) {
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

    if (bbIsLoopEnd(bb)) {
        loop_cnt--;
    }
}

Map *bbFindAllLoopNodes(BasicBlock *loop_head) {
    int loop_cnt = 0;
    Map *map = mapNew(32, &map_int_to_basic_block_type);
    mapAdd(map,(void *)(long)loop_head->block_no,loop_head);
    _bbFindAllLoopNodes(loop_head,loop_head,map,loop_cnt);

    MapIter it;
    mapIterInit(loop_head->prev_blocks, &it);
    while (mapIterNext(&it)) {
        BasicBlock *prev = (BasicBlock *)it.node->value;
        if (bbIsType(prev, BB_LOOP_BLOCK) && prev->prev == loop_head) {
            break;
        }
    }
    return map;
}

/* DFS our way through the graph, as it is created in the correct order above 
 * we get the nodes in the correct order here. */
void cfgExplore(CFG *cfg, Set *seen, int block_no) {
    if (setHas(seen,(void *)(long)block_no)) return;
    setAdd(seen,(void *)(long)block_no);
    Set *iset = (Set *)mapGet(cfg->graph,(void *)(long)block_no);
    printf("bb%d\n",block_no);

    SetIter it;
    setIterInit(iset, &it);
    while (setIterNext(&it)) {
        s64 next_block_no = (long)it.value;
        BasicBlock *bb = mapGet(cfg->no_to_block,(void *)(long)next_block_no);
        cfgExplore(cfg,seen,bb->block_no);
    }
}

void cfgIter(CFG *cfg) {
    Set *seen = setNew(32, &set_int_type);
    MapIter it;
    mapIterInit(cfg->no_to_block, &it);
    while (mapIterNext(&it)) {
        BasicBlock *bb = (BasicBlock *)it.node->value;
        cfgExplore(cfg,seen,bb->block_no);
    }
    setRelease(seen);
}

/* This returns a vector of CFG's */
Vec *cfgConstruct(Cctrl *cc) {
    Vec *cfgs = vecNew(&vec_cfg_type);
    int block_no = 0;
    Map *leaf_cache = mapNew(32, &map_int_to_basic_block_type);
    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        /* XXX: What do we want to do with global variables? */
        if (ast->kind == AST_FUNC) {
            CFG *cfg = cfgCreateForFunction(cc,ast,leaf_cache,&block_no);
            vecPush(cfgs,cfg);
        }
    }
    mapRelease(leaf_cache);
    return cfgs;
}
