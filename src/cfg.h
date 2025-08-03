#ifndef CFG_H__
#define CFG_H__

#include "aostr.h"
#include "cctrl.h"
#include "containers.h"
#include "mempool.h"
#include "types.h"

enum bbType {
    BB_GARBAGE       = -1,
    BB_END_BLOCK     = 0,
    BB_HEAD_BLOCK    = 1,
    BB_CONTROL_BLOCK = 2,
    BB_BRANCH_BLOCK  = 3,
    BB_LOOP_BLOCK    = 4,
    BB_RETURN_BLOCK  = 5,
    BB_BREAK_BLOCK   = 6,
    BB_DO_WHILE_COND = 7,
    BB_GOTO          = 8,
    BB_SWITCH        = 9,
    BB_CASE          = 10,
    BB_CONTINUE      = 11,
};

#define CFG_MAX_PREV      (32)
#define CFG_MAX_CONTINUES (32)

/* These are builder flags */
#define CFG_BUILDER_FLAG_IN_CONDITIONAL (0x1)
#define CFG_BUILDER_FLAG_IN_LOOP        (0x2)
#define CFG_BUILDER_FLAG_IN_SWITCH      (0x4)

/* @FlagManagement - this is getting a bit ridiculous */
#define BB_FLAG_LOOP_HEAD          (0x1)
#define BB_FLAG_LOOP_END           (0x2)
#define BB_FLAG_REDUNDANT_LOOP     (0x4)
#define BB_FLAG_UNCONDITIONAL_JUMP (0x8)
#define BB_FLAG_LABEL              (0x10)
#define BB_FLAG_ELSE_BRANCH        (0x20)
#define BB_FLAG_IF_BRANCH          (0x40)
#define BB_FLAG_LOOP_JUMP          (0x80) /* this is for when a goto is jumping
                                           * out of a loop */
#define BB_FLAG_GOTO_LOOP          (0x100) /* this is for when a goto forms a 
                                            * loop */
#define BB_FLAG_CASE_OWNED         (0x200)
#define BB_FLAG_WHILE_LOOP         (0x400)
#define BB_FLAG_CASE_BREAK         (0x800)
#define BB_FLAG_FOR_LOOP           (0x1000)
#define BB_FLAG_FOR_LOOP_HAS_STEP  (0x2000)
#define BB_FLAG_HAD_NO_ELSE        (0x4000) /* Where an if branch had no written
                                             * else branch */

typedef struct BasicBlock {
    /* @Confirm:
     * Should we stick ssa on this struct? */
    int type;
    /* For denoting a loop head / end, not a big fan of having flags AND 
     * a type field but I presently can't think of a better way for loops. */
    u32 flags;
    int block_no;
    int visited;

    struct BasicBlock *_if;
    struct BasicBlock *_else;
    struct BasicBlock *prev;
    /* For a control block points to the next node, for a branch that is a 
     * loop head the next pointer is the BB_LOOP_BLOCK, so we can keep track of 
     * it more easily */
    struct BasicBlock *next;
    Map *prev_blocks;
    /* this is to be able to handle a switch */
    Vec *next_blocks;
    Vec *ast_array;
} BasicBlock;

#define bbPrevCnt(bb) \
  ((bb)->prev_blocks->size)

/* Head of a CFG is a function */
typedef struct CFG {
    /* CFG Does not own the function name, the AST does */
    AoStr *ref_fname;
    /* This head block and start of the CFG */
    BasicBlock *head;
    /* How many basic blocks are in the graph */
    int bb_count;
    /* A more compact representation of the graph, a block number to its 
     * connected blocks (not including previous nodes)
     * [block_no] => {block, block...}*/
    Map *graph;
    /* Block number to it's block struct
     * [block_no] => block 
     * hashtable */
    Map *no_to_block;
    /* This a pointer to the memory pool which holds all of the basic blocks */
    MemPool *_memory;
} CFG;

typedef struct CFGBuilder {
    int bb_count;
    int bb_block_no;
    u64 flags;
    Cctrl *cc;
    CFG *cfg;
    MemPool *block_pool;

    BasicBlock *bb;
    BasicBlock *bb_cur_loop;
    Map *leaf_cache;

    List *ast_list;
    List *ast_iter;
    List *unresolved_gotos;
    Map *resolved_labels;
} CFGBuilder;

BasicBlock *bbNew(int type);
BasicBlock *bbAddNext(BasicBlock *cur, int type, BasicBlock *next);
char *bbTypeToString(int type);
char *bbFlagsToString(u32 flags);
char *bbPreviousBlockNumbersToString(BasicBlock *bb);
int bbPrevHas(BasicBlock *bb, int block_no);
BasicBlock *cfgGet(CFG *cfg, int block_no);
Vec *cfgConstruct(Cctrl *cc);
void bbPrint(BasicBlock *bb);
void bbPrintNoAst(BasicBlock *bb);
char *bbToString(BasicBlock *bb);
char *bbToJSON(BasicBlock *bb);

#endif
