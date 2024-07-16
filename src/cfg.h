#ifndef CFG_H__
#define CFG_H__

#include "aostr.h"
#include "cctrl.h"
#include "map.h"

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
};

#define CFG_MAX_PREV    (32)

/* These are builder flags */
#define CFG_BUILDER_FLAG_IN_CONDITIONAL (0x1)
#define CFG_BUILDER_FLAG_IN_LOOP        (0x2)

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

typedef struct BasicBlock {
    /* @Confirm:
     * Should we stick ssa on this struct? */
    int type;
    /* For denoting a loop head / end, not a big fan of having flags AND 
     * a type field but I presently can't think of a better way for loops. As
     * the loop head in BasicBlock land is a conditional jump */
    unsigned int flags;
    int block_no;
    /* @Unused ? */
    int prev_cnt;
    int visited;
    struct BasicBlock *_if;
    struct BasicBlock *_else;
    struct BasicBlock *prev;
 //    IntSet *prev_block_ids;
    struct BasicBlock *prev_blocks[32];
    /* this is to be able to handle a switch */
    PtrVec *next_blocks;
    /* this is for most other blocks */
    struct BasicBlock *next;
    PtrVec *ast_array;
} BasicBlock;

/* Head of a CFG is a function */
typedef struct CFG {
    /* CFG Does not own the function name, the AST does */
    aoStr *ref_fname;
    /* This head block is also a pointer to the begining of `bb_pool` in 
     * the `CFGBuilder` so freeing this frees every `BasicBlock` */
    BasicBlock *head;
    /* How many basic blocks are in the graph */
    int bb_count;
    /* A more compact representation of the graph */
    IntMap *graph;
    /* This a pointer to the memory pool which holds all of the basic blocks,
     * why am I not using head?*/
    void *_memory;
} CFG;

typedef struct CFGBuilder {
    int bb_count;
    unsigned long flags;
    Cctrl *cc;
    CFG *cfg;

    BasicBlock *bb;
    BasicBlock *bb_pool;
    BasicBlock *bb_cur_loop;
    BasicBlock *bb_cur_else;

    List *ast_list;
    List *ast_iter;
    int bb_pos;
    int bb_cap;
    List *unresoved_gotos;
    Dict *resolved_labels;
} CFGBuilder;

BasicBlock *bbNew(int type);
BasicBlock *bbAddNext(BasicBlock *cur, int type, BasicBlock *next);
char *bbTypeToString(int type);
char *bbFlagsToString(unsigned int flags);
char *bbPreviousBlockNumbersToString(BasicBlock *bb);
int bbPrevHas(BasicBlock *bb, int block_no);
BasicBlock *cfgGet(CFG *cfg, int block_no);
CFG *cfgConstruct(Cctrl *cc);
void bbPrint(BasicBlock *bb);

#endif
