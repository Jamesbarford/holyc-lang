#ifndef CFG_H__
#define CFG_H__

#include "ast.h"
#include "ast.h"
#include "aostr.h"
#include "cctrl.h"

#define BB_GARBAGE        (-1)
#define BB_END_BLOCK      (0)
#define BB_HEAD_BLOCK     (1)
#define BB_CONTROL_BLOCK  (2)
#define BB_BRANCH_BLOCK   (3)
#define BB_LOOP_BLOCK     (4)
#define BB_RETURN_BLOCK   (5)
#define BB_BREAK_BLOCK    (6)
#define BB_DO_WHILE_COND  (7)
#define BB_GOTO           (8)

#define CFG_MAX_PREV    (32)

/* These are builder flags */
#define CFG_BUILDER_FLAG_IN_CONDITIONAL (0x1)
#define CFG_BUILDER_FLAG_IN_LOOP        (0x2)

#define BB_FLAG_LOOP_HEAD          (0x1)
#define BB_FLAG_LOOP_END           (0x2)
#define BB_FLAG_REDUNDANT_LOOP     (0x4)
#define BB_FLAG_UNCONDITIONAL_JUMP (0x8)
#define BB_FLAG_LABEL              (0x10)

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
    struct BasicBlock *next;
    struct BasicBlock *prev;
    struct BasicBlock *prev_blocks[32];
 //    IntSet *prev_block_ids;
    AstArray *ast_array;
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
    /* This a pointer to the memory pool which holds all of the basic blocks */
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
    List *ast_list;
    List *ast_iter;
    int bb_pos;
    int bb_cap;
    List *unresoved_gotos;
    Dict *resolved_labels;
} CFGBuilder;

BasicBlock *bbNew(int type);
BasicBlock *bbAddNext(BasicBlock *cur, int type, BasicBlock *next);
const char *bbTypeToString(BasicBlock *bb);
const int bbPrevHas(BasicBlock *bb, int block_no);
CFG *cfgConstruct(Cctrl *cc);

#endif
