#ifndef CFG_H__
#define CFG_H__

#include "ast.h"
#include "ast.h"
#include "aostr.h"
#include "cctrl.h"

#define CFG_HEAD_BLOCK    (1<<0)
#define CFG_CONTROL_BLOCK (1<<2)
#define CFG_LOOP_BLOCK    (1<<3)

#define CFG_MAX_PREV    (32)

#define CFG_FLAG_IN_CONDITIONAL (0x1)
#define CFG_FLAG_IN_LOOP        (0x2)

typedef struct BasicBlock {
    /* @Confirm:
     * Should we stick ssa on this struct? */
    int type;
    int block_no;
    int prev_cnt;
    struct BasicBlock *_if;
    struct BasicBlock *_else;
    struct BasicBlock *next;
    struct BasicBlock *prev[CFG_MAX_PREV];
    AstArray *ast_array;
} BasicBlock;

/* Head of a CFG is a function */
typedef struct CFG {
    /* CFG Does not own the function name, the AST does */
    aoStr *ref_fname;
    /* This head block is also a pointer to the begining of `bb_pool` in 
     * the `CFGBuilder` so freeing this frees every `BasicBlock` */
    BasicBlock *head;
} CFG;

typedef struct CFGBuilder {
    int bb_count;
    unsigned long flags;
    Cctrl *cc;
    CFG *cfg;
    BasicBlock *bb;
    BasicBlock *bb_pool;
    List *ast_list;
    List *ast_iter;
    int needs_convergence;
    int bb_pos;
    int bb_cap;
} CFGBuilder;

BasicBlock *bbNew(int type);
BasicBlock *bbAddNext(BasicBlock *cur, int type, BasicBlock *next);

CFG *cfgConstruct(Cctrl *cc);

#endif
