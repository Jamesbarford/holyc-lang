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
    BasicBlock *head;
} CFG;

typedef struct CFGBuilder {
    int bb_count;
    Cctrl *cc;
    CFG *cfg;
    BasicBlock *bb;
} CFGBuilder;

BasicBlock *bbNew(int type);
BasicBlock *bbAddNext(BasicBlock *cur, int type, BasicBlock *next);

CFG *cfgConstruct(Cctrl *cc);

#endif