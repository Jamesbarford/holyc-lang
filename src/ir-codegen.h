#ifndef IR_CODEGEN_H__
#define IR_CODEGEN_H__

#include "cctrl.h"
#include "aostr.h"
#include "ir-types.h"
#include  "ir-regalloc.h"
#include "containers.h"

typedef struct IrCgCtx {
    Cctrl *cc;
    AoStr *buf;
    /* Slot offset map + extra-stack counter + IR function. The
     * regalloc helpers in ir-regalloc.c take an IrRaCtx*; the codegen
     * passes `&ctx->ra` when calling into them. */
    IrRaCtx ra;
    /* Unique block-label prefix per function so multiple slice
     * functions compiled in the same translation unit don't collide. */
    int func_uid;
    /* Set during the per-block emission loop so terminator codegen
     * knows who's emitting and which block (if any) follows in layout
     * order. */
    IrBlock *cur_block;
    IrBlock *next_block;
    /* Map<u32 IrValue.var.id -> IrInstr* (IR_LEA)>. Populated once per
     * function from peephole-marked LEAs. The IR_CALL emit consults
     * this to re-create `leaq <source>, <target_reg>` directly at the
     * call site instead of going through the slot. NULL when no
     * LEA-into-call fusion fired. */
    Map *lea_inline_map;
} IrCgCtx;

typedef void ir_cg_emit_one_phi(IrCgCtx *ctx, IrInstr *phi, IrPair *match);

uint64_t ieee754(double _f64);
void irCgEmitPhiMaterialize(IrCgCtx *ctx, IrBlock *from, IrBlock *to,
                            ir_cg_emit_one_phi *emit_one_phi);
int phiPairValueLiveInRax(IrBlock *from, IrValue *v);
int irCgIsImm32(IrValue *val, s64 *out);

#endif
