#include <limits.h>
#include <stdlib.h>

#include "containers.h"
#include "ir-fold.h"
#include "ir-types.h"
#include "list.h"
#include "util.h"

/* Provided by ir.c — used to allocate replacement constants in the IR arena. */
extern IrValue *irConstInt(IrValueType type, s64 i64);

/* `Map<u32 tmp_id -> IrValue *>` — known-constant tmps within the current
 * block. Reuses map_uint_to_uint_type because we only need an opaque pointer
 * value; map type's display formatting isn't exercised. */
static Map *foldCtxNewMap(void) {
    return mapNew(16, &map_uint_to_uint_type);
}

static IrValue *foldResolve(Map *m, IrValue *v) {
    if (!v) return NULL;
    if (v->kind != IR_VAL_TMP) return v;
    if (!mapHasInt(m, v->as.var.id)) return v;
    return (IrValue *)mapGetInt(m, v->as.var.id);
}

static int isConstInt(IrValue *v) {
    return v && v->kind == IR_VAL_CONST_INT;
}

/* Try to compute `a <op> b`. Returns 1 on success and writes result; 0 if
 * the op isn't safe to fold (div/0, shift out of range, INT64_MIN / -1, etc.). */
static int foldBinop(IrOp op, s64 a, s64 b, s64 *out) {
    switch (op) {
    case IR_IADD: *out = (s64)((u64)a + (u64)b); return 1;
    case IR_ISUB: *out = (s64)((u64)a - (u64)b); return 1;
    case IR_IMUL: *out = (s64)((u64)a * (u64)b); return 1;
    case IR_AND:  *out = a & b; return 1;
    case IR_OR:   *out = a | b; return 1;
    case IR_XOR:  *out = a ^ b; return 1;

    case IR_IDIV:
        if (b == 0) return 0;
        if (a == INT64_MIN && b == -1) return 0;
        *out = a / b; return 1;
    case IR_UDIV:
        if (b == 0) return 0;
        *out = (s64)((u64)a / (u64)b); return 1;
    case IR_IREM:
        if (b == 0) return 0;
        if (a == INT64_MIN && b == -1) return 0;
        *out = a % b; return 1;
    case IR_UREM:
        if (b == 0) return 0;
        *out = (s64)((u64)a % (u64)b); return 1;

    case IR_SHL:
        if (b < 0 || b >= 64) return 0;
        *out = (s64)((u64)a << b); return 1;
    case IR_SHR:
        if (b < 0 || b >= 64) return 0;
        *out = (s64)((u64)a >> b); return 1;
    case IR_SAR:
        if (b < 0 || b >= 64) return 0;
        *out = a >> b; return 1;

    default:
        return 0;
    }
}

static int foldCmp(IrCmpKind kind, s64 a, s64 b, s64 *out) {
    switch (kind) {
    case IR_CMP_EQ:  *out = (a == b); return 1;
    case IR_CMP_NE:  *out = (a != b); return 1;
    case IR_CMP_LT:  *out = (a < b); return 1;
    case IR_CMP_LE:  *out = (a <= b); return 1;
    case IR_CMP_GT:  *out = (a > b); return 1;
    case IR_CMP_GE:  *out = (a >= b); return 1;
    case IR_CMP_ULT: *out = ((u64)a <  (u64)b); return 1;
    case IR_CMP_ULE: *out = ((u64)a <= (u64)b); return 1;
    case IR_CMP_UGT: *out = ((u64)a >  (u64)b); return 1;
    case IR_CMP_UGE: *out = ((u64)a >= (u64)b); return 1;
    default:
        return 0;
    }
}

static void foldBlock(IrBlock *block, Map *known) {
    listForEach(block->instructions) {
        IrInstr *instr = (IrInstr *)it->value;

        /* Resolve operands through any known-constant tmps. dst is rewritten
         * for IR_BR/IR_RET because those use dst as a source operand; for
         * everything else dst is the SSA result and stays untouched. */
        instr->r1 = foldResolve(known, instr->r1);
        instr->r2 = foldResolve(known, instr->r2);
        if (instr->op == IR_BR || instr->op == IR_RET) {
            instr->dst = foldResolve(known, instr->dst);
        }
        if (instr->op == IR_PHI && instr->extra.phi_pairs) {
            for (u64 i = 0; i < instr->extra.phi_pairs->size; ++i) {
                IrPair *p = vecGet(IrPair *, instr->extra.phi_pairs, i);
                p->ir_value = foldResolve(known, p->ir_value);
            }
        }
        if (instr->op == IR_CALL && instr->r1 &&
            instr->r1->as.array.values) {
            Vec *args = instr->r1->as.array.values;
            for (u64 i = 0; i < args->size; ++i) {
                args->entries[i] = foldResolve(known,
                                               (IrValue *)args->entries[i]);
            }
        }

        s64 result;
        int folded = 0;
        if (instr->op == IR_ICMP && isConstInt(instr->r1) && isConstInt(instr->r2)) {
            folded = foldCmp(instr->extra.cmp_kind,
                             instr->r1->as._i64, instr->r2->as._i64, &result);
        } else if (isConstInt(instr->r1) && isConstInt(instr->r2)) {
            folded = foldBinop(instr->op,
                               instr->r1->as._i64, instr->r2->as._i64, &result);
        }

        if (folded && instr->dst && instr->dst->kind == IR_VAL_TMP) {
            IrValue *kv = irConstInt(instr->dst->type, result);
            mapAddIntOrErr(known, instr->dst->as.var.id, kv);
            /* The folded instruction is dropped from emission; codegen treats
             * IR_NOP as a no-op. */
            instr->op = IR_NOP;
            instr->r1 = NULL;
            instr->r2 = NULL;
        }
    }
}

void irFoldFunction(IrFunction *func) {
    if (!func) return;
    /* known is function-wide so use sites in successor blocks (phi pairs,
     * call args, etc.) can substitute folded constants for tmps defined
     * in earlier blocks. SSA's single-def-per-tmp guarantees this is
     * safe — the constant value is whatever the unique defining block
     * computed. */
    Map *known = foldCtxNewMap();
    listForEach(func->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        foldBlock(block, known);
    }
    mapRelease(known);
}
