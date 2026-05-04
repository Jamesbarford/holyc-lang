#include <stdlib.h>

#include "ir-types.h"
#include "ir-eval.h"
#include "containers.h"

static IrValue *irResolveTmp(Map *resolver, IrValue *v) {
    if (!v) return NULL;
    if (v->kind != IR_VAL_TMP) return v;
    IrValue *resolved = (IrValue *)mapGetInt(resolver, v->as.var.id);
    if (!resolved) return v;
    return resolved;
}

static int irEvalBinop(IrOp op, s64 a, s64 b, s64 *out) {
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

static int irEvalCmp(IrCmpKind kind, s64 a, s64 b, s64 *out) {
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

void irEvalBlock(IrBlock *bb, Map *resolver) {
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;

        I->r1 = irResolveTmp(resolver, I->r1);
        I->r2 = irResolveTmp(resolver, I->r2);

        if (I->op == IR_BR || I->op == IR_RET) {
            I->dst = irResolveTmp(resolver, I->dst);
        }

        if (I->op == IR_PHI && I->extra.phi_pairs) {
            for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                p->ir_value = irResolveTmp(resolver, p->ir_value);
            }
        }

        if (I->op == IR_CALL && I->r1 && I->r1->as.array.values) {
            Vec *args = I->r1->as.array.values;
            for (u64 i = 0; i < args->size; ++i) {
                args->entries[i] = irResolveTmp(resolver,
                                    (IrValue *)args->entries[i]);
            }
        }

        s64 result;
        u8 was_eval = 0;

        if (I->op == IR_ICMP && irIsConstInt(I->r1) && irIsConstInt(I->r2)) {
            was_eval = irEvalCmp(I->extra.cmp_kind,
                                 I->r1->as._i64,
                                 I->r2->as._i64,
                                 &result);
        } else if (irIsConstInt(I->r1) && irIsConstInt(I->r2)) {
            was_eval = irEvalBinop(I->op,
                                   I->r1->as._i64,
                                   I->r2->as._i64,
                                   &result);
        }

        if (was_eval && irIsTmp(I->dst)) {
            IrValue *evaled = irConstInt(I->dst->type, result);
            mapAddIntOrErr(resolver, I->dst->as.var.id, evaled);
            I->op = IR_NOP;
            I->r1 = I->r2 = NULL;
        }
    }
}

void irEvalConstantExpressions(IrFunction *fn) {
    Map *resolver = irVarValueMapNew();
    listForEach(fn->blocks) {
        IrBlock *bb = (IrBlock *)it->value;
        irEvalBlock(bb,resolver);
    }
    mapRelease(resolver);
}
