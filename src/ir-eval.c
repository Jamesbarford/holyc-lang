#include <stdlib.h>

#include "ir-types.h"
#include "ir-eval.h"
#include "containers.h"

static IrValue *irResolveTmp(Map *resolver, IrValue *v) {
    if (!v) return NULL;
    if (!irIsTmp(v)) return v;
    IrValue *resolved = (IrValue *)mapGetInt(resolver, irVarId(v));
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

/* True if `I` is `x <op> k` (or commuted) where the algebraic
 * identity collapses to `x`. `c` is the constant operand (whichever
 * side of `I`) and `k` is its value. */
static int irEvalCanFold(IrInstr *I, IrValue *c, s64 k) {
    /* x * 1, x / 1, x /u 1 - div only when divisor is r2. */
    if ((I->op == IR_IMUL || I->op == IR_IDIV || I->op == IR_UDIV) &&
        k == 1 && c == I->r2) return 1;
    /* x + 0, x - 0, x | 0, x ^ 0, x << 0, x >> 0, x >>s 0
     * non-commutative ops only when 0 is r2. */
    if ((I->op == IR_IADD || I->op == IR_ISUB || I->op == IR_OR ||
         I->op == IR_XOR  || I->op == IR_SHL  || I->op == IR_SHR ||
         I->op == IR_SAR) && k == 0 && c == I->r2) return 1;
    /* 0 + x, 0 | x, 0 ^ x - commutative zero on r1. */
    if ((I->op == IR_IADD || I->op == IR_OR || I->op == IR_XOR) &&
        k == 0 && c == I->r1) return 1;
    /* 1 * x - commutative. */
    if (I->op == IR_IMUL && k == 1 && c == I->r1) return 1;
    /* x & -1 = x (either side, all bits set). */
    if (I->op == IR_AND && k == -1) return 1;
    return 0;
}

void irEvalBlock(IrBlock *bb, Map *resolver) {
    listForEach(bb->instructions) {
        IrInstr *I = (IrInstr *)it->value;

        I->r1 = irResolveTmp(resolver, I->r1);
        I->r2 = irResolveTmp(resolver, I->r2);

        /* IR_BR/IR_RET pass a value via dst; IR_STORE_DEREF's dst is
         * the pointer being written through (an input, not a write
         * target). Resolve those so algebraic-identity folds that
         * forward dst -> r1 chain through subsequent users. Other
         * ops leave dst as the freshly-defined value. */
        if (I->op == IR_BR || I->op == IR_RET ||
            I->op == IR_STORE_DEREF)
        {
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
            mapAddIntOrErr(resolver, irDstVarId(I), evaled);
            I->op = IR_NOP;
            I->r1 = I->r2 = NULL;
            continue;
        }

        /* Algebraic identities (one const, one variable). The
         * 64-bit ops only, sub-word arithmetic is rare here and
         * the codegen handles the widening. */
        if (irIsTmp(I->dst)) {
            IrValue *c = NULL, *x = NULL;
            if (irIsConstInt(I->r2)) { c = I->r2; x = I->r1; }
            else if (irIsConstInt(I->r1)) { c = I->r1; x = I->r2; }
            if (c && x) {
                s64 k = c->as._i64;
                int folded = 0;
                /* x * 1 = x; x + 0 = x; x | 0 = x; x ^ 0 = x; x & -1 = x */
                if (irEvalCanFold(I, c, k)) {
                    mapAddIntOrErr(resolver, irDstVarId(I), x);
                    I->op = IR_NOP;
                    I->r1 = I->r2 = NULL;
                    folded = 1;
                }
                /* x * 0 = 0; x & 0 = 0 */
                else if ((I->op == IR_IMUL || I->op == IR_AND) && k == 0) {
                    IrValue *zero = irConstInt(I->dst->type, 0);
                    mapAddIntOrErr(resolver, irDstVarId(I), zero);
                    I->op = IR_NOP;
                    I->r1 = I->r2 = NULL;
                    folded = 1;
                }
                /* x * 2^n = x << n. Only when const is r2 (mul is
                 * commutative; canonicalising here lets codegen
                 * emit `shlq $n, %rax`). Restrict to positive
                 * pow2 to avoid surprises with the sign bit. */
                else if (I->op == IR_IMUL && c == I->r2 &&
                         k > 1 && (k & (k - 1)) == 0)
                {
                    int n = __builtin_ctzll((u64)k);
                    I->op = IR_SHL;
                    I->r2 = irConstInt(c->type, n);
                    folded = 1;
                }
                if (folded) continue;
            }
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
