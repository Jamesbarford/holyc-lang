/* Unit tests for ir-fold.c. Builds IR by hand (bypassing the AST lowerer),
 * runs irFoldFunction, and asserts the resulting instruction list.
 *
 * Run as part of `make ir-fold-test`. */

#include <inttypes.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ir-fold.h"
#include "ir-types.h"
#include "list.h"
#include "memory.h"
#include "util.h"

/* `is_terminal` is defined in main.c which we exclude from the test build,
 * but ir-debug.c and others reference it. Provide a definition here. */
int is_terminal = 0;

/* ir.c internals we re-use to build IR by hand. */
extern void irMemoryInit(void);
extern IrFunction *irFunctionNew(AoStr *fname);
extern void irFnAddBlock(IrFunction *fn, IrBlock *block);
extern IrBlock *irBlockNew(void);
extern void irResetBlockId(void);
extern void irTmpVariableCountReset(void);
extern IrInstr *irInstrNew(IrOp op, IrValue *dst, IrValue *r1, IrValue *r2);
extern IrValue *irConstInt(IrValueType type, s64 i64);
extern IrValue *irTmp(IrValueType type, u16 size);

/* ---- mini assertion harness ---- */

static int tests_run = 0;
static int tests_failed = 0;

#define CHECK(cond, ...)                                                       \
    do {                                                                       \
        tests_run++;                                                           \
        if (!(cond)) {                                                         \
            tests_failed++;                                                    \
            fprintf(stderr, "FAIL %s:%d: ", __FILE__, __LINE__);               \
            fprintf(stderr, __VA_ARGS__);                                      \
            fprintf(stderr, "\n");                                             \
        }                                                                      \
    } while (0)

/* ---- helpers for building a single-block function ---- */

static IrFunction *new_test_function(IrBlock **block_out) {
    AoStr *name = aoStrNew();
    aoStrCatLen(name, "test", 4);
    IrFunction *func = irFunctionNew(name);
    IrBlock *block = irBlockNew();
    func->entry_block = block;
    irFnAddBlock(func, block);
    *block_out = block;
    return func;
}

static void append(IrBlock *block, IrInstr *instr) {
    listAppend(block->instructions, instr);
}

static IrInstr *binop(IrBlock *block, IrOp op, IrValue *r1, IrValue *r2) {
    IrValue *dst = irTmp(IR_TYPE_I64, 8);
    IrInstr *instr = irInstrNew(op, dst, r1, r2);
    append(block, instr);
    return instr;
}

static IrInstr *cmp(IrBlock *block, IrCmpKind kind, IrValue *r1, IrValue *r2) {
    IrValue *dst = irTmp(IR_TYPE_I64, 8);
    IrInstr *instr = irInstrNew(IR_ICMP, dst, r1, r2);
    instr->extra.cmp_kind = kind;
    append(block, instr);
    return instr;
}

static IrInstr *nth(IrBlock *block, int n) {
    int i = 0;
    listForEach(block->instructions) {
        if (i++ == n) return (IrInstr *)it->value;
    }
    return NULL;
}

static int is_const(IrValue *v, s64 expected) {
    return v && v->kind == IR_VAL_CONST_INT && v->as._i64 == expected;
}

/* ---- tests ---- */

static void test_pure_const_add(void) {
    IrBlock *bb;
    IrFunction *f = new_test_function(&bb);
    binop(bb, IR_IADD, irConstInt(IR_TYPE_I64, 2), irConstInt(IR_TYPE_I64, 3));

    irFoldFunction(f);

    IrInstr *a = nth(bb, 0);
    CHECK(a->op == IR_NOP, "iadd 2,3 should fold to NOP, got op=%d", a->op);
}

static void test_pure_const_chain_propagates(void) {
    /* %t1 = iadd 2, 3   ; folds to 5
     * %t2 = imul %t1, 4 ; should fold to 20 because t1 is known-const
     */
    IrBlock *bb;
    IrFunction *f = new_test_function(&bb);
    IrInstr *i1 = binop(bb, IR_IADD,
                        irConstInt(IR_TYPE_I64, 2),
                        irConstInt(IR_TYPE_I64, 3));
    binop(bb, IR_IMUL, i1->dst, irConstInt(IR_TYPE_I64, 4));

    irFoldFunction(f);

    IrInstr *a = nth(bb, 0);
    IrInstr *b = nth(bb, 1);
    CHECK(a->op == IR_NOP, "first iadd should be NOP");
    CHECK(b->op == IR_NOP, "imul should propagate from t1 and fold");
}

static void test_mixed_const_var_kept(void) {
    /* `param + 5` cannot be folded — the param value is unknown at compile
     * time. The instruction must remain as-is. */
    IrBlock *bb;
    IrFunction *f = new_test_function(&bb);
    IrValue *param = irTmp(IR_TYPE_I64, 8);
    param->kind = IR_VAL_PARAM;
    binop(bb, IR_IADD, param, irConstInt(IR_TYPE_I64, 5));

    irFoldFunction(f);

    IrInstr *a = nth(bb, 0);
    CHECK(a->op == IR_IADD, "mixed const/param iadd must not be folded");
    CHECK(a->r1 == param, "param operand should be unchanged");
    CHECK(is_const(a->r2, 5), "const operand should be unchanged");
}

static void test_noop_passthrough(void) {
    /* IR_RET / IR_NOP should not be folded as if they were binops. */
    IrBlock *bb;
    IrFunction *f = new_test_function(&bb);
    IrInstr *nop_in = irInstrNew(IR_NOP, NULL, NULL, NULL);
    append(bb, nop_in);
    IrInstr *ret = irInstrNew(IR_RET, irConstInt(IR_TYPE_I64, 42), NULL, NULL);
    append(bb, ret);

    irFoldFunction(f);

    IrInstr *a = nth(bb, 0);
    IrInstr *b = nth(bb, 1);
    CHECK(a->op == IR_NOP, "NOP stays NOP");
    CHECK(b->op == IR_RET, "RET stays RET");
    CHECK(is_const(b->dst, 42), "RET operand intact");
}

static void test_div_by_zero_not_folded(void) {
    IrBlock *bb;
    IrFunction *f = new_test_function(&bb);
    binop(bb, IR_IDIV, irConstInt(IR_TYPE_I64, 10), irConstInt(IR_TYPE_I64, 0));
    binop(bb, IR_UREM, irConstInt(IR_TYPE_I64, 10), irConstInt(IR_TYPE_I64, 0));

    irFoldFunction(f);

    CHECK(nth(bb, 0)->op == IR_IDIV, "x/0 must not fold");
    CHECK(nth(bb, 1)->op == IR_UREM, "x%%0 must not fold");
}

static void test_intmin_div_neg1_not_folded(void) {
    /* INT64_MIN / -1 traps on x86; must be left for runtime. */
    IrBlock *bb;
    IrFunction *f = new_test_function(&bb);
    binop(bb, IR_IDIV, irConstInt(IR_TYPE_I64, INT64_MIN),
                       irConstInt(IR_TYPE_I64, -1));
    binop(bb, IR_IREM, irConstInt(IR_TYPE_I64, INT64_MIN),
                       irConstInt(IR_TYPE_I64, -1));

    irFoldFunction(f);

    CHECK(nth(bb, 0)->op == IR_IDIV, "INT64_MIN/-1 must not fold");
    CHECK(nth(bb, 1)->op == IR_IREM, "INT64_MIN%%-1 must not fold");
}

static void test_shift_out_of_range_not_folded(void) {
    IrBlock *bb;
    IrFunction *f = new_test_function(&bb);
    binop(bb, IR_SHL, irConstInt(IR_TYPE_I64, 1), irConstInt(IR_TYPE_I64, 64));
    binop(bb, IR_SHR, irConstInt(IR_TYPE_I64, 1), irConstInt(IR_TYPE_I64, -1));
    binop(bb, IR_SAR, irConstInt(IR_TYPE_I64, 1), irConstInt(IR_TYPE_I64, 200));
    /* In-range shifts should still fold. */
    binop(bb, IR_SHL, irConstInt(IR_TYPE_I64, 1), irConstInt(IR_TYPE_I64, 3));

    irFoldFunction(f);

    CHECK(nth(bb, 0)->op == IR_SHL, "shift by 64 must not fold");
    CHECK(nth(bb, 1)->op == IR_SHR, "shift by -1 must not fold");
    CHECK(nth(bb, 2)->op == IR_SAR, "shift by 200 must not fold");
    CHECK(nth(bb, 3)->op == IR_NOP, "shift by 3 should fold");
}

static void test_signed_overflow_wraps(void) {
    /* (INT64_MAX) + 1 wraps to INT64_MIN under two's-complement; we use
     * unsigned arithmetic in the folder so this is defined and folds. */
    IrBlock *bb;
    IrFunction *f = new_test_function(&bb);
    IrInstr *add = binop(bb, IR_IADD,
                         irConstInt(IR_TYPE_I64, INT64_MAX),
                         irConstInt(IR_TYPE_I64, 1));
    /* Chain a multiply that consumes the wrapped value to verify propagation. */
    binop(bb, IR_IMUL, add->dst, irConstInt(IR_TYPE_I64, 1));

    irFoldFunction(f);

    CHECK(nth(bb, 0)->op == IR_NOP, "INT64_MAX+1 should fold (wrap)");
    CHECK(nth(bb, 1)->op == IR_NOP, "imul of folded * 1 should also fold");
}

static void test_cmp_folds(void) {
    IrBlock *bb;
    IrFunction *f = new_test_function(&bb);
    cmp(bb, IR_CMP_LT, irConstInt(IR_TYPE_I64, 3), irConstInt(IR_TYPE_I64, 5));
    cmp(bb, IR_CMP_GE, irConstInt(IR_TYPE_I64, 5), irConstInt(IR_TYPE_I64, 5));
    cmp(bb, IR_CMP_NE, irConstInt(IR_TYPE_I64, 7), irConstInt(IR_TYPE_I64, 7));

    irFoldFunction(f);

    CHECK(nth(bb, 0)->op == IR_NOP, "cmp_lt(3,5) should fold");
    CHECK(nth(bb, 1)->op == IR_NOP, "cmp_ge(5,5) should fold");
    CHECK(nth(bb, 2)->op == IR_NOP, "cmp_ne(7,7) should fold");
}

static void test_propagate_into_store(void) {
    /* %t1 = iadd 2, 3   ; folds to 5
     * store slot, %t1   ; r1 should be rewritten to const 5
     */
    IrBlock *bb;
    IrFunction *f = new_test_function(&bb);
    IrInstr *add = binop(bb, IR_IADD,
                         irConstInt(IR_TYPE_I64, 2),
                         irConstInt(IR_TYPE_I64, 3));
    IrValue *slot = irTmp(IR_TYPE_I64, 8);
    IrInstr *st = irInstrNew(IR_STORE, slot, add->dst, NULL);
    append(bb, st);

    irFoldFunction(f);

    IrInstr *st2 = nth(bb, 1);
    CHECK(st2->op == IR_STORE, "store opcode preserved");
    CHECK(is_const(st2->r1, 5), "store source rewritten to const 5");
}

/* ---- runner ---- */

int main(void) {
    /* Skip the rest of memoryInit (ast/lexeme) — none of the compiler
     * front-end is exercised by these tests. We just need the IR arena, plus
     * globalArenaInit because some IR helpers (e.g. aoStrNew) reach for the
     * global arena. globalArenaInit is declared in memory.h. */
    globalArenaInit(4096);
    irMemoryInit();
    irResetBlockId();
    irTmpVariableCountReset();

    test_pure_const_add();
    test_pure_const_chain_propagates();
    test_mixed_const_var_kept();
    test_noop_passthrough();
    test_div_by_zero_not_folded();
    test_intmin_div_neg1_not_folded();
    test_shift_out_of_range_not_folded();
    test_signed_overflow_wraps();
    test_cmp_folds();
    test_propagate_into_store();

    printf("ir-fold tests: %d/%d passed\n", tests_run - tests_failed, tests_run);
    return tests_failed == 0 ? 0 : 1;
}
