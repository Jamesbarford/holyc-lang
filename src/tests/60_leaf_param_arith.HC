#include "testhelper.HC"

/* Leaf functions mixing constants with register-homed parameters.
 *
 * Regression for a store-forwarding bug: a leaf function's param arrives
 * register-homed (x0/x1 on AArch64), and `irForwardStoreToReads` rewrote a
 * read of its spill slot into the arrive value even in the r2 operand
 * position - but codegen materialises r1 into scratch x0 FIRST, so
 * `I64 D(I64 x){ return 10/x; }` emitted `mov x0,#10` over the param and
 * computed 10/10 == 1 for every input (same for +,-,*,%). The fix refuses
 * forwarding a scratch-homed source into positions the consumer's own
 * operand loading clobbers (r2, STORE_DEREF/RMW_DEREF value, phi pairs).
 * Wrong in BOTH AOT and JIT; x86_64 was immune (arg regs don't overlap
 * scratch). */

I64 DivConstLeft(I64 x)  { return 10 / x; }
I64 AddConstLeft(I64 x)  { return 10 + x; }
I64 SubConstLeft(I64 x)  { return 10 - x; }
I64 MulConstLeft(I64 x)  { return 10 * x; }
I64 RemConstLeft(I64 x)  { return 17 % x; }
I64 DivConstRight(I64 x) { return x / 3; }
I64 SelfDiv(I64 x)       { return x / x; }
I64 SelfAdd(I64 x)       { return x + x; }

I64 Sub2(I64 a, I64 b)    { return b - a; }
I64 Sub2Rev(I64 a, I64 b) { return a - b; }
I64 Div2(I64 a, I64 b)    { return b / a; }
I64 Mix3(I64 a, I64 b, I64 c) { return a + 10 * b - c; }

F64 FDivConstLeft(F64 x) { return 10.0 / x; }
F64 FSub2(F64 a, F64 b)  { return b - a; }

U0 StoreThrough(I64 a, I64 x, I64 *p) { *p = x; }

Bool ConstLeftOps()
{
  return DivConstLeft(2) == 5  && AddConstLeft(7) == 17 &&
         SubConstLeft(4) == 6  && MulConstLeft(4) == 40 &&
         RemConstLeft(5) == 2;
}

Bool ParamOnBothSides()
{
  return DivConstRight(21) == 7 && SelfDiv(9) == 1 && SelfAdd(21) == 42;
}

Bool TwoAndThreeParams()
{
  return Sub2(3, 10) == 7 && Sub2Rev(10, 3) == 7 && Div2(5, 30) == 6 &&
         Mix3(1, 2, 3) == 18;
}

Bool FloatLeafParams()
{
  return FDivConstLeft(4.0) == 2.5 && FSub2(1.5, 4.0) == 2.5;
}

Bool PointerStoreParam()
{
  I64 v = 0;
  StoreThrough(1, 42, &v);
  return v == 42;
}

I32 Main()
{
  "Test - leaf fns mixing constants with register-homed params:\n";
  I64 tests = 5, correct = 0;
  if (ConstLeftOps())       correct++;
  if (ParamOnBothSides())   correct++;
  if (TwoAndThreeParams())  correct++;
  if (FloatLeafParams())    correct++;
  if (PointerStoreParam())  correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
