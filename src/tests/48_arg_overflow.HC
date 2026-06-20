#include "testhelper.HC"

/* Argument register overflow: when a call has more arguments than there
 * are parameter registers, the excess spills onto the stack (caller) and
 * is read back from the incoming argument area (callee). Covers overflow
 * of plain scalars (int + float) and of by-value structs (INTEGER, HFA,
 * INDIRECT), including unused overflow params. */

class Color { U8 r; U8 g; U8 b; U8 a; };          /* INTEGER agg */
class Vec2  { F32 x; F32 y; };                     /* HFA agg     */
class Big   { I64 a; I64 b; I64 c; };              /* INDIRECT    */

I64 Sum9(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 h,I64 i) {
  return a + b + c + d + e + f + g + h + i;
}
F64 SumF10(F64 a,F64 b,F64 c,F64 d,F64 e,F64 f,F64 g,F64 h,F64 i,F64 j) {
  return a + b + c + d + e + f + g + h + i + j;
}
/* g and h overflow on x86 (6 GP regs) and are UNUSED - exercises the
 * unreferenced-overflow-param path. */
I64 Unused(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 h,I64 i,I64 j) {
  return i * 10 + j;
}
/* Struct after 8 integer args -> stack on aarch64; on x86 the trailing
 * scalars and the struct both spill. */
I64 IntStructOv(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 h, Color c2) {
  return h * 1000 + c2.r + c2.g * 10;
}
F64 HfaOv(F64 a,F64 b,F64 c,F64 d,F64 e,F64 f,F64 g,F64 h, Vec2 v) {
  return h + v.x + v.y;
}
I64 BigOv(I64 a,I64 b,I64 c,I64 d,I64 e,I64 f,I64 g,I64 h, Big bg) {
  return h * 100 + bg.a + bg.b * 10 + bg.c;
}

I32 Main()
{
  I64 tests = 6, correct = 0;

  if (Sum9(1,2,3,4,5,6,7,8,9) == 45) correct++;
  if (SumF10(1.0,2,3,4,5,6,7,8,9,10) == 55.0) correct++;
  if (Unused(1,2,3,4,5,6,7,8,9,10) == 100) correct++;

  Color c; c.r=9; c.g=1; c.b=0; c.a=0;
  if (IntStructOv(1,2,3,4,5,6,7,8,c) == 8019) correct++;

  Vec2 v; v.x=2.0; v.y=3.0;
  if (HfaOv(1,2,3,4,5,6,7,8,v) == 13.0) correct++;

  Big bg; bg.a=1; bg.b=2; bg.c=3;
  if (BigOv(1,2,3,4,5,6,7,8,bg) == 824) correct++;

  "Test - argument register overflow (stack spill):\n";
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
