#include "testhelper.HC"

/* Returning a struct by value, per the platform C ABI. <=16-byte aggregates
 * come back in registers (AArch64: HFA->v0.., INTEGER->x0,x1; SysV: per
 * eightbyte INTEGER->rax,rdx / SSE->xmm0,xmm1); >16-byte aggregates use a
 * hidden out-pointer. Previously hcc used the hidden out-pointer for ALL
 * struct returns, which mismatched the C ABI at every boundary (e.g. calling
 * an `extern "c"` function that returns a small struct returned garbage). */

class Vec2  { F32 x, y; };          /* 8B HFA / single SSE eightbyte */
class Pt    { I32 a, b, c, d; };    /* 16B -> 2 INTEGER eightbytes */
class Pair  { I64 a, b; };          /* 16B -> 2 INTEGER */
class Mix   { I64 n; F64 f; };      /* 16B -> INTEGER + SSE */
class Big   { I64 a, b, c; };       /* 24B -> indirect / MEMORY */

Vec2 MakeVec2(F32 x, F32 y) { Vec2 v; v.x = x; v.y = y; return v; }
Pt   MakePt(I32 n)          { Pt p; p.a=n; p.b=n+1; p.c=n+2; p.d=n+3; return p; }
Pair MakePair(I64 a, I64 b) { Pair p; p.a = a; p.b = b; return p; }
Mix  MakeMix(I64 n, F64 f)  { Mix m; m.n = n; m.f = f; return m; }
Big  MakeBig(I64 n)         { Big b; b.a=n; b.b=n*2; b.c=n*3; return b; }

I64 SumPt(Pt p) { return p.a + p.b + p.c + p.d; }

Bool HfaReturn()
{
  Vec2 v = MakeVec2(3.5, 7.5);
  return v.x == 3.5 && v.y == 7.5;
}

Bool IntegerReturn()
{
  Pt p = MakePt(10);
  Pair q = MakePair(100, 200);
  return p.a == 10 && p.b == 11 && p.c == 12 && p.d == 13 &&
         q.a == 100 && q.b == 200;
}

Bool MixedReturn()
{
  Mix m = MakeMix(42, 9.5);
  return m.n == 42 && m.f == 9.5;
}

Bool IndirectReturn()
{
  Big b = MakeBig(100);
  return b.a == 100 && b.b == 200 && b.c == 300;
}

Bool DirectFieldAccess()
{
  /* Field of a call result without binding it to a variable. */
  return MakeVec2(1.5, 2.5).y == 2.5 && MakePt(5).c == 7;
}

Bool ReturnPassedAsArg()
{
  /* A returned struct fed straight into another by-value parameter. */
  return SumPt(MakePt(20)) == 86;   /* 20+21+22+23 */
}

Bool ReturnIntoCopy()
{
  Pt p;
  p = MakePt(1);                    /* struct assignment from a call result */
  return p.a == 1 && p.d == 4;
}

I32 Main()
{
  "Test - returning a struct by value (C ABI):\n";
  I64 tests = 7, correct = 0;
  if (HfaReturn())          correct++;
  if (IntegerReturn())      correct++;
  if (MixedReturn())        correct++;
  if (IndirectReturn())     correct++;
  if (DirectFieldAccess())  correct++;
  if (ReturnPassedAsArg())  correct++;
  if (ReturnIntoCopy())     correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
