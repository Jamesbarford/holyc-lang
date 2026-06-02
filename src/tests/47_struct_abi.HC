#include "testhelper.HC"

/* Structs passed and returned BY VALUE, following the platform C ABI.
 * Exercises both sides (HolyC callee + HolyC caller): small integer
 * aggregates in GP regs, HFAs (F32/F64) in FP regs, >16-byte aggregates
 * by reference, mixed scalar/struct args (register accounting), and
 * struct returns. */

class Color { U8 r; U8 g; U8 b; U8 a; };          /* 4B  INTEGER, 1 GP */
class I2    { I64 a; I64 b; };                     /* 16B INTEGER, 2 GP */
class Vec2  { F32 x; F32 y; };                     /* HFA, 2x s */
class Rect  { F32 x; F32 y; F32 w; F32 h; };       /* HFA, 4x s */
class V2d   { F64 a; F64 b; };                     /* HFA, 2x d */
class Big   { I64 a; I64 b; I64 c; };              /* 24B, by reference */

I64 ColorSum(Color c) { return c.r + c.g*10 + c.b*100 + c.a*1000; }
I64 I2Sum(I2 v)       { return v.a*10 + v.b; }
F64 RectSum(Rect r)   { return r.x + r.y + r.w + r.h; }
F64 Vec2Dot(Vec2 a, Vec2 b) { return a.x*b.x + a.y*b.y; }
F64 V2dSum(V2d v)     { return v.a*2 + v.b; }
I64 BigSum(Big b)     { return b.a + b.b*10 + b.c*100; }
F64 Mixed(I64 n, Color c, F64 f) { return n + c.r + c.g + f; }
I64 StructThenInt(Color c, I64 n) { return c.r + c.g + n; }

Color MakeColor(I64 base) {
  Color c; c.r = base; c.g = base+1; c.b = base+2; c.a = base+3; return c;
}
Vec2 MakeVec2(F64 x, F64 y) { Vec2 v; v.x = x; v.y = y; return v; }
I2   MakeI2(I64 a, I64 b)   { I2 v; v.a = a; v.b = b; return v; }

Bool IntegerArgs()
{
  I64 tests = 3, correct = 0;
  Color c; c.r=1; c.g=2; c.b=3; c.a=4;
  if (ColorSum(c) == 4321) correct++;
  I2 v; v.a=5; v.b=7;
  if (I2Sum(v) == 57) correct++;
  Big b; b.a=1; b.b=2; b.c=3;
  if (BigSum(b) == 321) correct++;
  return correct == tests;
}

Bool HfaArgs()
{
  I64 tests = 3, correct = 0;
  Rect r; r.x=1.0; r.y=2.0; r.w=3.0; r.h=4.0;
  if (RectSum(r) == 10.0) correct++;
  Vec2 a; a.x=2.0; a.y=3.0;
  Vec2 b; b.x=4.0; b.y=5.0;
  if (Vec2Dot(a, b) == 23.0) correct++;        /* 8 + 15 */
  V2d d; d.a=2.5; d.b=1.0;
  if (V2dSum(d) == 6.0) correct++;
  return correct == tests;
}

Bool MixedArgs()
{
  I64 tests = 2, correct = 0;
  Color c; c.r=1; c.g=2; c.b=3; c.a=4;
  if (Mixed(100, c, 5.0) == 108.0) correct++;
  if (StructThenInt(c, 100) == 103) correct++;
  return correct == tests;
}

Bool StructReturns()
{
  I64 tests = 3, correct = 0;
  Color c = MakeColor(10);
  if (c.r==10 && c.g==11 && c.b==12 && c.a==13) correct++;
  Vec2 v = MakeVec2(2.5, 7.5);
  if (v.x==2.5 && v.y==7.5) correct++;
  I2 i = MakeI2(100, 200);
  if (i.a==100 && i.b==200) correct++;
  return correct == tests;
}

I32 Main()
{
  "Test - struct by value (C ABI):\n";
  I64 tests = 4, correct = 0;
  if (IntegerArgs())   correct++;
  if (HfaArgs())       correct++;
  if (MixedArgs())     correct++;
  if (StructReturns()) correct++;
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
