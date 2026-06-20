#include "testhelper.HC"

/* Float -> integer conversions across every integer width and both
 * signednesses, for F32 and F64. Truncation is toward zero (C semantics):
 * -100.9 -> -100, not -101. F32 values are chosen to be exactly
 * representable in single precision so the truncated result is exact. */

Bool F32ToInt()
{
  I64 tests = 8, correct = 0;
  F32 f;

  f = -100.5;     I8  a = f;  if (a == -100)     correct++;
  f =  200.5;     U8  b = f;  if (b ==  200)     correct++;
  f = -5000.5;    I16 c = f;  if (c == -5000)    correct++;
  f =  50000.5;   U16 d = f;  if (d ==  50000)   correct++;
  f = -1000000.25; I32 e = f; if (e == -1000000) correct++;
  f =  1000000.75; U32 g = f; if (g ==  1000000) correct++;
  f = -2000000.5; I64 h = f;  if (h == -2000000) correct++;
  f =  2000000.5; U64 i = f;  if (i ==  2000000) correct++;

  return correct == tests;
}

Bool F64ToInt()
{
  I64 tests = 8, correct = 0;
  F64 f;

  f = -100.9;            I8  a = f; if (a == -100)            correct++;
  f =  250.9;            U8  b = f; if (b ==  250)            correct++;
  f = -30000.9;          I16 c = f; if (c == -30000)          correct++;
  f =  65000.9;          U16 d = f; if (d ==  65000)          correct++;
  f = -2000000000.9;     I32 e = f; if (e == -2000000000)     correct++;
  /* 4e9 is > I32_MAX: exercises fptoui, not fptosi. */
  f =  4000000000.9;     U32 g = f; if (g ==  4000000000)     correct++;
  f = -9000000000000.9;  I64 h = f; if (h == -9000000000000)  correct++;
  f =  9000000000000.9;  U64 i = f; if (i ==  9000000000000)  correct++;

  return correct == tests;
}

/* The reverse direction: integer -> float, signed and unsigned. */
Bool IntToFloat()
{
  I64 tests = 4, correct = 0;

  I32 si = -2000000000;  F64 a = si; if (a == -2000000000.0)  correct++;
  U32 ui =  4000000000;  F64 b = ui; if (b ==  4000000000.0)  correct++;
  I64 sl = -9;           F32 c = sl; if (c == -9.0)           correct++;
  U8  ub =  200;         F32 d = ub; if (d ==  200.0)         correct++;

  return correct == tests;
}

I64 WideRet() { return 0x1234567890ABCDEF; }

/* Identity passthroughs: the coercion happens at the call site (the
 * argument is converted to the parameter's type) and again at the return. */
U8  TakeU8(U8 p)   { return p; }
I64 TakeI64(I64 p) { return p; }
F64 TakeF64(F64 p) { return p; }
F32 TakeF32(F32 p) { return p; }

/* Arguments are coerced to the parameter's type at the call site: a wider
 * int truncates to a narrow param, a narrow signed int promotes (sign
 * extends) to a wide one, an int promotes to a float param and a float
 * truncates (toward zero) to an int param, and F32<->F64 convert. */
Bool FnCallConversions()
{
  I64 tests = 9, correct = 0;

  if (TakeU8(300)        == 44)  correct++;   /* wide int -> U8 param    */
  if (TakeU8(0x1FF)      == 255) correct++;   /* 511 & 0xFF              */
  I8 n = -7;
  if (TakeI64(n)         == -7)  correct++;   /* I8 -> I64 (sign extend) */
  if (TakeF64(5)         == 5.0) correct++;   /* int -> F64 param        */
  if (TakeI64(7.9)       == 7)   correct++;   /* F64 -> I64 param (trunc)*/
  if (TakeI64(-7.9)      == -7)  correct++;   /* trunc toward zero       */
  F32 g = 2.5;
  if (TakeF64(g)         == 2.5) correct++;   /* F32 arg -> F64 param    */
  if (TakeF32(3.5)       == 3.5) correct++;   /* F64 arg -> F32 param    */
  if (TakeU8(WideRet())  == 239) correct++;   /* truncate a call result  */

  return correct == tests;
}

/* Assigning a wider integer to a narrower type truncates to the
 * destination width; the surviving low bits are then interpreted per the
 * destination's signedness (so a high bit becomes a sign bit for a signed
 * target). */
Bool IntTruncation()
{
  I64 tests = 10, correct = 0;
  I64 big = 0x1234567890ABCDEF;

  U8  a = big;  if (a == 239)         correct++;   /* 0xEF             */
  U16 b = big;  if (b == 52719)       correct++;   /* 0xCDEF           */
  U32 c = big;  if (c == 2427178479)  correct++;   /* 0x90ABCDEF       */
  I8  d = big;  if (d == -17)         correct++;   /* 0xEF  as signed  */
  I16 e = big;  if (e == -12817)      correct++;   /* 0xCDEF as signed */
  I32 f = big;  if (f == -1867788817) correct++;   /* 0x90ABCDEF signed*/

  U8  g = 300;  if (g == 44)          correct++;   /* 300 - 256        */
  I8  h = 200;  if (h == -56)         correct++;   /* 200 wraps signed */

  /* truncation of a (mismatched) wider value through a function return  */
  U8  i = WideRet(); if (i == 239)    correct++;
  /* explicit narrowing cast in an expression (HolyC postfix cast)       */
  if (big(U8) == 239)                 correct++;

  return correct == tests;
}

/* Same coercions, but the argument is a *variable* (not a literal) and the
 * parameter is mutated in the body then returned - the case that has been
 * a source of bugs (the value must be truncated/promoted into the param's
 * type at the call site, and the in-body modification must survive). */
U64 AddU16(U16 arg)  { arg += 3;   return arg; }
I64 DecI8(I8 arg)    { arg -= 1;   return arg; }
F64 BumpF32(F32 arg) { arg += 0.5; return arg; }
I64 BumpI64(I64 arg) { arg += 1;   return arg; }

Bool VarArgCoercion()
{
  I64 tests = 6, correct = 0;

  I64 x = 23;          if (AddU16(x)   == 26)   correct++; /* I64 var -> U16 */
  I64 big = 0x12345;   if (AddU16(big) == 9032) correct++; /* 0x2345 + 3     */
  I64 neg = -7;        if (DecI8(neg)  == -8)   correct++; /* signed narrow  */
  F64 d = 2.0;         if (BumpF32(d)  == 2.5)  correct++; /* F64 var -> F32  */
  I64 i = 10;          if (BumpF32(i)  == 10.5) correct++; /* int var -> F32  */
  F64 f = 7.9;         if (BumpI64(f)  == 8)    correct++; /* F64 var -> I64  */

  return correct == tests;
}

I32 Main()
{
  I64 tests = 6, correct = 0;
  if (F32ToInt())         correct++;
  if (F64ToInt())         correct++;
  if (IntToFloat())       correct++;
  if (IntTruncation())    correct++;
  if (FnCallConversions()) correct++;
  if (VarArgCoercion())   correct++;
  "Test - float<->int conversions (all widths, signed + unsigned):\n";
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
