#include "testhelper.HC"

/* F32 arithmetic is only exact for values representable in single
 * precision (sums of powers of two within the 24-bit mantissa), so
 * unlike 26_f64.HC these comparisons stick to halves/quarters/eighths
 * where the result is exact in both F32 and F64. An F32 compared
 * against an F64 literal is widened first, so the two sides match
 * exactly when the value is representable. */

Bool F32Addition()
{
  I64 tests = 6, correct = 0;
  F32 f1 = 0.5 + 0.25;
  if (f1 == 0.75) correct++;

  F32 f2 = 1.5 + 2.5;
  if (f2 == 4.0) correct++;

  F32 f3 = 100.0 + 0.5;
  if (f3 == 100.5) correct++;

  F32 f4 = -3.25 + 1.25;
  if (f4 == -2.0) correct++;

  F32 f5 = 0.125 + 0.125;
  if (f5 == 0.25) correct++;

  F32 f6 = 16777216.0 + 0.0;   /* 2^24, largest exact integer run */
  if (f6 == 16777216.0) correct++;

  return correct == tests;
}

Bool F32Subtraction()
{
  I64 tests = 6, correct = 0;
  F32 f1 = 0.75 - 0.25;
  if (f1 == 0.5) correct++;

  F32 f2 = 4.0 - 2.5;
  if (f2 == 1.5) correct++;

  F32 f3 = 100.5 - 0.5;
  if (f3 == 100.0) correct++;

  F32 f4 = (-2.0) - (1.25);
  if (f4 == -3.25) correct++;

  F32 f5 = 0.25 - 0.125;
  if (f5 == 0.125) correct++;

  F32 f6 = 10.0 - 10.0;
  if (f6 == 0.0) correct++;
  return correct == tests;
}

Bool F32Division()
{
  I64 tests = 6, correct = 0;
  F32 f1 = 1.0 / 4.0;
  if (f1 == 0.25) correct++;

  F32 f2 = 10.0 / 2.5;
  if (f2 == 4.0) correct++;

  F32 f3 = -6.0 / 2.0;
  if (f3 == -3.0) correct++;

  F32 f4 = 7.0 / 2.0;
  if (f4 == 3.5) correct++;

  F32 f5 = 0.25 / 0.25;
  if (f5 == 1.0) correct++;

  F32 f6 = 100.0 / 8.0;
  if (f6 == 12.5) correct++;
  return correct == tests;
}

Bool F32Multiplication()
{
  I64 tests = 6, correct = 0;
  F32 f1 = 0.5 * 0.5;
  if (f1 == 0.25) correct++;

  F32 f2 = 2.5 * 4.0;
  if (f2 == 10.0) correct++;

  F32 f3 = -3.0 * 2.0;
  if (f3 == -6.0) correct++;

  F32 f4 = 1.25 * 8.0;
  if (f4 == 10.0) correct++;

  F32 f5 = 0.125 * 0.125;
  if (f5 == 0.015625) correct++;

  F32 f6 = 9.0 * 0.0;
  if (f6 == 0.0) correct++;
  return correct == tests;
}

Bool F32Operators()
{
  I64 tests = 9, correct = 0;
  /* Use F32 variables so the relational ops genuinely exercise the
   * single-precision compare path, not an F64 literal fold. */
  F32 a = 1.5, b = 89.125, c = 1.75, d = 420.5;

  if (a * 8.0 > 10.0) correct++;   /* 1.5 * 8 = 12 */
  if (b == 89.125) correct++;
  if (d != b) correct++;
  if (a < b) correct++;
  if (a <= a) correct++;
  if (c > a) correct++;
  if (a == a) correct++;
  if (c >= a) correct++;
  if (a >= a) correct++;
  return tests == correct;
}

Bool F32Conversions()
{
  I64 tests = 6, correct = 0;

  /* int -> F32 */
  I64 n = 7;
  F32 a = n;
  if (a == 7.0) correct++;

  /* F32 -> int (truncates toward zero) */
  F32 b = 3.75;
  I64 i = b;
  if (i == 3) correct++;

  /* F32 -> F64 (widening keeps the value) */
  F32 c = 1.5;
  F64 d = c;
  if (d == 1.5) correct++;

  /* F64 -> F32 (narrowing, value is representable) */
  F64 e = 2.25;
  F32 f = e;
  if (f == 2.25) correct++;

  /* mixed F32 + F64 promotes to F64 */
  F32 g = 1.5;
  F64 h = g + 2.5;
  if (h == 4.0) correct++;

  /* negative -> int truncation */
  F32 k = -2.75;
  I64 m = k;
  if (m == -2) correct++;

  return correct == tests;
}

Bool F32Precision()
{
  I64 tests = 3, correct = 0;

  /* 2^24 + 1 is not representable in single precision and rounds back
   * to 2^24 - the defining difference between F32 and F64. */
  F32 a = 16777217.0;
  if (a == 16777216.0) correct++;

  /* The same value held in an F64 keeps its exact magnitude. */
  F64 b = 16777217.0;
  if (b != 16777216.0) correct++;

  /* Round-tripping a representable value through F32 is lossless. */
  F32 c = 0.5;
  F64 d = c;
  F32 e = d;
  if (e == 0.5) correct++;

  return correct == tests;
}

class F32Vec { F32 x; F32 y; F32 z; };
class F32Mix { U8  tag; F32 f; F64 d; };

/* Aggregate initialiser items are bare F64 literals; they must be coerced
 * to each field's real type and stored at the field's offset (an F32 field
 * gets a 4-byte single, not a truncated 8-byte double). */
Bool F32StructInit()
{
  I64 tests = 5, correct = 0;
  F32Vec v = {1.0, 2.5, 3.0};
  if (v.x == 1.0) correct++;
  if (v.y == 2.5) correct++;
  if (v.z == 3.0) correct++;
  F32Mix m = {7, 0.25, 8.5};
  if (m.tag == 7 && m.f == 0.25) correct++;
  if (m.d == 8.5) correct++;
  return correct == tests;
}

I32 Main()
{
  "Test - F32 (single precision) arithmetic:\n";
  I64 tests = 8, correct = 0;
  if (F32Addition()) correct++;
  if (F32Subtraction()) correct++;
  if (F32Division()) correct++;
  if (F32Multiplication()) correct++;
  if (F32Operators()) correct++;
  if (F32Conversions()) correct++;
  if (F32Precision()) correct++;
  if (F32StructInit()) correct++;
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
