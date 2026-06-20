#include "testhelper.HC"

/* Struct layout + the `alignof` operator. Sizes/alignments are checked
 * against the C ABI (a struct's alignment is its widest member's, and
 * its size is rounded up to that - not blindly to 8). */

class Color     { U8 r; U8 g; U8 b; U8 a; };        /* size 4,  align 1 */
class Rectangle { F32 x; F32 y; F32 w; F32 h; };    /* size 16, align 4 */
class Mixed     { I8 a; I64 b; I8 c; };             /* size 24, align 8 */
class S2        { I16 x; I16 y; };                  /* size 4,  align 2 */
class S3        { I8 a; I32 b; };                   /* size 8,  align 4 */
class S5        { I8 a; I8 b; I8 c; };              /* size 3,  align 1 */
class Inner     { U8 a; U8 b; U8 c; U8 d; };        /* size 4,  align 1 */
class Outer     { U8 x; Inner i; };                /* size 5,  align 1 */
class Has8      { U8 x; I64 y; };                   /* size 16, align 8 */
class OuterH    { U8 x; Has8 h; };                 /* size 24, align 8 */

Bool ScalarAlign()
{
  I64 tests = 9, correct = 0;
  if (alignof(U8)  == 1) correct++;
  if (alignof(Bool)== 1) correct++;
  if (alignof(I16) == 2) correct++;
  if (alignof(I32) == 4) correct++;
  if (alignof(I64) == 8) correct++;
  if (alignof(U64) == 8) correct++;
  if (alignof(F32) == 4) correct++;
  if (alignof(F64) == 8) correct++;
  if (alignof(U8 *)== 8) correct++;
  return correct == tests;
}

Bool StructSize()
{
  I64 tests = 10, correct = 0;
  if (sizeof(Color)     == 4)  correct++;
  if (sizeof(Rectangle) == 16) correct++;
  if (sizeof(Mixed)     == 24) correct++;
  if (sizeof(S2)        == 4)  correct++;
  if (sizeof(S3)        == 8)  correct++;
  if (sizeof(S5)        == 3)  correct++;
  if (sizeof(Inner)     == 4)  correct++;
  if (sizeof(Outer)     == 5)  correct++;
  if (sizeof(Has8)      == 16) correct++;
  if (sizeof(OuterH)    == 24) correct++;
  return correct == tests;
}

Bool StructAlign()
{
  I64 tests = 8, correct = 0;
  if (alignof(Color)     == 1) correct++;
  if (alignof(Rectangle) == 4) correct++;
  if (alignof(Mixed)     == 8) correct++;
  if (alignof(S2)        == 2) correct++;
  if (alignof(S3)        == 4) correct++;
  if (alignof(Inner)     == 1) correct++;
  if (alignof(Outer)     == 1) correct++;
  if (alignof(OuterH)    == 8) correct++;
  return correct == tests;
}

Bool AlignofExpr()
{
  /* alignof works on an expression, not just a type name. */
  I64 tests = 3, correct = 0;
  Color  c;
  OuterH o;
  F64    d;
  if (alignof(c) == 1) correct++;
  if (alignof(o) == 8) correct++;
  if (alignof(d) == 8) correct++;
  return correct == tests;
}

Bool SizeMultipleOfAlign()
{
  /* Invariant: sizeof is always a multiple of alignof. */
  I64 tests = 4, correct = 0;
  if (sizeof(Mixed)  % alignof(Mixed)  == 0) correct++;
  if (sizeof(OuterH) % alignof(OuterH) == 0) correct++;
  if (sizeof(Outer)  % alignof(Outer)  == 0) correct++;
  if (sizeof(S3)     % alignof(S3)     == 0) correct++;
  return correct == tests;
}

I32 Main()
{
  "Test - alignof and struct layout:\n";
  I64 tests = 5, correct = 0;
  if (ScalarAlign())         correct++;
  if (StructSize())          correct++;
  if (StructAlign())         correct++;
  if (AlignofExpr())         correct++;
  if (SizeMultipleOfAlign()) correct++;
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
