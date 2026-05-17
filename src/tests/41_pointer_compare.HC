#include "testhelper.HC"

/* Relational comparisons (`<`, `<=`, `>`, `>=`) between two pointers
 * of the same type. Used to be rejected even though `astIsValidPointerOp`
 * already allowed them; pointer arithmetic loops (e.g. `while (p<end)`)
 * depended on this. */

Bool U8PtrCompare()
{
  U8 buf[16];
  U8 *a = buf;
  U8 *b = buf + 7;
  return a < b &&
         b > a &&
         a <= a &&
         b >= b &&
         !(a > b) &&
         !(b < a);
}

Bool I64PtrCompare()
{
  I64 arr[4];
  I64 *p = arr;
  I64 *q = arr + 3;
  return p < q && q > p && !(p > q);
}

Bool WhileLoopOverBuffer()
{
  U8 buf[5] = {1, 2, 3, 4, 5};
  U8 *p = buf;
  U8 *end = buf + 5;
  I64 sum = 0;
  while (p < end) {
    sum += *p;
    p++;
  }
  return sum == 15;
}

I32 Main()
{
  "Test - Pointer relational comparisons: ";
  I64 correct = 0, tests = 3;
  if (U8PtrCompare) correct++;
  if (I64PtrCompare) correct++;
  if (WhileLoopOverBuffer) correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
