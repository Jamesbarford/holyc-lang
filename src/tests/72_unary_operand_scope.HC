#include "testhelper.HC"

/* A unary operator takes the postfix chain after it and nothing more. With
 * a subscript in the chain it used to take the rest of the expression:
 * `-v[0] * 10 + v[1]` was `-(v[0] * 10 + v[1])`. */

class Pair
{
  I64 x;
  I64 a[2];
};

I64 g_correct = 0;
I64 g_total = 0;

U0 Check(I64 got, I64 want, U8 *what)
{
  g_total++;
  if (got == want) g_correct++;
  else "%s: got %ld, expected %ld\n", what, got, want;
}

I32 Main()
{
  "Test - unary operand scope: ";
  I64 v[3], m[2][2], *pv = v;
  F64 f[2];
  Pair p, arr[2], *q = &p;

  v[0] = 2; v[1] = 3; v[2] = 0;
  f[0] = 2.0; f[1] = 3.0;
  m[1][1] = 5;
  p.a[1] = 3;
  arr[1].x = 9;
  arr[1].a[1] = 4;

  Check(-v[0] * 10 + v[1], -17, "negate");
  Check((-f[0] * 10.0 + f[1])(I64), -17, "negate a float");
  Check(-m[1][1] * 2 + 1, -9, "negate 2d");
  Check(-pv[1] * 2 + 1, -5, "negate through a pointer");
  Check(-q->a[1] * 2 + 1, -5, "negate an arrow field");
  Check(-p.a[1] * 2 + 1, -5, "negate a dot field");
  Check(-arr[1].x * 2 + 1, -17, "negate a field of an element");
  Check(-arr[1].a[1] * 2 + 1, -7, "negate subscripts either side of a field");
  Check(-v[v[2]] * 2, -4, "negate a nested subscript");
  Check(- -v[0] * 2, 4, "negate twice");
  Check(+v[0] * 3, 6, "unary plus");
  Check(~v[0] & 1, 1, "bit not");
  Check(!v[2] && 0, 0, "not, then and");
  Check(!v[2] || 0, 1, "not, then or");
  Check(!v[0] + 5, 5, "not, then add");
  Check(++v[0] * 2, 6, "pre-increment");
  v[0] = 2;
  Check(--v[1] + 1, 3, "pre-decrement");
  v[1] = 3;
  Check(-v[0]++ * 2, -4, "post-increment under negate");
  v[0] = 2;
  Check(-v[0](I64) * 3, -6, "postfix cast");
  Check(-v[0] < v[1], 1, "comparison");

  PrintResult(g_correct, g_total);
  "====\n";
  return g_correct != g_total;
}
