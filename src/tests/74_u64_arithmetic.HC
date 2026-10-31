#include "testhelper.HC"

/* A binary operator on integers always yielded a signed I64, so a U64 lost
 * its unsignedness at the first operator: `>>` filled with the sign bit and
 * `/` and `%` divided a negative. x86 also treated U64<->float as signed.
 *
 * Only U64 can make a result unsigned, because anything narrower widens to
 * 64 bits with its value intact. */

class Holder
{
  U64 u;
  I64 i;
};

U64 g_top = 0x8000000000000000;

I64 g_correct = 0;
I64 g_total = 0;

U0 Check(I64 got, I64 want, U8 *what)
{
  g_total++;
  if (got == want) g_correct++;
  else "%s: got %ld, expected %ld\n", what, got, want;
}

U0 CheckF(F64 got, F64 want, U8 *what)
{
  g_total++;
  if (got == want) g_correct++;
  else "%s: got %f, expected %f\n", what, got, want;
}

F64 TakeF64(F64 v) { return v; }
F64 U64ToF64(U64 v) { return v; }
U64 F64ToU64(F64 v) { return v; }
I64 F64ToI64(F64 v) { return v; }

I32 Main()
{
  "Test - U64 arithmetic: ";
  U64 u = 1, m = 0xFFFFFFFFFFFFFFFF, n = 1, t, k;
  I64 s = -8, r;
  I32 s32 = -8;
  I8 s8 = -8;
  U32 u32 = 0xFFFFFFFF;
  U8 u8 = 0xFF;
  F64 f;
  F32 g;
  Holder h;
  U64 a[2];
  u = u << 63;

  /* Shifts */
  r = u >> 63;         Check(r, 1, "U64 >>");
  r = m >> 60;         Check(r, 15, "U64 >> 60");
  r = g_top >> 63;     Check(r, 1, "global U64 >>");
  t = u; t >>= 63;     Check(t, 1, ">>=");
  h.u = u; h.u >>= 63; Check(h.u, 1, ">>= on a field");
  a[1] = u;
  r = a[1] >> 63;      Check(r, 1, "element >>");

  /* The unsignedness survives other operators */
  r = (u | 1) >> 63;   Check(r, 1, "after |");
  r = (u ^ m) >> 62;   Check(r, 1, "after ^");
  r = (m & u) >> 63;   Check(r, 1, "after &");
  r = (u + u - 1) >> 63; Check(r, 1, "after + and -");
  r = (m * 3) >> 63;   Check(r, 1, "after *");
  r = ~n >> 63;        Check(r, 1, "after ~");
  r = s(U64) >> 60;    Check(r, 15, "cast to U64");
  auto v = u >> 63;    Check(v, 1, "auto");

  /* Signed stays arithmetic */
  r = s >> 1;          Check(r, -4, "I64 >>");
  r = s >> n;          Check(r, -4, "a U64 count does not change the left");
  r = s32 >> 1;        Check(r, -4, "I32 >>");
  r = s8 >> 1;         Check(r, -4, "I8 >>");
  r = u(I64) >> 63;    Check(r, -1, "cast to I64");
  r = s; r >>= 1;      Check(r, -4, "signed >>=");
  h.i = -8;
  r = h.i >> 1;        Check(r, -4, "signed field >>");
  r = u32 >> 31;       Check(r, 1, "U32 >>");
  r = u8 >> 7;         Check(r, 1, "U8 >>");

  /* Division */
  r = m / 2;           Check(r, 0x7FFFFFFFFFFFFFFF, "U64 /");
  r = m % 10;          Check(r, 5, "U64 %");
  t = m; t /= 2;       Check(t, 0x7FFFFFFFFFFFFFFF, "/=");
  t = m; t %= 10;      Check(t, 5, "%=");
  r = s / 2;           Check(r, -4, "I64 /");
  r = s % 3;           Check(r, -2, "I64 %");

  /* A pointer difference can be negative */
  I64 *lo = 0x1000, *hi = 0x1010;
  r = lo - hi;         Check(r, -2, "pointer difference");
  r = (lo - hi) / 2;   Check(r, -1, "pointer difference /");
  r = (lo - hi) >> 1;  Check(r, -1, "pointer difference >>");
  r = (lo - hi) < 0;   Check(r, 1, "pointer difference < 0");

  /* Comparisons */
  r = u > n;           Check(r, 1, "U64 >");
  r = (u + 0) > 0;     Check(r, 1, "U64 expression > 0");
  r = s < 0;           Check(r, 1, "I64 < 0");
  r = (s + 1) < 0;     Check(r, 1, "I64 expression < 0");

  /* U64 to float */
  f = u;               CheckF(f, 9223372036854775808.0, "U64 to F64");
  f = m;               CheckF(f, 18446744073709551616.0, "max U64 to F64");
  g = u;               CheckF(g, 9223372036854775808.0, "U64 to F32");
  g = m;               CheckF(g, 18446744073709551616.0, "max U64 to F32");
  CheckF(U64ToF64(u), 9223372036854775808.0, "returned");
  CheckF(TakeF64(u), 9223372036854775808.0, "as an argument");
  CheckF(u + 0.0, 9223372036854775808.0, "U64 + float");
  CheckF(0.0 + u, 9223372036854775808.0, "float + U64");
  r = u > 0.0;         Check(r, 1, "U64 > float");
  r = 0.0 > u;         Check(r, 0, "float > U64");
  /* Halving must not lose the bit that decides the rounding */
  k = 0x8000000000000401; f = k;
  CheckF(f, 9223372036854777856.0, "sticky bit, F64");
  k = 0x8000008000000001; g = k;
  CheckF(g, 9223373136366403584.0, "sticky bit, F32");
  k = 1; f = k;        CheckF(f, 1.0, "small U64");
  f = s;               CheckF(f, -8.0, "I64 to F64");
  f = u32;             CheckF(f, 4294967295.0, "U32 to F64");

  /* Float to U64 */
  f = 9223372036854775808.0;
  t = f;               Check(t >> 63, 1, "F64 to U64");
  Check(F64ToU64(f) >> 63, 1, "returned as a U64");
  f = 18446744073709549568.0;
  t = f;               Check(t, 0xFFFFFFFFFFFFF800, "largest F64 below 2^64");
  g = 9223372036854775808.0;
  t = g;               Check(t >> 63, 1, "F32 to U64");
  f = 5.9;
  t = f;               Check(t, 5, "small F64 to U64");
  Check(F64ToI64(-5.9), -5, "returned as an I64");

  PrintResult(g_correct, g_total);
  "====\n";
  return g_correct != g_total;
}
