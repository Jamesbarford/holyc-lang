#include "testhelper.HC"

/* Global aggregate initialisers: class/struct + arrays.
 *
 * Regression coverage for several bugs that all lived in the static-data
 * path for globals:
 *   1. `class`/`struct` globals with a `{...}` initialiser failed to parse
 *      (the global `=` path used parseExpr, which can't read `{`).
 *   2. sub-8-byte array/field element literals were emitted at the *literal's*
 *      width (always 8 bytes), so `U32 a[]={7,8,9}` was written with an
 *      8-byte stride and read back as `7 0 8`.
 *   3. expression elements (`-1`, `2+3`, `1<<4`) were not constant-folded, so
 *      the emitter read garbage / wrote 0 bytes.
 *   4. ieee754_64 zeroed the mantissa of negative doubles, so `F64 f[]={-1.5}`
 *      was stored as -1.0.
 *
 * Note: narrow *signed* global reads (I8/I16) do not yet sign-extend on load
 * - a separate, pre-existing bug in the read path - so signed-negative cases
 * here use I32/I64, and the narrow-width/stride cases use the unsigned types
 * (positive values still exercise the stride fix). */

class ColorI { I32 r,g,b; };
class ColorF { F32 r,g,b; };

ColorI g_ci = {11, 22, 33};
ColorF g_cf = {-2.5, 1.25, 4.75};

U8  g_u8[3]  = {7, 8, 9};
U16 g_u16[3] = {300, 4000, 500};
U32 g_u32[3] = {7, 8, 9};
I32 g_i32[3] = {-100000, 200000, -300000};
I64 g_i64[3] = {-1, -2, -3};
I32 g_expr[3] = {1000 * 1000, 7 + 8, 5 << 3};
F64 g_f64[3] = {-1.5, 2.5, -3.5};
F32 g_f32[3] = {-1.5, 2.5, -3.5};

Bool GlobalClassInt()
{
  return g_ci.r == 11 && g_ci.g == 22 && g_ci.b == 33;
}

Bool GlobalClassFloat()
{
  return g_cf.r == -2.5 && g_cf.g == 1.25 && g_cf.b == 4.75;
}

Bool GlobalClassWrite()
{
  g_ci.g = 99;
  Bool ok = g_ci.r == 11 && g_ci.g == 99 && g_ci.b == 33;
  g_ci.g = 22;     /* restore so re-runs are stable */
  return ok;
}

Bool GlobalNarrowArrays()
{
  /* The stride bug: each element must occupy its own width, not 8 bytes. */
  I64 c = 0, t = 3;
  if (g_u8[0]  == 7   && g_u8[1]  == 8    && g_u8[2]  == 9)   c++;
  if (g_u16[0] == 300 && g_u16[1] == 4000 && g_u16[2] == 500) c++;
  if (g_u32[0] == 7   && g_u32[1] == 8    && g_u32[2] == 9)   c++;
  return c == t;
}

Bool GlobalSignedArrays()
{
  I64 c = 0, t = 2;
  if (g_i32[0] == -100000 && g_i32[1] == 200000 && g_i32[2] == -300000) c++;
  if (g_i64[0] == -1 && g_i64[1] == -2 && g_i64[2] == -3) c++;
  return c == t;
}

Bool GlobalExprFold()
{
  return g_expr[0] == 1000000 && g_expr[1] == 15 && g_expr[2] == 40;
}

Bool GlobalFloatArrays()
{
  I64 c = 0, t = 2;
  if (g_f64[0] == -1.5 && g_f64[1] == 2.5 && g_f64[2] == -3.5) c++;
  if (g_f32[0] == -1.5 && g_f32[1] == 2.5 && g_f32[2] == -3.5) c++;
  return c == t;
}

I32 Main()
{
  "Test - global aggregate initialisers (class/struct + arrays):\n";
  I64 tests = 7, correct = 0;
  if (GlobalClassInt())    correct++;
  if (GlobalClassFloat())  correct++;
  if (GlobalClassWrite())  correct++;
  if (GlobalNarrowArrays()) correct++;
  if (GlobalSignedArrays()) correct++;
  if (GlobalExprFold())    correct++;
  if (GlobalFloatArrays()) correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
