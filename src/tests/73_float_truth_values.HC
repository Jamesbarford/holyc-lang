#include "testhelper.HC"

/* A comparison or logical operator yields 0 or 1 whatever it compared. A
 * float comparison was typed as a float, so its 0/1 went through a float
 * to int conversion on the way into a variable or an argument and came
 * out as 0. `Bool && F64` and `U8 + F64` did not type check at all. */

class Holder
{
  Bool b;
  I64 i;
  F64 f;
};

Bool g_less = 1.0 < 2.0;

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

I64 Pass(I64 v) { return v; }
Bool ReturnBool(F64 a, F64 b) { return a < b; }
F64 ReturnFloat(F64 a, F64 b) { return a < b; }

I32 Main()
{
  "Test - float truth values: ";
  F64 a = 1.0, b = 2.0, zero = 0.0, frac = 0.25, d;
  F32 fa = 1.5, fb = 2.5;
  I64 y, i = 3, low = 0x100, high = 0x200, none = 0, big = 0x100000000;
  U8 c = 65;
  Bool on = TRUE, off = FALSE, x;
  Holder h;

  /* Issue 284: the int side converts, not the float side */
  x = -1.5 > 0;   Check(x, 0, "-1.5 > 0");
  x = -1.5 < 0;   Check(x, 1, "-1.5 < 0");
  x = 2.5 > 2;    Check(x, 1, "2.5 > 2");
  x = a < i;      Check(x, 1, "float < int");
  x = i < a;      Check(x, 0, "int < float");

  /* Issue 285 */
  x = a < b;      Check(x, 1, "into a Bool");
  y = a < b;      Check(y, 1, "into an I64");
  y = a >= b;     Check(y, 0, ">=");
  y = a != b;     Check(y, 1, "!=");
  y = fa < fb;    Check(y, 1, "F32");
  y = fa < b;     Check(y, 1, "F32 < F64");
  d = a < b;      CheckF(d, 1.0, "into an F64");
  Check((a < b) + (a < b), 2, "as an operand");
  Check(Pass(a < b), 1, "as an argument");
  Check(ReturnBool(a, b), 1, "returned as a Bool");
  CheckF(ReturnFloat(a, b), 1.0, "returned as an F64");
  h.b = a < b;    Check(h.b, 1, "into a Bool field");
  h.i = a < b;    Check(h.i, 1, "into an I64 field");
  h.f = a < b;    CheckF(h.f, 1.0, "into an F64 field");
  Check(g_less, 1, "global initialiser");

  /* Logical operators and floats */
  y = a && b;     Check(y, 1, "float && float");
  y = zero && b;  Check(y, 0, "zero && float");
  y = zero || b;  Check(y, 1, "zero || float");
  y = frac && 1;  Check(y, 1, "a fraction is true");
  y = !a;         Check(y, 0, "!float");
  y = !zero;      Check(y, 1, "!zero");
  y = !frac;      Check(y, 0, "!fraction");

  /* Issue 286: the whole right operand is tested */
  y = none || low;   Check(y, 1, "0 || 0x100");
  y = low && high;   Check(y, 1, "0x100 && 0x200");
  y = big && big;    Check(y, 1, "above 32 bits");
  y = none || big;   Check(y, 1, "|| above 32 bits");

  /* Issue 288 */
  y = on && a > 0.0;   Check(y, 1, "Bool && comparison");
  y = off || a > 0.0;  Check(y, 1, "Bool || comparison");
  y = on && a;         Check(y, 1, "Bool && float");
  y = a && on;         Check(y, 1, "float && Bool");
  y = off && a;        Check(y, 0, "false && float");
  y = c && a;          Check(y, 1, "U8 && float");

  /* The same kind ordering rejected these */
  CheckF(c + 1.5, 66.5, "U8 + float");
  CheckF(1.5 + c, 66.5, "float + U8");
  CheckF(on + 0.5, 1.5, "Bool + float");
  y = c > 64.5;   Check(y, 1, "U8 > float");
  y = c == 65.0;  Check(y, 1, "U8 == float");
  c = 98.7;       Check(c, 98, "float into a U8");

  /* Range comparisons are comparisons too */
  y = 0.0 < a < 2.0;  Check(y, 1, "float range");
  y = 0.0 < a < 0.5;  Check(y, 0, "float range, false");

  PrintResult(g_correct, g_total);
  "====\n";
  return g_correct != g_total;
}
