#include "testhelper.HC"

/* Passing a struct field read through a pointer (`p->field`) BY VALUE,
 * including alongside a struct copy into the same object.
 *
 * Two regressions:
 *  - a class-typed field read decayed to a scalar load of its first 8 bytes
 *    instead of its address, so the by-value-arg lowering dereferenced
 *    garbage (bus error);
 *  - once it decayed to its address, the by-value tagging mutated the shared
 *    operand pointer (`e` in `e->color`), which was ALSO the destination of
 *    the earlier `e->color = src` copy - corrupting that copy so it produced
 *    zeros. The fix returns a value private to the field read and keeps it
 *    out of the algebraic-identity fold while tagged. */

class Color { I32 r, g, b, a; };
class E     { Color color; };

Color DARKGRAY = {80, 80, 80, 255};

I64 SumColor(Color c) { return c.r + c.g + c.b + c.a; }

Bool FieldByValueAfterCopy()
{
  E storage;
  E *e = &storage;
  e->color = DARKGRAY;                  /* struct copy: global -> p->field */
  Bool ok = e->color.r == 80 && e->color.g == 80 &&
            e->color.b == 80 && e->color.a == 255;
  ok = ok && SumColor(e->color) == 495; /* pass p->field by value */
  ok = ok && SumColor(DARKGRAY) == 495; /* global directly (control) */
  return ok;
}

Bool FieldByValueNoCopy()
{
  E storage;
  E *e = &storage;
  e->color.r = 1; e->color.g = 2; e->color.b = 3; e->color.a = 4;
  return SumColor(e->color) == 10;
}

Bool TwoFieldArgs()
{
  E s1, s2;
  E *a = &s1, *b = &s2;
  a->color = DARKGRAY;
  b->color.r = 1; b->color.g = 1; b->color.b = 1; b->color.a = 1;
  return SumColor(a->color) == 495 && SumColor(b->color) == 4;
}

I32 Main()
{
  "Test - passing a struct field (p->field) by value:\n";
  I64 tests = 3, correct = 0;
  if (FieldByValueAfterCopy()) correct++;
  if (FieldByValueNoCopy())    correct++;
  if (TwoFieldArgs())          correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
