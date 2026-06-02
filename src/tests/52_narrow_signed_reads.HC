#include "testhelper.HC"

/* Narrow signed integer reads must sign-extend on load (C integer promotion):
 * a signed I8/I16 read in an expression widens to 64-bit with its sign. The
 * IR is signedness-lossy, so the builder emits SEXT/ZEXT at every read that
 * loads a scalar through an address.
 *
 * Regression for a pre-existing hole: only plain scalar *local* variable
 * reads promoted; scalar globals, global AND local class fields, array
 * elements and pointer derefs did not - so a signed I8 of -1 read full-width
 * (==, "%d", widening into a wider slot) came back as 255. Unsigned narrows
 * are checked too, to confirm they still zero-extend. */

class S { I8 a; I16 b; I32 c; U8 ua; U16 ub; };

I8  g_i8;
I16 g_i16;
U8  g_u8;
U16 g_u16;
S   g_s;
I8  g_i8arr[3];
I16 g_i16arr[3];

Bool ScalarGlobals()
{
  g_i8 = -1; g_i16 = -1000; g_u8 = 200; g_u16 = 50000;
  return g_i8 == -1 && g_i16 == -1000 && g_u8 == 200 && g_u16 == 50000;
}

Bool GlobalFields()
{
  g_s.a = -5; g_s.b = -2000; g_s.c = -100000; g_s.ua = 200; g_s.ub = 50000;
  return g_s.a == -5 && g_s.b == -2000 && g_s.c == -100000 &&
         g_s.ua == 200 && g_s.ub == 50000;
}

Bool GlobalArrays()
{
  g_i8arr[0]  = -3;   g_i8arr[1]  = -7;   g_i8arr[2]  = 100;
  g_i16arr[0] = -300; g_i16arr[1] = 4000; g_i16arr[2] = -5000;
  return g_i8arr[0]  == -3   && g_i8arr[1]  == -7   && g_i8arr[2]  == 100 &&
         g_i16arr[0] == -300 && g_i16arr[1] == 4000 && g_i16arr[2] == -5000;
}

Bool LocalFieldsAndArrays()
{
  S sl; sl.a = -11; sl.b = -3000; sl.ua = 250;
  I8 la[3]; la[0] = -9; la[1] = 5; la[2] = -1;
  return sl.a == -11 && sl.b == -3000 && sl.ua == 250 &&
         la[0] == -9 && la[1] == 5 && la[2] == -1;
}

Bool PointerDeref()
{
  I8  v = -42;   I8  *p = &v;
  I16 w = -1234; I16 *q = &w;
  return *p == -42 && *q == -1234;
}

Bool WidenIntoWiderSlot()
{
  /* The sign must survive widening into a wider slot, not only comparison. */
  g_i8 = -1;     I64 x = g_i8;
  g_i16 = -1000; I64 y = g_i16;
  g_s.a = -5;    I64 z = g_s.a;
  return x == -1 && y == -1000 && z == -5;
}

I32 Main()
{
  "Test - narrow signed integer reads sign-extend (all read paths):\n";
  I64 tests = 6, correct = 0;
  if (ScalarGlobals())        correct++;
  if (GlobalFields())         correct++;
  if (GlobalArrays())         correct++;
  if (LocalFieldsAndArrays()) correct++;
  if (PointerDeref())         correct++;
  if (WidenIntoWiderSlot())   correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
