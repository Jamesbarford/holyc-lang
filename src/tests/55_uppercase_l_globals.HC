#include "testhelper.HC"

/* Global symbols whose name begins with an uppercase `L` (e.g. `LIGHTGRAY`)
 * must still assemble + link. On macOS/Mach-O the assembler treats any symbol
 * starting with `L` as a local (assembler-temporary) label, so emitting a
 * bare `.globl LIGHTGRAY` failed with "non-local symbol required". The fix
 * escapes such data-global symbols with a leading `_` consistently at the
 * definition and every reference. This test just has to compile, link and
 * read/write these globals correctly. */

class Color { U8 r, g, b, a; };

Color LIGHTGRAY = {200, 200, 200, 255};   /* class global, array-init */
I64   LArr[3]   = {10, 20, 30};            /* array global */
I64   Lscalar   = 42;                      /* scalar global */
U8   *Lstr      = "hello";                 /* string-pointer global */

Bool ClassGlobal()
{
  Bool ok = LIGHTGRAY.r == 200 && LIGHTGRAY.g == 200 &&
            LIGHTGRAY.b == 200 && LIGHTGRAY.a == 255;
  LIGHTGRAY.g = 99;                         /* write a field */
  ok = ok && LIGHTGRAY.g == 99;
  LIGHTGRAY.g = 200;                        /* restore */
  return ok;
}

Bool ArrayGlobal()
{
  Bool ok = LArr[0] == 10 && LArr[1] == 20 && LArr[2] == 30;
  LArr[1] = 77;
  ok = ok && LArr[1] == 77;
  LArr[1] = 20;
  return ok;
}

Bool ScalarGlobal()
{
  Bool ok = Lscalar == 42;
  Lscalar = 7;
  ok = ok && Lscalar == 7;
  Lscalar = 42;
  return ok;
}

Bool StringGlobal()
{
  return Lstr[0] == 'h' && Lstr[4] == 'o';
}

I32 Main()
{
  "Test - uppercase-L global symbols (Mach-O local-label escape):\n";
  I64 tests = 4, correct = 0;
  if (ClassGlobal())  correct++;
  if (ArrayGlobal())  correct++;
  if (ScalarGlobal()) correct++;
  if (StringGlobal()) correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
