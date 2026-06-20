#include "testhelper.HC"

/* Passing a GLOBAL struct by value as a call argument. Regression for a bus
 * error: irLowerGlobal returned a scalar load of a class global's first 8
 * bytes instead of its address, so the by-value argument lowering treated
 * those bytes as the struct's address and dereferenced garbage. A global
 * aggregate must decay to its address (like an array / like a local struct).
 * Covers all three AAPCS/SysV classes: small integer (in regs), large
 * (passed indirectly), and an HFA (in vector regs). */

class Small { I32 r, g, b, a; };   /* 16B integer aggregate */
class Big   { I64 a, b, c; };      /* 24B -> indirect / MEMORY */
class Hfa   { F32 x, y, z; };      /* homogeneous float aggregate */

Small gSmall = {1, 2, 3, 4};
Big   gBig   = {10, 20, 30};
Hfa   gHfa   = {1.5, 2.5, 3.5};

I64 SumSmall(Small s) { return s.r + s.g + s.b + s.a; }
I64 SumBig(Big b)     { return b.a + b.b + b.c; }
F64 SumHfa(Hfa h)     { return h.x + h.y + h.z; }

Bool GlobalSmallArg() { return SumSmall(gSmall) == 10; }
Bool GlobalBigArg()   { return SumBig(gBig) == 60; }
Bool GlobalHfaArg()   { return SumHfa(gHfa) == 7.5; }

I32 Main()
{
  "Test - passing a global struct by value as an argument:\n";
  I64 tests = 3, correct = 0;
  if (GlobalSmallArg()) correct++;
  if (GlobalBigArg())   correct++;
  if (GlobalHfaArg())   correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
