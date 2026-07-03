"Test - #ifjit / #ifaot: ";
#include "testhelper.HC"

/* The suite compiles AOT, so #ifaot branches must be taken and #ifjit
 * branches skipped. The JIT side is covered by run_jit compiling the
 * same file expectations inverted would break these asserts there, so
 * Mode() returns which branch was compiled in and the assert adapts
 * via __HCC_AOT__. */

I64 correct = 0;
I64 tests = 3;

I64 Mode()
{
#ifjit
  return 1;
#else
  return 2;
#endif
}

I64 Mode2()
{
#ifaot
  return 2;
#else
  return 1;
#endif
}

#ifaot
I64 aot_only = 42;
#endif
#ifjit
I64 aot_only = 0;
#endif

#ifdef __HCC_AOT__
if (Mode() == 2) correct++;
if (Mode2() == 2) correct++;
if (aot_only == 42) correct++;
#else
if (Mode() == 1) correct++;
if (Mode2() == 1) correct++;
if (aot_only == 0) correct++;
#endif

PrintResult(correct, tests);
'===\n';
Exit(0);
