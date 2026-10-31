#include "testhelper.HC"
/* An `asm {}` block may write any register, so a parameter still in its
 * argument register must not be read from there after one.
 *
 * Store-to-read forwarding treated only calls as clobbering argument
 * registers. x0-x2 were protected by accident - they were the emitter's
 * scratch, assumed clobbered by everything - and x3-x7 were not
 * protected at all: `s` below read 99 back out of x3. Moving the
 * scratch off x0-x2 took the accident away from those too. */

#ifdef __aarch64__
I64 InX0(I64 a) {
  I64 k = 1;
  asm {
    MOV X0, 99
    MOV X1, 98
  }
  return a + k;
}

I64 InX3(I64 p, I64 q, I64 r, I64 s) {
  I64 k = 1;
  asm {
    MOV X3, 99
  }
  return s + k + p + q + r - 6;
}
#else
I64 InX0(I64 a) { return a + 1; }
I64 InX3(I64 p, I64 q, I64 r, I64 s) { return s + 1 + p + q + r - 6; }
#endif

I32 Main(I32 argc, U8 **argv)
{
  "Test - asm blocks clobber argument registers: ";
  I64 tests = 2, correct = 0;

  if (InX0(argc * 5) == 6) correct++;
  else "Failed - a param in x0 was read after an asm block wrote x0\n";

  if (InX3(1, 2, 3, argc * 7) == 8) correct++;
  else "Failed - a param in x3 was read after an asm block wrote x3\n";

  PrintResult(correct,tests);
  "====\n";
  return 0;
}
