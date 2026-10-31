#include "testhelper.HC"
/* An `int` slot far enough below the frame pointer that its offset
 * fits no immediate form is loaded through a register offset. That
 * form forgot to sign-extend, so a negative int came back as a large
 * positive one and every 64-bit compare on it was wrong. -1 read as
 * 0xffffffff made `if (chosen < 0)` false; the hcc-built hcc then
 * indexed an array with it and crashed on every program with an `if`.
 *
 * The pad pushes the locals past 256 bytes below fp, and taking their
 * addresses keeps them in memory rather than in registers. */

U0 *g_sink;

U0
escape(U0 *p)
{
  g_sink = p;
}

I32
first_free(I32 *last, I32 n, I32 first)
{
  U8 pad[400];
  for (I32 i = 0; i < 400; ++i) pad[i] = (i)(U8);
  I32 chosen = -1;
  escape(&chosen);
  for (I32 s = 0; s < n; ++s) {
    if (last[s] < first) { chosen = s; break; }
  }
  if (chosen < 0) chosen = 99;
  return chosen + pad[3];
}

I32 NegativeIntCompares() {
  I32 last[2] = {5, 5};
  return first_free(last, 2, 1) == 102;
}

I64
widen(I32 v)
{
  U8 pad[400];
  pad[0] = 0;
  I32 x = v;
  escape(&x);
  escape(pad);
  I64 wide = x;
  return wide;
}

I32 NegativeIntWidens() {
  return widen(-7) == -7 && widen(-1) < 0 && widen(42) == 42;
}

I32 Main()
{
  "Test - signed loads from big frames: ";
  I64 tests = 2, correct = 0;

  if (NegativeIntCompares()) correct++; else "Failed - NegativeIntCompares()\n";
  if (NegativeIntWidens())   correct++; else "Failed - NegativeIntWidens()\n";

  PrintResult(correct,tests);
  "====\n";
  return 0;
}
