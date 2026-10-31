/* An index folded into an addressing mode must survive the trip.
 *
 * irFuseIaddIntoMemAddressing sinks an address computation into the
 * memory instruction that consumes it. That MOVES the read of both
 * operands forward, so a value living in a register has to still be
 * there when the consumer runs. The base was asked that question
 * (irLocIsScratchClobbered + irRangeClobbersScratch); the index was
 * asked only whether it crossed a CALL.
 *
 * A global's address is materialised into the scratch pair - `adrp x1,
 * _g@PAGE` - which is not a call and destroys an index sitting in x1
 * just the same. x1 is where the second integer argument arrives, so
 * `void f(struct S *s, long i) { s->e[i] += g; }` folded the index
 * down past the lea and then indexed by the ADDRESS of `g`:
 *
 *     adrp x1, _g_v@PAGE
 *     add  x1, x1, _g_v@PAGEOFF
 *     str  x0, [x13, x1, lsl #3]     <- x1 is no longer `i`
 *
 * A wild store, so it bus-errored rather than answering wrongly. It
 * needed no flags: default AOT and the JIT both did it, on aarch64
 * only. x86_64 was unaffected - its index lands elsewhere.
 *
 * Every value below is what clang produces for the same program.
 */
#include "testhelper.HC"
extern "c" U0 *memset(U0 *p, I32 c, U64 n);
class S { I64 e[8]; F64 d[8]; };

I64 g_add = 5;
I64 g_idx = 3;
F64 g_fadd = 2.5;

/* The shape that crashed: the index is the second parameter, so it
   arrives in the register the global's address is materialised into. */
U0 BumpG(S *s, I64 i)      { s->e[i] += g_add; }
/* The global on the LOAD side of the read-modify-write. */
U0 BumpG2(S *s, I64 i)     { s->e[i] = s->e[i] + g_add + 1; }
/* A plain store, no RMW, with a global between address and use. */
U0 SetG(S *s, I64 i)       { s->e[i] = g_add; }
/* The index is itself read from a global. */
U0 BumpAtGlobalIdx(S *s)    { s->e[g_idx] += g_add; }
/* Floats take the other emitter path. */
U0 BumpFloat(S *s, I64 i)  { s->d[i] += g_fadd; }

I32 GlobalBetweenAddressAndUse()
{
  S s;
  memset(&s, 0, sizeof s);

  s.e[4] = 100;
  BumpG(&s, 4);
  if (s.e[4] != 105) return FALSE;
  /* Nothing may have been written anywhere else. */
  if (s.e[0] != 0 || s.e[7] != 0) return FALSE;

  s.e[2] = 10;
  BumpG2(&s, 2);
  if (s.e[2] != 16) return FALSE;

  SetG(&s, 6);
  if (s.e[6] != 5) return FALSE;

  s.e[3] = 1;
  BumpAtGlobalIdx(&s);
  if (s.e[3] != 6) return FALSE;

  s.d[5] = 1.5;
  BumpFloat(&s, 5);
  return s.d[5] == 4.0;
}

/* The same hazard reached through every argument position: whichever
   register the index arrives in, the fold must not outlive it. */
U0 P1(S *s, I64 i)                 { s->e[i] += g_add; }
U0 P2(S *s, I64 a, I64 i)         { s->e[i] += g_add + a; }
U0 P3(S *s, I64 a, I64 b, I64 i) { s->e[i] += g_add + a + b; }

I32 EveryArgumentPosition()
{
  S s;
  memset(&s, 0, sizeof s);

  s.e[1] = 1;  P1(&s, 1);
  if (s.e[1] != 6) return FALSE;

  s.e[2] = 1;  P2(&s, 10, 2);
  if (s.e[2] != 16) return FALSE;

  s.e[3] = 1;  P3(&s, 10, 100, 3);
  return s.e[3] == 116;
}

I32 Main()
{
  "Test - index folded into an addressing mode survives: ";
  I64 tests = 2, correct = 0;

  if (GlobalBetweenAddressAndUse()) correct++;
  else "Failed - GlobalBetweenAddressAndUse()\n";
  if (EveryArgumentPosition()) correct++;
  else "Failed - EveryArgumentPosition()\n";

  PrintResult(correct,tests);
  "====\n";
  return 0;
}
