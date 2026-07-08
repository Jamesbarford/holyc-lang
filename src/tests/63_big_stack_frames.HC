"Test - big stack frames: ";
#include "testhelper.HC"

/* aarch64 add/sub immediates encode 12 bits (optionally lsl #12);
 * frames past 4095 bytes once emitted unencodable prologue/epilogue
 * adjusts, `add x0, x29, #-big` local addresses, and out-of-range
 * ldr/str displacements. Every function here carries an 8KB pad so
 * all of those paths run in the >4095 regime. */

class BigPair
{
  I64 a;
  I64 b;
};

class BigHfa
{
  F64 x;
  F64 y;
};

class BigFive
{
  I64 vals[5];
};

I64 correct = 0;
I64 tests = 10;

I64 SumByValue(BigFive f)
{ /* 40-byte by-value param copied into a big frame */
  U8 pad[8192];
  pad[0] = 7;
  pad[8191] = 9;
  I64 s = 0;
  for (I64 i = 0; i < 5; i++) s = s + f.vals[i];
  return s + pad[0] + pad[8191];
}

BigPair MakePair(I64 base)
{ /* <=16-byte INTEGER-class return out of a big frame */
  U8 pad[8192];
  pad[100] = 1;
  BigPair p;
  p.a = base + pad[100];
  p.b = base * 2;
  return p;
}

BigHfa MakeHfa()
{ /* HFA return out of a big frame */
  U8 pad[8192];
  pad[50] = 2;
  I64 t = pad[50];
  BigHfa h;
  h.x = 1.5 + t;
  h.y = 2.5;
  return h;
}

U0 Main()
{
  U8 buf[8192];      /* prologue/epilogue immediate split */
  I64 sentinel = 42; /* scalar slot at a deep offset */
  buf[0] = 11;
  buf[8191] = 22;    /* constant-index store, disp > 4095 */
  U8 *p = &buf[8191]; /* address-of at a deep offset */
  I64 mid = 4000;
  buf[mid] = 33;      /* register-indexed store */

  BigFive five;
  for (I64 i = 0; i < 5; i++) five.vals[i] = i + 1;

  if (buf[0] == 11) correct++;
  if (buf[8191] == 22) correct++;
  if (*p == 22) correct++;
  if (buf[mid] == 33) correct++;
  if (sentinel == 42) correct++;
  if (SumByValue(five) == 31) correct++; /* 15 + 7 + 9 */

  BigPair pr = MakePair(10);
  if (pr.a == 11) correct++;
  if (pr.b == 20) correct++;

  BigHfa h = MakeHfa();
  if (h.x == 3.5) correct++;
  if (h.y == 2.5) correct++;
}
Main;

PrintResult(correct, tests);
'===\n';
Exit(0);
