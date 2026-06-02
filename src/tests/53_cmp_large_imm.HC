#include "testhelper.HC"

/* Comparing an integer against a large constant whose low 12 bits are zero
 * (a clean multiple of 4096, e.g. 0x100000) must use the shifted-imm12
 * encoding rather than truncating to the low 12 bits.
 *
 * Regression for a JIT-only bug: aarch64_enc_cmp_imm hardcoded the shift bit
 * to 0 and masked imm & 0xFFF, so `x == 1048576` (0x100000) compiled to
 * `cmp x, #0` and returned false under -jit. The AOT path (text asm via
 * clang) was always correct, so this only reproduced on the JIT. */

Bool ShiftedEquality()
{
  I64 c = 0, t = 4;
  I64 a = 1048576;        /* 0x100000 = 0x100 << 12 */
  if (a == 1048576)   c++;
  if (!(a == 0))      c++;
  I64 b = 4096;           /* 0x1000 = 1 << 12 */
  if (b == 4096)      c++;
  I64 d = 16769024;       /* 0xFFD000, near the top of the shift-12 range */
  if (d == 16769024)  c++;
  return c == t;
}

Bool ShiftedRelational()
{
  I64 c = 0, t = 6;
  I64 a = 1048576;
  if (a <  2097152)   c++;   /* < 0x200000 */
  if (a <= 1048576)   c++;
  if (a >  524288)    c++;   /* > 0x80000  */
  if (a >= 1048576)   c++;
  if (a != 1048575)   c++;
  if (a != 1052672)   c++;   /* 0x101000, also shift-12 encodable */
  return c == t;
}

Bool NarrowReadThenCompare()
{
  /* The same constants compared after a narrow (I32/U32) read. */
  I32 a = 1048576;
  U32 b = 4096000;           /* 0x3E8000, low 12 bits zero */
  return a == 1048576 && b == 4096000;
}

Bool PlainAndRegPath()
{
  /* A plain imm12 (<=0xFFF) and a non-encodable constant (forced onto the
   * register compare path) must both still work. */
  I64 a = 4095;              /* 0xFFF */
  I64 b = 1000000;           /* 0xF4240, low 12 bits non-zero -> reg path */
  return a == 4095 && b == 1000000 && a != 4094 && b != 999999;
}

I32 Main()
{
  "Test - integer compare against large (shift-12) immediates:\n";
  I64 tests = 4, correct = 0;
  if (ShiftedEquality())       correct++;
  if (ShiftedRelational())     correct++;
  if (NarrowReadThenCompare()) correct++;
  if (PlainAndRegPath())       correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
