#include "testhelper.HC"
/* `a[i].field` where the element stride and the field width differ.
 *
 * AArch64's register-offset addressing has one bit for the shift, and
 * it means "scale by the ACCESS width" - so `[base, idx, lsl #n]` can
 * only ever express a scale equal to the size being loaded or stored.
 * `Point pts[3]` indexed by a variable wants `base + i*8` for a
 * four-byte field, which is `lsl #3` on a four-byte access. That is
 * not encodable, and both backends got it wrong in different ways:
 * the AOT assembler rejected the file outright, while the JIT's own
 * encoder accepted the request and scaled by 4, reading and writing
 * the element next door.
 *
 * Constant indices were fine throughout - they fold to a plain
 * displacement and never reach the scaled form - so this only ever
 * showed up with a variable index.
 *
 * Every combination below has a stride that differs from at least one
 * of its field widths. Values are distinct per element AND per field,
 * so an off-by-one-element read shows up as a wrong number rather than
 * an accidentally equal one. */

class P2 { I32 x, y; };                 /* stride 8,  fields 4       */
class P3 { U8 a; U8 b; I16 c; };  /* stride 4,  fields 1, 1, 2 */
class P4 { I64 a; I32 b; };            /* stride 16, fields 8, 4    */
class P5 { I16 s; I16 t; };         /* stride 4,  fields 2       */

I32 FourByteFieldsEightByteStride()
{
  P2 a[3];
  for (U64 i = 0; i < 3; ++i) {
    a[i].x = (i)(I32);
    a[i].y = (i)(I32) + 10;
  }
  for (U64 i = 0; i < 3; ++i) {
    if (a[i].x != (i)(I32)) return FALSE;
    if (a[i].y != (i)(I32) + 10) return FALSE;
  }
  return TRUE;
}

I32 NarrowFields()
{
  P3 b[3];
  P5 d[3];
  for (U64 i = 0; i < 3; ++i) {
    b[i].a = (i)(U8);
    b[i].b = ((i + 1))(U8);
    b[i].c = ((i + 100))(I16);
    d[i].s = (i)(I16);
    d[i].t = ((i + 7))(I16);
  }
  for (U64 i = 0; i < 3; ++i) {
    if (b[i].a != (i)(U8)) return FALSE;
    if (b[i].b != ((i + 1))(U8)) return FALSE;
    if (b[i].c != ((i + 100))(I16)) return FALSE;
    if (d[i].s != (i)(I16)) return FALSE;
    if (d[i].t != ((i + 7))(I16)) return FALSE;
  }
  return TRUE;
}

I32 MixedWidthsInOneElement()
{
  P4 c[3];
  for (U64 i = 0; i < 3; ++i) {
    c[i].a = (i)(I64) + 1000;   /* 8-byte field, 16-byte stride */
    c[i].b = (i)(I32) + 50;      /* 4-byte field, 16-byte stride */
  }
  for (U64 i = 0; i < 3; ++i) {
    if (c[i].a != (i)(I64) + 1000) return FALSE;
    if (c[i].b != (i)(I32) + 50) return FALSE;
  }
  return TRUE;
}

/* The shape it was found in: an array of structs nested inside another
   struct, so the base is itself an offset from the frame. */
class Grid { P2 pts[3]; U64 n; };

I32 NestedInAStruct()
{
  Grid g;
  g.n = 3;
  for (U64 i = 0; i < 3; ++i) {
    g.pts[i].x = (i)(I32);
    g.pts[i].y = 100;
  }
  if (g.n != 3) return FALSE;              /* nothing ran past the end */
  for (U64 i = 0; i < 3; ++i) {
    if (g.pts[i].x != (i)(I32)) return FALSE;
    if (g.pts[i].y != 100) return FALSE;
  }
  return TRUE;
}

I32 ThroughAPointer()
{
  P2 storage[3];
  P2 *a = storage;
  for (U64 i = 0; i < 3; ++i) {
    a[i].x = (i)(I32) * 3;
    a[i].y = (i)(I32) * 3 + 1;
  }
  for (U64 i = 0; i < 3; ++i) {
    if (a[i].x != (i)(I32) * 3) return FALSE;
    if (a[i].y != (i)(I32) * 3 + 1) return FALSE;
  }
  /* A constant index reaches the same bytes as a variable one. */
  return a[1].x == 3 && a[1].y == 4 && a[2].y == 7;
}

I32 Main()
{
  "Test - indexed struct fields: ";
  I64 tests = 5, correct = 0;

  if (FourByteFieldsEightByteStride()) correct++;
  else "Failed - FourByteFieldsEightByteStride()\n";
  if (NarrowFields())          correct++; else "Failed - NarrowFields()\n";
  if (MixedWidthsInOneElement()) correct++;
  else "Failed - MixedWidthsInOneElement()\n";
  if (NestedInAStruct())       correct++; else "Failed - NestedInAStruct()\n";
  if (ThroughAPointer())       correct++; else "Failed - ThroughAPointer()\n";

  PrintResult(correct,tests);
  "====\n";
  return 0;
}
