#include "testhelper.HC"

/* Every size a class can be when it travels in registers, in and out of the
 * same function. A struct's last register holds whatever is left of it, and
 * arm64 has no single load or store for 3, 5, 6 or 7 bytes, so those sizes
 * did not compile. The bytes have their top bit set because a word load
 * sign-extends, and each class sits between guards because storing a whole
 * register would clobber its neighbour. */

class Cell
{
  I32 x, y, z;
};

class Wide
{
  I64 x, y;
};

class Shorts
{
  I16 x, y, z;
};

Cell AddCell(Cell a, Cell b)
{
  Cell r;
  r.x = a.x + b.x;
  r.y = a.y + b.y;
  r.z = a.z + b.z;
  return r;
}

Wide AddWide(Wide a, Wide b)
{
  Wide r;
  r.x = a.x + b.x;
  r.y = a.y + b.y;
  return r;
}

Shorts AddShorts(Shorts a, Shorts b)
{
  Shorts r;
  r.x = a.x + b.x;
  r.y = a.y + b.y;
  r.z = a.z + b.z;
  return r;
}

I64 g_correct = 0;
I64 g_total = 0;

U0 Check(Bool ok, U8 *what, I64 size)
{
  g_total++;
  if (ok) g_correct++;
  else "%s: failed for %d bytes\n", what, size;
}

Bool Guarded(U8 *g1, U8 *g2, U8 *g3)
{
  I64 i;
  for (i = 0; i < 8; i++) {
    if (g1[i] != 0xAA || g2[i] != 0xBB || g3[i] != 0xCC) return FALSE;
  }
  return TRUE;
}

class B1 { U8 b[1]; };

B1 Fill1(U8 seed)
{
  B1 r;
  I64 i;
  for (i = 0; i < 1; i++) r.b[i] = seed + i;
  return r;
}

B1 Add1(I64 x, B1 a, I64 y, B1 b)
{
  B1 r;
  I64 i;
  for (i = 0; i < 1; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late1(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B1 s)
{
  I64 i, sum = g;
  for (i = 0; i < 1; i++) sum += s.b[i];
  return sum;
}

U0 Test1()
{
  U8 g1[8];
  B1 v;
  U8 g2[8];
  B1 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill1(0xF0);
  w = Add1(1, v, 2, v);
  for (i = 0; i < 1; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 1);
  Check(added, "passed and returned", 1);
  Check(Late1(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 1);
  Check(Guarded(g1, g2, g3), "guards", 1);
}

class B2 { U8 b[2]; };

B2 Fill2(U8 seed)
{
  B2 r;
  I64 i;
  for (i = 0; i < 2; i++) r.b[i] = seed + i;
  return r;
}

B2 Add2(I64 x, B2 a, I64 y, B2 b)
{
  B2 r;
  I64 i;
  for (i = 0; i < 2; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late2(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B2 s)
{
  I64 i, sum = g;
  for (i = 0; i < 2; i++) sum += s.b[i];
  return sum;
}

U0 Test2()
{
  U8 g1[8];
  B2 v;
  U8 g2[8];
  B2 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill2(0xF0);
  w = Add2(1, v, 2, v);
  for (i = 0; i < 2; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 2);
  Check(added, "passed and returned", 2);
  Check(Late2(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 2);
  Check(Guarded(g1, g2, g3), "guards", 2);
}

class B3 { U8 b[3]; };

B3 Fill3(U8 seed)
{
  B3 r;
  I64 i;
  for (i = 0; i < 3; i++) r.b[i] = seed + i;
  return r;
}

B3 Add3(I64 x, B3 a, I64 y, B3 b)
{
  B3 r;
  I64 i;
  for (i = 0; i < 3; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late3(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B3 s)
{
  I64 i, sum = g;
  for (i = 0; i < 3; i++) sum += s.b[i];
  return sum;
}

U0 Test3()
{
  U8 g1[8];
  B3 v;
  U8 g2[8];
  B3 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill3(0xF0);
  w = Add3(1, v, 2, v);
  for (i = 0; i < 3; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 3);
  Check(added, "passed and returned", 3);
  Check(Late3(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 3);
  Check(Guarded(g1, g2, g3), "guards", 3);
}

class B4 { U8 b[4]; };

B4 Fill4(U8 seed)
{
  B4 r;
  I64 i;
  for (i = 0; i < 4; i++) r.b[i] = seed + i;
  return r;
}

B4 Add4(I64 x, B4 a, I64 y, B4 b)
{
  B4 r;
  I64 i;
  for (i = 0; i < 4; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late4(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B4 s)
{
  I64 i, sum = g;
  for (i = 0; i < 4; i++) sum += s.b[i];
  return sum;
}

U0 Test4()
{
  U8 g1[8];
  B4 v;
  U8 g2[8];
  B4 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill4(0xF0);
  w = Add4(1, v, 2, v);
  for (i = 0; i < 4; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 4);
  Check(added, "passed and returned", 4);
  Check(Late4(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 4);
  Check(Guarded(g1, g2, g3), "guards", 4);
}

class B5 { U8 b[5]; };

B5 Fill5(U8 seed)
{
  B5 r;
  I64 i;
  for (i = 0; i < 5; i++) r.b[i] = seed + i;
  return r;
}

B5 Add5(I64 x, B5 a, I64 y, B5 b)
{
  B5 r;
  I64 i;
  for (i = 0; i < 5; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late5(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B5 s)
{
  I64 i, sum = g;
  for (i = 0; i < 5; i++) sum += s.b[i];
  return sum;
}

U0 Test5()
{
  U8 g1[8];
  B5 v;
  U8 g2[8];
  B5 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill5(0xF0);
  w = Add5(1, v, 2, v);
  for (i = 0; i < 5; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 5);
  Check(added, "passed and returned", 5);
  Check(Late5(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 5);
  Check(Guarded(g1, g2, g3), "guards", 5);
}

class B6 { U8 b[6]; };

B6 Fill6(U8 seed)
{
  B6 r;
  I64 i;
  for (i = 0; i < 6; i++) r.b[i] = seed + i;
  return r;
}

B6 Add6(I64 x, B6 a, I64 y, B6 b)
{
  B6 r;
  I64 i;
  for (i = 0; i < 6; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late6(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B6 s)
{
  I64 i, sum = g;
  for (i = 0; i < 6; i++) sum += s.b[i];
  return sum;
}

U0 Test6()
{
  U8 g1[8];
  B6 v;
  U8 g2[8];
  B6 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill6(0xF0);
  w = Add6(1, v, 2, v);
  for (i = 0; i < 6; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 6);
  Check(added, "passed and returned", 6);
  Check(Late6(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 6);
  Check(Guarded(g1, g2, g3), "guards", 6);
}

class B7 { U8 b[7]; };

B7 Fill7(U8 seed)
{
  B7 r;
  I64 i;
  for (i = 0; i < 7; i++) r.b[i] = seed + i;
  return r;
}

B7 Add7(I64 x, B7 a, I64 y, B7 b)
{
  B7 r;
  I64 i;
  for (i = 0; i < 7; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late7(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B7 s)
{
  I64 i, sum = g;
  for (i = 0; i < 7; i++) sum += s.b[i];
  return sum;
}

U0 Test7()
{
  U8 g1[8];
  B7 v;
  U8 g2[8];
  B7 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill7(0xF0);
  w = Add7(1, v, 2, v);
  for (i = 0; i < 7; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 7);
  Check(added, "passed and returned", 7);
  Check(Late7(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 7);
  Check(Guarded(g1, g2, g3), "guards", 7);
}

class B8 { U8 b[8]; };

B8 Fill8(U8 seed)
{
  B8 r;
  I64 i;
  for (i = 0; i < 8; i++) r.b[i] = seed + i;
  return r;
}

B8 Add8(I64 x, B8 a, I64 y, B8 b)
{
  B8 r;
  I64 i;
  for (i = 0; i < 8; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late8(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B8 s)
{
  I64 i, sum = g;
  for (i = 0; i < 8; i++) sum += s.b[i];
  return sum;
}

U0 Test8()
{
  U8 g1[8];
  B8 v;
  U8 g2[8];
  B8 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill8(0xF0);
  w = Add8(1, v, 2, v);
  for (i = 0; i < 8; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 8);
  Check(added, "passed and returned", 8);
  Check(Late8(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 8);
  Check(Guarded(g1, g2, g3), "guards", 8);
}

class B9 { U8 b[9]; };

B9 Fill9(U8 seed)
{
  B9 r;
  I64 i;
  for (i = 0; i < 9; i++) r.b[i] = seed + i;
  return r;
}

B9 Add9(I64 x, B9 a, I64 y, B9 b)
{
  B9 r;
  I64 i;
  for (i = 0; i < 9; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late9(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B9 s)
{
  I64 i, sum = g;
  for (i = 0; i < 9; i++) sum += s.b[i];
  return sum;
}

U0 Test9()
{
  U8 g1[8];
  B9 v;
  U8 g2[8];
  B9 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill9(0xF0);
  w = Add9(1, v, 2, v);
  for (i = 0; i < 9; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 9);
  Check(added, "passed and returned", 9);
  Check(Late9(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 9);
  Check(Guarded(g1, g2, g3), "guards", 9);
}

class B10 { U8 b[10]; };

B10 Fill10(U8 seed)
{
  B10 r;
  I64 i;
  for (i = 0; i < 10; i++) r.b[i] = seed + i;
  return r;
}

B10 Add10(I64 x, B10 a, I64 y, B10 b)
{
  B10 r;
  I64 i;
  for (i = 0; i < 10; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late10(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B10 s)
{
  I64 i, sum = g;
  for (i = 0; i < 10; i++) sum += s.b[i];
  return sum;
}

U0 Test10()
{
  U8 g1[8];
  B10 v;
  U8 g2[8];
  B10 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill10(0xF0);
  w = Add10(1, v, 2, v);
  for (i = 0; i < 10; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 10);
  Check(added, "passed and returned", 10);
  Check(Late10(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 10);
  Check(Guarded(g1, g2, g3), "guards", 10);
}

class B11 { U8 b[11]; };

B11 Fill11(U8 seed)
{
  B11 r;
  I64 i;
  for (i = 0; i < 11; i++) r.b[i] = seed + i;
  return r;
}

B11 Add11(I64 x, B11 a, I64 y, B11 b)
{
  B11 r;
  I64 i;
  for (i = 0; i < 11; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late11(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B11 s)
{
  I64 i, sum = g;
  for (i = 0; i < 11; i++) sum += s.b[i];
  return sum;
}

U0 Test11()
{
  U8 g1[8];
  B11 v;
  U8 g2[8];
  B11 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill11(0xF0);
  w = Add11(1, v, 2, v);
  for (i = 0; i < 11; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 11);
  Check(added, "passed and returned", 11);
  Check(Late11(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 11);
  Check(Guarded(g1, g2, g3), "guards", 11);
}

class B12 { U8 b[12]; };

B12 Fill12(U8 seed)
{
  B12 r;
  I64 i;
  for (i = 0; i < 12; i++) r.b[i] = seed + i;
  return r;
}

B12 Add12(I64 x, B12 a, I64 y, B12 b)
{
  B12 r;
  I64 i;
  for (i = 0; i < 12; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late12(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B12 s)
{
  I64 i, sum = g;
  for (i = 0; i < 12; i++) sum += s.b[i];
  return sum;
}

U0 Test12()
{
  U8 g1[8];
  B12 v;
  U8 g2[8];
  B12 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill12(0xF0);
  w = Add12(1, v, 2, v);
  for (i = 0; i < 12; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 12);
  Check(added, "passed and returned", 12);
  Check(Late12(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 12);
  Check(Guarded(g1, g2, g3), "guards", 12);
}

class B13 { U8 b[13]; };

B13 Fill13(U8 seed)
{
  B13 r;
  I64 i;
  for (i = 0; i < 13; i++) r.b[i] = seed + i;
  return r;
}

B13 Add13(I64 x, B13 a, I64 y, B13 b)
{
  B13 r;
  I64 i;
  for (i = 0; i < 13; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late13(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B13 s)
{
  I64 i, sum = g;
  for (i = 0; i < 13; i++) sum += s.b[i];
  return sum;
}

U0 Test13()
{
  U8 g1[8];
  B13 v;
  U8 g2[8];
  B13 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill13(0xF0);
  w = Add13(1, v, 2, v);
  for (i = 0; i < 13; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 13);
  Check(added, "passed and returned", 13);
  Check(Late13(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 13);
  Check(Guarded(g1, g2, g3), "guards", 13);
}

class B14 { U8 b[14]; };

B14 Fill14(U8 seed)
{
  B14 r;
  I64 i;
  for (i = 0; i < 14; i++) r.b[i] = seed + i;
  return r;
}

B14 Add14(I64 x, B14 a, I64 y, B14 b)
{
  B14 r;
  I64 i;
  for (i = 0; i < 14; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late14(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B14 s)
{
  I64 i, sum = g;
  for (i = 0; i < 14; i++) sum += s.b[i];
  return sum;
}

U0 Test14()
{
  U8 g1[8];
  B14 v;
  U8 g2[8];
  B14 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill14(0xF0);
  w = Add14(1, v, 2, v);
  for (i = 0; i < 14; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 14);
  Check(added, "passed and returned", 14);
  Check(Late14(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 14);
  Check(Guarded(g1, g2, g3), "guards", 14);
}

class B15 { U8 b[15]; };

B15 Fill15(U8 seed)
{
  B15 r;
  I64 i;
  for (i = 0; i < 15; i++) r.b[i] = seed + i;
  return r;
}

B15 Add15(I64 x, B15 a, I64 y, B15 b)
{
  B15 r;
  I64 i;
  for (i = 0; i < 15; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late15(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B15 s)
{
  I64 i, sum = g;
  for (i = 0; i < 15; i++) sum += s.b[i];
  return sum;
}

U0 Test15()
{
  U8 g1[8];
  B15 v;
  U8 g2[8];
  B15 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill15(0xF0);
  w = Add15(1, v, 2, v);
  for (i = 0; i < 15; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 15);
  Check(added, "passed and returned", 15);
  Check(Late15(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 15);
  Check(Guarded(g1, g2, g3), "guards", 15);
}

class B16 { U8 b[16]; };

B16 Fill16(U8 seed)
{
  B16 r;
  I64 i;
  for (i = 0; i < 16; i++) r.b[i] = seed + i;
  return r;
}

B16 Add16(I64 x, B16 a, I64 y, B16 b)
{
  B16 r;
  I64 i;
  for (i = 0; i < 16; i++) r.b[i] = a.b[i] + b.b[i] + x + y;
  return r;
}

/* Out of registers: arrives on the stack */
I64 Late16(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, B16 s)
{
  I64 i, sum = g;
  for (i = 0; i < 16; i++) sum += s.b[i];
  return sum;
}

U0 Test16()
{
  U8 g1[8];
  B16 v;
  U8 g2[8];
  B16 w;
  U8 g3[8];
  I64 i, want = 7;
  Bool filled = TRUE, added = TRUE;

  MemSet(g1, 0xAA, 8);
  MemSet(g2, 0xBB, 8);
  MemSet(g3, 0xCC, 8);
  v = Fill16(0xF0);
  w = Add16(1, v, 2, v);
  for (i = 0; i < 16; i++) {
    if (v.b[i] != ((0xF0 + i) & 0xFF)) filled = FALSE;
    if (w.b[i] != ((2 * (0xF0 + i) + 3) & 0xFF)) added = FALSE;
    want += (0xF0 + i) & 0xFF;
  }
  Check(filled, "returned", 16);
  Check(added, "passed and returned", 16);
  Check(Late16(1, 2, 3, 4, 5, 6, 7, v) == want, "on the stack", 16);
  Check(Guarded(g1, g2, g3), "guards", 16);
}

I32 Main()
{
  "Test - class by value sizes: ";
  Cell a, b, c;
  Wide d, e;
  Shorts f, g;

  a.x = 1; a.y = 2; a.z = 3;
  b.x = 10; b.y = 20; b.z = 30;
  c = AddCell(a, b);
  Check(c.x == 11 && c.y == 22 && c.z == 33, "3 x I32", 12);

  d.x = 1; d.y = 2;
  e = AddWide(d, d);
  Check(e.x == 2 && e.y == 4, "2 x I64", 16);

  f.x = 1; f.y = -2; f.z = 3;
  g = AddShorts(f, f);
  Check(g.x == 2 && g.y == -4 && g.z == 6, "3 x I16", 6);

  Test1;
  Test2;
  Test3;
  Test4;
  Test5;
  Test6;
  Test7;
  Test8;
  Test9;
  Test10;
  Test11;
  Test12;
  Test13;
  Test14;
  Test15;
  Test16;

  PrintResult(g_correct, g_total);
  "====\n";
  return g_correct != g_total;
}
