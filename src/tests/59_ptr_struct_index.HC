#include "testhelper.HC"

/* Address-of a subscripted POINTER: `&ptr[i]` and `&ptr[i].field`.
 *
 * Regression for a parser bug: in the unary-operator path, a `[` subscript
 * only bound to the operand for array/class-ref types, not pointers - so
 * `&t[i]` (t a pointer) parsed as `(&t)[i]` = `*((&t)+i)`, indexing the
 * pointer variable's own address by sizeof(ptr) instead of computing
 * `t + i*sizeof(*t)`. For a pointer to a 24-byte struct that read adjacent
 * stack/heap and bus-errored. The fix also keeps consuming the postfix chain
 * after the subscript so `&t[i].x` is `&(t[i].x)`, not `(&t[i]).x`. */

class Tile { I64 x, y; I32 type; };

Bool AddressOfElement()
{
  Tile arr[4];
  Tile *t = arr;                       /* pointer to a 24-byte struct */
  for (I64 i = 0; i < 4; ++i) {
    Tile *it = &t[i];                  /* the bug: must be t + i*24 */
    it->x = i; it->y = i * 10; it->type = i * 100;
  }
  I64 c = 0;
  for (I64 i = 0; i < 4; ++i)
    if (t[i].x == i && t[i].y == i * 10 && t[i].type == i * 100) c++;
  return c == 4;
}

Bool AddressOfField()
{
  Tile arr[3];
  Tile *t = arr;
  I64 *xp = &t[1].x;     *xp = 777;    /* &(t[1].x), not (&t[1]).x */
  I32 *tp = &t[2].type;  *tp = 55;
  return t[1].x == 777 && t[2].type == 55;
}

Bool PointerToElement()
{
  Tile arr[3];
  Tile *t = arr;
  Tile *p = &t[1];
  p->type = 42;
  p->x = 9;
  return t[1].type == 42 && t[1].x == 9;
}

Bool ScalarPointerIndex()
{
  I64 a[5];
  I64 *p = a;
  for (I64 i = 0; i < 5; ++i) { I64 *e = &p[i]; *e = i * 7; }
  return p[0] == 0 && p[3] == 21 && p[4] == 28;
}

I32 Main()
{
  "Test - &ptr[i] / &ptr[i].field on pointers to structs:\n";
  I64 tests = 4, correct = 0;
  if (AddressOfElement())   correct++;
  if (AddressOfField())     correct++;
  if (PointerToElement())   correct++;
  if (ScalarPointerIndex()) correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
