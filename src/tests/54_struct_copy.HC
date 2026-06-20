#include "testhelper.HC"

/* By-value struct/class assignment and copy-initialisation: `dst = src` and
 * `T d = src;` between two values of the same class copy the bytes (memcpy),
 * not the address. Regression for a missing feature: `c = black` raised
 * "the `=` operator cannot be applied to a class", and `Color c = black;`
 * silently stored the source's *address* into the first 8 bytes (garbage).
 * Covers local/global/deref targets, struct-returning calls, chained
 * assignment, and that the copy is independent of the source. */

class Color { F32 r, g, b; };
class Pt    { I64 x, y; };

Color g_src = {7.0, 8.0, 9.0};
Color g_dst;

Color MakeColor(F32 v)
{
  Color c;
  c.r = v; c.g = v + 1.0; c.b = v + 2.0;
  return c;
}

Bool LocalAssign()
{
  Color black = {1.0, 2.0, 3.0};
  Color c;
  c = black;
  return c.r == 1.0 && c.g == 2.0 && c.b == 3.0;
}

Bool DeclInitCopy()
{
  Color black = {1.0, 2.0, 3.0};
  Color d = black;
  return d.r == 1.0 && d.g == 2.0 && d.b == 3.0;
}

Bool GlobalInvolved()
{
  Color black = {1.0, 2.0, 3.0};
  g_dst = black;                      /* global = local */
  Color e;
  e = g_src;                          /* local = global */
  return g_dst.r == 1.0 && g_dst.g == 2.0 && g_dst.b == 3.0 &&
         e.r == 7.0 && e.g == 8.0 && e.b == 9.0;
}

Bool DerefCopy()
{
  Color black = {1.0, 2.0, 3.0};
  Color f;
  Color *pf = &f, *pb = &black;
  *pf = *pb;
  return f.r == 1.0 && f.g == 2.0 && f.b == 3.0;
}

Bool CallAndChained()
{
  Color a;
  a = MakeColor(5.0);                 /* assign from struct-returning call */
  Color src = {1.0, 2.0, 3.0};
  Color b, c;
  c = b = src;                        /* chained struct assignment */
  return a.r == 5.0 && a.g == 6.0 && a.b == 7.0 &&
         b.r == 1.0 && b.b == 3.0 && c.r == 1.0 && c.b == 3.0;
}

Bool IndependentCopy()
{
  /* The copy must be a real value copy, not an alias: mutating the source
   * afterwards must not change the destination. */
  Pt p1; p1.x = 10; p1.y = 20;
  Pt p2;
  p2 = p1;
  p1.x = 99; p1.y = 88;
  return p2.x == 10 && p2.y == 20 && p1.x == 99 && p1.y == 88;
}

I32 Main()
{
  "Test - by-value struct/class copy and assignment:\n";
  I64 tests = 6, correct = 0;
  if (LocalAssign())     correct++;
  if (DeclInitCopy())    correct++;
  if (GlobalInvolved())  correct++;
  if (DerefCopy())       correct++;
  if (CallAndChained())  correct++;
  if (IndependentCopy()) correct++;
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
