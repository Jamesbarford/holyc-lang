#include "testhelper.HC"
/* Copy propagation must invalidate an entry when the slot it names as
 * its SOURCE is overwritten.
 *
 * The store-to-read forwarding pass records "this slot holds whatever
 * that slot holds". It killed the entry for a slot that was written,
 * but not the entries that pointed AT it, so a three-way swap folded
 * into nothing:
 *
 *     t = p;    recorded: t holds p
 *     p = q;    recorded: p holds q; the entry for t is now stale
 *     q = t;    t rewritten to p, p rewritten to q  ->  q = q
 *
 * Dead-store elimination then removed the other two stores as unread
 * and the swap vanished, leaving two names for the same value. Any
 * swap through a temporary was affected, which is most of them. */

class Pair { I32 v; };

I32 PointerSwap() {
  Pair a; Pair b;
  a.v = 1; b.v = 2;
  Pair *p = &a;
  Pair *q = &b;
  Pair *t = p;
  p = q;
  q = t;
  if (p == q) return FALSE;
  return p->v == 2 && q->v == 1;
}

/* Inside a loop, and an even number of swaps must land back where it
 * started - the shape a two-list algorithm uses every iteration. */
I32 LoopSwap() {
  Pair a; Pair b;
  a.v = 1; b.v = 2;
  Pair *p = &a;
  Pair *q = &b;
  for (I32 i = 0; i < 4; ++i) {
    Pair *t = p;
    p = q;
    q = t;
    if (p == q) return FALSE;
  }
  return p->v == 1 && q->v == 2;
}

/* The same rule for a slot rewritten in place by a non-store op. */
I32 InPlaceKill() {
  I32 a = 5;
  I32 b = a;      /* b holds a */
  a = a + 1;      /* a is 6; b must still be 5 */
  return b == 5 && a == 6;
}

/* Three-deep, so a stale entry has somewhere to propagate to. */
I32 Rotate() {
  I32 x = 1, y = 2, z = 3;
  I32 t = x;
  x = y;
  y = z;
  z = t;
  return x == 2 && y == 3 && z == 1;
}

I32 Main()
{
  "Test - swap forwarding: ";
  I64 tests = 4, correct = 0;

  if (PointerSwap()) correct++; else "Failed - PointerSwap()\n";
  if (LoopSwap())    correct++; else "Failed - LoopSwap()\n";
  if (InPlaceKill()) correct++; else "Failed - InPlaceKill()\n";
  if (Rotate())      correct++; else "Failed - Rotate()\n";

  PrintResult(correct,tests);
  "====\n";
  return 0;
}
