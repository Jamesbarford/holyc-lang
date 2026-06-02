#include "testhelper.HC"

/* C++17-style `if` with an init-statement and/or a declaration used
 * directly as the condition: `if (init; cond)`. The declarations are
 * scoped to the `if` (its then and else bodies). */

class Foo { U32 x; };

I32 Main()
{
  I64 tests = 9, correct = 0;

  /* plain if is unchanged */
  if (1) correct++;
  if (0) ; else correct++;

  /* a declaration used as the condition */
  if (I64 a = 5) correct++;            /* a = 5 is truthy */
  if (I64 b = 0) ; else correct++;     /* b = 0 is falsy  */

  /* init-statement followed by a separate condition expression */
  if (I64 c = 10; c > 3) correct++;

  /* init-statement followed by a declaration-condition (the if.cpp shape) */
  Foo foo;
  if (Foo *f = &foo; I64 x = 32) {
    f->x = x;
    if (f->x == 32) correct++;
  }

  /* an expression init-statement (not a declaration) */
  I64 g = 0;
  if (g = 7; g == 7) correct++;

  /* else body also sees the if-scoped declaration */
  if (I64 d = 0; d) correct++; else if (d == 0) correct++;

  /* the init var is scoped to the if: reusing the name afterwards is fine */
  I64 a = 99;
  if (a == 99) correct++;

  "Test - if with initialiser (`if (init; cond)`):\n";
  PrintResult(correct, tests);
  "====\n";
  return 0;
}
