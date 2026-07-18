#include "testhelper.HC"

/* A derived class with NO fields of its own used to lose its entire
 * inheritance: parseClassOffsets early-returned on an empty field list
 * before flattening the base class in, so `class Empty:Base {};` had
 * no fields and size 0 - every member access was a compile error.
 * 24_union.HC covers inheritance where the derived class adds fields;
 * this covers the empty-derived shape plus layout/copy details. */

class Base
{
  I64 a;
  U8 b;
};

class Empty:Base
{
};

class Grand:Empty
{
  I64 c;
};

class NarrowBase { U8 lo; };
class WideDerived:NarrowBase { I64 wide; };

U0 FillBase(Base *base)
{
  base->a = 111;
  base->b = 7;
}

I32 Main()
{
  "Test - class inheritance (empty derived, layout, upcast): ";
  I64 correct = 0, tests = 7;

  if (sizeof(Empty) == sizeof(Base)) correct++;

  Empty e;
  e.a = 42;
  e.b = 9;
  if (e.a == 42 && e.b == 9) correct++;

  /* An empty derived class still works as a base one level down. */
  Grand g;
  g.a = 1;
  g.b = 2;
  g.c = 3;
  if (g.a == 1 && g.b == 2 && g.c == 3) correct++;
  if (sizeof(Grand) == sizeof(Base) + 8) correct++;

  /* Derived's first own field re-aligns past a small base. */
  WideDerived w;
  w.lo = 5;
  w.wide = 123456789;
  if (w.lo == 5 && w.wide == 123456789) correct++;
  if (sizeof(WideDerived) == 16) correct++;

  /* Upcast: base fields sit at offset 0, so a Base* view works. */
  FillBase(&e);
  if (e.a == 111 && e.b == 7) correct++;

  PrintResult(correct,tests);
  "====\n";
  return 0;
}
