#include "testhelper.HC"

class Example
{
  I64 i1;
  I64 i2;
  I64 i3;
  I64 x;
  I64 i4;
};

U0 Assign(I64 *i)
{
  *i = 9;
}

Bool PointerAssignNoBrackets()
{
  Bool correct;
  Example *e = MAlloc(sizeof(Example));
  I64 *p = &e->x;

  Assign(p);
  correct = *p == 9;
  Free(e);
  return correct;
}

Bool PointerAssignBrackets()
{
  Bool correct;
  Example *e = MAlloc(sizeof(Example));
  I64 *p = &e->x;
  Assign(p);
  correct = *p == 9;
  Free(e);
  return correct;
}

I32 Main()
{
  "Test - Assigning pointer to a pointer from a class: ";
  I64 tests = 2, passed = 0;
  if (PointerAssignNoBrackets()) passed++;
  if (PointerAssignBrackets()) passed++;
  PrintResult(passed,tests);
  return 0;
}
