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

I32 Main()
{
  "Test Assigning pointer to a pointer from a class, with brackets: ";
  Example *e = MAlloc(sizeof(Example));
  I64 *p = &(e->x); 
  Assign(p);
  PrintResult(*p == 9, 1);
  "====\n";
  return 0;
}
