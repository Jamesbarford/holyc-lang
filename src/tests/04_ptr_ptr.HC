#include "testhelper.HC"

class Point
{
  I64 x;
  I64 y;
};

I32 Main()
{
  "Test - Pointer to a pointer from a class: ";
  Point *p = MAlloc(sizeof(Point *));
  p->y = 420; // assign 420
  Point **ptr = &p; // Take a pointer to pointer p
  Point *x = *ptr;  // Dereference to get the pointer to p
  PrintResult(x->y == 420, 1); // x should be the original value we set
  "=====\n";
  return 0;
}
