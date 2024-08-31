#include "testhelper.HC"

class Point
{
  I64 x;
  I64 y;
};

U0 PointerAddressing()
{
  I64 *j = 0, correct = 0, tests = 2;
  Point *ptr = MAlloc(sizeof(Point *));

  ptr->x = 10;
  if (ptr->x == 10) {
    correct++;
  }

  j = &(ptr->x);
  if (*j == 10) {
    correct++;
  }
  Free(ptr);
  PrintResult(correct,tests);
}

I32 Main()
{
  "Addressing: ";
  PointerAddressing();
  "====\n";
  return 0;
}
