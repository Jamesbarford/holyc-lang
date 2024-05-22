#include "testhelper.HC"

class Point
{
  I64 x;
  I64 y;
};

Bool PointerAddressing()
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
  return correct == tests;
}

Bool PointerToAPointer()
{
  Point *p = MAlloc(sizeof(Point *));
  p->y = 420; // assign 420
  Point **ptr = &p; // Take a pointer to pointer p
  Point *x = *ptr;  // Dereference to get the pointer to p
  return x->y == 420; // x should be the original value we set
}

Bool PointerToALocal()
{
  I64 i = 420;
  I64 *x = &i;
  *x = 69;
  return *x == 69;
}

I32 Main()
{
  "Test - Pointer Addressing: ";
  I64 tests = 3, correct = 0;
  if (PointerAddressing()) correct++;
  if (PointerToAPointer()) correct++;
  if (PointerToALocal()) correct++;

  PrintResult(correct,tests);
  return 0;
}
