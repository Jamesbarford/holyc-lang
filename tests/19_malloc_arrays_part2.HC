#include "testhelper.HC"

class Point
{
  I64 x;
  I64 y;
};

Bool IntArray()
{
  I64 *arr = MAlloc(sizeof(I64) * 10);
  I64 j = 1, correct = 0, tests = 10;
  arr[0] = 1;
  arr[1] = 2;
  arr[2] = 3;
  arr[3] = 4;
  arr[4] = 5;
  arr[5] = 6;
  arr[6] = 7;
  arr[7] = 8;
  arr[8] = 9;
  arr[9] = 10;
    
  if (arr[0] == 1)  correct++;
  if (arr[1] == 2)  correct++;
  if (arr[2] == 3)  correct++;
  if (arr[3] == 4)  correct++;
  if (arr[4] == 5)  correct++;
  if (arr[5] == 6)  correct++;
  if (arr[6] == 7)  correct++;
  if (arr[7] == 8)  correct++;
  if (arr[8] == 9)  correct++;
  if (arr[9] == 10) correct++;

  Free(arr);
  return correct == tests;
}

Bool PointerArray()
{
  I64 j = 1, correct = 0, tests = 2;
  Point *arr = MAlloc(sizeof(Point *) * 10);
  
  arr[8].y = 420;
  arr[0].x = 10;

  if (arr[0].x == 10) correct++;
  if (arr[8].y == 420) correct++;

  Free(arr);
  return correct == tests;
}

I32 Main()
{
  I64 tests = 2, correct = 0;
  "Test - Malloced Arrays: ";
  if (IntArray()) correct++;
  if (PointerArray()) correct++;
  PrintResult(correct,tests);
  return 0;
}
