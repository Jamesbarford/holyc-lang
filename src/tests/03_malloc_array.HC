#include "testhelper.HC"

U0 AssignI64(I64 *ptr)
{
  *ptr = 360;
}

Bool ArrayJuggling()
{
  I64 *ptr = 0;
  I64 arr[5][5];
  arr[0][0] = 0;
  arr[0][0] += 1; // arr[0][0] = 1
  arr[0][0]++;    // arr[0][0] = 2
  ++(arr[0][0]);  // arr[0][0] = 3
  ptr = &arr[0][0];
  *++ptr;        // ptr is now pointing at arr[0][1]
  *ptr = 69;     // arr[0][1] = 69
  return arr[0][1] == 69 && arr[0][0] == 3;
}

I32 Main()
{
  "Test - Juggling an array MAlloc(I64 * 10): ";
  I64 correct = 0, tests = 4;
  I64 *arr = MAlloc(sizeof(I64) * 10);
  I64 *ptr = &arr[2];
  AssignI64(ptr);
  if (*ptr == 360) {
    correct++;
  }

  arr[2] = 420;
  if (arr[2] == 420 && *ptr == 420) {
    correct++;
  }

  *ptr = 10;
  if (arr[2] == 10 && *ptr == 10) {
    correct++;
  }
  
  if (ArrayJuggling()) correct++;

  PrintResult(correct,tests);
  "====\n";
  return 0;
}
