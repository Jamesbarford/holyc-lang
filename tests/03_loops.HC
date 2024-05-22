#include "testhelper.HC"

Bool TestForLoop()
{
  "Testing For Loop: ";
  I64 test_cases = 2, passed = 0;

  I64 x = 0, i = 0;
  for (I64 i = 0; i < 10; ++i) {
    x = i;
  }

  if (x == 9) {
    passed++;
  }

  for (i = x; i >= 0; --i) {
    x = i;
  }

  if (x == 0) {
    passed++;
  }

  return passed == test_cases;
}

Bool TestWhileLoop()
{
  "Testing while loop: ";
  I64 i = 0;
  I64 passed = 0, test_cases = 1;
  while (i < 9) {
    i++;
  }
  if (i == 9) {
    passed++;
  }
  return passed == test_cases;
}

Bool TestDoWhileLoop()
{
  "Testing do-while loop: ";
  I64 i = 10;
  do {
    i = i + 1;

  } while (i < 10);
  return i == 11;
}

I32 Main()
{
  "Test - while, for and do while: ";
  I64 tests = 3;
  I64 correct = 0;
  if (TestForLoop()) correct++;
  if (TestWhileLoop()) correct++;
  if (TestDoWhileLoop()) correct++;
  PrintResult(correct,tests);
  return 0;
}
