#include "testhelper.HC"

U0 TestForLoop()
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

  PrintResult(passed,test_cases);
}

U0 TestWhileLoop()
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
  PrintResult(passed,test_cases);
}

U0 TestDoWhileLoop()
{
  "Testing do-while loop: ";
  I64 i = 10;
  do {
    i = i + 1;

  } while (i < 10);
  PrintResult(i == 11, 1);
}

I32 Main()
{
  "Loop test suite\n";
  TestForLoop();
  TestWhileLoop();  
  TestDoWhileLoop();
  "=====\n";
  return 0;
}
