#include "testhelper.HC"

U0 TestIfElse()
{
  "Testing if else\n";
  I64 test_cases = 4, passed = 0;
  I64 i = 0, j = 0;
  if (i == 0) {
    passed++;
  }

  i = 9;
  if (i == 0) {
    PrintResult(passed,test_cases);
    return;
  } else if (i == 9) {
    passed++;
  } else {
    PrintResult(passed,test_cases);
    return;
  }

  i = 10;
  j = 420;
  if (i == 0 && j == 69) {
    PrintResult(passed,test_cases);
    return;
  } else if (i == 10 && j == 439) {
    PrintResult(passed,test_cases);
    return;
  } else {
    passed++;
  }

  i = 10;
  j = 7;
  if (i == 7 || j == 7) {
    passed++;
  }
  PrintResult(passed,test_cases);
}

I32 Main()
{
  TestIfElse();
  "=====\n";
  return 0;
}
