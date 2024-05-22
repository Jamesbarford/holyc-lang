#include "testhelper.HC"

Bool TestIfElse()
{
  I64 test_cases = 4, passed = 0;
  I64 i = 0, j = 0;
  if (i == 0) {
    passed++;
  }

  i = 9;
  if (i == 0) {
    PrintResult(passed,test_cases);
    return FALSE;
  } else if (i == 9) {
    passed++;
  } else {
    PrintResult(passed,test_cases);
    return FALSE;
  }

  i = 10;
  j = 420;
  if (i == 0 && j == 69) {
    PrintResult(passed,test_cases);
    return FALSE;
  } else if (i == 10 && j == 439) {
    PrintResult(passed,test_cases);
    return FALSE;
  } else {
    passed++;
  }

  i = 10;
  j = 7;
  if (i == 7 || j == 7) {
    passed++;
  }

  return passed == test_cases;
}

I32 Main()
{
  "Test - 'if', 'else' and 'else if': ";
  I64 tests = 1, correct = 0;
  if (TestIfElse()) correct++;
  PrintResult(correct,tests);
  return 0;
}
