#include "testhelper.HC"

I64 TestWhileBreak()
{
  I64 i = 0;
  while (i != 5) {
    i++;
    if (i == 3) {
      break;
    }
  }
  return i == 3;
}

I64 TestWhileContinue()
{ // j should never get incremented
  I64 i = 0, j = 0;
  while (i != 5) {
    if (i != 5) {
      i++;
      continue;
    }
    j++;
  }
  return j == 0;
}

I64 TestForBreak()
{
  I64 j = 0;
  for (I64 i = 0; i < 5; ++i) {
    if (j == 3) {
      break;
    }
    j++;
  }
  return j == 3;
}

I64 TestForContinue()
{ // j should get incremented once
  I64 j = 0;
  for (I64 i = 0; i < 5; i++) {
    if (i < 4) {
      continue;
    }
    j++;
  }
  return j == 1;
}

I32 Main()
{
  "Test - 'break' and 'continue' inside while and for loops: ";
  I64 correct = 0, tests = 4;
  correct += TestWhileBreak();
  correct += TestWhileContinue();
  correct += TestForBreak();
  correct += TestForContinue();
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
