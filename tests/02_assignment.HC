#include "testhelper.HC"

I64 Identity(I64 i)
{
  return i;
}

Bool Assign()
{
  I64 correct = 0, tests = 2;
  I64 i = 10;

  if (i == 10) {
    correct++;
  }
  
  i = Identity(69);
  if (i == 69) {
    correct++;
  }

  if (correct != tests) {
    "= test: ";
    PrintResult(correct,tests);
  }

  return correct == tests;
}

Bool AddAssign()
{
  I64 correct = 0, tests = 2;
  I64 i = 10;

  i += 20;
  if (i == 30) {
    correct++;
  }

  i += Identity(69);
  if (i == 99) {
    correct++;
  }
  if (correct != tests) {
    "+= test: ";
    PrintResult(correct,tests);
  }

  return correct == tests;
}

Bool SubAssign()
{
  I64 correct = 0, tests = 2;
  I64 i = 10;

  i -= 20;
  if (i == -10) {
    correct++;
  }

  i -= Identity(10);
  if (i == -20) {
    correct++;
  }

  if (correct != tests) {
    "-= test: ";
    PrintResult(correct,tests);
  }
  return correct == tests;
}

Bool MulAssign()
{
  I64 correct = 0, tests = 2;
  I64 i = 10;

  i *= 10;  
  if (i == 100) {
    correct++;
  }

  i *= Identity(2);
  if (i == 200) {
    correct++;
  }

  if (correct != tests) {
    "*= test: ";
    PrintResult(correct,tests);
  }
  return correct == tests;
}

Bool DivAssign()
{
  I64 correct = 0, tests = 2;
  I64 i = 100;

  i /= 10;
  if (i == 10) {
    correct++;
  }

  i /= Identity(10);
  if (i == 1) {
    correct++;
  }

  if (correct != tests) {
    "/= test: ";
    PrintResult(correct,tests);
  }

  return correct == tests;
}

Bool ModAssign()
{
  I64 correct = 0, tests = 2;
  I64 i = 35;

  i %= 32;
  if (i == 3) {
    correct++;
  }

  i %= Identity(2);
  if (i == 1) {
    correct++;
  }

  if (correct != tests) {
    "%%= test: ";
    PrintResult(correct,tests);
  }
  return correct == tests;
}

Bool OrAssign()
{
  I64 correct = 0, tests = 2;
  I64 i = 1;

  i |= 8;
  if (i == 9) {
    correct++;
  }
  
  i |= Identity(4);
  if (i == 13) {
    correct++;
  }

  if (correct != tests) {
    "|= test: ";
    PrintResult(correct,tests);
  }
  return correct == tests;
}

Bool AndAssign()
{
  I64 correct = 0, tests = 2;
  I64 i = 11;

  i &= 1;
  if (i == 1) {
    correct++;
  }

  i &= Identity(4);
  if (i == 0) {
    correct++;
  }
  
  if (correct != tests) {
    "&= test: ";
    PrintResult(correct,tests);
  }
  return correct == tests;
}

Bool XorAssign()
{ // Toggling bits
  I64 correct = 0, tests = 2;
  I64 i = 8;

  i ^= 1;
  if (i == 9) {
    correct++;
  }

  i ^= Identity(1);
  if (i == 8) {
    correct++;
  }
  if (correct != tests) {
    "^= test: ";
    PrintResult(correct,tests);
  }
  return correct == tests;
}

Bool ShrAssign()
{
  I64 correct = 0, tests = 2;
  ">>= test: ";
  I64 i = 1024;

  i >>= 1;
  if (i == 512) {
    correct++;
  }

  i >>= Identity(1);
  if (i == 256) {
    correct++;
  }
  return correct == tests;
}

Bool ShlAssign()
{
  I64 correct = 0, tests = 2;
  I64 i = 1;

  i <<= 10;
  if (i == 1024) {
    correct++;
  }

  i <<= Identity(1);
  if (i == 2048) {
    correct++;
  }
  if (correct != tests) {
    "<<= test: ";
    PrintResult(correct,tests);
  }
  return correct == tests;
}

I32 Main()
{
  "Test - Assignments: ";
  I64 tests = 11;
  I64 correct = 0;
  if (Assign()) correct++;
  if (AddAssign()) correct++;
  if (SubAssign()) correct++;
  if (MulAssign()) correct++;
  if (DivAssign()) correct++;
  if (ModAssign()) correct++;
  if (AndAssign()) correct++;
  if (OrAssign()) correct++;
  if (XorAssign()) correct++;
  if (ShrAssign()) correct++;
  if (ShlAssign()) correct++;

  PrintResult(correct,tests);
  return 0;
}
