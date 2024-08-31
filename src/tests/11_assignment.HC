#include "testhelper.HC"

I64 Identity(I64 i)
{
  return i;
}

U0 Assign()
{
  I64 correct = 0, tests = 2;
  "= test: ";

  I64 i = 10;
  if (i == 10) {
    correct++;
  }
  
  i = Identity(69);
  if (i == 69) {
    correct++;
  }
  PrintResult(correct,tests);
}

U0 AddAssign()
{
  I64 correct = 0, tests = 2;
  "+= test: ";
  I64 i = 10;

  i += 20;
  if (i == 30) {
    correct++;
  }

  i += Identity(69);
  if (i == 99) {
    correct++;
  }
  PrintResult(correct,tests);
}

U0 SubAssign()
{
  I64 correct = 0, tests = 2;
  "-= test: ";
  I64 i = 10;

  i -= 20;
  if (i == -10) {
    correct++;
  }

  i -= Identity(10);
  if (i == -20) {
    correct++;
  }
  PrintResult(correct,tests);
}

U0 MulAssign()
{
  I64 correct = 0, tests = 2;
  "*= test: ";
  I64 i = 10;

  i *= 10;  
  if (i == 100) {
    correct++;
  }

  i *= Identity(2);
  if (i == 200) {
    correct++;
  }
  PrintResult(correct,tests);
}

U0 DivAssign()
{
  I64 correct = 0, tests = 2;
  "/= test: ";
  I64 i = 100;

  i /= 10;
  if (i == 10) {
    correct++;
  }

  i /= Identity(10);
  if (i == 1) {
    correct++;
  }

  PrintResult(correct,tests);
}

U0 ModAssign()
{
  I64 correct = 0, tests = 2;
  "%%= test: ";
  I64 i = 35;

  i %= 32;
  if (i == 3) {
    correct++;
  }

  i %= Identity(2);
  if (i == 1) {
    correct++;
  }
  PrintResult(correct,tests);
}

U0 OrAssign()
{
  I64 correct = 0, tests = 2;
  "|= test: ";
  I64 i = 1;

  i |= 8;
  if (i == 9) {
    correct++;
  }
  
  i |= Identity(4);
  if (i == 13) {
    correct++;
  }
  PrintResult(correct,tests);
}

U0 AndAssign()
{
  I64 correct = 0, tests = 2;
  "&= test: ";
  I64 i = 11;

  i &= 1;
  if (i == 1) {
    correct++;
  }

  i &= Identity(4);
  if (i == 0) {
    correct++;
  }
  
  PrintResult(correct,tests);
}

U0 XorAssign()
{ // Toggling bits
  I64 correct = 0, tests = 2;
  "^= test: ";
  I64 i = 8;

  i ^= 1;
  if (i == 9) {
    correct++;
  }

  i ^= Identity(1);
  if (i == 8) {
    correct++;
  }
  PrintResult(correct,tests);
}

U0 ShrAssign()
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
  PrintResult(correct,tests);
}

U0 ShlAssign()
{
  I64 correct = 0, tests = 2;
  "<<= test: ";
  I64 i = 1;

  i <<= 10;
  if (i == 1024) {
    correct++;
  }

  i <<= Identity(1);
  if (i == 2048) {
    correct++;
  }
  PrintResult(correct,tests);
}

I32 Main()
{
  "Assignment tests:\n";
  Assign();
  AddAssign();
  SubAssign();
  MulAssign();
  DivAssign();
  ModAssign();
  AndAssign();
  OrAssign();
  XorAssign();
  ShrAssign();
  ShlAssign();
  "=====\n";
  return 0;
}
