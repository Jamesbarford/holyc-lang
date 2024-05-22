#include "testhelper.HC"

#define MATHS_MUL 0
#define MATHS_MOD 1
#define MATHS_SUB 2
#define MATHS_ADD 3
#define MATHS_DIV 5
#define MATHS_SHL 6
#define MATHS_SHR 7
#define MATHS_XMATHS_OR 8
#define MATHS_OR  9
#define MATHS_AND 10
#define MATHS_NOT 11

I64 TestRunner(I64 i1, I64 i2, I64 op, I64 expected)
{
  I64 answer = 0;
  if (op == MATHS_MUL) {
    answer = i1 * i2;
  } else if (op == MATHS_MOD) {
    answer = i1 % i2;
  } else if (op == MATHS_SUB) {
    answer = i1 - i2;
  } else if (op == MATHS_ADD) {
    answer = i1 + i2;
  } else if (op == MATHS_DIV) {
    answer = i1 / i2;
  } else if (op == MATHS_SHL) {
    answer = i1 << i2;
  } else if (op == MATHS_SHR) {
    answer = i1 >> i2;
  } else if (op == MATHS_XMATHS_OR) {
    answer = i1 ^ i2;
  } else if (op == MATHS_OR) {
    answer = i1 | i2;
  } else if (op == MATHS_AND) {
    answer = i1 & i2;
  } else if (op == MATHS_NOT) {
    answer = ~i1;
  } else {
    "operation: %d not supported\n";
    Exit(1);
  }
  return answer == expected;
}

U0 MulTests()
{
  I64 correct = 0;
  I64 total = 5;
  "Multiplication * : ";
  correct += TestRunner(3, 4, MATHS_MUL, 12);
  correct += TestRunner(-2, 6, MATHS_MUL, -12);
  correct += TestRunner(-3, -4, MATHS_MUL, 12);
  correct += TestRunner(5, 0, MATHS_MUL, 0);
  correct += TestRunner(0, -5, MATHS_MUL, 0);
  PrintResult(correct, total);
}

U0 ModTests()
{
  I64 correct = 0;
  I64 total = 5;
  "Modulo %% : ";
  correct += TestRunner(15, 4, MATHS_MOD, 3);
  correct += TestRunner(20, 6, MATHS_MOD, 2);
  correct += TestRunner(-13, 5, MATHS_MOD, -3);
  correct += TestRunner(14, -3, MATHS_MOD, 2);
  correct += TestRunner(-15, -4, MATHS_MOD, -3);
  PrintResult(correct, total);
}

U0 SubTests()
{
  I64 correct = 0;
  I64 total = 7;
  "Subtraction - : ";
  correct += TestRunner(-12,16,MATHS_SUB,-28);
  correct += TestRunner(12,-16,MATHS_SUB,28);
  correct += TestRunner(0,16,MATHS_SUB,-16);
  correct += TestRunner(24,16,MATHS_SUB,8);
  correct += TestRunner(-8,-8,MATHS_SUB,0);
  correct += TestRunner(-8,24,MATHS_SUB,-32);
  correct += TestRunner(16,24,MATHS_SUB,-8);
  PrintResult(correct, total);
}

U0 AddTests()
{
  I64 correct = 0;
  I64 total = 5;
  "Addition + : ";
  correct += TestRunner(10, 15, MATHS_ADD, 25);
  correct += TestRunner(-10, 20, MATHS_ADD, 10);
  correct += TestRunner(-5, -5, MATHS_ADD, -10);
  correct += TestRunner(0, 10, MATHS_ADD, 10);
  correct += TestRunner(20, 0, MATHS_ADD, 20);
  PrintResult(correct, total);
}

U0 DivTests()
{
  I64 correct = 0;
  I64 total = 3;
  "Division / : ";
  correct += TestRunner(20, 5, MATHS_DIV, 4);
  correct += TestRunner(-15, 3, MATHS_DIV, -5);
  correct += TestRunner(-24, -4, MATHS_DIV, 6);
  PrintResult(correct,total);
}

U0 ShlTests()
{
  I64 correct = 0;
  I64 total = 3;
  "MATHS_SHL << : ";
  correct += TestRunner(1, 2, MATHS_SHL, 4); // 1 << 2
  correct += TestRunner(3, 3, MATHS_SHL, 24); // 3 << 3
  correct += TestRunner(-2, 1, MATHS_SHL, -4); // -2 << 1
  PrintResult(correct, total);
}

U0 ShrTests()
{
  I64 correct = 0;
  I64 total = 3;
  "MATHS_SHR >> : ";
  correct += TestRunner(4, 1, MATHS_SHR, 2); // 4 >> 1
  correct += TestRunner(16, 3, MATHS_SHR, 2); // 16 >> 3
  correct += TestRunner(-8, 2, MATHS_SHR, -2); // -8 >> 2
  PrintResult(correct, total);
}

U0 XorTests()
{
  I64 correct = 0;
  I64 total = 3;
  "Xor ^: ";
  correct += TestRunner(5, 3, MATHS_XMATHS_OR, 6);
  correct += TestRunner(6, 2, MATHS_XMATHS_OR, 4);
  correct += TestRunner(-1, 1, MATHS_XMATHS_OR, -2);
  PrintResult(correct, total);
}

U0 OrTests()
{
  I64 correct = 0;
  I64 total = 3;
  "Or |: ";
  correct += TestRunner(5, 3, MATHS_OR, 7);
  correct += TestRunner(4, 2, MATHS_OR, 6);
  correct += TestRunner(-1, 1, MATHS_OR, -1);
  PrintResult(correct, total);
}

U0 AndTests()
{
  I64 correct = 0;
  I64 total = 3;
  "And &: ";
  correct += TestRunner(5, 3, MATHS_AND, 1);
  correct += TestRunner(6, 4, MATHS_AND, 4);
  correct += TestRunner(-1, 1, MATHS_AND, 1);
  PrintResult(correct, total);
}

U0 NotTests()
{
  I64 correct = 0;
  I64 total = 3;
  "Not ~: ";
  correct += TestRunner(5, 0, MATHS_NOT, -6); // MATHS_NOT operation usually only takes one operand
  correct += TestRunner(-1, 0, MATHS_NOT, 0);
  correct += TestRunner(0, 0, MATHS_NOT, -1);
  PrintResult(correct, total);
}

I32 Main()
{
 "Test Mathematics:\n";
  SubTests();
  MulTests();
  ModTests();
  AddTests();
  DivTests();
  ShlTests();
  ShrTests();
  XorTests();
  OrTests();
  AndTests();
  NotTests();
  "=====\n";
  return 0;
}
