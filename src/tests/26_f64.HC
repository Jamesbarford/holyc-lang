#include "testhelper.HC"

Bool FloatAddition()
{
  I64 tests = 6, correct = 0;
  F64 f1 = .0000001 + .0000002;
  if (f1 == 0.0000003) correct++;


  F64 f2 = 12345678.9 + 0.1111111;
  if (f2 == 12345679.011111101) correct++;
  
  F64 f3 = -12345.6789 + 12345.6788;
  /* floats are really weird */
  if (f3 == -0.00010000000111176632) correct++;

  F64 f4 = 1.0000000000000001 + 1.0000000000000001;
  if (f4 == 2.0000000000000002) correct++;

  F64 f5 = 0.3333333333333333 + 0.6666666666666666;
  if (f5 == 1) correct++;

  F64 f6 = 9.876543210123456 + 0.000000000000001;
  if (f6 == 9.876543210123458) correct++;

  return correct == tests;
}

Bool FloatSubraction()
{
  I64 tests = 6, correct = 0;
  F64 f1 = 0.0000002 - 0.0000001;
  if (f1 ==  0.0000001) correct++;

  F64 f2 = 12345678.9 - 0.1111111;
  if (f2 == 12345678.7888889) correct++;

  F64 f3 = (-12345.6789) - (12345.6788);
  if (f3 == -24691.3577) correct++;


  F64 f4 = 1.000000000000001 - 0.000000000000001;
  if (f4 == 1.0) correct++;

  F64 f5 = 0.3333333333333333 - 0.3333333333333333;
  if (f5 == 0) correct++;

  F64 f6 = 9.8765432101234567 - 0.8765432101234567;
  if (f6 == 9) correct++;
  return correct == tests;
}

Bool FloatDivision()
{
  I64 tests = 8, correct = 0;

  F64 f1 = 0.0000003 / 0.0000001;
  if (f1 == 3) correct++;

  F64 f2 = 12345678.9 / 1000000;
  if (f2 == 12.345678900000001) correct++;

  F64 f3 = -12345.6789 / 1;
  if (f3 == -12345.6789) correct++;

  F64 f4 = 1.0000000000000001 / 1.0000000000000001;
  if (f4 == 1) correct++;

  F64 f5 = 0.3333333333333333 / 0.3333333333333333;
  if (f5 == 1) correct++;

  if (23/9 == 2.5555555555555554)correct++;

  if (24/10 < 50) correct++;

  F64 f6 = 9.876543210123456 / 9.876543210123456;
  if (f6 == 1) correct++;
  return correct == tests;
}

Bool FloatMultiplication()
{
  I64 tests = 6, correct = 0;
  F64 f1 = 0.0000001 * 10000000;
  if (f1 == 1) correct++;

  F64 f2 = 12345.6789 * 0.0001;
  if (f2 == 1.2345678900000001) correct++;

  F64 f3 = -12345.6789 * -1;
  if (f3 == 12345.6789) correct++;

  F64 f4 = 1.0000000000000001 * 1.0000000000000001;
  if (f4 == 1.0) correct++;

  F64 f5 = 0.3333333333333333 * 3;
  if (f5 == 1.0) correct++;

  F64 f6 = 9.876543210123456 * 1.234567890123456;
  if (f6 == 12.193263112635261) correct++;
  return correct == tests;
}

Bool FloatOperators()
{
  I64 tests = 9, correct = 0;

  if (45.5*8  > 32.123) correct++;
  if (89.1231 == 89.1231) correct++;
  if (420.1   != 89.1231) correct++;
  if (1.43234 < 89.1231) correct++;
  if (1.43234 <= 1.43234) correct++;
  if (1.43234 <= 1.43234) correct++;
  if (1.43234 == 1.43234) correct++;
  if (1.43234 >= 1.43230) correct++;
  if (1.43234 >= 1.43234) correct++;
  return tests == correct;
}

I32 Main()
{
  "Test - Floating point arithmetic:\n";
  I64 tests = 5, correct = 0;
  if (FloatAddition()) correct++;
  if (FloatSubraction()) correct++;
  if (FloatDivision()) correct++;
  if (FloatMultiplication()) correct++;
  if (FloatOperators()) correct++;
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
