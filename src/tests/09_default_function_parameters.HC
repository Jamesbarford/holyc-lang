#include "testhelper.HC"

I64 Add(I64 a, I64 b=76)
{
  return a + b;
}

I64 Add2(I64 a=10, I64 b=43)
{
  return a + b;
}

I32 Main()
{
  "Test - Default function parameters: ";
  I64 i = Add(20);
  PrintResult(i == 96 && Add2(,43) == 53, 1);
  "====\n";
  return 0;
}
