#include "testhelper.HC"

U0 FN()
{
  "FN()\n";
}

U8 *PrintMessage(U8 *msg,
                 I64 i,
                 U0 (*fn)(I64 x, I64 y) = NULL,
                 I64 N)
{
  if (fn) fn();
  U8 *buffer = MStrPrint("%s i=%d N=%d\n",msg, i, N);
  return buffer;
}

I32 Main()
{
  "Repro 171 (Default function pointer arguments): ";
  I64 correct = 0;
  I64 total = 1;

  U8 *msg = PrintMessage("hello!",24,,4);

  if (!StrCmp(msg, "hello! i=24 N=4\n")) {
    correct++;
  }
  PrintResult(correct,total);
  Free(msg);

  "====\n";
  return 0;
}
