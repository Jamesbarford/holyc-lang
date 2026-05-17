#include "testhelper.HC"

#define ARG_COUNT_TEST_FILE "__39_arg_count_fail.HC"
#define ARG_COUNT_TEST_OUT  "__39_arg_count_fail.out"

U0 CleanupArgCountTest(U0)
{
  Rm(ARG_COUNT_TEST_FILE);
  Rm(ARG_COUNT_TEST_OUT);
  Rm("./a.out");
}

Bool CompileFailsWith(U8 *source, U8 *expected)
{
  U8 *out;
  I64 res;

  CleanupArgCountTest;

  if (!FileWrite(ARG_COUNT_TEST_FILE, source, StrLen(source), O_CREAT | O_RDWR | O_TRUNC)) {
    return FALSE;
  }

  res = System("../../hcc " ARG_COUNT_TEST_FILE " > " ARG_COUNT_TEST_OUT " 2>&1");
  if (res == 0) {
    CleanupArgCountTest;
    return FALSE;
  }

  out = FileRead(ARG_COUNT_TEST_OUT);
  if (!out) {
    CleanupArgCountTest;
    return FALSE;
  }

  if (!strstr(out, expected)) {
    "%s\n", out;
    Free(out);
    CleanupArgCountTest;
    return FALSE;
  }

  Free(out);
  CleanupArgCountTest;
  return TRUE;
}

Bool TestOverArgs(U0)
{
  return CompileFailsWith(
"
U0 DoNothing(I32 a, I32 b) {
  return;
}

U0 Main() {
  DoNothing(1,2,3,4);
}
",
    "Unexpected number of arguments 4 in call to DoNothing(), expected 2 args");
}

Bool TestUnderArgs(U0)
{
  return CompileFailsWith(
"
U0 DoNothing(I32 a, I32 b) {
  return;
}

U0 Main() {
  DoNothing(1);
}
",
    "Unexpected number of arguments 1 in call to DoNothing(), expected 2 args");
}

Bool TestOverDefaultArgs(U0)
{
  return CompileFailsWith(
"
I64 AddS(I64 i1,
         I64 i2=2,
         I64 i3)
{
  return i1+i2+i3;
}

U0 Main()
{
  I64 res = AddS(3,4,5,6);
}
",
    "Unexpected number of arguments 4 in call to AddS(), expected args in range 2 to 3");
}

Bool TestUnderDefaultArgs(U0)
{
  return CompileFailsWith(
"
I64 AddS(I64 i1,
         I64 i2=2,
         I64 i3)
{
  return i1+i2+i3;
}

U0 Main()
{
  I64 res = AddS(3);
}
",
    "Unexpected number of arguments 1 in call to AddS(), argument 3 is required");
}

Bool TestMidDefaultMissingRequiredArg(U0)
{
  return CompileFailsWith(
"
I64 AddS(I64 i1, I64 i2=2, I64 i3)
{
  return i1+i2+i3;
}

U0 Main()
{
  AddS(1,2);
}
",
    "Unexpected number of arguments 2 in call to AddS(), argument 3 is required");
}

Bool TestUnderVarArgs(U0)
{
  return CompileFailsWith(
"
I64 AddS(I64 i1, ...)
{
  return i1;
}

U0 Main()
{
  AddS();
}
",
    "Unexpected number of arguments 0 in call to AddS(), expected 1 args");
}

Bool TestUnderFunctionPointerArgs(U0)
{
  return CompileFailsWith(
"
I64 Add(I64 i1, I64 i2)
{
  return i1+i2;
}

U0 Main()
{
  I64 (*fn)(I64 i1, I64 i2) = &Add;
  fn(1);
}
",
    "Unexpected number of arguments 1 in call to fn(), expected 2 args");
}

I32 Main()
{
  "Test - Argument count errors: \n";
  I64 tests = 7;
  I64 correct = 0;

  if (TestOverArgs()) correct++;
  else "Failed - TestOverArgs()\n";

  if (TestUnderArgs()) correct++;
  else "Failed - TestUnderArgs()\n";

  if (TestOverDefaultArgs()) correct++;
  else "Failed - TestOverDefaultArgs()\n";

  if (TestUnderDefaultArgs()) correct++;
  else "Failed - TestUnderDefaultArgs()\n";

  if (TestMidDefaultMissingRequiredArg()) correct++;
  else "Failed - TestMidDefaultMissingRequiredArg()\n";

  if (TestUnderVarArgs()) correct++;
  else "Failed - TestUnderVarArgs()\n";

  if (TestUnderFunctionPointerArgs()) correct++;
  else "Failed - TestUnderFunctionPointerArgs()\n";

  PrintResult(correct, tests);
  "====\n";
  return 0;
}
