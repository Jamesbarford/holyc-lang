#include "testhelper.HC"

I64 global_int;

U8 *TestStaticSetup(U8 *str)
{
  static U8 buffer[128];
  snprintf(buffer,sizeof(buffer),str);
  return buffer;
}

Bool TestStatic()
{
  U8 *str = TestStaticSetup("hello");
  if (!StrNCmp(str,"hello",5)) {
    return TRUE;
  }
  return FALSE;
}

I64 TestStaticIntSetup(I64 value)
{
  static I64 static_int = 32;
  if (value > 42) {
    static_int = value;
  }
  return static_int;
}

Bool TestStaticInt()
{
  I64 tests = 3, correct = 0;
  I64 value = TestStaticIntSetup(16);
  if (value == 32) {
    correct++;
  }
  
  value = TestStaticIntSetup(420);
  if (value == 420) {
    correct++;
  }
  
  value = TestStaticIntSetup(32);
  if (value == 420) {
    correct++;
  }

  return correct == tests;
}

U0 TestGlobalSetup(I64 value)
{
  global_int = value;
}

Bool TestGlobal()
{
  TestGlobalSetup(69);
  if (global_int == 69) {
    return TRUE;
  }
  return FALSE;
}

I32 Main()
{
  "Test - local static variables and global variables: \n";
  I64 tests = 3, correct = 0;
  if (TestStatic())    correct++;
  if (TestGlobal())    correct++;
  if (TestStaticInt()) correct++;
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
