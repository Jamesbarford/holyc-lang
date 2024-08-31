#include "testhelper.HC"

I32 Main()
{
  "Test - comma separated declarators & types: ";
  I64 correct = 0, tests = 3;
  U8 *ptr, *ptr2 = "hey", ch;
  ptr = "hello";
  ch = 'f';

  if (!StrNCmp(ptr,"hello",5)) ++correct;
  if (!StrNCmp(ptr2,"hey",3)) ++correct;
  if (ch == 'f') ++correct;

  PrintResult(correct,tests);
  "====\n";
  return 0;
}
