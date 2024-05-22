#include "testhelper.HC"

I32 Main()
{
  "Test - MemCpy, MemSet: ";
  I64 correct = 0, tests = 4;
  U8 *str1 = MAlloc(10);
  MemCpy(str1,"hello",5);
  str1[6]='\0';

  U8 *str2 = MAlloc(10);
  MemCpy(str2,"zooo",4);
  str2[5]='\0';

  if (!StrNCmp(str1,"hello",5)) ++correct;
  if (!StrNCmp(str2,"zooo",4)) ++correct;
  if (StrNCmp(str1, str2, 4) != 0) ++correct;

  MemSet(str1,'z',8);
  MemCpy(str2,str1,8);

  str1[9] = str2[9] = '\0';
  if (!StrNCmp(str1, str2, 4) != 0) ++correct;

  Free(str1);
  Free(str2);

  PrintResult(correct,tests);
  return 0;
}
