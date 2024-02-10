#include "testhelper.HC"

#define _SOME_STRING      ("Hello World")
#define _SOME_INT_MATHS   (10+(2*5+8-1))
#define _SOME_FLOAT_MATHS (0.2+(2.2*5.2+8-1))

#ifdef _SOME_STRING
#define FOO 24
#else
#define FOO 0
#endif

#ifndef FOO_BAR
#define BAR 32
#else 
#define BAR 4
#endif

U0 Main()
{
  I64 correct = 0, tests = 5;
  "Test - Preprocessor: ";

  if (!StrCmp(_SOME_STRING,"Hello World")) correct++;
  if (_SOME_INT_MATHS == 27) correct++;
  if (_SOME_FLOAT_MATHS == 18.64) correct++;
  if (FOO == 24) correct++;
  if (BAR == 32) correct++;
  PrintResult(correct,tests);
  "====\n";
}
