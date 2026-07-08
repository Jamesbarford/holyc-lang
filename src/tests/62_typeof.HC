"Test - typeof: ";
#include "testhelper.HC"

class TypeofVec2
{
  F64 x;
  F64 y;
};

I64 correct = 0;
I64 tests = 8;

F64 Half(F64 v) { return v / 2.0; }

U0 Main()
{
  I64 i = 5;
  U8 *s = "hi";
  TypeofVec2 v;

  if (!StrCmp(typeof(1+1), "I64")) correct++;
  if (!StrCmp(typeof(1.5), "F64")) correct++;
  if (!StrCmp(typeof(i), "I64")) correct++;
  if (!StrCmp(typeof(&i), "I64 *")) correct++;
  if (!StrCmp(typeof(s), "U8 *")) correct++;
  if (!StrCmp(typeof(v), "TypeofVec2")) correct++;
  if (!StrCmp(typeof(U8*), "U8 *")) correct++;
  /* The operand is type-checked, not evaluated. */
  if (!StrCmp(typeof(Half(4.0)), "F64")) correct++;
}
Main;

PrintResult(correct, tests);
'===\n';
Exit(0);
