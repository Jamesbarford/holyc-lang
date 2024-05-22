#include "testhelper.HC"

I32 Main()
{
  "Test - Taking the address of a local: ";
  I64 i = 420;
  I64 *x = &i;
  *x = 69;

  PrintResult(*x == 69, 1);
  return 0;
}
