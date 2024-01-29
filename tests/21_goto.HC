#include "testhelper.HC"

Bool testGoto() {
  for (i64 i = 0; i < 9; ++i) {
    if (i == 4) {
      goto passed;
    }
  } 
  return FALSE;

passed:
  return TRUE;
}

/* test that we can use the same label between functions */
Bool testGoto2() {
  for (i64 i = 0; i < 9; ++i) {
    if (i == 4) {
      goto passed;
    }
  } 
  return FALSE;

passed:
  return TRUE;
}

U0 Main()
{
  i64 correct = 0, test_cases = 2;
  "Testing GOTO: ";
  if (testGoto()) ++correct;
  if (testGoto2()) ++correct;
  PrintResult(correct,test_cases);
  "=====\n";
}
