#include "testhelper.HC"

Bool TestBreakInLoopCrazy()
{
  I64 i,j,k;
  switch (1) {
    case 1:
      for (i = 0; i < 10; ++i) {
        for (j = 0; j < 10; ++j) {
          for (k = 0; k < 10; ++k) {
            if (k == 5) break;
          }
          if (j == 6) break;
        }
        if (i == 7) break;
      }
      break;
    case 2:  return FALSE;
    default: return FALSE;
  }

  return i == 7 && 
         j == 6 && 
         k == 5;
}

Bool TestBreakInLoop()
{
  I64 i = 0;
  switch (1) {
    case 1:
      for (; i < 10; ++i) {
        if (i == 5) {
          break; // This break should be part of the loop
        }
      }
      break;
    case 2:  return FALSE;
    default: return FALSE;
  }
  return i == 5;
}

Bool UnNamedCase()
{
  I32 num = 0;
  for (I32 i = 0; i <= 5; ++i) {
    switch (i) {
      case: num++; break; //0
      case: num++; break; //1
      case 3: num++; break; //3
      case: num++; break; //4
      case: num++; break; //5
    }
  }
  return num == 5;
}

Bool TestOutOfRange()
{
  I64 num = 2000;
  I64 is_set = 0;

  switch (num) {
    case 10: is_set = -1; break;
    case 20: is_set = -1; break;
    case 30: is_set = -1; break;
    default: is_set = 1; break;
  }

  return is_set == 1;
}

Bool TestNoBoundsSwitch()
{
  I64 num = 0;
  for (I64 i = 0; i < 5; ++i) {
    switch [i] {
      case:   num++; break;
      case 1: num++; break;
      case 2: num++; break;
      case 4: num++; break;
      default: break;
    }
  }

  return num == 4;
}

I32 Main(I32 argc, U8 **argv)
{
  "Test - Switch statement: ";
  I64 correct = 0, tests = 7, count = 0;
  I64 i = 0;

  for (; i < 10; ++i) {
    switch (i) {
      case 1:  count++; break;
      case 2:  count++; break;
      case 3:  count++; break;
      case 7:  count++; break;
      case 8:  count++; break;
      case 10: count++; break;
      case 9:  count++; break;
      default: count--; break;
    }
  }
  if (count == 2) {
    correct++;
  }

  count ^= count;
  U8 *name = "hello", *ptr = name;
  while (*ptr) {
    switch (*ptr) {
      case 'h': count++; break;
      case 'e': count++; break;
      case 'l': count++; break;
      case 'o': count++; break;
      case 'z': count++; break;
      default:  count--; break;
    }
    ++ptr;
  }

  if (count == 5) {
    correct++;
  }

  if (TestBreakInLoop())      correct++;
  if (TestBreakInLoopCrazy()) correct++;
  if (UnNamedCase())          correct++;
  if (TestNoBoundsSwitch())   correct++;
  if (TestOutOfRange())       correct++;

  PrintResult(correct,tests);
  return 0;
}
