#include "testhelper.HC"

I64 Add(...)
{
  I64 acc = 0, arg;
  for (I64 i = 0; i < argc; ++i) {
    arg = cast<I64>(argv[i]);
    acc += argv[i];
  }
  return acc;
}

I64 Arg2(I64 i1, ...)
{
  I64 arg,acc = 0;
  for (I64 i = 0; i < argc; ++i) {
    arg = cast<I64>(argv[i]);
    acc += arg;
  }
  return i1+acc;
}

I64 Arg3(I64 i1, I64 i2, ...)
{
  I64 arg,acc;
  acc = 0;
  for (I64 i = 0; i < argc; ++i) {
    arg = cast<I64>(argv[i]);
    acc += argv[i];
  }
  return i1+i2+acc;
}

F64 FAdd(F64 something, ...)
{
  F64 arg,acc;
  acc = 0.0;
  for (I64 i = 0; i < argc; ++i) {
    arg = argv[i](F64);
    acc += arg;
  }
  return acc+something;
}

U8 buf[512];
U8 *Concat(...)
{
  U8 *arg;
  I64 len;
  len = 0;
  for (I64 i = 0; i < argc; ++i) {
    arg = cast<U8*>(argv[i]);
    for (U8 *ptr = arg;*ptr;++ptr) {
      buf[len++] = *ptr;
    }
    buf[len++] = ' ';
    buf[len] = '\0';
  }
  return buf;
}

I64 AddS(I64 i1, I64 i2, I64 i3, I64 i4, I64 i5, I64 i6)
{
  return i1+i2+i3+i4+i5+i6;
}

I64 Wrap(I64 i)
{// simulate a nested function call
  return i;
}

I32 Main()
{
  "Test - Variable arguments: ";
  I64 res, correct=0, tests = 15;
  if (!StrNCmp("foo bar baz", Concat("foo", "bar", "baz"), 11)) {
    correct++;
  }

  if (FAdd(2.08,4.0,6.82) == 12.9) correct++;
  if (Arg2(5,6,7,1) == 19) correct++;
  // needs to work both in a function call and variable assignment
  res = Arg3(5,6,7,1); 
  if (res == 19) correct++;

  if (AddS(2,2,2,4,4,0) == 14) correct++;
  res = AddS(2,2,2,4,4,0);
  if (res == 14) correct++;

  /* assign and check */
  res = Add(2);
  if (res == 2) correct++;

  res = Add(2,2);
  if (res == 4) correct++;

  res = Add(2,2,2);
  if (res == 6) correct++;

  res = Add(2,2,2,4);
  if (res == 10) correct++;

  res = Add(2,2,2,4,4);
  if (res == 14) correct++;
  
  res = Add(2,2,2,4,4,4);
  if (res == 18) correct++;

  res = Add(2,2,2,4,4,4,3);
  if (res == 21) correct++;

  res = Wrap(Add(2,2,2,4,4,4,4,3,3));
  if (res == 28) correct++;

  res = Wrap(Wrap(Add(2,2,2,4,4,4,4,3,3,5)));
  if (res == 33) correct++;


  PrintResult(correct,tests);
  "=====\n";
  return 0;
}
