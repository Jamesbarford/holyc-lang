#include "testhelper.HC"

extern "c" I64 strcmp(U8 *p,U8*p2);

class Example
{
  U0 (*fn)(U8 *name);
};

U8 *Identity(U8 *chars)
{
  return chars;
}

Bool Test2()
{
  I64 correct, tests;
  Example *e = MAlloc(sizeof(Example));
  correct = 0;
  tests = 4;

  e->fn = &Identity;

  U0 (*fn)(U8 *arg);
  U0 (*fn2)(U8 *arg);
  U0 (*fn3)(U8 *arg) = &Identity;
  fn = &Identity;
  fn2 = fn;

  if (!strcmp(fn("fn->hello"), "fn->hello")) correct++;
  if (!strcmp(fn2("fn2->hello"), "fn2->hello")) correct++;
  if (!strcmp(fn3("fn3->hello"), "fn3->hello")) correct++;
  if (!strcmp(e->fn("class->hello"), "class->hello")) correct++;
  Free(e);

  return correct == tests;
}

Bool Test3()
{
  I64 correct, tests;
  Example *e = MAlloc(sizeof(Example));
  U0 (*fn)(U8 *arg);
  U0 (*fn2)(U8 *arg);
  U0 (*fn3)(U8 *arg);

  fn = e->fn = fn2 = fn3 = &Identity;
  correct = 0;
  tests = 4;

  if (!strcmp(fn(" fn->hello"), " fn->hello")) correct++;
  if (!strcmp(fn2(" fn2->hello"), " fn2->hello")) correct++;
  if (!strcmp(fn3(" fn3->hello"), " fn3->hello")) correct++;
  if (!strcmp(e->fn(" class->hello"), " class->hello")) correct++;
  Free(e);

  return correct == tests;
}

Bool Test4()
{ // You need to add a '&'
  U0 (*fn)(U8 *arg) = &Identity;
  if (!strcmp(fn("hey"),"hey")) {
    return TRUE;
  }
  return FALSE;
}

I64 Add(I64 x, I64 y)
{
  return x + y;
}

I64 DoMaths(I64 (*mathfunc)(I64 a, I64 b), I64 x, I64 y)
{
  return mathfunc(x,y);
}

U0 Main()
{
  "Test - Function pointers: ";
  I64 correct = 0, tests = 4;
  I64 i = DoMaths(&Add,10,1);

  if (i == 11) correct++;
  if (Test2()) correct++;
  if (Test3()) correct++;
  if (Test4()) correct++;
  
  PrintResult(correct, tests);
  "====\n";
}
