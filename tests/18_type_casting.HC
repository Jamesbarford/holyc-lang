#include "testhelper.HC"

class Ptr
{
  I64 x,y;
};

I64 PtrCast(U0 *ptr)
{ // This mess shows we've casted from 'U0 *' to a 'Ptr *'
  return cast<Ptr *>(ptr)->y == 11;
}

I64 I64ToI8Cast(I64 ll)
{ // down cast
  return cast<I8>(ll) == 'h'; 
}

I64 I64ToU8Cast(I64 ll)
{ // down cast
  return ll(U8) == 0xFF; 
}

I64 U8ToI64(U8 ch)
{ // Upcast
  return cast<I64>(ch) == 99;
}

I64 Varadic(...)
{
  for (I64 i = 0; i < argc; ++i) {
    (argv[i](Ptr *))->x = 10;
  }
}

Bool VaradicTest()
{
  Ptr *p1 = MAlloc(sizeof(Ptr)),
      *p2 = MAlloc(sizeof(Ptr)),
      *p3 = MAlloc(sizeof(Ptr)),
      *p4 = MAlloc(sizeof(Ptr));
  Varadic(p1,p2,p3,p4);
  return p1->x == p2->x == p3->x == p4->x == 10;
}

U0 Main()
{
  "Test - Type casting: ";
  I64 correct = 0, tests = 4;
  Ptr *p = MAlloc(sizeof(Ptr));
  p->x = 10;
  p->y = 11;

  VaradicTest();

  if (PtrCast(p)) ++correct;
  /* hello in hexidecimal */
  if (I64ToI8Cast(0x6F6C6C6568)) ++correct;
  if (U8ToI64(99)) ++correct;
  if (I64ToU8Cast(0xAAFF)) ++correct;
  if (VaradicTest()) ++correct;

  PrintResult(correct,tests);
  "=====\n";
}
