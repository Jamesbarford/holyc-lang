#include "testhelper.HC"

/**
 * Type casting can be a postfix cast of a functional style `cast<TYPE>`.
 * the result is the same.
 */

class Foo
{
  I32 x;
};

class Ptr
{
  I64 x,y;
  I64 (*fn)(I64 x);
  U0 *priv;
};

I64 Add2(I64 x)
{
  return x + 2;
}

I64 PtrCast(U0 *ptr)
{ // This shows we've casted from 'U0 *' to a 'Ptr *'
  return ptr(Ptr *)->y == 11;
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
    argv[i](Ptr *)->x = 10;
  }
}

Bool VaradicTest()
{
  Ptr *p1 = MAlloc(sizeof(Ptr)),
      *p2 = MAlloc(sizeof(Ptr)),
      *p3 = MAlloc(sizeof(Ptr)),
      *p4 = MAlloc(sizeof(Ptr));
  Varadic(p1,p2,p3,p4);
  return p1->x == 10 &&
         p2->x == 10 &&
         p3->x == 10 && 
         p4->x == 10;
}

Bool CallingAFunctionPointer(U0 *ptr)
{ // This is the most amount of casting possible to do
  ptr(Ptr *)->fn = &Add2;
  return ptr(Ptr *)->fn(5)(I32) == 7;
}

Bool CallingAFunctionPointerPart2(U0 *_ptr)
{
  Ptr *ptr = _ptr(Ptr *);
  ptr->fn = &Add2;
  return ptr->fn(5)(I32) == 7;
}

Bool CastingClassMember(Ptr *p)
{
  Foo *foo = p->priv(Foo *);
  return foo->x == 32;
}

Bool CastingArray()
{
  U64 arr[3] = {1,2,3};
  return arr[0](I32) == 1 && 
         arr[1](I16) == 2 && 
         arr[2](I8) == 3;
}

I32 Main()
{
  "Test - Type casting:\n";
  I64 correct = 0, tests = 9;
  Ptr *p = MAlloc(sizeof(Ptr));
  p->x = 10;
  p->y = 11;

  if (PtrCast(p)) ++correct;
  /* hello in hexidecimal */
  if (I64ToI8Cast(0x6F6C6C6568)) ++correct;
  if (U8ToI64(99)) ++correct;
  if (I64ToU8Cast(0xAAFF)) ++correct;
  if (VaradicTest()) ++correct;
  if (CallingAFunctionPointer(p)) ++correct;
  if (CallingAFunctionPointerPart2(p)) ++correct;
  if (CastingArray()) ++correct;
  Foo *foo = MAlloc(sizeof(Foo));
  foo->x = 32;
  p->priv = foo;
  if (CastingClassMember(p)) ++correct;
  Free(foo);

  PrintResult(correct,tests);
  "=====\n";
  return 0;
}
