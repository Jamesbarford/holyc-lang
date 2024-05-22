#include "testhelper.HC"

class Person
{
  U8 name[10];
  I64 age;
};

auto PersonNew(U8 name[10], I64 age)
{
  Person *p = MAlloc(sizeof(Person));
  p->age = age;
  MemCpy(p->name,name,sizeof(p->name));
  return p;
}

auto CreateRandom()
{
  return PersonNew("PersonA", 42);
}

auto Add(I64 i, I64 x)
{
  return i + x;
}

auto Identity(I64 i)
{
  return i;
}

Bool VariableAutos()
{
  auto correct = 0, tests = 5;
  auto x = 1<<2,
       f = 4.5,
       ch = 'hello', 
       p = cast<Person *>(MAlloc(sizeof(Person)));
  if (x + 4 == 8)    correct++;
  if (f + 4.5 == 9)  correct++;
  if (ch == 'hello') correct++;
  p->age = 42;
  MemCpy(p->name,"PersonB",7);
  if (p->age == 42) correct++;
  if (!StrNCmp(p->name,"PersonB",7)) correct++;
  return correct == tests;
}

Bool FunctionCalls()
{
  I64 correct = 0, tests = 3;
  auto p = CreateRandom();
  if (p->age == 42 && !StrNCmp(p->name,"PersonA",7)) correct++;
  if (Identity(10) == 10) correct++;
  if (Add(420,36) == 456) correct++;
  return correct == tests;
}

I32 Main()
{
  "Test - auto keyword: ";
  I64 correct = 0, tests = 2;
  if (VariableAutos()) correct++;
  if (FunctionCalls()) correct++;
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
