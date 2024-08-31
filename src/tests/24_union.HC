#include "testhelper.HC"

class BaseClass
{
  I64 base;
  U8 *base_name;
};

class SuperClass:BaseClass
{
  I64 super;
  U8 *super_name;
};

Bool TestInheritanceOneLevel()
{
  I64 correct = 0, tests = 4;
  SuperClass *s = MAlloc(sizeof(SuperClass));

  s->base = 420;
  s->base_name = "base_class";
  s->super = 69;
  s->super_name = "super_name";

  if (!StrCmp(s->base_name,"base_class")) correct++;
  if (s->base == 420) correct++;
  if (!StrCmp(s->super_name,"super_name")) correct++;
  if (s->super == 69) correct++;

  Free(s);
  return correct == tests;
}

public class Another:SuperClass
{
  I64 another;
  U8 *another_name;
};

Bool TestInheritanceTwoLevels()
{
  I64 correct = 0, tests = 6;
  Another *a = MAlloc(sizeof(Another)); 

  a->base = 420;
  a->base_name = "base_class";
  a->super = 69;
  a->super_name = "super_name";
  a->another = 42;
  a->another_name = "another_name";

  if (a->base == 420) correct++;
  if (!StrCmp(a->base_name,"base_class")) correct++;

  if (a->super == 69) correct++;
  if (!StrCmp(a->super_name,"super_name")) correct++;

  if (a->another == 42) correct++;
  if (!StrCmp(a->another_name,"another_name")) correct++;

  Free(a);
  return correct == tests;
}

class UnionClass 
{
  I64 type;
  union {
    U8 *name;
    Bool is_ok;
  };
};

union Cat
{
  U8 *fish;
  I64 is;
};

Bool TestSimpleUnion()
{
  I64 correct = 0, tests = 2;
  UnionClass *u = MAlloc(sizeof(UnionClass));
  Cat cat;
  u->type = 0;
  u->name = "Foo";

  cat.fish = "food";
  if (!StrCmp(cat.fish,"food")) {
    correct++;
  }

  if (!StrCmp(u->name,"Foo")) {
    correct++;
  }
  Free(u);
  return correct == tests;
}

class UnionClass2
{
  I64 type;
  union {
    class {
      U8 *name;
      I64 age;
    };
    class {
      U8 *car;
      I64 milage;
    };
  };
};

/**
 * This test looks very strange but is how unions work.
 * They allocate enough space for the largest member.
 * As both of the anonymous classes are the same size,
 * accessing a member of one references the other. ie:
 * car == name and age == milage
 */
Bool TestComplexUnion()
{
  I64 correct = 0, tests = 4;
  UnionClass2 *u = MAlloc(sizeof(UnionClass2));
  u->name = "Brian";
  u->age = 46;

  if (!StrCmp(u->name,"Brian") && u->age == 46) correct++;
  /* This is how unions work, despite it looking weird*/
  if (!StrCmp(u->car,"Brian") && u->milage == 46) correct++;

  u->car = "Suzuki";
  u->milage = 17;

  if (!StrCmp(u->car,"Suzuki") && u->milage == 17) correct++;
  if (!StrCmp(u->name,"Suzuki") && u->age == 17) correct++;

  Free(u);
  return correct == tests;
}


I32 Main()
{
  I64 correct = 0, tests = 4;
  "Test: Union and class inheritance: ";
  if (TestInheritanceOneLevel()) correct++;
  if (TestInheritanceTwoLevels()) correct++;
  if (TestSimpleUnion()) correct++;
  if (TestComplexUnion()) correct++;
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
