#include "testhelper.HC"

class Class1
{
  I64 val;
};

class Class2
{
  Class1 *c1;
  I64 val2;
};

class Student
{
  I64 id, age;
  U8 name[8];
};

class SuperStudent
{
  Student *s;
  I64 id;
};

I32 Main()
{
  "Test - class definition: ";
  I64 correct = 0, tests = 6;
  Class2 *c2 = MAlloc(sizeof(Class2));
  c2->val2 = 10;
  
  if (c2->val2 == 10) {
    ++correct;
  }

  SuperStudent *spr = MAlloc(sizeof(SuperStudent));
  
  Student *s = MAlloc(sizeof(Student));
  s->id = 420;
  s->name[0] = 'J';
  s->name[1] = 'a';
  s->name[2] = 'm';
  s->name[3] = 'e';
  s->name[4] = 's';
  s->name[5] = '\0';
  s->age = 31;

  if (s->id == 420) ++correct;
  if (s->age == 31) ++correct;
  if (!StrNCmp(s->name,"James",5)) ++correct;

  spr->s = MAlloc(sizeof(Student));
  spr->id = 420;
  spr->s->id = 419;
  spr->s->name[0] = 'W';
  spr->s->name[1] = 'i';
  spr->s->name[2] = 'l';
  spr->s->age = 40;
  spr->s->name[3] = '\0';
  ++spr->s->age;

  if (!StrNCmp(spr->s->name,"Wil",3)) ++correct;
  if (spr->s->age == 41) ++correct;

  PrintResult(correct,tests);
  Free(c2);
  Free(s);
  "====\n";
  return 0;
}
