#include "./testhelper.HC"

#define TEST_CAPACITY 32

class Container
{
  I64 *entries;
  I64 size;
};

Bool TestContainerLoop(U0)
{
  Container *c = MAlloc(sizeof(Container));
  c->entries = MAlloc(sizeof(I64) * TEST_CAPACITY);
  c->entries[c->size++] = 11;
  c->entries[c->size++] = 22;
  c->entries[c->size++] = 32;
  c->entries[c->size++] = 43;
  I64 cases[] = {11,22,32,43};
  I64 i = 0;

  for (auto it : c) {
    if (cases[i] != it) {
      break;
    }
    i++;
  }

  Free(c->entries);
  Free(c);
  return i == sizeof(cases)/sizeof(cases[0]);
}

class StaticContainer
{
  I64 entries[8];
  I64 size;
};

Bool TestStaticContainerEntriesLoop(U0)
{
  StaticContainer *c = MAlloc(sizeof(StaticContainer));

  c->entries[c->size++] = 91;
  c->entries[c->size++] = 79;
  c->entries[c->size++] = 33;
  c->entries[c->size++] = 47;
  I64 cases[] = {91,79,33,47};
  I64 i = 0;

  for (auto it : c) {
    if (cases[i] != it) {
      break;
    }
    i++;
  }

  Free(c);
  return i == sizeof(cases)/sizeof(cases[0]);
}

class StructWithArray
{
  I64 entries[8];
};

Bool TestStructWithArrayLoop(U0)
{
  StructWithArray *c = MAlloc(sizeof(StructWithArray));
  I64 size = 0;
  c->entries[size++] = 91;
  c->entries[size++] = 79;
  c->entries[size++] = 33;
  c->entries[size++] = 47;

  c->entries[size++] = 102;
  c->entries[size++] = 14;
  c->entries[size++] = 90;
  c->entries[size++] = 77;
  I64 cases[] = {91,79,33,47,102,14,90,77};
  I64 i = 0;

  /* Can range over arrays in structs */
  for (auto it : c->entries) {
    if (cases[i] != it) {
      break;
    }
    i++;
  }

  Free(c);
  return i == sizeof(cases)/sizeof(cases[0]);
}

Bool TestFloatVecLoop(U0)
{
  FloatVec *fvec = FloatVecNew();

  F64 cases[] = {8.8, 4.2, 3.8};
  I64 i = 0;
  FloatVecPush(fvec,8.8);
  FloatVecPush(fvec,4.2);
  FloatVecPush(fvec,3.8);

  for (auto it : fvec) {
    if (cases[i] != it) {
      break;
    }
    i++;
  }

  FloatVecRelease(fvec);
  return i == sizeof(cases)/sizeof(cases[0]);
}

class BoxInt
{
  I64 i;
};

BoxInt *BInt(I64 i)
{
  BoxInt *b = MAlloc(sizeof(BoxInt));
  b->i = i;
  return b;
}

Bool TestPtrVecLoop(U0)
{
  PtrVec *vec = PtrVecNew();
  PtrVecPush(vec,BInt(1));
  PtrVecPush(vec,BInt(4));
  PtrVecPush(vec,BInt(8));
  PtrVecPush(vec,BInt(9));
  I64 cases[] = {1,4,8,9};
  I64 i = 0;

  for (BoxInt *b : vec) {
    if (cases[i] != b->i) {
      break;
    }
    i++;
    Free(b);
  }
  return i == sizeof(cases)/sizeof(cases[0]);
}

Bool TestArrayLoop(U0)
{
  F64 arr[]   = {9.2, 8.1, 7.8, 5.6, 3.33, 1.12313};
  F64 cases[] = {9.2, 8.1, 7.8, 5.6, 3.33, 1.12313};
  I64 i = 0;

  for (auto it : arr) {
    if (it != cases[i]) {
      break;
    }
    i++;
  }

  return i == sizeof(cases)/sizeof(cases[0]);
}

class C2
{
  IntVec *v;
};

class C1
{
  I32 padding;
  C2 *c2;
};

Bool TestNested(U0)
{
  C1 c1;
  c1.c2 = MAlloc(sizeof(C2));
  c1.c2->v = IntVecNew();

  IntVecPush(c1.c2->v,10);
  IntVecPush(c1.c2->v,100);
  IntVecPush(c1.c2->v,101);
  IntVecPush(c1.c2->v,1011);

  I64 cases[] = {10,100,101,1011};
  I64 i = 0;
  
  for (auto it : c1.c2->v) {
    if (cases[i] != it) {
      break;
    }
    i++;
  }

  IntVecRelease(c1.c2->v);
  Free(c1.c2);
  return i == sizeof(cases)/sizeof(cases[0]);
}

I32 Main(U0)
{
  "Test - Range loop: \n";
  I64 correct = 0, tests = 7;
  if (TestContainerLoop()) correct++;
  if (TestStaticContainerEntriesLoop()) correct++;
  if (TestStructWithArrayLoop()) correct++;
  if (TestFloatVecLoop()) correct++;
  if (TestPtrVecLoop()) correct++;
  if (TestArrayLoop()) correct++;
  if (TestNested()) correct++;
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
