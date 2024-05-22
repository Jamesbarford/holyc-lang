"Test Scripting: ";
#include "testhelper.HC"

/* While this is possible, it is quite difficult to read */

extern "c" U0 *malloc(U64 size);
extern "c" U0 free(U0 *ptr);
extern "c" I64 strncmp(U8 *s1, U8 *s2, I64 n);

I64 correct = 0;
I64 tests = 4;

class MyClass
{
  I64 i;
};

I64 x = 0;

MyClass *c = malloc(sizeof(MyClass));
c->i = 0;

while (c->i < 10) {
  ++c->i;
}

I64 Func(I64 y = 10) {
  return y;
}

U0 Assign()
{
  c->i = 89;
}

for (I64 i = 0; i < 10; ++i) {
  x += i;
}

if (!strncmp("hello","hello",5)) correct++;
if (strncmp("helo","hello",4) != 0) correct++;

Assign;
I32 Main()
{
  if (x == 45) correct++;
  if (c->i == 89) correct++;
  return 0;
}
Main;
PrintResult(correct,tests);
free(c);
'===\n';
Exit(0);
