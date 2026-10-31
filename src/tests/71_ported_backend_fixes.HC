#include "testhelper.HC"

/* Backend and lowering fixes shared with a sibling compiler.
 * Each function is a shape that once miscompiled or crashed.
 *
 * Deliberately NOT here: HolyC does its arithmetic in 64 bits, so
 * `U8 + U8` is 260 rather than wrapping, unlike C. */

extern "c" I32 strcmp(U8 *a, U8 *b);

/* `continue` inside a `switch` inside a loop belongs to the loop. The
 * switch's context has no continue target, and jumping to it panicked. */
Bool TestContinueInSwitch()
{
  I64 i, n = 0;
  for (i = 0; i < 5; ++i) {
    switch (i) {
      case 2: continue;
      default: n += 1; break;
    }
    n += 10;
  }
  return n == 44;
}

/* Same width, opposite signedness: the cast has to re-extend. */
Bool TestSignChangingCast()
{
  I8 c = -95;
  I64 a = c(U8);
  U8 u = 200;
  I64 b = u(I8);
  U16 s = 65535;
  I64 d = s(I16);
  return a == 161 && b == -56 && d == -1;
}

/* A string initialising an array keeps its bytes inline; storing the
 * 8-byte pointer ran off the end of anything smaller. */
Bool TestStringIntoArray()
{
  I64 guard = 0x1122334455667788;
  U8 small[6] = "hello";
  U8 sized[] = "abc";
  return !strcmp(small, "hello") && !strcmp(sized, "abc") &&
         sizeof(sized) == 4 && guard == 0x1122334455667788;
}

/* A `return` out of a `try` must pop its catch frame, or the next throw
 * longjmps into a frame that no longer exists. */
I64 ReturnFromTry()
{
  try {
    return 1;
  } catch {
    return -1;
  }
  return 2;
}

U0 ThrowAfterReturnFromTry()
{
  ReturnFromTry;
  throw(5);
}

I64 BreakOutOfTry()
{
  I64 i;
  for (i = 0; i < 3; ++i) {
    try {
      if (i == 1) break;
    } catch {
      return -1;
    }
  }
  return i;
}

Bool TestLeaveTryEarly()
{
  I64 caught = 0;
  try {
    ThrowAfterReturnFromTry;
  } catch {
    caught = 1;
  }
  return caught && BreakOutOfTry() == 1;
}

/* Elements that are whole classes are copied, not pointed at. */
class Triple
{
  I64 x;
  I64 y;
  I64 z;
};

Bool TestArrayOfClasses()
{
  Triple a, b;
  a.x = 1; a.y = 2; a.z = 3;
  b.x = 4; b.y = 5; b.z = 6;
  Triple arr[2] = {a, b};
  return arr[0].x == 1 && arr[0].z == 3 && arr[1].y == 5 && arr[1].z == 6;
}

/* The target of a compound assignment is evaluated once. */
I64 g_next_calls = 0;
Triple g_slots[3];

Triple *NextSlot()
{
  g_next_calls++;
  return &g_slots[g_next_calls - 1];
}

Bool TestCompoundAssignOnce()
{
  g_next_calls = 0;
  g_slots[0].x = 2;
  NextSlot()->x += 5;
  return g_next_calls == 1 && g_slots[0].x == 7;
}

/* A variadic function that returns a class: the two call paths have to
 * agree that the tail goes on the stack. */
Triple VariadicTriple(I64 x, ...)
{
  Triple t;
  t.x = x;
  t.y = argc;
  t.z = argv[1];
  return t;
}

Bool TestVariadicClassReturn()
{
  Triple t = VariadicTriple(1, 2, 3);
  return t.x == 1 && t.y == 2 && t.z == 3;
}

/* Reached through a pointer the callee is still variadic. */
I64 SumVariadic(...)
{
  I64 sum = 0;
  for (I64 i = 0; i < argc; ++i) sum += argv[i];
  return sum;
}

Bool TestIndirectVariadic()
{
  I64 (*fn)(...);
  fn = &SumVariadic;
  return fn(1, 2, 3) == 6;
}

/* Four doubles are 32 bytes, and still an HFA: back in d0-d3 on AAPCS64,
 * in memory on SysV. Either way the caller and callee have to agree. */
class Rect
{
  F64 x;
  F64 y;
  F64 w;
  F64 h;
};

Rect MakeRect(F64 x, F64 y, F64 w, F64 h)
{
  Rect r;
  r.x = x; r.y = y; r.w = w; r.h = h;
  return r;
}

F64 RectArea(Rect r) { return r.w * r.h; }

Bool TestWideHfaReturn()
{
  Rect r = MakeRect(1.5, 2.5, 3.0, 4.0);
  return r.x == 1.5 && r.y == 2.5 && RectArea(r) == 12.0;
}

/* Overflowing the argument registers: stacked floats must not bounce
 * through a register that already holds an earlier argument. */
F64 TenFloats(F64 a, F64 b, F64 c, F64 d, F64 e, F64 f, F64 g, F64 h, F64 i,
              F64 j)
{
  return a + b*2 + c*3 + d*4 + e*5 + f*6 + g*7 + h*8 + i*9 + j*10;
}

I64 TenInts(I64 a, I64 b, I64 c, I64 d, I64 e, I64 f, I64 g, I64 h, I64 i,
            I64 j)
{
  return a + b*2 + c*3 + d*4 + e*5 + f*6 + g*7 + h*8 + i*9 + j*10;
}

/* Calls nested in the arguments of a call with stack arguments: the
 * outgoing area is shared, so the inner call must not trample it. */
Bool TestArgumentOverflow()
{
  return TenFloats(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0) == 55.0 &&
         TenInts(1, 1, 1, 1, 1, 1, 1, 1, 1, 1) == 55 &&
         TenInts(1, 1, 1, 1, 1, 1, 1, 1,
                 TenInts(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 2) == 551;
}

/* A slot that only ever holds one function: `-O` calls it directly. */
I64 Twice(I64 x) { return x * 2; }

Bool TestKnownIndirectCall()
{
  I64 (*fn)(I64);
  fn = &Twice;
  I64 sum = 0;
  for (I64 i = 0; i < 4; ++i) sum += fn(i);
  return sum == 12;
}

/* Narrow locals stay canonical for their own type in a register. */
Bool TestNarrowRegisterHomes()
{
  U8 u = 250;
  I8 s = -3;
  U16 h = 65000;
  I32 w = -100000;
  u += 10;
  s -= 1;
  h += 1000;
  w -= 1;
  return u == 4 && s == -4 && h == 464 && w == -100001;
}

I32 Main()
{
  "Test - backend fixes ported from a sibling compiler: ";
  I64 correct = 0, tests = 13;
  correct += TestContinueInSwitch();
  correct += TestSignChangingCast();
  correct += TestStringIntoArray();
  correct += TestLeaveTryEarly();
  correct += TestArrayOfClasses();
  correct += TestCompoundAssignOnce();
  correct += TestVariadicClassReturn();
  correct += TestIndirectVariadic();
  correct += TestWideHfaReturn();
  correct += TestArgumentOverflow();
  correct += TestKnownIndirectCall();
  correct += TestNarrowRegisterHomes();
  correct += strcmp("a", "b") < 0;
  PrintResult(correct, tests);
  "====\n";
  return correct != tests;
}
