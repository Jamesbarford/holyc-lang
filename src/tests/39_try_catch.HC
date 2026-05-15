#include "testhelper.HC"

/* TempleOS-style try/catch/throw. Each test sets `marker` to a
 * known value and reads `Fs->except_ch` inside the catch handler
 * to verify the thrown u64 made it through unchanged. */

U0 ThrowFromHelper()
{
  throw('Help');
}

U0 ChainedThrowA()
{
  throw('Aaa');
}

U0 ChainedThrowB()
{
  ChainedThrowA();
}

I32 Main()
{
  "Test - try/catch/throw: ";
  I64 correct = 0, tests = 8;

  /* 1. Basic: throw fires, catch runs, value preserved. */
  I64 marker_1 = 0;
  try {
    throw('Foo');
    marker_1 = 0xBAD;          /* unreachable */
  } catch
    marker_1 = Fs->except_ch;
  if (marker_1 == 'Foo') correct++;

  /* 2. No-throw: try body completes, catch skipped. */
  I64 marker_2 = 0;
  try {
    marker_2 = 1;
  } catch
    marker_2 = 0xBAD;
  if (marker_2 == 1) correct++;

  /* 3. Throw escapes a function boundary. */
  I64 marker_3 = 0;
  try {
    ThrowFromHelper();
    marker_3 = 0xBAD;
  } catch
    marker_3 = Fs->except_ch;
  if (marker_3 == 'Help') correct++;

  /* 4. Throw escapes two function levels. */
  I64 marker_4 = 0;
  try {
    ChainedThrowB();
    marker_4 = 0xBAD;
  } catch
    marker_4 = Fs->except_ch;
  if (marker_4 == 'Aaa') correct++;

  /* 5. Nested try: inner catch handles, outer doesn't fire. */
  I64 marker_5_inner = 0, marker_5_outer = 0;
  try {
    try {
      throw('In');
    } catch
      marker_5_inner = Fs->except_ch;
    /* This statement should run after the inner catch completes. */
    marker_5_inner += 1;
  } catch
    marker_5_outer = 0xBAD;
  if (marker_5_inner == 'In' + 1 && marker_5_outer == 0) correct++;

  /* 6. Rethrow from inside catch: inner handler throws a new
   *    value, outer catches it. */
  I64 marker_6_outer = 0;
  try {
    try {
      throw('Inn');
    } catch
      throw('Out');
  } catch
    marker_6_outer = Fs->except_ch;
  if (marker_6_outer == 'Out') correct++;

  /* 7. Sequential try blocks: second try is independent of first. */
  I64 marker_7a = 0, marker_7b = 0;
  try {
    throw('AAA');
  } catch
    marker_7a = Fs->except_ch;
  try {
    throw('BBB');
  } catch
    marker_7b = Fs->except_ch;
  if (marker_7a == 'AAA' && marker_7b == 'BBB') correct++;

  /* 8. Catch handler with a compound statement body, runs more
   *    than a single expression. */
  I64 marker_8 = 0;
  try {
    throw('Big');
  } catch {
    marker_8 = Fs->except_ch;
    marker_8 = marker_8 ^ 0xFF;
  }
  if (marker_8 == ('Big' ^ 0xFF)) correct++;

  PrintResult(correct, tests);
  "=====\n";
  return 0;
}
