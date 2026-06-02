#include "testhelper.HC"

/* File-scope variables with initialisers are run by a synthetic entry
 * point that must also call the user's Main. The fact that Main runs at
 * all (so this file produces output) is itself the core regression
 * guard - before the fix the program initialised the globals and exited
 * without ever entering Main. */

I64 g_int   = 42;
I64 g_neg   = -7;
I64 g_expr  = 3 + 4 * 2;     /* 11 */
F64 g_f64   = 1.5;
F32 g_f32   = 2.25;
U8  g_char  = 'A';
U8 *g_str   = "holyc";
auto g_auto_i = 99;          /* inferred I64 */
auto g_auto_f = 3.5;         /* inferred F64 */

Bool GlobalScalars()
{
  I64 tests = 7, correct = 0;
  if (g_int == 42) correct++;
  if (g_neg == -7) correct++;
  if (g_expr == 11) correct++;
  if (g_char == 'A') correct++;
  if (g_str[0] == 'h' && g_str[4] == 'c') correct++;
  if (g_auto_i == 99) correct++;
  if (g_auto_f == 3.5) correct++;
  return correct == tests;
}

Bool GlobalFloats()
{
  I64 tests = 3, correct = 0;
  if (g_f64 == 1.5) correct++;
  if (g_f32 == 2.25) correct++;
  if (g_f64 + g_f32 == 3.75) correct++;
  return correct == tests;
}

Bool GlobalMutation()
{
  I64 tests = 2, correct = 0;
  /* Globals are writable after their initialiser runs. */
  g_int = g_int + 8;
  if (g_int == 50) correct++;
  g_f32 = g_f32 * 2.0;
  if (g_f32 == 4.5) correct++;
  return correct == tests;
}

I32 Main()
{
  "Test - Global initialisers:\n";
  I64 tests = 3, correct = 0;
  if (GlobalScalars()) correct++;
  if (GlobalFloats()) correct++;
  if (GlobalMutation()) correct++;
  PrintResult(correct,tests);
  "====\n";
  return 0;
}
