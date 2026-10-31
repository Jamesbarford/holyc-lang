#include "testhelper.HC"

/* HolyC has no function-like macros. hcc used to crash on one. */

#define MACRO_TEST_FILE "__79_function_macro.HC"
#define MACRO_TEST_OUT  "__79_function_macro.out"

/* Only a `(` touching the name makes a macro function-like */
#define SPACED (1 + 2)
#define TABBED	(3)

#ifdef NEVER_DEFINED
#define SKIPPED(x) x
#endif

U0 Cleanup()
{
  Rm(MACRO_TEST_FILE);
  Rm(MACRO_TEST_OUT);
  Rm("./a.out");
}

Bool Rejected(U8 *source, U8 *printed)
{
  U8 *out;
  Bool ok;
  I64 res;

  Cleanup;
  if (!FileWrite(MACRO_TEST_FILE, source, StrLen(source), O_CREAT | O_RDWR | O_TRUNC)) {
    return FALSE;
  }
  res = System("../../hcc $HCC_TEST_FLAGS " MACRO_TEST_FILE " > " MACRO_TEST_OUT " 2>&1");
  out = FileRead(MACRO_TEST_OUT);
  ok = res != 0 && out && strstr(out, "Function-like macros are not supported");
  /* The error prints the line back. A `#` directive used to re-lex with no
   * text of its own, which dropped it from the line or crashed hcc. */
  ok = ok && strstr(out, printed);
  if (!ok && out) "%s\n", out;
  if (out) Free(out);
  Cleanup;
  return ok;
}

I32 Main()
{
  "Test - function macros rejected: ";
  I64 correct = 0;
  if (Rejected("#define SQ(x) ((x) * (x))\nI32 Main() { return SQ(3); }\n", "#define SQ(x)")) correct++;
  if (Rejected("#define EMPTY() 1\nI32 Main() { return 0; }\n", "#define EMPTY()")) correct++;
  if (Rejected("#define ADD(a, b) a + b\nI32 Main() { return 0; }\n", "#define ADD(a, b)")) correct++;
  if (SPACED == 3) correct++;
  if (TABBED == 3) correct++;
  PrintResult(correct, 5);
  "====\n";
  return correct != 5;
}
