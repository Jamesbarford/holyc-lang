#include "testhelper.HC"

/* The address of a function from another image comes out of the GOT: a
 * PC-relative reference does not link into a PIE. A function's address is
 * also a generic pointer, so storing it in a `U0 *` must not warn. */

#define ADDR_TEST_FILE "__80_extern_fn_address.HC"
#define ADDR_TEST_OUT  "__80_extern_fn_address.out"

extern "c" I32 abs(I32 v);

U0 *g_abs = &abs;
U0 *g_later;

Bool IsSet(U0 *p) { return p != NULL; }

Bool CompilesWithoutWarning(U8 *source)
{
  U8 *out;
  Bool ok;
  I64 res;

  if (!FileWrite(ADDR_TEST_FILE, source, StrLen(source), O_CREAT | O_RDWR | O_TRUNC)) {
    return FALSE;
  }
  res = System("../../hcc $HCC_TEST_FLAGS " ADDR_TEST_FILE " > " ADDR_TEST_OUT " 2>&1");
  out = FileRead(ADDR_TEST_OUT);
  ok = res == 0 && out && !strstr(out, "warning");
  if (!ok && out) "%s\n", out;
  if (out) Free(out);
  Rm(ADDR_TEST_FILE);
  Rm(ADDR_TEST_OUT);
  Rm("./a.out");
  return ok;
}

I32 Main()
{
  "Test - extern function address: ";
  I64 correct = 0;
  U0 *f = &abs;
  g_later = &abs;
  if (f != NULL) correct++;
  if (f == g_abs) correct++;
  if (f == g_later) correct++;
  if (IsSet(&abs)) correct++;
  if (CompilesWithoutWarning("extern \"c\" I32 abs(I32 v);\nI32 Main() { U0 *f = &abs; return f == NULL; }\n")) correct++;
  PrintResult(correct, 5);
  "====\n";
  return correct != 5;
}
