#include "testhelper.HC"

/* A constant initialiser is static data, not a store in the entry point:
 * a unit holding one must not gain a `main`. Whatever cannot be folded
 * exactly as it would have run stays an assignment. */

#define INIT_TEST_LIB  "__81_global_init_lib"
#define INIT_TEST_MAIN "__81_global_init_main"

I64 g_int = 334;
I64 g_neg = -5;
I32 g_i32 = -7;
U8  g_u8 = 300;
F64 g_f64 = 1.5;
F64 g_fneg = -2.5;
F64 g_from_int = 3;
U8 *g_null = NULL;
auto g_auto = 9;
I64 g_expr = (1 << 4) | 3;
I64 g_first = 1, *g_ptr = NULL, g_third = 3;

static I64 s_int = 11, s_second = 22, s_zero;
static F64 s_f64 = 0.25;
static U8 *s_str = "static";

/* Must keep the meaning they have at runtime */
F64 g_int_div = 3 / 2;
I64 g_truncated = 1.5;
I64 g_float_expr = 1.5 + 1.5;
I64 g_from_global = g_int + 1;

/* The first declaration owns the storage */
I64 g_tentative;
I64 g_tentative = 5;
I64 g_redeclared = 1;
I64 g_redeclared = 2;

I64 Bump()
{
  g_int++;
  return g_int;
}

Bool WriteUnit(U8 *path, U8 *source)
{
  return FileWrite(path, source, StrLen(source), O_CREAT | O_RDWR | O_TRUNC);
}

/* Two units, each with an initialised global: a `main` in each cannot link */
Bool LinksTwoUnits()
{
  Bool ok;
  if (!WriteUnit(INIT_TEST_LIB ".HC",
        "I64 lib_value = 7;\n"
        "static I64 lib_hidden = 35;\n"
        "I64 LibValue() { return lib_value + lib_hidden; }\n")) {
    return FALSE;
  }
  if (!WriteUnit(INIT_TEST_MAIN ".HC",
        "I64 LibValue();\n"
        "I64 main_value = 0;\n"
        "I32 Main() { return LibValue() + main_value != 42; }\n")) {
    return FALSE;
  }
  ok = System("../../hcc $HCC_TEST_FLAGS -c " INIT_TEST_LIB ".HC -o " INIT_TEST_LIB ".o && "
              "../../hcc $HCC_TEST_FLAGS -c " INIT_TEST_MAIN ".HC -o " INIT_TEST_MAIN ".o && "
              "cc " INIT_TEST_LIB ".o " INIT_TEST_MAIN ".o -o " INIT_TEST_MAIN " && "
              "./" INIT_TEST_MAIN) == 0;
  Rm(INIT_TEST_LIB ".HC");
  Rm(INIT_TEST_MAIN ".HC");
  Rm(INIT_TEST_LIB ".o");
  Rm(INIT_TEST_MAIN ".o");
  Rm(INIT_TEST_MAIN);
  return ok;
}

I32 Main()
{
  "Test - global constant initialisers: ";
  I64 correct = 0;

  if (g_int == 334 && g_neg == -5 && g_i32 == -7 && g_u8 == 44) correct++;
  if (g_f64 == 1.5 && g_fneg == -2.5 && g_from_int == 3.0) correct++;
  if (g_null == NULL && g_auto == 9 && g_expr == 19) correct++;
  if (g_first == 1 && g_ptr == NULL && g_third == 3) correct++;
  if (s_int == 11 && s_second == 22 && s_zero == 0) correct++;
  if (s_f64 == 0.25 && !strcmp(s_str, "static")) correct++;
  if (g_int_div == 1.0 && g_truncated == 1 && g_float_expr == 3) correct++;
  if (g_from_global == 335) correct++;
  if (g_tentative == 5 && g_redeclared == 2) correct++;
  if (Bump() == 335 && g_int == 335) correct++;
  if (LinksTwoUnits()) correct++;

  PrintResult(correct, 11);
  "====\n";
  return correct != 11;
}
