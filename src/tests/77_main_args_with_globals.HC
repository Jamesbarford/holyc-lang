#include "testhelper.HC"

/* The generated entry runs global initialisers before calling Main, with
 * argc and argv still in the argument registers. Using those registers for
 * the initialiser handed Main the stored value and the global's address. */

U64 g_seed = 7;
I64 g_count = 0;
F64 g_ratio = 0.5;

I32 Main(I32 argc, U8 **argv)
{
  "Test - main args with globals: ";
  I64 correct = 0;
  if (argc == 1) correct++;
  if (argv[0] && StrLen(argv[0]) > 0) correct++;
  if (argv[1] == NULL) correct++;
  if (g_seed == 7 && g_count == 0 && g_ratio == 0.5) correct++;
  PrintResult(correct, 4);
  "====\n";
  return correct != 4;
}
