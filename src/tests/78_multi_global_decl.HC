#include "testhelper.HC"

/* Global declarations with more than one declarator. Only the first used
 * to be declared. */

class Vec2
{
  I64 x, y;
};

I64 g_a, g_b;
Vec2 g_v1, g_v2;
Vec2 *g_p1, g_v3, *g_p2;
I64 *g_ip, g_i, **g_ipp;
I64 g_i1 = 1, g_i2 = 2, g_i3, g_i4 = 4;
I64 g_arr[3], g_after, g_arr2[2];
I64 g_init[3] = {1, 2, 3}, g_after2 = 7, g_init2[2] = {8, 9};
Vec2 g_ini1 = {1, 2}, g_ini2 = {3, 4}, g_ini3;
U8 *g_str = "abc", *g_str2 = "de", g_chr = 'x';
F64 g_f1 = 1.5, g_f2, g_f3 = 2.5;
auto g_auto1 = 5, g_auto2 = 2.5;
public I64 g_pub1, g_pub2 = 3;
static I64 g_st1, g_st2 = 4;
I64 g_expr1 = 1 + 2, g_expr2 = g_expr1 * 2;

I64 g_correct = 0;
I64 g_total = 0;

U0 Check(I64 got, I64 want, U8 *what)
{
  g_total++;
  if (got == want) g_correct++;
  else "%s: got %ld, expected %ld\n", what, got, want;
}

I32 Main()
{
  "Test - multiple global declarators: ";
  g_a = 1; g_b = 2;
  Check(g_a + g_b, 3, "plain");
  g_v1.x = 1; g_v2.y = 2;
  Check(g_v1.x + g_v2.y, 3, "class");

  /* A `*` belongs to its own declarator */
  g_p1 = &g_v3; g_v3.x = 5; g_p2 = g_p1;
  Check(g_p2->x, 5, "pointer either side");
  Check(sizeof(g_v3), 16, "the middle one is not a pointer");
  g_i = 4; g_ip = &g_i; g_ipp = &g_ip;
  Check(**g_ipp, 4, "pointer to pointer");
  Check(sizeof(g_i), 8, "plain after a pointer");

  Check(g_i1 + g_i2 + g_i3 + g_i4, 7, "initialisers");

  g_arr[2] = 9; g_after = 3; g_arr2[1] = 2;
  Check(g_arr[2] + g_after + g_arr2[1], 14, "arrays");
  Check(sizeof(g_arr), 24, "array size");
  Check(sizeof(g_after), 8, "dimensions belong to one declarator");
  Check(sizeof(g_arr2), 16, "second array size");

  Check(g_init[2] + g_after2 + g_init2[1], 19, "array initialisers");
  Check(g_ini1.y + g_ini2.x + g_ini3.x, 5, "class initialisers");
  Check(g_str[0] + g_str2[1] + g_chr, 'a' + 'e' + 'x', "strings");
  Check((g_f1 + g_f2 + g_f3)(I64), 4, "floats");
  Check(g_auto1, 5, "auto");
  Check((g_auto2 * 2.0)(I64), 5, "auto takes each initialiser's type");
  g_pub1 = 1; g_st1 = 2;
  Check(g_pub1 + g_pub2 + g_st1 + g_st2, 10, "public and static");
  Check(g_expr2, 6, "a later initialiser reads an earlier one");

  PrintResult(g_correct, g_total);
  "====\n";
  return g_correct != g_total;
}
