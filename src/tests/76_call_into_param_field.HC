#include "testhelper.HC"

/* A call's result stored through a pointer parameter: the parameter's
 * register is reused for the call's arguments, so the pointer has to
 * outlive them. */

class Game
{
  I64 cam_x;
  I64 cam_y;
  I64 cam_z;
};

Game g_game;

I64 Max(I64 a, I64 b)
{
  if (a > b) return a;
  return b;
}

I64 Sum3(I64 a, I64 b, I64 c) { return a + b + c; }

U0 FirstParam(Game *g, I64 x) { g->cam_y = Max(0, x); }
U0 SecondParam(I64 x, Game *g) { g->cam_y = Max(x, 1); }

U0 ThirdParam(I64 x, I64 y, Game *g)
{
  g->cam_z = Sum3(x, y, 3);
  g->cam_y = Max(x, y);
  g->cam_x = g->cam_y + g->cam_z;
}

U0 ThroughLocal(Game *g, I64 x)
{
  Game *h = g;
  h->cam_y = Max(0, x);
}

U0 ThroughIndex(I64 *p, I64 i, I64 x) { p[i] = Max(0, x); }
U0 Compound(Game *g, I64 x) { g->cam_y += Max(0, x); }

I32 Main()
{
  "Test - call into a param field: ";
  I64 correct = 0, arr[3];

  FirstParam(&g_game, 7);
  if (g_game.cam_y == 7) correct++;
  SecondParam(8, &g_game);
  if (g_game.cam_y == 8) correct++;
  ThirdParam(1, 2, &g_game);
  if (g_game.cam_x == 8 && g_game.cam_y == 2 && g_game.cam_z == 6) correct++;
  ThroughLocal(&g_game, 9);
  if (g_game.cam_y == 9) correct++;
  ThroughIndex(arr, 2, 6);
  if (arr[2] == 6) correct++;
  Compound(&g_game, 1);
  if (g_game.cam_y == 10) correct++;

  PrintResult(correct, 6);
  "====\n";
  return correct != 6;
}
