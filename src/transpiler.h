#ifndef TRANSPILER_H
#define TRANSPILER_H

#include "cli.h"
#include "cctrl.h"

AoStr *transpileToC(Cctrl *cc, CliArgs *args);

#endif // TRANSPILER_H
