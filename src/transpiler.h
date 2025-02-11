#ifndef TRANSPILER_H
#define TRANSPILER_H

#include "cctrl.h"
#include "cli.h"

aoStr *transpileToC(Cctrl *cc, CliArgs *args);

#endif // TRANSPILER_H
