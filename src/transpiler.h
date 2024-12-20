#ifndef TRANSPILER_H
#define TRANSPILER_H

#include "aostr.h"
#include "cctrl.h"

aoStr *transpileToC(Cctrl *cc, HccOpts *opts);

#endif // TRANSPILER_H
