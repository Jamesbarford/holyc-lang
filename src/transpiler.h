#ifndef TRANSPILER_H
#define TRANSPILER_H

#include "aostr.h"
#include "cctrl.h"

aoStr *transpileToC(Cctrl *cc, char *file_name);

#endif // TRANSPILER_H
