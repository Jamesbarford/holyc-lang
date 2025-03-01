#ifndef TRANSPILER_H
#define TRANSPILER_H

#include "cli.h"
#include "cctrl.h"

aoStr *transpileToC(Cctrl *cc, CliArgs *args);
void astPrintCType(AstType *type);
void astPrintHolyCType(AstType *type);

#endif // TRANSPILER_H
