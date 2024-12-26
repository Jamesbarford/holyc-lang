#ifndef TRANSPILER_H
#define TRANSPILER_H

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "compile.h"

aoStr *transpileToC(Cctrl *cc, HccOpts *opts);
char *transpileOneAst(Cctrl *cc, Ast *ast);

#endif // TRANSPILER_H
