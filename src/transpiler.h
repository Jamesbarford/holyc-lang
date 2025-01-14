#ifndef TRANSPILER_H
#define TRANSPILER_H

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "compile.h"

#ifdef __cplusplus
extern "C" {
#endif

aoStr *transpileToC(Cctrl *cc, HccOpts *opts);
char *transpileOneAst(Cctrl *cc, Ast *ast);

#ifdef __cplusplus
};
#endif

#endif // TRANSPILER_H
