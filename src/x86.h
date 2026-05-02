#ifndef ASM_H
#define ASM_H

#include <stdint.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "list.h"

AoStr *asmGenerate(Cctrl *cc);

/* Shared helpers exposed for the IR-driven codegen path. */
int asmFunctionInit(Cctrl *cc, AoStr *buf, Ast *func);
void asmFunctionLeave(AoStr *buf);
int asmHasRet(AoStr *buf);
void asmRemovePreviousTab(AoStr *buf);
char *asmNormaliseFunctionName(char *fname);
uint64_t ieee754(double _f64);

#endif // !ASM_H
