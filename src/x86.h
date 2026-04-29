#ifndef ASM_H
#define ASM_H

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "list.h"

AoStr *asmGenerate(Cctrl *cc);

/* Shared helpers exposed for the IR-driven codegen path. */
int asmFunctionInit(Cctrl *cc, AoStr *buf, Ast *func);
void asmFunctionLeave(AoStr *buf);
int asmHasRet(AoStr *buf);

#endif // !ASM_H
