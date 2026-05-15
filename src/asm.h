#ifndef ASM_H__
#define ASM_H__

#include "aostr.h"
#include "cctrl.h"

char *asmNormaliseFunctionName(Cctrl *cc, AoStr *fname);
AoStr *asmGenerate(Cctrl *cc);
uint64_t ieee754(double _f64);

#endif
