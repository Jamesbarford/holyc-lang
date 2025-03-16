#ifndef IR_H__
#define IR_H__

#include "cctrl.h"

void irArenaInit(unsigned int capacity);
void irMemoryRelease(void);
void irMemoryStats(void);
void irLowerAst(Cctrl *cc);

#endif
