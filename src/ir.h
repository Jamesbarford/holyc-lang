#ifndef IR_H__
#define IR_H__

#include "aostr.h"
#include "cctrl.h"
#include "ir-types.h"
#include "map.h"

void irArenaInit(unsigned int capacity);
void irMemoryRelease(void);
void irMemoryStats(void);
void irLowerAst(Cctrl *cc);

#endif
