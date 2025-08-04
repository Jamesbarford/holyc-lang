#ifndef IR_H__
#define IR_H__

#include "cctrl.h"

void irMemoryInit(void);
void irMemoryRelease(void);
void irMemoryStats(void);
void irDump(Cctrl *cc);

#endif
