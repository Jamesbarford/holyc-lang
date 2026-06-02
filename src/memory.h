#ifndef MEMORY_H__
#define MEMORY_H__

#include "types.h"

void globalArenaInit(u32 capacity);
void *globalArenaAllocate(u32 size);
void globalArenaRelease(void);
void globalArenaPrintStats(void);

#endif
