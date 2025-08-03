#ifndef MEMORY_H__
#define MEMORY_H__

void *xmalloc(u64 size);
void xfree(void *ptr);

void globalArenaInit(u32 capacity);
void *globalArenaAllocate(u32 size);
void globalArenaRelease(void);
void globalArenaPrintStats(void);

#endif
