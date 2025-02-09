#ifndef MEMORY_H__
#define MEMORY_H__

void *xmalloc(size_t size);
void xfree(void *ptr);

void globalArenaInit(unsigned int capacity);
void *globalArenaAllocate(unsigned int size);
void globalArenaRelease(void);
void globalArenaPrintStats(void);

#endif
