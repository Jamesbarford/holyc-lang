#ifndef MEMORY_H__
#define MEMORY_H__

#define MEM_PREFIX (sizeof(long))

void *xmalloc(size_t size);
void xfree(void *ptr);

#endif
