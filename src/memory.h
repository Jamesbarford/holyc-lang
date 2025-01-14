#ifndef MEMORY_H__
#define MEMORY_H__

#include "map.h"

#ifdef __cplusplus
extern "C" {
#endif

#define MEM_PREFIX (sizeof(long))

typedef struct MemoryPool {
    /* Used to index the entries above which are the memory */
    IntVec *free_indexes;
    /* How big one member is */
    unsigned int member_size;
    /* This will be how big the array of entries is */
    long slab_size;
    /* What the current starting index is for the next allocation  */
    long idx_start;
    /* Is a list of all of the memory pool entries that have been allocated.
     * Makes it easier to use an index to find the next free piece of memory.
     * Where the index to this vector is `(idx / slab_size)` */
    PtrVec *mem_pool_entries;

    /* If you want to inialise some of the memory before using it */
    void (*init_memory)(void *);
} MemoryPool;

typedef struct MemoryPoolIterator {
    MemoryPool *pool;
    long vec_idx;
    long entry_idx;
} MemoryPoolIterator;

#define memPoolSetInitMemory(pool, init_fn) \
    ((pool)->init_memory = (init_fn))

void *xmalloc(size_t size);
void xfree(void *ptr);
MemoryPool *memPoolNew(unsigned int member_size, long slab_size);
MemoryPool *memPoolNewAndInitialise(unsigned int member_size,
        long slab_size, void (*init_memory)(void *));
void *memPoolAlloc(MemoryPool *pool);
void memPoolFree(MemoryPool *pool, void *ptr);
void memPoolRelease(MemoryPool *pool);
MemoryPoolIterator *memPoolIteratorNew(MemoryPool *pool);
void memPoolIteratorRelease(MemoryPoolIterator *it);
void *memPoolNext(MemoryPoolIterator *it);

#ifdef __cplusplus
};
#endif

#endif
