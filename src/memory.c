#include <stdlib.h>

#include "map.h"
#include "memory.h"
#include "util.h"

void *xmalloc(size_t size) {
    void *ptr = (void *)malloc(size);
    if (!ptr) {
        loggerPanic("OOM\n");
    }
    return ptr;
}

void xfree(void *ptr) {
    if (ptr) free(ptr);
}

/* Creates an array of memory that will be stored in mem_pool_entries vector. 
 * To keep track of free entries we use indexes and add them to the free_indexes 
 * vector. */
static void *memPoolEntriesNew(MemoryPool *pool) {
    unsigned int entry_size = (pool->member_size + MEM_PREFIX);
    unsigned int memory_size = entry_size * (unsigned int)pool->slab_size;
    void *entries = (void *)xmalloc(memory_size);

    /* Walk the entries setting their indexes, and initalise each entry 
     * if there is an initalisaion function */
    for (long i = pool->slab_size-1; i >= 0; --i) {
        long idx = i + pool->idx_start;
        void *entry = (void *)((char *)entries + i * entry_size);
        *((long*)entry) = idx;
        if (pool->init_memory) pool->init_memory(entry+MEM_PREFIX);
        intVecPush(pool->free_indexes,idx);
    }
    return entries;
}

MemoryPool *memPoolNewAndInitialise(unsigned int member_size,
        long slab_size, void (*init_memory)(void *))
{
    MemoryPool *pool = (MemoryPool *)xmalloc(sizeof(MemoryPool));
    pool->member_size = member_size;
    pool->free_indexes = intVecNew();
    pool->mem_pool_entries = ptrVecNew();
    pool->idx_start = 0;
    pool->slab_size = slab_size;
    pool->init_memory = init_memory;
    void *entries = memPoolEntriesNew(pool);
    ptrVecPush(pool->mem_pool_entries,entries);
    return pool;
}

MemoryPool *memPoolNew(unsigned int member_size, long slab_size) {
    return memPoolNewAndInitialise(member_size,slab_size,NULL);
}

/* Get the position in the mem_pool_entries */
static long memPoolGetEntryVecIdx(MemoryPool *pool, long entry_no) {
    return entry_no / pool->slab_size;
}

/* Get the position in the entries array */
static long memPoolGetEntryIdx(MemoryPool *pool, long entry_no,
        long mem_pool_entries_idx)
{
    return entry_no - (pool->slab_size * mem_pool_entries_idx);
}

static void *memPoolGetMemory(MemoryPool *pool, long entry_no) {
    unsigned int entry_size = (pool->member_size + MEM_PREFIX);
    long mem_pool_entries_idx = memPoolGetEntryVecIdx(pool, entry_no);
    long entry_idx = memPoolGetEntryIdx(pool, entry_no, mem_pool_entries_idx);

    void *entries = vecGet(void*,pool->mem_pool_entries,
            mem_pool_entries_idx);    
    long idx = entry_idx * entry_size;
   // loggerDebug("entry_no = %ld, idx = %3ld, mem_pool_entries_idx = %ld\n",entry_no,idx, mem_pool_entries_idx);
    return (void *)(entries + idx + MEM_PREFIX);
}

static void memPoolResize(MemoryPool *pool) {
    pool->idx_start += pool->slab_size;
    void *new_entries = memPoolEntriesNew(pool);
    ptrVecPush(pool->mem_pool_entries,new_entries);
}

void *memPoolAlloc(MemoryPool *pool) {
    int ok;
    long entry_no = intVecPop(pool->free_indexes,&ok);
    void *ptr;

    if (ok) {
        ptr = memPoolGetMemory(pool,entry_no);
    } else {
        memPoolResize(pool);
        entry_no = intVecPop(pool->free_indexes,&ok);
        ptr = memPoolGetMemory(pool,entry_no);
    }

    return ptr;
}

void memPoolFree(MemoryPool *pool, void *ptr) {
    if (ptr) {
        long idx = *((long *)(ptr - MEM_PREFIX));
        intVecPush(pool->free_indexes,idx);
    }
}

void memPoolRelease(MemoryPool *pool) {
    for (int i = 0; i < pool->mem_pool_entries->size; ++i) {
        xfree(pool->mem_pool_entries->entries[i]);
    }
    ptrVecRelease(pool->mem_pool_entries);
    intVecRelease(pool->free_indexes);
    xfree(pool);
}

MemoryPoolIterator *memPoolIteratorNew(MemoryPool *pool) {
    MemoryPoolIterator *it = (MemoryPoolIterator *)xmalloc(
            sizeof(MemoryPoolIterator));
    it->pool = pool;
    it->vec_idx = 0;
    it->entry_idx = 0;
    return it;
}

void memPoolIteratorRelease(MemoryPoolIterator *it) {
    xfree(it);
}

void *memPoolNext(MemoryPoolIterator *it) {
    if (it->entry_idx >= it->pool->slab_size) {
        if (it->vec_idx + 1 >= it->pool->mem_pool_entries->size) {
            return NULL;
        }
        it->vec_idx++;
        it->entry_idx = 0;
    }

    unsigned int entry_size = (it->pool->member_size + MEM_PREFIX);
    void *entries = ptrVecGet(it->pool->mem_pool_entries,it->vec_idx);
    long idx = (it->entry_idx * entry_size);
    void *ptr = (void *)((char *)entries + idx + MEM_PREFIX);
    it->entry_idx++;
    return ptr;
}
