#ifndef MEMPOOL_H__
#define MEMPOOL_H__

#include <pthread.h>
#include "types.h"

typedef enum {
    MEMPOOL_OK = 0,                          
    MEMPOOL_ALLOCATION_GREATER_THAN_CAPACITY_ERR,
    MEMPOOL_ALLOCATION_SPLIT_BLOCK_ERR,
    MEMPOOL_ALLOCATION_SEGMENT_ARRAY_ERR,
    MEMPOOL_ALLOCATION_SEGMENT_ALLOC_ERR,
} MemPoolError;

typedef void memPoolDeallocate(void *owner, void *ptr);

typedef struct MemChunk MemChunk;
typedef struct MemChunk {
    u32 segment_id; /* Which segment this block belongs to */
    u32 size;       /* How big this chunk is */
    MemChunk *next;          /* Next bump */
    char free;               /* Is this block free? */
} __attribute__((aligned(8))) MemChunk;

typedef struct MemSegment {
    u32 id;        /* Id of this segment */
    MemChunk *list;         /* The memory list, which is an offset to the
                             * buffer */

    void *buffer;           /* The memory for this segment */
    u32 allocated; /* How much memory has been allocated */
} MemSegment;

typedef struct MemPool {
    MemSegment **segments;         /* Segments array */
    u32 segments_array_capacity; /* The capacity of the segments array */

    u32 segment_count;    /* How many segments we have */

    u32 segment_capacity; /* The size in bytes of a segment. Cannot
                                    * allocate anything bigger than this */

    pthread_mutex_t mutex;         /* Lock for the pool to allow for multi
                                    * threading */

    MemPoolError error;            /* Error code for what the allocation 
                                    * error was */
    memPoolDeallocate *deallocate; /* If not null it will be called on an
                                    * object which is free - this is useful
                                    * if an object also has its own memory
                                    * independent of the pool, or indeed is
                                    * another pool? */
} MemPool;

const char *memPoolErrorToString(MemPool *pool);
char *memChunkToString(MemChunk *chunk);
char *memPoolToString(MemPool *pool);
char *memSegmentToString(MemSegment *segment);

MemPool *memPoolNew(u32 bytes);
void memPoolInit(MemPool *pool, u32 bytes);
void *memPoolAlloc(MemPool *pool, u32 size);
void *memPoolTryAlloc(MemPool *pool, u32 size);
void memPoolSetDeallocate(MemPool *pool, memPoolDeallocate *deallocate);
void memPoolRelease(MemPool *pool, char free_pool);
void memPoolFree(MemPool *pool, void *ptr);

typedef struct MemPoolIterator {
    MemPool *pool;
    MemChunk *chunk;
    u32 segment_id;
} MemPoolIterator;

MemPoolIterator *memPoolIteratorNew(MemPool *pool);
void memPoolIteratorRelease(MemPoolIterator *it);
void *memPoolNext(MemPoolIterator *it);

#endif
