#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <pthread.h>

#include "aostr.h"
#include "util.h"
#include "mempool.h"

const char *memPoolErrorToString(MemPool *pool) {
    switch (pool->error) {
        case MEMPOOL_OK:
            return "Ok";
        case MEMPOOL_ALLOCATION_SPLIT_BLOCK_ERR:
            return "MEMPOOL_ALLOCATION_SPLIT_BLOCK_FAIL";
        case MEMPOOL_ALLOCATION_GREATER_THAN_CAPACITY_ERR:
            return "MEMPOOL_ALLOCATION_GREATER_THAN_CAPACITY";
        case MEMPOOL_ALLOCATION_SEGMENT_ARRAY_ERR:
            return "MEMPOOL_ALLOCATION_NEW_SEGMENT_ERR";
        case MEMPOOL_ALLOCATION_SEGMENT_ALLOC_ERR:
            return "MEMPOOL_ALLOCATION_SEGMENT_ALLOC_ERR";
        default:
            loggerPanic("Unexpected memory pool exception; %d\n", pool->error);
    }
}

char *memSegmentToString(MemSegment *segment) {
    AoStr *str = aoStrNew();
    aoStrCatPrintf(str,
            "MemSegment {\n  id = %u;\n  buffer = %p;\n  allocated = %u;\n}",
            segment->id,segment->buffer,segment->allocated);
    return aoStrMove(str);
}

char *memChunkToString(MemChunk *chunk) {
    AoStr *str = aoStrNew();
    aoStrCatPrintf(str,
            "MemChunk {\n"
            "  segment_id = %u;\n"
            "  address = %p;\n"
            "  size = %u;\n"
            "  next = %p;\n"
            "  free = %d;\n}",
            chunk->segment_id,chunk,
            chunk->size, chunk->next, chunk->free);
    return aoStrMove(str);
}

/* This is a messy implmentation but good enough as it is for debugging not 
 * really for actual use. */
char *memPoolToString(MemPool *pool) {
    pthread_mutex_lock(&pool->mutex);
    AoStr *str = aoStrAlloc(2048);

    aoStrCatPrintf(str,"MemPool {\n  segment_count = %u;\n"
                           "  segment_capacity = %u;\n"
                           "  error = %s;\n"
                           "  segments = [",
                           pool->segment_count,
                           pool->segment_capacity,
                           memPoolErrorToString(pool));

    for (u32 i = 0; i < pool->segment_count; ++i) {
        MemSegment *segment = pool->segments[i];
        MemChunk *chunk = segment->list;
        aoStrCatPrintf(str,
                "\n"
                "    MemSegment {\n"
                "      id = %u;\n"
                "      address = %p\n"
                "      allocated = %u;\n"
                "      list = [",
                segment->id, segment, segment->allocated);

        while (chunk) {
            aoStrCatPrintf(str,
                    "\n"
                    "        MemChunk {\n"
                    "          segment_id = %u;\n"
                    "          address = %p\n"
                    "          size = %u;\n"
                    "          free = %u;\n"
                    "        }",
                    chunk->segment_id,
                    chunk,
                    chunk->size,
                    chunk->free);
            if (chunk->next != NULL) {
                aoStrCatPrintf(str,",");
            }
            chunk = chunk->next;
        }

        aoStrCatPrintf(str, "\n    ]");
    }

    aoStrCatPrintf(str, "\n  ]\n}");
    pthread_mutex_unlock(&pool->mutex);
    return aoStrMove(str);
}

/* Align to 8 bytes */
static u32 _memAlign(u32 size) {
    static const int alignment = 8;
    return (size + (alignment - 1)) & ~(alignment - 1);
}

static MemSegment *memSegmentAlloc(u32 capacity,
                                   u32 segment_id)
{
    MemSegment *segment = (MemSegment *)malloc(sizeof(MemSegment));
    segment->allocated = 0;
    segment->buffer = (void *)malloc(capacity);
    segment->list = (MemChunk *)segment->buffer;
    segment->list->next = NULL;
    segment->list->free = 1;
    /* The block is the full size of the allocated memory */
    segment->list->size = capacity;
    segment->list->segment_id = segment_id;
    segment->id = segment_id;
    return segment;
}

void memPoolInit(MemPool *pool, u32 bytes) {
    const u32 segments_array_capacity = 4;

    MemSegment **segments_array = (MemSegment **)malloc(sizeof(MemSegment *) * segments_array_capacity);
    if (!segments_array) {
        loggerPanic("Failed to allocate segments array\n");
    }
    
    MemSegment *segment = memSegmentAlloc(bytes, pool->segment_count);

    if (!segment) {
        loggerPanic("Failed to allocate initial segment\n");
    }

    pool->segments_array_capacity = segments_array_capacity;
    pool->segment_capacity = bytes;
    pool->segment_count = 0;
    pool->segments = segments_array;
    pool->segments[pool->segment_count++] = segment; 

    pool->error = MEMPOOL_OK;
    pool->deallocate = NULL;

    if (pthread_mutex_init(&pool->mutex, NULL) != 0) {
        loggerPanic("Failed to initialise Mutex\n");
    }
}

/* How many bytes should initially be in the pool, note the pool will not 
 * be able to allocate anything larger than 'bytes' size. Thus allocating 
 * something like a dynamically resizable array from this pool makes no sense */
MemPool *memPoolNew(u32 bytes) {
    MemPool *pool = (MemPool *)malloc(sizeof(MemPool));
    memPoolInit(pool,bytes);
    return pool;
}

void memPoolSetDeallocate(MemPool *pool, memPoolDeallocate *deallocate) {
    pool->deallocate = deallocate;
}

#define MIN_PAYLOAD_SIZE 8
#define MIN_SPLIT_SIZE (sizeof(MemChunk) + MIN_PAYLOAD_SIZE)

/* We only take what we need from a block and split bits off it. This can 
 * only be called if we are sure that we have enough memory in the given 
 * `MemChunk`. This can fail if there is not enough space to allocate 
 * the `alloc_size + sizeof(MemChunk)` */
static void memPoolSplitBlock(MemChunk *chunk, u32 alloc_size) {
    u32 total_needed = alloc_size + sizeof(MemChunk);
    if (chunk->size < total_needed + MIN_SPLIT_SIZE) {
        return;
    }

    /* The remaining size of the block */
    u32 new_chunk_size = chunk->size - total_needed;

    MemChunk *new_chunk = (MemChunk*)((u8 *)chunk + total_needed);
    new_chunk->size = new_chunk_size;
    new_chunk->free = 1;
    new_chunk->next = chunk->next;
    new_chunk->segment_id = chunk->segment_id;

    chunk->size = alloc_size+sizeof(MemChunk);
    chunk->next = new_chunk;
}

/* Find a segment that has enough memory left in it to allocate from. This 
 * highlights why segments should be fairly big - it allows for less scanning 
 * to find a suitable segment. */
static MemSegment *memPoolGetSegment(MemPool *pool, u32 alloc_size) {
    u32 total_size = alloc_size + sizeof(MemChunk);

    for (u32 i = 0; i < pool->segment_count; ++i) {
        /* If there is enough space use this segment and increment how many 
         * bytes have been allocated from the segment. */
        MemSegment *segment = pool->segments[i];
        if (segment->allocated + total_size <= pool->segment_capacity) {
            return segment;
        }
    }

    u32 new_segment_id = pool->segment_count;
    MemSegment *segment = memSegmentAlloc(pool->segment_capacity,
                                          new_segment_id);
    if (segment == NULL) {
        pool->error = MEMPOOL_ALLOCATION_SEGMENT_ALLOC_ERR;
        goto error;
    }

    if (pool->segment_count + 1 >= pool->segments_array_capacity) {
        /* Expand the array */
        u32 new_capacity = pool->segments_array_capacity * 2;

        MemSegment **new_segments = (MemSegment **)malloc(sizeof(MemSegment *)*(new_capacity));
        if (new_segments == NULL) {
            pool->error = MEMPOOL_ALLOCATION_SEGMENT_ARRAY_ERR;
            free(segment->buffer);
            free(segment);
            goto error;
        }

        /* Copy over the old segments */
        for (u32 i = 0; i < new_segment_id; ++i) {
            new_segments[i] = pool->segments[i];
        }

        free(pool->segments);
        pool->segments = new_segments;
        pool->segments_array_capacity = new_capacity;
    }

    pool->segments[pool->segment_count++] = segment;
    return segment;

error:
    return NULL;
}

/* Allocate some memory from the pool */
void *memPoolAlloc(MemPool *pool, u32 size) {
    /* Gotos make controlling the lock easier by only needing to keep track 
     * of them in one place */
    pthread_mutex_lock(&pool->mutex);
    if (size == 0) goto error;

    /* We only provide aligned memory */
    size = _memAlign(size);
    if (size > pool->segment_capacity) {
        pool->error = MEMPOOL_ALLOCATION_GREATER_THAN_CAPACITY_ERR;
        goto error;
    }

    /* Try to either get or allocate a new segment */
    MemSegment *segment = memPoolGetSegment(pool, size);
    if (segment == NULL) {
        goto error;
    }

    void *ptr = NULL;
    MemChunk *cur = segment->list;
    unsigned total_size = size + sizeof(MemChunk);

    while (cur) {
        if (cur->free && cur->size >= total_size) {
            if (cur->size > total_size) {
                memPoolSplitBlock(cur, size);
            }

            cur->segment_id = segment->id;
            cur->free = 0;
            segment->allocated += cur->size;
            ptr = (void *)((u8 *)cur + sizeof(MemChunk));
            break;
        }
        cur = cur->next;
    }

    pthread_mutex_unlock(&pool->mutex);
    return ptr;

error:
    pthread_mutex_unlock(&pool->mutex);
    return NULL;
}

/* Allocate a block or panic */
void *memPoolTryAlloc(MemPool *pool, u32 size) {
    void *ptr = memPoolAlloc(pool, size);
    if (ptr == NULL) {
        u32 max_allocation_size = pool->segment_capacity - sizeof(MemChunk);
        loggerPanic("Failed to allocate %u bytes, max allocation size = %u - %s\n",
                size, max_allocation_size, memPoolErrorToString(pool));
    }
    return ptr;
}

static void memSegmentMerge(MemSegment *segment) {
    MemChunk *cur = segment->list;
    while (cur && cur->next) {
        u8 *end = (u8 *)cur + sizeof(MemChunk) + cur->size;
        if (cur->free && cur->next->free && (u8 *)cur->next == end) {
            cur->size += sizeof(MemChunk) + cur->next->size;
            cur->next = cur->next->next;
        } else {
            cur = cur->next;
        }
    }
}

/* Free some memory that was allocated from the pool */
void memPoolFree(MemPool *pool, void *ptr) {
    pthread_mutex_lock(&pool->mutex);
    if (ptr) {
        /* Pass the pool in incase it is needed by the callee */
        if (pool->deallocate) {
            pool->deallocate((void *)pool, ptr);
        }

        MemChunk *chunk = (MemChunk*)((u8 *)ptr - sizeof(MemChunk));
        MemSegment *segment = pool->segments[chunk->segment_id];

        chunk->free = 1;
        segment->allocated -= chunk->size;
        memSegmentMerge(segment);
    }
    pthread_mutex_unlock(&pool->mutex);
}

/* This has the potential to be extremely slow, however the upfront cost 
 * of multiple allocations is reduced and keeping track of memory simplified 
 * as it is all traceable back too the pool. */
void memPoolRelease(MemPool *pool, char free_pool) {
    if (pool) {
        MemPoolIterator *it = memPoolIteratorNew(pool);
        void *ptr = NULL;

        /* Sometimes we may want to have non-pooled memory used on 
         * an allocated object */
        if (pool->deallocate) {
            while ((ptr = memPoolNext(it)) != NULL) {
                pool->deallocate(pool, ptr);
            }
        }

        for (u32 i = 0; i < pool->segment_count; ++i) {
            MemSegment *segment = pool->segments[i];
            free(segment->buffer);
            free(segment);
        }

        pthread_mutex_destroy(&pool->mutex);
        free(pool->segments);
        if (free_pool) {
            free(pool);
        }
    }
}

MemPoolIterator *memPoolIteratorNew(MemPool *pool) {
    MemPoolIterator *iter = (MemPoolIterator *)malloc(sizeof(MemPoolIterator));
    if (iter == NULL) {
        loggerPanic("Failed to allocated memory for MemPoolIterator\n");
    }
    iter->pool = pool;
    iter->segment_id = 0;
    iter->chunk = pool->segments[0]->list;
    return iter;
}

void memPoolIteratorRelease(MemPoolIterator *it) {
    free(it);
}

void *memPoolNext(MemPoolIterator *it) {
    /* If the chunk is free or there is no chunk because we are at the end 
     * of a list, we need a new segment or are finished iterating */
    if (it->chunk == NULL || it->chunk->free) {
        it->segment_id++;
        if (it->segment_id >= it->pool->segment_count) {
            return NULL;
        }
        it->chunk = it->pool->segments[it->segment_id]->list;
    }

    void *ptr = (void *)((u8 *)it->chunk + sizeof(MemChunk));
    it->chunk = it->chunk->next;
    return ptr;
}
