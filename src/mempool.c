#include <errno.h>
#include <pthread.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mempool.h"

typedef struct _MemString {
    unsigned int len;
    unsigned int capacity;
    char *buf;
} _MemString;

__attribute__((noreturn)) void memPanic(const char *fmt, ...) {
    char buffer[BUFSIZ];
    va_list ap;
    va_start(ap, fmt);
    long len = vsnprintf(buffer, sizeof(buffer), fmt, ap);
    buffer[len] = '\0';
    fprintf(stderr, "Error: %s", buffer);
    va_end(ap);
    exit(EXIT_FAILURE);
}

_MemString *memStringNew(unsigned int capacity) {
    _MemString *str = (_MemString *)malloc(sizeof(_MemString));
    str->len = 0;
    str->capacity = capacity;
    str->buf = (char *)malloc(sizeof(char) * capacity);
    return str;
}

void memStringRelease(_MemString *str) {
    if (str) {
        free(str->buf);
        free(str);
    }
}

void memStringCatPrintf(_MemString *str, const char *fmt, ...) {
    va_list ap, copy;
    va_start(ap, fmt);

    unsigned int fmt_len = strlen(fmt);
    unsigned int bufferlen = 1024;
    unsigned int len = 0;

    if (fmt_len > bufferlen) {
        bufferlen = fmt_len;
    }

    /* Probably big enough */
    char *buf = (char *)malloc(sizeof(char) * bufferlen + 1);

    while (1) {
        va_copy(copy, ap);
        len = vsnprintf(buf, bufferlen, fmt, copy);
        va_end(copy);

        if (((size_t)len) >= bufferlen) {
            free(buf);
            bufferlen = ((size_t)len) + 2;
            buf = (char *)malloc(bufferlen);
            if (buf == NULL) {
                va_end(ap);
                return;
            }
            continue;
        }
        break;
    }

    if (str->len + len >= str->capacity) {
        unsigned int new_capacity = (str->capacity * 2) + len;
        char *tmp = (char *)realloc(str->buf, sizeof(char) * new_capacity);
        if (tmp == NULL) {
            memPanic("Failed to reallocated _MemString\n");
        }
        str->buf = tmp;
        str->capacity = new_capacity;
    }

    memcpy(str->buf + str->len, buf, len);
    str->len += len;
    str->buf[str->len] = '\0';
    free(buf);
    va_end(ap);
}

char *memStringMove(_MemString *str) {
    char *buf = str->buf;
    free(str);
    return buf;
}

const char *memPoolErrorToString(const MemPool *pool) {
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
    }
}

char *memSegmentToString(const MemSegment *segment) {
    _MemString *str = memStringNew(128);
    memStringCatPrintf(
            str,
            "MemSegment {\n  id = %u;\n  buffer = %p;\n  allocated = %u;\n}",
            segment->id, segment->buffer, segment->allocated);
    return memStringMove(str);
}

char *memChunkToString(MemChunk *chunk) {
    _MemString *str = memStringNew(128);
    memStringCatPrintf(str,
                       "MemChunk {\n"
                       "  segment_id = %u;\n"
                       "  address = %p;\n"
                       "  size = %u;\n"
                       "  next = %p;\n"
                       "  free = %d;\n}",
                       chunk->segment_id, chunk, chunk->size, chunk->next,
                       chunk->free);
    return memStringMove(str);
}

/* This is a messy implmentation but good enough as it is for debugging not
 * really for actual use. */
char *memPoolToString(MemPool *pool) {
    pthread_mutex_lock(&pool->mutex);
    _MemString *str = memStringNew(2048);

    memStringCatPrintf(str,
                       "MemPool {\n  segment_count = %u;\n"
                       "  segment_capacity = %u;\n"
                       "  error = %s;\n"
                       "  segments = [",
                       pool->segment_count, pool->segment_capacity,
                       memPoolErrorToString(pool));

    for (unsigned int i = 0; i < pool->segment_count; ++i) {
        MemSegment *segment = pool->segments[i];
        MemChunk *chunk = segment->list;
        memStringCatPrintf(str,
                           "\n"
                           "    MemSegment {\n"
                           "      id = %u;\n"
                           "      address = %p\n"
                           "      allocated = %u;\n"
                           "      list = [",
                           segment->id, segment, segment->allocated);

        while (chunk) {
            memStringCatPrintf(str,
                               "\n"
                               "        MemChunk {\n"
                               "          segment_id = %u;\n"
                               "          address = %p\n"
                               "          size = %u;\n"
                               "          free = %u;\n"
                               "        }",
                               chunk->segment_id, chunk, chunk->size,
                               chunk->free);
            if (chunk->next != NULL) {
                memStringCatPrintf(str, ",");
            }
            chunk = chunk->next;
        }

        memStringCatPrintf(str, "\n    ]");
    }

    memStringCatPrintf(str, "\n  ]\n}");
    pthread_mutex_unlock(&pool->mutex);
    return memStringMove(str);
}

/* Align to 8 bytes */
static unsigned int _memAlign(unsigned int size) {
    static const int alignment = 8;
    return (size + (alignment - 1)) & ~(alignment - 1);
}

static MemSegment *memSegmentAlloc(unsigned int capacity,
                                   unsigned int segment_id) {
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

void memPoolInit(MemPool *pool, unsigned int bytes) {
    const unsigned int segments_array_capacity = 4;

    MemSegment **segments_array = (MemSegment **)malloc(
            sizeof(MemSegment *) * segments_array_capacity);
    if (!segments_array) {
        memPanic("Failed to allocate segments array\n");
    }

    MemSegment *segment = memSegmentAlloc(bytes, pool->segment_count);

    if (!segment) {
        memPanic("Failed to allocate initial segment\n");
    }

    pool->segments_array_capacity = segments_array_capacity;
    pool->segment_capacity = bytes;
    pool->segment_count = 0;
    pool->segments = segments_array;
    pool->segments[pool->segment_count++] = segment;

    pool->error = MEMPOOL_OK;
    pool->deallocate = NULL;

    if (pthread_mutex_init(&pool->mutex, NULL) != 0) {
        memPanic("Failed to initialise Mutex\n");
    }
}

/* How many bytes should initially be in the pool, note the pool will not
 * be able to allocate anything larger than 'bytes' size. Thus allocating
 * something like a dynamically resizable array from this pool makes no sense */
MemPool *memPoolNew(unsigned int bytes) {
    MemPool *pool = (MemPool *)malloc(sizeof(MemPool));
    memPoolInit(pool, bytes);
    return pool;
}

void memPoolSetDeallocate(MemPool *pool, memPoolDeallocate *deallocate) {
    pool->deallocate = deallocate;
}

#define MIN_PAYLOAD_SIZE 8
#define MIN_SPLIT_SIZE   (sizeof(MemChunk) + MIN_PAYLOAD_SIZE)

/* We only take what we need from a block and split bits off it. This can
 * only be called if we are sure that we have enough memory in the given
 * `MemChunk`. This can fail if there is not enough space to allocate
 * the `alloc_size + sizeof(MemChunk)` */
static void memPoolSplitBlock(MemChunk *chunk, unsigned int alloc_size) {
    unsigned int total_needed = alloc_size + sizeof(MemChunk);
    if (chunk->size < total_needed + MIN_SPLIT_SIZE) {
        return;
    }

    /* The remaining size of the block */
    unsigned int new_chunk_size = chunk->size - total_needed;

    MemChunk *new_chunk = (MemChunk *)((unsigned char *)chunk + total_needed);
    new_chunk->size = new_chunk_size;
    new_chunk->free = 1;
    new_chunk->next = chunk->next;
    new_chunk->segment_id = chunk->segment_id;

    chunk->size = alloc_size + sizeof(MemChunk);
    chunk->next = new_chunk;
}

/* Find a segment that has enough memory left in it to allocate from. This
 * highlights why segments should be fairly big - it allows for less scanning
 * to find a suitable segment. */
static MemSegment *memPoolGetSegment(MemPool *pool, unsigned int alloc_size) {
    unsigned int total_size = alloc_size + sizeof(MemChunk);

    for (unsigned int i = 0; i < pool->segment_count; ++i) {
        /* If there is enough space use this segment and increment how many
         * bytes have been allocated from the segment. */
        MemSegment *segment = pool->segments[i];
        if (segment->allocated + total_size <= pool->segment_capacity) {
            return segment;
        }
    }

    unsigned int new_segment_id = pool->segment_count;
    MemSegment *segment = memSegmentAlloc(pool->segment_capacity,
                                          new_segment_id);
    if (segment == NULL) {
        pool->error = MEMPOOL_ALLOCATION_SEGMENT_ALLOC_ERR;
        goto error;
    }

    if (pool->segment_count + 1 >= pool->segments_array_capacity) {
        /* Expand the array */
        unsigned int new_capacity = pool->segments_array_capacity * 2;

        MemSegment **new_segments = (MemSegment **)malloc(sizeof(MemSegment *) *
                                                          (new_capacity));
        if (new_segments == NULL) {
            pool->error = MEMPOOL_ALLOCATION_SEGMENT_ARRAY_ERR;
            free(segment->buffer);
            free(segment);
            goto error;
        }

        /* Copy over the old segments */
        for (unsigned int i = 0; i < new_segment_id; ++i) {
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
void *memPoolAlloc(MemPool *pool, unsigned int size) {
    /* Gotos make controlling the lock easier by only needing to keep track
     * of them in one place */
    pthread_mutex_lock(&pool->mutex);
    if (size == 0)
        goto error;

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
            ptr = (void *)((unsigned char *)cur + sizeof(MemChunk));
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
void *memPoolTryAlloc(MemPool *pool, unsigned int size) {
    void *ptr = memPoolAlloc(pool, size);
    if (ptr == NULL) {
        unsigned int max_allocation_size = pool->segment_capacity -
                sizeof(MemChunk);
        memPanic("Failed to allocate %u bytes, max allocation size = %u - %s\n",
                 size, max_allocation_size, memPoolErrorToString(pool));
    }
    return ptr;
}

static void memSegmentMerge(MemSegment *segment) {
    MemChunk *cur = segment->list;
    while (cur && cur->next) {
        const unsigned char *end = (unsigned char *)cur + sizeof(MemChunk) +
                cur->size;
        if (cur->free && cur->next->free && (unsigned char *)cur->next == end) {
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

        MemChunk *chunk = (MemChunk *)((unsigned char *)ptr - sizeof(MemChunk));
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

        for (unsigned int i = 0; i < pool->segment_count; ++i) {
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
        memPanic("Failed to allocated memory for MemPoolIterator\n");
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

    void *ptr = (void *)((unsigned char *)it->chunk + sizeof(MemChunk));
    it->chunk = it->chunk->next;
    return ptr;
}
