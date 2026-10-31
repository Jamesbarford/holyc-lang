#ifndef __MACHINE_MEMORY_H
#define __MACHINE_MEMORY_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* The machine layer's allocator: a bump arena fronted by power-of-2
 * size-class freelists. Nothing here is ever returned to the system
 * until the owning context is released - most machine-layer memory
 * lives for the lifetime of whatever is happening (a parse, an
 * encoder, the whole compile) - but buffers freed with mem_ctx_free /
 * xfree recycle through the buckets, so grow-by-doubling patterns and
 * short-lived scratch stop accreting.
 *
 * Every allocation carries a fat-pointer header holding its TOTAL
 * bucket size, so free and realloc need no size argument. Requests
 * beyond the largest bucket are served straight from the arena with a
 * header whose size is not a power of two - mem_ctx_free recognises
 * that and simply leaves them to the arena. */
#define MEM_POOL_MIN_LOG2 5  /* 32 bytes minimum bucket */
#define MEM_POOL_MAX_LOG2 16 /* 64KB maximum bucket; anything larger bypasses */
#define MEM_POOL_BUCKETS  (MEM_POOL_MAX_LOG2 - MEM_POOL_MIN_LOG2 + 1)
#define MEM_POOL_MIN_SIZE (1u << MEM_POOL_MIN_LOG2)
#define MEM_POOL_MAX_SIZE (1u << MEM_POOL_MAX_LOG2)
/* Header before every user pointer. 8 bytes even though the size fits
 * in 4: the arena hands out 8-aligned chunks, and the user pointer
 * sits directly after the header, so a smaller header would misalign
 * every struct that contains a pointer or a double. */
#define MEM_POOL_CAP_HDR_SIZE 8u

typedef struct ArenaBlock {
    unsigned int capacity;
    unsigned int used;
    void *mem;
    struct ArenaBlock *next; 
} ArenaBlock; 

typedef struct Arena {
    unsigned int block_capacity;
    unsigned int used;
    ArenaBlock *head; /* Active block */
    ArenaBlock *tail; /* Used blocks */
} Arena;

typedef struct MemCtxNode {
    struct MemCtxNode *next;
} MemCtxNode;

/* HCC_MEM_DEBUG mode: every allocation is an individual malloc carrying
 * this prefix (then the capacity header, then the user bytes), linked
 * into its owning context so wholesale release still works and a free
 * into the WRONG context asserts instead of corrupting silently. The
 * point of the mode is to hand real allocations to the platform's
 * tools - ASan, leaks, guard malloc - which the arena hides. */
typedef struct MemDebugHdr {
    struct MemDebugHdr *prev;
    struct MemDebugHdr *next;
    struct MemCtx *owner;
} MemDebugHdr;

typedef struct MemCtx {
    Arena *arena;               /* NULL in HCC_MEM_DEBUG mode */
    MemCtxNode *buckets[MEM_POOL_BUCKETS];
    MemDebugHdr *debug_live;    /* HCC_MEM_DEBUG: all live allocations */
    unsigned long hits[MEM_POOL_BUCKETS];
    unsigned long misses[MEM_POOL_BUCKETS];
    unsigned long releases[MEM_POOL_BUCKETS];
    unsigned long bypass_allocs;
} MemCtx;

/* 1 when HCC_MEM_DEBUG is set in the environment (read once). */
int mem_debug_mode(void);

void arena_init(Arena *arena, unsigned int capacity);
Arena *arena_new(unsigned int capacity);
void *arena_alloc(Arena *arena, unsigned int size);
void arena_clear(Arena *arena);
void arena_reset(Arena *arena);
void arena_release(Arena *arena);

void mem_ctx_init(MemCtx *ctx, unsigned int capacity);
int mem_ctx_bucket_idx(unsigned int bucket_size);
unsigned int mem_ctx_get_buf_capacity(void *ptr);
void *mem_ctx_arena_alloc(MemCtx *ctx, unsigned int req);

void *mem_ctx_alloc(MemCtx *ctx, unsigned int req, unsigned int *_actual);
void mem_ctx_free(MemCtx *ctx, void *ptr);
void mem_ctx_reset(MemCtx *ctx);
void mem_ctx_release(MemCtx *ctx);
void *mem_ctx_realloc(MemCtx *ctx, void *ptr, unsigned int cap, unsigned int *_actual);

/* Per-bucket hit/miss/recycle counters plus arena footprint, for
 * `--mem-stats`. `name` labels the context in the output. */
void mem_ctx_stats_dump(MemCtx *ctx, const char *name, FILE *fp);
void glbl_mem_stats_dump(FILE *fp);

void glbl_mem_init(void);
void *glbl_mem_alloc(unsigned int cap, unsigned int *_actual);
void *glbl_mem_calloc(unsigned int cap, unsigned int *_actual);
void glbl_mem_free(void *ptr);
void *glbl_mem_realloc(void *ptr, unsigned int cap, unsigned int *_actual);
void glbl_mem_reset(void);

#endif
