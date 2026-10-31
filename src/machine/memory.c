#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "memory.h"

static void *mem_ctx_stamp(void *raw, unsigned int total);

static ArenaBlock
*arena_block_new(unsigned int capacity)
{
    ArenaBlock *block = (ArenaBlock *)malloc(sizeof(ArenaBlock));
    if (!block) { fprintf(stderr, "out of memory\n"); exit(1); }
    block->capacity = capacity;
    block->used = 0;
    block->next = NULL;
    block->mem = (void *)malloc(capacity);
    if (!block->mem) { fprintf(stderr, "out of memory\n"); exit(1); }
    return block;
}

static void
arena_block_release(ArenaBlock *block)
{
    if (block) {
        /* Need to go to the start address */
        free(((char *)block->mem) - block->used);
        free(block);
    }
}

/* Align to 8 bytes */
static unsigned int
arena_align_memory_size(unsigned int size)
{
    static const int alignment = 8;
    return (size + (alignment - 1)) & ~(alignment - 1);
}

void
arena_init(Arena *arena, unsigned int capacity)
{
    arena->tail = NULL;
    arena->block_capacity = arena_align_memory_size(capacity);
    assert(arena->block_capacity > 0);
    arena->used = 0;
    arena->head = arena_block_new(arena->block_capacity);
}

Arena *
arena_new(unsigned int capacity)
{
    Arena *arena = (Arena *)malloc(sizeof(Arena));
    arena_init(arena, capacity);
    return arena;
}

void *
arena_alloc(Arena *arena, unsigned int size)
{
    /* Allocate aligned memory only */
    unsigned int allocation_size = arena_align_memory_size(size);
    /* Keep a track of how big this is getting */
    arena->used += allocation_size;
    /* If we are allocating something larger than the block allocation_size,
     * then simply allocate it */
    if (allocation_size > arena->block_capacity) {
        /* Do not set `block->used` so `arenaBlockRelease(...)` doesn't touch 
         * arbitrary memory */
        ArenaBlock *block = arena_block_new(allocation_size);
        block->next = arena->tail;
        /* Immediately add to the list of blocks that are used up */
        arena->tail = block;
        return block->mem;
    } else {
        ArenaBlock *block = arena->head;
        /* We accept that `mem` may not be fully used up. In fact it probably 
         * never is. This keeps the implementation fast and simple. */
        if (block->used + allocation_size >= block->capacity) {
            ArenaBlock *new_block = arena_block_new(arena->block_capacity);
            block->next = arena->tail;
            arena->tail = block;
            arena->head = new_block;
            /* Set block to the new block */
            block = new_block;
        }

        void *memory = block->mem;
        block->used += allocation_size;
        block->mem = ((char *)block->mem) + allocation_size;
        return memory;
    }
}

void
arena_clear(Arena *arena)
{
    if (arena) {
        arena_block_release(arena->head);
        ArenaBlock *block = arena->tail;
        ArenaBlock *next = NULL;
        while (block) {
            next = block->next;
            arena_block_release(block);
            block = next;
        }
    }
}

/* Reset for REUSE: free the overflow blocks and rewind the head block
 * to empty. Unlike arenaClear - which is arenaRelease's destructor
 * half and leaves head/tail dangling - the arena is as-new afterwards.
 * The per-message arena pattern: allocate freely, arenaReset between
 * messages, arenaRelease at the end. */
void
arena_reset(Arena *arena)
{
    if (arena == NULL) return;
    ArenaBlock *block = arena->tail;
    while (block) {
        ArenaBlock *next = block->next;
        arena_block_release(block);
        block = next;
    }
    arena->tail = NULL;
    ArenaBlock *head = arena->head;
    head->mem = ((char *)head->mem) - head->used;
    head->used = 0;
    arena->used = 0;
}

void
arena_release(Arena *arena)
{
    if (arena) {
        arena_clear(arena);
        free(arena);
    }
}

int
mem_debug_mode(void)
{
    static int mode = -1;
    if (mode < 0) {
        const char *s = getenv("HCC_MEM_DEBUG");
        mode = (s && *s && strcmp(s, "0") != 0) ? 1 : 0;
    }
    return mode;
}

static void *
mem_debug_alloc(MemCtx *ctx, unsigned int req, unsigned int *_actual)
{
    MemDebugHdr *h = malloc(sizeof(MemDebugHdr) + MEM_POOL_CAP_HDR_SIZE +
                            req);
    if (!h) { fprintf(stderr, "out of memory\n"); exit(1); }
    h->prev = NULL;
    h->next = ctx->debug_live;
    h->owner = ctx;
    if (ctx->debug_live) ctx->debug_live->prev = h;
    ctx->debug_live = h;
    if (_actual) *_actual = req;
    return mem_ctx_stamp((char *)h + sizeof(MemDebugHdr),
                         req + MEM_POOL_CAP_HDR_SIZE);
}

static void
mem_debug_free(MemCtx *ctx, void *ptr)
{
    MemDebugHdr *h = (MemDebugHdr *)((char *)ptr - MEM_POOL_CAP_HDR_SIZE -
                                     sizeof(MemDebugHdr));
    /* Freeing into a context that does not own the allocation is
     * exactly the lifetime bug the arena would paper over. */
    assert(h->owner == ctx);
    if (h->prev) h->prev->next = h->next;
    else         ctx->debug_live = h->next;
    if (h->next) h->next->prev = h->prev;
    free(h);
}

static void
mem_debug_free_all(MemCtx *ctx)
{
    MemDebugHdr *h = ctx->debug_live;
    while (h) {
        MemDebugHdr *next = h->next;
        free(h);
        h = next;
    }
    ctx->debug_live = NULL;
}

void
mem_ctx_init(MemCtx *ctx, unsigned int capacity)
{
    ctx->arena = mem_debug_mode() ? NULL : arena_new(capacity);
    ctx->debug_live = NULL;
    for (int i = 0; i < MEM_POOL_BUCKETS; ++i) {
        ctx->buckets[i] = NULL;
        ctx->hits[i] = 0;
        ctx->misses[i] = 0;
        ctx->releases[i] = 0;
    }
    ctx->bypass_allocs = 0;
}

int
mem_ctx_bucket_idx(unsigned int bucket_size)
{
    return __builtin_ctz(bucket_size) - MEM_POOL_MIN_LOG2;
}

static unsigned int
mem_ctx_bucket_size(unsigned int size)
{
    if (size <= MEM_POOL_MIN_SIZE)
        return MEM_POOL_MIN_SIZE;
    unsigned int v = size - 1;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    return v + 1;
}

/* The TOTAL size stored in the header, including the header itself. */
static unsigned int
mem_ctx_hdr_total(void *ptr)
{
    unsigned int total;
    memcpy(&total, (char *)ptr - MEM_POOL_CAP_HDR_SIZE, sizeof(total));
    return total;
}

/* USABLE capacity of an allocation - what the caller may write. */
unsigned int
mem_ctx_get_buf_capacity(void *ptr)
{
    return mem_ctx_hdr_total(ptr) - MEM_POOL_CAP_HDR_SIZE;
}

/* Stamp the header and hand back the user pointer. */
static void *
mem_ctx_stamp(void *raw, unsigned int total)
{
    memcpy(raw, &total, sizeof(total));
    return (char *)raw + MEM_POOL_CAP_HDR_SIZE;
}

/* For something you want to live for a while and are not going to recycle
 * and/or don't want to be a power of 2 */
void *
mem_ctx_arena_alloc(MemCtx *ctx, unsigned int req)
{
    ctx->bypass_allocs++;
    if (mem_debug_mode()) {
        void *p = malloc(req);
        if (!p) { fprintf(stderr, "out of memory\n"); exit(1); }
        return p;
    }
    return arena_alloc(ctx->arena, req);
}

void *
mem_ctx_alloc(MemCtx *ctx, unsigned int req, unsigned int *_actual)
{
    if (mem_debug_mode())
        return mem_debug_alloc(ctx, req, _actual);
    /* Beyond the largest bucket: straight from the arena, but still
     * headed so free/realloc can inspect it. The stored total is not a
     * power of two, which is how mem_ctx_free knows to leave it. */
    if (req + MEM_POOL_CAP_HDR_SIZE > MEM_POOL_MAX_SIZE) {
        ctx->bypass_allocs++;
        if (_actual) *_actual = req;
        void *raw = arena_alloc(ctx->arena, req + MEM_POOL_CAP_HDR_SIZE);
        return mem_ctx_stamp(raw, req + MEM_POOL_CAP_HDR_SIZE);
    }

    unsigned int bucket = mem_ctx_bucket_size(req + MEM_POOL_CAP_HDR_SIZE);
    int idx = mem_ctx_bucket_idx(bucket);
    MemCtxNode *node = ctx->buckets[idx];
    if (_actual) *_actual = bucket - MEM_POOL_CAP_HDR_SIZE;

    if (node) {
        ctx->buckets[idx] = node->next;
        ctx->hits[idx]++;
        return (void *)node;
    }

    ctx->misses[idx]++;
    return mem_ctx_stamp(arena_alloc(ctx->arena, bucket), bucket);
}

void
mem_ctx_free(MemCtx *ctx, void *ptr)
{
    if (ptr == NULL)
        return;
    if (mem_debug_mode()) {
        mem_debug_free(ctx, ptr);
        return;
    }
    unsigned int total = mem_ctx_hdr_total(ptr);
    /* Bypass allocations carry a non-power-of-two total; they stay in
     * the arena. */
    if (total < MEM_POOL_MIN_SIZE || total > MEM_POOL_MAX_SIZE)
        return;
    if (total != mem_ctx_bucket_size(total))
        return;
    int idx = mem_ctx_bucket_idx(total);
    MemCtxNode *node = (MemCtxNode *)ptr;
    node->next = ctx->buckets[idx];
    ctx->buckets[idx] = node;
    ctx->releases[idx]++;
}

void
mem_ctx_reset(MemCtx *ctx)
{
    if (mem_debug_mode()) mem_debug_free_all(ctx);
    else arena_reset(ctx->arena);
    for (int i = 0; i < MEM_POOL_BUCKETS; ++i) {
        ctx->buckets[i] = NULL;
        ctx->hits[i] = 0;
        ctx->misses[i] = 0;
        ctx->releases[i] = 0;
    }
    ctx->bypass_allocs = 0;
}

void
mem_ctx_release(MemCtx *ctx)
{
    if (mem_debug_mode()) mem_debug_free_all(ctx);
    arena_release(ctx->arena);
    /* The freelists point into the arena that just went away; a
     * released context must be re-initialised before reuse, and empty
     * buckets make a use-after-release fail loudly instead of handing
     * out dangling recycled buffers. */
    for (int i = 0; i < MEM_POOL_BUCKETS; ++i) ctx->buckets[i] = NULL;
    ctx->arena = NULL;
}

void *
mem_ctx_realloc(MemCtx *ctx, void *ptr, unsigned int cap, unsigned int *_actual)
{
    if (ptr == NULL)
        return mem_ctx_alloc(ctx, cap, _actual);
    unsigned int old_usable = mem_ctx_get_buf_capacity(ptr);
    /* Still fits its bucket: nothing to move. */
    if (cap <= old_usable) {
        if (_actual) *_actual = old_usable;
        return ptr;
    }
    void *newp = mem_ctx_alloc(ctx, cap, _actual);
    memcpy(newp, ptr, old_usable);
    mem_ctx_free(ctx, ptr);
    return newp;
}

void
mem_ctx_stats_dump(MemCtx *ctx, const char *name, FILE *fp)
{
    fprintf(fp, "machine mem [%s]%s:\n", name,
            mem_debug_mode() ? " (HCC_MEM_DEBUG: recycling disabled)" : "");
    unsigned long total_hits = 0, total_misses = 0, total_rel = 0;
    for (int i = 0; i < MEM_POOL_BUCKETS; ++i) {
        if (!ctx->hits[i] && !ctx->misses[i] && !ctx->releases[i]) continue;
        fprintf(fp,
                "  %6u B bucket: %8lu hits %8lu misses %8lu frees\n",
                1u << (i + MEM_POOL_MIN_LOG2),
                ctx->hits[i], ctx->misses[i], ctx->releases[i]);
        total_hits += ctx->hits[i];
        total_misses += ctx->misses[i];
        total_rel += ctx->releases[i];
    }
    fprintf(fp, "  totals: %lu hits, %lu misses, %lu frees, %lu bypass\n",
            total_hits, total_misses, total_rel, ctx->bypass_allocs);
    if (ctx->arena) {
        unsigned int blocks = 0;
        for (ArenaBlock *b = ctx->arena->tail; b; b = b->next) blocks++;
        fprintf(fp, "  arena: %u bytes requested, %u block(s) of %u B\n",
                ctx->arena->used, blocks + 1, ctx->arena->block_capacity);
    }
}

MemCtx glbl_mem_ctx;
static int glbl_mem_initted = 0;

void
glbl_mem_init(void)
{
    if (glbl_mem_initted) return;
    glbl_mem_initted = 1;
    mem_ctx_init(&glbl_mem_ctx, 64 * 1024);
}

void *
glbl_mem_alloc(unsigned int cap, unsigned int *_actual)
{
    glbl_mem_init();
    return mem_ctx_alloc(&glbl_mem_ctx, cap, _actual);
}

void *
glbl_mem_calloc(unsigned int cap, unsigned int *_actual)
{
    unsigned int actual;
    void *ptr = glbl_mem_alloc(cap, &actual);
    memset(ptr, 0, actual);
    if (_actual) *_actual = actual;
    return ptr;
}

void
glbl_mem_free(void *ptr)
{
    glbl_mem_init();
    mem_ctx_free(&glbl_mem_ctx, ptr);
}

void *
glbl_mem_realloc(void *ptr, unsigned int cap, unsigned *_actual)
{
    glbl_mem_init();
    return mem_ctx_realloc(&glbl_mem_ctx, ptr, cap, _actual);
}

void
glbl_mem_reset(void)
{
    mem_ctx_reset(&glbl_mem_ctx);
}

void
glbl_mem_stats_dump(FILE *fp)
{
    glbl_mem_init();
    mem_ctx_stats_dump(&glbl_mem_ctx, "global", fp);
}
