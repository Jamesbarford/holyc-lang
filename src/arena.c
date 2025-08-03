#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "aostr.h"
#include "arena.h"

static ArenaBlock *arenaBlockNew(u32 capacity) {
    ArenaBlock *block = (ArenaBlock *)malloc(sizeof(ArenaBlock));
    block->capacity = capacity;
    block->used = 0;
    block->mem = (void *)malloc(capacity);
    return block;
}

static void arenaBlockRelease(ArenaBlock *block) {
    if (block) {
        /* Need to go to the start address */
        free(((char *)block->mem) - block->used);
        free(block);
    }
}

/* Align to 8 bytes */
static u32 arenaAlignMemorySize(u32 size) {
    static const int alignment = 8;
    return (size + (alignment - 1)) & ~(alignment - 1);
}

void arenaInit(Arena *arena, u32 capacity) {
    arena->tail = NULL;
    arena->block_capacity = arenaAlignMemorySize(capacity);
    assert(arena->block_capacity > 0);
    arena->used = 0;
    arena->head = arenaBlockNew(arena->block_capacity);
}

Arena *arenaNew(u32 capacity) {
    Arena *arena = (Arena *)malloc(sizeof(Arena));
    arenaInit(arena, capacity);
    return arena;
}

void *arenaAlloc(Arena *arena, u32 size) {
    /* Allocate aligned memory only */
    u32 allocation_size = arenaAlignMemorySize(size);
    /* Keep a track of how big this is getting */
    arena->used += allocation_size;
    /* If we are allocating something larger than the block allocation_size,
     * then simply allocate it */
    if (allocation_size > arena->block_capacity) {
        /* Do not set `block->used` so `arenaBlockRelease(...)` doesn't touch 
         * arbitrary memory */
        ArenaBlock *block = arenaBlockNew(allocation_size);
        block->next = arena->tail;
        /* Immediately add to the list of blocks that are used up */
        arena->tail = block;
        return block->mem;
    } else {
        ArenaBlock *block = arena->head;
        /* We accept that `mem` may not be fully used up. In fact it probably 
         * never is. This keeps the implementation fast and simple. */
        if (block->used + allocation_size >= block->capacity) {
            ArenaBlock *new_block = arenaBlockNew(arena->block_capacity);
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


void arenaClear(Arena *arena) {
    if (arena) {
        arenaBlockRelease(arena->head);
        ArenaBlock *block = arena->tail;
        ArenaBlock *next = NULL;
        while (block) {
            next = block->next;
            arenaBlockRelease(block);
            block = next;
        }
    }
}

void arenaRelease(Arena *arena) {
    if (arena) {
        arenaClear(arena);
        free(arena);
    }
}

void arenaPrintStats(Arena *arena) {
    u32 total_blocks = 0;
    u32 total_capacity = 0;
    u32 total_used = 0;
    u32 big_allocs = 0;
    u32 total_non_full_blocks = 0;
    u32 total_diff = 0;

    if (arena->head) {
        total_blocks++;
        total_used += arena->head->used;
        total_capacity += arena->head->capacity;
    }

    ArenaBlock *block = arena->tail;
    while (block) {
        total_capacity += block->capacity;
        if (block->used == 0 && block->capacity > arena->block_capacity) {
            big_allocs++;
            total_used += block->capacity;
        } else {
            total_blocks++;
            total_used += block->used;
            if (block->used < block->capacity) {
                total_diff += block->capacity - block->used;
                total_non_full_blocks++;
            }
        }
        block = block->next;
    }

    u32 average_unused_amount = total_diff == 0 || total_non_full_blocks == 0 ? 0 : total_diff / total_non_full_blocks;

    AoStr *total_mem = aoStrIntToHumanReadableBytes((long) total_capacity);
    AoStr *total_allocated = aoStrIntToHumanReadableBytes((long)total_used);
    AoStr *per_block_capacity = aoStrIntToHumanReadableBytes((long)arena->block_capacity);

    printf("Total Memory: %s (%u)\n"
           "Total Allocated: %s (%u)\n"
           "Per Block capacity: %s (%u)\n"
           "Total blocks: %u\n"
           "Total Big Blocks: %u\n"
           "Total non full blocks: %u\n"
           "Average unused amount: %u\n",
           total_mem->data, total_capacity,
           total_allocated->data, total_used,
           per_block_capacity->data, arena->block_capacity,

           total_blocks,
           big_allocs,
           total_non_full_blocks,
           average_unused_amount);
}
