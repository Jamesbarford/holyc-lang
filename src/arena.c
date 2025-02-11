#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "aostr.h"
#include "arena.h"

static ArenaBlock *arenaBlockNew(unsigned int capacity) {
    ArenaBlock *block = (ArenaBlock *)malloc(sizeof(ArenaBlock));
    block->capacity = capacity;
    block->used = 0;
    block->mem = (void *)malloc(capacity);
    return block;
}

static void arenaBlockRelease(ArenaBlock *block) {
    if (block) {
        /* Need to go to the start address */
        free(block->mem - block->used);
        free(block);
    }
}

/* Align to 8 bytes */
static unsigned int arenaAlignMemorySize(unsigned int size) {
    static const int alignment = 8;
    return (size + (alignment - 1)) & ~(alignment - 1);
}

void arenaInit(Arena *arena, unsigned int capacity) {
    arena->tail = NULL;
    arena->block_capacity = arenaAlignMemorySize(capacity);
    assert(arena->block_capacity > 0);
    arena->used = 0;
    arena->head = arenaBlockNew(arena->block_capacity);
}

Arena *arenaNew(unsigned int capacity) {
    Arena *arena = (Arena *)malloc(sizeof(Arena));
    arenaInit(arena, capacity);
    return arena;
}

void *arenaAlloc(Arena *arena, unsigned int size) {
    /* Allocate aligned memory only */
    unsigned int allocation_size = arenaAlignMemorySize(size);
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
    }

    ArenaBlock *block = arena->head;
    /* We accept that `mem` may not be fully used up. In fact it probably
     * never is. This keeps the implementation fast and simple. */
    if (block->used + allocation_size >= block->capacity) {
        ArenaBlock *new_block = arenaBlockNew(arena->block_capacity);
        block->next = arena->tail;
        arena->tail = block;
        arena->head = new_block;
        void *memory = new_block->mem;
        new_block->used = allocation_size;
        new_block->mem += allocation_size;
        return memory;
    }

    /* The simple path! */
    void *memory = block->mem;
    block->used += allocation_size;
    block->mem += allocation_size;
    return memory;
}

void arenaClear(const Arena *arena) {
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

void arenaPrintStats(const Arena *arena) {
    unsigned int total_blocks = 0;
    unsigned int total_capacity = 0;
    unsigned int total_used = 0;
    unsigned int big_allocs = 0;
    unsigned int total_non_full_blocks = 0;
    unsigned int total_diff = 0;

    if (arena->head) {
        total_blocks++;
        total_used += arena->head->used;
        total_capacity += arena->head->capacity;
    }

    const ArenaBlock *block = arena->tail;
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

    unsigned int average_unused_amount = total_diff == 0 ||
                    total_non_full_blocks == 0 ?
            0 :
            total_diff / total_non_full_blocks;

    const aoStr *total_mem = aoStrIntToHumanReadableBytes((long)total_capacity);
    const aoStr *total_allocated = aoStrIntToHumanReadableBytes(
            (long)total_used);
    const aoStr *per_block_capacity = aoStrIntToHumanReadableBytes(
            (long)arena->block_capacity);

    printf("Total Memory: %s (%u)\n"
           "Total Allocated: %s (%u)\n"
           "Per Block capacity: %s (%u)\n"
           "Total blocks: %u\n"
           "Total Big Blocks: %u\n"
           "Total non full blocks: %u\n"
           "Average unused amount: %u\n",
           total_mem->data, total_capacity, total_allocated->data, total_used,
           per_block_capacity->data, arena->block_capacity,

           total_blocks, big_allocs, total_non_full_blocks,
           average_unused_amount);
}
