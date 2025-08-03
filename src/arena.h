#ifndef MEMORY_ARENA_H__
#define MEMORY_ARENA_H__

#include "types.h"

typedef struct ArenaBlock ArenaBlock;
typedef struct ArenaBlock {
    u32 capacity;
    u32 used;
    void *mem;
    ArenaBlock *next; 
} ArenaBlock; 

typedef struct Arena {
    u32 block_capacity;
    u32 used;
    ArenaBlock *head; /* Active block */
    ArenaBlock *tail; /* Used blocks */
} Arena;

void arenaInit(Arena *arena, u32 capacity);
void arenaClear(Arena *arena);
Arena *arenaNew(u32 capacity);
void *arenaAlloc(Arena *arena, u32 size);
void arenaRelease(Arena *arena);
void arenaPrintStats(Arena *arena);

#endif // MEMORY_ARENA_H__
