#ifndef MEMORY_ARENA_H__
#define MEMORY_ARENA_H__

typedef struct ArenaBlock ArenaBlock;
typedef struct ArenaBlock {
    unsigned int capacity;
    unsigned int used;
    void *mem;
    ArenaBlock *next;
} ArenaBlock;

typedef struct Arena {
    unsigned int block_capacity;
    unsigned int used;
    ArenaBlock *head; /* Active block */
    ArenaBlock *tail; /* Used blocks */
} Arena;

void arenaInit(Arena *arena, unsigned int capacity);
void arenaClear(const Arena *arena);
Arena *arenaNew(unsigned int capacity);
void *arenaAlloc(Arena *arena, unsigned int size);
void arenaRelease(Arena *arena);
void arenaPrintStats(const Arena *arena);

#endif // MEMORY_ARENA_H__
