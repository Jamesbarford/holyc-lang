/**
 * The implementation of the pool below is very simplistic. It can only allocate
 * one type of object as you give it a fixed size when creating it.
 *
 * Free objects are handled by a unique integer id (that increments) being 
 * pushed to a vector.
 */
#include <stdlib.h>

#include "aostr.h"
#include "arena.h"
#include "memory.h"
#include "util.h"

static Arena global_memory_arena;
static int global_memory_arena_init = 0;

void globalArenaInit(u32 capcity) {
    if (!global_memory_arena_init) {
        arenaInit(&global_memory_arena, capcity);
        global_memory_arena_init = 1;
    }
}

void *globalArenaAllocate(u32 size) {
    return (void *)arenaAlloc(&global_memory_arena, size);
}

void globalArenaRelease(void) {
    if (global_memory_arena_init) {
        /* Pool entries point into arena blocksm clear the pool first so we
         * don't leave dangling next pointers behind. */
        aoStrPoolReset();
        arenaClear(&global_memory_arena);
        global_memory_arena_init = 0;
    }
}

void globalArenaPrintStats(void) {
    printf("Global Arena Stats\n");
    arenaPrintStats(&global_memory_arena);
}
