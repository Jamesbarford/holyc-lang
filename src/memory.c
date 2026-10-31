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
#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
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

u64 globalArenaBytes(void) {
    if (!global_memory_arena_init) return 0;
    return (u64)global_memory_arena.used;
}

u64 memoryArenaBytes(void) {
    return astArenaBytes() + lexemeArenaBytes() + globalArenaBytes();
}

void memoryRecycleArenas(void) {
    /* Caches first. Both hold arena pointers, and reaching one through
     * a reset arena is a use-after-free, not a leak. */
    lexerForgetMacroProcessor();
    cctrlForgetTokenFiller();

    astMemoryRelease();
    lexemeMemoryRelease();
    /* Also empties the AoStr free-list buckets, whose nodes are
     * addresses inside this arena's blocks. */
    globalArenaRelease();

    astMemoryInit();
    lexemeMemoryInit();
    globalArenaInit(4096 * 10);
}

void globalArenaPrintStats(void) {
    printf("Global Arena Stats\n");
    arenaPrintStats(&global_memory_arena);
}
