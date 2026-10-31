#ifndef MEMORY_H__
#define MEMORY_H__

#include "types.h"

void globalArenaInit(u32 capacity);
void *globalArenaAllocate(u32 size);
void globalArenaRelease(void);
void globalArenaPrintStats(void);
/* Bytes the global arena has handed out since (re)init. */
u64 globalArenaBytes(void);

/* Total bytes across every compiler arena (AST, AST types, lexemes,
 * global/string). Nothing is ever returned to an arena, so this only
 * grows - it is how a long-lived process measures its own garbage. */
u64 memoryArenaBytes(void);

/* Throw away every arena and start over, as if the process had just
 * booted. ALL arena-allocated memory (every Ast, AstType, Lexeme and
 * AoStr) is invalid afterwards, so the caller must have dropped every
 * pointer into them - including any Cctrl. See `hcc -lsp`, the only
 * caller: it is a server that re-parses forever and has to bound the
 * garbage somehow. */
void memoryRecycleArenas(void);

#endif
