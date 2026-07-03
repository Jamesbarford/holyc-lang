#ifndef MEMSAFE_H__
#define MEMSAFE_H__

#include <stdio.h>

#include "jit-common.h"

/* Tracking allocator for JIT'd HolyC.
 *
 * Interposes the HolyC allocator entry points (_MALLOC/Free/CAlloc/
 * ReAlloc) in a jit's host_symbols - which the resolver checks before
 * dlsym - so every allocation user code makes is classified at the
 * Free call: double frees and wild frees are reported with their
 * allocating/freeing call sites and never forwarded, leaving the heap
 * unharmed. macOS malloc's own detection is probabilistic for small
 * blocks (SIGABRT, SIGTRAP or silent corruption, run-dependent); this
 * is deterministic.
 *
 * Used always by the REPL (plus the `ReplHeap;` builtin), and by
 * `hcc -Memsafe -jit`, which adds a leak report at exit. */

/* Register the tracking allocator in `jit`'s host_symbols. Must run
 * before any chunk is compiled - finalize binds call sites then. */
void memsafeInit(HccJit *jit);

/* Attribution tag for reports; the REPL sets its eval round. Rounds
 * <= 0 are omitted from output. */
void memsafeSetRound(int round);

/* Live allocations, biggest first (the `ReplHeap;` builtin). */
void memsafeDumpLive(FILE *f);

/* End-of-run leak summary: silent when nothing is live. */
void memsafeReportLeaks(FILE *f);

#endif
