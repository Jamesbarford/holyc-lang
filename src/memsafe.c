/* Tracking allocator for JIT'd HolyC. Disabled by default on the JIT,
 * enabled by default with the repl.
 *
 * MAlloc/Free/CAlloc/ReAlloc normally live in the libtos dylib, whose
 * INTERNAL libc calls never pass the JIT resolver - so interposition
 * happens at the HolyC entry points. The wrappers ARE the
 * implementation rather than forwarding to the dylib: that keeps them
 * independent of the installed libtos (a stale or missing build for
 * the running arch would otherwise silently disable tracking). The
 * layout is just a u64 size header at [p-8] (memory.HC), so MSize and
 * dylib-internal frees interoperate with blocks made here, and vice
 * versa. The tracked pointer is the payload - exactly what HolyC code
 * holds. */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#if defined(__APPLE__)
#include <malloc/malloc.h> /* malloc_zone_from_ptr */
#endif

#include "containers.h"
#include "jit-common.h"
#include "memsafe.h"

typedef struct MemsafeEntry {
    void *ptr;
    u64 size;
    int alloc_round;
    int free_round;
    /* Raw return addresses into JIT code; symbolised lazily so the
     * alloc hot path never scans the symbol table. */
    uintptr_t alloc_site[2];
    uintptr_t free_site[2];
} MemsafeEntry;

static HccJit *memsafe_jit = NULL;
static Map *memsafe_live = NULL; /* (u64)ptr -> MemsafeEntry* */
static int memsafe_round = 0;    /* attribution tag; <= 0 omitted */

/* Recently-freed ring: bounds how long a double free stays
 * classifiable. Beyond it the pointer falls through to the zone
 * check, i.e. untracked behaviour. */
#define MEMSAFE_RING 256
static MemsafeEntry memsafe_ring[MEMSAFE_RING];
static int memsafe_ring_next = 0;

void memsafeSetRound(int round) {
    memsafe_round = round;
}

/* First `max` return addresses landing in JIT code, walking this
 * (healthy) C call stack: wrapper <- _FREE/_MALLOC (JIT) <- user fn
 * (JIT) <- entry (C, stops matching). */
static int memsafeCallSites(uintptr_t *out, int max) {
    HccJit *jit = memsafe_jit;
    if (jit == NULL) return 0;
    int n = 0;
    uintptr_t fp = (uintptr_t)__builtin_frame_address(0);
    uintptr_t prev = 0;
    for (int depth = 0; depth < 32 && fp && n < max; ++depth) {
        if (fp & 7) break;
        if (prev && fp <= prev) break;
        uintptr_t ret = ((uintptr_t *)fp)[1];
        uintptr_t next = ((uintptr_t *)fp)[0];
        if (ret && hccJitFindChunk(jit, (void *)ret)) out[n++] = ret;
        prev = fp;
        fp = next;
    }
    return n;
}

/* The stdlib shims (`_MALLOC`, `Free`, `ReAlloc`, ...) sit between the
 * wrapper and the user's code, so skip them when naming a site. */
static int memsafeIsAllocShim(const char *sym) {
    while (*sym == '_') sym++;
    return strcasecmp(sym, "malloc") == 0 ||
           strcasecmp(sym, "free") == 0 ||
           strcasecmp(sym, "calloc") == 0 ||
           strcasecmp(sym, "realloc") == 0 ||
           strcasecmp(sym, "mallocident") == 0 ||
           strcasecmp(sym, "msize") == 0;
}

static void memsafePrintSite(FILE *f, const uintptr_t *sites, int nsites) {
    HccJit *jit = memsafe_jit;
    for (int i = 0; i < nsites; ++i) {
        if (sites[i] == 0) continue;
        size_t off = 0;
        const char *sym = jit
            ? hccJitFindSymbolForAddr(jit, (void *)sites[i], &off)
            : NULL;
        if (sym && memsafeIsAllocShim(sym)) continue;
        if (sym) {
            /* sites[] are return addresses - the CALL is the
             * instruction before, so look the line up at addr-1 or
             * the report reads one statement late. */
            int line = hccJitFindLineForAddr(jit, (void *)(sites[i] - 1));
            fprintf(f, "in `%s`+%zu", sym, off);
            if (line) fprintf(f, ", line %d", line);
        } else {
            fprintf(f, "at %p", (void *)sites[i]);
        }
        return;
    }
    fprintf(f, "at an unknown site");
}

/* "round N " when rounds are in play (the REPL); nothing otherwise. */
static void memsafePrintRound(FILE *f, int round) {
    if (round > 0) fprintf(f, "round %d ", round);
}

static void memsafeTrack(void *p, u64 size) {
    if (memsafe_live == NULL || p == NULL) return;
    u64 key = (u64)(uintptr_t)p;
    /* A stale entry can exist if host code freed this pointer behind
     * our back and libc has now reused the address so replace it. */
    MemsafeEntry *old = (MemsafeEntry *)mapGetInt(memsafe_live, key);
    if (old) {
        mapRemoveInt(memsafe_live, key);
        free(old);
    }
    MemsafeEntry *e = (MemsafeEntry *)calloc(1, sizeof(*e));
    if (e == NULL) return;
    e->ptr = p;
    e->size = size;
    e->alloc_round = memsafe_round;
    memsafeCallSites(e->alloc_site, 2);
    mapAddIntOrErr(memsafe_live, key, e);
}

static MemsafeEntry *memsafeRingFind(void *p) {
    for (int i = 0; i < MEMSAFE_RING; ++i)
        if (memsafe_ring[i].ptr == p) return &memsafe_ring[i];
    return NULL;
}

/* Untrack a live pointer, recording it in the recently-freed ring.
 * Returns 1 if it was live. */
static int memsafeRetire(void *p) {
    if (memsafe_live == NULL || p == NULL) return 0;
    u64 key = (u64)(uintptr_t)p;
    MemsafeEntry *e = (MemsafeEntry *)mapGetInt(memsafe_live, key);
    if (e == NULL) return 0;
    mapRemoveInt(memsafe_live, key);
    e->free_round = memsafe_round;
    memset(e->free_site, 0, sizeof(e->free_site));
    memsafeCallSites(e->free_site, 2);
    memsafe_ring[memsafe_ring_next] = *e;
    memsafe_ring_next = (memsafe_ring_next + 1) % MEMSAFE_RING;
    free(e);
    return 1;
}

static void memsafeReportDoubleFree(void *p, MemsafeEntry *r) {
    uintptr_t here[2] = { 0, 0 };
    memsafeCallSites(here, 2);
    fprintf(stderr, "Free: double free of %p (%llu bytes)\n",
                    p, (unsigned long long)r->size);
    fprintf(stderr, "  allocated   ");
    memsafePrintRound(stderr, r->alloc_round);
    memsafePrintSite(stderr, r->alloc_site, 2);
    fprintf(stderr, "\n  first freed ");
    memsafePrintRound(stderr, r->free_round);
    memsafePrintSite(stderr, r->free_site, 2);
    fprintf(stderr, "\n  this free   ");
    memsafePrintRound(stderr, memsafe_round);
    memsafePrintSite(stderr, here, 2);
    fprintf(stderr, " - ignored, heap unharmed\n");
    fflush(stderr);
}

/* Linux shouts there is an unused function */
#if defined(__APPLE__)
static void memsafeReportWildFree(void *p) {
    uintptr_t here[2] = { 0, 0 };
    memsafeCallSites(here, 2);
    fprintf(stderr, "Free: %p was never heap-allocated "
            "(stack, global or interior pointer?) ", p);
    memsafePrintSite(stderr, here, 2);
    fprintf(stderr, " - ignored\n");
    fflush(stderr);
}
#endif

/* ---------------- the MAlloc-ABI wrappers ---------------- */

static void *memsafeMAlloc(u64 size) {
    u8 *base = (u8 *)malloc(size + 8);
    if (base == NULL) return NULL;
    *(u64 *)base = size;
    void *p = base + 8;
    memsafeTrack(p, size);
    return p;
}

static void *memsafeCAlloc(u64 size) {
    void *p = memsafeMAlloc(size);
    if (p) memset(p, 0, size);
    return p;
}

static void memsafeFree(void *p) {
    if (p == NULL) return;
    if (memsafeRetire(p)) {
        free((u8 *)p - 8);
        return;
    }
    MemsafeEntry *r = memsafeRingFind(p);
    if (r) {
        memsafeReportDoubleFree(p, r);
        return;
    }
#if defined(__APPLE__)
    /* Unknown pointer: probe the header address. Owned by a zone allocated by
     * MAlloc inside libtos/host code we can't see; the layout is identical, so
     * free the base. */
    if (malloc_zone_from_ptr((uint8_t *)p - 8)) {
        free((u8 *)p - 8);
        return;
    }
    memsafeReportWildFree(p);
#else
    /* No safe way to probe an arbitrary pointer on Linux. */
    free((u8 *)p - 8);
#endif
}

static void *memsafeReAlloc(void *p, u64 size) {
    if (p == NULL) return memsafeMAlloc(size);
    /* Refuse before touching the old block: a freed block's header
     * (and bytes) are not ours to read any more. */
    MemsafeEntry *r = memsafeRingFind(p);
    if (r != NULL) {
        memsafeReportDoubleFree(p, r);
        return NULL;
    }
    void *q = memsafeMAlloc(size);
    if (q == NULL) return NULL;
    u64 old_size = *(u64 *)((u8 *)p - 8);
    memcpy(q, p, old_size < size ? old_size : size);
    memsafeFree(p);
    return q;
}

static int memsafeCmpSize(const void *a, const void *b) {
    const MemsafeEntry *ea = *(MemsafeEntry *const *)a;
    const MemsafeEntry *eb = *(MemsafeEntry *const *)b;
    if (ea->size != eb->size) return eb->size > ea->size ? 1 : -1;
    return 0;
}

/* Fill a Vec with the live entries (caller vecRelease's it - the vec
 * type's release is a no-op, entries stay owned by the map). */
static Vec *memsafeCollect(u64 *out_total) {
    Vec *v = vecNew(&vec_unsigned_long_type);
    u64 total = 0;
    MapIter mi;
    mapIterInit(memsafe_live, &mi);
    while (mapIterNext(&mi)) {
        MemsafeEntry *e = (MemsafeEntry *)mi.node->value;
        total += e->size;
        vecPush(v, e);
    }
    if (out_total) *out_total = total;
    return v;
}

static void memsafeList(FILE *f, Vec *v) {
    qsort(v->entries, v->size, sizeof(void *), memsafeCmpSize);
    u64 shown = v->size < 20 ? v->size : 20;
    for (u64 i = 0; i < shown; ++i) {
        MemsafeEntry *e = vecGet(MemsafeEntry *, v, i);
        fprintf(f, "  %p  %8llu bytes  ", e->ptr,
                (unsigned long long)e->size);
        memsafePrintRound(f, e->alloc_round);
        memsafePrintSite(f, e->alloc_site, 2);
        fprintf(f, "\n");
    }
    if (v->size > shown)
        fprintf(f, "  ... %llu more\n",
                (unsigned long long)(v->size - shown));
}

void memsafeDumpLive(FILE *f) {
    if (memsafe_live == NULL) return;
    u64 total = 0;
    Vec *v = memsafeCollect(&total);
    fprintf(f, "heap: %llu live allocation(s), %llu bytes\n",
            (unsigned long long)v->size, (unsigned long long)total);
    memsafeList(f, v);
    vecRelease(v);
    fflush(f);
}

void memsafeReportLeaks(FILE *f) {
    if (memsafe_live == NULL || memsafe_live->size == 0) return;
    /* The program's buffered stdout should land before the report. */
    fflush(stdout);
    u64 total = 0;
    Vec *v = memsafeCollect(&total);
    fprintf(f, "memsafe: %llu allocation(s) (%llu bytes) never freed\n",
            (unsigned long long)v->size, (unsigned long long)total);
    memsafeList(f, v);
    vecRelease(v);
    fflush(f);
}

void memsafeInit(HccJit *jit) {
    if (jit == NULL) return;
    memsafe_jit = jit;
    if (memsafe_live == NULL)
        memsafe_live = mapNew(64, &map_uint_to_uint_type);
    /* host_symbols wins over dlsym, so these shadow the libtos dylib's
     * implementations at every JIT'd call site. Both the asm-label
     * spellings (_MALLOC on x86_64) and the compiled-HolyC ones (Free
     * on aarch64) are covered; register before any chunk compiles. */
    hccJitDefineSymbol(jit, "_MALLOC",  (void *)(uintptr_t)memsafeMAlloc);
    hccJitDefineSymbol(jit, "_CALLOC",  (void *)(uintptr_t)memsafeCAlloc);
    hccJitDefineSymbol(jit, "_REALLOC", (void *)(uintptr_t)memsafeReAlloc);
    hccJitDefineSymbol(jit, "_FREE",    (void *)(uintptr_t)memsafeFree);
    hccJitDefineSymbol(jit, "Free",     (void *)(uintptr_t)memsafeFree);
    hccJitDefineSymbol(jit, "CAlloc",   (void *)(uintptr_t)memsafeCAlloc);
    hccJitDefineSymbol(jit, "ReAlloc",  (void *)(uintptr_t)memsafeReAlloc);
}
