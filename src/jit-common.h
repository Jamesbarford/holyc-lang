#ifndef JIT_COMMON_H__
#define JIT_COMMON_H__

/* Arch-independent machinery shared by the aarch64 and x86_64 JIT
 * backends: JIT state, label-number bookkeeping, fixup helpers, symbol
 * resolution, globals/string-literal layout, phi-materialise ordering,
 * and the compile-pipeline driver. The per-arch files own only the
 * instruction selection and emission.
 *
 * Build-gated: only compiled when HCC_ENABLE_JIT is defined. */

#include <stdint.h>

#include "ast.h"
#include "cctrl.h"
#include "containers.h"
#include "ir.h"
#include "asm/asm_enc.h"
#include "asm/asm_jit.h"

typedef struct HccJit {
    Cctrl *cc;

    /* The arch backend driving compile_function for every chunk. */
    const struct HccJitBackend *backend;

    /* Per-chunk accumulating encoder. All function bytes of the chunk
     * being built land here in emission order; public labels carry the
     * function name. Reset at the start of every hccJitCompileChunk. */
    AsmEnc enc;

    /* name -> entry address inside an RX mapping. Accumulates across
     * chunks; redefinitions overwrite so lookups see the newest one. */
    Map *symbols;
    /* User-defined host symbols (printf etc.) - checked before dlsym.
     * Finalized functions and globals/string arenas are registered
     * here too so later chunks can address them. */
    Map *host_symbols;

    /* Function names defined in the chunk CURRENTLY being emitted.
     * These route through label fixups; everything else resolves via
     * host_symbols/dlsym. Reset per chunk. */
    Map *chunk_fns;

    /* Finalized RX mappings (AsmJitCode *), one per chunk. Old chunks
     * stay mapped forever - earlier code may hold pointers into them. */
    List *chunks;

    /* RW arenas for globals + string literals, one per chunk; the
     * absolute addresses are registered in host_symbols. */
    List *globals_arenas;

    /* Incremental cursors: last node of cc->ast_list / cc->asm_blocks
     * already consumed by a chunk. A chunk compiles (cursor, sentinel]. */
    List *ast_cursor;
    List *asm_cursor;

    /* Per-IrBlock -> AsmEnc local_num.  Key = (fn_uuid<<32) | block_id;
     * value = small int.  Local nums are pulled from `next_local`. */
    Map *block_local;
    /* Per-function epilogue label local_num. Key = fn_uuid. */
    Map *epi_local;
    int  next_local;

    /* pc -> source line. Backends note (enc offset, line) as they emit
     * (pending_lines); hccJitCompileChunk rebases the offsets onto the
     * finalized chunk's address and appends to `lines`. Consulted only
     * when reporting (faults, memsafe), so a linear scan is fine. */
    struct HccJitLine *lines;
    u32 n_lines, cap_lines;
    struct HccJitLine *pending_lines;
    u32 n_pending, cap_pending;
} HccJit;

typedef struct HccJitLine {
    uintptr_t addr; /* absolute pc in `lines`; enc offset in pending */
    int line;
} HccJitLine;

/* What a per-arch backend plugs into the shared compile pipeline. */
typedef struct HccJitBackend {
    /* For diagnostics ("aarch64", "x86_64"). */
    const char *name;
    /* Is this Cctrl target compatible with the backend? */
    int (*target_ok)(enum CliTarget target);
    /* Install the backend's IrRegPool (idempotent). */
    void (*init_reg_pool)(void);
    /* Lower + emit one function into jit->enc. Non-zero on error. */
    int (*compile_function)(HccJit *jit, Ast *ast, IrCtx *ir_ctx);
} HccJitBackend;

/* Run the full front-end -> IR -> native bytes -> RX mapping pipeline
 * for every function in `cc` through `backend`. NULL on error. */
HccJit *hccJitCompile(Cctrl *cc, const HccJitBackend *backend);

/* Create an empty jit (no code compiled yet) for incremental use.
 * Checks target compatibility, installs the register pool and loads
 * libtos. NULL on error. */
HccJit *hccJitNew(Cctrl *cc, const HccJitBackend *backend);

/* Compile everything appended to cc->ast_list / cc->asm_blocks since
 * the previous chunk - plus `extra_fn` if non-NULL (a synthetic
 * function that is NOT in ast_list, e.g. the REPL's per-input
 * wrapper) - into a fresh RX mapping. New globals and string literals
 * get slots in a new RW arena. Finalized function addresses are
 * registered so later chunks (and hccJitLookup) can reach them;
 * redefinitions rebind the name for future chunks while old code
 * keeps calling the address it was linked against.
 *
 * The cursors advance even on failure so a broken input isn't
 * recompiled by the next call. Non-zero on error. */
int hccJitCompileChunk(HccJit *jit, Ast *extra_fn);

/* Look up a public function by name (tries the underscore-prefixed
 * macOS mangling too). The returned pointer is owned by the jit and
 * remains valid until hccJitFree. */
void *hccJitLookup(HccJit *jit, const char *name);

/* Reverse-map a pc: the chunk whose live executable bytes (code +
 * veneers) contain it, or NULL if it isn't JIT'd code. */
AsmJitCode *hccJitFindChunk(HccJit *jit, void *pc);

/* Name of the JIT function containing `pc`: the nearest symbol at or
 * below it within its chunk's code (veneer pcs return NULL - use
 * hccJitFindChunk to tell "veneer" from "not ours"). *off_out gets
 * pc - symbol. */
const char *hccJitFindSymbolForAddr(HccJit *jit, void *pc, size_t *off_out);

/* Record "code emitted from enc offset jit->enc.len onward is source
 * line `line`" - called by the backends per IR instruction; consecutive
 * duplicates collapse. line 0 (synthetic) is ignored. */
void hccJitNoteLine(HccJit *jit, int line);

/* Source line for a pc inside JIT code, 0 when unknown. */
int hccJitFindLineForAddr(HccJit *jit, void *pc);

/* Register a host-provided symbol (e.g. printf, malloc, a libtos hook)
 * so JITted code can call it. Overrides dlsym for that name. */
void hccJitDefineSymbol(HccJit *jit, const char *name, void *addr);

/* Convenience: look up `main` and invoke it as `int main(int, char **)`,
 * returning its exit code. */
int hccJitRunMain(HccJit *jit, int argc, char **argv);

void hccJitFree(HccJit *jit);

/* ---- helpers for the per-arch emitters ---- */

/* Stable AsmEnc local label numbers for (fn, block) / fn epilogue, and
 * fresh anonymous ones for shim labels. */
int hccJitBlockLocalNum(HccJit *jit, IrFunction *fn, IrBlock *block);
int hccJitEpilogueLocalNum(HccJit *jit, IrFunction *fn);
int hccJitFreshLocalNum(HccJit *jit);

/* Was `sym` pre-registered as a function defined in this TU? Internal
 * functions route through label fixups; externals through dlsym. */
int hccJitIsInternalFunc(HccJit *jit, const char *sym);

/* AF_SYMBOL call fixup / AF_LOCAL branch fixup with the arch reloc. */
void hccJitAddCallFixup(AsmEnc *enc, size_t patch_off, const char *sym,
                        AsmFixupReloc reloc);
void hccJitAddLocalBranchFixup(AsmEnc *enc, size_t patch_off, int local_num,
                               AsmFixupReloc reloc);

/* Phi-materialise driver: collects `to`'s phis matching predecessor
 * `from`, orders them so no pending phi's source is overwritten, and
 * calls `emit` for each. `ud` is the backend's emission context. */
typedef void (*HccJitEmitPhiFn)(void *ud, IrInstr *phi, IrPair *match);
void hccJitPhiMaterialise(IrBlock *from, IrBlock *to,
                          HccJitEmitPhiFn emit, void *ud);

/* Assemble a TempleOS-style asm chunk (captured by prsasm.c) with
 * libtasm and splice the encoded bytes into the jit's encoder at the
 * current position. Used for `asm {}` function bodies and inline
 * IR_ASM statements. Non-zero on error (diagnostics already pushed). */
int hccJitAssembleText(HccJit *jit, AoStr *text, int src_line);

#endif
