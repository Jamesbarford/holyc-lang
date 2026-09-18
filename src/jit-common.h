#ifndef JIT_COMMON_H__
#define JIT_COMMON_H__

/* Arch-independent machinery shared by the aarch64 and x86_64 JIT
 * backends: JIT state, label-number bookkeeping, fixup helpers, symbol
 * resolution, globals/string-literal layout, phi-materialise ordering,
 * and the compile-pipeline driver. The per-arch files own only the
 * instruction selection and emission.
 *
 * Always built: the AOT path prints the same selector's stream. */

#include <stdint.h>

#include "ast.h"
#include "cctrl.h"
#include "containers.h"
#include "ir.h"
#include "machine/asm_enc.h"
#include "machine/asm_jit.h"
#include "machine/obj.h"

/* How a reference to a global, string literal or function becomes an
 * address.
 *
 * ABSOLUTE   the address is known NOW - this process's own arenas and
 *            dlsym - so it is materialised inline (movabs on x86_64, a
 *            movz/movk sequence on aarch64). Only possible because the
 *            code is about to run in this process.
 * RELOCATED  the address is not known until link time, so the reference
 *            is emitted PC-relative with a fixup and somebody else
 *            fills it in. This is what an object file needs.
 *
 * Both backends already emitted the RELOCATED shape for calls to
 * functions defined in the same chunk, since those addresses do not
 * exist yet either - so selecting it everywhere is choosing an existing
 * branch rather than a new one. */
typedef enum {
    HCC_ADDR_ABSOLUTE = 0,
    HCC_ADDR_RELOCATED,
} HccAddrMode;

/* One chunk's globals-and-strings arena. */
typedef struct HccJitArena {
    unsigned char *base;
    size_t size;
} HccJitArena;

typedef struct HccJit {
    Cctrl *cc;

    /* See HccAddrMode. Defaults to ABSOLUTE, which is the JIT. */
    HccAddrMode addr_mode;

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
     * absolute addresses are registered in host_symbols. Each entry is
     * an HccJitArena: the size is carried because -Memsafe range-checks
     * a suspect pointer against them, and a bare base cannot say
     * whether an address falls inside. */
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

    /* Function addresses referenced from a global's initialiser. The
     * globals are laid out before any function is compiled, so the
     * address is not known when the bytes are written - record where it
     * goes and patch once the chunk has finalized and its labels are
     * registered. */
    struct HccJitPtrFixup {
        unsigned char *addr;
        char *symbol;
    } *ptr_fixups;
    u32 n_ptr_fixups, cap_ptr_fixups;

    /* RELOCATED mode only: the `.data` image and where each symbol sits
     * within it.
     *
     * Globals are laid out BEFORE any function is compiled, but the
     * data has to land AFTER the code in the byte stream - so it is
     * held here and appended once emission is done, by
     * hccJitAppendDataSection. `data_base` is the arena the layout
     * wrote into, which is also what ptr_fixups point into. */
    unsigned char *data_base;
    size_t data_len;
    struct HccJitDataSym {
        char *name;
        size_t off;
        /* Private to this translation unit, so the object writer must
         * emit an internal symbol rather than export the name. True for
         * every string literal and every `static` global: both are
         * named by astMakeLabel's `.L<n>` counter, which restarts at
         * zero in each compilation, so any two objects would otherwise
         * both export `.L12` and refuse to link. */
        int is_local;
    } *data_syms;
    u32 n_data_syms, cap_data_syms;
} HccJit;

/* Append the RELOCATED-mode `.data` image to `jit->enc`, define a
 * symbol for every global and string in it, and turn the pointer
 * fixups into AFR_ABS64 relocations. No-op in ABSOLUTE mode. Call once
 * code emission for the chunk is complete. */
void hccJitAppendDataSection(HccJit *jit);

/* Does THIS object define `sym`? Functions compiled into this chunk and
 * globals/strings laid into its data image do; anything else is the
 * linker's to find, and its address cannot be computed PC-relatively.
 * Only meaningful in HCC_ADDR_RELOCATED mode. */
int hccJitDefinesSymbol(HccJit *jit, const char *sym);

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

/* Same, but `load_host_libs` 0 skips dlopening the runtime and any
 * `#link` shared objects - for contexts that only EMIT (the AOT text
 * path, the object writer) and never execute what they compile. */
/* As hccJitNewEx, but the caller says whether the backend has to match
 * the PROGRAM's target. `#eval` says no: its code runs on the host. */
HccJit *hccJitNewFor(Cctrl *cc, const HccJitBackend *backend,
                     int load_host_libs, int check_target);

HccJit *hccJitNewEx(Cctrl *cc, const HccJitBackend *backend,
                    int load_host_libs);

/* Lower the whole program and write it as a relocatable object, using
 * the same lowering as the JIT with HCC_ADDR_RELOCATED selected.
 * Returns 0 on success. */
int hccJitCompileObject(Cctrl *cc, const HccJitBackend *backend,
                        Target target, ObjFormat fmt, FILE *out);

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
