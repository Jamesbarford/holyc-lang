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

    /* Single accumulating encoder. All function bytes land here in
     * emission order; public labels carry the function name. */
    AsmEnc enc;

    /* name -> entry address inside the RX mapping. Populated post-finalize. */
    Map *symbols;
    /* User-defined host symbols (printf etc.) - checked before dlsym. */
    Map *host_symbols;

    /* RW arena for globals + string literals; absolute addresses are
     * registered in host_symbols so the resolver picks them up. */
    uint8_t *globals;
    size_t   globals_size;

    /* Per-IrBlock -> AsmEnc local_num.  Key = (fn_uuid<<32) | block_id;
     * value = small int.  Local nums are pulled from `next_local`. */
    Map *block_local;
    /* Per-function epilogue label local_num. Key = fn_uuid. */
    Map *epi_local;
    int  next_local;

    AsmJitCode code;
} HccJit;

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

/* Look up a public function by name (tries the underscore-prefixed
 * macOS mangling too). The returned pointer is owned by the jit and
 * remains valid until hccJitFree. */
void *hccJitLookup(HccJit *jit, const char *name);

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
