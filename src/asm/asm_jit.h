#ifndef HCC_ASM_JIT_H
#define HCC_ASM_JIT_H

/* Thin JIT layer on top of libtasm: copies the encoded bytes into a fresh
 * RX mapping, patches AF_SYMBOL fixups against caller-supplied addresses,
 * and flushes the I-cache so the result is callable as a function. */

#include <stddef.h>

#include "asm_enc.h"

/* Resolve a symbol name to its runtime address. Return NULL if unknown. */
typedef void *(*asm_jit_resolver_fn)(void *ud, const char *sym);

/* Default resolver: looks symbols up via `dlsym(RTLD_DEFAULT, name)`.
 * Useful for JIT'd code that calls libc or any function the host process
 * has already loaded. `ud` is ignored; pass NULL. */
void *asm_jit_dlsym_resolver(void *ud, const char *sym);

/* Result of asm_jit_finalize. `code` is the executable entry; cast it to a
 * function pointer of the appropriate signature and call. The other fields
 * are bookkeeping owned by the JIT - pass the whole struct to asm_jit_free
 * when done. */
typedef struct {
    void *code;
    size_t size;
    /* Bytes of out-of-range branch veneers emitted after `size` (the
     * region [code+size, code+size+veneer_size) is live executable code). */
    size_t veneer_size;
    void *_mapping;
    size_t _mapping_size;
} AsmJitCode;

/* Finalize `enc` into an executable region. Walks `enc->fixups`, resolves
 * each AF_SYMBOL via `resolver(ud, sym)`, patches the placeholder bytes
 * (BL/B/CALL/JMP/Jcc), then transitions the mapping to executable and
 * invalidates the I-cache. On failure routes a message through the
 * AsmEnc's error handler and returns -1; on success returns 0 and writes
 * `*out`. */
int asm_jit_finalize(AsmEnc *enc, asm_jit_resolver_fn resolver, void *ud,
                     AsmJitCode *out);

/* Unmap the executable region. Safe to call on a zeroed AsmJitCode. */
void asm_jit_free(AsmJitCode *code);

#endif
