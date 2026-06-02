#ifndef X86_64_JIT_H__
#define X86_64_JIT_H__

/* Optional in-process JIT for x86_64 hosts. Lowers IR straight to
 * native instruction bytes via the libtasm x86_64 direct-emit encoders;
 * all the arch-independent machinery (state, globals, symbol
 * resolution, the runner) lives in jit-common.h.
 *
 * Build-gated: this header is only meaningful when HCC is built on an
 * x86_64 host with HCC_ENABLE_JIT defined. */

#include "cctrl.h"
#include "jit-common.h"

/* Run the front-end -> IR -> x86_64 bytes -> RX mapping pipeline for
 * every function in `cc`. Returns NULL on error (errors are reported
 * via the same channels as the AOT path). Use hccJitRunMain /
 * hccJitLookup / hccJitFree on the result. */
HccJit *x86_64JitCompile(Cctrl *cc);

#endif
