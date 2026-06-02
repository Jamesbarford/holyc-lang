#ifndef AARCH64_JIT_H__
#define AARCH64_JIT_H__

/* Optional in-process JIT for AArch64 hosts. Lowers IR straight to
 * native instruction bytes via the libtasm AArch64 encoders; all the
 * arch-independent machinery (state, globals, symbol resolution, the
 * runner) lives in jit-common.h.
 *
 * Build-gated: this header is only meaningful when HCC is built on an
 * AArch64 host with HCC_ENABLE_JIT defined. */

#include "cctrl.h"
#include "jit-common.h"

/* Run the front-end -> IR -> aarch64 bytes -> RX mapping pipeline for
 * every function in `cc`. Returns NULL on error (errors are reported
 * via the same channels as the AOT path). Use hccJitRunMain /
 * hccJitLookup / hccJitFree on the result. */
HccJit *aarch64JitCompile(Cctrl *cc);

#endif
