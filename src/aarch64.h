#ifndef AARCH64_H__
#define AARCH64_H__

/* The AArch64 backend: one instruction selector (IR -> machine IR)
 * whose stream is encoded for the JIT and the object writer, or
 * printed as GNU-as text for the AOT path. See aarch64.c. */

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "ir-types.h"
#include "jit-common.h"

void aarch64InitRegPool(void);

/* AOT text: the string table, `asm {}` function bodies, one function
 * body (selected through the MIR selector), one global. */
void aarch64PasteDataSection(Cctrl *cc, AoStr *buf);
void aarch64PasteAsmBlocks(Cctrl *cc, AoStr *buf);
void aarch64MirGenerateFunction(IrCgCtx *ctx, Ast *ast);
void aarch64GlobalVar(Cctrl *cc, Set *seen_globals, AoStr *buf, Ast *ast);

/* Run the front-end -> IR -> aarch64 bytes -> RX mapping pipeline for
 * every function in `cc`. NULL on error. */
HccJit *aarch64JitCompile(Cctrl *cc);

/* The backend vtable, for incremental compilation (the REPL) and the
 * object writer. */
const HccJitBackend *aarch64JitBackend(void);

#endif
