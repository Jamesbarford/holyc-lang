#ifndef X86_64_H__
#define X86_64_H__

/* The x86_64 backend: one instruction selector (IR -> machine IR)
 * whose stream is encoded for the JIT and the object writer, or
 * printed as Intel-dialect GNU-as text for the AOT path. See
 * x86_64.c. */

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "ir-types.h"
#include "jit-common.h"

void x86_64InitRegPool(void);

/* AOT text: the string table, `asm {}` function bodies, one function
 * body (selected through the MIR selector), one global. */
void x86_64PasteDataSection(Cctrl *cc, AoStr *buf);
void x86_64PasteAsmBlocks(Cctrl *cc, AoStr *buf);
void x86_64MirGenerateFunction(IrCgCtx *ctx, Ast *ast);
void x86_64GlobalVar(Cctrl *cc, Set *seen_globals, AoStr *buf, Ast *ast);

/* Run the front-end -> IR -> x86_64 bytes -> RX mapping pipeline for
 * every function in `cc`. NULL on error. */
HccJit *x86_64JitCompile(Cctrl *cc);

/* The backend vtable, for incremental compilation (the REPL) and the
 * object writer. */
const HccJitBackend *x86_64JitBackend(void);

#endif
