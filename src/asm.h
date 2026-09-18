#ifndef ASM_H__
#define ASM_H__

#include "aostr.h"
#include "cctrl.h"
#include "types.h"

char *asmNormaliseFunctionName(Cctrl *cc, AoStr *fname);
int asmFunctionIsExtern(Cctrl *cc, AoStr *fname);
/* `-fPIC`: may this symbol bind to another image at load time, so
 * that its address must come from the GOT rather than adrp+add /
 * rip-relative lea? Always 0 without `-fPIC`. */
int asmSymbolIsPreemptible(Cctrl *cc, const char *sym);
AoStr *asmNormaliseGlobalLabel(Cctrl *cc, AoStr *name);
/* The symbol an `__asm("sym")` rename points `name` at, or NULL. The
 * JIT needs this on its own: in ABSOLUTE mode it keys host symbols by
 * the RAW name (see a64JitGlobalName), so it must apply the rename
 * without the platform normalisation that comes with it above. */
const char *asmRenameFor(Cctrl *cc, AoStr *name);
/* `@[spangle]` - see asm.c. The first binds a name to an unspellable
 * symbol, the second reads that binding back so `-S` can say whose it
 * is. */
void asmSpangleRegister(Cctrl *cc, char *name, int len);
const char *asmSpangledOrigin(Cctrl *cc, const char *sym);
void asmSpangleNote(Cctrl *cc, AoStr *buf, char *sym);
AoStr *asmSpangleFor(AoStr *name);
AoStr *asmGenerate(Cctrl *cc);
/* Synthetic file-scope-initialiser `main` (calls the user's Main).
 * NULL when there are no initialisers. Shared by all backends. */
Ast *asmBuildInitialiserMain(Cctrl *cc);
uint64_t ieee754_64(double _f64);
uint32_t ieee754_32(f32 _f32);

/* Assemble a TempleOS-style asm chunk (captured by prsasm.c) with
 * libtasm for cc->target and append the encoded bytes to `buf` as
 * `.byte` runs for the system assembler. Symbol references that
 * libtasm leaves as fixups are re-expressed as relocation expressions
 * (`.long sym - . - 4` for rel32) with platform name mangling applied.
 * Returns 0 on success; on failure pushes cctrl error diagnostics and
 * returns -1. */
int asmEmitBlockBytes(Cctrl *cc, AoStr *buf, AoStr *text, int src_line);

void asmEmitAsmInfo(Cctrl *cc, AoStr *buf);

#endif
