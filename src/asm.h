#ifndef ASM_H__
#define ASM_H__

#include "aostr.h"
#include "cctrl.h"
#include "types.h"

char *asmNormaliseFunctionName(Cctrl *cc, AoStr *fname);
AoStr *asmNormaliseGlobalLabel(Cctrl *cc, AoStr *name);
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
