#ifndef PRS_ASM
#define PRS_ASM

#include "ast.h"
#include "cctrl.h"
#include "asm/asm.h"
#include "asm/target.h"

/* Parse an `asm { ... }` block. The heavy lifting (TempleOS-style asm
 * parsing + encoding) lives in the libtasm assembler under src/asm;
 * this layer re-renders the macro-expanded HolyC token stream into
 * plain text for it, tracks the `NAME::` function labels so HolyC code
 * can call them, and syntax-checks the result.
 *
 * `parse_one` is the inline-statement mode (an `asm {}` inside a
 * function body); 0 is the top-level function-defining form. */
Ast *prsAsm(Cctrl *cc, int parse_one);

/* The libtasm dialect for cc->target. */
Target prsAsmTasmArch(Cctrl *cc);

/* asm_error_handler that forwards libtasm parse/encode diagnostics
 * into the cctrl accumulator as errors. Pass the Cctrl as ctx. Shared
 * by the parse-time syntax check here and the backends' encode calls. */
void prsAsmDiagSink(void *ctx, AsmLine *ln, const char *msg);

#endif /* PRS_ASM */
