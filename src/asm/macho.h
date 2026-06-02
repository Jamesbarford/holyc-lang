#ifndef HCC_MACHO_H__
#define HCC_MACHO_H__

/* Minimal Mach-O object-file writer.
 *
 * Takes an encoded AsmEnc (machine-code bytes, label definitions, fixups)
 * and writes a 64-bit Mach-O `MH_OBJECT` file. The result is suitable
 * for `ld` to consume, producing a runnable Mach-O executable on macOS.
 *
 * Layout we produce:
 *
 *   mach_header_64
 *   LC_SEGMENT_64 (single anonymous segment)
 *     section_64    __TEXT,__text  (the encoded bytes)
 *   LC_SYMTAB
 *   __text bytes
 *   [relocation_info * n_relocs]   (TODO; not emitted yet)
 *   nlist_64 * n_syms
 *   string table
 *
 * Phase 1 (current): aarch64 only, no relocations. AF_SYMBOL fixups are
 * dropped on the floor with a warning - we can't reach external symbols
 * yet. AF_LOCAL fixups were already resolved by the encoder. */

#include <stdio.h>

#include "asm_enc.h"

/* Write a `.o` for `target` from `enc` to `out`. Returns 0 on success;
 * negative on I/O error. Does NOT close `out`.
 *
 * `blk` is optional - pass NULL to emit a single __TEXT,__text section
 * with everything in enc->bytes. If `blk` is supplied, the writer walks
 * the line list and partitions enc->bytes into __text (AINS_INSTR) and
 * __DATA,__data (AINS_DIRECTIVE), patching label offsets and fixups so
 * each lands in the right section. */
int macho_write(AsmEnc *enc, AsmBlock *blk, Target target, FILE *out);

#endif
