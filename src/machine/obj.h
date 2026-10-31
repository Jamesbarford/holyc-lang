#ifndef HCC_OBJ_H__
#define HCC_OBJ_H__

/* Object-file writing, across formats.
 *
 * `Target` is architecture only - see target.h, which is explicit that
 * "Mach-O vs ELF is the caller's problem", because the bytes an encoder
 * produces do not depend on the container they end up in. The two
 * choices really are independent: hcc targets x86_64 and aarch64,
 * each on both Linux and Darwin, so all four combinations are live.
 *
 * That is why this is a 2x2 and not four writers. What varies with the
 * FORMAT - how sections, symbols and relocations are laid out, and
 * whether addends are explicit - is the whole of elf.c or macho.c. What
 * varies with the ARCHITECTURE inside one of them is small: the machine
 * field, the relocation type map, and the addend convention. Each
 * writer therefore handles both architectures itself. */

#include <stdio.h>

#include "asm_enc.h"

typedef enum {
    OBJ_FMT_ELF,   /* Linux, and anything else System V */
    OBJ_FMT_MACHO, /* Darwin */
} ObjFormat;

/* The format a triple implies. Kept here rather than in the compiler so
 * there is one answer to the question. */
ObjFormat obj_format_for_os_darwin(int is_darwin);

/* Human-readable, for diagnostics. */
const char *obj_format_name(ObjFormat fmt);

/* Write a relocatable object for `target` in `fmt` from `enc` to `out`.
 * Returns 0 on success, negative on I/O error or an unsupported
 * combination. Does NOT close `out`.
 *
 * `blk` is optional; see elf.h / macho.h for what it does. */
int asm_obj_write(AsmEnc *enc, AsmBlock *blk, Target target, ObjFormat fmt,
                  FILE *out);

#endif
