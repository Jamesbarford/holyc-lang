#include <stdio.h>

#include "elf.h"
#include "macho.h"
#include "obj.h"

ObjFormat
obj_format_for_os_darwin(int is_darwin)
{
    return is_darwin ? OBJ_FMT_MACHO : OBJ_FMT_ELF;
}

const char *
obj_format_name(ObjFormat fmt)
{
    switch (fmt) {
        case OBJ_FMT_ELF:   return "ELF";
        case OBJ_FMT_MACHO: return "Mach-O";
        default:            return "<unknown>";
    }
}

int
asm_obj_write(AsmEnc *enc, AsmBlock *blk, Target target, ObjFormat fmt,
              FILE *out)
{
    switch (fmt) {
        case OBJ_FMT_ELF:
            return elf_write(enc, blk, target, out);
        case OBJ_FMT_MACHO:
            return macho_write(enc, blk, target, out);
        default:
            fprintf(stderr, "hcc: unknown object format\n");
            return -1;
    }
}
