#ifndef HCC_ELF_H__
#define HCC_ELF_H__

/* Minimal ELF64 relocatable-object writer.
 *
 * Takes an encoded AsmEnc (machine-code bytes, label definitions,
 * fixups) and writes an `ET_REL` object a system linker will consume.
 * The point is to stop shelling out to an assembler: hcc already
 * knows the bytes, and this turns them into a `.o` directly, leaving
 * the linker as the only external tool.
 *
 * The counterpart of macho.c, which does the same job for Darwin.
 * Where that one is Phase 1 and drops relocations on the floor, this
 * one emits them - an object with no relocations cannot call printf,
 * which is most of the point.
 *
 * Layout we produce:
 *
 *   Elf64_Ehdr
 *   .text bytes
 *   .data bytes                (only when there are any)
 *   .rela.text                 (only when there are any)
 *   .rela.data
 *   .symtab
 *   .strtab
 *   .shstrtab
 *   Elf64_Shdr * n_sections
 *
 * Both architectures. They share everything except the relocation
 * type map and the addend convention (see elf_reloc_type_for and
 * elf_addend_for), which is the split obj.h describes. */

#include <stdio.h>

#include "asm_enc.h"

/* Write a `.o` for `target` from `enc` to `out`. Returns 0 on success,
 * negative on I/O error or an unsupported target. Does NOT close `out`.
 *
 * `blk` is optional. Pass NULL to put everything in `.text`. When it is
 * supplied the writer walks the line list and partitions enc->bytes
 * into `.text` (AINS_INSTR) and `.data` (AINS_DIRECTIVE), moving label
 * offsets and fixups with them so each lands in the right section. */
int elf_write(AsmEnc *enc, AsmBlock *blk, Target target, FILE *out);

#endif
