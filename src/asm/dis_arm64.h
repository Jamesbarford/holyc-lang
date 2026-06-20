#ifndef SAGARIS_DIS_ARM64_H
#define SAGARIS_DIS_ARM64_H

/* Disassembler for the subset of AArch64 emitted by our encoder.
 * Inverse of enc_arm64. Mostly used to verify JIT output and to
 * roundtrip-test the encoders.
 *
 * Output syntax mirrors the assembler we accept on the input side
 * (lowercase mnemonics, x/w/sp register names, hash-prefixed
 * immediates). The buffer is always NUL-terminated. */

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

/* Disassemble one 32-bit instruction into `out` (NUL-terminated).
 * Returns 1 on success, 0 if the bit pattern wasn't recognised by the
 * subset we cover (the buffer still receives a `.word 0x...` line in
 * that case). */
int aarch64_disasm(uint32_t insn, char *out, size_t outsz);

/* Disassemble `len` bytes (assumed 4-byte aligned). One line per
 * instruction, prefixed with byte offset and raw hex word. */
void aarch64_disasm_buf(const uint8_t *bytes, size_t len, FILE *f);

#endif
