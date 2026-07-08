#ifndef SAGARIS_DIS_X86_64_H
#define SAGARIS_DIS_X86_64_H

/* Disassembler for the subset of x86_64 our encoder emits. Like the
 * AArch64 disassembler this is mostly for inspecting JIT output and
 * roundtrip-testing the encoders. Covers the integer GPR core
 * (MOV/ALU/branches/CALL/RET/LEA/MOVSX<*>/MOVZX<*>/MUL/IDIV/etc) and
 * the SSE/SSE2 scalar subset the encoder supports (moves, arithmetic,
 * compares, converts, packed bitwise); not AVX, not the x87 stack ops,
 * not the BMI subset.
 *
 * Output uses Intel-style syntax: `<mnemonic> dst, src` with explicit
 * register names and `[base + disp]` memory operands. */

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

/* Decode one x86_64 instruction at `bytes`. Returns the number of
 * bytes consumed (>= 1) on success, 0 if the leading byte couldn't be
 * decoded. On failure the buffer receives a `.byte 0x..` line. */
int x86_64_disasm(const uint8_t *bytes, size_t len, char *out, size_t outsz);

/* Walk `bytes` and disassemble each instruction. One line per insn,
 * prefixed with byte offset and raw hex. */
void x86_64_disasm_buf(const uint8_t *bytes, size_t len, FILE *f);

/* Same, but a non-zero `base` replaces the offset column with the
 * absolute address `base + off` (hex) - pass the buffer's runtime
 * address to label JIT code with real addresses. base 0 == offsets. */
void x86_64_disasm_buf_at(const uint8_t *bytes, size_t len, uint64_t base,
                          FILE *f);

#endif
