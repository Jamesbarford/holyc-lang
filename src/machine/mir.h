#ifndef SAGARIS_MIR_H
#define SAGARIS_MIR_H

/* Machine IR: the target-level instruction stream the compiler selects
 * into, built programmatically instead of parsed from text.
 *
 * There is deliberately no new instruction type. An inline `asm { }`
 * block already parses into AsmLine - mnemonic id, operands, labels -
 * and everything downstream consumes that: asm_line_format prints it,
 * asm_encode turns it into bytes and fixups, the object writers eat
 * the result. A second machine-instruction struct would need a second
 * printer and a second encoder kept in sync with these by hand. So a
 * MirFn IS an AsmBlock under construction, and this header is only the
 * builder: constructors for operands and an append API for lines.
 *
 * Ownership: operand constructors return by VALUE and do not allocate;
 * any string they carry is borrowed. mir_emit deep-copies operands
 * (including strings) into the line, so a constructed operand is safe
 * to pass from a stack temporary and the whole function frees with the
 * ordinary asm_block_free ownership rules.
 *
 * The AsmLine* returned by the emit calls is valid only until the next
 * append - the line array grows by realloc. */

#include <stdint.h>
#include <stdio.h>

#include "asm.h"
#include "asm_enc.h"
#include "target.h"

typedef struct MirFn {
    char *name;     /* public label at offset 0; NULL for a raw stream */
    Target target;
    AsmBlock *block;
} MirFn;

/* `name` may be NULL. When set, the function begins with `name::`. */
MirFn *mir_fn_new(const char *name, Target target);
void mir_fn_free(MirFn *fn);

/* ---------------- operands ---------------- */

AsmOperand mir_reg(AsmRegClass cls, int num);
AsmOperand mir_x(int num);   /* x0..x30, 31 = xzr */
AsmOperand mir_w(int num);   /* w0..w30, 31 = wzr */
AsmOperand mir_d(int num);   /* d0..d31 */
AsmOperand mir_s(int num);   /* s0..s31 */
AsmOperand mir_q(int num);   /* q0..q31 */
AsmOperand mir_sp(void);
/* x86_64 registers, by hardware number (X86Reg). mir_r8 is the low
 * byte (al..dil, r8b..); the legacy AH family is not constructible. */
AsmOperand mir_r64(int num); /* rax..r15 */
AsmOperand mir_r32(int num); /* eax..r15d */
AsmOperand mir_r16(int num); /* ax..r15w */
AsmOperand mir_r8(int num);  /* al..r15b */
AsmOperand mir_xmm(int num); /* xmm0..xmm15 */
AsmOperand mir_imm(int64_t v);
AsmOperand mir_fimm(double v);
/* ASHIFT_LSL/LSR/ASR/ROR + amount, the arm64 4th-operand form. */
AsmOperand mir_shift(int shift_type, int amount);
/* [base, #disp] - `base` from mir_x/mir_sp. */
AsmOperand mir_mem(AsmOperand base, int64_t disp);
/* [base, index, LSL #n] - n 0 for no shift. */
AsmOperand mir_mem_idx(AsmOperand base, AsmOperand index, int lsl);
/* [base, #disp]! - pre-index with writeback. */
AsmOperand mir_mem_pre(AsmOperand base, int64_t disp);
/* [base], #disp - post-index. */
AsmOperand mir_mem_post(AsmOperand base, int64_t disp);
/* x86 [base + index*scale + disp]; scale 1/2/4/8. */
AsmOperand mir_mem_sib(AsmOperand base, AsmOperand index, int scale,
                       int64_t disp);
/* Size-hint a memory operand (1/2/4/8 bytes): `u8 [rbp - 8]`. Needed
 * where no register operand implies the width (movzx/movsx sources). */
AsmOperand mir_mem_hint(AsmOperand mem, int width);
AsmOperand mir_label_local(int num);      /* @@num */
AsmOperand mir_sym(const char *name);     /* external / public symbol */

/* ---------------- instructions ---------------- */

/* Append `mnemonic_id` (A64MN_* / X86MN_*) with `n` operands. The
 * canonical mnemonic text is looked up from the same table the parser
 * uses, so the printed form and the parsed form cannot drift. */
AsmLine *mir_emit(MirFn *fn, int mnemonic_id, const AsmOperand *ops, int n);
AsmLine *mir0(MirFn *fn, int mnemonic_id);
AsmLine *mir1(MirFn *fn, int mnemonic_id, AsmOperand a);
AsmLine *mir2(MirFn *fn, int mnemonic_id, AsmOperand a, AsmOperand b);
AsmLine *mir3(MirFn *fn, int mnemonic_id, AsmOperand a, AsmOperand b,
              AsmOperand c);
AsmLine *mir4(MirFn *fn, int mnemonic_id, AsmOperand a, AsmOperand b,
              AsmOperand c, AsmOperand d);

/* The arm64 `b.<cc>` family has no mnemonic id - sixteen condition
 * suffixes are recognised off the TEXT by the encoder - so it is
 * emitted by spelling: mir_bcond(fn, "ne", 3) is `b.ne @@3`. */
AsmLine *mir_bcond(MirFn *fn, const char *cc, int local_num);

void mir_local_label(MirFn *fn, int num);          /* @@num: */
void mir_public_label(MirFn *fn, const char *name); /* name:: */

/* ---------------- sinks ---------------- */

/* Encode into `out` via the target's existing encoder (asm_encode):
 * same validation, same fixups, same local-label resolution as inline
 * asm. Returns the error count, 0 = clean. */
int mir_fn_encode(MirFn *fn, AsmResolver *r, AsmEnc *out);

/* Canonical text of one line - thin wrapper over asm_line_format so a
 * caller assembling `-S` output can indent and join lines itself. */
int mir_fn_format_line(MirFn *fn, int i, char *buf, size_t buflen);
int mir_fn_n_lines(MirFn *fn);

/* Debug print, one line per instruction, labels unindented. */
void mir_fn_print(MirFn *fn, FILE *f);

/* Canonical spelling for a mnemonic id from the parse table; NULL if
 * the id is not in the table. */
const char *mir_mnemonic_name(Target target, int mnemonic_id);

/* AArch64 peephole over lines [from, n): dead local labels,
 * branch-to-next, self-moves, duplicate adjacent extensions,
 * store-then-reload pairs. Deletes lines in place; returns how many.
 * Run it before sinking a segment (mir_arm64.c). */
int mir_fn_peephole_arm64(MirFn *fn, int from);

/* The x86_64 twin (mir_x86_64.c): same shapes in the x86 spellings -
 * jmp/jcc-to-next, `mov r64, r64` / xmm self-moves (32-bit self-moves
 * zero-extend and stay), duplicate reg-reg extensions, full-width
 * store-then-reload pairs, dead local labels. */
int mir_fn_peephole_x86_64(MirFn *fn, int from);

#endif
