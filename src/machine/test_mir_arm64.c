#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "asm.h"
#include "asm_enc.h"
#include "enc_arm64.h"
#include "mir.h"
#include "target.h"
#include "test-helper.h"

/* MIR round-trip tests.
 *
 * No golden byte values here on purpose. The parser's encoding is
 * already pinned by test_aarch64.c's golden words, so each case
 * asserts two things against expected TEXT instead:
 *
 *   1. the built MirFn prints as exactly that text, and
 *   2. encoding the MirFn and encoding the parsed text produce
 *      identical bytes and fixups.
 *
 * Together they tie the builder to the parser's goldens: a builder
 * that spells or encodes anything differently from the assembler
 * fails, without a second set of hex constants to keep in sync. */

/* The canonical formatter pads mnemonics into a column; compare with
 * runs of blanks collapsed (and none kept before a newline) so the
 * expected strings stay readable and the column width stays free to
 * change. */
static void
squash_blanks(const char *src, char *dst, size_t cap)
{
    size_t n = 0;
    int pending = 0;
    for (const char *p = src; *p && n + 2 < cap; p++) {
        if (*p == ' ' || *p == '\t') { pending = n > 0; continue; }
        if (pending && *p != '\n') dst[n++] = ' ';
        pending = 0;
        dst[n++] = *p;
    }
    dst[n] = '\0';
}

static int
mir_text_eq(MirFn *fn, const char *want)
{
    char got[4096];
    size_t off = 0;
    for (int i = 0; i < mir_fn_n_lines(fn); i++) {
        char line[256];
        mir_fn_format_line(fn, i, line, sizeof(line));
        off += (size_t)snprintf(got + off, sizeof(got) - off, "%s\n", line);
        if (off >= sizeof(got)) return 0;
    }
    char got_n[4096], want_n[4096];
    squash_blanks(got, got_n, sizeof(got_n));
    squash_blanks(want, want_n, sizeof(want_n));
    if (strcmp(got_n, want_n) != 0) {
        printf("  text mismatch\n  want:\n%s  got:\n%s", want, got);
        return 0;
    }
    return 1;
}

static int
mir_bytes_eq_parse(TestCtx *T, MirFn *fn, const char *text)
{
    if (mir_fn_encode(fn, NULL, &T->enc) != 0) {
        printf("  mir encode reported errors\n");
        return 0;
    }
    AsmEnc pe;
    asm_enc_init(&pe);
    asm_parse_and_encode(&pe, NULL, T->target, (char *)text,
                         (int)strlen(text), "test_mir", 0, NULL, NULL);
    int ok = 1;
    if (pe.errors) {
        printf("  parser encode reported errors\n");
        ok = 0;
    }
    if (ok && (pe.len != T->enc.len ||
               memcmp(pe.bytes, T->enc.bytes, pe.len) != 0)) {
        printf("  byte mismatch: mir %zu bytes, parser %zu bytes\n",
               T->enc.len, pe.len);
        ok = 0;
    }
    if (ok && pe.n_fixups != T->enc.n_fixups) {
        printf("  fixup count mismatch: mir %d, parser %d\n",
               T->enc.n_fixups, pe.n_fixups);
        ok = 0;
    }
    if (ok) {
        for (int i = 0; i < pe.n_fixups; i++) {
            AsmFixup *a = &T->enc.fixups[i];
            AsmFixup *b = &pe.fixups[i];
            if (a->kind != b->kind || a->reloc != b->reloc ||
                a->patch_offset != b->patch_offset ||
                a->width != b->width || a->pcrel != b->pcrel ||
                ((a->sym != NULL) != (b->sym != NULL)) ||
                (a->sym && strcmp(a->sym, b->sym) != 0)) {
                printf("  fixup %d differs\n", i);
                ok = 0;
                break;
            }
        }
    }
    asm_enc_free(&pe);
    return ok;
}

static int
mir_roundtrip(TestCtx *T, MirFn *fn, const char *text)
{
    int ok = mir_text_eq(fn, text) && mir_bytes_eq_parse(T, fn, text);
    mir_fn_free(fn);
    return ok;
}

/* ============================================================ cases */

static int
mir_add_reg_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir3(fn, A64MN_ADD, mir_x(0), mir_x(1), mir_x(2));
    mir3(fn, A64MN_ADD, mir_w(0), mir_w(1), mir_w(2));
    return mir_roundtrip(T, fn, "add x0, x1, x2\n"
                                "add w0, w1, w2\n");
}

static int
mir_add_imm_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir3(fn, A64MN_ADD, mir_x(0), mir_x(1), mir_imm(16));
    mir3(fn, A64MN_SUB, mir_sp(), mir_sp(), mir_imm(32));
    return mir_roundtrip(T, fn, "add x0, x1, #16\n"
                                "sub sp, sp, #32\n");
}

static int
mir_mov_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_MOV, mir_x(0), mir_imm(42));
    mir2(fn, A64MN_MOV, mir_x(0), mir_x(1));
    return mir_roundtrip(T, fn, "mov x0, #42\n"
                                "mov x0, x1\n");
}

static int
mir_ldur_stur_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_LDUR, mir_x(0), mir_mem(mir_x(29), -8));
    mir2(fn, A64MN_STUR, mir_x(0), mir_mem(mir_x(29), -16));
    return mir_roundtrip(T, fn, "ldur x0, [x29, #-8]\n"
                                "stur x0, [x29, #-16]\n");
}

static int
mir_ldr_indexed_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_LDR, mir_x(0), mir_mem_idx(mir_x(1), mir_x(2), 3));
    return mir_roundtrip(T, fn, "ldr x0, [x1, x2, lsl #3]\n");
}

static int
mir_fcvt_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_FCVTZS, mir_x(0), mir_d(0));
    mir2(fn, A64MN_SCVTF, mir_d(0), mir_x(0));
    return mir_roundtrip(T, fn, "fcvtzs x0, d0\n"
                                "scvtf d0, x0\n");
}

static int
mir_fadd_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir3(fn, A64MN_FADD, mir_d(0), mir_d(1), mir_d(2));
    return mir_roundtrip(T, fn, "fadd d0, d1, d2\n");
}

static int
mir_sxtw_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_SXTW, mir_x(13), mir_w(0));
    return mir_roundtrip(T, fn, "sxtw x13, w0\n");
}

static int
mir_ret_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir0(fn, A64MN_RET);
    return mir_roundtrip(T, fn, "ret\n");
}

static int
mir_bl_sym_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir1(fn, A64MN_BL, mir_sym("_printf"));
    return mir_roundtrip(T, fn, "bl _printf\n");
}

/* A whole function: public label, compare, conditional branch to a
 * local label, fall-through, branch resolution. The one shape every
 * selected function will exercise. */
static int
mir_branchy_fn_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new("clamp0", TASM_ARCH_ARM64);
    mir2(fn, A64MN_CMP, mir_x(0), mir_imm(0));
    mir_bcond(fn, "ge", 1);
    mir2(fn, A64MN_MOV, mir_x(0), mir_imm(0));
    mir_local_label(fn, 1);
    mir0(fn, A64MN_RET);
    return mir_roundtrip(T, fn, "clamp0::\n"
                                "cmp x0, #0\n"
                                "b.ge @@1\n"
                                "mov x0, #0\n"
                                "@@1:\n"
                                "ret\n");
}

static int
mir_shift_operand_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir4(fn, A64MN_ADD, mir_x(0), mir_x(1), mir_x(2),
         mir_shift(ASHIFT_LSL, 2));
    return mir_roundtrip(T, fn, "add x0, x1, x2, lsl #2\n");
}


/* The mnemonics added for the compiler's MIR leaves - shapes chosen to
 * match exactly what the JIT arith/convert leaves emit. */
static int
mir_divrem_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir3(fn, A64MN_SDIV, mir_x(0), mir_x(0), mir_x(1));
    mir3(fn, A64MN_UDIV, mir_x(0), mir_x(0), mir_x(1));
    mir3(fn, A64MN_SDIV, mir_x(2), mir_x(0), mir_x(1));
    mir4(fn, A64MN_MSUB, mir_x(0), mir_x(2), mir_x(1), mir_x(0));
    return mir_roundtrip(T, fn, "sdiv x0, x0, x1\n"
                                "udiv x0, x0, x1\n"
                                "sdiv x2, x0, x1\n"
                                "msub x0, x2, x1, x0\n");
}

static int
mir_mvn_neg_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_MVN, mir_x(0), mir_x(0));
    mir2(fn, A64MN_MVN, mir_w(0), mir_w(0));
    mir2(fn, A64MN_NEG, mir_x(0), mir_x(0));
    mir2(fn, A64MN_NEG, mir_w(0), mir_w(0));
    return mir_roundtrip(T, fn, "mvn x0, x0\n"
                                "mvn w0, w0\n"
                                "neg x0, x0\n"
                                "neg w0, w0\n");
}

static int
mir_shifts_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir3(fn, A64MN_LSL, mir_x(0), mir_x(0), mir_imm(3));
    mir3(fn, A64MN_LSR, mir_x(0), mir_x(0), mir_imm(3));
    mir3(fn, A64MN_ASR, mir_x(0), mir_x(0), mir_imm(3));
    mir3(fn, A64MN_LSL, mir_x(0), mir_x(0), mir_x(1));
    mir3(fn, A64MN_LSR, mir_x(0), mir_x(0), mir_x(1));
    mir3(fn, A64MN_ASR, mir_x(0), mir_x(0), mir_x(1));
    return mir_roundtrip(T, fn, "lsl x0, x0, #3\n"
                                "lsr x0, x0, #3\n"
                                "asr x0, x0, #3\n"
                                "lsl x0, x0, x1\n"
                                "lsr x0, x0, x1\n"
                                "asr x0, x0, x1\n");
}

static int
mir_uxt_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_UXTB, mir_w(0), mir_w(0));
    mir2(fn, A64MN_UXTH, mir_w(0), mir_w(0));
    mir2(fn, A64MN_SXTB, mir_x(0), mir_w(0));
    mir2(fn, A64MN_SXTH, mir_x(0), mir_w(0));
    return mir_roundtrip(T, fn, "uxtb w0, w0\n"
                                "uxth w0, w0\n"
                                "sxtb x0, w0\n"
                                "sxth x0, w0\n");
}

static int
mir_fcvt_f32_f64_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_FCVT, mir_s(0), mir_d(0));
    mir2(fn, A64MN_FCVT, mir_d(0), mir_s(0));
    return mir_roundtrip(T, fn, "fcvt s0, d0\n"
                                "fcvt d0, s0\n");
}

static int
mir_fmov_bitcast_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_FMOV, mir_d(0), mir_x(0));
    mir2(fn, A64MN_FMOV, mir_x(0), mir_d(0));
    mir2(fn, A64MN_FMOV, mir_s(0), mir_w(0));
    mir2(fn, A64MN_FMOV, mir_w(0), mir_s(0));
    return mir_roundtrip(T, fn, "fmov d0, x0\n"
                                "fmov x0, d0\n"
                                "fmov s0, w0\n"
                                "fmov w0, s0\n");
}

static int
mir_add_shifted_imm_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir3(fn, A64MN_ADD, mir_x(0), mir_x(0), mir_imm(8192));
    return mir_roundtrip(T, fn, "add x0, x0, #8192\n");
}


static int
mir_ldur_subword_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_LDURB, mir_w(0), mir_mem(mir_x(29), -3));
    mir2(fn, A64MN_LDURH, mir_w(0), mir_mem(mir_x(29), 10));
    mir2(fn, A64MN_STURB, mir_w(0), mir_mem(mir_x(29), -3));
    mir2(fn, A64MN_STURH, mir_w(0), mir_mem(mir_x(29), 10));
    mir2(fn, A64MN_LDUR, mir_w(1), mir_mem(mir_x(29), -8));
    mir2(fn, A64MN_STUR, mir_w(1), mir_mem(mir_x(29), -8));
    return mir_roundtrip(T, fn, "ldurb w0, [x29, #-3]\n"
                                "ldurh w0, [x29, #10]\n"
                                "sturb w0, [x29, #-3]\n"
                                "sturh w0, [x29, #10]\n"
                                "ldur w1, [x29, #-8]\n"
                                "stur w1, [x29, #-8]\n");
}

static int
mir_frame_regoff_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_LDR, mir_x(0), mir_mem_idx(mir_x(29), mir_x(9), 0));
    mir2(fn, A64MN_LDRB, mir_w(0), mir_mem_idx(mir_x(29), mir_x(9), 0));
    mir2(fn, A64MN_STR, mir_w(0), mir_mem_idx(mir_x(29), mir_x(9), 0));
    mir2(fn, A64MN_LDR, mir_d(0), mir_mem_idx(mir_x(29), mir_x(9), 0));
    mir2(fn, A64MN_STR, mir_s(0), mir_mem_idx(mir_x(29), mir_x(9), 0));
    return mir_roundtrip(T, fn, "ldr x0, [x29, x9]\n"
                                "ldrb w0, [x29, x9]\n"
                                "str w0, [x29, x9]\n"
                                "ldr d0, [x29, x9]\n"
                                "str s0, [x29, x9]\n");
}

static int
mir_fp_frame_imm_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_LDR, mir_d(0), mir_mem(mir_x(29), 16));
    mir2(fn, A64MN_STR, mir_s(1), mir_mem(mir_x(29), 8));
    mir2(fn, A64MN_LDRSW, mir_x(0), mir_mem(mir_x(29), 16));
    return mir_roundtrip(T, fn, "ldr d0, [x29, #16]\n"
                                "str s1, [x29, #8]\n"
                                "ldrsw x0, [x29, #16]\n");
}

static int
mir_movn_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_MOVN, mir_x(0), mir_imm(41));
    mir2(fn, A64MN_MOV, mir_x(9), mir_imm(1));
    return mir_roundtrip(T, fn, "movn x0, #41\n"
                                "mov x9, #1\n");
}

static int
mir_adrp_pair_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_ADRP, mir_x(0), mir_sym("_target"));
    mir3(fn, A64MN_ADD, mir_x(0), mir_x(0), mir_sym("_target"));
    return mir_roundtrip(T, fn, "adrp x0, _target\n"
                                "add x0, x0, _target\n");
}


/* The control-flow and frame shapes the JIT emits - cset, pair
 * loads/stores with writeback, local branches (fixups compared against
 * the parser, including the reloc tags the JIT finalize keys on), and
 * the shifted cmp immediate. */
static int
mir_cset_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_CMP, mir_x(0), mir_imm(0));
    mir2(fn, A64MN_CSET, mir_x(0), mir_sym("eq"));
    mir2(fn, A64MN_CSET, mir_x(0), mir_sym("hi"));
    return mir_roundtrip(T, fn, "cmp x0, #0\n"
                                "cset x0, eq\n"
                                "cset x0, hi\n");
}

static int
mir_pair_writeback_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir3(fn, A64MN_STP, mir_x(29), mir_x(30), mir_mem_pre(mir_sp(), -16));
    mir2(fn, A64MN_MOV, mir_x(29), mir_sp());
    mir3(fn, A64MN_LDP, mir_x(29), mir_x(30), mir_mem_post(mir_sp(), 16));
    mir0(fn, A64MN_RET);
    return mir_roundtrip(T, fn, "stp x29, x30, [sp, #-16]!\n"
                                "mov x29, sp\n"
                                "ldp x29, x30, [sp], #16\n"
                                "ret\n");
}

static int
mir_local_branches_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir_local_label(fn, 1);
    mir2(fn, A64MN_CMP, mir_x(0), mir_imm(10));
    mir_bcond(fn, "lt", 1);
    mir1(fn, A64MN_B, mir_label_local(2));
    mir_local_label(fn, 2);
    mir0(fn, A64MN_RET);
    return mir_roundtrip(T, fn, "@@1:\n"
                                "cmp x0, #10\n"
                                "b.lt @@1\n"
                                "b @@2\n"
                                "@@2:\n"
                                "ret\n");
}

static int
mir_cmp_shifted_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_CMP, mir_x(0), mir_imm(8192));
    mir2(fn, A64MN_FCMP, mir_d(0), mir_imm(0));
    return mir_roundtrip(T, fn, "cmp x0, #8192\n"
                                "fcmp d0, #0\n");
}

static int
mir_q_reg_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_STR, mir_q(0), mir_mem(mir_x(9), 64));
    mir2(fn, A64MN_LDR, mir_q(7), mir_mem(mir_x(9), 176));
    return mir_roundtrip(T, fn, "str q0, [x9, #64]\n"
                                "ldr q7, [x9, #176]\n");
}


/* The peephole: build the noisy shapes, run the pass, compare the
 * surviving stream. */
static int
mir_peephole_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    /* dead label + branch-to-next + self-move + dup extension +
     * store/reload, interleaved with real work. */
    mir2(fn, A64MN_CMP, mir_x(0), mir_imm(0));
    mir_bcond(fn, "ge", 1);
    mir2(fn, A64MN_MOV, mir_x(0), mir_x(0));            /* self-move */
    mir1(fn, A64MN_B, mir_label_local(1));              /* b to next */
    mir_local_label(fn, 1);
    mir2(fn, A64MN_SXTW, mir_x(0), mir_w(0));
    mir2(fn, A64MN_SXTW, mir_x(0), mir_w(0));           /* dup */
    mir2(fn, A64MN_MOV, mir_w(3), mir_w(3));            /* NOT a no-op */
    mir2(fn, A64MN_MOV, mir_x(15), mir_x(0));
    mir2(fn, A64MN_MOV, mir_x(0), mir_x(15));           /* move back */
    mir2(fn, A64MN_MOV, mir_w(4), mir_w(5));
    mir2(fn, A64MN_MOV, mir_w(5), mir_w(4));            /* NOT: zero-extends */
    mir2(fn, A64MN_STUR, mir_x(0), mir_mem(mir_x(29), -16));
    mir2(fn, A64MN_LDUR, mir_x(0), mir_mem(mir_x(29), -16))->line = 0;
    mir_local_label(fn, 2);                             /* dead */
    mir0(fn, A64MN_RET);
    mir_fn_peephole_arm64(fn, 0);
    return mir_roundtrip(T, fn, "cmp x0, #0\n"
                                "b.ge @@1\n"
                                "@@1:\n"
                                "sxtw x0, w0\n"
                                "mov w3, w3\n"
                                "mov x15, x0\n"
                                "mov w4, w5\n"
                                "mov w5, w4\n"
                                "stur x0, [x29, #-16]\n"
                                "ret\n");
}


static int
mir_fp_unscaled_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_ARM64);
    mir2(fn, A64MN_STR, mir_d(0), mir_mem(mir_x(29), -16));
    mir2(fn, A64MN_LDR, mir_d(1), mir_mem(mir_x(29), -8));
    mir2(fn, A64MN_STR, mir_s(2), mir_mem(mir_x(29), -4));
    return mir_roundtrip(T, fn, "str d0, [x29, #-16]\n"
                                "ldr d1, [x29, #-8]\n"
                                "str s2, [x29, #-4]\n");
}

/* ============================================================ main */

int
main(void)
{
    TestCtx T;
    test_ctx_init(&T, TASM_ARCH_ARM64);

    EncoderTest tests[] = {
        PrepTest(mir_add_reg_test),
        PrepTest(mir_add_imm_test),
        PrepTest(mir_mov_test),
        PrepTest(mir_ldur_stur_test),
        PrepTest(mir_ldr_indexed_test),
        PrepTest(mir_fcvt_test),
        PrepTest(mir_fadd_test),
        PrepTest(mir_sxtw_test),
        PrepTest(mir_ret_test),
        PrepTest(mir_bl_sym_test),
        PrepTest(mir_branchy_fn_test),
        PrepTest(mir_shift_operand_test),
        PrepTest(mir_divrem_test),
        PrepTest(mir_mvn_neg_test),
        PrepTest(mir_shifts_test),
        PrepTest(mir_uxt_test),
        PrepTest(mir_fcvt_f32_f64_test),
        PrepTest(mir_fmov_bitcast_test),
        PrepTest(mir_add_shifted_imm_test),
        PrepTest(mir_ldur_subword_test),
        PrepTest(mir_frame_regoff_test),
        PrepTest(mir_fp_frame_imm_test),
        PrepTest(mir_movn_test),
        PrepTest(mir_adrp_pair_test),
        PrepTest(mir_cset_test),
        PrepTest(mir_pair_writeback_test),
        PrepTest(mir_local_branches_test),
        PrepTest(mir_cmp_shifted_test),
        PrepTest(mir_q_reg_test),
        PrepTest(mir_peephole_test),
        PrepTest(mir_fp_unscaled_test),
    };

    int n = (int)(sizeof(tests) / sizeof(tests[0]));
    int failed = 0;
    for (int i = 0; i < n; i++) {
        EncoderTest *test = &tests[i];
        test->pass = test->fn(&T);
        if (test->pass) {
            test_pass("%s\n", test->name);
        } else {
            test_fail("%s\n", test->name);
            failed++;
        }
        test_asm_enc_free(&T);
    }

    printf("\n%d/%d passed\n", n - failed, n);
    return failed ? 1 : 0;
}
