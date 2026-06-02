#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>
#include <unistd.h>

#include "asm_enc.h"
#include "test-helper.h"

static void
test_aarch64_dump_failure(TestCtx *T)
{
    printf("  Expected:\n");
    for (int i = 0; i < T->expected_len; ++i) {
        printf("    0x%08x\n", T->expected[i]);
    }

    printf("  Got:\n");
    uint32_t *bytes = (uint32_t *)T->enc.bytes;
    int len = T->enc.len / sizeof(uint32_t);
    for (int i = 0; i < len; ++i) {
        printf("    0x%08x\n", bytes[i]);
    }
}

int
_test_aarch64_assert(TestCtx *T, ...)
{
    va_list ap;
    va_start(ap, T);

    T->expected_len = 0;

    int retval = 1;
    int idx = 0;
    uint32_t dsm = 0;
    uint32_t *bytes = (uint32_t *)T->enc.bytes;
    int got_words = (int)(T->enc.len / sizeof(uint32_t));

    while ((dsm = va_arg(ap, uint32_t)) != 0) {
        if (idx >= got_words || dsm != bytes[idx]) {
            retval = 0;
            T->expected[T->expected_len++] = dsm;
        }
        idx++;
    }

    if (idx != got_words) retval = 0;

    va_end(ap);
    return retval;
}

// Where __VA_ARGS__ is `expected`
#define test_aarch64_assert(T, ...) _test_aarch64_assert(T, __VA_ARGS__, 0);

/* ============================================================ MOV */

int
aarch64_enc_mov_small_imm_test(TestCtx *T)
{
    test_asm_enc(T, "mov x0, #0xa");
    return test_aarch64_assert(T, 0xd2800140);
}

int
aarch64_enc_mov_large_imm_test(TestCtx *T)
{
    // INT_MAX, however we can handle up to UINT64_MAX and the assembler will
    // figure it out.
    test_asm_enc(T, "mov x0, #2147483647");
    // Becomes;
    // 0xd29fffe0    movz x0, #0xffff
    // 0xf2afffe0    movk x0, #0x7fff, lsl #16
    return test_aarch64_assert(T, 0xd29fffe0,
                              0xf2afffe0);
}

int
aarch64_enc_mov_small_32bit_imm_test(TestCtx *T)
{
    test_asm_enc(T, "mov w0, #32");
    return test_aarch64_assert(T, 0x52800400);
}

int
aarch64_enc_mov_64bit_reg_test(TestCtx *T)
{
    test_asm_enc(T, "mov x0, x2");
    // Becomes;
    // aa0203e0       orr     x0, xzr, x2
    return test_aarch64_assert(T, 0xaa0203e0);
}

int
aarch64_enc_mov_64bit_imm_reg_test(TestCtx *T)
{
    test_asm_enc(T, "mov x0, #21\n\t"
                           "mov x2, x0");
    // 528002a0        movz    x0, #0x15
    // 2a0003e2        orr     x2, wzr, x0
    return test_aarch64_assert(T, 0xd28002a0,
                                  0xaa0003e2);
}

int
aarch64_enc_mov_32bit_reg_test(TestCtx *T)
{
    test_asm_enc(T, "mov w0, w2");
    // Becomes;
    // aa0203e0       orr     x0, xzr, x2
    return test_aarch64_assert(T, 0x2a0203e0);
}

int
aarch64_enc_mov_32bit_imm_reg_test(TestCtx *T)
{
    test_asm_enc(T, "mov w0, #21\n\t"
                           "mov w2, w0");
    // 528002a0        movz    w0, #0x15
    // 2a0003e2        orr     w2, wzr, w0
    return test_aarch64_assert(T, 0x528002a0,
                                  0x2a0003e2);
}

/* ============================================================ ADD/SUB */

int
aarch64_enc_add_reg_test(TestCtx *T)
{
    test_asm_enc(T, "add x0, x1, x2");
    return test_aarch64_assert(T, 0x8b020020);
}

int
aarch64_enc_add_reg_32_test(TestCtx *T)
{
    test_asm_enc(T, "add w0, w1, w2");
    return test_aarch64_assert(T, 0x0b020020);
}

int
aarch64_enc_add_imm_test(TestCtx *T)
{
    test_asm_enc(T, "add x0, x1, #0x10");
    return test_aarch64_assert(T, 0x91004020);
}

int
aarch64_enc_sub_reg_test(TestCtx *T)
{
    test_asm_enc(T, "sub x0, x1, x2");
    return test_aarch64_assert(T, 0xcb020020);
}

int
aarch64_enc_sub_reg_32_test(TestCtx *T)
{
    test_asm_enc(T, "sub w0, w1, w2");
    return test_aarch64_assert(T, 0x4b020020);
}

int
aarch64_enc_sub_imm_test(TestCtx *T)
{
    test_asm_enc(T, "sub x0, x1, #0x10");
    return test_aarch64_assert(T, 0xd1004020);
}

int
aarch64_enc_add_reg_shifted_lsl_test(TestCtx *T)
{
    test_asm_enc(T, "add x0, x1, x2, lsl #2");
    return test_aarch64_assert(T, 0x8b020820);
}

int
aarch64_enc_sub_reg_shifted_lsr_test(TestCtx *T)
{
    test_asm_enc(T, "sub x0, x1, x2, lsr #4");
    return test_aarch64_assert(T, 0xcb421020);
}

int
aarch64_enc_add_reg_shifted_asr_test(TestCtx *T)
{
    test_asm_enc(T, "add x0, x1, x2, asr #4");
    return test_aarch64_assert(T, 0x8b821020);
}

/* ============================================================ LOGICAL */

int
aarch64_enc_and_reg_test(TestCtx *T)
{
    test_asm_enc(T, "and x0, x1, x2");
    return test_aarch64_assert(T, 0x8a020020);
}

int
aarch64_enc_and_reg_32_test(TestCtx *T)
{
    test_asm_enc(T, "and w0, w1, w2");
    return test_aarch64_assert(T, 0x0a020020);
}

int
aarch64_enc_orr_reg_test(TestCtx *T)
{
    test_asm_enc(T, "orr x0, x1, x2");
    return test_aarch64_assert(T, 0xaa020020);
}

int
aarch64_enc_orr_reg_32_test(TestCtx *T)
{
    test_asm_enc(T, "orr w0, w1, w2");
    return test_aarch64_assert(T, 0x2a020020);
}

int
aarch64_enc_eor_reg_test(TestCtx *T)
{
    test_asm_enc(T, "eor x0, x1, x2");
    return test_aarch64_assert(T, 0xca020020);
}

int
aarch64_enc_eor_reg_32_test(TestCtx *T)
{
    test_asm_enc(T, "eor w0, w1, w2");
    return test_aarch64_assert(T, 0x4a020020);
}

/* ============================================================ MUL / SDIV */

int
aarch64_enc_mul_test(TestCtx *T)
{
    test_asm_enc(T, "mul x0, x1, x2");
    return test_aarch64_assert(T, 0x9b027c20);
}

int
aarch64_enc_mul_32_test(TestCtx *T)
{
    test_asm_enc(T, "mul w0, w1, w2");
    return test_aarch64_assert(T, 0x1b027c20);
}

int
aarch64_enc_sdiv_test(TestCtx *T)
{
    test_asm_enc(T, "sdiv x0, x1, x2");
    return test_aarch64_assert(T, 0x9ac20c20);
}

int
aarch64_enc_sdiv_32_test(TestCtx *T)
{
    test_asm_enc(T, "sdiv w0, w1, w2");
    return test_aarch64_assert(T, 0x1ac20c20);
}

/* ============================================================ LSL / NEG */

int
aarch64_enc_lsl_imm_test(TestCtx *T)
{
    test_asm_enc(T, "lsl x0, x1, #4");
    return test_aarch64_assert(T, 0xd37cec20);
}

int
aarch64_enc_lsl_imm_32_test(TestCtx *T)
{
    test_asm_enc(T, "lsl w0, w1, #3");
    return test_aarch64_assert(T, 0x531d7020);
}

int
aarch64_enc_neg_test(TestCtx *T)
{
    test_asm_enc(T, "neg x0, x1");
    return test_aarch64_assert(T, 0xcb0103e0);
}

int
aarch64_enc_neg_32_test(TestCtx *T)
{
    test_asm_enc(T, "neg w0, w1");
    return test_aarch64_assert(T, 0x4b0103e0);
}

/* ============================================================ CMP / SUBS */

int
aarch64_enc_cmp_reg_test(TestCtx *T)
{
    test_asm_enc(T, "cmp x0, x1");
    return test_aarch64_assert(T, 0xeb01001f);
}

int
aarch64_enc_cmp_reg_32_test(TestCtx *T)
{
    test_asm_enc(T, "cmp w0, w1");
    return test_aarch64_assert(T, 0x6b01001f);
}

int
aarch64_enc_cmp_imm_test(TestCtx *T)
{
    test_asm_enc(T, "cmp x0, #0x10");
    return test_aarch64_assert(T, 0xf100401f);
}

int
aarch64_enc_cmp_imm_32_test(TestCtx *T)
{
    test_asm_enc(T, "cmp w0, #16");
    return test_aarch64_assert(T, 0x7100401f);
}

int
aarch64_enc_subs_reg_test(TestCtx *T)
{
    test_asm_enc(T, "subs x0, x1, x2");
    return test_aarch64_assert(T, 0xeb020020);
}

int
aarch64_enc_subs_reg_32_test(TestCtx *T)
{
    test_asm_enc(T, "subs w0, w1, w2");
    return test_aarch64_assert(T, 0x6b020020);
}

int
aarch64_enc_subs_imm_test(TestCtx *T)
{
    test_asm_enc(T, "subs x0, x1, #4");
    return test_aarch64_assert(T, 0xf1001020);
}

int
aarch64_enc_subs_imm_32_test(TestCtx *T)
{
    test_asm_enc(T, "subs w0, w1, #4");
    return test_aarch64_assert(T, 0x71001020);
}

/* ============================================================ LDR / STR */

int
aarch64_enc_ldr_base_only_test(TestCtx *T)
{
    test_asm_enc(T, "ldr x0, [x1]");
    return test_aarch64_assert(T, 0xf9400020);
}

int
aarch64_enc_ldr_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldr x0, [x1, #16]");
    return test_aarch64_assert(T, 0xf9400820);
}

int
aarch64_enc_str_imm_test(TestCtx *T)
{
    test_asm_enc(T, "str x0, [x1, #16]");
    return test_aarch64_assert(T, 0xf9000820);
}

int
aarch64_enc_ldr32_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldr w0, [x1, #8]");
    return test_aarch64_assert(T, 0xb9400820);
}

int
aarch64_enc_str32_imm_test(TestCtx *T)
{
    test_asm_enc(T, "str w0, [x1, #8]");
    return test_aarch64_assert(T, 0xb9000820);
}

int
aarch64_enc_ldr_pre_test(TestCtx *T)
{
    test_asm_enc(T, "ldr x0, [x1, #8]!");
    return test_aarch64_assert(T, 0xf8408c20);
}

int
aarch64_enc_ldr_post_test(TestCtx *T)
{
    test_asm_enc(T, "ldr x0, [x1], #8");
    return test_aarch64_assert(T, 0xf8408420);
}

int
aarch64_enc_str_pre_test(TestCtx *T)
{
    test_asm_enc(T, "str x0, [x1, #8]!");
    return test_aarch64_assert(T, 0xf8008c20);
}

int
aarch64_enc_str_post_test(TestCtx *T)
{
    test_asm_enc(T, "str x0, [x1], #8");
    return test_aarch64_assert(T, 0xf8008420);
}

int
aarch64_enc_ldr_regoff_test(TestCtx *T)
{
    test_asm_enc(T, "ldr x0, [x1, x2]");
    return test_aarch64_assert(T, 0xf8626820);
}

int
aarch64_enc_ldr_regoff_shifted_test(TestCtx *T)
{
    test_asm_enc(T, "ldr x0, [x1, x2, lsl #3]");
    return test_aarch64_assert(T, 0xf8627820);
}

int
aarch64_enc_str_regoff_test(TestCtx *T)
{
    test_asm_enc(T, "str x0, [x1, x2]");
    return test_aarch64_assert(T, 0xf8226820);
}

/* ============================================================ LDRB / STRB / LDRH / STRH */

int
aarch64_enc_ldrb_base_only_test(TestCtx *T)
{
    test_asm_enc(T, "ldrb w0, [x1]");
    return test_aarch64_assert(T, 0x39400020);
}

int
aarch64_enc_ldrb_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldrb w0, [x1, #1]");
    return test_aarch64_assert(T, 0x39400420);
}

int
aarch64_enc_ldrb_imm_max_test(TestCtx *T)
{
    test_asm_enc(T, "ldrb w0, [x1, #4095]");
    return test_aarch64_assert(T, 0x397ffc20);
}

int
aarch64_enc_ldrb_pre_test(TestCtx *T)
{
    test_asm_enc(T, "ldrb w0, [x1, #1]!");
    return test_aarch64_assert(T, 0x38401c20);
}

int
aarch64_enc_ldrb_post_test(TestCtx *T)
{
    test_asm_enc(T, "ldrb w0, [x1], #1");
    return test_aarch64_assert(T, 0x38401420);
}

int
aarch64_enc_ldrb_regoff_test(TestCtx *T)
{
    test_asm_enc(T, "ldrb w0, [x1, x2]");
    return test_aarch64_assert(T, 0x38626820);
}

int
aarch64_enc_strb_imm_test(TestCtx *T)
{
    test_asm_enc(T, "strb w0, [x1, #1]");
    return test_aarch64_assert(T, 0x39000420);
}

int
aarch64_enc_strb_pre_test(TestCtx *T)
{
    test_asm_enc(T, "strb w0, [x1, #1]!");
    return test_aarch64_assert(T, 0x38001c20);
}

int
aarch64_enc_strb_post_test(TestCtx *T)
{
    test_asm_enc(T, "strb w0, [x1], #1");
    return test_aarch64_assert(T, 0x38001420);
}

int
aarch64_enc_ldrh_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldrh w0, [x1, #2]");
    return test_aarch64_assert(T, 0x79400420);
}

int
aarch64_enc_ldrh_imm_max_test(TestCtx *T)
{
    test_asm_enc(T, "ldrh w0, [x1, #4094]");
    return test_aarch64_assert(T, 0x795ffc20);
}

int
aarch64_enc_ldrh_pre_test(TestCtx *T)
{
    test_asm_enc(T, "ldrh w0, [x1, #2]!");
    return test_aarch64_assert(T, 0x78402c20);
}

int
aarch64_enc_ldrh_post_test(TestCtx *T)
{
    test_asm_enc(T, "ldrh w0, [x1], #2");
    return test_aarch64_assert(T, 0x78402420);
}

int
aarch64_enc_strh_imm_test(TestCtx *T)
{
    test_asm_enc(T, "strh w0, [x1, #2]");
    return test_aarch64_assert(T, 0x79000420);
}

int
aarch64_enc_strh_pre_test(TestCtx *T)
{
    test_asm_enc(T, "strh w0, [x1, #2]!");
    return test_aarch64_assert(T, 0x78002c20);
}

int
aarch64_enc_strh_post_test(TestCtx *T)
{
    test_asm_enc(T, "strh w0, [x1], #2");
    return test_aarch64_assert(T, 0x78002420);
}

/* ============================================================ LDRSB / LDRSH / LDRSW */

int
aarch64_enc_ldrsb_x_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsb x0, [x1, #1]");
    return test_aarch64_assert(T, 0x39800420);
}

int
aarch64_enc_ldrsb_w_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsb w0, [x1, #1]");
    return test_aarch64_assert(T, 0x39c00420);
}

int
aarch64_enc_ldrsb_x_pre_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsb x0, [x1, #1]!");
    return test_aarch64_assert(T, 0x38801c20);
}

int
aarch64_enc_ldrsb_x_post_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsb x0, [x1], #1");
    return test_aarch64_assert(T, 0x38801420);
}

int
aarch64_enc_ldrsb_w_regoff_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsb w0, [x1, x2]");
    return test_aarch64_assert(T, 0x38e26820);
}

int
aarch64_enc_ldrsh_x_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsh x0, [x1, #2]");
    return test_aarch64_assert(T, 0x79800420);
}

int
aarch64_enc_ldrsh_w_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsh w0, [x1, #2]");
    return test_aarch64_assert(T, 0x79c00420);
}

int
aarch64_enc_ldrsh_x_pre_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsh x0, [x1, #2]!");
    return test_aarch64_assert(T, 0x78802c20);
}

int
aarch64_enc_ldrsh_x_post_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsh x0, [x1], #2");
    return test_aarch64_assert(T, 0x78802420);
}

int
aarch64_enc_ldrsh_w_regoff_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsh w0, [x1, x2]");
    return test_aarch64_assert(T, 0x78e26820);
}

int
aarch64_enc_ldrsw_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsw x0, [x1, #4]");
    return test_aarch64_assert(T, 0xb9800420);
}

int
aarch64_enc_ldrsw_pre_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsw x0, [x1, #4]!");
    return test_aarch64_assert(T, 0xb8804c20);
}

int
aarch64_enc_ldrsw_post_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsw x0, [x1], #4");
    return test_aarch64_assert(T, 0xb8804420);
}

int
aarch64_enc_ldrsw_regoff_test(TestCtx *T)
{
    test_asm_enc(T, "ldrsw x0, [x1, x2]");
    return test_aarch64_assert(T, 0xb8a26820);
}

/* `ldur`/`stur` aren't surfaced as their own mnemonics, but `ldr`/`str`
 * with a negative/unaligned offset falls through to the unscaled imm9
 * encoding internally. */
int
aarch64_enc_ldr_neg_offset_test(TestCtx *T)
{
    test_asm_enc(T, "ldr x0, [x1, #-8]");
    return test_aarch64_assert(T, 0xf85f8020);
}

int
aarch64_enc_str_neg_offset_test(TestCtx *T)
{
    test_asm_enc(T, "str x0, [x1, #-8]");
    return test_aarch64_assert(T, 0xf81f8020);
}

/* ============================================================ LDP / STP */

int
aarch64_enc_ldp_test(TestCtx *T)
{
    test_asm_enc(T, "ldp x0, x1, [x2, #16]");
    return test_aarch64_assert(T, 0xa9410440);
}

int
aarch64_enc_stp_test(TestCtx *T)
{
    test_asm_enc(T, "stp x0, x1, [x2, #16]");
    return test_aarch64_assert(T, 0xa9010440);
}

int
aarch64_enc_stp_pre_sp_test(TestCtx *T)
{
    test_asm_enc(T, "stp x0, x1, [sp, #-16]!");
    return test_aarch64_assert(T, 0xa9bf07e0);
}

int
aarch64_enc_ldp_post_sp_test(TestCtx *T)
{
    test_asm_enc(T, "ldp x0, x1, [sp], #16");
    return test_aarch64_assert(T, 0xa8c107e0);
}

int
aarch64_enc_ldp_pre_test(TestCtx *T)
{
    test_asm_enc(T, "ldp x0, x1, [x2, #-32]!");
    return test_aarch64_assert(T, 0xa9fe0440);
}

int
aarch64_enc_stp_post_test(TestCtx *T)
{
    test_asm_enc(T, "stp x0, x1, [x2], #32");
    return test_aarch64_assert(T, 0xa8820440);
}

int
aarch64_enc_ldnp_test(TestCtx *T)
{
    test_asm_enc(T, "ldnp x0, x1, [x2, #16]");
    return test_aarch64_assert(T, 0xa8410440);
}

int
aarch64_enc_stnp_test(TestCtx *T)
{
    test_asm_enc(T, "stnp x0, x1, [x2, #16]");
    return test_aarch64_assert(T, 0xa8010440);
}

/* ============================================================ CBZ / CBNZ */

int
aarch64_enc_cbz_self_test(TestCtx *T)
{
    test_asm_enc(T, "@@1:\n\tcbz x0, @@1");
    return test_aarch64_assert(T, 0xb4000000);
}

int
aarch64_enc_cbnz_self_test(TestCtx *T)
{
    test_asm_enc(T, "@@1:\n\tcbnz x0, @@1");
    return test_aarch64_assert(T, 0xb5000000);
}

int
aarch64_enc_cbz_back_test(TestCtx *T)
{
    test_asm_enc(T, "@@1:\n\tnop\n\tcbz x0, @@1");
    /* nop, then cbz x0, -1 word */
    return test_aarch64_assert(T, 0xd503201f, 0xb4ffffe0);
}

/* ============================================================ Branches */

int
aarch64_enc_b_self_test(TestCtx *T)
{
    test_asm_enc(T, "@@1:\n\tb @@1");
    return test_aarch64_assert(T, 0x14000000);
}

int
aarch64_enc_bl_self_test(TestCtx *T)
{
    test_asm_enc(T, "@@1:\n\tbl @@1");
    return test_aarch64_assert(T, 0x94000000);
}

int
aarch64_enc_b_cond_eq_test(TestCtx *T)
{
    test_asm_enc(T, "@@1:\n\tb.eq @@1");
    return test_aarch64_assert(T, 0x54000000);
}

int
aarch64_enc_b_cond_ne_test(TestCtx *T)
{
    test_asm_enc(T, "@@1:\n\tb.ne @@1");
    return test_aarch64_assert(T, 0x54000001);
}

int
aarch64_enc_b_cond_lt_test(TestCtx *T)
{
    test_asm_enc(T, "@@1:\n\tb.lt @@1");
    return test_aarch64_assert(T, 0x5400000b);
}

int
aarch64_enc_b_cond_gt_test(TestCtx *T)
{
    test_asm_enc(T, "@@1:\n\tb.gt @@1");
    return test_aarch64_assert(T, 0x5400000c);
}

int
aarch64_enc_b_cond_le_test(TestCtx *T)
{
    test_asm_enc(T, "@@1:\n\tb.le @@1");
    return test_aarch64_assert(T, 0x5400000d);
}

int
aarch64_enc_b_cond_ge_test(TestCtx *T)
{
    test_asm_enc(T, "@@1:\n\tb.ge @@1");
    return test_aarch64_assert(T, 0x5400000a);
}

int
aarch64_enc_br_test(TestCtx *T)
{
    test_asm_enc(T, "br x0");
    return test_aarch64_assert(T, 0xd61f0000);
}

int
aarch64_enc_blr_test(TestCtx *T)
{
    test_asm_enc(T, "blr x0");
    return test_aarch64_assert(T, 0xd63f0000);
}

int
aarch64_enc_ret_test(TestCtx *T)
{
    test_asm_enc(T, "ret");
    return test_aarch64_assert(T, 0xd65f03c0);
}

int
aarch64_enc_ret_reg_test(TestCtx *T)
{
    test_asm_enc(T, "ret x30");
    return test_aarch64_assert(T, 0xd65f03c0);
}

/* ============================================================ Misc system */

int
aarch64_enc_nop_test(TestCtx *T)
{
    test_asm_enc(T, "nop");
    return test_aarch64_assert(T, 0xd503201f);
}

int
aarch64_enc_dsb_sy_test(TestCtx *T)
{
    test_asm_enc(T, "dsb sy");
    return test_aarch64_assert(T, 0xd5033f9f);
}

int
aarch64_enc_dmb_ish_test(TestCtx *T)
{
    test_asm_enc(T, "dmb ish");
    return test_aarch64_assert(T, 0xd5033bbf);
}

int
aarch64_enc_isb_test(TestCtx *T)
{
    test_asm_enc(T, "isb");
    return test_aarch64_assert(T, 0xd5033fdf);
}

/* ============================================================ FP arithmetic */

int
aarch64_enc_fadd_s_test(TestCtx *T)
{
    test_asm_enc(T, "fadd s0, s1, s2");
    return test_aarch64_assert(T, 0x1e222820);
}

int
aarch64_enc_fadd_d_test(TestCtx *T)
{
    test_asm_enc(T, "fadd d0, d1, d2");
    return test_aarch64_assert(T, 0x1e622820);
}

int
aarch64_enc_fsub_s_test(TestCtx *T)
{
    test_asm_enc(T, "fsub s0, s1, s2");
    return test_aarch64_assert(T, 0x1e223820);
}

int
aarch64_enc_fmul_s_test(TestCtx *T)
{
    test_asm_enc(T, "fmul s0, s1, s2");
    return test_aarch64_assert(T, 0x1e220820);
}

int
aarch64_enc_fdiv_s_test(TestCtx *T)
{
    test_asm_enc(T, "fdiv s0, s1, s2");
    return test_aarch64_assert(T, 0x1e221820);
}

/* ============================================================ FMOV / FP 1-src */

int
aarch64_enc_fmov_s_reg_test(TestCtx *T)
{
    test_asm_enc(T, "fmov s0, s1");
    return test_aarch64_assert(T, 0x1e204020);
}

int
aarch64_enc_fmov_d_reg_test(TestCtx *T)
{
    test_asm_enc(T, "fmov d0, d1");
    return test_aarch64_assert(T, 0x1e604020);
}

int
aarch64_enc_fmov_d_from_x_test(TestCtx *T)
{
    test_asm_enc(T, "fmov d0, x0");
    return test_aarch64_assert(T, 0x9e670000);
}

int
aarch64_enc_fmov_x_from_d_test(TestCtx *T)
{
    test_asm_enc(T, "fmov x0, d0");
    return test_aarch64_assert(T, 0x9e660000);
}

int
aarch64_enc_fabs_s_test(TestCtx *T)
{
    test_asm_enc(T, "fabs s0, s1");
    return test_aarch64_assert(T, 0x1e20c020);
}

int
aarch64_enc_fneg_d_test(TestCtx *T)
{
    test_asm_enc(T, "fneg d0, d1");
    return test_aarch64_assert(T, 0x1e614020);
}

int
aarch64_enc_fsqrt_s_test(TestCtx *T)
{
    test_asm_enc(T, "fsqrt s0, s1");
    return test_aarch64_assert(T, 0x1e21c020);
}

int
aarch64_enc_fcmp_s_test(TestCtx *T)
{
    test_asm_enc(T, "fcmp s0, s1");
    return test_aarch64_assert(T, 0x1e212000);
}

int
aarch64_enc_fcmpe_d_test(TestCtx *T)
{
    test_asm_enc(T, "fcmpe d0, d1");
    return test_aarch64_assert(T, 0x1e612010);
}

int
aarch64_enc_fcvtzs_test(TestCtx *T)
{
    test_asm_enc(T, "fcvtzs x0, d0");
    return test_aarch64_assert(T, 0x9e780000);
}

int
aarch64_enc_scvtf_test(TestCtx *T)
{
    test_asm_enc(T, "scvtf d0, x0");
    return test_aarch64_assert(T, 0x9e620000);
}

/* ============================================================ MRS / MSR */

int
aarch64_enc_mrs_nzcv_test(TestCtx *T)
{
    test_asm_enc(T, "mrs x0, nzcv");
    return test_aarch64_assert(T, 0xd53b4200);
}

int
aarch64_enc_msr_nzcv_test(TestCtx *T)
{
    test_asm_enc(T, "msr nzcv, x0");
    return test_aarch64_assert(T, 0xd51b4200);
}

/* FPCR holds the rounding mode (RMode, bits 23:22); FPSR the cumulative
 * exception flags. Writing FPCR is how JIT'd code switches rounding. */
int
aarch64_enc_mrs_fpcr_test(TestCtx *T)
{
    test_asm_enc(T, "mrs x0, fpcr");
    return test_aarch64_assert(T, 0xd53b4400);
}

int
aarch64_enc_msr_fpcr_test(TestCtx *T)
{
    test_asm_enc(T, "msr fpcr, x9");
    return test_aarch64_assert(T, 0xd51b4409);
}

int
aarch64_enc_mrs_fpsr_test(TestCtx *T)
{
    test_asm_enc(T, "mrs x3, fpsr");
    return test_aarch64_assert(T, 0xd53b4423);
}

int
aarch64_enc_msr_fpsr_test(TestCtx *T)
{
    test_asm_enc(T, "msr fpsr, x3");
    return test_aarch64_assert(T, 0xd51b4423);
}

/* ============================================================ MOVZ/MOVK/MOVN, LDUR/STUR
 *
 * The explicit wide-move and unscaled load/store mnemonics. `mov` and
 * `ldr`/`str` synthesise these forms; the explicit spellings encode
 * exactly one instruction and let disassembler output re-assemble. */

int
aarch64_enc_movz_test(TestCtx *T)
{
    test_asm_enc(T, "movz x0, #42");
    return test_aarch64_assert(T, 0xd2800540);
}

int
aarch64_enc_movz_shift_test(TestCtx *T)
{
    test_asm_enc(T, "movz x1, #4660, lsl #16");
    return test_aarch64_assert(T, 0xd2a24681);
}

int
aarch64_enc_movk_test(TestCtx *T)
{
    test_asm_enc(T, "movk x0, #32767, lsl #16");
    return test_aarch64_assert(T, 0xf2afffe0);
}

int
aarch64_enc_movn_test(TestCtx *T)
{
    test_asm_enc(T, "movn w0, #0");
    return test_aarch64_assert(T, 0x12800000);
}

int
aarch64_enc_ldur_test(TestCtx *T)
{
    test_asm_enc(T, "ldur x0, [x1, #-4]");
    return test_aarch64_assert(T, 0xf85fc020);
}

int
aarch64_enc_stur_w_test(TestCtx *T)
{
    test_asm_enc(T, "stur w0, [x1, #12]");
    return test_aarch64_assert(T, 0xb800c020);
}

/* ============================================================ Numeric branch targets
 *
 * `#<byte-offset>` PC-relative form, as printed by the disassembler.
 * Closes the disasm -> asm round-trip for branches. */

int
aarch64_enc_b_numeric_test(TestCtx *T)
{
    test_asm_enc(T, "b #+8");
    return test_aarch64_assert(T, 0x14000002);
}

int
aarch64_enc_bl_numeric_test(TestCtx *T)
{
    test_asm_enc(T, "bl #-4");
    return test_aarch64_assert(T, 0x97ffffff);
}

int
aarch64_enc_bcond_numeric_test(TestCtx *T)
{
    test_asm_enc(T, "b.ne #+16");
    return test_aarch64_assert(T, 0x54000081);
}

int
aarch64_enc_cbz_numeric_test(TestCtx *T)
{
    test_asm_enc(T, "cbz x0, #-4");
    return test_aarch64_assert(T, 0xb4ffffe0);
}

int
aarch64_enc_cbnz_numeric_test(TestCtx *T)
{
    test_asm_enc(T, "cbnz w0, #+8");
    return test_aarch64_assert(T, 0x35000040);
}

/* ============================================================ Data directives
 *
 * db/dw/dd/dq emit raw little-endian bytes. The test framework asserts
 * on 32-bit words, so we pick operand sets whose byte streams pack into
 * whole words. */

int
aarch64_enc_db_test(TestCtx *T)
{
    test_asm_enc(T, "db 0x01, 0x02, 0x03, 0x04");
    return test_aarch64_assert(T, 0x04030201);
}

int
aarch64_enc_dw_test(TestCtx *T)
{
    test_asm_enc(T, "dw 0x1234, 0x5678");
    return test_aarch64_assert(T, 0x56781234);
}

int
aarch64_enc_dd_test(TestCtx *T)
{
    test_asm_enc(T, "dd 0x12345678");
    return test_aarch64_assert(T, 0x12345678);
}

int
aarch64_enc_dq_test(TestCtx *T)
{
    test_asm_enc(T, "dq 0x123456789a");
    return test_aarch64_assert(T, 0x3456789a, 0x00000012);
}

/* ============================================================ FP vector arithmetic
 *
 * Three-same FP vector ops: FADD/FSUB/FMUL/FDIV on V<n>.<arr> with the
 * three supported arrangements: 2S, 4S, 2D. The arrangement picks both Q
 * (vector width) and sz (single/double precision); the encoder asserts
 * that all three operands' arrangements match. */

int
aarch64_enc_fadd_v4s_test(TestCtx *T)
{
    test_asm_enc(T, "fadd v0.4s, v1.4s, v2.4s");
    return test_aarch64_assert(T, 0x4e22d420);
}

int
aarch64_enc_fadd_v2s_test(TestCtx *T)
{
    test_asm_enc(T, "fadd v0.2s, v1.2s, v2.2s");
    return test_aarch64_assert(T, 0x0e22d420);
}

int
aarch64_enc_fadd_v2d_test(TestCtx *T)
{
    test_asm_enc(T, "fadd v0.2d, v1.2d, v2.2d");
    return test_aarch64_assert(T, 0x4e62d420);
}

int
aarch64_enc_fsub_v4s_test(TestCtx *T)
{
    test_asm_enc(T, "fsub v0.4s, v1.4s, v2.4s");
    return test_aarch64_assert(T, 0x4ea2d420);
}

int
aarch64_enc_fmul_v4s_test(TestCtx *T)
{
    test_asm_enc(T, "fmul v0.4s, v1.4s, v2.4s");
    return test_aarch64_assert(T, 0x6e22dc20);
}

int
aarch64_enc_fdiv_v4s_test(TestCtx *T)
{
    test_asm_enc(T, "fdiv v0.4s, v1.4s, v2.4s");
    return test_aarch64_assert(T, 0x6e22fc20);
}

/* ============================================================ SIMD load/store
 *
 * `ldr`/`str` dispatched to aarch64_enc_simd_ldst when the data reg is a
 * SIMD scalar (B/H/S/D/Q). The size field in the encoding is selected by
 * register class. */

int
aarch64_enc_ldr_q_test(TestCtx *T)
{
    test_asm_enc(T, "ldr q0, [x1]");
    return test_aarch64_assert(T, 0x3dc00020);
}

int
aarch64_enc_ldr_q_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldr q0, [x1, #16]");
    return test_aarch64_assert(T, 0x3dc00420);
}

int
aarch64_enc_str_q_test(TestCtx *T)
{
    test_asm_enc(T, "str q0, [x1]");
    return test_aarch64_assert(T, 0x3d800020);
}

int
aarch64_enc_ldr_d_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldr d0, [x1, #8]");
    return test_aarch64_assert(T, 0xfd400420);
}

int
aarch64_enc_str_d_test(TestCtx *T)
{
    test_asm_enc(T, "str d0, [x1]");
    return test_aarch64_assert(T, 0xfd000020);
}

int
aarch64_enc_ldr_s_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldr s0, [x1, #4]");
    return test_aarch64_assert(T, 0xbd400420);
}

int
aarch64_enc_ldr_h_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldr h0, [x1, #2]");
    return test_aarch64_assert(T, 0x7d400420);
}

int
aarch64_enc_ldr_b_test(TestCtx *T)
{
    test_asm_enc(T, "ldr b0, [x1]");
    return test_aarch64_assert(T, 0x3d400020);
}

/* ============================================================ SIMD LDP / STP */

int
aarch64_enc_ldp_q_pre_sp_test(TestCtx *T)
{
    test_asm_enc(T, "ldp q0, q1, [sp, #-32]!");
    return test_aarch64_assert(T, 0xadff07e0);
}

int
aarch64_enc_stp_q_test(TestCtx *T)
{
    test_asm_enc(T, "stp q0, q1, [sp, #32]");
    return test_aarch64_assert(T, 0xad0107e0);
}

int
aarch64_enc_ldp_d_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ldp d0, d1, [x2, #16]");
    return test_aarch64_assert(T, 0x6d410440);
}

int
aarch64_enc_stp_s_pre_test(TestCtx *T)
{
    test_asm_enc(T, "stp s0, s1, [x2, #-8]!");
    return test_aarch64_assert(T, 0x2dbf0440);
}

/* ============================================================ FMOV immediate
 *
 * FMOV (immediate) uses the AArch64 8-bit "expanded FP immediate"
 * encoding - a sparse 64-value set (+/-0.125, +/-0.1875, ..., +/-31.0) that
 * round-trips through aarch64_fmov_encode_imm8. */

int
aarch64_enc_fmov_s_imm_1_test(TestCtx *T)
{
    test_asm_enc(T, "fmov s0, #1.0");
    return test_aarch64_assert(T, 0x1e2e1000);
}

int
aarch64_enc_fmov_d_imm_half_test(TestCtx *T)
{
    test_asm_enc(T, "fmov d0, #0.5");
    return test_aarch64_assert(T, 0x1e6c1000);
}

int
aarch64_enc_fmov_d_imm_2_test(TestCtx *T)
{
    test_asm_enc(T, "fmov d0, #2.0");
    return test_aarch64_assert(T, 0x1e601000);
}

int
aarch64_enc_fmov_s_imm_neg1_test(TestCtx *T)
{
    test_asm_enc(T, "fmov s0, #-1.0");
    return test_aarch64_assert(T, 0x1e3e1000);
}

/* ============================================================ Half-precision FMOV */

int
aarch64_enc_fmov_h_reg_test(TestCtx *T)
{
    test_asm_enc(T, "fmov h0, h1");
    return test_aarch64_assert(T, 0x1ee04020);
}

int
aarch64_enc_fmov_h_from_w_test(TestCtx *T)
{
    test_asm_enc(T, "fmov h0, w0");
    return test_aarch64_assert(T, 0x1ee70000);
}

int
aarch64_enc_fmov_w_from_h_test(TestCtx *T)
{
    test_asm_enc(T, "fmov w0, h0");
    return test_aarch64_assert(T, 0x1ee60000);
}

/* ============================================================ BF16 family
 *
 * Brain-float 16 extension (FEAT_BF16). All ops below were assembled by
 * `as -arch arm64` with `.arch armv8.6-a+bf16+fp16` enabled. */

int
aarch64_enc_bfdot_v4s_test(TestCtx *T)
{
    test_asm_enc(T, "bfdot v0.4s, v1.8h, v2.8h");
    return test_aarch64_assert(T, 0x6e42fc20);
}

int
aarch64_enc_bfdot_v2s_test(TestCtx *T)
{
    test_asm_enc(T, "bfdot v0.2s, v1.4h, v2.4h");
    return test_aarch64_assert(T, 0x2e42fc20);
}

int
aarch64_enc_bfdot_indexed_test(TestCtx *T)
{
    test_asm_enc(T, "bfdot v0.4s, v1.8h, v2.2h[1]");
    return test_aarch64_assert(T, 0x4f62f020);
}

int
aarch64_enc_bfmlalb_test(TestCtx *T)
{
    test_asm_enc(T, "bfmlalb v0.4s, v1.8h, v2.8h");
    return test_aarch64_assert(T, 0x2ec2fc20);
}

int
aarch64_enc_bfmlalt_test(TestCtx *T)
{
    test_asm_enc(T, "bfmlalt v0.4s, v1.8h, v2.8h");
    return test_aarch64_assert(T, 0x6ec2fc20);
}

int
aarch64_enc_bfmmla_test(TestCtx *T)
{
    test_asm_enc(T, "bfmmla v0.4s, v1.8h, v2.8h");
    return test_aarch64_assert(T, 0x6e42ec20);
}

int
aarch64_enc_bfcvt_scalar_test(TestCtx *T)
{
    test_asm_enc(T, "bfcvt h0, s0");
    return test_aarch64_assert(T, 0x1e634000);
}

int
aarch64_enc_bfcvtn_test(TestCtx *T)
{
    test_asm_enc(T, "bfcvtn v0.4h, v1.4s");
    return test_aarch64_assert(T, 0x0ea16820);
}

int
aarch64_enc_bfcvtn2_test(TestCtx *T)
{
    test_asm_enc(T, "bfcvtn2 v0.8h, v1.4s");
    return test_aarch64_assert(T, 0x4ea16820);
}

int
main(void)
{
    TestCtx T;
    test_ctx_init(&T, TASM_ARCH_ARM64);

    EncoderTest tests[] = {
        PrepTest(aarch64_enc_mov_small_imm_test),
        PrepTest(aarch64_enc_mov_large_imm_test),
        PrepTest(aarch64_enc_mov_small_32bit_imm_test),
        PrepTest(aarch64_enc_mov_64bit_reg_test),
        PrepTest(aarch64_enc_mov_64bit_imm_reg_test),
        PrepTest(aarch64_enc_mov_32bit_reg_test),
        PrepTest(aarch64_enc_mov_32bit_imm_reg_test),

        PrepTest(aarch64_enc_add_reg_test),
        PrepTest(aarch64_enc_add_reg_32_test),
        PrepTest(aarch64_enc_add_imm_test),
        PrepTest(aarch64_enc_sub_reg_test),
        PrepTest(aarch64_enc_sub_reg_32_test),
        PrepTest(aarch64_enc_sub_imm_test),
        PrepTest(aarch64_enc_add_reg_shifted_lsl_test),
        PrepTest(aarch64_enc_sub_reg_shifted_lsr_test),
        PrepTest(aarch64_enc_add_reg_shifted_asr_test),

        PrepTest(aarch64_enc_and_reg_test),
        PrepTest(aarch64_enc_and_reg_32_test),
        PrepTest(aarch64_enc_orr_reg_test),
        PrepTest(aarch64_enc_orr_reg_32_test),
        PrepTest(aarch64_enc_eor_reg_test),
        PrepTest(aarch64_enc_eor_reg_32_test),

        PrepTest(aarch64_enc_mul_test),
        PrepTest(aarch64_enc_mul_32_test),
        PrepTest(aarch64_enc_sdiv_test),
        PrepTest(aarch64_enc_sdiv_32_test),

        PrepTest(aarch64_enc_lsl_imm_test),
        PrepTest(aarch64_enc_lsl_imm_32_test),
        PrepTest(aarch64_enc_neg_test),
        PrepTest(aarch64_enc_neg_32_test),

        PrepTest(aarch64_enc_cmp_reg_test),
        PrepTest(aarch64_enc_cmp_reg_32_test),
        PrepTest(aarch64_enc_cmp_imm_test),
        PrepTest(aarch64_enc_cmp_imm_32_test),
        PrepTest(aarch64_enc_subs_reg_test),
        PrepTest(aarch64_enc_subs_reg_32_test),
        PrepTest(aarch64_enc_subs_imm_test),
        PrepTest(aarch64_enc_subs_imm_32_test),

        PrepTest(aarch64_enc_ldr_base_only_test),
        PrepTest(aarch64_enc_ldr_imm_test),
        PrepTest(aarch64_enc_str_imm_test),
        PrepTest(aarch64_enc_ldr32_imm_test),
        PrepTest(aarch64_enc_str32_imm_test),
        PrepTest(aarch64_enc_ldr_pre_test),
        PrepTest(aarch64_enc_ldr_post_test),
        PrepTest(aarch64_enc_str_pre_test),
        PrepTest(aarch64_enc_str_post_test),
        PrepTest(aarch64_enc_ldr_regoff_test),
        PrepTest(aarch64_enc_ldr_regoff_shifted_test),
        PrepTest(aarch64_enc_str_regoff_test),
        PrepTest(aarch64_enc_ldr_neg_offset_test),
        PrepTest(aarch64_enc_str_neg_offset_test),

        PrepTest(aarch64_enc_ldrb_base_only_test),
        PrepTest(aarch64_enc_ldrb_imm_test),
        PrepTest(aarch64_enc_ldrb_imm_max_test),
        PrepTest(aarch64_enc_ldrb_pre_test),
        PrepTest(aarch64_enc_ldrb_post_test),
        PrepTest(aarch64_enc_ldrb_regoff_test),
        PrepTest(aarch64_enc_strb_imm_test),
        PrepTest(aarch64_enc_strb_pre_test),
        PrepTest(aarch64_enc_strb_post_test),
        PrepTest(aarch64_enc_ldrh_imm_test),
        PrepTest(aarch64_enc_ldrh_imm_max_test),
        PrepTest(aarch64_enc_ldrh_pre_test),
        PrepTest(aarch64_enc_ldrh_post_test),
        PrepTest(aarch64_enc_strh_imm_test),
        PrepTest(aarch64_enc_strh_pre_test),
        PrepTest(aarch64_enc_strh_post_test),

        PrepTest(aarch64_enc_ldrsb_x_imm_test),
        PrepTest(aarch64_enc_ldrsb_w_imm_test),
        PrepTest(aarch64_enc_ldrsb_x_pre_test),
        PrepTest(aarch64_enc_ldrsb_x_post_test),
        PrepTest(aarch64_enc_ldrsb_w_regoff_test),
        PrepTest(aarch64_enc_ldrsh_x_imm_test),
        PrepTest(aarch64_enc_ldrsh_w_imm_test),
        PrepTest(aarch64_enc_ldrsh_x_pre_test),
        PrepTest(aarch64_enc_ldrsh_x_post_test),
        PrepTest(aarch64_enc_ldrsh_w_regoff_test),
        PrepTest(aarch64_enc_ldrsw_imm_test),
        PrepTest(aarch64_enc_ldrsw_pre_test),
        PrepTest(aarch64_enc_ldrsw_post_test),
        PrepTest(aarch64_enc_ldrsw_regoff_test),

        PrepTest(aarch64_enc_ldp_test),
        PrepTest(aarch64_enc_stp_test),
        PrepTest(aarch64_enc_stp_pre_sp_test),
        PrepTest(aarch64_enc_ldp_post_sp_test),
        PrepTest(aarch64_enc_ldp_pre_test),
        PrepTest(aarch64_enc_stp_post_test),
        PrepTest(aarch64_enc_ldnp_test),
        PrepTest(aarch64_enc_stnp_test),

        PrepTest(aarch64_enc_cbz_self_test),
        PrepTest(aarch64_enc_cbnz_self_test),
        PrepTest(aarch64_enc_cbz_back_test),

        PrepTest(aarch64_enc_b_self_test),
        PrepTest(aarch64_enc_bl_self_test),
        PrepTest(aarch64_enc_b_cond_eq_test),
        PrepTest(aarch64_enc_b_cond_ne_test),
        PrepTest(aarch64_enc_b_cond_lt_test),
        PrepTest(aarch64_enc_b_cond_gt_test),
        PrepTest(aarch64_enc_b_cond_le_test),
        PrepTest(aarch64_enc_b_cond_ge_test),
        PrepTest(aarch64_enc_br_test),
        PrepTest(aarch64_enc_blr_test),
        PrepTest(aarch64_enc_ret_test),
        PrepTest(aarch64_enc_ret_reg_test),

        PrepTest(aarch64_enc_nop_test),
        PrepTest(aarch64_enc_dsb_sy_test),
        PrepTest(aarch64_enc_dmb_ish_test),
        PrepTest(aarch64_enc_isb_test),

        PrepTest(aarch64_enc_fadd_s_test),
        PrepTest(aarch64_enc_fadd_d_test),
        PrepTest(aarch64_enc_fsub_s_test),
        PrepTest(aarch64_enc_fmul_s_test),
        PrepTest(aarch64_enc_fdiv_s_test),

        PrepTest(aarch64_enc_fmov_s_reg_test),
        PrepTest(aarch64_enc_fmov_d_reg_test),
        PrepTest(aarch64_enc_fmov_d_from_x_test),
        PrepTest(aarch64_enc_fmov_x_from_d_test),
        PrepTest(aarch64_enc_fabs_s_test),
        PrepTest(aarch64_enc_fneg_d_test),
        PrepTest(aarch64_enc_fsqrt_s_test),
        PrepTest(aarch64_enc_fcmp_s_test),
        PrepTest(aarch64_enc_fcmpe_d_test),
        PrepTest(aarch64_enc_fcvtzs_test),
        PrepTest(aarch64_enc_scvtf_test),

        PrepTest(aarch64_enc_mrs_nzcv_test),
        PrepTest(aarch64_enc_msr_nzcv_test),
        PrepTest(aarch64_enc_mrs_fpcr_test),
        PrepTest(aarch64_enc_msr_fpcr_test),
        PrepTest(aarch64_enc_mrs_fpsr_test),
        PrepTest(aarch64_enc_msr_fpsr_test),
        PrepTest(aarch64_enc_movz_test),
        PrepTest(aarch64_enc_movz_shift_test),
        PrepTest(aarch64_enc_movk_test),
        PrepTest(aarch64_enc_movn_test),
        PrepTest(aarch64_enc_ldur_test),
        PrepTest(aarch64_enc_stur_w_test),
        PrepTest(aarch64_enc_b_numeric_test),
        PrepTest(aarch64_enc_bl_numeric_test),
        PrepTest(aarch64_enc_bcond_numeric_test),
        PrepTest(aarch64_enc_cbz_numeric_test),
        PrepTest(aarch64_enc_cbnz_numeric_test),

        PrepTest(aarch64_enc_fadd_v4s_test),
        PrepTest(aarch64_enc_fadd_v2s_test),
        PrepTest(aarch64_enc_fadd_v2d_test),
        PrepTest(aarch64_enc_fsub_v4s_test),
        PrepTest(aarch64_enc_fmul_v4s_test),
        PrepTest(aarch64_enc_fdiv_v4s_test),

        PrepTest(aarch64_enc_ldr_q_test),
        PrepTest(aarch64_enc_ldr_q_imm_test),
        PrepTest(aarch64_enc_str_q_test),
        PrepTest(aarch64_enc_ldr_d_imm_test),
        PrepTest(aarch64_enc_str_d_test),
        PrepTest(aarch64_enc_ldr_s_imm_test),
        PrepTest(aarch64_enc_ldr_h_imm_test),
        PrepTest(aarch64_enc_ldr_b_test),

        PrepTest(aarch64_enc_ldp_q_pre_sp_test),
        PrepTest(aarch64_enc_stp_q_test),
        PrepTest(aarch64_enc_ldp_d_imm_test),
        PrepTest(aarch64_enc_stp_s_pre_test),

        PrepTest(aarch64_enc_fmov_s_imm_1_test),
        PrepTest(aarch64_enc_fmov_d_imm_half_test),
        PrepTest(aarch64_enc_fmov_d_imm_2_test),
        PrepTest(aarch64_enc_fmov_s_imm_neg1_test),

        PrepTest(aarch64_enc_fmov_h_reg_test),
        PrepTest(aarch64_enc_fmov_h_from_w_test),
        PrepTest(aarch64_enc_fmov_w_from_h_test),

        PrepTest(aarch64_enc_bfdot_v4s_test),
        PrepTest(aarch64_enc_bfdot_v2s_test),
        PrepTest(aarch64_enc_bfdot_indexed_test),
        PrepTest(aarch64_enc_bfmlalb_test),
        PrepTest(aarch64_enc_bfmlalt_test),
        PrepTest(aarch64_enc_bfmmla_test),
        PrepTest(aarch64_enc_bfcvt_scalar_test),
        PrepTest(aarch64_enc_bfcvtn_test),
        PrepTest(aarch64_enc_bfcvtn2_test),

        PrepTest(aarch64_enc_db_test),
        PrepTest(aarch64_enc_dw_test),
        PrepTest(aarch64_enc_dd_test),
        PrepTest(aarch64_enc_dq_test),
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
            test_aarch64_dump_failure(&T);
            failed++;
        }
        test_asm_enc_free(&T);
    }

    printf("\n%d/%d passed\n", n - failed, n);
    return failed ? 1 : 0;
}
