/* Disassembler tests for AArch64. Each test encodes one instruction
 * via our encoders, then disassembles the bytes back into a string and
 * checks the result.
 *
 * That covers the most useful roundtrip: encode -> disasm -> compare,
 * which catches both sides drifting. */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "asm_enc.h"
#include "dis_arm64.h"
#include "enc_arm64.h"

typedef int (*TestFn)(void);
typedef struct {
    const char *name;
    TestFn fn;
} DisTest;
#define PrepTest(n) { #n, n }

/* Encode whatever `emit_fn(e)` puts down, then disassemble the first
 * 4-byte word and compare against `want`. The encoder is given a fresh
 * AsmEnc; we own its lifetime. */
static int
check_one(uint32_t insn_word, const char *want)
{
    char buf[128];
    int ok = aarch64_disasm(insn_word, buf, sizeof buf);
    if (!ok) {
        fprintf(stderr, "  dis returned 0 for 0x%08x; got '%s' (want '%s')\n",
                insn_word, buf, want);
        return 0;
    }
    if (strcmp(buf, want) != 0) {
        fprintf(stderr, "  insn 0x%08x: got '%s', want '%s'\n", insn_word, buf,
                want);
        return 0;
    }
    return 1;
}

/* Encode by calling `emit_fn` then read back the first word from enc. */
static uint32_t
encode(void (*emit_fn)(AsmEnc *e))
{
    AsmEnc e;
    asm_enc_init(&e);
    emit_fn(&e);
    uint32_t w = (uint32_t)e.bytes[0] | ((uint32_t)e.bytes[1] << 8) |
            ((uint32_t)e.bytes[2] << 16) | ((uint32_t)e.bytes[3] << 24);
    asm_enc_free(&e);
    return w;
}

/* ----------------- individual tests ------------------ */

static void
e_mov_imm_42(AsmEnc *e)
{
    aarch64_enc_mov_imm64(e, A_X0, 42);
}
static int
test_aarch64_dis_mov_imm_42(void)
{
    return check_one(encode(e_mov_imm_42), "mov x0, #42");
}

static void
e_mov_imm_high(AsmEnc *e)
{
    aarch64_enc_mov_imm64(e, A_X1, 0x12340000ULL);
}
static int
test_aarch64_dis_mov_imm_high(void)
{
    /* movz x1, #0x1234, lsl #16; no movk because the rest is 0. The
     * preferred disassembly is the MOV alias with the shifted value. */
    char buf[128];
    aarch64_disasm(encode(e_mov_imm_high), buf, sizeof buf);
    return strcmp(buf, "mov x1, #305397760") == 0;
}

static void
e_add_imm(AsmEnc *e)
{
    aarch64_enc_add_imm(e, 1, A_X0, A_X1, 32);
}
static int
test_aarch64_dis_add_imm(void)
{
    return check_one(encode(e_add_imm), "add x0, x1, #32");
}

static void
e_sub_imm(AsmEnc *e)
{
    aarch64_enc_sub_imm(e, 1, A_X2, A_X3, 4096);
}
static int
test_aarch64_dis_sub_imm(void)
{
    return check_one(encode(e_sub_imm), "sub x2, x3, #4096");
}

static void
e_add_reg(AsmEnc *e)
{
    aarch64_enc_add_reg(e, 1, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_add_reg(void)
{
    return check_one(encode(e_add_reg), "add x0, x1, x2");
}

static void
e_sub_reg_w(AsmEnc *e)
{
    aarch64_enc_sub_reg(e, 0, A_X4, A_X5, A_X6);
}
static int
test_aarch64_dis_sub_reg_w(void)
{
    return check_one(encode(e_sub_reg_w), "sub w4, w5, w6");
}

static void
e_neg(AsmEnc *e)
{
    aarch64_enc_sub_reg(e, 1, A_X9, A_XZR, A_X9);
}
static int
test_aarch64_dis_neg(void)
{
    return check_one(encode(e_neg), "neg x9, x9");
}

static void
e_mul(AsmEnc *e)
{
    aarch64_enc_mul(e, 1, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_mul(void)
{
    return check_one(encode(e_mul), "mul x0, x1, x2");
}

static void
e_and(AsmEnc *e)
{
    aarch64_enc_and_reg(e, 1, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_and(void)
{
    return check_one(encode(e_and), "and x0, x1, x2");
}

static void
e_orr_w(AsmEnc *e)
{
    aarch64_enc_orr_reg(e, 0, A_X3, A_X4, A_X5);
}
static int
test_aarch64_dis_orr_w(void)
{
    return check_one(encode(e_orr_w), "orr w3, w4, w5");
}

static void
e_eor(AsmEnc *e)
{
    aarch64_enc_eor_reg(e, 1, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_eor(void)
{
    return check_one(encode(e_eor), "eor x0, x1, x2");
}

static void
e_lsl(AsmEnc *e)
{
    aarch64_enc_lslv(e, 1, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_lsl(void)
{
    return check_one(encode(e_lsl), "lsl x0, x1, x2");
}

static void
e_lsr(AsmEnc *e)
{
    aarch64_enc_lsrv(e, 0, A_X3, A_X4, A_X5);
}
static int
test_aarch64_dis_lsr(void)
{
    return check_one(encode(e_lsr), "lsr w3, w4, w5");
}

static void
e_cmp(AsmEnc *e)
{
    aarch64_enc_cmp_reg(e, 1, A_X1, A_X2);
}
static int
test_aarch64_dis_cmp(void)
{
    return check_one(encode(e_cmp), "cmp x1, x2");
}

static void
e_cset(AsmEnc *e)
{
    aarch64_enc_cset(e, A_X9, A_LT);
}
static int
test_aarch64_dis_cset(void)
{
    return check_one(encode(e_cset), "cset x9, lt");
}

static void
e_cset_eq(AsmEnc *e)
{
    aarch64_enc_cset(e, A_X3, A_EQ);
}
static int
test_aarch64_dis_cset_eq(void)
{
    return check_one(encode(e_cset_eq), "cset x3, eq");
}

static void
e_b(AsmEnc *e)
{
    aarch64_enc_b(e, 8);
} /* +8 words = +32 bytes */
static int
test_aarch64_dis_b(void)
{
    return check_one(encode(e_b), "b #+32");
}

static void
e_bl(AsmEnc *e)
{
    aarch64_enc_bl(e, -4);
}
static int
test_aarch64_dis_bl(void)
{
    return check_one(encode(e_bl), "bl #-16");
}

static void
e_b_cond(AsmEnc *e)
{
    aarch64_enc_b_cond(e, A_NE, 2);
}
static int
test_aarch64_dis_b_cond(void)
{
    return check_one(encode(e_b_cond), "b.ne #+8");
}

static void
e_ret(AsmEnc *e)
{
    aarch64_enc_ret(e);
}
static int
test_aarch64_dis_ret(void)
{
    return check_one(encode(e_ret), "ret");
}

static void
e_ldr(AsmEnc *e)
{
    aarch64_enc_ldr_imm(e, A_X0, A_SP, 16);
}
static int
test_aarch64_dis_ldr(void)
{
    return check_one(encode(e_ldr), "ldr x0, [sp, #16]");
}

static void
e_str(AsmEnc *e)
{
    aarch64_enc_str_imm(e, A_X1, A_SP, 24);
}
static int
test_aarch64_dis_str(void)
{
    return check_one(encode(e_str), "str x1, [sp, #24]");
}

static void
e_ldr_zero(AsmEnc *e)
{
    aarch64_enc_ldr_imm(e, A_X5, A_X6, 0);
}
static int
test_aarch64_dis_ldr_zero(void)
{
    return check_one(encode(e_ldr_zero), "ldr x5, [x6]");
}

static void
e_ldrb(AsmEnc *e)
{
    aarch64_enc_ldrb_imm(e, A_X0, A_X1, 3);
}
static int
test_aarch64_dis_ldrb(void)
{
    return check_one(encode(e_ldrb), "ldrb w0, [x1, #3]");
}

static void
e_ldrh(AsmEnc *e)
{
    aarch64_enc_ldrh_imm(e, A_X2, A_X3, 6);
}
static int
test_aarch64_dis_ldrh(void)
{
    return check_one(encode(e_ldrh), "ldrh w2, [x3, #6]");
}

static void
e_ldr32(AsmEnc *e)
{
    aarch64_enc_ldr32_imm_gpr(e, A_X4, A_SP, 12);
}
static int
test_aarch64_dis_ldr32(void)
{
    return check_one(encode(e_ldr32), "ldr w4, [sp, #12]");
}

static void
e_str32(AsmEnc *e)
{
    aarch64_enc_str32_imm_gpr(e, A_X4, A_SP, 12);
}
static int
test_aarch64_dis_str32(void)
{
    return check_one(encode(e_str32), "str w4, [sp, #12]");
}

static void
e_strb(AsmEnc *e)
{
    aarch64_enc_strb_imm(e, A_X0, A_X1, 5);
}
static int
test_aarch64_dis_strb(void)
{
    return check_one(encode(e_strb), "strb w0, [x1, #5]");
}

static void
e_strh(AsmEnc *e)
{
    aarch64_enc_strh_imm(e, A_X0, A_X1, 6);
}
static int
test_aarch64_dis_strh(void)
{
    return check_one(encode(e_strh), "strh w0, [x1, #6]");
}

static void
e_ldrsb_x(AsmEnc *e)
{
    aarch64_enc_ldrsb_imm(e, 1, A_X0, A_X1, 2);
}
static int
test_aarch64_dis_ldrsb_x(void)
{
    return check_one(encode(e_ldrsb_x), "ldrsb x0, [x1, #2]");
}

static void
e_ldrsh_x(AsmEnc *e)
{
    aarch64_enc_ldrsh_imm(e, 1, A_X2, A_X3, 4);
}
static int
test_aarch64_dis_ldrsh_x(void)
{
    return check_one(encode(e_ldrsh_x), "ldrsh x2, [x3, #4]");
}

static void
e_ldrsw(AsmEnc *e)
{
    aarch64_enc_ldrsw_imm(e, A_X4, A_X5, 8);
}
static int
test_aarch64_dis_ldrsw(void)
{
    return check_one(encode(e_ldrsw), "ldrsw x4, [x5, #8]");
}

static void
e_stp_pre(AsmEnc *e)
{
    aarch64_enc_stp_pre(e, A_FP, A_LR, A_SP, -16);
}
static int
test_aarch64_dis_stp_pre(void)
{
    return check_one(encode(e_stp_pre), "stp x29, x30, [sp, #-16]!");
}

static void
e_ldp_post(AsmEnc *e)
{
    aarch64_enc_ldp_post(e, A_FP, A_LR, A_SP, 16);
}
static int
test_aarch64_dis_ldp_post(void)
{
    return check_one(encode(e_ldp_post), "ldp x29, x30, [sp], #16");
}

/* CBZ/CBNZ aren't emitted via a public helper. Test with the raw
 * encoding. */
static int
test_aarch64_dis_cbnz(void)
{
    uint32_t w = 0xB5000000u | (4 << 5) | A_X9; /* cbnz x9, #+16 */
    return check_one(w, "cbnz x9, #+16");
}

static int
test_aarch64_dis_cbz_w(void)
{
    /* cbz w0, #-8: sf=0, op=0, imm19=-2 */
    uint32_t imm19 = (uint32_t)((-2) & 0x7FFFF);
    uint32_t w = 0x34000000u | (imm19 << 5) | A_X0;
    return check_one(w, "cbz w0, #-8");
}

/* Aliases for SXT and UXT via SBFM/UBFM, as emitted by the IR's
 * emit_extend_low_n helper. */
static int
test_aarch64_dis_sxtb(void)
{
    /* sbfm x10, x9, #0, #7 -> sxtb x10, w9 */
    uint32_t w = 0x93400000u | (7 << 10) | (A_X9 << 5) | A_X10;
    return check_one(w, "sxtb x10, w9");
}

static int
test_aarch64_dis_sxth(void)
{
    uint32_t w = 0x93400000u | (15 << 10) | (A_X9 << 5) | A_X10;
    return check_one(w, "sxth x10, w9");
}

static int
test_aarch64_dis_sxtw(void)
{
    uint32_t w = 0x93400000u | (31 << 10) | (A_X9 << 5) | A_X10;
    return check_one(w, "sxtw x10, w9");
}

static int
test_aarch64_dis_uxtb(void)
{
    uint32_t w = 0xD3400000u | (7 << 10) | (A_X9 << 5) | A_X10;
    return check_one(w, "uxtb x10, w9");
}

static int
test_aarch64_dis_uxth(void)
{
    uint32_t w = 0xD3400000u | (15 << 10) | (A_X9 << 5) | A_X10;
    return check_one(w, "uxth x10, w9");
}

/* MOV reg alias: orr xd, xzr, xn. */
static void
e_mov_reg(AsmEnc *e)
{
    aarch64_enc_orr_reg(e, 1, A_X10, A_XZR, A_X9);
}

static int
test_aarch64_dis_mov_reg(void)
{
    return check_one(encode(e_mov_reg), "mov x10, x9");
}

/* ADR placeholder + nonzero displacement. */
static int
test_aarch64_dis_adr(void)
{
    /* adr x9, +#100: encoding 0x10000000 | (immlo<<29) | (immhi<<5) | Rd
     * imm = 100; immlo = bits[1:0] = 0; immhi = bits[20:2] = 25. */
    int32_t off = 100;
    uint32_t immlo = ((uint32_t)off & 0x3) << 29;
    uint32_t immhi = (((uint32_t)off >> 2) & 0x7FFFF) << 5;
    uint32_t w = 0x10000000u | immlo | immhi | A_X9;
    return check_one(w, "adr x9, #+100");
}

/* Unknown bit pattern falls through to `.word`. */
static int
test_aarch64_dis_unknown(void)
{
    char buf[128];
    int rc = aarch64_disasm(0x00000000, buf, sizeof buf);
    if (rc != 0) return 0;
    return strcmp(buf, ".word 0x00000000") == 0;
}

/* ADD imm with sh=1 (operand shifted left 12 bits). Covers the encoder
 * fix where values >= 4096 must set the sh bit. */
static void
e_add_imm_sh(AsmEnc *e)
{
    aarch64_enc_add_imm(e, 1, A_SP, A_SP, 8192);
}
static int
test_aarch64_dis_add_imm_sh(void)
{
    return check_one(encode(e_add_imm_sh), "add sp, sp, #8192");
}

static void
e_sub_imm_sh(AsmEnc *e)
{
    aarch64_enc_sub_imm(e, 1, A_SP, A_SP, 4096);
}
static int
test_aarch64_dis_sub_imm_sh(void)
{
    return check_one(encode(e_sub_imm_sh), "sub sp, sp, #4096");
}

/* B.cond for several condition codes, ensuring the cond-name table is
 * indexed correctly. */
#define BCOND_TEST(name, cc, mnem)                               \
    static void e_bc_##name(AsmEnc *e)                           \
    {                                                            \
        aarch64_enc_b_cond(e, cc, 1);                            \
    }                                                            \
    static int test_aarch64_dis_bc_##name(void)                  \
    {                                                            \
        return check_one(encode(e_bc_##name), "b." mnem " #+4"); \
    }

BCOND_TEST(eq, A_EQ, "eq")
BCOND_TEST(ne, A_NE, "ne")
BCOND_TEST(cs, A_CS, "cs")
BCOND_TEST(cc, A_CC, "cc")
BCOND_TEST(hi, A_HI, "hi")
BCOND_TEST(ls, A_LS, "ls")
BCOND_TEST(ge, A_GE, "ge")
BCOND_TEST(le, A_LE, "le")
BCOND_TEST(gt, A_GT, "gt")

/* Bit-field aliases. */
static int
test_aarch64_dis_lsl_imm(void)
{
    /* lsl x3, x4, #5  ==  ubfm x3, x4, #(64-5), #(63-5)
     *                  ==  ubfm x3, x4, #59, #58 */
    uint32_t w = 0xD3400000u | (59u << 16) | (58u << 10) | (4u << 5) | 3u;
    return check_one(w, "lsl x3, x4, #5");
}
static int
test_aarch64_dis_lsr_imm(void)
{
    /* lsr x3, x4, #5  ==  ubfm x3, x4, #5, #63 */
    uint32_t w = 0xD3400000u | (5u << 16) | (63u << 10) | (4u << 5) | 3u;
    return check_one(w, "lsr x3, x4, #5");
}
static int
test_aarch64_dis_asr_imm(void)
{
    /* asr x3, x4, #5  ==  sbfm x3, x4, #5, #63 */
    uint32_t w = 0x93400000u | (5u << 16) | (63u << 10) | (4u << 5) | 3u;
    return check_one(w, "asr x3, x4, #5");
}
static int
test_aarch64_dis_ubfx(void)
{
    /* ubfx x3, x4, #4, #8  ==  ubfm x3, x4, #4, #11 */
    uint32_t w = 0xD3400000u | (4u << 16) | (11u << 10) | (4u << 5) | 3u;
    return check_one(w, "ubfx x3, x4, #4, #8");
}
static int
test_aarch64_dis_sbfx(void)
{
    /* sbfx x3, x4, #2, #6  ==  sbfm x3, x4, #2, #7 */
    uint32_t w = 0x93400000u | (2u << 16) | (7u << 10) | (4u << 5) | 3u;
    return check_one(w, "sbfx x3, x4, #2, #6");
}

/* MVN x0, x1 = ORN x0, xzr, x1. */
static int
test_aarch64_dis_mvn(void)
{
    /* ORN: opc=01, N=1, shift=0, imm6=0, Rn=XZR(31), Rm=1, Rd=0
     *  encoding base = 0xAA200000 | (Rm<<16) | (XZR<<5) | Rd
     *  with shift=0, imm6=0. */
    uint32_t w = 0xAA200000u | (1u << 16) | (31u << 5) | 0u;
    return check_one(w, "mvn x0, x1");
}

/* TST x1, x2  = ANDS xzr, x1, x2. */
static int
test_aarch64_dis_tst(void)
{
    /* ANDS reg: opc=11, N=0. Base = 0xEA000000.  */
    uint32_t w = 0xEA000000u | (2u << 16) | (1u << 5) | 31u;
    return check_one(w, "tst x1, x2");
}

/* BIC reg: opc=00, N=1. */
static int
test_aarch64_dis_bic(void)
{
    /* bic x0, x1, x2 -> 0x8A200000 | Rm<<16 | Rn<<5 | Rd */
    uint32_t w = 0x8A200000u | (2u << 16) | (1u << 5) | 0u;
    return check_one(w, "bic x0, x1, x2");
}

/* BR x16: JIT veneer style. */
static int
test_aarch64_dis_br(void)
{
    uint32_t w = 0xD61F0000u | (16u << 5);
    return check_one(w, "br x16");
}

/* NOP */
static int
test_aarch64_dis_nop(void)
{
    return check_one(0xD503201Fu, "nop");
}

/* Shifted reg add: add x0, x1, x2, lsl #4 */
static int
test_aarch64_dis_add_reg_shifted(void)
{
    /* sf=1, op=0, S=0, opc=01011, shift=00 (lsl), N=0, Rm=2, imm6=4, Rn=1, Rd=0
     */
    uint32_t w = 0x8B000000u | (0u << 22) | (2u << 16) | (4u << 10) |
            (1u << 5) | 0u;
    return check_one(w, "add x0, x1, x2, lsl #4");
}

/* CMN imm alias: ADDS xzr, xn, #imm. Encode via adds_imm? No - let's
 * just hand-encode. cmn x1, #5 = adds xzr, x1, #5.
 * Base 0xB1000000 | imm12<<10 | Rn<<5 | Rd=31. */
static int
test_aarch64_dis_cmn_imm(void)
{
    uint32_t w = 0xB1000000u | (5u << 10) | (1u << 5) | 31u;
    return check_one(w, "cmn x1, #5");
}

/* CMP imm alias: SUBS xzr, xn, #imm. Base 0xF1000000. */
static int
test_aarch64_dis_cmp_imm(void)
{
    uint32_t w = 0xF1000000u | (10u << 10) | (3u << 5) | 31u;
    return check_one(w, "cmp x3, #10");
}

/* CSEL family ---------------------------------------------------- */
static int
test_aarch64_dis_csel(void)
{
    /* csel x0, x1, x2, ne
     * Encoding: sf=1 op=0 S=0 11010100 Rm=2 cond=NE op2=00 Rn=1 Rd=0 */
    uint32_t w = 0x9A800000u | (2u << 16) | (A_NE << 12) | (1u << 5) | 0u;
    return check_one(w, "csel x0, x1, x2, ne");
}
static int
test_aarch64_dis_csinv(void)
{
    /* csinv x0, x1, x2, lt: op=1 op2=00, base 0xDA800000 */
    uint32_t w = 0xDA800000u | (2u << 16) | (A_LT << 12) | (1u << 5) | 0u;
    return check_one(w, "csinv x0, x1, x2, lt");
}
static int
test_aarch64_dis_csneg(void)
{
    /* csneg x0, x1, x2, gt: op=1 op2=01, base 0xDA800400 */
    uint32_t w = 0xDA800400u | (2u << 16) | (A_GT << 12) | (1u << 5) | 0u;
    return check_one(w, "csneg x0, x1, x2, gt");
}
static int
test_aarch64_dis_csetm(void)
{
    /* csetm x9, lt = csinv x9, xzr, xzr, ge (the inverse condition).
     * Build CSINV with Rm=Rn=XZR, cond = GE. */
    uint32_t w = 0xDA800000u | (31u << 16) | (A_GE << 12) | (31u << 5) | 9u;
    return check_one(w, "csetm x9, lt");
}

/* CCMP / CCMN ---------------------------------------------------- */
static int
test_aarch64_dis_ccmp_reg(void)
{
    /* ccmp x1, x2, #4, ne
     * sf=1 op=1 S=1 11010010 Rm=2 cond=NE 0 mode=0 Rn=1 0 nzcv=4 */
    uint32_t w = 0xFA400000u | (2u << 16) | (A_NE << 12) | (1u << 5) | 4u;
    return check_one(w, "ccmp x1, x2, #4, ne");
}
static int
test_aarch64_dis_ccmp_imm(void)
{
    /* ccmp x1, #7, #4, eq: mode bit at 11 = 1 */
    uint32_t w = 0xFA400800u | (7u << 16) | (A_EQ << 12) | (1u << 5) | 4u;
    return check_one(w, "ccmp x1, #7, #4, eq");
}
static int
test_aarch64_dis_ccmn_reg(void)
{
    /* ccmn x1, x2, #0, mi: op=0 -> base 0xBA400000 */
    uint32_t w = 0xBA400000u | (2u << 16) | (A_MI << 12) | (1u << 5) | 0u;
    return check_one(w, "ccmn x1, x2, #0, mi");
}

/* MADD / MSUB / MNEG --------------------------------------------- */
static int
test_aarch64_dis_madd(void)
{
    /* madd x0, x1, x2, x3: sf=1, o0=0, Rm=2, Ra=3, Rn=1, Rd=0
     * base 0x9B000000 */
    uint32_t w = 0x9B000000u | (2u << 16) | (3u << 10) | (1u << 5) | 0u;
    return check_one(w, "madd x0, x1, x2, x3");
}
static int
test_aarch64_dis_msub(void)
{
    /* msub x0, x1, x2, x3: o0=1, base 0x9B008000 */
    uint32_t w = 0x9B008000u | (2u << 16) | (3u << 10) | (1u << 5) | 0u;
    return check_one(w, "msub x0, x1, x2, x3");
}
static int
test_aarch64_dis_mneg(void)
{
    /* mneg x0, x1, x2 = msub x0, x1, x2, xzr (Ra=31) */
    uint32_t w = 0x9B008000u | (2u << 16) | (31u << 10) | (1u << 5) | 0u;
    return check_one(w, "mneg x0, x1, x2");
}

/* SDIV / UDIV ---------------------------------------------------- */
static int
test_aarch64_dis_udiv(void)
{
    /* udiv x0, x1, x2: sf=1, op2=000010, base 0x9AC00800 */
    uint32_t w = 0x9AC00800u | (2u << 16) | (1u << 5) | 0u;
    return check_one(w, "udiv x0, x1, x2");
}
static int
test_aarch64_dis_sdiv(void)
{
    /* sdiv x0, x1, x2: op2=000011, base 0x9AC00C00 */
    uint32_t w = 0x9AC00C00u | (2u << 16) | (1u << 5) | 0u;
    return check_one(w, "sdiv x0, x1, x2");
}

/* Data-processing 1-source --------------------------------------- */
static int
test_aarch64_dis_clz(void)
{
    /* clz x0, x1: sf=1, op2=000100, base 0xDAC01000 */
    uint32_t w = 0xDAC01000u | (1u << 5) | 0u;
    return check_one(w, "clz x0, x1");
}
static int
test_aarch64_dis_cls(void)
{
    /* cls x0, x1: op2=000101 */
    uint32_t w = 0xDAC01400u | (1u << 5) | 0u;
    return check_one(w, "cls x0, x1");
}
static int
test_aarch64_dis_rbit(void)
{
    /* rbit x0, x1: op2=000000 */
    uint32_t w = 0xDAC00000u | (1u << 5) | 0u;
    return check_one(w, "rbit x0, x1");
}
static int
test_aarch64_dis_rev16(void)
{
    /* rev16 w0, w1: sf=0, op2=000001 */
    uint32_t w = 0x5AC00400u | (1u << 5) | 0u;
    return check_one(w, "rev16 w0, w1");
}
static int
test_aarch64_dis_rev_x(void)
{
    /* rev x0, x1: sf=1, op2=000011 */
    uint32_t w = 0xDAC00C00u | (1u << 5) | 0u;
    return check_one(w, "rev x0, x1");
}

/* EXTR / ROR-imm ------------------------------------------------- */
static int
test_aarch64_dis_extr(void)
{
    /* extr x0, x1, x2, #16: sf=1 N=1, base 0x93C00000.
     * imms field at 15:10. */
    uint32_t w = 0x93C00000u | (2u << 16) | (16u << 10) | (1u << 5) | 0u;
    return check_one(w, "extr x0, x1, x2, #16");
}
static int
test_aarch64_dis_ror_imm(void)
{
    /* ror x0, x1, #16 == extr x0, x1, x1, #16 */
    uint32_t w = 0x93C00000u | (1u << 16) | (16u << 10) | (1u << 5) | 0u;
    return check_one(w, "ror x0, x1, #16");
}

/* LDR literal ---------------------------------------------------- */
static int
test_aarch64_dis_ldr_literal(void)
{
    /* ldr x0, #+24:  imm19 = 24/4 = 6.  Base 0x58000000. */
    uint32_t imm19 = 6u;
    uint32_t w = 0x58000000u | (imm19 << 5) | 0u;
    return check_one(w, "ldr x0, #+24");
}
static int
test_aarch64_dis_ldrsw_literal(void)
{
    /* ldrsw x0, #-8: imm19 = -2. Base 0x98000000. */
    uint32_t imm19 = (uint32_t)(-2) & 0x7FFFF;
    uint32_t w = 0x98000000u | (imm19 << 5) | 0u;
    return check_one(w, "ldrsw x0, #-8");
}

/* LDR/STR unscaled + pre/post-indexed --------------------------- */
static int
test_aarch64_dis_ldur(void)
{
    /* ldur x0, [x1, #-4]: size=11, V=0, opc=01, idx=00 (unscaled).
     * Base 0xF8400000.  simm9 at bits[20:12]. */
    uint32_t simm9 = (uint32_t)(-4) & 0x1FF;
    uint32_t w = 0xF8400000u | (simm9 << 12) | (1u << 5) | 0u;
    return check_one(w, "ldur x0, [x1, #-4]");
}
static int
test_aarch64_dis_stur_w(void)
{
    /* stur w0, [x1, #12]: size=10, V=0, opc=00, idx=00. Base 0xB8000000. */
    uint32_t w = 0xB8000000u | (12u << 12) | (1u << 5) | 0u;
    return check_one(w, "stur w0, [x1, #12]");
}
static int
test_aarch64_dis_ldr_post(void)
{
    /* ldr x0, [x1], #8: idx=01 (post-indexed). Base 0xF8400400. */
    uint32_t w = 0xF8400400u | (8u << 12) | (1u << 5) | 0u;
    return check_one(w, "ldr x0, [x1], #8");
}
static int
test_aarch64_dis_str_pre(void)
{
    /* str x0, [x1, #-16]!: idx=11 (pre-indexed). Base 0xF8000C00. */
    uint32_t simm9 = (uint32_t)(-16) & 0x1FF;
    uint32_t w = 0xF8000C00u | (simm9 << 12) | (1u << 5) | 0u;
    return check_one(w, "str x0, [x1, #-16]!");
}

/* Exception generating ------------------------------------------ */
static int
test_aarch64_dis_brk(void)
{
    /* brk #1: 0xD4200000 | (imm16 << 5) | 0 */
    uint32_t w = 0xD4200020u; /* imm16=1 at bits 20:5 */
    return check_one(w, "brk #1");
}
static int
test_aarch64_dis_svc(void)
{
    /* svc #0x80: opc=000 ll=01, base 0xD4000001. imm16 at bits 20:5. */
    uint32_t w = 0xD4000001u | (0x80u << 5);
    return check_one(w, "svc #128");
}

/* W-form variants of common ops --------------------------------- */
static void
e_mov_imm_w(AsmEnc *e)
{
    aarch64_enc_movn(e, A_X0, 0, 0, 0);
}
static int
test_aarch64_dis_mov_imm_w(void)
{
    return check_one(encode(e_mov_imm_w), "movn w0, #0");
}

static void
e_cmp_w(AsmEnc *e)
{
    aarch64_enc_cmp_reg(e, 0, A_X1, A_X2);
}
static int
test_aarch64_dis_cmp_w(void)
{
    return check_one(encode(e_cmp_w), "cmp w1, w2");
}

static void
e_cset_w(AsmEnc *e)
{
    aarch64_enc_cset(e, A_X0, A_NE);
}
static int
test_aarch64_dis_cset_w(void)
{
    return check_one(encode(e_cset_w), "cset x0, ne");
}

/* Logical immediate (bitmask) -------------------------------------- */
static int
test_aarch64_dis_and_imm(void)
{
    /* and x0, x1, #0xFF (single 64-bit pattern, not replicated):
     * needs N=1, immr=0, imms=7. opc=00. Base 0x92000000. */
    uint32_t w = 0x92000000u | (1u << 22) | (0u << 16) | (7u << 10) |
            (1u << 5) | 0u;
    return check_one(w, "and x0, x1, #0xff");
}
static int
test_aarch64_dis_and_imm_replicated(void)
{
    /* and w0, w1, #0xF0F0F0F0: 8-bit-repeating pattern.
     * len=3 -> esize=8.  Encoded form has imms[5:3]=110, imms[2:0]=S=3,
     * so imms = 0b110011 = 51. immr=4 (rotate right by 4 within the
     * 8-bit element to move the ones up to bits 7:4). */
    uint32_t w = 0x12000000u | (0u << 22) | (4u << 16) | (51u << 10) |
            (1u << 5) | 0u;
    return check_one(w, "and w0, w1, #0xf0f0f0f0");
}
static int
test_aarch64_dis_orr_imm(void)
{
    /* orr x0, x1, #0x1: opc=01, N=1, immr=0, imms=0. Base 0xB2400000. */
    uint32_t w = 0xB2400000u | (1u << 5) | 0u;
    return check_one(w, "orr x0, x1, #0x1");
}
static int
test_aarch64_dis_mov_bitmask(void)
{
    /* mov x0, #0xFFFF_FFFF_FFFF_FFFE is impossible (all-ones reserved).
     * Try mov x0, #0xFFFE (low 16 bits set, top zero) - too complex for
     * bitmask. Pick a clean one: mov x0, #0xFFFFFFFFFFFFFFF0 doesn't fit.
     * Easier: mov x0, #0xFFFFFFFFFFFFFFFE - all ones except bit 0.
     *   N=1, immr=63, imms=62: pattern 64-bit, levels=63, S=62 ones starting
     * bit 0, then rotated right by 63 -> 0xFFFFFFFFFFFFFFFE. */
    uint32_t w = 0xB2000000u | (1u << 22) | (63u << 16) | (62u << 10) |
            (31u << 5) | 0u;
    return check_one(w, "mov x0, #0xfffffffffffffffe");
}
static int
test_aarch64_dis_tst_imm(void)
{
    /* tst x1, #0xF: ands xzr, x1, #0xF (64-bit, single pattern).
     * opc=11, N=1, immr=0, imms=3. Base 0xF2400000. */
    uint32_t w = 0xF2400000u | (0u << 16) | (3u << 10) | (1u << 5) | 31u;
    return check_one(w, "tst x1, #0xf");
}

/* TBZ / TBNZ ------------------------------------------------------- */
static int
test_aarch64_dis_tbz(void)
{
    /* tbz w0, #5, #+8: b5=0, op=0, b40=5, imm14=2 (8/4=2), Rt=0
     * Base 0x36000000. */
    uint32_t w = 0x36000000u | (5u << 19) | (2u << 5) | 0u;
    return check_one(w, "tbz w0, #5, #+8");
}
static int
test_aarch64_dis_tbnz_x(void)
{
    /* tbnz x9, #34 ; #+16: b5=1, op=1, b40=2 (34&31). */
    uint32_t w = 0xB7000000u | (2u << 19) | (4u << 5) | 9u;
    return check_one(w, "tbnz x9, #34, #+16");
}

/* HINT family ------------------------------------------------------ */
static int
test_aarch64_dis_yield(void)
{
    return check_one(0xD503203Fu, "yield");
}
static int
test_aarch64_dis_wfe(void)
{
    return check_one(0xD503205Fu, "wfe");
}
static int
test_aarch64_dis_wfi(void)
{
    return check_one(0xD503207Fu, "wfi");
}

/* LDR/STR register offset ------------------------------------------ */
static int
test_aarch64_dis_ldr_reg(void)
{
    /* ldr x0, [x1, x2]: size=11, V=0, opc=01, Rm=2, option=011 (lsl),
     * S=0. Base 0xF8600800 | option<<13 | Rm<<16 | Rn<<5 | Rt. */
    uint32_t w = 0xF8600800u | (2u << 16) | (3u << 13) | (1u << 5) | 0u;
    return check_one(w, "ldr x0, [x1, x2]");
}
static int
test_aarch64_dis_ldr_reg_sxtw(void)
{
    /* ldr x0, [x1, w2, sxtw #3]: option=110 (sxtw), S=1.
     * (Rm here is W since option bit 0 = 0.)  */
    uint32_t w = 0xF8600800u | (2u << 16) | (6u << 13) | (1u << 12) |
            (1u << 5) | 0u;
    return check_one(w, "ldr x0, [x1, w2, sxtw #3]");
}
static int
test_aarch64_dis_strb_reg(void)
{
    /* strb w0, [x1, x2]: size=00, opc=00, option=011, S=0. Base 0x38200800. */
    uint32_t w = 0x38200800u | (2u << 16) | (3u << 13) | (1u << 5) | 0u;
    return check_one(w, "strb w0, [x1, x2]");
}

/* ADD/SUB extended register ---------------------------------------- */
static int
test_aarch64_dis_add_ext(void)
{
    /* add x0, sp, x1, uxtb #0: sf=1, op=0, S=0, opc=01011, bit21=1
     * option=000 (uxtb), imm3=0, Rn=31(SP), Rd=0.  Base 0x8B200000. */
    uint32_t w = 0x8B200000u | (1u << 16) | (0u << 13) | (0u << 10) |
            (31u << 5) | 0u;
    return check_one(w, "add x0, sp, w1, uxtb");
}
static int
test_aarch64_dis_sub_ext_shift(void)
{
    /* sub sp, sp, x9, uxtx #2: op=1, option=011 (uxtx), imm3=2.
     * Base 0xCB200000. */
    uint32_t w = 0xCB200000u | (9u << 16) | (3u << 13) | (2u << 10) |
            (31u << 5) | 31u;
    return check_one(w, "sub sp, sp, x9, uxtx #2");
}

/* Widening multiplies ---------------------------------------------- */
static int
test_aarch64_dis_smull(void)
{
    /* smull x0, w1, w2: sf=1, op31=001, o0=0, Ra=31, Rm=2, Rn=1, Rd=0.
     * Base 0x9B200000. */
    uint32_t w = 0x9B200000u | (2u << 16) | (31u << 10) | (1u << 5) | 0u;
    return check_one(w, "smull x0, w1, w2");
}
static int
test_aarch64_dis_umull(void)
{
    /* umull x0, w1, w2: op31=101. Base 0x9BA00000. */
    uint32_t w = 0x9BA00000u | (2u << 16) | (31u << 10) | (1u << 5) | 0u;
    return check_one(w, "umull x0, w1, w2");
}
static int
test_aarch64_dis_smulh(void)
{
    /* smulh x0, x1, x2: op31=010, no Ra. Base 0x9B407C00 (Ra field reads as
     * ignored). */
    uint32_t w = 0x9B400000u | (2u << 16) | (31u << 10) | (1u << 5) | 0u;
    return check_one(w, "smulh x0, x1, x2");
}
static int
test_aarch64_dis_umulh(void)
{
    /* umulh x0, x1, x2: op31=110. Base 0x9BC00000. */
    uint32_t w = 0x9BC00000u | (2u << 16) | (31u << 10) | (1u << 5) | 0u;
    return check_one(w, "umulh x0, x1, x2");
}
static int
test_aarch64_dis_smaddl(void)
{
    /* smaddl x0, w1, w2, x3: op31=001, o0=0, Ra=3. Base 0x9B200000. */
    uint32_t w = 0x9B200000u | (2u << 16) | (3u << 10) | (1u << 5) | 0u;
    return check_one(w, "smaddl x0, w1, w2, x3");
}

/* CINC / CINV / CNEG conditional aliases --------------------------- */
static int
test_aarch64_dis_cinc(void)
{
    /* cinc x0, x1, eq = csinc x0, x1, x1, ne */
    uint32_t w = 0x9A800400u | (1u << 16) | (A_NE << 12) | (1u << 5) | 0u;
    return check_one(w, "cinc x0, x1, eq");
}
static int
test_aarch64_dis_cinv(void)
{
    /* cinv x0, x1, lt = csinv x0, x1, x1, ge */
    uint32_t w = 0xDA800000u | (1u << 16) | (A_GE << 12) | (1u << 5) | 0u;
    return check_one(w, "cinv x0, x1, lt");
}
static int
test_aarch64_dis_cneg(void)
{
    /* cneg x0, x1, ne = csneg x0, x1, x1, eq */
    uint32_t w = 0xDA800400u | (1u << 16) | (A_EQ << 12) | (1u << 5) | 0u;
    return check_one(w, "cneg x0, x1, ne");
}

/* BFI / BFXIL ---------------------------------------------------- */
static int
test_aarch64_dis_bfi(void)
{
    /* bfi x0, x1, #4, #8: BFM with immr = (64-4)%64 = 60, imms = 8-1 = 7.
     * opc=01, N=1. Base 0xB3400000. */
    uint32_t w = 0xB3400000u | (60u << 16) | (7u << 10) | (1u << 5) | 0u;
    return check_one(w, "bfi x0, x1, #4, #8");
}
static int
test_aarch64_dis_bfxil(void)
{
    /* bfxil x0, x1, #4, #8: BFM with immr=4, imms = 4+8-1 = 11. */
    uint32_t w = 0xB3400000u | (4u << 16) | (11u << 10) | (1u << 5) | 0u;
    return check_one(w, "bfxil x0, x1, #4, #8");
}

/* Barriers + system regs --------------------------------------- */
static int
test_aarch64_dis_dmb_ish(void)
{
    return check_one(0xD5033BBFu, "dmb ish");
}
static int
test_aarch64_dis_dsb_sy(void)
{
    return check_one(0xD5033F9Fu, "dsb sy");
}
static int
test_aarch64_dis_isb(void)
{
    return check_one(0xD5033FDFu, "isb");
}

static int
test_aarch64_dis_mrs_nzcv(void)
{
    /* mrs x0, nzcv: L=1, op0=3 (o0=1), op1=3, CRn=4, CRm=2, op2=0, Rt=0
     *   = 0xD5300000 | (1<<19) | (3<<16) | (4<<12) | (2<<8) | (0<<5) | 0 */
    uint32_t w = 0xD5300000u | (1u << 19) | (3u << 16) | (4u << 12) |
            (2u << 8) | (0u << 5) | 0u;
    return check_one(w, "mrs x0, nzcv");
}
static int
test_aarch64_dis_msr_tpidr(void)
{
    /* msr tpidr_el0, x1: L=0, op0=3, op1=3, CRn=13, CRm=0, op2=2, Rt=1 */
    uint32_t w = 0xD5100000u | (1u << 19) | (3u << 16) | (13u << 12) |
            (0u << 8) | (2u << 5) | 1u;
    return check_one(w, "msr tpidr_el0, x1");
}

/* Acquire/release + exclusive ---------------------------------- */
static int
test_aarch64_dis_ldar(void)
{
    /* ldar x0, [x1]: size=11, L=1. Base 0xC8DFFC00 | (Rn<<5) | Rt */
    uint32_t w = 0xC8DFFC00u | (1u << 5) | 0u;
    return check_one(w, "ldar x0, [x1]");
}
static int
test_aarch64_dis_stlr_w(void)
{
    /* stlr w0, [x1]: size=10, L=0. Base 0x889FFC00. */
    uint32_t w = 0x889FFC00u | (1u << 5) | 0u;
    return check_one(w, "stlr w0, [x1]");
}
static int
test_aarch64_dis_ldarb(void)
{
    /* ldarb w0, [x1]: size=00. Base 0x08DFFC00. */
    uint32_t w = 0x08DFFC00u | (1u << 5) | 0u;
    return check_one(w, "ldarb w0, [x1]");
}
static int
test_aarch64_dis_ldxr(void)
{
    /* ldxr x0, [x1]: size=11, L=1, bit23=0, Rs=11111. Base 0xC85F7C00. */
    uint32_t w = 0xC85F7C00u | (1u << 5) | 0u;
    return check_one(w, "ldxr x0, [x1]");
}
static int
test_aarch64_dis_stxr(void)
{
    /* stxr w9, x0, [x1]: size=11, L=0, Rs=9 (success/fail dst).
     * Base 0xC8007C00 | (Rs<<16) | (Rn<<5) | Rt. */
    uint32_t w = 0xC8007C00u | (9u << 16) | (1u << 5) | 0u;
    return check_one(w, "stxr w9, x0, [x1]");
}

/* PRFM ---------------------------------------------------------- */
static int
test_aarch64_dis_prfm(void)
{
    /* prfm pldl1keep, [x1, #16]: size=11, opc=10, imm12=2 (=16/8), Rt=0
     * Rt op: type=00(pld), target=00(l1), strm=0 -> pldl1keep. */
    uint32_t w = 0xF9800000u | (2u << 10) | (1u << 5) | 0u;
    return check_one(w, "prfm pldl1keep, [x1, #16]");
}
static int
test_aarch64_dis_prfm_strm(void)
{
    /* prfm plil2strm, [x0]: type=01 (pli), target=01 (l2), strm=1
     *   Rt = (1<<3) | (1<<1) | 1 = 0xB. */
    uint32_t w = 0xF9800000u | (0u << 10) | (0u << 5) | 0xBu;
    return check_one(w, "prfm plil2strm, [x0]");
}

/* BLR ----------------------------------------------------------- */
static int
test_aarch64_dis_blr(void)
{
    uint32_t w = 0xD63F0000u | (16u << 5);
    return check_one(w, "blr x16");
}

/* More HINTs ---------------------------------------------------- */
static int
test_aarch64_dis_sev(void)
{
    return check_one(0xD503209Fu, "sev");
}
static int
test_aarch64_dis_sevl(void)
{
    return check_one(0xD50320BFu, "sevl");
}

/* FP 2-source --------------------------------------------------- */
static void
e_fadd_d(AsmEnc *e)
{
    aarch64_enc_fp_2src_scalar(e, /*is_double=*/1, /*FADD*/ 2, A_X0, A_X1,
                               A_X2);
}
static int
test_aarch64_dis_fadd_d(void)
{
    return check_one(encode(e_fadd_d), "fadd d0, d1, d2");
}
static void
e_fmul_s(AsmEnc *e)
{
    aarch64_enc_fp_2src_scalar(e, 0, /*FMUL*/ 0, A_X3, A_X4, A_X5);
}
static int
test_aarch64_dis_fmul_s(void)
{
    return check_one(encode(e_fmul_s), "fmul s3, s4, s5");
}
static void
e_fdiv_d(AsmEnc *e)
{
    aarch64_enc_fp_2src_scalar(e, 1, /*FDIV*/ 1, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_fdiv_d(void)
{
    return check_one(encode(e_fdiv_d), "fdiv d0, d1, d2");
}
static void
e_fsub_s(AsmEnc *e)
{
    aarch64_enc_fp_2src_scalar(e, 0, /*FSUB*/ 3, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_fsub_s(void)
{
    return check_one(encode(e_fsub_s), "fsub s0, s1, s2");
}

/* FNMUL (opc4=8): hand-encode since the encoder helper only takes 4-bit
 * values we use (0..3). */
static int
test_aarch64_dis_fnmul(void)
{
    /* fnmul d0, d1, d2: ftype=01, opc4=1000. Base 0x1E608000. */
    uint32_t w = 0x1E600000u | (1u << 21) | (2u << 16) | (8u << 12) |
            (2u << 10) | (1u << 5) | 0u;
    return check_one(w, "fnmul d0, d1, d2");
}

/* FP 1-source --------------------------------------------------- */
static void
e_fmov_d(AsmEnc *e)
{
    aarch64_enc_fmov_reg(e, 1, A_X0, A_X1);
}
static int
test_aarch64_dis_fmov_d(void)
{
    return check_one(encode(e_fmov_d), "fmov d0, d1");
}
static void
e_fmov_s(AsmEnc *e)
{
    aarch64_enc_fmov_reg(e, 0, A_X3, A_X4);
}
static int
test_aarch64_dis_fmov_s(void)
{
    return check_one(encode(e_fmov_s), "fmov s3, s4");
}
static void
e_fmov_h(AsmEnc *e)
{
    aarch64_enc_fmov_reg_h(e, A_X0, A_X1);
}
static int
test_aarch64_dis_fmov_h(void)
{
    return check_one(encode(e_fmov_h), "fmov h0, h1");
}

/* FABS / FNEG / FSQRT: hand-encode (no helper). */
static int
test_aarch64_dis_fabs_d(void)
{
    /* fabs d0, d1: ftype=01, opcode6=000001. Base 0x1E60C000 | Rn<<5 | Rd. */
    uint32_t w = 0x1E60C000u | (1u << 5) | 0u;
    return check_one(w, "fabs d0, d1");
}
static int
test_aarch64_dis_fneg_s(void)
{
    /* fneg s0, s1: ftype=00, opcode6=000010. Base 0x1E214000. */
    uint32_t w = 0x1E214000u | (1u << 5) | 0u;
    return check_one(w, "fneg s0, s1");
}
static int
test_aarch64_dis_fsqrt_d(void)
{
    /* fsqrt d0, d1: ftype=01, opcode6=000011. Base 0x1E61C000. */
    uint32_t w = 0x1E61C000u | (1u << 5) | 0u;
    return check_one(w, "fsqrt d0, d1");
}

/* FCVT widening / narrowing (S<->D) */
static int
test_aarch64_dis_fcvt_d_to_s(void)
{
    /* fcvt s0, d1: src ftype=01 (D), dst opc6=0x04 (S).
     * Encoding: 0x1E000000 | (1<<22) | (1<<21) | (4<<15) | (16<<10) | Rn<<5 |
     * Rd = 0x1E624000 | (1<<5). */
    uint32_t w = 0x1E624000u | (1u << 5) | 0u;
    return check_one(w, "fcvt s0, d1");
}
static int
test_aarch64_dis_fcvt_s_to_d(void)
{
    /* fcvt d0, s1: src ftype=00 (S), dst opc6=0x05 (D). */
    uint32_t w = 0x1E22C000u | (1u << 5) | 0u;
    return check_one(w, "fcvt d0, s1");
}

/* Rounding */
static int
test_aarch64_dis_frintn(void)
{
    /* frintn d0, d1: opc6=001000. ftype=01. Base 0x1E644000. */
    uint32_t w = 0x1E644000u | (1u << 5) | 0u;
    return check_one(w, "frintn d0, d1");
}
static int
test_aarch64_dis_frintz(void)
{
    /* frintz s0, s1: opc6=001011. ftype=00. */
    uint32_t w = 0x1E20C000u | (0xBu << 15) | (1u << 5) | 0u;
    /* Actually base for ftype=00 is 0x1E204000 with opc6=0. Need opc6=0xB.
     * 0x1E204000 | (0xB << 15) = 0x1E204000 | 0x58000 = 0x1E25C000. */
    w = 0x1E204000u | (0xBu << 15) | (1u << 5) | 0u;
    return check_one(w, "frintz s0, s1");
}

/* FCMP --------------------------------------------------------- */
static int
test_aarch64_dis_fcmp_reg(void)
{
    /* fcmp d0, d1: ftype=01, Rm=1, opc2=00000, bits[15:12]=0010.
     * Base 0x1E602000 | (Rm<<16) | (Rn<<5) | opc2. */
    uint32_t w = 0x1E602000u | (1u << 16) | (0u << 5) | 0u;
    return check_one(w, "fcmp d0, d1");
}
static int
test_aarch64_dis_fcmpe_reg(void)
{
    /* fcmpe s0, s1: ftype=00, opc2=10000 (bit 4=1). */
    uint32_t w = 0x1E202000u | (1u << 16) | (0u << 5) | 0x10u;
    return check_one(w, "fcmpe s0, s1");
}
static int
test_aarch64_dis_fcmp_zero(void)
{
    /* fcmp d0, #0.0: opc2=01000 (compare_zero=1), Rm=00000. */
    uint32_t w = 0x1E602000u | (0u << 16) | (0u << 5) | 0x08u;
    return check_one(w, "fcmp d0, #0.0");
}

/* FMOV immediate ---------------------------------------------- */
static int
test_aarch64_dis_fmov_d_imm(void)
{
    /* fmov d0, #imm8 - pick imm8 = 0x70 (which encodes 1.0). */
    uint32_t w = 0x1E601000u | (0x70u << 13) | 0u;
    return check_one(w, "fmov d0, #1.0");
}

/* FMOV between FP and GPR ------------------------------------ */
static void
e_fmov_x_to_d(AsmEnc *e)
{
    aarch64_enc_fmov_gpr(e, /*is_double=*/1, /*fp_to_gpr=*/0, A_X0, A_X1);
}
static int
test_aarch64_dis_fmov_x_to_d(void)
{
    return check_one(encode(e_fmov_x_to_d), "fmov d0, x1");
}
static void
e_fmov_d_to_x(AsmEnc *e)
{
    aarch64_enc_fmov_gpr(e, 1, 1, A_X0, A_X1);
}
static int
test_aarch64_dis_fmov_d_to_x(void)
{
    return check_one(encode(e_fmov_d_to_x), "fmov x0, d1");
}
static void
e_fmov_w_to_s(AsmEnc *e)
{
    aarch64_enc_fmov_gpr(e, 0, 0, A_X0, A_X1);
}
static int
test_aarch64_dis_fmov_w_to_s(void)
{
    return check_one(encode(e_fmov_w_to_s), "fmov s0, w1");
}

/* FP <-> int conversions ------------------------------------- */
static int
test_aarch64_dis_scvtf_d_x(void)
{
    /* scvtf d0, x1: sf=1, ftype=01, rmode_opcode5=00010. Base 0x9E620000. */
    uint32_t w = 0x9E620000u | (1u << 5) | 0u;
    return check_one(w, "scvtf d0, x1");
}
static int
test_aarch64_dis_ucvtf_s_w(void)
{
    /* ucvtf s0, w1: sf=0, ftype=00, rmode_opcode5=00011. Base 0x1E230000. */
    uint32_t w = 0x1E230000u | (1u << 5) | 0u;
    return check_one(w, "ucvtf s0, w1");
}
static int
test_aarch64_dis_fcvtzs_x_d(void)
{
    /* fcvtzs x0, d1: sf=1, ftype=01, rmode_opcode5=11000. Base 0x9E780000. */
    uint32_t w = 0x9E780000u | (1u << 5) | 0u;
    return check_one(w, "fcvtzs x0, d1");
}
static int
test_aarch64_dis_fcvtzu_w_s(void)
{
    /* fcvtzu w0, s1: sf=0, ftype=00, rmode_opcode5=11001. */
    uint32_t w = 0x1E390000u | (1u << 5) | 0u;
    return check_one(w, "fcvtzu w0, s1");
}
static int
test_aarch64_dis_fcvtns_x_d(void)
{
    /* fcvtns x0, d1: rmode=00, opcode=000 (signed). */
    uint32_t w = 0x9E600000u | (1u << 5) | 0u;
    return check_one(w, "fcvtns x0, d1");
}

/* FP load/store immediate ------------------------------------ */
static void
e_str_d(AsmEnc *e)
{
    aarch64_enc_fp_ldst_imm(e, /*is_load=*/0, /*width=*/8, A_X0, A_SP, 16);
}
static int
test_aarch64_dis_str_d(void)
{
    return check_one(encode(e_str_d), "str d0, [sp, #16]");
}
static void
e_ldr_s(AsmEnc *e)
{
    aarch64_enc_fp_ldst_imm(e, 1, 4, A_X3, A_X4, 12);
}
static int
test_aarch64_dis_ldr_s(void)
{
    return check_one(encode(e_ldr_s), "ldr s3, [x4, #12]");
}
static void
e_ldr_b(AsmEnc *e)
{
    aarch64_enc_fp_ldst_imm(e, 1, 1, A_X0, A_X1, 5);
}
static int
test_aarch64_dis_ldr_b(void)
{
    return check_one(encode(e_ldr_b), "ldr b0, [x1, #5]");
}
static void
e_ldr_h(AsmEnc *e)
{
    aarch64_enc_fp_ldst_imm(e, 1, 2, A_X0, A_X1, 6);
}
static int
test_aarch64_dis_ldr_h(void)
{
    return check_one(encode(e_ldr_h), "ldr h0, [x1, #6]");
}

/* Vector FP 3-same --------------------------------------------- */
static void
e_fadd_2s(AsmEnc *e)
{
    aarch64_enc_fp_3same_vec(e, /*Q=*/0, /*U=*/0, /*bit23=*/0, /*sz=*/0,
                             /*opc5=*/0x1A, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_fadd_2s(void)
{
    return check_one(encode(e_fadd_2s), "fadd v0.2s, v1.2s, v2.2s");
}
static void
e_fadd_4s(AsmEnc *e)
{
    aarch64_enc_fp_3same_vec(e, 1, 0, 0, 0, 0x1A, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_fadd_4s(void)
{
    return check_one(encode(e_fadd_4s), "fadd v0.4s, v1.4s, v2.4s");
}
static void
e_fadd_2d(AsmEnc *e)
{
    aarch64_enc_fp_3same_vec(e, 1, 0, 0, 1, 0x1A, A_X3, A_X4, A_X5);
}
static int
test_aarch64_dis_fadd_2d(void)
{
    return check_one(encode(e_fadd_2d), "fadd v3.2d, v4.2d, v5.2d");
}
static void
e_fsub_4s(AsmEnc *e)
{
    aarch64_enc_fp_3same_vec(e, 1, 0, 1, 0, 0x1A, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_fsub_4s(void)
{
    return check_one(encode(e_fsub_4s), "fsub v0.4s, v1.4s, v2.4s");
}
static void
e_fmul_2d(AsmEnc *e)
{
    aarch64_enc_fp_3same_vec(e, 1, 1, 0, 1, 0x1B, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_fmul_2d(void)
{
    return check_one(encode(e_fmul_2d), "fmul v0.2d, v1.2d, v2.2d");
}
static void
e_fdiv_4s(AsmEnc *e)
{
    aarch64_enc_fp_3same_vec(e, 1, 1, 0, 0, 0x1F, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_fdiv_4s(void)
{
    return check_one(encode(e_fdiv_4s), "fdiv v0.4s, v1.4s, v2.4s");
}

/* Hand-encoded extras: FMAX/FMIN/FMLA/FCMEQ */
static int
test_aarch64_dis_fmax_4s(void)
{
    /* fmax v0.4s, v1.4s, v2.4s: U=0, bit23=0, opc5=11110(0x1E). Q=1, sz=0. */
    uint32_t w = 0x0E200400u | (1u << 30) | (0u << 29) | (0u << 23) |
            (0u << 22) | (2u << 16) | (0x1Eu << 11) | (1u << 5) | 0u;
    return check_one(w, "fmax v0.4s, v1.4s, v2.4s");
}
static int
test_aarch64_dis_fmla_2s(void)
{
    /* fmla v0.2s, v1.2s, v2.2s: U=0, bit23=0, opc5=11001(0x19). */
    uint32_t w = 0x0E200400u | (0u << 30) | (0u << 29) | (0u << 23) |
            (0u << 22) | (2u << 16) | (0x19u << 11) | (1u << 5) | 0u;
    return check_one(w, "fmla v0.2s, v1.2s, v2.2s");
}

/* BF16 --------------------------------------------------------- */
static void
e_bfdot_2s(AsmEnc *e)
{
    aarch64_enc_bfdot_vec(e, /*Q=*/0, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_bfdot_2s(void)
{
    return check_one(encode(e_bfdot_2s), "bfdot v0.2s, v1.4h, v2.4h");
}
static void
e_bfdot_4s(AsmEnc *e)
{
    aarch64_enc_bfdot_vec(e, 1, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_bfdot_4s(void)
{
    return check_one(encode(e_bfdot_4s), "bfdot v0.4s, v1.8h, v2.8h");
}
static void
e_bfdot_idx_2s(AsmEnc *e)
{
    aarch64_enc_bfdot_idx(e, /*Q=*/0, A_X0, A_X1, A_X2, /*idx=*/0);
}
static int
test_aarch64_dis_bfdot_idx_2s(void)
{
    return check_one(encode(e_bfdot_idx_2s), "bfdot v0.2s, v1.4h, v2.2h[0]");
}
static void
e_bfdot_idx_4s(AsmEnc *e)
{
    aarch64_enc_bfdot_idx(e, 1, A_X3, A_X4, A_X5, /*idx=*/3);
}
static int
test_aarch64_dis_bfdot_idx_4s(void)
{
    return check_one(encode(e_bfdot_idx_4s), "bfdot v3.4s, v4.8h, v5.2h[3]");
}
/* Indexed form supports Rm in the 0..31 range (M bit at 20). */
static void
e_bfdot_idx_hi(AsmEnc *e)
{
    aarch64_enc_bfdot_idx(e, 1, A_X0, A_X1, (A64Reg)17, /*idx=*/2);
}
static int
test_aarch64_dis_bfdot_idx_hi(void)
{
    return check_one(encode(e_bfdot_idx_hi), "bfdot v0.4s, v1.8h, v17.2h[2]");
}
static void
e_bfmlalb(AsmEnc *e)
{
    aarch64_enc_bfmlalb(e, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_bfmlalb(void)
{
    return check_one(encode(e_bfmlalb), "bfmlalb v0.4s, v1.8h, v2.8h");
}
static void
e_bfmlalt(AsmEnc *e)
{
    aarch64_enc_bfmlalt(e, A_X3, A_X4, A_X5);
}
static int
test_aarch64_dis_bfmlalt(void)
{
    return check_one(encode(e_bfmlalt), "bfmlalt v3.4s, v4.8h, v5.8h");
}

/* BFCVT scalar (FP 1-source opc6=6). */
static void
e_bfcvt(AsmEnc *e)
{
    aarch64_enc_bfcvt(e, A_X0, A_X1);
}
static int
test_aarch64_dis_bfcvt(void)
{
    return check_one(encode(e_bfcvt), "bfcvt h0, s1");
}

/* BFCVTN / BFCVTN2 (vector convert). */
static int
test_aarch64_dis_bfcvtn(void)
{
    uint32_t w = 0x0EA16800u | (1u << 5);
    return check_one(w, "bfcvtn v0.4h, v1.4s");
}
static int
test_aarch64_dis_bfcvtn2(void)
{
    uint32_t w = 0x4EA16800u | (1u << 5);
    return check_one(w, "bfcvtn2 v0.8h, v1.4s");
}

/* ================================================================
 * Direct-encoder roundtrips - exercise the public encoder API for
 * everything we previously hand-built or only covered transitively.
 * ============================================================== */

/* ADRP - encode a +1-page displacement. */
static void
e_adrp(AsmEnc *e)
{
    aarch64_enc_adrp(e, A_X0, /*pages=*/1);
}
static int
test_aarch64_dis_adrp(void)
{
    return check_one(encode(e_adrp), "adrp x0, #+4096");
}

/* BLR via the encoder. */
static void
e_blr(AsmEnc *e)
{
    aarch64_enc_blr(e, A_X16);
}
static int
test_aarch64_dis_blr_enc(void)
{
    return check_one(encode(e_blr), "blr x16");
}

/* RET via the explicit-Rn form. */
static void
e_ret_x9(AsmEnc *e)
{
    aarch64_enc_ret_reg(e, A_X9);
}
static int
test_aarch64_dis_ret_x9(void)
{
    return check_one(encode(e_ret_x9), "ret x9");
}

/* MSUB / SDIV / ASRV / LSL_imm / NEG. */
static void
e_msub(AsmEnc *e)
{
    aarch64_enc_msub(e, 1, A_X0, A_X1, A_X2, A_X3);
}
static int
test_aarch64_dis_msub_enc(void)
{
    return check_one(encode(e_msub), "msub x0, x1, x2, x3");
}
static void
e_sdiv(AsmEnc *e)
{
    aarch64_enc_sdiv(e, 1, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_sdiv_enc(void)
{
    return check_one(encode(e_sdiv), "sdiv x0, x1, x2");
}
static void
e_asrv(AsmEnc *e)
{
    aarch64_enc_asrv(e, 1, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_asrv_enc(void)
{
    return check_one(encode(e_asrv), "asr x0, x1, x2");
}
static void
e_lsl_imm(AsmEnc *e)
{
    aarch64_enc_lsl_imm(e, 1, A_X3, A_X4, 5);
}
static int
test_aarch64_dis_lsl_imm_enc(void)
{
    return check_one(encode(e_lsl_imm), "lsl x3, x4, #5");
}
static void
e_neg_reg(AsmEnc *e)
{
    aarch64_enc_neg_reg(e, 1, A_X9, A_X9);
}
static int
test_aarch64_dis_neg_reg_enc(void)
{
    return check_one(encode(e_neg_reg), "neg x9, x9");
}

/* SUBS imm / reg + ADD/SUB shifted register. */
static void
e_subs_imm(AsmEnc *e)
{
    aarch64_enc_subs_imm(e, 1, A_X0, A_X1, 5);
}
static int
test_aarch64_dis_subs_imm_enc(void)
{
    return check_one(encode(e_subs_imm), "subs x0, x1, #5");
}
static void
e_subs_reg(AsmEnc *e)
{
    aarch64_enc_subs_reg(e, 1, A_X0, A_X1, A_X2, /*shift_type=*/0,
                         /*shift_amount=*/0);
}
static int
test_aarch64_dis_subs_reg_enc(void)
{
    return check_one(encode(e_subs_reg), "subs x0, x1, x2");
}
static void
e_add_lsl(AsmEnc *e)
{
    aarch64_enc_add_reg_shifted(e, 1, A_X0, A_X1, A_X2, /*lsl=*/0, /*amt=*/3);
}
static int
test_aarch64_dis_add_lsl_enc(void)
{
    return check_one(encode(e_add_lsl), "add x0, x1, x2, lsl #3");
}
static void
e_sub_lsr(AsmEnc *e)
{
    aarch64_enc_sub_reg_shifted(e, 1, A_X0, A_X1, A_X2, /*lsr=*/1, /*amt=*/2);
}
static int
test_aarch64_dis_sub_lsr_enc(void)
{
    return check_one(encode(e_sub_lsr), "sub x0, x1, x2, lsr #2");
}

/* LDP/STP via the unified encoder. */
static void
e_stp_off(AsmEnc *e)
{
    aarch64_enc_ldst_pair(e, /*simd=*/0, /*load=*/0, /*width=*/8, /*mode=*/0,
                          A_X0, A_X1, A_SP, 16);
}
static int
test_aarch64_dis_stp_off(void)
{
    return check_one(encode(e_stp_off), "stp x0, x1, [sp, #16]");
}
static void
e_ldp_off_w(AsmEnc *e)
{
    aarch64_enc_ldst_pair(e, 0, 1, 4, 0, A_X0, A_X1, A_X2, 8);
}
static int
test_aarch64_dis_ldp_off_w(void)
{
    return check_one(encode(e_ldp_off_w), "ldp w0, w1, [x2, #8]");
}

/* SIMD&FP register pairs (V=1): opc selects S/D/Q. */
static void
e_stp_q_off(AsmEnc *e)
{
    aarch64_enc_ldst_pair(e, /*simd=*/1, /*load=*/0, /*width=*/16, /*mode=*/0,
                          0, 1, A_SP, 32);
}
static int
test_aarch64_dis_stp_q_off(void)
{
    return check_one(encode(e_stp_q_off), "stp q0, q1, [sp, #32]");
}
static void
e_ldp_q_pre(AsmEnc *e)
{
    aarch64_enc_ldst_pair(e, 1, 1, 16, /*mode=pre*/1, 0, 1, A_SP, -32);
}
static int
test_aarch64_dis_ldp_q_pre(void)
{
    return check_one(encode(e_ldp_q_pre), "ldp q0, q1, [sp, #-32]!");
}
static void
e_ldp_d_off(AsmEnc *e)
{
    aarch64_enc_ldst_pair(e, 1, 1, 8, 0, 0, 1, A_X2, 16);
}
static int
test_aarch64_dis_ldp_d_off(void)
{
    return check_one(encode(e_ldp_d_off), "ldp d0, d1, [x2, #16]");
}
static void
e_stp_s_pre(AsmEnc *e)
{
    aarch64_enc_ldst_pair(e, 1, 0, 4, 1, 0, 1, A_X2, -8);
}
static int
test_aarch64_dis_stp_s_pre(void)
{
    return check_one(encode(e_stp_s_pre), "stp s0, s1, [x2, #-8]!");
}

/* Non-temporal pairs (X=00). */
static void
e_ldnp_x(AsmEnc *e)
{
    aarch64_enc_ldst_pair(e, 0, 1, 8, /*mode=nontemporal*/3, A_X0, A_X1, A_X2, 16);
}
static int
test_aarch64_dis_ldnp(void)
{
    return check_one(encode(e_ldnp_x), "ldnp x0, x1, [x2, #16]");
}
static void
e_stnp_x(AsmEnc *e)
{
    aarch64_enc_ldst_pair(e, 0, 0, 8, 3, A_X0, A_X1, A_X2, 16);
}
static int
test_aarch64_dis_stnp(void)
{
    return check_one(encode(e_stnp_x), "stnp x0, x1, [x2, #16]");
}

/* FMOV imm8 expansion: negative, sub-one, and a full-mantissa value. */
static int
test_aarch64_dis_fmov_s_imm_neg(void)
{
    /* fmov s0, #-1.0: imm8 = 0xF0, S base 0x1E201000. */
    return check_one(0x1E201000u | (0xF0u << 13), "fmov s0, #-1.0");
}
static int
test_aarch64_dis_fmov_d_imm_half(void)
{
    /* fmov d0, #0.5: imm8 = 0x60. */
    return check_one(0x1E601000u | (0x60u << 13), "fmov d0, #0.5");
}
static int
test_aarch64_dis_fmov_d_imm_full_mant(void)
{
    /* imm8 = 0x4F: (16+15)/16 * 2^-3 = 0.2421875 - exercises the
     * longest decimal the scheme can produce. */
    return check_one(0x1E601000u | (0x4Fu << 13), "fmov d0, #0.2421875");
}

/* LDR/STR pre/post-indexed via encoder. */
static void
e_ldr_post(AsmEnc *e)
{
    aarch64_enc_ldst_post(e, /*simd=*/0, /*load=*/1, /*width=*/8, A_X0, A_X1,
                          8);
}
static int
test_aarch64_dis_ldr_post_enc(void)
{
    return check_one(encode(e_ldr_post), "ldr x0, [x1], #8");
}
static void
e_str_pre_enc(AsmEnc *e)
{
    aarch64_enc_ldst_pre(e, 0, 0, 8, A_X0, A_X1, -16);
}
static int
test_aarch64_dis_str_pre_enc(void)
{
    return check_one(encode(e_str_pre_enc), "str x0, [x1, #-16]!");
}

/* LDR (register offset) via encoder. */
static void
e_ldr_regoff(AsmEnc *e)
{
    aarch64_enc_ldst_regoff(e, /*simd=*/0, /*load=*/1, /*width=*/8,
                            /*scaled=*/0, A_X0, A_X1, A_X2);
}
static int
test_aarch64_dis_ldr_regoff_enc(void)
{
    return check_one(encode(e_ldr_regoff), "ldr x0, [x1, x2]");
}

/* LDUR / STUR via encoder. */
static void
e_ldur_enc(AsmEnc *e)
{
    aarch64_enc_ldur(e, /*width=*/8, A_X0, A_X1, -4);
}
static int
test_aarch64_dis_ldur_enc(void)
{
    return check_one(encode(e_ldur_enc), "ldur x0, [x1, #-4]");
}
static void
e_stur_enc(AsmEnc *e)
{
    aarch64_enc_stur(e, /*width=*/4, A_X0, A_X1, 12);
}
static int
test_aarch64_dis_stur_enc(void)
{
    return check_one(encode(e_stur_enc), "stur w0, [x1, #12]");
}

/* FMOV H<->GPR via encoder. */
static void
e_fmov_w_to_h(AsmEnc *e)
{
    aarch64_enc_fmov_h_gpr(e, /*fp_to_gpr=*/0, A_X0, A_X1);
}
static int
test_aarch64_dis_fmov_w_to_h(void)
{
    return check_one(encode(e_fmov_w_to_h), "fmov h0, w1");
}
static void
e_fmov_h_to_w(AsmEnc *e)
{
    aarch64_enc_fmov_h_gpr(e, 1, A_X0, A_X1);
}
static int
test_aarch64_dis_fmov_h_to_w(void)
{
    return check_one(encode(e_fmov_h_to_w), "fmov w0, h1");
}

/* MSR (immediate): set DAIF flags. */
static void
e_msr_daifset(AsmEnc *e)
{
    aarch64_enc_msr_imm(e, /*op1=*/3, /*op2=*/6, /*imm4=*/4);
}
static int
test_aarch64_dis_msr_daifset(void)
{
    return check_one(encode(e_msr_daifset), "msr daifset, #4");
}
static void
e_msr_daifclr(AsmEnc *e)
{
    aarch64_enc_msr_imm(e, 3, 7, 0xF);
}
static int
test_aarch64_dis_msr_daifclr(void)
{
    return check_one(encode(e_msr_daifclr), "msr daifclr, #15");
}

/* CMP imm / TST reg / MVN reg / MOV reg / FMOV imm - these alias other
 * encodings; roundtripping via the direct encoder API confirms the
 * encoder hits the bytes the disassembler turns back into the alias. */
static void
e_cmp_imm_enc(AsmEnc *e)
{
    aarch64_enc_cmp_imm(e, 1, A_X3, 10);
}
static int
test_aarch64_dis_cmp_imm_enc(void)
{
    return check_one(encode(e_cmp_imm_enc), "cmp x3, #10");
}
static void
e_tst_reg_enc(AsmEnc *e)
{
    aarch64_enc_tst_reg(e, 1, A_X1, A_X2);
}
static int
test_aarch64_dis_tst_reg_enc(void)
{
    return check_one(encode(e_tst_reg_enc), "tst x1, x2");
}
static void
e_mvn_reg_enc(AsmEnc *e)
{
    aarch64_enc_mvn_reg(e, 1, A_X0, A_X1);
}
static int
test_aarch64_dis_mvn_reg_enc(void)
{
    return check_one(encode(e_mvn_reg_enc), "mvn x0, x1");
}
static void
e_mov_reg_enc(AsmEnc *e)
{
    aarch64_enc_mov_reg(e, A_X10, A_X9);
}
static int
test_aarch64_dis_mov_reg_enc(void)
{
    return check_one(encode(e_mov_reg_enc), "mov x10, x9");
}
static void
e_mov_reg_w_enc(AsmEnc *e)
{
    aarch64_enc_mov_reg_w(e, A_X4, A_X5);
}
static int
test_aarch64_dis_mov_reg_w_enc(void)
{
    return check_one(encode(e_mov_reg_w_enc), "mov w4, w5");
}
static void
e_fmov_imm_enc(AsmEnc *e)
{
    aarch64_enc_fmov_imm(e, /*is_double=*/1, A_X0, 0x70);
}
static int
test_aarch64_dis_fmov_imm_enc(void)
{
    return check_one(encode(e_fmov_imm_enc), "fmov d0, #1.0");
}

/* BR via the direct encoder. */
static void
e_br_enc(AsmEnc *e)
{
    aarch64_enc_br(e, A_X16);
}
static int
test_aarch64_dis_br_enc(void)
{
    return check_one(encode(e_br_enc), "br x16");
}

/* MSR (register, system) via the direct encoder. sysreg16 = 0xDA10
 * encodes NZCV. */
static void
e_msr_reg_enc(AsmEnc *e)
{
    aarch64_enc_msr_reg(e, 0xDA10, A_X1);
}
static int
test_aarch64_dis_msr_reg_enc(void)
{
    return check_one(encode(e_msr_reg_enc), "msr nzcv, x1");
}

/* BFMMLA via the encoder (line-based, but we can hand-encode). */
static int
test_aarch64_dis_bfmmla(void)
{
    /* bfmmla v0.4s, v1.8h, v2.8h:
     *   base 0x6E40EC00 | (Rm<<16) | (Rn<<5) | Rd. */
    uint32_t w = 0x6E40EC00u | (2u << 16) | (1u << 5) | 0u;
    return check_one(w, "bfmmla v0.4s, v1.8h, v2.8h");
}

/* Verify the buffer disassembler walks a sequence: encode a tiny
 * prologue, capture into a memory file, compare line-by-line. */
static int
test_aarch64_dis_buffer_dump(void)
{
    AsmEnc enc;
    asm_enc_init(&enc);
    aarch64_enc_stp_pre(&enc, A_FP, A_LR, A_SP, -16);
    aarch64_enc_add_imm(&enc, 1, A_FP, A_SP, 0);
    aarch64_enc_mov_imm64(&enc, A_X0, 42);
    aarch64_enc_ret(&enc);

    char *p = NULL;
    size_t n = 0;
    FILE *f = open_memstream(&p, &n);
    aarch64_disasm_buf(enc.bytes, enc.len, f);
    fclose(f);
    asm_enc_free(&enc);

    const char *want = "   0: a9bf7bfd  stp x29, x30, [sp, #-16]!\n"
                       "   4: 910003fd  add x29, sp, #0\n"
                       "   8: d2800540  mov x0, #42\n"
                       "  12: d65f03c0  ret\n";
    int ok = (p && strcmp(p, want) == 0);
    if (!ok && p)
        fprintf(stderr, "buffer dump mismatch:\n%s---want:\n%s", p, want);
    free(p);
    return ok;
}

int
main(void)
{
    DisTest tests[] = {
        PrepTest(test_aarch64_dis_mov_imm_42),
        PrepTest(test_aarch64_dis_mov_imm_high),
        PrepTest(test_aarch64_dis_add_imm),
        PrepTest(test_aarch64_dis_sub_imm),
        PrepTest(test_aarch64_dis_add_reg),
        PrepTest(test_aarch64_dis_sub_reg_w),
        PrepTest(test_aarch64_dis_neg),
        PrepTest(test_aarch64_dis_mul),
        PrepTest(test_aarch64_dis_and),
        PrepTest(test_aarch64_dis_orr_w),
        PrepTest(test_aarch64_dis_eor),
        PrepTest(test_aarch64_dis_lsl),
        PrepTest(test_aarch64_dis_lsr),
        PrepTest(test_aarch64_dis_cmp),
        PrepTest(test_aarch64_dis_cset),
        PrepTest(test_aarch64_dis_cset_eq),
        PrepTest(test_aarch64_dis_b),
        PrepTest(test_aarch64_dis_bl),
        PrepTest(test_aarch64_dis_b_cond),
        PrepTest(test_aarch64_dis_ret),
        PrepTest(test_aarch64_dis_ldr),
        PrepTest(test_aarch64_dis_str),
        PrepTest(test_aarch64_dis_ldr_zero),
        PrepTest(test_aarch64_dis_ldrb),
        PrepTest(test_aarch64_dis_ldrh),
        PrepTest(test_aarch64_dis_ldr32),
        PrepTest(test_aarch64_dis_str32),
        PrepTest(test_aarch64_dis_strb),
        PrepTest(test_aarch64_dis_strh),
        PrepTest(test_aarch64_dis_ldrsb_x),
        PrepTest(test_aarch64_dis_ldrsh_x),
        PrepTest(test_aarch64_dis_ldrsw),
        PrepTest(test_aarch64_dis_stp_pre),
        PrepTest(test_aarch64_dis_ldp_post),
        PrepTest(test_aarch64_dis_cbnz),
        PrepTest(test_aarch64_dis_cbz_w),
        PrepTest(test_aarch64_dis_sxtb),
        PrepTest(test_aarch64_dis_sxth),
        PrepTest(test_aarch64_dis_sxtw),
        PrepTest(test_aarch64_dis_uxtb),
        PrepTest(test_aarch64_dis_uxth),
        PrepTest(test_aarch64_dis_mov_reg),
        PrepTest(test_aarch64_dis_adr),
        PrepTest(test_aarch64_dis_unknown),
        PrepTest(test_aarch64_dis_add_imm_sh),
        PrepTest(test_aarch64_dis_sub_imm_sh),
        PrepTest(test_aarch64_dis_bc_eq),
        PrepTest(test_aarch64_dis_bc_ne),
        PrepTest(test_aarch64_dis_bc_cs),
        PrepTest(test_aarch64_dis_bc_cc),
        PrepTest(test_aarch64_dis_bc_hi),
        PrepTest(test_aarch64_dis_bc_ls),
        PrepTest(test_aarch64_dis_bc_ge),
        PrepTest(test_aarch64_dis_bc_le),
        PrepTest(test_aarch64_dis_bc_gt),
        PrepTest(test_aarch64_dis_lsl_imm),
        PrepTest(test_aarch64_dis_lsr_imm),
        PrepTest(test_aarch64_dis_asr_imm),
        PrepTest(test_aarch64_dis_ubfx),
        PrepTest(test_aarch64_dis_sbfx),
        PrepTest(test_aarch64_dis_mvn),
        PrepTest(test_aarch64_dis_tst),
        PrepTest(test_aarch64_dis_bic),
        PrepTest(test_aarch64_dis_br),
        PrepTest(test_aarch64_dis_nop),
        PrepTest(test_aarch64_dis_add_reg_shifted),
        PrepTest(test_aarch64_dis_cmn_imm),
        PrepTest(test_aarch64_dis_cmp_imm),
        PrepTest(test_aarch64_dis_csel),
        PrepTest(test_aarch64_dis_csinv),
        PrepTest(test_aarch64_dis_csneg),
        PrepTest(test_aarch64_dis_csetm),
        PrepTest(test_aarch64_dis_ccmp_reg),
        PrepTest(test_aarch64_dis_ccmp_imm),
        PrepTest(test_aarch64_dis_ccmn_reg),
        PrepTest(test_aarch64_dis_madd),
        PrepTest(test_aarch64_dis_msub),
        PrepTest(test_aarch64_dis_mneg),
        PrepTest(test_aarch64_dis_udiv),
        PrepTest(test_aarch64_dis_sdiv),
        PrepTest(test_aarch64_dis_clz),
        PrepTest(test_aarch64_dis_cls),
        PrepTest(test_aarch64_dis_rbit),
        PrepTest(test_aarch64_dis_rev16),
        PrepTest(test_aarch64_dis_rev_x),
        PrepTest(test_aarch64_dis_extr),
        PrepTest(test_aarch64_dis_ror_imm),
        PrepTest(test_aarch64_dis_ldr_literal),
        PrepTest(test_aarch64_dis_ldrsw_literal),
        PrepTest(test_aarch64_dis_ldur),
        PrepTest(test_aarch64_dis_stur_w),
        PrepTest(test_aarch64_dis_ldr_post),
        PrepTest(test_aarch64_dis_str_pre),
        PrepTest(test_aarch64_dis_brk),
        PrepTest(test_aarch64_dis_svc),
        PrepTest(test_aarch64_dis_mov_imm_w),
        PrepTest(test_aarch64_dis_cmp_w),
        PrepTest(test_aarch64_dis_cset_w),
        PrepTest(test_aarch64_dis_and_imm),
        PrepTest(test_aarch64_dis_and_imm_replicated),
        PrepTest(test_aarch64_dis_orr_imm),
        PrepTest(test_aarch64_dis_mov_bitmask),
        PrepTest(test_aarch64_dis_tst_imm),
        PrepTest(test_aarch64_dis_tbz),
        PrepTest(test_aarch64_dis_tbnz_x),
        PrepTest(test_aarch64_dis_yield),
        PrepTest(test_aarch64_dis_wfe),
        PrepTest(test_aarch64_dis_wfi),
        PrepTest(test_aarch64_dis_ldr_reg),
        PrepTest(test_aarch64_dis_ldr_reg_sxtw),
        PrepTest(test_aarch64_dis_strb_reg),
        PrepTest(test_aarch64_dis_add_ext),
        PrepTest(test_aarch64_dis_sub_ext_shift),
        PrepTest(test_aarch64_dis_smull),
        PrepTest(test_aarch64_dis_umull),
        PrepTest(test_aarch64_dis_smulh),
        PrepTest(test_aarch64_dis_umulh),
        PrepTest(test_aarch64_dis_smaddl),
        PrepTest(test_aarch64_dis_cinc),
        PrepTest(test_aarch64_dis_cinv),
        PrepTest(test_aarch64_dis_cneg),
        PrepTest(test_aarch64_dis_bfi),
        PrepTest(test_aarch64_dis_bfxil),
        PrepTest(test_aarch64_dis_dmb_ish),
        PrepTest(test_aarch64_dis_dsb_sy),
        PrepTest(test_aarch64_dis_isb),
        PrepTest(test_aarch64_dis_mrs_nzcv),
        PrepTest(test_aarch64_dis_msr_tpidr),
        PrepTest(test_aarch64_dis_ldar),
        PrepTest(test_aarch64_dis_stlr_w),
        PrepTest(test_aarch64_dis_ldarb),
        PrepTest(test_aarch64_dis_ldxr),
        PrepTest(test_aarch64_dis_stxr),
        PrepTest(test_aarch64_dis_prfm),
        PrepTest(test_aarch64_dis_prfm_strm),
        PrepTest(test_aarch64_dis_blr),
        PrepTest(test_aarch64_dis_sev),
        PrepTest(test_aarch64_dis_sevl),
        PrepTest(test_aarch64_dis_fadd_d),
        PrepTest(test_aarch64_dis_fmul_s),
        PrepTest(test_aarch64_dis_fdiv_d),
        PrepTest(test_aarch64_dis_fsub_s),
        PrepTest(test_aarch64_dis_fnmul),
        PrepTest(test_aarch64_dis_fmov_d),
        PrepTest(test_aarch64_dis_fmov_s),
        PrepTest(test_aarch64_dis_fmov_h),
        PrepTest(test_aarch64_dis_fabs_d),
        PrepTest(test_aarch64_dis_fneg_s),
        PrepTest(test_aarch64_dis_fsqrt_d),
        PrepTest(test_aarch64_dis_fcvt_d_to_s),
        PrepTest(test_aarch64_dis_fcvt_s_to_d),
        PrepTest(test_aarch64_dis_frintn),
        PrepTest(test_aarch64_dis_frintz),
        PrepTest(test_aarch64_dis_fcmp_reg),
        PrepTest(test_aarch64_dis_fcmpe_reg),
        PrepTest(test_aarch64_dis_fcmp_zero),
        PrepTest(test_aarch64_dis_fmov_d_imm),
        PrepTest(test_aarch64_dis_fmov_x_to_d),
        PrepTest(test_aarch64_dis_fmov_d_to_x),
        PrepTest(test_aarch64_dis_fmov_w_to_s),
        PrepTest(test_aarch64_dis_scvtf_d_x),
        PrepTest(test_aarch64_dis_ucvtf_s_w),
        PrepTest(test_aarch64_dis_fcvtzs_x_d),
        PrepTest(test_aarch64_dis_fcvtzu_w_s),
        PrepTest(test_aarch64_dis_fcvtns_x_d),
        PrepTest(test_aarch64_dis_str_d),
        PrepTest(test_aarch64_dis_ldr_s),
        PrepTest(test_aarch64_dis_ldr_b),
        PrepTest(test_aarch64_dis_ldr_h),
        PrepTest(test_aarch64_dis_fadd_2s),
        PrepTest(test_aarch64_dis_fadd_4s),
        PrepTest(test_aarch64_dis_fadd_2d),
        PrepTest(test_aarch64_dis_fsub_4s),
        PrepTest(test_aarch64_dis_fmul_2d),
        PrepTest(test_aarch64_dis_fdiv_4s),
        PrepTest(test_aarch64_dis_fmax_4s),
        PrepTest(test_aarch64_dis_fmla_2s),
        PrepTest(test_aarch64_dis_bfdot_2s),
        PrepTest(test_aarch64_dis_bfdot_4s),
        PrepTest(test_aarch64_dis_bfdot_idx_2s),
        PrepTest(test_aarch64_dis_bfdot_idx_4s),
        PrepTest(test_aarch64_dis_bfdot_idx_hi),
        PrepTest(test_aarch64_dis_bfmlalb),
        PrepTest(test_aarch64_dis_bfmlalt),
        PrepTest(test_aarch64_dis_bfcvt),
        PrepTest(test_aarch64_dis_bfcvtn),
        PrepTest(test_aarch64_dis_bfcvtn2),
        PrepTest(test_aarch64_dis_adrp),
        PrepTest(test_aarch64_dis_blr_enc),
        PrepTest(test_aarch64_dis_ret_x9),
        PrepTest(test_aarch64_dis_msub_enc),
        PrepTest(test_aarch64_dis_sdiv_enc),
        PrepTest(test_aarch64_dis_asrv_enc),
        PrepTest(test_aarch64_dis_lsl_imm_enc),
        PrepTest(test_aarch64_dis_neg_reg_enc),
        PrepTest(test_aarch64_dis_subs_imm_enc),
        PrepTest(test_aarch64_dis_subs_reg_enc),
        PrepTest(test_aarch64_dis_add_lsl_enc),
        PrepTest(test_aarch64_dis_sub_lsr_enc),
        PrepTest(test_aarch64_dis_stp_off),
        PrepTest(test_aarch64_dis_ldp_off_w),
        PrepTest(test_aarch64_dis_stp_q_off),
        PrepTest(test_aarch64_dis_ldp_q_pre),
        PrepTest(test_aarch64_dis_ldp_d_off),
        PrepTest(test_aarch64_dis_stp_s_pre),
        PrepTest(test_aarch64_dis_ldnp),
        PrepTest(test_aarch64_dis_stnp),
        PrepTest(test_aarch64_dis_fmov_s_imm_neg),
        PrepTest(test_aarch64_dis_fmov_d_imm_half),
        PrepTest(test_aarch64_dis_fmov_d_imm_full_mant),
        PrepTest(test_aarch64_dis_ldr_post_enc),
        PrepTest(test_aarch64_dis_str_pre_enc),
        PrepTest(test_aarch64_dis_ldr_regoff_enc),
        PrepTest(test_aarch64_dis_ldur_enc),
        PrepTest(test_aarch64_dis_stur_enc),
        PrepTest(test_aarch64_dis_fmov_w_to_h),
        PrepTest(test_aarch64_dis_fmov_h_to_w),
        PrepTest(test_aarch64_dis_msr_daifset),
        PrepTest(test_aarch64_dis_msr_daifclr),
        PrepTest(test_aarch64_dis_cmp_imm_enc),
        PrepTest(test_aarch64_dis_tst_reg_enc),
        PrepTest(test_aarch64_dis_mvn_reg_enc),
        PrepTest(test_aarch64_dis_mov_reg_enc),
        PrepTest(test_aarch64_dis_mov_reg_w_enc),
        PrepTest(test_aarch64_dis_fmov_imm_enc),
        PrepTest(test_aarch64_dis_br_enc),
        PrepTest(test_aarch64_dis_msr_reg_enc),
        PrepTest(test_aarch64_dis_bfmmla),
        PrepTest(test_aarch64_dis_buffer_dump),
    };
    int n = (int)(sizeof tests / sizeof tests[0]);
    int failed = 0;
    for (int i = 0; i < n; i++) {
        int pass = tests[i].fn();
        if (pass) printf("PASSED: %s\n", tests[i].name);
        else {
            printf("FAILED: %s\n", tests[i].name);
            failed++;
        }
    }
    printf("\n%d/%d passed\n", n - failed, n);
    return failed ? 1 : 0;
}
