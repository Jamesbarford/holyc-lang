/* ARM64 inline-asm encoder.
 * The encoder is mnemonic-driven and uses the same AsmBlock parse output as
 * the x86 path. Register names are case insensitive, immediates prefixed with
 * '#' (though the parser also accepts bare integers), and memory references
 * inside square brackets.
 *
 * The supported list of instructions has balooned such that I'm not actually
 * sure what is/isn't supported. The core instructions are below. A good way
 * to get into the swing of adding them is using a python repl and using
 * `bin(...)` and `hex(...)` along with `objdump` of a small assembled piece of
 * code. Then you simply need to get the shifts right to create the machine
 * code.
 *
 * There is some BF16 support, though that is because I'm messing
 *
 * Core instructions, these should always work:
 *   MOV   reg, imm           (any 64-bit literal, synthesised via MOVZ+MOVK)
 *   MOV   reg, reg
 *   ADD   reg, reg, imm12
 *   ADD   reg, reg, reg
 *   SUB   reg, reg, imm12
 *   SUB   reg, reg, reg
 *   AND/ORR/EOR reg, reg, reg
 *   MUL   reg, reg, reg
 *   SDIV  reg, reg, reg
 *   CMP   reg, imm12         (alias for SUBS XZR, ...)
 *   CMP   reg, reg
 *   NEG   reg, reg
 *   LDR   reg, [base]        (unsigned-offset, 64-bit; offset optional)
 *   LDR   reg, [base, #imm]
 *   STR   reg, [base]
 *   STR   reg, [base, #imm]
 *   B     label              (unconditional)
 *   BL    label
 *   B.<cond> label           (cc = EQ/NE/LT/GT/LE/GE/...)
 *   BR    reg
 *   BLR   reg
 *   RET
 *
 * Local labels (`@@N`) use BR26 (B/BL) or IMM19 (B.cond) within the block;
 * unresolved symbolic labels become AF_SYMBOL fixups that the codegen
 * layer turns into Mach-O BRANCH26 relocations. */

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "asm.h"
#include "asm_enc.h"
#include "enc_arm64.h"
#include "tasm_util.h"

/*----------- Instruction encoders ------*/

/* ================================================================ moves */

/* MOVZ / MOVK / MOVN share the wide-immediate encoding layout:
 *   sf | opc(2) | 1 0 0 1 0 1 | hw(2) | imm16(16) | Rd(5)
 *
 * opc: 00 = MOVN, 10 = MOVZ, 11 = MOVK. */
static uint32_t
aarch64_mov_wide(int sf, uint32_t opc, A64Reg rd, uint16_t imm16, int hw)
{
    return (sf << 31) | /* sf = 1 (64-bit) */
           (opc << 29) |
           (0x25u << 23) | /* 0b00100101 in bits 28:23 */
           ((uint32_t)(hw & 3) << 21) |
           ((uint32_t)imm16 << 5) |
           ((uint32_t)rd & 0x1F);
}

static size_t
aarch64_enc_movz(AsmEnc *e, A64Reg rd, int sf, uint32_t imm16, int hw)
{
    return put_word(e, aarch64_mov_wide(sf, 0x2, rd, imm16, hw));
}

static size_t
aarch64_enc_movk(AsmEnc *e, A64Reg rd, int sf, uint16_t imm16, int hw)
{
    return put_word(e, aarch64_mov_wide(sf, 0x3, rd, imm16, hw));
}

size_t
aarch64_enc_movn(AsmEnc *e, A64Reg rd, int sf, uint16_t imm16, int hw)
{
    return put_word(e, aarch64_mov_wide(sf, 0x0, rd, imm16, hw));
}

static void
aarch64_enc_mov_imm(AsmEnc *e, A64Reg rd, int sf, uint64_t imm)
{
    /* Optimise the simple cases first. */
    if (imm == 0) {
        aarch64_enc_movz(e, rd, sf, 0, 0);
        return;
    }

    /* Always emit MOVZ for the lowest non-zero chunk, then MOVK for the
     * rest. Trailing zero chunks are skipped because MOVZ starts at zero. */
    int placed = 0;
    for (int hw = 0; hw < 4; hw++) {
        uint16_t chunk = (uint16_t)((imm >> (hw * 16)) & 0xFFFF);
        if (chunk == 0 && placed == 0 && hw < 3) continue;
        if (!placed) {
            aarch64_enc_movz(e, rd, sf, chunk, hw);
            placed = 1;
        } else if (chunk != 0) aarch64_enc_movk(e, rd, sf, chunk, hw);
    }
}

void
aarch64_enc_mov_imm64(AsmEnc *e, A64Reg rd, uint64_t imm)
{
    aarch64_enc_mov_imm(e, rd, 1, imm);
}

void
aarch64_enc_mov_imm32(AsmEnc *e, A64Reg rd, uint64_t imm)
{
    aarch64_enc_mov_imm(e, rd, 0, imm);
}


static size_t
aarch64_enc_mov_reg_base(AsmEnc *e, uint16_t op, A64Reg rd, A64Reg rm)
{
    uint32_t w = (op << 21)           |
                 ((uint32_t)rm << 16) |
                 (0x1Fu << 5)         |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

size_t
aarch64_enc_mov_reg(AsmEnc *e, A64Reg rd, A64Reg rm)
{
    /* MOV Xd, Xm  ≡  ORR Xd, XZR, Xm  (logical shifted register form).
     * Bits 31:21 are the fixed `1 01 01010 00 0` = 0x550, then Rm(5),
     * imm6 = 0, Rn = XZR (31), Rd. */
    return aarch64_enc_mov_reg_base(e, 0x550u, rd, rm);
}

size_t
aarch64_enc_mov_reg_w(AsmEnc *e, A64Reg rd, A64Reg rm)
{
    /* 32-bit variant: same as MOV X form but sf=0 (bit 31 cleared).
     * Base = 0x150 << 21 = ORR Wd, WZR, Wm. */
    return aarch64_enc_mov_reg_base(e, 0x150u, rd, rm);
}

/* ================================================================ arithmetic
 */

/* ADD/SUB (immediate). Encoding:
 *   sf | op | S | 1 0 0 0 1 0 | sh | imm12 | Rn | Rd
 * sf: 0 = W (32-bit), 1 = X (64-bit).
 * op: 0 = ADD, 1 = SUB.  S: 0 (set-flags = ADDS/SUBS when 1).
 *
 * Values up to 4095 use sh=0. Values that are a multiple of 4096 and
 * fit in the upper 12-bit field (i.e. value <= 4095 * 4096) use sh=1.
 * Anything else truncates - callers expecting larger values must split
 * with MOVZ/MOVK + register form themselves. */
static uint32_t
aarch64_add_sub_imm(int sf, uint32_t op, A64Reg rd, A64Reg rn, uint32_t imm)
{
    uint32_t sh = 0;
    uint32_t imm12;
    if (imm <= 0xFFFu) {
        imm12 = imm;
    } else if ((imm & 0xFFFu) == 0 && (imm >> 12) <= 0xFFFu) {
        sh = 1;
        imm12 = imm >> 12;
    } else {
        imm12 = imm & 0xFFFu;  /* legacy fallback, caller's problem */
    }
    return (((uint32_t)sf & 1u) << 31)  |
           (op << 30)                   |
           (0x22u << 23)                |
           (sh << 22)                   |
           ((imm12 & 0xFFFu) << 10)     |
           ((uint32_t)rn << 5)          |
           ((uint32_t)rd & 0x1F);
}

size_t
aarch64_enc_add_imm(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, uint32_t imm12)
{
    return put_word(e, aarch64_add_sub_imm(is_64, 0, rd, rn, imm12));
}

size_t
aarch64_enc_sub_imm(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, uint32_t imm12)
{
    return put_word(e, aarch64_add_sub_imm(is_64, 1, rd, rn, imm12));
}

/* ADD/SUB (shifted register), generalised:
 *   sf | op | S | 0 1 0 1 1 | shift_type:2 | 0 | Rm:5 | imm6:6 | Rn:5 | Rd:5
 *
 *   shift_type: 0=LSL, 1=LSR, 2=ASR, 3=ROR (ROR not valid for ADD/SUB)
 *   imm6:       shift amount (0..63 for X, 0..31 for W)
 *   `set_flags` is S, used by SUBS/ADDS. */
static uint32_t
aarch64_add_sub_reg_shifted(int sf,
                            int op,
                            int set_flags,
                            int shift_type,
                            int shift_amount,
                            A64Reg rd,
                            A64Reg rn,
                            A64Reg rm)
{
    return (((uint32_t)sf & 1u) << 31)              |
           (((uint32_t)op & 1u) << 30)              |
           (((uint32_t)set_flags & 1u) << 29)       |
           (0x0Bu << 24)                            |
           (((uint32_t)shift_type & 0x3u) << 22)    |
           (((uint32_t)rm & 0x1F) << 16)            |
           (((uint32_t)shift_amount & 0x3Fu) << 10) |
           (((uint32_t)rn & 0x1F) << 5)             |
           ((uint32_t)rd & 0x1F);
}

size_t
aarch64_enc_add_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return put_word(e, aarch64_add_sub_reg_shifted(is_64, 0, 0, 0, 0, rd, rn, rm));
}

size_t
aarch64_enc_sub_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return put_word(e, aarch64_add_sub_reg_shifted(is_64, 1, 0, 0, 0, rd, rn, rm));
}

/* Shifted-register variants exposed for the dispatcher. */
size_t
aarch64_enc_add_reg_shifted(AsmEnc *e,
                            int is_64,
                            A64Reg rd,
                            A64Reg rn,
                            A64Reg rm,
                            int shift_type,
                            int shift_amount)
{
    uint32_t w = aarch64_add_sub_reg_shifted(is_64, 0, 0, shift_type,
                                             shift_amount, rd, rn, rm) ;
    return put_word(e, w);
}

size_t
aarch64_enc_sub_reg_shifted(AsmEnc *e,
                            int is_64,
                            A64Reg rd,
                            A64Reg rn,
                            A64Reg rm,
                            int shift_type,
                            int shift_amount)
{
    uint32_t w = aarch64_add_sub_reg_shifted(is_64, 1, 0, shift_type,
                                             shift_amount, rd, rn, rm) ;
    return put_word(e, w);
}

size_t
aarch64_enc_uxtb(AsmEnc *e,
                 int is_64,
                 A64Reg rd,
                 A64Reg rn)
{
    uint32_t w = (is_64 << 31) |
                 (0x2   << 29) |
                 (0x26u << 23) |
                 (0x7   << 10) |
                 ((uint32_t)rn << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* UXTH Wd, Wn = UBFM Wd, Wn, #0, #15. */
size_t
aarch64_enc_uxth(AsmEnc *e, A64Reg rd, A64Reg rn)
{
    uint32_t w = 0x53003C00u |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* SXTB/SXTH/SXTW Xd, Wn = SBFM Xd, Xn, #0, #(width-1). */
static uint32_t
aarch64_sxt(uint32_t imms, A64Reg rd, A64Reg rn)
{
    return 0x93400000u |
           (imms << 10) |
           (((uint32_t)rn & 0x1F) << 5) |
           ((uint32_t)rd & 0x1F);
}

size_t
aarch64_enc_sxtb(AsmEnc *e, A64Reg rd, A64Reg rn)
{
    return put_word(e, aarch64_sxt(7, rd, rn));
}

size_t
aarch64_enc_sxth(AsmEnc *e, A64Reg rd, A64Reg rn)
{
    return put_word(e, aarch64_sxt(15, rd, rn));
}

size_t
aarch64_enc_sxtw(AsmEnc *e, A64Reg rd, A64Reg rn)
{
    return put_word(e, aarch64_sxt(31, rd, rn));
}

/* ================================================================ loads /
 * stores */

/* LDR/STR (unsigned offset), 64-bit:
 *   bits 31:24 = 11111001 (0xF9)
 *   bits 23:22 = opc (00 STR, 01 LDR)
 *   bits 21:10 = imm12 (offset / 8, in 8-byte units)
 *   bits 9:5   = Rn, bits 4:0 = Rt */
static uint32_t
aarch64_ldst_uimm(uint32_t opc, A64Reg rt, A64Reg rn, uint32_t off)
{
    uint32_t pimm12 = (off / 8u) & 0xFFFu;
    return (0xF9u << 24)         |
           (opc << 22)           |
           (pimm12 << 10)        |
           ((uint32_t)rn << 5)   |
           ((uint32_t)rt & 0x1F);
}

size_t
aarch64_enc_ldr_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off)
{
    return put_word(e, aarch64_ldst_uimm(0x1, rt, rn, off));
}

size_t
aarch64_enc_str_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off)
{
    return put_word(e, aarch64_ldst_uimm(0x0, rt, rn, off));
}

/* Same shape, but for 32-bit GPRs (Wt).  size=10, V=0.  imm12 scaled by 4. */
size_t
aarch64_enc_ldr32_imm_gpr(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off)
{
    uint32_t pimm12 = (off / 4u) & 0xFFFu;
    uint32_t w = (0xB9u << 24)         |
                 (1u << 22)            |
                 (pimm12 << 10)        |
                 ((uint32_t)rn << 5)   |
                 ((uint32_t)rt & 0x1F);
    return put_word(e, w);
}

size_t
aarch64_enc_str32_imm_gpr(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off)
{
    uint32_t pimm12 = (off / 4u) & 0xFFFu;
    uint32_t w = (0xB9u << 24)         |
                 (0u << 22)            |
                 (pimm12 << 10)        |
                 ((uint32_t)rn << 5)   |
                 ((uint32_t)rt & 0x1F);
    return put_word(e, w);
}

/* Scalar FP 2-source arithmetic.  Encoding:
 *   0 0 0 11110 ftype 1 Rm opcode4 10 Rn Rd
 *
 *   ftype: 00=S, 01=D, 11=H (FP16 - not used here)
 *   opcode4: 0000=FMUL, 0001=FDIV, 0010=FADD, 0011=FSUB
 *   `is_double` chooses S(0) vs D(1). */
size_t
aarch64_enc_fp_2src_scalar(AsmEnc *e,
                           int is_double,
                           int opcode4,
                           A64Reg rd,
                           A64Reg rn,
                           A64Reg rm)
{
    uint32_t ftype = is_double ? 1u : 0u;
    uint32_t w = (0x1Eu << 24)                      |
                 (ftype << 22)                      |
                 (1u << 21)                         |
                 (((uint32_t)rm & 0x1F) << 16)      |
                 (((uint32_t)opcode4 & 0xFu) << 12) |
                 (2u << 10)                         | /* bit 11 = 1, bit 10 = 0 */
                 (((uint32_t)rn & 0x1F) << 5)       |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* FNEG (scalar): 0 0 0 11110 ftype 1 000010 10000 Rn Rd. */
size_t
aarch64_enc_fneg_scalar(AsmEnc *e, int is_double, A64Reg rd, A64Reg rn)
{
    uint32_t base = is_double ? 0x1E614000u : 0x1E214000u;
    uint32_t w = base |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* FCMP (scalar, register): 0 0 0 11110 ftype 1 Rm 001000 Rn 00000. */
size_t
aarch64_enc_fcmp_reg2(AsmEnc *e, int is_double, A64Reg rn, A64Reg rm)
{
    uint32_t base = is_double ? 0x1E602000u : 0x1E202000u;
    uint32_t w = base |
                 (((uint32_t)rm & 0x1F) << 16) |
                 (((uint32_t)rn & 0x1F) << 5);
    return put_word(e, w);
}

/* FCMP (scalar, zero): as register form with Rm=0 and opcode2 bit 3 set. */
size_t
aarch64_enc_fcmp_zero(AsmEnc *e, int is_double, A64Reg rn)
{
    uint32_t base = is_double ? 0x1E602008u : 0x1E202008u;
    return put_word(e, base | (((uint32_t)rn & 0x1F) << 5));
}

/* FCVT Sd, Dn - narrow double to single. */
size_t
aarch64_enc_fcvt_narrow(AsmEnc *e, A64Reg rd, A64Reg rn)
{
    uint32_t w = 0x1E624000u |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* FCVT Dd, Sn - widen single to double. */
size_t
aarch64_enc_fcvt_widen(AsmEnc *e, A64Reg rd, A64Reg rn)
{
    uint32_t w = 0x1E22C000u |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* FP<->integer conversions.  Encoding:
 *   sf 0 0 11110 ftype 1 rmode opcode 000000 Rn Rd
 * `is_64` selects the X(1) vs W(0) GPR; `fp_is_double` selects the
 * FP side: D(ftype=01) vs S(ftype=00). */
static uint32_t
aarch64_fp_int_cvt(int is_64, int fp_is_double, uint32_t rmode_opc,
                   A64Reg rd, A64Reg rn)
{
    uint32_t ftype = fp_is_double ? 1u : 0u;
    return (((uint32_t)is_64 & 1u) << 31) |
           0x1E200000u |            /* base with ftype = 00 (single) */
           (ftype << 22)           |
           (rmode_opc << 16) |
           (((uint32_t)rn & 0x1F) << 5) |
           ((uint32_t)rd & 0x1F);
}

/* FCVTZS <Rd>, <Fn> (rmode=11, opcode=000). */
size_t
aarch64_enc_fcvtzs(AsmEnc *e, int is_64, int fp_is_double, A64Reg rd, A64Reg rn)
{
    return put_word(e, aarch64_fp_int_cvt(is_64, fp_is_double, 0x18, rd, rn));
}

/* FCVTZU <Rd>, <Fn> (rmode=11, opcode=001). */
size_t
aarch64_enc_fcvtzu(AsmEnc *e, int is_64, int fp_is_double, A64Reg rd, A64Reg rn)
{
    return put_word(e, aarch64_fp_int_cvt(is_64, fp_is_double, 0x19, rd, rn));
}

/* SCVTF <Fd>, <Rn> (rmode=00, opcode=010). */
size_t
aarch64_enc_scvtf(AsmEnc *e, int is_64, int fp_is_double, A64Reg rd, A64Reg rn)
{
    return put_word(e, aarch64_fp_int_cvt(is_64, fp_is_double, 0x02, rd, rn));
}

/* UCVTF <Fd>, <Rn> (rmode=00, opcode=011). */
size_t
aarch64_enc_ucvtf(AsmEnc *e, int is_64, int fp_is_double, A64Reg rd, A64Reg rn)
{
    return put_word(e, aarch64_fp_int_cvt(is_64, fp_is_double, 0x03, rd, rn));
}

/* Vector AdvSIMD floating-point 3-same.  Encoding:
 *   0 Q U 01110 bit23 sz 1 Rm opcode5 1 Rn Rd
 *
 *   FADD: U=0, bit23=0, opcode=11010 (0x1A)
 *   FSUB: U=0, bit23=1, opcode=11010
 *   FMUL: U=1, bit23=0, opcode=11011 (0x1B)
 *   FDIV: U=1, bit23=0, opcode=11111 (0x1F)
 *
 *   <arr>: 2S -> Q=0, sz=0;  4S -> Q=1, sz=0;  2D -> Q=1, sz=1.  Other
 *   combinations (2D@Q=0, 4S@sz=1, etc.) are not valid AdvSIMD forms. */
size_t
aarch64_enc_fp_3same_vec(AsmEnc *e,
                         int Q,
                         int U,
                         int bit23,
                         int sz,
                         int opcode5,
                         A64Reg rd,
                         A64Reg rn,
                         A64Reg rm)
{
    uint32_t w = (((uint32_t)Q & 1) << 30)          |
                 (((uint32_t)U & 1) << 29)          |
                 (0xEu << 24)                       | /* 01110 */
                 (((uint32_t)bit23 & 1) << 23)      |
                 (((uint32_t)sz & 1) << 22)         |
                 (1u << 21)                         |
                 (((uint32_t)rm & 0x1F) << 16)      |
                 (((uint32_t)opcode5 & 0x1F) << 11) |
                 (1u << 10)                         |
                 (((uint32_t)rn & 0x1F) << 5)       |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* LDP/STP - load/store pair of registers.  Encoding:
 *   opc:2 101 V mode:3 L imm7:7 Rt2:5 Rn:5 Rt:5
 *
 *   opc: 00 = 32-bit (W / S), 01 = 64-bit FP (D), 10 = 64-bit GPR (X) / 128-bit
 * (Q)
 *        - When V=0: opc=00 -> W, opc=10 -> X (opc=01 unused / reserved here)
 *        - When V=1: opc=00 -> S, opc=01 -> D, opc=10 -> Q
 *   mode: 001 = post-index    [Xn], #imm
 *         010 = signed offset [Xn, #imm]
 *         011 = pre-index     [Xn, #imm]!
 *         000 = non-temporal  (not exposed here)
 *   imm7: signed 7-bit, scaled by the access size of one element (4/8/16).
 */
static size_t
aarch64_ldp_stp_imm(AsmEnc *e,
                    uint32_t opc,
                    int is_simd,
                    uint32_t mode_bits,
                    int is_load,
                    A64Reg rt,
                    A64Reg rt2,
                    A64Reg rn,
                    int32_t simm7)
{
    uint32_t imm7 = ((uint32_t)simm7) & 0x7Fu;
    uint32_t w = (opc << 30)                          |
                 (0x5u << 27) /* bits 29..27 = 101 */ |
                 (((uint32_t)is_simd & 1u) << 26)     |
                 (mode_bits << 23)                    |
                 (((uint32_t)is_load & 1u) << 22)     |
                 (imm7 << 15)                         |
                 (((uint32_t)rt2 & 0x1F) << 10)       |
                 (((uint32_t)rn & 0x1F) << 5)         |
                 ((uint32_t)rt & 0x1F);
    return put_word(e, w);
}

/* Convenience wrappers. `mode` codes: 0 = signed offset, 1 = pre-index,
 * 2 = post-index, 3 = non-temporal (LDNP/STNP). `width` is the
 * per-element width in bytes (4/8/16); is_simd selects GPR vs SIMD.
 * Returns 1 on success, 0 if the operands aren't encodable. */
int
aarch64_enc_ldst_pair(AsmEnc *e,
                      int is_simd,
                      int is_load,
                      int width,
                      int mode,
                      A64Reg rt,
                      A64Reg rt2,
                      A64Reg rn,
                      int32_t imm_bytes)
{
    uint32_t opc;
    int scale;
    if (is_simd) {
        switch (width) {
            case 4:  opc = 0; scale = 4; break; /* S */
            case 8:  opc = 1; scale = 8; break; /* D */
            case 16: opc = 2; scale = 16; break; /* Q */
            default: return 0;
        }
    } else {
        switch (width) {
            case 4:  opc = 0; scale = 4; break; /* W */
            case 8:  opc = 2; scale = 8; break; /* X */
            default: return 0;
        }
    }
    if (imm_bytes % scale != 0) return 0;
    int32_t simm7 = imm_bytes / scale;
    if (simm7 < -64 || simm7 > 63) return 0;
    uint32_t mode_bits;
    switch (mode) {
        case 0:  mode_bits = 0x2u; break; /* signed offset */
        case 1:  mode_bits = 0x3u; break; /* pre-index */
        case 2:  mode_bits = 0x1u; break; /* post-index */
        case 3:  mode_bits = 0x0u; break; /* non-temporal */
        default: return 0;
    }
    aarch64_ldp_stp_imm(e, opc, is_simd, mode_bits, is_load, rt, rt2, rn, simm7);
    return 1;
}

/* SUBS (subtract, set flags).  Two variants:
 *   - immediate:   sf op=1 S=1 100010 sh imm12 Rn Rd        (sh=0 only here)
 *   - shifted reg: sf op=1 S=1 01011 shift_type 0 Rm imm6 Rn Rd
 *
 * `is_64` chooses sf (1=X, 0=W).  For the reg form, `shift_amount` goes
 * into imm6 (LSL only - shift_type=00). */
size_t
aarch64_enc_subs_imm(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, uint32_t imm12)
{
    uint32_t w = (((uint32_t)is_64 & 1u) << 31) |
                 (1u << 30) /* op = SUB */      |
                 (1u << 29) /* S = set flags */ |
                 (0x22u << 23) /* 100010 */     |
                 ((imm12 & 0xFFFu) << 10)       |
                 (((uint32_t)rn & 0x1F) << 5)   |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

size_t
aarch64_enc_subs_reg(AsmEnc *e,
                     int is_64,
                     A64Reg rd,
                     A64Reg rn,
                     A64Reg rm,
                     int shift_type,
                     int shift_amount)
{
    uint32_t w = aarch64_add_sub_reg_shifted(is_64, 1, 1, shift_type,
                                             shift_amount, rd, rn, rm);
    return put_word(e, w);
}

/* Pre/post-index immediate (signed 9-bit) load/store.  Encoding:
 *   size:2 111 V 00 opc:2 0 imm9:9 op2:2 Rn:5 Rt:5
 *
 *   op2 = 01 -> post-index    [Xn], #imm
 *   op2 = 11 -> pre-index     [Xn, #imm]!
 *
 * imm9 is NOT scaled (raw bytes, -256..255). */
static size_t
aarch64_fp_or_gpr_ldst_idx(AsmEnc *e,
                           int is_simd,
                           int is_load,
                           int width,
                           uint32_t op2,
                           A64Reg rt,
                           A64Reg rn,
                           int32_t simm9)
{
    uint32_t size, opc;
    if (is_simd) {
        switch (width) {
            case 1: size = 0;  opc = is_load ? 1u : 0u; break;
            case 2: size = 1;  opc = is_load ? 1u : 0u; break;
            case 4: size = 2;  opc = is_load ? 1u : 0u; break;
            case 8: size = 3;  opc = is_load ? 1u : 0u; break;
            case 16: size = 0; opc = is_load ? 3u : 2u; break;
            default: return 0;
        }
    } else {
        /* GPR variants. Width => size: 1=>00, 2=>01, 4=>10, 8=>11.
         * LDRB/H/W/X all use opc=01 (zero-extending load); STR uses opc=00.
         * Signed-extending sub-int loads (LDRSB/H/SW) are not emitted here. */
        switch (width) {
            case 1: size = 0; opc = is_load ? 1u : 0u; break;
            case 2: size = 1; opc = is_load ? 1u : 0u; break;
            case 4: size = 2; opc = is_load ? 1u : 0u; break;
            case 8: size = 3; opc = is_load ? 1u : 0u; break;
            default: return 0;
        }
    }

    uint32_t imm9 = ((uint32_t)simm9) & 0x1FFu;
    uint32_t w = (size << 30) |
                 (0x7u << 27) /* bits 29..27 = 111 */ |
                 (((uint32_t)is_simd & 1u) << 26) /* V bit */|
                 (opc << 22) |
                 (imm9 << 12) |
                 (op2 << 10) |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rt & 0x1F);
    return put_word(e, w);
}

size_t
aarch64_enc_ldst_post(AsmEnc *e,
                      int is_simd,
                      int is_load,
                      int width,
                      A64Reg rt,
                      A64Reg rn,
                      int32_t simm9)
{
    return aarch64_fp_or_gpr_ldst_idx(e, is_simd, is_load, width, 0x1u, rt, rn, simm9);
}

size_t
aarch64_enc_ldst_pre(AsmEnc *e,
                    int is_simd,
                    int is_load,
                    int width,
                    A64Reg rt,
                    A64Reg rn,
                    int32_t simm9)
{
    return aarch64_fp_or_gpr_ldst_idx(e, is_simd, is_load, width, 0x3u, rt, rn, simm9);
}

/* LDUR/STUR - unscaled signed-imm9 offset.  Same encoding as the
 * pre/post-index family but with op2=00, so the address isn't
 * writeback-updated.  Used when a [Xn, #imm] reaches a negative or
 * non-natural-aligned displacement that the scaled imm12 form rejects. */
static size_t
_aarch64_enc_ldst_unscaled(AsmEnc *e,
                           int is_simd,
                           int is_load,
                           int width,
                           A64Reg rt,
                           A64Reg rn,
                           int32_t simm9)
{
    return aarch64_fp_or_gpr_ldst_idx(e, is_simd, is_load, width, 0x0u, rt, rn, simm9);
}

/* Register-offset load/store.  Encoding:
 *   size:2 111 V 00 opc:2 1 Rm:5 option:3 S 10 Rn:5 Rt:5
 *
 * We only emit the `LSL` shift variant (option=011 = UXTX/LSL).  S=1 when
 * the user wrote an LSL that matches the natural access-width scale,
 * S=0 when no shift was specified.
 *
 * Broadly;
 * ldr  Rt, [Rn, Rm]
 * str  Rt, [Rn, Rm]
 * ldr  Rt, [Rn, Rm, LSL #scale]
 * str  Rt, [Rn, Rm, LSL #scale]
 */
size_t
aarch64_enc_ldst_regoff(AsmEnc *e,
                        int is_simd,
                        int is_load,
                        int width,
                        int scaled,
                        A64Reg rt,
                        A64Reg rn,
                        A64Reg rm)
{
    uint32_t size, opc;
    if (is_simd) {
        switch (width) {
            case 1: size = 0; opc = is_load ? 1u : 0u; break;
            case 2: size = 1; opc = is_load ? 1u : 0u; break;
            case 4: size = 2; opc = is_load ? 1u : 0u; break;
            case 8: size = 3; opc = is_load ? 1u : 0u; break;
            case 16: size = 0; opc = is_load ? 3u : 2u; break;
            default: return 0;
        }
    } else {
        switch (width) {
            case 1: size = 0; opc = is_load ? 1u : 0u; break;
            case 2: size = 1; opc = is_load ? 1u : 0u; break;
            case 4: size = 2; opc = is_load ? 1u : 0u; break;
            case 8: size = 3; opc = is_load ? 1u : 0u; break;
            default: return 0;
        }
    }
    /* Scaled offset bit */
    uint32_t S = scaled ? 1u : 0u;
    uint32_t option = 0x3u; /* UXTX/LSL */
    uint32_t w = (size << 30) |
                 (0x7u << 27) |
                 (((uint32_t)is_simd & 1u) << 26) |
                 (opc << 22) |
                 (1u << 21) |
                 (((uint32_t)rm & 0x1F) << 16) |
                 (option << 13) |
                 (S << 12) |
                 (0x2u << 10) |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rt & 0x1F);
    return put_word(e, w);
}

/* CBZ / CBNZ - compare and branch (if zero / non-zero).  Encoding:
 *   sf 011010 op imm19 Rt
 *
 * sf=1 for X, sf=0 for W.  op=0 for CBZ, 1 for CBNZ.
 * imm19 is the signed offset in 4-byte units from this instruction. */
static size_t
_aarch64_enc_cbz(AsmEnc *e, int is_64, int is_nonzero, A64Reg rt, int32_t rel_words)
{
    uint32_t w = (((uint32_t)is_64 & 1u) << 31) |
                 (0x1Au << 25) /* 011010 at bits 30..25 */ |
                 (((uint32_t)is_nonzero & 1u) << 24) |
                 ((((uint32_t)rel_words) & 0x7FFFFu) << 5) |
                 ((uint32_t)rt & 0x1F);
    return put_word(e, w);
}

/* FP scalar 1-source: FABS / FNEG / FSQRT.  Encoding:
 *   0 0 0 11110 ftype 1 opcode6 10000 Rn Rd
 *
 *   opcode6: FMOV=000000 (already), FABS=000001, FNEG=000010, FSQRT=000011
 *   ftype:   00 = S, 01 = D. */
static size_t
_aarch64_enc_fp_1src(AsmEnc *e, int is_double, int opcode6, A64Reg rd, A64Reg rn)
{
    uint32_t ftype = is_double ? 1u : 0u;
    uint32_t w = (0x1Eu << 24) |
                 (ftype << 22) |
                 (1u << 21) |
                 (((uint32_t)opcode6 & 0x3Fu) << 15) |
                 (0x10u << 10) /* bits 14..10 = 10000 */ |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* FP compare.  Encoding:
 *   0 0 0 11110 ftype 1 Rm 0010 00 Rn opc2
 *
 *   opc2 (5 bits at 4..0): 00000 FCMP reg, 10000 FCMPE reg,
 *                          01000 FCMP zero, 11000 FCMPE zero.
 *   ftype: 00=S, 01=D.  For zero-compare Rm must be 0. */
static size_t
_aarch64_enc_fcmp(AsmEnc *e,
                  int is_double,
                  int with_exceptions,
                  int compare_zero,
                  A64Reg rn,
                  A64Reg rm)
{
    uint32_t ftype = is_double ? 1u : 0u;
    uint32_t opc2 = (((uint32_t)compare_zero & 1u) << 3) |
                    (((uint32_t)with_exceptions & 1u) << 4);
    uint32_t w = (0x1Eu << 24) |
                 (ftype << 22) |
                 (1u << 21) |
                 (((uint32_t)rm & 0x1F) << 16) |
                 (0x2u << 12) /* bits 15..12 = 0010 */ |
                 (((uint32_t)rn & 0x1F) << 5) |
                 (opc2 & 0x1F);
    return put_word(e, w);
}

/* FP <-> int conversions.  Shares encoding family with FMOV reg/gpr:
 *   sf 0 0 11110 ftype 1 [rmode:2 opcode:3] 000000 Rn Rd
 *
 *   rmode<<3 | opcode (5 bits) selects:
 *     FCVTZS  = 11_000 (rmode=11 truncate, opcode=000 signed)
 *     FCVTZU  = 11_001
 *     SCVTF   = 00_010
 *     UCVTF   = 00_011
 *   ftype/sf are tied to the operand widths:
 *     S<->W : sf=0 ftype=00
 *     D<->X : sf=1 ftype=01 */
static size_t
_aarch64_enc_fp_int_cvt(AsmEnc *e,
                        int sf,
                        int ftype,
                        int rmode_opcode5,
                        A64Reg rd,
                        A64Reg rn)
{
    uint32_t w = (((uint32_t)sf & 1u) << 31) |
                 (0x1Eu << 24) |
                 (((uint32_t)ftype & 3u) << 22) |
                 (1u << 21) |
                 (((uint32_t)rmode_opcode5 & 0x1Fu) << 16) |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* BF16 instruction encoders (FEAT_BF16, Armv8.6+).  Encodings byte-verified
 * against clang's assembler (armv8.6-a+bf16). They also seem to do what I'd
 * expect them to do. `Q` is the vector size
 *
 *   BFDOT V<d>.<T>, V<n>.<Ta>, V<m>.<Ta>
 *     T = 2S -> Q=0, Ta=4H ; T = 4S -> Q=1, Ta=8H
 *     bits 31..0 : 0 Q 1 0 1 1 1 0 0 1 0 Rm 1 1 1 1 1 1 Rn Rd
 *
 *   BFDOT V<d>.<T>, V<n>.<Ta>, V<m>.2H[index]   (indexed-element form)
 *     bits 31..0 : 0 Q 0 0 1 1 1 1 0 1 L M Rm[3:0] 1 1 1 1 H 0 Rn Rd
 *     index = (H << 1) | L          (H = bit 11, L = bit 21)
 *
 *   BFMLALB V<d>.4S, V<n>.8H, V<m>.8H
 *     bits 31..0 : 0 0 1 0 1 1 1 0 1 1 0 Rm 1 1 1 1 1 1 Rn Rd
 *
 *   BFMLALT V<d>.4S, V<n>.8H, V<m>.8H
 *     bits 31..0 : 0 1 1 0 1 1 1 0 1 1 0 Rm 1 1 1 1 1 1 Rn Rd
 *
 *   BFMMLA V<d>.4S, V<n>.8H, V<m>.8H
 *     bits 31..0 : 0 1 1 0 1 1 1 0 0 1 0 Rm 1 1 1 0 1 1 Rn Rd
 *
 *   BFCVT H<d>, S<n>           - scalar fp32 -> bf16
 *     bits 31..0 : 0 0 0 1 1 1 1 0 0 1 1 0 0 0 1 1 0 1 0 0 0 0 Rn Rd
 *
 *   BFCVTN  V<d>.4H, V<n>.4S   - vector narrow (low half)
 *   BFCVTN2 V<d>.8H, V<n>.4S   - vector narrow (high half)
 *     bits 31..0 : 0 Q 0 0 1 1 1 0 1 0 1 0 0 0 0 1 0 1 1 0 1 0 Rn Rd
 *     Q = 0 -> BFCVTN ; Q = 1 -> BFCVTN2 */
size_t
aarch64_enc_bfdot_vec(AsmEnc *e, int Q, A64Reg rd, A64Reg rn, A64Reg rm)
{
    uint32_t w = 0x2E40FC00u |
                 (((uint32_t)Q & 1) << 30) |
                 (((uint32_t)rm & 0x1F) << 16) |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

size_t
aarch64_enc_bfdot_idx(AsmEnc *e, int Q, A64Reg rd, A64Reg rn, A64Reg rm, int idx)
{
    uint32_t M = ((uint32_t)rm >> 4) & 1u;
    uint32_t Rm4 = (uint32_t)rm & 0xFu;
    uint32_t H = ((uint32_t)idx >> 1) & 1u;
    uint32_t L = (uint32_t)idx & 1u;
    uint32_t w = 0x0F40F000u                  |
                 (((uint32_t)Q & 1) << 30)    |
                 (L << 21)                    |
                 (M << 20)                    |
                 (Rm4 << 16)                  |
                 (H << 11)                    |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

size_t
aarch64_enc_bfmlalb(AsmEnc *e, A64Reg rd, A64Reg rn, A64Reg rm)
{
    uint32_t w = 0x2EC0FC00u                   |
                 (((uint32_t)rm & 0x1F) << 16) |
                 (((uint32_t)rn & 0x1F) << 5)  |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

size_t
aarch64_enc_bfmlalt(AsmEnc *e, A64Reg rd, A64Reg rn, A64Reg rm)
{
    uint32_t w = 0x6EC0FC00u                   |
                 (((uint32_t)rm & 0x1F) << 16) |
                 (((uint32_t)rn & 0x1F) << 5)  |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

size_t
aarch64_enc_bfcvt(AsmEnc *e, A64Reg rd, A64Reg rn)
{
    uint32_t w = 0x1E634000u                  |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

size_t
_aarch64_enc_bfcvtn(AsmEnc *e, int Q, A64Reg rd, A64Reg rn)
{
    uint32_t w = 0x0EA16800u                  |
                 (((uint32_t)Q & 1) << 30)    |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* FMOV (FP<->FP, FP<->GPR, FP←imm).
 *
 * Variants encoded here:
 *   FMOV Sd, Sn   - scalar register copy (single)
 *   FMOV Dd, Dn   - scalar register copy (double)
 *   FMOV Sd, Wn   - GPR->FP (32-bit)
 *   FMOV Wd, Sn   - FP->GPR (32-bit)
 *   FMOV Dd, Xn   - GPR->FP (64-bit)
 *   FMOV Xd, Dn   - FP->GPR (64-bit)
 *   FMOV Sd, #imm - 8-bit constrained FP immediate (single)
 *   FMOV Dd, #imm - 8-bit constrained FP immediate (double)
 */
size_t
aarch64_enc_fmov_reg(AsmEnc *e, int is_double, A64Reg rd, A64Reg rn)
{
    /* 0 0 0 11110 type 1 00000 100000 Rn Rd
     *   type=00 (S) -> base 0x1E204000
     *   type=01 (D) -> base 0x1E604000
     */
    uint32_t base = is_double ? 0x1E604000u : 0x1E204000u;
    uint32_t w = base                         |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* FMOV between GPR and FP scalar:
 *   sf 0 0 11110 type 1 rmode opcode 000000 Rn Rd
 *
 * is_double=0 -> S<->W (sf=0, type=00);  is_double=1 -> D<->X (sf=1, type=01).
 * fp_to_gpr=1 selects opcode=110 (FP->GPR); =0 selects opcode=111 (GPR->FP). */
size_t
aarch64_enc_fmov_gpr(AsmEnc *e, int is_double, int fp_to_gpr, A64Reg rd, A64Reg rn)
{
    uint32_t sf = is_double ? 1u : 0u;
    uint32_t type = is_double ? 1u : 0u;
    uint32_t opc = fp_to_gpr ? 6u : 7u; /* 110 or 111 */
    uint32_t w = (sf << 31) |
                 (0x1Eu << 24) /* bits 28..24 = 11110, bit29=0, bit28 stays 0 */
                 | (type << 22) |
                 (1u << 21) |
                 (opc << 16) |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* FMOV Hd, Hn - half-precision scalar copy (FEAT_FP16, type=11). */
size_t
aarch64_enc_fmov_reg_h(AsmEnc *e, A64Reg rd, A64Reg rn)
{
    uint32_t w = 0x1EE04000u                  |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* FMOV W<->H - GPR/FP bridge for the bf16 widen sequence.
 * `fp_to_gpr=1` -> `FMOV Wd, Hn`; `fp_to_gpr=0` -> `FMOV Hd, Wn`. */
size_t
aarch64_enc_fmov_h_gpr(AsmEnc *e, int fp_to_gpr, A64Reg rd, A64Reg rn)
{
    uint32_t opc = fp_to_gpr ? 6u : 7u;
    uint32_t w = (0x1Eu << 24) /* bits 28..24 = 11110 */ |
                 (3u << 22) /* type = 11 (half) */       |
                 (1u << 21)                              |
                 (opc << 16)                             |
                 (((uint32_t)rn & 0x1F) << 5)            |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

size_t
aarch64_enc_fmov_imm(AsmEnc *e, int is_double, A64Reg rd, uint8_t imm8)
{
    /* 0 0 0 11110 type 1 imm8 100 00000 Rd
     *   type=00 (S) -> base 0x1E201000
     *   type=01 (D) -> base 0x1E601000
     */
    uint32_t base = is_double ? 0x1E601000u : 0x1E201000u;
    uint32_t w = base                     |
                 (((uint32_t)imm8) << 13) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* SIMD/FP load+store (unsigned offset).  Encoding:
 *   [size:2][111][V=1][01][opc:2][imm12:12][Rn:5][Rt:5]
 *
 * size/opc per access width:
 *   B  load  : size=00 opc=01    B  store : size=00 opc=00
 *   H  load  : size=01 opc=01    H  store : size=01 opc=00
 *   S  load  : size=10 opc=01    S  store : size=10 opc=00
 *   D  load  : size=11 opc=01    D  store : size=11 opc=00
 *   Q  load  : size=00 opc=11    Q  store : size=00 opc=10
 *
 * imm12 is the byte offset divided by the access width (scaled). */
size_t
aarch64_enc_fp_ldst_imm(AsmEnc *e,
                        int is_load,
                        int width,
                        A64Reg rt,
                        A64Reg rn,
                        uint32_t off)
{
    uint32_t size, opc, scale;
    switch (width) {
        case 1: size = 0; opc = is_load ? 1u : 0u; scale = 1; break;
        case 2: size = 1; opc = is_load ? 1u : 0u; scale = 2; break;
        case 4: size = 2; opc = is_load ? 1u : 0u; scale = 4; break;
        case 8: size = 3; opc = is_load ? 1u : 0u; scale = 8; break;
        case 16: size = 0; opc = is_load ? 3u : 2u; scale = 16; break;
        default: return 0;
    }
    if (off % scale != 0) return 0; /* offset must be aligned */
    uint32_t pimm12 = (off / scale) & 0xFFFu;
    uint32_t w = (size << 30)                              |
                 (0x7u << 27) /* bits 29:27 = 111 */       |
                 (1u << 26)   /* V bit */                  |
                 (1u << 24)   /* bit 24 = 1; bit 25 = 0 */ |
                 (opc << 22)                               |
                 (pimm12 << 10)                            |
                 (((uint32_t)rn & 0x1F) << 5)              |
                 ((uint32_t)rt & 0x1F);
    return put_word(e, w);
}

/* LDP/STP (pair, 64-bit). Index modes:
 *   pre-indexed  : opc=10 | 1 0 1 | V=0 | 1 0 0 | L | imm7 | Rt2 | Rn | Rt
 *   post-indexed : opc=10 | 1 0 1 | V=0 | 0 0 1 | L | imm7 | Rt2 | Rn | Rt
 *   signed-off   : opc=10 | 1 0 1 | V=0 | 0 1 0 | L | imm7 | Rt2 | Rn | Rt
 * imm7 is a signed multiple of 8 (so we divide bytes by 8 here). L=1 is LDP. */
static uint32_t
aarch64_ldp_stp(uint32_t pat,
                uint32_t L,
                A64Reg rt1,
                A64Reg rt2,
                A64Reg rn,
                int32_t simm)
{
    int32_t imm7 = (simm / 8) & 0x7F;
    return (0x2u << 30)    /* opc=10 */             |
           (0x5u << 27) /* 0b101 */                 |
           (pat << 23)  /* index-mode bits 24:23 */ |
           (L << 22)                                |
           (((uint32_t)imm7 & 0x7F) << 15)          |
           ((uint32_t)rt2 << 10)                    |
           ((uint32_t)rn << 5)                      |
           ((uint32_t)rt1 & 0x1F);
}

size_t
aarch64_enc_stp_pre(AsmEnc *e, A64Reg rt1, A64Reg rt2, A64Reg rn, int32_t simm)
{
    return put_word(e, aarch64_ldp_stp(0x3, 0, rt1, rt2, rn, simm));
}

size_t
aarch64_enc_ldp_post(AsmEnc *e, A64Reg rt1, A64Reg rt2, A64Reg rn, int32_t simm)
{
    return put_word(e, aarch64_ldp_stp(0x1, 1, rt1, rt2, rn, simm));
}

/* ================================================================ control flow
 */

/* RET Xn: 1101 0110 0101 1111 0000 00 | Rn(5) | 00000 - default Rn=X30. */
size_t
aarch64_enc_ret_reg(AsmEnc *e, A64Reg rn)
{
    uint32_t w = 0xD65F0000u | ((uint32_t)rn << 5);
    return put_word(e, w);
}

size_t
aarch64_enc_ret(AsmEnc *e)
{
    return aarch64_enc_ret_reg(e, A_LR);
}

/* B / BL (unconditional immediate):
 *   op | 0 0 1 0 1 | imm26 - op=0 for B, op=1 for BL. */
size_t
aarch64_enc_b(AsmEnc *e, int32_t rel_words)
{
    uint32_t imm26 = (uint32_t)rel_words & 0x03FFFFFFu;
    return put_word(e, (0x5u << 26) | imm26);
}

size_t
aarch64_enc_bl(AsmEnc *e, int32_t rel_words)
{
    uint32_t imm26 = (uint32_t)rel_words & 0x03FFFFFFu;
    return put_word(e, (1u << 31) | (0x5u << 26) | imm26);
}

/* BR / BLR Xn: 1101 0110 00X1 1111 0000 00 | Rn | 00000 - BR has X=0, BLR has
 * X=1. */
size_t
aarch64_enc_br(AsmEnc *e, A64Reg rn)
{
    return put_word(e, 0xD61F0000u | ((uint32_t)rn << 5));
}

size_t
aarch64_enc_blr(AsmEnc *e, A64Reg rn)
{
    return put_word(e, 0xD63F0000u | ((uint32_t)rn << 5));
}

/* B.cond: 0101 0100 | imm19 | 0 | cond(4). */
size_t
aarch64_enc_b_cond(AsmEnc *e, A64Cond cc, int32_t rel_words)
{
    uint32_t imm19 = (uint32_t)rel_words & 0x0007FFFFu;
    return put_word(e, (0x54u << 24) | (imm19 << 5) | ((uint32_t)cc & 0xF));
}

/* ADRP Xd, #(pages*4096):
 *   op=1 | immlo(2) | 1 0 0 0 0 | immhi(19) | Rd(5)
 * `pages` is a signed 21-bit count of 4 KiB pages. */
size_t
aarch64_enc_adrp(AsmEnc *e, A64Reg rd, int32_t pages)
{
    uint32_t imm21 = (uint32_t)pages & 0x001FFFFFu;
    uint32_t immlo = imm21 & 0x3;
    uint32_t immhi = (imm21 >> 2) & 0x7FFFFu;
    uint32_t w = (1u << 31) |
                 (immlo << 29) |
                 (0x10u << 24) |
                 (immhi << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* ================================================================ patchers */

static uint32_t
aarch64_read_word(AsmEnc *e, size_t off)
{
    const uint8_t *p = e->bytes + off;
    return (uint32_t)p[0]         |
           ((uint32_t)p[1] << 8)  |
           ((uint32_t)p[2] << 16) |
           ((uint32_t)p[3] << 24);
}

static void
aarch64_write_word(AsmEnc *e, size_t off, uint32_t w)
{
    uint8_t *p = e->bytes + off;
    p[0] = (uint8_t)(w);
    p[1] = (uint8_t)(w >> 8);
    p[2] = (uint8_t)(w >> 16);
    p[3] = (uint8_t)(w >> 24);
}

/* Patch a branch now that we know where we are going */
void
aarch64_enc_patch_branch26(AsmEnc *e, size_t patch, int32_t rel_words)
{
    uint32_t w = aarch64_read_word(e, patch) & 0xFC000000u;
    w |= (uint32_t)rel_words & 0x03FFFFFFu;
    aarch64_write_word(e, patch, w);
}

void
aarch64_enc_patch_adrp_imm21(AsmEnc *e, size_t patch, int32_t page_delta)
{
    uint32_t imm21 = (uint32_t)page_delta & 0x001FFFFFu;
    uint32_t immlo = imm21 & 0x3;
    uint32_t immhi = (imm21 >> 2) & 0x7FFFFu;
    /* Clear bits 30:29 (immlo) and 23:5 (immhi), then OR in. */
    uint32_t w = aarch64_read_word(e, patch) & ~((0x3u << 29) | (0x7FFFFu << 5));
    w |= (immlo << 29) | (immhi << 5);
    aarch64_write_word(e, patch, w);
}

void
aarch64_enc_patch_imm12(AsmEnc *e, size_t patch, uint32_t imm12)
{
    /* Clears bits 21:10 then ORs in imm12. Works for ADD-imm and LDR-uimm. */
    uint32_t w = aarch64_read_word(e, patch) & ~(0xFFFu << 10);
    w |= (imm12 & 0xFFFu) << 10;
    aarch64_write_word(e, patch, w);
}

void
aarch64_enc_patch_imm19(AsmEnc *e, size_t patch, int32_t rel_words)
{
    /* For B.cond and CBZ/CBNZ: imm19 sits in bits 23:5. */
    uint32_t w = aarch64_read_word(e, patch) & ~(0x7FFFFu << 5);
    w |= ((uint32_t)rel_words & 0x7FFFFu) << 5;
    aarch64_write_word(e, patch, w);
}

/* ================================================================ sub-int
 * loads/stores */

/* LDR/STR (immediate, unsigned offset) for byte/halfword/word.
 * Encoding (size=8/16/32-bit):
 *   size(2) | 1 1 1 0 0 1 | opc(2) | imm12 | Rn | Rt
 * size: 00=byte, 01=halfword, 10=word.
 * opc:  00=STR, 01=LDR(zero-ext). */
static uint32_t
ldst_sub(uint32_t size, uint32_t opc, A64Reg rt, A64Reg rn, uint32_t off)
{
    uint32_t scale = (uint32_t)(1u << size);
    uint32_t pimm12 = (off / scale) & 0xFFFu;
    return (size << 30)                                 |
           (0x39u << 24) /* 0b00111001 -> bits 29:24 */ |
           (opc << 22)                                  |
           (pimm12 << 10)                               |
           ((uint32_t)rn << 5)                          |
           ((uint32_t)rt & 0x1F);
}

size_t
aarch64_enc_ldrb_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off)
{
    return put_word(e, ldst_sub(0, 0x1, rt, rn, off));
}

size_t
aarch64_enc_strb_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off)
{
    return put_word(e, ldst_sub(0, 0x0, rt, rn, off));
}

    size_t
aarch64_enc_ldrh_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off)
{
    return put_word(e, ldst_sub(1, 0x1, rt, rn, off));
}

size_t
aarch64_enc_strh_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off)
{
    return put_word(e, ldst_sub(1, 0x0, rt, rn, off));
}

size_t
aarch64_enc_ldr32_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off)
{
    return put_word(e, ldst_sub(2, 0x1, rt, rn, off));
}

size_t
aarch64_enc_str32_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off)
{
    return put_word(e, ldst_sub(2, 0x0, rt, rn, off));
}

/* Signed loads. opc=10 sign-extends to X (64-bit dst), opc=11 to W (32-bit).
 * LDRSW only has an X-destination form. */
size_t
aarch64_enc_ldrsb_imm(AsmEnc *e, int dst_64, A64Reg rt, A64Reg rn, uint32_t off)
{
    return put_word(e, ldst_sub(0, dst_64 ? 0x2u : 0x3u, rt, rn, off));
}

size_t
aarch64_enc_ldrsh_imm(AsmEnc *e, int dst_64, A64Reg rt, A64Reg rn, uint32_t off)
{
    return put_word(e, ldst_sub(1, dst_64 ? 0x2u : 0x3u, rt, rn, off));
}

size_t
aarch64_enc_ldrsw_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off)
{
    return put_word(e, ldst_sub(2, 0x2u, rt, rn, off));
}

/* Signed-load imm9 (pre/post/unscaled). op2: 0=unscaled, 1=post-idx, 3=pre-idx. */
static size_t
aarch64_ldrs_imm9_emit(AsmEnc *e, int width, int dst_64, uint32_t op2,
                       A64Reg rt, A64Reg rn, int32_t simm9)
{
    uint32_t size = (width == 1) ? 0u : (width == 2) ? 1u : 2u;
    uint32_t opc = dst_64 ? 0x2u : 0x3u;
    uint32_t imm9 = ((uint32_t)simm9) & 0x1FFu;
    uint32_t w = (size << 30) |
                 (0x7u << 27) |
                 (opc << 22) |
                 (imm9 << 12) |
                 (op2 << 10) |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rt & 0x1F);
    return put_word(e, w);
}

/* Signed-load reg-offset. Same encoding as LDR (reg) but with opc=10/11. */
static size_t
aarch64_ldrs_regoff_emit(AsmEnc *e, int width, int dst_64, int scaled,
                         A64Reg rt, A64Reg rn, A64Reg rm)
{
    uint32_t size = (width == 1) ? 0u : (width == 2) ? 1u : 2u;
    uint32_t opc = dst_64 ? 0x2u : 0x3u;
    uint32_t S = scaled ? 1u : 0u;
    uint32_t option = 0x3u; /* UXTX/LSL */
    uint32_t w = (size << 30) |
                 (0x7u << 27) |
                 (opc << 22) |
                 (1u << 21) |
                 (((uint32_t)rm & 0x1F) << 16) |
                 (option << 13) |
                 (S << 12) |
                 (0x2u << 10) |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rt & 0x1F);
    return put_word(e, w);
}

/* LDUR/STUR (immediate, signed 9-bit offset, unscaled):
 *   size(2) | 1 1 1 0 0 0 | opc(2) | 0 | imm9(9) | 0 0 | Rn | Rt */
static uint32_t
ldur_stur(uint32_t size, uint32_t opc, A64Reg rt, A64Reg rn, int32_t simm9)
{
    uint32_t imm9 = (uint32_t)simm9 & 0x1FFu;
    return (size << 30) |
           (0x38u << 24) |
           (opc << 22) |
           (imm9 << 12) |
           ((uint32_t)rn << 5) |
           ((uint32_t)rt & 0x1F);
}

static uint32_t
aarch64_size_for_width(int width)
{
    switch (width) {
        case 1: return 0;
        case 2: return 1;
        case 4: return 2;
        default: return 3; /* 8 */
    }
}

size_t
aarch64_enc_ldur(AsmEnc *e, int width, A64Reg rt, A64Reg rn, int32_t simm9)
{
    return put_word(e, ldur_stur(aarch64_size_for_width(width), 0x1, rt, rn, simm9));
}

size_t
aarch64_enc_stur(AsmEnc *e, int width, A64Reg rt, A64Reg rn, int32_t simm9)
{
    return put_word(e, ldur_stur(aarch64_size_for_width(width), 0x0, rt, rn, simm9));
}

/* ================================================================ logical /
 * shifts */

/* Logical (shifted register), shift=LSL #0, N=0:
 *   sf | opc(2) | 0 1 0 1 0 | shift=00 | N=0 | Rm | imm6=0 | Rn | Rd
 * opc: 00=AND, 01=ORR, 10=EOR. */
static uint32_t
aarch64_log_reg(int sf, uint32_t opc, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return (((uint32_t)sf & 1u) << 31) |
           (opc << 29) |
           (0x0Au << 24) |
           ((uint32_t)rm << 16) |
           ((uint32_t)rn << 5) |
           ((uint32_t)rd & 0x1F);
}

size_t
aarch64_enc_and_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return put_word(e, aarch64_log_reg(is_64, 0x0, rd, rn, rm));
}

size_t
aarch64_enc_orr_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return put_word(e, aarch64_log_reg(is_64, 0x1, rd, rn, rm));
}

size_t
aarch64_enc_eor_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return put_word(e, aarch64_log_reg(is_64, 0x2, rd, rn, rm));
}

/* Data-processing 3-source: MADD/MSUB.
 *   sf | 0 0 | 1 1 0 1 1 | op31(3) | Rm | o0 | Ra | Rn | Rd
 * MUL  = MADD with Ra=XZR.       op31=000, o0=0
 * MSUB = MSUB.                   op31=000, o0=1 */
static uint32_t
dp3(int sf, uint32_t op31, uint32_t o0, A64Reg rd, A64Reg rn, A64Reg rm, A64Reg ra)
{
    return (((uint32_t)sf & 1u) << 31) |
           (0x1Bu << 24) |
           (op31 << 21) |
           ((uint32_t)rm << 16) |
           (o0 << 15) |
           ((uint32_t)ra << 10) |
           ((uint32_t)rn << 5) |
           ((uint32_t)rd & 0x1F);
}

size_t
aarch64_enc_mul(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return put_word(e, dp3(is_64, 0, 0, rd, rn, rm, A_XZR));
}

size_t
aarch64_enc_msub(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm, A64Reg ra)
{
    return put_word(e, dp3(is_64, 0, 1, rd, rn, rm, ra));
}

/* Data-processing 2-source: SDIV / UDIV / variable shifts.
 *   sf | 0 | S=0 | 1 1 0 1 0 1 1 0 | Rm | op2(6) | Rn | Rd */
static uint32_t
aarch64_dp2(int sf, uint32_t op2, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return (((uint32_t)sf & 1u) << 31) |
           (0x0D6u << 21) |
           ((uint32_t)rm << 16) |
           (op2 << 10) |
           ((uint32_t)rn << 5) |
           ((uint32_t)rd & 0x1F);
}

size_t
aarch64_enc_sdiv(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return put_word(e, aarch64_dp2(is_64, 0x03, rd, rn, rm));
}

size_t
aarch64_enc_udiv(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return put_word(e, aarch64_dp2(is_64, 0x02, rd, rn, rm));
}

size_t
aarch64_enc_lslv(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return put_word(e, aarch64_dp2(is_64, 0x08, rd, rn, rm));
}

/* LSL (immediate) - UBFM alias.  `LSL <Rd>, <Rn>, #shift` =
 *   UBFM <Rd>, <Rn>, #(-shift mod regsize), #(regsize - 1 - shift). */
size_t
aarch64_enc_lsl_imm(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, uint32_t shift)
{
    uint32_t regsize = is_64 ? 64u : 32u;
    if (shift >= regsize) return 0;
    uint32_t immr = (regsize - shift) & (regsize - 1u);
    uint32_t imms = regsize - 1u - shift;
    uint32_t base = is_64 ? 0xD3400000u : 0x53000000u;
    uint32_t w = base |
                 ((immr & 0x3F) << 16) |
                 ((imms & 0x3F) << 10) |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* LSR (immediate) - UBFM alias: UBFM <Rd>, <Rn>, #shift, #(regsize - 1). */
size_t
aarch64_enc_lsr_imm(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, uint32_t shift)
{
    uint32_t regsize = is_64 ? 64u : 32u;
    if (shift >= regsize) return 0;
    uint32_t base = is_64 ? 0xD3400000u : 0x53000000u;
    uint32_t w = base |
                 ((shift & 0x3F) << 16) |
                 ((regsize - 1u) << 10) |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* ASR (immediate) - SBFM alias: SBFM <Rd>, <Rn>, #shift, #(regsize - 1). */
size_t
aarch64_enc_asr_imm(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, uint32_t shift)
{
    uint32_t regsize = is_64 ? 64u : 32u;
    if (shift >= regsize) return 0;
    uint32_t base = is_64 ? 0x93400000u : 0x13000000u;
    uint32_t w = base |
                 ((shift & 0x3F) << 16) |
                 ((regsize - 1u) << 10) |
                 (((uint32_t)rn & 0x1F) << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

size_t
aarch64_enc_lsrv(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return put_word(e, aarch64_dp2(is_64, 0x09, rd, rn, rm));
}

size_t
aarch64_enc_asrv(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm)
{
    return put_word(e, aarch64_dp2(is_64, 0x0A, rd, rn, rm));
}

size_t
aarch64_enc_neg_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rm)
{
    /* NEG Rd, Rm = SUB Rd, ZR, Rm (shifted-register form). */
    return aarch64_enc_sub_reg(e, is_64, rd, A_XZR, rm);
}

size_t
aarch64_enc_mvn_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rm)
{
    /* MVN Rd, Rm = ORN Rd, ZR, Rm. ORN is logical-shifted-register with N=1.
     * Bits: sf | opc=01(ORR) | 01010 | shift=00 | N=1 | Rm | imm6 | Rn | Rd */
    uint32_t w = (((uint32_t)is_64 & 1u) << 31) |
                 (0x1u << 29) |
                 (0x0Au << 24) |
                 (1u << 21) |
                 ((uint32_t)rm << 16) |
                 (0x1Fu << 5) |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* ================================================================ compare /
 * cset */

size_t
aarch64_enc_cmp_reg(AsmEnc *e, int is_64, A64Reg rn, A64Reg rm)
{
    /* SUBS ZR, Rn, Rm - like SUB but S=1 and Rd=ZR. */
    uint32_t w = (((uint32_t)is_64 & 1u) << 31) |
                 (1u << 30) |
                 (1u << 29) |
                 (0x0Bu << 24) |
                 ((uint32_t)rm << 16) |
                 ((uint32_t)rn << 5) |
                 0x1Fu;
    return put_word(e, w);
}

size_t
aarch64_enc_cmp_imm(AsmEnc *e, int is_64, A64Reg rn, uint32_t imm)
{
    /* SUBS ZR, Rn, #imm - op=1, S=1, fixed 100010. The 12-bit immediate may
     * be optionally shifted left by 12 (sh bit), exactly like ADD/SUB - so a
     * value such as 0x100000 (0x100 << 12) encodes with sh=1 rather than
     * being truncated to its (zero) low 12 bits. */
    uint32_t sh = 0;
    uint32_t imm12 = imm;
    if (imm > 0xFFFu && (imm & 0xFFFu) == 0 && (imm >> 12) <= 0xFFFu) {
        sh = 1;
        imm12 = imm >> 12;
    }
    uint32_t w = (((uint32_t)is_64 & 1u) << 31) |
                 (1u << 30) |
                 (1u << 29) |
                 (0x22u << 23) |
                 (sh << 22) |
                 ((imm12 & 0xFFFu) << 10) |
                 ((uint32_t)rn << 5) |
                 0x1Fu;
    return put_word(e, w);
}

size_t
aarch64_enc_tst_reg(AsmEnc *e, int is_64, A64Reg rn, A64Reg rm)
{
    /* ANDS ZR, Rn, Rm - logical shifted reg, opc=11, S implicit via Rd=ZR. */
    uint32_t w = (((uint32_t)is_64 & 1u) << 31) |
                 (0x3u << 29) |
                 (0x0Au << 24) |
                 ((uint32_t)rm << 16) |
                 ((uint32_t)rn << 5) |
                 0x1Fu;
    return put_word(e, w);
}

size_t
aarch64_enc_cset(AsmEnc *e, A64Reg rd, A64Cond cc)
{
    /* CSET Xd, cc = CSINC Xd, XZR, XZR, !cc.
     * CSINC encoding: sf=1 | 0 | 0 | 1 1 0 1 0 1 0 0 | Rm | cond | 0 1 | Rn |
     * Rd */
    uint32_t inv = (uint32_t)cc ^ 1u;                       /* invert low bit */
    uint32_t w = (1u << 31) |
                 (0xD4u << 21) |
                 (0x1Fu << 16) /* Rm = XZR */ |
                 ((inv & 0xF) << 12) |
                 (0x1u << 10) |
                 (0x1Fu << 5) /* Rn = XZR */ |
                 ((uint32_t)rd & 0x1F);
    return put_word(e, w);
}

/* (CBZ/CBNZ encoder lives further up - `aarch64_enc_cbz` takes is_64 +
 * is_nonzero, supporting both X and W variants.  The older X-only stub
 * was removed.) */

/* ================================================================ system */

/* DMB/DSB/ISB share the encoding
 *   1101 0101 0000 0011 0011 CRm:4 1 opc2:3 11111
 * where opc2 picks the barrier kind:
 *   DSB = 0b100 (4), DMB = 0b101 (5), ISB = 0b110 (6).
 * CRm is the option (SY/ISH/...); ISB uses CRm=0xF (SY) by convention. */
size_t
_aarch64_enc_barrier(AsmEnc *e, int opc2, uint32_t crm)
{
    uint32_t w = 0xD5033000u |
                 ((crm & 0xFu) << 8) |
                 (((uint32_t)opc2 & 0x7u) << 5) |
                 0x1Fu;
    return put_word(e, w);
}

/* MRS  Xt, sysreg  : 1101 0101 0011 o0 op1 CRn CRm op2 Rt  (L=1)
 * MSR  sysreg, Xt  : 1101 0101 0001 o0 op1 CRn CRm op2 Rt  (L=0)
 *
 * sysreg16 is the 16-bit field packed as o0[1]|op1[3]|CRn[4]|CRm[4]|op2[3].
 * In the instruction, the encoded `o0` bit is the low bit of op0 (i.e.
 * `op0 & 1`), since op0 ∈ {2,3} for sysregs. */
size_t
_aarch64_enc_mrs(AsmEnc *e, A64Reg rt, uint16_t sysreg16)
{
    uint32_t w = 0xD5300000u |
                 (((uint32_t)sysreg16 & 0xFFFFu) << 5) |
                 ((uint32_t)rt & 0x1Fu);
    return put_word(e, w);
}

size_t
aarch64_enc_msr_reg(AsmEnc *e, uint16_t sysreg16, A64Reg rt)
{
    uint32_t w = 0xD5100000u |
                (((uint32_t)sysreg16 & 0xFFFFu) << 5) |
                ((uint32_t)rt & 0x1Fu);
    return put_word(e, w);
}

/* MSR (immediate) - writes a PSTATE field.  Encoding:
 *   1101 0101 0000 0 op1:3 0100 CRm:4 op2:3 11111
 * op1+op2 identify the PSTATE field; CRm is the 4-bit immediate. */
size_t
aarch64_enc_msr_imm(AsmEnc *e, uint32_t op1, uint32_t op2, uint32_t imm4)
{
    uint32_t w = 0xD500401Fu |
                 ((op1 & 0x7u) << 16) |
                 ((imm4 & 0xFu) << 8) |
                 ((op2 & 0x7u) << 5);
    return put_word(e, w);
}

/* ---------- END Instruction encoders - */

/* ---------- operand helpers ---------- */

/* True for any SIMD/FP register (B/H/S/D/Q/V). */
static int
aarch64_is_simd_reg(AsmOperand *o)
{
    if (!asm_is_reg(o)) return 0;
    AsmRegClass c = o->reg.cls;
    return c == AR_A64_B ||
           c == AR_A64_H ||
           c == AR_A64_S ||
           c == AR_A64_D ||
           c == AR_A64_Q ||
           c == AR_A64_V;
}

/* Width of a scalar SIMD/FP register in bytes (1/2/4/8/16); 0 for V (V is
 * vector-form only - caller must look at v_lane_bits/v_n_lanes). */
static int
aarch64_simd_scalar_width(AsmReg r)
{
    switch (r.cls) {
        case AR_A64_B: return 1;
        case AR_A64_H: return 2;
        case AR_A64_S: return 4;
        case AR_A64_D: return 8;
        case AR_A64_Q: return 16;
        default: return 0;
    }
}

/* Translate an A64 register operand to its 0..31 hardware number. */
static int
aarch64_reg_num(AsmReg r)
{
    return r.num & 0x1F;
}

static int
aarch64_is_x_or_sp(AsmReg r)
{
    return r.cls == AR_A64_X ||
           r.cls == AR_A64_SP;
}

/* ---------- condition-code parsing for B.<cc> ---------- */

static int
aarch64_parse_cond_suffix(const char *mn, A64Cond *out)
{
    /* Mnemonic comes in lowercase; we accept `b.eq`, `b.ne`, etc. */
    if (strncmp(mn, "b.", 2) != 0) return 0;
    const char *cc = mn + 2;
    if (!strcmp(cc, "eq")) *out = A_EQ;
    else if (!strcmp(cc, "ne")) *out = A_NE;
    else if (!strcmp(cc, "cs") || !strcmp(cc, "hs")) *out = A_CS;
    else if (!strcmp(cc, "cc") || !strcmp(cc, "lo")) *out = A_CC;
    else if (!strcmp(cc, "mi")) *out = A_MI;
    else if (!strcmp(cc, "pl")) *out = A_PL;
    else if (!strcmp(cc, "vs")) *out = A_VS;
    else if (!strcmp(cc, "vc")) *out = A_VC;
    else if (!strcmp(cc, "hi")) *out = A_HI;
    else if (!strcmp(cc, "ls")) *out = A_LS;
    else if (!strcmp(cc, "ge")) *out = A_GE;
    else if (!strcmp(cc, "lt")) *out = A_LT;
    else if (!strcmp(cc, "gt")) *out = A_GT;
    else if (!strcmp(cc, "le")) *out = A_LE;
    else if (!strcmp(cc, "al")) *out = A_AL;
    else return 0;
    return 1;
}

/* ---------- branch helpers ---------- */

/* Emit a B/BL to a local label, adding a BRANCH26 fixup. */
static void
aarch64_emit_branch_local(AsmEnc *e, int local_num, int is_bl)
{
    size_t patch = e->len;
    if (is_bl) aarch64_enc_bl(e, 0);
    else aarch64_enc_b(e, 0);
    AsmFixup f = { 0 };
    f.kind = AF_LOCAL;
    f.local_num = local_num;
    f.patch_offset = patch;
    f.width = 4;
    f.pcrel = 1;
    asm_add_fixup(e, f);
}

/* Emit a B.cond to a local label, adding an IMM19 fixup. We tag fixups for
 * 19-bit displacements via width=19 so the resolver picks the right patcher
 * later (BRANCH26 uses width=4 which we treat as the 26-bit variant). */
static void
aarch64_emit_b_cond_local(AsmEnc *e, A64Cond cc, int local_num)
{
    size_t patch = e->len;
    aarch64_enc_b_cond(e, cc, 0);
    AsmFixup f = { 0 };
    f.kind = AF_LOCAL;
    f.local_num = local_num;
    f.patch_offset = patch;
    f.width = 19; /* sentinel: B.cond/CBZ imm19, not BRANCH26 */
    f.pcrel = 1;
    asm_add_fixup(e, f);
}

/* Emit a BL <symbol> targeting an external name. The resolver/codegen
 * layer turns this into a Mach-O BRANCH26 relocation. */
static void
aarch64_emit_branch_sym(AsmEnc *e, const char *sym, int is_bl)
{
    size_t patch = e->len;
    if (is_bl) aarch64_enc_bl(e, 0);
    else aarch64_enc_b(e, 0);
    AsmFixup f = { 0 };
    f.kind = AF_SYMBOL;
    f.reloc = is_bl ? AFR_AARCH64_CALL26 : AFR_AARCH64_JUMP26;
    f.sym = xstrdup(sym);
    f.patch_offset = patch;
    f.width = 4;
    f.pcrel = 1;
    asm_add_fixup(e, f);
}

/* ---------- mnemonic dispatch ---------- */

void
aarch64_enc_mov(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "MOV needs 2 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    AsmOperand *s = &ln->operands[1];

    int dst_x = asm_is_reg(d) && aarch64_is_x_or_sp(d->reg);
    int dst_w = asm_is_reg(d) && d->reg.cls == AR_A64_W;
    if (!dst_x && !dst_w) {
        asm_err_at(e, ln, "MOV: bad dst");
        return;
    }
    int rd = aarch64_reg_num(d->reg);

    if (asm_is_imm(s)) {
        if (dst_w) {
            /* 32-bit MOVZ for now; only validates the low 32 bits. */
            aarch64_enc_mov_imm32(e, rd, (uint64_t)(uint32_t)s->imm);
        } else {
            aarch64_enc_mov_imm64(e, rd, (uint64_t)s->imm);
        }
        return;
    }
    if (asm_is_reg(s)) {
        if (dst_x && aarch64_is_x_or_sp(s->reg)) {
            int rm = aarch64_reg_num(s->reg);
            /* "mov reg, sp" / "mov sp, reg" must encode as ADD #0 because
             * the ORR-form encodes Rn=31 as XZR (not SP). */
            if (d->reg.cls == AR_A64_SP || s->reg.cls == AR_A64_SP)
                aarch64_enc_add_imm(e, 1, rd, rm, 0);
            else aarch64_enc_mov_reg(e, rd, rm);
            return;
        }
        if (dst_w && s->reg.cls == AR_A64_W) {
            aarch64_enc_mov_reg_w(e, rd, aarch64_reg_num(s->reg));
            return;
        }
    }
    asm_err_at(e, ln, "MOV: unsupported operand combo");
}

/* Pull an optional 4th `LSL/LSR/ASR #N` operand off the line.  Writes
 * *out_type and *out_amount; returns 1 if a valid shift was found, 0
 * if there is no 4th operand (no error), -1 on a bad 4th operand. */
static int
aarch64_extract_shift_suffix(AsmEnc *e,
                             AsmLine *ln,
                             int *_out_type,
                             int *_out_amount)
{
    *_out_type = 0;
    *_out_amount = 0;
    if (ln->n_operands < 4) return 0;
    if (ln->n_operands > 4) {
        asm_err_at(e, ln, "too many operands");
        return -1;
    }
    AsmOperand *s = &ln->operands[3];
    if (s->kind != AOP_SHIFT) {
        asm_err_at(e, ln, "4th operand must be LSL/LSR/ASR #N");
        return -1;
    }
    if (s->imm < 0 || s->imm > 63) {
        asm_err_at(e, ln, "shift amount must be 0..63");
        return -1;
    }
    *_out_type = s->shift_type;
    *_out_amount = (int)s->imm;
    return 1;
}

void
aarch64_enc_add_sub(AsmEnc *e, AsmLine *ln, int is_sub)
{
    if (ln->n_operands < 3 || ln->n_operands > 4) {
        asm_err_at(e, ln, "ADD/SUB needs 3 or 4 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0], *n = &ln->operands[1],
               *m = &ln->operands[2];
    if (!asm_is_reg(d) || !asm_is_reg(n)) {
        asm_err_at(e, ln, "ADD/SUB: dst/lhs must be reg");
        return;
    }
    int is_64 = aarch64_is_x_or_sp(d->reg);
    int rd = aarch64_reg_num(d->reg), rn = aarch64_reg_num(n->reg);
    if (asm_is_imm(m)) {
        if (ln->n_operands == 4) {
            asm_err_at(e, ln, "ADD/SUB: shift suffix not valid with immediate");
            return;
        }
        if (m->imm < 0 || m->imm > 0xFFF) {
            asm_err_at(e, ln, "ADD/SUB imm out of 12-bit range");
            return;
        }
        if (is_sub) aarch64_enc_sub_imm(e, is_64, rd, rn, (uint32_t)m->imm);
        else aarch64_enc_add_imm(e, is_64, rd, rn, (uint32_t)m->imm);
        return;
    }
    /* `add Xd, Xn, _sym` is the second half of an ADRP+ADD pair that
     * loads the address of `_sym`. We emit ADD with imm12=0 and record a
     * PAGEOFF12 fixup so the linker can patch the bottom 12 bits at
     * link time. SUB doesn't have an equivalent pairing. */
    if (asm_is_label(m) && !is_sub) {
        if (ln->n_operands == 4) {
            asm_err_at(e, ln, "ADD: shift suffix not valid with symbol");
            return;
        }
        aarch64_enc_add_imm(e, is_64, rd, rn, 0);
        AsmFixup f = { 0 };
        int local_n = asm_operand_local_num(m);
        if (local_n >= 0) {
            f.kind = AF_LOCAL;
            f.local_num = local_n;
        } else {
            f.kind = AF_SYMBOL;
            f.sym = xstrdup(m->label_name);
        }
        /* Fixup patches the imm12 field of the just-emitted ADD - the
         * instruction sits at e->len - 4. */
        f.patch_offset = e->len - 4;
        f.width = 4;
        f.pcrel = 0;
        f.reloc = AFR_AARCH64_PAGEOFF12;
        asm_add_fixup(e, f);
        return;
    }
    if (asm_is_reg(m)) {
        int rm = aarch64_reg_num(m->reg);
        int st = 0, sa = 0;
        int rc = aarch64_extract_shift_suffix(e, ln, &st, &sa);
        if (rc < 0) return;
        if (st == ASHIFT_ROR) {
            asm_err_at(e, ln, "ADD/SUB: ROR shift not valid");
            return;
        }
        if (is_sub) aarch64_enc_sub_reg_shifted(e, is_64, rd, rn, rm, st, sa);
        else aarch64_enc_add_reg_shifted(e, is_64, rd, rn, rm, st, sa);
        return;
    }
    asm_err_at(e, ln, "ADD/SUB: unsupported rhs");
}

void
aarch64_enc_logical_reg(AsmEnc *e,
                        AsmLine *ln,
                        size_t (*emit_cb)(AsmEnc *, int, A64Reg, A64Reg, A64Reg),
                        const char *msg)
{
    if (ln->n_operands != 3) {
        asm_err_at(e, ln, msg);
        return;
    }
    AsmOperand *d = &ln->operands[0], *n = &ln->operands[1],
               *m = &ln->operands[2];
    if (!asm_is_reg(d) || !asm_is_reg(n) || !asm_is_reg(m)) {
        asm_err_at(e, ln, msg);
        return;
    }
    int is_64 = (d->reg.cls == AR_A64_X);
    emit_cb(e, is_64,
               aarch64_reg_num(d->reg),
               aarch64_reg_num(n->reg),
               aarch64_reg_num(m->reg));
}

void
aarch64_enc_mulm(AsmEnc *e, AsmLine *ln)
{
    aarch64_enc_logical_reg(e, ln, aarch64_enc_mul, "MUL needs 3 reg operands");
}

void
aarch64_enc_sdi(AsmEnc *e, AsmLine *ln)
{
    aarch64_enc_logical_reg(e, ln, aarch64_enc_sdiv, "SDIV needs 3 reg operands");
}

/* LSL <Rd>, <Rn>, #imm    - UBFM alias (immediate shift)
 * LSL <Rd>, <Rn>, <Rm>    - LSLV (variable shift) */
void
aarch64_enc_lsl(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 3 || !asm_is_reg(&ln->operands[0]) ||
            !asm_is_reg(&ln->operands[1])) {
        asm_err_at(e, ln, "LSL: expected `Rd, Rn, #imm` or `Rd, Rn, Rm`");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    AsmOperand *n = &ln->operands[1];
    AsmOperand *m = &ln->operands[2];
    int is_64 = (d->reg.cls == AR_A64_X);
    if (asm_is_imm(m)) {
        int64_t sh = m->imm;
        uint32_t regsize = is_64 ? 64 : 32;
        if (sh < 0 || (uint64_t)sh >= regsize) {
            asm_err_at(e, ln, "LSL: shift amount out of range");
            return;
        }
        aarch64_enc_lsl_imm(e, is_64, aarch64_reg_num(d->reg), aarch64_reg_num(n->reg),
                (uint32_t)sh);
    } else if (asm_is_reg(m)) {
        aarch64_enc_lslv(e, is_64, aarch64_reg_num(d->reg), aarch64_reg_num(n->reg),
                aarch64_reg_num(m->reg));
    } else {
        asm_err_at(e, ln, "LSL: third operand must be reg or imm");
    }
}

void
aarch64_enc_neg(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 2 || !asm_is_reg(&ln->operands[0]) ||
            !asm_is_reg(&ln->operands[1])) {
        asm_err_at(e, ln, "NEG: need 2 reg operands");
        return;
    }
    int is_64 = (ln->operands[0].reg.cls == AR_A64_X);
    aarch64_enc_neg_reg(e, is_64, aarch64_reg_num(ln->operands[0].reg),
            aarch64_reg_num(ln->operands[1].reg));
}

void
aarch64_enc_cmp(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "CMP needs 2 operands");
        return;
    }
    AsmOperand *a = &ln->operands[0], *b = &ln->operands[1];
    if (!asm_is_reg(a)) {
        asm_err_at(e, ln, "CMP: lhs must be reg");
        return;
    }
    int is_64 = (a->reg.cls == AR_A64_X);
    if (asm_is_reg(b)) aarch64_enc_cmp_reg(e, is_64, aarch64_reg_num(a->reg), aarch64_reg_num(b->reg));
    else if (asm_is_imm(b) && b->imm >= 0 && b->imm <= 0xFFF)
        aarch64_enc_cmp_imm(e, is_64, aarch64_reg_num(a->reg), (uint32_t)b->imm);
    else asm_err_at(e, ln, "CMP: unsupported rhs");
}

/* LDR/STR (64-bit GPR). Now also handles pre-index / post-index and
 * register-offset forms in addition to the original unsigned-offset.
 *
 * Forms recognised:
 *   LDR Xt, [Xn]                    -> unsigned offset, imm=0
 *   LDR Xt, [Xn, #imm]              -> unsigned offset (scaled)
 *   LDR Xt, [Xn, #imm]!             -> pre-index    (writeback)
 *   LDR Xt, [Xn], #imm              -> post-index
 *   LDR Xt, [Xn, Xm]                -> register offset
 *   LDR Xt, [Xn, Xm, LSL #3]        -> register offset (scaled)
 */
void
aarch64_enc_ldst64(AsmEnc *e, AsmLine *ln, int is_load)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "LDR/STR needs 2 operands");
        return;
    }
    AsmOperand *r = &ln->operands[0], *m = &ln->operands[1];
    if (!asm_is_reg(r) || !asm_is_mem(m) || !m->has_base) {
        asm_err_at(e, ln, "LDR/STR: bad operands");
        return;
    }
    int rt = aarch64_reg_num(r->reg);
    int rn = aarch64_reg_num(m->base);
    int width = (r->reg.cls == AR_A64_W) ? 4 : 8;

    /* Register-offset: [Xn, Xm] or [Xn, Xm, LSL #N]. */
    if (m->has_index) {
        int scaled = 0;
        if (m->extend_lsl != 0) {
            if (m->extend_lsl != (width == 8 ? 3 : 2)) {
                asm_err_at(e, ln,
                        "LDR/STR reg-off: LSL must be #0 or natural scale");
                return;
            }
            scaled = 1;
        }
        aarch64_enc_ldst_regoff(e, 0, is_load, width, scaled, rt, rn,
                aarch64_reg_num(m->index));
        return;
    }
    /* Pre-index. */
    if (m->writeback) {
        if (m->disp < -256 || m->disp > 255) {
            asm_err_at(e, ln, "LDR/STR pre-index: imm9 out of range (-256..255)");
            return;
        }
        aarch64_enc_ldst_pre(e, 0, is_load, width, rt, rn, (int32_t)m->disp);
        return;
    }
    /* Post-index. */
    if (m->post_index) {
        if (m->disp < -256 || m->disp > 255) {
            asm_err_at(e, ln, "LDR/STR post-index: imm9 out of range (-256..255)");
            return;
        }
        aarch64_enc_ldst_post(e, 0, is_load, width, rt, rn, (int32_t)m->disp);
        return;
    }
    /* Default: prefer scaled imm12 ([Xn, #imm]); fall back to LDUR/STUR
     * (unscaled imm9) when the displacement is negative or misaligned.
     * FP-relative locals in particular always sit at negative offsets. */
    int use_unscaled = (m->disp < 0) || (m->disp & (width - 1)) != 0;
    if (use_unscaled) {
        if (m->disp < -256 || m->disp > 255) {
            asm_err_at(e, ln,
                    "LDR/STR: offset out of range for unscaled imm9 (-256..255)");
            return;
        }
        _aarch64_enc_ldst_unscaled(e, 0, is_load, width, rt, rn, (int32_t)m->disp);
        return;
    }
    uint32_t off = (uint32_t)m->disp;
    if (width == 8) {
        if (is_load) aarch64_enc_ldr_imm(e, rt, rn, off);
        else aarch64_enc_str_imm(e, rt, rn, off);
    } else if (width == 4) {
        if (is_load) aarch64_enc_ldr32_imm_gpr(e, rt, rn, off);
        else aarch64_enc_str32_imm_gpr(e, rt, rn, off);
    } else {
        asm_err_at(e, ln, "LDR/STR: unsupported width");
        return;
    }
}

/* LDRB/STRB/LDRH/STRH dispatch. The destination/source must be a W
 * register; `width` is 1 (byte) or 2 (halfword). Supports the same
 * address forms as LDR/STR: [Xn], [Xn,#imm], pre/post-index, [Xn,Xm],
 * [Xn,Xm,LSL #scale]. Negative or misaligned offsets fall back to the
 * unscaled imm9 form. */
void
aarch64_enc_ldst_sub(AsmEnc *e, AsmLine *ln, int is_load, int width)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "LDR/STR (byte/halfword) needs 2 operands");
        return;
    }
    AsmOperand *r = &ln->operands[0], *m = &ln->operands[1];
    if (!asm_is_reg(r) || r->reg.cls != AR_A64_W ||
            !asm_is_mem(m) || !m->has_base) {
        asm_err_at(e, ln, "LDR/STR (byte/halfword): bad operands");
        return;
    }
    int rt = aarch64_reg_num(r->reg);
    int rn = aarch64_reg_num(m->base);

    if (m->has_index) {
        int scaled = 0;
        int natural = (width == 2) ? 1 : 0;
        if (m->extend_lsl != 0) {
            if (m->extend_lsl != natural) {
                asm_err_at(e, ln,
                        "LDR/STR reg-off: LSL must be #0 or natural scale");
                return;
            }
            scaled = 1;
        }
        aarch64_enc_ldst_regoff(e, 0, is_load, width, scaled, rt, rn,
                aarch64_reg_num(m->index));
        return;
    }
    if (m->writeback) {
        if (m->disp < -256 || m->disp > 255) {
            asm_err_at(e, ln, "LDR/STR pre-index: imm9 out of range (-256..255)");
            return;
        }
        aarch64_enc_ldst_pre(e, 0, is_load, width, rt, rn, (int32_t)m->disp);
        return;
    }
    if (m->post_index) {
        if (m->disp < -256 || m->disp > 255) {
            asm_err_at(e, ln, "LDR/STR post-index: imm9 out of range (-256..255)");
            return;
        }
        aarch64_enc_ldst_post(e, 0, is_load, width, rt, rn, (int32_t)m->disp);
        return;
    }
    int use_unscaled = (m->disp < 0) || (m->disp & (width - 1)) != 0;
    if (use_unscaled) {
        if (m->disp < -256 || m->disp > 255) {
            asm_err_at(e, ln,
                    "LDR/STR: offset out of range for unscaled imm9 (-256..255)");
            return;
        }
        _aarch64_enc_ldst_unscaled(e, 0, is_load, width, rt, rn, (int32_t)m->disp);
        return;
    }
    uint32_t off = (uint32_t)m->disp;
    if (width == 1) {
        if (is_load) aarch64_enc_ldrb_imm(e, rt, rn, off);
        else aarch64_enc_strb_imm(e, rt, rn, off);
    } else {
        if (is_load) aarch64_enc_ldrh_imm(e, rt, rn, off);
        else aarch64_enc_strh_imm(e, rt, rn, off);
    }
}

/* LDRSB/LDRSH/LDRSW dispatch. `width` is 1/2/4. The destination class
 * picks the sign-extend target: X (64-bit) or W (32-bit). LDRSW has no
 * W-destination form. Supports the same address forms as LDR/STR. */
void
aarch64_enc_ldrs(AsmEnc *e, AsmLine *ln, int width)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "LDRS needs 2 operands");
        return;
    }
    AsmOperand *r = &ln->operands[0];
    AsmOperand *m = &ln->operands[1];
    if (!asm_is_reg(r) || !asm_is_mem(m) || !m->has_base) {
        asm_err_at(e, ln, "LDRS: bad operands");
        return;
    }
    int dst_64 = (r->reg.cls == AR_A64_X);
    if (r->reg.cls != AR_A64_X && r->reg.cls != AR_A64_W) {
        asm_err_at(e, ln, "LDRS: destination must be W or X register");
        return;
    }
    if (width == 4 && !dst_64) {
        asm_err_at(e, ln, "LDRSW: destination must be X");
        return;
    }
    int rt = aarch64_reg_num(r->reg);
    int rn = aarch64_reg_num(m->base);

    if (m->has_index) {
        int scaled = 0;
        int natural = (width == 2) ? 1 : (width == 4) ? 2 : 0;
        if (m->extend_lsl != 0) {
            if (m->extend_lsl != natural) {
                asm_err_at(e, ln,
                        "LDRS reg-off: LSL must be #0 or natural scale");
                return;
            }
            scaled = 1;
        }
        aarch64_ldrs_regoff_emit(e, width, dst_64, scaled, rt, rn,
                aarch64_reg_num(m->index));
        return;
    }
    if (m->writeback) {
        if (m->disp < -256 || m->disp > 255) {
            asm_err_at(e, ln, "LDRS pre-index: imm9 out of range (-256..255)");
            return;
        }
        aarch64_ldrs_imm9_emit(e, width, dst_64, 0x3u, rt, rn, (int32_t)m->disp);
        return;
    }
    if (m->post_index) {
        if (m->disp < -256 || m->disp > 255) {
            asm_err_at(e, ln, "LDRS post-index: imm9 out of range (-256..255)");
            return;
        }
        aarch64_ldrs_imm9_emit(e, width, dst_64, 0x1u, rt, rn, (int32_t)m->disp);
        return;
    }
    int use_unscaled = (m->disp < 0) || (m->disp & (width - 1)) != 0;
    if (use_unscaled) {
        if (m->disp < -256 || m->disp > 255) {
            asm_err_at(e, ln,
                    "LDRS: offset out of range for unscaled imm9 (-256..255)");
            return;
        }
        aarch64_ldrs_imm9_emit(e, width, dst_64, 0x0u, rt, rn, (int32_t)m->disp);
        return;
    }
    uint32_t off = (uint32_t)m->disp;
    if (width == 1) aarch64_enc_ldrsb_imm(e, dst_64, rt, rn, off);
    else if (width == 2) aarch64_enc_ldrsh_imm(e, dst_64, rt, rn, off);
    else aarch64_enc_ldrsw_imm(e, rt, rn, off);
}

/* Try to encode a double as an 8-bit ARM "expanded FP immediate" for use
 * by FMOV (immediate).  The valid set is a sparse mix of 32 positive and
 * 32 negative values (e.g. +/-0.125, +/-0.1875, ..., +/-31.0).  Returns 1 on
 * success and writes *out; returns 0 if the value isn't representable.
 *
 * is_double=0 -> 32-bit single; =1 -> 64-bit double. */
int
aarch64_fmov_encode_imm8(double v, int is_double, uint8_t *out)
{
    if (is_double) {
        union {
            double d;
            uint64_t u;
        } cvt;
        cvt.d = v;
        uint64_t u = cvt.u;
        uint64_t sign = (u >> 63) & 1ULL;
        uint64_t exp = (u >> 52) & 0x7FFULL;
        uint64_t mant = u & ((1ULL << 52) - 1);
        if (mant & ((1ULL << 48) - 1))
            return 0; /* low 48 mant bits must be 0 */
        uint64_t efgh = (mant >> 48) & 0xFULL;
        uint64_t b_hi = (exp >> 10) & 1ULL;
        uint64_t b = !b_hi ? 1ULL : 0ULL;
        for (int i = 2; i <= 9; ++i) {
            if (((exp >> i) & 1ULL) != b)
                return 0; /* mid 8 bits must all equal b */
        }
        uint64_t cd = exp & 0x3ULL;
        *out = (uint8_t)((sign << 7) | (b << 6) | (cd << 4) | efgh);
        return 1;
    } else {
        union {
            float f;
            uint32_t u;
        } cvt;
        cvt.f = (float)v;
        uint32_t u = cvt.u;
        uint32_t sign = (u >> 31) & 1U;
        uint32_t exp = (u >> 23) & 0xFFU;
        uint32_t mant = u & ((1U << 23) - 1);
        if (mant & ((1U << 19) - 1)) return 0;
        uint32_t efgh = (mant >> 19) & 0xFU;
        uint32_t b_hi = (exp >> 7) & 1U;
        uint32_t b = !b_hi ? 1U : 0U;
        for (int i = 2; i <= 6; ++i) {
            if (((exp >> i) & 1U) != b) return 0;
        }
        uint32_t cd = exp & 0x3U;
        *out = (uint8_t)((sign << 7) | (b << 6) | (cd << 4) | efgh);
        return 1;
    }
}

/* FADD/FSUB/FMUL/FDIV dispatch - works on either scalar SIMD regs
 * (S0,S1,S2 / D0,D1,D2) or V-form vectors (V0.4S etc.).  `op` selects
 * the instruction.  Only 3-operand forms (Vd, Vn, Vm) are accepted. */
void
aarch64_enc_fp_arith(AsmEnc *e, AsmLine *ln, TasmFOp op)
{
    if (ln->n_operands != 3) {
        asm_err_at(e, ln, "FADD/FSUB/FMUL/FDIV: 3 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0], *n = &ln->operands[1],
               *m = &ln->operands[2];
    if (!asm_is_reg(d) || !asm_is_reg(n) || !asm_is_reg(m)) {
        asm_err_at(e, ln, "FP arith: bad operands");
        return;
    }

    AsmRegClass dc = d->reg.cls, nc = n->reg.cls, mc = m->reg.cls;
    if (dc != nc || nc != mc) {
        asm_err_at(e, ln, "FP arith: operand classes must match");
        return;
    }

    /* Scalar form: all three are S or D registers. */
    if (dc == AR_A64_S || dc == AR_A64_D) {
        int is_double = (dc == AR_A64_D);
        int opcode4 = op == TASM_FOP_FMUL ? 0 :
                op == TASM_FOP_FDIV       ? 1 :
                op == TASM_FOP_FADD       ? 2 :
                                            3;
        aarch64_enc_fp_2src_scalar(e, is_double, opcode4, aarch64_reg_num(d->reg),
                aarch64_reg_num(n->reg), aarch64_reg_num(m->reg));
        return;
    }

    /* Vector form: all three are V-regs with matching arrangement. */
    if (dc == AR_A64_V) {
        if (d->v_lane_bits == 0 || d->v_n_lanes == 0 ||
                d->v_lane_bits != n->v_lane_bits ||
                n->v_lane_bits != m->v_lane_bits ||
                d->v_n_lanes != n->v_n_lanes || n->v_n_lanes != m->v_n_lanes) {
            asm_err_at(e, ln,
                    "FP vec arith: arrangement mismatch (need matching V<n>.<arr>)");
            return;
        }
        /* Determine Q and sz from arrangement. */
        int Q, sz;
        if (d->v_lane_bits == 32 && d->v_n_lanes == 4) {
            Q = 1;
            sz = 0;
        } /* 4S */
        else if (d->v_lane_bits == 32 && d->v_n_lanes == 2) {
            Q = 0;
            sz = 0;
        } /* 2S */
        else if (d->v_lane_bits == 64 && d->v_n_lanes == 2) {
            Q = 1;
            sz = 1;
        } /* 2D */
        else {
            asm_err_at(e, ln, "FP vec arith: arrangement must be 2S/4S/2D");
            return;
        }

        int U = (op == TASM_FOP_FMUL || op == TASM_FOP_FDIV) ? 1 : 0;
        int bit23 = (op == TASM_FOP_FSUB) ? 1 : 0;
        int op5 = (op == TASM_FOP_FADD || op == TASM_FOP_FSUB) ? 0x1A :
                (op == TASM_FOP_FMUL)                          ? 0x1B :
                                                                 0x1F; /* FDIV */
        aarch64_enc_fp_3same_vec(e, Q, U, bit23, sz, op5, aarch64_reg_num(d->reg),
                aarch64_reg_num(n->reg), aarch64_reg_num(m->reg));
        return;
    }
    asm_err_at(e, ln, "FP arith: register must be S/D scalar or V.<arr> vector");
}

/* ---------- PRINTREG (debug helper, calls a C-side runtime) ----------
 *
 * `PRINTREG <reg>` expands to a save-args / setup / BL <helper> /
 * restore-args sequence around a call to one of:
 *
 *   _tasm_print_x(uint64_t v, int reg_num)         // GPR X regs
 *   _tasm_print_w(uint32_t v, int reg_num)         // GPR W regs (passed in low
 * half of X0) _tasm_print_s(float    v, int reg_num) _tasm_print_d(double   v,
 * int reg_num) _tasm_print_v(const void *p, int reg_num, int n_lanes, int
 * lane_bits, int kind)
 *
 * The helpers live in src/libtasm/tasm_debug.c - link that file into
 * any program that uses PRINTREG.
 *
 * For vectors the optional second operand selects a non-default kind:
 *   PRINTREG V0.4S            -> kind=2 (FP)         default for S/D
 * arrangements PRINTREG V0.16B           -> kind=0 (UINT)       default for B/H
 * arrangements PRINTREG V0.16B, SIGNED   -> kind=1 PRINTREG V0.8H, BF16      ->
 * kind=3
 */
void
aarch64_enc_printreg(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands < 1 || ln->n_operands > 2) {
        asm_err_at(e, ln, "PRINTREG: 1 or 2 operands");
        return;
    }
    AsmOperand *r = &ln->operands[0];
    if (!asm_is_reg(r)) {
        asm_err_at(e, ln, "PRINTREG: reg required");
        return;
    }
    AsmRegClass cls = r->reg.cls;
    int rn = aarch64_reg_num(r->reg);
    if (rn == 31 && (cls == AR_A64_X || cls == AR_A64_W || cls == AR_A64_SP)) {
        asm_err_at(e, ln, "PRINTREG: refuses XZR/SP");
        return;
    }

    /* Helper name to BL, and the per-class setup. */
    const char *helper = NULL;
    int is_vec = 0;
    int load_into_fp = 0;
    int fp_width = 0;
    if (cls == AR_A64_X) helper = "_tasm_print_x";
    else if (cls == AR_A64_W) helper = "_tasm_print_w";
    else if (cls == AR_A64_S) {
        helper = "_tasm_print_s";
        load_into_fp = 1;
        fp_width = 4;
    } else if (cls == AR_A64_D) {
        helper = "_tasm_print_d";
        load_into_fp = 1;
        fp_width = 8;
    } else if (cls == AR_A64_V) {
        helper = "_tasm_print_v";
        is_vec = 1;
    } else {
        asm_err_at(e, ln, "PRINTREG: unsupported register class");
        return;
    }

    // @TODO make it so it's not compile time platform specific
    //remove prefixed `_` on linux, only apple needs the `_`
#if defined(__linux__)
    helper++;
#endif

    /* Determine kind for vectors. */
    int v_kind = 0;
    if (is_vec) {
        if (r->v_lane_bits == 0) {
            asm_err_at(e, ln, "PRINTREG V: need .<arr> suffix");
            return;
        }
        if (r->v_lane_bits == 32 || r->v_lane_bits == 64) v_kind = 2; /* FP */
        else v_kind = 0;                                              /* UINT */
        if (ln->n_operands == 2) {
            AsmOperand *h = &ln->operands[1];
            if (h->kind == AOP_LABEL && h->label_name) {
                if (strcasecmp(h->label_name, "BF16") == 0) v_kind = 3;
                else if (strcasecmp(h->label_name, "SIGNED") == 0) v_kind = 1;
                else if (strcasecmp(h->label_name, "SINT") == 0) v_kind = 1;
                else if (strcasecmp(h->label_name, "UINT") == 0) v_kind = 0;
                else if (strcasecmp(h->label_name, "FP") == 0) v_kind = 2;
                else {
                    asm_err_at(e, ln, "PRINTREG: unknown kind hint");
                    return;
                }
            } else {
                asm_err_at(e, ln, "PRINTREG: 2nd operand must be a kind name");
                return;
            }
        }
    }

    /* Save EVERY caller-saved GPR (X0..X17) plus FP/LR so the BL into
     * `tasm_print_*` looks transparent to the surrounding asm.  That's
     * 9 STP pairs for X0..X17 + 1 STP for FP/LR = 10 STPs = 160 bytes
     * of stack.  Vector regs are not saved here - if you need them
     * preserved across PRINTREG, save/restore manually around the call. */
    static const A64Reg savepairs_a[] = { A_X0, A_X2, A_X4, A_X6, A_X8, A_X10,
        A_X12, A_X14, A_X16, A_FP };
    static const A64Reg savepairs_b[] = { A_X1, A_X3, A_X5, A_X7, A_X9, A_X11,
        A_X13, A_X15, A_X17, A_LR };
    const int n_pairs = (int)(sizeof savepairs_a / sizeof savepairs_a[0]);
    const int caller_save_bytes = n_pairs * 16;    /* 160 */
    const int target_slot_off = caller_save_bytes; /* where SUB slot ended up */

    if (!is_vec) {
        /* Scalar form (X / W / S / D). */
        aarch64_enc_sub_imm(e, 1, A_SP, A_SP, 16);
        if (load_into_fp) aarch64_enc_fp_ldst_imm(e, 0, fp_width, rn, A_SP, 0);
        else aarch64_enc_str_imm(e, rn, A_SP, 0);
        for (int i = 0; i < n_pairs; i++)
            aarch64_enc_ldst_pair(e, 0, 0, 8, 1, savepairs_a[i], savepairs_b[i],
                    A_SP, -16);

        if (load_into_fp) {
            aarch64_enc_fp_ldst_imm(e, 1, fp_width, /*Rt=*/0, A_SP,
                    target_slot_off);
            aarch64_enc_mov_imm64(e, A_X0, (uint64_t)rn);
        } else {
            aarch64_enc_ldr_imm(e, A_X0, A_SP, target_slot_off);
            aarch64_enc_mov_imm64(e, A_X1, (uint64_t)rn);
        }
        aarch64_emit_branch_sym(e, helper, 1);
        for (int i = n_pairs - 1; i >= 0; i--)
            aarch64_enc_ldst_pair(e, 0, 1, 8, 2, savepairs_a[i], savepairs_b[i],
                    A_SP, 16);
        if (load_into_fp) aarch64_enc_fp_ldst_imm(e, 1, fp_width, rn, A_SP, 0);
        else aarch64_enc_ldr_imm(e, rn, A_SP, 0);
        aarch64_enc_add_imm(e, 1, A_SP, A_SP, 16);
        return;
    }

    /* Vector form - same save set, but the spill slot is a Q reg (16B)
     * and we set up 5 args. */
    aarch64_enc_sub_imm(e, 1, A_SP, A_SP, 16);
    aarch64_enc_fp_ldst_imm(e, 0, 16, rn, A_SP, 0); /* STR Qn, [SP] */
    for (int i = 0; i < n_pairs; i++)
        aarch64_enc_ldst_pair(e, 0, 0, 8, 1, savepairs_a[i], savepairs_b[i], A_SP,
                -16);
    aarch64_enc_add_imm(e, 1, A_X0, A_SP, target_slot_off); /* ptr = &spilled Q */
    aarch64_enc_mov_imm64(e, A_X1, (uint64_t)rn);
    aarch64_enc_mov_imm64(e, A_X2, (uint64_t)r->v_n_lanes);
    aarch64_enc_mov_imm64(e, A_X3, (uint64_t)r->v_lane_bits);
    aarch64_enc_mov_imm64(e, A_X4, (uint64_t)v_kind);
    aarch64_emit_branch_sym(e, helper, 1);
    for (int i = n_pairs - 1; i >= 0; i--)
        aarch64_enc_ldst_pair(e, 0, 1, 8, 2, savepairs_a[i], savepairs_b[i], A_SP,
                16);
    aarch64_enc_fp_ldst_imm(e, 1, 16, rn, A_SP, 0); /* restore Qn */
    aarch64_enc_add_imm(e, 1, A_SP, A_SP, 16);
}

/* ---------- BF16 (FEAT_BF16) ---------- */

/* Helper: assert V-form arrangement (n_lanes, bits) and return reg num.
 * Returns -1 on mismatch and reports via err_at. */
static int
aarch64_chk_varr(AsmEnc *e, AsmLine *ln, AsmOperand *o, int want_lanes, int want_bits,
        int want_lane_idx_neg)
{
    if (!asm_is_reg(o) || o->reg.cls != AR_A64_V) {
        asm_err_at(e, ln, "BF16: operand must be V-form");
        return -1;
    }
    if (o->v_lane_bits != want_bits || o->v_n_lanes != want_lanes) {
        asm_err_at(e, ln, "BF16: wrong V<n>.<arr> arrangement");
        return -1;
    }
    if (want_lane_idx_neg && o->v_lane_idx != -1) {
        asm_err_at(e, ln, "BF16: unexpected lane index");
        return -1;
    }
    return aarch64_reg_num(o->reg);
}

/* BFDOT V<d>.<2S|4S>, V<n>.<4H|8H>, V<m>.<4H|8H>
 *   - Q=0 form: V<d>.2S, V<n>.4H, V<m>.4H
 *   - Q=1 form: V<d>.4S, V<n>.8H, V<m>.8H
 *
 * Indexed form: V<d>.<...>, V<n>.<...>, V<m>.2H[index]  (index 0..3)
 *   Vd/Vn arrangement same as above; Vm uses .2H with [idx]. */
void
aarch64_enc_bfdot(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 3) {
        asm_err_at(e, ln, "BFDOT: 3 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0], *n = &ln->operands[1],
               *m = &ln->operands[2];

    if (!asm_is_reg(d) || d->reg.cls != AR_A64_V || !asm_is_reg(n) ||
            n->reg.cls != AR_A64_V || !asm_is_reg(m) || m->reg.cls != AR_A64_V) {
        asm_err_at(e, ln, "BFDOT: V-form regs required");
        return;
    }

    int Q;
    if (d->v_lane_bits == 32 && d->v_n_lanes == 4) Q = 1;
    else if (d->v_lane_bits == 32 && d->v_n_lanes == 2) Q = 0;
    else {
        asm_err_at(e, ln, "BFDOT: Vd must be .2S or .4S");
        return;
    }

    int want_nh = Q ? 8 : 4;
    if (n->v_lane_bits != 16 || n->v_n_lanes != want_nh) {
        asm_err_at(e, ln, "BFDOT: Vn arrangement must be .4H (2S) or .8H (4S)");
        return;
    }

    if (m->v_lane_idx >= 0) {
        /* Indexed form: Vm.2H[idx] regardless of Q. */
        if (m->v_lane_bits != 16 || m->v_n_lanes != 2) {
            asm_err_at(e, ln, "BFDOT idx: Vm must be V<n>.2H[idx]");
            return;
        }
        if (m->v_lane_idx < 0 || m->v_lane_idx > 3) {
            asm_err_at(e, ln, "BFDOT idx: lane index must be 0..3");
            return;
        }
        aarch64_enc_bfdot_idx(e, Q, aarch64_reg_num(d->reg), aarch64_reg_num(n->reg),
                aarch64_reg_num(m->reg), m->v_lane_idx);
        return;
    }
    /* Plain vector form. */
    if (m->v_lane_bits != 16 || m->v_n_lanes != want_nh) {
        asm_err_at(e, ln, "BFDOT: Vm arrangement must match Vn");
        return;
    }
    aarch64_enc_bfdot_vec(e, Q, aarch64_reg_num(d->reg), aarch64_reg_num(n->reg),
            aarch64_reg_num(m->reg));
}

/* BFMLALB / BFMLALT Vd.4S, Vn.8H, Vm.8H - output is always full 4S. */
void
aarch64_enc_bfmlal(AsmEnc *e, AsmLine *ln, int is_top)
{
    if (ln->n_operands != 3) {
        asm_err_at(e, ln, "BFMLAL: 3 operands");
        return;
    }
    int rd = aarch64_chk_varr(e, ln, &ln->operands[0], 4, 32, 1);
    int rn = aarch64_chk_varr(e, ln, &ln->operands[1], 8, 16, 1);
    int rm = aarch64_chk_varr(e, ln, &ln->operands[2], 8, 16, 1);
    if (rd < 0 || rn < 0 || rm < 0) return;
    if (is_top) aarch64_enc_bfmlalt(e, rd, rn, rm);
    else aarch64_enc_bfmlalb(e, rd, rn, rm);
}

/* BFMMLA Vd.4S, Vn.8H, Vm.8H */
void
aarch64_enc_bfmmla(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 3) {
        asm_err_at(e, ln, "BFMMLA: 3 operands");
        return;
    }
    int rd = aarch64_chk_varr(e, ln, &ln->operands[0], 4, 32, 1);
    int rn = aarch64_chk_varr(e, ln, &ln->operands[1], 8, 16, 1);
    int rm = aarch64_chk_varr(e, ln, &ln->operands[2], 8, 16, 1);
    if (rd < 0 || rn < 0 || rm < 0) return;
    uint32_t w = 0x6E40EC00u |
                 (((uint32_t)rm & 0x1F) << 16) |
                 (((uint32_t)rn & 0x1F) << 5)  |
                 ((uint32_t)rd & 0x1F);
    put_word(e, w);
}

/* BFCVT Hd, Sn - scalar fp32 -> bf16. */
void
aarch64_enc_bfcvt_scalar(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "BFCVT: 2 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0], *n = &ln->operands[1];
    if (!asm_is_reg(d) || d->reg.cls != AR_A64_H || !asm_is_reg(n) ||
            n->reg.cls != AR_A64_S) {
        asm_err_at(e, ln, "BFCVT: expected Hd, Sn");
        return;
    }
    aarch64_enc_bfcvt(e, aarch64_reg_num(d->reg), aarch64_reg_num(n->reg));
}

/* BFCVTN  Vd.4H, Vn.4S
 * BFCVTN2 Vd.8H, Vn.4S */
void
aarch64_enc_bfcvtn(AsmEnc *e, AsmLine *ln, int is_top)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "BFCVTN: 2 operands");
        return;
    }
    int want_lanes = is_top ? 8 : 4;
    int rd = aarch64_chk_varr(e, ln, &ln->operands[0], want_lanes, 16, 1);
    int rn = aarch64_chk_varr(e, ln, &ln->operands[1], 4, 32, 1);
    if (rd < 0 || rn < 0) return;
    _aarch64_enc_bfcvtn(e, is_top ? 1 : 0, rd, rn);
}

/* FMOV dispatch.  Recognises the scalar/imm/GPR-swap forms. */
void
aarch64_enc_fmov(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "FMOV: 2 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0], *s = &ln->operands[1];

    /* FMOV Sd, #imm / FMOV Dd, #imm */
    if (asm_is_reg(d) && (d->reg.cls == AR_A64_S || d->reg.cls == AR_A64_D) &&
            asm_is_imm(s) && s->is_float) {
        int is_double = (d->reg.cls == AR_A64_D);
        uint8_t imm8 = 0;
        if (!aarch64_fmov_encode_imm8(s->f64, is_double, &imm8)) {
            asm_err_at(e, ln,
                    "FMOV: float immediate not representable as 8-bit FP imm");
            return;
        }
        aarch64_enc_fmov_imm(e, is_double, aarch64_reg_num(d->reg), imm8);
        return;
    }

    /* FMOV Sd, Sn / FMOV Dd, Dn / FMOV Hd, Hn - same scalar class. */
    if (asm_is_reg(d) && asm_is_reg(s) && d->reg.cls == s->reg.cls) {
        if (d->reg.cls == AR_A64_S || d->reg.cls == AR_A64_D) {
            int is_double = (d->reg.cls == AR_A64_D);
            aarch64_enc_fmov_reg(e, is_double, aarch64_reg_num(d->reg),
                    aarch64_reg_num(s->reg));
            return;
        }
        if (d->reg.cls == AR_A64_H) {
            aarch64_enc_fmov_reg_h(e, aarch64_reg_num(d->reg), aarch64_reg_num(s->reg));
            return;
        }
    }

    /* FMOV Wd, Sn / FMOV Sd, Wn / FMOV Xd, Dn / FMOV Dd, Xn / FMOV Wd, Hn /
     * FMOV Hd, Wn */
    if (asm_is_reg(d) && asm_is_reg(s)) {
        int d_is_w = (d->reg.cls == AR_A64_W),
            d_is_x = (d->reg.cls == AR_A64_X);
        int d_is_s = (d->reg.cls == AR_A64_S),
            d_is_d = (d->reg.cls == AR_A64_D);
        int d_is_h = (d->reg.cls == AR_A64_H);
        int s_is_w = (s->reg.cls == AR_A64_W),
            s_is_x = (s->reg.cls == AR_A64_X);
        int s_is_s = (s->reg.cls == AR_A64_S),
            s_is_d = (s->reg.cls == AR_A64_D);
        int s_is_h = (s->reg.cls == AR_A64_H);

        if (d_is_w && s_is_s) {
            aarch64_enc_fmov_gpr(e, 0, 1, aarch64_reg_num(d->reg), aarch64_reg_num(s->reg));
            return;
        }
        if (d_is_s && s_is_w) {
            aarch64_enc_fmov_gpr(e, 0, 0, aarch64_reg_num(d->reg), aarch64_reg_num(s->reg));
            return;
        }
        if (d_is_x && s_is_d) {
            aarch64_enc_fmov_gpr(e, 1, 1, aarch64_reg_num(d->reg), aarch64_reg_num(s->reg));
            return;
        }
        if (d_is_d && s_is_x) {
            aarch64_enc_fmov_gpr(e, 1, 0, aarch64_reg_num(d->reg), aarch64_reg_num(s->reg));
            return;
        }
        if (d_is_w && s_is_h) {
            aarch64_enc_fmov_h_gpr(e, 1, aarch64_reg_num(d->reg), aarch64_reg_num(s->reg));
            return;
        }
        if (d_is_h && s_is_w) {
            aarch64_enc_fmov_h_gpr(e, 0, aarch64_reg_num(d->reg), aarch64_reg_num(s->reg));
            return;
        }
    }

    asm_err_at(e, ln, "FMOV: unsupported operand combination");
}

/* LDP/STP and LDNP/STNP - load/store pair.  Three operands: Rt1, Rt2, [mem].
 *
 * Pair widths: W/X for GPRs, S/D/Q for SIMD.  When `nontemporal` is set
 * (LDNP/STNP), only the signed-offset addressing mode is allowed; ARM
 * doesn't define writeback or post-index for the NT variant. */
void
aarch64_enc_ldp_stp(AsmEnc *e, AsmLine *ln, int is_load, int nontemporal)
{
    if (ln->n_operands != 3) {
        asm_err_at(e, ln, "LDP/STP needs 3 operands");
        return;
    }
    AsmOperand *r1 = &ln->operands[0], *r2 = &ln->operands[1],
               *m = &ln->operands[2];
    if (!asm_is_reg(r1) || !asm_is_reg(r2) || !asm_is_mem(m) || !m->has_base) {
        asm_err_at(e, ln, "LDP/STP: bad operands");
        return;
    }
    if (r1->reg.cls != r2->reg.cls) {
        asm_err_at(e, ln, "LDP/STP: both regs must be same class");
        return;
    }

    int is_simd, width;
    switch (r1->reg.cls) {
        case AR_A64_X: is_simd = 0; width = 8; break;
        case AR_A64_W: is_simd = 0; width = 4; break;
        case AR_A64_S: is_simd = 1; width = 4; break;
        case AR_A64_D: is_simd = 1; width = 8; break;
        case AR_A64_Q: is_simd = 1; width = 16; break;
        default: asm_err_at(e, ln, "LDP/STP: regs must be W/X or S/D/Q"); return;
    }

    if (m->has_index) {
        asm_err_at(e, ln, "LDP/STP: register-offset not valid (ISA limit)");
        return;
    }

    int mode;
    if (nontemporal) {
        if (m->writeback || m->post_index) {
            asm_err_at(e, ln, "LDNP/STNP: writeback / post-index not allowed");
            return;
        }
        mode = 3;
    } else {
        mode = m->writeback ? 1 : (m->post_index ? 2 : 0);
    }

    if (aarch64_enc_ldst_pair(e, is_simd, is_load, width, mode,
        aarch64_reg_num(r1->reg), aarch64_reg_num(r2->reg),
        aarch64_reg_num(m->base), (int32_t)m->disp) == 0)
    {
        asm_err_at(e, ln, "LDP/STP: offset out of range or misaligned");
    }
}

/* Detect a `@@N`-style label name and return its number, or -1 otherwise.
 * Mirrors the x86 path's operand_local_num. */
static int
aarch64_local_label_num(AsmOperand *o)
{
    if (o->kind != AOP_LABEL || !o->label_name) return -1;
    if (o->label_name[0] != '@' || o->label_name[1] != '@') return -1;
    return atoi(o->label_name + 2);
}


/* CBZ / CBNZ Rt, label
 * Local @@N labels resolve via the same imm19 path as B.cond. */
void
aarch64_enc_cbz(AsmEnc *e, AsmLine *ln, int is_nonzero)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "CBZ/CBNZ: 2 operands");
        return;
    }

    AsmOperand *r = &ln->operands[0], *l = &ln->operands[1];
    if (!asm_is_reg(r)) {
        asm_err_at(e, ln, "CBZ/CBNZ: reg required");
        return;
    }

    if (r->reg.cls != AR_A64_X && r->reg.cls != AR_A64_W) {
        asm_err_at(e, ln, "CBZ/CBNZ: reg must be X or W");
        return;
    }

    int is_64 = (r->reg.cls == AR_A64_X);
    /* Numeric PC-relative target, as printed by the disassembler. */
    if (asm_is_imm(l) && !l->is_float) {
        int64_t off = l->imm;
        if (off % 4 != 0) {
            asm_err_at(e, ln, "CBZ/CBNZ: offset must be a multiple of 4");
            return;
        }
        int64_t words = off / 4;
        if (words < -(1 << 18) || words >= (1 << 18)) {
            asm_err_at(e, ln, "CBZ/CBNZ: offset out of range");
            return;
        }
        _aarch64_enc_cbz(e, is_64, is_nonzero, aarch64_reg_num(r->reg),
                         (int32_t)words);
        return;
    }
    if (!asm_is_label(l)) {
        asm_err_at(e, ln, "CBZ/CBNZ: label or #offset required");
        return;
    }
    int ln_num = aarch64_local_label_num(l);
    size_t patch = e->len;
    _aarch64_enc_cbz(e, is_64, is_nonzero, aarch64_reg_num(r->reg), 0);

    AsmFixup f = { 0 };
    if (ln_num >= 0) {
        f.kind = AF_LOCAL;
        f.local_num = ln_num;
    } else {
        f.kind = AF_SYMBOL;
        f.sym = xstrdup(l->label_name);
    }
    f.patch_offset = patch;
    f.width = 19; /* imm19 fixup, same as B.cond */
    f.pcrel = 1;
    asm_add_fixup(e, f);
}

/* FABS / FNEG / FSQRT, 2 operands, must be same scalar FP class (S or D). */
void
aarch64_enc_fp1src(AsmEnc *e, AsmLine *ln, int opcode6)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "FP 1-source: 2 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0], *n = &ln->operands[1];
    if (!asm_is_reg(d) || !asm_is_reg(n) || d->reg.cls != n->reg.cls) {
        asm_err_at(e, ln, "FP 1-source: matching S/D regs");
        return;
    }
    if (d->reg.cls != AR_A64_S && d->reg.cls != AR_A64_D) {
        asm_err_at(e, ln, "FP 1-source: dst must be S or D");
        return;
    }
    int is_double = (d->reg.cls == AR_A64_D);
    _aarch64_enc_fp_1src(e, is_double, opcode6, aarch64_reg_num(d->reg),
            aarch64_reg_num(n->reg));
}

/* FCMP / FCMPE, supports register form and zero (FP-immediate 0.0) form. */
void
aarch64_enc_fcmp(AsmEnc *e, AsmLine *ln, int with_exceptions)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "FCMP: 2 operands");
        return;
    }
    AsmOperand *n = &ln->operands[0], *m = &ln->operands[1];
    if (!asm_is_reg(n) || (n->reg.cls != AR_A64_S && n->reg.cls != AR_A64_D)) {
        asm_err_at(e, ln, "FCMP: lhs must be S or D reg");
        return;
    }
    int is_double = (n->reg.cls == AR_A64_D);
    int rn = aarch64_reg_num(n->reg);

    if (asm_is_reg(m) && m->reg.cls == n->reg.cls) {
        _aarch64_enc_fcmp(e, is_double, with_exceptions, 0, rn, aarch64_reg_num(m->reg));
        return;
    }
    if (asm_is_imm(m) && m->is_float && m->f64 == 0.0) {
        _aarch64_enc_fcmp(e, is_double, with_exceptions, 1, rn, 0);
        return;
    }
    /* Allow integer 0 too - Terry-style `FCMP S0, 0` without the `.0`. */
    if (asm_is_imm(m) && !m->is_float && m->imm == 0) {
        _aarch64_enc_fcmp(e, is_double, with_exceptions, 1, rn, 0);
        return;
    }
    asm_err_at(e, ln, "FCMP: rhs must be matching-class reg or #0.0");
}

/* FP <-> int conversions. Picks (sf, ftype) from the operand widths and
 * (rmode_opcode5) from the mnemonic family. */
void
aarch64_enc_fp_int_cvt(AsmEnc *e, AsmLine *ln, int rmode_opcode5, int fp_to_int)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "FP-int cvt: 2 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0], *n = &ln->operands[1];
    if (!asm_is_reg(d) || !asm_is_reg(n)) {
        asm_err_at(e, ln, "FP-int cvt: bad operands");
        return;
    }

    int sf, ftype;
    AsmRegClass dc = d->reg.cls, nc = n->reg.cls;
    if (fp_to_int) {
        /* FCVTZS/FCVTZU: dst is GPR (W/X), src is FP scalar (S/D). */
        if (dc == AR_A64_W && nc == AR_A64_S) {
            sf = 0;
            ftype = 0;
        } else if (dc == AR_A64_X && nc == AR_A64_D) {
            sf = 1;
            ftype = 1;
        } else if (dc == AR_A64_W && nc == AR_A64_D) {
            sf = 0;
            ftype = 1;
        } else if (dc == AR_A64_X && nc == AR_A64_S) {
            sf = 1;
            ftype = 0;
        } else {
            asm_err_at(e, ln, "FCVTZ*: dst W/X, src S/D");
            return;
        }
    } else {
        /* SCVTF/UCVTF: dst is FP scalar, src is GPR. */
        if (dc == AR_A64_S && nc == AR_A64_W) {
            sf = 0;
            ftype = 0;
        } else if (dc == AR_A64_D && nc == AR_A64_X) {
            sf = 1;
            ftype = 1;
        } else if (dc == AR_A64_S && nc == AR_A64_X) {
            sf = 1;
            ftype = 0;
        } else if (dc == AR_A64_D && nc == AR_A64_W) {
            sf = 0;
            ftype = 1;
        } else {
            asm_err_at(e, ln, "SCVTF/UCVTF: dst S/D, src W/X");
            return;
        }
    }
    _aarch64_enc_fp_int_cvt(e, sf, ftype,
                            rmode_opcode5,
                            aarch64_reg_num(d->reg),
                            aarch64_reg_num(n->reg));
}

/* SUBS Xd/Wd, Xn/Wn, Xm/Wm                 (shifted register)
 * SUBS Xd/Wd, Xn/Wn, Xm, LSL|LSR|ASR #N    (4-operand shifted reg)
 * SUBS Xd/Wd, Xn/Wn, #imm12 */
void
aarch64_enc_subs(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands < 3 || ln->n_operands > 4) {
        asm_err_at(e, ln, "SUBS: 3 or 4 operands");
        return;
    }

    AsmOperand *d = &ln->operands[0];
    AsmOperand *n = &ln->operands[1];
    AsmOperand *m = &ln->operands[2];
               
    if (!asm_is_reg(d) || !asm_is_reg(n)) {
        asm_err_at(e, ln, "SUBS: bad operands");
        return;
    }
    if (d->reg.cls != n->reg.cls ||
        (d->reg.cls != AR_A64_X && d->reg.cls != AR_A64_W))
    {
        asm_err_at(e, ln, "SUBS: dst/src1 must both be X or both W");
        return;
    }
    int is_64 = (d->reg.cls == AR_A64_X);

    if (asm_is_imm(m)) {
        if (ln->n_operands == 4) {
            asm_err_at(e, ln, "SUBS: shift suffix not valid with immediate");
            return;
        }
        if (m->imm < 0 || m->imm > 0xFFF) {
            asm_err_at(e, ln, "SUBS: imm12 must be 0..4095");
            return;
        }
        aarch64_enc_subs_imm(e, is_64, aarch64_reg_num(d->reg), aarch64_reg_num(n->reg),
                (uint32_t)m->imm);
        return;
    }

    if (asm_is_reg(m) && m->reg.cls == d->reg.cls) {
        int st = 0, sa = 0;
        int rc = aarch64_extract_shift_suffix(e, ln, &st, &sa);
        if (rc < 0) return;
        if (st == ASHIFT_ROR) {
            asm_err_at(e, ln, "SUBS: ROR shift not valid");
            return;
        }
        aarch64_enc_subs_reg(e, is_64,
                             aarch64_reg_num(d->reg),
                             aarch64_reg_num(n->reg),
                             aarch64_reg_num(m->reg), st, sa);
        return;
    }

    asm_err_at(e, ln, "SUBS: rhs must be imm or matching-class reg");
}

/* LDR/STR for SIMD/FP scalar registers (B/H/S/D/Q).
 * Handles unsigned-offset, pre-index, post-index, and register-offset forms. */
void
aarch64_enc_simd_ldst(AsmEnc *e, AsmLine *ln, int is_load)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "LDR/STR: 2 operands");
        return;
    }
    AsmOperand *r = &ln->operands[0];
    AsmOperand *m = &ln->operands[1];

    if (!aarch64_is_simd_reg(r) || !asm_is_mem(m) || !m->has_base) {
        asm_err_at(e, ln, "LDR/STR: bad SIMD operands");
        return;
    }

    int width = aarch64_simd_scalar_width(r->reg);
    if (width == 0) {
        asm_err_at(e, ln, "LDR/STR: V-form needs B/H/S/D/Q reg");
        return;
    }

    if (m->base.cls != AR_A64_X && m->base.cls != AR_A64_SP) {
        asm_err_at(e, ln, "LDR/STR: base must be an X register or SP");
        return;
    }

    int rt = aarch64_reg_num(r->reg);
    int rn = aarch64_reg_num(m->base);

    /* Register offset. */
    if (m->has_index) {
        int scaled = 0;
        if (m->extend_lsl != 0) {
            /* Natural scale per width: B=0 H=1 S=2 D=3 Q=4. */
            int natural = (width == 1) ? 0 :
                    (width == 2)       ? 1 :
                    (width == 4)       ? 2 :
                    (width == 8)       ? 3 :
                                         4;
            if (m->extend_lsl != natural) {
                asm_err_at(e, ln,
                        "LDR/STR reg-off SIMD: LSL must be #0 or natural scale");
                return;
            }
            scaled = 1;
        }
        aarch64_enc_ldst_regoff(e, 1, is_load, width, scaled, rt, rn,
                aarch64_reg_num(m->index));
        return;
    }
    if (m->writeback) {
        if (m->disp < -256 || m->disp > 255) {
            asm_err_at(e, ln, "LDR/STR pre-index SIMD: imm9 out of range");
            return;
        }
        aarch64_enc_ldst_pre(e, 1, is_load, width, rt, rn, (int32_t)m->disp);
        return;
    }
    if (m->post_index) {
        if (m->disp < -256 || m->disp > 255) {
            asm_err_at(e, ln, "LDR/STR post-index SIMD: imm9 out of range");
            return;
        }
        aarch64_enc_ldst_post(e, 1, is_load, width, rt, rn, (int32_t)m->disp);
        return;
    }
    /* Fall back to the unscaled imm9 (LDUR/STUR) form for negative or
     * unaligned offsets - needed e.g. for FP-relative spills which sit
     * at negative offsets from FP. */
    if (m->disp < 0 || ((uint32_t)m->disp) % (uint32_t)width != 0) {
        if (m->disp < -256 || m->disp > 255) {
            asm_err_at(e, ln,
                    "LDR/STR SIMD: offset out of range for unscaled imm9 (-256..255)");
            return;
        }
        _aarch64_enc_ldst_unscaled(e, 1, is_load, width, rt, rn, (int32_t)m->disp);
        return;
    }
    if (((uint32_t)m->disp / (uint32_t)width) > 0xFFFu) {
        asm_err_at(e, ln, "LDR/STR SIMD: offset exceeds 12-bit scaled range");
        return;
    }
    aarch64_enc_fp_ldst_imm(e, is_load, width, rt, rn, (uint32_t)m->disp);
}

/* B / BL / B.cond - operand is either a local @@N or a symbol. */
void
aarch64_enc_branch(AsmEnc *e, AsmLine *ln, int is_bl, int is_cond, A64Cond cc)
{
    if (ln->n_operands != 1) {
        asm_err_at(e, ln, "branch needs 1 operand");
        return;
    }
    AsmOperand *o = &ln->operands[0];
    /* Numeric PC-relative target (`b #+8`, `b.eq #-4`) - the form the
     * disassembler prints. Byte offset from this instruction. */
    if (asm_is_imm(o) && !o->is_float) {
        int64_t off = o->imm;
        if (off % 4 != 0) {
            asm_err_at(e, ln, "branch: offset must be a multiple of 4");
            return;
        }
        int64_t words = off / 4;
        int64_t lim = is_cond ? (1 << 18) : (1 << 25);
        if (words < -lim || words >= lim) {
            asm_err_at(e, ln, "branch: offset out of range");
            return;
        }
        if (is_cond) aarch64_enc_b_cond(e, cc, (int32_t)words);
        else if (is_bl) aarch64_enc_bl(e, (int32_t)words);
        else aarch64_enc_b(e, (int32_t)words);
        return;
    }
    if (!asm_is_label(o)) {
        asm_err_at(e, ln, "branch: operand must be a label or #offset");
        return;
    }
    int ln_num = aarch64_local_label_num(o);
    if (ln_num >= 0) {
        if (is_cond) aarch64_emit_b_cond_local(e, cc, ln_num);
        else aarch64_emit_branch_local(e, ln_num, is_bl);
        return;
    }
    if (!is_cond) {
        aarch64_emit_branch_sym(e, o->label_name, is_bl);
        return;
    }
    /* `b.<cc> external_sym`: Mach-O on arm64 has no first-class reloc
     * for the imm19 cond-branch displacement and BCOND19's +/-1MB reach
     * couldn't hit most external symbols anyway. We expand to a 2-instr
     * trampoline:
     *
     *     b.<!cc>  +8        ; skip the next instr if cond NOT taken
     *     b        sym       ; unconditional BRANCH26 to sym (linker veneers it for >128MB)
     *
     * `b.al` has no real inverse (NV is reserved) - it's already
     * unconditional, so degenerate it to a plain `b sym`. */
    if (cc == A_AL) {
        aarch64_emit_branch_sym(e, o->label_name, /*is_bl=*/0);
        return;
    }
    A64Cond inv = (A64Cond)((int)cc ^ 1);
    aarch64_enc_b_cond(e, inv, /*rel_words=*/2);
    aarch64_emit_branch_sym(e, o->label_name, /*is_bl=*/0);
}

/* ================================================================ system */

static int
aarch64_streq_ci(const char *a, const char *b)
{
    while (*a && *b) {
        int ca = tolower((unsigned char)*a++);
        int cb = tolower((unsigned char)*b++);
        if (ca != cb) return 0;
    }
    return *a == 0 && *b == 0;
}

/* Map a DMB/DSB option name (case-insensitive) to its 4-bit CRm value.
 * Returns -1 if unknown. */
static int
aarch64_barrier_option_from_name(const char *name)
{
    static const struct {
        const char *n;
        int v;
    } tab[] = {
        { "oshld", 1 },
        { "oshst", 2 },
        { "osh", 3 },
        { "nshld", 5 },
        { "nshst", 6 },
        { "nsh", 7 },
        { "ishld", 9 },
        { "ishst", 10 },
        { "ish", 11 },
        { "ld", 13 },
        { "st", 14 },
        { "sy", 15 },
    };
    for (size_t i = 0; i < sizeof tab / sizeof tab[0]; ++i)
        if (aarch64_streq_ci(name, tab[i].n)) return tab[i].v;
    return -1;
}

/* DMB/DSB/ISB.  Single optional operand: a label (option name) or imm4.
 * ISB with no operand defaults to SY (CRm=15). */
void
aarch64_enc_barrier(AsmEnc *e, AsmLine *ln, int opc2)
{
    uint32_t crm = 15; /* default SY */
    if (ln->n_operands == 1) {
        AsmOperand *o = &ln->operands[0];
        if (asm_is_label(o)) {
            int v = aarch64_barrier_option_from_name(o->label_name);
            if (v < 0) {
                asm_err_at(e, ln, "DMB/DSB/ISB: unknown barrier option");
                return;
            }
            crm = (uint32_t)v;
        } else if (asm_is_imm(o)) {
            if (o->imm < 0 || o->imm > 15) {
                asm_err_at(e, ln, "DMB/DSB/ISB: imm out of range (0..15)");
                return;
            }
            crm = (uint32_t)o->imm;
        } else {
            asm_err_at(e, ln, "DMB/DSB/ISB: bad operand");
            return;
        }
    } else if (ln->n_operands != 0) {
        asm_err_at(e, ln, "DMB/DSB/ISB: 0 or 1 operand");
        return;
    }
    _aarch64_enc_barrier(e, opc2, crm);
}

/* Pack op0|op1|CRn|CRm|op2 into the 16-bit sysreg field used by MRS/MSR.
 * `op0` is 2 or 3 (only its low bit is encoded in `o0`). */
static uint16_t
aarch64_sysreg_pack(int op0, int op1, int crn, int crm, int op2)
{
    return (uint16_t)(((op0 & 1) << 14) |
           ((op1 & 7) << 11) |
           ((crn & 0xF) << 7) |
           ((crm & 0xF) << 3) |
           (op2 & 7));
}

/* Lookup table of EL0-accessible named system registers.  Add entries on
 * demand; the generic `S<op0>_<op1>_C<n>_C<m>_<op2>` escape covers the
 * long tail. */
static int
aarch64_sysreg_from_name(const char *name, uint16_t *out)
{
    static const struct {
        const char *n;
        int op0, op1, crn, crm, op2;
    } tab[] = {
        /* NZCV/FPCR/FPSR: op0=3, op1=3, CRn=4, CRm=2/4, op2 selects. */
        { "nzcv", 3, 3, 4, 2, 0 },
        { "daif", 3, 3, 4, 2, 1 },
        { "fpcr", 3, 3, 4, 4, 0 },
        { "fpsr", 3, 3, 4, 4, 1 },
        { "tpidr_el0", 3, 3, 13, 0, 2 },
        { "tpidrro_el0", 3, 3, 13, 0, 3 },
        { "cntfrq_el0", 3, 3, 14, 0, 0 },
        { "cntvct_el0", 3, 3, 14, 0, 2 },
        { "cntpct_el0", 3, 3, 14, 0, 1 },
        { "ctr_el0", 3, 3, 0, 0, 1 },
        { "dczid_el0", 3, 3, 0, 0, 7 },
        { "midr_el1", 3, 0, 0, 0, 0 },
        { "mpidr_el1", 3, 0, 0, 0, 5 },
    };
    for (size_t i = 0; i < sizeof tab / sizeof tab[0]; ++i) {
        if (aarch64_streq_ci(name, tab[i].n)) {
            *out = aarch64_sysreg_pack(tab[i].op0, tab[i].op1, tab[i].crn, tab[i].crm,
                    tab[i].op2);
            return 1;
        }
    }
    /* Generic escape: S<op0>_<op1>_C<n>_C<m>_<op2>.  ARM's canonical form
     * has C before the CRn/CRm digits and bare op2 (e.g. S3_3_C14_C0_2),
     * but some assemblers also accept C2 at the end - accept both. */
    if ((name[0] == 's' || name[0] == 'S')) {
        const char *p = name + 1;
        int parts[5];
        int n = 0;
        while (n < 5) {
            if (*p == 'c' || *p == 'C') ++p; /* optional C prefix */
            if (*p < '0' || *p > '9') break;
            int v = 0;
            while (*p >= '0' && *p <= '9') {
                v = v * 10 + (*p - '0');
                ++p;
            }
            parts[n++] = v;
            if (*p == '_') ++p;
            else break;
        }
        if (n == 5 && *p == '\0' && (parts[0] == 2 || parts[0] == 3) &&
                parts[1] >= 0 && parts[1] <= 7 && parts[2] >= 0 &&
                parts[2] <= 15 && parts[3] >= 0 && parts[3] <= 15 &&
                parts[4] >= 0 && parts[4] <= 7) {
            *out = aarch64_sysreg_pack(parts[0], parts[1], parts[2], parts[3],
                    parts[4]);
            return 1;
        }
    }
    return 0;
}

/* PSTATE-field table for MSR (immediate).  op1/op2 identify the field;
 * the immediate is a 4-bit value supplied by the user. */
static int
aarch64_pstate_from_name(const char *name, int *op1, int *op2)
{
    static const struct {
        const char *n;
        int op1, op2;
    } tab[] = {
        { "spsel", 0, 5 },
        { "daifset", 3, 6 },
        { "daifclr", 3, 7 },
        { "uao", 0, 3 },
        { "pan", 0, 4 },
    };
    for (size_t i = 0; i < sizeof tab / sizeof tab[0]; ++i) {
        if (aarch64_streq_ci(name, tab[i].n)) {
            *op1 = tab[i].op1;
            *op2 = tab[i].op2;
            return 1;
        }
    }
    return 0;
}

void
aarch64_enc_mrs(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 2 || !asm_is_reg(&ln->operands[0]) ||
            ln->operands[0].reg.cls != AR_A64_X ||
            !asm_is_label(&ln->operands[1])) {
        asm_err_at(e, ln, "MRS: expected `Xt, sysreg`");
        return;
    }
    uint16_t sr;
    if (!aarch64_sysreg_from_name(ln->operands[1].label_name, &sr)) {
        asm_err_at(e, ln, "MRS: unknown system register");
        return;
    }
    _aarch64_enc_mrs(e, aarch64_reg_num(ln->operands[0].reg), sr);
}

void
aarch64_enc_msr(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 2 || !asm_is_label(&ln->operands[0])) {
        asm_err_at(e, ln, "MSR: expected `sysreg, Xt` or `PSTATE, #imm`");
        return;
    }
    const char *dst = ln->operands[0].label_name;
    int p1, p2;
    /* PSTATE field + immediate. */
    if (asm_is_imm(&ln->operands[1]) && aarch64_pstate_from_name(dst, &p1, &p2)) {
        if (ln->operands[1].imm < 0 || ln->operands[1].imm > 15) {
            asm_err_at(e, ln, "MSR imm: value out of range (0..15)");
            return;
        }
        aarch64_enc_msr_imm(e, (uint32_t)p1, (uint32_t)p2,
                (uint32_t)ln->operands[1].imm);
        return;
    }
    /* Named or generic sysreg + Xt. */
    if (!asm_is_reg(&ln->operands[1]) || ln->operands[1].reg.cls != AR_A64_X) {
        asm_err_at(e, ln, "MSR: second operand must be Xt (or PSTATE+#imm)");
        return;
    }
    uint16_t sr;
    if (!aarch64_sysreg_from_name(dst, &sr)) {
        asm_err_at(e, ln, "MSR: unknown system register or PSTATE field");
        return;
    }
    aarch64_enc_msr_reg(e, sr, aarch64_reg_num(ln->operands[1].reg));
}

/* MOVZ / MOVK / MOVN spelled explicitly: `movz Xd, #imm16 {, lsl #hw}`.
 * Unlike `mov` (which synthesises a movz/movk sequence for any 64-bit
 * value), these encode exactly one instruction, so the immediate must
 * fit 16 bits and the shift must be a multiple of 16. */
void
aarch64_enc_mov_wide_mn(AsmEnc *e, AsmLine *ln, uint32_t opc)
{
    if (ln->n_operands < 2 || !asm_is_reg(&ln->operands[0]) ||
            !asm_is_imm(&ln->operands[1]) || ln->operands[1].is_float) {
        asm_err_at(e, ln, "MOVZ/MOVK/MOVN: expected `Rd, #imm16 {, lsl #hw}`");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    int is_64 = (d->reg.cls == AR_A64_X);
    if (!is_64 && d->reg.cls != AR_A64_W) {
        asm_err_at(e, ln, "MOVZ/MOVK/MOVN: dst must be Xd or Wd");
        return;
    }
    int64_t imm = ln->operands[1].imm;
    if (imm < 0 || imm > 0xFFFF) {
        asm_err_at(e, ln, "MOVZ/MOVK/MOVN: immediate out of 16-bit range");
        return;
    }
    int sa = 0;
    if (ln->n_operands == 3) {
        AsmOperand *s = &ln->operands[2];
        if (s->kind != AOP_SHIFT || s->shift_type != ASHIFT_LSL) {
            asm_err_at(e, ln, "MOVZ/MOVK/MOVN: 3rd operand must be LSL #hw");
            return;
        }
        sa = (int)s->imm;
    } else if (ln->n_operands != 2) {
        asm_err_at(e, ln, "MOVZ/MOVK/MOVN: 2 or 3 operands");
        return;
    }
    if (sa % 16 != 0 || sa > (is_64 ? 48 : 16)) {
        asm_err_at(e, ln, "MOVZ/MOVK/MOVN: shift must be 0/16 (Wd) or 0/16/32/48 (Xd)");
        return;
    }
    put_word(e, aarch64_mov_wide(is_64, opc, aarch64_reg_num(d->reg),
                                 (uint16_t)imm, sa / 16));
}

/* LDUR / STUR spelled explicitly: `ldur Rt, [Xn, #simm9]`. Same word
 * `ldr`/`str` fall back to for negative or misaligned offsets, but with
 * no scaled-imm12 path: the offset must fit signed 9 bits. */
void
aarch64_enc_ldst_unscaled_mn(AsmEnc *e, AsmLine *ln, int is_load)
{
    if (ln->n_operands != 2 || !asm_is_reg(&ln->operands[0]) ||
            !asm_is_mem(&ln->operands[1])) {
        asm_err_at(e, ln, "LDUR/STUR: expected `Rt, [Xn, #imm]`");
        return;
    }
    AsmOperand *r = &ln->operands[0], *m = &ln->operands[1];
    if (!m->has_base || m->has_index || m->writeback || m->post_index) {
        asm_err_at(e, ln, "LDUR/STUR: base + immediate offset only");
        return;
    }
    int width;
    if (r->reg.cls == AR_A64_X) width = 8;
    else if (r->reg.cls == AR_A64_W) width = 4;
    else {
        asm_err_at(e, ln, "LDUR/STUR: register must be Xt or Wt");
        return;
    }
    if (m->disp < -256 || m->disp > 255) {
        asm_err_at(e, ln, "LDUR/STUR: imm9 out of range (-256..255)");
        return;
    }
    int rt = aarch64_reg_num(r->reg), rn = aarch64_reg_num(m->base);
    if (is_load) aarch64_enc_ldur(e, width, rt, rn, (int32_t)m->disp);
    else aarch64_enc_stur(e, width, rt, rn, (int32_t)m->disp);
}

void
aarch64_enc_br_blr(AsmEnc *e, AsmLine *ln, int is_bl)
{
    if (ln->n_operands != 1 || !asm_is_reg(&ln->operands[0])) {
        asm_err_at(e, ln, "BR/BLR needs 1 reg operand");
        return;
    }
    if (is_bl) aarch64_enc_blr(e, aarch64_reg_num(ln->operands[0].reg));
    else aarch64_enc_br(e, aarch64_reg_num(ln->operands[0].reg));
}

/* ADRP Xd, _sym
 *
 * Loads the page address of `_sym` (bits 12 and up of the symbol's
 * address minus the PC's page) into Xd. Always paired with an `add Xd,
 * Xd, _sym` to get the full address. We emit ADRP with imm21=0 and
 * record an AFR_AARCH64_PAGE21 fixup so the linker patches the 21-bit
 * page-delta immediate. */
static void
aarch64_enc_adrp_dispatch(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 2 || !asm_is_reg(&ln->operands[0]) ||
            ln->operands[0].reg.cls != AR_A64_X) {
        asm_err_at(e, ln, "ADRP: need Xd, <label>");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    AsmOperand *s = &ln->operands[1];
    if (!asm_is_label(s)) {
        asm_err_at(e, ln, "ADRP: second operand must be a label");
        return;
    }
    aarch64_enc_adrp(e, aarch64_reg_num(d->reg), 0);
    AsmFixup f = { 0 };
    int local_n = asm_operand_local_num(s);
    if (local_n >= 0) {
        f.kind = AF_LOCAL;
        f.local_num = local_n;
    } else {
        f.kind = AF_SYMBOL;
        f.sym = xstrdup(s->label_name);
    }
    f.patch_offset = e->len - 4;
    f.width = 4;
    f.pcrel = 1;
    f.reloc = AFR_AARCH64_PAGE21;
    asm_add_fixup(e, f);
}

/* ================================================================ JIT
 * helpers used by src/aarch64-jit.c. Each emits a single instruction
 * word; the branch placeholders leave the displacement field zeroed so
 * the JIT can push an AsmFixup against the returned byte offset and
 * have asm_jit_finalize patch the destination in later.
 *
 * TODO: implement each body. The expected encodings are documented in
 * src/asm/enc_arm64.h. */

size_t
aarch64_jit_emit_ret(AsmEnc *e)
{
    /* RET defaults to RET LR. Single fixed-encoding word. */
    return put_word(e, 0xD65F03C0);
}

size_t
aarch64_jit_emit_bl_placeholder(AsmEnc *e)
{
    /* BL <imm26>. Emit with imm26 = 0; caller adds a CALL26 fixup. */
    return put_word(e, 0x94000000);
}

size_t
aarch64_jit_emit_b_placeholder(AsmEnc *e)
{
    /* B <imm26>. Emit with imm26 = 0; caller adds a JUMP26 fixup. */
    return put_word(e, 0x14000000);
}

size_t
aarch64_jit_emit_bcond_placeholder(AsmEnc *e, A64Cond cond)
{
    /* B.<cond> <imm19>. Emit with imm19 = 0; caller adds an IMM19
     * fixup (new AsmFixupReloc - see TODO in asm_enc.h). */
    return put_word(e, 0x54000000 | (cond & 0xF));
}

size_t
aarch64_jit_emit_stp_pre(AsmEnc *e,
                         A64Reg rt1, A64Reg rt2,
                         A64Reg rn, int32_t imm)
{
    /* STP <Xt1>, <Xt2>, [<Xn|SP>, #imm]!  -- pre-indexed, 64-bit GPR.
     * imm is the byte offset; it must be a multiple of 8 in the range
     * [-512, +504]. */
    const uint32_t prefix = 0xA9800000;
    uint32_t w = prefix |
            (((imm / 8) & 0x7fu) << 15) |
            (rt2 & 0x1F) << 10          |
            (rn & 0x1F) << 5            |
            (rt1 & 0x1F);
    return put_word(e, w);
}

size_t
aarch64_jit_emit_ldp_post(AsmEnc *e,
                          A64Reg rt1, A64Reg rt2,
                          A64Reg rn, int32_t imm)
{
    /* LDP <Xt1>, <Xt2>, [<Xn|SP>], #imm   -- post-indexed, 64-bit GPR.
     * Same imm constraints as STP pre. */
    const uint32_t prefix = 0xA8C00000;
    uint32_t w = prefix |
            (((imm / 8) & 0x7fu) << 15) |
            (rt2 & 0x1F) << 10          |
            (rn & 0x1F) << 5            |
            (rt1 & 0x1F);
    return put_word(e, w);
}

size_t
aarch64_jit_emit_trampoline(AsmEnc *e, void *target)
{
    /* 5-instruction veneer that loads a full 64-bit `target` into x16 and
     * branches to it. Used by the JIT (or any caller) to reach a callee
     * beyond a BL/B's +/-128MB rel26 range. `br x16` (not `blr`) preserves
     * LR so the original BL's return address flows through unchanged -
     * the callee's `ret` returns to the BL's successor. x16 is the
     * AAPCS-defined intra-procedure-call scratch register; it's already
     * understood as clobbered across a call. */
    uintptr_t t = (uintptr_t)target;
    size_t start = e->len;
    aarch64_enc_movz(e, A_X16, /*sf=*/1, (uint32_t)((t      ) & 0xFFFFu), 0);
    aarch64_enc_movk(e, A_X16, /*sf=*/1, (uint16_t)((t >> 16) & 0xFFFFu), 1);
    aarch64_enc_movk(e, A_X16, /*sf=*/1, (uint16_t)((t >> 32) & 0xFFFFu), 2);
    aarch64_enc_movk(e, A_X16, /*sf=*/1, (uint16_t)((t >> 48) & 0xFFFFu), 3);
    aarch64_enc_br  (e, A_X16);
    return start;
}

void
aarch64_encode_instr(AsmEnc *e, AsmLine *ln)
{
    switch ((AArch64Mn)ln->mnemonic_id) {
        case A64MN_MOV:      aarch64_enc_mov(e, ln); return;
        case A64MN_UXTB: {
            AsmOperand *rd = &ln->operands[0];
            AsmOperand *rn = &ln->operands[1];
            if (!asm_is_reg(rd) || !asm_is_reg(rn)) {
                asm_err_at(e, ln, "uxtb only takes registers");
            }
            aarch64_enc_uxtb(e, 0, aarch64_reg_num(rd->reg),
                                   aarch64_reg_num(rn->reg));
            return;
        }
        case A64MN_ADD:      aarch64_enc_add_sub(e, ln, 0); return;
        case A64MN_ADRP:     aarch64_enc_adrp_dispatch(e, ln); return;
        case A64MN_SUB:      aarch64_enc_add_sub(e, ln, 1); return;
        case A64MN_AND:
            aarch64_enc_logical_reg(e, ln, aarch64_enc_and_reg,
                    "AND: 3 reg operands");
            return;
        case A64MN_ORR:
            aarch64_enc_logical_reg(e, ln, aarch64_enc_orr_reg,
                    "ORR: 3 reg operands");
            return;
        case A64MN_EOR:
            aarch64_enc_logical_reg(e, ln, aarch64_enc_eor_reg,
                    "EOR: 3 reg operands");
            return;
        case A64MN_MUL:      aarch64_enc_mulm(e, ln); return;
        case A64MN_SDIV:     aarch64_enc_sdi(e, ln); return;
        case A64MN_LSL:      aarch64_enc_lsl(e, ln); return;
        case A64MN_NEG:      aarch64_enc_neg(e, ln); return;
        case A64MN_CMP:      aarch64_enc_cmp(e, ln); return;
        case A64MN_LDR:
            if (ln->n_operands >= 1 && aarch64_is_simd_reg(&ln->operands[0]))
                aarch64_enc_simd_ldst(e, ln, 1);
            else aarch64_enc_ldst64(e, ln, 1);
            return;
        case A64MN_STR:
            if (ln->n_operands >= 1 && aarch64_is_simd_reg(&ln->operands[0]))
                aarch64_enc_simd_ldst(e, ln, 0);
            else aarch64_enc_ldst64(e, ln, 0);
            return;
        case A64MN_LDRB:     aarch64_enc_ldst_sub(e, ln, 1, 1); return;
        case A64MN_STRB:     aarch64_enc_ldst_sub(e, ln, 0, 1); return;
        case A64MN_LDRH:     aarch64_enc_ldst_sub(e, ln, 1, 2); return;
        case A64MN_STRH:     aarch64_enc_ldst_sub(e, ln, 0, 2); return;
        case A64MN_LDRSB:    aarch64_enc_ldrs(e, ln, 1); return;
        case A64MN_LDRSH:    aarch64_enc_ldrs(e, ln, 2); return;
        case A64MN_LDRSW:    aarch64_enc_ldrs(e, ln, 4); return;
        case A64MN_LDP:      aarch64_enc_ldp_stp(e, ln, 1, 0); return;
        case A64MN_STP:      aarch64_enc_ldp_stp(e, ln, 0, 0); return;
        case A64MN_LDNP:     aarch64_enc_ldp_stp(e, ln, 1, 1); return;
        case A64MN_STNP:     aarch64_enc_ldp_stp(e, ln, 0, 1); return;
        case A64MN_SUBS:     aarch64_enc_subs(e, ln); return;
        case A64MN_CBZ:      aarch64_enc_cbz(e, ln, 0); return;
        case A64MN_CBNZ:     aarch64_enc_cbz(e, ln, 1); return;
        case A64MN_FABS:     aarch64_enc_fp1src(e, ln, 1); return;  /* opcode6=000001 */
        case A64MN_FNEG:     aarch64_enc_fp1src(e, ln, 2); return;  /* opcode6=000010 */
        case A64MN_FSQRT:    aarch64_enc_fp1src(e, ln, 3); return;  /* opcode6=000011 */
        case A64MN_FCMP:     aarch64_enc_fcmp(e, ln, 0); return;
        case A64MN_FCMPE:    aarch64_enc_fcmp(e, ln, 1); return;
        /* rmode<<3 | opcode: FCVTZS=11_000=24, FCVTZU=11_001=25,
         *                    SCVTF =00_010=2,  UCVTF=00_011=3 */
        case A64MN_FCVTZS:   aarch64_enc_fp_int_cvt(e, ln, 24, 1); return;
        case A64MN_FCVTZU:   aarch64_enc_fp_int_cvt(e, ln, 25, 1); return;
        case A64MN_SCVTF:    aarch64_enc_fp_int_cvt(e, ln, 2, 0); return;
        case A64MN_UCVTF:    aarch64_enc_fp_int_cvt(e, ln, 3, 0); return;
        case A64MN_PRINTREG: aarch64_enc_printreg(e, ln); return;
        case A64MN_B:        aarch64_enc_branch(e, ln, 0, 0, A_AL); return;
        case A64MN_BL:       aarch64_enc_branch(e, ln, 1, 0, A_AL); return;
        case A64MN_BR:       aarch64_enc_br_blr(e, ln, 0); return;
        case A64MN_BLR:      aarch64_enc_br_blr(e, ln, 1); return;
        case A64MN_RET:
            if (ln->n_operands == 0) aarch64_enc_ret(e);
            else if (ln->n_operands == 1 && asm_is_reg(&ln->operands[0]))
                aarch64_enc_ret_reg(e, aarch64_reg_num(ln->operands[0].reg));
            else asm_err_at(e, ln, "RET takes 0 or 1 reg operand");
            return;
        case A64MN_FMOV:     aarch64_enc_fmov(e, ln); return;
        case A64MN_FADD:     aarch64_enc_fp_arith(e, ln, TASM_FOP_FADD); return;
        case A64MN_FSUB:     aarch64_enc_fp_arith(e, ln, TASM_FOP_FSUB); return;
        case A64MN_FMUL:     aarch64_enc_fp_arith(e, ln, TASM_FOP_FMUL); return;
        case A64MN_FDIV:     aarch64_enc_fp_arith(e, ln, TASM_FOP_FDIV); return;
        case A64MN_BFDOT:    aarch64_enc_bfdot(e, ln); return;
        case A64MN_BFMLALB:  aarch64_enc_bfmlal(e, ln, 0); return;
        case A64MN_BFMLALT:  aarch64_enc_bfmlal(e, ln, 1); return;
        case A64MN_BFMMLA:   aarch64_enc_bfmmla(e, ln); return;
        case A64MN_BFCVT:    aarch64_enc_bfcvt_scalar(e, ln); return;
        case A64MN_BFCVTN:   aarch64_enc_bfcvtn(e, ln, 0); return;
        case A64MN_BFCVTN2:  aarch64_enc_bfcvtn(e, ln, 1); return;
        case A64MN_NOP:
            /* NOP on arm64 is 0xd503201f. */
            put_byte(e, 0x1f); put_byte(e, 0x20);
            put_byte(e, 0x03); put_byte(e, 0xd5);
            return;
        case A64MN_DSB:      aarch64_enc_barrier(e, ln, 4); return;
        case A64MN_DMB:      aarch64_enc_barrier(e, ln, 5); return;
        case A64MN_ISB:      aarch64_enc_barrier(e, ln, 6); return;
        case A64MN_MRS:      aarch64_enc_mrs(e, ln); return;
        case A64MN_MSR:      aarch64_enc_msr(e, ln); return;
        case A64MN_MOVN:     aarch64_enc_mov_wide_mn(e, ln, 0x0); return;
        case A64MN_MOVZ:     aarch64_enc_mov_wide_mn(e, ln, 0x2); return;
        case A64MN_MOVK:     aarch64_enc_mov_wide_mn(e, ln, 0x3); return;
        case A64MN_LDUR:     aarch64_enc_ldst_unscaled_mn(e, ln, 1); return;
        case A64MN_STUR:     aarch64_enc_ldst_unscaled_mn(e, ln, 0); return;
        case A64MN_UNKNOWN:  break; /* fall through to b.cond / error */
    }

    /* The `b.<cc>` family isn't in the mnemonic table - the cc suffix
     * makes 16 variants; recognise them here by parsing the suffix off
     * the mnemonic text. */
    A64Cond cc;
    const char *m = ln->mnemonic ? ln->mnemonic : "";
    if (aarch64_parse_cond_suffix(m, &cc)) {
        aarch64_enc_branch(e, ln, 0, 1, cc);
        return;
    }
    asm_err_at(e, ln, "unsupported arm64 mnemonic");
}

/* ---------- entry ---------- */

int
asm_encode_arm64(AsmBlock *blk, AsmResolver *r, AsmEnc *out)
{
    out->src_file = blk ? blk->src_file : NULL;
    if (!blk) return 0;
    (void)r; /* we don't expose class.field resolution in arm64 yet */
    out->line_offsets = xmalloc(sizeof(size_t) * (size_t)(blk->n_lines + 1));
    out->n_line_offsets = blk->n_lines + 1;

    for (int i = 0; i < blk->n_lines; i++) {
        out->line_offsets[i] = out->len;
        AsmLine *ln = &blk->lines[i];
        switch (ln->kind) {
            case AINS_LABEL_LOCAL:
                if (ln->label_name) asm_define_label(out, -1, ln->label_name);
                else asm_define_label(out, ln->local_num, NULL);
                break;
            case AINS_LABEL_PUBLIC: asm_define_label(out, -1, ln->label_name); break;
            case AINS_INSTR: aarch64_encode_instr(out, ln); break;
            case AINS_DIRECTIVE: asm_enc_directive(out, ln); break;
            default: break;
        }
    }
    out->line_offsets[blk->n_lines] = out->len;

    aarch64_resolve_local_fixups(out);
    return out->errors;
}

/* Patch every AF_LOCAL fixup against the corresponding label_num in
 * `e->labels`, and drop the resolved entries from `e->fixups`. PAGE21
 * and PAGEOFF12 are left in place since their page-delta depends on
 * the runtime base address (resolved later by the JIT or linker).
 *
 * Public so consumers that emit bytes outside `asm_encode_arm64` can
 * still get encode-time local resolution. */
void
aarch64_resolve_local_fixups(AsmEnc *out)
{
    if (!out || out->n_fixups == 0) return;
    int resolved = 0;
    AsmFixup *kept = xmalloc(sizeof(AsmFixup) * (size_t)out->n_fixups);
    for (int i = 0; i < out->n_fixups; i++) {
        AsmFixup *f = &out->fixups[i];
        if (f->kind != AF_LOCAL) {
            kept[resolved++] = *f;
            continue;
        }
        if (f->reloc == AFR_AARCH64_PAGE21 ||
                f->reloc == AFR_AARCH64_PAGEOFF12) {
            kept[resolved++] = *f;
            continue;
        }
        size_t target_off = 0;
        int found = 0;
        for (int j = 0; j < out->n_labels; j++) {
            if (out->labels[j].local_num == f->local_num) {
                target_off = out->labels[j].byte_offset;
                found = 1;
                break;
            }
        }
        if (!found) {
            char *err = asm_tmp_printf("%s: arm64 asm: unresolved local label @@%d\n",
                    out->src_file ? out->src_file : "?", f->local_num);
            asm_err_at(out, NULL, err);
            continue;
        }
        int32_t rel_words = (int32_t)((int64_t)target_off -
                                    (int64_t)f->patch_offset) /
                4;
        if (f->width == 19)
            aarch64_enc_patch_imm19(out, f->patch_offset, rel_words);
        else aarch64_enc_patch_branch26(out, f->patch_offset, rel_words);
    }
    free(out->fixups);
    out->fixups = kept;
    out->n_fixups = resolved;
}

/* ---------------- mnemonic table ----------------
 * Sorted alphabetically for binary search. Update both this table and
 * the AArch64Mn enum in enc_arm64.h together. */
static const TasmMnemonicEntry aarch64_mnemonic_entries[] = {
    { "add",      A64MN_ADD },
    { "adrp",     A64MN_ADRP },
    { "and",      A64MN_AND },
    { "b",        A64MN_B },
    { "bfcvt",    A64MN_BFCVT },
    { "bfcvtn",   A64MN_BFCVTN },
    { "bfcvtn2",  A64MN_BFCVTN2 },
    { "bfdot",    A64MN_BFDOT },
    { "bfmlalb",  A64MN_BFMLALB },
    { "bfmlalt",  A64MN_BFMLALT },
    { "bfmmla",   A64MN_BFMMLA },
    { "bl",       A64MN_BL },
    { "blr",      A64MN_BLR },
    { "br",       A64MN_BR },
    /* TempleOS-flavoured asm spells calls the x86 way; accept `call` as
     * an alias for `bl` so the same HolyC source assembles on both
     * arches. (`ret` is already a real aarch64 mnemonic.) */
    { "call",     A64MN_BL },
    { "cbnz",     A64MN_CBNZ },
    { "cbz",      A64MN_CBZ },
    { "cmp",      A64MN_CMP },
    { "dmb",      A64MN_DMB },
    { "dsb",      A64MN_DSB },
    { "eor",      A64MN_EOR },
    { "fabs",     A64MN_FABS },
    { "fadd",     A64MN_FADD },
    { "fcmp",     A64MN_FCMP },
    { "fcmpe",    A64MN_FCMPE },
    { "fcvtzs",   A64MN_FCVTZS },
    { "fcvtzu",   A64MN_FCVTZU },
    { "fdiv",     A64MN_FDIV },
    { "fmov",     A64MN_FMOV },
    { "fmul",     A64MN_FMUL },
    { "fneg",     A64MN_FNEG },
    { "fsqrt",    A64MN_FSQRT },
    { "fsub",     A64MN_FSUB },
    { "isb",      A64MN_ISB },
    { "ldnp",     A64MN_LDNP },
    { "ldp",      A64MN_LDP },
    { "ldr",      A64MN_LDR },
    { "ldrb",     A64MN_LDRB },
    { "ldrh",     A64MN_LDRH },
    { "ldrsb",    A64MN_LDRSB },
    { "ldrsh",    A64MN_LDRSH },
    { "ldrsw",    A64MN_LDRSW },
    { "ldur",     A64MN_LDUR },
    { "lsl",      A64MN_LSL },
    { "mov",      A64MN_MOV },
    { "movk",     A64MN_MOVK },
    { "movn",     A64MN_MOVN },
    { "movz",     A64MN_MOVZ },
    { "mrs",      A64MN_MRS },
    { "msr",      A64MN_MSR },
    { "mul",      A64MN_MUL },
    { "neg",      A64MN_NEG },
    { "nop",      A64MN_NOP },
    { "orr",      A64MN_ORR },
    { "printreg", A64MN_PRINTREG },
    { "ret",      A64MN_RET },
    { "scvtf",    A64MN_SCVTF },
    { "sdiv",     A64MN_SDIV },
    { "stnp",     A64MN_STNP },
    { "stp",      A64MN_STP },
    { "str",      A64MN_STR },
    { "strb",     A64MN_STRB },
    { "strh",     A64MN_STRH },
    { "stur",     A64MN_STUR },
    { "sub",      A64MN_SUB },
    { "subs",     A64MN_SUBS },
    { "ucvtf",    A64MN_UCVTF },
};

static const TasmMnemonicTable aarch64_mnemonic_table_v = {
    aarch64_mnemonic_entries,
    (int)(sizeof aarch64_mnemonic_entries / sizeof aarch64_mnemonic_entries[0]),
};

const TasmMnemonicTable *
aarch64_mnemonic_table(void)
{
    return &aarch64_mnemonic_table_v;
}
