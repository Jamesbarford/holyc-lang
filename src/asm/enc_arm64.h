#ifndef HCC_ENC_ARM64_H
#define HCC_ENC_ARM64_H

#include "asm.h"
#include "asm_enc.h"

/* AArch64 mnemonic ids. Stashed on AsmLine->mnemonic_id at parse time
 * (via the table returned by aarch64_mnemonic_table()), so the encoder
 * can dispatch with a switch instead of a chain of strcmp. 0 is reserved
 * for `unknown` (matches TASM_MN_UNKNOWN). */
typedef enum {
    A64MN_UNKNOWN = 0,
    A64MN_ADD,
    A64MN_ADRP,
    A64MN_AND,
    A64MN_B,
    A64MN_BFCVT,
    A64MN_BFCVTN,
    A64MN_BFCVTN2,
    A64MN_BFDOT,
    A64MN_BFMLALB,
    A64MN_BFMLALT,
    A64MN_BFMMLA,
    A64MN_BL,
    A64MN_BLR,
    A64MN_BNE,
    A64MN_BR,
    A64MN_CBNZ,
    A64MN_CBZ,
    A64MN_CSEL,
    A64MN_CMP,
    A64MN_DMB,
    A64MN_DSB,
    A64MN_EOR,
    A64MN_FABS,
    A64MN_FADD,
    A64MN_FCMP,
    A64MN_FCMPE,
    A64MN_FCVTZS,
    A64MN_FCVTZU,
    A64MN_FDIV,
    A64MN_FMOV,
    A64MN_FMUL,
    A64MN_FNEG,
    A64MN_FSQRT,
    A64MN_FSUB,
    A64MN_ISB,
    A64MN_LDNP,
    A64MN_LDP,
    A64MN_LDR,
    A64MN_LDRB,
    A64MN_LDRH,
    A64MN_LDRSB,
    A64MN_LDRSH,
    A64MN_LDRSW,
    A64MN_LDUR,
    A64MN_LSL,
    A64MN_MOV,
    A64MN_MOVK,
    A64MN_MOVN,
    A64MN_MOVZ,
    A64MN_MRS,
    A64MN_MSR,
    A64MN_MUL,
    A64MN_NEG,
    A64MN_NOP,
    A64MN_ORR,
    A64MN_PRINTREG,
    A64MN_RET,
    A64MN_SCVTF,
    A64MN_SDIV,
    A64MN_STNP,
    A64MN_STP,
    A64MN_STR,
    A64MN_STRB,
    A64MN_STRH,
    A64MN_STUR,
    A64MN_SUB,
    A64MN_SUBS,
    A64MN_SXTB,
    A64MN_SXTH,
    A64MN_SXTW,
    A64MN_UCVTF,
    A64MN_UXTB,
} AArch64Mn;

/* Register numbers 0..31. 31 means XZR in most contexts and SP in a few
 * (encoded the same; semantics depend on the instruction). */
typedef enum {
    A_X0 = 0,
    A_X1 = 1,
    A_X2 = 2,
    A_X3 = 3,
    A_X4 = 4,
    A_X5 = 5,
    A_X6 = 6,
    A_X7 = 7,
    A_X8 = 8,
    A_X9 = 9,
    A_X10 = 10,
    A_X11 = 11,
    A_X12 = 12,
    A_X13 = 13,
    A_X14 = 14,
    A_X15 = 15,
    A_X16 = 16,
    A_X17 = 17,
    A_X18 = 18,
    A_X19 = 19,
    A_X20 = 20,
    A_X21 = 21,
    A_X22 = 22,
    A_X23 = 23,
    A_X24 = 24,
    A_X25 = 25,
    A_X26 = 26,
    A_X27 = 27,
    A_X28 = 28,
    A_FP = 29,
    A_LR = 30,
    A_SP = 31,
    A_XZR = 31,
} A64Reg;

/* AArch64 condition codes for B.cond / CSET / CSEL. Values match the
 * 4-bit field used in encodings. */
typedef enum {
    A_EQ = 0x0,
    A_NE = 0x1,
    A_CS = 0x2,
    A_CC = 0x3,
    A_MI = 0x4,
    A_PL = 0x5,
    A_VS = 0x6,
    A_VC = 0x7,
    A_HI = 0x8,
    A_LS = 0x9,
    A_GE = 0xA,
    A_LT = 0xB,
    A_GT = 0xC,
    A_LE = 0xD,
    A_AL = 0xE,
} A64Cond;

typedef enum {
    TASM_FOP_FADD,
    TASM_FOP_FSUB,
    TASM_FOP_FMUL,
    TASM_FOP_FDIV
} TasmFOp;

/* ---------------- instruction encoders ----------------
 * Each helper appends one instruction's bytes to `e` and returns the
 * byte offset at which it was placed (so callers can record fixups). */

void aarch64_enc_mov_imm64(AsmEnc *e, A64Reg rd, uint64_t imm);
void aarch64_enc_mov_imm32(AsmEnc *e, A64Reg rd, uint64_t imm);
size_t aarch64_enc_mov_reg(AsmEnc *e, A64Reg rd, A64Reg rm);
size_t aarch64_enc_mov_reg_w(AsmEnc *e, A64Reg rd, A64Reg rm);
size_t aarch64_enc_movn(AsmEnc *e, A64Reg rd, int sf, uint16_t imm16, int hw);
size_t aarch64_enc_add_imm(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, uint32_t imm12);
size_t aarch64_enc_sub_imm(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, uint32_t imm12);
size_t aarch64_enc_add_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_sub_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_add_reg_shifted(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm, int shift_type, int shift_amount);
size_t aarch64_enc_sub_reg_shifted(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm, int shift_type, int shift_amount);
size_t aarch64_enc_uxtb(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn);
size_t aarch64_enc_uxth(AsmEnc *e, A64Reg rd, A64Reg rn);
size_t aarch64_enc_sxtb(AsmEnc *e, A64Reg rd, A64Reg rn);
size_t aarch64_enc_sxth(AsmEnc *e, A64Reg rd, A64Reg rn);
size_t aarch64_enc_sxtw(AsmEnc *e, A64Reg rd, A64Reg rn);
size_t aarch64_enc_ldr_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_str_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_ldr32_imm_gpr(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_str32_imm_gpr(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_fp_2src_scalar(AsmEnc *e, int is_double, int opcode4, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_fneg_scalar(AsmEnc *e, int is_double, A64Reg rd, A64Reg rn);
size_t aarch64_enc_fcmp_reg2(AsmEnc *e, int is_double, A64Reg rn, A64Reg rm);
size_t aarch64_enc_fcmp_zero(AsmEnc *e, int is_double, A64Reg rn);
size_t aarch64_enc_fcvt_narrow(AsmEnc *e, A64Reg rd, A64Reg rn);
size_t aarch64_enc_fcvt_widen(AsmEnc *e, A64Reg rd, A64Reg rn);
size_t aarch64_enc_fcvtzs(AsmEnc *e, int is_64, int fp_is_double, A64Reg rd, A64Reg rn);
size_t aarch64_enc_fcvtzu(AsmEnc *e, int is_64, int fp_is_double, A64Reg rd, A64Reg rn);
size_t aarch64_enc_scvtf(AsmEnc *e, int is_64, int fp_is_double, A64Reg rd, A64Reg rn);
size_t aarch64_enc_ucvtf(AsmEnc *e, int is_64, int fp_is_double, A64Reg rd, A64Reg rn);
size_t aarch64_enc_fp_3same_vec(AsmEnc *e, int Q, int U, int bit23, int sz, int opcode5, A64Reg rd, A64Reg rn, A64Reg rm);
int aarch64_enc_ldst_pair(AsmEnc *e, int is_simd, int is_load, int width, int mode, A64Reg rt, A64Reg rt2, A64Reg rn, int32_t imm_bytes);
size_t aarch64_enc_subs_imm(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, uint32_t imm12);
size_t aarch64_enc_subs_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm, int shift_type, int shift_amount);
size_t aarch64_enc_ldst_post(AsmEnc *e, int is_simd, int is_load, int width, A64Reg rt, A64Reg rn, int32_t simm9);
size_t aarch64_enc_ldst_pre(AsmEnc *e, int is_simd, int is_load, int width, A64Reg rt, A64Reg rn, int32_t simm9);
size_t aarch64_enc_ldst_regoff(AsmEnc *e, int is_simd, int is_load, int width, int scaled, A64Reg rt, A64Reg rn, A64Reg rm);
size_t aarch64_enc_bfdot_vec(AsmEnc *e, int Q, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_bfdot_idx(AsmEnc *e, int Q, A64Reg rd, A64Reg rn, A64Reg rm, int idx);
size_t aarch64_enc_bfmlalb(AsmEnc *e, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_bfmlalt(AsmEnc *e, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_bfcvt(AsmEnc *e, A64Reg rd, A64Reg rn);
size_t aarch64_enc_fmov_reg(AsmEnc *e, int is_double, A64Reg rd, A64Reg rn);
size_t aarch64_enc_fmov_gpr(AsmEnc *e, int is_double, int fp_to_gpr, A64Reg rd, A64Reg rn);
size_t aarch64_enc_fmov_reg_h(AsmEnc *e, A64Reg rd, A64Reg rn);
size_t aarch64_enc_fmov_h_gpr(AsmEnc *e, int fp_to_gpr, A64Reg rd, A64Reg rn);
size_t aarch64_enc_fmov_imm(AsmEnc *e, int is_double, A64Reg rd, uint8_t imm8);
size_t aarch64_enc_fp_ldst_imm(AsmEnc *e, int is_load, int width, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_stp_pre(AsmEnc *e, A64Reg rt1, A64Reg rt2, A64Reg rn, int32_t simm);
size_t aarch64_enc_ldp_post(AsmEnc *e, A64Reg rt1, A64Reg rt2, A64Reg rn, int32_t simm);
size_t aarch64_enc_ret_reg(AsmEnc *e, A64Reg rn);size_t aarch64_enc_ret(AsmEnc *e);
size_t aarch64_enc_b(AsmEnc *e, int32_t rel_words);
size_t aarch64_enc_bl(AsmEnc *e, int32_t rel_words);
size_t aarch64_enc_br(AsmEnc *e, A64Reg rn);
size_t aarch64_enc_blr(AsmEnc *e, A64Reg rn);
size_t aarch64_enc_b_cond(AsmEnc *e, A64Cond cc, int32_t rel_words);
size_t aarch64_enc_adrp(AsmEnc *e, A64Reg rd, int32_t pages);
void aarch64_resolve_local_fixups(AsmEnc *e);
void aarch64_enc_patch_branch26(AsmEnc *e, size_t patch, int32_t rel_words);
void aarch64_enc_patch_adrp_imm21(AsmEnc *e, size_t patch, int32_t page_delta);
void aarch64_enc_patch_imm12(AsmEnc *e, size_t patch, uint32_t imm12);
void aarch64_enc_patch_imm19(AsmEnc *e, size_t patch, int32_t rel_words);
size_t aarch64_enc_ldrb_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_strb_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_ldrh_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_strh_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_ldr32_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_str32_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_ldrsb_imm(AsmEnc *e, int dst_64, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_ldrsh_imm(AsmEnc *e, int dst_64, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_ldrsw_imm(AsmEnc *e, A64Reg rt, A64Reg rn, uint32_t off);
size_t aarch64_enc_ldur(AsmEnc *e, int width, A64Reg rt, A64Reg rn, int32_t simm9);
size_t aarch64_enc_stur(AsmEnc *e, int width, A64Reg rt, A64Reg rn, int32_t simm9);
size_t aarch64_enc_and_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_orr_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_eor_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_mul(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_msub(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm, A64Reg ra);
size_t aarch64_enc_sdiv(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_udiv(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_lslv(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_lsl_imm(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, uint32_t shift);
size_t aarch64_enc_lsrv(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_lsr_imm(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, uint32_t shift);
size_t aarch64_enc_asrv(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, A64Reg rm);
size_t aarch64_enc_asr_imm(AsmEnc *e, int is_64, A64Reg rd, A64Reg rn, uint32_t shift);
size_t aarch64_enc_neg_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rm);
size_t aarch64_enc_mvn_reg(AsmEnc *e, int is_64, A64Reg rd, A64Reg rm);
size_t aarch64_enc_cmp_reg(AsmEnc *e, int is_64, A64Reg rn, A64Reg rm);
size_t aarch64_enc_cmp_imm(AsmEnc *e, int is_64, A64Reg rn, uint32_t imm);
size_t aarch64_enc_tst_reg(AsmEnc *e, int is_64, A64Reg rn, A64Reg rm);
size_t aarch64_enc_cset(AsmEnc *e, A64Reg rd, A64Cond cc);
size_t aarch64_enc_msr_reg(AsmEnc *e, uint16_t sysreg16, A64Reg rt);
size_t aarch64_enc_msr_imm(AsmEnc *e, uint32_t op1, uint32_t op2, uint32_t imm4);
void aarch64_enc_mov(AsmEnc *e, AsmLine *ln);
void aarch64_enc_add_sub(AsmEnc *e, AsmLine *ln, int is_sub);
void aarch64_enc_logical_reg(AsmEnc *e, AsmLine *ln, size_t (*emit_cb)(AsmEnc *, int, A64Reg, A64Reg, A64Reg), const char *msg);
void aarch64_enc_mulm(AsmEnc *e, AsmLine *ln);
void aarch64_enc_sdi(AsmEnc *e, AsmLine *ln);
void aarch64_enc_lsl(AsmEnc *e, AsmLine *ln);
void aarch64_enc_neg(AsmEnc *e, AsmLine *ln);
void aarch64_enc_cmp(AsmEnc *e, AsmLine *ln);
void aarch64_enc_ldst64(AsmEnc *e, AsmLine *ln, int is_load);
void aarch64_enc_ldst_sub(AsmEnc *e, AsmLine *ln, int is_load, int width);
void aarch64_enc_ldrs(AsmEnc *e, AsmLine *ln, int width);
void aarch64_enc_fp_arith(AsmEnc *e, AsmLine *ln, TasmFOp op);
void aarch64_enc_printreg(AsmEnc *e, AsmLine *ln);
void aarch64_enc_bfdot(AsmEnc *e, AsmLine *ln);
void aarch64_enc_bfmlal(AsmEnc *e, AsmLine *ln, int is_top);
void aarch64_enc_bfmmla(AsmEnc *e, AsmLine *ln);
void aarch64_enc_bfcvt_scalar(AsmEnc *e, AsmLine *ln);
void aarch64_enc_bfcvtn(AsmEnc *e, AsmLine *ln, int is_top);
void aarch64_enc_fmov(AsmEnc *e, AsmLine *ln);
void aarch64_enc_ldp_stp(AsmEnc *e, AsmLine *ln, int is_load, int nontemporal);
void aarch64_enc_cbz(AsmEnc *e, AsmLine *ln, int is_nonzero);
void aarch64_enc_fp1src(AsmEnc *e, AsmLine *ln, int opcode6);
void aarch64_enc_fcmp(AsmEnc *e, AsmLine *ln, int with_exceptions);
void aarch64_enc_fp_int_cvt(AsmEnc *e, AsmLine *ln, int rmode_opcode5, int fp_to_int);
void aarch64_enc_subs(AsmEnc *e, AsmLine *ln);
void aarch64_enc_simd_ldst(AsmEnc *e, AsmLine *ln, int is_load);
void aarch64_enc_branch(AsmEnc *e, AsmLine *ln, int is_bl, int is_cond, A64Cond cc);
void aarch64_enc_barrier(AsmEnc *e, AsmLine *ln, int opc2);
void aarch64_enc_mrs(AsmEnc *e, AsmLine *ln);
void aarch64_enc_msr(AsmEnc *e, AsmLine *ln);
void aarch64_enc_mov_wide_mn(AsmEnc *e, AsmLine *ln, uint32_t opc);
void aarch64_enc_ldst_unscaled_mn(AsmEnc *e, AsmLine *ln, int is_load);
void aarch64_enc_br_blr(AsmEnc *e, AsmLine *ln, int is_bl);

/* Top level */
void aarch64_encode_instr(AsmEnc *e, AsmLine *ln);







/* ---------------- branch/return + frame helpers ----------------
 * Emit the bytes with the branch displacement field zeroed; the caller
 * is expected to push an AsmFixup against the returned offset so the
 * displacement gets patched later (either by asm_jit_finalize or by the
 * codegen layer turning it into a linker relocation).
 *
 * TODO: implement these in enc_arm64.c. The encoding bit-patterns:
 *   ret           D65F03C0
 *   bl  #0        94000000
 *   b   #0        14000000
 *   b.cond #0     54000000 | cond
 *   stp pre       (LDP_STP_imm encoding, idx=pre, L=0)
 *   ldp post      (LDP_STP_imm encoding, idx=post, L=1) */
size_t aarch64_jit_emit_ret(AsmEnc *e);
size_t aarch64_jit_emit_bl_placeholder(AsmEnc *e);
size_t aarch64_jit_emit_b_placeholder(AsmEnc *e);
size_t aarch64_jit_emit_bcond_placeholder(AsmEnc *e, A64Cond cond);
size_t aarch64_jit_emit_stp_pre(AsmEnc *e,
                                A64Reg rt1,
                                A64Reg rt2,
                                A64Reg rn,
                                int32_t imm);
size_t aarch64_jit_emit_ldp_post(AsmEnc *e,
                                 A64Reg rt1,
                                 A64Reg rt2,
                                 A64Reg rn,
                                 int32_t imm);

/* Emit a 5-instruction veneer that loads `target` into x16 then branches
 * to it, used to reach symbols outside BL's +/-128MB range:
 *   movz x16, #target[15:0]
 *   movk x16, #target[31:16], lsl #16
 *   movk x16, #target[47:32], lsl #32
 *   movk x16, #target[63:48], lsl #48
 *   br   x16
 * Returns the byte offset where the veneer starts so callers can record
 * a label and point BLs at it. */
size_t aarch64_jit_emit_trampoline(AsmEnc *e, void *target);
int asm_encode_arm64(AsmBlock *blk, AsmResolver *r, AsmEnc *out);

#endif
