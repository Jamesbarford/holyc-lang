#ifndef ENC_X86_64_H__
#define ENC_X86_64_H__

#include "asm.h"
#include "asm_enc.h"

int asm_encode_x86_64(AsmBlock *blk, AsmResolver *r, AsmEnc *out);

/* Fallback resolution for AT&T-style width-suffixed spellings that
 * TempleOS-flavoured code mixes in (`movq`, `pushq`, `movb`, plus the
 * movz/movs src+dst forms `movzbq`/`movsbl`/...). Returns the base
 * mnemonic id (or TASM_MN_UNKNOWN) and writes the operand width the
 * suffix implies (0 if none) to `*width_out`. */
int x86_64_suffixed_mnemonic(const char *m, int *width_out);

/* Register numbering matches the x86 hardware encoding (0..15). The high
 * eight registers (R8..R15) require REX.B / REX.R / REX.X bits when used
 * in ModR/M or SIB fields. XMM registers use the same 0..15 numbering in
 * ModR/M, so the direct-emit SSE helpers below take a plain int. */
typedef enum {
    R_RAX = 0,
    R_RCX = 1,
    R_RDX = 2,
    R_RBX = 3,
    R_RSP = 4,
    R_RBP = 5,
    R_RSI = 6,
    R_RDI = 7,
    R_R8 = 8,
    R_R9 = 9,
    R_R10 = 10,
    R_R11 = 11,
    R_R12 = 12,
    R_R13 = 13,
    R_R14 = 14,
    R_R15 = 15,
} X86Reg;

/* x86 condition-code suffixes for SETcc (0F 9x) and Jcc (0F 8x). */
typedef enum {
    X86_CC_O  = 0x0, X86_CC_NO = 0x1,
    X86_CC_B  = 0x2, X86_CC_AE = 0x3,
    X86_CC_E  = 0x4, X86_CC_NE = 0x5,
    X86_CC_BE = 0x6, X86_CC_A  = 0x7,
    X86_CC_S  = 0x8, X86_CC_NS = 0x9,
    X86_CC_P  = 0xA, X86_CC_NP = 0xB,
    X86_CC_L  = 0xC, X86_CC_GE = 0xD,
    X86_CC_LE = 0xE, X86_CC_G  = 0xF,
} X86Cc;

/* ---- direct-emit API (used by the x86_64 JIT) ----
 * Each helper writes the instruction bytes straight into the AsmEnc.
 * 64-bit operand size unless stated otherwise. The rel32 emitters
 * return the byte offset of their displacement field so the caller
 * can attach an AsmFixup. */
void x86_64_enc_mov_reg_reg(AsmEnc *e, X86Reg dst, X86Reg src);
void x86_64_enc_mov32_reg_reg(AsmEnc *e, X86Reg dst, X86Reg src);
void x86_64_enc_movabsq_imm_reg(AsmEnc *e, X86Reg dst, uint64_t imm);
void x86_64_enc_xor32_reg(AsmEnc *e, X86Reg reg);
void x86_64_enc_mov_al_imm8(AsmEnc *e, uint8_t v);
void x86_64_enc_movzx_reg_reg(AsmEnc *e, int width, X86Reg dst, X86Reg src);
void x86_64_enc_movsx_reg_reg(AsmEnc *e, int width, X86Reg dst, X86Reg src);

/* Memory forms take a full [base + idx*scale + disp] operand; pass
 * idx < 0 for no index. Loads are width-aware with the same extension
 * rules as the AOT backend: 1/2 zero-extend, 4 sign-extends (movslq),
 * 8 plain movq. */
void x86_64_enc_load_mem(AsmEnc *e, int width, X86Reg dst, X86Reg base, int idx, int scale, int32_t disp);
void x86_64_enc_store_mem(AsmEnc *e, int width, X86Reg src, X86Reg base, int idx, int scale, int32_t disp);
void x86_64_enc_lea_mem(AsmEnc *e, X86Reg dst, X86Reg base, int idx, int scale, int32_t disp);
size_t x86_64_enc_lea_rip_rel_reg(AsmEnc *e, X86Reg dst);

/* ALU: op selects via the AT&T operator char: '+' '-' '&' '|' '^'. */
void x86_64_enc_alu_reg_reg(AsmEnc *e, int op, X86Reg dst, X86Reg src);
void x86_64_enc_alu_imm_reg(AsmEnc *e, int op, X86Reg dst, int32_t imm);
void x86_64_enc_addq_imm_reg(AsmEnc *e, X86Reg dst, int32_t imm);
void x86_64_enc_subq_imm_reg(AsmEnc *e, X86Reg dst, int32_t imm);
void x86_64_enc_neg_reg(AsmEnc *e, X86Reg reg);
void x86_64_enc_not_reg(AsmEnc *e, X86Reg reg);
void x86_64_enc_imul_reg_reg(AsmEnc *e, X86Reg dst, X86Reg src);
void x86_64_enc_idivq_reg(AsmEnc *e, X86Reg src);
void x86_64_enc_divq_reg(AsmEnc *e, X86Reg src);
void x86_64_enc_cqto(AsmEnc *e);

/* Shifts: kind is the /digit of the D3/C1 group: 4=SHL, 5=SHR, 7=SAR. */
void x86_64_enc_shift_cl_reg(AsmEnc *e, int kind, X86Reg dst);
void x86_64_enc_shift_imm_reg(AsmEnc *e, int kind, X86Reg dst, uint8_t imm);

void x86_64_enc_cmp_reg_reg(AsmEnc *e, X86Reg lhs, X86Reg rhs);
void x86_64_enc_cmpq_imm_reg(AsmEnc *e, X86Reg lhs, int32_t imm);
void x86_64_enc_test_reg_reg(AsmEnc *e, X86Reg a, X86Reg c);
void x86_64_enc_setcc_al(AsmEnc *e, int cc);
void x86_64_enc_movzbq_al_rax(AsmEnc *e);

void x86_64_enc_push_reg(AsmEnc *e, X86Reg reg);
void x86_64_enc_pop_reg(AsmEnc *e, X86Reg reg);
void x86_64_enc_pushq_rbp(AsmEnc *e);
void x86_64_enc_popq_rbp(AsmEnc *e);
void x86_64_enc_mov_rsp_rbp(AsmEnc *e); /* movq %rsp, %rbp */
void x86_64_enc_mov_rbp_rsp(AsmEnc *e); /* movq %rbp, %rsp */
void x86_64_enc_retq(AsmEnc *e);
size_t x86_64_enc_jmp_rel32(AsmEnc *e);
size_t x86_64_enc_jcc_rel32(AsmEnc *e, int cc);
size_t x86_64_enc_call_rel32(AsmEnc *e);
void x86_64_enc_call_reg(AsmEnc *e, X86Reg target);

/* SSE2 scalar-double helpers. xmm args are plain 0..15 indices. */
void x86_64_enc_sse_arith(AsmEnc *e, uint8_t opcode, int dst_xmm, int src_xmm);
#define X86_SSE_ADDSD 0x58
#define X86_SSE_MULSD 0x59
#define X86_SSE_SUBSD 0x5C
#define X86_SSE_DIVSD 0x5E
void x86_64_enc_movsd_xmm_xmm(AsmEnc *e, int dst_xmm, int src_xmm);
void x86_64_enc_movsd_load(AsmEnc *e, int xmm, X86Reg base, int idx, int scale, int32_t disp);
void x86_64_enc_movsd_store(AsmEnc *e, int xmm, X86Reg base, int idx, int scale, int32_t disp);
void x86_64_enc_movq_gpr_xmm(AsmEnc *e, int xmm, X86Reg gpr); /* movq %gpr, %xmm */
void x86_64_enc_movq_xmm_gpr(AsmEnc *e, X86Reg gpr, int xmm); /* movq %xmm, %gpr */
void x86_64_enc_ucomisd(AsmEnc *e, int a_xmm, int b_xmm);     /* flags = a cmp b */
void x86_64_enc_xorpd(AsmEnc *e, int dst_xmm, int src_xmm);
void x86_64_enc_cvttsd2si(AsmEnc *e, X86Reg dst, int src_xmm);
void x86_64_enc_cvtsi2sd(AsmEnc *e, int dst_xmm, X86Reg src);
void x86_64_enc_cvtsd2ss(AsmEnc *e, int dst_xmm, int src_xmm);
void x86_64_enc_cvtss2sd(AsmEnc *e, int dst_xmm, int src_xmm);

/* SSE scalar-single (F32) helpers. Same opcode bytes as the *sd forms;
 * the single-precision variants differ only in the mandatory prefix. */
void x86_64_enc_sse_arith_ss(AsmEnc *e, uint8_t opcode, int dst_xmm, int src_xmm);
void x86_64_enc_movss_xmm_xmm(AsmEnc *e, int dst_xmm, int src_xmm);
void x86_64_enc_movss_load(AsmEnc *e, int xmm, X86Reg base, int idx, int scale, int32_t disp);
void x86_64_enc_movss_store(AsmEnc *e, int xmm, X86Reg base, int idx, int scale, int32_t disp);
void x86_64_enc_movd_gpr_xmm(AsmEnc *e, int xmm, X86Reg gpr); /* movd %gpr, %xmm (32-bit) */
void x86_64_enc_movd_xmm_gpr(AsmEnc *e, X86Reg gpr, int xmm); /* movd %xmm, %gpr (32-bit) */
void x86_64_enc_ucomiss(AsmEnc *e, int a_xmm, int b_xmm);     /* flags = a cmp b */
void x86_64_enc_xorps(AsmEnc *e, int dst_xmm, int src_xmm);
void x86_64_enc_cvttss2si(AsmEnc *e, X86Reg dst, int src_xmm);
void x86_64_enc_cvtsi2ss(AsmEnc *e, int dst_xmm, X86Reg src);

/* x86_64 mnemonic ids. Stashed on AsmLine->mnemonic_id at parse time
 * (via the table returned by x86_64_mnemonic_table()). Aliases like
 * `jz`/`je`, `sal`/`shl`, `cqo`/`cqto`, `wait`/`fwait`, `fnstsw`/`fstsw`
 * collapse to a single id so the encoder only needs one switch case
 * each. 0 is reserved for UNKNOWN (matches TASM_MN_UNKNOWN). */
typedef enum {
    X86MN_UNKNOWN = 0,

    /* Integer ALU + flow */
    X86MN_ADC, X86MN_ADD, X86MN_AND, X86MN_CALL, X86MN_CMP, X86MN_DEC,
    X86MN_INC, X86MN_LEA, X86MN_LEAVE, X86MN_MOV, X86MN_MOVABS,
    X86MN_NEG, X86MN_NOP, X86MN_NOT,
    X86MN_OR, X86MN_POP, X86MN_PUSH, X86MN_RDTSC, X86MN_REP_MOVSB,
    X86MN_RET, X86MN_RET1, X86MN_RETN, X86MN_SAR, X86MN_SBB, X86MN_SHL,
    X86MN_SHR, X86MN_SUB, X86MN_SYSCALL, X86MN_TEST, X86MN_XOR,
    X86MN_CQO,
    X86MN_JMP,

    /* Conditional jumps. Aliases collapse to these canonical names. */
    X86MN_JA, X86MN_JAE, X86MN_JB, X86MN_JBE, X86MN_JE, X86MN_JG,
    X86MN_JGE, X86MN_JL, X86MN_JLE, X86MN_JNE, X86MN_JNO, X86MN_JNP,
    X86MN_JNS, X86MN_JO, X86MN_JP, X86MN_JS,

    /* Multiply / divide (1-op F7 group + 2-op IMUL). */
    X86MN_IDIV, X86MN_DIV, X86MN_IMUL, X86MN_MUL,

    /* Width-extending moves. */
    X86MN_MOVZX, X86MN_MOVSX,

    /* Conditional moves, ordered by their 4-bit cc so the encoder can
     * derive cc = id - X86MN_CMOVO. Aliases collapse like Jcc. */
    X86MN_CMOVO, X86MN_CMOVNO, X86MN_CMOVB, X86MN_CMOVAE,
    X86MN_CMOVE, X86MN_CMOVNE, X86MN_CMOVBE, X86MN_CMOVA,
    X86MN_CMOVS, X86MN_CMOVNS, X86MN_CMOVP, X86MN_CMOVNP,
    X86MN_CMOVL, X86MN_CMOVGE, X86MN_CMOVLE, X86MN_CMOVG,

    /* Byte string ops; REP-family prefixes glue on at parse time via
     * AsmLine.rep_prefix. */
    X86MN_CLD, X86MN_MOVSB, X86MN_CMPSB, X86MN_STOSB, X86MN_LODSB,
    X86MN_SCASB,

    /* x87 FPU */
    X86MN_F2XM1, X86MN_FABS, X86MN_FADD, X86MN_FADDP, X86MN_FCHS,
    X86MN_FCOMI, X86MN_FCOMIP, X86MN_FCOS, X86MN_FDECSTP, X86MN_FDIV,
    X86MN_FDIVP, X86MN_FDIVR, X86MN_FDIVRP, X86MN_FFREE, X86MN_FINCSTP,
    X86MN_FLD, X86MN_FLD1, X86MN_FLDL2E, X86MN_FLDL2T, X86MN_FLDLG2,
    X86MN_FLDLN2, X86MN_FLDPI, X86MN_FLDZ, X86MN_FMUL, X86MN_FMULP,
    X86MN_FNCLEX, X86MN_FNINIT, X86MN_FPATAN, X86MN_FPREM, X86MN_FPREM1,
    X86MN_FPTAN, X86MN_FRNDINT, X86MN_FSCALE, X86MN_FSIN, X86MN_FSINCOS,
    X86MN_FSQRT, X86MN_FST, X86MN_FSTP, X86MN_FSTSW, X86MN_FSUB,
    X86MN_FSUBP, X86MN_FSUBR, X86MN_FSUBRP, X86MN_FTST, X86MN_FUCOM,
    X86MN_FUCOMI, X86MN_FUCOMIP, X86MN_FUCOMP, X86MN_FWAIT, X86MN_FXAM,
    X86MN_FXTRACT, X86MN_FYL2X, X86MN_FYL2XP1,

    /* SSE / SSE2 scalar + packed-bitwise */
    X86MN_ADDSD, X86MN_ADDSS, X86MN_ANDNPD, X86MN_ANDNPS, X86MN_ANDPD,
    X86MN_ANDPS, X86MN_COMISD, X86MN_COMISS, X86MN_CVTSD2SI,
    X86MN_CVTSD2SS, X86MN_CVTSI2SD, X86MN_CVTSI2SS, X86MN_CVTSS2SD,
    X86MN_CVTSS2SI, X86MN_CVTTSD2SI, X86MN_CVTTSS2SI, X86MN_DIVSD,
    X86MN_DIVSS, X86MN_MAXSD, X86MN_MAXSS, X86MN_MINSD, X86MN_MINSS,
    X86MN_MOVAPD, X86MN_MOVAPS, X86MN_MOVSD, X86MN_MOVSS, X86MN_MOVUPD,
    X86MN_MOVUPS, X86MN_MULSD, X86MN_MULSS, X86MN_ORPD, X86MN_ORPS,
    X86MN_SQRTSD, X86MN_SQRTSS, X86MN_SUBSD, X86MN_SUBSS, X86MN_UCOMISD,
    X86MN_UCOMISS, X86MN_XORPD, X86MN_XORPS,
} X86_64Mn;

#endif
