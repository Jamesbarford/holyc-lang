/* Download the Arm Architecture Reference Manual here;
 * https://developer.arm.com/-/cdn-downloads/permalink/Exploration-Tools-A64-ISA/ISA_A64/ISA_A64_xml_A_profile-2026-03_96.tar.gz
 *
 * This handles most of AArch64, a sprinkling of NEON and a bit of BF16. SVE/SME
 * is not handled at all.
 * */

#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "dis_arm64.h"

/* Each decoder writes its mnemonic + operands into `out` via snprintf,
 * returning 1 on success. They share the same handful of formatting
 * helpers below.
 *
 * The dispatcher tries patterns in order of specificity - the most
 * specific bit masks first, since several encodings share a prefix
 * (e.g. CMP is a SUBS-with-XZR-dst, MOV-reg is an ORR-with-XZR-Rn). */

/* ------------------------------------------------------------ helpers */

/* Print a GPR. For most instructions a register number of 31 means
 * "the zero register"; for a small number of instructions it means
 * SP/WSP. The caller passes `allow_sp` to pick which. */
static void
reg_name(char *buf, size_t bufsz, int sf, int r, int allow_sp)
{
    char p = sf ? 'x' : 'w';
    if (r == 31) {
        if (allow_sp) snprintf(buf, bufsz, "%s", sf ? "sp" : "wsp");
        else          snprintf(buf, bufsz, "%czr", p);
    } else {
        snprintf(buf, bufsz, "%c%d", p, r);
    }
}

/* Sign-extend an N-bit value into int32_t. */
static int32_t
sx(uint32_t v, int n_bits)
{
    uint32_t sign = 1u << (n_bits - 1);
    if (v & sign) v |= ~((1u << n_bits) - 1);
    return (int32_t)v;
}

/* Condition code names (for B.cond / CSET). */
static const char *
cond_name(unsigned cc)
{
    static const char *names[16] = {
        "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc",
        "hi", "ls", "ge", "lt", "gt", "le", "al", "nv"
    };
    return names[cc & 0xF];
}

/* Decode the AArch64 N:immr:imms "logical immediate" bitmask form into
 * the actual constant. Returns 1 on success, 0 if the (N, immr, imms)
 * triple is reserved. */
static int
decode_logical_imm(unsigned sf, unsigned n, unsigned immr, unsigned imms,
                   uint64_t *out)
{
    /* Find pattern length: highest set bit of (N << 6) | NOT(imms). */
    unsigned x = (n << 6) | ((~imms) & 0x3F);
    int len = -1;
    for (int i = 6; i >= 0; i--) {
        if (x & (1u << i)) { len = i; break; }
    }
    if (len < 1) return 0;
    if (!sf && len == 6) return 0;     /* 64-bit pattern needs sf=1 */
    unsigned esize = 1u << len;
    unsigned levels = esize - 1;
    unsigned S = imms & levels;
    unsigned R = immr & levels;
    if (S == levels) return 0;          /* all-ones reserved */
    uint64_t welem = (1ull << (S + 1)) - 1;
    uint64_t mask = (esize == 64) ? ~0ull : ((1ull << esize) - 1);
    welem &= mask;
    uint64_t telem;
    if (R == 0) telem = welem;
    else telem = ((welem >> R) | (welem << (esize - R))) & mask;
    /* Replicate telem across the full register width. */
    int reg_size = sf ? 64 : 32;
    uint64_t result = 0;
    for (int i = 0; i < reg_size; i += (int)esize)
        result |= telem << i;
    if (!sf) result &= 0xFFFFFFFFull;
    *out = result;
    return 1;
}

/* ------------------------------------------------------------ decoders */

static int
dec_movz_movk_movn(uint32_t insn, char *out, size_t outsz)
{
    /* sf op(2) 100101 hw(2) imm16(16) Rd(5)
     * op = 00 MOVN, 10 MOVZ, 11 MOVK */
    unsigned sf  = (insn >> 31) & 1;
    unsigned opc = (insn >> 29) & 3;
    unsigned hw  = (insn >> 21) & 3;
    unsigned imm = (insn >> 5) & 0xFFFF;
    unsigned rd  = insn & 0x1F;
    const char *mn = opc == 0 ? "movn" : opc == 2 ? "movz" : "movk";
    if (opc == 1) return 0;
    char dst[8];
    reg_name(dst, sizeof(dst), (int)sf, (int)rd, 0);
    /* MOVZ's preferred disassembly is the MOV (wide immediate) alias,
     * valid unless imm16==0 with a nonzero shift. */
    if (opc == 2 && !(imm == 0 && hw != 0)) {
        snprintf(out, outsz, "mov %s, #%llu", dst,
                 (unsigned long long)imm << (hw * 16));
        return 1;
    }
    if (hw == 0)
        snprintf(out, outsz, "%s %s, #%u", mn, dst, imm);
    else
        snprintf(out, outsz, "%s %s, #%u, lsl #%u", mn, dst, imm, hw * 16);
    return 1;
}

static int
dec_add_sub_imm(uint32_t insn, char *out, size_t outsz)
{
    /* sf op S 100010 sh(1) imm12 Rn Rd */
    unsigned sf  = (insn >> 31) & 1;
    unsigned op  = (insn >> 30) & 1;          /* 0=ADD, 1=SUB */
    unsigned s   = (insn >> 29) & 1;          /* set-flags */
    unsigned sh  = (insn >> 22) & 1;          /* shift 12 */
    unsigned imm = (insn >> 10) & 0xFFF;
    unsigned rn  = (insn >> 5) & 0x1F;
    unsigned rd  = insn & 0x1F;
    const char *mn = op ? (s ? "subs" : "sub") : (s ? "adds" : "add");
    /* ADD/SUB imm permit SP for Rd/Rn (no S form when Rd=SP). */
    char dst[8], src[8];
    reg_name(dst, sizeof(dst), (int)sf, (int)rd, !s);
    reg_name(src, sizeof(src), (int)sf, (int)rn, 1);
    unsigned val = sh ? (imm << 12) : imm;
    if (s && rd == 31) {
        /* CMP / CMN alias: cmp rn, #imm */
        const char *cmp = op ? "cmp" : "cmn";
        snprintf(out, outsz, "%s %s, #%u", cmp, src, val);
    } else {
        snprintf(out, outsz, "%s %s, %s, #%u", mn, dst, src, val);
    }
    return 1;
}

static int
dec_add_sub_reg(uint32_t insn, char *out, size_t outsz)
{
    static const char *shifts[] = { "lsl", "lsr", "asr", "ror" };
    /* sf op S 01011 shift(2) 0 Rm imm6 Rn Rd  (shifted register, no extend) */
    unsigned sf  = (insn >> 31) & 1;
    unsigned op  = (insn >> 30) & 1;
    unsigned s   = (insn >> 29) & 1;
    unsigned shift = (insn >> 22) & 3;
    unsigned rm  = (insn >> 16) & 0x1F;
    unsigned imm6 = (insn >> 10) & 0x3F;
    unsigned rn  = (insn >> 5) & 0x1F;
    unsigned rd  = insn & 0x1F;
    const char *mn = op ? (s ? "subs" : "sub") : (s ? "adds" : "add");
    char dst[8], src1[8], src2[8];
    reg_name(dst,  sizeof(dst),  (int)sf, (int)rd, 0);
    reg_name(src1, sizeof(src1), (int)sf, (int)rn, 0);
    reg_name(src2, sizeof(src2), (int)sf, (int)rm, 0);
    /* NEG/NEGS alias: SUB/SUBS Rd, XZR, Rm */
    if (op && rn == 31) {
        const char *neg = s ? "negs" : "neg";
        if (imm6 == 0 && shift == 0) {
            snprintf(out, outsz, "%s %s, %s", neg, dst, src2);
            return 1;
        }
    }
    /* CMP/CMN alias when S=1 and Rd=XZR. */
    if (s && rd == 31) {
        const char *cmp = op ? "cmp" : "cmn";
        if (imm6 == 0 && shift == 0)
            snprintf(out, outsz, "%s %s, %s", cmp, src1, src2);
        else {
            snprintf(out, outsz, "%s %s, %s, %s #%u",
                    cmp, src1, src2, shifts[shift], imm6);
        }
        return 1;
    }
    if (imm6 == 0 && shift == 0) {
        snprintf(out, outsz, "%s %s, %s, %s", mn, dst, src1, src2);
    } else {
        snprintf(out, outsz, "%s %s, %s, %s, %s #%u",
                mn, dst, src1, src2, shifts[shift], imm6);
    }
    return 1;
}

static int
dec_logical_reg(uint32_t insn, char *out, size_t outsz)
{
    /* sf opc(2) 01010 shift(2) N Rm imm6 Rn Rd
     * (opc, N) pairs:
     *   (00,0) AND   (00,1) BIC
     *   (01,0) ORR   (01,1) ORN
     *   (10,0) EOR   (10,1) EON
     *   (11,0) ANDS  (11,1) BICS */
    unsigned sf  = (insn >> 31) & 1;
    unsigned opc = (insn >> 29) & 3;
    unsigned shift = (insn >> 22) & 3;
    unsigned n   = (insn >> 21) & 1;
    unsigned rm  = (insn >> 16) & 0x1F;
    unsigned imm6 = (insn >> 10) & 0x3F;
    unsigned rn  = (insn >> 5) & 0x1F;
    unsigned rd  = insn & 0x1F;
    static const char *base_mn[2][4] = {
        { "and", "orr", "eor", "ands" },
        { "bic", "orn", "eon", "bics" },
    };
    const char *mn = base_mn[n][opc];
    char dst[8], src1[8], src2[8];
    reg_name(dst,  sizeof(dst),  (int)sf, (int)rd, 0);
    reg_name(src1, sizeof(src1), (int)sf, (int)rn, 0);
    reg_name(src2, sizeof(src2), (int)sf, (int)rm, 0);
    int unshifted = (imm6 == 0 && shift == 0);
    /* MOV reg alias: ORR Rd, XZR, Rm. */
    if (opc == 1 && n == 0 && rn == 31 && unshifted) {
        snprintf(out, outsz, "mov %s, %s", dst, src2);
        return 1;
    }
    /* MVN alias: ORN Rd, XZR, Rm. */
    if (opc == 1 && n == 1 && rn == 31 && unshifted) {
        snprintf(out, outsz, "mvn %s, %s", dst, src2);
        return 1;
    }
    /* TST alias: ANDS XZR, Rn, Rm. */
    if (opc == 3 && n == 0 && rd == 31 && unshifted) {
        snprintf(out, outsz, "tst %s, %s", src1, src2);
        return 1;
    }
    static const char *shifts[] = { "lsl", "lsr", "asr", "ror" };
    if (unshifted)
        snprintf(out, outsz, "%s %s, %s, %s", mn, dst, src1, src2);
    else
        snprintf(out, outsz, "%s %s, %s, %s, %s #%u",
                mn, dst, src1, src2, shifts[shift], imm6);
    return 1;
}

static int
dec_logical_imm(uint32_t insn, char *out, size_t outsz)
{
    /* sf opc(2) 100100 N immr(6) imms(6) Rn Rd
     * opc: 00 AND, 01 ORR, 10 EOR, 11 ANDS */
    unsigned sf = (insn >> 31) & 1;
    unsigned opc = (insn >> 29) & 3;
    unsigned n = (insn >> 22) & 1;
    unsigned immr = (insn >> 16) & 0x3F;
    unsigned imms = (insn >> 10) & 0x3F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    static const char *mns[4] = { "and", "orr", "eor", "ands" };
    uint64_t imm;
    if (!decode_logical_imm(sf, n, immr, imms, &imm)) return 0;
    char dst[8], srn[8];
    /* MOV (bitmask immediate) alias: ORR Rd, XZR, #imm with Rd != XZR. */
    if (opc == 1 && rn == 31) {
        reg_name(dst, sizeof(dst), (int)sf, (int)rd, 1);
        snprintf(out, outsz, "mov %s, #0x%llx", dst, (unsigned long long)imm);
        return 1;
    }
    /* TST alias: ANDS XZR, Rn, #imm. */
    if (opc == 3 && rd == 31) {
        reg_name(srn, sizeof(srn), (int)sf, (int)rn, 0);
        snprintf(out, outsz, "tst %s, #0x%llx", srn, (unsigned long long)imm);
        return 1;
    }
    /* Non-flag forms permit SP for Rd. */
    reg_name(dst, sizeof(dst), (int)sf, (int)rd, opc != 3);
    reg_name(srn, sizeof(srn), (int)sf, (int)rn, 0);
    snprintf(out, outsz, "%s %s, %s, #0x%llx",
            mns[opc], dst, srn, (unsigned long long)imm);
    return 1;
}

static int
dec_dp3(uint32_t insn, char *out, size_t outsz)
{
    /* sf 00 11011 op31(3) Rm o0 Ra Rn Rd
     *
     * (op31, o0) selects the operation:
     *   (000, 0) MADD          -> MUL alias when Ra=XZR
     *   (000, 1) MSUB          -> MNEG when Ra=XZR
     *   (001, 0) SMADDL        -> SMULL when Ra=XZR
     *   (001, 1) SMSUBL        -> SMNEGL when Ra=XZR
     *   (010, 0) SMULH         (Ra ignored)
     *   (101, 0) UMADDL        -> UMULL when Ra=XZR
     *   (101, 1) UMSUBL        -> UMNEGL when Ra=XZR
     *   (110, 0) UMULH
     */
    unsigned sf = (insn >> 31) & 1;
    unsigned op31 = (insn >> 21) & 7;
    unsigned rm = (insn >> 16) & 0x1F;
    unsigned o0 = (insn >> 15) & 1;
    unsigned ra = (insn >> 10) & 0x1F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;

    /* Widening forms use a 64-bit destination and 32-bit Rn/Rm. */
    int is_wide = (op31 == 1 || op31 == 5);
    int is_high = (op31 == 2 || op31 == 6);
    int is_unsigned_wide = (op31 == 5 || op31 == 6);

    char dst[8], srn[8], srm[8], sra[8];
    if (is_wide) {
        /* widening MADD/MSUB: Rd is X, Rn/Rm are W, Ra is X. */
        reg_name(dst, sizeof(dst), 1, (int)rd, 0);
        reg_name(srn, sizeof(srn), 0, (int)rn, 0);
        reg_name(srm, sizeof(srm), 0, (int)rm, 0);
        reg_name(sra, sizeof(sra), 1, (int)ra, 0);
    } else if (is_high) {
        /* SMULH/UMULH: all 64-bit. */
        reg_name(dst, sizeof(dst), 1, (int)rd, 0);
        reg_name(srn, sizeof(srn), 1, (int)rn, 0);
        reg_name(srm, sizeof(srm), 1, (int)rm, 0);
        reg_name(sra, sizeof(sra), 1, (int)ra, 0);
    } else {
        reg_name(dst, sizeof(dst), (int)sf, (int)rd, 0);
        reg_name(srn, sizeof(srn), (int)sf, (int)rn, 0);
        reg_name(srm, sizeof(srm), (int)sf, (int)rm, 0);
        reg_name(sra, sizeof(sra), (int)sf, (int)ra, 0);
    }
    /* Widening high forms: no Ra, no MUL alias. */
    if (is_high) {
        snprintf(out, outsz, "%s %s, %s, %s",
                is_unsigned_wide ? "umulh" : "smulh", dst, srn, srm);
        return 1;
    }
    int has_alias = (ra == 31);
    const char *base, *alias;
    if (op31 == 0) {
        base  = o0 ? "msub" : "madd";
        alias = o0 ? "mneg" : "mul";
    } else if (op31 == 1) {
        base  = o0 ? "smsubl" : "smaddl";
        alias = o0 ? "smnegl" : "smull";
    } else if (op31 == 5) {
        base  = o0 ? "umsubl" : "umaddl";
        alias = o0 ? "umnegl" : "umull";
    } else {
        return 0;
    }
    if (has_alias)
        snprintf(out, outsz, "%s %s, %s, %s", alias, dst, srn, srm);
    else
        snprintf(out, outsz, "%s %s, %s, %s, %s", base, dst, srn, srm, sra);
    return 1;
}


static int
dec_dp2(uint32_t insn, char *out, size_t outsz)
{
    /* sf 0 0 11010110 Rm opcode2(6) Rn Rd
     * Covers UDIV/SDIV and the variable-shift family. */
    unsigned sf = (insn >> 31) & 1;
    unsigned rm = (insn >> 16) & 0x1F;
    unsigned op2 = (insn >> 10) & 0x3F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    const char *mn;
    switch (op2) {
        case 0x02: mn = "udiv"; break;
        case 0x03: mn = "sdiv"; break;
        case 0x08: mn = "lsl"; break;     /* LSLV */
        case 0x09: mn = "lsr"; break;     /* LSRV */
        case 0x0A: mn = "asr"; break;     /* ASRV */
        case 0x0B: mn = "ror"; break;     /* RORV */
        default: return 0;
    }
    char dst[8], srn[8], srm[8];
    reg_name(dst, sizeof(dst), (int)sf, (int)rd, 0);
    reg_name(srn, sizeof(srn), (int)sf, (int)rn, 0);
    reg_name(srm, sizeof(srm), (int)sf, (int)rm, 0);
    snprintf(out, outsz, "%s %s, %s, %s", mn, dst, srn, srm);
    return 1;
}

static int
dec_dp1(uint32_t insn, char *out, size_t outsz)
{
    /* sf 1 S 11010110 00000 opcode2(6) Rn Rd  (data-processing 1 source)
     * opcode2 picks the mnemonic. */
    unsigned sf = (insn >> 31) & 1;
    unsigned op2 = (insn >> 10) & 0x3F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    const char *mn;
    switch (op2) {
        case 0x00: mn = "rbit"; break;
        case 0x01: mn = "rev16"; break;
        case 0x02:                 /* 32-bit reverse */
            mn = sf ? "rev32" : "rev"; break;
        case 0x03: mn = "rev"; break;  /* 64-bit only */
        case 0x04: mn = "clz"; break;
        case 0x05: mn = "cls"; break;
        default: return 0;
    }
    char dst[8], srn[8];
    reg_name(dst, sizeof(dst), (int)sf, (int)rd, 0);
    reg_name(srn, sizeof(srn), (int)sf, (int)rn, 0);
    snprintf(out, outsz, "%s %s, %s", mn, dst, srn);
    return 1;
}

static int
dec_csel_family(uint32_t insn, char *out, size_t outsz)
{
    /* sf op S 11010100 Rm cond op2(2) Rn Rd
     * (op, op2) selects the family member:
     *   (0, 00) CSEL
     *   (0, 01) CSINC  - alias CSET (Rm=Rn=XZR), CINC (Rn=Rm != XZR)
     *   (1, 00) CSINV  - alias CSETM, CINV
     *   (1, 01) CSNEG  - alias CNEG
     */
    unsigned sf = (insn >> 31) & 1;
    unsigned op = (insn >> 30) & 1;
    unsigned rm = (insn >> 16) & 0x1F;
    unsigned cond = (insn >> 12) & 0xF;
    unsigned op2 = (insn >> 10) & 3;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    char dst[8], srn[8], srm[8];
    reg_name(dst, sizeof(dst), (int)sf, (int)rd, 0);
    reg_name(srn, sizeof(srn), (int)sf, (int)rn, 0);
    reg_name(srm, sizeof(srm), (int)sf, (int)rm, 0);
    /* Alias matrix (when cond != AL/NV):
     *   CSINC (op=0,op2=01): CSET when Rm=Rn=31; CINC when Rm=Rn != 31.
     *   CSINV (op=1,op2=00): CSETM when Rm=Rn=31; CINV when Rm=Rn != 31.
     *   CSNEG (op=1,op2=01): CNEG when Rm=Rn.
     *   CSEL  (op=0,op2=00): no aliases. */
    int alias_cond_ok = ((cond & 0xE) != 0xE);
    int is_csinc = (op == 0 && op2 == 1);
    int is_csinv = (op == 1 && op2 == 0);
    int is_csneg = (op == 1 && op2 == 1);
    if (alias_cond_ok && rn == rm) {
        if ((is_csinc || is_csinv) && rn == 31) {
            snprintf(out, outsz, "%s %s, %s",
                    is_csinv ? "csetm" : "cset", dst, cond_name(cond ^ 1));
            return 1;
        }
        if (is_csinc || is_csinv || is_csneg) {
            const char *alias = is_csneg ? "cneg" : (is_csinv ? "cinv" : "cinc");
            snprintf(out, outsz, "%s %s, %s, %s",
                    alias, dst, srn, cond_name(cond ^ 1));
            return 1;
        }
    }
    const char *mn;
    if      (is_csinc) mn = "csinc";
    else if (is_csinv) mn = "csinv";
    else if (is_csneg) mn = "csneg";
    else               mn = "csel";
    snprintf(out, outsz, "%s %s, %s, %s, %s",
            mn, dst, srn, srm, cond_name(cond));
    return 1;
}

/* CCMP / CCMN (register or immediate form).
 *
 *   sf op 1 11010010 (Rm|imm5) cond 0 mode Rn 0 nzcv
 *
 * op = 1 -> CCMP, op = 0 -> CCMN. mode bit at 11 picks register (0) or
 * immediate (1) form. */
static int
dec_ccmp_ccmn(uint32_t insn, char *out, size_t outsz)
{
    unsigned sf = (insn >> 31) & 1;
    unsigned op = (insn >> 30) & 1;
    unsigned imm_form = (insn >> 11) & 1;
    unsigned cond = (insn >> 12) & 0xF;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned nzcv = insn & 0xF;
    char srn[8];
    reg_name(srn, sizeof(srn), (int)sf, (int)rn, 0);
    const char *mn = op ? "ccmp" : "ccmn";
    if (imm_form) {
        unsigned imm5 = (insn >> 16) & 0x1F;
        snprintf(out, outsz, "%s %s, #%u, #%u, %s",
                mn, srn, imm5, nzcv, cond_name(cond));
    } else {
        unsigned rm = (insn >> 16) & 0x1F;
        char srm[8];
        reg_name(srm, sizeof(srm), (int)sf, (int)rm, 0);
        snprintf(out, outsz, "%s %s, %s, #%u, %s",
                mn, srn, srm, nzcv, cond_name(cond));
    }
    return 1;
}

static int
dec_bfm(uint32_t insn, char *out, size_t outsz)
{
    /* sf opc(2) 100110 N immr(6) imms(6) Rn Rd
     *
     * opc 00 = SBFM, 01 = BFM, 10 = UBFM.
     *
     * Alias priority (most preferred first):
     *   SXTB / SXTH / SXTW / UXTB / UXTH   (immr=0, imms in {7,15,31})
     *   ASR / LSR imm                       (imms = width-1)
     *   LSL imm   (UBFM only)               (imms + 1 = immr (mod width))
     *   SBFX / UBFX                         (imms >= immr)
     *   raw sbfm/ubfm/bfm */
    unsigned sf = (insn >> 31) & 1;
    unsigned opc = (insn >> 29) & 3;
    unsigned immr = (insn >> 16) & 0x3F;
    unsigned imms = (insn >> 10) & 0x3F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    unsigned width = sf ? 64 : 32;
    char dst[8], srn_full[8], srn_w[8];
    reg_name(dst, sizeof(dst), (int)sf, (int)rd, 0);
    reg_name(srn_full, sizeof(srn_full), (int)sf, (int)rn, 0);
    reg_name(srn_w, sizeof(srn_w), 0, (int)rn, 0);

    if (opc == 1) {
        /* BFM aliases:
         *   imms < immr -> BFI  Xd, Xn, #lsb, #width
         *     where lsb = (width_bits - immr) mod width_bits, bf_w = imms+1
         *   imms >= immr -> BFXIL Xd, Xn, #lsb, #width
         *     where lsb = immr, bf_w = imms - immr + 1 */
        if (imms < immr) {
            unsigned lsb = (width - immr) & (width - 1);
            unsigned bf_w = imms + 1;
            snprintf(out, outsz, "bfi %s, %s, #%u, #%u",
                    dst, srn_full, lsb, bf_w);
        } else {
            unsigned lsb = immr;
            unsigned bf_w = imms - immr + 1;
            snprintf(out, outsz, "bfxil %s, %s, #%u, #%u",
                    dst, srn_full, lsb, bf_w);
        }
        return 1;
    }
    int is_signed = (opc == 0);
    /* SXT / UXT aliases. The source operand is always W-form because the
     * extracted slice is from the low N bits. */
    if (immr == 0) {
        const char *alias = NULL;
        if (is_signed) {
            if (imms == 7)              alias = "sxtb";
            else if (imms == 15)        alias = "sxth";
            else if (sf && imms == 31)  alias = "sxtw";
        } else {
            if (imms == 7)              alias = "uxtb";
            else if (imms == 15)        alias = "uxth";
        }
        if (alias) {
            snprintf(out, outsz, "%s %s, %s", alias, dst, srn_w);
            return 1;
        }
    }
    /* ASR / LSR immediate. */
    if (imms == width - 1) {
        snprintf(out, outsz, "%s %s, %s, #%u",
                is_signed ? "asr" : "lsr", dst, srn_full, immr);
        return 1;
    }
    /* LSL immediate (UBFM only). */
    if (!is_signed && imms + 1 == immr) {
        unsigned shift = width - immr;
        snprintf(out, outsz, "lsl %s, %s, #%u", dst, srn_full, shift);
        return 1;
    }
    /* Bitfield extract. */
    if (imms >= immr) {
        snprintf(out, outsz, "%s %s, %s, #%u, #%u",
                is_signed ? "sbfx" : "ubfx",
                dst, srn_full, immr, imms - immr + 1);
        return 1;
    }
    /* Raw mnemonic. */
    snprintf(out, outsz, "%s %s, %s, #%u, #%u",
            is_signed ? "sbfm" : "ubfm", dst, srn_full, immr, imms);
    return 1;
}

static int
dec_b_bl(uint32_t insn, char *out, size_t outsz)
{
    /* op 00101 imm26 ; op=0 B, op=1 BL */
    unsigned link = (insn >> 31) & 1;
    int32_t off = sx(insn & 0x03FFFFFF, 26) * 4;
    snprintf(out, outsz, "%s #%+d", link ? "bl" : "b", off);
    return 1;
}

static int
dec_b_cond(uint32_t insn, char *out, size_t outsz)
{
    /* 01010100 imm19 0 cond */
    int32_t off = sx((insn >> 5) & 0x7FFFF, 19) * 4;
    unsigned cc = insn & 0xF;
    snprintf(out, outsz, "b.%s #%+d", cond_name(cc), off);
    return 1;
}

static int
dec_tbz_tbnz(uint32_t insn, char *out, size_t outsz)
{
    /* b5 011011 op b40(5) imm14(14) Rt
     * Bit index = (b5 << 5) | b40, range 0..63 for X or 0..31 for W. */
    unsigned b5 = (insn >> 31) & 1;
    unsigned op = (insn >> 24) & 1;
    unsigned b40 = (insn >> 19) & 0x1F;
    int32_t imm14 = sx((insn >> 5) & 0x3FFF, 14) * 4;
    unsigned rt = insn & 0x1F;
    unsigned bit = (b5 << 5) | b40;
    char rt_str[8];
    reg_name(rt_str, sizeof(rt_str), (int)b5, (int)rt, 0);
    snprintf(out, outsz, "%s %s, #%u, #%+d",
            op ? "tbnz" : "tbz", rt_str, bit, imm14);
    return 1;
}

static int
dec_cbz_cbnz(uint32_t insn, char *out, size_t outsz)
{
    /* sf 011010 op imm19 Rt ; op=0 CBZ, op=1 CBNZ */
    unsigned sf = (insn >> 31) & 1;
    unsigned op = (insn >> 24) & 1;
    int32_t off = sx((insn >> 5) & 0x7FFFF, 19) * 4;
    unsigned rt = insn & 0x1F;
    char src[8];
    reg_name(src, sizeof(src), (int)sf, (int)rt, 0);
    snprintf(out, outsz, "%s %s, #%+d", op ? "cbnz" : "cbz", src, off);
    return 1;
}

static int
dec_ret(uint32_t insn, char *out, size_t outsz)
{
    /* 1101011001011111000000 Rn 00000 */
    unsigned rn = (insn >> 5) & 0x1F;
    if (rn == 30) snprintf(out, outsz, "ret");
    else snprintf(out, outsz, "ret x%u", rn);
    return 1;
}

static int
dec_br(uint32_t insn, char *out, size_t outsz)
{
    /* 1101011000011111000000 Rn 00000 - branch to register. Used by JIT
     * veneers when the target is out of B/BL range. */
    unsigned rn = (insn >> 5) & 0x1F;
    snprintf(out, outsz, "br x%u", rn);
    return 1;
}

static int
dec_blr(uint32_t insn, char *out, size_t outsz)
{
    /* 1101011000111111000000 Rn 00000 - branch with link to register. */
    unsigned rn = (insn >> 5) & 0x1F;
    snprintf(out, outsz, "blr x%u", rn);
    return 1;
}

/* Memory barriers: DMB / DSB / ISB.
 *   1101 0101 0000 0011 0011 CRm op2(3) 11111
 * op2 selects: 100 DSB, 101 DMB, 110 ISB. */
static int
dec_barrier(uint32_t insn, char *out, size_t outsz)
{
    unsigned crm = (insn >> 8) & 0xF;
    unsigned op2 = (insn >> 5) & 7;
    const char *mn;
    if      (op2 == 4) mn = "dsb";
    else if (op2 == 5) mn = "dmb";
    else if (op2 == 6) mn = "isb";
    else return 0;
    /* CRm names the barrier domain + access type. */
    static const char *names[16] = {
        "#0",     "oshld", "oshst", "osh",
        "#4",     "nshld", "nshst", "nsh",
        "#8",     "ishld", "ishst", "ish",
        "#12",    "ld",    "st",    "sy",
    };
    /* ISB by convention only uses the SY operand. */
    if (op2 == 6 && crm == 0xF)
        snprintf(out, outsz, "isb");
    else
        snprintf(out, outsz, "%s %s", mn, names[crm]);
    return 1;
}

/* MSR (immediate) - writes a 4-bit immediate to a named PSTATE field.
 *   1101 0101 0000 0 op1(3) 0100 CRm(4) op2(3) 11111
 * (op1, op2) names the field; CRm carries the 4-bit immediate. */
static int
dec_msr_imm(uint32_t insn, char *out, size_t outsz)
{
    unsigned op1 = (insn >> 16) & 7;
    unsigned crm = (insn >> 8) & 0xF;
    unsigned op2 = (insn >> 5) & 7;
    const char *name = NULL;
    if      (op1 == 0 && op2 == 3) name = "uao";
    else if (op1 == 0 && op2 == 4) name = "pan";
    else if (op1 == 0 && op2 == 5) name = "spsel";
    else if (op1 == 3 && op2 == 1) name = "ssbs";
    else if (op1 == 3 && op2 == 2) name = "dit";
    else if (op1 == 3 && op2 == 4) name = "tco";
    else if (op1 == 3 && op2 == 6) name = "daifset";
    else if (op1 == 3 && op2 == 7) name = "daifclr";
    char raw[24];
    if (!name) {
        snprintf(raw, sizeof(raw), "s0_%u_c4_c%u_%u", op1, crm, op2);
        name = raw;
    }
    snprintf(out, outsz, "msr %s, #%u", name, crm);
    return 1;
}

/* MRS / MSR (register-to-register). Encoding:
 *   1101 0101 00 L 1 o0 op1(3) CRn(4) CRm(4) op2(3) Rt
 * L=1 -> MRS (read sysreg into Rt). L=0 -> MSR (write Rt into sysreg).
 * The sysreg name is built from (op0, op1, CRn, CRm, op2); we recognise
 * a few common ones by name and otherwise emit the raw form. */
static int
dec_mrs_msr(uint32_t insn, char *out, size_t outsz)
{
    unsigned l   = (insn >> 21) & 1;
    unsigned o0  = (insn >> 19) & 1;
    unsigned op1 = (insn >> 16) & 7;
    unsigned crn = (insn >> 12) & 0xF;
    unsigned crm = (insn >> 8) & 0xF;
    unsigned op2 = (insn >> 5) & 7;
    unsigned rt  = insn & 0x1F;
    unsigned op0 = o0 + 2;  /* o0 encodes (op0 - 2); op0 is 2 or 3 */
    char rt_str[8];
    reg_name(rt_str, sizeof(rt_str), 1, (int)rt, 0);
    const char *name = NULL;
    if (op0 == 3 && op1 == 3 && crn == 4  && crm == 2 && op2 == 0) name = "nzcv";
    else if (op0 == 3 && op1 == 3 && crn == 4  && crm == 4 && op2 == 0) name = "fpcr";
    else if (op0 == 3 && op1 == 3 && crn == 4  && crm == 4 && op2 == 1) name = "fpsr";
    else if (op0 == 3 && op1 == 3 && crn == 13 && crm == 0 && op2 == 2) name = "tpidr_el0";
    else if (op0 == 3 && op1 == 3 && crn == 13 && crm == 0 && op2 == 3) name = "tpidrro_el0";
    char raw[32];
    if (!name) {
        snprintf(raw, sizeof(raw), "s%u_%u_c%u_c%u_%u",
                op0, op1, crn, crm, op2);
        name = raw;
    }
    if (l) snprintf(out, outsz, "mrs %s, %s", rt_str, name);
    else   snprintf(out, outsz, "msr %s, %s", name, rt_str);
    return 1;
}

/* LDAR / STLR (and byte/half variants). One-shot ordered loads/stores.
 *   size(2) 001000 1 L 0 11111 1 11111 Rn Rt
 * L=1 LDAR family; L=0 STLR family. */
static int
dec_ldar_stlr(uint32_t insn, char *out, size_t outsz)
{
    unsigned size = (insn >> 30) & 3;
    unsigned l    = (insn >> 22) & 1;
    unsigned rn   = (insn >> 5) & 0x1F;
    unsigned rt   = insn & 0x1F;
    const char *mn = NULL;
    int dst_sf = (size == 3);
    if (l) {
        switch (size) {
            case 0: mn = "ldarb"; break;
            case 1: mn = "ldarh"; break;
            case 2: case 3: mn = "ldar"; break;
        }
    } else {
        switch (size) {
            case 0: mn = "stlrb"; break;
            case 1: mn = "stlrh"; break;
            case 2: case 3: mn = "stlr"; break;
        }
    }
    /* For byte/half forms, Rt is always W-form. */
    if (size < 2) dst_sf = 0;
    char rt_str[8], rn_str[8];
    reg_name(rt_str, sizeof(rt_str), dst_sf, (int)rt, 0);
    reg_name(rn_str, sizeof(rn_str), 1, (int)rn, 1);
    snprintf(out, outsz, "%s %s, [%s]", mn, rt_str, rn_str);
    return 1;
}

/* LDXR / STXR (load/store exclusive). Encoding:
 *   size(2) 001000 0 L 0 Rs 0 11111 Rn Rt
 * For STXR, Rs receives 0 on success / 1 on failure. */
static int
dec_ldxr_stxr(uint32_t insn, char *out, size_t outsz)
{
    unsigned size = (insn >> 30) & 3;
    unsigned l    = (insn >> 22) & 1;
    unsigned rs   = (insn >> 16) & 0x1F;
    unsigned rn   = (insn >> 5) & 0x1F;
    unsigned rt   = insn & 0x1F;
    const char *mn = NULL;
    int dst_sf = (size == 3);
    if (l) {
        switch (size) {
            case 0: mn = "ldxrb"; break;
            case 1: mn = "ldxrh"; break;
            case 2: case 3: mn = "ldxr"; break;
        }
    } else {
        switch (size) {
            case 0: mn = "stxrb"; break;
            case 1: mn = "stxrh"; break;
            case 2: case 3: mn = "stxr"; break;
        }
    }
    if (size < 2) dst_sf = 0;
    char rt_str[8], rn_str[8], rs_str[8];
    reg_name(rt_str, sizeof(rt_str), dst_sf, (int)rt, 0);
    reg_name(rn_str, sizeof(rn_str), 1, (int)rn, 1);
    reg_name(rs_str, sizeof(rs_str), 0, (int)rs, 0);  /* Ws is always W */
    if (l)
        snprintf(out, outsz, "%s %s, [%s]", mn, rt_str, rn_str);
    else
        snprintf(out, outsz, "%s %s, %s, [%s]", mn, rs_str, rt_str, rn_str);
    return 1;
}

/* PRFM (immediate). Shares the unsigned-immediate-offset slot but with
 * size=11, opc=10 - a combination the regular LDR/STR table rejects. */
static int
dec_prfm_imm(uint32_t insn, char *out, size_t outsz)
{
    unsigned imm12 = (insn >> 10) & 0xFFF;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rt = insn & 0x1F;
    unsigned scaled = imm12 << 3;
    char rn_str[8];
    reg_name(rn_str, sizeof(rn_str), 1, (int)rn, 1);
    /* Rt encodes the prefetch operation: type(2), target(2), strm(1). */
    unsigned type = (rt >> 3) & 3;
    unsigned target = (rt >> 1) & 3;
    unsigned strm = rt & 1;
    static const char *types[]   = { "pld", "pli", "pst", NULL };
    static const char *targets[] = { "l1", "l2", "l3", NULL };
    if (types[type] && targets[target]) {
        char hint[16];
        snprintf(hint, sizeof(hint), "%s%s%s",
                types[type], targets[target], strm ? "strm" : "keep");
        if (scaled == 0)
            snprintf(out, outsz, "prfm %s, [%s]", hint, rn_str);
        else
            snprintf(out, outsz, "prfm %s, [%s, #%u]", hint, rn_str, scaled);
    } else {
        /* Fallback: just emit the prefetch op as a number. */
        if (scaled == 0)
            snprintf(out, outsz, "prfm #%u, [%s]", rt, rn_str);
        else
            snprintf(out, outsz, "prfm #%u, [%s, #%u]", rt, rn_str, scaled);
    }
    return 1;
}

static int
dec_hint(uint32_t insn, char *out, size_t outsz)
{
    /* 11010101 00000011 0010 CRm(4) op2(3) 11111
     * imm7 = (CRm << 3) | op2 selects the hint kind. */
    unsigned crm = (insn >> 8) & 0xF;
    unsigned op2 = (insn >> 5) & 0x7;
    unsigned imm = (crm << 3) | op2;
    static const char *names[] = {
        "nop", "yield", "wfe", "wfi", "sev", "sevl",
    };
    if (imm < sizeof(names) / sizeof(names[0]))
        snprintf(out, outsz, "%s", names[imm]);
    else
        snprintf(out, outsz, "hint #%u", imm);
    return 1;
}

/* LDR/STR with a register offset:
 *   size(2) 111 V 00 opc(2) 1 Rm option(3) S 10 Rn Rt
 *
 * option encodes the optional extension applied to Rm before adding to
 * the base. S=1 multiplies the (already-extended) Rm by the access size
 * (i.e. shifts left by `size`). */
static int
dec_ldst_regoff(uint32_t insn, char *out, size_t outsz)
{
    unsigned size = (insn >> 30) & 3;
    unsigned v    = (insn >> 26) & 1;
    unsigned opc  = (insn >> 22) & 3;
    unsigned rm   = (insn >> 16) & 0x1F;
    unsigned option = (insn >> 13) & 7;
    unsigned s_bit  = (insn >> 12) & 1;
    unsigned rn   = (insn >> 5) & 0x1F;
    unsigned rt   = insn & 0x1F;
    if (v) return 0;
    if (opc >= 2 && size == 3) return 0;

    const char *mn = NULL;
    int dst_sf = (size == 3);
    if (opc == 0) {
        switch (size) { case 0: mn = "strb"; break; case 1: mn = "strh"; break;
                        case 2: case 3: mn = "str"; break; }
    } else if (opc == 1) {
        switch (size) { case 0: mn = "ldrb"; break; case 1: mn = "ldrh"; break;
                        case 2: case 3: mn = "ldr"; break; }
    } else {
        dst_sf = (opc == 2) ? 1 : 0;
        switch (size) {
            case 0: mn = "ldrsb"; break;
            case 1: mn = "ldrsh"; break;
            case 2: mn = "ldrsw"; break;
            default: return 0;
        }
    }
    /* option encoding picks the extension AND the source-reg width:
     *   010 UXTW (Rm=W), 011 LSL (Rm=X), 110 SXTW (Rm=W), 111 SXTX (Rm=X) */
    int rm_is_64 = (option & 1);
    char rt_str[8], rn_str[8], rm_str[8];
    reg_name(rt_str, sizeof(rt_str), dst_sf, (int)rt, 0);
    reg_name(rn_str, sizeof(rn_str), 1, (int)rn, 1);
    reg_name(rm_str, sizeof(rm_str), rm_is_64, (int)rm, 0);
    static const char *exts[] = {
        "uxtb", "uxth", "uxtw", "lsl", "sxtb", "sxth", "sxtw", "sxtx"
    };
    unsigned shift = s_bit ? size : 0;
    if (option == 3 && shift == 0)
        snprintf(out, outsz, "%s %s, [%s, %s]", mn, rt_str, rn_str, rm_str);
    else if (shift == 0)
        snprintf(out, outsz, "%s %s, [%s, %s, %s]",
                mn, rt_str, rn_str, rm_str, exts[option]);
    else
        snprintf(out, outsz, "%s %s, [%s, %s, %s #%u]",
                mn, rt_str, rn_str, rm_str, exts[option], shift);
    return 1;
}

/* ADD/SUB (extended register):
 *   sf op S 01011 opt(2) 1 Rm option(3) imm3 Rn Rd
 *
 * Rn (and Rd when S=0) may be SP. */
static int
dec_add_sub_ext_reg(uint32_t insn, char *out, size_t outsz)
{
    unsigned sf  = (insn >> 31) & 1;
    unsigned op  = (insn >> 30) & 1;
    unsigned s   = (insn >> 29) & 1;
    unsigned rm  = (insn >> 16) & 0x1F;
    unsigned option = (insn >> 13) & 7;
    unsigned imm3 = (insn >> 10) & 7;
    unsigned rn  = (insn >> 5) & 0x1F;
    unsigned rd  = insn & 0x1F;
    int rm_is_64 = (option & 1);
    static const char *exts[] = {
        "uxtb", "uxth", "uxtw", "uxtx", "sxtb", "sxth", "sxtw", "sxtx"
    };
    const char *mn = op ? (s ? "subs" : "sub") : (s ? "adds" : "add");
    char dst[8], srn[8], srm[8];
    reg_name(dst, sizeof(dst), (int)sf, (int)rd, !s);
    reg_name(srn, sizeof(srn), (int)sf, (int)rn, 1);
    reg_name(srm, sizeof(srm), rm_is_64, (int)rm, 0);
    if (imm3 == 0)
        snprintf(out, outsz, "%s %s, %s, %s, %s",
                mn, dst, srn, srm, exts[option]);
    else
        snprintf(out, outsz, "%s %s, %s, %s, %s #%u",
                mn, dst, srn, srm, exts[option], imm3);
    return 1;
}

static int
dec_adr(uint32_t insn, char *out, size_t outsz)
{
    /* op immlo(2) 10000 immhi(19) Rd ; op=0 ADR (byte offset),
     *                                    op=1 ADRP (page offset) */
    unsigned op = (insn >> 31) & 1;
    unsigned immlo = (insn >> 29) & 3;
    unsigned immhi = (insn >> 5) & 0x7FFFF;
    unsigned rd = insn & 0x1F;
    uint32_t raw = (immhi << 2) | immlo;
    int32_t off = sx(raw, 21);
    char dst[8];
    reg_name(dst, sizeof(dst), 1, (int)rd, 0);
    if (op) snprintf(out, outsz, "adrp %s, #%+d", dst, off * 4096);
    else    snprintf(out, outsz, "adr %s, #%+d", dst, off);
    return 1;
}

/* Sized unsigned-offset loads/stores: LDR/STR/LDRB/LDRH/STRB/STRH and
 * the signed variants LDRSB/LDRSH/LDRSW.
 *
 *   size(2) 111 V(1) 01 opc(2) imm12 Rn Rt
 *
 * V=0 (general), V=1 (SIMD/FP). We only decode V=0 here.
 * opc field encodes load vs store and signedness:
 *   opc=00 STR
 *   opc=01 LDR (zero-extend)
 *   opc=10 LDRS to X (signed, 64-bit dst)
 *   opc=11 LDRS to W (signed, 32-bit dst)
 *
 * size=00 byte, 01 half, 10 word (32), 11 double (64).
 * imm12 is in units of (1 << size) bytes. */
static int
dec_ldst_uimm(uint32_t insn, char *out, size_t outsz)
{
    unsigned size = (insn >> 30) & 3;
    unsigned v   = (insn >> 26) & 1;
    unsigned opc = (insn >> 22) & 3;
    unsigned imm = (insn >> 10) & 0xFFF;
    unsigned rn  = (insn >> 5) & 0x1F;
    unsigned rt  = insn & 0x1F;
    if (v) return 0;  /* SIMD/FP not covered here */
    /* opc=10/11 with size=11 is unallocated (LDRSW only exists for size=10). */
    if (opc >= 2 && size == 3) return 0;
    const char *mn = NULL;
    int dst_sf = (size == 3);          /* X for 64-bit doubleword, W otherwise */
    if (opc == 0) {
        switch (size) {
            case 0: mn = "strb"; break;
            case 1: mn = "strh"; break;
            case 2: mn = "str"; break;     /* w-reg */
            case 3: mn = "str"; break;     /* x-reg */
        }
    } else if (opc == 1) {
        switch (size) {
            case 0: mn = "ldrb"; break;
            case 1: mn = "ldrh"; break;
            case 2: mn = "ldr"; break;
            case 3: mn = "ldr"; break;
        }
    } else {
        /* signed load */
        if (opc == 2) dst_sf = 1;
        else          dst_sf = 0;
        switch (size) {
            case 0: mn = "ldrsb"; break;
            case 1: mn = "ldrsh"; break;
            case 2: mn = "ldrsw"; break;
            default: return 0;
        }
    }
    unsigned scaled = imm << size;
    char rt_str[8], rn_str[8];
    reg_name(rt_str, sizeof(rt_str), dst_sf, (int)rt, 0);
    reg_name(rn_str, sizeof(rn_str), 1, (int)rn, 1);  /* base is X-form / SP */
    if (scaled == 0)
        snprintf(out, outsz, "%s %s, [%s]", mn, rt_str, rn_str);
    else
        snprintf(out, outsz, "%s %s, [%s, #%u]", mn, rt_str, rn_str, scaled);
    return 1;
}

/* EXTR Rd, Rn, Rm, #imms - when Rn == Rm this is the ROR-immediate
 * alias. Encoding:
 *   sf 00 100111 N 0 Rm imms Rn Rd
 *   (sf must equal N). */
static int
dec_extr(uint32_t insn, char *out, size_t outsz)
{
    unsigned sf = (insn >> 31) & 1;
    unsigned rm = (insn >> 16) & 0x1F;
    unsigned imms = (insn >> 10) & 0x3F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    char dst[8], srn[8], srm[8];
    reg_name(dst, sizeof(dst), (int)sf, (int)rd, 0);
    reg_name(srn, sizeof(srn), (int)sf, (int)rn, 0);
    reg_name(srm, sizeof(srm), (int)sf, (int)rm, 0);
    if (rn == rm)
        snprintf(out, outsz, "ror %s, %s, #%u", dst, srn, imms);
    else
        snprintf(out, outsz, "extr %s, %s, %s, #%u", dst, srn, srm, imms);
    return 1;
}

/* LDR literal - PC-relative load with imm19 byte offset (×4). Encoding:
 *   opc(2) 011 V 00 imm19 Rt
 *
 * opc=00 V=0: LDR Wt, label (32-bit), opc=01 V=0: LDR Xt, label,
 * opc=10 V=0: LDRSW Xt, label. */
static int
dec_ldr_lit(uint32_t insn, char *out, size_t outsz)
{
    unsigned opc = (insn >> 30) & 3;
    unsigned v = (insn >> 26) & 1;
    int32_t off = sx((insn >> 5) & 0x7FFFF, 19) * 4;
    unsigned rt = insn & 0x1F;
    if (v) return 0;     /* SIMD/FP literal load not handled */
    const char *mn;
    int sf;
    if (opc == 0)      { mn = "ldr";   sf = 0; }
    else if (opc == 1) { mn = "ldr";   sf = 1; }
    else if (opc == 2) { mn = "ldrsw"; sf = 1; }
    else return 0;
    char dst[8];
    reg_name(dst, sizeof(dst), sf, (int)rt, 0);
    snprintf(out, outsz, "%s %s, #%+d", mn, dst, off);
    return 1;
}

/* Exception generating instructions: BRK, SVC, HVC, SMC, etc.
 *   11010100 opc(3) imm16 LL(2) (where LL distinguishes the variant)
 * We cover BRK (#imm) and SVC (#imm). */
static int
dec_exception(uint32_t insn, char *out, size_t outsz)
{
    unsigned opc = (insn >> 21) & 0x7;
    unsigned imm16 = (insn >> 5) & 0xFFFF;
    unsigned ll = insn & 0x3;
    const char *mn;
    if (opc == 0 && ll == 1)      mn = "svc";
    else if (opc == 1 && ll == 0) mn = "brk";
    else if (opc == 2 && ll == 0) mn = "hlt";
    else return 0;
    snprintf(out, outsz, "%s #%u", mn, imm16);
    return 1;
}

/* LDR/STR (immediate, simm9): unscaled / pre-indexed / post-indexed.
 *
 *   size(2) 111 V 00 opc(2) 0 imm9(9) idx(2) Rn Rt
 *
 * idx bits[11:10]: 00 unscaled (LDUR/STUR), 01 post-indexed, 11 pre-indexed.
 * opc selects load/store and signedness; same convention as the
 * unsigned-offset form. */
static int
dec_ldst_imm9(uint32_t insn, char *out, size_t outsz)
{
    unsigned size = (insn >> 30) & 3;
    unsigned v    = (insn >> 26) & 1;
    unsigned opc  = (insn >> 22) & 3;
    int32_t simm9 = sx((insn >> 12) & 0x1FF, 9);
    unsigned idx  = (insn >> 10) & 3;
    unsigned rn   = (insn >> 5) & 0x1F;
    unsigned rt   = insn & 0x1F;
    if (v || idx == 2) return 0;   /* "register" form has idx=10, handled elsewhere */

    /* Pick mnemonic. */
    const char *mn = NULL;
    int dst_sf = (size == 3);
    if (opc == 0) {
        switch (size) {
            case 0: mn = "strb"; break;
            case 1: mn = "strh"; break;
            case 2: case 3: mn = "str"; break;
        }
    } else if (opc == 1) {
        switch (size) {
            case 0: mn = "ldrb"; break;
            case 1: mn = "ldrh"; break;
            case 2: case 3: mn = "ldr"; break;
        }
    } else {
        if (opc == 2) dst_sf = 1; else dst_sf = 0;
        switch (size) {
            case 0: mn = "ldrsb"; break;
            case 1: mn = "ldrsh"; break;
            case 2: mn = "ldrsw"; break;
            default: return 0;
        }
    }
    /* Unscaled forms prepend `u` for the load/store group. */
    char mn_buf[16];
    if (idx == 0) {
        snprintf(mn_buf, sizeof(mn_buf), "%cdur%s", mn[0], mn + 3);
        /* For "ldr"  -> "ldur"; "ldrb" -> "ldurb"; etc. */
        /* (Hand-roll above; simpler to just snprintf "%cdur%s") */
        if (mn[0] == 's' && mn[1] == 't') {
            /* str/strb/strh -> stur/sturb/sturh */
            snprintf(mn_buf, sizeof(mn_buf), "stur%s", mn + 3);
        } else if (mn[0] == 'l') {
            snprintf(mn_buf, sizeof(mn_buf), "ldur%s", mn + 3);
        }
        mn = mn_buf;
    }
    char rt_str[8], rn_str[8];
    reg_name(rt_str, sizeof(rt_str), dst_sf, (int)rt, 0);
    reg_name(rn_str, sizeof(rn_str), 1, (int)rn, 1);
    if (idx == 0)
        snprintf(out, outsz, "%s %s, [%s, #%d]", mn, rt_str, rn_str, simm9);
    else if (idx == 1)
        snprintf(out, outsz, "%s %s, [%s], #%d", mn, rt_str, rn_str, simm9);
    else /* idx == 3 */
        snprintf(out, outsz, "%s %s, [%s, #%d]!", mn, rt_str, rn_str, simm9);
    return 1;
}

/* STP/LDP variants. Encoding:
 *   opc(2) 101 V(1) X(2) L(1) imm7 Rt2 Rn Rt
 *
 * Where X selects non-temporal (00, offset addressing only),
 * post-indexed (01), offset (10), pre-indexed (11). L=1 LDP, L=0 STP.
 * V=0: opc 00 = 32-bit GPR, 10 = 64-bit GPR (01 is LDPSW - skipped).
 * V=1: opc selects the SIMD/FP width: 00 = S, 01 = D, 10 = Q. */
static int
dec_ldp_stp(uint32_t insn, char *out, size_t outsz)
{
    unsigned opc = (insn >> 30) & 3;
    unsigned v   = (insn >> 26) & 1;
    unsigned xm  = (insn >> 23) & 3;
    unsigned l   = (insn >> 22) & 1;
    unsigned imm7 = (insn >> 15) & 0x7F;
    unsigned rt2 = (insn >> 10) & 0x1F;
    unsigned rn  = (insn >> 5) & 0x1F;
    unsigned rt  = insn & 0x1F;
    if (opc == 3) return 0;            /* reserved */
    if (!v && opc == 1) return 0;      /* LDPSW - not covered */
    int scale;
    char rt_str[8], rt2_str[8], rn_str[8];
    if (v) {
        char wl = opc == 0 ? 's' : opc == 1 ? 'd' : 'q';
        scale = 4 << opc;
        snprintf(rt_str,  sizeof(rt_str),  "%c%u", wl, rt);
        snprintf(rt2_str, sizeof(rt2_str), "%c%u", wl, rt2);
    } else {
        int sf = (opc == 2);
        scale = sf ? 8 : 4;
        reg_name(rt_str,  sizeof(rt_str),  sf, (int)rt,  0);
        reg_name(rt2_str, sizeof(rt2_str), sf, (int)rt2, 0);
    }
    int32_t off = sx(imm7, 7) * scale;
    const char *mn;
    if (xm == 0) mn = l ? "ldnp" : "stnp";  /* non-temporal */
    else         mn = l ? "ldp"  : "stp";
    reg_name(rn_str, sizeof(rn_str), 1, (int)rn, 1);
    if (xm == 1)
        snprintf(out, outsz, "%s %s, %s, [%s], #%d", mn, rt_str, rt2_str, rn_str, off);
    else if (xm == 3)
        snprintf(out, outsz, "%s %s, %s, [%s, #%d]!", mn, rt_str, rt2_str, rn_str, off);
    else /* xm == 0 or 2: plain offset */
        snprintf(out, outsz, "%s %s, %s, [%s, #%d]", mn, rt_str, rt2_str, rn_str, off);
    return 1;
}

/* ============================================================== FP scalar
 *
 * Bit layout shared by most scalar FP instructions:
 *   0 0 0 11110 ftype 1 <sub-opcode>  Rn Rd
 *
 * where `ftype` (bits 23:22) selects the operand width:
 *   00 = single (S), 01 = double (D), 11 = half (H), 10 = reserved. */

static char
fp_reg_letter(unsigned ftype)
{
    switch (ftype) {
        case 0: return 's';
        case 1: return 'd';
        case 3: return 'h';
        default: return '?';
    }
}

static void
fp_reg_name(char *buf, size_t bufsz, unsigned ftype, unsigned r)
{
    snprintf(buf, bufsz, "%c%u", fp_reg_letter(ftype), r);
}

/* FP 1-source: FMOV reg, FABS, FNEG, FSQRT, FCVT, FRINTx. */
static int
dec_fp_1src(uint32_t insn, char *out, size_t outsz)
{
    unsigned ftype = (insn >> 22) & 3;
    unsigned opc6 = (insn >> 15) & 0x3F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    const char *mn;
    unsigned dst_ftype = ftype, src_ftype = ftype;
    switch (opc6) {
        case 0x00: mn = "fmov"; break;
        case 0x01: mn = "fabs"; break;
        case 0x02: mn = "fneg"; break;
        case 0x03: mn = "fsqrt"; break;
        /* FCVT: opc6 = 0001 ## where ## selects dst ftype.
         *   opc6=4 -> dst=S (00), opc6=5 -> dst=D (01), opc6=7 -> dst=H (11). */
        case 0x04: mn = "fcvt"; dst_ftype = 0; src_ftype = ftype; break;
        case 0x05: mn = "fcvt"; dst_ftype = 1; src_ftype = ftype; break;
        case 0x07: mn = "fcvt"; dst_ftype = 3; src_ftype = ftype; break;
        /* BFCVT scalar - special: dst is always H (16-bit BF), src S. */
        case 0x06: {
            snprintf(out, outsz, "bfcvt h%u, s%u", rd, rn);
            return 1;
        }
        case 0x08: mn = "frintn"; break;
        case 0x09: mn = "frintp"; break;
        case 0x0A: mn = "frintm"; break;
        case 0x0B: mn = "frintz"; break;
        case 0x0C: mn = "frinta"; break;
        case 0x0E: mn = "frintx"; break;
        case 0x0F: mn = "frinti"; break;
        default: return 0;
    }
    char dst[8], srn[8];
    fp_reg_name(dst, sizeof(dst), dst_ftype, rd);
    fp_reg_name(srn, sizeof(srn), src_ftype, rn);
    snprintf(out, outsz, "%s %s, %s", mn, dst, srn);
    return 1;
}

/* FP 2-source: FMUL, FDIV, FADD, FSUB, FMAX, FMIN, FNMUL etc. */
static int
dec_fp_2src(uint32_t insn, char *out, size_t outsz)
{
    unsigned ftype = (insn >> 22) & 3;
    unsigned rm = (insn >> 16) & 0x1F;
    unsigned opc4 = (insn >> 12) & 0xF;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    static const char *mns[16] = {
        "fmul", "fdiv", "fadd", "fsub",
        "fmax", "fmin", "fmaxnm", "fminnm",
        "fnmul", NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    };
    if (!mns[opc4]) return 0;
    char dst[8], srn[8], srm[8];
    fp_reg_name(dst, sizeof(dst), ftype, rd);
    fp_reg_name(srn, sizeof(srn), ftype, rn);
    fp_reg_name(srm, sizeof(srm), ftype, rm);
    snprintf(out, outsz, "%s %s, %s, %s", mns[opc4], dst, srn, srm);
    return 1;
}

/* FCMP / FCMPE, register or zero form. */
static int
dec_fcmp(uint32_t insn, char *out, size_t outsz)
{
    unsigned ftype = (insn >> 22) & 3;
    unsigned rm = (insn >> 16) & 0x1F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned opc2 = insn & 0x1F;
    int compare_zero = (opc2 >> 3) & 1;
    int with_exc = (opc2 >> 4) & 1;
    const char *mn = with_exc ? "fcmpe" : "fcmp";
    char srn[8];
    fp_reg_name(srn, sizeof(srn), ftype, rn);
    if (compare_zero) {
        snprintf(out, outsz, "%s %s, #0.0", mn, srn);
    } else {
        char srm[8];
        fp_reg_name(srm, sizeof(srm), ftype, rm);
        snprintf(out, outsz, "%s %s, %s", mn, srn, srm);
    }
    return 1;
}

/* FMOV immediate (scalar). The imm8 encodes a constant via the ARM
 * "FP modified immediate" scheme (VFPExpandImm):
 *   imm8 = a:bcd:efgh  ->  (-1)^a * (16+efgh)/16 * 2^exp
 * where exp = bcd&3 - 3 when b is set, bcd&3 + 1 when clear. Every
 * encodable value is an exact dyadic rational, so the decimal we print
 * is exact and re-assembles to the same imm8. */
static int
dec_fmov_imm(uint32_t insn, char *out, size_t outsz)
{
    unsigned ftype = (insn >> 22) & 3;
    unsigned imm8 = (insn >> 13) & 0xFF;
    unsigned rd = insn & 0x1F;
    char dst[8];
    fp_reg_name(dst, sizeof(dst), ftype, rd);
    int exp = (imm8 & 0x40) ? (int)((imm8 >> 4) & 3) - 3
                            : (int)((imm8 >> 4) & 3) + 1;
    double val = (16.0 + (imm8 & 0xF)) / 16.0;
    while (exp > 0) { val *= 2.0; exp--; }
    while (exp < 0) { val /= 2.0; exp++; }
    if (imm8 & 0x80) val = -val;
    char num[32];
    snprintf(num, sizeof(num), "%.9g", val);
    /* Keep a decimal point so the assembler lexes it as a float. */
    if (!strchr(num, '.')) strcat(num, ".0");
    snprintf(out, outsz, "fmov %s, #%s", dst, num);
    return 1;
}

/* FP <-> int conversion and the FP/GPR FMOV bridge.
 *   sf 0 0 11110 ftype 1 rmode opcode5 000000 Rn Rd
 * rmode_opcode5 picks the operation. */
static int
dec_fp_int_cvt(uint32_t insn, char *out, size_t outsz)
{
    unsigned sf = (insn >> 31) & 1;
    unsigned ftype = (insn >> 22) & 3;
    unsigned rmode_opcode5 = (insn >> 16) & 0x1F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;

    /* (rmode, opcode):  rmode = top 2 bits of rmode_opcode5,
     *                   opcode = low 3 bits. */
    unsigned rmode = (rmode_opcode5 >> 3) & 3;
    unsigned op = rmode_opcode5 & 7;

    /* FMOV between FP scalar and GPR: rmode = 00, opcode = 110 or 111. */
    if (rmode == 0 && (op == 6 || op == 7)) {
        char dst[8], srn[8];
        if (op == 6) {
            /* FP -> GPR */
            reg_name(dst, sizeof(dst), (int)sf, (int)rd, 0);
            fp_reg_name(srn, sizeof(srn), ftype, rn);
        } else {
            /* GPR -> FP */
            fp_reg_name(dst, sizeof(dst), ftype, rd);
            reg_name(srn, sizeof(srn), (int)sf, (int)rn, 0);
        }
        snprintf(out, outsz, "fmov %s, %s", dst, srn);
        return 1;
    }

    /* SCVTF / UCVTF: GPR -> FP. */
    if (rmode == 0 && (op == 2 || op == 3)) {
        char dst[8], srn[8];
        fp_reg_name(dst, sizeof(dst), ftype, rd);
        reg_name(srn, sizeof(srn), (int)sf, (int)rn, 0);
        snprintf(out, outsz, "%s %s, %s",
                op == 2 ? "scvtf" : "ucvtf", dst, srn);
        return 1;
    }

    /* FCVT* (FP -> int) family. opcode bit 0 selects signed (0) vs
     * unsigned (1); rmode picks the rounding mode. */
    if ((op & 6) == 0) {
        static const char *signed_mns[4]   = { "fcvtns", "fcvtps", "fcvtms", "fcvtzs" };
        static const char *unsigned_mns[4] = { "fcvtnu", "fcvtpu", "fcvtmu", "fcvtzu" };
        const char *mn = (op & 1) ? unsigned_mns[rmode] : signed_mns[rmode];
        char dst[8], srn[8];
        reg_name(dst, sizeof(dst), (int)sf, (int)rd, 0);
        fp_reg_name(srn, sizeof(srn), ftype, rn);
        snprintf(out, outsz, "%s %s, %s", mn, dst, srn);
        return 1;
    }

    /* FCVTA* (round to nearest, away): opcode = 100/101. */
    if (op == 4 || op == 5) {
        char dst[8], srn[8];
        reg_name(dst, sizeof(dst), (int)sf, (int)rd, 0);
        fp_reg_name(srn, sizeof(srn), ftype, rn);
        snprintf(out, outsz, "%s %s, %s",
                op == 4 ? "fcvtas" : "fcvtau", dst, srn);
        return 1;
    }
    return 0;
}

/* FP load/store (immediate, unsigned offset). Same shape as integer
 * LDR/STR uimm but with V=1 at bit 26. The size field combined with the
 * top bit of opc selects element width B/H/S/D/Q. */
static int
dec_fp_ldst_uimm(uint32_t insn, char *out, size_t outsz)
{
    unsigned size = (insn >> 30) & 3;
    unsigned opc = (insn >> 22) & 3;       /* bits [23:22] */
    unsigned imm12 = (insn >> 10) & 0xFFF;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rt = insn & 0x1F;
    /* opc encoding:
     *   opc=00 STR (B/H/S/D)
     *   opc=01 LDR (B/H/S/D)
     *   opc=10 STR Q (128-bit)
     *   opc=11 LDR Q (128-bit)
     * The element width comes from size (with opc top bit acting as a
     * 5th bit selecting Q-form). */
    int is_load = (opc & 1);
    int is_q = (opc >> 1) & 1;
    unsigned width;          /* in bytes */
    char letter;
    if (is_q) {
        if (size != 0) return 0;
        width = 16; letter = 'q';
    } else {
        switch (size) {
            case 0: width = 1;  letter = 'b'; break;
            case 1: width = 2;  letter = 'h'; break;
            case 2: width = 4;  letter = 's'; break;
            case 3: width = 8;  letter = 'd'; break;
            default: return 0;
        }
    }
    unsigned scaled = imm12 * width;
    char rt_str[8], rn_str[8];
    snprintf(rt_str, sizeof(rt_str), "%c%u", letter, rt);
    reg_name(rn_str, sizeof(rn_str), 1, (int)rn, 1);
    const char *mn = is_load ? "ldr" : "str";
    if (scaled == 0)
        snprintf(out, outsz, "%s %s, [%s]", mn, rt_str, rn_str);
    else
        snprintf(out, outsz, "%s %s, [%s, #%u]", mn, rt_str, rn_str, scaled);
    return 1;
}

/* ============================================================== vector / SIMD
 *
 * Vector mnemonics use `.<arr>` suffixes (e.g. `2s`, `4s`, `2d`) to
 * spell out the lane layout. For FP 3-same the (Q, sz) pair picks the
 * arrangement:
 *   Q=0 sz=0 -> 2s   (4-byte lanes, half register)
 *   Q=1 sz=0 -> 4s
 *   Q=1 sz=1 -> 2d
 *   Q=0 sz=1 -> reserved */

static const char *
fp_vec_arr(unsigned q, unsigned sz)
{
    if (sz == 0) return q ? "4s" : "2s";
    return q ? "2d" : "1d";
}

/* AdvSIMD floating-point 3-same:
 *   0 Q U 01110 bit23 sz 1 Rm opcode5 1 Rn Rd
 *
 * The (U, bit23, opcode5) triple selects the operation. We cover the
 * forms our encoders emit (FADD/FSUB/FMUL/FDIV) plus a few neighbours. */
static int
dec_fp_3same_vec(uint32_t insn, char *out, size_t outsz)
{
    unsigned q = (insn >> 30) & 1;
    unsigned u = (insn >> 29) & 1;
    unsigned bit23 = (insn >> 23) & 1;
    unsigned sz = (insn >> 22) & 1;
    unsigned rm = (insn >> 16) & 0x1F;
    unsigned opc5 = (insn >> 11) & 0x1F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    const char *mn = NULL;
    if (!u) {
        switch (opc5) {
            case 0x19: mn = bit23 ? "fmls"   : "fmla";   break;
            case 0x1A: mn = bit23 ? "fsub"   : "fadd";   break;
            case 0x1B: mn = bit23 ? "fminnm" : "fmaxnm"; break;
            case 0x1C: mn = bit23 ? "fcmgt"  : "fcmeq";  break;
            case 0x1E: mn = bit23 ? "fmin"   : "fmax";   break;
            case 0x1F: mn = bit23 ? "frsqrts": "frecps"; break;
            default: return 0;
        }
    } else {
        switch (opc5) {
            case 0x1A: mn = bit23 ? "fabd" : "faddp";  break;
            case 0x1B: if (bit23) return 0; mn = "fmul"; break;
            case 0x1C: mn = bit23 ? "facgt" : "fcmge"; break;
            case 0x1E: mn = bit23 ? "fminp" : "fmaxp"; break;
            case 0x1F: if (bit23) return 0; mn = "fdiv"; break;
            default: return 0;
        }
    }
    const char *arr = fp_vec_arr(q, sz);
    snprintf(out, outsz, "%s v%u.%s, v%u.%s, v%u.%s",
            mn, rd, arr, rn, arr, rm, arr);
    return 1;
}

/* BFDOT vector. Encoding (Armv8.6 BF16):
 *   0 Q 1 0 1 1 1 0 0 1 0 Rm 1 1 1 1 1 1 Rn Rd
 * Q=0 -> Vd.2S, Vn.4H, Vm.4H ; Q=1 -> Vd.4S, Vn.8H, Vm.8H */
static int
dec_bfdot(uint32_t insn, char *out, size_t outsz)
{
    unsigned q = (insn >> 30) & 1;
    unsigned rm = (insn >> 16) & 0x1F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    const char *vt = q ? "4s" : "2s";
    const char *vh = q ? "8h" : "4h";
    snprintf(out, outsz, "bfdot v%u.%s, v%u.%s, v%u.%s",
            rd, vt, rn, vh, rm, vh);
    return 1;
}

/* BFDOT (indexed-element). Encoding:
 *   0 Q 0 01111 0 1 L M Rm[3:0] 1111 H 0 Rn Rd
 *
 *   index = (H << 1) | L, with H at bit 11, L at bit 21. Rm is split:
 *   M is bit 20, Rm[3:0] occupies bits 19:16. Rm assembles to a SIMD
 *   register number in 0..31.  The Vm operand is always `.2h` and the
 *   index selects one of the two BF16 lanes within that pair. */
static int
dec_bfdot_idx(uint32_t insn, char *out, size_t outsz)
{
    unsigned q = (insn >> 30) & 1;
    unsigned l = (insn >> 21) & 1;
    unsigned m = (insn >> 20) & 1;
    unsigned rm4 = (insn >> 16) & 0xF;
    unsigned h = (insn >> 11) & 1;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    unsigned rm = (m << 4) | rm4;
    unsigned idx = (h << 1) | l;
    const char *vt = q ? "4s" : "2s";
    const char *vn = q ? "8h" : "4h";
    snprintf(out, outsz, "bfdot v%u.%s, v%u.%s, v%u.2h[%u]",
            rd, vt, rn, vn, rm, idx);
    return 1;
}

/* BFMMLA - BF16 matrix-multiply-accumulate (FEAT_BF16).
 *   0 1 1 01110 010 Rm 111011 Rn Rd
 * Operands are always Vd.4S, Vn.8H, Vm.8H. */
static int
dec_bfmmla(uint32_t insn, char *out, size_t outsz)
{
    unsigned rm = (insn >> 16) & 0x1F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    snprintf(out, outsz, "bfmmla v%u.4s, v%u.8h, v%u.8h", rd, rn, rm);
    return 1;
}

/* BFMLALB / BFMLALT. Encoding:
 *   0 0 1 0 1 1 1 0 1 1 0 Rm 1 1 1 1 1 1 Rn Rd  (BFMLALB, Q=0)
 *   0 1 1 0 1 1 1 0 1 1 0 Rm 1 1 1 1 1 1 Rn Rd  (BFMLALT, Q=1)
 * Always Vd.4S, Vn.8H, Vm.8H. */
static int
dec_bfmlal(uint32_t insn, char *out, size_t outsz)
{
    unsigned q = (insn >> 30) & 1;
    unsigned rm = (insn >> 16) & 0x1F;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    snprintf(out, outsz, "%s v%u.4s, v%u.8h, v%u.8h",
            q ? "bfmlalt" : "bfmlalb", rd, rn, rm);
    return 1;
}

/* BFCVTN / BFCVTN2 - narrow 4 S-lane floats to bf16 halves.
 *   0 Q 0 01110 10 10001 0110110 Rn Rd
 * Q=0 -> Vd.4H from Vn.4S ; Q=1 -> Vd.8H from Vn.4S. */
static int
dec_bfcvtn(uint32_t insn, char *out, size_t outsz)
{
    unsigned q = (insn >> 30) & 1;
    unsigned rn = (insn >> 5) & 0x1F;
    unsigned rd = insn & 0x1F;
    snprintf(out, outsz, "%s v%u.%s, v%u.4s",
            q ? "bfcvtn2" : "bfcvtn",
            rd, q ? "8h" : "4h", rn);
    return 1;
}

/* ------------------------------------------------------------ dispatcher */

typedef int (*A64Decoder)(uint32_t, char *, size_t);

struct A64Pat {
    uint32_t mask;
    uint32_t value;
    A64Decoder decode;
};

/* Patterns ordered from most-specific to most-general. Each must be
 * narrow enough that distinct ISA classes don't collide. */
static const struct A64Pat g_patterns[] = {
    /* HINT family (NOP / YIELD / WFE / WFI / SEV / SEVL / ...).
     * CRm at bits[11:8], op2 at bits[7:5]; bits[15:12] = CRn = 0010. */
    { 0xFFFFF01Fu, 0xD503201Fu, dec_hint },
    /* Memory barriers (DMB / DSB / ISB): CRn = 0011. */
    { 0xFFFFF01Fu, 0xD503301Fu, dec_barrier },
    /* MSR (immediate): bits[31:21]=11010101000, bit 20=0,
     * bits[15:12]=0100, bits[4:0]=11111. */
    { 0xFFF8F01Fu, 0xD500401Fu, dec_msr_imm },
    /* MRS / MSR register form: 1101 0101 00 L 1 ... */
    { 0xFFD00000u, 0xD5100000u, dec_mrs_msr },
    /* RET: 1101011001011111000000 Rn 00000 - fixed except Rn */
    { 0xFFFFFC1Fu, 0xD65F0000u, dec_ret },
    /* BR: 1101011000011111000000 Rn 00000 */
    { 0xFFFFFC1Fu, 0xD61F0000u, dec_br },
    /* BLR: 1101011000111111000000 Rn 00000 */
    { 0xFFFFFC1Fu, 0xD63F0000u, dec_blr },
    /* Exception generating (BRK, SVC, HLT). Top bits 11010100 ... */
    { 0xFF000000u, 0xD4000000u, dec_exception },
    /* LDAR / STLR (and byte/half variants): bit 23 = 1, bit 21 = 0,
     *   Rs and Rt2 fields are 11111, o0 = 1. */
    { 0x3F9FFC00u, 0x089FFC00u, dec_ldar_stlr },
    /* LDXR / STXR: bit 23 = 0, o0 = 0, Rt2 = 11111. */
    { 0x3F808000u, 0x08000000u, dec_ldxr_stxr },
    /* PRFM (immediate, unsigned offset): size=11, opc=10. */
    { 0xFFC00000u, 0xF9800000u, dec_prfm_imm },
    /* CSEL family (CSEL / CSINC / CSINV / CSNEG). bit 30 = op (variable),
     * bit 29 = S = 0, bits[28:21] = 11010100, bit 11 = 0. */
    { 0x3FE00800u, 0x1A800000u, dec_csel_family },
    /* CCMP / CCMN. bit 29 = S = 1, bits[28:21] = 11010010, bit 10 = 0. */
    { 0x3FE00400u, 0x3A400000u, dec_ccmp_ccmn },
    /* B.cond */
    { 0xFF000010u, 0x54000000u, dec_b_cond },
    /* CBZ / CBNZ (op bit 24 selects) */
    { 0x7E000000u, 0x34000000u, dec_cbz_cbnz },
    /* TBZ / TBNZ */
    { 0x7E000000u, 0x36000000u, dec_tbz_tbnz },
    /* B / BL */
    { 0x7C000000u, 0x14000000u, dec_b_bl },
    /* ADR / ADRP */
    { 0x1F000000u, 0x10000000u, dec_adr },
    /* MOVZ / MOVK / MOVN */
    { 0x1F800000u, 0x12800000u, dec_movz_movk_movn },
    /* SBFM / BFM / UBFM */
    { 0x1F800000u, 0x13000000u, dec_bfm },
    /* Logical (immediate, bitmask) */
    { 0x1F800000u, 0x12000000u, dec_logical_imm },
    /* ADD/SUB immediate */
    { 0x1F000000u, 0x11000000u, dec_add_sub_imm },
    /* Logical (shifted register): AND, ORR, EOR, ANDS (and BIC family with N=1) */
    { 0x1F000000u, 0x0A000000u, dec_logical_reg },
    /* ADD/SUB (extended register): bit 21 = 1 */
    { 0x1F200000u, 0x0B200000u, dec_add_sub_ext_reg },
    /* ADD/SUB (shifted register): bit 21 = 0 */
    { 0x1F200000u, 0x0B000000u, dec_add_sub_reg },
    /* Data-processing (3 source): MADD/MSUB/SMADDL/SMULL/UMADDL/UMULL/SMULH/UMULH */
    { 0x1F000000u, 0x1B000000u, dec_dp3 },
    /* Data-processing (1 source) - CLZ/CLS/REV/RBIT etc.
     * Encoding: sf 1 0 11010110 00000 op2(6) Rn Rd. bit 30 = 1. */
    { 0x7FFF0000u, 0x5AC00000u, dec_dp1 },
    /* Data-processing (2 source) - UDIV/SDIV/LSLV/LSRV/ASRV/RORV.
     * Encoding: sf 0 0 11010110 Rm op2(6) Rn Rd. bit 30 = 0. */
    { 0x7FE00000u, 0x1AC00000u, dec_dp2 },
    /* EXTR (and the ROR-immediate alias when Rn == Rm) */
    { 0x7FA00000u, 0x13800000u, dec_extr },
    /* LDP / STP / LDNP / STNP, GPR and SIMD&FP (V bit left free) */
    { 0x3A000000u, 0x28000000u, dec_ldp_stp },
    /* BF16: BFDOT (Vd.<2s|4s>, Vn.<4h|8h>, Vm.<4h|8h>).
     * Encoding: 0 Q 1 01110 0 1 0 Rm 111111 Rn Rd.
     * Distinguished from generic FP 3-same by bits[15:10]=111111. */
    { 0xBFE0FC00u, 0x2E40FC00u, dec_bfdot },
    /* BFDOT (indexed): 0 Q 0 01111 0 1 L M Rm[3:0] 1111 H 0 Rn Rd */
    { 0xBFC0F400u, 0x0F40F000u, dec_bfdot_idx },
    /* BFMLALB / BFMLALT: Q variant, bits[28:21] = 01110110.
     * Encoding: 0 Q 1 01110 1 1 0 Rm 111111 Rn Rd. */
    { 0xBFE0FC00u, 0x2EC0FC00u, dec_bfmlal },
    /* BFMMLA: 0 1 1 01110 010 Rm 111011 Rn Rd. */
    { 0xFFE0FC00u, 0x6E40EC00u, dec_bfmmla },
    /* BFCVTN / BFCVTN2: bits[31:30]=0 Q, bits[28:10]=011_1010_1000_1011_0110. */
    { 0xBFFFFC00u, 0x0EA16800u, dec_bfcvtn },
    /* AdvSIMD floating-point 3-same: bit[31]=0, bits[28:24]=01110,
     * bit[21]=1, bit[10]=1. */
    { 0x9F200400u, 0x0E200400u, dec_fp_3same_vec },
    /* FP scalar arithmetic family. Order from most-specific first.
     * All share bits[31:24]=00011110 and bit[21]=1. */
    /* FP 1-source: bits[14:10] = 10000. */
    { 0xFF207C00u, 0x1E204000u, dec_fp_1src },
    /* FP 2-source: bits[11:10] = 10. */
    { 0xFF200C00u, 0x1E200800u, dec_fp_2src },
    /* FCMP / FCMPE: bits[15:10] = 001000. */
    { 0xFF20FC00u, 0x1E202000u, dec_fcmp },
    /* FMOV (scalar, immediate): bits[12:5] = 1_0000_000 - that is
     * bits[12:10]=100, bits[9:5]=00000. */
    { 0xFF201FE0u, 0x1E201000u, dec_fmov_imm },
    /* FP <-> int convert and FMOV (FP <-> GPR): bits[15:10] = 0,
     * sf at bit 31. */
    { 0x7F20FC00u, 0x1E200000u, dec_fp_int_cvt },
    /* FP load/store immediate, unsigned offset: V=1 variant of the
     * integer LDR/STR uimm form. */
    { 0x3F000000u, 0x3D000000u, dec_fp_ldst_uimm },
    /* LDR/STR (register offset): bit 21=1, bits[11:10]=10. */
    { 0x3F200C00u, 0x38200800u, dec_ldst_regoff },
    /* LDR/STR (immediate, simm9): unscaled/pre/post-indexed.
     * Bits[29:24] = 111000, bit 21 = 0. */
    { 0x3F200400u, 0x38000400u, dec_ldst_imm9 },  /* idx bits include writeback */
    { 0x3F200C00u, 0x38000000u, dec_ldst_imm9 },  /* idx=00 (LDUR/STUR) */
    /* LDR literal: opc(2) 011 0 00 imm19 Rt; V=0. Bit 31 is part of
     * opc (LDRSW is opc=10, so bit 31 = 1) - leave it unmasked. */
    { 0x3F000000u, 0x18000000u, dec_ldr_lit },
    /* Load/store immediate unsigned offset (LDR/STR/LDRB/STRB/etc).
     * V=0 only - the FP variant lives in dec_fp_ldst_uimm above. */
    { 0x3F000000u, 0x39000000u, dec_ldst_uimm },
};

static const int g_n_patterns = (int)(sizeof(g_patterns) / sizeof(g_patterns[0]));

int
aarch64_disasm(uint32_t insn, char *out, size_t outsz)
{
    if (!out || outsz == 0) return 0;
    for (int i = 0; i < g_n_patterns; i++) {
        const struct A64Pat *p = &g_patterns[i];
        if ((insn & p->mask) == p->value) {
            if (p->decode(insn, out, outsz)) return 1;
        }
    }
    snprintf(out, outsz, ".word 0x%08x", insn);
    return 0;
}

void
aarch64_disasm_buf_at(const uint8_t *bytes, size_t len, uint64_t base, FILE *f)
{
    if (!f) f = stdout;
    for (size_t off = 0; off + 4 <= len; off += 4) {
        uint32_t insn = (uint32_t)bytes[off] |
                        ((uint32_t)bytes[off + 1] << 8) |
                        ((uint32_t)bytes[off + 2] << 16) |
                        ((uint32_t)bytes[off + 3] << 24);
        char text[128];
        aarch64_disasm(insn, text, sizeof(text));
        if (base)
            fprintf(f, "0x%llx: %08x  %s\n",
                    (unsigned long long)(base + off), insn, text);
        else
            fprintf(f, "%4zu: %08x  %s\n", off, insn, text);
    }
}

void
aarch64_disasm_buf(const uint8_t *bytes, size_t len, FILE *f)
{
    aarch64_disasm_buf_at(bytes, len, 0, f);
}
