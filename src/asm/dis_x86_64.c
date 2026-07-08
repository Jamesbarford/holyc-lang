/* In comparison to AArch64's disassembler this is pretty light. Intel manual
 * can be downloaded from;
 * https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html
 *
 * I don't use x86_64 enough, nor is this disassembler even hooked up which,
 * means working on this is more hassle than it's worth and an LLM takes over
 * a day to produce even a subset correctly as the encodings are not fixed
 * length which seems to confuse it. */
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#include "dis_x86_64.h"

/* x86_64 instruction layout, condensed:
 *
 *   [legacy prefix(es)] [REX] [opcode 1-3 bytes] [ModRM] [SIB] [disp] [imm]
 *
 * We handle:
 *   prefixes:  0x66 (operand-size override -> 16-bit), 0x67 (addr override,
 *              ignored), 0xF0 (LOCK, ignored), 0xF2/0xF3 (REP*, ignored
 *              unless they're part of a real opcode).
 *   REX:       0x40..0x4F  (W = bit3, R = bit2, X = bit1, B = bit0)
 *   opcode:    1-byte or 2-byte (0x0F xx).
 *   ModRM:     mod (2) | reg (3) | rm (3)
 *   SIB:       scale (2) | index (3) | base (3)
 *
 * Operand width is determined by:
 *   REX.W=1               -> 64-bit
 *   operand-size override -> 16-bit
 *   otherwise             -> 32-bit (default for most general-purpose ops)
 *
 * Each decoder writes its formatted text to `out` and returns the
 * number of bytes consumed. The dispatcher is a giant switch keyed on
 * the leading opcode byte. */

/* ------------------------------------------------------------ context */

typedef struct {
    const uint8_t *bytes;
    size_t len;
    size_t pos;
    /* prefixes */
    int rex_w, rex_r, rex_x, rex_b;
    int op16;       /* 0x66 prefix seen */
    int has_rex;
    int rep;        /* 0xF2 (REPNE) or 0xF3 (REP) for some 2-byte ops */
    /* output */
    char *out;
    size_t outsz;
} Ctx;

static int
take_byte(Ctx *c, uint8_t *out)
{
    if (c->pos >= c->len) return 0;
    *out = c->bytes[c->pos++];
    return 1;
}

static int
take_u16(Ctx *c, uint16_t *out)
{
    if (c->pos + 2 > c->len) return 0;
    *out = (uint16_t)c->bytes[c->pos] |
           ((uint16_t)c->bytes[c->pos + 1] << 8);
    c->pos += 2;
    return 1;
}

static int
take_u32(Ctx *c, uint32_t *out)
{
    if (c->pos + 4 > c->len) return 0;
    *out = (uint32_t)c->bytes[c->pos] |
           ((uint32_t)c->bytes[c->pos + 1] << 8) |
           ((uint32_t)c->bytes[c->pos + 2] << 16) |
           ((uint32_t)c->bytes[c->pos + 3] << 24);
    c->pos += 4;
    return 1;
}

static int
take_u64(Ctx *c, uint64_t *out)
{
    if (c->pos + 8 > c->len) return 0;
    uint64_t v = 0;
    for (int i = 0; i < 8; i++)
        v |= (uint64_t)c->bytes[c->pos + i] << (8 * i);
    c->pos += 8;
    *out = v;
    return 1;
}

/* ------------------------------------------------------------ register names */

/* Width: 1=byte, 2=word, 4=dword, 8=qword. With REX present any byte
 * register access uses the new SIL/DIL/BPL/SPL naming for regs 4-7
 * (vs the legacy AH/CH/DH/BH); we always assume REX-aware naming when
 * has_rex is set. */
static const char *
gpr_name(int reg, int width, int has_rex)
{
    static const char *q[16] = {
        "rax","rcx","rdx","rbx","rsp","rbp","rsi","rdi",
        "r8", "r9", "r10","r11","r12","r13","r14","r15"
    };
    static const char *d[16] = {
        "eax","ecx","edx","ebx","esp","ebp","esi","edi",
        "r8d","r9d","r10d","r11d","r12d","r13d","r14d","r15d"
    };
    static const char *w[16] = {
        "ax","cx","dx","bx","sp","bp","si","di",
        "r8w","r9w","r10w","r11w","r12w","r13w","r14w","r15w"
    };
    static const char *b_rex[16] = {
        "al","cl","dl","bl","spl","bpl","sil","dil",
        "r8b","r9b","r10b","r11b","r12b","r13b","r14b","r15b"
    };
    static const char *b_legacy[8] = {
        "al","cl","dl","bl","ah","ch","dh","bh"
    };
    if (reg < 0 || reg > 15) return "?";
    switch (width) {
        case 8: return q[reg];
        case 4: return d[reg];
        case 2: return w[reg];
        case 1:
            if (has_rex || reg >= 8) return b_rex[reg];
            return b_legacy[reg];
        default: return "?";
    }
}

/* ------------------------------------------------------------ memory operands */

/* Format an `[base + disp]` (or `[base + index*scale + disp]`) operand
 * string into `buf`. `mod` and `rm` are from the ModRM byte; the
 * function advances the ctx past any SIB and displacement bytes. */
static int
format_mem(Ctx *c, int mod, int rm_lo, char *buf, size_t bufsz)
{
    /* RIP-relative: mod=00, rm=101. disp32 follows. */
    if (mod == 0 && rm_lo == 5) {
        uint32_t disp;
        if (!take_u32(c, &disp)) return 0;
        int32_t d = (int32_t)disp;
        snprintf(buf, bufsz, "[rip%+d]", d);
        return 1;
    }
    /* SIB indicator: rm=100 (after REX.B is excluded - rm_lo is the
     * raw low 3 bits). */
    int base_reg = rm_lo + (c->rex_b ? 8 : 0);
    int has_sib = (rm_lo == 4);

    int sib_scale = 0, sib_index = -1, sib_base = base_reg;
    int sib_no_base = 0;
    if (has_sib) {
        uint8_t sib;
        if (!take_byte(c, &sib)) return 0;
        sib_scale = 1 << ((sib >> 6) & 3);
        int idx_lo = (sib >> 3) & 7;
        int base_lo = sib & 7;
        sib_index = idx_lo + (c->rex_x ? 8 : 0);
        if (idx_lo == 4 && !c->rex_x) sib_index = -1;  /* index=4 means none */
        sib_base = base_lo + (c->rex_b ? 8 : 0);
        /* mod==00 && sib_base low==5 means there's no base, disp32 follows. */
        if (mod == 0 && base_lo == 5) sib_no_base = 1;
    }

    int32_t disp = 0;
    if (mod == 1) {
        uint8_t d8;
        if (!take_byte(c, &d8)) return 0;
        disp = (int8_t)d8;
    } else if (mod == 2 || sib_no_base) {
        uint32_t d32;
        if (!take_u32(c, &d32)) return 0;
        disp = (int32_t)d32;
    }

    /* Build the operand string. */
    char inner[64];
    size_t off = 0;
    if (sib_no_base) {
        if (sib_index >= 0)
            off += (size_t)snprintf(inner + off, sizeof(inner) - off,
                    "%s*%d", gpr_name(sib_index, 8, c->has_rex), sib_scale);
        if (off == 0 || disp != 0)
            off += (size_t)snprintf(inner + off, sizeof(inner) - off,
                    "%s0x%x", off ? " + " : "", (unsigned)disp);
    } else {
        off += (size_t)snprintf(inner + off, sizeof(inner) - off,
                "%s", gpr_name(sib_base, 8, c->has_rex));
        if (has_sib && sib_index >= 0)
            off += (size_t)snprintf(inner + off, sizeof(inner) - off,
                    " + %s*%d", gpr_name(sib_index, 8, c->has_rex), sib_scale);
        if (disp != 0)
            off += (size_t)snprintf(inner + off, sizeof(inner) - off,
                    " %s %d", disp < 0 ? "-" : "+",
                    disp < 0 ? -disp : disp);
    }
    snprintf(buf, bufsz, "[%s]", inner);
    return 1;
}

/* ------------------------------------------------------------ ModRM */

/* Read a ModRM byte, expand it into (mod, reg, rm) with REX extensions
 * applied to reg and rm. If `mod != 3` then `rm_buf` receives the
 * formatted memory operand string (and ctx advances past SIB/disp).
 * If `mod == 3` rm is a register; the caller formats it. */
static int
read_modrm(Ctx *c, int *mod, int *reg, int *rm, char *rm_buf, size_t rm_bufsz)
{
    uint8_t b;
    if (!take_byte(c, &b)) return 0;
    *mod = (b >> 6) & 3;
    int reg_lo = (b >> 3) & 7;
    int rm_lo = b & 7;
    *reg = reg_lo + (c->rex_r ? 8 : 0);
    *rm = rm_lo + (c->rex_b ? 8 : 0);
    if (*mod != 3) {
        if (!format_mem(c, *mod, rm_lo, rm_buf, rm_bufsz)) return 0;
    }
    return 1;
}

/* ------------------------------------------------------------ condition codes */

static const char *
cc_name(int cc)
{
    static const char *n[16] = {
        "o","no","b","ae","e","ne","be","a",
        "s","ns","p","np","l","ge","le","g"
    };
    return n[cc & 0xF];
}

/* ------------------------------------------------------------ decoders */

static int
dec_alu_rm_r(Ctx *c, uint8_t op, const char *mn)
{
    /* op = base for r/m, r (e.g. 0x01 ADD r/m, r). The "alternate" form
     * (op+2 = 0x03 ADD r, r/m) has reg as destination instead - handled
     * by passing `op+2` in. We detect direction from op bit 1. */
    int dir = (op >> 1) & 1;     /* 1 = reg is dst, 0 = r/m is dst */
    int width = c->rex_w ? 8 : c->op16 ? 2 : 4;
    /* For the byte-form (low bit of opcode = 0 with even op-base 0x00,
     * 0x08, 0x10, etc), width is 1. */
    if ((op & 1) == 0) width = 1;
    int mod, reg, rm;
    char rm_buf[64];
    if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
    const char *rs = gpr_name(reg, width, c->has_rex);
    if (mod == 3) {
        const char *rd = gpr_name(rm, width, c->has_rex);
        if (dir) snprintf(c->out, c->outsz, "%s %s, %s", mn, rs, rd);
        else     snprintf(c->out, c->outsz, "%s %s, %s", mn, rd, rs);
    } else {
        if (dir) snprintf(c->out, c->outsz, "%s %s, %s", mn, rs, rm_buf);
        else     snprintf(c->out, c->outsz, "%s %s, %s", mn, rm_buf, rs);
    }
    return 1;
}

/* MOV instructions covering 0x88/89 (store) and 0x8A/8B (load), where
 * 0x88/0x8A are 8-bit and 0x89/0x8B are width-extended. */
static int
dec_mov_rm_r(Ctx *c, uint8_t op)
{
    return dec_alu_rm_r(c, op, "mov");
}

/* MOVABS r64, imm64 - 0xB8+r with REX.W. The non-W form is "mov r32, imm32". */
static int
dec_mov_imm_reg(Ctx *c, uint8_t op)
{
    int reg = (op & 7) + (c->rex_b ? 8 : 0);
    int width = c->rex_w ? 8 : c->op16 ? 2 : 4;
    uint64_t imm;
    if (width == 8) {
        if (!take_u64(c, &imm)) return 0;
    } else if (width == 4) {
        uint32_t v; if (!take_u32(c, &v)) return 0; imm = v;
    } else {
        uint16_t v; if (!take_u16(c, &v)) return 0; imm = v;
    }
    const char *rs = gpr_name(reg, width, c->has_rex);
    if (width == 8 && c->rex_w)
        snprintf(c->out, c->outsz, "movabs %s, 0x%llx",
                rs, (unsigned long long)imm);
    else
        snprintf(c->out, c->outsz, "mov %s, 0x%llx",
                rs, (unsigned long long)imm);
    return 1;
}

/* MOV r/m, imm32 (0xC7 /0) and MOV r/m8, imm8 (0xC6 /0). */
static int
dec_mov_rm_imm(Ctx *c, uint8_t op)
{
    int width = (op == 0xC6) ? 1 : (c->rex_w ? 8 : c->op16 ? 2 : 4);
    int mod, reg, rm;
    char rm_buf[64];
    if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
    if (reg != 0) return 0;  /* only /0 form here */
    int32_t imm;
    if (width == 1) {
        uint8_t v; if (!take_byte(c, &v)) return 0; imm = (int8_t)v;
    } else if (width == 2) {
        uint16_t v; if (!take_u16(c, &v)) return 0; imm = (int16_t)v;
    } else {
        uint32_t v; if (!take_u32(c, &v)) return 0; imm = (int32_t)v;
    }
    const char *dst = (mod == 3) ? gpr_name(rm, width, c->has_rex) : rm_buf;
    snprintf(c->out, c->outsz, "mov %s, %d", dst, imm);
    return 1;
}

/* ALU r/m, imm - opcode 0x80/0x81/0x83 with reg field selecting op:
 *   /0 ADD  /1 OR  /2 ADC  /3 SBB  /4 AND  /5 SUB  /6 XOR  /7 CMP. */
static int
dec_alu_rm_imm(Ctx *c, uint8_t op)
{
    static const char *mns[8] = { "add","or","adc","sbb","and","sub","xor","cmp" };
    int width = (op == 0x80) ? 1 : (c->rex_w ? 8 : c->op16 ? 2 : 4);
    int mod, reg, rm;
    char rm_buf[64];
    if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
    int32_t imm;
    if (op == 0x83) {
        uint8_t v; if (!take_byte(c, &v)) return 0; imm = (int8_t)v;
    } else if (width == 1) {
        uint8_t v; if (!take_byte(c, &v)) return 0; imm = (int8_t)v;
    } else if (width == 2) {
        uint16_t v; if (!take_u16(c, &v)) return 0; imm = (int16_t)v;
    } else {
        uint32_t v; if (!take_u32(c, &v)) return 0; imm = (int32_t)v;
    }
    const char *dst = (mod == 3) ? gpr_name(rm, width, c->has_rex) : rm_buf;
    snprintf(c->out, c->outsz, "%s %s, %d", mns[reg], dst, imm);
    return 1;
}

/* F7 /n - unary group: NEG/NOT/TEST/MUL/DIV/IDIV. */
static int
dec_f7(Ctx *c, uint8_t op)
{
    int width = (op == 0xF6) ? 1 : (c->rex_w ? 8 : c->op16 ? 2 : 4);
    int mod, reg, rm;
    char rm_buf[64];
    if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
    const char *dst = (mod == 3) ? gpr_name(rm, width, c->has_rex) : rm_buf;
    if (reg == 0) {
        /* test r/m, imm */
        int32_t imm;
        if (width == 1) { uint8_t v; if (!take_byte(c, &v)) return 0; imm = (int8_t)v; }
        else if (width == 2) { uint16_t v; if (!take_u16(c, &v)) return 0; imm = (int16_t)v; }
        else { uint32_t v; if (!take_u32(c, &v)) return 0; imm = (int32_t)v; }
        snprintf(c->out, c->outsz, "test %s, %d", dst, imm);
        return 1;
    }
    static const char *mns[8] = { NULL,NULL,"not","neg","mul","imul","div","idiv" };
    if (!mns[reg]) return 0;
    snprintf(c->out, c->outsz, "%s %s", mns[reg], dst);
    return 1;
}

/* D0/D1 /n - shift by 1; D2/D3 /n - shift by CL.
 * /4 SHL, /5 SHR, /6 SAL (alias of SHL), /7 SAR. */
static int
dec_shift_cl(Ctx *c, uint8_t op)
{
    int width = (op == 0xD0 || op == 0xD2) ? 1
              : (c->rex_w ? 8 : c->op16 ? 2 : 4);
    int by_one = (op == 0xD0 || op == 0xD1);
    int mod, reg, rm;
    char rm_buf[64];
    if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
    static const char *mns[8] = { "rol","ror","rcl","rcr","shl","shr","shl","sar" };
    const char *dst = (mod == 3) ? gpr_name(rm, width, c->has_rex) : rm_buf;
    snprintf(c->out, c->outsz, "%s %s, %s", mns[reg], dst, by_one ? "1" : "cl");
    return 1;
}

/* C1 /n - shift by imm8. */
static int
dec_shift_imm(Ctx *c, uint8_t op)
{
    int width = (op == 0xC0) ? 1 : (c->rex_w ? 8 : c->op16 ? 2 : 4);
    int mod, reg, rm;
    char rm_buf[64];
    if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
    uint8_t sh;
    if (!take_byte(c, &sh)) return 0;
    static const char *mns[8] = { "rol","ror","rcl","rcr","shl","shr","shl","sar" };
    const char *dst = (mod == 3) ? gpr_name(rm, width, c->has_rex) : rm_buf;
    snprintf(c->out, c->outsz, "%s %s, %u", mns[reg], dst, sh);
    return 1;
}

/* FF /n - group containing CALL r/m, JMP r/m, PUSH r/m, INC, DEC, etc. */
static int
dec_ff(Ctx *c)
{
    int mod, reg, rm;
    char rm_buf[64];
    if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
    int width = c->rex_w ? 8 : c->op16 ? 2 : 4;
    /* CALL r/m and JMP r/m are always 64-bit in long mode. */
    int width_for = (reg == 2 || reg == 4) ? 8 : width;
    const char *dst = (mod == 3) ? gpr_name(rm, width_for, c->has_rex) : rm_buf;
    static const char *mns[8] = { "inc","dec","call","callf","jmp","jmpf","push","??" };
    snprintf(c->out, c->outsz, "%s %s", mns[reg], dst);
    return 1;
}

/* ---------------- SSE/SSE2 scalar + packed-bitwise ----------------
 *
 * The 0F-map subset our encoder emits. The mandatory prefix (none, 66,
 * F2, F3) is captured by the legacy-prefix walk (c->rep / c->op16) and
 * selects the mnemonic; the opcode selects the operation:
 *
 *   10/11  movss, movsd (F3/F2), movups, movupd (none/66)  load/store
 *   28/29  movaps, movapd (none/66)                        load/store
 *   2A     cvtsi2ss, cvtsi2sd (F3/F2)  xmm <- r/m32 or r/m64 (REX.W)
 *   2C/2D  cvtt..2si, cvt..2si (F3/F2) r32/r64 <- xmm/m
 *   2E/2F  ucomiss, comiss (none), ucomisd, comisd (66)
 *   51     sqrtss, sqrtsd (F3/F2)
 *   54-57  andps, andnps, orps, xorps (none); the pd forms with 66
 *   58/59/5C/5D/5E/5F  add, mul, sub, min, div, max ss|sd (F3/F2)
 *   5A     cvtss2sd (F3), cvtsd2ss (F2)
 *
 * Prefix combinations the encoder never produces (e.g. packed addps)
 * return 0 so the caller degrades to `.byte` instead of guessing. */

static const char *
xmm_name(int reg)
{
    static const char *names[16] = {
        "xmm0",  "xmm1",  "xmm2",  "xmm3",
        "xmm4",  "xmm5",  "xmm6",  "xmm7",
        "xmm8",  "xmm9",  "xmm10", "xmm11",
        "xmm12", "xmm13", "xmm14", "xmm15",
    };
    return names[reg & 15];
}

static int
dec_sse(Ctx *c, uint8_t op2)
{
    /* Mandatory prefix: F2/F3 win over 66 (66 then acts as padding). */
    int pfx = c->rep ? c->rep : (c->op16 ? 0x66 : 0x00);
    const char *mn = NULL;
    int store = 0;        /* operand order is rm, reg */
    int gpr_dst = 0;      /* dst is a GPR (cvt*2si) */
    int gpr_src = 0;      /* src is a GPR (cvtsi2*) */

    switch (op2) {
        case 0x10: case 0x11:
            mn = pfx == 0xF3 ? "movss" : pfx == 0xF2 ? "movsd"
               : pfx == 0x66 ? "movupd" : "movups";
            store = (op2 == 0x11);
            break;
        case 0x28: case 0x29:
            if (pfx == 0xF2 || pfx == 0xF3) return 0;
            mn = pfx == 0x66 ? "movapd" : "movaps";
            store = (op2 == 0x29);
            break;
        case 0x2A:
            if (pfx == 0xF3) mn = "cvtsi2ss";
            else if (pfx == 0xF2) mn = "cvtsi2sd";
            else return 0;
            gpr_src = 1;
            break;
        case 0x2C:
            if (pfx == 0xF3) mn = "cvttss2si";
            else if (pfx == 0xF2) mn = "cvttsd2si";
            else return 0;
            gpr_dst = 1;
            break;
        case 0x2D:
            if (pfx == 0xF3) mn = "cvtss2si";
            else if (pfx == 0xF2) mn = "cvtsd2si";
            else return 0;
            gpr_dst = 1;
            break;
        case 0x2E:
            if (pfx == 0xF2 || pfx == 0xF3) return 0;
            mn = pfx == 0x66 ? "ucomisd" : "ucomiss";
            break;
        case 0x2F:
            if (pfx == 0xF2 || pfx == 0xF3) return 0;
            mn = pfx == 0x66 ? "comisd" : "comiss";
            break;
        case 0x51:
            if (pfx == 0xF3) mn = "sqrtss";
            else if (pfx == 0xF2) mn = "sqrtsd";
            else return 0;
            break;
        case 0x54: case 0x55: case 0x56: case 0x57: {
            if (pfx == 0xF2 || pfx == 0xF3) return 0;
            static const char *ps[4] = { "andps", "andnps", "orps", "xorps" };
            static const char *pd[4] = { "andpd", "andnpd", "orpd", "xorpd" };
            mn = (pfx == 0x66 ? pd : ps)[op2 - 0x54];
            break;
        }
        case 0x58: case 0x59: case 0x5C: case 0x5D: case 0x5E: case 0x5F: {
            static const char *ss[8] = { "addss", "mulss", NULL, NULL,
                                         "subss", "minss", "divss", "maxss" };
            static const char *sd[8] = { "addsd", "mulsd", NULL, NULL,
                                         "subsd", "minsd", "divsd", "maxsd" };
            if (pfx == 0xF3) mn = ss[op2 - 0x58];
            else if (pfx == 0xF2) mn = sd[op2 - 0x58];
            else return 0;
            break;
        }
        case 0x5A:
            if (pfx == 0xF3) mn = "cvtss2sd";
            else if (pfx == 0xF2) mn = "cvtsd2ss";
            else return 0;
            break;
        default:
            return 0;
    }
    if (!mn) return 0;

    int mod, reg, rm;
    char rm_buf[64];
    if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
    int gw = c->rex_w ? 8 : 4;
    const char *r_str = gpr_dst ? gpr_name(reg, gw, c->has_rex) : xmm_name(reg);
    const char *rm_str;
    if (mod == 3)
        rm_str = gpr_src ? gpr_name(rm, gw, c->has_rex) : xmm_name(rm);
    else
        rm_str = rm_buf;
    if (store)
        snprintf(c->out, c->outsz, "%s %s, %s", mn, rm_str, xmm_name(reg));
    else
        snprintf(c->out, c->outsz, "%s %s, %s", mn, r_str, rm_str);
    return 1;
}

/* MOVZX / MOVSX with two-byte opcode 0x0F 0xB6/B7/BE/BF.
 *
 *   0x0F 0xB6 MOVZX r, r/m8
 *   0x0F 0xB7 MOVZX r, r/m16
 *   0x0F 0xBE MOVSX r, r/m8
 *   0x0F 0xBF MOVSX r, r/m16 */
static int
dec_movzx_movsx(Ctx *c, uint8_t op2)
{
    int dst_width = c->rex_w ? 8 : c->op16 ? 2 : 4;
    int src_width = (op2 & 1) ? 2 : 1;
    int is_signed = (op2 & 8) != 0;
    int mod, reg, rm;
    char rm_buf[64];
    if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
    const char *dst = gpr_name(reg, dst_width, c->has_rex);
    const char *src = (mod == 3) ? gpr_name(rm, src_width, c->has_rex) : rm_buf;
    snprintf(c->out, c->outsz, "%s %s, %s",
            is_signed ? "movsx" : "movzx", dst, src);
    return 1;
}

/* SETcc r/m8. Opcode 0x0F 0x90+cc. */
static int
dec_setcc(Ctx *c, uint8_t op2)
{
    int cc = op2 & 0xF;
    int mod, reg, rm;
    char rm_buf[64];
    if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
    const char *dst = (mod == 3) ? gpr_name(rm, 1, c->has_rex) : rm_buf;
    snprintf(c->out, c->outsz, "set%s %s", cc_name(cc), dst);
    return 1;
}

/* MOVSXD r64, r/m32 - 0x63 with REX.W. */
static int
dec_movsxd(Ctx *c)
{
    int mod, reg, rm;
    char rm_buf[64];
    if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
    const char *dst = gpr_name(reg, 8, c->has_rex);
    const char *src = (mod == 3) ? gpr_name(rm, 4, c->has_rex) : rm_buf;
    snprintf(c->out, c->outsz, "movsxd %s, %s", dst, src);
    return 1;
}

/* LEA r, [mem] - 0x8D. */
static int
dec_lea(Ctx *c)
{
    int mod, reg, rm;
    char rm_buf[64];
    if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
    if (mod == 3) return 0;     /* LEA reg, reg is invalid */
    int width = c->rex_w ? 8 : c->op16 ? 2 : 4;
    snprintf(c->out, c->outsz, "lea %s, %s",
            gpr_name(reg, width, c->has_rex), rm_buf);
    return 1;
}

/* ------------------------------------------------------------ dispatch */

static int
decode_one(Ctx *c)
{
    /* Walk legacy prefixes. */
    while (c->pos < c->len) {
        uint8_t b = c->bytes[c->pos];
        if      (b == 0x66) { c->op16 = 1; c->pos++; }
        else if (b == 0x67) { c->pos++; }     /* addr-size override (ignored) */
        else if (b == 0xF0) { c->pos++; }     /* LOCK */
        else if (b == 0xF2 || b == 0xF3) { c->rep = b; c->pos++; }
        else break;
    }
    /* REX prefix? */
    if (c->pos < c->len && (c->bytes[c->pos] & 0xF0) == 0x40) {
        uint8_t rex = c->bytes[c->pos++];
        c->has_rex = 1;
        c->rex_w = (rex >> 3) & 1;
        c->rex_r = (rex >> 2) & 1;
        c->rex_x = (rex >> 1) & 1;
        c->rex_b = rex & 1;
    }
    uint8_t op;
    if (!take_byte(c, &op)) return 0;

    switch (op) {
        /* push reg / pop reg - single-byte opcode + reg in low 3 bits. */
        case 0x50: case 0x51: case 0x52: case 0x53:
        case 0x54: case 0x55: case 0x56: case 0x57: {
            int reg = (op & 7) + (c->rex_b ? 8 : 0);
            snprintf(c->out, c->outsz, "push %s",
                    gpr_name(reg, 8, c->has_rex));
            return 1;
        }
        case 0x58: case 0x59: case 0x5A: case 0x5B:
        case 0x5C: case 0x5D: case 0x5E: case 0x5F: {
            int reg = (op & 7) + (c->rex_b ? 8 : 0);
            snprintf(c->out, c->outsz, "pop %s",
                    gpr_name(reg, 8, c->has_rex));
            return 1;
        }
        /* ADD/OR/ADC/SBB/AND/SUB/XOR/CMP r/m, r and r, r/m. */
        case 0x00: case 0x01: case 0x02: case 0x03:
            return dec_alu_rm_r(c, op, "add");
        case 0x08: case 0x09: case 0x0A: case 0x0B:
            return dec_alu_rm_r(c, op, "or");
        case 0x10: case 0x11: case 0x12: case 0x13:
            return dec_alu_rm_r(c, op, "adc");
        case 0x18: case 0x19: case 0x1A: case 0x1B:
            return dec_alu_rm_r(c, op, "sbb");
        case 0x20: case 0x21: case 0x22: case 0x23:
            return dec_alu_rm_r(c, op, "and");
        case 0x28: case 0x29: case 0x2A: case 0x2B:
            return dec_alu_rm_r(c, op, "sub");
        case 0x30: case 0x31: case 0x32: case 0x33:
            return dec_alu_rm_r(c, op, "xor");
        case 0x38: case 0x39: case 0x3A: case 0x3B:
            return dec_alu_rm_r(c, op, "cmp");
        case 0x84: case 0x85:
            return dec_alu_rm_r(c, op, "test");
        /* MOV r/m, r and r, r/m. */
        case 0x88: case 0x89: case 0x8A: case 0x8B:
            return dec_mov_rm_r(c, op);
        /* LEA */
        case 0x8D:
            return dec_lea(c);
        /* MOVSXD */
        case 0x63:
            return dec_movsxd(c);
        /* MOV r8, imm8 (B0+r). */
        case 0xB0: case 0xB1: case 0xB2: case 0xB3:
        case 0xB4: case 0xB5: case 0xB6: case 0xB7: {
            uint8_t v;
            if (!take_byte(c, &v)) return 0;
            int reg = (op & 7) + (c->rex_b ? 8 : 0);
            snprintf(c->out, c->outsz, "mov %s, 0x%x",
                     gpr_name(reg, 1, c->has_rex), v);
            return 1;
        }
        /* MOV reg, imm (B8+r) - including MOVABS when REX.W set. */
        case 0xB8: case 0xB9: case 0xBA: case 0xBB:
        case 0xBC: case 0xBD: case 0xBE: case 0xBF:
            return dec_mov_imm_reg(c, op);
        /* ALU r/m, imm */
        case 0x80: case 0x81: case 0x83:
            return dec_alu_rm_imm(c, op);
        /* MOV r/m, imm */
        case 0xC6: case 0xC7:
            return dec_mov_rm_imm(c, op);
        /* Shift groups */
        case 0xD0: case 0xD1: /* shift by 1 */
        case 0xD2: case 0xD3:
            return dec_shift_cl(c, op);
        case 0xC0: case 0xC1:
            return dec_shift_imm(c, op);
        /* F6/F7 unary group */
        case 0xF6: case 0xF7:
            return dec_f7(c, op);
        /* FF group */
        case 0xFF:
            return dec_ff(c);
        /* CALL rel32 */
        case 0xE8: {
            uint32_t d; if (!take_u32(c, &d)) return 0;
            int32_t rel = (int32_t)d;
            snprintf(c->out, c->outsz, "call %+d", rel);
            return 1;
        }
        /* JMP rel32 / rel8 */
        case 0xE9: {
            uint32_t d; if (!take_u32(c, &d)) return 0;
            snprintf(c->out, c->outsz, "jmp %+d", (int32_t)d);
            return 1;
        }
        case 0xEB: {
            uint8_t d; if (!take_byte(c, &d)) return 0;
            snprintf(c->out, c->outsz, "jmp %+d", (int8_t)d);
            return 1;
        }
        /* Jcc rel8 (0x70+cc) */
        case 0x70: case 0x71: case 0x72: case 0x73:
        case 0x74: case 0x75: case 0x76: case 0x77:
        case 0x78: case 0x79: case 0x7A: case 0x7B:
        case 0x7C: case 0x7D: case 0x7E: case 0x7F: {
            uint8_t d; if (!take_byte(c, &d)) return 0;
            snprintf(c->out, c->outsz, "j%s %+d",
                    cc_name(op & 0xF), (int8_t)d);
            return 1;
        }
        /* RET / LEAVE */
        case 0xC3: snprintf(c->out, c->outsz, "ret"); return 1;
        /* RET imm16 - the assembler spells this `ret1 <n>` (or `retn <n>`). */
        case 0xC2: {
            uint16_t v;
            if (!take_u16(c, &v)) return 0;
            snprintf(c->out, c->outsz, "ret1 %u", v);
            return 1;
        }
        case 0xC9: snprintf(c->out, c->outsz, "leave"); return 1;
        /* MOVSB - `rep movsb` when REP-prefixed (the only form we emit). */
        case 0xA4:
            snprintf(c->out, c->outsz,
                     c->rep == 0xF3 ? "rep_movsb" : "movsb");
            return 1;
        /* CDQ (0x99 w/o REX.W) / CQO (0x99 with REX.W) */
        case 0x99:
            snprintf(c->out, c->outsz, "%s", c->rex_w ? "cqo" : "cdq");
            return 1;
        case 0x98:
            snprintf(c->out, c->outsz, "%s", c->rex_w ? "cdqe" : "cwde");
            return 1;
        case 0x90:
            snprintf(c->out, c->outsz, "nop"); return 1;
        case 0xF4:
            snprintf(c->out, c->outsz, "hlt"); return 1;
        /* Two-byte opcodes */
        case 0x0F: {
            uint8_t op2;
            if (!take_byte(c, &op2)) return 0;
            /* Jcc rel32 */
            if ((op2 & 0xF0) == 0x80) {
                uint32_t d; if (!take_u32(c, &d)) return 0;
                snprintf(c->out, c->outsz, "j%s %+d",
                        cc_name(op2 & 0xF), (int32_t)d);
                return 1;
            }
            /* SETcc r/m8 */
            if ((op2 & 0xF0) == 0x90)
                return dec_setcc(c, op2);
            /* SSE/SSE2 (0F 10-2F, 51-5F subset) */
            if ((op2 >= 0x10 && op2 <= 0x2F) || (op2 >= 0x51 && op2 <= 0x5F))
                return dec_sse(c, op2);
            /* MOVZX / MOVSX */
            if (op2 == 0xB6 || op2 == 0xB7 || op2 == 0xBE || op2 == 0xBF)
                return dec_movzx_movsx(c, op2);
            /* IMUL r, r/m */
            if (op2 == 0xAF) {
                int mod, reg, rm;
                char rm_buf[64];
                if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
                int width = c->rex_w ? 8 : c->op16 ? 2 : 4;
                const char *dst = gpr_name(reg, width, c->has_rex);
                const char *src = (mod == 3) ? gpr_name(rm, width, c->has_rex) : rm_buf;
                snprintf(c->out, c->outsz, "imul %s, %s", dst, src);
                return 1;
            }
            /* CMOVcc */
            if ((op2 & 0xF0) == 0x40) {
                int cc = op2 & 0xF;
                int mod, reg, rm;
                char rm_buf[64];
                if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
                int width = c->rex_w ? 8 : c->op16 ? 2 : 4;
                snprintf(c->out, c->outsz, "cmov%s %s, %s",
                        cc_name(cc),
                        gpr_name(reg, width, c->has_rex),
                        (mod == 3) ? gpr_name(rm, width, c->has_rex) : rm_buf);
                return 1;
            }
            /* SYSCALL */
            if (op2 == 0x05) { snprintf(c->out, c->outsz, "syscall"); return 1; }
            /* BSF / BSR */
            if (op2 == 0xBC || op2 == 0xBD) {
                int mod, reg, rm;
                char rm_buf[64];
                if (!read_modrm(c, &mod, &reg, &rm, rm_buf, sizeof(rm_buf))) return 0;
                int width = c->rex_w ? 8 : c->op16 ? 2 : 4;
                snprintf(c->out, c->outsz, "%s %s, %s",
                        op2 == 0xBC ? "bsf" : "bsr",
                        gpr_name(reg, width, c->has_rex),
                        (mod == 3) ? gpr_name(rm, width, c->has_rex) : rm_buf);
                return 1;
            }
            return 0;
        }
    }
    return 0;
}

/* ------------------------------------------------------------ public */

int
x86_64_disasm(const uint8_t *bytes, size_t len, char *out, size_t outsz)
{
    if (!bytes || !out || outsz == 0 || len == 0) return 0;
    Ctx c = { .bytes = bytes, .len = len, .pos = 0, .out = out, .outsz = outsz };
    if (!decode_one(&c) || c.pos == 0) {
        snprintf(out, outsz, ".byte 0x%02x", bytes[0]);
        return 1;   /* consume at least one byte so callers can advance */
    }
    return (int)c.pos;
}

void
x86_64_disasm_buf_at(const uint8_t *bytes, size_t len, uint64_t base, FILE *f)
{
    if (!f) f = stdout;
    size_t off = 0;
    while (off < len) {
        char text[128];
        int n = x86_64_disasm(bytes + off, len - off, text, sizeof(text));
        if (n <= 0) n = 1;
        /* Print byte offset (or base+off when a base is given), the raw
         * bytes, then the mnemonic. */
        if (base)
            fprintf(f, "0x%llx:", (unsigned long long)(base + off));
        else
            fprintf(f, "%4zu:", off);
        for (int i = 0; i < n; i++) fprintf(f, " %02x", bytes[off + i]);
        for (int i = n; i < 8; i++) fputs("   ", f);
        fprintf(f, "  %s\n", text);
        off += (size_t)n;
    }
}

void
x86_64_disasm_buf(const uint8_t *bytes, size_t len, FILE *f)
{
    x86_64_disasm_buf_at(bytes, len, 0, f);
}
