#include "enc_x86_64.h"
#include "asm_enc.h"
#include "tasm_util.h"

#include <stdlib.h>
#include <string.h>

/* x86 setcc / jcc 4-bit suffixes. */
#define CC_E  0x4
#define CC_NE 0x5
#define CC_L  0xC
#define CC_GE 0xD
#define CC_LE 0xE
#define CC_G  0xF
#define CC_Z  CC_E
#define CC_NZ CC_NE

#define REX_W 0x48
#define REX_R 0x44
#define REX_X 0x42
#define REX_B 0x41

/* X86Reg lives in enc_x86_64.h so the direct-emit JIT API can use it. */

/* Encoder state including the active resolver. We attach the resolver to
 * the AsmEnc temporarily during asm_encode so memory-operand encoders can
 * see it without explicit threading. */
static AsmResolver *g_resolver; /* set per-asm_encode call */

/* ================================================================ helpers */

static uint8_t
modrm(int mod, int reg, int rm)
{
    return (uint8_t)((mod << 6) | ((reg & 7) << 3) | (rm & 7));
}

/* REX.W is always set for our 64-bit ops. R/B bits extend the reg/rm fields. */
static uint8_t
rex_w(int r, int b)
{
    uint8_t v = REX_W;
    if (r >= 8) v |= REX_R;
    if (b >= 8) v |= REX_B;
    return v;
}

/* SIB byte 0x24 = scale=0, index=4 (none), base=4 (rsp). Used for [rsp]. */

/* Emit ModR/M + optional disp for `[base + disp]` addressing. Special-case
 * for rsp/r12 (need SIB) and for rbp/r13 with disp=0 (mod=01 disp8=0). */
static void
emit_mem_disp(AsmEnc *e, int reg, X86Reg base, int32_t disp)
{
    int br = base & 7;
    int need_sib = (br == 4); /* rsp / r12 */
    int mod;
    if (disp == 0 && br != 5) mod = 0;             /* [base] */
    else if (disp >= -128 && disp <= 127) mod = 1; /* [base + disp8] */
    else mod = 2;                                  /* [base + disp32] */

    put_byte(e, modrm(mod, reg, br));
    if (need_sib) put_byte(e, 0x24);
    if (mod == 1) put_byte(e, (uint8_t)(int8_t)disp);
    else if (mod == 2) put_u32(e, (uint32_t)disp);
}

/* ================================================================ mov */

void
x86_64_enc_mov_reg_reg(AsmEnc *e, X86Reg dst, X86Reg src)
{
    put_byte(e, rex_w(src, dst));
    put_byte(e, 0x89);
    put_byte(e, modrm(3, src & 7, dst & 7));
}

void
x86_64_enc_movabsq_imm_reg(AsmEnc *e, X86Reg dst, uint64_t imm)
{
    put_byte(e, rex_w(0, dst));
    put_byte(e, 0xB8 + (dst & 7));
    put_u64(e, imm);
}

void
x86_64_enc_mov_mem_rbp_reg(AsmEnc *e, X86Reg dst, int32_t disp)
{
    put_byte(e, rex_w(dst, 0));
    put_byte(e, 0x8B);
    emit_mem_disp(e, dst & 7, R_RBP, disp);
}

void
x86_64_enc_mov_reg_mem_rbp(AsmEnc *e, X86Reg src, int32_t disp)
{
    put_byte(e, rex_w(src, 0));
    put_byte(e, 0x89);
    emit_mem_disp(e, src & 7, R_RBP, disp);
}

void
x86_64_enc_mov_mem_reg(AsmEnc *e, X86Reg dst, X86Reg base)
{
    put_byte(e, rex_w(dst, base));
    put_byte(e, 0x8B);
    emit_mem_disp(e, dst & 7, base, 0);
}

void
x86_64_enc_mov_reg_mem(AsmEnc *e, X86Reg src, X86Reg base)
{
    put_byte(e, rex_w(src, base));
    put_byte(e, 0x89);
    emit_mem_disp(e, src & 7, base, 0);
}

void
x86_64_enc_mov_reg_mem_w(AsmEnc *e, int width, X86Reg src, X86Reg base)
{
    switch (width) {
    case 1:
        if (src >= 8 || base >= 8)
            put_byte(e,
                    0x40 | (src >= 8 ? REX_R : 0) | (base >= 8 ? REX_B : 0));
        put_byte(e, 0x88);
        emit_mem_disp(e, src & 7, base, 0);
        return;
    case 2:
        put_byte(e, 0x66); /* operand-size override */
        if (src >= 8 || base >= 8)
            put_byte(e,
                    0x40 | (src >= 8 ? REX_R : 0) | (base >= 8 ? REX_B : 0));
        put_byte(e, 0x89);
        emit_mem_disp(e, src & 7, base, 0);
        return;
    case 4:
        if (src >= 8 || base >= 8)
            put_byte(e,
                    0x40 | (src >= 8 ? REX_R : 0) | (base >= 8 ? REX_B : 0));
        put_byte(e, 0x89);
        emit_mem_disp(e, src & 7, base, 0);
        return;
    default: x86_64_enc_mov_reg_mem(e, src, base);
    }
}

void
x86_64_enc_movzx_mem_reg(AsmEnc *e, int width, X86Reg dst, X86Reg base)
{
    if (width >= 8) {
        x86_64_enc_mov_mem_reg(e, dst, base);
        return;
    }
    put_byte(e, rex_w(dst, base));
    if (width == 1) {
        put_byte(e, 0x0F);
        put_byte(e, 0xB6); /* movzx r64, r/m8 */
    } else if (width == 2) {
        put_byte(e, 0x0F);
        put_byte(e, 0xB7); /* movzx r64, r/m16 */
    } else {
        /* movl auto-zero-extends to 64-bit on x86_64. Emit movl + ModR/M. */
        put_byte(e, 0x8B);
        emit_mem_disp(e, dst & 7, base, 0);
        return;
    }
    emit_mem_disp(e, dst & 7, base, 0);
}

void
x86_64_enc_movsx_mem_reg(AsmEnc *e, int width, X86Reg dst, X86Reg base)
{
    if (width >= 8) {
        x86_64_enc_mov_mem_reg(e, dst, base);
        return;
    }
    put_byte(e, rex_w(dst, base));
    if (width == 1) {
        put_byte(e, 0x0F);
        put_byte(e, 0xBE); /* movsx r64, r/m8 */
    } else if (width == 2) {
        put_byte(e, 0x0F);
        put_byte(e, 0xBF); /* movsx r64, r/m16 */
    } else {
        put_byte(e, 0x63); /* movsxd r64, r/m32 */
    }
    emit_mem_disp(e, dst & 7, base, 0);
}

void
x86_64_enc_mov_reg_mem_rbp_w(AsmEnc *e, int width, X86Reg src, int32_t disp)
{
    switch (width) {
    case 1:
        if (src >= 8) put_byte(e, 0x40 | (src >= 8 ? REX_R : 0));
        put_byte(e, 0x88);
        emit_mem_disp(e, src & 7, R_RBP, disp);
        return;
    case 2:
        put_byte(e, 0x66);
        if (src >= 8) put_byte(e, 0x40 | (src >= 8 ? REX_R : 0));
        put_byte(e, 0x89);
        emit_mem_disp(e, src & 7, R_RBP, disp);
        return;
    case 4:
        if (src >= 8) put_byte(e, 0x40 | (src >= 8 ? REX_R : 0));
        put_byte(e, 0x89);
        emit_mem_disp(e, src & 7, R_RBP, disp);
        return;
    default: x86_64_enc_mov_reg_mem_rbp(e, src, disp);
    }
}

size_t
x86_64_enc_mov_rip_rel_reg(AsmEnc *e, X86Reg dst)
{
    put_byte(e, rex_w(dst, 0));
    put_byte(e, 0x8B);
    put_byte(e, modrm(0, dst & 7, 5)); /* RIP-relative */
    size_t disp_off = e->len;
    put_u32(e, 0);
    return disp_off;
}

size_t
x86_64_enc_lea_rip_rel_reg(AsmEnc *e, X86Reg dst)
{
    put_byte(e, rex_w(dst, 0));
    put_byte(e, 0x8D);
    put_byte(e, modrm(0, dst & 7, 5));
    size_t disp_off = e->len;
    put_u32(e, 0);
    return disp_off;
}

size_t
x86_64_enc_mov_reg_rip_rel(AsmEnc *e, X86Reg src)
{
    put_byte(e, rex_w(src, 0));
    put_byte(e, 0x89);
    put_byte(e, modrm(0, src & 7, 5));
    size_t disp_off = e->len;
    put_u32(e, 0);
    return disp_off;
}

void
x86_64_enc_lea_rbp_disp_reg(AsmEnc *e, X86Reg dst, int32_t disp)
{
    put_byte(e, rex_w(dst, 0));
    put_byte(e, 0x8D);
    emit_mem_disp(e, dst & 7, R_RBP, disp);
}

/* ================================================================ alu */

/* Map our operator char to the 64-bit reg/reg opcode byte. dst <- dst op src */
static uint8_t
alu_opcode(int op)
{
    switch (op) {
    case '+': return 0x01; /* add */
    case '-': return 0x29; /* sub */
    case '&': return 0x21; /* and */
    case '|': return 0x09; /* or */
    case '^': return 0x31; /* xor */
    }
    return 0x01;
}

void
x86_64_enc_alu_reg_reg(AsmEnc *e, int op, X86Reg dst, X86Reg src)
{
    put_byte(e, rex_w(src, dst));
    put_byte(e, alu_opcode(op));
    put_byte(e, modrm(3, src & 7, dst & 7));
}

void
x86_64_enc_addq_imm_reg(AsmEnc *e, X86Reg dst, int32_t imm)
{
    put_byte(e, rex_w(0, dst));
    if (imm >= -128 && imm <= 127) {
        put_byte(e, 0x83);
        put_byte(e, modrm(3, 0, dst & 7)); /* /0 = ADD */
        put_byte(e, (uint8_t)(int8_t)imm);
    } else {
        put_byte(e, 0x81);
        put_byte(e, modrm(3, 0, dst & 7));
        put_u32(e, (uint32_t)imm);
    }
}

void
x86_64_enc_subq_imm_reg(AsmEnc *e, X86Reg dst, int32_t imm)
{
    put_byte(e, rex_w(0, dst));
    if (imm >= -128 && imm <= 127) {
        put_byte(e, 0x83);
        put_byte(e, modrm(3, 5, dst & 7)); /* /5 = SUB */
        put_byte(e, (uint8_t)(int8_t)imm);
    } else {
        put_byte(e, 0x81);
        put_byte(e, modrm(3, 5, dst & 7));
        put_u32(e, (uint32_t)imm);
    }
}

void
x86_64_enc_neg_reg(AsmEnc *e, X86Reg reg)
{
    put_byte(e, rex_w(0, reg));
    put_byte(e, 0xF7);
    put_byte(e, modrm(3, 3, reg & 7)); /* /3 = NEG */
}

void
x86_64_enc_not_reg(AsmEnc *e, X86Reg reg)
{
    put_byte(e, rex_w(0, reg));
    put_byte(e, 0xF7);
    put_byte(e, modrm(3, 2, reg & 7)); /* /2 = NOT */
}

void
x86_64_enc_imul_reg_reg(AsmEnc *e, X86Reg dst, X86Reg src)
{
    put_byte(e, rex_w(dst, src));
    put_byte(e, 0x0F);
    put_byte(e, 0xAF);
    put_byte(e, modrm(3, dst & 7, src & 7));
}

void
x86_64_enc_idivq_reg(AsmEnc *e, X86Reg src)
{
    put_byte(e, rex_w(0, src));
    put_byte(e, 0xF7);
    put_byte(e, modrm(3, 7, src & 7)); /* /7 = IDIV */
}

void
x86_64_enc_cqto(AsmEnc *e)
{
    put_byte(e, 0x48);
    put_byte(e, 0x99);
}

/* ================================================================ shifts */

void
x86_64_enc_shlq_cl_reg(AsmEnc *e, X86Reg dst)
{
    put_byte(e, rex_w(0, dst));
    put_byte(e, 0xD3);
    put_byte(e, modrm(3, 4, dst & 7)); /* /4 = SHL */
}

void
x86_64_enc_shrq_cl_reg(AsmEnc *e, X86Reg dst)
{
    put_byte(e, rex_w(0, dst));
    put_byte(e, 0xD3);
    put_byte(e, modrm(3, 5, dst & 7)); /* /5 = SHR */
}

/* ================================================================
 * cmp/test/setcc */

void
x86_64_enc_cmp_reg_reg(AsmEnc *e, X86Reg lhs, X86Reg rhs)
{
    /* cmpq rhs, lhs encodes as 39 /r with reg=rhs, r/m=lhs. */
    put_byte(e, rex_w(rhs, lhs));
    put_byte(e, 0x39);
    put_byte(e, modrm(3, rhs & 7, lhs & 7));
}

void
x86_64_enc_test_reg_reg(AsmEnc *e, X86Reg a, X86Reg c)
{
    put_byte(e, rex_w(c, a));
    put_byte(e, 0x85);
    put_byte(e, modrm(3, c & 7, a & 7));
}

void
x86_64_enc_setcc_al(AsmEnc *e, int cc)
{
    put_byte(e, 0x0F);
    put_byte(e, (uint8_t)(0x90 | (cc & 0xF)));
    put_byte(e, modrm(3, 0, 0)); /* AL */
}

void
x86_64_enc_movzbq_al_rax(AsmEnc *e)
{
    put_byte(e, 0x48);
    put_byte(e, 0x0F);
    put_byte(e, 0xB6);
    put_byte(e, modrm(3, 0, 0));
}

void
x86_64_enc_xor_eax_eax(AsmEnc *e)
{
    put_byte(e, 0x31);
    put_byte(e, 0xC0);
}

/* ================================================================ control */

void
x86_64_enc_pushq_rbp(AsmEnc *e)
{
    put_byte(e, 0x55);
}

void
x86_64_enc_popq_rbp(AsmEnc *e)
{
    put_byte(e, 0x5D);
}
void
x86_64_enc_mov_rsp_rbp(AsmEnc *e)
{
    put_byte(e, 0x48);
    put_byte(e, 0x89);
    put_byte(e, 0xE5);
}
void
x86_64_enc_mov_rbp_rsp(AsmEnc *e)
{
    put_byte(e, 0x48);
    put_byte(e, 0x89);
    put_byte(e, 0xEC);
}
void
x86_64_enc_retq(AsmEnc *e)
{
    put_byte(e, 0xC3);
}

size_t
x86_64_enc_jmp_rel32(AsmEnc *e)
{
    put_byte(e, 0xE9);
    size_t off = e->len;
    put_u32(e, 0);
    return off;
}

size_t
x86_64_enc_jcc_rel32(AsmEnc *e, int cc)
{
    put_byte(e, 0x0F);
    put_byte(e, (uint8_t)(0x80 | (cc & 0xF)));
    size_t off = e->len;
    put_u32(e, 0);
    return off;
}

size_t
x86_64_enc_call_rel32(AsmEnc *e)
{
    put_byte(e, 0xE8);
    size_t off = e->len;
    put_u32(e, 0);
    return off;
}

void
x86_64_enc_call_reg(AsmEnc *e, X86Reg target)
{
    if (target >= 8) put_byte(e, 0x41); /* REX.B */
    put_byte(e, 0xFF);
    put_byte(e, modrm(3, 2, target & 7)); /* /2 = CALL r/m64 */
}

/* ===================================================== direct-emit (JIT) */

/* REX byte for an arbitrary reg/idx/base triple. `w` sets REX.W; idx < 0
 * means no SIB index. Returns 0 when no REX byte is needed. */
static uint8_t
rex_for(int w, int reg, int idx, int base)
{
    uint8_t v = (uint8_t)(w ? REX_W : 0);
    if (reg >= 8) v |= REX_R;
    if (idx >= 8) v |= REX_X;
    if (base >= 8) v |= REX_B;
    return (v || w) ? (uint8_t)(0x40 | (v & 0x0F)) : 0;
}

/* ModR/M (+ SIB) (+ disp) for `[base + idx*scale + disp]`. idx < 0 means
 * no index. rsp/r12 as base always need a SIB byte; rbp/r13 with disp=0
 * need mod=01 disp8=0. */
static void
emit_mem_sib(AsmEnc *e, int reg, X86Reg base, int idx, int scale, int32_t disp)
{
    int br = base & 7;
    int need_sib = (idx >= 0) || (br == 4);
    int mod;
    if (disp == 0 && br != 5) mod = 0;
    else if (disp >= -128 && disp <= 127) mod = 1;
    else mod = 2;

    put_byte(e, modrm(mod, reg, need_sib ? 4 : br));
    if (need_sib) {
        int ss = (scale == 8) ? 3 : (scale == 4) ? 2 : (scale == 2) ? 1 : 0;
        int ix = (idx >= 0) ? (idx & 7) : 4; /* index=4 means none */
        put_byte(e, (uint8_t)((ss << 6) | (ix << 3) | br));
    }
    if (mod == 1) put_byte(e, (uint8_t)(int8_t)disp);
    else if (mod == 2) put_u32(e, (uint32_t)disp);
}

void
x86_64_enc_mov32_reg_reg(AsmEnc *e, X86Reg dst, X86Reg src)
{
    uint8_t rex = rex_for(0, src, -1, dst);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x89);
    put_byte(e, modrm(3, src & 7, dst & 7));
}

void
x86_64_enc_xor32_reg(AsmEnc *e, X86Reg reg)
{
    uint8_t rex = rex_for(0, reg, -1, reg);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x31);
    put_byte(e, modrm(3, reg & 7, reg & 7));
}

void
x86_64_enc_mov_al_imm8(AsmEnc *e, uint8_t v)
{
    put_byte(e, 0xB0); /* MOV AL, imm8 */
    put_byte(e, v);
}

/* movzbq / movzwq r64, r/m8|16; width 4 lowers to `movl src, dst` whose
 * 32-bit write zero-extends. width 8 is a plain register move. */
void
x86_64_enc_movzx_reg_reg(AsmEnc *e, int width, X86Reg dst, X86Reg src)
{
    if (width == 4) { x86_64_enc_mov32_reg_reg(e, dst, src); return; }
    if (width >= 8) {
        if (dst != src) x86_64_enc_mov_reg_reg(e, dst, src);
        return;
    }
    put_byte(e, rex_for(1, dst, -1, src));
    put_byte(e, 0x0F);
    put_byte(e, width == 1 ? 0xB6 : 0xB7);
    put_byte(e, modrm(3, dst & 7, src & 7));
}

/* movsbq / movswq / movslq r64, r/m8|16|32. */
void
x86_64_enc_movsx_reg_reg(AsmEnc *e, int width, X86Reg dst, X86Reg src)
{
    if (width >= 8) {
        if (dst != src) x86_64_enc_mov_reg_reg(e, dst, src);
        return;
    }
    put_byte(e, rex_for(1, dst, -1, src));
    if (width == 4) {
        put_byte(e, 0x63); /* movsxd */
    } else {
        put_byte(e, 0x0F);
        put_byte(e, width == 1 ? 0xBE : 0xBF);
    }
    put_byte(e, modrm(3, dst & 7, src & 7));
}

/* Width-aware load with the AOT backend's extension rules:
 * 1 -> movzbq, 2 -> movzwq, 4 -> movslq, 8 -> movq. */
void
x86_64_enc_load_mem(AsmEnc *e, int width, X86Reg dst, X86Reg base,
                    int idx, int scale, int32_t disp)
{
    put_byte(e, rex_for(1, dst, idx, base));
    switch (width) {
    case 1: put_byte(e, 0x0F); put_byte(e, 0xB6); break;
    case 2: put_byte(e, 0x0F); put_byte(e, 0xB7); break;
    case 4: put_byte(e, 0x63); break;
    default: put_byte(e, 0x8B); break;
    }
    emit_mem_sib(e, dst & 7, base, idx, scale, disp);
}

void
x86_64_enc_store_mem(AsmEnc *e, int width, X86Reg src, X86Reg base,
                     int idx, int scale, int32_t disp)
{
    uint8_t rex;
    switch (width) {
    case 1:
        rex = rex_for(0, src, idx, base);
        /* sil/dil/spl/bpl need a (possibly empty) REX byte, else the
         * r/m8 field 4..7 selects the legacy ah..bh registers. */
        if (!rex && (src & 7) >= 4 && src < 8) rex = 0x40;
        if (rex) put_byte(e, rex);
        put_byte(e, 0x88);
        break;
    case 2:
        put_byte(e, 0x66); /* operand-size override */
        rex = rex_for(0, src, idx, base);
        if (rex) put_byte(e, rex);
        put_byte(e, 0x89);
        break;
    case 4:
        rex = rex_for(0, src, idx, base);
        if (rex) put_byte(e, rex);
        put_byte(e, 0x89);
        break;
    default:
        put_byte(e, rex_for(1, src, idx, base));
        put_byte(e, 0x89);
        break;
    }
    emit_mem_sib(e, src & 7, base, idx, scale, disp);
}

void
x86_64_enc_lea_mem(AsmEnc *e, X86Reg dst, X86Reg base,
                   int idx, int scale, int32_t disp)
{
    put_byte(e, rex_for(1, dst, idx, base));
    put_byte(e, 0x8D);
    emit_mem_sib(e, dst & 7, base, idx, scale, disp);
}

/* op $imm32, %reg for the five ALU ops; /digit per the 81 group. */
void
x86_64_enc_alu_imm_reg(AsmEnc *e, int op, X86Reg dst, int32_t imm)
{
    int digit;
    switch (op) {
    case '+': digit = 0; break;
    case '|': digit = 1; break;
    case '&': digit = 4; break;
    case '-': digit = 5; break;
    case '^': digit = 6; break;
    default:  digit = 0; break;
    }
    put_byte(e, rex_w(0, dst));
    if (imm >= -128 && imm <= 127) {
        put_byte(e, 0x83);
        put_byte(e, modrm(3, digit, dst & 7));
        put_byte(e, (uint8_t)(int8_t)imm);
    } else {
        put_byte(e, 0x81);
        put_byte(e, modrm(3, digit, dst & 7));
        put_u32(e, (uint32_t)imm);
    }
}

void
x86_64_enc_divq_reg(AsmEnc *e, X86Reg src)
{
    put_byte(e, rex_w(0, src));
    put_byte(e, 0xF7);
    put_byte(e, modrm(3, 6, src & 7)); /* /6 = DIV */
}

void
x86_64_enc_shift_cl_reg(AsmEnc *e, int kind, X86Reg dst)
{
    put_byte(e, rex_w(0, dst));
    put_byte(e, 0xD3);
    put_byte(e, modrm(3, kind, dst & 7));
}

void
x86_64_enc_shift_imm_reg(AsmEnc *e, int kind, X86Reg dst, uint8_t imm)
{
    put_byte(e, rex_w(0, dst));
    put_byte(e, 0xC1);
    put_byte(e, modrm(3, kind, dst & 7));
    put_byte(e, imm);
}

void
x86_64_enc_cmpq_imm_reg(AsmEnc *e, X86Reg lhs, int32_t imm)
{
    put_byte(e, rex_w(0, lhs));
    if (imm >= -128 && imm <= 127) {
        put_byte(e, 0x83);
        put_byte(e, modrm(3, 7, lhs & 7)); /* /7 = CMP */
        put_byte(e, (uint8_t)(int8_t)imm);
    } else {
        put_byte(e, 0x81);
        put_byte(e, modrm(3, 7, lhs & 7));
        put_u32(e, (uint32_t)imm);
    }
}

void
x86_64_enc_push_reg(AsmEnc *e, X86Reg reg)
{
    if (reg >= 8) put_byte(e, 0x41);
    put_byte(e, (uint8_t)(0x50 + (reg & 7)));
}

void
x86_64_enc_pop_reg(AsmEnc *e, X86Reg reg)
{
    if (reg >= 8) put_byte(e, 0x41);
    put_byte(e, (uint8_t)(0x58 + (reg & 7)));
}

/* ---- SSE2 scalar double ---- */

/* F2 0F <op> /r : addsd/subsd/mulsd/divsd xmm_dst, xmm_src. */
void
x86_64_enc_sse_arith(AsmEnc *e, uint8_t opcode, int dst_xmm, int src_xmm)
{
    put_byte(e, 0xF2);
    uint8_t rex = rex_for(0, dst_xmm, -1, src_xmm);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, opcode);
    put_byte(e, modrm(3, dst_xmm & 7, src_xmm & 7));
}

void
x86_64_enc_movsd_xmm_xmm(AsmEnc *e, int dst_xmm, int src_xmm)
{
    put_byte(e, 0xF2);
    uint8_t rex = rex_for(0, dst_xmm, -1, src_xmm);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x10);
    put_byte(e, modrm(3, dst_xmm & 7, src_xmm & 7));
}

void
x86_64_enc_movsd_load(AsmEnc *e, int xmm, X86Reg base,
                      int idx, int scale, int32_t disp)
{
    put_byte(e, 0xF2);
    uint8_t rex = rex_for(0, xmm, idx, base);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x10);
    emit_mem_sib(e, xmm & 7, base, idx, scale, disp);
}

void
x86_64_enc_movsd_store(AsmEnc *e, int xmm, X86Reg base,
                       int idx, int scale, int32_t disp)
{
    put_byte(e, 0xF2);
    uint8_t rex = rex_for(0, xmm, idx, base);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x11);
    emit_mem_sib(e, xmm & 7, base, idx, scale, disp);
}

/* movq %gpr, %xmm : 66 REX.W 0F 6E /r. */
void
x86_64_enc_movq_gpr_xmm(AsmEnc *e, int xmm, X86Reg gpr)
{
    put_byte(e, 0x66);
    put_byte(e, rex_for(1, xmm, -1, gpr));
    put_byte(e, 0x0F);
    put_byte(e, 0x6E);
    put_byte(e, modrm(3, xmm & 7, gpr & 7));
}

/* movq %xmm, %gpr : 66 REX.W 0F 7E /r. */
void
x86_64_enc_movq_xmm_gpr(AsmEnc *e, X86Reg gpr, int xmm)
{
    put_byte(e, 0x66);
    put_byte(e, rex_for(1, xmm, -1, gpr));
    put_byte(e, 0x0F);
    put_byte(e, 0x7E);
    put_byte(e, modrm(3, xmm & 7, gpr & 7));
}

void
x86_64_enc_ucomisd(AsmEnc *e, int a_xmm, int b_xmm)
{
    put_byte(e, 0x66);
    uint8_t rex = rex_for(0, a_xmm, -1, b_xmm);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x2E);
    put_byte(e, modrm(3, a_xmm & 7, b_xmm & 7));
}

void
x86_64_enc_xorpd(AsmEnc *e, int dst_xmm, int src_xmm)
{
    put_byte(e, 0x66);
    uint8_t rex = rex_for(0, dst_xmm, -1, src_xmm);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x57);
    put_byte(e, modrm(3, dst_xmm & 7, src_xmm & 7));
}

/* cvttsd2si %xmm, %r64 : F2 REX.W 0F 2C /r. */
void
x86_64_enc_cvttsd2si(AsmEnc *e, X86Reg dst, int src_xmm)
{
    put_byte(e, 0xF2);
    put_byte(e, rex_for(1, dst, -1, src_xmm));
    put_byte(e, 0x0F);
    put_byte(e, 0x2C);
    put_byte(e, modrm(3, dst & 7, src_xmm & 7));
}

/* cvtsi2sdq %r64, %xmm : F2 REX.W 0F 2A /r. */
void
x86_64_enc_cvtsi2sd(AsmEnc *e, int dst_xmm, X86Reg src)
{
    put_byte(e, 0xF2);
    put_byte(e, rex_for(1, dst_xmm, -1, src));
    put_byte(e, 0x0F);
    put_byte(e, 0x2A);
    put_byte(e, modrm(3, dst_xmm & 7, src & 7));
}

void
x86_64_enc_cvtsd2ss(AsmEnc *e, int dst_xmm, int src_xmm)
{
    put_byte(e, 0xF2);
    uint8_t rex = rex_for(0, dst_xmm, -1, src_xmm);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x5A);
    put_byte(e, modrm(3, dst_xmm & 7, src_xmm & 7));
}

void
x86_64_enc_cvtss2sd(AsmEnc *e, int dst_xmm, int src_xmm)
{
    put_byte(e, 0xF3);
    uint8_t rex = rex_for(0, dst_xmm, -1, src_xmm);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x5A);
    put_byte(e, modrm(3, dst_xmm & 7, src_xmm & 7));
}

/* ---- SSE scalar single (F32) ---- */

/* F3 0F <op> /r : addss/subss/mulss/divss xmm_dst, xmm_src.
 * Same opcode bytes as the *sd forms, single-precision F3 prefix. */
void
x86_64_enc_sse_arith_ss(AsmEnc *e, uint8_t opcode, int dst_xmm, int src_xmm)
{
    put_byte(e, 0xF3);
    uint8_t rex = rex_for(0, dst_xmm, -1, src_xmm);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, opcode);
    put_byte(e, modrm(3, dst_xmm & 7, src_xmm & 7));
}

void
x86_64_enc_movss_xmm_xmm(AsmEnc *e, int dst_xmm, int src_xmm)
{
    put_byte(e, 0xF3);
    uint8_t rex = rex_for(0, dst_xmm, -1, src_xmm);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x10);
    put_byte(e, modrm(3, dst_xmm & 7, src_xmm & 7));
}

void
x86_64_enc_movss_load(AsmEnc *e, int xmm, X86Reg base,
                      int idx, int scale, int32_t disp)
{
    put_byte(e, 0xF3);
    uint8_t rex = rex_for(0, xmm, idx, base);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x10);
    emit_mem_sib(e, xmm & 7, base, idx, scale, disp);
}

void
x86_64_enc_movss_store(AsmEnc *e, int xmm, X86Reg base,
                       int idx, int scale, int32_t disp)
{
    put_byte(e, 0xF3);
    uint8_t rex = rex_for(0, xmm, idx, base);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x11);
    emit_mem_sib(e, xmm & 7, base, idx, scale, disp);
}

/* movd %gpr, %xmm : 66 0F 6E /r (no REX.W -> 32-bit). */
void
x86_64_enc_movd_gpr_xmm(AsmEnc *e, int xmm, X86Reg gpr)
{
    put_byte(e, 0x66);
    uint8_t rex = rex_for(0, xmm, -1, gpr);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x6E);
    put_byte(e, modrm(3, xmm & 7, gpr & 7));
}

/* movd %xmm, %gpr : 66 0F 7E /r (no REX.W -> 32-bit). */
void
x86_64_enc_movd_xmm_gpr(AsmEnc *e, X86Reg gpr, int xmm)
{
    put_byte(e, 0x66);
    uint8_t rex = rex_for(0, xmm, -1, gpr);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x7E);
    put_byte(e, modrm(3, xmm & 7, gpr & 7));
}

/* ucomiss %b, %a : 0F 2E /r (no prefix). */
void
x86_64_enc_ucomiss(AsmEnc *e, int a_xmm, int b_xmm)
{
    uint8_t rex = rex_for(0, a_xmm, -1, b_xmm);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x2E);
    put_byte(e, modrm(3, a_xmm & 7, b_xmm & 7));
}

/* xorps %src, %dst : 0F 57 /r (no prefix). */
void
x86_64_enc_xorps(AsmEnc *e, int dst_xmm, int src_xmm)
{
    uint8_t rex = rex_for(0, dst_xmm, -1, src_xmm);
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, 0x57);
    put_byte(e, modrm(3, dst_xmm & 7, src_xmm & 7));
}

/* cvttss2si %xmm, %r64 : F3 REX.W 0F 2C /r. */
void
x86_64_enc_cvttss2si(AsmEnc *e, X86Reg dst, int src_xmm)
{
    put_byte(e, 0xF3);
    put_byte(e, rex_for(1, dst, -1, src_xmm));
    put_byte(e, 0x0F);
    put_byte(e, 0x2C);
    put_byte(e, modrm(3, dst & 7, src_xmm & 7));
}

/* cvtsi2ssq %r64, %xmm : F3 REX.W 0F 2A /r. */
void
x86_64_enc_cvtsi2ss(AsmEnc *e, int dst_xmm, X86Reg src)
{
    put_byte(e, 0xF3);
    put_byte(e, rex_for(1, dst_xmm, -1, src));
    put_byte(e, 0x0F);
    put_byte(e, 0x2A);
    put_byte(e, modrm(3, dst_xmm & 7, src & 7));
}

#define REX_W 0x48
#define REX_R 0x44
#define REX_X 0x42
#define REX_B 0x41

static uint8_t
x86_64_sib(int scale, int idx, int base)
{
    int s = scale == 1 ? 0 : scale == 2 ? 1 : scale == 4 ? 2 : 3;
    return (uint8_t)((s << 6) | ((idx & 7) << 3) | (base & 7));
}

static int
x86_64_reg_width(AsmReg r)
{
    switch (r.cls) {
        case AR_GPR8:
        case AR_AHHIGH: return 1;
        case AR_GPR16: return 2;
        case AR_GPR32: return 4;
        case AR_GPR64: return 8;
        default: return 0;
    }
}

/* Emit a segment override prefix if needed. */
static void
x86_64_emit_seg(AsmEnc *e, AsmReg seg)
{
    static const uint8_t prefix[] = {
        [0] = 0x26, /* ES */
        [1] = 0x2E, /* CS */
        [2] = 0x36, /* SS */
        [3] = 0x3E, /* DS */
        [4] = 0x64, /* FS */
        [5] = 0x65, /* GS */
    };
    if (seg.num >= 0 && seg.num <= 5) put_byte(e, prefix[seg.num]);
}

/* REX byte computed from operands. W=1 for 64-bit. */
static void
x86_64_emit_rex_for(AsmEnc *e, int w, int reg_num, AsmOperand *mem, int extra_b)
{
    uint8_t rex = 0;
    if (w) rex |= REX_W;
    if (reg_num >= 8) rex |= REX_R;
    if (mem) {
        if (mem->has_index && mem->index.num >= 8) rex |= REX_X;
        if (mem->has_base && mem->base.num >= 8) rex |= REX_B;
    }
    if (extra_b >= 0 && extra_b >= 8) rex |= REX_B;
    if (rex) put_byte(e, rex);
}

/* Emit a ModR/M and SIB byte (+ optional disp) for a memory operand. The
 * reg field is the OTHER operand's encoding (the register being moved). */
static void
x86_64_emit_modrm_mem(AsmEnc *e,
                      int reg_field,
                      AsmOperand *m,
                      int has_explicit_disp,
                      int32_t explicit_disp,
                      AsmFixup *late_fixup_out)
{
    int b = m->has_base ? (m->base.num & 7) : -1;
    int has_idx = m->has_index;
    int idx = has_idx ? (m->index.num & 7) : -1;
    int scale = has_idx ? (m->scale ? m->scale : 1) : 1;
    int64_t disp = has_explicit_disp ? explicit_disp : m->disp;

    /* Choose mod based on disp width and whether we need disp32 for a sym. */
    int needs_disp32 = late_fixup_out != NULL;
    int mod;
    if (!m->has_base) {
        /* Absolute: in 64-bit mode, use ModRM(00, reg, 100) + SIB(0, 100, 101)
         * + disp32. */
        put_byte(e, modrm(0, reg_field, 4));
        put_byte(e, x86_64_sib(1, 4, 5)); /* index=none, base=none */
        if (late_fixup_out) {
            late_fixup_out->patch_offset = e->len;
            put_u32(e, 0);
        } else put_u32(e, (uint32_t)disp);
        return;
    }
    if (!has_idx && b != 4 && b != 5 && disp == 0 && !needs_disp32) {
        mod = 0;
    } else if (!has_idx && disp >= -128 && disp <= 127 && !needs_disp32) {
        mod = 1;
    } else {
        mod = 2;
    }

    if (has_idx) {
        put_byte(e, modrm(mod, reg_field, 4));
        put_byte(e, x86_64_sib(scale, idx, b));
    } else if (b == 4) { /* RSP/R12 require SIB even without index */
        put_byte(e, modrm(mod, reg_field, 4));
        put_byte(e, x86_64_sib(1, 4, b));
    } else {
        put_byte(e, modrm(mod, reg_field, b));
    }

    if (mod == 1) put_byte(e, (uint8_t)(int8_t)disp);
    else if (mod == 2) {
        if (late_fixup_out) {
            late_fixup_out->patch_offset = e->len;
            put_u32(e, 0);
        } else put_u32(e, (uint32_t)disp);
    } else if (b == 5 && mod == 0) {
        /* RBP/R13 base with mod=00 is RIP-relative; we already chose mod=1
         * above. */
    }
}

/* ================================================================ bit ops */

void
x86_64_enc_bsf_reg_reg(AsmEnc *e, X86Reg dst, X86Reg src)
{
    put_byte(e, rex_w(dst, src));
    put_byte(e, 0x0F);
    put_byte(e, 0xBC);
    put_byte(e, modrm(3, dst & 7, src & 7));
}

void
x86_64_enc_bsr_reg_reg(AsmEnc *e, X86Reg dst, X86Reg src)
{
    put_byte(e, rex_w(dst, src));
    put_byte(e, 0x0F);
    put_byte(e, 0xBD);
    put_byte(e, modrm(3, dst & 7, src & 7));
}

/* Try to resolve a memory operand's symbolic disp to a numeric value. If
 * successful, *out is set and we return 1; the caller uses the disp
 * directly and skips emitting a fixup. */
static int
x86_64_try_resolve_mem_disp(AsmOperand *m, int64_t *out)
{
    if (!g_resolver) return 0;
    if (m->cls_name && m->member_name) {
        if (g_resolver->find_class_member &&
                g_resolver->find_class_member(g_resolver->ud, m->cls_name,
                        m->member_name, out))
            return 1;
    } else if (m->label_name) {
        if (g_resolver->find_macro &&
                g_resolver->find_macro(g_resolver->ud, m->label_name, out))
            return 1;
    }
    return 0;
}

/* If a memory operand has class.field or a symbolic label as its disp, it
 * needs a fixup unless the resolver can fold it into a literal disp32. */
static int
x86_64_mem_needs_fixup(AsmOperand *m)
{
    return (m->cls_name && m->member_name) || (m->label_name && m->has_base);
}

/* Record a fixup for a memory operand's disp32. */
static void
x86_64_emit_mem_fixup(AsmEnc *e, AsmOperand *m, size_t patch_at)
{
    AsmFixup f = { 0 };
    f.patch_offset = patch_at;
    f.width = 4;
    f.pcrel = 0;
    if (m->cls_name) {
        f.kind = AF_CLASS_MEMBER;
        f.cls_name = xstrdup(m->cls_name);
        f.member_name = xstrdup(m->member_name);
    } else {
        f.kind = AF_SYMBOL;
        f.sym = xstrdup(m->label_name);
    }
    asm_add_fixup(e, f);
}

/* Emit a memory operand starting at the ModR/M byte. The caller is
 * responsible for having already emitted the segment override (before REX)
 * and the REX prefix (before opcode). */
static void
x86_64_emit_memref(AsmEnc *e, int reg_field, AsmOperand *m)
{
    /* Try to resolve the symbolic disp to a constant first. */
    int64_t resolved;
    int has_resolved = x86_64_try_resolve_mem_disp(m, &resolved);

    if (x86_64_mem_needs_fixup(m) && !has_resolved) {
        int b = m->has_base ? (m->base.num & 7) : -1;
        if (b < 0) {
            /* Absolute via [disp32 + SIB]. */
            put_byte(e, modrm(0, reg_field, 4));
            put_byte(e, x86_64_sib(1, 4, 5));
            size_t patch = e->len;
            put_u32(e, 0);
            x86_64_emit_mem_fixup(e, m, patch);
            return;
        }
        /* [class.field + base] uses mod=10 + disp32. */
        put_byte(e, modrm(2, reg_field, b));
        if (b == 4) put_byte(e, x86_64_sib(1, 4, b));
        size_t patch = e->len;
        put_u32(e, 0);
        x86_64_emit_mem_fixup(e, m, patch);
        return;
    }
    int32_t disp = has_resolved ? (int32_t)(m->disp + resolved) :
                                  (int32_t)m->disp;
    x86_64_emit_modrm_mem(e, reg_field, m, 1, disp, NULL);
}

/* push reg64 - single-byte encoding 50+r. */
static void
x86_64_enc_push(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 1 || !asm_is_reg(&ln->operands[0]) ||
            ln->operands[0].reg.cls != AR_GPR64) {
        asm_err_at(e, ln, "push expects a 64-bit register");
        return;
    }
    int r = ln->operands[0].reg.num;
    if (r >= 8) put_byte(e, 0x41);
    put_byte(e, (uint8_t)(0x50 + (r & 7)));
}

static void
x86_64_enc_pop(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 1 || !asm_is_reg(&ln->operands[0]) ||
            ln->operands[0].reg.cls != AR_GPR64) {
        asm_err_at(e, ln, "pop expects a 64-bit register");
        return;
    }
    int r = ln->operands[0].reg.num;
    if (r >= 8) put_byte(e, 0x41);
    put_byte(e, (uint8_t)(0x58 + (r & 7)));
}

/* mov - handles reg-reg, reg-imm, reg-mem, mem-reg for 64-bit. */
static void
x86_64_enc_mov(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "mov needs 2 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    AsmOperand *s = &ln->operands[1];

    if (asm_is_reg(d) && asm_is_reg(s)) {
        int w = x86_64_reg_width(d->reg);
        if (w == 2) put_byte(e, 0x66);
        uint8_t rex = 0;
        if (w == 8) rex |= REX_W;
        if (s->reg.num >= 8) rex |= REX_R;
        if (d->reg.num >= 8) rex |= REX_B;
        if (rex || w == 1) {
            if (w == 1 && d->reg.cls != AR_AHHIGH && s->reg.cls != AR_AHHIGH) {
                /* SPL/BPL/SIL/DIL need REX even with no high bits set. */
                if (d->reg.num >= 4 || s->reg.num >= 4) rex |= 0x40;
            }
            if (rex) put_byte(e, rex);
        }
        put_byte(e, w == 1 ? 0x88 : 0x89);
        put_byte(e, modrm(3, s->reg.num, d->reg.num));
        return;
    }
    if (asm_is_reg(d) && asm_is_imm(s)) {
        int w = x86_64_reg_width(d->reg);
        /* For 64-bit dst and a sign-extendable 32-bit imm, use MOV r/m64, imm32
         * (REX.W + C7 /0 + imm32). Otherwise full 10-byte MOV r64, imm64. */
        if (w == 8) {
            int64_t v = s->imm;
            if (v >= INT32_MIN && v <= INT32_MAX) {
                uint8_t rex = REX_W;
                if (d->reg.num >= 8) rex |= REX_B;
                put_byte(e, rex);
                put_byte(e, 0xC7);
                put_byte(e, modrm(3, 0, d->reg.num));
                put_u32(e, (uint32_t)v);
            } else {
                uint8_t rex = REX_W;
                if (d->reg.num >= 8) rex |= REX_B;
                put_byte(e, rex);
                put_byte(e, (uint8_t)(0xB8 + (d->reg.num & 7)));
                for (int i = 0; i < 8; i++)
                    put_byte(e, (uint8_t)(v >> (8 * i)));
            }
            return;
        }
        if (w == 4) {
            if (d->reg.num >= 8) put_byte(e, 0x41);
            put_byte(e, (uint8_t)(0xB8 + (d->reg.num & 7)));
            put_u32(e, (uint32_t)s->imm);
            return;
        }
        if (w == 2) {
            put_byte(e, 0x66);
            if (d->reg.num >= 8) put_byte(e, 0x41);
            put_byte(e, (uint8_t)(0xB8 + (d->reg.num & 7)));
            put_u16(e, (uint16_t)s->imm);
            return;
        }
        if (w == 1) {
            uint8_t rex = 0;
            if (d->reg.num >= 8) rex |= REX_B;
            /* SPL/BPL/SIL/DIL need a bare REX (0x40) to encode - without it
             * the 4..7 slots in B0+r decode to AH/CH/DH/BH. AH-family regs
             * must NOT have any REX prefix. */
            if (d->reg.cls != AR_AHHIGH && d->reg.num >= 4 && d->reg.num < 8)
                rex |= 0x40;
            if (rex) put_byte(e, rex);
            put_byte(e, (uint8_t)(0xB0 + (d->reg.num & 7)));
            put_byte(e, (uint8_t)s->imm);
            return;
        }
    }
    if (asm_is_reg(d) && asm_is_mem(s)) {
        int w = x86_64_reg_width(d->reg);
        if (s->has_seg) x86_64_emit_seg(e, s->seg);
        x86_64_emit_rex_for(e, w == 8, d->reg.num, s, -1);
        put_byte(e, w == 1 ? 0x8A : 0x8B);
        x86_64_emit_memref(e, d->reg.num, s);
        return;
    }
    if (asm_is_mem(d) && asm_is_reg(s)) {
        int w = x86_64_reg_width(s->reg);
        if (d->has_seg) x86_64_emit_seg(e, d->seg);
        x86_64_emit_rex_for(e, w == 8, s->reg.num, d, -1);
        put_byte(e, w == 1 ? 0x88 : 0x89);
        x86_64_emit_memref(e, s->reg.num, d);
        return;
    }
    if (asm_is_mem(d) && asm_is_imm(s)) {
        /* Width from the mem operand's size hint (`U8 [..]` or a
         * suffixed mnemonic); TempleOS-style default is 64-bit. */
        int w = d->width ? d->width : 8;
        if (d->has_seg) x86_64_emit_seg(e, d->seg);
        if (w == 2) put_byte(e, 0x66);
        x86_64_emit_rex_for(e, w == 8, 0, d, -1);
        put_byte(e, w == 1 ? 0xC6 : 0xC7);
        x86_64_emit_memref(e, 0, d);
        if (w == 1)      put_byte(e, (uint8_t)s->imm);
        else if (w == 2) put_u16(e, (uint16_t)s->imm);
        else             put_u32(e, (uint32_t)s->imm);
        return;
    }
    asm_err_at(e, ln, "mov: unsupported operand combination");
}

/* Generic ALU ops: add/sub/xor/and/or, all the same shape but different
 * opcode bytes. */
typedef struct {
    uint8_t op_rr;     /* reg, reg */
    uint8_t op_rm;     /* reg, mem */
    uint8_t op_mr;     /* mem, reg */
    uint8_t imm_subop; /* ModR/M /<n> for the imm form */
} AluVariant;

static void
x86_64_enc_alu(AsmEnc *e, AsmLine *ln, const AluVariant *v)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "alu needs 2 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    AsmOperand *s = &ln->operands[1];
    if (asm_is_reg(d) && asm_is_reg(s)) {
        int w = x86_64_reg_width(d->reg);
        uint8_t rex = 0;
        if (w == 8) rex |= REX_W;
        if (s->reg.num >= 8) rex |= REX_R;
        if (d->reg.num >= 8) rex |= REX_B;
        if (rex) put_byte(e, rex);
        put_byte(e, v->op_rr);
        put_byte(e, modrm(3, s->reg.num, d->reg.num));
        return;
    }
    if (asm_is_reg(d) && asm_is_imm(s)) {
        int w = x86_64_reg_width(d->reg);
        if (w == 1) {
            /* 80 /digit ib - byte ALU with immediate. */
            uint8_t rex = 0;
            if (d->reg.num >= 8) rex |= REX_B;
            if (d->reg.cls != AR_AHHIGH && d->reg.num >= 4 && d->reg.num < 8)
                rex |= 0x40;
            if (rex) put_byte(e, rex);
            put_byte(e, 0x80);
            put_byte(e, modrm(3, v->imm_subop, d->reg.num));
            put_byte(e, (uint8_t)s->imm);
            return;
        }
        uint8_t rex = 0;
        if (w == 8) rex |= REX_W;
        if (d->reg.num >= 8) rex |= REX_B;
        int64_t v_imm = s->imm;
        if (w == 2) put_byte(e, 0x66);
        if (rex) put_byte(e, rex);
        if (v_imm >= -128 && v_imm <= 127) {
            put_byte(e, 0x83);
            put_byte(e, modrm(3, v->imm_subop, d->reg.num));
            put_byte(e, (uint8_t)(int8_t)v_imm);
        } else {
            put_byte(e, 0x81);
            put_byte(e, modrm(3, v->imm_subop, d->reg.num));
            put_u32(e, (uint32_t)v_imm);
        }
        return;
    }
    if (asm_is_reg(d) && asm_is_mem(s)) {
        int w = x86_64_reg_width(d->reg);
        if (s->has_seg) x86_64_emit_seg(e, s->seg);
        x86_64_emit_rex_for(e, w == 8, d->reg.num, s, -1);
        put_byte(e, v->op_rm);
        x86_64_emit_memref(e, d->reg.num, s);
        return;
    }
    if (asm_is_mem(d) && asm_is_reg(s)) {
        int w = x86_64_reg_width(s->reg);
        if (d->has_seg) x86_64_emit_seg(e, d->seg);
        x86_64_emit_rex_for(e, w == 8, s->reg.num, d, -1);
        put_byte(e, v->op_mr);
        x86_64_emit_memref(e, s->reg.num, d);
        return;
    }
    if (asm_is_mem(d) && asm_is_imm(s)) {
        /* Width from the mem operand's size hint (`U8 [..]` or a
         * suffixed mnemonic); TempleOS-style default is 64-bit. */
        int w = d->width ? d->width : 8;
        if (d->has_seg) x86_64_emit_seg(e, d->seg);
        if (w == 2) put_byte(e, 0x66);
        x86_64_emit_rex_for(e, w == 8, v->imm_subop, d, -1);
        if (w == 1) {
            put_byte(e, 0x80);
            x86_64_emit_memref(e, v->imm_subop, d);
            put_byte(e, (uint8_t)s->imm);
        } else if (s->imm >= -128 && s->imm <= 127) {
            put_byte(e, 0x83);
            x86_64_emit_memref(e, v->imm_subop, d);
            put_byte(e, (uint8_t)(int8_t)s->imm);
        } else {
            put_byte(e, 0x81);
            x86_64_emit_memref(e, v->imm_subop, d);
            if (w == 2) put_u16(e, (uint16_t)s->imm);
            else        put_u32(e, (uint32_t)s->imm);
        }
        return;
    }
    asm_err_at(e, ln, "alu: unsupported operand combination");
}

static const AluVariant x86_64_V_ADD = { 0x01, 0x03, 0x01, 0 };
static const AluVariant x86_64_V_SUB = { 0x29, 0x2B, 0x29, 5 };
static const AluVariant x86_64_V_AND = { 0x21, 0x23, 0x21, 4 };
static const AluVariant x86_64_V_OR = { 0x09, 0x0B, 0x09, 1 };
static const AluVariant x86_64_V_XOR = { 0x31, 0x33, 0x31, 6 };
static const AluVariant x86_64_V_CMP = { 0x39, 0x3B, 0x39, 7 };
static const AluVariant x86_64_V_ADC = { 0x11, 0x13, 0x11, 2 };
static const AluVariant x86_64_V_SBB = { 0x19, 0x1B, 0x19, 3 };

/* test reg, reg / test reg, imm */
static void
x86_64_enc_test(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "test needs 2 operands");
        return;
    }
    AsmOperand *a = &ln->operands[0];
    AsmOperand *b = &ln->operands[1];
    if (asm_is_reg(a) && asm_is_reg(b)) {
        int w = x86_64_reg_width(a->reg);
        uint8_t rex = 0;
        if (w == 8) rex |= REX_W;
        if (b->reg.num >= 8) rex |= REX_R;
        if (a->reg.num >= 8) rex |= REX_B;
        if (rex) put_byte(e, rex);
        put_byte(e, w == 1 ? 0x84 : 0x85);
        put_byte(e, modrm(3, b->reg.num, a->reg.num));
        return;
    }
    if (asm_is_reg(a) && asm_is_imm(b)) {
        int w = x86_64_reg_width(a->reg);
        uint8_t rex = 0;
        if (w == 8) rex |= REX_W;
        if (a->reg.num >= 8) rex |= REX_B;
        if (rex) put_byte(e, rex);
        put_byte(e, w == 1 ? 0xF6 : 0xF7);
        put_byte(e, modrm(3, 0, a->reg.num));
        if (w == 1) put_byte(e, (uint8_t)b->imm);
        else if (w == 2) {
            put_byte(e, 0x66);
            put_u16(e, (uint16_t)b->imm);
        } /* shouldn't fire - fix below */
        else
            put_u32(e, (uint32_t)b->imm);
        return;
    }
    asm_err_at(e, ln, "test: unsupported operand combination");
}

/* shl/shr/sar reg, imm or reg, %cl */
static void
x86_64_enc_shift(AsmEnc *e, AsmLine *ln, int subop)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "shift needs 2 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    AsmOperand *s = &ln->operands[1];
    if (!asm_is_reg(d)) {
        asm_err_at(e, ln, "shift dst must be a register");
        return;
    }
    int w = x86_64_reg_width(d->reg);
    uint8_t rex = 0;
    if (w == 8) rex |= REX_W;
    if (d->reg.num >= 8) rex |= REX_B;
    if (asm_is_imm(s)) {
        if (s->imm == 1) {
            if (rex) put_byte(e, rex);
            put_byte(e, 0xD1);
            put_byte(e, modrm(3, subop, d->reg.num));
        } else {
            if (rex) put_byte(e, rex);
            put_byte(e, 0xC1);
            put_byte(e, modrm(3, subop, d->reg.num));
            put_byte(e, (uint8_t)s->imm);
        }
        return;
    }
    if (asm_is_reg(s) && s->reg.cls == AR_GPR8 && s->reg.num == 1) {
        if (rex) put_byte(e, rex);
        put_byte(e, 0xD3);
        put_byte(e, modrm(3, subop, d->reg.num));
        return;
    }
    asm_err_at(e, ln, "shift: unsupported count operand (need imm or %cl)");
}

/* unconditional jmp rel32 to a local label */
static void
x86_64_enc_jmp_local(AsmEnc *e, int local_num)
{
    put_byte(e, 0xE9);
    AsmFixup f = { 0 };
    f.kind = AF_LOCAL;
    f.patch_offset = e->len;
    f.width = 4;
    f.pcrel = 1;
    f.addend_bytes = 0;
    f.local_num = local_num;
    asm_add_fixup(e, f);
    put_u32(e, 0);
}

static void
x86_64_enc_jcc_local(AsmEnc *e, int cc, int local_num)
{
    put_byte(e, 0x0F);
    put_byte(e, (uint8_t)(0x80 | (cc & 0xF)));
    AsmFixup f = { 0 };
    f.kind = AF_LOCAL;
    f.patch_offset = e->len;
    f.width = 4;
    f.pcrel = 1;
    f.local_num = local_num;
    asm_add_fixup(e, f);
    put_u32(e, 0);
}

/* Map a canonical jcc mnemonic id to its 4-bit condition code. Returns -1
 * if the id isn't a jcc. Alias mnemonics (jz/je, jc/jb/jnae, ...) are
 * collapsed to one id by the parser, so only the 16 canonical names
 * appear here. */
static int
x86_64_cc_for_mn(int id)
{
    switch (id) {
        case X86MN_JO:  return 0x0;
        case X86MN_JNO: return 0x1;
        case X86MN_JB:  return 0x2;
        case X86MN_JAE: return 0x3;
        case X86MN_JE:  return 0x4;
        case X86MN_JNE: return 0x5;
        case X86MN_JBE: return 0x6;
        case X86MN_JA:  return 0x7;
        case X86MN_JS:  return 0x8;
        case X86MN_JNS: return 0x9;
        case X86MN_JP:  return 0xA;
        case X86MN_JNP: return 0xB;
        case X86MN_JL:  return 0xC;
        case X86MN_JGE: return 0xD;
        case X86MN_JLE: return 0xE;
        case X86MN_JG:  return 0xF;
        default:        return -1;
    }
}

/* call reg, call imm32-or-label */
static void
x86_64_enc_call(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 1) {
        asm_err_at(e, ln, "call needs 1 operand");
        return;
    }
    AsmOperand *o = &ln->operands[0];
    if (asm_is_reg(o)) {
        int r = o->reg.num;
        if (r >= 8) put_byte(e, 0x41);
        put_byte(e, 0xFF);
        put_byte(e, modrm(3, 2, r));
        return;
    }
    if (asm_is_label(o)) {
        put_byte(e, 0xE8);
        AsmFixup f = { 0 };
        int n = asm_operand_local_num(o);
        if (n >= 0) {
            f.kind = AF_LOCAL;
            f.local_num = n;
        } else {
            f.kind = AF_SYMBOL;
            f.sym = xstrdup(o->label_name);
        }
        f.reloc = AFR_X86_64_CALL32;
        f.patch_offset = e->len;
        f.width = 4;
        f.pcrel = 1;
        asm_add_fixup(e, f);
        put_u32(e, 0);
        return;
    }
    asm_err_at(e, ln, "call: unsupported operand");
}

/* LEA r{16,32,64}, m
 *
 * Three forms:
 *   1. `lea r, [base + index*scale + disp]` - effective address; uses the
 *      same memref machinery as `mov r, [...]`.
 *   2. `lea r, _sym`  - bare label; encoded RIP-relative via the
 *      mod=00 / rm=101 special case (no SIB). Local labels (@@N) resolve
 *      at encode time via AF_LOCAL pcrel=1; external symbols leave an
 *      AFR_X86_64_SIGNED fixup for the linker.
 *   3. `lea r, sym[base]` - existing class.field / disp32 form via memref.
 *
 * Opcode is 0x8D. REX.W=1 for 64-bit dst; 0x66 prefix for 16-bit. 8-bit
 * destination is illegal. */
static void
x86_64_enc_lea(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "lea needs 2 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    AsmOperand *s = &ln->operands[1];
    if (!asm_is_reg(d)) {
        asm_err_at(e, ln, "lea: dst must be reg");
        return;
    }
    int w = x86_64_reg_width(d->reg);
    if (w != 2 && w != 4 && w != 8) {
        asm_err_at(e, ln, "lea: dst must be r16/r32/r64");
        return;
    }

    /* Bare-label form: lea r, _sym -> RIP-rel. */
    if (asm_is_label(s)) {
        if (w == 2) put_byte(e, 0x66);
        uint8_t rex = 0;
        if (w == 8) rex |= REX_W;
        if (d->reg.num >= 8) rex |= REX_R;
        if (rex) put_byte(e, rex);
        put_byte(e, 0x8D);
        /* mod=00, reg=dst&7, rm=101 -> [RIP + disp32]. */
        put_byte(e, modrm(0, d->reg.num & 7, 5));

        AsmFixup f = { 0 };
        int n = asm_operand_local_num(s);
        if (n >= 0) {
            f.kind = AF_LOCAL;
            f.local_num = n;
        } else {
            f.kind = AF_SYMBOL;
            f.sym = xstrdup(s->label_name);
        }
        f.reloc        = AFR_X86_64_SIGNED;
        f.patch_offset = e->len;
        f.width        = 4;
        f.pcrel        = 1;
        asm_add_fixup(e, f);
        put_u32(e, 0);
        return;
    }

    /* General [base + ...] form. */
    if (asm_is_mem(s)) {
        if (w == 2) put_byte(e, 0x66);
        if (s->has_seg) x86_64_emit_seg(e, s->seg);
        x86_64_emit_rex_for(e, w == 8, d->reg.num, s, -1);
        put_byte(e, 0x8D);
        x86_64_emit_memref(e, d->reg.num, s);
        return;
    }

    asm_err_at(e, ln, "lea: src must be [mem] or symbol");
}

/* ret / ret imm16 (callee-pops). RET1 is Terry's mnemonic for `ret imm16`. */
static void
x86_64_enc_ret(AsmEnc *e, AsmLine *ln, int with_imm)
{
    if (with_imm) {
        if (ln->n_operands != 1 || !asm_is_imm(&ln->operands[0])) {
            asm_err_at(e, ln, "ret1 needs imm16 operand");
            return;
        }
        put_byte(e, 0xC2);
        put_u16(e, (uint16_t)ln->operands[0].imm);
    } else {
        put_byte(e, 0xC3);
    }
}

/* neg/not reg */
static void
x86_64_enc_unary(AsmEnc *e, AsmLine *ln, int subop)
{
    if (ln->n_operands != 1 || !asm_is_reg(&ln->operands[0])) {
        asm_err_at(e, ln, "unary: needs 1 register operand");
        return;
    }
    AsmReg r = ln->operands[0].reg;
    int w = x86_64_reg_width(r);
    uint8_t rex = 0;
    if (w == 8) rex |= REX_W;
    if (r.num >= 8) rex |= REX_B;
    if (rex) put_byte(e, rex);
    put_byte(e, w == 1 ? 0xF6 : 0xF7);
    put_byte(e, modrm(3, subop, r.num));
}

static void
x86_64_enc_inc_dec(AsmEnc *e, AsmLine *ln, int subop)
{
    if (ln->n_operands != 1 || !asm_is_reg(&ln->operands[0])) {
        asm_err_at(e, ln, "inc/dec: needs 1 register operand");
        return;
    }
    AsmReg r = ln->operands[0].reg;
    int w = x86_64_reg_width(r);
    uint8_t rex = 0;
    if (w == 8) rex |= REX_W;
    if (r.num >= 8) rex |= REX_B;
    if (rex) put_byte(e, rex);
    put_byte(e, w == 1 ? 0xFE : 0xFF);
    put_byte(e, modrm(3, subop, r.num));
}

/* ================================================================ x87 FPU */

/* Helper for the common pattern: instruction takes either an x87 reg ST(i)
 * (encoded as op1 op2_base+i) or a memory operand of either m32 or m64 form
 * (encoded as <first_byte> + ModR/M with /<subop>). */
static void
x86_64_enc_x87_mem_or_reg(AsmEnc *e, AsmLine *ln, uint8_t mem32_first,
        uint8_t mem64_first, int subop, uint8_t reg_first, uint8_t reg_base)
{
    if (ln->n_operands != 1) {
        asm_err_at(e, ln, "x87: needs 1 operand");
        return;
    }
    AsmOperand *o = &ln->operands[0];
    if (asm_is_mem(o)) {
        int w = o->width ? o->width : 8;
        if (o->has_seg) x86_64_emit_seg(e, o->seg);
        x86_64_emit_rex_for(e, 0, 0, o, -1);
        if (w == 4) put_byte(e, mem32_first);
        else if (w == 8) put_byte(e, mem64_first);
        else {
            asm_err_at(e, ln, "x87: unsupported memory width");
            return;
        }
        x86_64_emit_memref(e, subop, o);
        return;
    }
    if (asm_is_reg(o) && o->reg.cls == AR_X87) {
        put_byte(e, reg_first);
        put_byte(e, (uint8_t)(reg_base + (o->reg.num & 7)));
        return;
    }
    asm_err_at(e, ln, "x87: bad operand");
}

/* For the arithmetic family (FADD/FSUB/FSUBR/FMUL/FDIV/FDIVR), operand
 * shapes can be:
 *   FOP m32/m64           - ST(0) = ST(0) OP mem
 *   FOP ST(0), m32/m64    - same
 *   FOP ST(0), ST(i)      - D8 (base+i)
 *   FOP ST(i), ST(0)      - DC (alt-base+i)
 *   FOP                   - implicit FOP ST(1), ST(0): DE form (FADDP etc.)
 *
 * The `subop` is the ModR/M /N for mem forms. The base bytes encode which
 * arithmetic op for the reg forms. */
typedef struct {
    int subop;       /* /N for mem forms */
    uint8_t d8_base; /* second byte for D8 reg form (ST(0) op ST(i)) */
    uint8_t dc_base; /* second byte for DC reg form (ST(i) op ST(0)) */
    uint8_t de_base; /* second byte for DE pop form */
} FpuArith;

/* Matches Intel SDM tables for D8/DC/DE encodings. */
static const FpuArith x86_64_FA_ADD = { 0, 0xC0, 0xC0, 0xC0 };
static const FpuArith x86_64_FA_MUL = { 1, 0xC8, 0xC8, 0xC8 };
static const FpuArith x86_64_FA_SUB = { 4, 0xE0, 0xE8, 0xE8 };
static const FpuArith x86_64_FA_SUBR = { 5, 0xE8, 0xE0, 0xE0 };
static const FpuArith x86_64_FA_DIV = { 6, 0xF0, 0xF8, 0xF8 };
static const FpuArith x86_64_FA_DIVR = { 7, 0xF8, 0xF0, 0xF0 };

static void
x86_64_enc_x87_arith(AsmEnc *e, AsmLine *ln, const FpuArith *a, int pop)
{
    /* No operands: FADDP / FSUBP / FMULP / FDIVP with implicit ST(1), ST(0). */
    if (ln->n_operands == 0) {
        if (!pop) {
            asm_err_at(e, ln, "arith: needs operands");
            return;
        }
        put_byte(e, 0xDE);
        put_byte(e, (uint8_t)(a->de_base + 1)); /* ST(1), ST(0) */
        return;
    }
    if (ln->n_operands == 1 && asm_is_mem(&ln->operands[0])) {
        /* Memory operand -> ST(0) = ST(0) OP mem (m32 D8, m64 DC). */
        AsmOperand *m = &ln->operands[0];
        int w = m->width ? m->width : 8;
        if (m->has_seg) x86_64_emit_seg(e, m->seg);
        x86_64_emit_rex_for(e, 0, 0, m, -1);
        if (w == 4) put_byte(e, 0xD8);
        else if (w == 8) put_byte(e, 0xDC);
        else {
            asm_err_at(e, ln, "arith: bad memory width");
            return;
        }
        x86_64_emit_memref(e, a->subop, m);
        return;
    }
    if (ln->n_operands == 2 && asm_is_reg(&ln->operands[0]) &&
            ln->operands[0].reg.cls == AR_X87 && asm_is_mem(&ln->operands[1])) {
        /* FOP ST(0), mem - same encoding as the 1-arg mem form, but the
         * dst must be ST(0) (we don't model ST(i),mem). */
        if (ln->operands[0].reg.num != 0) {
            asm_err_at(e, ln, "arith ST(i), mem only supported when i=0");
            return;
        }
        AsmOperand *m = &ln->operands[1];
        int w = m->width ? m->width : 8;
        if (m->has_seg) x86_64_emit_seg(e, m->seg);
        x86_64_emit_rex_for(e, 0, 0, m, -1);
        if (w == 4) put_byte(e, 0xD8);
        else if (w == 8) put_byte(e, 0xDC);
        else {
            asm_err_at(e, ln, "arith: bad memory width");
            return;
        }
        x86_64_emit_memref(e, a->subop, m);
        return;
    }
    if (ln->n_operands == 2 && asm_is_reg(&ln->operands[0]) &&
            asm_is_reg(&ln->operands[1]) && ln->operands[0].reg.cls == AR_X87 &&
            ln->operands[1].reg.cls == AR_X87) {
        AsmReg d = ln->operands[0].reg;
        AsmReg s = ln->operands[1].reg;
        if (pop) {
            /* FOPP ST(i), ST(0) -> DE base+i */
            if (s.num != 0) {
                asm_err_at(e, ln, "popping form: src must be ST(0)");
                return;
            }
            put_byte(e, 0xDE);
            put_byte(e, (uint8_t)(a->de_base + (d.num & 7)));
            return;
        }
        if (d.num == 0) {
            /* FOP ST(0), ST(i) -> D8 base+i */
            put_byte(e, 0xD8);
            put_byte(e, (uint8_t)(a->d8_base + (s.num & 7)));
            return;
        }
        if (s.num == 0) {
            /* FOP ST(i), ST(0) -> DC base+i */
            put_byte(e, 0xDC);
            put_byte(e, (uint8_t)(a->dc_base + (d.num & 7)));
            return;
        }
        asm_err_at(e, ln, "arith ST(i), ST(j) - one operand must be ST(0)");
        return;
    }
    asm_err_at(e, ln, "arith: bad operands");
}

/* Two-operand FPU compare instructions (FCOMI/FCOMIP/FUCOMI/FUCOMIP). The
 * left operand must be ST(0) and the right is ST(i). */
static void
x86_64_enc_x87_cmpi(AsmEnc *e, AsmLine *ln, uint8_t first, uint8_t base)
{
    if (ln->n_operands != 2 || !asm_is_reg(&ln->operands[0]) ||
            ln->operands[0].reg.cls != AR_X87 || !asm_is_reg(&ln->operands[1]) ||
            ln->operands[1].reg.cls != AR_X87) {
        asm_err_at(e, ln, "fcomi-family: needs ST(0), ST(i)");
        return;
    }
    if (ln->operands[0].reg.num != 0) {
        asm_err_at(e, ln, "fcomi-family: first operand must be ST(0)");
        return;
    }
    put_byte(e, first);
    put_byte(e, (uint8_t)(base + (ln->operands[1].reg.num & 7)));
}

/* Data directives are encoded via the shared `asm_enc_directive` in
 * asm_enc.c - the logic is target-agnostic, just raw bytes and fixups. */

/* ================================================================ SSE / SSE2
 *
 * Scalar single-precision (F3 prefix) and scalar double-precision (F2
 * prefix), plus the packed forms used as scalar idioms (xorps/andps).
 *
 * Encoding shape:
 *   [mandatory prefix] [REX] 0F <opcode> <ModRM> [SIB] [disp] [imm]
 *
 * Most ops have one opcode for both reg-reg and reg-mem (load direction);
 * MOVxx variants have a second opcode for the mem-reg store direction. */

typedef struct {
    int id;           /* X86_64Mn enum value */
    uint8_t prefix;   /* 0, or one of 0x66/0xF2/0xF3 */
    uint8_t op_load;  /* opcode for xmm <- xmm / xmm <- mem  (i.e. dst is reg) */
    uint8_t op_store; /* opcode for mem <- xmm  (used by mov*); 0 if illegal */
} SseOp;

static const SseOp x86_64_sse_table[] = {
    /* scalar arithmetic */
    { X86MN_ADDSS,   0xF3, 0x58, 0 }, { X86MN_ADDSD,   0xF2, 0x58, 0 },
    { X86MN_SUBSS,   0xF3, 0x5C, 0 }, { X86MN_SUBSD,   0xF2, 0x5C, 0 },
    { X86MN_MULSS,   0xF3, 0x59, 0 }, { X86MN_MULSD,   0xF2, 0x59, 0 },
    { X86MN_DIVSS,   0xF3, 0x5E, 0 }, { X86MN_DIVSD,   0xF2, 0x5E, 0 },
    { X86MN_SQRTSS,  0xF3, 0x51, 0 }, { X86MN_SQRTSD,  0xF2, 0x51, 0 },
    { X86MN_MINSS,   0xF3, 0x5D, 0 }, { X86MN_MINSD,   0xF2, 0x5D, 0 },
    { X86MN_MAXSS,   0xF3, 0x5F, 0 }, { X86MN_MAXSD,   0xF2, 0x5F, 0 },
    /* unordered/ordered scalar compare */
    { X86MN_UCOMISS, 0x00, 0x2E, 0 }, { X86MN_UCOMISD, 0x66, 0x2E, 0 },
    { X86MN_COMISS,  0x00, 0x2F, 0 }, { X86MN_COMISD,  0x66, 0x2F, 0 },
    /* bitwise (packed but commonly used scalar-style: xor xmm0,xmm0 zero) */
    { X86MN_XORPS,   0x00, 0x57, 0 }, { X86MN_XORPD,   0x66, 0x57, 0 },
    { X86MN_ANDPS,   0x00, 0x54, 0 }, { X86MN_ANDPD,   0x66, 0x54, 0 },
    { X86MN_ORPS,    0x00, 0x56, 0 }, { X86MN_ORPD,    0x66, 0x56, 0 },
    { X86MN_ANDNPS,  0x00, 0x55, 0 }, { X86MN_ANDNPD,  0x66, 0x55, 0 },
    /* xmm<->xmm cross-precision converts */
    { X86MN_CVTSS2SD, 0xF3, 0x5A, 0 }, { X86MN_CVTSD2SS, 0xF2, 0x5A, 0 },
    /* scalar moves: opcode 10/28 for load (incl. reg-reg), 11/29 for store */
    { X86MN_MOVSS,   0xF3, 0x10, 0x11 }, { X86MN_MOVSD,   0xF2, 0x10, 0x11 },
    { X86MN_MOVAPS,  0x00, 0x28, 0x29 }, { X86MN_MOVAPD,  0x66, 0x28, 0x29 },
    { X86MN_MOVUPS,  0x00, 0x10, 0x11 }, { X86MN_MOVUPD,  0x66, 0x10, 0x11 },
};

static const SseOp *
x86_64_sse_lookup(int id)
{
    for (size_t i = 0; i < sizeof x86_64_sse_table / sizeof x86_64_sse_table[0]; i++)
        if (x86_64_sse_table[i].id == id)
            return &x86_64_sse_table[i];
    return NULL;
}

/* Emit prefix -> REX -> 0F -> opcode -> ModRM for an xmm-xmm pair.
 * `is_64` adds REX.W (used by integer conversions). */
static void
x86_64_emit_sse_rr(AsmEnc *e, uint8_t prefix, uint8_t opcode, int is_64,
                   int dst_num, int src_num)
{
    if (prefix) put_byte(e, prefix);
    uint8_t rex = 0;
    if (is_64) rex |= REX_W;
    if (dst_num >= 8) rex |= REX_R;
    if (src_num >= 8) rex |= REX_B;
    if (rex) put_byte(e, rex);
    put_byte(e, 0x0F);
    put_byte(e, opcode);
    put_byte(e, modrm(3, dst_num & 7, src_num & 7));
}

/* Emit prefix -> REX -> 0F -> opcode -> memref. `reg_num` is the reg field of
 * the ModR/M (either dst for loads, or src for stores). */
static void
x86_64_emit_sse_rm(AsmEnc *e, uint8_t prefix, uint8_t opcode, int is_64,
                   int reg_num, AsmOperand *mem)
{
    if (prefix) put_byte(e, prefix);
    if (mem->has_seg) x86_64_emit_seg(e, mem->seg);
    x86_64_emit_rex_for(e, is_64, reg_num, mem, -1);
    put_byte(e, 0x0F);
    put_byte(e, opcode);
    x86_64_emit_memref(e, reg_num, mem);
}

static void
x86_64_enc_sse(AsmEnc *e, AsmLine *ln, const SseOp *op)
{
    if (ln->n_operands != 2) {
        asm_err_at(e, ln, "sse op needs 2 operands");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    AsmOperand *s = &ln->operands[1];

    /* xmm, xmm */
    if (asm_is_reg(d) && d->reg.cls == AR_XMM &&
            asm_is_reg(s) && s->reg.cls == AR_XMM) {
        x86_64_emit_sse_rr(e, op->prefix, op->op_load, 0, d->reg.num, s->reg.num);
        return;
    }
    /* xmm, [mem] - load form */
    if (asm_is_reg(d) && d->reg.cls == AR_XMM && asm_is_mem(s)) {
        x86_64_emit_sse_rm(e, op->prefix, op->op_load, 0, d->reg.num, s);
        return;
    }
    /* [mem], xmm - store form, only for mov* */
    if (asm_is_mem(d) && asm_is_reg(s) && s->reg.cls == AR_XMM) {
        if (op->op_store == 0) {
            asm_err_at(e, ln, "sse op: memory destination not supported");
            return;
        }
        x86_64_emit_sse_rm(e, op->prefix, op->op_store, 0, s->reg.num, d);
        return;
    }
    asm_err_at(e, ln, "sse op: unsupported operand combination");
}

/* Integer <-> floating-point conversions. These mix XMM and GPR operands
 * and use REX.W when the GPR is 64-bit.
 *
 *   cvtsi2ss/cvtsi2sd  xmm, r/m{32,64}   (dst XMM, src GPR/mem)
 *   cvtss2si/cvtsd2si  r{32,64}, xmm/m   (dst GPR, src XMM/mem)
 *   cvttss2si/cvttsd2si - same as above, truncating.
 *
 * For mem operands the size is implicit from the mnemonic (ss=32, sd=64). */
static void
x86_64_enc_cvtsi2(AsmEnc *e, AsmLine *ln, uint8_t prefix)
{
    if (ln->n_operands != 2 || !asm_is_reg(&ln->operands[0]) ||
            ln->operands[0].reg.cls != AR_XMM) {
        asm_err_at(e, ln, "cvtsi2: needs xmm, r/m");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    AsmOperand *s = &ln->operands[1];
    if (asm_is_reg(s)) {
        int w = x86_64_reg_width(s->reg);
        if (w != 4 && w != 8) {
            asm_err_at(e, ln, "cvtsi2: src must be r32 or r64");
            return;
        }
        x86_64_emit_sse_rr(e, prefix, 0x2A, w == 8, d->reg.num, s->reg.num);
        return;
    }
    if (asm_is_mem(s)) {
        int is_64 = (s->width == 8); /* TODO: u32/u64 size hints */
        x86_64_emit_sse_rm(e, prefix, 0x2A, is_64, d->reg.num, s);
        return;
    }
    asm_err_at(e, ln, "cvtsi2: bad src operand");
}

static void
x86_64_enc_cvt_to_si(AsmEnc *e, AsmLine *ln, uint8_t prefix, uint8_t opcode)
{
    if (ln->n_operands != 2 || !asm_is_reg(&ln->operands[0]) ||
            (ln->operands[0].reg.cls != AR_GPR32 &&
             ln->operands[0].reg.cls != AR_GPR64)) {
        asm_err_at(e, ln, "cvt*2si: needs r{32,64}, xmm/m");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    AsmOperand *s = &ln->operands[1];
    int w = x86_64_reg_width(d->reg);
    if (asm_is_reg(s) && s->reg.cls == AR_XMM) {
        x86_64_emit_sse_rr(e, prefix, opcode, w == 8, d->reg.num, s->reg.num);
        return;
    }
    if (asm_is_mem(s)) {
        x86_64_emit_sse_rm(e, prefix, opcode, w == 8, d->reg.num, s);
        return;
    }
    asm_err_at(e, ln, "cvt*2si: bad src operand");
}

/* Helper: emit JMP/Jcc to a label operand (local @@N or symbol), or to
 * a numeric rel32 displacement (`jmp -5`, `je -6`) as printed by the
 * disassembler. For Jcc, `cc` is the 4-bit condition code; for JMP,
 * pass -1. */
static void
x86_64_emit_branch_to_label(AsmEnc *e, AsmLine *ln, int cc)
{
    if (ln->n_operands == 1 && asm_is_imm(&ln->operands[0]) &&
            !ln->operands[0].is_float) {
        int64_t rel = ln->operands[0].imm;
        if (rel < INT32_MIN || rel > INT32_MAX) {
            asm_err_at(e, ln, "branch: rel32 displacement out of range");
            return;
        }
        if (cc < 0) {
            put_byte(e, 0xE9);
        } else {
            put_byte(e, 0x0F);
            put_byte(e, (uint8_t)(0x80 | (cc & 0xF)));
        }
        put_u32(e, (uint32_t)(int32_t)rel);
        return;
    }
    if (ln->n_operands != 1 || !asm_is_label(&ln->operands[0])) {
        asm_err_at(e, ln, cc < 0 ? "jmp: needs label or rel32"
                                 : "jcc: needs label or rel32");
        return;
    }
    int n = asm_operand_local_num(&ln->operands[0]);
    if (n >= 0) {
        if (cc < 0) x86_64_enc_jmp_local(e, n);
        else x86_64_enc_jcc_local(e, cc, n);
        return;
    }
    /* Symbol form: emit opcode + rel32 placeholder, register a fixup. */
    if (cc < 0) {
        put_byte(e, 0xE9);
    } else {
        put_byte(e, 0x0F);
        put_byte(e, (uint8_t)(0x80 | (cc & 0xF)));
    }
    AsmFixup f = { 0 };
    f.kind = AF_SYMBOL;
    f.reloc = cc < 0 ? AFR_X86_64_JMP32 : AFR_X86_64_JCC32;
    f.sym = xstrdup(ln->operands[0].label_name);
    f.patch_offset = e->len;
    f.width = 4;
    f.pcrel = 1;
    asm_add_fixup(e, f);
    put_u32(e, 0);
}

/* MOVABS r64, imm64 - explicit spelling for the 10-byte wide-immediate
 * form. Plain `mov` picks this encoding automatically for immediates
 * that don't fit imm32; `movabs` forces it regardless, so disassembler
 * output re-assembles byte-identically. */
static void
x86_64_enc_movabs(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 2 || !asm_is_reg(&ln->operands[0]) ||
            ln->operands[0].reg.cls != AR_GPR64 ||
            !asm_is_imm(&ln->operands[1]) || ln->operands[1].is_float) {
        asm_err_at(e, ln, "movabs: needs r64, imm");
        return;
    }
    x86_64_enc_movabsq_imm_reg(e, ln->operands[0].reg.num,
                               (uint64_t)ln->operands[1].imm);
}

static void
x86_64_enc_jmp(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands == 1 &&
            (asm_is_label(&ln->operands[0]) || asm_is_imm(&ln->operands[0]))) {
        x86_64_emit_branch_to_label(e, ln, -1);
        return;
    }
    if (ln->n_operands == 1 && asm_is_reg(&ln->operands[0]) &&
            ln->operands[0].reg.cls == AR_GPR64) {
        int r = ln->operands[0].reg.num;
        if (r >= 8) put_byte(e, 0x41);
        put_byte(e, 0xFF);
        put_byte(e, modrm(3, 4, r));
        return;
    }
    asm_err_at(e, ln, "jmp: unsupported operand");
}

/* x87 no-operand opcodes: byte0=0xD9 (or DB), byte1 unique per mnemonic. */
static void
x86_64_emit_x87_2byte(AsmEnc *e, uint8_t b0, uint8_t b1)
{
    put_byte(e, b0);
    put_byte(e, b1);
}

static void
x86_64_enc_fstsw(AsmEnc *e, AsmLine *ln)
{
    /* FSTSW AX / FNSTSW AX -> DF E0; only the AX form is emitted. */
    if (ln->n_operands == 0 ||
            (ln->n_operands == 1 && asm_is_reg(&ln->operands[0]) &&
                    ln->operands[0].reg.cls == AR_GPR16 &&
                    ln->operands[0].reg.num == 0)) {
        put_byte(e, 0xDF);
        put_byte(e, 0xE0);
        return;
    }
    asm_err_at(e, ln, "fstsw: only AX form supported");
}

static void
x86_64_enc_ffree(AsmEnc *e, AsmLine *ln)
{
    if (ln->n_operands != 1 || !asm_is_reg(&ln->operands[0]) ||
            ln->operands[0].reg.cls != AR_X87) {
        asm_err_at(e, ln, "ffree: needs ST(i)");
        return;
    }
    put_byte(e, 0xDD);
    put_byte(e, (uint8_t)(0xC0 + (ln->operands[0].reg.num & 7)));
}

static void
x86_64_enc_fucom_or_fucomp(AsmEnc *e, AsmLine *ln, uint8_t base)
{
    int i = ln->n_operands == 1 ? (ln->operands[0].reg.num & 7) : 1;
    put_byte(e, 0xDD);
    put_byte(e, (uint8_t)(base + i));
}

/* IMUL r, r/imm (2-operand forms). The 1-operand F7 /5 form routes
 * through x86_64_enc_unary like its DIV siblings. */
static void
x86_64_enc_imul2(AsmEnc *e, AsmLine *ln)
{
    AsmOperand *d = &ln->operands[0];
    AsmOperand *s = &ln->operands[1];
    if (!asm_is_reg(d)) {
        asm_err_at(e, ln, "imul: destination must be a register");
        return;
    }
    int w = x86_64_reg_width(d->reg);
    if (w == 2) put_byte(e, 0x66);
    if (asm_is_reg(s)) {
        uint8_t rex = 0;
        if (w == 8) rex |= REX_W;
        if (d->reg.num >= 8) rex |= REX_R;
        if (s->reg.num >= 8) rex |= REX_B;
        if (rex) put_byte(e, rex);
        put_byte(e, 0x0F);
        put_byte(e, 0xAF);
        put_byte(e, modrm(3, d->reg.num, s->reg.num));
        return;
    }
    if (asm_is_imm(s)) {
        /* IMUL r, r, imm with dst == src1: 6B /r ib or 69 /r id. */
        uint8_t rex = 0;
        if (w == 8) rex |= REX_W;
        if (d->reg.num >= 8) rex |= REX_R | REX_B;
        if (rex) put_byte(e, rex);
        if (s->imm >= -128 && s->imm <= 127) {
            put_byte(e, 0x6B);
            put_byte(e, modrm(3, d->reg.num, d->reg.num));
            put_byte(e, (uint8_t)(int8_t)s->imm);
        } else {
            put_byte(e, 0x69);
            put_byte(e, modrm(3, d->reg.num, d->reg.num));
            put_u32(e, (uint32_t)s->imm);
        }
        return;
    }
    asm_err_at(e, ln, "imul: unsupported operand form");
}

/* MOVZX / MOVSX: 0F B6/B7 (zero) or 0F BE/BF (sign), 1- or 2-byte
 * source widening into a 2/4/8-byte register. */
static void
x86_64_enc_movx(AsmEnc *e, AsmLine *ln, int sign)
{
    if (ln->n_operands != 2 || !asm_is_reg(&ln->operands[0])) {
        asm_err_at(e, ln, "movzx/movsx: needs a register destination");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    AsmOperand *s = &ln->operands[1];
    int dw = x86_64_reg_width(d->reg);
    int sw = asm_is_reg(s) ? x86_64_reg_width(s->reg) : s->width;
    if (sw != 1 && sw != 2) {
        asm_err_at(e, ln, "movzx/movsx: source must be 1 or 2 bytes "
                          "(size the operand or use a suffixed mnemonic)");
        return;
    }
    uint8_t op = (uint8_t)((sign ? 0xBE : 0xB6) + (sw == 2 ? 1 : 0));
    if (dw == 2) put_byte(e, 0x66);
    if (asm_is_reg(s)) {
        uint8_t rex = 0;
        if (dw == 8) rex |= REX_W;
        if (d->reg.num >= 8) rex |= REX_R;
        if (s->reg.num >= 8) rex |= REX_B;
        /* SPL/BPL/SIL/DIL need a bare REX to select the low byte. */
        if (sw == 1 && s->reg.cls != AR_AHHIGH &&
            s->reg.num >= 4 && s->reg.num < 8)
            rex |= 0x40;
        if (rex) put_byte(e, rex);
        put_byte(e, 0x0F);
        put_byte(e, op);
        put_byte(e, modrm(3, d->reg.num, s->reg.num));
        return;
    }
    if (asm_is_mem(s)) {
        if (s->has_seg) x86_64_emit_seg(e, s->seg);
        x86_64_emit_rex_for(e, dw == 8, d->reg.num, s, -1);
        put_byte(e, 0x0F);
        put_byte(e, op);
        x86_64_emit_memref(e, d->reg.num, s);
        return;
    }
    asm_err_at(e, ln, "movzx/movsx: unsupported source operand");
}

/* CMOVcc r, r/m: 0F 4x /r. Byte-width destinations don't exist. */
static void
x86_64_enc_cmov(AsmEnc *e, AsmLine *ln, int cc)
{
    if (ln->n_operands != 2 || !asm_is_reg(&ln->operands[0])) {
        asm_err_at(e, ln, "cmovcc: needs a register destination");
        return;
    }
    AsmOperand *d = &ln->operands[0];
    AsmOperand *s = &ln->operands[1];
    int w = x86_64_reg_width(d->reg);
    if (w == 1) {
        asm_err_at(e, ln, "cmovcc: no byte form exists");
        return;
    }
    if (w == 2) put_byte(e, 0x66);
    if (asm_is_reg(s)) {
        uint8_t rex = 0;
        if (w == 8) rex |= REX_W;
        if (d->reg.num >= 8) rex |= REX_R;
        if (s->reg.num >= 8) rex |= REX_B;
        if (rex) put_byte(e, rex);
        put_byte(e, 0x0F);
        put_byte(e, (uint8_t)(0x40 | (cc & 0xF)));
        put_byte(e, modrm(3, d->reg.num, s->reg.num));
        return;
    }
    if (asm_is_mem(s)) {
        if (s->has_seg) x86_64_emit_seg(e, s->seg);
        x86_64_emit_rex_for(e, w == 8, d->reg.num, s, -1);
        put_byte(e, 0x0F);
        put_byte(e, (uint8_t)(0x40 | (cc & 0xF)));
        x86_64_emit_memref(e, d->reg.num, s);
        return;
    }
    asm_err_at(e, ln, "cmovcc: unsupported source operand");
}

int
x86_64_suffixed_mnemonic(const char *m, int *width_out)
{
    *width_out = 0;
    size_t n = strlen(m);
    /* AT&T movz/movs with explicit src+dst widths: movzbq, movsbl, ... */
    if (n == 6 && (!strncmp(m, "movz", 4) || !strncmp(m, "movs", 4))) {
        int src = (m[4] == 'b') ? 1 : (m[4] == 'w') ? 2 : 0;
        int dst_ok = (m[5] == 'l' || m[5] == 'q');
        if (src && dst_ok) {
            *width_out = src;
            return m[3] == 'z' ? X86MN_MOVZX : X86MN_MOVSX;
        }
    }
    if (n < 3 || n - 1 >= 15) return TASM_MN_UNKNOWN;
    int w;
    switch (m[n - 1]) {
        case 'b': w = 1; break;
        case 'w': w = 2; break;
        case 'l': w = 4; break;
        case 'q': w = 8; break;
        default: return TASM_MN_UNKNOWN;
    }
    char base[16];
    memcpy(base, m, n - 1);
    base[n - 1] = '\0';
    int id = tasm_mnemonic_lookup(x86_64_mnemonic_table(), base);
    if (id != TASM_MN_UNKNOWN) *width_out = w;
    return id;
}

static void
x86_64_encode_instr(AsmEnc *e, AsmLine *ln)
{
    int mn = ln->mnemonic_id;

    /* REP/REPE/REPNZ prefix glued on at parse time. */
    if (ln->rep_prefix) put_byte(e, (uint8_t)ln->rep_prefix);

    /* CMOVcc ids are contiguous and cc-ordered. */
    if (mn >= X86MN_CMOVO && mn <= X86MN_CMOVG) {
        x86_64_enc_cmov(e, ln, mn - X86MN_CMOVO);
        return;
    }

    /* Jcc fast path: any canonical jcc mnemonic maps to a non-negative cc
     * value. Aliases (jz/je, jc/jb/jnae, ...) were collapsed to canonical
     * ids by the parser, so this single block covers all 30+ spellings. */
    int cc = x86_64_cc_for_mn(mn);
    if (cc >= 0) {
        x86_64_emit_branch_to_label(e, ln, cc);
        return;
    }

    switch ((X86_64Mn)mn) {
        /* Integer ALU / data movement / flow */
        case X86MN_PUSH:      x86_64_enc_push(e, ln); return;
        case X86MN_POP:       x86_64_enc_pop(e, ln); return;
        case X86MN_MOV:       x86_64_enc_mov(e, ln); return;
        case X86MN_MOVABS:    x86_64_enc_movabs(e, ln); return;
        case X86MN_ADD:       x86_64_enc_alu(e, ln, &x86_64_V_ADD); return;
        case X86MN_SUB:       x86_64_enc_alu(e, ln, &x86_64_V_SUB); return;
        case X86MN_AND:       x86_64_enc_alu(e, ln, &x86_64_V_AND); return;
        case X86MN_OR:        x86_64_enc_alu(e, ln, &x86_64_V_OR); return;
        case X86MN_XOR:       x86_64_enc_alu(e, ln, &x86_64_V_XOR); return;
        case X86MN_CMP:       x86_64_enc_alu(e, ln, &x86_64_V_CMP); return;
        case X86MN_ADC:       x86_64_enc_alu(e, ln, &x86_64_V_ADC); return;
        case X86MN_SBB:       x86_64_enc_alu(e, ln, &x86_64_V_SBB); return;
        case X86MN_TEST:      x86_64_enc_test(e, ln); return;
        case X86MN_SHL:       x86_64_enc_shift(e, ln, 4); return; /* sal alias collapses here */
        case X86MN_SHR:       x86_64_enc_shift(e, ln, 5); return;
        case X86MN_SAR:       x86_64_enc_shift(e, ln, 7); return;
        case X86MN_NEG:       x86_64_enc_unary(e, ln, 3); return;
        case X86MN_NOT:       x86_64_enc_unary(e, ln, 2); return;
        case X86MN_INC:       x86_64_enc_inc_dec(e, ln, 0); return;
        case X86MN_DEC:       x86_64_enc_inc_dec(e, ln, 1); return;
        case X86MN_CALL:      x86_64_enc_call(e, ln); return;
        case X86MN_LEA:       x86_64_enc_lea(e, ln); return;
        case X86MN_RET:       x86_64_enc_ret(e, ln, 0); return;
        case X86MN_RET1:      x86_64_enc_ret(e, ln, 1); return;
        case X86MN_RETN:
            /* `retn <imm16>` aliases `ret1`; `retn` alone falls through to
             * the error - same behaviour as the old strcmp dispatch. */
            if (ln->n_operands == 1) { x86_64_enc_ret(e, ln, 1); return; }
            break;
        case X86MN_REP_MOVSB: put_byte(e, 0xF3); put_byte(e, 0xA4); return;
        case X86MN_SYSCALL:   put_byte(e, 0x0F); put_byte(e, 0x05); return;
        case X86MN_LEAVE:     put_byte(e, 0xC9); return;
        case X86MN_NOP:       put_byte(e, 0x90); return;
        case X86MN_CQO:       put_byte(e, 0x48); put_byte(e, 0x99); return;
        case X86MN_IDIV:      x86_64_enc_unary(e, ln, 7); return;
        case X86MN_DIV:       x86_64_enc_unary(e, ln, 6); return;
        case X86MN_MUL:       x86_64_enc_unary(e, ln, 4); return;
        case X86MN_IMUL:
            if (ln->n_operands <= 1) x86_64_enc_unary(e, ln, 5);
            else                     x86_64_enc_imul2(e, ln);
            return;
        case X86MN_MOVZX:     x86_64_enc_movx(e, ln, 0); return;
        case X86MN_MOVSX:     x86_64_enc_movx(e, ln, 1); return;
        case X86MN_CLD:       put_byte(e, 0xFC); return;
        case X86MN_MOVSB:     put_byte(e, 0xA4); return;
        case X86MN_CMPSB:     put_byte(e, 0xA6); return;
        case X86MN_STOSB:     put_byte(e, 0xAA); return;
        case X86MN_LODSB:     put_byte(e, 0xAC); return;
        case X86MN_SCASB:     put_byte(e, 0xAE); return;
        case X86MN_JMP:       x86_64_enc_jmp(e, ln); return;
        case X86MN_RDTSC:     put_byte(e, 0x0F); put_byte(e, 0x31); return;

        /* x87 no-operand opcodes. D9 ix family + the few DB ix ones. */
        case X86MN_FLD1:      x86_64_emit_x87_2byte(e, 0xD9, 0xE8); return;
        case X86MN_FLDZ:      x86_64_emit_x87_2byte(e, 0xD9, 0xEE); return;
        case X86MN_FLDPI:     x86_64_emit_x87_2byte(e, 0xD9, 0xEB); return;
        case X86MN_FLDL2T:    x86_64_emit_x87_2byte(e, 0xD9, 0xE9); return;
        case X86MN_FLDL2E:    x86_64_emit_x87_2byte(e, 0xD9, 0xEA); return;
        case X86MN_FLDLG2:    x86_64_emit_x87_2byte(e, 0xD9, 0xEC); return;
        case X86MN_FLDLN2:    x86_64_emit_x87_2byte(e, 0xD9, 0xED); return;
        case X86MN_FCHS:      x86_64_emit_x87_2byte(e, 0xD9, 0xE0); return;
        case X86MN_FABS:      x86_64_emit_x87_2byte(e, 0xD9, 0xE1); return;
        case X86MN_FTST:      x86_64_emit_x87_2byte(e, 0xD9, 0xE4); return;
        case X86MN_FXAM:      x86_64_emit_x87_2byte(e, 0xD9, 0xE5); return;
        case X86MN_F2XM1:     x86_64_emit_x87_2byte(e, 0xD9, 0xF0); return;
        case X86MN_FYL2X:     x86_64_emit_x87_2byte(e, 0xD9, 0xF1); return;
        case X86MN_FPTAN:     x86_64_emit_x87_2byte(e, 0xD9, 0xF2); return;
        case X86MN_FPATAN:    x86_64_emit_x87_2byte(e, 0xD9, 0xF3); return;
        case X86MN_FXTRACT:   x86_64_emit_x87_2byte(e, 0xD9, 0xF4); return;
        case X86MN_FPREM1:    x86_64_emit_x87_2byte(e, 0xD9, 0xF5); return;
        case X86MN_FDECSTP:   x86_64_emit_x87_2byte(e, 0xD9, 0xF6); return;
        case X86MN_FINCSTP:   x86_64_emit_x87_2byte(e, 0xD9, 0xF7); return;
        case X86MN_FPREM:     x86_64_emit_x87_2byte(e, 0xD9, 0xF8); return;
        case X86MN_FYL2XP1:   x86_64_emit_x87_2byte(e, 0xD9, 0xF9); return;
        case X86MN_FSQRT:     x86_64_emit_x87_2byte(e, 0xD9, 0xFA); return;
        case X86MN_FSINCOS:   x86_64_emit_x87_2byte(e, 0xD9, 0xFB); return;
        case X86MN_FRNDINT:   x86_64_emit_x87_2byte(e, 0xD9, 0xFC); return;
        case X86MN_FSCALE:    x86_64_emit_x87_2byte(e, 0xD9, 0xFD); return;
        case X86MN_FSIN:      x86_64_emit_x87_2byte(e, 0xD9, 0xFE); return;
        case X86MN_FCOS:      x86_64_emit_x87_2byte(e, 0xD9, 0xFF); return;
        case X86MN_FWAIT:     put_byte(e, 0x9B); return;
        case X86MN_FNINIT:    x86_64_emit_x87_2byte(e, 0xDB, 0xE3); return;
        case X86MN_FNCLEX:    x86_64_emit_x87_2byte(e, 0xDB, 0xE2); return;
        case X86MN_FSTSW:     x86_64_enc_fstsw(e, ln); return;

        /* x87 memory or ST(i) family */
        case X86MN_FLD:  x86_64_enc_x87_mem_or_reg(e, ln, 0xD9, 0xDD, 0, 0xD9, 0xC0); return;
        case X86MN_FST:  x86_64_enc_x87_mem_or_reg(e, ln, 0xD9, 0xDD, 2, 0xDD, 0xD0); return;
        case X86MN_FSTP: x86_64_enc_x87_mem_or_reg(e, ln, 0xD9, 0xDD, 3, 0xDD, 0xD8); return;

        case X86MN_FFREE:  x86_64_enc_ffree(e, ln); return;
        case X86MN_FUCOM:  x86_64_enc_fucom_or_fucomp(e, ln, 0xE0); return;
        case X86MN_FUCOMP: x86_64_enc_fucom_or_fucomp(e, ln, 0xE8); return;

        /* x87 arithmetic */
        case X86MN_FADD:   x86_64_enc_x87_arith(e, ln, &x86_64_FA_ADD,  0); return;
        case X86MN_FADDP:  x86_64_enc_x87_arith(e, ln, &x86_64_FA_ADD,  1); return;
        case X86MN_FSUB:   x86_64_enc_x87_arith(e, ln, &x86_64_FA_SUB,  0); return;
        case X86MN_FSUBP:  x86_64_enc_x87_arith(e, ln, &x86_64_FA_SUB,  1); return;
        case X86MN_FSUBR:  x86_64_enc_x87_arith(e, ln, &x86_64_FA_SUBR, 0); return;
        case X86MN_FSUBRP: x86_64_enc_x87_arith(e, ln, &x86_64_FA_SUBR, 1); return;
        case X86MN_FMUL:   x86_64_enc_x87_arith(e, ln, &x86_64_FA_MUL,  0); return;
        case X86MN_FMULP:  x86_64_enc_x87_arith(e, ln, &x86_64_FA_MUL,  1); return;
        case X86MN_FDIV:   x86_64_enc_x87_arith(e, ln, &x86_64_FA_DIV,  0); return;
        case X86MN_FDIVP:  x86_64_enc_x87_arith(e, ln, &x86_64_FA_DIV,  1); return;
        case X86MN_FDIVR:  x86_64_enc_x87_arith(e, ln, &x86_64_FA_DIVR, 0); return;
        case X86MN_FDIVRP: x86_64_enc_x87_arith(e, ln, &x86_64_FA_DIVR, 1); return;

        /* x87 compare-and-set-flags */
        case X86MN_FCOMI:   x86_64_enc_x87_cmpi(e, ln, 0xDB, 0xF0); return;
        case X86MN_FCOMIP:  x86_64_enc_x87_cmpi(e, ln, 0xDF, 0xF0); return;
        case X86MN_FUCOMI:  x86_64_enc_x87_cmpi(e, ln, 0xDB, 0xE8); return;
        case X86MN_FUCOMIP: x86_64_enc_x87_cmpi(e, ln, 0xDF, 0xE8); return;

        /* SSE / SSE2 mnemonics encoded by the shared SseOp helper. The
         * (prefix, opcode_load, opcode_store) for each id lives in
         * x86_64_sse_table. */
        case X86MN_ADDSS: case X86MN_ADDSD:
        case X86MN_SUBSS: case X86MN_SUBSD:
        case X86MN_MULSS: case X86MN_MULSD:
        case X86MN_DIVSS: case X86MN_DIVSD:
        case X86MN_SQRTSS: case X86MN_SQRTSD:
        case X86MN_MINSS: case X86MN_MINSD:
        case X86MN_MAXSS: case X86MN_MAXSD:
        case X86MN_UCOMISS: case X86MN_UCOMISD:
        case X86MN_COMISS:  case X86MN_COMISD:
        case X86MN_XORPS:   case X86MN_XORPD:
        case X86MN_ANDPS:   case X86MN_ANDPD:
        case X86MN_ORPS:    case X86MN_ORPD:
        case X86MN_ANDNPS:  case X86MN_ANDNPD:
        case X86MN_CVTSS2SD: case X86MN_CVTSD2SS:
        case X86MN_MOVSS: case X86MN_MOVSD:
        case X86MN_MOVAPS: case X86MN_MOVAPD:
        case X86MN_MOVUPS: case X86MN_MOVUPD: {
            const SseOp *op = x86_64_sse_lookup(mn);
            if (op) x86_64_enc_sse(e, ln, op);
            else asm_err_at(e, ln, "internal: SSE op missing table entry");
            return;
        }

        /* SSE int<->fp conversions need REX.W-aware dispatchers. */
        case X86MN_CVTSI2SS:  x86_64_enc_cvtsi2(e, ln, 0xF3); return;
        case X86MN_CVTSI2SD:  x86_64_enc_cvtsi2(e, ln, 0xF2); return;
        case X86MN_CVTSS2SI:  x86_64_enc_cvt_to_si(e, ln, 0xF3, 0x2D); return;
        case X86MN_CVTSD2SI:  x86_64_enc_cvt_to_si(e, ln, 0xF2, 0x2D); return;
        case X86MN_CVTTSS2SI: x86_64_enc_cvt_to_si(e, ln, 0xF3, 0x2C); return;
        case X86MN_CVTTSD2SI: x86_64_enc_cvt_to_si(e, ln, 0xF2, 0x2C); return;

        /* The 16 canonical jcc enum values are handled by the
         * x86_64_cc_for_mn fast path above. List them here so
         * -Wswitch-enum knows they're covered. */
        case X86MN_JO: case X86MN_JNO: case X86MN_JB:  case X86MN_JAE:
        case X86MN_JE: case X86MN_JNE: case X86MN_JBE: case X86MN_JA:
        case X86MN_JS: case X86MN_JNS: case X86MN_JP:  case X86MN_JNP:
        case X86MN_JL: case X86MN_JGE: case X86MN_JLE: case X86MN_JG:
            /* unreachable: handled above */
            break;

        case X86MN_UNKNOWN: break;
        default:
            asm_err_at(e, ln, "unsupported mnemonic");
            break;
            
    }
}

/* x86_64 backend. Called from the public asm_encode below when the block's
 * target is x86. */
int
asm_encode_x86_64(AsmBlock *blk, AsmResolver *r, AsmEnc *out)
{
    out->src_file = blk ? blk->src_file : NULL;
    if (!blk) return 0;

    g_resolver = r;

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
            case AINS_INSTR: x86_64_encode_instr(out, ln); break;
            case AINS_DIRECTIVE: asm_enc_directive(out, ln); break;
        default: break;
        }
    }
    out->line_offsets[blk->n_lines] = out->len;

    /* Resolve LOCAL-label fixups now that all labels are placed. Anything
     * that remains (AF_SYMBOL / AF_CLASS_MEMBER) is left for the codegen
     * layer to turn into Mach-O relocations. */
    int resolved = 0;
    AsmFixup *kept = xmalloc(sizeof(AsmFixup) * (size_t)out->n_fixups);
    for (int i = 0; i < out->n_fixups; i++) {
        AsmFixup *f = &out->fixups[i];
        if (f->kind != AF_LOCAL) {
            kept[resolved++] = *f;
            continue;
        }
        /* Find the label. */
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
            char *err = asm_tmp_printf("%s: x86_64 asm: unresolved local label @@%d\n",
                    out->src_file ? out->src_file : "?", f->local_num);
            asm_err_at(out, NULL, err);
            continue;
        }
        if (f->pcrel) {
            int64_t disp = (int64_t)target_off -
                    ((int64_t)f->patch_offset + (int64_t)f->width);
            if (f->width == 4) {
                int32_t d = (int32_t)disp;
                out->bytes[f->patch_offset + 0] = (uint8_t)d;
                out->bytes[f->patch_offset + 1] = (uint8_t)(d >> 8);
                out->bytes[f->patch_offset + 2] = (uint8_t)(d >> 16);
                out->bytes[f->patch_offset + 3] = (uint8_t)(d >> 24);
            }
        } else {
            /* Absolute address inside this block - codegen will rebase. We
             * write the byte offset; codegen adds the block's load address. */
            for (int b = 0; b < f->width; b++)
                out->bytes[f->patch_offset + b] = (uint8_t)(target_off >>
                        (8 * b));
            /* Keep it as a sym-style fixup so codegen relocates to __text base.
             */
            kept[resolved] = *f;
            kept[resolved].kind = AF_SYMBOL;
            char buf[32];
            snprintf(buf, sizeof buf, "__asm_local_%d", f->local_num);
            kept[resolved].sym = xstrdup(buf);
            resolved++;
        }
    }
    /* Free the old fixup array and replace with the kept (non-local) ones. */
    free(out->fixups);
    out->fixups = kept;
    out->n_fixups = resolved;

    g_resolver = NULL;
    return out->errors;
}

/* ---------------- mnemonic table ----------------
 * Sorted alphabetically by `name` so target.c's binary search works.
 * Aliases (jz=je, sal=shl, cqto=cqo, wait=fwait, fnstsw=fstsw, plus the
 * jcc synonym set) appear as additional entries pointing to the canonical
 * id. Update both this table and the X86_64Mn enum together. */
static const TasmMnemonicEntry x86_64_mnemonic_entries[] = {
    { "adc",        X86MN_ADC },
    { "add",        X86MN_ADD },
    { "addsd",      X86MN_ADDSD },
    { "addss",      X86MN_ADDSS },
    { "and",        X86MN_AND },
    { "andnpd",     X86MN_ANDNPD },
    { "andnps",     X86MN_ANDNPS },
    { "andpd",      X86MN_ANDPD },
    { "andps",      X86MN_ANDPS },
    { "call",       X86MN_CALL },
    { "cld",      X86MN_CLD },
    { "cmova",    X86MN_CMOVA },
    { "cmovae",   X86MN_CMOVAE },
    { "cmovb",    X86MN_CMOVB },
    { "cmovbe",   X86MN_CMOVBE },
    { "cmovc",    X86MN_CMOVB },
    { "cmove",    X86MN_CMOVE },
    { "cmovg",    X86MN_CMOVG },
    { "cmovge",   X86MN_CMOVGE },
    { "cmovl",    X86MN_CMOVL },
    { "cmovle",   X86MN_CMOVLE },
    { "cmovna",   X86MN_CMOVBE },
    { "cmovnae",  X86MN_CMOVB },
    { "cmovnb",   X86MN_CMOVAE },
    { "cmovnbe",  X86MN_CMOVA },
    { "cmovnc",   X86MN_CMOVAE },
    { "cmovne",   X86MN_CMOVNE },
    { "cmovng",   X86MN_CMOVLE },
    { "cmovnge",  X86MN_CMOVL },
    { "cmovnl",   X86MN_CMOVGE },
    { "cmovnle",  X86MN_CMOVG },
    { "cmovno",   X86MN_CMOVNO },
    { "cmovnp",   X86MN_CMOVNP },
    { "cmovns",   X86MN_CMOVNS },
    { "cmovnz",   X86MN_CMOVNE },
    { "cmovo",    X86MN_CMOVO },
    { "cmovp",    X86MN_CMOVP },
    { "cmovs",    X86MN_CMOVS },
    { "cmovz",    X86MN_CMOVE },
    { "cmp",        X86MN_CMP },
    { "cmpsb",    X86MN_CMPSB },
    { "comisd",     X86MN_COMISD },
    { "comiss",     X86MN_COMISS },
    { "cqo",        X86MN_CQO },
    { "cqto",       X86MN_CQO },
    { "cvtsd2si",   X86MN_CVTSD2SI },
    { "cvtsd2ss",   X86MN_CVTSD2SS },
    { "cvtsi2sd",   X86MN_CVTSI2SD },
    { "cvtsi2ss",   X86MN_CVTSI2SS },
    { "cvtss2sd",   X86MN_CVTSS2SD },
    { "cvtss2si",   X86MN_CVTSS2SI },
    { "cvttsd2si",  X86MN_CVTTSD2SI },
    { "cvttss2si",  X86MN_CVTTSS2SI },
    { "dec",        X86MN_DEC },
    { "div",      X86MN_DIV },
    { "divsd",      X86MN_DIVSD },
    { "divss",      X86MN_DIVSS },
    { "f2xm1",      X86MN_F2XM1 },
    { "fabs",       X86MN_FABS },
    { "fadd",       X86MN_FADD },
    { "faddp",      X86MN_FADDP },
    { "fchs",       X86MN_FCHS },
    { "fcomi",      X86MN_FCOMI },
    { "fcomip",     X86MN_FCOMIP },
    { "fcos",       X86MN_FCOS },
    { "fdecstp",    X86MN_FDECSTP },
    { "fdiv",       X86MN_FDIV },
    { "fdivp",      X86MN_FDIVP },
    { "fdivr",      X86MN_FDIVR },
    { "fdivrp",     X86MN_FDIVRP },
    { "ffree",      X86MN_FFREE },
    { "fincstp",    X86MN_FINCSTP },
    { "fld",        X86MN_FLD },
    { "fld1",       X86MN_FLD1 },
    { "fldl2e",     X86MN_FLDL2E },
    { "fldl2t",     X86MN_FLDL2T },
    { "fldlg2",     X86MN_FLDLG2 },
    { "fldln2",     X86MN_FLDLN2 },
    { "fldpi",      X86MN_FLDPI },
    { "fldz",       X86MN_FLDZ },
    { "fmul",       X86MN_FMUL },
    { "fmulp",      X86MN_FMULP },
    { "fnclex",     X86MN_FNCLEX },
    { "fninit",     X86MN_FNINIT },
    { "fnstsw",     X86MN_FSTSW },
    { "fpatan",     X86MN_FPATAN },
    { "fprem",      X86MN_FPREM },
    { "fprem1",     X86MN_FPREM1 },
    { "fptan",      X86MN_FPTAN },
    { "frndint",    X86MN_FRNDINT },
    { "fscale",     X86MN_FSCALE },
    { "fsin",       X86MN_FSIN },
    { "fsincos",    X86MN_FSINCOS },
    { "fsqrt",      X86MN_FSQRT },
    { "fst",        X86MN_FST },
    { "fstp",       X86MN_FSTP },
    { "fstsw",      X86MN_FSTSW },
    { "fsub",       X86MN_FSUB },
    { "fsubp",      X86MN_FSUBP },
    { "fsubr",      X86MN_FSUBR },
    { "fsubrp",     X86MN_FSUBRP },
    { "ftst",       X86MN_FTST },
    { "fucom",      X86MN_FUCOM },
    { "fucomi",     X86MN_FUCOMI },
    { "fucomip",    X86MN_FUCOMIP },
    { "fucomp",     X86MN_FUCOMP },
    { "fwait",      X86MN_FWAIT },
    { "fxam",       X86MN_FXAM },
    { "fxtract",    X86MN_FXTRACT },
    { "fyl2x",      X86MN_FYL2X },
    { "fyl2xp1",    X86MN_FYL2XP1 },
    { "idiv",     X86MN_IDIV },
    { "imul",     X86MN_IMUL },
    { "inc",        X86MN_INC },
    { "ja",         X86MN_JA },
    { "jae",        X86MN_JAE },
    { "jb",         X86MN_JB },
    { "jbe",        X86MN_JBE },
    { "jc",         X86MN_JB },
    { "je",         X86MN_JE },
    { "jg",         X86MN_JG },
    { "jge",        X86MN_JGE },
    { "jl",         X86MN_JL },
    { "jle",        X86MN_JLE },
    { "jmp",        X86MN_JMP },
    { "jna",        X86MN_JBE },
    { "jnae",       X86MN_JB },
    { "jnb",        X86MN_JAE },
    { "jnbe",       X86MN_JA },
    { "jnc",        X86MN_JAE },
    { "jne",        X86MN_JNE },
    { "jng",        X86MN_JLE },
    { "jnge",       X86MN_JL },
    { "jnl",        X86MN_JGE },
    { "jnle",       X86MN_JG },
    { "jno",        X86MN_JNO },
    { "jnp",        X86MN_JNP },
    { "jns",        X86MN_JNS },
    { "jnz",        X86MN_JNE },
    { "jo",         X86MN_JO },
    { "jp",         X86MN_JP },
    { "jpe",        X86MN_JP },
    { "jpo",        X86MN_JNP },
    { "js",         X86MN_JS },
    { "jz",         X86MN_JE },
    { "lea",        X86MN_LEA },
    { "leave",      X86MN_LEAVE },
    { "lodsb",    X86MN_LODSB },
    { "maxsd",      X86MN_MAXSD },
    { "maxss",      X86MN_MAXSS },
    { "minsd",      X86MN_MINSD },
    { "minss",      X86MN_MINSS },
    { "mov",        X86MN_MOV },
    { "movabs",     X86MN_MOVABS },
    { "movapd",     X86MN_MOVAPD },
    { "movaps",     X86MN_MOVAPS },
    { "movsb",    X86MN_MOVSB },
    { "movsd",      X86MN_MOVSD },
    { "movss",      X86MN_MOVSS },
    { "movsx",    X86MN_MOVSX },
    { "movupd",     X86MN_MOVUPD },
    { "movups",     X86MN_MOVUPS },
    { "movzx",    X86MN_MOVZX },
    { "mul",      X86MN_MUL },
    { "mulsd",      X86MN_MULSD },
    { "mulss",      X86MN_MULSS },
    { "neg",        X86MN_NEG },
    { "nop",        X86MN_NOP },
    { "not",        X86MN_NOT },
    { "or",         X86MN_OR },
    { "orpd",       X86MN_ORPD },
    { "orps",       X86MN_ORPS },
    { "pop",        X86MN_POP },
    { "push",       X86MN_PUSH },
    { "rdtsc",      X86MN_RDTSC },
    { "rep_movsb",  X86MN_REP_MOVSB },
    { "ret",        X86MN_RET },
    { "ret1",       X86MN_RET1 },
    { "retn",       X86MN_RETN },
    { "sal",        X86MN_SHL },
    { "sar",        X86MN_SAR },
    { "sbb",        X86MN_SBB },
    { "scasb",    X86MN_SCASB },
    { "shl",        X86MN_SHL },
    { "shr",        X86MN_SHR },
    { "sqrtsd",     X86MN_SQRTSD },
    { "sqrtss",     X86MN_SQRTSS },
    { "stosb",    X86MN_STOSB },
    { "sub",        X86MN_SUB },
    { "subsd",      X86MN_SUBSD },
    { "subss",      X86MN_SUBSS },
    { "syscall",    X86MN_SYSCALL },
    { "test",       X86MN_TEST },
    { "ucomisd",    X86MN_UCOMISD },
    { "ucomiss",    X86MN_UCOMISS },
    { "wait",       X86MN_FWAIT },
    { "xor",        X86MN_XOR },
    { "xorpd",      X86MN_XORPD },
    { "xorps",      X86MN_XORPS },
};

static const TasmMnemonicTable x86_64_mnemonic_table_v = {
    x86_64_mnemonic_entries,
    (int)(sizeof x86_64_mnemonic_entries / sizeof x86_64_mnemonic_entries[0]),
};

const TasmMnemonicTable *
x86_64_mnemonic_table(void)
{
    return &x86_64_mnemonic_table_v;
}
