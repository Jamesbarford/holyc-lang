#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <sys/types.h>
#include <unistd.h>

#include "asm_enc.h"
#include "test-helper.h"

static void
test_x86_64_dump_failure(TestCtx *T, const uint8_t *expected, size_t expected_len)
{
    printf("  Expected (%zu bytes):\n   ", expected_len);
    for (size_t i = 0; i < expected_len; ++i)
        printf(" %02x", expected[i]);
    printf("\n");

    printf("  Got (%zu bytes):\n   ", (size_t)T->enc.len);
    for (size_t i = 0; i < (size_t)T->enc.len; ++i)
        printf(" %02x", (unsigned)T->enc.bytes[i]);
    printf("\n");
}

/* Compare T->enc.bytes against an array literal of expected bytes.
 * Returns 1 on exact match (length and contents), 0 otherwise.
 * On mismatch, copies expected bytes to T->expected for the failure dump. */
static int
test_x86_64_check(TestCtx *T, const uint8_t *expected, size_t expected_len)
{
    T->expected_len = (int)expected_len;
    if (expected_len > sizeof T->expected) expected_len = sizeof T->expected;
    memcpy(T->expected, expected, expected_len);

    if ((size_t)T->enc.len != expected_len) return 0;
    return memcmp(T->enc.bytes, expected, expected_len) == 0;
}

#define EXPECT_BYTES(T, ...) \
    test_x86_64_check((T), (const uint8_t[]){__VA_ARGS__}, \
                      sizeof((uint8_t[]){__VA_ARGS__}))

/* ============================================================ MOV */

int
x86_64_enc_mov_r64_imm32_test(TestCtx *T)
{
    test_asm_enc(T, "mov rax, 1");
    return EXPECT_BYTES(T, 0x48, 0xc7, 0xc0, 0x01, 0x00, 0x00, 0x00);
}

int
x86_64_enc_mov_r64_r64_test(TestCtx *T)
{
    test_asm_enc(T, "mov rbx, rax");
    return EXPECT_BYTES(T, 0x48, 0x89, 0xc3);
}

int
x86_64_enc_mov_r32_imm_test(TestCtx *T)
{
    test_asm_enc(T, "mov eax, 1");
    return EXPECT_BYTES(T, 0xb8, 0x01, 0x00, 0x00, 0x00);
}

int
x86_64_enc_mov_r64_imm64_test(TestCtx *T)
{
    /* Imm > INT32_MAX; uses the 10-byte MOV r64, imm64 (REX.W + B8+r). */
    test_asm_enc(T, "mov rax, 0x123456789a");
    return EXPECT_BYTES(T, 0x48, 0xb8, 0x9a, 0x78, 0x56, 0x34, 0x12,
                           0x00, 0x00, 0x00);
}

/* NOTE: known failure - the 8-bit `mov r8, imm` path emits an
 * extraneous 0x00 prefix byte. Bug is in enc_x86_64.c around the
 * REX-or-zero ternary; the W=1 imm-form should produce `B0 imm8`. */
int
x86_64_enc_mov_r8_imm_test(TestCtx *T)
{
    test_asm_enc(T, "mov al, 0x12");
    return EXPECT_BYTES(T, 0xb0, 0x12);
}

/* NOTE: known failure - `mov r16, imm16` is not currently dispatched in
 * x86_64_enc_mov (only w==8/4/1 immediate paths exist). */
int
x86_64_enc_mov_r16_imm_test(TestCtx *T)
{
    test_asm_enc(T, "mov ax, 0x1234");
    return EXPECT_BYTES(T, 0x66, 0xb8, 0x34, 0x12);
}

int
x86_64_enc_mov_r8_r64_test(TestCtx *T)
{
    test_asm_enc(T, "mov r8, rax");
    return EXPECT_BYTES(T, 0x49, 0x89, 0xc0);
}

int
x86_64_enc_mov_r64_r8_test(TestCtx *T)
{
    test_asm_enc(T, "mov rax, r8");
    return EXPECT_BYTES(T, 0x4c, 0x89, 0xc0);
}

int
x86_64_enc_mov_r8_r8_high_test(TestCtx *T)
{
    test_asm_enc(T, "mov r9, r10");
    return EXPECT_BYTES(T, 0x4d, 0x89, 0xd1);
}

int
x86_64_enc_mov_r32_r32_test(TestCtx *T)
{
    test_asm_enc(T, "mov eax, ebx");
    return EXPECT_BYTES(T, 0x89, 0xd8);
}

int
x86_64_enc_mov_r8_lo_r8_lo_test(TestCtx *T)
{
    test_asm_enc(T, "mov al, bl");
    return EXPECT_BYTES(T, 0x88, 0xd8);
}

int
x86_64_enc_mov_r16_r16_test(TestCtx *T)
{
    test_asm_enc(T, "mov ax, bx");
    return EXPECT_BYTES(T, 0x66, 0x89, 0xd8);
}

/* ============================================================ MOV (memory) */

int
x86_64_enc_mov_r64_mem_test(TestCtx *T)
{
    test_asm_enc(T, "mov rax, [rbx]");
    return EXPECT_BYTES(T, 0x48, 0x8b, 0x03);
}

int
x86_64_enc_mov_r64_mem_disp8_test(TestCtx *T)
{
    test_asm_enc(T, "mov rax, 8[rbx]");
    return EXPECT_BYTES(T, 0x48, 0x8b, 0x43, 0x08);
}

int
x86_64_enc_mov_r64_mem_disp32_test(TestCtx *T)
{
    test_asm_enc(T, "mov rax, 256[rbx]");
    return EXPECT_BYTES(T, 0x48, 0x8b, 0x83, 0x00, 0x01, 0x00, 0x00);
}

int
x86_64_enc_mov_mem_r64_test(TestCtx *T)
{
    test_asm_enc(T, "mov [rbx], rax");
    return EXPECT_BYTES(T, 0x48, 0x89, 0x03);
}

int
x86_64_enc_mov_mem_disp_r64_test(TestCtx *T)
{
    test_asm_enc(T, "mov 8[rbx], rax");
    return EXPECT_BYTES(T, 0x48, 0x89, 0x43, 0x08);
}

/* ============================================================ ALU r-imm / r-mem */

int
x86_64_enc_add_r32_r32_test(TestCtx *T)
{
    test_asm_enc(T, "add eax, ebx");
    return EXPECT_BYTES(T, 0x01, 0xd8);
}

int
x86_64_enc_add_imm8_test(TestCtx *T)
{
    test_asm_enc(T, "add rax, 1");
    return EXPECT_BYTES(T, 0x48, 0x83, 0xc0, 0x01);
}

/* `as` (clang) would prefer the 6-byte `ADD RAX, imm32` accumulator form
 * (48 05 ...). Our encoder uses the general 7-byte 0x81 /0 form. Both are
 * valid; the test pins what we emit. */
int
x86_64_enc_add_imm32_test(TestCtx *T)
{
    test_asm_enc(T, "add rax, 1000");
    return EXPECT_BYTES(T, 0x48, 0x81, 0xc0, 0xe8, 0x03, 0x00, 0x00);
}

int
x86_64_enc_add_r_mem_test(TestCtx *T)
{
    test_asm_enc(T, "add rax, [rbx]");
    return EXPECT_BYTES(T, 0x48, 0x03, 0x03);
}

int
x86_64_enc_add_mem_r_test(TestCtx *T)
{
    test_asm_enc(T, "add [rbx], rax");
    return EXPECT_BYTES(T, 0x48, 0x01, 0x03);
}

int
x86_64_enc_add_r8_r9_test(TestCtx *T)
{
    test_asm_enc(T, "add r8, r9");
    return EXPECT_BYTES(T, 0x4d, 0x01, 0xc8);
}

int
x86_64_enc_sub_imm_test(TestCtx *T)
{
    test_asm_enc(T, "sub rax, 4");
    return EXPECT_BYTES(T, 0x48, 0x83, 0xe8, 0x04);
}

int
x86_64_enc_and_r32_imm_test(TestCtx *T)
{
    test_asm_enc(T, "and eax, 0x10");
    return EXPECT_BYTES(T, 0x83, 0xe0, 0x10);
}

int
x86_64_enc_xor_eax_eax_test(TestCtx *T)
{
    test_asm_enc(T, "xor eax, eax");
    return EXPECT_BYTES(T, 0x31, 0xc0);
}

int
x86_64_enc_adc_r64_r64_test(TestCtx *T)
{
    test_asm_enc(T, "adc rax, rbx");
    return EXPECT_BYTES(T, 0x48, 0x11, 0xd8);
}

int
x86_64_enc_sbb_r64_r64_test(TestCtx *T)
{
    test_asm_enc(T, "sbb rax, rbx");
    return EXPECT_BYTES(T, 0x48, 0x19, 0xd8);
}

/* ============================================================ TEST */

int
x86_64_enc_test_r32_r32_test(TestCtx *T)
{
    test_asm_enc(T, "test eax, eax");
    return EXPECT_BYTES(T, 0x85, 0xc0);
}

/* clang prefers `A9 imm32` (accumulator form). Our encoder uses the
 * general `F7 /0 imm32` form. */
int
x86_64_enc_test_r64_imm_test(TestCtx *T)
{
    test_asm_enc(T, "test rax, 16");
    return EXPECT_BYTES(T, 0x48, 0xf7, 0xc0, 0x10, 0x00, 0x00, 0x00);
}

/* ============================================================ SHIFTS */

int
x86_64_enc_shl_r64_1_test(TestCtx *T)
{
    test_asm_enc(T, "shl rax, 1");
    return EXPECT_BYTES(T, 0x48, 0xd1, 0xe0);
}

int
x86_64_enc_shl_r64_imm_test(TestCtx *T)
{
    test_asm_enc(T, "shl rax, 4");
    return EXPECT_BYTES(T, 0x48, 0xc1, 0xe0, 0x04);
}

int
x86_64_enc_shl_r32_imm_test(TestCtx *T)
{
    test_asm_enc(T, "shl eax, 3");
    return EXPECT_BYTES(T, 0xc1, 0xe0, 0x03);
}

int
x86_64_enc_shl_r64_cl_test(TestCtx *T)
{
    test_asm_enc(T, "shl rax, cl");
    return EXPECT_BYTES(T, 0x48, 0xd3, 0xe0);
}

int
x86_64_enc_shr_r64_imm_test(TestCtx *T)
{
    test_asm_enc(T, "shr rax, 4");
    return EXPECT_BYTES(T, 0x48, 0xc1, 0xe8, 0x04);
}

int
x86_64_enc_sar_r64_imm_test(TestCtx *T)
{
    test_asm_enc(T, "sar rax, 4");
    return EXPECT_BYTES(T, 0x48, 0xc1, 0xf8, 0x04);
}

/* ============================================================ UNARY (32) / INC / DEC */

int
x86_64_enc_neg_r32_test(TestCtx *T)
{
    test_asm_enc(T, "neg eax");
    return EXPECT_BYTES(T, 0xf7, 0xd8);
}

int
x86_64_enc_not_r32_test(TestCtx *T)
{
    test_asm_enc(T, "not eax");
    return EXPECT_BYTES(T, 0xf7, 0xd0);
}

int
x86_64_enc_inc_r32_test(TestCtx *T)
{
    test_asm_enc(T, "inc ecx");
    return EXPECT_BYTES(T, 0xff, 0xc1);
}

int
x86_64_enc_dec_r32_test(TestCtx *T)
{
    test_asm_enc(T, "dec ecx");
    return EXPECT_BYTES(T, 0xff, 0xc9);
}

/* ============================================================ PUSH / POP */

int
x86_64_enc_push_rbp_test(TestCtx *T)
{
    test_asm_enc(T, "push rbp");
    return EXPECT_BYTES(T, 0x55);
}

int
x86_64_enc_push_r12_test(TestCtx *T)
{
    test_asm_enc(T, "push r12");
    return EXPECT_BYTES(T, 0x41, 0x54);
}

int
x86_64_enc_pop_rbp_test(TestCtx *T)
{
    test_asm_enc(T, "pop rbp");
    return EXPECT_BYTES(T, 0x5d);
}

int
x86_64_enc_pop_r13_test(TestCtx *T)
{
    test_asm_enc(T, "pop r13");
    return EXPECT_BYTES(T, 0x41, 0x5d);
}

/* ============================================================ CALL / RET */

int
x86_64_enc_call_r64_test(TestCtx *T)
{
    test_asm_enc(T, "call rax");
    return EXPECT_BYTES(T, 0xff, 0xd0);
}

/* TempleOS syntax: `ret1 imm16` for the imm16-popping ret (opcode C2).
 * Plain `ret` => C3. */
int
x86_64_enc_ret1_imm_test(TestCtx *T)
{
    test_asm_enc(T, "ret1 16");
    return EXPECT_BYTES(T, 0xc2, 0x10, 0x00);
}

/* ============================================================ JMP / Jcc */

int
x86_64_enc_jmp_r64_test(TestCtx *T)
{
    test_asm_enc(T, "jmp rax");
    return EXPECT_BYTES(T, 0xff, 0xe0);
}

/* @@1: <instr> @@1 - local-label backwards branch with target = current
 * encoder offset. Our jmp/jcc always use the rel32 form (5/6 bytes),
 * never the 2-byte short form clang chooses for small displacements. */
int
x86_64_enc_jmp_label_test(TestCtx *T)
{
    test_asm_enc(T, "@@1: jmp @@1");
    return EXPECT_BYTES(T, 0xe9, 0xfb, 0xff, 0xff, 0xff);
}

int
x86_64_enc_je_label_test(TestCtx *T)
{
    test_asm_enc(T, "@@1: je @@1");
    return EXPECT_BYTES(T, 0x0f, 0x84, 0xfa, 0xff, 0xff, 0xff);
}

int
x86_64_enc_jne_label_test(TestCtx *T)
{
    test_asm_enc(T, "@@1: jne @@1");
    return EXPECT_BYTES(T, 0x0f, 0x85, 0xfa, 0xff, 0xff, 0xff);
}

int
x86_64_enc_jl_label_test(TestCtx *T)
{
    test_asm_enc(T, "@@1: jl @@1");
    return EXPECT_BYTES(T, 0x0f, 0x8c, 0xfa, 0xff, 0xff, 0xff);
}

int
x86_64_enc_jge_label_test(TestCtx *T)
{
    test_asm_enc(T, "@@1: jge @@1");
    return EXPECT_BYTES(T, 0x0f, 0x8d, 0xfa, 0xff, 0xff, 0xff);
}

int
x86_64_enc_jle_label_test(TestCtx *T)
{
    test_asm_enc(T, "@@1: jle @@1");
    return EXPECT_BYTES(T, 0x0f, 0x8e, 0xfa, 0xff, 0xff, 0xff);
}

int
x86_64_enc_jg_label_test(TestCtx *T)
{
    test_asm_enc(T, "@@1: jg @@1");
    return EXPECT_BYTES(T, 0x0f, 0x8f, 0xfa, 0xff, 0xff, 0xff);
}

int
x86_64_enc_ja_label_test(TestCtx *T)
{
    test_asm_enc(T, "@@1: ja @@1");
    return EXPECT_BYTES(T, 0x0f, 0x87, 0xfa, 0xff, 0xff, 0xff);
}

int
x86_64_enc_jae_label_test(TestCtx *T)
{
    test_asm_enc(T, "@@1: jae @@1");
    return EXPECT_BYTES(T, 0x0f, 0x83, 0xfa, 0xff, 0xff, 0xff);
}

int
x86_64_enc_jb_label_test(TestCtx *T)
{
    test_asm_enc(T, "@@1: jb @@1");
    return EXPECT_BYTES(T, 0x0f, 0x82, 0xfa, 0xff, 0xff, 0xff);
}

int
x86_64_enc_jbe_label_test(TestCtx *T)
{
    test_asm_enc(T, "@@1: jbe @@1");
    return EXPECT_BYTES(T, 0x0f, 0x86, 0xfa, 0xff, 0xff, 0xff);
}

int
x86_64_enc_jz_alias_test(TestCtx *T)
{
    /* jz is an alias of je (cc=0x4). */
    test_asm_enc(T, "@@1: jz @@1");
    return EXPECT_BYTES(T, 0x0f, 0x84, 0xfa, 0xff, 0xff, 0xff);
}

int
x86_64_enc_jnz_alias_test(TestCtx *T)
{
    test_asm_enc(T, "@@1: jnz @@1");
    return EXPECT_BYTES(T, 0x0f, 0x85, 0xfa, 0xff, 0xff, 0xff);
}

/* ============================================================ Misc / Misc */

int
x86_64_enc_rep_movsb_test(TestCtx *T)
{
    test_asm_enc(T, "rep_movsb");
    return EXPECT_BYTES(T, 0xf3, 0xa4);
}

int
x86_64_enc_cqo_test(TestCtx *T)
{
    test_asm_enc(T, "cqo");
    return EXPECT_BYTES(T, 0x48, 0x99);
}

int
x86_64_enc_cqto_alias_test(TestCtx *T)
{
    test_asm_enc(T, "cqto");
    return EXPECT_BYTES(T, 0x48, 0x99);
}

/* ============================================================ Disassembler-output forms
 *
 * Spellings the disassembler prints that the assembler must take back:
 * `movabs`, `[base + disp]` / `[base - disp]`, numeric rel32 branches. */

int
x86_64_enc_movabs_test(TestCtx *T)
{
    test_asm_enc(T, "movabs rax, 0x123456789a");
    return EXPECT_BYTES(T, 0x48, 0xb8, 0x9a, 0x78, 0x56, 0x34, 0x12,
                        0x00, 0x00, 0x00);
}

int
x86_64_enc_mem_plus_disp_test(TestCtx *T)
{
    test_asm_enc(T, "mov rax, [rbx + 8]");
    return EXPECT_BYTES(T, 0x48, 0x8b, 0x43, 0x08);
}

int
x86_64_enc_mem_minus_disp_test(TestCtx *T)
{
    test_asm_enc(T, "mov rax, [rbp - 8]");
    return EXPECT_BYTES(T, 0x48, 0x8b, 0x45, 0xf8);
}

int
x86_64_enc_jmp_numeric_test(TestCtx *T)
{
    test_asm_enc(T, "jmp -5");
    return EXPECT_BYTES(T, 0xe9, 0xfb, 0xff, 0xff, 0xff);
}

int
x86_64_enc_je_numeric_test(TestCtx *T)
{
    test_asm_enc(T, "je -6");
    return EXPECT_BYTES(T, 0x0f, 0x84, 0xfa, 0xff, 0xff, 0xff);
}

/* ============================================================ Data directives */

int
x86_64_enc_db_test(TestCtx *T)
{
    test_asm_enc(T, "db 0x12, 0x34");
    return EXPECT_BYTES(T, 0x12, 0x34);
}

int
x86_64_enc_dw_test(TestCtx *T)
{
    test_asm_enc(T, "dw 0x1234");
    return EXPECT_BYTES(T, 0x34, 0x12);
}

int
x86_64_enc_dd_test(TestCtx *T)
{
    test_asm_enc(T, "dd 0x12345678");
    return EXPECT_BYTES(T, 0x78, 0x56, 0x34, 0x12);
}

int
x86_64_enc_dq_test(TestCtx *T)
{
    test_asm_enc(T, "dq 0x123456789a");
    return EXPECT_BYTES(T, 0x9a, 0x78, 0x56, 0x34, 0x12, 0x00, 0x00, 0x00);
}

/* ============================================================ SSE / SSE2 scalar arithmetic */

int
x86_64_enc_addss_rr_test(TestCtx *T)
{
    test_asm_enc(T, "addss xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x58, 0xc1);
}

int
x86_64_enc_addsd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "addsd xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x58, 0xc1);
}

int
x86_64_enc_subss_rr_test(TestCtx *T)
{
    test_asm_enc(T, "subss xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x5c, 0xc1);
}

int
x86_64_enc_subsd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "subsd xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x5c, 0xc1);
}

int
x86_64_enc_mulss_rr_test(TestCtx *T)
{
    test_asm_enc(T, "mulss xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x59, 0xc1);
}

int
x86_64_enc_mulsd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "mulsd xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x59, 0xc1);
}

int
x86_64_enc_divss_rr_test(TestCtx *T)
{
    test_asm_enc(T, "divss xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x5e, 0xc1);
}

int
x86_64_enc_divsd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "divsd xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x5e, 0xc1);
}

int
x86_64_enc_sqrtss_rr_test(TestCtx *T)
{
    test_asm_enc(T, "sqrtss xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x51, 0xc1);
}

int
x86_64_enc_sqrtsd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "sqrtsd xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x51, 0xc1);
}

int
x86_64_enc_minss_rr_test(TestCtx *T)
{
    test_asm_enc(T, "minss xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x5d, 0xc1);
}

int
x86_64_enc_minsd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "minsd xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x5d, 0xc1);
}

int
x86_64_enc_maxss_rr_test(TestCtx *T)
{
    test_asm_enc(T, "maxss xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x5f, 0xc1);
}

int
x86_64_enc_maxsd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "maxsd xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x5f, 0xc1);
}

/* ============================================================ SSE compare */

int
x86_64_enc_ucomiss_rr_test(TestCtx *T)
{
    test_asm_enc(T, "ucomiss xmm0, xmm1");
    return EXPECT_BYTES(T, 0x0f, 0x2e, 0xc1);
}

int
x86_64_enc_ucomisd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "ucomisd xmm0, xmm1");
    return EXPECT_BYTES(T, 0x66, 0x0f, 0x2e, 0xc1);
}

int
x86_64_enc_comiss_rr_test(TestCtx *T)
{
    test_asm_enc(T, "comiss xmm0, xmm1");
    return EXPECT_BYTES(T, 0x0f, 0x2f, 0xc1);
}

int
x86_64_enc_comisd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "comisd xmm0, xmm1");
    return EXPECT_BYTES(T, 0x66, 0x0f, 0x2f, 0xc1);
}

/* ============================================================ SSE bitwise */

int
x86_64_enc_xorps_rr_test(TestCtx *T)
{
    test_asm_enc(T, "xorps xmm0, xmm1");
    return EXPECT_BYTES(T, 0x0f, 0x57, 0xc1);
}

int
x86_64_enc_xorpd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "xorpd xmm0, xmm1");
    return EXPECT_BYTES(T, 0x66, 0x0f, 0x57, 0xc1);
}

int
x86_64_enc_andps_rr_test(TestCtx *T)
{
    test_asm_enc(T, "andps xmm0, xmm1");
    return EXPECT_BYTES(T, 0x0f, 0x54, 0xc1);
}

int
x86_64_enc_andpd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "andpd xmm0, xmm1");
    return EXPECT_BYTES(T, 0x66, 0x0f, 0x54, 0xc1);
}

int
x86_64_enc_orps_rr_test(TestCtx *T)
{
    test_asm_enc(T, "orps xmm0, xmm1");
    return EXPECT_BYTES(T, 0x0f, 0x56, 0xc1);
}

int
x86_64_enc_orpd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "orpd xmm0, xmm1");
    return EXPECT_BYTES(T, 0x66, 0x0f, 0x56, 0xc1);
}

int
x86_64_enc_andnps_rr_test(TestCtx *T)
{
    test_asm_enc(T, "andnps xmm0, xmm1");
    return EXPECT_BYTES(T, 0x0f, 0x55, 0xc1);
}

int
x86_64_enc_andnpd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "andnpd xmm0, xmm1");
    return EXPECT_BYTES(T, 0x66, 0x0f, 0x55, 0xc1);
}

/* xor xmm, xmm is the canonical "zero this register" idiom for floats. */
int
x86_64_enc_xorps_zero_idiom_test(TestCtx *T)
{
    test_asm_enc(T, "xorps xmm0, xmm0");
    return EXPECT_BYTES(T, 0x0f, 0x57, 0xc0);
}

/* ============================================================ SSE moves */

int
x86_64_enc_movss_rr_test(TestCtx *T)
{
    test_asm_enc(T, "movss xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x10, 0xc1);
}

int
x86_64_enc_movsd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "movsd xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x10, 0xc1);
}

int
x86_64_enc_movaps_rr_test(TestCtx *T)
{
    test_asm_enc(T, "movaps xmm0, xmm1");
    return EXPECT_BYTES(T, 0x0f, 0x28, 0xc1);
}

int
x86_64_enc_movapd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "movapd xmm0, xmm1");
    return EXPECT_BYTES(T, 0x66, 0x0f, 0x28, 0xc1);
}

int
x86_64_enc_movups_rr_test(TestCtx *T)
{
    test_asm_enc(T, "movups xmm0, xmm1");
    return EXPECT_BYTES(T, 0x0f, 0x10, 0xc1);
}

int
x86_64_enc_movupd_rr_test(TestCtx *T)
{
    test_asm_enc(T, "movupd xmm0, xmm1");
    return EXPECT_BYTES(T, 0x66, 0x0f, 0x10, 0xc1);
}

int
x86_64_enc_movss_load_test(TestCtx *T)
{
    test_asm_enc(T, "movss xmm0, [rbx]");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x10, 0x03);
}

int
x86_64_enc_movss_store_test(TestCtx *T)
{
    test_asm_enc(T, "movss [rbx], xmm0");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x11, 0x03);
}

int
x86_64_enc_movsd_store_test(TestCtx *T)
{
    test_asm_enc(T, "movsd [rbx], xmm0");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x11, 0x03);
}

int
x86_64_enc_movaps_store_test(TestCtx *T)
{
    test_asm_enc(T, "movaps [rbx], xmm0");
    return EXPECT_BYTES(T, 0x0f, 0x29, 0x03);
}

int
x86_64_enc_addss_mem_test(TestCtx *T)
{
    test_asm_enc(T, "addss xmm0, [rbx]");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x58, 0x03);
}

int
x86_64_enc_addsd_mem_disp_test(TestCtx *T)
{
    test_asm_enc(T, "addsd xmm0, 8[rbx]");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x58, 0x43, 0x08);
}

/* ============================================================ SSE REX-extended xmm */

int
x86_64_enc_addss_xmm8_xmm9_test(TestCtx *T)
{
    test_asm_enc(T, "addss xmm8, xmm9");
    return EXPECT_BYTES(T, 0xf3, 0x45, 0x0f, 0x58, 0xc1);
}

int
x86_64_enc_movss_xmm8_xmm0_test(TestCtx *T)
{
    test_asm_enc(T, "movss xmm8, xmm0");
    return EXPECT_BYTES(T, 0xf3, 0x44, 0x0f, 0x10, 0xc0);
}

int
x86_64_enc_movsd_xmm0_xmm15_test(TestCtx *T)
{
    test_asm_enc(T, "movsd xmm0, xmm15");
    return EXPECT_BYTES(T, 0xf2, 0x41, 0x0f, 0x10, 0xc7);
}

/* ============================================================ SSE conversions */

int
x86_64_enc_cvtss2sd_test(TestCtx *T)
{
    test_asm_enc(T, "cvtss2sd xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x5a, 0xc1);
}

int
x86_64_enc_cvtsd2ss_test(TestCtx *T)
{
    test_asm_enc(T, "cvtsd2ss xmm0, xmm1");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x5a, 0xc1);
}

int
x86_64_enc_cvtsi2ss_r32_test(TestCtx *T)
{
    test_asm_enc(T, "cvtsi2ss xmm0, eax");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x2a, 0xc0);
}

int
x86_64_enc_cvtsi2ss_r64_test(TestCtx *T)
{
    test_asm_enc(T, "cvtsi2ss xmm0, rax");
    return EXPECT_BYTES(T, 0xf3, 0x48, 0x0f, 0x2a, 0xc0);
}

int
x86_64_enc_cvtsi2sd_r32_test(TestCtx *T)
{
    test_asm_enc(T, "cvtsi2sd xmm0, eax");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x2a, 0xc0);
}

int
x86_64_enc_cvtsi2sd_r64_test(TestCtx *T)
{
    test_asm_enc(T, "cvtsi2sd xmm0, rax");
    return EXPECT_BYTES(T, 0xf2, 0x48, 0x0f, 0x2a, 0xc0);
}

int
x86_64_enc_cvtss2si_r32_test(TestCtx *T)
{
    test_asm_enc(T, "cvtss2si eax, xmm0");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x2d, 0xc0);
}

int
x86_64_enc_cvtss2si_r64_test(TestCtx *T)
{
    test_asm_enc(T, "cvtss2si rax, xmm0");
    return EXPECT_BYTES(T, 0xf3, 0x48, 0x0f, 0x2d, 0xc0);
}

int
x86_64_enc_cvtsd2si_r32_test(TestCtx *T)
{
    test_asm_enc(T, "cvtsd2si eax, xmm0");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x2d, 0xc0);
}

int
x86_64_enc_cvtsd2si_r64_test(TestCtx *T)
{
    test_asm_enc(T, "cvtsd2si rax, xmm0");
    return EXPECT_BYTES(T, 0xf2, 0x48, 0x0f, 0x2d, 0xc0);
}

int
x86_64_enc_cvttss2si_r32_test(TestCtx *T)
{
    test_asm_enc(T, "cvttss2si eax, xmm0");
    return EXPECT_BYTES(T, 0xf3, 0x0f, 0x2c, 0xc0);
}

int
x86_64_enc_cvttss2si_r64_test(TestCtx *T)
{
    test_asm_enc(T, "cvttss2si rax, xmm0");
    return EXPECT_BYTES(T, 0xf3, 0x48, 0x0f, 0x2c, 0xc0);
}

int
x86_64_enc_cvttsd2si_r32_test(TestCtx *T)
{
    test_asm_enc(T, "cvttsd2si eax, xmm0");
    return EXPECT_BYTES(T, 0xf2, 0x0f, 0x2c, 0xc0);
}

int
x86_64_enc_cvttsd2si_r64_test(TestCtx *T)
{
    test_asm_enc(T, "cvttsd2si rax, xmm0");
    return EXPECT_BYTES(T, 0xf2, 0x48, 0x0f, 0x2c, 0xc0);
}

/* ============================================================ ALU r,r */

int
x86_64_enc_add_r64_r64_test(TestCtx *T)
{
    test_asm_enc(T, "add rax, rbx");
    return EXPECT_BYTES(T, 0x48, 0x01, 0xd8);
}

int
x86_64_enc_sub_r64_r64_test(TestCtx *T)
{
    test_asm_enc(T, "sub rax, rbx");
    return EXPECT_BYTES(T, 0x48, 0x29, 0xd8);
}

int
x86_64_enc_and_r64_r64_test(TestCtx *T)
{
    test_asm_enc(T, "and rax, rbx");
    return EXPECT_BYTES(T, 0x48, 0x21, 0xd8);
}

int
x86_64_enc_or_r64_r64_test(TestCtx *T)
{
    test_asm_enc(T, "or rax, rbx");
    return EXPECT_BYTES(T, 0x48, 0x09, 0xd8);
}

int
x86_64_enc_xor_r64_r64_test(TestCtx *T)
{
    test_asm_enc(T, "xor rax, rbx");
    return EXPECT_BYTES(T, 0x48, 0x31, 0xd8);
}

int
x86_64_enc_cmp_r64_r64_test(TestCtx *T)
{
    test_asm_enc(T, "cmp rax, rbx");
    return EXPECT_BYTES(T, 0x48, 0x39, 0xd8);
}

/* ============================================================ unary */

int
x86_64_enc_inc_r64_test(TestCtx *T)
{
    test_asm_enc(T, "inc rax");
    return EXPECT_BYTES(T, 0x48, 0xff, 0xc0);
}

int
x86_64_enc_dec_r64_test(TestCtx *T)
{
    test_asm_enc(T, "dec rax");
    return EXPECT_BYTES(T, 0x48, 0xff, 0xc8);
}

int
x86_64_enc_neg_r64_test(TestCtx *T)
{
    test_asm_enc(T, "neg rax");
    return EXPECT_BYTES(T, 0x48, 0xf7, 0xd8);
}

int
x86_64_enc_not_r64_test(TestCtx *T)
{
    test_asm_enc(T, "not rax");
    return EXPECT_BYTES(T, 0x48, 0xf7, 0xd0);
}

/* ============================================================ stack */

int
x86_64_enc_push_r64_test(TestCtx *T)
{
    test_asm_enc(T, "push rax");
    return EXPECT_BYTES(T, 0x50);
}

int
x86_64_enc_pop_r64_test(TestCtx *T)
{
    test_asm_enc(T, "pop rbx");
    return EXPECT_BYTES(T, 0x5b);
}

/* ============================================================ control */

int
x86_64_enc_ret_test(TestCtx *T)
{
    test_asm_enc(T, "ret");
    return EXPECT_BYTES(T, 0xc3);
}

int
x86_64_enc_nop_test(TestCtx *T)
{
    test_asm_enc(T, "nop");
    return EXPECT_BYTES(T, 0x90);
}

int
x86_64_enc_syscall_test(TestCtx *T)
{
    test_asm_enc(T, "syscall");
    return EXPECT_BYTES(T, 0x0f, 0x05);
}

int
x86_64_enc_leave_test(TestCtx *T)
{
    test_asm_enc(T, "leave");
    return EXPECT_BYTES(T, 0xc9);
}

int
main(void)
{
    TestCtx T;
    test_ctx_init(&T, TASM_ARCH_X86_64);

    EncoderTest tests[] = {
        PrepTest(x86_64_enc_mov_r64_imm32_test),
        PrepTest(x86_64_enc_mov_r64_r64_test),
        PrepTest(x86_64_enc_mov_r32_imm_test),
        PrepTest(x86_64_enc_mov_r64_imm64_test),
        PrepTest(x86_64_enc_movabs_test),
        PrepTest(x86_64_enc_mem_plus_disp_test),
        PrepTest(x86_64_enc_mem_minus_disp_test),
        PrepTest(x86_64_enc_jmp_numeric_test),
        PrepTest(x86_64_enc_je_numeric_test),
        PrepTest(x86_64_enc_mov_r8_imm_test),
        PrepTest(x86_64_enc_mov_r16_imm_test),
        PrepTest(x86_64_enc_mov_r8_r64_test),
        PrepTest(x86_64_enc_mov_r64_r8_test),
        PrepTest(x86_64_enc_mov_r8_r8_high_test),
        PrepTest(x86_64_enc_mov_r32_r32_test),
        PrepTest(x86_64_enc_mov_r8_lo_r8_lo_test),
        PrepTest(x86_64_enc_mov_r16_r16_test),

        PrepTest(x86_64_enc_mov_r64_mem_test),
        PrepTest(x86_64_enc_mov_r64_mem_disp8_test),
        PrepTest(x86_64_enc_mov_r64_mem_disp32_test),
        PrepTest(x86_64_enc_mov_mem_r64_test),
        PrepTest(x86_64_enc_mov_mem_disp_r64_test),

        PrepTest(x86_64_enc_add_r64_r64_test),
        PrepTest(x86_64_enc_sub_r64_r64_test),
        PrepTest(x86_64_enc_and_r64_r64_test),
        PrepTest(x86_64_enc_or_r64_r64_test),
        PrepTest(x86_64_enc_xor_r64_r64_test),
        PrepTest(x86_64_enc_cmp_r64_r64_test),
        PrepTest(x86_64_enc_add_r32_r32_test),
        PrepTest(x86_64_enc_add_imm8_test),
        PrepTest(x86_64_enc_add_imm32_test),
        PrepTest(x86_64_enc_add_r_mem_test),
        PrepTest(x86_64_enc_add_mem_r_test),
        PrepTest(x86_64_enc_add_r8_r9_test),
        PrepTest(x86_64_enc_sub_imm_test),
        PrepTest(x86_64_enc_and_r32_imm_test),
        PrepTest(x86_64_enc_xor_eax_eax_test),
        PrepTest(x86_64_enc_adc_r64_r64_test),
        PrepTest(x86_64_enc_sbb_r64_r64_test),

        PrepTest(x86_64_enc_test_r32_r32_test),
        PrepTest(x86_64_enc_test_r64_imm_test),

        PrepTest(x86_64_enc_shl_r64_1_test),
        PrepTest(x86_64_enc_shl_r64_imm_test),
        PrepTest(x86_64_enc_shl_r32_imm_test),
        PrepTest(x86_64_enc_shl_r64_cl_test),
        PrepTest(x86_64_enc_shr_r64_imm_test),
        PrepTest(x86_64_enc_sar_r64_imm_test),

        PrepTest(x86_64_enc_inc_r64_test),
        PrepTest(x86_64_enc_dec_r64_test),
        PrepTest(x86_64_enc_neg_r64_test),
        PrepTest(x86_64_enc_not_r64_test),
        PrepTest(x86_64_enc_neg_r32_test),
        PrepTest(x86_64_enc_not_r32_test),
        PrepTest(x86_64_enc_inc_r32_test),
        PrepTest(x86_64_enc_dec_r32_test),

        PrepTest(x86_64_enc_push_r64_test),
        PrepTest(x86_64_enc_pop_r64_test),
        PrepTest(x86_64_enc_push_rbp_test),
        PrepTest(x86_64_enc_push_r12_test),
        PrepTest(x86_64_enc_pop_rbp_test),
        PrepTest(x86_64_enc_pop_r13_test),

        PrepTest(x86_64_enc_call_r64_test),
        PrepTest(x86_64_enc_ret1_imm_test),

        PrepTest(x86_64_enc_jmp_r64_test),
        PrepTest(x86_64_enc_jmp_label_test),
        PrepTest(x86_64_enc_je_label_test),
        PrepTest(x86_64_enc_jne_label_test),
        PrepTest(x86_64_enc_jl_label_test),
        PrepTest(x86_64_enc_jge_label_test),
        PrepTest(x86_64_enc_jle_label_test),
        PrepTest(x86_64_enc_jg_label_test),
        PrepTest(x86_64_enc_ja_label_test),
        PrepTest(x86_64_enc_jae_label_test),
        PrepTest(x86_64_enc_jb_label_test),
        PrepTest(x86_64_enc_jbe_label_test),
        PrepTest(x86_64_enc_jz_alias_test),
        PrepTest(x86_64_enc_jnz_alias_test),

        PrepTest(x86_64_enc_ret_test),
        PrepTest(x86_64_enc_nop_test),
        PrepTest(x86_64_enc_syscall_test),
        PrepTest(x86_64_enc_leave_test),
        PrepTest(x86_64_enc_rep_movsb_test),
        PrepTest(x86_64_enc_cqo_test),
        PrepTest(x86_64_enc_cqto_alias_test),

        PrepTest(x86_64_enc_db_test),
        PrepTest(x86_64_enc_dw_test),
        PrepTest(x86_64_enc_dd_test),
        PrepTest(x86_64_enc_dq_test),

        PrepTest(x86_64_enc_addss_rr_test),
        PrepTest(x86_64_enc_addsd_rr_test),
        PrepTest(x86_64_enc_subss_rr_test),
        PrepTest(x86_64_enc_subsd_rr_test),
        PrepTest(x86_64_enc_mulss_rr_test),
        PrepTest(x86_64_enc_mulsd_rr_test),
        PrepTest(x86_64_enc_divss_rr_test),
        PrepTest(x86_64_enc_divsd_rr_test),
        PrepTest(x86_64_enc_sqrtss_rr_test),
        PrepTest(x86_64_enc_sqrtsd_rr_test),
        PrepTest(x86_64_enc_minss_rr_test),
        PrepTest(x86_64_enc_minsd_rr_test),
        PrepTest(x86_64_enc_maxss_rr_test),
        PrepTest(x86_64_enc_maxsd_rr_test),

        PrepTest(x86_64_enc_ucomiss_rr_test),
        PrepTest(x86_64_enc_ucomisd_rr_test),
        PrepTest(x86_64_enc_comiss_rr_test),
        PrepTest(x86_64_enc_comisd_rr_test),

        PrepTest(x86_64_enc_xorps_rr_test),
        PrepTest(x86_64_enc_xorpd_rr_test),
        PrepTest(x86_64_enc_andps_rr_test),
        PrepTest(x86_64_enc_andpd_rr_test),
        PrepTest(x86_64_enc_orps_rr_test),
        PrepTest(x86_64_enc_orpd_rr_test),
        PrepTest(x86_64_enc_andnps_rr_test),
        PrepTest(x86_64_enc_andnpd_rr_test),
        PrepTest(x86_64_enc_xorps_zero_idiom_test),

        PrepTest(x86_64_enc_movss_rr_test),
        PrepTest(x86_64_enc_movsd_rr_test),
        PrepTest(x86_64_enc_movaps_rr_test),
        PrepTest(x86_64_enc_movapd_rr_test),
        PrepTest(x86_64_enc_movups_rr_test),
        PrepTest(x86_64_enc_movupd_rr_test),
        PrepTest(x86_64_enc_movss_load_test),
        PrepTest(x86_64_enc_movss_store_test),
        PrepTest(x86_64_enc_movsd_store_test),
        PrepTest(x86_64_enc_movaps_store_test),
        PrepTest(x86_64_enc_addss_mem_test),
        PrepTest(x86_64_enc_addsd_mem_disp_test),

        PrepTest(x86_64_enc_addss_xmm8_xmm9_test),
        PrepTest(x86_64_enc_movss_xmm8_xmm0_test),
        PrepTest(x86_64_enc_movsd_xmm0_xmm15_test),

        PrepTest(x86_64_enc_cvtss2sd_test),
        PrepTest(x86_64_enc_cvtsd2ss_test),
        PrepTest(x86_64_enc_cvtsi2ss_r32_test),
        PrepTest(x86_64_enc_cvtsi2ss_r64_test),
        PrepTest(x86_64_enc_cvtsi2sd_r32_test),
        PrepTest(x86_64_enc_cvtsi2sd_r64_test),
        PrepTest(x86_64_enc_cvtss2si_r32_test),
        PrepTest(x86_64_enc_cvtss2si_r64_test),
        PrepTest(x86_64_enc_cvtsd2si_r32_test),
        PrepTest(x86_64_enc_cvtsd2si_r64_test),
        PrepTest(x86_64_enc_cvttss2si_r32_test),
        PrepTest(x86_64_enc_cvttss2si_r64_test),
        PrepTest(x86_64_enc_cvttsd2si_r32_test),
        PrepTest(x86_64_enc_cvttsd2si_r64_test),
    };
    asm_enc_init(&T.enc);

    int n = (int)(sizeof(tests) / sizeof(tests[0]));
    int failed = 0;
    for (int i = 0; i < n; i++) {
        EncoderTest *test = &tests[i];
        test->pass = test->fn(&T);
        if (test->pass) {
            test_pass("%s\n", test->name);
        } else {
            test_fail("%s\n", test->name);
            test_x86_64_dump_failure(&T, (uint8_t *)T.expected, (size_t)T.expected_len);
            failed++;
        }
        test_asm_enc_free(&T);
    }

    printf("\n%d/%d passed\n", n - failed, n);
    return failed ? 1 : 0;
}
