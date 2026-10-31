/* Roundtrip tests for the x86_64 disassembler: encode known patterns
 * (mostly hand-built byte sequences mirroring what our IR lowering
 * emits) and check that the decoder returns the expected mnemonic. */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "asm_enc.h"
#include "dis_x86_64.h"
#include "test-helper.h"

typedef int (*TestFn)(void);
typedef struct { const char *name; TestFn fn; } DisTest;
#undef PrepTest /* test-helper.h's 3-field form; ours is 2-field */
#define PrepTest(n) { #n, n }

static int
check(const uint8_t *bytes, size_t len, const char *want)
{
    char buf[128];
    int n = x86_64_disasm(bytes, len, buf, sizeof buf);
    if (n <= 0) {
        fprintf(stderr, "  dis returned 0 for %zu bytes; got '%s' (want '%s')\n",
                len, buf, want);
        return 0;
    }
    if (strcmp(buf, want) != 0) {
        fprintf(stderr, "  got '%s', want '%s' (consumed %d)\n",
                buf, want, n);
        return 0;
    }
    if ((size_t)n != len) {
        fprintf(stderr, "  consumed %d bytes, expected %zu (got '%s')\n",
                n, len, buf);
        return 0;
    }
    return 1;
}

/* ----------------- MOV / MOVABS ----------------- */

static int test_x86_64_dis_mov_reg_reg(void)
{
    /* mov rax, rcx - REX.W=1 + 89 /r with reg=1, rm=0 */
    uint8_t b[] = { 0x48, 0x89, 0xC8 };
    return check(b, sizeof b, "mov rax, rcx");
}

static int test_x86_64_dis_mov_load_rbp(void)
{
    /* mov rax, [rbp-8] - REX.W + 8B 45 F8 */
    uint8_t b[] = { 0x48, 0x8B, 0x45, 0xF8 };
    return check(b, sizeof b, "mov rax, [rbp - 8]");
}

static int test_x86_64_dis_mov_store_rbp(void)
{
    /* mov [rbp-16], rax */
    uint8_t b[] = { 0x48, 0x89, 0x45, 0xF0 };
    return check(b, sizeof b, "mov [rbp - 16], rax");
}

static int test_x86_64_dis_movabs(void)
{
    /* movabs rax, 0x123456789ABCDEF0 */
    uint8_t b[] = { 0x48, 0xB8,
                    0xF0, 0xDE, 0xBC, 0x9A,
                    0x78, 0x56, 0x34, 0x12 };
    return check(b, sizeof b, "movabs rax, 0x123456789abcdef0");
}

static int test_x86_64_dis_mov_eax_imm(void)
{
    /* mov eax, 42 (no REX.W) */
    uint8_t b[] = { 0xB8, 0x2A, 0x00, 0x00, 0x00 };
    return check(b, sizeof b, "mov eax, 0x2a");
}

/* ----------------- ALU ----------------- */

static int test_x86_64_dis_add_reg(void)
{
    /* add rax, rcx (01 /r) */
    uint8_t b[] = { 0x48, 0x01, 0xC8 };
    return check(b, sizeof b, "add rax, rcx");
}

static int test_x86_64_dis_sub_reg(void)
{
    uint8_t b[] = { 0x48, 0x29, 0xC8 };
    return check(b, sizeof b, "sub rax, rcx");
}

static int test_x86_64_dis_and_reg(void)
{
    uint8_t b[] = { 0x48, 0x21, 0xC8 };
    return check(b, sizeof b, "and rax, rcx");
}

static int test_x86_64_dis_or_reg(void)
{
    uint8_t b[] = { 0x48, 0x09, 0xC8 };
    return check(b, sizeof b, "or rax, rcx");
}

static int test_x86_64_dis_xor_reg(void)
{
    uint8_t b[] = { 0x48, 0x31, 0xC8 };
    return check(b, sizeof b, "xor rax, rcx");
}

static int test_x86_64_dis_xor_eax_eax(void)
{
    /* xor eax, eax - no REX, two-byte canonical clear */
    uint8_t b[] = { 0x31, 0xC0 };
    return check(b, sizeof b, "xor eax, eax");
}

static int test_x86_64_dis_cmp_reg(void)
{
    uint8_t b[] = { 0x48, 0x39, 0xC8 };
    return check(b, sizeof b, "cmp rax, rcx");
}

static int test_x86_64_dis_test_reg(void)
{
    uint8_t b[] = { 0x48, 0x85, 0xC0 };
    return check(b, sizeof b, "test rax, rax");
}

static int test_x86_64_dis_imul(void)
{
    /* imul rax, rcx - 0F AF /r */
    uint8_t b[] = { 0x48, 0x0F, 0xAF, 0xC1 };
    return check(b, sizeof b, "imul rax, rcx");
}

/* ALU with immediate */
static int test_x86_64_dis_add_imm8(void)
{
    /* add rax, 5 - 83 /0 ib */
    uint8_t b[] = { 0x48, 0x83, 0xC0, 0x05 };
    return check(b, sizeof b, "add rax, 5");
}

static int test_x86_64_dis_sub_rsp_imm8(void)
{
    /* sub rsp, 80 */
    uint8_t b[] = { 0x48, 0x83, 0xEC, 0x50 };
    return check(b, sizeof b, "sub rsp, 80");
}

static int test_x86_64_dis_sub_rsp_imm32(void)
{
    /* sub rsp, 256 - 81 /5 id */
    uint8_t b[] = { 0x48, 0x81, 0xEC, 0x00, 0x01, 0x00, 0x00 };
    return check(b, sizeof b, "sub rsp, 256");
}

/* ----------------- shifts ----------------- */

static int test_x86_64_dis_shl_cl(void)
{
    /* shl rax, cl - D3 /4 */
    uint8_t b[] = { 0x48, 0xD3, 0xE0 };
    return check(b, sizeof b, "shl rax, cl");
}

static int test_x86_64_dis_shr_imm(void)
{
    /* shr rax, 3 - C1 /5 ib */
    uint8_t b[] = { 0x48, 0xC1, 0xE8, 0x03 };
    return check(b, sizeof b, "shr rax, 3");
}

/* ----------------- F7 group ----------------- */

static int test_x86_64_dis_neg(void)
{
    /* neg rax - F7 /3 */
    uint8_t b[] = { 0x48, 0xF7, 0xD8 };
    return check(b, sizeof b, "neg rax");
}

static int test_x86_64_dis_not(void)
{
    uint8_t b[] = { 0x48, 0xF7, 0xD0 };
    return check(b, sizeof b, "not rax");
}

static int test_x86_64_dis_idiv(void)
{
    /* idiv rcx - F7 /7 */
    uint8_t b[] = { 0x48, 0xF7, 0xF9 };
    return check(b, sizeof b, "idiv rcx");
}

static int test_x86_64_dis_div(void)
{
    uint8_t b[] = { 0x48, 0xF7, 0xF1 };
    return check(b, sizeof b, "div rcx");
}

/* ----------------- MOVSX / MOVZX / MOVSXD ----------------- */

static int test_x86_64_dis_movsx_b(void)
{
    /* movsx rax, byte [rcx] - REX.W + 0F BE /r */
    uint8_t b[] = { 0x48, 0x0F, 0xBE, 0x01 };
    return check(b, sizeof b, "movsx rax, [rcx]");
}

static int test_x86_64_dis_movzx_h(void)
{
    /* movzx rax, word [rcx] */
    uint8_t b[] = { 0x48, 0x0F, 0xB7, 0x01 };
    return check(b, sizeof b, "movzx rax, [rcx]");
}

static int test_x86_64_dis_movsxd(void)
{
    /* movsxd rax, ecx */
    uint8_t b[] = { 0x48, 0x63, 0xC1 };
    return check(b, sizeof b, "movsxd rax, ecx");
}

/* ----------------- branches ----------------- */

static int test_x86_64_dis_jmp_rel32(void)
{
    /* jmp +16 */
    uint8_t b[] = { 0xE9, 0x10, 0x00, 0x00, 0x00 };
    return check(b, sizeof b, "jmp +16");
}

static int test_x86_64_dis_je_rel32(void)
{
    /* je +32 - 0F 84 id */
    uint8_t b[] = { 0x0F, 0x84, 0x20, 0x00, 0x00, 0x00 };
    return check(b, sizeof b, "je +32");
}

static int test_x86_64_dis_jne_rel8(void)
{
    /* jne +8 - 75 08 */
    uint8_t b[] = { 0x75, 0x08 };
    return check(b, sizeof b, "jne +8");
}

static int test_x86_64_dis_call_rel32(void)
{
    /* call +0x100 */
    uint8_t b[] = { 0xE8, 0x00, 0x01, 0x00, 0x00 };
    return check(b, sizeof b, "call +256");
}

static int test_x86_64_dis_call_reg(void)
{
    /* call r11 - FF /2 */
    uint8_t b[] = { 0x41, 0xFF, 0xD3 };
    return check(b, sizeof b, "call r11");
}

static int test_x86_64_dis_jmp_reg(void)
{
    /* jmp r11 - FF /4 */
    uint8_t b[] = { 0x41, 0xFF, 0xE3 };
    return check(b, sizeof b, "jmp r11");
}

/* ----------------- frame / setcc ----------------- */

static int test_x86_64_dis_push_rbp(void)
{
    uint8_t b[] = { 0x55 };
    return check(b, sizeof b, "push rbp");
}

static int test_x86_64_dis_pop_rbp(void)
{
    uint8_t b[] = { 0x5D };
    return check(b, sizeof b, "pop rbp");
}

static int test_x86_64_dis_mov_rbp_rsp(void)
{
    uint8_t b[] = { 0x48, 0x89, 0xE5 };
    return check(b, sizeof b, "mov rbp, rsp");
}

static int test_x86_64_dis_ret(void)
{
    uint8_t b[] = { 0xC3 };
    return check(b, sizeof b, "ret");
}

static int test_x86_64_dis_leave(void)
{
    uint8_t b[] = { 0xC9 };
    return check(b, sizeof b, "leave");
}

static int test_x86_64_dis_cqo(void)
{
    uint8_t b[] = { 0x48, 0x99 };
    return check(b, sizeof b, "cqo");
}

static int test_x86_64_dis_cdq(void)
{
    uint8_t b[] = { 0x99 };
    return check(b, sizeof b, "cdq");
}

static int test_x86_64_dis_sete(void)
{
    /* sete al - 0F 94 /0 */
    uint8_t b[] = { 0x0F, 0x94, 0xC0 };
    return check(b, sizeof b, "sete al");
}

static int test_x86_64_dis_setne(void)
{
    uint8_t b[] = { 0x0F, 0x95, 0xC0 };
    return check(b, sizeof b, "setne al");
}

static int test_x86_64_dis_movzbq_al_rax(void)
{
    /* movzx rax, al - 48 0F B6 C0 */
    uint8_t b[] = { 0x48, 0x0F, 0xB6, 0xC0 };
    return check(b, sizeof b, "movzx rax, al");
}

/* ----------------- LEA / RIP-relative ----------------- */

static int test_x86_64_dis_lea_rbp(void)
{
    /* lea rax, [rbp-8] */
    uint8_t b[] = { 0x48, 0x8D, 0x45, 0xF8 };
    return check(b, sizeof b, "lea rax, [rbp - 8]");
}

static int test_x86_64_dis_lea_rip(void)
{
    /* lea rax, [rip+16] - 48 8D 05 10 00 00 00 */
    uint8_t b[] = { 0x48, 0x8D, 0x05, 0x10, 0x00, 0x00, 0x00 };
    return check(b, sizeof b, "lea rax, [rip+16]");
}

/* ----------------- syscall / nop / hlt ----------------- */

static int test_x86_64_dis_nop(void) {
    uint8_t b[] = { 0x90 };
    return check(b, sizeof b, "nop");
}

static int test_x86_64_dis_hlt(void) {
    uint8_t b[] = { 0xF4 };
    return check(b, sizeof b, "hlt");
}

static int test_x86_64_dis_syscall(void) {
    uint8_t b[] = { 0x0F, 0x05 };
    return check(b, sizeof b, "syscall");
}

/* ----------------- buffer dump ----------------- */

static int test_x86_64_dis_buffer_dump(void)
{
    /* A real x86 prologue + body:
     *   push rbp
     *   mov rbp, rsp
     *   sub rsp, 16
     *   mov rax, rdi
     *   add rax, rsi
     *   leave
     *   ret
     */
    uint8_t b[] = {
        0x55,
        0x48, 0x89, 0xE5,
        0x48, 0x83, 0xEC, 0x10,
        0x48, 0x89, 0xF8,
        0x48, 0x01, 0xF0,
        0xC9,
        0xC3
    };
    char *p = NULL;
    size_t n = 0;
    FILE *f = open_memstream(&p, &n);
    x86_64_disasm_buf(b, sizeof b, f);
    fclose(f);
    const char *want =
        "   0: 55                       push rbp\n"
        "   1: 48 89 e5                 mov rbp, rsp\n"
        "   4: 48 83 ec 10              sub rsp, 16\n"
        "   8: 48 89 f8                 mov rax, rdi\n"
        "  11: 48 01 f0                 add rax, rsi\n"
        "  14: c9                       leave\n"
        "  15: c3                       ret\n";
    int ok = (p && strcmp(p, want) == 0);
    if (!ok && p) fprintf(stderr, "got:\n%s---want:\n%s", p, want);
    free(p);
    return ok;
}

/* ----------------- new integer decoders ----------------- */

static int test_x86_64_dis_shl_one(void)
{
    /* shl rax, 1 - D1 /4 (not the CL form, which is D3 /4) */
    uint8_t b[] = { 0x48, 0xD1, 0xE0 };
    return check(b, sizeof b, "shl rax, 1");
}

static int test_x86_64_dis_adc_reg(void)
{
    uint8_t b[] = { 0x48, 0x11, 0xD8 };
    return check(b, sizeof b, "adc rax, rbx");
}

static int test_x86_64_dis_sbb_reg(void)
{
    uint8_t b[] = { 0x48, 0x19, 0xD8 };
    return check(b, sizeof b, "sbb rax, rbx");
}

static int test_x86_64_dis_mov_r8_imm8(void)
{
    /* mov al, 0x12 - B0+r ib */
    uint8_t b[] = { 0xB0, 0x12 };
    return check(b, sizeof b, "mov al, 0x12");
}

static int test_x86_64_dis_ret_imm16(void)
{
    /* ret 16 - C2 iw; the assembler spells it `ret1 16` */
    uint8_t b[] = { 0xC2, 0x10, 0x00 };
    return check(b, sizeof b, "ret1 16");
}

static int test_x86_64_dis_rep_movsb(void)
{
    uint8_t b[] = { 0xF3, 0xA4 };
    return check(b, sizeof b, "rep_movsb");
}

/* ----------------- SSE ----------------- */

static int test_x86_64_dis_addsd(void)
{
    /* addsd xmm0, xmm1 - F2 0F 58 /r */
    uint8_t b[] = { 0xF2, 0x0F, 0x58, 0xC1 };
    return check(b, sizeof b, "addsd xmm0, xmm1");
}

static int test_x86_64_dis_movss_load(void)
{
    /* movss xmm0, [rbx] - F3 0F 10 /r */
    uint8_t b[] = { 0xF3, 0x0F, 0x10, 0x03 };
    return check(b, sizeof b, "movss xmm0, [rbx]");
}

static int test_x86_64_dis_movss_store(void)
{
    /* movss [rbx], xmm0 - F3 0F 11 /r (operands swap) */
    uint8_t b[] = { 0xF3, 0x0F, 0x11, 0x03 };
    return check(b, sizeof b, "movss [rbx], xmm0");
}

static int test_x86_64_dis_movaps_reg(void)
{
    /* movaps xmm0, xmm1 - 0F 28 /r, no prefix */
    uint8_t b[] = { 0x0F, 0x28, 0xC1 };
    return check(b, sizeof b, "movaps xmm0, xmm1");
}

static int test_x86_64_dis_xorps_high_regs(void)
{
    /* xorps xmm8, xmm9 - REX.R+B before 0F */
    uint8_t b[] = { 0x45, 0x0F, 0x57, 0xC1 };
    return check(b, sizeof b, "xorps xmm8, xmm9");
}

static int test_x86_64_dis_ucomisd(void)
{
    /* ucomisd xmm0, xmm1 - 66 0F 2E /r */
    uint8_t b[] = { 0x66, 0x0F, 0x2E, 0xC1 };
    return check(b, sizeof b, "ucomisd xmm0, xmm1");
}

static int test_x86_64_dis_cvtsi2sd_r64(void)
{
    /* cvtsi2sd xmm0, rax - F2 REX.W 0F 2A /r (GPR source) */
    uint8_t b[] = { 0xF2, 0x48, 0x0F, 0x2A, 0xC0 };
    return check(b, sizeof b, "cvtsi2sd xmm0, rax");
}

static int test_x86_64_dis_cvttsd2si_r64(void)
{
    /* cvttsd2si rax, xmm0 - F2 REX.W 0F 2C /r (GPR dest) */
    uint8_t b[] = { 0xF2, 0x48, 0x0F, 0x2C, 0xC0 };
    return check(b, sizeof b, "cvttsd2si rax, xmm0");
}

/* ----------------- round-trip: asm -> dis -> asm -----------------
 *
 * Mirror of the arm64 round-trip checks: assemble a source line,
 * disassemble the bytes, re-assemble the disassembly, and require the
 * two byte streams to be identical. Catches both decoder gaps (output
 * degrades to `.byte`) and syntax drift (dis prints something the
 * assembler won't take back).
 *
 * Corpus = the integer GPR core the disassembler documents. SSE is a
 * known decoder gap; add those lines when dis_x86_64 grows SSE. */

static int
rt_encode(const char *src, uint8_t *out, int max)
{
    TestCtx T;
    test_ctx_init(&T, TARGET_X86_64_DARWIN);
    test_asm_enc(&T, (char *)src);
    int n = (int)T.enc.len;
    if (n > max) n = max;
    memcpy(out, T.enc.bytes, (size_t)n);
    test_asm_enc_free(&T);
    return n;
}

static void
rt_hex(const uint8_t *b, int n, FILE *f)
{
    for (int i = 0; i < n; i++) fprintf(f, "%02x ", b[i]);
}

static int
rt_line(const char *src)
{
    uint8_t b1[64], b2[64];
    int n1 = rt_encode(src, b1, (int)sizeof b1);
    if (n1 <= 0) {
        fprintf(stderr, "  rt '%s': did not assemble\n", src);
        return 0;
    }
    char re[512] = "";
    int off = 0;
    while (off < n1) {
        char one[128];
        int c = x86_64_disasm(b1 + off, (size_t)(n1 - off), one, sizeof one);
        if (c <= 0 || one[0] == '.') {
            fprintf(stderr, "  rt '%s': undecodable at +%d ('%s')\n",
                    src, off, one);
            return 0;
        }
        strlcat(re, one, sizeof re);
        strlcat(re, "\n", sizeof re);
        off += c;
    }
    int n2 = rt_encode(re, b2, (int)sizeof b2);
    if (n2 != n1 || memcmp(b1, b2, (size_t)n1) != 0) {
        fprintf(stderr, "  rt '%s': ", src);
        rt_hex(b1, n1, stderr);
        fprintf(stderr, "-> \"%s\" -> ", re);
        rt_hex(b2, n2, stderr);
        fputc('\n', stderr);
        return 0;
    }
    return 1;
}

static int test_x86_64_dis_roundtrip(void)
{
    static const char *corpus[] = {
        /* mov */
        "mov rax, 1", "mov rbx, rax", "mov eax, 1", "mov eax, ebx",
        "mov r8, rax", "mov r9, r10", "mov rax, r8",
        "mov al, 0x12", "mov al, bl", "mov ax, 0x1234", "mov ax, bx",
        "mov rax, 0x123456789a", "movabs rax, 0x123456789a",
        "mov rax, [rbx]", "mov rax, [rbx + 8]", "mov rax, [rbx + 256]",
        "mov rax, [rbp - 8]", "mov [rbx], rax", "mov [rbp - 16], rax",
        /* alu */
        "add rax, rbx", "add eax, ebx", "add r8, r9", "add rax, 1",
        "add rax, 1000", "add rax, [rbx]", "add [rbx], rax",
        "sub rax, rbx", "sub rax, 4", "and rax, rbx", "and eax, 0x10",
        "or rax, rbx", "xor rax, rbx", "xor eax, eax",
        "adc rax, rbx", "sbb rax, rbx",
        "cmp rax, rbx", "test eax, eax", "test rax, 16",
        "inc rax", "inc ecx", "dec rax", "dec ecx",
        "neg rax", "neg eax", "not rax", "not eax",
        /* shifts */
        "shl rax, 1", "shl rax, 4", "shl eax, 3", "shl rax, cl",
        "shr rax, 4", "sar rax, 4",
        /* stack / flow / misc */
        "push rbp", "push rax", "push r12", "pop rbp", "pop rbx", "pop r13",
        "call rax", "jmp rax", "ret", "ret1 16", "leave",
        "cqo", "nop", "syscall", "rep_movsb",
        /* numeric pc-relative branches, as dis prints them */
        "jmp -5", "je -6", "jne -6", "jg -6", "jb -6", "ja -6",
        /* SSE scalar moves + arithmetic */
        "movss xmm0, xmm1", "movsd xmm0, xmm1", "movss xmm8, xmm0",
        "movsd xmm0, xmm15", "movss xmm0, [rbx]", "movss [rbx], xmm0",
        "movsd [rbx], xmm0", "movaps xmm0, xmm1", "movapd xmm0, xmm1",
        "movaps [rbx], xmm0", "movups xmm0, xmm1", "movupd xmm0, xmm1",
        "addss xmm0, xmm1", "addsd xmm0, xmm1", "addss xmm8, xmm9",
        "addss xmm0, [rbx]", "addsd xmm0, [rbx + 8]",
        "subss xmm0, xmm1", "subsd xmm0, xmm1",
        "mulss xmm0, xmm1", "mulsd xmm0, xmm1",
        "divss xmm0, xmm1", "divsd xmm0, xmm1",
        "sqrtss xmm0, xmm1", "sqrtsd xmm0, xmm1",
        "minss xmm0, xmm1", "minsd xmm0, xmm1",
        "maxss xmm0, xmm1", "maxsd xmm0, xmm1",
        /* SSE compare + packed bitwise */
        "ucomiss xmm0, xmm1", "ucomisd xmm0, xmm1",
        "comiss xmm0, xmm1", "comisd xmm0, xmm1",
        "andps xmm0, xmm1", "andpd xmm0, xmm1",
        "andnps xmm0, xmm1", "andnpd xmm0, xmm1",
        "orps xmm0, xmm1", "orpd xmm0, xmm1",
        "xorps xmm0, xmm1", "xorpd xmm0, xmm1", "xorps xmm0, xmm0",
        /* SSE converts */
        "cvtss2sd xmm0, xmm1", "cvtsd2ss xmm0, xmm1",
        "cvtsi2ss xmm0, eax", "cvtsi2ss xmm0, rax",
        "cvtsi2sd xmm0, eax", "cvtsi2sd xmm0, rax",
        "cvtss2si eax, xmm0", "cvtss2si rax, xmm0",
        "cvtsd2si eax, xmm0", "cvtsd2si rax, xmm0",
        "cvttss2si eax, xmm0", "cvttss2si rax, xmm0",
        "cvttsd2si eax, xmm0", "cvttsd2si rax, xmm0",
    };
    int ok = 1;
    for (size_t i = 0; i < sizeof corpus / sizeof corpus[0]; i++)
        if (!rt_line(corpus[i])) ok = 0;
    return ok;
}

int
main(void)
{
    DisTest tests[] = {
        PrepTest(test_x86_64_dis_mov_reg_reg),
        PrepTest(test_x86_64_dis_mov_load_rbp),
        PrepTest(test_x86_64_dis_mov_store_rbp),
        PrepTest(test_x86_64_dis_movabs),
        PrepTest(test_x86_64_dis_mov_eax_imm),
        PrepTest(test_x86_64_dis_add_reg),
        PrepTest(test_x86_64_dis_sub_reg),
        PrepTest(test_x86_64_dis_and_reg),
        PrepTest(test_x86_64_dis_or_reg),
        PrepTest(test_x86_64_dis_xor_reg),
        PrepTest(test_x86_64_dis_xor_eax_eax),
        PrepTest(test_x86_64_dis_cmp_reg),
        PrepTest(test_x86_64_dis_test_reg),
        PrepTest(test_x86_64_dis_imul),
        PrepTest(test_x86_64_dis_add_imm8),
        PrepTest(test_x86_64_dis_sub_rsp_imm8),
        PrepTest(test_x86_64_dis_sub_rsp_imm32),
        PrepTest(test_x86_64_dis_shl_cl),
        PrepTest(test_x86_64_dis_shr_imm),
        PrepTest(test_x86_64_dis_neg),
        PrepTest(test_x86_64_dis_not),
        PrepTest(test_x86_64_dis_idiv),
        PrepTest(test_x86_64_dis_div),
        PrepTest(test_x86_64_dis_movsx_b),
        PrepTest(test_x86_64_dis_movzx_h),
        PrepTest(test_x86_64_dis_movsxd),
        PrepTest(test_x86_64_dis_jmp_rel32),
        PrepTest(test_x86_64_dis_je_rel32),
        PrepTest(test_x86_64_dis_jne_rel8),
        PrepTest(test_x86_64_dis_call_rel32),
        PrepTest(test_x86_64_dis_call_reg),
        PrepTest(test_x86_64_dis_jmp_reg),
        PrepTest(test_x86_64_dis_push_rbp),
        PrepTest(test_x86_64_dis_pop_rbp),
        PrepTest(test_x86_64_dis_mov_rbp_rsp),
        PrepTest(test_x86_64_dis_ret),
        PrepTest(test_x86_64_dis_leave),
        PrepTest(test_x86_64_dis_cqo),
        PrepTest(test_x86_64_dis_cdq),
        PrepTest(test_x86_64_dis_sete),
        PrepTest(test_x86_64_dis_setne),
        PrepTest(test_x86_64_dis_movzbq_al_rax),
        PrepTest(test_x86_64_dis_lea_rbp),
        PrepTest(test_x86_64_dis_lea_rip),
        PrepTest(test_x86_64_dis_nop),
        PrepTest(test_x86_64_dis_hlt),
        PrepTest(test_x86_64_dis_syscall),
        PrepTest(test_x86_64_dis_buffer_dump),
        PrepTest(test_x86_64_dis_shl_one),
        PrepTest(test_x86_64_dis_adc_reg),
        PrepTest(test_x86_64_dis_sbb_reg),
        PrepTest(test_x86_64_dis_mov_r8_imm8),
        PrepTest(test_x86_64_dis_ret_imm16),
        PrepTest(test_x86_64_dis_rep_movsb),
        PrepTest(test_x86_64_dis_addsd),
        PrepTest(test_x86_64_dis_movss_load),
        PrepTest(test_x86_64_dis_movss_store),
        PrepTest(test_x86_64_dis_movaps_reg),
        PrepTest(test_x86_64_dis_xorps_high_regs),
        PrepTest(test_x86_64_dis_ucomisd),
        PrepTest(test_x86_64_dis_cvtsi2sd_r64),
        PrepTest(test_x86_64_dis_cvttsd2si_r64),
        PrepTest(test_x86_64_dis_roundtrip),
    };
    int n = (int)(sizeof tests / sizeof tests[0]);
    int failed = 0;
    for (int i = 0; i < n; i++) {
        int pass = tests[i].fn();
        if (pass) printf("PASSED: %s\n", tests[i].name);
        else      { printf("FAILED: %s\n", tests[i].name); failed++; }
    }
    printf("\n%d/%d passed\n", n - failed, n);
    return failed ? 1 : 0;
}
