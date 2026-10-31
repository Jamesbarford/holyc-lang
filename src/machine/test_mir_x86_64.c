#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "asm.h"
#include "asm_enc.h"
#include "enc_x86_64.h"
#include "mir.h"
#include "target.h"
#include "test-helper.h"

/* x86_64 MIR round-trip tests, the same invariant as test_mir_arm64.c:
 * no golden bytes - the parser's encoding is pinned by test_x86_64.c -
 * so each case asserts the built MirFn prints as the expected text AND
 * encodes byte+fixup identical to parsing that text. */

/* The canonical formatter pads mnemonics into a column; compare with
 * runs of blanks collapsed (and none kept before a newline) so the
 * expected strings stay readable and the column width stays free to
 * change. */
static void
squash_blanks(const char *src, char *dst, size_t cap)
{
    size_t n = 0;
    int pending = 0;
    for (const char *p = src; *p && n + 2 < cap; p++) {
        if (*p == ' ' || *p == '\t') { pending = n > 0; continue; }
        if (pending && *p != '\n') dst[n++] = ' ';
        pending = 0;
        dst[n++] = *p;
    }
    dst[n] = '\0';
}

static int
mir_text_eq(MirFn *fn, const char *want)
{
    char got[4096];
    size_t off = 0;
    for (int i = 0; i < mir_fn_n_lines(fn); i++) {
        char line[256];
        mir_fn_format_line(fn, i, line, sizeof(line));
        off += (size_t)snprintf(got + off, sizeof(got) - off, "%s\n", line);
        if (off >= sizeof(got)) return 0;
    }
    char got_n[4096], want_n[4096];
    squash_blanks(got, got_n, sizeof(got_n));
    squash_blanks(want, want_n, sizeof(want_n));
    if (strcmp(got_n, want_n) != 0) {
        printf("  text mismatch\n  want:\n%s  got:\n%s", want, got);
        return 0;
    }
    return 1;
}

static int
mir_bytes_eq_parse(TestCtx *T, MirFn *fn, const char *text)
{
    if (mir_fn_encode(fn, NULL, &T->enc) != 0) {
        printf("  mir encode reported errors\n");
        return 0;
    }
    AsmEnc pe;
    asm_enc_init(&pe);
    asm_parse_and_encode(&pe, NULL, T->target, (char *)text,
                         (int)strlen(text), "test_mir", 0, NULL, NULL);
    int ok = 1;
    if (pe.errors) {
        printf("  parser encode reported errors\n");
        ok = 0;
    }
    if (ok && (pe.len != T->enc.len ||
               memcmp(pe.bytes, T->enc.bytes, pe.len) != 0)) {
        printf("  byte mismatch: mir %zu bytes, parser %zu bytes\n",
               T->enc.len, pe.len);
        ok = 0;
    }
    if (ok && pe.n_fixups != T->enc.n_fixups) {
        printf("  fixup count mismatch: mir %d, parser %d\n",
               T->enc.n_fixups, pe.n_fixups);
        ok = 0;
    }
    if (ok) {
        for (int i = 0; i < pe.n_fixups; i++) {
            AsmFixup *a = &T->enc.fixups[i];
            AsmFixup *b = &pe.fixups[i];
            if (a->kind != b->kind || a->reloc != b->reloc ||
                a->patch_offset != b->patch_offset ||
                a->width != b->width || a->pcrel != b->pcrel ||
                ((a->sym != NULL) != (b->sym != NULL)) ||
                (a->sym && strcmp(a->sym, b->sym) != 0)) {
                printf("  fixup %d differs\n", i);
                ok = 0;
                break;
            }
        }
    }
    asm_enc_free(&pe);
    return ok;
}

static int
mir_roundtrip(TestCtx *T, MirFn *fn, const char *text)
{
    int ok = mir_text_eq(fn, text) && mir_bytes_eq_parse(T, fn, text);
    mir_fn_free(fn);
    return ok;
}

/* ============================================================ cases */

static int
mir_x86_mov_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_MOV, mir_r64(R_RAX), mir_r64(R_RCX));
    mir2(fn, X86MN_MOV, mir_r64(R_RAX), mir_imm(42));
    mir2(fn, X86MN_MOV, mir_r32(R_RAX), mir_r32(R_RCX));
    mir2(fn, X86MN_MOV, mir_r8(R_RAX), mir_imm(3));
    return mir_roundtrip(T, fn, "mov rax, rcx\n"
                                "mov rax, #42\n"
                                "mov eax, ecx\n"
                                "mov al, #3\n");
}

static int
mir_x86_mov_imm64_test(TestCtx *T)
{
    /* Plain mov picks C7 imm32 for small values and the 10-byte wide
     * form past int32; movabs forces the wide form regardless. */
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_MOV, mir_r64(R_RAX), mir_imm(-1));
    mir2(fn, X86MN_MOV, mir_r64(R_R11), mir_imm(0x123456789LL));
    mir2(fn, X86MN_MOVABS, mir_r64(R_RAX), mir_imm(42));
    return mir_roundtrip(T, fn, "mov rax, #-1\n"
                                "mov r11, #4886718345\n"
                                "movabs rax, #42\n");
}

static int
mir_x86_alu_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_ADD, mir_r64(R_RAX), mir_r64(R_RCX));
    mir2(fn, X86MN_ADD, mir_r32(R_RAX), mir_r32(R_RCX));
    mir2(fn, X86MN_SUB, mir_r64(R_RSP), mir_imm(32));
    mir2(fn, X86MN_ADD, mir_r64(R_RAX), mir_imm(1000));
    mir2(fn, X86MN_AND, mir_r64(R_RAX), mir_r64(R_RCX));
    mir2(fn, X86MN_OR, mir_r64(R_RAX), mir_imm(8));
    mir2(fn, X86MN_XOR, mir_r32(R_RAX), mir_r32(R_RAX));
    mir2(fn, X86MN_XOR, mir_r32(R_R11), mir_r32(R_R11));
    mir2(fn, X86MN_CMP, mir_r64(R_RAX), mir_r64(R_RCX));
    mir2(fn, X86MN_CMP, mir_r64(R_RAX), mir_imm(0));
    mir2(fn, X86MN_TEST, mir_r64(R_RAX), mir_r64(R_RAX));
    return mir_roundtrip(T, fn, "add rax, rcx\n"
                                "add eax, ecx\n"
                                "sub rsp, #32\n"
                                "add rax, #1000\n"
                                "and rax, rcx\n"
                                "or rax, #8\n"
                                "xor eax, eax\n"
                                "xor r11d, r11d\n"
                                "cmp rax, rcx\n"
                                "cmp rax, #0\n"
                                "test rax, rax\n");
}

static int
mir_x86_muldiv_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_IMUL, mir_r64(R_RAX), mir_r64(R_RCX));
    mir2(fn, X86MN_IMUL, mir_r32(R_RAX), mir_r32(R_RCX));
    mir0(fn, X86MN_CQO);
    mir1(fn, X86MN_IDIV, mir_r64(R_RCX));
    mir1(fn, X86MN_DIV, mir_r64(R_RCX));
    mir1(fn, X86MN_NEG, mir_r64(R_RAX));
    mir1(fn, X86MN_NEG, mir_r32(R_RAX));
    mir1(fn, X86MN_NOT, mir_r64(R_RAX));
    return mir_roundtrip(T, fn, "imul rax, rcx\n"
                                "imul eax, ecx\n"
                                "cqo\n"
                                "idiv rcx\n"
                                "div rcx\n"
                                "neg rax\n"
                                "neg eax\n"
                                "not rax\n");
}

static int
mir_x86_shift_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_SHL, mir_r64(R_RAX), mir_imm(5));
    mir2(fn, X86MN_SHR, mir_r64(R_RAX), mir_imm(1));
    mir2(fn, X86MN_SAR, mir_r64(R_RAX), mir_r8(R_RCX));
    /* `sal` is X86MN_SHL's alphabetically-first (thus canonical)
     * spelling; the two names share an id and an encoding. */
    return mir_roundtrip(T, fn, "sal rax, #5\n"
                                "shr rax, #1\n"
                                "sar rax, cl\n");
}

static int
mir_x86_movx_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_MOVZX, mir_r64(R_RAX), mir_r8(R_RAX));
    mir2(fn, X86MN_MOVZX, mir_r64(R_RAX), mir_r16(R_RAX));
    mir2(fn, X86MN_MOVSX, mir_r64(R_RAX), mir_r8(R_RAX));
    mir2(fn, X86MN_MOVSX, mir_r64(R_RAX), mir_r16(R_RAX));
    /* 4-byte source = movsxd. */
    mir2(fn, X86MN_MOVSX, mir_r64(R_RAX), mir_r32(R_RAX));
    mir2(fn, X86MN_MOVSX, mir_r64(R_R11), mir_r32(R_RDX));
    return mir_roundtrip(T, fn, "movzx rax, al\n"
                                "movzx rax, ax\n"
                                "movsx rax, al\n"
                                "movsx rax, ax\n"
                                "movsx rax, eax\n"
                                "movsx r11, edx\n");
}

static int
mir_x86_movx_mem_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_MOVZX, mir_r64(R_RAX),
         mir_mem_hint(mir_mem(mir_r64(R_RBP), -8), 1));
    mir2(fn, X86MN_MOVSX, mir_r64(R_RAX),
         mir_mem_hint(mir_mem(mir_r64(R_RBP), -16), 2));
    mir2(fn, X86MN_MOVSX, mir_r64(R_RAX),
         mir_mem_hint(mir_mem(mir_r64(R_RBP), -4), 4));
    mir2(fn, X86MN_MOV, mir_r32(R_RAX), mir_mem(mir_r64(R_RBP), -4));
    return mir_roundtrip(T, fn, "movzx rax, u8 [rbp - 8]\n"
                                "movsx rax, u16 [rbp - 16]\n"
                                "movsx rax, u32 [rbp - 4]\n"
                                "mov eax, [rbp - 4]\n");
}

static int
mir_x86_loadstore_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_MOV, mir_r64(R_RAX), mir_mem(mir_r64(R_RBP), -8));
    mir2(fn, X86MN_MOV, mir_mem(mir_r64(R_RBP), -8), mir_r64(R_RAX));
    mir2(fn, X86MN_MOV, mir_mem(mir_r64(R_RBP), -1), mir_r8(R_RAX));
    mir2(fn, X86MN_MOV, mir_mem(mir_r64(R_RBP), -2), mir_r16(R_RAX));
    mir2(fn, X86MN_MOV, mir_mem(mir_r64(R_RBP), -4), mir_r32(R_RAX));
    mir2(fn, X86MN_MOV, mir_mem(mir_r64(R_RBP), -1), mir_r8(R_RSI));
    mir2(fn, X86MN_MOV, mir_r64(R_RAX), mir_mem(mir_r64(R_RSP), 8));
    mir2(fn, X86MN_MOV, mir_mem(mir_r64(R_RSP), 0), mir_r64(R_R11));
    return mir_roundtrip(T, fn, "mov rax, [rbp - 8]\n"
                                "mov [rbp - 8], rax\n"
                                "mov [rbp - 1], al\n"
                                "mov [rbp - 2], ax\n"
                                "mov [rbp - 4], eax\n"
                                "mov [rbp - 1], sil\n"
                                "mov rax, [rsp + 8]\n"
                                "mov [rsp], r11\n");
}

static int
mir_x86_sib_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_MOV, mir_r64(R_RAX),
         mir_mem_sib(mir_r64(R_RCX), mir_r64(R_RDX), 4, 8));
    mir2(fn, X86MN_MOV,
         mir_mem_sib(mir_r64(R_RCX), mir_r64(R_RDX), 8, 0), mir_r64(R_RAX));
    mir2(fn, X86MN_LEA, mir_r64(R_RAX), mir_mem(mir_r64(R_RBP), -16));
    mir2(fn, X86MN_LEA, mir_r64(R_RAX),
         mir_mem_sib(mir_r64(R_RCX), mir_r64(R_RDX), 2, 0));
    return mir_roundtrip(T, fn, "mov rax, [rcx + rdx*4 + 8]\n"
                                "mov [rcx + rdx*8], rax\n"
                                "lea rax, [rbp - 16]\n"
                                "lea rax, [rcx + rdx*2]\n");
}

static int
mir_x86_setcc_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir1(fn, X86MN_SETE, mir_r8(R_RAX));
    mir1(fn, X86MN_SETNE, mir_r8(R_RAX));
    mir1(fn, X86MN_SETL, mir_r8(R_RAX));
    mir1(fn, X86MN_SETA, mir_r8(R_RAX));
    mir1(fn, X86MN_SETB, mir_r8(R_R11));
    mir1(fn, X86MN_SETGE, mir_r8(R_RSI));
    return mir_roundtrip(T, fn, "sete al\n"
                                "setne al\n"
                                "setl al\n"
                                "seta al\n"
                                "setb r11b\n"
                                "setge sil\n");
}

static int
mir_x86_local_branch_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir_local_label(fn, 1);
    mir2(fn, X86MN_CMP, mir_r64(R_RAX), mir_imm(0));
    mir1(fn, X86MN_JE, mir_label_local(2));
    mir1(fn, X86MN_JG, mir_label_local(1));
    mir1(fn, X86MN_JMP, mir_label_local(1));
    mir_local_label(fn, 2);
    mir0(fn, X86MN_RET);
    return mir_roundtrip(T, fn, "@@1:\n"
                                "cmp rax, #0\n"
                                "je @@2\n"
                                "jg @@1\n"
                                "jmp @@1\n"
                                "@@2:\n"
                                "ret\n");
}

static int
mir_x86_call_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir1(fn, X86MN_CALL, mir_sym("_foo"));
    mir1(fn, X86MN_CALL, mir_r64(R_RAX));
    mir1(fn, X86MN_CALL, mir_mem(mir_r64(R_RSP), 8));
    return mir_roundtrip(T, fn, "call _foo\n"
                                "call rax\n"
                                "call [rsp + 8]\n");
}

static int
mir_x86_frame_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir1(fn, X86MN_PUSH, mir_r64(R_RBP));
    mir2(fn, X86MN_MOV, mir_r64(R_RBP), mir_r64(R_RSP));
    mir2(fn, X86MN_SUB, mir_r64(R_RSP), mir_imm(48));
    mir2(fn, X86MN_MOV, mir_r64(R_RSP), mir_r64(R_RBP));
    mir1(fn, X86MN_POP, mir_r64(R_RBP));
    mir0(fn, X86MN_LEAVE);
    mir0(fn, X86MN_RET);
    return mir_roundtrip(T, fn, "push rbp\n"
                                "mov rbp, rsp\n"
                                "sub rsp, #48\n"
                                "mov rsp, rbp\n"
                                "pop rbp\n"
                                "leave\n"
                                "ret\n");
}

static int
mir_x86_lea_sym_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_LEA, mir_r64(R_RAX), mir_sym("_mysym"));
    mir2(fn, X86MN_LEA, mir_r64(R_R11), mir_sym("_other"));
    return mir_roundtrip(T, fn, "lea rax, _mysym\n"
                                "lea r11, _other\n");
}

static int
mir_x86_gotpcrel_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_MOV, mir_r64(R_RAX), mir_sym("_free@GOTPCREL"));
    mir2(fn, X86MN_MOV, mir_r64(R_R11), mir_sym("_malloc@GOTPCREL"));
    return mir_roundtrip(T, fn, "mov rax, _free@GOTPCREL\n"
                                "mov r11, _malloc@GOTPCREL\n");
}

/* A symbol that spells a register or operator: bare, GNU as reads `Fs`
 * as the segment register and takes `call Si` for an indirect call. */
static int
mir_x86_gas_sym_quoting_test(TestCtx *T)
{
    (void)T;
    char buf[64];
    return !strcmp(asm_sym_gas_x86("Fs", buf, sizeof(buf)), "\"Fs\"") &&
           !strcmp(asm_sym_gas_x86("Fs@GOTPCREL", buf, sizeof(buf)),
                   "\"Fs\"@GOTPCREL") &&
           !strcmp(asm_sym_gas_x86("_Fs", buf, sizeof(buf)), "_Fs") &&
           !strcmp(asm_sym_gas_x86(".L12", buf, sizeof(buf)), ".L12") &&
           !strcmp(asm_sym_gas_x86("Fs+8", buf, sizeof(buf)), "Fs+8");
}

static int
mir_x86_sse_arith_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_ADDSD, mir_xmm(0), mir_xmm(1));
    mir2(fn, X86MN_SUBSD, mir_xmm(0), mir_xmm(1));
    mir2(fn, X86MN_MULSD, mir_xmm(0), mir_xmm(1));
    mir2(fn, X86MN_DIVSD, mir_xmm(0), mir_xmm(1));
    mir2(fn, X86MN_ADDSS, mir_xmm(0), mir_xmm(1));
    mir2(fn, X86MN_UCOMISD, mir_xmm(0), mir_xmm(1));
    mir2(fn, X86MN_UCOMISS, mir_xmm(0), mir_xmm(1));
    mir2(fn, X86MN_XORPD, mir_xmm(0), mir_xmm(0));
    mir2(fn, X86MN_XORPS, mir_xmm(1), mir_xmm(1));
    return mir_roundtrip(T, fn, "addsd xmm0, xmm1\n"
                                "subsd xmm0, xmm1\n"
                                "mulsd xmm0, xmm1\n"
                                "divsd xmm0, xmm1\n"
                                "addss xmm0, xmm1\n"
                                "ucomisd xmm0, xmm1\n"
                                "ucomiss xmm0, xmm1\n"
                                "xorpd xmm0, xmm0\n"
                                "xorps xmm1, xmm1\n");
}

static int
mir_x86_sse_mem_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_MOVSD, mir_xmm(0), mir_mem(mir_r64(R_RBP), -8));
    mir2(fn, X86MN_MOVSD, mir_mem(mir_r64(R_RBP), -8), mir_xmm(0));
    mir2(fn, X86MN_MOVSS, mir_xmm(0), mir_mem(mir_r64(R_RBP), -4));
    mir2(fn, X86MN_MOVSS, mir_mem(mir_r64(R_RBP), -4), mir_xmm(0));
    mir2(fn, X86MN_MOVSD, mir_mem(mir_r64(R_RSP), 8), mir_xmm(12));
    mir2(fn, X86MN_MOVSD, mir_xmm(0), mir_xmm(1));
    mir2(fn, X86MN_MOVSS, mir_xmm(2), mir_xmm(3));
    return mir_roundtrip(T, fn, "movsd xmm0, [rbp - 8]\n"
                                "movsd [rbp - 8], xmm0\n"
                                "movss xmm0, [rbp - 4]\n"
                                "movss [rbp - 4], xmm0\n"
                                "movsd [rsp + 8], xmm12\n"
                                "movsd xmm0, xmm1\n"
                                "movss xmm2, xmm3\n");
}

static int
mir_x86_movq_movd_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_MOVQ, mir_xmm(0), mir_r64(R_RAX));
    mir2(fn, X86MN_MOVQ, mir_r64(R_RAX), mir_xmm(0));
    mir2(fn, X86MN_MOVQ, mir_xmm(12), mir_r64(R_R11));
    mir2(fn, X86MN_MOVD, mir_xmm(1), mir_r32(R_RAX));
    mir2(fn, X86MN_MOVD, mir_r32(R_RAX), mir_xmm(1));
    /* No xmm operand: movq is the AT&T width suffix for plain mov. */
    mir2(fn, X86MN_MOVQ, mir_r64(R_RAX), mir_r64(R_RBX));
    return mir_roundtrip(T, fn, "movq xmm0, rax\n"
                                "movq rax, xmm0\n"
                                "movq xmm12, r11\n"
                                "movd xmm1, eax\n"
                                "movd eax, xmm1\n"
                                "movq rax, rbx\n");
}

static int
mir_x86_cvt_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    mir2(fn, X86MN_CVTTSD2SI, mir_r64(R_RAX), mir_xmm(0));
    mir2(fn, X86MN_CVTTSS2SI, mir_r64(R_RAX), mir_xmm(0));
    mir2(fn, X86MN_CVTSI2SD, mir_xmm(0), mir_r64(R_RAX));
    mir2(fn, X86MN_CVTSI2SS, mir_xmm(0), mir_r64(R_RAX));
    mir2(fn, X86MN_CVTSD2SS, mir_xmm(0), mir_xmm(0));
    mir2(fn, X86MN_CVTSS2SD, mir_xmm(0), mir_xmm(0));
    return mir_roundtrip(T, fn, "cvttsd2si rax, xmm0\n"
                                "cvttss2si rax, xmm0\n"
                                "cvtsi2sd xmm0, rax\n"
                                "cvtsi2ss xmm0, rax\n"
                                "cvtsd2ss xmm0, xmm0\n"
                                "cvtss2sd xmm0, xmm0\n");
}

/* The peephole: build the noisy shapes, run the pass, compare the
 * surviving stream. */
static int
mir_x86_peephole_test(TestCtx *T)
{
    MirFn *fn = mir_fn_new(NULL, TASM_ARCH_X86_64);
    /* jcc/jmp-to-next + self-move + dup extension + store/reload +
     * dead labels, interleaved with real work. */
    mir2(fn, X86MN_CMP, mir_r64(R_RAX), mir_imm(0));
    mir1(fn, X86MN_JGE, mir_label_local(1));             /* jcc to next */
    mir2(fn, X86MN_MOV, mir_r64(R_RAX), mir_r64(R_RAX)); /* self-move */
    mir1(fn, X86MN_JMP, mir_label_local(1));             /* jmp to next */
    mir_local_label(fn, 1);
    mir2(fn, X86MN_MOVSX, mir_r64(R_RAX), mir_r32(R_RAX));
    mir2(fn, X86MN_MOVSX, mir_r64(R_RAX), mir_r32(R_RAX)); /* dup */
    mir2(fn, X86MN_MOV, mir_r32(R_RAX), mir_r32(R_RAX)); /* NOT a no-op */
    mir2(fn, X86MN_MOV, mir_mem(mir_r64(R_RBP), -16), mir_r64(R_RAX));
    mir2(fn, X86MN_MOV, mir_r64(R_RAX), mir_mem(mir_r64(R_RBP), -16));
    mir_local_label(fn, 2);                              /* dead */
    mir0(fn, X86MN_RET);
    mir_fn_peephole_x86_64(fn, 0);
    return mir_roundtrip(T, fn, "cmp rax, #0\n"
                                "movsx rax, eax\n"
                                "mov eax, eax\n"
                                "mov [rbp - 16], rax\n"
                                "ret\n");
}

static int
mir_x86_fn_test(TestCtx *T)
{
    /* A whole tiny function: public label, body, epilogue. */
    MirFn *fn = mir_fn_new("_two", TASM_ARCH_X86_64);
    mir2(fn, X86MN_MOV, mir_r64(R_RAX), mir_imm(2));
    mir0(fn, X86MN_RET);
    return mir_roundtrip(T, fn, "_two::\n"
                                "mov rax, #2\n"
                                "ret\n");
}

/* ============================================================ main */

int
main(void)
{
    TestCtx T;
    test_ctx_init(&T, TASM_ARCH_X86_64);

    EncoderTest tests[] = {
        PrepTest(mir_x86_mov_test),
        PrepTest(mir_x86_mov_imm64_test),
        PrepTest(mir_x86_alu_test),
        PrepTest(mir_x86_muldiv_test),
        PrepTest(mir_x86_shift_test),
        PrepTest(mir_x86_movx_test),
        PrepTest(mir_x86_movx_mem_test),
        PrepTest(mir_x86_loadstore_test),
        PrepTest(mir_x86_sib_test),
        PrepTest(mir_x86_setcc_test),
        PrepTest(mir_x86_local_branch_test),
        PrepTest(mir_x86_call_test),
        PrepTest(mir_x86_frame_test),
        PrepTest(mir_x86_lea_sym_test),
        PrepTest(mir_x86_gotpcrel_test),
        PrepTest(mir_x86_gas_sym_quoting_test),
        PrepTest(mir_x86_sse_arith_test),
        PrepTest(mir_x86_sse_mem_test),
        PrepTest(mir_x86_movq_movd_test),
        PrepTest(mir_x86_cvt_test),
        PrepTest(mir_x86_peephole_test),
        PrepTest(mir_x86_fn_test),
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
            failed++;
        }
        test_asm_enc_free(&T);
    }

    printf("\n%d/%d passed\n", n - failed, n);
    return failed ? 1 : 0;
}
