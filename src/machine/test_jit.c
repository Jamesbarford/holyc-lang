/* Tests that exercise the JIT path end-to-end: parse asm, encode, finalize
 * into an executable mapping, call the result as a real function pointer,
 * and check the value it returns / its observable side effects. */

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <sys/types.h>
#include <unistd.h>

#include "asm_enc.h"
#include "asm_jit.h"
#include "test-helper.h"

/* `code` from the JIT is `void *`. Casting it to a function pointer is
 * the textbook JIT pattern but is technically -Wpedantic ISO C noise. */
#pragma GCC diagnostic ignored "-Wpedantic"

#if defined(__aarch64__) || defined(__arm64__)
#define HOST_TARGET TASM_ARCH_ARM64
#else
#define HOST_TARGET TASM_ARCH_X86_64
#endif

/* Build, finalize, and return the executable mapping. NULL on failure.
 * `out_jit` receives ownership; caller asm_jit_frees it. */
static void *
test_jit_build(const char *asm_body, AsmJitCode *out_jit)
{
    AsmEnc enc;
    asm_enc_init(&enc);
    AsmBlock *blk = asm_parse(asm_body, (int)strlen(asm_body),
                              "jit_test", 1, HOST_TARGET);
    int errs = asm_encode(blk, NULL, &enc);
    asm_block_free(blk);
    if (errs) {
        asm_enc_free(&enc);
        return NULL;
    }
    int rc = asm_jit_finalize(&enc, asm_jit_dlsym_resolver, NULL, out_jit);
    asm_enc_free(&enc);
    return rc == 0 ? out_jit->code : NULL;
}

/* ============================================================ aarch64 tests */

#if defined(__aarch64__) || defined(__arm64__)

/* Simplest possible JIT: `mov w0, #42 ; ret`. Cast to int (*)(void), call. */
static int
test_jit_a64_const_return_test(void)
{
    AsmJitCode jit;
    void *code = test_jit_build("mov w0, #42\nret\n", &jit);
    if (!code) return 0;
    int (*fn)(void) = (int (*)(void))code;
    int r = fn();
    asm_jit_free(&jit);
    return r == 42;
}

/* Single argument echo: `mov w0, w0` is a no-op so the value flows through. */
static int
test_jit_a64_identity_test(void)
{
    AsmJitCode jit;
    void *code = test_jit_build("ret\n", &jit);
    if (!code) return 0;
    int (*fn)(int) = (int (*)(int))code;
    int r = fn(7);
    asm_jit_free(&jit);
    return r == 7;
}

/* Addition: `add w0, w0, w1 ; ret`. */
static int
test_jit_a64_add_test(void)
{
    AsmJitCode jit;
    void *code = test_jit_build("add w0, w0, w1\nret\n", &jit);
    if (!code) return 0;
    int (*fn)(int, int) = (int (*)(int, int))code;
    int r = fn(20, 22);
    asm_jit_free(&jit);
    return r == 42;
}

/* Local branch via @@-label - exercises the AF_LOCAL patcher. The body
 * does:  if (w0 == 0) return 100; else return 200; */
static int
test_jit_a64_local_branch_test(void)
{
    AsmJitCode jit;
    void *code = test_jit_build(
        "cbnz w0, @@1\n"     /* w0 != 0 - jump to @@1 */
        "mov w0, #100\n"     /* taken when w0 == 0 */
        "ret\n"
        "@@1:\n"
        "mov w0, #200\n"
        "ret\n",
        &jit);
    if (!code) return 0;
    int (*fn)(int) = (int (*)(int))code;
    int z = fn(0);
    int nz = fn(7);
    asm_jit_free(&jit);
    return z == 100 && nz == 200;
}

/* External call via dlsym. JIT'd code calls `getpid`, returns its value.
 * Compare against the C-side `getpid()` to verify the call worked. */
static int
test_jit_a64_external_call_test(void)
{
    AsmJitCode jit;
    void *code = test_jit_build(
        "stp x29, x30, [sp, #-16]!\n"
        "bl _getpid\n"
        "ldp x29, x30, [sp], #16\n"
        "ret\n",
        &jit);
    if (!code) return 0;
    int (*fn)(void) = (int (*)(void))code;
    int j = fn();
    int c = (int)getpid();
    asm_jit_free(&jit);
    return j == c;
}

/* b.cond to external symbol exercises the b.cond->trampoline expansion
 * we added. Branch to _abort if w0 is negative; otherwise return w0.
 * We test only the not-taken path (positive input) to avoid actually
 * aborting the test process. */
static int
test_jit_a64_bcond_external_test(void)
{
    AsmJitCode jit;
    void *code = test_jit_build(
        "cmp w0, #0\n"
        "b.lt _abort\n"      /* trampoline form: skip if !lt, then b _abort */
        "ret\n",
        &jit);
    if (!code) return 0;
    int (*fn)(int) = (int (*)(int))code;
    int r = fn(123);  /* not-taken; should just return 123 */
    asm_jit_free(&jit);
    return r == 123;
}

/* ADRP+ADD pair to load the address of a local label, then load the
 * 32-bit value sitting there. Exercises the PAGE21 + PAGEOFF12 JIT
 * patchers end-to-end, with the target inside the same JIT mapping. */
static int
test_jit_a64_adrp_local_test(void)
{
    AsmJitCode jit;
    void *code = test_jit_build(
        "adrp x0, @@1\n"
        "add x0, x0, @@1\n"
        "ldr w0, [x0]\n"
        "ret\n"
        "@@1:\n"
        "dd 0xcafebabe\n",
        &jit);
    if (!code) return 0;
    unsigned (*fn)(void) = (unsigned (*)(void))code;
    unsigned r = fn();
    asm_jit_free(&jit);
    return r == 0xcafebabe;
}

/* MSR/MRS against FPCR - switch the FP rounding mode, do an FADD whose
 * result depends on it, then restore. RMode lives in FPCR bits 23:22:
 * 0=nearest, 1=+inf, 2=-inf, 3=zero. 1.0 + 1e-20 is exactly 1.0 under
 * round-to-nearest but bumps to nextafter(1.0) when rounding toward
 * +inf, so the two calls returning different values proves the MSR
 * write actually took effect. */
static int
test_jit_a64_fpcr_rounding_test(void)
{
    AsmJitCode jit;
    void *code = test_jit_build(
        "mrs x9, fpcr\n"      /* save caller's FPCR */
        "lsl x0, x0, #22\n"   /* mode -> RMode field */
        "msr fpcr, x0\n"
        "fadd d0, d0, d1\n"
        "msr fpcr, x9\n"      /* restore */
        "ret\n",
        &jit);
    if (!code) return 0;
    double (*fn)(long, double, double) = (double (*)(long, double, double))code;
    double nearest = fn(0, 1.0, 1e-20);
    double upward = fn(1, 1.0, 1e-20);
    asm_jit_free(&jit);
    return nearest == 1.0 && upward > 1.0;
}

#endif /* arm64 host */

/* ============================================================ x86_64 tests */

#if defined(__x86_64__)

static int
test_jit_x86_const_return_test(void)
{
    AsmJitCode jit;
    void *code = test_jit_build("mov eax, 42\nret\n", &jit);
    if (!code) return 0;
    int (*fn)(void) = (int (*)(void))code;
    int r = fn();
    asm_jit_free(&jit);
    return r == 42;
}

static int
test_jit_x86_add_test(void)
{
    AsmJitCode jit;
    /* System V x86_64 ABI: 1st arg in edi, 2nd in esi, return in eax. */
    void *code = test_jit_build(
        "mov eax, edi\n"
        "add eax, esi\n"
        "ret\n", &jit);
    if (!code) return 0;
    int (*fn)(int, int) = (int (*)(int, int))code;
    int r = fn(20, 22);
    asm_jit_free(&jit);
    return r == 42;
}

/* Local branch via Jcc + @@-label. Exercises the AF_LOCAL pcrel=1
 * width-4 patch path. */
static int
test_jit_x86_local_branch_test(void)
{
    AsmJitCode jit;
    void *code = test_jit_build(
        "test edi, edi\n"
        "jnz @@1\n"
        "mov eax, 100\n"
        "ret\n"
        "@@1:\n"
        "mov eax, 200\n"
        "ret\n",
        &jit);
    if (!code) return 0;
    int (*fn)(int) = (int (*)(int))code;
    int z = fn(0);
    int nz = fn(7);
    asm_jit_free(&jit);
    return z == 100 && nz == 200;
}

/* External call via dlsym - exercises the BRANCH (CALL32) reloc. The
 * 8-byte `sub rsp, 8` keeps the stack 16-byte aligned across the call. */
static int
test_jit_x86_external_call_test(void)
{
    AsmJitCode jit;
    void *code = test_jit_build(
        "sub rsp, 8\n"
        "call _getpid\n"
        "add rsp, 8\n"
        "ret\n",
        &jit);
    if (!code) return 0;
    int (*fn)(void) = (int (*)(void))code;
    int j = fn();
    int c = (int)getpid();
    asm_jit_free(&jit);
    return j == c;
}

/* LEA + local label - exercises the AFR_X86_64_SIGNED patcher (which
 * reuses the same rel32 logic) against a label inside the JIT mapping. */
static int
test_jit_x86_lea_local_test(void)
{
    AsmJitCode jit;
    void *code = test_jit_build(
        "lea rax, @@1\n"
        "mov eax, [rax]\n"
        "ret\n"
        "@@1:\n"
        "dd 0xcafebabe\n",
        &jit);
    if (!code) return 0;
    unsigned (*fn)(void) = (unsigned (*)(void))code;
    unsigned r = fn();
    asm_jit_free(&jit);
    return r == 0xcafebabe;
}

#endif /* x86_64 host */

int
main(void)
{
    JitTest tests[] = {
#if defined(__aarch64__) || defined(__arm64__)
        PrepTest(test_jit_a64_const_return_test),
        PrepTest(test_jit_a64_identity_test),
        PrepTest(test_jit_a64_add_test),
        PrepTest(test_jit_a64_local_branch_test),
        PrepTest(test_jit_a64_external_call_test),
        PrepTest(test_jit_a64_bcond_external_test),
        PrepTest(test_jit_a64_adrp_local_test),
        PrepTest(test_jit_a64_fpcr_rounding_test),
#endif
#if defined(__x86_64__)
        PrepTest(test_jit_x86_const_return_test),
        PrepTest(test_jit_x86_add_test),
        PrepTest(test_jit_x86_local_branch_test),
        PrepTest(test_jit_x86_external_call_test),
        PrepTest(test_jit_x86_lea_local_test),
#endif
    };
    int n = (int)(sizeof tests / sizeof tests[0]);
    int failed = 0;
    for (int i = 0; i < n; i++) {
        tests[i].pass = tests[i].fn();
        if (tests[i].pass) {
            printf("PASSED: %s\n", tests[i].name);
        } else {
            printf("FAILED: %s\n", tests[i].name);
            failed++;
        }
    }
    printf("\n%d/%d passed\n", n - failed, n);
    return failed ? 1 : 0;
}
