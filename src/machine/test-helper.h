#ifndef HCC_ASM_TEST_HELPER_H__
#define HCC_ASM_TEST_HELPER_H__

#include "asm_enc.h"
#include "target.h"

typedef struct {
    AsmEnc enc;
    int expected_len;
    Target target;
    uint32_t expected[32];
} TestCtx;

typedef struct {
    const char *name;
    int (*fn)(TestCtx *ctx);
    int pass;
} EncoderTest;

typedef struct {
    const char *name;
    int (*fn)(void);
    int pass;
} JitTest;

#define PrepTest(fn) {#fn,fn,0}

void test_fail(const char *fmt, ...);
void test_pass(const char *fmt, ...);
void test_ctx_init(TestCtx *T, Target target);
void test_asm_enc_free(TestCtx *T);
void test_asm_enc(TestCtx *T, char *asm_body);

#endif
