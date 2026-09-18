#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "test-helper.h"

void
test_pass(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    char tmp[BUFSIZ];
    vsnprintf(tmp, sizeof tmp, fmt, ap);
    if (isatty(STDOUT_FILENO)) {
        printf("\033[0;32mPASSED\033[0m: %s", tmp);
    } else {
        printf("PASSED: %s", tmp);
    }
    va_end(ap);
}

void
test_fail(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    char tmp[BUFSIZ];
    vsnprintf(tmp, sizeof tmp, fmt, ap);
    if (isatty(STDOUT_FILENO)) {
        printf("\033[1;91mFAILED\033[0m: %s", tmp);
    } else {
        printf("FAILED: %s", tmp);
    }
    va_end(ap);
}

void
test_asm_enc_free(TestCtx *T)
{
    asm_enc_free(&T->enc);
    asm_enc_init(&T->enc);
}

void
test_ctx_init(TestCtx *T, Target target)
{
    asm_enc_init(&T->enc);
    T->target = target;
}

void
test_asm_enc(TestCtx *T, char *asm_body)
{
    /* encoded bytes are now on `AsmEnc` */
    asm_parse_and_encode(&T->enc,
                         NULL,
                         T->target,
                         asm_body,
                         strlen(asm_body),
                         "test_file",
                         0,
                         NULL,
                         NULL);
}
