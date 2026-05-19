#include <strings.h>
#include <math.h>
#include <string.h>

#include "aarch64.h"
#include "asm.h"
#include "cli.h"
#include "x86.h"
#include "x86_64.h"
#include "util.h"

/* Encode `64` as a `u64` */
uint64_t ieee754(double _f64) {
    if (_f64 == 0.0) return 0;  // Handle zero value explicitly

    // Calculate exponent and adjust fraction
    double base2_exp = floorl(log2l(fabs(_f64)));
    double exponet2_removed = ldexpl(_f64, -base2_exp - 1);

    // Initialize fraction and calculate it bit by bit
    uint64_t fraction = 0;
    double digit = 0.5;  // Start with 1/2
    for (s64 i = 0; i != 53; i++) {
        if (exponet2_removed >= digit) {
            exponet2_removed -= digit;
            fraction |= 1ULL << (52 - i);
        }
        digit *= 0.5;  // Move to the next digit (1/4, 1/8, ...)
    }

    // Calculate exponent representation
    uint64_t exponent = ((1 << 10) - 1) + base2_exp;

    // Handle sign bit
    uint64_t sign = (_f64 < 0.0) ? 1 : 0;

    // Assemble the IEEE 754 representation
    return (sign << 63) |
           ((exponent & 0x7FF) << 52) |
           (fraction & ~(1ULL << 52));
}

/* This is a hacky, but seemingly functional way of me being able
 * to run this on Macos and Linux */
char *asmNormaliseFunctionName(Cctrl *cc, AoStr *fname) {
    AoStr *newfn = aoStrNew();
    switch(cc->target) {
        case TARGET_AARCH64_APPLE_DARWIN:
        case TARGET_X86_64_APPLE_DARWIN: {
            if (fname->data[0] != '_') {
                aoStrPutChar(newfn, '_');
            }
            break;
        }
        case TARGET_AARCH64_UNKNOWN_LINUX_GNU:
        case TARGET_X86_64_UNKNOWN_LINUX_GNU:
            break;
    }

    aoStrCatAoStr(newfn, fname);

    /* User-defined `Main` (capital M) is the conventional HolyC entry.
     *  - With initialisers: rename to `MainFn`. The synthetic init-driver
     *    function (named `main` lowercase below) is the real `_main`.
     *  - Without initialisers: lowercase to `main` so it links as the
     *    process entry point.
     * Compare case-sensitively so the synthetic `main` (which we feed in
     * lowercase) flows straight through as `_main`. */
    if (fname->len == 4 && !strncmp(fname->data, str_lit("Main"))) {
         if (cc->flags & CCTRL_ASM_HAS_INITIALISERS) {
            aoStrCatPrintf(newfn, "Fn");
        } else {
            aoStrToLowerCase(newfn);
        }
    }
    return aoStrMove(newfn);
}

AoStr *asmGenerate(Cctrl *cc) {
    switch (cc->target) {
        case TARGET_AARCH64_APPLE_DARWIN:
        case TARGET_AARCH64_UNKNOWN_LINUX_GNU:
            return aarch64AsmGenerate(cc);
        case TARGET_X86_64_APPLE_DARWIN:
        case TARGET_X86_64_UNKNOWN_LINUX_GNU:
            /* Default: IR-based src/x86_64.c. --use-legacy-x86 opts
             * back into the AST-based src/x86.c for comparison. */
            if (cc->flags & CCTRL_USE_LEGACY_X86) {
                return x86AsmGenerate(cc);
            }
            return x86_64AsmGenerate(cc);
    }
}
