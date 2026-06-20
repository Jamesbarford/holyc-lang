#include <strings.h>
#include <string.h>

#include "target.h"

Target
target_host(void)
{
#if defined(__aarch64__) || defined(__arm64__)
    return TASM_ARCH_ARM64;
#else
    return TASM_ARCH_X86_64;
#endif
}

int
target_parse(const char *s, Target *out)
{
    if (!s) return 0;

    /* x86_64 any common spelling. */
    if (!strcmp(s, "x86_64") ||
        !strcmp(s, "x86_64-apple-darwin") ||
        !strcmp(s, "x86_64-darwin") ||
        !strcmp(s, "x86_64-linux-gnu") ||
        !strcmp(s, "x86_64-pc-linux-gnu") ||
        !strcmp(s, "x86_64-unknown-linux-gnu"))
    {
        *out = TASM_ARCH_X86_64;
        return 1;
    }

    /* arm64 / aarch64 same arch, different spelling. */
    if (!strcmp(s, "arm64") ||
        !strcmp(s, "aarch64") ||
        !strcmp(s, "arm64-apple-darwin") ||
        !strcmp(s, "aarch64-apple-darwin") ||
        !strcmp(s, "arm64-darwin") ||
        !strcmp(s, "aarch64-linux-gnu") ||
        !strcmp(s, "aarch64-pc-linux-gnu") ||
        !strcmp(s, "aarch64-unknown-linux-gnu"))
    {
        *out = TASM_ARCH_ARM64;
        return 1;
    }
    return 0;
}

const char *
target_triple(Target t)
{
    switch (t) {
        case TASM_ARCH_X86_64: return "x86_64";
        case TASM_ARCH_ARM64:  return "AArch64";
    }
    return "?";
}

const TasmMnemonicTable *
tasm_mnemonic_table_for(Target target)
{
    switch (target) {
        case TASM_ARCH_ARM64:  return aarch64_mnemonic_table();
        case TASM_ARCH_X86_64: return x86_64_mnemonic_table();
    }
    return NULL;
}

int
tasm_mnemonic_lookup(const TasmMnemonicTable *t, const char *name)
{
    if (!t || !name) return TASM_MN_UNKNOWN;
    int lo = 0, hi = t->n_entries - 1;
    while (lo <= hi) {
        int mid = lo + (hi - lo) / 2;
        int cmp = strcasecmp(name, t->entries[mid].name);
        if (cmp == 0) return t->entries[mid].id;
        if (cmp < 0) hi = mid - 1;
        else lo = mid + 1;
    }
    return TASM_MN_UNKNOWN;
}
