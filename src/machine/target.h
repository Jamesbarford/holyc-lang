#ifndef HCC_TARGET_H__
#define HCC_TARGET_H__

/* CPU architecture for the instruction encoder. The bytes we emit are
 * the same regardless of the host OS / object format - Mach-O vs ELF is
 * the caller's problem. The enum values are kept as `TARGET_*_DARWIN`
 * for backwards compatibility with code that vendored an earlier copy
 * of libtasm; new code should prefer the OS-free `TASM_ARCH_*` names. */
typedef enum {
    TASM_ARCH_X86_64 = 0,
    TASM_ARCH_ARM64 = 1,

    /* Back-compat aliases - same numeric values; no OS implied. */
    TARGET_X86_64_DARWIN = TASM_ARCH_X86_64,
    TARGET_ARM64_DARWIN = TASM_ARCH_ARM64,
} Target;

/* The host architecture - what this build of libtasm is running on. */
Target target_host(void);

/* Parse a `--target=...` argument. Returns 1 on success and writes
 * *out; returns 0 on unknown triple. Accepts:
 *   x86_64, x86_64-apple-darwin, x86_64-darwin,
 *   x86_64-linux-gnu, x86_64-unknown-linux-gnu, x86_64-pc-linux-gnu,
 *   arm64, aarch64, arm64-apple-darwin, aarch64-apple-darwin,
 *   arm64-darwin, aarch64-linux-gnu, aarch64-unknown-linux-gnu,
 *   aarch64-pc-linux-gnu. */
int target_parse(const char *s, Target *out);

/* Human-readable arch name, for diagnostics. */
const char *target_triple(Target t);

/* ---------------- Mnemonic tables ----------------
 *
 * Each backend owns an enum of the mnemonics it recognises and a sorted
 * `{name, id}` table mapping the source text to that enum. `asm.c` knows
 * nothing about either enum; at parse time it asks `target.c` for the
 * right table and does a case-insensitive binary search to convert the
 * mnemonic text into an opaque `int` id which is stashed on `AsmLine`.
 *
 * Aliases (jz/je, cqo/cqto, hs/cs, sal/shl, ...) appear as multiple
 * `name` entries pointing at the same `id`, so the encoder only sees one
 * case. */

typedef struct {
    const char *name;
    int id;
} TasmMnemonicEntry;

typedef struct {
    const TasmMnemonicEntry *entries;
    int n_entries;
} TasmMnemonicTable;

/* Sentinel value for "mnemonic not in the table". Each backend's enum
 * reserves 0 for its own UNKNOWN so zero-initialised lines are safe. */
#define TASM_MN_UNKNOWN 0

/* Per-backend table getters. Defined in enc_arm64.c / enc_x86_64.c. */
const TasmMnemonicTable *aarch64_mnemonic_table(void);
const TasmMnemonicTable *x86_64_mnemonic_table(void);

/* Pick the right table for `target`. Returns NULL for an unknown target. */
const TasmMnemonicTable *tasm_mnemonic_table_for(Target target);

/* Case-insensitive binary-search lookup. Returns the matched entry's id,
 * or TASM_MN_UNKNOWN if no entry's name matches `name`. */
int tasm_mnemonic_lookup(const TasmMnemonicTable *t, const char *name);

#endif
