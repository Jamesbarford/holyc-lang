#include <strings.h>
#include <string.h>

#include "aarch64.h"
#include "asm.h"
#include "machine/asm.h"
#include "machine/asm_enc.h"
#include "machine/dis_arm64.h"
#include "cctrl.h"
#include "cli.h"
#include "ir-inliner.h"
#include "ir.h"
#include "ir-optimise.h"
#include "config.h"
#include "prsasm.h"
#include "x86_64.h"
#include "types.h"
#include "util.h"
#include "version.h"

/* Encode `64` as a `u64`. Bit-cast through a union (like ieee754_32) so the
 * exact IEEE-754 representation is produced. The previous manual bit-by-bit
 * reconstruction zeroed the mantissa for negative values (the fraction loop
 * compared a negative number against positive thresholds), so e.g. -1.5 was
 * encoded as -1.0. */
uint64_t ieee754_64(double _f64) {
    union F64Conv { double f; uint64_t bits; };
    union F64Conv c;
    c.f = _f64;
    return c.bits;
}

uint32_t ieee754_32(f32 _f32) {
    union F32Conv { f32 f; uint32_t bits; };
    union F32Conv c;
    c.f = _f32;
    return c.bits;
}

/* Mach-O prefixes every C symbol with an underscore, data included. A
 * global `foo` is the symbol `_foo`, and `__stdoutp` in <stdio.h> is
 * `___stdoutp`. Getting this wrong only became visible once we started
 * linking against real libc rather than a library this compiler built
 * itself, where both sides happened to agree on the unmangled name.
 *
 * This also subsumes the older uppercase-`L` special case: Mach-O
 * treats a symbol starting with `L` as an assembler-temporary, so
 * `.globl LIGHTGRAY` was rejected - prefixing everything fixes that
 * for free.
 *
 * Raw assembly labels are exempt, exactly as for function names: they
 * are already final symbols. The SAME transform must be applied at the
 * definition and at every reference so they agree. */
AoStr *asmNormaliseGlobalLabel(Cctrl *cc, AoStr *name) {
    /* `extern long timezone __asm ("_timezone");` - a rename on DATA
     * works exactly as one on a function, and for the same reason. */
    const char *renamed = asmRenameFor(cc, name);
    if (renamed) return aoStrDupRaw((char *)renamed, strlen(renamed));
    int is_macos = cc->target == TARGET_AARCH64_APPLE_DARWIN ||
                   cc->target == TARGET_X86_64_APPLE_DARWIN;
    if (!is_macos || !name || name->len == 0) return name;
    if (cc->raw_asm_labels && setHas(cc->raw_asm_labels, name->data)) {
        return name;
    }
    /* A COMPILER-GENERATED label (`.L12`) is not a C identifier and
     * never had C linkage, so the underscore that gives a name the
     * platform's C spelling does not apply to it. Adding it did more
     * than rename: Mach-O treats a symbol starting with `L` as
     * assembler-local, so `.L12` is private and `_.L12` is PUBLIC.
     * Every static global therefore exported its label, and the label
     * counter restarts per translation unit - so linking any two
     * objects that both had a static global failed on a duplicate
     * `_.L12`. String literals were already exempt, having never been
     * routed through here, which is why only static globals showed it.
     *
     * Both the definition (jit-common) and the references (the two
     * backends) come through this function, so they stay in step. */
    if (name->len >= 2 && name->data[0] == '.' && name->data[1] == 'L') {
        return name;
    }
    AoStr *out = aoStrNew();
    aoStrPutChar(out, '_');
    aoStrCatAoStr(out, name);
    return out;
}

/* This is a hacky, but seemingly functional way of me being able
 * to run this on Macos and Linux */
/* A function with no body in this translation unit lives in another
 * image, so its ADDRESS has to come through the GOT: a direct
 * adrp+add (or a rip-relative leaq) leaves the static linker with
 * nothing to fold in, and it reports "does not have address". Calls
 * are unaffected - `bl`/`call` go via a stub. */
int asmFunctionIsExtern(Cctrl *cc, AoStr *fname) {
    Ast *fn = (Ast *)mapGetLen(cc->global_env, fname->data, fname->len);
    if (fn == NULL) return 1;
    switch (fn->kind) {
        case AST_FUNC:
        case AST_ASM_FUNCDEF:
        case AST_ASM_FUNC_BIND:
            return 0;
        default:
            return 1;
    }
}

/* Under `-fPIC` a symbol with EXTERNAL linkage may be interposed when
 * the image is loaded - another shared object's definition can win -
 * so its address is not known until the dynamic loader has run.
 * `adrp`+`add` (and a rip-relative `lea`) ask the STATIC linker to
 * fold the address in, which it cannot do; `ld` rejects the object
 * rather than guess: "relocation R_AARCH64_ADR_PREL_PG_HI21 against
 * symbol `glbl_mem_ctx' which may bind externally ... recompile with
 * -fPIC". The address has to be loaded from the GOT instead, which is
 * the one slot the loader does get to fill in.
 *
 * Two kinds of symbol are exempt, having internal linkage and so never
 * reaching the dynamic symbol table:
 *   - compiler-generated labels (`.L12`): static globals, string
 *     literals, jump tables.
 *   - `static` functions, which keep their C name but no `.globl`.
 *
 * Mach-O is exempt wholesale: its two-level namespace makes a symbol
 * defined in this image non-preemptible by default, and `@PAGE` /
 * `@PAGEOFF` against it is exactly what clang emits for `-fPIC`.
 *
 * Calls need none of this - `bl` / `call` route through a PLT stub the
 * linker synthesises - so this is only consulted where an ADDRESS is
 * materialised. */
int asmSymbolIsPreemptible(Cctrl *cc, const char *sym) {
    if (!cc->is_pic || sym == NULL) return 0;
    switch (cc->target) {
        case TARGET_AARCH64_APPLE_DARWIN:
        case TARGET_X86_64_APPLE_DARWIN:
            return 0;
        default:
            break;
    }
    if (sym[0] == '.') return 0;
    Ast *ast = (Ast *)mapGet(cc->global_env, (void *)sym);
    if (ast && (ast->flags & AST_FLAG_STATIC_FN)) return 0;
    return 1;
}

/* An `__asm("sym")` rename, if this name carries one.
 *
 * A rename REPLACES the symbol rather than decorating it, so it short-
 * circuits everything else the normalisers do: no platform underscore
 * (the headers already built the string out of
 * `__USER_LABEL_PREFIX__`), no `main` special case, no Mach-O `L`
 * escape. What the declaration asked for is what the assembler sees.
 *
 * Both normalisers consult this first, which is what lets one map
 * cover the definition, every call, every address taken of it, both
 * targets, and the AOT, JIT and object-writer paths alike - they all
 * come through here. */
/* `@[spangle]` - the symbol this name is emitted under.
 *
 * Deterministic, not random, however the attribute reads. A spangled
 * global defined in one translation unit and referenced from another
 * has to arrive at the same string in both, and a compiler that rolled
 * dice would link only by luck. The hash IS the randomisation: nothing
 * about `__k3f9zq7x.a1c9__` says `some_glbl`, and going back the other
 * way means inverting murmur.
 *
 * The `.` is what makes it unspellable. Every ELF and Mach-O toolchain
 * takes one in a symbol - gcc emits `foo.constprop.0` itself - and no C
 * identifier may contain one, so no program can name this by accident
 * or on purpose. `$` would do as well; a quote would NOT, and fails
 * quietly rather than loudly: `__0oads'zda__` assembles to
 * `__0oads122da__`, because gas reads `'z` as a character literal.
 *
 * Two hashes rather than one because they are printed in base 36 - one
 * 64-bit value is about 12 characters of it, and the second block is
 * what makes a collision between two names improbable rather than
 * merely unlikely. */
static const char spangle_digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";

static void asmSpangleBase36(AoStr *out, u64 v, int n) {
    char buf[16];
    for (int i = 0; i < n; ++i) {
        buf[n - 1 - i] = spangle_digits[v % 36];
        v /= 36;
    }
    aoStrCatLen(out, buf, (u64)n);
}

AoStr *asmSpangleFor(AoStr *name) {
    u64 a = cstringMurmur(name->data, (s64)name->len);
    /* A second, independent value from the same bytes: the length is
     * folded in so `ab`/`abc` do not differ only in the tail. */
    u64 b = cstringMurmur(name->data, (s64)name->len) ^
            (0x9e3779b97f4a7c15ULL * (name->len + 1));
    AoStr *sym = aoStrNew();
    aoStrCatLen(sym, str_lit("__"));
    asmSpangleBase36(sym, a, 8);
    aoStrPutChar(sym, '.');
    asmSpangleBase36(sym, b, 6);
    aoStrCatLen(sym, str_lit("__"));
    return sym;
}

/* Bind `name` to its spangled symbol for the rest of the compilation.
 *
 * The forward direction is the same map `__asm("sym")` renames use, so
 * the two features share every emit path - definitions, calls,
 * addresses, both targets, AOT and JIT and the object writer alike. */
void asmSpangleRegister(Cctrl *cc, char *name, int len) {
    if (!cc->asm_renames || !name || len <= 0) return;
    AoStr *from = aoStrDupRaw(name, (u64)len);
    if (mapGetLen(cc->asm_renames, from->data, from->len)) {
        aoStrRelease(from);
        return;
    }
    AoStr *sym = asmSpangleFor(from);
    mapAdd(cc->asm_spangled, aoStrMove(aoStrDup(sym)), aoStrMove(aoStrDup(from)));
    mapAdd(cc->asm_renames, aoStrMove(from), aoStrMove(sym));
}

/* `-S` only: name the source symbol beside a spangled one.
 *
 * On its own line above the `.globl`, rather than trailing it, so it
 * needs no agreement with whatever comment character the target's
 * assembler wants - `<the slash asterix comment pattern>` is taken by all of
 * them. Emitted for the reader; the object file carries none of it, which is
 * the point. Nothing is written for a symbol that was not spangled. */
void asmSpangleNote(Cctrl *cc, AoStr *buf, char *sym) {
    const char *from = asmSpangledOrigin(cc, sym);
    if (from) {
        aoStrCatFmt(buf, "/* @[spangle] %s */\n", (char *)from);
    }
}

/* The source name behind a spangled symbol, or NULL. */
const char *asmSpangledOrigin(Cctrl *cc, const char *sym) {
    if (!cc->asm_spangled || !sym) return NULL;
    return (const char *)mapGet(cc->asm_spangled, (void *)sym);
}

const char *asmRenameFor(Cctrl *cc, AoStr *name) {
    if (!cc->asm_renames || !name) return NULL;
    return (const char *)mapGetLen(cc->asm_renames, name->data, name->len);
}

static char *asmRenamedSymbol(Cctrl *cc, AoStr *name) {
    const char *sym = asmRenameFor(cc, name);
    if (!sym) return NULL;
    return aoStrMove(aoStrDupRaw((char *)sym, strlen(sym)));
}

char *asmNormaliseFunctionName(Cctrl *cc, AoStr *fname) {
    char *renamed = asmRenamedSymbol(cc, fname);
    if (renamed) return renamed;

    AoStr *newfn = aoStrNew();
    /* Raw assembly labels are already final symbols; C identifiers get
     * the platform prefix. Deciding this from the spelling ("starts
     * with _") was wrong: system headers are full of C functions named
     * `__swbuf`, `__sputc` and friends, which do need the prefix. */
    int is_raw_label = cc->raw_asm_labels &&
                       setHas(cc->raw_asm_labels, fname->data);
    switch(cc->target) {
        case TARGET_AARCH64_APPLE_DARWIN:
        case TARGET_X86_64_APPLE_DARWIN: {
            if (!is_raw_label) {
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
     *  - With initialisers: rename to `MainFn`. The synthetic
     *    `__hcc_entry` driver below becomes the real entry symbol.
     *  - Without initialisers: lowercase to `main` so it links as the
     *    process entry point.
     * Compare case-sensitively - a user function genuinely called
     * `main` is left alone. */
    if (fname->len == 4 && !strncmp(fname->data, str_lit("Main"))) {
        if (cc->flags & CCTRL_ASM_HAS_INITIALISERS) {
            aoStrCatPrintf(newfn, "Fn");
        } else {
            aoStrToLowerCase(newfn);
        }
    }

    /* The synthetic initialiser driver is the real entry symbol. */
    if (fname->len == 11 && !strncmp(fname->data, str_lit("__hcc_entry"))) {
        aoStrRelease(newfn);
        newfn = aoStrNew();
        switch (cc->target) {
            case TARGET_AARCH64_APPLE_DARWIN:
            case TARGET_X86_64_APPLE_DARWIN:
                aoStrCatLen(newfn, str_lit("_main"));
                break;
            case TARGET_AARCH64_UNKNOWN_LINUX_GNU:
            case TARGET_X86_64_UNKNOWN_LINUX_GNU:
                aoStrCatLen(newfn, str_lit("main"));
                break;
        }
    }
    return aoStrMove(newfn);
}

/* Build the synthetic entry point that runs file-scope initialiser
 * statements (cc->initalisers), shared by every backend (aarch64 /
 * x86_64 AOT and the JIT). Returns NULL when there are no
 * initialisers - then the user's `Main` is the entry point directly.
 *
 * When initialisers DO exist the user's `Main` is renamed to `MainFn`
 * and this driver becomes `_main`, so the driver must call `Main`
 * itself - otherwise the program would run the top-level statements
 * and exit without ever entering Main. */
Ast *asmBuildInitialiserMain(Cctrl *cc) {
    if (listEmpty(cc->initalisers)) return NULL;
    cc->flags |= CCTRL_ASM_HAS_INITIALISERS;

    /* A scripting-style file can drive execution itself with a trailing
     * `Main;` at file scope. If the initialisers already call Main,
     * don't auto-call it again. The auto-call only restores the
     * convenience for the "globals + main, no explicit call" case. */
    int calls_main = 0;
    listForEach(cc->initalisers) {
        Ast *a = (Ast *)it->value;
        if (a && a->kind == AST_FUNCALL && a->fname &&
            a->fname->len == 4 && !strncmp(a->fname->data, str_lit("Main")))
        {
            calls_main = 1;
            break;
        }
    }

    Ast *user_main = (Ast *)mapGetLen(cc->global_env, str_lit("Main"));

    if (!calls_main && user_main && user_main->kind == AST_FUNC) {
        AstType *rt = user_main->type ? user_main->type->rettype : NULL;
        Ast *call = astFunctionCall(rt ? rt : ast_void_type,
                                    str_lit("Main"), astVecNew());
        if (rt && rt->kind != AST_TYPE_VOID) {
            /* Propagate Main's value as the exit code, matching the
             * no-initialiser case where Main *is* the entry point. */
            listAppend(cc->initalisers, astReturn(call, ast_i32_type));
        } else {
            listAppend(cc->initalisers, call);
            listAppend(cc->initalisers,
                       astReturn(astI64Type(0), ast_i32_type));
        }
    } else {
        listAppend(cc->initalisers,
                   astReturn(astI64Type(0), ast_i32_type));
    }

    Ast *body = astCompountStatement(cc->initalisers);
    Vec *empty_params = astVecNew();
    AstType *fn_type = astMakeFunctionType(ast_i32_type, empty_params);
    /* Named `__hcc_entry` rather than `main` so it doesn't collide with
     * the user's function in global_env; asmNormaliseFunctionName maps
     * it to the real entry symbol. */
    return astFunction(fn_type, str_lit("__hcc_entry"), empty_params, body,
                       cc->initaliser_locals, 0);
}

typedef struct AsmGenerator {
    IrCgCtx ctx;
    Set *seen_globals;
    void (*init_reg_pool)(void);
    void (*paste_data_section)(Cctrl *cc, AoStr *buf);
    void (*paste_asm_blocks)(Cctrl *cc, AoStr *buf);
    void (*emit_function)(IrCgCtx *ctx, Ast *fn);
    void (*emit_global)(Cctrl *cc, Set *seen_globals, AoStr *buf, Ast *ast);
} AsmGenerator;

static AsmGenerator *AArch64CodeGenerator(Cctrl *cc) {
    AsmGenerator *gen = malloc(sizeof(AsmGenerator));
    memset(&gen->ctx, 0, sizeof(gen->ctx));
    gen->ctx.buf = aoStrAlloc(2048);
    gen->ctx.cc = cc;
    gen->seen_globals = setNew(32, &set_cstring_type);
    gen->init_reg_pool = &aarch64InitRegPool;
    gen->paste_data_section = &aarch64PasteDataSection;
    gen->paste_asm_blocks = &aarch64PasteAsmBlocks;
    gen->emit_function = &aarch64MirGenerateFunction;
    gen->emit_global = &aarch64GlobalVar;
    return gen;
}

static AsmGenerator *x86_64CodeGenerator(Cctrl *cc) {
    AsmGenerator *gen = malloc(sizeof(AsmGenerator));
    memset(&gen->ctx, 0, sizeof(gen->ctx));
    gen->ctx.buf = aoStrAlloc(2048);
    gen->ctx.cc = cc;
    gen->seen_globals = setNew(32, &set_cstring_type);
    gen->init_reg_pool = &x86_64InitRegPool;
    gen->paste_data_section = &x86_64PasteDataSection;
    gen->paste_asm_blocks = &x86_64PasteAsmBlocks;
    gen->emit_function = &x86_64MirGenerateFunction;
    gen->emit_global = &x86_64GlobalVar;
    return gen;
}

AoStr *asmGenerate(Cctrl *cc) {
    AsmGenerator *gen = NULL;
    switch (cc->target) {
        case TARGET_AARCH64_APPLE_DARWIN:
        case TARGET_AARCH64_UNKNOWN_LINUX_GNU:
            gen = AArch64CodeGenerator(cc);
            break;
        case TARGET_X86_64_APPLE_DARWIN:
        case TARGET_X86_64_UNKNOWN_LINUX_GNU:
            gen = x86_64CodeGenerator(cc);
            break;
    }
    if (gen == NULL) {
        loggerPanic("asm: unknown target %d\n", (int)cc->target);
    }

    IrCtx *ir_ctx = irCtxNew(cc);
    AoStr *buf = gen->ctx.buf;

    gen->init_reg_pool();
    gen->paste_data_section(cc, buf);
    gen->paste_asm_blocks(cc, buf);

    /* Top-level statements parsed into cc->initalisers become the
     * body of a synthetic `main`. CCTRL_ASM_HAS_INITIALISERS makes
     * asmNormaliseFunctionName rename the user's `Main` to `MainFn`
     * so the symbols don't collide. */
    Ast *synth_main = asmBuildInitialiserMain(cc);

    listForEach(cc->ast_list) {
        Ast *ast = it->value;
        if (ast->kind == AST_FUNC) {
            IrFunction *fn = irLowerFunction(ir_ctx, ast);
            irBasicFunctionOptimisations(fn);
            if (irInlineFunction(ir_ctx, fn)) irBasicFunctionOptimisations(fn);
            irFunctionPrepForCodeGen(&gen->ctx, fn, ast);
            gen->emit_function(&gen->ctx, ast);
        } else if (ast->kind == AST_DECL || ast->kind == AST_GVAR) {
            gen->emit_global(cc, gen->seen_globals, buf, ast);
        }
    }
    if (synth_main) {
        IrFunction *fn = irLowerFunction(ir_ctx, synth_main);
        irBasicFunctionOptimisations(fn);
        if (irInlineFunction(ir_ctx, fn)) irBasicFunctionOptimisations(fn);
        irFunctionPrepForCodeGen(&gen->ctx, fn, synth_main);
        gen->emit_function(&gen->ctx, synth_main);
    }

    asmEmitAsmInfo(cc, buf);
    setRelease(gen->seen_globals);
    free(gen);
    return buf;
}

/* ---- AOT byte emission for `asm {}` chunks ---- */

/* Order fixups by patch offset so the byte walk below can splice
 * relocation expressions in stream order. */
static int asmFixupOffsetCmp(const void *a, const void *b) {
    const AsmFixup *fa = a, *fb = b;
    if (fa->patch_offset < fb->patch_offset) return -1;
    if (fa->patch_offset > fb->patch_offset) return 1;
    return 0;
}

/* An `asm { }` block is written in HolyC's own dialect, which the
 * system assembler does not accept - so it goes out as its encoded
 * bytes. That leaves `-S` showing `.byte 224, 0, 128, 210` where the
 * reader wanted an instruction.
 *
 * We have a disassembler, so say what the bytes ARE. Not what was
 * written: the point of reading `-S` is to check what the encoder
 * produced, and a comment echoing the source would agree with it even
 * when the encoding is wrong. */
static void asmEmitByteRunDisasm(Cctrl *cc, AoStr *buf,
                                 const uint8_t *bytes,
                                 size_t from, size_t to)
{
    if (prsAsmTasmArch(cc) != TASM_ARCH_ARM64) return;
    char text[128];
    for (size_t i = from; i + 4 <= to; i += 4) {
        uint32_t insn = (uint32_t)bytes[i] | ((uint32_t)bytes[i+1] << 8) |
                        ((uint32_t)bytes[i+2] << 16) |
                        ((uint32_t)bytes[i+3] << 24);
        if (aarch64_disasm(insn, text, sizeof(text))) {
            aoStrCatFmt(buf, "\t// %s\n", text);
        }
    }
}

static void asmEmitByteRun(AoStr *buf, const uint8_t *bytes,
                           size_t from, size_t to)
{
    for (size_t i = from; i < to; ++i) {
        if (((i - from) & 15) == 0) {
            if (i != from) aoStrPutChar(buf, '\n');
            aoStrCatLen(buf, "\t.byte ", 7);
        } else {
            aoStrCatLen(buf, ", ", 2);
        }
        aoStrCatFmt(buf, "%I", (s64)bytes[i]);
    }
    if (to > from) aoStrPutChar(buf, '\n');
}

int asmEmitBlockBytes(Cctrl *cc, AoStr *buf, AoStr *text, int src_line) {
    if (!text || text->len == 0) return 0;
    const char *src_file = "<asm>";
    AsmBlock *blk = asm_parse_with_handler(text->data, (int)text->len,
                                           src_file, src_line,
                                           prsAsmTasmArch(cc),
                                           cc,
                                           prsAsmDiagSink);
    if (blk->errors) {
        asm_block_free(blk);
        return -1;
    }
    AsmEnc enc;
    asm_enc_init(&enc);
    asm_enc_set_error_handler(&enc, cc, prsAsmDiagSink);
    asm_encode(blk, NULL, &enc);
    asm_block_free(blk);
    if (enc.errors) {
        asm_enc_free(&enc);
        return -1;
    }

    /* Local-label fixups were resolved by asm_encode; what remains are
     * symbol references. Sort by offset and splice each back into the
     * byte stream as a relocation expression. */
    if (enc.n_fixups > 1) {
        qsort(enc.fixups, (size_t)enc.n_fixups, sizeof(AsmFixup),
              asmFixupOffsetCmp);
    }
    int rc = 0;
    size_t pos = 0;
    for (int i = 0; i < enc.n_fixups && rc == 0; ++i) {
        AsmFixup *f = &enc.fixups[i];
        if (f->kind != AF_SYMBOL) continue;
        /* C symbols get the platform prefix; internal `_NAME` asm
         * labels already carry their underscore so this is a no-op
         * for them on Darwin and everywhere on Linux. */
        AoStr *raw = aoStrDupRaw(f->sym, strlen(f->sym));
        char *sym = asmNormaliseFunctionName(cc, raw);
        /* Branch fixups go back out as textual instructions: Mach-O
         * has no subtraction reloc for undefined symbols, and
         * `call sym` assembles to the exact bytes libtasm produced
         * (opcode + rel32) with a proper branch relocation. The
         * opcode byte(s) sit just before the displacement. */
        static const char *const kCcNames[16] = {
            "o","no","b","ae","e","ne","be","a",
            "s","ns","p","np","l","ge","le","g"
        };
        switch (f->reloc) {
            case AFR_X86_64_CALL32:
            case AFR_X86_64_JMP32: {
                if (f->patch_offset < 1) { rc = -1; break; }
                asmEmitByteRun(buf, enc.bytes, pos, f->patch_offset - 1);
                int is_call = (f->reloc == AFR_X86_64_CALL32);
                aoStrCatFmt(buf, "\t%s %s\n", is_call ? "call" : "jmp", sym);
                pos = f->patch_offset + 4;
                break;
            }
            case AFR_X86_64_JCC32: {
                if (f->patch_offset < 2) { rc = -1; break; }
                asmEmitByteRun(buf, enc.bytes, pos, f->patch_offset - 2);
                int cc4 = enc.bytes[f->patch_offset - 1] & 0x0F;
                aoStrCatFmt(buf, "\tj%s %s\n", kCcNames[cc4], sym);
                pos = f->patch_offset + 4;
                break;
            }
            case AFR_X86_64_SIGNED:
                /* RIP-relative data ref; subtraction works for symbols
                 * defined in this translation unit. */
                asmEmitByteRun(buf, enc.bytes, pos, f->patch_offset);
                aoStrCatFmt(buf, "\t.long %s - . - 4\n", sym);
                pos = f->patch_offset + 4;
                break;
            case AFR_AARCH64_CALL26:
            case AFR_AARCH64_JUMP26: {
                /* aarch64 branch to an external symbol: the relocation
                 * lands inside the 4-byte instruction word, which a
                 * .byte run can't carry. Re-emit the branch as a textual
                 * instruction so the system assembler produces a proper
                 * ARM64_RELOC_BRANCH26 against `sym`. */
                asmEmitByteRun(buf, enc.bytes, pos, f->patch_offset);
                int is_bl = (f->reloc == AFR_AARCH64_CALL26);
                aoStrCatFmt(buf, "\t%s %s\n", is_bl ? "bl" : "b", sym);
                pos = f->patch_offset + 4;
                break;
            }
            default:
                if (!f->pcrel && f->width == 8) {
                    asmEmitByteRun(buf, enc.bytes, pos, f->patch_offset);
                    aoStrCatFmt(buf, "\t.quad %s\n", sym);
                    pos = f->patch_offset + 8;
                    break;
                }
                cctrlDiagPush(cc, cctrlMakeDiag(cc, CCTRL_ERROR,
                        aoStrPrintf("%s:%d: asm: reference to `%s` cannot be "
                                    "expressed on the AOT path for this "
                                    "target (reloc %d); use -jit",
                                    src_file, src_line, f->sym,
                                    (int)f->reloc),
                        NULL));
                rc = -1;
                break;
        }
    }
    if (rc == 0) {
        asmEmitByteRun(buf, enc.bytes, pos, enc.len);
        asmEmitByteRunDisasm(cc, buf, enc.bytes, pos, enc.len);
    }
    asm_enc_free(&enc);
    return rc;
}

void asmEmitAsmInfo(Cctrl *cc, AoStr *buf) {
    aoStrCatFmt(buf,
#if IS_LINUX
               ".section    .note.GNU-stack,\"\",@progbits\n"
#endif
               ".ident      \"hcc: %s %s %s hash: %s\"\n",
                OS_STR,
                cliTargetToString(cc->target),
                cctrlGetVersion(),
                HCC_GIT_HASH);
}

