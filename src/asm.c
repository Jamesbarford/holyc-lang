#include <strings.h>
#include <math.h>
#include <string.h>

#include "aarch64.h"
#include "asm.h"
#include "asm/asm.h"
#include "asm/asm_enc.h"
#include "cctrl.h"
#include "cli.h"
#include "config.h"
#include "prsasm.h"
#include "x86.h"
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

/* Mach-O treats any symbol beginning with an uppercase `L` as a local
 * (assembler-temporary) label, so `.globl LIGHTGRAY` is rejected with
 * "non-local symbol required". Data globals are otherwise emitted with their
 * raw name (the JIT keys its arena on that name and never goes through the
 * assembler, so it is unaffected). On macOS, escape the colliding names with
 * a leading `_` - matching the function-name convention; the SAME transform
 * must be applied at the symbol's definition and at every reference so they
 * agree. Returns the original AoStr when no change is needed. */
AoStr *asmNormaliseGlobalLabel(Cctrl *cc, AoStr *name) {
    int is_macos = cc->target == TARGET_AARCH64_APPLE_DARWIN ||
                   cc->target == TARGET_X86_64_APPLE_DARWIN;
    if (is_macos && name && name->len > 0 && name->data[0] == 'L') {
        AoStr *out = aoStrNew();
        aoStrPutChar(out, '_');
        aoStrCatAoStr(out, name);
        return out;
    }
    return name;
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

/* Build the synthetic `main` that runs file-scope initialiser
 * statements (cc->initalisers) as the process entry point, shared by
 * every backend (aarch64 / x86_64 AOT and the JIT). Returns NULL when
 * there are no initialisers - in that case the user's `Main` becomes
 * the entry directly (asmNormaliseFunctionName lowercases it).
 *
 * When initialisers DO exist the user's `Main` is renamed to `MainFn`
 * and this driver becomes `_main`, so the driver must call `Main`
 * itself - otherwise the program runs the initialisers and exits
 * without ever entering Main (the auto-entry convenience would be
 * silently lost the moment a file-scope initialiser appears). */
Ast *asmBuildInitialiserMain(Cctrl *cc) {
    if (listEmpty(cc->initalisers)) return NULL;
    cc->flags |= CCTRL_ASM_HAS_INITIALISERS;

    /* HolyC "scripting" files drive execution themselves with a
     * trailing `Main;` at file scope. If the initialisers already call
     * Main, don't auto-call it again (that would run Main twice). The
     * auto-call only restores the convenience for the "globals +
     * Main, no explicit call" case. */
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
    return astFunction(fn_type, str_lit("main"), empty_params, body,
                       cc->initaliser_locals, 0);
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

/* ---- AOT byte emission for `asm {}` chunks ---- */

/* Order fixups by patch offset so the byte walk below can splice
 * relocation expressions in stream order. */
static int asmFixupOffsetCmp(const void *a, const void *b) {
    const AsmFixup *fa = a, *fb = b;
    if (fa->patch_offset < fb->patch_offset) return -1;
    if (fa->patch_offset > fb->patch_offset) return 1;
    return 0;
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
    if (rc == 0) asmEmitByteRun(buf, enc.bytes, pos, enc.len);
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
