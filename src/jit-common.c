/* Arch-independent half of the in-process JIT. */

#include <ctype.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "asm.h"
#include "cli.h"
#include "prsasm.h"
#include "ir-optimise.h"
#include "jit-common.h"
#include "list.h"
#include "util.h"

/* Internal labels (cross-function calls within this TU) are matched
 * inside asm_jit_finalize via enc->labels by name -- the resolver only
 * sees external symbols. Host overrides win over libc. */
static void *jitResolveSymbol(void *ud, const char *sym) {
    HccJit *jit = ud;
    void *p;
    if ((p = mapGet(jit->host_symbols, (void *)sym))) return p;
    return asm_jit_dlsym_resolver(NULL, sym);
}

int hccJitBlockLocalNum(HccJit *jit, IrFunction *fn, IrBlock *block) {
    uint64_t key = ((uint64_t)fn->uuid << 32) | (uint64_t)block->id;
    void *v = mapGetInt(jit->block_local, key);
    if (v) return (int)(intptr_t)v;
    int n = ++jit->next_local;
    mapAddIntOrErr(jit->block_local, key, (void *)(intptr_t)n);
    return n;
}

int hccJitEpilogueLocalNum(HccJit *jit, IrFunction *fn) {
    void *v = mapGetInt(jit->epi_local, (uint64_t)fn->uuid);
    if (v) return (int)(intptr_t)v;
    int n = ++jit->next_local;
    mapAddIntOrErr(jit->epi_local, (uint64_t)fn->uuid, (void *)(intptr_t)n);
    return n;
}

int hccJitFreshLocalNum(HccJit *jit) {
    return ++jit->next_local;
}

int hccJitIsInternalFunc(HccJit *jit, const char *sym) {
    /* Stored as cstring -> any-nonnull-pointer in `symbols`. */
    return mapGet(jit->symbols, (void *)sym) != NULL;
}

/* ---------------- fixup helpers ---------------- */

void hccJitAddCallFixup(AsmEnc *enc, size_t patch_off, const char *sym,
                        AsmFixupReloc reloc)
{
    AsmFixup f = {
        .kind = AF_SYMBOL,
        .reloc = reloc,
        .patch_offset = patch_off,
        .width = 4,
        .pcrel = 1,
        .sym = strdup(sym),
    };
    asm_add_fixup(enc, f);
}

void hccJitAddLocalBranchFixup(AsmEnc *enc, size_t patch_off, int local_num,
                               AsmFixupReloc reloc)
{
    AsmFixup f = {
        .kind = AF_LOCAL,
        .reloc = reloc,
        .patch_offset = patch_off,
        .width = 4,
        .pcrel = 1,
        .local_num = local_num,
    };
    asm_add_fixup(enc, f);
}

#define JIT_MAX_PHIS 32
void hccJitPhiMaterialise(IrBlock *from, IrBlock *to,
                          HccJitEmitPhiFn emit, void *ud)
{
    if (!to || !from) return;
    IrInstr *phis[JIT_MAX_PHIS];
    IrPair  *pairs[JIT_MAX_PHIS];
    int     done[JIT_MAX_PHIS];
    int n = 0;

    listForEach(to->instructions) {
        IrInstr *I = (IrInstr *)it->value;
        if (I->op == IR_NOP) continue;
        if (I->op != IR_PHI) break;
        if (n >= JIT_MAX_PHIS) loggerPanic("jit: too many phis\n");
        IrPair *match = NULL;
        if (I->extra.phi_pairs) {
            for (u64 i = 0; i < I->extra.phi_pairs->size; ++i) {
                IrPair *p = vecGet(IrPair *, I->extra.phi_pairs, i);
                if (p->ir_block == from) { match = p; break; }
            }
        }
        if (!match || !match->ir_value) continue;
        phis[n] = I; pairs[n] = match; done[n] = 0; n++;
    }
    if (n == 0) return;

    /* Order so that no pending phi reads a value an earlier emit has
     * already overwritten; a cycle would need a temp and can't occur
     * with the IR builder's current output. */
    int emitted = 0;
    while (emitted < n) {
        int progress = 0;
        for (int i = 0; i < n; ++i) {
            if (done[i]) continue;
            int read_by_pending = 0;
            for (int j = 0; j < n; ++j) {
                if (i == j || done[j]) continue;
                IrValue *v = pairs[j]->ir_value;
                if (irIsTmp(v) && irIsTmp(phis[i]->dst) &&
                    irVarId(v) == irDstVarId(phis[i])) {
                    read_by_pending = 1; break;
                }
            }
            if (!read_by_pending) {
                emit(ud, phis[i], pairs[i]);
                done[i] = 1; emitted++; progress = 1;
            }
        }
        if (!progress)
            loggerPanic("jit: phi cycle at block %u\n", to->id);
    }
}

/* ---------------- asm {} blocks ---------------- */

int hccJitAssembleText(HccJit *jit, AoStr *text, int src_line) {
    if (!text || text->len == 0) return 0;
    Cctrl *cc = jit->cc;
    AsmBlock *blk = asm_parse_with_handler(text->data, (int)text->len,
                                           "<asm>", src_line,
                                           prsAsmTasmArch(cc),
                                           cc, prsAsmDiagSink);
    if (blk->errors) {
        asm_block_free(blk);
        return -1;
    }
    /* Encode into a private AsmEnc: libtasm resolves `@@N` local-label
     * fixups against the labels of THIS chunk only, which both scopes
     * them per function (TempleOS semantics) and keeps their numbers
     * from colliding with the jit's own anonymous local labels. */
    AsmEnc sub;
    asm_enc_init(&sub);
    asm_enc_set_error_handler(&sub, cc, prsAsmDiagSink);
    asm_encode(blk, NULL, &sub);
    asm_block_free(blk);
    if (sub.errors) {
        asm_enc_free(&sub);
        return -1;
    }

    /* Splice: bytes are appended at the current tail; named labels and
     * the remaining AF_SYMBOL fixups rebase onto it. */
    size_t base = jit->enc.len;
    for (int i = 0; i < sub.n_labels; ++i) {
        if (!sub.labels[i].name) continue; /* locals are resolved+dead */
        size_t save = jit->enc.len;
        jit->enc.len = base + sub.labels[i].byte_offset;
        asm_define_label(&jit->enc, -1, sub.labels[i].name);
        jit->enc.len = save;
    }
    for (size_t i = 0; i < sub.len; ++i) put_byte(&jit->enc, sub.bytes[i]);
    for (int i = 0; i < sub.n_fixups; ++i) {
        AsmFixup f = sub.fixups[i];
        f.patch_offset += base;
        if (f.sym) f.sym = strdup(f.sym);
        asm_add_fixup(&jit->enc, f);
    }
    asm_enc_free(&sub);
    return 0;
}

/* ---------------- globals layout ---------------- */

/* Estimate the size needed for all globals + string literals. */
static size_t jitGlobalsSize(Cctrl *cc) {
    size_t total = 0;
    listForEach(cc->ast_list) {
        Ast *ast = it->value;
        if (ast->kind != AST_DECL && ast->kind != AST_GVAR) continue;
        Ast *dv = ast->declvar;
        if (!dv || !dv->type) continue;
        if (ast->flags & AST_FLAG_EXTERN) continue;
        if (dv->flags & AST_FLAG_EXTERN) continue;
        total += (dv->type->size + 7) & ~7;  /* 8-byte align */
    }
    /* String literals are stored in cc->strs. */
    if (cc->strs) {
        MapIter mi; mapIterInit(cc->strs, &mi);
        while (mapIterNext(&mi)) {
            Ast *ast = (Ast *)mi.node->value;
            if (ast->kind != AST_STRING) continue;
            total += ((size_t)ast->sval->len + 1 + 7) & ~7;
        }
    }
    return total + 64; /* slack */
}

/* Walk an initialiser AST and write its byte image to `addr`.
 * Returns the number of bytes written. Mirrors the AOT data emitters:
 *   AST_LITERAL  - scalar, possibly float
 *   AST_STRING   - 8-byte pointer to the string's slot in the arena
 *                  (looked up via host_symbols, populated in pass 1)
 *   AST_ARRAY_INIT - recursively writes each element back-to-back */
static size_t jitWriteInit(HccJit *jit, uint8_t *addr, Ast *init) {
    if (!init) return 0;
    if (init->kind == AST_LITERAL) {
        size_t w = init->type ? (size_t)init->type->size : 8;
        if (w > 8) w = 8;
        if (init->type && init->type->kind == AST_TYPE_FLOAT) {
            /* F32 must use the 32-bit IEEE encoding, not the low half of the
             * 64-bit pattern (which would be garbage / zero). */
            if (w == 4) {
                uint32_t bits = ieee754_32((f32)init->f64);
                memcpy(addr, &bits, 4);
            } else {
                uint64_t bits = ieee754_64(init->f64);
                memcpy(addr, &bits, 8);
            }
        } else {
            int64_t v = init->i64;
            memcpy(addr, &v, w);
        }
        return w;
    }
    if (init->kind == AST_STRING) {
        void *p = init->slabel ? mapGet(jit->host_symbols,
                                        (void *)init->slabel->data) : NULL;
        uintptr_t v = (uintptr_t)p;
        memcpy(addr, &v, 8);
        return 8;
    }
    if (init->kind == AST_ARRAY_INIT && init->arrayinit) {
        size_t total = 0;
        /* arrayinit is a circular sentinel list; iterate it manually. */
        List *head = init->arrayinit;
        for (List *n = head->next; n && n != head; n = n->next) {
            Ast *el = (Ast *)n->value;
            total += jitWriteInit(jit, addr + total, el);
        }
        return total;
    }
    return 0;
}

/* Lay out strings + globals into the arena; register each by its
 * mangled name so the backends' global-address emitters can find them
 * via host_symbols. Strings come first so that AST_ARRAY_INIT
 * initialisers in globals can reference them by their already-known
 * addresses. */
static int jitAllocateGlobals(HccJit *jit) {
    size_t sz = jitGlobalsSize(jit->cc);
    if (sz == 0) return 0;
    jit->globals = calloc(1, sz);
    jit->globals_size = sz;
    size_t off = 0;

    if (jit->cc->strs) {
        MapIter mi; mapIterInit(jit->cc->strs, &mi);
        while (mapIterNext(&mi)) {
            Ast *ast = (Ast *)mi.node->value;
            if (ast->kind != AST_STRING) continue;
            char *addr = (char *)(jit->globals + off);
            /* The lexer stores escape sequences in their textual form
             * (`\n` is two bytes `\\` `n`). The asm path lets the
             * assembler decode them; we have to do it ourselves here
             * so JIT-emitted code sees the same byte image. */
            const char *s = ast->sval->data;
            int len = ast->sval->len;
            int o = 0;
            for (int i = 0; i < len; ++i) {
                char c = s[i];
                if (c != '\\' || i + 1 >= len) {
                    addr[o++] = c;
                    continue;
                }
                char n = s[++i];
                if (n >= '0' && n <= '7') {
                    int v = n - '0';
                    int d = 1;
                    while (d < 3 && i + 1 < len &&
                           s[i+1] >= '0' && s[i+1] <= '7') {
                        v = v * 8 + (s[++i] - '0');
                        d++;
                    }
                    addr[o++] = (char)v;
                    continue;
                }
                switch (n) {
                    case 'n': addr[o++] = '\n'; break;
                    case 't': addr[o++] = '\t'; break;
                    case 'r': addr[o++] = '\r'; break;
                    case 'b': addr[o++] = '\b'; break;
                    case 'f': addr[o++] = '\f'; break;
                    case 'v': addr[o++] = '\v'; break;
                    case 'a': addr[o++] = '\a'; break;
                    case 'e': addr[o++] = 0x1B; break;
                    case '\\': addr[o++] = '\\'; break;
                    case '"':  addr[o++] = '"';  break;
                    case '\'': addr[o++] = '\''; break;
                    case '?': addr[o++] = '?'; break;
                    case 'x': case 'X': {
                        int v = 0, d = 0;
                        while (d < 2 && i + 1 < len &&
                               isxdigit((unsigned char)s[i+1])) {
                            char h = s[++i];
                            int hv = (h >= '0' && h <= '9') ? h - '0' :
                                     (h | 32) - 'a' + 10;
                            v = v * 16 + hv;
                            d++;
                        }
                        addr[o++] = (char)v;
                        break;
                    }
                    default: addr[o++] = n; break;
                }
            }
            addr[o] = '\0';
            mapAdd(jit->host_symbols, strdup(ast->slabel->data), addr);
            /* Allocate based on the *decoded* length plus NUL, 8-aligned. */
            size_t decoded_size = (size_t)(o + 1);
            off += (decoded_size + 7) & ~7;
        }
    }

    /* Globals. Strings are now addressable, so AST_ARRAY_INIT
     * containing AST_STRING elements can resolve element addresses. */
    listForEach(jit->cc->ast_list) {
        Ast *ast = it->value;
        if (ast->kind != AST_DECL && ast->kind != AST_GVAR) continue;
        Ast *dv = ast->declvar;
        if (!dv || !dv->type) continue;
        if (ast->flags & AST_FLAG_EXTERN) continue;
        if (dv->flags & AST_FLAG_EXTERN) continue;
        AoStr *label = dv->is_static ? dv->glabel : dv->gname;
        if (!label) continue;
        uint8_t *addr = jit->globals + off;
        mapAdd(jit->host_symbols, strdup(label->data), addr);
        jitWriteInit(jit, addr, ast->declinit);
        off += (dv->type->size + 7) & ~7;
    }
    return 0;
}

/* Load libtos so HolyC stdlib symbols (MAlloc, Free, printf, ...) are
 * discoverable by dlsym. The AOT path links them in at the linker step;
 * the JIT has to make them visible at runtime. The configured install
 * prefix (cc->install_dir, `--install-dir`, defaults to /usr/local) is
 * probed first with both platforms' install conventions: macOS installs
 * lib<name>.0.0.1.dylib + a lib<name>.dylib symlink, Linux produces
 * <name>.so / <name>.so.0.0.1. */
static void jitLoadLibtos(Cctrl *cc) {
    static int tos_loaded = 0;
    if (tos_loaded) return;
    tos_loaded = 1;

    if (cc->install_dir && *cc->install_dir) {
        static const char *const suffixes[] = {
            "lib/libtos.dylib",
            "lib/libtos.0.0.1.dylib",
            "lib/libtos.so",
            "lib/libtos.so.0.0.1",
        };
        char path[1024];
        for (size_t i = 0; i < sizeof(suffixes)/sizeof(suffixes[0]); ++i) {
            snprintf(path, sizeof(path), "%s/%s", cc->install_dir, suffixes[i]);
            if (dlopen(path, RTLD_LAZY | RTLD_GLOBAL)) return;
        }
    }
    /* Fall back to the dynamic-linker search paths and the in-tree
     * `hcc -lib tos` build artifact. */
    static const char *const candidates[] = {
        "libtos.dylib",
        "libtos.so",
        "libtos.so.0.0.1",
        "./tos.so",
        "./tos.dylib",
    };
    for (size_t i = 0; i < sizeof(candidates)/sizeof(candidates[0]); ++i) {
        if (dlopen(candidates[i], RTLD_LAZY | RTLD_GLOBAL)) return;
    }
    /* Non-fatal if none load: programs that don't touch the HolyC
     * stdlib will still work. The unresolved-symbol error fires later
     * with a precise name if they do. */
}

static void jitLoadSharedObjects(Cctrl *cc) {
    static int libs_loaded = 0;
    if (libs_loaded) return;
    libs_loaded = 1;

    fprintf(stderr, "JIT cannot load in object files at this time\n");
    
    // if (cc->object_files) {
    //     fprintf(stderr, "Ignoring object files, only shared objects can be "
    //             "loaded by the JIT\n");
    // }

   // if (!listEmpty(cc->shared_object_files)) {
   //     listForEach(cc->shared_object_files) {
   //         AoStr *so_file = it->value;
   //         dlopen(so_file->data, RTLD_LAZY | RTLD_GLOBAL);
   //     }
   // }
}

/* ---------------- compile pipeline ---------------- */

HccJit *hccJitCompile(Cctrl *cc, const HccJitBackend *backend) {
    if (!backend->target_ok(cc->target)) {
        fprintf(stderr, "hcc: JIT only supports %s targets\n", backend->name);
        return NULL;
    }

    HccJit *jit = calloc(1, sizeof *jit);
    jit->cc           = cc;
    jit->symbols      = mapNew(64, &map_cstring_opaque_type);
    jit->host_symbols = mapNew(64, &map_cstring_opaque_type);
    jit->block_local  = mapNew(64, &map_uint_to_uint_type);
    jit->epi_local    = mapNew(16, &map_uint_to_uint_type);
    jit->next_local   = 0;

    asm_enc_init(&jit->enc);
    backend->init_reg_pool();
    jitLoadLibtos(cc);
    jitLoadSharedObjects(cc);

    if (jitAllocateGlobals(jit) != 0) goto fail;

    /* HolyC "scripting" mode: statements at file scope land in
     * cc->initalisers and get wrapped into a synthetic `main`. When
     * this happens the user-supplied Main is renamed to MainFn (see
     * asmNormaliseFunctionName). We have to apply the same wrapping
     * before compiling so the entry point we hand to `hccJitRunMain`
     * actually runs the init statements. */
    Ast *synth_main = asmBuildInitialiserMain(cc);

    /* Pre-register every internal function name so emit-time code
     * (IR_LEA on a function, IR_CALL to another function in this TU)
     * can route through label fixups rather than dlsym (which doesn't
     * see this-TU JIT code). The marker value is overwritten with the
     * real address post-finalize. */
    listForEach(cc->ast_list) {
        Ast *ast = it->value;
        if (ast->kind != AST_FUNC) continue;
        char *mangled = asmNormaliseFunctionName(cc, ast->fname);
        if (!mapHas(jit->symbols, mangled)) {
            mapAdd(jit->symbols, strdup(mangled), (void *)(uintptr_t)1);
        }
    }
    if (synth_main) {
        char *mangled = asmNormaliseFunctionName(cc, synth_main->fname);
        if (!mapHas(jit->symbols, mangled)) {
            mapAdd(jit->symbols, strdup(mangled), (void *)(uintptr_t)1);
        }
    }

    /* `asm {}` functions are internal too: register both the raw label
     * name and its platform-mangled form so calls and IR_LEA route
     * through label fixups (the finalize matcher is underscore-
     * tolerant for the raw/mangled difference). */
    if (cc->asm_blocks) {
        listForEach(cc->asm_blocks) {
            Ast *asm_block = (Ast *)it->value;
            if (!asm_block->funcs) continue;
            for (List *fl = asm_block->funcs->next; fl != asm_block->funcs;
                 fl = fl->next)
            {
                Ast *fn = (Ast *)fl->value;
                char *mangled = asmNormaliseFunctionName(cc, fn->asmfname);
                if (!mapHas(jit->symbols, fn->asmfname->data)) {
                    mapAdd(jit->symbols, strdup(fn->asmfname->data),
                           (void *)(uintptr_t)1);
                }
                if (!mapHas(jit->symbols, mangled)) {
                    mapAdd(jit->symbols, strdup(mangled),
                           (void *)(uintptr_t)1);
                }
            }
        }
    }

    IrCtx *ir_ctx = irCtxNew(cc);
    listForEach(cc->ast_list) {
        Ast *ast = it->value;
        if (ast->kind != AST_FUNC) continue;
        if (backend->compile_function(jit, ast, ir_ctx) != 0) goto fail;
    }
    if (synth_main) {
        if (backend->compile_function(jit, synth_main, ir_ctx) != 0) goto fail;
    }

    /* `asm {}` function bodies: define each public label, then splice
     * the libtasm-encoded bytes in. */
    if (cc->asm_blocks) {
        listForEach(cc->asm_blocks) {
            Ast *asm_block = (Ast *)it->value;
            if (!asm_block->funcs) continue;
            for (List *fl = asm_block->funcs->next; fl != asm_block->funcs;
                 fl = fl->next)
            {
                Ast *fn = (Ast *)fl->value;
                asm_define_label(&jit->enc, -1, fn->asmfname->data);
                if (hccJitAssembleText(jit, fn->body->asm_stmt, 0) != 0)
                    goto fail;
            }
        }
    }

    /* Dev hex dump of pre-finalize bytes. Triggered by HCC_JIT_DUMP=1. */
    if (getenv("HCC_JIT_DUMP")) {
        fprintf(stderr, "=== JIT encoded bytes (len=%zu) ===\n", jit->enc.len);
        for (size_t i = 0; i < jit->enc.len; ++i) {
            fprintf(stderr, "%02x%s", jit->enc.bytes[i],
                    (i & 15) == 15 ? "\n" : " ");
        }
        fputc('\n', stderr);
        fprintf(stderr, "=== labels (%d) ===\n", jit->enc.n_labels);
        for (int i = 0; i < jit->enc.n_labels; ++i) {
            AsmLabelDef *L = &jit->enc.labels[i];
            fprintf(stderr, "  %04zx %s%d %s\n", L->byte_offset,
                    L->name ? "" : "L", L->local_num,
                    L->name ? L->name : "");
        }
    }

    if (asm_jit_finalize(&jit->enc, jitResolveSymbol, jit, &jit->code) != 0) {
        goto fail;
    }

    /* Each public label now points at (code base + label byte_offset). */
    for (int i = 0; i < jit->enc.n_labels; ++i) {
        AsmLabelDef *L = &jit->enc.labels[i];
        if (!L->name) continue;
        void *addr = (uint8_t *)jit->code.code + L->byte_offset;
        mapAdd(jit->symbols, strdup(L->name), addr);
    }
    return jit;

fail:
    hccJitFree(jit);
    return NULL;
}

/* ---------------- public accessors ---------------- */

void *hccJitLookup(HccJit *jit, const char *name) {
    if (!jit || !name) return NULL;
    void *p = mapGet(jit->symbols, (void *)name);
    if (p) return p;
    /* Try the underscore-prefixed mangled form too (macOS convention). */
    if (name[0] != '_') {
        char buf[256];
        snprintf(buf, sizeof buf, "_%s", name);
        return mapGet(jit->symbols, (void *)buf);
    }
    return NULL;
}

void hccJitDefineSymbol(HccJit *jit, const char *name, void *addr) {
    if (!jit || !name) return;
    mapAdd(jit->host_symbols, strdup(name), addr);
}

union JitMainFunction {
    void *obj;
    int (*main_fn)(int, char **);
};

int hccJitRunMain(HccJit *jit, int argc, char **argv) {
    union JitMainFunction entry;
    entry.obj = hccJitLookup(jit, "main");
    if (!entry.obj) entry.obj = hccJitLookup(jit, "_main");
    return entry.obj ? entry.main_fn(argc, argv) : -1;
}

void hccJitFree(HccJit *jit) {
    if (!jit) return;
    asm_enc_free(&jit->enc);
    asm_jit_free(&jit->code);
    if (jit->globals) free(jit->globals);
    if (jit->symbols) mapRelease(jit->symbols);
    if (jit->host_symbols) mapRelease(jit->host_symbols);
    if (jit->block_local) mapRelease(jit->block_local);
    if (jit->epi_local) mapRelease(jit->epi_local);
    free(jit);
}
