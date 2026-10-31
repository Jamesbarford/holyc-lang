/* Arch-independent half of the in-process JIT. */

#include <ctype.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "asm.h"
#include "cli.h"
#include "config.h"
#include "prsasm.h"
#include "ir-optimise.h"
#include "jit-common.h"
#include "list.h"
#include "memsafe.h"
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
    /* "Internal" means defined in the chunk currently being emitted -
     * those calls route through label fixups. Functions from earlier
     * chunks resolve through host_symbols like any other host address. */
    return mapGet(jit->chunk_fns, (void *)sym) != NULL;
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
        if (f.sym) {
            /* An `asm {}` body names C functions by their raw spelling
             * (`CALL malloc`). The object writer hands the name to the
             * linker as-is, so it has to carry the platform prefix
             * (`_malloc` on Mach-O) - exactly what the text path did
             * before the assembler saw it. Raw assembly labels are
             * already final and come back unchanged. The JIT's own
             * resolver is underscore-tolerant, so ABSOLUTE mode keeps
             * the raw name. */
            if (jit->addr_mode == HCC_ADDR_RELOCATED) {
                AoStr *raw = aoStrDupRaw(f.sym, strlen(f.sym));
                f.sym = strdup(asmNormaliseFunctionName(cc, raw));
                aoStrRelease(raw);
            } else {
                f.sym = strdup(f.sym);
            }
        }
        asm_add_fixup(&jit->enc, f);
    }
    asm_enc_free(&sub);
    return 0;
}

/* ---------------- globals layout ---------------- */

/* The mangled arena label of a global decl, or NULL if it doesn't get
 * a slot (extern, typeless, ...). */
static AoStr *jitGlobalLabel(Ast *ast) {
    if (ast->kind != AST_DECL && ast->kind != AST_GVAR) return NULL;
    Ast *dv = ast->declvar;
    if (!dv || !dv->type) return NULL;
    if (ast->flags & AST_FLAG_EXTERN) return NULL;
    if (dv->flags & AST_FLAG_EXTERN) return NULL;
    return dv->is_static ? dv->glabel : dv->gname;
}

/* Estimate the size needed for the not-yet-allocated globals in
 * (from, sentinel] + string literals. Entries already registered in
 * host_symbols (allocated by a previous chunk) are skipped. */
static size_t jitGlobalsSize(HccJit *jit, List *from) {
    Cctrl *cc = jit->cc;
    size_t total = 0;
    for (List *it = from; it != cc->ast_list; it = it->next) {
        Ast *ast = it->value;
        AoStr *label = jitGlobalLabel(ast);
        if (!label) continue;
        if (mapGet(jit->host_symbols, (void *)label->data)) continue;
        total += (ast->declvar->type->size + 7) & ~7;  /* 8-byte align */
    }
    /* String literals are stored in cc->strs (accumulates across
     * chunks; the host_symbols check skips already-laid-out ones). */
    if (cc->strs) {
        MapIter mi; mapIterInit(cc->strs, &mi);
        while (mapIterNext(&mi)) {
            Ast *ast = (Ast *)mi.node->value;
            if (ast->kind != AST_STRING) continue;
            if (mapGet(jit->host_symbols, (void *)ast->slabel->data)) continue;
            total += ((size_t)ast->sval->len + 1 + 7) & ~7;
        }
    }
    return total;
}

/* Walk an initialiser AST and write its byte image to `addr`.
 * Returns the number of bytes written. Mirrors the AOT data emitters:
 *   AST_LITERAL  - scalar, possibly float
 *   AST_STRING   - 8-byte pointer to the string's slot in the arena
 *                  (looked up via host_symbols, populated in pass 1)
 *   AST_ARRAY_INIT - recursively writes each element back-to-back */
static size_t jitWriteInit(HccJit *jit, uint8_t *addr, Ast *init,
                           AstType *dst) {
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
    /* `&Fn` or a lambda in a global's initialiser. Unlike a string, the
     * address does not exist yet - the function has not been compiled -
     * so leave a hole and record a fixup. Returning 8 keeps the layout
     * right; falling through to `return 0` below wrote nothing AND
     * shifted every following field. */
    {
        /* `&Fn`, `Fn`, `&g`, or any of them behind a CAST. */
        Ast *fn = astInitAddressTarget(init);
        if (!fn) fn = init;
        /* `&g` - the address of another GLOBAL. Same treatment as a
         * function: the address does not exist until the linker (or,
         * in ABSOLUTE mode, until every global has been laid out), so
         * leave a hole and name the label. The deferred pass resolves
         * it either way, which is what makes a FORWARD reference work:
         * `AstType *p = &(AstType){...}` lays the pointer down before
         * the object it points at.
         *
         * Its label, not its name - a static global's symbol is the
         * generated `.L<n>`, and a file-scope compound literal is
         * always static. */
        /* `fn != init` means an address was unwrapped - `&g`. An ARRAY
         * name needs no `&`: it decays to its own address, which is
         * how a table-of-tables is written:
         *
         *     static const TasmMnemonicTable t = { entries, 85 };
         *
         * The text emitters take any AST_GVAR here, so this path was
         * the only one that dropped it - `-S` piped through the
         * assembler was right while `-c` left a null pointer, and the
         * mnemonic lookup walked it. */
        int gvar_is_address = fn->kind == AST_GVAR &&
                              (fn != init ||
                               (fn->type &&
                                fn->type->kind == AST_TYPE_ARRAY));
        if (gvar_is_address) {
            AoStr *g = fn->is_static ? fn->glabel : fn->gname;
            if (g) {
                if (jit->n_ptr_fixups == jit->cap_ptr_fixups) {
                    jit->cap_ptr_fixups = jit->cap_ptr_fixups
                                        ? jit->cap_ptr_fixups * 2 : 16;
                    jit->ptr_fixups = realloc(jit->ptr_fixups,
                            jit->cap_ptr_fixups * sizeof(*jit->ptr_fixups));
                }
                jit->ptr_fixups[jit->n_ptr_fixups].addr = addr;
                jit->ptr_fixups[jit->n_ptr_fixups].symbol =
                    jit->addr_mode == HCC_ADDR_RELOCATED
                    ? strdup(asmNormaliseGlobalLabel(jit->cc, g)->data)
                    : strdup(g->data);
                jit->n_ptr_fixups++;
            }
            uint64_t zero = 0;
            memcpy(addr, &zero, 8);
            return 8;
        }
        if (fn->kind == AST_FUNC || fn->kind == AST_FUN_PROTO ||
            fn->kind == AST_EXTERN_FUNC || fn->kind == AST_ASM_FUNCDEF ||
            fn->kind == AST_ASM_FUNC_BIND)
        {
            AoStr *nm = (fn->kind == AST_ASM_FUNCDEF ||
                         fn->kind == AST_ASM_FUNC_BIND)
                      ? fn->asmfname : fn->fname;
            if (nm) {
                if (jit->n_ptr_fixups == jit->cap_ptr_fixups) {
                    jit->cap_ptr_fixups = jit->cap_ptr_fixups
                                        ? jit->cap_ptr_fixups * 2 : 16;
                    jit->ptr_fixups = realloc(jit->ptr_fixups,
                            jit->cap_ptr_fixups * sizeof(*jit->ptr_fixups));
                }
                jit->ptr_fixups[jit->n_ptr_fixups].addr = addr;
                /* RELOCATED: the object's function labels carry the
                 * platform prefix (`_FpAdd` on Mach-O), so the
                 * relocation must name the same spelling. ABSOLUTE
                 * resolution is underscore-tolerant and keeps the raw
                 * name. */
                jit->ptr_fixups[jit->n_ptr_fixups].symbol =
                    jit->addr_mode == HCC_ADDR_RELOCATED
                    ? strdup(asmNormaliseFunctionName(jit->cc, nm))
                    : strdup(nm->data);
                jit->n_ptr_fixups++;
            }
            uint64_t zero = 0;
            memcpy(addr, &zero, 8);
            return 8;
        }
    }
    if (init->kind == AST_STRING) {
        /* `char buf[] = "hi"` keeps the BYTES inline; `char *p = "hi"`
         * stores the address. Same initialiser, and only the
         * destination says which - so without the type this wrote a
         * pointer into the array and every such global read back
         * empty. The AOT text emitter always distinguished them
         * (aarch64.c/x86_64.c), which is why `-S` piped through the
         * assembler was right while the object writer was not. */
        if (dst && dst->kind == AST_TYPE_ARRAY && init->sval) {
            int n = astDecodeStringLiteral(init->sval->data, (int)init->sval->len,
                                    (char *)addr);
            addr[n] = '\0';
            return (size_t)n + 1;
        }
        /* RELOCATED: the string has no address yet either, so it takes
         * the same treatment as a function pointer above - a hole and a
         * fixup naming the label. */
        if (jit->addr_mode == HCC_ADDR_RELOCATED && init->slabel) {
            if (jit->n_ptr_fixups == jit->cap_ptr_fixups) {
                jit->cap_ptr_fixups = jit->cap_ptr_fixups
                                    ? jit->cap_ptr_fixups * 2 : 16;
                jit->ptr_fixups = realloc(jit->ptr_fixups,
                        jit->cap_ptr_fixups * sizeof(*jit->ptr_fixups));
            }
            jit->ptr_fixups[jit->n_ptr_fixups].addr = addr;
            jit->ptr_fixups[jit->n_ptr_fixups].symbol =
                strdup(init->slabel->data);
            jit->n_ptr_fixups++;
            uint64_t zero = 0;
            memcpy(addr, &zero, 8);
            return 8;
        }
        void *p = init->slabel ? mapGet(jit->host_symbols,
                                        (void *)init->slabel->data) : NULL;
        uintptr_t v = (uintptr_t)p;
        memcpy(addr, &v, 8);
        return 8;
    }
    if (init->kind == AST_ARRAY_INIT && init->arrayinit) {
        size_t total = 0;
        /* In OFFSET order, one entry per offset - astAggregateInitOrder
         * explains why field order is not good enough. */
        u64 n_slots = 0;
        AstInitSlot *slots = astAggregateInitOrder(init->type,
                                                  init->arrayinit, &n_slots);
        for (u64 si = 0; si < n_slots; ++si) {
            Ast *el = slots[si].value;
            int off = slots[si].offset;
            /* Placed at its own offset rather than at a running cursor.
             * This writes into a buffer, so unlike the two text
             * emitters it can go BACKWARDS - which is what a union of
             * differently-sized members needs when a later designator
             * overwrites part of an earlier one. The buffer was zeroed
             * before this, so a gap needs no filling. */
            size_t at = (off >= 0) ? (size_t)off : total;
            size_t n = jitWriteInit(jit, addr + at, el,
                            astAggregateFieldType(init->type,
                                                  slots[si].index));
            if (at + n > total) total = at + n;
        }
        free(slots);
        /* Members the list did not mention are zero, and a NESTED
         * aggregate has to keep its full width or every field after it
         * lands at the wrong offset. Mirrors the AOT emitters. */
        if (init->type && (size_t)init->type->size > total) {
            memset(addr + total, 0, (size_t)init->type->size - total);
            total = (size_t)init->type->size;
        }
        return total;
    }
    return 0;
}


/* RELOCATED mode: remember where a global or string landed in the data
 * image so hccJitAppendDataSection can define a symbol for it. In
 * ABSOLUTE mode the address IS the answer and goes to host_symbols
 * instead, which is what the two call sites below select between. */
static void jitDataSymAdd(HccJit *jit, const char *name, size_t off,
                          int is_local)
{
    if (jit->n_data_syms == jit->cap_data_syms) {
        jit->cap_data_syms = jit->cap_data_syms ? jit->cap_data_syms * 2 : 32;
        jit->data_syms = realloc(jit->data_syms,
                jit->cap_data_syms * sizeof(*jit->data_syms));
    }
    jit->data_syms[jit->n_data_syms].name = strdup(name);
    jit->data_syms[jit->n_data_syms].off = off;
    jit->data_syms[jit->n_data_syms].is_local = is_local;
    jit->n_data_syms++;
}

/* Lay out the (from, sentinel] range's new strings + globals into a
 * fresh arena; register each by its mangled name so the backends'
 * global-address emitters can find them via host_symbols. Strings come
 * first so that AST_ARRAY_INIT initialisers in globals can reference
 * them by their already-known addresses. Anything a previous chunk
 * already laid out keeps its old (registered) address. */
/* The arena's bytes and the record describing them. */
static void jitArenaRelease(void *p) {
    HccJitArena *arena = (HccJitArena *)p;
    if (!arena) return;
    free(arena->base);
    free(arena);
}

/* `argc` / `argv`, with or without the platform's leading underscore. */
static int jitLabelIsArgcArgv(AoStr *label) {
    char *p = label->data;
    s64 n = label->len;
    if (n && *p == '_') { p++; n--; }
    return n == 4 && (!memcmp(p, "argc", 4) || !memcmp(p, "argv", 4));
}

static int jitAllocateGlobals(HccJit *jit, List *from) {
    size_t sz = jitGlobalsSize(jit, from);
    if (sz == 0) return 0;
    sz += 64; /* slack */
    uint8_t *globals = calloc(1, sz);
    HccJitArena *arena = (HccJitArena *)malloc(sizeof(*arena));
    arena->base = globals;
    arena->size = sz;
    listAppend(jit->globals_arenas, arena);
    size_t off = 0;

    if (jit->cc->strs) {
        MapIter mi; mapIterInit(jit->cc->strs, &mi);
        while (mapIterNext(&mi)) {
            Ast *ast = (Ast *)mi.node->value;
            
            if (ast->kind != AST_STRING) continue;
            if (mapGet(jit->host_symbols, (void *)ast->slabel->data)) continue;
            char *addr = (char *)(globals + off);
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
            if (jit->addr_mode == HCC_ADDR_RELOCATED) {
                /* A string literal's `.L<n>` label is this unit's own
                 * name for it; nothing outside may reach it. */
                jitDataSymAdd(jit, ast->slabel->data, off, 1);
            } else {
                mapAdd(jit->host_symbols, strdup(ast->slabel->data), addr);
            }
            /* Allocate based on the *decoded* length plus NUL, 8-aligned. */
            size_t decoded_size = (size_t)(o + 1);
            off += (decoded_size + 7) & ~7;
        }
    }

    /* Globals. Strings are now addressable, so AST_ARRAY_INIT
     * containing AST_STRING elements can resolve element addresses. */
    for (List *it = from; it != jit->cc->ast_list; it = it->next) {
        Ast *ast = it->value;
        AoStr *label = jitGlobalLabel(ast);
        if (!label) continue;
        /* `argc`/`argv` belong to the PROGRAM, not to each translation
         * unit, so only the object holding the entry point defines
         * them; elsewhere they stay undefined and the linker binds them
         * to that one definition. Two definitions made linking any two
         * HolyC objects impossible.
         *
         * The text emitters make the same decision, but this path never
         * consults them: the object writer builds its symbol table from
         * here, so the choice has to be made before the encoder sees
         * the name at all. */
        if (jit->addr_mode == HCC_ADDR_RELOCATED &&
            jitLabelIsArgcArgv(label) && !cctrlUnitHasEntryPoint(jit->cc))
        {
            continue;
        }
        /* The key both sides of the JIT agree on: the renamed symbol
         * when the declaration carried one, the raw name otherwise -
         * exactly what a64JitGlobalName hands the code. */
        const char *host_key = asmRenameFor(jit->cc, label);
        if (!host_key) host_key = label->data;
        if (mapGet(jit->host_symbols, (void *)host_key)) continue;
        uint8_t *addr = globals + off;
        size_t slot = (size_t)ast->declvar->type->size;
        if (jit->addr_mode == HCC_ADDR_RELOCATED) {
            /* The name the CODE references is the platform-normalised
             * one (`_g` on Mach-O); the data symbol must match or the
             * object defines `g` while every use asks for `_g`. On ELF
             * the transform is the identity.
             *
             * Dedup HERE too: the host_symbols check above only guards
             * the ABSOLUTE branch, so a global that appears twice in
             * the ast list (declared, then defined) registered two
             * symbols - Apple's ld asserts on the duplicate atom. */
            const char *n = asmNormaliseGlobalLabel(jit->cc, label)->data;
            if (hccJitDefinesSymbol(jit, n)) continue;
            /* `static` is internal linkage, and jitGlobalLabel has
             * already picked the `.L<n>` spelling for one - the same
             * per-unit counter the strings use, so exporting it would
             * collide with an unrelated static in the next object. */
            jitDataSymAdd(jit, n, off, ast->declvar->is_static);
        } else {
            mapAdd(jit->host_symbols, strdup(host_key), addr);
        }
        /* Zero first: an initialiser need not cover the whole object,
         * and what it leaves behind is defined to be zero rather than
         * whatever the arena happened to hold. */
        memset(addr, 0, slot);
        jitWriteInit(jit, addr, ast->declinit,
                     ast->declvar ? ast->declvar->type : NULL);
        off += (slot + 7) & ~7;
    }
    /* RELOCATED: the image goes into the byte stream after the code,
     * which has not been emitted yet - hold it until then. `off` is
     * what was actually used; the arena is over-sized because the
     * sizing pass counts undecoded escape sequences. */
    if (jit->addr_mode == HCC_ADDR_RELOCATED) {
        jit->data_base = globals;
        jit->data_len = off;
    }
    return 0;
}

int hccJitDefinesSymbol(HccJit *jit, const char *sym) {
    if (hccJitIsInternalFunc(jit, sym)) return 1;
    for (u32 i = 0; i < jit->n_data_syms; ++i) {
        if (!strcmp(jit->data_syms[i].name, sym)) return 1;
    }
    return 0;
}

void hccJitAppendDataSection(HccJit *jit) {
    if (jit->addr_mode != HCC_ADDR_RELOCATED) return;
    if (!jit->data_base || jit->data_len == 0) return;

    size_t split = jit->enc.len;
    jit->enc.data_split = (long)split;
    for (size_t i = 0; i < jit->data_len; ++i) {
        put_byte(&jit->enc, jit->data_base[i]);
    }

    for (u32 i = 0; i < jit->n_data_syms; ++i) {
        asm_define_label(&jit->enc, -1, jit->data_syms[i].name);
        AsmLabelDef *L = &jit->enc.labels[jit->enc.n_labels - 1];
        /* asm_define_label stamps the CURRENT end of the stream, which
         * is past the whole image - correct it to where the symbol
         * actually sits. */
        L->byte_offset = split + jit->data_syms[i].off;
        L->is_local = jit->data_syms[i].is_local;
    }

    /* Every hole a global's initialiser left for the address of a
     * string or a function becomes an absolute 64-bit relocation. */
    for (u32 i = 0; i < jit->n_ptr_fixups; ++i) {
        size_t off = (size_t)(jit->ptr_fixups[i].addr - jit->data_base);
        AsmFixup f = { 0 };
        f.kind = AF_SYMBOL;
        f.reloc = AFR_ABS64;
        f.patch_offset = split + off;
        f.width = 8;
        f.pcrel = 0;
        f.sym = strdup(jit->ptr_fixups[i].symbol);
        asm_add_fixup(&jit->enc, f);
    }
    jit->n_ptr_fixups = 0;   /* consumed; nothing to patch at runtime */
}

/* Make the exception runtime's symbols (HCC_PushFrame, HCC_TryLeave,
 * HCC_Throw, Fs) discoverable by dlsym. The AOT path links them at the
 * linker step; the JIT has to load them at runtime. Everything else a
 * program calls comes from libc, which is already in the process.
 *
 * The configured install prefix (cc->install_dir, `--install-dir`,
 * defaults to /usr/local) is probed first, then the dynamic linker's
 * own search paths and the in-tree build directory. */
static void jitLoadRuntime(Cctrl *cc) {
    static int rt_loaded = 0;
    if (rt_loaded) return;
    rt_loaded = 1;

    if (cc->install_dir && *cc->install_dir) {
        static const char *const suffixes[] = {
            "lib/libtos.0.0.1.dylib",
            "lib/libtos.so.0.0.1",
            "lib/libtos.dylib",
            "lib/libtos.so",
        };
        char path[1024];
        for (size_t i = 0; i < sizeof(suffixes)/sizeof(suffixes[0]); ++i) {
            snprintf(path, sizeof(path), "%s/%s", cc->install_dir, suffixes[i]);
            if (dlopen(path, RTLD_LAZY | RTLD_GLOBAL)) return;
        }
    }

    static const char *const candidates[] = {
        "libtos.dylib",
        "libtos.so",
        "./build/libtos.dylib",
        "./build/libtos.so",
    };
    for (size_t i = 0; i < sizeof(candidates)/sizeof(candidates[0]); ++i) {
        if (dlopen(candidates[i], RTLD_LAZY | RTLD_GLOBAL)) return;
    }
    /* Non-fatal if none load: only programs using try/catch need it,
     * and those fail later with a precise unresolved-symbol error. */
}

/* dlopen one library, making its exports visible to
 * asm_jit_dlsym_resolver. `name_form` entries came from `#link <name>`
 * and are probed with the linker's lib prefix + both platforms'
 * suffixes (dlopen inspects the file, not the extension, so trying
 * both is harmless); paths load verbatim. Returns 1 on success. */
static int jitDlopenLib(Cctrl *cc, const char *name, int name_form) {
    if (!name_form) {
        return dlopen(name, RTLD_LAZY | RTLD_GLOBAL) != NULL;
    }

    /* Bare names first: dyld / ld.so search their own default paths
     * (DYLD_LIBRARY_PATH / LD_LIBRARY_PATH, the dyld shared cache,
     * the ldconfig cache, ...). */
    static const char *const fmts[] = { "lib%s.dylib", "lib%s.so", "%s" };
    char buf[1024];
    for (size_t i = 0; i < sizeof(fmts)/sizeof(fmts[0]); ++i) {
        snprintf(buf, sizeof(buf), fmts[i], name);
        if (dlopen(buf, RTLD_LAZY | RTLD_GLOBAL)) return 1;
    }

    /* Then the places libraries commonly live that the dynamic linker
     * does NOT search for bare names: the configured install prefix
     * (mirrors the AOT `-L<prefix>/lib`), Homebrew (dlopen never
     * searches /opt/homebrew/lib on Apple Silicon), the classic system
     * dirs and Debian/Fedora arch dirs. */
    char prefix_lib[1024];
    const char *dirs[8];
    size_t ndirs = 0;
    if (cc->install_dir && *cc->install_dir) {
        snprintf(prefix_lib, sizeof(prefix_lib), "%s/lib", cc->install_dir);
        dirs[ndirs++] = prefix_lib;
    }
#ifdef __APPLE__ 
    dirs[ndirs++] = "/opt/homebrew/lib";
#endif

    dirs[ndirs++] = "/usr/local/lib";
    dirs[ndirs++] = "/usr/lib";

#if defined(__x86_64__)
    dirs[ndirs++] = "/usr/lib/x86_64-linux-gnu";
#elif defined(__aarch64__) || defined(__arm64__)
    dirs[ndirs++] = "/usr/lib/aarch64-linux-gnu";
#endif

    dirs[ndirs++] = "/usr/lib64";

    for (size_t d = 0; d < ndirs; ++d) {
        char *lib = tprintf("%s/lib%s.dylib", dirs[d], name);
        if (dlopen(lib, RTLD_LAZY | RTLD_GLOBAL)) return 1;
        lib = tprintf("%s/lib%s.so", dirs[d], name);
        if (dlopen(lib, RTLD_LAZY | RTLD_GLOBAL)) return 1;
    }
    return 0;
}

/* Load every shared object the CLI or a `#link` directive asked for.
 * Called from hccJitNew AND per chunk compile: in the REPL a `#link`
 * typed mid-session grows the lists after the JIT already exists.
 * The `loaded` set makes each entry load (and any failure report)
 * happen exactly once. */
static void jitLoadSharedObjects(Cctrl *cc) {
    static Set *loaded = NULL;
    if (loaded == NULL) loaded = setNew(32, &set_cstring_type);

    if (!listEmpty(cc->object_files)) {
        static int warned_object_files = 0;
        if (!warned_object_files) {
            warned_object_files = 1;
            fprintf(stderr, "Ignoring object files, only shared objects "
                    "can be loaded by the JIT\n");
        }
    }

    if (!listEmpty(cc->shared_object_files)) {
        listForEach(cc->shared_object_files) {
            AoStr *so = it->value;
            if (setHas(loaded, so->data)) continue;
            setAdd(loaded, so->data);
            if (!jitDlopenLib(cc, so->data, 0)) {
                fprintf(stderr, "hcc: #link: failed to load '%s': %s\n",
                        so->data, dlerror());
            }
        }
    }
    if (!listEmpty(cc->link_libs)) {
        listForEach(cc->link_libs) {
            AoStr *name = it->value;
            if (setHas(loaded, name->data)) continue;
            setAdd(loaded, name->data);
            if (!jitDlopenLib(cc, name->data, 1)) {
                fprintf(stderr, "hcc: #link: failed to load library '%s' "
                        "(probed lib%s.{dylib,so} in the dynamic linker's "
                        "default paths, %s/lib, /opt/homebrew/lib, "
                        "/usr/local/lib and the system lib dirs)\n",
                        name->data, name->data,
                        cc->install_dir ? cc->install_dir : "");
            }
        }
    }
}

/* ---------------- compile pipeline ---------------- */

/* `load_host_libs` distinguishes an EXECUTING context (the JIT, the
 * REPL) from a purely-emitting one (the AOT text path, the object
 * writer). Only the former needs the runtime and any `#link` shared
 * objects dlopened into this process - doing it for an AOT compile
 * was at best needless, and under a cross target it dlopen'd
 * foreign-architecture libraries just to print a failure. */
HccJit *hccJitNewEx(Cctrl *cc, const HccJitBackend *backend,
                    int load_host_libs) {
    return hccJitNewFor(cc, backend, load_host_libs, 1);
}

/* `check_target` is what `#eval` turns off.
 *
 * The guard asks whether the backend can emit for the target the
 * PROGRAM is being built for, which is the right question for the REPL
 * and for `-jit`. It is the wrong one for an `#eval` block: that code
 * never ships, it runs here, and evalJit deliberately picks the HOST
 * backend for exactly that reason. Everything below reaches codegen
 * through `backend->`, and this is the only line in the JIT that reads
 * `cc->target` at all, so declining to ask it is the whole change - no
 * shared state is mutated and no other path is affected. */
HccJit *hccJitNewFor(Cctrl *cc, const HccJitBackend *backend,
                     int load_host_libs, int check_target) {
    if (check_target && !backend->target_ok(cc->target)) {
        fprintf(stderr, "hcc: JIT only supports %s targets\n", backend->name);
        return NULL;
    }

    HccJit *jit = calloc(1, sizeof *jit);
    jit->cc             = cc;
    jit->backend        = backend;
    jit->symbols        = mapNew(64, &map_cstring_opaque_type);
    jit->host_symbols   = mapNew(64, &map_cstring_opaque_type);
    jit->chunk_fns      = mapNew(64, &map_cstring_opaque_type);
    jit->chunks         = listNew();
    jit->globals_arenas = listNew();
    jit->block_local    = mapNew(64, &map_uint_to_uint_type);
    jit->epi_local      = mapNew(16, &map_uint_to_uint_type);
    jit->next_local     = 0;
    /* Nothing consumed yet: cursors sit on the sentinels so the first
     * chunk sweeps the whole lists. */
    jit->ast_cursor     = cc->ast_list;
    jit->asm_cursor     = cc->asm_blocks;

    asm_enc_init(&jit->enc);
    backend->init_reg_pool();
    if (load_host_libs) {
        jitLoadRuntime(cc);
        jitLoadSharedObjects(cc);
    }
    return jit;
}

HccJit *hccJitNew(Cctrl *cc, const HccJitBackend *backend) {
    return hccJitNewEx(cc, backend, 1);
}

/* Register a chunk-internal function name (emit-time calls/IR_LEA to
 * it route through label fixups instead of host_symbols/dlsym). */
static void jitChunkFnAdd(HccJit *jit, const char *name) {
    if (!mapHas(jit->chunk_fns, (void *)name)) {
        mapAdd(jit->chunk_fns, strdup(name), (void *)(uintptr_t)1);
    }
}

int hccJitCompileChunk(HccJit *jit, Ast *extra_fn) {
    Cctrl *cc = jit->cc;

    /* Pick up any `#link` libraries recorded since the last chunk
     * (REPL inputs can add them at any point). A RELOCATED chunk never
     * executes, so it has no business loading anything. */
    if (jit->addr_mode != HCC_ADDR_RELOCATED) jitLoadSharedObjects(cc);

    /* Reset the per-chunk encoder + internal-function set. Pending
     * line notes from a failed previous chunk die with it. */
    asm_enc_free(&jit->enc);
    asm_enc_init(&jit->enc);
    mapClear(jit->chunk_fns);
    jit->n_pending = 0;

    /* First unconsumed nodes. The cursors advance immediately: even if
     * this chunk fails, the next call must not recompile its ASTs. */
    List *ast_from = jit->ast_cursor->next;
    List *asm_from = jit->asm_cursor ? jit->asm_cursor->next : NULL;
    jit->ast_cursor = cc->ast_list->prev;
    if (cc->asm_blocks) jit->asm_cursor = cc->asm_blocks->prev;

    if (jitAllocateGlobals(jit, ast_from) != 0) return -1;

    /* Pre-register every internal function name so emit-time code
     * (IR_LEA on a function, IR_CALL to another function in this
     * chunk) can route through label fixups rather than dlsym (which
     * doesn't see this-chunk JIT code). */
    for (List *it = ast_from; it != cc->ast_list; it = it->next) {
        Ast *ast = it->value;
        if (ast->kind != AST_FUNC) continue;
        jitChunkFnAdd(jit, asmNormaliseFunctionName(cc, ast->fname));
    }
    if (extra_fn) {
        jitChunkFnAdd(jit, asmNormaliseFunctionName(cc, extra_fn->fname));
    }

    /* `asm {}` functions are internal too: register both the raw label
     * name and its platform-mangled form so calls and IR_LEA route
     * through label fixups (the finalize matcher is underscore-
     * tolerant for the raw/mangled difference). */
    if (asm_from) {
        for (List *it = asm_from; it != cc->asm_blocks; it = it->next) {
            Ast *asm_block = (Ast *)it->value;
            if (!asm_block->funcs) continue;
            for (List *fl = asm_block->funcs->next; fl != asm_block->funcs;
                 fl = fl->next)
            {
                Ast *fn = (Ast *)fl->value;
                jitChunkFnAdd(jit, fn->asmfname->data);
                jitChunkFnAdd(jit, asmNormaliseFunctionName(cc, fn->asmfname));
            }
        }
    }

    IrCtx *ir_ctx = irCtxNew(cc);
    for (List *it = ast_from; it != cc->ast_list; it = it->next) {
        Ast *ast = it->value;
        if (ast->kind != AST_FUNC) continue;
        if (jit->backend->compile_function(jit, ast, ir_ctx) != 0) return -1;
        /* `static` means internal linkage. The label was defined while
         * compiling; mark it so the object writers emit a local symbol
         * rather than exporting a name another unit may also define
         * privately. */
        if (ast->flags & AST_FLAG_STATIC_FN) {
            asm_mark_label_local(&jit->enc,
                                 asmNormaliseFunctionName(cc, ast->fname));
        }
    }
    if (extra_fn) {
        if (jit->backend->compile_function(jit, extra_fn, ir_ctx) != 0)
            return -1;
    }
    /* `asm {}` function bodies: define each public label, then splice
     * the libtasm-encoded bytes in. */
    if (asm_from) {
        for (List *it = asm_from; it != cc->asm_blocks; it = it->next) {
            Ast *asm_block = (Ast *)it->value;
            if (!asm_block->funcs) continue;
            for (List *fl = asm_block->funcs->next; fl != asm_block->funcs;
                 fl = fl->next)
            {
                Ast *fn = (Ast *)fl->value;
                asm_define_label(&jit->enc, -1, fn->asmfname->data);
                if (hccJitAssembleText(jit, fn->body->asm_stmt, 0) != 0)
                    return -1;
            }
        }
    }

    /* Nothing emitted (e.g. the input only declared a class or a
     * prototype): success, no mapping needed. */
    if (jit->enc.len == 0) return 0;

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

    /* RELOCATED: everything below turns the bytes into RUNNABLE code -
     * mapping them RX, resolving symbols through dlsym, registering
     * addresses. An object file wants none of it; the linker does all
     * of it later. Append the data image and stop. */
    if (jit->addr_mode == HCC_ADDR_RELOCATED) {
        /* Branches between basic blocks are ours to resolve; only
         * symbol references are the linker's. Without this every one of
         * them reads zero and the code falls through instead of
         * looping. */
        if (asm_resolve_local_fixups(&jit->enc) != 0) return -1;
        hccJitAppendDataSection(jit);
        return 0;
    }

    AsmJitCode *code = calloc(1, sizeof *code);
    if (asm_jit_finalize(&jit->enc, jitResolveSymbol, jit, code) != 0) {
        free(code);
        return -1;
    }
    listAppend(jit->chunks, code);

    /* Rebase this chunk's pending (enc offset, line) notes onto the
     * finalized mapping and publish them for pc->line lookups. */
    if (jit->n_pending) {
        if (jit->n_lines + jit->n_pending > jit->cap_lines) {
            jit->cap_lines = jit->cap_lines ? jit->cap_lines * 2 : 128;
            while (jit->cap_lines < jit->n_lines + jit->n_pending)
                jit->cap_lines *= 2;
            jit->lines = realloc(jit->lines,
                    jit->cap_lines * sizeof(HccJitLine));
        }
        for (u32 i = 0; i < jit->n_pending; ++i) {
            jit->lines[jit->n_lines].addr =
                (uintptr_t)code->code + jit->pending_lines[i].addr;
            jit->lines[jit->n_lines].line = jit->pending_lines[i].line;
            /* HCC_DEBUG_LINES=1 dumps the table as chunks finalize -
             * pairs with `Uf` output when debugging attribution. */
            if (getenv("HCC_DEBUG_LINES"))
                fprintf(stderr, "line-table: %p line %d\n",
                        (void *)jit->lines[jit->n_lines].addr,
                        jit->lines[jit->n_lines].line);
            jit->n_lines++;
        }
        jit->n_pending = 0;
    }

    /* Each public label now points at (code base + label byte_offset).
     * Register in `symbols` (hccJitLookup) and in `host_symbols` so
     * later chunks' emit-time addressing and finalize-time resolver
     * both see it. A redefinition overwrites: future chunks bind to
     * the newest body, already-patched code keeps the old one. */
    for (int i = 0; i < jit->enc.n_labels; ++i) {
        AsmLabelDef *L = &jit->enc.labels[i];
        if (!L->name) continue;
        void *addr = (uint8_t *)code->code + L->byte_offset;
        mapAdd(jit->symbols, strdup(L->name), addr);
        mapAdd(jit->host_symbols, strdup(L->name), addr);
    }

    /* Patch the holes left where a global's initialiser named a
     * function. Its address exists only now that the chunk defining it
     * has been finalized and registered above. */
    for (u32 i = 0; i < jit->n_ptr_fixups; ) {
        void *target = jitResolveSymbol(jit, jit->ptr_fixups[i].symbol);
        if (!target && jit->ptr_fixups[i].symbol[0] != '_') {
            /* Labels are registered under the platform-mangled name -
             * `_G` on Mach-O - while the AST carries the source
             * spelling. Same retry hccJitLookup does. */
            char *u = malloc(strlen(jit->ptr_fixups[i].symbol) + 2);
            u[0] = '_';
            strcpy(u + 1, jit->ptr_fixups[i].symbol);
            target = jitResolveSymbol(jit, u);
            free(u);
        }
        if (!target) {
            /* Defined by a later chunk - leave it for the next pass. */
            ++i;
            continue;
        }
        uintptr_t v = (uintptr_t)target;
        memcpy(jit->ptr_fixups[i].addr, &v, 8);
        free(jit->ptr_fixups[i].symbol);
        jit->ptr_fixups[i] = jit->ptr_fixups[--jit->n_ptr_fixups];
    }
    return 0;
}

/* Lower the whole program and write it out as a relocatable object.
 *
 * The same lowering the JIT uses, with HCC_ADDR_RELOCATED selected so
 * every global, string and function reference becomes a relocation
 * rather than a baked address. Returns 0 on success. */
int hccJitCompileObject(Cctrl *cc, const HccJitBackend *backend,
                        Target target, ObjFormat fmt, FILE *out)
{
    HccJit *jit = hccJitNewEx(cc, backend, 0);
    if (!jit) return -1;
    jit->addr_mode = HCC_ADDR_RELOCATED;

    /* Same file-scope-statement wrapping the JIT path does: statements
     * at file scope become a synthetic `main`, and the user's own Main
     * is renamed. An object has to agree with that or the entry point
     * is missing. */
    Ast *synth_main = asmBuildInitialiserMain(cc);

    if (hccJitCompileChunk(jit, synth_main) != 0) {
        hccJitFree(jit);
        return -1;
    }
    int rc = asm_obj_write(&jit->enc, NULL, target, fmt, out);
    hccJitFree(jit);
    return rc;
}

HccJit *hccJitCompile(Cctrl *cc, const HccJitBackend *backend) {
    HccJit *jit = hccJitNew(cc, backend);
    if (!jit) return NULL;

    /* -Memsafe: install the tracking allocator before the chunk
     * compiles, so every call site binds to it at finalize. */
    if (cc->flags & CCTRL_MEMSAFE)
        memsafeInit(jit);

    /* HolyC "scripting" mode: statements at file scope land in
     * cc->initalisers and get wrapped into a synthetic `main`. When
     * this happens the user-supplied Main is renamed to MainFn (see
     * asmNormaliseFunctionName). We have to apply the same wrapping
     * before compiling so the entry point we hand to `hccJitRunMain`
     * actually runs the init statements. */
    Ast *synth_main = asmBuildInitialiserMain(cc);

    if (hccJitCompileChunk(jit, synth_main) != 0) {
        hccJitFree(jit);
        return NULL;
    }
    return jit;
}

/* ---------------- public accessors ---------------- */

void *hccJitLookup(HccJit *jit, const char *name) {
    if (!jit || !name) return NULL;
    void *p = mapGet(jit->symbols, (void *)name);
    if (p) return p;
    /* Try the underscore-prefixed mangled form too (macOS convention).
     * This must be attempted even when the name already starts with an
     * underscore: Mach-O prefixes every C symbol, so a function called
     * `__repl_1` is emitted as `___repl_1`. Skipping the retry for
     * `_`-leading names lost the REPL's own entry points. */
    char buf[256];
    snprintf(buf, sizeof buf, "_%s", name);
    return mapGet(jit->symbols, (void *)buf);
}

AsmJitCode *hccJitFindChunk(HccJit *jit, void *pc) {
    if (!jit || !pc) return NULL;
    listForEach(jit->chunks) {
        AsmJitCode *chunk = (AsmJitCode *)it->value;
        uint8_t *base = (uint8_t *)chunk->code;
        if ((uint8_t *)pc >= base &&
            (uint8_t *)pc < base + chunk->size + chunk->veneer_size)
        {
            return chunk;
        }
    }
    return NULL;
}

const char *hccJitFindSymbolForAddr(HccJit *jit, void *pc, size_t *off_out) {
    if (off_out) *off_out = 0;
    AsmJitCode *chunk = hccJitFindChunk(jit, pc);
    if (chunk == NULL) return NULL;
    /* Veneers belong to the chunk, not to whichever function happens
     * to be emitted last - don't attribute them. */
    if ((uint8_t *)pc >= (uint8_t *)chunk->code + chunk->size) return NULL;

    const char *best = NULL;
    uint8_t *best_addr = NULL;
    MapIter mi;
    mapIterInit(jit->symbols, &mi);
    while (mapIterNext(&mi)) {
        uint8_t *sym = (uint8_t *)mi.node->value;
        if (sym <= (uint8_t *)pc && sym >= (uint8_t *)chunk->code &&
            (best_addr == NULL || sym > best_addr))
        {
            best_addr = sym;
            best = (const char *)mi.node->key;
        }
    }
    if (best && off_out) *off_out = (size_t)((uint8_t *)pc - best_addr);
    return best;
}

void hccJitNoteLine(HccJit *jit, int line) {
    if (!jit || line <= 0) return;
    /* Collapse runs: only the line CHANGES matter. */
    if (jit->n_pending &&
        jit->pending_lines[jit->n_pending - 1].line == line)
    {
        return;
    }
    if (jit->n_pending == jit->cap_pending) {
        jit->cap_pending = jit->cap_pending ? jit->cap_pending * 2 : 64;
        jit->pending_lines = realloc(jit->pending_lines,
                jit->cap_pending * sizeof(HccJitLine));
    }
    jit->pending_lines[jit->n_pending].addr = (uintptr_t)jit->enc.len;
    jit->pending_lines[jit->n_pending].line = line;
    jit->n_pending++;
}

int hccJitFindLineForAddr(HccJit *jit, void *pc) {
    if (!jit || !jit->lines) return 0;
    AsmJitCode *chunk = hccJitFindChunk(jit, pc);
    if (chunk == NULL) return 0;
    uintptr_t lo = (uintptr_t)chunk->code;
    uintptr_t hi = lo + chunk->size; /* veneers carry no lines */
    uintptr_t p = (uintptr_t)pc;
    if (p >= hi) return 0;
    int best_line = 0;
    uintptr_t best_addr = 0;
    for (u32 i = 0; i < jit->n_lines; ++i) {
        HccJitLine *e = &jit->lines[i];
        if (e->addr < lo || e->addr >= hi) continue; /* other chunk */
        if (e->addr <= p && e->addr >= best_addr) {
            best_addr = e->addr;
            best_line = e->line;
        }
    }
    return best_line;
}

void hccJitDefineSymbol(HccJit *jit, const char *name, void *addr) {
    if (!jit || !name) return;
    /* Register BOTH spellings: HolyC-compiled call sites arrive
     * platform-mangled (`_free` on Mach-O) while `asm {}` blocks emit
     * the symbol verbatim (`CALL malloc` -> `malloc`). The resolver's
     * host_symbols lookup is exact, so one spelling would miss the
     * other's callers. */
    mapAdd(jit->host_symbols, strdup(name), addr);
    if (name[0] != '_') {
        char buf[64];
        snprintf(buf, sizeof(buf), "_%s", name);
        mapAdd(jit->host_symbols, strdup(buf), addr);
    } else {
        mapAdd(jit->host_symbols, strdup(name + 1), addr);
    }
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
    free(jit->lines);
    free(jit->pending_lines);
    asm_enc_free(&jit->enc);
    if (jit->chunks) {
        listForEach(jit->chunks) {
            AsmJitCode *code = (AsmJitCode *)it->value;
            asm_jit_free(code);
            free(code);
        }
        listRelease(jit->chunks, NULL);
    }
    if (jit->globals_arenas)
        listRelease(jit->globals_arenas, jitArenaRelease);
    if (jit->symbols) mapRelease(jit->symbols);
    if (jit->host_symbols) mapRelease(jit->host_symbols);
    if (jit->chunk_fns) mapRelease(jit->chunk_fns);
    if (jit->block_local) mapRelease(jit->block_local);
    if (jit->epi_local) mapRelease(jit->epi_local);
    free(jit);
}
