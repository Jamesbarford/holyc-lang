#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/mman.h>

#include "asm_jit.h"
#include "enc_arm64.h"

/* Apple Silicon JIT requires MAP_JIT + the per-thread write-protect
 * toggle. Apple-Intel and non-Apple Unixes use plain mmap RW + mprotect
 * RX, so we gate that machinery behind `APPLE_SILICON_JIT`. */
#if defined(__APPLE__) && defined(__aarch64__)
#  define APPLE_SILICON_JIT 1
#  include <pthread.h>
#  include <libkern/OSCacheControl.h>
#elif defined(__APPLE__)
#  include <libkern/OSCacheControl.h>  /* sys_icache_invalidate is defined here too */
#endif

#if defined(__APPLE__)
#  include <mach-o/dyld.h>
#  include <mach-o/nlist.h>
#  include <mach-o/loader.h>

/* dlsym always prepends a leading underscore, so it can't find symbols
 * whose Mach-O name has none. hcc emits global *variables* without the
 * underscore (functions get it via asmNormaliseFunctionName), so an
 * external HolyC global like libtos's `Fs` is invisible to dlsym even
 * though it's exported. Walk every loaded image's symbol table and
 * match the name verbatim. Returns NULL if not found. */
static void *jit_macho_image_lookup(const struct mach_header_64 *mh,
                                    intptr_t slide, const char *want) {
    if (!mh || mh->magic != MH_MAGIC_64) return NULL;
    const struct load_command *lc = (const struct load_command *)(mh + 1);
    const struct symtab_command *st = NULL;
    uint64_t le_vmaddr = 0, le_fileoff = 0;
    int have_le = 0;
    for (uint32_t c = 0; c < mh->ncmds; ++c) {
        if (lc->cmd == LC_SEGMENT_64) {
            const struct segment_command_64 *sc =
                (const struct segment_command_64 *)lc;
            if (!strcmp(sc->segname, "__LINKEDIT")) {
                le_vmaddr = sc->vmaddr;
                le_fileoff = sc->fileoff;
                have_le = 1;
            }
        } else if (lc->cmd == LC_SYMTAB) {
            st = (const struct symtab_command *)lc;
        }
        lc = (const struct load_command *)((const char *)lc + lc->cmdsize);
    }
    if (!st || !have_le) return NULL;
    /* __LINKEDIT is mapped at its own vmaddr (not contiguous with the
     * header at file offsets - especially for dyld-shared-cache images),
     * so resolve symtab/strtab through it: addr = le_base + (off - le_fileoff). */
    const char *le_base =
        (const char *)(uintptr_t)(le_vmaddr + slide) - le_fileoff;
    const struct nlist_64 *syms =
        (const struct nlist_64 *)(le_base + st->symoff);
    const char *strs = le_base + st->stroff;
    for (uint32_t s = 0; s < st->nsyms; ++s) {
        uint32_t off = syms[s].n_un.n_strx;
        if (off == 0 || off >= st->strsize) continue;
        if ((syms[s].n_type & N_STAB) != 0) continue;   /* debug entry */
        if ((syms[s].n_type & N_TYPE) == N_UNDF) continue; /* undefined */
        if (!strcmp(strs + off, want)) {
            return (void *)(uintptr_t)(syms[s].n_value + slide);
        }
    }
    return NULL;
}

/* Scan loaded images' symbol tables, matching the name verbatim.
 * Only non-system images are scanned: the underscore-less
 * symbols are exclusive to hcc-compiled objects (libtos), and skipping
 * the dyld shared cache keeps this fast and avoids its quirks. */
static void *jit_macho_symtab_lookup(const char *want) {
    uint32_t n = _dyld_image_count();
    for (uint32_t i = 0; i < n; ++i) {
        const char *path = _dyld_get_image_name(i);
        if (path && (strncmp(path, "/usr/lib/", 9) == 0 ||
                     strncmp(path, "/System/", 8) == 0)) {
            continue;
        }
        void *p = jit_macho_image_lookup(
            (const struct mach_header_64 *)_dyld_get_image_header(i),
            _dyld_get_image_vmaddr_slide(i), want);
        if (p) return p;
    }
    return NULL;
}
#endif /* __APPLE__ */

/* AArch64 BL/B: top byte is 0x94 (BL) or 0x14 (B); low 26 bits are a signed
 * word-count displacement. +/-128MB range. */
static int
aarch64_patch_branch26(uint8_t *buf, size_t off, void *target, int is_bl)
{
    intptr_t pc = (intptr_t)(buf + off);
    intptr_t rel = (intptr_t)target - pc;
    if (rel & 3) return -1;
    int32_t words = (int32_t)(rel / 4);
    if (words < -(1 << 25) || words >= (1 << 25)) return -1;
    uint32_t w = (is_bl ? 0x94000000u : 0x14000000u) |
                 ((uint32_t)words & 0x03FFFFFFu);
    memcpy(buf + off, &w, 4);
    return 0;
}

/* AArch64 B.cond: top byte 0x54, bits[23:5] = imm19 (signed word-count
 * displacement), bit[4] = 0, bits[3:0] = cond. +/-1MB range. */
static int
aarch64_patch_imm19(uint8_t *buf, size_t off, void *target)
{
    intptr_t pc = (intptr_t)(buf + off);
    intptr_t rel = (intptr_t)target - pc;
    if (rel & 3) return -1;
    int32_t words = (int32_t)(rel / 4);
    if (words < -(1 << 18) || words >= (1 << 18)) return -1;
    uint32_t w;
    memcpy(&w, buf + off, 4);
    w &= ~(0x7FFFFu << 5);
    w |= ((uint32_t)words & 0x7FFFFu) << 5;
    memcpy(buf + off, &w, 4);
    return 0;
}

/* x86_64 rel32: signed displacement from the END of the instruction (i.e.
 * patch_offset + 4) to the target. +/-2GB range. Used by CALL/JMP/Jcc and
 * by the SIGNED reloc (LEA RIP-relative). */
static int
x86_64_patch_rel32(uint8_t *buf, size_t off, void *target)
{
    intptr_t pc_next = (intptr_t)(buf + off + 4);
    intptr_t rel = (intptr_t)target - pc_next;
    if (rel > (intptr_t)INT32_MAX || rel < (intptr_t)INT32_MIN) return -1;
    int32_t d = (int32_t)rel;
    memcpy(buf + off, &d, 4);
    return 0;
}

/* AArch64 ADRP: imm21 = (target_page - pc_page), where each "page" is
 * 4KB. The 21-bit value is split across the encoding: low 2 bits go to
 * bits[30:29] (immlo), high 19 bits go to bits[23:5] (immhi). +/-4GB
 * range thanks to the 4KB page granularity. */
static int
aarch64_patch_page21(uint8_t *buf, size_t off, void *target)
{
    intptr_t pc = (intptr_t)(buf + off);
    intptr_t pc_page = pc & ~(intptr_t)0xFFF;
    intptr_t tgt_page = (intptr_t)target & ~(intptr_t)0xFFF;
    intptr_t delta_pages = (tgt_page - pc_page) >> 12;
    if (delta_pages < -(1 << 20) || delta_pages >= (1 << 20)) return -1;
    uint32_t imm21 = (uint32_t)delta_pages & 0x001FFFFFu;
    uint32_t immlo = imm21 & 0x3u;
    uint32_t immhi = (imm21 >> 2) & 0x7FFFFu;
    uint32_t w;
    memcpy(&w, buf + off, 4);
    w &= ~((0x3u << 29) | (0x7FFFFu << 5));
    w |= (immlo << 29) | (immhi << 5);
    memcpy(buf + off, &w, 4);
    return 0;
}

/* AArch64 PAGEOFF12: imm12 = (target & 0xFFF). Patches the imm12 field
 * of the paired ADD (immediate, sf=1, bits[21:10]). The fixup is NOT
 * PC-relative - the bottom 12 bits of the target's absolute address
 * land directly in the immediate. */
static int
aarch64_patch_pageoff12(uint8_t *buf, size_t off, void *target)
{
    uint32_t imm12 = (uint32_t)((uintptr_t)target & 0xFFFu);
    uint32_t w;
    memcpy(&w, buf + off, 4);
    w &= ~(0xFFFu << 10);
    w |= (imm12 & 0xFFFu) << 10;
    memcpy(buf + off, &w, 4);
    return 0;
}

static size_t
asm_page_round_up(size_t n)
{
    long ps = sysconf(_SC_PAGESIZE);
    size_t p = ps > 0 ? (size_t)ps : 4096;
    return (n + p - 1) & ~(p - 1);
}

/* Emit a 5-instruction trampoline that loads `target` into x16 then
 * branches to it. Used when an arm64 BL/B's target is more than +/-128MB
 * away, which is common when JIT'd code calls into libSystem.
 *
 *   movz x16, target[15:0]
 *   movk x16, target[31:16], lsl #16
 *   movk x16, target[47:32], lsl #32
 *   movk x16, target[63:48], lsl #48
 *   br   x16
 *
 * The veneer is reached via the original BL (which sets LR to the
 * instruction after the BL); `br x16` preserves LR so when the callee
 * eventually `ret`s, control returns to the original caller. */
#define AARCH64_VENEER_BYTES 20

/* The bytes we drop into the executable mapping must match what
 * `aarch64_jit_emit_trampoline` would have emitted into an AsmEnc.
 * Routing through a throwaway AsmEnc keeps the two paths in lock-step:
 * one canonical helper, no duplicated bit patterns. */
static void
aarch64_emit_veneer(uint8_t *p, void *target)
{
    AsmEnc tmp;
    asm_enc_init(&tmp);
    aarch64_jit_emit_trampoline(&tmp, target);
    memcpy(p, tmp.bytes, AARCH64_VENEER_BYTES);
    asm_enc_free(&tmp);
}

/* x86_64 veneer for when an out-of-range CALL/JMP rel32 won't reach:
 *
 *   movabs r11, target       ; 49 BB + 8-byte imm
 *   jmp    r11               ; 41 FF E3
 *
 * Total 13 bytes. r11 is a caller-saved scratch register in System V
 * and isn't used by any C calling convention. For a CALL, the original
 * CALL has already pushed the return address - `jmp r11` jumps into
 * the callee without disturbing it, so the callee's `ret` returns to
 * the original CALL's successor instruction. */
#define X86_64_VENEER_BYTES 13

static void
x86_64_emit_veneer(uint8_t *p, void *target)
{
    uintptr_t t = (uintptr_t)target;
    p[0] = 0x49; /* REX.W | REX.B */
    p[1] = 0xBB; /* MOV r11, imm64 (B8 + r11&7 = B8+3) */
    p[2]  = (uint8_t)(t      );
    p[3]  = (uint8_t)(t >>  8);
    p[4]  = (uint8_t)(t >> 16);
    p[5]  = (uint8_t)(t >> 24);
    p[6]  = (uint8_t)(t >> 32);
    p[7]  = (uint8_t)(t >> 40);
    p[8]  = (uint8_t)(t >> 48);
    p[9]  = (uint8_t)(t >> 56);
    p[10] = 0x41; /* REX.B */
    p[11] = 0xFF; /* JMP r/m64 (/4) */
    p[12] = 0xE3; /* modrm: mod=3 reg=4 rm=3 (r11&7) */
}

int
asm_jit_finalize(AsmEnc *enc, asm_jit_resolver_fn resolver, void *ud,
                 AsmJitCode *out)
{
    if (!enc || !out) return -1;

    out->code = NULL;
    out->size = 0;
    out->_mapping = NULL;
    out->_mapping_size = 0;
    if (enc->len == 0) return 0;

    /* Count fixups that MIGHT need a veneer (target out of direct
     * branch range). We over-allocate one veneer slot per potentially-
     * branching fixup; veneers are only emitted for the ones that
     * actually exceed range. arm64 BRANCH26 has +/-128MB; x86 rel32 has
     * +/-2GB - both are reachable distances normal in-process, but
     * libSystem under Rosetta can easily blow x86's +/-2GB. */
    int max_a64_veneers = 0;
    int max_x86_veneers = 0;
    for (int i = 0; i < enc->n_fixups; i++) {
        AsmFixupReloc r = enc->fixups[i].reloc;
        if (r == AFR_AARCH64_CALL26 || r == AFR_AARCH64_JUMP26)
            max_a64_veneers++;
        else if (r == AFR_X86_64_CALL32 || r == AFR_X86_64_JMP32 ||
                 r == AFR_X86_64_JCC32  || r == AFR_X86_64_SIGNED)
            max_x86_veneers++;
    }
    size_t veneer_area = (size_t)max_a64_veneers * AARCH64_VENEER_BYTES +
                         (size_t)max_x86_veneers * X86_64_VENEER_BYTES;
    size_t map_size = asm_page_round_up(enc->len + veneer_area);
    int prot = PROT_READ | PROT_WRITE;
    int flags = MAP_PRIVATE | MAP_ANONYMOUS;
#if APPLE_SILICON_JIT
    /* Apple Silicon: mapping is created RX with MAP_JIT; writability is
     * gated by the per-thread JIT write-protect toggle. */
    prot |= PROT_EXEC;
    flags |= MAP_JIT;
#endif
    void *mem = mmap(NULL, map_size, prot, flags, -1, 0);
    if (mem == MAP_FAILED) {
        asm_err_at(enc, NULL, asm_tmp_printf("JIT: mmap failed"));
        return -1;
    }

#if APPLE_SILICON_JIT
    pthread_jit_write_protect_np(0);
#endif

    memcpy(mem, enc->bytes, enc->len);
    uint8_t *buf = (uint8_t *)mem;
    size_t veneer_off = enc->len; /* next free veneer slot */
    int rc = 0;

    for (int i = 0; i < enc->n_fixups; i++) {
        AsmFixup *f = &enc->fixups[i];
        void *target = NULL;
        const char *desc = NULL;  /* for the error message */

        if (f->kind == AF_SYMBOL) {
            /* Public labels defined in this module win over external
             * symbols - cross-function calls within the JIT'd code
             * resolve to (mapping base + label offset). */
            for (int k = 0; k < enc->n_labels; ++k) {
                AsmLabelDef *def = &enc->labels[k];
                if (!def->name) continue;
                /* Underscore-tolerant: HolyC call sites carry the
                 * platform-mangled name (`_Foo` on Mach-O) while a
                 * label written in an `asm {}` block keeps the user's
                 * spelling - and vice versa for asm calling HolyC. */
                if (strcmp(def->name, f->sym) == 0 ||
                    (def->name[0] == '_' &&
                     strcmp(def->name + 1, f->sym) == 0) ||
                    (f->sym[0] == '_' &&
                     strcmp(def->name, f->sym + 1) == 0))
                {
                    target = buf + def->byte_offset;
                    break;
                }
            }
            if (target == NULL)
                target = resolver ? resolver(ud, f->sym) : NULL;
            desc = f->sym;
            if (target == NULL) {
                asm_err_at(enc, NULL,
                           asm_tmp_printf("JIT: unresolved symbol '%s'", f->sym));
                rc = -1;
                break;
            }
        } else if (f->kind == AF_LOCAL) {
            /* Local labels are stored in enc->labels with local_num >= 0
             * and name == NULL (public labels store the name instead).
             * Walk the table to find the matching local_num. */
            target = NULL;
            for (int k = 0; k < enc->n_labels; ++k) {
                AsmLabelDef *def = &enc->labels[k];
                if (def->name == NULL && def->local_num == f->local_num) {
                    target = buf + def->byte_offset;
                    break;
                }
            }
            if (target == NULL) {
                asm_err_at(enc, NULL,
                        asm_tmp_printf("JIT: unresolved local label @%d",
                            f->local_num));
                rc = -1;
                break;
            }
        } else {
            continue;  /* AF_CLASS_MEMBER not handled by JIT */
        }

        int prc = -1;
        switch (f->reloc) {
            case AFR_AARCH64_CALL26:
            case AFR_AARCH64_JUMP26: {
                int is_bl = (f->reloc == AFR_AARCH64_CALL26);
                prc = aarch64_patch_branch26(buf, f->patch_offset, target, is_bl);
                if (prc != 0) {
                    /* Target out of +/-128MB. Emit a veneer at the end of
                     * the mapping that loads the full 64-bit target into
                     * x16 and `br`s to it; then re-patch the original
                     * BL/B to jump to the veneer instead. */
                    void *vp = buf + veneer_off;
                    aarch64_emit_veneer(vp, target);
                    veneer_off += AARCH64_VENEER_BYTES;
                    prc = aarch64_patch_branch26(buf, f->patch_offset, vp, is_bl);
                }
                break;
            }
            case AFR_AARCH64_BCOND19:
                prc = aarch64_patch_imm19(buf, f->patch_offset, target);
                break;
            case AFR_AARCH64_PAGE21:
                prc = aarch64_patch_page21(buf, f->patch_offset, target);
                break;
            case AFR_AARCH64_PAGEOFF12:
                prc = aarch64_patch_pageoff12(buf, f->patch_offset, target);
                break;
            case AFR_X86_64_CALL32:
            case AFR_X86_64_JMP32:
            case AFR_X86_64_JCC32:
            case AFR_X86_64_SIGNED:
                prc = x86_64_patch_rel32(buf, f->patch_offset, target);
                if (prc != 0 && f->reloc != AFR_X86_64_SIGNED) {
                    /* Target out of +/-2GB. Emit a movabs+jmp veneer and
                     * redirect the CALL/JMP/Jcc to it.
                     *
                     * SIGNED (LEA RIP-rel) can't go through a veneer -
                     * we'd be loading the wrong address - so we surface
                     * that error directly to the caller. */
                    void *vp = buf + veneer_off;
                    x86_64_emit_veneer(vp, target);
                    veneer_off += X86_64_VENEER_BYTES;
                    prc = x86_64_patch_rel32(buf, f->patch_offset, vp);
                }
                break;
            default:
                break;
        }
        if (prc != 0) {
            asm_err_at(enc, NULL,
                       asm_tmp_printf("JIT: '%s' out of range or unsupported "
                                      "reloc=%d",
                                      desc ? desc : "<local>", (int)f->reloc));
            rc = -1;
            break;
        }
    }

#if APPLE_SILICON_JIT
    pthread_jit_write_protect_np(1);
#endif

    if (rc != 0) {
        munmap(mem, map_size);
        return -1;
    }

    /* Bytes the CPU may execute = original code + any veneers we emitted. */
    size_t live_bytes = veneer_off;
#if APPLE_SILICON_JIT
    /* Mapping is already RX via MAP_JIT; just flush the icache. */
    sys_icache_invalidate(mem, live_bytes);
#else
    /* Apple-Intel and Linux: transition the page from RW to RX. */
    if (mprotect(mem, map_size, PROT_READ | PROT_EXEC) != 0) {
        asm_err_at(enc, NULL, asm_tmp_printf("JIT: mprotect failed"));
        munmap(mem, map_size);
        return -1;
    }
#  if defined(__APPLE__)
    sys_icache_invalidate(mem, live_bytes);
#  else
    __builtin___clear_cache((char *)mem, (char *)mem + live_bytes);
#  endif
#endif

    out->code = mem;
    out->size = enc->len;
    out->_mapping = mem;
    out->_mapping_size = map_size;
    return 0;
}

void
asm_jit_free(AsmJitCode *code)
{
    if (!code || !code->_mapping) return;
    munmap(code->_mapping, code->_mapping_size);
    code->code = NULL;
    code->size = 0;
    code->_mapping = NULL;
    code->_mapping_size = 0;
}

#ifndef RTLD_DEFAULT
#define RTLD_DEFAULT ((void *) 0)
#endif

void *
asm_jit_dlsym_resolver(void *ud, const char *sym)
{
    (void)ud;
    if (!sym) return NULL;
    /* Try the name exactly as emitted first. On ELF/Linux a leading
     * underscore can be part of the real symbol name - e.g. libtos's
     * hand-rolled `_MALLOC`/`_FREE` trampolines are exported as
     * `_MALLOC`/`_FREE` - so stripping it up front would break the
     * lookup. */
    void *p = dlsym(RTLD_DEFAULT, sym);
    if (p) return p;
    /* macOS emits C symbols with a leading underscore in asm while the
     * matching dlsym lookup wants the unprefixed name, so fall back to
     * the stripped form. */
    if (sym[0] == '_') {
        p = dlsym(RTLD_DEFAULT, sym + 1);
        if (p) return p;
    }
#if defined(__APPLE__)
    /* dlsym can't see underscore-less Mach-O names (e.g. hcc's global
     * variables like libtos's `Fs`). Fall back to a raw symbol-table
     * scan, trying the name as emitted and the unprefixed form. */
    p = jit_macho_symtab_lookup(sym);
    if (!p && sym[0] == '_') p = jit_macho_symtab_lookup(sym + 1);
    if (p) return p;
#endif
    return NULL;
}
