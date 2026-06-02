/* This is not used but would be nice to use so then we only need a linker
 * and no longer need it to write object files. However it is _really_ fiddly */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "macho.h"
#include "tasm_util.h"

/* ============================================================== Mach-O format
 *
 * Defined inline so we don't depend on SDK header paths and can cross-compile.
 * Field types/order match the canonical
 * <mach-o/loader.h> + <mach-o/nlist.h> definitions . */

#define MH_MAGIC_64         0xFEEDFACFu
#define MH_OBJECT           0x1u
#define CPU_ARCH_ABI64      0x01000000u
#define CPU_TYPE_X86_64     (CPU_ARCH_ABI64 | 7u)
#define CPU_TYPE_ARM64      (CPU_ARCH_ABI64 | 12u)
#define CPU_SUBTYPE_ARM64_ALL  0u
#define CPU_SUBTYPE_X86_64_ALL 3u

#define LC_SEGMENT_64       0x19u
#define LC_SYMTAB           0x2u
#define LC_BUILD_VERSION    0x32u
#define PLATFORM_MACOS      1u

#define VM_PROT_READ        0x1
#define VM_PROT_WRITE       0x2
#define VM_PROT_EXECUTE     0x4

#define S_REGULAR                  0x0u
#define S_ATTR_SOME_INSTRUCTIONS   0x00000400u
#define S_ATTR_PURE_INSTRUCTIONS   0x80000000u

/* nlist_64 n_type bits. */
#define N_EXT   0x01u
#define N_UNDF  0x00u
#define N_SECT  0x0Eu

/* arm64 relocation type codes (from <mach-o/arm64/reloc.h>). */
#define ARM64_RELOC_UNSIGNED   0
#define ARM64_RELOC_SUBTRACTOR 1
#define ARM64_RELOC_BRANCH26   2
#define ARM64_RELOC_PAGE21     3
#define ARM64_RELOC_PAGEOFF12  4

/* x86_64 relocation type codes (from <mach-o/x86_64/reloc.h>). */
#define X86_64_RELOC_UNSIGNED  0
#define X86_64_RELOC_SIGNED    1
#define X86_64_RELOC_BRANCH    2
#define X86_64_RELOC_GOT_LOAD  3
#define X86_64_RELOC_GOT       4
#define X86_64_RELOC_SUBTRACTOR 5

struct mh_header64 {
    uint32_t magic;
    uint32_t cputype;
    uint32_t cpusubtype;
    uint32_t filetype;
    uint32_t ncmds;
    uint32_t sizeofcmds;
    uint32_t flags;
    uint32_t reserved;
};

struct mh_segment_command64 {
    uint32_t cmd;
    uint32_t cmdsize;
    char     segname[16];
    uint64_t vmaddr;
    uint64_t vmsize;
    uint64_t fileoff;
    uint64_t filesize;
    int32_t  maxprot;
    int32_t  initprot;
    uint32_t nsects;
    uint32_t flags;
};

struct mh_section64 {
    char     sectname[16];
    char     segname[16];
    uint64_t addr;
    uint64_t size;
    uint32_t offset;
    uint32_t align;     /* power of 2 */
    uint32_t reloff;
    uint32_t nreloc;
    uint32_t flags;
    uint32_t reserved1;
    uint32_t reserved2;
    uint32_t reserved3;
};

struct mh_symtab_command {
    uint32_t cmd;
    uint32_t cmdsize;
    uint32_t symoff;
    uint32_t nsyms;
    uint32_t stroff;
    uint32_t strsize;
};

/* LC_BUILD_VERSION with no associated build-tool list (ntools=0); sized
 * to 24 bytes so we don't need a variable-length tail. */
struct mh_build_version_command {
    uint32_t cmd;
    uint32_t cmdsize;
    uint32_t platform;
    uint32_t minos;  /* X.Y.Z packed as (X<<16) | (Y<<8) | Z */
    uint32_t sdk;
    uint32_t ntools;
};

struct mh_nlist64 {
    uint32_t n_strx;
    uint8_t  n_type;
    uint8_t  n_sect;
    uint16_t n_desc;
    uint64_t n_value;
};

/* relocation_info: per-section reloc entry. The second word is a bitfield
 * laid out as (low-to-high): r_symbolnum:24, r_pcrel:1, r_length:2,
 * r_extern:1, r_type:4. We pack it manually so we don't depend on the
 * host's bitfield layout. */
struct mh_reloc {
    int32_t  r_address;
    uint32_t r_packed;
};

static uint32_t
macho_pack_reloc(uint32_t r_symbolnum, int r_pcrel, int r_length,
                 int r_extern, int r_type)
{
    return (r_symbolnum & 0x00FFFFFFu)
         | (((uint32_t)(r_pcrel  & 1u))  << 24)
         | (((uint32_t)(r_length & 3u))  << 25)
         | (((uint32_t)(r_extern & 1u))  << 27)
         | (((uint32_t)(r_type   & 0xFu)) << 28);
}

/* Map our generic AsmFixupReloc tag to the per-arch Mach-O reloc type.
 * Returns -1 if the tag has no representation for the given target.
 * Caller drops the fixup. */
static int
macho_reloc_type_for(Target target, AsmFixupReloc tag)
{
    if (target == TASM_ARCH_ARM64) {
        switch (tag) {
            case AFR_AARCH64_CALL26:
            case AFR_AARCH64_JUMP26:   return ARM64_RELOC_BRANCH26;
            case AFR_AARCH64_PAGE21:   return ARM64_RELOC_PAGE21;
            case AFR_AARCH64_PAGEOFF12:return ARM64_RELOC_PAGEOFF12;
            /* BCOND19 has no first-class arm64 Mach-O reloc; ld can't
             * patch a 19-bit signed branch displacement directly. Real
             * code routes b.cond through a near unconditional jump
             * (`b.eq L; b sym; L:`); we silently drop it here. */
            default: return -1;
        }
    }
    if (target == TASM_ARCH_X86_64) {
        switch (tag) {
            case AFR_X86_64_CALL32:
            case AFR_X86_64_JMP32:
            case AFR_X86_64_JCC32:   return X86_64_RELOC_BRANCH;
            case AFR_X86_64_SIGNED:  return X86_64_RELOC_SIGNED;
            default: return -1;
        }
    }
    return -1;
}

/* ================================================================ helpers */

static int
macho_target_supported(Target t)
{
    return t == TASM_ARCH_ARM64 || t == TASM_ARCH_X86_64;
}

static void
macho_cpu_for(Target t, uint32_t *cputype, uint32_t *cpusubtype)
{
    if (t == TASM_ARCH_ARM64) {
        *cputype = CPU_TYPE_ARM64;
        *cpusubtype = CPU_SUBTYPE_ARM64_ALL;
    } else {
        *cputype = CPU_TYPE_X86_64;
        *cpusubtype = CPU_SUBTYPE_X86_64_ALL;
    }
}

/* Set a fixed-size segname/sectname field, zero-padded. */
static void
macho_name(char *dst, const char *src, size_t n)
{
    memset(dst, 0, n);
    size_t k = strlen(src);
    if (k > n) k = n;
    memcpy(dst, src, k);
}

/* String table: starts with a `\0` so offset 0 means "no name". We append
 * NUL-terminated strings and remember the offset returned to callers. */
typedef struct {
    char *buf;
    int len, cap;
} StrTab;

static int
strtab_add(StrTab *st, const char *s)
{
    if (st->len == 0) {
        /* Reserve offset 0 for the leading NUL. */
        if (st->cap == 0) { st->cap = 64; st->buf = xmalloc(st->cap); }
        st->buf[0] = '\0';
        st->len = 1;
    }
    int off = st->len;
    int need = (int)strlen(s) + 1;
    while (st->len + need > st->cap) {
        st->cap = st->cap * 2;
        st->buf = xrealloc(st->buf, st->cap);
    }
    memcpy(st->buf + st->len, s, need);
    st->len += need;
    return off;
}

/* ================================================================ writer */

/* Partition enc->bytes into __text and __data buffers using the line
 * kind from blk. For each label in enc->labels and each fixup in
 * enc->fixups, record which section it now belongs to and its offset
 * within that section. If blk is NULL, everything goes to __text and the
 * remap maps are identity. */
typedef struct {
    int section;        /* 0 = __text, 1 = __data */
    uint32_t new_off;   /* offset within that section */
} Remap;

static void
macho_partition(AsmEnc *enc, AsmBlock *blk,
                uint8_t **text_buf_out, uint32_t *text_sz_out,
                uint8_t **data_buf_out, uint32_t *data_sz_out,
                Remap **label_remap_out, Remap **fixup_remap_out)
{
    uint32_t text_sz = (uint32_t)enc->len;
    uint32_t data_sz = 0;
    uint8_t *text_buf = NULL, *data_buf = NULL;
    Remap *lr = xcalloc((size_t)(enc->n_labels > 0 ? enc->n_labels : 1), sizeof(*lr));
    Remap *fr = xcalloc((size_t)(enc->n_fixups > 0 ? enc->n_fixups : 1), sizeof(*fr));

    if (!blk || !enc->line_offsets || blk->n_lines == 0) {
        /* No block context - identity mapping, everything is __text. */
        if (text_sz) {
            text_buf = xmalloc(text_sz);
            memcpy(text_buf, enc->bytes, text_sz);
        }
        for (int i = 0; i < enc->n_labels; i++) {
            lr[i].section = 0;
            lr[i].new_off = (uint32_t)enc->labels[i].byte_offset;
        }
        for (int i = 0; i < enc->n_fixups; i++) {
            fr[i].section = 0;
            fr[i].new_off = (uint32_t)enc->fixups[i].patch_offset;
        }
        *text_buf_out = text_buf;
        *text_sz_out  = text_sz;
        *data_buf_out = NULL;
        *data_sz_out  = 0;
        *label_remap_out = lr;
        *fixup_remap_out = fr;
        return;
    }

    /* Per-line: which section the line's bytes belong to, and where they
     * land within that section. Label lines inherit the section of the
     * next non-label line (which is what the user wrote `_foo: db ...`
     * intended - the label points into __data). */
    int *line_section = xcalloc((size_t)blk->n_lines, sizeof(*line_section));
    uint32_t *line_new_off = xcalloc((size_t)blk->n_lines, sizeof(*line_new_off));

    int last_section = 0;
    for (int i = blk->n_lines - 1; i >= 0; i--) {
        AsmLineKind k = blk->lines[i].kind;
        if (k == AINS_INSTR)          last_section = 0;
        else if (k == AINS_DIRECTIVE) last_section = 1;
        line_section[i] = last_section;
    }

    /* Forward sweep: compute new_off per line, copy bytes into the right
     * buffer, total sizes. */
    text_sz = 0;
    data_sz = 0;
    for (int i = 0; i < blk->n_lines; i++) {
        if (line_section[i] == 0) text_sz += (uint32_t)
            (enc->line_offsets[i+1] - enc->line_offsets[i]);
        else                       data_sz += (uint32_t)
            (enc->line_offsets[i+1] - enc->line_offsets[i]);
    }
    if (text_sz) text_buf = xmalloc(text_sz);
    if (data_sz) data_buf = xmalloc(data_sz);
    uint32_t to = 0, dao = 0;
    for (int i = 0; i < blk->n_lines; i++) {
        size_t lo = enc->line_offsets[i];
        size_t hi = enc->line_offsets[i+1];
        size_t sz = hi - lo;
        if (line_section[i] == 0) {
            line_new_off[i] = to;
            if (sz) memcpy(text_buf + to, enc->bytes + lo, sz);
            to += (uint32_t)sz;
        } else {
            line_new_off[i] = dao;
            if (sz) memcpy(data_buf + dao, enc->bytes + lo, sz);
            dao += (uint32_t)sz;
        }
    }

    /* Remap labels: a label at byte_offset X was emitted while processing
     * the line whose enc->line_offsets[i] == X (LABEL lines emit 0 bytes,
     * so line_offsets[i] == line_offsets[i+1] == X). Find that line and
     * use its (section, new_off). */
    for (int li = 0; li < enc->n_labels; li++) {
        size_t off = enc->labels[li].byte_offset;
        int found = -1;
        for (int i = 0; i < blk->n_lines; i++) {
            if (enc->line_offsets[i] == off) { found = i; break; }
        }
        if (found < 0) {
            lr[li].section = 0;
            lr[li].new_off = (uint32_t)off;
        } else {
            lr[li].section = line_section[found];
            lr[li].new_off = line_new_off[found];
        }
    }

    /* Remap fixups: locate the line containing the patch byte. */
    for (int fi = 0; fi < enc->n_fixups; fi++) {
        size_t off = enc->fixups[fi].patch_offset;
        int found = -1;
        for (int i = 0; i < blk->n_lines; i++) {
            if (off >= enc->line_offsets[i] && off < enc->line_offsets[i+1]) {
                found = i; break;
            }
        }
        if (found < 0) {
            fr[fi].section = 0;
            fr[fi].new_off = (uint32_t)off;
        } else {
            fr[fi].section = line_section[found];
            fr[fi].new_off = line_new_off[found] +
                             (uint32_t)(off - enc->line_offsets[found]);
        }
    }

    free(line_section);
    free(line_new_off);
    *text_buf_out = text_buf;
    *text_sz_out  = text_sz;
    *data_buf_out = data_buf;
    *data_sz_out  = data_sz;
    *label_remap_out = lr;
    *fixup_remap_out = fr;
}

int
macho_write(AsmEnc *enc, AsmBlock *blk, Target target, FILE *out)
{
    if (!enc || !out || !macho_target_supported(target)) return -1;

    /* ---------- 1. partition bytes into __text / __data ---------- */
    uint8_t *text_buf = NULL, *data_buf = NULL;
    uint32_t text_sz = 0, data_sz = 0;
    Remap *label_remap = NULL, *fixup_remap = NULL;
    macho_partition(enc, blk, &text_buf, &text_sz, &data_buf, &data_sz,
                    &label_remap, &fixup_remap);

    int n_sects = data_sz ? 2 : 1;
    int text_sect_idx = 1;            /* 1-based: __text is section #1 */
    int data_sect_idx = data_sz ? 2 : 0;

    /* ---------- 2. build symbol + string tables ----------
     * Sorted: locals first, then defined externals, then undefined
     * externals (no LC_DYSYMTAB needed). Each defined symbol's n_sect
     * comes from its section assignment in the partition above. */
    int n_labels_named = 0;
    for (int i = 0; i < enc->n_labels; i++)
        if (enc->labels[i].name) n_labels_named++;

    int sym_cap = n_labels_named + enc->n_fixups + 1;
    struct mh_nlist64 *syms = xcalloc((size_t)sym_cap, sizeof(*syms));
    int *fixup_sym_idx = xcalloc((size_t)(enc->n_fixups > 0 ? enc->n_fixups : 1),
                                  sizeof(*fixup_sym_idx));
    StrTab st = { 0 };
    int n_syms = 0;

    /* `n_value` is the symbol's segment-relative virtual address, NOT a
     * section-relative offset. In our single-segment .o, __text sits at
     * addr=0 and __data sits at addr=text_sz, so data symbols need the
     * text-section size folded in. */
    uint64_t section_addr[2] = { 0, text_sz };

    /* Pass 1: local defined symbols (no leading `_`). */
    for (int i = 0; i < enc->n_labels; i++) {
        AsmLabelDef *L = &enc->labels[i];
        if (!L->name || L->name[0] == '_') continue;
        int sect = label_remap[i].section == 1 ? data_sect_idx : text_sect_idx;
        syms[n_syms].n_strx  = (uint32_t)strtab_add(&st, L->name);
        syms[n_syms].n_type  = N_SECT;
        syms[n_syms].n_sect  = (uint8_t)sect;
        syms[n_syms].n_desc  = 0;
        syms[n_syms].n_value = section_addr[label_remap[i].section] +
                               label_remap[i].new_off;
        n_syms++;
    }
    /* Pass 2: external defined symbols (leading `_`). */
    for (int i = 0; i < enc->n_labels; i++) {
        AsmLabelDef *L = &enc->labels[i];
        if (!L->name || L->name[0] != '_') continue;
        int sect = label_remap[i].section == 1 ? data_sect_idx : text_sect_idx;
        syms[n_syms].n_strx  = (uint32_t)strtab_add(&st, L->name);
        syms[n_syms].n_type  = N_SECT | N_EXT;
        syms[n_syms].n_sect  = (uint8_t)sect;
        syms[n_syms].n_desc  = 0;
        syms[n_syms].n_value = section_addr[label_remap[i].section] +
                               label_remap[i].new_off;
        n_syms++;
    }
    /* Pass 3: undefined externals from AF_SYMBOL fixups. */
    for (int i = 0; i < enc->n_fixups; i++) {
        AsmFixup *f = &enc->fixups[i];
        fixup_sym_idx[i] = -1;
        if (f->kind != AF_SYMBOL || !f->sym) continue;
        int idx = -1;
        for (int j = 0; j < n_syms; j++) {
            if (strcmp(st.buf + syms[j].n_strx, f->sym) == 0) { idx = j; break; }
        }
        if (idx < 0) {
            syms[n_syms].n_strx  = (uint32_t)strtab_add(&st, f->sym);
            syms[n_syms].n_type  = N_UNDF | N_EXT;
            syms[n_syms].n_sect  = 0; /* NO_SECT */
            syms[n_syms].n_desc  = 0;
            syms[n_syms].n_value = 0;
            idx = n_syms++;
        }
        fixup_sym_idx[i] = idx;
    }
    while (st.len == 0 || st.len % 8 != 0) {
        if (st.len == st.cap) {
            st.cap = st.cap ? st.cap * 2 : 8;
            st.buf = xrealloc(st.buf, st.cap);
        }
        st.buf[st.len++] = '\0';
    }

    /* ---------- 3. build relocations per section ----------
     *
     * In practice only __text gets relocations (AINS_DIRECTIVE bytes are
     * raw literals with no fixups), but we walk by section flag for
     * generality. */
    struct mh_reloc *text_relocs = xcalloc((size_t)(enc->n_fixups > 0 ? enc->n_fixups : 1),
                                            sizeof *text_relocs);
    struct mh_reloc *data_relocs = xcalloc((size_t)(enc->n_fixups > 0 ? enc->n_fixups : 1),
                                            sizeof *data_relocs);
    int n_text_relocs = 0, n_data_relocs = 0;
    for (int i = 0; i < enc->n_fixups; i++) {
        AsmFixup *f = &enc->fixups[i];
        if (f->kind != AF_SYMBOL || fixup_sym_idx[i] < 0) continue;
        int type = macho_reloc_type_for(target, f->reloc);
        if (type < 0) continue;
        struct mh_reloc *dst = (fixup_remap[i].section == 1) ? data_relocs : text_relocs;
        int *cnt = (fixup_remap[i].section == 1) ? &n_data_relocs : &n_text_relocs;
        dst[*cnt].r_address = (int32_t)fixup_remap[i].new_off;
        dst[*cnt].r_packed  = macho_pack_reloc((uint32_t)fixup_sym_idx[i],
                f->pcrel, /*length*/ 2, /*extern*/ 1, type);
        (*cnt)++;
    }

    /* ---------- 4. compute file layout ----------
     *
     *   header
     *   LC_SEGMENT_64 ( + nsects section_64 entries )
     *   LC_BUILD_VERSION
     *   LC_SYMTAB
     *   __text bytes
     *  [__data bytes]
     *   __text relocations
     *  [__data relocations]
     *   symbol table
     *   string table */
    uint32_t header_sz  = (uint32_t)sizeof(struct mh_header64);
    uint32_t seg_cmdsz  = (uint32_t)(sizeof(struct mh_segment_command64) +
                                     (size_t)n_sects * sizeof(struct mh_section64));
    uint32_t bldver_sz  = (uint32_t)sizeof(struct mh_build_version_command);
    uint32_t symtab_sz  = (uint32_t)sizeof(struct mh_symtab_command);

    uint32_t cmds_end   = header_sz + seg_cmdsz + bldver_sz + symtab_sz;
    uint32_t text_off   = cmds_end;
    uint32_t text_end   = text_off + text_sz;
    uint32_t data_off   = data_sz ? text_end : 0;
    uint32_t data_end   = data_off + data_sz;
    uint32_t section_bytes_end = data_sz ? data_end : text_end;

    uint32_t text_reloc_off = n_text_relocs ?
            ((section_bytes_end + 3u) & ~3u) : 0;
    uint32_t text_reloc_sz  = (uint32_t)(n_text_relocs * sizeof(struct mh_reloc));
    uint32_t after_text_reloc = n_text_relocs ?
            (text_reloc_off + text_reloc_sz) : section_bytes_end;

    uint32_t data_reloc_off = n_data_relocs ?
            ((after_text_reloc + 3u) & ~3u) : 0;
    uint32_t data_reloc_sz  = (uint32_t)(n_data_relocs * sizeof(struct mh_reloc));
    uint32_t after_relocs   = n_data_relocs ?
            (data_reloc_off + data_reloc_sz) : after_text_reloc;

    uint32_t syms_off   = (after_relocs + 7u) & ~7u;
    uint32_t syms_sz    = (uint32_t)(n_syms * sizeof(struct mh_nlist64));
    uint32_t strtab_off = syms_off + syms_sz;
    uint32_t strtab_sz  = (uint32_t)st.len;

    /* ---------- 5. fill structs ---------- */
    uint32_t cputype, cpusubtype;
    macho_cpu_for(target, &cputype, &cpusubtype);

    struct mh_header64 hdr = {
        .magic      = MH_MAGIC_64,
        .cputype    = cputype,
        .cpusubtype = cpusubtype,
        .filetype   = MH_OBJECT,
        .ncmds      = 3,
        .sizeofcmds = seg_cmdsz + bldver_sz + symtab_sz,
        .flags      = 0,
        .reserved   = 0,
    };

    struct mh_segment_command64 seg = {
        .cmd      = LC_SEGMENT_64,
        .cmdsize  = seg_cmdsz,
        .vmaddr   = 0,
        .vmsize   = text_sz + data_sz,
        .fileoff  = text_off,
        .filesize = text_sz + data_sz,
        .maxprot  = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE,
        .initprot = VM_PROT_READ | VM_PROT_WRITE | VM_PROT_EXECUTE,
        .nsects   = (uint32_t)n_sects,
        .flags    = 0,
    };
    macho_name(seg.segname, "", sizeof seg.segname); /* MH_OBJECT: anonymous */

    struct mh_section64 sects[2] = { {0}, {0} };
    {
        struct mh_section64 *s = &sects[0];
        s->addr      = 0;
        s->size      = text_sz;
        s->offset    = text_off;
        s->align     = 2;
        s->reloff    = n_text_relocs ? text_reloc_off : 0;
        s->nreloc    = (uint32_t)n_text_relocs;
        s->flags     = S_REGULAR | S_ATTR_PURE_INSTRUCTIONS |
                       S_ATTR_SOME_INSTRUCTIONS;
        macho_name(s->sectname, "__text", sizeof s->sectname);
        macho_name(s->segname,  "__TEXT", sizeof s->segname);
    }
    if (data_sz) {
        struct mh_section64 *s = &sects[1];
        s->addr      = text_sz; /* lives after __text in our single segment */
        s->size      = data_sz;
        s->offset    = data_off;
        s->align     = 3; /* 2^3 = 8-byte align, conservative */
        s->reloff    = n_data_relocs ? data_reloc_off : 0;
        s->nreloc    = (uint32_t)n_data_relocs;
        s->flags     = S_REGULAR;
        macho_name(s->sectname, "__data", sizeof s->sectname);
        macho_name(s->segname,  "__DATA", sizeof s->segname);
    }

    struct mh_build_version_command bldver = {
        .cmd      = LC_BUILD_VERSION,
        .cmdsize  = bldver_sz,
        .platform = PLATFORM_MACOS,
        .minos    = (11u << 16),   /* 11.0.0 - generous floor */
        .sdk      = (11u << 16),
        .ntools   = 0,
    };

    struct mh_symtab_command symtab = {
        .cmd     = LC_SYMTAB,
        .cmdsize = symtab_sz,
        .symoff  = syms_off,
        .nsyms   = (uint32_t)n_syms,
        .stroff  = strtab_off,
        .strsize = strtab_sz,
    };

    /* ---------- 6. write the file ---------- */
#define WR(p, sz) do { if (fwrite((p), 1, (sz), out) != (size_t)(sz)) goto io_err; } while (0)

    WR(&hdr,    sizeof hdr);
    WR(&seg,    sizeof seg);
    WR(&sects[0], sizeof sects[0]);
    if (data_sz) WR(&sects[1], sizeof sects[1]);
    WR(&bldver, sizeof bldver);
    WR(&symtab, sizeof symtab);

    if (text_sz) WR(text_buf, text_sz);
    if (data_sz) WR(data_buf, data_sz);

    if (n_text_relocs) {
        while ((uint32_t)ftell(out) < text_reloc_off) {
            uint8_t z = 0; WR(&z, 1);
        }
        WR(text_relocs, text_reloc_sz);
    }
    if (n_data_relocs) {
        while ((uint32_t)ftell(out) < data_reloc_off) {
            uint8_t z = 0; WR(&z, 1);
        }
        WR(data_relocs, data_reloc_sz);
    }

    while ((uint32_t)ftell(out) < syms_off) {
        uint8_t z = 0; WR(&z, 1);
    }
    if (syms_sz) WR(syms, syms_sz);

    if (strtab_sz) WR(st.buf, strtab_sz);

#undef WR

    free(text_buf); free(data_buf);
    free(label_remap); free(fixup_remap);
    free(syms); free(text_relocs); free(data_relocs);
    free(fixup_sym_idx); free(st.buf);
    return 0;

io_err:
    free(text_buf); free(data_buf);
    free(label_remap); free(fixup_remap);
    free(syms); free(text_relocs); free(data_relocs);
    free(fixup_sym_idx); free(st.buf);
    return -1;
}
