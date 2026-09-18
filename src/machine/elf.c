#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "elf.h"
#include "tasm_util.h"

/* ================================================================= ELF format
 *
 * Defined inline rather than pulled from <elf.h> so this builds the same
 * way when cross-compiling from a host that has no such header. Field
 * types and order match the canonical System V gABI definitions. */

#define EI_NIDENT   16

#define ET_REL      1
#define EM_X86_64   62
#define EM_AARCH64  183
#define EV_CURRENT  1

#define ELFCLASS64  2
#define ELFDATA2LSB 1

#define SHN_UNDEF   0

#define SHT_NULL     0
#define SHT_PROGBITS 1
#define SHT_SYMTAB   2
#define SHT_STRTAB   3
#define SHT_RELA     4
#define SHT_NOBITS   8

#define SHF_WRITE     0x1
#define SHF_ALLOC     0x2
#define SHF_EXECINSTR 0x4

#define STB_LOCAL   0
#define STB_GLOBAL  1

#define STT_NOTYPE  0
#define STT_OBJECT  1
#define STT_FUNC    2
#define STT_SECTION 3

/* x86_64 relocation types (System V AMD64 psABI). */
#define R_X86_64_64    1
#define R_X86_64_PC32  2
#define R_X86_64_PLT32 4
#define R_X86_64_GOTPCREL 9

/* aarch64 relocation types (ELF for the Arm 64-bit Architecture). */
#define R_AARCH64_ABS64             257
#define R_AARCH64_ADR_PREL_PG_HI21  275
#define R_AARCH64_ADD_ABS_LO12_NC   277
#define R_AARCH64_CONDBR19          280
#define R_AARCH64_JUMP26            282
#define R_AARCH64_CALL26            283
#define R_AARCH64_ADR_GOT_PAGE      311
#define R_AARCH64_LD64_GOT_LO12_NC  312

struct elf64_ehdr {
    uint8_t  e_ident[EI_NIDENT];
    uint16_t e_type;
    uint16_t e_machine;
    uint32_t e_version;
    uint64_t e_entry;
    uint64_t e_phoff;
    uint64_t e_shoff;
    uint32_t e_flags;
    uint16_t e_ehsize;
    uint16_t e_phentsize;
    uint16_t e_phnum;
    uint16_t e_shentsize;
    uint16_t e_shnum;
    uint16_t e_shstrndx;
};

struct elf64_shdr {
    uint32_t sh_name;
    uint32_t sh_type;
    uint64_t sh_flags;
    uint64_t sh_addr;
    uint64_t sh_offset;
    uint64_t sh_size;
    uint32_t sh_link;
    uint32_t sh_info;
    uint64_t sh_addralign;
    uint64_t sh_entsize;
};

struct elf64_sym {
    uint32_t st_name;
    uint8_t  st_info;
    uint8_t  st_other;
    uint16_t st_shndx;
    uint64_t st_value;
    uint64_t st_size;
};

struct elf64_rela {
    uint64_t r_offset;
    uint64_t r_info;
    int64_t  r_addend;
};

static uint8_t
elf_sym_info(int bind, int type)
{
    return (uint8_t)((bind << 4) | (type & 0xF));
}

static uint64_t
elf_rela_info(uint32_t sym, uint32_t type)
{
    return ((uint64_t)sym << 32) | (uint64_t)type;
}

/* Map our generic AsmFixupReloc tag to an ELF relocation type for
 * `target`. Returns -1 when the tag has no representation there, and
 * the caller drops the fixup with a warning.
 *
 * Branches use PLT32 rather than PC32 on x86_64. The two are
 * interchangeable for a target the linker can reach directly, and PLT32
 * additionally lets it route the call via a stub when it cannot - which
 * is what a call to a shared-library function needs. It is what gcc and
 * clang emit for exactly this reason. AArch64's CALL26 covers both
 * cases on its own; the linker inserts a stub when the target is out of
 * +/-128MB without needing a different relocation to be told to. */
static int
elf_reloc_type_for(Target target, AsmFixupReloc tag)
{
    if (tag == AFR_ABS64) {
        return target == TASM_ARCH_ARM64 ? R_AARCH64_ABS64 : R_X86_64_64;
    }
    if (target == TASM_ARCH_ARM64) {
        switch (tag) {
            case AFR_AARCH64_CALL26:   return R_AARCH64_CALL26;
            case AFR_AARCH64_JUMP26:   return R_AARCH64_JUMP26;
            case AFR_AARCH64_BCOND19:  return R_AARCH64_CONDBR19;
            case AFR_AARCH64_PAGE21:   return R_AARCH64_ADR_PREL_PG_HI21;
            /* Both low-12 forms exist because ELF names the relocation
             * after the INSTRUCTION it patches, where Mach-O has one
             * PAGEOFF12 for either. The backend only ever pairs a plain
             * `:lo12:` with an ADD and a `:got_lo12:` with a 64-bit LDR
             * (jitGlobalAddrEx), so the mapping is one-to-one. */
            case AFR_AARCH64_PAGEOFF12:return R_AARCH64_ADD_ABS_LO12_NC;
            /* A symbol this module does not define: the linker puts its
             * address in a GOT slot and these two address the slot,
             * rather than the symbol, which is the only thing that
             * works when it binds elsewhere. */
            case AFR_AARCH64_GOT_PAGE21:    return R_AARCH64_ADR_GOT_PAGE;
            case AFR_AARCH64_GOT_PAGEOFF12: return R_AARCH64_LD64_GOT_LO12_NC;
            default:                   return -1;
        }
    }
    switch (tag) {
        case AFR_X86_64_CALL32:
        case AFR_X86_64_JMP32:
        case AFR_X86_64_JCC32:  return R_X86_64_PLT32;
        case AFR_X86_64_SIGNED: return R_X86_64_PC32;
        case AFR_X86_64_GOTPCREL: return R_X86_64_GOTPCREL;
        default:                return -1;
    }
}

/* The addend for a RELA entry.
 *
 * The two architectures disagree about where a PC-relative displacement
 * is measured FROM, and the relocation has to make up the difference.
 * The linker computes S + A - P with P at the start of the field being
 * patched:
 *
 *   x86_64    the displacement is relative to the END of the
 *             instruction, and the field is always its last `width`
 *             bytes - so P is `width` bytes short and A = -width.
 *
 *   aarch64   the relocation applies to the whole instruction word and
 *             is measured from the instruction's own address, which is
 *             already P. A = 0.
 *
 * An absolute relocation has no PC term at all, so nothing to correct.
 *
 * Getting this wrong still links; it just jumps somewhere else. */
static int64_t
elf_addend_for(Target target, const AsmFixup *f)
{
    if (!f->pcrel) return 0;
    if (target == TASM_ARCH_ARM64) return 0;
    return -(int64_t)f->width;
}

/* ================================================================== helpers */

/* String table. Offset 0 is the empty string, which is what a symbol
 * with no name and a section header with no name both point at. */
typedef struct {
    char *buf;
    int len, cap;
} StrTab;

static void
strtab_init(StrTab *st)
{
    st->cap = 64;
    st->buf = xmalloc((size_t)st->cap);
    st->buf[0] = '\0';
    st->len = 1;
}

static int
strtab_add(StrTab *st, const char *s)
{
    int off = st->len;
    int need = (int)strlen(s) + 1;
    while (st->len + need > st->cap) {
        st->cap *= 2;
        st->buf = xrealloc(st->buf, (size_t)st->cap);
    }
    memcpy(st->buf + st->len, s, (size_t)need);
    st->len += need;
    return off;
}

/* Where a label or fixup ended up once the bytes were split in two. */
typedef struct {
    int section;      /* 0 = .text, 1 = .data */
    uint32_t new_off; /* offset within that section */
} Remap;

/* A symbol we are going to write, before it is ordered and indexed. */
typedef struct {
    const char *name;
    int section;   /* 0 = .text, 1 = .data, -1 = undefined */
    uint64_t value;
    int bind;
    int type;
} ElfSym;

typedef struct {
    ElfSym *v;
    int n, cap;
} SymVec;

static void
symvec_push(SymVec *sv, ElfSym s)
{
    if (sv->n == sv->cap) {
        sv->cap = sv->cap ? sv->cap * 2 : 16;
        sv->v = xrealloc(sv->v, (size_t)sv->cap * sizeof(*sv->v));
    }
    sv->v[sv->n++] = s;
}

static int
symvec_find(SymVec *sv, const char *name)
{
    if (!name) return -1;
    for (int i = 0; i < sv->n; i++) {
        if (sv->v[i].name && !strcmp(sv->v[i].name, name)) return i;
    }
    return -1;
}

/* ================================================================ partition
 *
 * Split enc->bytes into .text and .data using the line kinds from `blk`,
 * and record where every label and fixup moved to. With no block context
 * everything is code and the mapping is the identity.
 *
 * A LABEL line emits no bytes, so it takes the section of the next line
 * that does - which is what `msg: db "hi"` means. */
static void
elf_partition(AsmEnc *enc, AsmBlock *blk,
              uint8_t **text_out, uint32_t *text_sz_out,
              uint8_t **data_out, uint32_t *data_sz_out,
              Remap **label_remap_out, Remap **fixup_remap_out)
{
    Remap *lr = xcalloc((size_t)(enc->n_labels > 0 ? enc->n_labels : 1),
                        sizeof(*lr));
    Remap *fr = xcalloc((size_t)(enc->n_fixups > 0 ? enc->n_fixups : 1),
                        sizeof(*fr));
    uint8_t *text_buf = NULL, *data_buf = NULL;
    uint32_t text_sz = 0, data_sz = 0;

    /* An explicit split wins: the producer emitted code then data into
     * one stream and knows exactly where the boundary is, which is more
     * than the line kinds can say. */
    if (enc->data_split >= 0) {
        uint32_t split = (uint32_t)enc->data_split;
        if (split > enc->len) split = (uint32_t)enc->len;
        text_sz = split;
        data_sz = (uint32_t)enc->len - split;
        if (text_sz) {
            text_buf = xmalloc(text_sz);
            memcpy(text_buf, enc->bytes, text_sz);
        }
        if (data_sz) {
            data_buf = xmalloc(data_sz);
            memcpy(data_buf, enc->bytes + split, data_sz);
        }
        for (int i = 0; i < enc->n_labels; i++) {
            size_t o = enc->labels[i].byte_offset;
            lr[i].section = o >= split ? 1 : 0;
            lr[i].new_off = (uint32_t)(o >= split ? o - split : o);
        }
        for (int i = 0; i < enc->n_fixups; i++) {
            size_t o = enc->fixups[i].patch_offset;
            fr[i].section = o >= split ? 1 : 0;
            fr[i].new_off = (uint32_t)(o >= split ? o - split : o);
        }
        goto done;
    }

    if (!blk || !enc->line_offsets || blk->n_lines == 0) {
        text_sz = (uint32_t)enc->len;
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
        goto done;
    }

    {
        int *line_section = xcalloc((size_t)blk->n_lines,
                                    sizeof(*line_section));
        uint32_t *line_new_off = xcalloc((size_t)blk->n_lines,
                                         sizeof(*line_new_off));
        int last_section = 0;

        /* Backwards, so a run of labels inherits the section of the
         * first line after them that actually emits something. */
        for (int i = blk->n_lines - 1; i >= 0; i--) {
            AsmLineKind k = blk->lines[i].kind;
            if (k == AINS_INSTR)          last_section = 0;
            else if (k == AINS_DIRECTIVE) last_section = 1;
            line_section[i] = last_section;
        }

        for (int i = 0; i < blk->n_lines; i++) {
            uint32_t sz = (uint32_t)(enc->line_offsets[i+1] -
                                     enc->line_offsets[i]);
            if (line_section[i] == 0) text_sz += sz;
            else                      data_sz += sz;
        }
        if (text_sz) text_buf = xmalloc(text_sz);
        if (data_sz) data_buf = xmalloc(data_sz);

        uint32_t to = 0, dao = 0;
        for (int i = 0; i < blk->n_lines; i++) {
            size_t lo = enc->line_offsets[i];
            size_t sz = enc->line_offsets[i+1] - lo;
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

        /* A label sits at the byte offset the line it precedes starts
         * at; find that line and take its placement. */
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

        /* A fixup sits INSIDE a line's bytes, so find the line whose
         * range contains it and carry the offset within that line. */
        for (int fi = 0; fi < enc->n_fixups; fi++) {
            size_t off = enc->fixups[fi].patch_offset;
            int found = -1;
            for (int i = 0; i < blk->n_lines; i++) {
                if (off >= enc->line_offsets[i] &&
                    off <  enc->line_offsets[i+1]) { found = i; break; }
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

        xfree(line_section);
        xfree(line_new_off);
    }

done:
    *text_out = text_buf;
    *text_sz_out = text_sz;
    *data_out = data_buf;
    *data_sz_out = data_sz;
    *label_remap_out = lr;
    *fixup_remap_out = fr;
}

/* =================================================================== writer */

/* Pad `out` to `align` and return the new offset. */
static uint64_t
elf_align_to(FILE *out, uint64_t off, uint64_t align)
{
    static const uint8_t zeros[16] = { 0 };
    uint64_t rem = align ? off % align : 0;
    if (rem == 0) return off;
    uint64_t pad = align - rem;
    while (pad > 0) {
        uint64_t n = pad > sizeof(zeros) ? sizeof(zeros) : pad;
        if (fwrite(zeros, 1, (size_t)n, out) != n) return off;
        off += n;
        pad -= n;
    }
    return off;
}

int
elf_write(AsmEnc *enc, AsmBlock *blk, Target target, FILE *out)
{
    if (target != TASM_ARCH_X86_64 && target != TASM_ARCH_ARM64) {
        fprintf(stderr, "hcc: elf_write: unknown target\n");
        return -1;
    }

    uint8_t *text_buf, *data_buf;
    uint32_t text_sz, data_sz;
    Remap *lr, *fr;
    elf_partition(enc, blk, &text_buf, &text_sz, &data_buf, &data_sz,
                  &lr, &fr);

    int have_data = data_sz > 0;

    /* ---- section numbering -------------------------------------------
     * Fixed up front so symbols can name their section while they are
     * being built. Indices that do not exist stay 0, which is SHN_UNDEF
     * and never referenced. */
    int sh_null = 0, sh_text = 1;
    int sh_rela_text = 0, sh_data = 0, sh_rela_data = 0;
    int next = 2;

    /* Relocations are only known after the symbol table is built, so
     * count them first - the section numbering has to be settled before
     * symbols can reference it. */
    int n_rel_text = 0, n_rel_data = 0;
    for (int i = 0; i < enc->n_fixups; i++) {
        AsmFixup *f = &enc->fixups[i];
        if (f->kind != AF_SYMBOL) continue;      /* AF_LOCAL already patched */
        if (elf_reloc_type_for(target, f->reloc) < 0) continue;
        if (fr[i].section == 0) n_rel_text++;
        else                    n_rel_data++;
    }

    if (n_rel_text)          sh_rela_text = next++;
    if (have_data)           sh_data      = next++;
    if (have_data && n_rel_data) sh_rela_data = next++;
    int sh_symtab   = next++;
    int sh_strtab   = next++;
    int sh_shstrtab = next++;
    int n_sections  = next;
    (void)sh_null;

    /* ---- symbols ------------------------------------------------------
     * ELF requires every STB_LOCAL symbol to precede every STB_GLOBAL
     * one, and sh_info to name the first global. Locals are collected
     * first for that reason, not for tidiness.
     *
     * A section symbol per output section: a relocation against a local
     * label is expressed as "this section, plus an addend", so the
     * section itself has to be nameable. */
    SymVec syms = { 0 };
    StrTab strtab;
    strtab_init(&strtab);

    symvec_push(&syms, (ElfSym){ NULL, -1, 0, STB_LOCAL, STT_NOTYPE });
    symvec_push(&syms, (ElfSym){ NULL, 0, 0, STB_LOCAL, STT_SECTION });
    if (have_data) {
        symvec_push(&syms, (ElfSym){ NULL, 1, 0, STB_LOCAL, STT_SECTION });
    }
    /* Every named label becomes a defined global. A local label
     * (local_num >= 0) is block-internal and was resolved by the
     * encoder, so it needs no symbol. */
    /* Two passes, because ELF requires every local to precede every
     * global and a `static` function is now a named LOCAL. One pass
     * would interleave them and sh_info could no longer name the first
     * global. */
    int first_global = 0;
    for (int pass = 0; pass < 2; pass++) {
        int want_local = (pass == 0);
        /* Taken between the passes, not before them: the section
         * symbols above are no longer the only locals, so counting
         * them alone left sh_info pointing at the first named LOCAL -
         * `.symtab local symbol at index N (>= sh_info of N)`, and ld
         * rejected every object with a `static` function in it. */
        if (!want_local) first_global = syms.n;
        for (int i = 0; i < enc->n_labels; i++) {
            AsmLabelDef *l = &enc->labels[i];
            if (!l->name) continue;
            if ((l->is_local != 0) != want_local) continue;
            if (symvec_find(&syms, l->name) >= 0) continue;
            int sect = lr[i].section;
            symvec_push(&syms, (ElfSym){
                l->name, sect, lr[i].new_off,
                want_local ? STB_LOCAL : STB_GLOBAL,
                sect == 0 ? STT_FUNC : STT_OBJECT,
            });
        }
    }

    /* Anything a fixup names and no label defined is external: an
     * undefined global for the linker to resolve. */
    for (int i = 0; i < enc->n_fixups; i++) {
        AsmFixup *f = &enc->fixups[i];
        if (f->kind != AF_SYMBOL || !f->sym) continue;
        if (elf_reloc_type_for(target, f->reloc) < 0) continue;
        if (symvec_find(&syms, f->sym) >= 0) continue;
        symvec_push(&syms, (ElfSym){
            f->sym, -1, 0, STB_GLOBAL, STT_NOTYPE,
        });
    }

    /* ---- relocations --------------------------------------------------
     * RELA, so the addend lives in the entry rather than in the
     * instruction. Every x86_64 displacement we emit is measured from
     * the END of the instruction and the field is always its last four
     * bytes, so the addend is -4: the linker computes S + A - P with P
     * at the start of the field. */
    struct elf64_rela *rel_text = n_rel_text
        ? xcalloc((size_t)n_rel_text, sizeof(*rel_text)) : NULL;
    struct elf64_rela *rel_data = n_rel_data
        ? xcalloc((size_t)n_rel_data, sizeof(*rel_data)) : NULL;
    int rt = 0, rd = 0;

    for (int i = 0; i < enc->n_fixups; i++) {
        AsmFixup *f = &enc->fixups[i];
        if (f->kind != AF_SYMBOL) continue;
        int type = elf_reloc_type_for(target, f->reloc);
        if (type < 0) {
            fprintf(stderr,
                    "hcc: elf_write: dropping unrepresentable fixup for '%s'\n",
                    f->sym ? f->sym : "<local>");
            continue;
        }
        int si = symvec_find(&syms, f->sym);
        if (si < 0) continue;   /* counted above, so cannot happen */

        struct elf64_rela r;
        r.r_offset = fr[i].new_off;
        r.r_info   = elf_rela_info((uint32_t)si, (uint32_t)type);
        r.r_addend = elf_addend_for(target, f);
        if (fr[i].section == 0) rel_text[rt++] = r;
        else                    rel_data[rd++] = r;
    }

    /* ---- section name table ------------------------------------------- */
    StrTab shstr;
    strtab_init(&shstr);
    int nm_text     = strtab_add(&shstr, ".text");
    int nm_rela_text= n_rel_text ? strtab_add(&shstr, ".rela.text") : 0;
    int nm_data     = have_data  ? strtab_add(&shstr, ".data") : 0;
    int nm_rela_data= (have_data && n_rel_data)
                      ? strtab_add(&shstr, ".rela.data") : 0;
    int nm_symtab   = strtab_add(&shstr, ".symtab");
    int nm_strtab   = strtab_add(&shstr, ".strtab");
    int nm_shstrtab = strtab_add(&shstr, ".shstrtab");

    /* Symbol names go into .strtab now that its content is final. */
    struct elf64_sym *esyms = xcalloc((size_t)syms.n, sizeof(*esyms));
    for (int i = 0; i < syms.n; i++) {
        ElfSym *s = &syms.v[i];
        esyms[i].st_name  = s->name ? (uint32_t)strtab_add(&strtab, s->name) : 0;
        esyms[i].st_info  = elf_sym_info(s->bind, s->type);
        esyms[i].st_other = 0;
        esyms[i].st_shndx = s->section < 0
                          ? SHN_UNDEF
                          : (uint16_t)(s->section == 0 ? sh_text : sh_data);
        esyms[i].st_value = s->value;
        esyms[i].st_size  = 0;
    }

    /* ---- write it out -------------------------------------------------
     * Contents first, headers last, so every sh_offset is known by the
     * time the header table is written. */
    struct elf64_ehdr eh;
    memset(&eh, 0, sizeof(eh));
    eh.e_ident[0] = 0x7F; eh.e_ident[1] = 'E';
    eh.e_ident[2] = 'L';  eh.e_ident[3] = 'F';
    eh.e_ident[4] = ELFCLASS64;
    eh.e_ident[5] = ELFDATA2LSB;
    eh.e_ident[6] = EV_CURRENT;
    eh.e_type      = ET_REL;
    eh.e_machine   = target == TASM_ARCH_ARM64 ? EM_AARCH64 : EM_X86_64;
    eh.e_version   = EV_CURRENT;
    eh.e_ehsize    = (uint16_t)sizeof(eh);
    eh.e_shentsize = (uint16_t)sizeof(struct elf64_shdr);
    eh.e_shnum     = (uint16_t)n_sections;
    eh.e_shstrndx  = (uint16_t)sh_shstrtab;

    uint64_t off = sizeof(eh);
    if (fwrite(&eh, 1, sizeof(eh), out) != sizeof(eh)) return -1;

    uint64_t text_off = off;
    if (text_sz && fwrite(text_buf, 1, text_sz, out) != text_sz) return -1;
    off += text_sz;

    uint64_t data_off = off;
    if (data_sz && fwrite(data_buf, 1, data_sz, out) != data_sz) return -1;
    off += data_sz;

    off = elf_align_to(out, off, 8);
    uint64_t rela_text_off = off;
    if (n_rel_text) {
        size_t n = (size_t)n_rel_text * sizeof(*rel_text);
        if (fwrite(rel_text, 1, n, out) != n) return -1;
        off += n;
    }

    uint64_t rela_data_off = off;
    if (n_rel_data) {
        size_t n = (size_t)n_rel_data * sizeof(*rel_data);
        if (fwrite(rel_data, 1, n, out) != n) return -1;
        off += n;
    }

    off = elf_align_to(out, off, 8);
    uint64_t symtab_off = off;
    {
        size_t n = (size_t)syms.n * sizeof(*esyms);
        if (fwrite(esyms, 1, n, out) != n) return -1;
        off += n;
    }

    uint64_t strtab_off = off;
    if (fwrite(strtab.buf, 1, (size_t)strtab.len, out) != (size_t)strtab.len)
        return -1;
    off += (uint64_t)strtab.len;

    uint64_t shstrtab_off = off;
    if (fwrite(shstr.buf, 1, (size_t)shstr.len, out) != (size_t)shstr.len)
        return -1;
    off += (uint64_t)shstr.len;

    off = elf_align_to(out, off, 8);
    uint64_t shoff = off;

    struct elf64_shdr *sh = xcalloc((size_t)n_sections, sizeof(*sh));

    sh[sh_text].sh_name      = (uint32_t)nm_text;
    sh[sh_text].sh_type      = SHT_PROGBITS;
    sh[sh_text].sh_flags     = SHF_ALLOC | SHF_EXECINSTR;
    sh[sh_text].sh_offset    = text_off;
    sh[sh_text].sh_size      = text_sz;
    /* aarch64 instructions are all four bytes; x86_64 entries are
     * conventionally 16-byte aligned for the I-cache fetch line. */
    sh[sh_text].sh_addralign = target == TASM_ARCH_ARM64 ? 4 : 16;

    if (n_rel_text) {
        sh[sh_rela_text].sh_name      = (uint32_t)nm_rela_text;
        sh[sh_rela_text].sh_type      = SHT_RELA;
        sh[sh_rela_text].sh_offset    = rela_text_off;
        sh[sh_rela_text].sh_size      = (uint64_t)n_rel_text * sizeof(*rel_text);
        sh[sh_rela_text].sh_link      = (uint32_t)sh_symtab;
        sh[sh_rela_text].sh_info      = (uint32_t)sh_text;
        sh[sh_rela_text].sh_addralign = 8;
        sh[sh_rela_text].sh_entsize   = sizeof(*rel_text);
    }

    if (have_data) {
        sh[sh_data].sh_name      = (uint32_t)nm_data;
        sh[sh_data].sh_type      = SHT_PROGBITS;
        sh[sh_data].sh_flags     = SHF_ALLOC | SHF_WRITE;
        sh[sh_data].sh_offset    = data_off;
        sh[sh_data].sh_size      = data_sz;
        sh[sh_data].sh_addralign = 8;
    }

    if (have_data && n_rel_data) {
        sh[sh_rela_data].sh_name      = (uint32_t)nm_rela_data;
        sh[sh_rela_data].sh_type      = SHT_RELA;
        sh[sh_rela_data].sh_offset    = rela_data_off;
        sh[sh_rela_data].sh_size      = (uint64_t)n_rel_data * sizeof(*rel_data);
        sh[sh_rela_data].sh_link      = (uint32_t)sh_symtab;
        sh[sh_rela_data].sh_info      = (uint32_t)sh_data;
        sh[sh_rela_data].sh_addralign = 8;
        sh[sh_rela_data].sh_entsize   = sizeof(*rel_data);
    }

    sh[sh_symtab].sh_name      = (uint32_t)nm_symtab;
    sh[sh_symtab].sh_type      = SHT_SYMTAB;
    sh[sh_symtab].sh_offset    = symtab_off;
    sh[sh_symtab].sh_size      = (uint64_t)syms.n * sizeof(*esyms);
    sh[sh_symtab].sh_link      = (uint32_t)sh_strtab;
    sh[sh_symtab].sh_info      = (uint32_t)first_global;
    sh[sh_symtab].sh_addralign = 8;
    sh[sh_symtab].sh_entsize   = sizeof(*esyms);

    sh[sh_strtab].sh_name      = (uint32_t)nm_strtab;
    sh[sh_strtab].sh_type      = SHT_STRTAB;
    sh[sh_strtab].sh_offset    = strtab_off;
    sh[sh_strtab].sh_size      = (uint64_t)strtab.len;
    sh[sh_strtab].sh_addralign = 1;

    sh[sh_shstrtab].sh_name      = (uint32_t)nm_shstrtab;
    sh[sh_shstrtab].sh_type      = SHT_STRTAB;
    sh[sh_shstrtab].sh_offset    = shstrtab_off;
    sh[sh_shstrtab].sh_size      = (uint64_t)shstr.len;
    sh[sh_shstrtab].sh_addralign = 1;

    {
        size_t n = (size_t)n_sections * sizeof(*sh);
        if (fwrite(sh, 1, n, out) != n) return -1;
    }

    /* e_shoff was not known when the header went out; go back for it. */
    if (fseek(out, 0, SEEK_SET) != 0) return -1;
    eh.e_shoff = shoff;
    if (fwrite(&eh, 1, sizeof(eh), out) != sizeof(eh)) return -1;
    if (fseek(out, 0, SEEK_END) != 0) return -1;

    xfree(text_buf);
    xfree(data_buf);
    xfree(lr);
    xfree(fr);
    xfree(rel_text);
    xfree(rel_data);
    xfree(esyms);
    xfree(sh);
    xfree(syms.v);
    xfree(strtab.buf);
    xfree(shstr.buf);
    return 0;
}
