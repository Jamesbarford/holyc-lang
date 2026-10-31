#ifndef HCC_ASM_ENC_H__
#define HCC_ASM_ENC_H__

/* Houses the architecture agnostic parts of the instruction encoders */

#include <stddef.h>
#include <stdint.h>

#include "asm.h"
#include "memory.h"

/* Encode an AsmBlock into machine code bytes + a list of label definitions
 * and fixups. The fixups carry information the codegen layer turns into
 * proper Mach-O symbols and relocations. */

typedef enum {
    AF_LOCAL,        /* @@N - resolved entirely within this block */
    AF_SYMBOL,       /* external symbol name */
    AF_CLASS_MEMBER, /* Class.field offset - needs class registry */
} AsmFixupKind;

/* What relocation the patched instruction expects. Set by the encoder at
 * fixup-creation time so consumers don't have to re-decode the placeholder
 * bytes to figure out what to emit. AFR_NONE = unspecified (consumer should
 * fall back to whatever it did before tags existed). */
typedef enum {
    AFR_NONE = 0,
    AFR_AARCH64_CALL26,    /* BL <sym>     - 26-bit branch-with-link */
    AFR_AARCH64_JUMP26,    /* B  <sym>     - 26-bit branch */
    AFR_AARCH64_BCOND19,   /* B.cc <sym>   - 19-bit conditional branch */
    AFR_AARCH64_PAGE21,    /* ADRP Xd,<sym> - page-relative high 21 bits */
    AFR_AARCH64_PAGEOFF12, /* ADD/LDR low 12 bits of @PAGEOFF */
    AFR_X86_64_CALL32,  /* CALL <sym>   - E8 + rel32 */
    AFR_X86_64_JMP32,   /* JMP  <sym>   - E9 + rel32 */
    AFR_X86_64_JCC32,   /* Jcc  <sym>   - 0F 8x + rel32 */
    AFR_X86_64_SIGNED,  /* LEA / MOV [rip+sym] - RIP-rel disp32, non-branch */
    /* Absolute 64-bit address of a symbol, stored as data rather than
     * folded into an instruction: a global initialised with the address
     * of another global, or of a function. Deliberately NOT arch-tagged
     * - the encoding is eight little-endian bytes on both targets, and
     * every object format has a relocation that means exactly this
     * (R_X86_64_64, R_AARCH64_ABS64, *_RELOC_UNSIGNED). */
    AFR_ABS64,
    /* MOV reg, [rip+sym@GOTPCREL] - the address of a symbol this module
     * does NOT define, read out of the GOT slot the linker allocates.
     * Distinct from AFR_X86_64_SIGNED because it names a different
     * INSTRUCTION (a load, not an lea) as well as a different
     * relocation: computing a PC-relative address only works when the
     * target is in this module. */
    AFR_X86_64_GOTPCREL,
    /* The aarch64 twins: ADRP of a GOT slot's page and the LDR that
     * reads the address out of it (`sym@GOTPAGE`/`sym@GOTPAGEOFF` on
     * Darwin, `:got:`/`:got_lo12:` on ELF). Same reasoning as
     * GOTPCREL: a symbol from another image cannot be reached by
     * adrp+add, only loaded from the GOT. */
    AFR_AARCH64_GOT_PAGE21,
    AFR_AARCH64_GOT_PAGEOFF12,
} AsmFixupReloc;

typedef struct {
    AsmFixupKind kind;
    AsmFixupReloc reloc; /* see AsmFixupReloc above */
    size_t patch_offset; /* byte offset in AsmEnc.bytes to patch */
    int width;           /* 1, 2, 4 bytes */
    int pcrel;           /* 1 = PC-relative branch/disp */
    int addend_bytes;    /* bytes after patch (for PC-rel base) */
    /* For AF_LOCAL: target local label number. */
    int local_num;
    /* For AF_SYMBOL: name (owned). */
    char *sym;
    /* For AF_CLASS_MEMBER: class and member names (owned). */
    char *cls_name;
    char *member_name;
} AsmFixup;

typedef struct {
    /* Either local (local_num >= 0, name == NULL) or public (local_num < 0,
     * name set). */
    int local_num;
    char *name;
    size_t byte_offset;
    /* A NAMED label that must not be exported: a `static` function has
     * internal linkage, so two translation units may each define one
     * privately and two global symbols would collide at link time.
     * Distinct from `local_num`, which is a numbered block-internal
     * label the encoder resolves and never emits a symbol for. */
    int is_local;
} AsmLabelDef;

/* AsmErrHandler / asm_error_handler are now declared in asm.h so both the
 * parser and the encoder can share the same error-sink type. */

typedef struct AsmEnc {
    uint8_t *bytes;
    size_t len, cap;

    AsmLabelDef *labels;
    int n_labels, cap_labels;

    AsmFixup *fixups;
    int n_fixups, cap_fixups;

     /* Per-AsmLine byte ranges.  line_offsets is sized n_lines+1; the bytes
     * produced by lines[i] span [line_offsets[i], line_offsets[i+1]).  Used
     * by callers that need to interleave the source asm text with the
     * encoded bytes (e.g. -S output).  May be NULL if the encoder didn't
     * populate it. */
    size_t *line_offsets;
    int n_line_offsets;

    /* Byte offset at which DATA begins; everything below it is code.
     * -1 means the stream is all code, which is what the assembler
     * produces (it describes its own sections through AsmBlock line
     * kinds instead).
     *
     * Set by a producer that emits code and data into one stream and
     * has no AsmBlock to describe them - the compiler, which appends
     * its globals and string literals after the last function. An
     * object writer prefers this over the line kinds when it is set. */
    long data_split;

    int errors;
    const char *src_file; /* borrowed for diagnostics */

    MemCtx mem;

    AsmErrHandler err_handler;
} AsmEnc;

void asm_enc_init(AsmEnc *enc);

void asm_enc_set_error_handler(AsmEnc *enc,
                               void *errctx,
                               asm_error_handler *handler);

/* Resolver callbacks used while encoding. NULL = no resolution; encoder
 * emits fixups that the codegen layer is expected to handle later. */
typedef struct AsmResolver {
    int (*find_class_member)(void *ud, const char *cls, const char *member,
            int64_t *offset_out);
    int (*find_macro)(void *ud, const char *name, int64_t *value_out);
    void *ud;
} AsmResolver;

void asm_err_at(AsmEnc *e, AsmLine *ln, const char *msg);
char *asm_tmp_printf(const char *fmt, ...);

/* Target-agnostic data directive: db/du8 / dw/du16 / dd/du32 / dq/du64.
 * Emits raw little-endian bytes for immediate operands; for label
 * operands emits zeros and records an AF_LOCAL / AF_SYMBOL fixup so the
 * codegen (or Mach-O writer) can resolve it later. */
void asm_enc_directive(AsmEnc *e, AsmLine *ln);

void put_byte(AsmEnc *e, uint8_t b);
void put_u16(AsmEnc *e, uint16_t v);
void put_u32(AsmEnc *e, uint32_t v);
size_t put_word(AsmEnc *e, uint32_t w);
void put_u64(AsmEnc *e, uint64_t v);
void put_zero_n(AsmEnc *e, int n);
void asm_define_label(AsmEnc *e, int local_num, const char *name);
/* Mark an already-defined named label as internal. Idempotent, and a
 * no-op when the name is not found. */
void asm_mark_label_local(AsmEnc *e, const char *name);
void asm_add_fixup(AsmEnc *e, AsmFixup f);
int asm_operand_local_num(AsmOperand *o);

/* Dispatch out to either AArch64 or x86_64 */
int asm_encode(AsmBlock *blk, AsmResolver *r, AsmEnc *out);
int asm_parse_and_encode(AsmEnc *out,
                         AsmResolver *r,
                         Target target,
                         char *asm_code,
                         int len,
                         const char *src_file,
                         int src_line,
                         void *err_ctx,
                         asm_error_handler *err_handler);

int asm_is_reg(AsmOperand *o);
int asm_is_imm(AsmOperand *o);
int asm_is_mem(AsmOperand *o);
int asm_is_label(AsmOperand *o);
int asm_eq(const char *a, const char *b);

void asm_enc_free(AsmEnc *enc);

#endif
