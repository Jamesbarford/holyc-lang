#ifndef HCC_ASM_ENC_H__
#define HCC_ASM_ENC_H__

/* Houses the architecture agnostic parts of the instruction encoders */

#include <stddef.h>
#include <stdint.h>

#include "asm.h"

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

    int errors;
    const char *src_file; /* borrowed for diagnostics */

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
