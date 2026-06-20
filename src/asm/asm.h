#ifndef HCC_ASM_H
#define HCC_ASM_H

/* HolyC inline asm subparser + encoder. Lives between the lexer (which
 * captures the asm body bytes as a single token) and the IR emitter
 * (which lowers parsed asm to byte blobs). */

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "target.h"

typedef enum {
    AOP_NONE = 0,
    AOP_REG,   /* register operand */
    AOP_IMM,   /* immediate constant */
    AOP_MEM,   /* memory reference (with optional displacement) */
    AOP_LABEL, /* symbolic label, resolved later */
    AOP_SHIFT, /* ARM64 4-operand shift suffix: LSL/LSR/ASR/ROR #N */
} AsmOperandKind;

/* ARM64 shift type for AOP_SHIFT operands. */
enum {
    ASHIFT_LSL = 0,
    ASHIFT_LSR = 1,
    ASHIFT_ASR = 2,
    ASHIFT_ROR = 3,
};

/* Register identifier - we encode both kind and hardware number. */
typedef enum {
    AR_GPR8 = 0, /* AL/CL/DL/BL/SPL/BPL/SIL/DIL/R8B..R15B */
    AR_GPR16,    /* AX/CX/DX/BX/SP/BP/SI/DI/R8W..R15W */
    AR_GPR32,    /* EAX/ECX/EDX/EBX/ESP/EBP/ESI/EDI/R8D..R15D */
    AR_GPR64,    /* RAX..R15 */
    AR_SEG,      /* CS/DS/ES/FS/GS/SS */
    AR_X87,      /* ST0..ST7 */
    AR_XMM,      /* XMM0..XMM15 */
    AR_AHHIGH,   /* AH/BH/CH/DH (legacy high-byte) */
    /* ARM64 register classes. `num` follows the hardware encoding (31 = ZR
     * or SP depending on context; FP/LR are 29/30). */
    AR_A64_X,  /* X0..X30, XZR (num 31) */
    AR_A64_W,  /* W0..W30, WZR (num 31) */
    AR_A64_SP, /* SP / WSP (num 31, treated as stack pointer) */
    /* ARM64 SIMD/FP register classes.  All share the V0..V31 hardware
     * file; the class chooses the access width (and which encoder
     * variant is selected). */
    AR_A64_B, /* B0..B31  - 8-bit FP scalar (lowest byte of Vn) */
    AR_A64_H, /* H0..H31  - 16-bit FP scalar (lowest halfword) */
    AR_A64_S, /* S0..S31  - 32-bit FP scalar (single) */
    AR_A64_D, /* D0..D31  - 64-bit FP scalar (double) */
    AR_A64_Q, /* Q0..Q31  - 128-bit quad-word */
    AR_A64_V, /* V0..V31  - vector form, with `.<arr>` element-size suffix */
} AsmRegClass;

typedef struct {
    AsmRegClass cls;
    int num; /* hardware register number 0..15 */
} AsmReg;

typedef struct {
    AsmOperandKind kind;
    int width; /* 1/2/4/8 bytes for size-hinted memory ops; 0 = default */
    /* AOP_REG */
    AsmReg reg;
    /* AOP_REG, arm64 V-form: e.g. V0.4S -> v_lane_bits=32 v_n_lanes=4
     * v_lane_idx=-1; V0.S[2] -> v_lane_bits=32 v_n_lanes=0  v_lane_idx=2;
     *                            V0.2H[1] (BFDOT indexed) -> 16 / 2 / 1.
     * For B/H/S/D/Q scalar classes the fields are all 0/-1.            */
    int v_lane_bits; /* 8 / 16 / 32 / 64, or 0 if not a V-form */
    int v_n_lanes;   /* lane count if arrangement, else 0    */
    int v_lane_idx;  /* -1 = no [i], else the index           */
    /* AOP_IMM / AOP_SHIFT */
    int64_t imm;
    int is_float;   /* 1 if the imm came from a float literal (`1.5`/`1f`) */
    double f64;     /* float value when is_float is set */
    int shift_type; /* AOP_SHIFT only: ASHIFT_LSL/LSR/ASR/ROR */
    /* AOP_MEM: [seg : base + index*scale + disp]
     * `cls_name` and `member_name` are non-NULL when the operand uses the
     * `Class.field[reg]` form - resolved by sema to a concrete offset. */
    AsmReg seg; /* segment override; cls==AR_SEG and num<0 means none */
    int has_seg;
    AsmReg base;
    int has_base;
    AsmReg index;
    int has_index;
    int scale; /* 1/2/4/8 */
    int64_t disp;
    /* ARM64 addressing-mode flags.  Mutually exclusive with each other
     * but compose freely with has_base / has_index / disp. */
    int writeback;     /* 1: `[Xn, #imm]!`   - Xn += imm, then load */
    int post_index;    /* 1: `[Xn], #imm`    - load, then Xn += imm */
    int extend_lsl;    /* shift amount in `[Xn, Xm, LSL #N]`; 0 if none */
    char *cls_name;    /* owned; for Class.field operands */
    char *member_name; /* owned; for Class.field operands */
    char *label_name;  /* owned; for AOP_LABEL or for plain symbol disp */
} AsmOperand;

typedef enum {
    AINS_NONE = 0,
    AINS_INSTR,        /* mnemonic + operands */
    AINS_LABEL_LOCAL,  /* @@NN: */
    AINS_LABEL_PUBLIC, /* NAME:: */
    AINS_DIRECTIVE,    /* DB/DW/DD/DQ/DU32 followed by comma-list */
} AsmLineKind;

typedef struct AsmLine {
    AsmLineKind kind;
    int line; /* source line for diagnostics */
    /* INSTR: mnemonic + operands. */
    char *mnemonic;       /* owned; lowercase canonical form (kept around for
                           * diagnostics so error messages print the user's
                           * actual spelling). */
    int mnemonic_id;      /* opaque per-target id resolved at parse time; one
                           * of AArch64Mn / X86_64Mn. TASM_MN_UNKNOWN (0) if
                           * the mnemonic wasn't in the target's table. */
    AsmOperand *operands; /* owned */
    int n_operands;
    /* INSTR, x86 only: REP-family prefix byte glued onto the following
     * string op at parse time (0xF3 for REP/REPE/REPZ, 0xF2 for
     * REPNE/REPNZ, 0 for none). */
    int rep_prefix;
    /* LABEL_LOCAL: stores the @@<n> number. */
    int local_num;
    /* LABEL_PUBLIC: name field (owned). */
    char *label_name;
    /* DIRECTIVE: directive name (db/dw/...) + operand list (treated as
     * imm/label). */
    /* (mnemonic + operands fields reused.) */
} AsmLine;

typedef struct AsmBlock {
    AsmLine *lines;
    int n_lines;
    int cap_lines;
    int src_line; /* line of `asm {` in the .HC file */
    const char *src_file;
    Target target; /* selects the instruction dialect */
    int errors;   /* parse-time error count; asm_encode() refuses to run
                   * when this is non-zero. */
} AsmBlock;

/* ---------------- error sink ----------------
 *
 * Shared between parser and encoder. The handler is called once per error;
 * for parser errors `ln` is NULL (the AsmLine is still half-built) and the
 * `msg` carries a pre-formatted "file:line: ..." prefix. For encoder
 * errors `ln` _might_ be populated and the encoder prefixes the file/line
 * itself. */
typedef void asm_error_handler(void *errctx, AsmLine *ln, const char *msg);

typedef struct AsmErrHandler {
    void *data;
    asm_error_handler *handler;
} AsmErrHandler;

/* ---------------- parser ---------------- */

/* Parse `body` into an AsmBlock. Parse-time errors are NON-fatal: they go
 * to a default stderr handler and the parser skips to the next line and
 * keeps going so a single bad line doesn't mask the rest. The error
 * tally lands on `block->errors`. */
AsmBlock *asm_parse(const char *body, int body_len, const char *src_file,
        int src_line, Target target);

/* Like asm_parse but the caller supplies a custom error sink. Pass NULL
 * for `handler` to get the default stderr behaviour. */
AsmBlock *asm_parse_with_handler(const char *body, int body_len,
        const char *src_file, int src_line, Target target,
        void *errctx, asm_error_handler *handler);

void asm_block_free(AsmBlock *b);

/* Format a single AsmLine as canonical textual asm (lowercase mnemonic,
 * operands separated by ", ").  Writes up to buflen-1 bytes + NUL.
 * Returns the number of chars that would have been written (snprintf
 * semantics).  Used by callers that want a textual rendering alongside
 * encoded bytes (e.g. -S output for `asm {}` blocks). */
int asm_line_format(const AsmLine *ln, char *buf, size_t buflen);


/* Debug print. */
void asm_block_print(FILE *f, AsmBlock *b, int indent);

#endif
