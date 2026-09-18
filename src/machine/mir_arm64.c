#include <stdlib.h>
#include <string.h>

#include "asm.h"
#include "asm_enc.h"
#include "enc_arm64.h"
#include "mir.h"
#include "tasm_util.h"

/* AArch64 peephole pass over a MirFn's line stream.
 *
 * Runs between selection and the sink, on lines [from, n) - `from` is
 * the flush cursor, so a segment never reaches back across an inline
 * `asm {}` barrier whose bytes are already out. Every pattern here is
 * local and self-evident from the lines alone; anything that needs
 * liveness or value tracking belongs in the IR passes, not here.
 *
 *   - a local label no branch in the stream references is noise from
 *     the selector's block bookkeeping (the always-emitted epilogue
 *     label chief among them) and is dropped;
 *   - `b @@N` straight into `@@N:` falls through anyway;
 *   - `mov r, r` moves nothing;
 *   - two identical adjacent extensions compute the same value twice
 *     (the ops are idempotent: their source bits are unchanged by the
 *     first application);
 *   - a load of the exact slot the previous instruction stored, into
 *     the register it was stored from, re-reads what is already
 *     there. Only the full-width forms: a w-form load also zeroes the
 *     upper half, which the stored register may not have had. */

static int mir_local_label_num(const AsmLine *ln) {
    if (ln->kind != AINS_LABEL_LOCAL || ln->label_name) return -1;
    return ln->local_num;
}

/* The local-label number a line's operand refers to, or -1. */
static int mir_operand_local_ref(const AsmOperand *op) {
    if (op->kind != AOP_LABEL || !op->label_name) return -1;
    if (op->label_name[0] != '@' || op->label_name[1] != '@') return -1;
    return atoi(op->label_name + 2);
}

static int mir_line_refs_label(const AsmLine *ln, int num) {
    if (ln->kind != AINS_INSTR) return 0;
    for (int i = 0; i < ln->n_operands; i++) {
        if (mir_operand_local_ref(&ln->operands[i]) == num) return 1;
    }
    return 0;
}

/* Whole-stream scan: a branch in an already-flushed segment may target
 * a label in this one. */
static int mir_label_is_referenced(const AsmBlock *b, int num) {
    for (int i = 0; i < b->n_lines; i++) {
        if (mir_line_refs_label(&b->lines[i], num)) return 1;
    }
    return 0;
}

static void mir_line_release(AsmLine *ln) {
    xfree(ln->mnemonic);
    xfree(ln->label_name);
    for (int j = 0; j < ln->n_operands; j++) {
        xfree(ln->operands[j].cls_name);
        xfree(ln->operands[j].member_name);
        xfree(ln->operands[j].label_name);
    }
    xfree(ln->operands);
}

static void mir_block_delete_line(AsmBlock *b, int idx) {
    mir_line_release(&b->lines[idx]);
    memmove(&b->lines[idx], &b->lines[idx + 1],
            (size_t)(b->n_lines - idx - 1) * sizeof(AsmLine));
    b->n_lines--;
}

static int mir_is_reg(const AsmOperand *op, AsmRegClass cls) {
    return op->kind == AOP_REG && op->reg.cls == cls;
}

static int mir_same_reg(const AsmOperand *a, const AsmOperand *b) {
    return a->kind == AOP_REG && b->kind == AOP_REG &&
           a->reg.cls == b->reg.cls && a->reg.num == b->reg.num;
}

/* `mov x3, x3` / `fmov d1, d1` - the FULL-WIDTH forms only. A w-form
 * self-move is not a no-op: `mov w0, w0` zeroes the upper half, which
 * is precisely the zero-extension canonicalisation the selector emits
 * it for. An s-form fmov likewise clears the rest of the vector reg. */
static int mir_is_self_move(const AsmLine *ln) {
    if (ln->kind != AINS_INSTR || ln->n_operands != 2) return 0;
    const AsmOperand *d = &ln->operands[0];
    if (ln->mnemonic_id == A64MN_MOV && !mir_is_reg(d, AR_A64_X)) return 0;
    if (ln->mnemonic_id == A64MN_FMOV && !mir_is_reg(d, AR_A64_D)) return 0;
    if (ln->mnemonic_id != A64MN_MOV && ln->mnemonic_id != A64MN_FMOV)
        return 0;
    return mir_same_reg(&ln->operands[0], &ln->operands[1]);
}

/* Ops whose repetition computes the same value: the extension family
 * (the source bits they read are not changed by the first application)
 * and plain register moves. */
static int mir_is_idempotent_op(const AsmLine *ln) {
    switch (ln->mnemonic_id) {
    case A64MN_SXTB: case A64MN_SXTH: case A64MN_SXTW:
    case A64MN_UXTB: case A64MN_UXTH:
        return 1;
    case A64MN_MOV: case A64MN_FMOV:
        return ln->n_operands == 2 && ln->operands[0].kind == AOP_REG &&
               ln->operands[1].kind == AOP_REG;
    default:
        return 0;
    }
}

static int mir_operand_eq(const AsmOperand *a, const AsmOperand *b) {
    if (a->kind != b->kind) return 0;
    switch (a->kind) {
    case AOP_REG:
        return mir_same_reg(a, b);
    case AOP_IMM:
        return a->imm == b->imm && a->is_float == b->is_float &&
               a->f64 == b->f64;
    default:
        return 0;
    }
}

static int mir_line_eq(const AsmLine *a, const AsmLine *b) {
    if (a->kind != AINS_INSTR || b->kind != AINS_INSTR) return 0;
    if (a->mnemonic_id != b->mnemonic_id || a->mnemonic_id == 0) return 0;
    if (a->n_operands != b->n_operands) return 0;
    for (int i = 0; i < a->n_operands; i++) {
        if (!mir_operand_eq(&a->operands[i], &b->operands[i])) return 0;
    }
    return 1;
}

/* Plain `[base, #disp]` memory operand equality: no index, no
 * writeback, no symbol. */
static int mir_simple_mem_eq(const AsmOperand *a, const AsmOperand *b) {
    if (a->kind != AOP_MEM || b->kind != AOP_MEM) return 0;
    if (a->has_index || b->has_index || a->writeback || b->writeback ||
        a->post_index || b->post_index || a->label_name || b->label_name)
    {
        return 0;
    }
    return a->has_base && b->has_base &&
           a->base.cls == b->base.cls && a->base.num == b->base.num &&
           a->disp == b->disp;
}

/* `str/stur R, [m]` immediately followed by `ldr/ldur R, [m]`: the
 * load is a re-read. Full-width X and FP forms only; see the header
 * comment for why the w-forms stay. */
static int mir_store_load_pair(const AsmLine *st, const AsmLine *ld) {
    int st_ok = st->mnemonic_id == A64MN_STR || st->mnemonic_id == A64MN_STUR;
    int ld_ok = ld->mnemonic_id == A64MN_LDR || ld->mnemonic_id == A64MN_LDUR;
    if (!st_ok || !ld_ok) return 0;
    if (st->n_operands != 2 || ld->n_operands != 2) return 0;
    const AsmOperand *r = &st->operands[0];
    if (!mir_is_reg(r, AR_A64_X) && !mir_is_reg(r, AR_A64_D) &&
        !mir_is_reg(r, AR_A64_S))
    {
        return 0;
    }
    return mir_same_reg(r, &ld->operands[0]) &&
           mir_simple_mem_eq(&st->operands[1], &ld->operands[1]);
}

int mir_fn_peephole_arm64(MirFn *fn, int from) {
    AsmBlock *b = fn->block;
    int removed = 0;
    int changed = 1;
    while (changed) {
        changed = 0;
        for (int i = from; i < b->n_lines; i++) {
            AsmLine *ln = &b->lines[i];

            /* `b @@N` into `@@N:`. */
            if (ln->kind == AINS_INSTR && ln->mnemonic_id == A64MN_B &&
                ln->n_operands == 1 && i + 1 < b->n_lines)
            {
                int target = mir_operand_local_ref(&ln->operands[0]);
                if (target >= 0 &&
                    mir_local_label_num(&b->lines[i + 1]) == target)
                {
                    mir_block_delete_line(b, i);
                    removed++; changed = 1; i--;
                    continue;
                }
            }

            if (mir_is_self_move(ln)) {
                mir_block_delete_line(b, i);
                removed++; changed = 1; i--;
                continue;
            }

            if (i + 1 < b->n_lines) {
                AsmLine *nx = &b->lines[i + 1];
                if (mir_is_idempotent_op(ln) && mir_line_eq(ln, nx)) {
                    mir_block_delete_line(b, i + 1);
                    removed++; changed = 1;
                    continue;
                }
                if (nx->kind == AINS_INSTR && mir_store_load_pair(ln, nx)) {
                    mir_block_delete_line(b, i + 1);
                    removed++; changed = 1;
                    continue;
                }
            }
        }

        /* Dead labels last, so branch removals above can strand more of
         * them within the same fixpoint loop. */
        for (int i = from; i < b->n_lines; i++) {
            int num = mir_local_label_num(&b->lines[i]);
            if (num < 0) continue;
            if (!mir_label_is_referenced(b, num)) {
                mir_block_delete_line(b, i);
                removed++; changed = 1; i--;
            }
        }
    }
    return removed;
}
