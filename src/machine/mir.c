#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "asm.h"
#include "asm_enc.h"
#include "mir.h"
#include "target.h"
#include "tasm_util.h"

MirFn *
mir_fn_new(const char *name, Target target)
{
    MirFn *fn = xmalloc(sizeof(MirFn));
    memset(fn, 0, sizeof(*fn));
    fn->name = name ? xstrdup(name) : NULL;
    fn->target = target;
    fn->block = xmalloc(sizeof(AsmBlock));
    memset(fn->block, 0, sizeof(*fn->block));
    fn->block->target = target;
    if (name) mir_public_label(fn, name);
    return fn;
}

void
mir_fn_free(MirFn *fn)
{
    if (!fn) return;
    asm_block_free(fn->block);
    xfree(fn->name);
    xfree(fn);
}

/* ---------------- operands ---------------- */

AsmOperand
mir_reg(AsmRegClass cls, int num)
{
    AsmOperand o;
    memset(&o, 0, sizeof(o));
    o.kind = AOP_REG;
    o.reg.cls = cls;
    o.reg.num = num;
    o.v_lane_idx = -1;
    return o;
}

AsmOperand mir_x(int num) { return mir_reg(AR_A64_X, num); }
AsmOperand mir_w(int num) { return mir_reg(AR_A64_W, num); }
AsmOperand mir_d(int num) { return mir_reg(AR_A64_D, num); }
AsmOperand mir_s(int num) { return mir_reg(AR_A64_S, num); }
AsmOperand mir_q(int num) { return mir_reg(AR_A64_Q, num); }
AsmOperand mir_sp(void)   { return mir_reg(AR_A64_SP, 31); }

AsmOperand mir_r64(int num) { return mir_reg(AR_GPR64, num); }
AsmOperand mir_r32(int num) { return mir_reg(AR_GPR32, num); }
AsmOperand mir_r16(int num) { return mir_reg(AR_GPR16, num); }
AsmOperand mir_r8(int num)  { return mir_reg(AR_GPR8, num); }
AsmOperand mir_xmm(int num) { return mir_reg(AR_XMM, num); }

AsmOperand
mir_imm(int64_t v)
{
    AsmOperand o;
    memset(&o, 0, sizeof(o));
    o.kind = AOP_IMM;
    o.imm = v;
    return o;
}

AsmOperand
mir_fimm(double v)
{
    AsmOperand o = mir_imm(0);
    o.is_float = 1;
    o.f64 = v;
    return o;
}

AsmOperand
mir_shift(int shift_type, int amount)
{
    AsmOperand o;
    memset(&o, 0, sizeof(o));
    o.kind = AOP_SHIFT;
    o.shift_type = shift_type;
    o.imm = amount;
    return o;
}

AsmOperand
mir_mem(AsmOperand base, int64_t disp)
{
    AsmOperand o;
    memset(&o, 0, sizeof(o));
    o.kind = AOP_MEM;
    o.base = base.reg;
    o.has_base = 1;
    o.disp = disp;
    return o;
}

AsmOperand
mir_mem_idx(AsmOperand base, AsmOperand index, int lsl)
{
    AsmOperand o = mir_mem(base, 0);
    o.index = index.reg;
    o.has_index = 1;
    o.scale = 1;
    o.extend_lsl = lsl;
    return o;
}

AsmOperand
mir_mem_pre(AsmOperand base, int64_t disp)
{
    AsmOperand o = mir_mem(base, disp);
    o.writeback = 1;
    return o;
}

AsmOperand
mir_mem_post(AsmOperand base, int64_t disp)
{
    AsmOperand o = mir_mem(base, disp);
    o.post_index = 1;
    return o;
}

AsmOperand
mir_mem_sib(AsmOperand base, AsmOperand index, int scale, int64_t disp)
{
    AsmOperand o = mir_mem(base, disp);
    o.index = index.reg;
    o.has_index = 1;
    o.scale = scale;
    return o;
}

AsmOperand
mir_mem_hint(AsmOperand mem, int width)
{
    mem.width = width;
    return mem;
}

AsmOperand
mir_label_local(int num)
{
    AsmOperand o;
    memset(&o, 0, sizeof(o));
    o.kind = AOP_LABEL;
    /* The parser spells a local label operand as the TEXT `@@N` in
     * label_name - asm_operand_local_num parses the digits back out -
     * so the builder has to produce the same shape. Static buffer on
     * the asm_tmp_printf precedent: emit deep-copies it, and no
     * instruction takes two label operands. */
    static char buf[16];
    snprintf(buf, sizeof(buf), "@@%d", num);
    o.label_name = buf;
    return o;
}

AsmOperand
mir_sym(const char *name)
{
    AsmOperand o;
    memset(&o, 0, sizeof(o));
    o.kind = AOP_LABEL;
    o.label_name = (char *)name;   /* borrowed; deep-copied on emit */
    return o;
}

/* ---------------- instructions ---------------- */

static AsmLine *
mir_append_line(MirFn *fn)
{
    AsmBlock *b = fn->block;
    if (b->n_lines == b->cap_lines) {
        b->cap_lines = b->cap_lines ? b->cap_lines * 2 : 16;
        b->lines = xrealloc(b->lines,
                            sizeof(AsmLine) * (size_t)b->cap_lines);
    }
    AsmLine *ln = &b->lines[b->n_lines++];
    memset(ln, 0, sizeof(*ln));
    return ln;
}

static void
mir_copy_operands(AsmLine *ln, const AsmOperand *ops, int n)
{
    if (n <= 0) return;
    ln->operands = xmalloc(sizeof(AsmOperand) * (size_t)n);
    ln->n_operands = n;
    for (int i = 0; i < n; i++) {
        ln->operands[i] = ops[i];
        /* The line owns its strings (asm_block_free frees them), and
         * the constructors only borrow. */
        if (ops[i].label_name)
            ln->operands[i].label_name = xstrdup(ops[i].label_name);
        if (ops[i].cls_name)
            ln->operands[i].cls_name = xstrdup(ops[i].cls_name);
        if (ops[i].member_name)
            ln->operands[i].member_name = xstrdup(ops[i].member_name);
    }
}

AsmLine *
mir_emit(MirFn *fn, int mnemonic_id, const AsmOperand *ops, int n)
{
    AsmLine *ln = mir_append_line(fn);
    ln->kind = AINS_INSTR;
    ln->mnemonic_id = mnemonic_id;
    const char *name = mir_mnemonic_name(fn->target, mnemonic_id);
    ln->mnemonic = xstrdup(name ? name : "?");
    mir_copy_operands(ln, ops, n);
    return ln;
}

AsmLine *
mir0(MirFn *fn, int mnemonic_id)
{
    return mir_emit(fn, mnemonic_id, NULL, 0);
}

AsmLine *
mir1(MirFn *fn, int mnemonic_id, AsmOperand a)
{
    return mir_emit(fn, mnemonic_id, &a, 1);
}

AsmLine *
mir2(MirFn *fn, int mnemonic_id, AsmOperand a, AsmOperand b)
{
    AsmOperand ops[2] = { a, b };
    return mir_emit(fn, mnemonic_id, ops, 2);
}

AsmLine *
mir3(MirFn *fn, int mnemonic_id, AsmOperand a, AsmOperand b, AsmOperand c)
{
    AsmOperand ops[3] = { a, b, c };
    return mir_emit(fn, mnemonic_id, ops, 3);
}

AsmLine *
mir4(MirFn *fn, int mnemonic_id, AsmOperand a, AsmOperand b, AsmOperand c,
     AsmOperand d)
{
    AsmOperand ops[4] = { a, b, c, d };
    return mir_emit(fn, mnemonic_id, ops, 4);
}

AsmLine *
mir_bcond(MirFn *fn, const char *cc, int local_num)
{
    AsmLine *ln = mir_append_line(fn);
    ln->kind = AINS_INSTR;
    ln->mnemonic_id = TASM_MN_UNKNOWN;  /* dispatched off the text */
    char buf[16];
    snprintf(buf, sizeof(buf), "b.%s", cc);
    ln->mnemonic = xstrdup(buf);
    AsmOperand target = mir_label_local(local_num);
    mir_copy_operands(ln, &target, 1);
    return ln;
}

void
mir_local_label(MirFn *fn, int num)
{
    AsmLine *ln = mir_append_line(fn);
    ln->kind = AINS_LABEL_LOCAL;
    ln->local_num = num;
}

void
mir_public_label(MirFn *fn, const char *name)
{
    AsmLine *ln = mir_append_line(fn);
    ln->kind = AINS_LABEL_PUBLIC;
    ln->label_name = xstrdup(name);
}

/* ---------------- sinks ---------------- */

int
mir_fn_encode(MirFn *fn, AsmResolver *r, AsmEnc *out)
{
    return asm_encode(fn->block, r, out);
}

int
mir_fn_n_lines(MirFn *fn)
{
    return fn->block->n_lines;
}

int
mir_fn_format_line(MirFn *fn, int i, char *buf, size_t buflen)
{
    if (i < 0 || i >= fn->block->n_lines) {
        if (buflen) buf[0] = '\0';
        return 0;
    }
    return asm_line_format(&fn->block->lines[i], buf, buflen);
}

void
mir_fn_print(MirFn *fn, FILE *f)
{
    char buf[256];
    for (int i = 0; i < fn->block->n_lines; i++) {
        AsmLine *ln = &fn->block->lines[i];
        asm_line_format(ln, buf, sizeof(buf));
        fprintf(f, "%s%s\n", ln->kind == AINS_INSTR ? "\t" : "", buf);
    }
}

const char *
mir_mnemonic_name(Target target, int mnemonic_id)
{
    const TasmMnemonicTable *t = tasm_mnemonic_table_for(target);
    if (!t || mnemonic_id == TASM_MN_UNKNOWN) return NULL;
    /* Linear scan; aliases share an id and the table is alphabetical,
     * so the first hit is the canonical spelling (`bl` over `call`).
     * Tables are a few hundred entries and this runs once per emitted
     * instruction - not worth a reverse index until it shows up in a
     * profile. */
    for (int i = 0; i < t->n_entries; i++) {
        if (t->entries[i].id == mnemonic_id) return t->entries[i].name;
    }
    return NULL;
}
