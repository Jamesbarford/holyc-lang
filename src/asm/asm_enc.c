#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <stdarg.h>

#include "asm_enc.h"
#include "asm.h"
#include "enc_arm64.h"
#include "enc_x86_64.h"
#include "tasm_util.h"

/* ================================================================ output */

char *
asm_tmp_printf(const char *fmt, ...)
{
    static char tmp[256];
    va_list ap;
    va_start(ap, fmt);
    int n = vsnprintf(tmp, sizeof tmp, fmt, ap);
    va_end(ap);
    if (n < 0)
        return NULL;
    return tmp;
}

void
asm_err_at(AsmEnc *e, AsmLine *ln, const char *msg)
{
    e->err_handler.handler(e->err_handler.data, ln, msg);
}

void
put_byte(AsmEnc *e, uint8_t b)
{
    if (e->len == e->cap) {
        e->cap = e->cap ? e->cap * 2 : 256;
        e->bytes = xrealloc(e->bytes, e->cap);
    }
    e->bytes[e->len++] = b;
}

void
put_u16(AsmEnc *e, uint16_t v)
{
    put_byte(e, (uint8_t)v);
    put_byte(e, (uint8_t)(v >> 8));
}

void
put_u32(AsmEnc *e, uint32_t v)
{
    put_byte(e, (uint8_t)v);
    put_byte(e, (uint8_t)(v >> 8));
    put_byte(e, (uint8_t)(v >> 16));
    put_byte(e, (uint8_t)(v >> 24));
}

size_t
put_word(AsmEnc *e, uint32_t w)
{
    size_t off = e->len;
    put_u32(e, w);
    return off;
}

void
put_u64(AsmEnc *e, uint64_t v)
{
    put_u32(e, (uint32_t)v);
    put_u32(e, (uint32_t)(v >> 32));
}

void
put_zero_n(AsmEnc *e, int n)
{
    for (int i = 0; i < n; i++)
        put_byte(e, 0);
}

int
asm_eq(const char *a, const char *b)
{
    return !strcasecmp(a, b);
}

int
asm_is_reg(AsmOperand *o)
{
    return o->kind == AOP_REG;
}

int
asm_is_imm(AsmOperand *o)
{
    return o->kind == AOP_IMM;
}

int
asm_is_mem(AsmOperand *o)
{
    return o->kind == AOP_MEM;
}

int
asm_is_label(AsmOperand *o)
{
    return o->kind == AOP_LABEL;
}

void
asm_define_label(AsmEnc *e, int local_num, const char *name)
{
    if (e->n_labels == e->cap_labels) {
        e->cap_labels = e->cap_labels ? e->cap_labels * 2 : 32;
        e->labels = xrealloc(e->labels,
                sizeof(AsmLabelDef) * (size_t)e->cap_labels);
    }
    AsmLabelDef *L = &e->labels[e->n_labels++];
    L->local_num = local_num;
    L->name = name ? xstrdup(name) : NULL;
    L->byte_offset = e->len;
}

void
asm_add_fixup(AsmEnc *e, AsmFixup f)
{
    if (e->n_fixups == e->cap_fixups) {
        e->cap_fixups = e->cap_fixups ? e->cap_fixups * 2 : 32;
        e->fixups = xrealloc(e->fixups,
                sizeof(AsmFixup) * (size_t)e->cap_fixups);
    }
    e->fixups[e->n_fixups++] = f;
}

int
asm_operand_local_num(AsmOperand *o)
{
    if (o->kind != AOP_LABEL || !o->label_name) return -1;
    if (o->label_name[0] != '@' || o->label_name[1] != '@') return -1;
    return atoi(o->label_name + 2);
}

void
asm_enc_directive(AsmEnc *e, AsmLine *ln)
{
    int width = 0;
    const char *m = ln->mnemonic ? ln->mnemonic : "";
    if (!strcasecmp(m, "db") || !strcasecmp(m, "du8"))  width = 1;
    if (!strcasecmp(m, "dw") || !strcasecmp(m, "du16")) width = 2;
    if (!strcasecmp(m, "dd") || !strcasecmp(m, "du32")) width = 4;
    if (!strcasecmp(m, "dq") || !strcasecmp(m, "du64")) width = 8;
    if (!width) {
        asm_err_at(e, ln, "unknown directive");
        return;
    }

    for (int i = 0; i < ln->n_operands; i++) {
        AsmOperand *o = &ln->operands[i];
        if (asm_is_imm(o)) {
            int64_t v = o->imm;
            for (int b = 0; b < width; b++)
                put_byte(e, (uint8_t)(v >> (8 * b)));
        } else if (asm_is_label(o)) {
            int n = asm_operand_local_num(o);
            AsmFixup f = { 0 };
            if (n >= 0) {
                f.kind = AF_LOCAL;
                f.local_num = n;
            } else {
                f.kind = AF_SYMBOL;
                f.sym = xstrdup(o->label_name);
            }
            f.patch_offset = e->len;
            f.width = width;
            /* Data directives are NOT pc-relative - absolute address. */
            f.pcrel = 0;
            asm_add_fixup(e, f);
            put_zero_n(e, width);
        } else {
            asm_err_at(e, ln, "directive: unsupported operand kind");
        }
    }
}

/* Dispatch to the per-arch encoder. Refuses to run if the parser logged
 * any errors on `blk` - the encoder would just produce garbage from a
 * half-built AsmLine list. The error tally propagates to out->errors so
 * callers see one consolidated count. */
int
asm_encode(AsmBlock *blk, AsmResolver *r, AsmEnc *out)
{
    if (blk && blk->errors > 0) {
        out->errors += blk->errors;
        return blk->errors;
    }
    if (blk && blk->target == TASM_ARCH_ARM64)
        return asm_encode_arm64(blk, r, out);
    return asm_encode_x86_64(blk, r, out);
}

/* While needing _a lot_ of arguments the beauty is that we can use the 
 * same error handler for both parsing assembly and encoding it which is
 * simpler and probably what a user will want. It's certainly what I want.
 * Make sure to call `asm_enc_free(...)` when you are done */
int
asm_parse_and_encode(AsmEnc *out,
                     AsmResolver *r,
                     Target target,
                     char *asm_code,
                     int len,
                     const char *src_file,
                     int src_line,
                     void *err_ctx,
                     asm_error_handler *err_handler)
{
    asm_enc_init(out);
    AsmBlock *blk;
    if (err_ctx && err_handler) {
        asm_enc_set_error_handler(out, err_ctx, err_handler); 
        blk = asm_parse_with_handler(asm_code,
                                     len,
                                     src_file,
                                     src_line,
                                     target,
                                     err_ctx,
                                     err_handler);
    } else {
        blk = asm_parse(asm_code, len, src_file, src_line, target);
    }
    
    int err_cnt = 0;
    /* We have errors, stop progress */
    if (blk && blk->errors > 0) {
        out->errors += blk->errors;
        err_cnt = blk->errors;
    } else {
        err_cnt = asm_encode(blk, r, out);
    }
    asm_block_free(blk);
    return err_cnt;
}

static void
asm_default_err_cb(void *errctx, AsmLine *ln, const char *msg)
{
    AsmEnc *e = (AsmEnc *)errctx;
    if (ln) {
        fprintf(stderr, "%s:%d: asm: %s ('%s')\n", e->src_file ? e->src_file : "?",
                ln->line, msg, ln->mnemonic ? ln->mnemonic : "?");
    } else {
        fprintf(stderr, "%s\n", msg);
    }
    e->errors++;
}

/* Does not free `AsmEnc *e`, you need to free that if you malloced it. */
void
asm_enc_free(AsmEnc *e)
{
    if (!e) return;
    for (int i = 0; i < e->n_labels; i++)
        free(e->labels[i].name);
    for (int i = 0; i < e->n_fixups; i++) {
        free(e->fixups[i].sym);
        free(e->fixups[i].cls_name);
        free(e->fixups[i].member_name);
    }

    if (e->bytes)        free(e->bytes);
    if (e->labels)       free(e->labels);
    if (e->fixups)       free(e->fixups);
    if (e->line_offsets) free(e->line_offsets);
}

void
asm_enc_init(AsmEnc *e)
{
    memset(e, 0, sizeof(AsmEnc));
    e->err_handler.handler = asm_default_err_cb;
    e->err_handler.data = e;
}

void
asm_enc_set_error_handler(AsmEnc *e, void *errctx, asm_error_handler *handler)
{
    e->err_handler.handler = handler;
    e->err_handler.data = errctx;
}
