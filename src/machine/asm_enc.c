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
        /* A byte buffer can use the whole bucket, so take the rounded
         * capacity back. */
        unsigned int actual = 0;
        e->bytes = mem_ctx_realloc(&e->mem, e->bytes,
                                   e->cap ? (unsigned int)e->cap * 2 : 256,
                                   &actual);
        e->cap = actual;
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
        e->labels = mem_ctx_realloc(&e->mem, e->labels,
                sizeof(AsmLabelDef) * (unsigned int)e->cap_labels, NULL);
    }
    AsmLabelDef *L = &e->labels[e->n_labels++];
    L->local_num = local_num;
    L->name = name ? xstrdup(name) : NULL;
    L->byte_offset = e->len;
    L->is_local = 0;
}

void
asm_mark_label_local(AsmEnc *e, const char *name)
{
    if (!name) return;
    for (int i = 0; i < e->n_labels; i++) {
        AsmLabelDef *L = &e->labels[i];
        if (L->name && !strcmp(L->name, name)) L->is_local = 1;
    }
}

void
asm_add_fixup(AsmEnc *e, AsmFixup f)
{
    if (e->n_fixups == e->cap_fixups) {
        e->cap_fixups = e->cap_fixups ? e->cap_fixups * 2 : 32;
        e->fixups = mem_ctx_realloc(&e->mem, e->fixups,
                sizeof(AsmFixup) * (unsigned int)e->cap_fixups, NULL);
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
            /* An eight-byte one is a pointer, which every object format
             * has a relocation for; tag it so an object writer can emit
             * that rather than drop the fixup. Narrower widths are left
             * untagged deliberately - a 32-bit absolute address is not
             * generally representable in position-independent code, so
             * there is nothing safe to emit and being told about it is
             * better than a silently truncated pointer. */
            if (width == 8) f.reloc = AFR_ABS64;
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
    /* bytes/labels/fixups live in the encoder's own context and go
     * down with its arena in one release. Label names and fixup syms
     * are global-context strings; they recycle if someone frees them
     * and are otherwise compile-lifetime by design. */
    xfree(e->line_offsets);
    mem_ctx_release(&e->mem);
    e->bytes = NULL;
    e->labels = NULL;
    e->fixups = NULL;
    e->line_offsets = NULL;
}

void
asm_enc_init(AsmEnc *e)
{
    memset(e, 0, sizeof(AsmEnc));
    e->data_split = -1;   /* all code until a producer says otherwise */
    e->err_handler.handler = asm_default_err_cb;
    e->err_handler.data = e;
    mem_ctx_init(&e->mem, 64 * 1024);
}

void
asm_enc_set_error_handler(AsmEnc *e, void *errctx, asm_error_handler *handler)
{
    e->err_handler.handler = handler;
    e->err_handler.data = errctx;
}
