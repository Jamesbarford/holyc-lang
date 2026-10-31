#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <strings.h>

#include "asm.h"
#include "enc_x86_64.h"
#include "tasm_util.h"

/* ================================================================ lexer */

typedef enum {
    ATK_EOF = 0,
    ATK_IDENT,       /* mnemonic, register, label name, or symbol */
    ATK_INT,
    ATK_FLOAT,       /* decimal float literal: `1.5`, `1.5f`, `1f` */
    ATK_STR,
    ATK_PUNCT,       /* one of: [ ] , : + - * ( ) . */
    ATK_DOUBLECOLON, /* :: */
    ATK_LOCAL_LABEL, /* @@NN, value is the integer NN */
    ATK_NEWLINE,
} AsmTok;

typedef struct AsmLex {
    const char *src;
    int len;
    int pos;
    int line; /* 1-based */

    char *start;
    /* Pointer to the start of the current source line. */
    char *line_start_ptr;
    /* Column of the first byte of the token currently
     * being lexed. */
    int tok_start_col;

    const char *src_file;
    Target target; /* selects register dialect */
    /* Look-ahead. */
    AsmTok have_kind;
    int have;
    /* Last token data. */
    char *text; /* owned for IDENT/STR */
    int64_t ival;
    double fval; /* float literal value (ATK_FLOAT) */
    int punc;
    /* Error reporting. asm_parse_err_at() formats a message, calls the
     * handler, increments `errors`, and advances the lexer to the end of
     * the current line so subsequent parse steps see ATK_NEWLINE (or
     * ATK_EOF) and bail naturally without spinning or producing a flood
     * of follow-on errors. */
    AsmErrHandler err_handler;
    int errors;
} AsmLex;

/* Standalone libtasm has nowhere to put a message but stderr, so it
 * needs `file:line:` in front. An embedder does not: cctrl frames the
 * text in its own `error: ... --> file:line` block, and a prefix baked
 * into the message came out doubled. So the handler gets the bare
 * message and whoever prints it decides the framing.
 *
 * Only reachable while `err_handler.data` is still the AsmLex - an
 * installed handler replaces both the callback and the context. */
static void
asm_default_parse_err_cb(void *errctx, AsmLine *ln, const char *msg)
{
    AsmLex *l = (AsmLex *)errctx;
    fprintf(stderr, "%s:%d: error: %s\n",
            l && l->src_file ? l->src_file : "<input>",
            ln ? ln->line : 0, msg);
}

/* Format an error, call the handler, and resync the lexer to end-of-line
 * so the rest of the line is discarded. Callers continue normally; the
 * recursive-descent parse helpers will see ATK_NEWLINE / ATK_EOF and
 * unwind on their own. blk->errors > 0 then prevents asm_encode from
 * producing bytes for a half-built line. */
static void
asm_parse_err_at(AsmLex *l, int line, const char *fmt, ...)
{
    char body[512];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(body, sizeof body, fmt, ap);
    va_end(ap);

    /* This is simply an error diagnostic */
    AsmLine *ln = calloc(1,sizeof(AsmLine));
    ln->kind = AINS_NONE;
    ln->line = line;
    /* Without a column the renderer has no anchor and draws the gutter
     * with no `^` under it. `tok_start_col` is only refreshed on
     * instruction lines, so compute it from the current token instead
     * of reporting a stale one. */
    if (l->start && l->line_start_ptr && l->start >= l->line_start_ptr) {
        ln->col = (int)(l->start - l->line_start_ptr) + 1;
    }

    l->err_handler.handler(l->err_handler.data, ln, body);
    l->errors++;

    /* Skip to (but not past) the next '\n' so the next peek/consume sees a
     * NEWLINE token. The outer loop's asm_skip_newlines will eat it. */
    while (l->pos < l->len && l->src[l->pos] != '\n') l->pos++;
    l->have = 0; /* invalidate any cached look-ahead token */
}

static void
asm_lex_init(AsmLex *l, const char *body, int len, const char *file, int start_line,
        Target target)
{
    memset(l, 0, sizeof *l);
    l->src = body;
    l->len = len;
    l->line = start_line;
    l->err_handler.handler = asm_default_parse_err_cb;
    l->err_handler.data = l;
    l->src_file = file;
    l->target = target;
    /* Pointer to the start of the current source line. */
    l->line_start_ptr = (char *)body;
    l->start = (char *)body;
    l->tok_start_col = 1;
}

char *
asm_lex_cur_ptr(AsmLex *l)
{
    if (l->pos < l->len) {
        return &((char *)l->src)[l->pos];
    }
    return NULL;
}

void
asm_lex_set_start(AsmLex *l)
{
    l->start = asm_lex_cur_ptr(l);
}

static int
asm_lex_peek(AsmLex *l, int o)
{
    int i = l->pos + o;
    if (i >= l->len) {
        return 0;
    } else {
        return (unsigned char)l->src[i];
    }
}

static void
asm_skip_inline_ws(AsmLex *l)
{
    while (l->pos < l->len) {
        int c = asm_lex_peek(l, 0);
        if (c == ' ' || c == '\t' || c == '\r') l->pos++;
        else if (c == '/' && asm_lex_peek(l, 1) == '/') {
            while (l->pos < l->len && asm_lex_peek(l, 0) != '\n')
                l->pos++;
        } else if (c == '/' && asm_lex_peek(l, 1) == '*') {
            l->pos += 2;
            while (l->pos < l->len &&
                    !(asm_lex_peek(l, 0) == '*' && asm_lex_peek(l, 1) == '/'))
                l->pos++;
            if (l->pos < l->len) l->pos += 2;
        }
        /* Skip DolDoc `$...$` tags embedded in source (lexer-equivalent). */
        else if (c == '$') {
            if (asm_lex_peek(l, 1) == '$') {
                l->pos += 2;
                continue;
            }
            l->pos++;
            while (l->pos < l->len && asm_lex_peek(l, 0) != '$')
                l->pos++;
            if (l->pos < l->len) l->pos++;
        } else break;
    }
}

/* Returns 1 on success and fills l->text / l->ival / l->punc. */
static AsmTok
asm_lex_next(AsmLex *l)
{
    xfree(l->text);
    l->text = NULL;
    asm_skip_inline_ws(l);
    asm_lex_set_start(l);

    if (l->pos >= l->len)
        return ATK_EOF;

    int c = asm_lex_peek(l, 0);
    if (c == '\n') {
        l->pos++;
        l->line++;
        l->line_start_ptr = asm_lex_cur_ptr(l);
        return ATK_NEWLINE;
    }

    /* `::` and `:` */
    if (c == ':') {
        if (asm_lex_peek(l, 1) == ':') {
            l->pos += 2;
            return ATK_DOUBLECOLON;
        }
        l->pos++;
        l->punc = ':';
        return ATK_PUNCT;
    }

    /* @@NN local labels. */
    if (c == '@' && asm_lex_peek(l, 1) == '@') {
        l->pos += 2;
        int n = 0;
        if (!isdigit(asm_lex_peek(l, 0))) {
            asm_parse_err_at(l, l->line, "expected digits after `@@`");
        }
        while (isdigit(asm_lex_peek(l, 0))) {
            n = n * 10 + (asm_lex_peek(l, 0) - '0');
            l->pos++;
        }
        l->ival = n;
        return ATK_LOCAL_LABEL;
    }

    /* Numeric literals: 0x/0b/decimal/char/float.
     *
     * Decimal numbers become floats when followed by `.<digit>` (e.g.
     * `1.5`), an exponent (`1e3`), or an `f`/`F` type suffix (`1f`,
     * `1.5f`).  Hex and binary stay integer-only.  The float parse
     * lifts the whole lexeme out as a span and hands it to `strtod`. */
    if (isdigit(c)) {
        int64_t v = 0;
        if (c == '0' && (asm_lex_peek(l, 1) == 'x' || asm_lex_peek(l, 1) == 'X')) {
            l->pos += 2;
            while (isxdigit(asm_lex_peek(l, 0))) {
                int d = asm_lex_peek(l, 0);
                v = v * 16 + (d <= '9' ? d - '0' : (d | 32) - 'a' + 10);
                l->pos++;
            }
            l->ival = v;
            return ATK_INT;
        }
        if (c == '0' && asm_lex_peek(l, 1) == 'b') {
            l->pos += 2;
            while (asm_lex_peek(l, 0) == '0' || asm_lex_peek(l, 0) == '1') {
                v = v * 2 + (asm_lex_peek(l, 0) - '0');
                l->pos++;
            }
            l->ival = v;
            return ATK_INT;
        }
        int start = l->pos;
        while (isdigit(asm_lex_peek(l, 0))) {
            v = v * 10 + (asm_lex_peek(l, 0) - '0');
            l->pos++;
        }
        /* Decide between int and float.  We look for either:
         *   - `.<digit>` - fractional part follows
         *   - `e|E[+|-]?<digit>` - exponent follows
         *   - `f|F` (not part of an identifier) - explicit float suffix
         * If none, this stays an integer. */
        int is_float = 0;
        if (asm_lex_peek(l, 0) == '.' && isdigit(asm_lex_peek(l, 1))) {
            is_float = 1;
            l->pos++;
            while (isdigit(asm_lex_peek(l, 0)))
                l->pos++;
        }
        if (asm_lex_peek(l, 0) == 'e' || asm_lex_peek(l, 0) == 'E') {
            int saved = l->pos;
            l->pos++;
            if (asm_lex_peek(l, 0) == '+' || asm_lex_peek(l, 0) == '-') l->pos++;
            if (isdigit(asm_lex_peek(l, 0))) {
                is_float = 1;
                while (isdigit(asm_lex_peek(l, 0)))
                    l->pos++;
            } else {
                /* No digits after `e` - not an exponent; rewind. */
                l->pos = saved;
            }
        }
        if ((asm_lex_peek(l, 0) == 'f' || asm_lex_peek(l, 0) == 'F')) {
            int nxt = asm_lex_peek(l, 1);
            /* Only treat as float suffix if not followed by an ident-char
             * (so `1f` is float but `1fabc` would be ambiguous - we
             * conservatively still consume `f` here since identifiers
             * starting with a digit are otherwise illegal anyway). */
            (void)nxt;
            is_float = 1;
            l->pos++; /* consume the `f` / `F` */
        }
        if (is_float) {
            int end = l->pos;
            /* Strip a trailing f/F before handing to strtod. */
            int span = end - start;
            char *buf = xmalloc((size_t)span + 1);
            int j = 0;
            for (int i = 0; i < span; ++i) {
                char ch = l->src[start + i];
                if ((ch == 'f' || ch == 'F') && i == span - 1) break;
                buf[j++] = ch;
            }
            buf[j] = '\0';
            l->fval = strtod(buf, NULL);
            xfree(buf);
            return ATK_FLOAT;
        }
        l->ival = v;
        return ATK_INT;
    }

    /* Char literal '...'. Multi-byte packed into i64 (HolyC convention). */
    if (c == '\'') {
        l->pos++;
        int64_t v = 0;
        int n = 0;
        while (l->pos < l->len && asm_lex_peek(l, 0) != '\'') {
            int ch;
            if (asm_lex_peek(l, 0) == '\\') {
                l->pos++;
                int esc = asm_lex_peek(l, 0);
                l->pos++;
                switch (esc) {
                case 'n': ch = '\n'; break;
                case 't': ch = '\t'; break;
                case 'r': ch = '\r'; break;
                case '0': ch = '\0'; break;
                case '\\': ch = '\\'; break;
                case '\'': ch = '\''; break;
                case '"': ch = '"'; break;
                default: ch = esc;
                }
            } else {
                ch = asm_lex_peek(l, 0);
                l->pos++;
            }
            v |= ((int64_t)(unsigned char)ch) << (n * 8);
            if (++n >= 8) break;
        }
        if (asm_lex_peek(l, 0) == '\'') l->pos++;
        l->ival = v;
        return ATK_INT;
    }

    /* String literal "...". */
    if (c == '"') {
        l->pos++;
        size_t cap = 64, sn = 0;
        char *s = xmalloc(cap);
        while (l->pos < l->len && asm_lex_peek(l, 0) != '"') {
            int ch;
            if (asm_lex_peek(l, 0) == '\\') {
                l->pos++;
                int esc = asm_lex_peek(l, 0);
                l->pos++;
                switch (esc) {
                case 'n': ch = '\n'; break;
                case 't': ch = '\t'; break;
                case '0': ch = '\0'; break;
                case '\\': ch = '\\'; break;
                case '"': ch = '"'; break;
                default: ch = esc;
                }
            } else {
                ch = asm_lex_peek(l, 0);
                l->pos++;
            }
            if (sn + 1 >= cap) {
                cap *= 2;
                s = xrealloc(s, cap);
            }
            s[sn++] = (char)ch;
        }
        if (asm_lex_peek(l, 0) == '"') l->pos++;
        s[sn] = '\0';
        l->text = s;
        return ATK_STR;
    }

    /* Identifier (letters, digits, _, and a few asm-friendly extras). */
    if (isalpha(c) || c == '_') {
        size_t cap = 32, n = 0;
        char *s = xmalloc(cap);
        while (l->pos < l->len) {
            int d = asm_lex_peek(l, 0);
            /* '@' continues an identifier for relocation-decorated
             * symbols (`_free@GOTPCREL`); a LEADING '@' still starts a
             * local label, so `@@1` is untouched. */
            if (!(isalnum(d) || d == '_' || d == '@')) break;
            if (n + 1 >= cap) {
                cap *= 2;
                s = xrealloc(s, cap);
            }
            s[n++] = (char)d;
            l->pos++;
        }
        s[n] = '\0';
        l->text = s;
        return ATK_IDENT;
    }

    /* Single-char punctuation. ARM64 source uses `#` to prefix immediates
     * (e.g. `MOV X0, #42`). We swallow it as whitespace so the integer
     * lexer just sees the digits that follow. */
    if (c == '#') {
        l->pos++;
        return asm_lex_next(l);
    }
    if (strchr("[],+-*().;!", c)) {
        l->punc = c;
        l->pos++;
        return ATK_PUNCT;
    }

    asm_parse_err_at(l, l->line, "asm: unexpected character '%c' (0x%02x)",
            isprint(c) ? c : '?', c);
    return ATK_EOF;
}

/* One-token lookahead. */
static AsmTok
asm_lex_peek_tok(AsmLex *l)
{
    if (!l->have) {

        l->have_kind = asm_lex_next(l);
        l->have = 1;
    }
    return l->have_kind;
}

static AsmTok
asm_lex_consume(AsmLex *l)
{
    if (l->have) {
        l->have = 0;
        return l->have_kind;
    }
    AsmTok t = asm_lex_next(l);
    return t;
}

/* ================================================================ registers */

/* Returns 1 if `name` is a register and fills out_reg; case-insensitive.
 * Some spellings (e.g. "SP", "LR") name different registers depending on
 * the target - we pick the right interpretation based on the block's
 * target. */
static int
asm_lookup_register(const char *name, Target target, AsmReg *out)
{
    static const struct {
        const char *n;
        AsmRegClass cls;
        int num;
    } x86_table[] = {
        /* 64-bit GPRs */
        { "RAX", AR_GPR64, 0 },
        { "RCX", AR_GPR64, 1 },
        { "RDX", AR_GPR64, 2 },
        { "RBX", AR_GPR64, 3 },
        { "RSP", AR_GPR64, 4 },
        { "RBP", AR_GPR64, 5 },
        { "RSI", AR_GPR64, 6 },
        { "RDI", AR_GPR64, 7 },
        { "R8", AR_GPR64, 8 },
        { "R9", AR_GPR64, 9 },
        { "R10", AR_GPR64, 10 },
        { "R11", AR_GPR64, 11 },
        { "R12", AR_GPR64, 12 },
        { "R13", AR_GPR64, 13 },
        { "R14", AR_GPR64, 14 },
        { "R15", AR_GPR64, 15 },
        /* 32-bit GPRs */
        { "EAX", AR_GPR32, 0 },
        { "ECX", AR_GPR32, 1 },
        { "EDX", AR_GPR32, 2 },
        { "EBX", AR_GPR32, 3 },
        { "ESP", AR_GPR32, 4 },
        { "EBP", AR_GPR32, 5 },
        { "ESI", AR_GPR32, 6 },
        { "EDI", AR_GPR32, 7 },
        { "R8D", AR_GPR32, 8 },
        { "R9D", AR_GPR32, 9 },
        { "R10D", AR_GPR32, 10 },
        { "R11D", AR_GPR32, 11 },
        { "R12D", AR_GPR32, 12 },
        { "R13D", AR_GPR32, 13 },
        { "R14D", AR_GPR32, 14 },
        { "R15D", AR_GPR32, 15 },
        /* 16-bit GPRs */
        { "AX", AR_GPR16, 0 },
        { "CX", AR_GPR16, 1 },
        { "DX", AR_GPR16, 2 },
        { "BX", AR_GPR16, 3 },
        { "SP", AR_GPR16, 4 },
        { "BP", AR_GPR16, 5 },
        { "SI", AR_GPR16, 6 },
        { "DI", AR_GPR16, 7 },
        { "R8W", AR_GPR16, 8 },
        { "R9W", AR_GPR16, 9 },
        { "R10W", AR_GPR16, 10 },
        { "R11W", AR_GPR16, 11 },
        { "R12W", AR_GPR16, 12 },
        { "R13W", AR_GPR16, 13 },
        { "R14W", AR_GPR16, 14 },
        { "R15W", AR_GPR16, 15 },
        /* 8-bit GPRs (with REX) */
        { "AL", AR_GPR8, 0 },
        { "CL", AR_GPR8, 1 },
        { "DL", AR_GPR8, 2 },
        { "BL", AR_GPR8, 3 },
        { "SPL", AR_GPR8, 4 },
        { "BPL", AR_GPR8, 5 },
        { "SIL", AR_GPR8, 6 },
        { "DIL", AR_GPR8, 7 },
        { "R8B", AR_GPR8, 8 },
        { "R9B", AR_GPR8, 9 },
        { "R10B", AR_GPR8, 10 },
        { "R11B", AR_GPR8, 11 },
        { "R12B", AR_GPR8, 12 },
        { "R13B", AR_GPR8, 13 },
        { "R14B", AR_GPR8, 14 },
        { "R15B", AR_GPR8, 15 },
        /* 8-bit high regs (legacy) */
        { "AH", AR_AHHIGH, 4 },
        { "CH", AR_AHHIGH, 5 },
        { "DH", AR_AHHIGH, 6 },
        { "BH", AR_AHHIGH, 7 },
        /* Segment regs */
        { "CS", AR_SEG, 1 },
        { "SS", AR_SEG, 2 },
        { "DS", AR_SEG, 3 },
        { "ES", AR_SEG, 0 },
        { "FS", AR_SEG, 4 },
        { "GS", AR_SEG, 5 },
        /* x87 stack */
        { "ST0", AR_X87, 0 },
        { "ST1", AR_X87, 1 },
        { "ST2", AR_X87, 2 },
        { "ST3", AR_X87, 3 },
        { "ST4", AR_X87, 4 },
        { "ST5", AR_X87, 5 },
        { "ST6", AR_X87, 6 },
        { "ST7", AR_X87, 7 },
        { "ST", AR_X87, 0 },
        /* SSE XMM */
        { "XMM0", AR_XMM, 0 },
        { "XMM1", AR_XMM, 1 },
        { "XMM2", AR_XMM, 2 },
        { "XMM3", AR_XMM, 3 },
        { "XMM4", AR_XMM, 4 },
        { "XMM5", AR_XMM, 5 },
        { "XMM6", AR_XMM, 6 },
        { "XMM7", AR_XMM, 7 },
        { "XMM8", AR_XMM, 8 },
        { "XMM9", AR_XMM, 9 },
        { "XMM10", AR_XMM, 10 },
        { "XMM11", AR_XMM, 11 },
        { "XMM12", AR_XMM, 12 },
        { "XMM13", AR_XMM, 13 },
        { "XMM14", AR_XMM, 14 },
        { "XMM15", AR_XMM, 15 },
        { NULL, 0, 0 },
    };
    static const struct {
        const char *n;
        AsmRegClass cls;
        int num;
    } a64_table[] = {
        /* 64-bit GPRs X0..X30 */
        { "X0", AR_A64_X, 0 },
        { "X1", AR_A64_X, 1 },
        { "X2", AR_A64_X, 2 },
        { "X3", AR_A64_X, 3 },
        { "X4", AR_A64_X, 4 },
        { "X5", AR_A64_X, 5 },
        { "X6", AR_A64_X, 6 },
        { "X7", AR_A64_X, 7 },
        { "X8", AR_A64_X, 8 },
        { "X9", AR_A64_X, 9 },
        { "X10", AR_A64_X, 10 },
        { "X11", AR_A64_X, 11 },
        { "X12", AR_A64_X, 12 },
        { "X13", AR_A64_X, 13 },
        { "X14", AR_A64_X, 14 },
        { "X15", AR_A64_X, 15 },
        { "X16", AR_A64_X, 16 },
        { "X17", AR_A64_X, 17 },
        { "X18", AR_A64_X, 18 },
        { "X19", AR_A64_X, 19 },
        { "X20", AR_A64_X, 20 },
        { "X21", AR_A64_X, 21 },
        { "X22", AR_A64_X, 22 },
        { "X23", AR_A64_X, 23 },
        { "X24", AR_A64_X, 24 },
        { "X25", AR_A64_X, 25 },
        { "X26", AR_A64_X, 26 },
        { "X27", AR_A64_X, 27 },
        { "X28", AR_A64_X, 28 },
        { "X29", AR_A64_X, 29 },
        { "FP", AR_A64_X, 29 },
        { "X30", AR_A64_X, 30 },
        { "LR", AR_A64_X, 30 },
        { "XZR", AR_A64_X, 31 },
        /* 32-bit GPRs W0..W30 */
        { "W0", AR_A64_W, 0 },
        { "W1", AR_A64_W, 1 },
        { "W2", AR_A64_W, 2 },
        { "W3", AR_A64_W, 3 },
        { "W4", AR_A64_W, 4 },
        { "W5", AR_A64_W, 5 },
        { "W6", AR_A64_W, 6 },
        { "W7", AR_A64_W, 7 },
        { "W8", AR_A64_W, 8 },
        { "W9", AR_A64_W, 9 },
        { "W10", AR_A64_W, 10 },
        { "W11", AR_A64_W, 11 },
        { "W12", AR_A64_W, 12 },
        { "W13", AR_A64_W, 13 },
        { "W14", AR_A64_W, 14 },
        { "W15", AR_A64_W, 15 },
        { "W16", AR_A64_W, 16 },
        { "W17", AR_A64_W, 17 },
        { "W18", AR_A64_W, 18 },
        { "W19", AR_A64_W, 19 },
        { "W20", AR_A64_W, 20 },
        { "W21", AR_A64_W, 21 },
        { "W22", AR_A64_W, 22 },
        { "W23", AR_A64_W, 23 },
        { "W24", AR_A64_W, 24 },
        { "W25", AR_A64_W, 25 },
        { "W26", AR_A64_W, 26 },
        { "W27", AR_A64_W, 27 },
        { "W28", AR_A64_W, 28 },
        { "W29", AR_A64_W, 29 },
        { "W30", AR_A64_W, 30 },
        { "WZR", AR_A64_W, 31 },
        /* Stack pointer (encoded as num=31, semantically SP not ZR). */
        { "SP", AR_A64_SP, 31 },
        { "WSP", AR_A64_SP, 31 },
        { NULL, 0, 0 },
    };
    const void *first = target == TASM_ARCH_ARM64 ? (const void *)a64_table :
                                                    (const void *)x86_table;
    const void *second = target == TASM_ARCH_ARM64 ? (const void *)x86_table :
                                                     (const void *)a64_table;
    /* Search the target's primary table first; fall back to the other so
     * (target-irrelevant) regs like XMM still resolve in cross-arch tests. */
    for (int pass = 0; pass < 2; pass++) {
        const struct {
            const char *n;
            AsmRegClass cls;
            int num;
        } *tab = pass ? second : first;
        for (int i = 0; tab[i].n; i++) {
            if (strcasecmp(name, tab[i].n) == 0) {
                out->cls = tab[i].cls;
                out->num = tab[i].num;
                return 1;
            }
        }
    }
    /* arm64 SIMD/FP file: B0..B31, H0..H31, S0..S31, D0..D31, Q0..Q31,
     * V0..V31 - generated by prefix rather than a table.  All 32 slots
     * are valid hardware regs.  V<n> needs a `.<arr>` suffix (parsed by
     * the operand layer) before encoding. */
    if (target == TASM_ARCH_ARM64 && name[0] != '\0' && name[1] != '\0') {
        char head = name[0];
        AsmRegClass cls = (AsmRegClass)-1;
        if (head == 'B' || head == 'b') cls = AR_A64_B;
        else if (head == 'H' || head == 'h') cls = AR_A64_H;
        else if (head == 'S' || head == 's') cls = AR_A64_S;
        else if (head == 'D' || head == 'd') cls = AR_A64_D;
        else if (head == 'Q' || head == 'q') cls = AR_A64_Q;
        else if (head == 'V' || head == 'v') cls = AR_A64_V;
        if ((int)cls != -1) {
            /* Parse 1-2 decimal digits after the prefix; reject if anything
             * trails. */
            const char *digits = name + 1;
            int num = 0, ndig = 0;
            while (digits[ndig] >= '0' && digits[ndig] <= '9' && ndig < 2) {
                num = num * 10 + (digits[ndig] - '0');
                ++ndig;
            }
            if (ndig > 0 && digits[ndig] == '\0' && num >= 0 && num <= 31) {
                out->cls = cls;
                out->num = num;
                return 1;
            }
        }
    }
    return 0;
}

/* Size hint: U8/U16/U32/U64/I8/I16/I32/I64. Returns byte width or 0. */
#ifndef HCC_SELFHOST_ASM
int
asm_lookup_size_hint(const char *name)
{
    if (strcasecmp(name, "U8") == 0 || strcasecmp(name, "I8") == 0) return 1;
    if (strcasecmp(name, "U16") == 0 || strcasecmp(name, "I16") == 0) return 2;
    if (strcasecmp(name, "U32") == 0 || strcasecmp(name, "I32") == 0) return 4;
    if (strcasecmp(name, "U64") == 0 || strcasecmp(name, "I64") == 0) return 8;
    return 0;
}
#else
int asm_lookup_size_hint(const char *name);
#endif

/* ================================================================ parser */

static void
line_push_operand(AsmLine *ln, AsmOperand op)
{
    int new_n = ln->n_operands + 1;
    ln->operands = xrealloc(ln->operands, sizeof(AsmOperand) * (size_t)new_n);
    ln->operands[ln->n_operands++] = op;
}

static void
block_push(AsmBlock *b, AsmLine ln)
{
    if (b->n_lines == b->cap_lines) {
        b->cap_lines = b->cap_lines ? b->cap_lines * 2 : 32;
        b->lines = xrealloc(b->lines, sizeof(AsmLine) * (size_t)b->cap_lines);
    }
    b->lines[b->n_lines++] = ln;
}

/* Skip blank newlines. */
static void
asm_skip_newlines(AsmLex *l)
{
    while (asm_lex_peek_tok(l) == ATK_NEWLINE) {
        asm_lex_consume(l);
    }
}

/* Parse a single operand. Stops on `,` or newline or EOF. */
static AsmOperand
asm_parse_operand(AsmLex *l)
{

    AsmOperand op = { 0 };
    int width = 0;
    int has_seg = 0;
    AsmReg seg = { 0 };
    int neg = 0;

    /* Leading sign for an immediate or displacement. */
    while (asm_lex_peek_tok(l) == ATK_PUNCT && (l->punc == '+' || l->punc == '-')) {
        if (l->punc == '-') neg = !neg;
        asm_lex_consume(l);
    }

    /* Optional size hint and segment-prefix, possibly multiple times. */
    while (asm_lex_peek_tok(l) == ATK_IDENT) {
        /* peek without consuming text: we need to look at l->text */
        /* But asm_lex_peek_tok stores it. Look at l->text. */
        int w = asm_lookup_size_hint(l->text);
        if (w) {
            width = w;
            asm_lex_consume(l);
            continue;
        }
        AsmReg sr;
        if (asm_lookup_register(l->text, l->target, &sr) && sr.cls == AR_SEG) {
            /* Could be `FS:` (segment prefix to a memory op) or a bare seg
             * register operand. Peek next non-text token. */
            /* Cache name and consume; then check for `:`. */
            char *saved = xstrdup(l->text);
            asm_lex_consume(l);
            if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == ':') {
                asm_lex_consume(l);
                seg = sr;
                has_seg = 1;
                xfree(saved);
                continue;
            }
            /* Bare segment register operand. */
            op.kind = AOP_REG;
            op.reg = sr;
            op.width = 2;
            xfree(saved);
            return op;
        }
        break;
    }

    /* Now the operand body. Cases:
     *   - `[` ... `]`  -> memory operand with disp = 0
     *   - identifier `.` identifier `[` ... `]` -> class member offset memory op
     *   - identifier `[` ... `]` -> symbol+base memory op
     *   - identifier -> register or label/symbol
     *   - integer -> immediate
     *   - integer `[` ... `]` -> memory with int disp
     */
    AsmTok t = asm_lex_peek_tok(l);

    if (t == ATK_IDENT) {
        /* AOP_SHIFT: bare `lsl|lsr|asr|ror #N` as a 4th-operand shift
         * suffix on ARM64 shifted-register instructions. */
        if (l->target == TASM_ARCH_ARM64 &&
                (strcasecmp(l->text, "lsl") == 0 ||
                 strcasecmp(l->text, "lsr") == 0 ||
                 strcasecmp(l->text, "asr") == 0 ||
                 strcasecmp(l->text, "ror") == 0)) {
            int st = (strcasecmp(l->text, "lsl") == 0) ? ASHIFT_LSL :
                    (strcasecmp(l->text, "lsr") == 0)  ? ASHIFT_LSR :
                    (strcasecmp(l->text, "asr") == 0)  ? ASHIFT_ASR :
                                                         ASHIFT_ROR;
            asm_lex_consume(l);
            int64_t amt = 0;
            if (asm_lex_peek_tok(l) == ATK_INT) {
                amt = l->ival;
                asm_lex_consume(l);
            }
            op.kind = AOP_SHIFT;
            op.shift_type = st;
            op.imm = amt;
            op.width = width;
            return op;
        }
        char *name = xstrdup(l->text);
        asm_lex_consume(l);
        AsmReg r;
        op.v_lane_bits = 0;
        op.v_n_lanes = 0;
        op.v_lane_idx = -1;
        /* Register lookup first, so ARM SIMD names like `V0` aren't
         * mis-parsed as `Class.field`.  The Class.field path runs only
         * if the identifier is not a known register. */
        if (asm_lookup_register(name, l->target, &r)) {
            xfree(name);
            op.kind = AOP_REG;
            op.reg = r;
            op.width = r.cls == AR_GPR8 || r.cls == AR_AHHIGH ? 1 :
                    r.cls == AR_GPR16                         ? 2 :
                    r.cls == AR_GPR32                         ? 4 :
                    r.cls == AR_GPR64                         ? 8 :
                    r.cls == AR_XMM                           ? 16 :
                    r.cls == AR_A64_B                         ? 1 :
                    r.cls == AR_A64_H                         ? 2 :
                    r.cls == AR_A64_S                         ? 4 :
                    r.cls == AR_A64_D                         ? 8 :
                    r.cls == AR_A64_Q                         ? 16 :
                                                                0;
            /* For ARM SIMD V/B/H/S/D regs, an optional `.<arr>` or
             * `.E[i]` suffix may follow.  Q regs don't use it. */
            int can_suffix = (r.cls == AR_A64_V || r.cls == AR_A64_B ||
                    r.cls == AR_A64_H || r.cls == AR_A64_S ||
                    r.cls == AR_A64_D);
            if (can_suffix && asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == '.') {
                asm_lex_consume(l);
                /* Two forms:
                 *   .<lanes><E>      e.g. .4S .8H .16B .2D - arrangement
                 *   .<E>[idx]        e.g. .S[2]    - lane access on V/B/H/S/D
                 * After arrangement, an optional [idx] selects an indexed
                 * element variant (e.g. BFDOT V.4S, V.8H, V.2H[i]).
                 */
                int n_lanes = 0;
                if (asm_lex_peek_tok(l) == ATK_INT) {
                    n_lanes = (int)l->ival;
                    asm_lex_consume(l);
                }
                if (asm_lex_peek_tok(l) != ATK_IDENT)
                    asm_parse_err_at(l, l->line,
                            "asm: expected element kind (B/H/S/D) after '.'");
                char e = l->text[0];
                if (l->text[1] != '\0' ||
                        (e != 'B' && e != 'H' && e != 'S' && e != 'D' &&
                                e != 'b' && e != 'h' && e != 's' && e != 'd'))
                    asm_parse_err_at(l, l->line,
                            "asm: unknown element kind '%s'", l->text);
                asm_lex_consume(l);
                int lane_bits = (e == 'B' || e == 'b') ? 8 :
                        (e == 'H' || e == 'h')         ? 16 :
                        (e == 'S' || e == 's')         ? 32 :
                                                         64;
                op.v_lane_bits = lane_bits;
                op.v_n_lanes = n_lanes;
                op.v_lane_idx = -1;
                /* Optional [idx] for lane/indexed-element. */
                if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == '[') {
                    asm_lex_consume(l);
                    if (asm_lex_peek_tok(l) != ATK_INT)
                        asm_parse_err_at(l, l->line,
                                "asm: expected lane index integer");
                    op.v_lane_idx = (int)l->ival;
                    asm_lex_consume(l);
                    if (asm_lex_peek_tok(l) != ATK_PUNCT || l->punc != ']')
                        asm_parse_err_at(l, l->line,
                                "asm: expected ']' after lane index");
                    asm_lex_consume(l);
                }
            }
            return op;
        }
        /* Class.member[...] form. */
        if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == '.') {
            asm_lex_consume(l);
            if (asm_lex_peek_tok(l) != ATK_IDENT)
                asm_parse_err_at(l, l->line, "asm: expected member after '.'");
            char *member = xstrdup(l->text);
            asm_lex_consume(l);
            op.cls_name = name;
            op.member_name = member;
            /* Optional [base] follows. */
            if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == '[') {
                asm_lex_consume(l);
                if (asm_lex_peek_tok(l) == ATK_IDENT &&
                        asm_lookup_register(l->text, l->target, &r)) {
                    op.base = r;
                    op.has_base = 1;
                    asm_lex_consume(l);
                }
                if (asm_lex_peek_tok(l) != ATK_PUNCT || l->punc != ']')
                    asm_parse_err_at(l, l->line,
                            "asm: expected ']' after class.member operand");
                asm_lex_consume(l);
            }
            op.kind = AOP_MEM;
            op.width = width;
            op.seg = seg;
            op.has_seg = has_seg;
            return op;
        }
        /* Identifier followed by `[reg]` means symbol-as-displacement memory
         * operand. */
        if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == '[') {
            asm_lex_consume(l);
            op.label_name = name;
            if (asm_lex_peek_tok(l) == ATK_IDENT &&
                    asm_lookup_register(l->text, l->target, &r)) {
                op.base = r;
                op.has_base = 1;
                asm_lex_consume(l);
            }
            if (asm_lex_peek_tok(l) != ATK_PUNCT || l->punc != ']')
                asm_parse_err_at(l, l->line, "asm: expected ']'");
            asm_lex_consume(l);
            op.kind = AOP_MEM;
            op.width = width;
            op.seg = seg;
            op.has_seg = has_seg;
            return op;
        }
        /* Bare identifier - a label/symbol. Used as an immediate disp. */
        op.kind = AOP_LABEL;
        op.label_name = name;
        op.width = width;
        return op;
    }

    if (t == ATK_LOCAL_LABEL) {
        op.kind = AOP_LABEL;
        char buf[16];
        snprintf(buf, sizeof buf, "@@%lld", (long long)l->ival);
        op.label_name = xstrdup(buf);
        asm_lex_consume(l);
        return op;
    }

    if (t == ATK_INT) {
        int64_t v = neg ? -l->ival : l->ival;
        asm_lex_consume(l);
        /* Could be a memory disp `<disp>[reg]`. */
        if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == '[') {
            asm_lex_consume(l);
            AsmReg r;
            if (asm_lex_peek_tok(l) == ATK_IDENT &&
                    asm_lookup_register(l->text, l->target, &r)) {
                op.base = r;
                op.has_base = 1;
                asm_lex_consume(l);
                /* Optional + INDEX*SCALE. */
                if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == '+') {
                    asm_lex_consume(l);
                    if (asm_lex_peek_tok(l) == ATK_IDENT &&
                            asm_lookup_register(l->text, l->target, &r)) {
                        op.index = r;
                        op.has_index = 1;
                        op.scale = 1;
                        asm_lex_consume(l);
                        if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == '*') {
                            asm_lex_consume(l);
                            if (asm_lex_peek_tok(l) == ATK_INT) {
                                op.scale = (int)l->ival;
                                asm_lex_consume(l);
                            }
                        }
                    }
                }
            }
            if (asm_lex_peek_tok(l) != ATK_PUNCT || l->punc != ']')
                asm_parse_err_at(l, l->line, "asm: expected ']'");
            asm_lex_consume(l);
            op.kind = AOP_MEM;
            op.disp = v;
            op.width = width;
            op.seg = seg;
            op.has_seg = has_seg;
            return op;
        }
        op.kind = AOP_IMM;
        op.imm = v;
        op.width = width;
        return op;
    }

    if (t == ATK_FLOAT) {
        double fv = neg ? -l->fval : l->fval;
        asm_lex_consume(l);
        op.kind = AOP_IMM;
        op.is_float = 1;
        op.f64 = fv;
        /* Stash the raw bits in imm too - convenient for encoders that
         * want the bit pattern (e.g. FMOV with full 32-bit float imm). */
        union {
            float f;
            uint32_t u;
        } cvt;
        cvt.f = (float)fv;
        op.imm = (int64_t)cvt.u;
        op.width = width;
        return op;
    }

    /* `[` starts a memory operand. Two flavours, distinguished by which
     * arch we're parsing for (and by what's after the base):
     *   - x86:  `[base]` or `[base+disp]` (disp goes before in `<disp>[reg]`)
     *   - arm64:`[base]` or `[base, #disp]` (immediate offset after `,`)
     */
    if (t == ATK_PUNCT && l->punc == '[') {
        asm_lex_consume(l);
        AsmReg r;
        if (asm_lex_peek_tok(l) == ATK_IDENT &&
                asm_lookup_register(l->text, l->target, &r)) {
            op.base = r;
            op.has_base = 1;
            asm_lex_consume(l);
        }
        /* `[base + disp]` / `[base - disp]` - Intel-style displacement,
         * the form the x86 disassembler prints - and the full
         * `[base + index*scale +- disp]` the canonical formatter emits. */
        if (asm_lex_peek_tok(l) == ATK_PUNCT &&
                (l->punc == '+' || l->punc == '-')) {
            int dneg = (l->punc == '-');
            asm_lex_consume(l);
            AsmReg ir;
            if (!dneg && asm_lex_peek_tok(l) == ATK_IDENT &&
                    asm_lookup_register(l->text, l->target, &ir)) {
                op.index = ir;
                op.has_index = 1;
                op.scale = 1;
                asm_lex_consume(l);
                if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == '*') {
                    asm_lex_consume(l);
                    if (asm_lex_peek_tok(l) == ATK_INT) {
                        op.scale = (int)l->ival;
                        asm_lex_consume(l);
                    }
                }
                if (asm_lex_peek_tok(l) == ATK_PUNCT &&
                        (l->punc == '+' || l->punc == '-')) {
                    dneg = (l->punc == '-');
                    asm_lex_consume(l);
                    if (asm_lex_peek_tok(l) == ATK_INT) {
                        op.disp = dneg ? -l->ival : l->ival;
                        asm_lex_consume(l);
                    } else {
                        asm_parse_err_at(l, l->line,
                                "asm: expected displacement after '%c'",
                                dneg ? '-' : '+');
                    }
                }
            } else if (asm_lex_peek_tok(l) != ATK_INT) {
                asm_parse_err_at(l, l->line,
                        "asm: expected displacement after '%c'", dneg ? '-' : '+');
            } else {
                op.disp = dneg ? -l->ival : l->ival;
                asm_lex_consume(l);
            }
        }
        /* Inside the brackets, after the base we accept one of:
         *   `, #imm`                - immediate displacement
         *   `, Xm`                  - register offset
         *   `, Xm, LSL #N`          - scaled register offset
         * x86's `+disp` form is handled above. */
        if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == ',') {
            asm_lex_consume(l);
            int dneg = 0;
            if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == '-') {
                dneg = 1;
                asm_lex_consume(l);
            }
            if (asm_lex_peek_tok(l) == ATK_INT) {
                op.disp = dneg ? -l->ival : l->ival;
                asm_lex_consume(l);
            } else if (asm_lex_peek_tok(l) == ATK_IDENT) {
                AsmReg r2;
                if (asm_lookup_register(l->text, l->target, &r2)) {
                    op.index = r2;
                    op.has_index = 1;
                    op.scale = 1;
                    asm_lex_consume(l);
                    /* Optional `, LSL #N`. */
                    if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == ',') {
                        int save_pos = l->pos;
                        AsmTok save_kind = l->have_kind;
                        int save_have = l->have;
                        asm_lex_consume(l);
                        if (asm_lex_peek_tok(l) == ATK_IDENT &&
                                (strcasecmp(l->text, "lsl") == 0)) {
                            asm_lex_consume(l);
                            if (asm_lex_peek_tok(l) == ATK_INT) {
                                op.extend_lsl = (int)l->ival;
                                asm_lex_consume(l);
                            }
                        } else {
                            /* Not LSL - roll back the comma. */
                            l->pos = save_pos;
                            l->have_kind = save_kind;
                            l->have = save_have;
                        }
                    }
                }
            }
        }
        if (asm_lex_peek_tok(l) != ATK_PUNCT || l->punc != ']')
            asm_parse_err_at(l, l->line, "asm: expected ']'");
        asm_lex_consume(l);

        /* After `]`: optional `!` for pre-index writeback, or `,` for the
         * post-index `[Xn], #imm` form.  Both are arm64-only constructs;
         * the lexer doesn't see them on x86 paths because x86 callers
         * don't write them. */
        if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == '!') {
            asm_lex_consume(l);
            op.writeback = 1;
        } else if (l->target == TASM_ARCH_ARM64 &&
                   asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == ',') {
            /* Save state and try to parse `, #imm` - if the next thing
             * doesn't look like an immediate, leave it for the outer
             * operand-list comma. */
            int save_pos = l->pos;
            AsmTok save_kind = l->have_kind;
            int save_have = l->have;
            asm_lex_consume(l);
            int dneg = 0;
            if (asm_lex_peek_tok(l) == ATK_PUNCT && l->punc == '-') {
                dneg = 1;
                asm_lex_consume(l);
            }
            if (asm_lex_peek_tok(l) == ATK_INT) {
                op.disp = dneg ? -l->ival : l->ival;
                op.post_index = 1;
                asm_lex_consume(l);
            } else {
                /* Not a post-index disp; roll back so the outer
                 * operand parser sees the comma. */
                l->pos = save_pos;
                l->have_kind = save_kind;
                l->have = save_have;
            }
        }
        op.kind = AOP_MEM;
        op.width = width;
        op.seg = seg;
        op.has_seg = has_seg;
        return op;
    }

    asm_parse_err_at(l, l->line, "asm: unexpected token starting operand");
    return op;
}

/* Lowercase the mnemonic and apply Terry-aliases. Returns a freshly malloc'd
 * string the caller owns. */
#ifndef HCC_SELFHOST_ASM
char *
normalise_mnemonic(const char *m)
{
    char *out = xstrdup(m);
    for (char *p = out; *p; p++)
        *p = (char)tolower((unsigned char)*p);
    /* Terry-isms -> standard forms. */
    if (strcmp(out, "rep_movsb") == 0) {
        xfree(out);
        return xstrdup("rep_movsb");
    }
    /* Keep `ret1` as-is, it's caller-pops `ret imm16`; encoder picks it up. */
    return out;
}

/* Returns 1 if mnemonic is a recognized data directive. */
int
is_directive(const char *m)
{
    return strcasecmp(m, "DB")   == 0 || strcasecmp(m, "DW") == 0 ||
           strcasecmp(m, "DD")   == 0 || strcasecmp(m, "DQ") == 0 ||
           strcasecmp(m, "DU32") == 0 || strcasecmp(m, "DU16") == 0 ||
           strcasecmp(m, "DU8")  == 0 || strcasecmp(m, "DU64") == 0;
}
#else
char *normalise_mnemonic(const char *m);
int is_directive(const char *m);
#endif

void
asm_line_set_col(AsmLex *l, AsmLine *ln)
{
    l->tok_start_col = (l->start - l->line_start_ptr)+1;
    ln->col = l->tok_start_col;
}

AsmBlock *
asm_parse_with_handler(const char *body,
                       int body_len,
                       const char *src_file,
                       int src_line,
                       Target target,
                       void *errctx,
                       asm_error_handler *handler)
{
    AsmBlock *b = xcalloc(1, sizeof *b);
    b->src_file = src_file;
    b->src_line = src_line;
    b->target = target;

    AsmLex l;
    asm_lex_init(&l, body, body_len, src_file, src_line, target);
    if (handler) {
        l.err_handler.handler = handler;
        l.err_handler.data = errctx;
    }

    for (;;) {
        AsmLine ln = { 0 };
        asm_skip_newlines(&l);
        AsmTok t = asm_lex_peek_tok(&l);
        if (t == ATK_EOF)
            break;


        ln.line = l.line;
        asm_line_set_col(&l,&ln);

        /* Local label `@@NN:`, but actually the lexer emits ATK_LOCAL_LABEL
         * for `@@NN`. The colon follows. */
        if (t == ATK_LOCAL_LABEL) {
            int n = (int)l.ival;
            asm_lex_consume(&l);
            if (asm_lex_peek_tok(&l) == ATK_PUNCT && l.punc == ':') {
                asm_lex_consume(&l);
                ln.kind = AINS_LABEL_LOCAL;
                ln.local_num = n;
                block_push(b, ln);
                /* Allow another statement on the same line. */
                continue;
            }
            asm_parse_err_at(&l, l.line, "asm: expected ':' after @@%d label", n);
        }

        if (t == ATK_IDENT) {
            char *name = xstrdup(l.text);
            asm_lex_consume(&l);

            /* ARM64 conditional branches use a `.` between mnemonic and
             * condition (e.g. `B.GE`). If we see `IDENT . IDENT` at the
             * start of a line, glue them into a single mnemonic. */
            if (l.target == TASM_ARCH_ARM64 &&
                asm_lex_peek_tok(&l) == ATK_PUNCT &&
                l.punc == '.')
            {
                int save = l.pos;
                AsmTok save_kind = l.have_kind;
                int save_have = l.have;
                asm_lex_consume(&l);
                if (asm_lex_peek_tok(&l) == ATK_IDENT) {
                    size_t a = strlen(name), b = strlen(l.text);
                    char *joined = xmalloc(a + 1 + b + 1);
                    memcpy(joined, name, a);
                    joined[a] = '.';
                    memcpy(joined + a + 1, l.text, b + 1);
                    xfree(name);
                    name = joined;
                    asm_lex_consume(&l);
                } else {
                    /* Roll back: it wasn't a B.cond style mnemonic. */
                    l.pos = save;
                    l.have_kind = save_kind;
                    l.have = save_have;
                }
            }

            /* Label `NAME:` or `NAME::`. */
            if (asm_lex_peek_tok(&l) == ATK_PUNCT && l.punc == ':') {
                asm_lex_consume(&l);
                ln.kind = AINS_LABEL_LOCAL;
                ln.label_name = name;
                /* Use AINS_LABEL_LOCAL for both since we only differentiate
                 * by leading underscore for symbol export, public labels are
                 * named with leading `_` per Terry's convention. */
                block_push(b, ln);
                continue;
            }
            if (asm_lex_peek_tok(&l) == ATK_DOUBLECOLON) {
                asm_lex_consume(&l);
                ln.kind = AINS_LABEL_PUBLIC;
                ln.label_name = name;
                block_push(b, ln);
                continue;
            }

            /* Mnemonic or directive followed by operands until newline. */
            ln.kind = is_directive(name) ? AINS_DIRECTIVE : AINS_INSTR;
            ln.mnemonic = normalise_mnemonic(name);
            int width_suffix = 0;
            if (ln.kind == AINS_INSTR) {
                /* x86 REP-family prefix: glue `REP <stringop>` into a
                 * single line carrying the prefix byte. */
                if (target == TASM_ARCH_X86_64 &&
                    (!strcmp(ln.mnemonic, "rep")   ||
                     !strcmp(ln.mnemonic, "repe")  ||
                     !strcmp(ln.mnemonic, "repz")  ||
                     !strcmp(ln.mnemonic, "repne") ||
                     !strcmp(ln.mnemonic, "repnz")) &&
                    asm_lex_peek_tok(&l) == ATK_IDENT)
                {
                    ln.rep_prefix = (!strcmp(ln.mnemonic, "repne") ||
                                     !strcmp(ln.mnemonic, "repnz")) ? 0xF2
                                                                    : 0xF3;
                    xfree(ln.mnemonic);
                    ln.mnemonic = normalise_mnemonic(l.text);
                    asm_lex_consume(&l);
                }
                ln.mnemonic_id = tasm_mnemonic_lookup(
                        tasm_mnemonic_table_for(target), ln.mnemonic);
                /* x86 fallback for AT&T-style width-suffixed spellings
                 * (`movq`, `pushq`, `movzbq`, ...): resolve to the base
                 * mnemonic and remember the operand width the suffix
                 * implies so unsized memory operands inherit it. */
                if (ln.mnemonic_id == TASM_MN_UNKNOWN &&
                    target == TASM_ARCH_X86_64)
                {
                    ln.mnemonic_id = x86_64_suffixed_mnemonic(ln.mnemonic,
                                                              &width_suffix);
                }
            }
            xfree(name);

            /* Parse operands. */
            while (asm_lex_peek_tok(&l) != ATK_NEWLINE &&
                    asm_lex_peek_tok(&l) != ATK_EOF) {
                AsmOperand op = asm_parse_operand(&l);
                line_push_operand(&ln, op);
                if (asm_lex_peek_tok(&l) == ATK_PUNCT && l.punc == ',') {
                    asm_lex_consume(&l);
                    /* A trailing comma is a line-continuation marker. */
                    asm_skip_newlines(&l);
                    continue;
                }
                if (asm_lex_peek_tok(&l) == ATK_PUNCT && l.punc == ';') {
                    /* HolyC sometimes uses `;` to terminate directives. */
                    asm_lex_consume(&l);
                    break;
                }
                if (asm_lex_peek_tok(&l) == ATK_NEWLINE ||
                        asm_lex_peek_tok(&l) == ATK_EOF)
                    break;
                asm_parse_err_at(&l, l.line,
                        "asm: expected ',' or end-of-line between operands");
            }

            /* A width-suffixed mnemonic sizes any memory operand that
             * didn't carry its own size hint (`MOVB -16[RBP], 0`). */
            if (width_suffix) {
                for (int oi = 0; oi < ln.n_operands; oi++) {
                    if (ln.operands[oi].kind == AOP_MEM &&
                        ln.operands[oi].width == 0)
                        ln.operands[oi].width = width_suffix;
                }
            }

            block_push(b, ln);
            continue;
        }

        asm_parse_err_at(&l, l.line, "asm: unexpected token at start of line");
    }

    b->errors = l.errors;
    xfree(l.text);
    return b;
}

AsmBlock *
asm_parse(const char *body,
          int body_len,
          const char *src_file,
          int src_line,
          Target target)
{
    return asm_parse_with_handler(body, body_len, src_file, src_line,
                                  target, NULL, NULL);
}

/* ================================================================ free + print
 */

void
asm_block_free(AsmBlock *b)
{
    if (!b) return;
    for (int i = 0; i < b->n_lines; i++) {
        AsmLine *ln = &b->lines[i];
        xfree(ln->mnemonic);
        xfree(ln->label_name);
        for (int j = 0; j < ln->n_operands; j++) {
            xfree(ln->operands[j].cls_name);
            xfree(ln->operands[j].member_name);
            xfree(ln->operands[j].label_name);
        }
        xfree(ln->operands);
    }
    xfree(b->lines);
    xfree(b);
}

static const char *
asm_reg_name(AsmReg r)
{
    static const char *names[] = {
        /* GPR64 indexed by num */
        "rax",
        "rcx",
        "rdx",
        "rbx",
        "rsp",
        "rbp",
        "rsi",
        "rdi",
        "r8",
        "r9",
        "r10",
        "r11",
        "r12",
        "r13",
        "r14",
        "r15",
    };
    static char buf[16];

    /* The r8..r15 sub-width forms suffix the 64-bit name (r8d/r8w/r8b);
     * the low eight have their legacy names. */
    static const char *low8[] = {
        "al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil",
    };

    switch (r.cls) {
    case AR_GPR64: return names[r.num & 15];
    case AR_GPR32:
        if (r.num >= 8) snprintf(buf, sizeof buf, "%sd", names[r.num & 15]);
        else snprintf(buf, sizeof buf, "e%s", names[r.num & 15] + 1);
        return buf;
    case AR_GPR16:
        if (r.num >= 8) snprintf(buf, sizeof buf, "%sw", names[r.num & 15]);
        else snprintf(buf, sizeof buf, "%s", names[r.num & 15] + 1);
        return buf;
    case AR_GPR8:
        if (r.num >= 8) {
            snprintf(buf, sizeof buf, "%sb", names[r.num & 15]);
            return buf;
        }
        return low8[r.num & 7];
    case AR_AHHIGH:
        snprintf(buf, sizeof buf, "%c%c", "abcd"[r.num - 4], 'h');
        return buf;
    case AR_SEG: {
        static const char *s[] = { "es", "cs", "ss", "ds", "fs", "gs" };
        return s[r.num];
    }
    case AR_X87: snprintf(buf, sizeof buf, "st%d", r.num); return buf;
    case AR_XMM: snprintf(buf, sizeof buf, "xmm%d", r.num); return buf;
    case AR_A64_X:
        if (r.num == 31) return "xzr";
        snprintf(buf, sizeof buf, "x%d", r.num);
        return buf;
    case AR_A64_W:
        if (r.num == 31) return "wzr";
        snprintf(buf, sizeof buf, "w%d", r.num);
        return buf;
    case AR_A64_SP: return "sp";
    case AR_A64_B: snprintf(buf, sizeof buf, "b%d", r.num); return buf;
    case AR_A64_H: snprintf(buf, sizeof buf, "h%d", r.num); return buf;
    case AR_A64_S: snprintf(buf, sizeof buf, "s%d", r.num); return buf;
    case AR_A64_D: snprintf(buf, sizeof buf, "d%d", r.num); return buf;
    case AR_A64_Q: snprintf(buf, sizeof buf, "q%d", r.num); return buf;
    case AR_A64_V: snprintf(buf, sizeof buf, "v%d", r.num); return buf;
    }
    return "?";
}

static void
asm_print_operand(FILE *f, AsmOperand *op)
{
    if (op->width && op->kind == AOP_MEM) fprintf(f, "u%d ", op->width * 8);
    if (op->has_seg) fprintf(f, "%s:", asm_reg_name(op->seg));
    switch (op->kind) {
    case AOP_REG: fprintf(f, "%s", asm_reg_name(op->reg)); break;
    case AOP_IMM: fprintf(f, "#%lld", (long long)op->imm); break;
    case AOP_LABEL: fprintf(f, "%s", op->label_name); break;
    case AOP_MEM:
        if (op->cls_name) fprintf(f, "%s.%s", op->cls_name, op->member_name);
        else if (op->label_name) fprintf(f, "%s", op->label_name);
        else if (op->disp) fprintf(f, "%lld", (long long)op->disp);
        fputc('[', f);
        if (op->has_base) fputs(asm_reg_name(op->base), f);
        if (op->has_index) fprintf(f, "+%s*%d", asm_reg_name(op->index), op->scale);
        fputc(']', f);
        break;
    default: fputs("?", f); break;
    }
}

void
asm_block_print(FILE *f, AsmBlock *b, int indent)
{
    for (int i = 0; i < indent; i++)
        fputs("  ", f);

    if (!b) {
        fputs("ASM <null>\n", f);
        return;
    }

    fprintf(f, "ASM (%d lines, src %s:%d)\n", b->n_lines,
            b->src_file ? b->src_file : "?", b->src_line);

    for (int i = 0; i < b->n_lines; i++) {
        AsmLine *ln = &b->lines[i];
        for (int j = 0; j < indent + 1; j++)
            fputs("  ", f);
        switch (ln->kind) {
        case AINS_LABEL_LOCAL:
            if (ln->label_name) fprintf(f, "%s:\n", ln->label_name);
            else fprintf(f, "@@%d:\n", ln->local_num);
            break;
        case AINS_LABEL_PUBLIC: fprintf(f, "%s::\n", ln->label_name); break;
        case AINS_INSTR:
        case AINS_DIRECTIVE:
            fputs(ln->mnemonic, f);
            for (int j = 0; j < ln->n_operands; j++) {
                fputs(j == 0 ? " " : ", ", f);
                asm_print_operand(f, &ln->operands[j]);
            }
            fputc('\n', f);
            break;
        default: break;
        }
    }
}


/* ---------- text formatter for one AsmLine (for -S output) ---------- */

/* Append-only writer: tracks total chars that would be written (for the
 * snprintf-style return value) while clamping to the supplied buffer. */
typedef struct {
    char *buf;
    size_t cap;
    size_t pos;   /* chars written into buf (<= cap-1 if cap>0) */
    size_t total; /* chars that would be written if buf were unbounded */
} Writer;

static void
w_init(Writer *w, char *buf, size_t cap)
{
    w->buf = buf;
    w->cap = cap;
    w->pos = 0;
    w->total = 0;
    if (cap > 0) buf[0] = '\0';
}

static void
w_putc(Writer *w, char c)
{
    if (w->cap > 0 && w->pos + 1 < w->cap) { w->buf[w->pos++] = c; w->buf[w->pos] = '\0'; }
    w->total++;
}

static void
w_puts(Writer *w, const char *s)
{
    while (*s) w_putc(w, *s++);
}

static void
w_printf(Writer *w, const char *fmt, ...)
{
    char tmp[64];
    va_list ap;
    va_start(ap, fmt);
    int n = vsnprintf(tmp, sizeof tmp, fmt, ap);
    va_end(ap);
    if (n < 0) return;
    if ((size_t)n < sizeof tmp) {
        w_puts(w, tmp);
    } else {
        /* Truncated; still account for total. */
        w_puts(w, tmp);
        w->total += (size_t)n - (sizeof tmp - 1);
    }
}

static const char *kArm64ShiftNames[] = { "lsl", "lsr", "asr", "ror" };

static void
format_reg(Writer *w, const AsmOperand *op)
{
    const AsmReg *r = &op->reg;
    if (r->cls == AR_A64_V) {
        w_printf(w, "v%d", r->num);
        if (op->v_lane_bits != 0) {
            char k = '?';
            switch (op->v_lane_bits) {
                case 8:  k = 'b'; break;
                case 16: k = 'h'; break;
                case 32: k = 's'; break;
                case 64: k = 'd'; break;
            }
            if (op->v_n_lanes > 0)
                w_printf(w, ".%d%c", op->v_n_lanes, k);
            else
                w_printf(w, ".%c", k);
            if (op->v_lane_idx >= 0)
                w_printf(w, "[%d]", op->v_lane_idx);
        }
        return;
    }
    w_puts(w, asm_reg_name(*r));
}

static void
format_mem(Writer *w, const AsmOperand *op)
{
    /* x86 size hint, e.g. "u32 ". */
    if (op->width && (op->reg.cls == AR_GPR8 || op->reg.cls == AR_GPR16 ||
                      op->reg.cls == AR_GPR32 || op->reg.cls == AR_GPR64))
        w_printf(w, "u%d ", op->width * 8);
    if (op->has_seg) w_printf(w, "%s:", asm_reg_name(op->seg));

    /* x86 memory operand (base register in an x86 class): print the
     * Intel-style `[base + index*scale + disp]` the parser reads back -
     * the arm64 comma-immediate form below is not in x86's grammar. */
    if (!op->cls_name && !op->label_name && op->has_base &&
        op->base.cls <= AR_AHHIGH && !op->writeback && !op->post_index)
    {
        w_putc(w, '[');
        w_puts(w, asm_reg_name(op->base));
        if (op->has_index) {
            w_printf(w, " + %s", asm_reg_name(op->index));
            if (op->scale > 1) w_printf(w, "*%d", op->scale);
        }
        if (op->disp)
            w_printf(w, " %c %lld", op->disp < 0 ? '-' : '+',
                     (long long)(op->disp < 0 ? -op->disp : op->disp));
        w_putc(w, ']');
        return;
    }

    /* arm64 post-index: `[Xn], #imm` - printed differently. */
    if (op->post_index) {
        w_putc(w, '[');
        if (op->has_base) w_puts(w, asm_reg_name(op->base));
        w_puts(w, "], ");
        w_printf(w, "#%lld", (long long)op->disp);
        return;
    }

    w_putc(w, '[');
    if (op->cls_name) {
        w_printf(w, "%s.%s", op->cls_name, op->member_name);
        if (op->has_base) w_printf(w, "+%s", asm_reg_name(op->base));
    } else if (op->label_name) {
        w_puts(w, op->label_name);
        if (op->has_base) w_printf(w, "+%s", asm_reg_name(op->base));
    } else if (op->has_base) {
        w_puts(w, asm_reg_name(op->base));
        if (op->has_index) {
            w_printf(w, ", %s", asm_reg_name(op->index));
            if (op->extend_lsl) w_printf(w, ", lsl #%d", op->extend_lsl);
            else if (op->scale > 1) w_printf(w, "*%d", op->scale);
        }
        if (op->disp) w_printf(w, ", #%lld", (long long)op->disp);
    } else {
        /* Plain disp / x86-style indexed without base. */
        if (op->has_index) {
            w_puts(w, asm_reg_name(op->index));
            if (op->scale > 1) w_printf(w, "*%d", op->scale);
            if (op->disp) w_printf(w, "+%lld", (long long)op->disp);
        } else if (op->disp) {
            w_printf(w, "%lld", (long long)op->disp);
        }
    }
    w_putc(w, ']');
    if (op->writeback) w_putc(w, '!');
}

static void
format_operand(Writer *w, const AsmOperand *op)
{
    switch (op->kind) {
        case AOP_REG:
            format_reg(w, op);
            break;
        case AOP_IMM:
            if (op->is_float)
                w_printf(w, "#%g", op->f64);
            else
                w_printf(w, "#%lld", (long long)op->imm);
            break;
        case AOP_LABEL:
            w_puts(w, op->label_name ? op->label_name : "?");
            break;
        case AOP_SHIFT: {
            int t = op->shift_type & 3;
            w_printf(w, "%s #%lld", kArm64ShiftNames[t], (long long)op->imm);
            break;
        }
        case AOP_MEM:
            format_mem(w, op);
            break;
        default:
            w_putc(w, '?');
            break;
    }
}

/* GNU-as flavoured formatting of one line, for feeding an external
 * assembler. Three deltas from the canonical form above:
 *
 *   - a local label `@@N` becomes `<local_prefix>N` (".LjagN"), both
 *     as a definition and as a branch operand - `@@` is this
 *     assembler's own dialect;
 *   - a memory operand carrying a symbol prints `[base, sym]`, the
 *     GOT-load spelling, rather than the `[sym+base]` class form;
 *   - a float immediate always carries a decimal point (`#0.0`) -
 *     GNU as refuses `fcmp d0, #0`.
 *
 * Symbol operands print verbatim: relocation decorations (@PAGE,
 * :lo12:) are the CALLER's dialect knowledge, baked into the operand
 * text when the line is built. Public labels print as `name:`. */
static void
format_operand_gas(Writer *w, const AsmOperand *op, const char *local_prefix)
{
    if (op->kind == AOP_LABEL && op->label_name &&
        op->label_name[0] == '@' && op->label_name[1] == '@')
    {
        w_printf(w, "%s%d", local_prefix, atoi(op->label_name + 2));
        return;
    }
    if (op->kind == AOP_IMM && op->is_float) {
        w_printf(w, "#%g", op->f64);
        /* %g drops the point for whole values; put it back. */
        if (op->f64 == (double)(long long)op->f64) w_puts(w, ".0");
        return;
    }
    if (op->kind == AOP_MEM && op->label_name && op->has_base) {
        w_putc(w, '[');
        w_puts(w, asm_reg_name(op->base));
        w_printf(w, ", %s", op->label_name);
        w_putc(w, ']');
        return;
    }
    format_operand(w, op);
}

int
asm_line_format_gas(const AsmLine *ln, const char *local_prefix, char *buf,
                    size_t buflen)
{
    Writer w; w_init(&w, buf, buflen);
    if (!ln) { return 0; }
    switch (ln->kind) {
        case AINS_LABEL_LOCAL:
            if (ln->label_name) w_printf(&w, "%s:", ln->label_name);
            else w_printf(&w, "%s%d:", local_prefix, ln->local_num);
            break;
        case AINS_LABEL_PUBLIC:
            w_printf(&w, "%s:", ln->label_name ? ln->label_name : "?");
            break;
        case AINS_INSTR:
        case AINS_DIRECTIVE:
            w_printf(&w, "%-8s", ln->mnemonic ? ln->mnemonic : "");
            for (int i = 0; i < ln->n_operands; i++) {
                w_puts(&w, i == 0 ? " " : ", ");
                format_operand_gas(&w, &ln->operands[i], local_prefix);
            }
            break;
        default:
            break;
    }
    return (int)w.total;
}

/* x86 flavour of the GNU-as formatting, for `.intel_syntax noprefix`
 * output. The canonical form is already Intel-shaped; the deltas are
 * the pieces GNU as spells differently:
 *
 *   - immediates print bare - `#` opens a comment in some GAS modes;
 *   - a memory size hint spells `byte/word/dword/qword ptr`, not the
 *     TempleOS `u8`/`u16`/... prefix;
 *   - `lea r, sym` prints `lea r, [rip + sym]` - the encoder emits the
 *     RIP-relative form and the Intel spelling has to say so, or the
 *     assembler makes it an absolute disp32;
 *   - `mov r, sym@GOTPCREL` prints `mov r, qword ptr [rip + sym@GOTPCREL]`;
 *   - `movsx` with a 4-byte source is spelled `movsxd` (GNU as rejects
 *     the movsx form for a dword source).
 *
 * Labels and everything else follow asm_line_format_gas. */
static void
format_mem_gas_x86(Writer *w, const AsmOperand *op)
{
    static const char *const ptr[] = {
        NULL, "byte ptr ", "word ptr ", NULL, "dword ptr ",
        NULL, NULL, NULL, "qword ptr ",
    };
    if (op->width >= 1 && op->width <= 8 && ptr[op->width])
        w_puts(w, ptr[op->width]);
    w_putc(w, '[');
    if (op->has_base) w_puts(w, asm_reg_name(op->base));
    if (op->has_index) {
        w_printf(w, " + %s", asm_reg_name(op->index));
        if (op->scale > 1) w_printf(w, "*%d", op->scale);
    }
    if (op->disp)
        w_printf(w, " %c %lld", op->disp < 0 ? '-' : '+',
                 (long long)(op->disp < 0 ? -op->disp : op->disp));
    w_putc(w, ']');
}

/* Under `.intel_syntax noprefix` a bare name is looked up as a register
 * or operator first, in any case. ELF symbols carry no prefix, so the
 * global `Fs` reads as the segment register, `Mod` as an operator, and
 * nothing guarantees an error: `call Si` assembles, as an indirect call
 * through %si. Quoting makes it a symbol again, and both GNU as and
 * clang accept the quotes.
 *
 * A name led by `_` or `.` cannot collide, which covers every Mach-O
 * symbol and every compiler-generated label. `@GOTPCREL` and the like
 * stay outside the quotes. Anything that is not a plain symbol - raw
 * `asm {}` text can write `sym+8` - is left alone. */
const char *
asm_sym_gas_x86(const char *name, char *buf, size_t buflen)
{
    if (!name || !name[0] || name[0] == '_' || name[0] == '.') return name;
    size_t len = 0;
    while (name[len] && name[len] != '@') {
        char c = name[len];
        if (!isalnum((unsigned char)c) && c != '_' && c != '$' && c != '.')
            return name;
        len++;
    }
    snprintf(buf, buflen, "\"%.*s\"%s", (int)len, name, name + len);
    return buf;
}

static void
format_operand_gas_x86(Writer *w, const AsmOperand *op,
                       const char *local_prefix)
{
    switch (op->kind) {
        case AOP_LABEL:
            if (op->label_name && op->label_name[0] == '@' &&
                op->label_name[1] == '@')
            {
                w_printf(w, "%s%d", local_prefix, atoi(op->label_name + 2));
                return;
            }
            if (op->label_name) {
                char sym[512];
                w_puts(w, asm_sym_gas_x86(op->label_name, sym, sizeof(sym)));
            } else {
                w_puts(w, "?");
            }
            return;
        case AOP_IMM:
            if (op->is_float) w_printf(w, "%g", op->f64);
            else w_printf(w, "%lld", (long long)op->imm);
            return;
        case AOP_MEM:
            format_mem_gas_x86(w, op);
            return;
        default:
            format_operand(w, op);
            return;
    }
}

int
asm_line_format_gas_x86(const AsmLine *ln, const char *local_prefix,
                        char *buf, size_t buflen)
{
    Writer w; w_init(&w, buf, buflen);
    if (!ln) { return 0; }
    switch (ln->kind) {
        case AINS_LABEL_LOCAL:
            if (ln->label_name) w_printf(&w, "%s:", ln->label_name);
            else w_printf(&w, "%s%d:", local_prefix, ln->local_num);
            break;
        case AINS_LABEL_PUBLIC:
            w_printf(&w, "%s:", ln->label_name ? ln->label_name : "?");
            break;
        case AINS_INSTR:
        case AINS_DIRECTIVE: {
            const char *mn = ln->mnemonic ? ln->mnemonic : "?";
            const AsmOperand *src =
                ln->n_operands == 2 ? &ln->operands[1] : NULL;
            /* movsx with a dword source is movsxd to GNU as. */
            if (strcmp(mn, "movsx") == 0 && src &&
                ((src->kind == AOP_REG && src->reg.cls == AR_GPR32) ||
                 (src->kind == AOP_MEM && src->width == 4)))
            {
                mn = "movsxd";
            }
            /* `lea r, sym` / `mov r, sym@GOTPCREL`: RIP-relative forms
             * whose Intel spelling is a [rip + sym] memory operand. */
            if (src && src->kind == AOP_LABEL && src->label_name &&
                !(src->label_name[0] == '@' && src->label_name[1] == '@'))
            {
                char sym[512];
                const char *spelt = asm_sym_gas_x86(src->label_name, sym,
                                                    sizeof(sym));
                if (strcmp(mn, "lea") == 0) {
                    w_printf(&w, "%-8s ", mn);
                    format_operand_gas_x86(&w, &ln->operands[0],
                                           local_prefix);
                    w_printf(&w, ", [rip + %s]", spelt);
                    break;
                }
                if (strcmp(mn, "mov") == 0 &&
                    strstr(src->label_name, "@GOTPCREL"))
                {
                    w_printf(&w, "%-8s ", mn);
                    format_operand_gas_x86(&w, &ln->operands[0],
                                           local_prefix);
                    w_printf(&w, ", qword ptr [rip + %s]", spelt);
                    break;
                }
            }
            w_printf(&w, "%-8s", mn);
            for (int i = 0; i < ln->n_operands; i++) {
                w_puts(&w, i == 0 ? " " : ", ");
                format_operand_gas_x86(&w, &ln->operands[i], local_prefix);
            }
            break;
        }
        default:
            break;
    }
    return (int)w.total;
}

/* ---------------- AT&T flavour of the x86 GNU-as formatting ----------
 *
 * The same instruction stream as asm_line_format_gas_x86, spelled the
 * other way round. Everything the dialect needs is already on the
 * operand - a register's class carries its width, a memory operand
 * carries `width` - so this is a second rendering of the same facts,
 * not a second model.
 *
 * The deltas, in the order they bite:
 *
 *   - operands print right to left: `mov dst, src` is `mov src, dst`;
 *   - registers take `%`, immediates take `$`;
 *   - `[base + index*scale + disp]` becomes `disp(base, index, scale)`,
 *     and a symbol reached PC-relatively is `sym(%rip)`;
 *   - an indirect call or jump takes `*` (`call *%rax`), which is what
 *     distinguishes it from a direct one - AT&T has no other marker;
 *   - a size suffix is required when NO operand is a register, since
 *     then nothing else says how wide the access is: `mov qword ptr
 *     [rax], 5` is `movq $5, (%rax)`. Where a register is present gas
 *     infers the width and the suffix is left off, which is also what
 *     gcc and clang emit;
 *   - the extension mnemonics are spelled by their operand widths
 *     (`movsx r64, r/m32` is `movslq`), and the sign-extend-accumulator
 *     family has entirely different names (`cqo` is `cqto`). */

static void
format_reg_att(Writer *w, AsmReg r)
{
    w_printf(w, "%%%s", asm_reg_name(r));
}

static void
format_mem_att(Writer *w, const AsmOperand *op)
{
    if (op->has_seg) {
        format_reg_att(w, op->seg);
        w_putc(w, ':');
    }
    /* A bare symbol in a memory operand is PC-relative here; the Intel
     * side spells the same thing `[rip + sym]`. */
    if (op->label_name) {
        w_puts(w, op->label_name);
        if (!op->has_base && !op->has_index) {
            w_puts(w, "(%rip)");
            return;
        }
    } else if (op->disp) {
        w_printf(w, "%lld", (long long)op->disp);
    }
    if (!op->has_base && !op->has_index) {
        /* Absolute address with no base: the displacement alone. */
        if (!op->label_name && !op->disp) w_putc(w, '0');
        return;
    }
    w_putc(w, '(');
    if (op->has_base) format_reg_att(w, op->base);
    if (op->has_index) {
        w_puts(w, ", ");
        format_reg_att(w, op->index);
        w_printf(w, ", %d", op->scale > 0 ? op->scale : 1);
    }
    w_putc(w, ')');
}

static void
format_operand_att(Writer *w, const AsmOperand *op, const char *local_prefix,
                   int indirect)
{
    switch (op->kind) {
        case AOP_LABEL:
            /* A branch target is bare; an indirect one through a symbol
             * still needs its `*`. */
            if (indirect) w_putc(w, '*');
            if (op->label_name && op->label_name[0] == '@' &&
                op->label_name[1] == '@')
            {
                w_printf(w, "%s%d", local_prefix, atoi(op->label_name + 2));
                return;
            }
            w_puts(w, op->label_name ? op->label_name : "?");
            return;
        case AOP_IMM:
            if (op->is_float) w_printf(w, "$%g", op->f64);
            else w_printf(w, "$%lld", (long long)op->imm);
            return;
        case AOP_REG:
            if (indirect) w_putc(w, '*');
            format_reg_att(w, op->reg);
            return;
        case AOP_MEM:
            if (indirect) w_putc(w, '*');
            format_mem_att(w, op);
            return;
        default:
            format_operand(w, op);
            return;
    }
}

/* The b/w/l/q for a width, or 0 when there is nothing to say. */
static char
att_suffix_for_width(int width)
{
    switch (width) {
        case 1:  return 'b';
        case 2:  return 'w';
        case 4:  return 'l';
        case 8:  return 'q';
        default: return 0;
    }
}

static int
att_reg_width(const AsmOperand *op)
{
    if (op->kind != AOP_REG) return 0;
    switch (op->reg.cls) {
        case AR_GPR8: case AR_AHHIGH: return 1;
        case AR_GPR16:                return 2;
        case AR_GPR32:                return 4;
        case AR_GPR64:                return 8;
        default:                      return 0;
    }
}

/* movsx/movzx name both widths: `movs<src><dst>`. Returns NULL when the
 * operands do not describe a pair we can spell. */
static const char *
att_extend_mnemonic(const char *mn, const AsmOperand *dst,
                    const AsmOperand *src)
{
    int is_sx = strcmp(mn, "movsx") == 0 || strcmp(mn, "movsxd") == 0;
    if (!is_sx && strcmp(mn, "movzx") != 0) return NULL;
    if (!dst || !src) return NULL;

    int dw = att_reg_width(dst);
    int sw = src->kind == AOP_REG ? att_reg_width(src) : src->width;
    char s = att_suffix_for_width(sw), d = att_suffix_for_width(dw);
    if (!s || !d) return NULL;

    static char buf[8];
    snprintf(buf, sizeof(buf), "mov%c%c%c", is_sx ? 's' : 'z', s, d);
    return buf;
}

/* Whether a mnemonic is spelled with a b/w/l/q size suffix.
 *
 * Only the integer instructions whose operands can be any of the four
 * widths. Deliberately NOT here:
 *
 *   - the SSE forms (`movsd`, `movq`, `cvtsi2sd`, `ucomisd`, ...) -
 *     their width is part of the mnemonic already, and `movsdq` is not
 *     an instruction;
 *   - branches and `setcc` - `jne` and `setne` have one operand size
 *     between them, and gas rejects a suffix on either;
 *   - the sign-extenders (`cqto`, `movslq`) - likewise already sized.
 *
 * gas can usually infer the width from a register operand and accepts
 * the bare form, but the suffix is what gcc and clang emit and is the
 * only spelling that says what a memory-only instruction does. */
static int
att_takes_suffix(const char *mn)
{
    static const char *const suffixable[] = {
        "adc", "add", "and", "cmp", "dec", "div", "idiv", "imul", "inc",
        "lea", "mov", "neg", "not", "or", "pop", "push", "sal", "sar",
        "sbb", "shl", "shr", "sub", "test", "xchg", "xor",
    };
    for (size_t i = 0; i < sizeof(suffixable)/sizeof(suffixable[0]); i++) {
        if (strcmp(mn, suffixable[i]) == 0) return 1;
    }
    return 0;
}

/* The width one operand implies, or 0 if it implies nothing. */
static int
att_operand_width(const AsmOperand *op)
{
    int w = att_reg_width(op);
    if (w) return w;
    if (op->kind == AOP_MEM && op->width) return op->width;
    return 0;
}

/* The accumulator sign-extenders are simply different words. */
static const char *
att_rename_mnemonic(const char *mn)
{
    if (strcmp(mn, "cqo")  == 0) return "cqto";
    if (strcmp(mn, "cdq")  == 0) return "cltd";
    if (strcmp(mn, "cdqe") == 0) return "cltq";
    if (strcmp(mn, "cwde") == 0) return "cwtl";
    if (strcmp(mn, "cbw")  == 0) return "cbtw";
    if (strcmp(mn, "movabs") == 0) return "movabsq";
    return mn;
}

static int
att_is_indirect_branch(const char *mn, const AsmOperand *op)
{
    if (strcmp(mn, "call") != 0 && strcmp(mn, "jmp") != 0) return 0;
    return op->kind == AOP_REG || op->kind == AOP_MEM;
}

int
asm_line_format_att_x86(const AsmLine *ln, const char *local_prefix,
                        char *buf, size_t buflen)
{
    Writer w; w_init(&w, buf, buflen);
    if (!ln) { return 0; }
    switch (ln->kind) {
        case AINS_LABEL_LOCAL:
            if (ln->label_name) w_printf(&w, "%s:", ln->label_name);
            else w_printf(&w, "%s%d:", local_prefix, ln->local_num);
            break;
        case AINS_LABEL_PUBLIC:
            w_printf(&w, "%s:", ln->label_name ? ln->label_name : "?");
            break;
        case AINS_INSTR:
        case AINS_DIRECTIVE: {
            const char *mn = ln->mnemonic ? ln->mnemonic : "?";
            const AsmOperand *dst =
                ln->n_operands >= 1 ? &ln->operands[0] : NULL;
            const AsmOperand *src =
                ln->n_operands == 2 ? &ln->operands[1] : NULL;

            const char *ext = att_extend_mnemonic(mn, dst, src);
            char sfx = 0;
            if (ext) {
                mn = ext;
            } else {
                mn = att_rename_mnemonic(mn);
                /* Destination first: operand 0 is what the instruction
                 * writes, and it names the width. Taking any register
                 * would get `sar %cl, %rax` wrong - the count is a byte
                 * and the shift is not. */
                if (att_takes_suffix(mn)) {
                    int w = 0;
                    for (int i = 0; i < ln->n_operands && !w; i++) {
                        w = att_operand_width(&ln->operands[i]);
                    }
                    sfx = att_suffix_for_width(w);
                }
            }

            char mnbuf[24];
            if (sfx) {
                snprintf(mnbuf, sizeof(mnbuf), "%s%c", mn, sfx);
                mn = mnbuf;
            }
            w_printf(&w, "%-8s", mn);

            /* `lea r, sym` and `mov r, sym@GOTPCREL` are the two places
             * the Intel side has to spell out `[rip + sym]`; here the
             * memory form falls out of the symbol operand itself. */
            int rip_src = src && src->kind == AOP_LABEL && src->label_name &&
                          !(src->label_name[0] == '@' &&
                            src->label_name[1] == '@') &&
                          (strcmp(ln->mnemonic ? ln->mnemonic : "", "lea") == 0 ||
                           strstr(src->label_name, "@GOTPCREL") != NULL);
            if (rip_src) {
                w_printf(&w, " %s(%%rip), ", src->label_name);
                format_operand_att(&w, dst, local_prefix, 0);
                break;
            }

            for (int i = ln->n_operands - 1; i >= 0; i--) {
                w_puts(&w, i == ln->n_operands - 1 ? " " : ", ");
                format_operand_att(&w, &ln->operands[i], local_prefix,
                                   ln->n_operands == 1 &&
                                   att_is_indirect_branch(
                                       ln->mnemonic ? ln->mnemonic : "",
                                       &ln->operands[i]));
            }
            break;
        }
        default:
            break;
    }
    return (int)w.total;
}

int
asm_line_format(const AsmLine *ln, char *buf, size_t buflen)
{
    Writer w; w_init(&w, buf, buflen);
    if (!ln) { return 0; }
    switch (ln->kind) {
        case AINS_LABEL_LOCAL:
            if (ln->label_name) w_printf(&w, "%s:", ln->label_name);
            else                w_printf(&w, "@@%d:", ln->local_num);
            break;
        case AINS_LABEL_PUBLIC:
            w_printf(&w, "%s::", ln->label_name ? ln->label_name : "?");
            break;
        case AINS_INSTR:
        case AINS_DIRECTIVE:
            w_printf(&w, "%-8s", ln->mnemonic ? ln->mnemonic : "");
            for (int i = 0; i < ln->n_operands; i++) {
                w_puts(&w, i == 0 ? " " : ", ");
                format_operand(&w, &ln->operands[i]);
            }
            break;
        default:
            break;
    }
    return (int)w.total;
}
