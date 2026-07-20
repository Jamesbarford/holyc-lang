/* HolyC `asm {}` block capture.
 *
 * All actual assembling is done by the libtasm assembler in src/asm,
 * which speaks TempleOS-style asm natively for both x86_64 and arm64.
 * This file's job is much smaller:
 *
 *   1. Re-render the asm body from the macro-expanded HolyC token
 *      stream into plain text libtasm can consume. Working from the
 *      token stream (not a raw source slice) means `#define`s are
 *      already substituted, comments are gone, char constants become
 *      integer literals and `sizeof(X)` folds to a number.
 *
 *   2. Track the functions a block defines (`NAME::` / `NAME:` labels)
 *      so the HolyC parser can resolve calls to them. Each function's
 *      body is captured as its own chunk - libtasm resolves `@@N`
 *      local labels per encode call, and TempleOS scopes them per
 *      function, so backends encode one function at a time.
 *
 *   3. Syntax-check each chunk with libtasm right away, routing its
 *      diagnostics into the cctrl accumulator via prsAsmDiagSink.
 *
 * Backends later re-parse + encode the captured text and patch the
 * bytes in: the JIT splices them into its executable mapping, the AOT
 * paths emit them as `.byte` runs. */

#include <string.h>
#include <stdio.h>

#include "aostr.h"
#include "asm/asm.h"
#include "asm/asm_enc.h"
#include "asm/target.h"
#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "list.h"
#include "prsasm.h"
#include "prslib.h"
#include "util.h"

Target prsAsmTasmArch(Cctrl *cc) {
    switch (cc->target) {
        case TARGET_AARCH64_APPLE_DARWIN:
        case TARGET_AARCH64_UNKNOWN_LINUX_GNU:
            return TASM_ARCH_ARM64;
        default:
            return TASM_ARCH_X86_64;
    }
}

void prsAsmCreateErrorLineAt(Cctrl *cc, AsmLine *ln, int len, AoStr *msg) {
    AoStr *buf = cctrlCreateErrorLineAt(cc,
                                        ln->line,
                                        ln->col,
                                        len,
                                        msg->data,
                                        CCTRL_ERROR,
                                        NULL);
    CctrlDiagnostic *d = cctrlMakeDiag(cc, CCTRL_ERROR, buf, NULL);
    if (ln && ln->line > 0) {
        d->line = ln->line;
        d->col = ln->col;
        d->end_line = ln->line;
        d->end_col = 10;
    }
    cctrlDiagPush(cc, d);
}

void prsAsmDiagSink(void *ctx, AsmLine *ln, const char *msg) {
    Cctrl *cc = (Cctrl *)ctx;
    /* cctrlMakeDiag defaults the position to the current token, which
     * for an asm error sits PAST the whole block - every mnemonic
     * error then collapses onto the line after `}`. libtasm knows the
     * real source line; use it so errors land on their instructions. */
    AoStr *bold_msg = aoStrNew();
    aoStrCatColoured(bold_msg, ESC_BOLD, msg);

    switch (ln->kind) {
        case AINS_NONE:
            prsAsmCreateErrorLineAt(cc, ln, -1, bold_msg);
            break;
        case AINS_INSTR:
            prsAsmCreateErrorLineAt(cc, ln, strlen(ln->mnemonic), bold_msg);
            break;
        case AINS_LABEL_PUBLIC:
        case AINS_DIRECTIVE:
        case AINS_LABEL_LOCAL:
            prsAsmCreateErrorLineAt(cc, ln, strlen(ln->label_name), bold_msg);
            break;
    }
    aoStrRelease(bold_msg);
}

static const char *prsAsmSrcFile(Cctrl *cc) {
    if (cc->lexer_ && cc->lexer_->cur_file &&
        cc->lexer_->cur_file->filename)
        return cc->lexer_->cur_file->filename->data;
    return "<asm>";
}

/* Syntax-check `text` with libtasm; errors land in cc->diagnostics as
 * CCTRL_ERROR entries (so compilation stops before codegen) without
 * aborting the parse. We both parse AND encode: many problems (unknown
 * mnemonic, bad operand combo) are only caught by the encoder, and if
 * we only parsed they'd surface at codegen - after the diagnostic flush
 * - and get silently dropped. */
static void prsAsmValidate(Cctrl *cc, AoStr *text, int src_line) {
    if (!text || text->len == 0) return;
    if (getenv("HCC_DUMP_ASM")) {
        fprintf(stderr, "=== asm chunk (%s:%d) ===\n%s\n",
                prsAsmSrcFile(cc), src_line, text->data);
    }
    AsmBlock *blk = asm_parse_with_handler(text->data, (int)text->len,
                                           prsAsmSrcFile(cc), src_line,
                                           prsAsmTasmArch(cc),
                                           cc, prsAsmDiagSink);
    AsmEnc enc;
    asm_enc_init(&enc);
    asm_enc_set_error_handler(&enc, cc, prsAsmDiagSink);
    asm_encode(blk, NULL, &enc);
    asm_enc_free(&enc);
    asm_block_free(blk);
}

/* Word-like tokens need a separating space when adjacent; punctuation
 * binds tight on both sides, which keeps `-8[RBP]` and `@@1:` intact. */
static int prsAsmTokIsWord(Lexeme *tok) {
    switch (tok->tk_type) {
        case TK_IDENT:
        case TK_KEYWORD:
        case TK_I64:
        case TK_F64:
        case TK_CHAR_CONST:
        case TK_STR:
            return 1;
        default:
            return 0;
    }
}

static void prsAsmEmitTok(AoStr *out, Lexeme *tok, int *prev_word) {
    if (tokenPunctIs(tok, '\n')) {
        aoStrPutChar(out, '\n');
        *prev_word = 0;
        return;
    }
    int word = prsAsmTokIsWord(tok);
    if (word && *prev_word) aoStrPutChar(out, ' ');
    switch (tok->tk_type) {
        case TK_CHAR_CONST:
            /* libtasm has no HolyC char-literal syntax; fold to int. */
            aoStrCatFmt(out, "%I", (s64)tok->i64);
            break;
        case TK_STR:
            aoStrCatFmt(out, "\"%.*s\"", tok->len, tok->start);
            break;
        default:
            aoStrCatLen(out, tok->start, tok->len);
            break;
    }
    *prev_word = word;
}

/* Pre-folded value (sizeof, char consts routed by the caller). */
static void prsAsmEmitNum(AoStr *out, s64 v, int *prev_word) {
    if (*prev_word) aoStrPutChar(out, ' ');
    aoStrCatFmt(out, "%I", v);
    *prev_word = 1;
}

/* Close the in-flight function chunk: build the AST_ASM_FUNCDEF,
 * register it for call resolution, syntax-check its body. */
static void prsAsmFlushFunc(Cctrl *cc, List *funcs, AoStr *curfunc,
                            AoStr *cur, int src_line)
{
    if (!curfunc) return;
    prsAsmValidate(cc, cur, src_line);
    Ast *fn = astAsmFunctionDef(curfunc, cur);
    listAppend(funcs, fn);
    if (!mapAddOrErr(cc->asm_functions, curfunc->data, fn)) {
        /* REPL/LSP reparse the same buffer against a persistent Cctrl,
         * so the asm block is genuinely redefined - shadow it like a
         * function redefinition rather than aborting. */
        if (cc->flags & CCTRL_REPL) {
            mapRemove(cc->asm_functions, curfunc->data);
            mapAdd(cc->asm_functions, curfunc->data, fn);
        } else {
            cctrlIce(cc, "Already defined assembly function: %s",
                     curfunc->data);
        }
    }
}

/* The next token is expected to be '{'; the parser has just seen `asm`. */
Ast *prsAsm(Cctrl *cc, int parse_one) {
    (void)parse_one; /* both modes capture the same way */
    AoStr *block_text = aoStrNew(); /* whole block, labels included */
    AoStr *cur = NULL;              /* current function body, label-less */
    AoStr *curfunc = NULL;
    List *funcs = listNew();
    int prev_block = 0, prev_cur = 0;
    int src_line = (int)cc->lineno;
    int func_line = src_line;

    cctrlTokenExpect(cc, '{');
    Lexeme *tok = cctrlAsmTokenGet(cc);
    /* This is so we can preserve column numbers for the assembly parser errors
     * that parse has a skip whitespace function so there is no harm in using it*/
    cc->lexer_->flags |= CCF_ACCEPT_WHITESPACE;
    
    while (tok && !tokenPunctIs(tok, '}')) {
        /* `NAME::` (or `NAME:`) opens a new function. */
        if (tok->tk_type == TK_IDENT) {
            Lexeme *next = cctrlTokenPeek(cc);
            if (tokenPunctIs(next, TK_DBL_COLON) || tokenPunctIs(next, ':')) {
                cctrlAsmTokenGet(cc);
                prsAsmFlushFunc(cc, funcs, curfunc, cur, func_line);
                curfunc = aoStrDupRaw(tok->start, tok->len);
                cc->tmp_asm_fname = curfunc;
                cur = aoStrNew();
                prev_cur = 0;
                /* `cur` collects the body AFTER the `NAME::\n` label,
                 * so its first line is the one below the label - anchor
                 * validation there or every asm error reports one line
                 * high. */
                func_line = (int)tok->line + 1;
                aoStrCatFmt(block_text, "%S::\n", curfunc);
                prev_block = 0;
                tok = cctrlAsmTokenGet(cc);
                continue;
            }
        }

        if (tok->tk_type == TK_KEYWORD) {
            if (tok->i64 == KW_SIZEOF) {
                Ast *sizeof_ast = parseSizeof(cc);
                prsAsmEmitNum(block_text, (s64)sizeof_ast->i64, &prev_block);
                if (cur) prsAsmEmitNum(cur, (s64)sizeof_ast->i64, &prev_cur);
                tok = cctrlAsmTokenGet(cc);
                continue;
            }
            cctrlRaiseException(cc,
                    "Cannot handle keyword `%.*s` inside an asm block",
                    tok->len, tok->start);
        }

        prsAsmEmitTok(block_text, tok, &prev_block);
        if (cur) prsAsmEmitTok(cur, tok, &prev_cur);
        tok = cctrlAsmTokenGet(cc);
    }

    prsAsmFlushFunc(cc, funcs, curfunc, cur, func_line);

    /* Inline statements (parse_one) have no function labels; the whole
     * body is the statement. Validate it as one chunk. */
    if (!curfunc) {
        prsAsmValidate(cc, block_text, src_line);
    }

    return astAsmBlock(block_text, funcs);
}
