#include <string.h>
#include <stdio.h>

#include "aostr.h"
#include "asm.h"
#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "list.h"
#include "prsasm.h"
#include "prslib.h"
#include "util.h"

typedef struct AsmUnit {
    AoStr *op1;
    AoStr *op2;
    AoStr *op3;
    int op_count;
} AsmUnit;

/* format the assembly based on how many characters there are for the 
 * mneumonic */
static inline char *getTabs(AoStr *str) {
    switch (str->len) {
        case 2: return "\t\t";
        case 3: return "\t\t";
        case 4: return "\t";
        default: return "\t";
    }
}

void prsAsmMem(Cctrl *cc, AoStr *buf) {
    Lexeme *tok;
    tok = cctrlAsmTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        cctrlRaiseException(cc,"[<register>] expected got: %s",lexemeToString(tok));
    }
    cctrlTokenExpect(cc,']');
    aoStrCatFmt(buf,"(%.*s)",tok->len,tok->start);
    aoStrToLowerCase(buf);
}

void prsAsmOffset(Cctrl *cc, AoStr *buf, Lexeme *tok) {
    if (tok->tk_type != TK_I64) {
        cctrlRaiseException(cc,"Expected TK_I64 type got: %s",lexemeToString(tok));
    }

    cctrlTokenExpect(cc,'[');
    tok = cctrlAsmTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        cctrlRaiseException(cc,"Expected <number>[<register>] Got: %s",
                lexemeToString(tok));
    }
    cctrlTokenExpect(cc,']');
    aoStrCatFmt(buf, "(%.*s)",tok->len,tok->start);
}

void prsAsmImm(Cctrl *cc, AoStr *buf, Lexeme *tok) {
    Lexeme *next;
    switch (tok->tk_type) {
        case TK_PUNCT:
            if (tok->i64 != '-') {
                cctrlRaiseException(cc,"Expected '-'<numerical>");
            }

            tok = cctrlAsmTokenGet(cc);
            if (tok->tk_type != TK_I64 && tok->tk_type != TK_F64) {
                cctrlRaiseException(cc,"Expected -<numerical> got: %s\n",
                        lexemeToString(tok));
            }
            next = cctrlTokenPeek(cc);
            if (!tokenPunctIs(next,'[')) {
                aoStrPutChar(buf,'$');
                aoStrCatPrintf(buf, "-%zu",(unsigned long)tok->i64);
            } else {
                aoStrCatPrintf(buf, "-%zu",(unsigned long)tok->i64);
                cctrlAsmTokenGet(cc);
                prsAsmMem(cc,buf);
            }
            break;

        case TK_CHAR_CONST:
        case TK_I64:
            next = cctrlTokenPeek(cc);
            if (!tokenPunctIs(next,'[')) {
                aoStrPutChar(buf,'$');
                if (tok->ishex) {
                    aoStrCatPrintf(buf, "%#llX",(size_t)tok->i64);
                } else { 
                    aoStrCatPrintf(buf, "%zu",(size_t)tok->i64);
                }
            } else {
                if (tok->ishex) {
                    aoStrCatPrintf(buf, "%#llX",(size_t)tok->i64);
                } else { 
                    aoStrCatPrintf(buf, "%zu",(size_t)tok->i64);
                }
                if (tokenPunctIs(next,'[')) {
                    cctrlAsmTokenGet(cc);
                    prsAsmMem(cc,buf);
                }
            }
            break;
        default:
            printf("mem parse: %s\n", lexemeToString(tok));
            break;
    }
}

void prsAsmLabel(Cctrl *cc, AoStr *buf) {
    Lexeme *tok = cctrlAsmTokenGet(cc);
    if (!tokenPunctIs(tok,'@')) {
        cctrlRaiseException(cc,": Labels must be: '@@<int>'");
    }

    tok = cctrlAsmTokenGet(cc);
    if (tok->tk_type != TK_I64) {
        cctrlRaiseException(cc,": Labels must be: '@@<int>'");
    }
    
    s64 label_num = tok->i64;
    Lexeme *next = cctrlTokenPeek(cc);
    AoStr *label = aoStrDup(cc->tmp_asm_fname);
    aoStrToLowerCase(label);
    if (tokenPunctIs(next,':')) {
        aoStrCatFmt(buf, ".%S_%U:",label,label_num);
        cctrlAsmTokenGet(cc);
    } else {
        aoStrCatFmt(buf, ".%S_%U",label,label_num);
    }
    aoStrRelease(label);
}

void prsAsmPunct(Cctrl *cc, Lexeme *tok, AoStr *buf) {
    switch (tok->i64) {
        case '[':
            prsAsmMem(cc,buf);
            break;
        case '-':
            prsAsmImm(cc,buf,tok);
            break;
        case '@':
            prsAsmLabel(cc,buf);
            break;
        default:
            lexemePrint(tok);
            cctrlRaiseException(cc,": Unexpected character");
    }

    Lexeme *next = cctrlTokenPeek(cc);
    if (tokenPunctIs(next,'[')) {
        cctrlTokenRewind(cc);
        tok = cctrlAsmTokenGet(cc);
        prsAsmOffset(cc,buf,tok);
    }
}

/* Custom assembly to AT&T */
/* Format used to placeholder-mark a TempleOS `&y` operand inside an
 * AT&T asm body. The numeric suffix is the index into `all_lvars`
 * built up during parsing; after each instruction is emitted into
 * `curblock`, we walk the freshly-written text looking for these
 * markers and split into AsmFragment::TEXT / ASM_FRAG_LVAR_REF runs
 * so codegen can resolve to `<loff>(%rbp)` once layout has assigned
 * loffs.
 *
 * `__hcclv_` is intentionally lowercase + underscore-prefixed so it
 * survives `aoStrToLowerCase(op1)` and won't collide with any token
 * a user might reasonably write in inline asm. */
#define ATT_LVAR_PLACEHOLDER_PREFIX "__hcclv_"
#define ATT_LVAR_PLACEHOLDER_SUFFIX "__"

Ast *prsAsmToATT(Cctrl *cc, int parse_one) {
    AoStr *op1 = NULL, *op2 = NULL, *op3 = NULL, *asm_code = NULL, *curblock = NULL, *curfunc = NULL;
    List *asm_functions;
    Ast *asm_function, *asm_block;
    Lexeme *tok, *next;
    int isbol = 0, count = 0;

    asm_functions = listNew();
    asm_code = aoStrNew();
    curblock = parse_one ? asm_code : NULL;
    memset(asm_code->data,'\0',asm_code->capacity-1);

    /* TempleOS `&var` references: every `&IDENT` whose IDENT is a
     * local in the surrounding HolyC function records the lvar here
     * and stamps a `__hcclv_<idx>__` placeholder into the operand
     * buffer. After each instruction flush we scan curblock for the
     * placeholders and emit fragments. fragments stays empty for
     * asm blocks that don't use the feature - we attach it only when
     * non-empty, so the plain-text path is unchanged for those. */
    Vec *all_lvars = astVecNew();
    List *fragments = listNew();
    s64 last_flush_pos = 0;

    cctrlTokenExpect(cc, '{');
    tok = cctrlAsmTokenGet(cc);

    while (!tokenPunctIs(tok, '}')) {
        switch (tok->tk_type) {
            case TK_CHAR_CONST:
            case TK_I64: {
                switch (count) {
                    case 1:
                        count++;
                        op2 = aoStrNew();
                        prsAsmImm(cc,op2,tok);
                        break;
                    case 2:
                        count++;
                        op3 = aoStrNew();
                        prsAsmImm(cc,op3,tok);
                        break;
                }
                break;
            }

            case TK_KEYWORD: {
                if (tok->i64 == KW_SIZEOF) {
                    Ast *sizeof_ast = parseSizeof(cc);
                    u64 size = (unsigned long)sizeof_ast->i64;
                    switch (count) {
                        case 1:
                            count++;
                            op2 = aoStrPrintf("$%zu",size);
                            break;
                        case 2:
                            count++;
                            op3 = aoStrPrintf("$%zu",size);
                            break;
                    }
                } else {
                    cctrlRaiseException(cc, "\nCannot Handle keyword: %.*s in this context", tok->len, tok->start);
                }
                break;
            }    


            case TK_IDENT: {
                next = cctrlTokenPeek(cc);
                if (tokenPunctIs(next, TK_DBL_COLON) || tokenPunctIs(next,':')) {
                    cctrlAsmTokenGet(cc);
                    cctrlTokenExpect(cc, '\n');

                    if (curblock == NULL) {
                        curblock = aoStrNew();
                    } else {
                        /* Save the current function */
                        aoStrCatLen(asm_code,curfunc->data,curfunc->len);
                        aoStrCatLen(asm_code,":\n",2);
                        aoStrCatLen(asm_code,curblock->data,curblock->len);
                        aoStrPutChar(asm_code,'\n');
                        /* Create an ast */
                        asm_function = astAsmFunctionDef(curfunc,curblock);
                        listAppend(asm_functions,asm_function);
                        if (!mapAddOrErr(cc->asm_functions, curfunc->data, asm_function)) {
                            cctrlIce(cc, "Already defined assembly function: %s", curfunc->data);
                        }
                        curblock = aoStrNew();
                    }

                    /* Just the function name in op1, this is so it can be 
                     * called in the c code. */
                    curfunc = aoStrDupRaw(tok->start,tok->len);
                    /* Save the name for pasting labels */
                    cc->tmp_asm_fname = curfunc;
                    tok = cctrlAsmTokenGet(cc);
                    isbol = 1;
                    continue;
                } 

                else if (!isbol && count != 1 && tokenPunctIs(next, '[')) {
                    /* Possibly loading a global */
                    op3 = aoStrDupRaw(tok->start,tok->len);
                    cctrlAsmTokenGet(cc);
                    next = cctrlAsmTokenGet(cc);
                    aoStrCatFmt(op3, "(%.*s)",next->len, next->start);
                    cctrlAsmTokenGet(cc);
                } else
                if (isbol) {
                    op1 = aoStrDupRaw(tok->start,tok->len);
                    isbol = 0;
                } else {
                    if (count == 1) {
                        op2 = aoStrDupRaw(tok->start,tok->len);
                    } else {
                        op3 = aoStrDupRaw(tok->start,tok->len);
                    }
                }
                count++;
                break;
            }

            case TK_PUNCT: {
                switch (tok->i64) {
                    case '\n': {
                        /* curblock is NULL until the first `Name::`
                         * label is seen; with CCF_ACCEPT_NEWLINES on,
                         * the `\n` between `asm {` and that label
                         * arrives here first. count is 0 in that
                         * case, so the inner switch is a no-op and
                         * there's nothing to scan for placeholders. */
                        s64 inst_start = curblock ? (s64)curblock->len : 0;
                        switch (count) {
                            case 0:
                                break;

                            case 1:
                                /* Not A jump */
                                if (op1->data[0] != '.') { 
                                    aoStrPutChar(curblock,'\t');
                                }
                                /* Only a few functions get called as one 
                                 * operation */
                                aoStrToLowerCase(op1);
                                if (setHasLen(cc->libc_functions, op1->data,op1->len)) {
                                    /* Target-aware mangling: Darwin
                                     * needs a leading `_`, Linux
                                     * doesn't. asmNormaliseFunctionName
                                     * reads cc->target rather than
                                     * the build host's IS_BSD. */
                                    char *stdfn = asmNormaliseFunctionName(cc, op1);
                                    aoStrCatPrintf(curblock,"%s\n",stdfn);
                                } else {
                                    aoStrCatFmt(curblock,"%S\n",op1);
                                }
                                aoStrRelease(op1);
                                break;

                            case 2: {
                                aoStrToLowerCase(op1);
                                aoStrCatPrintf(curblock,"\t%s%s",op1->data,getTabs(op1));
                                int is_call = op1->len == 4 && !strncasecmp(op1->data,str_lit("call"));

                                if (is_call) {
                                    if (mapGetLen(cc->global_env, op2->data, op2->len) != NULL ||
                                        setHasLen(cc->libc_functions, op2->data, op2->len)) {
                                        char *stdfn = asmNormaliseFunctionName(cc, op2);
                                        aoStrCatPrintf(curblock,"%s\n",stdfn);
                                    } else {
                                        if (op2->data[0] == '%') {
                                            aoStrPutChar(curblock,'*');
                                        }
                                        aoStrCatFmt(curblock,"%S\n",op2);
                                    }
                                } else {
                                  int is_jump = op1->len == 3 && 
                                                !strncasecmp(op1->data,str_lit("jmp"));
                                    if (is_jump && op2->data[0] == '%') {
                                        aoStrPutChar(curblock,'*');
                                    }
                                    aoStrToLowerCase(op2);
                                    aoStrCatFmt(curblock,"%S\n",op2);
                                }
                                aoStrRelease(op1);
                                aoStrRelease(op2);
                                break;
                            }

                            case 3:
                                aoStrToLowerCase(op1);

                                aoStrCatFmt(curblock,"\t%S%s", op1,getTabs(op1));

                                aoStrCatFmt(curblock,"%S, %S\n",op3, op2);

                                aoStrRelease(op1);
                                aoStrRelease(op2);
                                aoStrRelease(op3);
                                break;
                            default:
                                cctrlRaiseException(cc,"Unexpected number of arguments for"
                                        " x86 transpilation: %d %s %s",
                                        count,op1->data,op2->data);
                        }
                        /* If the freshly-emitted instruction has any
                         * `__hcclv_<idx>__` placeholders, split the
                         * range [inst_start, curblock->len) into
                         * TEXT / LVAR_REF fragments. The text before
                         * the placeholder becomes a TEXT fragment;
                         * the placeholder itself maps to LVAR_REF
                         * via all_lvars[idx]; trailing text after
                         * the last placeholder is left for the next
                         * iteration (or the final flush). */
                        if (curblock && all_lvars->size > 0) {
                            s64 cursor = inst_start;
                            s64 end = (s64)curblock->len;
                            size_t pfx_len =
                                strlen(ATT_LVAR_PLACEHOLDER_PREFIX);
                            while (cursor < end) {
                                /* Bounded search for the prefix. */
                                char *hay = curblock->data + cursor;
                                size_t hay_len = (size_t)(end - cursor);
                                char *hit = NULL;
                                for (size_t i = 0;
                                     i + pfx_len <= hay_len; ++i)
                                {
                                    if (memcmp(hay + i,
                                            ATT_LVAR_PLACEHOLDER_PREFIX,
                                            pfx_len) == 0)
                                    {
                                        hit = hay + i;
                                        break;
                                    }
                                }
                                if (!hit) break;
                                s64 ph_pos = (s64)(hit - curblock->data);
                                char *idx_str = hit + pfx_len;
                                char *idx_end = NULL;
                                long long idx_val = strtoll(idx_str,
                                                            &idx_end,
                                                            10);
                                if (!idx_end ||
                                    idx_end[0] != '_' ||
                                    idx_end[1] != '_')
                                {
                                    /* Malformed - skip past the
                                     * prefix and keep scanning so a
                                     * spurious "__hcclv_" in user
                                     * text doesn't break the loop. */
                                    cursor = ph_pos + (s64)pfx_len;
                                    continue;
                                }
                                s64 ph_end =
                                    (s64)((idx_end + 2) - curblock->data);

                                if (ph_pos > last_flush_pos) {
                                    AoStr *chunk = aoStrDupRaw(
                                        curblock->data + last_flush_pos,
                                        ph_pos - last_flush_pos);
                                    listAppend(fragments,
                                            asmFragmentText(chunk));
                                }
                                Ast *lvar = vecGet(Ast *, all_lvars,
                                                   (u64)idx_val);
                                listAppend(fragments,
                                        asmFragmentLvarRef(lvar));
                                last_flush_pos = ph_end;
                                cursor = ph_end;
                            }
                        }
                        isbol = 1;
                        count = 0;
                        break;
                    }

                    case ',':
                        break;
                    case '&': {
                        /* TempleOS `&y` -> address of a HolyC local.
                         * Record the lvar, stamp a placeholder into
                         * the current operand slot; codegen resolves
                         * it once layout has assigned the loff. Only
                         * active for inline asm with a surrounding
                         * localenv - falls through to the default
                         * punct path otherwise so `&FUNCNAME`-style
                         * usages keep working. */
                        int handled = 0;
                        if (parse_one && cc->localenv) {
                            Lexeme *p = cctrlTokenPeek(cc);
                            if (p && p->tk_type == TK_IDENT) {
                                Ast *local = mapGetLen(cc->localenv,
                                        p->start, p->len);
                                if (local && local->kind == AST_LVAR) {
                                    cctrlAsmTokenGet(cc);
                                    s64 idx = (s64)all_lvars->size;
                                    vecPush(all_lvars, local);
                                    AoStr *ph = aoStrPrintf(
                                        ATT_LVAR_PLACEHOLDER_PREFIX
                                        "%lld"
                                        ATT_LVAR_PLACEHOLDER_SUFFIX,
                                        (long long)idx);
                                    if (count == 0)      op1 = ph;
                                    else if (count == 1) op2 = ph;
                                    else                 op3 = ph;
                                    count++;
                                    handled = 1;
                                }
                            }
                        }
                        if (handled) break;
                        /* fallthrough to default punct emission */
                    }
                    /* fallthrough */
                    default:
                        switch (count) {
                            case 0:
                                op1 = aoStrNew();
                                prsAsmPunct(cc,tok,op1);
                                break;
                            case 1:
                                op2 = aoStrNew();
                                prsAsmPunct(cc,tok,op2);
                                break;
                            case 2:
                                op3 = aoStrNew();
                                prsAsmPunct(cc,tok,op3);
                                break;
                        }
                        count++;
                        break;
                }
                break;
            }

        }
        tok = cctrlAsmTokenGet(cc);
    }

    if (curfunc) {
        aoStrCatLen(asm_code,curfunc->data,curfunc->len);
        aoStrCatLen(asm_code,":\n",2);
        aoStrCatLen(asm_code,curblock->data,curblock->len);
        asm_function = astAsmFunctionDef(curfunc,curblock);
        listAppend(asm_functions,asm_function);
        if (!mapAddOrErr(cc->asm_functions, curfunc->data, asm_function)) {
            cctrlIce(cc, "Already defined assembly function: %s", curfunc->data);
        }
    }
    asm_block = astAsmBlock(asm_code,asm_functions);
    /* Attach fragments only if any `&var` placeholder was actually
     * seen. Otherwise the plain-text path runs unchanged. */
    if (parse_one && fragments && fragments->next != fragments) {
        if ((s64)curblock->len > last_flush_pos) {
            AoStr *chunk = aoStrDupRaw(
                curblock->data + last_flush_pos,
                curblock->len - last_flush_pos);
            listAppend(fragments, asmFragmentText(chunk));
        }
        asm_block->asm_fragments = fragments;
    }
    return asm_block;
}

/* The next token is expected to be '{' so the parser has just seen 'asm' */
Ast *prsAsm(Cctrl *cc, int parse_one) {
    if (cc->target == TARGET_AARCH64_APPLE_DARWIN ||
        cc->target == TARGET_AARCH64_UNKNOWN_LINUX_GNU)
    {
        loggerPanic("Unsupported target, cannot use asm blocks %s\n",
                cliTargetToString(cc->target));
    }
    return prsAsmToATT(cc, parse_one);
}

#ifdef PRSASM_TEST

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
char *readfile(char *path) {
    int fd;
    if ((fd = open(path, O_RDONLY, 0644)) == -1) {
        loggerPanic("Failed to open file\n");
    }

    int len = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);

    char *buf = (char *)malloc(sizeof(char) * len);
    if (read(fd, buf, len) != len) {
        loggerPanic("Failed to read whole file\n");
    }
    buf[len] = '\0';
    close(fd);
    return buf;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        loggerPanic("usage: %s<file>\n", argv[0]);
    }
    lexer l;
    List *tokens;
    Cctrl *cc;
    char *file;
    Ast *asm_block;
    lexeme *tok, *peek;

    cc = cctrlNew();
    file = readfile(argv[1]);

    lexInit(&l, file);
    tokens = lexToLexemes(cc->macro_defs, &l);
    cctrlInitTokenIter(cc, tokens);

    while ((tok = cctrlAsmTokenGet(cc)) != NULL) {
        if (tokenIdentIs(tok,"asm",3)) {
            peek = cctrlTokenPeek(cc);
            if (tokenPunctIs(peek,'{')) {
                asm_block = prsAsm(cc,0);
                printf("%s\n", asm_block->asm_stmt->data);
            }
        }
    }
    free(file);
}

#endif
