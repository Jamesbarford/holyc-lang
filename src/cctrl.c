#include <assert.h>
#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "config.h"
#include "lexer.h"
#include "list.h"
#include "util.h"
#include "version.h"

/* VecType for the CtrlDiagnostic accumulator. Owns its entries,
 * release frees the heap-allocated CctrlDiagnostic plus its message /
 * suggestion AoStrs. */
static void cctrlDiagnosticStringify(AoStr *buf, void *ptr) {
    CctrlDiagnostic *d = (CctrlDiagnostic *)ptr;
    if (d && d->message) {
        aoStrCatFmt(buf, "%S", d->message);
    }
}

static int cctrlDiagnosticMatch(void *a, void *b) {
    return a == b;
}

static int cctrlDiagnosticRelease(void *ptr) {
    CctrlDiagnostic *d = (CctrlDiagnostic *)ptr;
    if (d) {
        if (d->message) aoStrRelease(d->message);
        if (d->suggestion) aoStrRelease(d->suggestion);
        free(d);
    }
    return 1;
}

static VecType vec_diagnostic_type = {
    .stringify = cctrlDiagnosticStringify,
    .match     = cctrlDiagnosticMatch,
    .release   = cctrlDiagnosticRelease,
    .type_str  = "CctrlDiagnostic *",
};

/* `Map<char *, Lexeme *>` Map does not own either the key nor the Lexeme */
MapType map_cstring_lexeme_type = {
    .match           = mapCStringEq,
    .hash            = mapCStringHash,
    .get_key_len     = mapCStringLen,
    .key_to_string   = mapCStringToString,
    .key_release     = NULL,
    .value_to_string = (mapValueToString *)lexemeToAoStr,
    .value_release   = NULL,
    .key_type        = "char *",
    .value_type      = "Lexeme *",
};

Map *cctrlCreateLexemeMap(void) {
    return mapNew(32, &map_cstring_lexeme_type);
}

Map *cctrlCreateAstMap(Map *parent) {
    return mapNewWithParent(parent, 32, &map_ast_type);
}

static char *x86_registers = "rax,rbx,rcx,rdx,rsi,rdi,rbp,rsp,r8,r9,r10,r11,r12,"
    "r13,r14,r15,cs,ds,es,fs,gs,ss,rip,rflags,st0,st1,st2,st3,st4,st5,st6,st7,"
    "mm0,mm1,mm2,mm3,mm4,mm5,mm6,mm7,xmm0,xmm1,xmm2,xmm3,xmm4,xmm5,xmm6,xmm7,"
    "xmm8,xmm9,xmm10,xmm11,xmm12,xmm13,xmm14,xmm15,ymm0,ymm1,ymm2,ymm3,ymm4,"
    "ymm5,ymm6,ymm7,ymm8,ymm9,ymm10,ymm11,ymm12,ymm13,ymm14,ymm15,cr0,cr2,"
    "cr3,cr4,cr8,dr0,dr1,dr2,dr3,dr6,dr7,eax,ebx,ecx,edx,esi,edi,ebp,eip,esp,"
    "ax,bx,cx,dx,ah,al,bh,bl,ch,cl,dh,dl,"
    // Not registers but need % in front of them
    "sil";

/* eventually it would be nice to remove the dependency on these functions 
 * syscall can be used for a lot of them */
static char *libc_functions = "printf,snprintf,exit,malloc,free,open,read,close,"
    "exit,system";

typedef struct BuiltInType {
    char *name;
    int kind;
    int size;
    int issigned;
} BuiltInType;

/* Name, kind, size, issigned, essentially duplicated in the 
 * lexer */
static BuiltInType built_in_types[] = {
    /* XXX: merge with lexers types
     * Holyc Types */
    {"U0",AST_TYPE_VOID,0,0},
    {"Bool",AST_TYPE_CHAR,1,1},
    {"I8",AST_TYPE_CHAR,1,1},
    {"U8",AST_TYPE_CHAR,1,0},
    {"I16",AST_TYPE_INT,2,1},
    {"U16",AST_TYPE_INT,2,0},
    {"I32",AST_TYPE_INT,4,1},
    {"U32",AST_TYPE_INT,4,0},
    {"I64",AST_TYPE_INT,8,1},
    {"U64",AST_TYPE_INT,8,0},
    {"F64",AST_TYPE_FLOAT,8,0},
    {"F32",AST_TYPE_FLOAT,4,0},
    {"inline",AST_TYPE_INLINE,0,0},
    {"public",AST_TYPE_VIS_MODIFIER,0,0},
    {"auto",AST_TYPE_AUTO,0,0},
    {"private",AST_TYPE_VIS_MODIFIER,0,0},
};

static void cctrlAddX86_64Defines(Cctrl *cc, Lexeme *le) {
    mapAdd(cc->macro_defs,"IS_X86_64",le);
    mapAdd(cc->macro_defs,"__x86_64__",le);
    le = lexemeNew(str_lit("x86_64"));
    le->tk_type = TK_STR;
    mapAdd(cc->macro_defs,"__ARCH__",le);
}

static void cctrlAddAArch64Defines(Cctrl *cc, Lexeme *le) {
    mapAdd(cc->macro_defs,"IS_ARM_64",le);
    mapAdd(cc->macro_defs,"__aarch64__",le);
    le = lexemeNew(str_lit("AArch64"));
    le->tk_type = TK_STR;
    mapAdd(cc->macro_defs,"__ARCH__",le);
}

static void cctrlAddLinuxDefines(Cctrl *cc, Lexeme *le) {
    mapAdd(cc->macro_defs,"__linux__",le);
    mapAdd(cc->macro_defs,"IS_LINUX",le);
}

static void cctrlAddMacOsDefines(Cctrl *cc, Lexeme *le) {
    mapAdd(cc->macro_defs,"__macos__",le);
    mapAdd(cc->macro_defs,"IS_MACOS",le);
}

void cctrlAddDefine(Cctrl *cc, char *name) {
    Lexeme *le = lexemeSentinal();
    mapAdd(cc->macro_defs,name,le);
}

static void cctrlAddBuiltinMacros(Cctrl *cc) {
    s64 bufsize = sizeof(char)*128;
    Lexeme *le = lexemeSentinal();

    /* This is so we can also cross compile, often the library uses ifdefs but
     * using what the host platform is would mean for `#ifdefs` on assembly we
     * can assemble the wrong code for the wrong platform. */
    switch (cc->target) {
        case TARGET_AARCH64_APPLE_DARWIN:
            cctrlAddMacOsDefines(cc, le);
            cctrlAddAArch64Defines(cc, le);
            break;
        case TARGET_AARCH64_UNKNOWN_LINUX_GNU:
            cctrlAddLinuxDefines(cc, le);
            cctrlAddAArch64Defines(cc, le);
            break;
        case TARGET_X86_64_APPLE_DARWIN:
            cctrlAddMacOsDefines(cc, le);
            cctrlAddX86_64Defines(cc, le);
            break;
        case TARGET_X86_64_UNKNOWN_LINUX_GNU:
            cctrlAddLinuxDefines(cc, le);
            cctrlAddX86_64Defines(cc, le);
            break;
    }


    struct timeval tm;
    gettimeofday(&tm,NULL);
    s64 milliseconds = (tm.tv_sec*1000) +
        (tm.tv_usec/1000);
    time_t seconds = milliseconds / 1000;
    struct tm *ptm = localtime(&seconds);

    char *time = (char *)malloc(bufsize);
    char *date = (char *)malloc(bufsize);
    char *time_stamp = (char *)malloc(bufsize);

    s64 len = snprintf(time,bufsize,"%02d:%02d:%02d",
            ptm->tm_hour,ptm->tm_min,ptm->tm_sec);
    time[len] = '\0';
    le = lexemeNew(time,len);
    le->tk_type = TK_STR;
    mapAdd(cc->macro_defs,"__TIME__",le);

    len = snprintf(date,bufsize,"%04d/%02d/%02d",
            ptm->tm_year+1900,ptm->tm_mon+1,ptm->tm_mday);
    date[len] = '\0';
    le = lexemeNew(date,len);
    le->tk_type = TK_STR;
    mapAdd(cc->macro_defs,"__DATE__",le);

    len = snprintf(time_stamp,bufsize,
            "%d-%02d-%02d %02d:%02d:%02d",
            ptm->tm_year+1900,
            ptm->tm_mon+1,ptm->tm_mday,ptm->tm_hour,
            ptm->tm_min,ptm->tm_sec);
    date[len] = '\0';
    le = lexemeNew(time_stamp,len);
    le->tk_type = TK_STR;
    mapAdd(cc->macro_defs,"__TIMESTAMP__",le);

#ifdef HCC_LINK_SQLITE3
    le = lexemeSentinal();
    mapAdd(cc->macro_defs,"__HCC_LINK_SQLITE3__",le);
#endif

    char *version = (char *)cctrlGetVersion();
    le = lexemeNew(version,strlen(version));
    le->tk_type = TK_STR;
    mapAdd(cc->macro_defs,"__HCC_VERSION__",le);
}

void cctrlAddBuiltinFunctions(Cctrl *cc) {
      Vec *params = vecNew(&vec_ast_type);
      AstType *type = astMakePointerType(astMakePointerType(ast_u8_type));
      Ast *var = astLVar(type,str_lit("fname"));
      vecPush(params, var);
      Ast *fn = astFunction(type,str_lit("Uf"),params,NULL,NULL,0);
      fn->kind = AST_FUN_PROTO;
      fn->flags |= AST_FLAG_BUILTIN;
      mapAdd(cc->global_env,fn->fname->data,fn);
}

Cctrl *ccMacroProcessor(Map *macro_defs) {
    Cctrl *cc = (Cctrl *)malloc(sizeof(Cctrl));
    cc->macro_defs = macro_defs;
    cc->strs = mapNew(32, &map_ast_type);
    cc->ast_list = NULL;
    /* The macro-processor stub Cctrl is passed to helpers that may
     * call cctrlRaiseException; keep diagnostics state consistent
     * so those paths don't read uninitialised fields. */
    cc->diagnostics = NULL;
    cc->n_errors = 0;
    cc->current_recovery = NULL;
    return cc;
}

/* Instantiate a new compiler control struct */
Cctrl *cctrlNew(enum CliTarget target) {
    Cctrl *cc = (Cctrl *)malloc(sizeof(Cctrl));

    cc->flags = 0;
    cc->global_env = mapNew(32, &map_ast_type);
    cc->clsdefs = mapNew(32, &map_asttype_type);
    cc->uniondefs = mapNew(32, &map_asttype_type);
    cc->symbol_table = mapNew(32, &map_asttype_type);
    cc->asm_funcs = mapNew(32, &map_ast_type);
    cc->macro_defs = cctrlCreateLexemeMap();
    cc->x86_registers = setNew(32, &set_cstring_type);
    cc->libc_functions = setNew(32, &set_cstring_type);
    cc->strs = mapNew(32, &map_ast_type);
    cc->asm_blocks = listNew();
    cc->asm_functions = mapNew(32, &map_ast_type);
    cc->ast_list = listNew();
    cc->initalisers = listNew();
    cc->initaliser_locals = listNew();
    /* These are temoraries that the parser will allocate and 
     * NULL out between parses of classes and functions */
    cc->localenv = NULL;
    cc->tmp_rettype = NULL;
    cc->func_params = NULL;
    cc->tmp_params = NULL;
    cc->tmp_loop_begin = NULL;
    cc->tmp_loop_end = NULL;
    cc->tmp_func = NULL;
    cc->tmp_gvar_decl = NULL;
    cc->token_buffer = NULL;
    cc->diagnostics = vecNew(&vec_diagnostic_type);
    cc->n_errors = 0;
    cc->current_recovery = NULL;
    /* main() overwrites the first two with the CLI's lists; `#link`
     * appends to all three during lexing (creating on demand). */
    cc->object_files = NULL;
    cc->shared_object_files = NULL;
    cc->link_libs = NULL;

    int len;
    AoStr **str_array = aoStrSplit(x86_registers,',',&len);
    for (int i = 0; i < len; ++i) {
        AoStr *upper_reg = aoStrDup(str_array[i]);
        aoStrToUpperCase(upper_reg);
        // Add both lower and upper case
        setAdd(cc->x86_registers,str_array[i]->data);
        setAdd(cc->x86_registers,upper_reg->data);

        //free(upper_reg);
        //free(str_array[i]);
    }
    free(str_array);

    str_array = aoStrSplit(libc_functions,',',&len);
    for (int i = 0; i < len; ++i) {
        char *libc_func = aoStrMove(str_array[i]);
        setAdd(cc->libc_functions,libc_func);
    }
    free(str_array);

    for (int i = 0; i < (int)static_size(built_in_types); ++i) {
        AstType *type = (AstType *)malloc(sizeof(AstType));
        BuiltInType *built_in = &built_in_types[i]; 
        memset(type, 0, sizeof(AstType));
        type->size = built_in->size;
        type->issigned = built_in->issigned;
        type->kind = built_in->kind;
        type->ptr = NULL;
        mapAdd(cc->symbol_table, built_in->name, type);
    }

    cc->target = target;

    cctrlAddBuiltinMacros(cc);
    /* We have no real mechanism of adding compiler builtins */
    // cctrlAddBuiltinFunctions(cc);

    Ast *cmd_args = astGlobalCmdArgs();
    listAppend(cc->ast_list,cmd_args->argc);
    listAppend(cc->ast_list,cmd_args->argv);
    mapAdd(cc->global_env,"argc",cmd_args->argc->declvar);
    mapAdd(cc->global_env,"argv",cmd_args->argv->declvar);
    return cc;
}

static Lexeme *token_ring_buffer[CCTRL_TOKEN_BUFFER_SIZE];

TokenRingBuffer *tokenRingBufferStaticNew(void) {
    TokenRingBuffer *ring_buffer = (TokenRingBuffer *)malloc(sizeof(TokenRingBuffer));
    ring_buffer->tail = 0;
    ring_buffer->head = 0;
    ring_buffer->size = 0;
    ring_buffer->entries = token_ring_buffer;
    ring_buffer->capacity = CCTRL_TOKEN_BUFFER_SIZE;
    for (s64 i = 0; i < CCTRL_TOKEN_BUFFER_SIZE; ++i) {
        ring_buffer->entries[i] = NULL;
    }
    return ring_buffer;
}

TokenRingBuffer *tokenRingBufferNew(void) {
    TokenRingBuffer *ring_buffer = (TokenRingBuffer *)malloc(sizeof(TokenRingBuffer));
    ring_buffer->tail = 0;
    ring_buffer->head = 0;
    ring_buffer->size = 0;
    return ring_buffer;
}

/* This always moves forward by one so is more of a get next */
s64 tokenRingBufferGetIdx(s64 idx, u64 capacity) {
    return (idx + 1) & (capacity - 1);
}

void tokenBufferPrint(TokenRingBuffer *ring_buffer) {
    for (int i = 0, idx = ring_buffer->tail; i < ring_buffer->size;
            i++, idx = tokenRingBufferGetIdx(idx, ring_buffer->capacity)) {
        printf(">> %s\n", lexemeToString(ring_buffer->entries[idx]));
    }
    printf("\n");
}

int tokenRingBufferEmpty(TokenRingBuffer *ring_buffer) {
    return ring_buffer->size == 0;
}

/* Add a token to the ring buffer and remove the oldest element */
void tokenRingBufferPush(TokenRingBuffer *ring_buffer, Lexeme *token) {
    ring_buffer->entries[ring_buffer->head] = token;
    ring_buffer->head = tokenRingBufferGetIdx(ring_buffer->head,
                                              ring_buffer->capacity);
    if (ring_buffer->size == ring_buffer->capacity) {
        ring_buffer->tail = tokenRingBufferGetIdx(ring_buffer->tail,
                                                  ring_buffer->capacity);
    } else {
        ring_buffer->size++;
    }
}

/* Take one token from the ring buffer */
Lexeme *tokenRingBufferPop(TokenRingBuffer *ring_buffer) {
    if (tokenRingBufferEmpty(ring_buffer)) {
        return NULL;
    }
    Lexeme *token = ring_buffer->entries[ring_buffer->tail];
    ring_buffer->tail = tokenRingBufferGetIdx(ring_buffer->tail,
                                              ring_buffer->capacity);
    ring_buffer->size--;
    return token;
}

Lexeme *tokenRingBufferPeekBy(TokenRingBuffer *ring_buffer, s64 offset) {
    /* we are out of tokens */
    if (tokenRingBufferEmpty(ring_buffer)) {
        return NULL;
    }
    s64 idx = tokenRingBufferGetIdx(ring_buffer->tail + offset,
                                        ring_buffer->capacity);
    return ring_buffer->entries[idx];
}

Lexeme *tokenRingBufferPeek(TokenRingBuffer *ring_buffer) {
    /* we are out of tokens */
    if (tokenRingBufferEmpty(ring_buffer)) {
        return NULL;
    }
    return ring_buffer->entries[ring_buffer->tail];
}

int tokenRingBufferRewind(TokenRingBuffer *ring_buffer) {
    //if (tokenRingBufferEmpty(ring_buffer)) { //|| ring_buffer->tail == ring_buffer->head) {
    //    return 0;
    //}

    Lexeme *token = ring_buffer->entries[ring_buffer->tail];
    u64 capacity = ring_buffer->capacity;
    s64 offset = 1;
    do {
        ring_buffer->tail = ((ring_buffer->tail - offset) + capacity) & (capacity-1);
        token = ring_buffer->entries[ring_buffer->tail];
        offset++;
    } while (token && token->tk_type == TK_COMMENT);
    ring_buffer->size++;
    return 1;
}

void cctrLoadNextTokens(Cctrl *cc, s64 token_count) {
    for (s64 i = 0; i < token_count; ++i) {
        Lexeme *token = lexToken(cc->macro_defs,cc->lexer_);
        if (!token) break;
        tokenRingBufferPush(cc->token_buffer, token);
    }
}

void cctrlInitParse(Cctrl *cc, Lexer *lexer_) {
    cc->lexer_ = lexer_;
    /* Wire the back-pointer so lex-time errors can route through
     * this Cctrl's diagnostic accumulator and recovery longjmp. */
    if (lexer_) lexer_->cc = cc;
    if (cc->token_buffer == NULL) {
        cc->token_buffer = tokenRingBufferStaticNew();
    }
    cctrLoadNextTokens(cc, cc->token_buffer->capacity);
}

void cctrlInitMacroProcessor(Cctrl *cc) {
    cc->lexer_ = NULL;
    /* The caller will tack on the entries and size */
    TokenRingBuffer *ring_buffer = (TokenRingBuffer *)malloc(sizeof(TokenRingBuffer));
    ring_buffer->tail = 0;
    ring_buffer->head = 0;
    ring_buffer->size = 0;
    cc->token_buffer = ring_buffer;
    cc->ast_list = NULL;
    cc->tmp_locals = NULL;
    cc->tmp_func = NULL;
}

Lexeme *cctrlMaybeExpandToken(Cctrl *cc, Lexeme *token) {
    if (token->tk_type != TK_IDENT) {
        return token;
    }

    Lexeme *maybe_define = mapGetLen(cc->macro_defs, token->start, token->len);
    if (maybe_define) {
        if (cc->flags & CCTRL_PASTE_DEFINES) {
            return token;
        }
        return maybe_define;
    }
    return token; 
}

Lexeme *cctrlTokenPeekBy(Cctrl *cc, int cnt) {
    assert(cnt > 0);
    /* The -1 is bizzare, however as an argument peeking by `1` you'd
     * expect to see the next token, which is infact `0` */
    return tokenRingBufferPeekBy(cc->token_buffer, cnt-1);
}

Lexeme *cctrlTokenPeek(Cctrl *cc) {
    Lexeme *token = tokenRingBufferPeek(cc->token_buffer);
    s64 offset = 0;
    while (token) {
        if (token->tk_type == TK_COMMENT) {
            token = tokenRingBufferPeekBy(cc->token_buffer, offset);
            offset++;
            continue;
        }
        cc->lineno = token->line;
        return cctrlMaybeExpandToken(cc,token);
    }
    return NULL;
}

void cctrlTokenRewind(Cctrl *cc) {
    TokenRingBuffer *ring_buffer = cc->token_buffer;
    if (!tokenRingBufferRewind(ring_buffer)) {
        return;
    }
}

Lexeme *cctrlTokenGet(Cctrl *cc) {
    Lexeme *token = tokenRingBufferPeek(cc->token_buffer);
    while (token) {
        tokenRingBufferPop(cc->token_buffer);
        if (token->tk_type == TK_COMMENT) {
            /* XXX: Not really sure what to do with the comments or where they
             * should go ??*/
            token = tokenRingBufferPeek(cc->token_buffer);
            continue;
        }
        if (cc->token_buffer->size < 3 && cc->lexer_) {
            cctrLoadNextTokens(cc,5);
        }
        cc->lineno = token->line;
        return cctrlMaybeExpandToken(cc, token);
    }
    return NULL;
}

/* Should this be at the lexer level? */   
Lexeme *cctrlAsmTokenGet(Cctrl *cc) {
    /* Tokens flow through to prsasm.c verbatim - the libtasm assembler
     * consumes TempleOS-style register spellings directly, so the old
     * AT&T `RAX` -> `%rax` rewrite is gone. */
    return cctrlTokenGet(cc);
}

AoStr *cctrlSeverityMessage(int severity) {
    AoStr *buf = aoStrNew();
    switch (severity) {
        case CCTRL_ERROR:
            aoStrCatColoured(buf, ESC_BOLD_RED, "error: ");
            break;
        case CCTRL_WARN:
            aoStrCatColoured(buf, ESC_BOLD_YELLOW, "warning: ");
            break;
        case CCTRL_INFO:
            aoStrCatColoured(buf, ESC_BOLD_CYAN, "info: ");
            break;
        case CCTRL_ICE:
            aoStrCatColoured(buf, ESC_BOLD_RED, "INTERNAL COMPILER ERROR");
            aoStrCatPrintf(buf, " - hcc %s: ", cctrlGetVersion());
            break;
    }
    return buf;
}

void cctrlFileAndLine(Cctrl *cc, AoStr *buf, s64 lineno, s64 char_pos, char *msg, int severity) {
    AoStr *severity_msg = cctrlSeverityMessage(severity);

    aoStrCatFmt(buf,"%S%s", severity_msg, msg);
    if (buf->data[buf->len - 1] != '\n') {
        aoStrPutChar(buf,'\n');
    }

    if (cc->lexer_) {
        LexFile *file = cc->lexer_->cur_file;
        char *file_name = file->filename->data;
        aoStrPutChar(buf, ' ');
        aoStrCatColoured(buf, ESC_CYAN, "-->");
        aoStrCatPrintf(buf, " %s:%ld", file_name, (long)lineno);
        if (char_pos > -1) {
            aoStrCatFmt(buf,":%I",char_pos+1);
        }
        aoStrPutChar(buf,'\n');
    } else {
        aoStrCatFmt(buf, "Parsing macro:%I ", lineno);
    }
    aoStrRelease(severity_msg);
}

char *lexemeToColor(Cctrl *cc, Lexeme *tok, int is_err) {
    /* Error token wins: render in bold red, no syntax colouring.
     * Otherwise pick a per-kind colour, with TK_IDENT looking up
     * the keyword table to catch user-defined-but-actually-builtin
     * names (e.g. type aliases). */
    if (is_err) {
        const char *red = clr(ESC_BOLD_RED);
        const char *rst = clr(ESC_RESET);
        switch (tok->tk_type) {
            case TK_STR: return mprintf("%s\"%.*s\"%s", red, tok->len, tok->start, rst);
            case TK_I64: return mprintf("%s%ld%s", red, tok->i64, rst);
            case TK_F64: return mprintf("%s%f%s", red, tok->f64, rst);
            default:     return mprintf("%s%.*s%s", red, tok->len, tok->start, rst);
        }
    }

    const char *rst = clr(ESC_RESET);
    switch (tok->tk_type) {
        case TK_KEYWORD:
            return mprintf("%s%.*s%s", clr(ESC_BLUE), tok->len, tok->start, rst);
        case TK_STR:
            return mprintf("%s\"%.*s\"%s", clr(ESC_GREEN), tok->len, tok->start, rst);
        case TK_I64:
            return mprintf("%s%ld%s", clr(ESC_PURPLE), tok->i64, rst);
        case TK_F64:
            return mprintf("%s%f%s", clr(ESC_PURPLE), tok->f64, rst);
        case TK_IDENT:
            if (cctrlIsKeyword(cc, tok->start, tok->len)) {
                return mprintf("%s%.*s%s", clr(ESC_BLUE), tok->len, tok->start, rst);
            }
            return mprintf("%.*s", tok->len, tok->start);
        default:
            return mprintf("%.*s", tok->len, tok->start);
    }
}

s64 cctrlGetErrorIdx(Cctrl *cc, s64 line, char ch,
                         const char *line_buffer)
{
    (void)cc;
    (void)line;
    char *ptr = (char *)line_buffer;
    while (*ptr) {
        if (*ptr == ch) {
            break;
        }
        ptr++;
    }
    return ptr-line_buffer;
}

void cctrlCreateColoredLine(Cctrl *cc,
                            AoStr *buf,
                            s64 lineno,
                            int should_color_err,
                            char *suggestion,
                            s64 char_pos,
                            s64 *_offset,
                            s64 *_tok_len,
                            const char *line_buffer)
{
    (void)suggestion;
    aoStrCatColoured(buf, ESC_CYAN, "     |\n");
    Lexeme *cur_tok = cctrlTokenPeek(cc);

    AoStr *colored_buffer = aoStrNew();
    s64 offset = -1;
    s64 tok_len = -1;
    Lexeme tok;
    Lexer l;
    /* Accept whitespace AND comments so we preserve every source character.
     * Without this, comments get silently swallowed during re-lexing and the
     * `^` underline (positioned by source column) lands past the wrong text.
     * Also flip on CCF_PERMISSIVE so malformed escapes (e.g. `"\q"` in the
     * source) don't make the renderer call lexRaise. We're only trying to
     * syntax-colour the line, not to validate it. */
    lexInit(&l, (char *)line_buffer,
            CCF_ACCEPT_WHITESPACE|CCF_ACCEPT_COMMENTS|CCF_PERMISSIVE);
    s64 current_offset = 0;

    /* This assumes we want the last match of an error as opposed to the first */
    while (lex(&l,&tok)) {
        char *colored_lexeme = NULL;
        int is_err = 0;
        if (should_color_err) {
            if (cur_tok->tk_type != TK_STR && tok.tk_type == TK_STR) current_offset++;
            if (lexemeEq(cur_tok, &tok)) {
                if (current_offset == char_pos) {
                    tok_len = tok.len;
                    offset = current_offset;
                    is_err = 1;
                    if (tok.tk_type == TK_STR) tok_len++;
                }
            }
        }

        colored_lexeme = lexemeToColor(cc,&tok, is_err && should_color_err);
        aoStrCat(colored_buffer, colored_lexeme);
        current_offset += tok.len;
    }

    aoStrCatColouredFmt(buf, ESC_CYAN, "%4ld |", (long)lineno);
    aoStrCatPrintf(buf, "    %s\n", colored_buffer->data);
    if (_offset) *_offset = offset;
    if (_tok_len) *_tok_len = tok_len;
    aoStrRelease(colored_buffer);
}

/* Render a diagnostic positioned at the current peek token. */
AoStr *cctrlCreateErrorLine(Cctrl *cc, s64 lineno, char *msg,
                            int severity, char *suggestion)
{
    Lexeme *peek = cctrlTokenPeek(cc);
    s64 line = peek ? peek->line : lineno;
    s64 col  = peek ? peek->col  : 0;
    s64 len  = peek ? peek->len  : 0;
    return cctrlCreateErrorLineAt(cc, line, col, len, msg, severity, suggestion);
}

/* Render a diagnostic at an *explicit* source position. Unlike
 * cctrlCreateErrorLine this never calls cctrlTokenPeek, so it works correctly
 * when the caller knows where the error is but the token buffer doesn't.
 * 0 for a column means "we don't have a column", the line is rendered without
 * an underline. `len` is the width of the `^` underline
 * (clamped to 1 if 0 is given). */
AoStr *cctrlCreateErrorLineAt(Cctrl *cc, s64 lineno, s64 col, s64 len,
                              char *msg, int severity, char *suggestion)
{
    char *color = severity == CCTRL_ERROR || CCTRL_ICE
            ? ESC_BOLD_RED
            : CCTRL_WARN ? ESC_BOLD_YELLOW : ESC_CYAN;
    AoStr *buf = aoStrNew();
    if (!cc->lexer_) {
        cctrlFileAndLine(cc, buf, lineno, -1, msg, severity);
        aoStrCatColoured(buf, ESC_CYAN, "     |\n");
        return buf;
    }

    s64 char_pos = col > 0 ? col - 1 : -1;
    s64 tok_len  = len > 0 ? len : 1;
    cctrlFileAndLine(cc, buf, lineno, char_pos, msg, severity);

    const char *line_buffer = lexerReportLine(cc->lexer_, lineno);
    s64 unused_off = -1, unused_len = -1;
    cctrlCreateColoredLine(cc, buf, lineno, 0, NULL, -1,
                           &unused_off, &unused_len, line_buffer);

    if (char_pos != -1) {
        aoStrCatColoured(buf, ESC_CYAN, "     |    ");
        for (int i = 0; i < char_pos; ++i) aoStrPutChar(buf, ' ');
        for (int i = 0; i < tok_len; ++i) {
            aoStrCatColoured(buf, color, "^");
        }
        if (suggestion) {
            aoStrCatColouredFmt(buf, color, " %s", suggestion);
        }
    } else {
        aoStrCatColoured(buf, ESC_CYAN, "     |");
    }
    return buf;
}

AoStr *cctrlMessagVnsPrintF(Cctrl *cc, char *fmt, va_list ap, int severity) {
    char *msg = mprintVa(fmt, ap, NULL);
    AoStr *bold_msg = aoStrNew();
    aoStrCatColoured(bold_msg, ESC_BOLD, msg);
    Lexeme *cur_tok = cctrlTokenPeek(cc);
    /* At EOF the peek returns NULL; use the last known line so the
     * error message still has useful location info instead of
     * crashing on the field-dereference below. */
    s64 line = cur_tok ? cur_tok->line : cc->lineno;
    AoStr *buf = cctrlCreateErrorLine(cc,line,bold_msg->data,severity,NULL);
    aoStrRelease(bold_msg);
    return buf;
}

AoStr *cctrlMessagVnsPrintFWithSuggestion(Cctrl *cc, char *fmt, va_list ap,
                                          int severity, char *suggestion)
{
    char *msg = mprintVa(fmt, ap, NULL);
    AoStr *bold_msg = aoStrNew();
    aoStrCatColoured(bold_msg, ESC_BOLD, msg);
    Lexeme *cur_tok = cctrlTokenPeek(cc);
    s64 line = cur_tok ? cur_tok->line : cc->lineno;
    AoStr *buf = cctrlCreateErrorLine(cc,line,bold_msg->data,severity,suggestion);
    aoStrRelease(bold_msg);
    return buf;
}

AoStr *cctrlMessagePrintF(Cctrl *cc, int severity, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    AoStr *buf = cctrlMessagVnsPrintF(cc,fmt,ap,severity);
    va_end(ap);
    return buf;
}

void cctrlInfo(Cctrl *cc, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    AoStr *buf = cctrlMessagVnsPrintF(cc,fmt,ap,CCTRL_INFO);
    va_end(ap);
    /* Route through the diagnostic accumulator so info/warning
     * lines interleave with errors in source order at flush time
     * instead of bleeding out mid-parse. */
    CctrlDiagnostic *d = cctrlMakeDiag(cc, CCTRL_INFO, buf, NULL);
    cctrlDiagPush(cc, d);
}

void cctrlWarning(Cctrl *cc, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    /* If we are in a repl we are super conservative and treat warnings
     * as errors. This is so the reepl is _less_ likey to crash.
     * A bit like -Werror in C */
    int is_werror = cc->flags & (CCTRL_WERROR);
    int severity = is_werror ? CCTRL_ERROR : CCTRL_WARN;
    AoStr *buf = cctrlMessagVnsPrintF(cc,fmt,ap,severity);
    va_end(ap);
    CctrlDiagnostic *d = cctrlMakeDiag(cc, severity, buf, NULL);
    cctrlDiagPush(cc, d);
    if (severity == CCTRL_ERROR) {
        cctrlTerminate(cc);
    }
}

/* Like cctrlWarning, but anchored at an explicit (line, col) instead of
 * the current token. Needed when the thing being warned about was parsed
 * earlier and the cursor has since moved on - e.g. a missing return is
 * only known after the whole function body is consumed, by which point
 * the next declaration is the "current" token. */
void cctrlWarningAt(Cctrl *cc, s64 lineno, s64 col, s64 len, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    char *msg = mprintVa(fmt, ap, NULL);
    va_end(ap);
    AoStr *bold_msg = aoStrNew();
    aoStrCatColoured(bold_msg, ESC_BOLD, msg);
    int is_werror = cc->flags & (CCTRL_WERROR);
    int severity = is_werror ? CCTRL_ERROR : CCTRL_WARN;
    AoStr *rendered = cctrlCreateErrorLineAt(cc, lineno, col, len,
                                             bold_msg->data, severity, NULL);
    aoStrRelease(bold_msg);

    CctrlDiagnostic *d = (CctrlDiagnostic *)calloc(1, sizeof(CctrlDiagnostic));
    d->severity = severity;
    d->message = rendered;
    d->suggestion = NULL;
    d->file = cc->lexer_ ? cc->lexer_->cur_file : NULL;
    d->line = (int)lineno;
    d->col = (int)col;
    d->end_line = (int)lineno;
    d->end_col = (int)(col + (len > 0 ? len : 1));
    cctrlDiagPush(cc, d);
    if (severity == CCTRL_ERROR) {
        cctrlTerminate(cc);
    }
}

CctrlDiagnostic *cctrlMakeDiag(Cctrl *cc,
                               int severity,
                               AoStr *rendered,
                               AoStr *suggestion)
{
    CctrlDiagnostic *d = (CctrlDiagnostic *)calloc(1, sizeof(CctrlDiagnostic));
    d->severity = severity;
    d->message = rendered;
    d->suggestion = suggestion;
    d->file = cc->lexer_ ? cc->lexer_->cur_file : NULL;

    Lexeme *cur = cctrlTokenPeek(cc);
    if (cur) {
        d->line = cur->line;
        d->col = cur->col;
        d->end_line = cur->line;
        d->end_col = cur->col + (cur->len > 0 ? cur->len : 1);
    } else {
        d->line = (int)cc->lineno;
        d->col = 0;
        d->end_line = d->line;
        d->end_col = 0;
    }
    return d;
}

void cctrlDiagPush(Cctrl *cc, CctrlDiagnostic *d) {
    if (!cc || !cc->diagnostics) {
        /* No accumulation storage (e.g. macro-processor stub).
         * Print immediately so the message isn't lost. */
        if (d) {
            if (d->message) fprintf(stderr, "%s\n", d->message->data);
            if (d->message) aoStrRelease(d->message);
            if (d->suggestion) aoStrRelease(d->suggestion);
            free(d);
        }
        return;
    }
    vecPush(cc->diagnostics, d);
    if (d->severity == CCTRL_ERROR) cc->n_errors++;
}

int cctrlDiagFlush(Cctrl *cc) {
    if (!cc || !cc->diagnostics) return 0;
    for (u64 i = 0; i < cc->diagnostics->size; i++) {
        CctrlDiagnostic *d = (CctrlDiagnostic *)cc->diagnostics->entries[i];
        if (d && d->message) {
            fprintf(stderr, "%s\n", d->message->data);
        }
    }
    return cc->n_errors;
}

/* Drop all queued diagnostics and reset the error count. The REPL
 * calls this between inputs so one bad line doesn't poison (or
 * re-print with) the next. */
void cctrlDiagClear(Cctrl *cc) {
    if (!cc) return;
    if (cc->diagnostics) vecClear(cc->diagnostics);
    cc->n_errors = 0;
}

/* Empty the token ring buffer. After an errored parse the buffer can
 * still hold tokens from the abandoned input; a fresh REPL parse must
 * not see them. The freed slots are filled with a sentinel rather
 * than NULL: parsePrimary rewinds one token to inspect what preceded
 * an expression, and when the expression opens the input that step
 * walks into the ring's history - which must therefore always hold
 * valid lexemes. */
void cctrlResetTokenBuffer(Cctrl *cc) {
    TokenRingBuffer *ring = cc->token_buffer;
    if (!ring) return;
    static Lexeme *filler = NULL;
    if (filler == NULL) filler = lexemeSentinal();
    ring->tail = 0;
    ring->head = 0;
    ring->size = 0;
    for (s64 i = 0; i < ring->capacity; ++i) {
        ring->entries[i] = filler;
    }
}

__noreturn void cctrlTerminate(Cctrl *cc) {
    /* Optionally we can not have a jump buffer */
    if (cc->current_recovery) {
        /* Hand control to the nearest active recovery point. The
         * jumper resyncs tokens and resumes; the message is
         * printed by `cctrlDiagFlush` at the end of compilation. */
        longjmp(*cc->current_recovery, 1);
    }
    /* No recovery active: keep the legacy fail-fast behaviour. */
    cctrlDiagFlush(cc);
    exit(EXIT_FAILURE);
}

/* Walks the ring buffer back to a token on a strictly earlier
 * line than `tok->line`, renders an INFO message positioned at
 * that token, then re-advances so the buffer head ends up where
 * the caller left it. The walk is capped at 16 rewinds and bails
 * on a same-peek-after-rewind (ring floor). Returns NULL when no
 * earlier-line token exists in reach - the caller should treat
 * that as "no hint available" rather than an error. */
AoStr *cctrlInfoAtPreviousLine(Cctrl *cc, Lexeme *tok, const char *fmt, ...) {
    int rewinds = 0;
    Lexeme *prev = cctrlTokenPeek(cc);
    while (prev && prev->line >= tok->line && rewinds < 16) {
        cctrlTokenRewind(cc);
        Lexeme *new_prev = cctrlTokenPeek(cc);
        if (new_prev == prev) break;
        prev = new_prev;
        rewinds++;
    }

    AoStr *info_msg = NULL;
    if (prev && prev->line < tok->line) {
        va_list ap;
        va_start(ap, fmt);
        info_msg = cctrlMessagVnsPrintF(cc, (char *)fmt, ap, CCTRL_INFO);
        va_end(ap);
    }

    /* Restore buffer head so the recovery code sees the same
     * state the caller had before this helper ran. */
    while (rewinds-- > 0) {
        if (!cctrlTokenGet(cc)) break;
    }
    return info_msg;
}

void cctrlRaiseException(Cctrl *cc, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    AoStr *buf = cctrlMessagVnsPrintF(cc,fmt,ap,CCTRL_ERROR);
    va_end(ap);

    CctrlDiagnostic *d = cctrlMakeDiag(cc, CCTRL_ERROR, buf, NULL);
    cctrlDiagPush(cc, d);
    cctrlTerminate(cc);
}

void cctrlRaiseSuggestion(Cctrl *cc, char *suggestion, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    AoStr *buf = cctrlMessagVnsPrintFWithSuggestion(cc,fmt,ap,CCTRL_ERROR,suggestion);
    va_end(ap);

    AoStr *sug = suggestion ? aoStrPrintf("%s", suggestion) : NULL;
    CctrlDiagnostic *d = cctrlMakeDiag(cc, CCTRL_ERROR, buf, sug);
    cctrlDiagPush(cc, d);
    cctrlTerminate(cc);
}

void cctrlIce(Cctrl *cc, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    AoStr *buf = cctrlMessagVnsPrintF(cc,fmt,ap,CCTRL_ICE);
    fprintf(stderr,"%s\n",buf->data);
    aoStrRelease(buf);
    va_end(ap);
    exit(EXIT_FAILURE);
}

/* Walk forwards through the token stream, balancing `{`/`}`, until
 * we land on a recovery point. Returns once `peek()` is positioned
 * at the first token the caller's loop should resume from. */
static void cctrlSyncCommon(Cctrl *cc, int stop_at_toplevel_kw) {
    int depth = 0;
    while (1) {
        Lexeme *peek = cctrlTokenPeek(cc);
        if (!peek) return;

        if (tokenPunctIs(peek, '{')) {
            depth++;
            cctrlTokenGet(cc);
            continue;
        }
        if (tokenPunctIs(peek, '}')) {
            if (depth == 0) {
                /* Hit the closing brace of our containing block;
                 * leave it in place so the parent parser can consume
                 * it and exit cleanly. */
                return;
            }
            depth--;
            cctrlTokenGet(cc);
            continue;
        }
        if (depth == 0 && tokenPunctIs(peek, ';')) {
            /* Consume the terminator so the next iteration starts
             * fresh on the following statement. */
            cctrlTokenGet(cc);
            return;
        }
        if (depth == 0 && stop_at_toplevel_kw && peek->tk_type == TK_KEYWORD) {
            /* Likely the start of a new top-level decl; leave it
             * for parseToplevelDef. */
            return;
        }
        cctrlTokenGet(cc);
    }
}

void cctrlSyncToplevel(Cctrl *cc) {
    cctrlSyncCommon(cc, 1);
}

void cctrlSyncStatement(Cctrl *cc) {
    cctrlSyncCommon(cc, 0);
}

/* Rewind the token buffer until there is a match. NULL `peek`
 * (EOF in the ring buffer) and a NULL-source `ch` are tolerated -
 * callers in the parser sometimes hit this when the lexer has run
 * dry mid-construct. */
void cctrlRewindUntilPunctMatch(Cctrl *cc, s64 ch, int *_count) {
    int count = 0;
    Lexeme *peek = cctrlTokenPeek(cc);
    while (peek && !tokenPunctIs(peek, ch)) {
        cctrlTokenRewind(cc);
        Lexeme *new_peek = cctrlTokenPeek(cc);
        /* Rewind can't go below the start of the ring; if peek
         * doesn't change, we've hit the floor. Bail. */
        if (new_peek == peek) break;
        peek = new_peek;
        count++;
        if (count > 5) break; // has to be some limit
    }
    if (_count) *_count = count;
}

void cctrlRewindUntilStrMatch(Cctrl *cc, char *str, int len, int *_count) {
    int count = 0;
    Lexeme *peek = cctrlTokenPeek(cc);
    /* `peek` is NULL when the ring is fully drained. A single line
     * input will hit this when the error fires at the end of a statement.
     * */
    while (peek == NULL ||
           !(peek->len == len && memcmp(peek->start,str,len) == 0)) {
        cctrlTokenRewind(cc);
        Lexeme *new_peek = cctrlTokenPeek(cc);
        if (new_peek == peek) break;
        peek = new_peek;
        count++;
        if (count > 5) break; // has to be some limit
    }
    if (_count) *_count = count;
}


/* assert the token we are currently pointing at is a TK_PUNCT and the 'i64'
 * matches 'expected'. Then consume this token else throw an error */
void cctrlTokenExpect(Cctrl *cc, s64 expected) {
    Lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok, expected)) return;

    if (!tok) {
        /* End of input while we still wanted `expected`. This is
         * almost always an unbalanced bracket / missing terminator
         * - point at the line we were last consuming so the user
         * sees the actual region the parse fell off at. */
        const char *hint = (expected == '}')
            ? " (unterminated `{ ... }` block?)"
          : (expected == ')')
            ? " (unterminated `( ... )`?)"
          : (expected == ']')
            ? " (unterminated `[ ... ]`?)"
          : (expected == ';')
            ? " (missing terminator?)"
          : "";
        cctrlRaiseException(cc,
            "Unexpected end of input, expected `%c`%s",
            (char)expected, hint);
        return; /* unreachable; cctrlRaiseException is noreturn */
    }

    /* tok was consumed by the get above so peek now points past it. Rewind
     * once so the diagnostic renderer captures tok's actual line/col. */
    cctrlTokenRewind(cc);
    AoStr *err_msg = cctrlMessagePrintF(cc, CCTRL_ERROR,
            "Syntax error, got an unexpected %s `%.*s`, perhaps you meant `%c`?",
            lexemeTypeToString(tok->tk_type),
            tok->len, tok->start,
            (char)expected);
    /* Re-consume so the buffer is back to where the caller left us before we
     * go hunting for an explanatory hint. */
    cctrlTokenGet(cc);

    AoStr *info_msg = cctrlInfoAtPreviousLine(cc, tok,
            "previous statement starts here");

    CctrlDiagnostic *err_d = cctrlMakeDiag(cc, CCTRL_ERROR, err_msg, NULL);
    cctrlDiagPush(cc, err_d);
    if (info_msg) {
        CctrlDiagnostic *info_d = cctrlMakeDiag(cc, CCTRL_INFO, info_msg, NULL);
        cctrlDiagPush(cc, info_d);
    }
    cctrlTerminate(cc);
}

/* Render a diagnostic whose underline spans the chars `from`..`to`
 * within the offending source line (e.g. `{` ... `}` for an
 * unbalanced block). Position-derivation is the only thing that
 * differs from the regular path - actual rendering goes through
 * cctrlCreateErrorLineAt. */
AoStr *cctrlRaiseFromTo(Cctrl *cc, int severity, char *suggestion, char from,
                        char to, char *fmt, va_list ap)
{
    char *msg = mprintVa(fmt, ap, NULL);
    AoStr *bold_msg = aoStrNew();
    aoStrCatColoured(bold_msg, ESC_BOLD, msg);

    Lexeme *cur_tok = cctrlTokenPeek(cc);
    s64 line = cur_tok ? cur_tok->line : cc->lineno;

    s64 col = 0, len = 0;
    if (cur_tok && cc->lexer_) {
        const char *line_buffer = lexerReportLine(cc->lexer_, line);
        s64 from_idx = cctrlGetErrorIdx(cc, line, from, line_buffer);
        s64 to_idx   = cctrlGetErrorIdx(cc, line, to,   line_buffer);
        if (from_idx != -1 && to_idx != -1) {
            col = from_idx + 1;
            len = (to_idx + 1) - from_idx;
        }
    }

    AoStr *buf = cctrlCreateErrorLineAt(cc, line, col, len,
                                        bold_msg->data, severity, suggestion);
    aoStrRelease(bold_msg);
    return buf;
}

void cctrlRaiseExceptionFromTo(Cctrl *cc, char *suggestion, char from, char to, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    AoStr *buf = cctrlRaiseFromTo(cc,CCTRL_ERROR,suggestion,from,to,fmt,ap);
    va_end(ap);

    AoStr *sug = suggestion ? aoStrPrintf("%s", suggestion) : NULL;
    CctrlDiagnostic *d = cctrlMakeDiag(cc, CCTRL_ERROR, buf, sug);
    cctrlDiagPush(cc, d);

    cctrlTerminate(cc);
}

void cctrlWarningFromTo(Cctrl *cc, char *suggestion, char from, char to, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    int is_werror = cc->flags & (CCTRL_WERROR);
    int severity = is_werror ? CCTRL_ERROR : CCTRL_WARN;
    AoStr *buf = cctrlRaiseFromTo(cc,severity,suggestion,from,to,fmt,ap);
    va_end(ap);

    /* Warnings accumulate but never longjmp as the parser keeps going.
     * `cctrlDiagFlush(...)` prints them alongside the errors at the end of
     * compilation. */
    AoStr *sug = suggestion ? aoStrPrintf("%s", suggestion) : NULL;
    CctrlDiagnostic *d = cctrlMakeDiag(cc, severity, buf, sug);
    cctrlDiagPush(cc, d);
    if (severity == CCTRL_ERROR) {
        cctrlTerminate(cc);
    }
}

/* Get variable either from the local or global scope */
Ast *cctrlGetVar(Cctrl *cc, char *varname, int len) {
    Ast *ast_var;
    Lexeme *tok;

    if (cc->localenv && (ast_var = mapGetLen(cc->localenv, varname, len)) != NULL) {
        return ast_var;
    }

    if ((ast_var = (Ast *)mapGetLen(cc->global_env, varname, len)) != NULL) {
        return ast_var;
    }

    if ((ast_var = mapGetLen(cc->asm_funcs, varname, len)) != NULL) {
        return ast_var;
    }

    /* Expand a macro definition */
    if ((tok = mapGetLen(cc->macro_defs, varname, len)) != NULL) {
        switch (tok->tk_type) {
        case TK_I64:
            if (cc->flags & CCTRL_PASTE_DEFINES) {
                return astLVar(ast_int_type, varname, len);
            }
            return astI64Type(tok->i64);
        case TK_F64:
            if (cc->flags & CCTRL_PASTE_DEFINES) {
                return astLVar(ast_float_type, varname, len);
            }
            return astF64Type(tok->f64);
        case TK_STR: {
            if (cc->flags & CCTRL_PASTE_DEFINES) {
                return astLVar(astMakePointerType(ast_u8_type), varname, len);
            }
            return cctrlGetOrSetString(cc, tok->start, tok->len, tok->i64);
        }
        default:
            return NULL;
        }
    }
    return NULL;
}

AstType *cctrlGetKeyWord(Cctrl *cc, char *name, int len) {
    AstType *type;
    assert(name != NULL);
    if ((type = mapGetLen(cc->symbol_table, name, len)) != NULL) {
        return type;
    }
    /* Classes are types */
    if ((type = mapGetLen(cc->clsdefs,name,len)) != NULL) {
        return type;
    }
    if ((type = mapGetLen(cc->uniondefs,name,len)) != NULL) {
        return type;
    }
    return NULL;
}

int cctrlIsKeyword(Cctrl *cc, char *name, int len) {
    return cctrlGetKeyWord(cc,name,len) != NULL;
}

void cctrlSetCommandLineDefines(Cctrl *cc, List *defines_list) {
    listForEach(defines_list) {
        mapAdd(cc->macro_defs,(char*)it->value,lexemeSentinal());
    }
}

/* De-duplicates strings by checking their existance */
Ast *cctrlGetOrSetString(Cctrl *cc, char *str, int len, s64 real_len) {
    Ast *ast_str = NULL;
    if (cc->strs) {
        ast_str = mapGetLen(cc->strs, str, len);
        if (!ast_str) {
            ast_str = astString(str,len,real_len);
            mapAddLen(cc->strs, ast_str->sval->data, len, ast_str);
        }
    } else {
        ast_str = astString(str,len,real_len);
    }
    return ast_str;
}
