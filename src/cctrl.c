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
    {"inline",AST_TYPE_INLINE,0,0},
    {"public",AST_TYPE_VIS_MODIFIER,0,0},
    {"auto",AST_TYPE_AUTO,0,0},
    {"private",AST_TYPE_VIS_MODIFIER,0,0},
};

static void cctrlAddBuiltinMacros(Cctrl *cc) {
    s64 bufsize = sizeof(char)*128;

    Lexeme *le = lexemeSentinal();
    if (IS_BSD)   mapAdd(cc->macro_defs,"IS_BSD",le);
    if (IS_LINUX) mapAdd(cc->macro_defs,"IS_LINUX",le);

#ifdef IS_MACOS
    mapAdd(cc->macro_defs,"IS_MACOS",le);
#endif
    
    if (IS_X86_64)      {
        mapAdd(cc->macro_defs,"IS_X86_64",le);
        le = lexemeNew("X86_64",6);
        le->tk_type = TK_STR;
        mapAdd(cc->macro_defs,"__ARCH__",le);
    } else if (IS_ARM_64) {
        mapAdd(cc->macro_defs,"IS_ARM_64",le);
        le = lexemeNew("ARM_64",6);
        le->tk_type = TK_STR;
        mapAdd(cc->macro_defs,"__ARCH__",le);
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

    le = lexemeNew((char *)cctrlGetVersion(),len);
    le->tk_type = TK_STR;
    mapAdd(cc->macro_defs,"__HCC_VERSION__",le);
}

Cctrl *ccMacroProcessor(Map *macro_defs) {
    Cctrl *cc = (Cctrl *)malloc(sizeof(Cctrl));
    cc->macro_defs = macro_defs;
    cc->strs = mapNew(32, &map_ast_type);
    cc->ast_list = NULL;
    return cc;
}

/* Instantiate a new compiler control struct */
Cctrl *cctrlNew(void) {
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
    cc->token_buffer = NULL;

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
        type->size = built_in->size;
        type->issigned = built_in->issigned;
        type->kind = built_in->kind;
        type->ptr = NULL;
        mapAdd(cc->symbol_table, built_in->name, type);
    }

    cctrlAddBuiltinMacros(cc);

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
    Lexeme *token = cctrlTokenGet(cc);
    if (token) {
        /* If the token is a register as defined by the string at the top of 
         * this file we may it AT&T syntax `RAX` -> `%rax`. Which saves a 
         * massive headache in prsasm.c */
        if (setHasLen(cc->x86_registers,token->start,token->len)) {
            char *reg = mprintFmt("%%%.*s", token->len, token->start);
            char *ptr = reg;
            while (*ptr) {
                *ptr = tolower(*ptr);
                ptr++;
            }
            int register_len = strlen(reg);
            token->start = reg;
            token->len = register_len;
            return token;
        }
    }
    return token;
}

AoStr *cctrlSeverityMessage(int severity) {
    AoStr *buf = aoStrNew();
    if (is_terminal) {
        switch (severity) {
            case CCTRL_ERROR:
                aoStrCatLen(buf,str_lit(ESC_BOLD_RED));
                aoStrCatLen(buf,str_lit("error: "));
                aoStrCatLen(buf,str_lit(ESC_RESET));
                aoStrCatLen(buf,str_lit(ESC_CLEAR_BOLD));
                break;
            case CCTRL_WARN:
                aoStrCatLen(buf,str_lit(ESC_BOLD_YELLOW));
                aoStrCatLen(buf,str_lit("warning: "));
                aoStrCatLen(buf,str_lit(ESC_RESET));
                break;
            case CCTRL_INFO:
                aoStrCatLen(buf,str_lit(ESC_BOLD_CYAN));
                aoStrCatLen(buf,str_lit("info: "));
                aoStrCatLen(buf,str_lit(ESC_RESET));
                break;
            case CCTRL_ICE: {
                char *ice_msg = mprintf(ESC_BOLD_RED"INTERNAL COMPILER ERROR"ESC_RESET" - hcc %s: ",
                                        cctrlGetVersion());
                aoStrCat(buf,ice_msg);
                break;
            }
        }
    } else {
        switch (severity) {
            case CCTRL_ERROR: aoStrCatLen(buf,str_lit("error: ")); break;
            case CCTRL_WARN: aoStrCatLen(buf,str_lit("warning: ")); break;
            case CCTRL_INFO: aoStrCatLen(buf,str_lit("info: ")); break;
            case CCTRL_ICE: {
                char *ice_msg = mprintf("INTERNAL COMPILER ERROR - hcc %s: ",
                                        cctrlGetVersion());
                aoStrCat(buf,ice_msg);
                break;
            }
        }
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
        if (is_terminal) {
            aoStrCatFmt(buf, " "ESC_CYAN"-->"ESC_RESET" %s:%I",
                    file_name,
                    lineno);
        } else {
            aoStrCatFmt(buf, " --> %s:%I", file_name, lineno);
        }
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
    if (!is_terminal) {
        switch (tok->tk_type) {
            case TK_KEYWORD: return mprintf("%.*s", tok->len,tok->start);
            case TK_STR: return mprintf("\"%.*s\"", tok->len,tok->start);
            case TK_I64: return mprintf("%ld",tok->i64);
            case TK_F64: return mprintf("%f",tok->f64);
            case TK_IDENT: return mprintf("%.*s", tok->len,tok->start);
            default: return mprintf("%.*s",tok->len, tok->start);
        }
    } else if (is_terminal && is_err) {
        AoStr *buf = aoStrNew();
        aoStrCat(buf,ESC_BOLD_RED);
        switch (tok->tk_type) {
            case TK_KEYWORD: aoStrCatFmt(buf, "%.*s", tok->len,tok->start); break;
            case TK_STR: aoStrCatFmt(buf,"\"%.*s\"", tok->len,tok->start); break;
            case TK_I64: aoStrCatFmt(buf,"%I",tok->i64); break;
            case TK_F64: aoStrCatFmt(buf,"%f",tok->f64); break;
            case TK_IDENT: aoStrCatFmt(buf,"%.*s", tok->len,tok->start); break;
            default: aoStrCatFmt(buf,"%.*s",tok->len, tok->start); break;
        }
        aoStrCat(buf,ESC_RESET);
        return aoStrMove(buf);
    } else {
        switch (tok->tk_type) {
            case TK_KEYWORD: return mprintf(ESC_BLUE"%.*s"ESC_RESET, tok->len, tok->start);
            case TK_STR: return mprintf(ESC_GREEN"\"%.*s\""ESC_RESET, tok->len,tok->start);
            case TK_I64: return mprintf(ESC_PURPLE"%ld"ESC_RESET, tok->i64);
            case TK_F64: return mprintf(ESC_PURPLE"%f"ESC_RESET, tok->f64);
            case TK_IDENT: {
                if (cctrlIsKeyword(cc, tok->start, tok->len)) {
                    return mprintf(ESC_BLUE"%.*s"ESC_RESET,tok->len, tok->start);
                } else {
                    return mprintf("%.*s",tok->len, tok->start);
                }
            }
            default: return mprintf("%.*s",tok->len, tok->start);
        }
    }
}

s64 cctrlGetCharErrorIdx(Cctrl *cc, Lexeme *cur_tok,
                             const char *line_buffer)
{
    (void)cc;
    Lexeme tok;
    Lexer l;
    lexInit(&l, (char *)line_buffer, CCF_ACCEPT_WHITESPACE);

    s64 offset = 0;
    s64 latest_offset = 0;
    int match = 0;
    /* to the beginning of the line */
    while (lex(&l,&tok)) {
        /* We want to find the last instance of the error not the first */
        if (lexemeEq(cur_tok,&tok)) {
            match = 1;
            latest_offset = offset;
        }
        offset += tok.len;
        if (cur_tok->tk_type != TK_STR && tok.tk_type == TK_STR) offset++;
    }
    if (!match) {
        return -1;
    }
    return latest_offset;
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
    if (is_terminal) {
        aoStrCat(buf, ESC_CYAN"     |\n"ESC_RESET);
    } else {
        aoStrCat(buf, "     |\n");
    }
    Lexeme *cur_tok = cctrlTokenPeek(cc);

    AoStr *colored_buffer = aoStrNew();
    s64 offset = -1;
    s64 tok_len = -1;
    Lexeme tok;
    Lexer l;
    lexInit(&l, (char *)line_buffer, CCF_ACCEPT_WHITESPACE);
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

    if (is_terminal) {
        aoStrCatPrintf(buf, ESC_CYAN"%4ld |"ESC_RESET"    ", lineno);
        aoStrCatFmt(buf, "%S\n", colored_buffer);
    } else {
        aoStrCatPrintf(buf, "%4ld |    %s\n", lineno, colored_buffer->data);
    }
    if (_offset) *_offset = offset;
    if (_tok_len) *_tok_len = tok_len;
    aoStrRelease(colored_buffer);
}

AoStr *cctrlCreateErrorLine(Cctrl *cc,
                            s64 lineno, 
                            char *msg,
                            int severity,
                            char *suggestion)
{
    char *color = severity == CCTRL_ERROR || CCTRL_ICE ? ESC_BOLD_RED : CCTRL_WARN ? ESC_BOLD_YELLOW : ESC_CYAN;
    if (!cc->lexer_) {
        AoStr *buf = aoStrNew();
        cctrlFileAndLine(cc,buf,lineno,-1,msg,severity);
        if (is_terminal) {
            aoStrCat(buf, ESC_CYAN"     |\n"ESC_RESET);
        } else {
            aoStrCat(buf, "     |\n");
        }
        return buf;
    }

    const char *line_buffer = lexerReportLine(cc->lexer_,lineno);
    Lexeme *cur_tok = cctrlTokenPeek(cc);
    AoStr *buf = aoStrNew();
    s64 char_pos = cctrlGetCharErrorIdx(cc,cur_tok, line_buffer);

    s64 offset = -1;
    s64 tok_len = -1;

    cctrlFileAndLine(cc,buf,cur_tok->line,char_pos,msg,severity);
    cctrlCreateColoredLine(cc, buf, lineno, 1, suggestion,
            char_pos, &offset, &tok_len, line_buffer);

    if (char_pos != -1 && tok_len != -1) {
        if (is_terminal) {
            aoStrCat(buf, ESC_CYAN"     |    "ESC_RESET);
        } else {
            aoStrCat(buf, "     |    ");
        }

        for (int i = 0; i < char_pos; ++i) {
            aoStrPutChar(buf,' ');
        }

        if (is_terminal) {
            for (int i = 0; i < tok_len; ++i) {
                aoStrCatFmt(buf,"%s^%s",color,ESC_RESET);
            }
        } else {
            for (int i = 0; i < tok_len; ++i) {
                aoStrCat(buf, "^");
            }
        }

        if (suggestion) {
            if (is_terminal) {
                aoStrCatFmt(buf, "%s %s%s",color,suggestion,ESC_RESET);
            } else {
                aoStrCatFmt(buf, " %s", suggestion);
            }
        }
    } else {
        if (is_terminal) {
            aoStrCat(buf, ESC_CYAN"     |"ESC_RESET);
        } else {
            aoStrCat(buf, "     |");
        }
    }
    return buf;
}

AoStr *cctrlMessagVnsPrintF(Cctrl *cc, char *fmt, va_list ap, int severity) {
    char *msg = mprintVa(fmt, ap, NULL);
    AoStr *bold_msg = aoStrNew();
    if (is_terminal) {
        aoStrCatFmt(bold_msg, ESC_BOLD"%s"ESC_CLEAR_BOLD, msg);
    } else {
        aoStrCatFmt(bold_msg, "%s", msg);
    }
    Lexeme *cur_tok = cctrlTokenPeek(cc);
    AoStr *buf = cctrlCreateErrorLine(cc,cur_tok->line,bold_msg->data,severity,NULL);
    aoStrRelease(bold_msg);
    return buf;
}

AoStr *cctrlMessagVnsPrintFWithSuggestion(Cctrl *cc, char *fmt, va_list ap, 
                                          int severity, char *suggestion)
{
    char *msg = mprintVa(fmt, ap, NULL);
    AoStr *bold_msg = aoStrNew();
    if (is_terminal) {
        aoStrCatFmt(bold_msg, ESC_BOLD"%s"ESC_CLEAR_BOLD, msg);
    } else {
        aoStrCatFmt(bold_msg, "%s", msg);
    }
    Lexeme *cur_tok = cctrlTokenPeek(cc);
    AoStr *buf = cctrlCreateErrorLine(cc,cur_tok->line,bold_msg->data,severity,suggestion);
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
    fprintf(stderr,"%s\n",buf->data);
    aoStrRelease(buf);
    va_end(ap);
}

void cctrlWarning(Cctrl *cc, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    AoStr *buf = cctrlMessagVnsPrintF(cc,fmt,ap,CCTRL_WARN);
    fprintf(stderr,"%s\n",buf->data);
    aoStrRelease(buf);
    va_end(ap);
}

void cctrlRaiseException(Cctrl *cc, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    AoStr *buf = cctrlMessagVnsPrintF(cc,fmt,ap,CCTRL_ERROR);
    fprintf(stderr,"%s\n",buf->data);
    aoStrRelease(buf);
    va_end(ap);
    exit(EXIT_FAILURE);
}

void cctrlRaiseSuggestion(Cctrl *cc, char *suggestion, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    AoStr *buf = cctrlMessagVnsPrintFWithSuggestion(cc,fmt,ap,CCTRL_ERROR,suggestion);
    fprintf(stderr,"%s\n",buf->data);
    aoStrRelease(buf);
    va_end(ap);
    exit(EXIT_FAILURE);
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

/* Rewind the token buffer until there is a match */
void cctrlRewindUntilPunctMatch(Cctrl *cc, s64 ch, int *_count) {
    int count = 0;
    Lexeme *peek = cctrlTokenPeek(cc);
    while (!tokenPunctIs(peek, ch)) {
        cctrlTokenRewind(cc);
        peek = cctrlTokenPeek(cc);
        count++;
        if (count > 5) break; // has to be some limit
    }
    if (_count) *_count = count;
}

void cctrlRewindUntilStrMatch(Cctrl *cc, char *str, int len, int *_count) {
    int count = 0;
    Lexeme *peek = cctrlTokenPeek(cc);
    while (!(peek->len == len && memcmp(peek->start,str,len) == 0)) {
        cctrlTokenRewind(cc);
        peek = cctrlTokenPeek(cc);
        count++;
        if (count > 5) break; // has to be some limit
    }
    if (_count) *_count = count;
}


/* assert the token we are currently pointing at is a TK_PUNCT and the 'i64'
 * matches 'expected'. Then consume this token else throw an error */
void cctrlTokenExpect(Cctrl *cc, s64 expected) {
    Lexeme *tok = cctrlTokenGet(cc);
    if (!tokenPunctIs(tok, expected)) {
        if (!tok) {
            loggerPanic("line %lld: Ran out of tokens\n",cc->lineno);
        } else {
            cctrlRewindUntilStrMatch(cc,tok->start,tok->len,NULL);
            cctrlTokenRewind(cc);
            AoStr *info_msg = cctrlMessagePrintF(cc,CCTRL_INFO,"Previous line was");
            cctrlTokenGet(cc);


            AoStr *err_msg = cctrlMessagePrintF(cc,CCTRL_ERROR,"Syntax error, got an unexpected %s `%.*s`, perhaps you meant `%c`?",
                    lexemeTypeToString(tok->tk_type), 
                    tok->len, tok->start,
                    (char)expected);

            fprintf(stderr,"%s\n%s\n",info_msg->data, err_msg->data);
            aoStrRelease(info_msg);
            aoStrRelease(err_msg);
            exit(EXIT_FAILURE);
        }
    }
}

AoStr *cctrlRaiseFromTo(Cctrl *cc, int severity, char *suggestion, char from, 
                        char to, char *fmt, va_list ap)
{
    char *color = severity == CCTRL_ERROR || CCTRL_ICE ? ESC_BOLD_RED : CCTRL_WARN ? ESC_BOLD_YELLOW : ESC_CYAN;
    char *msg = mprintVa(fmt, ap, NULL);
    AoStr *bold_msg = aoStrNew();
    aoStrCatFmt(bold_msg, ESC_BOLD"%s"ESC_CLEAR_BOLD, msg);
    Lexeme *cur_tok = cctrlTokenPeek(cc);

    char *line_buffer = lexerReportLine(cc->lexer_, cur_tok->line);
    AoStr *buf = aoStrNew();
    /* This is not a terribly efficient way of getting an error message */
    s64 char_pos = cctrlGetCharErrorIdx(cc,cur_tok, line_buffer);
    s64 from_idx = cctrlGetErrorIdx(cc,cur_tok->line,from, line_buffer);
    s64 to_idx = cctrlGetErrorIdx(cc,cur_tok->line,to, line_buffer);

    s64 offset = -1;
    s64 tok_len = -1;

    cctrlFileAndLine(cc,buf,cur_tok->line,char_pos,bold_msg->data,severity);
    cctrlCreateColoredLine(cc, buf, cur_tok->line, 0, NULL, char_pos, &offset, &tok_len, 
            line_buffer);

    if (from_idx != -1 && to_idx != -1) {
        aoStrCat(buf, ESC_CYAN"     |    "ESC_RESET);
        for (int i = 0; i < from_idx; ++i) {
            aoStrPutChar(buf,' ');
        }
        for (int i = 0; i < (to_idx+1)-from_idx; ++i) {
            aoStrCatFmt(buf,"%s^%s",color,ESC_RESET);
        }
        if (suggestion) {
            if (is_terminal) {
                aoStrCatFmt(buf,"%s %s%s",color,suggestion,ESC_RESET);
            } else {
                aoStrCatFmt(buf, " %s", suggestion);
            }
        }
    } else {
        aoStrCat(buf, ESC_CYAN"     |"ESC_RESET);
    }

    aoStrRelease(bold_msg);
    return buf;
}

void cctrlRaiseExceptionFromTo(Cctrl *cc, char *suggestion, char from, char to, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    AoStr *buf = cctrlRaiseFromTo(cc,CCTRL_ERROR,suggestion,from,to,fmt,ap);
    fprintf(stderr,"%s\n",buf->data);
    va_end(ap);
    aoStrRelease(buf);
    exit(EXIT_FAILURE);
}

void cctrlWarningFromTo(Cctrl *cc, char *suggestion, char from, char to, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    AoStr *buf = cctrlRaiseFromTo(cc,CCTRL_WARN,suggestion,from,to,fmt,ap);
    fprintf(stderr,"%s\n",buf->data);
    va_end(ap);
    aoStrRelease(buf);
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
