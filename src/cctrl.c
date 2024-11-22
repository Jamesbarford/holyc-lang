#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "list.h"
#include "map.h"
#include "util.h"
#include "config.h"

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
    lexeme *le;
    struct tm *ptm;
    struct timeval tm;
    long milliseconds,len;
    time_t seconds;
    char *date,*time,*time_stamp;
    long bufsize = sizeof(char)*128;

    le = lexemeSentinal();
    if (IS_BSD)   strMapAdd(cc->macro_defs,"IS_BSD",le);
    if (IS_LINUX) strMapAdd(cc->macro_defs,"IS_LINUX",le);

#ifdef IS_MACOS
    strMapAdd(cc->macro_defs,"IS_MACOS",le);
#endif
    
    if (IS_X86_64)      {
        strMapAdd(cc->macro_defs,"IS_X86_64",le);
        le = lexemeNew("X86_64",6);
        le->tk_type = TK_STR;
        strMapAdd(cc->macro_defs,"__ARCH__",le);
    } else if (IS_ARM_64) {
        strMapAdd(cc->macro_defs,"IS_ARM_64",le);
        le = lexemeNew("ARM_64",6);
        le->tk_type = TK_STR;
        strMapAdd(cc->macro_defs,"__ARCH__",le);
    }

    gettimeofday(&tm,NULL);
    milliseconds = (tm.tv_sec*1000) +
        (tm.tv_usec/1000);
    seconds = milliseconds / 1000;
    ptm = localtime(&seconds);

    time = malloc(bufsize);
    date = malloc(bufsize);
    time_stamp = malloc(bufsize);

    len = snprintf(time,bufsize,"%02d:%02d:%02d",
            ptm->tm_hour,ptm->tm_min,ptm->tm_sec);
    time[len] = '\0';
    le = lexemeNew(time,len);
    le->tk_type = TK_STR;
    strMapAdd(cc->macro_defs,"__TIME__",le);

    len = snprintf(date,bufsize,"%04d/%02d/%02d",
            ptm->tm_year+1900,ptm->tm_mon+1,ptm->tm_mday);
    date[len] = '\0';
    le = lexemeNew(date,len);
    le->tk_type = TK_STR;
    strMapAdd(cc->macro_defs,"__DATE__",le);

    len = snprintf(time_stamp,bufsize,
            "%d-%02d-%02d %02d:%02d:%02d",
            ptm->tm_year+1900,
            ptm->tm_mon+1,ptm->tm_mday,ptm->tm_hour,
            ptm->tm_min,ptm->tm_sec);
    date[len] = '\0';
    le = lexemeNew(time_stamp,len);
    le->tk_type = TK_STR;
    strMapAdd(cc->macro_defs,"__TIMESTAMP__",le);

#ifdef HCC_LINK_SQLITE3
    le = lexemeSentinal();
    strMapAdd(cc->macro_defs,"__HCC_LINK_SQLITE3__",le);
#endif

    le = lexemeNew((char *)cctrlGetVersion(),len);
    le->tk_type = TK_STR;
    strMapAdd(cc->macro_defs,"__HCC_VERSION__",le);
}

Cctrl *ccMacroProcessor(StrMap *macro_defs) {
    Cctrl *cc = malloc(sizeof(Cctrl));
    cc->macro_defs = macro_defs;
    cc->strs = strMapNew(32);
    return cc;
}

/* Instantiate a new compiler control struct */
Cctrl *cctrlNew(void) {
    Cctrl *cc = malloc(sizeof(Cctrl));
    AstType *type;
    BuiltInType *bilt;
    aoStr **str_array;
    int len;

    cc->global_env = strMapNew(32);
    cc->clsdefs = strMapNew(32);
    cc->uniondefs = strMapNew(32);
    cc->symbol_table = strMapNew(32);
    cc->asm_funcs = strMapNew(32);
    cc->macro_defs = strMapNew(32);
    cc->x86_registers = strMapNew(32);
    cc->libc_functions = strMapNew(32);
    cc->strs = strMapNew(32);
    cc->asm_blocks = listNew();
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

    str_array = aoStrSplit(x86_registers,',',&len);
    for (int i = 0; i < len; ++i) {
        char *reg = aoStrMove(str_array[i]);
        strMapAdd(cc->x86_registers,reg,reg);
    }
    free(str_array);

    str_array = aoStrSplit(libc_functions,',',&len);
    for (int i = 0; i < len; ++i) {
        char *libc_func = aoStrMove(str_array[i]);
        strMapAdd(cc->libc_functions,libc_func,libc_func);
    }
    free(str_array);

    for (int i = 0; i < static_size(built_in_types); ++i) {
        type = malloc(sizeof(AstType));
        bilt = &built_in_types[i]; 
        type->size = bilt->size;
        type->issigned = bilt->issigned;
        type->kind = bilt->kind;
        type->ptr = NULL;
        strMapAdd(cc->symbol_table, bilt->name, type);
    }

    cctrlAddBuiltinMacros(cc);

    Ast *cmd_args = astGlobalCmdArgs();
    listAppend(cc->ast_list,cmd_args->argc);
    listAppend(cc->ast_list,cmd_args->argv);
    strMapAdd(cc->global_env,"argc",cmd_args->argc->declvar);
    strMapAdd(cc->global_env,"argv",cmd_args->argv->declvar);
    return cc;
}

static lexeme *token_ring_buffer[CCTRL_TOKEN_BUFFER_SIZE];

TokenRingBuffer *tokenRingBufferStaticNew(void) {
    TokenRingBuffer *ring_buffer = (TokenRingBuffer *)malloc(sizeof(TokenRingBuffer));
    ring_buffer->tail = 0;
    ring_buffer->head = 0;
    ring_buffer->size = 0;
    ring_buffer->entries = token_ring_buffer;
    for (ssize_t i = 0; i < CCTRL_TOKEN_BUFFER_SIZE; ++i) {
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

ssize_t tokenRingBufferGetIdx(ssize_t idx) {
    return (idx + 1) & CCTRL_TOKEN_BUFFER_MASK;
}

void tokenBufferPrint(TokenRingBuffer *ring_buffer) {
    for (int i = 0, idx = ring_buffer->tail; i < ring_buffer->size;
            i++, idx = tokenRingBufferGetIdx(idx)) {
        printf(">> %s\n", lexemeToString(ring_buffer->entries[idx]));
    }
    printf("\n");
}


int tokenRingBufferEmpty(TokenRingBuffer *ring_buffer) {
    return ring_buffer->size == 0;
}


/* Add a token to the ring buffer and remove the oldest element */
void tokenRingBufferPush(TokenRingBuffer *ring_buffer, lexeme *token) {
    ring_buffer->entries[ring_buffer->head] = token;
    ring_buffer->head = tokenRingBufferGetIdx(ring_buffer->head);
    if (ring_buffer->size == CCTRL_TOKEN_BUFFER_SIZE) {
        ring_buffer->tail = tokenRingBufferGetIdx(ring_buffer->tail);
    } else {
        ring_buffer->size++;
    }
}

/* Take one token from the ring buffer */
lexeme *tokenRingBufferPop(TokenRingBuffer *ring_buffer) {
    if (tokenRingBufferEmpty(ring_buffer)) {
        return NULL;
    }
    lexeme *token = ring_buffer->entries[ring_buffer->tail];
    ring_buffer->tail = tokenRingBufferGetIdx(ring_buffer->tail);
    ring_buffer->size--;
    return token;
}

lexeme *tokenRingBufferPeek(TokenRingBuffer *ring_buffer) {
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
    ring_buffer->tail = (ring_buffer->tail - 1 + CCTRL_TOKEN_BUFFER_SIZE) & CCTRL_TOKEN_BUFFER_MASK;
    ring_buffer->size++;
    return 1;
}

void cctrlInitParse(Cctrl *cc, lexer *lexer_) {
    cc->lexer_ = lexer_;
    cc->token_buffer = tokenRingBufferStaticNew();
    for (int i = 0; i < CCTRL_TOKEN_BUFFER_SIZE; ++i) {
        lexeme *token = lexToken(cc->macro_defs, cc->lexer_);
        if (!token) break;
        tokenRingBufferPush(cc->token_buffer,token);
    }
}

void cctrlInitMacroProcessor(Cctrl *cc) {
    cc->lexer_ = NULL;
    /* The caller will tack on the entries and size */
    TokenRingBuffer *ring_buffer = (TokenRingBuffer *)malloc(sizeof(TokenRingBuffer));
    ring_buffer->tail = 0;
    ring_buffer->head = 0;
    ring_buffer->size = 0;
    cc->token_buffer = ring_buffer;
}

lexeme *cctrlMaybeExpandToken(Cctrl *cc, lexeme *token) {
    if (token->tk_type != TK_IDENT) {
        return token;
    }

    lexeme *maybe_define = strMapGetLen(cc->macro_defs, token->start, token->len);
    if (maybe_define) {
        return maybe_define;
    }
    return token; 
}

lexeme *cctrlTokenPeek(Cctrl *cc) {
    lexeme *token = tokenRingBufferPeek(cc->token_buffer);
    if (token) {
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

lexeme *cctrlTokenGet(Cctrl *cc) {
    lexeme *token = cctrlTokenPeek(cc);
    if (token) {
        tokenRingBufferPop(cc->token_buffer);
        if (cc->token_buffer->size < 3 && cc->lexer_) {
            for (ssize_t i = 0; i < 10; ++i) {
                lexeme *new_token = lexToken(cc->macro_defs,cc->lexer_);
                if (!new_token) {
                    break;
                }
                tokenRingBufferPush(cc->token_buffer, new_token);
            }
        }
        return token;
    }
    return NULL;
}

aoStr *cctrlSeverityMessage(int severity) {
    aoStr *buf = aoStrNew();
    int is_terminal = isatty(STDOUT_FILENO);
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
                free(ice_msg);
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
                free(ice_msg);
                break;
            }
        }
    }
    return buf;
}

void cctrlFileAndLine(Cctrl *cc, aoStr *buf, ssize_t lineno, ssize_t char_pos, char *msg, int severity) {
    aoStr *severity_msg = cctrlSeverityMessage(severity);
    int is_terminal = isatty(STDOUT_FILENO);

    aoStrCatPrintf(buf,"%s%s", severity_msg->data, msg);
    if (buf->data[buf->len - 1] != '\n') {
        aoStrPutChar(buf,'\n');
    }

    if (cc->lexer_) {
        lexFile *file = cc->lexer_->cur_file;
        char *file_name = file->filename->data;
        if (is_terminal) {
            aoStrCatPrintf(buf, " "ESC_CYAN"-->"ESC_RESET" %s:%ld",
                    file_name,
                    lineno);
        } else {
            aoStrCatPrintf(buf, " --> %s:%ld", file_name, lineno);
        }
        if (char_pos > -1) {
            aoStrCatPrintf(buf,":%ld",char_pos+1);
        }
        aoStrPutChar(buf,'\n');
    } else {
        aoStrCatPrintf(buf, "Parsing macro:%d ", lineno);
    }
    aoStrRelease(severity_msg);
}

char *lexemeToColor(Cctrl *cc, lexeme *tok, int is_err) {
    int is_terminal = isatty(STDOUT_FILENO);
    if (!is_terminal) {
        switch (tok->tk_type) {
            case TK_KEYWORD: return mprintf("%.*s", tok->len,tok->start);
            case TK_STR: return mprintf("\"%.*s\"", tok->len,tok->start);
            case TK_I64: return mprintf("%ld",tok->i64);
            case TK_F64: return mprintf("%f",tok->f64);
            case TK_IDENT: return mprintf("%.*s", tok->len,tok->start);
            default: return mprintf("%.*s",tok->len, tok->start);
        }
    } else if (is_err) {
        aoStr *buf = aoStrNew();
        aoStrCat(buf,ESC_BOLD_RED);
        switch (tok->tk_type) {
            case TK_KEYWORD: aoStrCatPrintf(buf, "%.*s", tok->len,tok->start); break;
            case TK_STR: aoStrCatPrintf(buf,"\"%.*s\"", tok->len,tok->start); break;
            case TK_I64: aoStrCatPrintf(buf,"%ld",tok->i64); break;
            case TK_F64: aoStrCatPrintf(buf,"%f",tok->f64); break;
            case TK_IDENT: aoStrCatPrintf(buf,"%.*s", tok->len,tok->start); break;
            default: aoStrCatPrintf(buf,"%.*s",tok->len, tok->start); break;
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

ssize_t cctrlGetCharErrorIdx(Cctrl *cc, lexeme *cur_tok) {
    const char *line_buffer = lexerReportLine(cc->lexer_,cur_tok->line);
    ssize_t offset = 0;
    lexeme tok;
    lexer l;
    ssize_t latest_offset = 0;
    lexInit(&l, (char *)line_buffer, CCF_ACCEPT_WHITESPACE);
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

ssize_t cctrlGetErrorIdx(Cctrl *cc, ssize_t line, char ch) {
    const char *line_buffer = lexerReportLine(cc->lexer_,line);
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
                            aoStr *buf,
                            ssize_t lineno,
                            int should_color_err,
                            char *suggestion,
                            ssize_t char_pos,
                            ssize_t *_offset,
                            ssize_t *_tok_len,
                            ssize_t *_line_len)
{
    int is_terminal = isatty(STDOUT_FILENO);
    if (is_terminal) {
        aoStrCat(buf, ESC_CYAN"     |\n"ESC_RESET);
    } else {
        aoStrCat(buf, "     |\n");
    }
    lexeme *cur_tok = cctrlTokenPeek(cc);
    const char *line_buffer = lexerReportLine(cc->lexer_,lineno);

    aoStr *colored_buffer = aoStrNew();
    long offset = -1;
    long tok_len = -1;
    char *ptr = (char *)line_buffer;
    lexeme tok;
    lexer l;
    lexInit(&l, (char *)line_buffer, CCF_ACCEPT_WHITESPACE);
    int collect_whitespace = 0;
    ssize_t current_offset = 0;

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
        free(colored_lexeme);
        ptr += tok.len;
        current_offset += tok.len;
    }

    if (is_terminal) {
        aoStrCatPrintf(buf, ESC_CYAN"%4ld |"ESC_RESET"    %s\n", lineno, colored_buffer->data);
    } else {
        aoStrCatPrintf(buf, "%4ld |    %s\n", lineno, colored_buffer->data);
    }
    if (_offset) *_offset = offset;
    if (_tok_len) *_tok_len = tok_len;
    if (_line_len) *_line_len = strlen(line_buffer);
    aoStrRelease(colored_buffer);
}

aoStr *cctrlCreateErrorLine(Cctrl *cc,
                            ssize_t lineno, 
                            char *msg,
                            int severity,
                            char *suggestion)
{
    int is_terminal = isatty(STDOUT_FILENO);
    if (!cc->lexer_) {
        aoStr *buf = aoStrNew();
        cctrlFileAndLine(cc,buf,lineno,-1,msg,severity);
        if (is_terminal) {
            aoStrCat(buf, ESC_CYAN"     |\n"ESC_RESET);
        } else {
            aoStrCat(buf, "     |\n");
        }
        return buf;
    }

    lexeme *cur_tok = cctrlTokenPeek(cc);
    aoStr *buf = aoStrNew();
    long char_pos = cctrlGetCharErrorIdx(cc,cur_tok);

    long offset = -1;
    long tok_len = -1;
    long line_len = -1;

    cctrlFileAndLine(cc,buf,cur_tok->line,char_pos,msg,severity);
    cctrlCreateColoredLine(cc, buf, lineno, 1, suggestion,
            char_pos, &offset, &tok_len, &line_len);

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
                aoStrCat(buf,ESC_BOLD_RED"^"ESC_RESET);
            }
        } else {
            for (int i = 0; i < tok_len; ++i) {
                aoStrCat(buf, "^");
            }
        }

        if (suggestion) {
            if (is_terminal) {
                aoStrCatPrintf(buf, ESC_BOLD_RED" %s"ESC_RESET, suggestion);
            } else {
                aoStrCatPrintf(buf, " %s", suggestion);
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

aoStr *cctrlMessagVnsPrintF(Cctrl *cc, char *fmt, va_list ap, int severity) {
    char *msg = mprintVa(fmt, ap);
    aoStr *bold_msg = aoStrNew();
    aoStrCatPrintf(bold_msg, ESC_BOLD"%s"ESC_CLEAR_BOLD, msg);
    lexeme *cur_tok = cctrlTokenPeek(cc);
    aoStr *buf = cctrlCreateErrorLine(cc,cur_tok->line,bold_msg->data,severity,NULL);
    aoStrRelease(bold_msg);
    free(msg);
    return buf;
}

aoStr *cctrlMessagVnsPrintFWithSuggestion(Cctrl *cc, char *fmt, va_list ap, 
                                          int severity, char *suggestion)
{
    char *msg = mprintVa(fmt, ap);
    aoStr *bold_msg = aoStrNew();
    aoStrCatPrintf(bold_msg, ESC_BOLD"%s"ESC_CLEAR_BOLD, msg);
    lexeme *cur_tok = cctrlTokenPeek(cc);
    aoStr *buf = cctrlCreateErrorLine(cc,cur_tok->line,bold_msg->data,severity,suggestion);
    aoStrRelease(bold_msg);
    free(msg);
    return buf;
}

aoStr *cctrlMessagePrintF(Cctrl *cc, int severity, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    aoStr *buf = cctrlMessagVnsPrintF(cc,fmt,ap,severity);
    va_end(ap);
    return buf;
}

void cctrlInfo(Cctrl *cc, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    aoStr *buf = cctrlMessagVnsPrintF(cc,fmt,ap,CCTRL_INFO);
    fprintf(stderr,"%s\n",buf->data);
    aoStrRelease(buf);
    va_end(ap);
}

void cctrlWarning(Cctrl *cc, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    aoStr *buf = cctrlMessagVnsPrintF(cc,fmt,ap,CCTRL_WARN);
    fprintf(stderr,"%s\n",buf->data);
    aoStrRelease(buf);
    va_end(ap);
}

void cctrlRaiseException(Cctrl *cc, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    aoStr *buf = cctrlMessagVnsPrintF(cc,fmt,ap,CCTRL_ERROR);
    fprintf(stderr,"%s\n",buf->data);
    aoStrRelease(buf);
    va_end(ap);
    exit(EXIT_FAILURE);
}

void cctrlRaiseSuggestion(Cctrl *cc, char *suggestion, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    aoStr *buf = cctrlMessagVnsPrintFWithSuggestion(cc,fmt,ap,CCTRL_ERROR,suggestion);
    fprintf(stderr,"%s\n",buf->data);
    aoStrRelease(buf);
    va_end(ap);
    exit(EXIT_FAILURE);
}

void cctrlIce(Cctrl *cc, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    aoStr *buf = cctrlMessagVnsPrintF(cc,fmt,ap,CCTRL_ICE);
    fprintf(stderr,"%s\n",buf->data);
    aoStrRelease(buf);
    va_end(ap);
    exit(EXIT_FAILURE);
}

/* Rewind the token buffer until there is a match */
void cctrlRewindUntilPunctMatch(Cctrl *cc, long ch, int *_count) {
    int count = 0;
    lexeme *peek = cctrlTokenPeek(cc);
    while (!tokenPunctIs(peek, ch)) {
        cctrlTokenRewind(cc);
        peek = cctrlTokenPeek(cc);
        count++;
        if (count > 8) break; // has to be some limit
    }
    if (_count) *_count = count;
}

void cctrlRewindUntilStrMatch(Cctrl *cc, char *str, int len, int *_count) {
    int count = 0;
    lexeme *peek = cctrlTokenPeek(cc);
    while (!(peek->len == len && memcmp(peek->start,str,len) == 0)) {
        cctrlTokenRewind(cc);
        peek = cctrlTokenPeek(cc);
        count++;
    }
    if (_count) *_count = count;
}


/* assert the token we are currently pointing at is a TK_PUNCT and the 'i64'
 * matches 'expected'. Then consume this token else throw an error */
void cctrlTokenExpect(Cctrl *cc, long expected) {
    lexeme *tok = cctrlTokenGet(cc);
    if (!tokenPunctIs(tok, expected)) {
        if (!tok) {
            loggerPanic("line %ld: Ran out of tokens\n",cc->lineno);
        } else {
            cctrlRewindUntilStrMatch(cc,tok->start,tok->len,NULL);
            cctrlTokenRewind(cc);
            aoStr *info_msg = cctrlMessagePrintF(cc,CCTRL_INFO,"Previous line was");
            cctrlTokenGet(cc);


            aoStr *err_msg = cctrlMessagePrintF(cc,CCTRL_ERROR,"Syntax error, got an unexpected %s `%.*s`, perhaps you meant `%c`?",
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

void cctrlRaiseExceptionFromTo(Cctrl *cc, char *suggestion, char from, char to, char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    char *msg = mprintVa(fmt, ap);
    aoStr *bold_msg = aoStrNew();
    aoStrCatPrintf(bold_msg, ESC_BOLD"%s"ESC_CLEAR_BOLD, msg);
    lexeme *cur_tok = cctrlTokenPeek(cc);

    aoStr *buf = aoStrNew();
    /* This is not a terribly efficient way of getting an error message */
    long char_pos = cctrlGetCharErrorIdx(cc,cur_tok);
    long from_idx = cctrlGetErrorIdx(cc,cur_tok->line,from);
    long to_idx = cctrlGetErrorIdx(cc,cur_tok->line,to);

    long offset = -1;
    long tok_len = -1;
    long line_len = -1;

    cctrlFileAndLine(cc,buf,cur_tok->line,char_pos,bold_msg->data,CCTRL_ERROR);
    cctrlCreateColoredLine(cc, buf, cur_tok->line, 0, NULL, char_pos, &offset, &tok_len, &line_len);

    if (from_idx != -1 && to_idx != -1) {
        aoStrCat(buf, ESC_CYAN"     |    "ESC_RESET);
        for (int i = 0; i < from_idx; ++i) {
            aoStrPutChar(buf,' ');
        }
        for (int i = 0; i < (to_idx+1)-from_idx; ++i) {
            aoStrCat(buf,ESC_BOLD_RED"^"ESC_RESET);
        }
        if (suggestion) {
            aoStrCatPrintf(buf, ESC_BOLD_RED" %s"ESC_RESET, suggestion);
        }
    } else {
        aoStrCat(buf, ESC_CYAN"     |"ESC_RESET);
    }

    fprintf(stderr,"%s\n",buf->data);
    aoStrRelease(buf);
    aoStrRelease(bold_msg);
    va_end(ap);
    exit(EXIT_FAILURE);
}



/* Get variable either from the local or global scope */
Ast *cctrlGetVar(Cctrl *cc, char *varname, int len) {
    Ast *ast_var;
    lexeme *tok;

    if (cc->localenv && (ast_var = strMapGetLen(cc->localenv, varname, len)) != NULL) {
        return ast_var;
    }

    if ((ast_var = strMapGetLen(cc->global_env, varname, len)) != NULL) {
        return ast_var;
    }

    if ((ast_var = strMapGetLen(cc->asm_funcs, varname, len)) != NULL) {
        return ast_var;
    }

    /* Expand a macro definition */
    if ((tok = strMapGetLen(cc->macro_defs, varname, len)) != NULL) {
        switch (tok->tk_type) {
        case TK_I64:
            return astI64Type(tok->i64);
        case TK_F64:
            return astF64Type(tok->f64);
        case TK_STR:
            return cctrlGetOrSetString(cc, tok->start, tok->len);
        default:
            return NULL;
        }
    }
    return NULL;
}

AstType *cctrlGetKeyWord(Cctrl *cc, char *name, int len) {
    AstType *type;
    assert(name != NULL);
    if ((type = strMapGetLen(cc->symbol_table,name,len)) != NULL) {
        return type;
    }
    /* Classes are types */
    if ((type = strMapGetLen(cc->clsdefs,name,len)) != NULL) {
        return type;
    }
    if ((type = strMapGetLen(cc->uniondefs,name,len)) != NULL) {
        return type;
    }
    return NULL;
}

int cctrlIsKeyword(Cctrl *cc, char *name, int len) {
    return cctrlGetKeyWord(cc,name,len) != NULL;
}

void cctrlSetCommandLineDefines(Cctrl *cc, List *defines_list) {
    listForEach(defines_list) {
        strMapAdd(cc->macro_defs,(char*)it->value,lexemeSentinal());
    }
}

/* De-duplicates strings by checking their existance */
Ast *cctrlGetOrSetString(Cctrl *cc, char *str, int len) {
    Ast *ast_str = NULL;
    if (cc->strs) {
        ast_str = strMapGetLen(cc->strs, str, len);
        if (!ast_str) {
            ast_str = astString(str,len);
            strMapAdd(cc->strs, ast_str->sval->data, ast_str);
        }
    } else {
        ast_str = astString(str,len);
    }
    return ast_str;
}
