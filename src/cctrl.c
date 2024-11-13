#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>

#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "list.h"
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
    if (IS_BSD)        strMapAdd(cc->macro_defs,"IS_BSD",le);
    if (IS_MACOS)      strMapAdd(cc->macro_defs,"IS_MACOS",le);
    else if (IS_LINUX) strMapAdd(cc->macro_defs,"IS_LINUX",le);
    
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
    cc->tkit = malloc(sizeof(TokenIter));
    cc->macro_defs = macro_defs;
    cc->strings = listNew();
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
    cc->strings = listNew();
    cc->asm_blocks = listNew();
    cc->ast_list = listNew();
    cc->initalisers = listNew();
    cc->initaliser_locals = listNew();
    cc->tkit = malloc(sizeof(TokenIter));
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

void cctrlInitTokenIter(Cctrl *cc, List *tokens) {
    cc->tkit->tokens = tokens;
    listPrepend(tokens,lexemeSentinal());
    cc->tkit->cur = tokens->next->next;
}

/* Have a look at the next lexeme but don't consume */
lexeme *cctrlTokenPeek(Cctrl *cc) {
    TokenIter *it = cc->tkit;
    lexeme *retval, *macro;

    if (it->cur == it->tokens) {
        return NULL;
    }

    retval = (lexeme *)it->cur->value;
    if (retval->tk_type == TK_IDENT) {
        if ((macro = strMapGetLen(cc->macro_defs,retval->start,retval->len)) != NULL) {
            return macro;
        }
    }
    return retval;
}

/** 
 * Get the current lexeme pointed to by cur and set cur to the 
 * next lexeme */
lexeme *cctrlTokenGet(Cctrl *cc) {
    lexeme *tok;
    if ((tok = cctrlTokenPeek(cc)) != NULL) {
        cc->lineno = tok->line;
        cc->tkit->cur = cc->tkit->cur->next;
        return tok;
    }
    return NULL;
}

/* assert the token we are currently pointing at is a TK_PUNCT and the 'i64'
 * matches 'expected'. Then consume this token else throw an error */
void cctrlTokenExpect(Cctrl *cc, long expected) {
    lexeme *tok = cctrlTokenGet(cc);
    if (!tokenPunctIs(tok, expected)) {
        if (!tok) {
            //lexemePrintList(cc->tkit->tokens->next);
            loggerPanic("line %ld: Ran out of tokens\n",cc->lineno);
        } else {
            loggerPanic("line %d: Syntax error in on line expected '%c' got: %.*s\n",
                    tok->line, (char)expected,
                    tok->len, tok->start);
        }
    }
}

/* Go back one */
void cctrlTokenRewind(Cctrl *cc) {
    cc->tkit->cur = cc->tkit->cur->prev;
    lexeme *current = (lexeme *)cc->tkit->cur->value;
    cc->lineno = current->line;
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
            ast_var = astString(tok->start, tok->len);
            listAppend(cc->strings, ast_var);
            return ast_var;
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
