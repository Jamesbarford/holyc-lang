#ifndef CCTRL_H
#define CCTRL_H

#include <sys/types.h>

#include "aostr.h"
#include "ast.h"
#include "map.h"
#include "lexer.h"

#define HCC_VERSION "beta-v0.0.6"
#define CCTRL_TOKEN_BUFFER_SIZE 16
#define CCTRL_TOKEN_BUFFER_MASK CCTRL_TOKEN_BUFFER_SIZE-1

static const char *cctrlGetVersion(void) {
    return HCC_VERSION;
}

typedef struct TokenRingBuffer {
    ssize_t tail;
    ssize_t head;
    ssize_t size;
    lexeme **entries;
} TokenRingBuffer;

TokenRingBuffer *tokenRingBufferNew(void);

void tokenBufferPrint(TokenRingBuffer *ring_buffer);
int tokenRingBufferEmpty(TokenRingBuffer *ring_buffer);
void tokenRingBufferPush(TokenRingBuffer *ring_buffer, lexeme *token);
lexeme *tokenRingBufferPop(TokenRingBuffer *ring_buffer);
lexeme *tokenRingBufferPeek(TokenRingBuffer *ring_buffer);
int tokenRingBufferRewind(TokenRingBuffer *ring_buffer);

typedef struct Cctrl {
    /* The global environment for user defined types, functions and global
     * variables */
    StrMap *global_env;

    /* key words defined in the language */
    StrMap *symbol_table;

    /* Class definitions */
    StrMap *clsdefs;

    /* Union definitions */
    StrMap *uniondefs;

    /* Local environment for a function */
    StrMap *localenv;

    /* assembly functions that have been bound to HC */
    StrMap *asm_funcs;

    /* Macro definitions */
    StrMap *macro_defs;

    /* Registers */
    StrMap *x86_registers;

    /* libc function names */
    StrMap *libc_functions;

    /* asm blocks */
    List *asm_blocks;

    /* Strings */
    StrMap *strs;

    /* The Ast Tree */
    List *ast_list;

    /* Current function return type */
    AstType *tmp_rettype;

    /* Temporary function parameters, for trying to keep track of function 
     * pointers */
    List *tmp_params;

    /* Local variables */
    List *tmp_locals;

    /* For parsing a switch statement */
    PtrVec *tmp_case_list;

    /* function parameters */
    List *func_params;

    /* Top level statements, calls and globals */
    List *initalisers;
    List *initaliser_locals;

    /* For parsing a switch */
    Ast *tmp_default_case;

    /* When parsing & converting to assembly these keep a reference to the
     * current loop's labels */
    aoStr *tmp_loop_begin;
    aoStr *tmp_loop_end;

    /* Temporary asm function name */
    aoStr *tmp_asm_fname;

    /* Temporary name of the function being parsed */
    aoStr *tmp_fname;

    /* current line number */
    ssize_t lineno;

    long stack_local_space;

    /* Is the current type or function static,
     * with functions this has no real difference */
    int is_static;
    TokenRingBuffer *token_buffer;
    lexer *lexer_;
} Cctrl;

/* Instantiate a new compiler control struct */
Cctrl *cctrlNew(void);
/* Slimmed down Cctrl, for expanding macros */
Cctrl *ccMacroProcessor(StrMap *macro_defs);
lexeme *cctrlTokenGet(Cctrl *cc);
lexeme *cctrlTokenPeek(Cctrl *cc);
void cctrlInitMacroProcessor(Cctrl *cc);
void cctrlTokenRewind(Cctrl *cc);
void cctrlTokenExpect(Cctrl *cc, long expected);
void cctrlSetCommandLineDefines(Cctrl *cc, List *defines_list);

void cctrlInitParse(Cctrl *cc, lexer *lexer_);

Ast *cctrlGetVar(Cctrl *cc, char *varname, int len);
int cctrlIsKeyword(Cctrl *cc, char *name, int len);
AstType *cctrlGetKeyWord(Cctrl *cc, char *name, int len);
[[noreturn]] void cctrlRaiseException(Cctrl *cc, char *fmt, ...);
[[noreturn]] void cctrlIce(Cctrl *cc, char *fmt, ...);
void cctrlWarning(Cctrl *cc, char *fmt, ...);
Ast *cctrlGetOrSetString(Cctrl *cc, char *str, int len);

#endif // !CCTRL_H
