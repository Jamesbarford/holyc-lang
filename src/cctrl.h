#ifndef CCTRL_H
#define CCTRL_H

#include <sys/types.h>

#include "aostr.h"
#include "ast.h"
#include "config.h"
#include "containers.h"
#include "lexer.h"

#define CCTRL_TOKEN_BUFFER_SIZE 16
#define CCTRL_TOKEN_BUFFER_MASK CCTRL_TOKEN_BUFFER_SIZE-1

#define CCTRL_TRANSPILING     (1<<0)
#define CCTRL_SAVE_ANONYMOUS  (1<<1)
#define CCTRL_PASTE_DEFINES   (1<<2)
#define CCTRL_PRESERVE_SIZEOF (1<<3)

/* For messages */
#define CCTRL_ICE   0
#define CCTRL_INFO  1
#define CCTRL_WARN  2
#define CCTRL_ERROR 3

typedef struct TokenRingBuffer {
    s64 tail;
    s64 head;
    s64 size;
    s64 capacity;
    Lexeme **entries;
} TokenRingBuffer;

TokenRingBuffer *tokenRingBufferNew(void);

void tokenBufferPrint(TokenRingBuffer *ring_buffer);
int tokenRingBufferEmpty(TokenRingBuffer *ring_buffer);
void tokenRingBufferPush(TokenRingBuffer *ring_buffer, Lexeme *token);
Lexeme *tokenRingBufferPop(TokenRingBuffer *ring_buffer);
Lexeme *tokenRingBufferPeek(TokenRingBuffer *ring_buffer);
int tokenRingBufferRewind(TokenRingBuffer *ring_buffer);

typedef struct Cctrl {
    u64 flags;
    /* The global environment for user defined types, functions and global
     * variables */
    Map *global_env;

    /* key words defined in the language */
    Map *symbol_table;

    /* Class definitions */
    Map *clsdefs;

    /* Union definitions */
    Map *uniondefs;

    /* Local environment for a function */
    Map *localenv;

    /* assembly functions that have been bound to HC, this is the HC 
     * name, not the assembly name */
    Map *asm_funcs;

    /* Macro definitions */
    Map *macro_defs;

    /* Registers */
    Set *x86_registers;

    /* libc function names */
    Set *libc_functions;

    /* Assembly function name to assembly block mapping */
    Map *asm_functions;

    /* asm blocks */
    List *asm_blocks;

    /* Strings */
    Map *strs;

    /* The Ast Tree */
    List *ast_list;

    /* Current function return type */
    AstType *tmp_rettype;

    /* Temporary function parameters, for trying to keep track of function 
     * pointers */
    Vec *tmp_params;

    /* Local variables */
    List *tmp_locals;

    /* For parsing a switch statement */
    Vec *tmp_case_list;

    /* function parameters */
    List *func_params;

    /* Top level statements, calls and globals */
    List *initalisers;
    List *initaliser_locals;

    /* For parsing a switch */
    Ast *tmp_default_case;

    /* Function being parsed */
    Ast *tmp_func;

    /* When parsing & converting to assembly these keep a reference to the
     * current loop's labels */
    AoStr *tmp_loop_begin;
    AoStr *tmp_loop_end;

    /* Temporary asm function name */
    AoStr *tmp_asm_fname;

    /* Temporary name of the function being parsed */
    AoStr *tmp_fname;

    /* current line number */
    s64 lineno;

    s64 stack_local_space;

    /* Is the current type or function static,
     * with functions this has no real difference */
    int is_static;
    TokenRingBuffer *token_buffer;
    Lexer *lexer_;
} Cctrl;

/* Instantiate a new compiler control struct */
Cctrl *cctrlNew(void);
/* Slimmed down Cctrl, for expanding macros */
Cctrl *ccMacroProcessor(Map *macro_defs);
Lexeme *cctrlTokenGet(Cctrl *cc);
Lexeme *cctrlAsmTokenGet(Cctrl *cc);
Lexeme *cctrlTokenPeek(Cctrl *cc);
Lexeme *cctrlTokenPeekBy(Cctrl *cc, int cnt);
void cctrlInitMacroProcessor(Cctrl *cc);
void cctrlTokenRewind(Cctrl *cc);
void cctrlTokenExpect(Cctrl *cc, s64 expected);
void cctrlSetCommandLineDefines(Cctrl *cc, List *defines_list);

void cctrlInitParse(Cctrl *cc, Lexer *lexer_);

Ast *cctrlGetVar(Cctrl *cc, char *varname, int len);
int cctrlIsKeyword(Cctrl *cc, char *name, int len);
AstType *cctrlGetKeyWord(Cctrl *cc, char *name, int len);
void cctrlInfo(Cctrl *cc, char *fmt, ...);
void cctrlWarning(Cctrl *cc, char *fmt, ...);
void cctrlWarningFromTo(Cctrl *cc, char *suggestion, char from, char to, char *fmt, ...);
__noreturn void cctrlRaiseExceptionFromTo(Cctrl *cc, char *suggestion, char from, char to, char *fmt, ...);
__noreturn void cctrlRaiseException(Cctrl *cc, char *fmt, ...);
__noreturn void cctrlRaiseSuggestion(Cctrl *cc, char *suggestion, char *fmt, ...);
__noreturn void cctrlIce(Cctrl *cc, char *fmt, ...);
Ast *cctrlGetOrSetString(Cctrl *cc, char *str, int len, s64 real_len);
void cctrlRewindUntilPunctMatch(Cctrl *cc, s64 ch, int *_count);
void cctrlRewindUntilStrMatch(Cctrl *cc, char *str, int len, int *_count);
AoStr *cctrlMessagePrintF(Cctrl *cc, int severity, char *fmt,...);

Map *cctrlCreateAstMap(Map *parent);
Map *cctrlCreateLexemeMap(void);

#endif // !CCTRL_H
