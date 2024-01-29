#ifndef CCTRL_H
#define CCTRL_H

#include "aostr.h"
#include "ast.h"
#include "dict.h"
#include "lexer.h"

typedef struct TokenIter {
    /* All of the tokens */
    List *tokens;
    /* Current position in the list */
    List *cur;
} TokenIter;

typedef struct Cctrl {
    /* The global environment for user defined types, functions and global
     * variables */
    Dict *global_env;

    /* key words defined in the language */
    Dict *symbol_table;

    /* Class definitions */
    Dict *clsdefs;

    /* Union definitions */
    Dict *uniondefs;

    /* Local environment for a function */
    Dict *localenv;

    /* assembly functions that have been bound to HC */
    Dict *asm_funcs;

    /* Macro definitions */
    Dict *macro_defs;

    /* Registers */
    Dict *x86_registers;

    /* libc function names */
    Dict *libc_functions;

    /* Asm blocks */
    List *asm_blocks;

    /* Strings */
    List *strings;

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
    List *tmp_case_list;

    /* function parameters */
    List *func_params;

    /* Top level statements, calls and globals */
    List *initalisers;
    List *initaliser_locals;

    /* For parsing a switch */
    aoStr *tmp_default_case;

    /* When parsing & converting to assembly these keep a reference to the
     * current loop's labels */
    aoStr *tmp_loop_begin;
    aoStr *tmp_loop_end;

    /* Temporary asm function name */
    aoStr *tmp_asm_fname;

    /* Temporary name of the function being parsed */
    aoStr *tmp_fname;

    /* A list of tokens that have been through the lexer */
    TokenIter *tkit;

    /* current line number */
    long lineno;

    long stack_local_space;

    /* Is the current type or function static,
     * with functions this has no real difference */
    int is_static;
} Cctrl;

/* Instantiate a new compiler control struct */
Cctrl *CctrlNew(void);
void CctrlInitTokenIter(Cctrl *cc, List *tokens);
lexeme *CctrlTokenGet(Cctrl *cc);
lexeme *CctrlTokenPeek(Cctrl *cc);
lexeme *CctrlTokenPeekBy(Cctrl *cc, int steps);
void CctrlTokenIterSetCur(Cctrl *cc, List *cur);
void CctrlTokenRewind(Cctrl *cc);
void CctrlTokenExpect(Cctrl *cc, long expected);

/* Does a list scan */
Ast *CctrlFindFunctionDef(Cctrl *cc, char *fname, int len);
Ast *CctrlFindFuncParam(Cctrl *cc, char *name, int len);
Ast *CctrlGetVar(Cctrl *cc, char *varname, int len);
int CctrlIsKeyword(Cctrl *cc, char *name, int len);
AstType *CctrlGetKeyWord(Cctrl *cc, char *name, int len);

#endif // !CCTRL_H
