#ifndef CCTRL_H
#define CCTRL_H

#include "aostr.h"
#include "ast.h"
#include "dict.h"
#include "ir.h"
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

    /* A list of intermediate representations of the code */
    List *ir_list;

    /* Temporary ir list for storing a list of ir i.e for a function body
     * or a function call */
    List *tmp_ir_list;

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

    /* Temporary jump label */
    aoStr *tmp_label;

    /* Temporary jump label 2, can be used for anything */
    aoStr *tmp_label_2;

    /* A list of tokens that have been through the lexer */
    TokenIter *tkit;

    /* current line number */
    long lineno;

    long stack_local_space;

    /* Is the current type or function static,
     * with functions this has no real difference */
    int is_static;

    /* Function that is being transformed to an intermediate 
     * representation */
    IrInstruction *ir_func;
} Cctrl;

/* Instantiate a new compiler control struct */
Cctrl *CctrlNew(void);
/* Slimmed down Cctrl, for expanding macros */
Cctrl *CcMacroProcessor(Dict *macro_defs);
void CctrlInitTokenIter(Cctrl *cc, List *tokens);
lexeme *CctrlTokenGet(Cctrl *cc);
lexeme *CctrlTokenPeek(Cctrl *cc);
void CctrlTokenIterSetCur(Cctrl *cc, List *cur);
void CctrlTokenRewind(Cctrl *cc);
void CctrlTokenExpect(Cctrl *cc, long expected);

Ast *CctrlGetVar(Cctrl *cc, char *varname, int len);
int CctrlIsKeyword(Cctrl *cc, char *name, int len);
AstType *CctrlGetKeyWord(Cctrl *cc, char *name, int len);

/* Ir Routines */
long CctrlGetTmpRegister(Cctrl *cc, char *varname, int len);
void CctrlSetTmpRegister(Cctrl *cc, char *varname, long register_num);
void irMain(Cctrl *cc);

#endif // !CCTRL_H
