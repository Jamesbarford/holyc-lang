#ifndef CCTRL_H
#define CCTRL_H

#include <setjmp.h>
#include <stdarg.h>
#include <sys/types.h>

#include "aostr.h"
#include "ast.h"
#include "cli.h"
#include "config.h"
#include "containers.h"
#include "lexer.h"

#define CCTRL_TOKEN_BUFFER_SIZE 16
#define CCTRL_TOKEN_BUFFER_MASK CCTRL_TOKEN_BUFFER_SIZE-1

#define CCTRL_TRANSPILING          (1<<0)
#define CCTRL_SAVE_ANONYMOUS       (1<<1)
#define CCTRL_PASTE_DEFINES        (1<<2)
#define CCTRL_PRESERVE_SIZEOF      (1<<3)
#define CCTRL_ASM_HAS_INITIALISERS (1<<4)
/* REPL mode: re-defining a function replaces the previous definition
 * instead of raising "Cannot redefine function", and floating
 * expressions (`2+2;`, `(x*3);`) are legal at the top level so they
 * can be evaluated and echoed. */
#define CCTRL_REPL                 (1<<6)
/* Treat warnings as errors */
#define CCTRL_WERROR               (1<<7)
/* Create wrappers over MAlloc/Free and friends for tracking allocations
 * JIT only presently */
#define CCTRL_MEMSAFE              (1<<8)

/* For messages */
#define CCTRL_ICE   0
#define CCTRL_INFO  1
#define CCTRL_WARN  2
#define CCTRL_ERROR 3

/* A single accumulated parser/lexer diagnostic. Carries enough
 * spatial info (file + start line/col + end line/col) for an LSP
 * to render a squiggle without re-tokenising. `message` and
 * `suggestion` are owned by the diagnostic. `file` is borrowed
 * from the Lexer's LexFile list and outlives the diagnostic. */
typedef struct CctrlDiagnostic {
    int severity;          /* CCTRL_INFO / CCTRL_WARN / CCTRL_ERROR */
    struct LexFile *file;  /* borrowed; NULL if the source is unknown */
    int line;
    int col;
    int end_line;
    int end_col;
    AoStr *message;        /* owned */
    AoStr *suggestion;     /* owned; may be NULL */
} CctrlDiagnostic;

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

    /* Every file this compile has lexed: `Map<u32, AoStr *>` keyed by
     * the 1-based id stamped onto Ast->file_id (0 = unknown). */
    Map *file_map;

    /* Class definitions */
    Map *clsdefs;

    /* Union definitions */
    Map *uniondefs;

    /* Local environment for a function */
    Map *localenv;

    /* Every scope map `cctrlCreateAstMap` has handed out for the parse
     * in flight. Closing a scope only rewinds `localenv` to its parent,
     * so without an owner the map itself is unreachable and unfreed -
     * one ~1KB Map per lexical scope. A batch compile never noticed
     * (the process exits); `hcc -lsp` re-parses on every keystroke and
     * it was the single largest source of its growth. Released by
     * `cctrlReleaseScopeMaps`. */
    Vec *scope_maps;

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

    /* Global variable whose declaration is mid-parse. If the
     * initialiser errors, top-level recovery unregisters it from
     * global_env. Otherwise later code could name a variable whose
     * storage (the AST_DECL) was rolled back. */
    Ast *tmp_gvar_decl;

    /* Base type of a top-level `I64 a, b;` after a `,`: the next call of
     * parseToplevelDef carries on with the next declarator. */
    AstType *tmp_decl_base;

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

    /* Accumulated diagnostics emitted during this compilation.
     * Pushed in source order. Includes info and warnings. Not just errors. */ 
    Vec *diagnostics;

    /* `n_errors` counts only `CCTRL_ERROR` entries so the driver can decide
     * whether to proceed to codegen. */
    int n_errors;

    /* Innermost active parser recovery point. `NULL` means errors
     * are fatal. non-NULL means `cctrlRaiseException` will record the
     * diagnostic and longjmp here instead of calling `exit()`. Set/cleared by
     * the parser around top-level decls, statements which are points of
     * recovery. */
    jmp_buf *current_recovery;

    /* Externl compiler command e.g `clang --target=x86_64-apple-darwin`.
     * This will default to clang on mac and gcc on x86_64 linux */
    char *CC;
    
    /* Target triple */
    enum CliTarget target;

    /* Install prefix (`--install-dir`, defaults to INSTALL_PREFIX).
     * Headers live at <install_dir>/include, libraries at
     * <install_dir>/lib - the JIT probes the latter for libtos. */
    char *install_dir;

    /* Are we compiling position independent code? */
    int is_pic;

    /* .so files that may have been passed on the commandline or recorded
     * by a `#link "<path>"` directive, these should be usable with the
     * JIT */
    List *shared_object_files;

    /* .o files that may have been passed on the commandline */
    List *object_files;

    /* Library names recorded by `#link <name>` directives. AOT passes
     * each as `-l<name>` to the linker; the JIT probes
     * lib<name>.{dylib,so} with dlopen. */
    List *link_libs;

    /* Symbols that are RAW assembly labels rather than C identifiers:
     * `_extern _MALLOC ...` bindings and labels defined by an `asm { }`
     * block. The platform's leading-underscore mangling must not be
     * applied to these - they are already the final symbol - whereas
     * every C identifier needs it, including the `_`-prefixed ones that
     * fill the system headers (`__swbuf`, `__sputc`). Provenance is the
     * only reliable way to tell the two apart; the spelling is not. */
    Set *raw_asm_labels;
    /* `__asm("sym")` renames and `@[spangle]` bindings (see asm.c).
     * Nothing in the HolyC front end populates these yet, so they stay
     * NULL and every lookup falls through to the ordinary spelling. */
    Map *asm_renames;
    Map *asm_spangled;

    Set *extern_globals;
} Cctrl;

/* Instantiate a new compiler control struct */
Cctrl *cctrlNew(enum CliTarget target);
/* Free a Cctrl and the containers it owns. For a caller that discards
 * a whole compiler rather than exiting - see the definition for what
 * it deliberately leaves alone, and for the ordering it needs against
 * memoryRecycleArenas. */
void cctrlRelease(Cctrl *cc);
/* Slimmed down Cctrl, for expanding macros */
Cctrl *ccMacroProcessor(Map *macro_defs);
/* File-id registry over cc->file_map; ids are what Ast->file_id holds. */
u32 cctrlRegisterFile(Cctrl *cc, AoStr *filename);
AoStr *cctrlLookUpFile(Cctrl *cc, u32 file_id);

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
void cctrlWarningAt(Cctrl *cc, s64 lineno, s64 col, s64 len, char *fmt, ...);
void cctrlWarningFromTo(Cctrl *cc, char *suggestion, char from, char to, char *fmt, ...);
__noreturn void cctrlRaiseExceptionFromTo(Cctrl *cc, char *suggestion, char from, char to, char *fmt, ...);
__noreturn void cctrlRaiseException(Cctrl *cc, char *fmt, ...);
__noreturn void cctrlRaiseSuggestion(Cctrl *cc, char *suggestion, char *fmt, ...);
__noreturn void cctrlIce(Cctrl *cc, char *fmt, ...);
Ast *cctrlGetOrSetString(Cctrl *cc, char *str, int len, s64 real_len);
void cctrlRewindUntilPunctMatch(Cctrl *cc, s64 ch, int *_count);
void cctrlRewindUntilStrMatch(Cctrl *cc, char *str, int len, int *_count);
AoStr *cctrlMessagePrintF(Cctrl *cc, int severity, char *fmt,...);
/* va_list variant of cctrlMessagePrintF. Public so subsystems
 * outside cctrl.c (the lexer) can build pre-rendered diagnostic
 * messages without re-implementing the printf-+-position dance. */
AoStr *cctrlMessagVnsPrintF(Cctrl *cc, char *fmt, va_list ap, int severity);

/* Build a fully-rendered diagnostic at an explicit (line, col, len).
 * Bypasses cctrlTokenPeek so callers that know their own position
 * (the lexer, mid-token) aren't subject to whatever stale token the
 * parser's ring buffer is holding. `col` is 1-based; pass 0 to skip
 * the underline entirely. */
AoStr *cctrlCreateErrorLineAt(Cctrl *cc, s64 lineno, s64 col, s64 len,
                              char *msg, int severity, char *suggestion);

/* Diagnostic accumulation. `cctrlDiagPush` adds an entry without
 * printing it */
void cctrlDiagPush(Cctrl *cc, CctrlDiagnostic *d);
/* prints every queued diagnostic to stderr (in source order) and returns the
 * number of CCTRL_ERROR entries. */
int cctrlDiagFlush(Cctrl *cc);
/* Drop queued diagnostics and reset the error count (REPL: between inputs). */
void cctrlDiagClear(Cctrl *cc);
/* Empty the token ring buffer so a fresh parse doesn't consume tokens
 * left over from a previous (possibly errored) one. */
void cctrlResetTokenBuffer(Cctrl *cc);
/* Drop the ring buffer's cached filler Lexeme; it lives in the lexeme
 * arena and must not survive a memoryRecycleArenas. */
void cctrlForgetTokenFiller(void);
/* Empty the lists a parse appends its output to (ast_list, initialisers,
 * asm blocks). For a Cctrl that gets re-parsed instead of code
 * generated - see the definition. */
void cctrlResetParseAccumulators(Cctrl *cc);

/* Build a CctrlDiagnostic from a pre-rendered message and the current source
 * position. Takes ownership of `rendered` and `suggestion`. */
CctrlDiagnostic *cctrlMakeDiag(Cctrl *cc,
                               int severity,
                               AoStr *rendered,
                               AoStr *suggestion);

/* Hand off to the nearest active recovery point (longjmp) if one is set.
 * Otherwise flush queued diagnostics and exit. */
__noreturn void cctrlTerminate(Cctrl *cc);

/* Walk the token ring back to a token on a strictly earlier line than
 * `tok->line`, render a CCTRL_INFO message positioned there. */
AoStr *cctrlInfoAtPreviousLine(Cctrl *cc, Lexeme *tok, const char *fmt, ...);

/* Token-stream resync after an error. Both skip tokens until they
 * reach a brace-depth-zero boundary; the caller's recovery loop
 * then resumes from there.
 *  - cctrlSyncToplevel: consume up to and including a top-level
 *    `;` or stop right before a `}` (which would close a top-level
 *    block) or a token that looks like the start of a new top-level
 *    decl (keyword / `#`).
 *  - cctrlSyncStatement: consume up to and including a statement
 *    `;` or stop right before a `}` (lets the enclosing block
 *    close cleanly). */
void cctrlSyncToplevel(Cctrl *cc);
void cctrlSyncStatement(Cctrl *cc);

Map *cctrlCreateAstMap(Cctrl *cc);
/* Does this translation unit own the program entry point - a `Main`
 * definition or file-scope statements? Only that unit may define the
 * `argc`/`argv` globals in an object file; every other unit leaves
 * them undefined for the linker to bind. */
int cctrlUnitHasEntryPoint(Cctrl *cc);
void cctrlReleaseScopeMaps(Cctrl *cc);
Map *cctrlCreateLexemeMap(void);
void cctrlAddDefine(Cctrl *cc, char *name);

#endif // !CCTRL_H
