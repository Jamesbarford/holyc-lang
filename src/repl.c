/* Interactive HolyC REPL.
 *
 * The trick that makes this cheap: the front end is already
 * incremental. One long-lived Cctrl accumulates types, functions,
 * globals and macros across parses, and the JIT's chunk API
 * (hccJitCompileChunk) compiles only the ASTs appended since the last
 * chunk into a fresh RX mapping, resolving names from earlier chunks
 * through the persistent symbol map.
 *
 * Per input:
 *   1. read lines until braces/brackets/parens balance and the input
 *      is terminated (`;`, `}`, a directive, or a blank line)
 *   2. lex + parse into the shared Cctrl (errors recover, REPL lives on)
 *   3. wrap this round's top-level statements into `__repl_N`,
 *      returning the trailing expression's value if it has one
 *   4. compile the chunk, call `__repl_N`, echo the result
 *
 * TempleOS niceties fall out of the front end for free:
 *   "Hello\n";            - naked strings print
 *   F("%d\n", x); F(x);   - functions with default args, etc.
 *
 * Memory is semi-managed through the "memsafe" helpers. This makes it easier
 * to report common errors.
 */

#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <unistd.h>
#if defined(__APPLE__) && (defined(__aarch64__) || defined(__arm64__))
#include <pthread.h> /* pthread_jit_write_protect_np */
#endif

#if defined(__linux__) && defined(__x86_64__)
#define REG_RIP 16
#define REG_RBP 10
#endif 

#include "linenoise/linenoise.h"

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "cli.h"
#include "lexer.h"
#include "list.h"
#include "memory.h"
#include "parser.h"
#include "repl.h"
#include "util.h"
#include "version.h"

#include "jit-common.h"
#include "memsafe.h"
#if defined(__aarch64__) || defined(__arm64__)
#include "aarch64-jit.h"
#include "asm/dis_arm64.h"
#elif defined(__x86_64__)
#include "x86_64-jit.h"
#include "asm/dis_x86_64.h"
#endif

#define REPL_PROMPT      ">>> "
#define REPL_PROMPT_CONT "..> "
#define REPL_HISTORY_MAX 1000

/* linenoise callbacks carry no user-data pointer, so the completion
 * callback reaches the compiler state through these. repl_jit is for
 * the `Uf` builtin, which JIT'd code calls back into. */
static Cctrl *repl_cc = NULL;
static HccJit *repl_jit = NULL;
static char repl_history_path[512];
static char *repl_prompt = REPL_PROMPT;
static char *repl_prompt_cont = REPL_PROMPT_CONT;
static char *repl_root_dir = NULL;

/* -------------- Forward definitions ------------------------- */

static AoStr *replGetRcPath(void);
static void replDropInitialisers(Cctrl *cc);
static int replParse(Cctrl *cc, char *root_dir, char *name, AoStr *src);
static Ast *replBuildWrapper(Cctrl *cc, int round, AstType **echo_type);
static void replExec(AoStr *input, int round);

/* -------------- Forward definitions END --------------------- */

static const HccJitBackend *replHostBackend(void) {
#if defined(__aarch64__) || defined(__arm64__)
    return aarch64JitBackend();
#elif defined(__x86_64__)
    return x86_64JitBackend();
#else
    return NULL;
#endif
}

/* ---------------- fault recovery ----------------
 *
 * JIT'd code runs in-process, so `U8 *p = 0; *p = 1;` would take the
 * whole session down with it - history, definitions, everything.
 * Instead the hardware fault is caught, reported with a disassembly
 * window around the faulting pc, and control siglongjmps back to the
 * prompt. TempleOS dropped you into its debugger on a fault; we drop
 * you back at the prompt.
 *
 * Recovery is best-effort by nature: if the fault fired while a libc
 * lock was held (mid-malloc, say) the session may wedge later. That
 * trade is right for a playground. Faults OUTSIDE an eval keep the
 * default disposition - a crash in hcc itself dies loudly. */

static sigjmp_buf repl_fault_env;
static volatile sig_atomic_t repl_in_eval = 0;
static volatile int repl_fault_sig = 0;
static volatile uintptr_t repl_fault_pc = 0;
static volatile uintptr_t repl_fault_addr = 0;
static volatile uintptr_t repl_fault_fp = 0;
static volatile uintptr_t repl_fault_lr = 0;
static volatile uintptr_t repl_fault_frame = 0;

/* The handler's frame-pointer walk (replFaultFindJitFrame) reads
 * saved-fp/return-address pairs off a stack we no longer trust; if a
 * read lands on an unmapped page, the nested fault delivery bails the
 * walk out through here instead of killing the session. */
static sigjmp_buf repl_walk_env;
static volatile sig_atomic_t repl_in_walk = 0;

/* A stack overflow in JIT'd code exhausts the main stack, so the
 * handler needs somewhere else to run (SA_ONSTACK). Static, so
 * recovery never depends on a working malloc. */
static uint8_t repl_fault_stack[64 * 1024];

static uintptr_t replFaultPcOf(void *uc_) {
    ucontext_t *uc = (ucontext_t *)uc_;
#if defined(__APPLE__) && (defined(__aarch64__) || defined(__arm64__))
    return (uintptr_t)uc->uc_mcontext->__ss.__pc;
#elif defined(__APPLE__) && defined(__x86_64__)
    return (uintptr_t)uc->uc_mcontext->__ss.__rip;
#elif defined(__linux__) && defined(__aarch64__)
    return (uintptr_t)uc->uc_mcontext.pc;
#elif defined(__linux__) && defined(__x86_64__)
    return (uintptr_t)uc->uc_mcontext.gregs[REG_RIP];
#else
    (void)uc;
    return 0;
#endif
}

static uintptr_t replFaultFpOf(void *uc_) {
    ucontext_t *uc = (ucontext_t *)uc_;
#if defined(__APPLE__) && (defined(__aarch64__) || defined(__arm64__))
    return (uintptr_t)uc->uc_mcontext->__ss.__fp;
#elif defined(__APPLE__) && defined(__x86_64__)
    return (uintptr_t)uc->uc_mcontext->__ss.__rbp;
#elif defined(__linux__) && defined(__aarch64__)
    return (uintptr_t)uc->uc_mcontext.regs[29];
#elif defined(__linux__) && defined(__x86_64__)
    return (uintptr_t)uc->uc_mcontext.gregs[REG_RBP];
#else
    (void)uc;
    return 0;
#endif
}

/* Link register (arm64 only): a fault in a frameless libc leaf keeps
 * its caller here rather than in a stack frame record. */
static uintptr_t replFaultLrOf(void *uc_) {
    ucontext_t *uc = (ucontext_t *)uc_;
#if defined(__APPLE__) && (defined(__aarch64__) || defined(__arm64__))
    return (uintptr_t)uc->uc_mcontext->__ss.__lr;
#elif defined(__linux__) && defined(__aarch64__)
    return (uintptr_t)uc->uc_mcontext.regs[30];
#else
    (void)uc;
    return 0;
#endif
}

static uintptr_t replFaultFindJitFrame(HccJit *jit, uintptr_t fp,
                                       uintptr_t lr);

static void replFaultHandler(int sig, siginfo_t *si, void *uc) {
    if (repl_in_walk) {
        /* The frame walk below hit unmapped stack memory - abandon
         * the walk, not the report. */
        repl_in_walk = 0;
        siglongjmp(repl_walk_env, 1);
    }
    if (!repl_in_eval) {
        /* Not our eval: restore the default action and return - the
         * faulting instruction re-executes and the process dies with
         * a normal crash report. */
        signal(sig, SIG_DFL);
        return;
    }
    repl_fault_sig  = sig;
    repl_fault_addr = (uintptr_t)si->si_addr;
    repl_fault_pc   = replFaultPcOf(uc);
    repl_fault_fp   = replFaultFpOf(uc);
    repl_fault_lr   = replFaultLrOf(uc);

    /* Attribute a host-code fault (abort() from a double free, a crash
     * deep in libc) to the JIT frame that called out. This MUST happen
     * here, on the alternate stack: after the siglongjmp the reporter's
     * own calls recycle the main stack and overwrite the very frame
     * records we need to read. */
    repl_fault_frame = 0;
    if (repl_jit &&
        hccJitFindChunk(repl_jit, (void *)repl_fault_pc) == NULL)
    {
        repl_fault_frame = replFaultFindJitFrame(repl_jit, repl_fault_fp,
                                                 repl_fault_lr);
    }

    repl_in_eval = 0;
    siglongjmp(repl_fault_env, 1);
}

static void replInstallFaultHandlers(void) {
    stack_t ss;
    memset(&ss, 0, sizeof(ss));
    ss.ss_sp = repl_fault_stack;
    ss.ss_size = sizeof(repl_fault_stack);
    sigaltstack(&ss, NULL);

    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_sigaction = replFaultHandler;
    sa.sa_flags = SA_SIGINFO | SA_ONSTACK;
    sigemptyset(&sa.sa_mask);
    /* SIGTRAP: macOS malloc reports some heap corruption via a brk
     * trap (__os_crash) rather than abort(). */
    static const int sigs[] = { SIGSEGV, SIGBUS, SIGILL, SIGABRT, SIGFPE,
                                SIGTRAP };
    for (size_t i = 0; i < sizeof(sigs) / sizeof(sigs[0]); ++i)
        sigaction(sigs[i], &sa, NULL);
}

static const char *replSigName(int sig) {
    switch (sig) {
        case SIGSEGV: return "SIGSEGV";
        case SIGBUS:  return "SIGBUS";
        case SIGILL:  return "SIGILL";
        case SIGFPE:  return "SIGFPE";
        case SIGABRT: return "SIGABRT";
        case SIGTRAP: return "SIGTRAP";
        default:      return "signal";
    }
}

/* A fault whose pc is in host code (a double free aborting inside
 * libSystem, say) still has JIT frames further up the stack - abort
 * and friends preserve frame pointers, and so does JIT'd HolyC.
 * Walk the saved-fp/return-address chain until a return address
 * lands in a JIT chunk; that's the HolyC frame that made the call.
 * Frame records: [fp] = caller's fp, [fp+8] = return address, on
 * both AAPCS64 (x29) and SysV x86-64 (rbp).
 *
 * The stack may be arbitrarily damaged, so every step is checked
 * (alignment, growth toward the stack base, a depth cap) and the
 * loads run under repl_in_walk so a fault bails out via
 * repl_walk_env instead of killing the report. */
static uintptr_t replFaultFindJitFrame(HccJit *jit, uintptr_t fp,
                                       uintptr_t lr) {
    if (jit == NULL) return 0;
    if (lr && hccJitFindChunk(jit, (void *)lr)) return lr;

    uintptr_t found = 0;
    if (sigsetjmp(repl_walk_env, 1) == 0) {
        repl_in_walk = 1;
        uintptr_t prev = 0;
        for (int depth = 0; depth < 64 && fp; ++depth) {
            if (fp & 7) break;              /* torn frame record */
            if (prev && fp <= prev) break;  /* must move toward base */
            uintptr_t ret = ((uintptr_t *)fp)[1];
            uintptr_t next = ((uintptr_t *)fp)[0];
            if (ret && hccJitFindChunk(jit, (void *)ret)) {
                found = ret;
                break;
            }
            prev = fp;
            fp = next;
        }
    }
    repl_in_walk = 0;
    return found;
}

static void replReportFault(void) {
#if defined(__APPLE__) && (defined(__aarch64__) || defined(__arm64__))
    /* If the fault hit while a finalize had the MAP_JIT region
     * writable, put this thread back in execute mode. */
    pthread_jit_write_protect_np(1);
#endif

    uint8_t *pc = (uint8_t *)repl_fault_pc;
    fprintf(stderr, "\n*** %s: fault address %p, pc %p",
            replSigName(repl_fault_sig),
            (void *)repl_fault_addr,
            (void *)pc);

    HccJit *jit = repl_jit;
    AsmJitCode *chunk = jit ? hccJitFindChunk(jit, pc) : NULL;
    size_t off = 0;
    const char *sym = jit ? hccJitFindSymbolForAddr(jit, pc, &off) : NULL;
    uint8_t *show = pc; /* where the disassembly window points */

    if      (sym)   fprintf(stderr, " in `%s`+%zu\n", sym, off);
    else if (chunk) fprintf(stderr, " in a call veneer\n");
    else {
        /* pc is host/libc code - the handler already walked the frame
         * chain (while the faulting frames were still intact) to find
         * the HolyC frame that called out. */
        uintptr_t frame = repl_fault_frame;
        if (frame) {
            chunk = hccJitFindChunk(jit, (void *)frame);
            sym = hccJitFindSymbolForAddr(jit, (void *)frame, &off);
            show = (uint8_t *)frame;
            fprintf(stderr, " in host code,\n    called from ");
            if (sym) fprintf(stderr, "`%s`+%zu\n", sym, off);
            else     fprintf(stderr, "%p in JIT code\n", (void *)frame);
        } else {
            fprintf(stderr, " (no JIT frame on the stack - possibly an hcc bug)\n");
        }
    }

    /* Disassemble around the interesting pc - only when it's inside a
     * JIT mapping, so the window reads live, readable bytes. For a
     * walked frame `show` is the return address: the call itself is
     * the line just above it. */
    if (chunk) {
        uint8_t *base = (uint8_t *)chunk->code;
        uint8_t *live_end = base + chunk->size + chunk->veneer_size;
        uint8_t *lo = show, *hi = show + 20;

#if defined(__aarch64__) || defined(__arm64__)
        lo = show - 16; /* fixed-width insns: backing up is safe */
#endif

        if (lo < base) lo = base;
        if (hi > live_end) hi = live_end;
#if defined(__aarch64__) || defined(__arm64__)
        aarch64_disasm_buf_at(lo, (size_t)(hi - lo),
                              (uint64_t)(uintptr_t)lo, stderr);
#elif defined(__x86_64__)
        x86_64_disasm_buf_at(lo, (size_t)(hi - lo),
                             (uint64_t)(uintptr_t)lo, stderr);
#endif
    }
    fprintf(stderr, "recovered; session state is best-effort from here\n");
    fflush(stderr);
}

/* `ReplHeap;` - live allocations, biggest first. The tracker itself
 * lives in memsafe.c; replRun installs it via memsafeInit. */
static void replBuiltinReplHeap(void) {
    memsafeDumpLive(stdout);
}

/* ---------------- Uf: the unassembler builtin ----------------
 *
 * `Uf("Fib");` prints the disassembly of a JIT-compiled function,
 * TempleOS's "unassemble function", pointed at libtasm's disassembler. It's a
 * host C function registered in the jit's symbol table, so compiled HolyC
 * calls straight back into the compiler. */

static void replBuiltinUf(char *name) {
    HccJit *jit = repl_jit;
    if (name == NULL || jit == NULL) return;

    uint8_t *addr = (uint8_t *)hccJitLookup(jit, name);
    if (addr == NULL) {
        printf("Uf: `%s` is not a JIT-compiled function in this session\n",
               name);
        return;
    }

    /* The containing chunk's mapping bounds the code hard (chunk size
     * excludes the veneer area)... */
    uint8_t *end = NULL;
    AsmJitCode *owner = NULL;
    listForEach(jit->chunks) {
        AsmJitCode *chunk = (AsmJitCode *)it->value;
        uint8_t *base = (uint8_t *)chunk->code;
        if (addr >= base && addr < base + chunk->size) {
            end = base + chunk->size;
            owner = chunk;
            break;
        }
    }
    if (end == NULL) {
        printf("Uf: cannot locate the code chunk for `%s`\n", name);
        return;
    }

    /* ...tightened to the next function emitted in the same chunk. */
    MapIter mi;
    mapIterInit(jit->symbols, &mi);
    while (mapIterNext(&mi)) {
        uint8_t *sym = (uint8_t *)mi.node->value;
        if (sym > addr && sym < end) end = sym;
    }

    printf("%s @ %p, %zu bytes:\n", name, (void *)addr,
           (size_t)(end - addr));
#if defined(__aarch64__) || defined(__arm64__)
    aarch64_disasm_buf_at(addr, (size_t)(end - addr), (uint64_t)(uintptr_t)addr,
                          stdout);
#elif defined(__x86_64__)
    x86_64_disasm_buf_at(addr, (size_t)(end - addr), (uint64_t)(uintptr_t)addr,
                         stdout);
#endif

    /* Out-of-range calls (e.g. into libSystem) go through movz/movk
     * trampolines parked after the chunk's code - show them so a `bl`
     * past the function's end has a visible target. Shared by every
     * function in the chunk, hence the separate heading. */
    if (owner->veneer_size > 0) {
        uint8_t *vbase = (uint8_t *)owner->code + owner->size;
        printf("veneers @ %p, %zu bytes:\n", (void *)vbase,
               owner->veneer_size);
#if defined(__aarch64__) || defined(__arm64__)
        aarch64_disasm_buf_at(vbase, owner->veneer_size,
                              (uint64_t)(uintptr_t)vbase, stdout);
#elif defined(__x86_64__)
        x86_64_disasm_buf_at(vbase, owner->veneer_size,
                             (uint64_t)(uintptr_t)vbase, stdout);
#endif
    }
    fflush(stdout);
}

static void replEvalString(char *code) {
    if (!code) return;
    u64 len = strlen(code);
    AoStr b = {
        .data = code,
        .len = len,
        .capacity = 0,
    };
    replExec(&b, INT_MAX);
}

/* --------- ReplReloadRc: Refresh the `~/.hcc_rc.HC` ----------
 * Reinclude the `~/.hcc_rc.HC` this does not however delete the previous
 * function or `#define` definitions, so it won't delete stuff. But it will
 * re-define already existing symbols
 */
static void replBuiltinReloadRc(void) {
    HccJit *jit = repl_jit;
    if (jit == NULL) return;
    Cctrl *cc = jit->cc;

    AoStr *hcc_rc = replGetRcPath();
    if (!hcc_rc) return;

    Lexer *l = (Lexer *)malloc(sizeof(Lexer));
    lexInit(l, NULL, CCF_PRE_PROC);
    lexSetBuiltinRoot(l, repl_root_dir);
    lexPushFile(l, hcc_rc);
    cctrlDiagClear(cc);
    cctrlResetTokenBuffer(cc);
    cctrlInitParse(cc, l);
    parseToAst(cc);

    if (cctrlDiagFlush(cc) > 0) {
        fprintf(stderr, "ReplReloadRc: errors\n");
    }

    cctrlDiagClear(cc);
    replDropInitialisers(cc);

    if (hccJitCompileChunk(jit, NULL) != 0) {
        fprintf(stderr, "repl: warning: failed to compile rc file\n");
    }

    lexerRelease(l);
}

/* --------- ReplListFn: List all defined functions ---------- */
static void replBuiltinListFn(void) {
    HccJit *jit = repl_jit;
    if (jit == NULL) return;
    Cctrl *cc = jit->cc;
    MapIter it;
    mapIterInit(cc->global_env, &it);
    while (mapIterNext(&it)) {
        Ast *entry = it.node->value;
        if (astIsFnLike(entry)) {
            /* Can't free the string as it belongs to the AoStr pool... We
             * need a way to be able to detach buffers from AoStr and free
             * the underlying buffer as we leak everywhere. */
            char *fn_str = astFunctionToString(entry);
            fprintf(stderr, "%s\n", fn_str);
        }
    }
}


/* --------- ReplFindFn: Find a function -------------------------------- */
static void replBuiltinFindFn(char *name) {
    HccJit *jit = repl_jit;
    if (jit == NULL) return;
    Cctrl *cc = jit->cc;
    MapIter it;
    mapIterInit(cc->global_env, &it);
    Vec *vec = vecNew(&vec_cstring_type);
    while (mapIterNext(&it)) {
        Ast *entry = it.node->value;
        if (astIsFnLike(entry) && entry->fname && strstr(entry->fname->data, name)) {
            char *fn_str = astFunctionToString(entry);
            vecPush(vec, fn_str);
        }
    }
    for (u64 i = 0; i < vec->size; ++i) {
        char *fn = (char *)vec->entries[i];
        fprintf(stderr, "%s\n",fn);
    }
    vecRelease(vec);
}

/* --------- ReplListGlobal: List all defined global variables ---------- */
static void replBuiltinListGVars(void) {
    HccJit *jit = repl_jit;
    if (jit == NULL) return;
    Cctrl *cc = jit->cc;
    MapIter it;
    mapIterInit(cc->global_env, &it);
    while (mapIterNext(&it)) {
        Ast *entry = it.node->value;
        if (entry->kind == AST_GVAR) {
            AoStr *gbl_str = astLValueToAoStr(entry,0);
            AoStr *gbl_type = astTypeToColorAoStr(entry->type);
            if (astTypeIsPtr(entry->type)) {
                fprintf(stderr, "%s%s\n", gbl_type->data, gbl_str->data);
            } else {
                fprintf(stderr, "%s %s\n", gbl_type->data, gbl_str->data);
            }
            aoStrRelease(gbl_str);
            aoStrRelease(gbl_type);
        }
    }
}

/* ---------------- ReplClassRep: Print a class or union --------- */
static void replBuiltinPrintClass(char *name) {
    HccJit *jit = repl_jit;
    if (name == NULL || *name == '\0' || jit == NULL) return;
    Cctrl *cc = jit->cc;
    s64 len = (s64)strlen(name);

    AstType *cls = mapGetLen(cc->clsdefs, name, len);
    if (cls) {
        AoStr *cls_str = astClassToAoStr(cls);
        fprintf(stderr, "%s\n",cls_str->data);
        aoStrRelease(cls_str);
    }
}

/* ---------------- ReplDel: drop a definition ----------------
 *
 * `ReplDel("name");` removes a global variable, function, class,
 * union or #define from the session so the name can be redefined
 * from scratch. Already-compiled code that captured the old
 * definition keeps working - this only frees the NAME (and, for
 * globals, the JIT's storage binding so a redefinition gets a fresh
 * slot instead of aliasing the old one at a possibly different
 * size). */
static void replBuiltinReplDel(char *name) {
    HccJit *jit = repl_jit;
    if (name == NULL || *name == '\0' || jit == NULL) return;
    Cctrl *cc = jit->cc;
    s64 len = (s64)strlen(name);
    int removed = 0;

    struct { Map *m; const char *what; } tables[] = {
        { cc->global_env, "global" },
        { cc->clsdefs,    "class" },
        { cc->uniondefs,  "union" },
        { cc->macro_defs, "#define" },
        { cc->asm_funcs,  "asm function" },
    };
    for (size_t i = 0; i < sizeof(tables)/sizeof(tables[0]); ++i) {
        if (tables[i].m && mapGetLen(tables[i].m, name, len)) {
            mapRemove(tables[i].m, name);
            printf("ReplDel: removed %s `%s`\n", tables[i].what, name);
            removed = 1;
        }
    }
    if (!removed) {
        printf("ReplDel: nothing named `%s`\n", name);
        return;
    }

    /* Unbind the JIT's view under both the raw and the Mach-O-mangled
     * spelling. Old storage/code is intentionally leaked - compiled
     * chunks may still reference it. */
    char mangled[256];
    snprintf(mangled, sizeof(mangled), "_%s", name);
    if (jit->host_symbols) {
        mapRemove(jit->host_symbols, name);
        mapRemove(jit->host_symbols, mangled);
    }
    if (jit->symbols) {
        mapRemove(jit->symbols, name);
        mapRemove(jit->symbols, mangled);
    }
}

/* Prototypes for the REPL's host builtins, parsed at startup so calls
 * type-check like any other function (and show up in completion and
 * hints). The implementations are registered via hccJitDefineSymbol. */
static const char repl_builtin_protos[] =
    "extern \"c\" U0 Uf(U8 *fname);\n"
    "extern \"c\" U0 ReplReloadRc();\n"
    "extern \"c\" U0 ReplListFn();\n"
    "extern \"c\" U0 ReplFindFn(U8 *name);\n"
    "extern \"c\" U0 ReplListGVars();\n"
    "extern \"c\" U0 ReplClassRep(U8 *name);\n"
    "extern \"c\" U0 ReplEval(U8 *code);\n"
    "extern \"c\" U0 ReplDel(U8 *name);\n"
    "extern \"c\" U0 ReplHeap();\n";

/* ---------------- tab completion ---------------- */

#define REPL_MAX_CANDIDATES 512

static void replCollectFromMap(Map *m, const char *prefix, size_t plen,
                               const char **cands, int *n) {
    if (!m) return;
    MapIter mi;
    mapIterInit(m, &mi);
    while (mapIterNext(&mi) && *n < REPL_MAX_CANDIDATES) {
        const char *key = (const char *)mi.node->key;
        if (!key || strncmp(key, prefix, plen) != 0) continue;
        cands[(*n)++] = key;
    }
}

static int replCandidateCmp(const void *a, const void *b) {
    return strcmp(*(const char *const *)a, *(const char *const *)b);
}

/* Complete the identifier being typed at the end of `line` against
 * everything the session knows: functions and globals (global_env),
 * class/union names, built-in type keywords and #defines. linenoise
 * wants each completion as the WHOLE replacement line, so the part of
 * the line before the identifier is carried over verbatim. */
static void replCompletion(const char *line, linenoiseCompletions *lc) {
    Cctrl *cc = repl_cc;
    if (!cc) return;

    size_t len = strlen(line);
    size_t start = len;
    while (start > 0) {
        char c = line[start - 1];
        if (isalnum((unsigned char)c) || c == '_') start--;
        else break;
    }
    if (start == len) return;                        /* nothing to complete */
    if (isdigit((unsigned char)line[start])) return; /* a number literal */

    const char *prefix = line + start;
    size_t plen = len - start;

    const char *cands[REPL_MAX_CANDIDATES];
    int n = 0;
    replCollectFromMap(cc->global_env, prefix, plen, cands, &n);
    /* Asm-bound functions (MAlloc, Free, ... - `_extern _NAME` in
     * tos.HH) live in their own map, not global_env. */
    replCollectFromMap(cc->asm_funcs, prefix, plen, cands, &n);
    replCollectFromMap(cc->clsdefs, prefix, plen, cands, &n);
    replCollectFromMap(cc->uniondefs, prefix, plen, cands, &n);
    replCollectFromMap(cc->symbol_table, prefix, plen, cands, &n);
    replCollectFromMap(cc->macro_defs, prefix, plen, cands, &n);
    if (n == 0) return;

    qsort(cands, n, sizeof(cands[0]), replCandidateCmp);

    char buf[1024];
    for (int i = 0; i < n; ++i) {
        if (i > 0 && strcmp(cands[i], cands[i-1]) == 0) continue; /* dedup */
        snprintf(buf, sizeof(buf), "%.*s%s", (int)start, line, cands[i]);
        linenoiseAddCompletion(lc, buf);
    }
}

/* ---------------- input reading ---------------- */

typedef struct ReplScan {
    int depth;      /* net paren/bracket/brace nesting */
    int open;      /* inside a string, char const or block comment */
    int pp_depth;   /* net #if* / #endif nesting */
} ReplScan;

/* Nesting scan that ignores delimiters inside strings, char consts
 * (HolyC allows multi-char 'ab'), // and block comments. Also counts
 * preprocessor conditional nesting (#if / #ifdef / #ifndef / #ifjit /
 * #ifaot vs #endif) so a multi-line conditional block is held open and
 * submitted as ONE input - split across inputs each fresh parse gets a
 * fresh lexer and the skip/collect state is lost. */
static void replScan(const char *s, size_t len, ReplScan *out) {
    int depth = 0;
    int pp_depth = 0;
    enum { CODE, STR, CHR, LINE_COMMENT, BLOCK_COMMENT } st = CODE;
    for (size_t i = 0; i < len; ++i) {
        char c = s[i];
        switch (st) {
            case STR:
                if (c == '\\') { if (i + 1 < len) i++; }
                else if (c == '"') st = CODE;
                break;
            case CHR:
                if (c == '\\') { if (i + 1 < len) i++; }
                else if (c == '\'') st = CODE;
                break;
            case LINE_COMMENT:
                if (c == '\n') st = CODE;
                break;
            case BLOCK_COMMENT:
                if (c == '*' && i + 1 < len && s[i+1] == '/') { i++; st = CODE; }
                break;
            case CODE:
                if (c == '"') st = STR;
                else if (c == '\'') st = CHR;
                else if (c == '/' && i + 1 < len && s[i+1] == '/') { i++; st = LINE_COMMENT; }
                else if (c == '/' && i + 1 < len && s[i+1] == '*') { i++; st = BLOCK_COMMENT; }
                else if (c == '(' || c == '[' || c == '{') depth++;
                else if (c == ')' || c == ']' || c == '}') depth--;
                else if (c == '#') {
                    size_t j = i + 1;
                    while (j < len && (s[j] == ' ' || s[j] == '\t')) j++;
                    size_t k = j;
                    while (k < len && isalpha((unsigned char)s[k])) k++;
                    /* "if" prefix covers if/ifdef/ifndef/ifjit/ifaot;
                     * #include starts "in" so it doesn't match. */
                    if (k - j >= 2 && s[j] == 'i' && s[j+1] == 'f') {
                        pp_depth++;
                    } else if (k - j == 5 && !strncmp(s + j, "endif", 5)) {
                        pp_depth--;
                    }
                }
                break;
        }
    }
    out->depth = depth;
    out->open = (st == STR || st == CHR || st == BLOCK_COMMENT);
    out->pp_depth = pp_depth;
}

static int replLineIsBlank(const char *s) {
    for (; *s; ++s) {
        if (!isspace((unsigned char)*s)) return 0;
    }
    return 1;
}

/* Last non-whitespace char of the buffer, or '\0' if it's all space. */
static char replLastChar(AoStr *buf) {
    for (s64 i = (s64)buf->len - 1; i >= 0; --i) {
        if (!isspace((unsigned char)buf->data[i])) return buf->data[i];
    }
    return '\0';
}

static char replFirstChar(AoStr *buf) {
    for (u64 i = 0; i < buf->len; ++i) {
        if (!isspace((unsigned char)buf->data[i])) return buf->data[i];
    }
    return '\0';
}

static int replStartsWithShellEscape(AoStr *buf) {
    return buf->len >= 3 && buf->data[0] == '@' && buf->data[1] == ' ';
}

/* ---------------- live syntax highlighting & hints ----------------
 *
 * The vendored linenoise is used as-is; it has no display hooks. What
 * it does have is the multiplexed editing API, which exposes the whole
 * edit state (buffer, cursor, columns). So we drive the feed loop
 * ourselves and, after every keystroke, repaint the line over the one
 * linenoise just drew - same glyphs in the same cells, plus zero-width
 * SGR colour codes, so the cursor maths of both renderers agree. */

#define C_KEYWORD "\x1b[1;35m"  /* control keywords - bold magenta  */
#define C_TYPE    "\x1b[33m"    /* built-in + class types - yellow  */
#define C_STRING  "\x1b[32m"    /* strings / char consts - green    */
#define C_NUMBER  "\x1b[36m"    /* numeric literals - cyan          */
#define C_COMMENT "\x1b[90m"    /* comments + hints - bright black  */
#define C_RESET   "\x1b[0m"

static int repl_use_colour = 0;

/* Control keywords, mirroring lexer.c's lexer_types[] (the type
 * keywords are looked up live in cc->symbol_table instead). */
static const char *repl_keywords[] = {
    "auto", "_extern", "extern", "asm", "switch", "case", "break",
    "continue", "while", "do", "for", "goto", "default", "return",
    "try", "catch", "throw", "reg", "noreg", "if", "else", "sizeof",
    "alignof", "typeof", "inline", "atomic", "volatile", "public", "private",
    "class", "union", "static",
};

static int replIsKeyword(const char *s, size_t len) {
    for (size_t i = 0; i < sizeof(repl_keywords)/sizeof(repl_keywords[0]); ++i) {
        if (strlen(repl_keywords[i]) == len &&
            memcmp(repl_keywords[i], s, len) == 0) return 1;
    }
    return 0;
}

/* Built-in type keywords (I64, U0, ...) and user-defined classes and
 * unions - both live on the persistent Cctrl. */
static int replIsTypeName(Cctrl *cc, char *s, size_t len) {
    if (mapGetLen(cc->symbol_table, s, len)) return 1;
    if (mapGetLen(cc->clsdefs, s, len)) return 1;
    if (mapGetLen(cc->uniondefs, s, len)) return 1;
    return 0;
}

static int replIsRecognisedConstant(Cctrl *cc, char *s, size_t len) {
    if (mapGetLen(cc->macro_defs, s, len)) return 1;
    return 0;
}

static int replIsIdentChar(char c) {
    return isalnum((unsigned char)c) || c == '_';
}

/* Append `s` colourised into `out`. Same token classes the lexer sees:
 * strings, char consts, comments, numbers, identifiers (split into
 * keyword / type / plain), `#directives`. */
static void replColourise(Cctrl *cc, char *s, size_t len, AoStr *out) {
    size_t i = 0;

    /* A leading #directive is keyword-coloured as one token. */
    while (i < len && isspace((unsigned char)s[i])) i++;
    if (i < len && s[i] == '#') {
        size_t end = i + 1;
        while (end < len && replIsIdentChar(s[end])) end++;
        aoStrCatLen(out, s, i);
        aoStrCatLen(out, C_KEYWORD, strlen(C_KEYWORD));
        aoStrCatLen(out, s + i, end - i);
        aoStrCatLen(out, C_RESET, strlen(C_RESET));
        i = end;
    } else {
        i = 0;
    }

    while (i < len) {
        char c = s[i];
        if (c == '"' || c == '\'') {
            char quote = c;
            size_t end = i + 1;
            while (end < len) {
                if (s[end] == '\\' && end + 1 < len) end += 2;
                else if (s[end] == quote) { end++; break; }
                else end++;
            }
            aoStrCatLen(out, C_STRING, strlen(C_STRING));
            aoStrCatLen(out, s + i, end - i);
            aoStrCatLen(out, C_RESET, strlen(C_RESET));
            i = end;
        } else if (c == '/' && i + 1 < len &&
                   (s[i+1] == '/' || s[i+1] == '*')) {
            size_t end;
            if (s[i+1] == '/') {
                end = len;
            } else {
                end = i + 2;
                while (end + 1 < len &&
                       !(s[end] == '*' && s[end+1] == '/')) end++;
                end = (end + 1 < len) ? end + 2 : len;
            }
            aoStrCatLen(out, C_COMMENT, strlen(C_COMMENT));
            aoStrCatLen(out, s + i, end - i);
            aoStrCatLen(out, C_RESET, strlen(C_RESET));
            i = end;
        } else if (isdigit((unsigned char)c)) {
            size_t end = i + 1;
            while (end < len &&
                   (isalnum((unsigned char)s[end]) || s[end] == '.')) end++;
            aoStrCatLen(out, C_NUMBER, strlen(C_NUMBER));
            aoStrCatLen(out, s + i, end - i);
            aoStrCatLen(out, C_RESET, strlen(C_RESET));
            i = end;
        } else if (replIsIdentChar(c) && !isdigit((unsigned char)c)) {
            size_t end = i + 1;
            while (end < len && replIsIdentChar(s[end])) end++;
            const char *colour = NULL;
            if      (replIsKeyword(s + i, end - i)) colour = C_KEYWORD;
            else if (replIsRecognisedConstant(cc, s + i, end - i)) colour = C_NUMBER;
            else if (replIsTypeName(cc, s + i, end - i)) colour = C_TYPE;
            if (colour) {
                aoStrCatLen(out, colour, strlen(colour));
                aoStrCatLen(out, s + i, end - i);
                aoStrCatLen(out, C_RESET, strlen(C_RESET));
            } else {
                aoStrCatLen(out, s + i, end - i);
            }
            i = end;
        } else {
            aoStrPutChar(out, c);
            i++;
        }
    }
}

/* If the line ends in the full name of a known function (and the `(`
 * hasn't been typed yet), return its parameter list - "(I64 x, ...)" -
 * to paint as a grey hint after the cursor. Cached per function name:
 * this runs on every keystroke and astTypeToString allocates. */
static const char *replHintFor(Cctrl *cc, const char *buf, size_t len) {
    static char cached_fn[128];
    static AoStr *cached_hint = NULL;

    size_t start = len;
    while (start > 0 && replIsIdentChar(buf[start-1])) start--;
    size_t ilen = len - start;
    if (ilen == 0 || ilen >= sizeof(cached_fn)) return NULL;
    if (isdigit((unsigned char)buf[start])) return NULL;

    if (strncmp(cached_fn, buf + start, ilen) != 0 ||
        cached_fn[ilen] != '\0')
    {
        snprintf(cached_fn, sizeof(cached_fn), "%.*s", (int)ilen,
                 buf + start);
        if (cached_hint) aoStrRelease(cached_hint);
        cached_hint = NULL;

        Ast *fn = (Ast *)mapGetLen(cc->global_env, (char *)buf + start, ilen);
        if (fn == NULL) fn = (Ast *)mapGetLen(cc->asm_funcs,
                                              (char *)buf + start, ilen);
        if (fn == NULL || fn->params == NULL) return NULL;
        if (fn->kind != AST_FUNC && fn->kind != AST_FUN_PROTO &&
            fn->kind != AST_EXTERN_FUNC && fn->kind != AST_ASM_FUNC_BIND)
        {
            return NULL;
        }

        AoStr *h = aoStrNew();
        aoStrPutChar(h, '(');
        for (u64 i = 0; i < fn->params->size; ++i) {
            Ast *p = vecGet(Ast *, fn->params, i);
            if (i > 0) aoStrCatLen(h, ", ", 2);
            if (p == NULL) continue;
            if (p->kind == AST_VAR_ARGS) {
                aoStrCatLen(h, "...", 3);
                continue;
            }
            Ast *var = (p->kind == AST_DEFAULT_PARAM) ? p->declvar : p;
            int glue = 0; /* type already ends in `*` (or a space) */
            if (var && var->type) {
                char *t = astTypeToString(var->type);
                size_t tlen = strlen(t);
                aoStrCatPrintf(h, "%s", t);
                glue = tlen > 0 && (t[tlen-1] == '*' || t[tlen-1] == ' ');
            }
            if (var && var->lname) {
                aoStrCatPrintf(h, glue ? "%s" : " %s", var->lname->data);
            }
            if (p->kind == AST_DEFAULT_PARAM) aoStrCatLen(h, "=..", 3);
        }
        aoStrPutChar(h, ')');
        cached_hint = h;
    }
    return cached_hint ? cached_hint->data : NULL;
}

/* Repaint the line linenoise just drew, with colours (and optionally
 * the grey signature hint). Zero-width SGR codes only, so the glyphs
 * land in exactly the cells linenoise put them in - no flicker, and
 * its cursor arithmetic stays valid. Bail out whenever our simple
 * one-row ASCII model doesn't hold (completion menu cycling, lines
 * that wrap, UTF-8) - the plain rendering underneath is still correct. */
static void replOverdraw(struct linenoiseState *ls, int with_hint) {
    if (!repl_use_colour || repl_cc == NULL) return;
    if (ls->in_completion) return;
    if (ls->plen + ls->len >= ls->cols) return;
    for (size_t i = 0; i < ls->len; ++i) {
        if ((unsigned char)ls->buf[i] >= 0x80) return;
    }

    AoStr *out = aoStrNew();
    aoStrPutChar(out, '\r');
    aoStrCatLen(out, ls->prompt, ls->plen);
    replColourise(repl_cc, ls->buf, ls->len, out);
    if (with_hint) {
        const char *hint = replHintFor(repl_cc, ls->buf, ls->len);
        if (hint &&
            ls->plen + ls->len + strlen(hint) < ls->cols) {
            aoStrCatLen(out, C_COMMENT, strlen(C_COMMENT));
            aoStrCatLen(out, hint, strlen(hint));
            aoStrCatLen(out, C_RESET, strlen(C_RESET));
        }
    }
    aoStrCatLen(out, "\x1b[0K", 4);
    aoStrCatPrintf(out, "\r\x1b[%dC", (int)(ls->plen + ls->pos));
    if (write(ls->ofd, out->data, out->len) == -1) { /* not fatal */ }
    aoStrRelease(out);
}

/* TERM values linenoise's blocking API refuses to do escapes on; its
 * multiplexed API skips that check, so mirror it here and fall back
 * to the (already dumb-terminal-safe) blocking call. */
static int replDumbTerm(void) {
    static const char *unsupported[] = {"dumb", "cons25", "emacs"};
    char *term = getenv("TERM");
    if (term == NULL) return 0;
    for (size_t i = 0; i < sizeof(unsupported)/sizeof(unsupported[0]); ++i) {
        if (strcasecmp(term, unsupported[i]) == 0) return 1;
    }
    return 0;
}

/* Blocking line edit with live highlighting: linenoiseBlockingEdit's
 * feed loop with a colour repaint after every keystroke. The final
 * repaint before Enter drops the hint so no grey text is left behind
 * in scrollback. */
static char *replLinenoiseEdit(const char *prompt) {
    if (replDumbTerm()) return linenoise(prompt);

    struct linenoiseState ls;
    char *buf = (char *)malloc(4096);
    if (buf == NULL) return linenoise(prompt);

    if (linenoiseEditStart(&ls, -1, -1, buf, 4096, prompt) == -1) {
        free(buf);
        return linenoise(prompt);
    }
    ls.buflen_max = 1024 * 1024; /* let big pastes grow it, as stock does */

    char *res;
    while ((res = linenoiseEditFeed(&ls)) == linenoiseEditMore) {
        replOverdraw(&ls, 1);
    }
    if (res != NULL) replOverdraw(&ls, 0);
    linenoiseEditStop(&ls);
    free(ls.buf);
    return res;
}

/* Fetch one physical line. Interactive lines come from linenoise
 * (editing, history, tab completion); piped input stays on plain
 * stdio so scripted use is deterministic. Returns a heap line WITHOUT
 * its trailing newline, or NULL on EOF. `*cancelled` is set when the
 * user pressed Ctrl-C (interactive only). */
static char *replGetLine(int interactive, const char *prompt,
                         int *cancelled) {
    *cancelled = 0;
    if (interactive) {
        errno = 0;
        char *line = replLinenoiseEdit(prompt);
        if (line == NULL && errno == EAGAIN) *cancelled = 1; /* Ctrl-C */
        return line;
    }
    char chunk[4096];
    AoStr *acc = aoStrNew();
    for (;;) {
        if (fgets(chunk, sizeof(chunk), stdin) == NULL) {
            if (acc->len == 0) {
                aoStrRelease(acc);
                return NULL;
            }
            break;
        }
        aoStrCatLen(acc, chunk, strlen(chunk));
        if (acc->data[acc->len - 1] == '\n') {
            acc->data[--acc->len] = '\0';
            break;
        }
    }
    /* Copy out of the AoStr pool: the caller frees with free(), the
     * same way it frees a linenoise() line. */
    char *line = (char *)malloc(acc->len + 1);
    memcpy(line, acc->data, acc->len + 1);
    aoStrRelease(acc);
    return line;
}

/* Accumulate lines until the input forms a submittable unit:
 *   - balanced and ending in `;` or `}`        -> submit
 *   - a single-line `#directive`               -> submit
 *   - balanced and the user enters a blank line -> submit (a `;` is
 *     appended if missing, so `2+2` <enter> <enter> evaluates)
 * Ctrl-C drops whatever is pending and re-prompts. Returns NULL on
 * EOF with nothing buffered. */
static AoStr *replReadInput(int interactive) {
    AoStr *buf = aoStrNew();

    for (;;) {
        const char *prompt = buf->len == 0 ? repl_prompt : repl_prompt_cont;
        int cancelled = 0;
        char *line = replGetLine(interactive, prompt, &cancelled);
        if (line == NULL) {
            if (cancelled) {
                buf->len = 0;
                buf->data[0] = '\0';
                continue;
            }
            if (buf->len == 0 || replLineIsBlank(buf->data)) {
                aoStrRelease(buf);
                return NULL;
            }
            if (interactive) fputc('\n', stdout);
            break;
        }

        int blank = replLineIsBlank(line);
        if (interactive && !blank) {
            linenoiseHistoryAdd(line);
            if (repl_history_path[0] != '\0') {
                linenoiseHistorySave(repl_history_path);
            }
        }
        aoStrCatLen(buf, line, strlen(line));
        aoStrPutChar(buf, '\n');
        free(line);

        if (replLineIsBlank(buf->data)) {
            /* Nothing typed yet - stay on the primary prompt. */
            buf->len = 0;
            buf->data[0] = '\0';
            continue;
        }

        ReplScan sc;
        replScan(buf->data, buf->len, &sc);
        if (sc.depth > 0 || sc.open || sc.pp_depth > 0) continue;

        char last = replLastChar(buf);
        if (replFirstChar(buf) == '#') {
            /* Preprocessor directive: newline-terminated (unless the
             * line is continued with a trailing backslash). */
            s64 i = (s64)buf->len - 1;
            while (i >= 0 && isspace((unsigned char)buf->data[i])) i--;
            if (i >= 0 && buf->data[i] == '\\') continue;
            break;
        } else if (replStartsWithShellEscape(buf)) {
            break;
        }
        if (last == ';' || last == '}') break;
        if (blank) {
            /* Balanced but unterminated and the user hit enter on an
             * empty line: terminate it for them. */
            aoStrPutChar(buf, ';');
            break;
        }
    }
    return buf;
}

/* ---------------- result echo ---------------- */

/* Can (and should) the value of `ast` be returned from the wrapper
 * and echoed? Statements and aggregate-typed values can't: aggregates
 * would need the struct-return ABI dance for no display benefit. */
static int replIsEchoable(Ast *ast) {
    if (!ast || !ast->type) return 0;
    /* Assignments and increments are mutations, not queries - echoing
     * the stored value back (`x = 5;` printing 5, `I64 x = 5;`
     * printing 0) reads as noise. Ask for the variable (`x;`) to see
     * it. Compound assigns (`x += 2`) desugar to plain ASSIGN, so
     * this covers them too. */
    if (ast->kind == AST_BINOP && ast->binop == AST_BIN_OP_ASSIGN) return 0;
    if (ast->kind == AST_UNOP &&
        (ast->unop == AST_UN_OP_POST_INC || ast->unop == AST_UN_OP_POST_DEC ||
         ast->unop == AST_UN_OP_PRE_INC  || ast->unop == AST_UN_OP_PRE_DEC))
    {
        return 0;
    }
    /* Direct calls to the print family already put their output on
     * stdout; echoing the returned byte count after it is just
     * confusing ("hi" followed by a mysterious 3). */
    if (ast->kind == AST_FUNCALL && ast->fname) {
        static const char *no_echo[] = {"printf"};
        for (size_t i = 0; i < sizeof(no_echo)/sizeof(no_echo[0]); ++i) {
            if (strlen(no_echo[i]) == (size_t)ast->fname->len &&
                memcmp(no_echo[i], ast->fname->data, ast->fname->len) == 0)
            {
                return 0;
            }
        }
    }
    switch (ast->type->kind) {
        case AST_TYPE_INT:
        case AST_TYPE_CHAR:
        case AST_TYPE_FLOAT:
        case AST_TYPE_POINTER:
        case AST_TYPE_FUNC:
            break;
        case AST_TYPE_ARRAY:
            /* A string literal's value is just its label address, so
             * it echoes fine as U8* (with the string preview) - e.g.
             * a floating `typeof(x);`. Other aggregates would need
             * the struct-return ABI dance. */
            if (ast->kind == AST_STRING) break;
            return 0;
        default:
            return 0;
    }
    switch (ast->kind) {
        case AST_IF: case AST_FOR: case AST_WHILE: case AST_DO_WHILE:
        case AST_SWITCH: case AST_CASE: case AST_DEFAULT: case AST_JUMP:
        case AST_GOTO: case AST_LABEL: case AST_BREAK: case AST_CONTINUE:
        case AST_RETURN: case AST_COMPOUND_STMT: case AST_DECL:
        case AST_ASM_STMT: case AST_TRY: case AST_THROW: case AST_COMMENT:
        case AST_PLACEHOLDER:
            return 0;
        default:
            return 1;
    }
}

static void replPrintInt(AstType *type, s64 val) {
    if (type->kind == AST_TYPE_POINTER || type->kind == AST_TYPE_FUNC) {
        printf("0x%llx", (unsigned long long)val);
        /* A pointer to a 1-byte int is HolyC's string type (U8 *) -
         * show a preview. This trusts the pointer the user's own code
         * just returned. */
        AstType *pt = type->ptr;
        if (val && pt && pt->size == 1 &&
            (pt->kind == AST_TYPE_CHAR || pt->kind == AST_TYPE_INT))
        {
            printf(" \"%.256s\"", (const char *)(uintptr_t)val);
        }
        putchar('\n');
        return;
    }
    if (type->issigned) printf("%lld\n", (long long)val);
    else                printf("%llu\n", (unsigned long long)val);
}

/* ---------------- per-round pipeline ---------------- */

typedef union ReplEntry {
    void *obj;
    s64 (*ifn)(void);
    double (*ffn)(void);
} ReplEntry;

/* Steal this round's top-level statements off the Cctrl (handing it
 * fresh lists) and wrap them into `__repl_<round>`. If the trailing
 * statement is an echoable expression it becomes the return value;
 * `*echo_type` is set to the type to print with (NULL = no echo).
 * Returns NULL when there were no statements this round. */
static Ast *replBuildWrapper(Cctrl *cc, int round, AstType **echo_type) {
    *echo_type = NULL;
    if (listEmpty(cc->initalisers)) return NULL;

    List *stmts = cc->initalisers;
    List *locals = cc->initaliser_locals;
    cc->initalisers = listNew();
    cc->initaliser_locals = listNew();

    AstType *rettype;
    Ast *last = (Ast *)stmts->prev->value;
    if (replIsEchoable(last)) {
        /* Return through I64/U64/F64 so the host-side caller only has
         * to deal with two machine-level signatures; the IR's return
         * conversion widens smaller ints/floats. Pointers keep their
         * type so the echo can show a string preview. */
        if (astIsFloatType(last->type)) {
            rettype = ast_float_type;
        } else if (last->kind == AST_STRING) {
            /* String literal: return the label address as U8* so the
             * echo shows the string preview. */
            rettype = astMakePointerType(ast_u8_type);
        } else if (last->type->kind == AST_TYPE_POINTER ||
                   last->type->kind == AST_TYPE_FUNC) {
            rettype = last->type;
        } else {
            rettype = last->type->issigned ? ast_int_type : ast_uint_type;
        }
        stmts->prev->value = astReturn(last, rettype);
        *echo_type = rettype;
    } else {
        rettype = ast_int_type;
        listAppend(stmts, astReturn(astI64Type(0), rettype));
    }

    char name[32];
    int len = snprintf(name, sizeof(name), "__repl_%d", round);
    Ast *body = astCompountStatement(stmts);
    Vec *params = astVecNew();
    AstType *fn_type = astMakeFunctionType(rettype, params);
    return astFunction(fn_type, name, len, params, body, locals, 0);
}

/* Drop statements a failed round left behind so they don't run as a
 * prelude to the next input. */
static void replDropInitialisers(Cctrl *cc) {
    if (!listEmpty(cc->initalisers)) {
        listRelease(cc->initalisers, NULL);
        cc->initalisers = listNew();
    }
    if (!listEmpty(cc->initaliser_locals)) {
        listRelease(cc->initaliser_locals, NULL);
        cc->initaliser_locals = listNew();
    }
}

/* Parse one buffer into the shared Cctrl. Returns the number of
 * errors (diagnostics already printed). */
static int replParse(Cctrl *cc, char *root_dir, char *name, AoStr *src) {
    Lexer *l = (Lexer *)malloc(sizeof(Lexer));
    lexInit(l, NULL, CCF_PRE_PROC);
    lexSetBuiltinRoot(l, root_dir);
    lexPushString(l, name, src->data, src->len);

    cctrlDiagClear(cc);
    cctrlResetTokenBuffer(cc);

    /* cctrlInitParse prefills the token ring buffer, which lexes the
     * input before parseToAst has installed its recovery point. Catch
     * lex-time raises (e.g. an over-long char const) here so a bad
     * literal errors like any other diagnostic instead of exiting
     * the REPL. */
    jmp_buf lex_recovery;
    jmp_buf *prev_recovery = cc->current_recovery;
    cc->current_recovery = &lex_recovery;
    if (setjmp(lex_recovery) == 0) {
        cctrlInitParse(cc, l);
        parseToAst(cc);
    }
    cc->current_recovery = prev_recovery;

    int errors = cctrlDiagFlush(cc);
    cctrlDiagClear(cc);
    lexerRelease(l);
    return errors;
}

static AoStr *replGetRcPath(void) {
    char *home = getenv("HOME");
    AoStr *hcc_rc = NULL;
    if (home) {
        hcc_rc = aoStrPrintf("%s/.hcc_rc.HC", home);
        if (access(hcc_rc->data, R_OK) != 0) {
            aoStrRelease(hcc_rc);
            hcc_rc = NULL;
        }
    }
    return hcc_rc;
}

/* Warm-up: parse the builtin stdlib header (prototypes, classes,
 * #defines for libtos, whose implementations the JIT dlopens) and
 * compile whatever it defines as chunk 0. */
static void replBootstrap(Cctrl *cc, HccJit *jit, CliArgs *args,
                          char *root_dir) {
    AoStr *builtin = aoStrPrintf("%s/include/tos.HH", args->install_dir);
    if (access(builtin->data, R_OK) != 0) {
        fprintf(stderr,
                "repl: warning: %s not found; the HolyC standard library "
                "will be unavailable\n", builtin->data);
        aoStrRelease(builtin);
        builtin = NULL;
    }
    
    AoStr *hcc_rc = replGetRcPath();

    /* We have nothing to preload */
    if (!builtin && !hcc_rc)
        return;

    Lexer *l = (Lexer *)malloc(sizeof(Lexer));
    lexInit(l, NULL, CCF_PRE_PROC);
    lexSetBuiltinRoot(l, root_dir);

    if (hcc_rc)  lexPushFile(l, hcc_rc);
    if (builtin) lexPushFile(l, builtin);

    cctrlDiagClear(cc);
    cctrlResetTokenBuffer(cc);
    cctrlInitParse(cc, l);
    parseToAst(cc);
    if (cctrlDiagFlush(cc) > 0) {
        fprintf(stderr, "repl: warning: errors in %s\n", builtin->data);
    }
    cctrlDiagClear(cc);
    replDropInitialisers(cc);

    if (hccJitCompileChunk(jit, NULL) != 0) {
        fprintf(stderr, "repl: warning: failed to compile the standard "
                "library header\n");
    }
    lexerRelease(l);
}

static void replExecInner(AoStr *input, int round) {
    HccJit *jit = repl_jit;
    if (input == NULL || *input->data == '\0' || jit == NULL) return;
    Cctrl *cc = jit->cc;

    if (replParse(cc, repl_root_dir, "<repl>", input) > 0) {
        replDropInitialisers(cc);
        return;
    }

    AstType *echo_type = NULL;
    Ast *wrapper = replBuildWrapper(cc, round, &echo_type);

    if (hccJitCompileChunk(jit, wrapper) != 0) {
        /* Diagnostics (unresolved symbol etc.) already printed by
         * the backend / finalizer. */
        cctrlDiagFlush(cc);
        cctrlDiagClear(cc);
        return;
    }

    if (wrapper == NULL) return; /* only definitions this round */

    ReplEntry entry;
    entry.obj = hccJitLookup(jit, wrapper->fname->data);
    if (entry.obj == NULL) {
        fprintf(stderr, "repl: internal error: lost entry point %s\n",
                wrapper->fname->data);
        return;
    }

    if (echo_type && astIsFloatType(echo_type)) {
        double r = entry.ffn();
        printf("%g\n", r);
    } else {
        s64 r = entry.ifn();
        if (echo_type)
            replPrintInt(echo_type, r);
    }
    fflush(stdout);
}

/* Fault-guard around an eval. Nested evals (JIT'd code calling the
 * ReplEval builtin re-enters here) stack: the outer jump point is
 * saved and restored either way, so a fault always unwinds to the
 * innermost live eval. sigsetjmp's savemask=1 restores the signal
 * mask on the jump - without it the delivered signal would stay
 * blocked and the second fault of a session would kill it. */
static void replExec(AoStr *input, int round) {
    sigjmp_buf saved_env;
    sig_atomic_t saved_in_eval = repl_in_eval;
    memcpy(&saved_env, &repl_fault_env, sizeof(sigjmp_buf));

    if (sigsetjmp(repl_fault_env, 1) == 0) {
        repl_in_eval = 1;
        /* Rounds tag memsafe reports; ReplEval's INT_MAX isn't a real
         * round, so drop it to "untagged". */
        memsafeSetRound(round == INT_MAX ? 0 : round);
        replExecInner(input, round);
    } else {
        replReportFault();
    }
    repl_in_eval = saved_in_eval;
    memcpy(&repl_fault_env, &saved_env, sizeof(sigjmp_buf));
}

int replRun(Cctrl *cc, CliArgs *args) {
    const HccJitBackend *backend = replHostBackend();
    if (!backend) {
        fprintf(stderr,
                "hcc: -repl is not supported on this host architecture\n");
        return 1;
    }

    cc->flags |= CCTRL_REPL;
    /* Try and prevent the repl from crashing through common errors like
     * passing mis-matching function argument types */
    cc->flags |= CCTRL_WERROR;

    HccJit *jit = hccJitNew(cc, backend);
    if (!jit) return 1;
    repl_jit = jit;

    /* Builtin helper functions */
    hccJitDefineSymbol(jit, "Uf", (void *)(uintptr_t)replBuiltinUf);
    hccJitDefineSymbol(jit, "ReplDel", (void *)(uintptr_t)replBuiltinReplDel);
    hccJitDefineSymbol(jit, "ReplReloadRc", (void *)(uintptr_t)replBuiltinReloadRc);
    hccJitDefineSymbol(jit, "ReplListFn", (void *)(uintptr_t)replBuiltinListFn);
    hccJitDefineSymbol(jit, "ReplListGVars", (void *)(uintptr_t)replBuiltinListGVars);
    hccJitDefineSymbol(jit, "ReplClassRep", (void *)(uintptr_t)replBuiltinPrintClass);
    hccJitDefineSymbol(jit, "ReplEval", (void *)(uintptr_t)replEvalString);
    hccJitDefineSymbol(jit, "ReplHeap", (void *)(uintptr_t)replBuiltinReplHeap);
    hccJitDefineSymbol(jit, "ReplFindFn", (void *)(uintptr_t)replBuiltinFindFn);

    /* Tracking allocator (memsafe.c) - always on in the REPL, and
     * registered before the bootstrap so the very first chunk already
     * binds to it. */
    memsafeInit(jit);

    int interactive = isatty(STDIN_FILENO);
    repl_root_dir = mprintf("%s/include/", args->install_dir);

    replInstallFaultHandlers();

    replBootstrap(cc, jit, args, repl_root_dir);

    AoStr *protos = aoStrDupRaw((char *)repl_builtin_protos,
                                sizeof(repl_builtin_protos) - 1);
    if (replParse(cc, repl_root_dir, "<builtins>", protos) > 0) {
        fprintf(stderr, "repl: warning: failed to parse builtin "
                "prototypes\n");
    }
    replDropInitialisers(cc);

    if (interactive) {
        repl_cc = cc;
        repl_use_colour = !replDumbTerm() && getenv("NO_COLOR") == NULL;
        linenoiseSetMultiLine(1);
        linenoiseSetCompletionCallback(replCompletion);
        linenoiseHistorySetMaxLen(REPL_HISTORY_MAX);
        const char *home = getenv("HOME");
        if (home != NULL) {
            snprintf(repl_history_path, sizeof(repl_history_path),
                     "%s/.hcc_repl_history", home);
            linenoiseHistoryLoad(repl_history_path);
        }
        if (!mapHas(cc->macro_defs, "HCC_REPL_NO_HELLO")) {
            printf("hcc %s [%s]\n"
                   "Statements run as typed; a trailing expression's value is "
                   "echoed.\nTab completes, arrows browse history, Ctrl-D "
                   "exits.\n",
                   cctrlGetVersion(),
                   cliTargetToString(cc->target));
        }
    }

    int round = 0;
    for (;;) {
        AoStr *input = replReadInput(interactive);
        if (input == NULL) break; /* EOF */

        if (replStartsWithShellEscape(input)) {
            char *cmd = input->data + strlen("@ ");
            int retval = system(cmd);
            printf("%d\n", retval);
            aoStrRelease(input);
            continue;
        }

        round++;
        replExec(input, round);
    }

    hccJitFree(jit);
    return 0;
}
