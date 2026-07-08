#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "cfg-print.h"
#include "cfg.h"
#include "cli.h"
#include "compile.h"
#include "config.h"
#include "ir.h"
#include "ir-types.h"
#ifdef HCC_ENABLE_JIT
#include "aarch64-jit.h"
#include "x86_64-jit.h"
#include "memsafe.h"
#include "repl.h"
#endif
#include "lexer.h"
#include "list.h"
#include "memory.h"
#include "transpiler.h"
#include "types.h"
#include "util.h"

int is_terminal;

#define ASM_TMP_FILE "/tmp/holyc-asm.s"
#define LIB_BUFSIZ 256

#ifndef INSTALL_PREFIX
    #define INSTALL_PREFIX "/usr/local"
#endif

#define CLIBS_BASE "-lpthread -lc -lm"

#ifdef HCC_LINK_SQLITE3
#define CLIBS CLIBS_BASE" -lsqlite3"
#else
#define CLIBS CLIBS_BASE
#endif

void safeSystem(const char *cmd, int print_cmd) {
    int ok = system(cmd);
    if (print_cmd)
        fprintf(stderr, "%s\n", cmd);
    if (ok != 0) {
        loggerPanic("Failed to execute command: '%s'\n", cmd);
    }
}

typedef struct hccLib {
    char *name;
    char *version;
    char *objectfile;
    char *dylib_ext;
    char dylib_name[LIB_BUFSIZ];
    char dylib_version_name[LIB_BUFSIZ];
    char *stylib_ext;
    char stylib_name[LIB_BUFSIZ];
    char *dylib_cmd;
    char *stylib_cmd;
    char *install_cmd;
} hccLib;

/* `-l` flags for every `#link <name>` directive, ready to splice into
 * a link command. Empty string when nothing was #link'd. Also adds
 * `-L` for lib dirs the platform linker doesn't necessarily search on
 * its own (Homebrew on Apple Silicon most notably) - but only ones
 * that exist, so ld doesn't warn, and only when a `#link <name>` is
 * actually in play. */
static AoStr *linkLibFlags(Cctrl *cc) {
    AoStr *flags = aoStrNew();
    if (!listEmpty(cc->link_libs)) {
        static const char *const extra_dirs[] = {
            "/opt/homebrew/lib",
            "/usr/local/lib",
        };
        for (size_t i = 0; i < sizeof(extra_dirs)/sizeof(extra_dirs[0]); ++i) {
            if (access(extra_dirs[i], F_OK) == 0) {
                aoStrCatFmt(flags, "-L%s ", extra_dirs[i]);
            }
        }
        listForEach(cc->link_libs) {
            AoStr *name = it->value;
            aoStrCatFmt(flags, "-l%s ", name->data);
        }
    }
    return flags;
}

int hccLibInit(Cctrl *cc, hccLib *lib, CliArgs *args, char *name) {
    AoStr *dylib_cmd = aoStrNew();
    AoStr *stylib_cmd = aoStrNew();
    AoStr *installcmd = aoStrNew();
    AoStr *link_flags = linkLibFlags(cc);
    
    /* All literal INSTALL_PREFIX paths are now plumbed through
     * args->install_dir so a `-lib tos --install-dir=X` build lands
     * in X/lib/ rather than always /usr/local/. The default install
     * dir is still INSTALL_PREFIX (set in cliArgsInit), so existing
     * invocations behave unchanged - this is purely an opt-in
     * redirection for cross-builds and sandboxed installs. */
#if IS_BSD
    snprintf(lib->stylib_name,LIB_BUFSIZ,"%s.a",name);
    snprintf(lib->dylib_name,LIB_BUFSIZ,"%s.dylib",name);
    snprintf(lib->dylib_version_name,LIB_BUFSIZ,"%s.0.0.1.dylib",name);

    aoStrCatPrintf(dylib_cmd,
            "cp -pPR ./%s %s/lib/lib%s && "
            "%s -dynamiclib -Wl,-install_name,%s/lib/%s -o %s "CLIBS" -o %s %s %s",
            lib->stylib_name,
            args->install_dir,
            lib->stylib_name,
            cc->CC,
            args->install_dir,
            name,
            lib->dylib_version_name,
            lib->dylib_name,
            args->obj_outfile,
            link_flags->data);

    aoStrCatPrintf(installcmd,
            "cp -pPR ./%s %s/lib/lib%s && "
            "ln -sf %s/lib/%s %s/lib/%s",
            lib->dylib_name,
            args->install_dir,
            lib->dylib_version_name,
            args->install_dir,
            lib->dylib_version_name,
            args->install_dir,
            lib->dylib_name);

#elif IS_LINUX
    snprintf(lib->stylib_name,LIB_BUFSIZ,"lib%s.a",name);
    snprintf(lib->dylib_name,LIB_BUFSIZ,"lib%s.so",name);
    snprintf(lib->dylib_version_name,LIB_BUFSIZ,"lib%s.so.0.0.1",name);

    char *lib_install_dir = tprintf("%s/lib", args->install_dir);

    /* `-Bsymbolic` binds libtos's internal global references (e.g. the
     * `Fs` exception object shared by HCC_PushFrame/HCC_Throw) to
     * libtos's own definitions. That makes those symbols non-preemptible,
     * which is what lets the code generators use direct RIP-relative /
     * adrp+add addressing (already position independent) rather than
     * routing every global access through the GOT. The symbols stay in
     * the dynamic table, so the JIT can still dlsym them (e.g. `Fs`). */
    aoStrCatPrintf(dylib_cmd,
            "%s -fPIC -shared -Wl,-Bsymbolic -Wl,-soname,%s %s -o %s "CLIBS" %s",
            cc->CC,
            lib->dylib_version_name,
            args->obj_outfile,
            lib->dylib_name,
            link_flags->data);

    /* Install two artefacts:
     *   - the static archive `libtos.a`, which AOT links via `-ltos`.
     *   - the VERSIONED shared object `libtos.so.0.0.1`, which the JIT
     *     dlopen's (jitLoadLibtos probes `lib/libtos.so.0.0.1`).
     *
     * Deliberately NO unversioned `libtos.so` symlink: that would make
     * `-ltos` prefer the shared object and dynamic-link AOT. Because
     * libtos is built with `-Bsymbolic` (see above), a dynamically
     * linked executable gets a COPY relocation for exported data like
     * `Fs` while libtos's own functions keep writing their internal
     * copy - two `Fs` objects, so the first `throw` null-derefs. Static
     * linking keeps a single `Fs`; `-ltos` finds the archive because no
     * `libtos.so` exists beside it. */
    aoStrCatPrintf(installcmd,
            "cp -pPR ./%s %s/%s && ",
            lib->stylib_name,
            lib_install_dir,
            lib->stylib_name);

    /* Copy the versioned .so file to somewhere like /usr/local/lib */
    aoStrCatPrintf(installcmd, "cp -pPR ./%s %s/%s",
            lib->dylib_name,
            lib_install_dir,
            lib->dylib_version_name);
#else
#error "System not supported"
#endif
    aoStrCatPrintf(stylib_cmd,"ar rcs %s %s",lib->stylib_name,args->obj_outfile);
    aoStrRelease(link_flags);
    lib->install_cmd = aoStrMove(installcmd);
    lib->dylib_cmd = aoStrMove(dylib_cmd);
    lib->stylib_cmd = aoStrMove(stylib_cmd);
    
    return 1;
}

int writeAsmToTmp(AoStr *asmbuf) {
    int fd;
    s64 written = 0;
    u64 towrite = 0;
    char *ptr;
    ptr = asmbuf->data;

    if ((fd = open(ASM_TMP_FILE,O_RDWR|O_TRUNC|O_CREAT,0644)) == -1) {
        loggerPanic("Failed to create file for intermediary assembly: %s\n",
                strerror(errno));
    }

    towrite = asmbuf->len;
    ptr = asmbuf->data;

    while (towrite > 0) {
        written = write(fd,ptr,towrite);
        if (written < 0) {
            if (written == EINTR) {
                continue;
            }
            close(fd);
            loggerPanic("Failed to create file for intermediary assembly: %s\n",
                    strerror(errno));
        }
        towrite -= written;
        ptr += written;
    }
    close(fd);
    return 1;
} 

void emitFile(Cctrl *cc, AoStr *asmbuf, CliArgs *args) {
    AoStr *cmd = aoStrNew();
    hccLib lib;
    if (args->emit_object) {
        writeAsmToTmp(asmbuf);
        char *object_file_name = args->output_filename ? 
                                 args->output_filename :
                                 args->obj_outfile;
        char *fPIC = args->fPIC ? "-fPIC -shared" : "";
        aoStrCatPrintf(cmd, "%s -c %s %s "CLIBS" %s -o ./%s",
                cc->CC,
                fPIC,
                ASM_TMP_FILE,
                args->clibs,
                object_file_name);
        safeSystem(cmd->data, 0);
    } else if (args->asm_outfile && args->assemble_only) {
        int fd;
        u64 flags = O_CREAT|O_RDWR|O_TRUNC;

        if (args->to_stdout) {
            fd = STDOUT_FILENO;
        } else if (args->output_filename != NULL) {
            fd = open(args->output_filename, flags, 0644);
        } else {
            fd = open(args->asm_outfile, flags, 0644);
        }
        if (fd == -1) {
            loggerPanic("Failed to open '%s' - %s\n",
                args->asm_outfile, strerror(errno));
        }
        s64 written = write(fd,asmbuf->data,asmbuf->len);
        if (written != (s64)asmbuf->len) {
            loggerPanic("Failed to write data expected %ld got %ld\n",
                    (s64)asmbuf->len, written);
        }
        close(fd);
    } else if (args->emit_dylib) {
        writeAsmToTmp(asmbuf);
        hccLibInit(cc, &lib,args,args->lib_name);
        aoStrCatPrintf(cmd, "%s -shared -fPIC -c %s -o ./%s",
                cc->CC,
                ASM_TMP_FILE,args->obj_outfile);
        safeSystem(cmd->data,1);
        safeSystem(lib.dylib_cmd, 1);
        safeSystem(lib.stylib_cmd, 1);
        safeSystem(lib.install_cmd, 1);
    } else {
        AoStr *ofiles = aoStrNew();
        AoStr *link_flags = linkLibFlags(cc);

        /* Concatinate a string with all of the .o and .so files */
        if (!listEmpty(cc->object_files)) {
            listForEach(cc->object_files) {
                AoStr *o = it->value;
                aoStrCatFmt(ofiles, "%s ", o->data);
            }
        }
        if (!listEmpty(cc->shared_object_files)) {
            listForEach(cc->shared_object_files) {
                AoStr *so = it->value;
                aoStrCatFmt(ofiles, "%s ", so->data);
            }
        }

        if (ofiles->len == 0) {
            aoStrPutChar(ofiles, ' ');
        }

        if (args->run) {
            AoStr *run_cmd = aoStrNew();
            writeAsmToTmp(asmbuf);
            aoStrCatPrintf(run_cmd,"%s -L%s/lib %s %s %s %s "CLIBS" -ltos && ./a.out && rm ./a.out",
                    cc->CC,
                    args->install_dir,
                    ASM_TMP_FILE,
                    ofiles->data,
                    link_flags->data,
                    args->clibs ? args->clibs : "");
            /* Don't use 'safeSystem' else anything other than a '0' exit
             * code will cause a panic which is incorrect... This is a bit of a
             * hack as run, in an ideal world, would not be calling out to `_CC` */
            int ret = system(run_cmd->data);
            (void)ret;
            aoStrRelease(run_cmd);
            exit(EXIT_SUCCESS);
        }

        writeAsmToTmp(asmbuf);

        if (args->output_filename == NULL) {
            args->output_filename = "a.out";
        }
        char *clibs = args->clibs ? args->clibs : "";

        aoStrCatPrintf(cmd, "%s -L%s/lib %s %s %s %s -ltos "CLIBS" -o %s",
                cc->CC,
                args->install_dir,
                ASM_TMP_FILE,
                ofiles->data,
                link_flags->data,
                clibs,
                args->output_filename);

        safeSystem(cmd->data, 0);
    }
    remove(ASM_TMP_FILE);
    aoStrRelease(cmd);
    aoStrRelease(asmbuf);
}

void assemble(Cctrl *cc, CliArgs *args) {
    AoStr *run_cmd = aoStrNew();

    if (args->run) {
        s64 len = 0;
        char *buffer = lexReadfile(args->infile,&len);
        AoStr asm_buf = {
            .data = buffer,
            .len = len,
            .capacity = len,
        };
        writeAsmToTmp(&asm_buf);
        aoStrCatPrintf(run_cmd,
                "%s -L"INSTALL_PREFIX"/lib %s "CLIBS" -ltos && ./a.out && rm ./a.out",
                cc->CC,
                ASM_TMP_FILE);
        free(buffer);
        int ret = system(run_cmd->data);
        (void)ret;
    } else {
        aoStrCatPrintf(run_cmd, "%s %s -L"INSTALL_PREFIX"/lib "CLIBS" -ltos",
                cc->CC, args->infile);
        safeSystem(run_cmd->data, 0);
    }
    aoStrRelease(run_cmd);
}

void memoryInit(void) {
    astMemoryInit();
    lexemeMemoryInit();
    globalArenaInit(4096*10);
    irMemoryInit();
    aoStrTmpBufInit();
}

void memoryRelease(void) {
    astMemoryRelease();
    irMemoryRelease();
    lexemeMemoryRelease();
    globalArenaRelease();
}

void memoryPrintStats(void) {
    printf("============== Arena Stats ===========\n");
    lexemeMemoryStats();
    printf("\n\n");
    astMemoryStats();
    printf("\n\n");
    irMemoryStats();
    printf("\n\n");
    globalArenaPrintStats();
    printf("\n\n");
    aoStrPoolPrintStats();
    printf("=========== Arena Stats End ==========\n");
}

int main(int argc, char **argv) {
    memoryInit();
    is_terminal = isatty(STDOUT_FILENO) && isatty(STDERR_FILENO);

    /* Using these pools vastly simplifies everything as we can free everything 
     * at the end. */

    int lexer_flags = CCF_PRE_PROC;
    AoStr *asmbuf;
    Cctrl *cc;

    CliArgs args;
    cliArgsInit(&args);
    /* now parse cli options */
    args.install_dir = INSTALL_PREFIX;
    cliParseArgs(&args,argc,argv);

    cc = cctrlNew(args.target);

    cc->install_dir = args.install_dir;
    cc->is_pic = args.fPIC;

    cc->object_files = args.object_files;
    cc->shared_object_files = args.shared_object_files;

    /* Plumb in support for cross compiling... currently does nothing :)*/
    const char *str_target = cliTargetToString(args.target);
    if (args.is_cross_compile) {
        switch (args.target) {
            /* Let clang do the heavy lifting */
            case TARGET_AARCH64_UNKNOWN_LINUX_GNU:
            case TARGET_X86_64_APPLE_DARWIN:
            case TARGET_X86_64_UNKNOWN_LINUX_GNU:
            case TARGET_AARCH64_APPLE_DARWIN:
                cc->CC = mprintf("clang --target=%s", str_target);
                break;
        }
    } else {
        cc->CC = mprintf("cc");
    }

    if (args.use_legacy_x86) cc->flags |= CCTRL_USE_LEGACY_X86;
    if (args.werror)         cc->flags |= CCTRL_WERROR;
    if (args.memsafe)        cc->flags |= CCTRL_MEMSAFE;

    /* Exactly one of the two is always defined; `#ifjit` / `#ifaot`
     * key off them. The REPL runs on the JIT, so it counts. */
    if (args.jit || args.repl) {
        cctrlAddDefine(cc, "__HCC_JIT__");
    } else {
        cctrlAddDefine(cc, "__HCC_AOT__");
    }

    if (args.assemble) {
        assemble(cc, &args);
        goto success;
    }

    if (!listEmpty(args.defines_list)) {
        cctrlSetCommandLineDefines(cc,args.defines_list);
    }

    if (args.repl) {
#ifdef HCC_ENABLE_JIT
        int rc = replRun(cc, &args);
        memoryRelease();
        return rc;
#else
        fprintf(stderr, "hcc: this build has no JIT; -repl is unavailable "
                "(rebuild with -DHCC_ENABLE_JIT=on)\n");
        memoryRelease();
        return 1;
#endif
    }

    if (args.print_tokens) {
        compileToTokens(cc,&args,lexer_flags);
        goto success;
    }

    if (args.transpile) {
        AoStr *buf = transpileToC(cc,&args);
        printf("/* This code has been automatically generated by running: \n"
               " * `hcc -transpile %s`\n"
               " * please check for errors! */\n\n"
               "%s",args.infile, buf->data);
        goto success;
    }

    if (!compileToAst(cc,&args,lexer_flags)) {
        /* Parser surfaced one or more errors and the diagnostics
         * have already been printed. Bail before codegen. */
        memoryRelease();
        return 1;
    }

    if (args.print_ast) {
        compilePrintAst(cc);
        goto success;
    }

    if (args.dump_ir) {
        irDump(cc);
        goto success;
    }

#ifdef HCC_ENABLE_JIT
    if (args.jit) {
        /* Arm the tracking allocator before compiling - the JIT's
         * finalize binds MAlloc/Free call sites at chunk compile. */
#if defined(__aarch64__) || defined(__arm64__)
        HccJit *jit = aarch64JitCompile(cc);
#elif defined(__x86_64__)
        HccJit *jit = x86_64JitCompile(cc);
#else
        HccJit *jit = NULL;
        fprintf(stderr, "hcc: -jit is not supported on this host architecture\n");
#endif
        if (!jit) {
            /* The backend prints its own diagnostic. */
            memoryRelease();
            return 1;
        }
        int rc = hccJitRunMain(jit, argc, argv);
        if (args.memsafe) memsafeReportLeaks(stderr);
        hccJitFree(jit);
        memoryRelease();
        return rc;
    }
#endif
    /* -jit returns above and -repl returned earlier; reaching here
     * with the flag set means an AOT-style invocation. */
    if (args.memsafe)
        fprintf(stderr,
                "hcc: -Memsafe currently requires -jit or -repl; ignored\n");

    if (args.cfg_create || args.cfg_create_png || args.cfg_create_svg) {
        Vec *cfgs = cfgConstruct(cc);
        char *dot_outfile = mprintf("./%s.dot",args.infile_no_ext);
        cfgsToFile(cfgs,dot_outfile);
        if (args.cfg_create_png || args.cfg_create_svg) {
            char *ext = args.cfg_create_png ? "png" : "svg";
            char *dot_cmd = mprintf("dot -T%s %s -o ./%s.%s",
                    ext,dot_outfile,args.infile_no_ext,ext);
            printf("Creating %s: %s\n",ext,dot_cmd);
            safeSystem(dot_cmd, 0);
            unlink(dot_outfile);
        }
        goto success;
    }

    asmbuf = compileToAsm(cc);
    emitFile(cc, asmbuf, &args);

    if (args.defines_list) {
        listRelease(args.defines_list,NULL);
    }

success:
    if (args.print_mem_stats) {
        memoryPrintStats();
    }
    memoryRelease();
    return 0;
}
