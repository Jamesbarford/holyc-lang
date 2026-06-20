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

void safeSystem(const char *cmd) {
    int ok = system(cmd);
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

static int hccCanLinkLibTos(Cctrl *cc) {
    (void)cc;
    return 1;
}

int hccLibInit(Cctrl *cc, hccLib *lib, CliArgs *args, char *name) { 
    AoStr *dylib_cmd = aoStrNew();
    AoStr *stylib_cmd = aoStrNew();
    AoStr *installcmd = aoStrNew();
    
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
            "%s -dynamiclib -Wl,-install_name,%s/lib/%s -o %s "CLIBS" -o %s %s",
            lib->stylib_name,
            args->install_dir,
            lib->stylib_name,
            cc->CC,
            args->install_dir,
            name,
            lib->dylib_version_name,
            lib->dylib_name,
            args->obj_outfile);

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
    snprintf(lib->stylib_name,LIB_BUFSIZ,"%s.a",name);
    snprintf(lib->dylib_name,LIB_BUFSIZ,"%s.so",name);
    snprintf(lib->dylib_version_name,LIB_BUFSIZ,"%s.so.0.0.1",name);
    aoStrCatPrintf(dylib_cmd,
            "%s -fPIC -shared -Wl,-soname,%s/lib/%s -o %s "CLIBS,
            cc->CC,
            args->install_dir,
            name,
            lib->dylib_name,
            lib->dylib_name);
    aoStrCatPrintf(dylib_cmd," -o %s %s",lib->dylib_name,args->obj_outfile);
    /* Install the static lib (AOT links this via `-ltos`) AND the
     * VERSIONED shared object (the JIT dlopen's it - jitLoadLibtos
     * probes `lib/libtos.so.0.0.1`). Deliberately no unversioned
     * `libtos.so` symlink: that would make `-ltos` prefer the shared
     * object over the archive, changing AOT to dynamic linking. */
    aoStrCatPrintf(installcmd,
            "cp -pPR ./%s %s/lib/lib%s",
            lib->stylib_name, args->install_dir, lib->stylib_name);
#else
#error "System not supported"
#endif
    aoStrCatPrintf(stylib_cmd,"ar rcs %s %s",lib->stylib_name,args->obj_outfile);
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
        aoStrCatPrintf(cmd, "%s -c %s "CLIBS" %s -o ./%s",
                cc->CC,
                ASM_TMP_FILE,
                args->clibs,
                object_file_name);
        safeSystem(cmd->data);
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
        aoStrCatPrintf(cmd, "%s -fPIC -c %s -o ./%s",
                cc->CC,
                ASM_TMP_FILE,args->obj_outfile);
        fprintf(stderr,"%s\n",cmd->data);
        safeSystem(cmd->data);
        fprintf(stderr,"%s\n",lib.stylib_cmd);
        safeSystem(lib.stylib_cmd);

#if IS_MACOS
        fprintf(stderr,"%s\n",lib.dylib_cmd);
        safeSystem(lib.dylib_cmd);
#endif

        fprintf(stderr,"%s\n",lib.install_cmd);
        safeSystem(lib.install_cmd);
    } else {
        if (args->run) {
            AoStr *run_cmd = aoStrNew();
            writeAsmToTmp(asmbuf);
            if (hccCanLinkLibTos(cc)) {
                aoStrCatPrintf(run_cmd,"%s -L%s/lib %s %s "CLIBS" -ltos && ./a.out && rm ./a.out",
                        cc->CC,
                        args->install_dir,
                        ASM_TMP_FILE,
                        args->clibs ? args->clibs : "");
            } else {
                aoStrCatPrintf(run_cmd,"%s %s %s "CLIBS" && ./a.out && rm ./a.out",
                        cc->CC,
                        ASM_TMP_FILE,
                        args->clibs ? args->clibs : "");
            }
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

        if (args->clibs) {
            if (hccCanLinkLibTos(cc)) {
                aoStrCatPrintf(cmd, "%s -L%s/lib %s %s "CLIBS" -ltos -o %s",
                        cc->CC,
                        args->install_dir,
                        ASM_TMP_FILE,args->clibs,args->output_filename);
            } else {
                aoStrCatPrintf(cmd, "%s %s %s "CLIBS" -o %s",
                        cc->CC,
                        ASM_TMP_FILE,
                        args->clibs,
                        args->output_filename);
            }

        } else {
            if (hccCanLinkLibTos(cc)) {
                aoStrCatPrintf(cmd,
                               "%s -L%s/lib %s "CLIBS" -ltos -o %s",
                               cc->CC,
                               args->install_dir,
                               ASM_TMP_FILE,
                               args->output_filename);
            } else {
                aoStrCatPrintf(cmd,
                               "%s %s "CLIBS" -o %s",
                               cc->CC,
                               ASM_TMP_FILE,
                               args->output_filename);
            }
        }
        safeSystem(cmd->data);
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
        if (hccCanLinkLibTos(cc)) {
            aoStrCatPrintf(run_cmd,
                           "%s -L"INSTALL_PREFIX"/lib %s "CLIBS" -ltos && ./a.out && rm ./a.out",
                            cc->CC,
                            ASM_TMP_FILE);
        } else {
            aoStrCatPrintf(run_cmd,
                           "%s %s "CLIBS" && ./a.out && rm ./a.out",
                           cc->CC,
                           ASM_TMP_FILE);
        }
        free(buffer);
        int ret = system(run_cmd->data);
        (void)ret;
    } else {
        if (hccCanLinkLibTos(cc)) {
            aoStrCatPrintf(run_cmd, "%s %s -L"INSTALL_PREFIX"/lib "CLIBS" -ltos",
                    cc->CC, args->infile);
        } else {
            aoStrCatPrintf(run_cmd, "%s %s "CLIBS, cc->CC, args->infile);
        }
        safeSystem(run_cmd->data);
    }
    aoStrRelease(run_cmd);
}


void memoryInit(void) {
    astMemoryInit();
    lexemeMemoryInit();
    globalArenaInit(4096*10);
    irMemoryInit();
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

    /* Plumb in support for cross compiling... currently does nothing :)*/
    const char *str_target = cliTargetToString(args.target);
    switch (args.target) {
        /* Let clang do the heavy lifting */
        case TARGET_AARCH64_UNKNOWN_LINUX_GNU:
        case TARGET_X86_64_APPLE_DARWIN:
        case TARGET_X86_64_UNKNOWN_LINUX_GNU:
        case TARGET_AARCH64_APPLE_DARWIN:
            cc->CC = mprintf("clang --target=%s", str_target);
            break;
    }

    if (args.use_legacy_x86) {
        cc->flags |= CCTRL_USE_LEGACY_X86;
    }

    if (args.assemble) {
        assemble(cc, &args);
        goto success;
    }

    if (!listEmpty(args.defines_list)) {
        cctrlSetCommandLineDefines(cc,args.defines_list);
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
        hccJitFree(jit);
        memoryRelease();
        return rc;
    }
#endif

    if (args.cfg_create || args.cfg_create_png || args.cfg_create_svg) {
        Vec *cfgs = cfgConstruct(cc);
        char *dot_outfile = mprintf("./%s.dot",args.infile_no_ext);
        cfgsToFile(cfgs,dot_outfile);
        if (args.cfg_create_png || args.cfg_create_svg) {
            char *ext = args.cfg_create_png ? "png" : "svg";
            char *dot_cmd = mprintf("dot -T%s %s -o ./%s.%s",
                    ext,dot_outfile,args.infile_no_ext,ext);
            printf("Creating %s: %s\n",ext,dot_cmd);
            safeSystem(dot_cmd);
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
