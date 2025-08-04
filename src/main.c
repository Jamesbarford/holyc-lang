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
#include "lexer.h"
#include "list.h"
#include "memory.h"
#include "transpiler.h"
#include "types.h"
#include "util.h"

int is_terminal;

#if defined(__aarch64__) || defined(__arm64ec__)
    #define _CC "clang --target=x86_64-apple-darwin"
#else
    #define _CC "gcc"
#endif

#define ASM_TMP_FILE "/tmp/holyc-asm.s"
#define LIB_BUFSIZ 256

#ifndef INSTALL_PREFIX
    #define INSTALL_PREFIX "/usr/local"
#endif

#define CLIBS_BASE "-ltos -lpthread -lc -lm"

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

int hccLibInit(hccLib *lib, CliArgs *args, char *name) { 
    AoStr *dylib_cmd = aoStrNew();
    AoStr *stylib_cmd = aoStrNew();
    AoStr *installcmd = aoStrNew();
    
#if IS_BSD
    snprintf(lib->stylib_name,LIB_BUFSIZ,"%s.a",name);
    snprintf(lib->dylib_name,LIB_BUFSIZ,"%s.dylib",name);
    snprintf(lib->dylib_version_name,LIB_BUFSIZ,"%s.0.0.1.dylib",name);

    aoStrCatFmt(dylib_cmd,
            "cp -pPR ./%s "INSTALL_PREFIX"/lib/lib%s && "
            "cc -dynamiclib -Wl,-install_name,"INSTALL_PREFIX"/lib/%s -o %s "CLIBS" -o %s %s",
            lib->stylib_name,
            lib->stylib_name,
            name,
            lib->dylib_version_name,
            lib->dylib_name,
            args->obj_outfile);

    aoStrCatPrintf(installcmd,
            "cp -pPR ./%s "INSTALL_PREFIX"/lib/lib%s && "
            "ln -sf "INSTALL_PREFIX"/lib/%s "INSTALL_PREFIX"/lib/%s",
            lib->dylib_name,
            lib->dylib_version_name,

            lib->dylib_version_name,
            lib->dylib_name);

#elif IS_LINUX
    snprintf(lib->stylib_name,LIB_BUFSIZ,"%s.a",name);
    snprintf(lib->dylib_name,LIB_BUFSIZ,"%s.so",name);
    snprintf(lib->dylib_version_name,LIB_BUFSIZ,"%s.so.0.0.1",name);
    aoStrCatPrintf(dylib_cmd,
            _CC" -fPIC -shared -Wl,-soname,"INSTALL_PREFIX"/lib/%s -o %s "CLIBS,
            name,
            lib->dylib_name,
            lib->dylib_name);
    aoStrCatPrintf(dylib_cmd," -o %s %s",lib->dylib_name,args->obj_outfile);
    aoStrCatPrintf(installcmd,
            "cp -pPR ./%s "INSTALL_PREFIX"/lib/lib%s",
            lib->stylib_name,lib->stylib_name);
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

void emitFile(AoStr *asmbuf, CliArgs *args) {
    AoStr *cmd = aoStrNew();
    hccLib lib;
    if (args->emit_object) {
        writeAsmToTmp(asmbuf);
        aoStrCatPrintf(cmd, _CC" -c %s "CLIBS" %s -o ./%s",
                ASM_TMP_FILE,args->clibs,args->obj_outfile);
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
            loggerPanic("Failed to write data expected %lld got %lld\n",
                    (s64)asmbuf->len, written);
        }
        close(fd);
    } else if (args->emit_dylib) {
        writeAsmToTmp(asmbuf);
        hccLibInit(&lib,args,args->lib_name);
        aoStrCatPrintf(cmd, _CC" -fPIC -c %s -o ./%s",
                ASM_TMP_FILE,args->obj_outfile);
        fprintf(stderr,"%s\n",cmd->data);
        safeSystem(cmd->data);
        fprintf(stderr,"%s\n",lib.stylib_cmd);
        safeSystem(lib.stylib_cmd);

#if IS_MACOS
        fprintf(stderr,"%s\n",lib.dylib_cmd);
        safeSystem(lib.dylib_cmd);
#endif /* ifdef  IS_MACOS */

        fprintf(stderr,"%s\n",lib.install_cmd);
        safeSystem(lib.install_cmd);
    } else {
        if (args->run) {
            AoStr *run_cmd = aoStrNew();
            writeAsmToTmp(asmbuf);
            aoStrCatPrintf(run_cmd,_CC" -L"INSTALL_PREFIX"/lib %s "CLIBS" && ./a.out && rm ./a.out",
                    ASM_TMP_FILE);
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
            aoStrCatPrintf(cmd, _CC" -L"INSTALL_PREFIX"/lib %s %s "CLIBS" -o %s", 
                    ASM_TMP_FILE,args->clibs,args->output_filename);
        } else {
            aoStrCatPrintf(cmd, _CC" -L"INSTALL_PREFIX"/lib %s "CLIBS" -o %s", 
                    ASM_TMP_FILE, args->output_filename);
        }
        safeSystem(cmd->data);
    }
    if (strnlen(args->clibs,10) > 1) {
        free(args->clibs);
    }
    remove(ASM_TMP_FILE);
    aoStrRelease(cmd);
    aoStrRelease(asmbuf);
}

void assemble(CliArgs *args) {
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
        aoStrCatPrintf(run_cmd,_CC" -L"INSTALL_PREFIX"/lib %s "CLIBS" && ./a.out && rm ./a.out",
                ASM_TMP_FILE);
        free(buffer);
        int ret = system(run_cmd->data);
        (void)ret;
    } else {
        aoStrCatPrintf(run_cmd, _CC" %s -L"INSTALL_PREFIX"/lib "CLIBS, args->infile);
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

    if (args.assemble) {
        assemble(&args);
        goto success;
    }

    cc = cctrlNew();
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

    compileToAst(cc,&args,lexer_flags);

    if (args.print_ast) {
        compilePrintAst(cc);
        goto success;
    }

    if (args.dump_ir) {
        irDump(cc);
        goto success;
    }

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

    emitFile(asmbuf, &args);
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
