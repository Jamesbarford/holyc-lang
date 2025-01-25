#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "aostr.h"
#include "cctrl.h"
#include "cfg-print.h"
#include "cfg.h"
#include "compile.h"
#include "config.h"
#include "lexer.h"
#include "list.h"
#include "transpiler.h"
#include "util.h"
#include "version.h"

#define ASM_TMP_FILE "/tmp/holyc-asm.s"
#define LIB_BUFSIZ 256

#ifndef INSTALL_PREFIX
    #define INSTALL_PREFIX "/usr/local"
#endif

#define CLIBS_BASE "-ltos -lpthread  -lc -lm"

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

int hccLibInit(hccLib *lib, HccOpts *opts, char *name) { 
    aoStr *dylib_cmd = aoStrNew();
    aoStr *stylib_cmd = aoStrNew();
    aoStr *installcmd = aoStrNew();
    
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
            opts->obj_outfile);

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
            "gcc -fPIC -shared -Wl,-soname,"INSTALL_PREFIX"/lib/%s -o %s "CLIBS,
            name,
            lib->dylib_name,
            lib->dylib_name);
    aoStrCatPrintf(dylib_cmd," -o %s %s",lib->dylib_name,opts->obj_outfile);
    aoStrCatPrintf(installcmd,
            "cp -pPR ./%s "INSTALL_PREFIX"/lib/lib%s",
            lib->stylib_name,lib->stylib_name);
#else
#error "System not supported"
#endif
    aoStrCatPrintf(stylib_cmd,"ar rcs %s %s",lib->stylib_name,opts->obj_outfile);
    lib->install_cmd = aoStrMove(installcmd);
    lib->dylib_cmd = aoStrMove(dylib_cmd);
    lib->stylib_cmd = aoStrMove(stylib_cmd);
    
    return 1;
}

void getASMFileName(HccOpts *opts, char *file_name) {
    int len = strlen(file_name);
    char *slashptr = NULL, *end = &file_name[len-1];

    for (int i = len -1; i >= 0; --i) {
      if (file_name[i] == '/') {
        slashptr = &file_name[i];
        slashptr += 1;
        break;
      }
    }

    if (tolower(*end) == 'c' && tolower(*(end-1)) == 'h' && *(end-2) == '.') {
      end -= 2;
    } else if (tolower(*end) == 'c' && tolower(*(end-1)) == 'h' && *(end-2) == '.') {
      end -= 2;
    } else if (tolower(*end) == 's' && *(end-1) == '.') {
      opts->assemble = 1;
      end -= 1;
    } else {
      loggerPanic("Unknown file extension, file must end with .HC or .HH case insensitive. Got: %s\n", file_name);
    }

    if (slashptr == NULL) {
        if (len == 0) {
            loggerPanic("Failed to extract filename\n");
        }
        slashptr = file_name;
    }

    const char *dot_end = strrchr(slashptr, '.');
    int no_ext_len = (dot_end == NULL) ? 0 : dot_end - slashptr;

    opts->infile_no_ext = mprintf("%.*s", no_ext_len, slashptr);
    opts->asm_outfile = mprintf("%s.s", opts->infile_no_ext);
    opts->obj_outfile =  mprintf("%s.o", opts->infile_no_ext);
}

int writeAsmToTmp(aoStr *asmbuf) {
    int fd;
    ssize_t written = 0;
    size_t towrite = 0;
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

void emitFile(aoStr *asmbuf, HccOpts *opts) {
    aoStr *cmd = aoStrNew();
    hccLib lib;
    if (opts->emit_object) {
        writeAsmToTmp(asmbuf);
        aoStrCatPrintf(cmd, "gcc -c %s "CLIBS" %s -o ./%s",
                ASM_TMP_FILE,opts->clibs,opts->obj_outfile);
        safeSystem(cmd->data);
    } else if (opts->asm_outfile && opts->assemble_only) {
        int fd;
        unsigned long flags = O_CREAT|O_RDWR|O_TRUNC;

        if (opts->to_stdout) {
            fd = STDOUT_FILENO;
        } else if (opts->output_filename != NULL) {
            fd = open(opts->output_filename, flags, 0644);
        } else {
            fd = open(opts->asm_outfile, flags, 0644);
        }
        if (fd == -1) {
            loggerPanic("Failed to open '%s' - %s\n",
                opts->asm_outfile, strerror(errno));
        }
        ssize_t written = write(fd,asmbuf->data,asmbuf->len);
        if (written != (ssize_t)asmbuf->len) {
            loggerPanic("Failed to write data expected %ld got %ld\n",
                    (ssize_t)asmbuf->len, written);
        }
        close(fd);
    } else if (opts->emit_dylib) {
        writeAsmToTmp(asmbuf);
        hccLibInit(&lib,opts,opts->lib_name);
        aoStrCatPrintf(cmd, "gcc -fPIC -c %s -o ./%s",
                ASM_TMP_FILE,opts->obj_outfile);
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
        if (opts->run) {
            aoStr *run_cmd = aoStrNew();
#if IS_MACOS
            aoStrCatPrintf(run_cmd,"echo '%s' | gcc -x assembler - "CLIBS" -L"INSTALL_PREFIX"/lib && ./a.out && rm ./a.out",
                    asmbuf->data);
#else
            writeAsmToTmp(asmbuf);
            aoStrCatPrintf(run_cmd,"gcc -L"INSTALL_PREFIX"/lib %s "CLIBS" && ./a.out && rm ./a.out",
                    ASM_TMP_FILE);
#endif
            /* Don't use 'safeSystem' else anything other than a '0' exit 
             * code will cause a panic which is incorrect... This is a bit of 
             * as run, in an ideall world, would not be calling out to gcc */
            system(run_cmd->data);
            aoStrRelease(run_cmd);
            exit(0);
        }

        writeAsmToTmp(asmbuf);

        if (opts->output_filename == NULL) {
            opts->output_filename = "a.out";
        }

        if (opts->clibs) {
            aoStrCatPrintf(cmd, "gcc -L"INSTALL_PREFIX"/lib %s %s "CLIBS" -o %s", 
                    ASM_TMP_FILE,opts->clibs,opts->output_filename);
        } else {
            aoStrCatPrintf(cmd, "gcc -L"INSTALL_PREFIX"/lib %s "CLIBS" -o %s", 
                    ASM_TMP_FILE, opts->output_filename);
        }
        safeSystem(cmd->data);
    }
    if (strnlen(opts->clibs,10) > 1) {
        free(opts->clibs);
    }
    remove(ASM_TMP_FILE);
    aoStrRelease(cmd);
    aoStrRelease(asmbuf);
}

void assemble(HccOpts *opts) {
    aoStr *run_cmd = aoStrNew();

    if (opts->run) {
        ssize_t len = 0;
        char *buffer = lexReadfile(opts->infile,&len);
#if IS_MACOS
        aoStrCatPrintf(run_cmd,"echo '%s' | gcc -x assembler - "CLIBS" -L"INSTALL_PREFIX"/lib && ./a.out && rm ./a.out",
                buffer);
#else
        aoStr asm_buf = {
            .data = buffer,
            .len = len,
            .capacity = len,
        };
        writeAsmToTmp(&asm_buf);
        aoStrCatPrintf(run_cmd,"gcc -L"INSTALL_PREFIX"/lib %s "CLIBS" && ./a.out && rm ./a.out",
                ASM_TMP_FILE);
#endif
        free(buffer);
    } else {
        aoStrCatPrintf(run_cmd, "gcc %s -L"INSTALL_PREFIX"/lib "CLIBS, opts->infile);
    }
    safeSystem(run_cmd->data);
    aoStrRelease(run_cmd);
}

void usage(void) {
    fprintf(stderr,
            "HolyC Compiler 2024. %s UNSTABLE\n"
            "hcc [..OPTIONS] <..file>\n\n"
            "OPTIONS:\n"
            "  -ast       Print the ast and exit\n"
            "  -cfg       Create graphviz control flow graph\n"
            "  -cfg-png   Create graphviz control flow graph as a png\n"
            "  -cfg-svg   Create graphviz control flow graph as a svg\n"
            "  -tokens    Print the tokens and exit\n"
            "  -S         Emit assembly only\n"
            "  -obj       Emit an objectfile\n"
            "  -lib       Emit a dynamic and static library\n"
            "  -clibs     Link c libraries like: -clibs=`-lSDL2 -lxml2 -lcurl...`\n"
            "  -o         Output filename: hcc -o <name> ./<file>.HC\n"
            "  -o-        Output assembly to stdout, only for use with -S\n"
            "  -run       Immediately run the file (not JIT)\n"
            "  -transpile Transpile the code to C, this is best effort\n"
            "  -g         Not implemented\n"
            "  -D<var>    Set a compiler #define (does not accept a value)\n"
            "  --help     Print this message\n",
            cctrlGetVersion());
    exit(1);
}

void parseCliOptions(HccOpts *opts, int argc, char **argv) {
    if (!strncmp(argv[argc-1], "--help",6)) {
        usage();
    }

    char *infile = argv[argc-1];
    char *ptr = NULL;
    char *tmp = NULL;

    getASMFileName(opts,infile);
    opts->infile = infile;
    for (int i = 1; i < argc - 1; ++i) {
        if (!strncmp(argv[i],"-ast",4)) {
            opts->print_ast = 1;
        } else if (!strncmp(argv[i],"-tokens",7)) {
            opts->print_tokens = 1;
        } else if (!strncmp(argv[i],"-clibs",6)) {
            const char *error = "Invalid compile command, -clibs must be followed "
                "by a list of libraries in single quotes for example "
                "-clibs=\'-lxml2 ....\'.";
            ptr = argv[i];
            ptr += 6;
            if (*ptr != '=' && *(ptr + 1) != '\'') {
                loggerPanic("%s got '%c'\n", error, *ptr);
            }
            aoStr *str = aoStrNew();
            ptr++;
            while (*ptr != '\0' && *ptr != '\'') {
                aoStrPutChar(str,*ptr);
                ptr++;
            }
            opts->clibs = aoStrMove(str);
        } else if (!strncmp(argv[i],"--help",6)) {
            usage();
        } else if (!strncmp(argv[i],str_lit("-transpile"))) {
            opts->transpile = 1;
        } else if (!strncmp(argv[i],"-run",4)) {
            opts->run = 1;
        } else if (!strncmp(argv[i],"-lib",4)) {
            if (i+1 >= argc) {
                loggerPanic("Invalid compile command, -lib must be followed "
                        "by a string\n");
            }
            opts->emit_dylib = 1;
            opts->lib_name = argv[i+1];
            i++;
        } else if (!strncmp(argv[i],"-obj",4)) {
            opts->emit_object = 1;
        } else if (!strncmp(argv[i],"-o-",3)) {
            opts->to_stdout = 1;
        } else if (!strncmp(argv[i],"-o",2)) {
            opts->output_filename = argv[i+1];
            i++;
            if (opts->output_filename == infile) {
                fprintf(stderr, "hcc: \033[0;31mfatal error\033[0m: no input files.\n"
                        "Usage: hcc -o <program_name> <file>.HC\n");
                exit(1);
            }
            char *outfile = opts->output_filename;
            char *infile_no_dot = (infile[0] == '.' && infile[1] == '/')
            ? infile+2 : infile;
            if (outfile[0] == '.' && outfile[1] == '/') {
                outfile = opts->output_filename+2;
            }
            if (!strcmp(outfile,infile_no_dot)) {
                fprintf(stderr, "hcc: \033[0;31mfatal error\033[0m: output file"
                        " same name as input file.\n");
                exit(1);
            }
        } else if (!strncmp(argv[i],"-g",2)) {
            opts->asm_debug_comments = 1;
            loggerPanic("--g not implemented\n");
        } else if (!strncmp(argv[i],"-S",2)) {
            opts->assemble_only = 1;
        } else if (!strncmp(argv[i],"-cfg-png",8)) {
            opts->cfg_create_png = 1;
        } else if (!strncmp(argv[i],"-cfg-svg",8)) {
            opts->cfg_create_svg = 1;
        } else if (!strncmp(argv[i],"-cfg",3)) {
            opts->cfg_create = 1;
        } else if (!strncmp(argv[i],"-D",2)) {
            if (opts->defines_list == NULL) {
                opts->defines_list = listNew();
            }
            ptr = argv[i];
            ptr += 2;
            tmp = strndup(ptr,128);
            /*@Leak who owns this memory? This list or the macro_defs hashtable
             * on Cctrl? */
            listAppend(opts->defines_list,tmp);
        }
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "hcc: \033[0;31mfatal error\033[0m: no input files\n"
                "compilation terminated.\n");
        exit(EXIT_FAILURE);
    }
    HccOpts opts;
    int lexer_flags = CCF_PRE_PROC;
    aoStr *asmbuf;
    Cctrl *cc;
    

    memset(&opts,0,sizeof(opts));
    opts.clibs = "";
    opts.defines_list = NULL;
    /* now parse cli options */
    parseCliOptions(&opts,argc,argv);

    opts.install_dir = INSTALL_PREFIX;

    if (opts.assemble) {
        assemble(&opts);
        return 0;
    }

    cc = cctrlNew();
    if (opts.defines_list) {
        cctrlSetCommandLineDefines(cc,opts.defines_list);
    }

    if (opts.print_tokens) {
        compileToTokens(cc,&opts,lexer_flags);
        return 0;
    }

    if (opts.transpile) {
        aoStr *buf = transpileToC(cc,&opts);
        printf("/* This code has been automatically generated by running: \n"
               " * `hcc -transpile %s`\n"
               " * please check for errors! */\n\n"
               "%s",opts.infile, buf->data);
        aoStrRelease(buf);
        return 0;
    }

    compileToAst(cc,&opts,lexer_flags);

    if (opts.print_ast) {
        compilePrintAst(cc);
        return 0;
    }

    if (opts.cfg_create || opts.cfg_create_png || opts.cfg_create_svg) {
        PtrVec *cfgs = cfgConstruct(cc);
        char *dot_outfile = mprintf("./%s.dot",opts.infile_no_ext);
        cfgsToFile(cfgs,dot_outfile);
        if (opts.cfg_create_png || opts.cfg_create_svg) {
            char *ext = opts.cfg_create_png ? "png" : "svg";
            char *dot_cmd = mprintf("dot -T%s %s -o ./%s.%s",
                    ext,dot_outfile,opts.infile_no_ext,ext);
            printf("Creating %s: %s\n",ext,dot_cmd);
            safeSystem(dot_cmd);
            free(dot_cmd);
            unlink(dot_outfile);
        }
        free(dot_outfile);
        return 0;
    }

    asmbuf = compileToAsm(cc);

    emitFile(asmbuf, &opts);
    if (opts.defines_list) {
        listRelease(opts.defines_list,NULL);
    }
}
