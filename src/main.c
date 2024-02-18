#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "aostr.h"
#include "config.h"
#include "compile.h"
#include "cctrl.h"
#include "util.h"

#define ASM_TMP_FILE "/tmp/holyc-asm.s"
#define LIB_PATH "/usr/local/lib"
#define LIB_BUFSIZ 256

typedef struct hccOpts {
    int print_ast;
    int print_tokens;
    int print_help;
    int asm_debug_comments;
    int assemble_only;
    int emit_dylib;
    int emit_object;
    char *infile;
    char *asm_outfile;
    char *obj_outfile;
    char *lib_name;
    char *clibs;
} hccOpts;

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

char *getVersion(void) {
    return "0.0.1";
}

int hccLibInit(hccLib *lib, hccOpts *opts, char *name) { 
    aoStr *dylibcmd = aoStrNew();
    aoStr *stylibcmd = aoStrNew();
    aoStr *installcmd = aoStrNew();

    /*
     *
     *
     *cp -pPR ./tos.dylib /usr/local/lib/tos.0.0.1.dylib
cp -pPR ./tos.a /usr/local/lib/
cp ../../holyc-lang-website/website/lexer.HC ./scripts/
cp ./tos.HH /usr/local/include/
cp ./holyc-lib/tos.HH ~/.holyc-lib/
cp /Users/jamesbarford-evans/.holyc-lib/tos.HH ./holyc-lib/
ln -sf ./tos.0.0.1.dylib ./tos.dylib
     *
     * */
    
#if IS_BSD
    snprintf(lib->stylib_name,LIB_BUFSIZ,"%s.a",name);
    snprintf(lib->dylib_name,LIB_BUFSIZ,"%s.dylib",name);
    snprintf(lib->dylib_version_name,LIB_BUFSIZ,"%s.0.0.1.dylib",name);
    aoStrCatPrintf(
            dylibcmd, "cc -dynamiclib -Wl,-install_name,%s/%s -o %s -lm -lc -lpthread", 
            LIB_PATH, name, lib->dylib_version_name,lib->dylib_name);
#elif IS_LINUX
    snprintf(lib->stylib_name,LIB_BUFSIZ,"%s.a",name);
    snprintf(lib->dylib_name,LIB_BUFSIZ,"%s.so",name);
    snprintf(lib->dylib_minor_name,LIB_BUFSIZ,"%s.so.0.0.1",name);
    aoStrCatPrintf(
            dylibcmd, "cc -shared -Wl,-so_name,%s/%s -o %s -lm -lc -lpthread", 
            LIB_PATH,name,lib->dylib_minor_name,lib->dylib_name);
#else
#error "System not supported"
#endif
    aoStrCatPrintf(installcmd, "cp -pPR ./%s /usr/local/lib/lib%s && "
            "cp -pPR ./%s /usr/local/lib/lib%s && "
            "ln -sf /usr/local/lib/%s /usr/local/lib/%s",
            lib->dylib_name,lib->dylib_version_name,
            lib->stylib_name,lib->stylib_name,
            lib->dylib_version_name,lib->dylib_name);
    aoStrCatPrintf(stylibcmd,"ar rcs %s %s",lib->stylib_name,opts->obj_outfile);
    aoStrCatPrintf(dylibcmd," -o %s %s",lib->dylib_name,opts->obj_outfile);
    lib->install_cmd = aoStrMove(installcmd);
    lib->dylib_cmd = aoStrMove(dylibcmd);
    lib->stylib_cmd = aoStrMove(stylibcmd);
    
    return 1;
}

void getASMFileName(hccOpts *opts, char *file_name) {
    int len = strlen(file_name);
    int i;
    char *slashptr = NULL, *dotptr = NULL,
         *asm_outfile, *obj_outfile; 

    for (i = len -1; i >= 0; --i) {
        if (file_name[i] == '.') {
            dotptr = &file_name[i];
        }

        if (file_name[i] == '/') {
            slashptr = &file_name[i];
            slashptr += 1;
            break;
        }
    }

    fprintf(stderr,"%s\n",file_name);
    if (slashptr == NULL || dotptr == NULL) {
        loggerPanic("Failed to extract filename\n");
    }

    asm_outfile = malloc(sizeof(char) * len+1);
    obj_outfile = malloc(sizeof(char) * len+1);

    memcpy(asm_outfile, slashptr, dotptr-slashptr);
    memcpy(obj_outfile, slashptr, dotptr-slashptr);

    memcpy(asm_outfile+(dotptr-slashptr), ".s", 2);
    memcpy(obj_outfile+(dotptr-slashptr), ".o", 2);

    asm_outfile[dotptr-slashptr+2] = '\0';
    asm_outfile[len] = '\0';

    obj_outfile[dotptr-slashptr+2] = '\0';
    obj_outfile[len] = '\0';
    opts->asm_outfile = asm_outfile;
    opts->obj_outfile = obj_outfile;
}

void execGcc(char *filename, aoStr *asmbuf, aoStr *cmd) {
    printf("%s\n", cmd->data);
    system(cmd->data);
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

void emitFile(aoStr *asmbuf, hccOpts *opts) {
    aoStr *cmd = aoStrNew();
    hccLib lib;
    if (opts->emit_object) {
        writeAsmToTmp(asmbuf);
        aoStrCatPrintf(cmd, "gcc -c %s -lm -lpthread -lc -ltos %s -o ./%s",
                ASM_TMP_FILE,opts->clibs,opts->obj_outfile);
        system(cmd->data);
    } else if (opts->asm_outfile && opts->assemble_only) {
        int fd = open(opts->asm_outfile, O_CREAT|O_RDWR|O_TRUNC, 0666);
        write(fd,asmbuf->data,asmbuf->len);
        close(fd);
    } else if (opts->emit_dylib) {
        writeAsmToTmp(asmbuf);
        hccLibInit(&lib,opts,opts->lib_name);
        aoStrCatPrintf(cmd, "gcc -c %s -fPIC -o ./%s",
                ASM_TMP_FILE,opts->obj_outfile);
        fprintf(stderr,"%s\n",cmd->data);
        system(cmd->data);
        fprintf(stderr,"%s\n",lib.stylib_cmd);
        system(lib.stylib_cmd);
        fprintf(stderr,"%s\n",lib.dylib_cmd);
        system(lib.dylib_cmd);
        fprintf(stderr,"%s\n",lib.install_cmd);
        system(lib.install_cmd);
    } else {
        writeAsmToTmp(asmbuf);
        aoStrCatPrintf(cmd, "gcc -L/usr/local/lib %s -lm -lpthread -lc -ltos %s -o ./a.out", 
                ASM_TMP_FILE,opts->clibs);
        system(cmd->data);
        remove(ASM_TMP_FILE);

    }
    if (strnlen(opts->clibs,10) > 1) {
        free(opts->clibs);
    }
    aoStrRelease(cmd);
    aoStrRelease(asmbuf);
}

void usage(void) {
    fprintf(stderr,
            "HolyC Compiler 2024. UNSTABLE\n"
            "hcc [..OPTIONS] <..file>\n\n"
            "OPTIONS:\n"
            "  -ast     print the ast and exit\n"
            "  -tokens  print the tokens and exit\n"
            "  -S       emit assembly only\n"
            "  -obj     emit an objectfile\n"
            "  -lib     emit a dynamic and static library\n"
            "  -clibs   link c libraries like: -clibs=`-lSDL2 -lxml2 -lcurl...`\n"
            "  -g       add comments to assembly\n"
            "  --help   print this message\n");
    exit(1);
}

void parseCliOptions(hccOpts *opts, int argc, char **argv) {
    if (!strncmp(argv[argc-1], "--help",6)) {
        usage();
    }

    char *infile = argv[argc-1];
    getASMFileName(opts,infile);
    opts->infile = infile;
    for (int i = 1; i < argc - 1; ++i) {
        if (!strncmp(argv[i],"-ast",4)) {
            opts->print_ast = 1;
        } else if (!strncmp(argv[i],"-tokens",7)) {
            opts->print_tokens = 1;
        } else if (!strncmp(argv[i],"-S",2)) {
            opts->assemble_only = 1;
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
        } else if (!strncmp(argv[i],"-clibs",6)) {
            const char *error = "Invalid compile command, -clibs must be followed "
                "by a list of libraries in single quotes for example "
                "-clibs=\'-lxml2 -lsqlite3 ....\'.";
            char *ptr = argv[i];
            ptr += 6;
            loggerDebug("%s\n",ptr);
            if (*ptr != '=' && *(ptr + 1) != '\'') {
                loggerPanic("%s got '%c'\n", error, *ptr);
            }
            aoStr *str = aoStrNew();
            ptr++;
            loggerDebug("%s\n",ptr);
            while (*ptr != '\0' && *ptr != '\'') {
                aoStrPutChar(str,*ptr);
                ptr++;
            }
            opts->clibs = aoStrMove(str);
        } else if (!strncmp(argv[i],"-g",2)) {
            opts->asm_debug_comments = 1;
            loggerPanic("--g not implemented\n");
        } else if (!strncmp(argv[i],"--help",6)) {
            usage();
        }
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "hcc: \033[0;31mfatal error\033[0m: no input files\n"
                "compilation terminated.\n");
        exit(EXIT_FAILURE);
    }
    hccOpts opts;

    aoStr *asmbuf;
    Cctrl *cc;
    

    memset(&opts,0,sizeof(opts));
    opts.clibs = "";
    /* now parse cli options */
    parseCliOptions(&opts,argc,argv);

    cc = CctrlNew();
    asmbuf = CompileFile(cc,opts.infile);

    if (opts.print_tokens) {
        CompilePrintTokens(cc);
    }

    if (opts.print_ast) {
        CompilePrintAst(cc);
    }

    emitFile(asmbuf, &opts);
}
