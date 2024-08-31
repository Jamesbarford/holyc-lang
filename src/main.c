#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "aostr.h"
#include "cfg.h"
#include "cfg-print.h"
#include "config.h"
#include "compile.h"
#include "cctrl.h"
#include "lexer.h"
#include "list.h"
#include "util.h"

#define ASM_TMP_FILE "/tmp/holyc-asm.s"
#define LIB_PATH "/usr/local/lib"
#define LIB_BUFSIZ 256

typedef struct hccOpts {
    int print_ast;
    int print_tokens;
    int print_help;
    int cfg_create;
    int cfg_create_png;
    int cfg_create_svg;
    int asm_debug_comments;
    int assemble_only;
    int emit_dylib;
    int emit_object;
    int run;
    char *infile;
    char *infile_no_ext;
    char *asm_outfile;
    char *obj_outfile;
    char *lib_name;
    char *output_filename;
    char *clibs;
    List *defines_list;
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
    
#if IS_BSD
    snprintf(lib->stylib_name,LIB_BUFSIZ,"%s.a",name);
    snprintf(lib->dylib_name,LIB_BUFSIZ,"%s.dylib",name);
    snprintf(lib->dylib_version_name,LIB_BUFSIZ,"%s.0.0.1.dylib",name);
    aoStrCatPrintf(
            dylibcmd, "cc -dynamiclib -Wl,-install_name,%s/%s -o %s -lpthread -lsqlite3 -lc -lm",
            LIB_PATH, name, lib->dylib_version_name,lib->dylib_name);
    aoStrCatPrintf(installcmd,
            "cp -pPR ./%s /usr/local/lib/lib%s && "
            "cp -pPR ./%s /usr/local/lib/lib%s && "
            "ln -sf /usr/local/lib/%s /usr/local/lib/%s",
            lib->dylib_name,lib->dylib_version_name,
            lib->stylib_name,lib->stylib_name,
            lib->dylib_version_name,lib->dylib_name);
#elif IS_LINUX
    snprintf(lib->stylib_name,LIB_BUFSIZ,"%s.a",name);
    snprintf(lib->dylib_name,LIB_BUFSIZ,"%s.so",name);
    snprintf(lib->dylib_version_name,LIB_BUFSIZ,"%s.so.0.0.1",name);
    aoStrCatPrintf(
            dylibcmd, "gcc -fPIC -shared -Wl,-soname,%s/%s -o %s -lpthread -lsqlite3 -lc -lm",
            LIB_PATH,name,lib->dylib_name,lib->dylib_name);
    aoStrCatPrintf(installcmd,
            "cp -pPR ./%s /usr/local/lib/lib%s",
            lib->stylib_name,lib->stylib_name);
#else
#error "System not supported"
#endif
    aoStrCatPrintf(stylibcmd,"ar rcs %s %s",lib->stylib_name,opts->obj_outfile);
    aoStrCatPrintf(dylibcmd," -o %s %s",lib->dylib_name,opts->obj_outfile);
    lib->install_cmd = aoStrMove(installcmd);
    lib->dylib_cmd = aoStrMove(dylibcmd);
    lib->stylib_cmd = aoStrMove(stylibcmd);
    
    return 1;
}

void getASMFileName(hccOpts *opts, char *file_name) {
    int len = strlen(file_name);
    int no_ext_len = 0;
    int i;
    char *slashptr = NULL, *asm_outfile, *obj_outfile, *end, *infile_no_ext; 

    if ((file_name[0] != '.' && file_name[1] != '/') && 
         file_name[0] != '/' && file_name[0] != '~') {
        end = 0;
        slashptr = 0;
    } else {
        end = &file_name[len-1];
        for (i = len -1; i >= 0; --i) {
            if (file_name[i] == '/') {
                slashptr = &file_name[i];
                slashptr += 1;
                break;
            }
        }

        if (tolower(*end) == 'c' && tolower(*(end-1)) == 'h' && 
                *(end-2) == '.') {
            end -= 2;
        } else if (tolower(*end) == 'c' && tolower(*(end-1)) == 'h' && 
                *(end-2) == '.') {
            end -= 2;
        } else {
            loggerPanic("Unknown file extension, file must end with .HC or .HH case insensitive. Got: %s\n", file_name);
        }

        if (slashptr == NULL) {
            loggerPanic("Failed to extract filename\n");
        }
    }

    asm_outfile = malloc(sizeof(char) * len+1);
    obj_outfile = malloc(sizeof(char) * len+1);
    infile_no_ext = malloc(sizeof(char) * len);
    no_ext_len = end-slashptr;

    memcpy(asm_outfile,   slashptr, no_ext_len);
    memcpy(obj_outfile,   slashptr, no_ext_len);
    memcpy(infile_no_ext, slashptr, no_ext_len);

    memcpy(asm_outfile+(no_ext_len), ".s", 2);
    memcpy(obj_outfile+(no_ext_len), ".o", 2);

    asm_outfile[no_ext_len+2] = '\0';
    obj_outfile[no_ext_len+2] = '\0';
    infile_no_ext[no_ext_len] = '\0';

    opts->asm_outfile = asm_outfile;
    opts->obj_outfile = obj_outfile;
    opts->infile_no_ext = infile_no_ext;
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
        aoStrCatPrintf(cmd, "gcc -c %s -lpthread -ltos -lsqlite3 -lc -lm %s -o ./%s",
                ASM_TMP_FILE,opts->clibs,opts->obj_outfile);
        system(cmd->data);
    } else if (opts->asm_outfile && opts->assemble_only) {
        int fd = open(opts->asm_outfile, O_CREAT|O_RDWR|O_TRUNC, 0666);
        write(fd,asmbuf->data,asmbuf->len);
        close(fd);
    } else if (opts->emit_dylib) {
        writeAsmToTmp(asmbuf);
        hccLibInit(&lib,opts,opts->lib_name);
        aoStrCatPrintf(cmd, "gcc -fPIC -c %s -o ./%s",
                ASM_TMP_FILE,opts->obj_outfile);
        fprintf(stderr,"%s\n",cmd->data);
        system(cmd->data);
        fprintf(stderr,"%s\n",lib.stylib_cmd);
        system(lib.stylib_cmd);

#if IS_MACOS
        fprintf(stderr,"%s\n",lib.dylib_cmd);
        system(lib.dylib_cmd);
#endif /* ifdef  IS_MACOS */

        fprintf(stderr,"%s\n",lib.install_cmd);
        system(lib.install_cmd);
    } else {
        writeAsmToTmp(asmbuf);
        if (opts->clibs) {
            aoStrCatPrintf(cmd, "gcc -L/usr/local/lib %s -lpthread -ltos -lsqlite3 %s -lc -lm -o %s", 
                    ASM_TMP_FILE,opts->clibs,opts->output_filename);
        } else {
            aoStrCatPrintf(cmd, "gcc -L/usr/local/lib %s -lpthread -ltos -lsqlite3 -lc -lm -o %s", 
                    ASM_TMP_FILE, opts->output_filename);
        }
        system(cmd->data);
        if (opts->run) {
            char run_cmd[64];
            snprintf(run_cmd,sizeof(run_cmd),"./%s",opts->output_filename);
            system(run_cmd);
            //unlink(opts->output_filename);
        }
    }
    if (strnlen(opts->clibs,10) > 1) {
        free(opts->clibs);
    }
    //remove(ASM_TMP_FILE);
    aoStrRelease(cmd);
    aoStrRelease(asmbuf);
}

void usage(void) {
    fprintf(stderr,
            "HolyC Compiler 2024. UNSTABLE\n"
            "hcc [..OPTIONS] <..file>\n\n"
            "OPTIONS:\n"
            "  -ast     Print the ast and exit\n"
            "  -cfg     Create graphviz control flow graph\n"
            "  -cfg-png Create graphviz control flow graph as a png\n"
            "  -cfg-svg Create graphviz control flow graph as a svg\n"
            "  -tokens  Print the tokens and exit\n"
            "  -S       Emit assembly only\n"
            "  -obj     Emit an objectfile\n"
            "  -lib     Emit a dynamic and static library\n"
            "  -clibs   Link c libraries like: -clibs=`-lSDL2 -lxml2 -lcurl...`\n"
            "  -o       Output filename: hcc -o <name> ./<file>.HC\n"
            "  -run     Immediately run the file (not JIT)\n"
            "  -g       Not implemented\n"
            "  -D<var>  Set a compiler #define (does not accept a value)\n"
            "  --help   Print this message\n");
    exit(1);
}

void parseCliOptions(hccOpts *opts, int argc, char **argv) {
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
    hccOpts opts;
    int lexer_flags = CCF_PRE_PROC;
    aoStr *asmbuf;
    Cctrl *cc;
    

    memset(&opts,0,sizeof(opts));
    opts.clibs = "";
    opts.defines_list = NULL;
    opts.output_filename = "a.out";
    /* now parse cli options */
    parseCliOptions(&opts,argc,argv);

    cc = cctrlNew();
    if (opts.defines_list) {
        cctrlSetCommandLineDefines(cc,opts.defines_list);
    }

    if (opts.print_tokens) {
        List *tokens = compileToTokens(cc,opts.infile,lexer_flags);
        lexemePrintList(tokens);
        lexemelistRelease(tokens);
        return 0;
    }

    compileToAst(cc,opts.infile,lexer_flags);

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
            system(dot_cmd);
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
