#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "aostr.h"
#include "compile.h"
#include "cctrl.h"
#include "util.h"

#define ASM_TMP_FILE "/tmp/holyc-asm.s"

typedef struct mccOptions {
    int print_ast;
    int print_tokens;
    int print_help;
    int asm_debug_comments;
    int assemble_only;
    int emit_object;
    char *infile;
    char *asm_outfile;
    char *obj_outfile;
} mccOptions;

void getASMFileName(mccOptions *opts, char *file_name) {
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

void emitFile(aoStr *asmbuf, mccOptions *opts) {
    aoStr *cmd = aoStrNew();

    if (opts->emit_object) {
        writeAsmToTmp(asmbuf);
        aoStrCatPrintf(cmd, "gcc -c %s -lm -lc -o ./%s",
                ASM_TMP_FILE,opts->obj_outfile);
        system(cmd->data);
    } else if (opts->asm_outfile && opts->assemble_only) {
        int fd = open(opts->asm_outfile, O_CREAT|O_RDWR|O_TRUNC, 0666);
        write(fd,asmbuf->data,asmbuf->len);
        close(fd);
    } else {
        writeAsmToTmp(asmbuf);
        aoStrCatPrintf(cmd, "gcc %s -lm -lc -o ./a.out", ASM_TMP_FILE);
        system(cmd->data);
        remove(ASM_TMP_FILE);

    }
    aoStrRelease(cmd);
    aoStrRelease(asmbuf);
}

void usage(void) {
    fprintf(stderr,
            "HolyC Compiler 2024. UNSTABLE\n"
            "hcc [..OPTIONS] <..file>\n\n"
            "OPTIONS:\n"
            "  -ast    print the ast and exit\n"
            "  -tokens print the tokens and exit\n"
            "  -S      emit assembly only\n"
            "  -o      emit an objectfile\n"
            "  -g      add comments to assembly\n"
            "  --help   print this message\n");
    exit(1);
}

void parseCliOptions(mccOptions *opts, int argc, char **argv) {
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
        } else if (!strncmp(argv[i],"-o",2)) {
            opts->emit_object = 1;
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
    mccOptions opts;

    aoStr *asmbuf;
    Cctrl *cc;
    

    memset(&opts,0,sizeof(opts));
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
