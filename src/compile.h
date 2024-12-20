#ifndef COMPILE_H
#define COMPILE_H

#include "aostr.h"
#include "cctrl.h"

typedef struct HccOpts {
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
    int assemble;
    int transpile;
    int to_stdout;
    char *infile;
    char *infile_no_ext;
    char *asm_outfile;
    char *obj_outfile;
    char *lib_name;
    char *output_filename;
    char *clibs;
    char *install_dir;
    List *defines_list;
} HccOpts;

int compileToAst(Cctrl *cc, HccOpts *opts, int lexer_flags);
void compileToTokens(Cctrl *cc, HccOpts *opts, int lexer_flags);

aoStr *compileToAsm(Cctrl *cc);
void compileAssembleToFile(aoStr *asmbuf, char *filename);
void compilePrintAst(Cctrl *cc);


#endif // !COMPILE_H
