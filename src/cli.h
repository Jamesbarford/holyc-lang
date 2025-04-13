#ifndef CLI_H
#define CLI_H

#include <stddef.h>
#include <stdint.h>

#include "list.h"

enum CliType {
    CLI_STRING,
    CLI_INT,
    CLI_FLAG,
};

enum CliArgType {
    CLI_PRINT_AST,
    CLI_PRINT_TOKENS,
    CLI_CFG_CREATE,
    CLI_CFG_CREATE_PNG,
    CLI_CFG_CREATE_SVG,
    CLI_ASM_DEBUG_COMMENTS,
    CLI_ASSEMBLE_ONLY,
    CLI_EMIT_DYLIB,
    CLI_EMIT_OBJECT,
    CLI_RUN,
    CLI_DUMP_IR,
    CLI_EMIT_TYPE,
    CLI_ASSEMBLE,
    CLI_TRANSPILE,
    CLI_TO_STDOUT,
    CLI_OUTPUT_FILENAME,
    CLI_CLIBS,
    CLI_DEFINES_LIST,
    CLI_VERSION,
    CLI_MEM_STATS,
    CLI_HELP,
    CLI_TERRY,
    CLI_OPTIMISE,
};

typedef struct CliValue {
    enum CliType type;
    union {
        char *str;
        int integer;
        int boolean;
    };
} CliValue;

typedef struct CliParser {
    char *optname;
    size_t optlen;
    int arg_count;
    enum CliArgType arg_type; 
    char *usage;
    char *help;
    int (*parse)(struct CliValue *value, char *rawarg);
} CliParser;

enum CliEmitType {
    CLI_EMIT_X86_64 = 1,
    CLI_EMIT_AARCH64,
    CLI_EMIT_IR,
    CLI_EMIT_AST,
    CLI_EMIT_C,
};

typedef struct CliArgs {
    int print_ast;
    int print_tokens;
    int print_help;
    int print_mem_stats;
    int cfg_create;
    int cfg_create_png;
    int cfg_create_svg;
    int asm_debug_comments;
    int dump_ir;
    int assemble_only;
    enum CliEmitType emit_type;
    int emit_dylib;
    int emit_object;
    int run;
    int assemble;
    int transpile;
    int to_stdout;
    int optimise;
    char *infile;
    char *infile_no_ext;
    char *asm_outfile;
    char *obj_outfile;
    char *lib_name;
    char *output_filename;
    char *clibs;
    char *install_dir;
    List *defines_list;
} CliArgs;

void cliArgsInit(CliArgs *args);
int cliParseArgs(struct CliArgs *cli_args, int argc, char **argv);
__noreturn void cliPanicGeneric(const char *const_msg, const char *fmt, va_list ap);

#endif
