#ifndef CLI_H
#define CLI_H

#include <stdint.h>
#include <stdarg.h>

#include "config.h"
#include "list.h"
#include "types.h"

enum CliTarget {
    TARGET_AARCH64_APPLE_DARWIN,
    TARGET_AARCH64_UNKNOWN_LINUX_GNU,
    TARGET_X86_64_APPLE_DARWIN,
    TARGET_X86_64_UNKNOWN_LINUX_GNU,
};

enum CliType {
    CLI_STRING,
    CLI_INT,
    CLI_FLAG,
};

enum CliArgType {
    CLI_ASM_DEBUG_COMMENTS,
    CLI_ASSEMBLE,
    CLI_ASSEMBLE_ONLY,
    CLI_CFG_CREATE,
    CLI_CFG_CREATE_PNG,
    CLI_CFG_CREATE_SVG,
    CLI_CLIBS,
    CLI_DEFINES_LIST,
    CLI_DUMP_IR,
    CLI_EMIT_DYLIB,
    CLI_EMIT_OBJECT,
    CLI_HELP,
    CLI_INSTALL_DIR,
    CLI_JIT,
    CLI_MEM_STATS,
    CLI_OUTPUT_FILENAME,
    CLI_PRINT_AST,
    CLI_PRINT_TOKENS,
    CLI_RUN,
    CLI_TARGET,
    CLI_TERRY,
    CLI_TO_STDOUT,
    CLI_TRANSPILE,
    CLI_USE_LEGACY_X86,
    CLI_VERSION,
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
    u64 optlen;
    int arg_count;
    enum CliArgType arg_type; 
    char *usage;
    char *help;
    int (*parse)(struct CliValue *value, char *rawarg);
} CliParser;

typedef struct CliArgs {
    int print_ast;
    int print_tokens;
    int print_help;
    int print_mem_stats;
    int cfg_create;
    int cfg_create_png;
    int cfg_create_svg;
    int dump_ir;
    int asm_debug_comments;
    int assemble_only;
    int emit_dylib;
    int emit_object;
    int run;
    int jit;
    int assemble;
    int transpile;
    int to_stdout;
    int use_legacy_x86;
    char *infile;
    char *infile_no_ext;
    char *asm_outfile;
    char *obj_outfile;
    char *lib_name;
    char *output_filename;
    char *clibs;
    char *install_dir;
    enum CliTarget target;
    List *defines_list;
} CliArgs;

void cliArgsInit(CliArgs *args);
int cliParseArgs(struct CliArgs *cli_args, int argc, char **argv);
__noreturn void cliPanicGeneric(const char *const_msg, const char *fmt, va_list ap);

const char *cliTargetToString(enum CliTarget target);

#endif
