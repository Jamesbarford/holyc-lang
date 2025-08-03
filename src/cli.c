#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "aostr.h"
#include "config.h"
#include "cli.h"
#include "util.h"
#include "version.h"

#define BUFFER_INITIAL_CAPACITY 1024
#define LINELEN_DEFAULT 80


char *cliParseAssign(char *arg) {
    while (*arg != '=') arg++;
    return arg;
}

int cliParseString(CliValue *value, char *rawarg) {
    value->str = rawarg;
    value->type = CLI_STRING;
    return 1;
}

int cliParseDefine(CliValue *value, char *rawarg) {
    rawarg += 2; /* move past -D */
    value->type = CLI_STRING;
    value->str = mprintf("%s",rawarg);
    return 1;
}

int cliParseInt(CliValue *value, char *rawarg) {
    char *endptr = NULL;
    /* This program makes no sense if the argument passed it is greater than 
     * INT_MAX or <= `0` */
    value->integer = (int)strtol(rawarg, &endptr, 10);
    if (value->integer <= 0) {
        return 0;
    }
    value->type = CLI_INT;
    if (*endptr != 0) {
        return 0;
    }
    return 1;
}

int cliParseNop(CliValue *value, char *rawarg) {
    (void)value;
    (void)rawarg;
    return 1;
}

int cliParseBoolean(CliValue *value, char *rawarg) {
    int retval = 0;
    value->type = CLI_FLAG;

    if (!strncmp(rawarg,str_lit("true")) || !strncmp(rawarg,str_lit("1"))) {
        value->boolean = 1;
        retval = 1;
    } else if (!strncmp(rawarg,str_lit("false")) || !strncmp(rawarg,str_lit("0"))) {
        value->boolean = 0;
        retval = 1;
    }
    return retval;
}

static CliParser parsers[] = {
    {str_lit("-ast"),       0, CLI_PRINT_AST, "-ast", "Print the ast and exit", &cliParseNop},
    {str_lit("-cfg"),       0, CLI_CFG_CREATE, "-cfg", "Create graphviz control flow graph", &cliParseNop},
    {str_lit("-cfg-png"),   0, CLI_CFG_CREATE_PNG, "-cfg-svg", "Create graphviz control flow graph as a PNG", &cliParseNop},
    {str_lit("-cfg-svg"),   0, CLI_CFG_CREATE_SVG, "-cfg-png", "Create graphviz control flow graph as a SVG", &cliParseNop},
    {str_lit("-tokens"),    0, CLI_PRINT_TOKENS, "-tokens", "Print the tokens and exit", &cliParseNop},
    {str_lit("-S"),         0, CLI_ASSEMBLE_ONLY, "-S", "Emit assembly only", &cliParseNop},
    {str_lit("-obj"),       0, CLI_EMIT_OBJECT, "-obj", "Emit an objectfile", &cliParseNop},
    {str_lit("-lib"),       1, CLI_EMIT_DYLIB, "-lib <libname>", "Emit a dynamic and static library: `-lib <libname>`", &cliParseString},
    {str_lit("-clibs"),     0, CLI_CLIBS, "-clibs", "Link c libraries like: -clibs=`-lSDL2 -lxml2 -lcurl...`", &cliParseNop},
    {str_lit("-run"),       0, CLI_RUN, "-run", "Immediately run the file (not JIT)", &cliParseNop},
    {str_lit("-o"),         1, CLI_OUTPUT_FILENAME, "-o <binary_name>", "Output filename: `-o <name> ./<file>.HC`", &cliParseString},
    {str_lit("-o-"),        0, CLI_TO_STDOUT, "-o-", "Output assembly to stdout, only for use with -S", &cliParseNop},
    {str_lit("-transpile"), 0, CLI_TRANSPILE, "-transpile", "Transpile the code to C, this is best effort", &cliParseNop},
    {str_lit("-D"),         0, CLI_DEFINES_LIST, "-D<VAR>", "Set a compiler #define (does not accept a value)", &cliParseDefine},
    {str_lit("--dump-ir"),  0, CLI_DUMP_IR, "--dump-ir", "Dump ir to stdout" , &cliParseNop},
    {str_lit("--mem-stats"),  0, CLI_MEM_STATS, "--mem-stats", "Stats about memory usage when compiling" , &cliParseNop},
    {str_lit("--version"),  0, CLI_VERSION, "--version", "Print the version of the compiler", &cliParseNop},
    {str_lit("--help"),     0, CLI_HELP, "--help", "Print this message", &cliParseNop},
    {str_lit("--terry"),    0, CLI_TERRY, "--terry", "Information about Terry A. Davis", &cliParseNop},
};

static u64 longestCommand(void) {
    int commands_len = (int)(sizeof(parsers)/sizeof(parsers[0]));
    u64 max_len = 0;
    for (int i = 0; i < commands_len; ++i) {
        CliParser *parser = &parsers[i];
        u64 len = strlen(parser->optname);
        if (len > max_len) {
            max_len = len;
        }
    }
    return max_len;
}

AoStr *gitGetHash(void) {
#ifdef HCC_GIT_HASH
    return aoStrDupRaw(str_lit(HCC_GIT_HASH));
#else
    return aoStrPrintf("git hash retrival failed");
#endif
}

const char *getBuildModeStr(void) {
#if defined(DEBUG)
    return "Debug";
#else
    return "Release";
#endif
}

/* For creating error messages that can't take advantage of the nicer cctrl one */
__noreturn void cliPanicGeneric(const char *const_msg, const char *fmt, va_list ap) {
    char buffer[BUFSIZ];
    u64 len = vsnprintf(buffer, sizeof(buffer), fmt, ap);
    buffer[len] = '\0';
    if (const_msg) {
        if (is_terminal) {
            fprintf(stderr, "\033[1;91m%s\033[0m", const_msg);
        } else {
            fprintf(stderr, "%s", const_msg);
        }
    }
    fprintf(stderr, "%s", buffer);
    va_end(ap);
    exit(EXIT_FAILURE);
}

__noreturn void cliPanic(const char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    cliPanicGeneric("Error: ", fmt, ap);
}

__noreturn void cliVersionPrint(CliArgs *args) {
    AoStr *git_hash = gitGetHash();
    AoStr *buffer = aoStrNew();
    if (is_terminal) {
        aoStrCatFmt(buffer, "\033[1;1mhcc\033[0m %s\n", cctrlGetVersion());
    } else {
        aoStrCatFmt(buffer, "hcc %s\n", cctrlGetVersion());
    }
    aoStrCatFmt(buffer,
            "binary: %s/hcc\n"
            "commit-hash: %s\n"
            "arch: %s %s\n"
            "build: %s\n",
            args->install_dir,
            git_hash->data,
            OS_STR,
            ARCH_STR,
            getBuildModeStr());

    printf("%s",buffer->data);
    aoStrRelease(git_hash);
    aoStrRelease(buffer);
    exit(EXIT_SUCCESS);
}

__noreturn void cliTerryInfo(void) {
    if (is_terminal) {
        fprintf(stderr, "\033[1;1mTerry A. Davis\033[0m\n");
    } else {
        fprintf(stderr, "Terry A. Davis\n");
    }
    fprintf(stderr, "1969 - 2018\n"
                    "Terry was an electrical engineer and computer programmer who created TempleOS\n"
                    "as well as the HolyC programming language. More can be read about Terry and\n"
                    "his life here https://en.wikipedia.org/wiki/Terry_A._Davis\n");
    exit(EXIT_SUCCESS);
}

__noreturn void cliNoInputFiles(void) {
    if (is_terminal) {
        fprintf(stderr, "\033[1;1mhcc\033[0m: "ESC_BOLD_RED"fatal error\033[0m: no input files\n"
                "compilation terminated.\n");
    } else {
        fprintf(stderr, "hcc: fatal error: no input files\n"
                "compilation terminated.\n");
    }
    exit(EXIT_FAILURE);
}

__noreturn void cliPrintUsage(void) {
    int commands_len = (int)(sizeof(parsers)/sizeof(parsers[0]));
    u64 longest = longestCommand() + 4;
    AoStr *buffer = aoStrNew();

    if (is_terminal) {
        aoStrCatFmt(buffer,"\033[1;1mhcc - HolyC Compiler %s\033[0m\n", cctrlGetVersion());
    } else {
        aoStrCatFmt(buffer,"hcc - HolyC Compiler %s\n", cctrlGetVersion());
    }
    aoStrCatFmt(buffer, "Compile .HC files for documentation please see here - https://holyc-lang.com/docs/intro\n\n");

    if (is_terminal) {
        aoStrCatFmt(buffer, "\033[1;1mUSAGE:\033[0m\n");
    } else {
        aoStrCatFmt(buffer, "USAGE:\n");
    }
    aoStrCatFmt(buffer, "    hcc [OPTIONS] file\n\n");

    if (is_terminal) {
        aoStrCatFmt(buffer, "\033[1;1mOPTIONS:\033[0m\n");
    } else {
        aoStrCatFmt(buffer, "OPTIONS:\n");
    }

    for (int i = 0; i < commands_len; ++i) {
        CliParser *parser = &parsers[i];

        if (is_terminal) {
            aoStrCatFmt(buffer, "    \033[1;32m%s\033[0m", parser->optname);
        } else {
            aoStrCatFmt(buffer, "    %s", parser->optname);
        }

        /* Making the spacing uniform */
        if (parser->optlen < longest) {
            for (u64 optlen = parser->optlen; optlen < longest; ++optlen) {
                aoStrPutChar(buffer, ' '); 
            }
        }

        aoStrCatFmt(buffer, "%s\n", parser->help);
    }
    printf("%s\n", buffer->data);
    aoStrRelease(buffer);
    exit(EXIT_SUCCESS);
}

CliParser *cliParserFind(char *arg, u64 arg_len) {
    int len = (int)(sizeof(parsers)/sizeof(parsers[0]));

    for (int i = 0; i < len; ++i) {
        CliParser *parser = &parsers[i];

        /* Exact match */
        if (arg_len == parser->optlen &&
            !strncmp(parser->optname, arg, arg_len))
        {
            return parser;
        }

        /* For something like `--file=<arg>` where we want to check there is a 
         * '=' and that the `--file` part matches */
        if (arg_len > parser->optlen) {
            if (arg[parser->optlen] == '=' && 
                !strncmp(parser->optname, arg, parser->optlen)) {
                return parser;
            } else if (!strncmp("-D", arg, 2) && !strncmp("-D", parser->optname, 2)) {
                return parser;
            }
        }
    }
    return NULL;
}

enum CliFileType {
    HC_INVALID,
    HC_SOURCE,
    HC_HEADER,
    HC_ASSEMBLY,
};

enum CliFileType cliGetFileType(char *filename, u64 filename_len) {
    char *end = &filename[filename_len-1];
    if (tolower(*end) == 'c' && tolower(*(end-1)) == 'h' && *(end-2) == '.') {
        return HC_SOURCE;
    } else if (tolower(*end) == 'h' && tolower(*(end-1)) == 'h' && *(end-2) == '.') {
        return HC_HEADER;
    } else if (tolower(*end) == 's' && *(end-1) == '.') {
        return HC_ASSEMBLY;
    } else {
        return HC_INVALID;
    }
}

void getASMFileName(CliArgs *args, enum CliFileType file_type, char *filename, u64 filename_len) {
    switch (file_type) {
        case HC_INVALID:
            cliPanic("Unknown file extension, file must end with .HC, .HH or .s case insensitive. Got: %s\n", filename);
            break;
        case HC_SOURCE:
        case HC_HEADER:
            break;
        case HC_ASSEMBLY:
        args->assemble = 1;
            break;
    }

    char *slashptr = NULL;
    for (int i = filename_len -1; i >= 0; --i) {
        if (filename[i] == '/') {
            slashptr = &filename[i];
            slashptr += 1;
            break;
        }
    }

    if (slashptr == NULL) {
        if (filename_len == 0) {
            cliPanic("Failed to extract filename\n");
        }
        slashptr = filename;
    }

    const char *dot_end = strrchr(slashptr, '.');
    int no_ext_len = (dot_end == NULL) ? 0 : dot_end - slashptr;

    args->infile = mprintf("%s", filename);
    args->infile_no_ext = mprintf("%.*s", no_ext_len, slashptr);
    args->asm_outfile = mprintf("%s.s", args->infile_no_ext);
    args->obj_outfile =  mprintf("%s.o", args->infile_no_ext);
}

int cliParseArgs(CliArgs *args, int argc, char **argv) {
    /* move past the program name */
    argc--;
    argv++;

    CliValue value;
    CliParser *parser = NULL;

    for (int i = 0; i < argc; i++) {
        char *arg = argv[i];
        u64 arg_len = strlen(arg);
        char *next_arg = i + 1 < argc ? argv[i+1] : NULL;
        char *arg_to_parse = NULL;

        if ((parser = cliParserFind(arg, arg_len)) == NULL) {
            /* Are we looking at a source file? */
            enum CliFileType file_type = cliGetFileType(arg, arg_len);
            if (file_type != HC_INVALID) {
                getASMFileName(args, file_type, arg, arg_len);
                continue;
            } else {
                cliPanic("Unknown cli option `%s`\n", arg);
            }
        }

        if (*(arg + parser->optlen) == '=') {
            arg = arg + parser->optlen + 1;
            arg_to_parse = arg;
        } else if (parser->arg_count > 0) {
            if (!next_arg && parser->parse != cliParseNop) {
                cliPanic("Unexpected end of input - expected `%s <option?>`\n",
                      parser->optname);
            }
            arg_to_parse = next_arg;
            i++;
        } else {
            arg_to_parse = arg;
        }

        if (!parser->parse(&value, arg_to_parse)) {
            cliPanic("Failed to parse option for `%s` got `%s`\n",
                  parser->usage,
                  arg_to_parse);
        }

        switch (parser->arg_type) {
            case CLI_PRINT_AST:          args->print_ast = 1; break;
            case CLI_PRINT_TOKENS:       args->print_tokens = 1; break;
            case CLI_CFG_CREATE:         args->cfg_create = 1; break;
            case CLI_CFG_CREATE_PNG:     args->cfg_create_png = 1; break;
            case CLI_CFG_CREATE_SVG:     args->cfg_create_svg = 1; break;
            case CLI_ASM_DEBUG_COMMENTS: args->asm_debug_comments = 1; break;
            case CLI_ASSEMBLE_ONLY:      args->assemble_only = 1; break;
            case CLI_EMIT_DYLIB: {
                args->emit_dylib = 1;
                args->lib_name = mprintf("%s",value.str);
                break;
            }
            case CLI_EMIT_OBJECT:        args->emit_object = 1; break;
            case CLI_RUN:                args->run = 1; break;
            case CLI_ASSEMBLE:           args->assemble = 1; break;
            case CLI_TRANSPILE:          args->transpile = 1; break;
            case CLI_TO_STDOUT:          args->to_stdout = 1; break;
            case CLI_OUTPUT_FILENAME:    args->output_filename = mprintf("%s", value.str); break;
            case CLI_CLIBS:              args->clibs = mprintf("%s", value.str); break;
            case CLI_DEFINES_LIST: {
                /* @Leak who owns this memory? This list or the macro_defs hashtable
                 * on Cctrl? */
                if (args->defines_list == NULL) {
                    args->defines_list = listNew();
                }
                listAppend(args->defines_list, mprintf("%s", value.str));
                break;
            }
            case CLI_DUMP_IR:   args->dump_ir = 1; break;
            case CLI_MEM_STATS: args->print_mem_stats = 1; break;
            case CLI_HELP:    cliPrintUsage(); break;
            case CLI_VERSION: cliVersionPrint(args); break;
            case CLI_TERRY:   cliTerryInfo(); break;
        }
    }

    if (args->to_stdout && !args->assemble_only) {
        cliPanic("`-o-` can only be used in conjunction with `-S` for "
                 "printing assembly to stdout\n");
    }

    if (args->infile == NULL) {
        cliNoInputFiles();
    }
    return 1;
}

void cliArgsInit(CliArgs *args) {
    memset(args,0,sizeof(CliArgs));
    args->clibs = "";
    args->defines_list = NULL;
}
