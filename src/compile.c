#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "cli.h"
#include "compile.h"
#include "lexer.h"
#include "list.h"
#include "memory.h"
#include "parser.h"
#include "util.h"
#include "x86.h"

void compilePrintAst(const Cctrl *cc) {

    printf("AST: \n");

    const List *it = cc->ast_list->next;
    while (it != cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast == NULL) {
            printf("here\n");
        } else {
            if (ast->kind != AST_FUN_PROTO && ast->kind != AST_EXTERN_FUNC &&
                ast->kind != AST_ASM_FUNC_BIND) {
                const char *tmp = astToString(ast);
                printf("%s\n", tmp);
            }
        }

        it = it->next;
    }
}

aoStr *compileToAsm(Cctrl *cc) {
    if (cc->ast_list == NULL) {
        loggerWarning("Create AST before compiling AST\n");
        return NULL;
    }

    aoStr *asmbuf = asmGenerate(cc);
    return asmbuf;
}

void compileToTokens(Cctrl *cc, CliArgs *args, int lexer_flags) {
    Lexer *l = (Lexer *)globalArenaAllocate(sizeof(Lexer));
    StrMap *seen_files = strMapNew(32);
    char *root_dir = mprintf("%s/include/", args->install_dir);

    lexInit(l, NULL, CCF_PRE_PROC | lexer_flags);
    l->seen_files = seen_files;
    l->lineno = 1;
    lexSetBuiltinRoot(l, root_dir);

    lexPushFile(l, aoStrDupRaw(args->infile, strlen(args->infile)));

    Lexeme *token = NULL;
    while ((token = lexToken(cc->macro_defs, l))) {
        lexemePrint(token);
    }

    strMapRelease(seen_files);
}

int compileToAst(Cctrl *cc, CliArgs *args, int lexer_flags) {
    Lexer *l = (Lexer *)globalArenaAllocate(sizeof(Lexer));
    aoStr *builtin_path = aoStrPrintf("%s/include/tos.HH", args->install_dir);
    char *root_dir = mprintf("%s/include/", args->install_dir);

    lexInit(l, NULL, CCF_PRE_PROC | lexer_flags);
    l->lineno = 1;
    lexSetBuiltinRoot(l, root_dir);

    lexPushFile(l, aoStrDupRaw(args->infile, strlen(args->infile)));
    /* library files */
    /* the structure is a so this will get popped first */
    lexPushFile(l, builtin_path);

    cctrlInitParse(cc, l);

    parseToAst(cc);

    lexReleaseAllFiles(l);
    listRelease(l->files, NULL);
    return 1;
}
