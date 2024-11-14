#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "compile.h"
#include "x86.h"
#include "lexer.h"
#include "list.h"
#include "parser.h"
#include "util.h"

void compilePrintAst(Cctrl *cc) {
    List *it;
    Ast *ast;
    //StrMapNode *dn;
    char *tmp;

    printf("AST: \n");
    //for (int i = 0; i < cc->clsdefs->capacity; ++i) {
    //    dn = cc->clsdefs->body[i];
    //    while (dn) {
    //        tmp = astTypeToString(dn->val);
    //        printf("%s\n", tmp);
    //        free(tmp);
    //        dn = dn->next;
    //    }
    //}

    it = cc->ast_list->next;
    while (it != cc->ast_list) {
        ast = (Ast *)it->value;
        if (ast == NULL) {
            printf("here\n");
        }

        if (ast->kind != AST_FUN_PROTO &&
            ast->kind != AST_EXTERN_FUNC && 
            ast->kind != AST_ASM_FUNC_BIND)
        {
            tmp = astToString(ast);
            printf("%s\n", tmp);
            free(tmp);
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

void compileToTokens(Cctrl *cc, char *entrypath, int lexer_flags) {
    lexer *l = malloc(sizeof(lexer));
    aoStr *builtin_path;
    StrMap *seen_files;

    seen_files = strMapNew(32);
    
    builtin_path = aoStrDupRaw("/usr/local/include/tos.HH",25); //aoStrNew();

    lexInit(l,NULL,CCF_PRE_PROC);
    l->seen_files = seen_files;
    l->lineno = 1;
    lexSetBuiltinRoot(l,"/usr/local/include/");

    /* library files */
    lexPushFile(l,aoStrDupRaw(entrypath,strlen(entrypath)));
    /* the structure is a stack so this will get popped first */
    lexPushFile(l,builtin_path);

    lexeme *token = NULL;

    while ((token = lexToken(cc->macro_defs,l))) {
        lexemePrint(token);
        //free(token);
    }

    strMapRelease(seen_files);
    free(l);
}

int compileToAst(Cctrl *cc, char *entrypath, int lexer_flags) {
    lexer *l = malloc(sizeof(lexer));
    aoStr *builtin_path;
    StrMap *seen_files;

    seen_files = strMapNew(32);
    builtin_path = aoStrDupRaw("/usr/local/include/tos.HH",25); //aoStrNew();

    lexInit(l,NULL,CCF_PRE_PROC);
    l->seen_files = seen_files;
    l->lineno = 1;
    lexSetBuiltinRoot(l,"/usr/local/include/");

    /* library files */
    lexPushFile(l,aoStrDupRaw(entrypath,strlen(entrypath)));
    /* the structure is a stack so this will get popped first */
    lexPushFile(l,builtin_path);

    cctrlInitParse(cc,l);

    parseToAst(cc);
    
    lexerPoolRelease();

    strMapRelease(seen_files);
    lexReleaseAllFiles(l);
    aoStrRelease(builtin_path);
    listRelease(l->files,NULL);
    free(l);

    /* @Leak - Jamesbarford 2024/07/19, when should this be freed if at all? */
    // listRelease(code_list,free);
    // lexemelistRelease(tokens);
    return 1;
}
