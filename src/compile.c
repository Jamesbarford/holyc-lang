#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "cfg-print.h"
#include "cfg.h"
#include "compile.h"
#include "x86.h"
#include "lexer.h"
#include "list.h"
#include "parser.h"
#include "util.h"

void compilePrintTokens(Cctrl *cc) {
    if (!cc->tkit->tokens || 
            cc->tkit->tokens == cc->tkit->tokens->next) {
        loggerWarning("No tokens to print\n");
        return;
    }
    lexemePrintList(cc->tkit->tokens);
}

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

List *compileToTokens(Cctrl *cc, char *entrypath, int lexer_flags) {
    List *tokens;
    lexer l;
    aoStr *builtin_path;
    StrMap *seen_files;

    seen_files = strMapNew(32);
    tokens = listNew();
    builtin_path = aoStrDupRaw("/usr/local/include/tos.HH",25); //aoStrNew();

    lexInit(&l,NULL,CCF_PRE_PROC);
    l.seen_files = seen_files;
    l.lineno = 1;
    lexSetBuiltinRoot(&l,"/usr/local/include/");

    /* library files */
    lexPushFile(&l,aoStrDupRaw(entrypath,strlen(entrypath)));
    /* the structure is a stack so this will get popped first */
    lexPushFile(&l,builtin_path);

    tokens = lexToLexemes(cc->macro_defs,&l);
    lexemePrintList(tokens);
    strMapRelease(seen_files);
    return tokens;
}

int compileToAst(Cctrl *cc, char *entrypath, int lexer_flags) {
    List *tokens;
    lexer l;
    aoStr *builtin_path;
    StrMap *seen_files;

    seen_files = strMapNew(32);
    tokens = listNew();
    builtin_path = aoStrDupRaw("/usr/local/include/tos.HH",25); //aoStrNew();

    lexInit(&l,NULL,CCF_PRE_PROC);
    l.seen_files = seen_files;
    l.lineno = 1;
    lexSetBuiltinRoot(&l,"/usr/local/include/");

    /* library files */
    lexPushFile(&l,aoStrDupRaw(entrypath,strlen(entrypath)));
    /* the structure is a stack so this will get popped first */
    lexPushFile(&l,builtin_path);

    tokens = lexToLexemes(cc->macro_defs,&l);

    strMapRelease(seen_files);
    cctrlInitTokenIter(cc,tokens);
    parseToAst(cc);

    lexReleaseAllFiles(&l);
    aoStrRelease(builtin_path);
    listRelease(l.files,NULL);

    /* @Leak - Jamesbarford 2024/07/19, when should this be freed if at all? */
    // listRelease(code_list,free);
    // lexemelistRelease(tokens);
    return 1;
}

aoStr *compileCode(Cctrl *cc, char *code, int lexer_flags) {
    aoStr *asm_str;
    List *tokens;
    lexer l;
    lexInit(&l,code,lexer_flags);
    tokens = lexToLexemes(cc->macro_defs,&l);
    cctrlInitTokenIter(cc,tokens);
    parseToAst(cc);
    asm_str = compileToAsm(cc);
    lexemelistRelease(tokens);
    free(l.files);
    return asm_str;
}
