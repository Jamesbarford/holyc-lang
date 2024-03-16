#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "compile.h"
#include "dict.h"
#include "ir.h"
#include "x86.h"
#include "lexer.h"
#include "list.h"
#include "parser.h"
#include "util.h"

void CompilePrintTokens(Cctrl *cc) {
    if (!cc->tkit->tokens || 
            cc->tkit->tokens == cc->tkit->tokens->next) {
        loggerWarning("No tokens to print\n");
        return;
    }
    lexemePrintList(cc->tkit->tokens);
}

void CompilePrintAst(Cctrl *cc) {
    List *it;
    Ast *ast;
    //DictNode *dn;
    char *tmp;

    printf("AST: \n");
    //for (int i = 0; i < cc->clsdefs->capacity; ++i) {
    //    dn = cc->clsdefs->body[i];
    //    while (dn) {
    //        tmp = AstTypeToString(dn->val);
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
            tmp = AstToString(ast);
            printf("%s\n", tmp);
            free(tmp);
        }
        it = it->next;
    }
}

aoStr *CompileToAsm(Cctrl *cc) {
    if (cc->ast_list == NULL) {
        loggerWarning("Create AST before compiling AST\n");
        return NULL;
    }

    aoStr *asmbuf = AsmGenerate(cc);
    return asmbuf;
}

int CompileToAst(Cctrl *cc, char *entrypath, int lexer_flags) {
    List *tokens;
    lexer l;
    aoStr *builtin_path;
    Dict *seen_files;

    seen_files = DictNew(&default_table_type);
    tokens = ListNew();
    builtin_path = aoStrDupRaw("/usr/local/include/tos.HH",25); //aoStrNew();

    lexerInit(&l,NULL,CCF_PRE_PROC);
    l.seen_files = seen_files;
    l.lineno = 1;
    lexerSetBuiltinRoot(&l,"/usr/local/include/");

    /* library files */
    lexPushFile(&l,aoStrDupRaw(entrypath,strlen(entrypath)));
    /* the structure is a stack so this will get popped first */
    lexPushFile(&l,builtin_path);

    tokens = lexToLexemes(cc->macro_defs,&l);
    DictRelease(seen_files);
    CctrlInitTokenIter(cc,tokens);
    ParseToAst(cc);
    irMain(cc);
    irPrint(cc->ir_list);
    lexReleaseAllFiles(&l);
    aoStrRelease(builtin_path);
    // ListRelease(code_list,free);
    ListRelease(l.files,NULL);
    lexemeListRelease(tokens);
    return 1;
}

aoStr *CompileCode(Cctrl *cc, char *code, int lexer_flags) {
    aoStr *asm_str;
    List *tokens;
    lexer l;
    lexerInit(&l,code,lexer_flags);
    tokens = lexToLexemes(cc->macro_defs,&l);
    CctrlInitTokenIter(cc,tokens);
    ParseToAst(cc);
    asm_str = CompileToAsm(cc);
    lexemeListRelease(tokens);
    free(l.files);
    return asm_str;
}
