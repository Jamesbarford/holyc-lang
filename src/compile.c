#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "compile.h"
#include "dict.h"
#include "x86.h"
#include "lexer.h"
#include "list.h"
#include "parser.h"
#include "util.h"

char *readfile(char *path) {
    char *buf;
    int fd;

    if ((fd = open(path, O_RDONLY, 0644)) == -1) {
        loggerPanic("Failed to open file: %s\n", path);
    }
 
    int len = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);

    buf = malloc(sizeof(char) * len);
    if (read(fd, buf, len) != len) {
        loggerPanic("Failed to read whole file\n");
    }
    buf[len-1] = '\0';
    close(fd);
    return buf;
}

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
    DictNode *dn;
    char *tmp;

    printf("AST: \n");
    for (int i = 0; i < cc->clsdefs->capacity; ++i) {
        dn = cc->clsdefs->body[i];
        while (dn) {
            tmp = AstTypeToString(dn->val);
            printf("%s\n", tmp);
            free(tmp);
            dn = dn->next;
        }
    }

    it = cc->ast_list->next;
    while (it != cc->ast_list) {
        ast = (Ast *)it->value;
        if (ast == NULL) {
            printf("here\n");
        }
        tmp = AstToString(ast);
        printf("%s\n", tmp);
        free(tmp);
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

int CompileToAst(Cctrl *cc, char *filepath, int lexer_flags) {
    List *tokens, *next_tokens, *code_list;
    lexer l;
    aoStr *file_path;
    char *src, *strpath;
    Dict *seen_files;

    seen_files = DictNew(&default_table_type);
    tokens = ListNew();
    code_list = ListNew();

    lexerInit(&l,NULL);

    ListAppend(l.files,aoStrDupRaw(filepath,strlen(filepath),0));
    l.flags |= lexer_flags;

    while ((file_path = ListPop(l.files)) != NULL) {
        if (DictGetLen(seen_files,file_path->data,file_path->len) != NULL) {
            aoStrRelease(file_path);
            continue;
        }

        src = readfile(file_path->data);
        ListAppend(code_list,src);
        l.ptr = src;
        l.lineno = 1;
        next_tokens = lexToLexemes(cc->macro_defs,&l);

        if (!next_tokens || next_tokens->next == next_tokens) {
            ListRelease(next_tokens, NULL);
            continue;
        }

        /* Get one massive list of all of the tokens, in order of how
         * they arrived in #include's */
        strpath = aoStrMove(file_path);
        ListMergePrepend(tokens,next_tokens);
        DictSet(seen_files,strpath,strpath);
    }

    DictRelease(seen_files);
    CctrlInitTokenIter(cc,tokens);
    ParseToAst(cc);
    ListRelease(code_list,free);
    ListRelease(l.files,NULL);
    lexemeListRelease(tokens);
    return 1;
}

aoStr *CompileFile(Cctrl *cc, char *file_path) {
    aoStr *asm_str;
    CompileToAst(cc,file_path,CCF_PRE_PROC);
    asm_str = CompileToAsm(cc);
    return asm_str;
}

aoStr *CompileCode(Cctrl *cc, char *code, int lexer_flags) {
    aoStr *asm_str;
    List *tokens;
    lexer l;
    lexerInit(&l,code);
    tokens = lexToLexemes(cc->macro_defs,&l);
    CctrlInitTokenIter(cc,tokens);
    ParseToAst(cc);
    asm_str = CompileToAsm(cc);
    lexemeListRelease(tokens);
    free(l.files);
    return asm_str;
}
