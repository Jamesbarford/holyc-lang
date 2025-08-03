#include <string.h>
#include <stdio.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "list.h"
#include "prsasm.h"
#include "prslib.h"
#include "util.h"

typedef struct AsmUnit {
    AoStr *op1;
    AoStr *op2;
    AoStr *op3;
    int op_count;
} AsmUnit;

/* format the assembly based on how many characters there are for the 
 * mneumonic */
static inline char *getTabs(AoStr *str) {
    switch (str->len) {
        case 2: return "\t\t";
        case 3: return "\t\t";
        case 4: return "\t";
        default: return "\t";
    }
}

void prsAsmMem(Cctrl *cc, AoStr *buf) {
    Lexeme *tok;
    tok = cctrlAsmTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        cctrlRaiseException(cc,"[<register>] expected got: %s",lexemeToString(tok));
    }
    cctrlTokenExpect(cc,']');
    aoStrCatFmt(buf,"(%.*s)",tok->len,tok->start);
    aoStrToLowerCase(buf);
}

void prsAsmOffset(Cctrl *cc, AoStr *buf, Lexeme *tok) {
    if (tok->tk_type != TK_I64) {
        cctrlRaiseException(cc,"Expected TK_I64 type got: %s",lexemeToString(tok));
    }

    cctrlTokenExpect(cc,'[');
    tok = cctrlAsmTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        cctrlRaiseException(cc,"Expected <number>[<register>] Got: %s",
                lexemeToString(tok));
    }
    cctrlTokenExpect(cc,']');
    aoStrCatFmt(buf, "(%.*s)",tok->len,tok->start);
}

void prsAsmImm(Cctrl *cc, AoStr *buf, Lexeme *tok) {
    Lexeme *next;
    switch (tok->tk_type) {
        case TK_PUNCT:
            if (tok->i64 != '-') {
                cctrlRaiseException(cc,"Expected '-'<numerical>");
            }

            tok = cctrlAsmTokenGet(cc);
            if (tok->tk_type != TK_I64 && tok->tk_type != TK_F64) {
                cctrlRaiseException(cc,"Expected -<numerical> got: %s\n",
                        lexemeToString(tok));
            }
            next = cctrlTokenPeek(cc);
            if (!tokenPunctIs(next,'[')) {
                aoStrPutChar(buf,'$');
                aoStrCatPrintf(buf, "-%zu",(unsigned long)tok->i64);
            } else {
                aoStrCatPrintf(buf, "-%zu",(unsigned long)tok->i64);
                cctrlAsmTokenGet(cc);
                prsAsmMem(cc,buf);
            }
            break;

        case TK_CHAR_CONST:
        case TK_I64:
            next = cctrlTokenPeek(cc);
            if (!tokenPunctIs(next,'[')) {
                aoStrPutChar(buf,'$');
                if (tok->ishex) {
                    aoStrCatPrintf(buf, "%#llX",(size_t)tok->i64);
                } else { 
                    aoStrCatPrintf(buf, "%zu",(size_t)tok->i64);
                }
            } else {
                if (tok->ishex) {
                    aoStrCatPrintf(buf, "%#llX",(size_t)tok->i64);
                } else { 
                    aoStrCatPrintf(buf, "%zu",(size_t)tok->i64);
                }
                if (tokenPunctIs(next,'[')) {
                    cctrlAsmTokenGet(cc);
                    prsAsmMem(cc,buf);
                }
            }
            break;
        default:
            printf("mem parse: %s\n", lexemeToString(tok));
            break;
    }
}

void prsAsmLabel(Cctrl *cc, AoStr *buf) {
    Lexeme *tok = cctrlAsmTokenGet(cc);
    if (!tokenPunctIs(tok,'@')) {
        cctrlRaiseException(cc,": Labels must be: '@@<int>'");
    }

    tok = cctrlAsmTokenGet(cc);
    if (tok->tk_type != TK_I64) {
        cctrlRaiseException(cc,": Labels must be: '@@<int>'");
    }
    
    s64 label_num = tok->i64;
    Lexeme *next = cctrlTokenPeek(cc);
    AoStr *label = aoStrDup(cc->tmp_asm_fname);
    aoStrToLowerCase(label);
    if (tokenPunctIs(next,':')) {
        aoStrCatFmt(buf, ".%S_%U:",label,label_num);
        cctrlAsmTokenGet(cc);
    } else {
        aoStrCatFmt(buf, ".%S_%U",label,label_num);
    }
    aoStrRelease(label);
}

void prsAsmPunct(Cctrl *cc, Lexeme *tok, AoStr *buf) {
    switch (tok->i64) {
        case '[':
            prsAsmMem(cc,buf);
            break;
        case '-':
            prsAsmImm(cc,buf,tok);
            break;
        case '@':
            prsAsmLabel(cc,buf);
            break;
        default:
            lexemePrint(tok);
            cctrlRaiseException(cc,": Unexpected character");
    }

    Lexeme *next = cctrlTokenPeek(cc);
    if (tokenPunctIs(next,'[')) {
        cctrlTokenRewind(cc);
        tok = cctrlAsmTokenGet(cc);
        prsAsmOffset(cc,buf,tok);
    }
}

/* Custom assembly to AT&T */
Ast *prsAsmToATT(Cctrl *cc, int parse_one) {
    AoStr *op1 = NULL, *op2 = NULL, *op3 = NULL, *asm_code = NULL, *curblock = NULL, *stdfunc = NULL, *curfunc = NULL;
    List *asm_functions;
    Ast *asm_function, *asm_block;
    Lexeme *tok, *next;
    int isbol = 0, count = 0;

    asm_functions = listNew();
    asm_code = aoStrNew();
    curblock = parse_one ? asm_code : NULL;
    memset(asm_code->data,'\0',asm_code->capacity-1);

    cctrlTokenExpect(cc, '{');
    tok = cctrlAsmTokenGet(cc);

    while (!tokenPunctIs(tok, '}')) {
        switch (tok->tk_type) {
            case TK_CHAR_CONST:
            case TK_I64: {
                switch (count) {
                    case 1:
                        count++;
                        op2 = aoStrNew();
                        prsAsmImm(cc,op2,tok);
                        break;
                    case 2:
                        count++;
                        op3 = aoStrNew();
                        prsAsmImm(cc,op3,tok);
                        break;
                }
                break;
            }

            case TK_KEYWORD: {
                if (tok->i64 == KW_SIZEOF) {
                    Ast *sizeof_ast = parseSizeof(cc);
                    u64 size = (unsigned long)sizeof_ast->i64;
                    switch (count) {
                        case 1:
                            count++;
                            op2 = aoStrPrintf("$%zu",size);
                            break;
                        case 2:
                            count++;
                            op3 = aoStrPrintf("$%zu",size);
                            break;
                    }
                } else {
                    cctrlRaiseException(cc, "\nCannot Handle keyword: %.*s in this context", tok->len, tok->start);
                }
                break;
            }    


            case TK_IDENT: {
                next = cctrlTokenPeek(cc);
                if (tokenPunctIs(next, TK_DBL_COLON) || tokenPunctIs(next,':')) {
                    cctrlAsmTokenGet(cc);
                    cctrlTokenExpect(cc, '\n');

                    if (curblock == NULL) {
                        curblock = aoStrNew();
                    } else {
                        /* Save the current function */
                        aoStrCatLen(asm_code,curfunc->data,curfunc->len);
                        aoStrCatLen(asm_code,":\n",2);
                        aoStrCatLen(asm_code,curblock->data,curblock->len);
                        aoStrPutChar(asm_code,'\n');
                        /* Create an ast */
                        asm_function = astAsmFunctionDef(curfunc,curblock);
                        listAppend(asm_functions,asm_function);
                        if (!mapAddOrErr(cc->asm_functions, curfunc->data, asm_function)) {
                            cctrlIce(cc, "Already defined assembly function: %s", curfunc->data);
                        }
                        curblock = aoStrNew();
                    }

                    /* Just the function name in op1, this is so it can be 
                     * called in the c code. */
                    curfunc = aoStrDupRaw(tok->start,tok->len);
                    /* Save the name for pasting labels */
                    cc->tmp_asm_fname = curfunc;
                    tok = cctrlAsmTokenGet(cc);
                    isbol = 1;
                    continue;
                } 

                else if (!isbol && count != 1 && tokenPunctIs(next, '[')) {
                    /* Possibly loading a global */
                    op3 = aoStrDupRaw(tok->start,tok->len);
                    cctrlAsmTokenGet(cc);
                    next = cctrlAsmTokenGet(cc);
                    aoStrCatFmt(op3, "(%.*s)",next->len, next->start);
                    cctrlAsmTokenGet(cc);
                } else
                if (isbol) {
                    op1 = aoStrDupRaw(tok->start,tok->len);
                    isbol = 0;
                } else {
                    if (count == 1) {
                        op2 = aoStrDupRaw(tok->start,tok->len);
                    } else {
                        op3 = aoStrDupRaw(tok->start,tok->len);
                    }
                }
                count++;
                break;
            }

            case TK_PUNCT: {
                switch (tok->i64) {
                    case '\n':
                        switch (count) {
                            case 0:
                                break;

                            case 1:
                                /* Not A jump */
                                if (op1->data[0] != '.') { 
                                    aoStrPutChar(curblock,'\t');
                                }
                                /* Only a few functions get called as one 
                                 * operation */
                                aoStrToLowerCase(op1);
                                if (setHasLen(cc->libc_functions, op1->data,op1->len)) {
                                    stdfunc = astNormaliseFunctionName(op1->data);
                                    aoStrCatFmt(curblock,"%S\n",stdfunc);
                                    aoStrRelease(stdfunc);
                                } else {
                                    aoStrCatFmt(curblock,"%S\n",op1);
                                }
                                aoStrRelease(op1);
                                break;

                            case 2: {
                                aoStrToLowerCase(op1);
                                aoStrCatPrintf(curblock,"\t%s%s",op1->data,getTabs(op1));
                                int is_call = op1->len == 4 && !strncasecmp(op1->data,str_lit("call"));

                                if (is_call) {
                                    if (mapGetLen(cc->global_env, op2->data, op2->len) != NULL || 
                                        setHasLen(cc->libc_functions, op2->data, op2->len)) {
                                        stdfunc = astNormaliseFunctionName(op2->data);
                                        aoStrCatFmt(curblock,"%S\n",stdfunc);
                                        aoStrRelease(stdfunc);
                                    } else {
                                        if (op2->data[0] == '%') {
                                            aoStrPutChar(curblock,'*');
                                        }
                                        aoStrCatFmt(curblock,"%S\n",op2);
                                    }
                                } else {
                                  int is_jump = op1->len == 3 && 
                                                !strncasecmp(op1->data,str_lit("jmp"));
                                    if (is_jump && op2->data[0] == '%') {
                                        aoStrPutChar(curblock,'*');
                                    }
                                    aoStrToLowerCase(op2);
                                    aoStrCatFmt(curblock,"%S\n",op2);
                                }
                                aoStrRelease(op1);
                                aoStrRelease(op2);
                                break;
                            }

                            case 3:
                                aoStrToLowerCase(op1);

                                aoStrCatFmt(curblock,"\t%S%s", op1,getTabs(op1));

                                aoStrCatFmt(curblock,"%S, %S\n",op3, op2);

                                aoStrRelease(op1);
                                aoStrRelease(op2);
                                aoStrRelease(op3);
                                break;
                            default:
                                cctrlRaiseException(cc,"Unexpected number of arguments for"
                                        " x86 transpilation: %d %s %s",
                                        count,op1->data,op2->data);
                        }
                        isbol = 1;
                        count = 0;
                        break;

                    case ',':
                        break;
                    default:
                        switch (count) {
                            case 0:
                                op1 = aoStrNew();
                                prsAsmPunct(cc,tok,op1);
                                break;
                            case 1:
                                op2 = aoStrNew();
                                prsAsmPunct(cc,tok,op2);
                                break;
                            case 2:
                                op3 = aoStrNew();
                                prsAsmPunct(cc,tok,op3);
                                break;
                        }
                        count++;
                        break;
                }
                break;
            }

        }
        tok = cctrlAsmTokenGet(cc);
    }

    if (curfunc) {
        aoStrCatLen(asm_code,curfunc->data,curfunc->len);
        aoStrCatLen(asm_code,":\n",2);
        aoStrCatLen(asm_code,curblock->data,curblock->len);
        asm_function = astAsmFunctionDef(curfunc,curblock);
        listAppend(asm_functions,asm_function);
        if (!mapAddOrErr(cc->asm_functions, curfunc->data, asm_function)) {
            cctrlIce(cc, "Already defined assembly function: %s", curfunc->data);
        }
    }
    asm_block = astAsmBlock(asm_code,asm_functions);
    return asm_block;
}

/* The next token is expected to be '{' so the parser has just seen 'asm' */
Ast *prsAsm(Cctrl *cc, int parse_one) {
    return prsAsmToATT(cc, parse_one);
}

#ifdef PRSASM_TEST

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
char *readfile(char *path) {
    int fd;
    if ((fd = open(path, O_RDONLY, 0644)) == -1) {
        loggerPanic("Failed to open file\n");
    }

    int len = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);

    char *buf = (char *)malloc(sizeof(char) * len);
    if (read(fd, buf, len) != len) {
        loggerPanic("Failed to read whole file\n");
    }
    buf[len] = '\0';
    close(fd);
    return buf;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        loggerPanic("usage: %s<file>\n", argv[0]);
    }
    lexer l;
    List *tokens;
    Cctrl *cc;
    char *file;
    Ast *asm_block;
    lexeme *tok, *peek;

    cc = cctrlNew();
    file = readfile(argv[1]);

    lexInit(&l, file);
    tokens = lexToLexemes(cc->macro_defs, &l);
    cctrlInitTokenIter(cc, tokens);

    while ((tok = cctrlAsmTokenGet(cc)) != NULL) {
        if (tokenIdentIs(tok,"asm",3)) {
            peek = cctrlTokenPeek(cc);
            if (tokenPunctIs(peek,'{')) {
                asm_block = prsAsm(cc,0);
                printf("%s\n", asm_block->asm_stmt->data);
            }
        }
    }
    free(file);
}

#endif
