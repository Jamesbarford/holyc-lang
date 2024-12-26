#include <string.h>
#include <stdio.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "list.h"
#include "map.h"
#include "prsasm.h"


/* format the assembly based on how many characters there are for the 
 * mneumonic */
static inline char *getTabs(aoStr *str) {
    switch (str->len) {
        case 2: return "\t\t";
        case 3: return "\t\t";
        case 4: return "\t";
        default: return "\t";
    }
}

void prsAsmMem(Cctrl *cc, aoStr *buf) {
    Lexeme *tok;
    tok = cctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        cctrlRaiseException(cc,"[<register>] expected got: %s",lexemeToString(tok));
    }
    cctrlTokenExpect(cc,']');
    aoStrCatPrintf(buf,"(%%%.*s)",tok->len,tok->start);
}

void prsAsmOffset(Cctrl *cc, aoStr *buf, Lexeme *tok) {
    if (tok->tk_type != TK_I64) {
        cctrlRaiseException(cc,"Expected TK_I64 type got: %s",lexemeToString(tok));
    }

    cctrlTokenExpect(cc,'[');
    tok = cctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        cctrlRaiseException(cc,"Expected <number>[<register>] Got: %s",
                lexemeToString(tok));
    }
    cctrlTokenExpect(cc,']');
    aoStrCatPrintf(buf, "(%%%.*s)",tok->len,tok->start);
}

void prsAsmImm(Cctrl *cc, aoStr *buf, Lexeme *tok) {
    Lexeme *next;
    switch (tok->tk_type) {
        case TK_PUNCT:
            if (tok->i64 != '-') {
                cctrlRaiseException(cc,"Expected '-'<numerical>");
            }

            tok = cctrlTokenGet(cc);
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
                cctrlTokenGet(cc);
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
                    cctrlTokenGet(cc);
                    prsAsmMem(cc,buf);
                }
            }

            break;
    }
}

void prsAsmLabel(Cctrl *cc, aoStr *buf) {
    Lexeme *tok, *next;
    long label_num;

    tok = cctrlTokenGet(cc);
    if (!tokenPunctIs(tok,'@')) {
        cctrlRaiseException(cc,": Labels must be: '@@<int>'");
    }

    tok = cctrlTokenGet(cc);
    if (tok->tk_type != TK_I64) {
        cctrlRaiseException(cc,": Labels must be: '@@<int>'");
    }
    
    label_num = tok->i64;
    next = cctrlTokenPeek(cc);
    if (tokenPunctIs(next,':')) {
        aoStrCatPrintf(buf, ".%s_%zu:",cc->tmp_asm_fname->data,label_num);
        cctrlTokenGet(cc);
    } else {
        aoStrCatPrintf(buf, ".%s_%zu",cc->tmp_asm_fname->data, label_num);
    }
}

void prsAsmPunct(Cctrl *cc, Lexeme *tok, aoStr *buf) {
    Lexeme *next;
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

    next = cctrlTokenPeek(cc);
    if (tokenPunctIs(next,'[')) {
        cctrlTokenRewind(cc);
        tok = cctrlTokenGet(cc);
        prsAsmOffset(cc,buf,tok);
    }
}

static void maybePutRegister(Cctrl *cc, aoStr *buf, aoStr *maybe_register) {
    if (strMapGetLen(cc->x86_registers,
                maybe_register->data,maybe_register->len) != NULL) {
        aoStrCatPrintf(buf,"%%%s",maybe_register->data);
    } else {
        aoStrCatLen(buf,maybe_register->data,maybe_register->len);
    }
}

/* Custom assembly to AT&T */
Ast *prsAsmToATT(Cctrl *cc) {
    aoStr *op1 = NULL, *op2 = NULL, *op3 = NULL, *asm_code = NULL, *curblock = NULL, *stdfunc = NULL, *curfunc = NULL;
    List *asm_functions;
    Ast *asm_function, *asm_block;
    Lexeme *tok, *next;
    int isbol = 0, count = 0;

    asm_functions = listNew();
    curblock = NULL;
    asm_code = aoStrNew();
    memset(asm_code->data,'\0',asm_code->capacity-1);

    cctrlTokenExpect(cc, '{');
    tok = cctrlTokenGet(cc);

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

            case TK_IDENT: {
                next = cctrlTokenPeek(cc);
                if (tokenPunctIs(next, TK_DBL_COLON) || tokenPunctIs(next,':')) {
                    cctrlTokenGet(cc);
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
                        if (!strMapAddOrErr(cc->asm_functions, curfunc->data, asm_function)) {
                            cctrlIce(cc, "Already defined assembly function: %s", curfunc->data);
                        }
                        curblock = aoStrNew();
                    }

                    /* Just the function name in op1, this is so it can be 
                     * called in the c code. */
                    curfunc = aoStrDupRaw(tok->start,tok->len);
                    /* Save the name for pasting labels */
                    cc->tmp_asm_fname = curfunc;
                    tok = cctrlTokenGet(cc);
                    isbol = 1;
                    continue;
                }
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
                                if (strMapGetLen(cc->libc_functions,
                                            op1->data,op1->len) != NULL) {
                                    stdfunc = astNormaliseFunctionName(op1->data);
                                    aoStrCatPrintf(curblock,"%s\n",stdfunc->data);
                                    aoStrRelease(stdfunc);
                                } else {
                                    aoStrCatPrintf(curblock,"%s\n",op1->data);
                                }
                                aoStrRelease(op1);
                                break;

                            case 2: {
                                aoStrToLowerCase(op1);
                                aoStrCatPrintf(curblock,"\t%s%s",op1->data,getTabs(op1));

                                if (op1->len == 4 && !memcmp(op1->data,"call",4)) {
                                    if (strMapGetLen(cc->libc_functions,
                                                op2->data,op2->len) != NULL) { 
                                        stdfunc = astNormaliseFunctionName(op2->data);
                                        aoStrCatPrintf(curblock,"%s\n",stdfunc->data);
                                        aoStrRelease(stdfunc);
                                    } else {
                                        aoStrCatPrintf(curblock,"%s\n",op2->data);
                                    }
                                } else {
                                    aoStrToLowerCase(op2);
                                    maybePutRegister(cc,curblock,op2);
                                    aoStrPutChar(curblock,'\n');
                                }
                                aoStrRelease(op1);
                                aoStrRelease(op2);
                                break;
                            }

                            case 3:
                                aoStrToLowerCase(op1);
                                aoStrToLowerCase(op2);
                                aoStrToLowerCase(op3);

                                aoStrCatPrintf(curblock,"\t%s%s", op1->data,getTabs(op1));

                                maybePutRegister(cc,curblock,op3);
                                aoStrCatLen(curblock, ", ", 2);

                                maybePutRegister(cc,curblock,op2);
                                aoStrPutChar(curblock,'\n');

                                aoStrRelease(op1);
                                aoStrRelease(op2);
                                aoStrRelease(op3);
                                break;
                            default:
                                cctrlRaiseException(cc,"Unexpected number of arguments for"
                                        " x86 transpilation: %d"" Expression",
                                        count);
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
        tok = cctrlTokenGet(cc);
    }

    if (curfunc) {
        aoStrCatLen(asm_code,curfunc->data,curfunc->len);
        aoStrCatLen(asm_code,":\n",2);
        aoStrCatLen(asm_code,curblock->data,curblock->len);
        asm_function = astAsmFunctionDef(curfunc,curblock);
        listAppend(asm_functions,asm_function);
        if (!strMapAddOrErr(cc->asm_functions, curfunc->data, asm_function)) {
            cctrlIce(cc, "Already defined assembly function: %s", curfunc->data);
        }
    }
    asm_block = astAsmBlock(asm_code,asm_functions);
    return asm_block;
}

/* The next token is expected to be '{' so the parser has just seen 'asm' */
Ast *prsAsm(Cctrl *cc) {
    return prsAsmToATT(cc);
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

    while ((tok = cctrlTokenGet(cc)) != NULL) {
        if (tokenIdentIs(tok,"asm",3)) {
            peek = cctrlTokenPeek(cc);
            if (tokenPunctIs(peek,'{')) {
                asm_block = prsAsm(cc);
                printf("%s\n", asm_block->asm_stmt->data);
            }
        }
    }
    free(file);
}

#endif
