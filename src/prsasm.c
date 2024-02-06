#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "list.h"
#include "prsasm.h"
#include "util.h"


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

void PrsAsmMem(Cctrl *cc, aoStr *buf) {
    lexeme *tok;
    tok = CctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        loggerPanic("[<register>] expected got: %s at line: %d\n",
                lexemeToString(tok), tok->len);
    }
    CctrlTokenExpect(cc,']');
    aoStrCatPrintf(buf,"(%%%.*s)",tok->len,tok->start);
}

void PrsAsmOffset(Cctrl *cc, aoStr *buf, lexeme *tok) {
    if (tok->tk_type != TK_I64) {
        loggerPanic("Expected TK_I64 type at line: %d got: %s\n",
                tok->line,lexemeToString(tok));
    }

    CctrlTokenExpect(cc,'[');
    tok = CctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        loggerPanic("Expected <number>[<register>] expected at line: %d. Got: %s\n",
                tok->len, lexemeToString(tok));
    }
    CctrlTokenExpect(cc,']');
    aoStrCatPrintf(buf, "(%%%.*s)",tok->len,tok->start);
}

void PrsAsmImm(Cctrl *cc, aoStr *buf, lexeme *tok) {
    lexeme *next;
    switch (tok->tk_type) {
        case TK_PUNCT:
            if (tok->i64 != '-') {
                loggerPanic("Expected '-'<numerical> at Line: %d\n", tok->line);
            }

            tok = CctrlTokenGet(cc);
            if (tok->tk_type != TK_I64 && tok->tk_type != TK_F64) {
                loggerPanic("Expected -<numerical> at Line: %d got: %s\n",
                        tok->line, lexemeToString(tok));
            }
            next = CctrlTokenPeek(cc);
            if (!TokenPunctIs(next,'[')) {
                aoStrPutChar(buf,'$');
                aoStrCatPrintf(buf, "-%zu",(unsigned long)tok->i64);
            } else {
                aoStrCatPrintf(buf, "-%zu",(unsigned long)tok->i64);
                CctrlTokenGet(cc);
                PrsAsmMem(cc,buf);
            }
            break;

        case TK_I64:
            next = CctrlTokenPeek(cc);
            if (!TokenPunctIs(next,'[')) {
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
                if (TokenPunctIs(next,'[')) {
                    CctrlTokenGet(cc);
                    PrsAsmMem(cc,buf);
                }
            }

            break;
    }
}

void PrsAsmLabel(Cctrl *cc, aoStr *buf) {
    lexeme *tok, *next;
    long label_num;

    tok = CctrlTokenGet(cc);
    if (!TokenPunctIs(tok,'@')) {
        loggerPanic("Labels must be: '@@<int>' at line: %d\n", tok->len);
    }

    tok = CctrlTokenGet(cc);
    if (tok->tk_type != TK_I64) {
        loggerPanic("Labels must be: '@@<int>' at line: %d\n", tok->len);
    }
    
    label_num = tok->i64;
    next = CctrlTokenPeek(cc);
    if (TokenPunctIs(next,':')) {
        aoStrCatPrintf(buf, ".%s_%zu:",cc->tmp_asm_fname->data,label_num);
        CctrlTokenGet(cc);
    } else {
        aoStrCatPrintf(buf, ".%s_%zu",cc->tmp_asm_fname->data, label_num);
    }
}

void PrsAsmPunct(Cctrl *cc, lexeme *tok, aoStr *buf) {
    lexeme *next;
    switch (tok->i64) {
        case '[':
            PrsAsmMem(cc,buf);
            break;
        case '-':
            PrsAsmImm(cc,buf,tok);
            break;
        case '@':
            PrsAsmLabel(cc,buf);
            break;
        default:
            lexemePrint(tok);
            loggerPanic("Unexpected character at line: %d\n", tok->line);
    }

    next = CctrlTokenPeek(cc);
    if (TokenPunctIs(next,'[')) {
        CctrlTokenRewind(cc);
        tok = CctrlTokenGet(cc);
        PrsAsmOffset(cc,buf,tok);
    }
}

static void maybePutRegister(Cctrl *cc, aoStr *buf, aoStr *maybe_register) {
    if (DictGetLen(cc->x86_registers,
                maybe_register->data,maybe_register->len) != NULL) {
        aoStrCatPrintf(buf,"%%%s",maybe_register->data);
    } else {
        aoStrCatLen(buf,maybe_register->data,maybe_register->len);
    }
}

/* Custom assembly to AT&T */
Ast *PrsAsmToATT(Cctrl *cc) {
    aoStr *op1, *op2, *op3, *asm_code, *curblock, *stdfunc, *curfunc;
    List *asm_functions;
    Ast *asm_function, *asm_block;
    lexeme *tok, *next;
    int isbol = 0, count = 0;

    asm_functions = ListNew();
    curblock = NULL;
    asm_code = aoStrNew();
    memset(asm_code->data,'\0',asm_code->capacity-1);

    CctrlTokenExpect(cc, '{');
    tok = CctrlTokenGet(cc);

    while (!TokenPunctIs(tok, '}')) {
        switch (tok->tk_type) {
            case TK_I64: {
                switch (count) {
                    case 1:
                        count++;
                        op2 = aoStrNew();
                        PrsAsmImm(cc,op2,tok);
                        break;
                    case 2:
                        count++;
                        op3 = aoStrNew();
                        PrsAsmImm(cc,op3,tok);
                        break;
                }
                break;
            }

            case TK_IDENT: {
                next = CctrlTokenPeek(cc);
                if (TokenPunctIs(next, TK_DBL_COLON) || TokenPunctIs(next,':')) {
                    CctrlTokenGet(cc);
                    CctrlTokenExpect(cc, '\n');

                    if (curblock == NULL) {
                        curblock = aoStrNew();
                    } else {
                        /* Save the current function */
                        aoStrCatLen(asm_code,curfunc->data,curfunc->len);
                        aoStrCatLen(asm_code,":\n",2);
                        aoStrCatLen(asm_code,curblock->data,curblock->len);
                        aoStrPutChar(asm_code,'\n');
                        /* Create an ast */
                        asm_function = AstAsmFunctionDef(curfunc,curblock);
                        ListAppend(asm_functions,asm_function);
                        curblock = aoStrNew();
                    }

                    /* Just the function name in op1, this is so it can be 
                     * called in the c code. */
                    curfunc = aoStrDupRaw(tok->start,tok->len);
                    /* Save the name for pasting labels */
                    cc->tmp_asm_fname = curfunc;
                    tok = CctrlTokenGet(cc);
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
                                if (DictGetLen(cc->libc_functions,
                                            op1->data,op1->len) != NULL) {
                                    stdfunc = AstNormaliseFunctionName(op1->data);
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
                                    if (DictGetLen(cc->libc_functions,
                                                op2->data,op2->len) != NULL) { 
                                        stdfunc = AstNormaliseFunctionName(op2->data);
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
                                loggerPanic("Unexpected number of arguments for"
                                        " x86 transpilation: %d"" Expression\n", 
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
                                PrsAsmPunct(cc,tok,op1);
                                break;
                            case 1:
                                op2 = aoStrNew();
                                PrsAsmPunct(cc,tok,op2);
                                break;
                            case 2:
                                op3 = aoStrNew();
                                PrsAsmPunct(cc,tok,op3);
                                break;
                        }
                        count++;
                        break;
                }
                break;
            }

        }
        tok = CctrlTokenGet(cc);
    }

    aoStrCatLen(asm_code,curfunc->data,curfunc->len);
    aoStrCatLen(asm_code,":\n",2);
    aoStrCatLen(asm_code,curblock->data,curblock->len);
    asm_function = AstAsmFunctionDef(curfunc,curblock);
    ListAppend(asm_functions,asm_function);
    asm_block = AstAsmBlock(asm_code,asm_functions);
    return asm_block;
}

/* The next token is expected to be '{' so the parser has just seen 'asm' */
Ast *PrsAsm(Cctrl *cc) {
    return PrsAsmToATT(cc);
}

#ifdef PRSASM_TEST

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
char *readfile(char *path) {
    char *buf;
    int fd;

    if ((fd = open(path, O_RDONLY, 0644)) == -1) {
        loggerPanic("Failed to open file\n");
    }

    int len = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);

    buf = malloc(sizeof(char) * len);
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

    cc = CctrlNew();
    file = readfile(argv[1]);

    lexerInit(&l, file);
    tokens = lexToLexemes(cc->macro_defs, &l);
    CctrlInitTokenIter(cc, tokens);

    while ((tok = CctrlTokenGet(cc)) != NULL) {
        if (TokenIdentIs(tok,"asm",3)) {
            peek = CctrlTokenPeek(cc);
            if (TokenPunctIs(peek,'{')) {
                asm_block = PrsAsm(cc);
                printf("%s\n", asm_block->asm_stmt->data);
            }
        }
    }
    free(file);
}

#endif
