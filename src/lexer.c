#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <unistd.h>

#include "aostr.h"
#include "dict.h"
#include "lexer.h"
#include "list.h"
#include "util.h"

typedef struct {
    char *name;
    int kind;
} LexerTypes;

/* Name, kind, size, issigned */
static LexerTypes lexer_types[] = {
    /* Holyc Types, this language is semi interoperable */
    {"U0", KW_U0},
    {"Bool", KW_BOOL},
    {"I8", KW_I8},
    {"U8", KW_U8},
    {"I16", KW_I16},
    {"U16", KW_U16},
    {"I32", KW_I32},
    {"U32", KW_U32},
    {"I64", KW_I64},
    {"U64", KW_U64},
    {"F64", KW_F64},

    {"auto", KW_AUTO},

    {"_extern", KW_ASM_EXTERN},
    {"extern", KW_EXTERN},
    {"asm", KW_ASM},

    {"switch", KW_SWITCH},
    {"case", KW_CASE},
    {"break", KW_BREAK},
    {"continue", KW_CONTINUE},
    {"while", KW_WHILE},
    {"do", KW_DO},
    {"for", KW_FOR},
    {"goto", KW_GOTO},
    {"default", KW_DEFAULT},
    {"if", KW_IF},
    {"else", KW_ELSE},
    {"ifndef", KW_IF_NDEF},
    {"endif", KW_ENDIF},
    {"elif", KW_ELIF},
    {"defined", KW_DEFINED},
    {"undef", KW_UNDEF},

    {"cast", KW_CAST},
    {"sizeof", KW_SIZEOF},
    {"return", KW_RETURN},
    {"inline", KW_INLINE},
    {"atomic", KW_ATOMIC},
    {"volatile", KW_VOLATILE},

    {"public", KW_PUBLIC},
    {"private", KW_PRIVATE},
    {"class", KW_CLASS},
    {"union", KW_UNION},

    {"define", KW_DEFINE},
    {"include", KW_INCLUDE},
    {"static", KW_STATIC},
};

#define isNum(ch) ((ch) >= '0' && (ch) <= '9')
#define isHex(ch) \
    (isNum(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'))
#define toInt(ch)   (ch - '0')
#define toUpper(ch) ((ch >= 'a' && ch <= 'z') ? (ch - 'a' + 'A') : ch)
#define toHex(ch)   (toUpper(ch) - 'A' + 10)
#define isNumTerminator(ch) (!isNum(ch) && !isHex(ch) && ch != '.' && ch != 'x' \
        && ch != 'X')

lexeme *lexemeNew(char *start, int len) {
    lexeme *le = malloc(sizeof(lexeme));
    le->start = start;
    le->len = len;
    le->line = -1;
    le->tk_type = -1;
    return le;
}

lexeme *lexemeSentinal(void) {
    lexeme *le = malloc(sizeof(lexeme));
    le->start = "(-sentinal-)";
    le->len = 12;
    le->line = 1;
    le->tk_type = -1;
    return le;
}

void lexemeAssignOp(lexeme *le, char *start, int len, long op, int line) {
    le->tk_type = TK_PUNCT;
    le->line = line;
    le->start = start;
    le->len = len;
    le->i64 = op; /* instead of 'char'*/
}

lexeme *lexemeTokNew(char *start, int len, int line, long ch) {
    lexeme *copy = malloc(sizeof(lexeme));
    copy->tk_type = TK_PUNCT;
    copy->start = start;
    copy->len = len;
    copy->line = line;
    copy->i64 = ch;
    return copy;
}

lexeme *lexemeCopy(lexeme *le) {
    lexeme *copy = malloc(sizeof(lexeme));
    memcpy(copy,le,sizeof(lexeme));
    return copy;
}

lexeme *lexemeNewOp(char *start, int len, long op, int line) {
    lexeme *le = lexemeNew(start,len);
    lexemeAssignOp(le,start,len,op,line);
    return le;
}

void lexerInit(lexer *l, char *source) {
    l->ptr = source;
    l->cur_ch = -1;
    l->lineno = 1;
    l->cur_f64 = 0;
    l->cur_i64 = 0;
    l->cur_str = NULL;
    l->cur_strlen = 0;
    l->flags = 0;
    l->ishex = 0;
    l->files = ListNew();
}

/* Is the lexeme both of type TK_PUNCT and does 'ch' match */
int TokenPunctIs(lexeme *tok, long ch) {
    return tok && (tok->tk_type == TK_PUNCT || tok->tk_type == TK_EOF) && tok->i64 == ch;
}

/* Is the token an identifier and does the string match */
int TokenIdentIs(lexeme *tok, char *ident, int len) {
    return tok && tok->tk_type == TK_IDENT 
               && tok->len == len 
               && !memcmp(tok->start,ident,len);
}

char *tokenTypeToString(int tk_type) {
    switch (tk_type) {
    case TK_IDENT:
        return "TK_IDENT";
    case TK_PUNCT:
        return "TK_PUNCT";
    case TK_I64:
        return "TK_I64";
    case TK_F64:
        return "TK_F64";
    case TK_EOF:
        return "TK_EOF";
    case TK_CHAR_CONST:
        return "TK_CHAR_CONST";
    case TK_STR:
        return "TK_STR";
    }
    return "UNKNOWN";
}

/* Convert all of this to a massive lookup table */
char *lexemePunctToString(long op) {
    aoStr *str = aoStrNew();
    switch (op) {
    case TK_AND_AND:     aoStrCatPrintf(str,"&&"); break;
    case TK_OR_OR:       aoStrCatPrintf(str,"||"); break;
    case TK_EQU_EQU:     aoStrCatPrintf(str,"=="); break;
    case TK_NOT_EQU:     aoStrCatPrintf(str,"!="); break;
    case TK_LESS_EQU:    aoStrCatPrintf(str,"<="); break;
    case TK_GREATER_EQU: aoStrCatPrintf(str,">="); break;
    case TK_PLUS_PLUS:   aoStrCatPrintf(str,"++"); break;
    case TK_MINUS_MINUS: aoStrCatPrintf(str,"--"); break;
    case TK_SHL:         aoStrCatPrintf(str,"<<"); break;
    case TK_SHR:         aoStrCatPrintf(str,">>"); break;
    case TK_ARROW:       aoStrCatPrintf(str,"->"); break;
    case TK_DBL_COLON:   aoStrCatPrintf(str,"::"); break;
    case TK_ELLIPSIS:    aoStrCatPrintf(str,"..."); break;
    default:
        if (op == '\n') {
            aoStrCatPrintf(str,"\\n");
        } else {
            aoStrCatPrintf(str,"%c", (char)op);
        }
        break;
    }
    return aoStrMove(str);
}

char *lexemeToString(lexeme *tok) {
    if (!tok) {
        return "(null)";
    }
    aoStr *str = aoStrNew();
    char *tmp;
    aoStrCatPrintf(str,"Line: %d\tType: ",tok->line);
    switch (tok->tk_type) {
        case TK_IDENT:
            aoStrCatPrintf(str,"TK_IDENT\t %.*s",tok->len,tok->start);
            return aoStrMove(str);
        case TK_CHAR_CONST:
            aoStrCatPrintf(str,"TK_CHAR_CONST\t%x",tok->i64);
            return aoStrMove(str);
        case TK_PUNCT: {
            aoStrCatPrintf(str,"TK_PUNCT\t");
            tmp = lexemePunctToString(tok->i64);
            aoStrCatPrintf(str,"%s",tmp);
            free(tmp);
            return aoStrMove(str);
        }
        case TK_I64:
            aoStrCatPrintf(str,"TK_I64\t%lld",tok->i64);
            return aoStrMove(str);
        case TK_F64:
            aoStrCatPrintf(str,"TK_F64\t%g",tok->f64);
            return aoStrMove(str);
        case TK_STR:
            aoStrCatPrintf(str,"TK_STR\t\"%.*s\"",tok->len,tok->start);
            return aoStrMove(str);
        case TK_EOF:
            aoStrCatPrintf(str,"TK_EOF");
            return aoStrMove(str);
        case TK_KEYWORD: {
            aoStrCatPrintf(str,"TK_KEYWORD\t");
            switch (tok->i64) {
                case KW_CLASS:       aoStrCatPrintf(str,"class");   break;
                case KW_UNION:       aoStrCatPrintf(str,"union");   break;
                case KW_U0:          aoStrCatPrintf(str,"U0");      break;
                case KW_BOOL:        aoStrCatPrintf(str,"Bool");    break;
                case KW_I8:          aoStrCatPrintf(str,"I8");      break;
                case KW_U8:          aoStrCatPrintf(str,"U8");      break;
                case KW_I16:         aoStrCatPrintf(str,"I16");     break;
                case KW_U16:         aoStrCatPrintf(str,"U16");     break;
                case KW_I32:         aoStrCatPrintf(str,"I32");     break;
                case KW_U32:         aoStrCatPrintf(str,"U32");     break;
                case KW_I64:         aoStrCatPrintf(str,"I64");     break;
                case KW_U64:         aoStrCatPrintf(str,"U64");     break;
                case KW_F64:         aoStrCatPrintf(str,"F64");     break;
                case KW_PUBLIC:      aoStrCatPrintf(str,"public");  break;
                case KW_ATOMIC:      aoStrCatPrintf(str,"atomic");  break;
                case KW_DEFINE:      aoStrCatPrintf(str,"define");  break;
                case KW_INCLUDE:     aoStrCatPrintf(str,"include"); break;
                case KW_CAST:        aoStrCatPrintf(str,"cast"); break;
                case KW_SIZEOF:      aoStrCatPrintf(str,"sizeof");  break;
                case KW_RETURN:      aoStrCatPrintf(str,"return");  break;
                case KW_SWITCH:      aoStrCatPrintf(str,"switch");  break;
                case KW_CASE:        aoStrCatPrintf(str,"case");    break;
                case KW_BREAK:       aoStrCatPrintf(str,"break");   break;
                case KW_CONTINUE:    aoStrCatPrintf(str,"continue"); break;
                case KW_ASM:         aoStrCatPrintf(str,"asm");     break;
                case KW_ASM_EXTERN:  aoStrCatPrintf(str,"_extern"); break;
                case KW_EXTERN:      aoStrCatPrintf(str,"extern");  break;
                case KW_PRIVATE:     aoStrCatPrintf(str,"private");  break;
                case KW_INLINE:      aoStrCatPrintf(str,"inline");  break;
                case KW_FOR:         aoStrCatPrintf(str,"for");     break;
                case KW_WHILE:       aoStrCatPrintf(str,"while");   break;
                case KW_VOLATILE:    aoStrCatPrintf(str,"volatile"); break;
                case KW_GOTO:        aoStrCatPrintf(str,"goto");    break;
                case KW_IF:          aoStrCatPrintf(str,"if");      break;
                case KW_ELSE:        aoStrCatPrintf(str,"else");    break;
                case KW_ELIF:        aoStrCatPrintf(str,"elif");    break;
                case KW_IF_NDEF:     aoStrCatPrintf(str,"ifndef");  break;
                case KW_IF_DEF:      aoStrCatPrintf(str,"ifdef");   break;
                case KW_ENDIF:       aoStrCatPrintf(str,"endif");   break;
                case KW_DEFINED:     aoStrCatPrintf(str,"defined"); break;
                case KW_UNDEF:       aoStrCatPrintf(str,"undef");   break;
                case KW_AUTO:        aoStrCatPrintf(str,"auto");    break;
                case KW_DEFAULT:     aoStrCatPrintf(str,"default"); break;
                case KW_DO:          aoStrCatPrintf(str,"do");      break;
                case KW_STATIC:      aoStrCatPrintf(str,"static");  break;
                default:
                    loggerPanic("Keyword %.*s: is not defined at line: %d\n",
                            tok->len,tok->start,tok->line);
            }
            return aoStrMove(str);
        }
    }
    loggerPanic("Unexpected type: %d\n", tok->tk_type);
}

/* Print one lexeme */
void lexemePrint(lexeme *le) {
    if (le) {
        char *str = lexemeToString(le);
        printf("%s\n", str);
        free(str);
    }
}

static char lexNextChar(lexer *l) {
    char ch = *l->ptr;
    if (ch == '\0') {
        return '\0';
    }
    l->cur_ch = ch;
    l->ptr++;
    return ch;
}

static void lexRewindChar(lexer *l) {
    l->ptr--;
}

/* Doese the next character match the expected character */
static int lexPeekMatch(lexer *l, char expected) {
    return *l->ptr == expected;
}

static char lexPeek(lexer *l) {
    return *l->ptr;
}

static void lexSkipCodeComment(lexer *l) {
    if (*l->ptr == '/') {
        while (*l->ptr != '\0') {
            if (*l->ptr == '\n') {
                break;
            }
            l->ptr++;
        }
    } else if (*l->ptr == '*') {
        while ((*l->ptr != '\0' && *(l->ptr + 1) != '\0')) {
            if (*l->ptr == '*' && *(l->ptr + 1) == '/') {
                l->ptr += 2;
                break;
            }
            if (*l->ptr == '\n') {
                l->lineno++;
            }
            l->ptr++;
        }
    }
}

static int countNumberLen(lexer *l, char *ptr, int *isfloat, int *ishex,
                          int *err) {
    char *start = ptr;
    int seen_e = 0;

    while (!isNumTerminator(*ptr)) {
        switch (*ptr) {
        case 'e':
        case 'E':
            if (!*ishex) {
                if (seen_e) {
                    loggerWarning("Hex and seen e\n");
                    *err = 1;
                    return -1;
                }
                seen_e = 1;
            }
            break;
        case 'x':
        case 'X':
            if (*ishex) {
                loggerWarning("seen x\n");
                *err = 1;
                return -1;
            }
            *ishex = 1;
            break;
        case '-':
        case '+':
            break;

        case '.':
            if (*isfloat) {
                *err = 1;
                return -1;
            }
            *isfloat = 1;
            break;
        /* Anything else is invalid */
        default:
            if (!isHex(*ptr)) {
                loggerWarning("Number errored with char: '%c'\n", *ptr);
                *err = 1;
                return -1;
            }
            break;
        }
        ptr++;
    }

    /* Floating point hex does not exist */
    if (*isfloat && *ishex) {
        *err = 1;
        loggerWarning("LEX error is float and ishex\n");
        return -1;
    }

    /* Exponent hex does not exist */
    if (*ishex && seen_e) {
        *err = 1;
        loggerWarning("LEX error seen_e and ishex\n");
        return -1;
    }

    return ptr - start;
}

int lexIdentifier(lexer *l, char ch) {
    int i = 0;
    while (1) {
        switch (ch) {
        case 'a' ... 'z':
        case 'A' ... 'Z':
        case '0' ... '9':
        case '$':
        case '_':
            i++;
            break;
        default:
            goto finish;
        }
        ch = lexNextChar(l);
    }

finish:
    l->cur_strlen = i;
    lexRewindChar(l);
    return TK_IDENT;
}

long lexInStr(lexer *l, unsigned char *buf, long size, int *done,
        char terminator, int escape_quotes)
{
    long i = 0, j = 0, k = 0, ch = 0;
    *done = 1;

    if (escape_quotes) {
        ch = lexNextChar(l);
    }

    while (i < size - 1) {
        ch = lexNextChar(l);
        if (escape_quotes && (ch == '"' && lexPeek(l) == '"')) {
            ch = lexNextChar(l);
            buf[i++] = '\0';
            return i;
        } else if (!escape_quotes && (!ch || ch == terminator)) {
            buf[i++] = '\0';
            return i;
        } else if (ch == '\\') {
            ch = lexNextChar(l);

            if (l->flags & CCF_ESCAPE_STRING_NEWLINES) {
                buf[i++] = '\\';
                buf[i++] = ch;
                goto out;
            }

            switch (ch) {
            case '\\':
                buf[i++] = '\\';
                buf[i++] = '\\';
                break;
            case '0':
                buf[i++] = '\\';
                buf[i++] = '0';
                break;
            case '\'':
                buf[i++] = '\\';
                buf[i++] = '\'';
                break;
            case '`':
                buf[i++] = '\\';
                buf[i++] = '`';
            case '"':
                buf[i++] = '\\';
                buf[i++] = '"';
                break;
            case 'd':
                buf[i++] = '\\';
                buf[i++] = 'd';
                break;
            /* XXX: This potentially breaks multi line strings*/
            case 'n':
                buf[i++] = '\\';
                buf[i++] = 'n';
                break;
            case 'r':
                buf[i++] = '\\';
                buf[i++] = 'r';
                break;
            case 't':
                buf[i++] = '\\';
                buf[i++] = 't';
                break;
            case 'x':
            case 'X':
                j = 0;
                for (k = 0; k < 2; k++) {
                    ch = toupper(lexNextChar(l));
                    if (isHex(ch)) {
                        if (ch <= '9') {
                            j = (j << 4) + ch - '0';
                        } else {
                            j = (j << 4) - 'A' + 10;
                        }
                    } 
                        break;
                }
                buf[i++] = j;
                break;
            default:
                buf[i++] = '\\';
            }
        } else {
            if (escape_quotes && ch == '\"') {
                buf[i++] = '\\';
            }
            buf[i++] = ch;
        }
    }
out:
    *done = 0;
    return i;
}

long lexString(lexer *l, char terminator, int escape_quotes) {
    char *buf2, *buf3, buf[128];
    int str_done = 0;
    long len, char_count;

    if (escape_quotes) {
        loggerDebug("excape\n");
    }
    len = char_count = 0;
    buf2 = buf3 = NULL;
    
    do {
        char_count = lexInStr(l, (unsigned char *)buf, sizeof(buf),
                &str_done,terminator,escape_quotes);
        buf3 = malloc(len+char_count);
        if (buf2) {
            memcpy(buf3,buf2,len);
            free(buf2);
            buf2 = buf3;
            memcpy(buf2+len,buf,char_count);
        } else {
            buf2 = buf3;
            memcpy(buf2,buf,char_count);
        }
        len += char_count;
    } while(!str_done);

    free(l->cur_str);
    l->cur_str = malloc(len);
    memcpy(l->cur_str,buf2,len);
    free(buf2);
    l->cur_strlen = len;
    return TK_STR;
}

/* Length of the char const is returned, it OR's in at max 8 characters. 
 * A long being 64 bits and 64/8 = 8. */
unsigned long lexCharConst(lexer *l) {
    unsigned long char_const = 0;
    long hex_num = 0;
    long len;
    char ch;

    for (len = 0; len < LEX_CHAR_CONST_LEN; ++len) {
        ch = lexNextChar(l);
        if (!ch || ch == '\'') {
            break;
        }
        if (ch == '\\') {
            ch = lexNextChar(l);
            switch (ch) {
                case '0':  char_const |= '\0' << (len * 8); break;
                case '\'': char_const |= '\'' << (len * 8); break;
                case '`':  char_const |= '`'  << (len * 8); break;
                case '\"': char_const |= '\"' << (len * 8); break;
                case 'd':  char_const |= '$'  << (len * 8); break;
                case 'n':  char_const |= '\n' << (len * 8); break;
                case 'r':  char_const |= '\r' << (len * 8); break;
                case 't':  char_const |= '\t' << (len * 8); break;
                case 'x':
                case 'X':
                    for (int i = 0; i < 2; ++i) {
                        ch = toupper(lexNextChar(l));
                        if (isHex(ch)) {
                            if (ch <= '9') {
                                hex_num |= (hex_num<<4)+ch-'0';
                            } else {
                                hex_num |= (hex_num<<4)+ch-'A'+10;
                            }
                        } else {
                            break;
                        }
                    }
                    char_const |= hex_num << (len * 8);
                    break;
                default:
                    char_const |= '\\' << (len * 8);
                    break;
            }
        } else {
            loggerDebug("[%ld]%c => 0x%x\n",len*8,ch,ch);
            char_const |= (unsigned long)(((unsigned long)ch) << ((unsigned long)(len * 8)));
        }
    }

    if (ch != '\'' && lexPeek(l) != '\'') {
        loggerPanic("Char const limited to 8 characters! at line: %d\n",
                l->lineno);
    }

    /* Consume next character if it is the end of the char const */
    if (lexPeek(l) == '\'') {
        lexNextChar(l);
    } 
    loggerDebug("%lx\n",char_const);
    l->cur_i64 = char_const;
    l->cur_strlen = len;
    return TK_CHAR_CONST;
}

int lexNumeric(lexer *l, int _isfloat) {
    int ishex, isfloat, err, numlen;
    char *endptr;

    isfloat = _isfloat;

    ishex = isfloat = err = 0;
    char *start = l->ptr - 1;
    numlen = countNumberLen(l, start, &isfloat, &ishex, &err);
    if (err) {
        return -1;
    }

    l->ptr += numlen - 1;
    if (isfloat) {
        l->cur_f64 = strtold(start, &endptr);
        return TK_F64;
    } else if (ishex) {
        l->cur_i64 = strtoull(start, &endptr, 16);
        l->ishex = ishex;
        return TK_I64;
    } else {
        l->cur_i64 = strtoll(start, &endptr, 10);
        return TK_I64;
    }
}

int lex(lexer *l, lexeme *le) {
    char ch, *start;
    int tk_type;

    while (1) {
        start = l->ptr;
        ch = lexNextChar(l);

        switch (ch) {
        case '\n':
            l->lineno++;
            if (l->flags & CCF_ACCEPT_NEWLINES) {
                lexemeAssignOp(le,start,1,ch,l->lineno);
                return 1;
            }
            break;

        case '\0':
            return 0;

        case 'a' ... 'z':
        case 'A' ... 'Z':
        case '_':
            if ((tk_type = lexIdentifier(l, ch)) == -1) {
                loggerPanic("Lex error while lexing lexIdentifier\n");
                goto error;
            }

            le->start = start;
            le->len = l->cur_strlen;
            le->tk_type = tk_type;
            le->line = l->lineno;
            return 1;

        case '0' ... '9':
            if ((tk_type = lexNumeric(l,0)) == -1) {
                loggerPanic("Lex error while lexing lexNumeric\n");
                goto error;
            }
            le->len = l->ptr - start;
            le->tk_type = tk_type;
            le->line = l->lineno;
            if (tk_type == TK_F64) {
                le->f64 = l->cur_f64;
            } else {
                le->i64 = l->cur_i64;
            }
            le->ishex = l->ishex;
            l->ishex = 0;
            return 1;

        case '\"':
            if (lexPeek(l) == '"') {
                lexString(l,'"',1);
            } else {
                lexString(l,'"',0);
            }
            le->start = l->cur_str;
            le->len = l->cur_strlen;
            l->cur_str = NULL;
            l->cur_strlen = 0;
            le->tk_type = TK_STR;
            le->line = l->lineno;
            return 1;
        
        case '\'':
            lexCharConst(l);
            le->start = start+1;
            le->len = l->cur_strlen;
            le->i64 = l->cur_i64;
            le->tk_type = TK_CHAR_CONST;
            l->cur_str = NULL;
            l->cur_strlen = 0;
            le->line = l->lineno;
            return 1;

        case '/':
            if (*l->ptr == '/' || *l->ptr == '*') {
                lexSkipCodeComment(l);
            } else {
                if (lexPeekMatch(l,'=')) {
                    lexNextChar(l);
                    lexemeAssignOp(le,start,2,TK_DIV_EQU,l->lineno);
                    return 1;
                } else {
                /* Divide */
                    lexemeAssignOp(le,start,1,ch,l->lineno);
                    return 1;
                }
            }
            break;

        case '=':
            /* Check for equality */
            if (lexPeekMatch(l,'=')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_EQU_EQU,l->lineno);
            } else {
                lexemeAssignOp(le,start,1,ch,l->lineno);
            }
            return 1;
        
        case '<':
            if (lexPeekMatch(l,'=')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_LESS_EQU,l->lineno);
            } else if (lexPeekMatch(l, '<')) {
                lexNextChar(l);
                if (lexPeekMatch(l, '=')) {
                    lexNextChar(l);
                    lexemeAssignOp(le,start,2,TK_SHL_EQU,l->lineno);
                } else {
                    lexemeAssignOp(le,start,2,TK_SHL,l->lineno);
                }
            } else {
                lexemeAssignOp(le,start,1,ch,l->lineno);
            }
            return 1;
        
        case '>':
            if (lexPeekMatch(l,'=')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_GREATER_EQU,l->lineno);
            } else if (lexPeekMatch(l, '>')) {
                lexNextChar(l);
                if (lexPeekMatch(l, '=')) {
                    lexNextChar(l);
                    lexemeAssignOp(le,start,2,TK_SHR_EQU,l->lineno);
                } else {
                    lexemeAssignOp(le,start,2,TK_SHR,l->lineno);
                }
            } else {
                lexemeAssignOp(le,start,1,ch,l->lineno);
            }
            return 1;

        case '+':
            if (lexPeekMatch(l,'+')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_PLUS_PLUS,l->lineno);
            } else if (lexPeekMatch(l, '=')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_ADD_EQU,l->lineno);
            } else {
                lexemeAssignOp(le,start,1,ch,l->lineno);
            }
            return 1;

        case '-':
            if (lexPeekMatch(l,'-')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_MINUS_MINUS,l->lineno);
            } else if (lexPeekMatch(l, '>')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_ARROW,l->lineno);
            } else if (lexPeekMatch(l, '=')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_SUB_EQU,l->lineno);
            } else {
                lexemeAssignOp(le,start,1,ch,l->lineno);
            }
            return 1;
        
        case '!':
            if (lexPeekMatch(l,'=')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_NOT_EQU,l->lineno);
            } else {
                lexemeAssignOp(le,start,1,ch,l->lineno);
            }
            return 1;

        case '&':
            if (lexPeekMatch(l,'&')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_AND_AND,l->lineno);
            } else if (lexPeekMatch(l, '=')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_AND_EQU,l->lineno);
            } else {
                lexemeAssignOp(le,start,1,ch,l->lineno);
            }
            return 1;

        case '|':
            if (lexPeekMatch(l,'|')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_OR_OR,l->lineno);
            } else if (lexPeekMatch(l, '=')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_OR_EQU,l->lineno);
            } else {
                lexemeAssignOp(le,start,1,ch,l->lineno);
            }
            return 1;

        case '*':
            if (lexPeekMatch(l, '=')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_MUL_EQU,l->lineno);
            } else {
                lexemeAssignOp(le,start,1,ch,l->lineno);
            }
            return 1;

        case '%':
            if (lexPeekMatch(l, '=')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_MOD_EQU,l->lineno);
            } else {
                lexemeAssignOp(le,start,1,ch,l->lineno);
            }
            return 1;
        
        case '^':
            if (lexPeekMatch(l, '=')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_XOR_EQU,l->lineno);
            } else {
                lexemeAssignOp(le,start,1,ch,l->lineno);
            }
            return 1;

        case '$':
        case '@':
            if (l->flags & CCF_ASM_BLOCK) {
                lexemeAssignOp(le,start,1,ch,l->lineno);
                return 1;
            }
            break;
        case '.':
            if (lexPeekMatch(l,'.')) {
                if (isNum(lexPeek(l))) {
                    if ((tk_type = lexNumeric(l,1)) == -1) {
                        loggerPanic("Lex error while lexing lexNumeric\n");
                        goto error;
                    }
                    le->len = l->ptr - start;
                    le->tk_type = tk_type;
                    le->line = l->lineno;
                    le->ishex = 0;
                    le->f64 = l->cur_f64;
                    return 1;
                } else {
                    lexNextChar(l);
                    if (lexPeekMatch(l,'.')) {
                        lexNextChar(l);
                        lexemeAssignOp(le,start,3,TK_ELLIPSIS,l->lineno);
                        return 1;
                    }
                    loggerPanic(".. is an invalid token sequence at line: %d\n"
                            , l->lineno);
                }
            }
            lexemeAssignOp(le,start,1,ch,l->lineno);
            return 1;

        case '#':
        case '~':
        case '(':
        case ')':
        case ',':
        case ';':
        case ':':
            if (l->flags & CCF_MULTI_COLON && lexPeekMatch(l,':')) {
                lexNextChar(l);
                lexemeAssignOp(le,start,2,TK_DBL_COLON,l->lineno);
                return 1;
            }
        case '[':
        case ']':
        case '{':
        case '}':
            lexemeAssignOp(le,start,1,ch,l->lineno);
            return 1;
        }
    }
error:
    loggerPanic("Lexer error\n");
    return 0;
}

char *lexGetBuiltInRoot(void) {
    struct passwd *pwd = getpwuid(getuid());
    aoStr *full_path = aoStrNew();
    aoStrCatPrintf(full_path, "%s/.holyc-lib", pwd->pw_dir);
    return aoStrMove(full_path);
}

int lexHasFile(lexer *l, aoStr *file) {
    aoStr *tmp;
    for (List *it = l->files->next; it != l->files; it = it->next) {
        tmp = it->value;
        if (!aoStrCmp(tmp,file)) {
            return 1;
        }
    }
    return 0;
}

List *lexUntil(Dict *macro_defs, lexer *l, char to) {
    List *tokens; 
    Dict *symbol_table = DictNew(&default_table_type);
    LexerTypes *bilt;
    DictSetFreeKey(symbol_table, NULL);

    /* XXX: create one symbol table for the whole application ;
     * hoist to 'compile.c'*/
    for (int i = 0; i < static_size(lexer_types); ++i) {
        bilt = &lexer_types[i]; 
        DictSet(symbol_table, bilt->name, bilt);
    }

    int ok,tmp_len;
    lexeme le,next,*copy;
    char *builtin_root, prevous_to, tmp[128];
    aoStr *ident, *include_path;

    prevous_to = to;
    builtin_root = lexGetBuiltInRoot();
    tokens = ListNew();

    while (1) {
        ok = lex(l,&le);
        if (!ok) {
            break;
        }

        if (l->flags & (CCF_ASM_BLOCK) && TokenPunctIs(&le, to)) {
            copy = lexemeCopy(&le);
            ListAppend(tokens, copy);
            /* turn off assembly lexing */
            l->flags &= ~(CCF_MULTI_COLON|CCF_ACCEPT_NEWLINES|CCF_ASM_BLOCK);
            to = prevous_to;
            continue;
        }
        
        if (le.tk_type == TK_IDENT) {
            if ((bilt = DictGetLen(symbol_table,le.start,le.len)) != NULL) {
                le.tk_type = TK_KEYWORD;
                le.i64 = bilt->kind;


                /* Build in seeing assembly into the lexer, feels less bad than doing 
                 * multiple passes and fast forwarding to '}'. Which is what I was 
                 * previously doing */
                switch (le.i64) {
                    case KW_ASM:
                        lex(l,&next);
                        if (!TokenPunctIs(&next,'{')) {
                            loggerPanic("asm '{' expected at line: %d. Got: %s\n",
                                    next.line, lexemeToString(&next));
                        }
                        copy = lexemeCopy(&le);
                        ListAppend(tokens, copy);
                        copy = lexemeCopy(&next);
                        ListAppend(tokens, copy);

                        l->flags |= (CCF_MULTI_COLON|CCF_ACCEPT_NEWLINES|CCF_ASM_BLOCK);

                        to = '}';
                        continue;
                }
            }
            copy = lexemeCopy(&le);
            ListAppend(tokens, copy);
            continue;
        }

        if (l->flags & CCF_PRE_PROC && TokenPunctIs(&le, '#')) {
            if (lex(l,&next) && next.tk_type == TK_IDENT) {
                if ((bilt = DictGetLen(symbol_table,next.start,next.len)) != NULL) {
                    switch (bilt->kind) {
                        case KW_INCLUDE: {
                            lex(l, &next);
                            if (TokenPunctIs(&next, '<')) {
                                lex(l, &next);
                                ident = aoStrNew();
                                do {
                                    aoStrCatPrintf(ident, "%.*s", next.len,
                                                   next.start);
                                    lex(l, &next);
                                } while (next.i64 != '>');
                                include_path = aoStrNew();
                                aoStrCatPrintf(include_path, "%s/%s",
                                               builtin_root, ident->data);
                                aoStrRelease(ident);
                            } else if (next.tk_type == TK_STR) {
                                include_path = aoStrDupRaw(next.start, next.len);
                            } else {
                                loggerPanic(
                                        "Syntax is: #include \"<value>\" got: %s\n",
                                        lexemeToString(&next));
                            }

                            if (!lexHasFile(l, include_path)) {
                                ListAppend(l->files, include_path);
                            } else {
                                aoStrRelease(include_path);
                            }
                            break;
                        }

                        case KW_DEFINE: {
                            /* <ident> <value> */
                            lex(l, &next);
                            if (next.tk_type != TK_IDENT) {
                                loggerPanic(
                                        "Syntax is: #define <TK_IDENT> <value>\n");
                            }
                            ident = aoStrDupRaw(next.start, next.len);

                            lex(l, &next);
                            copy = lexemeCopy(&next);
                            DictSet(macro_defs, ident->data, copy);
                            break;
                        }

                        case KW_UNDEF: {
                            lex(l, &next);
                            if (next.tk_type != TK_IDENT) {
                                loggerPanic(
                                        "Syntax is: #undef <TK_IDENT> <value>\n");
                            }
                            tmp_len = snprintf(tmp,sizeof(tmp),"%.*s",
                                    next.len,next.start);
                            tmp[tmp_len] = '\0';
                            DictDelete(macro_defs,tmp);
                            break;
                        }

                        case KW_IF_DEF:
                        case KW_IF_NDEF:
                            loggerPanic("#ifdef or #ifndef unimplemented at line: %d\n", le.line);
                            break;
                        default:
                            loggerPanic("#%.*s unimplemented at line: %d\n",le.len,le.start, le.line);
                    }
                }
            }
        } else {
            copy = lexemeCopy(&le);
            ListAppend(tokens, copy);
        }
    }
    free(builtin_root);
    DictRelease(symbol_table);
    return tokens;
}

List *lexToLexemes(Dict *defs, lexer *l) {
    return lexUntil(defs,l,'\0');
}

void lexemeFree(void *_le) {
    if (_le) {
        lexeme *le = (lexeme *)_le;
        free(le);
    }
}

void lexemeListRelease(List *tokens) {
    ListRelease(tokens,lexemeFree);
}

void lexemePrintList(List *tokens) {
    List *it = tokens->next;
    while (it != tokens) {
        lexemePrint(it->value);
        it = it->next;
    }
}
