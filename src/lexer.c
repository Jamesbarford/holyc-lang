#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <unistd.h>
#include <fcntl.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "map.h"
#include "lexer.h"
#include "list.h"
#include "prslib.h"
#include "prsutil.h"
#include "util.h"

/* prototypes */
int lexPreProcIf(StrMap *macro_defs, lexer *l);

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
    {"define", KW_DEFINE},
    {"ifndef", KW_IF_NDEF},
    {"ifdef", KW_IF_DEF},
    {"elifdef", KW_ELIF_DEF},
    {"endif", KW_ENDIF},
    {"elif", KW_ELIF},
    {"defined", KW_DEFINED},
    {"undef", KW_UNDEF},

    {"#if", KW_PP_IF},
    {"#else", KW_PP_ELSE},
    {"#define", KW_PP_DEFINE},
    {"#ifndef", KW_PP_IF_NDEF},
    {"#ifdef", KW_PP_IF_DEF},
    {"#elifdef", KW_PP_ELIF_DEF},
    {"#endif", KW_PP_ENDIF},
    {"#elif", KW_PP_ELIF},
    {"#defined", KW_PP_DEFINED},
    {"#undef", KW_PP_UNDEF},
    {"#error", KW_PP_ERROR},
    {"#include", KW_PP_INCLUDE},

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

    {"static", KW_STATIC},
};


#define isNum(ch) ((ch) >= '0' && (ch) <= '9')
#define isHex(ch) \
    (isNum(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'))
#define toInt(ch)   (ch - '0')
#define toUpper(ch) ((ch >= 'a' && ch <= 'z') ? (ch - 'a' + 'A') : ch)
#define toHex(ch)   (toUpper(ch) - 'A' + 10)
#define isNumTerminator(ch) (!isNum(ch) && !isHex(ch) && ch != '.' && ch != 'x' \
        && ch != 'X' && ch != '\\')

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

static Cctrl *macro_proccessor = NULL;

void lexInit(lexer *l, char *source, int flags) {
    l->ptr = source;
    l->cur_ch = -1;
    l->lineno = 1;
    l->cur_f64 = 0;
    l->cur_i64 = 0;
    l->cur_str = NULL;
    l->cur_strlen = 0;
    l->cur_file = NULL;
    l->ishex = 0;
    l->flags = flags;
    l->files = listNew();
    l->all_source = listNew();
    l->symbol_table = strMapNew(32);
    l->symbol_table->_free_key = NULL;
    l->collecting = 1;
    l->skip_else = 1;
    if (macro_proccessor == NULL) {
        macro_proccessor = ccMacroProcessor(NULL);
    }
    /* XXX: create one symbol table for the whole application ;
     * hoist to 'compile.c'*/
    for (int i = 0; i < static_size(lexer_types); ++i) {
        LexerTypes *bilt = &lexer_types[i]; 
        strMapAdd(l->symbol_table, bilt->name, bilt);
    }
}

void lexSetBuiltinRoot(lexer *l, char *root) {
    l->builtin_root = root;
}

/* Is the lexeme both of type TK_PUNCT and does 'ch' match */
int tokenPunctIs(lexeme *tok, long ch) {
    return tok && (tok->tk_type == TK_PUNCT || tok->tk_type == TK_EOF) && tok->i64 == ch;
}

/* Is the token an identifier and does the string match */
int tokenIdentIs(lexeme *tok, char *ident, int len) {
    return tok && tok->tk_type == TK_IDENT 
               && tok->len == len 
               && !memcmp(tok->start,ident,len);
}

char *lexemeTypeToString(int tk_type) {
    switch (tk_type) {
    case TK_IDENT: return "TK_IDENT";
    case TK_PUNCT: return "TK_PUNCT";
    case TK_I64:   return "TK_I64";
    case TK_F64:   return "TK_F64";
    case TK_EOF:   return "TK_EOF";
    case TK_CHAR_CONST: return "TK_CHAR_CONST";
    case TK_STR:   return "TK_STR";
    }
    return "UNKNOWN";
}

/* This is for something that will play nicely with HTML or GraphViz*/
char *lexemePunctToEncodedString(long op) {
    static char buf[4];
    switch(op) {
    case '\\':               return "\\";
    case '\n':               return "\\n";
    case '\t':               return "\\t";
    case '\r':               return "\\r";
    case '\"':               return "&#34;";
    case '\'':               return "&#39;";
    case ' ':                return "&#32;";
    case '>':                return "&#62;";
    case '<':                return "&#60;";
    case '&':                return "&#38;";
    case '|':                return "&#124;";
    case ';':                return "&#59;";
    case '@':                return "&#64;";
    case '=':                return "&#61;";
    case '(':                return "&#40;";
    case ')':                return "&#41;";
    case '~':                return "&#126;";
    case '^':                return "&#94;";
    case '_':                return "&#65;";
    case '!':                return "&#33;";
    case '{':                return "&#123;";
    case '}':                return "&#125;";
    case '[':                return "&#91;";
    case ']':                return "&#93;";
    case '`':                return "&#96;";
    case '*':                return "&#42;";
    case '+':                return "&#43;";
    case '-':                return "&#45;";
    case '/':                return "&#47;";
    case '%':                return "&#37;";
    case '$':                return "&#36;";
    case '#':                return "&#35;";
    case ',':                return "&#44;";
    case '.':                return "&#46;";
    case '?':                return "&#63;";
    case ':':                return "&#48;";

    case TK_EQU_EQU:         return "&#61;&#61;";
    case TK_NOT_EQU:         return "&#33;&#61;";
    case TK_LESS_EQU:        return "&#60;&#61;";
    case TK_GREATER_EQU:     return "&#62;&#61;";      
    case TK_AND_AND:         return "&#38;&#38;";
    case TK_OR_OR:           return "&#124;&#124;";
    case TK_SHL:             return "&#60;&#60;";
    case TK_SHL_EQU:         return "&#60;&#60;&#61;";
    case TK_SHR:             return "&#62;&#62;";
    case TK_SHR_EQU:         return "&#62;&#62;&#61;";
    case TK_MUL_EQU:         return "&#42;&#61;";
    case TK_DIV_EQU:         return "&#47;&#61;";
    case TK_OR_EQU:          return "&#124;&#61;";
    case TK_XOR_EQU:         return "&#94;&#61;";
    case TK_AND_EQU:         return "&#38;&#61;";
    case TK_SUB_EQU:         return "&#45;&#61;";
    case TK_ADD_EQU:         return "&#43;&#61;";
    case TK_MOD_EQU:         return "&#37;&#61;";
    case TK_ELLIPSIS:        return "&#48;&#48;&#48;";
    case TK_ARROW:           return "&#45;&#62;";
    case TK_PRE_PLUS_PLUS:   return "&#43;&#43;";
    case TK_PLUS_PLUS:       return "&#43;&#43;";
    case TK_PRE_MINUS_MINUS: return "&#45;&#45;";
    case TK_MINUS_MINUS:     return "&#45;&#45;";
    default: {
        int len = snprintf(buf,sizeof(buf),"%c",(char)op);
        buf[len] = '\0';
        return buf;
    }
    }
}

/* Convert all of this to a massive lookup table */
char *lexemePunctToString(long op) {
    static char buf[4];
    switch (op) {
    case '\\':               return "\\";
    case '\n':               return "\\n";
    case '\t':               return "\\t";
    case '\r':               return "\\r";
    case '\"':               return "\\\"";
    case '\'':               return "\\\'";
    case TK_AND_AND:         return "&&";
    case TK_OR_OR:           return "||";
    case TK_EQU_EQU:         return "==";
    case TK_NOT_EQU:         return "!=";
    case TK_LESS_EQU:        return "<=";
    case TK_GREATER_EQU:     return ">=";
    case TK_PLUS_PLUS:       return "++";
    case TK_MINUS_MINUS:     return "--";
    case TK_SHL:             return "<<";
    case TK_SHR:             return ">>";
    case TK_ARROW:           return "->";
    case TK_DBL_COLON:       return "::";
    case TK_ELLIPSIS:        return "...";
    case TK_SHL_EQU:         return "<<=";
    case TK_SHR_EQU:         return ">>=";
    case TK_MUL_EQU:         return "*=";
    case TK_DIV_EQU:         return "/=";
    case TK_OR_EQU:          return "|=";
    case TK_XOR_EQU:         return "^=";
    case TK_AND_EQU:         return "&=";
    case TK_SUB_EQU:         return "-=";
    case TK_ADD_EQU:         return "+=";
    case TK_MOD_EQU:         return "%=";
    case TK_PRE_PLUS_PLUS:   return "++";
    case TK_PRE_MINUS_MINUS: return "--";
    default: {
        int len = snprintf(buf,sizeof(buf),"%c",(char)op);
        buf[len] = '\0';
        return buf;
    }
    }
}

char *lexemePunctToStringWithFlags(long op, unsigned long flags) {
    if (flags & LEXEME_ENCODE_PUNCT) return lexemePunctToEncodedString(op);
    else                             return lexemePunctToString(op);
}

char *lexemeToString(lexeme *tok) {
    if (!tok) {
        return "(null)";
    }
    aoStr *str = aoStrNew();
    char *tmp;
    switch (tok->tk_type) {
        case TK_IDENT:
            aoStrCatPrintf(str,"TK_IDENT      %.*s",tok->len,tok->start);
            return aoStrMove(str);
        case TK_CHAR_CONST:
            aoStrCatPrintf(str,"TK_CHAR_CONST %x",tok->i64);
            return aoStrMove(str);
        case TK_PUNCT: {
            tmp = lexemePunctToString(tok->i64);
            aoStrCatPrintf(str,"TK_PUNCT      %s", tmp);
            return aoStrMove(str);
        }
        case TK_I64:
            aoStrCatPrintf(str,"TK_I64        %lld",tok->i64);
            return aoStrMove(str);
        case TK_F64:
            aoStrCatPrintf(str,"TK_F64        %g",tok->f64);
            return aoStrMove(str);
        case TK_STR:
            aoStrCatPrintf(str,"TK_STR        \"%.*s\"",tok->len,tok->start);
            return aoStrMove(str);
        case TK_EOF:
            aoStrCatPrintf(str,"TK_EOF");
            return aoStrMove(str);
        case TK_KEYWORD: {
            aoStrCatPrintf(str,"TK_KEYWORD    ");
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
                case KW_PP_INCLUDE:     aoStrCatPrintf(str,"include"); break;
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
                case KW_ELIF_DEF:    aoStrCatPrintf(str,"elifdef");   break;
                case KW_ENDIF:       aoStrCatPrintf(str,"endif");   break;
                case KW_UNDEF:       aoStrCatPrintf(str,"undef");   break;
                case KW_AUTO:        aoStrCatPrintf(str,"auto");    break;
                case KW_DEFAULT:     aoStrCatPrintf(str,"default"); break;
                case KW_DO:          aoStrCatPrintf(str,"do");      break;
                case KW_STATIC:      aoStrCatPrintf(str,"static");  break;
                case KW_DEFINED:     aoStrCatPrintf(str,"defined"); break;

                case KW_PP_IF: aoStrCatPrintf(str,"#if"); break;   
                case KW_PP_ELSE: aoStrCatPrintf(str,"#else"); break;   
                case KW_PP_DEFINE: aoStrCatPrintf(str,"#define"); break;  
                case KW_PP_IF_NDEF: aoStrCatPrintf(str,"#ifndef"); break; 
                case KW_PP_IF_DEF: aoStrCatPrintf(str,"#ifdef"); break;
                case KW_PP_ELIF_DEF:aoStrCatPrintf(str,"#elifdef"); break;
                case KW_PP_ENDIF: aoStrCatPrintf(str,"#endif"); break;   
                case KW_PP_ELIF: aoStrCatPrintf(str,"#elif"); break;    
                case KW_PP_DEFINED: aoStrCatPrintf(str,"#defined"); break; 
                case KW_PP_UNDEF:  aoStrCatPrintf(str,"#undef"); break;  
                case KW_PP_ERROR: aoStrCatPrintf(str,"#error"); break;   

                default:
                    loggerPanic("line %d: Keyword %.*s: is not defined\n",
                            tok->line,tok->len,tok->start);
            }
            return aoStrMove(str);
        }
    }
    loggerPanic("line %d: Unexpected type %s |%.*s|\n",
            tok->line,lexemeTypeToString(tok->tk_type),tok->len,tok->start);
}

/* Print one lexeme */
void lexemePrint(lexeme *le) {
    if (le) {
        char *str = lexemeToString(le);
        printf("%s\n", str);
      //  free(str);
    }
}

static char lexNextChar(lexer *l) {
    l->start = l->ptr;
    lexFile *lex_file;
    char ch = '\0';
    if (l->ptr) {
        ch = *l->ptr;
    }
    if (ch == '\0') {
        if (listEmpty(l->files)) return '\0';
        if ((lex_file = listPop(l->files)) == NULL) {
            return '\0';
        }
        l->cur_file = lex_file;
        l->ptr = lex_file->ptr; 
        l->lineno = lex_file->lineno;
        ch = *l->ptr;
        l->cur_ch = ch;
        l->start = l->ptr;
        l->ptr++;
        return ch;
    } else {
        l->cur_ch = ch;
        l->start = l->ptr;
        l->ptr++;
    }
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

/* Read an entire file to a mallocated buffer */
char *lexReadfile(char *path, ssize_t *_len) {
    char *buf;
    int fd,rbytes,len,size;

    if ((fd = open(path, O_RDONLY, 0644)) == -1) {
        loggerPanic("Failed to open file: %s\n", path);
    }
 
    len = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);

    buf = malloc(sizeof(char) * len);
    size = 0;

    while ((rbytes = read(fd,buf,len)) != 0) {
        size += rbytes;
    }

    if (size != len) {
        loggerPanic("Failed to read whole file\n");
    }

    *_len = len;
    buf[len-1] = '\0';
    close(fd);
    return buf;
}

void lexPushFile(lexer *l, aoStr *filename) {
    /* We need to save what we are currently lexing and 
     * make the file we've just seen the file we want to lex */
    lexFile *f = malloc(sizeof(lexFile));
    ssize_t file_len = 0;
    char *src = lexReadfile(filename->data, &file_len);
    aoStr *src_code = malloc(sizeof(aoStr));
    src_code->data = src;
    src_code->len = file_len;
    src_code->capacity = 0;

    f->ptr = src_code->data;
    f->src = src_code;
    f->lineno = 1;
    f->filename = filename;
    strMapAdd(l->seen_files,filename->data,filename);
    if (l->cur_file) {
        listAppend(l->files,l->cur_file);
    }
    l->cur_file = f;
    l->ptr = f->ptr;
    l->lineno = f->lineno;
    l->start = f->ptr;
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
            if (*l->ptr == '\n') {
                l->lineno++;
            }
            if (*l->ptr == '*' && *(l->ptr + 1) == '/') {
                l->ptr += 2;
                break;
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
                    loggerWarning("line %d: Hex and seen e\n", l->lineno);
                    *err = 1;
                    return -1;
                }
                seen_e = 1;
            }
            break;
        case 'x':
        case 'X':
            if (*ishex) {
                loggerWarning("line %d: seen x\n", l->lineno);
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
                loggerWarning("line %d: Number errored with char: '%c'\n",l->lineno,*ptr);
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
        loggerWarning("line %d: LEX error is float and ishex\n",l->lineno);
        return -1;
    }

    /* Exponent hex does not exist */
    if (*ishex && seen_e) {
        *err = 1;
        loggerWarning("line %d: lexer seen_e and ishex\n", l->lineno);
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
    if (ch != '\0') {
        lexRewindChar(l);
    }
    return TK_IDENT;
}

long lexInStr(lexer *l, unsigned char *buf, long size, int *done,
        char terminator, int escape_quotes)
{
    long i = 0, j = 0, k = 0, ch = 0;
    *done = 1;

    escape_quotes = 0;
    if (escape_quotes) {
        ch = lexNextChar(l);
    }

    while (i < size - 1) {
        ch = lexNextChar(l);
        if (ch == '\n') {
            l->lineno++;
        }
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
            case 'v':
                buf[i++] = '\\';
                buf[i++] = 'v';
                break;
            case 'f':
                buf[i++] = '\\';
                buf[i++] = 'f';
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
    unsigned long char_const = 0, idx;
    long hex_num = 0;
    long len;
    char ch;

    for (len = 0; len < LEX_CHAR_CONST_LEN; ++len) {
        ch = lexNextChar(l);
        if (!ch || ch == '\'') {
            break;
        }
        idx = len * 8;
        if (ch == '\\') {
            ch = lexNextChar(l);
            switch (ch) {
                case '0':  char_const |= (unsigned long)'\0' << ((unsigned long)idx); break;
                case '\'': char_const |= (unsigned long)'\'' << ((unsigned long)idx); break;
                case '`':  char_const |= (unsigned long)'`'  << ((unsigned long)idx); break;
                case '\"': char_const |= (unsigned long)'\"' << ((unsigned long)idx); break;
                case 'd':  char_const |= (unsigned long)'$'  << ((unsigned long)idx); break;
                case 'n':  {
                    char_const |= (unsigned long)'\n' << ((unsigned long)idx);
                    l->lineno++;
                    break;
                }
                case 'r':  char_const |= (unsigned long)'\r' << ((unsigned long)idx); break;
                case 't':  char_const |= (unsigned long)'\t' << ((unsigned long)idx); break;
                case 'v':  char_const |= (unsigned long)'\v' << ((unsigned long)idx); break;
                case 'f':  char_const |= (unsigned long)'\f' << ((unsigned long)idx); break;
                case 'x':
                case 'X':
                    for (int i = 0; i < 2; ++i) {
                        ch = toupper(lexNextChar(l));
                        if (isHex(ch)) {
                            if (ch <= '9') {
                                hex_num |= (unsigned long)(hex_num<<4)+ch-'0';
                            } else {
                                hex_num |= (unsigned long)(hex_num<<4)+ch-'A'+10;
                            }
                        } else {
                            break;
                        }
                    }
                    char_const |= (unsigned long)hex_num << (unsigned long)(idx);
                    break;
                default:
                    char_const |= (unsigned long)'\\' << (unsigned long)(idx);
                    break;
            }
        } else {
            char_const |= (unsigned long)(((unsigned long)ch) << ((unsigned long)(idx)));
        }
    }

    if (ch != '\'' && lexPeek(l) != '\'') {
        loggerPanic("line %d: Char const limited to 8 characters!\n",l->lineno);
    }

    /* Consume next character if it is the end of the char const */
    if (lexPeek(l) == '\'') {
        lexNextChar(l);
    } 
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

LexerTypes *lexPreProcDirective(lexer *l) {
    lexeme le;
    if (!lex(l,&le)) return 0;
    char buffer[32];
    ssize_t len = snprintf(buffer,sizeof(buffer),"#%.*s",le.len,le.start);
    LexerTypes *type = strMapGetLen(l->symbol_table,buffer,len);
    if (!type) {
        loggerPanic("line %d: invalid preprocessor directive '%s'\n", le.line, buffer);
    }
    return type;
}

int lex(lexer *l, lexeme *le) {
    char ch, *start;
    int tk_type;
    LexerTypes *type;

    while (1) {
        ch = lexNextChar(l);
        start = l->start;

        switch (ch) {
        case '\r':
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
                loggerPanic("line %d: Lex error while lexing lexIdentifier\n",
                        l->lineno);
                goto error;
            }

            le->start = start;
            le->len = l->cur_strlen;
            le->tk_type = tk_type;
            le->line = l->lineno;

            if ((type = strMapGetLen(l->symbol_table,le->start,le->len)) != NULL) {
                le->tk_type = TK_KEYWORD;
                le->i64 = type->kind;
                type = NULL;
            }
            return 1;

        case '0' ... '9':
            if ((tk_type = lexNumeric(l,0)) == -1) {
                loggerPanic("line %d: Lex error while lexing lexNumeric\n",
                        l->lineno);
                goto error;
            }
            le->len = l->ptr - start;
            le->tk_type = tk_type;
            le->line = l->lineno;
            le->start = start;
            le->len = 0;
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
            if (isNum(lexPeek(l))) {
                if ((tk_type = lexNumeric(l,0)) == -1) {
                    loggerPanic("line %d: Lex error while lexing lexNumeric\n",
                            l->lineno);
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
            } else if (lexPeekMatch(l,'.')) {
                lexNextChar(l);
                if (lexPeekMatch(l,'.')) {
                    lexNextChar(l);
                    lexemeAssignOp(le,start,3,TK_ELLIPSIS,l->lineno);
                    return 1;
                }
                loggerPanic("line %d: .. is an invalid token sequence\n",
                        l->lineno);
            }
            
            lexemeAssignOp(le,start,1,ch,l->lineno);
            return 1;

        case '\\':
            lexemeAssignOp(le,start,1,'\\',l->lineno);
            return 1;

        case '#': {
            type = lexPreProcDirective(l);
            le->tk_type = TK_KEYWORD;
            le->i64 = type->kind;
            return 1;
        }

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
    loggerPanic("line %d: Lexer error\n", l->lineno);
    return 0;
}

int lexHasFile(lexer *l, aoStr *file) {
    return strMapGet(l->seen_files,file->data) != NULL;
}

void lexInclude(lexer *l) {
    aoStr *ident, *include_path;
    lexeme next;

    lex(l, &next);
    if (tokenPunctIs(&next, '<')) {
        lex(l, &next);
        ident = aoStrNew();
        do {
            aoStrCatPrintf(ident, "%.*s", next.len,
                    next.start);
            lex(l, &next);
        } while (next.i64 != '>');
        include_path = aoStrNew();
        aoStrCatPrintf(include_path, "%s/%s",
                l->builtin_root, ident->data);
        aoStrRelease(ident);
    } else if (next.tk_type == TK_STR) {
        include_path = aoStrDupRaw(next.start, next.len);
    } else {
        loggerPanic(
                "line %d: Syntax is: #include \"<value>\" got: %s\n",
                next.line,lexemeToString(&next));
    }

    if (!lexHasFile(l,include_path)) {
        lexPushFile(l,include_path);
    } else {
        aoStrRelease(include_path);
    }
}

lexeme *lexDefine(StrMap *macro_defs, lexer *l) {
    int tk_type,iters;
 //   List *macro_tokens;
    lexeme next,*start,*end,*expanded,*macro;
    aoStr *ident;
    PtrVec *tokens = ptrVecNew();


    tk_type = -1;
    /* <ident> <value> */
    lex(l, &next);
    if (next.tk_type != TK_IDENT) {
        loggerPanic("line %d: Syntax is: #define <TK_IDENT> <value>\n",next.line);
    }

    ident = aoStrDupRaw(next.start, next.len);
    /* A define must be on one line a \n determines the end of a define */
    l->flags |= CCF_ACCEPT_NEWLINES;
    iters = 0;
    do {
        iters++;
        if (!lex(l,&next)) break;

        if (next.tk_type == TK_IDENT) {
            if ((macro = strMapGetLen(macro_defs,next.start,next.len)) != NULL) {
                ptrVecPush(tokens, lexemeCopy(macro));
                tk_type = macro->tk_type;
                continue;
            }
        }

        if (tk_type == -1) {
            switch (next.tk_type) {
                case TK_F64:
                case TK_I64:
                case TK_STR: 
                case TK_CHAR_CONST:
                    tk_type = next.tk_type;
                    break;
            }
        }
        if (!tokenPunctIs(&next,'\n') && !tokenPunctIs(&next,'\0')) {
            ptrVecPush(tokens, lexemeCopy(&next));
        }
    } while (!tokenPunctIs(&next,'\n') && !tokenPunctIs(&next,'\0'));
    /* Turn off the flag */
    l->flags &= ~CCF_ACCEPT_NEWLINES;

    start = tokens->entries[0]; 
    end = tokens->entries[tokens->size - 1];

    if (start == end && iters == 1) {
        strMapAdd(macro_defs,ident->data,lexemeSentinal());
        ptrVecRelease(tokens);
        return NULL;
    }

    if (tk_type == -1) {
        loggerPanic("line %d: Error while parsing #define %s; #define either be a numerical expression or a string\n",
                next.line,ident->data);
    }

    if (start == end) {
        expanded = lexemeCopy(start);
        strMapAdd(macro_defs,ident->data,expanded);
    } else {
        /* XXX: this is a hack as sometimes the number of tokens in a macro 
         * will exceed what is allowed in our ring buffer. Conveniently PtrVec 
         * has some fields we can use. */
        cctrlInitMacroProcessor(macro_proccessor);
        macro_proccessor->token_buffer->entries = (lexeme **)tokens->entries;
        macro_proccessor->token_buffer->size = tokens->size;

        Ast *ast = parseExpr(macro_proccessor,16);
        expanded = lexemeNew(start->start,end->len-start->len);
        expanded->tk_type = tk_type;
        if (tk_type == TK_STR) {
            if (!ast && start->tk_type == TK_STR) {
                expanded->start = strndup(start->start,start->len);
                expanded->len = start->len;
            } else if (ast && ast->kind != TK_STR && ast->kind != AST_STRING) {
                loggerPanic("line %d: #define %s expected string but got: %s\n",
                        next.line,ident->data,astKindToString(ast->kind));
            } else if (ast && ast->kind == AST_STRING) {
                /* Copy as we will free the AST which will free the string*/
                expanded->start = strndup(ast->sval->data,ast->sval->len);
                expanded->len = ast->sval->len;
            } else {
                loggerPanic("line: %d failed to parse #define %s\n",
                        next.line, ident->data);
            }
        } else if (tk_type == TK_F64) {
            expanded->f64 = (long double)evalFloatExpr(ast);
            expanded->line = start->line;
        } else if (tk_type == TK_I64 || tk_type == TK_CHAR_CONST) {
            if (ast) {
                expanded->i64 = evalIntConstExpr(ast);
            } else {
                printf("Failed to expand: %s\n",ident->data);
                expanded->i64 = -1;
            }
            expanded->line = start->line;
        }
        strMapAdd(macro_defs,ident->data,expanded);
        astRelease(ast);
    }
    for (ssize_t i = 0; i < tokens->size; ++i) {
        lexemeFree(tokens->entries[i]);
    }
    ptrVecRelease(tokens);
    return expanded;
}

void lexUndef(StrMap *macro_defs, lexer *l) {
    lexeme next;
    char tmp[256];
    int tmp_len = 0;

    lex(l, &next);
    if (next.tk_type != TK_IDENT) {
        loggerPanic("line %d: Syntax is: #undef <TK_IDENT>\n",next.line);
    }
    tmp_len = snprintf(tmp,sizeof(tmp),"%.*s",
            next.len,next.start);
    tmp[tmp_len] = '\0';
    strMapRemove(macro_defs,tmp);
}

void lexExpandAndCollect(lexer *l, StrMap *macro_defs, List *tokens, int should_collect) {
    lexeme next,*macro;
    int endif_count = 1;

    do {
        if (!lex(l,&next)) {
            break;
        }
        if (tokenPunctIs(&next,'#')) {
            lex(l,&next);
            if (next.tk_type == TK_KEYWORD) {
                switch (next.i64) {
                    case KW_PP_INCLUDE: {
                        if (should_collect) {
                            lexInclude(l);
                        }
                        break;
                    }
                    case KW_PP_DEFINE: {
                        if (should_collect) {
                            lexDefine(macro_defs,l);
                        }
                        break;
                    }
                    case KW_PP_UNDEF: {
                        if (should_collect) {
                            lexUndef(macro_defs, l);
                        }
                        break;
                    }
                    case KW_PP_ELIF_DEF: {
                        lex(l,&next);
                        should_collect = 0;
                        if ((macro = strMapGetLen(macro_defs,next.start,next.len)) != NULL) {
                            should_collect = 1;
                        }
                        break;
                    }
                    case KW_PP_ELIF: {
                        should_collect = lexPreProcIf(macro_defs,l);
                        break;
                    }
                    case KW_PP_IF: {
                        should_collect = lexPreProcIf(macro_defs,l);
                        break;
                    }
                    case KW_PP_IF_DEF: {
                        lex(l,&next);
                        should_collect = 0;
                        if ((macro = strMapGetLen(macro_defs,next.start,next.len)) != NULL) {
                            should_collect = 1;
                        }
                        endif_count++;
                        break;
                    }
                    case KW_PP_IF_NDEF: {
                        lex(l,&next);
                        should_collect = 0;
                        if ((macro = strMapGetLen(macro_defs,next.start,next.len)) == NULL) {
                            should_collect = 1;
                        }
                        endif_count++;
                        break;
                    }
                    case KW_PP_ENDIF:
                        endif_count--;
                        if (endif_count == 0) {
                            goto done;
                        }
                        break;

                    case KW_PP_ELSE: {
                        if (should_collect) should_collect = 0;
                        else should_collect = 1;
                        break;
                    }
                    default:
                       loggerPanic("line %d: Invalid #<keyword> '%.*s'\n",
                               next.line,next.len,next.start);

                }
            }
        } else {
            if (should_collect) {
                listAppend(tokens,lexemeCopy(&next));
            }
        }
    } while (1);
done:
    return;
}

int lexPreProcIf(StrMap *macro_defs, lexer *l) {
    int tk_type,iters,should_collect;
    PtrVec *macro_tokens;
    lexeme next,*start,*end,*expanded,*macro;

    tk_type = -1;
    should_collect = 0;
    macro_tokens = ptrVecNew();

    /* An if must be on one line a \n determines the end of a define */
    l->flags |= CCF_ACCEPT_NEWLINES;
    iters = 0;

    if (!lex(l,&next)) {
        loggerPanic("line %d: Run out of tokens\n", l->lineno);
    }

    while (!tokenPunctIs(&next,'\n') && !tokenPunctIs(&next,'\0')){ 
        iters++;

        if (tokenPunctIs(&next,'\\')) {
            if (!lex(l,&next)) break;
            if (!tokenPunctIs(&next,'\n')) {
                loggerPanic("line %d: Invalid use of '\\' should be \\ \\n got %s\n",
                        next.line, lexemeToString(&next));
            }
            if (!lex(l,&next)) break;
        } 

        if (next.tk_type == TK_IDENT) {
            if ((macro = strMapGetLen(macro_defs,next.start,next.len)) != NULL) {
                ptrVecPush(macro_tokens,lexemeCopy(macro));
                tk_type = macro->tk_type;
            }
            if (!lex(l,&next)) break;
        }

        if (tk_type == -1) {
            switch (next.tk_type) {
                case TK_F64:
                case TK_I64:
                case TK_CHAR_CONST:
                    tk_type = next.tk_type;
                    break;
            }
        }
        if (!tokenPunctIs(&next,'\n') && !tokenPunctIs(&next,'\0')) {
            ptrVecPush(macro_tokens,lexemeCopy(&next));
        }
        if (!lex(l,&next)) break;
    }
    /* Turn off the flag */
    l->flags &= ~CCF_ACCEPT_NEWLINES;

    start = macro_tokens->entries[0];
    end = macro_tokens->entries[macro_tokens->size-1];

    if (start == end && iters == 1) {
        loggerPanic("line %d: a #if must evaluate some expression\n",next.line);
        return 0;
    }
    cctrlInitMacroProcessor(macro_proccessor);
    macro_proccessor->token_buffer->entries = (lexeme **)macro_tokens->entries;
    macro_proccessor->token_buffer->size = macro_tokens->size;

    Ast *ast = parseExpr(macro_proccessor,16);
    expanded = lexemeNew(start->start,end->len-start->len);
    expanded->tk_type = tk_type;

    if (tk_type == TK_STR) {
        should_collect = 1;
    } else if (tk_type == TK_F64) {
        expanded->f64 = (long double)evalFloatExpr(ast);
        if (expanded->f64 != 0) {
            should_collect = 1;
        }
        expanded->line = start->line;
    } else {
        expanded->i64 = evalIntConstExpr(ast);
        if (expanded->i64 != 0) {
            should_collect = 1;
        }
        expanded->line = start->line;
    }
    astRelease(ast);
    for (ssize_t i = 0; i < macro_tokens->size; ++i) {
        lexemeFree(macro_tokens->entries[i]);
    }
    ptrVecRelease(macro_tokens);

    return should_collect;
}

/* Decide if we should collect tokens or not */
int lexPreProcBoolean(lexer *l, StrMap *macro_defs, lexeme *le, int can_collect) {
    lexeme next,*macro;

    switch (le->i64) {
        case KW_PP_IF: {
            int ok = lexPreProcIf(macro_defs,l);
            if (ok) {
                l->collecting = 1;
                l->skip_else = 1;
            } else {
                l->collecting = 0;
                l->skip_else = 0;
            }
            return ok;
        }

        case KW_PP_IF_DEF: {
            lex(l,&next);
            if ((macro = strMapGetLen(macro_defs,next.start,next.len)) != NULL) {
                l->collecting = 1;
                l->skip_else = 1;
                return 1;
            } 

            l->collecting = 0;
            l->skip_else = 0;
            return 0;
        }

        case KW_PP_IF_NDEF: {
            lex(l,&next);
            if ((macro = strMapGetLen(macro_defs,next.start,next.len)) == NULL) {
                l->collecting = 0;
                l->skip_else = 1;
                return 1;
            }
            l->collecting = 1;
            l->skip_else = 0;
            return 0;
        }

        case KW_PP_ELIF: {
            if (l->skip_else) return 0;
           // if (!can_collect) {
            int ok = lexPreProcIf(macro_defs,l);
            if (ok) {
             //   l->collecting = 1;
                l->skip_else = 1;
            } else {
               // l->collecting = 0;
                l->skip_else = 0;
            }
            return ok;
           // }
           // return 0;
        }

        case KW_PP_ELIF_DEF: {
            if (l->skip_else) return 0;
           // if (!can_collect) {
                lex(l,&next);
                if ((macro = strMapGetLen(macro_defs,next.start,next.len)) != NULL) {
                    l->collecting = 1;
                    l->skip_else = 1;
                    return 1;
                }
                l->collecting = 0;
                l->skip_else = 0;
           // }
            return 0;
        }


        case KW_PP_ELSE: {
            if (l->skip_else) return 0;
            l->collecting = 1;
            //if (!can_collect) {
            //    return 1;
           // }
            return 1;
        }

        case KW_PP_ENDIF:
            l->skip_else = 0;
            l->collecting = 1;
            return 1;

        default:
            return 0;
    }
}

lexeme *lexToken(StrMap *macro_defs, lexer *l) {
    lexeme le,next,*copy,*macro;

    macro_proccessor->macro_defs = macro_defs;

    while (1) {
        if (!lex(l,&le)) {
            return NULL;
        }

        if (l->flags & (CCF_ASM_BLOCK) && tokenPunctIs(&le, '}')) {
            copy = lexemeCopy(&le);
            /* turn off assembly lexing */
            l->flags &= ~(CCF_MULTI_COLON|CCF_ACCEPT_NEWLINES|CCF_ASM_BLOCK);
            return copy;
        }

        if (le.tk_type == TK_KEYWORD) {
            switch (le.i64) {
                case KW_ASM:
                    l->flags |= (CCF_MULTI_COLON|CCF_ACCEPT_NEWLINES|CCF_ASM_BLOCK);
                    copy = lexemeCopy(&le);
                    return copy;

                case KW_PP_INCLUDE: {
                    lexInclude(l);
                    continue;
                }
                case KW_PP_DEFINE: {
                    copy = lexDefine(macro_defs,l);
                    continue;
                }
                case KW_PP_UNDEF: {
                    lexUndef(macro_defs, l);
                    continue;
                }

                case KW_PP_ELIF_DEF:
                case KW_PP_ELIF:
                case KW_PP_ELSE: {
                    if (l->skip_else) {
                        while (lex(l,&le)) {
                            if (le.tk_type == TK_KEYWORD && le.i64 == KW_PP_ENDIF) {
                                break;
                            }
                        }
                        l->skip_else = 0;
                    } else {
                        int should_collect = 0;
                        int line = le.line;
                        while ((should_collect = lexPreProcBoolean(l,macro_defs,&le, should_collect)) != 1) {
                            int ok = lex(l,&le);
                            if (!ok) {
                                loggerPanic("line %d: Unterminated #if\n", line);
                            }
                        }
                    }
                    continue;
                }

                case KW_PP_IF_DEF:
                case KW_PP_IF_NDEF:
                case KW_PP_IF: {
                    int should_collect = 0;
                    int line = le.line;
                    while ((should_collect = lexPreProcBoolean(l,macro_defs,&le,should_collect)) != 1) {
                        int ok = lex(l,&le);
                        if (!ok) {
                            loggerPanic("line %d: Unterminated #if\n", line);
                        }
                    }
                    continue;
                }
                case KW_PP_ENDIF:
                    l->skip_else = 0;
                    continue;

                case KW_PP_ERROR:
                    lex(l,&next);
                    loggerPanic("line %d: %.*s",next.line,next.len,next.start);
                    break;

                default:
                    copy = lexemeCopy(&le);
                    return copy;
            }
        }
        
        if (le.tk_type == TK_IDENT) {
            if ((macro = strMapGetLen(macro_defs,le.start,le.len)) != NULL) {
                copy = lexemeCopy(macro);
                return copy;
            }
            copy = lexemeCopy(&le);
            return copy;
        }

        copy = lexemeCopy(&le);
        return copy;
    }
    return NULL;
}

void lexemeFree(void *_le) {
    if (_le) {
        lexeme *le = (lexeme *)_le;
        free(le);
    }
}

static void lexReleaseLexFile(lexFile *lex_file) {
    aoStrRelease(lex_file->filename);
    free(lex_file->ptr);
    free(lex_file);
}

void lexReleaseAllFiles(lexer *l) {
    listRelease(l->all_source,
            ((void (*))&lexReleaseLexFile));
}

const char *lexerReportLine(lexer *l, ssize_t lineno) {
    static char buffer[4096];
    char *tmp_ptr = buffer;

    char *ptr = l->cur_file->src->data;
    ssize_t line = 1;

    while (line != lineno && *ptr) {
        if (*ptr == '\n') {
            line++;
        }
        ptr++;
    }
    if (!*ptr) {
        memcpy((void*)buffer,str_lit("invalid line number"));
    }
    while (*ptr && *ptr != '\n') {
        *tmp_ptr++ = *ptr++;
    }
    *tmp_ptr = '\0';
    return buffer;
}
