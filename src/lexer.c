#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>

#include "aostr.h"
#include "ast.h"
#include "arena.h"
#include "cctrl.h"
#include "containers.h"
#include "lexer.h"
#include "list.h"
#include "prslib.h"
#include "prsutil.h"
#include "util.h"

typedef struct {
    char *name;
    int kind;
} LexerType;

void vecLexemeToString(AoStr *buf, void *tok) {
    char *lexeme_str = lexemeToString(tok);
    aoStrCatPrintf(buf, "%s\n", lexeme_str);
}

int vecLexemeRelease(void *_tok) {
    (void)_tok;
    return 1;
}

VecType vec_lexeme_type = {
    .stringify = vecLexemeToString,
    .match     = NULL,
    .release   = vecLexemeRelease,
    .type_str  = "Lexeme *",
};

Vec *lexemeVecNew(void) {
    return vecNew(&vec_lexeme_type);
}

AoStr *mapLexerTypeToString(void *ltype) {
    LexerType *t = (LexerType *)ltype;
    return aoStrPrintf("\"%s\" %d", t->name, t->kind);
}

MapType map_cstring_builtin_lexer_type = {
    .match           = mapCStringEq,
    .hash            = mapCStringHash,
    .get_key_len     = mapCStringLen,
    .key_to_string   = mapCStringToString,
    .key_release     = NULL,
    .value_to_string = mapLexerTypeToString,
    .value_release   = NULL,
    .key_type        = "char *",
    .value_type      = "LexerType *",
};

/* prototypes */
int lexPreProcIf(Map *macro_defs, Lexer *l);

static Arena lexeme_arena;
static int lexeme_arena_init = 0;

void lexemeMemoryInit(void) {
    if (!lexeme_arena_init) {
        lexeme_arena_init = 1;
        arenaInit(&lexeme_arena, sizeof(Lexeme) * 1000);
    }
}

void lexemeMemoryRelease(void) {
    if (lexeme_arena_init) {
        lexeme_arena_init = 0;
        arenaClear(&lexeme_arena);
    }
}

void lexemeMemoryStats(void) {
    printf("Lexeme Arena:\n");
    arenaPrintStats(&lexeme_arena);
}

Lexeme *lexerAllocLexeme(void) {
    return (Lexeme *)arenaAlloc(&lexeme_arena, sizeof(Lexeme));
}

char *lexerAllocateBuffer(u32 size) {
    return (char *)arenaAlloc(&lexeme_arena, size);
}

char *lexerReAllocBuffer(char *ptr, u32 old_size, u32 new_size) {
    (void)ptr;
    char *buffer = lexerAllocateBuffer(new_size);
    memcpy(buffer,ptr,old_size);
    return buffer;
}

/* Name, kind, size, issigned */
static LexerType lexer_types[] = {
    /* Holyc Types, this language is semi interoperable */
    {"U0",   KW_U0},
    {"Bool", KW_BOOL},
    {"I8",   KW_I8},
    {"U8",   KW_U8},
    {"I16",  KW_I16},
    {"U16",  KW_U16},
    {"I32",  KW_I32},
    {"U32",  KW_U32},
    {"I64",  KW_I64},
    {"U64",  KW_U64},
    {"F64",  KW_F64},

    {"auto", KW_AUTO},

    {"_extern", KW_ASM_EXTERN},
    {"extern",  KW_EXTERN},
    {"asm",     KW_ASM},

    {"switch",   KW_SWITCH},
    {"case",     KW_CASE},
    {"break",    KW_BREAK},
    {"continue", KW_CONTINUE},
    {"while",    KW_WHILE},
    {"do",       KW_DO},
    {"for",      KW_FOR},
    {"goto",     KW_GOTO},
    {"default",  KW_DEFAULT},
    {"return",   KW_RETURN},

    {"if",      KW_IF},
    {"else",    KW_ELSE},
    {"define",  KW_DEFINE},
    {"ifndef",  KW_IF_NDEF},
    {"ifdef",   KW_IF_DEF},
    {"elifdef", KW_ELIF_DEF},
    {"endif",   KW_ENDIF},
    {"elif",    KW_ELIF},
    {"defined", KW_DEFINED},
    {"undef",   KW_UNDEF},

    {"#if",      KW_PP_IF},
    {"#else",    KW_PP_ELSE},
    {"#define",  KW_PP_DEFINE},
    {"#ifndef",  KW_PP_IF_NDEF},
    {"#ifdef",   KW_PP_IF_DEF},
    {"#elifdef", KW_PP_ELIF_DEF},
    {"#endif",   KW_PP_ENDIF},
    {"#elif",    KW_PP_ELIF},
    {"#defined", KW_PP_DEFINED},
    {"#undef",   KW_PP_UNDEF},
    {"#error",   KW_PP_ERROR},
    {"#include", KW_PP_INCLUDE},

    {"cast",     KW_CAST},
    {"sizeof",   KW_SIZEOF},
    {"inline",   KW_INLINE},
    {"atomic",   KW_ATOMIC},
    {"volatile", KW_VOLATILE},

    {"public",  KW_PUBLIC},
    {"private", KW_PRIVATE},
    {"class",   KW_CLASS},
    {"union",   KW_UNION},

    {"static",  KW_STATIC},
};


#define isNum(ch) ((ch) >= '0' && (ch) <= '9')
#define isHex(ch) \
    (isNum(ch) || (ch >= 'a' && ch <= 'f') || (ch >= 'A' && ch <= 'F'))
#define isNumTerminator(ch) (!isNum(ch) && !isHex(ch) && ch != '.' && ch != 'x' \
        && ch != 'X' && ch != '\\')

Lexeme *lexemeNew(char *start, int len) {
    Lexeme *le = lexerAllocLexeme();
    le->start = start;
    le->len = len;
    le->line = -1;
    le->tk_type = -1;
    return le;
}

Lexeme *lexemeSentinal(void) {
    Lexeme *le = lexerAllocLexeme();
    le->start = "(-sentinal-)";
    le->len = 12;
    le->line = 1;
    le->tk_type = -1;
    return le;
}

void lexemeAssignOp(Lexeme *le, char *start, int len, s64 op, int line) {
    le->tk_type = TK_PUNCT;
    le->line = line;
    le->start = start;
    le->len = len;
    le->i64 = op; /* instead of 'char'*/
}

Lexeme *lexemeTokNew(char *start, int len, int line, s64 ch) {
    Lexeme *copy = lexerAllocLexeme();
    copy->tk_type = TK_PUNCT;
    copy->start = start;
    copy->len = len;
    copy->line = line;
    copy->i64 = ch;
    return copy;
}

Lexeme *lexemeCopy(Lexeme *le) {
    Lexeme *copy = lexerAllocLexeme();
    memcpy(copy,le,sizeof(Lexeme));
    return copy;
}

Lexeme *lexemeNewOp(char *start, int len, s64 op, int line) {
    Lexeme *le = lexemeNew(start,len);
    lexemeAssignOp(le,start,len,op,line);
    return le;
}

static Cctrl *macro_proccessor = NULL;

void lexInit(Lexer *l, char *source, int flags) {
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
    l->symbol_table = mapNew(32, &map_cstring_builtin_lexer_type);
    l->seen_files = setNew(32, &set_cstring_type);
    l->collecting = 1;
    l->skip_else = 1;
    if (macro_proccessor == NULL) {
        macro_proccessor = ccMacroProcessor(NULL);
    }

    if (!lexeme_arena_init) {
        lexemeMemoryInit();
    }

    /* XXX: create one symbol table for the whole application ;
     * hoist to 'compile.c'*/
    for (int i = 0; i < (int)static_size(lexer_types); ++i) {
        LexerType *bilt = &lexer_types[i]; 
        mapAddLen(l->symbol_table, bilt->name, strlen(bilt->name), bilt);
    }
}

void lexSetBuiltinRoot(Lexer *l, char *root) {
    l->builtin_root = root;
}

/* Is the lexeme both of type TK_PUNCT and does 'ch' match */
int tokenPunctIs(Lexeme *tok, s64 ch) {
    return tok && (tok->tk_type == TK_PUNCT || tok->tk_type == TK_EOF) && tok->i64 == ch;
}

/* Is the token an identifier and does the string match */
int tokenIdentIs(Lexeme *tok, char *ident, int len) {
    return tok && tok->tk_type == TK_IDENT 
               && tok->len == len 
               && !memcmp(tok->start,ident,len);
}

char *lexemeTypeToString(int tk_type) {
    switch (tk_type) {
    case TK_IDENT:      return "identifier";
    case TK_PUNCT:      return "character";
    case TK_I64:        return "integer";
    case TK_F64:        return "float";
    case TK_EOF:        return "end of file";
    case TK_STR:        return "string";
    case TK_KEYWORD:    return "keyword";
    case TK_CHAR_CONST: return "character constant";
    }
    return "UNKNOWN";
}

/* This is for something that will play nicely with HTML or GraphViz*/
char *lexemePunctToEncodedString(s64 op) {
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
char *lexemePunctToString(s64 op) {
    static char buf[4];
    switch (op) {
    case '\\':               return "\\";
    case '\n':               return "\\n";
    case '\t':               return "\\t";
    case '\r':               return "\\r";
    case '\"':               return "\\\"";
    case '\'':               return "\\\'";
    case '\0':               return "\\0'";
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

char *lexemePunctToStringWithFlags(s64 op, u64 flags) {
    if (flags & LEXEME_ENCODE_PUNCT) return lexemePunctToEncodedString(op);
    else                             return lexemePunctToString(op);
}

AoStr *lexemeToAoStr(Lexeme *tok) {
    if (!tok) {
        return aoStrPrintf("(null)");
    }
    AoStr *str = aoStrNew();
    char *tmp;
    switch (tok->tk_type) {
        case TK_COMMENT: {
            aoStrCatPrintf(str,"TK_COMMENT    %.*s",tok->len,tok->start);
            return str;
        }
        case TK_IDENT:
            aoStrCatPrintf(str,"TK_IDENT      %.*s",tok->len,tok->start);
            return str;
        case TK_CHAR_CONST:
            aoStrCatPrintf(str,"TK_CHAR_CONST %x",tok->i64);
            return str;
        case TK_PUNCT: {
            tmp = lexemePunctToString(tok->i64);
            aoStrCatPrintf(str,"TK_PUNCT      %s", tmp);
            return str;
        }
        case TK_I64:
            aoStrCatPrintf(str,"TK_I64        %lld",tok->i64);
            return str;
        case TK_F64:
            aoStrCatPrintf(str,"TK_F64        %g",tok->f64);
            return str;
        case TK_STR:
            aoStrCatPrintf(str,"TK_STR        \"%.*s\"",tok->len,tok->start);
            return str;
        case TK_EOF:
            aoStrCatPrintf(str,"TK_EOF");
            return str;
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
            return str;
        }
    }
    loggerPanic("line %d: Unexpected type %s |%.*s|\n",
            tok->line,lexemeTypeToString(tok->tk_type),tok->len,tok->start);
}

char *lexemeToString(Lexeme *tok) {
    return aoStrMove(lexemeToAoStr(tok));
}

/* Print one lexeme */
void lexemePrint(Lexeme *le) {
    if (le) {
        char *str = lexemeToString(le);
        printf("%d: %s\n", le->line, str);
      //  free(str);
    }
}

static char lexNextChar(Lexer *l) {
    l->start = l->ptr;
    LexFile *lex_file;
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

static void lexRewindChar(Lexer *l) {
    l->ptr--;
}

/* Doese the next character match the expected character */
static int lexPeekMatch(Lexer *l, char expected) {
    return *l->ptr == expected;
}

static char lexPeek(Lexer *l) {
    return *l->ptr;
}

/* Read an entire file to a mallocated buffer */
char *lexReadfile(char *path, s64 *_len) {
    int fd;
    if ((fd = open(path, O_RDONLY, 0644)) == -1) {
        loggerPanic("Failed to open file: %s\n", path);
    }
 
    int len = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);

    /* Add a `+1` for `\0` */
    char *buf = (char *)malloc((sizeof(char) * len)+1);
    int size = 0;
    int rbytes = 0;
    while ((rbytes = read(fd,buf,len)) != 0) {
        size += rbytes;
    }

    if (size != len) {
        loggerPanic("Failed to read whole file\n");
    }

    *_len = len;
    buf[len] = '\0';
    close(fd);
    return buf;
}

void lexPushFile(Lexer *l, AoStr *filename) {
    /* We need to save what we are currently lexing and 
     * make the file we've just seen the file we want to lex */
    LexFile *f = (LexFile *)malloc(sizeof(LexFile));
    s64 file_len = 0;
    char *src = lexReadfile(filename->data, &file_len);
    AoStr *src_code = aoStrNew();
    src_code->data = src;
    src_code->len = file_len;
    src_code->capacity = 0;

    f->ptr = src_code->data;
    f->src = src_code;
    f->lineno = 1;
    f->filename = filename;
    setAdd(l->seen_files,filename->data);
    if (l->cur_file) {
        listAppend(l->files,l->cur_file);
    }
    l->cur_file = f;
    l->ptr = f->ptr;
    l->lineno = f->lineno;
    l->start = f->ptr;
}

static void lexSkipCodeComment(Lexer *l) {
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
                //while (*l->ptr != '\n') {
                //    l->ptr++;
                //}
                l->ptr += 2;
                break;
            }
            l->ptr++;
        }
    }
}

static int countNumberLen(Lexer *l, char *ptr, int *isfloat, int *ishex,
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

int lexIdentifier(Lexer *l, char ch) {
    int i = 0;
    while (ch && (isalnum(ch) || ch == '_' ||  ch == '$')) {
        i++;
        ch = lexNextChar(l);
    }

    l->cur_strlen = i;
    if (ch != '\0') {
        lexRewindChar(l);
    }
    return TK_IDENT;
}

/* As this function escapes strings we pass in `_real_len` to be able 
 * to capture the length of the string minus escape sequences. 
 * The string is allocated from the lexers arean not the global allocator */
char *lexString(Lexer *l, char terminator, s64 *_real_len, int *_buffer_len) {
    u32 capacity = 64;
    int len = 0;
    s64 real_len = 0;

    char *buffer = lexerAllocateBuffer(64);
    char ch = '\0';
    int is_bytes = 0;

    while ((ch = lexNextChar(l)) != terminator) {
        real_len++;
        if (ch == '\n') {
            l->lineno++;
        }

        if ((unsigned int)(len + 3) >= capacity) {
            buffer = lexerReAllocBuffer(buffer, len, capacity * 2);
            capacity *= 2;
        }

        if (!ch || ch == terminator) {
            goto done;
        } else if (ch == '\\') {
            ch = lexNextChar(l);

            buffer[len++] = '\\';
            switch (ch) {
                case '\\': buffer[len++] = '\\'; break;
                case '\'': buffer[len++] = '\''; break;
                case '0':  buffer[len++] = '0' ; break;
                case '`':  buffer[len++] = '`' ; break;
                case '"':  buffer[len++] = '"' ; break;
                case 'n':  buffer[len++] = 'n' ; break;
                case 'r':  buffer[len++] = 'r' ; break;
                case 't':  buffer[len++] = 't' ; break;
                case 'b':  buffer[len++] = 'b' ; break;
                case 'f':  buffer[len++] = 'f' ; break;
                case 'v':  
                    buffer[len++] = 'x';
                    buffer[len++] = '0';
                    buffer[len++] = 'B';
                    break;

                case 'x':
                case 'X':
                    is_bytes = 1;
                    buffer[len++] = 'x';
                    buffer[len++] = toupper(lexNextChar(l));
                    buffer[len++] = toupper(lexNextChar(l));
                    break;
            default:
                loggerPanic("Line: %d - Invalid escape character: '%c'\n",
                        l->lineno, (char)ch);
            }
        } else {
            /* Because HC can have multi line strings, tabs or other escaped 
             * characters which are typeable we need to escape them. */
            switch (ch) {
                case '\n':
                    buffer[len++] = '\\';
                    buffer[len++] = 'n';
                    break;
                case '\t':
                    buffer[len++] = '\\';
                    buffer[len++] = 't';
                    break;
                case '\r':
                    buffer[len++] = '\\';
                    buffer[len++] = 'r';
                    break;
                case '\f':
                    buffer[len++] = '\\';
                    buffer[len++] = 'f';
                    break;
                case '\v':
                    buffer[len++] = '\\';
                    buffer[len++] = 'x';
                    buffer[len++] = '0';
                    buffer[len++] = 'B';
                    break;
                default:
                    buffer[len++] = ch;
                    break;
            }
        }
    }

done:
    /* XXX: Should raise exception if we run out of tokens as it is 
     * an unterminated string. */
    if (!is_bytes) {
        real_len++;
    }
    buffer[len] = '\0';
    *_real_len = real_len;
    *_buffer_len = len;
    return buffer;
}

/* Length of the char const is returned, it OR's in at max 8 characters. 
 * A s64 being 64 bits and 64/8 = 8. */
u64 lexCharConst(Lexer *l) {
    u64 char_const = 0, idx;
    s64 hex_num = 0;
    s64 len;
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

int lexNumeric(Lexer *l, int _isfloat) {
    int ishex, isfloat, err, numlen;
    char *endptr;

    isfloat = _isfloat;

    ishex = isfloat = err = 0;
    char *start = l->ptr - 1;
    numlen = countNumberLen(l, start, &isfloat, &ishex, &err);
    if (err) {
        return -1;
    }

    l->cur_strlen = numlen;
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

LexerType *lexPreProcDirective(Lexer *l) {
    Lexeme le;
    if (!lex(l,&le)) return 0;
    char buffer[32];
    s64 len = snprintf(buffer,sizeof(buffer),"#%.*s",le.len,le.start);
    LexerType *type = mapGetLen(l->symbol_table,buffer,len);
    if (!type) {
        loggerPanic("line %d: invalid preprocessor directive '%s'\n", le.line, buffer);
    }
    return type;
}

int lex(Lexer *l, Lexeme *le) {
    char ch, *start;
    int tk_type;
    LexerType *type;

    while (1) {
        ch = lexNextChar(l);
        start = l->start;

        switch (ch) {
            case '\r':
            case '\n':
                l->lineno++;
                if (l->flags & (CCF_ACCEPT_NEWLINES|CCF_ACCEPT_WHITESPACE)) {
                    lexemeAssignOp(le,start,1,ch,l->lineno);
                    return 1;
                }
                break;

            case ' ':
                if (l->flags & (CCF_ACCEPT_WHITESPACE)) {
                    lexemeAssignOp(le,start,1,ch,l->lineno);
                    return 1;
                }
                break;


            case '\0':
                return 0;

            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                if ((tk_type = lexNumeric(l,0)) == -1) {
                    loggerPanic("line %d: Lex error while lexing lexNumeric\n",
                            l->lineno);
                    goto error;
                }
                le->len = l->cur_strlen;
                le->tk_type = tk_type;
                le->line = l->lineno;
                le->start = start;
                if (tk_type == TK_F64) {
                    le->f64 = l->cur_f64;
                } else {
                    le->i64 = l->cur_i64;
                }
                le->ishex = l->ishex;
                l->ishex = 0;
                return 1;

            case '\"': {
                le->len = 0;
                le->i64 = 0;
                le->start = lexString(l,'"',&le->i64,&le->len);
                le->tk_type = TK_STR;
                le->line = l->lineno;
                l->cur_str = NULL;
                l->cur_strlen = 0;
                return 1;
            }

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
                    start = l->ptr-1;
                    lexSkipCodeComment(l);
                    s64 lineno = l->lineno;
                    if (l->flags & CCF_ACCEPT_COMMENTS) {
                        s64 len = l->ptr-start;
                        le->start = start;
                        le->len = len;
                        le->line = lineno;
                        le->tk_type = TK_COMMENT;
                        return 1;
                    }
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
                        lexemeAssignOp(le,start,3,TK_SHL_EQU,l->lineno);
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
                        lexemeAssignOp(le,start,3,TK_SHR_EQU,l->lineno);
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

            case ':': {
                if (l->flags & CCF_MULTI_COLON && lexPeekMatch(l,':')) {
                    lexNextChar(l);
                    lexemeAssignOp(le,start,2,TK_DBL_COLON,l->lineno);
                    return 1;
                }
                lexemeAssignOp(le,start,1,ch,l->lineno);
                return 1;
            }
            case '~':
            case '(':
            case ')':
            case ',':
            case ';':
            case '[':
            case ']':
            case '{':
            case '}':
                lexemeAssignOp(le,start,1,ch,l->lineno);
                return 1;
            
            default: {
                if (isalpha(ch) || ch == '_') {
                    if ((tk_type = lexIdentifier(l, ch)) == -1) {
                        loggerPanic("line %d: Lex error while lexing lexIdentifier\n",
                                l->lineno);
                        goto error;
                    }

                    le->start = start;
                    le->len = l->cur_strlen;
                    le->tk_type = tk_type;
                    le->line = l->lineno;

                    if ((type = mapGetLen(l->symbol_table,le->start,le->len)) != NULL) {
                        le->tk_type = TK_KEYWORD;
                        le->i64 = type->kind;
                        type = NULL;
                    }
                    return 1;
                }
                break;
            }
        }
    }
error:
    loggerPanic("line %d: Lexer error\n", l->lineno);
    return 0;
}

void lexInclude(Lexer *l) {
    AoStr *ident, *include_path;
    Lexeme next;

    lex(l, &next);
    if (tokenPunctIs(&next, '<')) {
        lex(l, &next);
        ident = aoStrNew();
        do {
            aoStrCatPrintf(ident, "%.*s", next.len, next.start);
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

    if (!setHas(l->seen_files,include_path->data)) {
        lexPushFile(l,include_path);
    } else {
        aoStrRelease(include_path);
    }
}

Lexeme *lexDefine(Map *macro_defs, Lexer *l) {
    int tk_type,iters;
    Lexeme next,*start,*end,*expanded,*macro;
    AoStr *ident;
    Vec *tokens = lexemeVecNew();

    tk_type = -1;
    /* <ident> <value> */
    lex(l, &next);
    if (next.tk_type != TK_IDENT) {
        loggerPanic("%s: line %d: Syntax is: #define <TK_IDENT> <value> got %s\n",
                l->cur_file->filename->data,
                next.line,
                lexemeToString(&next));
    }

    ident = aoStrDupRaw(next.start, next.len);
    /* A define must be on one line a \n determines the end of a define */
    l->flags |= CCF_ACCEPT_NEWLINES;
    iters = 0;
    do {
        iters++;
        if (!lex(l, &next)) break;

        if (next.tk_type == TK_IDENT) {
            if ((macro = mapGetLen(macro_defs,next.start,next.len)) != NULL) {
                vecPush(tokens, lexemeCopy(macro));
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
            vecPush(tokens, lexemeCopy(&next));
        }
    } while (!tokenPunctIs(&next,'\n') && !tokenPunctIs(&next,'\0'));
    /* Turn off the flag */
    l->flags &= ~CCF_ACCEPT_NEWLINES;

    start = tokens->entries[0]; 
    end = tokens->entries[tokens->size - 1];

    if (start == end && iters == 1) {
        mapAdd(macro_defs,ident->data,lexemeSentinal());
        vecRelease(tokens);
        return NULL;
    }

    if (tk_type == -1) {
        loggerPanic("line %d: Error while parsing #define %s; #define either be a numerical expression or a string\n",
                next.line,ident->data);
    }

    if (start == end) {
        expanded = lexemeCopy(start);
        mapAdd(macro_defs,ident->data,expanded);
    } else {
        /* XXX: this is a hack as sometimes the number of tokens in a macro 
         * will exceed what is allowed in our ring buffer. Conveniently Vec 
         * has some fields we can use. */
        cctrlInitMacroProcessor(macro_proccessor);
        macro_proccessor->token_buffer->entries = (Lexeme **)tokens->entries;
        macro_proccessor->token_buffer->size = tokens->size;
        macro_proccessor->token_buffer->capacity = roundUpToNextPowerOf2(tokens->size);

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
            expanded->f64 = (f64)evalFloatExpr(ast);
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
        mapAdd(macro_defs,ident->data,expanded);
    }
    vecRelease(tokens);
    return expanded;
}

void lexUndef(Map *macro_defs, Lexer *l) {
    Lexeme next;
    char tmp[256];
    int tmp_len = 0;

    lex(l, &next);
    if (next.tk_type != TK_IDENT) {
        loggerPanic("line %d: Syntax is: #undef <TK_IDENT>\n",next.line);
    }
    tmp_len = snprintf(tmp,sizeof(tmp),"%.*s",
            next.len,next.start);
    tmp[tmp_len] = '\0';
    mapRemove(macro_defs,tmp);
}

int lexPreProcIf(Map *macro_defs, Lexer *l) {
    int tk_type,iters,should_collect;
    Vec *macro_tokens;
    Lexeme next,*start,*end,*expanded,*macro;

    tk_type = -1;
    should_collect = 0;
    macro_tokens = lexemeVecNew();

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
            if ((macro = mapGetLen(macro_defs,next.start,next.len)) != NULL) {
                vecPush(macro_tokens,lexemeCopy(macro));
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
            vecPush(macro_tokens,lexemeCopy(&next));
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
    macro_proccessor->token_buffer->entries = (Lexeme **)macro_tokens->entries;
    macro_proccessor->token_buffer->size = macro_tokens->size;
    macro_proccessor->token_buffer->capacity = roundUpToNextPowerOf2(macro_tokens->size);

    Ast *ast = parseExpr(macro_proccessor,16);
    expanded = lexemeNew(start->start,end->len-start->len);
    expanded->tk_type = tk_type;

    if (tk_type == TK_STR) {
        should_collect = 1;
    } else if (tk_type == TK_F64) {
        expanded->f64 = (s64)evalFloatExpr(ast);
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
    vecRelease(macro_tokens);

    return should_collect;
}

/* Decide if we should collect tokens or not */
int lexPreProcBoolean(Lexer *l, Map *macro_defs, Lexeme *le) {
    Lexeme next,*macro;

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
            if ((macro = mapGetLen(macro_defs,next.start,next.len)) != NULL) {
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
            if ((macro = mapGetLen(macro_defs,next.start,next.len)) == NULL) {
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
            int ok = lexPreProcIf(macro_defs,l);
            if (ok) {
                l->skip_else = 1;
            } else {
                l->skip_else = 0;
            }
            return ok;
        }

        case KW_PP_ELIF_DEF: {
            if (l->skip_else) return 0;
            lex(l,&next);
            if ((macro = mapGetLen(macro_defs,next.start,next.len)) != NULL) {
                l->collecting = 1;
                l->skip_else = 1;
                return 1;
            }
            l->collecting = 0;
            l->skip_else = 0;
            return 0;
        }


        case KW_PP_ELSE: {
            if (l->skip_else) return 0;
            l->collecting = 1;
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

Lexeme *lexToken(Map *macro_defs, Lexer *l) {
    Lexeme le,next,*copy;

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
                        int line = le.line;
                        while ((lexPreProcBoolean(l,macro_defs,&le)) != 1) {
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
                    int line = le.line;
                    while ((lexPreProcBoolean(l,macro_defs,&le)) != 1) {
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
        Lexeme *le = (Lexeme *)_le;
        (void)le;
    }
}

static void lexReleaseLexFile(LexFile *lex_file) {
    free(lex_file);
}

void lexReleaseAllFiles(Lexer *l) {
    listRelease(l->all_source,
            ((void (*)(void *))&lexReleaseLexFile));
}

char *lexerReportLine(Lexer *l, s64 lineno) {
    AoStr *buf = aoStrAlloc(256);

    char *ptr = l->cur_file->src->data;
    s64 size = l->cur_file->src->len;
    s64 line = 1;
    s64 i = 0;
    
    for (; i < size-1; ++i) {
        if (ptr[i] == '\n') {
            line++;
        }
        if (ptr[i] == '\0') break;
        if (line == lineno) break;
    }

    i++;
    if (ptr[i] == '\0') {
        return aoStrMove(buf);
    }

    while (isspace(ptr[i])) {
        i++;
    }

    while (ptr[i] && ptr[i] != '\n') {
        aoStrPutChar(buf, ptr[i++]);
    }
    aoStrPutChar(buf, '\0');
    return aoStrMove(buf);
}

int lexemeEq(Lexeme *l1, Lexeme *l2) {
    if (l1->tk_type  != l2->tk_type) {
        return 0;
    }

    switch (l1->tk_type) {
        case TK_STR:
        case TK_IDENT: return l1->len == l2->len && !memcmp(l1->start,l2->start,l1->len);

        case TK_KEYWORD:
        case TK_I64:
        case TK_PUNCT:
        case TK_EOF:
        case TK_CHAR_CONST: return l1->i64 == l2->i64;
        case TK_F64: return l1->f64 == l2->f64;
        default:
            loggerPanic("Cannot compare %s with %s\n",
                        lexemeToString(l1),
                        lexemeToString(l2));
    }
}
