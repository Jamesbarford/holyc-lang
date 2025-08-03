#ifndef LEXER_H
#define LEXER_H

#include <sys/types.h>

#include "aostr.h"
#include "containers.h"
#include "list.h"

#define LEX_MAX_IDENT_LEN  128
#define LEX_CHAR_CONST_LEN 9

/* TOKENS */
#define TK_EOF          0
#define TK_SUPERSCRIPT  0x301
#define TK_SUBSCRIPT    0x302
#define TK_NORMALSCRIPT 0x303
#define TK_IDENT        0x300
#define TK_STR          0x301
#define TK_I64          0x302
#define TK_CHAR_CONST   0x303
#define TK_F64          0x304
#define TK_PLUS_PLUS    0x305
#define TK_MINUS_MINUS  0x306
#define TK_DEREFERENCE  0x307
#define TK_DBL_COLON    0x308
#define TK_SHL          0x309
#define TK_SHR          0x30A
#define TK_EQU_EQU      0x30B
#define TK_NOT_EQU      0x30C
#define TK_LESS_EQU     0x30D
#define TK_GREATER_EQU  0x30E
#define TK_AND_AND      0x30F
#define TK_OR_OR        0x310
#define TK_XOR_XOR      0x311
#define TK_SHL_EQU      0x312
#define TK_SHR_EQU      0x313
#define TK_MUL_EQU      0x314
#define TK_DIV_EQU      0x315
#define TK_AND_EQU      0x316
#define TK_OR_EQU       0x317
#define TK_XOR_EQU      0x318
#define TK_ADD_EQU      0x319
#define TK_SUB_EQU      0x31A
#define TK_IF           0x31B
#define TK_IFDEF        0x31C
#define TK_IFNDEF       0x31D
#define TK_IFAOT        0x31E
#define TK_IFJIT        0x31F
#define TK_ENDIF        0x320
#define TK_ELSE         0x321
#define TK_MOD_EQU      0x322
#define TK_DOT_DOT      0x323
#define TK_ELLIPSIS     0x324
#define TK_INS_BIN      0x325
#define TK_INS_BIN_SIZE 0x326
#define TK_TKS_NUM      0x327
#define TK_ARROW        0x328
#define TK_PUNCT        0x329
#define TK_PRE_PLUS_PLUS 0x330
#define TK_PRE_MINUS_MINUS 0x331
#define TK_KEYWORD      0x332
#define TK_COMMENT      0x333

#define KW_CLASS       (1<<0) /* Easy to see if struct or union */
#define KW_UNION       (1<<1)
#define KW_U0           0
/* Integers are odd numbers */
#define KW_BOOL         3
#define KW_I8           5
#define KW_U8           7
#define KW_I16          9
#define KW_U16          11
#define KW_I32          13
#define KW_U32          15
#define KW_I64          17
#define KW_U64          19
/* back to even */
#define KW_F64          4
#define KW_PUBLIC       6
#define KW_ATOMIC       8
#define KW_DEFINE       10
#define KW_PP_INCLUDE   12
#define KW_CAST  14
#define KW_SIZEOF       16
#define KW_RETURN       18
#define KW_SWITCH       20
#define KW_CASE         22
#define KW_BREAK        24
#define KW_CONTINUE     26
#define KW_ASM          28
#define KW_ASM_EXTERN   30
#define KW_EXTERN       32
#define KW_PRIVATE      34
#define KW_INLINE       36
#define KW_FOR          38
#define KW_WHILE        40
#define KW_VOLATILE     42
#define KW_GOTO         44
#define KW_IF           46
#define KW_ELSE         48
#define KW_ELIF         50
#define KW_IF_NDEF      52
#define KW_IF_DEF       54
#define KW_ENDIF        56
#define KW_DEFINED      58
#define KW_UNDEF        60
#define KW_AUTO         62
#define KW_DEFAULT      64
#define KW_DO           66
#define KW_STATIC       67
#define KW_ELIF_DEF     68
#define KW_PP_IF        69
#define KW_PP_ELSE      70
#define KW_PP_DEFINE    71
#define KW_PP_IF_NDEF   72
#define KW_PP_IF_DEF    73
#define KW_PP_ELIF_DEF  74
#define KW_PP_ENDIF     75
#define KW_PP_ELIF      76
#define KW_PP_DEFINED   77
#define KW_PP_UNDEF     78
#define KW_PP_ERROR     79

/* Compiler Flags*/
#define CCF_MULTI_CHAR_OP     (1<<0)
#define CCF_PRE_PROC          (1<<1)
#define CCF_ACCEPT_NEWLINES   (1<<2)
#define CCF_MULTI_COLON       (1<<3)
#define CCF_ASM_BLOCK         (1<<4)
#define CCF_BLOCK             (1<<5)
#define CCF_ACCEPT_WHITESPACE (1<<6)
#define CCF_ACCEPT_COMMENTS   (1<<7)

#define LEXEME_RAW_PUNCT              (1<<0)
#define LEXEME_ENCODE_PUNCT           (1<<1)
#define LEXEME_GRAPH_VIZ_ENCODE_PUNCT (1<<2)

typedef struct Lexeme {
    int tk_type;
    int len;
    int line;
    char *start;
    int ishex;
    union {
        s64 i64;
        double f64;
    };
} Lexeme;

typedef struct LexFile {
    AoStr *filename; /* name of the file */
    char *ptr; /* Where we are in the file */
    int lineno; /* line number in the file */
    AoStr *src; /* source */
} LexFile;

typedef struct Lexer {
    int tk_type;
    char *ptr;
    char *start;
    char cur_ch;
    char *cur_str;
    s64 cur_strlen;
    s64 cur_i64;
    double cur_f64;
    int lineno;
    int flags;
    int ishex;
    int collecting;
    int skip_else;
    char *builtin_root;
    List *files;
    List *all_source;/* This saves all of the files we see so we can free them later */
    Set *seen_files;
    Map *symbol_table;
    LexFile *cur_file;
} Lexer;

void lexemeMemoryInit(void);
void lexemeMemoryRelease(void);
void lexemeMemoryStats(void);

Lexeme *lexemeTokNew(char *start, int len, int line, s64 ch);
Lexeme *lexemeNew(char *start, int len);
Lexeme *lexemeSentinal(void);
void lexSetBuiltinRoot(Lexer *l, char *root);
void lexInit(Lexer *l, char *source, int flag);
void lexPushFile(Lexer *l, AoStr *filename);
int lex(Lexer *l, Lexeme *le);
Lexeme *lexToken(Map *macro_defs, Lexer *l);
void lexemePrint(Lexeme *le);
char *lexemeTypeToString(int tk_type);
char *lexemePunctToString(s64 op);
char *lexemePunctToStringWithFlags(s64 op, u64 flags);
char *lexemePunctToEncodedString(s64 op);
char *lexemeToString(Lexeme *tok);
AoStr *lexemeToAoStr(Lexeme *tok);
void lexReleaseAllFiles(Lexer *l);
int tokenPunctIs(Lexeme *tok, s64 ch);
int tokenIdentIs(Lexeme *tok, char *ident, int len);
void lexemeFree(void *_le);
char *lexerReportLine(Lexer *l, s64 lineno);
int lexemeEq(Lexeme *l1, Lexeme *l2);
char *lexReadfile(char *path, s64 *_len);


#endif // !LEXER_H
