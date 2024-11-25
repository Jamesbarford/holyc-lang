#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "list.h"
#include "map.h"
#include "parser.h"
#include "transpiler.h"
#include "util.h"

#define TRANSPILE_FLAG_SWITCH_CHAR (1<<0)
#define TRANSPILE_FLAG_ISATTY      (1<<1)

typedef struct TranspilationMapping {
    char *name;
    int name_len;
    char *sub;
    int sub_len;
} TranspilationMapping;

typedef struct TranspileCtx {
    unsigned long flags;
    StrMap *used_types;
    StrMap *used_defines;
    StrMap *skip_defines;
    StrMap *skip_types;
    Cctrl *cc;
    aoStr *buf;
} TranspileCtx;

static void transpileFields(TranspileCtx *ctx,
                            StrMap *fields,
                            StrMap *seen,
                            char *clsname, 
                            aoStr *buf,
                            ssize_t *ident);
aoStr *transpileVarDecl(TranspileCtx *ctx, AstType *type, char *name);
void transpileAstInternal(Ast *ast, TranspileCtx *ctx, ssize_t *indent);
aoStr *transpileArgvList(PtrVec *argv, TranspileCtx *ctx);
aoStr *transpileAst(Ast *ast, TranspileCtx *ctx);
aoStr *transpileFunctionProto(TranspileCtx *ctx, AstType *type, char *name);

static char *transpile_used_c_headers[] = {
    "arpa/inet.h",
    "dirent.h",
    "fcntl.h",
    "math.h",
    "netdb.h",
    "pthread.h",
#ifdef HCC_LINK_SQLITE3
    "sqlite3.h",
#endif
    "stdio.h",
    "stdlib.h",
    "string.h",
    "strings.h",
    "sys/socket.h",
    "sys/stat.h",
    "sys/wait.h",
    "time.h",
    "unistd.h",
};

static char *transpile_skip_defines[] = {
    "__ARCH__",
    "__TIME__",
    "__DATE__",
    "__TIMESTAMP__",
    "NULL",
    "EXIT_FAIL",
    "EXIT_OK",
    "STDOUT",
    "STDERR",
    "STDIN",
    "SOL_SOCKET",
    "AF_UNSPEC",
    "AF_UNIX",
    "AF_INET",
    "AF_INET6",
    "SOCK_STREAM",
    "SOCK_DGRAM",
    "SOCK_RAW",
    "SOCK_SEQPACKET",
    "SO_DEBUG",
    "SO_ACCEPTCONN",
    "SO_REUSEADDR",
    "SO_KEEPALIVE",
    "SO_DONTROUTE",
    "SO_BROADCAST",
    "SO_LINGER",
    "SO_OOBINLINE",
    "SO_REUSEPORT",
    "SO_TIMESTAMP",
    "SO_TIMESTAMP_MONOTONIC",
    "IPPROTO_IP",
    "IPPROTO_IPV6",
    "TCP_NODELAY",
    "TCP_MAXSEG",
    "TCP_NOPUSH",
    "TCP_NOOPT",
    "TCP_KEEPALIVE",
    "TCP_CONNECTIONTIMEOUT",
    "PERSIST_TIMEOUT",
    "TCP_RXT_CONNDROPTIME",
    "TCP_RXT_FINDROP",
    "TCP_KEEPINTVL",
    "TCP_KEEPCNT",
    "TCP_SENDMOREACKS",
    "TCP_ENABLE_ECN",
    "TCP_FASTOPEN",
    "TCP_CONNECTION_INFO",
    "INET6_ADDRSTRLEN",
    "AI_PASSIVE",
    "AI_CANONNAME",
    "AI_NUMERICHOST",
    "AI_NUMERICSERV",
    "AI_UNUSABLE",
    "_SS_PAD1SIZE",
    "_SS_PAD2SIZE",
    "F_DUPFD",
    "F_GETFD",
    "F_SETFD",
    "F_GETFL",
    "F_SETFL",
    "F_FLUSH_DATA",
    "F_CHKCLEAN",
    "F_PREALLOCATE",
    "F_SETSIZE",
    "F_RDADVISE",
    "F_RDAHEAD",
    "FD_CLOEXEC",
    "F_GETOWN",
    "F_SETOWN",
    "F_GETLK",
    "F_SETLK",
    "F_SETLKW",
    "F_RDLCK",
    "F_UNLCK",
    "F_WRLCK",
    "O_RDONLY",
    "O_WRONLY",
    "O_RDWR",
    "O_ACCMODE",
    "O_CREAT",
    "O_TRUNC",
    "O_EXCL",
    "O_NONBLOCK",
    "O_APPEND",
    "SEEK_SET",
    "SEEK_CUR",
    "SEEK_END",
    "DT_UNKNOWN",
    "DT_FIFO",
    "DT_CHR",
    "DT_DIR",
    "DT_BLK",
    "DT_REG",
    "DT_LNK",
    "DT_SOCK",
    "DT_WHT",
    "S_IFMT",
    "S_IFIFO",
    "S_IFCHR",
    "S_IFDIR",
    "S_IFBLK",
    "S_IFREG",
    "S_IFLNK",
    "S_IFSOCK",
    "S_IRWXU",
    "S_IRUSR",
    "S_IWUSR",
    "S_IXUSR",
    "S_IRWXG",
    "S_IRGRP",
    "S_IWGRP",
    "S_IXGRP",
    "S_IRWXO",
    "S_IROTH",
    "S_IWOTH",
    "S_IXOTH",
    "S_ISUID",
    "S_ISGID",
    "S_ISVTX",
};

static char *transpile_skip_classes[] = {
    "hostent",
    "netent",
    "servent",
    "protoent",
    "sockaddr",
    "in_addr",
    "sockaddr_in",
    "in6_addr",
    "sockaddr_in6",
    "addrinfo",
    "sockaddr_storage",
    "sockaddr_un",
    "iovec",
    "pthread_mutex_t",
    "pthread_mutexattr_t",
    "pthread_cond_t",
    "pthread_condattr_t",
    "pthread_handler_rec",
    "pthread_attr_t",
    "pthread_t",
    "tm",
    "timeval",
    "msghdr"
};

static TranspilationMapping transpile_function_substitutions_table[] = {
    {str_lit("_MALLOC"), str_lit("malloc")},
    {str_lit("_CALLOC"), str_lit("calloc")},
    {str_lit("_REALLOC"), str_lit("realloc")},
    {str_lit("_MEMCPY"), str_lit("memcpy")},
    {str_lit("_MEMSET"), str_lit("memset")},
    {str_lit("_FREE"), str_lit("free")},
    {str_lit("_STRLEN_FAST"), str_lit("strlen")},
    {str_lit("_STRNCMP"), str_lit("strncmp")},
    {str_lit("_STRNICMP"), str_lit("strncasecmp")},
    {str_lit("_STRCMP"), str_lit("strcmp")},
    {str_lit("_STRCPY"), str_lit("strcpy")},
    {str_lit("_TOLOWER"), str_lit("tolower")},
    {str_lit("_TOUPPER"), str_lit("toupper")},
    {str_lit("_ISSPACE"), str_lit("isspace")},
    {str_lit("_ATOI"), str_lit("atoi")},
    {str_lit("_EXIT"), str_lit("exit")},
    {str_lit("Exit"), str_lit("exit")},
};

/* singleton maps, we don't need unique instances and do not mutate these */
static StrMap *transpile_function_substitutions = NULL;
static StrMap *transpile_skip_defines_map = NULL;
static StrMap *transpile_skip_classes_map = NULL;
static int is_init = 0;

void transpileInitMaps(void) {
    if (!is_init) {
        transpile_function_substitutions = strMapNew(32);
        transpile_skip_defines_map = strMapNew(32);
        transpile_skip_classes_map = strMapNew(32);

        ssize_t len = static_size(transpile_skip_defines);
        for (ssize_t i = 0; i < len; ++i) {
            char *define = transpile_skip_defines[i];
            strMapAdd(transpile_skip_defines_map, define, define);
        }

        len = static_size(transpile_skip_classes);
        for (ssize_t i = 0; i < len; ++i) {
            char *cls = transpile_skip_classes[i];
            strMapAdd(transpile_skip_classes_map, cls, cls);
        }
    
        len = static_size(transpile_function_substitutions_table);
        for (ssize_t i = 0; i < len; ++i) {
            TranspilationMapping *sub = &transpile_function_substitutions_table[i]; 
            strMapAdd(transpile_function_substitutions,sub->name,sub->sub);
        }
    }
}

TranspileCtx *transpileCtxNew(Cctrl *cc) {
    TranspileCtx *ctx = malloc(sizeof(TranspileCtx));
    ctx->used_types = strMapNew(32);
    ctx->used_defines = strMapNew(32);

    transpileInitMaps();
    ctx->skip_types = transpile_skip_classes_map;
    ctx->skip_defines = transpile_skip_defines_map;
    ctx->buf = NULL;
    ctx->cc = cc;
    ctx->flags = 0;
    if (isatty(STDOUT_FILENO)) {
        ctx->flags |= TRANSPILE_FLAG_ISATTY;
    }

    return ctx;
}

void transpileCtxAddType(TranspileCtx *ctx, char *type_name) {
    strMapAdd(ctx->used_types,type_name,type_name);
}

void transpileCtxAddDefine(TranspileCtx *ctx, char *def_name) {
    strMapAdd(ctx->used_defines,def_name,def_name);
}

void transpileCtxSetBuffer(TranspileCtx *ctx, aoStr *buf) {
    ctx->buf = buf;
}

int transpileCtxShouldEmitType(TranspileCtx *ctx, char *name, int len) {
    if (strMapGetLen(ctx->skip_types, name, len) != NULL) {
        return 0;
    }
    return 1;
}

int transpileCtxShouldEmitDefine(TranspileCtx *ctx, char *name, int len) {
    if (strMapGetLen(ctx->skip_defines, name, len) != NULL) {
        return 0;
    }
    return strMapGetLen(ctx->used_defines, name, len) != NULL;
}

/* make lower camelcase */
aoStr *transpileFormatFunction(aoStr *fname) {
    aoStr *dupped = aoStrDup(fname);
    dupped->data[0] = tolower(dupped->data[0]);
    return dupped;
}

char *transpileKeyWordHighlight(TranspileCtx *ctx, int ast_kind) {
    if (ctx->flags & TRANSPILE_FLAG_ISATTY) {
        switch (ast_kind) {
            case KW_DO: return ESC_WHITE"do"ESC_RESET;
            case KW_WHILE: return ESC_WHITE"while"ESC_RESET;
            case KW_FOR: return ESC_WHITE"for"ESC_RESET;
            case KW_SWITCH: return ESC_WHITE"switch"ESC_RESET;
            case KW_IF: return ESC_WHITE"if"ESC_RESET;
            case KW_ELSE: return ESC_WHITE"else"ESC_RESET;
            case KW_DEFAULT: return ESC_WHITE"default"ESC_RESET;
            case KW_SIZEOF: return ESC_WHITE"sizeof"ESC_RESET;
            case KW_CONTINUE: return ESC_WHITE"continue"ESC_RESET;
            case KW_BREAK: return ESC_WHITE"break"ESC_RESET;
            case KW_STATIC: return ESC_WHITE"static"ESC_RESET;
            case KW_INLINE: return ESC_WHITE"inline"ESC_RESET;
            case KW_RETURN: return ESC_WHITE"return"ESC_RESET;
            case KW_GOTO: return ESC_WHITE"goto"ESC_RESET;
            case KW_CASE: return ESC_WHITE"case"ESC_RESET;
            case KW_VOLATILE: return ESC_WHITE"volatile"ESC_RESET;
            default: loggerDebug("Internal error - invalid keyword!");
        }
    } else {
        switch (ast_kind) {
            case KW_DO: return "do";
            case KW_WHILE: return "while";
            case KW_FOR: return "for";
            case KW_SWITCH: return "switch";
            case KW_IF: return "if";
            case KW_ELSE: return "else";
            case KW_DEFAULT: return "default";
            case KW_SIZEOF: return "sizeof";
            case KW_CONTINUE: return "continue";
            case KW_BREAK: return "break";
            case KW_STATIC: return "static";
            case KW_INLINE: return "inline";
            case KW_RETURN: return "return";
            case KW_GOTO: return "goto";
            case KW_CASE: return "case";
            case KW_VOLATILE: return "volatile";
            default: loggerDebug("Internal error - invalid keyword!");
        }
    }
    return NULL;
}

char *transpileHighlightInt(TranspileCtx *ctx, long integer) {
    static char buf[64];
    if (ctx->flags & TRANSPILE_FLAG_ISATTY) {
        snprintf(buf,sizeof(buf),ESC_WHITE"%ld"ESC_RESET,integer);
    } else {
        snprintf(buf,sizeof(buf),"%ld",integer);
    }
    return buf;
}

char *transpileHighlightChar(TranspileCtx *ctx, char character) {
    static char buf[64];
    ssize_t len = 0;
    if (ctx->flags & TRANSPILE_FLAG_ISATTY) {
        len = snprintf(buf,sizeof(buf),ESC_CYAN"'%c'"ESC_RESET,character);
    } else {
        len = snprintf(buf,sizeof(buf),"'%c'",character);
    }
    buf[len] = '\0';
    return buf;
}

char *transpileHighlightStringAsCharacter(TranspileCtx *ctx, char *str) {
    if (ctx->flags & TRANSPILE_FLAG_ISATTY) {
        return mprintf(ESC_CYAN"'%s'"ESC_RESET,str);
    } else {
        return mprintf("'%s'",str);
    }
}

char *transpileHighlightStringAsInt(TranspileCtx *ctx, char *str) {
    if (ctx->flags & TRANSPILE_FLAG_ISATTY) {
        return mprintf(ESC_WHITE"%s"ESC_RESET,str);
    } else {
        return mprintf("%s",str);
    }
}

char *transpileHighlightString(TranspileCtx *ctx, char *str) {
    if (ctx->flags & TRANSPILE_FLAG_ISATTY) {
        return mprintf(ESC_GREEN"\"%s\""ESC_RESET,str);
    } else {
        return mprintf("\"%s\"",str);
    }
}

char *transpileHighlightFloat(TranspileCtx *ctx, double floatingpoint) {
    static char buf[64];
    if (ctx->flags & TRANSPILE_FLAG_ISATTY) {
        snprintf(buf,sizeof(buf),ESC_WHITE"%g"ESC_RESET,floatingpoint);
    } else {
        snprintf(buf,sizeof(buf),"%g",floatingpoint);
    }
    return buf;
}

char *transpileGetFunctionSub(char *name, int len) {
    return strMapGet(transpile_function_substitutions, name);
}

static void transpileEndStmt(aoStr *str) {
    if (str->data[str->len-1] != '\n' && str->data[str->len-2] != ';' && str->data[str->len-2] != '}') {
        aoStrCatLen(str,str_lit(";\n"));
    }
}


void transpileBinaryOp(TranspileCtx *ctx, aoStr *buf, char *op, Ast *ast, ssize_t *indent) {
    ssize_t saved_indent = *indent;
    int needs_brackets = 0;

    *indent = 0;
    if (ast->left && ast->left->kind == AST_DEREF) {
        if (ast->left) {
            transpileAstInternal(ast->left,ctx, indent);
        } else {
            transpileAstInternal(ast,ctx, indent);
        }
        aoStrCatFmt(buf, " %s ", op);
        if (ast->right) {
            transpileAstInternal(ast->right,ctx, indent);
        }
    } else if (!ast->right) { // unary
        /* If the operation on the left is a binary op then we need brackets */
        aoStrCatFmt(buf, "%s",op);
        if (ast->left && ast->left->right) {
            needs_brackets = 1;
            aoStrPutChar(buf, '(');
        }
        transpileAstInternal(ast->left,ctx, indent);
        if (needs_brackets) {
            aoStrPutChar(buf, ')');
        }
    } else {
        /* We need to add parenthesis around something like: 
         * ```
         * while ((var = funCall()) != NULL) {
         *   // ... 
         * }
         * ```
         *
         * Which is where the left hand side is an assignment and the operator 
         * for the binary expression is a comparison.
         * */
        if (astIsAssignment(ast->left->kind) && astIsBinCmp(ast->kind)) {
            needs_brackets = 1;
            aoStrPutChar(buf,'(');
        }

        transpileAstInternal(ast->left,ctx, indent);
        if (needs_brackets) {
            aoStrPutChar(buf,')');
        }
        aoStrCatFmt(buf, " %s ",op);
        transpileAstInternal(ast->right, ctx, indent);
    }
    *indent = saved_indent;
}

aoStr *transpileLValue(Ast *ast, TranspileCtx *ctx) {
    aoStr *buf = aoStrNew();
    ssize_t indent = 0;
    aoStr *saved = ctx->buf;
    transpileCtxSetBuffer(ctx, buf);
    transpileAstInternal(ast,ctx,&indent);
    transpileCtxSetBuffer(ctx, saved);
    return buf;
}

void transpileAstInternal(Ast *ast, TranspileCtx *ctx, ssize_t *indent) {
    if (ast == NULL) {
        return;
    }

    aoStr *buf = ctx->buf;

    ssize_t saved_indent = 0;
    List *node;
    aoStr *escaped;

    switch(ast->kind) {
    case AST_LITERAL: {
        switch (ast->type->kind) {
        case AST_TYPE_VOID: break;
        case AST_TYPE_INT: {
            aoStrCat(buf, transpileHighlightInt(ctx, ast->i64));
            break;
        }    
        case AST_TYPE_CHAR:  {
            char tmp_buf[32];
            char *tmp_ptr = tmp_buf;
            unsigned long ch = ast->i64;
            int i = 0;
            for (; i < 8; ++i) {
                unsigned long idx = i * 8;
                char ch2 = ((unsigned long)ch) >> idx & 0xFF;
                if ((idx > 0 && !ch2) || (ch2 < 0)) break;
                switch (ch2) {
                    case '\'': { *tmp_ptr++ = '\\'; *tmp_ptr++ = '\''; break; }
                    case '\\': { *tmp_ptr++ = '\\'; *tmp_ptr++ = '\\'; break; }
                    case '\0': { *tmp_ptr++ = '\\'; *tmp_ptr++ = '0'; break; }
                    case '\b': { *tmp_ptr++ = '\\'; *tmp_ptr++ = 'b'; break; }
                    case '\n': { *tmp_ptr++ = '\\'; *tmp_ptr++ = 'n'; break; }
                    case '\t': { *tmp_ptr++ = '\\'; *tmp_ptr++ = 't'; break; }
                    case '\v': { *tmp_ptr++ = '\\'; *tmp_ptr++ = 'v'; break; }
                    case '\f': { *tmp_ptr++ = '\\'; *tmp_ptr++ = 'f'; break; }
                    case '\r': { *tmp_ptr++ = '\\'; *tmp_ptr++ = 'r'; break; }
                    default: *tmp_ptr++ = ch2; break;
                }
            }
            if (tmp_ptr - tmp_buf == 1) {
                tmp_buf[i] = '\0';
            } else {
                tmp_buf[i+1] = '\0';
            }
            char *defn = transpileHighlightStringAsCharacter(ctx, tmp_buf);
            aoStrCat(buf,defn);
            free(defn);
            break;
        }
        
        case AST_TYPE_FLOAT: {
            aoStrCat(buf, transpileHighlightFloat(ctx, ast->f64));
            break;
        }
        default:
            loggerPanic("Unhandled type: %d\n", ast->type->kind);
        }
        break;

        case AST_STRING: {
            escaped = aoStrEscapeString(ast->sval);
            char *defn = transpileHighlightString(ctx,escaped->data);
            aoStrCat(buf,defn);
            aoStrRelease(escaped);
            free(defn);
            break;
        }
    }

    case AST_COMMENT: {
        aoStrCatFmt(buf, "%S", ast->sval);
        break;
    }

    /* we are not defining anything ... */
    case AST_LVAR: {
        strMapAdd(ctx->used_defines,ast->lname->data,ast->lname->data);
        aoStrCatAoStr(buf,ast->lname);
        break;
    }    
    
    case AST_DECL: {
        char *name = NULL;
        if (ast->declvar->kind == AST_FUNPTR) {
            name = ast->declvar->fname->data;
        } else if (ast->declvar->kind == AST_LVAR) {
            name = ast->declvar->lname->data;
            strMapAdd(ctx->used_defines,name,name);
        } else if (ast->declvar->kind == AST_GVAR) {
            name = ast->declvar->gname->data;
        } else {
            loggerPanic("Unhandled declaration\n");
        }

        aoStr *decl = transpileVarDecl(ctx, ast->declvar->type, name);
        if (ast->declvar->kind == AST_GVAR && ast->declvar->is_static) {
            aoStrCatFmt(buf,  "%s ", transpileKeyWordHighlight(ctx,KW_STATIC));
        }

        aoStrCatAoStr(buf, decl);
        if (ast->declinit) {
            aoStrCatFmt(buf, " = ");
            saved_indent = *indent;
            *indent = 0;
            transpileAstInternal(ast->declinit,ctx,indent);
            *indent = saved_indent;
        }
        aoStrRelease(decl);
        break;
    }

    case AST_GVAR:
        aoStrCatFmt(buf, "%S", ast->gname);
        break;
    
    case AST_ASM_FUNCALL: {
        Ast *asm_stmt = strMapGet(ctx->cc->asm_functions, ast->fname->data);
        char *substitution_name = transpileGetFunctionSub(ast->fname->data,
                                                          ast->fname->len);
        aoStr *asm_fname = asm_stmt ? asm_stmt->fname : ast->fname;
        char *fname = substitution_name ? substitution_name : asm_fname->data;
        aoStr *argv = transpileArgvList(ast->args, ctx);
        aoStrCatFmt(buf, "%s(%S)", fname, argv);
        aoStrRelease(argv);
        break;
    }

    case AST_FUNPTR_CALL:
        /* For when a function pointer exists on a class, interestingly 
         * due to maintaining a reference to the class this we be trivial 
         * to create class methods */
        if (ast->ref && ast->ref->kind == AST_CLASS_REF) {
            Ast *ref = ast->ref;
            transpileAstInternal(ref->cls, ctx, indent);
            if (ref->cls->deref_symbol == TK_ARROW) {
                aoStrCat(buf, "->");
            } else {
                aoStrCat(buf, ".");
            }
        }
    case AST_FUNPTR:
    case AST_FUNCALL: {
        aoStr *formatted = transpileFormatFunction(ast->fname);
        // this is not a function call!
        if (!ast->args) {
            aoStrCatFmt(buf, "%S", formatted);
            break;
        }
        aoStr *argv = transpileArgvList(ast->args, ctx);
        if (argv->len) {
            aoStrCatFmt(buf, "%S(%S)", formatted, argv);
        } else {
            aoStrCatFmt(buf, "%S()", formatted);
        }
        aoStrRelease(argv);
        aoStrRelease(formatted);
        break;
    }

    case AST_FUNC:
    case AST_FUN_PROTO:
    case AST_EXTERN_FUNC: {
        aoStr *formatted = transpileFormatFunction(ast->fname);
        aoStrCatFmt(buf, "&%S", formatted);
        aoStrRelease(formatted);
        break;
    }

    case AST_ASM_FUNC_BIND: {
        aoStrCatFmt(buf, "// unsupported x86 function bind %S\n",ast->asmfname);
        break;
    }

    case AST_ASM_STMT: {
        aoStrCatFmt(buf, "<asm_block>\n");
        break;
    }

    case AST_ARRAY_INIT: {
        node = ast->arrayinit->next;
        aoStrCatFmt(buf, "{");
        saved_indent = *indent;
        *indent = 0;
        while (node != ast->arrayinit) {
            transpileAstInternal(node->value, ctx, indent);
            if (node->next != ast->arrayinit) {
                aoStrCatLen(buf,str_lit(", "));
            }
            node = node->next;
        }
        *indent = saved_indent;
        aoStrCatFmt(buf, "}");
        break;
    }

    case AST_IF: {
        char *_if = transpileKeyWordHighlight(ctx,KW_IF);
        char *_else = transpileKeyWordHighlight(ctx,KW_ELSE);
        aoStrCatFmt(buf, "%s (", _if);

        saved_indent = *indent;
        *indent = 0;
        transpileAstInternal(ast->cond,ctx, indent);
        *indent = saved_indent;

        aoStrCatFmt(buf, ") {\n");
        *indent += 4;
        if (ast->then) {
            if (ast->then->kind != AST_COMPOUND_STMT) {
                aoStrRepeatChar(buf,' ',*indent);
            }
            transpileAstInternal(ast->then,ctx, indent);
            if (ast->then->kind != AST_COMPOUND_STMT) {
                transpileEndStmt(buf); 
            }
        }
        *indent -= 4;
        aoStrRepeatChar(buf, ' ', *indent);
        aoStrCatFmt(buf, "}");
        if (ast->els) {
            aoStrCatFmt(buf, " %s {\n", _else);
            *indent += 4;
            if (ast->els->kind != AST_COMPOUND_STMT) {
                aoStrRepeatChar(buf,' ',*indent);
            }
            transpileAstInternal(ast->els,ctx, indent);
            if (ast->els->kind != AST_COMPOUND_STMT) {
                transpileEndStmt(buf); 
            }
            *indent -= 4;
            aoStrRepeatChar(buf, ' ', *indent);
            aoStrCatFmt(buf, "}\n");
        } else {
            aoStrPutChar(buf,'\n');
        }
        break;
    }

    case AST_DO_WHILE: {
        char *_do = transpileKeyWordHighlight(ctx,KW_DO);
        char *_while = transpileKeyWordHighlight(ctx,KW_WHILE);
        aoStrCatFmt(buf, "%s {\n", _do);
        *indent += 4;
        if (ast->whilebody && ast->whilebody->kind != AST_COMPOUND_STMT) {
            aoStrRepeatChar(buf, ' ', *indent);
        }
        transpileAstInternal(ast->whilebody,ctx, indent);
        if (ast->whilebody && ast->whilebody->kind != AST_COMPOUND_STMT) {
            transpileEndStmt(buf);
        }
        *indent -= 4;
        saved_indent = *indent;
        aoStrRepeatChar(buf, ' ', *indent);
        aoStrCatFmt(buf, "} %s (", _while);
        *indent = 0;
        transpileAstInternal(ast->whilecond,ctx, indent);
        *indent = saved_indent;
        aoStrCatFmt(buf, ");\n");
        break;
    }

    case AST_WHILE: {
        char *_while = transpileKeyWordHighlight(ctx,KW_WHILE);
        aoStrCatFmt(buf, "%s (", _while); 
        saved_indent = *indent;
        *indent = 0;
        transpileAstInternal(ast->whilecond,ctx, indent);
        aoStrCatFmt(buf, ")");
        *indent = saved_indent;
        *indent += 4;
        if (!ast->whilebody) {
            aoStrPutChar(buf,'\n');
            aoStrRepeatChar(buf, ' ', *indent);
            *indent -= 4;
        } else {
            aoStrCatLen(buf, str_lit(" {\n"));
            if (ast->whilebody->kind != AST_COMPOUND_STMT) {
                aoStrRepeatChar(buf, ' ', *indent);
            }
            transpileAstInternal(ast->whilebody,ctx, indent);
            if (ast->whilebody->kind != AST_COMPOUND_STMT) {
                transpileEndStmt(buf);
            }
            *indent -= 4;
            aoStrRepeatChar(buf,' ', *indent);
            aoStrCatFmt(buf,"}\n");
        }
        break;
    }

    case AST_FOR: {
        char *_for = transpileKeyWordHighlight(ctx, KW_FOR);
        aoStrCatFmt(buf, "%s (", _for);

        saved_indent = *indent;
        if (ast->forinit) {
            *indent = 0;
            transpileAstInternal(ast->forinit,ctx, indent);
            *indent = saved_indent;
        }
        aoStrCatLen(buf,str_lit("; "));

        if (ast->forcond) {
            *indent = 0;
            transpileAstInternal(ast->forcond,ctx, indent);
            *indent = saved_indent;
        }
        aoStrCatLen(buf,str_lit("; "));

        if (ast->forstep) {
            *indent = 0;
            transpileAstInternal(ast->forstep,ctx, indent);
            *indent = saved_indent;
        }

        aoStrCatFmt(buf,") {\n");
        *indent += 4;
        if (ast->forbody && ast->forbody->kind != AST_COMPOUND_STMT) {
            aoStrRepeatChar(buf, ' ', *indent);
        }
        transpileAstInternal(ast->forbody,ctx, indent);
        if (ast->forbody && ast->forbody->kind != AST_COMPOUND_STMT) {
            transpileEndStmt(buf);
        }
        *indent -= 4;
        aoStrRepeatChar(buf, ' ', *indent);
        aoStrCatFmt(buf,"}\n");
        break;
    }

    case AST_RETURN: {
        char *_return = transpileKeyWordHighlight(ctx,KW_RETURN);
        aoStrCatFmt(buf, "%s ", _return);
        transpileAstInternal(ast->retval,ctx,indent);
        break;
    }

    case AST_VAR_ARGS:
        aoStrCatFmt(buf, "...");
        break;

    case AST_COMPOUND_STMT: {
        List *it = ast->stms->next;
        Ast *next;
        while (it != ast->stms) {
            next = it->value;
            aoStrRepeatChar(buf, ' ', *indent);
            transpileAstInternal(next,ctx, indent);
            transpileEndStmt(buf);
            it = it->next;
        }
        break;
    }

    case AST_CLASS_REF: {
        transpileAstInternal(ast->cls, ctx, indent);
        if (ast->cls->deref_symbol == TK_ARROW) {
            aoStrCatFmt(buf, "->%s", ast->field);
        } else {
            aoStrCatFmt(buf, ".%s", ast->field);
        }
        break;
    }

    case AST_JUMP: {
        char *label = ast->jump_label->data;
        if (*label == '.') label++;
        char *_goto = transpileKeyWordHighlight(ctx, KW_GOTO);
        aoStrCatFmt(buf, "%s %s;\n", _goto, label);
        break;
    }

    case AST_GOTO: {
        char *label = ast->slabel->data;
        if (*label == '.') label++;
        char *_goto = transpileKeyWordHighlight(ctx, KW_GOTO);
        aoStrCatFmt(buf, "%s %s;\n", _goto, label);
        break;
    }

    /* XXX: fix labels */
    case AST_LABEL: {
        char *label = ast->slabel ? ast->slabel->data : ast->slabel->data;
        while (buf->data[buf->len - 1] != '\n') {
            buf->len--;
        }
        if (*label == '.') label++;
        aoStrCatFmt(buf, "\n%s:\n", label);
        break;
    }

    case AST_CAST: {
        aoStr *type_cast = transpileVarDecl(ctx, ast->type,NULL);
        aoStr *lvalue = transpileLValue(ast->operand, ctx);
        aoStrCatFmt(buf, "(%S)%S", type_cast, lvalue);
        aoStrRelease(type_cast);
        aoStrRelease(lvalue);
        break;
    }

    case AST_SWITCH: {
        char *_switch = transpileKeyWordHighlight(ctx, KW_SWITCH);
        aoStrCatFmt(buf, "%s (", _switch);
        saved_indent = *indent;
        *indent = 0;
        transpileAstInternal(ast->switch_cond, ctx, indent);
        aoStrCatFmt(buf, ") {\n");
        *indent = saved_indent;

        if (ast->switch_cond->type->size == 1) {
            ctx->flags |= TRANSPILE_FLAG_SWITCH_CHAR;
        }
        *indent += 4;
        for (int i = 0; i < ast->cases->size; ++i) {
            Ast *_case = ast->cases->entries[i];
            aoStrRepeatChar(buf, ' ', *indent);
            transpileAstInternal(_case,ctx,indent);
        }
        ctx->flags &= ~TRANSPILE_FLAG_SWITCH_CHAR;
        if (ast->case_default) {
            aoStrRepeatChar(buf,' ', *indent);
            transpileAstInternal(ast->case_default,ctx,indent);
        }
        *indent -= 4;
        aoStrRepeatChar(buf,' ', *indent);
        aoStrCatFmt(buf,"}\n");
        break;
    }

    case AST_CASE: {
        char *_case = transpileKeyWordHighlight(ctx,KW_CASE);
        aoStrCatFmt(buf, "%s ", _case);
        if (ast->type->kind == AST_TYPE_VOID) {
            aoStrCatFmt(buf, "%s:", transpileHighlightStringAsInt(ctx, ast->case_label->data));
        } else if (ctx->flags & TRANSPILE_FLAG_SWITCH_CHAR) {
            if (ast->case_begin == ast->case_end) {
                aoStrCatFmt(buf, "%s:",transpileHighlightChar(ctx, ast->case_begin));
            } else {
                aoStrCatFmt(buf, "%s ... %s:",
                        transpileHighlightChar(ctx,(char)ast->case_begin),
                        transpileHighlightChar(ctx,(char)ast->case_end));
            }
        } else {
            if (ast->case_begin == ast->case_end) {
                aoStrCatFmt(buf, "%s:", transpileHighlightInt(ctx, ast->case_begin));
            } else {
                aoStrCatFmt(buf, "%s ... %s:",
                        transpileHighlightInt(ctx, ast->case_begin),
                        transpileHighlightInt(ctx, ast->case_end));
            }
        }

        if (!listEmpty(ast->case_asts)) {
            aoStrCatFmt(buf," {\n");
            *indent += 4;
            listForEach(ast->case_asts) {
                Ast *case_ast = (Ast *)it->value;
                if (case_ast->kind != AST_COMPOUND_STMT) {
                    aoStrRepeatChar(buf,' ', *indent);
                }
                transpileAstInternal(case_ast,ctx, indent);
                if (case_ast->kind != AST_COMPOUND_STMT) {
                    transpileEndStmt(buf);
                }
            }
            *indent -= 4;
            aoStrRepeatChar(buf,' ', *indent);
            aoStrCatFmt(buf,"}\n\n");
        } else {
            aoStrPutChar(buf,'\n');
        }
        break;
    }

    case AST_DEFAULT: {
        char *_default = transpileKeyWordHighlight(ctx, KW_DEFAULT);
        aoStrCatFmt(buf, "%s: {\n", _default);
        if (!listEmpty(ast->case_asts)) {
            *indent += 4;
            listForEach(ast->case_asts) {
                aoStrRepeatChar(buf,' ', *indent);
                transpileAstInternal(it->value,ctx,indent);
                transpileEndStmt(buf);
            }
            *indent -= 4;
            aoStrRepeatChar(buf,' ', *indent);
        }
        aoStrCatFmt(buf,"}\n");
        break;
    }

    case AST_BREAK:
        aoStrCatFmt(buf, "%s;\n", transpileKeyWordHighlight(ctx,KW_BREAK));
        break;

    case AST_CONTINUE:
        aoStrCatFmt(buf, "%s;\n", transpileKeyWordHighlight(ctx,KW_CONTINUE));
        break;

    case AST_DEFAULT_PARAM: {
        aoStrCatFmt(buf, "%S", ast->declvar->lname);
        break;
    }

    case AST_SIZEOF: {
        aoStr *type_str = transpileVarDecl(ctx, ast->type, NULL);
        aoStrCatFmt(buf, "%s(%S)", transpileKeyWordHighlight(ctx,KW_SIZEOF), type_str);
        aoStrRelease(type_str);
        break;
    }

    case TK_PLUS_PLUS:
    case TK_MINUS_MINUS: {
        char *op = lexemePunctToString(ast->kind);
        transpileAstInternal(ast->left,ctx, indent);
        aoStrCatFmt(buf, "%s",op);
        break;
    }

    case AST_ADDR:
        aoStrCatFmt(buf, "&");
        saved_indent = *indent;
        *indent = 0;
        transpileAstInternal(ast->operand, ctx, indent);
        *indent = saved_indent;
        break;

    case AST_DEREF: {
        if (ast->operand->kind == '+') {
            Ast *left = ast->operand->left;
            Ast *right = ast->operand->right;
            transpileAstInternal(left,ctx,indent);
            aoStrPutChar(buf, '[');
            transpileAstInternal(right,ctx,indent);
            aoStrPutChar(buf, ']');
        } else {
            /* As `->` is a dereference we need to be able to distinguish 
             * between a class dereference and a general pointer dereference */
            if (ast->deref_symbol != TK_ARROW) {
                aoStrCatFmt(buf, "*");
            }
            transpileAstInternal(ast->operand,ctx,indent);
        }
        break;
    }

    default: {
        transpileBinaryOp(ctx,buf,lexemePunctToString(ast->kind),ast,indent);
        break;
    }
    }
}

aoStr *transpileAst(Ast *ast, TranspileCtx *ctx) {
    ssize_t indent = 4;
    aoStr *body_buf = aoStrNew();
    aoStr *saved = ctx->buf;
    transpileCtxSetBuffer(ctx, body_buf);
    transpileAstInternal(ast,ctx,&indent);
    transpileCtxSetBuffer(ctx, saved);
    return body_buf;
}

aoStr *transpileParamsList(PtrVec *params, TranspileCtx *ctx) {
    aoStr *buf = aoStrNew();
    aoStr *decl = NULL;
    for (int i = 0; i < params->size; ++i) {
        Ast *param = params->entries[i];
        aoStr *var = transpileLValue(param, ctx);
        if (param->kind == AST_DEFAULT_PARAM) {
            decl = transpileVarDecl(ctx, param->declvar->type,
                    param->declvar->lname->data);
        } else if (param->kind == AST_FUNPTR) {
            decl = transpileVarDecl(ctx, param->type, param->fname->data);
        } else if (param->kind == AST_VAR_ARGS) {
            decl = aoStrPrintf("...");
        } else  {
            decl = transpileVarDecl(ctx, param->type, param->lname->data);
        } 
        aoStrCatFmt(buf, "%S", decl);
        if (i + 1 != params->size) {
            aoStrCatLen(buf, str_lit(", "));
        }
        aoStrRelease(decl);
        aoStrRelease(var);
    }
    return buf;
}

aoStr *transpileArgvList(PtrVec *argv, TranspileCtx *ctx) {
    if (!argv) return NULL;
    aoStr *buf = aoStrNew();
    for (int i = 0; i < argv->size; ++i) {
        Ast *arg = argv->entries[i];
        aoStr *var = transpileLValue(arg, ctx);
        aoStrCatFmt(buf, "%S", var);
        if (i + 1 != argv->size) {
            aoStrCatLen(buf, str_lit(", "));
        }
        aoStrRelease(var);
    }
    return buf;
}

typedef struct TypeInfo {
    int kind;
    aoStr *base_name;
    int array_dimensions;
    int stars;
    aoStr *params;
    aoStr *array_init_label;
} TypeInfo;

static void transpileTypeInternal(TranspileCtx *ctx, AstType *type, TypeInfo *info) {
    switch (type->kind) {
    case AST_TYPE_VOID: {
        info->base_name = aoStrPrintf("void");
        break;
    }
    
    case AST_TYPE_INT: {
        aoStr *type_str= aoStrNew();
        switch (type->size) {
            case 0:
                aoStrCatFmt(type_str, "void");
                break;
            case 1:
                if (type->issigned) aoStrCatFmt(type_str, "char");
                else                aoStrCatFmt(type_str, "unsigned char");
                break;
            case 2:
                if (type->issigned) aoStrCatFmt(type_str, "short");
                else                aoStrCatFmt(type_str, "unsigned short");
                break;
            case 4:
                if (type->issigned) aoStrCatFmt(type_str, "int");
                else                aoStrCatFmt(type_str, "unsigned int");
                break;
            case 8:
                if (type->issigned) aoStrCatFmt(type_str, "long");
                else                aoStrCatFmt(type_str, "unsigned long");
                break;
            default: loggerPanic("Unknown integer size: %d\n", type->size);
        }
        info->base_name = type_str;
        break;
    }
    
    case AST_TYPE_CHAR: {
        aoStr *type_str= aoStrNew();
        if (type->issigned) aoStrCatFmt(type_str, "char");
        else                aoStrCatFmt(type_str, "unsigned char");
        info->base_name = type_str;
        break;
    }

    case AST_TYPE_FLOAT:
        info->base_name = aoStrPrintf("double");
        break;

    case AST_TYPE_POINTER: {
        info->stars++;
        transpileTypeInternal(ctx,type->ptr,info);
        break;
    }

    case AST_TYPE_ARRAY: {
        transpileTypeInternal(ctx, type->ptr, info);
        if (type->size == -1 && type->ptr->clsname != NULL) {
            info->array_init_label = type->ptr->clsname;
        } else {
            info->array_dimensions = type->size;
        }
        break;
    }

    case AST_TYPE_UNION: {
        if (type->clsname) {
            info->base_name = aoStrPrintf("%s", type->clsname->data);
        }
        break;
    }

    case AST_TYPE_CLASS: {
        if (type->clsname) {
            info->base_name = aoStrPrintf("%s", type->clsname->data);
        }
        break;
    }

    case AST_TYPE_FUNC: {
        transpileTypeInternal(ctx, type->rettype, info);
        aoStr *params = transpileParamsList(type->params, ctx);
        info->params = params;
        break;
    }

    case AST_TYPE_AUTO:
        info->base_name = aoStrPrintf("auto");
        break;

    default:
        loggerPanic("Unknown type: %d\n", type->kind);
    }
}

aoStr *transpileVarDeclInfo(TranspileCtx *ctx, TypeInfo *info, char *name) {
    aoStr *str = aoStrNew();
    if (ctx->flags & TRANSPILE_FLAG_ISATTY) {
        aoStrCatFmt(str,ESC_WHITE"%S"ESC_RESET,info->base_name);
    } else {
        aoStrCatFmt(str, "%S",info->base_name);
    }
    strMapAdd(ctx->used_types,info->base_name->data,info->base_name);

    if (name || info->stars > 0) {
        aoStrPutChar(str,' ');
    }

    if (info->stars) {
        for (int i = 0; i < info->stars; ++i) {
            aoStrPutChar(str, '*');
        }
    }

    /* a function pointer */
    if (info->params) {
        if (name) {
            aoStrCatFmt(str, "(*%s)",name);
        }
        aoStrCatFmt(str, "(%S)", info->params);
    } else if (info->array_dimensions) {
        if (name) {
            aoStrCatFmt(str, "%s",name);
        }
        aoStrCatFmt(str, "[%i]",info->array_dimensions);
    } else if (info->array_init_label) {
        if (name) {
            aoStrCatFmt(str, "%s",name);
        }
        aoStrCatFmt(str, "[%S]",info->array_init_label);
    } else {
        if (name) {
            aoStrCatFmt(str, "%s",name);
        }
    }
    
    if (info->params) aoStrRelease(info->params);

    return str;
}


/* types cannot be parsed in isolation if you want a string */
aoStr *transpileVarDecl(TranspileCtx *ctx, AstType *type, char *name) {
    TypeInfo info;
    memset(&info,0,sizeof(TypeInfo));
    transpileTypeInternal(ctx,type,&info);
    return transpileVarDeclInfo(ctx,&info,name);
}

aoStr *transpileFunctionProto(TranspileCtx *ctx, AstType *type, char *name) {
    TypeInfo info;
    memset(&info,0,sizeof(TypeInfo));

    transpileTypeInternal(ctx,type,&info);
    aoStr *str = aoStrNew();
    if (ctx->flags & TRANSPILE_FLAG_ISATTY) {
        aoStrCatFmt(str,ESC_WHITE"%S"ESC_RESET,info.base_name);
    } else {
        aoStrCatFmt(str, "%S",info.base_name);
    }

    if (info.stars) {
        aoStrPutChar(str,' ');
        for (int i = 0; i < info.stars; ++i) {
            aoStrPutChar(str, '*');
        }
    }
    aoStrPutChar(str, '\n');

    /* a function pointer */
    if (info.params) {
        aoStrCatFmt(str, "%s",name);
        if (info.params->len > 0) {
            aoStrCatFmt(str, "(%S)", info.params);
        } else {
            if (ctx->flags & TRANSPILE_FLAG_ISATTY) {
                aoStrCatFmt(str, "("ESC_WHITE"void"ESC_RESET")");
            } else {
                aoStrCatFmt(str, "(void)");
            }
        }
    } else if (info.array_dimensions) {
        aoStrCatFmt(str, "%s",name);
        aoStrCatFmt(str, "[%i]",info.array_dimensions);
    } else {
        aoStrCatFmt(str, "%s",name);
    }
    
    if (info.base_name) aoStrRelease(info.base_name);
    if (info.params)    aoStrRelease(info.params);

    return str;
}

static void transpileFields(TranspileCtx *ctx,
                            StrMap *fields,
                            StrMap *seen,
                            char *clsname,
                            aoStr *buf,
                            ssize_t *indent)
{
    StrMapIterator *it = strMapIteratorNew(fields);
    StrMapNode *n = NULL;
    while ((n = strMapNext(it)) != NULL) {
        AstType *field = n->value;
        /* When transpiling we save the nested flattened class properties on the 
         * class AND in a separate struct, however when parsing we save the struct 
         * first so this check ensures we do not double up. It's not _really_ 
         * needed but makes the code symantically similar */
        if (strMapGetLen(seen,n->key,n->key_len) != NULL) continue;

        aoStrCatRepeat(buf, " ", *indent);
        if (!strncmp(n->key, str_lit("cls_label "))) {
            if (field->kind == AST_TYPE_UNION) {
                aoStrCatFmt(buf, "union {\n");
            } else if (field->kind == AST_TYPE_CLASS) {
                aoStrCatFmt(buf, "struct {\n");
            }
            *indent += 4;
            transpileFields(ctx,field->fields,seen,clsname,buf,indent);
            *indent -= 4;
            aoStrCatRepeat(buf, " ", *indent);
            aoStrCatLen(buf, str_lit("};\n"));
        } else {
            TypeInfo info;
            memset(&info,0,sizeof(TypeInfo));
            transpileTypeInternal(ctx,field,&info);
            aoStr *decl = transpileVarDeclInfo(ctx,&info,n->key);

            if (!strncmp(clsname,info.base_name->data,info.base_name->len)) {
                aoStrCatFmt(buf,"struct %S;\n", decl);
            } else {
                aoStrCatFmt(buf,"%S;\n", decl);
            }
            aoStrRelease(decl);
        }
        strMapAdd(seen, n->key, n->value);
    }
    strMapIteratorRelease(it);
}

void transpileClassDefinitions(Cctrl *cc, TranspileCtx *ctx, StrMap *built_in_types) {
    StrMapIterator *it = strMapIteratorNew(cc->clsdefs);
    StrMapNode *n = NULL;
    aoStr *buf = ctx->buf;
    ssize_t indent = 4;
    StrMap *seen = strMapNew(32);

    while ((n = strMapNext(it)) != NULL) {
        if (!transpileCtxShouldEmitType(ctx, n->key, n->key_len)) {
            continue;
        }
        
        if (strMapGetLen(built_in_types,n->key,n->key_len)) {
            if (!strMapGetLen(ctx->used_types, n->key,n->key_len)) {
                continue;
            }
        }

        AstType *cls = n->value;
        /* Line numbers on an AST would be great... */
        if (cls->kind != AST_TYPE_CLASS) {
            loggerPanic("Should not be here\n"); 
        }

        aoStrCatFmt(buf, "typedef struct %s {\n", n->key);
        transpileFields(ctx,cls->fields,seen,n->key,buf,&indent);
        aoStrCatFmt(buf, "} %s;\n\n", n->key);
    }
    strMapIteratorRelease(it);
    strMapRelease(seen);
}

void transpileUnionDefinitions(Cctrl *cc, TranspileCtx *ctx) {
    StrMapIterator *it = strMapIteratorNew(cc->uniondefs);
    StrMapNode *n = NULL;
    ssize_t indent = 4;
    aoStr *buf = ctx->buf;
    StrMap *seen = strMapNew(32);
    while ((n = strMapNext(it)) != NULL) {
        if (!transpileCtxShouldEmitType(ctx, n->key, n->key_len)) {
            continue;
        }

        AstType *cls = n->value;
        /* Line numbers on an AST would be great... */
        if (cls->kind != AST_TYPE_UNION) {
            loggerPanic("Should not be here\n"); 
        }

        aoStrCatFmt(buf, "typedef union %s {\n", n->key);
        transpileFields(ctx,cls->fields,seen,n->key,buf,&indent);
        aoStrCatFmt(buf, "} %s;\n\n", n->key);
    }
    strMapIteratorRelease(it);
    strMapRelease(seen);
}

void transpileDefines(Cctrl *cc, TranspileCtx *ctx) {
    aoStr *buf = ctx->buf;
    StrMapIterator *it = strMapIteratorNew(cc->macro_defs);
    StrMapNode *n = NULL;
    while ((n = strMapNext(it)) != NULL) {
        lexeme *tok = (lexeme *)n->value;
        if (!transpileCtxShouldEmitDefine(ctx, n->key, n->key_len)) {
            continue;
        }

        if (ctx->flags & TRANSPILE_FLAG_ISATTY) {
            aoStrCatFmt(buf,ESC_WHITE"#define %s "ESC_RESET,n->key);
            switch (tok->tk_type) {
                case TK_I64: aoStrCatFmt(buf, ESC_PURPLE"%I\n"ESC_RESET,tok->i64); break;
                case TK_F64: aoStrCatFmt(buf, ESC_PURPLE"%f\n"ESC_RESET,tok->f64); break;
                case TK_STR: aoStrCatFmt(buf, ESC_GREEN"\"%s\"\n"ESC_RESET,tok->start); break;
                default:     aoStrCatFmt(buf, "\n"); break;
            }
        } else {
            aoStrCatFmt(buf,"#define %s ",n->key);
            switch (tok->tk_type) {
                case TK_I64: aoStrCatFmt(buf, "%I\n",tok->i64); break;
                case TK_F64: aoStrCatFmt(buf, "%f\n",tok->f64); break;
                case TK_STR: aoStrCatFmt(buf, "\"%s\"\n",tok->start); break;
                default:     aoStrCatFmt(buf, "\n"); break;
            }
        }

    }
    strMapIteratorRelease(it);
}

aoStr *transpileFunction(Ast *fn, TranspileCtx *ctx) {
    aoStr *function = aoStrNew();
    aoStr *fn_proto = NULL;
    aoStr *fname = transpileFormatFunction(fn->fname);
    if (fname->len == 4 && !memcmp(fname->data,"main",4)) {
        AstType *c_main_type = astMakeFunctionType(ast_i32_type,
                fn->type->params);
        fn_proto = transpileFunctionProto(ctx, c_main_type,"main");
        free(c_main_type);
    } else {
        fn_proto = transpileFunctionProto(ctx, fn->type,fname->data);
    }

    /* Parse out the function body */
    aoStr *body = transpileAst(fn->body, ctx);

    /* Add inline modifier if the function is inline */
    if (fn->flags & AST_FLAG_INLINE) {
        aoStrCatFmt(function, "%s ",
                       transpileKeyWordHighlight(ctx,KW_INLINE));
    }

    aoStrCatFmt(function, "%S\n{\n%S}",fn_proto, body);
    return function;
}

/* Format the assembly to remove the tabs, leading whitespace and make calls 
 * to the c function wrappers rather than the original assmebly */
char *transpileFormatAsmLine(Cctrl *cc, char *line) {
    static char buffer[128];
    static char tmp_fname[128];
    char *tmp_fname_ptr;
    char *tmp_ptr = buffer;
    int first_space = 1;
    ssize_t len = 0;

    while (*line) {
        /* Swap out the assembly names for c function names */
        if (!strncmp(line,str_lit("syscall"))) {
            len = snprintf(buffer,sizeof(buffer),"syscall");
            tmp_ptr = buffer+len;
            break;
        } else if (!strncmp(line, str_lit("call"))) {
            while (!isspace(*line)) line++;
            while (isspace(*line)) line++;
            tmp_fname_ptr = tmp_fname;
            while (*line != '\0') *tmp_fname_ptr++ = *line++;
            *tmp_fname_ptr = '\0';

            /* If we have a HC function name use that rather than the raw 
             * assembly function name */
            Ast *maybe_asm_fn = strMapGet(cc->asm_functions, tmp_fname);
            if (maybe_asm_fn) {
                aoStr *name = astNormaliseFunctionName(maybe_asm_fn->fname->data);
                len = snprintf(buffer,sizeof(buffer),"call %s", name->data);
                aoStrRelease(name);
            } else {
                /* Otherwise we are probably not looking at a call to an 
                 * assembly routine */
                len = snprintf(buffer,sizeof(buffer),"call %s", tmp_fname);
            }
            /* make tmp_ptr point to the end of the buffer and break the loop */
            tmp_ptr = buffer+len;
            break;
        } else if (!isspace(*line)) {
            if (*line == '%') {
                /* It needs %% to be valid */
                *tmp_ptr++ = '%';
            }
            *tmp_ptr++ = *line;
            first_space = 1;
        } else if (first_space) {
            *tmp_ptr++ = ' ';
            first_space = 0;
        }

        line++;
    }
    *tmp_ptr = '\0';
    return buffer;
}

/* This is a best effort! */
aoStr *transpileAsmFunction(Cctrl *cc, Ast *asmfn, Ast *asm_stmt, TranspileCtx *ctx) {
    static char c_regs[] = {'D', 'S', 'd', 'c', 'b', 'a'};
    int is_void = asmfn->type->rettype->kind == AST_TYPE_VOID;
    AstType *retval_type = is_void ? ast_int_type : asmfn->type->rettype;

    aoStr *fn_proto = transpileFunctionProto(ctx, asmfn->type,asmfn->fname->data);
    aoStr *var_type = transpileVarDecl(ctx, retval_type, "retval");

    aoStr *assembly = asm_stmt->body->asm_stmt;

    int line_count = 0;
    aoStr **lines = aoStrSplit(assembly->data,'\n',&line_count);
    aoStr *c_asm_blk = aoStrNew();
    int indent = 4;

    /* Initial function preamble: 
     *
     * <type>
     * <ident(<args ...>)
     * {
     *     <type> retval;
     *     __asm__ volatile (\n
     * */
    aoStrCatFmt(c_asm_blk, "%S\n{\n", fn_proto);
    aoStrRepeatChar(c_asm_blk,' ', indent);
    aoStrCatFmt(c_asm_blk, "%S;\n", var_type);
    aoStrRepeatChar(c_asm_blk,' ', indent);
    aoStrCatFmt(c_asm_blk, "__asm__ %s (\n", transpileKeyWordHighlight(ctx,KW_VOLATILE));

    indent += 4;
    /* we need to remove `pushq %rbp\n movq %rsp, %rbp` as the function we are 
     * wrapping this in will do that */
    for (int i = 0; i < line_count; ++i) {
        aoStr *line = lines[i];
        char *ptr = line->data;
        while (isspace(*ptr)) {
            ptr++;
        }
        /* Check the first line for push<ch> %rbp */
        if (i == 0) {
            if (!strncmp(ptr,str_lit("push"))) {
                while (!isspace(*ptr)) ptr++;
                while (isspace(*ptr)) ptr++;
                if (!strncmp(ptr,str_lit("%rbp"))) continue;
            }
        } else if (i == 1) {
            /* Check the first line for mov<ch> %rsp, %rbp */
            if (!strncmp(ptr,str_lit("mov"))) {
                while (!isspace(*ptr)) ptr++;
                while (isspace(*ptr)) ptr++;
                if (!strncmp(ptr,str_lit("%rsp"))) {
                    while (*ptr != ',') ptr++;
                    ptr++;
                    while (isspace(*ptr)) ptr++;
                    if (!strncmp(ptr,str_lit("%rbp"))) continue;
                }
            }
        }
        if (!*ptr) continue;
        aoStrRepeatChar(c_asm_blk,' ', indent);
        aoStrCatFmt(c_asm_blk, "\"%s\\n\\t\"\n",transpileFormatAsmLine(cc, ptr));
    }

    /* Prepare the registers needed for the function */
    aoStrRepeatChar(c_asm_blk,' ', indent);
    aoStrCatFmt(c_asm_blk, ": \"=a\"(retval)\n");
    int params_count = asmfn->params->size;
    if (params_count) {
        aoStrRepeatChar(c_asm_blk,' ', indent);
        aoStrCat(c_asm_blk, ": ");

        for (int i = 0; i < params_count; i++) {
            Ast *ast_param = asmfn->params->entries[i];
            char reg = c_regs[i];
            char *param = astLValueToString(ast_param,0);
            aoStrCatFmt(c_asm_blk, "\"%c\"(%s)",reg,param);
            if (i + 1 != params_count) {
                aoStrCat(c_asm_blk, ", ");
            }
        }
        aoStrPutChar(c_asm_blk,'\n');
    }
    indent -= 4;
    aoStrRepeatChar(c_asm_blk,' ', indent);
    aoStrCatLen(c_asm_blk, str_lit(");"));
    if (!is_void) {
        aoStrPutChar(c_asm_blk, '\n');
        aoStrRepeatChar(c_asm_blk,' ', indent);
        aoStrCatFmt(c_asm_blk, "%s retval;", transpileKeyWordHighlight(ctx,KW_RETURN));
    }
    aoStrCat(c_asm_blk, "\n}");

    for (int i = 0; i < line_count; ++i) {
        aoStrRelease(lines[i]);
    }
    aoStrRelease(var_type);
    aoStrRelease(fn_proto);

    return c_asm_blk;
}

void transpileAstList(Cctrl *cc, TranspileCtx *ctx) {
    StrMap *seen = strMapNew(32);
    listForEach(cc->ast_list) {
        Ast *ast = it->value;
        if (ast->kind == AST_FUNC) {
            if (strMapGet(seen, ast->fname->data) != NULL) continue;
            aoStr *c_function = transpileFunction(ast, ctx);
            aoStrCatAoStr(ctx->buf,c_function);
            aoStrRepeatChar(ctx->buf, '\n', 2);
            aoStrRelease(c_function);
            strMapAdd(seen, ast->fname->data, ast);
        } else if (ast->kind == AST_ASM_FUNC_BIND) {
            Ast *asm_stmt = strMapGet(cc->asm_functions, ast->asmfname->data);
            if (asm_stmt) {
                if (strMapGet(seen, ast->asmfname->data) != NULL) continue;
                aoStr *asm_func = transpileAsmFunction(cc, ast, asm_stmt, ctx);
                aoStrCatAoStr(ctx->buf,asm_func);
                aoStrRepeatChar(ctx->buf, '\n', 2);
                aoStrRelease(asm_func);
                strMapAdd(seen, ast->asmfname->data, ast);
            }
        } else if (ast->kind == AST_COMMENT) {
            ssize_t indent = 0;
            transpileAstInternal(ast, ctx, &indent);
        }
    }
}

aoStr *transpileIncludes(TranspileCtx *ctx) {
    ssize_t len = static_size(transpile_used_c_headers);
    aoStr *buf = aoStrNew();
    for (ssize_t i = 0; i < len; ++i) {
        if (ctx->flags & TRANSPILE_FLAG_ISATTY) {
            aoStrCatFmt(buf,ESC_GREEN"#include"ESC_RESET);
            aoStrCatFmt(buf,ESC_RED" <%s>\n"ESC_RESET,transpile_used_c_headers[i]);
        } else {
            aoStrCatFmt(buf,"#include <%s>\n",transpile_used_c_headers[i]);
        }
    }
    return buf;
}

aoStr *transpileToC(Cctrl *cc, char *file_name) {
    TranspileCtx *ctx = transpileCtxNew(cc);
    transpileInitMaps();
    
    cc->flags |= (CCTRL_SAVE_ANONYMOUS|CCTRL_PASTE_DEFINES|CCTRL_PRESERVE_SIZEOF|CCTRL_TRANSPILING);
    StrMap *built_in_types = strMapNew(32);
    StrMap *clsdefs = cc->clsdefs;

    aoStr *builtin_path = aoStrDupRaw(str_lit("/usr/local/include/tos.HH")); 

    lexer *l = malloc(sizeof(lexer));
    l->lineno = 1;
    
    lexInit(l,NULL,(CCF_PRE_PROC));
    lexSetBuiltinRoot(l,"/usr/local/include/");
    lexPushFile(l,builtin_path);

    cctrlInitParse(cc,l);
    // we are parsing the built in types
    cc->clsdefs = built_in_types;
    parseToAst(cc);

    lexPushFile(l,aoStrDupRaw(file_name,strlen(file_name)));
    cctrlInitParse(cc,l);
    cc->clsdefs = clsdefs;
    strMapMerge(clsdefs, built_in_types);
    parseToAst(cc);
    lexerPoolRelease();
    lexReleaseAllFiles(l);
    listRelease(l->files,NULL);
    free(l);

    
    aoStr *include_buf = transpileIncludes(ctx);

    aoStr *ast_buf = aoStrNew();
    transpileCtxSetBuffer(ctx, ast_buf);
    transpileAstList(cc,ctx);

    aoStr *define_buf = aoStrNew();
    transpileCtxSetBuffer(ctx, define_buf);
    transpileDefines(cc, ctx);

    aoStr *class_buf = aoStrNew();
    transpileCtxSetBuffer(ctx, class_buf);
    transpileClassDefinitions(cc,ctx, built_in_types);

    aoStr *union_buf = aoStrNew();
    transpileCtxSetBuffer(ctx, union_buf);
    transpileUnionDefinitions(cc,ctx);

    aoStr *buffers[] = {
        include_buf,
        define_buf,
        class_buf,
        union_buf,
        ast_buf,
    };

    aoStr *code = aoStrAlloc(ast_buf->len + define_buf->len +
                             class_buf->len + union_buf->len);

    ssize_t len = static_size(buffers);
    for (ssize_t i = 0; i < len; ++i) {
        aoStr *buf = buffers[i];
        if (buf->len > 0) {
            aoStrCatAoStr(code,buf);
            aoStrPutChar(code, '\n');
            aoStrRelease(buf);
        }
    }

    return code;
}
