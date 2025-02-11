#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "lexer.h"
#include "memory.h"

static aoStr *_aoStrAlloc(void) {
    return (aoStr *)globalArenaAllocate(sizeof(aoStr));
}

static char *aoStrBufferAlloc(size_t capacity) {
    return (char *)globalArenaAllocate((unsigned int)capacity);
}

aoStr *aoStrAlloc(size_t capacity) {
    aoStr *buf = _aoStrAlloc();
    capacity += 10;
    buf->capacity = capacity;
    buf->len = 0;
    buf->data = aoStrBufferAlloc(sizeof(char) * capacity);
    return buf;
}

aoStr *aoStrNew(void) {
    return aoStrAlloc(1 << 5);
}

void aoStrRelease(const aoStr *buf) {
}

/* Get the underlying string, we do not free the `aoStr`... it will get
 * collected later. This means we don't need to manually keep track of this
 * buffer */
char *aoStrMove(const aoStr *buf) {
    char *buffer = buf->data;
    return buffer;
}

/* Grow the capacity of the string buffer by `additional` space */
int aoStrExtendBuffer(aoStr *buf, const size_t additional) {
    const size_t new_capacity = (buf->capacity * 2) + additional;
    assert(new_capacity > buf->capacity);
    if (new_capacity <= buf->capacity) {
        return -1;
    }

    char *tmp = aoStrBufferAlloc(new_capacity);
    if (tmp == NULL) {
        return 0;
    }
    memcpy(tmp, buf->data, buf->capacity);
    buf->data = tmp;
    buf->capacity = new_capacity;
    return 1;
}

/* Only extend the buffer if the additional space required would overspill the
 * current allocated capacity of the buffer */
static int aoStrExtendBufferIfNeeded(aoStr *buf, const size_t additional) {
    if ((buf->len + additional + 1) >= buf->capacity) {
        return aoStrExtendBuffer(buf, additional + 1);
    }
    return 0;
}

void aoStrToLowerCase(aoStr *buf) {
    for (size_t i = 0; i < buf->len; ++i) {
        buf->data[i] = tolower(buf->data[i]);
    }
}

void aoStrPutChar(aoStr *buf, const char ch) {
    aoStrExtendBufferIfNeeded(buf, 10);
    buf->data[buf->len++] = ch;
    buf->data[buf->len] = '\0';
}

void aoStrRepeatChar(aoStr *buf, const char ch, const int times) {
    for (int i = 0; i < times; ++i) {
        aoStrPutChar(buf, ch);
    }
}

int aoStrCmp(const aoStr *b1, const aoStr *b2) {
    const size_t l1 = b1->len;
    const size_t l2 = b2->len;
    return l1 == l2 && !memcmp(b1->data, b2->data, l1);
}

aoStr *aoStrDupRaw(const char *s, const size_t len) {
    const size_t capacity = len + 10;
    aoStr *dupe = aoStrAlloc(capacity);
    memcpy(dupe->data, s, len);
    dupe->len = len;
    dupe->data[len] = '\0';
    dupe->capacity = capacity;
    return dupe;
}

aoStr *aoStrDup(const aoStr *buf) {
    aoStr *dupe = aoStrAlloc(buf->len);
    memcpy(dupe->data, buf->data, buf->len);
    dupe->len = buf->len;
    dupe->data[dupe->len] = '\0';
    dupe->capacity = buf->len;
    return dupe;
}

void aoStrCatLen(aoStr *buf, const void *d, const size_t len) {
    aoStrExtendBufferIfNeeded(buf, len);
    memcpy(buf->data + buf->len, d, len);
    buf->len += len;
    buf->data[buf->len] = '\0';
}

void aoStrCatAoStr(aoStr *buf, const aoStr *s2) {
    aoStrCatLen(buf, s2->data, s2->len);
}

void aoStrCat(aoStr *buf, const void *d) {
    size_t len = strlen(d);
    aoStrCatLen(buf, d, len);
}

void aoStrCatRepeat(aoStr *buf, const char *str, const int times) {
    const int len = strlen(str);
    for (int i = 0; i < times; ++i) {
        aoStrCatLen(buf, str, len);
    }
}

char *aoStrEncodeChar(const long op) {
    switch (op) {
    case '\\':
        return "\\";
    case '\n':
        return "\\n";
    case '\t':
        return "\\t";
    case '\r':
        return "\\r";
    case '\"':
        return "&#34;";
    case '\'':
        return "&#39;";
    case ' ':
        return "&#32;";
    case '>':
        return "&#62;";
    case '<':
        return "&#60;";
    case '&':
        return "&#38;";
    case '|':
        return "&#124;";
    case ';':
        return "&#59;";
    case '@':
        return "&#64;";
    case '=':
        return "&#61;";
    case '(':
        return "&#40;";
    case ')':
        return "&#41;";
    case '~':
        return "&#126;";
    case '^':
        return "&#94;";
    case '_':
        return "&#65;";
    case '!':
        return "&#33;";
    case '{':
        return "&#123;";
    case '}':
        return "&#125;";
    case '[':
        return "&#91;";
    case ']':
        return "&#93;";
    case '`':
        return "&#96;";
    case '*':
        return "&#42;";
    case '+':
        return "&#43;";
    case '-':
        return "&#45;";
    case '/':
        return "&#47;";
    case '%':
        return "&#37;";
    case '$':
        return "&#36;";
    case '#':
        return "&#35;";
    case ',':
        return "&#44;";
    case '.':
        return "&#46;";
    case '?':
        return "&#63;";
    case ':':
        return "&#48;";

    case TK_EQU_EQU:
        return "&#61;&#61;";
    case TK_NOT_EQU:
        return "&#33;&#61;";
    case TK_LESS_EQU:
        return "&#60;&#61;";
    case TK_GREATER_EQU:
        return "&#62;&#61;";
    case TK_AND_AND:
        return "&#38;&#38;";
    case TK_OR_OR:
        return "&#124;&#124;";
    case TK_SHL:
        return "&#60;&#60;";
    case TK_SHL_EQU:
        return "&#60;&#60;&#61;";
    case TK_SHR:
        return "&#62;&#62;";
    case TK_SHR_EQU:
        return "&#62;&#62;&#61;";
    case TK_MUL_EQU:
        return "&#42;&#61;";
    case TK_DIV_EQU:
        return "&#47;&#61;";
    case TK_OR_EQU:
        return "&#124;&#61;";
    case TK_XOR_EQU:
        return "&#94;&#61;";
    case TK_AND_EQU:
        return "&#38;&#61;";
    case TK_SUB_EQU:
        return "&#45;&#61;";
    case TK_ADD_EQU:
        return "&#43;&#61;";
    case TK_MOD_EQU:
        return "&#37;&#61;";
    case TK_ELLIPSIS:
        return "&#48;&#48;&#48;";
    case TK_ARROW:
        return "&#45;&#62;";
    case TK_PRE_PLUS_PLUS:
    case TK_PLUS_PLUS:
        return "&#43;&#43;";
    case TK_PRE_MINUS_MINUS:
    case TK_MINUS_MINUS:
        return "&#45;&#45;";
    default: {
        static char buf[4];
        const int len = snprintf(buf, sizeof(buf), "%c", (char)op);
        buf[len] = '\0';
        return buf;
    }
    }
}

aoStr *aoStrEncode(const aoStr *buf) {
    aoStr *out = aoStrAlloc(buf->capacity);
    const char *ptr = buf->data;

    while (*ptr) {
        if (*ptr == '\\') {
            aoStrPutChar(out, '\\');
            switch (*ptr) {
            case '\'':
                aoStrPutChar(out, '\'');
                break;
            case '\\':
                aoStrPutChar(out, '\\');
                break;
            case '\"':
                aoStrPutChar(out, '\"');
                break;
            case '\b':
                aoStrPutChar(out, 'b');
                break;
            case '\n':
                aoStrPutChar(out, 'n');
                break;
            case '\t':
                aoStrPutChar(out, 't');
                break;
            case '\v':
                aoStrPutChar(out, 'v');
                break;
            case '\f':
                aoStrPutChar(out, 'f');
                break;
            case '\r':
                aoStrPutChar(out, 'r');
                break;
            default:;
            }
        } else {
            const char *encoded = aoStrEncodeChar(*ptr);
            aoStrCat(out, encoded);
        }
        ptr++;
    }
    return out;
}

aoStr *aoStrEscapeString(const aoStr *buf) {
    if (buf == NULL)
        return NULL;

    aoStr *out = aoStrAlloc(buf->capacity);
    const char *ptr = buf->data;

    while (*ptr) {
        if (*ptr > 31 && *ptr != '\"' && *ptr != '\\') {
            switch (*ptr) {
            case '$':
                aoStrCat(out, "\\$");
                break;
            case '`':
                aoStrCat(out, "\\`");
                break;
            default:
                aoStrPutChar(out, *ptr);
                break;
            }
        } else {
            aoStrPutChar(out, '\\');
            switch (*ptr) {
            case '\'':
                aoStrPutChar(out, '\'');
                break;
            case '\\':
                aoStrPutChar(out, '\\');
                break;
            case '\"':
                aoStrPutChar(out, '\"');
                break;
            case '\b':
                aoStrPutChar(out, 'b');
                break;
            case '\n':
                aoStrPutChar(out, 'n');
                break;
            case '\t':
                aoStrPutChar(out, 't');
                break;
            case '\v':
                aoStrPutChar(out, 'v');
                break;
            case '\f':
                aoStrPutChar(out, 'f');
                break;
            case '\r':
                aoStrPutChar(out, 'r');
                break;
            default:
                aoStrCatPrintf(out, "u%04x", (unsigned int)*ptr);
                break;
            }
        }
        ++ptr;
    }
    return out;
}

void aoStrArrayRelease(aoStr **arr, const int count) {
    if (arr) {
        for (int i = 0; i < count; ++i) {
            aoStrRelease(arr[i]);
        }
        free(arr);
    }
}

/**
 * Split into strings on delimiter
 */
aoStr **aoStrSplit(const char *to_split, const char delimiter, int *_count) {
    aoStr **outArr;
    long end;
    const char *ptr = to_split;

    if (*ptr == delimiter) {
        ptr++;
    }

    int memslot = 5;
    long start = end = 0;
    int arrsize = 0;

    if ((outArr = (aoStr **)malloc(sizeof(aoStr *) * memslot)) == NULL)
        return NULL;

    while (*ptr != '\0') {
        if (arrsize + 1 >= memslot) {
            memslot *= 5;
            aoStr **tmp = (aoStr **)realloc(outArr, sizeof(aoStr) * memslot);
            if (tmp == NULL) {
                goto error;
            }
            outArr = tmp;
        }

        if (*ptr == delimiter) {
            outArr[arrsize] = aoStrDupRaw(to_split + start, end - start);
            ptr++;
            arrsize++;
            start = end + 1;
            end++;
            continue;
        }

        end++;
        ptr++;
    }

    outArr[arrsize] = aoStrDupRaw(to_split + start, end - start);
    arrsize++;
    *_count = arrsize;

    return outArr;

error:
    aoStrArrayRelease(outArr, arrsize);
    *_count = 0;
    return NULL;
}

static char *mprintVaImpl(const char *fmt, va_list ap, size_t *_len,
                          size_t *_allocated) {
    va_list copy;

    /* Probably big enough */
    const size_t fmt_len = strlen(fmt);
    size_t bufferlen = 1024;
    if (fmt_len > bufferlen) {
        bufferlen = fmt_len;
    }
    int len = 0;
    char *buf = aoStrBufferAlloc(sizeof(char) * bufferlen + 1);

    while (1) {
        va_copy(copy, ap);
        len = vsnprintf(buf, bufferlen, fmt, copy);
        va_end(copy);

        if (len < 0) {
            return NULL;
        }

        if (((size_t)len) >= bufferlen) {
            bufferlen = ((size_t)len) + 2;
            buf = aoStrBufferAlloc(bufferlen);
            if (buf == NULL) {
                return NULL;
            }
            continue;
        }
        break;
    }

    if (_len)
        *_len = len;
    if (_allocated)
        *_allocated = bufferlen;
    buf[len] = '\0';
    return buf;
}

char *mprintVa(const char *fmt, va_list ap, ssize_t *_len) {
    return mprintVaImpl(fmt, ap, (size_t *)_len, NULL);
}

static aoStr *aoStrPrintfVa(const char *fmt, va_list ap) {
    size_t len = 0;
    size_t capacity = 0;
    char *new_buf = mprintVaImpl(fmt, ap, &len, &capacity);
    aoStr *buffer = _aoStrAlloc();
    buffer->data = new_buf;
    buffer->len = len;
    buffer->capacity = capacity;
    return buffer;
}

/* Allocating printf */
char *mprintf(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    /* This is so we can use the pool allocator, and not have to deal with
     * freeing aribitary strings */
    const aoStr *buffer = aoStrPrintfVa(fmt, ap);
    va_end(ap);
    return aoStrMove(buffer);
}

void aoStrCatPrintf(aoStr *b, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);

    const aoStr *new_str = aoStrPrintfVa(fmt, ap);
    aoStrCatAoStr(b, new_str);
    aoStrRelease(new_str);
    va_end(ap);
}

/* For the happy path of strings this is very fast */
void aoStrCatFmt(aoStr *buf, const char *fmt, ...) {
    va_list ap;
    const char *ptr = fmt;
    va_start(ap, fmt);

    while (*ptr) {
        switch (*ptr) {
        case '%': {
            ptr++;
            if (*ptr == '\0') {
                va_end(ap);
                return;
            }
            switch (*ptr) {
            case 's': {
                const char *str = va_arg(ap, char *);
                aoStrCat(buf, str);
                break;
            }

            case 'S': {
                const aoStr *str = va_arg(ap, aoStr *);
                aoStrCatLen(buf, str->data, str->len);
                break;
            }

            case '.': {
                if (*(ptr + 1) == '*' && *(ptr + 2) == 's') {
                    ssize_t len = va_arg(ap, ssize_t);
                    const char *str = va_arg(ap, char *);
                    aoStrCatLen(buf, str, len);
                    ptr += 2;
                } else {
                    aoStrPutChar(buf, '.');
                }
                break;
            }

            case 'U': {
                size_t uint_ = va_arg(ap, size_t);
                aoStrCatPrintf(buf, "%zu", uint_);
                break;
            }

            case 'u': {
                unsigned int uint_ = va_arg(ap, unsigned int);
                aoStrCatPrintf(buf, "%u", uint_);
                break;
            }

            case 'I': {
                ssize_t int_ = va_arg(ap, ssize_t);
                aoStrCatPrintf(buf, "%lld", int_);
                break;
            }

            case 'i': {
                int int_ = va_arg(ap, int);
                aoStrCatPrintf(buf, "%d", int_);
                break;
            }

            case 'f': {
                double float_ = va_arg(ap, double);
                aoStrCatPrintf(buf, "%g", float_);
                break;
            }

            case 'c': {
                char ch = (char)va_arg(ap, int);
                aoStrPutChar(buf, ch);
                break;
            }

            case 'A': {
                const Ast *ast = va_arg(ap, Ast *);
                const aoStr *ast_str = astLValueToAoStr(ast, 0);
                aoStrCatAoStr(buf, ast_str);
                break;
            }
            default: {
                /* Put the character, probably a %% */
                aoStrPutChar(buf, *ptr);
                break;
            }
            }
            break;
        }
        default: {
            aoStrPutChar(buf, *ptr);
            break;
        }
        }
        ptr++;
    }
}

aoStr *aoStrPrintf(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    aoStr *buffer = aoStrPrintfVa(fmt, ap);
    va_end(ap);
    return buffer;
}

aoStr *aoStrError(void) {
    const char *err = strerror(errno);
    aoStr *str = aoStrDupRaw(err, strlen(err));
    return str;
}

aoStr *aoStrIntToHumanReadableBytes(long bytes) {
    aoStr *str = aoStrAlloc(32);
    double d;

    if (bytes < 1024) {
        aoStrCatFmt(str, "%uB", bytes);
    } else if (bytes < (1024 * 1024)) {
        d = (double)bytes / (1024);
        aoStrCatPrintf(str, "%.2fK", d);
    } else if (bytes < (1024LL * 1024 * 1024)) {
        d = (double)bytes / (1024 * 1024);
        aoStrCatPrintf(str, "%.2fM", d);
    } else if ((long long)bytes < (1024LL * 1024 * 1024 * 1024)) {
        d = (double)bytes / (1024LL * 1024 * 1024);
        aoStrCatPrintf(str, "%.2fG", d);
    }
    return str;
}
