#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "lexer.h"

aoStr *aoStrAlloc(size_t capacity) {
    aoStr *buf = malloc(sizeof(aoStr));
    capacity += 10;
    buf->capacity = capacity;
    buf->len = 0;
    buf->data = malloc(sizeof(char) * capacity);
    return buf;
}

aoStr *aoStrNew(void) {
    return aoStrAlloc(1 << 5);
}

void aoStrRelease(aoStr *buf) {
    if (buf) {
        free(buf->data);
        free(buf);
    }
}

/* Get the underlying string and free the container 'aoStr' */
char *aoStrMove(aoStr *buf) {
    char *buffer = buf->data;
    free(buf);
    return buffer;
}

/* Grow the capacity of the string buffer by `additional` space */
int aoStrExtendBuffer(aoStr *buf, size_t additional) {
    size_t new_capacity = (buf->capacity*2) + additional;
    assert(new_capacity > buf->capacity);
    if (new_capacity <= buf->capacity) {
        return -1;
    }

    char *tmp = (char *)realloc(buf->data, new_capacity);
    if (tmp == NULL) {
        return 0;
    }
    buf->data = tmp;
    buf->capacity = new_capacity;
    return 1;
}

/* Only extend the buffer if the additional space required would overspill the
 * current allocated capacity of the buffer */
static int aoStrExtendBufferIfNeeded(aoStr *buf, size_t additional) {
    if ((buf->len + additional + 1) >= buf->capacity) {
        return aoStrExtendBuffer(buf, additional+1);
    }
    return 0;
}

void aoStrToLowerCase(aoStr *buf) {
    for (size_t i = 0; i < buf->len; ++i) {
        buf->data[i] = tolower(buf->data[i]);
    }
}

void aoStrPutChar(aoStr *buf, char ch) {
    aoStrExtendBufferIfNeeded(buf, 10);
    buf->data[buf->len] = ch;
    buf->data[buf->len + 1] = '\0';
    buf->len++;
}

void aoStrRepeatChar(aoStr *buf, char ch, int times) {
    for (int i = 0; i < times; ++i) {
        aoStrPutChar(buf,ch);
    }
}

int aoStrCmp(aoStr *b1, aoStr *b2) {
    size_t l1 = b1->len;
    size_t l2 = b2->len;
    return l1==l2&&!memcmp(b1->data, b2->data, l1);
}

aoStr *aoStrDupRaw(char *s, size_t len) {
    size_t capacity = len+10;
    aoStr *dupe = aoStrAlloc(capacity);
    memcpy(dupe->data, s, len);
    dupe->len = len;
    dupe->data[len] = '\0';
    dupe->capacity = capacity;
    return dupe;
}

aoStr *aoStrDup(aoStr *buf) {
    aoStr *dupe = aoStrAlloc(buf->len);
    memcpy(dupe->data, buf->data, buf->len);
    dupe->len = buf->len;
    dupe->data[dupe->len] = '\0';
    dupe->capacity = buf->len;
    return dupe;
}

void aoStrCatLen(aoStr *buf, const void *d, size_t len) {
    aoStrExtendBufferIfNeeded(buf, len);
    memcpy(buf->data + buf->len, d, len);
    buf->len += len;
    buf->data[buf->len] = '\0';
}

void aoStrCatAoStr(aoStr *buf, aoStr *s2) {
    aoStrCatLen(buf, s2->data, s2->len);
}

void aoStrCat(aoStr *buf, const void *d) {
    size_t len = strlen(d);
    aoStrCatLen(buf, d, len);
}

void aoStrCatRepeat(aoStr *buf, char *str, int times) {
    int len = strlen(str);
    for (int i = 0; i < times; ++i) {
        aoStrCatLen(buf,str,len);
    }
}

void aoStrCatPrintf(aoStr *b, const char *fmt, ...) {
    va_list ap, copy;
    va_start(ap, fmt);

    /* Probably big enough */
    size_t fmt_len = strlen(fmt);
    size_t bufferlen = 1024;
    if (fmt_len > bufferlen) {
        bufferlen = fmt_len;
    }
    int len = 0;
    char *buf = (char *)malloc(sizeof(char) * bufferlen+1);

    while (1) {
        va_copy(copy, ap);
        len = vsnprintf(buf, bufferlen, fmt, copy);
        va_end(copy);

        if (len < 0) {
            free(buf);
            return;
        }

        if (((size_t)len) >= bufferlen) {
            free(buf);
            bufferlen = ((size_t)len) + 2;
            buf = malloc(bufferlen);
            if (buf == NULL) {
                return;
            }
            continue;
        }
        break;
    }

    aoStrCatLen(b, buf, len);
    free(buf);
    va_end(ap);
}

char *aoStrEncodeChar(long op) {
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

aoStr *aoStrEncode(aoStr *buf) {
    aoStr *outstr = aoStrAlloc(buf->capacity);
    char *ptr = buf->data;
    char *encoded;

    if (buf == NULL) return outstr;
    while (*ptr) {
        if (*ptr == '\\') {
            aoStrPutChar(outstr, '\\');
            switch (*ptr) {
                case '\'': aoStrPutChar(outstr,'\''); break;
                case '\\': aoStrPutChar(outstr, '\\'); break;
                case '\"': aoStrPutChar(outstr, '\"'); break;
                case '\b': aoStrPutChar(outstr, 'b'); break;
                case '\n': aoStrPutChar(outstr, 'n'); break;
                case '\t': aoStrPutChar(outstr, 't'); break;
                case '\v': aoStrPutChar(outstr, 'v'); break;
                case '\f': aoStrPutChar(outstr, 'f'); break;
                case '\r': aoStrPutChar(outstr, 'r'); break;
            }
        } else {
            encoded = aoStrEncodeChar(*ptr);
            aoStrCat(outstr,encoded);
        }
        ptr++;
    }
    return outstr;
}

aoStr *aoStrEscapeString(aoStr *buf) {
    aoStr *outstr = aoStrAlloc(buf->capacity);
    char *ptr = buf->data;

    if (buf == NULL) {
        return outstr;
    }

    while (*ptr) {
        if (*ptr > 31 && *ptr != '\"' && *ptr != '\\') {
            switch (*ptr) {
            case '$': aoStrCat(outstr,"\\$"); break;
            case '`': aoStrCat(outstr,"\\`"); break;
            default: aoStrPutChar(outstr, *ptr); break;
            }
        } else {
            aoStrPutChar(outstr, '\\');
            switch (*ptr) {
            case '\'': aoStrPutChar(outstr,'\''); break;
            case '\\': aoStrPutChar(outstr, '\\'); break;
            case '\"': aoStrPutChar(outstr, '\"'); break;
            case '\b': aoStrPutChar(outstr, 'b'); break;
            case '\n': aoStrPutChar(outstr, 'n'); break;
            case '\t': aoStrPutChar(outstr, 't'); break;
            case '\v': aoStrPutChar(outstr, 'v'); break;
            case '\f': aoStrPutChar(outstr, 'f'); break;
            case '\r': aoStrPutChar(outstr, 'r'); break;
            default:
                aoStrCatPrintf(outstr, "u%04x", (unsigned int)*ptr);
                break;
            }
        }
        ++ptr;
    }
    return outstr;
}

void aoStrArrayRelease(aoStr **arr, int count) {
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
aoStr **aoStrSplit(char *to_split, char delimiter, int *count) {
    aoStr **outArr, **tmp;
    char *ptr;
    int arrsize, memslot;
    long start, end;

    if (*to_split == delimiter) {
        to_split++;
    }

    memslot = 5;
    ptr = to_split;
    *count = 0;
    start = end = 0;
    arrsize = 0;

    if ((outArr = malloc(sizeof(aoStr *) * memslot)) == NULL)
        return NULL;

    while (*ptr != '\0') {
        if (arrsize + 1 >= memslot) {
            memslot *= 5;
            if ((tmp = (aoStr **)realloc(outArr, sizeof(aoStr) * memslot)) ==
                NULL)
                goto error;
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
    *count = arrsize;

    return outArr;

error:
    aoStrArrayRelease(outArr, arrsize);
    return NULL;
}

char *mprintVa(const char *fmt, va_list ap) {
    va_list copy;

    int allocated = 256;
    int len = 0;
    char *buffer = (char *)malloc(sizeof(char)*allocated+1);

    while (1) {
        va_copy(copy,ap);
        len = vsnprintf(buffer,allocated,fmt,copy);
        va_end(copy);

        if (len < 0) {
            free(buffer);
            return NULL;
        }

        if (len >= allocated) {
            free(buffer);
            allocated = len + 2;
            buffer = (char *)malloc(sizeof(char)*allocated);
            if (buffer == NULL) return NULL;
            continue;
        }
        break;
    }
    buffer[len] = '\0';
    return buffer;
}

/* Allocating printf */
char *mprintf(const char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    char *buf = mprintVa(fmt, ap);
    va_end(ap);
    return buf;
}

aoStr *aoStrError(void) {
    char *err = strerror(errno);
    aoStr *str = aoStrDupRaw(err,strlen(err));
    return str;
}
