#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"

aoStr *aoStrAlloc(size_t capacity) {
    aoStr *buf = malloc(sizeof(aoStr));
    buf->capacity = capacity + 10;
    buf->len = 0;
    buf->data = malloc(sizeof(char) * buf->capacity);
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
int aoStrExtendBuffer(aoStr *buf, unsigned int additional) {
    size_t new_capacity = (buf->capacity*2) + additional;
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
    aoStrExtendBufferIfNeeded(buf, 100);
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
    size_t min = l1 < l2 ? l1 : l2;
    return memcmp(b1->data, b2->data, min);
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
    char *buf = (char *)malloc(sizeof(char) * bufferlen);

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
            bufferlen = ((size_t)len) + 1;
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

aoStr *aoStrEscapeString(aoStr *buf) {
    aoStr *outstr = aoStrAlloc(buf->capacity);
    char *ptr = buf->data;

    if (buf == NULL) {
        return NULL;
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
