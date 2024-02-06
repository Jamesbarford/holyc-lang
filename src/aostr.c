#include <ctype.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "util.h"

aoStr *aoStrAlloc(size_t capacity) {
    aoStr *buf = malloc(sizeof(aoStr));
    buf->capacity = capacity + 10;
    buf->len = 0;
    buf->offset = 0;
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

char *aoStrGetData(aoStr *buf) {
    return buf->data;
}

/* Get the underlying string and free the container 'aoStr' */
char *aoStrMove(aoStr *buf) {
    char *buffer = buf->data;
    free(buf);
    return buffer;
}

size_t aoStrGetOffset(aoStr *buf) {
    return buf->offset;
}

void aoStrSetOffset(aoStr *buf, size_t offset) {
    buf->offset = offset;
}

void aoStrSetLen(aoStr *buf, size_t len) {
    buf->len = len;
    buf->data[len] = '\0';
}

size_t aoStrLen(aoStr *buf) {
    return buf->len;
}

void aoStrSetCapacity(aoStr *buf, size_t capacity) {
    buf->capacity = capacity;
}

size_t aoStrCapacity(aoStr *buf) {
    return buf->capacity;
}

/* At is relative to the current internal offset */
char aoStrGetCharAt(aoStr *buf, size_t at) {
    if (buf->offset + at > buf->len) {
        return '\0';
    }

    return buf->data[buf->offset + at];
}

char aoStrGetChar(aoStr *buf) {
    if (buf->offset > buf->len) {
        return '\0';
    }

    return buf->data[buf->offset];
}

/* At is relative to the current internal offset */
int aoStrMatchCharAt(aoStr *buf, char ch, size_t at) {
    if (at + buf->offset > buf->len) {
        return 0;
    }
    return buf->data[buf->offset + at] == ch;
}

int aoStrMatchChar(aoStr *buf, char ch) {
    return buf->data[buf->offset] == ch;
}

int aoStrEndsWith(aoStr *buf, char *pattern, int patternlen) {
    if (patternlen > buf->len) {
        return 0;
    }
    for (size_t i = buf->len-1; patternlen; --i) {
        if (buf->data[i] != pattern[patternlen-1]) {
            return 0;
        }
        --patternlen;
    }
    return 1;
}

void aoStrAdvance(aoStr *buf) {
    buf->offset++;
}

void aoStrAdvanceBy(aoStr *buf, size_t by) {
    buf->offset += by;
}

void aoStrRewindBy(aoStr *buf, size_t by) {
    buf->offset -= by;
}

int aoStrWouldOverflow(aoStr *buf, size_t additional) {
    return buf->offset + additional > buf->len;
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

void aoStrToUpperCase(aoStr *buf) {
    for (size_t i = 0; i < buf->len; ++i) {
        buf->data[i] = toupper(buf->data[i]);
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

char aoStrUnPutChar(aoStr *buf) {
    if (buf->len == 0) {
        return '\0';
    }

    char ch = buf->data[buf->len - 2];
    buf->len--;
    buf->data[buf->len] = '\0';
    return ch;
}

/* DANGER: This assumes the both buffers at a minimum have `size` bytes in their
 * data */
int aoStrNCmp(aoStr *b1, aoStr *b2, size_t size) {
    return memcmp(b1->data, b2->data, size);
}

int aoStrNCaseCmp(aoStr *b1, aoStr *b2, size_t size) {
    return strncasecmp((char *)b1->data, (char *)b2->data, size);
}

int aoStrStrNCmp(aoStr *b1, char *s, size_t len) {
    if (b1->len != len) {
        return -1;
    }
    return strncmp((char *)b1->data, (char *)s, len);
}

int aoStrStrNCaseCmp(aoStr *b1, char *s, size_t len) {
    if (b1->len != len) {
        return -1;
    }
    return strncasecmp((char *)b1->data, (char *)s, len);
}

int aoStrCmp(aoStr *b1, aoStr *b2) {
    size_t l1 = b1->len;
    size_t l2 = b2->len;
    size_t min = l1 < l2 ? l1 : l2;
    return memcmp(b1->data, b2->data, min);
}

int aoStrCaseCmp(aoStr *b1, aoStr *b2) {
    size_t l1 = b1->len;
    size_t l2 = b2->len;
    size_t min = l1 < l2 ? l1 : l2;
    return strncasecmp((char *)b1->data, (char *)b2->data, min);
}

void aoStrSlice(aoStr *buf, size_t from, size_t to, size_t size) {
    memmove(buf->data + to, buf->data + from, size);
    buf->data[size] = '\0';
    buf->len = size;
}

aoStr *aoStrFromString(char *s, size_t len) {
    aoStr *str = (aoStr *)malloc(sizeof(aoStr));
    str->data = s;
    str->offset = 0;
    str->len = len;
    str->capacity = len;
    return str;
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

/* Set the length to none and the offset to none */
void aoStrReset(aoStr *buf) {
    buf->len = 0;
    buf->offset = 0;
    buf->data[buf->len] = '\0';
}

/* Create a string view */
aoStr *aoStrView(char *s, size_t len) {
    return aoStrDupRaw(s, len);
}

void aoStrViewRelease(aoStr *buf) {
    if (buf) {
        /* String views do not own their buffer */
        free(buf);
    }
}

/* We duplicate a lot of strings sometimes they are null */
aoStr *aoStrMaybeDup(aoStr *buf) {
    if (buf) {
        return aoStrDup(buf);
    }
    return NULL;
}

aoStr *aoStrDup(aoStr *buf) {
    aoStr *dupe = aoStrAlloc(buf->len);
    memcpy(dupe->data, buf->data, buf->len);
    aoStrSetLen(dupe, buf->len);
    aoStrSetCapacity(dupe, buf->len);
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
            loggerDebug("got here\n");
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

int *aoStrComputePrefixTable(char *pattern, size_t patternlen) {
    int *table = (int *)malloc(sizeof(int) * patternlen);
    table[0] = 0;
    int k = 0;

    for (size_t i = 1; i < patternlen; ++i) {
        while (k > 0 && pattern[k] != pattern[i]) {
            k = table[k - 1];
        }
        if (pattern[k] == pattern[i]) {
            k++;
        }
        table[i] = k;
    }
    return table;
}

int aoStrContainsPatternWithTable(aoStr *buf, int *table, char *pattern,
                                  size_t patternlen) {
    char *_buf = buf->data;
    int retval = -1;

    if (patternlen == 0) {
        return retval;
    }

    size_t q = 0;
    for (size_t i = 0; i < buf->len; ++i) {
        if (_buf[i] == '\0') {
            break;
        }
        while (q > 0 && pattern[q] != _buf[i]) {
            q = table[q - 1];
        }
        if (pattern[q] == _buf[i]) {
            q++;
        }

        if (q == patternlen) {
            retval = i - patternlen + 1;
            break;
        }
    }

    return retval;
}

/* Implementation of KMP Matcher algorithm from introduction to algorithms */
int aoStrContainsPattern(aoStr *buf, char *pattern, size_t patternlen) {
    int retval = -1;
    int *table = aoStrComputePrefixTable(pattern, patternlen);
    retval = aoStrContainsPatternWithTable(buf, table, pattern, patternlen);
    free(table);
    return retval;
}

int aoStrContainsCasePatternWithTable(aoStr *buf, int *table, char *pattern,
                                      size_t patternlen) {
    size_t q = 0;
    char *_buf = buf->data;
    int retval = -1;

    if (patternlen == 0) {
        return retval;
    }

    for (size_t i = 0; i < buf->len; ++i) {
        if (_buf[i] == '\0') {
            break;
        }
        while (q > 0 && tolower(pattern[q]) != tolower(_buf[i])) {
            q = table[q - 1];
        }
        if (tolower(pattern[q]) == tolower(_buf[i])) {
            q++;
        }

        if (q == patternlen) {
            retval = i - patternlen + 1;
            break;
        }
    }

    return retval;
}

int aoStrContainsCasePattern(aoStr *buf, char *pattern, size_t patternlen) {
    int retval = -1;
    int *table = aoStrComputePrefixTable(pattern, patternlen);
    retval = aoStrContainsCasePatternWithTable(buf, table, pattern, patternlen);
    free(table);
    return retval;
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

int aoStrIsMimeEncoded(aoStr *buf) {
    static const char *prefix = "=?utf-8?Q?";
    static const char *suffix = "?=";

    return strncmp(prefix, (char *)buf->data, 10) == 0 &&
            strncmp((char *)buf->data + (buf->len - 2), suffix, 2) == 0;
}

static int hexToInt(char c) {
    if (c >= '0' && c <= '9') {
        return c - '0';
    } else if (c >= 'A' && c <= 'F') {
        return c - 'A' + 10;
    } else if (c >= 'a' && c <= 'f') {
        return c - 'a' + 10;
    }
    return -1;
}

static void decodeMimeEncoded(char *str, char *out, size_t *outlen) {
    size_t len = 0;
    char *in = str;

    while (*in) {
        if (*in == '=') {
            int hi = hexToInt(*(in + 1));
            int lo = hexToInt(*(in + 2));

            if (hi != -1 && lo != -1) {
                out[len] = (hi << 4) | lo;
                in += 3;
            } else {
                out[len] = *in++;
            }
            len++;
        } else {
            out[len] = *in++;
            len++;
        }
    }
    *outlen = len;
}

/* Decodes the string inplace mutating the string passed in. Will set the new
 * length on the string */
void aoStrDecodeMimeEncodedInplace(aoStr *buf) {
    size_t outlen = 0;
    aoStrSlice(buf, 10, 0, buf->len - 10 - 2);
    decodeMimeEncoded(buf->data, buf->data, &outlen);
    aoStrSetLen(buf, outlen);
}

/* Allocates a new string for the decoded version */
aoStr *aoStrDecodeMimeEncoded(aoStr *buf) {
    int has_alloced = 0;
    size_t outlen = 0;
    size_t len = buf->len;
    aoStr *out = aoStrAlloc(512);
    char buffer[2048];
    char *tmp = NULL;

    if (len < sizeof(buffer)) {
        tmp = buffer;
    } else {
        has_alloced++;
        tmp = (char *)malloc(sizeof(char) * len);
    }

    memcpy(tmp, buf + 10, len - 10 - 2);
    tmp[len - 10 - 2] = '\0';

    decodeMimeEncoded(tmp, out->data, &outlen);
    aoStrSetLen(out, outlen);

    if (has_alloced) {
        free(tmp);
    }
    return out;
}

#ifdef AOSTR_TEST
int main(void) {
    aoStr *str = aoStrDupRaw("hello", 5, 10);
    printf("endsWith ello: %d\n", aoStrEndsWith(str, "ello", 4));
    aoStrRelease(str);
}
#endif
