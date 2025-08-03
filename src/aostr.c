#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ast.h"
#include "aostr.h"
#include "lexer.h"
#include "memory.h"

static AoStr *_aoStrAlloc(void) {
    return (AoStr *)globalArenaAllocate(sizeof(AoStr));
}

static char *aoStrBufferAlloc(u64 capacity) {
    return (char *)globalArenaAllocate((unsigned int)capacity);
}

AoStr *aoStrAlloc(u64 capacity) {
    AoStr *buf = _aoStrAlloc();
    capacity += 10;
    buf->capacity = capacity;
    buf->len = 0;
    buf->data = aoStrBufferAlloc(sizeof(char) * capacity);
    return buf;
}

AoStr *aoStrNew(void) {
    return aoStrAlloc(1 << 5);
}

void aoStrRelease(AoStr *buf) {
    (void)buf;
    return;
}

/* Get the underlying string, we do not free the `aoStr`... it will get 
 * collected later. This means we don't need to manually keep track of this 
 * buffer */
char *aoStrMove(AoStr *buf) {
    char *buffer = buf->data;
    return buffer;
}

/* Grow the capacity of the string buffer by `additional` space */
int aoStrExtendBuffer(AoStr *buf, u64 additional) {
    u64 new_capacity = (buf->capacity*2) + additional;
    assert(new_capacity > buf->capacity);
    if (new_capacity <= buf->capacity) {
        return -1;
    }

    char *tmp = aoStrBufferAlloc(new_capacity);
    if (tmp == NULL) {
        return 0;
    }
    memcpy(tmp,buf->data,buf->capacity);
    buf->data = tmp;
    buf->capacity = new_capacity;
    return 1;
}

/* Only extend the buffer if the additional space required would overspill the
 * current allocated capacity of the buffer */
static int aoStrExtendBufferIfNeeded(AoStr *buf, u64 additional) {
    if ((buf->len + additional + 1) >= buf->capacity) {
        return aoStrExtendBuffer(buf, additional+1);
    }
    return 0;
}

void aoStrToLowerCase(AoStr *buf) {
    for (u64 i = 0; i < buf->len; ++i) {
        buf->data[i] = tolower(buf->data[i]);
    }
}

void aoStrToUpperCase(AoStr *buf) {
    for (u64 i = 0; i < buf->len; ++i) {
        buf->data[i] = toupper(buf->data[i]);
    }
}

void aoStrPutChar(AoStr *buf, char ch) {
    aoStrExtendBufferIfNeeded(buf, 10);
    buf->data[buf->len++] = ch;
    buf->data[buf->len] = '\0';
}

void aoStrRepeatChar(AoStr *buf, char ch, int times) {
    for (int i = 0; i < times; ++i) {
        aoStrPutChar(buf,ch);
    }
}

int aoStrCmp(AoStr *b1, AoStr *b2) {
    u64 l1 = b1->len;
    u64 l2 = b2->len;
    return l1==l2&&!memcmp(b1->data, b2->data, l1);
}

AoStr *aoStrDupRaw(char *s, u64 len) {
    u64 capacity = len+10;
    AoStr *dupe = aoStrAlloc(capacity);
    memcpy(dupe->data, s, len);
    dupe->len = len;
    dupe->data[len] = '\0';
    dupe->capacity = capacity;
    return dupe;
}

AoStr *aoStrDup(AoStr *buf) {
    AoStr *dupe = aoStrAlloc(buf->len);
    memcpy(dupe->data, buf->data, buf->len);
    dupe->len = buf->len;
    dupe->data[dupe->len] = '\0';
    dupe->capacity = buf->len;
    return dupe;
}

void aoStrCatLen(AoStr *buf, const void *d, u64 len) {
    aoStrExtendBufferIfNeeded(buf, len);
    memcpy(buf->data + buf->len, d, len);
    buf->len += len;
    buf->data[buf->len] = '\0';
}

void aoStrCatAoStr(AoStr *buf, AoStr *s2) {
    aoStrCatLen(buf, s2->data, s2->len);
}

void aoStrCat(AoStr *buf, const void *d) {
    u64 len = strlen(d);
    aoStrCatLen(buf, d, len);
}

void aoStrCatRepeat(AoStr *buf, char *str, int times) {
    int len = strlen(str);
    for (int i = 0; i < times; ++i) {
        aoStrCatLen(buf,str,len);
    }
}

char *aoStrEncodeChar(s64 op) {
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

AoStr *aoStrEncode(AoStr *buf) {
    AoStr *outstr = aoStrAlloc(buf->capacity);
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

AoStr *aoStrEscapeString(AoStr *buf) {
    AoStr *outstr = aoStrAlloc(buf->capacity);
    char *ptr = buf->data;

    if (buf == NULL) {
        return outstr;
    }

    while (*ptr) {
        if (*ptr > 31 && *ptr != '\"' && *ptr != '\\') {
            switch (*ptr) {
            case '$': aoStrCat(outstr,"\\$"); break;
            case '`': aoStrCat(outstr,"\\`"); break;
            default:  aoStrPutChar(outstr, *ptr); break;
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

void aoStrArrayRelease(AoStr **arr, int count) {
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
AoStr **aoStrSplit(char *to_split, char delimiter, int *_count) {
    AoStr **outArr;
    s64 start, end;
    char *ptr = to_split;

    if (*ptr == delimiter) {
        ptr++;
    }

    int memslot = 5;
    start = end = 0;
    int arrsize = 0;

    if ((outArr = (AoStr **)malloc(sizeof(AoStr *) * memslot)) == NULL)
        return NULL;

    while (*ptr != '\0') {
        if (arrsize + 1 >= memslot) {
            memslot *= 5;
            AoStr **tmp = (AoStr **)realloc(outArr, sizeof(AoStr) * memslot);
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

static char *mprintVaImpl(const char *fmt, va_list ap, u64 *_len, u64 *_allocated) {
    va_list copy;

    /* Probably big enough */
    u64 fmt_len = strlen(fmt);
    u64 bufferlen = 1024;
    if (fmt_len > bufferlen) {
        bufferlen = fmt_len;
    }
    int len = 0;
    char *buf = aoStrBufferAlloc(sizeof(char) * bufferlen+1);

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

    if (_len) *_len = len;
    if (_allocated) *_allocated = bufferlen;
    buf[len] = '\0';
    return buf;
}

char *mprintVa(const char *fmt, va_list ap, s64 *_len) {
    return mprintVaImpl(fmt,ap,(u64 *)_len,NULL);
}

static AoStr *aoStrPrintfVa(const char *fmt, va_list ap) {
    u64 len = 0;
    u64 capacity = 0;
    char *new_buf = mprintVaImpl(fmt,ap,&len,&capacity);
    AoStr *buffer = _aoStrAlloc();
    buffer->data = new_buf;
    buffer->len = len;
    buffer->capacity = capacity;
    return buffer;
}

/* Allocating printf */
char *mprintf(const char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    /* This is so we can use the pool allocator, and not have to deal with 
     * freeing aribitary strings */
    AoStr *buffer = aoStrPrintfVa(fmt, ap);
    va_end(ap);
    return aoStrMove(buffer);
}

void aoStrCatPrintf(AoStr *b, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);

    AoStr *new_str = aoStrPrintfVa(fmt, ap);
    aoStrCatAoStr(b,new_str);
    aoStrRelease(new_str);
    va_end(ap);
}

char *mprintFmtVa(const char *fmt, va_list ap, u64 *_len, u64 *_allocated) {
    AoStr _buf = { .data = aoStrBufferAlloc(256), .len = 0, .capacity = 256 };
    AoStr *buf = &_buf;
    const char *ptr = fmt;

    while (*ptr) {
        switch (*ptr) {
            case '%': {
                ptr++;
                if (*ptr == '\0') {
                    goto done;
                }
                switch (*ptr) {
                    case 's': {
                        char *str = va_arg(ap,char*);
                        aoStrCat(buf, str);
                        break;
                    }

                    case 'S': {
                        AoStr *str = va_arg(ap, AoStr *);
                        aoStrCatLen(buf, str->data, str->len);
                        break;
                    }

                    case '.': {
                        if (*(ptr+1) == '*' && *(ptr+2) == 's') {
                            s64 len = va_arg(ap,ssize_t);
                            char *str = va_arg(ap,char*);
                            aoStrCatLen(buf,str,len);
                            ptr += 2;
                        } else {
                            aoStrPutChar(buf,'.');
                        }
                        break;
                    }

                    case 'U': {
                        u64 uint_ = va_arg(ap, size_t);
                        aoStrCatPrintf(buf,"%zu",uint_);
                        break;
                    }

                    case 'u': {
                        u32 uint_ = va_arg(ap, unsigned int);
                        aoStrCatPrintf(buf,"%u",uint_);
                        break;
                    }

                    case 'I': {
                        s64 int_ = va_arg(ap, ssize_t);
                        aoStrCatPrintf(buf,"%lld",int_);
                        break;
                    }

                    case 'i': {
                        int int_ = va_arg(ap, int);
                        aoStrCatPrintf(buf,"%d",int_);
                        break;
                    }

                    case 'f': {
                        double float_ = va_arg(ap, double);
                        aoStrCatPrintf(buf,"%g",float_);
                        break;
                    }

                    case 'c': {
                        char ch = (char)va_arg(ap, int);
                        aoStrPutChar(buf,ch);
                        break;
                    }

                    case 'A': {
                        Ast *ast = va_arg(ap, Ast*);
                        AoStr *ast_str = astLValueToAoStr(ast,0);
                        aoStrCatAoStr(buf,ast_str);
                        break;
                    }
                    
                    default: {
                        /* Put the character, probably a %% */
                        aoStrPutChar(buf,*ptr);
                        break;
                    }
                }
                break;
            }
            default: {
                aoStrPutChar(buf,*ptr);
                break;
            }
        }
        ptr++;
    }

done:
    if (_len)       *_len = buf->len;
    if (_allocated) *_allocated = buf->capacity;
    return buf->data;
}

/* For the happy path of strings this is very fast */
void aoStrCatFmt(AoStr *buf, const char *fmt, ...) {
    va_list ap;
    va_start(ap,fmt);
    u64 buffer_len = 0;
    u64 buffer_capacity = 0;
    char *buffer = mprintFmtVa(fmt,ap,&buffer_len,&buffer_capacity);
    aoStrCatLen(buf,buffer,buffer_len);
    va_end(ap);
}

char *mprintFmt(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    char *buffer = mprintFmtVa(fmt,ap,NULL,NULL);
    va_end(ap);
    return buffer;
}

AoStr *aoStrPrintf(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    AoStr *buffer = aoStrPrintfVa(fmt,ap);
    va_end(ap);
    return buffer;
}

AoStr *aoStrError(void) {
    char *err = strerror(errno);
    AoStr *str = aoStrDupRaw(err,strlen(err));
    return str;
}

AoStr *aoStrIntToHumanReadableBytes(s64 bytes) {
    AoStr *str = aoStrAlloc(32);
    double d;

    if (bytes < 1024) {
        aoStrCatFmt(str, "%uB", bytes);
    } else if (bytes < (1024*1024)) {
        d = (f64)bytes/(1024);
        aoStrCatPrintf(str, "%.2fK", d);
    } else if (bytes < (1024LL*1024*1024)) {
        d = (f64)bytes/(1024*1024);
        aoStrCatPrintf(str, "%.2fM", d);
    } else if ((u64)bytes < (1024LL*1024*1024*1024)) {
        d = (f64)bytes/(1024LL*1024*1024);
        aoStrCatPrintf(str, "%.2fG", d);
    }
    return str;
}

/* Murmur hash function modified to return a `unsigned long` */
static u64 murmur(const char *key, u64 len, u64 seed) {
    const u8 *data = (const u8*)key;
    const int nblocks = len / 8;

    u64 h1 = seed;
    u64 h2 = seed;

    const u64 c1 = 0x87c37b91114253d5ULL;
    const u64 c2 = 0x4cf5ad432745937fULL;

#define ROT(x,n1,n2) ((x << n1) | (x >> n2))

    // Body
    const u64 *blocks = (const u64 *)(data);

    for (int i = 0; i < nblocks; i++) {
        u64 k1 = blocks[i];

        k1 *= c1; 
        k1 = ROT(k1, 31 ,33);
        k1 *= c2; 

        h1 ^= k1;
        h1 = ROT(h1, 27, 37);
        h1 += h2;
        h1 = h1*5 + 0x52dce729;

        h2 ^= k1;
        h2 = ROT(h2, 41, 23);
        h2 += h1;
        h2 = h2*5 + 0x38495ab5;
    }

    // Tail
    const u8 *tail = (const u8*)(data + nblocks*8);
    u64 k1 = 0;

    switch (len & 7) {
    case 7: k1 ^= ((unsigned long)tail[6]) << 48; /* FALLTHROUGH */
    case 6: k1 ^= ((unsigned long)tail[5]) << 40; /* FALLTHROUGH */
    case 5: k1 ^= ((unsigned long)tail[4]) << 32; /* FALLTHROUGH */
    case 4: k1 ^= ((unsigned long)tail[3]) << 24; /* FALLTHROUGH */
    case 3: k1 ^= ((unsigned long)tail[2]) << 16; /* FALLTHROUGH */
    case 2: k1 ^= ((unsigned long)tail[1]) << 8; /* FALLTHROUGH */
    case 1: k1 ^= ((unsigned long)tail[0]); /* FALLTHROUGH */
            k1 *= c1; 
            k1 = ROT(k1,31,33);
            k1 *= c2; 
            h1 ^= k1;
            /* FALLTHROUGH */
    case 0:
            break;
    }

    // Finalization
    h1 ^= len;
    h2 ^= len;

    h1 += h2;
    h2 += h1;

    // Mixing functions from the 128-bit finalization
    h1 ^= h1 >> 33;
    h1 *= 0xff51afd7ed558ccdULL;
    h1 ^= h1 >> 33;
    h1 *= 0xc4ceb9fe1a85ec53ULL;
    h1 ^= h1 >> 33;

    h2 ^= h2 >> 33;
    h2 *= 0xff51afd7ed558ccdULL;
    h2 ^= h2 >> 33;
    h2 *= 0xc4ceb9fe1a85ec53ULL;
    h2 ^= h2 >> 33;

    h1 += h2;

    // We only need 64 bits, so just return h1
    return h1;
#undef ROT
}

/* Seed for the murmur hash function, this is a meme number but works
 * surprisingly well */
#define MURMUR_HASH_SEED (0xBABECAFE69)
u64 cstringMurmur(char *key, s64 len) {
//    u64 hash = 0;
//    for (s64 i = 0; i < len; ++i) {
//        hash = ((hash << 5) - hash) + key[i];
//    }
//    return hash;
//
    return murmur(key, len, MURMUR_HASH_SEED);
}

u64 aoStrHashFunction(AoStr *str) {
    return cstringMurmur(str->data, str->len);
}

u64 aoStrGetLen(AoStr *buf) {
    return buf->len;
}

AoStr *aoStrIdentity(AoStr *buf) {
    return buf;
}

int aoStrEq(AoStr *b1, AoStr *b2) {
    if (b1 == b2) return 1;
    u64 l1 = b1->len;
    u64 l2 = b2->len;
    return l1==l2&&!memcmp(b1->data, b2->data, l1);
}
