/* Copyright (C) 2023 James W M Barford-Evans
 * <jamesbarfordevans at gmail dot com>
 * All Rights Reserved
 *
 * This code is released under the BSD 2 clause license.
 * See the COPYING file for more information. */
#ifndef AOSTR_H
#define AOSTR_H

#include <stddef.h>

typedef struct aoStr aoStr;

typedef struct aoStr {
    char *data;
    size_t offset;
    size_t len;
    size_t capacity;
} aoStr;

aoStr *aoStrAlloc(size_t capacity);
aoStr *aoStrNew(void);
void aoStrRelease(aoStr *buf);

char *aoStrGetData(aoStr *buf);
void aoStrSetLen(aoStr *buf, size_t len);
size_t aoStrLen(aoStr *buf);
size_t aoStrGetOffset(aoStr *buf);
void aoStrSetOffset(aoStr *buf, size_t offset);
char *aoStrGetData(aoStr *buf);
char aoStrGetChar(aoStr *buf);
char aoStrGetCharAt(aoStr *buf, size_t at);
int aoStrEndsWith(aoStr *buf, char *pattern, int patternlen);
void aoStrAdvance(aoStr *buf);
void aoStrAdvanceBy(aoStr *buf, size_t by);
void aoStrRewindBy(aoStr *buf, size_t by);
int aoStrWouldOverflow(aoStr *buf, size_t additional);
int aoStrMatchChar(aoStr *buf, char ch);
int aoStrMatchCharAt(aoStr *buf, char ch, size_t at);
void aoStrSetCapacity(aoStr *buf, size_t capacity);
size_t aoStrCapacity(aoStr *buf);
int aoStrExtendBuffer(aoStr *buf, unsigned int additional);
void aoStrToLowerCase(aoStr *buf);
void aoStrToUpperCase(aoStr *buf);
void aoStrPutChar(aoStr *buf, char ch);
void aoStrRepeatChar(aoStr *buf, char ch, int times);
char aoStrUnPutChar(aoStr *buf);
int aoStrNCmp(aoStr *b1, aoStr *b2, size_t size);
int aoStrNCaseCmp(aoStr *b1, aoStr *b2, size_t size);
int aoStrStrNCmp(aoStr *b1, char *s, size_t len);
int aoStrStrNCaseCmp(aoStr *b1, char *s, size_t len);
int aoStrCmp(aoStr *b1, aoStr *b2);
int aoStrCaseCmp(aoStr *b1, aoStr *b2);
void aoStrSlice(aoStr *buf, size_t from, size_t to, size_t size);
aoStr *aoStrFromString(char *s, size_t len);
aoStr *aoStrDupRaw(char *s, size_t len);
aoStr *aoStrDup(aoStr *buf);
aoStr *aoStrMaybeDup(aoStr *buf);
void aoStrReset(aoStr *buf);

aoStr *aoStrView(char *s, size_t len);
void aoStrViewRelease(aoStr *buf);

char *aoStrMove(aoStr *buf);

void aoStrCatLen(aoStr *buf, const void *d, size_t len);
void aoStrCat(aoStr *buf, const void *d);
void aoStrCatRepeat(aoStr *buf, char *str, int times);
void aoStrCatPrintf(aoStr *b, const char *fmt, ...);
aoStr *aoStrEscapeString(aoStr *buf);

int *aoStrComputePrefixTable(char *pattern, size_t patternlen);
int aoStrContainsPatternWithTable(aoStr *buf, int *table, char *pattern,
                                  size_t patternlen);
int aoStrContainsCasePatternWithTable(aoStr *buf, int *table, char *pattern,
                                      size_t patternlen);
int aoStrContainsPattern(aoStr *buf, char *pattern, size_t patternlen);
int aoStrContainsCasePattern(aoStr *buf, char *pattern, size_t patternlen);

void aoStrArrayRelease(aoStr **arr, int count);
aoStr **aoStrSplit(char *to_split, char delimiter, int *count);
int aoStrIsMimeEncoded(aoStr *buf);
void aoStrDecodeMimeEncodedInplace(aoStr *buf);
aoStr *aoStrDecodeMimeEncoded(aoStr *buf);

#endif
