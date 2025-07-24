/* Copyright (C) 2023 James W M Barford-Evans
 * <jamesbarfordevans at gmail dot com>
 * All Rights Reserved
 *
 * This code is released under the BSD 2 clause license.
 * See the COPYING file for more information. */
#ifndef AOSTR_H
#define AOSTR_H

#include <stdarg.h>
#include <stddef.h>
#include <sys/types.h>

typedef struct AoStr {
    char *data;
    size_t len;
    size_t capacity;
} AoStr;

AoStr *aoStrAlloc(size_t capacity);
AoStr *aoStrNew(void);
void aoStrRelease(AoStr *buf);

int aoStrExtendBuffer(AoStr *buf, size_t additional);
void aoStrToLowerCase(AoStr *buf);
void aoStrToUpperCase(AoStr *buf);
void aoStrPutChar(AoStr *buf, char ch);
void aoStrRepeatChar(AoStr *buf, char ch, int times);
int aoStrCmp(AoStr *b1, AoStr *b2);
AoStr *aoStrDupRaw(char *s, size_t len);
AoStr *aoStrDup(AoStr *buf);

char *aoStrMove(AoStr *buf);

void aoStrCatLen(AoStr *buf, const void *d, size_t len);
void aoStrCatAoStr(AoStr *buf, AoStr *s2);
void aoStrCat(AoStr *buf, const void *d);
void aoStrCatRepeat(AoStr *buf, char *str, int times);
void aoStrCatPrintf(AoStr *b, const char *fmt, ...);
void aoStrCatFmt(AoStr *buf, const char *fmt, ...);
AoStr *aoStrPrintf(const char *fmt, ...);
AoStr *aoStrEscapeString(AoStr *buf);
AoStr *aoStrEncode(AoStr *buf);

void aoStrArrayRelease(AoStr **arr, int count);
AoStr **aoStrSplit(char *to_split, char delimiter, int *count);
char *mprintf(const char *fmt, ...);
char *mprintFmt(const char *fmt, ...);
char *mprintVa(const char *fmt, va_list ap, ssize_t *_len);
AoStr *aoStrError(void);
AoStr *aoStrIntToHumanReadableBytes(long bytes);

#endif
