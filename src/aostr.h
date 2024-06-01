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

int aoStrExtendBuffer(aoStr *buf, unsigned int additional);
void aoStrToLowerCase(aoStr *buf);
void aoStrPutChar(aoStr *buf, char ch);
void aoStrRepeatChar(aoStr *buf, char ch, int times);
int aoStrCmp(aoStr *b1, aoStr *b2);
aoStr *aoStrDupRaw(char *s, size_t len);
aoStr *aoStrDup(aoStr *buf);

char *aoStrMove(aoStr *buf);

void aoStrCatLen(aoStr *buf, const void *d, size_t len);
void aoStrCat(aoStr *buf, const void *d);
void aoStrCatRepeat(aoStr *buf, char *str, int times);
void aoStrCatPrintf(aoStr *b, const char *fmt, ...);
aoStr *aoStrEscapeString(aoStr *buf);

void aoStrArrayRelease(aoStr **arr, int count);
aoStr **aoStrSplit(char *to_split, char delimiter, int *count);
char *mprintf(const char *fmt, ...);

#endif
