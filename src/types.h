#ifndef TYPES_H__
#define TYPES_H__

#include <stdint.h>
#include <limits.h>
#include <float.h>

/* Make sane types */

#define I8_MAX  INT8_MAX
#define U8_MAX  UINT8_MAX
#define I16_MAX INT16_MAX
#define U16_MAX UINT16_MAX
#define I32_MAX INT32_MAX
#define U32_MAX UINT32_MAX
#define I64_MAX INT64_MAX
#define U64_MAX UINT64_MAX
#define F32_MAX FLT_MAX
#define F64_MAX DBL_MAX

/* Originally these were the stdint fixed sized types however when printing them
 * it would cause havoc... Thus they are now just whatever C gives us*/

/* Signed 8 bit integer */
typedef char s8;
/* Unsigned 8 bit integer */
typedef unsigned char u8;
/* Signed 16 bit integer */
typedef short s16;
/* Unsigned 16 bit integer */
typedef unsigned short u16;
/* Signed 32 bit integer */
typedef int s32;
/* Unsigned 32 bit integer */
typedef unsigned int u32;
/* Signed 64 bit integer */
typedef long s64;
/* Unsigned 64 bit integer */
typedef unsigned long  u64;
/* 32 bit float */
typedef float     f32;
/* 64 bit float */
typedef double    f64;

_Static_assert(sizeof(s8) == 1, "`sizeof(s8)` should be 1");
_Static_assert(sizeof(u8) == 1, "`sizeof(u8)` should be 1");
_Static_assert(sizeof(s16) == 2, "`sizeof(s16)` should be 2");
_Static_assert(sizeof(u16) == 2, "`sizeof(u16)` should be 2");
_Static_assert(sizeof(s32) == 4, "`sizeof(s32)` should be 4");
_Static_assert(sizeof(u32) == 4, "`sizeof(u32)` should be 4");
_Static_assert(sizeof(s64) == 8, "`sizeof(s64)` should be 8");
_Static_assert(sizeof(u64) == 8, "`sizeof(u64)` should be 8");

_Static_assert(sizeof(f32) == 4, "`sizeof(f32)` should be 4");
_Static_assert(sizeof(f64) == 8, "`sizeof(f64)` should be 8");
#endif
