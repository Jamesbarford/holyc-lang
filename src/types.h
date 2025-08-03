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

/* Signed 8 bit integer */
typedef int8_t     s8;
/* Unsigned 8 bit integer */
typedef uint8_t    u8;
/* Signed 16 bit integer */
typedef int16_t   s16;
/* Unsigned 16 bit integer */
typedef uint16_t  u16;
/* Signed 32 bit integer */
typedef int32_t   s32;
/* Unsigned 32 bit integer */
typedef uint32_t  u32;
/* Signed 64 bit integer */
typedef int64_t   s64;
/* Unsigned 64 bit integer */
typedef uint64_t  u64;
/* 32 bit float */
typedef float     f32;
/* 64 bit float */
typedef double    f64;

#endif
