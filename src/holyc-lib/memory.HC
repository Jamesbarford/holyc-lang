/* Yup we can mix #define's and assembly */
#define ALIGN_16 0xFFFFFFFFFFFFFFF0

asm {
_MALLOC::
    PUSHQ  RBP
    MOVQ   RBP, RSP 
    SUB    RSP,16
    MOV    -8[RBP], RDI
    ADD    RDI, 8
    CALL   malloc
    MOV    RCX, -8[RBP]
    MOV    [RAX], RCX
    ADD    RAX, 8
    LEAVE
    RET

// RDI = size
// Different from c's calloc
_CALLOC::
    PUSHQ  RBP
    MOVQ   RBP, RSP 
    SUB    RSP,16
    MOV    -8[RBP],RDI
    CALL   _MALLOC
    MOV    -16[RBP], RAX
    MOV    RDX, -8[RBP]
    MOV    RSI, 0
    MOV    RDI, -16[RBP]
    CALL   _MEMSET
    MOV    RAX, -16[RBP]
    LEAVE
    RET

// RDI = U0 *ptr
// RSI = U64 size
_REALLOC:: // U0 *ReAlloc(U0 *ptr, I64 size);
    PUSHQ  RBP
    MOVQ   RBP, RSP 
    SUB    RSP, 16
    TEST   RDI, RDI
    JZ     @@1
    MOV    -8[RBP], RDI
    MOV    RDI, RSI      // new_size
    CALL   _MALLOC
    MOV    -16[RBP], RAX // new memory saved
    MOV    RDI, -8[RBP]  // find out the old size
    CALL   _MSIZE
    MOV    RDX, RAX      // old size
    MOV    RSI, -8[RBP]  // old pointer
    MOV    RDI, -16[RBP] // new pointer
    CALL   _MEMCPY       // copy over
    MOV    RDI, -8[RBP]
    CALL   _FREE
    MOV    RAX, -16[RBP]
    JMP    @@2
@@1: // If pointer is NULL
    MOV    RDI, RSI
    CALL   _MALLOC
@@2:
    LEAVE
    RET

/* How many bytes were allocated */
_MSIZE::
    PUSHQ  RBP
    MOVQ   RBP, RSP 
    TEST   RDI, RDI
    JZ     @@2/* NULL CHECK */
    SUB    RDI, 8
    MOV    RAX, [RDI]
@@1:
    LEAVE
    RET
@@2:
    MOVQ   RAX, 0
    LEAVE
    RET

/* Gets to the start of the actual allocated buffer 
 * And then calls free */
_FREE::
    PUSHQ  RBP
    MOVQ   RBP, RSP 
    TEST   RDI,RDI
    JZ     @@1
    SUB    RDI, 8
    CALL   free
@@1:
    LEAVE
    RET

_MEMSET::
    PUSHQ RBP
    MOVQ  RBP, RSP 
    MOVQ  RCX, RDX
    MOVB  AL,  SIL
    CLD
    REP   STOSB
    MOVQ  RDI, RAX
    LEAVE
    RET

_MEMCPY::
    PUSHQ  RBP
    MOV    RBP, RSP 
    MOVQ   RCX, RDX 
    CLD
    REP    MOVSB
    LEAVE
    RET 
}

public extern "c" U0 *memchr(U0 *__s, I32 __c, U64 __n);
public extern "c" I32 memcmp(U0 *__s1, U0 *__s2, U64 __n);
public extern "c" U0 *memmove(U0 *__dst, U0 *__src, U64 __len);
public extern "c" U0 free(U0 *ptr);
public extern "c" U0 *malloc(U64 size);
public extern "c" U0 *calloc(U64 count, U64 size);
public extern "c" U0 *realloc(U0 *ptr, U64 size);
public _extern _MALLOC U0 *MAlloc(U64 size);
public _extern _FREE U0 Free(U0 *ptr);
public _extern _REALLOC U0 *ReAlloc(U0 *ptr, U64 new_size);
public _extern _CALLOC U0 *CAlloc(U64 new_size);
public _extern _MSIZE U64 MSize(U0 *ptr);
public _extern _MEMCPY U0 *MemCpy(U0 *dst, U0 *src, U64 len);
public _extern _MEMSET U0 *MemSet(U0 *dst, I32 ch, U64 len);
