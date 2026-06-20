/* Yup we can mix #define's and assembly */
#define ALIGN_16 0xFFFFFFFFFFFFFFF0

public extern "c" U0 *memchr(U0 *__s, I32 __c, U64 __n);
public extern "c" I32 memcmp(U0 *__s1, U0 *__s2, U64 __n);
public extern "c" U0 *memmove(U0 *__dst, U0 *__src, U64 __len);
public extern "c" U0 free(U0 *ptr);
public extern "c" U0 *malloc(U64 size);
public extern "c" U0 *calloc(U64 count, U64 size);
public extern "c" U0 *realloc(U0 *ptr, U64 size);
public extern "c" U0 *memset(U0 *__dst, I32 __c, U64 __n);
public extern "c" U0 *memcpy(U0 *__dst, U0 *__src, U64 __n);

#ifdef IS_X86_64
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
public _extern _MALLOC U0 *MAlloc(U64 size);
public _extern _FREE U0 Free(U0 *ptr);
public _extern _REALLOC U0 *ReAlloc(U0 *ptr, U64 new_size);
public _extern _CALLOC U0 *CAlloc(U64 new_size);
public _extern _MSIZE U64 MSize(U0 *ptr);
public _extern _MEMCPY U0 *MemCpy(U0 *dst, U0 *src, U64 len);
public _extern _MEMSET U0 *MemSet(U0 *dst, I32 ch, U64 len);
#endif

#ifdef __aarch64__
/* HolyC fallbacks for non-x86 targets. MAlloc's 8-byte size header
 * is preserved so MSize/Free/ReAlloc remain compatible with code
 * that depends on it. */
asm {
_MALLOC::
    STP    X29, X30, [SP, -16]! // save FP/LR - we make a call
    STR    X0, [SP, -16]!       // save size across the call (X1 is caller-saved)
    ADD    X0, X0, 8            // add 8, big enough for a U64 header
    CALL   malloc               // X0 = malloc(size + 8)
    LDR    X1, [SP], 16         // reload size
    STR    X1, [X0]             // store the U64 size header at [ptr]
    ADD    X0, X0, 8            // step the pointer past the header
    LDP    X29, X30, [SP], 16   // restore FP/LR
    RET
}
public _extern _MALLOC U0 *MAlloc(U64 size);

public U0 Free(U0 *ptr)
{
  U64 *p;
  if (!ptr) return;
  p = ptr;
  free(&p[-1]);
}

public U64 MSize(U0 *ptr)
{
  U64 *p;
  if (!ptr) return 0;
  p = ptr;
  return p[-1];
}

public U0 *MemSet(U0 *dst, I32 ch, U64 len)
{
  memset(dst, ch, len);
  return dst;
}

public U0 *MemCpy(U0 *dst, U0 *src, U64 len)
{
  memcpy(dst, src, len);
  return dst;
}

public U0 *CAlloc(U64 size)
{
  U0 *p = MAlloc(size);
  MemSet(p, 0, size);
  return p;
}

public U0 *ReAlloc(U0 *ptr, U64 new_size)
{
  U0 *new_ptr;
  U64 old_size;
  if (!ptr) return MAlloc(new_size);
  new_ptr = MAlloc(new_size);
  old_size = MSize(ptr);
  if (old_size > new_size) old_size = new_size;
  MemCpy(new_ptr, ptr, old_size);
  Free(ptr);
  return new_ptr;
}
#endif
