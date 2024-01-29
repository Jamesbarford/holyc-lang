#include <defs.HH>
/* Yup we can mix #define's and assembly */
#define ALIGN_16 0xFFFFFFFFFFFFFFF0

asm {
_MALLOC::
	PUSHQ	RBP
	MOVQ	RBP, RSP 
    SUB     RSP,16
    MOV     -8[RBP], RDI
    ADD     RDI, 8
    CALL    malloc
    MOV     RCX, -8[RBP]
    MOV     [RAX], RCX
    ADD     RAX, 8
    LEAVE
    RET

// RDI = size
// Different from c's calloc
_CALLOC::
	PUSHQ	RBP
	MOVQ	RBP, RSP 
    SUB     RSP,16
    MOV     -8[RBP],RDI
    CALL    _MALLOC
    MOV     -16[RBP], RAX
    MOV     RDI, -16[RBP]
    MOV     RSI, 0
    MOV     RDX, -8[RBP]
    CALL    _MEMSET
    MOV     RAX, -16[RBP]
    LEAVE
    RET

// RDI = U0 *ptr
// RSI = U64 size
_REALLOC::
	PUSHQ	RBP
	MOVQ	RBP, RSP 
    SUB     RSP, 32
    TEST    RDI, RDI
    JZ      @@1
    MOV     -8[RBP], RDI
    MOV     -16[RBP],RSI
    MOV     RDI, -16[RBP]
    CALL    _MALLOC
    MOV     -24[RBP], RAX
    MOV     RDI, -8[RBP]
    CALL    _MSIZE
    MOV     RDX, RAX
    MOV     RSI, -8[RBP]
    MOV     RDI, -24[RBP]
    CALL    _MEMCPY
    MOV     RDI, -8[RBP]
    CALL    _FREE
    MOV     RAX, -24[RBP]
    JMP     @@2
@@1: // If pointer is NULL
    MOV     RDI, RSI
    CALL    _MALLOC
@@2:
    LEAVE
    RET

/* How many bytes were allocated */
_MSIZE::
	PUSHQ	RBP
	MOVQ	RBP, RSP 
    TEST    RDI, RDI
    JZ      @@2/* NULL CHECK */
    SUB     RDI, 8
    MOV     RAX, [RDI]
@@1:
    LEAVE
    RET
@@2:
    MOVQ    RAX, 0
    LEAVE
    RET

/* Gets to the start of the actual allocated buffer 
 * And then calls free */
_FREE::
	PUSHQ	RBP
	MOVQ	RBP, RSP 
    TEST    RDI,RDI
    JZ      @@1
    SUB     RDI, 8
    CALL    free
@@1:
    LEAVE
    RET

/* Attempt at allocating aligned memory */
_MALLOC_ALIGN16::
    PUSHQ   RBP
    MOVQ    RBP, RSP

    MOVQ    RAX, RDI
    ADDQ    RAX, 15
    ANDQ    RAX, ALIGN_16
    ADDQ    RAX, 8
    MOVQ    RDI, RAX
    CALL    malloc

    TEST    RAX, RAX
    JZ      @@01

    ADDQ    RAX, 15
    ANDQ    RAX, ALIGN_16
    MOVQ    -16[RBP], RAX
    MOVQ    RCX, -16[RBP]
    SUBQ    RCX, 8
    MOVQ    [RCX], RAX
    MOVQ    RAX, -16[RBP] 
@@01:
    LEAVE
    RET

_FREE_ALIGNED::
    PUSHQ   RBP
    MOVQ    RBP, RSP
    MOVQ    RAX, RDI
    SUBQ    RAX, 8
    MOVQ    RDI, [RAX]
    CALL    free
    LEAVE
    RET

_MEMSET::
	PUSHQ	RBP
	MOVQ	RBP, RSP 
    MOVQ    RCX, RDX
    MOVB    AL,  SIL
	CLD
	REP     STOSB
	MOVQ	RDI, RAX
    LEAVE
    RET

_MEMCPY::
	PUSHQ   RBP
	MOV     RBP, RSP 
    MOVQ    RCX, RDX 
	CLD
	REP     MOVSB
    LEAVE
	RET 
}

public _extern _MALLOC U0 *MAlloc(I64 size);
public _extern _FREE U0 Free(U0 *ptr);
public _extern _REALLOC U0 *ReAlloc(U0 *ptr, U64 new_size);
public _extern _CALLOC U0 *CAlloc(U0 *ptr, U64 new_size);
public _extern _MSIZE U64 MSize(U0 *ptr);
public _extern _MALLOC_ALIGN16 U0 *MAlloc16(I64 size);
public _extern _FREE_ALIGNED U0 Free16(U0 *ptr);
public _extern _MEMCPY U0 *MemCpy(U0 *dst, U0 *src, I64 len);
public _extern _MEMSET U0 *MemSet(U0 *dst, I32 ch, I64 len);
