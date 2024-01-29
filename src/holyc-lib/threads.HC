#include <memory.HC>

asm {
_THREAD_NEW::
	PUSHQ	RBP
	MOVQ	RBP, RSP
	SUBQ	RSP, 32
	MOVQ	-24[RBP], RDI
	MOVQ	-32[RBP], RSI
	MOVQ	RCX,-32[RBP]
	MOVQ	RDX,-24[RBP]
	LEAQ	RAX, -8[RBP] 
	MOVQ	RSI, 0
	MOVQ	RDI,RAX
	CALL	pthread_create
	MOVQ	RAX, -8[RBP]
    ADDQ    RSP, 32
	LEAVE
	RET

_THREAD_JOIN::
	PUSHQ	RBP
	MOVQ	RBP, RSP
	MOVQ    -8[RBP], RDI
	MOVQ	RDI, -8[RBP]
	MOVQ	RSI, 0
	CALL	pthread_join
	LEAVE
	RET

_THREAD_LOCK_INIT::
	PUSHQ	RBP
	MOVQ	RBP, RSP
	SUBQ	RSP, 16
	MOVQ	-8[RBP], RDI
	MOVQ	RDI, -8[RBP]
    MOVQ    RSI, 0
	CALL	pthread_mutex_init
    ADDQ    RSP, 16
	LEAVE
	RET

_THREAD_LOCK::
	PUSHQ	RBP
	MOVQ	RBP, RSP
	SUBQ	RSP, 8
	MOVQ	-8[RBP], RDI
	MOVQ	RDI, -8[RBP]
	CALL	pthread_mutex_lock
    ADDQ    RSP, 8
	LEAVE
	RET

_THREAD_UNLOCK::
	PUSHQ	RBP
	MOVQ	RBP, RSP
	SUBQ	RSP, 8
	MOVQ	-8[RBP], RDI
	MOVQ	RDI, -8[RBP]
	CALL	pthread_mutex_unlock
    ADDQ    RSP, 8
	LEAVE
	RET

_THREAD_LOCK_DESTORY::
	PUSHQ	RBP
	MOVQ	RBP, RSP
	SUBQ	RSP, 8
	MOVQ	-8[RBP], RDI
	MOVQ	RDI, -8[RBP]
	CALL	pthread_mutex_destroy
    ADDQ    RSP, 8
	LEAVE
	RET
}

#define __MUTEX_LOCK_PADDING__ 56

/* This I _think_ is specific to MacOS implementation of pthreads 
 * It does work. */
class MutexLock 
{
  I64 __sig;
  I8 __opaque[__MUTEX_LOCK_PADDING__];
};

public _extern _THREAD_NEW I64 ThreadNew(U0 (*function_ptr)(U0 *argv),
        U0 *argv=NULL);
public _extern _THREAD_JOIN I64 ThreadJoin(I64 thread_id,
        U0 *thread_attr=NULL);
public _extern _THREAD_LOCK_INIT I64 ThreadLockInit(MutexLock *lk,
        U0 *thread_attr=NULL);
public _extern _THREAD_LOCK_DESTORY I64 ThreadLockDestroy(MutexLock *lk);
public _extern _THREAD_LOCK I64 ThreadLock(MutexLock *lk);
public _extern _THREAD_UNLOCK I64 ThreadUnLock(MutexLock *lk);

public MutexLock *MutexLockNew()
{
  MutexLock *lk = MAlloc(sizeof(MutexLock));
  ThreadLockInit(lk);
  return lk;
}

public U0 MutexLockDestroy(MutexLock *lk)
{
   ThreadLockDestroy(lk);
   Free(lk);
}
