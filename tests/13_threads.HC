#include <memory.HC>
#include <threads.HC>
#include "testhelper.HC"

#define THREAD_COUNT 5

class ThreadArgs
{
  MutexLock *lk;
  I64 id;
  I64 *ptr;
};

U0 *Func(ThreadArgs *args)
{
  ThreadLock(args->lk);
  I64 x = *args->ptr;
  x++;
  *args->ptr = x;
  ThreadUnLock(args->lk);
  return NULL;
}

U0 Main()
{
  "Test - Simple Threading: ";
  I64 threads[THREAD_COUNT];
  I64 correct = 0, tests = 1;
  ThreadArgs thread_args[THREAD_COUNT];

  MutexLock *lk = MutexLockNew();

  I64 x = 0;

  for (I64 i = 0; i < THREAD_COUNT; ++i) {
    thread_args[i].lk = lk;
    thread_args[i].ptr = &x;
    thread_args[i].id = i;
    threads[i] = ThreadNew(&Func, &thread_args[i]);
  }

  for (I64 i = 0; i < THREAD_COUNT; ++i) {
    ThreadJoin(threads[i]);
  }

  MutexLockDestroy(lk);
  /* this proves that 1: thread join works, 2: the mutex lock works */
  PrintResult(x == THREAD_COUNT, tests);
  "====\n";
}
