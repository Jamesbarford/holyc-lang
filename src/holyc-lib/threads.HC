#include "./memory.HC"
#include "./list.HC"

#ifdef IS_MACOS
#define __MUTEX_LOCK_PADDING__   56
#define __MUTEX_LOCK_ATTR_SIZE__ 8

#define __THREAD_COND_SIZE__     40
#define __THREAD_CONDATTR_SIZE__ 8
#define __THREAD_SIZE__          8176
#define __THREAD_ATTR_SIZE__     56

/* MACOS definitions */
class pthread_mutex_t 
{
  I64 __sig;
  I8 __opaque[__MUTEX_LOCK_PADDING__];
};

class pthread_mutexattr_t
{
  I64 __sig;
  I8 __opaque[__MUTEX_LOCK_ATTR_SIZE__];
};

class pthread_cond_t
{
  I64 __sig;
  I8 __opaque[__THREAD_COND_SIZE__];
};

class pthread_condattr_t
{
  I64 __sig;
  I8 __opaque[__THREAD_CONDATTR_SIZE__];
};

class pthread_handler_rec;
class pthread_handler_rec
{
  U0 (*__routine)(U0 *__arg); // Routine to call
  U0 *__arg;                  // Argument to pass
  pthread_handler_rec *__next;
};

class pthread_attr_t
{  
  I64 __sig;
  I8 __opaque[__THREAD_ATTR_SIZE__];
};

class pthread_t 
{
  I64 __sig;
  pthread_handler_rec  *__cleanup_stack;
  I8 __opaque[__THREAD_SIZE__];
};

#elifdef IS_LINUX /* Linux definitions, this needs to be auto generated */

#define __SIZEOF_PTHREAD_MUTEX_T   40
#define __SIZEOF_PTHREAD_ATTR_T    56
#define __SIZEOF_PTHREAD_RWLOCK_T  56
#define __SIZEOF_PTHREAD_BARRIER_T 32

#define __SIZEOF_PTHREAD_MUTEXATTR_T   4
#define __SIZEOF_PTHREAD_COND_T        48
#define __SIZEOF_PTHREAD_CONDATTR_T    4
#define __SIZEOF_PTHREAD_RWLOCKATTR_T  8
#define __SIZEOF_PTHREAD_BARRIERATTR_T 4

union pthread_mutexattr_t
{
  I8  __size[__SIZEOF_PTHREAD_MUTEXATTR_T];
  I32 __align;
};

union pthread_condattr_t
{
  I8  __size[__SIZEOF_PTHREAD_CONDATTR_T];
  I32 __align;
};

union pthread_attr_t
{
  I8  __size[__SIZEOF_PTHREAD_ATTR_T];
  I64 __align;
};


/* this is the spiritual equivilent of typedef  */ 
public class pthread_t
{
  U64 __padding;
};

class __pthread_list_t;
class __pthread_list_t
{
  __pthread_list_t *__prev;
  __pthread_list_t *__next;
};

class __pthread_mutex_s
{
  I32 __lock;
  U32 __count;
  I32 __owner;
  U32 __nusers;
  I32 __kind;
  I16 __spins;
  I16 __elision;
  __pthread_list_t __list;
};

union pthread_mutex_t 
{
  __pthread_mutex_s __data;
  I32 __size[__SIZEOF_PTHREAD_MUTEX_T];
  I64 __align;
};

union __atomic_wide_counter
{
  U64 __value64;
  class
  {
    U32 __low;
    U32 __high;
  };
};

class __pthread_cond_s
{
  __atomic_wide_counter __wseg;
  __atomic_wide_counter __g1_start;
  U32 __g_regs[2];
  U32 __g_size[2];
  U32 __g1_orig_size;
  U32 __wrefs;
  U32 __g_signals[2];
};

union pthread_cond_t
{
  __pthread_cond_s __data;
  I8  __size[__SIZEOF_PTHREAD_COND_T];
  I64 __align;
};
#endif

class ThreadWorker
{
  pthread_t *th;
  I32 id;
};

class ThreadSemaphore
{
  pthread_cond_t cond;
  pthread_mutex_t lk;
  I32 val;
}; 

class ThreadWorkerJob;
class ThreadWorkerJob
{
  U0 (*callback)(U0 *priv_data, U0 *argv);
  U0 *argv;
  ThreadWorkerJob *next;
};

class ThreadPool
{
  U64 worker_count,alive_threads,active_threads;
  Bool run;
  pthread_cond_t has_work,no_work;
  pthread_mutex_t lock, qlock;
  ThreadSemaphore *sem;
  ThreadWorker *workers;
  List *jobs;
  U0 *priv_data;
};

public extern "c" I64 pthread_create(pthread_t **th, U0 *thread_attr=NULL, 
		U0 (*function_ptr)(U0 *argv), U0 *argv=NULL);
public extern "c" I32 pthread_mutex_init(pthread_mutex_t *lk, pthread_mutexattr_t *mutex_attr=NULL);
public extern "c" I32 pthread_mutex_destroy(pthread_mutex_t *lk);
public extern "c" I32 pthread_mutex_lock(pthread_mutex_t *lk);
public extern "c" I32 pthread_mutex_unlock(pthread_mutex_t *lk);
public extern "c" I32 pthread_join(pthread_t *th, U0 *ptr=NULL);
public extern "c" I32 pthread_cond_broadcast(pthread_cond_t *cond);
public extern "c" I32 pthread_cond_destroy(pthread_cond_t *cond);
public extern "c" I32 pthread_cond_init(pthread_cond_t *cond,
		pthread_condattr_t *attr=NULL);
public extern "c" I32 pthread_cond_signal(pthread_cond_t *cond);
public extern "c" I32 pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *lk);
public extern "c" I32 pthread_exit(U0 *data=NULL);
public extern "c" I32 pthread_detach(pthread_t *th);
public extern "c" I32 pthread_attr_setstacksize(pthread_attr_t *attr, U64 stacksize);

static U0 ThreadSemaphoreSignal(ThreadSemaphore *sem)
{
  pthread_mutex_lock(&sem->lk);
  sem->val = 1;
  pthread_cond_signal(&sem->cond);
  pthread_mutex_unlock(&sem->lk);
}

static U0 ThreadSemaphoreWait(ThreadSemaphore *sem)
{
  pthread_mutex_lock(&sem->lk);
  while (sem->val != 1) {
    pthread_cond_wait(&sem->cond,&sem->lk);
  }
  sem->val = 0;
  pthread_mutex_unlock(&sem->lk);
}

U0 ThreadPoolEnqueue(ThreadPool *pool, U0 *argv, U0 (*callback)(U0 *priv_data, U0 *argv))
{// Enqueue a job
  ThreadWorkerJob *job = MAlloc(sizeof(ThreadWorkerJob));
  job->callback = callback;
  job->argv = argv;
  job->next = NULL;
  pthread_mutex_lock(&pool->qlock);
  ListAppend(pool->jobs,job);
  ThreadSemaphoreSignal(pool->sem);
  pthread_mutex_unlock(&pool->qlock);
}

static ThreadWorkerJob *ThreadPoolDequeue(ThreadPool *pool)
{
  pthread_mutex_lock(&pool->qlock);
  ThreadWorkerJob *job = ListDeque(pool->jobs);
  if (job) {
    ThreadSemaphoreSignal(pool->sem);
  }
  pthread_mutex_unlock(&pool->qlock);
  return job;
}

U0 ThreadPoolWait(ThreadPool *pool)
{//Wait for all jobs in the queue to be complete
  pthread_mutex_lock(&pool->lock);
  while (!ListEmpty(pool->jobs) || pool->active_threads != 0) {
    pthread_cond_wait(&pool->no_work,&pool->lock);
  }
  pthread_mutex_unlock(&pool->lock);
}

static U0 *ThreadPoolMain(U0 *argv)
{// This takes jobs of the queue, calls them and waits for more work
  ThreadPool *pool = argv(ThreadPool *);
  ThreadWorkerJob *job;

  pthread_mutex_lock(&pool->lock);
  pool->alive_threads++;
  pthread_mutex_unlock(&pool->lock);
  
  while (pool->run) {
    ThreadSemaphoreWait(pool->sem);
    pthread_mutex_lock(&pool->lock);
    pool->active_threads++;
    pthread_mutex_unlock(&pool->lock);

    if ((job = ThreadPoolDequeue(pool)) != NULL) {
      job->callback(pool->priv_data,job->argv);
      Free(job);
    }

    pthread_mutex_lock(&pool->lock);
    pool->active_threads--;
    if (pool->active_threads == 0) {
      pthread_cond_signal(&pool->no_work);
    }
    pthread_mutex_unlock(&pool->lock);

  }
  pool->worker_count--;
  if (pool->active_threads == 0) {
    pthread_cond_signal(&pool->no_work);
  }
  pthread_mutex_unlock(&pool->lock);
  pthread_exit();
  return NULL;
}

ThreadPool *ThreadPoolNew(I64 worker_count)
{// Create a new pool
  ThreadPool *pool = MAlloc(sizeof(ThreadPool));
  ThreadSemaphore *sem = MAlloc(sizeof(ThreadSemaphore));
  ThreadWorker *workers = MAlloc(sizeof(ThreadWorker) * worker_count);

  pool->jobs = ListNew();
  pool->run = TRUE;
  pool->active_threads = 0;
  pool->alive_threads = 0;
  pool->sem = sem;
  pool->worker_count = worker_count;
  pool->workers = workers;
  pool->priv_data = NULL;

  sem->val = 0;

  pthread_mutex_init(&sem->lk);
  pthread_cond_init(&sem->cond);

  pthread_cond_init(&pool->no_work);
  pthread_cond_init(&pool->has_work);
  pthread_mutex_init(&pool->lock);
  pthread_mutex_init(&pool->qlock);

  for (I64 i = 0; i < pool->worker_count; ++i) {
    ThreadWorker *worker = workers + i;
    worker->id = i;
    pthread_create(&worker->th,NULL,&ThreadPoolMain,pool);
    pthread_detach(worker->th);
  }
  while (pool->alive_threads != pool->worker_count);
  return pool;
}

U0 ThreadPoolStop(ThreadPool *pool)
{// Wait for all the jobs to be done
  pthread_mutex_lock(&pool->lock);
  while (pool->active_threads != 0) {
    pthread_cond_wait(&pool->has_work,&pool->lock);
  }
  pool->run = FALSE;
  pthread_cond_broadcast(&pool->has_work);
  pthread_mutex_unlock(&pool->lock);

  for (I64 i = 0; i < pool->worker_count; ++i) {
    pthread_join(pool->workers[i].th);
  }
  pool->worker_count = 0;
  pthread_cond_signal(&pool->no_work);
}

U0 ThreadPoolRelease(ThreadPool *pool)
{// abrubtly stop the pool, destroy conditions, locks and free structures
  if (pool) {
    pthread_cond_t *sem_cond = &(pool->sem->cond);
    auto sem = pool->sem;
    pool->run = FALSE;
    ThreadPoolStop(pool);

    pthread_cond_destroy(&pool->has_work);
    pthread_cond_destroy(&pool->no_work);
    pthread_mutex_destroy(&pool->lock);
    pthread_mutex_destroy(&pool->qlock);

    pthread_mutex_destroy(&sem->lk);
    pthread_cond_destroy(sem_cond);
    ListRelease(pool->jobs,NULL);

    Free(pool->workers);
    Free(sem);
    Free(pool);
  }
}
