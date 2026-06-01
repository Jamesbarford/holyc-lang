class Coroutine
{
  U0 *rsp;
  U0 *stack_base;
  Bool is_done;
};

#define STACK_CAPACITY (635421)

extern "c" U0 *mmap(U0 *addr, U64 len, I32 prot, I32 flags, I32 fd, I32 offset);
extern "c" U0 *munmap(U0 *addr, U64 len);

static Coroutine *coroutine_stack[1000];
static I64 coroutine_sp = 0;

static U0 CoroutineStackPush(Coroutine *c)
{
  coroutine_stack[coroutine_sp++] = c;
}

static Coroutine *CoroutineStackPop()
{
  if (coroutine_sp - 1 < 0) return NULL;
  return coroutine_stack[--coroutine_sp];
}

static Coroutine *CoroutineStackLast()
{
  if (coroutine_sp == 0) return NULL;
  return coroutine_stack[coroutine_sp-1];
}

static U0 CoroutineInit()
{
  Coroutine *c = MAlloc(sizeof(Coroutine));
  MemSet(c, 0, sizeof(Coroutine));
  CoroutineStackPush(c);
}

U0 *CoroutineNext(Coroutine *c) {
asm {
    MOVL ESI, 16[RDI]
    CMP  ESI, 1
    JE   @@1
    PUSH RDI
    PUSH RBP
    PUSH RBX
    PUSH R12
    PUSH R13
    PUSH R14
    PUSH R15
    MOV RSI, RSP
    JMP CoroutineSwitchContext
@@1:
    XOR RAX, RAX
    RET
}
}

U0 CoroutineRestoreContext(U0 *_rsp)
{
asm {
    MOVQ RSP, RDI
    POP R15
    POP R14
    POP R13
    POP R12
    POP RBX
    POP RBP
    POP RDI
    RET
}
}

U0 CoroutineSwitchContext(Coroutine *c, U0 *_rsp)
{
  auto tail = CoroutineStackLast;
  if (tail) {
    tail->rsp = _rsp;
  }
  CoroutineStackPush(c);
  CoroutineRestoreContext(c->rsp);
}

U0 CoroutineRestoreContextAndReturn(U0 *_rsp, U0 *_arg)
{
asm {
    MOVQ RSP, RDI
    MOVQ RAX, RSI
    POP R15
    POP R14
    POP R13
    POP R12
    POP RBX
    POP RBP
    POP RDI
    RET
}
}

U0 *CoroutineYield(U0 *_arg)
{
asm {
    PUSH RDI
    PUSH RBP
    PUSH RBX
    PUSH R12
    PUSH R13
    PUSH R14
    PUSH R15
    MOVQ RSI, RSP
    JMP CoroutineReturn
}
}

U0 CoroutineReturn(U0 *_arg, U0 *_rsp)
{
  auto tail = CoroutineStackPop;
  if (!tail) {
    return;
  }
  tail->rsp = _rsp;
  auto next_tail = CoroutineStackLast;
  if (next_tail) {
    CoroutineRestoreContextAndReturn(next_tail->rsp, _arg);
  }
}

U0 CoroutineFinish()
{
  auto tail = CoroutineStackPop;
  if (!tail) {
    return;
  }

  tail->is_done = TRUE;
  auto next_tail = CoroutineStackLast;
  if (next_tail) {
    CoroutineRestoreContextAndReturn(next_tail->rsp, NULL);
  }
}

U0 CoroutineRelease(Coroutine *c)
{
  if (c) {
    munmap(c->stack_base, STACK_CAPACITY);
    Free(c);
  }
}

Coroutine *CoroutineNew(U0 (*cb)(U0 *_arg), U0 *arg)
{
  Coroutine *c = MAlloc(sizeof(Coroutine));
  c->is_done = 0;
  c->stack_base = mmap(NULL, STACK_CAPACITY, 0x02|0x01, 0x1|0x2, -1, 0);
  U0 **_rsp = (c->stack_base + STACK_CAPACITY)(U0 **);

  *(--_rsp) = &CoroutineFinish;
  *(--_rsp) = cb;
  *(--_rsp) = arg;
  *(--_rsp) = 0;
  *(--_rsp) = 0;
  *(--_rsp) = 0;
  *(--_rsp) = 0;
  *(--_rsp) = 0;
  *(--_rsp) = 0;
  c->rsp = _rsp;
  return c;
}

U0 Fib(U0 *arg)
{
  I64 max = arg(I64);
  I64 a = 0;
  I64 b = 1;
  while (a < max) {
    CoroutineYield(a(U0 *));
    I64 c = a + b;
    a = b;
    b = c;
  }
}

U0 Main()
{
  CoroutineInit;
  Coroutine *c = CoroutineNew(&Fib, 5(U0 *));
  for (auto it = CoroutineNext(c); !c->is_done; it = CoroutineNext(c)) {
    "%d\n",it;
  }
}
