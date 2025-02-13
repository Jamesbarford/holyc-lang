class CExceptStk;
class CExceptStk
{
  I64 fn_address;
  I64 fn_stack_address;
  I64 fn_catch_address;
  CExceptStk *next;
};

class CTask
{
  I64 except_ch;
  I64 fn_address; // on X64 this is RBP
  I64 fn_stack_address;
  Bool catch_except;
  CExceptStk *stk;
  /* Put anything you want on this - not thread safe but it will be globally 
   * accessable which is nice */
  U0 *user_data;
};

/* This is a global context object */
CTask *Fs;

CTask *CTaskNew()
{
  CTask *task = CAlloc(sizeof(CTask));
  task->stk = NULL;
  return task;
}

/* Push a function return address and stack on to our stack */
U0 CTaskPushExcept(I64 fn_address, I64 fn_stack_address, I64 fn_catch_address)
{
  CExceptStk *new_pending_exeception = MAlloc(sizeof(CExceptStk));
  new_pending_exeception->fn_address = fn_address;
  new_pending_exeception->fn_stack_address = fn_stack_address;
  new_pending_exeception->fn_catch_address = fn_catch_address;
  if (Fs->stk) {
    new_pending_exeception->next = Fs->stk;
    Fs->stk = new_pending_exeception;
  } else {
    Fs->stk = new_pending_exeception;
  }
}

CExceptStk *CTaskPopExcept(U0)
{
  if (Fs->stk) {
    CExceptStk *stk = Fs->stk;
    Fs->stk = Fs->stk->next;
    return stk;
  }
  return NULL;
}

U0 PutChars(U64 chars)
{
  auto msg = StrPrint(,"%c", chars);
  "%s\n",msg;
  Free(msg);
}

U8 *_throw_not_in_catch = "Throw called while not in a catch block\n";
U0 throw(U64 error) {
  asm {
    PUSH RBP
    MOV  RBP, RSP
    CALL PutChars
    CALL CTaskPopExcept
    CMP  RAX, 0
    JZ   @@1
    MOV  RBP, [RAX]
    MOV  RSP, 8[RAX]
    MOV  RDI, 16[RAX]
    JMP  @@2
  @@1:
    LEA  RDI, _throw_not_in_catch[RIP]
    CALL printf
    CALL _EXIT
  @@2:
    MOV RAX, RDI
    RET
  }
}

/* This has to be something the compiler adds in ASM, it can't be defined 
 * properly in user space */
U0 asm_try(U0)
{
  asm {
    MOV RBX, RDI
    MOV RDI, RBP
    MOV RSI, RSP
    CALL CTaskPushExcept
    RET
  }
}

asm {
FN2::
    PUSH RBP
    MOV  RBP, RSP
    MOV  RDI, 'fn2'
    CALL PutChars
    MOV  RDI, 'woops'
    CALL throw
    MOV  RDI, 'defno'
    CALL PutChars
    LEAVE
    RET

FN1::
    PUSH RBP
    MOV  RBP, RSP
    MOV  RDI, 'hello'
    CALL PutChars
    CALL FN2
    MOV  RDI, 'why?' // Should not be reached
    CALL PutChars
    LEAVE
    RET

_DRIVER::
    PUSH RBP
    MOV  RBP, RSP
    LEA  RDI, @@1[RIP]
    CALL asm_try
    CALL FN1
    MOV  RDI, 'end'
    CALL PutChars
@@1:
    MOV  RDI, 'caught'
    CALL PutChars
    LEAVE  // should jump here
    RET
}

_extern _DRIVER U0 Driver();

U8 *global_string = "foo2";

U0 Main()
{
 // try {        // Push RBP & RSP, set address to 
 //              //                             |   
 // } catch {    // <---- the catch down here --+
 //   "error\n"; // The Label for catch, so we can get the address, can be
 //              // Implemented as `<fn_name>catch<cnt>` and then we jump to it
 // }

  Fs = MAlloc(sizeof(CTask));
  Driver();
  throw('yeet');
  "%s\n",global_string;
}
