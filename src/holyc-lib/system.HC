#include "./defs.HH"

#define SYS_CALL_WRITE 0x2000004 /* Non portable */

asm {
_EXIT::
    CALL    exit

_SYSTEM::
    PUSHQ  RBP
    MOVQ   RBP, RSP
    CALL   system
    LEAVE
    RET

_WRITE::
    MOVQ    RAX, SYS_CALL_WRITE
    SYSCALL
    LEAVE
    RET
}

public _extern _EXIT U0 Exit(I64 exit_code=EXIT_FAIL);
public _extern _WRITE I64 Write(I64 fd, U8 *buf, I64 len);
public _extern _SYSTEM I64 System(U8 *command);
