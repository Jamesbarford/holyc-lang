#include "./defs.HH"
#include "./memory.HC"
#include "./system.HC"
#include "./strings.HC"

/* Yup we can mix #define's and assembly */
#define ALIGN_16 0xFFFFFFFFFFFFFFF0

asm {
/* Read a file storing the length into the RDX */
_READ_FILE::
    PUSHQ   RBP
    MOVQ    RBP, RSP
    SUBQ    RSP, 48
    MOVQ    -8[RBP], RDI
    MOVQ    -16[RBP], RSI
    MOVQ    -24[RBP], RDX
    MOVQ    RDI, -16[RBP]
    CALL    _MALLOC
    MOVQ    -32[RBP], RAX
    MOVQ    RDI, -8[RBP]
    MOVQ    RSI, 0
    MOVQ    RDX, 420
    CALL    open
    MOVQ    -40[RBP], RAX
    MOVQ    RDI, -40[RBP]
    MOVQ    RSI, -32[RBP]
    MOVQ    RDX, -16[RBP]
    CALL    read
    MOVQ    -48[RBP], RAX

    MOVQ    RDX, RAX
    MOVQ    RAX, -24[RBP]
    MOVQ    [RAX], RDX
    MOVQ    RAX, -32[RBP]
    MOVQ    RCX, -48[RBP]
    ADDQ    RAX, RCX
    MOVQ    [RAX], 0
    MOVQ    RAX, -32[RBP]

    ADDQ    RSP, 48
    LEAVE
    RET

//_U64_MOD::
//    PUSHQ   RBP
//    MOVQ    RBP, RSP
//    MOVQ    RDX,0
//    CQTO
//    MOVQ    RDX,RSI
//    MOVQ    RAX,RDI
//    DIV     RBX
//    MOVQ    RAX,RDX
//    LEAVE
//    RET
}

public _extern _READ_FILE U8 *FileRead(U8 *name, I64 max_size, I64 *_len);
//public _extern _U64_MOD I64 ModU64(U64 num, U64 n);

public U64 MemAddr(U0 *ptr)
{
  U8 buffer[__BUFSIZ__];
  I64 len;
  len = StrNPrintf(buffer, __BUFSIZ__, "%p", ptr);
  buffer[len] = '\0';
  return strtoll(buffer,NULL,16);
}

U8 *SysExec(U8 *command, U64 *_captured_len=NULL,
    U8 *file_out="/tmp/capture.txt")
{ // execute a command using System and capture the output so you can do
  // something with it. Grim but useful for scripting
  U8 *full_command, *capture;
  U64 size,len;

  size = StrLen(command) * sizeof(U8) + 100;
  full_command = MAlloc(size);

  len = StrNPrintf(full_command,size,"%s > %s",command,file_out);
  full_command[len] = '\0';

  System(full_command);
  capture = FileRead(file_out,__BUFSIZ__,&len);
  if (_captured_len) {
    *_captured_len = len;
  }

  Free(full_command);
  return capture;
}
