#ifndef IR_TYPES_H__
#define IR_TYPES_H__

#include "aostr.h"
#include "types.h"
#include "containers.h"
#include "list.h"

typedef enum IrOp {
    IR_ADD, // etc...
} IrOp;

typedef struct IrInstr {
    IrOp op;
    u16 dst;
    u16 r1;
    u16 r2;
} IrInstr;

typedef struct IrBlock {
    u32 id;
    u8 removed;
    u8 sealed;
    /* List<IrInstr *>*/
    List *instructions;
} IrBlock;


typedef struct IrBlockMapping {
    u32 id; /* This block */
    Map *successors;   /* `Map<u32, IrBlock *>` */
    Map *predecessors; /* `Map<u32, IrBlock *>` */
} IrBlockMapping;

typedef struct IrFunction {
    AoStr *name;
    u16 stack_space;
    /* `List<IrBlock *>`*/
    List *blocks;
    /* `Map<u32, IrBlockMapping *>`*/
    Map *cfg;
} IrFunction;

typedef struct IrProgram {
    Vec *functions;
    Vec *globals;
} IrProgram;

typedef struct IrCtx {
    /* Current function being converted to IR */
    IrFunction *cur_func;
    /* The entrity of the program in IR */
    IrProgram *prog;

} IrCtx;

#endif
