#ifndef IR_H
#define IR_H

#include "cctrl.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct IrBlock IrBlock;
typedef struct IrConnection IrConnection;
typedef struct IrCtx IrCtx;
typedef struct IrInstruction IrInstruction;
typedef struct IrModule IrModule;
typedef struct IrVar IrVar;

void irFromAst(Cctrl *cc);
aoStr *irInstructionToString(IrInstruction *inst);

#ifdef __cplusplus
};
#endif

#endif
