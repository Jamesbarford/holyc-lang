#ifndef IR_OPTIMISER_H__
#define IR_OPTIMISER_H__

#include "aostr.h"
#include "map.h"

AoStr *irLivenessAnalysisToString(IrLivenessAnalysis *analysis);
Map *irLivenessAnalysis(IrFunction *func);
int irInstrHasSideEffects(IrInstr *instr);
int irFnCallReturnUsed(IrBlock *block, IrLivenessInfo *info, IrInstr *instr);
AoStr *irMemLocation(IrValue *value);
void irReplaceAllUses(IrFunction *func, IrValue *old_value, IrValue *new_value);
void irBlockRemoveNops(IrBlock *block);
int irCanPromoteAlloca(IrFunction *func, IrInstr *instr);
int irStoreIsReadBefore(IrFunction *func, IrBlock *block, IrInstr *instr);
int irInstrLive(IrBlock *block, IrLivenessInfo *info, IrInstr *instr);

void irOptimiseFunction(IrFunction *func);

/* Optimisation passese */
void irEliminateRedundantLoads(IrFunction *func);
void irEliminateRedundantStores(IrFunction *func);
void irEliminateDeadStores(IrFunction *func);
void irEliminateDeadAllocas(IrFunction *func);
void irPerformLoadStoreForwarding(IrFunction *func);
void irPerformCopyPropagation(IrFunction *func);
void irFoldParameterLoads(IrFunction *func);
Map *irEliminateDeadCode(IrFunction *func, Map *liveness);

#endif
