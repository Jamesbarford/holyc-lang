#ifndef X86_64_H__
#define X86_64_H__

#include "aostr.h"
#include "ir-types.h"
#include "cctrl.h"

void x86_64InitRegPool(void);
void x86_64PasteDataSection(Cctrl *cc, AoStr *buf);
void x86_64PasteAsmBlocks(Cctrl *cc, AoStr *buf);
void x86_64GenerateFunction(IrCgCtx *ctx, Ast *ast);
void x86_64GlobalVar(Cctrl *cc, Set *seen_globals, AoStr *buf, Ast *ast);

#endif
