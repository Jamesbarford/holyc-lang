#ifndef AARCH64_H__
#define AARCH64_H__

#include "aostr.h"
#include "ast.h"
#include "ir-types.h"
#include "cctrl.h"

void aarch64InitRegPool(void);
void aarch64PasteDataSection(Cctrl *cc, AoStr *buf);
void aarch64PasteAsmBlocks(Cctrl *cc, AoStr *buf);
void aarch64GenerateFunction(IrCgCtx *ctx, Ast *ast);
void aarch64GlobalVar(Cctrl *cc, Set *seen_globals, AoStr *buf, Ast *ast);

#endif
