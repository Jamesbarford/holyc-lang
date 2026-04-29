#ifndef CODEGEN_X86_IR_H
#define CODEGEN_X86_IR_H

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"

/* Emit assembly for a single AST_FUNC by lowering it to IR first.
 * The function must have already been accepted by irFunctionEligibleForSlice. */
void asmFunctionFromIr(Cctrl *cc, AoStr *buf, Ast *func);

#endif
