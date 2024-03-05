#ifndef COMPILE_H
#define COMPILE_H

#include "aostr.h"
#include "cctrl.h"

int CompileToToAst(Cctrl *cc, char *file_path, int lexer_flags);
aoStr *CompileToAsm(Cctrl *cc);
void CompileAssembleToFile(aoStr *asmbuf, char *filename);
void CompilePrintTokens(Cctrl *cc);
void CompilePrintAst(Cctrl *cc);
int CompileToAst(Cctrl *cc, char *entrypath, int lexer_flags);

#endif // !COMPILE_H
