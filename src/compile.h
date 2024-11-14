#ifndef COMPILE_H
#define COMPILE_H

#include "aostr.h"
#include "cctrl.h"

int compileToToAst(Cctrl *cc, char *file_path, int lexer_flags);
aoStr *compileToAsm(Cctrl *cc);
void compileAssembleToFile(aoStr *asmbuf, char *filename);
void compilePrintAst(Cctrl *cc);

int compileToAst(Cctrl *cc, char *entrypath, int lexer_flags);
void compileToTokens(Cctrl *cc, char *entrypath, int lexer_flags);

#endif // !COMPILE_H
