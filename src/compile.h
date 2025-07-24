#ifndef COMPILE_H
#define COMPILE_H

#include "aostr.h"
#include "cli.h"
#include "cctrl.h"

int compileToAst(Cctrl *cc, CliArgs *args, int lexer_flags);
void compileToTokens(Cctrl *cc, CliArgs *args, int lexer_flags);

AoStr *compileToAsm(Cctrl *cc);
void compileAssembleToFile(AoStr *asmbuf, char *filename);
void compilePrintAst(Cctrl *cc);


#endif // !COMPILE_H
