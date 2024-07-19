#ifndef __CFG_PRINT_H
#define __CFG_PRINT_H

#include "aostr.h"
#include "cfg.h"
#include "map.h"

void cfgToFile(CFG *cfg, char *filename);
void cfgsToFile(PtrVec *cfgs, char *filename);

#endif
