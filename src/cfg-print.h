#ifndef __CFG_PRINT_H
#define __CFG_PRINT_H

#include "cfg.h"
#include "containers.h"

void cfgToFile(CFG *cfg, char *filename);
void cfgsToFile(Vec *cfgs, char *filename);

#endif
