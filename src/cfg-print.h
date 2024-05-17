#ifndef __CFG_PRINT_H
#define __CFG_PRINT_H

#include "aostr.h"
#include "cfg.h"

void cfgToFile(CFG *cfg, const char *filename);
aoStr *cfgCreateGraphViz(CFG *cfg);

#endif
