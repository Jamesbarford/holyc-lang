#ifndef REPL_H__
#define REPL_H__

#include "cctrl.h"
#include "cli.h"

/* Run the read-eval-print loop until EOF (Ctrl-D). Returns the
 * process exit code. */
int replRun(Cctrl *cc, CliArgs *args);

#endif
