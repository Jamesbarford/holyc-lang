#ifndef LSP_H__
#define LSP_H__

#include "cli.h"

/* `hcc -lsp`: a Language Server Protocol server over stdio.
 * 0 after a clean shutdown request, 1 otherwise. */
int lspRun(CliArgs *args);

#endif
