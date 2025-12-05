#include "errorcodes.h"

char *err_messages[] =
{
"Can't obtain enough memory to solve this problem.",
"Not a correct keyword line.",
"Line describing graph incorrect.",
"Bad number of target nodes.",
"Bad target node.",
"Bad line.",
"Bad number of h-edge.",
"Input line too long.",
"Bad h-edge label or h-edge weight.",
"Bad number of target nodes in h-edge.",
"Bad target node in h-edge.",
"Too few h-edges found.",
"Bad problem name."
};

void parse_error(int err_index)
{
(void) fprintf(stderr, "parse_hgraph:%s: %s\n", (err_index & ERROR_BIT) ? "ERROR":"WARNING", err_messages[(err_index % ERROR_BIT) - 1]);

if(ERROR_BIT & err_index) exit(1);
}
