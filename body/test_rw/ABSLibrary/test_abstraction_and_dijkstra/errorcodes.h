/* errorcodes.h
   GR, 2009
*/

#ifndef ERRORCODES_H
#define ERRORCODES_H

#ifdef TEST
#include	<assert.h>
#endif
#include	<stdio.h>
#include	<stdlib.h>

#define ERROR_BIT	            1000 /* ohne dieses bit ist's nur ein Warning */
#define NOMEM		            1001
#define BAD_KEYWORD_LINE            1002
#define BAD_HGRAPH_DESC_LINE        1003
#define BAD_NO_TARGET_NODES         1004
#define BAD_TARGET_NODE             1005
#define BAD_LINE                    1006
#define BAD_HEDGE_NUM               1007
#define LINE_TOO_LONG               1008
#define BAD_HEDGE_LABEL_OR_WEIGHT   1009
#define BAD_HEDGE_NUM_OF_TARGETS    1010
#define BAD_HEDGE_TARGET            1011
#define TOO_FEW_HEDGES_READ         1012
#define BAD_NAME                    1013


void parse_error(int err_index);

#endif
