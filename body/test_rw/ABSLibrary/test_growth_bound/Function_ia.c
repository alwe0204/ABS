#include "Function_ia.h"
#include "interval_library.h"

void f(interval_t *y,interval_t *v, interval_t *x, interval_t *u)
{
IA_SET(v[0],x[1]);
IA_SET(v[1],v[0]);
IA_SET(y[0],v[1]);
IA_SET(v[2],x[0]);
IA_SIN(v[3],v[2]);
IA_SET_NEG(v[4],v[3]);
IA_COS(v[5],v[2]);
IA_SET(v[6],u[0]);
IA_TIMES(v[7],v[5],v[6]);
IA_MINUS(v[8],v[4],v[7]);
IA_SET_NUM_DEC(v[9],2.0);
IA_SET_INTERVAL(v[10],0x1.9999999999999p-7,0x1.9ce075f6fd22p-7);
IA_TIMES(v[11],v[9],v[10]);
IA_TIMES(v[12],v[11],v[0]);
IA_MINUS(v[13],v[8],v[12]);
IA_SET(y[1],v[13]);

}
