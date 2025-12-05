#include "Jacobian.h"
#include "interval_library.h"

void Jacobian(interval_t **cx,interval_t *v,interval_t *x0,interval_t *u)
{
IA_SET(v[0],x0[0]);
IA_SET(v[1],v[0]);
IA_SET(v[2],x0[1]);
IA_SET(v[3],v[2]);
IA_SIN(v[4],v[1]);
IA_SET_NEG(v[5],v[4]);
IA_COS(v[6],v[1]);
IA_SET(v[7],u[0]);
IA_TIMES(v[8],v[6],v[7]);
IA_MINUS(v[9],v[5],v[8]);
IA_SET_NUM_DEC(v[10],2.0);
IA_SET_INTERVAL(v[11],0x1.9999999999999p-7,0x1.9ce075f6fd22p-7);
IA_TIMES(v[12],v[10],v[11]);
IA_TIMES(v[13],v[12],v[3]);
IA_SET_NUM_DEC(v[14],1.0);
IA_SET(cx[0][1],v[14]);
IA_SET_NEG(v[15],v[6]);
IA_TIMES(v[16],v[5],v[7]);
IA_MINUS(v[17],v[15],v[16]);
IA_SET(cx[1][0],v[17]);
IA_SET_NEG(v[18],v[12]);
IA_SET(cx[1][1],v[18]);
IA_SET_ZERO(v[19]);
IA_SET(cx[0][0],v[19]);

}
