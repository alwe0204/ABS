#ifndef INTERVAL_LIBRARY_H
#define INTERVAL_LIBRARY_H

#include "mpfi.h"

typedef mpfi_t interval_t;

#define IA_INIT(X)              mpfi_init((X))

#define IA_SET(X,Y)             mpfi_set((X),(Y))
#define IA_SET_NUM_DEC(X,Y)     mpfi_set_d((X),(Y))
#define IA_SET_INTERVAL(X,Y,Z)  mpfi_interv_d((X),(Y),(Z))
#define IA_ATAN(X,Y)            mpfi_atan((X),(Y))
#define IA_SIN(X,Y)             mpfi_sin((X),(Y))
#define IA_COS(X,Y)             mpfi_cos((X),(Y))
#define IA_SINH(X,Y)            mpfi_sinh((X),(Y))
#define IA_COSH(X,Y)            mpfi_cosh((X),(Y))
#define IA_TAN(X,Y)             mpfi_tan((X),(Y))
#define IA_PLUS(X,Y,Z)          mpfi_add((X),(Y),(Z))
#define IA_MINUS(X,Y,Z)         mpfi_sub((X),(Y),(Z))
#define IA_TIMES(X,Y,Z)         mpfi_mul((X),(Y),(Z))
#define IA_DIV(X,Y,Z)           mpfi_div((X),(Y),(Z))
#define IA_SQR(X,Y)             mpfi_sqr((X),(Y))
#define IA_SQRT(X,Y)            mpfi_sqrt((X),(Y))
#define IA_LN(X,Y)              mpfi_log((X),(Y))
#define IA_EXP(X,Y)             mpfi_exp((X),(Y))
#define IA_POW(X,Y,Z)         { IA_LN((X),(Y)) ; IA_TIMES((X),(X),(Z)) ; IA_EXP((X),(X)); }
#define IA_SET_NEG(X,Y)         mpfi_neg((X),(Y))
#define IA_SET_ZERO(X)          mpfi_set_ui((X),0)

#endif
