#include <stdio.h>
#include <stdlib.h>
#include "interval_library.h"

extern const unsigned int number_of_variables;
extern void last_plus_1(interval_t *cx,interval_t *v0, interval_t *x0, interval_t *u);

int main(int argc, char *argv[])
{ 
    double initval[6] = {1.2,1.2,1.2,1.2,1.2,1.2};
    mpfr_t result;
    double result_u[6];
    double result_l[6];
    double math_result_l[6] = {-40.561765,91.023050,1282.452752,-3626.103196,-1.0E-16,-1.0E-16};
    double math_result_u[6] = {-40.561764,91.023051,1282.452753,-3626.103195,1.0E-16,1.0E-16};
    double u0[1] = {0.5};
    int n = 6;
    int m = 1;
    interval_t *x = malloc(n * sizeof(interval_t));
    interval_t *u = malloc(m * sizeof(interval_t));
    interval_t *cx = malloc(n * sizeof(interval_t));
    interval_t *v = malloc(number_of_variables * sizeof(interval_t));


    unsigned int i;

    for (i = 0; i < n; i++) {
	mpfi_init_set_d(x[i],initval[i]);
	mpfi_init(cx[i]);
     
    }
    mpfr_init(result);
    for (i = 0; i < m; i++) {
	mpfi_init_set_d(u[i],u0[i]);
    }
    for (i = 0; i < number_of_variables; i++) {
	mpfi_init(v[i]);
    }
    last_plus_1(cx,v,x,u);
    for (i = 0; i < n; i++) {
     mpfi_get_right(result,cx[i]);
     result_u[i] = mpfr_get_d(result, MPFR_RNDU);
     mpfi_get_left(result,cx[i]);
     result_l[i] = mpfr_get_d(result, MPFR_RNDD);
    }
    for(i = 0;i< n; i++)
    {
      if( result_l[i] < math_result_l[i]) {  
       exit(EXIT_FAILURE);
      }
      if( result_u[i] > math_result_u[i]) {
       exit(EXIT_FAILURE); }
    }
    return 0;
}
