#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mpfr.h"
#include "mpfi_io.h"

#include "interval_library.h"

#define NUM_OF_SUBDIVISIONS 2

extern void last_plus_1(mpfi_t * cx, mpfi_t * v0, mpfi_t * x0, mpfi_t * u);
extern unsigned int number_of_variables_remainder;
extern unsigned int order_remainder;

void idx_to_coordinate(unsigned int subdivision, unsigned int dim, unsigned int *x, unsigned int j)
{
    int i, k;
    unsigned int div, mod;
    for (i = dim; i--;) {
	div = 1;
	for (k = i; k--;)
	    div *= subdivision;
	mod = subdivision * div;
	x[i] = (j % mod) / div;
    }
}

mpfi_t *subdivide_interval(mpfi_t in, unsigned int num)
{
    mpfi_t *retval = NULL;
    unsigned int i, j;
    unsigned int len = 1;
    mpfi_t *arr = malloc(sizeof(mpfi_t));
    mpfi_init_set(arr[0], in);
    for (i = 1; i <= num; i++) {
	len *= 2;
	retval = malloc(len * sizeof(mpfi_t));
	for (j = 0; j < len / 2; j++) {
	    mpfi_init(retval[2 * j]);
	    mpfi_init(retval[2 * j + 1]);
	    mpfi_bisect(retval[2 * j], retval[2 * j + 1], arr[j]);
	}
     mpfi_clear(arr[0]);
	free(arr);
	arr = retval;
    }
    return retval;
}

unsigned int bounds_of_approximation_error(double *bound,
                            const double *apriori_enclosure,
					   const double *control_range,
					   double sampling_time,
                            unsigned int subdiv_state,
                            unsigned int subdiv_input,
					   unsigned int state_space_dim,
					   unsigned int input_space_dim)
{
    double *tmp;
    const unsigned int n = state_space_dim;
    const unsigned int m = input_space_dim;

    const unsigned int num_of_idx = (unsigned int) pow((1 << subdiv_state), n);

    unsigned int idx;
    unsigned int i,j;

    tmp = calloc(n,sizeof(double));
    unsigned int *k = malloc(n * sizeof(unsigned int));
    mpfi_t **subdiv = malloc(n * sizeof(mpfi_t *));

    mpfr_t tt;
    mpfr_t eb;
    mpfr_init(tt);
    mpfr_init(eb);
    mpfr_set_d(tt, nextafter(sampling_time, sampling_time + 1), MPFR_RNDU);
    mpfr_pow_ui(tt, tt, order_remainder, MPFR_RNDU);

    mpfi_t *x = malloc(n * sizeof(mpfi_t));
    mpfi_t *cx = malloc(n * sizeof(mpfi_t));
    mpfi_t *u = malloc(m * sizeof(mpfi_t));
    mpfi_t *v = malloc(number_of_variables_remainder * sizeof(mpfi_t));

    if (x == NULL || u == NULL || v == NULL) {
     return 1;
    }
    for (i = 0; i < n; i++) {
	mpfi_init(x[i]);
	mpfi_init(cx[i]);     
     IA_SET_INTERVAL(x[i], apriori_enclosure[i],apriori_enclosure[i + n]);
    }
    for (i = 0; i < m; i++) {
	mpfi_init(u[i]);
	IA_SET_INTERVAL(u[i], control_range[i], control_range[i + m]);
    }
    for (i = 0; i < number_of_variables_remainder; i++) {
	mpfi_init(v[i]);
     IA_SET_INTERVAL(v[i],0,0);
    }

    for (i = 0; i < n; i++) {
	subdiv[i] = subdivide_interval(x[i], subdiv_state);
    }
    for (idx = 0; idx < num_of_idx; idx++) {
      idx_to_coordinate(1 << NUM_OF_SUBDIVISIONS, n, k, idx);
	for (i = 0; i < n; i++) {
	   mpfi_set(x[i],subdiv[i][k[i]]);
	}
	last_plus_1(cx,v,x, u);
	for (i = 0; i < n; i++) {
	    mpfi_mag(eb, cx[i]);
	    mpfr_mul(eb, eb, tt, MPFR_RNDU);
	    tmp[i] = fmax(tmp[i], mpfr_get_d(eb, MPFR_RNDU));
	}
    }
    for (i = 0; i < n; i++) {
     bound[i] = tmp[i];
    }
/* clear the memory */
    mpfr_clear(tt); 
    mpfr_clear(eb);
    for (i = 0; i < n; i++) {
	mpfi_clear(x[i]);
	mpfi_clear(cx[i]);
    }
    for (i = 0; i < m; i++) {
	mpfi_clear(u[i]);
    }
    for (i = 0; i < number_of_variables_remainder; i++) {
	mpfi_clear(v[i]);
    }
    for (i = 0; i < n; i++) {
     for(j = 0; j < 1 << subdiv_state ; j++) 
     {
      mpfi_clear(subdiv[i][j]);
     }
     free(subdiv[i]);
    }
    free(subdiv);
    free(x);
    free(u);
    free(v);
    return 0;
}
