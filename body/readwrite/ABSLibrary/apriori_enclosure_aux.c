/**
 * @file apriori_enclosure_aux.c
 * @author Alexander Weber (weber13@hm.edu)
 */

#include <stdlib.h>
#include <math.h>
#include "interval_library.h"

#define SVN_AUTHOR    "$Author: Weber_A $"
#define SVN_REVISION  "$Revision: 2354 $"
#define SVN_DATE      "$Date: 2021-09-23 09:42:25 +0200 (Do, 23 Sep 2021) $"

#define NUM_OF_ATTEMPTS_APRIORI 10000 /**< Number of candidate sets to test for enclosure */

/** All possible results of the algorithm to compute enclosures */
enum status { FOUND, /**< An enclosure is successfully computed */
              NOT_FOUND, /**< The algorithm can't compute an enclosure although all evaluations are performed regularily */
              RHS_NAN, /**< The algorithm fails: The right hand side of the dynamics includes a NAN value */
              RHS_UNBOUNDED, /**< The algorithm fails: The right hand side of the dynamics is unbounded or the candidate set is unbounded */
              NO_MEM /**< The algorithm fails: Memory can't be allocated. */
            };

enum ae_time { BACKWARD, /** The enclosure is computed backward in time */
               FORWARD /** The enclosure is computed forward in time */
             };


extern void f ( interval_t * y, interval_t * v, interval_t * x, interval_t * u ); /**< Interval implementation of the right hand side of the dynamics */
extern unsigned int number_of_variables; /**< Auxiliary parameter to evaluate right hand side of the dynamics */

/** @brief If \p op represents \f$[a,b]\f$, \f$a,b \in \mathbb{R}\f$, \f$a\leq b\f$ then
 * the function returns \f$b'\f$ where \f$b'\f$ is the smallest double number
 * satisfying \f$b'\geq b\f$.
*/
double mpfi_get_right_d ( mpfi_t op )
{
    double out;
    mpfr_t a;
    mpfr_init ( a );
    mpfi_get_right ( a, op );
    out = mpfr_get_d ( a, MPFR_RNDU );
    mpfr_clear ( a );
    return out;
}

/** @brief If \p op represents \f$[a,b]\f$, \f$a,b \in \mathbb{R}\f$, \f$a\leq b\f$ then
 * the function returns \f$a'\f$ where \f$a'\f$ is the largest double number
 * satisfying \f$a'\leq a\f$.
*/

double mpfi_get_left_d ( mpfi_t op )
{
    double out;
    mpfr_t a;
    mpfr_init ( a );
    mpfi_get_left ( a, op );
    out = mpfr_get_d ( a, MPFR_RNDD );
    mpfr_clear ( a );
    return out;
}

/** @brief Returns an upper bound to a sum of two numbers (by evaluating a + b) assuming that a and b are at most one ULP smaller than the mathematical values for a and b.
 * Note that a call of this function might be time consuming.
 */
double addition_RU( double a, double b)
{
  interval_t ia;
  mpfi_init ( ia );
  interval_t ib;
  mpfi_init ( ib );
  IA_SET_INTERVAL(ia, a, nextafter(a, a + 1.));
  IA_SET_INTERVAL(ib, b, nextafter(b, b + 1.));
  mpfi_add(ia, ia, ib);
  double retval = mpfi_get_right_d( ia );
  mpfi_clear ( ia );
  mpfi_clear ( ib );
  return retval;
}

/** @brief Returns a lower bound the difference of two numbers (by evaluating a - b) under the same assumptions as for addition_RU and additionally that b is non-negative.
 */
double subtraction_RD( double a, double b)
{
  interval_t ia;
  mpfi_init ( ia );
  interval_t ib;
  mpfi_init ( ib );
  IA_SET_INTERVAL(ia, nextafter(a, a - 1.), a);
  IA_SET_INTERVAL(ib, b, nextafter(b, b + 1.)); 
  mpfi_sub(ia, ia, ib);
  double retval = mpfi_get_left_d( ia );
  mpfi_clear ( ia );
  mpfi_clear ( ib );
  return retval;
}

/** @brief This method computes an apriori enclosure derived from R. Lohner \cite Lohner88, Satz 1.2.2.8 on p. 29, also cited in \cite CorlissRihm96 (Th. 1).
 *@param[out] enclosure The resulting hyper-interval in endpoint representation, i.e. the first half of the array defines the lower endpoint and the second half the upper endpoint of the hyper-interval
 *@param[in] sampling_time The sampling time. It must be positive
 *@param[in] sign_of_time One of BACKWARD or FORWARD.
 *@param[in] operating_range The hyper-interval of initial values in endpoint representation
 *@param[in] control_range The hyper-interval of controls in endpoint representation
 *@param[in] uncertainties The vector of uncertainties to the right hand side of the dynamics. All entries must be non-negative
 *@param[in] state_space_dim It must be the length of \p uncertainties and half the length of \p enclosure and \p operating_range
 *@param[in] input_space_dim It must be half the length of \p control_range
 * 
 *
 * [Lohner88] R. Lohner, Einschliessung der Loesung gewoehnlicher Anfangs- und Randwertaufgaben und Anwendungen, 1988.
 * [CorlissRihm96] G. F. Corliss, R. Rihm, Validating an A Priori Enclosure Using High-Order Taylor Series, 1996.
 *
*/
static unsigned int apriori_enclosure_core ( double *enclosure,
        double sampling_time,
        enum ae_time sign_of_time,
        const double *operating_range,
        const double *control_range,
        const double *uncertainties,
        unsigned int state_space_dim,
        unsigned int input_space_dim )
{
    unsigned int retval = NOT_FOUND;
    char found = 0;
    unsigned int num_of_attempts = 0;
    const unsigned int n = state_space_dim;
    const unsigned int m = input_space_dim;

    /* Allocate required memory */
    interval_t * const x = malloc ( n * sizeof ( interval_t ) );
    interval_t * const u = malloc ( m * sizeof ( interval_t ) );
    interval_t * const x0 = malloc ( n * sizeof ( interval_t ) );
    interval_t * const xa = malloc ( n * sizeof ( interval_t ) );
    interval_t * const fx = malloc ( n * sizeof ( interval_t ) );

    interval_t * const v = malloc ( number_of_variables * sizeof ( interval_t ) ); /* Auxiliary variable for evaluating the right hand side */
    /* Verify that allocation was successful */
    if ( x == NULL || u == NULL || x0 == NULL || xa == NULL || fx == NULL || v == NULL )
    {
        return NO_MEM;
    }

    /* Initialize variables */
    unsigned int i;
    for ( i = 0; i < n; i++ )
    {
        mpfi_init ( x[i] );
        mpfi_init ( x0[i] );
        mpfi_init ( xa[i] );
        mpfi_init ( fx[i] );
        IA_SET_INTERVAL ( x[i], operating_range[i], operating_range[i + n] );
        IA_SET_INTERVAL ( x0[i], operating_range[i], operating_range[i + n] );
    }
    for ( i = 0; i < m; i++ )
    {
        mpfi_init ( u[i] );
        IA_SET_INTERVAL ( u[i], control_range[i],control_range[i + m] );
    }
    for ( i = 0; i < number_of_variables; i++ )
    {
        mpfi_init ( v[i] );
    }

    /* Declare and initialize to more one-dimensional intervals */
    interval_t tmp;
    mpfi_init ( tmp );
    /* Interval for the sampling time */
    interval_t time;
    mpfi_init ( time );
    IA_SET_INTERVAL ( time, 0, sampling_time );


    /* THE ALGORITHM: The algorithm is derived from \cite Lohner88, Satz 1.2.2.8 on p. 29, also cited in \cite CorlissRihm95 (Th. 1) */
    while ( !found && num_of_attempts < NUM_OF_ATTEMPTS_APRIORI )
    {
        num_of_attempts++;
        /* Evaluation of the continuous-time dynamics */
        f ( fx, v, x, u );
        switch ( sign_of_time )
        {
        case BACKWARD:
            /* Multiply right hand side of dynamics by -1 in case of backward time */
            for ( i = 0; i < n; i++ )
            {
                mpfi_mul_si ( fx[i], fx[i], -1l );
            }
            break;
        case FORWARD:
            break;
        }
        /* Add the uncertainties to right hand side and test for boundedness */
        for ( i = 0; i < n; i++ )
        {
            /* Enlarge image of right hand side by the uncertainties in each component */
            mpfi_interv_d ( tmp, -uncertainties[i], uncertainties[i] );
            mpfi_add ( fx[i], fx[i], tmp );
            /* Tests for boundedness */
            if ( mpfi_nan_p ( fx[i] ) )
            {
                retval = RHS_NAN;
                goto exit;
            }
            if ( !mpfi_bounded_p ( fx[i] ) )
            {
                retval = RHS_UNBOUNDED;
                goto exit;
            }
        }
        /* Implementation of the condition \cite Lohner88 Eq. 1.2.2.9 */
        for ( i = 0; i < n; i++ )
        {
            /* Save right hand side of \cite Lohner88 Eq. 1.2.2.9 */
            mpfi_set ( xa[i], x[i] );
            /* Calculate left hand side of \cite Lohner88 Eq. 1.2.2.9 */
            mpfi_mul ( tmp, fx[i], time ); /* Multiplication in \cite Lohner88 Eq. 1.2.2.9 */
            mpfi_add ( tmp, tmp, x0[i] ); /* Addition in \cite Lohner88 Eq. 1.2.2.9 */
            mpfi_set ( x[i], tmp ); /* Save left hand side of \cite Lohner88 Eq. 1.2.2.9, which is the next candidate for testing if this iteration is not successful */

            /* Verify that the candidate is still a bounded hyper-interval */
            if ( !mpfi_bounded_p ( x[i] ) )
            {
                retval = RHS_UNBOUNDED;
                goto exit;
            }
            /* The inclusion test in \cite Lohner88 Eq. 1.2.2.9, for each component */
            if ( mpfi_is_inside ( x[i], xa[i] ) == 0 )
            {
                break;
            }
        }
        if ( i == n ) /* Inclusion test successful */
        {
            found = 1; /* Stop the while-loop */
        }
    } /* end while */
    if ( found )
    {
        retval = FOUND;
        /* Write the enclosure to the output variable */
        for ( i = 0; i < n; i++ )
        {
            enclosure[i] = mpfi_get_left_d ( x[i] ); /* Lower bound */
            enclosure[i + n] = mpfi_get_right_d ( x[i] ); /* Upper bound */
        }
    }
    else
    {
        retval = NOT_FOUND;
    }
exit:
    /* Clear the memory */
    mpfi_clear ( tmp );
    mpfi_clear ( time );
    for ( i = 0; i < n; i++ )
    {
        mpfi_clear ( x[i] );
        mpfi_clear ( xa[i] );
        mpfi_clear ( x0[i] );
        mpfi_clear ( fx[i] );
    }
    for ( i = 0; i < m; i++ )
    {
        mpfi_clear ( u[i] );
    }
    for ( i = 0; i < number_of_variables; i++ )
    {
        mpfi_clear ( v[i] );
    }
    free ( x );
    free ( u );
    free ( x0 );
    free ( xa );
    free ( fx );
    free ( v );
    return retval;
}

/** @brief Wrapper function to compute an enclosure forward in time. For arguments see description of apriori_enclosure_core */
unsigned int apriori_enclosure ( double *enclosure,
                                 double sampling_time,
                                 const double *operating_range,
                                 const double *control_range,
                                 const double *uncertainties,
                                 unsigned int state_space_dim,
                                 unsigned int input_space_dim )
{
    return apriori_enclosure_core ( enclosure, sampling_time, FORWARD, operating_range, control_range, uncertainties, state_space_dim, input_space_dim );
}

/** @brief Wrapper function to compute an enclosure backward in time. For arguments see description of apriori_enclosure_core */
unsigned int apriori_enclosure_backward ( double *enclosure,
        double sampling_time,
        const double *operating_range,
        const double *control_range,
        const double *uncertainties,
        unsigned int state_space_dim,
        unsigned int input_space_dim )
{
    return apriori_enclosure_core ( enclosure, sampling_time, BACKWARD, operating_range, control_range, uncertainties, state_space_dim, input_space_dim );
}
