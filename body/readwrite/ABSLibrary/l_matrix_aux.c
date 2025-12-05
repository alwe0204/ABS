#include "interval_library.h"
#include <stdlib.h>
#include <stdio.h>


#define SVN_AUTHOR    "$Author: lf3eelma $"
#define SVN_REVISION  "$Revision: 2085 $"
#define SVN_DATE      "$Date: 2020-07-01 13:42:13 +0200 (Mi, 01 Jul 2020) $"

/* extern functions and variables */
extern void Jacobian(interval_t **cx,interval_t *v,interval_t *x0,interval_t *u);

extern unsigned int number_of_variables_Jacobian;

/* a series of local subfunctions */
static double mpfi_get_right_d ( mpfi_t op )
{
  double out;
  mpfr_t v;
  mpfr_init ( v );
  
  mpfi_get_right ( v, op );
  out = mpfr_get_d ( v, MPFR_RNDU );
  
  mpfr_clear ( v );
  return out;
}
static double mpfi_get_left_d ( mpfi_t op )
{
  double out;
  mpfr_t v;
  mpfr_init ( v );
  
  mpfi_get_left ( v, op );
  out = mpfr_get_d ( v, MPFR_RNDD );
  
  mpfr_clear ( v );
  return out;
}
double DjFi_others ( mpfi_t op )
{
  double left, right;
  left = mpfi_get_left_d ( op );
  right = mpfi_get_right_d ( op );

  if ( left + right >= 0 )
    return right;
  else
    return -left;
}


/* main function */
void l_matrix ( double * const l,
		const double* const state_min,
		const double* const state_max,
		const double* const input,
		const double* const input_radius,
		unsigned int     input_dim,
		unsigned int     state_dim)
{
  // allocate
  //  printf("%d ,%d ",state_dim,input_dim);
  mpfi_t* x = malloc ( state_dim * sizeof ( mpfi_t ) );
  mpfi_t* u = malloc ( input_dim * sizeof ( mpfi_t ) );
  mpfi_t* v = malloc ( number_of_variables_Jacobian * sizeof( mpfi_t ) );
  mpfi_t** lmatrix = malloc ( state_dim * sizeof ( mpfi_t* ) );
  int i;
  for ( i=0; i < state_dim; i++ )
    {
      lmatrix[i] = malloc ( state_dim * sizeof ( mpfi_t ) );
    }

  // initialize
  int j;
  for ( i=0; i < state_dim; i++ )
    {
      mpfi_init ( x[i] );
      for ( j=0; j < state_dim; j++ )
	{
	  mpfi_init ( lmatrix[i][j] );
	}
    }

  for ( i=0; i < input_dim; i++ )
    {
      mpfi_init ( u[i] );
    }

  for ( i=0; i < number_of_variables_Jacobian ; i++ )
    {
      mpfi_init ( v[i] );
    }
  
  // set intervals
  for ( i=0; i < state_dim; i++ )
    {
      mpfi_interv_d ( x[i], state_min[i], state_max[i] );
    }

  for ( i=0; i < input_dim; i++ )
    {
      mpfi_interv_d ( u[i], input[i]+input_radius[i], input[i]-input_radius[i] );
    }
  
  // compute lmatrix using "Jacobian"
  Jacobian (lmatrix, v, x, u);
  
  // write result to the output
  for ( i = 0; i < state_dim; i++ ) {
    for ( j = 0; j < state_dim; j++) {
      if ( i == j )
	l[i*state_dim+j] = mpfi_get_right_d ( lmatrix[i][j] );
      else
	l[i*state_dim+j] = DjFi_others ( lmatrix[i][j] );
    }
  }
  
}

void l_b_matrix ( double * const l,
		const double* const state_min,
		const double* const state_max,
		const double* const input,
		const double* const input_radius,
		unsigned int     input_dim,
		unsigned int     state_dim)
{
  // allocate
  //  printf("%d ,%d ",state_dim,input_dim);
  mpfi_t* x = malloc ( state_dim * sizeof ( mpfi_t ) );
  mpfi_t* u = malloc ( input_dim * sizeof ( mpfi_t ) );
  mpfi_t* v = malloc ( number_of_variables_Jacobian * sizeof( mpfi_t ) );
  mpfi_t** lmatrix = malloc ( state_dim * sizeof ( mpfi_t* ) );
  int i;
  for ( i=0; i < state_dim; i++ )
    {
      lmatrix[i] = malloc ( state_dim * sizeof ( mpfi_t ) );
    }

  // initialize
  int j;
  for ( i=0; i < state_dim; i++ )
    {
      mpfi_init ( x[i] );
      for ( j=0; j < state_dim; j++ )
	{
	  mpfi_init ( lmatrix[i][j] );
	}
    }

  for ( i=0; i < input_dim; i++ )
    {
      mpfi_init ( u[i] );
    }

  for ( i=0; i < number_of_variables_Jacobian ; i++ )
    {
      mpfi_init ( v[i] );
    }
  
  // set intervals
  for ( i=0; i < state_dim; i++ )
    {
      mpfi_interv_d ( x[i], state_min[i], state_max[i] );
    }

  for ( i=0; i < input_dim; i++ )
    {
      mpfi_interv_d ( u[i], input[i]+input_radius[i], input[i]-input_radius[i] );
    }
  
  // compute lmatrix using "Jacobian"
  Jacobian (lmatrix, v, x, u);
  
  // write result to the output
  for ( i = 0; i < state_dim; i++ ) {
    for ( j = 0; j < state_dim; j++) {
      if ( i == j )
	l[i*state_dim+j] = - mpfi_get_left_d ( lmatrix[i][j] );
      else
	l[i*state_dim+j] = DjFi_others ( lmatrix[i][j] );
    }
  }
  
}



