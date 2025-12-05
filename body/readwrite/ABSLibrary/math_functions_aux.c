/**
 * @file math_functions_aux.c
 * @author Alexander Weber (weber13@hm.edu)
 */
#include <stdio.h>
#include <math.h>
#include <mpfr.h>

#define MPFR_PREC 54

#define SVN_AUTHOR    "$Author: Weber_A $"
#define SVN_REVISION  "$Revision: 2380 $"
#define SVN_DATE      "$Date: 2021-10-22 23:01:27 +0200 (Fr, 22 Okt 2021) $"

double ABS_atan(double x)
{
 static mpfr_t mx,my;
 static char init = 0;
 if( init == 0 )
 {
  mpfr_set_default_prec (MPFR_PREC);
  mpfr_init(mx);
  mpfr_init(my); 
  init = 1;
 }
 mpfr_set_d(mx,x,MPFR_RNDN);
 mpfr_atan(my,mx,MPFR_RNDN);
 return mpfr_get_d(my, MPFR_RNDN);  
}

double ABS_cos(double x)
{
 static mpfr_t mx,my;
 static char init = 0;
 if( init == 0 )
 {
  mpfr_set_default_prec (MPFR_PREC);
  mpfr_init(mx);
  mpfr_init(my); 
  init = 1;
 }
 mpfr_set_d(mx,x,MPFR_RNDN);
 mpfr_cos(my,mx,MPFR_RNDN);
 return mpfr_get_d(my, MPFR_RNDN);  
}

double ABS_cosh(double x)
{
 static mpfr_t mx,my;
 static char init = 0;
 if( init == 0 )
 {
  mpfr_set_default_prec (MPFR_PREC);
  mpfr_init(mx);
  mpfr_init(my); 
  init = 1;
 }
 mpfr_set_d(mx,x,MPFR_RNDN);
 mpfr_cosh(my,mx,MPFR_RNDN);
 return mpfr_get_d(my, MPFR_RNDN);  
}

double ABS_exp(double x)
{
 static mpfr_t mx,my;
 static char init = 0;
 if( init == 0 )
 {
  mpfr_set_default_prec (MPFR_PREC);
  mpfr_init(mx);
  mpfr_init(my); 
  init = 1;
 }
 mpfr_set_d(mx,x,MPFR_RNDN);
 mpfr_exp(my,mx,MPFR_RNDN);
 return mpfr_get_d(my, MPFR_RNDN);  
}

double ABS_log(double x)
{
 static mpfr_t mx,my;
 static char init = 0;
 if( init == 0 )
 {
  mpfr_set_default_prec (MPFR_PREC);
  mpfr_init(mx);
  mpfr_init(my); 
  init = 1;
 }
 mpfr_set_d(mx,x,MPFR_RNDN);
 mpfr_log(my,mx,MPFR_RNDN);
 return mpfr_get_d(my, MPFR_RNDN);  
}

double ABS_sin(double x)
{
 static mpfr_t mx,my;
 static char init = 0;
 if( init == 0 )
 {
  mpfr_set_default_prec (MPFR_PREC);
  mpfr_init(mx);
  mpfr_init(my); 
  init = 1;
 }
 mpfr_set_d(mx,x,MPFR_RNDN);
 mpfr_sin(my,mx,MPFR_RNDN);
 return mpfr_get_d(my, MPFR_RNDN);  
}

double ABS_sinh(double x)
{
 static mpfr_t mx,my;
 static char init = 0;
 if( init == 0 )
 {
  mpfr_set_default_prec (MPFR_PREC);
  mpfr_init(mx);
  mpfr_init(my); 
  init = 1;
 }
 mpfr_set_d(mx,x,MPFR_RNDN);
 mpfr_sinh(my,mx,MPFR_RNDN);
 return mpfr_get_d(my, MPFR_RNDN);  
}

double ABS_tan(double x)
{
 static mpfr_t mx,my;
 static char init = 0;
 if( init == 0 )
 {
  mpfr_set_default_prec (MPFR_PREC);
  mpfr_init(mx);
  mpfr_init(my); 
  init = 1;
 }
 mpfr_set_d(mx,x,MPFR_RNDN);
 mpfr_tan(my,mx,MPFR_RNDN);
 return mpfr_get_d(my, MPFR_RNDN);  
}
