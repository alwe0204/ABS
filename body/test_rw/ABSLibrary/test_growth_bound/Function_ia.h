#ifndef FUNCTION_IA_H
#define FUNCTION_IA_H
#include "interval_library.h"
#define NUMBER_OF_VARIABLES 14
const unsigned int number_of_variables = NUMBER_OF_VARIABLES;
/**
* @brief The procedure evaluates the explicitly defined function with interval arithmetic
* @param y Output variable. Assumptions: y in R^(2 x 1)
* @param v Array of auxiliary variables. Assumptions: v has length 14
* @param x Input variable. Assumptions: x in (?,?) subset R^(2 x 1)
* @param u Input variable. Assumptions: u in ([-2,2]) subset R^(1 x 1)
*/
void f(interval_t *y,interval_t *v, interval_t *x, interval_t *u);
#endif
