#ifndef JACOBIAN_H
#define JACOBIAN_H
#include "interval_library.h"
#define NUMBER_OF_VARIABLES 20
const unsigned int number_of_variables_Jacobian = NUMBER_OF_VARIABLES;
/**
* @brief This procedure evaluates the first derivative of the specified function with interval arithmetic.
* @param cx First derivative with respect to x. Assumptions: cx is a two-dimensional array of length (2,2)
* @param v Array of auxiliary variables. Assumptions: v has length 20
* @param x0 Initial value. Assumptions: x0 subset ([-0.876627,7.159813],[-2.922091,2.922091])
* @param u Parameter. Assumptions: u subset ([-2,2])
*/
void Jacobian(interval_t **cx,interval_t *v,interval_t *x0,interval_t *u);
#endif
