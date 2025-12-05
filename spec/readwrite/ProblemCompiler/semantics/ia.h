/** @file 
 * @brief Header for ia.c
 * @author Alexander Weber (a.weber@unibw.de)
*/
#ifndef IA_H
#define IA_H
#define NUM_OF_ATTEMPTS_APRIORI 10000

#include "mpfi.h"

#include "ast.h"
#include "task.h"
#include "variables.h"

double mpfi_get_right_d(mpfi_t);
double mpfi_get_left_d(mpfi_t);

double get_mid_of_expression(Expression, ConstSequenceOfStates);
double get_upperbound_of_expression(Expression, ConstSequenceOfStates);
double get_lowerbound_of_expression(Expression, ConstSequenceOfStates);

mpfi_t *evaluate_expression(Expression in, mpfi_t ** arg, dim_t * abs_dim, ConstSequenceOfStates);
void evaluate_function(ConstSequenceOfStates S);
void check_existence_of_solution(struct ASTNode *, struct task *, ConstSequenceOfStates);
int is_Expression_subset_Expression(Expression, Expression, ConstSequenceOfStates);
#endif
