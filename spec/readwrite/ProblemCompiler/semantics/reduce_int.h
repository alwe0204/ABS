/**
 * @file reduce_int.h
 * @author Alexander Weber
 * @date 2 Feb 2016
 * @brief Header for reduce_int.c 
*/
#ifndef REDUCE_INT_H
#define REDUCE_INT_H

#include "ast.h"
#include "variables.h"


int compute_int_expression(int *result, enum expression_type type,
			   Expression op1, Expression op2, unsigned int line);
int expression_to_int(Expression, ConstSequenceOfStates);
int reduce_expression_to_int(Expression, ConstSequenceOfStates);
int get_int_from_expression(Expression, ConstSequenceOfStates);
void get_indices_of_PostFixExpression(PostFixExpression, dim_t * row,
				      dim_t * col, int epsilon, ConstSequenceOfStates);
#endif
