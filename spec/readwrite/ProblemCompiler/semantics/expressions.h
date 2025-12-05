/**
 * @file expressions.h
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 24 Mar 2016
 * @brief Header for expressions.c
*/
#ifndef EXPRESSIONS_H
#define EXPRESSIONS_H

#include "ast.h"
#include "variables.h"

#define    WITH_INPUT 1
#define WITHOUT_INPUT 0


Expression definition_of(PostFixExpression, ConstSequenceOfStates);
Expression interval_of(PostFixExpression, ConstSequenceOfStates);

enum set_T type_of(PostFixExpression, ConstSequenceOfStates);
dim_t idx_of(PostFixExpression, ConstSequenceOfStates);
enum set_R role_of(PostFixExpression, ConstSequenceOfStates);
int is_role_of(PostFixExpression, enum set_R, ConstSequenceOfStates);


unsigned int get_length_of_expression_list(Expression);


int is_Expression_type_number(enum expression_type);
int is_Expression_type_function(enum expression_type);
int is_Expression_type_elementary_operation(enum expression_type);


void remove_PostFixExpression_involved_in_Expression(Expression * op1,
						     PostFixExpression op2, ConstSequenceOfStates);
int is_Expression_computable(Expression, ConstSequenceOfStates, int mode);
int is_Expression_constant(Expression, ConstSequenceOfStates);
int is_PostFixExpression_Interval(PostFixExpression, ConstSequenceOfStates);
void replace_PostFixExpression_by_its_Expression(Expression * op1,
						 PostFixExpression op2, ConstSequenceOfStates);
int is_PostFixExpression_Identifier(PostFixExpression);
int is_PostFixExpression_Vector(PostFixExpression);
int is_PostFixExpression_Matrix(PostFixExpression);
int is_Expression_literal_Number(Expression);
int is_Expression_elementary_operation(Expression);
int is_Expression_function(Expression);
int is_Expression_Interval(Expression);
int is_Expression_RealSpace(Expression);
int is_Expression_Number(Expression);

int is_Expression_bounded_Interval(Expression, ConstSequenceOfStates);

void dimension(dim_t * r, dim_t * c, Expression, ConstSequenceOfStates);
Expression copy_Expression(Expression);

Expression simplify_Expression(Expression, ConstSequenceOfStates);
void check_valid_real_assignment(Assignment, ConstSequenceOfStates);

void assume_interval_bounded(Expression, char *name, unsigned int line, ConstSequenceOfStates);

dim_t get_dim_row(Expression, ConstSequenceOfStates);
dim_t *dimensions_of_expression_list(Identifier, ConstSequenceOfStates);
date_t get_date_of_postfix(PostFixExpression, ConstSequenceOfStates);

void accumulate(dim_t * vec, dim_t len);
dim_t *make_abs_dim(const dim_t *);
#endif
