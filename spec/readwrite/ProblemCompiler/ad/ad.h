/** @file
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 01 Feb 2016
 * @brief Header for ad.c. This is the API for the automatic differentiation
*/
#ifndef AD_H
#define AD_H

#include "ast.h"
#include "task.h"
#include "variables.h"

typedef struct three_variable_list *ThreeVariableList;	/* For hiding the data structure (but, in general, bad practice!) */
typedef const struct three_variable_list *ConstThreeVariableList;	/* This is required as const and typedef interact in a bad way */
typedef size_t var3_t;

ThreeVariableList install_three_variable_list(void);
size_t get_three_variable_list_len(ConstThreeVariableList);

void init_three_variable_list_for_ode(dim_t dimension_of_ode,
				      dim_t * acc_dim_of_output_indices,
				      dim_t num_of_output_indices, ThreeVariableList);
void clean_three_variable_list(ThreeVariableList);
ThreeVariableList copy_evaluation_trace(ConstThreeVariableList);

void evaluation_trace(ThreeVariableList, ConstSequenceOfStates);
void evaluation_trace_for_ode(Variable const *states_list,
			      dim_t len, ThreeVariableList, ConstSequenceOfStates);
void make_taylor_coefficients(ThreeVariableList);
void make_Jacobian(ThreeVariableList);

void method_quardatic_in_order(dim_t order, ThreeVariableList);
void method_recursive(dim_t order, ThreeVariableList);

size_t get_number_of_variables(ConstThreeVariableList);

enum expression_type get_type_of_var(var3_t, ConstThreeVariableList);
var3_t get_left_var(var3_t, ConstThreeVariableList);
var3_t get_rght_var(var3_t, ConstThreeVariableList);
dim_t get_row_of_var(var3_t, ConstThreeVariableList);
dim_t get_col_of_var(var3_t, ConstThreeVariableList);
enum set_R get_role_of_var(var3_t, ConstThreeVariableList);
order_t get_order_of_var(var3_t, ConstThreeVariableList);
dim_t get_dim_of_var(var3_t, ConstThreeVariableList);
Expression get_value_of_var(var3_t, ConstThreeVariableList);

int is_var_zero(var3_t, ConstThreeVariableList);
int is_variable_constant(var3_t, ConstThreeVariableList);
int is_unused(var3_t, ConstThreeVariableList);
int is_used_twice(var3_t, ConstThreeVariableList);
#endif
