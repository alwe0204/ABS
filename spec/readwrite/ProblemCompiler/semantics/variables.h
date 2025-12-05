/** @file
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 04.02.2016 
 * @brief Header for variables.c
*/

#ifndef VARIABLES_H
#define VARIABLES_H

#include "ast.h"

typedef unsigned int date_t;				 /**< This is the type of the sequence index */
typedef struct states *SequenceOfStates;	/* For hiding the data structure (but, in general, bad practice!) */
typedef const struct states *ConstSequenceOfStates;	/* This is required as const and typedef interact in a bad way */
typedef struct variable *Variable;
typedef const struct variable *ConstVariable;

SequenceOfStates install_SequenceOfStates(void);

Variable find_in_variables_from_string(const char *, ConstSequenceOfStates);
Variable find_in_variables(PostFixExpression, ConstSequenceOfStates);
Variable find_in_variables_check_declared(PostFixExpression, ConstSequenceOfStates);
Variable find_in_variables_check_undeclared(PostFixExpression, ConstSequenceOfStates);

Variable find_in_variables_match_role_at_date(enum set_R role, date_t, ConstSequenceOfStates);
Variable find_in_variables_match_role_idx_at_date(enum set_R role,
						  dim_t idx, date_t, ConstSequenceOfStates);

Variable version_of_variable(Variable, dim_t row, dim_t col, date_t);



Variable IsEntry(PostFixExpression, ConstSequenceOfStates);

dim_t row_dim_of(ConstVariable);
dim_t col_dim_of(ConstVariable);

Expression data_of(ConstVariable, dim_t row, dim_t col);
Expression intv_of(ConstVariable, dim_t row, dim_t col);
enum set_T var_type_of(ConstVariable);
enum set_R var_role_of(ConstVariable);
void set_var_role_of(Variable, enum set_R role);
void set_var_idx(Variable, dim_t idx);

Variable get_var(size_t i, ConstSequenceOfStates);
size_t get_states_len(ConstSequenceOfStates);
dim_t var_idx(ConstVariable);
char *name_of(ConstVariable);
int is_undeclared(ConstVariable);




void write_data_1(Expression, Variable, dim_t row, dim_t col, ConstSequenceOfStates);
void write_data_2(Expression, Variable, dim_t row, dim_t col);
void write_data_in_function_head(Expression, Variable, dim_t row, dim_t col);

Variable current_state_of(PostFixExpression in, SequenceOfStates);

Variable insert_id_to_variables(Identifier in, dim_t row, dim_t col,
				enum set_T type, SequenceOfStates);
Variable insert_id_to_variables_manually(Identifier in, dim_t row,
					 dim_t col, enum set_T type,
					 enum set_R role, dim_t idx, SequenceOfStates);
Variable insert_new_version_of_variable_with_role(struct variable *x,
						  enum set_R role, SequenceOfStates);
void initialize_variable(Variable new, ConstVariable old, date_t);

void make_newstate_initialvalue(Assignment in, dim_t row, dim_t col, Variable, SequenceOfStates);
void make_variable_undeclared(Variable, ConstSequenceOfStates);


Variable check_declared(PostFixExpression, ConstSequenceOfStates);
void check_output_defined(ConstSequenceOfStates);
void check_defined(ConstVariable);
int is_PostFixExpression_read_only(PostFixExpression, ConstSequenceOfStates);
int is_var_dead(ConstVariable);

void kill_var(Variable);

void check_used_variables(ConstSequenceOfStates);

date_t get_date(ConstSequenceOfStates);
date_t get_date_of_rhs(ConstSequenceOfStates);
void increase_date(SequenceOfStates);
void set_date_of_rhs(SequenceOfStates);
#endif
