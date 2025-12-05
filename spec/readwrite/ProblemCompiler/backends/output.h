/** @file 
 * @brief Header for output.c
 * @author Alexander Weber (a.weber@unibw.de)
*/
#ifndef OUTPUT_H
#define OUTPUT_H

#include <stdlib.h>
#include "ast.h"
#include "task.h"
#include "variables.h"
#include "ad.h"
#include "to_string.h"

void output_evaluation_trace(FILE *, FORMAT, enum number_format,
			     const struct function *,
			     ConstThreeVariableList, ConstSequenceOfStates);
void output_three_variable_list(FILE *, FORMAT, char **coeff_strings,
				const dim_t * acc_dim,
				ConstThreeVariableList, ConstSequenceOfStates);

void output_three_variable_list_entry(FILE *, FORMAT, enum number_format,
				      const char *v, var3_t i,
				      ConstThreeVariableList, ConstSequenceOfStates);
void output_evaluation_trace_variable(FILE *, FORMAT, const char *type,
				      ConstThreeVariableList, ConstSequenceOfStates);
void output_coeffients_entry(FILE *, FORMAT, var3_t i,
			     const dim_t * acc_dim, char **coeff_names,
			     const char *va, ConstThreeVariableList);

void output_taylor(FILE *, const struct ASTNode *, const struct task *,
		   ConstThreeVariableList, ConstSequenceOfStates);
void output_Jacobian(FILE *, FORMAT, char **coeff_strings, char *name,
		     const struct task *, ConstThreeVariableList, ConstSequenceOfStates);

void make_strings(struct task *, ConstSequenceOfStates);
void output_coefficients(FILE *, FORMAT, char **coeff_strings, char *name,
			 const struct task *, ConstThreeVariableList, ConstSequenceOfStates);
void output_concrete_coefficient(FILE * file, FORMAT f, const order_t in, char **coeff_strings,
		    char *name, const struct task *op,
		    ConstThreeVariableList tri, ConstSequenceOfStates S);
char *definition_of_variable_to_string(var3_t initializer, const char *v,
				       var3_t i, enum number_format,
				       FORMAT, ConstThreeVariableList, ConstSequenceOfStates);
void output_output_var(FILE *, FORMAT, var3_t i, const dim_t * acc_dim,
		       char **coeff_names, const char *va, ConstThreeVariableList);
void output_jacobian_entry(FILE *, FORMAT, var3_t i, const dim_t * acc_dim,
			   char **coeff_names, const char *va, ConstThreeVariableList);
void
output_coefficients_entry(FILE *, FORMAT, var3_t i,
			  const dim_t * acc_dim, char **coeff_names,
			  const char *va, ConstThreeVariableList);

char *var_string(const char *name, var3_t idx, FORMAT);
void output_assignment(FILE *, FORMAT, enum expression_type,
		       enum set_R role, const char *lhs, const char *rhs);


void output_three_variable_list_entry_output(FILE *,
					     FORMAT,
					     enum number_format mod,
					     const char *v,
					     const var3_t i,
					     char **out_strings,
					     const dim_t * acc_dim,
					     void (*out_fct) (FILE *,
							      FORMAT,
							      const var3_t,
							      const dim_t
							      *, char **,
							      const char *,
							      ConstThreeVariableList),
					     ConstThreeVariableList, ConstSequenceOfStates);
void output_function_comment(FILE * file, FORMAT f,
				 const struct function *in,
				 const struct task *op, ConstThreeVariableList tri, ConstSequenceOfStates S);
void output_function_image_set(FILE * file, FORMAT f, ConstSequenceOfStates S);
#endif
