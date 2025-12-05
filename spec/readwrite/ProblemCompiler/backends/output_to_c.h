/** @file 
 * @brief Header for output_to_c.c
 * @author Alexander Weber (a.weber@unibw.de)
*/
#ifndef OUTPUT_TO_C_H
#define OUTPUT_TO_C_H


#include <stdlib.h>
#include "ast.h"
#include "task.h"
#include "variables.h"
#include "ad.h"

void
output_function_in_c(FILE * file, FILE * header,
		     const struct function *,
		     const struct task *, ConstThreeVariableList, ConstSequenceOfStates);
void
output_function_in_c_ia(FILE * file, FILE * header,
			const struct function *in,
			const struct task *op, ConstThreeVariableList, ConstSequenceOfStates);
void
output_coeff_to_c_ia(FILE * file, FILE * header,
		  char **coeff_strings, const struct task *,
		  ConstThreeVariableList, ConstSequenceOfStates);
void output_coeff_to_c(FILE *, FILE *, char **coeff_strings,
		       const struct task *, ConstThreeVariableList, ConstSequenceOfStates);
void output_Jacobian_to_c(FILE *, FILE *, char **coeff_strings, char *name,
			  const struct task *, ConstThreeVariableList, ConstSequenceOfStates);
void output_taylor_to_c(FILE * file, FILE * header,
			const struct ASTNode *in,
			const struct task *op, ConstThreeVariableList tri, ConstSequenceOfStates S);
#endif
