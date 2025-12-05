/** @file 
 * @brief Header for output_to_ada.c
 * @author Alexander Weber (a.weber@unibw.de)
*/
#ifndef OUTPUT_TO_ADA_H
#define OUTPUT_TO_ADA_H

#include <stdlib.h>
#include "ast.h"
#include "task.h"
#include "variables.h"
#include "ad.h"

#define ADA_IN_TYPE      "Vector_Float_T"
#define ADA_IN_TYPE_MAT  "Matrix_Float_T"
#define ADA_IN_TYPE_TIME "Time_T"


void output_function_in_ada(FILE * file, FILE * header,
			    const struct function *, const struct task *,
			    ConstThreeVariableList, ConstSequenceOfStates);
void output_coeff_to_ada(FILE *, FILE *, char **coeff_strings,
			 const struct task *, ConstThreeVariableList, ConstSequenceOfStates);
void output_Jacobian_to_ada(FILE *, FILE *, char **coeff_strings,
			    char *name, const struct task *,
			    ConstThreeVariableList, ConstSequenceOfStates);
void output_taylor_to_ada(FILE * file, FILE * header,
			const struct ASTNode *in,
			const struct task *op, ConstThreeVariableList tri, ConstSequenceOfStates S);
#endif
