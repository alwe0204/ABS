/** @file 
 * @brief Header for abcs.c
 * @author Alexander Weber (a.weber@unibw.de)
*/
#ifndef ABCS_H
#define ABCS_H

#include "ast.h"
#include "task.h"
#include "variables.h"
#include "to_string.h"

void output_operatingrange(FILE * file, FILE * header, FORMAT,
			   struct ASTNode *, struct task *, ConstSequenceOfStates);
void output_inputspace(FILE * file, FILE * header, FORMAT f,
		       struct ASTNode *in, struct task *op, ConstSequenceOfStates S);
void output_initialset(FILE * file, FILE * header, FORMAT,
		       struct ASTNode *, struct task *, ConstSequenceOfStates);
void output_targetset(FILE * file, FILE * header, FORMAT,
		      struct ASTNode *, struct task *, ConstSequenceOfStates);
void output_obstacleset(FILE * file, FILE * header, FORMAT,
			struct ASTNode *, struct task *, ConstSequenceOfStates);
void output_Index_Of_Dimension_function(FILE * file, FILE * header,
					FORMAT, const char *name, unsigned int number);
void output_periods(FILE * file, FILE * header, FORMAT,
		    struct ASTNode *, struct task *, ConstSequenceOfStates);
void output_uncertainties(FILE * file, FILE * header, FORMAT,
			  struct ASTNode *, struct task *, ConstSequenceOfStates);
void output_measurementerrors(FILE * file, FILE * header, FORMAT,
			      struct ASTNode *, struct task *, ConstSequenceOfStates);
void output_samplingtime(FILE * file, FILE * header, FORMAT,
			 struct ASTNode *, ConstSequenceOfStates);
void output_state_space_discretization(FILE * file, FILE * header,
				       FORMAT f, struct ASTNode *in,
				       struct task *, ConstSequenceOfStates);
void output_input_space_discretization(FILE * file, FILE * header,
				       FORMAT f, struct ASTNode *,
				       struct task *, ConstSequenceOfStates );
void output_dimension_of_spaces(FILE *file, FILE *header, FORMAT, struct task *);
void output_num_of_periods(FILE *file, FILE *header, FORMAT, struct task *);
void output_matrix_exponential_template(FILE * file,  struct task *);
void output_rounded_initial_state_radius(FILE * file, FILE * header, FORMAT f,
		    struct ASTNode *, struct task *, ConstSequenceOfStates);
void output_Bounds_of_Lipschitz_Matrices(FILE * file, FILE * header, FORMAT f,
		    struct ASTNode *, struct task *, ConstSequenceOfStates);
void output_Bounds_of_Approximation_Errors(FILE * file, FILE * header, FORMAT f,
					 struct ASTNode *, struct task *,
					 ConstSequenceOfStates);
void output_Bounds_of_Rounding_Errors(FILE * file, FILE * header, FORMAT f,
					 struct ASTNode *, struct task *,
					 ConstSequenceOfStates);
void output_Bounds_of_Summation_Errors(FILE * file, FILE * header, FORMAT f,
				      struct ASTNode *, struct task *, ConstSequenceOfStates);
void output_Bounds_of_Input_Value_Rounding_Error(FILE *file,FILE *header, FORMAT, struct ASTNode *, struct task *,
					 ConstSequenceOfStates);
void output_Bounds_of_Overapproximation(FILE * file, FILE * header, FORMAT,
				      struct ASTNode *, struct task *, ConstSequenceOfStates);
#endif
