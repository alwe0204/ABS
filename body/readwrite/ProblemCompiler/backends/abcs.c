/** @file 
 *@author Alexander Weber (a.weber@unibw.de)
 *@data 06 Dez 2016
 *@brief Functions to output the specifications and parameters of the control problem and corresponding abstraction
*/
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include <math.h>
#include <sys/time.h>
#include "abcs.h"
#include "ia.h"
#include "expressions.h"
#include "reduce_int.h"
#include "to_string.h"
#include "output.h"
#include "output_aux.h"
#include "my_strcat.h"

#include "gappa_abcs.h"

enum hyperinterval_approximation { OVERAPPROXIMATION, UNDERAPPROXIMATION };
enum discretization_type { STATEDISCRET, INPUTDISCRET };


/** @brief Source code generation for a list of hyper-intervals
 * Every entry of the list is an array of length twice \p dim where the first \p dim 
 * entries correspond to the lower end point and the last \p dim entries to the upper end point
 * @param source The file to write the source code
 * @param header The header file for \p source
 * @param f The format of the source code, one of ::C_FORMAT, ::ADA_FORMAT
 * @param approx The method of rounding the numerical values
 * @param name The identifier for the hyper-intervals
 * @param in The hyper-intervals
 * @param number The number of hyper-intervals in \p in
 * @param dim The dimension of the hyper-intervals
 * @param S The sequence of states
 */

void output_union_of_hyperintervals(FILE * source, FILE * header,
				    FORMAT f,
				    enum hyperinterval_approximation
				    approx, char *name, Expression in,
				    const unsigned int number,
				    const dim_t dim, ConstSequenceOfStates S)
{
    static int z_ada = 0;
    static int z_c = 0;
    char *buf, *buf2;
    size_t k, i;
    double a = 0, b = 0;

    char *outtype = "Compact_Hyperinterval_T_Array";
    char *typefree = "Vector_Float_T_Access";

    Expression ptr, qtr;
    switch (f) {
    case C_FORMAT:
	if (z_c == 0) {
	    fprintf(header, "void union_of_hyperintervals(double **);\n");
	    fprintf(source, "void union_of_hyperintervals(double **op)\n{\n");
	    fprintf(source, "unsigned int i;\n");
	    fprintf(source, "for(i = 0;i < %u;i++)\n{\n", number);
	    fprintf(source, "free(op[i]);\n}\n");
	    fprintf(source, "free(op);\n");
	    fprintf(source, "\n}\n");
	    z_c = 1;
	}
	fprintf(header, "unsigned int Get_Num_Of_%s(void);\n", name);
	fprintf(source, "unsigned int Get_Num_Of_%s(void)\n{\n\treturn %u;\n}\n", name, number);
	fprintf(header, "double **get_%s(void);\n", name);
	fprintf(source, "double **get_%s(void)\n{\n", name);
	fprintf(source, "unsigned int i;\n");
	fprintf(source, "double **retval = (double **)malloc(%u*sizeof(double *));\n", number);
	fprintf(source, "if( retval == NULL ) return NULL;\n");
	fprintf(source, "for(i = 0;i < %u;i++)\n{\n", number);
	fprintf(source, "retval[i] = (double *)malloc(%u*sizeof(double));\n}\n", 2 * dim);
	fprintf(source, "if( retval[i] == NULL ) return NULL;\n");
	break;
    case ADA_FORMAT:
	if (z_ada == 0) {
	    fprintf(header, "procedure free_union_of_hyperintervals(op : in out %s);\n", outtype);
	    fprintf(header, "-- @description Deallocates a union_of_hyperintervals.\n");
	    fprintf(source, "procedure free_union_of_hyperintervals(op : in out %s) is\n", outtype);
	    fprintf(source,
		    "procedure free is new Ada.Unchecked_Deallocation(Vector_Float_T,%s);\n",
		    typefree);
	    fprintf(source, "begin\n");
	    fprintf(source, "for i in op'Range loop\n");
	    fprintf(source, "free(op(i)(Lower));\n");
	    fprintf(source, "free(op(i)(Upper));\n");
	    fprintf(source, "end loop;\n");
	    fprintf(source, "end free_union_of_hyperintervals;\n\n");
	    z_ada = 1;
	}
	fprintf(header, "function Get_Num_Of_%s return NonNegative_Integer_T;\n", name);
	fprintf(header, "-- @return The length of the list \'%s\'.\n", name);
	fprintf(source,
		"function Get_Num_Of_%s return NonNegative_Integer_T is\nbegin\n\treturn %u;\nend Get_Num_Of_%s;\n\n",
		name, number, name);
	fprintf(header, "function Get_%s return %s;\n", name, outtype);
	fprintf(header, "-- @return The list \'%s\'.\n", name);
	fprintf(source, "function Get_%s return %s is\n", name, outtype);
	fprintf(source,
		"retval : %s (1 .. %u) := (others => (others => new Vector_Float_T (1 .. %u)) ) ;\nbegin\n",
		outtype, number, dim);
	break;
    default:
	assert(0);
    }
    if (in != NULL) {
	for (k = 0, ptr = (Expression) wind_back(AST_EXPR, in); ptr != NULL; ptr = ptr->next, k++) {
	    qtr = ptr->left;
	    assert(qtr != NULL);
	    for (i = 0, qtr = (Expression) wind_back(AST_EXPR, qtr); i < dim; i++, qtr = qtr->next) {
		Expression intv;
		assert(qtr->type == CONT_INTV || qtr->type == POSTFIX);
		if (qtr->type == POSTFIX)
		    intv = definition_of(qtr, S);
		else
		    intv = qtr;
		assert(intv->type == CONT_INTV);

		switch (approx) {
		case OVERAPPROXIMATION:
		    a = get_lowerbound_of_expression(intv->left, S);
		    b = get_upperbound_of_expression(intv->right, S);
		    break;
		case UNDERAPPROXIMATION:
		    a = get_upperbound_of_expression(intv->left, S);
		    b = get_lowerbound_of_expression(intv->right, S);
		    break;
		default:
		    assert(0);
		}		// end switch
		switch (f) {
		case C_FORMAT:
		    buf =
			array_of_array_entry_to_string_int("retval", k, i,
							   format_to_string_format(f));
		    buf2 = double_to_string(a, format_to_string_format(f));
		    output_assignment(source, f, 0, 0, buf, buf2);
		    fprintf(source, "\n");
		    free(buf);
		    free(buf2);
		    buf =
			array_of_array_entry_to_string_int("retval", k,
							   i + dim, format_to_string_format(f));
		    buf2 = double_to_string(b, format_to_string_format(f));
		    output_assignment(source, f, 0, 0, buf, buf2);
		    fprintf(source, "\n");
		    free(buf);
		    free(buf2);
		    break;
		case ADA_FORMAT:
		    {
			char *k_str = int_to_string(k + 1, SYMBOLIC);
			char *i_str = int_to_string(i + 1, SYMBOLIC);
			buf = "";
			my_strcat_several(&buf, 5, "retval(", k_str, ")(Lower)(", i_str, ")");
			buf2 = double_to_string(a, format_to_string_format(f));
			output_assignment(source, f, 0, 0, buf, buf2);
			fprintf(source, "\n");
			free(buf);
			free(buf2);
			buf = "";
			my_strcat_several(&buf, 5, "retval(", k_str, ")(Upper)(", i_str, ")");
			buf2 = double_to_string(b, format_to_string_format(f));
			output_assignment(source, f, 0, 0, buf, buf2);
			fprintf(source, "\n");
			free(buf);
			free(buf2);
			free(k_str);
			free(i_str);
		    }
		    break;
		default:
		    assert(0);
		}		// end switch
	    }			// end for i
	}			// end for k
	assert(k == number);
    }				// end if
    switch (f) {
    case C_FORMAT:
	fprintf(source, "return retval;\n}\n");
	break;
    case ADA_FORMAT:
	fprintf(source, "return retval;\nend Get_%s;\n\n", name);
	break;
    default:
	assert(0);
    }
}

/** @brief Source code generation for the sampling time in round up mode
 * @param file The file to write the source code
 * @param header The header file for \p file
 * @param f The format of the source code, one of ::C_FORMAT, ::ADA_FORMAT
 * @param in The abstract syntax tree
 * @param S The sequence of states
 */

void output_samplingtime(FILE * file, FILE * header, FORMAT f,
			 struct ASTNode *in, ConstSequenceOfStates S)
{
    char *name = "Get_Sampling_Time";
    double samplingtime;
    assert(is_Expression_literal_Number(in->options->SamplingTime));
    if (TYPE(in->options->SamplingTime) == NUM_INT) {
	samplingtime = (double) (*(int *) (CONTENT(in->options->SamplingTime)));
    } else if (TYPE(in->options->SamplingTime) == NUM_DEC) {
	samplingtime = strtod(CONTENT(in->options->SamplingTime), NULL);
    } else {
	samplingtime = math_constant_to_double(in->options->SamplingTime);
    }
    char *buf = double_to_string(samplingtime, format_to_string_format(f));
    switch (f) {
    case C_FORMAT:
	fprintf(header, "double %s(void);\n", name);
	fprintf(file, "double %s(void)\n{\n\treturn %s;\n}\n", name, buf);
	break;
    case ADA_FORMAT:
	fprintf(header, "function %s return Time_T;\n", name);
	fprintf(header, "-- @return The sampling time\n");
	fprintf(file,
		"function %s return Time_T is\nbegin\n\treturn Time_T(%s);\nend %s;\n\n",
		name, buf, name);
	break;
    default:
	assert(0);
    }
    free(buf);
}

/** @brief Source code generation for the operating range in round up mode. 
 * It uses output_union_of_hyperintervals().
 * @param file The file to write the source code
 * @param header The header file for \p file
 * @param f The format of the source code, one of ::C_FORMAT, ::ADA_FORMAT
 * @param in The abstract syntax tree
 * @param op The task data structure
 * @param S The sequence of states 
*/

void output_operatingrange(FILE * file, FILE * header, FORMAT f,
			   struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    const Expression arg = in->options->Range;
    const dim_t dim = in->ode->dim;
    output_union_of_hyperintervals(file, header, f, OVERAPPROXIMATION,
				   "Operating_Range", arg, 1, dim, S);
}

void output_inputspace(FILE * file, FILE * header, FORMAT f,
		       struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    Expression arg;
    if (TYPE(in->Function->dom) == LIST) {
	arg = in->Function->dom->left;
    } else {
	arg = copy_Expression(in->Function->dom);
    }
    const dim_t dim = op->input_space_dimension;
    const Expression expr = install_expression(LIST, arg, NULL);
    output_union_of_hyperintervals(file, header, f, UNDERAPPROXIMATION,
				   "Input_Space", expr, 1, dim, S);
}

/** @brief Source code generation for the initial set in round up mode. 
 * It uses output_union_of_hyperintervals().
 * @param file The file to write the source code
 * @param header The header file for \p file
 * @param f The format of the source code, one of ::C_FORMAT, ::ADA_FORMAT
 * @param in The abstract syntax tree
 * @param op The task data structure
 * @param S The sequence of states 
*/

void output_initialset(FILE * file, FILE * header, FORMAT f,
		       struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    const Expression arg = in->options->InitialSet;
    const dim_t dim = in->ode->dim;
    output_union_of_hyperintervals(file, header, f, OVERAPPROXIMATION,
				   "Initial_Set", arg, op->num_of_initialsets, dim, S);
}

/** @brief Source code generation for the target set in round down mode. 
 * It uses output_union_of_hyperintervals().
 * @param file The file to write the source code
 * @param header The header file for \p file
 * @param f The format of the source code, one of ::C_FORMAT, ::ADA_FORMAT
 * @param in The abstract syntax tree
 * @param op The task data structure
 * @param S The sequence of states 
*/

void output_targetset(FILE * file, FILE * header, FORMAT f,
		      struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    const Expression arg = in->options->TargetSet;
    const dim_t dim = in->ode->dim;
    output_union_of_hyperintervals(file, header, f, UNDERAPPROXIMATION,
				   "Target_Set", arg, op->num_of_targetsets, dim, S);
}


/** @brief Source code generation for the obstacle set in round up mode. 
 * It uses output_union_of_hyperintervals().
 * @param file The file to write the source code
 * @param header The header file for \p file
 * @param f The format of the source code, one of ::C_FORMAT, ::ADA_FORMAT
 * @param in The abstract syntax tree
 * @param op The task data structure
 * @param S The sequence of states 
*/
void output_obstacleset(FILE * file, FILE * header, FORMAT f,
			struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    const Expression arg = in->options->ObstacleSet;
    const dim_t dim = in->ode->dim;
    output_union_of_hyperintervals(file, header, f, OVERAPPROXIMATION,
				   "Obstacle_Set", arg, op->num_of_obstaclesets, dim, S);
}

void output_Vector_RU_core(FILE * file, FILE * header, FORMAT f, char *name,
			   char *outtype, char *comment, dim_t length, ConstSequenceOfStates S)
{
    static int stat = 0;
    if (stat == 0) {
	switch (f) {
	case C_FORMAT:
	    fprintf(header, "%s *%s(void);\n", outtype, name);
	    fprintf(file, "%s *%s(void)\n{\n", outtype, name);
	    fprintf(file, "%s *retval = malloc(%u*sizeof(%s));\n", outtype, length, outtype);
	    break;
	case ADA_FORMAT:
	    fprintf(header, "function %s return %s;\n", name, outtype);
	    fprintf(header, "%s\n", comment);
	    fprintf(file, "function %s return %s is\n", name, outtype);
	    fprintf(file, "retval : %s (1 .. %u);\n", outtype, length);
	    break;
	default:
	    assert(0);
	}
	switch (f) {
	case C_FORMAT:
	    break;
	case ADA_FORMAT:
	    fprintf(file, "begin\n");
	    break;
	default:
	    assert(0);
	}
	stat = 1;
    } else {
	switch (f) {
	case C_FORMAT:
	    fprintf(file, "return retval;\n}\n");
	    break;
	case ADA_FORMAT:
	    fprintf(file, "return retval;\nend %s;\n\n", name);
	    break;
	default:
	    assert(0);
	}
	stat = 0;
    }
}

void output_double_Vector_RU(FILE * file, FILE * header, FORMAT f, char *name,
			     char *outtype, char *comment, dim_t length,
			     double *in, ConstSequenceOfStates S)
{
    dim_t k;
    output_Vector_RU_core(file, header, f, name, outtype, comment, length, S);
    for (k = 0; k < length; k++) {
	char *buf = vector_entry_to_string_int("retval", k, format_to_string_format(f));
	double res = in[k];
	char *buf2 = double_to_string(res, format_to_string_format(f));
	output_assignment(file, f, 0, 0, buf, buf2);
	fprintf(file, "\n");
	free(buf);
	free(buf2);
    }
    output_Vector_RU_core(file, header, f, name, outtype, comment, length, S);
}


double *Expression_to_double_Vector_RU(Expression in, dim_t length, ConstSequenceOfStates S)
{
    Expression ptr;
    dim_t k;
    double *retval = my_malloc(length * sizeof(double));
    if (in == NULL) {
	for (k = 0; k < length; k++) {
	    retval[k] = 0;
	}
    } else {
	for (k = 0, ptr = (Expression) wind_back(AST_EXPR, in->left);
	     ptr != NULL; ptr = ptr->next, k++) {
	    retval[k] = get_upperbound_of_expression(ptr, S);
	}
    }
    return retval;
}


/** @brief Source code generation for a function returning a vector in round up mode.
 * @param file The file to write the source code
 * @param header The header file for \p file
 * @param f The format of the source code, one of ::C_FORMAT, ::ADA_FORMAT
 * @param name The name of the function
 * @param outtype The return value type of the function, e.g., "double"
 * @param command A command to add to the generated source code
 * @param length The dimension of the vector
 * @param in The vector
 * @param S The sequence of states 
*/

void output_Vector(FILE * file, FILE * header, FORMAT f, char *name,
		   char *outtype, char *comment, dim_t length,
		   Expression in, ConstSequenceOfStates S)
{
    char *buf, *buf2;
    dim_t k;
    Expression ptr;
    output_Vector_RU_core(file, header, f, name, outtype, comment, length, S);
    if (in == NULL) {
	for (k = 0; k < length; k++) {
	    buf = vector_entry_to_string_int("retval", k, format_to_string_format(f));
	    buf2 = int_to_string(0, NUMERICAL);
	    output_assignment(file, f, 0, 0, buf, buf2);
	    fprintf(file, "\n");
	    free(buf);
	    free(buf2);
	}
    } else {
	for (k = 0, ptr = (Expression) wind_back(AST_EXPR, in->left);
	     ptr != NULL; ptr = ptr->next, k++) {
	    buf = vector_entry_to_string_int("retval", k, format_to_string_format(f));
	    double res = get_upperbound_of_expression(ptr, S);
	    buf2 = double_to_string(res, format_to_string_format(f));
	    output_assignment(file, f, 0, 0, buf, buf2);
	    fprintf(file, "\n");
	    free(buf);
	    free(buf2);
	}
    }
    output_Vector_RU_core(file, header, f, name, outtype, comment, length, S);
}


/** @brief Source code generation for the uncertainties. 
 * It uses output_Vector().
 * @param file The file to write the source code
 * @param header The header file for \p file
 * @param f The format of the source code, one of ::C_FORMAT, ::ADA_FORMAT
 * @param in The abstract syntax tree
 * @param op The task data structure
 * @param S The sequence of states 
*/
void output_uncertainties(FILE * file, FILE * header, FORMAT f,
			  struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    char *outtype;
    char *comment = "";
    Option opt = in->options;
    switch (f) {
    case C_FORMAT:
	outtype = "dim_t";
	break;
    case ADA_FORMAT:
	outtype = "Bounds_of_Dynamic_Uncertainties_T";
	comment = "-- @return The vector specifying the uncertainties in the dynamics.";
	break;
    default:
	assert(0);
    }
    op->Bounds_of_Dynamic_Uncertainties =
	Expression_to_double_Vector_RU(opt->Uncertainties, op->state_space_dimension, S);
    output_Vector(file, header, f, "Get_Bounds_Of_Dynamic_Uncertainties", outtype, comment,
		  op->state_space_dimension, opt->Uncertainties, S);
}

/** @brief Source code generation for the measurement errors. 
 * It uses output_Vector().
 * @param file The file to write the source code
 * @param header The header file for \p file
 * @param f The format of the source code, one of ::C_FORMAT, ::ADA_FORMAT
 * @param in The abstract syntax tree
 * @param op The task data structure
 * @param S The sequence of states 
*/
void output_measurementerrors(FILE * file, FILE * header, FORMAT f,
			      struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    char *outtype;
    char *comment = "";
    Option opt = in->options;
    switch (f) {
    case C_FORMAT:
	outtype = "dim_t";
	break;
    case ADA_FORMAT:
	outtype = "Bounds_of_Measurement_Errors_T";
	comment = "-- @return The vector specifying the bound on the measurement errors.";
	break;
    default:
	assert(0);
    }
    op->Bounds_of_Measurement_Errors =
	Expression_to_double_Vector_RU(opt->MeasErrors, op->state_space_dimension, S);
    output_Vector(file, header, f, "Get_Bounds_Of_Measurement_Errors", outtype,
		  comment, op->state_space_dimension, opt->MeasErrors, S);
}

void output_grid_discretization(FILE * file, FILE * fileheader, FORMAT f,
				struct ASTNode *in, struct task *op,
				enum discretization_type t, ConstSequenceOfStates S)
{
    char *name;
    char *comment;
    char *return_type;
    dim_t dim;
    Expression expr;
    Option opt = in->options;
    switch (t) {
    case STATEDISCRET:
	name = "Get_Initial_State_Space_Subdivision";
	comment = "The number of subdivisions in each egde of the operating range.";
	return_type = "State_Space_Subdivision_T";
	dim = op->state_space_dimension;
	expr = opt->StateSpaceDiscret->left;
	break;
    case INPUTDISCRET:
	name = "Get_Initial_Input_Space_Subdivision";
	comment = "The number of subdivisions in each egde of the continuous input set.";
	return_type = "Input_Space_Subdivision_T";
	dim = op->input_space_dimension;
	expr = opt->InputSpaceDiscret->left;
	break;
    default:
	assert(0);
    }
    char *buf, *buf2;
    dim_t k;

    Expression ptr;
    switch (f) {
    case C_FORMAT:
	fprintf(fileheader, "dim_t *%s(void);\n", name);
	fprintf(file, "dim_t *%s(void)\n{\n", name);
	fprintf(file, "dim_t *retval = malloc(%u*sizeof(dim_t));\n", dim);
	break;
    case ADA_FORMAT:
	fprintf(fileheader, "function %s return %s;\n", name, return_type);
	fprintf(fileheader, "-- @return %s\n", comment);
	fprintf(file, "function %s return %s is\n", name, return_type);
	fprintf(file, "retval : %s (1 .. %u);\n", return_type, dim);
	break;
    default:
	assert(0);
    }
    switch (f) {
    case C_FORMAT:
	break;
    case ADA_FORMAT:
	fprintf(file, "begin\n");
	break;
    default:
	assert(0);
    }
    for (k = 0, ptr = (Expression) wind_back(AST_EXPR, expr); ptr != NULL; ptr = ptr->next, k++) {
	buf = vector_entry_to_string_int("retval", k, format_to_string_format(f));
	buf2 = Expression_to_string(ptr, SYMBOLIC, MYSTRING, S);
	output_assignment(file, f, 0, 0, buf, buf2);
	fprintf(file, "\n");
	free(buf);
	free(buf2);
    }
    switch (f) {
    case C_FORMAT:
	fprintf(file, "return retval;\n}\n");
	break;
    case ADA_FORMAT:
	fprintf(file, "return retval;\nend %s;\n\n", name);
	break;
    default:
	assert(0);
    }
}

void output_state_space_discretization(FILE * file, FILE * fileheader,
				       FORMAT f, struct ASTNode *in,
				       struct task *op, ConstSequenceOfStates S)
{
    output_grid_discretization(file, fileheader, f, in, op, STATEDISCRET, S);
}

void output_input_space_discretization(FILE * file, FILE * fileheader,
				       FORMAT f, struct ASTNode *in,
				       struct task *op, ConstSequenceOfStates S)
{
    output_grid_discretization(file, fileheader, f, in, op, INPUTDISCRET, S);
}


void output_periods(FILE * file, FILE * fileheader, FORMAT f,
		    struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    dim_t *periods;
    periods = calloc(op->state_space_dimension, sizeof(dim_t));
    char *name_periods = "Get_Coord_With_Periods";
    char *buf, *buf2;
    char *return_type = "List_of_Component_Index_T";
    dim_t anz = op->num_of_coord_with_periods;
    dim_t k;
    dim_t a;
    switch (f) {
    case C_FORMAT:
	fprintf(fileheader, "dim_t *%s(void);\n", name_periods);
	fprintf(file, "dim_t *%s(void)\n{\n", name_periods);
	if (anz != 0)
	    fprintf(file, "dim_t *retval = malloc(%u*sizeof(dim_t));\n", anz);
	else {
	    fprintf(file, "return NULL;\n}\n");
	}
	break;
    case ADA_FORMAT:
	fprintf(fileheader, "function %s return %s;\n", name_periods, return_type);
	fprintf(fileheader, "-- @return An array with the coordinates that possess a period.\n");
	fprintf(fileheader,
		"-- In the case that there is no such coordinate the empty array is returned.\n");
	fprintf(file, "function %s return %s is\n", name_periods, return_type);
	fprintf(file, "retval : %s (1 .. %u);\n", return_type, anz);
	if (anz == 0) {
	    fprintf(file, "begin\nreturn retval;\nend %s;\n\n", name_periods);
	} else
	    fprintf(file, "begin\n");
	break;
    default:
	assert(0);
    }

    if (anz != 0) {
	Option opt = in->options;
	Expression ptr;
	for (k = 0, ptr =
	     (Expression) wind_back(AST_EXPR, opt->Periods->left);
	     ptr != NULL; ptr = ptr->next, k++) {
	    buf = vector_entry_to_string_int("retval", k, format_to_string_format(f));
	    a = (dim_t) get_int_from_expression(ptr, S);
	    switch (f) {
	    case C_FORMAT:
		buf2 = int_to_string(a, SYMBOLIC);
		break;
	    case ADA_FORMAT:
		buf2 = int_to_string(a + 1, SYMBOLIC);
		break;
	    default:
		assert(0);
	    }
	    output_assignment(file, f, 0, 0, buf, buf2);
	    fprintf(file, "\n");
	    free(buf);
	    free(buf2);
	    periods[a] = 1;
	}
	switch (f) {
	case C_FORMAT:
	    fprintf(file, "return retval;\n}\n");
	    break;
	case ADA_FORMAT:
	    fprintf(file, "return retval;\nend %s;\n\n", name_periods);
	    break;
	default:
	    assert(0);
	}
    }
    char *name_no_periods = "Get_Coord_Without_Periods";
    anz = op->state_space_dimension - op->num_of_coord_with_periods;
    switch (f) {
    case C_FORMAT:
	fprintf(fileheader, "dim_t *%s(void);\n", name_no_periods);
	fprintf(file, "dim_t *%s(void)\n{\n", name_no_periods);
	if (anz != 0)
	    fprintf(file, "dim_t *retval = malloc(%u*sizeof(dim_t));\n", anz);
	else {
	    fprintf(file, "return NULL;\n}\n");
	    return;
	}
	break;
    case ADA_FORMAT:
	fprintf(fileheader, "function %s return %s;\n", name_no_periods, return_type);
	fprintf(fileheader,
		"-- @return An array with the coordinates that do not possess a period.\n");
	fprintf(fileheader,
		"-- In the case that there is no such coordinate the empty array is returned.\n");
	fprintf(file, "function %s return %s is\n", name_no_periods, return_type);
	fprintf(file, "retval : %s (1 .. %u);\n", return_type, anz);
	if (anz == 0) {
	    fprintf(file, "begin\nreturn retval;\nend %s;\n\n", name_no_periods);
	    return;
	}
	break;
    default:
	assert(0);
    }
    if (anz != 0) {
	switch (f) {
	case C_FORMAT:
	    break;
	case ADA_FORMAT:
	    fprintf(file, "begin\n");
	    break;
	default:
	    assert(0);
	}
	dim_t i = 0;
	for (k = 0; k < op->state_space_dimension; k++) {
	    if (periods[k] != 1) {
		buf = vector_entry_to_string_int("retval", i, format_to_string_format(f));
		switch (f) {
		case C_FORMAT:
		    buf2 = int_to_string(k, SYMBOLIC);
		    break;
		case ADA_FORMAT:
		    buf2 = int_to_string(k + 1, SYMBOLIC);
		    break;
		default:
		    assert(0);
		}
		output_assignment(file, f, 0, 0, buf, buf2);
		fprintf(file, "\n");
		free(buf);
		free(buf2);
		i++;
	    }
	}
	switch (f) {
	case C_FORMAT:
	    fprintf(file, "return retval;\n}\n");
	    break;
	case ADA_FORMAT:
	    fprintf(file, "return retval;\nend %s;\n\n", name_no_periods);
	    break;
	default:
	    assert(0);
	}
    }
    free(periods);
}

void output_Index_Of_Dimension_function(FILE * file, FILE * fileheader,
					FORMAT f, const char *name, unsigned int number)
{
    char *return_type = "Component_Index_T";
    switch (f) {
    case C_FORMAT:
	fprintf(fileheader, "dim_t %s(void);\n", name);
	fprintf(file, "dim_t %s(void)\n{\n\treturn %u;\n}\n", name, number);
	break;
    case ADA_FORMAT:
	fprintf(fileheader, "function %s return %s;\n", name, return_type);
	fprintf(file,
		"function %s return %s is\nbegin\n\treturn %u;\nend %s;\n\n",
		name, return_type, number, name);
	break;
    default:
	assert(0);
    }
}

void output_dimension_of_spaces(FILE * file, FILE * header, FORMAT f, struct task *op)
{
    switch (f) {
    case C_FORMAT:
	output_Index_Of_Dimension_function(file, header, C_FORMAT, "Get_State_Space_Dimension",
					   op->state_space_dimension);
	output_Index_Of_Dimension_function(file, header, C_FORMAT, "Get_Input_Space_Dimension",
					   op->input_space_dimension);
	break;
    case ADA_FORMAT:
	output_Index_Of_Dimension_function(file, header, ADA_FORMAT, "Get_State_Space_Dimension",
					   op->state_space_dimension);
	fprintf(header, "-- @return The dimension of the state space.\n");
	output_Index_Of_Dimension_function(file, header, ADA_FORMAT, "Get_Input_Space_Dimension",
					   op->input_space_dimension);
	fprintf(header, "-- @return The dimension of the input space.\n");
	break;
    default:
	assert(0);
    }
}

void output_num_of_periods(FILE * file, FILE * header, FORMAT f, struct task *op)
{
    switch (f) {
    case C_FORMAT:
	output_Index_Of_Dimension_function(file, header, C_FORMAT, "Get_Num_Of_Coord_With_Periods",
					   op->num_of_coord_with_periods);
	output_Index_Of_Dimension_function(file, header, C_FORMAT,
					   "Get_Num_Of_Coord_Without_Periods",
					   op->state_space_dimension -
					   op->num_of_coord_with_periods);
	break;
    case ADA_FORMAT:
	output_Index_Of_Dimension_function(file, header, ADA_FORMAT,
					   "Get_Num_Of_Coord_With_Periods",
					   op->num_of_coord_with_periods);
	fprintf(header,
		"-- @return The number of coordinates in the state space that possess a period.\n");
	output_Index_Of_Dimension_function(file, header, ADA_FORMAT,
					   "Get_Num_Of_Coord_Without_Periods",
					   op->state_space_dimension -
					   op->num_of_coord_with_periods);
	fprintf(header,
		"-- @return The number of coordinates in the state space that do not possess a period.\n");
	break;
    default:
	assert(0);
    }
}

/** @brief Source code generation for the rounded initial state radius. 
 * It uses output_Vector() and Expression_to_double_Vector_RU().
 * @param file The file to write the source code
 * @param header The header file for \p file
 * @param f The format of the source code, one of ::C_FORMAT, ::ADA_FORMAT
 * @param in The abstract syntax tree
 * @param op The task data structure
 * @param S The sequence of states 
*/
void output_rounded_initial_state_radius(FILE * file, FILE * header, FORMAT f,
					 struct ASTNode *in, struct task *op,
					 ConstSequenceOfStates S)
{
    unsigned int i;
    dim_t dim = op->state_space_dimension;
    char *outtype;
    char *comment = "";
    Option opt = in->options;
    Expression vector = NULL, ptr, qtr;

    switch (f) {
    case C_FORMAT:
	outtype = "double";
	break;
    case ADA_FORMAT:
	outtype = "State_Radius_T";
	comment =
	    "-- @return Returns a value >= (xmax - xmix) / Initial_State_Space_Division / 2 (component-wise)\n-- @description See Programmer's Manual for presice definitions.";
	break;
    default:
	assert(0);
    }
    for (i = 0,
	 ptr = (Expression) wind_back(AST_EXPR, opt->StateSpaceDiscret->left),
	 qtr = (Expression) wind_back(AST_EXPR, opt->Range->left);
	 i < dim; i++, qtr = qtr->next, ptr = ptr->next) {
	Expression intv;
	assert(qtr->type == CONT_INTV || qtr->type == POSTFIX);
	if (qtr->type == POSTFIX)
	    intv = definition_of(qtr, S);
	else
	    intv = qtr;
	assert(intv->type == CONT_INTV);
	/* Begin: Computation */
	/* xmax - xmin */
	Expression expr = install_expression(MINUS, intv->right, intv->left);
	/* ( xmax - xmin ) / d_x */
	expr = install_expression(DIV, expr, ptr);
	/* ( xmax - xmin ) / d_x / 2 */
	expr = install_expression(DIV, expr, install_Number_int(2));
	/* End: Computation */

	if (i == 0) {
	    vector = expr;
	} else {
	    vector = append(AST_EXPR, vector, expr);
	}
    }
    vector = install_expression(LIST, vector, NULL);
    op->Rounded_Initial_State_Radius = Expression_to_double_Vector_RU(vector, dim, S);
    output_Vector(file, header, f, "Get_Rounded_Initial_State_Radius", outtype,
		  comment, dim, vector, S);
}

void output_Bounds_of_Lipschitz_Matrices(FILE * file, FILE * header, FORMAT f,
					 struct ASTNode *in, struct task *op,
					 ConstSequenceOfStates S)
{
    dim_t dim = op->state_space_dimension;
    char *outtype;
    char *comment = "";
    switch (f) {
    case C_FORMAT:
	outtype = "double";
	comment = "";
	break;
    case ADA_FORMAT:
	outtype = "Vector_Float_T";
	comment = "-- @return See Programmer's Manual.";
	break;
    default:
	assert(0);
    }
    output_double_Vector_RU(file, header, f, "Get_Bounds_of_Lipschitz_Matrices", outtype,
			    comment, dim * dim, op->Bounds_of_Lipschitz_Matrices, S);
}

void output_Bounds_of_Approximation_Errors(FILE * file, FILE * header, FORMAT f,
					   struct ASTNode *in, struct task *op,
					   ConstSequenceOfStates S)
{
    dim_t dim = op->state_space_dimension;
    char *outtype;
    char *comment = "";
    switch (f) {
    case C_FORMAT:
	outtype = "double";
	comment = "";
	break;
    case ADA_FORMAT:
	outtype = "Bounds_of_Numerical_Errors_State_T";
	comment = "-- @return See Programmer's Manual.";
	break;
    default:
	assert(0);
    }
    output_double_Vector_RU(file, header, f,
			    "Get_Bounds_of_Approximation_Error_of_General_Solution", outtype,
			    comment, dim, op->Bounds_of_Approximation_Error_General_Solution, S);
    output_double_Vector_RU(file, header, f, "Get_Bounds_of_Approximation_Error_of_Growth_Bound",
			    outtype, comment, dim, op->Bounds_of_Approximation_Error_Growth_Bound,
			    S);
}

void output_Bounds_of_Summation_Errors(FILE * file, FILE * header, FORMAT f,
				       struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    dim_t dim = op->state_space_dimension;
    char *outtype;
    char *comment = "";
    switch (f) {
    case C_FORMAT:
	outtype = "double";
	comment = "";
	break;
    case ADA_FORMAT:
	outtype = "Bounds_of_Numerical_Errors_State_T";
	comment = "-- @return See Programmer's Manual.";
	break;
    default:
	assert(0);
    }
    output_double_Vector_RU(file, header, f, "Get_Bounds_of_Summation_Error_General_Solution",
			    outtype, comment, dim, op->Bounds_of_Summation_Error_General_Solution,
			    S);
    output_double_Vector_RU(file, header, f, "Get_Bounds_of_Summation_Error_Growth_Bound", outtype,
			    comment, dim, op->Bounds_of_Summation_Error_Growth_Bound, S);

}

void output_Bounds_of_Overapproximation(FILE * file, FILE * header, FORMAT f,
					struct ASTNode *in, struct task *op,
					ConstSequenceOfStates S)
{
    dim_t dim = op->state_space_dimension;
    char *outtype;
    char *comment = "";
    switch (f) {
    case C_FORMAT:
	outtype = "double";
	comment = "";
	break;
    case ADA_FORMAT:
	outtype = "Bounds_of_Numerical_Errors_State_T";
	comment = "-- @return See Programmer's Manual.";
	break;
    default:
	assert(0);
    }
    output_double_Vector_RU(file, header, f, "Get_Bounds_of_Overapproximation_Rounding_Error",
			    outtype, comment, dim, op->Bounds_of_Overapproximation_Rounding_Error,
			    S);
    output_double_Vector_RU(file, header, f, "Get_Bounds_of_Overapproximation_Radius", outtype,
			    comment, dim, op->Bounds_of_Overapproximation_Radius, S);
}


void output_Bounds_of_Rounding_Errors(FILE * file, FILE * header, FORMAT f,
				      struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    dim_t dim = op->state_space_dimension;
    char *outtype;
    char *comment = "";
    switch (f) {
    case C_FORMAT:
	outtype = "double";
	comment = "";
	break;
    case ADA_FORMAT:
	outtype = "Bounds_of_Numerical_Errors_State_T";
	comment = "-- @return See Programmer's Manual.";
	break;
    default:
	assert(0);
    }
    output_double_Vector_RU(file, header, f, "Get_Bounds_of_Rounding_Error_of_General_Solution",
			    outtype, comment, dim, op->Bounds_of_Rounding_Error_General_Solution,
			    S);
    output_double_Vector_RU(file, header, f, "Get_Bounds_of_Rounding_Error_of_Growth_Bound",
			    outtype, comment, dim, op->Bounds_of_Rounding_Error_Growth_Bound, S);
}


void output_Bounds_of_Input_Value_Rounding_Error(FILE * file, FILE * header, FORMAT f,
						 struct ASTNode *in, struct task *op,
						 ConstSequenceOfStates S)
{
    dim_t dim = op->input_space_dimension;
    char *outtype;
    char *comment = "";
    switch (f) {
    case C_FORMAT:
	outtype = "double";
	comment = "";
	break;
    case ADA_FORMAT:
	outtype = "Bounds_of_Numerical_Errors_Input_T";
	comment = "-- @return See Programmer's Manual.";
	break;
    default:
	assert(0);
    }
    output_double_Vector_RU(file, header, f, "Get_Bounds_of_Input_Value_Rounding_Error", outtype,
			    comment, dim, op->Bounds_of_Input_Value_Rounding_Error, S);
}

/**
 *@brief The implementation of the term
 * \f$B([0,\tau],\llbracket 0, r_0 + z \rrbracket,M,\llbracket 0 , w_\circ \rrbracket)\f$
 * @param file The file to write the template
 * @param op The task data structure
*/

void output_matrix_exponential_template(FILE * file, struct task *op)
{
    dim_t i;
    char *id = "x";
    char *comma = "(";
    dim_t dim = op->state_space_dimension;

    mpfi_t ri;
    mpfi_t zi;
    mpfi_t ai;
    mpfi_init(ri);
    mpfi_init(zi);
    mpfi_init(ai);

    double *r_plus_z = my_malloc(dim * sizeof(double));
    double *w = op->Bounds_of_Dynamic_Uncertainties;
    double *M = op->Bounds_of_Lipschitz_Matrices;

    for (i = 0; i < dim; i++) {
	mpfi_set_d(ri, op->Rounded_Initial_State_Radius[i]);
	mpfi_set_d(zi, op->Bounds_of_Measurement_Errors[i]);
	mpfi_add(ai, ri, zi);
	r_plus_z[i] = mpfi_get_right_d(ai);
    }

    mpfi_clear(ri);
    mpfi_clear(zi);
    mpfi_clear(ai);

    fprintf(file, "rhs: (%s,L,w) in (", id);
    for (i = 0; i < dim; i++) {
	fprintf(file, "%s[0,%.16lf]", comma, r_plus_z[i]);
	comma = ",";
    }
    comma = "),(";
    for (i = 0; i < dim * dim; i++) {
	fprintf(file, "%s[%.16lf,%.16lf]", comma, -M[i], M[i]);
	comma = ",";
    }
    fprintf(file, ")");
    comma = ",(";
    for (i = 0; i < dim; i++) {
	fprintf(file, "%s[0,%.16lf]", comma, w[i]);
	comma = ",";
    }
    fprintf(file, ")) to y[%u]\n{\n", dim);
    fprintf(file, "for(i=0,%u,+)\n{\n", dim - 1);
    fprintf(file, "y[i] = w[i];\n");
    fprintf(file, "for(j=0,%u,+)\n{\n", dim - 1);
    fprintf(file, "y[i] = y[i] + L[%u*i+j]*%s[j];\n}\n", dim, id);
    fprintf(file, "}\n}\n");
    fprintf(file, "Integration_for_growth_bound: (t,r0,L,w) in ([0,%.16lf],", op->samplingtime_RU);
    comma = "(";
    for (i = 0; i < dim; i++) {
	fprintf(file, "%s[0,%.16lf]", comma, r_plus_z[i]);
	comma = ",";
    }
    comma = "),(";
    for (i = 0; i < dim * dim; i++) {
	fprintf(file, "%s[%.16lf,%.16lf]", comma, -M[i], M[i]);
	comma = ",";
    }
    fprintf(file, "),");
    comma = "(";
    for (i = 0; i < dim; i++) {
	fprintf(file, "%s[0,%.16lf]", comma, w[i]);
	comma = ",";
    }
    fprintf(file, ")) to %s[%u]\n{\n", id, dim);
    fprintf(file, "diff(%s,t) = rhs(%s,L,w);\n", id, id);
    fprintf(file, "initialvalue(%s,r0);\n}\n", id);

    free(r_plus_z);
}
