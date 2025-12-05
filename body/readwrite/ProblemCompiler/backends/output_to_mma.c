/** @file 
 *@author Alexander Weber (a.weber@unibw.de)
 *@data 06 Feb 2016
 *@brief Functions to output the input in on screen and Mathematica
*/
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "output_to_mma.h"

#include "my_strcat.h"
#include "expressions.h"
#include "to_string.h"
#include "ad.h"
#include "output_aux.h"
#include "output.h"

extern struct task Task;

void print_dependent_var(FILE * file, ConstSequenceOfStates S)
{
    Variable x;
    size_t i, j, k = 0;
    char *comma;
    comma = "{";
    fprintf(file, "in = ");
    const size_t len = get_states_len(S);
    for (i = len; i--;) {
	x = get_var(i, S);
	x = version_of_variable(x, 0, 0, get_date(S));
	if (var_role_of(x) == STATE_VAR || var_role_of(x) == OUTPUTSTATE_VAR) {
	    for (j = 0; j < row_dim_of(x); j++) {
		k++;
		fprintf(file, "%sin%lu", comma, k);
		comma = ",";
	    }
	}
    }
    fprintf(file, "}\n");
}

/** @brief Prints the explicitly defined function in the input file to \p file in the Mathematica format.
 *@param file The file to write on
 *@param f One of ::MMAPRINTSTRING, ::MMASTRING. The first one is appropriate for printing on screen
 *@param S A sequence \f$(\sigma_0,\ldots,\sigma_k,\ldots,\sigma_n)\f$ of states in \f$S\f$
*/

void print_function_in_mma(FILE * file, char *name, enum string_format f, ConstSequenceOfStates S)
{
    Variable ptr;
    size_t i, j, k;
    size_t num_of_output_var = 0;
    char *buf;
    char *comma, *comma2, *comma3;
    if (name == NULL) {
	comma = generate_identifier("f", S);
    } else
	comma = name;
    fprintf(file, "%s", comma);
    if (name == NULL)
	free(comma);
    comma = "[";
    const size_t len = get_states_len(S);
    for (i = len; i--;) {
	ptr = get_var(i, S);
	ptr = version_of_variable(ptr, 0, 0, get_date_of_rhs(S));
	if (ptr == NULL)
	    continue;
	if (var_role_of(ptr) == OUTPUT_VAR)
	    num_of_output_var++;
	if (var_role_of(ptr) == INPUT_VAR) {
	    fprintf(file, "%s%s_", comma, name_of(ptr));
	    comma = ",";
	}
    }
    fprintf(file, "]:=");
    comma = "{";
    comma2 = "";
    comma3 = "";
    for (k = 0; k < len; k++) {
	ptr = get_var(k, S);
	ptr = version_of_variable(ptr, 0, 0, get_date_of_rhs(S));
	if (ptr == NULL)
	    continue;
	if (var_role_of(ptr) == OUTPUT_VAR) {
	    fprintf(file, "%s", comma);
	    comma = ",";
	    if ((row_dim_of(ptr) > 1 || (row_dim_of(ptr) == 1 && col_dim_of(ptr) > 1))
		&& num_of_output_var > 1) {
		comma2 = "{";
	    }
	    for (i = 0; i < row_dim_of(ptr); i++) {
		fprintf(file, "%s", comma2);
		comma2 = ",";
		if (col_dim_of(ptr) > 1) {
		    comma3 = "{";
		}
		for (j = 0; j < col_dim_of(ptr); j++) {
		    if (IS_MMA_SYM(&Task))
			buf =
			    Expression_to_string(simplify_Expression
						 (data_of(ptr, i, j), S), SYMBOLIC, f, S);
		    else
			buf =
			    Expression_to_string(simplify_Expression
						 (data_of(ptr, i, j), S), NUMERICAL, f, S);
		    fprintf(file, "%s%s", comma3, buf);
		    free(buf);
		    comma3 = ",";
		}
		if (col_dim_of(ptr) > 1) {
		    fprintf(file, "}");
		}
		comma3 = "";
	    }
	    if ((row_dim_of(ptr) > 1 || (row_dim_of(ptr) == 1 && col_dim_of(ptr) > 1))
		&& num_of_output_var > 1) {
		fprintf(file, "}");
	    }
	    comma2 = "";
	}
    }
    fprintf(file, "}\n");
}

/** @brief Prints the explicitly defined function on stdout
 * @param in The abstract syntax tree
 * @param S A sequence \f$(\sigma_0,\ldots,\sigma_k,\ldots,\sigma_n)\f$ of states in \f$S\f$
*/

void output_function_on_screen(const struct ASTNode *in, ConstSequenceOfStates S)
{
    char *name = FCT_NAME(in->Function);
    fprintf(stdout, "Read function in Mathematica language:\n");
    fprintf(stdout, "======================================\n");
    print_function_in_mma(stdout, name, MMAPRINTSTRING, S);
    fprintf(stdout, "======================================\n");
}

void output_function_in_mma(const struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    char *folder = op->output_directory;
    char *buf = "";
    char *name = FCT_NAME(in->Function);
    FILE *file;
    if (IS_VERBOSE(op))
	my_strcat_several(&buf, 2, "mkdir -p -v ", folder);
    else
	my_strcat_several(&buf, 2, "mkdir -p ", folder);
    assert(!system(buf));
    free(buf);
    buf = "";
    my_strcat_several(&buf, 4, folder, "/", "Function", ".mma");
    file = fopen(buf, "w+");

    if (in->ode != NULL)
	print_dependent_var(file, S);

    print_function_in_mma(file, name, MMASTRING, S);

    fclose(file);
    free(buf);
    buf = "";
}
