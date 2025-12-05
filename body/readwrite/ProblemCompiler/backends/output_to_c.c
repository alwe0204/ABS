/** @file 
 *@author Alexander Weber (a.weber@unibw.de)
 *@data 06 Feb 2016
 *@brief Functions to output the input in c
*/
#include <assert.h>
#include <stdio.h>

#include <string.h>
#include <ctype.h>

#include "output_to_c.h"

#include "expressions.h"
#include "to_string.h"
#include "output_aux.h"
#include "output.h"

void
output_function_declaration_in_c(FILE * file, FORMAT f,
				 const struct function *in,
				 const struct task *op, ConstSequenceOfStates S)
{
    Identifier ptr;
    char *comma = "(";
    char *stars;
    char *type;
    switch (f) {
    case C_FORMAT:
	type = "double";
	break;
    case C_IA_FORMAT:
	type = "interval_t";
	break;
    default:
	assert(0);
    }
    if (FCT_NAME(in) != NULL)
	fprintf(file, "void %s", FCT_NAME(in));
    else {
	stars = generate_identifier("fct", S);
	fprintf(file, "void %s", stars);
	free(stars);
    }
    for (ptr = (Expression) wind_back(AST_EXPR, in->img); ptr != NULL; ptr = ptr->next) {
	Variable x = find_in_variables(ptr, S);
	x = version_of_variable(x, 0, 0, get_date_of_postfix(ptr, S));
	if (col_dim_of(x) > 1)
	    stars = "**";
	else
	    stars = "*";
	fprintf(file, "%s%s %s%s", comma, type, stars, NAME(ptr));
	comma = ",";
    }
    if (f == C_IA_FORMAT) {
	char *v = generate_identifier("v", S);
	fprintf(file, "%sinterval_t *%s", comma, v);
    }
    for (ptr = (Expression) wind_back(AST_EXPR, in->var); ptr != NULL; ptr = ptr->next) {
	if (!strcmp(NAME(ptr), op->timevar_name)) {
	    fprintf(file, "%s%s %s", comma, type, NAME(ptr));
	} else {
	    fprintf(file, "%s %s *%s", comma, type, NAME(ptr));
	}
	comma = ",";
    }
    fprintf(file, ")");
}

void output_inputs_to_c(FILE * file, FILE * fileheader, FORMAT f, const char *type,
			ConstSequenceOfStates S)
{
    size_t i;
    char *comma = "";
    char *rel;
    if (f == C_IA_FORMAT) {
	rel = "subset";
    } else {
	rel = "in";
    }
    Variable ptr;
    if (type != NULL) {
	for (i = 0; i < get_states_len(S); i++) {
	    ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	    if (var_role_of(ptr) == INPUT_VAR) {
		fprintf(file, "%s%s%s", comma, type, name_of(ptr));
		fprintf(fileheader, "%s%s%s", comma, type, name_of(ptr));
		comma = ",";
	    }
	}
	for (i = get_states_len(S); i--;) {
	    ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	    if (var_role_of(ptr) == INPUTCONST_VAR) {
		fprintf(file, "%s%s%s", comma, type, name_of(ptr));
		fprintf(fileheader, "%s%s%s", comma, type, name_of(ptr));
		comma = ",";
	    }
	}
    } else {
	char *param = "* @param";
	for (i = 0; i < get_states_len(S); i++) {
	    ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	    if (var_role_of(ptr) == INPUT_VAR) {
		char *buf = data_of_identifier_to_hyperinterval_string(ptr, MY_FORMAT, MYSTRING, S);
		fprintf(fileheader, "\n%s %s Initial value. Assumptions: %s %s %s", param,
			name_of(ptr), name_of(ptr), rel, buf);
		free(buf);
	    }
	}
	for (i = 0; i < get_states_len(S); i++) {
	    ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	    if (var_role_of(ptr) == INPUTCONST_VAR) {
		char *buf = data_of_identifier_to_hyperinterval_string(ptr, MY_FORMAT, MYSTRING, S);
		fprintf(fileheader, "\n%s %s Parameter. Assumptions: %s %s %s", param, name_of(ptr),
			name_of(ptr), rel, buf);
	    }
	}
    }
}

void
output_outputstates_to_c(FILE * file, FILE * header, char **comma, char *out_type,
			 char **coeff_strings, const struct task *op)
{
    size_t i, idx;
    for (i = 0; i < op->outputstates_len; i++) {
	idx = op->outputstates[i];
	fprintf(file, "%s%s%s", *comma, out_type, coeff_strings[idx]);
	fprintf(header, "%s%s%s", *comma, out_type, coeff_strings[idx]);
	*comma = ",";
    }

}

void
output_coeff_to_c(FILE * file, FILE * fileheader,
		  char **coeff_strings, const struct task *op,
		  ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    char *comma = "";
    size_t i, idx, j;
    char *upper;

    FILE *files[2] = { file, fileheader };

    look_for_keywords_of_C(S);
    char *first = generate_identifier("first", S);
    char *last = generate_identifier("last", S);
    char *v = generate_identifier("v", S);

    upper = ToUpperCase(op->coeff_function_name);

    fprintf(fileheader, "#ifndef %s_H\n#define %s_H\n\n", upper, upper);

    free(upper);

    fprintf(fileheader, "#define NUMBER_OF_VARIABLES %ld\n", get_number_of_variables(tri));
    fprintf(fileheader, "#define MAX_ORDER %d\n", op->max_order);

    char *param = "* @param";
    char *brief = "* @brief";
    Variable ptr;
    fprintf(fileheader, "/**\n");
    fprintf(fileheader,
	    "%s This procedure computes the Taylor coefficients for order %s up to order %s", brief,
	    first, last);
    fprintf(fileheader, "\n* provided that it was previously executed for orders 0 up to %s-1",
	    first);
    fprintf(fileheader, "\n* and %s is holding the corresponding auxiliary variables\n", v);
    for (i = 0; i < op->outputstates_len; i++) {
	idx = op->outputstates[i];
	ptr = op->states[idx];
	fprintf(fileheader, "%s %s Matrix holding the coefficients for state %s: ", param,
		coeff_strings[idx], name_of(ptr));
	fprintf(fileheader,
		"The entry in the ith row and jth column is the component j of the coefficient of order i-1\n");
    }
    fprintf(fileheader, "%s %s Array of auxiliary variables. ", param, v);
    fprintf(fileheader,
	    "Assumptions: 1) %s has length %lu; 2) %s has already been computed for orders 0 .. %s - 1 \n",
	    v, get_number_of_variables(tri), v, first);
    fprintf(fileheader, "%s %s Lower coefficient order to compute. \n", param, first);
    fprintf(fileheader,
	    "%s %s Upper coefficient order to compute. Assumptions: %s is not greater than %d\n",
	    param, last, last, op->max_order);
    for (i = 0; i < get_states_len(S); i++) {
	ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	if (var_role_of(ptr) == INPUT_VAR) {
	    char *buf = data_of_identifier_to_hyperinterval_string(ptr, MY_FORMAT, MYSTRING, S);
	    fprintf(fileheader, "%s %s Initial value. Assumptions: %s in %s\n", param, name_of(ptr),
		    name_of(ptr), buf);
	    free(buf);
	}
    }
    for (i = 0; i < get_states_len(S); i++) {
	ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	if (var_role_of(ptr) == INPUTCONST_VAR) {
	    char *buf = data_of_identifier_to_hyperinterval_string(ptr, MY_FORMAT, MYSTRING, S);
	    if (!strcmp(op->inputvar_name, name_of(ptr))) {
		fprintf(fileheader, "%s %s Control value. Assumptions: %s in %s\n", param,
			name_of(ptr), name_of(ptr), buf);
	    } else {
		fprintf(fileheader, "%s %s Constant parameter. Assumptions: %s in %s\n", param,
			name_of(ptr), name_of(ptr), buf);
	    }
	}
    }
    fprintf(fileheader, "\n*/\n");

    fprintf(file, "#include <math.h>\n\n");
    fprintf(file, "void %s(", op->coeff_function_name);
    fprintf(fileheader, "void %s(", op->coeff_function_name);
    comma = "";
    output_outputstates_to_c(file, fileheader, &comma, "double **", coeff_strings, op);
/*
    for (i = 0; i < op->outputstates_len; i++) {
	idx = op->outputstates[i];
	fprintf(file, "%sdouble **%s", comma, coeff_strings[idx]);
	fprintf(fileheader, "%sdouble **%s", comma, coeff_strings[idx]);
	comma = ",";
    }
*/
    for (j = 0; j < 2; j++) {
	fprintf(files[j], "%sdouble *%s,", comma, v);
	fprintf(files[j], "unsigned int %s,", first);
	fprintf(files[j], "unsigned int %s,", last);
    }
    output_inputs_to_c(file, fileheader, C_FORMAT, "const double *", S);
    fprintf(fileheader, ");\n");

    fprintf(file, ")\n{\n");
    output_three_variable_list(file, C_FORMAT, coeff_strings, op->acc_dim, tri, S);
    fprintf(file, "\n}\n");
    fprintf(fileheader, "#endif\n");
    free(first);
    free(last);
    free(v);
}


void
output_coeff_to_c_ia(FILE * file, FILE * header,
		     char **coeff_strings, const struct task *op,
		     ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    size_t j;
    char *comma = "";
    char *v = generate_identifier("v", S);
    FILE *files[2] = { file, header };
    char *upper = ToUpperCase(op->coeff_function_name);
    fprintf(header, "#ifndef %s_IA_H\n#define %s_IA_H\n\n", upper, upper);
    fprintf(header, "#include \"interval_library.h\"\n");
    free(upper);
    fprintf(header, "#define NUMBER_OF_VARIABLES %ld\n", get_number_of_variables(tri));
    fprintf(header, "#define MAX_ORDER %d\n", op->max_order);
    fprintf(header, "const unsigned int number_of_variables_%s_ia = NUMBER_OF_VARIABLES;\n",
	    op->coeff_function_name);
    fprintf(file, "#include \"interval_library.h\"\n\n");
    fprintf(file, "void %s_ia(", op->coeff_function_name);
    fprintf(header, "void %s_ia(", op->coeff_function_name);
    comma = "";
    output_outputstates_to_c(file, header, &comma, "interval_t **", coeff_strings, op);
/*
    for (i = 0; i < op->outputstates_len; i++) {
	size_t idx = op->outputstates[i];
	for (j = 0; j < 2; j++) {
	    fprintf(files[j], "%sinterval_t **%s", comma, coeff_strings[idx]);
	}
	comma = ",";
    }
*/
    for (j = 0; j < 2; j++) {
	fprintf(files[j], "%sinterval_t *%s", comma, v);
	fprintf(files[j], ",");
    }
    output_inputs_to_c(file, header, C_IA_FORMAT, "interval_t *", S);
    fprintf(header, ");\n");
    fprintf(file, ")\n{\n");
    output_three_variable_list(file, C_IA_FORMAT, coeff_strings, op->acc_dim, tri, S);
    fprintf(file, "\n}\n");
    fprintf(header, "#endif\n");
}

void
output_Jacobian_to_c(FILE * file, FILE * fileheader,
		     char **coeff_strings, char *name,
		     const struct task *op, ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    size_t i, j;
    char *comma = "";
    char *v = generate_identifier("v", S);
    const size_t len = get_three_variable_list_len(tri);
    FILE *files[2] = { file, fileheader };
    fprintf(fileheader, "#define NUMBER_OF_VARIABLES %ld\n", get_number_of_variables(tri));
    fprintf(fileheader, "const unsigned int number_of_variables_Jacobian = NUMBER_OF_VARIABLES;\n");

    char *param = "* @param";
    char *brief = "* @brief";
    fprintf(fileheader, "/**\n");
    fprintf(fileheader,
	    "%s This procedure evaluates the first derivative of the specified function with interval arithmetic.\n",
	    brief);
    for (i = 0; i < op->outputstates_len; i++) {
	size_t idx = op->outputstates[i];
	Variable ptr = op->states[idx];
	fprintf(fileheader,
		"%s %s First derivative with respect to %s. Assumptions: %s is a two-dimensional array of length (%d,%d)\n",
		param, coeff_strings[idx], name_of(ptr), coeff_strings[idx], row_dim_of(ptr),
		row_dim_of(ptr));
    }
    fprintf(fileheader, "%s %s Array of auxiliary variables. Assumptions: %s has length %ld", param,
	    v, v, get_number_of_variables(tri));
    output_inputs_to_c(file, fileheader, C_IA_FORMAT, NULL, S);
    fprintf(fileheader, "\n*/\n");

    fprintf(file, "#include \"interval_library.h\"\n\n");
    fprintf(file, "void %s(", op->coeff_function_name);
    fprintf(fileheader, "void %s(", op->coeff_function_name);
    comma = "";
    output_outputstates_to_c(file, fileheader, &comma, "interval_t **", coeff_strings, op);
/*
    for (i = 0; i < op->outputstates_len; i++) {
	size_t idx = op->outputstates[i];
	for (j = 0; j < 2; j++) {
	    fprintf(files[j], "%sinterval_t **%s", comma, coeff_strings[idx]);
	}
	comma = ",";
    }
*/
    for (j = 0; j < 2; j++) {
	fprintf(files[j], "%sinterval_t *%s", comma, v);
	fprintf(files[j], ",");
    }
    output_inputs_to_c(file, fileheader, C_IA_FORMAT, "interval_t *", S);
    fprintf(fileheader, ");\n");
    fprintf(file, ")\n{\n");
    for (i = 0; i < len; i++) {
	output_three_variable_list_entry_output(file, C_IA_FORMAT,
						NUMERICAL, v, i,
						coeff_strings,
						op->acc_dim, (&output_jacobian_entry), tri, S);
    }
    fprintf(file, "\n}\n");
}


void
output_function_in_c(FILE * file, FILE * header,
		     const struct function *in,
		     const struct task *op, ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    output_function_declaration_in_c(file, C_FORMAT, in, op, S);
    output_function_comment(header, C_FORMAT, in, op, tri, S);
    output_function_declaration_in_c(header, C_FORMAT, in, op, S);
    fprintf(header, ";");
    fprintf(file, "\n{\n");
    output_evaluation_trace_variable(file, C_FORMAT, "double", tri, S);
    output_evaluation_trace(file, C_FORMAT, NUMERICAL, in, tri, S);
    fprintf(file, "\n}\n");
}

void
output_function_in_c_ia(FILE * file, FILE * header,
			const struct function *in,
			const struct task *op, ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    fprintf(header, "#define NUMBER_OF_VARIABLES %ld\n", get_number_of_variables(tri));
    fprintf(header, "const unsigned int number_of_variables = NUMBER_OF_VARIABLES;\n");
    fprintf(file, "#include \"interval_library.h\"\n\n");
    output_function_declaration_in_c(file, C_IA_FORMAT, in, op, S);
    output_function_comment(header, C_IA_FORMAT, in, op, tri, S);
    output_function_declaration_in_c(header, C_IA_FORMAT, in, op, S);
    fprintf(header, ";");
    fprintf(file, "\n{\n");
    output_evaluation_trace(file, C_IA_FORMAT, NUMERICAL, in, tri, S);
    fprintf(file, "\n}\n");
}

void Horner_to_string_in_c(FILE * file, char *out_name, char *time, char *name, dim_t order,
			   dim_t dim, ConstSequenceOfStates S)
{
    char *iter = generate_identifier("j", S);
    char *tmp = generate_identifier("tmp", S);
    char *i = generate_identifier("i", S);

    fprintf(file, "size_t %s,%s;\n", iter, i);
    fprintf(file, "double %s;\n", tmp);
    fprintf(file, "for(%s=0;%s<=%d;%s++)\n{\n", i, i, dim, i);
    fprintf(file, "%s = %s*%s[%d][%s];\n", tmp, time, name, order, i);
    fprintf(file, "for(%s=1;%s<=%d;%s++)\n{\n", iter, iter, order - 1, iter);
    fprintf(file, "%s = %s*(%s + %s[%d-%s][%s]);", tmp, time, tmp, name, order, iter, i);
    fprintf(file, "\n}\n");
    fprintf(file, "%s[%s] = %s + %s[0][%s];\n}\n", out_name, i, tmp, name, i);
    free(iter);
    free(tmp);
    free(i);
}

void output_taylor_to_c(FILE * file, FILE * header,
			const struct ASTNode *in,
			const struct task *op, ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    FunctionDefinition fct = in->Function;
    Ode ode = in->ode;
    char *v = generate_identifier("v", S);
    const size_t len = get_three_variable_list_len(tri);
    size_t i, k;
    char *comma = "(";
    fprintf(file, "void %s", FCT_NAME(ode->phi));
    fprintf(header, "void %s", FCT_NAME(ode->phi));
    for (i = 0; i < op->outputstates_len; i++) {
	fprintf(file, "%sdouble *%s", comma, op->out_strings[i]);
	fprintf(header, "%sdouble *%s", comma, op->out_strings[i]);
	comma = ",";
    }
    fprintf(file, ",");
    fprintf(header, ",");
    Variable x = NULL;
    for (i = get_states_len(S); i--;) {
	x = get_var(i, S);
	x = version_of_variable(x, 0, 0, get_date(S));
	if (x == NULL)
	    continue;

	if (TIME_VAR == var_role_of(x)) {
	    break;
	}
    }
    assert(x != NULL);
    fprintf(file, "double %s,", name_of(x));
    fprintf(header, "double %s,", name_of(x));
    output_inputs_to_c(file, header, C_FORMAT, "const double *", S);
    fprintf(file, ")");
    fprintf(header, ")");
    fprintf(header, ";");
    fprintf(file, "\n{\n");
    output_evaluation_trace_variable(file, C_FORMAT, "double", tri, S);
    for (k = 0; k < op->outputstates_len; k++) {
	size_t idx = op->outputstates[k];
	ConstVariable ptr = op->states[idx];
	fprintf(file, "double %s[%d][%d];\n", op->coeff_strings[idx],
		op->max_order + 1, row_dim_of(ptr));
    }
    for (i = 0; i < len; i++) {
	output_three_variable_list_entry_output(file, C_FORMAT, NUMERICAL, v, i,
						op->coeff_strings, op->acc_dim,
						(&output_coefficients_entry), tri, S);
	if (get_role_of_var(i, tri) == STATE_VAR) {
	    char *buf = var_string(v, i, C_FORMAT);
	    output_coefficients_entry(file, C_FORMAT, i, op->acc_dim, op->coeff_strings, buf, tri);
	    free(buf);
	}
    }
    for (k = 0; k < op->outputstates_len; k++) {
	size_t tmp = op->outputstates[k];
	Horner_to_string_in_c(file, op->out_strings[k], NAME(ode->time),
			      op->coeff_strings[tmp], op->max_order, fct->img_dim[tmp + 1] - 1, S);
    }
    fprintf(file, "\n}\n");
}
