/** @file 
 *@author Alexander Weber (a.weber@unibw.de)
 *@data 06 Feb 2016
 *@brief Functions to output the input
*/
#include <assert.h>
#include <stdio.h>

#include <string.h>
#include <ctype.h>

#include "my_strcat.h"
#include "output.h"

#include "expressions.h"
#include "reduce_int.h"
#include "errors.h"

#include "output_aux.h"

#include "ia.h"




/** 
 * @brief Writes on \p buf the string representation in format \p f of a variable. 
 * @param name  The name (identifier) of the variable
 * @param idx   The index of the variable
 * @param f     The output format
 * @param buf   The location to write the string
 * @returns buf
 * Assumptions:
 * \p idx != -1
*/
char *var_string(const char *name, const var3_t idx, FORMAT f)
{
    assert(idx != -1);
    char *le = "_", *ri = "";
    char *buf = "";
    switch (f) {
    case ADA_FORMAT:
    case MY_FORMAT:
    case C_FORMAT:
    case C_IA_FORMAT:
    case MMA_FORMAT:
	buf = vector_entry_to_string_int(name, idx, format_to_string_format(f));
	break;
    case C_FORMAT_WITH_LOOPS:
    case MY_FORMAT_WITH_LOOPS:
	buf = matrix_entry_to_string_int(name, idx, 0, format_to_string_format(f));
	break;
    case GAPPA_FORMAT:
	buf = my_malloc((strlen(name) + strlen(le) + strlen(ri) + 15 + 1) * sizeof(char));
	sprintf(buf, "%s%s%lu%s_g", name, le, idx, ri);
	break;
    case GAPPA_MATH_FORMAT:
	buf = my_malloc((strlen(name) + strlen(le) + strlen(ri) + 15 + 1) * sizeof(char));
	sprintf(buf, "M%s%s%lu%s_g", name, le, idx, ri);
	break;
    default:
	assert(0);
	break;
    }
    return buf;
}

void
output_assignment(FILE * file, FORMAT f,
		  const enum expression_type type,
		  const enum set_R role, const char *lhs, const char *rhs)
{
    switch (f) {
    case C_IA_FORMAT:
	{
	    switch (type) {
	    case CONT_INTV:
	    case DISC_INTV:
		{
		    char *buf = "";
		    my_strcat(&buf, rhs);
		    char *a = strtok(buf, ",");
		    char *b = strtok(NULL, ",");
		    fprintf(file, "IA_SET_INTERVAL(%s,%s,%s);", lhs, a, b);
		    free(buf);
		}
		break;
	    case NUM_CONST:
		{
		    size_t i;
		    char *math_const[] = { "Pi", "Exp" };
		    for (i = 0; i < sizeof(math_const) / sizeof(math_const[0]); i++) {
			if (!strcmp(math_const[i], rhs))
			    break;
		    }
		    switch (i) {
		    case 0:
			fprintf(file, "mpfi_const_pi(%s);", lhs);
			break;
		    case 1:
			fprintf(file, "IA_SET_NUM_DEC(%s,M_E);", lhs);
			break;
		    default:
			assert(0);
		    }
		}
		break;
	    case NUM_INT:
	    case NUM_DEC:
	    case POSTFIX:
		if (role != INPUT_VAR)
		    fprintf(file, "IA_SET_NUM_DEC(%s,%s);", lhs, rhs);
		else
		    fprintf(file, "IA_SET(%s,%s);", lhs, rhs);
		break;
	    default:
		fprintf(file, "IA_SET(%s,%s);", lhs, rhs);
		break;
	    }
	}
	break;
    default:
	{
	    char *eq = string_of_assignment(f);
	    fprintf(file, "%s %s %s;", lhs, eq, rhs);
	    free(eq);
	}
	break;
    }
}

void
output_output_var(FILE * file, FORMAT f, const var3_t i,
		  const dim_t * acc_dim, char **coeff_names,
		  const char *va, ConstThreeVariableList tri)
{
    dim_t idx = 0;
    const dim_t dim = get_dim_of_var(i, tri);
    while (dim > acc_dim[idx + 1]) {
	idx++;
    }
    const dim_t row = dim - acc_dim[idx] - 1;

    switch (f) {
    case GAPPA_FORMAT:
	{
	    fprintf(file, "%s_%d_g %s= %s;\n", coeff_names[idx], row, GAPPA_ROUNDING_KEYWORD, va);
	}
	break;
    case GAPPA_MATH_FORMAT:
	{
	    fprintf(file, "M%s_%d_g = %s;\n", coeff_names[idx], row, va);
	}
	break;
    default:
	{
	    char *a = vector_entry_to_string_int(coeff_names[idx], row,
						 format_to_string_format(f));
	    output_assignment(file, f, 0, 0, a, va);
	    fprintf(file, "\n");
	    free(a);
	}
	break;
    }

}

void
output_coefficients_entry(FILE * file, FORMAT f, var3_t i,
			  const dim_t * acc_dim, char **coeff_names,
			  const char *va, ConstThreeVariableList tri)
{
    dim_t idx = 0;
    const dim_t dim = get_dim_of_var(i, tri);
    const order_t ord = get_order_of_var(i, tri);
    const dim_t row = ord;
    while (dim > acc_dim[idx + 1]) {
	idx++;
    }
    const dim_t col = dim - acc_dim[idx] - 1;
    char *a = matrix_entry_to_string_int(coeff_names[idx], row, col,
					 format_to_string_format(f));
    output_assignment(file, f, 0, 0, a, va);
    fprintf(file, "\n");
    free(a);
}


void
output_jacobian_entry(FILE * file, FORMAT f, const var3_t i,
		      const dim_t * acc_dim, char **coeff_names,
		      const char *va, ConstThreeVariableList tri)
{
    dim_t idx = 0;
    const dim_t dim = get_dim_of_var(i, tri);
    const order_t ord = get_order_of_var(i, tri);
    const dim_t col = ord;	/* Here is a transposition taking place */
    while (dim > acc_dim[idx + 1]) {
	idx++;
    }
    const dim_t row = dim - acc_dim[idx] - 1;
    char *a = matrix_entry_to_string_int(coeff_names[idx], row, col,
					 format_to_string_format(f));
    output_assignment(file, f, 0, 0, a, va);
    fprintf(file, "\n");
    free(a);
}

void
output_concrete_coefficient_entry(FILE * file, FORMAT f, var3_t i,
				  const dim_t * acc_dim, char **coeff_names,
				  const char *va, ConstThreeVariableList tri)
{
    static int order = 0;
    static int stat = 0;
    if (file == NULL && stat == 1) {
	order = 0;
    }
    if (file == NULL) {
	order++;
	return;
    } else {
	stat = 1;
    }
    dim_t idx = 0;
    const dim_t dim = get_dim_of_var(i, tri);
    const order_t ord = get_order_of_var(i, tri);
    if (order == ord) {
	while (dim > acc_dim[idx + 1]) {
	    idx++;
	}
	const dim_t col = dim - acc_dim[idx] - 1;
	char *a = vector_entry_to_string_int(coeff_names[idx], col,
					     format_to_string_format(f));
	output_assignment(file, f, 0, 0, a, va);
	fprintf(file, "\n");
	free(a);
    }
}

int is_lhs_x_plusminus_zero(const var3_t i, ConstThreeVariableList tri)
{

    return ((get_type_of_var(i, tri) == PLUS || get_type_of_var(i, tri) == MINUS)
	    && is_var_zero(get_rght_var(i, tri), tri));
}

int is_lhs_zero_plus_x(const var3_t i, ConstThreeVariableList tri)
{

    return ((get_type_of_var(i, tri) == PLUS)
	    && is_var_zero(get_left_var(i, tri), tri));
}

int is_lhs_zero_minus_x(const var3_t i, ConstThreeVariableList tri)
{

    return ((get_type_of_var(i, tri) == MINUS)
	    && is_var_zero(get_left_var(i, tri), tri));
}



char *definition_of_variable_to_string(const var3_t initializer,
				       const char *v,
				       const var3_t i,
				       const enum number_format mod,
				       const enum output_format f,
				       ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    char *buf = "";
    const enum string_format style = format_to_string_format(f);

    if (is_var_zero(i, tri)) {
	return int_to_string(0, mod);
    }
    if (initializer != i && is_used_twice(i, tri)) {
	assert(i < initializer);
	buf = var_string(v, i, f);
    } else if (is_Expression_type_number(get_type_of_var(i, tri))) {
	if (get_role_of_var(i, tri) == INPUT_VAR) {
	    buf = Expression_to_string(get_value_of_var(i, tri), SYMBOLIC, style, S);
	} else {
	    buf = Expression_to_string(get_value_of_var(i, tri), mod, style, S);
	}
    } else if (is_Expression_type_elementary_operation(get_type_of_var(i, tri))) {
	if (is_var_zero(get_left_var(i, tri), tri)
	    && is_var_zero(get_rght_var(i, tri), tri)) {
	    buf = int_to_string(0, mod);
	} else if (is_lhs_zero_plus_x(i, tri)) {
	    buf =
		definition_of_variable_to_string(initializer, v,
						 get_rght_var(i, tri), mod, f, tri, S);
	} else if (is_lhs_zero_minus_x(i, tri)) {
	    char *buf2 = definition_of_variable_to_string(initializer, v,
							  get_rght_var(i,
								       tri),
							  mod,
							  f, tri, S);
	    buf = "";
	    my_strcat_several(&buf, 3, "(-", buf2, ")");
	    free(buf2);
	} else if (is_lhs_x_plusminus_zero(i, tri)) {
	    buf =
		definition_of_variable_to_string(initializer, v,
						 get_left_var(i, tri), mod, f, tri, S);
	} else {
	    char *left = definition_of_variable_to_string(initializer, v,
							  get_left_var(i,
								       tri),
							  mod,
							  f, tri, S);
	    char *rght = definition_of_variable_to_string(initializer, v,
							  get_rght_var(i,
								       tri),
							  mod,
							  f, tri, S);
	    buf = function_to_string(left, rght, get_type_of_var(i, tri), f);
	    free(left);
	    free(rght);
	}
    } else if (is_Expression_type_function(get_type_of_var(i, tri))) {
	char *rght = definition_of_variable_to_string(initializer, v,
						      get_rght_var(i, tri),
						      mod, f,
						      tri, S);
	buf = function_to_string(NULL, rght, get_type_of_var(i, tri), f);
	free(rght);
    }
    return buf;
}

char *lower_upper_bound_to_string(double a, double b)
{
    char *buff;
    char *buf2;
    buf2 = double_to_string(a, CSTRING);
    buff = "";
    my_strcat(&buff, buf2);
    free(buf2);
    my_strcat(&buff, ",");
    buf2 = double_to_string(b, CSTRING);
    my_strcat(&buff, buf2);
    free(buf2);
    return buff;
}

void
output_three_variable_list_entry_IA(FILE * file, FORMAT f,
				    const char *v, size_t i,
				    ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    char *buf = var_string(v, i, f);
    char *buf2;
    char *buff;
    double a, b;

    if (is_Expression_type_elementary_operation(get_type_of_var(i, tri))) {
	if (is_var_zero(get_left_var(i, tri), tri)
	    && is_var_zero(get_rght_var(i, tri), tri)) {
	    buf = var_string(v, i, f);
	    fprintf(file, "IA_SET_ZERO(%s);", buf);
	} else if (is_lhs_zero_plus_x(i, tri)) {
	    const var3_t j = get_rght_var(i, tri);
	    buff = definition_of_variable_to_string(i, v, j, NUMERICAL, f, tri, S);
	    output_assignment(file, f, PLUS, get_role_of_var(j, tri), buf, buff);
	    free(buff);
	} else if (is_lhs_zero_minus_x(i, tri)) {
	    buff =
		definition_of_variable_to_string(i, v, get_rght_var(i, tri), NUMERICAL, f, tri, S);
	    switch (get_type_of_var(get_rght_var(i, tri), tri)) {
	    case NUM_INT:
		fprintf(file, "IA_SET_NEG_INT(%s,%s);", buf, buff);	// TODO: What about this case?
		break;
	    case NUM_DEC:
	    case NUM_CONST:
	    default:
		fprintf(file, "IA_SET_NEG(%s,%s);", buf, buff);
		break;
	    }
	    free(buff);
	} else if (is_lhs_x_plusminus_zero(i, tri)) {
	    const var3_t j = get_left_var(i, tri);
	    buff = definition_of_variable_to_string(i, v, j, NUMERICAL, f, tri, S);
	    output_assignment(file, f, get_type_of_var(j, tri), get_role_of_var(j, tri), buf, buff);
	    free(buff);
	    assert(!is_Expression_type_number(get_type_of_var(j, tri)));
	} else {
	    buff =
		definition_of_variable_to_string(i, v, get_rght_var(i, tri), NUMERICAL, f, tri, S);
	    buf2 = var_string(v, get_left_var(i, tri), f);
	    if (get_type_of_var(i, tri) == POW
		&& GET_INT(get_value_of_var(get_rght_var(i, tri), tri)) == 2) {
		fprintf(file, "IA_SQR(%s,%s);", buf, buf2);
	    } else if (get_type_of_var(i, tri) == TIMES
		       && get_rght_var(i, tri) == get_left_var(i, tri)) {
		fprintf(file, "IA_SQR(%s,%s);", buf, buf2);
	    } else {
		char *functionname = _string_of_function(get_type_of_var(i, tri), f);
		fprintf(file, "IA_%s(%s,%s,%s);", ToUpperCase(functionname), buf, buf2, buff);
	    }
	    free(buf2);
	    free(buff);
	}
    } else if (is_Expression_type_function(get_type_of_var(i, tri))) {
	if (is_var_zero(get_rght_var(i, tri), tri))
	    assert(0);
	else {
	    buf2 = var_string(v, get_rght_var(i, tri), f);
	    fprintf(file, "IA_%s(%s,%s);",
		    ToUpperCase(_string_of_function(get_type_of_var(i, tri), CSTRING)), buf, buf2);
	    free(buf2);
	}
    } else if (is_Expression_type_number(get_type_of_var(i, tri))) {
	if (get_type_of_var(i, tri) == POSTFIX && get_role_of_var(i, tri) != INPUT_VAR) {
	    dim_t row, col;
	    get_indices_of_PostFixExpression(get_value_of_var(i, tri), &row, &col, 0, S);
	    Variable x = find_in_variables_from_string(NAME(get_value_of_var(i, tri)), S);
	    x = version_of_variable(x, 0, 0, get_date(S));
	    if (intv_of(x, row, col) != NULL) {
		Expression intv = intv_of(x, row, col);
		assert(TYPE(intv) == CONT_INTV);
		a = get_lowerbound_of_expression(intv->left, S);
		b = get_upperbound_of_expression(intv->right, S);
	    } else {
		Expression data = data_of(x, row, col);
		a = get_lowerbound_of_expression(data, S);
		b = get_upperbound_of_expression(data, S);
	    }
	    buff = lower_upper_bound_to_string(a, b);
	    output_assignment(file, f, CONT_INTV, get_role_of_var(i, tri), buf, buff);
	    free(buff);
	} else if (get_type_of_var(i, tri) == NUM_DEC && get_role_of_var(i, tri) != INPUT_VAR) {
	    a = get_lowerbound_of_expression(get_value_of_var(i, tri), S);
	    b = get_upperbound_of_expression(get_value_of_var(i, tri), S);
	    buff = lower_upper_bound_to_string(a, b);
	    output_assignment(file, f, CONT_INTV, get_role_of_var(i, tri), buf, buff);
	    free(buff);
	} else if (get_type_of_var(i, tri) == NUM_CONST && get_role_of_var(i, tri) != INPUT_VAR) {
	    output_assignment(file, f, NUM_CONST, get_role_of_var(i, tri), buf,
			      (char *) get_value_of_var(i, tri)->content);
	} else {
	    buff =
		Expression_to_string(get_value_of_var(i, tri), NUMERICAL,
				     format_to_string_format(f), S);
	    output_assignment(file, f, get_type_of_var(i, tri), get_role_of_var(i, tri), buf, buff);
	    free(buff);
	}
    } else if (get_type_of_var(i, tri) == NOTHING)
	return;
    else {
	assert(0);
    }
    free(buf);
}

void
output_three_variable_list_entry(FILE * file, FORMAT f,
				 enum number_format mod,
				 const char *v, var3_t i,
				 ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    char *buf;
    char *buf2;
    static size_t cchck = 0;
    if (is_unused(i, tri)) {
	return;
    } else {
	if (f == C_IA_FORMAT) {
	    output_three_variable_list_entry_IA(file, f, v, i, tri, S);
	} else {
	    buf2 = definition_of_variable_to_string(i, v, i, mod, f, tri, S);
	    if (f == GAPPA_FORMAT && is_Expression_type_function(get_type_of_var(i, tri))
		&& get_type_of_var(i, tri) != FCT_SQRT) {
		char *buf3 = "";
		my_strcat_several(&buf3, 4, GAPPA_ROUNDING_KEYWORD, "(", buf2, ")");
		free(buf2);
		buf2 = buf3;
	    }
	    buf = var_string(v, i, f);
	    output_assignment(file, f, 0, 0, buf, buf2);
	    free(buf2);
	    free(buf);
	}
	if (i != 0 && i != cchck) {
	    printf("%ld %ld\n", i, cchck);
	BAD_ERROR} else {
	    if (i == 0)
		cchck = 1;
	    else
		cchck++;
	}
    }

    if (f == MY_FORMAT) {
	if (get_role_of_var(i, tri) == INPUT_VAR) {
	    fprintf(file, "\t/* INPUT(%d,%d) */", get_row_of_var(i, tri), get_col_of_var(i, tri));
	}
	if (get_role_of_var(i, tri) == OUTPUT_VAR) {
	    fprintf(file, "\t/* OUTPUT */");
	}
	if (get_role_of_var(i, tri) == STATE_VAR) {
	    fprintf(file, "\t/* STATE(%d,%d;%d) */",
		    get_row_of_var(i, tri), get_col_of_var(i, tri), get_dim_of_var(i, tri));
	}
    }
    fprintf(file, "\n");
}

void
output_three_variable_list_entry_output(FILE * file,
					FORMAT f,
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
					ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    char *buf;
    output_three_variable_list_entry(file, f, mod, v, i, tri, S);
    if (get_role_of_var(i, tri) == OUTPUT_VAR) {
	if (is_used_twice(i, tri))
	    buf = var_string(v, i, f);
	else
	    buf = definition_of_variable_to_string(i, v, i, mod, f, tri, S);

	(*out_fct) (file, f, i, acc_dim, out_strings, buf, tri);
	free(buf);
    }
}

void
output_evaluation_trace_variable(FILE * file, FORMAT f, const char *type,
				 ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    char *v = generate_identifier("v", S);
    size_t k = get_number_of_variables(tri);
    assert(k > 0);
    switch (f) {
    case ADA_FORMAT:
	fprintf(file, "%s : %s ( 1 .. %lu );\n", v, type, k);
	break;
    default:
	{
	    char *a = vector_entry_to_string_int(v, k,
						 format_to_string_format(f));
	    fprintf(file, "%s %s;\n", type, a);
	    free(a);
	}
	break;
    }
    free(v);
}

void
output_evaluation_trace(FILE * file, FORMAT f, enum number_format mod,
			const struct function *in,
			ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    size_t i;
    char *v = generate_identifier("v", S);
    Identifier ptr;
    ptr = (Expression) wind_back(AST_EXPR, in->img);
    char **out_strings = my_malloc(in->img_dim[0] * sizeof(char *));
    const size_t len = get_three_variable_list_len(tri);
    for (i = 0; i < in->img_dim[0]; i++) {
	out_strings[i] = NAME(ptr);
	ptr = ptr->next;
    }
    for (i = 0; i < len; i++) {
	output_three_variable_list_entry_output(file, f, mod, v, i,
						out_strings,
						in->img_acc_dim, (&output_output_var), tri, S);
    }
    free(v);
    free(out_strings);
}



void
output_three_variable_list(FILE * file, FORMAT f,
			   char **coeff_strings,
			   const dim_t * acc_dim,
			   ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    size_t i, old = 0;
    char *v = generate_identifier("v", S);
    char *now = NULL;
    char *last = NULL;
    char *first = NULL;
    char *buf;

    const size_t len = get_three_variable_list_len(tri);
    enum number_format mod;
    if (f == C_FORMAT || f == ADA_FORMAT || f == C_IA_FORMAT) {
	now = generate_identifier("k", S);
	first = generate_identifier("first", S);
	last = generate_identifier("last", S);
    }
    switch (f) {
    case ADA_FORMAT:
	mod = NUMERICAL;
	fprintf(file, "for %s in %s .. %s loop\n", now, first, last);
	fprintf(file, "case %s is\n", now);
	fprintf(file, "when %ld =>\n", old);
	break;
    case MY_FORMAT:
	mod = SYMBOLIC;
	buf = vector_entry_to_string_int(v, get_number_of_variables(tri), CSTRING);
	fprintf(file, "Real %s;\n", buf);
	free(buf);
	break;
    case MY_FORMAT_WITH_LOOPS:
	mod = SYMBOLIC;
	buf = vector_entry_to_string_int(v, get_number_of_variables(tri), CSTRING);
	fprintf(file, "Real %s[ord];\n", buf);
	free(buf);
	break;
    case C_FORMAT:
	mod = NUMERICAL;
	fprintf(file, "unsigned int %s;\n", now);
	fprintf(file, "for(%s=%s;%s<=%s;%s++)\n{\n", now, first, now, last, now);
	fprintf(file, "switch(%s)\n{\n", now);
	fprintf(file, "case %ld:\n", old);
	break;
    case C_IA_FORMAT:
	mod = NUMERICAL;
	fprintf(file, "unsigned int %s;\n", now);
	fprintf(file, "for(%s=%s;%s<=%s;%s++)\n{\n", now, first, now, last, now);
	fprintf(file, "switch(%s)\n{\n", now);
	fprintf(file, "case %ld:\n", old);
	break;
    case C_FORMAT_WITH_LOOPS:
	mod = NUMERICAL;
	break;
    default:
	assert(0);
    }
    for (i = 0; i < len; i++) {
	if (f == C_FORMAT || f == ADA_FORMAT || f == C_IA_FORMAT) {
	    if (get_order_of_var(i, tri) > old) {
		old = get_order_of_var(i, tri);
		if (f == C_FORMAT || f == C_IA_FORMAT) {
		    fprintf(file, "break;\n");
		    fprintf(file, "case %ld:\n", old);
		}
		if (f == ADA_FORMAT) {
		    fprintf(file, "when %ld =>\n", old);
		}
	    }
	}
	output_three_variable_list_entry_output(file, f, mod, v, i,
						coeff_strings, acc_dim,
						(&output_coefficients_entry), tri, S);
	if (get_role_of_var(i, tri) == STATE_VAR) {
	    buf = var_string(v, i, f);
	    output_coefficients_entry(file, f, i, acc_dim, coeff_strings, buf, tri);
	    free(buf);
	}
    }
    free(v);
    switch (f) {
    case ADA_FORMAT:
	fprintf(file, "when others => null;\nend case;\n");
	fprintf(file, "end loop;\n");
    case MY_FORMAT:
    case MY_FORMAT_WITH_LOOPS:
	break;
    case C_FORMAT:
	fprintf(file, "\nbreak;\n");
	fprintf(file, "default: break;\n}\n");
	fprintf(file, "}\n");
	break;
    case C_IA_FORMAT:
	fprintf(file, "\nbreak;\n");
	fprintf(file, "default: break;\n}\n");
	fprintf(file, "}\n");
	break;
    case C_FORMAT_WITH_LOOPS:
	break;
    default:
	assert(0);
    }
    if (f == C_FORMAT || f == ADA_FORMAT) {
	free(now);
	free(last);
	free(first);
    }
}


/** @brief Prints a function declaration in symbolic ascii math
 *  @param The favorite name of the function (will be changed if it causes conflicts to other identifiers
 *  @param S The current state \f$\sigma \in S \f$
*/

void output_function_declaration(FILE * file, char *name, ConstSequenceOfStates S)
{
    if (name == NULL) {
	fprintf(file, "Function: ");
	return;
    }
    /* char *out = generate_identifier(name, S); 
       fprintf(file, "%s: ", out); 
       free(out); */
    fprintf(file, "%s: ", name);
}

void make_strings(struct task *op, ConstSequenceOfStates S)
{
    size_t i;
    char *c;
    char *buf = "";
    struct variable *ptr;
    op->coeff_strings = my_malloc(op->states_len * sizeof(char *));
    op->out_strings = my_malloc(op->outputstates_len * sizeof(char *));
    for (i = op->states_len; i--;) {
	op->coeff_strings[i] = NULL;
    }
    for (i = op->outputstates_len; i--;) {
	op->out_strings[i] = NULL;
    }
    for (i = 0; i < op->outputstates_len; i++) {
	ptr = op->states[op->outputstates[i]];
	buf = "";
	my_strcat_several(&buf, 2, "c", name_of(ptr));
	c = generate_identifier(buf, S);
	free(buf);
	op->coeff_strings[var_idx(ptr) - 1] = c;
    }
    for (i = 0; i < op->outputstates_len; i++) {
	ptr = op->states[op->outputstates[i]];
	buf = "";
	my_strcat_several(&buf, 2, name_of(ptr), "_out");
	c = generate_identifier(buf, S);
	free(buf);
	op->out_strings[i] = c;
    }
}




void
output_function_head_image_coefficients(FILE * file, char **names,
					const struct task *op, ConstSequenceOfStates S)
{
    size_t i, idx;
    struct variable *ptr;
    char *buf;
    char *buf2 = "";
    char *comma3 = "";
    for (i = 0; i < op->outputstates_len; i++) {
	idx = op->outputstates[i];
	ptr = op->states[idx];
	buf = matrix_entry_to_string_int(names[idx], op->max_order + 1, row_dim_of(ptr), CSTRING);
	my_strcat_several(&buf2, 2, comma3, buf);
	free(buf);
	comma3 = ",";
    }
    if (strstr(buf2, ",") != NULL)
	fprintf(file, "(%s)", buf2);
    else
	fprintf(file, "%s", buf2);
    free(buf2);
}

void
output_function_head_image_concrete_coefficient(FILE * file, char **names,
						const struct task *op, ConstSequenceOfStates S)
{
    size_t i, idx;
    struct variable *ptr;
    char *buf;
    char *buf2 = "";
    char *comma3 = "";
    for (i = 0; i < op->outputstates_len; i++) {
	idx = op->outputstates[i];
	ptr = op->states[idx];
	buf = vector_entry_to_string_int(names[idx], row_dim_of(ptr), CSTRING);
	my_strcat_several(&buf2, 2, comma3, buf);
	free(buf);
	comma3 = ",";
    }
    if (strstr(buf2, ",") != NULL)
	fprintf(file, "(%s)", buf2);
    else
	fprintf(file, "%s", buf2);
    free(buf2);
}


void
output_function_head_image_Jac(FILE * file, char **names,
			       const struct task *op, ConstSequenceOfStates S)
{
    size_t i, idx;
    struct variable *ptr;
    char *buf = "";
    char *buf2 = "";
    char *comma3 = "";
    for (i = 0; i < op->outputstates_len; i++) {
	idx = op->outputstates[i];
	ptr = op->states[idx];
	const dim_t tmp = op->acc_dim[idx + 1] - op->acc_dim[idx];
	buf = matrix_entry_to_string_int(names[idx], tmp, row_dim_of(ptr), CSTRING);
	my_strcat_several(&buf2, 2, comma3, buf);
	free(buf);
	comma3 = ",";
    }
    if (strstr(buf2, ",") != NULL)
	fprintf(file, "(%s)", buf2);
    else
	fprintf(file, "%s", buf2);
    free(buf2);
}

void
output_function_head_arguments(FILE * file, size_t * roles,
			       const struct task *op, ConstSequenceOfStates S)
{
    size_t i;
    dim_t k, b = 0, l;
    struct variable *ptr;
    char *buf = "";
    char *buf3 = "";
    char *comma = "";
    char *comma2 = "";
    char *a;
    const size_t len = get_states_len(S);
    for (k = 1; k <= roles[0]; k++) {
	for (i = len; i--;) {
	    ptr = get_var(i, S);
	    ptr = version_of_variable(ptr, 0, 0, get_date(S));
	    if (ptr == NULL)
		continue;

	    if (roles[k] == var_role_of(ptr)) {
		char *buf2 = "";
		b++;
		comma2 = "";
		for (l = 0; l < row_dim_of(ptr); l++) {
		    assert(data_of(ptr, l, 0) != NULL);
		    a = interval_to_string(data_of(ptr, l, 0), MYSTRING, S);
		    my_strcat_several(&buf2, 2, comma2, a);
		    comma2 = ",";
		    free(a);
		}
		my_strcat(&buf3, comma);
		if (row_dim_of(ptr) > 1) {
		    my_strcat_several(&buf3, 3, "(", buf2, ")");
		} else
		    my_strcat(&buf3, buf2);
		free(buf2);
		buf2 = "";
		my_strcat_several(&buf, 2, comma, name_of(ptr));
		comma = ",";
	    }
	}
    }
    if (strstr(buf, ",") != NULL)
	fprintf(file, "(%s)", buf);
    else
	fprintf(file, "%s", buf);
    fprintf(file, " in ");
    if (b > 1) {
	fprintf(file, "(%s)", buf3);
    } else {
	fprintf(file, "%s", buf3);
    }
    fprintf(file, " to ");
    free(buf);
    free(buf3);
}

void print_declaration(FILE * file, const char *name, const enum set_T type)
{
    fprintf(file, "%s %s;\n", (type == TY_REAL) ? "Real" : "Interval", name);
}

void print_real_in_interval(FILE * file, const char *name, const char *intv)
{
    fprintf(file, "%s in %s;\n", name, intv);
}

void print_variable(FILE * file, ConstVariable x, Expression in, ConstSequenceOfStates S)
{
    char *buf;
    if (var_type_of(x) != TY_FCT && var_type_of(x) != UNDECLARED && var_role_of(x) == ORDINARY_VAR) {
	if (row_dim_of(x) == 1 && col_dim_of(x) == 1) {
	    print_declaration(file, name_of(x), var_type_of(x));
	    // Real Space cannot be the case
	    if (var_type_of(x) == TY_REAL && is_Expression_Interval(intv_of(x, 0, 0))) {
		buf = interval_to_string(intv_of(x, 0, 0), MYSTRING, S);
		print_real_in_interval(file, name_of(x), buf);
		free(buf);
	    }
	    if (var_type_of(x) == TY_REAL && is_Expression_type_number(in->type)) {
		buf = Expression_to_string(in, NUMERICAL, MYSTRING, S);
		output_assignment(file, MY_FORMAT, 0, 0, name_of(x), buf);
		free(buf);
	    }
	    fprintf(file, "\n");
	} else if (col_dim_of(x) == 1) {
	    buf = vector_entry_to_string_int(name_of(x), row_dim_of(x), MYSTRING);
	    print_declaration(file, buf, var_type_of(x));
	    free(buf);
	    dim_t k;
	    for (k = 0; k < row_dim_of(x); k++) {
		if (var_type_of(x) == TY_REAL && is_Expression_Interval(intv_of(x, k, 0))) {
		    buf = interval_to_string(intv_of(x, k, 0), MYSTRING, S);
		    char *buf2 = vector_entry_to_string_int(name_of(x), k,
							    MYSTRING);
		    print_real_in_interval(file, buf2, buf);
		    free(buf);
		    free(buf2);
		}
	    }
	} else {
	    buf = matrix_entry_to_string_int(name_of(x), row_dim_of(x), col_dim_of(x), MYSTRING);
	    print_declaration(file, buf, var_type_of(x));
	    free(buf);
	    dim_t k, l;
	    for (k = 0; k < row_dim_of(x); k++) {
		for (l = 0; l < col_dim_of(x); l++) {
		    if (var_type_of(x) == TY_REAL && is_Expression_Interval(intv_of(x, k, l))) {
			buf = interval_to_string(intv_of(x, k, l), MYSTRING, S);
			char *buf2 = matrix_entry_to_string_int(name_of(x), k, l,
								MYSTRING);
			print_real_in_interval(file, buf2, buf);
			free(buf);
			free(buf2);
		    }
		}
	    }
	}
    }
}

void output_constants(FILE * file, ConstSequenceOfStates S)
{
    size_t i;
    struct variable *ptr;
    const size_t len = get_states_len(S);
    for (i = 0; i < len; i++) {
	ptr = get_var(i, S);
	ptr = version_of_variable(ptr, 0, 0, get_date_of_rhs(S));
	if (is_var_dead(ptr))
	    continue;
	if (var_role_of(ptr) == ORDINARY_VAR) {
	    print_variable(file, ptr, data_of(ptr, 0, 0), S);
	}
    }
}

void
Horner_to_string(FILE * file, char *out_name, char *time, char *name,
		 dim_t order, dim_t dim, ConstSequenceOfStates S)
{
    char *iter = generate_identifier("j", S);
    char *tmp = generate_identifier("tmp", S);
    char *i = generate_identifier("i", S);


    fprintf(file, "for(%s=0,%d,+)\n{\n\t", i, dim);
    fprintf(file, "%s = %s*%s[%d][%s];\n", tmp, time, name, order, i);
    fprintf(file, "\tfor(%s=1,%d,+)\n\t{\n\t", iter, order - 1);
    fprintf(file, "\t%s = %s*(%s + %s[%d-%s][%s]);", tmp, time, tmp, name, order, iter, i);
    fprintf(file, "\t\n\t}\n");
    fprintf(file, "\t%s[%s] = %s + %s[0][%s];\n}\n", out_name, i, tmp, name, i);
    free(iter);
    free(tmp);
    free(i);
}

void
output_Jacobian(FILE * file, FORMAT f, char **coeff_strings,
		char *name, const struct task *op,
		ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    size_t i;

    const size_t len = get_three_variable_list_len(tri);

    output_constants(file, S);
    output_function_declaration(file, name, S);
    size_t roles[3] = { 2, INPUT_VAR, INPUTCONST_VAR };
    output_function_head_arguments(file, roles, op, S);
    output_function_head_image_Jac(file, coeff_strings, op, S);
    fprintf(file, "\n{\n");
    char *v = generate_identifier("v", S);
    output_evaluation_trace_variable(file, f, "Real", tri, S);
    for (i = 0; i < len; i++) {
	output_three_variable_list_entry_output(file, f, SYMBOLIC, v, i,
						coeff_strings, op->acc_dim,
						(&output_jacobian_entry), tri, S);
    }
    fprintf(file, "\n}\n");
    free(v);
}


void
output_coefficients(FILE * file, FORMAT f, char **coeff_strings,
		    char *name, const struct task *op,
		    ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    size_t i;
    assert(f == MY_FORMAT || f == MY_FORMAT_WITH_LOOPS);
    output_constants(file, S);

    const size_t len = get_three_variable_list_len(tri);
    for (i = 0; i < len; i++) {
	if (get_role_of_var(i, tri) != OUTPUT_VAR
	    && (is_Expression_type_elementary_operation(get_type_of_var(i, tri))
		|| is_Expression_type_function(get_type_of_var(i, tri))
		|| is_Expression_type_number(get_type_of_var(i, tri)))) {
	    continue;
	} else if (is_unused(i, tri)
		   || get_role_of_var(i, tri) == OUTPUT_VAR)
	    continue;
	else {
	    assert(0);
	}
    }
    output_function_declaration(file, name, S);
    size_t roles[3] = { 2, INPUT_VAR, INPUTCONST_VAR };
    output_function_head_arguments(file, roles, op, S);
    output_function_head_image_coefficients(file, coeff_strings, op, S);
    fprintf(file, "\n{\n");

    output_three_variable_list(file, f, coeff_strings, op->acc_dim, tri, S);

    fprintf(file, "}\n");
}

void output_concrete_coefficient(FILE * file, FORMAT f, const order_t in, char **coeff_strings,
				 char *name, const struct task *op,
				 ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    size_t i;
    char *v = generate_identifier("v", S);
    char *buf;
    const size_t len = get_three_variable_list_len(tri);
    enum number_format mod;
    output_constants(file, S);
    output_function_declaration(file, name, S);
    size_t roles[3] = { 2, INPUT_VAR, INPUTCONST_VAR };
    output_function_head_arguments(file, roles, op, S);
    output_function_head_image_concrete_coefficient(file, coeff_strings, op, S);
    fprintf(file, "\n{\n");

    switch (f) {
    case ADA_FORMAT:
	mod = NUMERICAL;
	break;
    case MY_FORMAT:
	mod = SYMBOLIC;
	buf = vector_entry_to_string_int(v, get_number_of_variables(tri), CSTRING);
	fprintf(file, "Real %s;\n", buf);
	free(buf);
	break;
    case MY_FORMAT_WITH_LOOPS:
	mod = SYMBOLIC;
	buf = vector_entry_to_string_int(v, get_number_of_variables(tri), CSTRING);
	fprintf(file, "Real %s[ord];\n", buf);
	free(buf);
	break;
    case C_FORMAT:
	mod = NUMERICAL;
	break;
    case C_IA_FORMAT:
	mod = NUMERICAL;
	break;
    case C_FORMAT_WITH_LOOPS:
	mod = NUMERICAL;
	break;
    default:
	assert(0);
    }
    for (i = 0; i < in; i++) {
	output_concrete_coefficient_entry(NULL, f, 0, NULL, NULL, NULL, NULL);	// Load the order
    }

    for (i = 0; i < len; i++) {
	output_three_variable_list_entry_output(file, f, mod, v, i,
						coeff_strings, op->acc_dim,
						(&output_concrete_coefficient_entry), tri, S);
    }
    fprintf(file, "}\n");
    free(v);
    output_concrete_coefficient_entry(NULL, f, 0, NULL, NULL, NULL, NULL);	// Set back to default
}

void
output_taylor(FILE * file, const struct ASTNode *in,
	      const struct task *op, ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    dim_t k = 0, tmp;
    FunctionDefinition fct = in->Function;
    Ode ode = in->ode;
    output_constants(file, S);
    output_function_declaration(file, FCT_NAME(ode->phi), S);
    size_t roles[4] = { 3, TIME_VAR, INPUT_VAR, INPUTCONST_VAR };
    output_function_head_arguments(file, roles, op, S);
    char *comma = "(";
    for (k = 0; k < op->outputstates_len; k++) {
	tmp = op->outputstates[k];
	fprintf(file, "%s%s[%d]", comma, op->out_strings[k], fct->img_dim[tmp + 1]);
	comma = ",";
    }
    fprintf(file, ")\n{\n");
    for (k = 0; k < op->outputstates_len; k++) {
	size_t idx = op->outputstates[k];
	ConstVariable ptr = op->states[idx];
	fprintf(file, "Real %s[%d][%d];\n", op->coeff_strings[idx],
		op->max_order + 1, row_dim_of(ptr));
    }
    output_three_variable_list(file, MY_FORMAT, op->coeff_strings, op->acc_dim, tri, S);
    char *c = generate_identifier("tmp", S);
    fprintf(file, "Real %s;\n", c);
    for (k = 0; k < op->outputstates_len; k++) {
	tmp = op->outputstates[k];
	Horner_to_string(file, op->out_strings[k], NAME(ode->time),
			 op->coeff_strings[tmp], op->max_order, fct->img_dim[tmp + 1] - 1, S);
    }
    fprintf(file, "}\n");
    free(c);
}

void output_function_comment(FILE * file, FORMAT f,
			     const struct function *in,
			     const struct task *op, ConstThreeVariableList tri,
			     ConstSequenceOfStates S)
{
    char *param;
    char *brief;
    switch (f) {
    case C_FORMAT:
    case C_IA_FORMAT:
	param = "* @param";
	brief = "* @brief";
	break;
    case ADA_FORMAT:
	param = "-- @param";
	brief = "\n-- @description";
	break;
    default:
	assert(0);
    }
    if (f == C_FORMAT || f == C_IA_FORMAT) {
	fprintf(file, "/**\n");
    }
    char *mode;
    Expression ptr;
    if (f == C_IA_FORMAT) {
	mode = "with interval arithmetic";
    } else {
	mode = "";
    }
    fprintf(file, "%s The procedure evaluates the explicitly defined function %s\n", brief, mode);
    for (ptr = (Expression) wind_back(AST_EXPR, in->img); ptr != NULL; ptr = ptr->next) {
	Variable x = find_in_variables(ptr, S);
	x = version_of_variable(x, 0, 0, get_date_of_postfix(ptr, S));
	fprintf(file, "%s %s Output variable. Assumptions: %s in R^(%d x %d)\n", param, NAME(ptr),
		NAME(ptr), row_dim_of(x), col_dim_of(x));
    }
    if (f == C_IA_FORMAT) {
	char *v = generate_identifier("v", S);
	fprintf(file, "%s %s Array of auxiliary variables. Assumptions: %s has length %ld\n", param,
		v, v, get_number_of_variables(tri));
    }
    for (ptr = (Expression) wind_back(AST_EXPR, in->var); ptr != NULL; ptr = ptr->next) {
	Variable x = find_in_variables(ptr, S);
	char *buf = data_of_identifier_to_hyperinterval_string(x, MY_FORMAT, MMAPRINTSTRING, S);
	fprintf(file, "%s %s Input variable. Assumptions: %s in %s subset R^(%d x %d)\n", param,
		NAME(ptr), NAME(ptr), buf, row_dim_of(x), col_dim_of(x));
    }
    if (f == C_FORMAT || f == C_IA_FORMAT) {
	fprintf(file, "*/\n");
    }
}

void output_function_image_set(FILE * file, FORMAT f, ConstSequenceOfStates S)
{
    size_t i, j, k;
    Variable x;
    char *buf;
    for (i = 0; i < get_states_len(S); i++) {
	x = get_var(i, S);
	x = version_of_variable(x, 0, 0, get_date_of_rhs(S));
	if (x == NULL)
	    continue;
	if (var_role_of(x) == OUTPUT_VAR) {
	    for (j = 0; j < row_dim_of(x); j++) {
		for (k = 0; k < col_dim_of(x); k++) {
		    assert(intv_of(x, j, k) != NULL);
		    assert(TYPE(intv_of(x, j, k)) == CONT_INTV);
		    buf = Expression_to_string(intv_of(x, j, k), NUMERICAL,
					       format_to_string_format(f), S);
		    fprintf(file, "%s\n", buf);
		    free(buf);
		}
	    }
	}
    }
}
