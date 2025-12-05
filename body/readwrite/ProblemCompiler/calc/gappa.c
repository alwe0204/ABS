#define _GNU_SOURCE
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <float.h>

#include "gappa.h"
#include "my_strcat.h"
#include "expressions.h"
#include "output.h"
#include "output_aux.h"
#include "ia.h"
#include "lists.h"
#include "gappa_utils.h"
#include "gappa_abcs.h"
#include "errors.h"

#define GAPPA_EVAL_TRACE_FILE "Function.g"
#define GAPPA_QUESTION_FILE "Question.g"
#define GAPPA_INPUT_FILE "res.g"
#define GAPPA_RESULTS_FILE "res.dat"

#define DIFF 0
#define FLOAT 1
#define MATH 2

extern struct task Task;

char *gappa_var_to_string(const char *name, size_t idx, FORMAT f)
{
    assert(idx != -1);
    char *le = "_", *ri = "_g";
    char *buf = "";
    char idx_str[21];
    sprintf(idx_str, "%lu", idx);
    switch (f) {
    case GAPPA_FORMAT:
	my_strcat_several(&buf, 4, name, le, idx_str, ri);
	break;
    case GAPPA_MATH_FORMAT:
	my_strcat_several(&buf, 5, "M", name, le, idx_str, ri);
	break;
    default:
	assert(0);
    }
    return buf;
}

char *gappa_interval(double lower, double upper)
{
    char *buf = "";
    char *buf1 = "";
    char *buf2 = "";
    if (lower == 0.) {
	my_strcat(&buf1, "0");
    } else {
	buf1 = double_to_string(lower, CSTRING);
    }
    if (upper == 0.) {
	my_strcat(&buf2, "0");
    } else {
	buf2 = double_to_string(upper, CSTRING);
    }
    my_strcat_several(&buf, 5, "[", buf1, ",", buf2, "]");
    free(buf1);
    free(buf2);
    return buf;
}

char *gappa_bounded_difference(const char *a, const char *b, double lower, double upper)
{
    char *buf = "";
    char *buf2 = gappa_interval(lower, upper);
    my_strcat_several(&buf, 6, "|", a, " - ", b, "| in ", buf2);
    return buf;
}

size_t
output_evaluation_trace_gappa(FILE * file, enum output_format f,
			      enum number_format mod, var3_t j,
			      const struct function * in,
			      ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    size_t i;
    char *buf;

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
	if (is_unused(i, tri)) {
	    continue;
	}
	enum expression_type type = get_type_of_var(i, tri);
	if ((type == FCT_SIN || type == FCT_COS || type == FCT_EXP || type == FCT_LOG
	     || type == FCT_TAN || type == FCT_ATAN || type == FCT_COSH || type == FCT_SINH)
	    && i > j) {
	    free(v);
	    return i;
	}
	output_three_variable_list_entry(file, f, mod, v, i, tri, S);
	if (get_role_of_var(i, tri) == OUTPUT_VAR) {
	    buf = var_string(v, i, f);
	    output_output_var(file, f, i, in->img_acc_dim, out_strings, buf, tri);
	    free(buf);
	}
    }
    free(v);
    free(out_strings);
    return len;
}

void output_special_gappa_input_assumptions(FILE * file, struct task *op)
{
    if (strcmp(op->gappa_input_assumptions_file, "")) {
	FILE *tmp = fopen(op->gappa_input_assumptions_file, "r");
	assert(tmp != NULL);
	copy_file_content(file, tmp);
	fclose(tmp);
	fprintf(file, "/\\");
    }
}

void output_gappa_assumption(FILE * file, char **comma, char *name, dim_t row, dim_t col,
			     double lower, double upper)
{
    char *buf = matrix_entry_to_string_int(name, row, col, GAPPAMATHSTRING);
    char *buf2 = gappa_interval(lower, upper);
    fprintf(file, "%s %s in %s", *comma, buf, buf2);
    free(buf);
    free(buf2);
    *comma = " /\\";
}

void output_constants_to_gappa(FILE * file, ConstSequenceOfStates S)
{
    char *comma = "";
    size_t i;
    dim_t j, k;
    Variable x;
    for (i = get_states_len(S); i--;) {
	x = get_var(i, S);
	x = version_of_variable(x, 0, 0, get_date(S));
	if (x == NULL)
	    continue;
	if (var_role_of(x) == ORDINARY_VAR && var_type_of(x) == TY_REAL) {
	    for (k = 0; k < col_dim_of(x); k++) {
		for (j = 0; j < row_dim_of(x); j++) {
		    if (intv_of(x, j, k) != NULL) {
			Expression intv = intv_of(x, j, k);
			assert(TYPE(intv) == CONT_INTV);
			const double a = get_lowerbound_of_expression(intv->left, S);
			const double b = get_upperbound_of_expression(intv->right, S);
			output_gappa_assumption(file, &comma, name_of(x), j, k, a, b);
		    } else {
			Expression intv = data_of(x, j, k);
			const double a = get_lowerbound_of_expression(intv, S);
			const double b = get_upperbound_of_expression(intv, S);
			output_gappa_assumption(file, &comma, name_of(x), j, k, a, b);
		    }
		}
	    }
	}
	if (var_role_of(x) == INPUT_VAR) {
	    for (j = 0; j < row_dim_of(x); j++) {
		Expression intv = data_of(x, j, 0);
		assert(TYPE(intv) == CONT_INTV);
		const double a = get_lowerbound_of_expression(intv->left, S);
		const double b = get_upperbound_of_expression(intv->right, S);
		output_gappa_assumption(file, &comma, name_of(x), j, 0, a, b);
	    }
	}
    }
    char *buf = gappa_interval(nextafter(M_PI, -10.), nextafter(M_PI, 10.));
    fprintf(file, "%s Pi in %s", comma, buf);
    free(buf);
    comma = " /\\";
}

void output_output_in_gappa(FILE * file, ConstSequenceOfStates S)
{
    char *comma = "";
    size_t i;
    dim_t j;
    Variable x;
    for (i = get_states_len(S); i--;) {
	x = get_var(i, S);
	x = version_of_variable(x, 0, 0, get_date(S));
	if (x == NULL)
	    continue;
	if (var_role_of(x) == OUTPUT_VAR) {
	    for (j = 0; j < row_dim_of(x); j++) {
		char *bufM = gappa_var_to_string(name_of(x), (size_t) j, GAPPA_MATH_FORMAT);
		char *buf = gappa_var_to_string(name_of(x), (size_t) j, GAPPA_FORMAT);
		fprintf(file, "%s | %s - %s | in ?", comma, bufM, buf);
		free(bufM);
		free(buf);
		comma = " /\\";
	    }
	}
    }
}

void alloc_buf(void *in, const size_t beg, const size_t end)
{
    register size_t i;
    char **ptr = (void *) in;
    for (i = beg; i < end; i++) {
	ptr[i] = NULL;
    }
    return;
}

void add_to_buf(char ***buffer, size_t * len, char *to_add)
{
    alloc_list((void **) (buffer), *len, sizeof(char *), 10, alloc_buf);
    (*buffer)[*len] = to_add;
    (*len)++;
}

void cat_files_and_run_gappa(struct task *op, ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    assert(!system("cat " GAPPA_EVAL_TRACE_FILE " " GAPPA_QUESTION_FILE " > " GAPPA_INPUT_FILE));
    if (strcmp(op->gappa_input_assumptions_file, "")) {
	prepare_gappa_file_for_abcs(GAPPA_INPUT_FILE, op, tri, S);
    }
    if (run_gappa(GAPPA_INPUT_FILE, GAPPA_RESULTS_FILE, GAPPA_EXECUTABLE)) {
	assert(!system("cat " GAPPA_INPUT_FILE));
	ERROR_GAPPA;
    }
}

static void add_fct_deriv_to_buffer(char ***buffer, size_t * len, enum expression_type type,
				    const double *c_l, const double *c_u, const char *math_str,
				    const char *float_str)
{
    char *buf = "";
    mpfi_t intv;
    mpfi_init(intv);
    mpfi_interv_d(intv, c_l[FLOAT], c_u[FLOAT]);
    mpfi_put_d(intv, c_l[MATH]);
    mpfi_put_d(intv, c_u[MATH]);
    switch (type) {
    case FCT_ATAN:
	{
	    mpfi_t one;
	    mpfi_init_set_d(one, 1);
	    mpfi_sqr(intv, intv);
	    mpfi_add(intv, intv, one);
	    if (mpfi_has_zero(intv))
		ERROR_GAPPA;
	    mpfi_inv(intv, intv);
	    mpfi_clear(one);
	}
	break;
    case FCT_COSH:
	mpfi_sinh(intv, intv);
	break;
    case FCT_COS:
	assert(0);
	break;
    case FCT_EXP:
	mpfi_exp(intv, intv);
	break;
    case FCT_LOG:
	if (mpfi_has_zero(intv))
	    ERROR_GAPPA;
	mpfi_inv(intv, intv);
	break;
    case FCT_SIN:
	assert(0);
	break;
    case FCT_SINH:
	mpfi_cosh(intv, intv);
	break;
    case FCT_TAN:
	mpfi_sec(intv, intv);
	mpfi_sqr(intv, intv);
	break;
    default:
	assert(0);
    }
    double d = mpfi_get_right_d(intv);
    mpfi_interv_d(intv, c_l[DIFF], c_u[DIFF]);
    mpfi_mul_d(intv, intv, d);
    buf = gappa_bounded_difference(math_str, float_str, 0, mpfi_get_right_d(intv));
    add_to_buf(buffer, len, buf);
    mpfi_clear(intv);
}

static void add_fct_img_to_buffer(char ***buffer, size_t * len, enum expression_type type,
				  const double *c_l, const double *c_u, const char *math_str)
{
    char *buf = "";
    char *buf2;
    mpfi_t intv;
    mpfi_init(intv);
    mpfi_interv_d(intv, c_l[MATH], c_u[MATH]);
    switch (type) {
    case FCT_ATAN:
	mpfi_atan(intv, intv);
	break;
    case FCT_COSH:
	mpfi_cosh(intv, intv);
	break;
    case FCT_COS:
	mpfi_cos(intv, intv);
	break;
    case FCT_EXP:
	mpfi_exp(intv, intv);
	break;
    case FCT_LOG:
	mpfi_log(intv, intv);
	break;
    case FCT_SIN:
	mpfi_sin(intv, intv);
	break;
    case FCT_SINH:
	mpfi_sinh(intv, intv);
	break;
    case FCT_TAN:
	mpfi_tan(intv, intv);
	break;
    default:
	assert(0);
    }
    buf2 = gappa_interval(mpfi_get_left_d(intv), mpfi_get_right_d(intv));
    my_strcat_several(&buf, 3, math_str, " in ", buf2);
    mpfi_clear(intv);
    free(buf2);
    add_to_buf(buffer, len, buf);
}



void
output_function_in_gappa(struct function *in, struct task *op, ConstThreeVariableList tri,
			 ConstSequenceOfStates S)
{
    char **buffer = NULL;
    char *buf;
    size_t buffer_len = 0;
    alloc_list((void **) (&buffer), buffer_len, sizeof(char *), 10, alloc_buf);
    char *v = generate_identifier("v", S);
    const size_t len = get_three_variable_list_len(tri);
    size_t cur = 0;
    double c_l[3], c_u[3];
    FILE *quest = NULL;
    FILE *file = NULL;
    size_t i, j;
    for (i = 0; i < len; i++) {
     file = gappa_fopen(GAPPA_EVAL_TRACE_FILE);
	output_evaluation_trace_gappa(file, GAPPA_MATH_FORMAT, SYMBOLIC, cur, in, tri, S);
	cur = output_evaluation_trace_gappa(file, GAPPA_FORMAT, SYMBOLIC, cur, in, tri, S);
	if (cur != len) {
	    i = 0;
	    fclose(file);
	    int k;
	    for (k = 0; k < 3; k++) {
		quest = fopen(GAPPA_QUESTION_FILE, "w+");
		fprintf(quest, "\n{\n");
		output_special_gappa_input_assumptions(quest, op);
		output_constants_to_gappa(quest, S);
		for (j = 0; j < buffer_len; j++) {
		    fprintf(quest, "/\\ %s ", buffer[j]);
		}
		fprintf(quest, " -> ");
		switch (k) {
		case 0:
		    {
			char *bufM = gappa_var_to_string(v, (size_t) get_rght_var(cur, tri), GAPPA_MATH_FORMAT);
			char *buf = gappa_var_to_string(v, (size_t) get_rght_var(cur, tri), GAPPA_FORMAT);
			fprintf(quest, "| %s - %s | in ?", bufM, buf);
			free(bufM);
			free(buf);
		    }
		    break;
		case 1:
		    {
			char *buf = gappa_var_to_string(v, (size_t) get_rght_var(cur, tri), GAPPA_FORMAT);
			fprintf(quest, "%s in ?", buf);
			free(buf);
		    }
		    break;
		case 2:
		    {
			char *bufM = gappa_var_to_string(v, (size_t) get_rght_var(cur, tri), GAPPA_MATH_FORMAT);
			fprintf(quest, "%s in ?", bufM);
			free(bufM);
		    }
		    break;
		default:
		    assert(0);
		    break;
		}
		fprintf(quest, "\n}\n");
		fclose(quest);
		cat_files_and_run_gappa(op, tri, S);
		c_l[k] = gappa_parser_lower(GAPPA_RESULTS_FILE);
		c_u[k] = gappa_parser_upper(GAPPA_RESULTS_FILE);
	    }
	    enum expression_type type = get_type_of_var(cur, tri);
	    char *math_str = definition_of_variable_to_string(cur, v, cur, SYMBOLIC,
							      GAPPA_MATH_FORMAT, tri,
							      S);
	    char *float_str = definition_of_variable_to_string(cur, v, cur, SYMBOLIC,
							       GAPPA_FORMAT, tri, S);
	    switch (type) {
	    case FCT_ATAN:
		{
		    add_fct_deriv_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str,
					    float_str);
		    add_fct_img_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str);
		}
		break;
	    case FCT_COSH:
		{
		    add_fct_deriv_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str,
					    float_str);
		    add_fct_img_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str);
		}
		break;
	    case FCT_SINH:
		{
		    add_fct_deriv_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str,
					    float_str);
		    add_fct_img_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str);
		}
		break;
	    case FCT_COS:
	    case FCT_SIN:
		{
		    /* |sin/cos(a) - sin/cos(b)| <= |a - b| */
		    buf = gappa_bounded_difference(math_str, float_str, 0, c_u[DIFF]);
		    add_to_buf(&buffer, &buffer_len, buf);
		    buf = "";
              
              add_fct_img_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str);
		}
		break;
	    case FCT_TAN:
		{

		    add_fct_deriv_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str,
					    float_str);
		    /* Output mathematical range */
		    add_fct_img_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str);
		}
		break;
	    case FCT_EXP:
		{
		    /* Output mathematical range */
		    add_fct_img_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str);
		    /* Output (bound on derivative) times difference */
		    add_fct_deriv_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str,
					    float_str);
		}
		break;
	    case FCT_LOG:
		{
		    /* Output mathematical range */
		    add_fct_img_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str);
		    /* Output (bound on derivative) times difference */
		    add_fct_deriv_to_buffer(&buffer, &buffer_len, type, c_l, c_u, math_str,
					    float_str);
		}
		break;
	    default:
		assert(0);
	    }
	    free(math_str);
	    free(float_str);
	} else { fclose(file); }
    }
    file = fopen(GAPPA_EVAL_TRACE_FILE, "a+");
    if (file == NULL)
	ERROR_OPEN_FILE(GAPPA_EVAL_TRACE_FILE);
    fprintf(file, "\n{\n");
    output_special_gappa_input_assumptions(file, op);
    output_constants_to_gappa(file, S);
    for (j = 0; j < buffer_len; j++) {
	fprintf(file, "/\\ %s ", buffer[j]);
    }
    fprintf(file, " -> ");
    fclose(file);
    op->Bounds_of_out_rounding_errors = (double **) my_malloc(in->img_dim[0] * sizeof(double *));
    for (i = 1; i <= in->img_dim[0]; i++) {
	op->Bounds_of_out_rounding_errors[i - 1] =
	    (double *) my_malloc(in->img_dim[i] * sizeof(double));
    }
    Variable x;
    dim_t jj = 0;
    for (i = 0; i < get_states_len(S); i++) {
	x = get_var(i, S);
	x = version_of_variable(x, 0, 0, get_date(S));
	if (x == NULL)
	    continue;
	if (var_role_of(x) == OUTPUT_VAR) {
	    for (j = 0; j < row_dim_of(x); j++) {
		quest = fopen(GAPPA_QUESTION_FILE, "w+");
		char *bufM = gappa_var_to_string(name_of(x), (size_t) j, GAPPA_MATH_FORMAT);
		char *buf = gappa_var_to_string(name_of(x), (size_t) j, GAPPA_FORMAT);
		fprintf(quest, "| %s - %s | in ?", bufM, buf);
		free(bufM);
		free(buf);
		fprintf(quest, "\n}\n");
		fclose(quest);
		cat_files_and_run_gappa(op, tri, S);
		op->Bounds_of_out_rounding_errors[jj][j] = gappa_parser_upper(GAPPA_RESULTS_FILE);
	    }
	    jj++;
	}
    }
    unlink(GAPPA_QUESTION_FILE);
    unlink(GAPPA_RESULTS_FILE);
    unlink(GAPPA_EVAL_TRACE_FILE);
    unlink(GAPPA_INPUT_FILE);
    free(v);
}

void output_Bounds_of_out_rounding_errors(FILE * file, struct task *op, ConstThreeVariableList tri,
					  ConstSequenceOfStates S)
{
    Variable x;
    dim_t i, j, jj = 0;
    for (i = 0; i < get_states_len(S); i++) {
	x = get_var(i, S);
	x = version_of_variable(x, 0, 0, get_date(S));
	if (x == NULL)
	    continue;
	if (var_role_of(x) == OUTPUT_VAR) {
	    for (j = 0; j < row_dim_of(x); j++) {
		fprintf(file, "%a\n", op->Bounds_of_out_rounding_errors[jj][j]);
	    }
	    jj++;
	}
    }
}
