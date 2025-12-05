/** @file 
 * @brief Interval arithmetic
 * @date 01 Jun 2016
 * @author Alexander Weber (a.weber@unibw.de)
 * Changes on 19 Nov 2017: Lookup table for interval evaluation added
*/

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <mpfi_io.h>

#include "ia.h"

#include "errors.h"

#include "expressions.h"
#include "reduce_int.h"

#include "to_string.h"
#include "lists.h"

#define ALLOC_IA 100

struct expression_value_list {
    size_t len;
    Expression *list;
    mpfi_t **value;
};
typedef struct expression_value_list *ExpressionValueList;
struct expression_value_list L;

void alloc_ExpressionValueList(void *in, const size_t beg, const size_t end)
{
    return;
}

void create_ExpressionValueList(ExpressionValueList L)
{
    L->len = 0;
    alloc_list((void **) (&(L->list)), L->len, sizeof(Expression), ALLOC_IA,
	       &alloc_ExpressionValueList);
    alloc_list((void **) (&(L->value)), L->len, sizeof(mpfi_t *), ALLOC_IA,
	       &alloc_ExpressionValueList);
    NO_MEM(L->list);
    NO_MEM(L->value);
}

void insert_to_list(Expression in, mpfi_t * intv)
{
    assert(in != NULL);
    assert(intv != NULL);
    assert(L.list != NULL);
    alloc_list((void **) (&(L.list)), L.len, sizeof(Expression), ALLOC_IA,
	       &alloc_ExpressionValueList);
    alloc_list((void **) (&(L.value)), L.len, sizeof(mpfi_t *), ALLOC_IA,
	       &alloc_ExpressionValueList);
    NO_MEM(L.list);
    NO_MEM(L.value);
    L.list[L.len] = in;
    L.value[L.len] = intv;
    (L.len)++;
}

mpfi_t *find_in_list(Expression in)
{
    size_t i;
    static int first_call = 0;
    if (first_call == 0) {
	first_call = 1;
	create_ExpressionValueList(&L);
    }
    size_t len = L.len;
    Expression *list = L.list;
    mpfi_t **value = L.value;
    for (i = 0; i < len; i++) {
	if (in == list[i]) {
	    return value[i];
	}
    }
    return NULL;
}

int is_standard_evaluation(mpfi_t ** arg, dim_t * abs_dim)
{
    return (arg == NULL && abs_dim == NULL);
}

/** @brief If \p op represents \f$[a,b]\f$, \f$a,b \in \mathbb{R}\f$, \f$a\leq b\f$ then
 * the function returns \f$b'\f$ where \f$b'\f$ is the smallest double number 
 * satisfying \f$b'\geq b\f$. 
*/
double mpfi_get_right_d(mpfi_t op)
{
    double out;
    mpfr_t a;
    mpfr_init(a);
    mpfi_get_right(a, op);
    out = mpfr_get_d(a, MPFR_RNDU);
    mpfr_clear(a);
    return out;
}

/** @brief If \p op represents \f$[a,b]\f$, \f$a,b \in \mathbb{R}\f$, \f$a\leq b\f$ then
 * the function returns \f$a'\f$ where \f$a'\f$ is the largest double number 
 * satisfying \f$a'\leq a\f$. 
*/

double mpfi_get_left_d(mpfi_t op)
{
    double out;
    mpfr_t a;
    mpfr_init(a);
    mpfi_get_left(a, op);
    out = mpfr_get_d(a, MPFR_RNDD);
    mpfr_clear(a);
    return out;
}

/** @brief Implementation of \f$c = a^b \f$ in interval arithmetic
 * @param rop \f$c\f$
 * @param op1 \f$a\f$
 * @param op2 \f$b\f$
 */
int mpfi_pow(mpfi_t rop, mpfi_t op1, mpfi_t op2)
{
    mpfi_t tmp;
    mpfi_init(tmp);
    mpfi_log(tmp, op1);
    mpfi_mul(tmp, tmp, op2);
    int retval = mpfi_exp(rop, tmp);
    mpfi_clear(tmp);
    return retval;
}

mpfi_t *evaluate_expression_not_postfix(Expression in, mpfi_t ** arg,
					dim_t * abs_dim, ConstSequenceOfStates S)
{
    mpfi_t *rop = malloc(sizeof(mpfi_t));
    mpfi_init(*rop);
    if (in == NULL)
	return rop;

    assert(TYPE(in) != POSTFIX);

    mpfi_t *op = NULL, *op1 = NULL, *op2 = NULL;

    if (is_Expression_function(in)) {
	op = evaluate_expression(in->right, arg, abs_dim, S);
    } else if (is_Expression_elementary_operation(in)
	       || is_Expression_Interval(in)) {
	op1 = evaluate_expression(in->left, arg, abs_dim, S);
	op2 = evaluate_expression(in->right, arg, abs_dim, S);
    } else {
    }

    switch (in->type) {
    case PLUS:
	mpfi_add(*rop, *op1, *op2);
	break;
    case MINUS:
	mpfi_sub(*rop, *op1, *op2);
	break;
    case TIMES:
	mpfi_mul(*rop, *op1, *op2);
	break;
    case DIV:
	mpfi_div(*rop, *op1, *op2);
	break;
    case POW:
	{
	    dim_t i;
	    mpfi_set_si(*rop, 1);

	    int n = GET_INT(in->right);
	    if (n == 2)
		mpfi_sqr(*rop, *op1);
	    else if (n == -2) {
		mpfi_sqr(*rop, *op1);
		mpfi_inv(*rop, *rop);
	    } else {
		if (n >= 0)
		    for (i = 0; i < n; i++) {
			mpfi_mul(*rop, *rop, *op1);
		} else {
		    for (i = 0; i < -n; i++) {
			mpfi_mul(*rop, *rop, *op1);
		    }
		    mpfi_inv(*rop, *rop);
		}
	    }
	}
	break;
    case EXP:
	mpfi_pow(*rop, *op1, *op2);
	break;
    case FCT_ATAN:
	mpfi_atan(*rop, *op);
	break;
    case FCT_COS:
	mpfi_cos(*rop, *op);
	break;
    case FCT_COSH:
	mpfi_cosh(*rop, *op);
	break;
    case FCT_EXP:
	mpfi_exp(*rop, *op);
	break;
    case FCT_LOG:
	mpfi_log(*rop, *op);
	break;
    case FCT_SIN:
	mpfi_sin(*rop, *op);
	break;
    case FCT_SINH:
	mpfi_sinh(*rop, *op);
	break;
    case FCT_SQRT:
	mpfi_sqrt(*rop, *op);
	break;
    case FCT_TAN:
	mpfi_tan(*rop, *op);
	break;
    case NUM_INT:
	mpfi_set_si(*rop, (long int) GET_INT(in));
	break;
    case NUM_DEC:
	assert(!mpfi_set_str(*rop, CONTENT(in), 10));
	break;
    case NUM_CONST:
	{
	    char *math_const[] = { "Pi", "Exp" };
	    int i;
	    for (i = 0; i < 2; i++) {
		if (!strcmp(math_const[i], in->content))
		    break;
	    }
	    switch (i) {
	    case 0:
		mpfi_const_pi(*rop);
		break;
	    case 1:
		assert(0);
	    default:
		assert(0);
	    }
	}
	break;
    case CONT_INTV:
	{
	    mpfi_set(*rop, *op1);
	    mpfi_put(*rop, *op2);
	}
	break;
    case REAL_SPACE:
	{
	    mpfi_set_d(*rop, -HUGE_VAL);
	    mpfi_put_d(*rop, HUGE_VAL);
	}
	break;
    default:
	assert(0);
    }
    if (is_standard_evaluation(arg, abs_dim) == 0) {
	if (is_Expression_function(in)) {
	    mpfi_clear(*op);
	    free(op);
	} else if (is_Expression_elementary_operation(in)
		   || is_Expression_Interval(in)) {
	    mpfi_clear(*op1);
	    mpfi_clear(*op2);
	    free(op1);
	    free(op2);
	} else if (is_Expression_Number(in) || is_Expression_RealSpace(in)) {
	} else
	    assert(0);
    }
    return rop;
}


/** @brief Evaluation of the expression \p in with interval arithmetic
 * @param in The expression to evaluate 
 * @param arg The range of the input variables
 * @param abs_dim The absolute dimensions of the input variables 
 * @param S The sequence of states
*/
mpfi_t *evaluate_expression(Expression in, mpfi_t ** arg, dim_t * abs_dim, ConstSequenceOfStates S)
{
    mpfi_t *rop;
    if (is_standard_evaluation(arg, abs_dim)) {
	rop = find_in_list(in);
	if (rop != NULL)
	    return rop;
    }
    if (in == NULL || TYPE(in) != POSTFIX) {
	rop = evaluate_expression_not_postfix(in, arg, abs_dim, S);
    } else {
	if ((arg != NULL) && (role_of(in, S) == INPUT_VAR)
	    && (get_date_of_postfix(in, S) <= get_date_of_rhs(S))) {
	    rop = (mpfi_t *) my_malloc(sizeof(mpfi_t));
	    mpfi_init(*rop);
	    dim_t row, col;
	    get_indices_of_PostFixExpression(in, &row, &col, 0, S);
	    assert(col == 0);
	    mpfi_set(*rop, *(arg[abs_dim[idx_of(in, S) - 1] + row]));
	} else {
	    if (interval_of(in, S) != NULL)
		rop = evaluate_expression(interval_of(in, S), arg, abs_dim, S);
	    else
		rop = evaluate_expression(definition_of(in, S), arg, abs_dim, S);
	}
    }
    if (is_standard_evaluation(arg, abs_dim)) {
	insert_to_list(in, rop);
    }
    return rop;
}

void evaluate_rhs(mpfi_t ** rop, mpfi_t ** arg, dim_t * abs_dim, ConstSequenceOfStates S)
{
    assert(arg != NULL);
    assert(abs_dim != NULL);
    size_t i, j, k;
    struct variable *x;
    k = 0;
    for (i = 0; i < get_states_len(S); i++) {
	x = get_var(i, S);
	x = version_of_variable(x, 0, 0, get_date_of_rhs(S));
	if (x == NULL)
	    continue;
	if (var_role_of(x) == OUTPUT_VAR) {
	    assert(col_dim_of(x) == 1 && row_dim_of(x) > 0);
	    for (j = 0; j < row_dim_of(x); j++) {
		Expression tmp = data_of(x, j, 0);
		rop[k++] = evaluate_expression(tmp, arg, abs_dim, S);
	    }
	}
    }
}

mpfi_t **Identifier_to_mpfi(Identifier in, ConstSequenceOfStates S)
{
    dim_t i, j, k = 0;
    dim_t a;
    struct variable *x;
    dim_t *dim = dimensions_of_expression_list(in, S);
    accumulate(dim + 1, dim[0]);
    mpfi_t **out = my_malloc(dim[dim[0]] * sizeof(mpfi_t *));
    Identifier ptr = (Expression) wind_back(AST_EXPR, in);
    for (i = 0; i < dim[0]; i++) {
	a = get_date_of_postfix(ptr, S);
	x = version_of_variable(find_in_variables_check_declared(ptr, S), 0, 0, a);
	for (j = 0; j < row_dim_of(x); j++) {
	    x = version_of_variable(find_in_variables_check_declared(ptr, S), j, 0, a);
	    out[k] = evaluate_expression(data_of(x, j, 0), NULL, &i, S); /* &i only for not being NULL */
	    k++;
	}
	ptr = ptr->next;
    }
    return out;
}


int
compute_aprioribound(Identifier in, dim_t * abs_dim,
		     dim_t dimension_of_ode, struct task *op, ConstSequenceOfStates S)
{

    int retval;
    char found = 0;
    dim_t num_of_attempts = 0;
    const dim_t n = dimension_of_ode;

    mpfi_t **x = Identifier_to_mpfi(in, S);
    mpfi_t **x0 = Identifier_to_mpfi(in, S);
    mpfi_t *xa = my_malloc(n * sizeof(mpfi_t));
    mpfi_t **fx = my_malloc(n * sizeof(mpfi_t *));
    dim_t i, j;
    for (i = 0; i < n; i++) {
	mpfi_init(xa[i]);
    }

    mpfi_t tmp;
    mpfi_init(tmp);

    struct variable *t = find_in_variables_match_role_at_date(TIME_VAR, get_date(S), S);
    mpfi_t *time = evaluate_expression(data_of(t, 0, 0), NULL, &i, S); /* &i only for not being NULL */
    mpfi_put_si(*time, 0);

    while (!found && num_of_attempts < NUM_OF_ATTEMPTS_APRIORI) {
	num_of_attempts++;
	if (IS_VERBOSE(op)) {
	    fprintf(stdout, "\rVerifying existence of solution (attempt: %u)", num_of_attempts);
	    fflush(stdout);
	}
	evaluate_rhs(fx, x, abs_dim, S);
	for (i = 0; i < n; i++) {
	    if (mpfi_nan_p(*(fx[i]))) {
		retval = 4;
		goto exit;
	    }
	    if (!mpfi_bounded_p(*(fx[i]))) {
		retval = 3;
		goto exit;
	    }
	}
	for (i = 0; i < n; i++) {
	    mpfi_set(xa[i], *(x[i]));
	    mpfi_mul(tmp, *(fx[i]), *time);
	    mpfi_add(tmp, tmp, *(x0[i]));
	    mpfi_set(*(x[i]), tmp);
	    if (!mpfi_bounded_p(*(x[i]))) {
		retval = 2;
		goto exit;
	    }
	    if (mpfi_is_inside(*(x[i]), xa[i]) == 0) {
		break;
	    }
	}
	if (i == n) {
	    found = 1;
	}
	for (j = 0; j < i; j++) {
	    mpfi_clear(*(fx[j]));
	    free(*fx[j]);
	}
    }
    if (IS_VERBOSE(op)) {
	fprintf(stdout, "\n");
    }
    if (num_of_attempts == NUM_OF_ATTEMPTS_APRIORI) {
	retval = 1;
	goto exit;
    }
    if (found && IS_VERBOSE(op)) {
	fprintf(stdout, "Enclosure:\n");
	for (i = 0; i < n; i++) {
	    mpfi_out_str(stdout, 10, 5, *(x[i]));
	    fprintf(stdout, "\n");
	}
    }
    if (found) {
	op->apriori_enclosure = malloc(2 * n * sizeof(double));
	for (i = 0; i < n; i++) {
	    op->apriori_enclosure[i] = mpfi_get_left_d(*(x[i]));
	    op->apriori_enclosure[i + n] = mpfi_get_right_d(*(x[i]));
	}
	retval = 0;
	goto exit;
    } else {
	retval = 1;
	goto exit;
    }
  exit:
    mpfi_clear(tmp);
    mpfi_clear(*time);
    for (i = 0; i < n; i++) {
	mpfi_clear((*x[i]));
	mpfi_clear(xa[i]);
	mpfi_clear((*x0[i]));
	free(*x0[i]);
	free(*x[i]);
    }
    free(x0);
    free(x);
    free(xa);
    free(fx);
    free(time);
    return retval;
}


Expression mpfi_to_interval_expression(mpfi_t * in)
{
    char buf_a[50];
    char buf_b[50];
    double a = mpfi_get_left_d(*in);
    double b = mpfi_get_right_d(*in);
    if (!isfinite(a) || !isfinite(b))
	return NULL;
    else {
	sprintf(buf_a, "%a", a);
	sprintf(buf_b, "%a", b);
	Expression expr_a = install_Number(NULL, NUM_DEC, buf_a);
	Expression expr_b = install_Number(NULL, NUM_DEC, buf_b);
	Expression expr = install_expression(CONT_INTV, expr_a, expr_b);
	return expr;
    }
}

void evaluate_function(ConstSequenceOfStates S)
{
    size_t i, j, k;
    struct variable *x;
    mpfi_t *rop;
    for (i = 0; i < get_states_len(S); i++) {
	x = get_var(i, S);
	x = version_of_variable(x, 0, 0, get_date_of_rhs(S));
	if (x == NULL)
	    continue;
	if (var_role_of(x) == OUTPUT_VAR) {
	    for (k = 0; k < col_dim_of(x); k++) {
		for (j = 0; j < row_dim_of(x); j++) {
		    Expression tmp = data_of(x, j, k);
		    rop = evaluate_expression(tmp, NULL, NULL, S);
		    write_data_2(mpfi_to_interval_expression(rop), x, j, k);
		}
	    }
	}
    }
}

void check_existence_of_solution(struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    FunctionDefinition fct = in->Function;
    Ode ode = in->ode;
    if (ode == NULL)
	return;
    switch (compute_aprioribound(ode->rhs_var, fct->var_abs_dim, ode->dim, op, S)) {
    case 1:
	ERROR_NO_APRIORI_ATTEMPTS(in->ode->line);
    case 2:
	ERROR_NO_APRIORI(in->ode->line);
    case 3:
	ERROR_NO_APRIORI_RHS_UNBOUNDED(in->ode->line);
    case 4:
	ERROR_NO_APRIORI_RHS_NAN(in->ode->line);
    default:
	break;
    }
}


double get_mid_of_expression(Expression in, ConstSequenceOfStates S)
{
    mpfi_t *res = evaluate_expression(in, NULL, NULL, S);
    double retval = mpfi_get_d(*res);
    mpfi_clear(*res);
    free(res);
    if (!isfinite(retval))
	ERROR_DEC_NOT_FINITE(in->line);
    return retval;
}

/** @brief Wrapper for mpfi_get_right_d()
 * @param in The expression to evaluate
 * @param S The sequence of states
*/

double get_upperbound_of_expression(Expression in, ConstSequenceOfStates S)
{
    mpfi_t *res;
    res = evaluate_expression(in, NULL, NULL, S);
    double retval = mpfi_get_right_d(*res);
    if (!isfinite(retval))
	ERROR_DEC_NOT_FINITE(in->line) return retval;
}

/** @brief Wrapper for mpfi_get_left_d()
 * @param in The expression to evaluate
 * @param S The sequence of states
*/
double get_lowerbound_of_expression(Expression in, ConstSequenceOfStates S)
{
    mpfi_t *res;
    res = evaluate_expression(in, NULL, NULL, S);
    double retval = mpfi_get_left_d(*res);
    if (!isfinite(retval))
	ERROR_DEC_NOT_FINITE(in->line) return retval;
}

int is_Expression_subset_Expression(Expression op1, Expression op2, ConstSequenceOfStates S)
{
    mpfi_t *res1 = evaluate_expression(op1, NULL, NULL, S);
    mpfi_t *res2 = evaluate_expression(op2, NULL, NULL, S);
    return mpfi_is_inside(*res1, *res2);
}
