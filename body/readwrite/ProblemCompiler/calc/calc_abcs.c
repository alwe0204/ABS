#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "calc_abcs.h"
#include "expressions.h"
#include "reduce_int.h"
#include "ia.h"
#include "gappa_abcs.h"

#include <math.h>

/** @brief Allocates memory in op
 * @param op The task data structure 
*/
void malloc_abcs_data(struct task *op)
{
    op->Bounds_of_Lipschitz_Matrices =
	my_malloc(op->state_space_dimension * op->state_space_dimension * sizeof(double));
    op->Bounds_of_Approximation_Error_General_Solution =
	my_malloc(op->state_space_dimension * sizeof(double));
    op->Bounds_of_Approximation_Error_Growth_Bound =
	my_malloc(op->state_space_dimension * sizeof(double));
    op->Bounds_of_Rounding_Error_General_Solution =
	my_malloc(op->state_space_dimension * sizeof(double));
    op->Bounds_of_Rounding_Error_Growth_Bound =
	my_malloc(op->state_space_dimension * sizeof(double));
    op->Bounds_of_Center_of_Cells_Rounding_Error =
	my_malloc(op->state_space_dimension * sizeof(double));
    op->Bounds_of_Summation_Error_Growth_Bound =
	my_malloc(op->state_space_dimension * sizeof(double));
    op->Bounds_of_Summation_Error_General_Solution =
	my_malloc(op->state_space_dimension * sizeof(double));
    op->Growth_Bound_Codomain = my_malloc(op->state_space_dimension * sizeof(double));
    op->General_Solution_Formula_Codomain =
	my_malloc(2 * op->state_space_dimension * sizeof(double));
    op->Bounds_of_Input_Value_Rounding_Error =
	my_malloc(op->input_space_dimension * sizeof(double));
    op->Bounds_of_Overapproximation_Radius = my_malloc(op->state_space_dimension * sizeof(double));
    op->Bounds_of_Overapproximation_Rounding_Error =
	my_malloc(op->state_space_dimension * sizeof(double));
}

/** @brief Computes FP approximation of the sampling time in RN and RU rounding mode
 * @param in The abstract syntax tree
 * @param op The task data structure
 * @param S The sequence of states
*/
void calc_Sampling_Time(struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    op->samplingtime = strtod(CONTENT(in->options->SamplingTime), NULL);
    op->samplingtime_RU = get_upperbound_of_expression(in->options->SamplingTime, S);
}

void calc_Bounds_of_Approximation_Error(double *result, order_t order, struct task *op)
{
    dim_t i;
    mpfi_t ti;
    mpfi_t ti_pow;
    mpfi_t epsi;
    mpfi_init_set_d(ti, op->samplingtime_RU);
    mpfi_init_set(ti_pow, ti);
    mpfi_init(epsi);
    for (i = 1; i < order; i++) {
	mpfi_mul(ti_pow, ti_pow, ti);
    }
    for (i = 0; i < op->state_space_dimension; i++) {
	mpfi_set_d(epsi, result[i]);
	mpfi_mul(epsi, epsi, ti_pow);
	result[i] = mpfi_get_right_d(epsi);
    }
    mpfi_clear(ti);
    mpfi_clear(ti_pow);
    mpfi_clear(epsi);
}


void calc_Bounds_of_Input_Values_Rounding_Error(struct ASTNode *in, struct task *op,
						ConstSequenceOfStates S)
{
    size_t k;

    Expression arg, intv;
    Expression discret = in->options->InputSpaceDiscret->left;
    Expression ptr, qtr;

    if (TYPE(in->Function->dom) == LIST) {
	arg = in->Function->dom->left;
    } else {
	arg = copy_Expression(in->Function->dom);
    }
    const Expression expr = install_expression(LIST, arg, NULL);

    for (k = 0, ptr = (Expression) wind_back(AST_EXPR, expr->left),
	 qtr = (Expression) wind_back(AST_EXPR, discret); ptr != NULL; ptr = ptr->next, k++) {
	assert(ptr->type == CONT_INTV || ptr->type == POSTFIX);
	if (ptr->type == POSTFIX)
	    intv = definition_of(ptr, S);
	else
	    intv = ptr;
	assert(intv->type == CONT_INTV);
	double a_u = get_upperbound_of_expression(intv->left, S);
	double a_l = get_lowerbound_of_expression(intv->left, S);
     /** \f$\verb|a_l| \leq u_\mathrm{min} \leq \verb|a_u|\f$ */
	double b_l = get_lowerbound_of_expression(intv->right, S);
	double b_u = get_upperbound_of_expression(intv->right, S);
     /** \f$\verb|b_l| \leq u_\mathrm{max} \leq \verb|b_u|\f$ */
	int d = get_int_from_expression(qtr, S);
	op->Bounds_of_Input_Value_Rounding_Error[k] =
	    rounding_error_indexedreals_input(a_u, a_l, a_u, b_l, b_l, b_u, d);
    }
    assert(k == op->input_space_dimension);
}

void calc_Bounds_of_Center_of_Cells_Rounding_Error(struct ASTNode *in, struct task *op,
						   ConstSequenceOfStates S)
{
    size_t k, l;

    Expression expr, intv;
    Expression discret = in->options->StateSpaceDiscret->left;
    Expression qtr;
    Expression ptr;
    expr = in->options->Range;

    /* for the tree */
    dim_t i;
    unsigned int two_pow_stmp;
    dim_t s = 0, stmp;
    /** s := \mathrm{max}_{i \in [1;n]} \mathrm{min}\{ k \in \mathbb{Z}_+ \mid 2^k \geq (d_\mathrm{x})_i \} \f$ */
    for (i = 0, qtr = (Expression) wind_back(AST_EXPR, discret);
	 i < op->state_space_dimension; qtr = qtr->next, i++) {
	int d = get_int_from_expression(qtr, S);
	stmp = 0;
	two_pow_stmp = 1;
	while (two_pow_stmp < d) {
	    stmp++;
	    two_pow_stmp = (1lu << stmp);
	}
	s = fmax(s, stmp);
    }
    for (k = 0, ptr = (Expression) wind_back(AST_EXPR, expr->left),
	 qtr = (Expression) wind_back(AST_EXPR, discret); ptr != NULL;
	 ptr = ptr->next, qtr = qtr->next, k++) {
	assert(ptr->type == CONT_INTV || ptr->type == POSTFIX);
	if (ptr->type == POSTFIX)
	    intv = definition_of(ptr, S);
	else
	    intv = ptr;
	assert(intv->type == CONT_INTV);
	double a_u = get_upperbound_of_expression(intv->left, S);
	double a_l = get_lowerbound_of_expression(intv->left, S);
     /** \f$\verb|a_l| \leq x_\mathrm{min} \leq \verb|a_u|\f$ */
	double b_l = get_lowerbound_of_expression(intv->right, S);
	double b_u = get_upperbound_of_expression(intv->right, S);
     /** \f$\verb|b_l| \leq x_\mathrm{max} \leq \verb|b_u|\f$ */
	int d = get_int_from_expression(qtr, S);
	op->Bounds_of_Center_of_Cells_Rounding_Error[k] = 0.;
	op->Bounds_of_Center_of_Cells_Rounding_Error[k] =
	    fmax(op->Bounds_of_Center_of_Cells_Rounding_Error[k],
		 rounding_error_indexedreals(a_l, a_l, a_u, b_u, b_l, b_u, d - 1, d));
	for (l = s; l <= s + 1; l++) {
	    op->Bounds_of_Center_of_Cells_Rounding_Error[k] =
		fmax(op->Bounds_of_Center_of_Cells_Rounding_Error[k],
		     rounding_error_indexedreals_tree(a_l, a_l, a_u, b_u, b_l, b_u, (1u << l) - 1,
						      (1u << l), d));
	}
    }
    assert(k == op->state_space_dimension);
}

void calc_Bounds_of_Summation_Error_Growth_Bound(struct task *op)
{
    dim_t i;
    for (i = 0; i < op->state_space_dimension; i++) {
	op->Bounds_of_Summation_Error_Growth_Bound[i] =
	    Gappa_Bounds_of_Summation_Error_Growth_Bound(op->Growth_Bound_Codomain[i],
							 op->Growth_Bound_Codomain[i],
							 op->
							 Bounds_of_Approximation_Error_Growth_Bound
							 [i],
							 op->
							 Bounds_of_Rounding_Error_Growth_Bound[i]);
    }
}

void calc_Bounds_of_Summation_Error_General_Solution(struct task *op)
{
    dim_t i;
    for (i = 0; i < op->state_space_dimension; i++) {
	op->Bounds_of_Summation_Error_General_Solution[i] =
	    Gappa_Bounds_of_Summation_Error_General_Solution
	    (op->Bounds_of_Approximation_Error_General_Solution[i],
	     op->Bounds_of_Rounding_Error_General_Solution[i],
	     op->Growth_Bound_Codomain[i] + op->Bounds_of_Rounding_Error_Growth_Bound[i] +
	     op->Growth_Bound_Codomain[i] + op->Bounds_of_Rounding_Error_Growth_Bound[i] +
	     op->Bounds_of_Approximation_Error_Growth_Bound[i] +
	     op->Bounds_of_Summation_Error_Growth_Bound[i]);
    }
}

void calc_Bounds_of_Overapproximation(struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    dim_t i, l;
    const dim_t dim = op->state_space_dimension;
    Option opt = in->options;
    Expression ptr, qtr, rtr, Set;


    /* for the tree */
    unsigned int two_pow_stmp;
    dim_t s = 0, stmp;
    /** s := \mathrm{max}_{i \in [1;n]} \mathrm{min}\{ k \in \mathbb{Z}_+ \mid 2^k \geq (d_\mathrm{x})_i \} \f$ */
    for (i = 0, qtr = (Expression) wind_back(AST_EXPR, opt->StateSpaceDiscret->left);
	 i < op->state_space_dimension; qtr = qtr->next, i++) {
	int d = get_int_from_expression(qtr, S);
	stmp = 0;
	two_pow_stmp = 1;
	while (two_pow_stmp < d) {
	    stmp++;
	    two_pow_stmp = (1lu << stmp);
	}
	s = fmax(s, stmp);
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
	double ymax = op->General_Solution_Formula_Codomain[i + dim];
	double ymin = op->General_Solution_Formula_Codomain[i];
	op->Bounds_of_Overapproximation_Rounding_Error[i] =
	    Gappa_Bounds_of_Overapproximation_Rounding_Error(ymin,
							     ymax,
							     s,
							     intv->left,
							     intv->right,
							     get_lowerbound_of_expression
							     (intv->left, S),
							     get_upperbound_of_expression
							     (intv->right, S),
							     op->Bounds_of_Overapproximation_Radius
							     [i],
							     op->Bounds_of_Measurement_Errors[i],
							     S);
    }

    for (l = 0; l < 3; l++) {
	switch (l) {
	case 0:
	    Set = opt->InitialSet;
	    break;
	case 1:
	    Set = opt->TargetSet;
	    break;
	case 2:
	    Set = opt->ObstacleSet;
	    break;
	}
	if (Set == NULL)
	    continue;


	for (ptr = (Expression) wind_back(AST_EXPR, Set); ptr != NULL; ptr = ptr->next) {
	    qtr = ptr->left;
	    assert(qtr != NULL);

	    for (i = 0, qtr = (Expression) wind_back(AST_EXPR, qtr), rtr =
		 (Expression) wind_back(AST_EXPR, opt->Range->left); i < dim;
		 i++, qtr = qtr->next, rtr = rtr->next) {
		Expression intv, intv2;
		assert(qtr->type == CONT_INTV || qtr->type == POSTFIX);
		if (qtr->type == POSTFIX)
		    intv = definition_of(qtr, S);
		else
		    intv = qtr;

		if (rtr->type == POSTFIX)
		    intv2 = definition_of(rtr, S);
		else
		    intv2 = rtr;

		assert(intv->type == CONT_INTV);
		assert(intv2->type == CONT_INTV);

		double tmp =
		    Gappa_Bounds_of_Overapproximation_Rounding_Error(get_lowerbound_of_expression
								     (intv->left, S),
								     get_upperbound_of_expression
								     (intv->right, S),
								     s,
								     intv2->left,
								     intv2->right,
								     get_lowerbound_of_expression
								     (intv2->left, S),
								     get_upperbound_of_expression
								     (intv2->right, S),
								     0,
								     0, S);
		op->Bounds_of_Overapproximation_Rounding_Error[i] =
		    fmax(op->Bounds_of_Overapproximation_Rounding_Error[i], tmp);
	    }			// end for (i)
	}			// end for (ptr)
    }				// end for(k)

}

void calc_Bounds_of_Overapproximation_Radius(struct task *op)
{
    dim_t i;
    for (i = 0; i < op->state_space_dimension; i++) {
	op->Bounds_of_Overapproximation_Radius[i] =
	    op->Growth_Bound_Codomain[i] +
	    op->Bounds_of_Rounding_Error_Growth_Bound[i] +
	    op->Growth_Bound_Codomain[i] +
	    op->Bounds_of_Rounding_Error_Growth_Bound[i] +
	    op->Bounds_of_Approximation_Error_Growth_Bound[i] +
	    op->Bounds_of_Summation_Error_Growth_Bound[i] +
	    op->Bounds_of_Summation_Error_General_Solution[i];
    }
}
