/**
 * @file
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 01 Dez 2016
 * @brief Semantics for a control problem and corresponding abstraction
*/
#include <string.h>
#include <assert.h>
#include <math.h>
#include "semantics_abcs.h"
#include "expressions.h"
#include "reduce_int.h"
#include "output_aux.h"
#include "ia.h"

#include "errors.h"
#include "errors_abcs.h"


/** @brief This method appends to the abstract syntax tree
 * the ordinary differential equation associated with the
 * abcs input file.
 * @param in The abstract syntax tree
 * @param S The sequence of states
*/

void install_ode_for_abcs(struct ASTNode *in, ConstSequenceOfStates S)
{


/* TIME */

    char *buf = generate_identifier(ABCS_INTERVAL_KEYWORD_TIME_VARIABLE, S);
    Identifier time = install_Identifier(buf);
    free(buf);

    Expression nu = install_Number_int(0);
    Expression timeval = in->options->SamplingTime;
    Expression timeint = install_expression(CONT_INTV, nu, timeval);

/* STATE */

    struct variable *x = find_in_variables_match_role_idx_at_date(INPUT_VAR, 1, get_date(S), S);
    buf = generate_identifier(name_of(x), S);
    Identifier initval = install_Identifier(buf);
    Identifier state = install_Identifier(name_of(x));
    Identifier state_tmp = install_Identifier(name_of(x));
    Identifier stateout = install_Identifier(name_of(x));
    stateout = build_PostFixExpression(stateout, install_Number_int(row_dim_of(x)), NULL);
    free(buf);

    Expression OperatingRange = (Expression) wind_back(AST_EXPR, in->options->Range);
    assert(OperatingRange->left != NULL);
    assert(OperatingRange->left->type == CONT_INTV || OperatingRange->left->type == POSTFIX);
    OperatingRange = (Expression) wind_back(AST_EXPR, OperatingRange->left);
    assert(OperatingRange->type == CONT_INTV || OperatingRange->type == POSTFIX);
    Expression tmp;
    tmp = copy_Expression(OperatingRange);
    dim_t i;
    for (i = 1; i < row_dim_of(x); i++) {
	OperatingRange = OperatingRange->next;
	tmp->next = copy_Expression(OperatingRange);
	assert(tmp->next->type == CONT_INTV || tmp->next->type == POSTFIX);
	(tmp->next)->prev = tmp;
	tmp = tmp->next;
    }
    Expression stateint = install_expression(LIST, tmp, NULL);


/* INPUT */

    x = find_in_variables_match_role_idx_at_date(INPUT_VAR, 2, get_date(S), S);
    Identifier input = install_Identifier(name_of(x));
    Identifier input_tmp = install_Identifier(name_of(x));

    tmp = copy_Expression(data_of(x, 0, 0));
    for (i = 1; i < row_dim_of(x); i++) {
	tmp->next = copy_Expression(data_of(x, i, 0));
	(tmp->next)->prev = tmp;
	tmp = tmp->next;
    }
    Expression inputint = install_expression(LIST, tmp, NULL);

/* BUILD ODE */

    inputint = append(AST_EXPR, stateint, inputint);
    Expression dom = append(AST_EXPR, inputint, timeint);
    input = append(AST_EXPR, initval, input);
    Expression vardom = append(AST_EXPR, input, time);
    Expression rhs_val = append(AST_EXPR, state_tmp, input_tmp);

    Identifier rhs = install_Identifier(in->Function->head->content->name);
    buf = generate_identifier("Integration", S);
    Identifier phiname = install_Identifier(buf);
    FunctionDefinition phi = install_function(phiname);
    current_fct = phi;
    free(buf);
    build_function(NESTED, current_fct, vardom, dom, stateout);
    Ode ode = install_ode_statement(state, time, rhs, rhs_val);
    Statement stmt = install_statement(DEFI, (void *) install_assignment(1, state_tmp, initval));
    build_ode_statement(ode, stmt);
    in->ode = ode;

}

/** @brief Denotational semantics for the order specifications
 * in the abcs input file. 
 * @param Order The order for the approximation of the dynamics
 * @param OrderGrowth The order for the approximation of the growth bounds 
 * @param S The sequence of states
 */
void semantics_orders(Expression Order, Expression OrderGrowth , struct task *op, ConstSequenceOfStates S)
{
    Expression ptr = Order;
    if( ptr != NULL ) {
    if (ptr->type != NUM_INT)
	    ERROR_NOT_POS_INTEGER(ptr->line);
	dim_t b = get_int_from_expression(ptr, S);
	if (b < 1)
	    ERROR_NOT_POS_INTEGER(ptr->line);
     op->max_order = b;
    }
    ptr = OrderGrowth;
    if( ptr != NULL ) {
    if (ptr->type != NUM_INT)
	    ERROR_NOT_POS_INTEGER(ptr->line);
	dim_t b = get_int_from_expression(ptr, S);
	if (b < 1)
	    ERROR_NOT_POS_INTEGER(ptr->line);
     op->max_order_growth_bound = b;
    }
}

/** @brief Denotational semantics for the dynamics of the control system
 * represented in the abcs input file 
 * @param in The abstract syntax tree
 * @param op The task data structure 
 * @param S The sequence of states 
*/
void semantics_rhs_for_abcs(struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    struct variable *x = find_in_variables_match_role_idx_at_date(INPUT_VAR, 1, get_date(S), S);
    assert(NULL != x);
    if (strcmp(name_of(x), ABCS_INTERVAL_KEYWORD_STATE_VARIABLE))
	ERROR_RHS_FOR_ABCS;
    op->state_space_dimension = row_dim_of(x);

    x = find_in_variables_match_role_idx_at_date(INPUT_VAR, 2, get_date(S), S);
    if (NULL == x)
	ERROR_RHS_FOR_ABCS;
    if (strcmp(name_of(x), ABCS_INTERVAL_KEYWORD_INPUT_VARIABLE))
	ERROR_RHS_FOR_ABCS;
    op->inputvar_name = name_of(x);
    op->input_space_dimension = row_dim_of(x);

    if (NULL != find_in_variables_match_role_idx_at_date(INPUT_VAR, 3, get_date(S), S))
	ERROR_RHS_FOR_ABCS;
    x = find_in_variables_match_role_idx_at_date(OUTPUT_VAR, 1, get_date(S), S);
    assert(NULL != x);

    x = find_in_variables_match_role_idx_at_date(OUTPUT_VAR, 2, get_date(S), S);
    if (!(NULL == x))
	ERROR_RHS_FOR_ABCS;
    x = find_in_variables_from_string(ABCS_INTERVAL_KEYWORD_TIME_VARIABLE, S);
    if (!(NULL == x))
	ERROR_RESERVED_TIME_VAR;
    x = find_in_variables_from_string("Integration", S);
    if (!(NULL == x))
	ERROR_RESERVED_VAR;

    if (in->options->InputSet == NULL )
    {
      Expression arg;
      if( TYPE(in->Function->dom) == LIST) {
       arg = in->Function->dom->left;
     }
     else {
      arg = copy_Expression(in->Function->dom);
     }
     const Expression expr = install_expression(LIST, arg, NULL);
     in->options->InputSet = expr;
    }
    else { assert(0) ; }

}

/** @brief Denotational semantics for the sampling time 
 * @param in The sampling time
 * @param op The task data structure
 * @param S The sequence of states 
*/
void semantics_sampling_time(Expression in, struct task *op, ConstSequenceOfStates S)
{
    if (in == NULL)
	ERROR_NO_SAMPLING_TIME;
    double samplingtime = get_upperbound_of_expression(in, S);
    if (!isfinite(samplingtime) || !(samplingtime > 0))
	ERROR_BAD_SAMPLING_TIME(in->line);
    op->samplingtime = samplingtime;
}

/** @brief Denotational semantics for the period specification
 * @param in The input expression for the periods
 * @param op The task data structure
 * @param S The sequence of states
*/

void semantics_periods(Expression in, struct task *op, ConstSequenceOfStates S)
{
    unsigned int k = 0;
    Expression ptr;
    dim_t a = 0, b;
    if (in == NULL)
	return;
    for (k = 0, ptr = (Expression) wind_back(AST_EXPR, in->left); ptr != NULL; ptr = ptr->next, k++) {
	if (ptr->type != NUM_INT)
	    ERROR_NOT_POS_INTEGERS(in->line);
	b = get_int_from_expression(ptr, S);
	if (b < 0)
	    ERROR_NOT_POS_INTEGERS(in->line);
	if (b >= op->state_space_dimension)
	    ERROR_IDX("state space vector", in->line);
	if (b != 0 && b <= a)
	    ERROR_NOT_ORDERED(in->line);
	a = b;
    }
    if (op->state_space_dimension < k)
	ERROR_LIST_TOO_LONG(op->state_space_dimension, in->line);
    op->num_of_coord_with_periods = k;
}


/** @brief Denotational semantics for the grid discretization
 * @param in The input expression for the grid discretization
 * @param op The task data structure
 * @param S The sequence of states
*/
void semantics_grid_discretization(Expression in, dim_t required_dim, struct task *op, ConstSequenceOfStates S)
{
    unsigned int k = 0;
    Expression ptr;
    if (in == NULL)
	ERROR_NO_GRIDDISCRETIZATION;
    for (k = 0, ptr = (Expression) wind_back(AST_EXPR, in->left); ptr != NULL; ptr = ptr->next, k++) {
	if (ptr->type != NUM_INT)
	    ERROR_NOT_POS_INTEGERS(in->line);
	dim_t b = get_int_from_expression(ptr, S);
	if (b < 1)
	    ERROR_NOT_POS_INTEGERS(in->line);
    }
    if (required_dim != k)
	ERROR_LIST_BAD_LENGTH(required_dim, in->line);
}

/** @brief Denotational semantics for the uncertainties and the measurement errors
 * @param in The input expression for the uncertainties and the measurement errors
 * @param op The task data structure
 * @param S The sequence of states
*/

void semantics_uncertainties_measurementerrors(Expression in,
					       struct task *op, ConstSequenceOfStates S)
{
    unsigned int k = 0;
    Expression ptr;
    if (in != NULL) {
	for (k = 0, ptr = (Expression) wind_back(AST_EXPR, in->left);
	     ptr != NULL; ptr = ptr->next, k++) {
	    if (!is_Expression_constant(ptr, S))
		ERROR_UNCERT_MEAS(ptr->line);
	    if (get_upperbound_of_expression(ptr, S) < 0)
		ERROR_UNCERT_MEAS(ptr->line);
	}
	if (op->state_space_dimension != k)
	    ERROR_LIST_BAD_LENGTH(op->state_space_dimension, in->line);
    }
}


void semantics_input_signals(Expression in, struct task *op, ConstSequenceOfStates S)
{
    unsigned int k = 0;
    dim_t i;
    Expression ptr, qtr;
    if (in == NULL)
	ERROR_NO_INPUT_SIGNAL;
    for (k = 0, ptr = (Expression) wind_back(AST_EXPR, in); ptr != NULL; ptr = ptr->next, k++) {
	qtr = ptr->left;
	for (i = 0, qtr = (Expression) wind_back(AST_EXPR, qtr); qtr != NULL; i++, qtr = qtr->next) {
	    Variable x = find_in_variables_match_role_idx_at_date(INPUT_VAR,
								  2,
								  get_date(S), S);
	    if (!(is_Expression_subset_Expression(qtr, data_of(x, i, 0), S)))
		ERROR_INPUT_NOT_IN_INPUTSET(k + 1, ptr->line);
	}
	if (i != op->input_space_dimension)
	    ERROR_LIST_BAD_LENGTH(op->input_space_dimension, in->line);
    }
    op->num_of_input_signals = k;
}

void semantics_initial_set(Expression in,
			   Expression operating_range, struct task *op, ConstSequenceOfStates S)
{
    unsigned int k = 1;
    dim_t i;
    Expression ptr, qtr, rtr;
    if (in == NULL)
	ERROR_NO_INITIALSET;
    assert(operating_range != NULL);
    for (k = 1, ptr = (Expression) wind_back(AST_EXPR, in); ptr != NULL; ptr = ptr->next, k++) {
	qtr = ptr->left;
	assert(NULL != qtr);
	rtr = operating_range->left;
	assert(NULL != rtr);
	for (i = 0, qtr =
	     (Expression) wind_back(AST_EXPR, qtr), rtr =
	     (Expression) wind_back(AST_EXPR, rtr);
	     qtr != NULL; i++, qtr = qtr->next, rtr = rtr->next) {
	    if (!(is_Expression_subset_Expression(qtr, rtr, S)))
		ERROR_INITIAL_SET_NOT_IN_OPERATING_RANGE(in->line);
	}
	if (i != op->state_space_dimension)
	    ERROR_LIST_BAD_LENGTH(op->state_space_dimension, in->line);
	assert(k != 0);
    }
    op->num_of_initialsets = k - 1;
}

void semantics_sets(Expression in,
		    enum option_type type_of_set, struct task *op, ConstSequenceOfStates S)
{
    unsigned int k = 0;
    dim_t i;
    Expression ptr, qtr;
    switch (type_of_set) {
    case OPTION_OBSTACLESET:
	op->num_of_obstaclesets = 0;
	if (in == NULL)
	    return;
	break;
    case OPTION_TARGETSET:
	if (in == NULL)
	    ERROR_NO_TARGETSET;
	break;
    case OPTION_RANGE:
	if (in == NULL)
	    ERROR_NO_OPERATINGRANGE;
	break;
    case OPTION_INPUTSET:
     if (in == NULL)
      assert(0);
    break;
    default:
	assert(0);
    }
    for (k = 1, ptr = (Expression) wind_back(AST_EXPR, in); ptr != NULL; ptr = ptr->next, k++) {
	qtr = ptr->left;
	assert(NULL != qtr);
	for (i = 0, qtr = (Expression) wind_back(AST_EXPR, qtr); qtr != NULL; i++, qtr = qtr->next) {
	    Expression intv;
	    if (qtr->type == POSTFIX)
		intv = definition_of(qtr, S);
	    else
		intv = qtr;
         if( intv->type != CONT_INTV) ERROR_INTERVAL(in->line);

	    assume_interval_bounded(intv, "Hyperinterval", in->line, S);
	    double a = get_lowerbound_of_expression(intv->left,
						    S);
	    double b = get_upperbound_of_expression(intv->right, S);
         double c = get_upperbound_of_expression(intv->left,  S);
	    double d = get_lowerbound_of_expression(intv->right, S);
	 if( type_of_set == OPTION_RANGE ) {
        if (!(a < b))
		ERROR_INTERVAL(in->line);
      } else {
	    if (!(a <= b))
		ERROR_INTERVAL(in->line);
      }
      if( type_of_set == OPTION_INPUTSET )
      {
        if( !(c<=d))
         ERROR_ROUNDED_INPUTSET_EMPTY(in->line);
      }
	}
     if( type_of_set != OPTION_INPUTSET) {
	if (i != op->state_space_dimension)
	    ERROR_LIST_BAD_LENGTH(op->state_space_dimension, in->line);
     }
     else {
       if (i != op->input_space_dimension)
	    ERROR_LIST_BAD_LENGTH(op->input_space_dimension, in->line);
     }
    }
    switch (type_of_set) {
    case OPTION_OBSTACLESET:
	op->num_of_obstaclesets = k - 1;
	break;
    case OPTION_TARGETSET:
	op->num_of_targetsets = k - 1;
	break;
    case OPTION_RANGE:
    case OPTION_INPUTSET:
	assert(k == 2);
	break;
    default:
	assert(0);
    }
}


void semantics_Option(struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    Option opt = in->options;
    if (opt == NULL)
	return;
    semantics_orders(opt->IntegrationOrder,opt->IntegrationOrderGrowth, op, S);
    semantics_rhs_for_abcs(in, op, S);
    semantics_periods(opt->Periods, op, S);
    semantics_uncertainties_measurementerrors(opt->Uncertainties, op, S);
    semantics_uncertainties_measurementerrors(opt->MeasErrors, op, S);
    semantics_sampling_time(opt->SamplingTime, op, S);

    semantics_grid_discretization(opt->StateSpaceDiscret, op->state_space_dimension, op, S);
    semantics_grid_discretization(opt->InputSpaceDiscret, op->input_space_dimension, op, S);
  //  semantics_input_signals(opt->InputSignal, op, S);
    semantics_sets(opt->Range, OPTION_RANGE, op, S);
    semantics_sets(opt->InputSet, OPTION_INPUTSET, op, S);
    semantics_initial_set(opt->InitialSet, opt->Range, op, S);
    semantics_sets(opt->TargetSet, OPTION_TARGETSET, op, S);
    semantics_sets(opt->ObstacleSet, OPTION_OBSTACLESET, op, S);
    install_ode_for_abcs(in, S);
}
