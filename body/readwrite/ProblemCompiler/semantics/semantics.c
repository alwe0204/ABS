/**
 * @file semantics.c
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 3 Feb 2016
 * @brief Implementation of the interpretation functions 
*/

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <stdbool.h>

#include "semantics.h"

#include "expressions.h"
#include "errors.h"
#include "reduce_int.h"



/**
 * @brief Implementation of 
 * \f$ \mathcal{I} \colon [ \verb|declaration| ] \to \{ S \to S\}\f$, i.e.
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma)\f$ is generated.
 * @param in \f$a\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
 *
*/
void semantics_declaration(Declaration in, SequenceOfStates S)
{
    assert(in != NULL);
    assert(in->type != TY_FCT);
    assert(CONTENT(in) != NULL);
    assert(!(ROW_IDX(CONTENT(in)) == NULL && COL_IDX(CONTENT(in)) != NULL));

    dim_t row, col;
    get_indices_of_PostFixExpression(CONTENT(in), &row, &col, 1, S);
    Variable dest = insert_id_to_variables(CONTENT(in), row, col, in->type, S);
    initialize_variable(dest, NULL, 0);
}



/** @brief Implementation of \f$\mathcal{I} \colon [ \verb|definition| ] \to \{ S \to S \}\f$,
 * i.e. 
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma)\f$ is generated.
 * @param in \f$a\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/
void semantics_definition(Assignment in, SequenceOfStates S)
{
    /* Work on left hand side */
    dim_t row, col;
    Variable x = IsEntry(in->left, S);
    if (var_role_of(x) == INPUT_VAR)
	ERROR_DEFINE_INPUT(name_of(x), in->line);
    get_indices_of_PostFixExpression(in->left, &row, &col, 0, S);
    /* Work on right hand side */
    switch (in->type) {
    case 1:
	{
	    if (TYPE(in->right) == POSTFIX) {
		ConstVariable y =
		    version_of_variable(find_in_variables_check_declared
					(in->right, S), 0, 0, get_date(S));
		if (var_type_of(y) != var_type_of(x)) {
		    if (var_type_of(x) == TY_REAL) {
			ERROR_DECL_NOT_INTERVAL(name_of(x), in->line);
		    } else {
			ERROR_DECL_NOT_REAL(name_of(x), in->line);
		    }
		}
	    }
	    if (var_type_of(x) == TY_REAL)
		check_valid_real_assignment(in, S);
	    else if (var_type_of(x) == TY_INTV && TYPE(in->right) != POSTFIX)
		ERROR_DEFINE_INTERVAL(name_of(x), in->line);
	}
	break;
    case 2:
	{
	    if (var_type_of(x) != TY_INTV)
		ERROR_DECL_NOT_INTERVAL(name_of(x), in->line);
	    assert(is_Expression_Interval(in->right) == true);
	    assume_interval_bounded(in->right, name_of(x), in->line, S);
	}
	break;
    case 3:
	{
	    if (var_type_of(x) != TY_REAL)
		ERROR_DECL_NOT_REAL(name_of(x), in->line);
	    if (data_of(x, row, col) != NULL)
		ERROR_DEFINED(name_of(x), in->line);
	    assume_interval_bounded(in->right, name_of(x), in->line, S);
	}
	break;
    default:
	assert(0);
    }
    remove_PostFixExpression_involved_in_Expression(&(in->right), in->left, S);
    switch (in->type) {
    case 1:
    case 2:
	write_data_1(in->right, x, row, col, S);
	break;
    case 3:
	{
	    Expression intv;
	    if (TYPE(in->right) == CONT_INTV)
		intv = in->right;
	    else {
		intv = definition_of(in->right, S);
	    }
	    assert(intv != NULL && TYPE(intv) == CONT_INTV);
	    write_data_2(intv, x, row, col);
	    Expression tmp = install_expression(PLUS, (void *) (intv->left),
						(void *) (intv->right));
	    tmp->line = in->line;
	    Expression midpoint = install_expression(DIV, tmp,
						     install_Number_int(2));
	    midpoint->line = in->line;
	    write_data_1(midpoint, x, row, col, S);
	}
	break;
    default:
	assert(0);
    }
}


/**
 * @brief Implementation of 
 * \f$ \mathcal{I} \colon [ \verb|declaration_and_definition| ] \to \{ S \to S\}\f$, i.e.
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma)\f$ is generated.
 * @param in \f$a\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
 *
*/
void semantics_declaration_and_definition(Declaration in, SequenceOfStates S)
{
    assert(in->right != NULL);
    semantics_declaration(in, S);
    struct assignment tmp;
    tmp.left = in->content;
    tmp.right = in->right;
    tmp.line = in->line;
    if (in->type == TY_REAL && is_Expression_Interval(in->right))
	tmp.type = 3;
    else if (in->type == TY_INTV)
	tmp.type = 2;
    else
	tmp.type = 1;
    semantics_definition(&tmp, S);
}

void semantics_iteration_statement(Iteration in, SequenceOfStates S);
void semantics_statement_list(Statement in, SequenceOfStates S);

/** @brief Implementation of \f$\mathcal{I} \colon [\verb|statement|] \to \{ S \to S \}\f$, 
 * i.e.
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma)\f$ is generated.
 * @param in \f$a\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/
void semantics_statement(Statement in, SequenceOfStates S)
{
    switch (in->type) {
    case DECL:
	semantics_declaration((Declaration) in->ptr, S);
	break;
    case DEFI:
	semantics_definition((Assignment) in->ptr, S);
	break;
    case DECLDEFI:
	semantics_declaration_and_definition((Declaration) in->ptr, S);
	break;
    case ITER:
	semantics_iteration_statement((Iteration) in->ptr, S);
	break;
    default:
	assert(0);
    }
}



/** @brief Implementation of \f$\mathcal{I} \colon [\verb|iteration_head|] \to \{ S \to S \}\f$
 * and of \f$\mathrm{iter}\f$,
 * i.e. 
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma)\f$ is generated and
 * \f$\operatorname{iter}\llbracket a \rrbracket(\sigma)\f$ is a return value. 
 * @param in \f$a\f$
 * @param start \f$\operatorname{iter}\llbracket a \rrbracket(\sigma)_2\f$
 * @param end \f$\operatorname{iter}\llbracket a \rrbracket(\sigma)_3\f$
 * @param type \f$\operatorname{iter}\llbracket a \rrbracket(\sigma)_4\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
 * @returns \f$\sigma'(\operatorname{iter}\llbracket a \rrbracket(\sigma)_1)\f$
*/

struct variable *semantics_iteration_head(Iteration in,
					  int *start, int *end,
					  enum iteration_type *type, SequenceOfStates S)
{
    *start = expression_to_int(in->start, S);
    *end = expression_to_int(in->end, S);
    *type = TYPE(in);
    switch (TYPE(in)) {
    case IT_FOR_INC:
	if (*start > *end)
	    *type = IT_FOR_NULL;
	break;
    case IT_FOR_DEC:
	if (*start < *end)
	    *type = IT_FOR_NULL;
	break;
    default:
	assert(0);
    }
    return (insert_id_to_variables_manually(in->var, 1, 1, TY_REAL, ITER_VAR, 1, S));
}

/** @brief Help function for the implementation of \f$\mathcal{I} \colon [\verb|iteration_statement|] \to \{ S \to S \}\f$. 
 * @param in \f$a \in [\verb|iteration_statement|]\f$
 * @param i Current iteration index. 
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/
void semantics_iteration_statement_core(Iteration in, int i, SequenceOfStates S)
{
    assert(in != NULL);

    struct assignment iter;
    iter.type = 1;
    iter.left = in->var;
    iter.right = install_Number_int(i);
    iter.line = 0;
    increase_date(S);
    semantics_definition(&iter, S);
    Statement ptr = in->body;
    for (ptr = (Statement) wind_back(AST_STMT, ptr); ptr != NULL; ptr = ptr->next) {
	switch (TYPE(ptr)) {
	case DEFI:
	    {
		struct assignment a;
		a.type = ((Assignment) ptr->ptr)->type;
		a.left = copy_Expression((((Assignment) ptr->ptr)->left));
		a.right = copy_Expression((((Assignment) ptr->ptr)->right));
		a.line = 0;
		increase_date(S);
		semantics_definition(&a, S);
	    }
	    break;
	case ITER:
	    semantics_iteration_statement((Iteration) ptr->ptr, S);
	    break;
	default:
	    assert(0);
	}
    }
}

/** @brief Implementation of \f$\mathcal{I} \colon [\verb|iteration_statement|] \to \{ S \to S \}\f$, 
 * i.e.
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma)\f$ is generated.
 * @param in \f$a\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/
void semantics_iteration_statement(Iteration in, SequenceOfStates S)
{
    int start = 0, end = 0;
    int i;
    enum iteration_type type;
    Variable x = semantics_iteration_head(in, &start, &end, &type, S);
    switch (type) {
    case IT_FOR_NULL:
	break;
    case IT_FOR_INC:
	{
	    for (i = start; i <= end; i++) {
		semantics_iteration_statement_core(in, i, S);
	    }
	}
	break;
    case IT_FOR_DEC:
	{
	    for (i = start; i >= end; i--) {
		semantics_iteration_statement_core(in, i, S);
	    }
	}
	break;
    default:
	assert(0);
    }
    x = version_of_variable(find_in_variables(in->var, S), 0, 0, get_date(S));
    make_variable_undeclared(x, S);
}


/**
 * @brief Implementation of \f$\mathcal{I} \colon [\verb|statement_list|] \to \{S \to S\}\f$,
 * i.e.
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma)\f$ is generated.
 * @param in \f$a\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/
void semantics_statement_list(Statement in, SequenceOfStates S)
{
    Statement ptr = in;
    if (ptr == NULL)
	return;
    for (ptr = (Statement) wind_back(AST_STMT, ptr); ptr != NULL; ptr = ptr->next) {
	increase_date(S);
	semantics_statement(ptr, S);
    }
}

/**
 * @brief Help function for parts (i),(ii) in the definition of \f$\mathcal{I} \colon [\verb|function_head|] \to \{ S \to S\}\f$
 * @param in \f$a \in [\verb|function_head|]\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/

void semantics_function_head_i_ii(FunctionDefinition in, SequenceOfStates S)
{
    assert(in != NULL);
    assert(TYPE(in->var) == POSTFIX);


    Expression v = in->var;
    Expression d = in->dom;
    dim_t r = 0;
    dim_t k;
    dimension(&r, NULL, d, S);
    const dim_t idx = 1;
    Variable x = insert_id_to_variables_manually(v, r, 1, TY_REAL,
						 INPUT_VAR, idx, S);
    switch (TYPE(d)) {
    case CONT_INTV:
    case DISC_INTV:
	if (!is_Expression_bounded_Interval(d, S))
	    ERROR_NOT_INTERVAL(in->line)
		write_data_in_function_head(d, x, 0, 0);
	break;
    case REAL_SPACE:
	for (k = 0; k < r; k++) {
	    write_data_in_function_head(d, x, k, 0);
	}
	break;
    case POSTFIX:
	{
	    struct variable *z;
	    struct variable *y = find_in_variables(d, S);
	    for (k = 0; k < r; k++) {
		z = version_of_variable(y, k, 0, get_date(S));
		if (is_undeclared(z))
		    ERROR_UNDECL(NAME(d), in->line)
			write_data_in_function_head(data_of(z, k, 0), x, k, 0);
	    }
	}
	break;
    default:
	assert(0);
    }
}



/**
 * @brief Help function for parts (iii),(iv) in the definition of 
 * \f$\mathcal{I} \colon [\verb|function_head|] \to \{ S \to S\}\f$
 * @param v 
 * @param d
 * @param idx 
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/
void semantics_function_head_iii_iv(Expression v, Expression d, dim_t idx, SequenceOfStates S)
{
    assert(TYPE(v) == POSTFIX);
    assert(TYPE(d) == LIST);

    Expression ptr = d->left;
    dim_t r = 0;
    dim_t k;
    while (ptr != NULL) {
	if (TYPE(ptr) == LIST)
	    ERROR_NOT_ONE_DIMENSION(ptr->line) dimension(NULL, NULL, ptr, S);
	ptr = ptr->prev;
	r++;
    }
    Variable x = insert_id_to_variables_manually(v, r, 1, TY_REAL,
						 INPUT_VAR, idx, S);
    for (k = 0, ptr = d->left; k < r; k++, ptr = ptr->prev) {
	switch (TYPE(ptr)) {
	case POSTFIX:
	    write_data_in_function_head(definition_of(ptr, S), x, r - (k + 1), 0);
	    break;
	case CONT_INTV:
	case DISC_INTV:
	    if (!is_Expression_bounded_Interval(ptr, S))
		ERROR_NOT_INTERVAL(ptr->line)
		    write_data_in_function_head(ptr, x, r - (k + 1), 0);
	    break;
	case REAL_SPACE:
	    write_data_in_function_head(ptr, x, r - (k + 1), 0);
	    break;
	default:
	    assert(0);
	}
    }
}




/**
 *@brief Help function for parts (v),(vi) in the definition of \f$\mathcal{I} \colon [\verb|function_head|] \to \{ S \to S\}\f$
 *@param in \f$a \in [\verb|function_head|]\f$
 *@param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/
void semantics_function_head_v_vi(FunctionDefinition in, SequenceOfStates S)
{
    dim_t r, k;
    Variable x;
    Expression v = in->var;
    Expression d;
    dim_t n = get_length_of_expression_list(v);
    dim_t idx = n;
    for (d = in->dom; d != NULL; d = d->prev, v = v->prev) {
	switch (TYPE(d)) {
	case LIST:
	    semantics_function_head_iii_iv(v, d, idx--, S);
	    break;
	case POSTFIX:
	    {
		r = 0;
		dimension(&r, NULL, d, S);
		x = insert_id_to_variables_manually(v, r, 1, TY_REAL, INPUT_VAR, idx--, S);
		struct variable *z;
		struct variable *y = find_in_variables(d, S);
		for (k = 0; k < r; k++) {
		    z = version_of_variable(y, k, 0, get_date(S));
		    if (is_undeclared(z))
			ERROR_UNDECL(NAME(d), in->line)
			    write_data_in_function_head(data_of(z, k, 0), x, k, 0);
		}
	    }
	    break;
	case CONT_INTV:
	case DISC_INTV:
	    if (!is_Expression_bounded_Interval(d, S))
		ERROR_NOT_INTERVAL(in->line)
		    x = insert_id_to_variables_manually(v, 1, 1, TY_REAL, INPUT_VAR, idx--, S);
	    write_data_in_function_head(d, x, 0, 0);
	    break;
	case REAL_SPACE:
	    r = 0;
	    dimension(&r, NULL, d, S);
	    x = insert_id_to_variables_manually(v, r, 1, TY_REAL, INPUT_VAR, idx--, S);
	    for (k = 0; k < r; k++) {
		write_data_in_function_head(d, x, k, 0);
	    }
	    break;
	default:
	    assert(0);
	}
    }
}


/**
 * @brief Implementation of \f$\mathcal{I} \colon [\verb|function_head|] \to \{ S \to S\}\f$,
 * i.e.
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma)\f$ is generated.
 * @param in \f$a\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/

void semantics_function_head(FunctionDefinition in, SequenceOfStates S)
{
    dim_t r, c;
    Expression v = in->var;
    Expression d = in->dom;
    Expression im = in->img;
    assert(im != NULL);
    Expression ptr;
    dim_t i = 0;
    for (ptr = (Expression) wind_back(AST_EXPR, im); ptr != NULL; ptr = ptr->next) {
	i++;
	get_indices_of_PostFixExpression(ptr, &r, &c, 1, S);
	insert_id_to_variables_manually(ptr, r, c, TY_REAL, OUTPUT_VAR, i, S);
    }
    switch (in->type) {
    case IDENT:
	semantics_function_head_i_ii(in, S);
	break;
    case MIXED:
	semantics_function_head_iii_iv(v, d, 1, S);
	break;
    case NESTED:
	if (get_length_of_expression_list(d) != get_length_of_expression_list(v))
	    ERROR_LENGTH_MISMATCH(FCT_NAME(in), in->line)
		semantics_function_head_v_vi(in, S);
	break;
    default:
	assert(0);
    }
    (in->var_dim) = dimensions_of_expression_list(in->var, S);
    (in->img_dim) = dimensions_of_expression_list(in->img, S);
    (in->var_abs_dim) = make_abs_dim(in->var_dim);
    (in->img_acc_dim) = make_abs_dim(in->img_dim);
    increase_date(S);
}


/**
 * @brief Implementation of \f$\mathcal{I} \colon [\verb|function_declaration|] \to \{ S \to S \}\f$,
 * i.e.
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma)\f$ is generated.
 * @param in \f$a\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/
void semantics_function_declaration(Declaration in, SequenceOfStates S)
{
    increase_date(S);
    Variable x;
    size_t i, j, k;

    for (i = 0; i < get_states_len(S); i++) {
	x = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	if (var_role_of(x) == OUTPUT_VAR || var_role_of(x) == INPUT_VAR) {
	    make_variable_undeclared(get_var(i, S), S);
	} else if (var_role_of(x) == ORDINARY_VAR && var_type_of(x) == TY_REAL) {
	    for (j = 0; j < row_dim_of(x); j++) {
		for (k = 0; k < col_dim_of(x); k++) {
		    if (!is_Expression_constant(data_of(x, j, k), S)) {
			break;
		    }
		}
		if (k != col_dim_of(x))
		    break;
	    }
	    if (j != row_dim_of(x))
		make_variable_undeclared(get_var(i, S), S);
	}
    }
    if (in != NULL) {
	x = insert_id_to_variables_manually(CONTENT(in), 1, 1, TY_FCT, ORDINARY_VAR, 0, S);
	kill_var(x);
    }
}


/** @brief Implementation of \f$\mathcal{I} \colon [\verb|ode_equation|] \to \{ S \times S \to S \}\f$,
 * i.e. 
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma_0,\sigma)\f$ is generated.
 * @param in \f$a\f$
 * @param rhs \f$\sigma_0\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/

void semantics_ode_equation(Ode in, struct function *rhs, SequenceOfStates S)
{
    dim_t i, j;
    Identifier ptr, qtr;
    Variable x = check_declared(in->rhs, S);
    if (var_type_of(x) != TY_FCT)
	ERROR_ODE_EQUATION_DECL_NOT_FUNC(NAME(in->rhs), in->line)
	dim_t n = get_length_of_expression_list(in->var);
    dim_t m = get_length_of_expression_list(in->rhs_var);
    if (n > m)
	ERROR_ODE_EQUATION_INPUT_OUTPUT_DIMENSION(NAME(in->rhs), in->line)
	    if (rhs->img_dim[0] != n)
	    ERROR_ODE_IDX_MISMATCH(FCT_NAME(in->phi), rhs->img_dim[0], n, in->line)
		if (rhs->var_dim[0] != m)
		ERROR_ODE_ARG(NAME(in->rhs), in->line)
		    in->dim = rhs->var_abs_dim[n];

    for (ptr = (Expression) wind_back(AST_EXPR, in->var),
	 qtr =
	 (Expression) wind_back(AST_EXPR, in->rhs_var), i = 0;
	 i < n; ptr = ptr->next, qtr = qtr->next, i++) {
	if (rhs->img_dim[i + 1] != rhs->var_dim[i + 1])
	    ERROR_ODE_EQUATION_RHS(NAME(in->rhs), in->line)
		if (rhs->img_dim[n + i + 1] != 1)
		ERROR_ODE_EQUATION_RHS(NAME(in->rhs), in->line)
		    if (strcmp(NAME(qtr), NAME(ptr)))
		    ERROR_ODE_EQUATION_RHS(NAME(in->rhs), in->line)
			x = current_state_of(ptr, S);
	if (!is_undeclared(x) && var_role_of(x) != OUTPUT_VAR)
	    ERROR_DECL(name_of(x), in->line)
		if (!is_undeclared(x)
		    && var_role_of(x) == OUTPUT_VAR && (row_dim_of(x) != rhs->var_dim[i + 1]
							|| col_dim_of(x) != 1))
		ERROR_ODE_EQUATION_RHS(NAME(in->rhs), in->line);
    }

    for (ptr = (Expression) wind_back(AST_EXPR, in->rhs_var), i = 0; i < m; ptr = ptr->next, i++) {
	qtr = in->rhs_var;
	j = m - 1;
	while (strcmp(NAME(qtr), NAME(ptr))) {
	    j--;
	    qtr = qtr->prev;
	}
	if (i != j)
	    ERROR_ODE_EQUATION_RHS(NAME(in->rhs), in->line);
	if (!strcmp(NAME(in->time), NAME(ptr)))
	    ERROR_ODE_EQUATION_RHS(NAME(in->rhs), in->line);
	if (i > n) {
	    x = current_state_of(ptr, S);
	    if (var_role_of(x) != INPUT_VAR)
		ERROR_ODE_EQUATION_RHS(NAME(in->rhs), in->line)
		    if (row_dim_of(x) != rhs->var_dim[i + 1])
		    ERROR_ODE_EQUATION_RHS(NAME(in->rhs), in->line);
	}
    }

    x = current_state_of(in->time, S);

    if (var_role_of(x) != INPUT_VAR)
	ERROR_ODE_TIME_VARIABLE(name_of(x), in->line);
    if (col_dim_of(x) * row_dim_of(x) != 1)
	ERROR_ODE_EQUATION_RHS(NAME(in->rhs), in->line);
    if (TYPE(data_of(x, 0, 0)) != CONT_INTV)
	ERROR_ODE_TIME_VARIABLE(name_of(x), in->line)
	    set_var_role_of(x, TIME_VAR);

    for (ptr = (Expression)
	 wind_back(AST_EXPR, in->rhs_var), i = 0; i < m; ptr = ptr->next, i++) {
	x = current_state_of(ptr, S);
	if (i < n) {
	    if (is_undeclared(x))
		insert_id_to_variables_manually
		    (ptr, rhs->var_dim[i + 1], 1, TY_REAL, STATE_VAR, i + 1, S);
	    else {
		assert(var_role_of(x) == OUTPUT_VAR);
		x = insert_new_version_of_variable_with_role(x, OUTPUTSTATE_VAR, S);
		set_var_idx(x, i + 1);
	    }
	} else {
	    if (is_undeclared(x))
		ERROR_UNDECL(NAME(ptr), in->line)
		    if (var_role_of(x) != INPUT_VAR)
		    ERROR_NOT_OF_ROLE_INPUT(name_of(x), in->line)
			x = insert_new_version_of_variable_with_role(x, INPUTCONST_VAR, S);
	}
    }

}

/** @brief Implementation of \f$\mathcal{I} \colon [\verb|initialvalue|] \to \{ S \to S \}\f$
 * in the sense that 
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma)\f$ is generated.
 * @param in \f$a\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/

void semantics_initialvalue(Assignment in, SequenceOfStates S)
{
    Variable x = find_in_variables(in->left, S);
    x = version_of_variable(x, 0, 0, get_date(S));

    if (is_undeclared(x)) {
	ERROR_INIT_VAL(name_of(x), in->line);
    } else if (var_role_of(x) != STATE_VAR && var_role_of(x) != OUTPUTSTATE_VAR)
	ERROR_INIT_VAL(name_of(x), in->line);
    if (is_PostFixExpression_Identifier(in->left)) {
	if (TYPE(in->right) == POSTFIX && is_PostFixExpression_Identifier(in->right)
	    && type_of(in->right, S) != TY_REAL)
	    ERROR_INIT_VAL(name_of(x), in->line);
	if (TYPE(in->right) == POSTFIX && !is_PostFixExpression_Identifier(in->right)) {
	    IsEntry(in->right, S);
	    if (row_dim_of(x) != 1)
		ERROR_INIT_VAL(name_of(x), in->line);
	    check_valid_real_assignment(in, S);
	    semantics_definition(in, S);
	} else if (TYPE(in->right) != POSTFIX) {
	    check_valid_real_assignment(in, S);
	    semantics_definition(in, S);
	} else {
	    Variable y = current_state_of(in->right, S);
	    if (row_dim_of(y) != row_dim_of(x))
		ERROR_INIT_VAL(name_of(x), in->line) register dim_t i;
	    Expression tmpr, tmpl;
	    for (i = 0; i < row_dim_of(y); i++) {
		tmpl = copy_Expression(in->left);
		tmpr = copy_Expression(in->right);
		semantics_definition
		    (install_assignment
		     (1,
		      build_PostFixExpression
		      (tmpl,
		       install_Number_int
		       (i), NULL), build_PostFixExpression(tmpr, install_Number_int(i), NULL)), S);
	    }
	}
    } else if (is_PostFixExpression_Vector(in->left)) {
	if (TYPE(in->right) == POSTFIX) {
	    IsEntry(in->right, S);
	}
	IsEntry(in->left, S);
	check_valid_real_assignment(in, S);
	semantics_definition(in, S);
    } else
	ERROR_NOT_VECTOR(NAME(in->left), in->line);
}

/** @brief Implementation of \f$\mathcal{I} \colon [\verb|initialvalue_list|] \to \{ S \to S \}\f$
 * i.e.
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma)\f$ is generated.
 * @param in \f$a\f$
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/

void semantics_initialvalue_list(Statement in, SequenceOfStates S)
{
    assert(in != NULL);
    Statement ptr;
    for (ptr = (Statement)
	 wind_back(AST_STMT, in); ptr != NULL; ptr = ptr->next) {
	increase_date(S);
	semantics_initialvalue((Assignment) ptr->ptr, S);
    }
}


/**
 *@brief Implementation of \f$\mathcal{I}_1\llbracket \cdot \rrbracket (\sigma_0) \f$ in
 * the production \f$\verb|program|\f$, cases (iv) -- (viii).
 *@param in An element of \f$L\f$ (intuitively, the actual program).
 *@param S The initial data structure for \f$S\f$
*/
void semantics_initial_statement_list(struct ASTNode *in, SequenceOfStates S)
{
    Statement ptr = in->Prologue;
    semantics_statement_list(ptr, S);
}

/**
 * @brief Implementation of \f$\mathcal{I} \colon [\verb|function_definition|] \to \{ S \to S \}\f$,
 * i.e. 
 * \f$\sigma' = \mathcal{I}\llbracket a \rrbracket (\sigma_0,\sigma)\f$ is generated.
 * @param in The abstract syntax tree
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/

void semantics_function_definition(struct ASTNode *in, SequenceOfStates S)
{
    FunctionDefinition fct = in->Function;
    Statement stmt = fct->body;
    semantics_function_declaration(fct->head, S);
    semantics_function_head(fct, S);
    semantics_statement_list(stmt, S);
}

/** @brief Implementation of \f$\mathcal{I} \colon [\verb|ode_definition|] \to \{ S \to S \times S \} \f$
 * @param in The abstract syntax tree
 * @param S Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/

void semantics_ode_definition(struct ASTNode *in, SequenceOfStates S)
{
    Ode ode = in->ode;
    if (ode == NULL)
	return;

    semantics_function_declaration(ode->phi->head, S);
    semantics_function_head(ode->phi, S);
    semantics_ode_equation(ode, in->Function, S);
    semantics_initialvalue_list(ode->initval, S);

    Variable x;
    if (NULL != (x = find_in_variables_match_role_at_date(OUTPUT_VAR, get_date(S), S)))
	ERROR_ODE_OUTPUT_NOT_STATE(name_of(x), in->ode->line);
    if (NULL == (x = find_in_variables_match_role_at_date(INPUT_VAR, get_date(S), S)))
	ERROR_ODE_NO_INPUT(in->ode->line);
}
