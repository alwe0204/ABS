/**
 * @file expressions.c
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 24 Mar 2016
 * @brief Functions operating on expressions
*/

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

#include "expressions.h"
#include "reduce_int.h"
#include "errors.h"


/**
 * @brief Implementation of \f$\operatorname{read} \colon [ \verb|postfix_expression|] \to \{ S \to \overline{\mathbb{R}} \cup \mathcal{F}_{\overline{\mathbb{R}}} \}\f$
 * @param in The argument \f$a_1\f$
 * @param S The current state \f$\sigma \in S \f$
 * @returns A pointer to the expression that is assigned to \p in is returned
*/
Expression definition_of(PostFixExpression in, ConstSequenceOfStates S)
{
    if (in == NULL)
	return NULL;
    dim_t row, col;
    IsEntry(in, S);
    get_indices_of_PostFixExpression(in, &row, &col, 0, S);
    const date_t a = get_date_of_postfix(in, S);
    Variable x = version_of_variable(find_in_variables_check_declared(in, S), row,
				     col, a);
    assert(x != NULL);
    return data_of(x, row, col);
}

/**
 * @param in The argument \f$a_1\f$
 * @param S The current state \f$\sigma \in S \f$
 * @returns A pointer to the expression that is assigned to \p in is returned
*/
Expression interval_of(PostFixExpression in, ConstSequenceOfStates S)
{
    if (in == NULL)
	return NULL;
    dim_t row, col;
    IsEntry(in, S);
    get_indices_of_PostFixExpression(in, &row, &col, 0, S);
    const date_t a = get_date_of_postfix(in, S);
    Variable x = version_of_variable(find_in_variables_check_declared(in, S), row,
				     col, a);
    assert(x != NULL);
    return intv_of(x, row, col);
}

/** @brief Returns \f$\sigma(\operatorname{name}\llbracket a \rrbracket )_\mathrm{type} \f$
 * @param in \f$a \in [\verb|postfix_expression|]\f$
 * @param S The current state \f$\sigma \in S \f$
 * 
*/
enum set_T type_of(PostFixExpression in, ConstSequenceOfStates S)
{
    const date_t a = get_date_of_postfix(in, S);
    struct variable *x = version_of_variable(find_in_variables_check_declared(in, S), 0, 0,
					     a);
    assert(x != NULL);
    return var_type_of(x);
}

/** @brief Returns \f$\sigma(\operatorname{name}\llbracket a \rrbracket )_\mathrm{idx} \f$
 * @param in \f$a \in [\verb|postfix_expression|]\f$
 * @param S The current state \f$\sigma \in S \f$
 * 
*/
dim_t idx_of(PostFixExpression in, ConstSequenceOfStates S)
{
    const date_t a = get_date_of_postfix(in, S);
    struct variable *x = version_of_variable(find_in_variables_check_declared(in, S), 0, 0,
					     a);
    assert(x != NULL);
    return var_idx(x);
}

/** @brief Returns \f$\sigma(\operatorname{name}\llbracket a \rrbracket )_\mathrm{role} \f$
 * @param in \f$a \in [\verb|postfix_expression|]\f$
 * @param S The current state \f$\sigma \in S \f$
 * 
*/

enum set_R role_of(PostFixExpression in, ConstSequenceOfStates S)
{
    const date_t a = get_date_of_postfix(in, S);
    struct variable *x = version_of_variable(find_in_variables_check_declared(in, S), 0, 0,
					     a);
    assert(x != NULL);
    return var_role_of(x);
}


/**
 * @brief Implementation of the test \f$ \sigma(\verb|x|)_\mathrm{role} = r\f$, 
 * where \f$r \in \mathbf{R} \f$. 
 * @param in \f$a \in [\verb|postfix_expression|]\f$
 * @param role \f$r \in \mathbf{R}\f$
 * @param S The current state \f$\sigma \in S \f$
 *
 * @returns \c true if \f$ \sigma(\verb|x|)_\mathrm{role} = r\f$ otherwise \c false
*/

int is_role_of(PostFixExpression in, enum set_R const role, ConstSequenceOfStates S)
{
    return ((role_of(in, S) == role) ? true : false);
}


/**
 * @param t Type of expression
 * @returns \c true if \p t is one of ::PLUS, ::MINUS, ::TIMES, ::DIV, ::POW, ::EXP else \c false
*/
int is_Expression_type_elementary_operation(enum expression_type t)
{
    return ((t == PLUS || t == MINUS || t == TIMES || t == DIV || t == POW
	     || t == EXP) ? true : false);
}

/**
 * @param in The input Expression
 * @returns \c true if the type of \p in is one of ::PLUS, ::MINUS, ::TIMES, ::DIV, ::POW, ::EXP else \c false
 * 
 *
 * The function also includes a consistency check.
 * 
 * The core of this function is is_Expression_type_elementary_operation()
 *
*/
int is_Expression_elementary_operation(Expression in)
{
    if (in == NULL)
	return false;
    if (is_Expression_type_elementary_operation(TYPE(in))) {
	return ((in->left != NULL && in->right != NULL) ? true : false);
    } else
	return false;
}

/**
 * @param in The input Expression
 * @returns \c true if \p in is one of ::NUM_INT, ::NUM_DEC, ::NUM_CONST, otherwise returns \c false
 *
 *
 * Assumptions: 
 * TYPE(\p in) is set if \p in != \c NULL
*/
int is_Expression_literal_Number(Expression in)
{
    return (in != NULL
	    && (TYPE(in) == NUM_INT || TYPE(in) == NUM_DEC
		|| TYPE(in) == NUM_CONST) ? true : false);
}

/**
 * @returns \c true if \p in is one of ::NUM_INT, ::NUM_DEC, ::NUM_CONST, ::POSTFIX,  otherwise returns \c false.
 * @param type The type of an expression
*/

int is_Expression_type_number(enum expression_type type)
{
    return ((type == NUM_INT || type == NUM_DEC || type == NUM_CONST
	     || type == POSTFIX) ? true : false);
}

/** 
 * @returns \c true if \p in is one of ::NUM_INT, ::NUM_DEC, ::NUM_CONST, ::POSTFIX,  otherwise returns \c false.
          If \c NULL is the input then the program terminates.
 * @param in The input Expression
 *
 *
 * Assumptions: 
 * TYPE(\p in) is set if \p in != \c NULL
*/

int is_Expression_Number(Expression in)
{
    if (in == NULL)
	return false;
    else
	return is_Expression_type_number(TYPE(in));
}

/** 
 * @brief Returns \c true if \p in is one of ::DISC_INTV or ::CONT_INTV, i.e. an interval, otherwise returns \c false.
 * @param in The input Expression
 *
 *
*/
int is_Expression_Interval(Expression in)
{
    if (in == NULL || in->left == NULL || in->right == NULL)
	return false;
    else
	return ((TYPE(in) == DISC_INTV || TYPE(in) == CONT_INTV) ? true : false);
}

/** 
 * @brief Returns \c true if \p in is one of ::REAL_SPACE otherwise returns \c false.
 * @param in The input Expression
 *
 *
*/
int is_Expression_RealSpace(Expression in)
{
    if (in == NULL)
	return false;
    else
	return ((TYPE(in) == REAL_SPACE) ? true : false);
}

/**
 * @param type Type of expression
 * @returns \c true if \p type is one of ::FCT_ATAN, ::FCT_COS, ::FCT_COSH, ::FCT_EXP, ::FCT_LOG, ::FCT_SIN, ::FCT_SINH, ::FCT_SQRT, ::FCT_TAN otherwise returns \c false
*/
int is_Expression_type_function(enum expression_type type)
{
    switch (type) {
    case FCT_ATAN:
    case FCT_COS:
    case FCT_COSH:
    case FCT_EXP:
    case FCT_LOG:
    case FCT_SIN:
    case FCT_SINH:
    case FCT_SQRT:
    case FCT_TAN:
	return true;
	break;
    default:
	return false;
	break;
    }
}

/** @brief Wrapper for is_Expression_type_function()
*/

int is_Expression_function(Expression in)
{
    if (in == NULL || in->right == NULL)
	return false;
    else
	return (is_Expression_type_function(TYPE(in)));
}


/** @Returns \c true if \f$x \in [\verb|identifier|] \f$ else \c false
 * @param in \f$x\f$
*/
int is_PostFixExpression_Identifier(PostFixExpression in)
{
    assert(in != NULL);
    assert(TYPE(in) == POSTFIX);
    return ((ROW_IDX(in) == NULL && COL_IDX(in) == NULL) ? true : false);
}

int is_PostFixExpression_Vector(PostFixExpression in)
{
    assert(in != NULL);
    assert(TYPE(in) == POSTFIX);
    return ((ROW_IDX(in) != NULL && COL_IDX(in) == NULL) ? true : false);
}

int is_PostFixExpression_Matrix(PostFixExpression in)
{
    assert(in != NULL);
    assert(TYPE(in) == POSTFIX);
    return ((ROW_IDX(in) != NULL && COL_IDX(in) != NULL) ? true : false);
}


/** @returns \c true if \p in is equal to \p val otherwise \c false
 * @param in The first operand
 * @param val The second operand
 * @param S The sequence of states
*/
int is_Expression_equal_to_Int(Expression in, int val, ConstSequenceOfStates S)
{
    if (!is_Expression_Number(in))
	return false;
    if (TYPE(in) == NUM_INT && GET_INT(in) == val)
	return true;
    else if (TYPE(in) == POSTFIX) {
	Expression e = definition_of(in, S);
	return (is_Expression_equal_to_Int(e, val, S));
    } else
	return false;
}

/**
 * @brief Applies is_Expression_equal_to_Int() to 0.
 * @param in The input expression
 * @param S The current state \f$\sigma \in S \f$
*/

int is_Expression_equal_to_zero(const Expression in, ConstSequenceOfStates S)
{
    return is_Expression_equal_to_Int(in, 0, S);
}

/**
 * @brief Applies is_Expression_equal_to_Int() to 1.
 * @param in The input expression
 * @param S The current state \f$\sigma \in S \f$
*/
int is_Expression_equal_to_one(const Expression in, ConstSequenceOfStates S)
{
    return is_Expression_equal_to_Int(in, 1, S);
}

/** @brief Applies ::is_Expression_Interval to \f$\operatorname{name}\llbracket a \rrbracket\f$
 * @param in \f$a \in [\verb|postfix_expression|]\f$
 * @param S The current state \f$\sigma \in S \f$
*/
int is_PostFixExpression_Interval(PostFixExpression in, ConstSequenceOfStates S)
{
    return (is_Expression_Interval(definition_of(in, S)) ? true : false);
}



/**
 *@brief Function copies the input expression.
 *@param in The input expression
 *@return The return value points to the copy.
*/
Expression copy_Expression(Expression in)
{
    if (in == NULL)
	return NULL;
    Expression out = alloc_expression();
    TYPE(out) = TYPE(in);
    if (is_Expression_literal_Number(in) || is_Expression_function(in)) {
	out->content = in->content;
	out->line = in->line;
    }
    if (TYPE(in) == POSTFIX) {
	out->name = (char *) my_malloc(ID_LEN * sizeof(char));
	strcpy(out->name, in->name);
	out->line = in->line;
	if (CONTENT(in) != NULL) {
	    assert(*((unsigned int *) CONTENT(in)) >= 0);
	    CONTENT(out) = my_malloc(sizeof(unsigned int));
	    *((unsigned int *) CONTENT(out)) = *((unsigned int *) CONTENT(in));
	}
    }
    out->left = copy_Expression(in->left);
    out->right = copy_Expression(in->right);
    return out;
}


void
replace_PostFixExpression_by_its_Expression(Expression * op1,
					    PostFixExpression op2, ConstSequenceOfStates S)
{
    dim_t row, col;
    dim_t row1, col1;
    if (TYPE(*op1) == POSTFIX) {
	get_indices_of_PostFixExpression(op2, &row, &col, 0, S);
	if (strcmp(NAME(op2), NAME(*op1)) == 0) {
	    get_indices_of_PostFixExpression(*op1, &row1, &col1, 0, S);
	    if (row == row1 && col == col1) {
		*op1 = copy_Expression(definition_of(op2, S));
	    }
	}
    }
}

/** @brief Method to handle definitions with self reference, e.g. \f$\verb|x=x+1;|\f$.
 * Used in semantics_definition()
 * @param op1
 * @param op2
*/
void
remove_PostFixExpression_involved_in_Expression(Expression * op1,
						PostFixExpression op2, ConstSequenceOfStates S)
{
    if (is_Expression_literal_Number(*op1)) {
	return;
    }
    replace_PostFixExpression_by_its_Expression(op1, op2, S);
    if ((*op1)->left != NULL) {
	remove_PostFixExpression_involved_in_Expression(&((*op1)->left), op2, S);
    }
    if ((*op1)->right != NULL) {
	remove_PostFixExpression_involved_in_Expression(&((*op1)->right), op2, S);
    }
}


int make_definition_pow_or_exp(Expression in, ConstSequenceOfStates S)
{
    if (TYPE(in) != EXP || TYPE(in) == POW)
	return false;
    if (!reduce_expression_to_int(in->right, S)) {
	Expression iln = install_expression(FCT_LOG, NULL, (void *) in->left);
	Expression itimes = install_expression(TIMES, in->right, iln);
	in->type = FCT_EXP;
	in->left = NULL;
	in->right = itimes;
	return true;
    } else {
	TYPE(in) = POW;
	return false;
    }
}

/**
 * @brief Copies the content of the expression \p src to the expression \p tar
 *
 * @param src source expression
 * @param tar target expression
*/
void ExpressionCopy(Expression tar, Expression src)
{
    TYPE(tar) = TYPE(src);
    tar->left = src->left;
    tar->right = src->right;
    tar->name = src->name;
    tar->line = src->line;
    tar->content = src->content;
    tar->ad = src->ad;
    tar->next = src->next;
    tar->prev = src->prev;
}

/**
 *
 *@param in The input expression
 *@param S \f$\sigma \in S\f$
*/

Expression simplify_Expression(Expression in, ConstSequenceOfStates S)
{
    Expression e = NULL;
    reduce_expression_to_int(in, S);
    if (is_Expression_elementary_operation(in)) {
	switch (TYPE(in)) {
	case PLUS:
	    simplify_Expression(in->left, S);
	    simplify_Expression(in->right, S);

	    if (is_Expression_equal_to_zero(in->left, S)) {
		e = in->right;
	    } else if (is_Expression_equal_to_zero(in->right, S)) {
		e = in->left;
	    }
	    if (e != NULL) {
		ExpressionCopy(in, e);
		simplify_Expression(in, S);
	    }
	    break;
	case TIMES:
	    {
		simplify_Expression(in->left, S);
		simplify_Expression(in->right, S);
		if (is_Expression_equal_to_Int(in->left, 0, S)
		    || is_Expression_equal_to_Int(in->right, 0, S)) {
		    e = install_Number_int(0);
		} else if (is_Expression_equal_to_one(in->left, S)) {
		    e = in->right;
		} else if (is_Expression_equal_to_one(in->right, S)) {
		    e = in->left;
		}
		if (e != NULL) {
		    ExpressionCopy(in, e);
		    simplify_Expression(in, S);
		}
	    }
	    break;
	case POW:
	case EXP:
	    simplify_Expression(in->right, S);
	    if (is_Expression_equal_to_one(in->right, S)) {
		e = in->left;
	    }
	    if (e != NULL) {
		TYPE(in) = TYPE(e);
		in->left = e->left;
		in->right = e->right;
		in->name = e->name;
		in->line = e->line;
		simplify_Expression(in, S);
	    }
	    break;
	default:
	    simplify_Expression(in->left, S);
	    simplify_Expression(in->right, S);
	    break;
	}
    }
    if (TYPE(in) == POSTFIX && definition_of(in, S) != NULL) {
	simplify_Expression(definition_of(in, S), S);
    }
    return in;
}


/**
 * @brief Implementation of \f$\operatorname{IsBoundedInterval} \colon [\verb|expression_or_constant_interval_expression|] \to \{S \to \{0,1\}\} \f$.
 * @returns \c true if \p in is an interval and both end points of the interval are computable otherwise \c false
 * @param in The input Expression
 * @param S A sequence \f$(\sigma_0,\ldots,\sigma_k,\ldots,\sigma_n)\f$ of states in \f$S\f$
*/
int is_Expression_bounded_Interval(Expression in, ConstSequenceOfStates S)
{
    if (in == NULL)
	return false;
    if (TYPE(in) == POSTFIX)
	return is_Expression_bounded_Interval(definition_of(in, S), S);
    else if (!is_Expression_Interval(in))
	return false;
    else {
	if (is_Expression_computable(in->left, S, WITHOUT_INPUT) == false
	    || is_Expression_computable(in->right, S, WITHOUT_INPUT) == false) {
	    return false;
	} else
	    return true;
    }
}


/**
 *@brief Checks the assumption that an interval is bounded.
 *@param in The input expression
 *@param name The variable to be assigned to the interval
 *@param line The line in the input file where the assumption must be verified
*/
void assume_interval_bounded(Expression in, char *name, unsigned int line, ConstSequenceOfStates S)
{
    assert(in != NULL);
    if (is_Expression_bounded_Interval(in, S) == false)
	ERROR_UNBOUNDED(name, line);
}


/**
 *@brief Checks whether \f$E \in [\verb|expression|]\f$ satisfies \f$\mathcal{E}\llbracket E \rrbracket (\sigma) \in \overline{\mathbb{R}}\f$ or 
 * \f$\mathcal{I}\llbracket E \rrbracket (\sigma) \in F_{\overline{\mathbb{R}}}\f$, respectively,
 * where \f$\mathcal{E}\f$ be the interpretation function for \f$\texttt{expression}\f$
 *@param in The input expression \f$E\f$
 *@param S \f$\sigma \in S\f$ 
 *@param mode One of ::WITH_INPUT (associated with \f$F_\mathbb{R}\f$), ::WITHOUT_INPUT (associated with \f$\mathbb{R}\f$)
*/
int is_Expression_computable(Expression in, ConstSequenceOfStates S, int mode)
{
    if (in == NULL)
	return false;
    if (is_Expression_literal_Number(in))
	return true;
    else if (TYPE(in) == POSTFIX) {
	if (CONTENT(in) == NULL) {
	    CONTENT(in) = my_malloc(sizeof(unsigned int));
	    *((unsigned int *) CONTENT(in)) = get_date(S) - 1;
	}
	IsEntry(in, S);
	if (type_of(in, S) != TY_REAL)
	    return false;
	enum set_R role = role_of(in, S);
	if (!(role == ORDINARY_VAR || role == ITER_VAR || role == OUTPUT_VAR || role == INPUT_VAR))
	    return false;
	if (role == INPUT_VAR)
	    return mode;
	else if (role == ITER_VAR)
	    return true;
     else if (in->computable == true)
         return true;
     else {
         in->computable = is_Expression_computable(definition_of(in, S), S, mode);
	    return in->computable;
     }
    } else if (is_Expression_elementary_operation(in)) {
	assert(in != NULL);
	if (reduce_expression_to_int(in, S))
	    return true;
	if (make_definition_pow_or_exp(in, S))
	    return is_Expression_computable(in, S, mode);
	if (is_Expression_computable(in->left, S, mode)
	    && is_Expression_computable(in->right, S, mode))
	    return true;
	else
	    return false;
    } else if (is_Expression_function(in)) {
	Expression p;
	for (p = in->right; p != NULL; p = p->prev) {
	    if (is_Expression_computable(p, S, mode) == false)
		return false;
	}
	return true;
    } else if (is_Expression_Interval(in)) {
	return is_Expression_bounded_Interval(in, S);
    } else
	return false;
}

/** @brief Let \f$\mathcal{E}\f$ be the interpretation function for \f$\texttt{expression}\f$ and let \f$E \in [\texttt{expression}]\f$.
 * Returns \c true if \f$\mathcal{E}\llbracket E \rrbracket (\sigma) \in \overline{\mathbb{R}} \f$ otherwise \c false
 * @param in \f$E\f$
 * @param S A sequence \f$(\sigma_0,\ldots,\sigma_k,\ldots,\sigma_n)\f$ of states in \f$S\f$
*/
int is_Expression_constant(const Expression in, ConstSequenceOfStates S)
{
    return is_Expression_computable(in, S, WITHOUT_INPUT);
}


/** @brief Implementation of \f$\operatorname{dimension} \colon [\verb|identifier_or_interval|] \to \{S \to \mathbb{Z}_+\}\f$
 * It is, however, prepared for future generalizations to matrix-valued inputs.
 * @param in The input expression
 * @param r If \p r is not \c NULL then it is set to the found row dimension 
 * @param c If \p c is not \c NULL then it is set to the found column dimension 
 * @param S The current state \f$\sigma \in S \f$.
 * 
 *
 * Assumptions: 
 * -# \p in != \c NULL
 * -# \p c and \p r shall be initialized because they are actually incremented and not set. 
 * This is done to use this function with get_dimensions_of_expression_list()
*/
void dimension(dim_t * r, dim_t * c, const Expression in, ConstSequenceOfStates S)
{
    assert(in != NULL && TYPE(in) != LIST);
    struct variable *x;
    switch (TYPE(in)) {
    case POSTFIX:
	x = check_declared(in, S);
	check_defined(x);
	if (var_type_of(x) != TY_INTV)
	    ERROR_DECL_NOT_INTERVAL(NAME(in), in->line) if (r != NULL) {
		*r += row_dim_of(x);
	    }
	if (c != NULL) {
	    *c += col_dim_of(x);
	}
	if (r == NULL && row_dim_of(x) != 1)
	    ERROR_NOT_SCALAR(NAME(in), in->line)
		if (c == NULL && col_dim_of(x) != 1)
		ERROR_NOT_VECTOR(NAME(in), in->line) break;
    case DISC_INTV:
    case CONT_INTV:
	if (r != NULL) {
	    *r += 1;
	}
	if (c != NULL) {
	    *c = 1;
	}
	break;
    case REAL_SPACE:
	if (in->left == NULL && in->right == NULL) {
	    if (r != NULL) {
		*r += 1;
	    }
	    if (c != NULL) {
		*c = 1;
	    }
	} else if (in->right != NULL && in->left == NULL) {
	    dim_t r_tmp;
	    if (get_int_from_expression(in->right, S) > 0) {
		r_tmp = get_int_from_expression(in->right, S);
		if (r != NULL)
		    *r += r_tmp;
		if (r == NULL && r_tmp != 1)
		    ERROR_NOT_ONE_DIMENSION(in->line);
	    } else
		ERROR_IDX_NOT_POSITIVE(NAME(in), in->line);
	    if (c != NULL) {
		*c = 1;
	    }
	} else
	    assert(0);
	break;
    default:
	assert(0);
	break;
    }
}

/** @brief Gets the declared dimensions of a list of expressions from the variable list
 * @param in The input expression
 * @param r If \p r is not \c NULL then it is set to the found row dimension 
 * @param c If \p c is not \c NULL then it is set to the found column dimension 
 * @param S The current state \f$\sigma \in S \f$.
 * 
 *
 * Assumptions: 
 * \p in != \c NULL
*/
void
get_dimensions_of_expression_list(dim_t * r, dim_t * c,
				  const Expression in, ConstSequenceOfStates S)
{
    Identifier p = in;
    while (p != NULL) {
	switch (TYPE(p)) {
	case REAL_SPACE:
	    dimension(r, c, p, S);
	    break;
	case LIST:
	    get_dimensions_of_expression_list(r, c, p->left, S);
	    break;
	default:
	    dimension(r, c, p, S);
	    break;
	}
	p = p->prev;
    }
}

/** @param in The input Expression
 * @returns The length of the expression, i.e., the number of expressions linked (linearly) to \p in.
*/
unsigned int get_length_of_expression_list(Expression in)
{
    unsigned int out = 0;
    Expression p = in;
    while (p != NULL) {
	out++;
	p = p->prev;
	assert(out != 0);
    }
    return out;
}



/**
 * @brief Returns a vector with the dimensions of each entry of the identifier_list. 
 * The zeroth entry is the length of vector.
 * @param in The input list
 * @returns A dim_t pointer whose length is equal to the length of the list plus 1.
*/
dim_t *dimensions_of_expression_list(const Identifier in, ConstSequenceOfStates S)
{
    int i = 1;
    struct variable *x;
    dim_t a = get_length_of_expression_list(in);
    dim_t *out = my_malloc((1 + 2 * a) * sizeof(dim_t));
    out[0] = a;
    Identifier p = (Expression) wind_back(AST_EXPR, in);
    while (p != NULL) {
	switch (TYPE(p)) {
	case POSTFIX:
	    x = find_in_variables(p, S);
	    out[i] = row_dim_of(x);
	    out[i + a] = col_dim_of(x);
	    break;
	case DISC_INTV:
	case CONT_INTV:
	    out[i] = 1;
	    out[i + a] = 1;
	    break;
	case LIST:
	    get_dimensions_of_expression_list(&(out[i]), &(out[i + a]), p->left, S);
	    break;
	default:
	    assert(0);
	}
	i++;
	p = p->next;
    }
    return out;

}


dim_t get_dim_row(const Expression in, ConstSequenceOfStates S)
{
    dim_t r = 0, r2 = 0;
    get_dimensions_of_expression_list(&r, NULL, in, S);
    if (in->prev == NULL)
	return r;
    else {
	get_dimensions_of_expression_list(&r2, NULL, in->prev, S);
	return (r - r2);
    }
}

/** @brief Implementation of the test \f$\mathcal{E}\llbracket E \rrbracket (\sigma) \neq \zeta \f$
 * to be used in semantics_definition()
 * @param in \f$E\f$
 * @param S A sequence \f$(\sigma_0,\ldots,\sigma_k,\ldots,\sigma_n)\f$ of states in \f$S\f$
*/

void check_valid_real_assignment(Assignment in, ConstSequenceOfStates S)
{
    if (is_Expression_computable(in->right, S, WITH_INPUT) == false) {
	dim_t row, col;
	get_indices_of_PostFixExpression(in->left, &row, &col, 0, S);
	ERROR_UNBOUNDED_ROW_COL(NAME(in->left), row, col, in->line);
    }
}

/**
 * @param in  \f$a\f$
 * @param S A sequence \f$(\sigma_0,\ldots,\sigma_k,\ldots,\sigma_n)\f$ of states in \f$S\f$
 * @returns \f$k\f$, where \f$a\f$ appears at date \f$k\f$ in the program
*/
date_t get_date_of_postfix(PostFixExpression in, ConstSequenceOfStates S)
{
    assert(in != NULL && TYPE(in) == POSTFIX);
    return (CONTENT(in) == NULL ? get_date(S) : *((unsigned int *) CONTENT(in)));
}

/** @brief If \p vec represents initially \f$(a_1,\ldots,a_n)\f$ with \f$a_i \in \mathbb{N}\f$ for all \f$i\f$ then 
* after execution \p vec represents \f$(a_1,a_1 + a_2,\ldots,a_1 + \ldots + a_n) \f$. 
* @param vec The input vector
* @param len \f$n\f$
*/

void accumulate(dim_t * vec, const dim_t len)
{
    dim_t i;
    for (i = 1; i < len; i++) {
	vec[i] += vec[i - 1];
    }
}

dim_t *make_abs_dim(const dim_t * in)
{
    dim_t i;
    dim_t *out = my_malloc((1 + in[0]) * sizeof(dim_t));
    out[0] = 0;
    for (i = 1; i <= in[0]; i++) {
	out[i] = in[i] * in[i + in[0]];
    }
    accumulate(out, in[0] + 1);
    return out;
}
