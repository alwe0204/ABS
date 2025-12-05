/**
 * @file reduce_int.c
 * @author Alexander Weber
 * @date 2 Feb 2016
 * @brief Functions to evaluate an expression that includes only integers. 
*/

#include <gmp.h>
#include <limits.h>
#include <assert.h>
#include <stdbool.h>

#include "reduce_int.h"

#include "errors.h"
#include "expressions.h"





/** 
 * @brief Implementation of the pair \f$ ( \operatorname{row}_\varepsilon,\operatorname{col}_\varepsilon ) \f$
 * @param in The PostFixExpression to work on
 * @param row The location where the image of \f$\operatorname{row}_\varepsilon\f$ is stored
 * @param col The location where the image of \f$\operatorname{col}_\varepsilon\f$ is stored
 * @param epsilon The parameter \f$\varepsilon \f$
 * @param S The current state \f$\sigma \in S\f$
 *
 *
 * This function implements the pair of functions
 * \f$ ( \operatorname{row}_\varepsilon,\operatorname{col}_\varepsilon ) \colon [ \verb|postfix_expression| ] \to \{ S \to [0;n_\mathrm{max}] \cup \{ \zeta \} \}^2\f$. In particular, it checks \f$\exists_{b \in \{0,\zeta\}} b \in \{\operatorname{row}_1\llbracket a \rrbracket(\sigma),\operatorname{col}_1\llbracket a \rrbracket(\sigma)\}\f$
*/
void
get_indices_of_PostFixExpression(PostFixExpression in, dim_t * row,
				 dim_t * col, int epsilon, ConstSequenceOfStates S)
{
    assert(epsilon == 0 || epsilon == 1);
    assert(in != NULL);
    assert(TYPE(in) == POSTFIX);
    int row_tmp, col_tmp;
    if (ROW_IDX(in) != NULL) {
	row_tmp = get_int_from_expression(ROW_IDX(in), S);
    } else
	row_tmp = epsilon;
    if (COL_IDX(in) != NULL) {
	col_tmp = get_int_from_expression(COL_IDX(in), S);
    } else
	col_tmp = epsilon;
    if (row_tmp < epsilon || col_tmp < epsilon) {
	if (epsilon == 0) {
	    ERROR_IDX_NOT_NONNEGATIVE(NAME(in), in->line);
	} else {
	    ERROR_IDX_NOT_POSITIVE(NAME(in), in->line);
	}
    }
    int tmp;
    compute_int_expression(&tmp, TIMES, ROW_IDX(in), COL_IDX(in), in->line);	// this is needed to ensure that row*col does not overflow.
    *row = (dim_t) row_tmp;
    *col = (dim_t) col_tmp;
}

/** @brief Computes the integer value of two integers and includes a test for division by zero and for overflow as follows.
 * \f$ c = g(a,b) \text{ if } a,b,c \in \mathbb{Z} \text{ and } n_\mathrm{min} < c < n_\mathrm{max} \text{ otherwise } c = \zeta \f$. 
 * Here, \f$n_\mathrm{min}\f$ and \f$n_\mathrm{max}\f$ are specified by #INT_MIN and #INT_MAX, respectively. 
 * The constants are defined in limits.h
 * @param result \f$c\f$
 * @param type \f$g\f$, one of ::PLUS, ::MINUS, ::TIMES, ::DIV, ::POW, ::EXP
 * @param x \f$a\f$
 * @param y \f$b\f$
 * @param line The line in which the operation appears in the input file
 * @Returns \c true if \f$c \neq \zeta\f$ otherwise \c false
 * 
 * 
 * In words, an overflow is returned if
 * -# the integer value is not greater than #INT_MAX or not less than #INT_MIN
 * -# a division by zero is detected
 *
*/
int
compute_int_expression_core(int *result, enum expression_type type, int x, int y, unsigned int line)
{
    mpz_t a, b, c, d;
    mpz_inits(a, b, c, d, NULL);
    mpz_set_si(a, (signed long int) x);
    mpz_set_si(b, (signed long int) y);
    switch (type) {
    case PLUS:
	mpz_add(c, a, b);
	break;
    case MINUS:
	mpz_sub(c, a, b);
	break;
    case TIMES:
	mpz_mul(c, a, b);
	break;
    case DIV:
	if (mpz_cmp_si(b, 0) == 0) {
	    ERROR_DIV_BY_ZERO(line);
	} else {
	    mpz_fdiv_q(c, a, b);
	    mpz_fdiv_r(d, a, b);
	    if (mpz_cmp_si(d, 0) != 0) {
		mpz_clears(a, b, c, d, NULL);
		return false;
	    }
	}
	break;
    case POW:
    case EXP:
	if (y < 0) {
	    mpz_clears(a, b, c, d, NULL);
	    return false;
	}
	mpz_pow_ui(c, a, (unsigned long int) y);
	break;
    default:
	assert(0);
    }
    if (mpz_cmp_si(c, INT_MAX) < 0 && mpz_cmp_si(c, INT_MIN) > 0) {
	*result = mpz_get_si(c);
    } else {
	ERROR_INT_OVERFLOW(line);
    }
    mpz_clears(a, b, c, d, NULL);
    return true;
}





/** @brief Wrapper for compute_int_expression_core() for expressions rather than integers
 * @param result See compute_int_expression_core()
 * @param type See compute_int_expression_core()
 * @param op1 The first operand
 * @param op2 The second operand
 * @param line See compute_int_expression_core()
*/

int
compute_int_expression(int *result, enum expression_type type,
		       Expression op1, Expression op2, unsigned int line)
{
    if (op1 == NULL || op2 == NULL)
	return false;
    if (TYPE(op1) != NUM_INT || TYPE(op2) != NUM_INT)
	return false;
    return compute_int_expression_core(result, type,
				       *((int *) op1->content), *((int *) op2->content), line);
}

/** @brief Another wrapper for compute_int_expression_core() where the integer operation
 * is included in \p in. It throws #ERROR_IDX_NOT_INT if \p in is not an integer operation.  
 * @param in The expression to process
 * @param S The sequence of states
 */
int expression_to_int(Expression in, ConstSequenceOfStates S)
{
    assert(in != NULL);
    const enum expression_type type = in->type;
    assert(!(type == CONT_INTV || type == DISC_INTV || type == LIST || type == REAL_SPACE));
    if (type == NUM_INT)
	return *((int *) CONTENT(in));
    if (type == NUM_DEC || type == NUM_CONST || is_Expression_type_function(type))
	ERROR_IDX_NOT_INT(in, in->line);
    if (type == POSTFIX) {
	Expression ptr = definition_of(in, S);
	if (ptr == NULL)
	    ERROR_IDX_NOT_INT(in, in->line);
	return expression_to_int(ptr, S);
    }
    if (!(in->left != NULL && in->right != NULL))
	ERROR_IDX_NOT_INT(in, in->line) int a = expression_to_int(in->left, S);
    int b = expression_to_int(in->right, S);
    int res;
    compute_int_expression_core(&res, in->type, a, b, in->line);
    return res;
}


/** @brief Tries to set the input expression to type #NUM_INT. Uses compute_int_expression()
 * @param in The input expression
 * @param S The current state \f$\sigma \in S\f$
 * @return \c true on success otherwise \c false
 * 
*/
int reduce_expression_to_int(Expression in, ConstSequenceOfStates S)
{
    assert(in != NULL);
    const enum expression_type type = in->type;
    if (type == CONT_INTV || type == DISC_INTV || type == LIST || type == REAL_SPACE)
	return false;
    if (type == NUM_INT)
	return true;
    if (type == NUM_DEC || type == NUM_CONST || is_Expression_type_function(type))
	return false;
    if (type == POSTFIX) {
	Expression ptr = definition_of(in, S);
	if (ptr == NULL)
	    return false;
	if (reduce_expression_to_int(ptr, S) == false)
	    return false;
	in->type = NUM_INT;
	if (in->content != NULL)
	    free(in->content);
	in->content = my_malloc(sizeof(int));
	*((int *) in->content) = *((int *) ptr->content);
	return true;
    }
    assert(!(in->left == NULL || in->right == NULL));
    if (in->left->type != NUM_INT || in->right->type != NUM_INT) {
	if (reduce_expression_to_int(in->left, S) == false)
	    return false;
	if (reduce_expression_to_int(in->right, S) == false)
	    return false;
	return reduce_expression_to_int(in, S);
    } else {
	in->content = malloc(sizeof(int));
	if (compute_int_expression
	    (((int *) in->content), in->type, in->left, in->right, in->line) == false) {
	    free(in->content);
	    return false;
	} else {
	    in->type = NUM_INT;
	    return true;
	}
    }
    assert(0);
}

/** @brief Returns the result of an integer expression. It uses reduce_expression_to_int()
 * and throws #ERROR_IDX_NOT_INT if the result is not an integer
 * @param in The integer expression
 * @param S The sequence of states
 * @return The integer result
 */
int get_int_from_expression(Expression in, ConstSequenceOfStates S)
{
    if (reduce_expression_to_int(in, S) == 0)
	ERROR_IDX_NOT_INT(in, in->line);
    assert(TYPE(in) == NUM_INT);
    return *((int *) CONTENT(in));
}
