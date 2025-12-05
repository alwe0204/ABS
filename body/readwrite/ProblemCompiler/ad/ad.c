/** @file ad.c
 * @brief Automatic differentiation
 * @author Alexander Weber (a.weber@unibw.de)
 *
 *
 * This file includes the algorithms for
 * -# generating an evaluation tree of a function.
 * -# generating derivatives of a function in terms of an evaluation trace. (An evaluation trace is the evaluation tree of a function plus the evaluation tree of derivatives.) 
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <errno.h>
#include <assert.h>
#include <stdbool.h>

#include "ad.h"
#include "lists.h"
#include "expressions.h"

#include "reduce_int.h"
#include "errors.h"


#define DERIVATIVE(X,Y) (tri->list[X]).D[(Y)]

#define ADD_ONE insert_integer_to_three_variable_list(1,tri)
#define ADD_TWO insert_integer_to_three_variable_list(2,tri)

#define SET_UNUSED(X) ((tri->list[X].bits) |= 1)
#define IS_UNUSED(X)  ((tri->list[X].bits) & 1 )
#define IS_USED(X)  (!((tri->list[X].bits) & 1 ))
#define SET_USED(X) ((tri->list[X].bits) &= ~(1<<0))

#define SET_USED_ONCE(X)  ((tri->list[X].bits) |=  (1<<1) )
#define SET_USED_TWICE(X)  ((tri->list[X].bits) |=  (1<<2) )

#define IS_USED_ONCE(X)  ((tri->list[X].bits) >> 1 & 1 )
#define IS_USED_TWICE(X)  ((tri->list[X].bits)>> 2 & 1 )

#define AD_ZERO -1

#define ALLOC_VAR 6 /**< The block length to allocate used in alloc_list() */

extern struct task Task;

struct three_variable_list {
    struct node *list;	 /**< Array of entries of the three variable list */
    size_t len;		 /**< The length of list */
    size_t *rhs;
    size_t *initval;
    dim_t *acc_dim;
    dim_t len_acc_dim;
    size_t len_rhs;
    size_t len_D;
};

struct node {
    enum expression_type type;
			     /**< The type of the elemental operation is stored here */
    var3_t left;	 /**< First argument, i.e. refers to \f$a\f$ in \f$g(a,b)\f$.  */
    var3_t rght;	 /**< Second argument, i.e. refers to \f$b\f$ in \f$g(a,b)\f$ or in \f$g(b)\f$. */
    dim_t row;
    dim_t col;
    enum set_R role;
    order_t k;
    dim_t dim;
    Expression value;	 /**< The expression that the node represents is referenced here */
    char bits;
    size_t *D;		 /**< The derivatives of the node. An entry refers to \f$D_i v(x_1,\ldots,x_n)\f$. */
    char *name;		 /**< In case the node represents a postfix expression \f$a\f$ then \f$\operatorname{name}\llbracket a \rrbracket\f$ is stored here. */
};

/**
 *@brief This method instantiates and initializes a three variable list
 *@returns A pointer to the location of the instance.
*/
struct three_variable_list *install_three_variable_list(void)
{
    struct three_variable_list *out = (struct three_variable_list *)
    my_malloc(sizeof(struct three_variable_list));
    out->list = NULL;
    out->len = 0;
    out->initval = NULL;
    out->acc_dim = NULL;
    out->len_acc_dim = 0;
    out->len_rhs = 0;
    out->len_D = 0;
    return out;
}

int is_unused(var3_t i, ConstThreeVariableList tri)
{
    return IS_UNUSED(i);
}

int is_used_twice(var3_t i, ConstThreeVariableList tri)
{
    return IS_USED_TWICE(i);
}

/** @returns \c true if the variable with index \p i equals 0 otherwise \c false
 * @param i The index of the variable
 * @param tri The three variable list
*/

int is_var_zero(var3_t i, ConstThreeVariableList tri)
{
    return i == AD_ZERO;
}

/**
 *@returns The length of the three variable list
 *@param tri The three variable list
*/

size_t get_three_variable_list_len(ConstThreeVariableList tri)
{
    return tri->len;
}

enum expression_type get_type_of_var(var3_t i, ConstThreeVariableList tri)
{
    return tri->list[i].type;
}

var3_t get_left_var(var3_t i, ConstThreeVariableList tri)
{
    return tri->list[i].left;
}

var3_t get_rght_var(var3_t i, ConstThreeVariableList tri)
{
    return tri->list[i].rght;
}

dim_t get_row_of_var(var3_t i, ConstThreeVariableList tri)
{
    return tri->list[i].row;
}

dim_t get_col_of_var(var3_t i, ConstThreeVariableList tri)
{
    return tri->list[i].col;
}

enum set_R get_role_of_var(var3_t i, ConstThreeVariableList tri)
{
    return tri->list[i].role;
}

order_t get_order_of_var(var3_t i, ConstThreeVariableList tri)
{
    return tri->list[i].k;
}

dim_t get_dim_of_var(var3_t i, ConstThreeVariableList tri)
{
    return tri->list[i].dim;
}

Expression get_value_of_var(var3_t i, ConstThreeVariableList tri)
{
    return tri->list[i].value;
}

/** @returns The number of variables in the three variable list
 * @param tri The three variable list
*/
size_t get_number_of_variables(ConstThreeVariableList tri)
{
    size_t i, k = 0;
    for (i = tri->len; i--;)
	if (IS_USED(i)) {
	    k++;
	}
    return k;
}

/** @brief Initializes entries of a list of struct #node
 * @param in The input list
 * @param beg The first entry to initialize
 * @param end The first entry not to initialize
 * 
 *
 * Assumptions: 
 * Entries are allocated.
*/

void init_node(void *in, const size_t beg, const size_t end)
{
    register size_t i;
    struct node *ptr = (struct node *) in;
    for (i = beg; i < end; i++) {
	ptr[i].left = AD_ZERO;
	ptr[i].rght = AD_ZERO;
	ptr[i].D = NULL;
	ptr[i].value = NULL;
	ptr[i].role = ORDINARY_VAR;
	ptr[i].name = NULL;
	ptr[i].row = 0;
	ptr[i].col = 0;
	ptr[i].dim = 0;
	ptr[i].k = 0;
	ptr[i].bits = 0;
    }
}


/** @brief Checks if two nodes are the same by checking the content. 
 * It also uses the commutative property of addition and multiplication.
 * @param op1 Node 1
 * @param op2 Node 2
 * 
 * Suppose the content of both Node1 and Node2 is a + b. Then the function returns \c true
 *
 * Assumptions: 
 * \p op1 and \p op2 are not \c NULL
*/

int are_nodes_equal(const struct node *op1, const struct node *op2)
{
    assert(op1 != NULL && op2 != NULL);
    enum expression_type t;
    if (op1 == op2)
	return true;
    if ((t = TYPE(op1)) == TYPE(op2)) {
	switch (t) {
	case PLUS:
	case TIMES:
	    if ((op1->left == op2->left && op1->rght == op2->rght)
		|| (op1->left == op2->rght && op2->left == op1->rght))
		return true;	/* a + b = b + a and a * b = b * a */
	    break;
	case MINUS:
	case DIV:
	case POW:
	case EXP:
	    if ((op1->left == op2->left && op1->rght == op2->rght))
		return true;
	    break;
	case NUM_INT:
	    if (GET_INT(op1->value) == GET_INT(op2->value))
		return true;
	    break;
	case POSTFIX:
	    if (op1->row == op2->row && op1->col == op2->col && !strcmp(op1->name, op2->name))
		return true;
	    break;
	default:
	    return false;
	    break;
	}
	return false;
    } else
	return false;
}

/** @returns \c true if the variable \p a represents the integer 1 otherwise \c false
 * @param a The variable to check
 * @param tri The three variable list
 * 
 * Assumptions: 
 * \p a is not out of range in \p tri
*/

int is_variable_one(const var3_t a, const struct three_variable_list *tri)
{
    if (a == AD_ZERO)
	return false;
    if ((tri->list)[a].value == NULL)
	return false;
    return ((TYPE((tri->list)[a].value) == NUM_INT
	     && GET_INT((tri->list)[a].value) == 1) ? true : false);
}

/** @returns \c true if it can be proved that the variables \p a and \p b coincide otherwise \c false
 * @param a The first variable
 * @param b The second variable
 * @param tri The three variable list
 * 
 * Remarks: 
 * Function is intensionally not symmetric in \p a and \p b.
 *
 * The following identities are considered:
*/
int is_node_identical_to_node(const var3_t a, const var3_t b, const struct three_variable_list *tri)
{
    struct node *list = tri->list;
    if (list[a].type == PLUS) {
    /** \f$\forall_{a,b \in \mathbb{R}} \ a = 0 + b \Rightarrow a = b \f$ */
    /** \f$\forall_{a,b \in \mathbb{R}} \ a = b + 0 \Rightarrow a = b \f$ */
	if ((list[a].left == AD_ZERO && list[a].rght == b)
	    || (list[a].left == b && list[a].rght == AD_ZERO))
	    return true;
    }
    if (list[a].type == MINUS) {
    /** \f$\forall_{a,b \in \mathbb{R}} \ a = b - 0 \Rightarrow a = b \f$ */
	if ((list[a].rght == AD_ZERO && list[a].left == b))
	    return true;
    }
    if (list[a].type == TIMES) {
     /** \f$\forall_{a,b \in \mathbb{R}} \ a = b \cdot 1 \Rightarrow a = b \f$ */
     /** \f$\forall_{a,b \in \mathbb{R}} \ a = 1 \cdot b \Rightarrow a = b \f$ */ 
     /** \f$\forall_{a,b \in \mathbb{R}_+} \ a = \sqrt{b} \cdot \sqrt{b} \Rightarrow a = b \f$ */ 
	if ((is_variable_one(list[a].rght, tri) && list[a].left == b)
	    || (is_variable_one(list[a].left, tri) && list[a].rght == b))
	    return true;
	if (list[(list[a].left)].type == FCT_SQRT
	    && list[(list[a].rght)].type == FCT_SQRT
	    && list[(list[a].rght)].rght == list[(list[a].left)].rght
	    && list[(list[a].rght)].rght == b) {
	    return true;
     }
    }
    if (list[a].type == DIV) {
     /** \f$\forall_{a,b \in \mathbb{R}} \ a = b / 1 \Rightarrow a = b \f$ */
	if ((is_variable_one(list[a].rght, tri) && list[a].left == b))
	    return true;
    }
    if (is_Expression_type_function(list[a].type)) {
    /** \f$\forall_{a_1,a_2,b_1,b_2 \in \mathbb{R}} \ g = h \wedge a_1 = b_1 \wedge a_2 = b_2  \Rightarrow g(a_1,a_2) = h(b_1,b_2) \f$ */
	if ((list[a].type == list[b].type)
	    && (list[a].rght == list[b].rght)
	    && (list[a].left == list[b].left))
	    return true;
    }
    return false;
}


/** @returns \c true if it can be proved that the variable \p a vanishes otherwise \c false
 * @param a The variable
 * @param tri The three variable list
 * 
 * The following identities are considered:
*/
int is_node_identical_to_zero(const var3_t a, const struct three_variable_list *tri)
{
    struct node *list = tri->list;
    if (a == AD_ZERO)
	return true;
    if (list[a].type == PLUS) {
     /** \f$0 + 0 = 0\f$ */
	if (list[a].left == AD_ZERO && list[a].rght == AD_ZERO)
	    return true;
    }
    if (list[a].type == MINUS) {
     /** \f$\forall_{x \in \mathbb{R}} \ x - x = 0\f$ */
	if (list[a].left == list[a].rght)
	    return true;
    }
    if (list[a].type == TIMES) {
     /** \f$\forall_{x \in \mathbb{R}} \ 0\cdot x = x \cdot 0 = 0\f$ */
	if (is_node_identical_to_zero(list[a].left, tri)
	    || is_node_identical_to_zero(list[a].rght, tri))
	    return true;
    }
    if (list[a].type == DIV) {
     /** \f$\forall_{x \in \mathbb{R}} \ 0/x = 0\f$ */
	if (is_node_identical_to_zero(list[a].left, tri))
	    return true;
    }
    return false;
}

/**
 * @brief This method labels the variables not being used for defining any other variable.
 * @param tri The three variable list
*/

void find_unused_nodes(const struct three_variable_list *tri)
{
    struct node *list = tri->list;
    size_t i, j, k;
    for (i = 0; i < tri->len; i++) {
	if (IS_UNUSED(i) || list[i].role == OUTPUT_VAR || list[i].role == STATE_VAR)
	    continue;
	for (k = 0; k < tri->len_rhs; k++) {
	    if (tri->rhs[k] == i)
		break;
	}
	if (k != tri->len_rhs)
	    continue;
	for (j = 0; j < tri->len; j++) {
	    if (IS_UNUSED(j))
		continue;
	    for (k = 0; k < tri->len_rhs; k++) {
		if (list[j].D != NULL && list[j].D[k] == i)
		    break;
	    }
	    if (k != tri->len_rhs)
		break;
	}
	if (j != tri->len)
	    continue;
	for (j = i + 1; j < tri->len; j++) {
	    if (IS_UNUSED(j))
		continue;
	    if (list[j].rght == i || list[j].left == i)
		break;
	}
	if (j == tri->len) {
	    SET_UNUSED(i);
	}
    }
}

/** @brief This method replaces all indices equal to \p old by the index \p new in the three variable list.
 * Used in find_equal_nodes() and prepare_method_quadratic_in_order().
 *@param new The new index 
 *@param old The index to replace
 *@param tri The three variable list
*/

void replace_nodes(const var3_t new, const var3_t old, const struct three_variable_list *tri)
{
    const var3_t i = new;
    const var3_t j = old;
    struct node *list = tri->list;
    size_t k;
    SET_UNUSED(j);
    list[j].role = ORDINARY_VAR;
    for (k = 0; k < tri->len; k++) {
	if (list[k].left == j) {
	    list[k].left = i;
	}
	if (list[k].rght == j) {
	    list[k].rght = i;
	}
	if (list[k].D != NULL) {
	    dim_t l;
	    for (l = 0; l < tri->len_D; l++) {
		if (list[k].D[l] == j) {
		    list[k].D[l] = i;
		}
	    }
	}
    }
}

/**
 * @brief This method tries to reduce every variable to type ::NUM_INT.
 * @param tri The three variable list
*/

void find_int_nodes(const struct three_variable_list *tri)
{
    struct node *list = tri->list;
    size_t i;
    int a;
    for (i = tri->len; i--;) {
	if (is_Expression_type_elementary_operation(list[i].type)
	    && (list[i].left) != AD_ZERO && (list[i].rght) != AD_ZERO) {
	    if (compute_int_expression
		(&a, list[i].type, list[(list[i].left)].value,
		 list[(list[i].rght)].value, 0) == true) {
		list[i].type = NUM_INT;
		list[i].value = install_Number_int(a);
	    }
	}
    }
}

/**
 * @brief This method tries to find variables that are equal. In case of equality one variable is removed.
 * @param tri The three variable list
*/

int find_equal_nodes(const struct three_variable_list *tri)
{
    struct node *list = tri->list;
    size_t i, j;
    int retval = true;
    if (!IS_QUIET(&Task)) {
    }
    for (i = tri->len; i--;) {
	if (IS_UNUSED(i))
	    continue;
	for (j = i + 1; j < tri->len; j++) {
	    if (IS_UNUSED(j))
		continue;
	    if (list[j].role == OUTPUT_VAR
		|| (list[i].role != STATE_VAR && list[j].role == STATE_VAR)) {
		if (is_node_identical_to_zero(j, tri)) {
		    list[j].type = PLUS;
		    list[j].left = AD_ZERO;
		    list[j].rght = AD_ZERO;
		}
		continue;
	    }
	    if (are_nodes_equal(&list[i], &list[j])
		|| is_node_identical_to_node(j, i, tri)) {
		replace_nodes(i, j, tri);
		retval = false;
	    }
	    if (is_node_identical_to_zero(j, tri)) {
		list[j].type = PLUS;
		list[j].left = AD_ZERO;
		list[j].rght = AD_ZERO;
		replace_nodes(AD_ZERO, j, tri);
		retval = false;
	    }
	}
    }
    if (!IS_QUIET(&Task)) {
    }
    return retval;
}



/**
 * @brief Inserts a new variable in \p tri
 * @param in The expression that causes the generation of a new variable 
 * @param tri The three variable list
 * @returns The name (equiv. index) of the new variable
 * 
 *
*/

var3_t insert_Expression_to_tri(const Expression in, struct three_variable_list * tri)
{
    alloc_list((void **) (&(tri->list)), tri->len, sizeof(struct node), ALLOC_VAR, &init_node);
    NO_MEM(tri->list);
    CURRENT_ENTRY(tri).type = TYPE(in);
    CURRENT_ENTRY(tri).value = in;
    CON_AD(in) = tri->len;
    return tri->len;
}

var3_t
insert_PostFixExpression_to_tri(const Expression in,
				struct three_variable_list * tri, ConstSequenceOfStates S)
{
    assert(TYPE(in) == POSTFIX);
    dim_t row, col;
    get_indices_of_PostFixExpression(in, &row, &col, 0, S);
    var3_t idx = insert_Expression_to_tri(in, tri);
    CURRENT_ENTRY(tri).role = ORDINARY_VAR;
    CURRENT_ENTRY(tri).row = row;
    CURRENT_ENTRY(tri).col = col;
    CURRENT_ENTRY(tri).name = NAME(in);
    return idx;
}

void
insert_state_to_tri(char *name, dim_t row, dim_t col, const dim_t idx,
		    struct three_variable_list *tri)
{
    alloc_list((void **) (&(tri->list)), tri->len, sizeof(struct node), ALLOC_VAR, &init_node);
    NO_MEM(tri->list);
    CURRENT_ENTRY(tri).type = PLUS;
    CURRENT_ENTRY(tri).left = AD_ZERO;
    CURRENT_ENTRY(tri).rght = tri->initval[idx];
    CURRENT_ENTRY(tri).role = STATE_VAR;
    CURRENT_ENTRY(tri).row = row;
    CURRENT_ENTRY(tri).col = col;
    CURRENT_ENTRY(tri).name = name;
    CURRENT_ENTRY(tri).dim = idx + 1;
}

void
insert_Expression_as_state_to_tri(Expression in, const dim_t idx,
				  struct three_variable_list *tri, ConstSequenceOfStates S)
{
    assert(idx > 0);
    assert(TYPE(in) == POSTFIX);
    dim_t row, col;
    get_indices_of_PostFixExpression(in, &row, &col, 0, S);
    size_t abs_idx = (idx == 1 ? row : tri->acc_dim[idx - 2] + row);
    insert_state_to_tri(NAME(in), row, col, abs_idx, tri);
    CURRENT_ENTRY(tri).dim = 0;
}

/**@returns \f$D_i v_\alpha\f$
 * @param a \f$\alpha\f$
 * @param i \f$i\f$
 * @param tri The three variable list
*/
var3_t derivative(var3_t a, dim_t i, const struct three_variable_list *tri)
{
    if (a == AD_ZERO)
	return AD_ZERO;
    else
	return DERIVATIVE(a, i);
}

/**@brief Inserts \f$v_n = k \in \mathbb{Z}\f$ to the three variable list unless there exists \f$m<n\f$ such that \f$v_{m} = k\f$. 
 * @param k \f$k\f$
 * @param tri The three variable list of length \f$n-1\f$
*/
var3_t insert_integer_to_three_variable_list(int k, struct three_variable_list * tri)
{
    size_t i;
    for (i = 0; i < tri->len; i++) {
	if (tri->list[i].type == NUM_INT && GET_INT((tri->list[i].value)) == k) {
	    SET_USED(i);
	    return i;
	}
    }
    insert_Expression_to_tri(install_Number_int(k), tri);
    UPDATE(tri);
    return ((tri->len) - 1);
}



int is_variable_constant(var3_t i, const struct three_variable_list *tri)
{
    if (i == AD_ZERO)
	return true;
    if ((tri->list[i]).role == STATE_VAR)
	return false;
    if ((tri->list[i]).type == NUM_INT || (tri->list[i]).type == NUM_DEC
	|| (tri->list[i]).type == NUM_CONST)
	return true;
    if ((tri->list[i]).type == POSTFIX
	&& ((tri->list[i]).role == ORDINARY_VAR || (tri->list[i]).role == INPUT_VAR))
	return true;
    else if (is_Expression_type_elementary_operation((tri->list[i]).type)) {
	if (tri->list[i].left == AD_ZERO && tri->list[i].rght == AD_ZERO)
	    return true;
	else if (tri->list[i].left == AD_ZERO && !(tri->list[i].rght == AD_ZERO))
	    return is_variable_constant(tri->list[i].rght, tri);
	else if (tri->list[i].rght == AD_ZERO && !(tri->list[i].left == AD_ZERO))
	    return is_variable_constant(tri->list[i].left, tri);
	else if (is_variable_constant(tri->list[i].left, tri)
		 && is_variable_constant(tri->list[i].rght, tri))
	    return true;
    } else if (is_Expression_type_function((tri->list[i]).type)) {
	if (is_variable_constant(tri->list[i].rght, tri))
	    return true;
    }
    return false;
}

/** @brief This function inserts a new entry \f$(g,v_a,v_b)\f$ to the three variable list or returns the index of the variable where the entry to add already resides. 
 *The latter is of course understood in a non-mathematical sense. 
 *Note that 
 *it makes simplifications if elementary algebraic identifies occur, e.g, if zero is added.
 *@param type \f$g\f$
 *@param a Index of first argument of \f$g\f$
 *@param b Index of second argument of \f$g\f$
 *@param tri The three variable list
 *@returns \f$k\f$, where \f$v_k = g(v_a,v_b)\f$
*/

var3_t
insert_elemental_to_three_variable_list(enum expression_type type,
					var3_t a, var3_t b, struct three_variable_list * tri)
{
    struct node *list = tri->list;
    switch (type) {
    case PLUS:
	if (a == AD_ZERO)
	    return b;		/* c = x + 0 */
	if (b == AD_ZERO)
	    return a;		/* c = 0 + x */
	if (list[b].type == MINUS && list[b].left == AD_ZERO)
	    return insert_elemental_to_three_variable_list(MINUS, a, list[b].rght, tri);	/* c = a + ( 0 - b ) = a - b */
	if (list[a].type == MINUS && list[a].left == AD_ZERO)
	    return insert_elemental_to_three_variable_list(MINUS, b, list[a].rght, tri);	/* c = ( 0 - a ) + b = b - a */
	break;
    case MINUS:
	{
	    if (a == b)
		return AD_ZERO;	/* x - x = 0 */
	    if (b == AD_ZERO)
		return a;	/* c = a - 0 */
	    if (a == AD_ZERO && list[b].type == NUM_INT)
		return (insert_integer_to_three_variable_list(-GET_INT(list[b].value), tri));	/*c = 0 - b */
	    if (a == AD_ZERO && list[b].type == MINUS)
		return (insert_elemental_to_three_variable_list(MINUS, list[b].rght, list[b].left, tri));	/* c = 0 - ( aa - bb ) = 0 + (bb - aa) */
	    if (a == AD_ZERO && list[b].type == TIMES && list[list[b].rght].type == MINUS && list[list[b].rght].left == AD_ZERO)	/* c = 0 - ( ba * ( 0 - bbb )) = ba * bbb */
		return (insert_elemental_to_three_variable_list
			(TIMES, list[b].left, list[list[b].rght].rght, tri));
	    if (a == AD_ZERO && list[b].type == TIMES && list[list[b].left].type == MINUS && list[list[b].left].left == AD_ZERO)	/* c = 0 - ( (0 - bab ) * bb ) = bab * bb */
		return (insert_elemental_to_three_variable_list
			(TIMES, list[b].rght, list[list[b].left].rght, tri));
	}
	break;
    case TIMES:
	{
	    if (a == AD_ZERO || b == AD_ZERO)
		return AD_ZERO;	/* 0*x = x*0 = 0 */
	    if (list[b].type == NUM_INT && GET_INT(list[b].value) == 0)
		return AD_ZERO;	/* 0*x = x*0 = 0 */
	    if (list[a].type == NUM_INT && GET_INT(list[a].value) == 0)
		return AD_ZERO;	/* 0*x = x*0 = 0 */
	    if (list[b].type == NUM_INT && GET_INT(list[b].value) == 1)
		return (a);	/*c = a * 1 */
	    if (list[a].type == NUM_INT && GET_INT(list[a].value) == 1)
		return (b);	/*c = 1 * b */
	    if (list[b].type == NUM_INT && GET_INT(list[b].value) == -1)
		return (insert_elemental_to_three_variable_list(MINUS, AD_ZERO, a, tri));	/*c = a * -1 */
	    if (list[a].type == NUM_INT && GET_INT(list[a].value) == -1)
		return (insert_elemental_to_three_variable_list(MINUS, AD_ZERO, b, tri));	/*c = -1 * b */
	    if (list[b].type == DIV && a == list[b].rght) {
		return list[b].left;
	    }			/* c = x * ( y / x ) = y */
	    if (list[a].type == DIV && b == list[a].rght) {
		return list[a].left;
	    }			/* c = ( y / x ) * x = y */
	    if (list[b].type == TIMES && list[list[b].left].type == DIV
		&& a == list[list[b].left].rght)
		return (insert_elemental_to_three_variable_list(TIMES, list[list[b].left].left, list[b].rght, tri));	/* c = x * ( (y/x) * z ) = y*z */
	    if (list[b].type == TIMES && list[list[b].rght].type == DIV
		&& a == list[list[b].rght].rght)
		return (insert_elemental_to_three_variable_list(TIMES, list[list[b].rght].left, list[b].left, tri));	/* c = x * ( z * (y/x) ) = z*y */
	    /* TODO: symmetry! */
	    if (list[a].type == FCT_SQRT && list[b].type == FCT_SQRT
		&& list[a].rght == list[b].rght)
		return list[a].rght;	/* x = sqrt(x) * sqrt(x) */
	}
	break;
    case DIV:
	{
	    assert(b != AD_ZERO);
	    if (a == AD_ZERO)
		return AD_ZERO;
	    if (list[b].type == NUM_INT && GET_INT(list[b].value) == 1)
		return a;
	    if (list[b].type == NUM_INT && GET_INT(list[b].value) < 0) {
		var3_t e = insert_integer_to_three_variable_list(-GET_INT(list[b].value),
								 tri);
		e = insert_elemental_to_three_variable_list(DIV, a, e, tri);
		return (insert_elemental_to_three_variable_list(MINUS, AD_ZERO, e, tri));
	    }
	    if ((list[a].type == EXP && list[list[a].rght].type == NUM_INT)
		|| list[a].type == POW) {
		if (list[a].left == b) {
		    var3_t e =
			insert_integer_to_three_variable_list(GET_INT(list[(list[a]).rght].value)
							      - 1,
							      tri);
		    return (insert_elemental_to_three_variable_list(EXP, b, e, tri));
		}
	    }
	    if (list[a].type == TIMES && list[a].rght == b)
		return list[a].left;	/* c = (aa * b) / b = aa */
	    if (list[a].type == TIMES && list[a].left == b)
		return list[a].rght;	/* c = (ab * aa) / aa = ab */
	    if (list[b].type == TIMES && list[b].left == a) {
		return insert_elemental_to_three_variable_list(DIV, ADD_ONE, list[b].rght, tri);
	    }			/* c = a / (a * bb) = 1 / bb */
	    if (list[b].type == TIMES && list[b].rght == a) {
		return insert_elemental_to_three_variable_list(DIV, ADD_ONE, list[b].left, tri);
	    }			/* c = a / (ba * a) = 1 / ba */
	}
	break;
    case POW:
    case EXP:
	if (b == AD_ZERO)
	    return (ADD_ONE);
	if (tri->list[b].type == NUM_INT && GET_INT(tri->list[b].value) == 1)
	    return a;
	break;
    case FCT_ATAN:
	if (b == AD_ZERO)
	    return AD_ZERO;
	break;
    case FCT_COS:
    case FCT_COSH:
	if (b == AD_ZERO)
	    return (ADD_ONE);
	break;
    case FCT_LOG:
	if (is_variable_one(b, tri))
	    return AD_ZERO;
	break;
    case FCT_SIN:
    case FCT_SINH:
	if (b == AD_ZERO)
	    return AD_ZERO;
	break;
    default:
	assert(0);
	break;
    }
    alloc_list((void **) (&(tri->list)), tri->len, sizeof(struct node), ALLOC_VAR, &init_node);
    NO_MEM(tri->list);
    CURRENT_ENTRY(tri).type = type;
    CURRENT_ENTRY(tri).value = NULL;
    CURRENT_ENTRY(tri).left = a;
    CURRENT_ENTRY(tri).rght = b;
    UPDATE(tri);
    return ((tri->len) - 1);
}



/**
 * @brief This method inserts first derivatives to the three variable list
 * @param dim The dimension of the dependent variable.
 * @param var The entry of the dependent variable to create the derivative
 * @param tri
 * 
 *
 * The following formulas are used :
*/
void
insert_derivative_to_three_variable_list(const dim_t dim,
					 const var3_t var, struct three_variable_list *tri)
{
    register dim_t i;
    var3_t a, b;
    assert(var <= tri->len);
    assert((tri->list[var]).D != NULL);
    struct node v = (tri->list)[var];
    if (is_variable_constant(var, tri)) {
		 /** \f$D_i v = 0\f$ */
	for (i = 0; i < dim; i++) {
	    DERIVATIVE(var, i) = AD_ZERO;
	}
	return;
    }
    assert(v.role != STATE_VAR);
    for (i = 0; i < dim; i++) {
	switch (v.type) {
	case PLUS:
	case MINUS:
			/** \f$D_i(f + g) = D_i f + D_i g\f$ */
			/** \f$D_i(f - g) = D_i f - D_i g\f$ */
	    DERIVATIVE(var, i) =
		insert_elemental_to_three_variable_list(v.type,
							derivative(v.left,
								   i, tri),
							derivative(v.rght, i, tri), tri);
	    break;
	case TIMES:
	    a = insert_elemental_to_three_variable_list(TIMES,
							derivative(v.left, i, tri), v.rght, tri);
	    b = insert_elemental_to_three_variable_list(TIMES,
							derivative(v.rght, i, tri), v.left, tri);
	    DERIVATIVE(var, i) = insert_elemental_to_three_variable_list(PLUS, a, b, tri);
	    break;
	case DIV:
			/** \f$D_i v = D_i(f/g) = (D_i f)/g - \frac{f}{g^2} \cdot D_i g = (D_i f)/g - v/g \cdot D_i g\f$ */
	    a = insert_elemental_to_three_variable_list(DIV, var, v.rght, tri);
	    b = insert_elemental_to_three_variable_list(TIMES, a, derivative(v.rght, i, tri), tri);
	    a = insert_elemental_to_three_variable_list(DIV,
							derivative(v.left, i, tri), v.rght, tri);
	    DERIVATIVE(var, i) = insert_elemental_to_three_variable_list(MINUS, a, b, tri);
	    break;
	case POW:
	case EXP:
			/** \f$D_i(f^g) = f^{g-1} \cdot ( g\cdot D_i f + f \cdot \log(f) \cdot D_i g ) \f$ */
	    if (derivative(v.rght, i, tri) == AD_ZERO) {
		a = AD_ZERO;
	    } else {
		a = insert_elemental_to_three_variable_list(FCT_LOG, AD_ZERO, v.left, tri);
		b = insert_elemental_to_three_variable_list(TIMES, v.left, a, tri);
		a = insert_elemental_to_three_variable_list(TIMES,
							    derivative(v.rght, i, tri), b, tri);
	    }
	    b = insert_elemental_to_three_variable_list(TIMES, v.rght,
							derivative(v.left, i, tri), tri);
	    a = insert_elemental_to_three_variable_list(PLUS, a, b, tri);
	    b = insert_elemental_to_three_variable_list(DIV, var, v.left, tri);
	    DERIVATIVE(var, i) = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
	    break;
	case FCT_EXP:
			/** \f$D_i \exp(v) = D_i v \cdot \exp(v) \f$ */
	    DERIVATIVE(var, i) =
		insert_elemental_to_three_variable_list(TIMES,
							derivative(v.rght, i, tri), var, tri);
	    break;
	case FCT_LOG:
	    DERIVATIVE(var, i) =
		insert_elemental_to_three_variable_list(DIV,
							derivative(v.rght, i, tri), v.rght, tri);
	    break;
	case FCT_ATAN:
			/** \f$D_i(\operatorname{atan}(v)) = D_i v / ( 1 + v \cdot v ) \f$ */
	    a = insert_elemental_to_three_variable_list(TIMES, v.rght, v.rght, tri);
	    b = insert_elemental_to_three_variable_list(PLUS, ADD_ONE, a, tri);
	    DERIVATIVE(var, i) =
		insert_elemental_to_three_variable_list(DIV, derivative(v.rght, i, tri), b, tri);
	    break;
	case FCT_COS:
			/** \f$D_i(\cos(v)) = - D_i v \cdot \sin(v) \f$ */
	    a = insert_elemental_to_three_variable_list(FCT_SIN, AD_ZERO, v.rght, tri);
	    b = insert_elemental_to_three_variable_list(TIMES, a, derivative(v.rght, i, tri), tri);
	    DERIVATIVE(var, i) = insert_elemental_to_three_variable_list(MINUS, AD_ZERO, b, tri);
	    break;
	case FCT_COSH:
			/** \f$D_i(\cosh(v)) = D_i v \cdot \sinh(v) \f$ */
	    a = insert_elemental_to_three_variable_list(FCT_SINH, AD_ZERO, v.rght, tri);
	    DERIVATIVE(var, i) =
		insert_elemental_to_three_variable_list(TIMES, a, derivative(v.rght, i, tri), tri);
	    break;
	case FCT_SIN:
			/** \f$D_i(\sin(v)) = D_i v \cdot \cos(v) \f$ */
	    a = insert_elemental_to_three_variable_list(FCT_COS, AD_ZERO, v.rght, tri);
	    DERIVATIVE(var, i) =
		insert_elemental_to_three_variable_list(TIMES, a, derivative(v.rght, i, tri), tri);
	    break;
	case FCT_SINH:
			/** \f$D_i(\sinh(v)) = D_i v \cdot \cosh(v) \f$ */
	    a = insert_elemental_to_three_variable_list(FCT_COSH, AD_ZERO, v.rght, tri);
	    DERIVATIVE(var, i) =
		insert_elemental_to_three_variable_list(TIMES, a, derivative(v.rght, i, tri), tri);
	    break;
	case FCT_SQRT:
			/** \f$D_i \sqrt(v) = D_i 1/(2v)\f$ */
	    a = insert_elemental_to_three_variable_list(TIMES, ADD_TWO, var, tri);
	    DERIVATIVE(var, i) =
		insert_elemental_to_three_variable_list(DIV, derivative(v.rght, i, tri), a, tri);
	    break;
	case FCT_TAN:
			/** \f$D_i \tan(v) = D_i v / \cos^2(v) \f$ */
	    a = insert_elemental_to_three_variable_list(FCT_COS, AD_ZERO, v.rght, tri);
	    b = insert_elemental_to_three_variable_list(POW, a, ADD_TWO, tri);
	    DERIVATIVE(var, i) = insert_elemental_to_three_variable_list(DIV, derivative(v.rght, i, tri), b, tri);
	    break;
	case POSTFIX:
	    DERIVATIVE(var, i) = derivative(v.rght, i, tri);
	    break;
	default:
	    assert(0);
	    break;
	}
    }
}


/**
 * @brief Inserts a variable that is an output, i.e., a component of a Taylor coefficient
 * @param a The variable to be inserted and marked as output
 * @param k The order of the Taylor coefficient
 * @param tri The three variable list
 * 
*/
void
insert_to_three_variable_list_output(const var3_t a, const order_t k,
				     dim_t dim, struct three_variable_list *tri)
{
    if (a != AD_ZERO && tri->list[a].role == ORDINARY_VAR) {
	tri->list[a].role = OUTPUT_VAR;
	tri->list[a].k = k;
	tri->list[a].dim = dim;
    } else {
	alloc_list((void **) (&(tri->list)), tri->len, sizeof(struct node), ALLOC_VAR, &init_node);
     NO_MEM(tri->list);
	CURRENT_ENTRY(tri).type = PLUS;
	CURRENT_ENTRY(tri).left = AD_ZERO;
	CURRENT_ENTRY(tri).rght = a;
	CURRENT_ENTRY(tri).k = k;
	CURRENT_ENTRY(tri).dim = dim;
	CURRENT_ENTRY(tri).role = OUTPUT_VAR;
	UPDATE(tri);
    }
}

/**@brief This method creates a node in the three variable list from an expression
 *@param tri The three variable list
 *@param S The sequence of states. 
*/

void Expression_to_Node(Expression in, struct three_variable_list *tri, ConstSequenceOfStates S)
{
    if (is_Expression_literal_Number(in)) {
	insert_Expression_to_tri(in, tri);
	UPDATE(tri);
	return;
    } else if (is_Expression_elementary_operation(in)) {
	if (CON_AD(in->left) == AD_ZERO)
	    Expression_to_Node(in->left, tri, S);
	if (CON_AD(in->right) == AD_ZERO)
	    Expression_to_Node(in->right, tri, S);
	if (CON_AD(in->left) != AD_ZERO && CON_AD(in->right) != AD_ZERO) {
	    insert_Expression_to_tri(in, tri);
	    if (TYPE(in) == TIMES && TYPE(in->left) == NUM_INT && GET_INT(in->left) == AD_ZERO) {
		CURRENT_ENTRY(tri).type = MINUS;
		CURRENT_ENTRY(tri).left = AD_ZERO;
	    } else {
		CURRENT_ENTRY(tri).left = CON_AD(in->left);
	    }
	    CURRENT_ENTRY(tri).rght = CON_AD(in->right);
	    UPDATE(tri);
	    return;
	} else {
	    assert(0);
	}
	assert(CON_AD(in->left) != AD_ZERO);
	assert(CON_AD(in->right) != AD_ZERO);
    } else if (TYPE(in) == POSTFIX) {
	dim_t row, col;
	get_indices_of_PostFixExpression(in, &row, &col, 0, S);
	enum set_R t = role_of(in, S);
	assert(t != STATE_VAR && t != OUTPUTSTATE_VAR);
	if (t == INPUT_VAR && (get_date_of_postfix(in, S) <= get_date_of_rhs(S))) {
	    if (((find_in_variables_match_role_idx_at_date
		  (OUTPUTSTATE_VAR, idx_of(in, S), get_date(S), S)) != NULL)
		||
		((find_in_variables_match_role_idx_at_date
		  (STATE_VAR, idx_of(in, S), get_date(S), S)) != NULL)) {
		t = STATE_VAR;
		insert_Expression_as_state_to_tri(in, idx_of(in, S), tri, S);
		CON_AD(in) = tri->len;
	    } else {
		insert_PostFixExpression_to_tri(in, tri, S);
		CURRENT_ENTRY(tri).role = t;
	    }
	    UPDATE(tri);
	    return;
	} else if (t == INPUT_VAR && (get_date_of_postfix(in, S) > get_date_of_rhs(S))) {
	    insert_PostFixExpression_to_tri(in, tri, S);
	    CURRENT_ENTRY(tri).role = t;
	    UPDATE(tri);
	    return;
	} else {
	    enum expression_type t = TYPE(definition_of(in, S));
	    if (t == NUM_DEC || t == NUM_CONST || t == CONT_INTV
		|| t == DISC_INTV || interval_of(in, S) != NULL) {
		insert_PostFixExpression_to_tri(in, tri, S);
		UPDATE(tri);
		return;
	    } else {
		Expression_to_Node(definition_of(in, S), tri, S);
		CON_AD(in) = tri->len - 1;
	    }
	}
    } else if (is_Expression_type_function(TYPE(in))) {
	switch (TYPE(in)) {
	case FCT_ATAN:
	case FCT_SIN:
	case FCT_SINH:
	case FCT_COS:
	case FCT_COSH:
	case FCT_EXP:
	case FCT_LOG:
	case FCT_SQRT:
	case FCT_TAN:
	    if (CON_AD(in->right) == AD_ZERO) {
		Expression_to_Node(in->right, tri, S);
	    }
	    insert_Expression_to_tri(in, tri);
	    CURRENT_ENTRY(tri).rght = CON_AD(in->right);
	    UPDATE(tri);
	    break;
	default:
	    assert(0);
	    break;
	}
    } else {
	assert(0);
    }
}

/** @brief This method computes the evaluation trace of the function presented by the program
 * @param tri The three variable list
 * @param S The final state of the program
*/
void evaluation_trace(struct three_variable_list *tri, ConstSequenceOfStates S)
{
    size_t i, j, k;
    alloc_list((void **) (&(tri->list)), tri->len, sizeof(struct node), ALLOC_VAR, &init_node);
    struct variable *ptr;
    size_t dim = 0;
    for (i = 0; i < get_states_len(S); i++) {
	ptr = get_var(i, S);
	ptr = version_of_variable(ptr, 0, 0, get_date_of_rhs(S));
	if (ptr == NULL)
	    continue;
	if (var_role_of(ptr) == OUTPUT_VAR) {
	    for (k = 0; k < col_dim_of(ptr); k++) {
		//assert( col_dim_of(ptr) == 1);
		for (j = 0; j < row_dim_of(ptr); j++) {
		    dim++;	/* .dim starts at 1 */
		    Expression_to_Node(data_of(ptr, j, k), tri, S);
		    insert_to_three_variable_list_output((tri->len) - 1, 1, dim, tri);
		}
	    }
	}
    }
    find_int_nodes(tri);
    while (!find_equal_nodes(tri)) {
    }
}


/** @brief This method computes the evaluation trace of the function presented by the program taking into account the specified differential equation
 * @param tri The three variable list
 * @param S The final state of the program
*/
void
evaluation_trace_for_ode(struct variable *const *states_list,
			 dim_t len, struct three_variable_list *tri, ConstSequenceOfStates S)
{
    size_t i, j;
    dim_t m = 0;
    alloc_list((void **) (&(tri->list)), tri->len, sizeof(struct node), ALLOC_VAR, &init_node);
    struct variable *ptr;

    for (i = 0; i < len; i++) {
	ptr = states_list[i];
	for (j = 0; j < row_dim_of(ptr); j++) {
	    assert(col_dim_of(ptr) == 1);
	    Expression_to_Node(data_of(ptr, j, 0), tri, S);
	    tri->initval[m] = tri->len - 1;
	    m++;
	    insert_state_to_tri(name_of(ptr), j, 0, m - 1, tri);	/* .dim starts at 1 */
	    UPDATE(tri);
	}
    }
    size_t len_org = tri->len;
    for (i = 0; i < tri->len; i++) {
	tri->list[i].k = 0;
    }
    evaluation_trace(tri, S);
    m = 0;
    for (i = 0; i < tri->len; i++) {
	if ((tri->list)[i].role == OUTPUT_VAR) {
	    tri->rhs[m] = i;
	    m++;
	}
    }
    for (i = len_org; i < tri->len; i++) {
	tri->list[i].k = 1;
    }
    if (!IS_NOCLEAN(&Task)) {
	while (!find_equal_nodes(tri)) {
	}
	find_unused_nodes(tri);
    }
}

void make_taylor_coefficients(struct three_variable_list *tri)
{
    var3_t a, b;
    size_t i, j, v, n = tri->len_rhs;
    dim_t k_to_do = 0;
    struct node *list = (tri->list);
    size_t len_org = tri->len;
    for (i = 0; i < len_org; i++) {
	if (list[i].role == OUTPUT_VAR) {
	    k_to_do = ((k_to_do < list[i].k) ? list[i].k : k_to_do);
	}
	if (IS_USED(i) && list[i].D == NULL) {
	    list[i].D = my_malloc(n * sizeof(var3_t));
	    if (list[i].role == STATE_VAR) {
		for (j = 0; j < n; j++) {
		    DERIVATIVE(i, j) = -1;
		}
		DERIVATIVE(i, list[i].row) = insert_integer_to_three_variable_list(1, tri);
	    } else {
		insert_derivative_to_three_variable_list(n, i, tri);
	    }
	}
	list = (tri->list);
    }
    size_t *current_out = my_malloc(tri->len_rhs * sizeof(size_t));
    j = 0;
    for (i = 0; i < len_org; i++) {
	if (list[i].role == OUTPUT_VAR && list[i].k == k_to_do) {
	    current_out[j] = i;
	    j++;
	}
    }
    assert(j == tri->len_rhs);
    k_to_do++;
    for (j = 0; j < tri->len_rhs; j++) {
	b = -1;
	v = current_out[j];
	for (i = 0; i < tri->len_rhs; i++) {
	    a = insert_elemental_to_three_variable_list(TIMES,
							derivative(v, i, tri), tri->rhs[i], tri);
	    b = insert_elemental_to_three_variable_list(PLUS, b, a, tri);
	}
	b = insert_elemental_to_three_variable_list(DIV, b,
						    insert_integer_to_three_variable_list
						    (k_to_do, tri), tri);
	insert_to_three_variable_list_output(b, k_to_do, j + 1, tri);
    }
    list = (tri->list);
    for (i = len_org + 1; i < tri->len; i++) {
	list[i].k = k_to_do;
    }
    find_int_nodes(tri);
    if (!IS_NOCLEAN(&Task)) {
	while (!find_equal_nodes(tri)) {
	}
	find_unused_nodes(tri);
    }
    free(current_out);
}

/** @brief This method creates the matrix \f$ ( D_j f_i(x) )_{i,j}\f$ for a function \f$f\f$.
 * @param tri The three variable list
*/

void make_Jacobian(struct three_variable_list *tri)
{
    size_t i, j, v, n = tri->len_rhs;
    dim_t k_to_do = 0;
    struct node *list = (tri->list);
    size_t len_org = tri->len;
    for (i = 0; i < len_org; i++) {
	if (list[i].role == OUTPUT_VAR) {
	    k_to_do = ((k_to_do < list[i].k) ? list[i].k : k_to_do);
	}
	if (IS_USED(i) && list[i].D == NULL) {
	    list[i].D = my_malloc(n * sizeof(var3_t));
	    if (list[i].role == STATE_VAR) {
		for (j = 0; j < n; j++) {
		    DERIVATIVE(i, j) = -1;
		}
		DERIVATIVE(i, list[i].row) = insert_integer_to_three_variable_list(1, tri);
	    } else {
		insert_derivative_to_three_variable_list(n, i, tri);
	    }
	}
	list = (tri->list);
    }
    size_t *current_out = my_malloc(tri->len_rhs * sizeof(size_t));
    j = 0;
    for (i = 0; i < len_org; i++) {
	if (list[i].role == OUTPUT_VAR && list[i].k == k_to_do) {
	    current_out[j] = i;
	    j++;
	    list[i].role = ORDINARY_VAR;
	}
    }
    assert(j == tri->len_rhs);
    k_to_do++;
    for (j = 0; j < tri->len_rhs; j++) {
	v = current_out[j];
	for (i = 0; i < tri->len_rhs; i++) {
	    insert_to_three_variable_list_output(derivative(v, i, tri), i, j + 1, tri);
	}
    }
    find_int_nodes(tri);
    if (!IS_NOCLEAN(&Task)) {
	while (!find_equal_nodes(tri)) {
	}
	find_unused_nodes(tri);
    }
    free(current_out);
    clean_three_variable_list(tri);
}

/**
 *@brief This method copies the content of \p src to \p dest.
 *
 *Assumptions: \p src is initialized, \p dest != NULL
*/

void copy_nodes(struct node *dest, const struct node *src)
{
    dest->type = src->type;
    dest->left = src->left;
    dest->rght = src->rght;
    dest->D = src->D;
    dest->name = src->name;
    dest->row = src->row;
    dest->col = src->col;
    dest->role = src->role;
    dest->k = src->k;
    dest->dim = src->dim;
    dest->value = src->value;
    dest->bits = src->bits;
}

/** @brief This methods swaps the nodes \p i and \p j in the array \p tri. Used in clean_three_variable_list().
 *@param i Index of first node
 *@param j Index of second node
 *@param tri The three variable list
*/

void swap_nodes(var3_t i, var3_t j, struct three_variable_list *tri)
{
    struct node *list = (tri->list);
    struct node tmp;
    copy_nodes(&tmp, list + i);
    copy_nodes(list + i, list + j);
    copy_nodes(list + j, &tmp);
    size_t k;
    for (k = 0; k < tri->len; k++) {
	if (list[k].left == j) {
	    list[k].left = i;
	} else if (list[k].left == i) {
	    list[k].left = j;
	}
	if (list[k].rght == j) {
	    list[k].rght = i;
	} else if (list[k].rght == i) {
	    list[k].rght = j;
	}
	if (list[k].D != NULL) {
	    dim_t l;
	    for (l = 0; l < tri->len_D; l++) {
		if (list[k].D[l] == j) {
		    list[k].D[l] = i;
		} else if (list[k].D[l] == i) {
		    list[k].D[l] = j;
		}
	    }
	}
    }
}

/** @brief This method removes redundancies in the three variable list, e.g., removes 
 * variables that are not used for representing the function / coefficients.
 *@param tri The three variable list
*/

void clean_three_variable_list(struct three_variable_list *tri)
{
    size_t i, j;
    struct node *list = (tri->list);
    for (i = 0; i < tri->len; i++) {
	if (list[i].role == INPUT_VAR && IS_GAPPA(&Task))
	    continue;
	if (IS_UNUSED(i) || list[i].role == STATE_VAR || list[i].role == OUTPUT_VAR)
	    continue;		/* These variables are definitely needed */
	assert((list[i].left == AD_ZERO
		|| list[i].type == FCT_SIN
		|| list[i].type == FCT_COS
		|| list[i].type == FCT_SINH
		|| list[i].type == FCT_COSH 
          || list[i].type == FCT_ATAN
          || list[i].type == FCT_TAN) ? 1 : (i > list[i].left));
	assert(list[i].rght == AD_ZERO ? 1 : (i > list[i].rght));

	if (is_Expression_type_elementary_operation(list[i].type)
	    && list[i].left != AD_ZERO && !is_Expression_type_number(list[list[i].left].type)) {
	    if (!IS_USED(list[i].left)) {
		assert(0);
	    }
	}
	if (is_Expression_type_number(list[i].type) && !IS_KEEPDEC(&Task)) {
	    SET_UNUSED(i);
	    continue;
	}
	for (j = i + 1; j < tri->len; j++) {
	    if (IS_UNUSED(j))
		continue;
	    if (list[j].rght == i || list[j].left == i)
		break;
	}
	if (j == tri->len && !IS_USED_TWICE(i)) {
	    SET_UNUSED(i);
	}
    }
    if (!IS_TRI(&Task)) {
	for (i = 0; i < tri->len; i++) {
	    if (list[i].role == STATE_VAR || list[i].role == OUTPUT_VAR) {
		SET_USED_TWICE(i);
	    }
	    if (!IS_UNUSED(i)) {
		if (list[i].left != AD_ZERO && IS_USED(list[i].left)
		    && IS_USED_ONCE(list[i].left) == 0)
		    SET_USED_ONCE(list[i].left);
		else if (list[i].left != AD_ZERO && IS_USED(list[i].left)
			 && IS_USED_ONCE(list[i].left) == 1)
		    SET_USED_TWICE(list[i].left);
		if (list[i].rght != AD_ZERO && IS_USED(list[i].rght)
		    && IS_USED_ONCE(list[i].rght) == 0)
		    SET_USED_ONCE(list[i].rght);
		else if (list[i].rght != AD_ZERO && IS_USED(list[i].rght)
			 && IS_USED_ONCE(list[i].rght) == 1)
		    SET_USED_TWICE(list[i].rght);
	    }
	}
	for (i = 0; i < tri->len; i++) {
	    if (IS_UNUSED(i))
		continue;
	    if (!IS_USED_TWICE(i))
		SET_UNUSED(i);
	}
    } else {
	for (i = 0; i < tri->len; i++) {
	    if (IS_USED(i)) {
		SET_USED_TWICE(i);
	    }
	}
    }

    for (i = 0; i < tri->len; i++) {
	if (IS_UNUSED(i)) {
	    j = i + 1;
	    while (j < tri->len && IS_UNUSED(j)) {
		j++;
	    }
	    if (j < tri->len) {
		swap_nodes(i, j, tri);
	    }
	}
    }
    if (IS_VERBOSE(&Task))
	printf("Number of variables: %ld\n", get_number_of_variables(tri));
}

/**@brief This function inserts the formula \f$v = b^{-1} \Rightarrow D^{[k]}b^{-1} = (-\sum_{j=0}^{k-1} D^{[k-j]}b \cdot D^{[j]}v)/b \f$ 
 * to the three variable list 
*/

var3_t insert_derivative_inv(dim_t k, var3_t i, struct three_variable_list *tri)
{
    var3_t a, b, c, d;
    register dim_t j;
    d = AD_ZERO;
    for (j = 0; j <= k - 1; j++) {
	a = derivative(i, j, tri);
	b = derivative((tri->list)[i].left, k - j, tri);
	c = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
	d = insert_elemental_to_three_variable_list(MINUS, d, c, tri);
    }
    b = derivative((tri->list)[i].left, 0, tri);
    return (insert_elemental_to_three_variable_list(DIV, d, b, tri));
}

void
init_three_variable_list_for_ode(dim_t dimension_of_ode,
				 dim_t * acc_dim_of_output_indices,
				 dim_t num_of_output_indices, struct three_variable_list *tri)
{
    tri->rhs = my_malloc(dimension_of_ode * sizeof(var3_t));
    tri->initval = my_malloc(dimension_of_ode * sizeof(var3_t));
    tri->acc_dim = acc_dim_of_output_indices;
    tri->len_acc_dim = num_of_output_indices;
    tri->len_rhs = dimension_of_ode;
    tri->len_D = dimension_of_ode;
}

struct three_variable_list *copy_evaluation_trace(const struct
						  three_variable_list *tri)
{
    size_t i;
    struct three_variable_list *out = my_malloc(sizeof(struct three_variable_list));
    out->len = 0;
    out->initval = NULL;
    init_three_variable_list_for_ode(tri->len_D, tri->acc_dim, tri->len_acc_dim, out);
    for (i = 0; i < tri->len_rhs; i++) {
	out->rhs[i] = tri->rhs[i];
	out->initval[i] = tri->initval[i];
    }
    for (i = 0; i < tri->len; i++) {
	alloc_list((void **) (&(out->list)), out->len, sizeof(struct node), ALLOC_VAR, &init_node);
	assert(tri->list[i].D == NULL);
	copy_nodes((out->list) + i, (tri->list) + i);
    UPDATE(out)}
    return out;
}

/**@brief This method expands the evaluation tree of a function by adding companion functions as below.
 * It also prepare the data struction for subsequent differentiation
 *@param tri The three variable list
*/

size_t prepare_method_quadratic_in_order(dim_t order, struct three_variable_list * tri)
{
    size_t len_org = tri->len;
    dim_t i, j;
    var3_t a, b, c;
    tri->len_D = (order + 1);
    for (i = 0; i < len_org; i++) {
	if ((tri->list)[i].type == FCT_TAN) {
        /**The companion function of the tangent is \f$\operatorname{sec^2}\f$, where \f$\operatorname{sec} := 1/\cos\f$ */
         a = insert_elemental_to_three_variable_list(FCT_COS, AD_ZERO, (tri->list)[i].rght, tri);
         b = insert_elemental_to_three_variable_list(POW, a, ADD_TWO, tri);
         c = insert_elemental_to_three_variable_list(DIV, ADD_ONE, b, tri);
         (tri->list)[i].left = c;
         SET_USED_TWICE(c);
         SET_USED(c);
	}
	if ((tri->list)[i].type == FCT_ATAN) {
        /**The companion function of the arc tangent is \f$x \mapsto 1 + x^2\f$ */
	    c = ADD_ONE;
	    a = insert_elemental_to_three_variable_list(TIMES,
							(tri->list)[i].rght,
							(tri->list)[i].rght, tri);
	    b = insert_elemental_to_three_variable_list(PLUS, a, c, tri);
	    (tri->list)[i].left = b;
	    SET_USED_TWICE(b);
	    SET_USED(b);
	}
    }
    len_org = tri->len;
    for (i = 0; i < len_org; i++) {
	(tri->list)[i].D = my_malloc((order + 1) * sizeof(var3_t));
	DERIVATIVE(i, 0) = i;
	for (j = 1; j < order + 1; j++)
	    (tri->list)[i].D[j] = AD_ZERO;
	if ((tri->list)[i].role == OUTPUT_VAR) {
	    DERIVATIVE(tri->initval[(tri->list)[i].dim - 1] + 1, 1) = i;
	}
	if (IS_UNUSED(i))
	    continue;
	enum expression_type type = get_type_of_var(i, tri);
	if (type == FCT_SIN || type == FCT_SINH || type == FCT_COS || type == FCT_COSH) {
        /**The companion function of \f$\sin\f$ is \f$\cos\f$ */
        /**The companion function of the \f$\cos\f$ is \f$\sin\f$ */
        /**The companion function of the \f$\sinh\f$ is \f$\cosh\f$ */
        /**The companion function of the \f$\cosh\f$ is \f$\sinh\f$ */
	    enum expression_type sincos;
	    if (type == FCT_SIN)
		sincos = FCT_COS;
	    else if (type == FCT_COS)
		sincos = FCT_SIN;
	    else if (type == FCT_SINH)
		sincos = FCT_COSH;
	    else if (type == FCT_COSH)
		sincos = FCT_SINH;
	    else
		assert(0);
	    for (j = i; j < len_org; j++) {
		if ((tri->list)[j].type == sincos && (tri->list)[j].rght == (tri->list)[i].rght) {
		    a = j;
		    (tri->list)[j].left = i;
		    (tri->list)[i].left = a;
		    SET_USED_TWICE(a);
		    SET_USED(a);
		    break;
		}
	    }
	    if (j == len_org) {
		a = insert_elemental_to_three_variable_list(sincos, i, (tri->list)[i].rght, tri);
		(tri->list)[a].D = my_malloc((order + 1) * sizeof(var3_t));
		DERIVATIVE(a, 0) = a;
		for (j = 1; j < order + 1; j++)
		    (tri->list)[a].D[j] = AD_ZERO;
		(tri->list)[i].left = a;
		SET_USED_TWICE(a);	// neu um bei der for-Variante diesen Knoten zu erhalten.
		SET_USED(a);
	    }
	}
	if ((tri->list)[i].type == FCT_TAN) {
	}
    }
    return tri->len;
}

/**@brief The method for performing ad up to \p order with quadratic run time
 * @param order The order
 * @param tri The three variable list 
 * 
 * 
 * The following formulas are used ( \f$D^{[k]}a := D^{i}a / k! \f$ ) :
*/

void method_quardatic_in_order(dim_t order, struct three_variable_list *tri)
{
    dim_t i, k, j;
    var3_t a, b, c, d, e;
    size_t len_org = prepare_method_quadratic_in_order(order, tri);
    for (k = 1; k < order; k++) {
	if (!IS_QUIET(&Task)) {
	    printf("\rMaking order %d", k + 1);
	    fflush(stdout);
	}
	for (i = 0; i < len_org; i++) {
	    if (IS_UNUSED(i))
		continue;
	    if ((tri->list)[i].role == STATE_VAR || (tri->list)[i].role == INPUT_VAR) {
	    } else {
		switch ((tri->list)[i].type) {
		case PLUS:
		case MINUS:
              /** \f$D^{[k]}( a \pm b ) = D^{[k]}a \pm D^{[k]}b \f$ */
		    a = derivative((tri->list)[i].left, k, tri);
		    b = derivative((tri->list)[i].rght, k, tri);
		    DERIVATIVE(i, k) = insert_elemental_to_three_variable_list((tri->list)
									       [i].type, a, b, tri);
		    break;
		case TIMES:
              /** \f$D^{[k]}( a \cdot b ) = \sum_{j=0}^k D^{[k-j]}a \cdot D^{[j]}b \f$ */
		    a = AD_ZERO;
		    for (j = 0; j <= k; j++) {
			b = derivative((tri->list)[i].left, k - j, tri);
			c = derivative((tri->list)[i].rght, j, tri);
			d = insert_elemental_to_three_variable_list(TIMES, b, c, tri);
			a = insert_elemental_to_three_variable_list(PLUS, a, d, tri);
		    }
		    DERIVATIVE(i, k) = a;
		    break;
		case DIV:
              /** \f$v = a / b \Rightarrow D^{[k]}v = (D^{[k]} a -  \sum_{j=1}^k D^{[k-j]}v \cdot D^{[j]}b)/b \f$ */
		    b = AD_ZERO;
		    for (j = 1; j <= k; j++) {
			a = derivative(i, k - j, tri);
			c = derivative((tri->list)[i].rght, j, tri);
			d = insert_elemental_to_three_variable_list(TIMES, c, a, tri);
			b = insert_elemental_to_three_variable_list(MINUS, b, d, tri);
		    }
		    d = b;
		    b = derivative((tri->list)[i].left, k, tri);
		    d = insert_elemental_to_three_variable_list(PLUS, b, d, tri);
		    c = derivative((tri->list)[i].rght, 0, tri);
		    DERIVATIVE(i, k) = insert_elemental_to_three_variable_list(DIV, d, c, tri);
		    break;
		case POW:
              /** \f$v = b^{-1} \Rightarrow D^{[k]}b^{-1} = (-\sum_{j=0}^{k-1} D^{[k-j]}b \cdot D^{[j]}v)/b \f$ */
		    a = (tri->list)[i].rght;
		    int l = GET_INT((tri->list)[a].value);
		    if (l == -1)
			DERIVATIVE(i, k) = insert_derivative_inv(k, i, tri);
		    else if (l == 2) {	/* x^2 = x * x -> rhs is cheaper! */
			a = AD_ZERO;
			for (j = 0; j < (k + 1) / 2; j++) {
			    b = derivative((tri->list)[i].left, k - j, tri);
			    c = derivative((tri->list)[i].left, j, tri);
			    d = insert_elemental_to_three_variable_list(TIMES, b, c, tri);
			    a = insert_elemental_to_three_variable_list(PLUS, a, d, tri);
			}
			a = insert_elemental_to_three_variable_list(PLUS, a, a, tri);
			if (k % 2 == 0) {
			    b = derivative((tri->list)[i].left, k / 2, tri);
			    d = insert_elemental_to_three_variable_list(TIMES, b, b, tri);
			    a = insert_elemental_to_three_variable_list(PLUS, a, d, tri);
			}
			DERIVATIVE(i, k) = a;
		    } else {
               /** \f$v = a^l \Rightarrow D^{[k]}b^{-1} = (\sum_{j=0}^{k-1} (k\cdot l-j\cdot(l+1))\cdot D^{[k-j]}a \cdot D^{[j]}v)/(k\cdot a) \f$ */
			d = AD_ZERO;
			for (j = 0; j <= k - 1; j++) {
			    a = derivative(i, j, tri);
			    b = derivative((tri->list)[i].left, k - j, tri);
			    c = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
			    e = insert_integer_to_three_variable_list(k * l - j * (l + 1), tri);
			    c = insert_elemental_to_three_variable_list(TIMES, e, c, tri);
			    d = insert_elemental_to_three_variable_list(PLUS, d, c, tri);
			}
			e = insert_integer_to_three_variable_list(k, tri);
			b = derivative((tri->list)[i].left, 0, tri);
			e = insert_elemental_to_three_variable_list(TIMES, e, b, tri);
			DERIVATIVE(i, k) = insert_elemental_to_three_variable_list(DIV, d, e, tri);
		    }
		    break;
		case EXP:
		    assert(0);
		case FCT_ATAN:
		    {
			d = AD_ZERO;
			for (j = 1; j <= k - 1; j++) {
			    a = derivative(i, j, tri);
			    b = derivative((tri->list)[i].left, k - j, tri);
			    c = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
			    e = insert_integer_to_three_variable_list(j, tri);
			    c = insert_elemental_to_three_variable_list(TIMES, e, c, tri);
			    d = insert_elemental_to_three_variable_list(PLUS, d, c, tri);
			}
			e = insert_integer_to_three_variable_list(k, tri);
			d = insert_elemental_to_three_variable_list(DIV, d, e, tri);
			c = derivative((tri->list)[i].rght, k, tri);
			a = insert_elemental_to_three_variable_list(MINUS, c, d, tri);
			c = derivative((tri->list)[i].left, 0, tri);
			DERIVATIVE(i, k) = insert_elemental_to_three_variable_list(DIV, a, c, tri);
		    }
		    break;
		case FCT_SIN:
		    {
			var3_t le = (tri->list)[i].left;
			var3_t ri = (tri->list)[i].rght;
			if (le < i)
			    break;
			d = AD_ZERO;
			var3_t d0 = AD_ZERO;
			for (j = 1; j <= k; j++) {
			    a = derivative(le, k - j, tri);
			    c = derivative(ri, j, tri);
			    e = insert_integer_to_three_variable_list(j, tri);
			    b = insert_elemental_to_three_variable_list(TIMES, e, c, tri);
			    e = insert_elemental_to_three_variable_list(TIMES, b, a, tri);
			    d = insert_elemental_to_three_variable_list(PLUS, d, e, tri);

			    a = derivative(i, k - j, tri);
			    e = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
			    d0 = insert_elemental_to_three_variable_list(MINUS, d0, e, tri);
			}
			e = insert_integer_to_three_variable_list(k, tri);
			DERIVATIVE(i, k) = insert_elemental_to_three_variable_list(DIV, d, e, tri);
			DERIVATIVE(le, k) =
			    insert_elemental_to_three_variable_list(DIV, d0, e, tri);
		    }
		    break;
		case FCT_SINH:
		    {
			var3_t le = (tri->list)[i].left;
			var3_t ri = (tri->list)[i].rght;
			if (le < i)
			    break;
			d = AD_ZERO;
			var3_t d0 = AD_ZERO;
			for (j = 1; j <= k; j++) {
			    a = derivative(le, k - j, tri);
			    c = derivative(ri, j, tri);
			    e = insert_integer_to_three_variable_list(j, tri);
			    b = insert_elemental_to_three_variable_list(TIMES, e, c, tri);
			    e = insert_elemental_to_three_variable_list(TIMES, b, a, tri);
			    d = insert_elemental_to_three_variable_list(PLUS, d, e, tri);

			    a = derivative(i, k - j, tri);
			    e = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
			    d0 = insert_elemental_to_three_variable_list(PLUS, d0, e, tri);
			}
			e = insert_integer_to_three_variable_list(k, tri);
			DERIVATIVE(i, k) = insert_elemental_to_three_variable_list(DIV, d, e, tri);
			DERIVATIVE(le, k) =
			    insert_elemental_to_three_variable_list(DIV, d0, e, tri);
		    }
		    break;
		case FCT_COS:
           /** \f$v = \cos{a} \Rightarrow D^{[k]} v = (-\sum_{j=1}^{k} j \cdot D^{[j]}v \cdot D^{[k-j]}\sin{a})/k \f$ */
		    {
			var3_t le = (tri->list)[i].left;
			var3_t ri = (tri->list)[i].rght;
			if (le < i)
			    break;
			d = AD_ZERO;
			var3_t d0 = AD_ZERO;
			for (j = 1; j <= k; j++) {
			    b = derivative(le, k - j, tri);
			    c = derivative(ri, j, tri);
			    e = insert_integer_to_three_variable_list(j, tri);
			    a = insert_elemental_to_three_variable_list(TIMES, e, c, tri);
			    e = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
			    d = insert_elemental_to_three_variable_list(MINUS, d, e, tri);

			    b = derivative(i, k - j, tri);
			    e = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
			    d0 = insert_elemental_to_three_variable_list(PLUS, d0, e, tri);

			}
			e = insert_integer_to_three_variable_list(k, tri);
			DERIVATIVE(i, k) = insert_elemental_to_three_variable_list(DIV, d, e, tri);
			DERIVATIVE(le, k) =
			    insert_elemental_to_three_variable_list(DIV, d0, e, tri);
		    }
		    break;
		case FCT_COSH:
           /** \f$v = \cosh{a} \Rightarrow D^{[k]} v = (\sum_{j=1}^{k} j \cdot D^{[j]}v \cdot D^{[k-j]}\sinh{a})/k \f$ */
		    {
			var3_t le = (tri->list)[i].left;
			var3_t ri = (tri->list)[i].rght;
			if (le < i)
			    break;
			d = AD_ZERO;
			var3_t d0 = AD_ZERO;
			for (j = 1; j <= k; j++) {
			    b = derivative(le, k - j, tri);
			    c = derivative(ri, j, tri);
			    e = insert_integer_to_three_variable_list(j, tri);
			    a = insert_elemental_to_three_variable_list(TIMES, e, c, tri);
			    e = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
			    d = insert_elemental_to_three_variable_list(PLUS, d, e, tri);

			    b = derivative(i, k - j, tri);
			    e = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
			    d0 = insert_elemental_to_three_variable_list(PLUS, d0, e, tri);

			}
			e = insert_integer_to_three_variable_list(k, tri);
			DERIVATIVE(i, k) = insert_elemental_to_three_variable_list(DIV, d, e, tri);
			DERIVATIVE(le, k) =
			    insert_elemental_to_three_variable_list(DIV, d0, e, tri);
		    }
		    break;
		case FCT_EXP:
            /** \f$v=\exp{a} \Rightarrow D^{[k]} v = (\sum_{j=1}^{k-1} (k-j) \cdot D^{[j]}v \cdot D^{[k-j]}a)/k \f$ */
		    d = AD_ZERO;
		    for (j = 0; j <= k - 1; j++) {
			a = derivative(i, j, tri);
			b = derivative((tri->list)[i].rght, k - j, tri);
			c = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
			e = insert_integer_to_three_variable_list(k - j, tri);
			c = insert_elemental_to_three_variable_list(TIMES, e, c, tri);
			d = insert_elemental_to_three_variable_list(PLUS, d, c, tri);
		    }
		    e = insert_integer_to_three_variable_list(k, tri);
		    DERIVATIVE(i, k) = insert_elemental_to_three_variable_list(DIV, d, e, tri);
		    break;
		case FCT_LOG:
            /** \f$v=\ln{a} \Rightarrow D^{[k]} v = (D^{[k]}a - (\sum_{j=1}^{k-1} (k-j) \cdot D^{[j]}a \cdot D^{[k-j]}v)/k)/a \f$ */
		    d = AD_ZERO;
		    for (j = 1; j <= k - 1; j++) {
			a = derivative(i, k - j, tri);
			b = derivative((tri->list)[i].rght, j, tri);
			c = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
			e = insert_integer_to_three_variable_list(k - j, tri);
			c = insert_elemental_to_three_variable_list(TIMES, e, c, tri);
			d = insert_elemental_to_three_variable_list(PLUS, d, c, tri);
		    }
		    e = insert_integer_to_three_variable_list(k, tri);
		    d = insert_elemental_to_three_variable_list(DIV, d, e, tri);
		    b = derivative((tri->list)[i].rght, k, tri);
		    d = insert_elemental_to_three_variable_list(MINUS, b, d, tri);
		    b = derivative((tri->list)[i].rght, 0, tri);
		    DERIVATIVE(i, k) = insert_elemental_to_three_variable_list(DIV, d, b, tri);
		    break;
		case FCT_SQRT:
             /** \f$v=\sqrt{a} \Rightarrow D^{[k]} v = (\sum_{j=0}^{k-1} (k-3j) \cdot D^{[j]}v \cdot D^{[k-j]}a)/(2\cdot k\cdot a) \f$ */
		    d = AD_ZERO;
		    for (j = 0; j <= k - 1; j++) {
			a = derivative(i, j, tri);
			b = derivative((tri->list)[i].rght, k - j, tri);
			c = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
			e = insert_integer_to_three_variable_list(k - j * 3, tri);
			c = insert_elemental_to_three_variable_list(TIMES, e, c, tri);
			d = insert_elemental_to_three_variable_list(PLUS, d, c, tri);
		    }
		    e = insert_integer_to_three_variable_list(2 * k, tri);
		    b = derivative((tri->list)[i].rght, 0, tri);
		    e = insert_elemental_to_three_variable_list(TIMES, e, b, tri);
		    DERIVATIVE(i, k) = insert_elemental_to_three_variable_list(DIV, d, e, tri);
		    break;
		case FCT_TAN:
              {
               /** \f$D^{[k]} (\tan a) = (\sum_{j=1}^k j D^{[j]}a \cdot D^{[k-j]}((\operatorname{sec} a) ^2))/k \f$ */
			d = AD_ZERO;
               var3_t le = (tri->list)[i].left;
			var3_t ri = (tri->list)[i].rght;
			for (j = 1; j <= k ; j++) {
			    a = derivative(ri, j, tri);
			    b = derivative(le, k - j, tri);
			    c = insert_elemental_to_three_variable_list(TIMES, a, b, tri);
			    e = insert_integer_to_three_variable_list(j, tri);
			    c = insert_elemental_to_three_variable_list(TIMES, e, c, tri);
			    d = insert_elemental_to_three_variable_list(PLUS, d, c, tri);
			}
			e = insert_integer_to_three_variable_list(k, tri);
			d = insert_elemental_to_three_variable_list(DIV, d, e, tri);
			DERIVATIVE(i, k) = d;
		    }
		    break;
		case NUM_DEC:
		case NUM_CONST:
		case NUM_INT:
		case POSTFIX:
		    DERIVATIVE(i, k) = AD_ZERO;
		    break;
		default:
		    {
			assert(0);
		    }
		}
		if ((tri->list)[i].role == OUTPUT_VAR) {
		    a = insert_integer_to_three_variable_list(k + 1, tri);
		    b = insert_elemental_to_three_variable_list(DIV, derivative(i, k, tri), a, tri);
		    DERIVATIVE(tri->initval[(tri->list)[i].dim - 1] + 1, k + 1) = b;
		    insert_to_three_variable_list_output(b, k + 1, (tri->list)[i].dim, tri);
		}
	    }
	}
	printf("\r                  \r");
	fflush(stdout);
	find_int_nodes(tri);
    }
    if (!IS_NOCLEAN(&Task)) {
	while (!find_equal_nodes(tri)) {
	}
    }
    clean_three_variable_list(tri);
}


void method_recursive(const dim_t order, struct three_variable_list *tri)
{
    register dim_t k;

    for (k = 2; k <= order; k++) {
	if (!IS_VERBOSE(&Task) && !IS_QUIET(&Task)) {
	    printf("\rMaking order %d", k);
	    fflush(stdout);
	}
	make_taylor_coefficients(tri);
    }
    printf("\r                  \r");
    fflush(stdout);
    clean_three_variable_list(tri);
}
