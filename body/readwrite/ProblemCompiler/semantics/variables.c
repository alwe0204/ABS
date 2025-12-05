/** @file variables.c
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 04.02.2016 
 * @brief Functions required to create the data structure for the set of states and various related search functions
*/
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include <limits.h>

#include "variables.h"

#include "lists.h"
#include "expressions.h"
#include "reduce_int.h"
#include "errors.h"


/** Implements the image \f$\sigma(\verb|x|), \ \sigma \in S\f$ as well as the reference to the next state \f$\sigma' \in S \f$. */
struct variable {
    char *name;		      /**< Corresponds to the argument \f$\verb|x|\f$ of \f$\sigma\f$. The string of the identifier is copied in here. See insert_id_to_variables() */
    enum set_T type;	      /**< Component \f$\mathrm{type}\f$ of \f$\sigma(\verb|x|)\f$*/
    enum set_R role;	      /**< Component \f$\mathrm{role}\f$ of \f$\sigma(\verb|x|)\f$*/
    dim_t row;		      /**< Component \f$\mathrm{rowdim}\f$ of \f$\sigma(\verb|x|)\f$*/
    dim_t col;		      /**< Component \f$\mathrm{coldim}\f$ of \f$\sigma(\verb|x|)\f$*/
    struct expression **def;  /**< Component 1 of \f$\mathrm{data}\f$ of \f$\sigma(\verb|x|)\f$*/
    struct expression **intv; /**< Component 2 of \f$\mathrm{data}\f$ of \f$\sigma(\verb|x|)\f$*/
    dim_t idx;		      /**< Component \f$\mathrm{idx}\f$ of \f$\sigma(\verb|x|)\f$*/
    struct variable *next;    /**< The location of \f$\sigma'(\verb|x|)\f$. */
    date_t *dates;	      /**< An index for \f$\sigma'\f$. */
};


struct states {
    struct variable *list;    /**< List */
    size_t len;		      /**< Length of \p list */
    date_t date;	      /**< The current state of the parsing */
    date_t date_of_rhs;	      /**< The state where the parsing of the first function ended. */
};

#define ALLOC_VAR 6 /**< The block length to allocate used in alloc_list() */
/** 
 * @brief This method allocates the space for the name of an
 * identifier.
 * @param in  Pointer to the list of identifiers
 * @param beg The first index to work on
 * @param end The last index to work on
 *
 * Assumptions:
 * \c in != \p NULL
*/
void alloc_variable(void *in, const size_t beg, const size_t end)
{
    register size_t i;
    struct variable *ptr = (struct variable *) in;
    for (i = beg; i < end; i++) {
	ptr[i].name = my_malloc(ID_LEN * sizeof(char));
	memset(ptr[i].name, 0, ID_LEN * sizeof(char));
	ptr[i].role = UNUSED_VAR;
	ptr[i].dates = NULL;
	ptr[i].idx = 0;
	ptr[i].next = NULL;
    }
}


SequenceOfStates install_SequenceOfStates(void)
{
    struct states *out = my_malloc(sizeof(struct states));
    out->len = 0;
    out->date = 0;
    out->date_of_rhs = 0;
    alloc_list((void **) (&(out->list)), out->len, sizeof(struct variable),
	       ALLOC_VAR, &alloc_variable);
    NO_MEM(out->list) return out;
}

/** @brief Returns the location of the sequence \f$(\sigma_1(\texttt{x}),\ldots,\sigma_n(\texttt{x}))\f$
 * @param in \f$\texttt{x}\f$
 * @param var
 * @returns \c NULL if \p in is not found else the pointer to the struct found
 * 
 * 
 * This function searches the string \p in in \p list. On success it also sets
 * the role from ::UNUSED_VAR to ::ORDINARY_VAR. The latter is done to detect
 * unused variables in the input file and to generate a warning.
 *
 * 
 * Assumptions:
 * \p in != \c NULL 
*/
Variable find_in_variables_from_string(const char *in, ConstSequenceOfStates S)
{
    assert(in != NULL);
    register size_t i;
    int a;
    Variable list = S->list;
    const size_t len = S->len;
    for (i = 0; i < len; i++) {
	a = strcmp(in, list[i].name);
	if (a == 0) {
	    if (list[i].role == UNUSED_VAR)
		list[i].role = ORDINARY_VAR;
	    return (list + i);
	}
    }
    return NULL;
}

/**
 * @brief Returns the location of the sequence 
 * \f$(\sigma_1(\mathrm{name}\llbracket a \rrbracket),\ldots,\sigma_n(\mathrm{name}\llbracket a \rrbracket))\f$
 * @param in   \f$a\f$
 * @param var
 * @returns \c NULL if not in \p list else the pointer to the found entry
 *
 *
 * The core of this function is find_in_variables_from_string() 
 *
 * Assumptions:
 * The type of \p in is ::POSTFIX
*/
Variable find_in_variables(const PostFixExpression in, ConstSequenceOfStates S)
{
    assert(TYPE(in) == POSTFIX);
    return (find_in_variables_from_string(NAME(in), S));
}

/** @brief Returns the location of \f$\sigma_k(\texttt{x})_{\mathrm{data},1+r,1+c,\cdot}\f$
 * @param in \f$\texttt{x}\f$
 * @param row \f$r\f$
 * @param col \f$c\f$
 * @param date \f$k\f$
 *
 * @Returns \c NULL if \f$ \sigma_k(\texttt{x}) = \sigma_0(\texttt{x})\f$ otherwise as described above.
*/

Variable
version_of_variable(struct variable * in, const dim_t row, const dim_t col, const date_t date)
{
    Variable x = in;
    Variable y = x;
    if (y != NULL && y->dates[(y->row) * col + row] >= date + 1)
	return NULL;
    while (y != NULL && y->dates[(y->row) * col + row] < date + 1) {
	x = y;
	y = y->next;
    }
    return x;
}

/** @brief Returns the location of the first \f$x \in A\f$ such that \f$\sigma(x)_\mathrm{role} = \rho\f$, where \f$\sigma\f$ is at \f$k\f$
 * @param role \f$\rho\f$
 * @param date \f$k\f$
 * @param var A sequence \f$(\sigma_0,\ldots,\sigma_k,\ldots,\sigma_n)\f$ of states in \f$S\f$
 *
 * @Returns \c NULL if there is no such \f$x\f$
*/
Variable
find_in_variables_match_role_at_date(const enum set_R role,
				     const date_t date, ConstSequenceOfStates S)
{
    register size_t i;
    Variable x;
    for (i = 0; i < S->len; i++) {
	x = version_of_variable((S->list) + i, 0, 0, date);
	if (x->role == role)
	    return x;
    }
    return NULL;
}

/** @brief Returns the location of the first \f$x \in A\f$ such that \f$\sigma(x)_\mathrm{role} = \rho\f$ and \f$\sigma(x)_\mathrm{idx} = i\f$, where \f$\sigma\f$ is at \f$k\f$
 * @param role \f$\rho\f$
 * @param idx \f$i\f$
 * @param date \f$k\f$
 * @param var A sequence \f$(\sigma_0,\ldots,\sigma_k,\ldots,\sigma_n)\f$ of states in \f$S\f$
 *
 * @Returns \c NULL if there is no such \f$x\f$
*/
Variable
find_in_variables_match_role_idx_at_date(const enum set_R role,
					 const dim_t idx,
					 const date_t date, ConstSequenceOfStates S)
{
    register size_t i;
    Variable x;
    for (i = 0; i < S->len; i++) {
	x = version_of_variable((S->list) + i, 0, 0, date);
	if (x->role == role && x->idx == idx)
	    return x;
    }
    return NULL;
}

/**
 * @param x \f$\sigma(\verb|x|)\f$
 * @returns \c true if \f$\sigma(x)_\mathrm{type} = \mathbf{undeclared}\f$ otherwise returns \c false
*/
int is_undeclared(ConstVariable x)
{
    return ((x != NULL && x->type != UNDECLARED) ? false : true);
}

/** @brief Checks if \f$\sigma_k(\operatorname{name}\llbracket a \rrbracket)_\mathrm{type} \neq \mathbf{undeclared}\f$, where \f$k\f$ is the
 * date when \f$a\f$ appears in the program. When this is not the case then #ERROR_UNDECL is thrown. It relies on is_undeclared().
 * @param in \f$a\f$
 * @param var A sequence \f$(\sigma_0,\ldots,\sigma_k,\ldots,\sigma_n)\f$ of states in \f$S\f$
 * @returns \f$\sigma_k(\operatorname{name}\llbracket a \rrbracket)\f$
*/
Variable check_declared(PostFixExpression in, ConstSequenceOfStates S)
{
    Variable x = find_in_variables(in, S);
    x = version_of_variable(x, 0, 0, get_date(S));
    if (is_undeclared(x))
	ERROR_UNDECL(NAME(in), in->line) return x;
}

/** @brief Checks if \f$\sigma_k(\operatorname{name}\llbracket a \rrbracket)_\mathrm{type} = \mathbf{undeclared}\f$, where \f$k\f$ is the
 * date when \f$a\f$ appears in the program. When this is not the case then #ERROR_DECL is thrown. It relies on is_undeclared().
 * @param in \f$a\f$
 * @param var A sequence \f$(\sigma_0,\ldots,\sigma_k,\ldots,\sigma_n)\f$ of states in \f$S\f$
 * @returns \f$\sigma_k(\operatorname{name}\llbracket a \rrbracket)\f$
*/
Variable check_undeclared(PostFixExpression in, ConstSequenceOfStates S)
{
    Variable x = find_in_variables(in, S);
    x = version_of_variable(x, 0, 0, get_date(S));
    if (!is_undeclared(x))
	ERROR_DECL(NAME(in), in->line) return x;
}

/**
 * @brief Same functionality as find_in_variables() but if \p in is not in \p list then
 * #ERROR_UNDECL is thrown.
 * @param in   PostFixExpression to look up in the list.
 * @param list List of variables already defined.
 * @param len  Lenght of list
*/
struct variable *find_in_variables_check_declared(PostFixExpression in, ConstSequenceOfStates S)
{
    Variable x = find_in_variables(in, S);
    if (x == NULL) {
	ERROR_UNDECL(NAME(in), in->line);
    } else
	return x;
}





/**
 * @brief Checks whether the entries in \p var have role ::UNUSED_VAR. If so it throws #WARNING_UNUSED.
 * @param var The variable environment
*/
void check_used_variables(ConstSequenceOfStates S)
{
    size_t i;
    for (i = 0; i < S->len; i++) {
	if ((S->list[i]).role == UNUSED_VAR) {
	WARNING_UNUSED((S->list[i]).name)}
    }
}



void set_date_of_rhs(SequenceOfStates S)
{
    S->date_of_rhs = S->date;
}


void increase_date(SequenceOfStates S)
{
    (S->date)++;
    if (S->date == UINT_MAX)
	NO_MEM(NULL);
}

/**
 * @brief Implementation of \f$\operatorname{IsEntry} \colon [ \verb|postfix_expression| ] \to \{ S \to \{0,1\} \} \f$
 * but with returning the location of \f$\sigma(\operatorname{name}\llbracket a \rrbracket)\f$ if 
 * \f$\operatorname{IsEntry}\llbracket a \rrbracket(\sigma) = 1\f$ and otherwise throws #ERROR_IDX.
 * @param in \f$a \in [\verb|postfix_expression|]\f$
 * @param var The current state \f$\sigma \in S\f$
*/
struct variable *IsEntry(PostFixExpression in, ConstSequenceOfStates S)
{
    assert(TYPE(in) == POSTFIX);
    dim_t row, col;
    Variable x = find_in_variables_check_declared(in, S);
    const date_t a = get_date_of_postfix(in, S);
    x = version_of_variable(x, 0, 0, a);
    get_indices_of_PostFixExpression(in, &row, &col, 0, S);
    if ((ROW_IDX(in) == NULL && LEN_ROW(x) > 1) || row >= LEN_ROW(x))
	ERROR_IDX(NAME(in), in->line)
	    if ((COL_IDX(in) == NULL && LEN_COL(x) > 1)
		|| col >= LEN_COL(x))
	    ERROR_IDX(NAME(in), in->line) return x;
}

dim_t row_dim_of(const struct variable * const x)
{
    return x->row;
}

dim_t col_dim_of(const struct variable * const x)
{
    return x->col;
}

char *name_of(const struct variable *const x)
{
    return x->name;
}

enum set_T var_type_of(const struct variable *const x)
{
    return x->type;
}

void set_var_role_of(struct variable *x, enum set_R role)
{
    x->role = role;
}

dim_t var_idx(const struct variable *x)
{
    return x->idx;
}

void set_var_idx(struct variable *x, dim_t idx)
{
    x->idx = idx;
}

enum set_R var_role_of(const struct variable *x)
{
    return x->role;
}

date_t get_date(const struct states * var)
{
    return var->date;
}

date_t get_date_of_rhs(const struct states * var)
{
    return var->date_of_rhs;
}

struct variable *get_var(size_t i, const struct states *var)
{
    return (var->list + i);
}

size_t get_states_len(const struct states * var)
{
    return var->len;
}

void kill_var(struct variable *x)
{
    free(x->def);
    x->def = NULL;
}

int is_var_dead(const struct variable *x)
{
    return (x == NULL || x->def == NULL || x->def[0] == NULL);
}

/** @brief Returns \f$\sigma(\verb|x|)_{\mathrm{data},1+r,1+c,1} \f$
 * @param x \f$\sigma(\verb|x|)\f$
 * @param row \f$r\f$
 * @param col \f$c\f$
*/

Expression data_of(const struct variable * const x, dim_t const row, dim_t const col)
{
    assert(x != NULL);
    assert(x->def != NULL);
    return x->def[(x->row) * col + row];
}

/** @brief Returns \f$\sigma(\verb|x|)_{\mathrm{data},1+r,1+c,2} \f$
 * @param x \f$\sigma(\verb|x|)\f$
 * @param row \f$r\f$
 * @param col \f$c\f$
*/

Expression intv_of(const struct variable * const x, dim_t const row, dim_t const col)
{
    assert(x != NULL);
    assert(x->intv != NULL);
    return x->intv[(x->row) * col + row];
}

/** @brief Prepares the data structures when a transition
 * \f$\sigma \in S\f$ to \f$\sigma' \in S\f$ occurs.
 * @param new Storage location of \f$\sigma'(\verb|x|)\f$
 * @param old Storage location of \f$\sigma(\verb|x|)\f$
*/

void initialize_variable(struct variable *new, const struct variable *old, date_t date)
{
    new->def = (Expression *) my_malloc((new->row) * (new->col) * sizeof(Expression));
    new->intv = (Expression *) my_malloc((new->row) * (new->col) * sizeof(Expression));
    new->dates = (date_t *) my_malloc((new->row) * (new->col) * sizeof(date_t));
    size_t i;
    for (i = 0; i < (new->row) * (new->col); i++) {
	if (old == NULL) {
	    new->def[i] = NULL;
	    new->intv[i] = NULL;
	} else {
	    new->def[i] = old->def[i];
	    new->intv[i] = old->intv[i];
	}
	new->dates[i] = date;
    }
}

/** @brief Prepares the data structures when a transition
 * \f$\sigma \in S\f$ to \f$\sigma' \in S\f$ occurs due to \f$x \in [\verb|identifier|]\f$
 * @param x   \f$x\f$
*/
struct variable *insert_new_version_of_variable(struct variable *x)
{
    assert(x != NULL);
    struct variable *old = x;
    while (old->next != NULL) {
	old = old->next;
    }
    struct variable *new = my_malloc(1 * sizeof(struct variable));
    old->next = new;
    new->next = NULL;
    new->name = x->name;
    new->row = x->row;
    new->col = x->col;
    new->role = x->role;
    new->type = x->type;
    new->idx = x->idx;
    initialize_variable(new, old, 0);
    return new;
}

struct variable *insert_new_version_of_variable_with_role(struct
							  variable *x, enum set_R role, struct states
							  *var)
{
    x = insert_new_version_of_variable(x);
    x->dates[0] = get_date(var);
    x->role = role;
    return x;
}

/** @brief Transforms
 * \f$\sigma \in S\f$ to \f$\sigma' \in S\f$ where \f$\sigma'(x)_\mathrm{type} = \mathbf{undeclared}\f$
 * @param x   \f$x\f$
 * @param var Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/
void make_variable_undeclared(struct variable *x, const struct states *var)
{
    struct variable *y;
    y = insert_new_version_of_variable(x);
    y->dates[0] = get_date(var);
    y->type = UNDECLARED;
    y->role = ORDINARY_VAR;
    y->idx = 0;
}


/** @brief Prepares storage location for \f$\sigma(\verb|x|)\f$.
 * In particular, it checks \f$\sigma(\verb|x|)_\mathrm{type} = \mathbf{undeclared} \f$.
 * @param in \f$\verb|x|\f$
 * @param var The current state \f$\sigma \in S \f$.
*/

struct variable *insert_id_to_variables(Identifier in, dim_t row,
					dim_t col, enum set_T type, struct states *var)
{
    struct variable *x = check_undeclared(in, var);
    struct variable *out;
    if (x != NULL) {
	out = insert_new_version_of_variable(x);
    } else {
	alloc_list((void **) (&(var->list)), var->len, sizeof(struct variable), ALLOC_VAR, &alloc_variable);	/* Allocate space in list (of variables) */
	NO_MEM(var->list) struct variable *dest = &(var->list)[var->len];

	(var->len)++;
	strcpy(NAME(dest), NAME(in));
	out = dest;
    }
    out->row = row;
    out->col = col;
    out->type = type;
    return out;
}

/** @brief Implementation of the \a implicit declarations occuring in
 *   -# \f$\mathcal{I} \colon [\verb|iteration_head|] \to \{ S \to S\}\f$,
 *   -# \f$\mathcal{I} \colon [\verb|function_head|] \to \{ S \to S \}\f$
 * @param in \f$\verb|x|\f$
 * @param row \f$\sigma'(\verb|x|)_\mathrm{rowdim} =\f$ \p row
 * @param col \f$\sigma'(\verb|x|)_\mathrm{coldim} =\f$ \p col
 * @param role \f$\sigma'(\verb|x|)_\mathrm{role} = \f$ \p role
 * @param idx \f$\sigma'(\verb|x|)_\mathrm{idx} =\f$ \p idx
 * @param var Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
 * 
 * Moreover, \f$\sigma'(\verb|x|)_\mathrm{role} = \mathbf{Real}\f$, and
 * checks \f$\sigma(\verb|x|)_\mathrm{type} = \mathbf{undeclared} \f$
*/

struct variable *insert_id_to_variables_manually(Identifier in,
						 dim_t row, dim_t col,
						 enum set_T type,
						 enum set_R role, dim_t idx, struct states *var)
{
    assert(in != NULL && row > 0 && col > 0);
    struct variable *dest = insert_id_to_variables(in, row, col, type, var);
    dest->role = role;
    dest->def = NULL;
    dest->intv = NULL;
    dest->idx = idx;
    initialize_variable(dest, NULL, get_date(var));
    return dest;
}

/** @brief Returns \f$\sigma_n(\operatorname{name}\llbracket a \rrbracket)\f$.
 * @param in \f$a\f$
 * @param var A sequence \f$(\sigma_0,\ldots,\sigma_k,\ldots,\sigma_n)\f$ of states in \f$S\f$
*/
struct variable *current_state_of(PostFixExpression in, struct states *var)
{
    struct variable *x = find_in_variables(in, var);
    dim_t row, col;
    get_indices_of_PostFixExpression(in, &row, &col, 0, var);
    x = version_of_variable(x, row, col, get_date(var));
    return x;
}


void check_defined(const struct variable *x)
{
    register size_t j;
    if (x->def == NULL)
	ERROR_UNBOUNDED(NAME(x), 0);
    for (j = 0; j < LEN_ROW(x) * LEN_COL(x); j++) {
	if (x->def[j] == NULL)
	    ERROR_UNBOUNDED(NAME(x), 0);
    }
}




void check_output_defined(const struct states *var)
{
    register size_t i;
    struct variable *list = var->list;
    struct variable *x;
    for (i = 0; i < var->len; i++) {
	if (list[i].role == OUTPUT_VAR) {
	    x = list + i;
	    x = version_of_variable(x, 0, 0, get_date(var));
	    check_defined(x);
	}
    }
}

int is_PostFixExpression_read_only(PostFixExpression in, const struct states *var)
{
    struct variable *x = find_in_variables(in, var);
    return ((x->next == NULL ? true : false));
}

/** @brief Sets \f$\big ( (\sigma'(\operatorname{name}\llbracket a \rrbracket)_{\mathrm{data}} )_{1+\operatorname{row}_0\llbracket a\rrbracket(\sigma),1+\operatorname{col}_0\llbracket a\rrbracket(\sigma)} \big )_1 =\mathcal{I}_1\llbracket E \rrbracket(\sigma)\f$
 * @param in \f$E\f$
 * @param x \f$a\f$
 * @param row \f$\operatorname{row}_0\llbracket a\rrbracket(\sigma)\f$
 * @param col \f$\operatorname{col}_0\llbracket a\rrbracket(\sigma)\f$
 * @param var Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/
void
write_data_1(Expression in,
	     struct variable *x, const dim_t row, const dim_t col, const struct states *var)
{
    struct variable *ptr = x;
    const dim_t idx = (x->row) * col + row;
    if (ptr->dates[idx] != 0) {
	ptr = x->next;
	while (ptr != NULL && ptr->dates[idx] != 0) {
	    ptr = ptr->next;
	}
	if (ptr == NULL) {
	    ptr = insert_new_version_of_variable(x);
	}
    }
    (ptr->dates)[idx] = get_date(var);
    (ptr->def)[idx] = in;
    struct variable *y = ptr->next;
    while (y != NULL) {
	y->def[idx] = ptr->def[idx];
	y = y->next;
    }
}

/** @brief Sets \f$\big ( (\sigma'(\operatorname{name}\llbracket a \rrbracket)_{\mathrm{data}} )_{1+\operatorname{row}_0\llbracket a\rrbracket(\sigma),1+\operatorname{col}_0\llbracket a\rrbracket(\sigma)} \big )_2 =\mathcal{I}_2\llbracket E \rrbracket(\sigma)\f$
 * @param in \f$E\f$
 * @param x \f$\sigma(\operatorname{name}\llbracket a \rrbracket)\f$
 * @param row \f$\operatorname{row}_0\llbracket a\rrbracket(\sigma)\f$
 * @param col \f$\operatorname{col}_0\llbracket a\rrbracket(\sigma)\f$
 * @param var Before execution \f$ \sigma \in S\f$, after execution \f$\sigma' \in S \f$.
*/
void write_data_2(Expression in, struct variable *x, const dim_t row, const dim_t col)
{
    const dim_t idx = (x->row) * col + row;
    x->intv[idx] = in;
}

void
write_data_in_function_head(Expression in, struct variable *x, const dim_t row, const dim_t col)
{
    const dim_t idx = (x->row) * col + row;
    x->def[idx] = in;
    x->intv[idx] = in;
}
