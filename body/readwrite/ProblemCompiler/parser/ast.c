/** @file
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 01 Feb 2016
 * @brief All functions and methods required to create from the input file
 *        the abstract syntax tree.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>		/* for true and false */
#include <limits.h>
#include <inttypes.h>
#include <errno.h>
#include <assert.h>
#include <math.h>

#include "ast.h"

#include "errors.h"
#include "errors_abcs.h"


extern int yylineno;

void *my_malloc(size_t size)
{
    void *out = malloc(size);
    NO_MEM(out)
	return out;
}

/**
 * @brief Allocates and initializes an expression (type is set to ::NOTHING)
 * @returns The pointer to the allocated memory
*/
Expression alloc_expression(void)
{
    Expression out = (Expression) my_malloc(1 * sizeof(struct expression));
    out->type = NOTHING;
    out->name = NULL;
    out->left = NULL;
    out->right = NULL;
    out->content = NULL;
    out->computable = false;
    out->ad = -1;
    out->next = NULL;
    out->prev = NULL;
    out->line = 0;
    return out;
}


/** @brief Allocates and initializes an identifier
 * @param s the name of the identifier in the input file
 * @returns The pointer to the allocated memory
 *
 * It is checked whether the input string is not longer than #ID_LEN characters. It allocates
 * memory for the string of precisely this length.
*/
Identifier install_Identifier(char s[])
{
    if (strlen(s) >= ID_LEN) {
    ERROR_INVALID_INPUTSTRING(s, ID_LEN)}
    Identifier out = alloc_expression();
    out->name = (char *) my_malloc(ID_LEN * sizeof(char));
    strcpy(out->name, s);
    out->type = POSTFIX;
    out->line = yylineno;
    return out;
}

/** @brief Allocates space for an identifier when an identifier is detected by lex
 * @param s Identifier read from the input file
*/
void lex_id(char s[])
{
    current_id = install_Identifier(s);
}

/** @brief Appends \p op2 to \p op1 where both \p op1 and \p op2 are data structures of type \p type. So member \p next of \p op1 is set to \p op2.
 * @param type The type of the data structure
 * @param op1 First data structure
 * @param op2 Second data structure
*/
void *append(enum ASTNode_type type, void *op1, void *op2)
{
    assert(op1 != NULL && op2 != NULL);
    switch (type) {
    case AST_EXPR:
	((Expression) op1)->next = (Expression) op2;
	((Expression) op2)->prev = (Expression) op1;
	break;
    case AST_STMT:
	((Statement) op1)->next = (Statement) op2;
	((Statement) op2)->prev = (Statement) op1;
	break;
    default:
	assert(0);
    }
    return op2;
}

Expression install_Number_int(const int s)
{
    Expression out = alloc_expression();
    out->type = NUM_INT;
    out->content = malloc(sizeof(int));
    *((int *) out->content) = s;
    return out;
}

/** @brief Sets the row and column index of \p in to \p row and \p col, respectively.
 * @param in The identifier to become a postfix expression
 * @param row The row index
 * @param col The column index
 * @returns \p in
*/
PostFixExpression build_PostFixExpression(Identifier in, Expression row, Expression col)
{
    ROW_IDX(in) = row;
    COL_IDX(in) = col;
    return in;
}



/** @brief Allocates memory for an expression and initializes it.
 *  @param type Expression type
 *  @param op1 Member left of the newly allocated expression.
 *  @param op2 Member right of the newly allocated expression.
 *  @returns The created expression.
 */
Expression install_expression(enum expression_type type, void *op1, void *op2)
{
    Expression out = alloc_expression();
    out->type = type;
    out->line = yylineno;
    out->left = (Expression) op1;
    out->right = (Expression) op2;
    return out;
}


Assignment install_assignment(unsigned short int type, PostFixExpression op1, Expression op2)
{
    assert(op1 != NULL && op2 != NULL);
    Assignment out = (Assignment) my_malloc(sizeof(struct assignment));
    if (type == 1) {
	if (op2->type == CONT_INTV || op2->type == DISC_INTV)
	    out->type = 2;
	else
	    out->type = type;
    } else {
	out->type = type;
    }
    out->left = op1;
    out->right = op2;
    out->line = yylineno;
    return out;
}

/** @brief Allocates and initializes a statement
 * @param type The type of the statement.
 * @param in The content of the statement
 * @returns The pointer to the allocated memory
 *
*/

Statement install_statement(enum statement_type type, void *in)
{
    Statement out = my_malloc(sizeof(struct statement));
    out->type = type;
    out->ptr = in;
    out->next = NULL;
    out->prev = NULL;
    return out;
}

/** @brief Allocates and initializes a declaration
 * @param type The type of the declaration
 * @param in The postfix expression to declare
 * @returns The pointer to the allocated memory
 *
*/
Declaration install_declaration(enum set_T type, PostFixExpression in)
{
    Declaration out = (Declaration) my_malloc(sizeof(struct declaration));
    out->type = type;
    out->content = in;
    out->right = NULL;
    out->line = yylineno;
    return out;
}

Declaration install_declaration_and_definition(enum set_T type, Identifier op1, Expression op2)
{
    Declaration out = (Declaration) my_malloc(sizeof(struct declaration));
    out->type = type;
    out->content = op1;
    out->right = op2;
    out->line = yylineno;
    return out;
}


/** 
 * @brief Converts a string to an integer. If the integer is larger than #INT_MAX then
 * an error is returned
 * @param s The string to convert to an integer
 * @param line The line in which the string appears in the input file
 * @returns The integer number represented by the string unless the number is larger than #INT_MAX
*/
int string_to_int(const char *s, unsigned int line)
{
    char *ptr;
    errno = 0;
    intmax_t a = strtoimax(s, &ptr, 10);
    if (errno != 0 || a > INT_MAX) {
	ERROR_INT_OVERFLOW(line)
    }
    return (int) a;
}

Expression install_Number(PostFixExpression in, enum expression_type type, char s[])
{
    Expression out = alloc_expression();
    out->type = type;
    out->line = yylineno;
    if (in == NULL) {
	if (type == NUM_INT) {
	    out->content = malloc(sizeof(intmax_t));
	    *((intmax_t *) out->content) = string_to_int(s, out->line);
	} else if (type == NUM_DEC) {
	    if (!isfinite((double) strtod(s, NULL)))
		ERROR_DEC_OVERFLOW(yylineno)
		    out->content = malloc((strlen(s) + 1) * sizeof(char));
	    strcpy((char *) out->content, s);
	} else {
	    out->content = malloc((strlen(s) + 1) * sizeof(char));
	    strcpy((char *) out->content, s);
	}
    } else {
	out->content = (void *) in;
    }
    return out;
}

/** @brief Allocates memory for the abstract syntax tree
 * @param op1 The type of the statement
 * @param op2
 * @param op3
 * @param op4
 * @returns The pointer to the allocated memory
 *
*/

struct ASTNode *install_program(Statement op1, FunctionDefinition op2, Ode op3, Option op4)
{
    struct ASTNode *out = my_malloc(sizeof(struct ASTNode));
    out->Prologue = op1;
    out->Function = op2;
    out->ode = op3;
    out->options = op4;
    return out;
}

/** @brief Allocates and initializes an iteration
 * @param type The type of the iteration
 * @param op1 The iteration variable
 * @param op2 The first value of the iteration variable
 * @param op3 The last value of the iteration variable
 * @returns The pointer to the allocated memory
 *
*/

Iteration install_iteration(enum iteration_type type, Identifier op1, Expression op2,
			    Expression op3)
{
    Iteration out = my_malloc(sizeof(struct iteration));
    out->type = type;
    out->var = op1;
    out->start = op2;
    out->end = op3;
    out->line = yylineno;
    return out;
}


/** @brief Appends to \p head the iteration body \p body
 * @param head The iteration head
 * @param body The iteration body
 * @returns \p head
*/
Iteration build_iteration(Iteration head, Statement body)
{
    head->body = body;
    return head;
}


/** @brief Allocates memory for a function
 * @param in The identifier declared as the funciton
 * @returns The pointer to the allocated memory
 *
*/
FunctionDefinition install_function(Identifier in)
{
    FunctionDefinition out = (FunctionDefinition) my_malloc(sizeof(struct function));
    if (in == NULL) {
	out->head = NULL;
    } else {
	out->head = install_declaration(TY_FCT, in);
    }
    out->line = yylineno;
    return out;
}

void build_function(enum function_type type, FunctionDefinition in, Expression op1, Expression op2,
		    Expression op3)
{
    in->type = type;
    in->var = op1;
    in->dom = op2;
    in->img = op3;
}

FunctionDefinition finish_function(FunctionDefinition in, Statement op1)
{
    if (op1 != NULL)
	in->body = op1;
    else
	assert(0);
    return in;
}

Ode install_ode_statement(Identifier var, Identifier time, Identifier rhs, Identifier rhs_var)
{
    Ode out = (Ode) my_malloc(sizeof(struct ode));
    out->phi = current_fct;
    out->time = time;
    out->var = var;
    out->rhs = rhs;
    out->rhs_var = rhs_var;
    out->initval = NULL;
    out->dim = 0;
    out->line = yylineno;
    return out;
}

Ode build_ode_statement(Ode in, Statement s)
{
    in->initval = s;
    return in;
}

void build_option_core(Expression op1, Expression * op2)
{
    op1->prev = *op2;
    if ((*op2) != NULL)
	(*op2)->next = op1;
    (*op2) = op1;
}

void build_option_core_two(Expression op1, Expression * op2)
{
    if ((*op2) != NULL)
	ERROR_MULTIPLE_DEFINITIONS(op1->line)
	    (*op2) = op1;
}

Option install_option(void)
{
    Option out = (struct option *) my_malloc(sizeof(struct option));
    out->SamplingTime = NULL;
    out->Range = NULL;
    out->Periods = NULL;
    out->Uncertainties = NULL;
    out->MeasErrors = NULL;
    out->StateSpaceDiscret = NULL;
    out->InputSpaceDiscret = NULL;
    out->ObstacleSet = NULL;
    out->TargetSet = NULL;
    out->InitialSet = NULL;
    out->IntegrationOrder = 0;
    out->IntegrationOrderGrowth = 0;
    out->InputSet = NULL;
    return out;
}

Option build_option(enum option_type type, Option in, void *op1)
{
    assert(in != NULL);
    switch (type) {
    case OPTION_SAMPLINGTIME:
	build_option_core_two((Expression) op1, &(in->SamplingTime));
	break;
    case OPTION_PERIODS:
	build_option_core_two((Expression) op1, &(in->Periods));
	break;
    case OPTION_UNCERTAINTIES:
	build_option_core_two((Expression) op1, &(in->Uncertainties));
	break;
    case OPTION_MEAS:
	build_option_core_two((Expression) op1, &(in->MeasErrors));
	break;
    case OPTION_RANGE:
	build_option_core_two((Expression) op1, &(in->Range));
	break;
    case OPTION_STATE_SPACE:
	build_option_core_two((Expression) op1, &(in->StateSpaceDiscret));
	break;
    case OPTION_INPUT_SPACE:
	build_option_core((Expression) op1, &(in->InputSpaceDiscret));
	break;
    case OPTION_OBSTACLESET:
	build_option_core((Expression) op1, &(in->ObstacleSet));
	break;
    case OPTION_TARGETSET:
	build_option_core((Expression) op1, &(in->TargetSet));
	break;
    case OPTION_INITIALSET:
	build_option_core((Expression) op1, &(in->InitialSet));
	break;
    case OPTION_INTEGR_ORDER:
     build_option_core_two((Expression) op1, &(in->IntegrationOrder));
     break;
    case OPTION_INTEGR_ORDER_GROWTH:
     build_option_core_two((Expression) op1, &(in->IntegrationOrderGrowth));
     break;
    default:
	assert(0);
	break;
    }
    return in;
}

/** @brief When \p in is a linked data structure then the method follows the trace prev->prev->... until prev = \c NULL
 * @param type The type of the data structure
 * @param in The data structure
 * @returns The last prev not equal to \c NULL
 *
 * The caller must ensure \p in != \c NULL and \p type != ::AST_OPT
*/
void *wind_back(enum ASTNode_type type, void *in)
{
    assert(in != NULL);
    switch (type) {
    case AST_EXPR:
	{
	    Expression ptr = (Expression) in;
	    while (ptr->prev != NULL) {
		ptr = ptr->prev;
	    }
	    return (void *) ptr;
	}
	break;
    case AST_STMT:
	{
	    Statement ptr = (Statement) in;
	    while (ptr->prev != NULL) {
		ptr = ptr->prev;
	    }
	    return (void *) ptr;
	}
	break;
    default:
	assert(0);
	break;
    }
}
