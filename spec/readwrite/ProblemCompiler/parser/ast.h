/** @file
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 01 Nov 2016
 * @brief Header for ast.c. In particular, data structures required to build the abstract syntax tree are defined here
*/
#ifndef AST_H
#define AST_H
#include <stdio.h>
#include <stdlib.h>

#define CON_AD(X)  ((X)->ad)
#define CONTENT(X) ((X)->content)
#define TYPE(X)    ((X)->type)
#define NAME(X)    ((X)->name)
#define DEF(X)     ((X)->def)
#define GET_INT(X) *((int *)((X)->content))
#define DATA(X,Y,Z) ((X)->def[((X)->row)*(Z)+(Y)])

#define ROW_IDX(X) ((X)->left)
#define COL_IDX(X) ((X)->right)

#define LEN_ROW(X) ((X)->row)
#define LEN_COL(X) ((X)->col)
#define FCT_NAME(X) (((X)->head == NULL) ? NULL : ((X)->head->content->name))


#define ID_LEN    31 /**< The maximum number of characters allowed for an identifier */

enum ASTNode_type { AST_EXPR, AST_OPT, AST_STMT };

/** Implementation of the set \f$\mathbf{T}\f$. */
enum set_T { TY_REAL	     /**<Real number, \f$\mathbf{Real} \in \mathbf{T}\f$ */
	, TY_INTV	     /**<Interval, \f$\mathbf{Interval} \in \mathbf{T}\f$ */
	, TY_FCT		   /**<Function, \f$\mathbf{Function} \in \mathbf{T}\f$*/
    , UNDECLARED /**<None of the previous, \f$\mathbf{undeclared} \in \mathbf{T}\f$*/
};

/** Implementation of the set \f$\mathbf{R}\f$. */
enum set_R { ORDINARY_VAR      /**<\f$\mathbf{Ordinary} \in \mathbf{R}\f$*/
	, INPUT_VAR	       /**<\f$\mathbf{Input} \in \mathbf{R}\f$*/
	, OUTPUT_VAR	       /**<\f$\mathbf{Output} \in \mathbf{R}\f$*/
	, ITER_VAR	       /**<\f$\mathbf{Iterator} \in \mathbf{R}\f$*/
	, STATE_VAR	       /**<\f$\mathbf{State} \in \mathbf{R}\f$*/
	, TIME_VAR	       /**<\f$\mathbf{Time} \in \mathbf{R}\f$*/
	, OUTPUTSTATE_VAR	     /**<\f$\mathbf{OutputState} \in \mathbf{R}\f$*/
	, INPUTCONST_VAR       /**<\f$\mathbf{InputState} \in \mathbf{R}\f$*/
    , UNUSED_VAR /**<\f$\mathbf{unused} \in \mathbf{R}\f$*/
};

/** Indicates the object which is stored in struct Expression */
enum expression_type { PLUS	 /**<Addition */
	, MINUS		     /**<Subtraction */
	, TIMES		     /**<Multiplication */
	, DIV		     /**<Division */
	, POW			      /**<Exponentiation with integer exponent (this is NOT parsed but set afterwards) */
	, EXP		     /**<Exponentiation */
	, FCT_ATAN	     /**<Arctangent */
	, FCT_COS	     /**<Cosine */
	, FCT_COSH	     /**<Hyperbolic cosine */
	, FCT_EXP	     /**<Exponential function */
	, FCT_LOG	     /**<Logarithm (to base e)*/
	, FCT_SIN	     /**<Sine*/
	, FCT_SINH	     /**<Hyperbolic sine */
	, FCT_SQRT	     /**<Square root */
	, FCT_TAN	     /**<Tangent*/
	, NUM_INT	     /**<Integer */
	, NUM_DEC	     /**<Decimal */
	, NUM_CONST	      /**<Mathematical constant */
	, POSTFIX	      /**<Variable */
	, CONT_INTV	      /**<Continuous interval */
	, DISC_INTV	      /**<Discrete interval */
    , REAL_SPACE, LIST, NOTHING
};

enum statement_type { DECL, DEFI, DECLDEFI, ITER, COMP };

enum iteration_type { IT_FOR_NULL, IT_FOR_INC, IT_FOR_DEC };

enum function_type { IDENT, MIXED, NESTED, UNKN };



enum option_type { 
    OPTION_RANGE,
    OPTION_STATE_SPACE,
    OPTION_INPUT_SPACE,
    OPTION_OBSTACLESET,
    OPTION_TARGETSET,
    OPTION_INITIALSET,
    OPTION_PERIODS,
    OPTION_UNCERTAINTIES,
    OPTION_SAMPLINGTIME,
    OPTION_MEAS, 
    OPTION_INTEGR_ORDER,
    OPTION_INTEGR_ORDER_GROWTH,
    OPTION_INPUTSET
};

typedef struct expression *Identifier;
typedef struct expression *PostFixExpression;
typedef struct declaration *Declaration;
typedef struct expression *Expression;
typedef struct assignment *Assignment;
typedef struct function *FunctionDefinition;
typedef struct ode *Ode;
typedef struct option *Option;
typedef struct statement *Statement;
typedef struct iteration *Iteration;
typedef struct selection *Selection;
typedef unsigned int dim_t;


struct declaration {
    enum set_T type;	      /**< The left operand of a declaration is stored here. */
    PostFixExpression content;/**< Pointer to the right operand of the declaration. */
    Expression right;	      /**< If declaration includes an initialization then \p right points to the expression representing the initialization otherwise NULL. */
    unsigned int line;	      /**< Stores the line in which the declaration appears in the input file */
};

struct expression {
    enum expression_type type;	 /**< The type of information stored in struct expression is recorded here. */
    char *name;		     /**< The string of the identifier in the input file is copied in here. */
    struct expression *left;
    struct expression *right;
    void *content;
    int computable;
    size_t ad;
    struct expression *next; /**< If expression is an entry of a list then next points to the next expression in the list otherwise equals NULL. */
    struct expression *prev; /**< If expression is an entry of a list then prev points to the previous expression in the list otherwise equals NULL. */
    unsigned int line;	     /**< Stores the line in which the identifier appears in the input file */
};

struct assignment {
    unsigned short int type;
    PostFixExpression left;  /**< An assignment is always of the form <left> = <right> */
    Expression right;	     /**< An assignment is always of the form <left> = <right> */
    unsigned int line;	     /**< Stores the line in which the assignment appears in the input file */
};

struct function {
    Declaration head;
    enum function_type type;
    Expression var;    /**< Stores position 1 of functionhead */
    Expression dom;    /**< Stores position 3 of functionhead */
    Expression img;    /**< Stores position 5 of functionhead */
    Statement body;
    int line;	       /**< Stores the line in which the declaration of the function appears in the input file */
    dim_t *var_dim;    /**< Stores the dimensions of var, where the zeroth entry is the length of the list. */
    dim_t *img_dim;    /**< Stores the dimensions of img, where the zeroth entry is the length of the list. */
    dim_t *var_abs_dim;
    dim_t *img_acc_dim;
};

struct statement {
    enum statement_type type;
    void *ptr;
    struct statement *next;
    struct statement *prev;
};

struct iteration {
    enum iteration_type type;
    Identifier var;
    Expression start;
    Expression end;
    Statement body;
    unsigned int line;
};

struct ode {
    FunctionDefinition phi;
    Identifier time;
    Identifier var;
    Identifier rhs_var;
    Identifier rhs;	   /**< This is the identifier corresponding to the first function defined in the program */
    Statement initval;
    dim_t dim;		   /**< Dimension of the ode */
    unsigned int line;
};

struct option {
    Expression SamplingTime;
    Expression Periods;
    Expression Uncertainties;
    Expression MeasErrors;
    Expression Range;
    Expression StateSpaceDiscret;
    Expression InputSpaceDiscret;
    Expression ObstacleSet;
    Expression TargetSet;
    Expression InitialSet;
    Expression IntegrationOrder;
    Expression IntegrationOrderGrowth;
    Expression InputSet;
    unsigned int line;
};


struct ASTNode {
    Statement Prologue;
    FunctionDefinition Function;
    Ode ode;
    Option options;
};

void *my_malloc(size_t size);

Expression alloc_expression(void);
void *append(enum ASTNode_type type, void *op1, void *op2);
void lex_id(char s[]);

PostFixExpression build_PostFixExpression(Identifier in, Expression row,
					  Expression col);
void build_function(enum function_type type, FunctionDefinition in, Identifier op1, Expression op2,
		    Identifier op3);
Iteration build_iteration(Iteration head, Statement body);
Option build_option(enum option_type type, Option in, void *op1);
Ode build_ode_statement(Ode in, Statement s);
Identifier install_Identifier(char s[]);
Iteration install_iteration(enum iteration_type type, Identifier op1, Expression op2,
			    Expression op3);
struct ASTNode *install_program(Statement op1, FunctionDefinition op2, Ode op3, Option op4);
Expression install_expression(enum expression_type type, void *op1, void *op2);
Declaration install_declaration(enum set_T type, PostFixExpression in);
Declaration install_declaration_and_definition(enum set_T type, Identifier op1, Expression op2);
Expression install_Number(PostFixExpression in, enum expression_type type, char s[]);
Assignment install_assignment(unsigned short int type, PostFixExpression op1, Expression op2);
Statement install_statement(enum statement_type type, void *in);
FunctionDefinition install_function(Identifier in);
Expression install_Number_int(int s);
Ode install_ode_statement(Identifier op1, Identifier op2, Identifier op3, Identifier op4);
Option install_option(void);

FunctionDefinition finish_function(FunctionDefinition in, Statement op1);


void *wind_back(enum ASTNode_type type, void *in);

int string_to_int(const char *s, unsigned int line);


extern Identifier current_id;
extern struct ASTNode *AST;
extern FunctionDefinition current_fct;
extern Option current_opt;
#endif
/** \mainpage Documentation to the implementation of the language
 * 
 * 
*/
