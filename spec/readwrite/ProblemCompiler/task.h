/**
 * @file task.h
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 14 Nov 2016
 * @brief The task data structure.
*/
#ifndef TASK_H
#define TASK_H
#include <stdint.h>
#include "ast.h"
#include "variables.h"


#define FILENAME_EXTENSION "abcs"

typedef unsigned short int order_t;

#define SET_VERBOSE(X)    ((X)->bits |= 1 )
#define SET_NOCLEAN(X)    ((X)->bits |= (1<<1) )
#define SET_QUIET(X)      ((X)->bits |= (1<<2) )
#define SET_TRI(X)        ((X)->bits |= (1<<3) )
#define SET_MMA_SYM(X)    ((X)->bits |= (1<<4) )
#define SET_KEEPDEC(X)    ((X)->bits |= (1<<5) )
#define SET_ABCS(X)       ((X)->bits |= (1<<6) )
#define SET_GAPPA(X)      ((X)->bits |= (1<<7) )
#define SET_ONLYSCREEN(X) ((X)->bits |= (1<<8) )
#define SET_WITHOUTODE(X) ((X)->bits |= (1<<9) )
#define SET_CODOMAIN(X)   ((X)->bits |= (1<<10) )
#define SET_NOMMAFILE(X)  ((X)->bits |= (1<<11) )

#define IS_VERBOSE(X)     ((X)->bits & 1 )
#define IS_NOCLEAN(X)     ((X)->bits >> 1 & 1 )
#define IS_QUIET(X)       ((X)->bits >> 2 & 1 )
#define IS_TRI(X)         ((X)->bits >> 3 & 1 )
#define IS_MMA_SYM(X)     ((X)->bits >> 4 & 1 )
#define IS_KEEPDEC(X)     ((X)->bits >> 5 & 1 )
#define IS_ABCS(X)        ((X)->bits >> 6 & 1 )
#define IS_GAPPA(X)       ((X)->bits >> 7 & 1 )
#define IS_ONLYSCREEN(X)  ((X)->bits >> 8 & 1 )
#define IS_WITHOUTODE(X)  ((X)->bits >> 9 & 1 )
#define IS_CODOMAIN(X)    ((X)->bits >> 10 & 1 )
#define IS_NOMMAFILE(X)   ((X)->bits >> 11 & 1 )

enum task_type { TASK_METHOD_RECURSIVE,	/**< This method uses ad with respect to state. It has exponential run time in the order. Not recommended for use! */
    TASK_METHOD_QUADRATIC,		 /**< This method uses ad with respect to time,  see e.g. JorbaZou05. It implements the ad formulars by introducing one variable per term (in other words, it unrolls loops). It has quadratic run time in the order. */
    TASK_METHOD_LOOPS,			  /**< Same as ::TASK_METHOD_QUADRATIC but implements the ad formulars as loops. */
    TASK_JACOBIAN,		 /**< This method computes the Jacobian of the right hand side. */
    TASK_FUNCTION /**< This method is active if no differential equation is specified. */
};

struct task {
    dim_t states_len; /**< This is the variable \f$n\f$ in the denotational semantics for \f$\verb|ode_equation|\f$, or equivalently, in assumption \f$(A_3)\f$. */
    struct variable **states; /**< Points to an array, where the locations of \f$\sigma(x_1),\ldots,\sigma(x_n)\f$ are stored. Here, the notation is as in the denotational semantics for \f$\verb|ode_equation|\f$ and \f$\sigma\f$ is the final state of the program. */
    dim_t outputstates_len; /**< This is the variable \f$n'\f$ in assumption \f$(A_3)\f$. */
    dim_t *outputstates;    /**< Similar to second entry. */
    dim_t *acc_dim;	    /**< \f$\sum_{i=1}^n \sigma(x_i)_\mathrm{rowdim}\f$. Notation as in ::job::states. */
    char **coeff_strings;   /**< The names of the coefficient variables in any output file are stored here. */
    char **out_strings;	    /**< The names of the output variables in any output file are stored here. */
    enum task_type method; /**< The method for generating the Taylor coefficients. */
    order_t max_order;	    /**< The order up to which Taylor coefficients are generated. */
    order_t max_order_growth_bound; /**< The order up to which Taylor coefficients are generated for the growth bound. */
    char *coeff_function_name;
			    /**< The name of the coefficient function in the c code. */
    uint16_t bits;		/**< Bits whose functionality are specified in this file. */
    char *filename;
    char *output_directory;
    char *timevar_name;
    char *statevar_name;
    char *inputvar_name;

    char *ada_type_statevar;
    char *ada_type_inputvar;

    char *gappa_input_assumptions_file;

    double ** Bounds_of_out_rounding_errors;

    unsigned int num_of_initialsets;
    unsigned int num_of_targetsets;
    unsigned int num_of_obstaclesets;
    unsigned int num_of_input_signals;
    dim_t num_of_coord_with_periods;
    dim_t state_space_dimension;
    dim_t input_space_dimension;

    double   samplingtime;
    double   samplingtime_RU;

    double * apriori_enclosure;
    double * Bounds_of_Dynamic_Uncertainties;
    double * Bounds_of_Measurement_Errors;
    double * Rounded_Initial_State_Radius;
    double * Bounds_of_Lipschitz_Matrices;
    double * Bounds_of_Approximation_Error_General_Solution;   
    double * Bounds_of_Approximation_Error_Growth_Bound;
    double * Bounds_of_Rounding_Error_General_Solution;
    double * Bounds_of_Rounding_Error_Growth_Bound;
    double * Bounds_of_Input_Value_Rounding_Error;
    double * Bounds_of_Center_of_Cells_Rounding_Error;
    double * Bounds_of_Overapproximation_Radius;
    double * Bounds_of_Overapproximation_Rounding_Error;
    double * Bounds_of_Summation_Error_General_Solution;
    double * Bounds_of_Summation_Error_Growth_Bound;
    
    double * Growth_Bound_Codomain;
    double * General_Solution_Formula_Codomain;
};

void do_the_task(struct ASTNode *in, struct task *, ConstSequenceOfStates);

#endif
