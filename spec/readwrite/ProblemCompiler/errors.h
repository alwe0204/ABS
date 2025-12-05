/** @file
 *  @author Alexander Weber (a.weber@unibw.de)
 * @brief This file contains all error messages that might be produced by a faulty input file or if no memory is available anymore.
*/
#ifndef ERRORS_H
#define ERRORS_H
#include <stdio.h>
#include <stdlib.h>

#define BAD_ERROR { fprintf(stderr,"Error in %s line %d\n",__FILE__,__LINE__); exit(EXIT_FAILURE); }

#define NO_MEM(ptr) if( NULL == ptr ) { fprintf(stderr,"No memory (%s).\n",__func__); exit(EXIT_FAILURE); }

#define ERROR_INVALID_ORDER(X,Y) { fprintf(stderr,"Invalid order (%lu). It shall be less than %u",X,Y); exit(EXIT_FAILURE); }

#define ERROR_INVALID_INPUTSTRING(X,Y) { fprintf(stderr,"Line %d: error: Identifier \'%s\' exceeds %d characters\n",yylineno,X,(Y)-1); exit(EXIT_FAILURE); }

#define ERROR_DECL(X,Y)              { fprintf(stderr,"Line %d: error: Multiple declarations of \'%s\'\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_IDX_NOT_POSITIVE(X,Y)      { fprintf(stderr,"Line %d: error: Indices of \'%s\' are not positive integers.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_IDX_NOT_NONNEGATIVE(X,Y)       { fprintf(stderr,"Line %d: error: Indices of \'%s\' are not non-negative integers.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_IDX_NOT_INT(X,Y)       { fprintf(stderr,"Line %d: error: Expression is not an integer.\n",(Y)); exit(EXIT_FAILURE); }

#define ERROR_UNDECL(X,Y)            { fprintf(stderr,"Line %d: error: \'%s\' was not declared.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_UNBOUNDED(X,Y)         { fprintf(stderr,"Line %d: error: \'%s\' is unbounded.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_NOT_INTERVAL(Y)         { fprintf(stderr,"Line %d: error: Interval not well-defined.\n",(Y)); exit(EXIT_FAILURE); }

#define ERROR_UNBOUNDED_ROW_COL(X,A,B,Y)         { fprintf(stderr,"Line %d: error: \'%s[%u][%u]\' is unbounded.\n",(Y),(X),(A),(B)); exit(EXIT_FAILURE); }

#define ERROR_IDX(X,Y)             { fprintf(stderr,"Line %d: error: Invalid reference to component of \'%s\' .\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_DECL_NOT_INTERVAL(X,Y) { fprintf(stderr,"Line %d: error: \'%s\' is not declared as \'Interval\'.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_DEFINED(X,Y)           { fprintf(stderr,"Line %d: error: \'%s\' was defined previously.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_DEFINE_INPUT(X,Y)     { fprintf(stderr,"Line %d: error: Writing on an input variable (\'%s\').\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_DECL_NOT_REAL(X,Y)     { fprintf(stderr,"Line %d: error: \'%s\' is not declared as \'Real\'.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_NOT_SCALAR(X,Y) { fprintf(stderr,"Line %d: error: \'%s\' is not 1-dimensional.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_NOT_ONE_DIMENSION(Y) { fprintf(stderr,"Line %d: error: Entry shall be 1-dimensional.\n",(Y)); exit(EXIT_FAILURE); }

#define ERROR_NOT_OF_ROLE_INPUT(X,Y) { fprintf(stderr,"Line %d: error: The role of \'%s\' is not \'Input\'.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_DIMENSION_MISMATCH(X,Y) { fprintf(stderr,"Line %d: error: The setmembership relation in function \'%s\' is not well-defined (dimension mismatch).\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_LENGTH_MISMATCH(X,Y) { fprintf(stderr,"Line %d: error: The setmembership relation in function \'%s\' is not well-defined (length mismatch).\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_DIV_BY_ZERO(Y) { fprintf(stderr,"Line %d: error: Integer division by 0.\n",(Y)); exit(EXIT_FAILURE); }

#define ERROR_INT_OVERFLOW(Y) { fprintf(stderr,"Line %d: error: Overflow in integer operation.\n",(Y)); exit(EXIT_FAILURE); }

#define ERROR_DEC_OVERFLOW(Y) { fprintf(stderr,"Line %d: error: Overflow in decimal number operation.\n",(Y)); exit(EXIT_FAILURE); }

#define ERROR_DEC_NOT_FINITE(Y)         { fprintf(stderr,"Line %d: error: Decimal number is not finite.\n",(Y)); exit(EXIT_FAILURE); }

#define ERROR_INT_EXPONENTIATION(Y) { fprintf(stderr,"Line %d: error: Integer exponentiation with negative integer.\n",(Y)); exit(EXIT_FAILURE); }

/* * */

#define ERROR_NOT_VECTOR(X,Y)        { fprintf(stderr,"Line %d: error: \'%s\' not a vector.\n",(Y),(X)); exit(EXIT_FAILURE);  }

#define ERROR_DEFINE_INTERVAL(X,Y)   { fprintf(stderr,"Line %d: error: \'%s\' is not defined as an interval.\n",(Y),(X)); exit(EXIT_FAILURE);  }

#define ERROR_ODE_EQUATION_INPUT_OUTPUT_DIMENSION(X,Y)  { fprintf(stderr,"Line %d: error: More states than inputs to \'%s\'.\n",(Y),(X)); exit(EXIT_FAILURE);  }

#define ERROR_ODE_EQUATION_DECL_NOT_FUNC(X,Y) { fprintf(stderr,"Line %d: error: \'%s\' is not declared as \'Function\'.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_ODE_EQUATION_RHS(X,Y) { fprintf(stderr,"Line %d: error: \'%s\' is not a valid right hand side.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_ODE(X,Y) { fprintf(stderr,"Line %d: error: Invalid specification of an ordinary differential equation in \'%s\' .\n",(Y),(X)); exit(EXIT_FAILURE);  }

#define ERROR_ODE_TIME_VARIABLE(X,Y) { fprintf(stderr,"Line %d: error: The time variable \'%s\' is unbounded.\n",(Y),(X)); exit(EXIT_FAILURE);  }

#define ERROR_ODE_NO_OUTPUT(X,Y) { fprintf(stderr,"Line %d: error: No state is an output for the solution \'%s\' for the ordinary differential equation.\n",(Y),(X)); exit(EXIT_FAILURE);  }

#define ERROR_ODE_OUTPUT_NOT_STATE(X,Y) { fprintf(stderr,"Line %d: error: The output \'%s\' is not a state of the ordinary differential equation.\n",(Y),(X)); exit(EXIT_FAILURE);  }

#define ERROR_ODE_NO_INPUT(Y) { fprintf(stderr,"Line %d: error: The ordinary differential equation does not have an input variable.\n",(Y)); exit(EXIT_FAILURE);  }

#define ERROR_ODE_NO_INITIAL(X,Y) { fprintf(stderr,"Line %d: error: No initial value specified for \'%s\' .\n",(Y),(X)); exit(EXIT_FAILURE);  }

#define ERROR_ODE_ARG(X,Y) { fprintf(stderr,"Line %d: error: Invalid arguments to the right hand side \'%s\'.\n",(Y),(X)); exit(EXIT_FAILURE);  }

#define ERROR_ODE_IDX_MISMATCH(X,Y,Z,W) { fprintf(stderr,"Line %d: error: The number of output variables of the right hand side (%d) must coincide with the number of states in the ordinary differential equation (%d).\n",(W),(Y),(Z)); exit(EXIT_FAILURE);  }

#define ERROR_INIT_VAL(X,Y) { fprintf(stderr,"Line %d: error: Invalid specification for the initial value near \'%s\' .\n",(Y),(X)); exit(EXIT_FAILURE);  }

#define WARNING_UNUSED(X) { fprintf(stderr,"WARNING: Variable \'%s\' unused.\n",(X)) ; }

#define ERROR_OUTPUT_KEYWORD_OF_C(X) { fprintf(stderr,"Output error: The input file contains the identifier \'%s\' which is a keyword of the C programming language. Rename \'%s\' and try again.\n",(X),(X)); }

#define ERROR_NO_APRIORI(X) { fprintf(stderr,"Line %d: error: An apriori enclosure cannot be computed.\n",(X)); exit(EXIT_FAILURE);  }
#define ERROR_NO_APRIORI_ATTEMPTS(X) { fprintf(stderr,"Line %d: error: An apriori enclosure cannot be computed. Try to increase number of attempts.\n",(X)); exit(EXIT_FAILURE);  }

#define ERROR_NO_APRIORI_RHS_UNBOUNDED(X) { fprintf(stderr,"Line %d: error: An apriori enclosure cannot be computed. The right hand side seems to be unbounded.\n",(X)); exit(EXIT_FAILURE);  }

#define ERROR_NO_APRIORI_RHS_NAN(X) { fprintf(stderr,"Line %d: error: An apriori enclosure cannot be computed. The right hand side seems to be undefined.\n",(X)); exit(EXIT_FAILURE);  }

#define ERROR_UNKNOWN_OPTION(X) { fprintf(stderr,"Unknown command-line option \'%s\'\n",(X)); exit(EXIT_FAILURE); }

#define ERROR_GAPPA { fprintf(stderr,"The tool Gappa returned an error.\n"); exit(EXIT_FAILURE); }

#define ERROR_GAPPA_GEN { fprintf(stderr,"Some serious problem with the tool gappa.\n"); exit(EXIT_FAILURE); }

#define ERROR_SYSTEM_CALL(X,Y) { fprintf(stderr,"%s returned error. Execution command was\n%s\n",(X),(Y)); exit(EXIT_FAILURE); }

#define ERROR_OPEN_FILE(X) { fprintf(stderr, "File %s cannot be opened.\n", (X)); exit(EXIT_FAILURE); }

#endif
