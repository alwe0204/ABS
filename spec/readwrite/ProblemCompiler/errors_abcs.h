/** @file
 *  @author Alexander Weber (a.weber@unibw.de)
 * @brief This file contains all error messages that might be produced by a faulty specification of the control task or the parameters of the abstraction
*/
#ifndef ERRORS_ABCS_H
#define ERRORS_ABCS_H

#define ERROR_MULTIPLE_DEFINITIONS(Y) { fprintf(stderr,"Line %d: error: Multiply defined quantity.\n",(Y)); exit(EXIT_FAILURE); }

#define ERROR_RHS_FOR_ABCS { fprintf(stderr,"error: The dynamics (without uncertainties) must be of the form x' = f(x,u).\n"); exit(EXIT_FAILURE); }

#define ERROR_NO_SAMPLING_TIME { fprintf(stderr,"error: No sampling time specified.\n"); exit(EXIT_FAILURE);  }
#define ERROR_BAD_SAMPLING_TIME(Y) { fprintf(stderr,"Line %d: error: Illegal specification of the sampling time.\n",(Y)); exit(EXIT_FAILURE); }

#define ERROR_INPUT_NOT_IN_INPUTSET(X,Y) { fprintf(stderr,"Line %d: error: It cannot be verified whether the input signal %u is in the input set.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_NOT_POS_INTEGER(Y) { fprintf(stderr,"Line %d: error: Positive integer expected.\n",(Y)); exit(EXIT_FAILURE); }

#define ERROR_NOT_POS_INTEGERS(Y) { fprintf(stderr,"Line %d: error: List of positive integers expected.\n",(Y)); exit(EXIT_FAILURE); }

#define ERROR_NOT_ORDERED(Y) { fprintf(stderr,"Line %d: error: Ordered list expected.\n",(Y)); exit(EXIT_FAILURE); }

#define ERROR_LIST_TOO_LONG(X,Y) { fprintf(stderr,"Line %d: error: A list with at most %u component(s) expected.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_LIST_BAD_LENGTH(X,Y) { fprintf(stderr,"Line %d: error: A list with %u component(s) expected.\n",(Y),(X)); exit(EXIT_FAILURE); }

#define ERROR_NO_INPUT_SIGNAL { fprintf(stderr,"error: No input signal specified.\n"); exit(EXIT_FAILURE);  }

#define ERROR_NO_OPERATINGRANGE { fprintf(stderr,"error: No operating range specified.\n"); exit(EXIT_FAILURE);  }
#define ERROR_NO_GRIDDISCRETIZATION { fprintf(stderr,"error: No grid discretization specified.\n"); exit(EXIT_FAILURE);  }

#define ERROR_NO_INITIALSET { fprintf(stderr,"error: No initial set specified.\n"); exit(EXIT_FAILURE);  }
#define ERROR_NO_TARGETSET { fprintf(stderr,"error: No target set specified.\n"); exit(EXIT_FAILURE);  }

#define ERROR_INITIAL_SET_NOT_IN_OPERATING_RANGE(Y) { fprintf(stderr,"Line %d: error: Initial set not contained in the operating range.\n",(Y)); exit(EXIT_FAILURE);  }

#define ERROR_UNCERT_MEAS(Y) { fprintf(stderr,"Line %d: error: Uncertainties or measurement errors not correctly specified.\n",(Y)); exit(EXIT_FAILURE);  }

#define ERROR_INTERVAL(Y) { fprintf(stderr,"Line %d: error: Illegal specification of an interval.\n",(Y)); exit(EXIT_FAILURE);  }

#define ERROR_RESERVED_VAR { fprintf(stderr,"error: The identifier \'t\' is reserved. It cannot be used elsewhere.\n"); exit(EXIT_FAILURE);  }

#define ERROR_RESERVED_TIME_VAR { fprintf(stderr,"error: The identifier \'t\' is reserved for the time parameter. It cannot be used elsewhere.\n"); exit(EXIT_FAILURE);  }

#define ERROR_USERINPUT_ORDER { fprintf(stderr, "Your input couldn't be read. Please start over again.\n"); exit(EXIT_FAILURE); }

#define ERROR_ROUNDED_INPUTSET_EMPTY(Y) { fprintf(stderr,"Line %d: error: The input set cannot be represent on finite arithmetic.\n",(Y)); exit(EXIT_FAILURE);  }

#endif
