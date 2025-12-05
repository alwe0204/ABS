/**
 * @file main.c
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 02/02/2016
 * @brief This file contains the entry point of the program
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <sys/time.h>
#include "ast.h"
#include "task.h"
#include "semantics.h"
#include "semantics_abcs.h"
#include "ia.h"
#include "errors.h"

#define USEC_PER_SEC 1000000

extern int yyparse();
extern FILE *yyin;

struct task Task;
struct ASTNode *AST;
Identifier current_id;
FunctionDefinition current_fct;
Option current_opt;

static void gettimedifference(struct timeval *begin,struct timeval *end)
{
    long int usec,sec;
    usec = end->tv_usec-begin->tv_usec;
    sec = end->tv_sec-begin->tv_sec;
    if(usec < 0) 
    {
	usec += USEC_PER_SEC;
        sec--;
    }  
    end->tv_usec = usec;
    end->tv_sec = sec;
}

static float get_seconds(struct timeval *in)
{
    return (float)in->tv_sec+(float)in->tv_usec/USEC_PER_SEC;
}


int main(int argc, char *argv[])
{
    struct timeval time_at_start;
    struct timeval time_at_end;
    int i;
    FILE *inputfile = NULL;
    Task.bits = 0;
    Task.method = TASK_METHOD_QUADRATIC;
    Task.coeff_function_name = "taylorcoefficients";
    Task.timevar_name = "";
    Task.statevar_name = "";
    Task.inputvar_name = "";
    Task.ada_type_statevar = "Vector_Float_T";
    Task.ada_type_inputvar = "Vector_Float_T";
    Task.output_directory = ".";
    Task.gappa_input_assumptions_file = "";
    if (argc == 1) {
	fprintf(stdout, "*** Problem Compiler for ABS ***\n");
	fprintf(stdout, "Use: ./compiler <options> <input file>\n");
	fprintf(stdout, "Options for the user:\n");
	fprintf(stdout,
		"\t-codomain\tTries to compute a compact codomain of the explicitly specified function.\n");
	fprintf(stdout,
		"\t-gappa\t Computes the rounding error of the processed function for double precision floating-point arithmetic.\n");
	fprintf(stdout,
		"\t-Jacobian\tComputes the Jacobian of the right hand side of a differential equation.\n");
	fprintf(stdout,
		"\t-nofolding\tThe output of the three variable list is not a list of larger evaluation trees but an actual three variable list.\n");
	fprintf(stdout, "\t-nomma-file\tNo output file for Mathematica will be produced.\n");
	fprintf(stdout,
		"\t-nosimplify\tNo algebraic simplifications are applied on the evaluation tree.\n");
	fprintf(stdout,
		"\t-onlyscreen\tParses the input file and prints on screen the explicitly specified function. No output files are generated.\n");
	fprintf(stdout,
		"\t-order=<integer>\tSpecifies the order of the Taylor series to generate.\n");
	fprintf(stdout,
		"\t-outputdir=<name>\tSpecifies the directory for the output files. If not specified then it is \".\"\n");
	fprintf(stdout, "\t-quiet\tNo output on screen. No warning messages, too.\n");
	fprintf(stdout, "\t-recursive\tTaylor coefficients are computed recursively.\n");
	fprintf(stdout, "\t-v\tVerbose output on screen.\n");
	fprintf(stdout, "\t-o <name>\tThe coefficients will be called <name>.\n");
	fprintf(stdout,
		"\t-without-ode\t If an ordinary differential equation is specified in the input file it will be ignored and only the the explicitly specified function will be processed.\n");
	fprintf(stdout, "Options for internal use:\n");
	fprintf(stdout, "\t-mma-symbolic\n");
	fprintf(stdout,
		"\t-keepdec\tIf a variable in the three variable list is assigned to a decimal number then the variable is kept in the list. Enables -nofolding.\n");
	fprintf(stdout,
		"\t-timevar=<id>\tIn case of a polynomial in a variable t, specify the identifier for t. (Only relevant for ada output.)\n");
	fprintf(stdout, "\t-inputvar=<id>\tOnly relevant for ada output. Never use manually.\n");
	fprintf(stdout, "\t-statevar=<id>\tOnly relevant for ada output. Never use manually.\n");
	fprintf(stdout,
		"\t-statevar-ada-type=<id>.\tOnly relevant for ada output. Never use manually.\n");
	fprintf(stdout,
		"\t-inputvar-ada-type=<id>.\tOnly relevant for ada output. Never use manually.\n");
	return 0;
    }
    for (i = 1; i < argc; i++) {
	char *arg = argv[i];
	if (arg[0] == '-') {
	    switch (arg[1]) {
	    case 'c':
		if (!strncmp(arg, "-codomain", strlen("-codomain"))) {
		    arg += strlen("-codomain");
		    SET_CODOMAIN(&Task);
		} else {
		    ERROR_UNKNOWN_OPTION(arg);
		}
		break;
	    case 'g':
		if (!strncmp(arg, "-gappa-input-assumptions=", 25)) {
		    arg += 25;
		    Task.gappa_input_assumptions_file = arg;
		    arg += strlen(Task.gappa_input_assumptions_file);
		} else if (!strncmp(arg, "-gappa", 6)) {
		    arg += 6;
		    SET_TRI(&Task);
		    SET_GAPPA(&Task);
		} else {
		    ERROR_UNKNOWN_OPTION(arg);
		}
		break;
	    case 'i':
		if (!strncmp(arg, "-inputvar=", strlen("-inputvar="))) {
		    arg += strlen("-inputvar=");
		    Task.inputvar_name = arg;
		    arg += strlen(Task.inputvar_name);
		} else if (!strncmp(arg, "-inputvar-ada-type=", 19)) {
		    arg += 19;
		    Task.ada_type_inputvar = arg;
		    arg += strlen(Task.ada_type_inputvar);
		} else
		    ERROR_UNKNOWN_OPTION(arg);
		break;
	    case 'J':
		if (!strncmp(arg, "-Jacobian", 9)) {
		    arg += 9;
		    SET_TRI(&Task);
		    SET_KEEPDEC(&Task);
		    Task.max_order = 2;
		    Task.method = TASK_JACOBIAN;
		} else
		    ERROR_UNKNOWN_OPTION(arg);
		break;
	    case 'k':
		if (!strncmp(arg, "-keepdec", 8)) {
		    arg += 8;
		    SET_TRI(&Task);
		    SET_KEEPDEC(&Task);
		} else
		    ERROR_UNKNOWN_OPTION(arg);
		break;
	    case 'm':
		if (!strncmp(arg, "-mma-symbolic", 13)) {
		    arg += 13;
		    SET_MMA_SYM(&Task);
		} else
		    ERROR_UNKNOWN_OPTION(arg);
		break;
	    case 'n':
		if (!strncmp(arg, "-nomma-file", strlen("-nomma-file"))) {
		    arg += strlen("-nomma-file");
		    SET_NOMMAFILE(&Task);
		} else if (!strncmp(arg, "-nosimplify", strlen("-nosimplify"))) {
		    arg += strlen("-nosimplify");
		    SET_NOCLEAN(&Task);
		} else if (!strncmp(arg, "-nofolding", strlen("-nofolding"))) {
		    arg += strlen("-nofolding");
		    SET_TRI(&Task);
		} else
		    ERROR_UNKNOWN_OPTION(arg);
		break;
	    case 'O':
		if (!strncmp(arg, "-O", 2)) {
		    arg += 2;
		    Task.method = TASK_METHOD_QUADRATIC;
		} else
		    ERROR_UNKNOWN_OPTION(arg);
		break;
	    case 'o':
		if (!strncmp(arg, "-order=", 7)) {
		    arg += 7;
		    long int order = strtol(arg, NULL, 10);
		    if (order >= USHRT_MAX || order < 0) {
			ERROR_INVALID_ORDER(order, USHRT_MAX);
		    }
		    Task.max_order = (order_t) order;
		} else if (!strncmp(arg, "-onlyscreen", 11)) {
		    arg += 11;
		    SET_ONLYSCREEN(&Task);
		} else if (!strncmp(arg, "-outputdir=", 11)) {
		    arg += 11;
		    Task.output_directory = arg;
		    arg += strlen(Task.output_directory);
		    if (Task.output_directory[strlen(Task.output_directory) - 1] == '/')
			Task.output_directory[strlen(Task.output_directory) - 1] = '\0';
		} else if (!strncmp(arg, "-o", 2)) {
		    arg += 3;
		    Task.coeff_function_name = arg;
		    i++;
		} else
		    ERROR_UNKNOWN_OPTION(arg);
		break;
	    case 'q':
		if (!strncmp(arg, "-quiet", 6)) {
		    arg += 6;
		    SET_QUIET(&Task);
		} else
		    ERROR_UNKNOWN_OPTION(arg);
		break;
	    case 'r':
		if (!strncmp(arg, "-recursive", 10)) {
		    arg += 10;
		    Task.method = TASK_METHOD_RECURSIVE;
		} else
		    ERROR_UNKNOWN_OPTION(arg);
		break;
	    case 's':
		if (!strncmp(arg, "-statevar=", strlen("-statevar="))) {
		    arg += strlen("-statevar=");
		    Task.statevar_name = arg;
		    arg += strlen(Task.statevar_name);
		} else if (!strncmp(arg, "-statevar-ada-type=", 19)) {
		    arg += 19;
		    Task.ada_type_statevar = arg;
		    arg += strlen(Task.ada_type_statevar);
		} else
		    ERROR_UNKNOWN_OPTION(arg) break;
	    case 't':
		if (!strncmp(arg, "-timevar=", 9)) {
		    arg += 9;
		    Task.timevar_name = arg;
		    arg += strlen(Task.timevar_name);
		} else
		    ERROR_UNKNOWN_OPTION(arg) break;
	    case 'v':
		if (!strncmp(arg, "-v", 2)) {
		    arg += 2;
		    SET_VERBOSE(&Task);
		} else
		    ERROR_UNKNOWN_OPTION(arg);
		break;
	    case 'w':
		if (!strncmp(arg, "-without-ode", 12)) {
		    arg += 12;
		    SET_WITHOUTODE(&Task);
		} else
		    ERROR_UNKNOWN_OPTION(arg) break;
		break;
	    default:
		ERROR_UNKNOWN_OPTION(arg);
		break;
	    }
	} else {
	    inputfile = fopen(arg, "r");
	    if (inputfile == NULL) {
		ERROR_OPEN_FILE(arg);
	    }
	    Task.filename = arg;
	    yyin = inputfile;
	}
    }
    
    gettimeofday ( &time_at_start,0 );
    
    SequenceOfStates S = install_SequenceOfStates();
    current_opt = install_option();
    yyparse();
    semantics_initial_statement_list(AST, S);
    semantics_function_definition(AST, S);
    set_date_of_rhs(S);
    check_output_defined(S);
    if (!IS_WITHOUTODE(&Task)) {
	if (AST->options == NULL) {
	    semantics_ode_definition(AST, S);
	} else {
	    semantics_Option(AST, &Task, S);
	    semantics_ode_definition(AST, S);
	}
	check_existence_of_solution(AST, &Task, S);
    }
    do_the_task(AST, &Task, S);
    if (inputfile != NULL)
	fclose(inputfile);

    if (IS_ABCS(&Task) && !IS_WITHOUTODE(&Task)) {
        gettimeofday ( &time_at_end,0 );
        gettimedifference ( &time_at_start, &time_at_end );
        fprintf(stdout, "Done. Time: %.5E sec.\n", get_seconds( &time_at_end ) );
    }

    return 0;
}
