/** @file 
 *@brief Header for to_string.h
 *@author Alexander Weber (a.weber@unibw.de)
 *@date 20 Apr 2016
*/
#ifndef TO_STRING_H
#define TO_STRING_H

#include "ast.h"
#include "variables.h"

#define GAPPA_ROUNDING_KEYWORD "double"

#define ADA_DOUBLE_TYPE "Float_T"

extern char *string_of_mathfunctions[];
extern char *string_of_mmamathfunctions[];

enum number_format {
    NUMERICAL /**< All constant variables are replaced by their numerical values */ ,
    SYMBOLIC /**< All variables denote themselves*/
};
/** Defines in which format an expression should be transformed to a string. */
enum string_format {
    ADASTRING /**< All strings are in written in ada form */ ,
    CSTRING /**< All strings are in written in C form */ ,
    MMASTRING
	/**< All strings are written in Mathematica form and if \f$x \in \mathbb{R}\f$ then as "x[[1]]"*/
	,
    MMAPRINTSTRING
	/**< All strings are written in Mathematica form and if \f$x \in \mathbb{R}\f$ then as "x"*/
	,
    MYSTRING,
    GAPPAMATHSTRING,
    GAPPAFLOATSTRING
};

enum output_format {
    MY_FORMAT /**< The format of this software without using loops */,
    MY_FORMAT_WITH_LOOPS /**< The format of this software with using loops */,
    ADA_FORMAT /**< Ada source code without using loops */,
    C_FORMAT /**< C source code without using loops */,
    C_FORMAT_WITH_LOOPS /**< C source code using loops */,
    C_IA_FORMAT /**< C source code using interval arithmetic */,
    MMA_FORMAT /**< The format of Wolfram Mathematica */,
    GAPPA_FORMAT /**< The format of Gappa for floating-point arithmetic */,
    GAPPA_MATH_FORMAT /**< The format of Gappa for real arithmetic */
};

typedef enum output_format FORMAT;

char *int_to_string(int, enum number_format);	/* Due to ada we need the argument of type number_format */
char *double_to_string(double, enum string_format);

char *vector_entry_to_string_int(const char *name, dim_t a, enum string_format);
char *matrix_entry_to_string_int(const char *name, dim_t a, dim_t b, enum string_format);
char *array_of_array_entry_to_string_int(const char *name, dim_t a, dim_t b, enum string_format);

enum string_format format_to_string_format(FORMAT);
char *string_of_assignment(FORMAT);

char *function_to_string(const char *left, const char *rght, enum expression_type, FORMAT);
char *Expression_to_string(Expression, enum number_format mode,
			   enum string_format, ConstSequenceOfStates);
char *interval_to_string(Expression, enum string_format, ConstSequenceOfStates);
char *_string_of_function(enum expression_type, enum string_format);
char * data_of_identifier_to_hyperinterval_string(Variable x,enum number_format mode,
			   enum string_format f, ConstSequenceOfStates S);
#endif
