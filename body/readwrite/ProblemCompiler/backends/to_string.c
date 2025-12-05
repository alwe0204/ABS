/** @file 
 *@author Alexander Weber (a.weber@unibw.de)
 *@data 20 Apr 2016
 *@brief Functions to generate strings from expressions
*/
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "to_string.h"

#include "my_strcat.h"
#include "errors.h"
#include "expressions.h"
#include "reduce_int.h"
#include "task.h"
#include "output_aux.h"

extern struct task Task;

char *string_of_mathfunctions[] =
    { "atan", "cos", "cosh", "exp", "ln", "sin", "sinh", "sqrt", "tan",
    "plus", "minus", "times", "div", "pow"
};

char *string_of_cfunctions[] = { "atan", "cos", "cosh", "exp", "log", "sin", "sinh", "sqrt", "tan",
    "plus", "minus", "times", "div", "pow"
};

char *string_of_adafunctions[] = { "ABSLib_atan", "ABSLib_cos", "ABSLib_cosh", "ABSLib_exp", "ABSLib_log", "ABSLib_sin", "ABSLib_sinh", "ABSLib_sqrt", "ABSLib_tan",
    "plus", "minus", "times", "div", "pow"
};

char *string_of_mmamathfunctions[] =
    { "ArcTan", "Cos", "Cosh", "Exp", "Log", "Sin", "Sinh", "Sqrt", "Tan",
    "Plus", "Minus", "Times", "Divide", "Pow"
};


/** 
 * @param type One of ::PLUS, ::MINUS, ::TIMES, ::DIV, ::POW, ::EXP.
 * @brief Returns the character representing the elementary operation specified by \p type. 
 * Throws an error if \p type is not as below.
*/
char *string_of_elementary_operation(enum expression_type type)
{
    char *buf = "";
    switch (type) {
    case PLUS:
	my_strcat(&buf, "+");
	break;
    case MINUS:
	my_strcat(&buf, "-");
	break;
    case TIMES:
	my_strcat(&buf, "*");
	break;
    case DIV:
	my_strcat(&buf, "/");
	break;
    case POW:
    case EXP:
	my_strcat(&buf, "^");
	break;
    default:
	assert(0);
	break;
    }
    return buf;
}

/** 
 * @param type One of ::FCT_ATAN, ::FCT_COS, ::FCT_COSH, ...
 * @param f    The format to use 
 * @returns A newly allocated string representing the function name specified by \p type in the format \p f, or \c NULL if \p type is not as below. 
*/
char *_string_of_function(enum expression_type type, enum string_format f)
{
    char *out = "";
    char **in;
    int i;
    switch (f) {
    case MMAPRINTSTRING:
    case MMASTRING:
	in = string_of_mmamathfunctions;
	break;
    case CSTRING:
        in = string_of_cfunctions;
	break;
    case ADASTRING:
	in = string_of_adafunctions;
	break;
    default:
	in = string_of_mathfunctions;
	break;
    }
    switch (type) {
    case FCT_ATAN:
	i = 0;
	break;
    case FCT_COS:
	i = 1;
	break;
    case FCT_COSH:
	i = 2;
	break;
    case FCT_EXP:
	i = 3;
	break;
    case FCT_LOG:
	i = 4;
	break;
    case FCT_SIN:
	i = 5;
	break;
    case FCT_SINH:
	i = 6;
	break;
    case FCT_SQRT:
	i = 7;
	break;
    case FCT_TAN:
	i = 8;
	break;
    case PLUS:
	i = 9;
	break;
    case MINUS:
	i = 10;
	break;
    case TIMES:
	i = 11;
	break;
    case DIV:
	i = 12;
	break;
    case EXP:
    case POW:
	i = 13;
	break;
    default:
	return NULL;
	break;
    }
    my_strcat(&out, in[i]);
    return out;
}

/** @brief The is the mapping output format to string format 
 * @param f The output format
 * @returns The string format usually corresponding to \p f
*/

enum string_format format_to_string_format(FORMAT f)
{
    enum string_format style;
    switch (f) {
    case ADA_FORMAT:
	style = ADASTRING;
	break;
    case GAPPA_FORMAT:
	style = GAPPAFLOATSTRING;
	break;
    case GAPPA_MATH_FORMAT:
	style = GAPPAMATHSTRING;
	break;
    case MY_FORMAT:
	style = MYSTRING;
	break;
    case MMA_FORMAT:
	style = MMASTRING;
	break;
    default:
	style = CSTRING;
	break;
    }
    return style;
}

/** @returns A string with the assignment operator in the output format \p f is returned.
 * @param f Every output format except ::C_IA_FORMAT
 */

char *string_of_assignment(enum output_format f)
{
    char *buf = "";
    switch (f) {
    case ADA_FORMAT:
	my_strcat(&buf, ":=");
	break;
    case GAPPA_FORMAT:
	my_strcat_several(&buf, 2, GAPPA_ROUNDING_KEYWORD, "=");
	break;
    case C_IA_FORMAT:
	assert(0);
	break;
    default:
	my_strcat(&buf, "=");
	break;
    }
    return buf;
}

/** 
 * @brief Returns a newly allocated string representing the term \f$f(a,b)\f$ respectively \f$f(a)\f$.
 * @param left        \f$a\f$
 * @param right       \f$b\f$
 * @param type        The function type of \f$f\f$
 * @param f           The format to use
 * @param no_brackets If equal to 0 then the value of an elementary operation is enclosed in brackets, e.g. the string equals (a+b)
 *
 * @todo Implementation for gappa is not complete
*/
char *function_to_string_core(const char *left, const char *rght,
			      enum expression_type type, enum string_format f, int no_brackets)
{
    char *buf = "";
    char *ch = "";
    char mode = 0;
    switch (f) {
    case MMASTRING:
	{
	    if (is_Expression_type_elementary_operation(type)) {
		mode = 1;
	    } else if (is_Expression_type_function(type)) {
		mode = 2;
	    }
	}
	break;
    case ADASTRING:
	{
	    if (type == EXP || type == POW) {
		mode = 5;
	    } else if (is_Expression_type_elementary_operation(type)) {
		mode = 1;
	    } else if (is_Expression_type_function(type)) {
		mode = 7;
	    } else
		assert(0);
	}
	break;
    case CSTRING:
	{
	    if (type == EXP || type == POW) {
		mode = 3;
	    } else if (is_Expression_type_elementary_operation(type)) {
		mode = 1;
	    } else if (is_Expression_type_function(type)) {
		mode = 2;
	    } else
		assert(0);
	}
	break;
    case GAPPAFLOATSTRING:
	{
	    if (type == POW) {
		mode = 6;
	    } else if (type == EXP) {
		assert(0);
	    } else if (type == FCT_SQRT) {
		mode = 2;
	    } else if (type == FCT_SIN || type == FCT_COS || type == FCT_TAN || type == FCT_EXP
		       || type == FCT_LOG || type == FCT_ATAN || type == FCT_COSH
		       || type == FCT_SINH) {
		mode = 4;
	    } else if (is_Expression_type_elementary_operation(type)) {
		mode = 1;
	    } else if (is_Expression_type_function(type)) {
		assert(0);
	    } else
		assert(0);
	}
	break;
    case GAPPAMATHSTRING:
	{
	    if (type == POW) {
		mode = 6;
	    } else if (type == EXP) {
		assert(0);
	    } else if (type == FCT_SQRT) {
		mode = 2;
	    } else if (type == FCT_SIN || type == FCT_COS || type == FCT_TAN || type == FCT_EXP
		       || type == FCT_LOG || type == FCT_ATAN || type == FCT_COSH
		       || type == FCT_SINH) {
		mode = 4;
	    } else if (is_Expression_type_elementary_operation(type)) {
		mode = 1;
	    } else if (is_Expression_type_function(type)) {
		assert(0);
	    } else
		assert(0);
	}
	break;
    default:
	assert(0);
    }
    switch (mode) {
    case 1:
	buf = "";
	ch = string_of_elementary_operation(type);
	if (no_brackets)
	    my_strcat_several(&buf, 5, left, " ", ch, " ", rght);
	else
	    my_strcat_several(&buf, 7, "(", left, " ", ch, " ", rght, ")");
	free(ch);
	break;
    case 2:
	ch = _string_of_function(type, CSTRING); 
	my_strcat_several(&buf, 4, ch, "(", rght, ")");
	free(ch);
	break;
    case 3:
	my_strcat_several(&buf, 6, "pow", "(", left, ",", rght, ")");
	break;
    case 4:
	ch = _string_of_function(type, CSTRING);
	my_strcat_several(&buf, 3, ch, "_", rght);
	free(ch);
	break;
    case 5:
	my_strcat_several(&buf, 5, left, "**", "(", rght, ")");
	break;
    case 6:
	{
	    int i;
	    int expo = (int) strtol(rght, NULL, 10);
	    my_strcat(&buf, "1");
	    if (expo >= 0) {
		for (i = 1; i <= expo; i++) {
		    my_strcat_several(&buf, 2, "*", left);
		}
	    } else {
		for (i = 1; i <= -expo; i++) {
		    my_strcat_several(&buf, 2, "/", left);
		}
	    }
	}
	break;
     case 7:
	ch = _string_of_function(type, f); 
	my_strcat_several(&buf, 4, ch, "(", rght, ")");
	free(ch);
	break;
    default:
	assert(0);
    }
    return buf;
}


/** @brief Wrapper for function_to_string_core(). 
 * The function involves the global variable task()
 * @param left See function_to_string_core()
 * @param rght See function_to_string_core()
 * @param type See function_to_string_core()
 * @param f The output format
 */

char *function_to_string(const char *left, const char *rght, enum expression_type type, FORMAT f)
{
    enum string_format style;
    switch (f) {
    case MY_FORMAT:
    case MY_FORMAT_WITH_LOOPS:
    case MMA_FORMAT:
	style = MMASTRING;
	break;
    case ADA_FORMAT:
	style = ADASTRING;
	break;
    case C_FORMAT_WITH_LOOPS:
    case C_FORMAT:
	style = CSTRING;
	break;
    case GAPPA_MATH_FORMAT:
	style = GAPPAMATHSTRING;
	break;
    case GAPPA_FORMAT:
	style = GAPPAFLOATSTRING;
	break;
    default:
	assert(0);
    }
    return function_to_string_core(left, rght, type, style, IS_TRI(&Task));
}


/** @returns A string with the decimal representation of the integer \p k is returned. 
 * If \p mode is ::NUMERICAL then the string contains a decimal point and a zero 
 * to the right of the decimal point. 
 * Otherwise an ordinary integer representation is returned. 
 * @param k The integer
 * @param mode The number format; One of ::SYMBOLIC, ::NUMERICAL
*/

char *int_to_string(int k, enum number_format mode)
{
    char *a = NULL;
    size_t num_of_digits = (size_t) ceil(log10(1 + fabs(k))) + 2;
    switch (mode) {
    case SYMBOLIC:
	a = (char *) malloc(num_of_digits * sizeof(char));
	sprintf(a, "%d", k);
	break;
    case NUMERICAL:
	a = (char *) malloc((2 + num_of_digits) * sizeof(char));
	sprintf(a, "%d.0", k);
	break;
    default:
	assert(0);
    }
    return a;
}

/** @returns A string with the decimal representation of the double \p in is returned. 
 * A very complex string is returned if \p f is ::ADA_FORMAT.
 * @param in The double number
 * @param f The string format. 
*/

char *double_to_string(double in, enum string_format f)
{
    char *out = "";
    char buf[100];
    if (!isfinite(in))
	ERROR_DEC_NOT_FINITE(0) sprintf(buf, "%a", in);
    switch (f) {
    case ADASTRING:
	{
	    char *delimiter = "xp";
	    char *significand;
	    char *exponent;
	    significand = strtok(buf, delimiter);
	    significand = strtok(NULL, delimiter);
	    exponent = strtok(NULL, delimiter);
	    int expo = string_to_int(exponent, 0);
	    if (floor(in) == in) {
		sprintf(buf, "%.1lf", in);
		my_strcat(&out, buf);
		break;
	    } else if (in < 0)
		my_strcat_several(&out, 2, "-((16#", significand);
	    else
		my_strcat_several(&out, 2, "((16#", significand);

	    if (strstr(significand, ".") == NULL)
		my_strcat(&out, ".0");
	    if (expo == 0)
		my_strcat_several(&out, 1, "#E+0))");
	    else if (expo == 1)
		my_strcat_several(&out, 1, "#E+0)*2.0)");
	    else if (expo > 1)
		my_strcat_several(&out, 5, "#E+0)*", ADA_DOUBLE_TYPE, "(2**", exponent + 1, "))");
	    else
		my_strcat_several(&out, 5, "#E+0)*(1.0/", ADA_DOUBLE_TYPE, "(2**", exponent + 1,
				  ")))");
	}
	break;
    case MYSTRING:
    case MMASTRING:
	sprintf(buf, "%.16lf", in);
	my_strcat(&out, buf);
	break;
    case MMAPRINTSTRING:
	sprintf(buf, "%lf", in);
	my_strcat(&out, buf);
	break;
    default:
	my_strcat(&out, buf);
	break;
    }
    return out;
}

/** @returns A string representation of the number in \in is returned according to the parameters \p mode and \p f.
 * The error #ERROR_DEC_NOT_FINITE is thrown if the number cannot be represented in the output format
 * @param in The expression holding the number
 * @param mode The number format
 * @param f The string format
*/

char *literal_Number_to_string(Expression in, enum number_format mode, enum string_format f)
{
    char *a = "";
    if (is_Expression_literal_Number(in)) {
	if (TYPE(in) == NUM_INT) {
	    a = int_to_string(GET_INT(in), mode);
	} else if (TYPE(in) == NUM_CONST) {
	    switch (mode) {
	    case SYMBOLIC:
		a = (char *) my_malloc((strlen(in->content) + 1) * sizeof(char));
		strcpy(a, in->content);
		break;
	    case NUMERICAL:
		a = double_to_string(math_constant_to_double(in), f);
		break;
	    default:
		assert(0);
	    }
	} else {
	    switch (f) {
	    case MYSTRING:
		{
		    my_strcat(&a, in->content);
		}
		break;
	    default:
		{
		    double rounded_value = (double) strtod(in->content, NULL);
		    if (!isfinite(rounded_value))
			ERROR_DEC_NOT_FINITE(in->line)
			    a = double_to_string(rounded_value, f);
		}
		break;
	    }
	}
	return a;
    } else
	return NULL;
}

/** @returns A string representing an entry of a vector is returned in string format \p f 
 * having identifier \p name and entry \p a
 * @param name The identifier for the vector
 * @param a The entry index
 * @param f The string format
*/

char *vector_entry_to_string(const char *name, const char *a, enum string_format f)
{
    char *buf = "";
    switch (f) {
    case ADASTRING:
	{
	    if (!strcmp(name, Task.timevar_name))
		my_strcat(&buf, name);
	    else
		my_strcat_several(&buf, 4, name, "(", a, ")");
	}
	break;
    case MMAPRINTSTRING:
    case MMASTRING:
	{
	    int k = string_to_int(a, 0);
	    k++;
	    char *buf2 = int_to_string(k, SYMBOLIC);
	    my_strcat_several(&buf, 4, name, "[[", buf2, "]]");
	    free(buf2);
	}
	break;
    case GAPPAMATHSTRING:
	{
	    my_strcat_several(&buf, 4, name, "_", a, "_0");
	}
	break;
    case GAPPAFLOATSTRING:
	{
	    my_strcat_several(&buf, 6, GAPPA_ROUNDING_KEYWORD, "(", name, "_", a, "_0)");
	}
	break;
    case MYSTRING:
    case CSTRING:
	{
	    if (!strcmp(name, Task.timevar_name))
		my_strcat(&buf, name);
	    else
		my_strcat_several(&buf, 4, name, "[", a, "]");
	}
	break;
    default:
	assert(0);
    }
    return buf;
}

/** @brief Wrapper for vector_entry_to_string()
 * @param name See vector_entry_to_string()
 * @param a The index of the entry 
 * @param f See vector_entry_to_string()
*/

char *vector_entry_to_string_int(const char *name, dim_t a, enum string_format f)
{
    char aa[20];
    if (f == ADASTRING)
	a++;
    sprintf(aa, "%u", a);
    return vector_entry_to_string(name, aa, f);
}


/** @returns A string representing an entry of a matrix is returned in string format \p f 
 * having identifier \p name and row and column index \p a and \p b, respectively.
 * @param name The identifier for the matrix
 * @param a The row index
 * @param b The column index
 * @param f The string format
*/
char *matrix_entry_to_string(const char *name, const char *a, const char *b, enum string_format f)
{
    char *buf = "";
    switch (f) {
    case ADASTRING:
	my_strcat_several(&buf, 6, name, "(", a, ",", b, ")");
	break;
    case MMAPRINTSTRING:
    case MMASTRING:
	my_strcat_several(&buf, 8, name, "[[", a, "+1", ",", b, "+1", "]]");
	break;
    case GAPPAMATHSTRING:
	my_strcat_several(&buf, 5, name, "_", a, "_", b);
	break;
    case GAPPAFLOATSTRING:
	my_strcat_several(&buf, 8, GAPPA_ROUNDING_KEYWORD, "(", name, "_", a, "_", b, ")");
	break;
    case MYSTRING:
    case CSTRING:
	my_strcat_several(&buf, 7, name, "[", a, "]", "[", b, "]");
	break;
    default:
	assert(0);
    }
    return buf;
}

/** @brief Wrapper for matrix_entry_to_string()
 * @param name See matrix_entry_to_string()
 * @param a The row index
 * @param b The column index
 * @param f See vector_entry_to_string()
*/
char *matrix_entry_to_string_int(const char *name, dim_t a, dim_t b, enum string_format f)
{
    char aa[20];
    char bb[20];
    if (f == ADASTRING) {
	a++;
	b++;
    }
    sprintf(aa, "%u", a);
    sprintf(bb, "%u", b);
    return matrix_entry_to_string(name, aa, bb, f);
}

char *array_of_array_entry_to_string_int(const char *name, dim_t a, dim_t b, enum string_format f)
{
    char *buf = "";
    char aa[20];
    char bb[20];
    if (f == ADASTRING) {
	a++;
	b++;
    }
    sprintf(aa, "%u", a);
    sprintf(bb, "%u", b);
    switch (f) {
    case ADASTRING:
	my_strcat_several(&buf, 6, name, "(", aa, ")(", bb, ")");
	break;
    default:
	buf = matrix_entry_to_string(name, aa, bb, f);
	break;
    }
    return buf;
}


/** 
 * @param in \f$a \in [\verb|postfix_expression|]\f$
 * @param f    The format to use 
 * @returns A newly allocated string representing \p a in the format \p f. 
*/
char *PostFixExpression_to_string(PostFixExpression in,
				  enum string_format f, ConstSequenceOfStates S)
{
    char *buf = "";
    dim_t row, col;
    assert(in != NULL);
    assert(TYPE(in) == POSTFIX);
    get_indices_of_PostFixExpression(in, &row, &col, 0, S);
    if (ROW_IDX(in) == NULL && COL_IDX(in) == NULL) {
	const struct variable *x = version_of_variable(find_in_variables(in, S), 0, 0,
						       get_date(S));

	if (var_role_of(x) != ORDINARY_VAR)
	    buf = vector_entry_to_string_int(NAME(in), 0, f);
	else {
	    switch (f) {
	    case GAPPAMATHSTRING:
	    case GAPPAFLOATSTRING:
		buf = matrix_entry_to_string(NAME(in), "0", "0", f);
		break;
	    default:
		my_strcat(&buf, NAME(in));
		break;
	    }
	}

    } else if (ROW_IDX(in) != NULL && COL_IDX(in) == NULL) {
	buf = vector_entry_to_string_int(NAME(in), row, f);
    } else if (COL_IDX(in) != NULL) {
	buf = matrix_entry_to_string_int(NAME(in), row, col, f);
    }
    return buf;
}

/**
 * @returns String representation of an interval
 * @param in Expression whose string representation is sought
*/
char *interval_to_string(Expression in, enum string_format f, ConstSequenceOfStates S)
{
    char *buf = "";
    assert(in != NULL);
    if (is_Expression_Interval(in)) {
	char *a = Expression_to_string(in->left, SYMBOLIC, f, S);
	char *b = Expression_to_string(in->right, SYMBOLIC, f, S);
	if (TYPE(in) == DISC_INTV) {
	    my_strcat_several(&buf, 5, "[", a, ";", b, "]");
	} else {
	    my_strcat_several(&buf, 5, "[", a, ",", b, "]");
	}
	free(a);
	free(b);
    } else if (is_Expression_RealSpace(in)) {
	char *b = "";
	if (in->right != NULL)
	    b = Expression_to_string(in->right, SYMBOLIC, f, S);
	if (in->right != NULL) {
	    my_strcat_several(&buf, 3, "Real[", b, "]");
	    free(b);
	} else
	    my_strcat(&buf, "Real");
    } else if (in->type == POSTFIX) {
	return interval_to_string(definition_of(in, S), f, S);
    } else
	assert(0);
    return buf;
}

/**
 * @brief Computes the string representation of Expression \p in
 * @param in   Expression whose string representation is sought
 * @param mode One of ::SYMBOLIC, ::NUMERICAL
 * @param f    The string format
 * @param S    The sequence of states
 * @returns \c NULL if \p in is \c NULL otherwise string representing \p in
*/
char *Expression_to_string(Expression in, enum number_format mode,
			   enum string_format f, ConstSequenceOfStates S)
{
    if (in == NULL)
	return NULL;
    char *a = "", *b = "";
    reduce_expression_to_int(in, S);
    if (TYPE(in) == LIST) {
	Expression ptr;
	char *buf = "";
	char *sep = "(";
	for (ptr = (Expression) wind_back(AST_EXPR, in->left); ptr != NULL; ptr = ptr->next) {
	    my_strcat(&buf, sep);
	    sep = ",";
	    a = Expression_to_string(ptr, mode, f, S);
	    my_strcat(&buf, a);
	    free(a);
	}
	my_strcat(&buf, ")");
	return buf;
    }
    if ((a = literal_Number_to_string(in, mode, f)) != NULL)
	return a;
    else if (TYPE(in) == POSTFIX) {
	if ((is_role_of(in, INPUT_VAR, S) || is_role_of(in, STATE_VAR, S)))
	    return PostFixExpression_to_string(in, f, S);
	if (is_PostFixExpression_Interval(in, S))
	    return PostFixExpression_to_string(in, f, S);
	if (mode && (TYPE(definition_of(in, S)) == NUM_DEC)
	    && is_PostFixExpression_read_only(in, S))
	    return PostFixExpression_to_string(in, f, S);
	if (mode && interval_of(in, S) != NULL)
	    return PostFixExpression_to_string(in, f, S);
	replace_PostFixExpression_by_its_Expression(&(in), in, S);
	return Expression_to_string(in, mode, f, S);
    } else if (is_Expression_type_function(TYPE(in))) {
	b = Expression_to_string(in->right, mode, f, S);
	assert(in->content == NULL);
	a = _string_of_function(TYPE(in), f);
	char *buf = "";
	switch (f) {
	case MMAPRINTSTRING:
	case MMASTRING:
	    my_strcat_several(&buf, 4, a, "[", b, "]");
	    break;
	default:
	    my_strcat_several(&buf, 4, a, "(", b, ")");
	    break;
	}
	free(a);
	free(b);
	return buf;
    } else if (is_Expression_Interval(in) || is_Expression_RealSpace(in)) {
	return interval_to_string(in, f, S);
    } else {
	char *la = "(";
	char *ra = ")";
	char *lb = "(";
	char *rb = ")";
	if (is_Expression_elementary_operation(in)) {
	    a = Expression_to_string(in->left, mode, f, S);
	    b = Expression_to_string(in->right, mode, f, S);
	    if (a[0] == '(') {
		la = "";
		ra = "";
	    }
	    if (b[0] == '(') {
		lb = "";
		rb = "";
	    }
	}
	char *c = string_of_elementary_operation(in->type);
	char *buf = "";
	if (TYPE(in->left) == NUM_INT && GET_INT(in->left) == -1 && TYPE(in) == TIMES) {
	    my_strcat_several(&buf, 5, "(-", lb, b, rb, ")");
	} else if (TYPE(in->right) == NUM_INT && GET_INT(in->right) == -1 && TYPE(in) == TIMES) {
	    my_strcat_several(&buf, 5, "(-", lb, a, rb, ")");
	} else if ((is_Expression_Number(in->left))
		   && is_Expression_Number(in->right)) {
	    my_strcat_several(&buf, 5, "(", a, c, b, ")");
	} else if (is_Expression_Number(in->right)) {
	    my_strcat_several(&buf, 7, "(", la, a, ra, c, b, ")");
	} else if (is_Expression_Number(in->left)) {
	    my_strcat_several(&buf, 7, "(", a, c, lb, b, rb, ")");
	} else {
	    my_strcat_several(&buf, 9, "(", la, a, ra, c, lb, b, rb, ")");
	}
	free(c);

	if (is_Expression_elementary_operation(in)) {
	    free(a);
	    free(b);
	}
	return buf;
    }
    return NULL;
}

char *data_of_identifier_to_hyperinterval_string(Variable x, enum number_format mode,
						 enum string_format f, ConstSequenceOfStates S)
{
    char *buf = "";
    char *buf2 = "";
    char *comma = "(";
    dim_t j;
    dim_t a = get_date(S);
    Variable y = version_of_variable(x, 0, 0, a);
    for (j = 0; j < row_dim_of(y); j++) {
	y = version_of_variable(x, j, 0, a);
	if (TYPE(data_of(y, j, 0)) != CONT_INTV) {
	    my_strcat(&buf2, "?");
	} else {
	    buf2 = Expression_to_string(data_of(y, j, 0), mode, f, S);
	}
	my_strcat_several(&buf, 2, comma, buf2);
	free(buf2);
	buf2 = "";
	comma = ",";
    }
    my_strcat(&buf, ")");
    return buf;
}
