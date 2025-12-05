/** @file 
 *@author Alexander Weber (a.weber@unibw.de)
 *@data 20 Apr 2016
 *@brief Auxiliary Functions to output the input
*/

#define _POSIX_C_SOURCE 200809L	/* for using strdup */

#define _GNU_SOURCE
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "output_aux.h"
#include "errors.h"



void copy_file_content(FILE * dest, FILE * src)
{
    char buffer[256] = { 0 };
    while (!feof(src)) {
	if (fgets(buffer, sizeof(buffer), src) != NULL) {
	    fprintf(dest, "%s", buffer);
	}
    }
}


void look_for_keywords_of_C(ConstSequenceOfStates S)
{
    size_t i;
    Variable x;
    char *keywords[] = { "auto", "double", "int", "struct", "break", "else", "long",
	"switch", "case", "enum", "register", "typedef", "char", "extern",
	"return",
	"union", "const", "float", "short", "unsigned", "continue", "for",
	"signed",
	"void", "default", "goto", "sizeof", "volatile", "do", "if",
	"static",
	"while"
    };

    for (i = 0; i < sizeof(keywords) / sizeof(keywords[0]); i++) {
	x = find_in_variables_from_string(keywords[i], S);
	if (x != NULL)
	    ERROR_OUTPUT_KEYWORD_OF_C(name_of(x));
    }
}

char *ToUpperCase(const char *str)
{
    char *newstr, *p;
    p = newstr = strdup(str);
    while (*p != '\0') {
	*p = toupper(*p);
	p++;
    }
    return newstr;
}


char *new_string(size_t const size)
{
    size_t pos = 0;
    const char charset[] = "0abcdefghijklmnopqrstuvwxyz";
    char *out;
    size_t i;
    if (size) {
	out = my_malloc((size + 1) * sizeof(char));
    } else
	return NULL;
    for (i = 0; i < size; i++) {
	out[i] = charset[pos];
    }
    out[size] = '\0';
    return out;
}

/** @brief Checks whether \p stdvar was used in the input and returns 
 *if so a modified version of \p stdvar.
 *@param stdvar The input string
 *@param var The current state \f$\sigma \in S \f$.  
 *@returns \p stdvar if not used in the input file otherwise a modified version of \p stdvar.
 * 
 * 
 * Uses new_string(). The number of attempts is limited but hopefully never reached.
*/

char *generate_identifier(const char *stdvar, ConstSequenceOfStates S)
{
    const int max_attempts = 5;
    char *tmp, *str;
    size_t i = 0;
    size_t j = 0;
    Variable x = find_in_variables_from_string(stdvar, S);
    if (x == NULL) {
	str = malloc(strlen(stdvar) + 1);
	strcpy(str, stdvar);
	return str;
    } else {
	do {
	    i++;
	    str = malloc(strlen(stdvar) + i + 1);
	    do {
		j++;
		tmp = new_string(i);
		strcpy(str, stdvar);
		strcat(str, tmp);
		x = find_in_variables_from_string(str, S);
		free(tmp);
	    }
	    while (x != NULL && j < max_attempts);
	    if (x != NULL && i < max_attempts)
		free(str);
	}
	while (x != NULL && i < max_attempts);
    }
    if (i == max_attempts || x != NULL) {
	BAD_ERROR;
    } else
	return str;
}


double math_constant_to_double(PostFixExpression in)
{
    assert(in != NULL);
    assert(TYPE(in) == NUM_CONST);
    size_t i;
    char *math_const[] = { "Pi", "Exp" };
    for (i = 0; i < sizeof(math_const) / sizeof(math_const[0]); i++) {
	if (!strcmp(math_const[i], in->content))
	    break;
    }
    switch (i) {
    case 0:
	return M_PI;
	break;
    case 1:
	return M_E;
	break;
    default:
	assert(0);
    }
}
