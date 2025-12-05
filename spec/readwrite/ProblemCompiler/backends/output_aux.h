/** @file 
 * @brief Header for output_aux.c
 * @author Alexander Weber (a.weber@unibw.de)
*/
#ifndef OUTPUT_AUX_H
#define OUTPUT_AUX_H
#include "ast.h"
#include "variables.h"

void copy_file_content(FILE * dest, FILE * src);
char *generate_identifier(const char *stdvar, ConstSequenceOfStates);
double math_constant_to_double(Expression);
char *ToUpperCase(const char *str);
void look_for_keywords_of_C(ConstSequenceOfStates);
#endif
