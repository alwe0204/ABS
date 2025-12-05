/** @file
 *  @author Alexander Weber (a.weber@unibw.de)
 *  @brief Header for gappa_utils.c
*/
#ifndef GAPPA_UTILS_H
#define GAPPA_UTILS_H

#define GAPPA_ROUNDING_KEYWORD "double"
#define GAPPA_FLOAT_FORMAT_KEYWORD "ieee_64"
#define GAPPA_ROUNDING_MODE_KEYWORD "ne"
#define GAPPA_NUMBER_OF_ITERATIONS 100

#define GAPPA_EXECUTABLE "../../../libs/gappa-1.3.1/src/ABS_gappa"

int run_gappa(const char *script_name, const char *response_name,const char *gappa_binary_file);
double gappa_get_upper_bound(const char *script_name, const char *response_name,const char *gappa_binary_file);
double gappa_parser_lower(const char *file);
double gappa_parser_upper(const char *file);

void gappa_rounding_switch(FILE *file);
FILE * gappa_fopen(const char *script_name);

void read_list(FILE * file, double *out);
#endif
