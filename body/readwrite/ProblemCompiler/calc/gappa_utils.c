/** @file 
 * @brief Auxiliary functions for interacting with the software Gappa
 * @date 01 Apr 2017
 * @author Alexander Weber (a.weber@unibw.de)
*/
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>		/* for unlink */
#include <math.h>

#include "errors.h"
#include "gappa_utils.h"
#include "my_strcat.h"

/**
 * @brief Runs Gappa on \p script_name and writes the output to \p response_name
 * @param script_name The file that contains the Gappa input
 * @param response_name The file to write the Gappa output
 * @param gappa_binary_file The path to the executable of the Gappa software
 * @return Returns the return value of the Gappa software if the Gappa software is found at \p gappa_binary_file. 
 * Otherwise returns a non-zero value.
*/
int run_gappa(const char *script_name, const char *response_name, const char *gappa_binary_file)
{
    char *buf = "";
    int retval;
    my_strcat_several(&buf, 2, gappa_binary_file, " --help 2> /dev/null");
    retval = system(buf);
    if (retval != 0) {
	fprintf(stderr, "Executable of Gappa cannot be found.\n");
	exit(EXIT_FAILURE);
    }
    free(buf);
    buf = "";
    my_strcat_several(&buf, 5, gappa_binary_file, " ", script_name, " 2> ", response_name);
    retval = system(buf);
    free(buf);
    return retval;
}

/**
 * @brief Parses the output file of the MODIFIED Gappa software and 
 * returns the lower value of the found bound on the rounding error.
 * @param file The output file of the MODIFIED Gappa software
 * @return Returns the lower value of the found bound on the rounding error. 
 * If parsing was not successful then \f$-\infty\f$ is returned.
*/
double gappa_parser_lower(const char *file)
{
    FILE *str;
    double res = -HUGE_VAL;
    char *buf, *bufsave;
    buf = (char *) malloc(100 * sizeof(char));
    bufsave = buf;
    str = fopen(file, "r");
    if (!str) {
	ERROR_OPEN_FILE(file);
    }
    while (fscanf(str, "%s", buf) != EOF) {
	if (strchr(buf, '[')) {
	    buf += 1;
	    res = strtod(buf, NULL);
	    break;
	}
    }
    fclose(str);
    free(bufsave);
    return res;
}

/**
 * @brief Parses the output file of the MODIFIED Gappa software and 
 * returns the upper value of the found bound on the rounding error.
 * @param file The output file of the MODIFIED Gappa software
 * @return Returns the upper value of the found bound on the rounding error. 
 * If parsing was not successful then \f$\infty\f$ is returned.
*/
double gappa_parser_upper(const char *file)
{
    FILE *str;
    double res = HUGE_VAL;
    char *buf, *bufsave;
    buf = (char *) malloc(100 * sizeof(char));
    bufsave = buf;
    str = fopen(file, "r");
    if (!str) {
	ERROR_OPEN_FILE(file);
	exit(EXIT_FAILURE);
    }
    while (fscanf(str, "%s", buf) != EOF) {
	if (strchr(buf, ']')) {
	    res = strtod(buf, NULL);
	    break;
	}

    }
    fclose(str);
    free(bufsave);
    return res;
}

double gappa_get_upper_bound(const char *script_name, const char *response_name,
			     const char *gappa_binary_file)
{
    double retval;
    if (0 != run_gappa(script_name, response_name, gappa_binary_file)) {
	retval = HUGE_VAL;
    } else {
	retval = gappa_parser_upper(response_name);
    }
    unlink(script_name);
    unlink(response_name);
    return retval;
}

void gappa_rounding_switch(FILE * file)
{
    fprintf(file, "@%s = float<%s,%s>;\n", GAPPA_ROUNDING_KEYWORD, GAPPA_FLOAT_FORMAT_KEYWORD,
	    GAPPA_ROUNDING_MODE_KEYWORD);
}

FILE *gappa_fopen(const char *script_name)
{
    FILE *file = fopen(script_name, "w+");
    if (file == NULL)
	ERROR_OPEN_FILE(script_name);
    gappa_rounding_switch(file);
    return file;
}

void read_list(FILE * file, double *out)
{
    unsigned int i = 0;
    double x;
    while (fscanf(file, "%lf", &x) != EOF) {
	assert(isfinite(x));
	out[i] = x;
	i++;
    }
}
