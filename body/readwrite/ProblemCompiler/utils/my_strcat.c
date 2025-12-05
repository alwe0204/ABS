/** @file my_strcat.c
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 25.05.2017 
 * @brief String concatenate functions
 * @version 1.0
*/

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

/** @brief This method concatenates the string \p src to \p dest where it also 
 * reallocates space for the new string. 
 * @param dest The new string
 * @param src The string to append to \p dest
*/

void my_strcat(char **dest, const char *src)
{
    if (strlen(*dest) == 0) {
	*dest = malloc((strlen(src) + 1) * sizeof(char));
     assert(*dest != NULL);
	(*dest)[0] = '\0';
    } else {
	char *new_dest = realloc(*dest,
				 (strlen(*dest) + strlen(src) + 1) * sizeof(char));
	if (new_dest != NULL) {
	    *dest = new_dest;
	}
    }
    strcat(*dest, src);
}

/** @brief This method uses my_strcat() for appending several strings to \p dest.
 * @param dest The new string
 * @param numArgs The number of string to append
*/

void my_strcat_several(char **dest, int numArgs, ...)
{
    va_list args;
    va_start(args, numArgs);
    int i;
    for (i = 0; i < numArgs; i++) {
	char *str = va_arg(args, char *);
	my_strcat(dest, str);
    }
    va_end(args);
}
