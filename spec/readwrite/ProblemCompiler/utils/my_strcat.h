/** @file my_strcat.h
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 25.05.2017 
 * @brief Header for my_strcat.c
*/
#ifndef MY_STRCAT_H
#define MY_STRCAT_H

void my_strcat(char **dest, const char *src);
void my_strcat_several(char **dest, int numArgs, ...);

#endif
