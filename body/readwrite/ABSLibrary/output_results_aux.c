#include <stdio.h>
#include <stdlib.h>
char * double_to_exact_representation(double in)
{
 char *out = malloc(30*sizeof(char));
 sprintf(out,"%a",in);
 return out;
}
