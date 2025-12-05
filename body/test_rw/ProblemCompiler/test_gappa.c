#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include "my_strcat.h"
#include "gappa_utils.h"

const char *gappafiles[] = {
"1_gappa.dat", 
"1_atan_gappa.dat", 
"1_cos_gappa.dat", 
"1_cosh_gappa.dat", 
"1_exp_gappa.dat", 
"2_exp_gappa.dat",
"3_exp_gappa.dat",
"4_exp_gappa.dat",
"1_ln_gappa.dat", 
"2_ln_gappa.dat", 
"1_sin_gappa.dat", 
"1_sinh_gappa.dat",
"1_sqrt_gappa.dat",
"2_sqrt_gappa.dat",
"1_sqr_gappa.dat",
"1_tan_gappa.dat",
"2_tan_gappa.dat",
"3_tan_gappa.dat",
"4_tan_gappa.dat"};


int main(int argc, char *argv[])
{
 int retval;
 double res = 0;
 double old_res = 0;
 retval = run_gappa("../../../body/test_rw/ProblemCompiler/Testfiles/1.gappa","../../../body/test_rw/ProblemCompiler/Testfiles/1.answer",GAPPA_EXECUTABLE);
 if(retval) return 1;
 double a = gappa_parser_lower("../../../body/test_rw/ProblemCompiler/Testfiles/1.answer");
 double b = gappa_parser_upper("../../../body/test_rw/ProblemCompiler/Testfiles/1.answer");
 if(a != 1.0) return 2;
 if(b != 2.0) return 3;
 unlink("../../../body/test_rw/ProblemCompiler/Testfiles/1.answer");
 retval = run_gappa("../../../body/test_rw/ProblemCompiler/Testfiles/2.gappa","../../../body/test_rw/ProblemCompiler/Testfiles/2.answer",GAPPA_EXECUTABLE);
 if(retval) return 4;
 a = gappa_parser_lower("../../../body/test_rw/ProblemCompiler/Testfiles/2.answer");
 b = gappa_parser_upper("../../../body/test_rw/ProblemCompiler/Testfiles/2.answer");
 if(a != -1.0) return 5;
 if(b != 0.5 ) return 6;
 unlink("../../../body/test_rw/ProblemCompiler/Testfiles/2.answer");
 retval = run_gappa("../../../body/test_rw/ProblemCompiler/Testfiles/3.gappa","../../../body/test_rw/ProblemCompiler/Testfiles/3.answer",GAPPA_EXECUTABLE);
 if(retval) return 7;
 a = gappa_parser_lower("../../../body/test_rw/ProblemCompiler/Testfiles/3.answer");
 b = gappa_parser_upper("../../../body/test_rw/ProblemCompiler/Testfiles/3.answer");
 if(a > 0x1.54p-54) return 8;
 if(b > 1.0E-16 ) return 9;
 unlink("../../../body/test_rw/ProblemCompiler/Testfiles/3.answer");
 int i;
 for(i=0;i<sizeof(gappafiles)/sizeof(gappafiles[0]);i++) {
 char *buf = "";
 my_strcat(&buf,"./compiler -nomma-file -quiet -gappa -outputdir=. ../../../body/test_rw/ProblemCompiler/Testfiles/");
 my_strcat(&buf,gappafiles[i]);
 retval = system(buf);
 if(retval) return 10+i;
 free(buf);
 FILE *file =fopen("Bounds_of_Rounding_Errors.dat","r");
 read_list(file,&res);
 if(!strcmp(gappafiles[0],"4_exp_gappa.dat")) {
  if( old_res != res ) return 10+i;
 }
 if(i > sizeof(gappafiles)/sizeof(gappafiles[0]) - 4) {
  if(old_res > res) return 10+i;
 }
 old_res = res;
/* printf("%s %E\n",gappafiles[i],res);*/
 unlink("Bounds_of_Rounding_Errors.dat");
 } 
 return 0;
}
