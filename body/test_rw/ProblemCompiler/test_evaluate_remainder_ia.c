#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
int main(int argc, char *argv[])
{
 int retval;
 char *buf = "./compiler -quiet -nosimplify -nomma-file -outputdir=OutputCode -order=4 ../../../body/test_rw/ProblemCompiler/Testfiles/1_DoublePendulum.dat";
 retval = system(buf);
 unlink("OutputCode/Approximation_Error.dat");
 unlink("OutputCode/taylorcoefficients.h");
 unlink("OutputCode/taylorcoefficients.c");
 unlink("OutputCode/taylorcoefficients.ads");
 unlink("OutputCode/taylorcoefficients.adb");
 unlink("OutputCode/taylorcoefficients.abcs");
 unlink("OutputCode/Integration.h");
 unlink("OutputCode/Integration.c");
 unlink("OutputCode/Integration.ads");
 unlink("OutputCode/Integration.adb");
 unlink("OutputCode/Integration.abcs");
 if(retval) return 1;
 char *buf2 = "./compiler -quiet -nomma-file -nosimplify -outputdir=OutputCode/Remainder OutputCode/taylorcoefficient_last_plus_1.abcs";
 retval = system(buf2);
 if(retval) return 2;
 unlink("OutputCode/Remainder/Function.h");
 unlink("OutputCode/Remainder/Function.c");
 unlink("OutputCode/Remainder/Function.ads");
 unlink("OutputCode/Remainder/Function.adb");
 retval = system("/usr/bin/gcc -g -c OutputCode/Remainder/Function_ia.c -I../../../spec/readwrite/ProblemCompiler/misc -I../../../libs/mpfi-1.5.1/src -I../../../libs/mpfr-3.1.5/src -I../../../libs/gmp-6.1.2/");
 if(retval) return 3;
 retval = system("/usr/bin/gcc -g -c ../../../body/test_rw/ProblemCompiler/test_evaluate_remainder_ia_driver.c -I../../../spec/readwrite/ProblemCompiler/misc -I./OutputCode/Remainder/ -I../../../libs/mpfi-1.5.1/src -I../../../libs/mpfr-3.1.5/src -I../../../libs/gmp-6.1.2/");
 if(retval) return 4;
 retval = system("/usr/bin/gcc -g test_evaluate_remainder_ia_driver.o Function_ia.o -Wl,-Bstatic -L../../../libs/mpfi-1.5.1/src/.libs -lmpfi -L../../../libs/mpfr-3.1.5/src/.libs -lmpfr -L../../../libs/gmp-6.1.2/.libs -lgmp -Wl,-Bdynamic -lm");
 if(retval) return 5;
 retval = system("./a.out");
 if(retval) return 6;
 else 
 {
   unlink("a.out");
   unlink("test_evaluate_remainder_ia_driver.o");
   unlink("Function_ia.o");
   retval = system("rm -rf OutputCode");
   return 0;
 }
 
}
