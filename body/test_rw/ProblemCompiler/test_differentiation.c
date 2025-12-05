/** @file test_differentiation.c
 * @brief This is the main test for the part SourceCodeGeneration. 
 * It used Wolfram Mathematica. As every test implementation it contains a 
 * separate entry point.
 * @author Alexander Weber (a.weber@unibw.de)
 * @data 2 Feb 2017
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

char mathematica_file_string_part_1[] = 
"(* ::Package:: *)\n" \
"(* Generation of the Taylor coefficients *)\n" \
"TaylorCoefficient[k_,x_,f_]:=x/;k==0;\n" \
"TaylorCoefficient[k_,x_,f_]:=f[x]/;k==1;\n" \
"TaylorCoefficient[k_,x_,f_]:=k^(-1)*(D[TaylorCoefficient[k-1,x,f],{x,1}]).f[x]/;k>1\n" \
"(* Find out architecture *)\n" \
"Run[\"gcc -dumpmachine > .sys.str\"];\n" \
"str=OpenRead[\".sys.str\"];\n" \
"bindir=Read[str,Word];\n" \
"Close[str];\n" \
"Clear[str];\n" \
"Run[\"rm .sys.str\"];\n" \
"(* Set directory of binary *)\n" \
"bindir = \"./\";\n" \
"FileOfCoefficients =\"OutputCode/taylorcoefficients.abcs\";\n" \
"TestRecursive[filename_String,order_Integer]:=Module[{},\n" \
"Clear [f,coefficients];\n" \
"If[Run[\"\" <> bindir <> \"compiler -mma-symbolic -quiet " ;

char mathematica_file_string_part_2[] = 
"-outputdir=OutputCode -order=\" <> ToString[order] <> \" \" <> #]==0,\n" \
"Get[Directory[]<>\"/OutputCode/Function.mma\"];\n" \
"If[Run[\"\"<>bindir<>\"compiler -mma-symbolic -quiet -outputdir=OutputCode < \"<> FileOfCoefficients]==0,\n" \
"Get[Directory[]<>\"/OutputCode/Function.mma\"],Return[False]];\n" ; 

char mathematica_file_string_part_3_a[] = 
"If[Not[VectorQ[taylorcoefficients[in]]],Return[False],Return[Simplify[Table[TaylorCoefficient[k,in,f],{k,0,order}]-taylorcoefficients[in]]===Array[Array[0&,Length[in]]&,order+1]]],\n" ;

char mathematica_file_string_part_3_b[] = 
"If[Not[MatrixQ[taylorcoefficients[in,{u}]]],Return[False],Return[Simplify[Table[TaylorCoefficient[k,in,f[#,{u}]&],{k,0,order}]-taylorcoefficients[in,{u}]]===Array[Array[0&,Length[in]]&,order+1]]],\n" ;

char mathematica_file_string_part_4[] = 
"Return[False]]]&[filename]\n" \
"If[TestRecursive[\"../../../body/test_rw/ProblemCompiler/Testfiles/";

char mathematica_file_string_part_5[] = 
"]==False,Exit[255],Exit[0]];\n" \
"Quit[];\n" ;

char *test_files_autonomous[] = {"1_atan.dat","2_atan.dat","1_cos.dat","2_cos.dat","1_cosh.dat","2_cosh.dat","1_exp.dat","2_exp.dat"};

char *test_files_control[] = {"1_Pendulum.dat","1_DoublePendulum.dat"};
int test_files_control_order[2] = {4,3};

void print_failed_string(char *name)
{
	fprintf(stderr,"Differentiation test for \'%s\' failed.\n",name);
}

void prepare_for_exit(char *name)
{
	if(name != NULL ) print_failed_string(name);
	unlink("test_differentiation.m");
	if( !system("rm -rf OutputCode") ) {
     }
}

int main(int argc, char *argv[])
{
	unsigned int i;
	unsigned int order = 5;
	for(i=0;i<sizeof(test_files_autonomous)/sizeof(test_files_autonomous[0]);i++)
	{
		FILE *file = fopen("test_differentiation.m","w+");
		fprintf(file,"%s-recursive ",mathematica_file_string_part_1);
		fprintf(file,"%s",mathematica_file_string_part_2);
		fprintf(file,"%s",mathematica_file_string_part_3_a);
		fprintf(file,"%s",mathematica_file_string_part_4);
		fprintf(file,"%s\",",test_files_autonomous[i]);
		fprintf(file,"%u%s",order,mathematica_file_string_part_5);
		fclose(file);
		if( system("math -noprompt -run \"<<test_differentiation.m\" ") != 0 ) 
		{
			prepare_for_exit(test_files_autonomous[i]);
			return 255;
		}
		file = fopen("test_differentiation.m","w+");
		fprintf(file,"%s",mathematica_file_string_part_1);
		fprintf(file,"%s",mathematica_file_string_part_2);
		fprintf(file,"%s",mathematica_file_string_part_3_a);
		fprintf(file,"%s",mathematica_file_string_part_4);
		fprintf(file,"%s\",",test_files_autonomous[i]);
		fprintf(file,"%u%s",order,mathematica_file_string_part_5);
		fclose(file);
		if( system("math -noprompt -run \"<<test_differentiation.m\" ") != 0 ) 
		{
	 		prepare_for_exit(test_files_autonomous[i]);
			return 255;
		}
	}
	for(i=0;i<sizeof(test_files_control)/sizeof(test_files_control[0]);i++)
	{
		FILE *file = fopen("test_differentiation.m","w+");
		fprintf(file,"%s-nofolding ",mathematica_file_string_part_1);
		fprintf(file,"%s",mathematica_file_string_part_2);
		fprintf(file,"%s",mathematica_file_string_part_3_b);
		fprintf(file,"%s",mathematica_file_string_part_4);
		fprintf(file,"%s\",",test_files_control[i]);
		fprintf(file,"%u%s",test_files_control_order[i],mathematica_file_string_part_5);
		fclose(file);
		if( system("math -noprompt -run \"<<test_differentiation.m\" ") != 0 ) 
		{
			prepare_for_exit(test_files_control[i]);
			return 255;
		}
		file = fopen("test_differentiation.m","w+");
		fprintf(file,"%s",mathematica_file_string_part_1);
		fprintf(file,"%s",mathematica_file_string_part_2);
		fprintf(file,"%s",mathematica_file_string_part_3_b);
		fprintf(file,"%s",mathematica_file_string_part_4);
		fprintf(file,"%s\",",test_files_control[i]);
		fprintf(file,"%u%s",test_files_control_order[i],mathematica_file_string_part_5);
		fclose(file);
		if( system("math -noprompt -run \"<<test_differentiation.m\" ") != 0 ) 
		{
	 		prepare_for_exit(test_files_control[i]);
			return 255;
		}
	}
	prepare_for_exit(NULL);
	return 0;	
}
