/** @file
 *  @author Alexander Weber (a.weber@unibw.de)
 *  @brief Header for gappa_abcs.c
*/
#ifndef GAPPA_ABCS_H
#define GAPPA_ABCS_H
#include "ad.h"

double rounding_error_indexedreals(double x, double x_min, double x_max, double y,
				   double y_min, double y_max, unsigned int p_max,
				   unsigned int q);
double rounding_error_indexedreals_input(double x, double x_min, double x_max,
					    double y, double y_min, double y_max,unsigned int d);
double rounding_error_indexedreals_tree(double x, double x_min, double x_max, double y,
				   double y_min, double y_max,unsigned int p_max,
				   unsigned int q,unsigned int d);
void prepare_gappa_input_assumptions(const char *filename,struct task *, ConstThreeVariableList, ConstSequenceOfStates);
void prepare_gappa_file_for_abcs(const char *filename,struct task *, ConstThreeVariableList, ConstSequenceOfStates);

double Gappa_Bounds_of_Summation_Error_Growth_Bound(double Bound_GB_State, double Bound_GB_Uncert,
						  double Bound_App_Err, double Bound_Rnd_Err);
double Gappa_Bounds_of_Summation_Error_General_Solution(double Bound_App_Err, double Bound_Rnd_Err,double Bound_GB);
double Gappa_Bounds_of_Overapproximation_Rounding_Error(double ymin,double ymax,unsigned int q,Expression Mxmin,Expression Mxmax,double xmin,double xmax,double Bound_of_Overapproximation_Radius,double measurement_error,ConstSequenceOfStates S);
#endif
