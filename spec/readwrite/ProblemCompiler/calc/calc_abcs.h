#ifndef CALC_ABCS_H
#define CALC_ABCS_H

#include "ad.h"

void malloc_abcs_data(struct task *op);
void calc_Sampling_Time(struct ASTNode *, struct task *,
						ConstSequenceOfStates);
void calc_Bounds_of_Approximation_Error(double *result,order_t order,struct task *op);
void calc_Bounds_of_Input_Values_Rounding_Error(struct ASTNode *, struct task *,ConstSequenceOfStates);
void calc_Bounds_of_Center_of_Cells_Rounding_Error(struct ASTNode *, struct task *,ConstSequenceOfStates);

void calc_Bounds_of_Summation_Error_Growth_Bound(struct task *);
void calc_Bounds_of_Summation_Error_General_Solution(struct task *);
void calc_Bounds_of_Overapproximation_Radius(struct task *op);
void calc_Bounds_of_Overapproximation(struct ASTNode *,struct task *,ConstSequenceOfStates);
#endif
