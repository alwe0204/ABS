#ifndef GAPPA_H
#define GAPPA_H
#include "ast.h"
#include "task.h"
#include "variables.h"
#include "ad.h"
#include "to_string.h"
char *gappa_var_to_string(const char *name,size_t idx,FORMAT f);
char *gappa_interval(double lower, double upper);
char *gappa_bounded_difference(const char *a, const char *b, double lower, double upper);
void output_function_in_gappa(struct function *, struct task *, ConstThreeVariableList, ConstSequenceOfStates);
void output_Bounds_of_out_rounding_errors(FILE *,struct task *, ConstThreeVariableList, ConstSequenceOfStates);
#endif
