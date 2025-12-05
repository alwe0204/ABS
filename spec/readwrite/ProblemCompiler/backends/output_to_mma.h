/** @file 
 * @brief Header for output_to_mma.c
 * @author Alexander Weber (a.weber@unibw.de)
*/
#ifndef OUTPUT_TO_MMA_H
#define OUTPUT_TO_MMA_H

#include "ast.h"
#include "task.h"
#include "variables.h"

void output_function_on_screen(const struct ASTNode *, ConstSequenceOfStates);
void output_function_in_mma(const struct ASTNode *, struct task *, ConstSequenceOfStates);
#endif
