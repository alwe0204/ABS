/** @file
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 01 Dez 2016
 * @brief Header for semantics_abcs.c
*/
#ifndef SEMANTICS_ABCS_H
#define SEMANTICS_ABCS_H

#include "ast.h"
#include "task.h"
#include "variables.h"

#define ABCS_INTERVAL_KEYWORD_TIME_VARIABLE "t"
#define ABCS_INTERVAL_KEYWORD_STATE_VARIABLE "x"
#define ABCS_INTERVAL_KEYWORD_INPUT_VARIABLE "u"

void semantics_Option(struct ASTNode *,struct task *,ConstSequenceOfStates);
#endif
