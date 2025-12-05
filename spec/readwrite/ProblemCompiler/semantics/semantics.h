/**
 * @file semantics.h
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 3 Feb 2016
 * @brief Header for semantics.c
*/
#ifndef VISITOR_H
#define VISITOR_H

#include "ast.h"
#include "variables.h"

void semantics_initial_statement_list(struct ASTNode *, SequenceOfStates);
void semantics_function_definition(struct ASTNode *, SequenceOfStates);
void semantics_ode_definition(struct ASTNode *, SequenceOfStates);
#endif
