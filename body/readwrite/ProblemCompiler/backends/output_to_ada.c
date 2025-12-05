/** @file 
 *@author Alexander Weber (a.weber@unibw.de)
 *@data 07 Dez 2016
 *@brief Functions to output the input in ada
*/
#include <assert.h>
#include <stdio.h>

#include <string.h>
#include <ctype.h>

#include "output_to_ada.h"

#include "expressions.h"
#include "to_string.h"
#include "output_aux.h"
#include "output.h"

void output_function_in_argument_to_ada(FILE * file, const char *comma, const char *name,
					const char *type)
{
    fprintf(file, "%s%s : in %s", comma, name, type);
}

void output_function_in_out_argument_to_ada(FILE * file, const char *comma, const char *name,
					    const char *type)
{
    fprintf(file, "%s%s : in out %s", comma, name, type);
}


void output_inputs_to_ada(FILE * file, FILE * header, const char *type, const struct task *op,
			  ConstSequenceOfStates S)
{
    size_t i;
    char *comma = "";
    Variable ptr;
    if (type != NULL) {
	for (i = 0; i < get_states_len(S); i++) {
	    ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	    if (var_role_of(ptr) == INPUT_VAR) {
		output_function_in_argument_to_ada(file, comma, name_of(ptr),
						   op->ada_type_statevar);
		output_function_in_argument_to_ada(header, comma, name_of(ptr),
						   op->ada_type_statevar);
/*
         if( !strcmp(op->ada_type_statevar,"") ) {
          output_function_in_argument_to_ada(file,comma,name_of(ptr),type);
          output_function_in_argument_to_ada(header,comma,name_of(ptr),type);
         } else {
           output_function_in_argument_to_ada(file,comma,name_of(ptr),op->ada_type_statevar);
           output_function_in_argument_to_ada(header,comma,name_of(ptr),op->ada_type_statevar);
         }
*/
		comma = "; ";
	    }
	}
	for (i = get_states_len(S); i--;) {
	    ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	    if (var_role_of(ptr) == INPUTCONST_VAR) {
		if (!strcmp(name_of(ptr), op->inputvar_name)) {
		    output_function_in_argument_to_ada(file, comma, name_of(ptr),
						       op->ada_type_inputvar);
		    output_function_in_argument_to_ada(header, comma, name_of(ptr),
						       op->ada_type_inputvar);
		} else {
		    output_function_in_argument_to_ada(file, comma, name_of(ptr), type);
		    output_function_in_argument_to_ada(header, comma, name_of(ptr), type);
		}
		comma = "; ";
	    }
	}
    } else {
	char *param = "-- @param";
	for (i = 0; i < get_states_len(S); i++) {
	    ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	    if (var_role_of(ptr) == INPUT_VAR) {
		char *buf = data_of_identifier_to_hyperinterval_string(ptr, MY_FORMAT, MYSTRING, S);
		fprintf(header, "\n%s %s Initial value. Assumptions: %s in %s", param, name_of(ptr),
			name_of(ptr), buf);
		free(buf);
	    }
	}
	for (i = 0; i < get_states_len(S); i++) {
	    ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	    if (var_role_of(ptr) == INPUTCONST_VAR) {
		char *buf = data_of_identifier_to_hyperinterval_string(ptr, MY_FORMAT, MYSTRING, S);
		fprintf(header, "\n%s %s Parameter. Assumptions: %s in %s", param, name_of(ptr),
			name_of(ptr), buf);
	    }
	}
    }
}

void
output_coeff_to_ada(FILE * adb, FILE * ads,
		    char **coeff_strings, const struct task *op,
		    ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    FILE *files[2] = { adb, ads };

    char *comma = "";
    size_t i, idx, j;
    char *v = generate_identifier("v", S);
    char *first = generate_identifier("first", S);
    char *last = generate_identifier("last", S);
    fprintf(ads, "with Interfaces; use Interfaces;\n");
    fprintf(ads, "with types; use types;\n");
    fprintf(ads, "with Ada.Numerics.Generic_Elementary_Functions;\n");
    fprintf(adb, "package body %s is\n", op->coeff_function_name);
    fprintf(adb, "use GenElFun_64;\n");
    fprintf(ads, "package %s is\n", op->coeff_function_name);
    fprintf(ads,
	    "package GenElFun_64 is new Ada.Numerics.Generic_Elementary_Functions(%s);\n",
	    ADA_DOUBLE_TYPE);
    fprintf(ads, "number_of_variables : constant := %ld;\n", get_number_of_variables(tri));
    fprintf(ads, "max_order : constant := %u;\n", op->max_order);

    Variable ptr;
    comma = "";
    for (j = 0; j < 2; j++) {
	fprintf(files[j], "procedure %s(", op->coeff_function_name);
    }
    for (i = 0; i < op->outputstates_len; i++) {
	idx = op->outputstates[i];
	for (j = 0; j < 2; j++) {
	    output_function_in_out_argument_to_ada(files[j], comma, coeff_strings[idx],
						   ADA_IN_TYPE_MAT);
	}
	comma = "; ";
    }
    for (j = 0; j < 2; j++) {
	output_function_in_out_argument_to_ada(files[j], comma, v, "Float_T_Array");
	output_function_in_argument_to_ada(files[j], comma, first, "Natural");
	output_function_in_argument_to_ada(files[j], comma, last, "Natural");
    }
    comma = "; ";
    for (i = 0; i < get_states_len(S); i++) {
	ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	if (var_role_of(ptr) == INPUT_VAR) {
	    for (j = 0; j < 2; j++) {
		output_function_in_argument_to_ada(files[j], comma, name_of(ptr),
						   op->ada_type_statevar);
	    }
	    comma = "; ";
	}
    }
    for (i = 0; i < get_states_len(S); i++) {
	ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	if (var_role_of(ptr) == INPUTCONST_VAR) {
	    char *type;
	    if (!strcmp(op->inputvar_name, name_of(ptr))) {
		type = op->ada_type_inputvar;
	    } else {
		type = ADA_IN_TYPE;
	    }
	    for (j = 0; j < 2; j++) {
		output_function_in_argument_to_ada(files[j], comma, name_of(ptr), type);
	    }
	    comma = "; ";
	}
    }
    fprintf(ads, ");\n");
    fprintf(adb, ") is\n");
    fprintf(adb, "begin\n");
    fprintf(adb, "\nif ");
    comma = "";
/*
    for (i = 0; i < op->outputstates_len; i++) {
	idx = op->outputstates[i];
	fprintf(adb, "%s %s /= null", comma, coeff_strings[idx]);
	comma = " and ";
    }
    fprintf(adb, "%s %s /= null then\n", comma, v);
*/
    output_three_variable_list(adb, ADA_FORMAT, coeff_strings, op->acc_dim, tri, S);
/*
    fprintf(adb, "\nend if;");
*/
    fprintf(adb, "end %s;\n", op->coeff_function_name);
    char *param = "-- @param";
    char *brief = "-- @description";
    fprintf(ads, "%s This procedure computes the Taylor coefficients for order %s up to order %s",
	    brief, first, last);
    fprintf(ads, "\n-- provided that it was previously executed for orders 0 up to %s-1", first);
    fprintf(ads, "\n-- and %s is holding the corresponding auxiliary variables\n", v);
    for (i = 0; i < op->outputstates_len; i++) {
	idx = op->outputstates[i];
	ptr = op->states[idx];
	fprintf(ads, "%s %s Matrix holding the coefficients for state %s: ", param,
		coeff_strings[idx], name_of(ptr));
	fprintf(ads,
		"The entry in the ith row and jth column is the component j of the coefficient of order i-1\n");
    }
    fprintf(ads, "%s %s Array of auxiliary variables. ", param, v);
    fprintf(ads,
	    "Assumptions: 1) %s has length %lu (1-based indexing); 2) %s has already been computed for orders 0 .. %s - 1 \n",
	    v, get_number_of_variables(tri), v, first);
    fprintf(ads, "%s %s Lower coefficient order to compute. \n", param, first);
    fprintf(ads,
	    "%s %s Upper coefficient order to compute. Assumptions: %s is not greater than %d\n",
	    param, last, last, op->max_order);
    for (i = 0; i < get_states_len(S); i++) {
	ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	if (var_role_of(ptr) == INPUT_VAR) {
	    char *buf = data_of_identifier_to_hyperinterval_string(ptr, MY_FORMAT, MYSTRING, S);
	    fprintf(ads, "%s %s Initial value. Assumptions: %s in %s\n", param, name_of(ptr),
		    name_of(ptr), buf);
	    free(buf);
	}
    }
    for (i = 0; i < get_states_len(S); i++) {
	ptr = version_of_variable(get_var(i, S), 0, 0, get_date(S));
	if (var_role_of(ptr) == INPUTCONST_VAR) {
	    char *buf = data_of_identifier_to_hyperinterval_string(ptr, MY_FORMAT, MYSTRING, S);
	    if (!strcmp(op->inputvar_name, name_of(ptr))) {
		fprintf(ads, "%s %s Control value. Assumptions: %s in %s\n", param, name_of(ptr),
			name_of(ptr), buf);
	    } else {
		fprintf(ads, "%s %s Constant parameter. Assumptions: %s in %s\n", param,
			name_of(ptr), name_of(ptr), buf);
	    }
	}
    }
    fprintf(ads, "\nend %s;\n", op->coeff_function_name);
    free(first);
    free(last);
    free(v);
}

void
output_Jacobian_to_ada(FILE * file, FILE * fileheader,
		       char **coeff_strings, char *name,
		       const struct task *op, ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    size_t i;
    const size_t len = get_three_variable_list_len(tri);
    char *v = generate_identifier("v", S);
    for (i = 0; i < len; i++) {
	output_three_variable_list_entry_output(file, ADA_FORMAT,
						NUMERICAL, v, i,
						coeff_strings,
						op->acc_dim, (&output_jacobian_entry), tri, S);
    }
}

void
output_function_declaration_in_ada(FILE * file,
				   const struct function *in,
				   const struct task *op, ConstSequenceOfStates S)
{
    Identifier ptr;
    char *comma = "(";
    char *stars;
    if (FCT_NAME(in) != NULL)
	fprintf(file, "procedure %s", FCT_NAME(in));
    else {
	stars = generate_identifier("fct", S);
	fprintf(file, "procedure %s", stars);
	free(stars);
    }
    for (ptr = (Expression) wind_back(AST_EXPR, in->img); ptr != NULL; ptr = ptr->next) {
	Variable x = find_in_variables(ptr, S);
	x = version_of_variable(x, 0, 0, get_date_of_postfix(ptr, S));
	if (col_dim_of(x) > 1) {
	    stars = "Matrix_Float_T";
	} else {
	    stars = op->ada_type_statevar;
	}
	output_function_in_out_argument_to_ada(file, comma, NAME(ptr), stars);
	comma = "; ";
    }
    for (ptr = (Expression) wind_back(AST_EXPR, in->var); ptr != NULL; ptr = ptr->next) {
	if (!strcmp(NAME(ptr), op->timevar_name))
	    output_function_in_argument_to_ada(file, comma, NAME(ptr), ADA_IN_TYPE_TIME);
	else if (!strcmp(NAME(ptr), op->inputvar_name))
	    output_function_in_argument_to_ada(file, comma, NAME(ptr), op->ada_type_inputvar);
	else if (!strcmp(NAME(ptr), op->statevar_name))
	    output_function_in_argument_to_ada(file, comma, NAME(ptr), op->ada_type_statevar);
	else
	    output_function_in_argument_to_ada(file, comma, NAME(ptr), "Vector_Float_T");
	comma = "; ";
    }
    fprintf(file, ")");
}

void
output_function_in_ada(FILE * file, FILE * header,
		       const struct function *in,
		       const struct task *op, ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    output_function_declaration_in_ada(file, in, op, S);
    output_function_declaration_in_ada(header, in, op, S);
    fprintf(header, ";");
    output_function_comment(header, ADA_FORMAT, in, op, tri, S);
    fprintf(file, "\nis\n");
    output_evaluation_trace_variable(file, ADA_FORMAT, "Float_T_Array", tri, S);
    fprintf(file, "begin\n");
    output_evaluation_trace(file, ADA_FORMAT, NUMERICAL, in, tri, S);
    fprintf(file, "\nend %s;\n", FCT_NAME(in));
}

void Horner_to_string_in_ada(FILE * file, char *out_name, char *time, char *name, dim_t order,
			     dim_t dim, ConstSequenceOfStates S)
{
    char *iter = generate_identifier("j", S);
    char *tmp = generate_identifier("tmp", S);
    char *i = generate_identifier("i", S);

    fprintf(file, "declare\n%s : %s;\nbegin\n", tmp, ADA_DOUBLE_TYPE);
    fprintf(file, "for %s in mat'Range(2) loop\n", i);
    fprintf(file, "%s := 0.0;\n", tmp);
    fprintf(file, "for %s in Component_Index_T range 1 .. mat'Length(1)-1 loop\n", iter);
    fprintf(file, "%s := %s*(%s + %s(%d-%s,%s));", tmp, time, tmp, name, order + 1, iter, i);
    fprintf(file, "\nend loop;\n");
    fprintf(file, "%s(%s) := %s + %s(1,%s);\nend loop ;\n end ;\n", out_name, i, tmp, name, i);
    free(iter);
    free(tmp);
    free(i);
}

void output_taylor_to_ada(FILE * file, FILE * header,
			  const struct ASTNode *in,
			  const struct task *op, ConstThreeVariableList tri,
			  ConstSequenceOfStates S)
{
    FunctionDefinition fct = in->Function;
    Ode ode = in->ode;
    char *v = generate_identifier("v", S);
    const size_t len = get_three_variable_list_len(tri);
    size_t i, k;
    char *comma = "(";
    fprintf(file, "procedure %s", FCT_NAME(ode->phi));
    fprintf(header, "procedure %s", FCT_NAME(ode->phi));
    for (i = 0; i < op->outputstates_len; i++) {
	fprintf(file, "%s%s : in out %s", comma, op->out_strings[i], op->ada_type_statevar);
	fprintf(header, "%s%s : in out %s", comma, op->out_strings[i], op->ada_type_statevar);
	/*    if( !strcmp(op->ada_type_statevar,"") ) {
	   fprintf(file, "%s%s : in out Vector_Float_T_Access", comma, op->out_strings[i]);
	   fprintf(header, "%s%s : in out Vector_Float_T_Access", comma, op->out_strings[i]);
	   } else {
	   fprintf(file, "%s%s : in out %s", comma, op->out_strings[i],op->ada_type_statevar);
	   fprintf(header, "%s%s : in out %s", comma, op->out_strings[i],op->ada_type_statevar);
	   }
	 */
	comma = "; ";
    }
    fprintf(file, "; ");
    fprintf(header, "; ");
    Variable x = NULL;
    for (i = get_states_len(S); i--;) {
	x = get_var(i, S);
	x = version_of_variable(x, 0, 0, get_date(S));
	if (x == NULL)
	    continue;

	if (TIME_VAR == var_role_of(x)) {
	    break;
	}
    }
    assert(x != NULL);
    fprintf(file, "%s : in %s; ", name_of(x), ADA_IN_TYPE_TIME);
    fprintf(header, "%s : in %s; ", name_of(x), ADA_IN_TYPE_TIME);
    output_inputs_to_ada(file, header, "Vector_Float_T", op, S);
    fprintf(file, ")");
    fprintf(header, ")");
    fprintf(header, ";");
    fprintf(file, " is\n");
    output_evaluation_trace_variable(file, ADA_FORMAT, "Float_T_Array", tri, S);
    for (k = 0; k < op->outputstates_len; k++) {
	size_t idx = op->outputstates[k];
	ConstVariable ptr = op->states[idx];
	fprintf(file,
		"type mat is array (Component_Index_T range 1 .. %d, Component_Index_T range 1 .. %d) of %s;\n",
		op->max_order + 1, row_dim_of(ptr), ADA_DOUBLE_TYPE);
	fprintf(file, "%s : mat;\n", op->coeff_strings[idx]);
    }
    fprintf(file, "begin\n");
    for (i = 0; i < len; i++) {
	output_three_variable_list_entry_output(file, ADA_FORMAT, NUMERICAL, v, i,
						op->coeff_strings, op->acc_dim,
						(&output_coefficients_entry), tri, S);
	if (get_role_of_var(i, tri) == STATE_VAR) {
	    char *buf = var_string(v, i, ADA_FORMAT);
	    output_coefficients_entry(file, ADA_FORMAT, i, op->acc_dim, op->coeff_strings, buf,
				      tri);
	    free(buf);
	}
    }

    for (k = 0; k < op->outputstates_len; k++) {
	size_t tmp = op->outputstates[k];
	Horner_to_string_in_ada(file, op->out_strings[k], NAME(ode->time),
				op->coeff_strings[tmp], op->max_order + 1, fct->img_dim[tmp + 1],
				S);
    }
    fprintf(file, "\n");
    fprintf(file, "end %s;\n", FCT_NAME(ode->phi));
}
