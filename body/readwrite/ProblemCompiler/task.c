/** @file task.c
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 14 Nov 2016 
 * @brief This file organizes the execution of the 
 * various source code generation procedures
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <limits.h>
#include <unistd.h>		/* for unlink */
#include <assert.h>
#include <math.h>
#include "my_strcat.h"
#include "expressions.h"
#include "task.h"
#include "ad.h"
#include "to_string.h"
#include "output.h"
#include "output_aux.h"
#include "abcs.h"
#include "semantics_abcs.h"
#include "calc_abcs.h"
#include "output_to_ada.h"
#include "output_to_mma.h"
#include "output_to_c.h"
#include "ia.h"
#include "errors.h"
#include "errors_abcs.h"


#include "gappa.h"
#include "gappa_abcs.h"
#include "gappa_utils.h"

#define GROWTH_BOUND_DIR "GrowthBounds"
#define REMAINDER_DIR "Remainder"

#define SECTION(X) fprintf(stdout,"- %s:\n",X);
#define DONE fprintf(stdout,"  Done.\n");

void run_compiler(int numArgs, ...)
{
    va_list args;
    va_start(args, numArgs);
    int i;
    char *buf = "";
    my_strcat(&buf, "./compiler ");
    for (i = 0; i < numArgs; i++) {
	char *str = va_arg(args, char *);
	my_strcat(&buf, str);
    }
    if (system(buf)) {
	ERROR_SYSTEM_CALL("Problem Compiler", buf);
    }
    free(buf);
    va_end(args);
}

void run_system_call(int numArgs, ...)
{
    va_list args;
    va_start(args, numArgs);
    int i;
    char *buf = "";
    for (i = 0; i < numArgs; i++) {
	char *str = va_arg(args, char *);
	my_strcat(&buf, str);
    }
    if (system(buf)) {
	ERROR_SYSTEM_CALL("System call", buf);
    }
    free(buf);
    va_end(args);
}


FILE *my_fopen(const char *directory, const char *filename, const char *filename_extension,
	       const char *mode)
{
    char *buf = "";
    my_strcat_several(&buf, 5, directory, "/", filename, ".", filename_extension);
    FILE *file = fopen(buf, mode);
    assert(file != NULL);
    free(buf);
    return file;
}

void my_remove_file(const char *directory, const char *filename, const char *filename_extension)
{
    char *buf = "";
    my_strcat_several(&buf, 5, directory, "/", filename, ".", filename_extension);
    unlink(buf);
    free(buf);
}

void my_rename_file(const char *directory, const char *filename_old, const char *filename_new)
{
    char *buf = "";
    char *buf2 = "";
    my_strcat_several(&buf, 3, directory, "/", filename_old);
    my_strcat_several(&buf2, 3, directory, "/", filename_new);
    rename(buf, buf2);
    free(buf);
    free(buf2);
}

void read_codomain(FILE * file, double *out)
{
    unsigned int i = 0;
    char buffer[256] = { 0 };
    char *buf;
    while (!feof(file)) {
	if (fgets(buffer, sizeof(buffer), file) != NULL) {
	    buf = strtok(buffer, "[,]");
	    out[i] = strtod(buf, NULL);
	    assert(isfinite(out[i]));
	    i++;
	}
    }
    rewind(file);
    while (!feof(file)) {
	if (fgets(buffer, sizeof(buffer), file) != NULL) {
	    buf = strtok(buffer, "[,]");
	    buf = strtok(NULL, "[,]");
	    out[i] = strtod(buf, NULL);
	    assert(isfinite(out[i]));
	    i++;
	}
    }
}

void read_codomain_magnitude(FILE * file, double *out)
{
    unsigned int i = 0;
    char buffer[256] = { 0 };
    char *buf;
    while (!feof(file)) {
	if (fgets(buffer, sizeof(buffer), file) != NULL) {
	    buf = strtok(buffer, "[,]");
	    out[i] = fabs(strtod(buf, NULL));
	    buf = strtok(NULL, "[,]");
	    out[i] = fmax(out[i], fabs(strtod(buf, NULL)));
	    assert(isfinite(out[i]));
	    i++;
	}
    }
}

/** @brief Comparison function to be used with qsort in prepare_the_task()
 * @param a First operand
 * @param b Second operand
 */

int compare(const void *a, const void *b)
{
    if (*(dim_t *) a > *(dim_t *) b)
	return 1;
    if (*(dim_t *) a == *(dim_t *) b)
	return 0;
    if (*(dim_t *) a < *(dim_t *) b)
	return -1;
    return 0;
}


/** @brief This method prepare the data struction struct task()
 * @param fct The explicitly defined function
 * @param dimension_of_ode The dimension of of the ordinary differential equation
 * @param op The task to do
 * @param tri The three variable list
 * @param S The sequence of states
*/

void prepare_the_task(FunctionDefinition fct, dim_t dimension_of_ode, struct task *op,
		      ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    const size_t len = fct->img_dim[0];
    op->states = my_malloc(len * sizeof(struct variable *));
    op->outputstates = my_malloc(len * sizeof(dim_t));
    op->outputstates_len = 0;
    op->states_len = 0;

    dim_t i;

    op->acc_dim = fct->img_acc_dim;
    Variable x;
    for (i = 0; i < get_states_len(S); i++) {
	x = get_var(i, S);
	x = version_of_variable(x, 0, 0, get_date(S));
	if (x == NULL)
	    continue;
	if (var_role_of(x) == STATE_VAR || var_role_of(x) == OUTPUTSTATE_VAR) {
	    (op->states_len)++;
	    op->states[var_idx(x) - 1] = x;
	    if (var_role_of(x) == OUTPUTSTATE_VAR) {
		op->outputstates[op->outputstates_len] = var_idx(x) - 1;
		(op->outputstates_len)++;
	    }
	}
    }
    qsort(op->outputstates, op->outputstates_len, sizeof(dim_t), compare);
    make_strings(op, S);
}

/**@brief This method organizes the execution of 
 * automatic differentiation
 * @param op The task to do
 * @param tri The three variable list
 */

void do_ad(struct task *op, ThreeVariableList tri)
{
    const dim_t order = op->max_order;
    switch (op->method) {
    case TASK_METHOD_RECURSIVE:
	method_recursive(order, tri);
	break;
    case TASK_METHOD_QUADRATIC:
	method_quardatic_in_order(order, tri);
	break;
    case TASK_METHOD_LOOPS:
	method_quardatic_in_order(1, tri);
	break;
    case TASK_JACOBIAN:
	op->max_order = 2;
	op->coeff_function_name = "Jacobian";
	make_Jacobian(tri);
	break;
    default:
	assert(0);
    }
}

/**@brief This method organizes the generation
 * of the source code for the integration formula
 * for the growth bound
 * @param c The file to write the source code in C
 * @param h The file to write the header for \p c
 * @param adb The file to write the source code in Ada
 * @param ads The file to write the specification for \p adb
 * @param op The task to do
 */

void do_growth_bounds(FILE * c, FILE * h, FILE * adb, FILE * ads, struct task *op)
{
    char *buf = "";
    char *folderbuf = "";

    /* Begin: Set up directory */
    my_strcat_several(&folderbuf, 3, op->output_directory, "/", GROWTH_BOUND_DIR);
    run_system_call(2, "mkdir -p ", folderbuf);
    /* End: Set up directory */

    /* Begin: Write growth bound formula */
    my_strcat_several(&buf, 4, folderbuf, "/", "MatrixExp.", FILENAME_EXTENSION);
    FILE *file = fopen(buf, "w");
    assert(file != NULL);

    output_matrix_exponential_template(file, op);
    fclose(file);
    /* End: Write growth bound formula */

    /* Begin: Compute integration formula */
    SECTION("Growth Bound Formula");
    order_t growthbound_order = op->max_order_growth_bound;
    char *ord = int_to_string(growthbound_order, SYMBOLIC);
    run_compiler(12,
		 "-outputdir=", folderbuf,
		 " -statevar-ada-type=State_Radius_T",
		 " -nomma-file",
		 " -nosimplify",
		 " -inputvar-ada-type=Bounds_of_Dynamic_Uncertainties_T",
		 " -inputvar=w", " -quiet", " -order=", ord, " ", buf);
    free(ord);
    my_remove_file(folderbuf, "MatrixExp", FILENAME_EXTENSION);
    free(buf);
    buf = "";
    DONE;
    /* End: Compute integration formula */

    /* Begin: Compute bounds on approximation errors of integration formula */
    SECTION("Bounds on Approximation Errors of Growth Bound Formula");
    run_compiler(16,
		 " -quiet",
		 " -nosimplify",
		 " -nomma-file",
		 " -timevar=", ABCS_INTERVAL_KEYWORD_TIME_VARIABLE,
		 " -inputvar=w",
		 " -statevar-ada-type=State_Radius_T",
		 " -statevar=", "r0",
		 " -inputvar-ada-type=Bounds_of_Dynamic_Uncertainties_T",
		 " -outputdir=", folderbuf, " ", folderbuf, "/Integration.", FILENAME_EXTENSION);
    my_rename_file(folderbuf, "/Function.adb", "Integration.adb");
    my_rename_file(folderbuf, "/Function.c", "Integration.c");
    my_rename_file(folderbuf, "/Function.ads", "Integration.ads");
    my_rename_file(folderbuf, "/Function.h", "Integration.h");
    DONE;
    /* End: Compute bounds on approximation errors of integration formula */

    /* Begin: Copy integration formula to source code file */
    file = my_fopen(folderbuf, "Integration", "adb", "r");
    copy_file_content(adb, file);
    fclose(file);
    my_remove_file(folderbuf, "Integration", "adb");
    file = my_fopen(folderbuf, "Integration", "ads", "r");
    copy_file_content(ads, file);
/*
    fprintf(ads, "\n-- @param x_out The result of the integration.\n");
    fprintf(ads, "-- @param t The time parameter. Assumption: t in R_+\n");
    fprintf(ads, "-- @param r0 The initial value. Assumption: r0 in R_+^%d\n",
	    op->state_space_dimension);
    fprintf(ads, "-- @param L The Lipschitz-Matrix as an array row-by-row. Assumption: L in R^%d\n",
	    op->state_space_dimension * op->state_space_dimension);
    fprintf(ads, "-- @param v The vector specifying the disturbance. Assumption: v in R_+^%d\n",
	    op->state_space_dimension);
*/
    fprintf(ads,
	    "-- @description Numerical integration formula of order %u for the solution at time t of x_out' = L*x_out + w, x_out(0)=r0.",
	    growthbound_order);
    fprintf(adb, "\n");
    fprintf(ads, "\n");

    fclose(file);
    my_remove_file(folderbuf, "Integration", "ads");

    file = my_fopen(folderbuf, "Integration", "c", "r");
    copy_file_content(c, file);
    fclose(file);
    my_remove_file(folderbuf, "Integration", "c");

    file = my_fopen(folderbuf, "Integration", "h", "r");
    copy_file_content(h, file);
    fclose(file);
    my_remove_file(folderbuf, "Integration", "h");
    /* End: Copy integration formula to source code file */

    /* Begin: Compute approximation errors */
    file = my_fopen(folderbuf, "Approximation_Error", "dat", "r");
    read_codomain_magnitude(file, op->Bounds_of_Approximation_Error_Growth_Bound);
    fclose(file);
    my_remove_file(folderbuf, "Approximation_Error", "dat");

    calc_Bounds_of_Approximation_Error(op->Bounds_of_Approximation_Error_Growth_Bound,
				       op->max_order_growth_bound + 1, op);
    /* End: Compute approximation errors */

    /* Begin: Compute rounding errors */
    SECTION("Bounds on Rounding Errors of Growth Bound Formula");
    run_compiler(10,
		 " -quiet",
		 " -nosimplify",
           " -nomma-file",
		 " -gappa",
		 " -outputdir=", folderbuf, " ", folderbuf, "/Integration.", FILENAME_EXTENSION);

    file = my_fopen(folderbuf, "Bounds_of_Rounding_Errors", "dat", "r");
    read_list(file, op->Bounds_of_Rounding_Error_Growth_Bound);
    fclose(file);
    my_remove_file(folderbuf, "Bounds_of_Rounding_Errors", "dat");
    DONE;
    /* End: Compute rounding errors */

    /* Begin: Compute codomain of integration formula */
    run_compiler(8, "-quiet", " -codomain", " -outputdir=", folderbuf, " ", folderbuf,
		 "/Integration.", FILENAME_EXTENSION);
    file = my_fopen(folderbuf, "Function_codomain", "dat", "r");
    read_codomain_magnitude(file, op->Growth_Bound_Codomain);
    fclose(file);

    my_remove_file(folderbuf, "Function_codomain", "dat");
    my_remove_file(folderbuf, "Integration", FILENAME_EXTENSION);
    my_remove_file(folderbuf, "taylorcoefficient_last_plus_1", FILENAME_EXTENSION);
    free(folderbuf);
    /* End: Compute codomain of integration formula */
}

/** @brief This method organizes the writing of various files
 * @param in The abstract syntax tree
 * @param op The task to do
 * @param tri The three variable list 
 * @param S The sequence of states
*/

void write_files(const struct ASTNode *in, struct task *op, ThreeVariableList tri,
		 ConstSequenceOfStates S)
{
    FILE *file, *c = NULL, *h = NULL, *adb = NULL, *ads = NULL, *c2 = NULL, *h2 = NULL, *adb2 =
	NULL, *ads2 = NULL;

    char *buf = "";
    char *folder = op->output_directory;
    if (IS_VERBOSE(op))
	run_system_call(2, "mkdir -p -v ", folder);
    else
	run_system_call(2, "mkdir -p ", folder);
    switch (op->method) {
    case TASK_FUNCTION:
	{
	    if (IS_CODOMAIN(op)) {
		FILE *dat = my_fopen(folder, "Function_codomain", "dat", "w+");
		output_function_image_set(dat, MY_FORMAT, S);
		fclose(dat);
	    }
	    if (IS_GAPPA(op)) {
		clean_three_variable_list(tri);
		output_function_in_gappa(in->Function, op, tri, S);
		FILE *dat = my_fopen(folder, "Bounds_of_Rounding_Errors", "dat", "w+");
		output_Bounds_of_out_rounding_errors(dat, op, tri, S);
		fclose(dat);
	    }
	    if (IS_CODOMAIN(op) || IS_GAPPA(op))
		return;

	    c = my_fopen(folder, "Function", "c", "w+");
	    h = my_fopen(folder, "Function", "h", "w+");
	    c2 = my_fopen(folder, "Function_ia", "c", "w+");
	    h2 = my_fopen(folder, "Function_ia", "h", "w+");
	    adb = my_fopen(folder, "Function", "adb", "w+");
	    ads = my_fopen(folder, "Function", "ads", "w+");
	}
	break;
    case TASK_METHOD_LOOPS:
    case TASK_JACOBIAN:
	{
	    c = my_fopen(folder, op->coeff_function_name, "c", "w+");
	    h = my_fopen(folder, op->coeff_function_name, "h", "w+");
	}
	break;
    default:
	{
	    adb = my_fopen(folder, op->coeff_function_name, "adb", "w+");
	    ads = my_fopen(folder, op->coeff_function_name, "ads", "w+");
	    c = my_fopen(folder, op->coeff_function_name, "c", "w+");
	    h = my_fopen(folder, op->coeff_function_name, "h", "w+");
	    c2 = my_fopen(folder, "Integration", "c", "w+");
	    h2 = my_fopen(folder, "Integration", "h", "w+");
	    adb2 = my_fopen(folder, "Integration", "adb", "w+");
	    ads2 = my_fopen(folder, "Integration", "ads", "w+");
	}
	break;
    }
    switch (op->method) {
    case TASK_FUNCTION:
	{
	    ThreeVariableList tri2 = copy_evaluation_trace(tri);
	    clean_three_variable_list(tri);

	    output_function_in_c(c, h, in->Function, op, tri, S);
	    fclose(c);
	    fclose(h);

	    output_function_in_ada(adb, ads, in->Function, op, tri, S);
	    fclose(adb);
	    fclose(ads);

	    fprintf(c2, "#include \"Function_ia.h\"\n");
	    fprintf(h2, "#ifndef FUNCTION_IA_H\n");
	    fprintf(h2, "#define FUNCTION_IA_H\n");
	    fprintf(h2, "#include \"interval_library.h\"\n");
	    SET_TRI(op);
	    SET_KEEPDEC(op);
	    clean_three_variable_list(tri2);
	    output_function_in_c_ia(c2, h2, in->Function, op, tri2, S);
	    fprintf(h2, "\n#endif\n");
	    fclose(c2);
	    fclose(h2);
	}
	break;
    case TASK_METHOD_RECURSIVE:
    case TASK_METHOD_QUADRATIC:
	{
	    ThreeVariableList tri2 = copy_evaluation_trace(tri);
	    do_ad(op, tri);
	    int a = 0;
	    int b = 0;
	    if (!IS_TRI(op))
		a = 1;
	    if (!IS_KEEPDEC(op))
		b = 1;
	    SET_TRI(op);
	    SET_KEEPDEC(op);
	    op->max_order++;
	    do_ad(op, tri2);
	    if (a)
		op->bits &= ~(1 << 3);
	    if (b)
		op->bits &= ~(1 << 5);
	    op->max_order--;

	    file = my_fopen(folder, "taylorcoefficients", FILENAME_EXTENSION, "w+");
	    output_coefficients(file, MY_FORMAT, op->coeff_strings, op->coeff_function_name, op,
				tri, S);
	    fclose(file);

	    output_coeff_to_ada(adb, ads, op->coeff_strings, op, tri, S);
	    fclose(adb);
	    fclose(ads);
	    output_coeff_to_c(c, h, op->coeff_strings, op, tri, S);
	    fclose(c);
	    fclose(h);
	    file = my_fopen(folder, "Integration", FILENAME_EXTENSION, "w+");
	    output_taylor(file, in, op, tri, S);
	    fclose(file);
	    output_taylor_to_c(c2, h2, in, op, tri, S);
	    fclose(c2);
	    fclose(h2);
	    output_taylor_to_ada(adb2, ads2, in, op, tri, S);
	    fclose(adb2);
	    fclose(ads2);
	    if (IS_ABCS(op)) {
		Variable x = find_in_variables_match_role_at_date(INPUT_VAR, get_date(S), S);
		dim_t i;
		for (i = 0; i < op->state_space_dimension; i++) {
		    buf = double_to_string(op->apriori_enclosure[i], MYSTRING);
		    Expression a = install_Number(NULL, NUM_DEC, buf);
		    free(buf);
		    buf =
			double_to_string(op->apriori_enclosure[i + op->state_space_dimension],
					 MYSTRING);
		    Expression b = install_Number(NULL, NUM_DEC, buf);
		    free(buf);
		    Expression expr = install_expression(CONT_INTV, (void *) a, (void *) b);
		    write_data_1(expr, x, i, 0, S);
		}
	    }
	    file = my_fopen(folder, "taylorcoefficient_last_plus_1", FILENAME_EXTENSION, "w+");
	    output_concrete_coefficient(file, MY_FORMAT, op->max_order + 1, op->coeff_strings,
					"last_plus_1", op, tri2, S);
	    fclose(file);
	    buf = "";
	    run_compiler(8, "-quiet ", "-codomain ", "-outputdir=", folder, " ", folder,
			 "/taylorcoefficient_last_plus_1.", FILENAME_EXTENSION);
	    run_system_call(5, "mv ", folder, "/Function_codomain.dat ", folder,
			    "/Approximation_Error.dat");
	    break;
	}
    case TASK_METHOD_LOOPS:
	fprintf(stderr, "Algorithm not contained in this version.\n");
	exit(EXIT_FAILURE);
	break;
    case TASK_JACOBIAN:
	SET_TRI(op);
	SET_KEEPDEC(op);
	do_ad(op, tri);

	my_strcat_several(&buf, 4, folder, "/", "Jacobian.", FILENAME_EXTENSION);
	file = fopen(buf, "w+");
	free(buf);
	buf = "";
	output_Jacobian(file, MY_FORMAT, op->coeff_strings, op->coeff_function_name, op, tri, S);
	fclose(file);

	fprintf(c, "#include \"Jacobian.h\"\n");
	fprintf(h, "#ifndef JACOBIAN_H\n");
	fprintf(h, "#define JACOBIAN_H\n");
	fprintf(h, "#include \"interval_library.h\"\n");
	output_Jacobian_to_c(c, h, op->coeff_strings, op->coeff_function_name, op, tri, S);
	fprintf(h, "#endif\n");
	fclose(c);
	fclose(h);
	run_compiler(6, "-quiet ", "-onlyscreen ", folder, "/", "Jacobian.", FILENAME_EXTENSION);
	run_compiler(7, "-quiet -codomain -outputdir=", folder, " ", folder,
		     "/", "Jacobian.", FILENAME_EXTENSION);
	break;
    default:
	assert(0);
    }
}

/** @brief The method asks the user about the orders of integration
 * if those have not been specified otherwise
 * @param op The task to do
 */
void ask_for_order(struct task *op)
{
    order_t user_order;
    if (op->max_order == 0 || op->max_order_growth_bound == 0) {
	fprintf(stdout, "No integration order is specified in the input file.\n");
    }
    if (op->max_order == 0) {
	fprintf(stdout,
		"Which order should be used for the integration formula of the right hand side?\n");
	if (scanf("%hu", &user_order) != 1) {
	    ERROR_USERINPUT_ORDER;
	} else {
	    op->max_order = user_order;
	}
    }
    if (op->max_order_growth_bound == 0) {
	fprintf(stdout,
		"Which order should be used for the integration formula of the growth bounds?\n");
	if (scanf("%hu", &user_order) != 1) {
	    ERROR_USERINPUT_ORDER;
	} else
	    op->max_order_growth_bound = user_order;
    }
}


void do_the_task_for_ode(struct ASTNode *in, struct task *op, ThreeVariableList * tri,
			 ThreeVariableList * tri2, ConstSequenceOfStates S)
{
    FunctionDefinition const fct = in->Function;
    Ode const ode = in->ode;
    const dim_t dimension_of_ode = ode->dim;
    const dim_t *image_dimensions = fct->img_dim;

    init_three_variable_list_for_ode(dimension_of_ode, op->acc_dim, image_dimensions[0], *tri);
    prepare_the_task(fct, dimension_of_ode, op, *tri, S);
    evaluation_trace_for_ode(op->states, op->states_len, *tri, S);
    if (IS_ABCS(op)) {
	op->ada_type_statevar = "State_T";
	op->ada_type_inputvar = "Input_T";
	op->inputvar_name = ABCS_INTERVAL_KEYWORD_INPUT_VARIABLE;
	*tri2 = copy_evaluation_trace(*tri);
    }
    write_files(in, op, *tri, S);
}

/** @brief This is the main method for generating source code
 * @param in The abstract syntax tree
 * @param op The task to do
 * @parma S The sequence of states
*/
void do_the_task(struct ASTNode *in, struct task *op, ConstSequenceOfStates S)
{
    char *folder = op->output_directory;
    char *buf = "";

    Ode const ode = in->ode;
    Option const opt = in->options;

    ThreeVariableList tri = install_three_variable_list();
    ThreeVariableList tri2 = NULL;

    order_t order = op->max_order;

    if (opt != NULL) {
	SET_ABCS(op);
	if (!IS_WITHOUTODE(op)) {
	    ask_for_order(op);
	    order = op->max_order;
	}
    }

    if (IS_ABCS(op) && !IS_WITHOUTODE(op)) {
	fprintf(stdout, "======================================\n");
	fprintf(stdout, "========== PROBLEM COMPILER ==========\n");
	fprintf(stdout, "======================================\n");
    }

    if (!IS_QUIET(op))
	check_used_variables(S);

    if (IS_ONLYSCREEN(op)) {
	output_function_on_screen(in, S);
	return;
    }

    if (!IS_QUIET(op)) {
	output_function_on_screen(in, S);
    }
    if (!IS_CODOMAIN(op) && !IS_NOMMAFILE(op)) {
	output_function_in_mma(in, op, S);
    }

    if (ode == NULL) {
	op->method = TASK_FUNCTION;
	evaluation_trace(tri, S);
	evaluate_function(S);
	write_files(in, op, tri, S);
    }

    if (ode == NULL || order == 0)
	return;

    do_the_task_for_ode(in, op, &tri, &tri2, S);
    if (!IS_ABCS(op))
	return;



    /* Begin: Create main problem-specific source code files */
    char *name = "dynamics_specification_parameters";
    FILE *c = my_fopen(folder, name, "c", "w");
    FILE *h = my_fopen(folder, name, "h", "w");
    FILE *adb = my_fopen(folder, name, "adb", "w");
    FILE *ads = my_fopen(folder, name, "ads", "w");
    buf = ToUpperCase(name);
    fprintf(h, "#ifndef %s_H\n", buf);
    fprintf(h, "#define %s_H\n", buf);
    free(buf);
    buf = "";

    fprintf(c, "#include <stdlib.h>\n");
    fprintf(c, "#include <math.h>\n");
    fprintf(c, "#include \"%s.h\"\n", name);
    fprintf(h, "typedef unsigned int dim_t ;\n");

    fprintf(ads, "with Interfaces.C; use Interfaces.C;\n");
    char *types[] = { "Established_Types" };
    int i;
    for (i = 0; i < sizeof(types) / sizeof(types[0]); i++) {
	fprintf(ads, "with %s; use %s;\n", types[i], types[i]);
    }
    fprintf(ads, "with Ada.Numerics.Generic_Elementary_Functions;\n");
    fprintf(ads, "package %s is\n", name);
    /* fprintf(ads,
	    "package GenElFun_64 is new Ada.Numerics.Generic_Elementary_Functions(%s);\n",
	    ADA_DOUBLE_TYPE); */ /* Removed on 25/10/2021 */

    fprintf(adb, "with Ada.Unchecked_Deallocation;\n");
    fprintf(adb, "with math_functions; use math_functions;\n"); /* NEW on 25/10/2021 */
    fprintf(adb, "package body %s is\n", name);
    /* fprintf(adb, "use GenElFun_64;\n"); */ /* Removed on 25/10/2021 */
    /* End: Create main problem-specific source code files */


    /* Begin: Computations (1) */
    malloc_abcs_data(op);
    calc_Sampling_Time(in, op, S);
    calc_Bounds_of_Input_Values_Rounding_Error(in, op, S);
    calc_Bounds_of_Center_of_Cells_Rounding_Error(in, op, S);
    /* End: Computations (1) */


    /* Begin: Print data (1) */
    FILE *files[2] = { c, adb };
    FILE *headers[2] = { h, ads };
    enum output_format formats[2] = { C_FORMAT, ADA_FORMAT };
    for (i = 0; i < 2; i++) {
	output_dimension_of_spaces(files[i], headers[i], formats[i], op);
	output_num_of_periods(files[i], headers[i], formats[i], op);
	output_periods(files[i], headers[i], formats[i], in, op, S);
	output_uncertainties(files[i], headers[i], formats[i], in, op, S);
	output_measurementerrors(files[i], headers[i], formats[i], in, op, S);
	output_samplingtime(files[i], headers[i], formats[i], in, S);
	output_initialset(files[i], headers[i], formats[i], in, op, S);
	output_targetset(files[i], headers[i], formats[i], in, op, S);
	output_obstacleset(files[i], headers[i], formats[i], in, op, S);
	output_operatingrange(files[i], headers[i], formats[i], in, op, S);
	output_inputspace(files[i], headers[i], formats[i], in, op, S);
	output_state_space_discretization(files[i], headers[i], formats[i], in, op, S);
	output_input_space_discretization(files[i], headers[i], formats[i], in, op, S);
	output_rounded_initial_state_radius(files[i], headers[i], formats[i], in, op, S);
	output_Bounds_of_Input_Value_Rounding_Error(files[i], headers[i], formats[i], in, op, S);
    }
    /* End: Print data (1) */


    /* Begin: Compute Integration formula for general solution */
    SECTION("General Solution Approximation");
    run_compiler(18,
		 "-quiet",
		 " -nosimplify",
		 " -nomma-file",
		 " -timevar=", ABCS_INTERVAL_KEYWORD_TIME_VARIABLE,
		 " -inputvar=", ABCS_INTERVAL_KEYWORD_INPUT_VARIABLE,
		 " -statevar=", ABCS_INTERVAL_KEYWORD_STATE_VARIABLE, "0",
		 " -statevar-ada-type=State_T",
		 " -inputvar-ada-type=Input_T",
		 " -outputdir=", folder, " ", folder, "/Integration.", FILENAME_EXTENSION);
    my_rename_file(folder, "/Function.adb", "Integration.adb");
    my_rename_file(folder, "/Function.c", "Integration.c");
    my_rename_file(folder, "/Function.ads", "Integration.ads");
    my_rename_file(folder, "/Function.h", "Integration.h");
    DONE;
    /* End: Compute Integration formula for general solution */

    /* Begin: Print data (2) - general solution formula */
    FILE *file = my_fopen(folder, "Integration", "adb", "r");
    copy_file_content(adb, file);
    fclose(file);
    my_remove_file(folder, "Integration", "adb");
    file = my_fopen(folder, "Integration", "ads", "r");
    copy_file_content(ads, file);
    fprintf(ads,
	    "-- @description Numerical integration formula of order %u for the solution at time t of x_out' = f(x_out,u), x_out(0)=x0.",
	    order);
    fclose(file);
    my_remove_file(folder, "Integration", "ads");

    file = my_fopen(folder, "Integration", "c", "r");
    copy_file_content(c, file);
    fclose(file);
    my_remove_file(folder, "Integration", "c");

    file = my_fopen(folder, "Integration", "h", "r");
    copy_file_content(h, file);
    fclose(file);
    my_remove_file(folder, "Integration", "h");

    fprintf(adb, "\n");
    fprintf(ads, "\n");
    /* End: Print data (2) - general solution formula */

    /* Begin: Compute bounds on rounding errors and codomain of general solution formula */
    SECTION("Bounds on Rounding Errors of General Solution Approximation");
    prepare_gappa_input_assumptions("tmp.gg", op, tri, S);
    run_compiler(12,
		 "-quiet",
		 " -nosimplify",
		 " -codomain",
		 " -gappa -gappa-input-assumptions=tmp.gg",
		 " -timevar=", ABCS_INTERVAL_KEYWORD_TIME_VARIABLE,
		 " -outputdir=", folder, " ", folder, "/Integration.", FILENAME_EXTENSION);
    unlink("tmp.gg");
    file = my_fopen(folder, "Function_codomain", "dat", "r");
    read_codomain(file, op->General_Solution_Formula_Codomain);
    fclose(file);
    my_remove_file(folder, "Function_codomain", "dat");
    my_remove_file(folder, "Integration", FILENAME_EXTENSION);
    DONE;
    /* End: Compute bounds on rounding errors and codomain of general solution formula */

    /* Begin: Compute bounds of approximation error of general solution */
    SECTION("Bounds on Approximation Errors of General Solution Approximation");
    file = my_fopen(folder, "Approximation_Error", "dat", "r");
    read_codomain_magnitude(file, op->Bounds_of_Approximation_Error_General_Solution);
    fclose(file);
    calc_Bounds_of_Approximation_Error(op->Bounds_of_Approximation_Error_General_Solution,
				       order + 1, op);
    my_remove_file(folder, "Approximation_Error", "dat");
    run_system_call(4, "mkdir -p ", folder, "/", REMAINDER_DIR);
    run_compiler(16,
		 " -inputvar=", ABCS_INTERVAL_KEYWORD_INPUT_VARIABLE,
		 " -statevar=", ABCS_INTERVAL_KEYWORD_STATE_VARIABLE,
		 " -statevar-ada-type=State_T ",
		 " -inputvar-ada-type=Input_T ",
		 " -nosimplify",
		 " -nomma-file",
		 " -outputdir=",
		 folder, "/", REMAINDER_DIR, " -quiet ", folder, "/taylorcoefficient_last_plus_1.",
		 FILENAME_EXTENSION);
    buf = malloc(10 * sizeof(char));
    sprintf(buf, "%u", order + 1);
    run_system_call(7, "sed -i \"4i #define ORDER_REMAINDER ", buf,
		    "\ns/FUNCTION_IA_H/REMAINDER_H/g\" ", folder, "/", REMAINDER_DIR,
		    "/Function_ia.h");
    run_system_call(5, "sed -i \"5i const unsigned int order_remainder = ORDER_REMAINDER ;\" ",
		    folder, "/", REMAINDER_DIR, "/Function_ia.h");
    free(buf);
    buf = "";
    run_system_call(5,
		    "sed -i \"s/NUMBER_OF_VARIABLES/NUMBER_OF_VARIABLES_REMAINDER/g \ns/number_of_variables/number_of_variables_remainder/g\" ",
		    folder, "/", REMAINDER_DIR, "/Function_ia.h");

    my_strcat_several(&buf, 3, folder, "/", REMAINDER_DIR);
    my_remove_file(folder, "taylorcoefficient_last_plus_1", FILENAME_EXTENSION);
    my_remove_file(buf, "/Function", "c");
    my_remove_file(buf, "/Function", "h");
    my_remove_file(buf, "/Function", "adb");
    my_remove_file(buf, "/Function", "ads");
    free(buf);
    buf = "";
    run_system_call(5, "sed -i \"s/Function_ia.h/Remainder.h/g\" ", folder, "/", REMAINDER_DIR,
		    "/Function_ia.c");
    run_system_call(7, "mv ", folder, "/", REMAINDER_DIR, "/Function_ia.c ", folder,
		    "/Remainder.c");
    run_system_call(7, "mv ", folder, "/", REMAINDER_DIR, "/Function_ia.h ", folder,
		    "/Remainder.h");
    run_system_call(4, "rm -rf ", folder, "/", REMAINDER_DIR);
    DONE;
    /* End: Compute bounds of approximation error of general solution */

    /* Begin: Generate source code related to the right hand side - and not to the ode. */
    SECTION("Code associated with Right Hand Side");
    if (IS_TRI(op)) {
	run_compiler(10,
		     " -inputvar=", ABCS_INTERVAL_KEYWORD_INPUT_VARIABLE,
		     " -statevar=", ABCS_INTERVAL_KEYWORD_STATE_VARIABLE,
		     " -statevar-ada-type=State_T ",
		     " -inputvar-ada-type=Input_T ",
		     "-outputdir=", folder, " -quiet -nofolding -without-ode ", op->filename);
    } else {
	run_compiler(10,
		     " -inputvar=", ABCS_INTERVAL_KEYWORD_INPUT_VARIABLE,
		     " -statevar=", ABCS_INTERVAL_KEYWORD_STATE_VARIABLE,
		     " -statevar-ada-type=State_T",
		     " -inputvar-ada-type=Input_T",
		     " -outputdir=", folder, " -quiet -without-ode ", op->filename);
    }
    DONE;
    /* End: Generate source code related to the right hand side - and not to the ode. */

    /* Begin: Compute Jacobian */
    SECTION("First Derivative of Right Hand Side");
    op->method = TASK_JACOBIAN;
    op->max_order = 2;
    op->coeff_function_name = "Jacobian";

    write_files(in, op, tri2, S);

    file = my_fopen(folder, "Function_codomain", "dat", "r");
    read_codomain_magnitude(file, op->Bounds_of_Lipschitz_Matrices);
    fclose(file);
    my_remove_file(folder, "Function_codomain", "dat");
    DONE;
    /* End: Compute Jacobian */

    /* Begin: Read bounds of rounding errors of general solution */
    file = my_fopen(folder, "Bounds_of_Rounding_Errors", "dat", "r");
    read_list(file, op->Bounds_of_Rounding_Error_General_Solution);
    fclose(file);
    my_remove_file(folder, "Bounds_of_Rounding_Errors", "dat");
    /* End: Read bounds of rounding errors of general solution */


    /* Begin: Growth bounds */
    do_growth_bounds(c, h, adb, ads, op);
    /* End: Growth bounds */


    /* Begin: Compute bounds on rounding errors of the other involved FP operations */
    SECTION("Bounds on Rounding Errors of other FP Operations");
    calc_Bounds_of_Summation_Error_Growth_Bound(op);
    calc_Bounds_of_Summation_Error_General_Solution(op);
    calc_Bounds_of_Overapproximation_Radius(op);
    calc_Bounds_of_Overapproximation(in, op, S);	/* This ordering (previous 4 lines) is important!! */
    DONE;
    /* End: Compute bounds on rounding errors of the other involved FP operations */


    /* Begin: Print data (3) */
    for (i = 0; i < 2; i++) {
	output_Bounds_of_Lipschitz_Matrices(files[i], headers[i], formats[i], in, op, S);
	output_Bounds_of_Approximation_Errors(files[i], headers[i], formats[i], in, op, S);
	output_Bounds_of_Rounding_Errors(files[i], headers[i], formats[i], in, op, S);
	output_Bounds_of_Summation_Errors(files[i], headers[i], formats[i], in, op, S);
	output_Bounds_of_Overapproximation(files[i], headers[i], formats[i], in, op, S);
    }
    /* End: Print data (3) */

    /* Begin: Finalize main problem-specific source code files */
    fclose(c);
    fprintf(h, "\n#endif\n");
    fclose(h);
    fprintf(adb, "end %s;\n", name);
    fclose(adb);
    fprintf(ads, "end %s;\n", name);
    fclose(ads);
    /* End: Finalize main problem-specific source code files */
    fprintf(stdout, "======================================\n");
}
