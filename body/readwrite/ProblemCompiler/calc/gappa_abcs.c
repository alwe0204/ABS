/** @file 
 * @brief Computation of bounds of rounding error that occur during the controller synthesis
 * @date 01 Apr 2017
 * @author Alexander Weber (a.weber@unibw.de)
*/
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>		/* for unlink */
#include <string.h>

#include "my_strcat.h"
#include "to_string.h"
#include "ia.h"
#include "output.h"
#include "output_aux.h"

#include "gappa_utils.h"
#include "gappa.h"
#include "gappa_abcs.h"

/** @brief Implementation of the functions \f$\operatorname{IndexedFloats}_{x,y}\f$ and \f$\operatorname{IndexedReals}_{x,y}\f$.
 * @param file The file to put the strings
 * @param q The argument \f$q\f$ (for efficiency it is printed as a literal value)
*/
void fprintf_indexedfloatsreals_string(FILE * file, unsigned int q)
{
    fprintf(file, "indexedfloats %s = x + (p + 1/2)*((y - x)/(%u));\n", GAPPA_ROUNDING_KEYWORD, q);
    fprintf(file, "indexedreals = Mx + (p + 1/2)*((My - Mx)/(%u));\n", q);
}

static void Mx_My_p(FILE * file, double x_min, double x_max, double y_min, double y_max,
		    unsigned int p_max)
{
    fprintf(file, "{\n");
    char *buf2 = gappa_interval(x_min, x_max);
    fprintf(file, "Mx in %s /\\ ", buf2);
    free(buf2);
    buf2 = gappa_interval(y_min, y_max);
    fprintf(file, "My in %s /\\ ", buf2);
    free(buf2);
    fprintf(file, "@FIX(p,0) /\\ p in [0,%u] ", p_max);
}


double rounding_error_indexedreals(double x, double x_min, double x_max, double y,
				   double y_min, double y_max, unsigned int p_max, unsigned int q)
{
    const char *gappa_script_name = "tmp.g";
    const char *gappa_response_name = "res.dat";


    FILE *file = gappa_fopen(gappa_script_name);

    fprintf(file, "x = %a;\n", x);
    fprintf(file, "y = %a;\n", y);

    fprintf_indexedfloatsreals_string(file, q);
    Mx_My_p(file, x_min, x_max, y_min, y_max, p_max);

    fprintf(file, " -> |indexedfloats - indexedreals| in ?\n");
    fprintf(file, "}\n");
    fprintf(file, "$ p in %u;\n", p_max + 1);
    fclose(file);

    return gappa_get_upper_bound(gappa_script_name, gappa_response_name, GAPPA_EXECUTABLE);
}

static void Mx_Myy_p(FILE * file, double x_min, double x_max, double y_min, double y_max,
		     unsigned int p_max)
{
    fprintf(file, "{\n");
    char *buf2 = gappa_interval(x_min, x_max);
    fprintf(file, "Mx in %s /\\ ", buf2);
    free(buf2);
    buf2 = gappa_interval(y_min, y_max);
    fprintf(file, "Myy in %s /\\ ", buf2);
    free(buf2);
    fprintf(file, "@FIX(p,0) /\\ p in [0,%u] ", p_max);
}

double rounding_error_indexedreals_tree(double x, double x_min, double x_max, double y,
					double y_min, double y_max, unsigned int p_max,
					unsigned int q, unsigned int d)
{
    const char *gappa_script_name = "tmp.g";
    const char *gappa_response_name = "res.dat";


    FILE *file = gappa_fopen(gappa_script_name);

    fprintf(file, "x = %a;\n", x);
    fprintf(file, "y %s = x + %u *( %a - %a )/%u ;\n", GAPPA_ROUNDING_KEYWORD, q, y, x, d);

    fprintf(file, "My = Mx + %u *( Myy - Mx )/%u ;\n", q, d);

    fprintf_indexedfloatsreals_string(file, q);
    Mx_Myy_p(file, x_min, x_max, y_min, y_max, p_max);

    fprintf(file, " -> |indexedfloats - indexedreals| in ?\n");
    fprintf(file, "}\n");
    fprintf(file, "$ p in %u;\n", p_max + 1);
    fclose(file);

    return gappa_get_upper_bound(gappa_script_name, gappa_response_name, GAPPA_EXECUTABLE);
}


static void Mumin_Mumax_p(FILE * file, double x_min, double x_max, double y_min, double y_max,
			  unsigned int p_max)
{
    fprintf(file, "{\n");
    char *buf2 = gappa_interval(x_min, x_max);
    fprintf(file, "Mumin in %s /\\ ", buf2);
    free(buf2);
    buf2 = gappa_interval(y_min, y_max);
    fprintf(file, "Mumax in %s /\\ ", buf2);
    free(buf2);
    fprintf(file, "@FIX(p,0) /\\ p in [0,%u] ", p_max);
}

double rounding_error_indexedreals_input(double x, double x_min, double x_max,
					 double y, double y_min, double y_max, unsigned int d)
{
    FILE *file;
    unsigned int k = 0;
    const char *gappa_script_name = "tmp.g";
    const char *gappa_response_name = "res.dat";

    double retval = rounding_error_indexedreals(x, x_min, x_max, y, y_min, y_max, d - 1, d);

    int not_proved = 1;

    while (not_proved && k <= GAPPA_NUMBER_OF_ITERATIONS) {
	k++;

	file = gappa_fopen(gappa_script_name);
	fprintf(file, "umin = %a;\n", x);
	fprintf(file, "umax = %a;\n", y);
	fprintf(file, "x = umin - (umax - umin) / %u / 2 ;\n", d);
	fprintf(file, "y = umax + (umax - umin) / %u / 2 ;\n", d);

	fprintf(file, "Mx = Mumin - (Mumax - Mumin) / %u / 2 ;\n", d);
	fprintf(file, "My = Mumax + (Mumax - Mumin) / %u / 2 ;\n", d);

	fprintf_indexedfloatsreals_string(file, d + 1);

	fprintf(file, "roundingerror = %a;\n", k * retval);
	fprintf(file, "a %s = indexedfloats - roundingerror;\n", GAPPA_ROUNDING_KEYWORD);
	fprintf(file, "b %s = indexedfloats + roundingerror;\n", GAPPA_ROUNDING_KEYWORD);

	Mumin_Mumax_p(file, x_min, x_max, y_min, y_max, d);

	fprintf(file, " -> (a - indexedreals) <= 0 /\\ (b - indexedreals) >= 0  \n");
	fprintf(file, "}\n");
	fprintf(file, "$ p in %u;\n", d + 1);
	fclose(file);

	not_proved = run_gappa(gappa_script_name, gappa_response_name, GAPPA_EXECUTABLE);
    }
    if (k == GAPPA_NUMBER_OF_ITERATIONS)
	retval = HUGE_VAL;
    else
	retval *= k;
    unlink(gappa_script_name);
    unlink(gappa_response_name);
    return retval;
}

void prepare_gappa_input_assumptions(const char *filename, struct task *op,
				     ConstThreeVariableList tri, ConstSequenceOfStates S)
{
    const size_t len = get_three_variable_list_len(tri);
    char *v = generate_identifier("v", S);
    size_t i;
    char *comma = "";
    FILE *file = fopen(filename, "w+");
    for (i = 0; i < len; i++) {
	if (get_role_of_var(i, tri) == INPUT_VAR) {
	    if (!strcmp(get_value_of_var(i, tri)->name, op->timevar_name)) {
		continue;
	    }
	    if (!strcmp(get_value_of_var(i, tri)->name, op->inputvar_name)) {
		char *buf2 =
		    definition_of_variable_to_string(i, v, i, SYMBOLIC, GAPPA_MATH_FORMAT, tri, S);
		char *buf3 = gappa_interval(0,
					    op->Bounds_of_Input_Value_Rounding_Error[get_row_of_var
										     (i, tri)]);
		fprintf(file, "%s | %s - %s_o | in %s ", comma, buf2, buf2, buf3);
		comma = "/\\";
		free(buf2);
		free(buf3);
	    } else {
		char *buf2 =
		    definition_of_variable_to_string(i, v, i, SYMBOLIC, GAPPA_MATH_FORMAT, tri, S);
		char *buf3 = gappa_interval(0,
					    op->Bounds_of_Center_of_Cells_Rounding_Error
					    [get_row_of_var(i, tri)]);
		fprintf(file, "%s | %s - %s_o | in %s ", comma, buf2, buf2, buf3);
		comma = "/\\";
		free(buf2);
		free(buf3);
	    }
	}
    }
    fclose(file);
    free(v);
}

void prepare_gappa_file_for_abcs(const char *filename, struct task *op, ConstThreeVariableList tri,
				 ConstSequenceOfStates S)
{
    const size_t len = get_three_variable_list_len(tri);
    char *v = generate_identifier("v", S);
    size_t i;
    char *sed_str = "";
    char *buf, *buf2;
    for (i = 0; i < len; i++) {
	if (get_role_of_var(i, tri) == INPUT_VAR) {
	    if (!strcmp(get_value_of_var(i, tri)->name, op->timevar_name)) {
		continue;
	    }
	    buf = definition_of_variable_to_string(i, v, i, SYMBOLIC, GAPPA_FORMAT, tri, S);
	    buf2 = definition_of_variable_to_string(i, v, i, SYMBOLIC, GAPPA_MATH_FORMAT, tri, S);
	    my_strcat_several(&sed_str, 6, "sed -i -e 's#", buf, "#", buf2, "_o#g' ", filename);
	    assert(!system(sed_str));
	    free(sed_str);
	    free(buf);
	    free(buf2);
	    sed_str = "";
	    buf = "";
	    buf2 = "";
	}
    }
    free(v);
}

static void rnd_err_app_err_gb(FILE * file, double Bound_App_Err, double Bound_Rnd_Err,
			       double Bound_GB)
{
    char *buf = gappa_interval(0, Bound_Rnd_Err);
    fprintf(file, "rnd_err in %s /\\ ", buf);
    free(buf);
    buf = gappa_interval(0, Bound_App_Err);
    fprintf(file, "app_err in %s /\\ ", buf);
    free(buf);
    buf = gappa_interval(0, Bound_GB);
    fprintf(file, "gb in %s -> ", buf);
    free(buf);
}

/** @brief See Programmer's Manual
 * @param Bound_App_Error A bound on the approximation error of the general solution formula
 * @param Bound_Rnd_Error A bound on the rounding error of the general solution formula
 * @param Bound_GB A bound on the growth bound
*/
double Gappa_Bounds_of_Summation_Error_General_Solution(double Bound_App_Err, double Bound_Rnd_Err,
							double Bound_GB)
{
    unsigned int k = 0;
    double retval;
    const char *gappa_script_name = "tmp.g";
    const char *gappa_response_name = "res.dat";

    FILE *file = gappa_fopen(gappa_script_name);
    fprintf(file, "Mx = %s(app_err) + %s(rnd_err) + %s(gb);\n", GAPPA_ROUNDING_KEYWORD,
	    GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD);
    fprintf(file, "x %s = %s(app_err) + %s(rnd_err) + %s(gb);\n", GAPPA_ROUNDING_KEYWORD,
	    GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD);
    fprintf(file, "{\n");

    rnd_err_app_err_gb(file, Bound_App_Err, Bound_Rnd_Err, Bound_GB);

    fprintf(file, "| Mx - x | in ? \n");
    fprintf(file, "}\n");
    fclose(file);

    retval = gappa_get_upper_bound(gappa_script_name, gappa_response_name, GAPPA_EXECUTABLE);

    int not_proved = 1;

    while (not_proved && k <= GAPPA_NUMBER_OF_ITERATIONS) {
	k++;

	file = gappa_fopen(gappa_script_name);

     /** Formal arithmetic: \f$\gamma_1 + \gamma_2 + \gamma_3\f$ */
	fprintf(file, "Mx = %s(app_err) + %s(rnd_err) + %s(gb) + 0;\n", GAPPA_ROUNDING_KEYWORD,
		GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD);
	fprintf(file, "sigma = %a;\n", k * retval);
     /** FP arithmetic: \f$\gamma_1 \oplus_{\operatorname{RN}} \gamma_2 \oplus_{\operatorname{RN}} \gamma_3 \oplus_{\operatorname{RN}} \sigma_2 \f$ */
	fprintf(file, "x %s = %s(app_err) + %s(rnd_err) + %s(gb) + sigma;\n",
		GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD,
		GAPPA_ROUNDING_KEYWORD);
	fprintf(file, "{\n");

	rnd_err_app_err_gb(file, Bound_App_Err, Bound_Rnd_Err, Bound_GB);

	fprintf(file, "x - Mx >= 0 \n");

	fprintf(file, "}\n");
	fclose(file);
    }
    unlink(gappa_script_name);
    unlink(gappa_response_name);
    return retval;
}

static void gb_state_rnd_err_gb_uncert_app_err(FILE * file, double Bound_GB_State,
					       double Bound_GB_Uncert, double Bound_App_Err,
					       double Bound_Rnd_Err)
{
    char *buf = gappa_interval(0, Bound_GB_State);
    fprintf(file, "gb_state in %s /\\ ", buf);
    buf = gappa_interval(0, Bound_Rnd_Err);
    fprintf(file, "rnd_err in %s /\\ ", buf);
    buf = gappa_interval(0, Bound_GB_Uncert);
    fprintf(file, "gb_uncert in %s /\\ ", buf);
    buf = gappa_interval(0, Bound_App_Err);
    fprintf(file, "app_err in %s -> ", buf);
}

/** @brief See Programmer's Manual
 * @param Bound_GB_State A bound on \f$\hat B_\circ(\operatorname{RN}(t),\operatorname{RN}(r),M,0)\f$
 * @param Bound_GB_Uncert A bound on \f$\hat B_\circ(\operatorname{RN}(t),\tilde z,M,v)\f$
 * @param Bound_App_Err A bound on the approximation error in the growth bound formula
 * @param Bound_Rnd_Err A bound on the rounding error in the growth bound formula
*/
double Gappa_Bounds_of_Summation_Error_Growth_Bound(double Bound_GB_State, double Bound_GB_Uncert,
						    double Bound_App_Err, double Bound_Rnd_Err)
{
    unsigned int k = 0;
    double retval;
    const char *gappa_script_name = "tmp.g";
    const char *gappa_response_name = "res.dat";


    FILE *file = gappa_fopen(gappa_script_name);

    fprintf(file, "Mx = %s(gb_state) + %s(rnd_err) + %s(gb_uncert) + %s(rnd_err) + %s(app_err);\n",
	    GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD,
	    GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD);
    fprintf(file,
	    "x %s = %s(gb_state) + %s(rnd_err) + %s(gb_uncert) + %s(rnd_err) + %s(app_err);\n",
	    GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD,
	    GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD);
    fprintf(file, "{\n");

    gb_state_rnd_err_gb_uncert_app_err(file, Bound_GB_State, Bound_GB_Uncert, Bound_App_Err,
				       Bound_Rnd_Err);

    fprintf(file, "| Mx - x | in ? \n");
    fprintf(file, "}\n");
    fclose(file);

    retval = gappa_get_upper_bound(gappa_script_name, gappa_response_name, GAPPA_EXECUTABLE);

    int not_proved = 1;

    while (not_proved && k <= GAPPA_NUMBER_OF_ITERATIONS) {
	k++;

	file = gappa_fopen(gappa_script_name);
     /** Formal arithmetic: \f$\hat B_\circ(\operatorname{RN}(t),\operatorname{RN}(r),M,0) + \gamma_2 + \hat B_\circ(\operatorname{RN}(t),\tilde z,M,v) + \gamma_2' + \gamma_1\f$ */
	fprintf(file,
		"Mx = %s(gb_state) + %s(rnd_err) + %s(gb_uncert) + %s(rnd_err) + %s(app_err) + 0;\n",
		GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD,
		GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD);
	fprintf(file, "sigma = %a;\n", k * retval);
     /** FP arithmetic: \f$\hat B_\circ(\operatorname{RN}(t),\operatorname{RN}(r),M,0) \oplus_{\operatorname{RN}} \gamma_2 \oplus_{\operatorname{RN}} \hat B_\circ(\operatorname{RN}(t),\tilde z,M,v) \oplus_{\operatorname{RN}} \gamma_2' \oplus_{\operatorname{RN}} \gamma_1 \oplus_{\operatorname{RN}} \sigma_1 \f$ */
	fprintf(file,
		"x %s = %s(gb_state) + %s(rnd_err) + %s(gb_uncert) + %s(rnd_err) + %s(app_err) + sigma;\n",
		GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD,
		GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD, GAPPA_ROUNDING_KEYWORD);

	fprintf(file, "{\n");

	gb_state_rnd_err_gb_uncert_app_err(file, Bound_GB_State, Bound_GB_Uncert, Bound_App_Err,
					   Bound_Rnd_Err);

	fprintf(file, "x - Mx >= 0\n}\n");
	fclose(file);
	not_proved = run_gappa(gappa_script_name, gappa_response_name, GAPPA_EXECUTABLE);
    }
    if (k == GAPPA_NUMBER_OF_ITERATIONS)
	retval = HUGE_VAL;
    else
	retval *= k;
    unlink(gappa_script_name);
    unlink(gappa_response_name);
    return retval;
}

static void y__r__Mxmin_Mxmax(FILE * file, double ymin, double ymax, Expression Mxmin,
			      Expression Mxmax, double Bound_of_Overapproximation_Radius,
			      ConstSequenceOfStates S)
{
    char *buf2 = gappa_interval(ymin, ymax);
    fprintf(file, "y_ in %s /\\ ", buf2);
    free(buf2);
    buf2 = gappa_interval(0, Bound_of_Overapproximation_Radius);
    fprintf(file, "r_ in %s /\\ ", buf2);
    free(buf2);
    buf2 =
	gappa_interval(get_lowerbound_of_expression(Mxmin, S),
		       get_upperbound_of_expression(Mxmin, S));
    fprintf(file, "Mxmin in %s /\\ ", buf2);
    free(buf2);
    buf2 =
	gappa_interval(get_lowerbound_of_expression(Mxmax, S),
		       get_upperbound_of_expression(Mxmax, S));
    fprintf(file, "Mxmax in %s -> ", buf2);
    free(buf2);
}

static void xmin_xmax_measurement_error(FILE * file, double xmin, double xmax,
					double measurement_error)
{
    fprintf(file, "xmin = %a;\n", xmin);
    fprintf(file, "xmax = %a;\n", xmax);
    fprintf(file, "z = %a;\n", measurement_error);
}

/**
 * @brief See Programmer's Manual
 * @param ymin
 * @param ymax
 * @param q 
 * @param Mxmin \f$x_{\mathrm{min}}\f$
 * @param Mxmax \f$y\f$
 * @param xmin \f$x_{\mathrm{min},\circ}\f$
 * @param xmax \f$y_\circ\f$
 * @param Bound_of_Overapproximation_Radius
 * @param measurement_error \f$z_\circ\f$
 * @param S The sequence of states
*/

double Gappa_Bounds_of_Overapproximation_Rounding_Error(double ymin, double ymax, unsigned int q,
							Expression Mxmin, Expression Mxmax,
							double xmin, double xmax,
							double Bound_of_Overapproximation_Radius,
							double measurement_error,
							ConstSequenceOfStates S)
{
    unsigned int k = 0;
    double retval;
    const char *gappa_script_name = "tmp.g";
    const char *gappa_response_name = "res.dat";

    char buf[10];
    unsigned long Q = (1lu << q);
    sprintf(buf, "%lu", Q);

    FILE *file = gappa_fopen(gappa_script_name);

    xmin_xmax_measurement_error(file, xmin, xmax, measurement_error);

    fprintf(file, "eta double = (xmax - xmin) / (%s);\n", buf);
    fprintf(file, "Meta = (Mxmax - Mxmin) / (%s);\n", buf);
    fprintf(file, "r = double(r_);\n");
    fprintf(file, "y = double(y_);\n");
    fprintf(file, "Mx = (y - (r + z) - Mxmin) * ( 1 / Meta );\n");
    fprintf(file, "x double = (y - (r + z) - xmin) * ( 1 / eta );\n");
    fprintf(file, "{\n");

    y__r__Mxmin_Mxmax(file, ymin, ymax, Mxmin, Mxmax, Bound_of_Overapproximation_Radius, S);

    fprintf(file, "| Mx - x | in ? \n");
    fprintf(file, "}\n");
    fclose(file);

    retval = gappa_get_upper_bound(gappa_script_name, gappa_response_name, GAPPA_EXECUTABLE);

    file = gappa_fopen(gappa_script_name);

    xmin_xmax_measurement_error(file, xmin, xmax, measurement_error);

    fprintf(file, "eta double = (xmax - xmin) / (%s);\n", buf);
    fprintf(file, "Meta = (Mxmax - Mxmin) / (%s);\n", buf);
    fprintf(file, "r = double(r_);\n");
    fprintf(file, "y = double(y_);\n");
    fprintf(file, "Mx = (y + (r + z) - Mxmin) * ( 1 / Meta );\n");
    fprintf(file, "x double = (y + (r + z) - xmin) * ( 1 / eta );\n");
    fprintf(file, "{\n");

    y__r__Mxmin_Mxmax(file, ymin, ymax, Mxmin, Mxmax, Bound_of_Overapproximation_Radius, S);

    fprintf(file, "| Mx - x | in ? \n");
    fprintf(file, "}\n");
    fclose(file);

    retval =
	fmax(retval,
	     gappa_get_upper_bound(gappa_script_name, gappa_response_name, GAPPA_EXECUTABLE));

    int not_proved = 1;

    while (not_proved && k <= GAPPA_NUMBER_OF_ITERATIONS) {
	k++;
	file = fopen(gappa_script_name, "w+");
	assert(file != NULL);
	gappa_rounding_switch(file);
	fprintf(file, "sigma = %a;\n", k * retval);

	xmin_xmax_measurement_error(file, xmin, xmax, measurement_error);

	fprintf(file, "eta double = (xmax - xmin) / (%s);\n", buf);
	fprintf(file, "Meta = (Mxmax - Mxmin) / (%s);\n", buf);
	fprintf(file, "r = double(r_);\n");
	fprintf(file, "y = double(y_);\n");
    /** Formal arithmetic: \f$ (x + r + z_\circ - x_\mathrm{min}) / \eta \f$ */
	fprintf(file, "Mx = (y + (r + z) - Mxmin) * ( 1 / Meta ) + 0;\n");
    /** FP arithmetic: \f$ ((x \oplus_{\operatorname{RN}} (r \oplus_{\operatorname{RN}} z_\circ) \ominus_{\operatorname{RN}} x_{\mathrm{min},\circ}) \oslash_{\operatorname{RN}} \eta_\circ) \ominus_{\operatorname{RN}} \sigma_3 \f$ */
	fprintf(file, "x double = (y + (r + z) - xmin) * ( 1 / eta ) - sigma;\n");
    /** FP arithmetic: \f$ ((x \oplus_{\operatorname{RN}} (r \oplus_{\operatorname{RN}} z_\circ) \ominus_{\operatorname{RN}} x_{\mathrm{min},\circ}) \oslash_{\operatorname{RN}} \eta_\circ) \oplus_{\operatorname{RN}} \sigma_3 \f$ */
	fprintf(file, "x_up double = (y + (r + z) - xmin) * ( 1 / eta ) + sigma;\n");
    /** Formal arithmetic: \f$ (x - (r + z_\circ) - x_\mathrm{min}) / \eta \f$ */
	fprintf(file, "Mx2 = (y - (r + z) - Mxmin) * ( 1 / Meta ) + 0;\n");
    /** FP arithmetic: \f$ ((x \ominus_{\operatorname{RN}} (r \oplus_{\operatorname{RN}} z_\circ) \ominus_{\operatorname{RN}} x_{\mathrm{min},\circ}) \oslash_{\operatorname{RN}} \eta_\circ  ) \ominus_{\operatorname{RN}} \sigma_3 \f$ */
	fprintf(file, "x2 double = (y - (r + z) - xmin) * ( 1 / eta ) - sigma;\n");
    /** FP arithmetic: \f$ ((x \ominus_{\operatorname{RN}} (r \oplus_{\operatorname{RN}} z_\circ) \ominus_{\operatorname{RN}} x_{\mathrm{min},\circ}) \oslash_{\operatorname{RN}} \eta_\circ ) \oplus_{\operatorname{RN}} \sigma_3 \f$ */
	fprintf(file, "x2_up double = (y - (r + z) - xmin) * ( 1 / eta ) + sigma;\n");
	fprintf(file, "{\n");

	y__r__Mxmin_Mxmax(file, ymin, ymax, Mxmin, Mxmax, Bound_of_Overapproximation_Radius, S);

	fprintf(file,
		" ( Mx - x )  >= 0 /\\ ( x - Mx ) >= 0 /\\ ( Mx2 - x2 )  >= 0 /\\ ( x2 - Mx2 ) >= 0 \n");
	fprintf(file, "}\n");
	fclose(file);
    }
    if (k == GAPPA_NUMBER_OF_ITERATIONS)
	retval = HUGE_VAL;
    else
	retval *= k;
    unlink(gappa_script_name);
    unlink(gappa_response_name);
    return retval;
}
