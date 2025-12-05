with Ada.Unchecked_Deallocation;
package body dynamics_specification_parameters is
use GenElFun_64;
function Get_State_Space_Dimension return Component_Index_T is
begin
	return 2;
end Get_State_Space_Dimension;

function Get_Input_Space_Dimension return Component_Index_T is
begin
	return 1;
end Get_Input_Space_Dimension;

function Get_Num_Of_Coord_With_Periods return Component_Index_T is
begin
	return 1;
end Get_Num_Of_Coord_With_Periods;

function Get_Num_Of_Coord_Without_Periods return Component_Index_T is
begin
	return 1;
end Get_Num_Of_Coord_Without_Periods;

function Get_Coord_With_Periods return List_of_Component_Index_T is
retval : List_of_Component_Index_T (1 .. 1);
begin
retval(1) := 1;
return retval;
end Get_Coord_With_Periods;

function Get_Coord_Without_Periods return List_of_Component_Index_T is
retval : List_of_Component_Index_T (1 .. 1);
begin
retval(1) := 2;
return retval;
end Get_Coord_Without_Periods;

function Get_Bounds_Of_Dynamic_Uncertainties return Bounds_of_Dynamic_Uncertainties_T is
retval : Bounds_of_Dynamic_Uncertainties_T (1 .. 2);
begin
retval(1) := 0.0;
retval(2) := 0.0;
return retval;
end Get_Bounds_Of_Dynamic_Uncertainties;

function Get_Bounds_Of_Measurement_Errors return Bounds_of_Measurement_Errors_T is
retval : Bounds_of_Measurement_Errors_T (1 .. 2);
begin
retval(1) := 0.0;
retval(2) := 0.0;
return retval;
end Get_Bounds_Of_Measurement_Errors;

function Get_Sampling_Time return Time_T is
begin
	return Time_T((16#1.3333333333333#E+0)*(1.0/Float_T(2**2)));
end Get_Sampling_Time;

procedure free_union_of_hyperintervals(op : in out Compact_Hyperinterval_T_Array) is
procedure free is new Ada.Unchecked_Deallocation(Vector_Float_T,Vector_Float_T_Access);
begin
for i in op'Range loop
free(op(i)(Lower));
free(op(i)(Upper));
end loop;
end free_union_of_hyperintervals;

function Get_Num_Of_Initial_Set return NonNegative_Integer_T is
begin
	return 1;
end Get_Num_Of_Initial_Set;

function Get_Initial_Set return Compact_Hyperinterval_T_Array is
retval : Compact_Hyperinterval_T_Array (1 .. 1) := (others => (others => new Vector_Float_T (1 .. 2)) ) ;
begin
retval(1)(Lower)(1) := 0.0;
retval(1)(Upper)(1) := -0.0;
retval(1)(Lower)(2) := 0.0;
retval(1)(Upper)(2) := -0.0;
return retval;
end Get_Initial_Set;

function Get_Num_Of_Target_Set return NonNegative_Integer_T is
begin
	return 1;
end Get_Num_Of_Target_Set;

function Get_Target_Set return Compact_Hyperinterval_T_Array is
retval : Compact_Hyperinterval_T_Array (1 .. 1) := (others => (others => new Vector_Float_T (1 .. 2)) ) ;
begin
retval(1)(Lower)(1) := (16#1.8552e8777604d#E+0)*2.0;
retval(1)(Upper)(1) := (16#1.9eec82110f9e4#E+0)*2.0;
retval(1)(Lower)(2) := -(16#1.9999999999999#E+0)*(1.0/Float_T(2**4));
retval(1)(Upper)(2) := (16#1.9999999999999#E+0)*(1.0/Float_T(2**4));
return retval;
end Get_Target_Set;

function Get_Num_Of_Obstacle_Set return NonNegative_Integer_T is
begin
	return 0;
end Get_Num_Of_Obstacle_Set;

function Get_Obstacle_Set return Compact_Hyperinterval_T_Array is
retval : Compact_Hyperinterval_T_Array (1 .. 0) := (others => (others => new Vector_Float_T (1 .. 2)) ) ;
begin
return retval;
end Get_Obstacle_Set;

function Get_Num_Of_Operating_Range return NonNegative_Integer_T is
begin
	return 1;
end Get_Num_Of_Operating_Range;

function Get_Operating_Range return Compact_Hyperinterval_T_Array is
retval : Compact_Hyperinterval_T_Array (1 .. 1) := (others => (others => new Vector_Float_T (1 .. 2)) ) ;
begin
retval(1)(Lower)(1) := 0.0;
retval(1)(Upper)(1) := (16#1.921fb54442d19#E+0)*Float_T(2**2);
retval(1)(Lower)(2) := -2.0;
retval(1)(Upper)(2) := 2.0;
return retval;
end Get_Operating_Range;

function Get_Num_Of_Input_Space return NonNegative_Integer_T is
begin
	return 1;
end Get_Num_Of_Input_Space;

function Get_Input_Space return Compact_Hyperinterval_T_Array is
retval : Compact_Hyperinterval_T_Array (1 .. 1) := (others => (others => new Vector_Float_T (1 .. 1)) ) ;
begin
retval(1)(Lower)(1) := -2.0;
retval(1)(Upper)(1) := 2.0;
return retval;
end Get_Input_Space;

function Get_Initial_State_Space_Subdivision return State_Space_Subdivision_T is
retval : State_Space_Subdivision_T (1 .. 2);
begin
retval(1) := 100;
retval(2) := 100;
return retval;
end Get_Initial_State_Space_Subdivision;

function Get_Initial_Input_Space_Subdivision return Input_Space_Subdivision_T is
retval : Input_Space_Subdivision_T (1 .. 1);
begin
retval(1) := 2;
return retval;
end Get_Initial_Input_Space_Subdivision;

function Get_Rounded_Initial_State_Radius return State_Radius_T is
retval : State_Radius_T (1 .. 2);
begin
retval(1) := (16#1.015bf9217271b#E+0)*(1.0/Float_T(2**5));
retval(2) := (16#1.47ae147ae147b#E+0)*(1.0/Float_T(2**6));
return retval;
end Get_Rounded_Initial_State_Radius;

function Get_Bounds_of_Input_Value_Rounding_Error return Bounds_of_Numerical_Errors_Input_T is
retval : Bounds_of_Numerical_Errors_Input_T (1 .. 1);
begin
retval(1) := 0.0;
return retval;
end Get_Bounds_of_Input_Value_Rounding_Error;

procedure Integration(x_out : in out State_T; t : in Time_T; x0 : in State_T; u : in Input_T)
is
v0 : Float_T_Array ( 1 .. 30 );
begin
v0(1) := sin(x0(1));
v0(2) := (-(v0(1) * x0(2)));
v0(3) := cos(x0(1));
v0(4) := (2.0 * (((16#1.999999999999a#E+0)*(1.0/Float_T(2**7))+(16#1.9ce075f6fd22#E+0)*(1.0/Float_T(2**7)))/2.0));
v0(5) := (((-v0(1)) - (v0(3) * u(1))) - (v0(4) * x0(2)));
v0(6) := (((x0(2) * v0(2)) + (v0(5) * v0(3))) / 2.0);
v0(7) := (x0(2) * v0(3));
v0(8) := ((((-v0(7)) - (v0(2) * u(1))) - (v0(4) * v0(5))) / 2.0);
v0(9) := ((((-(v0(6) * x0(2))) - (v0(7) * v0(5))) - (v0(1) * v0(8))) / 3.0);
v0(10) := (((-(v0(7) * x0(2))) - (v0(1) * v0(5))) / 2.0);
v0(11) := ((((-v0(6)) - (v0(10) * u(1))) - (v0(4) * v0(8))) / 3.0);
v0(12) := (((((x0(2) * v0(9)) + (v0(5) * v0(10))) + (v0(8) * v0(2))) + (v0(11) * v0(3))) / 4.0);
v0(13) := ((((x0(2) * v0(10)) + (v0(5) * v0(2))) + (v0(8) * v0(3))) / 3.0);
v0(14) := ((((-v0(13)) - (v0(9) * u(1))) - (v0(4) * v0(11))) / 4.0);
v0(15) := ((((((-(v0(12) * x0(2))) - (v0(13) * v0(5))) - (v0(6) * v0(8))) - (v0(7) * v0(11))) - (v0(1) * v0(14))) / 5.0);
v0(16) := (((((-(v0(13) * x0(2))) - (v0(6) * v0(5))) - (v0(7) * v0(8))) - (v0(1) * v0(11))) / 4.0);
v0(17) := ((((-v0(12)) - (v0(16) * u(1))) - (v0(4) * v0(14))) / 5.0);
v0(18) := (((((((x0(2) * v0(15)) + (v0(5) * v0(16))) + (v0(8) * v0(9))) + (v0(11) * v0(10))) + (v0(14) * v0(2))) + (v0(17) * v0(3))) / 6.0);
v0(19) := ((((((x0(2) * v0(16)) + (v0(5) * v0(9))) + (v0(8) * v0(10))) + (v0(11) * v0(2))) + (v0(14) * v0(3))) / 5.0);
v0(20) := ((((-v0(19)) - (v0(15) * u(1))) - (v0(4) * v0(17))) / 6.0);
v0(21) := ((((((((-(v0(18) * x0(2))) - (v0(19) * v0(5))) - (v0(12) * v0(8))) - (v0(13) * v0(11))) - (v0(6) * v0(14))) - (v0(7) * v0(17))) - (v0(1) * v0(20))) / 7.0);
v0(22) := (((((((-(v0(19) * x0(2))) - (v0(12) * v0(5))) - (v0(13) * v0(8))) - (v0(6) * v0(11))) - (v0(7) * v0(14))) - (v0(1) * v0(17))) / 6.0);
v0(23) := ((((-v0(18)) - (v0(22) * u(1))) - (v0(4) * v0(20))) / 7.0);
v0(24) := (((((((((x0(2) * v0(21)) + (v0(5) * v0(22))) + (v0(8) * v0(15))) + (v0(11) * v0(16))) + (v0(14) * v0(9))) + (v0(17) * v0(10))) + (v0(20) * v0(2))) + (v0(23) * v0(3))) / 8.0);
v0(25) := ((((((((x0(2) * v0(22)) + (v0(5) * v0(15))) + (v0(8) * v0(16))) + (v0(11) * v0(9))) + (v0(14) * v0(10))) + (v0(17) * v0(2))) + (v0(20) * v0(3))) / 7.0);
v0(26) := (((((((((-(v0(25) * x0(2))) - (v0(18) * v0(5))) - (v0(19) * v0(8))) - (v0(12) * v0(11))) - (v0(13) * v0(14))) - (v0(6) * v0(17))) - (v0(7) * v0(20))) - (v0(1) * v0(23))) / 8.0);
v0(27) := ((((-v0(25)) - (v0(21) * u(1))) - (v0(4) * v0(23))) / 8.0);
v0(28) := ((((-v0(24)) - (v0(26) * u(1))) - (v0(4) * v0(27))) / 9.0);
v0(29) := ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * (v0(28) / 10.0)) + (v0(27) / 9.0))) + (v0(23) / 8.0))) + (v0(20) / 7.0))) + (v0(17) / 6.0))) + (v0(14) / 5.0))) + (v0(11) / 4.0))) + (v0(8) / 3.0))) + (v0(5) / 2.0))) + x0(2))) + x0(1));
x_out(1) := v0(29);
v0(30) := ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((((-((((((((((x0(2) * v0(26)) + (v0(5) * v0(21))) + (v0(8) * v0(22))) + (v0(11) * v0(15))) + (v0(14) * v0(16))) + (v0(17) * v0(9))) + (v0(20) * v0(10))) + (v0(23) * v0(2))) + (v0(27) * v0(3))) / 9.0)) - (((((((((((-(v0(24) * x0(2))) - (v0(25) * v0(5))) - (v0(18) * v0(8))) - (v0(19) * v0(11))) - (v0(12) * v0(14))) - (v0(13) * v0(17))) - (v0(6) * v0(20))) - (v0(7) * v0(23))) - (v0(1) * v0(27))) / 9.0) * u(1))) - (v0(4) * v0(28))) / 10.0)) + v0(28))) + v0(27))) + v0(23))) + v0(20))) + v0(17))) + v0(14))) + v0(11))) + v0(8))) + v0(5))) + x0(2));
x_out(2) := v0(30);

end Integration;

procedure Integration_for_growth_bound(x_out : in out State_Radius_T; t : in Time_T; r0 : in State_Radius_T; L : in Vector_Float_T; w : in Bounds_of_Dynamic_Uncertainties_T)
is
v0 : Float_T_Array ( 1 .. 30 );
begin
v0(1) := ((w(1) + (L(1) * r0(1))) + (L(2) * r0(2)));
v0(2) := ((w(2) + (L(3) * r0(1))) + (L(4) * r0(2)));
v0(3) := (((L(1) * v0(1)) + (L(2) * v0(2))) / 2.0);
v0(4) := (((L(3) * v0(1)) + (L(4) * v0(2))) / 2.0);
v0(5) := (((L(1) * v0(3)) + (L(2) * v0(4))) / 3.0);
v0(6) := (((L(3) * v0(3)) + (L(4) * v0(4))) / 3.0);
v0(7) := (((L(1) * v0(5)) + (L(2) * v0(6))) / 4.0);
v0(8) := (((L(3) * v0(5)) + (L(4) * v0(6))) / 4.0);
v0(9) := (((L(1) * v0(7)) + (L(2) * v0(8))) / 5.0);
v0(10) := (((L(3) * v0(7)) + (L(4) * v0(8))) / 5.0);
v0(11) := (((L(1) * v0(9)) + (L(2) * v0(10))) / 6.0);
v0(12) := (((L(3) * v0(9)) + (L(4) * v0(10))) / 6.0);
v0(13) := (((L(1) * v0(11)) + (L(2) * v0(12))) / 7.0);
v0(14) := (((L(3) * v0(11)) + (L(4) * v0(12))) / 7.0);
v0(15) := (((L(1) * v0(13)) + (L(2) * v0(14))) / 8.0);
v0(16) := (((L(3) * v0(13)) + (L(4) * v0(14))) / 8.0);
v0(17) := (((L(1) * v0(15)) + (L(2) * v0(16))) / 9.0);
v0(18) := (((L(3) * v0(15)) + (L(4) * v0(16))) / 9.0);
v0(19) := (((L(1) * v0(17)) + (L(2) * v0(18))) / 10.0);
v0(20) := (((L(3) * v0(17)) + (L(4) * v0(18))) / 10.0);
v0(21) := (((L(1) * v0(19)) + (L(2) * v0(20))) / 11.0);
v0(22) := (((L(3) * v0(19)) + (L(4) * v0(20))) / 11.0);
v0(23) := (((L(1) * v0(21)) + (L(2) * v0(22))) / 12.0);
v0(24) := (((L(3) * v0(21)) + (L(4) * v0(22))) / 12.0);
v0(25) := (((L(1) * v0(23)) + (L(2) * v0(24))) / 13.0);
v0(26) := (((L(3) * v0(23)) + (L(4) * v0(24))) / 13.0);
v0(27) := (((L(1) * v0(25)) + (L(2) * v0(26))) / 14.0);
v0(28) := (((L(3) * v0(25)) + (L(4) * v0(26))) / 14.0);
v0(29) := ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * (((L(1) * v0(27)) + (L(2) * v0(28))) / 15.0)) + v0(27))) + v0(25))) + v0(23))) + v0(21))) + v0(19))) + v0(17))) + v0(15))) + v0(13))) + v0(11))) + v0(9))) + v0(7))) + v0(5))) + v0(3))) + v0(1))) + r0(1));
x_out(1) := v0(29);
v0(30) := ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * ((t * (((L(3) * v0(27)) + (L(4) * v0(28))) / 15.0)) + v0(28))) + v0(26))) + v0(24))) + v0(22))) + v0(20))) + v0(18))) + v0(16))) + v0(14))) + v0(12))) + v0(10))) + v0(8))) + v0(6))) + v0(4))) + v0(2))) + r0(2));
x_out(2) := v0(30);

end Integration_for_growth_bound;

function Get_Bounds_of_Lipschitz_Matrices return Vector_Float_T is
retval : Vector_Float_T (1 .. 4);
begin
retval(1) := 0.0;
retval(2) := 1.0;
retval(3) := 3.0;
retval(4) := (16#1.9ce075f6fd22#E+0)*(1.0/Float_T(2**6));
return retval;
end Get_Bounds_of_Lipschitz_Matrices;

function Get_Bounds_of_Approximation_Error_of_General_Solution return Bounds_of_Numerical_Errors_State_T is
retval : Bounds_of_Numerical_Errors_State_T (1 .. 2);
begin
retval(1) := (16#1.7e1e9f37b1a28#E+0)*(1.0/Float_T(2**4));
retval(2) := (16#1.575840a1e9ad6#E+0);
return retval;
end Get_Bounds_of_Approximation_Error_of_General_Solution;

function Get_Bounds_of_Approximation_Error_of_Growth_Bound return Bounds_of_Numerical_Errors_State_T is
retval : Bounds_of_Numerical_Errors_State_T (1 .. 2);
begin
retval(1) := (16#1.a413146378d46#E+0)*(1.0/Float_T(2**65));
retval(2) := (16#1.51da77db22183#E+0)*(1.0/Float_T(2**65));
return retval;
end Get_Bounds_of_Approximation_Error_of_Growth_Bound;

function Get_Bounds_of_Rounding_Error_of_General_Solution return Bounds_of_Numerical_Errors_State_T is
retval : Bounds_of_Numerical_Errors_State_T (1 .. 2);
begin
retval(1) := (16#1.0#E+0)*(1.0/Float_T(2**55));
retval(2) := 0.0;
return retval;
end Get_Bounds_of_Rounding_Error_of_General_Solution;

function Get_Bounds_of_Rounding_Error_of_Growth_Bound return Bounds_of_Numerical_Errors_State_T is
retval : Bounds_of_Numerical_Errors_State_T (1 .. 2);
begin
retval(1) := (16#1.0#E+0)*(1.0/Float_T(2**55));
retval(2) := 0.0;
return retval;
end Get_Bounds_of_Rounding_Error_of_Growth_Bound;

function Get_Bounds_of_Summation_Error_General_Solution return Bounds_of_Numerical_Errors_State_T is
retval : Bounds_of_Numerical_Errors_State_T (1 .. 2);
begin
retval(1) := (16#1.8#E+0)*(1.0/Float_T(2**56));
retval(2) := (16#1.0#E+0)*(1.0/Float_T(2**52));
return retval;
end Get_Bounds_of_Summation_Error_General_Solution;

function Get_Bounds_of_Summation_Error_Growth_Bound return Bounds_of_Numerical_Errors_State_T is
retval : Bounds_of_Numerical_Errors_State_T (1 .. 2);
begin
retval(1) := (16#1.c#E+0)*(1.0/Float_T(2**55));
retval(2) := (16#1.c#E+0)*(1.0/Float_T(2**55));
return retval;
end Get_Bounds_of_Summation_Error_Growth_Bound;

function Get_Bounds_of_Overapproximation_Rounding_Error return Bounds_of_Numerical_Errors_State_T is
retval : Bounds_of_Numerical_Errors_State_T (1 .. 2);
begin
retval(1) := (16#1.8464ad3f397dd#E+0)*(1.0/Float_T(2**40));
retval(2) := (16#1.3228#E+0)*(1.0/Float_T(2**41));
return retval;
end Get_Bounds_of_Overapproximation_Rounding_Error;

function Get_Bounds_of_Overapproximation_Radius return Bounds_of_Numerical_Errors_State_T is
retval : Bounds_of_Numerical_Errors_State_T (1 .. 2);
begin
retval(1) := (16#1.5892012e1e7c#E+0)*(1.0/Float_T(2**4));
retval(2) := (16#1.aeec7ad93c168#E+0)*(1.0/Float_T(2**4));
return retval;
end Get_Bounds_of_Overapproximation_Radius;

end dynamics_specification_parameters;
