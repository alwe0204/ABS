with Interfaces.C; use Interfaces.C;
with Established_Types; use Established_Types;
with Ada.Numerics.Generic_Elementary_Functions;
package dynamics_specification_parameters is
package GenElFun_64 is new Ada.Numerics.Generic_Elementary_Functions(Float_T);
function Get_State_Space_Dimension return Component_Index_T;
-- @return The dimension of the state space.
function Get_Input_Space_Dimension return Component_Index_T;
-- @return The dimension of the input space.
function Get_Num_Of_Coord_With_Periods return Component_Index_T;
-- @return The number of coordinates in the state space that possess a period.
function Get_Num_Of_Coord_Without_Periods return Component_Index_T;
-- @return The number of coordinates in the state space that do not possess a period.
function Get_Coord_With_Periods return List_of_Component_Index_T;
-- @return An array with the coordinates that possess a period.
-- In the case that there is no such coordinate the empty array is returned.
function Get_Coord_Without_Periods return List_of_Component_Index_T;
-- @return An array with the coordinates that do not possess a period.
-- In the case that there is no such coordinate the empty array is returned.
function Get_Bounds_Of_Dynamic_Uncertainties return Bounds_of_Dynamic_Uncertainties_T;
-- @return The vector specifying the uncertainties in the dynamics.
function Get_Bounds_Of_Measurement_Errors return Bounds_of_Measurement_Errors_T;
-- @return The vector specifying the bound on the measurement errors.
function Get_Sampling_Time return Time_T;
-- @return The sampling time
procedure free_union_of_hyperintervals(op : in out Compact_Hyperinterval_T_Array);
-- @description Deallocates a union_of_hyperintervals.
function Get_Num_Of_Initial_Set return NonNegative_Integer_T;
-- @return The length of the list 'Initial_Set'.
function Get_Initial_Set return Compact_Hyperinterval_T_Array;
-- @return The list 'Initial_Set'.
function Get_Num_Of_Target_Set return NonNegative_Integer_T;
-- @return The length of the list 'Target_Set'.
function Get_Target_Set return Compact_Hyperinterval_T_Array;
-- @return The list 'Target_Set'.
function Get_Num_Of_Obstacle_Set return NonNegative_Integer_T;
-- @return The length of the list 'Obstacle_Set'.
function Get_Obstacle_Set return Compact_Hyperinterval_T_Array;
-- @return The list 'Obstacle_Set'.
function Get_Num_Of_Operating_Range return NonNegative_Integer_T;
-- @return The length of the list 'Operating_Range'.
function Get_Operating_Range return Compact_Hyperinterval_T_Array;
-- @return The list 'Operating_Range'.
function Get_Num_Of_Input_Space return NonNegative_Integer_T;
-- @return The length of the list 'Input_Space'.
function Get_Input_Space return Compact_Hyperinterval_T_Array;
-- @return The list 'Input_Space'.
function Get_Initial_State_Space_Subdivision return State_Space_Subdivision_T;
-- @return The number of subdivisions in each egde of the operating range.
function Get_Initial_Input_Space_Subdivision return Input_Space_Subdivision_T;
-- @return The number of subdivisions in each egde of the continuous input set.
function Get_Rounded_Initial_State_Radius return State_Radius_T;
-- @return Returns a value >= (xmax - xmix) / Initial_State_Space_Division / 2 (component-wise)
                -- @description See Programmer's Manual for presice definitions.
function Get_Bounds_of_Input_Value_Rounding_Error return Bounds_of_Numerical_Errors_Input_T;
-- @return See Programmer's Manual.
procedure Integration(x_out : in out State_T; t : in Time_T; x0 : in State_T; u : in Input_T);
procedure Integration_for_growth_bound(x_out : in out State_Radius_T; t : in Time_T; r0 : in State_Radius_T; L : in Vector_Float_T; w : in Bounds_of_Dynamic_Uncertainties_T);
function Get_Bounds_of_Lipschitz_Matrices return Vector_Float_T;
-- @return See Programmer's Manual.
function Get_Bounds_of_Approximation_Error_of_General_Solution return Bounds_of_Numerical_Errors_State_T;
-- @return See Programmer's Manual.
function Get_Bounds_of_Approximation_Error_of_Growth_Bound return Bounds_of_Numerical_Errors_State_T;
-- @return See Programmer's Manual.
function Get_Bounds_of_Rounding_Error_of_General_Solution return Bounds_of_Numerical_Errors_State_T;
-- @return See Programmer's Manual.
function Get_Bounds_of_Rounding_Error_of_Growth_Bound return Bounds_of_Numerical_Errors_State_T;
-- @return See Programmer's Manual.
function Get_Bounds_of_Summation_Error_General_Solution return Bounds_of_Numerical_Errors_State_T;
-- @return See Programmer's Manual.
function Get_Bounds_of_Summation_Error_Growth_Bound return Bounds_of_Numerical_Errors_State_T;
-- @return See Programmer's Manual.
function Get_Bounds_of_Overapproximation_Rounding_Error return Bounds_of_Numerical_Errors_State_T;
-- @return See Programmer's Manual.
function Get_Bounds_of_Overapproximation_Radius return Bounds_of_Numerical_Errors_State_T;
-- @return See Programmer's Manual.
end dynamics_specification_parameters;
