with Established_Types; use Established_Types;
with Cell_Cover;        use Cell_Cover;
with Input_Values;      use Input_Values;



-- @summary
-- Generating growth bounds.
--
-- @description
-- This package provides a data type to store growth bounds and 
-- two interfaces. The first one is to compute growth bounds and
-- return the result values and the second one is to provide a
-- particular value according to the cell index and input index.
--
package Growth_Bound is
   
   
   type Growth_Bound_Values_T_Access is private;
   -- An access pointing to an object of type storing growth bounds  
   
  procedure Compute_Growth_Bound 
     ( P : in Problem_T; GB_forward : out Growth_Bound_Values_T_Access );
   -- Compute growth bounds after loading problem.
   -- @param P The problem to compute growth bounds from
   -- @exception 
   -- @return The access of the object storing growth bounds

   procedure Compute_Backward_Growth_Bound_with_DynamicUncertanties 
     ( P : in Problem_T; GB_backward : out Growth_Bound_Values_T_Access );  
   
   procedure Compute_Backward_Growth_Bound_wout_DynamicUncertanties 
     ( P : in Problem_T; GB_backward : out Growth_Bound_Values_T_Access );   
   
   
   procedure Get_Growth_Bound
     (GBV         : in out State_Radius_T; -- "GBV" stands for "growth bound value"
      GBVs_Acc    : in  Growth_Bound_Values_T_Access;
      Cell_Ind    : in  Cell_Index_T;
      Input_Ind   : in  Input_Index_T);
   -- Get a growth bound according to the cell and input.
   -- @param GBV The growth bound value to obtain
   -- @param GBVs_Acc The access to the object storing growth bounds
   -- @param Cell_Ind The index of the cell 
   -- @param Input_Ind The index of the input 
   

   
   
   
private
   
   -- "G_State" means the state radius: "exp(Lt)r"
   -- "G_Uncer" means measurement error and uncertainties : "Int(exp(Lt)w) + exp(Lt)z"
   type Grow_Comp_T is 
     ( G_State, G_Uncer );
   
   type Growth_Bound_Value_T is 
     array ( Grow_Comp_T ) of State_Radius_T_Access;
   
   type Growth_Bound_Value_T_Access is 
     access Growth_Bound_Value_T;
   
   -- "NB" (no bounds) stands for wothout the info of error bounds from P 
   type Growth_Bound_Values_NB_T is 
     array ( Input_Index_T range <> ) of Growth_Bound_Value_T_Access;
   
   type Growth_Bound_Values_NB_T_Access is 
     access Growth_Bound_Values_NB_T;
   
   type Growth_Bound_Values_T is
      record
	 Values : 
	   Growth_Bound_Values_NB_T_Access;
	 Bounds_Of_Rounding_Error_Of_Growth_Bound : 
	   Bounds_Of_Numerical_Errors_State_T_Access;
	 Bounds_Of_Approximation_Error_Of_Growth_Bound:
	   Bounds_Of_Numerical_Errors_State_T_Access;
	 Bounds_Of_Summation_Error_Growth_Bound:
	   Bounds_Of_Numerical_Errors_State_T_Access;
      end record;
   type Growth_Bound_Values_T_Access is access Growth_Bound_Values_T;
   
   
end Growth_Bound;
