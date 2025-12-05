-- a package is for an internal use in package "Growth_Bound" 
-- an interface to C-code in order to implement parametrized matrix "L"

with Established_Types; use Established_Types;

package L_Matrix is
   
   procedure Compute_L ( L            : in out  Vector_Float_T;
			State_Min    : in Vector_Float_T_Access;
			State_Max    : in Vector_Float_T_Access;
			Input        : in Input_T;
                        Input_RoEr   : in Bounds_Of_Numerical_Errors_Input_T_Access);
   
      procedure Compute_L_b ( L            : in out  Vector_Float_T;
			State_Min    : in Vector_Float_T_Access;
			State_Max    : in Vector_Float_T_Access;
			Input        : in Input_T;
			Input_RoEr   : in Bounds_Of_Numerical_Errors_Input_T_Access);
			   
end L_Matrix;
