with Interfaces.C;  use Interfaces.C;


package body L_Matrix is
   
   svn_author  : constant String := "$Author: lf3eelma $";
   svn_revision: constant String := "$Revision: 2085 $";
   svn_date    : constant String := "$Date: 2020-07-01 13:42:13 +0200 (Mi, 01 Jul 2020) $";   
   
   procedure Compute_L (L            : in out Vector_Float_T;
			State_Min    : in Vector_Float_T_Access;
			State_Max    : in Vector_Float_T_Access;
			Input        : in Input_T;
			Input_RoEr   : in Bounds_Of_Numerical_Errors_Input_T_Access) is
      
      -- types
      type C_State_T is array (State_Min'First .. State_Min'Last) of Double;
      type C_Input_T is array (Input'First .. Input'Last) of Double;
      type C_Matrix_T is array (L'First .. L'Last) of Double;
      -- variables
      Input_Dimension : Component_Index_T := Input'Length;
      State_Dimension : Component_Index_T := State_Min'Length;
      C_L            : C_Matrix_T;
      C_State_Min    : C_State_T;
      C_State_Max    : C_State_T;
      C_Input        : C_Input_T;
      C_Input_RoEr   : C_Input_T;
      
      -- function that interfaces to c
      procedure L_Matrix ( L            : in out C_Matrix_T;
			   State_Min    : in C_State_T;
			   State_Max    : in C_State_T;
			   Input        : in C_Input_T;
			   Input_RoEr   : in C_Input_T;
			   Input_Dim    : in Component_Index_T;
			   State_Dim    : in Component_Index_T);
      
      pragma Import (C, L_Matrix, "l_matrix");
      
      
   begin

      -- Firstly, copy data to the variables which will be sent to c-language
      for I in C_State_Min'Range loop
	 C_State_Min(I) := State_Min(I);
	 C_State_Max(I) := State_Max(I);
      end loop;
      
      for I in C_Input'Range loop
	 C_Input(I) := Input(I);
	 C_Input_RoEr(I) := Input_RoEr(I);
      end loop;
      -- Secondly, call the interface function
      L_Matrix ( C_L, C_State_Min, C_State_Max, C_Input, C_Input_RoEr,
		 Input_Dimension, State_Dimension);
      -- Thirdly, write the result back to the variable "L"
      for I in C_L'Range loop
	 L(I) := C_L(I);
      end loop;

   end Compute_L;
   
    procedure Compute_L_b (L            : in out Vector_Float_T;
			State_Min    : in Vector_Float_T_Access;
			State_Max    : in Vector_Float_T_Access;
			Input        : in Input_T;
			Input_RoEr   : in Bounds_Of_Numerical_Errors_Input_T_Access) is
      
      -- types
      type C_State_T is array (State_Min'First .. State_Min'Last) of Double;
      type C_Input_T is array (Input'First .. Input'Last) of Double;
      type C_Matrix_T is array (L'First .. L'Last) of Double;
      -- variables
      Input_Dimension : Component_Index_T := Input'Length;
      State_Dimension : Component_Index_T := State_Min'Length;
      C_L            : C_Matrix_T;
      C_State_Min    : C_State_T;
      C_State_Max    : C_State_T;
      C_Input        : C_Input_T;
      C_Input_RoEr   : C_Input_T;
      
      -- function that interfaces to c
      procedure L_Matrix ( L            : in out C_Matrix_T;
			   State_Min    : in C_State_T;
			   State_Max    : in C_State_T;
			   Input        : in C_Input_T;
			   Input_RoEr   : in C_Input_T;
			   Input_Dim    : in Component_Index_T;
			   State_Dim    : in Component_Index_T);
      
      pragma Import (C, L_Matrix, "l_b_matrix");
      
      
   begin

      -- Firstly, copy data to the variables which will be sent to c-language
      for I in C_State_Min'Range loop
	 C_State_Min(I) := State_Min(I);
	 C_State_Max(I) := State_Max(I);
      end loop;
      
      for I in C_Input'Range loop
	 C_Input(I) := Input(I);
	 C_Input_RoEr(I) := Input_RoEr(I);
      end loop;
      -- Secondly, call the interface function
      L_Matrix ( C_L, C_State_Min, C_State_Max, C_Input, C_Input_RoEr,
		 Input_Dimension, State_Dimension);
      -- Thirdly, write the result back to the variable "L"
      for I in C_L'Range loop
	 L(I) := C_L(I);
      end loop;

   end Compute_L_b;
   
   
   
   
end L_Matrix;
