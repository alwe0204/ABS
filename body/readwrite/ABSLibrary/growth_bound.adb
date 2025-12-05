
with L_Matrix; 
with Interfaces.C; use Interfaces.C;
with Ada.Text_IO; use Ada.Text_IO;

-- "GBV" stands for growth bound values
package body Growth_Bound is
   
   svn_author  : constant String := "$Author: lf3eelma $";
   svn_revision: constant String := "$Revision: 2395 $";
   svn_date    : constant String := "$Date: 2021-10-25 23:29:03 +0200 (Mo, 25 Okt 2021) $";   
   
   ------------------------------------------------------------
   -- local subprogram 
   ------------------------------------------------------------
   function Input_Quantity
     ( Input_Subd_Acc : in Input_Space_Subdivision_T_Access )
     return Input_Index_T
   is
      
      -- variables
      Input_Values_Quantity: Input_Index_T;
      Input_Space_Subd: Positive_Integer_T := 1;
      
   begin
      
      -- compute the quantity of all possible input values
     for I in Input_Subd_Acc'Range loop
         Input_Space_Subd := Input_Space_Subd * (Input_Subd_Acc.all(I)+1);
       
      end loop;
      
      -- type convertion      
        Input_Space_Subd:=Input_Space_Subd-1;
    --   Put_Line(Input_Space_Subd'img);
      Input_Values_Quantity := Input_Index_T ( Input_Space_Subd );
     -- Input_Values_Quantity := Input_Index_T ( Input_Space_Subd -1 );
      
      return Input_Values_Quantity;
      
   end Input_Quantity;
   
   ------------------------------------------------------------
   -- local subprogram
   ------------------------------------------------------------
   procedure Copy_Error
     ( Y : out Bounds_Of_Numerical_Errors_State_T;
       X : in  Bounds_Of_Numerical_Errors_State_T )
   is
   begin
      
      for I in Y'Range loop
	 Y(I) := X (I);
      end loop ;
      
   end Copy_Error;
   ------------------------------------------------------------
   -- Implementation of program in spec
   ------------------------------------------------------------
   procedure Compute_Growth_Bound 
     ( P : in Problem_T; GB_forward : out Growth_Bound_Values_T_Access) 
   is
      
      GBVs_Acc : Growth_Bound_Values_T_Access :=
	new Growth_Bound_Values_T ;
      

      
      Input_Grid : Input_Values.Input_Grid_T;      

      L : Vector_Float_T( P.Xmin'First .. P.Xmin'Last ** 2 );
   --   L_max : Vector_Float_T( P.Xmin'First .. P.Xmin'Last ** 2 );
      c_L_max:Integer:=0;

      Input : Input_T( P.Umin'Range );

      Uncer_Zero : Bounds_of_Dynamic_Uncertainties_T( P.Xmin'Range ) := ( others => 0.0 );
      
      GB_Int: Growth_Bound_Integration_T := P.Growth_Bound_Formula;
     Assumption_Error :exception;
   begin
      
      ---------------
      -- preparation
      ---------------
      
      -- set range of of "Values" of the growth bound
      GBVs_Acc.all.Values := new Growth_Bound_Values_NB_T
	( 0 .. Input_Quantity (P.Initial_Input_Space_Subdivision));
      for I in GBVs_Acc.all.Values.all'Range loop
	 GBVs_Acc.all.Values.all(I) := new Growth_Bound_Value_T;
	 GBVs_Acc.all.Values.all(I).all(G_State) := new State_Radius_T(P.Xmin'Range);
	 GBVs_Acc.all.Values.all(I).all(G_Uncer) := new State_Radius_T(P.Xmin'Range);
      end loop;
      
    
      
      
      
      
      
      -- set range of "Bounds" of the growth bound
      GBVs_Acc.all.Bounds_Of_Rounding_Error_Of_Growth_Bound 
	:= new Bounds_Of_Numerical_Errors_State_T(P.Xmin'Range);
      GBVs_Acc.all.Bounds_Of_Approximation_Error_Of_Growth_Bound
	:= new Bounds_Of_Numerical_Errors_State_T(P.Xmin'Range);
      GBVs_Acc.all.Bounds_Of_Summation_Error_Growth_Bound
	:= new Bounds_Of_Numerical_Errors_State_T(P.Xmin'Range);
	
      
       
      
      
      -- get input value via input index using package "Input_Values"       
      Input_Grid := Input_Values.Install ( P );
      
      ---------------
      -- main part
      ---------------
      
      -- loop to compute growth bound
      for I in GBVs_Acc.all.Values.all'Range loop
	 -- get input value
	 Input_Values.Get_Input_Value ( Input,
					I,
					Input_Grid );

	 -- compute parametrized matrix "L"
	 L_Matrix.Compute_L ( L,
			      P.Apriori_Enclosure ( Lower ),
			      P.Apriori_Enclosure ( Upper ),
			      Input,
			      P.Bounds_Of_Input_Value_Rounding_Error );
	 

         
         
         
	 -- compute the first component of growth bound
	 -- for state radius : "exp(Lt)r"
	 GB_Int ( GBVs_Acc.all.Values.all(I).all(G_State).all,
		  P.Sampling_Time,
		  P.Rounded_Initial_State_Radius.all,
		  L,
		  Uncer_Zero );

         
         
        
         
         
         
         
	 -- compute the second component of growth bound 
	 -- for measurement error and uncertainties : "Int(exp(Lt)w) + exp(Lt)z"
	 GB_Int ( GBVs_Acc.all.Values.all(I).all(G_Uncer).all,
		  P.Sampling_Time,
		  P.Bounds_Of_Measurement_Errors.all,
		  L,
           P.Bounds_Of_Dynamic_Uncertainties.all );
         
         
        
         
         
      end loop;
      
      -- add bounds info to "GBVs_Acc"
      for I in P.Xmin'Range loop
	 Copy_Error ( GBVs_Acc.all.Bounds_Of_Rounding_Error_Of_Growth_Bound.all,
		      P.Bounds_Of_Rounding_Error_Of_Growth_Bound.all );
	 Copy_Error ( GBVs_Acc.all.Bounds_Of_Approximation_Error_Of_Growth_Bound.all,
		      P.Bounds_Of_Approximation_Error_Of_Growth_Bound.all );
	 Copy_Error ( GBVs_Acc.all.Bounds_Of_Summation_Error_Growth_Bound.all,
               P.Bounds_Of_Summation_Error_Growth_Bound.all );
         
         
        
         
         
         
         
      end loop;
      
      GB_forward:= GBVs_Acc;
      
   
    --  raise Assumption_Error;
   end Compute_Growth_Bound;
   
   
   
   
   
   
   
      procedure Compute_Backward_Growth_Bound_with_DynamicUncertanties 
     ( P : in Problem_T; GB_backward : out Growth_Bound_Values_T_Access ) 
         is
      
      GBVs_Acc : Growth_Bound_Values_T_Access :=
	new Growth_Bound_Values_T ;
            
      Input_Grid : Input_Values.Input_Grid_T;      

      L_b : Vector_Float_T( P.Xmin'First .. P.Xmin'Last ** 2 );

      Input : Input_T( P.Umin'Range );
      
      GB_Int: Growth_Bound_Integration_T := P.Growth_Bound_Formula;
      Uncer_Zero : Bounds_of_Dynamic_Uncertainties_T( P.Xmin'Range ) := ( others => 0.0 );
   begin
      
      ---------------
      -- preparation
      ---------------
      
      -- set range of of "Values" of the growth bound
      GBVs_Acc.all.Values := new Growth_Bound_Values_NB_T
	( 0 .. Input_Quantity (P.Initial_Input_Space_Subdivision));
      for I in GBVs_Acc.all.Values.all'Range loop
	 GBVs_Acc.all.Values.all(I) := new Growth_Bound_Value_T;
	 GBVs_Acc.all.Values.all(I).all(G_State) := new State_Radius_T(P.Xmin'Range);
	 GBVs_Acc.all.Values.all(I).all(G_Uncer) := new State_Radius_T(P.Xmin'Range);
      end loop;
      
    
      

      
      -- set range of "Bounds" of the growth bound
      GBVs_Acc.all.Bounds_Of_Rounding_Error_Of_Growth_Bound 
	:= new Bounds_Of_Numerical_Errors_State_T(P.Xmin'Range);
      GBVs_Acc.all.Bounds_Of_Approximation_Error_Of_Growth_Bound
	:= new Bounds_Of_Numerical_Errors_State_T(P.Xmin'Range);
      GBVs_Acc.all.Bounds_Of_Summation_Error_Growth_Bound
	:= new Bounds_Of_Numerical_Errors_State_T(P.Xmin'Range);
	

      
      -- get input value via input index using package "Input_Values"       
      Input_Grid := Input_Values.Install ( P );
      
      ---------------
      -- main part
      ---------------
      
      -- loop to compute growth bound
      for I in GBVs_Acc.all.Values.all'Range loop
	 -- get input value
	 Input_Values.Get_Input_Value ( Input,
					I,
					Input_Grid );

	 -- compute parametrized matrix "L_b" for dynamics backward in time, taking into account all errors
	
	     
            L_Matrix.Compute_L_b ( L_b,                        
                                   P.Apriori_Enclosure_Backward_with_DynamicUncertanties ( Lower ),
                                   P.Apriori_Enclosure_Backward_with_DynamicUncertanties ( Upper ),
                                   Input,
                                   P.Bounds_Of_Input_Value_Rounding_Error );
         
         
         
         
            -- compute the first component of growth bound
            -- for state radius : "exp(Lt)r"
            GB_Int ( GBVs_Acc.all.Values.all(I).all(G_State).all,
                     P.Sampling_Time,
                     P.Rounded_Initial_State_Radius.all,
                     L_b,
                     Uncer_Zero );

        
                       
         
            -- compute the second component of growth bound 
            -- for measurement error and uncertainties : "Int(exp(Lt)w) + exp(Lt)z"
            GB_Int ( GBVs_Acc.all.Values.all(I).all(G_Uncer).all,
                     P.Sampling_Time,
                     P.Bounds_Of_Measurement_Errors.all,
                     L_b,
                     P.Bounds_Of_Dynamic_Uncertainties.all );
         
      
      end loop;
      
      -- add bounds info to "GBVs_Acc"
      for I in P.Xmin'Range loop
	 Copy_Error ( GBVs_Acc.all.Bounds_Of_Rounding_Error_Of_Growth_Bound.all,
		      P.Bounds_Of_Rounding_Error_Of_Growth_Bound.all );
	 Copy_Error ( GBVs_Acc.all.Bounds_Of_Approximation_Error_Of_Growth_Bound.all,
		      P.Bounds_Of_Approximation_Error_Of_Growth_Bound.all );
	 Copy_Error ( GBVs_Acc.all.Bounds_Of_Summation_Error_Growth_Bound.all,
               P.Bounds_Of_Summation_Error_Growth_Bound.all );
         
         
         
      end loop;
      

      GB_backward:=GBVs_Acc;
      
      
      
      
      
      end Compute_Backward_Growth_Bound_with_DynamicUncertanties;
   
   procedure Compute_Backward_Growth_Bound_wout_DynamicUncertanties 
     ( P : in Problem_T; GB_backward : out Growth_Bound_Values_T_Access )
         is
      
      GBVs_Acc : Growth_Bound_Values_T_Access :=
	new Growth_Bound_Values_T ;
            
      Input_Grid : Input_Values.Input_Grid_T;      

      L_b : Vector_Float_T( P.Xmin'First .. P.Xmin'Last ** 2 );

      Input : Input_T( P.Umin'Range );
      
      GB_Int: Growth_Bound_Integration_T := P.Growth_Bound_Formula;
      Uncer_Zero : Bounds_of_Dynamic_Uncertainties_T( P.Xmin'Range ) := ( others => 0.0 );
   begin
      
      ---------------
      -- preparation
      ---------------
      
      -- set range of of "Values" of the growth bound
      GBVs_Acc.all.Values := new Growth_Bound_Values_NB_T
	( 0 .. Input_Quantity (P.Initial_Input_Space_Subdivision));
      for I in GBVs_Acc.all.Values.all'Range loop
	 GBVs_Acc.all.Values.all(I) := new Growth_Bound_Value_T;
	 GBVs_Acc.all.Values.all(I).all(G_State) := new State_Radius_T(P.Xmin'Range);
	 GBVs_Acc.all.Values.all(I).all(G_Uncer) := new State_Radius_T(P.Xmin'Range);
      end loop;
      
    
      

      
      -- set range of "Bounds" of the growth bound
      GBVs_Acc.all.Bounds_Of_Rounding_Error_Of_Growth_Bound 
	:= new Bounds_Of_Numerical_Errors_State_T(P.Xmin'Range);
      GBVs_Acc.all.Bounds_Of_Approximation_Error_Of_Growth_Bound
	:= new Bounds_Of_Numerical_Errors_State_T(P.Xmin'Range);
      GBVs_Acc.all.Bounds_Of_Summation_Error_Growth_Bound
	:= new Bounds_Of_Numerical_Errors_State_T(P.Xmin'Range);
	

      
      -- get input value via input index using package "Input_Values"       
      Input_Grid := Input_Values.Install ( P );
      
      ---------------
      -- main part
      ---------------
      
      -- loop to compute growth bound
      for I in GBVs_Acc.all.Values.all'Range loop
	 -- get input value
	 Input_Values.Get_Input_Value ( Input,
					I,
					Input_Grid );

	 -- compute parametrized matrix "L_b" for dynamics backward in time, taking into account all errors
	
	     
            L_Matrix.Compute_L_b ( L_b,                        
                                   P.Apriori_Enclosure_Backward_wout_DynamicUncertanties ( Lower ),
                                   P.Apriori_Enclosure_Backward_wout_DynamicUncertanties ( Upper ),
                                   Input,
                                   P.Bounds_Of_Input_Value_Rounding_Error );
         
         
         
         
            -- compute the first component of growth bound
            -- for state radius : "exp(Lt)r"
            GB_Int ( GBVs_Acc.all.Values.all(I).all(G_State).all,
                     P.Sampling_Time,
                     P.Rounded_Initial_State_Radius.all,
                     L_b,
                     Uncer_Zero );

        
                       
         
            -- compute the second component of growth bound 
            -- for measurement error and uncertainties : "Int(exp(Lt)w) + exp(Lt)z"
            GB_Int ( GBVs_Acc.all.Values.all(I).all(G_Uncer).all,
                     P.Sampling_Time,
                     P.Bounds_Of_Measurement_Errors.all,
                     L_b,
                     Uncer_Zero );
         
      
      end loop;
      
      -- add bounds info to "GBVs_Acc"
      for I in P.Xmin'Range loop
	 Copy_Error ( GBVs_Acc.all.Bounds_Of_Rounding_Error_Of_Growth_Bound.all,
		      P.Bounds_Of_Rounding_Error_Of_Growth_Bound.all );
	 Copy_Error ( GBVs_Acc.all.Bounds_Of_Approximation_Error_Of_Growth_Bound.all,
		      P.Bounds_Of_Approximation_Error_Of_Growth_Bound.all );
	 Copy_Error ( GBVs_Acc.all.Bounds_Of_Summation_Error_Growth_Bound.all,
               P.Bounds_Of_Summation_Error_Growth_Bound.all );
         
         
         
      end loop;
      

      GB_backward:=GBVs_Acc;
      
      
      
      
      
   end Compute_Backward_Growth_Bound_wout_DynamicUncertanties;
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   ------------------------------------------------------------
   -- Implementation of program in spec
   ------------------------------------------------------------
   procedure Get_Growth_Bound 
     ( GBV        : in out State_Radius_T;
       GBVs_Acc   : in     Growth_Bound_Values_T_Access;
       Cell_Ind   : in     Cell_Index_T;
       Input_Ind  : in     Input_Index_T )
   is
      
   begin
      
      -- clear the output
      -- GBV_Acc.all := ( others => 0.0 );
      
      -- dertermine the needed state radius using "Cell_Ind"
      -- Currently, use the rounded initial state radius
      
      
      -- compute growth bound by addition of data obtained from "GBVs_Acc" 
      -- here we take bounds of different kinds of errors into accounts
      for I in GBV'Range loop
	 GBV(I) :=
	   -- 1)
	   GBVs_Acc.all.Values.all(Input_Ind).all(G_State).all(I) +
	   -- 2) rounding error for 1)
	   GBVs_Acc.all.Bounds_Of_Rounding_Error_Of_Growth_Bound.all(I) +
	   -- 3) 
	   GBVs_Acc.all.Values.all(Input_Ind).all(G_Uncer).all(I) +
	   -- 4) rounding error for 2)
	   GBVs_Acc.all.Bounds_Of_Rounding_Error_Of_Growth_Bound.all(I) +
	   -- 5) formula approximation error  
	   GBVs_Acc.all.Bounds_Of_Approximation_Error_Of_Growth_Bound.all(I) +
	   -- 6) summation error of 1) + 3)
	   GBVs_Acc.all.Bounds_Of_Summation_Error_Growth_Bound.all(I);
      end loop;
      
   end Get_Growth_Bound;   
 
   

   
   
   
   
end Growth_Bound;
