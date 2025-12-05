with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C; 

with abcs_text_io; use abcs_text_io;

with Established_Types; use Established_Types;
with Input_Values; use Input_Values;

with problem_loading;

with cell_cover; use cell_cover;

with apriori_enclosure;
with Growth_Bound; 


-- @summary
-- a program testing the packages "growth_bound" and "apriori_enclosure"

-- @description
-- this test empolys the example "Single_Pole"

function Test_Growth_Bound 
  return Integer
is
   
   svn_author  : constant String := "$Author: lf3eelma $";
   svn_revision: constant String := "$Revision: 2406 $";
   svn_date    : constant String := "$Date: 2021-10-26 13:12:54 +0200 (Di, 26 Okt 2021) $";   

   -- build an instance of problem
   P : Problem_T := problem_loading.install;
   -- a reference for tesing package "apriori_enclosure" 
   Math_Enc : Compact_Hyperinterval_T;
   -- build an instance to store growth bound
   GB : Growth_Bound.Growth_Bound_Values_T_Access;
   GB2 : Growth_Bound.Growth_Bound_Values_T_Access;
   -- prepare input index   
   Input_Grid : Input_Values.Input_Grid_T
     := Input_Values.Install( P );
   Input_First_Index : Input_Values.Input_Index_T
     := Input_Values.Get_First_Index ( Input_Grid );
   Input_Last_Index : Input_Values.Input_Index_T 
     := Input_Values.Get_Last_Index ( Input_Grid );

   -- prepare cell index
   -- ** Currently, growth bound is independent of state radius
   -- ** so that cell index is not in use but for future
   --   Cell_Cover : Cell_Cover.Cell_Cover_T := Cell_Cover.Install( P );
   --   Cell_First_Index : Cell_Cover.Get_First_Index ( Cover );
   --   Cell_Last_Index : Cell_Cover.Get_Last_Index ( Cover );
   Cell_Index : Cell_Cover.Cell_Index_T := 0;
   
   -- store the result of the functions from package "growth_bound" to be tested     
   Beta : State_Radius_T(P.Xmin'Range) := (others =>0.0);
   -- store reference to compare with 

   type Math_Beta_T is array(P.Xmin'Range) of NonNegative_Float_T;
   type Math_Beta_Array_T is array(Input_First_Index..Input_Last_Index) of Math_Beta_T ;
   Math_Beta : Math_Beta_Array_T;


begin
   
   ------------------------------   
   -- from Alex : Test the package "apriori_enclosure"
   ------------------------------
   math_enc(Upper) := new Vector_Float_T (P.xmin'Range);
   math_enc(Lower) := new Vector_Float_T (P.xmin'Range);
   math_enc(Lower)(1):= Float_T(-7.4016);
   math_enc(Lower)(2):= Float_T(-3.7280);
   math_enc(Upper)(1):= Float_T(7.4016);
   math_enc(Upper)(2):= Float_T(3.7280);
   
   -- run function to be tested
   Apriori_Enclosure.Compute ( P );
   -- examine the result
   for i in P.xmin'Range loop
    if P.Apriori_Enclosure(Lower)(i) > P.xmin(i) then return 1;
    end if;
    if P.Apriori_Enclosure(Upper)(i) < P.xmax(i) then return 1;
    end if;
    if P.Apriori_Enclosure(Lower)(i) < math_enc(Lower)(i) then return 1;
    end if; 
    if P.Apriori_Enclosure(Upper)(i) > math_enc(Upper)(i) then return 1;
    end if; 
   end loop;
   
   -------------------------------
   -- Test the package "growth_bound"
   ------------------------------
   
   -- set the reference parameter that is a numerical result 
   Math_Beta := 
     ((NonNegative_Float_T(4.08025E-02),NonNegative_Float_T(4.35941E-02)),
      (NonNegative_Float_T(3.89043E-02),NonNegative_Float_T(3.02838E-02)),
      (NonNegative_Float_T(4.08025E-02),NonNegative_Float_T(4.35941E-02)));
   
   -- run the function "Compute_Growth_Bound" to be tested
   Growth_Bound.Compute_Growth_Bound ( P,Gb ); 
   -- run the function "Get_Growth_Bound" to be tested
   -- and compare with the reference
   for I in Input_First_Index .. Input_Last_Index loop
      Growth_Bound.Get_Growth_Bound ( Beta, GB, Cell_Index, I );    
      for J in Beta'Range loop
	 if Beta(J) < Math_Beta(I)(J)
	 then return 1;
	 end if;
      end loop;
   end loop;
------------------------------------------------------------
--      -- print growth bounds ordered by index line by line 
--   Put_Line ("########## beging examining the growth bound:");
--      Put_Line ( "Input Index : " & I'Image );
--      for J in Beta'Range loop
--	 Put ( Beta(J)'Image & "    " );
--      end loop;
--      New_Line;
--   Put_Line ("########## end growth bound");
------------------------------------------------------------      
   return 0;   
end Test_Growth_Bound;
