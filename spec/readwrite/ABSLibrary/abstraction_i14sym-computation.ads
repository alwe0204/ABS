with Established_Types; use Established_Types;
with Input_Values; use Input_Values;
-- @summary Method for computing the abstraction according i14sym. 
package Abstraction_I14sym.Computation is
 Assumption_Error : exception;

   procedure Install(P : in Problem_T;
                    Abstraction: in out Abstraction_T);
-- @param P The specification of the problem
-- @param Abstraction An instance of an abstraction
-- @description Creates the data structure for an abstraction

procedure Install_Discrete(First_Cell_Index  : in Cell_Index_T ;
                 Last_Cell_Index   : in Cell_Index_T ;
                 First_Input_Index : in Input_Index_T ; 
                  Last_Input_Index  : in Input_Index_T;
                  Abstraction: in out Abstraction_T) ;
-- @param First_Cell_Index The first index of the states in the abstraction
-- @param Last_Cell_Index The last index of the states in the abstraction
-- @param First_Input_Index The first input index used in the abstraction
-- @param Last_Input_Index The last input index used in the abstraction
-- @return An instance of an abstraction
-- @description Creates the data structure for an abstraction with only discrete data.


 generic
  with procedure Get_Successors(successors : in out Dynamic_Cell_Container;
                                      cell : in Cell_Index_T; 
                                         v : in Input_Index_T);
  -- @param successors The obtained successor cells
  -- @param cell The cell 
  -- @param v The control index
  -- @description This procedure obtaines the successor cells of the cell associated with control symbol v
procedure Compute(          Abstraction : in out Abstraction_T;
                  AbstractSpecification : in out Abstract_Specification_T; 
                                      P : in Problem_T);
-- @description Computation of the abstraction, the initial, target and obstacle cells.
-- @param Abstraction 
-- @param AbstractSpecification The target cells and the initial cells are stored in lists of this data structure
-- @param P Problem specification

   



 generic
  with procedure Get_Successors(successors : in out Dynamic_Cell_Container;
                                      cell : in Cell_Index_T; 
                                         v : in Input_Index_T);
  -- @param successors The obtained successor cells
  -- @param cell The cell 
  -- @param v The control index
  -- @description This procedure obtaines the successor cells of the cell associated with control symbol v
procedure Get_All_Transitions(Abstraction : in out Abstraction_T);
-- @description Computation of the transition function of the abstraction by using get_successors
-- @param Abstraction 

private 

 generic
  with procedure Get_Intersecting_Cells(found_cells : in out Dynamic_Cell_Container;
                                        lower       : State_T;  
                                        upper       : State_T);
  with procedure Get_Containing_Cells(found_cells : in out Dynamic_Cell_Container;
                                      lower       : State_T;  
                                      upper       : State_T);
procedure Get_Abstract_Specification(Abstraction : in out Abstraction_T; 
                                     AbstractSpec: in out Abstract_Specification_T; 
                                     P           : in Problem_T);
   
 
     
   
end Abstraction_I14sym.Computation ; 
