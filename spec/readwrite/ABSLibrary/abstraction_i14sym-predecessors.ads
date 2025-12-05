--@summary This package organizes the storing of the predecessors of each cell.
package abstraction_i14sym.predecessors is
function Initialize_Predecessors(Number_of_Controls : in Input_Index_T) return Predecessors_T;
-- @param Number_of_Controls The number of different control symbols
-- @return Data structure for storing the predecessors of a cell

   procedure Get_Predecessors(cell        : in Cell_Index_T; 
                              v           : in Input_Index_T; 
                              Abstraction : in Abstraction_T;
                              Predecessors: in out Dynamic_Cell_Container;
                              List_of_Overapproximations : in out Dynamic_Overapproximation_Container) ;
   -- @param cell The cell index
   -- @param v The input index
   -- @param Abstraction The abstraction
   -- @return Returns the predecessors of cell for input v in Abstraction
   procedure Save_As_Predecessor(from        : in Cell_Index_T; 
                                 v           : in Input_Index_T; 
                                 to          : in Cell_Index_T; 
                                 Abstraction : in out Abstraction_T);
   -- @param from Origin of the transition to store
   -- @param v Input index of transition
   -- @param to Target cell of the transition
   -- @param Abstraction The abstraction
   -- @description Stores in cell with index to that cell with index from under input index v is a predecessor of cell with index to

   procedure Save_As_Predecessor_Overflow(from        : in Cell_Index_T; 
                                          v           : in Input_Index_T; 
                                          Abstraction : in out Abstraction_T) ;   
   -- @param from Origin of the transition to store
   -- @param v Input index of transition
   -- @param Abstraction The abstraction
   -- @description Stores in the overflow cell that cell with index from under input index v which has transition tto outside of the domain
  
   
   
   procedure Save_As_Predecessor(
                                 
                                 from               : in Cell_Index_T;
                                 overapproximation  : in Abstract_Reachable_Set_Geometry_T;
                                 v                  : in Input_Index_T; 
                                 to                 : in Cell_Index_T; 
                                 Abstraction        : in out Abstraction_T) ;
  
   -- @param from Origin of the transition to store
   -- @param from Overapproximation associated with from and v
   -- @param v Input index of transition
   -- @param to Target cell of the transition
   -- @param Abstraction The abstraction
   -- @description Stores in cell with index to that cell with index from under input index v is a predecessor of cell with index to

   
   
end Abstraction_I14sym.Predecessors ;
