with Established_Types; use Established_Types;
with Input_Values; use Input_Values;
with Cell_Cover; use Cell_Cover;
with Grids.Data;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Calendar; use Ada.Calendar;
with Interfaces.C; use Interfaces.C;
-- @summary
-- Abstractions according i14sym
--
-- @description
-- The function to compute the abstraction according i14sym 
-- and functions to read and write data from and to the cells.
package Abstraction_I14sym is

Assumption_Error :exception;



    
type Specification_SynthesisMethod_AbstractionRepresentation is (
                                                                    ReachAvoid_StandardDijkstra_SparseMatrix,
                                                                    ReachAvoid_OnTheFlyDijkstra_SparseMatrix,
                                                                
                                                                    Invariance_StandardMethod_SparseMatrix,
                                                                    Invariance_OnTheFlyMethod_SparseMatrix
                                                                   );  
   
type Counter_T is new Natural; -- There is no other way due to "new"
type Counter_Transitions_T is range 0..2**63-1;
type Cell_and_Overapproximation_T is record
      Cell : Cell_Index_T;
      Overapproximation : Abstract_Reachable_Set_Geometry_T;
end record;

type Array_of_Cell_and_Overapproximation_T is array (Counter_T range <>) of Cell_and_Overapproximation_T;
type Array_of_Cell_and_Overapproximation_T_Access is  access Array_of_Cell_and_Overapproximation_T;   
   
type Array_of_Cell_Index_T is array (Counter_T range <>) of Cell_Index_T;
type Array_of_Cell_Index_T_Access is access Array_of_Cell_Index_T;

type Array_of_Overapproximation_T is array (Counter_T range <>) of Abstract_Reachable_Set_Geometry_T;
type Array_of_Overapproximation_T_Access is access Array_of_Overapproximation_T;   
   

type Array_of_Counter_T is array (Input_Index_T range <> ) of Counter_T ;
-- Type for counters in the cells (e.g. remaining successors during Dijkstra)
   
type Array_of_Boolean_Marks is array (Input_Index_T range <> ) of Boolean ;
   -- Used for On-the-fly synthesis
type Array_of_Boolean_Marks_Access is access Array_of_Boolean_Marks ;  
   
   
type Abstraction_T(Synthesis:Specification_SynthesisMethod_AbstractionRepresentation) is private; 
type Abstraction_T_Access is access Abstraction_T;
 
   
   
   
type Predecessors_T is private;
   
-- Data related to the abstract specification
type Abstract_Specification_T is record
 InitialCells: Array_of_Cell_Index_T_Access;  
 -- A list with cells forming the abstract initial set
 TargetCells : Array_of_Cell_Index_T_Access;
 -- A list with cells forming the abstract target set
end record;

   
procedure Initialize_Abstraction(Abstraction    : in out Abstraction_T_Access;
                                 Representation : in Specification_SynthesisMethod_AbstractionRepresentation);
-- @return Enumeration data type containing information of the type of problem for which abstraction is computed, synthesis algorithm, abstraction data structure
-- @param  Abstraction     
function Get_Abstraction_Data_Structure (Abstraction : in Abstraction_T) return Specification_SynthesisMethod_AbstractionRepresentation;
-- @return Enumeration data type containing information of the type of problem for which abstraction is computed, synthesis algorithm, abstraction data structure
-- @param  Abstraction     
procedure Get_Predecessors(cell        : in Cell_Index_T; 
                           v           : in Input_Index_T; 
                           Abstraction : in  out Abstraction_T;
                           Predecessors: in out Dynamic_Cell_Container;
                           Associated_Overapproximations: in out Dynamic_Overapproximation_Container) ;
-- @param   cell        The index of the cell for which the predecessors and the associated overapproximation  should be returned
-- @param   v           The index of the control symbol
-- @param   Abstraction 
-- @return  An array with the indices and an overapproximation
procedure Get_Predecessors(cell        : in Cell_Index_T; 
                           v           : in Input_Index_T; 
                           Abstraction : in out Abstraction_T;
                           Predecessors: in out Dynamic_Cell_Container);
-- @param cell        The index of the cell for which the predecessors should be returned
-- @param v           The index of the control symbol
-- @param Abstraction 
-- @return            An array with the indices corresponding to 
procedure Decrease_Number_Of_Remaining_Successors(cell        : in Cell_Index_T; 
                                                  v           : in Input_Index_T; 
                                                  Abstraction : in Abstraction_T);
-- @brief This procedure decreases the counter for the number of successors for control symbol v by 1
-- @param cell        The index of the cell to decrease the counter
-- @param v           The index of the control symbol
-- @param Abstraction 
function  No_Remaining_Successors(cell        : in Cell_Index_T; 
                                  v           : in Input_Index_T; 
                                  Abstraction : in Abstraction_T) return Boolean;
-- @return True if the counter corresponding to cell and v is 0 otherwise False
-- @param cell        The index of the cell to test  
-- @param v           The index of the control symbol 
-- @param Abstraction 
function Get_Number_Of_Remaining_Successors(cell        : in Cell_Index_T; 
                                            v           : in Input_Index_T; 
                                            Abstraction : in Abstraction_T) return Counter_T;
-- @return The number of remaining successors for cell and v   
-- @param cell        The index of the cell
-- @param v           The index of the control symbol 
-- @param Abstraction 

function Get_First_Input_Index(Abstraction : in Abstraction_T) return Input_Index_T;
-- @return The first input index used in the abstraction
-- @param Abstraction
function Get_Last_Input_Index(Abstraction : in Abstraction_T) return Input_Index_T;
-- @return The last input index used in the abstraction
-- @param Abstraction

function Is_Index_Cell(idx: in Cell_Index_T;Abstraction : in Abstraction_T) return Boolean;
-- @return True if valid cell index, False otherwise
-- @param  idx   The index of the cell  
-- @param  Abstraction

function Get_Number_Of_Control_Symbols    (Abstraction : in Abstraction_T) return Input_Index_T;
-- @return Number of control symbols
-- @param  Abstraction
function Get_First_Index                  (Abstraction:  in Abstraction_T) return Cell_Index_T;
-- @return First valid cell index
-- @param  Abstraction
function Get_Last_Used_Index              (Abstraction : in Abstraction_T) return Cell_Index_T;
-- @return Last valid cell index
-- @param  Abstraction
function Get_Overflow_Index               (Abstraction : in Abstraction_T) return Cell_Index_T;
-- @return Overflow  cell index
-- @param  Abstraction
function  Is_Obstacle      (cell        : in Cell_Index_T; 
                            Abstraction : in Abstraction_T)  return Boolean;
-- @return True if cell belongs to obstacle set, False otherwise
-- @param  cell   The index of the cell  
-- @param  Abstraction


function  Is_Target        (cell        : in Cell_Index_T; 
                            Abstraction : in Abstraction_T)   return Boolean;
-- @return True if cell belongs to target set, False otherwise
-- @param  cell   The index of the cell  
-- @param  Abstraction


function  Is_Initial      (cell        : in Cell_Index_T; 
                           Abstraction : in Abstraction_T)  return Boolean; 
-- @return True if cell belongs to Initial set, False otherwise
-- @param  cell   The index of the cell  
-- @param  Abstraction  

   
procedure Save_As_Predecessor(from               : in Cell_Index_T;
                              overapproximation  : in Abstract_Reachable_Set_Geometry_T;
                              v                  : in Input_Index_T; 
                              to                 : in Cell_Index_T; 
                              Abstraction        : in out Abstraction_T) ;
-- @param  from               cell predecessor of to under control input v  
-- @param  v                  control input index 
-- @param  to                 cell successor of from under control input v
-- @param  overapproximation  overapproximation associated with from under v
-- @param  Abstraction  


   
   
procedure Save_As_Predecessor(from        : in Cell_Index_T;                           
                              v           : in Input_Index_T; 
                              to          : in Cell_Index_T; 
                              Abstraction : in out Abstraction_T);
-- @param  from               cell predecessor of to under control input v  
-- @param  v                  control input index 
-- @param  to                 cell successor of from under control input v
-- @param  Abstraction  

   
procedure Set_Mark(Cell         : in Cell_Index_T;
                   input        : in Input_Index_T;
                   Abstraction  : in out Abstraction_T) ;
-- @param  cell               index of a cell to be marked under control input  
-- @param  input              control input index 
-- @param  Abstraction  
   
function  Check_Mark(Cell          : in Cell_Index_T;
                     input         : in Input_Index_T;
                     Abstraction   : in  Abstraction_T) return Boolean ;
-- @return True if cell marked under input, False othewise
-- @param  cell               index of a cell  
-- @param  input              control input index 
-- @param  Abstraction        Abstraction    
   
procedure Clear_Cell_Data(Cell        : in Cell_Index_T;
                          Abstraction : in out Abstraction_T) ;
-- @param  cell               index of a cell for which the associated data is removed from Abstraction 
-- @param  Abstraction        Abstraction   
   
   

   
procedure Save_one_Overapproximation (Cell : in Cell_Index_T; Overapproximation : in Abstract_Reachable_Set_Geometry_T; Abstraction: in out Abstraction_T);
-- @param  cell               index of a cell for which the associated overapproximation is saved
-- @param  Overapproximation  Overapproximation of a reachable set
-- @param  Abstraction        Abstraction

function Read_one_Overapproximation  (Cell : in Cell_Index_T; Abstraction: in  Abstraction_T) return Abstract_Reachable_Set_Geometry_T;
-- @return  Overapproximation  Overapproximation of a reachable set
-- @param   cell               index of a cell for which the associated overapproximation is saved
-- @param   Abstraction        Abstraction 

  
procedure  Make_Obstacle      (cell        : in Cell_Index_T; 
                               Abstraction : in Abstraction_T);

-- @param   cell   index of a cell to be marked as obstacle
-- @param   Abstraction  

procedure  Make_Target      (cell        : in Cell_Index_T; 
                               Abstraction : in Abstraction_T);

-- @param   cell   index of a cell to be marked as target
-- @param   Abstraction  

procedure  Make_initial      (cell        : in Cell_Index_T; 
                               Abstraction : in Abstraction_T);

-- @param   cell   index of a cell to be marked as initial
-- @param   Abstraction  

 
private

   type idata_Entry_T is record 
      number_of_predecessors  : Counter_T;
      Pos_of_first_predecessor: Counter_T;
   end record;



   type idata_T is array (Input_Index_T range <>) of idata_Entry_T;
   type idata_T_access is access idata_T;
   

   
   

   type Predecessors_T is record
      data   : Array_of_Cell_Index_T_Access := null ;
      idata  :  idata_T_access;
   end record;   

   type Predecessors_OTF_T is record
      data   :  Array_of_Cell_and_Overapproximation_T_Access := null ;
      idata  :  idata_T_access;
   end record;   
	   
   type Predecessors_OTF_T_Access is access Predecessors_OTF_T;

   
   
   type Cell_T is record
      Predecessors            : Predecessors_T;
      counters_rem_successors : access Array_of_Counter_T;
      target   : Boolean      := False;
      obstacle : Boolean      := False;
      initial  : Boolean      := False;
   end record;

   type Cell_OTF_T is record
      Predecessors            : Predecessors_OTF_T_Access:=null;
      Boolean_Marks           : Array_of_Boolean_Marks_Access:=null;
      target   : Boolean      := False;
      obstacle : Boolean      := False;
      initial  : Boolean      := False;
   end record;

   type Cell_Invariance_T is record
      Predecessors            : Predecessors_T;
      target   : Boolean      := False;
      obstacle : Boolean      := False;
      initial  : Boolean      := False;
   end record;
   
   type Cell_Invariance_OTF_T is record
      Stored_Overapproximation: Abstract_Reachable_Set_Geometry_T;
      target   : Boolean      := False;
      obstacle : Boolean      := False;
      initial  : Boolean      := False;
   end record;

   package CellDataGrid                     is new cell_cover.BasicGrid.data(Data_Type => Cell_T);
   package CellDataGrid_OTF                 is new cell_cover.BasicGrid.data(Data_Type => Cell_OTF_T);
   package CellDataGrid_Invariance          is new cell_cover.BasicGrid.data(Data_Type => Cell_Invariance_T); 
   package CellDataGrid_Invariance_OTF      is new cell_cover.BasicGrid.data(Data_Type => Cell_Invariance_OTF_T); 
   
   


   
 type Abstraction_T(Synthesis:Specification_SynthesisMethod_AbstractionRepresentation) is record
 Number_of_Controls : Input_Index_T;
 First_Input_Index  : Input_Index_T;
 Last_Input_Index   : Input_Index_T;
 Cover              : Cell_Cover_T;
 Input_data         : Input_Grid_T;
 Overflow_Cell      : Cell_Index_T;
 Number_of_all_Transitions             : Counter_Transitions_T:=0;    
      
      case Synthesis is 
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) =>   
            Cells                  : CellDataGrid.Grid_Record_With_Data;   
            Overflow_Cell_Data     : Cell_T;
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix) =>  
            Cells_OTF              : CellDataGrid_OTF.Grid_Record_With_Data; 
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix)   =>  
            Cells_Invariance       : CellDataGrid_Invariance.Grid_Record_With_Data;  
            Overflow_Cell_Data_Invariance     : Cell_Invariance_T;
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix)   => 
            Cells_Invariance_OTF   : CellDataGrid_Invariance_OTF.Grid_Record_With_Data;
         when others =>
            
            null;
      
      end case;
      
   end record;

   

   
end Abstraction_I14sym;
