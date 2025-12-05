with Established_Types; use Established_Types;
with Input_Values; use Input_Values;
with cell_cover; use cell_cover;
with Grids.Data;
with abstraction_i14sym; use abstraction_i14sym;
with Ada.Calendar; use Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO;use ADa.Text_IO;
with grids; 
with cell_cover; use cell_cover;
with Interfaces.C; use Interfaces.C;
package controller_i14sym is

type Controller_Data_Structure is (
                                      Defined_by_Abstraction,
                                      Explicit_Array_for_Reachabillity_Problem, 
                                      Explicit_Array_for_Safety_Problem_OnTheFly_Synthesis,
                                      Explicit_Array_for_Safety_Problem_Standard_Synthesis
                                     );   
   
   type Abstract_Controller_Type(Representation:Controller_Data_Structure) is private;
   type Abstract_Controller_Type_Access is access Abstract_Controller_Type;

   
   type Array_of_Control_Index is array (Cell_Index_T range <>) of Input_Index_T;
   type Array_of_Control_Index_access is access Array_of_Control_Index;

   type Array_of_Controls_for_a_Cell is array (Counter_T range <>) of Input_Index_T;
   type Array_of_Controls_for_a_Cell_Access is access Array_of_Controls_for_a_Cell;
   
  

procedure Initialize_Controller(
                                   Abstraction         : in out Abstraction_T; 
                                   Controller          : in out Abstract_Controller_Type_Access; 
                                   Representation      : in  Controller_Data_Structure
                                  ); 
--@param Abstraction Abstraction
--@param Controller controller to be initialized
--@param Representation controller data structure
   
procedure Finalize_Controller( 
                                  Controller          : in out Abstract_Controller_Type_Access
                                ); 
   
--@param Controller controller to be Finalized


procedure Set_Cell_Control(
                              cell       : in Cell_Index_T; 
                              v          : in Input_Index_T; 
                              Controller : in out Abstract_Controller_Type_Access;
                              Abstraction: in out Abstraction_T
                             );
   -- @description This procedure sets the controller's control symbol for cell to v
   -- @param cell The cell. Assumption: Valid index of a compact cell
   -- @param v The control symbol.
   -- @param Controller The controller. Assumption: Installed correctly.
procedure Read_Cell_Control(
                               cell          : in Cell_Index_T; 
                               Controller    : in  Abstract_Controller_Type_Access;
                               Abstraction   : in Abstraction_T;
                               Control_Input : out Input_Index_T
                              );
   -- @description This function returns the control symbol for cell in the controller
   -- @param cell The cell. Assumption: Valid index of a cell
   -- @param Controller The controller. Assumption: Installed correctly.

procedure Read_Array_of_Cell_Controls(
                               cell          : in Cell_Index_T; 
                               Controller    : in  Abstract_Controller_Type_Access;
                               Control_array : out Array_of_Controls_for_a_Cell_Access
                              );
   
   -- @description This function returns the array of control symbols for cell in the controller
   -- @param cell The cell. Assumption: Valid index of a cell
   -- @param Controller The controller. Assumption: Installed correctly.   
   
   
   
   
   
procedure Include_Cell_Control (
                                   cell       : in Cell_Index_T; 
                                   v          : in Input_Index_T; 
                                   Controller : in out Abstract_Controller_Type_Access
                                  );  
   
   -- @description This function includes a control input to the setof inputs associated with a cell
   -- @param cell The cell. Assumption: Valid index of a cell
   -- @param Controller The controller. Assumption: Installed correctly.   
   
      
procedure Exclude_Cell_Control (
                                   cell       : in Cell_Index_T; 
                                   v          : in Input_Index_T; 
                                   Controller : in out Abstract_Controller_Type_Access
                                  );   
   -- @description This function excludes a control input  from the set of inputs associated with a cell
   -- @param cell The cell. Assumption: Valid index of a cell
   -- @param Controller The controller. Assumption: Installed correctly.   
   
   
function Get_num_of_valid_Controls  (cell: in Cell_Index_T; Controller : in Abstract_Controller_Type_Access) return Input_Index_T;
   -- @return number of control inputs   
   -- @description This function returns the number of controlls associated with a cell
   -- @param cell The cell. Assumption: Valid index of a cell
   -- @param Controller The controller. Assumption: Installed correctly.   
   
function Get_First_Index (Controller : in  Abstract_Controller_Type_Access)  return Cell_Index_T;
   -- @return first valid cell index in the controller   
   -- @param Controller The controller. Assumption: Installed correctly.   
function Get_Last_Index  (Controller : in  Abstract_Controller_Type_Access)  return Cell_Index_T;
    -- @return last valid cell index in the controller   
    -- @param Controller The controller. Assumption: Installed correctly. 
   
function Get_Reserved_Control_Symbol_1(Controller : in  Abstract_Controller_Type_Access)  return Input_Index_T;
    -- @return control symbol that indicates all valid controls   
    -- @param Controller The controller. Assumption: Installed correctly.
function Get_Reserved_Control_Symbol_2(Controller : in  Abstract_Controller_Type_Access)  return Input_Index_T;
    -- @return control symbol that indicates no valid controls   
    -- @param Controller The controller. Assumption: Installed correctly.
   
function Controller_Representation (Controller  : in  Abstract_Controller_Type_Access)  return Controller_Data_Structure;
    -- @return  controller data structure    
    -- @param   Controller The controller. Assumption: Installed correctly.
procedure free_cell_controls         is new Ada.Unchecked_Deallocation (Array_of_Controls_for_a_Cell,Array_of_Controls_for_a_Cell_Access);
private 


   
   type Cell_Controls_T is record     
      controls_array : Array_of_Controls_for_a_Cell_Access:=null;
      num_of_controls: Counter_T:=0;    
   end record;
   
   type Set_Valued_Control_Map_Sparse_Matrix_T is array (Cell_Index_T range <>) of Cell_Controls_T ;
   type Set_Valued_Control_Map_Sparse_Matrix_T_Access is access Set_Valued_Control_Map_Sparse_Matrix_T;
 
   
   
   type Array_of_Control_Marks_for_a_Cell is array (Input_Index_T range <>) of Boolean;
   type Array_of_Control_Marks_for_a_Cell_Access is access Array_of_Control_Marks_for_a_Cell;
   
    type Cell_Control_Marks_T is record     
      marks : Array_of_Control_Marks_for_a_Cell_Access:=null;
      num_of_valid_controls: Input_Index_T;    
   end record;
   
   
   type Set_Valued_Control_Map_Dense_Matrix_T is array (Cell_Index_T range <>) of Cell_Control_Marks_T;
   type Set_Valued_Control_Map_Dense_Matrix_T_Access is access Set_Valued_Control_Map_Dense_Matrix_T;
   
   
   
   
   
   type Abstract_Controller_Type(Representation:Controller_Data_Structure) is record

      
      First_Valid_State_Index   :Cell_Index_T;
      Last_Valid_State_Index    :Cell_Index_T;
      First_Valid_Control_Index :Input_Index_T;
      Last_Valid_Control_Index  :Input_Index_T;
      
      Reserved_Control_Index_1  :Input_Index_T;
      --all controls for cell
      Reserved_Control_Index_2  :Input_Index_T;
      --no controls for cell
      
      case Representation is

        when Controller_Data_Structure'(Explicit_Array_for_Reachabillity_Problem) =>

            Controller_Array_Reach                       : Array_of_Control_Index_access:=null;
 
        when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_OnTheFly_Synthesis) =>

            Controller_Array_Invariance                  : Set_Valued_Control_Map_Sparse_Matrix_T_Access:=null;
         
        when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_Standard_Synthesis) =>   
            
            Controller_Array_Invariance_Dense            : Set_Valued_Control_Map_Dense_Matrix_T_Access:=null;
            
        when others =>

            null;

      end case;
   
   end record;
   
   
   
   




   
end controller_i14sym;
