with Established_Types; use Established_Types;
with Input_Values; use Input_Values;
with Cell_Cover; use Cell_Cover;
with Grids.Data;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Calendar; use Ada.Calendar;
with abstraction_i14sym; use abstraction_i14sym;
with grids; 
with cell_cover; use cell_cover;
with Interfaces.C; use Interfaces.C;
package Set_of_Settled_States_E is

   type Boolean_Array_T is array (Cell_Index_T range <>) of Boolean;
   type Boolean_Array_T_Access is access Boolean_Array_T;

   type Cardinality_Counter_T is range 0..Cell_Index_T'Last;

   type E_Data_Structure is (Explicit_Array);

   type Set_of_Settled_States(Representation:E_Data_Structure) is private;

   type Set_of_Settled_States_Access is access Set_of_Settled_States;


   procedure Initialize_E (
                           Abstraction : in out Abstraction_T;
                           E: in out Set_of_Settled_States_Access;
                           Representation:E_Data_Structure
                          );
--@param Abstraction Abstraction
--@param E set of cells to be initialized
--@param Representation data structure



   procedure Finalize_E  (E: in out Set_of_Settled_States_Access);
--@param E set of cells to be finalized
   procedure Add_to_E     (Cell : in Cell_Index_T;E: in out Set_of_Settled_States_Access; Abstraction: in out Abstraction_T);
--@param cell index of a cell to be added to set
--@param E set of cells 

   function Test_Belongs  (Cell : in Cell_Index_T;E: in Set_of_Settled_States_Access; Abstraction: in  Abstraction_T) return Boolean;
--@param cell index of a cell to be tested if belongs to set
--@param E set of cells 
--@return True if belongs, false otherwise
   function Card  (E: in Set_of_Settled_States_Access) return Cardinality_Counter_T;
--@param E set of cells 
--@return Cardinality


   
   function Get_Data_Structure (E: in Set_of_Settled_States_Access)  return E_Data_Structure;

   private

   type Set_of_Settled_States(Representation:E_Data_Structure) is record


    First_Valid_State_Index : Cell_Index_T;
    Last_Valid_State_Index  : Cell_Index_T;
    Cardinality             : Cardinality_Counter_T:=0;




      case Representation is

         when E_Data_Structure'(Explicit_Array) =>

            Set_E                   : Boolean_Array_T_Access:=null;

         when others =>
            
            null;

      end case;





   end record;




end Set_of_Settled_States_E;
