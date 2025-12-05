with Established_Types; use Established_Types;
with Input_Values; use Input_Values;
with Cell_Cover; use Cell_Cover;
with abstraction_i14sym; use abstraction_i14sym;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Calendar; use Ada.Calendar;
package value_function is

   type W_Data_Structure is (Explicit_Array, Binary_Decision_Diagram);

   W_Bits: constant := 32;
   type Value_T  is range 0 ..  2**W_Bits - 1 ;
   for Value_T'Size use W_Bits;


   type Value_Function_T(Representation:W_Data_Structure) is private;
   type Value_Function_T_access is access Value_Function_T;


   procedure Initialize_Value_Function (
                                        Abstraction                : in Abstraction_T;
                                        Abstract_Spec              : in Abstract_Specification_T;
                                        W                          : in out Value_Function_T_access;
                                        Representation             : in W_Data_Structure
                                       );
--@param Abstraction Abstraction
--@param Abstract_Spec Abstract specification
--@param W value function to be initialized
--@param Representation value function data structure


   procedure Finalize_Value_Function (W  : in out Value_Function_T_access);

--@param W value function to be finalized


   procedure Set_Cell_Value (
                             Cell    : in Cell_Index_T;
                             Value   : in Value_T;
                             W       : in out Value_Function_T_access
                            );

--@param Cell index of a cell for which thevalue is written
--@param Value value of cell
--@param W value function 


   procedure Read_Cell_Value (
                              Cell    : in Cell_Index_T;
                              Value   : out Value_T;
                              W       : in  Value_Function_T_access
                             );
--@param Cell index of a cell for which the value is read
--@param Value value of cell
--@param W value function 




   function Compare_Cell_to_Succ_plus_Trans_Cost (
                                                  Cell    : in Cell_Index_T;
                                                  Succ    : in Cell_Index_T;
                                                  W                          : in out Value_Function_T_access;
                                                  Value     : out Value_T
                                                 ) return Boolean;

--@param Cell index of a cell
--@param Succ index of a cell
--@param W value function 
--@param Value largest of two values 
--@return True if valuee of cell 1 is greater than value of cell 2 plus transition cost



   function Value_Function_Representation              (W: in Value_Function_T_access) return  W_Data_Structure;
   function Get_First_Index                            (W: in Value_Function_T_access) return  Cell_Index_T;
   function Get_Last_Index                             (W: in Value_Function_T_access) return  Cell_Index_T;

private

   type Array_of_Values_T is array (Cell_Index_T range <>) of Value_T;
   type Array_of_Values_T_access is access Array_of_Values_T;


   type Value_Function_T(Representation:W_Data_Structure) is record

   

      First_Valid_State_Index : Cell_Index_T;
      Last_Valid_State_Index  : Cell_Index_T;


      case Representation is

         when W_Data_Structure'(Explicit_Array) =>

            Value_Function                   : Array_of_Values_T_access:=null;

         when others =>

            null;


      end case;




   end record;













end value_function;
