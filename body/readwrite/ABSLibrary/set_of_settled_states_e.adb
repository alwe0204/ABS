package body Set_of_Settled_States_E is

   svn_author  : constant String := "$Author: lf3eelma $";
   svn_revision: constant String := "$Revision: 2402 $";
   svn_date    : constant String := "$Date: 2021-10-26 11:09:50 +0200 (Di, 26 Okt 2021) $";

   procedure Initialize_E (Abstraction : in  out Abstraction_T;
                           E: in out Set_of_Settled_States_Access;
                           Representation:in E_Data_Structure) is
   begin
      E:= new Set_of_Settled_States(Representation);

      E.First_Valid_State_Index:=Get_First_Index   (Abstraction);
      E.Last_Valid_State_Index:=Get_Last_Used_Index(Abstraction);


      case Representation is
      when E_Data_Structure'(Explicit_Array)  =>


         E.Set_E:= new Boolean_Array_T(E.First_Valid_State_Index .. E.Last_Valid_State_Index);
         E.Set_E.all:=(others=>False);
    
      when others =>
         null;

      end case;

   end Initialize_E;

  procedure free_Boolean_Array is new Ada.Unchecked_Deallocation(Boolean_Array_T, Boolean_Array_T_Access);
  procedure free_record_access is new Ada.Unchecked_Deallocation(Set_of_Settled_States, Set_of_Settled_States_Access);

  procedure Finalize_E  (E: in out Set_of_Settled_States_Access) is

   begin

      case E.Representation is

         when E_Data_Structure'(Explicit_Array) =>

            free_Boolean_Array(E.Set_E);
            free_record_access(E);


         when others =>


            null;

      end case;


   end Finalize_E;


   procedure Add_to_E     (Cell : in Cell_Index_T;E: in out Set_of_Settled_States_Access; Abstraction: in out Abstraction_T) is
 
   begin

--If E.Set_E(Cell) then Put_Line("Repeated inclusion of existing element to the set of settled states. Error"); raise Abstraction_I14sym.Assumption_Error; end if;

      case E.Representation is

         when E_Data_Structure'(Explicit_Array) =>

               E.Set_E(Cell):=True;
               E.Cardinality:=E.Cardinality+1;
       
         when others =>


            null;

      end case;


   end Add_to_E;



   function Test_Belongs  (Cell : in Cell_Index_T;E: in Set_of_Settled_States_Access; Abstraction: in  Abstraction_T) return Boolean is

b: Boolean;
   begin

      case E.Representation is

         when E_Data_Structure'(Explicit_Array) =>

                          b:= E.Set_E(Cell);
         when others =>

            null;

      end case;

      return b;
   end Test_Belongs;




   function Card  (E: in Set_of_Settled_States_Access) return Cardinality_Counter_T is

      c: Cardinality_Counter_T:=0;
   begin

      case E.Representation is

         when E_Data_Structure'(Explicit_Array) =>

            c:= E.Cardinality;

         when others =>


            null;

      end case;
      return c;
   end Card;


function Get_Data_Structure (E: in Set_of_Settled_States_Access)  return E_Data_Structure is

begin
return E.Representation;
end Get_Data_Structure;

end Set_of_Settled_States_E;
