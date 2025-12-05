package body value_function is

   svn_author  : constant String := "$Author: lf3eelma $";
   svn_revision: constant String := "$Revision: 2402 $";
   svn_date    : constant String := "$Date: 2021-10-26 11:09:50 +0200 (Di, 26 Okt 2021) $";


   procedure free_array_of_values is new Ada.Unchecked_Deallocation(Array_of_Values_T,Array_of_Values_T_Access);

   procedure free_value_function_pointer is new Ada.Unchecked_Deallocation(Value_Function_T,Value_Function_T_access);

   procedure Finalize_Value_Function ( W    : in out Value_Function_T_access) is

   begin

     case W.Representation is
      when W_Data_Structure'(Explicit_Array)  =>

         free_array_of_values(W.Value_Function);
         free_value_function_pointer(W);

      when others =>
         null;

      end case;


      end Finalize_Value_Function;



   procedure Set_Cell_Value (
                             Cell    : in Cell_Index_T;
                             Value   : in Value_T;
                             W       : in out Value_Function_T_access
                            ) is


   begin

      case W.Representation is
      when W_Data_Structure'(Explicit_Array)  =>

            W.Value_Function(Cell):=Value;

      when others =>
         null;

      end case;



   end Set_Cell_Value;


   procedure Initialize_Value_Function (
                                        Abstraction                : in Abstraction_T;
                                        Abstract_Spec              : in Abstract_Specification_T;
                                        W                          : in out Value_Function_T_access;
                                        Representation             : in W_Data_Structure
                                       ) is

   begin
      W:= new Value_Function_T(Representation);

      W.First_Valid_State_Index := Get_First_Index(Abstraction);
      W.Last_Valid_State_Index  := Get_Last_Used_Index(Abstraction);


      case Representation is
      when W_Data_Structure'(Explicit_Array)  =>


         W.Value_Function:= new Array_of_Values_T(W.First_Valid_State_Index .. W.Last_Valid_State_Index);



          case Abstraction.Synthesis is

         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix)=>
            For i in W.First_Valid_State_Index..W.Last_Valid_State_Index  loop

               Set_Cell_Value(Cell  => i,
                              Value => Value_T'Last,
                              W     => W);


               if  Is_Target(i,Abstraction)  then
                  Set_Cell_Value(Cell  => i,
                                 Value => 0,
                                 W     => W);

               end if;

            end loop;

         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix)=>
            For i in W.First_Valid_State_Index..W.Last_Valid_State_Index  loop

               Set_Cell_Value(Cell  => i,
                              Value => Value_T'Last,
                              W     => W);


               if  Is_Target(i,Abstraction)  then
                  Set_Cell_Value(Cell  => i,
                                 Value => 0,
                                 W     => W);

               end if;

            end loop;

          when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix)=>

               For i in W.First_Valid_State_Index..W.Last_Valid_State_Index  loop

                  Set_Cell_Value(Cell  => i,
                              Value => 0,
                              W     => W);


                  if Is_Obstacle(i,Abstraction) or not Is_Target(i,Abstraction)  then
                     Set_Cell_Value(Cell  => i,
                                    Value => Value_T'Last,
                                    W     => W);

                  end if;

               end loop;

            when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix)=>

               For i in W.First_Valid_State_Index..W.Last_Valid_State_Index  loop

                  Set_Cell_Value(Cell  => i,
                                 Value => 0,
                                 W     => W);


                  if Is_Obstacle(i,Abstraction) or not Is_Target(i,Abstraction)  then
                     Set_Cell_Value(Cell  => i,
                                    Value => Value_T'Last,
                                    W     => W);

                  end if;

               end loop;

         when others =>
            null;

      end case;


      when others =>
         null;

      end case;






   end Initialize_Value_Function;




   procedure Read_Cell_Value (
                              Cell    : in Cell_Index_T;
                              Value   : out Value_T;
                              W       : in  Value_Function_T_access
                             ) is

   begin

      case W.Representation is
      when W_Data_Structure'(Explicit_Array)  =>
  
            Value:= W.Value_Function(Cell);

      when others =>
         null;

      end case;



   end Read_Cell_Value;




   function Compare_Cell_to_Succ_plus_Trans_Cost (
                                                  Cell    : in Cell_Index_T;
                                                  Succ    : in Cell_Index_T;
                                                  W                          : in out Value_Function_T_access;
                                                  Value     : out Value_T
                                                 ) return Boolean is
      b:Boolean:=False;
      value_cell_1, value_cell_2: Value_T;
   begin


      case W.Representation is
      when W_Data_Structure'(Explicit_Array)  =>


           Read_Cell_Value(Cell  => Cell,
                            Value => value_cell_1,
                            W     => W);

           Read_Cell_Value(Cell  => Succ,
                            Value => value_cell_2,
                            W     => W);

            if value_cell_1>value_cell_2+1 then

               Value:=value_cell_2+1;
               b:=True;

            else
               Value:=value_cell_1;


            end if;

      when others =>
         null;

      end case;

           return b;

   end Compare_Cell_to_Succ_plus_Trans_Cost;


   function Value_Function_Representation(W: in Value_Function_T_access)  return  W_Data_Structure is

   begin
      return W.Representation;
   end Value_Function_Representation;

   function Get_First_Index(W: in Value_Function_T_access)  return Cell_Index_T is
   begin

      return W.First_Valid_State_Index;
   end Get_First_Index;

   function Get_Last_Index(W: in Value_Function_T_access)  return Cell_Index_T is
   begin

      return W.Last_Valid_State_Index;
   end Get_Last_Index;





   end value_function;
