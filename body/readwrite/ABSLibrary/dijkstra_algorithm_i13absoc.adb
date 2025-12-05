with Ada.Text_IO; use Ada.Text_IO;
with Input_Values; use Input_Values;
with abstraction_i14sym; use abstraction_i14sym;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
use Ada.Containers;
with abstraction_i14sym.predecessors;
with abstraction_i14sym.computation; use abstraction_i14sym.computation;
with grids;
with Established_Types; use Established_Types;
with cell_cover;
with Ada.Integer_Text_IO;           use Ada.Integer_Text_IO;




package body dijkstra_algorithm_i13absoc is

   svn_author  : constant String := "$Author: lf3eelma $";
   svn_revision: constant String := "$Revision: 2443 $";
   svn_date    : constant String := "$Date: 2021-10-29 15:32:12 +0200 (Fr, 29 Okt 2021) $";






procedure Compute(Solution    : in out Abstract_OCP_Solution_Type;
                     Abstraction : in out abstraction_i14sym.Abstraction_T;
                     AbstractSpec: in abstraction_i14sym.Abstract_Specification_T) is


------------------------------------------------------------------------------------------------------------------------------------------


      use cell_cover.BasicGrid;
      use input_values.InputGrid;


      u_first      :Input_Values.Input_Index_T := abstraction_i14sym.Get_First_Input_Index(Abstraction);  --first control symbol
      u_last       :Input_Values.Input_Index_T := get_number_of_control_symbols(Abstraction)-1;
      Closed_Cell  :Cell_Cover.Cell_Index_T;
      M            :Value_T;

      E            :Set_of_Settled_States_Access;
      Q            :Cell_Heap_access;

      Closed_Cell_Pred                     :Dynamic_Cell_Container;
      Associated_Overapproximations        :Dynamic_Overapproximation_Container;

      function Obstacle_Suboptimal_or_Self_loop(Cell: in Cell_Index_T; Successor : in Cell_Index_T) return Boolean is
         v:Value_T;
      begin
         If Is_Obstacle(Successor,Abstraction )   or Compare_Cell_to_Succ_plus_Trans_Cost(Cell  => Cell,
                                                                                       Succ  => Successor,
                                                                                       W     => Solution.Value_Function,
                                                                                       Value => v)=False
         then return True;
         else return False;
         end If;
      end Obstacle_Suboptimal_or_Self_loop;



      function Test_procedure_not_in_E(Cell: in Cell_Index_T; Successor : in Cell_Index_T) return Boolean is
      begin
         If Test_Belongs(Cell => Successor,
                         E    => E,
                         Abstraction => Abstraction)=False then return True;
         else return False;
         end If;
      end Test_procedure_not_in_E;


      function Test_procedure_in_E(Cell: in Cell_Index_T; Successor : in Cell_Index_T) return Boolean is
      begin
         return Test_Belongs(Cell => Successor,
                             E    => E,
                             Abstraction => Abstraction);
      end Test_procedure_in_E;


      function Test_Always_False(Cell: in Cell_Index_T; Successor : in Cell_Index_T) return Boolean is
      begin
         return False;
      end Test_Always_False;


     k:Integer:=0;

      Traversal_Complete    :Boolean;
      successor             :Cell_Index_T;
      overapproximation     :Abstract_Reachable_Set_Geometry_T;
      cell_value:Value_T;
      cell_control, new_cell_control: Input_Index_T;
      symbol_for_all_controls :Input_Index_T;
      symbol_for_no_controls  :Input_Index_T;
      number_of_abstract_controls : Input_Index_T;
      
      e_count:Long_Integer:=0;
      
begin


      Initialize_E                (Abstraction    => Abstraction,
                                   E              => E,
                                   Representation => E_Data_Structure'(Explicit_Array));
                                   
    

      Initialize_Heap             (H             => Q,
                                   Abstraction   => Abstraction,
                                   Abstract_Spec => AbstractSpec);


      Initialize_Value_Function   (Abstraction    => Abstraction,
                                   Abstract_Spec  => AbstractSpec,
                                   W              => Solution.Value_Function,
                                   Representation => W_Data_Structure'(Explicit_Array));





      Initialize_Controller       (Abstraction    => Abstraction,
                                   Controller     => Solution.Controller,
                                   Representation => Controller_Data_Structure'(Defined_by_Abstraction));



Put_line("  Initial heap size:          "& Current_card(Q)'img );


 loop                                                    
  exit when Is_Empty(Q);                       
  Closed_Cell:=Remove_min(Q); 
                           
         if Is_index_cell(Closed_Cell,Abstraction) then
            Add_to_E(Closed_Cell,E,Abstraction); 
            e_count:=e_count+1; 
            
    --     if e_count mod 100000 = 0 then 
     --    Put_Line("card(E)="& e_count'img);
     --    end if;   
                                               
         else null;
         end if;

 case Get_Abstraction_Data_Structure(Abstraction) is
    when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix)=>
-- standard Dijkstra Algorithms
  For iterated_control_symbol in  u_first..u_last loop         

     Empty_container(Closed_Cell_Pred);
     get_predecessors(Closed_Cell,iterated_control_symbol,Abstraction,Closed_Cell_Pred);

               if Closed_Cell_Pred.Length>0 then
                  for  i in 0..Closed_Cell_Pred.Length-1 loop                        
                   if not Is_obstacle(CLosed_Cell_Pred.Dynamic_Container(i),Abstraction) then

                     If Test_Belongs(CLosed_Cell_Pred.Dynamic_Container(i),E,Abstraction)=False then

                        decrease_number_of_remaining_successors(CLosed_Cell_Pred.Dynamic_Container(i),iterated_control_symbol,Abstraction);

                        If no_remaining_successors(CLosed_Cell_Pred.Dynamic_Container(i),iterated_control_symbol,Abstraction) then

                           If Compare_Cell_to_Succ_plus_Trans_Cost (CLosed_Cell_Pred.Dynamic_Container(i),Closed_Cell,Solution.Value_Function,M) then

                              Set_Cell_Value    (Cell  => CLosed_Cell_Pred.Dynamic_Container(i),
                                                 Value => M,
                                                 W     => Solution.Value_Function);

                              Set_Cell_Control  (cell       => CLosed_Cell_Pred.Dynamic_Container(i),
                                                 v          => iterated_control_symbol,
                                                 Controller => Solution.Controller,
                                                 Abstraction=> Abstraction);


                              Add_to_Heap(CLosed_Cell_Pred.Dynamic_Container(i),Q);



                           end if;                                                    

                        end If;                                                     

                     end if;

                   end if;

                  end loop;                                                    
       

               end if;

  end loop;                                                     

         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix) =>
-- On-the-fly Dijkstra Algorithm


            For iterated_control_symbol in  u_first..u_last loop             --loop over all control inputs

               Empty_container(Closed_Cell_Pred);
               Empty_container(Associated_Overapproximations);

               get_predecessors(Closed_Cell,
                                iterated_control_symbol,
                                Abstraction,Closed_Cell_Pred,
                                Associated_Overapproximations);




               If Closed_Cell_Pred.Length/=Associated_Overapproximations.Length then

                  Put_Line("Number of stored predecessors is not equal to the nubmer of stored reachable sets");
                  raise Abstraction_I14sym.Assumption_Error;

               end if;

               if Closed_Cell_Pred.Length>0 then


                  for  i in 0..Closed_Cell_Pred.Length-1 loop

                     If  Test_Belongs  (Closed_Cell_Pred.Dynamic_Container(i), E,Abstraction) then

                        null;

                     else

                     Traversal_Complete:=Continue_Abstract_Reachable_Set_Traversal (Closed_Cell_Pred.Dynamic_Container(i),
                                                                                    Closed_Cell,
                                                                                    successor,
                                                                                    Associated_Overapproximations.Dynamic_Container(i),
                                                                                    Test_procedure_not_in_E'Access,
                                                                                    Obstacle_Suboptimal_or_Self_loop'Access
                                                                                   );


                     If Traversal_Complete then

                          If Compare_Cell_to_Succ_plus_Trans_Cost (CLosed_Cell_Pred.Dynamic_Container(i),Closed_Cell,Solution.Value_Function,M) then


                              Set_Cell_Value    (Cell  => CLosed_Cell_Pred.Dynamic_Container(i),
                                                 Value => M,
                                                 W     => Solution.Value_Function);

                              Set_Cell_Control  (cell       => CLosed_Cell_Pred.Dynamic_Container(i),
                                                 v          => iterated_control_symbol,
                                                 Controller => Solution.Controller,
                                                 Abstraction=> Abstraction);



                              Add_to_Heap(CLosed_Cell_Pred.Dynamic_Container(i),Q);



                           end if;






                     elsif    successor/=Cell_Index_T'Last  then


                        Save_As_Predecessor(from              => CLosed_Cell_Pred.Dynamic_Container(i),
                                            overapproximation => Associated_Overapproximations.Dynamic_Container(i),
                                            v                 => iterated_control_symbol,
                                            to                => successor,
                                            Abstraction       => Abstraction);





                     end if;

                  end if;
                  end loop;

               end if;



            end loop;


            Clear_Cell_Data(Cell           => Closed_Cell,
                            Abstraction    => Abstraction);


            For iterated_control_symbol in  u_first..u_last loop             --loop over all control inputs

               Empty_container(Closed_Cell_Pred);

               Backward_Estimator(Closed_Cell_Pred,Closed_Cell, iterated_control_symbol);
--Put_Line(" pred_B: "& Closed_Cell_Pred.Length'img);
               if Closed_Cell_Pred.Length>0 then

                  for  i in 0..Closed_Cell_Pred.Length-1 loop

                     if Is_Index_Cell(Closed_Cell_Pred.Dynamic_Container(i),Abstraction) and not Is_Obstacle(Closed_Cell_Pred.Dynamic_Container(i),Abstraction) then

                        If Check_Mark    (Closed_Cell_Pred.Dynamic_Container(i), iterated_control_symbol,Abstraction)  or
                          Test_Belongs  (Closed_Cell_Pred.Dynamic_Container(i), E,Abstraction) then

                           null;

                        else



                           Set_Mark(Cell        => Closed_Cell_Pred.Dynamic_Container(i),
                                    input       => iterated_control_symbol,
                                    Abstraction => Abstraction);

                           Traversal_Complete := Abstract_Reachable_Set_Traversal(
                                                                                  Closed_Cell_Pred.Dynamic_Container(i),
                                                                                  iterated_control_symbol,
                                                                                  successor,
                                                                                  overapproximation,
                                                                                  Test_procedure_not_in_E'Access,
                                                                                  Obstacle_Suboptimal_or_Self_loop'Access
                                                                                 );



                           If Traversal_Complete then
--Put_Line(" tr comp: ");

                              If Compare_Cell_to_Succ_plus_Trans_Cost (CLosed_Cell_Pred.Dynamic_Container(i),Closed_Cell,Solution.Value_Function,M) then


                                 Set_Cell_Value    (Cell  => CLosed_Cell_Pred.Dynamic_Container(i),
                                                    Value => M,
                                                    W     => Solution.Value_Function);

                                 Set_Cell_Control  (cell       => CLosed_Cell_Pred.Dynamic_Container(i),
                                                    v          => iterated_control_symbol,
                                                    Controller => Solution.Controller,
                                                    Abstraction=> Abstraction);


                                 Add_to_Heap(CLosed_Cell_Pred.Dynamic_Container(i),Q);



                              end if;


                           elsif    successor/=Cell_Index_T'Last  then
--Put_Line(" saving: ");

                              Save_As_Predecessor(from              => CLosed_Cell_Pred.Dynamic_Container(i),
                                                  overapproximation => overapproximation,
                                                  v                 => iterated_control_symbol,
                                                  to                => successor,
                                                  Abstraction       => Abstraction);


                           end if;

                        end if;

                     end if;
                  end loop;

                end if;

            end loop;

         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix) =>
-- Standard Dijkstra-like algorithm to solve Invariance problems
           For iterated_control_symbol in  u_first..u_last loop         

               Empty_container(Closed_Cell_Pred);


               get_predecessors(Closed_Cell,
                                iterated_control_symbol,
                                Abstraction,Closed_Cell_Pred);



               If Closed_Cell_Pred.Length>0 then
                for i in 0..Closed_Cell_Pred.Length-1 loop

                     Read_Cell_Value(Cell  => Closed_Cell_Pred.Dynamic_Container(i),
                                     Value => cell_value,
                                     W     => Solution.Value_Function);


                     If cell_value=Value_T'Last then
                        null;
                     else

                        Exclude_Cell_Control(cell       => Closed_Cell_Pred.Dynamic_Container(i),
                                             v          => iterated_control_symbol,
                                             Controller => Solution.Controller);



                        If Get_num_of_valid_Controls(cell       => Closed_Cell_Pred.Dynamic_Container(i),
                                                     Controller => Solution.Controller) = 0 then

                           Add_to_Heap(CLosed_Cell_Pred.Dynamic_Container(i),Q);

                           Set_Cell_Value(Cell  => Closed_Cell_Pred.Dynamic_Container(i),
                                          Value => Value_T'Last,
                                          W     => Solution.Value_Function);

                        end if;
                     end if;
                  end loop;
               end if;





            end loop;


         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix) =>
--On-the-fly Dijkstra algorithm for solutionof Invariance problems
            symbol_for_all_controls :=Get_Reserved_Control_Symbol_1(Solution.Controller);
            symbol_for_no_controls  :=Get_Reserved_Control_Symbol_2(Solution.Controller);
            number_of_abstract_controls:= Get_Number_Of_Control_Symbols(Abstraction);

            For iterated_control_symbol in  u_first..u_last loop             --loop over all control inputs

               Empty_container(Closed_Cell_Pred);


               Backward_Reachable_Set(Closed_Cell_Pred,Closed_Cell, iterated_control_symbol);


               if Closed_Cell_Pred.Length>0 then

                  for  i in 0..Closed_Cell_Pred.Length-1 loop
                  
                     if Is_index_cell(idx         => Closed_Cell_Pred.Dynamic_Container(i),
                                    Abstraction => Abstraction)  then


                     Read_Cell_Value(Cell  => Closed_Cell_Pred.Dynamic_Container(i),
                                     Value => cell_value,
                                     W     => Solution.Value_Function);





                     if cell_value=Value_T'Last then null;


                     else

                        Read_Cell_Control(cell          => Closed_Cell_Pred.Dynamic_Container(i),
                                          Controller    => Solution.Controller,
                                          Abstraction   => Abstraction,
                                          Control_Input => cell_control);

                        new_cell_control:= cell_control;
                        if cell_control=iterated_control_symbol then

                           overapproximation:= Read_one_Overapproximation(Cell        => Closed_Cell_Pred.Dynamic_Container(i),
                                                                          Abstraction => Abstraction);


                           if Belongs_to_Abstract_Reachable_Set (Closed_Cell,overapproximation) then
                              Traversal_Complete := False;
                              for u in iterated_control_symbol+1..u_last loop
                                 Traversal_Complete := Abstract_Reachable_Set_Traversal(
                                                                                        Closed_Cell_Pred.Dynamic_Container(i),
                                                                                        u,
                                                                                        successor,
                                                                                        overapproximation,
                                                                                        Test_procedure_in_E'Access,
                                                                                        Test_Always_False'Access
                                                                                       );

                                 new_cell_control:=u;
                                 exit when Traversal_Complete;
                              end loop;


                              if Traversal_Complete then

                                 Save_one_Overapproximation(Cell              => Closed_Cell_Pred.Dynamic_Container(i),
                                                            Overapproximation => overapproximation,
                                                            Abstraction       => Abstraction);

                                 Set_Cell_Control(cell       => Closed_Cell_Pred.Dynamic_Container(i),
                                                  v          => new_cell_control,
                                                  Controller => Solution.Controller,
                                                  Abstraction=> Abstraction);

                              else
                                 new_cell_control:= symbol_for_no_controls;
                                 Set_Cell_Control(cell       => Closed_Cell_Pred.Dynamic_Container(i),
                                                  v          => new_cell_control,
                                                  Controller => Solution.Controller,
                                                  Abstraction=> Abstraction);



                              end if;



                           end if;


                        elsif cell_control=symbol_for_all_controls and number_of_abstract_controls>1 then
                              Traversal_Complete:=False;
                           for u in u_first..u_last loop
                              Traversal_Complete := Abstract_Reachable_Set_Traversal(
                                                                                     Closed_Cell_Pred.Dynamic_Container(i),
                                                                                     u,
                                                                                     successor,
                                                                                     overapproximation,
                                                                                     Test_procedure_in_E'Access,
                                                                                     Test_Always_False'Access
                                                                                    );

                              new_cell_control:=u;
                              exit when Traversal_Complete;
                           end loop;


                           if Traversal_Complete then

                              Save_one_Overapproximation(Cell              => Closed_Cell_Pred.Dynamic_Container(i),
                                                         Overapproximation => overapproximation,
                                                         Abstraction       => Abstraction);


                              Set_Cell_Control(cell       => Closed_Cell_Pred.Dynamic_Container(i),
                                               v          => new_cell_control,
                                               Controller => Solution.Controller,
                                                 Abstraction=> Abstraction);

                           else
                              new_cell_control:= symbol_for_no_controls;
                              Set_Cell_Control(cell       => Closed_Cell_Pred.Dynamic_Container(i),
                                               v          => new_cell_control,
                                               Controller => Solution.Controller,
                                                 Abstraction=> Abstraction);



                           end if;





                        end if;
                      if new_cell_control=symbol_for_no_controls then


                        Set_Cell_Value(Cell  => Closed_Cell_Pred.Dynamic_Container(i),
                                       Value => Value_T'Last,
                                       W     => Solution.Value_Function);


                        Add_to_Heap(Cell => Closed_Cell_Pred.Dynamic_Container(i),
                                    H    => Q);
                        end if;
                     end if;
                     end if;
                  end loop;

               end if;

            end loop;

         when others =>


            null;

end case;


 end loop;                                                        --end of search loop




      Put_Line("  Set of settled states size: "& Card(E)'img);

      Finalize_E(E);
      Finalize_Heap(Q);




end Compute;                                                      --end of procedure




----------------------------------------------------------------------------------------------------------------------------------





end dijkstra_algorithm_i13absoc;
