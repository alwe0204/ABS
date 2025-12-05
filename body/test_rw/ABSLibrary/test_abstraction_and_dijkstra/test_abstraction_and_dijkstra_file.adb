with grids;
with Established_Types; use Established_Types;
with cell_cover; use cell_cover;
with Input_Values; use input_values;
with abstraction_i14sym; use abstraction_i14sym;
with abstraction_i14sym.computation; use abstraction_i14sym.computation;
with dijkstra_algorithm_i13absoc;
with controller_i14sym; use controller_i14sym;
with output_results;
with Interfaces.C.Pointers;
with Ada.Integer_Text_IO;           use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;           use Ada.Float_Text_IO;
with Ada.Text_IO;   use Ada.Text_IO;
with value_function; use value_function;

function test_abstraction_and_dijkstra_file return Integer is
   package C renames Interfaces.C;
   
   Abstraction           : abstraction_i14sym.Abstraction_T_Access ;
   AbstractSpecification : abstraction_i14sym.Abstract_Specification_T;


   type Int_array_T is array (Integer range <>) of aliased Integer;  
   
   package Integer_Ptrs is                               --  pointer type needed to read arrays created in c
     new C.Pointers (Index              => Integer,
                     Element            => Integer,
                     Element_Array      => Int_array_T,
                     Default_Terminator => -1);   
   
   use Integer_Ptrs;
   use type Integer_Ptrs.Pointer;
   subtype Integer_Star_T is Integer_Ptrs.Pointer;
  
   
   type Float_array_T is array (Integer range <>) of aliased Float;  
   
   package Float_Ptrs is                               --  pointer type needed to read arrays created in c
     new C.Pointers (Index              => Integer,
                     Element            => Float,
                     Element_Array      => Float_array_T,
                     Default_Terminator => -1.0);   
   
   use Float_Ptrs;
   use type Float_Ptrs.Pointer;
   subtype Float_Star_T is Float_Ptrs.Pointer;
  
   
   
   function Parse_HGraph return Integer;                   -- imported c funtion for reading hypergraph from file
   pragma Import (C, Parse_HGraph, "parse_hgraph");   
    
   function Get_Num_of_Successors(Cell:Integer; v:Integer) return Integer;  -- imported c function for getting number of successors      
   pragma Import (C, get_num_of_successors, "numb_of_succ");
   

      
   
   function Create_Successors(cell:Integer;v:Integer) return Integer_Star_T;  -- imported c function for creating array of successors
   pragma Import (C, Create_Successors, "create_array_of_succ");

   
   procedure Get_Successors_from_File(successors : in out cell_cover.Dynamic_Cell_Container; 
                                      cell       : in cell_cover.Cell_Index_T; 
                                      v          : in input_values.Input_Index_T) is

      use cell_cover.BasicGrid;
      use input_values.InputGrid;
      use type Integer_Ptrs.Pointer;
  
      Num_of_successors: Integer;
      Temp_succ:Integer_Star_T;
      s:cell_cover.Cell_Index_T;

   
   begin
     
      Num_of_successors := Get_Num_of_Successors(Integer(cell),Integer(v));   --read number of successors
      Temp_succ := Create_Successors(Integer(cell),Integer(v));               --create array of successors
   Empty_container(successors);
      for i in  1..Num_of_successors loop 
         s:=cell_cover.Cell_Index_T(Temp_succ.all);
        cell_cover.Save_Cell_To_Container(successors,s);
         Increment(Temp_succ);
--Put(s'img);
      end loop;



   end Get_Successors_from_File;


procedure Lipschitz_Method_backward (successors : in out Dynamic_Cell_Container; 
                               cell       : in Cell_Index_T; 
                               v          : in Input_Index_T) is

   begin
 null;
   end Lipschitz_Method_backward;
 
procedure Lipschitz_Method_backward_with_disturbance (successors : in out Dynamic_Cell_Container; 
                               cell       : in Cell_Index_T; 
                               v          : in Input_Index_T) is

   begin
null;
   end Lipschitz_Method_backward_with_disturbance;
   
 function Abstract_Reachable_Set_Traversal (
                                              cell       : in  Cell_Index_T; 
                                              v          : in Input_Index_T;                                            
                                              successor  : out Cell_Index_T;
                                              overapproximation : out Abstract_Reachable_Set_Geometry_T;
                                              Test_procedure_not_in_E:  access function (cell: in Cell_Index_T; successor : in Cell_Index_T) return Boolean;
                                              Test_procedure_obstacle_suboptimal_or_self_loop:  access function (cell: in Cell_Index_T;successor : in Cell_Index_T) return Boolean
                                             ) return Boolean is
      traversal_complete:Boolean;
      
    begin
     null; 
       traversal_complete:=False;
       successor:=Cell_Index_T'Last;
       overapproximation.Lower:=Cell_Index_T'Last;
       overapproximation.Upper:=Cell_Index_T'Last;
       return traversal_complete;
   
   end Abstract_Reachable_Set_Traversal;
   
   

                                      

   
   
   
   
   function Continue_Abstract_Reachable_Set_Traversal (
                                                       cell                : in Cell_Index_T;
                                                       current_successor   : in  Cell_Index_T;
                                                       next_successor      : out Cell_Index_T;          
                                                       overapproximation   : in Abstract_Reachable_Set_Geometry_T;
                                                       Test_procedure_not_in_E
                                                                       :access function (cell: in Cell_Index_T;successor : in Cell_Index_T) return Boolean;
                                                       Test_procedure_obstacle_suboptimal_or_self_loop
                                                                       :access function (cell: in Cell_Index_T;successor : in Cell_Index_T) return Boolean
                                                      ) return Boolean is
    
      traversal_complete:Boolean;
   begin
   
    null; 
    traversal_complete:=False;
    next_successor:=Cell_Index_T'Last;
    return traversal_complete; 
      
   end Continue_Abstract_Reachable_Set_Traversal;
  
   
   function Belongs_to_Abstract_Reachable_Set (
                                               cell              : in Cell_Index_T;         
                                               overapproximation : in Abstract_Reachable_Set_Geometry_T                                                  
                                              ) return Boolean is
    
      belongs:Boolean;
   begin
   
   null;  
   belongs:=False; 
      return belongs;
   end Belongs_to_Abstract_Reachable_Set;
  


   
   procedure Abstraction_Compute is new Get_All_Transitions(Get_Successors => Get_Successors_from_File); 

   package Controller_Synthesis is new dijkstra_algorithm_i13absoc(Transition_Weight_T => Float_T); 
   
   procedure Solve_Control_Problem is new  Controller_Synthesis.Compute(backward_estimator                        => Lipschitz_Method_backward,
                                                                            Backward_Reachable_Set                    => Lipschitz_Method_backward_with_disturbance,
                                                                            Belongs_to_Abstract_Reachable_Set         => Belongs_to_Abstract_Reachable_Set,
                                                                            Abstract_Reachable_Set_Traversal          => Abstract_Reachable_Set_Traversal,
                                                                            Continue_Abstract_Reachable_Set_Traversal => Continue_Abstract_Reachable_Set_Traversal);
     
     



   Solution : Controller_Synthesis.Abstract_OCP_Solution_Type;                             
   use cell_cover.BasicGrid;
   use input_values.InputGrid;
   m : Counter_T;
 
   
   function Create_Target_Cells return Integer_Star_T;                         --imported function for creating array of target cells
   pragma Import (C, Create_Target_Cells, "create_array_of_target_cells");
   
   -- All cells present in the Controller.dat file are considered initial cells
   
   function Create_Initial_Cells return Integer_Star_T;                        --imported function for creating array of initial cells
   pragma Import (C, Create_Initial_Cells, "create_array_of_initial_cells");

   Temp_target_cells:Integer_Star_T; 
   Temp_initial_cells:Integer_Star_T;  
  
   Value_function_from_file:Float_Star_T;                         --array of values of  cells from the file Controller.dat 
   pragma Import (C, Value_function_from_file, "array_of_values");
   
   Copy1_Value_function_from_file:Float_Star_T;
 
   Num_of_target_cells: Integer;                                  --number of target cells
   pragma Import (C, Num_of_target_cells, "NumOfTargetNodes");
   
   Num_of_initial_cells: Integer;                                 --number of initial cells
   pragma Import (C, Num_of_initial_cells, "NumOfnonTargetNodes");
   
   Num_of_vertices: Integer;                                      --number_of_cells  
   pragma Import (C, Num_of_vertices, "NumOfHVertices");
   
   Num_of_control_inputs: Integer;                                --number of control inputs
   pragma Import (C, Num_of_control_inputs, "NumOfControls");
   
  

   function Get_First_Input_Index return Integer;  -- imported c function for getting first input index     
   pragma Import (C, Get_first_input_index, "get_first_control_symbol");
   
   procedure Free_HGraph ;                       
   pragma Import (C, Free_HGraph, "free_hgraph");
   
   function Is_Control_Symbol_Correct(Index:Integer; v:Integer) return Integer;  
   pragma Import (C, Is_Control_Symbol_Correct, "correctness_of_controller");
   
  use Cell_Cover.BasicGrid;
  use Cell_Cover.CellGrid;
  use Input_Values.InputGrid;

   First_Cell_Index : Cell_Cover.Cell_Index_T ;
   Last_Cell_Index : Cell_Cover.Cell_Index_T ;
   First_Input_Index: Input_Values.Input_Index_T ;
   Last_Input_Index: Input_Values.Input_Index_T ;
   value: Value_T;
   u:Input_Index_T;

begin

   if Parse_HGraph=0 then                           -- If graph is read successfully from file         

      First_Cell_Index :=0;
                                                    --first used cell index
      Last_Cell_Index  := Cell_Cover.Cell_Index_T(Num_of_vertices-1);
                                                    --get last used cell index
      First_Input_Index:=Input_Values.Input_Index_T(Get_First_Input_Index);
                                                    --get first used input index
      Last_Input_Index := Input_Values.Input_Index_T(Num_of_control_inputs-1);
                                                    --get last used input index
    

      
     
   Initialize_Abstraction(Abstraction    => Abstraction,
                          Representation => Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix));

       Install_Discrete(First_Cell_Index,Last_Cell_Index,First_Input_Index,Last_Input_Index,Abstraction.all);

      Temp_initial_cells:=Create_Initial_Cells;
                                                    --read array of initial cells from c array
      AbstractSpecification.InitialCells := new Array_of_Cell_Index_T ( 1 .. Counter_T(Num_of_initial_cells) );

                                                    --create Ada array of initial cells

      if(Num_of_initial_cells<1) then Put("No initial cells have been read"); return 3;
      end if;
      
      for i in  1..Counter_T(Num_of_initial_cells) loop 

                          
         AbstractSpecification.InitialCells(i) := cell_cover.Cell_Index_T(Temp_initial_cells.all);
if Abstraction /=null then        
 Make_Initial(AbstractSpecification.InitialCells(i),Abstraction.all); 
end if;                                        --Copy initial cells to Ada array
         Increment(Temp_initial_cells);
                                                   --Increment pointer
      end loop;


  
      Temp_target_cells:=Create_Target_Cells;
                                                  --read array of target cells from c array

      AbstractSpecification.TargetCells := new Array_of_Cell_Index_T ( 1 ..Counter_T(Num_of_target_cells)  );            
                                                  --create Ada array of target cells

      for i in reverse 1..Counter_T(Num_of_target_cells) loop                            
         AbstractSpecification.TargetCells(i) := cell_cover.Cell_Index_T(Temp_target_cells.all) ;
                                                  --Copy target cells to Ada array
if Abstraction /=null then 
          Make_Target(AbstractSpecification.TargetCells(i),Abstraction.all); 
end if; 
         Increment(Temp_target_cells);
                                                  --Increment pointer
      end loop;

      Abstraction_Compute(Abstraction.All);

      for v in input_values.Input_Index_T range First_Input_Index .. Last_Input_Index loop
         m := abstraction_i14sym.get_number_of_remaining_successors(0,v,Abstraction.All);
    
      end loop;

      for v in input_values.Input_Index_T range First_Input_Index .. Last_Input_Index loop
         for idx in cell_cover.Cell_Index_T range First_Cell_Index .. Last_Cell_Index loop
            declare 
                     Closed_Cell_Pred                     :Dynamic_Cell_Container; 
            begin
              null;

 --    get_predecessors(idx,v,Abstraction.all,Closed_Cell_Pred);

            end;
         end loop;
      end loop;

      Solve_Control_Problem(Solution,Abstraction.all,AbstractSpecification);
   
      if output_results.is_control_problem_solved(AbstractSpecification,Solution.Controller,Abstraction.all) = False then 
         Put("The control problem has not been solved");
         Free_HGraph; 
         return 1;
         --If not all initial cells have control inputs after the run of  the search loop return 1
      end if;
      
      Copy1_Value_function_from_file:=Value_function_from_file;
     
    
      
      for i in 1..Counter_T(Num_of_initial_cells) loop
         Read_Cell_Value(AbstractSpecification.InitialCells(i),value,Solution.Value_Function);
         If(Float(value)/=Copy1_Value_function_from_file.all) then  
         
         New_Line;
         Put("Found value that does not coincide with the one provided in Controller.dat");
         New_Line;

         Free_HGraph;
         return 2;
         --If all initial cells have control inputs assigned to them
         --but their values do not coincide with the ones 
         --provided in the Controller.dat file return 2
         
         end if;
         if(i<Counter_T(Num_of_initial_cells))then Increment(Copy1_Value_function_from_file);

         end if;
      end loop;
      
     
      
      for i in 1..Counter_T(Num_of_initial_cells) loop

            Read_Cell_Control(cell          => AbstractSpecification.InitialCells(i),
                           Controller    => Solution.Controller,
                           Abstraction   => Abstraction.all,
                           Control_Input => u);

         If(Is_Control_Symbol_Correct(Integer(i),Integer(u) )/=0) then           
            New_Line;
            Put("Found control symbol that does not belong to the maximum controller");
            New_Line;   
         --If all initial cells have control inputs assigned to them
         --but these control inputs do not belong to the maximum controller 
         --provided in the Controller.dat file return 4
            return 4;
         end if;
      end loop;
      
      Free_HGraph;
      --If all initial cells have control inputs assigned to them
      --and they correspond to the file Controller.dat return 0
      return 0;
 
   end if;
   --If the file Abstraction.dat has not been opened correctly return 3
   return 3;
   
end test_abstraction_and_dijkstra_file;
