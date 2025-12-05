with grids;

with Established_Types; use Established_Types;
with cell_cover;  use cell_cover;
with Input_Values; use Input_values;

with abstraction_i14sym; use abstraction_i14sym;
with abstraction_i14sym.computation; use abstraction_i14sym.computation;

with dijkstra_algorithm_i13absoc;
with output_results;



function test_abstraction_and_dijkstra return Integer is

   dim                   : constant Component_Index_T := 2;
   P                     : Problem_T;
   Abstraction           : Abstraction_T_Access ;
   AbstractSpecification : Abstract_Specification_T;
   Cover                 : cell_cover.Cell_Cover_T;
  
   xmin : State_T ( 1 .. dim );
   xmax : State_T ( 1 .. dim );

   umin : Input_T ( 1 .. 1 );
   umax : Input_T ( 1 .. 1 );
   
   Periods   : List_of_Component_Index_T ( 1 .. 0 );
   NoPeriods : List_of_Component_Index_T ( 1 .. dim ) := (1,2);
   
   
   procedure get_succ(successors : in out cell_cover.Dynamic_Cell_Container; 
                      cell       : in cell_cover.Cell_Index_T; 
                      v          : in input_values.Input_Index_T) is 
    use cell_cover.BasicGrid;
    use input_values.InputGrid;
   begin
    if cell = 0 and then v = 0 then
    cell_cover.Save_Cell_To_Container(successors,1);
    cell_cover.Save_Cell_To_Container(successors,2);
    elsif cell = 1 and then v = 0 then 
    cell_cover.Save_Cell_To_Container(successors,3);
    cell_cover.Save_Cell_To_Container(successors,4);
    elsif cell = 2 and then v = 1 then
    cell_cover.Save_Cell_To_Container(successors,5);
    elsif cell = 3 and then v = 1 then
    cell_cover.Save_Cell_To_Container(successors,15);
    elsif cell = 4 and then v = 0 then
    cell_cover.Save_Cell_To_Container(successors,6);
    elsif cell = 5 and then v = 1 then
    cell_cover.Save_Cell_To_Container(successors,3);
    cell_cover.Save_Cell_To_Container(successors,15);
    elsif cell = 6 and then v = 1 then 
    cell_cover.Save_Cell_To_Container(successors,15); 
    elsif cell = 6 and then v = 0 then 
    cell_cover.Save_Cell_To_Container(successors,7);
    else 
    cell_cover.Save_Cell_To_Container(successors,cell_cover.Cell_Index_T'Last); 
    end if;
   end get_succ;

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
   
   procedure Abstraction_Compute is new compute(get_successors => get_succ);  

   package Controller_Synthesis is new dijkstra_algorithm_i13absoc(Transition_Weight_T => Float_T); 

   procedure Solve_Control_Problem is new  Controller_Synthesis.Compute(backward_estimator                        => Lipschitz_Method_backward,
                                                                            Backward_Reachable_Set                    => Lipschitz_Method_backward_with_disturbance,
                                                                            Belongs_to_Abstract_Reachable_Set         => Belongs_to_Abstract_Reachable_Set,
                                                                            Abstract_Reachable_Set_Traversal          => Abstract_Reachable_Set_Traversal,
                                                                            Continue_Abstract_Reachable_Set_Traversal => Continue_Abstract_Reachable_Set_Traversal);
     
     


   -- Definition of the controller synthesis method
   Solution : Controller_Synthesis.Abstract_OCP_Solution_Type;                             
   use cell_cover.BasicGrid;
   use input_values.InputGrid;
   m : Counter_T;
begin

   xmin(1) := 0.0;
   xmin(2) := 0.0;
  
   xmax(1) := 1.0;
   xmax(2) := 1.0;

   umin(1) := 0.0;
   umax(1) := 1.0;
  
   P.State_Space_Dimension := dim;
   P.Input_Space_Dimension := 1;
   P.Initial_State_Space_Subdivision := new State_Space_Subdivision_T ( 1 .. dim );
   P.Initial_Input_Space_Subdivision := new Input_Space_Subdivision_T ( 1 .. 1 );
   P.xmin           := new State_T ( 1 .. dim );
   P.xmin.all       := xmin;
   P.xmax           := new State_T ( 1 .. dim );
   P.xmax.all       := xmax;

   P.umin           := new Input_T ( 1 .. 1 );
   P.umin.all       := umin;
   P.umax           := new Input_T ( 1 .. 1 );
   P.umax.all       := umax;

   P.Initial_State_Space_Subdivision.all := (others => 4);
   P.Initial_Input_Space_Subdivision.all := (others => 2);
   P.List_of_periodic_components        := new List_of_Component_Index_T ( 1 .. 0 );
   P.List_of_nonperiodic_components      := new List_of_Component_Index_T ( 1 .. dim );
   P.List_of_nonperiodic_components.all  := NoPeriods;
   P.Bounds_of_Overapproximation_Rounding_Error := new Bounds_of_Numerical_Errors_State_T ( 1 .. dim );
   P.Bounds_of_Overapproximation_Rounding_Error.all := (others => 0.0);
  
   P.Target_State_Set           := new Compact_Hyperinterval_T_Array (1 .. 1);
   P.Target_State_Set(1)(Lower) := new Vector_Float_T (1 .. dim) ;
   P.Target_State_Set(1)(Upper) := new Vector_Float_T (1 .. dim) ;
   P.Initial_State_Set     := new Compact_Hyperinterval_T_Array (1 .. 1);
   P.Initial_State_Set(1)(Lower)  := new Vector_Float_T (1 .. dim) ;
   P.Initial_State_Set(1)(Upper)  := new Vector_Float_T (1 .. dim) ;
   P.Obstacle_State_Set    := new Compact_Hyperinterval_T_Array (1 .. 0);
   P.Target_State_Set(1)(Lower)(1) := 0.74;
   P.Target_State_Set(1)(Lower)(2) := 0.74;
   P.Target_State_Set(1)(Upper)(1) := 1.1;
   P.Target_State_Set(1)(Upper)(2) := 1.1;
   
   P.Initial_State_Set(1)(Lower)(1) := 0.1;
   P.Initial_State_Set(1)(Lower)(2) := 0.1;
   P.Initial_State_Set(1)(Upper)(1) := 0.1;
   P.Initial_State_Set(1)(Upper)(2) := 0.1;
      
   Cover := cell_cover.Install(P);
                 
   Initialize_Abstraction(Abstraction    => Abstraction,
                          Representation => Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix));
   
   Install(P,Abstraction.all);    
  Abstraction_Compute(Abstraction.all,AbstractSpecification,P);

  if AbstractSpecification.InitialCells'Length /= 1 then return 2;
  end if;
  if AbstractSpecification.TargetCells'Length /= 1 then return 3;
  end if;
  if AbstractSpecification.TargetCells(1) /= 15 then return 4;
  end if;
  if AbstractSpecification.InitialCells(1) /= 0 then return 5;
  end if;

  for v in input_values.Input_Index_T range 0 .. 1 loop
   m := abstraction_i14sym.get_number_of_remaining_successors(0,v,Abstraction.all);
  end loop;
  for v in input_values.Input_Index_T range 0 .. 1 loop
   for idx in cell_cover.Cell_Index_T range 0 .. 15 loop
   declare 
       Closed_Cell_Pred                     :Dynamic_Cell_Container; 
    begin
     Empty_container(Closed_Cell_Pred);
     get_predecessors(idx,v,Abstraction.all,Closed_Cell_Pred);
    end;
   end loop;
  end loop;
 
Solve_Control_Problem(Solution,Abstraction.all,AbstractSpecification);
if output_results.is_control_problem_solved(AbstractSpecification,Solution.Controller,Abstraction.all) = False then return 6;
end if;
return 0;
end test_abstraction_and_dijkstra;
