with grids;

with continuous_problem_types; use continuous_problem_types;
with discrete_problem_types; use discrete_problem_types;
with problem_type; use problem_type;
with cell_cover; 

with abstraction_i14sym; use abstraction_i14sym;
with abstraction_i14sym.computation; use abstraction_i14sym.computation;



function test_abstraction_i14sym_01 return Integer is

   P : Problem_Specification;
   Abstraction : Abstraction_Type ;
   AbstractSpecification : Abstract_Specification_Type;

   Abstraction_More : Abstraction_Type ;
   AbstractSpecification_More : Abstract_Specification_Type;
   
   Cover : cell_cover.Cell_Cover_Type;
   
   xmin : Vector ( 1 .. 4 );
   xmax : Vector ( 1 .. 4 );
   
   Periods : Periods_Type ( 1 .. 0 );
   NoPeriods : Periods_Type ( 1 .. 4 ) := (1,2,3,4);
   
   num_of_cntr : Control_Index_Type := 5;
   
   procedure get_succ(successors : in out cell_cover.Dynamic_Array_of_Cell_Index; 
                      cell       : in cell_cover.Cell_Index_Type; 
                      v          : in Control_Index_Type) is
   begin
    cell_cover.enqueue(cell,successors);
   end get_succ;
   
   procedure get_succ_more(successors : in out cell_cover.Dynamic_Array_of_Cell_Index; 
                      cell       : in cell_cover.Cell_Index_Type; 
                      v          : in Control_Index_Type) is
    use cell_cover.BasicGrid;
   begin
    cell_cover.enqueue(cell,successors);
    if cell /= 0 then 
    cell_cover.enqueue(0,successors);
    end if;
    if cell /= 1 then
    cell_cover.enqueue(1,successors);
    end if;
   end get_succ_more;
   
   procedure Abstraction_Compute is new compute(get_successors => get_succ);
   
   procedure Abstraction_Compute_More is new compute(get_successors => get_succ_more);
                               

begin

   xmin(1) := 0.0;
   xmin(2) := 0.0;
   xmin(3) := 0.0;
   xmin(4) := 0.0;

   xmax(1) := 1.0;
   xmax(2) := 1.0;
   xmax(3) := 1.0;
   xmax(4) := 1.0;

   P.StateSpaceDimension := 4;
   P.InputSpaceDimension := 1;
   P.Discretization := new Vector_of_Positive ( 1 .. 4 );
   P.xmin           := new Vector ( 1 .. 4 );
   P.xmin.all       := xmin;
   P.xmax           := new Vector ( 1 .. 4 );
   P.xmax.all       := xmax;
   P.Discretization.all := (others => 5);
   P.Periods        := new Periods_Type ( 1 .. 0 );
   P.NoPeriods      := new Periods_Type ( 1 .. 4 );
   P.NoPeriods.all  := NoPeriods;
   P.ControlSignals := new Array_of_Control_Access ( 1 ..  num_of_cntr );
   P.TargetSet      := new Array_of_Hyperinterval_Access (1 .. 1);
   P.TargetSet(1)   := new Vector (1 .. 8) ;
   P.InitialSet     := new Array_of_Hyperinterval_Access (1 .. 1);
   P.InitialSet(1)  := new Vector (1 .. 8) ;
   P.ObstacleSet    := new Array_of_Hyperinterval_Access (1 .. 0);
   P.TargetSet(1)(1) := 0.5;
   P.TargetSet(1)(2) := 0.5;
   P.TargetSet(1)(3) := 0.5;
   P.TargetSet(1)(4) := 0.5;
   P.TargetSet(1)(5) := 0.5;
   P.TargetSet(1)(6) := 0.5;
   P.TargetSet(1)(7) := 0.5;
   P.TargetSet(1)(8) := 0.5;
   P.InitialSet(1)(1) := 0.1;
   P.InitialSet(1)(2) := 0.1;
   P.InitialSet(1)(3) := 0.1;
   P.InitialSet(1)(4) := 0.1;
   P.InitialSet(1)(5) := 0.1;
   P.InitialSet(1)(6) := 0.1;
   P.InitialSet(1)(7) := 0.1;
   P.InitialSet(1)(8) := 0.1;
      
   Cover := cell_cover.install(P);
                 
  Abstraction := install(P);
  if get_number_of_control_symbols(Abstraction) /=  num_of_cntr then return 1;
  end if;
  
  for cell in 0 .. get_last_used_index(Abstraction) loop
   for v in Control_Index_Type range 1 ..  num_of_cntr loop
    if get_number_of_remaining_successors(cell,v,Abstraction) /= 0 then return 2;
    end if;
   end loop;
  end loop;   
  
  Abstraction_Compute(Abstraction,AbstractSpecification,P);
  if AbstractSpecification.TargetCells'Length /= 0 then return 3;
  end if;
  declare 
   Predec : Array_of_Cell_Index := get_predecessors(0,2,Abstraction);
   use cell_cover.BasicGrid;
  begin
   if Predec'Length /= 1 then return 4;
   end if;
   if Predec(1) /= 0 then return 5;
   end if;
  end ;
  Abstraction_More := install(P);
  Abstraction_Compute_More(Abstraction_More,AbstractSpecification_More,P);
  declare 
   Predec : Array_of_Cell_Index := get_predecessors(0,2,Abstraction_More);
   use cell_cover.BasicGrid;
  begin
   if Predec'Length /= get_last_used_index(Abstraction)+1 then return 6;
   end if;
  end ;
return 0;
end test_abstraction_i14sym_01;
