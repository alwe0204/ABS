with abstraction_i14sym.predecessors; use abstraction_i14sym.predecessors;
with Input_Values; use Input_Values;
with Interfaces.C; use Interfaces.C;
with Ada.Containers; use Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;
with System; use System;
with Cell_COver; use Cell_Cover;
package body Abstraction_I14sym.Computation is

svn_author  : constant String := "$Author: lf3eelma $";
svn_revision: constant String := "$Revision: 2395 $";
svn_date    : constant String := "$Date: 2021-10-25 23:29:03 +0200 (Mo, 25 Okt 2021) $";

-- --------------------------------
-- Install ------------------------
-- --------------------------------

procedure Install_Discrete(First_Cell_Index  : in Cell_Index_T ;
                 Last_Cell_Index   : in Cell_Index_T ;
                 First_Input_Index : in Input_Index_T ; 
                 Last_Input_Index  : in Input_Index_T;
                 Abstraction : in out Abstraction_T )  
is
  use Cell_Cover.BasicGrid;
  use Cell_Cover.CellGrid;
  use Input_Values.InputGrid;

  CellData    : Cell_T;
  Cover       : Cell_Cover_T;
  subdivision : BasicGrid.Vector_of_Coordinate_Index (1 .. 1) := (others => BasicGrid.Unsigned_Coordinate_Index_Type((Last_Cell_Index - First_Cell_Index + 1)));
  x : State_T (1 .. 1) := (others => 0.0);
  a : List_of_Component_Index_T ( Component_Index_T(1) .. Component_Index_T(0) );
  b : List_of_Component_Index_T ( Component_Index_T(1) .. Component_Index_T(1) ) := (others => 1);
   begin
      
   if Abstraction.Synthesis/=Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) then raise Assumption_Error;  end if;  
  Abstraction.Number_of_Controls := Input_Index_T(Last_Input_Index - First_Input_Index + 1) ;
  Abstraction.First_Input_Index  := First_Input_Index;
  Abstraction.Last_Input_Index   := Last_Input_Index;
  CellDataGrid.init(grid        => Abstraction.Cells,
                    subdivision => subdivision,
                    xmin        => x,
                    xmax        => x,
                    datadivision=> subdivision);
 
  for idx in First_Cell_Index .. Last_Cell_Index loop
   CellData.counters_rem_successors := new Array_of_Counter_T ( First_Input_Index .. Last_Input_Index );
   CellData.counters_rem_successors.all := (others => 0);
   CellData.Predecessors := initialize_predecessors(Abstraction.Number_of_Controls);
   CellDataGrid.set_data(data => CellData,
                         idx  => idx,
                         grid => Abstraction.Cells);
  end loop;
  
  CellGrid.init(grid        => CellGrid.Grid_on_Manifold_Record(Cover),
                subdivision => subdivision,
                xmin        => x,
                xmax        => x,
                Periods     => a,
                NoPeriods   => b);

  Abstraction.Cover := Cover;
end Install_Discrete;

procedure Install(P : in Problem_T; Abstraction : in out Abstraction_T)  is
 CellData    : Cell_T;
 --     CellData_OTF: Cell_OTF_T; 
      CellData_Invariance : Cell_Invariance_T;
    --  CellData_Invariance_OTF : Cell_Invariance_OTF_T;
 InputData   : constant Input_Grid_T := Install(P);
 subdivision : BasicGrid.Vector_of_Coordinate_Index (P.Initial_State_Space_Subdivision'Range);
 use InputGrid;
 begin
  for i in P.Initial_State_Space_Subdivision'Range loop
   subdivision(i) := BasicGrid.Unsigned_Coordinate_Index_Type(P.Initial_State_Space_Subdivision(i));
  end loop;
  Abstraction.Number_of_Controls := Input_Index_T(InputData.Number_of_grid_points) ;
  Abstraction.First_Input_Index  :=  Get_First_Index(InputData);
  Abstraction.Last_Input_Index   :=  Get_Last_Index(InputData);
  Abstraction.Input_Data         :=  InputData;
      
      case Abstraction.Synthesis is      
      
      when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) =>
         
         CellDataGrid.init(grid        => Abstraction.Cells,
                           subdivision => subdivision,
                           xmin        => P.xmin.all,
                           xmax        => P.xmax.all,
                           datadivision=> subdivision);
      when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix) =>
         
         CellDataGrid_OTF.init(grid        => Abstraction.Cells_otf,
                               subdivision => subdivision,
                               xmin        => P.xmin.all,
                               xmax        => P.xmax.all,
                               datadivision=> subdivision);       
 
      when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix) =>   
         
         CellDataGrid_Invariance.init(grid        => Abstraction.Cells_Invariance,
                                      subdivision => subdivision,
                                      xmin        => P.xmin.all,
                                      xmax        => P.xmax.all,
                                      datadivision=> subdivision);    
         
      when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix) =>    
         
         CellDataGrid_Invariance_OTF.init(grid        => Abstraction.Cells_Invariance_OTF,
                                          subdivision => subdivision,
                                          xmin        => P.xmin.all,
                                          xmax        => P.xmax.all,
                                          datadivision=> subdivision);
         
      when others =>
         null;
      end case;
      
      
      
  Abstraction.Cover := cell_cover.install(P);
  Abstraction.Overflow_Cell:=Cell_Index_T(Abstraction.Cover.Number_of_grid_points);  
      
      for idx in Cell_Cover.Get_First_Index(Abstraction.Cover) .. Cell_Cover.Get_Last_Used_Index(Abstraction.Cover) loop
   
            
            case Abstraction.Synthesis is      
      
            when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) =>    
             
               CellData.counters_rem_successors := new Array_of_Counter_T ( Abstraction.First_Input_Index .. Abstraction.Last_Input_Index );
               CellData.counters_rem_successors.all := (others => 0);
               CellData.Predecessors := Initialize_Predecessors(Abstraction.Number_of_Controls);
               CellDataGrid.set_data(data => CellData,
                                     idx  => idx,
                                     grid => Abstraction.Cells);
  
         
            when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix) =>                
               CellData_Invariance.Predecessors := Initialize_Predecessors(Abstraction.Number_of_Controls);
               CellDataGrid_Invariance.set_data(data => CellData_Invariance,
                                                idx  => idx,
                                                grid => Abstraction.Cells_Invariance);   
               
               
            when others => 
               
               null;  

            end case;            
  
       
         
      end loop;
      
     
      
      case Abstraction.Synthesis is      
      
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) =>    
             
            Abstraction.Overflow_Cell_Data.counters_rem_successors := new Array_of_Counter_T ( Abstraction.First_Input_Index .. Abstraction.Last_Input_Index );
            Abstraction.Overflow_Cell_Data.counters_rem_successors.all := (others => Counter_T'Last);
            Abstraction.Overflow_Cell_Data.Predecessors := Initialize_Predecessors(Abstraction.Number_of_Controls);

  
   
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix) =>    
            Abstraction.Overflow_Cell_Data_Invariance.Predecessors := Initialize_Predecessors(Abstraction.Number_of_Controls);  
         when others => 
               
            null;  

      end case;     
      
end install;




   
   
   
   
   
   
   
   
-- --------------------------------
-- Get_Abstract_Specification -----
-- --------------------------------
 procedure free_temp_array          is new Ada.Unchecked_Deallocation(Array_of_Cell_Index_T,Array_of_Cell_Index_T_Access);

procedure Get_Abstract_Specification(Abstraction : in out Abstraction_T; 
                                     AbstractSpec: in out Abstract_Specification_T; 
                                     P           : in Problem_T) 
            
   is
      
    --  package MemoryPoolfortest is new memory_pool(Data_Type => Integer,Large_Index_Type => Integer); 
    --   Mypool :MemoryPoolfortest.Pool_T; 
      found_cells_container         : Dynamic_Cell_Container;
  --    found_cells: Dynamic_Array_of_Cell_Index_T;
 idx                  : Cell_Index_T;
 temp_array: Array_of_Cell_Index_T_Access;
 dim : constant Component_Index_T := P.State_Space_Dimension;
 
 a : State_T (1 .. dim) := (others => 0.0);
 b : State_T (1 .. dim);
 r_init : State_Radius_T (1 .. dim) := (others => 0.0);
-- c : aliased State_T := a;
-- r : aliased State_Radius_T := r_init;

      D : Cell_T;
      D_OTF : Cell_OTF_T;
      D_Invariance : Cell_Invariance_T;
      D_Invariance_OTF: Cell_Invariance_OTF_T;
 max:Cell_Index_T:=0;
 type set_enum is (initial,target,obstacle);
 -- We have 3 different types of sets
 
 type set_type is array (set_enum) of access Compact_Hyperinterval_T_Array;
 -- All sets are unions of hyper-intervals
 sets : set_type := (P.Initial_State_Set,P.Target_State_Set,P.Obstacle_State_Set);
 -- Load the sets from the problem data structure
   begin
   --   Put_Line("Spec");
 loop_over_the_sets:
 for k in sets'Range loop
 loop_over_the_union_of_hyperintervals:
         
  for i in sets(k)'Range loop
   a := State_T(sets(k)(i)(Lower).all);
   b := State_T(sets(k)(i)(Upper).all);
 --  for j in a'Range loop
 --   c(j) := (b(j)+a(j))/2.0;
--    r(j) := (b(j)-a(j))/2.0;
--   end loop ;
  -- now find the cells
   if k = target then 
    Get_Containing_Cells(found_cells_container,a,b);
   else 
    Get_Intersecting_Cells(found_cells_container,a,b);
   end if;
  end loop loop_over_the_union_of_hyperintervals;
  -- get the found cells and store the respective information
   temp_array:= new Array_of_Cell_Index_T (1 .. Counter_T(found_cells_container.Length));
   
         declare 
            curser : Counter_T := 1;
         begin
  
   
            for i in 0..found_cells_container.Length-1 loop
               idx:=found_cells_container.Dynamic_Container(i);
               if Is_Index_Cell(idx,Abstraction) then
               if Abstraction.Synthesis=Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) then
                  D := CellDataGrid.get_data(idx,Abstraction.Cells);
                  
                  case k is
                  when initial  => D.initial  := True; 
                     temp_array(curser) := idx;
                  when target   => D.target   := True;
                     temp_array(curser) := idx;
                  when obstacle => D.obstacle := True;
                  end case;    
                curser:=curser+1;  
               elsif Abstraction.Synthesis=Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix) then
                  D_OTF := CellDataGrid_OTF.get_data(idx,Abstraction.Cells_otf);
 
               
                  case k is
                  when initial  => D_OTF.initial  := True; 
                     temp_array(curser) := idx;
                  when target   => D_OTF.target   := True;
                     temp_array(curser) := idx;
                  when obstacle => D_OTF.obstacle := True;
                  end case;       
               curser:=curser+1;
               
               elsif Abstraction.Synthesis=Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix) then
                  D_Invariance:=CellDataGrid_Invariance.get_data(idx,Abstraction.Cells_Invariance);
                  case k is
                  when initial  => D_Invariance.initial  := True; 
                     temp_array(curser) := idx;
                  when target   => D_Invariance.target   := True;
                     temp_array(curser) := idx;
                  when obstacle =>D_Invariance.obstacle := True;
                  end case;       
                  curser:=curser+1;
                  
                  
               elsif Abstraction.Synthesis=Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix) then
                  D_Invariance_OTF:=CellDataGrid_Invariance_OTF.get_data(idx,Abstraction.Cells_Invariance_OTF);
                  case k is
                  when initial  => D_Invariance_OTF.initial  := True; 
                     temp_array(curser) := idx;
                  when target   => D_Invariance_OTF.target   := True;
                     temp_array(curser) := idx;
                  when obstacle =>D_Invariance_OTF.obstacle := True;
                  end case;       
                  curser:=curser+1;  
                  
               
               end if;                   
               

 
   
               
               
               if Abstraction.Synthesis=Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) then 
                  CellDataGrid.set_data(D,idx,Abstraction.Cells);
               elsif Abstraction.Synthesis=Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix) then
                  CellDataGrid_OTF.set_data(D_OTF,idx,Abstraction.Cells_otf);
               elsif Abstraction.Synthesis=Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix) then
                  CellDataGrid_Invariance.set_data(D_Invariance,idx,Abstraction.Cells_Invariance);
               elsif Abstraction.Synthesis=Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix) then
                  CellDataGrid_Invariance_OTF.set_data(D_Invariance_OTF,idx,Abstraction.Cells_Invariance_OTF);                  
               end if; 
             end if;  
            end loop;
    
     	case k is
         	when initial => AbstractSpec.InitialCells := new Array_of_Cell_Index_T (1 .. Counter_T(curser-1));
         	when target  => AbstractSpec.TargetCells  := new Array_of_Cell_Index_T (1 .. Counter_T(curser-1));
     	when others  => null;
    	end case;

     for i in 1..curser-1 loop
     
        case k is
                  when initial  => 
                     AbstractSpec.InitialCells(i) := temp_array(i);    
                  when target   => 
                     AbstractSpec.TargetCells(i) := temp_array(i);
                  when others   => null;
                  end case; 
    
       end loop;
  
    
free_temp_array (temp_array);   
         end ;

   
 Empty_container(found_cells_container);        
         
 end loop loop_over_the_sets;
   --   Put_Line("Spec "&max'img&System.address'size'img&System.Word_Size'img);
      
      
      
      
   
      
end Get_Abstract_Specification;

     
-- --------------------------------
-- Get_All_Transitions ------------
-- --------------------------------

procedure Get_All_Transitions(Abstraction : in out Abstraction_T) is 
      successors_container    : Dynamic_Cell_Container;
   --   successors_array:Dynamic_Array_of_Cell_Index_T;
-- target_cells  : Dynamic_Array_of_Cell_Index_T;
-- initial_cells : Dynamic_Array_of_Cell_Index_T;
 D             : Cell_T;
      dequeued_cell : Cell_Index_T; 
 --     start_time,end_time: Time;
  --    is_obst           : Boolean;
 use cell_cover.BasicGrid;
   begin
      
    
      
      for idx in Cell_Cover.Get_First_Index(Abstraction.Cover) .. Cell_Cover.Get_Last_Used_Index(Abstraction.Cover) loop
        
         if Abstraction.Synthesis = Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) then
            D := CellDataGrid.get_data(idx,Abstraction.Cells);
         end if; 
         if Is_Obstacle(idx,Abstraction) then null;
         else 

            for v in Input_Index_T range Get_First_Input_Index(Abstraction) .. Get_Last_Input_Index(Abstraction) loop
               get_successors(successors_container,idx,v);
               if successors_container.Length>0 then
                  for i in 0..successors_container.Length-1 loop
                     dequeued_cell:=successors_container.Dynamic_Container(i);
                 
                     if dequeued_cell /= cell_cover.Cell_Index_T'Last then 
                        Abstraction_I14sym.save_as_predecessor(from        => idx,
                                                               v           => v,
                                                               to          => dequeued_cell,
                                                               Abstraction => Abstraction);
                    
                        
                     else
                           
                        Abstraction_I14sym.predecessors.Save_As_Predecessor_Overflow(from        => idx,
                                                                                     v           => v,
                                                                                     Abstraction => Abstraction);
                        
                    
                     end if;
                    
                     if Abstraction.Synthesis = Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) then  
                        D.counters_rem_successors(v) := D.counters_rem_successors(v) + 1;                  
                     end if;
                     
                  end loop;
               end if;
                  
               Cell_Cover.Empty_Container(successors_container);             
            end loop;
         end if;
    
      
      
     
      
      end loop;
      Cell_Cover.Deallocate_container(successors_container);   
            
  
    
      
      
   
end Get_All_Transitions;

-- --------------------------------
-- Compute ------------------------
-- --------------------------------

procedure Compute(Abstraction           : in out Abstraction_T; 
                  AbstractSpecification : in out Abstract_Specification_T; 
                  P                     : in Problem_T) 
is
-- successors    : Dynamic_Array_of_Cell_Index_T;
-- target_cells  : Dynamic_Array_of_Cell_Index_T;
 --initial_cells : Dynamic_Array_of_Cell_Index_T;
 D             : Cell_T;
 InputData     : Input_Values.Input_Grid_T;
 
 procedure find_intersectingcells(cells  : in out Dynamic_Cell_Container;
                                  lower : State_T;
                                  upper : State_T) 
 is 
 begin
  cell_cover.CellGrid.findintersectingcells(cells => cells,
                                            lower => lower,
                                            upper => upper,
                                            grid  => CellGrid.Grid_on_Manifold_Record(Abstraction.Cover));
 end find_intersectingcells;
 
 procedure Find_Containingcells(cells  : in out Dynamic_Cell_Container;
                                lower  : State_T;
                                upper  : State_T) 
 is
  containing_cells : Boolean        := True;
 begin
  -- the abstract target set must be INCLUDED in the actual one
  Cell_Cover.CellGrid.Findcontainingcells(cells  => cells,
                                          lower  => lower,
                                          upper  => upper,
                                          grid   => CellGrid.Grid_on_Manifold_Record(Abstraction.Cover));
 end Find_Containingcells;

 
 procedure Get_Abstract_Specification_On_Grid is new Get_Abstract_Specification(Get_Intersecting_Cells => Find_Intersectingcells,
                                                                                Get_Containing_Cells   => Find_Containingcells);
 procedure Get_Transitions is new Get_All_Transitions(Get_Successors => Get_Successors);

  use Cell_Cover.BasicGrid;
  use Input_Values.InputGrid;
 begin
  Install(P,Abstraction);
  InputData   := Input_Values.install(P);
  -- Below: Abstract specification
  Get_Abstract_Specification_On_Grid(Abstraction,AbstractSpecification,P);
  -- Below: Computation of the abstraction for standard synthesis method. 
  --        For on-the-fly method abstraction is NOT pre-computed.
      if Abstraction.Synthesis = Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) or 
         Abstraction.Synthesis = Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix) then
         Get_Transitions(Abstraction);      
      end if;
      
end Compute;
   
   
   
   
   

   

   
   
   
   
   
end Abstraction_I14sym.Computation;
