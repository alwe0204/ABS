package body cell_heap is

   svn_author  : constant String := "$Author: lf3eelma $";
   svn_revision: constant String := "$Revision: 2402 $";
   svn_date    : constant String := "$Date: 2021-10-26 11:09:50 +0200 (Di, 26 Okt 2021) $";

procedure Add_to_Heap      (Cell : in Cell_Index_T; H: in out Cell_Heap_access) is
begin
 H.Cell_Queue.Enqueue(Cell);
end Add_to_Heap;

procedure Initialize_Heap   (H: in out Cell_Heap_access;Abstraction: in Abstraction_T; Abstract_Spec: in Abstract_Specification_T)is
begin
 H:=new Cell_heap_T;
 case Abstraction.Synthesis is
 	when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix)=>
            For i in Abstract_Spec.TargetCells'Range loop
               Add_to_Heap(Abstract_Spec.TargetCells(i),H);
            end loop;
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix)=>
            For i in Abstract_Spec.TargetCells'Range loop
               Add_to_Heap(Abstract_Spec.TargetCells(i),H);
            end loop;
        when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix)=>
            For i in Abstraction_I14sym.Get_First_Index(Abstraction)..Abstraction_I14sym.Get_Last_Used_Index(Abstraction) loop
               IF Is_Obstacle(i,Abstraction) or not Is_Target(i,Abstraction) then
                            Add_to_Heap(i,H);
                  end if;
               end loop;
            Add_to_Heap(Get_Overflow_Index(Abstraction),H);
        when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix)=>
            For i in Abstraction_I14sym.Get_First_Index(Abstraction)..Abstraction_I14sym.Get_Last_Used_Index(Abstraction) loop
               IF Is_Obstacle(i,Abstraction) or not Is_Target(i,Abstraction) then
                            Add_to_Heap(i,H);
                  end if;
               end loop;
         when others =>
            null;
      end case;

end Initialize_Heap;

procedure free_heap_pointer is new Ada.Unchecked_Deallocation(Cell_heap_T,Cell_heap_access);
procedure Finalize_Heap    (H: in out Cell_Heap_access) is
begin
 free_heap_pointer(H);
end Finalize_Heap;

function Remove_min   (H: in out Cell_Heap_access)                         return Cell_Index_T is
  Cell :  Cell_Index_T;
begin  
 H.Cell_Queue.Dequeue(Cell);
return Cell;
end Remove_min;

function Is_Empty     (H: in  Cell_Heap_access)                            return Boolean is
begin
 return H.Cell_Queue.Current_Use=0;
end Is_Empty;

function Current_card (H: in  Cell_Heap_access)    return Count_Type is
begin
      return H.Cell_Queue.Current_Use;
end Current_card;


function Peak_card   (H: in  Cell_Heap_access) return Count_Type is
begin
 return H.Cell_Queue.Peak_Use  ;
end Peak_card ;


end cell_heap;
