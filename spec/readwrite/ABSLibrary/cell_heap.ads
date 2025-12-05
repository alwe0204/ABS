with Established_Types; use Established_Types;
with Input_Values; use Input_Values;
with Cell_Cover; use Cell_Cover;
with Grids.Data;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Containers.Hashed_Sets;
use Ada.Containers;
with Abstraction_I14sym; use Abstraction_I14sym;
with Ada.Calendar; use Ada.Calendar;


package cell_heap is


type Cell_Heap_T is limited private;
type Cell_Heap_access is access Cell_Heap_T;


package Cell_Interface is
 new Ada.Containers.Synchronized_Queue_Interfaces(Element_Type=>Cell_Index_T);

package Unbounded_Synchronized_Cell_Queue_Package is
  new Ada.Containers.Unbounded_Synchronized_Queues(Queue_Interfaces=>Cell_Interface);





procedure Add_to_Heap      (Cell : in Cell_Index_T; H: in out Cell_Heap_access);
-- @param cell index of a cell to be added to heap
-- @param H cell heap   

procedure Initialize_Heap  (H: in out Cell_Heap_access;Abstraction: in Abstraction_T; Abstract_Spec: in Abstract_Specification_T);
-- @param H cell heap   

procedure Finalize_Heap    (H: in out Cell_Heap_access);
-- @param H cell heap 

function Remove_min   (H: in out Cell_Heap_access)                         return Cell_Index_T;
-- @return cell at the top of the heap
-- @param H cell heap
function Is_Empty     (H: in  Cell_Heap_access)                            return Boolean;
-- @return True if the heap is empty, False otherwise
-- @param  H cell heap

function Current_card (H: in  Cell_Heap_access)    return Count_Type;
-- @return number of cells in the heap
-- @param  H cell heap

function Peak_card    (H: in  Cell_Heap_access) return Count_Type;
-- @return max number that the heap contained at some point in time
-- @param  H cell heap

private

type Cell_heap_T is record
  Cell_Queue: Unbounded_Synchronized_Cell_Queue_Package.Queue;
end record;

end cell_heap;
