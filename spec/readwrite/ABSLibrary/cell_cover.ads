with grids;
with grids.intersections;
with Established_Types; use Established_Types;
package Cell_Cover is 

package BasicGrid is new grids(Dimension_Index_Type      => Component_Index_T,
                               Float_Type                => Float_T,
                               Vector_Type               => State_T,
                               Vector_Type_Access        => State_T_Access);
-- Instance of a package for a uniform grid whose points will become the centers of the compact cells of the abstraction
package CellGrid is new BasicGrid.intersections(NonNeg_Float_Type => NonNegative_Float_T,
                                                Radius_Type       => State_Radius_T,
                                                Periods_Type      => List_of_Component_Index_T);
-- Instance of a package for a uniform cell cover to be used for computing the transitions

subtype Cell_Index_T is BasicGrid.Grid_Index_Type;
subtype Vector_of_Coordinates_T is BasicGrid.Vector_of_Coordinate_Index;   
subtype Abstract_Reachable_Set_Geometry_T is BasicGrid.Overapproximation_T;
  
-- The type of the cell index
type Cell_Cover_T is new CellGrid.Grid_on_Manifold_Record with null record;
-- The type of the cell cover
subtype Dynamic_Array_of_Cell_Index_T is CellGrid.Cell_array_Access;
-- Type for an dynamically growing Array holding cell indices.
subtype Dynamic_Cell_Container is CellGrid.Cell_Container;
subtype Dynamic_Overapproximation_Container is CellGrid.Overapproximation_Container;
function Install(P : in Problem_T) return Cell_Cover_T;
-- @return An instance of a uniform cell cover
-- @param P The control problem
-- Assumptions. P has been initialized correctly.
function Get_First_Index(Cover : in Cell_Cover_T) return Cell_Index_T;
-- @return Returns the first index used to identify a cell
function Get_Last_Index(Cover : in Cell_Cover_T) return Cell_Index_T;
-- @return Returns the last index used to identify a cell

procedure Get_Center_Of_Cell(center : in out State_T;
                             idx    : in Cell_Index_T; 
                             Cover  : in Cell_Cover_T);
-- @description center is the center of the cell with index idx
-- @param center The center of the cell
-- @param idx An index of a cell
-- @param Cover A cell cover

function Is_Index_Cell(idx   : in Cell_Index_T; 
                       Cover : in Cell_Cover_T) return Boolean;
-- @return Returns true if idx identifies a cell otherwise returns false
-- @param idx An index
-- @param Cover A cell cover


procedure Save_Cell_To_Container (container : in out Dynamic_Cell_Container; idx : Cell_Index_T);  
-- @param container A cell container
-- @param idx An index of a cell to be saved

function  Is_container_allocated (container : in Dynamic_Cell_Container) return Boolean;  
-- @return True of the container is allocated, False otherwise
-- @param container A cell container
procedure Deallocate_container   (container : in out Dynamic_Cell_Container) ; 
-- @param container A cell container to be deallocated
procedure Empty_container        (container : in out Dynamic_Cell_Container);
-- @param container A cell container to be emptied but NOT deallocated

procedure Deallocate_Dynamic_Array_of_Cell_Index_T (dynamic_cell_array: in out Dynamic_Array_of_Cell_Index_T);
   
   
procedure Save_Overapproximation_To_Container(container : in out Dynamic_Overapproximation_Container; oa : Abstract_Reachable_Set_Geometry_T);  
-- @param container An overapproximation container
-- @param oa an overapproximation to be saved
function  Is_container_allocated (container : in Dynamic_Overapproximation_Container) return Boolean;  
-- @return True of the container is allocated, False otherwise
-- @param container An overapproximation container
procedure Deallocate_container   (container : in out Dynamic_Overapproximation_Container) ;
-- @param container An overapproximation container to be deallocated 
procedure Empty_container        (container : in out Dynamic_Overapproximation_Container);
-- @param container An overapproximation container to be emptied but NOT deallocated
   
end Cell_Cover;
