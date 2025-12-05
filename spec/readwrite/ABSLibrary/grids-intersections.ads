with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Unchecked_Deallocation;
generic
 type NonNeg_Float_Type is digits <>;
 -- The type for nonnegative numbers
 type Radius_Type is array (Dimension_Index_Type range <>) of NonNeg_Float_Type;
 -- The type for vectors of nonnegative numbers
 type Periods_Type is array (Dimension_Index_Type range <>) of Dimension_Index_Type;
 -- The type for describing the identified coordinates

-- @summary A grid data structure on a manifold and capabilities to perform intersection tests on the associated cover
-- @description Terminology:
--
-- - A cell refers to a hyper-interval whose center is a grid point and whose radius is half the grid parameter.
-- - Rd stands for rounding
package grids.intersections is

 type Periods_Type_Access is access Periods_Type;
 type Radius_Type_Access is access Radius_Type;
 type Grid_on_Manifold_Record is new Grid_Record with
 record
  NoPeriods         : Periods_Type_Access;
  -- The coordinates i such that Grid.xmin(i) is not identified with Grid.xmax(i)
  Periods           : Periods_Type_Access;
  -- The coordinates i such that Grid.xmin(i) is identified with Grid.xmax(i)
  Bound_on_Rd_Error : Radius_Type_Access;
  -- A bound on the rounding error in the private functions findcell_lower and findcell_upper
 end record;

--package Queue_Interface is new Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Grid_Index_Type);
--package Cells_Container_Pac is new Ada.Containers.Unbounded_Synchronized_Queues (Queue_Interfaces => Queue_Interface);
--subtype Cell_Container is Cells_Container_Pac.Queue;
 
   type Cell_array is array  (Component_Index_T range <> ) of  Grid_Index_Type;
   type Cell_array_Access is access  Cell_array;
   
   type Overapproximation_array is array  (Component_Index_T range <> ) of  Overapproximation_T;
   type Overapproximation_array_Access is access  Overapproximation_array;
   
   type Cell_Container is record
      Dynamic_Container :Cell_array_Access;
      Length: Component_Index_T:=0;
   end record;
  
 
   type Overapproximation_Container is record
      Dynamic_Container :Overapproximation_array_Access;
      Length: Component_Index_T:=0;
   end record;

   
   
   procedure save_index(container : in out Cell_Container; idx : Grid_Index_Type);



   procedure free_Cell_array is new Ada.Unchecked_Deallocation(Cell_array,Cell_array_Access);     
   function Is_container_allocated (container : in Cell_Container) return Boolean;  
   procedure Deallocate_container (container : in out Cell_Container) ;    
   procedure Empty_container (container : in out Cell_Container);
   
   
   procedure save_index(container : in out Overapproximation_Container; oa : Overapproximation_T);
   procedure free_Overapproximation_array is new Ada.Unchecked_Deallocation(Overapproximation_array ,Overapproximation_array_Access);     
   function Is_container_allocated (container : in  Overapproximation_Container) return Boolean;  
   procedure Deallocate_container (container : in out Overapproximation_Container) ;    
   procedure Empty_container (container : in out Overapproximation_Container);
   

   
   
 -- Data structure that grows dynamically to hold cell indices
 procedure init(grid        : in out Grid_on_Manifold_Record;
                subdivision : in Vector_of_Coordinate_Index;
                xmin        : in Vector_Type;
                xmax        : in Vector_Type;
                Periods     : in Periods_Type;
                NoPeriods   : in Periods_Type);
-- @description Creates a grid on the compact hyper-interval H:=[[xmin,xmax]] by subdividing
-- every edge i of H into Discretization(i) equal parts.
-- Additionally, the manifold is specified.
-- Bound_on_Rd_Error is set to 0. Assumption_Error is raised if
-- the dimensions of the input vectors do not coincide
-- @param grid
-- @param subdivision
-- @param xmin The lower end point of the associated hyper-interval
-- @param xmax The upper end point of the associated hyper-interval
-- @param Periods See description of the type of grid
-- @param NoPeriods See description of the type of grid
 procedure init(grid  : in out Grid_on_Manifold_Record;
                bound : access Radius_Type);
-- @description Initializes the bound on the rounding errors
-- @param grid
-- @param bound The bound on the rounding errors
 procedure findintersectingcells(cells  : in out Cell_Container;
                                 center : in Vector_Type ;
                                 rad    : in Radius_Type;
                                 grid   : in Grid_on_Manifold_Record);
 -- @description This procedure computes the cells that intersect the hyper-interval H := [[center-rad,center+rad]]
 -- @param cells The cells that have a non-empty intersection
 -- @param center The center of the hyper-interval
 -- @param rad The radius of the hyper-interval
 -- @param grid
procedure findintersectingcells (cells : in out Cell_Container; 
                                 lower : in Vector_Type ; 
                                 upper : in Vector_Type; 
                                 grid  : in Grid_on_Manifold_Record);
 -- @description This procedure computes the cells that intersect the hyper-interval H := [[lower,upper]]
 -- @param cells The cells that have a non-empty intersection
 -- @param lower The lower point of the hyper interval
 -- @param upper   The upper point of the hyper interval
 -- @param grid

 procedure findcontainingcells (cells  : in out Cell_Container;
                                center : in Vector_Type ;
                                rad    : in Radius_Type;
                                grid   : in Grid_on_Manifold_Record);
 -- @description This procedure computes the cells that are contained in the hyper-interval H := [[center-rad,center+rad]]
 -- @param cells The cells that have a non-empty intersection
 -- @param center The center of the hyper-interval
 -- @param rad The radius of the hyper-interval
 -- @param grid
 procedure findcontainingcells (cells : in out Cell_Container; 
                               lower : in Vector_Type ; 
                               upper : in Vector_Type ; 
                                grid  : in Grid_on_Manifold_Record);
 -- @description This procedure computes the cells that  are contained in the hyper-interval H := [[lower,upper]]
 -- @param cells The cells that have a non-empty intersection
 -- @param lower The lower point of the hyper interval
 -- @param upper   The upper point of the hyper interval
 -- @param grid
   
   
function Default_Boolean_Test_1  (cell: in Grid_Index_Type; successor : in Grid_Index_Type) return Boolean;
function Default_Boolean_Test_2  (cell: in Grid_Index_Type; successor : in Grid_Index_Type) return Boolean;
   
function traverse_overapproximation (
                                      cell : in Grid_Index_Type; 
                                      center : in Vector_Type ; 
                                      rad    : in Radius_Type; 
                                      grid   : in Grid_on_Manifold_Record;
                                      successors : in out Cell_Container;
                                      Boolean_Test_1:   access function (cell: in Grid_Index_Type; successor : in Grid_Index_Type) return Boolean:=Default_Boolean_Test_1'Access;
                                      Boolean_Test_2:   access function (cell: in Grid_Index_Type; successor : in Grid_Index_Type) return Boolean:=Default_Boolean_Test_2'Access
                                     ) return Boolean ;

 -- @description This function traverses the cells that intersect the overapproximation hyper-interval H := [[center-rad,center+rad]]
 -- @param cell The cell from which the overapproximation has been computed
 -- @param center The center of the hyper-interval
 -- @param rad The radius of the hyper-interval
 -- @param grid
--  @param successor list of found cells
--  @param Boolean_Test_1 boolean function to be applied to each traversed cell
--  @param Boolean_Test_2 boolean function to be applied to each traversed cell   


function continue_traverse_overapproximation(
                                                 cell : in Grid_Index_Type;  
                                                 grid   : in Grid_on_Manifold_Record;
                                                 successors : in out Cell_Container;
                                                 lower_index: in Grid_Index_Type;
                                                 upper_index: in Grid_Index_Type;
                                                 start_index: in Grid_Index_Type;  
                                                 Boolean_Test_1:   access function (cell: in Grid_Index_Type; successor : in Grid_Index_Type) return Boolean:=Default_Boolean_Test_1'Access;
                                                 Boolean_Test_2:   access function (cell: in Grid_Index_Type; successor : in Grid_Index_Type) return Boolean:=Default_Boolean_Test_2'Access
                                                ) return Boolean;  
   
 -- @description This function traverses the cells that intersect the overapproximation hyper-interval determined by lower_index and upper_index starting from start_index
 -- @param cell The cell from which the overapproximation has been computed
 -- @param grid
 -- @param successor list of found cells
 -- @param lower_index index of a cell that contains lower point of overapproximation interval
 -- @param upper_index index of a cell that contains upper point of overapproximation interval
 -- @param start_index index of a cell from which the taversal starts
--  @param Boolean_Test_1 boolean function to be applied to each traversed cell
--  @param Boolean_Test_2 boolean function to be applied to each traversed cell   
  
   
function belongs_to_overapproximation(
                                         cell : in Grid_Index_Type;  
                                         grid   : in Grid_on_Manifold_Record;
                                         lower_index: in Grid_Index_Type;
                                         upper_index: in Grid_Index_Type                                                
                                        ) return Boolean;
   
 -- @description This function tests if a cell belongs to a set determined by lower_index, upper_index.
 -- @param cell The cell 
 -- @param grid
 
 private
   type Signed_Coordinate_Index_Type is range -2**Bits .. 2**Bits-1;
 end grids.intersections;
