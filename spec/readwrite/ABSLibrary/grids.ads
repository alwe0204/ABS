with Established_Types; use Established_Types;

generic
   type Float_Type is digits <>;
   -- The type to represent real numbers;
   type Dimension_Index_Type is range <>;
   -- The type to enumerate the components of a vector
   type Vector_Type is array (Dimension_Index_Type range <>) of Float_Type;
   -- The type to represent a vector in the real vector space
   type Vector_Type_Access is access Vector_Type;

   
-- @summary A data structure for grids in a real vector space
package grids is

 Assumption_Error : exception;

  
   
 
   Bits: constant := 32;
   
 -- The maximum number of grid points is 2**Bits - 1.
 -- The last index (2**Bits - 1) is reserved for exceptional situations
 type Grid_Index_Type is range 0 ..  2**Bits - 1 ;
 for Grid_Index_Type'Size use Bits; 
 type Number_of_grid_points_Type is range 0 .. 2**Bits - 1;
 type Unsigned_Coordinate_Index_Type is range 0 ..  2**Bits - 1;
 type Vector_of_Coordinate_Index is array (Dimension_Index_Type range <>) of Unsigned_Coordinate_Index_Type;
 type Vector_of_Coordinate_Index_Access is access Vector_of_Coordinate_Index;

 -- A finite grid on a compact hyperinterval of a more dimensional real space.
 type Grid_Record is tagged record   
  Xmin       : Vector_Type_Access;
  -- The lower end point of the hyperinterval
  Xmax       : Vector_Type_Access;
  -- The upper end point of the hyperinterval
  Discretization: Vector_of_Coordinate_Index_Access;
  -- The subdivision coefficients for the grid
  Number_of_grid_points : Number_of_grid_points_Type;
  -- The number of grid points in the grid
  Parameter        : Vector_Type_Access;
  -- Parameter = (xmax - xmin) / Discretization (component-wise)
 end record;

 type Overapproximation_T is record
      Lower : Grid_Index_Type;
      Upper : Grid_Index_Type;     
 end record;
     
   
   
   
   
   
 procedure init(grid        : out Grid_Record; 
                subdivision : in Vector_of_Coordinate_Index; 
                xmin        : in Vector_Type; 
                xmax        : in Vector_Type);
-- @description Creates a grid on the compact hyper-interval H:=[[xmin,xmax]] by subdividing
-- every edge i of H into Discretization(i) equal parts. Assumption_Error is raised if 
-- the dimensions of the input vectors do not coincide

function get_number_of_grid_points(grid : in Grid_Record) return Number_of_grid_points_Type ;
-- @return The number of grid points
-- @param grid Grid
-- Assumptions: grid must have been initialized correctly.

function get_last_used_index(grid : in Grid_Record) return Grid_Index_Type;

procedure get_coordinate(coord : out Vector_of_Coordinate_Index; 
                         idx   : in Grid_Index_Type; 
                         grid  : in Grid_Record);
-- @description Implementation of the (bijective) map idx -> coordinate.
--
-- Assumptions: 
-- - idx is a valid index, i.e., 0 <= idx <= max idx
-- - coord has length that equals the dimension of the real vector space where the grid is defined
-- @param coord Result
-- @param idx   Argument
-- @param grid  Grid

function get_index(coord: in Vector_of_Coordinate_Index; 
                   grid : in Grid_Record) return Grid_Index_Type;
-- @description Implementation of the (bijective) map coordinate -> idx w.r.t. grid .
--
-- Assumptions:
-- - 0 <= coord < grid.Discretization
-- - grid must have been initialized correctly.
-- @return The index to which coord is mapped
-- @param coord The coordinate
-- @param grid 
   
procedure get_gridpoint(point : out Vector_Type; 
                        idx   : in Grid_Index_Type; 
                        grid  : in Grid_Record);
-- @description Implementation of the map idx -> grid point.
--
-- Assumptions: 
-- - idx is a valid index, i.e., 0 <= idx <= max idx
-- - point has length that equals the dimension of the real vector space where the grid is defined
-- @param point Resulting grid point
-- @param idx   Argument
-- @param grid  Grid


private

function get_index(coord       : in Vector_of_Coordinate_Index; 
                   subdivision : in Vector_of_Coordinate_Index_Access) return Grid_Index_Type;
-- @description See public get_index
function compute_number_of_grid_points (subdivision : Vector_of_Coordinate_Index) return Number_of_grid_points_Type;
end grids;
