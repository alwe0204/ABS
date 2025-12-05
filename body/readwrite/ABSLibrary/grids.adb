with Ada.Text_IO; use Ada.Text_IO;

package body grids is

svn_author  : constant String := "$Author: lf3eelma $";
svn_revision: constant String := "$Revision: 2085 $";
svn_date    : constant String := "$Date: 2020-07-01 13:42:13 +0200 (Mi, 01 Jul 2020) $";

-- --------------------------------
-- compute_number_of_grid_points --
-- --------------------------------

function compute_number_of_grid_points(subdivision : in Vector_of_Coordinate_Index) return Number_of_grid_points_Type is
 retval : Unsigned_Coordinate_Index_Type := 1 ;
begin
 for i in subdivision'Range loop
  retval := retval * subdivision(i);
  if Number_of_grid_points_Type(retval) >= Number_of_grid_points_Type'Last then raise Constraint_Error;
  end if;
 end loop;
return Number_of_grid_points_Type(retval);
end;


-- --------------------------------
-- get_number_of_grid_points ------
-- --------------------------------

function get_number_of_grid_points(grid : in Grid_Record) return Number_of_grid_points_Type is
begin
 return grid.number_of_grid_points;
end get_number_of_grid_points;

-- --------------------------------
-- init ---------------------------
-- --------------------------------

procedure init(grid        : out Grid_Record; 
               subdivision : in Vector_of_Coordinate_Index; 
               xmin        : in Vector_Type; 
               xmax        : in Vector_Type) is
begin
 if subdivision'Length /= xmin'Length then raise Assumption_Error;
 end if;
 if xmin'Length /= xmax'Length then raise Assumption_Error;
 end if;
-- Allocate the memory associated with the geometry of the grid
 grid.xmin := new Vector_Type (xmin'Range);
 grid.xmax := new Vector_Type (xmax'Range);
 grid.Discretization := new Vector_of_Coordinate_Index (subdivision'Range);
 grid.Parameter      := new Vector_Type (subdivision'Range);
 -- Fill the data associated with the geometry of the grid
 for i in subdivision'Range loop
  grid.Discretization(i) := subdivision(i);
  grid.xmin(i) := xmin(i);
  grid.xmax(i) := xmax(i);
  grid.Parameter(i)  := (xmax(i) - xmin(i)) / Float_Type(subdivision(i));
 end loop;
 grid.number_of_grid_points := compute_number_of_grid_points(grid.Discretization.all);
end init;

-- --------------------------------
-- get_last_used_index ------------
-- --------------------------------

function get_last_used_index(grid : in Grid_Record) return Grid_Index_Type is
begin
 return Grid_Index_Type(grid.number_of_grid_points)-1;
end get_last_used_index;


-- --------------------------------
-- get_index ----------------------
-- --------------------------------

function get_index(coord: in Vector_of_Coordinate_Index; 
                   grid : in Grid_Record) return Grid_Index_Type is
begin
 return get_index(coord,grid.Discretization);
end get_index;

function get_index(coord       : in Vector_of_Coordinate_Index; 
                   subdivision : in Vector_of_Coordinate_Index_Access) return Grid_Index_Type is
      Pragma Suppress(Range_Check);
      Pragma Suppress(Overflow_Check);
      Pragma Suppress(Access_Check);  
 a : Unsigned_Coordinate_Index_Type := 0;
 b : Unsigned_Coordinate_Index_Type := 0;
 c : Unsigned_Coordinate_Index_Type := 1;      
   begin
    
if subdivision=null then raise Assumption_Error; end if;
 
for i in coord'Range loop
 a := coord(i);
    if a>subdivision(i) then raise Constraint_Error; end if;        

  a:=a*c;       
  b := b + a;
  c:=c*subdivision(i);
 end loop;
 return Grid_Index_Type(b);
end get_index;

-- --------------------------------
-- get_coordinate -----------------
-- --------------------------------

procedure get_coordinate(coord : out Vector_of_Coordinate_Index; 
                         idx   : in Grid_Index_Type; 
                         grid  : in Grid_Record) is
div  : Unsigned_Coordinate_Index_Type;
modu : Unsigned_Coordinate_Index_Type;
u_idx : constant Unsigned_Coordinate_Index_Type := Unsigned_Coordinate_Index_Type(idx);
begin
 for i in grid.xmin.all'Range loop
  div := 1;
  for k in 1 .. i-1 loop
   div := div * grid.Discretization(k);
  end loop;
 modu := grid.Discretization(i)*div;
 coord(i) := ((u_idx mod modu) / div);
 end loop;
end get_coordinate;

-- --------------------------------
-- get_gridpoint ------------------
-- --------------------------------

procedure get_gridpoint(point : out Vector_Type; 
                        idx   : in Grid_Index_Type; 
                        grid  : in Grid_Record) is 
coord : Vector_of_Coordinate_Index (grid.xmin.all'Range);
begin
 get_coordinate(coord,idx,grid);
 for i in grid.xmin.all'Range loop
  point(i):= grid.xmin(i) + (Float_Type(coord(i))+0.5)*grid.Parameter(i);
 end loop;
end get_gridpoint;

end grids;
