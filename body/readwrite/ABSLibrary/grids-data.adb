package body grids.data is

   svn_author  : constant String := "$Author: lf3eelma $";
   svn_revision: constant String := "$Revision: 2402 $";
   svn_date    : constant String := "$Date: 2021-10-26 11:09:50 +0200 (Di, 26 Okt 2021) $";
-- --------------------------------
-- init ---------------------------
-- --------------------------------

procedure init(grid        : out Grid_Record_With_Data;
               subdivision : in Vector_of_Coordinate_Index;
               xmin        : in Vector_Type;
               xmax        : in Vector_Type;
               datadivision: in Vector_of_Coordinate_Index) is
begin
 if datadivision'Length /= subdivision'Length then
 raise Assumption_Error ;
 end if;
 init(grid,subdivision,xmin,xmax);
 grid.PartitionParameter := new Vector_of_Coordinate_Index (subdivision'Range);
 for i in subdivision'Range loop
  if (grid.Discretization(i) mod subdivision(i)) /= 0 then
  raise Assumption_Error ;
  end if;
  grid.PartitionParameter(i) := datadivision(i);
 end loop;
 grid.memory.memory := new Memory_Type_Aux (0 .. Grid_Index_Type(compute_number_of_grid_points(subdivision))-1);
end init;

-- --------------------------------
-- get_data_idx -------------------
-- --------------------------------

function get_data_idx(idx  : in Grid_Index_Type;
                      grid : in Grid_Record_With_Data) return Grid_Index_Type is
coord : Vector_of_Coordinate_Index (grid.xmin.all'Range);
a : Grid_Index_Type;
begin
 get_coordinate(coord,idx,Grid_Record(grid));
 for i in grid.xmin.all'Range loop
  coord(i) := coord(i) * grid.PartitionParameter(i) / grid.Discretization(i) ;
 end loop;
 a := get_index(coord,grid.PartitionParameter);
 return get_index(coord,grid.PartitionParameter);
end get_data_idx;

-- --------------------------------
-- set_data -----------------------
-- --------------------------------

procedure set_data(data : in Data_Type;
                   idx  : in Grid_Index_Type;
                   grid : in Grid_Record_With_Data) is
data_idx : Grid_Index_Type := get_data_idx(idx,grid);
begin
 grid.memory.memory(data_idx) := data;
end set_data;

-- --------------------------------
-- get_data -----------------------
-- --------------------------------

function get_data(idx  : in Grid_Index_Type;
                  grid : in Grid_Record_With_Data) return Data_Type is
data_idx : Grid_Index_Type := get_data_idx(idx,grid);
begin
 return grid.memory.memory(data_idx);
end get_data;


end grids.data;
