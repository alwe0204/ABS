generic
  type Data_Type is private;
   -- To every grid point a piece of data is attached.
   -- Several grid points may share the same piece of data according to a specifiable partition of the grid

package grids.data is

type Memory_Type is private;

type Grid_Record_With_Data is new Grid_Record with record
 PartitionParameter: Vector_of_Coordinate_Index_Access;
 -- Defines the partition
 Memory            : Memory_Type ;
 -- Data to store about the grid
end record;

procedure init(grid        : out Grid_Record_With_Data;
               subdivision : in Vector_of_Coordinate_Index;
               xmin        : in Vector_Type;
               xmax        : in Vector_Type;
               datadivision: in Vector_of_Coordinate_Index);
-- @description Initializes a grid with data. The input datadivision defines
-- a partition on the grid by grouping Discretization(i) parts per coordinate axis.
-- Assumption_Error is raised if Discretization(i) is not an integer multiple of the
-- grid's Discretization(i)
-- @param grid Grid
-- @param subdivision Subdivision for the partition

procedure set_data(data : in Data_Type;
                   idx  : in Grid_Index_Type;
                   grid : in Grid_Record_With_Data);
-- @description data is attached to the grid point with index idx.
-- Assumptions:
-- - idx is a valid index, i.e., 0 <= idx <= max idx
-- - grid must have been initialized correctly.
-- - data of the grid must have been initialized correctly.
-- @param data The piece of data to attach
-- @param idx The index to attach the data
-- @param grid Grid

function get_data(idx  : in Grid_Index_Type;
                  grid : in Grid_Record_With_Data) return Data_Type;
-- @return The data attached to the grid point with index idx
-- @param idx Index
-- @param grid Grid

private

type Memory_Type_Aux        is array (Grid_Index_Type range <> ) of Data_Type;
type Memory_Type_Aux_Access is access Memory_Type_Aux;

type Memory_Type is record
 Memory : Memory_Type_Aux_Access;
end record;

end grids.data;
