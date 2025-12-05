with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with grids;
with grids.data;
with Established_Types; use Established_Types;

function test_grids return Integer is
   type Status_Type is (Passed,Failed);
   status : Status_Type := Passed;

   package My_Grid is new grids(Float_Type           => Float_T,
                                Dimension_Index_Type => Component_Index_T,
                                Vector_Type          => Vector_Float_T,
                                Vector_Type_Access   => Vector_Float_T_Access);
   use My_Grid;
   
   package My_Grid_Data is new My_Grid.data(Data_Type => Vector_Float_T_Access);
   
   
   GridDisc : My_Grid.Vector_of_Coordinate_Index (1 .. 4); 
   DataDisc : My_Grid.Vector_of_Coordinate_Index (1 .. 4);

   coord : My_Grid.Vector_of_Coordinate_Index (1 .. 4);
   coord2: My_Grid.Vector_of_Coordinate_Index (1 .. 4);

   idx : My_Grid.Grid_Index_Type;
   idx2: My_Grid.Grid_Index_Type;
     
   grid : My_Grid.Grid_Record;
   griddata : My_Grid_Data.Grid_Record_With_Data;
   xmin : Vector_Float_T (1 .. 4);
   xmax : Vector_Float_T (1 .. 4);
   x_Access : Vector_Float_T_Access := new Vector_Float_T (1 .. 4);
   y_Access : Vector_Float_T_Access := new Vector_Float_T (1 .. 4);
   z_Access : Vector_Float_T_Access ;
begin
   xmin(1) := 0.0;
   xmin(2) := 0.0;
   xmin(3) := 0.0;
   xmin(4) := 0.0;

   xmax(1) := 1.0;
   xmax(2) := 1.0;
   xmax(3) := 1.0;
   xmax(4) := 1.0;
   
   x_Access.all := xmax;
   x_Access.all := (others => 0.0);

   GridDisc(1) := 100;
   GridDisc(2) := 211;
   GridDisc(3) := My_Grid.Unsigned_Coordinate_Index_Type(2**8);
   GridDisc(4) := 3;

   coord(1):=73;
   coord(2):=7;
   coord(3):=165;
   coord(4):=1;
   
    
   My_Grid.init(grid        => grid,
                subdivision => GridDisc,
                xmin        => xmin,
                xmax        => xmax);

   idx := My_Grid.get_index(coord => coord ,
                             grid  => grid);
   My_Grid.get_coordinate(coord2,idx,grid);
   for i in coord'Range loop
      if not (coord(i) = coord2(i)) then status := Failed; 
      end if;

   end loop;
   idx2 := 5214463;
   My_Grid.get_coordinate(coord,idx2,grid);
 
   idx := My_Grid.get_index(coord => coord ,
                            grid  => grid);

   if idx /= idx2 then status := Failed; 
   end if;

   if My_Grid.get_number_of_grid_points(grid) /= My_Grid.Number_of_grid_points_Type(GridDisc(1)*GridDisc(2)*GridDisc(3)*GridDisc(4)) 
   then status := Failed; 
   end if;
   
   DataDisc(1) := 10;
   DataDisc(2) := 1;
   DataDisc(3) := 8;
   DataDisc(4) := 3;
   
  
   My_Grid_Data.init(griddata,grid.Discretization.all,grid.xmin.all,grid.xmax.all,DataDisc);
   for i in 0 .. My_Grid.get_last_used_index(My_Grid.Grid_Record(griddata)) loop
    My_Grid_Data.set_data(x_Access,i,griddata);
   end loop;
   My_Grid_Data.set_data(y_Access,idx,griddata);
   z_Access := My_Grid_Data.get_data(idx,griddata);
   if z_Access /= y_Access then status := Failed;
   end if;
   z_Access := My_Grid_Data.get_data(0,griddata);
   if z_Access = y_Access then status := Failed; return 3;
   end if;
 if status = Passed then
   return 0;
 else
  return 255;
 end if;
end test_grids;
