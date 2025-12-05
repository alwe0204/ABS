with Ada.Containers; use Ada.Containers;
with Interfaces; use Interfaces;
with continuous_problem_types; use continuous_problem_types;
with discrete_problem_types; use discrete_problem_types;
with cell_cover; use cell_cover;

function test_overapproximation return Integer is
 dim : constant Index_of_Dimension := 2;
 Disc : cell_cover.BasicGrid.Vector_of_Coordinate_Index (1 .. dim) := (2,2);
 xmin : Vector (1 .. dim) := (-1.0,-1.0);
 xmax : Vector (1 .. dim) := (1.0,1.0);
 c : Vector (1 .. dim) := (others => 0.0);
 r : Radius (1 .. dim) := (others => 0.0);
 center : aliased Vector := c ;
 rad    : aliased Radius := r ;

 Cont : cell_cover.CellGrid.Cell_Container;


 len : Ada.Containers.Count_Type;
 idx : cell_cover.BasicGrid.Grid_Index_Type;

 Periods   : Periods_Type (1 .. 0);
 NoPeriods : Periods_Type (1 .. 2) := (1,2);

 Periods2   : Periods_Type (1 .. 2) := (1,2);
 NoPeriods2 : Periods_Type (1 .. 0);

 Cover : cell_cover.CellGrid.Grid_on_Manifold_Record;

 use cell_Cover.BasicGrid;

begin

cell_cover.CellGrid.init(grid => Cover,
       subdivision => Disc,
       xmin        => xmin,
       xmax        => xmax,Periods => Periods,NoPeriods => NoPeriods);
center := (0.0,0.0);
rad    := (0.0,0.0);
cell_cover.CellGrid.findintersectingcells(Cont,center'Access,rad'Access,Cover);
len := Cont.Current_Use;
if len /= 4 then return 3;
end if;
for i in 1 .. len loop
 Cont.Dequeue(idx);
 if idx /= Cell_Index_Type(i-1) then return 3;
 end if;
end loop;
center := (0.5,0.0);
rad    := (0.0,0.0);
CellGrid.findintersectingcells(Cont,center'Access,rad'Access,Cover);
len := Cont.Current_Use;
if len /= 2 then return 4;
end if;
Cont.Dequeue(idx);
if idx /= 1 then return 5;
end if;
Cont.Dequeue(idx);
if idx /= 3 then return 6;
end if;
center := (-0.5,0.0);
rad    := (0.0,0.0);
CellGrid.findintersectingcells(Cont,center'Access,rad'Access,Cover);
len := Cont.Current_Use;
if len /= 2 then return 7;
end if;
Cont.Dequeue(idx);
if idx /= 0 then return 8;
end if;
Cont.Dequeue(idx);
if idx /= 2 then return 9;
end if;
center := (-0.5,0.5);
rad    := (0.1,0.1);
CellGrid.findintersectingcells(Cont,center'Access,rad'Access,Cover);
len := Cont.Current_Use;
if len /= 1 then return 10;
end if;
Cont.Dequeue(idx);
if idx /= 2 then return 11;
end if;
center := (0.0,0.0);
rad    := (2.0,2.0);
CellGrid.findintersectingcells(Cont,center'Access,rad'Access,Cover);
len := Cont.Current_Use;
if len /= 1 then return 12;
end if;
Cont.Dequeue(idx);
if idx /= Cell_Index_Type'Last then return 13;
end if;
--
CellGrid.init(grid => Cover,
       subdivision => Disc,
       xmin        => xmin,
       xmax        => xmax,Periods => Periods2,NoPeriods => NoPeriods2);
CellGrid.findintersectingcells(Cont,center'Access,rad'Access,Cover);
len := Cont.Current_Use;
if len /= 4 then return Integer(len);
end if;
for i in 1 .. len loop
Cont.Dequeue(idx);
end loop;
center := (0.9,0.5);
rad    := (0.2,0.0);
CellGrid.findintersectingcells(Cont,center'Access,rad'Access,Cover);
len := Cont.Current_Use;
if len /= 2 then return 14;
end if;
Cont.Dequeue(idx);
if idx /= 3 then return 15;
end if;
Cont.Dequeue(idx);
if idx /= 2 then return 16;
end if;
return 0;
end test_overapproximation ;
