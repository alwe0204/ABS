with Ada.Text_IO; use Ada.Text_IO;
package body grids.intersections is

svn_author  : constant String := "$Author: lf3eelma $";
svn_revision: constant String := "$Revision: 2395 $";
svn_date    : constant String := "$Date: 2021-10-25 23:29:03 +0200 (Mo, 25 Okt 2021) $";



type Membership_Type is (In_Grid,Not_in_Grid);

type Vector_of_Signed_Coordinate_Index          is array (Dimension_Index_Type range <> ) of Signed_Coordinate_Index_Type;
type Vector_of_Signed_Coordinate_Index_Access   is access Vector_of_Signed_Coordinate_Index;

procedure init(grid        : in out Grid_on_Manifold_Record;
               subdivision : in Vector_of_Coordinate_Index;
               xmin        : in Vector_Type;
               xmax        : in Vector_Type;
               Periods     : in Periods_Type;
               NoPeriods   : in Periods_Type) is
begin
 init(grid,subdivision,xmin,xmax);
 grid.NoPeriods    := new Periods_Type (NoPeriods'Range);
 grid.NoPeriods.all:= NoPeriods;
 grid.Periods      := new Periods_Type (Periods'Range);
 grid.Periods.all  := Periods;
 grid.Bound_on_Rd_Error := new Radius_Type (xmin'Range);
 grid.Bound_on_Rd_Error.all := (others => 0.0);
end init;

procedure init(grid : in out Grid_on_Manifold_Record; bound : access Radius_Type) is
begin
 if grid.Bound_on_Rd_Error /= null then
 grid.Bound_on_Rd_Error.all := bound.all;
 else raise Constraint_Error;
 end if;
end init;

procedure findcell_lower(coord: out Vector_of_Signed_Coordinate_Index; point : in Vector_Type; grid : Grid_on_Manifold_Record)  is
 k : Float_Type;
 begin
  for j in point'Range loop
   k := (point(j)-grid.xmin(j))/grid.Parameter(j) - Float_Type(grid.Bound_on_Rd_Error(j)) ;
   coord(j) := Signed_Coordinate_Index_Type(Float_Type'Ceiling(k)) - 1;
  end loop;
 end findcell_lower;

procedure findcell_upper(coord: out Vector_of_Signed_Coordinate_Index; point : in Vector_Type; grid : Grid_on_Manifold_Record)  is
 k : Float_Type;
 begin
  for j in point'Range loop
   k := (point(j)-grid.xmin(j))/grid.Parameter(j) + Float_Type(grid.Bound_on_Rd_Error(j));
   coord(j) := Signed_Coordinate_Index_Type(Float_Type'Floor(k));
  end loop;
 end findcell_upper;

procedure enumerate(k : in out Vector_of_Signed_Coordinate_Index; tmp :  Vector_of_Signed_Coordinate_Index; i : Dimension_Index_Type )
is
 dim : constant Dimension_Index_Type := k'Length-1;
begin
 k(i) := tmp(i);
 k(i+1) := k(i+1) + 1;
 if i = dim then
  return ;
 end if;
 if k(i+1) > tmp(dim+i+1) then
  enumerate(k,tmp,i+1);
 end if;
end enumerate;
-- @description Counting function for place-value notation tracking on a numberal system

   
   procedure save_index(container : in out Cell_Container; idx : Grid_Index_Type) is
   begin
      
      container.Length:=container.Length+1;
      
      If container.Dynamic_Container=null then
         
         container.Dynamic_Container:= new Cell_array (Component_Index_T(0)..Component_Index_T(0));
         
      end if;
    
      If container.Dynamic_Container'Length < container.Length then   
         declare
           
            New_cell_Array:Cell_array_Access := new Cell_array(Component_Index_T(0)..2*(container.Length-1)-1) ;  
            
         begin
            
            New_cell_Array(0..container.Length-2):=container.Dynamic_Container(0..container.Length-2);
               
            
            free_Cell_array(container.Dynamic_Container);
               
            container.Dynamic_Container:=New_cell_Array;
         end;  
      end if;
      container.Dynamic_Container(container.Length-1):=idx;
      
            
   end save_index;  
 

   
 
   
   function Is_container_allocated (container : in Cell_Container) return Boolean is
      
      
      
   begin
      
      return container.Dynamic_Container/=null;
      
      
      
   end Is_container_allocated;
   
   procedure Deallocate_container (container : in out Cell_Container) is
      
   begin
      
      free_Cell_array(container.Dynamic_Container); 
      container.Length:=0;
      
   end Deallocate_container;

   procedure Empty_container (container : in out Cell_Container) is
      
   begin
      
    
      container.Length:=0;
      
   end Empty_container;
   
  


   procedure save_index(container : in out Overapproximation_Container; oa : Overapproximation_T) is
   begin
      
      container.Length:=container.Length+1;
      
      If container.Dynamic_Container=null then
         
         container.Dynamic_Container:= new Overapproximation_array (Component_Index_T(0)..Component_Index_T(0));
         
      end if;
    
      If container.Dynamic_Container'Length < container.Length then   
         declare
           
            New_overapproximation_Array: Overapproximation_array_Access := new Overapproximation_array(Component_Index_T(0)..2*(container.Length-1)-1) ;  
            
         begin
            
            New_overapproximation_Array(0..container.Length-2):=container.Dynamic_Container(0..container.Length-2);
               
            
            free_Overapproximation_array(container.Dynamic_Container);
               
            container.Dynamic_Container:=New_overapproximation_Array;
         end;  
      end if;
      container.Dynamic_Container(container.Length-1):=oa;
      
            
   end save_index;  
 

   
 
   
   function Is_container_allocated (container : in  Overapproximation_Container) return Boolean is
      
      
      
   begin
      
      return container.Dynamic_Container/=null;
      
      
      
   end Is_container_allocated;
   
   procedure Deallocate_container (container : in out Overapproximation_Container)  is
      
   begin
      
      free_Overapproximation_array(container.Dynamic_Container); 
      container.Length:=0;
      
   end Deallocate_container;

   procedure Empty_container (container : in out Overapproximation_Container)  is
      
   begin
      
    
      container.Length:=0;
      
   end Empty_container;


 
  
   
   function Default_Boolean_Test_1  (cell: in Grid_Index_Type; successor : in Grid_Index_Type) return Boolean is
      
   begin
      
      return False;
      
   end Default_Boolean_Test_1 ;
   
   function Default_Boolean_Test_2  (cell: in Grid_Index_Type;successor : in Grid_Index_Type) return Boolean is
      
   begin
      
      return False;
      
   end Default_Boolean_Test_2 ;
   
   
   
function traverse_hyperrectangle (    
                                      cell  : in Grid_Index_Type;
                                      lower : in Vector_of_Signed_Coordinate_Index ; 
                                      upper : in Vector_of_Signed_Coordinate_Index ; 
                                      start : in Vector_of_Signed_Coordinate_Index ;          
                                      grid   : in Grid_on_Manifold_Record;
                                      successors : in out Cell_Container;
                                      list_successors : Boolean:=True;
                                      Boolean_Test_1:   access function (cell: in Grid_Index_Type;successor : in Grid_Index_Type) return Boolean:=Default_Boolean_Test_1'Access;
                                      Boolean_Test_2:   access function (cell: in Grid_Index_Type;successor : in Grid_Index_Type) return Boolean:=Default_Boolean_Test_2'Access
                                  
                                  )   return Boolean is
      
      dim                : constant Dimension_Index_Type := lower'Length;
      tmp                : aliased Vector_of_Signed_Coordinate_Index (1 .. 2*dim);
      k                  : aliased Vector_of_Signed_Coordinate_Index(1 .. dim+1);
      ks                 : aliased Vector_of_Coordinate_Index(1 .. dim) ;
      idx                : Grid_Index_Type;
begin

   

      
      
      For i in lower'range loop
         tmp(i):=lower(i);
         tmp(i+dim):=upper(i);
       

      end loop;
      
      

 -- Traverse all intersecting cells
      k(1 .. dim) := start(1 .. dim);
      k(dim+1) := 0;
 while k(dim+1) = 0 loop
  for j in lower'Range loop
   -- Only cells within the grid region are indexed, so the "mod" below
   ks(j) := Unsigned_Coordinate_Index_Type(k(j) mod Signed_Coordinate_Index_Type(grid.Discretization(j)));
  end loop;
  -- Compute the index
            idx := get_index(coord => ks,subdivision => Grid.Discretization);
       
            
            if Boolean_Test_1(cell,idx)  then
          --     Put_Line("F test fails");
            save_index(successors,idx);
           
               return False ;
            end if;
            
            
         if Boolean_Test_2(cell,idx) then 
          --        Put_Line("S test fails");
            save_index(successors,Grid_Index_Type'Last);
            return False;
            end if;

            if list_successors then
               save_index(successors,idx);  
               
            end if;
            
            
            
  -- Compute the next coordinate vector
  k(1) := k(1) + 1;
  if k(1) > tmp(1+dim) then
   enumerate(k,tmp,1);
  end if;
 end loop;


 return True;     
end traverse_hyperrectangle;   
   
   

 function traverse_overapproximation (
                                      cell   : in Grid_Index_Type;
                                      center : in Vector_Type ; 
                                      rad    : in Radius_Type; 
                                      grid   : in Grid_on_Manifold_Record;
                                      successors : in out Cell_Container;
                                      Boolean_Test_1:   access function (cell: in Grid_Index_Type; successor : in Grid_Index_Type) return Boolean:=Default_Boolean_Test_1'Access;
                                      Boolean_Test_2:   access function (cell: in Grid_Index_Type; successor : in Grid_Index_Type) return Boolean:=Default_Boolean_Test_2'Access
                                     ) return Boolean is
      
 dim                : constant Dimension_Index_Type := center'Length;
 out_of_grid        : Boolean := False;
 partly_out_of_grid : Boolean := False;


 i                  : Dimension_Index_Type;
 idx                : Grid_Index_Type;


 lower       : Vector_Type(center'Range);
 upper       : Vector_Type(center'Range);
      
      lower_coord :  Vector_of_Signed_Coordinate_Index(1 .. dim) ; 
      upper_coord :  Vector_of_Signed_Coordinate_Index(1 .. dim) ;    
      ks                 : aliased Vector_of_Coordinate_Index(1 .. dim) ;
-- r : Membership_Type;
begin
-- generate the vertices of the hyper-interval

 for i in center'Range loop

   lower(i) := center(i) - Float_Type(rad(i));
   upper(i) := center(i) + Float_Type(rad(i));

 end loop ;

      
      findcell_lower(lower_coord,lower,grid);
      findcell_upper(upper_coord,upper,grid);
      
      
      
-- test the set membership of the vertices w.r.t to the grid
NoPeriods_Loop :
for j in grid.NoPeriods.all'Range loop
i := grid.NoPeriods.all(j);

         
         If lower_coord(i)<0 then
          partly_out_of_grid := True;
            lower_coord(i) := 0; 
         
            if upper_coord(i)<0 then
               out_of_grid:=True;          
            
            
            elsif upper_coord(i)<= Signed_Coordinate_Index_Type(grid.Discretization(i)-1) then
               
                  
              null;
            
            else   
               
          
               upper_coord(i) := Signed_Coordinate_Index_Type(grid.Discretization(i)-1);
               
            end if;
            
        
     
            
         
         elsif lower_coord(i) <= Signed_Coordinate_Index_Type(grid.Discretization(i)-1) then
            
            null;
            
           if upper_coord(i)<= Signed_Coordinate_Index_Type(grid.Discretization(i)-1) then
               
         
               null;
            
            else   
               
               partly_out_of_grid := True;
               upper_coord(i) := Signed_Coordinate_Index_Type(grid.Discretization(i)-1);
               
            end if;
            
         else
            
            out_of_grid:=True;          
                
            
            
            
         end if;
         

      
      exit NoPeriods_Loop when out_of_grid; 
      
end loop NoPeriods_Loop;

      
      
      
      
      
declare
 one_period : Signed_Coordinate_Index_Type;
begin
Periods_Loop :
for j in grid.Periods.all'Range loop
 i := grid.Periods(j);
 one_period := Signed_Coordinate_Index_Type(grid.Discretization(i)-1);
 upper_coord(i) := Signed_Coordinate_Index_Type'Min(upper_coord(i),lower_coord(i)+one_period);
end loop Periods_Loop;
end ;
      

      
      if  out_of_grid or partly_out_of_grid then
         
         save_index(successors,Grid_Index_Type'Last);
         save_index(successors,Grid_Index_Type'Last);
         save_index(successors,Grid_Index_Type'Last);
         return False;
end if;

      
      if  traverse_hyperrectangle(
                                  cell  => cell,
                                  lower => lower_coord,
                                  upper => upper_coord,
                                  start => lower_coord,
                                  grid  => grid,
                                  successors      => successors,
                                  list_successors => False,
                                  Boolean_Test_1  => Boolean_Test_1,
                                  Boolean_Test_2  => Boolean_Test_2) = True then
         
     
         
         save_index(successors,Grid_Index_Type'Last);
         for j in center'Range loop
            -- Only cells within the grid region are indexed, so the "mod" below
            ks(j) := Unsigned_Coordinate_Index_Type(lower_coord(j) mod Signed_Coordinate_Index_Type(grid.Discretization(j)));
 
         end loop; 
         
         idx := get_index(coord => ks,subdivision => Grid.Discretization);
         save_index(successors,idx); 
       
         for j in center'Range loop
            -- Only cells within the grid region are indexed, so the "mod" below
            ks(j) := Unsigned_Coordinate_Index_Type(upper_coord(j) mod Signed_Coordinate_Index_Type(grid.Discretization(j)));
 
         end loop; 
         
         idx := get_index(coord => ks,subdivision => Grid.Discretization);
         save_index(successors,idx);  
         
         
         
         
         
         return True;
      
      else
         
       
         for j in center'Range loop
            -- Only cells within the grid region are indexed, so the "mod" below
            ks(j) := Unsigned_Coordinate_Index_Type(lower_coord(j) mod Signed_Coordinate_Index_Type(grid.Discretization(j)));
 
         end loop; 
         
         idx := get_index(coord => ks,subdivision => Grid.Discretization);
         save_index(successors,idx); 
       
         for j in center'Range loop
            -- Only cells within the grid region are indexed, so the "mod" below
            ks(j) := Unsigned_Coordinate_Index_Type(upper_coord(j) mod Signed_Coordinate_Index_Type(grid.Discretization(j)));
 
         end loop; 
         
         idx := get_index(coord => ks,subdivision => Grid.Discretization);
         save_index(successors,idx);  
         
         return False;   
      
      end if;
      

      
      
end traverse_overapproximation;  
   
   
   
   
procedure findintersectingcells (
                                    cells : in out Cell_Container; 
                                    lower : in Vector_Type ; 
                                    upper : in Vector_Type; 
                                    grid  : in Grid_on_Manifold_Record
                                   ) is
 dim                : constant Dimension_Index_Type := lower'Length;
 out_of_grid        : Boolean := False;
 partly_out_of_grid : Boolean := False;
 cell: Grid_Index_Type:=Grid_Index_Type'Last;

 i                  : Dimension_Index_Type;

 lower_coord :  Vector_of_Signed_Coordinate_Index(1 .. dim) ; 
 upper_coord :  Vector_of_Signed_Coordinate_Index(1 .. dim) ;     
 --r : Membership_Type;
 tmp_bool_var: Boolean;
begin

      
      findcell_lower(lower_coord,lower,grid);
      findcell_upper(upper_coord,upper,grid);
      
      
      
-- test the set membership of the vertices w.r.t to the grid
NoPeriods_Loop :
for j in grid.NoPeriods.all'Range loop
i := grid.NoPeriods.all(j);

         
         If lower_coord(i)<0 then
            partly_out_of_grid := True;
            lower_coord(i) := 0; 
            if upper_coord(i)<0 then
               out_of_grid:=True;          

            elsif upper_coord(i)<= Signed_Coordinate_Index_Type(grid.Discretization(i)-1) then
               null;            
            else   
               upper_coord(i) := Signed_Coordinate_Index_Type(grid.Discretization(i)-1);            
            end if;

         elsif lower_coord(i) <= Signed_Coordinate_Index_Type(grid.Discretization(i)-1) then
            
            null;
            
           if upper_coord(i)<= Signed_Coordinate_Index_Type(grid.Discretization(i)-1) then
               
         
               null;
            
            else   
               
               partly_out_of_grid := True;
               upper_coord(i) := Signed_Coordinate_Index_Type(grid.Discretization(i)-1);
               
            end if;
            
         else
            
            out_of_grid:=True;          
                
            
            
            
         end if;
         

      
      exit NoPeriods_Loop when out_of_grid; 
      
end loop NoPeriods_Loop;

      
      
      
      
      
declare
 one_period : Signed_Coordinate_Index_Type;
begin
Periods_Loop :
for j in grid.Periods.all'Range loop
 i := grid.Periods(j);
 one_period := Signed_Coordinate_Index_Type(grid.Discretization(i)-1);
 upper_coord(i) := Signed_Coordinate_Index_Type'Min(upper_coord(i),lower_coord(i)+one_period);
end loop Periods_Loop;
end ;
      
         
          
  

      
      if  out_of_grid  then      
         save_index(cells,Grid_Index_Type'Last);
         return;  
      end if;   
      
      
      
      tmp_bool_var:= traverse_hyperrectangle(
                                             cell=>cell,
                                             lower => lower_coord,
                                             upper => upper_coord,
                                             start => lower_coord,
                                             grid => grid,
                                             successors => cells,
                                             list_successors => True);  
      
      
      
      if  partly_out_of_grid  then      
         save_index(cells,Grid_Index_Type'Last);
      end if;  
      
      
end findintersectingcells;
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
procedure findintersectingcells (cells  : in out Cell_Container; 
                                 center : in Vector_Type ; 
                                 rad    : in Radius_Type; 
                                 grid   : in Grid_on_Manifold_Record) is
 
  dim                : constant Dimension_Index_Type := center'Length;



      
 lower :  Vector_Type(1 .. dim) ; 
 upper :  Vector_Type(1 .. dim) ;     

      
   begin
      
 for i in center'Range loop

   lower(i) := center(i) - Float_Type(rad(i));
   upper(i) := center(i) + Float_Type(rad(i));

  end loop ;
      
   findintersectingcells(cells => cells,lower => lower,upper => upper,grid => grid);   
      
end findintersectingcells;   
   
   
   
   
procedure findcontainingcells (cells : in out Cell_Container; 
                               lower : in Vector_Type ; 
                               upper : in Vector_Type ; 
                               grid  : in Grid_on_Manifold_Record) is
 dim                : constant Dimension_Index_Type := lower'Length;
 out_of_grid        : Boolean := False;
 partly_out_of_grid : Boolean := False;

cell:Grid_Index_Type:=Grid_Index_Type'last;
 i                  : Dimension_Index_Type;
-- idx                : Grid_Index_Type;
      
 lower_coord :  Vector_of_Signed_Coordinate_Index(1 .. dim) ; 
 upper_coord :  Vector_of_Signed_Coordinate_Index(1 .. dim) ;     
 --r : Membership_Type;
 tmp_bool_var: Boolean;
   begin
      
      
      findcell_upper(lower_coord,lower,grid);
      findcell_lower(upper_coord,upper,grid);
        
      
      
      
      
-- test the set membership of the vertices w.r.t to the grid
NoPeriods_Loop :
for j in grid.NoPeriods.all'Range loop
i := grid.NoPeriods.all(j);

         
         If lower_coord(i)<0 then
            partly_out_of_grid := True;
            lower_coord(i) := 0; 
            if upper_coord(i)<0 then
               out_of_grid:=True;          

            elsif upper_coord(i)<= Signed_Coordinate_Index_Type(grid.Discretization(i)-1) then
               upper_coord(i):=upper_coord(i)-1;            
            else   
               upper_coord(i) := Signed_Coordinate_Index_Type(grid.Discretization(i)-1);            
            end if;

         elsif lower_coord(i) <= Signed_Coordinate_Index_Type(grid.Discretization(i)-1) then
            
            lower_coord(i) := lower_coord(i) +1;
            
           if upper_coord(i)<= Signed_Coordinate_Index_Type(grid.Discretization(i)-1) then
               
         
               upper_coord(i):=upper_coord(i)-1;
            
            else   
               
               partly_out_of_grid := True;
               upper_coord(i) := Signed_Coordinate_Index_Type(grid.Discretization(i)-1);
               
            end if;
            
         else
            
            out_of_grid:=True;          
                   
         end if;
         

      
      exit NoPeriods_Loop when out_of_grid; 
      
end loop NoPeriods_Loop;

      
      
      
      
      
declare
 one_period : Signed_Coordinate_Index_Type;
begin
Periods_Loop :
for j in grid.Periods.all'Range loop
 i := grid.Periods(j);
 one_period := Signed_Coordinate_Index_Type(grid.Discretization(i)-1);
 upper_coord(i) := Signed_Coordinate_Index_Type'Min(upper_coord(i),lower_coord(i)+one_period);
end loop Periods_Loop;
end ;
      
   
   if  out_of_grid  then      
         save_index(cells,Grid_Index_Type'Last);
         return;  
      end if;   
      
      
      
      tmp_bool_var:= traverse_hyperrectangle(
                                             cell  =>   cell,
                                             lower => lower_coord,
                                             upper => upper_coord,
                                             start => lower_coord,
                                             grid => grid,
                                             successors => cells,
                                             list_successors => True);  
      
      
      
      if  partly_out_of_grid  then      
         save_index(cells,Grid_Index_Type'Last);
      end if;      
      
      
      
      
      
      
end findcontainingcells;

  
   
   
procedure findcontainingcells (cells  : in out Cell_Container; 
                                 center : in Vector_Type ; 
                                 rad    : in Radius_Type; 
                                 grid   : in Grid_on_Manifold_Record) is
 
  dim                : constant Dimension_Index_Type := center'Length;



      
 lower :  Vector_Type(1 .. dim) ; 
 upper :  Vector_Type(1 .. dim) ;     

      
   begin
      
 for i in center'Range loop

   lower(i) := center(i) - Float_Type(rad(i));
   upper(i) := center(i) + Float_Type(rad(i));

  end loop ;
      
   findcontainingcells(cells => cells,lower => lower,upper => upper,grid => grid);   
      
end findcontainingcells;     
   
   
   
   
   
   
   
   
   
   
   
   
   
function continue_traverse_overapproximation (
                                                 cell : in Grid_Index_Type; 
                                                 grid   : in Grid_on_Manifold_Record;
                                                  successors : in out Cell_Container;
                                                  lower_index: in Grid_Index_Type;
                                                  upper_index: in Grid_Index_Type;
                                                  start_index: in Grid_Index_Type;  
                                                  Boolean_Test_1:   access function (cell: in Grid_Index_Type; successor : in Grid_Index_Type) return Boolean:=Default_Boolean_Test_1'Access;
                                                  Boolean_Test_2:   access function (cell: in Grid_Index_Type; successor : in Grid_Index_Type) return Boolean:=Default_Boolean_Test_2'Access) return boolean is
      
      
      dim                : constant Dimension_Index_Type :=grid.Xmin'Length;


      i                  : Dimension_Index_Type;

      
      lower_coord :  Vector_of_Coordinate_Index(1 .. dim) ; 
      upper_coord :  Vector_of_Coordinate_Index(1 .. dim) ;    
      start_coord :  Vector_of_Coordinate_Index(1 .. dim) ; 
      
      signed_lower_coord :  Vector_of_Signed_Coordinate_Index(1 .. dim) ; 
      signed_upper_coord :  Vector_of_Signed_Coordinate_Index(1 .. dim) ; 
      signed_start_coord :  Vector_of_Signed_Coordinate_Index(1 .. dim) ; 
      
      period_modif: Signed_Coordinate_Index_Type;
      
      traversal_complete:Boolean;
      
   begin
      
     get_coordinate(coord => lower_coord,idx => lower_index,grid => grid); 
     get_coordinate(coord => upper_coord,idx => upper_index,grid => grid);  
     get_coordinate(coord => start_coord,idx => start_index,grid => grid);  
      
      
      for i in signed_start_coord'Range loop
         
      signed_lower_coord(i):=  Signed_Coordinate_Index_Type(lower_coord(i)); 
      signed_upper_coord(i):=  Signed_Coordinate_Index_Type(upper_coord(i));    
      signed_start_coord(i):=  Signed_Coordinate_Index_Type(start_coord(i));    


      end loop;
      
      for j in grid.Periods.all'Range loop
       
         i := grid.Periods(j);
         period_modif := Signed_Coordinate_Index_Type(grid.Discretization(i));

         while signed_upper_coord(i)<signed_lower_coord(i) loop
         signed_upper_coord(i):=signed_upper_coord(i)+period_modif; 
            
         end loop;
      
         while signed_lower_coord(i)>signed_start_coord(i) loop
          signed_start_coord(i):=signed_start_coord(i)+period_modif;  
            
         end loop;
      
      end loop; 

      
      traversal_complete:= traverse_hyperrectangle(
                                                   cell  => cell,
                                                   lower => signed_lower_coord,
                                                   upper => signed_upper_coord,
                                                   start => signed_start_coord,
                                                   grid => grid,
                                                   successors => successors,
                                                   list_successors => False,
                                                   Boolean_Test_1 => Boolean_Test_1,
                                                   Boolean_Test_2 => Boolean_Test_2
                                                  ) ;  
      

                
      
      If traversal_complete then return True; else return False; end if;
      
      
      
   end continue_traverse_overapproximation ;
   
   
   function belongs_to_overapproximation(
                                         cell : in Grid_Index_Type;  
                                         grid   : in Grid_on_Manifold_Record;
                                         lower_index: in Grid_Index_Type;
                                         upper_index: in Grid_Index_Type                                                
                                        ) return Boolean is
      
      
      dim                : constant Dimension_Index_Type :=grid.Xmin'Length;


      i                  : Dimension_Index_Type;

      
      lower_coord :  Vector_of_Coordinate_Index(1 .. dim) ; 
      upper_coord :  Vector_of_Coordinate_Index(1 .. dim) ;    
      start_coord :  Vector_of_Coordinate_Index(1 .. dim) ; 
      
      signed_lower_coord :  Vector_of_Signed_Coordinate_Index(1 .. dim) ; 
      signed_upper_coord :  Vector_of_Signed_Coordinate_Index(1 .. dim) ; 
      signed_start_coord :  Vector_of_Signed_Coordinate_Index(1 .. dim) ; 
      
      period_modif: Signed_Coordinate_Index_Type;
   
   
   begin
      
    
      
         
      get_coordinate(coord => lower_coord,idx => lower_index,grid => grid); 
      get_coordinate(coord => upper_coord,idx => upper_index,grid => grid);  
      get_coordinate(coord => start_coord,idx => cell,grid => grid);  
      
      
      for i in signed_start_coord'Range loop
         
         signed_lower_coord(i):=  Signed_Coordinate_Index_Type(lower_coord(i)); 
         signed_upper_coord(i):=  Signed_Coordinate_Index_Type(upper_coord(i));    
         signed_start_coord(i):=  Signed_Coordinate_Index_Type(start_coord(i));    


      end loop;
      
      for j in grid.Periods.all'Range loop
       
         i := grid.Periods(j);
         period_modif := Signed_Coordinate_Index_Type(grid.Discretization(i));

         while signed_upper_coord(i)<signed_lower_coord(i) loop
            signed_upper_coord(i):=signed_upper_coord(i)+period_modif; 
            
         end loop;
      
         while signed_lower_coord(i)>signed_start_coord(i) loop
            signed_start_coord(i):=signed_start_coord(i)+period_modif;  
            
         end loop;
      
      end loop; 
      
      for i in signed_start_coord'Range loop
         
         if signed_start_coord(i)<signed_lower_coord(i) or signed_start_coord(i)>signed_upper_coord(i) then
            
           return false;
         end if;
         
       

      end loop;
      
      
      return True; 
      
   end belongs_to_overapproximation;
   
   
end grids.intersections;
