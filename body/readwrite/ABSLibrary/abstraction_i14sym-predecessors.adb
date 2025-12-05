with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
package body Abstraction_I14sym.Predecessors is

svn_author  : constant String := "$Author: lf3eelma $";
svn_revision: constant String := "$Revision: 2085 $";
svn_date    : constant String := "$Date: 2020-07-01 13:42:13 +0200 (Mi, 01 Jul 2020) $";

allocation_constant : Counter_T:=1;
 Assumption_Error : exception;

-- --------------------------------
-- Initialize_Predecessors --------
-- --------------------------------

function Initialize_Predecessors(Number_of_Controls : in Input_Index_T) return Predecessors_T is
 retval : Predecessors_T;
 use InputGrid;
begin
 retval.data := null;
 retval.idata := new idata_T (0 .. Number_of_Controls);
for v in Input_Index_T range 0 .. Number_of_Controls loop
 retval.idata(v).Pos_of_first_predecessor := Counter_T(v)*allocation_constant;
 retval.idata(v).number_of_predecessors := 0;
end loop;         
return retval;
end Initialize_Predecessors;

-- --------------------------------
-- Save_As_Predecessor ------------
-- --------------------------------
   procedure free_data           is new Ada.Unchecked_Deallocation(Array_of_Cell_Index_T,Array_of_Cell_Index_T_Access);
   procedure free_data_OTF       is new Ada.Unchecked_Deallocation(Array_of_Cell_and_Overapproximation_T,Array_of_Cell_and_Overapproximation_T_Access);
   procedure free_idata          is new Ada.Unchecked_Deallocation(idata_T,idata_T_Access);
   procedure free_pred_record    is new Ada.Unchecked_Deallocation(Predecessors_OTF_T,Predecessors_OTF_T_Access);
   procedure free_Boolean_marks  is new Ada.Unchecked_Deallocation(Array_of_Boolean_Marks,Array_of_Boolean_Marks_Access);
   
   procedure Save_As_Predecessor(from        : in Cell_Index_T; 
                                 v           : in Input_Index_T; 
                                 to          : in Cell_Index_T; 
                                 Abstraction : in out Abstraction_T) 
   is
       
       
      Data_of_to           : Cell_T; 
      Data_of_to_Invariance : Cell_Invariance_T;
      Predecessors_of_to : Predecessors_T;
      ofs,num,curser     : Counter_T;
      M                  : constant Counter_T := Counter_T(Get_Number_Of_Control_Symbols(Abstraction));
   
      
    
      use cell_cover.BasicGrid;
      use Input_Values.InputGrid;
  
   
   
   
   begin
   
      case Abstraction.Synthesis is
         
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix)=>
         
            Data_of_to  := CellDataGrid.get_data(to,Abstraction.Cells);
   
            Predecessors_of_to  := Data_of_to.Predecessors;
      
    
         
            if Predecessors_of_to.data = null then
               Predecessors_of_to.data := new Array_of_Cell_Index_T (0 .. M - 1);
            end if;
        
      
            ofs := Predecessors_of_to.idata(v).Pos_of_first_predecessor;
            num := Predecessors_of_to.idata(v).number_of_predecessors;
            curser := num + ofs;  
            if curser = Predecessors_of_to.idata(Input_Index_T'Succ(v)).Pos_of_first_predecessor then
               declare
                  New_Array : Array_of_Cell_Index_T_Access;
                  array_extention: Counter_T:=num;
               begin
                  New_Array := new Array_of_Cell_Index_T (0 .. Predecessors_of_to.data'Length - 1 + array_extention);
                  New_Array(0 .. curser-1) := Predecessors_of_to.data(0 .. curser-1); 
                  New_Array(curser+array_extention .. New_Array'Length - 1)  := Predecessors_of_to.data(curser .. Predecessors_of_to.data'Length - 1);
                  free_data(Predecessors_of_to.data);
                  Predecessors_of_to.data := New_Array;
                  for vv in Input_Index_T range v+1 .. Predecessors_of_to.idata'Last loop
                     Predecessors_of_to.idata(vv).Pos_of_first_predecessor := Predecessors_of_to.idata(vv).Pos_of_first_predecessor + array_extention;
                  end loop;
               end ;
            end if;
            Predecessors_of_to.data(curser) := from;
            Predecessors_of_to.idata(v).number_of_predecessors := Predecessors_of_to.idata(v).number_of_predecessors + 1;
 

         
            Data_of_to.Predecessors := Predecessors_of_to;
            CellDataGrid.set_data(Data_of_to,to,Abstraction.Cells);
    
      
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix)=> 
            
            Data_of_to_Invariance  := CellDataGrid_Invariance.get_data(to,Abstraction.Cells_Invariance);
   
            Predecessors_of_to  := Data_of_to_Invariance.Predecessors;
      
    
         
            if Predecessors_of_to.data = null then
               Predecessors_of_to.data := new Array_of_Cell_Index_T (0 .. M - 1);
            end if;
        
      
            ofs := Predecessors_of_to.idata(v).Pos_of_first_predecessor;
            num := Predecessors_of_to.idata(v).number_of_predecessors;
            curser := num + ofs;  
            if curser = Predecessors_of_to.idata(Input_Index_T'Succ(v)).Pos_of_first_predecessor then
               declare
                  New_Array : Array_of_Cell_Index_T_Access;
                  array_extention: Counter_T:=num;
               begin
                  New_Array := new Array_of_Cell_Index_T (0 .. Predecessors_of_to.data'Length - 1 + array_extention);
                  New_Array(0 .. curser-1) := Predecessors_of_to.data(0 .. curser-1); 
                  New_Array(curser+array_extention .. New_Array'Length - 1)  := Predecessors_of_to.data(curser .. Predecessors_of_to.data'Length - 1);
                  free_data(Predecessors_of_to.data);
                  Predecessors_of_to.data := New_Array;
                  for vv in Input_Index_T range v+1 .. Predecessors_of_to.idata'Last loop
                     Predecessors_of_to.idata(vv).Pos_of_first_predecessor := Predecessors_of_to.idata(vv).Pos_of_first_predecessor + array_extention;
                  end loop;
               end ;
            end if;
            Predecessors_of_to.data(curser) := from;
            Predecessors_of_to.idata(v).number_of_predecessors := Predecessors_of_to.idata(v).number_of_predecessors + 1;
 

         
            Data_of_to_Invariance.Predecessors := Predecessors_of_to;
            CellDataGrid_Invariance.set_data(Data_of_to_Invariance,to,Abstraction.Cells_Invariance);
            
         when others => null;
            
      end case;

  

      
   end save_as_predecessor ;

   procedure Save_As_Predecessor_Overflow(from        : in Cell_Index_T; 
                                          v           : in Input_Index_T; 
                                          Abstraction : in out Abstraction_T) 
   is
       
       
      Data_of_Overflow         : Cell_T; 
      Data_of_Overflow_Invariance: Cell_Invariance_T;
      
      Predecessors_of_to : Predecessors_T;
      ofs,num,curser     : Counter_T;
      M                  : constant Counter_T := Counter_T(Get_Number_Of_Control_Symbols(Abstraction));
   
      
    
      use cell_cover.BasicGrid;
      use Input_Values.InputGrid;
  
   
   
   
   begin
   
      case Abstraction.Synthesis is
         
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix)=>
         
            Data_of_Overflow  := Abstraction.Overflow_Cell_Data;
   
            Predecessors_of_to  := Data_of_Overflow.Predecessors;
      
    
         
            if Predecessors_of_to.data = null then
               Predecessors_of_to.data := new Array_of_Cell_Index_T (0 .. M - 1);
            end if;
        
      
            ofs := Predecessors_of_to.idata(v).Pos_of_first_predecessor;
            num := Predecessors_of_to.idata(v).number_of_predecessors;
            curser := num + ofs;  
            if curser = Predecessors_of_to.idata(Input_Index_T'Succ(v)).Pos_of_first_predecessor then
               declare
                  New_Array : Array_of_Cell_Index_T_Access;
                  array_extention: Counter_T:=num;
               begin
                  New_Array := new Array_of_Cell_Index_T (0 .. Predecessors_of_to.data'Length - 1 + array_extention);
                  New_Array(0 .. curser-1) := Predecessors_of_to.data(0 .. curser-1); 
                  New_Array(curser+array_extention .. New_Array'Length - 1)  := Predecessors_of_to.data(curser .. Predecessors_of_to.data'Length - 1);
                  free_data(Predecessors_of_to.data);
                  Predecessors_of_to.data := New_Array;
                  for vv in Input_Index_T range v+1 .. Predecessors_of_to.idata'Last loop
                     Predecessors_of_to.idata(vv).Pos_of_first_predecessor := Predecessors_of_to.idata(vv).Pos_of_first_predecessor + array_extention;
                  end loop;
               end ;
            end if;
            Predecessors_of_to.data(curser) := from;
            Predecessors_of_to.idata(v).number_of_predecessors := Predecessors_of_to.idata(v).number_of_predecessors + 1;
 

         
            Data_of_Overflow.Predecessors := Predecessors_of_to;
            Abstraction.Overflow_Cell_Data:=Data_of_Overflow;
    
      
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix)=> 
            
            
            Data_of_Overflow_Invariance  := Abstraction.Overflow_Cell_Data_Invariance;
   
            Predecessors_of_to  := Data_of_Overflow_Invariance.Predecessors;
      
    
         
            if Predecessors_of_to.data = null then
               Predecessors_of_to.data := new Array_of_Cell_Index_T (0 .. M - 1);
            end if;
        
      
            ofs := Predecessors_of_to.idata(v).Pos_of_first_predecessor;
            num := Predecessors_of_to.idata(v).number_of_predecessors;
            curser := num + ofs;  
            if curser = Predecessors_of_to.idata(Input_Index_T'Succ(v)).Pos_of_first_predecessor then
               declare
                  New_Array : Array_of_Cell_Index_T_Access;
                 array_extention: Counter_T:=4*num;
               begin
                  New_Array := new Array_of_Cell_Index_T (0 .. Predecessors_of_to.data'Length - 1 + array_extention);
                  New_Array(0 .. curser-1) := Predecessors_of_to.data(0 .. curser-1); 
                  New_Array(curser+array_extention .. New_Array'Length - 1)  := Predecessors_of_to.data(curser .. Predecessors_of_to.data'Length - 1);
                  free_data(Predecessors_of_to.data);
                  Predecessors_of_to.data := New_Array;
                  for vv in Input_Index_T range v+1 .. Predecessors_of_to.idata'Last loop
                     Predecessors_of_to.idata(vv).Pos_of_first_predecessor := Predecessors_of_to.idata(vv).Pos_of_first_predecessor + array_extention;
                  end loop;
               end ;
            end if;
            Predecessors_of_to.data(curser) := from;
            Predecessors_of_to.idata(v).number_of_predecessors := Predecessors_of_to.idata(v).number_of_predecessors + 1;
 

         
            Data_of_Overflow_Invariance.Predecessors := Predecessors_of_to;
            Abstraction.Overflow_Cell_Data_Invariance:=Data_of_Overflow_Invariance;      
            
            
            
        
         
         
         
         
         when others => 
            
            
            
            null;    
      end case;

  

      
   end save_as_predecessor_overflow ;   
   
   
   
   
 
   
   
   procedure Save_As_Predecessor(
                                 
                                 from        : in Cell_Index_T;
                                 overapproximation: in Abstract_Reachable_Set_Geometry_T;
                                 v           : in Input_Index_T; 
                                 to          : in Cell_Index_T; 
                                 Abstraction : in out Abstraction_T
                              
                                )  is
      Data_of_to_OTF        : Cell_OTF_T; 
      Predecessors_of_to_OTF: Predecessors_OTF_T_Access;
      ofs,num,curser     : Counter_T;
      M                  : constant Counter_T := Counter_T(Get_Number_Of_Control_Symbols(Abstraction));
   
      
    
      use cell_cover.BasicGrid;
      use Input_Values.InputGrid;
  
   
   
   
   begin
   

         
      Data_of_to_OTF  := CellDataGrid_OTF.get_data(to,Abstraction.Cells_OTF);
   
      Predecessors_of_to_OTF  := Data_of_to_OTF.Predecessors;
      
      If Predecessors_of_to_OTF=null then
        
         Predecessors_of_to_OTF:=new Predecessors_OTF_T;
         Predecessors_of_to_OTF.data := new Array_of_Cell_and_Overapproximation_T (0 .. M - 1);
         Predecessors_of_to_OTF.idata := new idata_T (0 .. Input_Index_T(M));
         
         for v in 0 .. Input_Index_T(M) loop
            Predecessors_of_to_OTF.idata(v).Pos_of_first_predecessor := Counter_T(v);
            Predecessors_of_to_OTF.idata(v).number_of_predecessors := 0;
         end loop;         
         
      end if;
    
         
  
        
      
      ofs := Predecessors_of_to_OTF.idata(v).Pos_of_first_predecessor;
      num := Predecessors_of_to_OTF.idata(v).number_of_predecessors;
      curser := num + ofs;  
      if curser = Predecessors_of_to_OTF.idata(Input_Index_T'Succ(v)).Pos_of_first_predecessor then
         declare
            New_Array : Array_of_Cell_and_Overapproximation_T_Access;
           array_extention: Counter_T:=num;
         begin
            New_Array := new Array_of_Cell_and_Overapproximation_T(0 .. Predecessors_of_to_OTF.data'Length - 1 + array_extention);
            New_Array(0 .. curser-1) := Predecessors_of_to_OTF.data(0 .. curser-1); 
            New_Array(curser+array_extention .. New_Array'Length - 1)  := Predecessors_of_to_OTF.data(curser .. Predecessors_of_to_OTF.data'Length - 1);
            free_data_OTF(Predecessors_of_to_OTF.data);
            Predecessors_of_to_OTF.data := New_Array;
            for vv in Input_Index_T range v+1 .. Predecessors_of_to_OTF.idata'Last loop
               Predecessors_of_to_OTF.idata(vv).Pos_of_first_predecessor := Predecessors_of_to_OTF.idata(vv).Pos_of_first_predecessor + array_extention;
            end loop;
         end ;
      end if;
      Predecessors_of_to_OTF.data(curser).Cell := from;
      Predecessors_of_to_OTF.data(curser).Overapproximation := overapproximation;
      Predecessors_of_to_OTF.idata(v).number_of_predecessors := Predecessors_of_to_OTF.idata(v).number_of_predecessors + 1;
 

         
      Data_of_to_OTF.Predecessors := Predecessors_of_to_OTF;
      CellDataGrid_OTF.set_data(Data_of_to_OTF,to,Abstraction.Cells_OTF);
    
    
      
   end save_as_predecessor ; 
   
   
   
   
   
   
   
   
   
   
-- --------------------------------
-- Get_Predecessors ---------------
-- --------------------------------

   procedure Get_Predecessors(cell        : in Cell_Index_T; 
                              v           : in Input_Index_T; 
                              Abstraction : in Abstraction_T;
                              Predecessors: in out Dynamic_Cell_Container;
                              List_of_Overapproximations : in out Dynamic_Overapproximation_Container) 
   is
      use cell_cover.BasicGrid;
      use Input_Values.InputGrid;
   begin

 
      
      case  Abstraction.Synthesis is
      when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) =>    
  
         declare
            D      : Cell_T;
            num    : Counter_T;
            pos    : Counter_T;
        
         begin          
          
            if Cell=Abstraction.Overflow_Cell then
               
            D:=Abstraction.Overflow_Cell_Data;   
            else
            
            D      := CellDataGrid.get_data(cell,Abstraction.Cells);           
            end if;
            
            num    := D.Predecessors.idata(v).number_of_predecessors;
            pos    :=D.Predecessors.idata(v).Pos_of_first_predecessor;  
            
            if D.Predecessors.data=null or num = 0 then 
               null;        
            else
              
               For i in 1..num loop          
                  Save_Cell_To_Container(Predecessors, D.Predecessors.data(pos+i-1) );
               end loop;
               
            
            end if;
              
         end;     

      when  Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix) =>
   
         declare
            D      : Cell_OTF_T ;
            num    : Counter_T;
            pos    : Counter_T;
        
         begin          

            if Cell=Abstraction.Overflow_Cell then
               
               null;  
            else
            
               D      := CellDataGrid_OTF.get_data(cell,Abstraction.Cells_OTF);           
            end if;
            
 
            
            
                  
            if D.Predecessors=null  then 
               null;
            
            else 
               
               num     := D.Predecessors.idata(v).number_of_predecessors;
               pos     := D.Predecessors.idata(v).Pos_of_first_predecessor;
               
            
               if  num = 0 then 
                  null;
               else
                 
              
             
                  
                  For i in 1..num loop          
                     Save_Cell_To_Container(Predecessors,D.Predecessors.data(pos+i-1).Cell);
                     Save_Overapproximation_To_Container(List_of_Overapproximations,D.Predecessors.data(pos+i-1).Overapproximation); 
                  end loop;
                  
                  
               end if;  

            
            
            
            
               
            end if;
         
            
            
         end;  
         
         when  Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix) =>  
         
               declare
            D      : Cell_Invariance_T;
            num    : Counter_T ;
            pos    : Counter_T;
        
         begin          

               
               if Cell=Abstraction.Overflow_Cell then
               
                  D:=Abstraction.Overflow_Cell_Data_Invariance;   
               else
            
                  D      := CellDataGrid_Invariance.get_data(cell,Abstraction.Cells_Invariance);           
               end if;
            
            num    := D.Predecessors.idata(v).number_of_predecessors;
            pos    :=D.Predecessors.idata(v).Pos_of_first_predecessor;      
               
               
               
               
               
                  
            if D.Predecessors.data=null or num = 0 then 
               null;        
            else
              
               For i in 1..num loop          
                  Save_Cell_To_Container(Predecessors, D.Predecessors.data(pos+i-1) );
               end loop;
               
            
            end if;
                      
         end;      
            
         when others =>
            null;
      end case;      
      
      
end Get_Predecessors;

end Abstraction_I14sym.Predecessors ;
