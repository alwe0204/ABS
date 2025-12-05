with Abstraction_I14sym.Predecessors;

package body Abstraction_I14sym is

svn_author  : String := "$Author: lf3eelma $";
svn_revision: String := "$Revision: 2408 $";
svn_date    : String := "$Date: 2021-10-26 15:36:24 +0200 (Di, 26 Okt 2021) $";

-- --------------------------------
-- Initialize_abstraction --
-- -------------------------------- 

  
procedure Initialize_Abstraction(Abstraction    : in out Abstraction_T_Access;
                                 Representation : in Specification_SynthesisMethod_AbstractionRepresentation) is    
begin      
 Abstraction:=new Abstraction_T(Representation);       
end Initialize_Abstraction;
   
-- --------------------------------
-- Get_Abstraction_Data_Structure --
-- -------------------------------- 
      
function Get_Abstraction_Data_Structure (Abstraction : in Abstraction_T) return Specification_SynthesisMethod_AbstractionRepresentation is
begin
  return Abstraction.Synthesis;   
end Get_Abstraction_Data_Structure;
   
   
   
-- --------------------------------
-- Get_Number_Of_Control_Symbols --
-- --------------------------------

function Get_Number_Of_Control_Symbols(Abstraction : in Abstraction_T) return Input_Index_T is
begin
 return Abstraction.Number_of_Controls;
end Get_Number_Of_Control_Symbols;

-- --------------------------------
-- Get_First_Index ----------------
-- --------------------------------

function Get_First_Index(Abstraction : in Abstraction_T) return Cell_Index_T is
begin
 return Cell_Cover.Get_First_Index(Abstraction.Cover);
end Get_First_Index;


-- --------------------------------
-- Get_Last_Used_Index ------------
-- --------------------------------

function Get_Last_Used_Index(Abstraction : in Abstraction_T) return Cell_Index_T is
begin
 return Cell_Cover.Get_Last_Index(Abstraction.Cover);
end Get_Last_Used_Index;
 
-- --------------------------------
-- Is_Index_Cell ------------
-- --------------------------------  
   
function Is_Index_Cell(idx: in Cell_Index_T;Abstraction : in Abstraction_T) return Boolean is
begin
 return Is_Index_Cell(idx,Abstraction.Cover);
end Is_Index_Cell;   
   

-- --------------------------------
-- Get_Predecessors ---------------
-- --------------------------------
  
   
procedure Get_Predecessors(   cell        : in Cell_Index_T; 
                              v           : in Input_Index_T; 
                              Abstraction : in out Abstraction_T;
                              Predecessors: in out Dynamic_Cell_Container;
                              Associated_Overapproximations: in out Dynamic_Overapproximation_Container) is
begin        
 Abstraction_I14sym.Predecessors.Get_Predecessors(cell,v,Abstraction,Predecessors,Associated_Overapproximations);    
end Get_Predecessors;
   
procedure Get_Predecessors   (cell        : in Cell_Index_T; 
                              v           : in Input_Index_T; 
                              Abstraction : in out Abstraction_T;
                              Predecessors: in out Dynamic_Cell_Container) is
 Temp_Overapproximations:  Dynamic_Overapproximation_Container;
begin
 Abstraction_I14sym.Predecessors.Get_Predecessors(cell,v,Abstraction,Predecessors,Temp_Overapproximations);    
end Get_Predecessors; 
   
   
   

-- ------------------------------------------
-- Decrease_Number_Of_Remaining_Successors --
-- ------------------------------------------

procedure Decrease_Number_Of_Remaining_Successors(cell        : in Cell_Index_T; 
                                                  v           : in Input_Index_T; 
                                                  Abstraction : in Abstraction_T) is
 D : Cell_T;
begin
 D := CellDataGrid.get_data(cell,Abstraction.Cells);
 D.counters_rem_successors(v) := D.counters_rem_successors(v) - 1;
 CellDataGrid.set_data(D,cell,Abstraction.Cells);
end Decrease_Number_Of_Remaining_Successors;

-- ------------------------------------------
-- No_Remaining_Successors ------------------
-- ------------------------------------------

function No_Remaining_Successors(cell        : in Cell_Index_T; 
                                 v           : in Input_Index_T; 
                                 Abstraction : in Abstraction_T) return Boolean is
begin
 return CellDataGrid.get_data(cell,Abstraction.Cells).counters_rem_successors(v) = 0;
end No_Remaining_Successors;

-- ------------------------------------------
-- Get_Number_Of_Remaining_Successors -------
-- ------------------------------------------

function Get_Number_Of_Remaining_Successors(cell        : in Cell_Index_T; 
                                            v           : in Input_Index_T; 
                                            Abstraction : in Abstraction_T) return Counter_T is
begin
 return CellDataGrid.get_data(cell,Abstraction.Cells).counters_rem_successors(v);
end Get_Number_Of_Remaining_Successors;

-- --------------------------------
-- Get_First_Input_Index ----------
-- --------------------------------

function Get_First_Input_Index(Abstraction : in Abstraction_T) return Input_Index_T is
begin
 return Abstraction.First_Input_Index;
end Get_First_Input_Index;

-- --------------------------------
-- Get_Last_Input_Index -----------
-- --------------------------------

function Get_Last_Input_Index(Abstraction : in Abstraction_T) return Input_Index_T is
begin
 return Abstraction.Last_Input_Index;
end Get_Last_Input_Index;
-- --------------------------------
-- Get_Overflow_Index -----------
-- --------------------------------
   
function Get_Overflow_Index(Abstraction : in Abstraction_T) return Cell_Index_T is    
begin
 return Abstraction.Overflow_Cell;
end Get_Overflow_Index;
   
-- --------------------------------
-- Is_Obstacle -----------
-- --------------------------------   
   
function  Is_Obstacle(cell        : in Cell_Index_T; 
                      Abstraction : in Abstraction_T) return Boolean is
begin
      case Abstraction.Synthesis is
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix)=> 
            return CellDataGrid.get_data(cell,Abstraction.Cells).obstacle;
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix)=> 
            return CellDataGrid_OTF.get_data(cell,Abstraction.Cells_otf).obstacle;
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix)=> 
            return CellDataGrid_Invariance.get_data(cell,Abstraction.Cells_Invariance).obstacle;
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix)=> 
        return CellDataGrid_Invariance_OTF.get_data(cell,Abstraction.Cells_Invariance_OTF).obstacle;
         when others =>
            null;     
      end case;
 return False;
end Is_Obstacle; 
 
-- --------------------------------
-- Is_Target -----------
-- --------------------------------     
   
function  Is_Target(cell        : in Cell_Index_T; 
                      Abstraction : in Abstraction_T) return Boolean is

begin
 
      case Abstraction.Synthesis is
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix)=> 
            return CellDataGrid.get_data(cell,Abstraction.Cells).target;
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix)=> 
            return CellDataGrid_OTF.get_data(cell,Abstraction.Cells_otf).target;
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix)=> 
            return CellDataGrid_Invariance.get_data(cell,Abstraction.Cells_Invariance).target;
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix)=> 
        return CellDataGrid_Invariance_OTF.get_data(cell,Abstraction.Cells_Invariance_OTF).target;
         when others =>
            null;
            
      
      end case;
   return False;
   end Is_Target; 
   
-- --------------------------------
-- Is_Target -----------
-- --------------------------------     
   
function  Is_Initial(cell        : in Cell_Index_T; 
                     Abstraction : in Abstraction_T) return Boolean is

begin
 
      case Abstraction.Synthesis is
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix)=> 
            return CellDataGrid.get_data(cell,Abstraction.Cells).initial;
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix)=> 
            return CellDataGrid_OTF.get_data(cell,Abstraction.Cells_otf).initial;
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix)=> 
            return CellDataGrid_Invariance.get_data(cell,Abstraction.Cells_Invariance).initial;
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix)=> 
        return CellDataGrid_Invariance_OTF.get_data(cell,Abstraction.Cells_Invariance_OTF).initial;
         when others =>
            null;
            
      
      end case;
   return False;
   end Is_Initial;  


procedure  Make_Obstacle      (cell        : in Cell_Index_T; 
                               Abstraction : in Abstraction_T) is

D   : Cell_T;
D1  : Cell_OTF_T;
D2  : Cell_Invariance_T;
D3  : Cell_Invariance_OTF_T;

begin

      case Abstraction.Synthesis is
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix)=> 
           D:= CellDataGrid.get_data(cell,Abstraction.Cells);
           D.obstacle:=True;
           CellDataGrid.set_data(D,cell,Abstraction.Cells);

         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix)=> 
           D1:= CellDataGrid_otf.get_data(cell,Abstraction.Cells_otf);
           D1.obstacle:=True;
           CellDataGrid_otf.set_data(D1,cell,Abstraction.Cells_otf);

         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix)=>      
           D2:= CellDataGrid_invariance.get_data(cell,Abstraction.Cells_invariance);
           D2.obstacle:=True;
           CellDataGrid_invariance.set_data(D2,cell,Abstraction.Cells_invariance);


         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix)=> 
           D3:= CellDataGrid_invariance_otf.get_data(cell,Abstraction.Cells_invariance_otf);
           D3.obstacle:=True;
           CellDataGrid_invariance_otf.set_data(D3,cell,Abstraction.Cells_invariance_otf);

         when others =>
            null;
            
      
      end case;


end Make_Obstacle;



procedure  Make_Target      (cell        : in Cell_Index_T; 
                               Abstraction : in Abstraction_T) is


D   : Cell_T;
D1  : Cell_OTF_T;
D2  : Cell_Invariance_T;
D3  : Cell_Invariance_OTF_T;

begin

      case Abstraction.Synthesis is
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix)=> 
           D:= CellDataGrid.get_data(cell,Abstraction.Cells);
           D.target:=True;
           CellDataGrid.set_data(D,cell,Abstraction.Cells);

         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix)=> 
           D1:= CellDataGrid_otf.get_data(cell,Abstraction.Cells_otf);
           D1.target:=True;
           CellDataGrid_otf.set_data(D1,cell,Abstraction.Cells_otf);

         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix)=>      
           D2:= CellDataGrid_invariance.get_data(cell,Abstraction.Cells_invariance);
           D2.target:=True;
           CellDataGrid_invariance.set_data(D2,cell,Abstraction.Cells_invariance);


         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix)=> 
           D3:= CellDataGrid_invariance_otf.get_data(cell,Abstraction.Cells_invariance_otf);
           D3.target:=True;
           CellDataGrid_invariance_otf.set_data(D3,cell,Abstraction.Cells_invariance_otf);

         when others =>
            null;
            
      
      end case;


end Make_Target;



procedure  Make_initial      (cell        : in Cell_Index_T; 
                               Abstraction : in Abstraction_T) is

D   : Cell_T;
D1  : Cell_OTF_T;
D2  : Cell_Invariance_T;
D3  : Cell_Invariance_OTF_T;

begin

      case Abstraction.Synthesis is
         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix)=> 
           D:= CellDataGrid.get_data(cell,Abstraction.Cells);
           D.initial:=True;
           CellDataGrid.set_data(D,cell,Abstraction.Cells);

         when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix)=> 
           D1:= CellDataGrid_otf.get_data(cell,Abstraction.Cells_otf);
           D1.initial:=True;
           CellDataGrid_otf.set_data(D1,cell,Abstraction.Cells_otf);

         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix)=>      
           D2:= CellDataGrid_invariance.get_data(cell,Abstraction.Cells_invariance);
           D2.initial:=True;
           CellDataGrid_invariance.set_data(D2,cell,Abstraction.Cells_invariance);


         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix)=> 
           D3:= CellDataGrid_invariance_otf.get_data(cell,Abstraction.Cells_invariance_otf);
           D3.initial:=True;
           CellDataGrid_invariance_otf.set_data(D3,cell,Abstraction.Cells_invariance_otf);

         when others =>
            null;
            
      
      end case;

end Make_Initial;



      
-- --------------------------------
-- Save_As_Predecessor -----------
-- --------------------------------   
   
procedure Save_As_Predecessor(from        : in Cell_Index_T;
                                 overapproximation: in Abstract_Reachable_Set_Geometry_T;
                                 v           : in Input_Index_T; 
                                 to          : in Cell_Index_T; 
                                 Abstraction : in out Abstraction_T) is
begin
      

         
         Abstraction_I14sym.Predecessors.Save_As_Predecessor(from,
                                                             overapproximation ,
                                                             v, 
                                                             to,
                                                             Abstraction ); 
end Save_As_Predecessor;
   
   
procedure Save_As_Predecessor(from        : in Cell_Index_T;                           
                                 v           : in Input_Index_T; 
                                 to          : in Cell_Index_T; 
                                 Abstraction : in out Abstraction_T) is
begin
      
         Abstraction_I14sym.Predecessors.Save_As_Predecessor(from,
                                                             v, 
                                                             to,
                                                             Abstraction );  
         
end Save_As_Predecessor;
   
   
   
   
 -- --------------------------------
-- Set_Mark -----------
-- --------------------------------    
   
      
procedure Set_Mark(Cell        : in Cell_Index_T;
                   input       : in Input_Index_T;
                   Abstraction : in out Abstraction_T) is
   D : Cell_OTF_T;   
begin
    
      case  Abstraction.Synthesis is
      when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix) =>    
         D:=CellDataGrid_OTF.get_data(cell,Abstraction.Cells_otf); 
         If D.Boolean_Marks= null then
            D.Boolean_Marks:= new Array_of_Boolean_Marks(Get_First_Input_Index(Abstraction).. Get_Last_Input_Index(Abstraction));
            D.Boolean_Marks.all:=(others=> False);   
         end if;
         D.Boolean_Marks(input):=True;
         CellDataGrid_OTF.set_data(D,cell,Abstraction.Cells_OTF);   
      when others =>
         Put_Line("Unexpected Abstraction type in call to Set_Mark");
         raise Assumption_Error;
   end case;
   
   
end Set_Mark;

 -- --------------------------------
-- Check_Mark -----------
-- --------------------------------  
   
function Check_Mark(Cell: in Cell_Index_T;
                    input:in Input_Index_T;
                    Abstraction : in  Abstraction_T) return Boolean is
   D : Cell_OTF_T;  
begin
   case  Abstraction.Synthesis is
      when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix) =>    
         
         D:=CellDataGrid_OTF.get_data(cell,Abstraction.Cells_otf);
         
         If D.Boolean_Marks= null then
            return False;
         else 
       --   Put_Line(Cell'img &" "& input'img&" "& Get_First_Input_Index(Abstraction)'img& " "& D.Boolean_Marks'Length'img);
         return D.Boolean_Marks.all(input);
         end if;
         
      when others =>
          
           Put_Line("Unexpected Abstraction type in call to Check_Mark");
           raise Assumption_Error;
         
   end case;
         
end Check_Mark;
   
   procedure free_data           is new Ada.Unchecked_Deallocation(Array_of_Cell_Index_T,Array_of_Cell_Index_T_Access);
   procedure free_data_OTF       is new Ada.Unchecked_Deallocation(Array_of_Cell_and_Overapproximation_T,Array_of_Cell_and_Overapproximation_T_Access);
   procedure free_idata          is new Ada.Unchecked_Deallocation(idata_T,idata_T_Access);

   procedure free_pred_record    is new Ada.Unchecked_Deallocation(Predecessors_OTF_T,Predecessors_OTF_T_Access);
   procedure free_Boolean_marks  is new Ada.Unchecked_Deallocation(Array_of_Boolean_Marks,Array_of_Boolean_Marks_Access);
   

 -- --------------------------------
-- Clear_Cell_Data -----------
-- --------------------------------  
   
procedure Clear_Cell_Data(Cell: in Cell_Index_T;
                             Abstraction : in out Abstraction_T) is 
      
      
   begin
      
      case  Abstraction.Synthesis is
      when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) =>    
  
         declare
            Data_of_Cell         : Cell_T         := CellDataGrid.get_data(Cell,Abstraction.Cells); 
         begin          
            
            if Data_of_Cell.Predecessors.data /= null then
               free_data(Data_of_Cell.Predecessors.data); 
               
            end if;

            if Data_of_Cell.Predecessors.idata /= null then
               free_idata(Data_of_Cell.Predecessors.idata); 
               
            end if;           
         CellDataGrid.set_data(Data_of_Cell,cell,Abstraction.Cells);   
         end;     
  
      when  Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix) =>
   
         declare
            Data_of_Cell_OTF         : Cell_OTF_T         := CellDataGrid_OTF.get_data(Cell,Abstraction.Cells_otf);  
         begin    
   
            if   Data_of_Cell_OTF.Predecessors/=null then
               
              if Data_of_Cell_OTF.Predecessors.data/=null then
               free_data_OTF    (Data_of_Cell_OTF.Predecessors.data);            
              end if;
              
              if  Data_of_Cell_OTF.Predecessors.idata/=null then
                  free_idata       (Data_of_Cell_OTF.Predecessors.idata);
                  
              end if;
               
              free_pred_record (Data_of_Cell_OTF.Predecessors);           
              Data_of_Cell_OTF.Predecessors:=null; 
     
               
            end if;
           
            if Data_of_Cell_OTF.Boolean_Marks/=null then
                  
               free_Boolean_marks(Data_of_Cell_OTF.Boolean_Marks);  
               Data_of_Cell_OTF.Boolean_Marks:=null;  
            end if;
  
            
            
         CellDataGrid_OTF.set_data(Data_of_Cell_OTF,cell,Abstraction.Cells_OTF);   
         end;
       
         
         when others => 
            
            null;
      end case; 
      
      
   end Clear_Cell_Data;
   
 -- --------------------------------
-- Save_one_Overapproximation -----------
-- --------------------------------  
   
procedure Save_one_Overapproximation(Cell : in Cell_Index_T; Overapproximation : in Abstract_Reachable_Set_Geometry_T; Abstraction: in out Abstraction_T) is
      D: Cell_Invariance_OTF_T; 
   begin
      
      case Abstraction.Synthesis is
         
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix)=>
            
            D:=CellDataGrid_Invariance_OTF.get_data(Cell,Abstraction.Cells_Invariance_OTF);
            D.Stored_Overapproximation:=Overapproximation;
            CellDataGrid_Invariance_OTF.set_data(D,cell,Abstraction.Cells_Invariance_OTF);
         when others =>
            
            null;
            
      end case; 
      
      
end Save_one_Overapproximation;
   
 -- --------------------------------
-- Read_one_Overapproximation -----------
-- --------------------------------  
   
function Read_one_Overapproximation(Cell : in Cell_Index_T; Abstraction: in  Abstraction_T) return Abstract_Reachable_Set_Geometry_T is 
      D: Cell_Invariance_OTF_T; 
      o: Abstract_Reachable_Set_Geometry_T;
   begin
      
      case Abstraction.Synthesis is
         
         when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix)=>
            
            D:=CellDataGrid_Invariance_OTF.get_data(Cell,Abstraction.Cells_Invariance_OTF);
            o:= D.Stored_Overapproximation;

         when others =>
            
            null;
            
      end case; 
      
     return o; 
end Read_one_Overapproximation;
   
   
     
function Get_Cell_Cover(Abstraction : in Abstraction_T) return Cell_Cover_T is
begin
 return Abstraction.Cover;
end Get_Cell_Cover;
   
function Get_Input_Data(Abstraction : in Abstraction_T) return Input_Grid_T is
begin
 return Abstraction.Input_Data;
end Get_Input_Data;

end Abstraction_I14sym;
