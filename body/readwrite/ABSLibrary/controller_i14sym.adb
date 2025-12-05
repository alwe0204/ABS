package body controller_i14sym is

   svn_author  : constant String := "$Author: lf3eelma $";
   svn_revision: constant String := "$Revision: 2402 $";
   svn_date    : constant String := "$Date: 2021-10-26 11:09:50 +0200 (Di, 26 Okt 2021) $";

   
 -- --------------------------------
-- Install ------------------------
-- --------------------------------

   
   procedure Initialize_Controller(
                                   Abstraction         : in out Abstraction_T; 
                                   Controller          : in out Abstract_Controller_Type_Access; 
                                   Representation      : in  Controller_Data_Structure
                                  ) is
    use BasicGrid; 
    use InputGrid;
           
   Data_Struc: Controller_Data_Structure;
   begin
   
     
      
      Data_Struc:=Representation;
      if Representation = Controller_Data_Structure'(Defined_by_Abstraction) then
            
         case Get_Abstraction_Data_Structure(Abstraction) is
               
            when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_StandardDijkstra_SparseMatrix) =>
               Data_Struc:=Controller_Data_Structure'(Explicit_Array_for_Reachabillity_Problem);
           
            when Specification_SynthesisMethod_AbstractionRepresentation'(ReachAvoid_OnTheFlyDijkstra_SparseMatrix) =>
              Data_Struc:=Controller_Data_Structure'(Explicit_Array_for_Reachabillity_Problem);
         
            when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_StandardMethod_SparseMatrix) =>
               Data_Struc:=Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_Standard_Synthesis);
               
             when Specification_SynthesisMethod_AbstractionRepresentation'(Invariance_OnTheFlyMethod_SparseMatrix) =>
               Data_Struc:=Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_OnTheFly_Synthesis);   
              
            when others =>
               null;
         end case;
         
         end if;
      
      
      
      
      Controller:= new Abstract_Controller_Type(Data_Struc);
      Controller.First_Valid_State_Index := Get_First_Index     (Abstraction);
      Controller.Last_Valid_State_Index  := Get_Last_Used_Index (Abstraction);
     
      Controller.First_Valid_Control_Index:=Get_First_Input_Index(Abstraction);
      Controller.Last_Valid_Control_Index :=Get_Last_Input_Index (Abstraction);
      
      Controller.Reserved_Control_Index_1:=Controller.Last_Valid_Control_Index+1;
      Controller.Reserved_Control_Index_2:=Controller.Last_Valid_Control_Index+2;
    
    
      
      case Data_Struc is
         when Controller_Data_Structure'(Explicit_Array_for_Reachabillity_Problem) =>
             
            Controller.Controller_Array_Reach := new Array_of_Control_Index(Controller.First_Valid_State_Index .. Controller.Last_Valid_State_Index);  
            Controller.Controller_Array_Reach.all:= (others=>Controller.Reserved_Control_Index_2);
            
         when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_OnTheFly_Synthesis) =>
            
            Controller.Controller_Array_Invariance:= new Set_Valued_Control_Map_Sparse_Matrix_T(Controller.First_Valid_State_Index .. Controller.Last_Valid_State_Index);
            
            for i in Controller.Controller_Array_Invariance'Range loop

               Controller.Controller_Array_Invariance(i).controls_array:= new Array_of_Controls_for_a_Cell(1..1);
               Controller.Controller_Array_Invariance(i).controls_array(1):=Controller.Reserved_Control_Index_2;
             
                if   Is_Target(i,Abstraction) then
               Controller.Controller_Array_Invariance(i).controls_array(1):=Controller.Reserved_Control_Index_1;
             end if;
             
               if   Is_Obstacle(i,Abstraction) then
               Controller.Controller_Array_Invariance(i).controls_array(1):=Controller.Reserved_Control_Index_2;
             end if;
             
               Controller.Controller_Array_Invariance(i).num_of_controls:=1;
               
               end loop;
   
            
          when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_Standard_Synthesis) =>   
            
            Controller.Controller_Array_Invariance_Dense:= new Set_Valued_Control_Map_Dense_Matrix_T(Controller.First_Valid_State_Index .. Controller.Last_Valid_State_Index);
            
              for i in Controller.Controller_Array_Invariance_Dense'Range loop
               
               Controller.Controller_Array_Invariance_Dense(i).marks:= new Array_of_Control_Marks_for_a_Cell(Controller.First_Valid_Control_Index..Controller.Last_Valid_Control_Index);
               
             Controller.Controller_Array_Invariance_Dense(i).marks.all:=(others=>False);
             Controller.Controller_Array_Invariance_Dense(i).num_of_valid_controls:=0;
            
                if   Is_Target(i,Abstraction) then
           Controller.Controller_Array_Invariance_Dense(i).marks.all:=(others=>True);
               Controller.Controller_Array_Invariance_Dense(i).num_of_valid_controls:=Controller.Last_Valid_Control_Index-Controller.First_Valid_Control_Index+1;
             end if;
             
               if   Is_Obstacle(i,Abstraction) then
                       Controller.Controller_Array_Invariance_Dense(i).marks.all:=(others=>False);
             Controller.Controller_Array_Invariance_Dense(i).num_of_valid_controls:=0;
             end if;
              
              
              
               end loop;
     
         when others =>
            null;
      end case;
      
   end Initialize_Controller;
   
   
   procedure free_reach_array           is new Ada.Unchecked_Deallocation (Array_of_Control_Index,Array_of_Control_Index_access);
   procedure free_controller_pointer    is new Ada.Unchecked_Deallocation (Abstract_Controller_Type,Abstract_Controller_Type_Access);
   procedure free_Set_valued_controller is new Ada.Unchecked_Deallocation (Set_Valued_Control_Map_Sparse_Matrix_T,Set_Valued_Control_Map_Sparse_Matrix_T_Access);
   procedure freed_cell_marks           is new Ada.Unchecked_Deallocation (Array_of_Control_Marks_for_a_Cell,Array_of_Control_Marks_for_a_Cell_Access);
   procedure free_Set_valued_c_dense    is new Ada.Unchecked_Deallocation (Set_Valued_Control_Map_Dense_Matrix_T,Set_Valued_Control_Map_Dense_Matrix_T_Access);
  
   procedure Finalize_Controller( 
                                  Controller          : in out Abstract_Controller_Type_Access 
                                 ) is   
   begin

      case Controller.Representation is
         
         when Controller_Data_Structure'(Explicit_Array_for_Reachabillity_Problem) =>
              
            free_reach_array(Controller.Controller_Array_Reach);
            free_controller_pointer(Controller);
          
         when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_OnTheFly_Synthesis) =>   
           
            for i in Controller.Controller_Array_Invariance'range loop
               
               free_cell_controls(Controller.Controller_Array_Invariance(i).controls_array);
               Controller.Controller_Array_Invariance(i).num_of_controls:=0;
             end loop;
            
           free_Set_valued_controller(Controller.Controller_Array_Invariance); 
         
            
          when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_Standard_Synthesis) =>    
          
            for i in Controller.Controller_Array_Invariance_Dense'Range loop                             
               freed_cell_marks(Controller.Controller_Array_Invariance_Dense(i).marks);              
               end loop;
          
          free_Set_valued_c_dense(Controller.Controller_Array_Invariance_Dense);  
         when others=>
           
            null;
            
      end case;
      
   end Finalize_Controller;
   
   
   
   
   
-- --------------------------------
-- Set_Control_Symbol -------------
-- --------------------------------

 
 procedure Set_Cell_Control(
                              cell       : in Cell_Index_T; 
                              v          : in Input_Index_T; 
                              Controller : in out Abstract_Controller_Type_Access;
                              Abstraction: in out Abstraction_T
                             ) is      
   
   begin
   
      case Controller.Representation is
         
         when Controller_Data_Structure'(Explicit_Array_for_Reachabillity_Problem) =>
            

               Controller.Controller_Array_Reach(Cell):=v;
            
            
          when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_OnTheFly_Synthesis) =>   
              
               Controller.Controller_Array_Invariance(cell).controls_array(1):=v;
            
         
          when others => null;
            
      end case;
      
      
      
   end Set_Cell_Control;

-- --------------------------------
-- Read_Control_Symbol -------------
-- --------------------------------


  
   procedure Read_Cell_Control(
                               cell          : in Cell_Index_T; 
                               Controller    : in  Abstract_Controller_Type_Access;
                               Abstraction: in Abstraction_T;
                               Control_Input : out Input_Index_T
                              )  is      

   begin
   
      case Controller.Representation is
         
         when Controller_Data_Structure'(Explicit_Array_for_Reachabillity_Problem) =>
                        
               Control_Input:= Controller.Controller_Array_Reach(Cell);
            
         when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_OnTheFly_Synthesis) =>   
             
               Control_Input:=Controller.Controller_Array_Invariance(cell).controls_array(1);
               
         when others => null;
            
      end case;
      
      
      
   end Read_Cell_Control;

   procedure Read_Array_of_Cell_Controls(
                               cell          : in Cell_Index_T; 
                               Controller    : in  Abstract_Controller_Type_Access;
                               Control_array : out Array_of_Controls_for_a_Cell_Access
                              )  is      
      array_length:Counter_T;
      curser: Counter_T:=1;
   begin
   
      case Controller.Representation is
         
         when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_Standard_Synthesis) =>
            
            array_length:=Counter_T(Controller.Controller_Array_Invariance_Dense(cell).num_of_valid_controls);
            if array_length>0 then
               Control_array:=new Array_of_Controls_for_a_Cell(1..array_length);
            
               For i in Controller.Controller_Array_Invariance_Dense(cell).marks'Range loop
               
                  if Controller.Controller_Array_Invariance_Dense(cell).marks(i) then
                     Control_array(curser):=i; curser:=curser+1;
                  end if;
               
               end loop;
            else
               Control_array:=null;   
            end if;
            
            when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_OnTheFly_Synthesis) =>   
            
            
            array_length:=Counter_T(Controller.Controller_Array_Invariance(cell).num_of_controls);
            if array_length>0 then
               Control_array:=new Array_of_Controls_for_a_Cell(1..array_length);           
               Control_array(1..array_length):=Controller.Controller_Array_Invariance(cell).controls_array(1..array_length);
            else
               Control_array:=null;   
            end if;
         when others => null;
            
      end case;
      
      
      
   end Read_Array_of_Cell_Controls;
   
   
   procedure Include_Cell_Control (
                                   cell       : in Cell_Index_T; 
                                   v          : in Input_Index_T; 
                                   Controller : in out Abstract_Controller_Type_Access
                                  ) is      
   begin      
      case Controller.Representation is
                 
         when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_OnTheFly_Synthesis) =>   
                             
               if Controller.Controller_Array_Invariance(cell).num_of_controls=Controller.Controller_Array_Invariance(cell).controls_array'Length then
                  
                  declare
                     num: Counter_T:=Controller.Controller_Array_Invariance(cell).num_of_controls;
                     new_control_array: Array_of_Controls_for_a_Cell_Access;   
                  begin
                     
                     new_control_array:= new Array_of_Controls_for_a_Cell(1..num*2);  
                  
                     new_control_array(1..num):=Controller.Controller_Array_Invariance(cell).controls_array(1..num);
                   
                     free_cell_controls(Controller.Controller_Array_Invariance(cell).controls_array);
                     Controller.Controller_Array_Invariance(cell).controls_array:=new_control_array;
                                   
                  end;
                    
               end if;
               Controller.Controller_Array_Invariance(cell).num_of_controls:=Controller.Controller_Array_Invariance(cell).num_of_controls+1;
               Controller.Controller_Array_Invariance(cell).controls_array(Controller.Controller_Array_Invariance(cell).num_of_controls):=v;
                 
         when others => null;
            
      end case;
   end Include_Cell_Control;
   
   
   procedure Exclude_Cell_Control (
                                   cell       : in Cell_Index_T; 
                                   v          : in Input_Index_T; 
                                   Controller : in out Abstract_Controller_Type_Access
                                  ) is     
  use InputGrid;
   begin    
      case Controller.Representation is
         
         when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_Standard_Synthesis) =>
            
            if Controller.Controller_Array_Invariance_Dense(cell).marks(v)=True then
               Controller.Controller_Array_Invariance_Dense(cell).num_of_valid_controls:=Controller.Controller_Array_Invariance_Dense(cell).num_of_valid_controls-1;
               end if;
            
            Controller.Controller_Array_Invariance_Dense(cell).marks(v):=False;   
            
         when others =>
                       
            null;
                       
      end case;
   end Exclude_Cell_Control;
   
   
   
   function Get_num_of_valid_Controls  (cell: in Cell_Index_T; Controller : in Abstract_Controller_Type_Access) return Input_Index_T is
      
   begin
      
      return Controller.Controller_Array_Invariance_Dense(cell).num_of_valid_controls;
      
   end Get_num_of_valid_Controls;
   
   
   function Get_Reserved_Control_Symbol_1(Controller : in  Abstract_Controller_Type_Access)  return Input_Index_T is
   begin
      return Controller.Reserved_Control_Index_1;
   end Get_Reserved_Control_Symbol_1;
      
   function Get_Reserved_Control_Symbol_2(Controller : in  Abstract_Controller_Type_Access)  return Input_Index_T is
   begin
      return Controller.Reserved_Control_Index_2;
   end Get_Reserved_Control_Symbol_2;  

-- --------------------------------
-- Get_First_Index ----------------
-- --------------------------------

function Get_First_Index (Controller : in  Abstract_Controller_Type_Access) return Cell_Index_T is
begin
 return Controller.First_Valid_State_Index;
end Get_First_Index;

-- --------------------------------
-- Get_Last_Index -----------------
-- --------------------------------

function Get_Last_Index (Controller : in  Abstract_Controller_Type_Access) return Cell_Index_T is
begin
 return Controller.Last_Valid_State_Index;
end Get_Last_Index;

    
function Controller_Representation (Controller  : in  Abstract_Controller_Type_Access)  return Controller_Data_Structure is
   begin
      return Controller.Representation;
end Controller_Representation;

end controller_i14sym;
