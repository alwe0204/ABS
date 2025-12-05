with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Input_Values ; use Input_Values;
with cell_cover; use cell_cover;


package body output_results is

svn_author  : constant String := "$Author$";
svn_revision: constant String := "$Revision$";
svn_date    : constant String := "$Date$";

-- --------------------------------
-- Is_Control_Problem_Solved ------
-- --------------------------------

function Is_Control_Problem_Solved(AbstractSpec: in Abstract_Specification_T; 
                                   Controller  : in Abstract_Controller_Type_Access;
                                   Abstraction : in Abstraction_T) return Boolean
is
      use InputGrid;
      symbol_for_no_controls,u:Input_Index_T;
       Array_of_controls :Array_of_Controls_for_a_Cell_Access;
begin 
   
      
       case Controller_Representation(Controller) is   
      
         when Controller_Data_Structure'(Explicit_Array_for_Reachabillity_Problem) =>
      for i in AbstractSpec.InitialCells'Range loop
        
         Read_Cell_Control(cell          => AbstractSpec.InitialCells(i),
                           Controller    => Controller,
                           Abstraction   => Abstraction,
                           Control_Input => u);
         
     
         if u = (Get_Reserved_Control_Symbol_2(Controller)) and not Is_Target(AbstractSpec.InitialCells(i),Abstraction) then
            return False;
         end if;
    
      
      
      end loop;
      
 return True;
 
   when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_OnTheFly_Synthesis) =>
   symbol_for_no_controls  :=Get_Reserved_Control_Symbol_2(Controller);
  for i in AbstractSpec.InitialCells'Range loop
       Read_Array_of_Cell_Controls(cell          => AbstractSpec.InitialCells(i),
                                           Controller    => Controller,
                                           Control_array => Array_of_controls) ;    
         
               	if Array_of_controls=null then 
                	return false;   
                	else
                	
                  	For i in Array_of_controls'Range loop                  
                    	if Array_of_controls(i)= symbol_for_no_controls  then 
                	return false;   
           	end if;
                  	end loop;
                  	end if;
 
 end loop;
 return True;
 
 
when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_Standard_Synthesis) =>
 symbol_for_no_controls  :=Get_Reserved_Control_Symbol_2(Controller);
   for i in AbstractSpec.InitialCells'Range loop
       Read_Array_of_Cell_Controls(cell          => AbstractSpec.InitialCells(i),
                                           Controller    => Controller,
                                           Control_array => Array_of_controls) ;    
         
               if Array_of_controls=null then 
                return false; 
               else
                	
               For i in Array_of_controls'Range loop                  
                if Array_of_controls(i)= symbol_for_no_controls  then 
               return false;   
           	end if;  
               end loop;
               end if;
 
 end loop;
 return True;

when others => return false; 
 end case;
 
 
end Is_Control_Problem_Solved;

-- --------------------------------
-- Write_Controller ---------------
-- --------------------------------





   
   procedure Write_Controller (P          : in Problem_T;
                               Controller : in Abstract_Controller_Type_Access;
                               Value_Function : in Value_Function_T_access;
                               Abstraction: in Abstraction_T) is
      v : Input_Index_T;
      Cell_Value: Value_T;
      Array_of_controls :Array_of_Controls_for_a_Cell_Access;
 Output : File_Type;
 Explicit_Value_Function : FIle_Type;     
 separator : String := "(";
 use InputGrid;
 use Cell_Cover.BasicGrid;
 min_string_c : chars_ptr ;
 max_string_c : chars_ptr ;
 symbol_for_no_controls:Input_Index_T;
 symbol_for_all_controls:Input_Index_T;
 tmp: Integer;
 function double_to_exact_representation(x : in double) return chars_ptr;
 pragma Import(C,double_to_exact_representation,"double_to_exact_representation");
 
 
begin
 -- Create the new file:
 Create(File => Output,
        Mode => Out_File,
        Name => "Controller.dat");
 -- 1st line is the dimension of the state space (it simplifies the parsing of the file)
 Put(Output,"StateSpaceDimension : ");
 Put_Line(Output,P.State_Space_Dimension'img);
 -- 2nd line is the dimension of the input space (it simplifies the parsing of the file)
 Put(Output,"InputSpaceDimension : ");
 Put_Line(Output,P.Input_Space_Dimension'img);
 -- 3rd line is the operating range
 Put(Output,"OperatingRange : ");
 for i in P.xmin'Range loop
  min_string_c := double_to_exact_representation(double(P.xmin(i)));
  max_string_c := double_to_exact_representation(double(P.xmax(i)));
  declare 
  xmin_string : String := Value(min_string_c);
  xmax_string : String := Value(max_string_c);
  begin
   Put(Output,separator & "[" & xmin_string & "," & xmax_string & "]");
  end;
  Free(min_string_c);
  Free(max_string_c);
  -- Put(Output,separator & "[" & P.xmin(i)'img & "," & P.xmax(i)'img & "]");
  separator := ",";
 end loop;
 Put_Line(Output,")");
 separator := "(";
 Put(Output,"InputSpace : ");
 for i in P.umin'Range loop
  min_string_c := double_to_exact_representation(double(P.umin(i)));
  max_string_c := double_to_exact_representation(double(P.umax(i)));
  declare 
  umin_string : String := Value(min_string_c);
  umax_string : String := Value(max_string_c);
  begin
   Put(Output,separator & "[" & umin_string & "," & umax_string & "]");
  end;
  Free(min_string_c);
  Free(max_string_c);
  -- Put(Output,separator & "[" & P.umin(i)'img & "," & P.umax(i)'img & "]");
  separator := ",";
 end loop;
 Put_Line(Output,")");
 separator := "(";
 -- 4th line is the discretization of the cell grid, 
 -- so, actually, the quantizer is now specified
 Put(Output,"StateSpaceDiscretization : ");
 for i in P.Initial_State_Space_Subdivision'Range loop
  Put(Output,separator & P.Initial_State_Space_Subdivision(i)'img);
  separator := ",";
 end loop;
 Put_Line(Output,")");
 separator := "(";
 Put(Output,"InputSpaceDiscretization : ");
 for i in P.Initial_Input_Space_Subdivision'Range loop
 tmp:=Integer(P.Initial_Input_Space_Subdivision(i))+1;
  Put(Output,separator & tmp'img);
  separator := ",";
 end loop;
 Put_Line(Output,")");
   separator := ",";
Put_line(Output,"Symbol singifying no controls: -");
symbol_for_all_controls:=Get_Reserved_Control_Symbol_1(Controller);
Put_line(Output,"Symbol singifying all controls: "&symbol_for_all_controls'img);

 -- Finally, write the control symbols

      case Controller_Representation(Controller) is   
      
         when Controller_Data_Structure'(Explicit_Array_for_Reachabillity_Problem) =>
         
            for cell in get_first_index(Controller) .. get_last_index(Controller) loop
 
               Read_Cell_Control(cell          => cell,
                                 Controller    => Controller,
                                 Abstraction => Abstraction,
                                 Control_Input => v);      
         
               if v = Get_Reserved_Control_Symbol_2(Controller) then 
                  Put_Line(Output,"-");
               else
                  Put_Line(Output,v'img);
               end if;
            end loop;
            Close(File => Output);
 
         when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_Standard_Synthesis) =>
       
            for cell in get_first_index(Controller) .. get_last_index(Controller) loop
 
               Read_Array_of_Cell_Controls(cell          => cell,
                                           Controller    => Controller,
                                           Control_array => Array_of_controls) ;    
         
               if Array_of_controls=null then 
                  Put(Output,"-");
               else
               
                           symbol_for_no_controls  :=Get_Reserved_Control_Symbol_2(Controller);
                    	For i in Array_of_controls'Range loop                  
                    	if Array_of_controls(i)= symbol_for_no_controls  then 
                  Put(Output,"-");  
           	end if;
           	end loop;
                
                  For i in Array_of_controls'Range loop 
                  if Array_of_controls(i)/= symbol_for_no_controls  then                 
                     Put(Output,Array_of_controls(i)'img);
                     end if;
                  end loop;
                  
                  free_cell_controls(Array_of_controls);
               end if;
               New_Line(Output);
            end loop;
            Close(File => Output);
            
            
            
           when Controller_Data_Structure'(Explicit_Array_for_Safety_Problem_OnTheFly_Synthesis) =>
       
            for cell in get_first_index(Controller) .. get_last_index(Controller) loop
 
               Read_Array_of_Cell_Controls(cell          => cell,
                                           Controller    => Controller,
                                           Control_array => Array_of_controls) ;    
         
               if Array_of_controls=null then 
                  Put(Output,"-");
               else
                 
                    symbol_for_no_controls  :=Get_Reserved_Control_Symbol_2(Controller);
                    	For i in Array_of_controls'Range loop                  
                    	if Array_of_controls(i)= symbol_for_no_controls  then 
                  Put(Output,"-");  
           	end if;
           	
           	end loop;
                 
                  For i in Array_of_controls'Range loop   
                  if Array_of_controls(i)/= symbol_for_no_controls  then                
                     Put(Output,Array_of_controls(i)'img);
                     end if;
                  end loop;
                  
                  free_cell_controls(Array_of_controls);
               end if;
               New_Line(Output);
            end loop;
            Close(File => Output);  
         
         when others =>null;
      end case;   
      
      
             Create(File => Explicit_Value_Function,
                Mode => Out_File,
                Name => "Value_Function.dat");        
    
         case Value_Function_Representation(Value_Function) is
            
            when W_Data_Structure'(Explicit_Array)=>
         
               for cell in get_first_index(Value_Function) .. get_last_index(Value_Function) loop
 
                  Read_Cell_Value(Cell  => Cell,
                                  Value => Cell_Value,
                                  W     => Value_Function) ;   
         
                  if Cell_Value = Value_T'Last then 
                     Put_Line(Explicit_Value_Function,"-");
                  else
                     Put_Line(Explicit_Value_Function,Cell_Value'img);
                  end if;
               end loop;       
            when others => null;
               
         end case;
         
    Close(File => Explicit_Value_Function);
        
         
   exception
      when others => 
         if Is_Open(File => Output) then Close(Output);
         end if;
    if Is_Open(File => Explicit_Value_Function) then Close(Explicit_Value_Function);
         end if;
         
end Write_Controller;

  procedure Write_Empty_Controller is

 Output : File_Type;

  
 function double_to_exact_representation(x : in double) return chars_ptr;
 pragma Import(C,double_to_exact_representation,"double_to_exact_representation");

begin
 -- Create the new file:
 Create(File => Output,
        Mode => Out_File,
        Name => "Controller.dat");
 Close(File => Output);
 exception
 when others => 
  if Is_Open(File => Output) then Close(Output);
  end if;
end Write_Empty_Controller; 
   
   
end output_results;
