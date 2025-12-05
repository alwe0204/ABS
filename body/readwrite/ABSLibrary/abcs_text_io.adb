with Ada.Text_IO; use Ada.Text_IO;

package body abcs_Text_IO is
   procedure Put_Vector ( vec : in Vector_Float_T ) is
      comma : String(1 .. 1) := "(";
   begin
      for i in vec'Range loop
         Put(comma & Float_T'Image(vec(i)));
         comma := ",";
      end loop;
      Put(")");
   end Put_Vector;
   procedure Put_Vector ( vec : in Vector_Positive_Integer_T ) is
      comma : String(1 .. 1) := "(";
   begin
      for i in vec'Range loop
         Put(comma & Positive_Integer_T'Image(vec(i)));
         comma := ",";
      end loop;
      Put(")");
   end Put_Vector;

   procedure Put_Character_Sequence(char : Character;length : Positive) is
   begin
    for i in 1 .. length loop
     Put(char);
    end loop;
    New_Line;
   end Put_Character_Sequence;
end abcs_Text_IO;
