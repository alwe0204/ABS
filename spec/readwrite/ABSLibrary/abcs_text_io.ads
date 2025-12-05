-- @summary 
-- Some functions to print certain objects on screen.
with Established_Types; use Established_Types;
package abcs_Text_IO is
   procedure Put_Vector ( vec : in Vector_Float_T );
   procedure Put_Vector ( vec : in Vector_Positive_Integer_T );
   -- Prints a vector on screen.
   --@param vec The vector to print
   procedure Put_Character_Sequence(char : Character;length : Positive);
end abcs_Text_IO;
