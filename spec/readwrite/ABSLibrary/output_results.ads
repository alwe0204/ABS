with controller_i14sym; use controller_i14sym;
with abstraction_i14sym; use abstraction_i14sym;
with Established_Types; use Established_Types;
with value_function; use value_function;
-- @summary The procedures to output the results of the computation
package output_results is

function Is_Control_Problem_Solved(AbstractSpec  : in Abstract_Specification_T;
                                   Controller    : in Abstract_Controller_Type_Access;
                                   Abstraction   : in Abstraction_T) return Boolean;
-- @param AbstractSpec The abstract specification
-- @param Controller The abstract controller
-- @return Returns True if Controller meets the abstract specification otherwise returns False

   procedure Write_Controller (P          : in Problem_T;
                               Controller : in Abstract_Controller_Type_Access;
                               Value_Function : in Value_Function_T_access;
                               Abstraction: in Abstraction_T);
-- @description This procedure writes the abstract controller and the specification of the quantizer to the file Controller.dat
-- @param P The control problem
-- @param Controller The abstract controller

procedure Write_Empty_Controller ;

end output_results;
