with Established_Types; use Established_Types;

package Bounds_Approximation_Error is

function  Compute( apriori_enclosure    : in Compact_Hyperinterval_T; 
                      sampling_time     : in Time_T  ; 
                  input_value_lower     : in Input_T ; 
                  input_value_upper     : in Input_T ;
                  subdivisions_in_state : in Positive_Integer_T ;
                  subdivisions_in_input : in Positive_Integer_T  ) return Bounds_of_Numerical_Errors_State_T;

procedure Compute(P : in out Problem_T);

   
procedure Set_Errors_to_Zero(P : in out Problem_T);  
   
end Bounds_Approximation_Error;
