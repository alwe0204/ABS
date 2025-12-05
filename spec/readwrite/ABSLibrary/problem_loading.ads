with Established_Types; use Established_Types;
-- @summary 
-- This package writes the data obtained from dynamics_specification_parameters to 
-- a data structure of type Problem_Specification;
package Problem_Loading is
function install return Problem_T;
-- @return This function returns the filled data structure
end Problem_Loading;
