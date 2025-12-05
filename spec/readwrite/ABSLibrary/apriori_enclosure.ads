with Established_Types; use Established_Types;
-- @summary
-- This package computes an apriori enclosure for the control system
package Apriori_Enclosure is
procedure Compute(enclosure: in out Compact_Hyperinterval_T; 
                         t : in Time_T; 
                        x0 : in Compact_Hyperinterval_T; 
                         u : in Compact_Hyperinterval_T; 
                         w : in Bounds_of_Dynamic_Uncertainties_T);
-- @description Computes an apriori enclosure K' such that xi([0,t]) subset K' whenever xi(0) in x0;
-- @param enclosure : The enclosure
-- @param t         : The sampling time
-- @param x0        : The set of initial values
-- @param u         : The set of control inputs
-- @param w         : The vector bounding the disturbances
-- Assumptions : All involved arrays have correct length.
procedure Compute(P : in out Problem_T);
-- @description Computes an apriori enclosure for the control system that does not depend on the control symbol.
-- The enclosure is written to P.AprioriEnclosure
-- Assumptions: P has been initialized correctly.
-- @param P The control problem   

   
 procedure Expand_State_Domain(P : in out Problem_T);   
   
end Apriori_Enclosure;
