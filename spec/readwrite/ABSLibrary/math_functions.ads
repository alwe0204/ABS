with Interfaces.C; use Interfaces.C;
with Established_Types; use Established_Types;
with Ada.Numerics.Generic_Elementary_Functions;

package math_functions is
package GenElFun_64 is new Ada.Numerics.Generic_Elementary_Functions(Float_T); -- only for square root
   function ABSLib_atan (x : in Float_T) return Float_T;
   function ABSLib_cos  (x : in Float_T) return Float_T;
   function ABSLib_cosh (x : in Float_T) return Float_T;
   function ABSLib_exp  (x : in Float_T) return Float_T;
   function ABSLib_log  (x : in Float_T) return Float_T;
   function ABSLib_sin  (x : in Float_T) return Float_T;
   function ABSLib_sinh (x : in Float_T) return Float_T;	   
   function ABSLib_sqrt (x : in Float_T) return Float_T;
   function ABSLib_tan  (x : in Float_T) return Float_T;
end math_functions;
