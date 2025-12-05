with grids;
with Established_Types; use Established_Types;
package Input_Values is
package InputGrid is new grids(Float_Type           => Float_T,
                               Dimension_Index_Type => Component_Index_T,
                               Vector_Type          => Input_T,
                               Vector_Type_Access   => Input_T_Access);
subtype Input_Index_T is InputGrid.Grid_Index_Type;
-- The type of the input index
type Input_Grid_T is new InputGrid.Grid_Record with null record;
-- The type of set of input values

subtype Input_Coord_T is InputGrid.Vector_of_Coordinate_Index;

function Install(P : Problem_T) return Input_Grid_T;
-- @return An instance of a discrete set of input values;
-- @param P The control problem
-- Assumptions. P has been initialized correctly.
function Install(P : Problem_T_Access) return Input_Grid_T;

function Get_First_Index(Inputs : in Input_Grid_T) return Input_Index_T;
-- @return Returns the first index used to identify an input value;
function Get_Last_Index(Inputs : in Input_Grid_T) return Input_Index_T;
-- @return Returns the last index used to identify an input value;


procedure Get_Input_Value(u         : out Input_T; 
                          idx       : in Input_Index_T; 
                          InputData : in Input_Grid_T);
                          
                          
procedure Get_Input_Coord (coord_u        : out Input_Coord_T; 
                          idx             : in Input_Index_T; 
                          InputData       : in Input_Grid_T);
                          
                          procedure Get_Input_Index (coord_u        : in Input_Coord_T; 
                          idx             : out Input_Index_T; 
                          InputData       : in Input_Grid_T);
end Input_Values;
