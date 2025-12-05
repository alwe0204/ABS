with controller_i14sym; use controller_i14sym;
with abstraction_i14sym;
with Input_Values; use Input_Values;
with cell_cover; use cell_cover;

with Set_of_Settled_States_E; use Set_of_Settled_States_E;
with cell_heap; use cell_heap;
with value_function; use value_function;

--@summary Implementation of symbolic solution methods
generic
   type Transition_Weight_T is digits <>;



package dijkstra_algorithm_i13absoc is


--type for solution of a control problem
type Abstract_OCP_Solution_Type is record
 Controller    : Abstract_Controller_Type_Access;
 Value_Function: Value_Function_T_access;
end record;



   generic

      with procedure backward_estimator(
                                        successors : in out Dynamic_Cell_Container;
                                        cell       : in Cell_Index_T;
                                        v          : in Input_Index_T
                                       ) ;
-- @description This procedure evaluates backward estimator
-- @param successors a set of cells
-- @param cell index of a cell for which backward estimator has to be computed
-- @param v  index of an input


      with procedure Backward_Reachable_Set(
                                        successors : in out Dynamic_Cell_Container;
                                        cell       : in Cell_Index_T;
                                        v          : in Input_Index_T
                                            ) ;
-- @description This procedure evaluates backward reachable set
-- @param successors a set of cells
-- @param cell index of a cell for which backward estimator has to be computed
-- @param v  index of an input


      with function Belongs_to_Abstract_Reachable_Set (
                                                       cell              : in Cell_Index_T;
                                                       overapproximation : in Abstract_Reachable_Set_Geometry_T
                                                      ) return Boolean;
-- @description This procedure checks if a cell belongs to the abstract reachable set
-- @param cell index of a cell 
-- @param overapproximation reachable set description


      with
        function Abstract_Reachable_Set_Traversal (
                                                   cell               : in  Cell_Index_T;
                                                   v                  : in  Input_Index_T;
                                                   successor          : out Cell_Index_T;
                                                   overapproximation  : out Abstract_Reachable_Set_Geometry_T;
                                                   Test_procedure_not_in_E:
                                                             access function (cell: in Cell_Index_T;successor : in Cell_Index_T) return Boolean;
                                                   Test_procedure_obstacle_suboptimal_or_self_loop:
                                                             access function (cell: in Cell_Index_T;successor : in Cell_Index_T) return Boolean
                                                  ) return Boolean;
-- @description This function traverses the reachable set  and applies functions to each sell member, outputs a cell index successor where traversal stopped
-- @return True if the reachable set has been traversed, false otherwise
-- @param cell index of a cell from which the reachable set has been computed
-- @param v input index
-- @param successor index of a cell from the overapproximation where traversal stopped. Assumes value Cell_Index_T'last if traversal further not needed
-- @param overapproximation reachable set description
-- @param Test_procedure_not_in_E an access to a  function returning boolean value 
-- @param Test_procedure_obstacle_suboptimal_or_self_loop an access to a  function returning boolean value


      with

        function Continue_Abstract_Reachable_Set_Traversal (
                                                            cell              : in  Cell_Index_T;
                                                            current_successor : in  Cell_Index_T;
                                                            next_successor    : out Cell_Index_T;
                                                            overapproximation : in  Abstract_Reachable_Set_Geometry_T;
                                                            Test_procedure_not_in_E:
                                                                       access function (cell: in Cell_Index_T;successor : in Cell_Index_T) return Boolean;
                                                            Test_procedure_obstacle_suboptimal_or_self_loop:
                                                                       access function (cell: in Cell_Index_T;successor : in Cell_Index_T) return Boolean

                                                           ) return Boolean;


-- @description This function traverses the reachable set starting from cell current_successor and applies functions to each sell member, outputs a cell index next_successor where traversal stopped.
-- @return True if the reachable set has been traversed, false otherwise
-- @param cell index of a cell from which the reachable set has been computed
-- @param v input index
-- @param current_successor index of a cell from the overapproximation
-- @param next_successor index of a cell from the overapproximation. Assumes value Cell_Index_T'last if traversal further not needed
-- @param overapproximation reachable set description
-- @param Test_procedure_not_in_E an access to a  function returning boolean value 
-- @param Test_procedure_obstacle_suboptimal_or_self_loop an access to a  function returning boolean value



procedure Compute(Solution    : in out Abstract_OCP_Solution_Type;
                  Abstraction : in out abstraction_i14sym.Abstraction_T;
                  AbstractSpec: in abstraction_i14sym.Abstract_Specification_T );
-- @description This procedure solves the underlying optimal control problem
-- @param Solution The solution (control law and value function) of the OCP
-- @param Abstraction
-- @param AbstractSpec






end dijkstra_algorithm_i13absoc;
