(* smt_solver.mli *)

exception SolverError of string
exception NonLinearProblem
exception SolverTimeout

module Counterexample :
  sig
    type variable =
      | Var of string * Ast.identifier
      | ArrayVar of variable * string
      | Div of string * string
      | Mod of string * string

    type example = variable * string
    
    val example_to_string : example -> string
    val counterexample_to_string : example list -> string
    val location_of_example : example -> Ast.location option
  end ;;

type inputMethod = Stdin | File of string ;;

val get_input_method : unit -> inputMethod ;;
val transform_input : Ast.expr -> string * (string, Ast.identifier) Hashtbl.t ;;
val parse_counterexample : string -> (string, Ast.identifier) Hashtbl.t -> Counterexample.example list ;;
val parse_output : (unit -> string) -> string * string option ;;
val parse_error : (unit -> string) -> string ;;
val get_arguments : unit -> string array ;;
val get_shutdown_command : unit -> string option ;;
