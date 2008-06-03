(* counterexamples.mli *)

type variable =
  | Var of string * Ast.identifier
  | ArrayVar of variable * string
  | Div of string * string ;;

type example = variable * string ;;

val parse_counterexample : string -> (string, Ast.identifier) Hashtbl.t -> example list
val example_to_string : example -> string ;;
val counterexample_to_string : example list -> string ;;
val location_of_example : example -> Ast.location option ;;
