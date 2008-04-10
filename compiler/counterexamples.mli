(* counterexamples.mli *)

type variable =
  | Var of string * Ast.identifier
  | ArrayVar of variable * string ;;

type example = variable * string ;;

val parse_counterexamples : string -> (string, Ast.identifier) Hashtbl.t -> example list
val example_to_string : example -> string ;;
val counterexamples_to_string : example list -> string ;;
val location_of_example : example -> Ast.location ;;
