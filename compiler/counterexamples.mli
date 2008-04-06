(* counterexamples.mli *)

type variable =
  | Var of string
  | ArrayVar of variable * string ;;

type example = variable * string ;;

val parse_counterexamples : string -> (string, Ast.identifier) Hashtbl.t -> example list
val counterexample_to_string : example list -> string ;;
