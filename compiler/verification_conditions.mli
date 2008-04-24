(* Verification conditions *)

val get_vc : Basic_paths.path_step list -> Ast.expr ;;
val string_of_vc : Ast.expr -> string ;;
val print_vc : Ast.expr -> unit ;;

(*Returns A -> expr where A is a set of conjuncts saying |x|>=0
  for every x such that |x| is in expr*)
val add_array_length_greater_than_0_to_expr : Ast.expr -> Ast.expr;;
