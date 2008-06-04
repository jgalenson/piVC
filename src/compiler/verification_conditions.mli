(* Verification conditions *)

val get_vc : Basic_paths.basic_path -> Ast.expr ;;
val string_of_vc : Ast.expr -> string ;;
val print_vc : Ast.expr -> unit ;;

(*Returns A -> expr where A is a set of conjuncts saying |x|>=0
  for every x such that |x| is in expr*)
val add_array_length_greater_than_0_to_expr : Ast.expr -> Ast.expr;;
(* VCs can't have length exprs, so this removes them.
   If you want to replace a whole path at once, use
   convert_basic_path_to_acceptable_form. *)
val replace_length_with_var : Ast.expr -> Ast.expr ;;
