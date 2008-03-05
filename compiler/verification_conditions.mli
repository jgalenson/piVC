(* Verification conditions *)

val get_vc : Basic_paths.path_node list -> Ast.expr ;;
val string_of_vc : Ast.expr -> string ;;
val print_vc : Ast.expr -> unit ;;
