open Ast ;;

val nnf : expr -> expr
val sub_idents_in_expr : expr -> (String.t * expr) list -> expr ;;
val get_idents_of_formals : fnDecl -> string list
val array_name_from_lval : lval -> string option
val array_write_to_array_update : expr -> expr -> expr
