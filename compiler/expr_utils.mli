open Ast ;;

val sub_idents_in_expr : expr -> (String.t * expr) list -> expr ;;
val get_idents_of_formals : fnDecl -> string list


