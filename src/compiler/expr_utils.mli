open Ast ;;

val nnf : expr -> expr
val sub_idents_in_expr_while_preserving_original_location : expr -> (identifier * expr) list -> expr ;;
val sub_idents : expr -> (identifier -> expr option) -> expr ;;
val get_idents_of_formals : fnDecl -> identifier list
val array_ident_from_lval : lval -> identifier
val array_write_to_array_update : expr -> expr -> expr
val remove_quantification_from_vc_with_array_dp : expr -> expr
val get_index_set : expr -> expr list
val guaranteed_unique_string_of_expr : expr -> string ;;
val conjuncts_of_exprs : expr list -> expr;;
val remove_duplicates_from_list : expr list -> expr list;;
val get_calls : expr -> expr list ;;
