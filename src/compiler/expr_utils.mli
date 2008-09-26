open Ast ;;

exception OutsideFragment

module Expr_set :
  sig
    type elt = Ast.expr
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
  end ;;

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
