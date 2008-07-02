(* ast_utils.mli *)

open Ast ;;

module Expr_map :
  sig
    type key = Ast.expr
    type 'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end ;;

module Fn_map :
  sig
    type key = Ast.fnDecl
    type 'a t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end ;;

val get_fn_calls : fnDecl -> (expr -> decl option) -> expr list ;;
val get_fndecl_from_call : (expr -> decl option) -> expr -> fnDecl option ;;
val get_loops : fnDecl -> stmt list ;;
val get_loop_ranking_annotation : stmt -> rankingAnnotation option ;;
val calls : fnDecl -> fnDecl -> (expr -> decl option) -> bool ;;
val get_called_function : expr -> identifier ;;
(* TODO: Change this when we add classes to use the call's or fn's class if any. *)
val get_called_fndecl : fnDecl -> program -> expr -> decl option ;;
