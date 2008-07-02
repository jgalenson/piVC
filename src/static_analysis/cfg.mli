(* cfg.mli *)

open Ast ;;

type cfg_expr =
  | NormExpr of expr
  | NormAnnotation of annotation
  | Entry
  | Exit ;;

module Cfg_expr_map :
  sig
    type key = cfg_expr
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

type function_cfg = {
  fn : fnDecl;
  nodes : cfg_expr list;
  entry : cfg_expr;
  exit : cfg_expr;
  predecessor_map : cfg_expr list Cfg_expr_map.t;
  successor_map : cfg_expr list Cfg_expr_map.t;
} ;;

type cfg = {
  functions : function_cfg list;
} ;;

val make_cfg : program -> cfg ;;
val string_of_cfg : cfg -> string ;;
