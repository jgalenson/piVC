(* ast_utils.mli *)

open Ast ;;

val get_fn_calls : fnDecl -> (expr -> decl option) -> expr list ;;
val get_fndecl_from_call : (expr -> decl option) -> expr -> fnDecl option ;;
val get_loops : fnDecl -> stmt list ;;
val get_loop_ranking_annotation : stmt -> rankingAnnotation option ;;
val calls : fnDecl -> fnDecl -> (expr -> decl option) -> bool ;;
val get_called_function : expr -> identifier ;;
(* TODO: Change this when we add classes to use the call's or fn's class if any. *)
val get_called_fndecl : fnDecl -> program -> expr -> decl option ;;
