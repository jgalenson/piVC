(*
 * The scope stack is a data structure for holding string/decl mappings at
 * different levels of scope.
 *)

type t
val create : unit -> t
val enter_scope : t -> unit
val exit_scope : t -> unit
val insert_decl : Ast.decl -> t -> unit
val insert_decl_without_setting_id : Ast.decl -> t -> unit
val lookup_decl : string -> t -> Ast.decl option
val lookup_decl_in_curr_scope_only : string -> t -> Ast.decl option
