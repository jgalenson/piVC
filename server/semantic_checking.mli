(*
 * This file does all the semantic checking. It currently prints
 * an error message whenever it finds a problem.
 *)

open Ast

type error_type =
  | SyntaxError
  | SemanticError

type error = {
  e_type : error_type;
  loc  : location;
  msg  : string;
}

val string_of_error : error -> string
val add_error : error_type -> string -> location -> error Queue.t ref -> unit
val check_program : Ast.program -> error Queue.t ref -> unit
val check_and_get_return_type : Scope_stack.t -> Ast.expr -> error Queue.t ref -> Ast.varType