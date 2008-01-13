(*
 * This file does all the semantic checking. It currently prints
 * an error message whenever it finds a problem.
 *)

val check_program : Ast.program -> unit
val check_and_get_return_type : Scope_stack.t -> Ast.expr -> Ast.varType