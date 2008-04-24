open Semantic_checking ;;
open Ast ;;

type validity = Valid | Invalid | Unknown;;

val get_all_info : program -> (Ast.fnDecl * (Basic_paths.path_step list * expr) list) list
val verify_program : (Ast.fnDecl * (Basic_paths.path_step list * expr) list) list -> Ast.program -> ((string, ((validity * Counterexamples.example list option) * float)) Hashtbl.t * Mutex.t) -> validity * (Ast.fnDecl * validity * (Basic_paths.path_step list * expr * validity * Counterexamples.example list option) list) list
val string_of_validity : validity -> string ;;
