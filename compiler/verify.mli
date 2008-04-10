open Semantic_checking ;;
open Ast ;;

type validity = Valid | Invalid | Unknown;;

val get_all_info : program -> (Ast.fnDecl * (Basic_paths.path_node list * expr) list) list
val verify_program : (Ast.fnDecl * (Basic_paths.path_node list * expr) list) list -> ((string, (validity * Counterexamples.example list option)) Hashtbl.t * Mutex.t) -> validity * (Ast.fnDecl * validity * (Basic_paths.path_node list * expr * validity * Counterexamples.example list option) list) list
