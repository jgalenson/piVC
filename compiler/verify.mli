open Semantic_checking ;;
open Ast ;;

type validity = Valid | Invalid | Unknown;;

val get_all_info : program -> (Ast.fnDecl * (Basic_paths.path_node list * expr) list) list
val verify_program : (Ast.fnDecl * (Basic_paths.path_node list * expr) list) list -> validity * (Ast.fnDecl * validity * (Basic_paths.path_node list * expr * validity * Counterexamples.example list option) list) list
