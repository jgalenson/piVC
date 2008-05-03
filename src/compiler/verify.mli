open Semantic_checking ;;
open Ast ;;

type validity = Valid | Invalid | Unknown;;

type termination_result = {
  overall_validity : validity;
  decreasing_paths : validity * (Basic_paths.basic_path * Ast.expr * validity * Counterexamples.example list option) list;
  nonnegative_vcs : validity * (Ast.expr * validity * Counterexamples.example list option) list;
} ;;

val get_all_info : program -> bool -> (Ast.fnDecl * (Basic_paths.basic_path * expr) list * (Basic_paths.basic_path * expr) list * expr list) list
val verify_program : (Ast.fnDecl * (Basic_paths.basic_path * expr) list  * (Basic_paths.basic_path * expr) list * expr list) list -> Ast.program -> ((string, ((validity * Counterexamples.example list option) * float)) Hashtbl.t * Mutex.t) -> validity * (Ast.fnDecl * validity * (validity * (Basic_paths.basic_path * expr * validity * Counterexamples.example list option) list) * termination_result option) list
val string_of_validity : validity -> string ;;
