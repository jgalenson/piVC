open Semantic_checking ;;
open Ast ;;

val get_all_info : program -> (Ast.fnDecl * (Basic_paths.path_node list * expr) list) list
val verify_program : (Ast.fnDecl * (Basic_paths.path_node list * expr) list) list -> bool * (Ast.fnDecl * bool * (Basic_paths.path_node list * expr * bool) list) list
val goParse : Pervasives.in_channel -> (program option * error list) ;;
val parse_string : string -> (program option * error list) ;;
