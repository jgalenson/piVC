open Ast ;;

type path_node = 
  | Expr of Ast.expr
  | Assume of Ast.expr
  | Annotation of Ast.expr * string ;;

val type_of_step : path_node -> string;;
val location_of_path_node : path_node -> Ast.location;;
val string_of_basic_path : path_node list -> string ;;
val string_of_path_node : path_node -> string ;;
val print_basic_path : path_node list -> unit ;;
val generate_paths_for_func : fnDecl -> program -> path_node list list ;;
val print_all_basic_paths : path_node list list -> unit
