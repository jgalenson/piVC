open Ast ;;

type path_step = 
  | Expr of Ast.expr
  | Assume of Ast.expr
  | Annotation of Ast.expr * string ;;

val type_of_step : path_step -> string;;
val location_of_path_step : path_step -> Ast.location;;
val string_of_basic_path : path_step list -> string ;;
val string_of_path_step : path_step -> string ;;
val print_basic_path : path_step list -> unit ;;
val generate_paths_for_func : fnDecl -> program -> path_step list list ;;
val print_all_basic_paths : path_step list list -> unit
