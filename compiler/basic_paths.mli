open Ast ;;

type path_step = 
  | Expr of Ast.expr
  | Assume of Ast.expr
  | Annotation of Ast.expr * string
  | RankingAnnotation of Ast.rankingAnnotation ;;

type basic_path =
  | NormalPath of path_step list
  | RuntimeAssertPath of path_step list
  | TerminationPath of path_step list ;;

val type_of_step : path_step -> string;;
val location_of_path_step : path_step -> Ast.location;;
val get_steps_from_path : basic_path -> path_step list ;;
val string_of_path_step : path_step -> string ;;
val print_basic_path : basic_path -> unit ;;
val generate_paths_for_func : fnDecl -> program -> bool -> (basic_path list * basic_path list) ;;
val print_all_basic_paths : basic_path list -> unit ;;
val is_termination_path : basic_path -> bool ;;
