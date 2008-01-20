open Ast ;;

type path_node = 
  | Expr of Ast.expr
  | Assume of Ast.expr
  | Annotation of Ast.expr * string ;;

val print_basic_path : path_node list -> unit ;;
val generate_paths_for_func : fnDecl -> program -> path_node list list ;;
