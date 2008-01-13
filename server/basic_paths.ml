open Ast

type path_node = 
  | Expr of Ast.expr
  | Assume of Ast.expr
  | Annotation of Ast.expr * string

let print_basic_path path = 
  let print_node node = match node with
      Expr(exp) -> print_string ((Ast.string_of_expr exp) ^ "\n") 
    | Assume(exp) -> print_string ("Assume " ^ (Ast.string_of_expr exp) ^ "\n" )
    | Annotation(exp, str) -> print_string ("@" ^ str ^ ": " ^ (Ast.string_of_expr exp) ^ "\n" )
  in
    print_string "---------\n";
    List.iter print_node path

let print_all_basic_paths paths =
  Queue.iter print_basic_path paths

let get_not condition = 
  Ast.Not (Ast.get_dummy_location (), condition)

let get_statement_list stmts = 
  match stmts with
      StmtBlock(loc,stmt_list) -> stmt_list
    | _ -> [stmts]

let create_rv_expression expr = 
  let rv_lval = Ast.NormLval(Ast.get_dummy_location (), {name="rv";location_id=Ast.get_dummy_location ()}) in
  let rv_assignment = Ast.Assign(Ast.location_of_expr expr, rv_lval, expr) in
    rv_assignment


let generate_paths_for_func func = 
  let all_paths : ((path_node list) Queue.t) = Queue.create () in
  let func_pre_condition = Annotation(func.preCondition, "pre") in
  let func_post_condition = Annotation(func.postCondition, "post") in    

  let rec generate_path (curr_path:path_node list) stmts loop_post_condition = 

    match List.length stmts with
	0 -> (match loop_post_condition with
                  None -> Queue.add (List.append curr_path [func_post_condition]) all_paths
                | Some(condition) -> Queue.add (List.append curr_path [condition]) all_paths
	     )
      | _ -> (

    let curr_stmt = List.hd stmts in
    let remaining_stmts = List.tl stmts in
      match curr_stmt with
	  Ast.Expr(loc, exp) -> generate_path (List.append curr_path [Expr(exp)]) remaining_stmts loop_post_condition
	| Ast.VarDeclStmt(loc, vd) -> generate_path curr_path remaining_stmts loop_post_condition
	| Ast.IfStmt(loc, condition, ifp, elsep) -> (
	    let remaining_stmts_if_branch = List.append (get_statement_list ifp) remaining_stmts in
	    let remaining_stmts_else_branch = List.append (get_statement_list elsep) remaining_stmts in 
	      generate_path (List.append curr_path [Assume(condition)]) remaining_stmts_if_branch loop_post_condition;
	      generate_path (List.append curr_path [Assume(get_not condition)]) remaining_stmts_else_branch loop_post_condition
	  )
        | Ast.ReturnStmt(loc, exp) -> (
	    Queue.add (List.append curr_path [Expr (create_rv_expression exp); func_post_condition]) all_paths
          )
        | _ -> print_string "J: NOT YET IMPLEMENTED"
	)
  in generate_path [func_pre_condition] (get_statement_list func.stmtBlock) None;
    all_paths

