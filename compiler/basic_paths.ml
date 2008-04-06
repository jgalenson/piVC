open Ast
open Expr_utils

exception UnexpectedStatementException
exception NoDeclException
exception BadDeclException

type path_node = 
  | Expr of Ast.expr
  | Assume of Ast.expr
  | Annotation of Ast.expr * string

type closing_scope_action = {
  post_condition: path_node;
  incr: path_node option;
  stmts: stmt list;
}

let type_of_step step = match step with
    Expr(e) -> "expr"
  | Assume(e) -> "assume"
  | Annotation(e,s) -> "annotation"
 
let location_of_path_node node = match node with
    Expr(e) -> Ast.location_of_expr e
  | Assume(e) -> Ast.location_of_expr e
  | Annotation(e,s) -> Ast.location_of_expr e

let string_of_path_node node = match node with
    Expr(exp) -> (Ast.string_of_expr exp)
  | Assume(exp) -> "Assume " ^ (Ast.string_of_expr exp)
  | Annotation(exp, str) -> "@" ^ str ^ ": " ^ (Ast.string_of_expr exp)

let string_of_basic_path path = 
  String.concat "\n" (List.map string_of_path_node path) ;;
    
let print_basic_path path = 
    print_string "---------\n";
    print_endline (string_of_basic_path path) ;;

let print_all_basic_paths paths =
  List.iter print_basic_path paths

let get_not condition = 
  Ast.Not (Ast.get_dummy_location (), condition)

let get_statement_list stmts = 
  match stmts with
      StmtBlock(loc,stmt_list) -> stmt_list
    | _ -> [stmts]

let create_rv_decl t ident =
  {varType = t; varName = ident; location_vd = Ast.get_dummy_location (); var_id = ref (Some(-1));}

let create_rv_expression expr t loc = 

  let rv_ident = (create_identifier "rv" (Ast.get_dummy_location())) in
    rv_ident.decl := Some(create_rv_decl t rv_ident);
    let rv_lval = Ast.NormLval(Ast.get_dummy_location (), rv_ident) in
    let rv_assignment = Ast.Assign(loc, rv_lval, expr) in
      rv_assignment


let gen_func_precondition_with_args_substitution func args =
  let rec get_replacement_list remaining_formals remaining_actuals = match remaining_formals with
      [] -> [] 
    | e :: l -> (List.hd remaining_formals, List.hd remaining_actuals) :: (get_replacement_list (List.tl remaining_formals) (List.tl remaining_actuals))
  in 
  let ident_subs = get_replacement_list (get_idents_of_formals func) args in
    sub_idents_in_expr func.preCondition ident_subs

let gen_func_postcondition_with_rv_substitution func rv_sub =
  let ident_subs = [("rv", rv_sub)] in
    sub_idents_in_expr func.postCondition ident_subs


(* CODE SECTION: GENERATING PATHS *)

let generate_paths_for_func func program = 
  let all_paths : ((path_node list) Queue.t) = Queue.create () in
  let func_pre_condition = Annotation(func.preCondition, "pre") in
  let func_post_condition = Annotation(func.postCondition, "post") in
  let temp_var_number = (ref 0) in
  let generate_nodes_for_expr (curr_path:path_node list) expr = 
    let (new_nodes:path_node list ref) = ref [] in
    let rec gnfe expr =
    match expr with
      Assign (loc,l, e) -> Assign(loc, l, gnfe e)
    | Constant (loc,c) -> expr
    | LValue (loc,l) -> expr
    | Call (loc,s, el) ->
        (   
            match (Ast.get_root_decl program s.name) with 
              None -> raise (NoDeclException)
            | Some(callee_prob) -> (
                match callee_prob with
                    VarDecl(loc, vd) -> raise (BadDeclException)
                  | FnDecl(loc, callee) -> (
                      let ident_name = "_v" ^ string_of_int !temp_var_number in
                      let ident = create_identifier ident_name (get_dummy_location ()) in
                      let decl = create_varDecl callee.returnType ident (Ast.get_dummy_location ()) in
                      let lval_for_new_ident = LValue(loc,NormLval(get_dummy_location (), ident)) in
                        decl.var_id := Some(-temp_var_number.contents - 2); (*rv is -1, so _v0 is -2, _v1 is -3, and so on*)
                        ident.decl := Some(decl);
                        temp_var_number := !temp_var_number + 1;
                        Queue.add (List.append curr_path [Annotation(gen_func_precondition_with_args_substitution callee el,"call-pre")]) all_paths;
                        new_nodes := Assume(gen_func_postcondition_with_rv_substitution callee lval_for_new_ident)::!new_nodes;
                        lval_for_new_ident
                    )
              )
        )
    | Plus (loc,t1, t2) -> Plus(loc, gnfe t1, gnfe t2)
    | Minus (loc,t1, t2) -> Minus(loc, gnfe t1, gnfe t2)
    | Times (loc,t1, t2) -> Times(loc, gnfe t1, gnfe t2)
    | Div (loc,t1, t2) -> Div(loc, gnfe t1, gnfe t2)
    | IDiv (loc,t1, t2) -> IDiv(loc, gnfe t1, gnfe t2)
    | Mod (loc,t1, t2) -> Mod(loc, gnfe t1, gnfe t2)
    | UMinus (loc,t) -> UMinus(loc, gnfe t)
    | ForAll (loc,decls,e) -> ForAll(loc,decls,gnfe e)
    | Exists (loc,decls,e) -> Exists(loc,decls,gnfe e)
    | ArrayUpdate (loc,expr,assign_to,assign_val) -> ArrayUpdate(loc,gnfe expr, gnfe assign_to, gnfe assign_val)
    | LT (loc,t1, t2) -> LT(loc, gnfe t1, gnfe t2)
    | LE (loc,t1, t2) -> LE(loc, gnfe t1, gnfe t2)
    | GT (loc,t1, t2) -> GT(loc, gnfe t1, gnfe t2)
    | GE (loc,t1, t2) -> GE(loc, gnfe t1, gnfe t2)
    | EQ (loc,t1, t2) -> EQ(loc, gnfe t1, gnfe t2)
    | NE (loc,t1, t2) -> NE(loc, gnfe t1, gnfe t2)
    | And (loc,t1, t2) -> And(loc, gnfe t1, gnfe t2)
    | Or (loc,t1, t2) -> Or(loc, gnfe t1, gnfe t2)
    | Not (loc,t) -> Not(loc, gnfe t)
    | Iff (loc,t1, t2) -> Iff(loc, gnfe t1, gnfe t2)
    | Implies (loc,t1, t2) -> Implies(loc, gnfe t1, gnfe t2)
    | Length (loc, t) -> Length(loc, gnfe t)
    | EmptyExpr -> expr
    in
    let new_expr = gnfe expr in
     (new_expr, !new_nodes)

  in 

  let rec generate_path (curr_path:path_node list) stmts (closing_scope_actions:closing_scope_action list) = 

    match List.length stmts with
	0 -> (match List.length closing_scope_actions with
                  0 -> Queue.add (List.append curr_path [func_post_condition]) all_paths (*this means we're not inside a loop, so we just use the function post condition*)
                | _ -> (
                    let closing_scope_action = List.hd closing_scope_actions in
                    match closing_scope_action.incr with
                        None -> Queue.add (List.append curr_path [closing_scope_action.post_condition]) all_paths
                      | Some(incr) -> Queue.add (List.append curr_path [incr;closing_scope_action.post_condition]) all_paths
                  )
	     )
      | _ -> (

    let curr_stmt = List.hd stmts in
    let remaining_stmts = List.tl stmts in
      match curr_stmt with
	  Ast.Expr(loc, exp) -> (
            let (new_exp, new_nodes) = generate_nodes_for_expr curr_path exp in
            generate_path ((curr_path @ new_nodes) @ [(Expr(new_exp))]) remaining_stmts closing_scope_actions
          )

	| Ast.VarDeclStmt(loc, vd) -> generate_path curr_path remaining_stmts closing_scope_actions
	| Ast.IfStmt(loc, condition, ifp, elsep) -> (
	    let remaining_stmts_if_branch = List.append (get_statement_list ifp) remaining_stmts in
	    let remaining_stmts_else_branch = List.append (get_statement_list elsep) remaining_stmts in 
	      generate_path (List.append curr_path [Assume(condition)]) remaining_stmts_if_branch closing_scope_actions;
	      generate_path (List.append curr_path [Assume(get_not condition)]) remaining_stmts_else_branch closing_scope_actions
	  )
        | Ast.WhileStmt(loc, test, block, annotation) -> (
            Queue.add (List.append curr_path [Annotation(annotation,"guard")]) all_paths;
            generate_path (List.append [Annotation(annotation,"guard")] [Assume(test)]) (get_statement_list block) ({post_condition = Annotation(annotation,"guard"); incr = None; stmts = remaining_stmts}::closing_scope_actions);                       
            generate_path (List.append [Annotation(annotation,"guard")] [Assume(get_not test)]) remaining_stmts closing_scope_actions           
          )
        | Ast.ForStmt(loc, init, test, incr, block, annotation) -> (
            Queue.add (List.append curr_path [Expr(init);Annotation(annotation,"guard")]) all_paths;
            generate_path (List.append [Annotation(annotation,"guard")] [Assume(test)]) (get_statement_list block) ({post_condition = Annotation(annotation,"guard"); incr = Some(Expr(incr)); stmts = remaining_stmts}::closing_scope_actions);                       
            generate_path (List.append [Annotation(annotation,"guard")] [Assume(get_not test)]) remaining_stmts closing_scope_actions          
          )
        | Ast.BreakStmt(loc) -> (
	    generate_path curr_path (List.hd closing_scope_actions).stmts (List.tl closing_scope_actions)
              (*Use the statements that follow the scope close.*)
          )
        | Ast.ReturnStmt(loc, exp) -> (
            match func.returnType with
                Void(loc) -> Queue.add (List.append curr_path [func_post_condition]) all_paths
              | _ -> Queue.add (List.append curr_path [Expr (create_rv_expression exp func.returnType loc); func_post_condition]) all_paths
          )
        | Ast.AssertStmt(loc, exp) -> 
            Queue.add (List.append curr_path [Annotation(exp,"assert")]) all_paths;
            generate_path curr_path remaining_stmts closing_scope_actions

        | Ast.StmtBlock(loc, stmts) -> generate_path curr_path (stmts @ remaining_stmts) closing_scope_actions
        | Ast.EmptyStmt -> generate_path curr_path remaining_stmts closing_scope_actions
	)
  in generate_path [func_pre_condition] (get_statement_list func.stmtBlock) [];
    Utils.queue_to_list all_paths

