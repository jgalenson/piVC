open Ast

exception UnexpectedStatementException
exception CantReplaceLValueWithExpr
exception NoDeclException
exception BadDeclException
exception NotLValueExpr

type path_node = 
  | Expr of Ast.expr
  | Assume of Ast.expr
  | Annotation of Ast.expr * string

type closing_scope_action = {
  post_condition: path_node;
  incr: path_node option;
  stmts: stmt list;
}

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

(* CODE SECTION: SUBSTITUTING VARIABLE NAMES IN EXPRS *)

let ident_name_of_lval lval = match lval with
    NormLval(loc, id) -> id.name
  | ArrayLval(loc, id, expr) -> id.name

let get_match_sub lval ident_subs = 
  let sub_to_return = ref None in
  let replacement_func possibility = match (String.compare (ident_name_of_lval lval) (fst possibility)) with
      0 -> sub_to_return := Some(possibility)
    | _ -> ignore ()
  in
    List.iter replacement_func ident_subs;
    !sub_to_return

let sub_idents_in_lval lval ident_subs = match get_match_sub lval ident_subs with
    None -> lval
  | Some(sub) -> (
      match snd sub with
          LValue(loc, l) -> l
        | _ -> (raise (CantReplaceLValueWithExpr))
    )

let sub_idents_in_lval_expr lval_expr ident_subs = match lval_expr with
  LValue(loc, l) -> (
    match get_match_sub l ident_subs with
        None -> lval_expr
      | Some(sub) -> snd sub
  )
  | _ -> (raise NotLValueExpr)


 
(* substitutes variable names, but does not substitue function/predicate names *)
let rec sub_idents_in_expr expr ident_subs = 
    match expr with
    | Assign (loc,l, e) -> Assign(loc, sub_idents_in_lval l ident_subs, sub_idents_in_expr e ident_subs)
    | Constant (loc,c) -> expr
    | LValue (loc,l) -> sub_idents_in_lval_expr expr ident_subs
    | Call (loc,s, el) -> Call(loc, s, sub_idents_in_expr_list el ident_subs)
    | Plus (loc,t1, t2) -> Plus(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Minus (loc,t1, t2) -> Minus(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Times (loc,t1, t2) -> Times(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Div (loc,t1, t2) -> Div(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | IDiv (loc,t1, t2) -> IDiv(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Mod (loc,t1, t2) -> Mod(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | UMinus (loc,t) -> UMinus(loc, sub_idents_in_expr t ident_subs)
    | LT (loc,t1, t2) -> LT(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | LE (loc,t1, t2) -> LE(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | GT (loc,t1, t2) -> GT(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | GE (loc,t1, t2) -> GE(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | EQ (loc,t1, t2) -> EQ(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | NE (loc,t1, t2) -> NE(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | And (loc,t1, t2) -> And(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Or (loc,t1, t2) -> Or(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Not (loc,t) -> Not(loc, sub_idents_in_expr t ident_subs)
    | Iff (loc,t1, t2) -> Iff(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Implies (loc,t1, t2) -> Implies(loc, sub_idents_in_expr t1 ident_subs, sub_idents_in_expr t2 ident_subs)
    | Length (loc, t) -> Length(loc, sub_idents_in_expr t ident_subs)
    | EmptyExpr  -> expr

and sub_idents_in_expr_list expr_list ident_subs = 
  match expr_list with 
      [] -> []
    | e::l -> sub_idents_in_expr e ident_subs :: sub_idents_in_expr_list l ident_subs



let get_idents_of_formals func = 
  let rec build_ident_list remaining = 
    match remaining with
        [] -> []
      | e :: l -> e.varName.name :: build_ident_list l
  in 
    build_ident_list func.formals

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
      Assign (loc,l, e) -> expr
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
                      let lval_for_new_ident = LValue(loc,NormLval(get_dummy_location (), create_identifier ident_name (get_dummy_location ()))) in
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
	    Queue.add (List.append curr_path [Expr (create_rv_expression exp); func_post_condition]) all_paths
          )
        | Ast.AssertStmt(loc, exp) -> 
            Queue.add (List.append curr_path [Annotation(exp,"assert")]) all_paths;
            generate_path curr_path remaining_stmts closing_scope_actions

        | Ast.StmtBlock(loc, stmts) -> raise (UnexpectedStatementException)
        | Ast.EmptyStmt -> generate_path curr_path remaining_stmts closing_scope_actions
	)
  in generate_path [func_pre_condition] (get_statement_list func.stmtBlock) [];
    all_paths

