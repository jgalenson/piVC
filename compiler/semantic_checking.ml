open Ast

exception SemanticCheckingError of string;;

type error_type =
  | SyntaxError
  | SemanticError ;;

type error = {
  e_type : error_type;
  loc  : location;
  msg  : string;
};;



let get_type_error_msg expr_name given expected =
  expr_name ^ " expr type is '" ^ (string_of_type given) ^ "' but should be " ^ expected ;;

let get_non_int_index_error_msg expr =
  "Array index '" ^ (string_of_expr expr) ^ "' must be an integer" ;;

let string_of_error_type t = match t with
  | SyntaxError -> "Syntax Error"
  | SemanticError -> "Semantic Error" ;;

let string_of_error_type_for_xml t = match t with
  | SyntaxError -> "syntax_error"
  | SemanticError -> "semantic_error" ;;

let string_of_error_msg e = match e.e_type with
  | SyntaxError -> "Error on the following token: " ^ e.msg
  | SemanticError -> e.msg

let string_of_error e =
  ("**" ^ (string_of_error_type e.e_type) ^ "**") ^ "\n"
  ^ (string_of_error_msg e) ^ "\n" ^ (string_of_location e.loc) ^ "\n";;

let add_error e_type msg loc errors =
  Queue.add { e_type=e_type; msg=msg; loc=loc } !errors;;

let insert_decl s errors decl = 
  let curr = Scope_stack.lookup_decl_in_curr_scope_only (Ast.name_of_decl decl) s in
    match curr with
	None -> Scope_stack.insert_decl decl s
      | Some (d) ->
	  let error_msg = "A declaration '" ^ name_of_decl d ^ "' is already defined" in
	  add_error SemanticError error_msg (location_of_decl decl) errors

let insert_var_decl s errors decl = 
  insert_decl s errors (Ast.VarDecl(decl.location_vd, decl))

let insert_decls s errors decls = 
  List.iter (insert_decl s errors) decls

let insert_var_decls s errors decls = 
  List.iter (insert_var_decl s errors) decls 

(* Functions on types *)
let rec types_equal t1 t2 = match (t1, t2) with
  | (Bool(loc1), Bool(loc2)) -> true
  | (Int(loc1), Int(loc2)) -> true
  | (Float(loc1), Float(loc2)) -> true
  | (Array(aType1, loc1), Array(aType2, loc2)) -> (types_equal aType1 aType2)
  | (Void(loc1), Void(loc2)) -> true
  | (Identifier(t1, loc1), Identifier(t2, loc2)) -> true (* TODO finish off *)
  | (ErrorType, _) -> true
  | (_, ErrorType) -> true
  | (_, _) -> false

and is_numeric_type t1 = match t1 with
  | Int (loc) -> true
  | Float (loc) -> true
  | ErrorType -> true (* So cascading errors work *)
  | Identifier (t, loc) -> true (* TODO finish off like above *)
  | _ -> false

and is_error_type t1 = match t1 with
  | ErrorType -> true
  | _ -> false
	
(* Is this too hacky? *)
and is_boolean_type t1 loc = let b = Bool(loc) in (types_equal t1 b)

and is_array_type t = match t with
  | Array(atype, loc) -> true
  | _ -> false
  

let annotate_ident ident s = 
  let lookupResult = Scope_stack.lookup_decl ident.name s in
    match lookupResult with
        None -> ignore () (*This probably means that the ident wasn't declared, but this will be reported elsewhere.*)
      | Some(decl) ->
          begin
            let vd = (varDecl_of_decl decl) in
              ident.decl := Some(vd)
          end
          
(* CAGRT *)
	
let rec check_and_get_return_type_lval (is_annotation, is_ranking_fn) s lval errors = match lval with
  | NormLval(loc, id) ->
      annotate_ident id s;
      let lookupResult = Scope_stack.lookup_decl id.name s in
      begin
	match lookupResult with
          | None ->
	      let error_msg = "Identifier '" ^ (string_of_identifier id) ^ "' not defined" in
	      add_error SemanticError error_msg  loc errors; ErrorType
	  | Some(decl) -> type_of_decl decl
      end;

  | ArrayLval(loc, arr, index) ->
      let typeOfIndex = check_and_get_return_type s index errors (is_annotation, is_ranking_fn, false) in
        begin
          match typeOfIndex with
            | Int(l) -> ()
            | _ ->
	        let error_msg = get_non_int_index_error_msg index in
		  add_error SemanticError error_msg loc errors
        end;
        let typeOfArray = check_and_get_return_type s arr errors (is_annotation, is_ranking_fn, false) in
          match typeOfArray with
              Array(t,loc) -> t
            | _ ->
                begin
                  let error_msg = "'" ^ (string_of_expr arr) ^ "' is of type '" ^ string_of_type typeOfArray ^ "' but should be an array" in
                    add_error SemanticError error_msg loc errors; ErrorType
                end

(*
        let lookupResult = (Scope_stack.lookup_decl arr.name s) in
        begin
	  match lookupResult with
           | None ->
	       let error_msg = "Identifier '" ^ (string_of_identifier arr) ^ "' not defined" in
	       add_error SemanticError error_msg loc errors; ErrorType
           | Some(decl) -> let typeOfArray = type_of_decl decl in
	     begin
               match typeOfArray with
                 | Array(t, l) -> t
                 | _ ->
		     let error_msg = "'" ^ (string_of_identifier arr) ^ "' is not an array" in
		     add_error SemanticError error_msg loc errors; ErrorType
             end
        end
*)
      
 
	
and check_for_same_type t1 t2 loc errors = 
  if not (types_equal t1 t2) then
    begin
      let error_msg = "LHS '" ^ (string_of_type t1) ^ "' and RHS '" ^ (string_of_type t2) ^ "' are of different types" in
      add_error SemanticError error_msg loc errors;
      false
    end
  else
    true

and check_and_get_return_type scope_stack e errors (is_annotation, is_ranking_fn, is_top_level) =
  
  let check_lhs_of_assign lval_orig = 
    let rec cloa lval = 
      match lval with
          NormLval(loc,id) ->
            begin
              match Scope_stack.lookup_decl id.name scope_stack with
                | Some(decl) ->
                    begin
                      match decl with 
                          VarDecl(loc2,vd) -> if(vd.is_param) then add_error SemanticError "Updates to parameters not permitted."  loc errors
                        | _ -> ignore() (*this is an error, but it'll be caught in cagrt*)
                    end
                | None -> ignore() (*this is an error, but it'll be caught in cagrt*)
            end
        | ArrayLval(loc,arr,index) -> 
            begin
              let rec check_lhs_expr_of_assign exp = 
                match exp with
                    LValue(loc,l) -> cloa l
                  | ArrayUpdate(loc,arr,index,value) -> check_lhs_expr_of_assign arr
                  | _ -> add_error SemanticError "Illegal LHS of assignment."  (location_of_lval lval_orig) errors
              in
                check_lhs_expr_of_assign arr
            end
    in cloa lval_orig
  in
  let rec check_and_get_return_type_relational loc t1 t2 =
    let lhsType = cagrt t1
    and rhsType = cagrt t2 in
    ignore (check_for_same_type lhsType rhsType loc errors);
    if not (is_numeric_type lhsType) then
      add_error SemanticError (get_type_error_msg "Relational" lhsType "numeric") loc errors;
    if not (is_numeric_type rhsType) then
      add_error SemanticError (get_type_error_msg "Relational" rhsType "numeric") loc errors;    
    Bool(loc)
      
  and check_and_get_return_type_equality loc t1 t2 =
    let lhsType = cagrt t1
    and rhsType = cagrt t2 in
    ignore (check_for_same_type lhsType rhsType loc errors);
    Bool(loc)

  and check_and_get_return_type_logical loc t1 t2 =
    let lhsType = cagrt t1
    and rhsType = cagrt t2 in
    if not (is_boolean_type lhsType loc) then
      add_error SemanticError (get_type_error_msg "Logical" lhsType "boolean") loc errors;
    if not (is_boolean_type rhsType loc) then
      add_error SemanticError (get_type_error_msg "Logical" rhsType "boolean") loc errors;
    Bool(loc)
      
  and check_and_get_return_type_arithmetic loc t1 t2 =
    let leftType = cagrt t1
    and rightType = cagrt t2 in
    let are_same_type = check_for_same_type leftType rightType loc errors in
    if not (is_numeric_type leftType) then
      begin
	add_error SemanticError (get_type_error_msg "Arithmetic" leftType "numeric") loc errors;
	ErrorType
      end
    else if not (is_numeric_type rightType) then
      begin
	add_error SemanticError (get_type_error_msg "Arithmetic" rightType "numeric") loc errors;
	ErrorType
      end	
    else if not are_same_type then
      ErrorType
    else
      rightType

  and check_and_get_return_type_quantifier loc decls expr = 
    begin
      match is_annotation with
          true ->
            begin
              Scope_stack.enter_scope scope_stack;
              let insert_decl_into_scope_stack decl = Scope_stack.insert_decl (Ast.VarDecl(decl.location_vd, decl)) scope_stack in
              List.iter insert_decl_into_scope_stack decls;
              ignore(cagrt expr);
              Scope_stack.exit_scope scope_stack                
            end
        | false -> (add_error SemanticError "Quantifier outside of annotation not permitted" loc errors)
    end;
    Bool(get_dummy_location ())

  and cagrt_full e (is_top_level) = match e with
    | Assign (loc,l,e) ->
	if (is_annotation || is_ranking_fn) then
	  begin
	    let error_msg = "Assign expr inside an annotation." in
            add_error SemanticError error_msg loc errors
	  end
	else if (not is_top_level) then
	  begin
	    let error_msg = "Assign expr not at the root of a statement." in
            add_error SemanticError error_msg loc errors
	  end;
        let lhsType = check_and_get_return_type_lval (is_annotation, is_ranking_fn) scope_stack l errors in
        let rhsType = cagrt e in
          ignore (check_lhs_of_assign l);
	  ignore (check_for_same_type lhsType rhsType loc errors);
          lhsType
    | Constant (loc,c) ->
	begin
	  match c with
	  | ConstInt (l, i) -> Int (loc)
	  | ConstFloat (l, f) -> Float (loc)
	  | ConstBool (l, b) -> Bool (loc)
	end
    | LValue (loc,l) -> check_and_get_return_type_lval (is_annotation, is_ranking_fn) scope_stack l errors
    | Call (loc, ident, ac) -> (* TODO: Check this more? *)
        begin
          let word = match is_annotation with
              true -> "predicate"
            | false -> "function"
          in
          let check_formals formals = 
	    if List.length ac != List.length formals then
	      begin
		let error_msg = "Incorrect number of arguments: expected " ^ (string_of_int (List.length formals)) ^ ", given: " ^ (string_of_int (List.length ac)) in
		  add_error SemanticError error_msg loc errors
	      end
	    else
	      let check_formal given expected =
		let given_type = cagrt given
		and expected_type = expected.varType in
                  match types_equal given_type expected_type with
                      true -> ignore ()
                    | false ->
		        let error_msg = "Incorrect type of argument. Given " ^ string_of_type given_type ^ ", expected " ^ string_of_type expected_type in
		          add_error SemanticError error_msg (location_of_expr given) errors
	      in
		List.iter2 check_formal ac formals
          in
	  let lookup_result = (Scope_stack.lookup_decl ident.name scope_stack) in
	    match lookup_result with
	      | None ->
		  let error_msg = "Undefined " ^ word ^ " '" ^ (string_of_identifier ident) ^ "'" in
		    add_error SemanticError error_msg loc errors; ErrorType
	      | Some (d) ->
                  begin
                    match d with
		      | VarDecl (l, vd) ->
		          let error_msg = "'" ^ (string_of_identifier vd.varName) ^ "' is not a " ^ word in
		            add_error SemanticError error_msg loc errors; ErrorType
		      | Predicate (l, p) ->
                          begin
                            match is_annotation with
                                true -> check_formals p.formals_p; Bool(gdl())
		              | false -> let error_msg = "'" ^ (string_of_identifier p.predName) ^ "' is not a function" in
		                  add_error SemanticError error_msg loc errors; ErrorType
                          end
		      | FnDecl (loc, fd) -> 
                          begin
                            match is_annotation with
                                true -> let error_msg = "'" ^ (string_of_identifier fd.fnName) ^ "' is not a predicate" in
		                  add_error SemanticError error_msg loc errors; ErrorType
		              | false -> check_formals fd.formals; fd.returnType
                          end
	          end          
        end
    | Plus (loc,t1, t2) -> check_and_get_return_type_arithmetic loc t1 t2
    | Minus (loc,t1, t2) -> check_and_get_return_type_arithmetic loc t1 t2
    | Times (loc,t1, t2) -> check_and_get_return_type_arithmetic loc t1 t2
    | Div (loc,t1, t2) -> check_and_get_return_type_arithmetic loc t1 t2
    | IDiv (loc,t1, t2) -> check_and_get_return_type_arithmetic loc t1 t2
    | Mod (loc,t1, t2) -> check_and_get_return_type_arithmetic loc t1 t2
    | UMinus (loc,t) ->
	let rtype = cagrt t in
	if not (is_numeric_type rtype) then
	  begin
	    (add_error SemanticError "Unary minus type is not numeric" loc) errors;
	     ErrorType
	  end
	else
	  rtype
    | ForAll (loc,i,e) -> check_and_get_return_type_quantifier loc i e
    | Exists (loc,i,e) -> check_and_get_return_type_quantifier loc i e
    | ArrayUpdate (loc, expr, assign_to, assign_val) -> 
        begin
          let expr_type = cagrt expr in
          let assign_to_type = cagrt assign_to in
          let assign_val_type = cagrt assign_val in
            begin
              match expr_type with
                  Array(t,loc) ->
                    begin
                      match types_equal t assign_val_type with
                          true -> ignore()
                        | false -> add_error SemanticError (get_type_error_msg (string_of_expr assign_val) assign_val_type (string_of_type t)) (location_of_expr assign_val) errors
                          
                    end
                | ErrorType -> ignore ()
                | _ -> add_error SemanticError "Array update applied to non-array" loc errors
            end;
            begin
              match assign_to_type with
                  Int(loc) -> ignore()
                | _ -> add_error SemanticError (get_non_int_index_error_msg assign_to) (location_of_expr assign_to) errors
            end;              
            expr_type
        end
    | LT (loc,t1, t2) -> check_and_get_return_type_relational loc t1 t2
    | LE (loc,t1, t2) -> check_and_get_return_type_relational loc t1 t2
    | GT (loc,t1, t2) -> check_and_get_return_type_relational loc t1 t2
    | GE (loc,t1, t2) -> check_and_get_return_type_relational loc t1 t2
    | EQ (loc,t1, t2) -> check_and_get_return_type_equality loc t1 t2
    | NE (loc,t1, t2) -> check_and_get_return_type_equality loc t1 t2
    | And (loc,t1, t2) -> check_and_get_return_type_logical loc t1 t2
    | Or (loc,t1, t2) -> check_and_get_return_type_logical loc t1 t2
    | Not (loc,t) ->
	let ltype = cagrt t in
	if not (is_boolean_type ltype loc) then
	  add_error SemanticError (get_type_error_msg "Logical not" ltype "boolean") loc errors;
	Bool(loc)
    | Iff (loc,t1, t2) -> check_and_get_return_type_logical loc t1 t2
    | Implies (loc,t1, t2) -> check_and_get_return_type_logical loc t1 t2
    | Length (loc, t) ->
	let atype = cagrt t in
	if not (is_array_type atype) then
	  begin
	    let error_msg = "Trying to take length of something that is of type '" ^ (string_of_type atype)  ^ "' and not an array" in
	    add_error SemanticError error_msg loc errors;
	  end;
	Int(loc)
    | EmptyExpr -> Void (Ast.get_dummy_location ())
  and cagrt e = cagrt_full e (false)
  in
  cagrt_full e (is_top_level) ;;

let check_ranking_annotation ra scope_stack errors = match ra with
    Some (expr) ->
      let check_ranking_expr e =
	let my_type = check_and_get_return_type scope_stack e errors (false, true, true) in
	if not (Ast.is_integral_type my_type) then
	  begin
	    let error_msg = ("Ranking annotation must be integral but instead is " ^ (string_of_type my_type)) in
            add_error SemanticError error_msg (location_of_expr e) errors;
	  end;
      in
      List.iter check_ranking_expr expr
  | None -> () ;;

let rec check_stmt scope_stack returnType errors (is_in_loop) stmt =
  match stmt with
    Expr (loc, e) -> ignore (check_and_get_return_type scope_stack e errors (false, false, true))

  | VarDeclStmt(loc,d) -> (insert_decl scope_stack errors (VarDecl(loc,d))) 

  | IfStmt (loc, test, then_block, else_block) ->
      let testType = check_and_get_return_type scope_stack test errors (false, false, true) in
      if not (is_boolean_type testType loc) then
	begin
	  let error_msg = "Test type is " ^ (string_of_type testType) ^ " but should be boolean" in
	  add_error SemanticError error_msg loc errors;
	end;
      check_stmt scope_stack returnType errors (is_in_loop) then_block;
      check_stmt scope_stack returnType errors (is_in_loop) else_block;

  | WhileStmt (loc, test, block, annotation, ra) -> 
      ignore (check_and_get_return_type scope_stack annotation errors (true, false, true));
      check_ranking_annotation ra scope_stack errors;
      let testType = check_and_get_return_type scope_stack test errors (false, false, true) in
      if not (is_boolean_type testType loc) then
	begin
	  let error_msg = "Test type is " ^ (string_of_type testType) ^ " but should be boolean" in
	  add_error SemanticError error_msg loc errors;
	end;
      check_stmt scope_stack returnType errors (true) block;
      
  | ForStmt (loc, init, test, incr, block, annotation, ra) ->
      ignore (check_and_get_return_type scope_stack annotation errors (true, false, true));
      check_ranking_annotation ra scope_stack errors;      
      ignore (check_and_get_return_type scope_stack init errors (false, false, true));
      let testType = check_and_get_return_type scope_stack test errors (false, false, true) in
      if not (is_boolean_type testType loc) then
	begin
	  let error_msg = "Test type is " ^ (string_of_type testType) ^ " but should be boolean" in
	  add_error SemanticError error_msg loc errors;
	end;
      ignore (check_and_get_return_type scope_stack incr errors (false, false, true));
      check_stmt scope_stack returnType errors (true) block;
      
  | BreakStmt (loc) ->
      if (not is_in_loop) then
	begin
	  let error_msg = "Break stmt outside of a loop." in
          add_error SemanticError error_msg loc errors
	end

  | ReturnStmt(loc,e) ->  let type_of_return = (check_and_get_return_type scope_stack e errors (false, false, true)) in
                          if not (types_equal type_of_return returnType) then
			    let error_msg = ("Incorrect return type: expected: " ^ (string_of_type returnType) ^ ", given: " ^ (string_of_type type_of_return)) in
                            add_error SemanticError error_msg  loc errors

  | AssertStmt (loc, e) -> ignore (check_and_get_return_type scope_stack e errors (true, false, true))

  | StmtBlock(loc,st) -> Scope_stack.enter_scope scope_stack;
                      List.iter (check_stmt scope_stack returnType errors (is_in_loop)) st;
                      Scope_stack.exit_scope scope_stack

  | EmptyStmt -> ignore ()

(* Ensures that a non-void function returns
   in all control paths. *)
let ensure_function_returns f =
  (* Check if a stmt returns by recursing on StmtBlocks
     and ifs with an else. *)
  let rec check_if_stmt_returns stmt =
    match stmt with
	ReturnStmt (_, _) -> true
      | StmtBlock (_, stmts)  -> check_if_stmts_return stmts
      | IfStmt (_, _, then_block, else_block) ->
	  begin
	    match else_block with
		EmptyStmt -> false
	      | _ -> (check_if_stmt_returns then_block) && (check_if_stmt_returns else_block)
	  end
      | _ -> false
  (* A list of stmts returns if one of the
     stmts returns. *)
  and check_if_stmts_return stmts =
    match stmts with
	[] -> false
      | s :: rest -> (check_if_stmt_returns s) || (check_if_stmts_return rest)
  in
  match f.returnType with
      Void (_) -> true
    | _ -> check_if_stmt_returns f.stmtBlock

let check_function func s errors =
  Scope_stack.enter_scope s;
  insert_var_decls s errors func.formals;
  ignore (check_and_get_return_type s func.preCondition errors (true, false, true));
  check_ranking_annotation func.rankingAnnotation s errors;


  Scope_stack.enter_scope s;
  begin
    let vd = Ast.create_varDecl func.returnType (Ast.create_identifier "rv" (Ast.get_dummy_location ()) ) (Ast.get_dummy_location ()) in
      vd.var_id := Some("rv");
      Scope_stack.insert_decl_without_setting_id (Ast.VarDecl(Ast.get_dummy_location (), vd)) s
  end;
  (*add rv to scope*)
  ignore (check_and_get_return_type s func.postCondition errors (true, false, true));
  Scope_stack.exit_scope s;


  check_stmt s func.returnType errors (false) func.stmtBlock;
  if not (ensure_function_returns func) then
    let error_msg = "Function " ^ (string_of_identifier func.fnName) ^ " does not return in all control paths." in
    add_error SemanticError error_msg func.location_fd errors;
  Scope_stack.exit_scope s

let check_predicate pred s errors =
  Scope_stack.enter_scope s;
  insert_var_decls s errors pred.formals_p;
  let pred_type = check_and_get_return_type s pred.expr errors (true, false, true) in
    begin
      match pred_type with
          Bool(loc) -> ignore ()
        | _ -> let error_msg = "Predicate " ^ (string_of_identifier pred.predName) ^ " does not evaluate to a boolean." in
            add_error SemanticError error_msg (location_of_expr pred.expr) errors
    end;
  Scope_stack.exit_scope s

let check_program program errors =
  let s = Scope_stack.create () in
  Scope_stack.enter_scope s;
  insert_decls s errors program.decls;
  let check_decl_if_necessary decl = 
    match decl with
	FnDecl(l, d) -> check_function d s errors
      | Predicate(l,p) -> check_predicate p s errors
      | VarDecl(l,v) -> print_string("")
  in
  List.iter (check_decl_if_necessary) program.decls;
  Scope_stack.exit_scope s
