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

and is_int_type t1 = match t1 with
  | Int (loc) -> true
  | ErrorType -> true (* So cascading errors work *)
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
	
and check_for_same_type t1 t2 loc errors = 
  if not (types_equal t1 t2) then
    begin
      let error_msg = "LHS '" ^ (string_of_type t1) ^ "' and RHS '" ^ (string_of_type t2) ^ "' are of different types" in
      add_error SemanticError error_msg loc errors;
      false
    end
  else
    true

(* Semantically check an annotation and assign its name. *)
and check_annotation annotation scope_stack errors annotation_id func = 
  let annotation_type = check_and_get_return_type scope_stack annotation.ann errors (true, false, true) in
  (* Ensure the annotation has type bool. *)
  begin
    if not (types_equal annotation_type (Bool(gdl()))) then
      let error_message = get_type_error_msg (string_of_expr annotation.ann) annotation_type "bool" in
      add_error SemanticError error_message (location_of_expr annotation.ann) errors
  end;
  (* Assign the annotation's name. *)
  let name =
    match annotation.ann_type with
      | Normal (id) ->
	  (* Use the user-provided annotation if provided, else generate our own. *)
	  begin
	    match id with
	      | Some (x) -> string_of_identifier x
	      | None ->
		  begin
		    let my_name = Ast.name_annotation func annotation_id annotation.ann_type in
		    incr annotation_id;
		    my_name
		  end
	  end
      | Precondition -> Ast.name_annotation func annotation_id Precondition
      | Postcondition -> Ast.name_annotation func annotation_id Postcondition
      | Runtime -> assert false (* We don't generate these annotations until basic path generation. *)
  in
  annotation.ann_name <- Some (name)


and check_type t scope_stack errors =
  match t with
    | Bool(loc) -> ignore ()
    | Int(loc) -> ignore ()
    | Float(loc) -> ignore ()
    | Identifier (ident, loc) -> 
        begin
	  let lookup_result = (Scope_stack.lookup_decl ident.name scope_stack) in
	    match lookup_result with
	      | None ->
		  let error_msg = "Undefined type '" ^ (string_of_identifier ident) ^ "'" in
		    add_error SemanticError error_msg loc errors
	      | Some (d) ->
                  begin
                    match d with
                        ClassDecl (l, cd) -> ignore ()
                      | _ ->
                          begin
		            let error_msg = "The identifier '" ^ (string_of_identifier ident) ^ "' does not refer to a class" in
		              add_error SemanticError error_msg loc errors
                          end
	          end
        end
    | Array (t, loc) -> check_type t scope_stack errors
    | Void(loc) -> ignore ()
    | ErrorType -> ignore () 

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
                          VarDecl(loc2,vd) -> if(vd.is_param) then add_error SemanticError "Updates to parameters not permitted."  (location_of_lval lval_orig) errors
                        | _ -> ignore() (*this is an error, but it'll be caught in cagrt*)
                    end
                | None -> ignore() (*this is an error, but it'll be caught in cagrt*)
            end
        | ArrayLval(loc,arr,index) -> 
            begin
              let rec check_lhs_expr_of_assign exp = 
                match exp with
                    LValue(loc,l) -> cloa l
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
      add_error SemanticError (get_type_error_msg "Logical" lhsType "bool") loc errors;
    if not (is_boolean_type rhsType loc) then
      add_error SemanticError (get_type_error_msg "Logical" rhsType "bool") loc errors;
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
			  if is_annotation || is_ranking_fn then begin
			    check_formals p.formals_p;
			    Bool(gdl())
			  end else begin
			    let error_msg = "'" ^ (string_of_identifier p.predName) ^ "' is not a function" in
		            add_error SemanticError error_msg loc errors;
			    ErrorType
			  end
		      | FnDecl (loc, fd) ->
			  if is_annotation || is_ranking_fn then begin
			    let error_msg = "'" ^ (string_of_identifier fd.fnName) ^ "' is not a predicate" in
		            add_error SemanticError error_msg loc errors;
			    ErrorType
			  end else begin
			    check_formals fd.formals;
			    fd.returnType
			  end
		      | ClassDecl (loc, cd) -> 
                          begin
                            let what_it_should_be = 
                              match is_annotation with
                                  true -> "predicate"
                                | false -> "function"
                            in
                            let error_msg = "'" ^ (string_of_identifier cd.className) ^ "' is not a " ^ what_it_should_be in
		              add_error SemanticError error_msg loc errors; ErrorType
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
	  add_error SemanticError (get_type_error_msg "Logical not" ltype "bool") loc errors;
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
    | NewArray(loc,t,e) -> 
        check_type t scope_stack errors;
	let size_type = cagrt e in
          begin
	    if not (is_int_type size_type) then
	      (add_error SemanticError "Array initialization size is not an int" loc) errors
          end;
	  Array(t,gdl())
    | EmptyExpr -> Void (Ast.get_dummy_location ())
  and cagrt e = cagrt_full e (false)
  in
  cagrt_full e (is_top_level) ;;

(* Check a ranking annotation.  We ensure that all the things
   in the tuples are integers. We also set its associated_annotation field. *)
let check_ranking_annotation ra_opt associated_annotation scope_stack errors = match ra_opt with
    Some (ra) ->
      begin
	let check_ranking_expr e =
	  let my_type = check_and_get_return_type scope_stack e errors (false, true, true) in
	  if not (Ast.is_integral_type my_type) then
	    begin
	      let error_msg = ("Ranking annotation must be integral but instead is " ^ (string_of_type my_type)) in
	      add_error SemanticError error_msg (location_of_expr e) errors;
	    end;
	in
	List.iter check_ranking_expr ra.tuple;
	ra.associated_annotation <- Some (associated_annotation)
      end
  | None -> () ;;

        
let rec check_stmt scope_stack returnType errors (is_in_loop) annotation_id func stmt =
  match stmt with
    Expr (loc, e) -> ignore (check_and_get_return_type scope_stack e errors (false, false, true))

  | VarDeclStmt(loc,d) -> check_type d.varType scope_stack errors; insert_decl scope_stack errors (VarDecl(loc,d))

  | IfStmt (loc, test, then_block, else_block) ->
      let testType = check_and_get_return_type scope_stack test errors (false, false, true) in
      if not (is_boolean_type testType loc) then
	begin
	  let error_msg = "Test type is " ^ (string_of_type testType) ^ " but should be boolean" in
	  add_error SemanticError error_msg loc errors;
	end;
      check_stmt scope_stack returnType errors (is_in_loop) annotation_id func then_block;
      check_stmt scope_stack returnType errors (is_in_loop) annotation_id func else_block;

  | WhileStmt (loc, test, block, annotation, ra) ->
      check_annotation annotation scope_stack errors annotation_id func;
      check_ranking_annotation ra annotation scope_stack errors;
      let testType = check_and_get_return_type scope_stack test errors (false, false, true) in
      if not (is_boolean_type testType loc) then
	begin
	  let error_msg = "Test type is " ^ (string_of_type testType) ^ " but should be boolean" in
	  add_error SemanticError error_msg loc errors;
	end;
      check_stmt scope_stack returnType errors (true) annotation_id func block;
      
  | ForStmt (loc, init, test, incr, block, annotation, ra) ->
      check_annotation annotation scope_stack errors annotation_id func;
      check_ranking_annotation ra annotation scope_stack errors;      
      ignore (check_and_get_return_type scope_stack init errors (false, false, true));
      let testType = check_and_get_return_type scope_stack test errors (false, false, true) in
      if not (is_boolean_type testType loc) then
	begin
	  let error_msg = "Test type is " ^ (string_of_type testType) ^ " but should be boolean" in
	  add_error SemanticError error_msg loc errors;
	end;
      ignore (check_and_get_return_type scope_stack incr errors (false, false, true));
      check_stmt scope_stack returnType errors (true) annotation_id func block;
      
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

  | AssertStmt (loc, e) -> check_annotation e scope_stack errors annotation_id func

  | StmtBlock(loc,st) -> Scope_stack.enter_scope scope_stack;
                      List.iter (check_stmt scope_stack returnType errors (is_in_loop) annotation_id func) st;
                      Scope_stack.exit_scope scope_stack

  | EmptyStmt -> ignore () ;;

(* Check termination.
   We ensure that all necessary termination arguments are present
   and that all termination paths begin and end with annotations
   of the same tuple size.
   Note that we could cache some things, like the result of Ast_utils.
   get_loops, get_fn_calls, and calls, since we likely call them multiple
   times for a given function.  But that would be a bit ugly, and we
   don't seem to spend much time in these functions, so we aren't for now. *)
let check_termination func scope_stack errors =
  let get_fndecl_from_scope_stack c =
    let called_fn = Ast_utils.get_called_function c in
    Scope_stack.lookup_decl called_fn.name scope_stack
  in
  (* Checks whether a functions is recursive (directly or indirectly). *)
  let is_recursive func =
    Ast_utils.calls func func get_fndecl_from_scope_stack
  in
  (* Gets whether or not the user is trying to prove that a function terminates.
     They are trying to prove a function terminates if they annotation
     the function itself or any of its loop.  Or if the function is not recursive
     and has no loops, in which case there is nothing to do. *)
  let is_trying_to_prove_termination func =
    if (Utils.is_some func.fnRankingAnnotation) then
      true
    else begin
      let loops = Ast_utils.get_loops func in
      let termination_args = List.map Ast_utils.get_loop_ranking_annotation loops in
      let is_termination_arg = List.exists Utils.is_some termination_args in
      is_termination_arg
    end
  in
  if is_trying_to_prove_termination func then begin
    let func_loops = Ast_utils.get_loops func in
    let func_calls = Ast_utils.get_fn_calls func get_fndecl_from_scope_stack in
    (* If we're trying to prove termination, we need to ensure that all loops
       are annotated and that all calls are themselves proved to terminate.
       We also need to make sure that paths we will use have the same size annotations. *)
    (* If the function is recursive, we need to annotate it. *)
    if (is_recursive func) && (Utils.is_none func.fnRankingAnnotation) then begin
      let fn_name_str = string_of_identifier func.fnName in
      let error_msg = "To prove that the function " ^ fn_name_str ^ " terminates, you must annotate the function itself since it is recursive." in
      add_error SemanticError error_msg func.location_fd errors
    end;
    (* Ensure that all loops are annotated. *)
    let ensure_loop_ra s =
      let ra = Ast_utils.get_loop_ranking_annotation s in
      if (Utils.is_none ra) then begin
	let fn_name_str = string_of_identifier func.fnName in
	let error_msg = "To prove that the function " ^ fn_name_str ^ " terminates, you must annotate this loop." in
	let loc = Ast.location_of_stmt s in
	add_error SemanticError error_msg loc errors
      end
    in
    List.iter ensure_loop_ra func_loops;
    (* Ensure that all called functions are proved to terminate. *)
    let ensure_trying_to_prove_fn_termination call =
      let fn_decl_opt = Ast_utils.get_fndecl_from_call get_fndecl_from_scope_stack call in
      if (Utils.is_some fn_decl_opt) then
	let fn_decl = Utils.elem_from_opt fn_decl_opt in
	let is_trying = is_trying_to_prove_termination fn_decl in
	let loops = Ast_utils.get_loops fn_decl in
	let doesnt_need_termination_arg = (not (is_recursive fn_decl) && (List.length loops) == 0) in
	if not (is_trying || doesnt_need_termination_arg) then begin
	  let caller = string_of_identifier func.fnName in
	  let callee = string_of_identifier fn_decl.fnName in
	  let error_msg = "To prove that the function " ^ caller ^ " terminates, you must prove that the function " ^ callee ^ " terminates since you call it." in
	  let loc = Ast.location_of_expr call in
	  add_error SemanticError error_msg loc errors
	end
    in
    List.iter ensure_trying_to_prove_fn_termination func_calls;
    (* Ensure all paths have same size annotations. *)
    let ensure_same_size_annotations () =
      (* Get the size of the first tuple in the fn.
	 We will compare all other sizes to it. *)
      let tuple_size =
	if (Utils.is_some func.fnRankingAnnotation) then
	  let ra = Utils.elem_from_opt func.fnRankingAnnotation in
	  List.length ra.tuple
	else (* We must have at least one loop annotation since is_trying_to_prove_termination is true. *)
	  let loop_ras = List.map Ast_utils.get_loop_ranking_annotation func_loops in
	  let first_tuple_opt = List.find Utils.is_some loop_ras in
	  let first_tuple = Utils.elem_from_opt first_tuple_opt in
	  List.length first_tuple.tuple
      in
      let check_loop_ra_tuple_size l =
	let ra_opt = Ast_utils.get_loop_ranking_annotation l in
	if Utils.is_some ra_opt then
	  let ra = Utils.elem_from_opt ra_opt in
	  let my_length = List.length ra.tuple in
	  if my_length != tuple_size then
	    let error_msg = "The ranking annotation for this loop has size " ^ string_of_int my_length ^ " when other ranking annotations in this function have size " ^ string_of_int tuple_size ^ "." in
	    let loc = Ast.location_of_stmt l in
	    add_error SemanticError error_msg loc errors
      in
      let check_call_ra_tuple_size call =
	let fndecl_opt = Ast_utils.get_fndecl_from_call get_fndecl_from_scope_stack call in
	if Utils.is_some fndecl_opt then
	  let fndecl = Utils.elem_from_opt fndecl_opt in
	  let ra_opt = fndecl.fnRankingAnnotation in
	  if (Utils.is_some ra_opt) then
	    let ra = Utils.elem_from_opt ra_opt in
	    let my_length = List.length ra.tuple in
	    if (Ast_utils.calls fndecl func get_fndecl_from_scope_stack && my_length != tuple_size) then
	      let error_msg = "The ranking annotation for this call has size " ^ string_of_int my_length ^ " when other ranking annotations in this function have size " ^ string_of_int tuple_size ^ "." in
	      let loc = Ast.location_of_expr call in
	      add_error SemanticError error_msg loc errors
      in
      List.iter check_loop_ra_tuple_size func_loops;
      List.iter check_call_ra_tuple_size func_calls
    in
    ensure_same_size_annotations ()
  end ;;

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
    | _ -> check_if_stmt_returns f.stmtBlock ;;

let check_function func s errors =
  let ann_id = ref 0 in
  Scope_stack.enter_scope s;
  insert_var_decls s errors func.formals;
  check_annotation func.preCondition s errors ann_id func;
  check_ranking_annotation func.fnRankingAnnotation func.preCondition s errors;
  check_termination func s errors;
  Scope_stack.enter_scope s;
  (*add rv to scope*)
  begin
    let vd = Ast.create_varDecl func.returnType (Ast.create_identifier "rv" (Ast.get_dummy_location ()) ) (Ast.get_dummy_location ()) in
      vd.var_id := Some("rv");
      Scope_stack.insert_decl_without_setting_id (Ast.VarDecl(Ast.get_dummy_location (), vd)) s
  end;
  check_annotation func.postCondition s errors ann_id func;  
  Scope_stack.exit_scope s;

  check_stmt s func.returnType errors (false) ann_id func func.stmtBlock;
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
  let check_decl decl = 
    match decl with
	FnDecl(l, d) -> check_function d s errors
      | Predicate(l,p) -> check_predicate p s errors
      | VarDecl(l,v) -> check_type v.varType s errors
      | ClassDecl(l,c) -> ignore ()
  in
    List.iter check_decl program.decls;
    Scope_stack.exit_scope s
      
