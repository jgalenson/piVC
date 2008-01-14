open Ast

type error_type =
  | SyntaxError
  | SemanticError ;;

type error = {
  e_type : error_type;
  loc  : location;
  msg  : string;
};;

let string_of_error_type t = match t with
  | SyntaxError -> "Syntax Error"
  | SemanticError -> "Semantic Error" ;;

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
      | _ -> add_error SemanticError "Already defined" (location_of_decl decl) errors

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
  
(* CAGRT *)
	
let rec check_and_get_return_type_lval s lval errors = match lval with
  | NormLval(loc, id) ->
      let lookupResult = Scope_stack.lookup_decl id.name s in
      begin
	match lookupResult with
          | None -> add_error SemanticError "Not defined" loc errors; ErrorType
	  | Some(decl) -> type_of_decl decl
      end
  | ArrayLval(loc, arr, index) ->
      begin
        let typeOfIndex = check_and_get_return_type s index errors in
        begin
	  match typeOfIndex with
            | Int(l) -> ()
            | _ -> add_error SemanticError "Array index must be an integer" loc errors
        end;
        let lookupResult = (Scope_stack.lookup_decl arr.name s) in
        begin
	  match lookupResult with
           | None -> add_error SemanticError "Not defined" loc errors; ErrorType
           | Some(decl) -> let typeOfArray = type_of_decl decl in
	     begin
               match typeOfArray with
                 | Array(t, l) -> t
                 | _ -> add_error SemanticError "This is not an array" loc errors; ErrorType
             end
        end
      end
 
	
and check_for_same_type t1 t2 loc errors = 
  if not (types_equal t1 t2) then
    begin
      add_error SemanticError "LHS and RHS are of different types" loc errors;
      false
    end
  else
    true
  
and check_and_get_return_type scope_stack e errors =
  
  let rec check_and_get_return_type_relational loc t1 t2 =
    let lhsType = cagrt t1
    and rhsType = cagrt t2 in
    ignore (check_for_same_type lhsType rhsType loc errors);
    if not (is_numeric_type lhsType) or not (is_numeric_type rhsType) then
      add_error SemanticError "Relational expr type is not numeric" loc errors;
    Bool(loc)
      
  and check_and_get_return_type_equality loc t1 t2 =
    let lhsType = cagrt t1
    and rhsType = cagrt t2 in
    ignore (check_for_same_type lhsType rhsType loc errors);
    Bool(loc)

  and check_and_get_return_type_logical loc t1 t2 =
    let lhsType = cagrt t1
    and rhsType = cagrt t2 in
    if not (is_boolean_type lhsType loc) or not (is_boolean_type rhsType loc) then
      add_error SemanticError "Logical expr type is not boolean" loc errors;
    Bool(loc)
      
  and check_and_get_return_type_arithmetic loc t1 t2 =
    let leftType = cagrt t1
    and rightType = cagrt t2 in
    let are_same_type = check_for_same_type leftType rightType loc errors in
    if not (is_numeric_type leftType) or not (is_numeric_type rightType) then
      begin
	add_error SemanticError "Arithmetic expr type is not numeric" loc errors;
	ErrorType
      end
    else if not are_same_type then
      ErrorType
    else
      rightType
      
  and cagrt e = match e with
    | Assign (loc,l,e) ->
	let lhsType = check_and_get_return_type_lval scope_stack l errors in
        let rhsType = cagrt e in
	ignore (check_for_same_type lhsType rhsType loc errors);
        lhsType
    | Constant (loc,c) ->
	begin
	  match c with
	  | ConstInt (l, i) -> Int (loc)
	  | ConstFloat (l, f) -> Float (loc)
	  | ConstBool (l, b) -> Bool (loc)
	end
    | LValue (loc,l) -> check_and_get_return_type_lval scope_stack l errors
    | Call (loc, ident, ac) -> (* Check this more? *)
	let map_fn e = ignore (cagrt e) in
	let check_actuals = List.iter (map_fn) ac
	and lookup_result = (Scope_stack.lookup_decl ident.name scope_stack) in
	(* Check if there is a function with that name *)
	let isfndecl =
	  begin
	    match lookup_result with
	      | None -> add_error SemanticError "Function name not defined." loc errors; check_actuals; None
	      | Some (d) -> match d with
		| VarDecl (l, vd) -> add_error SemanticError "Not a function" loc errors; check_actuals; None
		| FnDecl (loc, fd) -> Some (fd)
	  end
	in
	begin
	  match isfndecl with
	    | None -> ErrorType
	    | Some (fndecl) ->
		(* Check a call to a valid function *)
		if List.length ac != List.length fndecl.formals then
		  begin
		    add_error SemanticError "Incorrect number of arguments" loc errors;
		    check_actuals;
		    ErrorType
		  end
		else
		  let check_formal given expected =
		    let given_type = cagrt given
		    and expected_type = expected.varType in
		    ignore (check_for_same_type given_type expected_type loc errors)
		  in
		  List.iter2 check_formal ac fndecl.formals;
		  fndecl.returnType
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
	  add_error SemanticError "Not expr type is not boolean" loc errors;
	Bool(loc)
    | Iff (loc,t1, t2) -> check_and_get_return_type_logical loc t1 t2
    | Implies (loc,t1, t2) -> check_and_get_return_type_logical loc t1 t2
    | Length (loc, t) ->
	let atype = cagrt t in
	if not (is_array_type atype) then
	  add_error SemanticError "Trying to take length of something not an array" loc errors;
	Int(loc)
    | EmptyExpr -> Void (Ast.get_dummy_location ())

  in
  cagrt e

let rec check_stmt scope_stack returnType errors stmt =
  match stmt with
    Expr (loc, e) -> ignore (check_and_get_return_type scope_stack e errors)

  | VarDeclStmt(loc,d) -> (insert_decl scope_stack errors (VarDecl(loc,d))) 

  | IfStmt (loc, test, then_block, else_block) ->
      let testType = check_and_get_return_type scope_stack test errors in
      if not (is_boolean_type testType loc) then
	add_error SemanticError "Test not boolean" loc errors;
      check_stmt scope_stack returnType errors then_block;
      check_stmt scope_stack returnType errors else_block;

  | WhileStmt (loc, test, block, annotation) -> 
      let testType = check_and_get_return_type scope_stack test errors in
      if not (is_boolean_type testType loc) then
	add_error SemanticError "Test not boolean" loc errors;
      ignore (check_and_get_return_type scope_stack annotation errors);
      check_stmt scope_stack returnType errors block;
      
  | ForStmt (loc, init, test, incr, block, annotation) ->
      ignore (check_and_get_return_type scope_stack annotation errors);
      ignore (check_and_get_return_type scope_stack init errors);
      let testType = check_and_get_return_type scope_stack test errors in
      if not (is_boolean_type testType loc) then
	add_error SemanticError "Test not boolean" loc errors;
      ignore (check_and_get_return_type scope_stack incr errors);
      check_stmt scope_stack returnType errors block;
      
  | BreakStmt (loc) -> print_string "" (* TODO: Break stmts *)

  | ReturnStmt(loc,e) ->  let type_of_return = (check_and_get_return_type scope_stack e errors) in
                          if not (types_equal type_of_return returnType) then
                            add_error SemanticError "Incorrect return type" loc errors

  | AssertStmt (loc, e) -> ignore (check_and_get_return_type scope_stack e errors)
	(* TODO: Do semantic checking for asserts vs. exprs *)

  | StmtBlock(loc,st) -> Scope_stack.enter_scope scope_stack;
                      List.iter (check_stmt scope_stack returnType errors) st;
                      Scope_stack.exit_scope scope_stack

  | EmptyStmt -> ignore ()



let check_function func s errors =
  Scope_stack.enter_scope s;
  insert_var_decls s errors func.formals;
  check_stmt s func.returnType errors func.stmtBlock;
  Scope_stack.exit_scope s

let check_program program errors =
  print_string("Checking...\n");
  let s = Scope_stack.create () in
  Scope_stack.enter_scope s;
  insert_decls s errors program.decls;
  let check_decl_if_necessary decl = 
    match decl with
	FnDecl(l, d) -> (check_function d s errors)
        | _ -> print_string("")
  in
  List.iter (check_decl_if_necessary) program.decls;
  Scope_stack.exit_scope s
