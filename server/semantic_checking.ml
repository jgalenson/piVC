open Ast
open Scope_stack

let print_error message location =
print_string("**Semantic Error**\n");
print_string(string_of_location location);
print_string("\n");
print_string(message);
print_string("\n")


let insert_decl s decl = 
  let curr = lookup_decl_in_curr_scope decl s in
    match curr with
	None -> insert_decl decl s
      | _ -> print_error "Already defined" (location_of_decl decl)

let insert_var_decl s decl = 
  insert_decl s (Ast.VarDecl(decl.location_vd, decl))

let insert_decls s decls = 
  List.iter (insert_decl s) decls

let insert_var_decls s decls = 
  List.iter (insert_var_decl s) decls 

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
  | Array (aType, loc) -> (is_numeric_type aType)
  | Identifier (t, loc) -> true (* TODO finish off like above *)
  | _ -> false

and is_error_type t1 = match t1 with
  | ErrorType -> true
  | _ -> false
	
(* Is this too hacky? *)
and is_boolean_type t1 loc = let b = Bool(loc) in (types_equal t1 b)
	
(* CAGRT *)
	
let rec check_and_get_return_type_lval s lval = match lval with
    NormLval(loc, id) -> let lookupResult = (lookup_decl s id.name) in
                           (match lookupResult with
                               None -> (print_error "Not defined" loc; ErrorType)
                             | Some(decl) -> type_of_decl decl
                           )
  | ArrayLval(loc, arr, index) ->
      let typeOfArray = check_and_get_return_type s arr in
      let typeOfIndex = check_and_get_return_type s index in
        (match typeOfIndex with
             Int(l) -> print_string ""
           | _ -> (print_error "Array index must be an integer" loc)
        );
        (match typeOfArray with
            Array(t, l) -> t
          | _ -> (print_error "This is not an array" loc;
                  ErrorType
                 )
        )
	
and check_for_same_type t1 t2 loc = 
  if not (types_equal t1 t2) then
    print_error "LHS and RHS are of different types" loc;
  
and check_and_get_return_type scope_stack e =
  
  let rec check_and_get_return_type_relational loc t1 t2 =
    let lhsType = cagrt t1
    and rhsType = cagrt t2 in
    check_for_same_type lhsType rhsType loc;
    if not (is_numeric_type lhsType) or not (is_numeric_type rhsType) then
      print_error "Relational expr type is not numeric" loc;
    Bool(loc)
      
  and check_and_get_return_type_equality loc t1 t2 =
    let lhsType = cagrt t1
    and rhsType = cagrt t2 in
    check_for_same_type lhsType rhsType loc;
    Bool(loc)

  and check_and_get_return_type_logical loc t1 t2 =
    let lhsType = cagrt t1
    and rhsType = cagrt t2 in
    if not (is_boolean_type lhsType loc) or not (is_boolean_type rhsType loc) then
      print_error "Logical expr type is not boolean" loc;
    Bool(loc)
      
  and check_and_get_return_type_arithmetic loc t1 t2 =
    let leftType = cagrt t1
    and rightType = cagrt t2 in
    check_for_same_type leftType rightType loc;
    if not (is_numeric_type leftType) or not (is_numeric_type rightType) then
      print_error "Arithmetic expr type is not numeric" loc;
    rightType
      
  and cagrt e = match e with
    | Assign (loc,l,e) ->
	let lhsType = check_and_get_return_type_lval scope_stack l in
        let rhsType = cagrt e in
	check_for_same_type lhsType rhsType loc;
        lhsType
    | Constant (loc,c) ->
	begin
	  match c with
	  | ConstInt (l, i) -> Int (loc)
	  | ConstFloat (l, f) -> Float (loc)
	  | ConstBool (l, b) -> Bool (loc)
	end
    | LValue (loc,l) -> check_and_get_return_type_lval scope_stack l
    | Call (loc,s, el) -> Void(loc) (* TODO: this *)
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
	    (print_error "Unary minus type is not numeric" loc);
	    ignore ErrorType
	  end;
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
	  print_error "Not expr type is not boolean" loc;
	Bool(loc)
    | Iff (loc,t1, t2) -> check_and_get_return_type_logical loc t1 t2
    | Implies (loc,t1, t2) -> check_and_get_return_type_logical loc t1 t2
    | Length (loc, t) -> Void(loc) (* TODO: this *)
    | EmptyExpr -> Void(Ast.get_dummy_location)
  in
  cagrt e

(*TODO: finish writing*)
let rec check_stmt scope_stack returnType stmt =
  match stmt with
    Expr (loc, e) -> ignore (check_and_get_return_type scope_stack e)

  | VarDeclStmt(loc,d) -> (insert_decl scope_stack (VarDecl(loc,d))) 

  | IfStmt (loc, test, then_block, else_block) ->
      let testType = check_and_get_return_type scope_stack test in
      if not (is_boolean_type testType loc) then
	print_error "Test not boolean" loc;
      check_stmt scope_stack returnType then_block;
      check_stmt scope_stack returnType else_block;

  | WhileStmt (loc, test, block, annotation) -> 
      let testType = check_and_get_return_type scope_stack test in
      if not (is_boolean_type testType loc) then
	print_error "Test not boolean" loc;
      ignore (check_and_get_return_type scope_stack annotation);
      check_stmt scope_stack returnType block;
      
  | ForStmt (loc, init, test, incr, block, annotation) ->
      ignore (check_and_get_return_type scope_stack annotation);
      ignore (check_and_get_return_type scope_stack init);
      let testType = check_and_get_return_type scope_stack test in
      if not (is_boolean_type testType loc) then
	print_error "Test not boolean" loc;
      ignore (check_and_get_return_type scope_stack incr);
      check_stmt scope_stack returnType block;
      
  | BreakStmt (loc) -> print_string ""

  | ReturnStmt(loc,e) ->  let type_of_return = (check_and_get_return_type scope_stack e) in
                          if not (types_equal type_of_return returnType) then
                            print_error "Incorrect return type" loc

  | AssertStmt (loc, e) -> ignore (check_and_get_return_type scope_stack e)
	(* TODO: Do semantic checking for asserts vs. exprs *)

  | StmtBlock(loc,st) -> enter_scope scope_stack;
                      List.iter (check_stmt scope_stack returnType) st;
                      exit_scope scope_stack

  | EmptyStmt -> ignore ()



let check_function func s = 
  insert_var_decls s func.formals;
  check_stmt s func.returnType func.stmtBlock

let check_program program =
  print_string("Checking...\n");
  let s = create_scope_stack in
  enter_scope s;
  insert_decls s program.decls;
  let check_decl_if_necessary decl = 
    match decl with
	FnDecl(l, d) -> (check_function d s)
        | _ -> print_string("")
  in
  List.iter (check_decl_if_necessary) program.decls;
  exit_scope s
