open Ast
open Scope_stack


let print_error node message location =
print_string("**Semantic Error**\n");
print_string(string_of_location location);
print_string("\n");
print_string(message);
print_string("\n")


let insert_decl s decl = 
  let curr = lookup_decl_in_curr_scope decl s in
    match curr with
	None -> insert_decl decl s
      | _ -> print_error decl "Already defined" (location_of_decl decl)

let insert_decls s decls = 
  List.iter (insert_decl s) decls

let rec check_stmt s stmt = match stmt with
  StmtBlock(l, st) -> enter_scope s;
                      List.iter (check_stmt s) st;
                      exit_scope s
  | VarDeclStmt(l, d) -> (insert_decl s (VarDecl(l,d))) 
  | _ -> print_string("")

let check_function func s = 
  check_stmt s func.stmtBlock

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
