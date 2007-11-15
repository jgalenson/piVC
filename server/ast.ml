open Printf

type varType = 
  | Bool
  | Int
  | Float
  | Ident of string
  | Array of varType
  | Void

type varDecl = {
  varType : varType;
  varName : string;
}
let create_varDecl t name = {varType=t; varName=name}

type expr =
  | Plus of expr * expr
  | EmptyExpr
  | UnimplementedExprA of unit
  | UnimplementedExprB of expr	
let plus t1 t2 = Plus (t1, t2)
let emptyExpr = EmptyExpr 
let unimplementedExprA u = UnimplementedExprA (u)
let unimplementedExprB e = UnimplementedExprB (e)
    
type stmt =
  | Expr of expr
  | VarDeclStmt of varDecl
  | StmtBlock of stmt list
  | UnimplementedStmt of unit
let exprStmt e = Expr (e)
let varDeclStmt vd = VarDeclStmt (vd)
let stmtBlock sb = StmtBlock (sb)
let unimplementedStmt u = UnimplementedStmt (u)
	
type fnDecl = {
  fnName       : string;
  formals    : varDecl list;
  returnType   : varType;
  stmts : stmt list;
}
let create_fnDecl name formals returnType stmts = {fnName=name; returnType = returnType; formals = formals; stmts = stmts}

type decl = 
  | VarDecl of varDecl
  | FnDecl of fnDecl

type program = {
  decls : decl list;
}
let create_program decls = {decls=decls}


(******************
Printing functions
*******************)

let insert_tabs num_tabs = String.make num_tabs '\t'

let string_of_type typ num_tabs =
  let rec sot = function
    | Bool -> "bool"
    | Int -> "int"
    | Float -> "float"
    | Ident i -> i
    | Array t -> (sot t) ^ "[]"
    | Void -> "void"
  in
  (insert_tabs num_tabs) ^ (sot typ)

let string_of_expr e num_tabs =
  let rec soe = function
    | Plus (t1, t2) -> (soe t1) ^ " + " ^ (soe t2)
    | _ -> ""
  in
  (insert_tabs num_tabs) ^ (soe e)

let string_of_var_decl d num_tabs =
  (string_of_type d.varType num_tabs) ^ " " ^ d.varName
			     
let string_of_stmt s num_tabs =
  let sos = function
    | Expr e -> (string_of_expr e num_tabs)
    | VarDeclStmt d -> (string_of_var_decl d num_tabs)
    | _ -> ""
  in
  (insert_tabs num_tabs) ^ (sos s)
			     
let string_of_decl d num_tabs = match d with
  | VarDecl d ->
      (string_of_var_decl d num_tabs)
  | FnDecl d ->
      let map_fn s =
	string_of_stmt s (num_tabs + 1)
      in
      (string_of_type d.returnType (num_tabs + 1)) ^ " " ^ d.fnName ^ "\n"
      ^ (String.concat "\n" (List.map map_fn d.stmts))

let string_of_program p =
  let map_fn d =
    string_of_decl d 0
  in
  String.concat "\n" (List.map map_fn p.decls)
