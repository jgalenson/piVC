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

(* temp declarations: changeme todo *)
type lval =
  | LvalA of string
  | UnimplementedLval
type constant =
  | ConstInt of int
  | ConstFloat of float
  | ConstBool of bool
type formals = unit
    
type expr =
  | Assign of lval * expr
  | Constant of constant
  | LValue of lval
  | Call of string * expr list
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | UMinus of expr
  | LT of expr * expr
  | LE of expr * expr
  | GT of expr * expr
  | GE of expr * expr
  | EQ of expr * expr
  | NE of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | EmptyExpr
    
type stmt =
  | Expr of expr
  | VarDeclStmt of varDecl
  | IfStmt of expr * stmt * stmt
  | WhileStmt of expr * stmt
  | ForStmt of expr * expr * expr * stmt
  | BreakStmt
  | ReturnStmt of expr
  | StmtBlock of stmt list
  | EmptyStmt
	
type fnDecl = {
  fnName       : string;
  formals    : varDecl list;
  returnType   : varType;
  stmtBlock : stmt;
}
let create_fnDecl name formals returnType stmtBlock = {fnName=name; returnType = returnType; formals = formals; stmtBlock = stmtBlock}

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

let string_of_type typ =
  let rec sot = function
    | Bool -> "bool"
    | Int -> "int"
    | Float -> "float"
    | Ident i -> i
    | Array t -> (sot t) ^ "[]"
    | Void -> "void"
  in
  sot typ

(* temp *)
let string_of_lval lval = match lval with
  | LvalA (s) -> s
  | _ -> "[Unimplemented]"
(* temp *)
let string_of_constant c = match c with
   | ConstInt (c) -> string_of_int c
   | ConstFloat (c) -> string_of_float c
   | ConstBool (c) -> string_of_bool c
    
let string_of_expr e =
  let rec soe = function
    | Assign (l, e) -> (string_of_lval l) ^ " = " ^ (soe e)
    | Constant (c) -> (string_of_constant c)
    | LValue (l) -> (string_of_lval l)
    | Call (s, el) -> s ^ "(" ^ (String.concat ", " (List.map soe el)) ^ ")"
    | Plus (t1, t2) -> (soe t1) ^ " + " ^ (soe t2)
    | Minus (t1, t2) -> (soe t1) ^ " - " ^ (soe t2)
    | Times (t1, t2) -> (soe t1) ^ " * " ^ (soe t2)
    | Div (t1, t2) -> (soe t1) ^ " / " ^ (soe t2)
    | Mod (t1, t2) -> (soe t1) ^ " % " ^ (soe t2)
    | UMinus (t) -> "-" ^ (soe t)
    | LT (t1, t2) -> (soe t1) ^ " < " ^ (soe t2)
    | LE (t1, t2) -> (soe t1) ^ " <= " ^ (soe t2)
    | GT (t1, t2) -> (soe t1) ^ " > " ^ (soe t2)
    | GE (t1, t2) -> (soe t1) ^ " >= " ^ (soe t2)
    | EQ (t1, t2) -> (soe t1) ^ " == " ^ (soe t2)
    | NE (t1, t2) -> (soe t1) ^ " != " ^ (soe t2)
    | And (t1, t2) -> (soe t1) ^ " && " ^ (soe t2)
    | Or (t1, t2) -> (soe t1) ^ " || " ^ (soe t2)
    | Not (t) -> " !" ^ (soe t)
    | EmptyExpr -> ""
  in
  soe e

let string_of_var_decl d =
  (string_of_type d.varType) ^ " " ^ d.varName
			     
let rec string_of_stmt s num_tabs =
  let soe = string_of_expr in
  let rec sos = function
    | Expr e -> (soe e) ^ ";"
    | VarDeclStmt d -> (string_of_var_decl d) ^ ";"
    | IfStmt (test, then_block, else_block) ->
	let else_part else_block = match else_block with
	| EmptyStmt -> ""
	| _ -> " else " ^ (sos else_block)
	in
	"if (" ^ (soe test) ^ ") "
	^ (sos then_block) ^ (else_part else_block)
    | WhileStmt (test, block) ->
	"while (" ^ (soe test) ^ ") " ^ (sos block)
    | ForStmt (init, test, incr, block) ->
	"if (" ^ (soe init) ^ "; " ^ (soe test) ^ "; "
	^ (soe incr) ^ ") " ^ (sos block)
    | BreakStmt -> "break;"
    | ReturnStmt (t) ->
	let space t = match t with
	| EmptyExpr -> ""
	| _ -> " "
	in
	"return" ^ (space t) ^ (soe t) ^ ";"
    | StmtBlock (tl) ->
      let map_fn s =
	(string_of_stmt s (num_tabs + 1))
      in	
	"{\n" ^ (String.concat "\n" (List.map map_fn tl))
        ^ "\n" ^ (insert_tabs num_tabs) ^ "}"
    | EmptyStmt -> ""
  in
  match s with
  | StmtBlock (tl) -> (sos s)
  | _ -> (insert_tabs num_tabs) ^ (sos s)
			     
let string_of_decl d num_tabs = match d with
  | VarDecl d ->
      (string_of_var_decl d) ^ ";\n"
  | FnDecl d ->
      (string_of_type d.returnType) ^ " " ^ d.fnName ^ "("
      ^ (String.concat ", " (List.map string_of_var_decl d.formals)) ^ ") "
      ^ (string_of_stmt d.stmtBlock num_tabs) ^ "\n"

let string_of_program p =
  let map_fn d =
    string_of_decl d 0
  in
  String.concat "\n" (List.map map_fn p.decls)
