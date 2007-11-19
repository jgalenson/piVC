open Printf
open Lexing

type location = {
  loc_start  : Lexing.position;
  loc_end    : Lexing.position;
}
let create_location loc_start loc_end = {loc_start = loc_start; loc_end = loc_end}
let col_number_of_position pos = (pos.pos_cnum - pos.pos_bol) + 1
let location_union loc1 loc2 = {
  loc_start = 
    if loc1.loc_start.pos_cnum < loc2.loc_start.pos_cnum then
      loc1.loc_start
    else
      loc2.loc_start
    ;
  loc_end = 
    if loc1.loc_end.pos_cnum > loc2.loc_end.pos_cnum then
      loc1.loc_end
    else
      loc2.loc_end
    ;
}

type identifier = {
  name: string;
  location: location;
}
let create_identifier name location =
  {name = name; location = location;}

type varType = 
  | Bool of location
  | Int of location
  | Float of location
  | Identifier of identifier
  | Array of varType * location
  | Void of location

type varDecl = {
  varType : varType;
  varName : identifier;
  location : location
}
let create_varDecl t name location = {varType=t; varName=name; location=location;}



(* temp declarations: changeme todo *)
type lval =
  | LvalA of identifier
  | UnimplementedLval
type constant =
  | ConstInt of int
  | ConstFloat of float
  | ConstBool of bool
    
type expr =
  | Assign of lval * expr
  | Constant of constant
  | LValue of lval
  | Call of identifier * expr list
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
  fnName       : identifier;
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

let string_of_location loc =
    "(" ^ string_of_int loc.loc_start.pos_lnum ^ ", " ^ string_of_int (col_number_of_position loc.loc_start) ^ ")"

let string_of_identifier id =
  id.name

let string_of_type typ =
  let rec sot = function
    | Bool l -> "bool"
    | Int l -> "int"
    | Float l -> "float"
    | Identifier id -> string_of_identifier id
    | Array (t, l) -> (sot t) ^ "[]"
    | Void l -> "void"
  in
  sot typ
   

(* temp *)
let string_of_lval lval = match lval with
  | LvalA (s) -> string_of_identifier s
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
    | Call (s, el) -> string_of_identifier s ^ "(" ^ (String.concat ", " (List.map soe el)) ^ ")"
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
  (string_of_type d.varType) ^ " " ^ string_of_identifier d.varName
			     
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
      (string_of_type d.returnType) ^ " " ^ string_of_identifier d.fnName ^ "("
      ^ (String.concat ", " (List.map string_of_var_decl d.formals)) ^ ") "
      ^ (string_of_stmt d.stmtBlock num_tabs) ^ "\n"

let string_of_program p =
  let map_fn d =
    string_of_decl d 0
  in
  String.concat "\n" (List.map map_fn p.decls)
