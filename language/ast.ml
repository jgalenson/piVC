open Printf
open Lexing

exception Error of string

type location = {
  loc_start  : Lexing.position;
  loc_end    : Lexing.position;
}

let get_dummy_location () = {
  loc_start={pos_fname="dummy"; pos_lnum=0; pos_bol=0; pos_cnum=0};
  loc_end={pos_fname="dummy"; pos_lnum=0; pos_bol=0; pos_cnum=0};
}

let create_location loc_start loc_end = {loc_start = loc_start; loc_end = loc_end}

let col_number_of_position pos = (pos.pos_cnum - pos.pos_bol)

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
  location_id: location;
  decl : (varDecl option) ref;
}
and varType =  
  | Bool of location
  | Int of location
  | Float of location
  | Array of varType * location
  | Void of location
  | Identifier of identifier * location
  | ErrorType
	
and varDecl = {
  varType : varType;
  varName : identifier;
  location_vd : location;
  var_id: (int option) ref;
}
let create_varDecl t name location = {varType=t; varName=name; location_vd=location; var_id = ref None;}

let varDecl_of_identifier ident = 
  match !(ident.decl) with
      Some(d) -> (d)
    | None -> raise (Error "Variable decl not set")

let var_id_of_varDecl decl = 
  match !(decl.var_id) with
      Some(num) -> num
    | None -> raise (Error "Variable id not set")

let type_of_identifier ident = 
  (varDecl_of_identifier ident).varType

let id_of_identifier ident = 
  let d = varDecl_of_identifier ident in
    match !(d.var_id) with
        Some(id) -> id
      | None -> raise (Error "Variable id not set")

let id_of_identifier_if_available ident = 
  match !(ident.decl) with
      Some(d) ->
        begin
          match !(d.var_id) with
              Some(id) -> id
            | None -> -1
        end
    | None -> -1

let create_identifier name location =
  {name = name; location_id = location; decl = ref None}


type lval =
  | NormLval of location * identifier
  | ArrayLval of location * identifier * expr

and constant =
  | ConstInt of location * int
  | ConstFloat of location * float
  | ConstBool of location * bool
    
and expr =
  | Assign of location * lval * expr
  | Constant of location * constant
  | LValue of location * lval
  | Call of location * identifier * expr list
  | Plus of location * expr * expr
  | Minus of location * expr * expr
  | Times of location * expr * expr
  | Div of location * expr * expr
  | IDiv of location * expr * expr	
  | Mod of location * expr * expr
  | UMinus of location * expr
  | LT of location * expr * expr
  | LE of location * expr * expr
  | GT of location * expr * expr
  | GE of location * expr * expr
  | EQ of location * expr * expr
  | NE of location * expr * expr
  | And of location * expr * expr
  | Or of location * expr * expr
  | Not of location * expr
  | Iff of location * expr * expr
  | Implies of location * expr * expr
  | Length of location * expr
  | EmptyExpr

    
and stmt =
  | Expr of location * expr
  | VarDeclStmt of location * varDecl
  | IfStmt of location * expr * stmt * stmt
  | WhileStmt of location * expr * stmt * expr
  | ForStmt of location * expr * expr * expr * stmt * expr
  | BreakStmt of location
  | ReturnStmt of location * expr
  | AssertStmt of location * expr
  | StmtBlock of location * stmt list
  | EmptyStmt

	
type fnDecl = {
  fnName       : identifier;
  formals    : varDecl list;
  returnType   : varType;
  stmtBlock : stmt;
  preCondition : expr;
  postCondition : expr;
  location_fd : location;
}
let create_fnDecl name formals returnType stmtBlock preCondition postCondition location = {fnName=name; returnType = returnType; formals = formals; stmtBlock = stmtBlock; preCondition = preCondition; postCondition = postCondition; location_fd = location;}

type decl = 
  | VarDecl of location * varDecl
  | FnDecl of location * fnDecl

let varDecl_of_decl decl = match decl with
    VarDecl(loc, vd) -> vd
  | FnDecl(loc, fd) -> raise (Error "Not a varDecl")

let name_of_decl decl =
  match decl with
      VarDecl(l, d) -> d.varName.name
    | FnDecl(l, d) -> d.fnName.name

let type_of_decl = function
  | VarDecl (loc, d) -> d.varType
  | FnDecl (loc, d) -> d.returnType

type program = {
  decls : decl list;
  location : location;
}
let create_program decls location = {decls=decls; location = location;}

let get_root_decl program (declName:string) = 
  let decl_to_return = ref None in
  let rec find_decl remaining = (
    match remaining with
      [] -> ignore ()
    | e :: l -> ((
        match (String.compare (name_of_decl e) declName) with
          0 -> decl_to_return := Some(e)
        | _ -> ignore ()
      ); find_decl l)
  )
  in
    find_decl program.decls;
    !decl_to_return

let location_of_decl decl = 
  match decl with
    VarDecl(l,d) -> l
    | FnDecl(l,d) -> l

let location_of_stmt = function
  | Expr (loc, e) -> loc
  | VarDeclStmt (loc, d) -> loc
  | IfStmt (loc, e, s1, s2) -> loc
  | WhileStmt (loc, e1, s, e2) -> loc
  | ForStmt (loc, e1, e2, e3, s, e4) -> loc
  | BreakStmt (loc) -> loc
  | ReturnStmt (loc, e) -> loc
  | AssertStmt (loc, e) -> loc
  | StmtBlock (loc, s) -> loc
  | EmptyStmt -> get_dummy_location ()

let location_of_expr = function
    | Assign (loc,l, e) -> loc
    | Constant (loc,c) -> loc
    | LValue (loc,l) -> loc
    | Call (loc,s, el) -> loc
    | Plus (loc,t1, t2) -> loc
    | Minus (loc,t1, t2) -> loc
    | Times (loc,t1, t2) -> loc
    | Div (loc,t1, t2) -> loc
    | IDiv (loc,t1, t2) -> loc
    | Mod (loc,t1, t2) -> loc
    | UMinus (loc,t) -> loc
    | LT (loc,t1, t2) -> loc
    | LE (loc,t1, t2) -> loc
    | GT (loc,t1, t2) -> loc
    | GE (loc,t1, t2) -> loc
    | EQ (loc,t1, t2) -> loc
    | NE (loc,t1, t2) -> loc
    | And (loc,t1, t2) -> loc
    | Or (loc,t1, t2) -> loc
    | Not (loc,t) -> loc
    | Length (loc, t) -> loc
    | Iff (loc,t1, t2) -> loc
    | Implies (loc,t1, t2) -> loc
    | EmptyExpr -> get_dummy_location ()

(******************
Printing functions
*******************)

let insert_tabs num_tabs = String.make num_tabs '\t'

let string_of_location loc =
    "(" ^ string_of_int loc.loc_start.pos_lnum ^ ", " ^ string_of_int (col_number_of_position loc.loc_start) ^ ") to " ^
    "(" ^ string_of_int loc.loc_end.pos_lnum ^ ", " ^ string_of_int (col_number_of_position loc.loc_end) ^ ")"

let rec string_of_identifier id =
  match id.decl.contents with
      None -> id.name
    | Some(vd) ->
        begin
          id.name ^ "{" ^ string_of_int (var_id_of_varDecl(vd)) ^ ", " ^ (string_of_type vd.varType) ^ "}"
        end

and string_of_type typ =
  let rec sot = function
    | Bool l -> "bool"
    | Int l -> "int"
    | Float l -> "float"
    | Identifier (id, loc) -> string_of_identifier id
    | Array (t, l) -> (sot t) ^ "[]"
    | Void l -> "void"
    | ErrorType -> "error"
  in
  sot typ
   

(* temp *)
let rec string_of_lval lval = match lval with
  | NormLval (loc, s) -> string_of_identifier s
  | ArrayLval (loc, t1, t2) -> (string_of_identifier t1) ^ "[" ^ (string_of_expr t2) ^ "]"
(* temp *)
and string_of_constant c = match c with
   | ConstInt (loc,c) -> string_of_int c
   | ConstFloat (loc,c) -> string_of_float c
   | ConstBool (loc,c) -> string_of_bool c
    
and string_of_expr e =
  let rec soe = function
    | Assign (loc,l, e) -> (string_of_lval l) ^ " := " ^ (soe e)
    | Constant (loc,c) -> (string_of_constant c)
    | LValue (loc,l) -> (string_of_lval l)
    | Call (loc,s, el) -> string_of_identifier s ^ "(" ^ (String.concat ", " (List.map soe el)) ^ ")"
    | Plus (loc,t1, t2) -> (soe t1) ^ " + " ^ (soe t2)
    | Minus (loc,t1, t2) -> (soe t1) ^ " - " ^ (soe t2)
    | Times (loc,t1, t2) -> (soe t1) ^ " * " ^ (soe t2)
    | Div (loc,t1, t2) -> (soe t1) ^ " / " ^ (soe t2)
    | IDiv (loc,t1, t2) -> (soe t1) ^ " div " ^ (soe t2)					       
    | Mod (loc,t1, t2) -> (soe t1) ^ " % " ^ (soe t2)
    | UMinus (loc,t) -> "-" ^ (soe t)
    | LT (loc,t1, t2) -> (soe t1) ^ " < " ^ (soe t2)
    | LE (loc,t1, t2) -> (soe t1) ^ " <= " ^ (soe t2)
    | GT (loc,t1, t2) -> (soe t1) ^ " > " ^ (soe t2)
    | GE (loc,t1, t2) -> (soe t1) ^ " >= " ^ (soe t2)
    | EQ (loc,t1, t2) -> (soe t1) ^ " = " ^ (soe t2)
    | NE (loc,t1, t2) -> (soe t1) ^ " != " ^ (soe t2)
    | And (loc,t1, t2) -> (soe t1) ^ " && " ^ (soe t2)
    | Or (loc,t1, t2) -> (soe t1) ^ " || " ^ (soe t2)
    | Not (loc,t) -> " !(" ^ (soe t) ^ ")"
    | Iff (loc,t1, t2) -> (soe t1) ^ " <-> " ^ (soe t2)
    | Implies (loc,t1, t2) -> (soe t1) ^ " -> " ^ (soe t2)
    | Length (loc, t) -> "|" ^ (soe t) ^ "|"
    | EmptyExpr  -> ""
  in
  soe e

let string_of_var_decl d =
  (string_of_type d.varType) ^ " " ^ string_of_identifier d.varName
			     
let rec string_of_stmt s num_tabs =
  let soe = string_of_expr in
  let ins_tabs n = insert_tabs (num_tabs + n) in
  let rec sos = function
    | Expr (loc, e) -> (soe e) ^ ";"
    | VarDeclStmt (loc, d) -> (string_of_var_decl d) ^ ";"
    | IfStmt (loc, test, then_block, else_block) ->
	let else_part else_block = match else_block with
	| EmptyStmt -> ""
	| _ -> " else " ^ (sos else_block)
	in
	"if (" ^ (soe test) ^ ") "
	^ (sos then_block) ^ (else_part else_block)
    | WhileStmt (loc, test, block, annotation) ->
	"while\n"
	^ (ins_tabs 1) ^ "@ " ^ (soe annotation) ^ "\n" ^ (ins_tabs 1)
	^ "(" ^ (soe test) ^ ") " ^ (sos block)
    | ForStmt (loc, init, test, incr, block, annotation) ->
	"for\n"
	^ (ins_tabs 1) ^ "@ " ^ (soe annotation) ^ "\n" ^ (ins_tabs 1)
	^ "(" ^ (soe init) ^ "; " ^ (soe test) ^ "; "
	^ (soe incr) ^ ") " ^ (sos block)
    | BreakStmt loc -> "break;"
    | ReturnStmt (loc, t) ->
	let space t = match t with
	| EmptyExpr -> ""
	| _ -> " "
	in
	"return" ^ (space t) ^ (soe t) ^ ";"
    | AssertStmt (loc, e) -> "@ " ^ (soe e) ^ ";"
    | StmtBlock (loc, tl) ->
      let map_fn s =
	(string_of_stmt s (num_tabs + 1))
      in	
	"{\n" ^ (String.concat "\n" (List.map map_fn tl))
        ^ "\n" ^ (insert_tabs num_tabs) ^ "}"
    | EmptyStmt -> ""
  in
  match s with
  | StmtBlock (loc, tl) -> (sos s)
  | _ -> (insert_tabs num_tabs) ^ (sos s)

let tabbed_string_of_decl d num_tabs = match d with
  | VarDecl (loc, d) ->
      (string_of_var_decl d) ^ ";"
  | FnDecl  (loc, d) ->
      "@pre " ^ (string_of_expr d.preCondition) ^ "\n@post " ^ (string_of_expr d.postCondition) ^ "\n"
      ^ (string_of_type d.returnType) ^ " " ^ string_of_identifier d.fnName ^ "("
      ^ (String.concat ", " (List.map string_of_var_decl d.formals)) ^ ") "
      ^ (string_of_stmt d.stmtBlock num_tabs)

let string_of_decl d = tabbed_string_of_decl d 0 ;;
	
let string_of_program p =
  let map_fn d =
    (tabbed_string_of_decl d 0) ^ "\n"
  in
  String.concat "\n" (List.map map_fn p.decls)
