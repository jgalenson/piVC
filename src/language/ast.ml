open Printf
open Lexing

exception Error of string

type quantification = Unquantified | Existential | Universal;;

type location = {
  loc_start  : Lexing.position;
  loc_end    : Lexing.position;
}

let rec gdl () = (get_dummy_location ())
and get_dummy_location () = {
  loc_start={pos_fname="dummy"; pos_lnum=0; pos_bol=0; pos_cnum=0};
  loc_end={pos_fname="dummy"; pos_lnum=0; pos_bol=0; pos_cnum=0};
}
and is_dummy_location loc = 
  loc.loc_start.pos_fname="dummy"
  
let create_location loc_start loc_end = {loc_start = loc_start; loc_end = loc_end}

let col_number_of_position pos = (pos.pos_cnum - pos.pos_bol)

let location_union loc1 loc2 =
  if loc1==gdl() then loc2
  else if loc2==gdl() then loc1
  else
      {
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
  is_length : bool; (*array lengths are converted to identifiers*)
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
  var_id: (string option) ref;
  quant : quantification;
  is_param : bool;
}
let create_varDecl t name location = {varType=t; varName=name; location_vd=location; var_id = ref None; quant = Unquantified; is_param = false;}
let create_param_varDecl t name location = {varType=t; varName=name; location_vd=location; var_id = ref None; quant = Unquantified; is_param = true;}
let create_existential_varDecl t name location = {varType=t; varName=name; location_vd=location; var_id = ref None; quant = Existential; is_param = false;}
let create_universal_varDecl t name location = {varType=t; varName=name; location_vd=location; var_id = ref None; quant = Universal; is_param = false;}

let is_integral_type t = match t with
  | Int (loc) -> true
  | ErrorType -> true
  | _ -> false ;;
  
(*let set_quantification_on_varDecl_List vd_list quant = 
  let set_quantification_on_varDecl vd = 
    vd.quant := quant
  in
  (*List.iter set_quantification_on_varDecl vd_list*)ignore()
*)

let string_of_quantification quant = match quant with
    Universal -> "universal"
  | Existential -> "existential"
  | Unquantified -> "unquantified"

let varDecl_of_identifier ident = 
  match !(ident.decl) with
      Some(d) -> (d)
    | None -> raise (Error "Variable decl not set")

let id_of_varDecl decl = 
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
            | None -> ""
        end
    | None -> ""

let create_identifier name location =
  {name = name; location_id = location; decl = ref None; is_length = false}

let create_length_identifier name location =
  {name = name; location_id = location; decl = ref None; is_length = true}
    
type lval =
  | NormLval of location * identifier
  | ArrayLval of location * expr * expr

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
  | ForAll of location * varDecl list * expr
  | Exists of location * varDecl list * expr
  | ArrayUpdate of location * expr * expr * expr
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
  | WhileStmt of location * expr * stmt * annotation * rankingAnnotation option
  | ForStmt of location * expr * expr * expr * stmt * annotation * rankingAnnotation option
  | BreakStmt of location
  | ReturnStmt of location * expr
  | AssertStmt of location * annotation
  | StmtBlock of location * stmt list
  | EmptyStmt

and annotation_type =
  | Normal of identifier option
  | Runtime
  | Precondition
  | Postcondition
      
and annotation = {
  ann : expr;
  ann_type : annotation_type;
  mutable ann_name : string option
}
    
and rankingAnnotation = {
  tuple : expr list;
  location_ra : location;
  mutable associated_annotation : annotation option
}

let create_annotation a t = { ann = a ; ann_type = Normal t ; ann_name = None }
let create_precondition a = { ann = a; ann_type = Precondition ; ann_name = None }
let create_postcondition a = { ann = a; ann_type = Postcondition ; ann_name = None }
let create_annotation_copy a c = { ann = a; ann_type = c.ann_type ; ann_name = c.ann_name }
let create_ranking_annotation t l = { tuple = t ; location_ra = l ; associated_annotation = None }
let create_ranking_annotation_copy t c = { tuple = t ; location_ra = c.location_ra ; associated_annotation = c.associated_annotation }
  
type fnDecl = {
  fnName        : identifier;
  formals       : varDecl list;
  returnType    : varType;
  stmtBlock     : stmt;
  preCondition  : annotation;
  postCondition : annotation;
  fnRankingAnnotation : rankingAnnotation option;
  location_fd   : location;
}
let create_fnDecl name formals returnType stmtBlock preCondition postCondition rankingAnnotation location = {fnName=name; returnType = returnType; formals = formals; stmtBlock = stmtBlock; preCondition = preCondition; postCondition = postCondition; fnRankingAnnotation = rankingAnnotation; location_fd = location;}

type predicate = {
  predName   : identifier;
  formals_p  : varDecl list;
  expr       : expr;
  location_p : location;
}


let replace_loc_of_expr expr new_loc= 
  match expr with
    | Assign (loc,l, e) -> Assign(new_loc,l,e)
    | Constant (loc,c) -> Constant(new_loc,c)
    | LValue (loc,l) -> LValue(new_loc,l)
    | Call (loc,s, el) -> Call(new_loc,s,el)
    | Plus (loc,t1, t2) -> Plus(new_loc,t1,t2)
    | Minus (loc,t1, t2) -> Minus(new_loc,t1,t2)
    | Times (loc,t1, t2) -> Times(new_loc,t1,t2)
    | Div (loc,t1, t2) -> Div(new_loc,t1,t2)
    | IDiv (loc,t1, t2) -> IDiv(new_loc,t1,t2)
    | Mod (loc,t1, t2) -> Mod(new_loc,t1,t2)
    | UMinus (loc,t) -> UMinus(new_loc,t)
    | ForAll (loc,decls,e) -> ForAll(new_loc,decls,e)
    | Exists (loc,decls,e) -> Exists(new_loc,decls,e)
    | ArrayUpdate (loc, exp, assign_to, assign_val) -> ArrayUpdate(new_loc,exp,assign_to, assign_val)
    | LT (loc,t1, t2) -> LT(new_loc,t1,t2)
    | LE (loc,t1, t2) -> LE(new_loc,t1,t2)
    | GT (loc,t1, t2) -> GT(new_loc,t1,t2)
    | GE (loc,t1, t2) -> GE(new_loc,t1,t2)
    | EQ (loc,t1, t2) -> EQ(new_loc,t1,t2)
    | NE (loc,t1, t2) -> NE(new_loc,t1,t2)
    | And (loc,t1, t2) -> And(new_loc,t1,t2)
    | Or (loc,t1, t2) -> Or(new_loc,t1,t2)
    | Not (loc,t) -> Not(new_loc,t)
    | Length (loc, t) -> Length(new_loc,t)
    | Iff (loc,t1, t2) -> Iff(new_loc,t1,t2)
    | Implies (loc,t1, t2) -> Implies(new_loc,t1,t2)
    | EmptyExpr -> assert(false)


type decl = 
  | VarDecl of location * varDecl
  | FnDecl of location * fnDecl
  | Predicate of location * predicate

let varDecl_of_decl decl = match decl with
    VarDecl(loc, vd) -> vd
  | _ -> raise (Error "Not a varDecl")

let fnDecl_of_decl decl = match decl with
    FnDecl(loc, fd) -> fd
  | _ -> raise (Error "Not a fnDecl")

let name_of_decl decl =
  match decl with
      VarDecl(l, d) -> d.varName.name
    | FnDecl(l, d) -> d.fnName.name
    | Predicate(l,d) -> d.predName.name

let type_of_decl = function
  | VarDecl (loc, d) -> d.varType
  | FnDecl (loc, d) -> d.returnType
  | Predicate (loc, p) -> Bool(gdl())

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

(*
let identifier_of_lval lval = match lval with
    NormLval(loc,id) -> id
  | ArrayLval(loc,id,index) -> id
*)

let location_of_decl decl = 
  match decl with
      VarDecl(l,d) -> l
    | FnDecl(l,d) -> l
    | Predicate(l,d) -> l

let location_of_stmt = function
  | Expr (loc, e) -> loc
  | VarDeclStmt (loc, d) -> loc
  | IfStmt (loc, e, s1, s2) -> loc
  | WhileStmt (loc, e1, s, e2, ra) -> loc
  | ForStmt (loc, e1, e2, e3, s, e4, ra) -> loc
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
    | ForAll (loc,decls,e) -> loc
    | Exists (loc,decls,e) -> loc
    | ArrayUpdate (loc, exp, assign_to, assign_val) -> loc
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


let location_of_lval lval = match lval with
  | NormLval(loc,_) -> loc
  | ArrayLval(loc,_,_) -> loc

let location_of_ranking_annotation ra = ra.location_ra ;;      

(******************
Printing functions
*******************)

let insert_tabs num_tabs = String.make num_tabs '\t'

let string_of_location loc =
    "(" ^ string_of_int loc.loc_start.pos_lnum ^ ", " ^ string_of_int (col_number_of_position loc.loc_start) ^ ") to " ^
    "(" ^ string_of_int loc.loc_end.pos_lnum ^ ", " ^ string_of_int (col_number_of_position loc.loc_end) ^ ")"



let string_of_quantification_of_identifier_if_available ident = 
  match !(ident.decl) with
      Some(d) -> string_of_quantification(d.quant)
    | None -> ""

let string_of_identifier id =
  id.name (*^ "<" ^ string_of_quantification_of_identifier_if_available id ^ ">"*)

let rec string_of_identifier_with_extra_info id =
  match id.decl.contents with
      None -> id.name
    | Some(vd) ->
        begin
          id.name ^ "{" ^ (id_of_varDecl(vd)) ^ ", " ^ (string_of_type vd.varType) ^ "}"
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
   
let get_delimited_list_of_decl_names decls delimiter = 
  let rec delimiter_after_all_elems decls = 
    match decls with
        decl :: rest -> string_of_identifier decl.varName ^ (*(string_of_quantification (decl.quant)) ^*) delimiter ^ delimiter_after_all_elems rest
      | [] -> ""
  in
  let str_with_delimiter_at_end = delimiter_after_all_elems decls in
    String.sub str_with_delimiter_at_end 0 ((String.length str_with_delimiter_at_end)-(String.length delimiter))

(* temp *)
let rec string_of_lval lval = match lval with
  | NormLval (loc, s) -> string_of_identifier s
  | ArrayLval (loc, t1, t2) -> (string_of_expr t1) ^ "[" ^ (string_of_expr t2) ^ "]"
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
    | ForAll (loc,decls,e) -> "forall " ^ get_delimited_list_of_decl_names decls "," ^ ".(" ^ soe e ^ ")"
    | Exists (loc,decls,e) -> "exists " ^ get_delimited_list_of_decl_names decls "," ^ ".(" ^ soe e ^ ")"
    | ArrayUpdate (loc,exp,assign_to,assign_val) -> soe exp ^ "{" ^ soe assign_to ^ " <- " ^ soe assign_val ^ "}"
    | LT (loc,t1, t2) -> (soe t1) ^ " < " ^ (soe t2)
    | LE (loc,t1, t2) -> (soe t1) ^ " <= " ^ (soe t2)
    | GT (loc,t1, t2) -> (soe t1) ^ " > " ^ (soe t2)
    | GE (loc,t1, t2) -> (soe t1) ^ " >= " ^ (soe t2)
    | EQ (loc,t1, t2) -> (soe t1) ^ " = " ^ (soe t2)
    | NE (loc,t1, t2) -> (soe t1) ^ " != " ^ (soe t2)
    | And (loc,t1, t2) -> "(" ^ (soe t1) ^ ") && (" ^ (soe t2) ^ ")"
    | Or (loc,t1, t2) -> "(" ^ (soe t1) ^ ") || (" ^ (soe t2) ^ ")"
    | Not (loc,t) -> "!(" ^ (soe t) ^ ")"
    | Iff (loc,t1, t2) -> "(" ^ (soe t1) ^ ") <-> (" ^ (soe t2) ^ ")"
    | Implies (loc,t1, t2) -> "(" ^ (soe t1) ^ ") -> (" ^ (soe t2) ^ ")"
    | Length (loc, t) -> "|" ^ (soe t) ^ "|"
    | EmptyExpr  -> ""
  in
  soe e

let string_of_var_decl d =
  (string_of_type d.varType) ^ " " ^ string_of_identifier d.varName ;;

let string_of_ranking_annotation ra =
  let string_of_annotation prev a =
    (if (prev = "(") then prev else prev ^ ", ") ^ (string_of_expr a)
  in
  (List.fold_left string_of_annotation "(" ra.tuple) ^ ")" ;;

let string_of_ranking_annotation_with_tabs ra_opt num_tabs = 
  match ra_opt with
    | Some (a) -> (insert_tabs num_tabs) ^ "#" ^ (string_of_ranking_annotation a) ^ "\n"
    | None -> "" ;;

let string_of_annotation a =
  let prefix_str = match a.ann_type with
    | Normal (loc) ->
	if Utils.is_some loc then
	  (string_of_identifier (Utils.elem_from_opt loc)) ^ ":"
	else
	  ""
    | Runtime -> "runtime_assert"
    | Precondition -> "pre"
    | Postcondition -> "post"
  in
  "@" ^ prefix_str ^ " " ^ (string_of_expr a.ann) ;;

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
    | WhileStmt (loc, test, block, annotation, ra) ->
	"while\n"
	^ (ins_tabs 1) ^ (string_of_annotation annotation) ^ "\n" ^ (string_of_ranking_annotation_with_tabs ra (num_tabs+1))
	 ^ (ins_tabs 1) ^ "(" ^ (soe test) ^ ") " ^ (sos block)
    | ForStmt (loc, init, test, incr, block, annotation, ra) ->
	"for\n"
	^ (ins_tabs 1) ^ (string_of_annotation annotation) ^ "\n" ^ (string_of_ranking_annotation_with_tabs ra (num_tabs+1))
	^ (ins_tabs 1) ^ "(" ^ (soe init) ^ "; " ^ (soe test) ^ "; "
	^ (soe incr) ^ ") " ^ (sos block)
    | BreakStmt loc -> "break;"
    | ReturnStmt (loc, t) ->
	let space t = match t with
	| EmptyExpr -> ""
	| _ -> " "
	in
	"return" ^ (space t) ^ (soe t) ^ ";"
    | AssertStmt (loc, e) -> (string_of_annotation e) ^ ";"
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
      (string_of_annotation d.preCondition) ^ "\n" ^ (string_of_annotation d.postCondition) ^ "\n"
      ^ (string_of_ranking_annotation_with_tabs d.fnRankingAnnotation 0)
      ^ (string_of_type d.returnType) ^ " " ^ string_of_identifier d.fnName ^ "("
      ^ (String.concat ", " (List.map string_of_var_decl d.formals)) ^ ") "
      ^ (string_of_stmt d.stmtBlock num_tabs)
  | Predicate  (loc, d) ->
      "predicate " ^ string_of_identifier d.predName ^ "("
      ^ (String.concat ", " (List.map string_of_var_decl d.formals_p)) ^ ") = "
      ^ (string_of_expr d.expr)

let string_of_decl d = tabbed_string_of_decl d 0 ;;
	
let string_of_program p =
  let map_fn d =
    (tabbed_string_of_decl d 0) ^ "\n"
  in
  String.concat "\n" (List.map map_fn p.decls)


let rec identifier_of_array_expr exp = 
  match exp with
      LValue(loc,lval) ->
        begin
          match lval with
              NormLval(loc,id) -> id
            | _ -> raise (Error ("expr more than just an array ident: " ^ string_of_expr exp))
        end
    | ArrayUpdate(loc,expr,assign_to,assign_val) -> identifier_of_array_expr expr
    | _ -> raise (Error ("expr more than just an array ident: " ^ string_of_expr exp))
   
let unique_fn_name fn =
  string_of_identifier fn.fnName ;;

(* Generates a name for an annotation based on its type and enclosing function. *)
let name_annotation func annotation_id ann_type = match ann_type with
  | Normal (_) -> (string_of_identifier func.fnName) ^ "." ^ (string_of_int !annotation_id)
  | Runtime -> (string_of_identifier func.fnName) ^ ".runtime_assertion." ^ (string_of_int !annotation_id)
  | Precondition -> (string_of_identifier func.fnName) ^ ".pre"
  | Postcondition -> (string_of_identifier func.fnName) ^ ".post" ;;

let create_runtime_assertion a func annotation_id = { ann = a ; ann_type = Runtime; ann_name = Some (name_annotation func annotation_id Runtime) }
