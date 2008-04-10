(*
 * The abstract syntax tree
 * The entire interface is public, and is included in the file. We might want to prune
 * the interface later.
 *)

type quantification = Unquantified | Existential | Universal;;

type location = {
  loc_start  : Lexing.position;
  loc_end    : Lexing.position;
}
val gdl : unit -> location
val get_dummy_location : unit -> location
val create_location : Lexing.position -> Lexing.position -> location
val col_number_of_position : Lexing.position -> int
val location_union : location -> location -> location

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
  var_id : (int option) ref;
  quant: quantification;
}
val create_varDecl : varType -> identifier -> location -> varDecl
val create_Existential_varDecl : varType -> identifier -> location -> varDecl
val create_Universal_varDecl : varType -> identifier -> location -> varDecl

(*val set_quantification_on_varDecl_List : varDecl list -> quantification -> unit*)

val string_of_quantification : quantification -> string

val create_identifier : string -> location -> identifier
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
val create_fnDecl : identifier -> varDecl list -> varType -> stmt -> expr -> expr -> location -> fnDecl

type decl = 
  | VarDecl of location * varDecl
  | FnDecl of location * fnDecl

val name_of_decl : decl -> string

val type_of_decl : decl -> varType

type program = {
  decls : decl list;
  location : location;
}
val create_program : decl list -> location -> program

val get_root_decl : program -> string -> decl option

(*val identifier_of_lval : lval -> identifier*)

val location_of_decl : decl -> location 

val location_of_stmt : stmt -> location

val location_of_expr : expr -> location

val var_id_of_varDecl : varDecl -> int

val varDecl_of_identifier : identifier -> varDecl

val type_of_identifier : identifier -> varType

val id_of_identifier : identifier -> int

val varDecl_of_decl : decl -> varDecl

(******************
Printing functions
*******************)

val string_of_location : location -> string
val string_of_identifier : identifier -> string
val string_of_type : varType -> string
val string_of_lval : lval -> string (*temp*)
val string_of_constant : constant -> string (*temp*)
val string_of_expr : expr -> string
val string_of_var_decl : varDecl -> string			     
val string_of_stmt : stmt -> int -> string
val string_of_decl : decl -> string
val string_of_program : program -> string

