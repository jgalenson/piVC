open Printf

type varType = 
  | Bool
  | Int
  | Float
  | Ident of string
  | Array of varType
  | Void

type varDecl = {
(*  t    : varType;*)
  varName : string;
}
let create_varDecl name = {varName=name}

type fnDecl = {
  returnType   : varType;
  fnName       : string;
(*  formals    : varDecl list;*)
}
let create_fnDecl name returnType = {fnName=name; returnType = returnType}

type decl = 
  | VarDecl of varDecl
  | FnDecl of fnDecl

type program = {
  decls : decl list;
}
let create_program decls = {decls=decls}

(* Printing functions *)

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

let string_of_decl d = match d with
  | VarDecl d ->
      (*(string_of_type d.t) ^ " " ^*) d.varName ^ ";\n"
  | FnDecl d ->
      (string_of_type d.returnType) ^ " " ^ d.fnName ^ "\n"

let string_of_program p =
  String.concat "" (List.map string_of_decl p.decls)
