(* piVC *)

open Printf


(* A node can be anything *)

type varType = 
  | Bool
  | Int
  | Float
  | Ident of string
  | Array of varType

type varDecl = {
(*  t    : varType;*)
  varName : string;
}
let create_varDecl name = {varName=name};

type fnDecl = {
(*  returnType : varType;*)
  fnName       : string;
(*  formals    : varDecl list;*)
}
let create_fnDecl name = {fnName=name};

type decl = 
  | VarDecl of varDecl
  | FnDecl of fnDecl
  | Jason of int

type program = {
  decls : decl list;
}
let create_program decls = {decls=decls};

type pair = {
  a : int;
  b : int;
}

let test = let p = {a=5; b=6} in p;