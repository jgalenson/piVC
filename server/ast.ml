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
  t    : varType;
  name : string;
}

type fnDecl = {
  returnType : varType;
  name       : string;
  formals    : varDecl list;
}

type decl = 
  | VarDecl of varDecl
  | FnDecl of fnDecl


type program = {
  functions : fnDecl list;
}

