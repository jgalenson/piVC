open Hashtbl
open Ast

let scope_stack = [Hashtbl.create 5]

let enter_scope = 
  scope_stack = List.append [Hashtbl.create 5] scope_stack

let exit_scope = 
  match scope_stack with
   elem :: list -> scope_stack = list
    | _ -> raise (Invalid_argument "Can't remove scope.")
(*TODO: more appropraite exception*)

let getDeclName decl =
  match decl with
      Ast.VarDecl(l, d) -> d.varName.name
    | Ast.FnDecl(l, d) -> d.fnName.name
  
let insertDecl decl = Hashtbl.add (List.nth scope_stack 5) (getDeclName decl) decl

let lookupDecl decl declName =
  let rec lookupRecursive level = 
    let exists = Hashtbl.mem (List.nth scope_stack level) declName in
      if exists then
	Some (Hashtbl.find (List.nth scope_stack level) declName)
      else
	if (level == List.length scope_stack - 1) then
	  None
	else
	  lookupRecursive (level + 1)
  in
  lookupRecursive 0












(*
let lookupDeclInScopeNOrAbove declName N = 
  let exists = Hashtbl.mem (List.nth scope_stack Nab) declName in
    match exists with
	true -> Hashtbl.find (List.nth scope_stack N) declName
      | false -> let highestElemInScopeStack = (List.length scope_stack)-1 in
	    match N with
	           highestElemInScopeStack -> None
                   | _ -> lookupDeclInScopeNOrAbove declName (N-1)

let lookupDecl declName = lookupDeclInScopeNOrAbove declName 0

let test = lookupDeclInScopeNOrAbove "hi" 5
*)
