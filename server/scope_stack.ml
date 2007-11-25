open Hashtbl
open Ast
open Stack

let create_scope_stack : (string, Ast.decl) Hashtbl.t Stack.t = (Stack.create ())

let enter_scope s = 
  Stack.push (Hashtbl.create 5) s

let exit_scope s = 
  ignore(Stack.pop s)

let get_decl_name decl =
  match decl with
      Ast.VarDecl(l, d) -> d.varName.name
    | Ast.FnDecl(l, d) -> d.fnName.name
  
let insert_decl decl s = Hashtbl.add (Stack.top s) (get_decl_name decl) decl

let lookup_decl s declName =
  let copy = Stack.copy s in
  let rec lookupRecursive copy = 
    if (Stack.is_empty copy) then None else
    let exists = Hashtbl.mem (Stack.top copy) declName in
      if exists then
	Some (Hashtbl.find (Stack.top copy) declName)
      else
	lookupRecursive copy
  in
  lookupRecursive copy


let lookup_decl_in_curr_scope decl s = 
  let declName = get_decl_name decl in
   let exists = Hashtbl.mem (Stack.top s) declName in
      if exists then
	Some (Hashtbl.find (Stack.top s) declName)
      else
	None
