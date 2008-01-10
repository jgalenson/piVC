open Hashtbl
open Ast
open Stack

type t = (string, Ast.decl) Hashtbl.t Stack.t

let create () = (Stack.create ())

let enter_scope s = 
  Stack.push (Hashtbl.create 5) s

let exit_scope s = 
  ignore(Stack.pop s)

let insert_decl decl s = Hashtbl.add (Stack.top s) (Ast.name_of_decl decl) decl

let lookup_decl declName s =
  let copy = Stack.copy s in
  let rec lookupRecursive copy = 
    if (Stack.is_empty copy) then None else
    let exists = Hashtbl.mem (Stack.top copy) declName in
      if exists then
	Some (Hashtbl.find (Stack.top copy) declName)
      else
        (
        ignore (Stack.pop copy);
	lookupRecursive copy
        )
  in
  lookupRecursive copy


let lookup_decl_in_curr_scope_only declName s = 
  let exists = Hashtbl.mem (Stack.top s) declName in
     if exists then
       Some (Hashtbl.find (Stack.top s) declName)
     else
       None
