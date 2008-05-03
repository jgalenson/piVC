open Hashtbl
open Ast
open Stack

type t = (((string, Ast.decl) Hashtbl.t Stack.t)*int ref)

let create () = ((Stack.create ()), ref 0)

let enter_scope s = 
  Stack.push (Hashtbl.create 5) (fst s)

let exit_scope s = 
  ignore(Stack.pop (fst s))

let rec insert_decl decl s =
  begin
    match decl with
        VarDecl(loc,vd) ->
          begin
            vd.var_id := Some(vd.varName.name ^ "_" ^ string_of_int !(snd s));
            vd.varName.decl := Some(vd);
            (snd s) := !(snd s) + 1;
          end
      | FnDecl(loc, fd) -> ignore()
      | Predicate(loc, pd) -> ignore()
  end;
  insert_decl_without_setting_id decl s

and insert_decl_without_setting_id decl s = 
  Hashtbl.add (Stack.top (fst s)) (Ast.name_of_decl decl) decl

let lookup_decl declName s =
  let copy = Stack.copy (fst s) in
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
  let exists = Hashtbl.mem (Stack.top (fst s)) declName in
     if exists then
       Some (Hashtbl.find (Stack.top (fst s)) declName)
     else
       None
