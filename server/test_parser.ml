(* piVC *)

open Parse_utils
open Semantic_checking
open Ast

(* TODO: Split this into two functions: one that returns a list
   of all the basic paths (list of list of paths) and one that
   prints them. Joel will do this, but I'll wait until we modify
   gen basic paths to return a list not a queue. *)
  
let print_basic_paths program = 
  let print_decl_paths_if_appropriate decl = 
    match decl with
        VarDecl (loc, vd) -> ignore ()
      | FnDecl (loc, fd) -> Basic_paths.print_all_basic_paths (Basic_paths.generate_paths_for_func fd program)     
  in List.iter print_decl_paths_if_appropriate program.decls


let print_program program = match program with
  | Some (p) ->
    print_endline "\n---------";
    print_string (string_of_program p);

    print_endline "---------";
    print_endline "Basic paths:";

    print_basic_paths p;
    print_endline "---------"
  | None -> print_string "";;

let _ =
  let (program, errors) = goParse stdin in
  print_program program;
  let map_fn e = print_string (string_of_error e) in
  List.iter map_fn errors
