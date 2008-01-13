(* piVC *)

open Parse_utils
open Semantic_checking
open Ast

let print_program program = match program with
  | Some (p) ->
    print_endline "\n---------";
    print_string (string_of_program p);
    print_endline "\n---------"
  | None -> print_string "";;

let _ =
  let (program, errors) = goParse stdin in
  print_program program;
  let map_fn e = print_string (string_of_error e) in
  Queue.iter map_fn errors
