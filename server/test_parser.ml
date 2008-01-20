(* piVC *)

open Parse_utils
open Semantic_checking
open Ast
  
let print_basic_paths all_info =
  let print_paths (_, info) =
    List.iter (fun (path, _) -> Basic_paths.print_basic_path path) info
  in
    List.iter print_paths all_info ;;
  
let print_verification_conditions all_info =
  let print_vcs (_, info) =
    List.iter (fun (_, vc) -> Verification_conditions.print_vc vc) info
  in
    List.iter print_vcs all_info ;;

let print_program program = match program with
  | Some (p) ->
    print_endline "\n---------";
    print_string (string_of_program p);
    print_endline "---------";
    let all_info = get_all_info p in
      
    print_endline "Basic paths:";
    print_basic_paths all_info;
    print_endline "---------";

    print_endline "Verification conditions:";
    print_verification_conditions all_info;
    print_endline "---------"
  | None -> print_string "";;

let _ =
  let (program, errors) = goParse stdin in
  print_program program;
  let map_fn e = print_string (string_of_error e) in
  List.iter map_fn errors
