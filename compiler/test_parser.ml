(* piVC *)

open Parse_utils
open Semantic_checking
open Ast
open Verify
  
let print_basic_paths_and_vcs all_info =
  let print_paths_and_vcs (_, info) =
    let print_path_and_vc (path, vc) =
      Basic_paths.print_basic_path path;
      print_endline ("VC: " ^ Verification_conditions.string_of_vc vc) in
  List.iter print_path_and_vc info
  in
  List.iter print_paths_and_vcs all_info ;;

let print_program program = match program with
  | Some (p) ->
    print_endline "\n---------";
    print_string (string_of_program p);
    print_endline "---------";
    let all_info = get_all_info p in
      
    print_endline "Basic paths and VCs:";
    print_basic_paths_and_vcs all_info;
    print_endline "---------";
  | None -> print_string "";;

let _ =
  let input = ref "" in
    try
      while true do      
        input := !input ^ String.make 1 (Pervasives.input_char stdin) 
      done;
    with
        ex -> ignore()
    ;
    let (program, errors) = parse_strings [("",!input)] in
      print_program program;
      let map_fn e = print_string (string_of_error e) in
        List.iter map_fn errors
    
