(* piVC *)

open Parser
open Ast
open Semantic_checking
open Lexing
open Utils

let parse lexbuf =
  let errors = Queue.create () in
  let e = ref errors in
  try
    let program = Parser.main Lexer.lang lexbuf in
      check_program program e;
      ((Some program), queue_to_list !e)
  with Parsing.Parse_error ->
    let symbol_start_pos = Lexing.lexeme_start_p lexbuf in
    let symbol_end_pos = Lexing.lexeme_end_p lexbuf in
    let loc = Ast.create_location symbol_start_pos symbol_end_pos in
      add_error SyntaxError (Lexing.lexeme lexbuf) loc e;
      (None, queue_to_list !e)

let parse_strings user_files =

  let includes_string = 
    let file = Unix.openfile (get_absolute_path Constants.includes_path) [Unix.O_RDONLY] 0o640 in
    let file_size = (Unix.fstat file).Unix.st_size in
    let file_text = String.create file_size in
      ignore (Unix.read file file_text 0 file_size);
      file_text
  in
  let all_files = ("includes",includes_string) :: user_files in
    
    
  let rec all_strings_concatenated elems = 
    match elems with
        elem :: elems -> snd elem ^ all_strings_concatenated elems
      | [] -> ""
  in
  let rec list_of_all_names_and_sizes elems = 
    match elems with
        elem :: elems -> [(fst elem,String.length (snd elem))] @ list_of_all_names_and_sizes elems
      | [] -> []
  in
    Lexer.files := list_of_all_names_and_sizes all_files;
    let lexbuf = Lexing.from_string (all_strings_concatenated all_files) in
      parse lexbuf ;;
