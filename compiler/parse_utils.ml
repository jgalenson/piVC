(* piVC *)

open Parser
open Ast
open Semantic_checking
open Lexing
open Utils

let parseToken lexbuf errors = 
   let program = Parser.main Lexer.lang lexbuf in
   check_program program errors ;;

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
      
let goParse ic =
  let lexbuf = Lexing.from_channel ic in
  parse lexbuf ;;

let parse_string s =
  let lexbuf = Lexing.from_string s in
  parse lexbuf ;;
