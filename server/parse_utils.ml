(* piVC *)

open Parser
open Ast
open Semantic_checking
open Lexing

let rec parseToken (lexbuf) = 
   let program = Parser.main Lexer.lang lexbuf in
     print_string "\n---------\n";
     print_string (string_of_program program);
     print_string "\n---------\n";
     check_program program
    
let goParse ic = 
  let lexbuf = Lexing.from_channel ic in
    try
      parseToken(lexbuf)
  with Parsing.Parse_error -> (
    let symbol_start_pos = Lexing.lexeme_start_p lexbuf in
    let symbol_end_pos = Lexing.lexeme_end_p lexbuf in
    let loc = Ast.create_location symbol_start_pos symbol_end_pos in
    print_string("Syntax error at the following token: " ^ Lexing.lexeme lexbuf ^ "\n" ^
		 "Which is at the following location: " ^ Ast.string_of_location loc ^ "\n")

  
  )
