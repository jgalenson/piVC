(* piVC *)

open Parser
open Ast

let rec parseToken (lexbuf) = 
   let program = Parser.main Lexer.lang lexbuf in
     print_string (string_of_program program)

let goParse () =
  let lexbuf = Lexing.from_channel stdin in
    try
      parseToken(lexbuf)
  with Parsing.Parse_error -> print_string("Syntax error at the following token: " ^ Lexing.lexeme lexbuf ^ "\n")

let _ = goParse ()
