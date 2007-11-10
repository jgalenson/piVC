open Parser

let rec parseToken (lexbuf) = 
   match Parser.main Lexer.lang lexbuf with
     0 -> print_string("All done!\n");
   | _ -> parseToken(lexbuf)


let goParse () =
  let lexbuf = Lexing.from_channel stdin in
    try
      parseToken(lexbuf)
  with Parsing.Parse_error -> print_string("Syntax error at the following token: " ^ Lexing.lexeme lexbuf ^ "\n")

let _ = goParse ()