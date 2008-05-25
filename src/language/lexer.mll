(* The piVC lexer *)

{
  open Printf
  open Parser
  open Lexing
  exception Eof

let files = ref [];;
let actual_cnum = ref 0;;

let count_character_occurences str char = 
  let num_occurences = ref 0 in
  let count c = if c = char then incr num_occurences in
    String.iter count str; !num_occurences
;;

let get_curr_filename () =
  match !files with
      a :: b -> fst a
    | [] -> ""
;;

(*
  Multi-line comments can contain newline characters, so we need to iterate
  over the entire token and look for all the newline characters.
*)
let updateLocation lexbuf = 
  let tokenStr = Lexing.lexeme lexbuf in
  let tokenLength = String.length tokenStr in
  let currPos = lexbuf.Lexing.lex_curr_p in
  let numNewLines = count_character_occurences tokenStr '\n' in
    actual_cnum := !actual_cnum + tokenLength;
    if ((!actual_cnum) = (snd (List.hd !files))) && (List.length !files) > 1  then
     begin 
       actual_cnum := 0;
       files := List.tl !files;
       lexbuf.Lexing.lex_curr_p <- {	   
         Lexing.pos_lnum = 0;           
         Lexing.pos_bol = 0;
         Lexing.pos_fname = get_curr_filename ();
         Lexing.pos_cnum = !actual_cnum;
       }
     end
    else if numNewLines > 0 then
      begin
        lexbuf.Lexing.lex_curr_p <- {	   
	  Lexing.pos_lnum = currPos.Lexing.pos_lnum + numNewLines;           
	  Lexing.pos_bol = !actual_cnum - ((tokenLength-1) - (String.rindex tokenStr '\n'));
          Lexing.pos_fname = fst (List.hd !files);
          Lexing.pos_cnum = !actual_cnum;
        }
      end
    else
      begin
        lexbuf.Lexing.lex_curr_p <- {	   
	  Lexing.pos_lnum = currPos.Lexing.pos_lnum;
	  Lexing.pos_bol = currPos.Lexing.pos_bol;
          Lexing.pos_fname = fst (List.hd !files);
          Lexing.pos_cnum = !actual_cnum
        }
      end
        
}

let digit  = ['0'-'9']
let alpha = ['a'-'z''A'-'Z']


rule lang =  parse
    digit+ as num			 {updateLocation(lexbuf); T_IntConstant(int_of_string num)}
  | digit+ '.' digit+ as num             {updateLocation(lexbuf); T_FloatConstant(float_of_string num)}
  | "/*" ([^'*']*('*'+ [^'/''*'])?)* '*'* "*/" {updateLocation(lexbuf); lang lexbuf (*skip multi-line comments*)}
  | "//"[^'\n']*'\n'			 {updateLocation(lexbuf); lang lexbuf (*skip single-line comments*)}
  | [' ''\t''\n']   			 {updateLocation(lexbuf); lang lexbuf (*skip whitespace*)}
  | "define"                  		 {updateLocation(lexbuf); T_Define}
  | "declare"				 {updateLocation(lexbuf); T_Declare}
  | "pre"				 {updateLocation(lexbuf); T_Pre}
  | "post"				 {updateLocation(lexbuf); T_Post}
  | "bool"              		 {updateLocation(lexbuf); T_Bool}
  | "void"			 	 {updateLocation(lexbuf); T_Void}
  | "true"				 {updateLocation(lexbuf); T_True}
  | "false"				 {updateLocation(lexbuf); T_False}
  | "[]"				 {updateLocation(lexbuf); T_Dims}
  | ":="				 {updateLocation(lexbuf); T_Assign}
  | "<="				 {updateLocation(lexbuf); T_LessEqual}
  | ">="				 {updateLocation(lexbuf); T_GreaterEqual}
  | "="				         {updateLocation(lexbuf); T_Equal}
  | "!="				 {updateLocation(lexbuf); T_NotEqual}
  | "&&"				 {updateLocation(lexbuf); T_And}
  | "||"				 {updateLocation(lexbuf); T_Or}
  | "while"				 {updateLocation(lexbuf); T_While}
  | "for"				 {updateLocation(lexbuf); T_For}
  | "if"				 {updateLocation(lexbuf); T_If}
  | "else"				 {updateLocation(lexbuf); T_Else}
  | "return"				 {updateLocation(lexbuf); T_Return}
  | "break"				 {updateLocation(lexbuf); T_Break}
  | "void"                               {updateLocation(lexbuf); T_Void}
  | "int"                       	 {updateLocation(lexbuf); T_Int}
  | "float"            		         {updateLocation(lexbuf); T_Float}
  | "bool"                               {updateLocation(lexbuf); T_Bool}
  | "string"                             {updateLocation(lexbuf); T_String}
  | "null"                               {updateLocation(lexbuf); T_Null}
  | "while"                              {updateLocation(lexbuf); T_While}
  | "for"                                {updateLocation(lexbuf); T_For}
  | "forall"                             {updateLocation(lexbuf); T_ForAll}
  | "exists"                             {updateLocation(lexbuf); T_Exists}
  | "<->"                                {updateLocation(lexbuf); T_Iff}
  | "->"                                 {updateLocation(lexbuf); T_Implies}
  | "<-"                                 {updateLocation(lexbuf); T_LeftArrow}
  | "@pre"                               {updateLocation(lexbuf); T_Pre}
  | "@post"                              {updateLocation(lexbuf); T_Post}
  | "div"                                {updateLocation(lexbuf); T_Div}
  | "typedef"                            {updateLocation(lexbuf); T_Typedef}
  | "length"                             {updateLocation(lexbuf); T_Length}
  | "struct"                             {updateLocation(lexbuf); T_Struct}
  | "predicate"                          {updateLocation(lexbuf); T_Predicate}
  | alpha(alpha|digit|'_')* as ident 	 {updateLocation(lexbuf); T_Identifier(ident)}
  | '+'					 {updateLocation(lexbuf); T_Plus}
  | '-'					 {updateLocation(lexbuf); T_Minus}
  | '*'					 {updateLocation(lexbuf); T_Star}
  | '/'					 {updateLocation(lexbuf); T_Slash}
  | '<'					 {updateLocation(lexbuf); T_Less}
  | '>'					 {updateLocation(lexbuf); T_Greater}
  | '!'					 {updateLocation(lexbuf); T_Not}
  | ';'					 {updateLocation(lexbuf); T_Semicolon}
  | ','					 {updateLocation(lexbuf); T_Comma}
  | '.'					 {updateLocation(lexbuf); T_Period}
  | '['					 {updateLocation(lexbuf); T_LSquareBracket}
  | ']'					 {updateLocation(lexbuf); T_RSquareBracket}
  | '('					 {updateLocation(lexbuf); T_LParen}
  | ')'					 {updateLocation(lexbuf); T_RParen}
  | '{'					 {updateLocation(lexbuf); T_LCurlyBracket}
  | '}'					 {updateLocation(lexbuf); T_RCurlyBracket}
  | '?'					 {updateLocation(lexbuf); T_QuestionMark}
  | '@'					 {updateLocation(lexbuf); T_Assert}
  | '#'					 {updateLocation(lexbuf); T_Termination}
  | '|'					 {updateLocation(lexbuf); T_Bar}
  | '%'					 {updateLocation(lexbuf); T_Mod}
  | ':'                                  {updateLocation(lexbuf); T_Colon}
  | eof					 {T_EOF} (*if we update here, it's going to need the current filename, but there is none, because we're at eof*)
  | _ as token                           {updateLocation(lexbuf); print_endline ("read unknown token" ^ (Char.escaped token)); T_Unknown}
