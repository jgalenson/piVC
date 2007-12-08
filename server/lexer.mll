(* The piVC lexer *)

{
  open Printf
  open Parser
  open Lexing
  exception Eof


let countNewLines str  = 
  let numNewLines = ref 0 in
  let countFunction c = match c with
      '\n'  -> incr numNewLines
    | _     -> print_string("")
  in String.iter countFunction str; !numNewLines

(*
  Multi-line comments can contain newline characters, so we need to iterate
  over the entire token and look for all the newline characters.
*)
let updateLocation lexbuf = 
  let tokenStr = Lexing.lexeme lexbuf in
  let tokenLength = String.length tokenStr in
  let currPos = lexbuf.Lexing.lex_curr_p in
  let numNewLines = countNewLines tokenStr in
  match numNewLines with
      0 -> print_string("")
     |_ -> print_string("");
           lexbuf.Lexing.lex_curr_p <- {	   
	     Lexing.pos_lnum = currPos.Lexing.pos_lnum + 1;           
	     Lexing.pos_bol = currPos.Lexing.pos_cnum - ((tokenLength-1) - (String.rindex tokenStr '\n'));
             Lexing.pos_fname = currPos.Lexing.pos_fname;
             Lexing.pos_cnum = currPos.Lexing.pos_cnum;
           }
}

let digit  = ['0'-'9']
let alpha = ['a'-'z''A'-'Z']


rule lang = parse
    digit+ as num			{updateLocation(lexbuf); T_IntConstant(int_of_string num)}
  | "/*"_*"*/"		       		{updateLocation(lexbuf); lang lexbuf (*skip multi-line comments*)}
  | "//"[^'\n']*'\n'			{updateLocation(lexbuf); lang lexbuf (*skip single-line comments*)}
  | [' ''\t''\n']			{updateLocation(lexbuf); lang lexbuf (*skip whitespace*)}
  | "define"                  		{updateLocation(lexbuf); T_Define}
  | "declare"				{updateLocation(lexbuf); T_Declare}
  | "pre"				{updateLocation(lexbuf); T_Pre}
  | "post"				{updateLocation(lexbuf); T_Post}
  | "bool"              		{updateLocation(lexbuf); T_Bool}
  | "void"				{updateLocation(lexbuf); T_Void}
  | "true"				{updateLocation(lexbuf); T_True}
  | "false"				{updateLocation(lexbuf); T_False}
  | "[]"				{updateLocation(lexbuf); T_Dims}
  | ":="				{updateLocation(lexbuf); T_Assign}
  | "<="				{updateLocation(lexbuf); T_LessEqual}
  | ">="				{updateLocation(lexbuf); T_GreaterEqual}
  | "="				        {updateLocation(lexbuf); T_Equal}
  | "!="				{updateLocation(lexbuf); T_NotEqual}
  | "&&"				{updateLocation(lexbuf); T_And}
  | "||"				{updateLocation(lexbuf); T_Or}
  | "while"				{updateLocation(lexbuf); T_While}
  | "for"				{updateLocation(lexbuf); T_For}
  | "if"				{updateLocation(lexbuf); T_If}
  | "else"				{updateLocation(lexbuf); T_Else}
  | "return"				{updateLocation(lexbuf); T_Return}
  | "break"				{updateLocation(lexbuf); T_Break}
  | "void"                              {updateLocation(lexbuf); T_Void}
  | "int"                       	{updateLocation(lexbuf); T_Int}
  | "float"            		        {updateLocation(lexbuf); T_Float}
  | "bool"                              {updateLocation(lexbuf); T_Bool}
  | "string"                            {updateLocation(lexbuf); T_String}
  | "null"                              {updateLocation(lexbuf); T_Null}
  | "while"                             {updateLocation(lexbuf); T_While}
  | "for"                               {updateLocation(lexbuf); T_For}
  | "forall"                            {updateLocation(lexbuf); T_ForAll}
  | "exists"                            {updateLocation(lexbuf); T_Exists}
  | "<->"                               {updateLocation(lexbuf); T_Iff}
  | "->"                                {updateLocation(lexbuf); T_Implies}
  | "@pre"                              {updateLocation(lexbuf); T_Pre}
  | "@post"                             {updateLocation(lexbuf); T_Post}
  | "div"                               {updateLocation(lexbuf); T_Div}
  | "typedef"                           {updateLocation(lexbuf); T_Typedef}
  | "struct"                            {updateLocation(lexbuf); T_Struct}
  | alpha(alpha|digit|'_')* as ident 	{updateLocation(lexbuf); T_Identifier(ident)}
  | '+'					{updateLocation(lexbuf); T_Plus}
  | '-'					{updateLocation(lexbuf); T_Minus}
  | '*'					{updateLocation(lexbuf); T_Star}
  | '/'					{updateLocation(lexbuf); T_Slash}
  | '<'					{updateLocation(lexbuf); T_Less}
  | '>'					{updateLocation(lexbuf); T_Greater}
  | '!'					{updateLocation(lexbuf); T_Not}
  | ';'					{updateLocation(lexbuf); T_Semicolon}
  | ','					{updateLocation(lexbuf); T_Comma}
  | '.'					{updateLocation(lexbuf); T_Period}
  | '['					{updateLocation(lexbuf); T_LSquareBracket}
  | ']'					{updateLocation(lexbuf); T_RSquareBracket}
  | '('					{updateLocation(lexbuf); T_LParen}
  | ')'					{updateLocation(lexbuf); T_RParen}
  | '{'					{updateLocation(lexbuf); T_LCurlyBracket}
  | '}'					{updateLocation(lexbuf); T_RCurlyBracket}
  | '?'					{updateLocation(lexbuf); T_QuestionMark}
  | '@'					{updateLocation(lexbuf); T_Assert}
  | '|'					{updateLocation(lexbuf); T_Bar}
  | eof					{updateLocation(lexbuf); T_EOF}
  | '\n'                                {updateLocation(lexbuf); lang lexbuf (*skip new lines*)}
  | _					{updateLocation(lexbuf); print_string("read unknown");T_Unknown}




