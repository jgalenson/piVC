(* The piVC lexer *)



{
  open Printf
  open Parser
  exception Eof
}

let digit  = ['0'-'9']
let alpha = ['a'-'z''A'-'Z']


rule lang = parse
    digit+ as num			{T_Int(int_of_string num)}
  | alpha(alpha|digit|'_')* as ident 	{T_Identifier(ident)}
  | "/*"_*"*/"		       		{lang lexbuf (*skip multi-line comments*)}
  | "//"[^'\n']*'\n'			{lang lexbuf (*skip single-line comments*)}
  | [' ''\t''\n']			{lang lexbuf (*skip whitespace*)}
  | "define"                  		{T_Define}
  | "declare"				{T_Declare}
  | "pre"				{T_Pre}
  | "post"				{T_Post}
  | "bool"              		{T_Bool}
  | "void"				{T_Void}
  | "true"				{T_True}
  | "false"				{T_False}
  | "[]"				{T_Dims}
  | "<="				{T_LessEqual}
  | ">="				{T_GreaterEqual}
  | "=="				{T_Equal}
  | "!="				{T_NotEqual}
  | "&&"				{T_And}
  | "||"				{T_Or}
  | "while"				{T_While}
  | "for"				{T_For}
  | "if"				{T_If}
  | "else"				{T_Else}
  | "return"				{T_Return}
  | "break"				{T_Break}
  | '+'					{T_Plus}
  | '-'					{T_Minus}
  | '*'					{T_Star}
  | '/'					{T_Slash}
  | '<'					{T_Less}
  | '>'					{T_Greater}
  | '='					{T_Assign}
  | '!'					{T_Not}
  | ';'					{T_Semicolon}
  | ','					{T_Comma}
  | '.'					{T_Period}
  | '['					{T_LSquareBracket}
  | ']'					{T_RSquareBracket}
  | '('					{T_LParen}
  | ')'					{T_RParen}
  | '{'					{T_LCurlyBracket}
  | '}'					{T_RCurlyBracket}
  | '?'					{T_QuestionMark}
  | eof					{T_EOF}
  | _					{T_Unknown}





(* This stuff is here for testing purposes. We will remove it later.*)
(*
{

let print_token_info token = 
  match token with
     T_Int(int) -> print_string(string_of_int int ^ "\n")
   | T_Ident(ident) -> print_string(ident ^ "\n")
   | _         -> print_string("")


let rec iterate buffer =
  match lang buffer with
    T_EOF -> 0
  | j -> print_token_info(j);iterate buffer

let _ = iterate(Lexing.from_channel stdin)

}
*)