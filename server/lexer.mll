(* The piVC lexer *)

{
  open Printf
  open Parser
  exception Eof
}

let digit  = ['0'-'9']
let alpha = ['a'-'z''A'-'Z']


rule lang = parse
    digit+ as num			{Global.updateLocation(lexbuf); T_IntConstant(int_of_string num)}
  | "/*"_*"*/"		       		{Global.updateLocation(lexbuf); lang lexbuf (*skip multi-line comments*)}
  | "//"[^'\n']*'\n'			{Global.updateLocation(lexbuf); lang lexbuf (*skip single-line comments*)}
  | [' ''\t''\n']			{Global.updateLocation(lexbuf); lang lexbuf (*skip whitespace*)}
  | "define"                  		{Global.updateLocation(lexbuf); T_Define}
  | "declare"				{Global.updateLocation(lexbuf); T_Declare}
  | "pre"				{Global.updateLocation(lexbuf); T_Pre}
  | "post"				{Global.updateLocation(lexbuf); T_Post}
  | "bool"              		{Global.updateLocation(lexbuf); T_Bool}
  | "void"				{Global.updateLocation(lexbuf); T_Void}
  | "true"				{Global.updateLocation(lexbuf); T_True}
  | "false"				{Global.updateLocation(lexbuf); T_False}
  | "[]"				{Global.updateLocation(lexbuf); T_Dims}
  | "<="				{Global.updateLocation(lexbuf); T_LessEqual}
  | ">="				{Global.updateLocation(lexbuf); T_GreaterEqual}
  | "=="				{Global.updateLocation(lexbuf); T_Equal}
  | "!="				{Global.updateLocation(lexbuf); T_NotEqual}
  | "&&"				{Global.updateLocation(lexbuf); T_And}
  | "||"				{Global.updateLocation(lexbuf); T_Or}
  | "while"				{Global.updateLocation(lexbuf); T_While}
  | "for"				{Global.updateLocation(lexbuf); T_For}
  | "if"				{Global.updateLocation(lexbuf); T_If}
  | "else"				{Global.updateLocation(lexbuf); T_Else}
  | "return"				{Global.updateLocation(lexbuf); T_Return}
  | "break"				{Global.updateLocation(lexbuf); T_Break}
  | "void"                              {Global.updateLocation(lexbuf); T_Void}
  | "int"                       	{Global.updateLocation(lexbuf); T_Int}
  | "float"            		        {Global.updateLocation(lexbuf); T_Float}
  | "bool"                              {Global.updateLocation(lexbuf); T_Bool}
  | "string"                            {Global.updateLocation(lexbuf); T_String}
  | "null"                              {Global.updateLocation(lexbuf); T_Null}
  | "while"                             {Global.updateLocation(lexbuf); T_While}
  | "for"                               {Global.updateLocation(lexbuf); T_For}
  | alpha(alpha|digit|'_')* as ident 	{Global.updateLocation(lexbuf); T_Identifier(ident)}
  | '+'					{Global.updateLocation(lexbuf); T_Plus}
  | '-'					{Global.updateLocation(lexbuf); T_Minus}
  | '*'					{Global.updateLocation(lexbuf); T_Star}
  | '/'					{Global.updateLocation(lexbuf); T_Slash}
  | '<'					{Global.updateLocation(lexbuf); T_Less}
  | '>'					{Global.updateLocation(lexbuf); T_Greater}
  | '='					{Global.updateLocation(lexbuf); T_Assign}
  | '!'					{Global.updateLocation(lexbuf); T_Not}
  | ';'					{Global.updateLocation(lexbuf); T_Semicolon}
  | ','					{Global.updateLocation(lexbuf); T_Comma}
  | '.'					{Global.updateLocation(lexbuf); T_Period}
  | '['					{Global.updateLocation(lexbuf); T_LSquareBracket}
  | ']'					{Global.updateLocation(lexbuf); T_RSquareBracket}
  | '('					{Global.updateLocation(lexbuf); T_LParen}
  | ')'					{Global.updateLocation(lexbuf); T_RParen}
  | '{'					{Global.updateLocation(lexbuf); T_LCurlyBracket}
  | '}'					{Global.updateLocation(lexbuf); T_RCurlyBracket}
  | '?'					{Global.updateLocation(lexbuf); T_QuestionMark}
  | '@'					{Global.updateLocation(lexbuf); T_At}
  | eof					{Global.updateLocation(lexbuf); T_EOF}
  | '\n'                                {Global.updateLocation(lexbuf); lang lexbuf (*skip new lines*)}
  | _					{Global.updateLocation(lexbuf); print_string("read unknown");T_Unknown}




